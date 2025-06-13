#' Fit a Cubist Model for a Single Covariate Using PCA-Transformed Spectral Data
#'
#' This function builds, tunes, and evaluates a Cubist model for predicting a single
#' soil covariate from PCA-transformed MIR spectra. It applies an initial grid search
#' followed by Bayesian optimization for hyperparameter tuning.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import Cubist
#' @import rules
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict quantile
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom tidyselect starts_with
#' @importFrom parsnip cubist_rules set_engine set_mode fit
#' @importFrom workflows workflow add_model add_formula
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom dials grid_space_filling neighbors
#' @importFrom rules committees max_rules
#' @importFrom tune tune_grid tune_bayes control_grid control_bayes select_best finalize_workflow last_fit collect_predictions
#' @importFrom future plan multisession sequential
#'
#' @param input_data A data frame containing PCA-transformed predictors (`Dim.1`, `Dim.2`, ..., `Dim.50`)
#'        and one column matching the `covariate` to model as the response.
#' @param covariate A character string indicating which covariate to predict (e.g., `"Sand"`, `"pH"`).
#'
#' @return A list with three elements:
#'   \item{Model}{The finalized, fitted Cubist workflow object.}
#'   \item{Best_Parameters}{A tibble of the optimal hyperparameter values selected via Bayesian optimization.}
#'   \item{Evaluation}{Hold-out evaluation statistics.}
#'
#' @details
#' The modeling process proceeds in three stages:
#' \enumerate{
#'   \item{Split input data into training and testing sets with stratification on the response.}
#'   \item{Perform an initial max entropy grid search on committees, neighbors, and max_rules hyperparameters.}
#'   \item{Refine the model via Bayesian optimization, finalize the workflow, and evaluate on a hold-out set.}
#' }
#' Parallelization is automatically enabled for tuning phases via \code{future::plan(multisession)}.
#'
#' @examples
#' \dontrun{
#' # Simulated PCA-transformed dataset
#' df <- tibble::tibble(
#'   Dim.1 = rnorm(100),
#'   Dim.2 = rnorm(100),
#'   Dim.3 = rnorm(100),
#'   Sand  = runif(100, 50, 80)
#' )
#'
#' # Updated example call to use the new function name
#' result <- fit_cubist_model(input_data = df, covariate = "Sand")
#' result$Evaluation
#' }
#'
#' @seealso
#' \code{\link{predict_covariates}}, \code{\link{evaluate_predictions}}, \code{\link{reduce_dimensions_pca}} # Updated link
#'
#' @keywords internal

fit_cubist_model <- function(input_data,
                             covariate,
                             verbose) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Data validation
  ## ---------------------------------------------------------------------------

  if(verbose) cli::cli_progress_step("Preparing training data for covariate {.val {covariate}}")

  covariate_name <- grep(covariate, colnames(input_data), value = TRUE)

  if (length(covariate_name) == 0) {
    cli::cli_alert_danger("Covariate '{covariate}' not found in input_data.")
    return(NULL)
  }

  input_data$Response <- input_data[[covariate_name]]

  input_data %>%
    dplyr::select(Response,
                  dplyr::starts_with("Dim.")) %>%
    tidyr::drop_na() -> input_data

  if (nrow(input_data) == 0) {
    cli::cli_alert_warning("Input data for covariate '{covariate}' is empty after dropping NAs. Cannot train model.")
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Data Preparation
  ## ---------------------------------------------------------------------------

  set.seed(0307)

  safely_execute(expr          = {rsample::initial_split(input_data, strata = Response)},
                 default_value = NULL,
                 error_message = glue::glue("Failed to create initial data split for {covariate}")) -> split_data_safe

  split_data <- split_data_safe$result

  if (is.null(split_data)) return(NULL)

  Train_Data <- rsample::training(split_data)
  Test_Data  <- rsample::testing(split_data)


  safely_execute(expr          = {rsample::vfold_cv(Train_Data, v = 3)},
                 default_value = NULL,
                 error_message = glue::glue("Failed to create CV folds for {covariate}")) -> CV_Folds_safe

  CV_Folds <- CV_Folds_safe$result

  if (is.null(CV_Folds)) return(NULL)

  ## ---------------------------------------------------------------------------
  ## Step 2: Define and Tune Cubist Model
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Stage 1: Define cubist model specifications
    ## ---------------------------------------------------------------------------

    parsnip::cubist_rules(committees = tune::tune(),
                          neighbors  = tune::tune(),
                          max_rules  = tune::tune()) %>%
      parsnip::set_engine("Cubist") %>%
      parsnip::set_mode("regression") -> model_spec

    workflows::workflow() %>%
      workflows::add_model(model_spec) %>%
      workflows::add_formula(Response ~ .) -> wf

    ## ---------------------------------------------------------------------------
    ## Stage 2: Initial Grid Search for Hyperparameter Tuning
    ## ---------------------------------------------------------------------------

    safely_execute(expr          = {future::plan(future::multisession,
                                                 workers = parallel::detectCores(logical = TRUE) - 1)},
                   default_value = NULL,
                   error_message = "Failed to set parallel plan for tuning")

    dials::grid_space_filling(rules::committees(range = c(2L, 20L)),
                              dials::neighbors(range = c(2L, 9L)),
                              dials::max_rules(),
                              size = 5,
                              type = "max_entropy") -> grid

    if(verbose) cli::cli_progress_step("Running grid search for {.val {covariate}}")

    safely_execute(expr          = {tune::tune_grid(object    = wf,
                                   resamples = CV_Folds,
                                   grid      = grid,
                                   control   = tune::control_grid(allow_par = TRUE))},
                   default_value = NULL,
                   error_message = glue::glue("Grid tuning faliled for {covariate}")) -> grid_res_safe

    grid_res <- grid_res_safe$result

    if(is.null(grid_res)){
      safely_execute(expr = {future::plan(future::sequential)})
      return(NULL)
    }

    ## ---------------------------------------------------------------------------
    ## Stage 3: Bayesian Optimization for Hyperparameter Tuning
    ## ---------------------------------------------------------------------------

    if(verbose) cli::cli_progress_step("Running Bayesian optimization for {.val {covariate}}")

    safely_execute(expr        = {tune::tune_bayes(object    = wf,
                                                   resamples = CV_Folds,
                                                   initial   = grid_res,
                                                   iter      = 2,
                                                   control   = tune::control_bayes(allow_par = TRUE))},
                   default_value = NULL,
                   error_message = glue::glue("Bayesian tuning failed for {covariate}")) -> bayes_res_safe

    bayes_res <- bayes_res_safe$result

    if(is.null(bayes_res)){
      safely_execute(expr = {future::plan(future::sequential)})
      return(NULL)
    }

    ## ---------------------------------------------------------------------------
    ## Stage 4: Finalizing workflow
    ## ---------------------------------------------------------------------------

    cli::cli_progress_step("Finalizing workflow for {.val {covariate}}")

    best_params <- tune::select_best(bayes_res,
                                     metric = "rmse")

    final_wf    <- tune::finalize_workflow(wf,
                                           best_params)

    safely_execute(expr          = {future::plan(future::sequential)},
                   default_value = NULL,
                   error_message = "Failed to reset parallel plan after Cubist tuning")


  ## ---------------------------------------------------------------------------
  ## Step 3: Final Fit and Evaluation
  ## ---------------------------------------------------------------------------

  if(verbose) cli::cli_progress_step("Evaluating final model for {.val {covariate}}")

  safely_execute(expr          = {final_wf %>%
                                    tune::last_fit(split_data) %>%
                                    tune::collect_predictions() %>%
                                    dplyr::rename(Predicted = .pred) %>%
                                    drop_na() %>%
                                    soilspec::eval(pred = .$Predicted,
                                                   obs  = .$Response,
                                                   obj  = "quant")},
                 default_value = NULL,
                 error_message = ("Failed to perform last_fit() or collect_predictions() for the current model.")) -> eval_safe

  eval <- eval_safe$result

  if (is.null(eval)) return(NULL)

  safely_execute(expr          = {final_wf %>% parsnip::fit(Train_Data)},
                 default_value = NULL,
                 error_message = glue::glue("Failed to fit final workflow for {covariate}")) -> fitted_model_safe

  fitted_model <- fitted_model_safe$result

  if(is.null(fitted_model)) return(NULL)

  return(list(Model           = fitted_model,
              Best_Parameters = best_params,
              Evaluation      = eval))

}
