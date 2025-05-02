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
#' result <- cubist_model_function_covar(input_data = df, covariate = "Sand")
#' result$Evaluation
#' }
#'
#' @seealso
#' \code{\link{predict_covariates}}, \code{\link{evaluate_predictions}}, \code{\link{reduce_dimensions_pca_covpred}}
#'
#' @keywords internal

cubist_model_function_covar <- function(input_data,
                                        covariate) {

  cli::cli_progress_step("Preparing training data for covariate {.val {covariate}}")

  covariate_name <- grep(covariate, colnames(input_data), value = TRUE)

  input_data$Response <- input_data[[covariate_name]]

  input_data %>%
    dplyr::select(Response,
                  dplyr::starts_with("Dim.")) %>%
    tidyr::drop_na() -> input_data

  ## ---------------------------------------------------------------------------
  ## Step 1: Data Preparation
  ## ---------------------------------------------------------------------------

  set.seed(0307)

  split_obj  <- rsample::initial_split(input_data, strata = Response)
  Train_Data <- rsample::training(split_obj)
  Test_Data  <- rsample::testing(split_obj)
  CV_Folds   <- rsample::vfold_cv(Train_Data, v = 3)

  ## ---------------------------------------------------------------------------
  ## Step 2: Define and Tune Cubist Model
  ## ---------------------------------------------------------------------------

  parsnip::cubist_rules(committees = tune::tune(),
                        neighbors  = tune::tune(),
                        max_rules  = tune::tune()) %>%
    parsnip::set_engine("Cubist") %>%
    parsnip::set_mode("regression") -> model_spec

  workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_formula(Response ~ .) -> wf

  future::plan(future::multisession, workers = parallel::detectCores(logical = TRUE) - 1)

  dials::grid_space_filling(rules::committees(range = c(2L, 20L)),
                            dials::neighbors(range = c(2L, 9L)),
                            dials::max_rules(),
                            size = 5,
                            type = "max_entropy") -> grid

  cli::cli_progress_step("Running grid search for {.val {covariate}}")

  tune::tune_grid(object    = wf,
                  resamples = CV_Folds,
                  grid      = grid,
                  control   = tune::control_grid(allow_par = TRUE)) -> grid_res

  cli::cli_progress_step("Running Bayesian optimization for {.val {covariate}}")

  tune::tune_bayes(object    = wf,
                   resamples = CV_Folds,
                   initial   = grid_res,
                   iter      = 2,
                   control   = tune::control_bayes(allow_par = TRUE)) -> bayes_res

  best_params <- tune::select_best(bayes_res,
                                   metric = "rmse")

  final_wf    <- tune::finalize_workflow(wf,
                                         best_params)

  future::plan(future::sequential)

  ## ---------------------------------------------------------------------------
  ## Step 3: Final Fit and Evaluation
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Evaluating final model for {.val {covariate}}")

  final_wf %>%
    tune::last_fit(split_obj) %>%
    tune::collect_predictions() %>%
    dplyr::rename(Predicted = .pred) %>%
    soilspec::eval(pred = .$Predicted,
                   obs  = .$Response,
                   obj  = "quant") -> eval

  fitted_model <- final_wf %>% parsnip::fit(Train_Data)

  return(list(Model           = fitted_model,
              Best_Parameters = best_params,
              Evaluation      = eval))

}
