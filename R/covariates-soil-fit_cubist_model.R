#' Fit a Cubist Model Using PCA-Transformed Spectral Data
#'
#' Builds, tunes, and evaluates a Cubist model to predict a single soil covariate
#' using PCA-transformed MIR spectra. The function applies a max entropy grid search
#' followed by Bayesian optimization to tune hyperparameters, then fits the final model
#' and returns both performance metrics and workflow objects.
#'
#' @param input_data A `tibble` or `data.frame` containing PCA-transformed predictors
#'   (`Dim.1`, `Dim.2`, ..., `Dim.n`) and one numeric column corresponding to the
#'   covariate to be modeled. All rows with `NA` in the response are removed.
#' @param covariate A character string. Name of the column to use as the response variable (e.g., `"Sand"`, `"pH"`).
#' @param verbose Logical. If `TRUE`, prints progress messages using `cli::cli_*()` during model training. Defaults to `FALSE`.
#' @param parallel Logical. Enable parallel processing for hyperparameter tuning. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers for tuning. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A named `list` with the following components:
#' \itemize{
#'   \item \strong{Model}: A fitted Cubist workflow (`workflow`) trained on the full training set.
#'   \item \strong{Best_Parameters}: A `tibble` containing the best hyperparameter configuration selected via Bayesian optimization.
#'   \item \strong{Evaluation}: A `tibble` of evaluation metrics (e.g., RMSE, R², CCC) computed on the hold-out set using `soilspec::eval()`.
#' }
#'
#' @details
#' The modeling pipeline follows three main stages:
#' \enumerate{
#'   \item Stratified data split into training/testing sets.
#'   \item Max entropy grid search for tuning `committees`, `neighbors`, and `max_rules` using `tune::tune_grid()`.
#'   \item Bayesian optimization using `tune::tune_bayes()` for refinement.
#' }
#' The model is finalized with `tune::finalize_workflow()` and fitted to the training set.
#' Performance metrics are computed on the hold-out set using `tune::last_fit()` and `soilspec::eval()`.
#'
#' Parallel tuning is enabled with `future::plan(multisession)` and automatically reset afterward.
#' All error handling is wrapped with `safely_execute()` for fault-tolerant orchestration.
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   Dim.1 = rnorm(100),
#'   Dim.2 = rnorm(100),
#'   Dim.3 = rnorm(100),
#'   Sand  = runif(100, 50, 80)
#' )
#'
#' result <- fit_cubist_model(input_data = df, covariate = "Sand", verbose = TRUE)
#' result$Evaluation
#' }
#'
#' @seealso
#' \code{\link{predict_covariates}}, \code{\link{evaluate_predictions}}, \code{\link{reduce_dimensions_pca}}
#'
#' @importFrom dplyr select rename mutate bind_rows starts_with
#' @importFrom purrr map
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom stats quantile
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom dials grid_space_filling neighbors
#' @importFrom rules committees max_rules
#' @importFrom parsnip cubist_rules set_engine set_mode fit
#' @importFrom workflows workflow add_model add_formula
#' @importFrom tune tune_grid tune_bayes control_grid control_bayes select_best finalize_workflow last_fit collect_predictions
#' @importFrom future plan multisession sequential
#' @importFrom glue glue
#' @importFrom cli cli_progress_step cli_alert_danger cli_alert_warning
#' @export


fit_cubist_model <- function(input_data,
                             covariate,
                             verbose,
                             parallel = FALSE,
                             n_workers = NULL,
                             allow_nested = FALSE) {

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

    ## Step 2.1: Configure parallel processing with safety controls -----------

    # Determine safe worker count
    if (is.null(n_workers)) {
      max_cores <- parallel::detectCores(logical = TRUE)
      n_workers <- pmax(1, pmin(max_cores - 1, 10))  # Cap at 10 for safety
    }

    # Check for nested parallelization
    current_plan_class <- class(future::plan())[1]
    if (!allow_nested && !identical(current_plan_class, "sequential")) {
      if(verbose) cli::cli_alert_warning("Nested parallelization detected. Setting parallel=FALSE for safety")
      parallel <- FALSE
    }

    # Set parallel plan with proper cleanup
    if (parallel && n_workers > 1) {
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      
      safely_execute(expr          = {future::plan(future::multisession, workers = n_workers)},
                     default_value = NULL,
                     error_message = glue::glue("Failed to set parallel plan for {covariate} tuning"))
    } else {
      if(verbose) cli::cli_alert_info("Using sequential processing for {.val {covariate}} (parallel={parallel}, n_workers={n_workers})")
    }

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
