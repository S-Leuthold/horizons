#' Fit a Cubist Model Using Similarity-Selected Training Data
#'
#' Builds, tunes, and evaluates a Cubist model to predict a single soil covariate
#' using similarity-selected OSSL training data. The function applies a max entropy
#' grid search followed by Bayesian optimization to tune hyperparameters, then fits
#' the final model and returns both performance metrics and workflow objects.
#'
#' @param train_data A `tibble` containing PCA-transformed predictors (`Dim.1`, `Dim.2`, etc.)
#'   and the target covariate column. Should be pre-selected similar samples.
#' @param val_data A `tibble` with validation data (same structure as train_data)
#'   for computing unbiased performance metrics.
#' @param covariate A character string. Name of the target covariate (e.g., `"clay"`, `"ph"`).
#' @param verbose Logical. If `TRUE`, prints progress messages. Defaults to `FALSE`.
#' @param parallel Logical. Enable parallel processing for tuning. Defaults to `FALSE`.
#' @param n_workers Integer. Number of parallel workers. If `NULL`, uses safe defaults.
#' @param allow_nested Logical. Allow parallel processing in nested context. Defaults to `FALSE`.
#' @param bayesian_iter Integer. Number of Bayesian optimization iterations (default: 10).
#'
#' @return A named `list` with the following components:
#' \itemize{
#'   \item \strong{fitted_workflow}: A fitted Cubist workflow trained on train_data.
#'   \item \strong{best_params}: A `tibble` with the best hyperparameter configuration.
#'   \item \strong{validation_metrics}: A `tibble` of metrics computed on val_data.
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
#' Performance metrics are computed on the validation set using custom metric functions.
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
#' \code{\link{predict_soil_covariates}}, \code{\link{perform_pca_on_ossl}}
#'
#' @importFrom dplyr select rename mutate bind_rows starts_with
#' @importFrom purrr map
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom stats quantile predict
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom recipes recipe
#' @importFrom dials grid_space_filling neighbors
#' @importFrom rules committees max_rules
#' @importFrom parsnip cubist_rules set_engine set_mode fit
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid tune_bayes control_grid control_bayes select_best finalize_workflow last_fit collect_predictions
#' @importFrom future plan multisession sequential
#' @importFrom glue glue
#' @importFrom cli cli_progress_step cli_alert_danger cli_alert_warning
#' @export


fit_cubist_model <- function(train_data,
                             val_data,
                             covariate,
                             verbose       = FALSE,
                             parallel      = FALSE,
                             n_workers     = NULL,
                             bayesian_iter = 10) {

  ## Notes ---------------------------------------------------------------------

  # TODO: Update CLI messages to use tree structure. (9/10 SJL)

  ## ---------------------------------------------------------------------------
  ## Step 0: Setup
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Step 0.1: Validation checks
    ## ---------------------------------------------------------------------------

    if (verbose) {

      cli::cli_progress_step("Preparing training data for covariate {.val {covariate}}")

    }

    ## Make sure the covariate requested is in the test data ---------------------

    if (!covariate %in% names(train_data)) {

      # TODO: Move this validation to orchestrator level
      cli::cli_abort("Covariate '{covariate}' not found in train_data")

    }

    ## Make sure the covariate requested is in the validation data ---------------

    if (!covariate %in% names(val_data)) {

      # TODO: Move this validation to orchestrator level
      cli::cli_abort("Covariate '{covariate}' not found in val_data")

    }

    ## ---------------------------------------------------------------------------
    ## Step 0.2: Prepare data for modeling
    ## ---------------------------------------------------------------------------

    train_data %>%
      dplyr::mutate(Response = .data[[covariate]]) %>%
      dplyr::select(Response, dplyr::starts_with("Dim.")) %>%
      tidyr::drop_na() -> Train_Data

    val_data %>%
      dplyr::mutate(Response = .data[[covariate]]) %>%
      dplyr::select(Response, dplyr::starts_with("Dim.")) %>%
      tidyr::drop_na() -> Val_Data

    ## Make sure there's enough data to actually model ---------------------------

    if (nrow(Train_Data) < 20) {

      # TODO: Move this validation to orchestrator level
      cli::cli_abort("Insufficient training data for {covariate}: {nrow(Train_Data)} samples (minimum 20)")

    }

    if (nrow(Val_Data) < 10) {

      # TODO: Move this validation to orchestrator level
      cli::cli_abort("Insufficient validation data for {covariate}: {nrow(Val_Data)} samples (minimum 10)")

    }

  ## ---------------------------------------------------------------------------
  ## Step 1: Cross-validation setup
  ## ---------------------------------------------------------------------------

  set.seed(0307)

  safely_execute(expr = {rsample::vfold_cv(Train_Data, v = 10, strata = "Response")},
                 default_value      = NULL,
                 error_message      = "Failed to create CV folds for {covariate}",
                 capture_conditions = TRUE) -> CV_Folds_safe


  handle_results(safe_result   = CV_Folds_safe,
                 error_title   = glue::glue("Failed to create cross-validation folds for {covariate}:"),
                 error_hints   = c("Check that 'Response' column exists in Train_Data",
                                   "Ensure Response has at least 10 unique values for stratification",
                                   "Verify Response is not all NA values",
                                   "If Response is highly imbalanced, stratification may fail"),
                 abort_on_null = TRUE,
                 silent        = FALSE) -> CV_Folds


  ## -------------------------------------------------------------------------
  ## Step 2: Define recipe and model specifications
  ## -------------------------------------------------------------------------

  formula_obj  <- stats::as.formula("Response ~ .")
  model_recipe <- recipes::recipe(formula_obj, data = Train_Data[1, ])

  ## -------------------------------------------------------------------------
  ## Step 3: Define cubist model specifications
  ## -------------------------------------------------------------------------

  ## Create model object -----------------------------------------------------

  parsnip::cubist_rules(committees = tune::tune(),
                        neighbors  = tune::tune(),
                        max_rules  = tune::tune()
                        ) %>%
    parsnip::set_engine("Cubist") %>%
    parsnip::set_mode("regression") -> model_spec

  ## Create workflow ---------------------------------------------------------

  workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(model_recipe) -> wf

  ## -------------------------------------------------------------------------
  ## Step 4: Initial Grid Search for Hyperparameter Tuning
  ## -------------------------------------------------------------------------

  ## Configure parallel backend ----------------------------------------------

  if (is.null(n_workers)) {

    n_workers <- 1

  }

  ## ---------------------------------------------------------------------------

  if (parallel && n_workers > 1) {

    old_plan <- future::plan()

    ## Store and increase globals size limit for large spectral data
    old_maxSize <- getOption("future.globals.maxSize")
    options(future.globals.maxSize = 2 * 1024^3)  # Set to 2GB for spectral workflows

    on.exit({
      future::plan(old_plan)
      options(future.globals.maxSize = old_maxSize)  # Restore original limit
    }, add = TRUE)

    future::plan(future::multisession, workers = n_workers)

    }

  ## Create tuning grid ------------------------------------------------------

  grid_size <- 10

  dials::grid_space_filling(rules::committees(range = c(5L, 15L)),
                            dials::neighbors(range  = c(0L, 5L)),
                            dials::max_rules(range  = c(25L, 75L)),
                            size = grid_size,
                            type = "latin_hypercube") -> grid

  ## Run grid search ---------------------------------------------------------

  if(verbose) cli::cli_progress_step("Running grid search for {.val {covariate}}")

  safely_execute(expr = {tune::tune_grid(object    = wf,
                                         resamples = CV_Folds,
                                         grid      = grid,
                                         control   = tune::control_grid(allow_par = TRUE,
                                                                        save_pred = FALSE,
                                                                        extract   = NULL))},
                 default_value      = NULL,
                 error_message      = glue::glue("Grid tuning failed for {covariate}"),
                 log_error          = FALSE,  # We'll handle error reporting ourselves
                 capture_conditions = TRUE) -> grid_res_safe

  handle_results(safe_result   = grid_res_safe,
                 error_title   = glue::glue("Grid tuning failed for {covariate}"),
                 error_hints   = NULL,
                 abort_on_null = FALSE,
                 silent        = FALSE) -> grid_res

  ## If grid search failed, return NULL but don't kill whole run ---------------

  if (is.null(grid_res)) {

    if (verbose && !is.null(grid_res_safe$error)) {
      cli::cli_text("│  │  │  └─ {cli::col_red('✗ Grid search failed: {grid_res_safe$error$message}')}")
    }

    return(NULL)

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Bayesian optimization tuning
  ## ---------------------------------------------------------------------------

  if (bayesian_iter > 0) {

    if(verbose) {

      opt_text <- paste0("Bayesian optimization: ", bayesian_iter, " iterations")

      cli::cli_text(format_tree_item(opt_text, level = 1, is_last = FALSE))

    }

    ## Run Bayesian optimization ---------------------------------------------

    safely_execute(expr = {tune::tune_bayes(object    = wf,
                                            resamples = CV_Folds,
                                            initial   = grid_res,
                                            iter      = bayesian_iter,
                                            control   = tune::control_bayes(allow_par = TRUE,
                                                                            save_pred = FALSE,
                                                                            extract   = NULL))},
                   default_value      = NULL,
                   error_message      = glue::glue("Bayesian tuning failed for {covariate}"),
                   log_error          = FALSE,  # We'll handle error reporting ourselves
                   capture_conditions = TRUE) -> bayes_res_safe

    handle_results(safe_result   = bayes_res_safe,
                   error_title   = glue::glue("Bayesian optimization failed for {covariate}:"),
                   error_hints   = NULL,
                   abort_on_null = FALSE,
                   silent        = FALSE) -> bayes_res

    ## Fallback to grid results if Bayesian fails ----------------------------

    if (is.null(bayes_res)) {

      if (verbose && !is.null(bayes_res_safe$error)) {
        cli::cli_text("│  │  │  └─ {cli::col_yellow('⚠ Bayesian optimization failed, using grid results')}")
      }
      bayes_res <- grid_res

    }

    final_tune_result <- bayes_res

    } else {

      if(verbose) {
        cli::cli_text(format_tree_item("Skipping Bayesian optimization (using grid search only)", level = 1, is_last = FALSE))
      }

    final_tune_result <- grid_res

    }

  ## ---------------------------------------------------------------------------
  ## Step 6: Finalize the workflow
  ## ---------------------------------------------------------------------------

  ## TODO: Tree structure message

  if(verbose) cli::cli_progress_step("Finalizing workflow for {.val {covariate}}")

  best_params <- tune::select_best(final_tune_result, metric = "rmse")
  final_wf    <- tune::finalize_workflow(wf, best_params)

  ## ---------------------------------------------------------------------------
  ## Step 7: Fit the final model
  ## ---------------------------------------------------------------------------

  ## TODO: Tree structure message

  if (verbose) cli::cli_progress_step("Fitting final model for {.val {covariate}}")

  safely_execute(expr               = {parsnip::fit(final_wf, Train_Data)},
                 default_value      = NULL,
                 error_message      = "Failed to fit final workflow for {covariate}",
                 log_error          = FALSE,  # We'll handle error reporting ourselves
                 capture_conditions = TRUE) -> fitted_model_safe

  handle_results(safe_result   = fitted_model_safe,
                 error_title   = glue::glue("Failed to fit final model for {covariate}:"),
                 error_hints   = NULL,
                 abort_on_null = FALSE,
                 silent        = FALSE) -> fitted_model

  ## Return NULL if final model fitting failed -------------------------------

  if (is.null(fitted_model)) {

    if (verbose && !is.null(fitted_model_safe$error)) {
      cli::cli_text("│  │  │  └─ {cli::col_red('✗ Final model fitting failed: {fitted_model_safe$error$message}')}")
    }

    return(NULL)

  }

  ## ---------------------------------------------------------------------------
  ## Step 8: Test the fitted maodel on the validation set
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_progress_step("Computing validation metrics for {.val {covariate}}")

  safely_execute(expr = {

    ## Predict on validation set -----------------------------------------------

    val_predictions <- stats::predict(fitted_model, Val_Data)

    ## Calculate validation metrics --------------------------------------------

    tibble::tibble(observed  = Val_Data$Response,
                   predicted = val_predictions$.pred) %>%
      tidyr::drop_na() -> val_data_clean

    val_data_clean %>%
      dplyr::summarise(rmse  = sqrt(mean((observed - predicted)^2)),
                       mae   = mean(abs(observed - predicted)),
                       rsq   = cor(observed, predicted)^2,
                       ccc   = ccc_vec(observed, predicted),
                       rpd   = rpd_vec(observed, predicted),
                       rrmse = rrmse_vec(val_data_clean, truth = observed, estimate = predicted)) -> val_metrics
    },

    default_value      = NULL,
    error_message      = "Validation failed (Step 8)",
    log_error          = FALSE,  # We'll handle error reporting ourselves
    capture_conditions = TRUE) -> val_metrics_safe

  handle_results(safe_result   = val_metrics_safe,
                 error_title   = glue::glue("Failed to compute validation metrics for {covariate}:"),
                 error_hints   = c("Check that validation data has 'Response' column",
                                   "Ensure validation set has sufficient samples (n > 2)"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> val_metrics

  ## Return NULL if validation metrics computation failed ---------------------

  if (is.null(val_metrics)) {

    if (verbose && !is.null(val_metrics_safe$error)) {
      cli::cli_text("│  │  │  └─ {cli::col_red('✗ Validation metrics computation failed: {val_metrics_safe$error$message}')}")
    }

    return(NULL)

  }

  ## ---------------------------------------------------------------------------
  ## Step 9: Return results
  ## ---------------------------------------------------------------------------

  ## Print best parameters for the nerds ---------------------------------------

  if (verbose) {

    best_params_text <- paste0("Best params: ",
                               "committees=", round(best_params$committees),
                               ", neighbors=", round(best_params$neighbors))

    cli::cli_text(format_tree_item(best_params_text, level = 1, is_last = TRUE))
  }

  ## Return results list -------------------------------------------------------

  return(list(fitted_workflow    = fitted_model,
              best_params        = best_params,
              validation_metrics = val_metrics))

}
