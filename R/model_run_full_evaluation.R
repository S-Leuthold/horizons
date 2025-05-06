#' Run Full Ensemble Model Evaluation on Spectral Data
#'
#' Constructs and evaluates an ensemble of models to predict a soil property using
#' MIR spectra and optional covariates. The function performs model grid construction,
#' recipe creation, hyperparameter tuning (grid + Bayesian), model finalization,
#' and evaluation on a hold-out test set. Designed to support high-throughput model screening
#' and optimization in soil spectroscopy contexts.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom cli cli_alert_danger cli_progress_step cli_progress_done
#' @importFrom tune select_best control_grid finalize_workflow
#' @importFrom yardstick metric_set
#' @importFrom workflowsets workflow_map
#'
#' @param input_data A data frame containing columns `Sample_ID`, `Wavenumber`, `Absorbance`, and the response variable to predict.
#' @param models Character vector of model types to include (e.g., `"Cubist"`, `"PLSR"`, `"Random Forest"`).
#' @param transformations Character vector of response transformation labels. Options include:
#'   `"No Transformation"`, `"Log Transformation"`, `"Square Root Transformation"`, `"Box-Cox Transformation"`.
#' @param preprocessing Character vector of spectral preprocessing methods. Options include:
#'   `"No Preprocessing"`, `"Savitzky Golay - 0 Deriv"`, `"Savitzky Golay - 1 Deriv"`, and SNV variants.
#' @param variable Character string specifying the name of the response variable to model (must be present in `input_data`).
#' @param include_covariates Logical. If `TRUE`, includes additional covariates during modeling.
#' @param covariate_data Optional data frame of covariates (must include `Sample_ID` column). Required if `include_covariates = TRUE`.
#' @param expand_covariate_grid Logical. If `TRUE`, generates all possible covariate subsets for inclusion in the model grid.
#' @param grid_size Integer. Number of parameter combinations to evaluate per model in the initial grid search (default = 10).
#' @param bayesian_iter Integer. Number of Bayesian tuning iterations per model (default = 15).
#' @param cv_folds Integer. Number of cross-validation folds used in resampling (default = 5).
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{evaluation_results}{A tibble with hold-out performance metrics for each finalized model (`rsq`, `rmse`, `rrmse`).}
#'   \item{tuned_models}{A tibble of tuned workflows, including `final_wf`, `fitted_wf`, and back-transformed tuning results.}
#'   \item{training_data}{The internal training split used during model fitting.}
#'   \item{evaluation_data}{The held-out test set used to evaluate final models.}
#'}
#'
#' @details
#' This function integrates several modular components from the `tidymodels` and `workflowsets` ecosystems.
#' It supports flexible ensemble definitions and prioritizes interpretability of evaluation metrics.
#' The full modeling lifecycle is run automatically, including recipe generation, tuning,
#' and model fitting across a user-defined grid of modeling strategies.
#'
#' @section Model Types:
#' Supported models include:
#' \itemize{
#'   \item Partial Least Squares Regression (PLSR)
#'   \item Cubist
#'   \item Random Forest
#'   \item Support Vector Machine (SVM)
#'   \item Bagged Neural Network (BNN)
#' }
#'
#' @seealso
#' [build_model_grid()], [build_recipe()], [run_bayesian_tuning()], [evaluate_final_models()],
#' [workflowsets::workflow_set()], [tune::tune_grid()], [yardstick::metric_set()]
#'
#' @examples
#' \dontrun{
#' results <- full_model_evaluation(
#'   input_data            = my_spectra_data,
#'   models                = c("Cubist", "PLSR"),
#'   transformations       = c("No Transformation", "Log Transformation"),
#'   preprocessing         = c("Savitzky Golay - 0 Deriv", "SNV + SG1"),
#'   variable              = "SOC",
#'   include_covariates    = TRUE,
#'   covariate_data        = soil_covariates,
#'   expand_covariate_grid = FALSE,
#'   grid_size             = 10,
#'   bayesian_iter         = 15,
#'   cv_folds              = 5
#' )
#' }
#'
#' @export

full_model_evaluation <- function(input_data,
                                  models,
                                  transformations,
                                  preprocessing,
                                  variable,
                                  include_covariates    = FALSE,
                                  covariate_data        = NULL,
                                  expand_covariate_grid = FALSE,
                                  grid_size             = 10,
                                  bayesian_iter         = 15,
                                  cv_folds              = 5) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Data Preprocessing
  ## ---------------------------------------------------------------------------

  missing_vars <- setdiff(variable, colnames(input_data))

  if (length(missing_vars) > 0) {
    stop("The following response variables were not found in `input_data`: ",
         paste(missing_vars, collapse = ", "))
  }

  set.seed(0307)

  input_data      <- dplyr::rename(input_data, Response = !!rlang::sym(variable))
  data_split      <- rsample::initial_split(input_data, prop = 0.8, strata = Response)
  training_data   <- rsample::training(data_split)
  evaluation_data <- rsample::testing(data_split)
  folds           <- rsample::vfold_cv(training_data, v = cv_folds)

  ## ---------------------------------------------------------------------------
  ## Step 2: Build model grid
  ## ---------------------------------------------------------------------------

  tryCatch({
    model_grid <- build_model_grid(models                = models,
                                   transformations       = transformations,
                                   preprocessing         = preprocessing,
                                   variable              = variable,
                                   include_covariates    = include_covariates,
                                   covariate_data        = covariate_data,
                                   expand_covariate_grid = expand_covariate_grid)
  }, error = function(e) {
    cli::cli_alert_danger("Model grid construction failed: {e$message}")
    stop("Model evaluation run terminated.")
  })

   cli::cli_progress_step("Model grid constructed with {.val {nrow(model_grid)}} combinations.")

  ## ---------------------------------------------------------------------------
  ## Step 3: Set up Workflow Sets
  ## ---------------------------------------------------------------------------

  tryCatch({
    wf_grid <- tibble::tibble(
      model_spec = purrr::map(model_grid$Model, define_model_specifications),
      recipe     = purrr::pmap(list(model_grid$Transformation,
                                    model_grid$Preprocessing,
                                    model_grid$Covariates),
                               ~ build_recipe(input_data              = training_data,
                                              response_transformation = ..1,
                                              spectral_transformation = ..2,
                                              covariate_selection     = ..3,
                                              covariate_data           = covariate_data))
    )
  }, error = function(e) {
    cli::cli_alert_danger("Could not create initial workflow grid: {e$message}")
    stop("Model evaluation run terminated.")
  })

   cli::cli_progress_step("Initial workflow grid created.")

  tryCatch({
    wf_set <- workflowsets::workflow_set(
      preproc = wf_grid$recipe,
      models  = wf_grid$model_spec,
      cross   = FALSE
    )
  }, error = function(e) {
    cli::cli_alert_danger("Could not assign the workflow grid to a `workflow_set` object.")
    cat(conditionMessage(e))
    stop("Model evaluation run terminated.")
  })

   cli::cli_progress_step("Initial workflow set created.")

  tryCatch({
    wf_set <- dplyr::mutate(wf_set,
                            wflow_id = clean_workflow_id(
                              model          = model_grid$Model,
                              transformation = model_grid$Transformation,
                              preprocessing  = model_grid$Preprocessing,
                              covariates     = model_grid$Covariates
                            ))
  }, error = function(e) {
    cli::cli_alert_danger("Could not add clean workflow IDs to the workflow set.")
    cat(conditionMessage(e))
    stop("Model evaluation run terminated.")
  })

  cli::cli_progress_step("Finalized workflow set created.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Tune and Calibrate Models
  ## ---------------------------------------------------------------------------

  control_grid <- tune::control_grid(
    save_pred     = TRUE,
    allow_par     = TRUE,
    save_workflow = TRUE,
    verbose       = FALSE,
    parallel_over = "everything"
  )

  cli::cli_progress_step("Starting initial grid search: {.val {grid_size * nrow(wf_set) * cv_folds}} models to be evaluated.")

  tryCatch({
    initial_tune_results <- workflowsets::workflow_map(
      object    = wf_set,
      fn        = "tune_grid",
      resamples = folds,
      metrics   = yardstick::metric_set(rrmse, rsq),
      grid      = grid_size,
      control   = control_grid
    )
  }, error = function(e) {
    cli::cli_alert_danger("Errors occurred during initial model tuning.")
    cat(conditionMessage(e))
    stop("Model evaluation run terminated.")
  })

  cli::cli_progress_step("Initial hyperparameter search complete.")

  halfway_tuned  <- filter_workflows(wf_set_tuned = initial_tune_results)
  wf_set_updated <- halfway_tuned$passed_workflows

  cli::cli_progress_step("{nrow(wf_set_updated)} of {nrow(wf_set)} workflows passed initial filter.")

  cli::cli_progress_step("Starting Bayesian tuning: {.val {bayesian_iter * nrow(wf_set_updated) * cv_folds}} models to be evaluated.")

  tryCatch({
    tuned_models <- run_bayesian_tuning(
      tuned_wf_set = wf_set_updated,
      folds        = folds,
      iterations   = bayesian_iter,
      parallel     = TRUE
    )
  }, error = function(e) {
    cli::cli_alert_danger("Errors occurred during Bayesian tuning.")
    cat(conditionMessage(e))
    stop("Model evaluation run terminated.")
  })

  cli::cli_progress_step("Bayesian tuning complete. Back-transforming predictions...")

  tuned_models %>%
    dplyr::mutate(result = purrr::map2(result,
                                       wflow_id,
                                       backtransform_tune_results)) -> tuned_models

  ## ---------------------------------------------------------------------------
  ## Step 5: Finalize Tuned Models
  ## ---------------------------------------------------------------------------

  tuned_models %>%
    dplyr::mutate(best_model = purrr::map(result, tune::select_best, metric = "rrmse"),
                  workflow   = purrr::map(info, ~ .x$workflow[[1]]),
                  final_wf   = purrr::map2(workflow, best_model, tune::finalize_workflow),
                  fitted_wf  = purrr::map(final_wf, ~ parsnip::fit(.x, data = training_data))) -> tuned_models

  cli::cli_progress_step("Tuned models finalized.")

  ## ---------------------------------------------------------------------------
  ## Step 6: Predict and Evaluate on Hold-Out
  ## ---------------------------------------------------------------------------

  evaluate_final_models(finalized_wf_sets = tuned_models,
                        holdout_data      = evaluation_data) -> evaluation_results

  cli::cli_progress_step("Model evaluation complete. Minimum RRMSE = {round(min(evaluation_results$rrmse), 3)}%, Maximum RRMSE = {round(max(evaluation_results$rrmse), 3)}%")
  cli::cli_progress_done()

  return(list(
    evaluation_results = evaluation_results,
    tuned_models       = tuned_models,
    training_data      = training_data,
    evaluation_data    = evaluation_data
  ))
}

