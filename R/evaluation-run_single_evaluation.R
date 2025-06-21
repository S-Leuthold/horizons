#' Evaluate a Single Model Configuration on Spectral Data
#'
#' Builds, tunes, fits, and evaluates a single spectral model configuration for a specified response variable.
#' This includes response transformation, spectral preprocessing, optional covariate inclusion, grid search,
#' Bayesian optimization, final model fitting, and hold-out evaluation. Designed for use within
#' `safe_run_model()` in the `horizons` batch modeling pipeline.
#'
#' @param input_data A `tibble` containing spectral features with columns: `Sample_ID`, `Wavenumber`, `Absorbance`,
#'   and the target response variable.
#' @param covariate_data Optional `tibble` containing external covariates, matched by `Sample_ID`.
#'   Required if `include_covariates = TRUE`.
#' @param variable Character. Name of the response variable to model (must be in `input_data`).
#' @param model Character. Model type to evaluate (e.g., `"Cubist"`, `"PLSR"`, `"SVM"`).
#' @param transformation Character. Outcome transformation label (e.g., `"Log Transformation"`).
#' @param preprocessing Character. Spectral preprocessing method (e.g., `"SNV + SG1"`).
#' @param covariates Optional character vector of covariate names to include in the model.
#' @param include_covariates Logical. Whether to include covariates in the model workflow.
#' @param pruning Logical. Whether to skip poor-performing models after grid tuning based on RRMSE threshold (default = `TRUE`).
#' @param grid_size Integer. Number of initial grid search candidates (default = 10).
#' @param bayesian_iter Integer. Number of Bayesian tuning iterations (default = 15).
#' @param cv_folds Integer. Number of cross-validation folds for resampling (default = 5).
#'
#' @return A named `list` containing:
#' \itemize{
#'   \item \strong{evaluation_results}: A `tibble` of hold-out performance metrics (RRMSE, R², etc.).
#'   \item \strong{tuned_models}: A `tibble` containing the full tuned workflow, final workflow, and fitted model (stack-compatible).
#'   \item \strong{error}: Logical flag indicating whether the model failed.
#'   \item \strong{pruned}: Logical flag indicating whether the model was pruned due to poor early results.
#'   \item \strong{wflow_id}: Character ID of the workflow.
#'   \item \strong{reason}: Text summary of any error or pruning reason (or `NULL` if successful).
#' }
#'
#' @details
#' The function proceeds through the full modeling lifecycle:
#' \enumerate{
#'   \item Build recipe with response transformation, spectral preprocessing, and optional covariates.
#'   \item Define the model specification via `define_model_specifications()`.
#'   \item Perform grid search using `tune::tune_grid()` to explore hyperparameter space.
#'   \item Optionally prune poor configurations early using RRMSE thresholds.
#'   \item Refine using Bayesian optimization (`tune::tune_bayes()`).
#'   \item Finalize, fit, and evaluate the best model on a hold-out set.
#'   \item Return workflows and evaluation outputs for stacking and inspection.
#' }
#'
#' All stages use `safely_execute()` for robust error handling. Messages are emitted via `cli` to support real-time monitoring.
#' The function supports automatic `mtry` finalization and dynamic tuning grid construction.
#'
#' @examples
#' \dontrun{
#' config_result <- evaluate_model_config(
#'   input_data        = my_spectral_data,
#'   covariate_data    = my_covs,
#'   variable          = "MAOM_C_g_kg",
#'   model             = "cubist",
#'   transformation    = "Log",
#'   preprocessing     = "snv",
#'   covariates        = c("Clay", "pH"),
#'   include_covariates = TRUE
#' )
#'
#' config_result$evaluation_results
#' }
#'
#' @seealso
#' \code{\link{safe_run_model}}, \code{\link{run_model_evaluation}},
#' \code{\link{build_recipe}}, \code{\link{define_model_specifications}},
#' \code{\link{evaluate_final_models}}
#'
#' @importFrom dplyr filter mutate select pull arrange rename bind_cols
#' @importFrom tibble tibble as_tibble
#' @importFrom recipes prep bake
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid tune_bayes select_best finalize_workflow control_grid control_bayes collect_metrics
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom yardstick metric_set rsq
#' @importFrom hardhat extract_parameter_set_dials
#' @importFrom glue glue
#' @importFrom cli cli_alert_success cli_alert_warning
#' @export


evaluate_model_config <- function(input_data,
                                  covariate_data,
                                  variable,
                                  model,
                                  transformation,
                                  preprocessing,
                                  covariates,
                                  include_covariates,
                                  pruning       = TRUE,
                                  grid_size     = 10,
                                  bayesian_iter = 15,
                                  cv_folds      = 5) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Split and Fold
  ## ---------------------------------------------------------------------------

  stopifnot(variable %in% colnames(input_data))

  wflow_id <- clean_workflow_id(model          = model,
                                transformation = transformation,
                                preprocessing  = preprocessing,
                                covariates     = covariates)

  input_data <- dplyr::rename(input_data, Response = !!rlang::sym(variable))
  split      <- rsample::initial_split(input_data, prop = 0.8, strata = Response)
  train      <- rsample::training(split)
  test       <- rsample::testing(split)
  folds      <- rsample::vfold_cv(train, v = cv_folds)

  ## ---------------------------------------------------------------------------
  ## Step 2: Build Recipe + Workflow
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Building recipe and workflow.")

  safely_execute(expr = {build_recipe(input_data              = train,
                                      response_transformation = transformation,
                                      spectral_transformation = preprocessing,
                                      covariate_selection     = covariates,
                                      covariate_data          = covariate_data)},
              default_value = NULL,
              error_message = "Failed to build recipe for {wflow_id}") -> recipe_safe

  recipe <- recipe_safe$result

  if (is.null(recipe)) {

    cli::cli_alert_warning("Skipping model: Recipe construction failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = conditionMessage(recipe_safe$error)))
    }

  ## ---------------------------------------------------------------------------

  safely_execute(expr = {define_model_specifications(model)},
                 default_value = NULL,
                 error_message = "Model specification failed for {wflow_id}") -> model_spec_safe

  model_spec <- model_spec_safe$result

  if (is.null(model_spec)) {

    cli::cli_alert_warning("Skipping model: Model specification failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = conditionMessage(model_spec_safe$error)))
  }

  ## ---------------------------------------------------------------------------

  safely_execute(expr = {workflows::workflow() %>%
                          workflows::add_recipe(recipe) %>%
                          workflows::add_model(model_spec)},
                 default_value = NULL,
                 error_message = "Workflow creation failed for {wflow_id}") -> workflow_safe

  workflow <- workflow_safe$result

  if (is.null(workflow)) {

    cli::cli_alert_warning("Skipping model: Workflow construction failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = conditionMessage(workflow_safe$error)))
  }

  ##----------------------------------------------------------------------------

  param_set <- hardhat::extract_parameter_set_dials(workflow)

  if("mtry" %in% param_set$name) {

    recipe %>%
      recipes::prep() %>%
      recipes::bake(new_data = NULL) %>%
      dplyr::select(-Project, -Sample_ID, -Response)-> eval_data

    hardhat::extract_parameter_set_dials(workflow) %>%
      dials::finalize(eval_data) -> param_set

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Initial Grid Tuning
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Running initial grid search.")

  suppressWarnings({
    suppressMessages({
      safely_execute(expr = {tune::tune_grid(object     = workflow,
                                             resamples  = folds,
                                             param_info = param_set,
                                             grid       = grid_size,
                                             metrics    = yardstick::metric_set(rrmse, rsq),
                                             control    = tune::control_grid(save_pred     = FALSE,
                                                                             save_workflow = TRUE,
                                                                             verbose       = FALSE,
                                                                             allow_par     = TRUE,
                                                                             parallel_over = "everything"))},
                     default_value = NULL,
                     error_message = "Grid tuning failed for {wflow_id}") -> grid_res_safe

    })
  })

  grid_res <- grid_res_safe$result

  if (is.null(grid_res)) {

    cli::cli_alert_warning("Skipping model: Initial grid search failed.")

    notes_out <- capture.output(tune::show_notes(.Last.tune.result))

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = notes_out))
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Filter Poor Fits (Early Exit)
  ## ---------------------------------------------------------------------------

  if(pruning){

    safely_execute(expr = {min(tune::collect_metrics(grid_res) %>%
                            dplyr::filter(.metric == "rrmse") %>%
                            dplyr::pull(mean), na.rm = TRUE)},
                   default_value = Inf,
                   error_message = "Failed to calculate minimum RRMSE for {wflow_id}") -> min_rrmse_safe

    min_rrmse <- min_rrmse_safe$result

    if (is.infinite(min_rrmse) || min_rrmse > 50) {

      cli::cli_alert_warning("Skipping {wflow_id}: Poor performance in initial tuning (RRMSE = {round(min_rrmse, 1)}%)")
      return(list(evaluation_results = NULL,
                  tuned_models       = NULL,
                  error              = FALSE,
                  pruned             = TRUE,
                  wflow_id           = wflow_id,
                  reason             = glue::glue("Initial grid search RRMSE too high ({round(min_rrmse, 1)}%)")))
      }
    }

  ## ---------------------------------------------------------------------------
  ## Step 5: Bayesian Tuning
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Running Bayesian tuning.")

  suppressWarnings({
    suppressMessages({
      safely_execute(expr = {tune::tune_bayes(object     = workflow,
                                              initial    = grid_res,
                                              resamples  = folds,
                                              param_info = param_set,
                                              iter       = bayesian_iter,
                                              metrics    = yardstick::metric_set(rrmse, rsq),
                                              control    = tune::control_bayes(save_pred     = FALSE,
                                                                               save_workflow = TRUE,
                                                                               verbose       = FALSE,
                                                                               seed          = 307,
                                                                               no_improve    = 10L,
                                                                               allow_par     = TRUE,
                                                                               parallel_over = "everything"))},
                     default_value  = NULL,
                     error_message  = "Bayesian tuning failed for {wflow_id}") -> bayes_res_safe
    })
  })

  bayes_res <- bayes_res_safe$result

  if (is.null(bayes_res)) {

    cli::cli_alert_warning("Skipping model: Bayesian tuning failed.")

    notes_out <- capture.output(tune::show_notes(.Last.tune.result))

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = notes_out))
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Finalize + Fit
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Finalizing and fitting best models.")

  safely_execute(expr = {best_model <- tune::select_best(bayes_res, metric = "rrmse")
                         final_wf   <- tune::finalize_workflow(workflow, best_model)
                         fitted_wf  <- parsnip::fit(final_wf, data = train)
                         tibble::tibble(wflow_id  = wflow_id,
                                        workflow  = list(workflow),
                                        result    = list(bayes_res),
                                        final_wf  = list(final_wf),
                                        fitted_wf = list(fitted_wf))},
                 default_value = NULL,
                 error_message = "Finalization failed for {wflow_id}") -> finalized_wf_safe

  finalized_wf <- finalized_wf_safe$result

  if (is.null(finalized_wf)) {

    cli::cli_alert_warning("Skipping model: Model finalization failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = finalized_wf_safe$errors))
  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Evaluate fitted models on holdout data.
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {evaluate_final_models(finalized_wf_sets = finalized_wf,
                                               holdout_data      = test)},
                 default_value = NULL,
                 error_message = "Holdout evaluation failed for {wflow_id}") -> evaluation_res_safe

  evaluation_res <- evaluation_res_safe$result

  if (is.null(evaluation_res)) {

    cli::cli_alert_warning("Skipping model: Holdout evaluation failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = conditionMessage(evaluation_res_safe$error)))
  }

  ## ---------------------------------------------------------------------------
  ## Step 8: Collect and return results.
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Evaluation completed successfully: RRMSE = {round(evaluation_res$rrmse, 2)}%, R-squared = {round(evaluation_res$rsq * 100, 3)}%")

  return(list(evaluation_results = evaluation_res,
              tuned_models       = finalized_wf,
              error              = FALSE,
              pruned             = FALSE,
              wflow_id           = wflow_id,
              reason             = NULL))
}

