#' Evaluate a Single Model Configuration on Spectral Data
#'
#' Executes preprocessing, model specification, grid tuning, Bayesian tuning,
#' final model fitting, and hold-out evaluation for a single model configuration.
#' Intended for use inside `safe_run_model()` within the batch modeling pipeline.
#'
#' @param input_data A tibble containing columns `Sample_ID`, `Wavenumber`, `Absorbance`, and the target variable.
#' @param covariate_data Optional tibble of covariates, must include `Sample_ID`.
#' @param variable Character. The name of the response variable.
#' @param model Character. Model type (e.g. `"Cubist"`, `"PLSR"`).
#' @param transformation Character. Response transformation label.
#' @param preprocessing Character. Spectral preprocessing method.
#' @param covariates List of covariate names to include, or NULL.
#' @param include_covariates Logical. Whether to include covariates.
#' @param grid_size Integer. Number of grid search candidates per model (default = 10).
#' @param bayesian_iter Integer. Number of Bayesian optimization iterations (default = 15).
#' @param cv_folds Integer. Number of cross-validation folds (default = 5).
#'
#' @return A named list with:
#' \describe{
#'   \item{evaluation_results}{Tibble of holdout performance metrics.}
#'   \item{tuned_models}{Tibble with `wflow_id`, `workflow`, and tuning results (stack-compatible).}
#' }
#' @keywords internal

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
              error_message = "Failed to build recipe for {wflow_id}") -> recipe

  if (is.null(recipe)) {

    cli::cli_alert_warning("Skipping model: Recipe construction failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = "Recipe construction failed"))
    }

  ## ---------------------------------------------------------------------------

  safely_execute(expr = {define_model_specifications(model)},
                 default_value = NULL,
                 error_message = "Model Model specification failed for {wflow_id}") -> model_spec

  if (is.null(model_spec)) {

    cli::cli_alert_warning("Skipping model: Model specification failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = "Model specification failed"))
  }

  ## ---------------------------------------------------------------------------

  safely_execute(expr = {workflows::workflow() %>%
                          workflows::add_recipe(recipe) %>%
                          workflows::add_model(model_spec)},
                 default_value = NULL,
                 error_message = "Workflow creation failed for {wflow_id}") -> workflow

  if (is.null(workflow)) {

    cli::cli_alert_warning("Skipping model: Workflow construction failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = "Workflow construction failed"))
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Initial Grid Tuning
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Running initial grid search.")

  safely_execute(expr = {tune::tune_grid(object    = workflow,
                                         resamples = folds,
                                         grid      = grid_size,
                                         metrics   = yardstick::metric_set(rrmse, rsq),
                                         control   = tune::control_grid(save_pred     = FALSE,
                                                                        save_workflow = TRUE,
                                                                        verbose       = FALSE,
                                                                        parallel_over = "resamples"))},
                 default_value = NULL,
                 error_message = "Grid tuning failed for {wflow_id}") -> grid_res

  if (is.null(grid_res)) {

    cli::cli_alert_warning("Skipping model: Initial grid search failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = "Initial grid search failed"))
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Filter Poor Fits (Early Exit)
  ## ---------------------------------------------------------------------------

  if(pruning){

    safely_execute(expr = {min(tune::collect_metrics(grid_res) %>%
                            dplyr::filter(.metric == "rrmse") %>%
                            dplyr::pull(mean), na.rm = TRUE)},
                   default_value = Inf,
                   error_message = "Failed to calculate minimum RRMSE for {wflow_id}") -> min_rrmse


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

  tibble::tibble(wflow_id = wflow_id,
                 info     = list(tibble::tibble(workflow = list(workflow))),
                 result   = list(grid_res),
                 option   = list(list())) -> wf_set

  class(wf_set) <- c("workflow_set", class(wf_set))
  wf_set$rank <- NA_integer_

  safely_execute(expr = {run_bayesian_tuning(tuned_wf_set = wf_set,
                                             folds        = folds,
                                             iterations   = bayesian_iter,
                                             parallel     = FALSE)},
                 default_value = NULL,
                 error_message = "Bayesian tuning failed for {wflow_id}") -> bayes_res

  if (is.null(bayes_res)) {

    cli::cli_alert_warning("Skipping model: Bayesian tuning failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = "Bayesian tuning failed"))
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Finalize + Fit
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Finalizing and fitting best models.")

  safely_execute(expr = {bayes_res %>%
                          dplyr::mutate(best_model = purrr::map(result, tune::select_best, metric = "rrmse"),
                                        final_wf   = purrr::map2(info, best_model, ~ tune::finalize_workflow(.x$workflow[[1]], .y)),
                                        fitted_wf  = purrr::map(final_wf, ~ parsnip::fit(.x, data = train)))},
                 default_value = NULL,
                 error_message = "Finalization failed for {wflow_id}") -> finalized_wf

  if (is.null(finalized_wf)) {

    cli::cli_alert_warning("Skipping model: Model finalization failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = "Model finalization failed"))
  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Evaluate fitted models on holdout data.
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {evaluate_final_models(finalized_wf_sets = finalized_wf,
                                               holdout_data      = test)},
                 default_value = NULL,
                 error_message = "Holdout evaluation failed for {wflow_id}") -> evaluation_res

  if (is.null(evaluation_res)) {

    cli::cli_alert_warning("Skipping model: Holdout evaluation failed.")

    return(list(evaluation_results = NULL,
                tuned_models       = NULL,
                error              = TRUE,
                pruned             = FALSE,
                wflow_id           = wflow_id,
                reason             = "Holdout evaluation failed"))
  }

  ## ---------------------------------------------------------------------------
  ## Step 8: Collect and return results.
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Evaluation completed successfully!")

  return(list(evaluation_results = evaluation_res,
              tuned_models       = finalized_wf,
              error              = FALSE,
              pruned             = FALSE,
              wflow_id           = wflow_id,
              reason             = NULL))
}

