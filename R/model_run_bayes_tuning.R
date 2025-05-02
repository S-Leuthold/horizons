#' Run Bayesian Tuning on Filtered Workflows
#'
#' Applies Bayesian hyperparameter optimization using `tune::tune_bayes()` to a
#' filtered `workflow_set`. The function reconstructs workflows from previous
#' tuning results and optionally runs in parallel using `furrr::future_map2()`.
#' Each model is tuned using the initial grid search results as priors.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tune tune_bayes control_bayes collect_metrics
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map2 furrr_options
#' @importFrom yardstick metric_set
#'
#' @param tuned_wf_set A filtered `workflow_set` returned by [filter_workflows()],
#'   with prior tuning results stored in the `result` column.
#' @param folds An object created by [rsample::vfold_cv()] or similar,
#'   specifying the resampling strategy for Bayesian tuning.
#' @param parallel Logical. If TRUE (default), tuning is parallelized
#'   across models using `future::multisession`. Otherwise runs sequentially.
#' @param iterations Integer. Number of Bayesian optimization iterations
#'   per model (default = 15).
#'
#' @return A `workflow_set` object with updated `result` and `metrics` columns,
#'   containing the results of Bayesian tuning.
#'
#' @details
#' This function assumes all preprocessing functions used in workflows
#' (e.g., `step_transform_spectra`) have been sourced into the current session.
#' Each model is tuned independently using [tune::tune_bayes()], with the
#' initial search informed by results from [tune::tune_grid()].
#'
#' @seealso
#' [tune::tune_bayes()], [workflowsets::workflow_set()], [filter_workflows()],
#' [yardstick::metric_set()]
#'
#' @examples
#' \dontrun{
#' folds <- rsample::vfold_cv(training_data, v = 5)
#' bayes_tuned <- run_bayesian_tuning(tuned_wf_set = filtered_workflows,
#'                                    folds        = folds,
#'                                    parallel     = TRUE,
#'                                    iterations   = 15)
#' }
#'
#' @keywords internal

run_bayesian_tuning <- function(tuned_wf_set,
                                folds,
                                parallel   = TRUE,
                                iterations = 15) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Configure Bayesian tuning control parameters
  ## ---------------------------------------------------------------------------

  if (parallel == TRUE) {

    future::plan(future::multisession, workers = parallel::detectCores(logical = TRUE) - 1)

    tune::control_bayes(
      save_pred     = TRUE,
      allow_par     = TRUE,
      save_workflow = TRUE,
      verbose       = FALSE,
      seed          = 0307,
      no_improve    = 10L,
      parallel_over = "everything"
    ) -> bayesian_controls

  } else {

    tune::control_bayes(
      save_pred     = TRUE,
      allow_par     = FALSE,
      save_workflow = TRUE,
      verbose       = FALSE,
      seed          = 0307,
      no_improve    = 10L
    ) -> bayesian_controls
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Rebuild the workflow set with correct recipe and spec
  ## ---------------------------------------------------------------------------

  tuned_wf_set %>%
    dplyr::mutate(
      workflow   = purrr::map(info, ~ .x$workflow[[1]]),
      preproc    = purrr::map(workflow, hardhat::extract_preprocessor),
      model_spec = purrr::map(workflow, hardhat::extract_spec_parsnip)
    ) -> rebuilt_wf_set

  workflowsets::workflow_set(
    preproc = rebuilt_wf_set$preproc,
    models  = rebuilt_wf_set$model_spec,
    cross   = FALSE
  ) %>%
    dplyr::mutate(wflow_id = rebuilt_wf_set$wflow_id) -> rebuilt_wf_set

  ## ---------------------------------------------------------------------------
  ## Step 2: Rebuild workflow tibble with proper access to prior workflow and tuning results
  ## ---------------------------------------------------------------------------

  tuned_wf_set %>%
    dplyr::mutate(
      workflow = purrr::map(info, ~ .x$workflow[[1]])
    ) -> rebuilt_wf_tbl

  ## ---------------------------------------------------------------------------
  ## Step 3: Run Bayesian tuning
  ## ---------------------------------------------------------------------------

  furrr::future_map2(
    .x = rebuilt_wf_tbl$workflow,
    .y = tuned_wf_set$result,
    .f = ~ tune::tune_bayes(
      object    = .x,
      resamples = folds,
      initial   = .y,
      iter      = iterations,
      metrics   = yardstick::metric_set(rrmse, rsq),
      control   = bayesian_controls
    ),
    .options = furrr::furrr_options(seed = TRUE)
  ) -> bayesian_tuned_models

  rebuilt_wf_tbl$result  <- bayesian_tuned_models
  rebuilt_wf_tbl$metrics <- purrr::map(bayesian_tuned_models, tune::collect_metrics)

  return(rebuilt_wf_tbl)

  if (parallel == TRUE) future::plan(sequential)

  ## ---------------------------------------------------------------------------
  ## Step 4: Update results and metrics
  ## ---------------------------------------------------------------------------

  rebuilt_wf_set$result  <- bayesian_tuned_models
  rebuilt_wf_set$metrics <- purrr::map(bayesian_tuned_models, tune::collect_metrics)

  ## ---------------------------------------------------------------------------
  ## Step 5: Return final tuned workflow_set object
  ## ---------------------------------------------------------------------------

  return(rebuilt_wf_set)
}


