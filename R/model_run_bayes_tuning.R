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
#' @importFrom progressr handlers with_progress progressor
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
  ## Step 1: Configure Bayesian tuning control
  ## ---------------------------------------------------------------------------

  if (parallel) {
    future::plan(future::multisession, workers = 9)
    control <- tune::control_bayes(
      save_pred     = FALSE,
      allow_par     = TRUE,
      save_workflow = FALSE,
      verbose       = FALSE,
      seed          = 0307,
      no_improve    = 10L,
      parallel_over = "resamples"
    )
  } else {
    control <- tune::control_bayes(
      save_pred     = FALSE,
      allow_par     = FALSE,
      save_workflow = FALSE,
      verbose       = FALSE,
      seed          = 0307,
      no_improve    = 10L
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Rebuild workflows
  ## ---------------------------------------------------------------------------

  tuned_wf_set <- tuned_wf_set %>%
    dplyr::mutate(workflow = purrr::map(info, ~ .x$workflow[[1]]))

  ## ---------------------------------------------------------------------------
  ## Step 3: Bayesian tuning with progress
  ## ---------------------------------------------------------------------------

  progressr::handlers("cli")
  model_results <- progressr::with_progress({
    p <- progressr::progressor(steps = nrow(tuned_wf_set))

    furrr::future_map2(
      .x = tuned_wf_set$workflow,
      .y = tuned_wf_set$result,
      .f = function(wf, init) {
        p(message = paste("Tuning:", wf$fit$spec$mode))
        result <- safe_tune_bayes(
          wf         = wf,
          initial_res = init,
          folds      = folds,
          iterations = iterations,
          control    = control
        )
        if (!is.null(result$error)) {
          cli::cli_alert_warning("Bayes tuning failed: {result$error$message}")
        }
        result
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  })

  ## ---------------------------------------------------------------------------
  ## Step 4: Attach tuning results and metrics
  ## ---------------------------------------------------------------------------

  tuned_wf_set$result  <- purrr::map(model_results, "result")
  tuned_wf_set$metrics <- purrr::map(tuned_wf_set$result, tune::collect_metrics)

  if (parallel) future::plan(future::sequential)

  return(tuned_wf_set)
}
