#' Tune and Refit a Workflow Using Bayesian Optimization
#'
#' This function performs hyperparameter tuning using `tune_bayes()` on a given
#' `workflow` object using the provided cross-validation resamples, then finalizes
#' the workflow with the best hyperparameters (by RRMSE) and refits it using
#' `fit_resamples()`.
#'
#' Intended for internal use within the Horizons model stacking pipeline.
#'
#' @param wflow A `workflow` object to be tuned and refit.
#' @param resamples A resampling object created by `vfold_cv()` or similar.
#' @param metrics A metric set created with `yardstick::metric_set()` to guide tuning.
#' @param control_bayes A control object for Bayesian tuning, e.g., from `control_bayes()`.
#' @param control_refit A control object for model refitting, e.g., from `control_resamples()`.
#'
#' @return A fitted resamples object (from `fit_resamples()`), ready for stacking.
#' @importFrom tune tune_bayes select_best finalize_workflow fit_resamples
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' res <- tune_and_refit(my_workflow, my_resamples, metric_set(rmse, rsq, rrmse),
#'                       control_bayes(), control_resamples())
#' }
#' internal


tune_and_refit <- function(wflow,
                           resamples,
                           iter   = 20,
                           metrics,
                           control_bayes,
                           control_refit,
                           verbose = TRUE){

  if (verbose) cli::cli_alert_info("Tuning workflow with {.val tune_bayes()}")

  tune::tune_bayes(object    = wflow,
                   resamples = resamples,
                   iter      = 20,
                   control   = control_bayes,
                   metrics   = metrics) -> tuned_wflow

  best_params  <- tune::select_best(tuned_wflow, metric = "rrmse")
  finalized_wf <- tune::finalize_workflow(wflow, best_params)

  tune::fit_resamples(object    = finalized_wf,
                      resamples = resamples,
                      control   = refit_controls)
  }
