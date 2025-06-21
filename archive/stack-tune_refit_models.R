#' Tune Workflow via Bayesian Optimization and Refit with Best Parameters
#'
#' Performs Bayesian hyperparameter tuning using `tune_bayes()` on a single workflow,
#' selects the best configuration by RRMSE, finalizes the workflow, and fits it to the
#' cross-validation folds via `fit_resamples()`. Intended for internal use in Horizons
#' ensemble modeling pipelines.
#'
#' @param wflow A `workflow` object to tune and refit.
#' @param resamples A resampling object (e.g., from `rsample::vfold_cv()`) used for tuning and evaluation.
#' @param iter Integer. Number of Bayesian optimization iterations. Default is 20.
#' @param metrics A metric set created with `yardstick::metric_set()` (must include `"rrmse"`).
#' @param control_bayes A control object from `tune::control_bayes()`, specifying tuning options.
#' @param control_refit A control object from `tune::control_resamples()`, used for final resample fitting.
#' @param verbose Logical. If TRUE, prints progress messages to console. Default is TRUE.
#'
#' @return A fitted resample object (`tune_results`) from `fit_resamples()`, ready for use in stacking.
#'
#' @seealso
#'   \code{\link[tune]{tune_bayes}},
#'   \code{\link[tune]{select_best}},
#'   \code{\link[tune]{finalize_workflow}},
#'   \code{\link[tune]{fit_resamples}}
#'
#' @examples
#' \dontrun{
#' tune_and_refit(
#'   wflow           = my_workflow,
#'   resamples       = my_cv,
#'   iter            = 25,
#'   metrics         = yardstick::metric_set(rmse, rsq, rrmse),
#'   control_bayes   = tune::control_bayes(verbose = TRUE),
#'   control_refit   = tune::control_resamples(save_pred = TRUE)
#' )
#' }
#'
#' @keywords internal


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
