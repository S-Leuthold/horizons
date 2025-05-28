#' Safely Run Bayesian Tuning for a Single Workflow
#'
#' This helper function wraps `tune::tune_bayes()` in `purrr::safely()` to prevent
#' a single model failure from crashing the full tuning loop.
#'
#' @param wf A single `workflow` object.
#' @param initial_res A `tune_results` object from `tune_grid()` to seed the search.
#' @param folds A resampling object (e.g., from `rsample::vfold_cv()`).
#' @param iterations Number of iterations for Bayesian tuning.
#' @param control A `control_bayes()` object.
#'
#' @return A list with elements `result` (a tune_bayes result) or `error` if it failed.
#'
#' @keywords internal

safe_tune_bayes <- purrr::safely(function(wf, initial_res, folds, iterations, control) {
  tune::tune_bayes(
    object    = wf,
    resamples = folds,
    initial   = initial_res,
    iter      = iterations,
    metrics   = yardstick::metric_set(rrmse, yardstick::rsq),
    control   = control
  )
})
