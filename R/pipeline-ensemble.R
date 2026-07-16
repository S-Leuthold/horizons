# Pipeline verb: ensemble()
#
# Combines the models trained by fit() into a single stacked predictor. The
# verb owns dispatch, class promotion, and all console output (the render
# layer); the per-method machinery lives in the engine files
# (ensemble-weighted / ensemble-penalized / ensemble-xgb), which run silently
# and return the unified contract built by build_ensemble_contract().

## ---------------------------------------------------------------------------
## ensemble()
## ---------------------------------------------------------------------------

#' Combine Fitted Models into a Stacked Ensemble
#'
#' @description
#' Trains a meta-learner over the models produced by [fit()] and returns a
#' `horizons_ensemble` object carrying the fitted ensemble alongside the
#' original fit. Three meta-learners are available:
#'
#' - `"penalized"` (default): a glmnet penalized linear regression that learns
#'   regularized member weights.
#' - `"weighted"`: a fixed weighted average, weighting members by inverse
#'   out-of-fold RMSE (or equally when `optimize = FALSE`).
#' - `"xgb"`: an xgboost meta-learner that can capture non-linear interactions
#'   between members.
#'
#' All three train on the members' out-of-fold predictions and are evaluated on
#' the held-out Split-F test set, so the reported ensemble metrics are directly
#' comparable to the single-model metrics from `fit()`.
#'
#' @param x A `horizons_fit` object (output of [fit()]).
#' @param method Character. Meta-learner to use: `"penalized"` (default),
#'   `"weighted"`, or `"xgb"`.
#' @param optimize Logical. Tune the meta-learner's hyperparameters by CV on
#'   the out-of-fold matrix (`TRUE`), or use fixed defaults (`FALSE`).
#'   Default `TRUE`.
#' @param compute_uq Logical. Calibrate CV+ conformal prediction intervals for
#'   the ensemble (see [fit_ensemble_uq()]), stored in `$ensemble$uq` and
#'   consumed by `predict(..., interval = TRUE)`. Mirrors the `compute_uq`
#'   argument of [fit()]. Default `TRUE`.
#' @param seed Integer. Random seed for the meta-learner's CV folds (used by
#'   the `penalized` and `xgb` engines for both tuning and the genuine
#'   out-of-fold meta predictions; recorded on the contract for all methods).
#'   Ensemble UQ draws its calibration partition at `seed + 1000`, disjoint
#'   from the tuning folds. Mirrors the `seed` argument of [fit()] and
#'   [evaluate()]. Default `307L`.
#' @param verbose Logical. Print the progress tree to the console. Default
#'   `TRUE`.
#'
#' @return A `horizons_ensemble` object (inherits from `horizons_fit`) with the
#'   fitted ensemble in the `ensemble` slot.
#'
#' @examples
#' \dontrun{
#' fitted <- fit(evaluated, n_best = 5)
#'
#' # Default penalized meta-learner
#' ens <- ensemble(fitted)
#'
#' # Weighted average, no tuning
#' ens <- ensemble(fitted, method = "weighted", optimize = FALSE)
#' }
#'
#' @export
ensemble <- function(x,
                     method     = "penalized",
                     optimize   = TRUE,
                     compute_uq = TRUE,
                     seed       = 307L,
                     verbose    = TRUE) {

  ## -------------------------------------------------------------------------
  ## Step 0: Preflight
  ## -------------------------------------------------------------------------

  if (!inherits(x, "horizons_fit")) {

    cli::cli_abort(c(
      "{.arg x} must be a {.cls horizons_fit} object.",
      "i" = "Run {.fn fit} first to produce a fitted object."
    ))

  }

  valid_methods <- c("penalized", "weighted", "xgb")

  if (!is.character(method) || length(method) != 1 || !method %in% valid_methods) {

    cli::cli_abort(c(
      "{.arg method} must be one of {.val {valid_methods}}.",
      "x" = "Got {.val {method}}."
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 1: Gather members and build the canonical out-of-fold matrix
  ## -------------------------------------------------------------------------

  member_info <- gather_members(x)
  members     <- member_info$members
  rank_metric <- member_info$rank_metric

  oof <- build_oof_matrix(x, members)

  if (verbose) {

    cat("\n")
    cat(paste0("\u250c ensemble ",
               paste(rep("\u2500", 53), collapse = ""), "\n"))
    cat("\u2502\n")
    cat(paste0("\u2502  Method: ", method, "\n"))
    cat(paste0("\u2502  Members: ", length(members), "\n"))
    cat(paste0(
      "\u2502  Tuning: ",
      if (optimize) "CV on out-of-fold matrix" else "fixed defaults", "\n"
    ))
    cat("\u2502\n")

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Dispatch to the meta-learner engine
  ## -------------------------------------------------------------------------

  ## `seed` is threaded explicitly into each engine (rather than a set.seed()
  ## side-effect here) so the meta-learner's CV folds - which seed the Phase-2
  ## conformal calibration - are reproducible regardless of any RNG use between
  ## here and the vfold_cv() call. The tuned engines set the seed immediately
  ## before drawing folds; the weighted engine is deterministic and ignores it.

  contract <- switch(
    method,
    penalized = fit_ensemble_penalized(x, members, oof, rank_metric, optimize, seed),
    weighted  = fit_ensemble_weighted(x, members, oof, rank_metric, optimize, seed),
    xgb       = fit_ensemble_xgb(x, members, oof, rank_metric, optimize, seed)
  )

  ## -------------------------------------------------------------------------
  ## Step 3: Promote class and attach the ensemble
  ## -------------------------------------------------------------------------

  x$ensemble <- contract

  class(x) <- c("horizons_ensemble", "horizons_fit", "horizons_eval",
                "horizons_data", "list")

  ## -------------------------------------------------------------------------
  ## Step 4: Calibrate ensemble UQ (CV+ conformal) on the promoted object
  ## -------------------------------------------------------------------------

  ## Gate/failure inside fit_ensemble_uq degrades to a NULL bundle plus a
  ## one-line note — UQ never fails the ensemble.
  if (compute_uq) {

    x <- fit_ensemble_uq(x, verbose = verbose)

  }

  ## -------------------------------------------------------------------------
  ## Step 5: Render summary
  ## -------------------------------------------------------------------------

  if (verbose) {

    render_ensemble_summary(x$ensemble, rank_metric)

  }

  x

}

## ---------------------------------------------------------------------------
## render_ensemble_summary()
## ---------------------------------------------------------------------------

#' Render the Ensemble Summary Tree
#'
#' @description
#' Render layer for [ensemble()]: prints member weights, the ensemble's
#' performance on the rank metric, and the improvement over the best single
#' member. A non-positive improvement is surfaced in yellow rather than hidden,
#' because an ensemble that does not beat its best member is a real outcome.
#'
#' @param contract The ensemble contract list.
#' @param rank_metric Character. The metric improvement is measured on.
#' @return Invisibly NULL; called for its console output.
#' @keywords internal
render_ensemble_summary <- function(contract, rank_metric) {

  ## Top contributing members (by absolute weight) --------------------------

  w       <- contract$weights
  w       <- w[order(-abs(w$coef)), ]
  top     <- utils::head(w, 5)

  cat(paste0("\u2502  Top members (by weight)\n"))

  for (i in seq_len(nrow(top))) {

    is_last <- i == nrow(top)
    branch  <- if (is_last) "\u2514\u2500" else "\u251c\u2500"

    cat(paste0(
      "\u2502  ", branch, " ", top$member[i],
      ": ", round(top$coef[i], 4), "\n"
    ))

  }

  cat("\u2502\n")

  ## Performance + improvement ----------------------------------------------

  ens_val <- contract$metrics$.estimate[contract$metrics$.metric == rank_metric]

  cat(paste0("\u2502  Summary\n"))
  cat(paste0(
    "\u2502  \u251c\u2500 Ensemble ", rank_metric, ": ", round(ens_val, 3), "\n"
  ))

  imp <- contract$improvement

  imp_line <- paste0("Improvement over best member: ",
                     ifelse(imp >= 0, "+", ""), round(imp, 4))

  cat(paste0(
    "\u2502  \u251c\u2500 ",
    if (imp > 0) imp_line else cli::col_yellow(imp_line),
    "\n"
  ))

  ## UQ status ---------------------------------------------------------------

  uq <- contract$uq

  uq_line <- if (!is.null(uq)) {

    paste0("UQ: CV+ conformal (level ", uq$level_default,
           ", n_calib = ", uq$n_calib, ")")

  } else {

    "UQ: not computed"

  }

  cat(paste0("\u2502  \u251c\u2500 ", uq_line, "\n"))

  ## Runtime -----------------------------------------------------------------

  rt <- contract$runtime_secs

  time_str <- if (rt < 60) {
    paste0(round(rt, 1), "s")
  } else {
    paste0(round(rt / 60, 1), " min")
  }

  cat(paste0("\u2502  \u2514\u2500 Runtime: ", time_str, "\n"))
  cat("\u2502\n")
  cat(paste0(
    "\u2514\u2500 Class: horizons_fit \u2192 horizons_ensemble\n"
  ))

  invisible(NULL)

}
