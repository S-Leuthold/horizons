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
#' Combined predictions are clamped to `configure()`'s `outcome_range`, as
#' every scored and served prediction is, and `ensemble()` aborts with class
#' `horizons_error_input` before fitting anything when an observed outcome
#' lies outside it (#76). Its other refusals (an `x` that is not a fit, an
#' unknown `method`, an `optimize`, `seed` or `compute_uq` of the wrong type,
#' a fit with fewer than two members) carry the same class.
#'
#' After the outcome-range check and before it fits anything, `ensemble()`
#' checks that `x` still holds what a fitted object promises (its data,
#' evaluation and fitted models), and aborts with class
#' `horizons_error_validation` when it does not.
#'
#' The meta-learner is one unit: its tuning, its fit, its out-of-fold pass,
#' the members' predictions of the test rows and the scoring are its steps.
#' When any fails there is no ensemble to return, and `ensemble()` aborts with
#' `horizons_error_ensemble_failed`, the step and the cause in its `results`
#' field. A tuned meta-learner chooses its hyperparameters only among the
#' candidates scored on every fold, as [evaluate()] and [fit()] do (#101).
#' The warnings another package raised while it was built (the engine's,
#' tune's for the chosen candidate, a member's while predicting the test
#' rows) are kept in `ensemble$warnings`, as a member's are in
#' `models$results$warnings`, with the package's own lines about the
#' ensemble: the meta-learner's candidates left out for a missing fold, the
#' CV+ intervals not computed, and their measured test coverage not
#' computed. horizons' own warnings are written to the
#' log and raised once at the end. Among them, as in [evaluate()] and
#' [fit()]: the meta-learner's cross-validation folds or the CV+ calibration
#' folds drawn unstratified (`horizons_warning_substitution`); a draw rsample
#' stratified on fewer bins than asked is a note on the log.
#'
#' @param x A `horizons_fit` object (output of [fit()]).
#' @param method Character. Meta-learner to use: `"penalized"` (default),
#'   `"weighted"`, or `"xgb"`.
#' @param optimize Logical. Tune the meta-learner's hyperparameters by CV on
#'   the out-of-fold matrix (`TRUE`), or use fixed defaults (`FALSE`).
#'   Default `TRUE`.
#' @param seed Integer. Random seed for the meta-learner's CV folds (used by
#'   the `penalized` and `xgb` engines for both tuning and the genuine
#'   out-of-fold meta predictions; recorded on the contract for all methods).
#'   Ensemble UQ draws its calibration partition at `seed + 1000`, disjoint
#'   from the tuning folds. Mirrors the `seed` argument of [fit()] and
#'   [evaluate()]. Default `DEFAULT_ENSEMBLE_SEED` (307).
#' @param compute_uq Logical. Calibrate CV+ conformal prediction intervals for
#'   the ensemble (see [fit_ensemble_uq()]), stored in `$ensemble$uq` and
#'   consumed by `predict()`. When they cannot be calibrated (fewer than
#'   `min_calib_rows` rows, too few finite residuals for CV+ at its level, or
#'   a fold refit that fails), the ensemble is returned point-only with one
#'   warning of class `horizons_warning_skipped` saying why, whatever
#'   `verbose` is (#145). Their coverage is then measured on the test rows
#'   `fit()` held out and stored on the bundle as `test_coverage`,
#'   `test_mean_width` and `n_test`, as a member's is; when it cannot be
#'   measured they are `NA` (`n_test` 0), with one `skipped` warning saying
#'   why. Mirrors the `compute_uq`
#'   argument of [fit()]. Placed after `seed` so pre-existing positional
#'   callers (`method`, `optimize`, `seed`) are unaffected. Default `TRUE`.
#' @param control A [control_horizons()] object: how the run goes, not what
#'   it computes. `ensemble()` reads `allow_par`, `verbose`, `debug`,
#'   `keep_traces` and `min_calib_rows`, and refuses any other field that is
#'   set. With `debug = TRUE` the meta-learner runs in this session and its
#'   first failure, inside tuning included, stops the call with its
#'   backtrace; with `keep_traces = TRUE` a failure's backtrace is kept as
#'   text in the abort's `results`. With `allow_par = TRUE`, the meta-learner's CV tuning and its
#'   out-of-fold pass parallelise over folds on whatever `future::plan()` the
#'   caller has registered; `ensemble()` never registers a plan, and runs
#'   sequentially, warning with class `horizons_warning_substitution`, if the
#'   plan offers fewer than two workers. The control it ran with, defaults
#'   filled in, is recorded as `ensemble$control`.
#'
#' @return A `horizons_ensemble` object (inherits from `horizons_fit`) with the
#'   fitted ensemble in the `ensemble` slot. Before anything is built,
#'   `ensemble()` refuses members whose model's engine is not installed,
#'   since it predicts them, naming each package
#'   (`horizons_error_missing_package`).
#'
#' @examples
#' \dontrun{
#' fitted <- fit(evaluated, n_best = 5)
#'
#' # Default penalized meta-learner (CV+ intervals calibrated by default)
#' ens <- ensemble(fitted)
#'
#' # Weighted average, no tuning
#' ens <- ensemble(fitted, method = "weighted", optimize = FALSE)
#'
#' # Skip interval calibration (point predictions only)
#' ens <- ensemble(fitted, compute_uq = FALSE)
#' }
#'
#' @export
ensemble <- function(x,
                     method     = "penalized",
                     optimize   = TRUE,
                     seed       = DEFAULT_ENSEMBLE_SEED,
                     compute_uq = TRUE,
                     control    = control_horizons()) {

  ## -------------------------------------------------------------------------
  ## Step 0: Preflight
  ## -------------------------------------------------------------------------

  ## How the run goes: the fields ensemble() reads, defaults filled in, and
  ## the record of it on the ensemble
  control   <- resolve_control(control, "ensemble")
  allow_par <- control$allow_par
  verbose   <- control$verbose

  collector <- log_open("ensemble")

  if (!inherits(x, "horizons_fit")) {

    abort_horizons(c(
      "{.arg x} must be a {.cls horizons_fit} object.",
      "i" = "Run {.fn fit} first to produce a fitted object."
    ), kind = "input")

  }

  ## The meta-learner trains and is scored on predictions clamped to the
  ## outcome's range, so the outcome has to lie inside it (#76), as
  ## evaluate() and fit() require before they fit anything.
  check_outcome_range(x, verb = "ensemble")

  valid_methods <- c("penalized", "weighted", "xgb")

  if (!rlang::is_string(method) || !method %in% valid_methods) {

    abort_horizons(c(
      "{.arg method} must be one of {.val {valid_methods}}.",
      "x" = "Got {.val {method}}."
    ), kind = "input")

  }

  ## Both are recorded on the ensemble, which the validator checks only after
  ## the meta-learner is built, and fit_ensemble_uq() rebuilds the fold
  ## models from them; checked here so a bad value is refused before any
  ## engine runs. The seed, as at every verb, is a whole number set.seed()
  ## takes unchanged.
  check_arg(rlang::check_bool, optimize)
  check_arg(rlang::check_number_whole, seed, min = -SEED_MAX, max = SEED_MAX)
  check_arg(rlang::check_bool, compute_uq)

  ## The fit contract, and through it the evaluation and base contracts at
  ## the full stage, before anything is gathered or trained. The argument and
  ## outcome-range checks come first, so an outcome outside the range is
  ## reported as such rather than as the response bound it breaks; an unusable
  ## range is the validator's finding (check_outcome_range() leaves it).
  x <- validate_horizons_fit(x)

  ## The members: fitted, with a stored workflow and out-of-fold predictions.
  ## A combination needs two of them.
  member_info <- gather_members(x)
  members     <- member_info$members
  rank_metric <- member_info$rank_metric

  if (length(members) < 2) {

    abort_horizons(c(
      "{.fn ensemble} needs at least 2 fitted members; {.arg x} has {length(members)}.",
      "i" = "Fit with {.arg n_best} of at least 2, as {.code fit(x, n_best = 5)} does by default, on an evaluation with two or more configurations that succeed."
    ), kind = "input")

  }

  ## The members predict the test rows and the calibration rows, so their
  ## packages are checked, and loaded, before anything is built. The
  ## meta-learners' own engines (glmnet, xgboost) are in Imports.
  ensure_predict_namespaces(x, members, verb = "ensemble")

  ## -------------------------------------------------------------------------
  ## Step 0b: Parallel backend
  ## -------------------------------------------------------------------------
  ## The user owns the backend; confirm there is one and pin threads here
  ## before tune spawns anything. Never touch the plan.
  ##
  ## Debug runs in this session: a backtrace from a worker shows the worker's
  ## plumbing, and no debugger attaches there. The user asked for debug, so
  ## the tree says so rather than a warning.

  debug       <- control$debug
  par_by_user <- allow_par
  allow_par   <- allow_par && !debug

  if (allow_par) {

    allow_par <- check_parallel_backend("ensemble", collector)

    if (allow_par) {

      unpin_threads <- pin_parent_threads()
      on.exit(unpin_threads(), add = TRUE)

    }

  }

  ## -------------------------------------------------------------------------
  ## Step 1: The members, gathered above
  ## -------------------------------------------------------------------------

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
    if (par_by_user && debug) {
      cat("\u2502  Parallel: off, since debug runs in this session\n")
    }
    cat("\u2502\n")

  }

  ## -------------------------------------------------------------------------
  ## Step 2: The meta-learner, one unit
  ## -------------------------------------------------------------------------
  ## The meta-learner is the unit: a failure in any of its steps leaves
  ## nothing usable, so the call aborts with the record. Its warnings are
  ## recorded, not raised as they come, and a worker's death is systemic.
  ##
  ## `seed` is threaded explicitly into each engine (rather than a set.seed()
  ## side-effect here) so the meta-learner's CV folds - which seed the Phase-2
  ## conformal calibration - are reproducible regardless of any RNG use between
  ## here and the vfold_cv() call. The tuned engines set the seed immediately
  ## before drawing folds; the weighted engine is deterministic and ignores it.

  meta <- stop_on_worker_death(run_unit({

    oof <- unit_step("oof_matrix", build_oof_matrix(x, members))

    switch(
      method,
      penalized = fit_ensemble_penalized(x, members, oof, rank_metric, optimize, seed,
                                         allow_par = allow_par, debug = debug),
      weighted  = fit_ensemble_weighted(x, members, oof, rank_metric, optimize, seed),
      xgb       = fit_ensemble_xgb(x, members, oof, rank_metric, optimize, seed,
                                   allow_par = allow_par, debug = debug)
    )

  }, unit = "meta_learner", id = method, debug = debug, keep_traces = control$keep_traces),
  results = NULL, verb = "ensemble")

  if (meta$failed) abort_ensemble_failed(meta$records, method)

  contract <- meta$value

  ## What the run recorded: the meta-learner's warnings and notes, the
  ## members' while they predicted the test rows, the rows a member could not
  ## use, and the warnings tune kept as notes
  records     <- dplyr::bind_rows(meta$records, attr(contract, "unit_records"))
  flagged     <- attr(contract, "flagged")
  warning_log <- attr(contract, "warning_log")

  ## And the tree's lines for a stratified draw that did not stratify as
  ## asked, as evaluate() and fit() print them: the meta-learner's folds' here,
  ## the CV+ folds' once they are drawn
  strata_lines <- list(meta = attr(contract, "strata_line"), cv_plus = NULL)

  for (a in c("unit_records", "flagged", "warning_log", "strata_line")) attr(contract, a) <- NULL

  ## -------------------------------------------------------------------------
  ## Step 3: Promote class and attach the ensemble
  ## -------------------------------------------------------------------------

  x$ensemble         <- contract
  x$ensemble$control <- control

  class(x) <- c("horizons_ensemble", "horizons_fit", "horizons_eval",
                "horizons_data", "list")

  ## -------------------------------------------------------------------------
  ## Step 4: Calibrate ensemble UQ (CV+ conformal) on the promoted object
  ## -------------------------------------------------------------------------

  ## A unit of its own, after the meta-learner's (#145): a failure leaves the
  ## ensemble point-only and says why, whatever `verbose` is; a tripwire
  ## inside still stops the call.
  if (compute_uq) {

    cv_plus <- run_unit(
      fit_ensemble_uq(x, min_calib_rows = control$min_calib_rows),
      unit = "intervals", id = "ensemble", debug = debug
    )

    records <- dplyr::bind_rows(records, cv_plus$records[cv_plus$records$type != "failure", , drop = FALSE])

    if (cv_plus$failed) {

      cause <- unit_failure_message(cv_plus$records, outside = NULL)
      line  <- paste0("Prediction intervals (CV+) were not computed: ", cause)

      ## On the log, and pinned on the ensemble's row, as fit() keeps a
      ## member's lost intervals on the member's row
      log_warning(collector, "skipped", "{line}")
      warning_log <- dplyr::bind_rows(warning_log, text_records(line, pinned = TRUE))

    } else {

      x <- cv_plus$value

      strata_lines["cv_plus"] <- list(attr(x, "strata_line"))
      attr(x, "strata_line") <- NULL

    }

  }

  ## Measured coverage on the held-out test rows, a unit of its own after the
  ## intervals', run when they were calibrated, as fit()'s follows a member's
  ## UQ (#118): the intervals predict() serves, scored on rows that took no
  ## part in the fit. A failure leaves the figures NA and says why.
  if (!is.null(x$ensemble$uq)) {

    coverage <- run_unit(
      ensemble_test_coverage(x),
      unit = "coverage", id = "ensemble", debug = debug
    )

    records <- dplyr::bind_rows(records, coverage$records[coverage$records$type != "failure", , drop = FALSE])

    if (coverage$failed) {

      cause <- unit_failure_message(coverage$records, outside = NULL)
      line  <- paste0("Measured test coverage was not computed: ", cause)

      log_warning(collector, "skipped", "{line}")
      warning_log <- dplyr::bind_rows(warning_log, text_records(line, pinned = TRUE))

    }

    x$ensemble$uq <- c(x$ensemble$uq, coverage$value %||% test_interval_coverage_missing())

  }

  ## The ensemble's own warnings, as a member's are kept: another package's
  ## (an engine's, tune's notes for the chosen candidate) and the package's
  ## own lines, pinned (the candidates left out, the CV+ intervals not
  ## computed, their measured coverage not computed). horizons' topic
  ## warnings go to the log as well, once.
  warning_log <- dplyr::bind_rows(unit_warning_records(records), warning_log)
  x$ensemble["warnings"] <- list(render_warning_log(warning_log))

  log_unit_records(collector, records)
  log_flagged(collector, flagged)

  ## What the run reported goes on the object's log before it is certified
  x$log <- log_close(collector, x$log)

  ## Certify the contract before returning: structural checks only (see
  ## validate_horizons_ensemble), so every ensemble() return is guaranteed to
  ## match what predict.horizons_ensemble() and downstream consumers read.
  x <- validate_horizons_ensemble(x)

  ## -------------------------------------------------------------------------
  ## Step 5: Render summary
  ## -------------------------------------------------------------------------

  if (verbose) {

    render_ensemble_summary(x$ensemble, rank_metric, strata_lines)

  }

  ## Each warning the run recorded, raised once, after the summary
  signal_log(collector, x)

  x

}

## ---------------------------------------------------------------------------
## render_ensemble_summary()
## ---------------------------------------------------------------------------

#' Render the Ensemble Summary Tree
#'
#' @description
#' Render layer for [ensemble()]: prints member weights, the ensemble's
#' performance on the rank metric, the improvement over the best single
#' member, and the CV+ intervals' measured coverage on the held-out test rows
#' beside their nominal level. A non-positive improvement is surfaced in yellow rather than hidden,
#' because an ensemble that does not beat its best member is a real outcome.
#'
#' @param contract The ensemble contract list.
#' @param rank_metric Character. The metric improvement is measured on.
#' @param strata_lines List with `meta` and `cv_plus`: the tree's line for
#'   the meta-learner's folds and for the CV+ folds when either did not
#'   stratify as asked ([strata_tree_line()]), or `NULL`.
#' @return Invisibly NULL; called for its console output.
#' @keywords internal
render_ensemble_summary <- function(contract, rank_metric, strata_lines = list()) {

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

  ## The meta-learner's folds, when they did not stratify as asked, in the
  ## line evaluate() and fit() print for their own folds
  if (!is.null(strata_lines$meta)) {

    cat(paste0("\u2502  ", cli::col_yellow(strata_lines$meta), "\n"))

  }

  cat("\u2502\n")

  ## Performance + improvement ----------------------------------------------

  ens_val <- contract$metrics$.estimate[contract$metrics$.metric == rank_metric]

  cat(paste0("\u2502  Summary\n"))
  cat(paste0(
    "\u2502  \u251c\u2500 Ensemble ", rank_metric, ": ", round(ens_val, 3), "\n"
  ))

  imp <- contract$improvement

  ## NA when the ensemble or every member has no value of the rank metric
  imp_line <- if (is.na(imp)) {
    paste0("Improvement over best member: none to measure, without a ", rank_metric, " to compare")
  } else {
    paste0("Improvement over best member: ", ifelse(imp >= 0, "+", ""), round(imp, 4))
  }

  cat(paste0(
    "\u2502  \u251c\u2500 ",
    if (isTRUE(imp > 0)) imp_line else cli::col_yellow(imp_line),
    "\n"
  ))

  ## UQ status ---------------------------------------------------------------

  ## The intervals' measured coverage on the held-out test rows, as fit()'s
  ## tree gives a member's (format_test_coverage())

  uq <- contract$uq

  if (!is.null(uq)) {

    cat(paste0("\u2502  \u251c\u2500 UQ: CV+ conformal (n_calib = ", uq$n_calib, ")\n"))

    if (!is.null(strata_lines$cv_plus)) {

      cat(paste0("\u2502  \u2502  \u251c\u2500 ", cli::col_yellow(strata_lines$cv_plus), "\n"))

    }

    cat(paste0("\u2502  \u2502  \u2514\u2500 ", format_test_coverage(uq), "\n"))

  } else {

    cat(paste0("\u2502  \u251c\u2500 UQ: not computed\n"))

  }

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


## ---------------------------------------------------------------------------
## abort_ensemble_failed()
## ---------------------------------------------------------------------------

#' Abort because the meta-learner could not be built
#'
#' @description
#' The meta-learner is `ensemble()`'s unit, and without it there is no
#' ensemble to return. The failure travels on the condition as `results`, as
#' `evaluate()`'s, `fit()`'s and `predict()`'s do: one row, shaped as
#' `horizons_error_prediction_failed`'s, with the unit (`"meta_learner"`),
#' `config_id` (`NA`, the unit not being a configuration), the step it
#' failed in, the root cause's class, the message, and under
#' `control_horizons(keep_traces = TRUE)` the backtrace as text.
#'
#' @param records The meta-learner unit's records, holding its failure.
#' @param method Character(1). The ensemble method.
#' @param call The frame the error reports: `ensemble()`'s.
#' @return Does not return; aborts with class
#'   `horizons_error_ensemble_failed` (kind `run`).
#' @keywords internal
#' @noRd
abort_ensemble_failed <- function(records, method, call = rlang::caller_env()) {

  failure <- records[records$type == "failure", , drop = FALSE]

  results <- tibble::tibble(
    unit          = failure$unit,
    config_id     = NA_character_,
    step          = failure$step,
    class         = failure$class,
    error_message = unit_failure_message(failure, outside = "Meta-learner"),
    trace         = failure$trace
  )

  abort_horizons(c(
    "The {method} ensemble could not be built: its meta-learner failed.",
    "x" = cli_escape(results$error_message[1]),
    "i" = "The failure is on this condition as {.field results}: {.code rlang::last_error()$results}."
  ), kind = "run", class = "horizons_error_ensemble_failed", results = results, call = call)

}
