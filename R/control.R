## ---------------------------------------------------------------------------
## control_horizons(): how the long-running verbs run
## ---------------------------------------------------------------------------
##
## Arguments say what a verb computes; a control object says how it runs.
## Every field is a row of CONTROL_FIELDS: its default, its check, and the
## verbs that use it. The constructor, the refusal of a field set but unused
## and the per-verb resolution read that table, so a new field is a row there
## and an argument of control_horizons(); its documentation (the argument's
## @param, and the list of fields in each verb's `control` @param) is kept by
## hand.

## ---------------------------------------------------------------------------
## The field table
## ---------------------------------------------------------------------------

### Checks take the value, the field's name and the frame to report, and abort
### through check_arg(), so a bad value is an input error naming the field.

control_check_bool <- function(x, arg, call) {

  check_arg(rlang::check_bool, x, arg = arg, call = call)

}

### A row minimum is a whole number of at least `floor`, the fewest rows the
### computation it gates can use.
control_check_rows <- function(floor) {

  force(floor)

  function(x, arg, call) {

    check_arg(rlang::check_number_whole, x, min = as.numeric(floor), arg = arg, call = call)

  }

}

control_check_parallelize_over <- function(x, arg, call) {

  if (identical(x, "both")) {

    abort_horizons(c(
      "{.arg {arg}} = {.val both} is not supported.",
      "i" = "Use {.val configs} or {.val resamples}."
    ), kind = "input", call = call)

  }

  check_arg(rlang::check_string, x, arg = arg, call = call)

  if (!x %in% PARALLELIZE_OVER_VALUES) {

    abort_horizons(
      "{.arg {arg}} must be one of {.val {PARALLELIZE_OVER_VALUES}}, not {.val {x}}.",
      kind = "input", call = call
    )

  }

}

control_check_output_dir <- function(x, arg, call) {

  check_arg(rlang::check_string, x, allow_empty = FALSE, arg = arg, call = call)

}

### A count that can be turned off: FALSE, or a whole number of at least
### `floor`. The breaker's is the failures with one cause that stop a run,
### at least two; the Bayesian search's stopping rule is tune's no_improve,
### which counts the iterations since the best, so one is its least.
control_check_off_or_count <- function(floor) {

  force(floor)

  function(x, arg, call) {

    if (isFALSE(x)) return(invisible(NULL))

    if (!rlang::is_scalar_integerish(x) || is.na(x) || x < floor) {

      got <- if (is.atomic(x) && length(x) == 1) cli::format_inline("{.val {x}}") else cli::format_inline("{.obj_type_friendly {x}}")

      abort_horizons(
        "{.arg {arg}} must be {.code FALSE} or a whole number of at least {floor}, not {got}.",
        kind = "input", call = call
      )

    }

  }

}

### A count of at least one: the neighbour search's targets per chunk, the
### threads a sequential run's engines fit on
control_check_count <- function(x, arg, call) {

  check_arg(rlang::check_number_whole, x, min = 1, arg = arg, call = call)

}

CONTROL_VERBS <- c("evaluate", "fit", "ensemble", "select_training")

### `default` is a function, so a default read from an option is read when the
### verb runs, not when the package loads. `option` names that option, so a
### bad value of it is reported as the option's.
CONTROL_FIELDS <- list(

  allow_par = list(
    default = function() FALSE,
    check   = control_check_bool,
    verbs   = c("evaluate", "fit", "ensemble")
  ),

  parallelize_over = list(
    default = function() "auto",
    check   = control_check_parallelize_over,
    verbs   = "evaluate"
  ),

  verbose = list(
    default = function() getOption("horizons.verbose", TRUE),
    option  = "horizons.verbose",
    check   = control_check_bool,
    verbs   = CONTROL_VERBS
  ),

  output_dir = list(
    default = function() NULL,
    check   = control_check_output_dir,
    verbs   = "evaluate"
  ),

  debug = list(
    default = function() FALSE,
    check   = control_check_bool,
    verbs   = CONTROL_VERBS
  ),

  breaker = list(
    default = function() BREAKER_FAILURES,
    check   = control_check_off_or_count(2),
    verbs   = c("evaluate", "fit")
  ),

  ### select_training() has no unit that leaves a failure row to keep a
  ### trace on, and the log keeps none
  keep_traces = list(
    default = function() FALSE,
    check   = control_check_bool,
    verbs   = c("evaluate", "fit", "ensemble")
  ),

  chunk_size = list(
    default = function() SELECT_CHUNK_SIZE,
    check   = control_check_count,
    verbs   = "select_training"
  ),

  ### The split-conformal order statistic exists from this many rows at the
  ### calibration level; ensemble()'s CV+ needs more, and checks it itself.
  min_calib_rows = list(
    default = function() N_CALIB_MIN,
    check   = function(x, arg, call) {
      control_check_rows(conformal_min_rows(DEFAULT_UQ_LEVEL))(x, arg, call)
    },
    verbs   = c("fit", "ensemble")
  ),

  min_ad_rows = list(
    default = function() N_AD_TRAIN_MIN,
    check   = control_check_rows(2),
    verbs   = "fit"
  ),

  ### Below five rows a bootstrap of the test RPD mostly redraws the same
  ### rows, and the interval no longer says anything
  min_degradation_rows = list(
    default = function() DEGRADATION_MIN_N,
    check   = control_check_rows(5),
    verbs   = "fit"
  ),

  ### Read from an option, as verbose is, so a script (or the test setup)
  ### can turn isolated work off for every verb at once
  isolate = list(
    default = function() getOption("horizons.isolate", TRUE),
    option  = "horizons.isolate",
    check   = control_check_bool,
    verbs   = c("evaluate", "fit", "select_training")
  ),

  ### How the configs axis arranges its work within the daemons set; the
  ### results do not depend on it, so evaluate() does not record it with a
  ### checkpoint's settings. Read from an option, as isolate is
  inner_daemons = list(
    default = function() getOption("horizons.inner_daemons", TRUE),
    option  = "horizons.inner_daemons",
    check   = control_check_bool,
    verbs   = "evaluate"
  ),

  ### The threads the engines with a thread argument fit on when the run is
  ### sequential; on the daemons each fits on one, since the daemons set are
  ### the cores the run is given. The results do not depend on it, so
  ### evaluate() does not record it with a checkpoint's settings
  threads = list(
    default = function() ENGINE_THREADS,
    check   = control_check_count,
    verbs   = c("evaluate", "fit")
  ),

  ### Changes what a run computes, as isolate does for cubist, so evaluate()
  ### records it with a checkpoint's settings
  no_improve = list(
    default = function() BAYES_NO_IMPROVE_DEFAULT,
    check   = control_check_off_or_count(1),
    verbs   = c("evaluate", "fit")
  ),

  ### The reference is each row's distance to its nearest other row
  min_resemblance_rows = list(
    default = function() SELECT_RESEMBLANCE_MIN_ROWS,
    check   = control_check_rows(2),
    verbs   = "select_training"
  )

)


## ---------------------------------------------------------------------------
## control_horizons()
## ---------------------------------------------------------------------------

#' Control how evaluate(), fit(), ensemble() and select_training() run
#'
#' @description
#' Arguments say what a verb computes; a control object says how it runs:
#' parallelism, console output, checkpoints, how failures are handled, the
#' minimums below which part of a run is skipped or refused, and when a
#' search stops. Most of these change only how a run goes. Two change what
#' it computes: `isolate`, since a Cubist fit otherwise depends on the fits
#' before it, and `no_improve`, how far the Bayesian searches go. [evaluate()]
#' records both with a checkpoint's settings.
#'
#' Every field defaults to `NULL`, which leaves it to the verb's default. A
#' verb refuses a field that is set but that it does not use, so one control
#' object can be passed to several verbs, each reading its own fields. The
#' control a verb ran with, defaults filled in, is recorded on the slot it
#' writes (`evaluation$control`, `models$control`, `ensemble$control`,
#' `selection$settings$control`).
#'
#' @param ... Not used: every field is set by name.
#' @param allow_par `logical(1)`. Run on the mirai daemons the user set, as
#'   with `mirai::daemons(4)` before the call; the verb never sets, resizes or
#'   resets them, and a `future::plan()` is not used. Default `FALSE`.
#'   [evaluate()], [fit()], [ensemble()].
#' @param parallelize_over `character(1)`. The axis of [evaluate()]'s work the
#'   daemons are applied to: `"auto"` (the default), `"configs"` or `"resamples"`.
#'   `"auto"` never chooses `"configs"` without `output_dir`, and
#'   `"configs"` asked for explicitly needs one.
#' @param verbose `logical(1)`. Print the verb's progress tree. Default
#'   `getOption("horizons.verbose", TRUE)`, so a script can quiet the four
#'   verbs, and [average()], at once. All four verbs.
#' @param output_dir `character(1)`. A directory where [evaluate()] writes
#'   each configuration's result as it finishes, and from which it resumes;
#'   it keeps its fold preparations there too until the run completes. On
#'   the configs axis the daemons read the configurations' data and their
#'   fold preparations from it, so they must reach it (see [evaluate()]).
#'   Default none.
#' @param debug `logical(1)`. Stop at the first failure with its full
#'   backtrace, instead of recording the failed unit and going on. Warnings
#'   are still recorded. Default `FALSE`. All four verbs; each honours it as
#'   its failures are contained unit by unit.
#' @param breaker A whole number of at least 2, or `FALSE`. When the first
#'   configurations to finish all fail with the same cause, this many of them
#'   stop the run at once, since every other configuration would fail the
#'   same way; `FALSE` turns the breaker off. Default 3. [evaluate()],
#'   [fit()].
#' @param keep_traces `logical(1)`. Keep a failed unit's backtrace on its row
#'   of the results. Default `FALSE`. [evaluate()], [fit()], [ensemble()]:
#'   [select_training()] has no unit that leaves a row to keep one on.
#' @param chunk_size A whole number. Targets per chunk in
#'   [select_training()]'s neighbour search. Default 500.
#' @param min_calib_rows A whole number of at least 9. The fewest calibration
#'   rows [fit()]'s intervals and applicability domain, and [ensemble()]'s
#'   CV+ intervals, are computed from; below it they are skipped. Default 30.
#'   Nine is the fewest from which a 90% split-conformal interval exists;
#'   [ensemble()]'s CV+ intervals need 19 whatever this is set to. The
#'   applicability domain's out-of-domain cutoff is the largest calibration
#'   distance below 99 rows, so about one in `n + 1` in-domain samples is
#'   flagged out of domain: 3% at the default, 10% at 9.
#' @param min_ad_rows A whole number of at least 2. The fewest training rows
#'   [fit()] estimates the applicability domain's covariance from. Default 10.
#' @param min_degradation_rows A whole number of at least 5. The fewest test
#'   rows [fit()]'s degradation check bootstraps from. Default 10. Below the
#'   default the bootstrap interval widens and a healthy fit is flagged more
#'   often.
#' @param isolate `logical(1)`. Run work that would otherwise depend on what
#'   ran before it in the session in a process started for it alone, which
#'   ends when the work returns: each fit of a model whose engine carries
#'   state from one fit to the next (`cubist`), in [evaluate()] and [fit()],
#'   wherever the fit runs, and [select_training()]'s draw, so the session
#'   never holds the library. `FALSE` runs both in the process that calls
#'   them, and the run says so. Default `getOption("horizons.isolate", TRUE)`.
#'   [evaluate()], [fit()], [select_training()].
#' @param inner_daemons `logical(1)`. On [evaluate()]'s configs axis, let a
#'   configuration whose share of the run's work is large run its folds on
#'   inner daemons that `evaluate()` starts on that configuration's daemon,
#'   taking as many of the daemons set, so the run does not wait on its
#'   longest configurations. The results are the same either way where
#'   BLAS runs on one thread, which `evaluate()` sets on its daemons through
#'   `RhpcBLASctl` when it is installed; without it they can differ in
#'   floating-point rounding, which the run says (see [evaluate()]'s
#'   "Inner daemons"). `FALSE` runs every configuration on one daemon.
#'   Default `getOption("horizons.inner_daemons", TRUE)`. [evaluate()].
#' @param threads A whole number. The threads `rf`, `xgboost` and `lightgbm`
#'   fit and predict on when the verb runs in this session; the results are
#'   the same on any number. A run with `allow_par = TRUE` fits each on one
#'   thread a daemon, since the daemons set are its cores, and refuses a
#'   `threads` above 1. Default 1. [evaluate()], [fit()].
#' @param no_improve A whole number of at least 1, or `FALSE`. The Bayesian
#'   iterations without improvement on the best after which a search stops:
#'   [evaluate()]'s, and [fit()]'s re-tune. Each verb says how many of its
#'   searches stopped early, and how to change it. `FALSE` runs every
#'   iteration asked for. Default 10, tune's own. A smaller limit runs
#'   faster but can end a search that is still exploring: when the starting
#'   grid puts only one point in a narrow good region, as it can for
#'   `svm_rbf` on PCA scores, the search may take more than five iterations
#'   to improve on that point. [evaluate()], [fit()].
#' @param min_resemblance_rows A whole number of at least 2. The fewest library
#'   rows [select_training()]'s resemblance check takes its reference
#'   distribution from. Default 30.
#' @return A `horizons_control` object.
#'
#' @examples
#' ## Quiet, on the daemons set with mirai::daemons()
#' ctrl <- control_horizons(allow_par = TRUE, verbose = FALSE)
#'
#' \dontrun{
#' evaluated <- evaluate(configured, control = ctrl)
#' fitted    <- fit(evaluated, control = ctrl)
#' }
#'
#' @export
control_horizons <- function(...,
                             allow_par            = NULL,
                             parallelize_over     = NULL,
                             verbose              = NULL,
                             output_dir           = NULL,
                             debug                = NULL,
                             breaker              = NULL,
                             keep_traces          = NULL,
                             chunk_size           = NULL,
                             min_calib_rows       = NULL,
                             min_ad_rows          = NULL,
                             min_degradation_rows = NULL,
                             isolate              = NULL,
                             inner_daemons        = NULL,
                             threads              = NULL,
                             no_improve           = NULL,
                             min_resemblance_rows = NULL) {

  ### By name only, so a value never lands on a field by its position
  if (...length() > 0) {

    passed  <- names(list(...)) %||% rep("", ...length())
    unknown <- passed[passed != ""]

    abort_horizons(c(
      "{.fn control_horizons} takes its fields by name.",
      "x" = if (length(unknown) > 0) "It has no {cli::qty(length(unknown))}field{?s} {.field {unknown}}.",
      "x" = if (any(passed == "")) "{sum(passed == '')} value{?s} {?was/were} passed by position.",
      "i" = "Its arguments are the fields, e.g. {.code control_horizons(allow_par = TRUE)}."
    ), kind = "input")

  }

  fields <- mget(names(CONTROL_FIELDS), envir = environment())

  for (field in names(fields)) {

    if (!is.null(fields[[field]])) {

      CONTROL_FIELDS[[field]]$check(fields[[field]], arg = field, call = rlang::current_env())

    }

  }

  structure(fields, class = "horizons_control")

}


## ---------------------------------------------------------------------------
## resolve_control(): the fields one verb runs with
## ---------------------------------------------------------------------------

#' Resolve a control object for one verb
#'
#' @description
#' Refuses anything that is not a `horizons_control`, and any field set that
#' `verb` does not use, naming the verbs that do. Returns the fields `verb`
#' uses, each set value kept and every other filled with its default: the
#' control the verb runs with, and the one it records.
#'
#' @param control A `horizons_control`, or `NULL` for every default.
#' @param verb `character(1)`. One of `CONTROL_VERBS`.
#' @param call The frame a refusal reports: the verb's.
#' @return A `horizons_control` holding `verb`'s fields.
#' @keywords internal
#' @noRd
resolve_control <- function(control, verb, call = rlang::caller_env()) {

  if (is.null(control)) control <- control_horizons()

  if (!inherits(control, "horizons_control")) {

    abort_horizons(c(
      "{.arg control} must be made by {.fn control_horizons}.",
      "x" = "Got {.obj_type_friendly {control}}."
    ), kind = "input", call = call)

  }

  ### A name the table does not know is a typo or an edit, never a setting
  unknown <- setdiff(names(control), names(CONTROL_FIELDS))

  if (length(unknown) > 0) {

    abort_horizons(c(
      "{.arg control} has {cli::qty(length(unknown))}{?a field/fields} {.field {unknown}} that {.fn control_horizons} does not know.",
      "i" = "Make it with {.fn control_horizons}, whose arguments are the fields."
    ), kind = "input", call = call)

  }

  used   <- names(CONTROL_FIELDS)[vapply(CONTROL_FIELDS, function(f) verb %in% f$verbs, logical(1))]
  set    <- names(CONTROL_FIELDS)[!vapply(names(CONTROL_FIELDS), function(f) is.null(control[[f]]), logical(1))]
  unused <- setdiff(set, used)

  if (length(unused) > 0) {

    users <- vapply(unused, function(f) {
      paste0(f, ": ", paste0(CONTROL_FIELDS[[f]]$verbs, "()", collapse = ", "))
    }, character(1))

    abort_horizons(c(
      "{.fn {verb}} does not use {cli::qty(length(unused))}{?the control field/the control fields} {.field {unused}}.",
      stats::setNames(cli_escape(users), rep("i", length(users)))
    ), kind = "input", call = call)

  }

  resolved <- lapply(stats::setNames(used, used), function(f) {
    control[[f]] %||% CONTROL_FIELDS[[f]]$default()
  })

  ### A default read from an option is the user's to set, so it is checked
  ### too, and a bad one is reported as the option's
  for (f in used) {

    if (!is.null(resolved[[f]])) {

      option <- if (is.null(control[[f]])) CONTROL_FIELDS[[f]]$option
      arg    <- if (is.null(option)) f else paste0("getOption(\"", option, "\")")

      CONTROL_FIELDS[[f]]$check(resolved[[f]], arg = arg, call = call)

    }

  }

  structure(resolved, class = "horizons_control")

}


#' Refuse threads above one on a parallel run
#'
#' @description
#' On the daemons each engine fits on one thread, so the cores a parallel
#' run uses are the daemons set (DECISIONS 2026-11-13); `threads` is the
#' count for a run in this session. Asking for both is refused at the verb's
#' door, before anything is drawn.
#'
#' @param control The verb's resolved control ([resolve_control()]).
#' @param call The frame the refusal reports: the verb's.
#' @return `NULL`, invisibly; aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_threads_sequential <- function(control, call = rlang::caller_env()) {

  threads <- control$threads

  if (isTRUE(control$allow_par) && threads > 1) {

    abort_horizons(c(
      "{.code control_horizons(threads = {threads})} is for a run in this session, and {.code allow_par = TRUE} runs on the mirai daemons, each fitting on one thread.",
      "i" = "To use more cores in parallel, set more daemons with {.code mirai::daemons(n)}; to fit on {threads} threads here, leave {.arg allow_par} off."
    ), kind = "input", call = call)

  }

  invisible(NULL)

}


#' @export
print.horizons_control <- function(x, ...) {

  set <- Filter(Negate(is.null), unclass(x))

  cat("<horizons_control>\n")

  if (length(set) == 0) {

    cat("  every field at its default\n")

  } else {

    for (f in names(set)) {
      cat("  ", f, ": ", paste(format(set[[f]]), collapse = ", "), "\n", sep = "")
    }

  }

  invisible(x)

}
