## ---------------------------------------------------------------------------
## control_horizons(): how the long-running verbs run
## ---------------------------------------------------------------------------
##
## Arguments say what a verb computes; a control object says how it runs.
## Every field is a row of CONTROL_FIELDS: its default, its check, and the
## verbs that use it. The constructor, the refusal of a field set but unused,
## the per-verb resolution and the documentation all read that table, so a new
## field is a row there and an argument of control_horizons().

## ---------------------------------------------------------------------------
## The field table
## ---------------------------------------------------------------------------

### Checks take the value, the field's name and the frame to report, and abort
### through check_arg(), so a bad value is an input error naming the field.

control_check_bool <- function(x, arg, call) {

  check_arg(rlang::check_bool, x, arg = arg, call = call)

}

control_check_rows <- function(x, arg, call) {

  check_arg(rlang::check_number_whole, x, min = 2, arg = arg, call = call)

}

control_check_parallelize_over <- function(x, arg, call) {

  if (identical(x, "both")) {

    abort_horizons(c(
      "{.arg {arg}} = {.val both} is not supported.",
      "i" = "Nested parallelism is the user's to register: set a nested {.fn future::plan} and use {.val configs}."
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

control_check_breaker <- function(x, arg, call) {

  if (isFALSE(x)) return(invisible(NULL))

  if (!rlang::is_scalar_integerish(x) || is.na(x) || x < 2) {

    got <- if (is.atomic(x) && length(x) == 1) cli::format_inline("{.val {x}}") else cli::format_inline("{.obj_type_friendly {x}}")

    abort_horizons(
      "{.arg {arg}} must be {.code FALSE} or a whole number of at least 2, not {got}.",
      kind = "input", call = call
    )

  }

}

control_check_chunk_size <- function(x, arg, call) {

  check_arg(rlang::check_number_whole, x, min = 1, arg = arg, call = call)

}

control_check_bytes <- function(x, arg, call) {

  check_arg(rlang::check_number_decimal, x, min = 1, allow_infinite = TRUE, arg = arg, call = call)

}

CONTROL_VERBS <- c("evaluate", "fit", "ensemble", "select_training")

### `default` is a function, so a default read from an option is read when the
### verb runs, not when the package loads.
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
    check   = control_check_breaker,
    verbs   = c("evaluate", "fit")
  ),

  keep_traces = list(
    default = function() FALSE,
    check   = control_check_bool,
    verbs   = CONTROL_VERBS
  ),

  chunk_size = list(
    default = function() SELECT_CHUNK_SIZE,
    check   = control_check_chunk_size,
    verbs   = "select_training"
  ),

  min_calib_rows = list(
    default = function() N_CALIB_MIN,
    check   = control_check_rows,
    verbs   = c("fit", "ensemble")
  ),

  min_ad_rows = list(
    default = function() N_AD_TRAIN_MIN,
    check   = control_check_rows,
    verbs   = "fit"
  ),

  min_degradation_rows = list(
    default = function() DEGRADATION_MIN_N,
    check   = control_check_rows,
    verbs   = "fit"
  ),

  min_resemblance_rows = list(
    default = function() SELECT_RESEMBLANCE_MIN_ROWS,
    check   = control_check_rows,
    verbs   = "select_training"
  ),

  ### A future.globals.maxSize the user has set is theirs, and wins.
  max_worker_payload = list(
    default = function() getOption("future.globals.maxSize", EVAL_WORKER_PAYLOAD_LIMIT),
    check   = control_check_bytes,
    verbs   = "evaluate"
  )

)


## ---------------------------------------------------------------------------
## control_horizons()
## ---------------------------------------------------------------------------

#' Control how evaluate(), fit(), ensemble() and select_training() run
#'
#' @description
#' Arguments say what a verb computes; a control object says how it runs:
#' parallelism, console output, checkpoints, how failures are handled, and the
#' minimums below which part of a run is skipped or refused. None of these
#' change what a successful run means.
#'
#' Every field defaults to `NULL`, which leaves it to the verb's default. A
#' verb refuses a field that is set but that it does not use, so one control
#' object can be passed to several verbs, each reading its own fields. The
#' control a verb ran with, defaults filled in, is recorded on the slot it
#' writes (`evaluation$control`, `models$control`, `ensemble$control`,
#' `selection$settings$control`).
#'
#' @param allow_par `logical(1)`. Run on the registered `future` plan. The
#'   user owns the plan; the verb never sets one. Default `FALSE`.
#'   [evaluate()], [fit()], [ensemble()].
#' @param parallelize_over `character(1)`. The axis of [evaluate()]'s work the
#'   plan is applied to: `"auto"` (the default), `"configs"` or `"resamples"`.
#'   `"auto"` never chooses `"configs"` without `output_dir`, and
#'   `"configs"` asked for explicitly needs one.
#' @param verbose `logical(1)`. Print the verb's progress tree. Default
#'   `getOption("horizons.verbose", TRUE)`, so a script can quiet every verb
#'   at once. All four verbs.
#' @param output_dir `character(1)`. A directory where [evaluate()] writes
#'   each configuration's result as it finishes, and from which it resumes.
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
#'   of the results. Default `FALSE`. All four verbs.
#' @param chunk_size A whole number. Targets per chunk in
#'   [select_training()]'s neighbour search. Default 500.
#' @param min_calib_rows A whole number of at least 2. The fewest calibration
#'   rows [fit()]'s intervals and applicability domain, and [ensemble()]'s
#'   CV+ intervals, are computed from; below it they are skipped. Default 30.
#' @param min_ad_rows A whole number of at least 2. The fewest training rows
#'   [fit()] estimates the applicability domain's covariance from. Default 10.
#' @param min_degradation_rows A whole number of at least 2. The fewest test
#'   rows [fit()]'s degradation check bootstraps from. Default 10.
#' @param min_resemblance_rows A whole number of at least 2. The fewest library
#'   rows [select_training()]'s resemblance check takes its reference
#'   distribution from. Default 30.
#' @param max_worker_payload A number of bytes. The largest payload
#'   [evaluate()] sends a worker on the configs axis. Default the
#'   `future.globals.maxSize` option when you have set it, otherwise 1 GiB.
#'
#' @return A `horizons_control` object.
#'
#' @examples
#' ## Quiet, on the registered plan
#' ctrl <- control_horizons(allow_par = TRUE, verbose = FALSE)
#'
#' \dontrun{
#' evaluated <- evaluate(configured, control = ctrl)
#' fitted    <- fit(evaluated, control = ctrl)
#' }
#'
#' @export
control_horizons <- function(allow_par            = NULL,
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
                             min_resemblance_rows = NULL,
                             max_worker_payload   = NULL) {

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

  ### A default read from an option is the user's to set, so it is checked too
  for (f in used) {

    if (!is.null(resolved[[f]])) {
      CONTROL_FIELDS[[f]]$check(resolved[[f]], arg = f, call = call)
    }

  }

  structure(resolved, class = "horizons_control")

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
