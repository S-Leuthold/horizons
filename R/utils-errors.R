## ---------------------------------------------------------------------------
## abort_horizons
## ---------------------------------------------------------------------------

#' The kinds a horizons error can be
#'
#' @description
#' Every horizons error carries exactly one of these, as the class
#' `horizons_error_<kind>` (see [horizons_conditions]).
#'
#' @keywords internal
#' @noRd
CONDITION_KINDS <- c("input", "validation", "dependency", "run", "internal")

#' Raise a horizons error of one kind
#'
#' @description
#' The one constructor every horizons error goes through. The condition
#' carries the base class `horizons_error`, exactly one kind
#' (`horizons_error_<kind>`), and at most one specific subclass in front of
#' the kind, so a handler can catch all of horizons' errors, one kind of
#' them, or one refusal (see [horizons_conditions]).
#'
#' An `"internal"` error is a broken invariant, a bug in horizons. It is
#' raised with `.internal = TRUE`, which adds rlang's footer naming the
#' package and its issue tracker; the class is what handlers and tests
#' match on, since the flag adds none.
#'
#' @param message Character. A cli message: the problem statement, then
#'   bullets. Interpolated in `.envir`, so `{x}` reads the caller's `x`.
#' @param kind Character(1). One of `"input"`, `"validation"`,
#'   `"dependency"`, `"run"` or `"internal"`. Required: a horizons error
#'   always says which kind it is.
#' @param class Character(1) or `NULL`. A specific subclass, written in full
#'   (`"horizons_error_all_configs_failed"`), when the refusal earns one.
#' @param ... Passed to [cli::cli_abort()]: fields, `parent`, `body`.
#' @param call The frame the error reports, the caller's by default.
#' @param .envir The environment the message is interpolated in.
#'
#' @return Does not return.
#' @keywords internal
#' @noRd
abort_horizons <- function(message,
                           kind,
                           class  = NULL,
                           ...,
                           call   = rlang::caller_env(),
                           .envir = rlang::caller_env()) {

  ## Check the class arguments ------------------------------------------------

  ### Only horizons calls this, so a bad kind or subclass is a bug in the
  ### caller, reported as one rather than raised under a made-up class.
  if (!rlang::is_string(kind) || !kind %in% CONDITION_KINDS) {

    cli::cli_abort(
      "{.arg kind} must be one of {.or {.val {CONDITION_KINDS}}}, not {.obj_type_friendly {kind}}.",
      class = c("horizons_error_internal", "horizons_error"),
      .internal = TRUE
    )

  }

  if (!is.null(class) &&
      (!rlang::is_string(class) || !startsWith(class, "horizons_error_") ||
       class %in% paste0("horizons_error_", CONDITION_KINDS))) {

    cli::cli_abort(
      "{.arg class} must be one specific subclass named {.val horizons_error_<type>}, not a kind.",
      class = c("horizons_error_internal", "horizons_error"),
      .internal = TRUE
    )

  }

  ## Raise ---------------------------------------------------------------------

  cli::cli_abort(
    message,
    class     = c(class, paste0("horizons_error_", kind), "horizons_error"),
    ...,
    call      = call,
    .envir    = .envir,
    .internal = identical(kind, "internal")
  )

}

#' Check a user's argument with one of rlang's checkers
#'
#' @description
#' rlang's exported checkers ([rlang::check_bool()], [rlang::check_string()],
#' [rlang::check_number_whole()], [rlang::check_number_decimal()],
#' [rlang::check_data_frame()]) write the message the tidyverse uses for a
#' value of the wrong type. This runs one of them with horizons' input class,
#' so the refusal is a `horizons_error_input` like every other door check,
#' and passes on the argument's name and the verb's frame, so the message
#' names the user's argument and call rather than this helper.
#'
#' @param check A checker from rlang, e.g. `rlang::check_bool`.
#' @param x The value to check.
#' @param ... Passed to `check`, e.g. `allow_null = TRUE`, `min = 1`.
#' @param arg The argument's name, as the user wrote it.
#' @param call The frame the error reports, the caller's by default.
#'
#' @return `NULL` invisibly when `x` passes; otherwise an error of class
#'   `horizons_error_input`.
#' @keywords internal
#' @noRd
check_arg <- function(check,
                      x,
                      ...,
                      arg  = rlang::caller_arg(x),
                      call = rlang::caller_env()) {

  check(x, ..., arg = arg, call = call,
        class = c("horizons_error_input", "horizons_error"))

}

## ---------------------------------------------------------------------------
## horizons_conditions: the classes, documented in one place
## ---------------------------------------------------------------------------

#' Errors and warnings horizons raises
#'
#' @description
#' Every error horizons raises carries the base class `horizons_error`,
#' exactly one kind, and sometimes one more specific class in front of the
#' kind. Every warning carries the base class `horizons_warning` and one
#' topic. A handler can catch all of horizons' errors, one kind of them, or
#' one specific refusal, and filter warnings by what happened.
#'
#' @section Error kinds:
#' \describe{
#'   \item{`horizons_error_input`}{The call cannot run as written: an
#'     argument outside its domain, a required input missing, or data that
#'     breaks a stated requirement. Raised before any work starts.}
#'   \item{`horizons_error_validation`}{An object breaks its class contract,
#'     because it was edited by hand or written by an older version. Re-run
#'     the verb that wrote the field, or start again from [configure()].}
#'   \item{`horizons_error_dependency`}{A needed package is missing, or the
#'     environment cannot run the call (a parallel run under
#'     `devtools::load_all()`, whose workers load the installed build, or one
#'     whose workers load another version of horizons).}
#'   \item{`horizons_error_run`}{The work was attempted and failed: every
#'     configuration or member failed, a download failed, or the run stopped
#'     on a failure that would hit every unit the same way.}
#'   \item{`horizons_error_internal`}{A bug in horizons: something the code
#'     guarantees has not held. Please report it.}
#' }
#'
#' @section Specific errors:
#' \describe{
#'   \item{`horizons_error_all_configs_failed`}{(run) Every configuration
#'     failed in [evaluate()]. The per-configuration results travel on the
#'     condition as `results`.}
#'   \item{`horizons_error_all_members_failed`}{(run) Every member failed in
#'     `fit()`. The per-member results travel as `results`.}
#'   \item{`horizons_error_prediction_failed`}{(run) `predict()` had nothing
#'     to return: the configuration asked for, or every one of them, could
#'     not predict, or an ensemble's member or its combination could not. One
#'     row per failure travels as `results`.}
#'   \item{`horizons_error_systemic`}{(run) [evaluate()] stopped early,
#'     because the first configurations to run failed the same way across at
#'     least two models (the circuit breaker,
#'     `control_horizons(breaker = )`), or because a parallel worker died.
#'     The rows finished so far travel as `results`.}
#'   \item{`horizons_error_promoted_warnings`}{(run) A verb ended with
#'     warnings under `options(warn = 2)`. Instead of turning the first warning
#'     into an error that loses the result, the verb aborts with the finished
#'     result as `object` and its warnings as `log`.}
#'   \item{`horizons_error_download`}{(run) A spectral library could not be
#'     downloaded.}
#'   \item{`horizons_error_boruta`}{(run) [step_select_boruta()] failed.}
#'   \item{`horizons_error_qc`}{(run) A quality check failed once it had been
#'     computed.}
#'   \item{`horizons_error_consent`}{(input) A library download was declined.}
#'   \item{`horizons_error_missing_predict_package`}{(dependency) `predict()`
#'     needs a model package that is not installed.}
#' }
#'
#' @section Warning topics:
#' \describe{
#'   \item{`horizons_warning_substitution`}{Something other than what was
#'     asked ran, under a documented fallback.}
#'   \item{`horizons_warning_quality`}{Everything ran as asked, but the result
#'     is weaker than its output shows (small folds, every member below the
#'     prune threshold).}
#'   \item{`horizons_warning_skipped`}{Something asked for did not run, such
#'     as a member's prediction intervals.}
#'   \item{`horizons_warning_flagged`}{A step kept samples it could not use;
#'     modelling leaves them out.}
#'   \item{`horizons_warning_partial_failure`}{Some units failed and the rest
#'     returned.}
#'   \item{`horizons_warning_coverage`}{The data only partly covers what is
#'     asked of it, such as a wavenumber range.}
#'   \item{`horizons_warning_applicability`}{Predictions outside the
#'     applicability domain.}
#'   \item{`horizons_warning_input`}{A request that is legal but risky enough
#'     to say so.}
#' }
#'
#' @section Where the record lives:
#' Each verb raises a topic at most once per call, at its end, and only after
#' writing it to the object's log, so a warning that is silenced or never
#' seen is still on the object. The log also keeps the notes a verb reports
#' without warning, such as samples dropped as the user asked, so a
#' pipeline's history is in one place. `predict()`, which changes no object,
#' carries each warning's record as the field `log`.
#'
#' Under `options(warn = 2)`, a warning that no handler takes ends the call
#' with `horizons_error_promoted_warnings`, which carries the finished result
#' as `object`; a warning the caller muffles or catches is handled as R
#' handles any warning.
#'
#' @section Catching errors raised inside a recipe step:
#' recipes wraps every error raised inside a step in `recipes_error_step`,
#' with the original as its parent. A base [tryCatch()] on a horizons class
#' does not see through the wrapper; [rlang::try_fetch()] and
#' [rlang::cnd_inherits()] do.
#'
#' @name horizons_conditions
NULL

## ---------------------------------------------------------------------------
## safely_execute
## ---------------------------------------------------------------------------

#' Safely Evaluate an Expression
#'
#' @description
#' Evaluates an expression using `purrr::safely()` to prevent errors from
#' halting execution. Returns a structured list with result, error, and
#' optionally captured warnings/messages. This is the workhorse for
#' evaluate()'s config loop — each model fit is wrapped in safely_execute()
#' so one failure doesn't kill the batch.
#'
#' @param expr An expression to evaluate (unquoted).
#' @param default_value Value returned if the expression errors. Default NULL.
#' @param error_message Optional character string logged on error. Supports
#'   glue-style interpolation from the caller's environment.
#' @param log_error Logical. If TRUE (default), logs error via `warning()`.
#' @param capture_conditions Logical. If TRUE, captures warnings and messages
#'   emitted during evaluation. Default FALSE.
#'
#' @return Named list:
#'   - `result`: Evaluated result, or `default_value` on error.
#'   - `error`: Error condition object, or NULL on success.
#'   - `warnings`: List of warning strings (if captured), else NULL.
#'   - `messages`: List of message strings (if captured), else NULL.
#'   - `n_warnings`: Integer count of captured warnings.
#'   - `n_messages`: Integer count of captured messages.
#'
#' @keywords internal
safely_execute <- function(expr,
                           default_value      = NULL,
                           error_message      = NULL,
                           log_error          = TRUE,
                           capture_conditions = FALSE) {

  ## Capture expression and caller environment ------------------------------

  expr_quo   <- rlang::enquo(expr)
  caller_env <- rlang::caller_env()

  ## Condition storage ------------------------------------------------------

  captured_warnings <- list()
  captured_messages <- list()

  ## Build safely-wrapped evaluator -----------------------------------------

  safe_eval <- purrr::safely(function() {

    if (capture_conditions) {

      withCallingHandlers(
        rlang::eval_tidy(expr_quo, env = caller_env),
        warning = function(w) {
          captured_warnings <<- append(captured_warnings, list(conditionMessage(w)))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          captured_messages <<- append(captured_messages, list(conditionMessage(m)))
          invokeRestart("muffleMessage")
        }
      )

    } else {

      rlang::eval_tidy(expr_quo, env = caller_env)

    }

  }, otherwise = default_value, quiet = TRUE)

  ## Execute ----------------------------------------------------------------

  result_list <- safe_eval()

  ## Log error if requested -------------------------------------------------

  if (!is.null(result_list$error) && log_error) {

    msg <- if (!is.null(error_message)) {

      tryCatch(
        glue::glue(error_message, .envir = caller_env),
        error = function(e) error_message
      )

    } else {

      "An error occurred"

    }

    warning(paste0(msg, ": ", condition_summary(result_list$error)), call. = FALSE)

  }

  ## Return structured result -----------------------------------------------

  list(
    result     = result_list$result,
    error      = result_list$error,
    warnings   = if (length(captured_warnings) > 0) captured_warnings else NULL,
    messages   = if (length(captured_messages) > 0) captured_messages else NULL,
    n_warnings = length(captured_warnings),
    n_messages = length(captured_messages)
  )

}

## ---------------------------------------------------------------------------
## handle_results
## ---------------------------------------------------------------------------

#' Handle Results from safely_execute
#'
#' @description
#' Processes the output from `safely_execute()`. On success, returns the
#' result (optionally re-emitting captured warnings). On failure, either
#' returns NULL or aborts with a structured error message, depending on
#' `abort_on_null`.
#'
#' @param safe_result List from `safely_execute()` with `result` and `error`.
#' @param error_title Character. Main error message for abort. Default
#'   "Operation failed".
#' @param error_hints Character vector. Troubleshooting suggestions appended
#'   to the error message.
#' @param abort_on_null Logical. Abort if result is NULL? Default TRUE.
#' @param silent Logical. Suppress re-emitted warnings/messages? Default FALSE.
#'
#' @return The result from `safe_result`, or NULL if `abort_on_null = FALSE`.
#' @keywords internal
handle_results <- function(safe_result,
                           error_title   = "Operation failed",
                           error_hints   = NULL,
                           abort_on_null = TRUE,
                           silent        = FALSE) {

  ## Validate input structure -----------------------------------------------

  if (!is.list(safe_result) || !all(c("result", "error") %in% names(safe_result))) {

    rlang::abort("Invalid safe_result structure. Expected output from safely_execute()")

  }

  result <- safe_result$result

  ## Success path -----------------------------------------------------------

  if (!is.null(result)) {

    if (!silent) {

      if (!is.null(safe_result$warnings) && length(safe_result$warnings) > 0) {

        for (w in safe_result$warnings) {
          warning(w, call. = FALSE)
        }

      }

      if (!is.null(safe_result$messages) && length(safe_result$messages) > 0) {

        for (m in safe_result$messages) {
          message(m)
        }

      }

    }

    return(result)

  }

  ## Failure path: return NULL or abort -------------------------------------

  if (!abort_on_null) return(NULL)

  ## Build abort message ----------------------------------------------------

  abort_msg <- error_title

  if (!is.null(error_hints) && length(error_hints) > 0) {

    abort_msg <- c(abort_msg, stats::setNames(error_hints, rep("i", length(error_hints))))

  }

  if (!is.null(safe_result$error)) {

    abort_msg <- c(abort_msg, x = condition_summary(safe_result$error))

  }

  rlang::abort(abort_msg)

}

## ---------------------------------------------------------------------------
## create_failed_result
## ---------------------------------------------------------------------------

#' Create a Standardized Failed-Config Result Row
#'
#' @description
#' When a config fails during evaluate()'s inner loop, this creates a
#' single-row tibble with the expected column structure so the results
#' tibble stays rectangular. All metric columns are NA, status is "failed".
#'
#' @param config_id Character. The config hash identifier.
#' @param error Error condition object, character string, or NULL.
#'
#' @return Single-row tibble with config_id, status, error_message, the
#'   failure's root class (`NA` for an error given as text), `trace` (a list
#'   holding `NULL`), metric columns (all NA), and runtime_secs (NA).
#' @keywords internal
create_failed_result <- function(config_id, error = NULL) {

  ## Extract error message --------------------------------------------------

  error_msg <- if (is.null(error)) {

    NA_character_

  } else if (inherits(error, "condition")) {

    condition_summary(error)

  } else if (is.character(error)) {

    error

  } else {

    as.character(error)

  }

  ## Build result row -------------------------------------------------------

  tibble::tibble(
    config_id     = config_id,
    status        = "failed",
    below_prune_threshold = NA,
    prune_threshold       = NA_real_,
    rmse          = NA_real_,
    rrmse         = NA_real_,
    rsq           = NA_real_,
    ccc           = NA_real_,
    rpd           = NA_real_,
    mae           = NA_real_,
    cv_rmse       = NA_real_,
    cv_rrmse      = NA_real_,
    cv_rsq        = NA_real_,
    cv_ccc        = NA_real_,
    cv_rpd        = NA_real_,
    cv_mae        = NA_real_,
    scoring_schema = SCORING_SCHEMA,
    best_params   = list(NULL),
    error_message = error_msg,
    ## The root cause's class, so a script can tell one kind of failure from
    ## another without reading the message
    class         = if (inherits(error, "condition")) root_class(error) else NA_character_,
    trace         = list(NULL),
    warnings      = list(NULL),
    runtime_secs  = NA_real_
  )

}

## ---------------------------------------------------------------------------
## distinct_config_errors
## ---------------------------------------------------------------------------

#' Summarise the distinct error messages in a results table as cli bullets
#'
#' @description
#' Groups the configs of a results table by their `error_message` and returns
#' one `"x"` bullet per distinct message, in the order the messages first
#' appear, each followed by the configs that raised it, then an `"i"` bullet
#' counting the messages not shown. `evaluate()` and `fit()` put these
#' bullets in the abort they raise when every config (or member) fails, so
#' the cause is in the message rather than only in a table the caller may not
#' be able to reach.
#'
#' The bullets are finished: the upstream text in them has its braces escaped
#' with `cli_escape()`, so they can be passed to cli as they are. This is the
#' one place the brace-safety rule is enforced for these aborts; a caller
#' never builds the bullets itself.
#'
#' @param results Tibble with `config_id` and `error_message` columns.
#' @param n_show Integer. How many distinct messages to list. Default 3.
#' @param n_ids Integer. How many config ids to name per message before
#'   counting the rest. Default 3.
#'
#' @return Named character vector of cli bullets (names `"x"` and `"i"`),
#'   empty when no row has a message. Rows whose `error_message` is `NA` or
#'   empty are ignored.
#' @keywords internal
#' @noRd
distinct_config_errors <- function(results, n_show = 3L, n_ids = 3L) {

  ## Checked by name: `$` on a tibble without the column warns.
  if (!"error_message" %in% names(results)) {

    return(stats::setNames(character(0), character(0)))

  }

  msgs <- results$error_message

  has_msg  <- !is.na(msgs) & nzchar(msgs)
  distinct <- unique(msgs[has_msg])
  shown    <- utils::head(distinct, n_show)
  n_more   <- length(distinct) - length(shown)

  lines <- vapply(shown, function(m) {

    ids <- results$config_id[has_msg & msgs == m]

    id_label <- if (length(ids) > n_ids) {
      paste0(paste(ids[seq_len(n_ids)], collapse = ", "),
             " and ", length(ids) - n_ids, " more")
    } else {
      paste(ids, collapse = ", ")
    }

    paste0(m, " (", id_label, ")")

  }, character(1), USE.NAMES = FALSE)

  bullets <- stats::setNames(cli_escape(lines), rep("x", length(lines)))

  if (n_more > 0) {

    bullets <- c(bullets, "i" = paste0(
      n_more, " more distinct error message", if (n_more > 1) "s", " not shown."
    ))

  }

  bullets

}

## ---------------------------------------------------------------------------
## cli_escape
## ---------------------------------------------------------------------------

#' Escape text for use inside a cli template
#'
#' @description
#' Doubles every brace, which cli reads as a literal brace, so text that did
#' not come from the package (an upstream error message, a column name, a
#' path) can sit in a cli message without being evaluated as an
#' interpolation. A bare `"{wn_600}"` would otherwise make cli look up an
#' object called `wn_600` and crash the abort that was reporting it.
#'
#' @param x Character vector.
#' @return `x` with `{` and `}` doubled.
#' @keywords internal
#' @noRd
cli_escape <- function(x) {

  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)

}

## ---------------------------------------------------------------------------
## condition_summary
## ---------------------------------------------------------------------------

#' The classes of a condition horizons composed
#'
#' @description
#' A condition raised with [abort_horizons()], [warn_in_unit()] or
#' [note_in_unit()]. Its message is kept whole wherever it is recorded
#' ([condition_parts()]): any other package's text inside it was capped when
#' it was interpolated, through [condition_summary()]. Matched by exact
#' class, so a step's own class (`horizons_boruta_error`) or one from before
#' the conditions contract (`horizons_input_error`) is capped as another
#' package's text until its code raises through [abort_horizons()].
#'
#' @keywords internal
#' @noRd
HORIZONS_OWN_CLASSES <- c("horizons_error", "horizons_warning", "horizons_note")

#' Summarise a condition as one readable line
#'
#' @description
#' Turns a caught condition into the text a results-table cell or a cli
#' bullet should carry: the root cause of the condition's parent chain, on
#' one line, followed by where it happened when the cause arrived wrapped.
#' Every capture site that records why something failed goes through here.
#'
#' A recipe step error is the case this exists for. recipes wraps every step
#' error in a `recipes_error_step` condition whose `$message` is empty, with
#' the text in its parent chain, so capture sites that read `$message` stored
#' `"Parameter finalization failed: "` and nothing after it (#96). The same
#' error summarises as ``"boom from step (in `step_mutate()`)"``.
#'
#' Each condition's own text comes from `rlang::cnd_message(inherit = FALSE)`,
#' which renders base and rlang conditions alike, cli bodies included. The
#' deepest condition in the chain with any text is the cause; the outermost
#' condition above it whose call has a name says where. ANSI styling and cli
#' bullet glyphs are stripped, bullets are joined to the text before them, and
#' whitespace is collapsed so that lines cli wrapped rejoin.
#'
#' `x` may instead be text an upstream package already rendered from a
#' condition, such as a note in tune's `.notes`. Rendered text is read the
#' same way: the part after the last `"Caused by ..."` line is the cause, and
#' an opening ``"Error in `f()`:"`` line says where.
#'
#' The result is plain text, not a cli template. Interpolate it as a value
#' (`"{msg}"`), or pass it through [cli_escape()], before it meets cli.
#'
#' @param x A condition, or a character string holding a rendered message.
#' @param detail Character or `NULL`. Extra context for the closing
#'   parentheses, after the location, e.g. how many folds raised a note.
#' @param max_chars Integer. The longest cause kept before it is cut and
#'   marked with `"..."`. Only another package's text is cut: a condition
#'   horizons composed (`HORIZONS_OWN_CLASSES`) is kept whole.
#'
#' @return Character(1), never empty. A condition with no text anywhere in
#'   its chain is described by its class.
#' @keywords internal
#' @noRd
condition_summary <- function(x, detail = NULL, max_chars = 300L) {

  parts <- condition_parts(x, max_chars = max_chars)

  format_condition(parts$cause, parts$where, detail)

}

#' The cause and location of a condition, before they are joined
#'
#' @inheritParams condition_summary
#' @return List with `cause` (character(1), never empty, capped at
#'   `max_chars` unless horizons composed it) and `where` (a backticked
#'   call, or `NA`).
#' @keywords internal
#' @noRd
condition_parts <- function(x, max_chars = 300L) {

  ## This runs on the failure path, so it must not fail itself: text it
  ## cannot read (an invalid multibyte string, say) is reported as such
  ## rather than taking the config down with it.
  parts <- tryCatch(
    if (inherits(x, "condition")) condition_cause(x) else rendered_cause(x),
    error = function(e) list(
      cause    = "",
      where    = NULL,
      fallback = paste0(if (inherits(x, "condition")) class(x)[1] else "error",
                        " (message could not be read)")
    )
  )

  cause <- if (!is.na(parts$cause) && nzchar(parts$cause)) parts$cause else parts$fallback

  ## The cap keeps a results-table cell readable. The cause is at the front,
  ## so a cut only drops the tail of a long root message (in practice the
  ## trailing cli hint bullets), never the reason itself, and the location
  ## and detail are added after the cut so they always survive. Only text
  ## another package raised is cut: a message horizons composed is kept
  ## whole, since the other package's text inside it went through this cap
  ## when it was interpolated, and cutting it would drop what the package
  ## added after that text, a location or a fold count.
  if (!isTRUE(parts$own) && isTRUE(nchar(cause, allowNA = TRUE) > max_chars)) {

    cause <- paste0(substr(cause, 1L, max_chars - 3L), "...")

  }

  list(cause = cause, where = parts$where %||% NA_character_)

}

#' Join a cause to its location and detail
#'
#' @param cause Character(1).
#' @param where Character(1) or `NA`/`NULL`: a backticked call.
#' @param detail Character or `NULL`: further context, e.g. a fold count.
#' @return ``"cause (in `f()`; detail)"``, or `cause` alone when there is no
#'   context.
#' @keywords internal
#' @noRd
format_condition <- function(cause, where = NA_character_, detail = NULL) {

  context <- c(if (length(where) == 1 && !is.na(where)) paste0("in ", where), detail)
  context <- context[!is.na(context) & nzchar(context)]

  if (length(context) == 0) return(cause)

  paste0(cause, " (", paste(context, collapse = "; "), ")")

}

#' Root cause and location of a condition's parent chain
#'
#' @param cnd A condition.
#' @return List with `cause` (character, possibly empty), `where` (a
#'   backticked call such as ``"`step_mutate()`"``, or `NULL`), `fallback`
#'   (the text to use when `cause` is empty) and `own` (whether the condition
#'   the cause is read from is one horizons composed, `HORIZONS_OWN_CLASSES`).
#' @keywords internal
#' @noRd
condition_cause <- function(cnd) {

  ## Walk the parent chain, outermost first ----------------------------------

  ### Bounded, so a chain that loops back on itself cannot hang a capture site.
  chain <- list()
  node  <- cnd

  while (inherits(node, "condition") && length(chain) < 50L) {

    chain[[length(chain) + 1L]] <- node
    node <- tryCatch(node[["parent"]], error = function(e) NULL)

  }

  ## Render each condition's own text -----------------------------------------

  ### With the rendering pinned rather than left to the session: unicode
  ### bullet glyphs, which clean_message_text() can tell from text (under
  ### testthat, or in a non-UTF-8 locale, cli would otherwise use the letters
  ### "i", "x" and "v"), no colour, and no wrapping.
  old_opts <- options(cli.unicode = TRUE, cli.num_colors = 1L, cli.width = 100000L)
  on.exit(options(old_opts), add = TRUE)

  texts    <- vapply(chain, condition_own_text, character(1))
  has_text <- which(nzchar(texts))
  fallback <- paste0(class(cnd)[1], " (no message)")

  if (length(has_text) == 0) {

    return(list(cause = "", where = NULL, fallback = fallback))

  }

  ## The deepest condition with text is the reason ---------------------------

  root <- max(has_text)

  ## The outermost wrapper above it with a named call is where ---------------

  ### For a recipe step error that is the step, e.g. `step_select_boruta()`.
  where <- NULL

  for (outer in chain[seq_len(root - 1L)]) {

    where <- condition_call_label(outer)

    if (!is.null(where)) break

  }

  list(cause = texts[root], where = where, fallback = fallback,
       own = inherits(chain[[root]], HORIZONS_OWN_CLASSES))

}

#' One condition's own text, without its parents
#'
#' @param cnd A condition.
#' @return Character(1), cleaned by [clean_message_text()]; `""` when the
#'   condition carries no text of its own.
#' @keywords internal
#' @noRd
condition_own_text <- function(cnd) {

  txt <- tryCatch(rlang::cnd_message(cnd, inherit = FALSE),
                  error = function(e) NULL)

  ### A conditionMessage() method that errors, or returns something that is
  ### not text, leaves the raw field as the last resort.
  if (!is.character(txt) || length(txt) == 0) {

    txt <- tryCatch(cnd[["message"]], error = function(e) NULL)

  }

  if (!is.character(txt) || length(txt) == 0) return("")

  clean_message_text(txt)

}

#' Name a condition's call for a location label
#'
#' @param cnd A condition.
#' @return ``"`fn()`"`` or ``"`pkg::fn()`"``, or `NULL` when the condition has
#'   no call or its call has no plain function name: a handler frame such as
#'   `value[[3L]](cond)` or an operator such as `x[[i]]` says nothing useful.
#' @keywords internal
#' @noRd
condition_call_label <- function(cnd) {

  call <- tryCatch(conditionCall(cnd), error = function(e) NULL)

  if (!is.call(call)) return(NULL)

  fn <- tryCatch(rlang::call_name(call), error = function(e) NULL)

  if (is.null(fn) || !grepl("^[.A-Za-z][.A-Za-z0-9_]*$", fn)) return(NULL)

  ns <- tryCatch(rlang::call_ns(call), error = function(e) NULL)

  paste0("`", if (!is.null(ns)) paste0(ns, "::"), fn, "()`")

}

#' Root cause and location of an already rendered message
#'
#' @param x Character. Text rendered from a condition, e.g. a tune note, in
#'   rlang's layout: an optional ``"Error in `f()`:"`` line, then
#'   `"Caused by error ...:"` lines, each followed by that parent's message.
#' @return List shaped like [condition_cause()]'s.
#' @keywords internal
#' @noRd
rendered_cause <- function(x) {

  text  <- cli::ansi_strip(valid_utf8(paste(as.character(x), collapse = "\n")))
  lines <- strsplit(text, "\r?\n")[[1]]
  where <- NULL

  if (length(lines) == 0) {

    return(list(cause = "", where = NULL, fallback = "no message"))

  }

  caused <- grep("^Caused by (error|warning)( in .+)?:\\s*$", lines)

  if (length(caused) > 0) {

    header <- regmatches(
      lines[1],
      regexec("^(?:Error|Warning) in (.+):\\s*$", lines[1], perl = TRUE)
    )[[1]]

    if (length(header) == 2L) where <- header[2]

    lines <- lines[-seq_len(max(caused))]

  } else if (grepl("^(Error|Warning)( in .+)?:\\s*$", lines[1])) {

    lines <- lines[-1L]

  }

  ### Text rendered elsewhere may carry cli's ASCII bullets.
  list(cause    = clean_message_text(lines, ascii_bullets = TRUE),
       where    = where,
       fallback = "no message")

}

#' Flatten message text to one clean line
#'
#' @param x Character. Message text, possibly multi-line, styled, and
#'   bulleted by cli.
#' @param ascii_bullets Logical. Also read cli's ASCII bullets (`"i"`, `"x"`,
#'   `"v"`, `">"`) at the start of a line after the first. For text rendered
#'   outside [condition_cause()]'s pinned options, where they cannot be told
#'   apart from a line that starts with one of those letters as a word, so the
#'   first line, where cli puts a header rather than a bullet, is left alone.
#' @return Character(1): ANSI styling and leading bullet glyphs removed, each
#'   bullet joined to the text before it with `"; "` (or a space, when that
#'   text already ends in punctuation), every other line break and run of
#'   whitespace collapsed to one space.
#' @keywords internal
#' @noRd
clean_message_text <- function(x, ascii_bullets = FALSE) {

  text  <- cli::ansi_strip(valid_utf8(paste(x, collapse = "\n")))
  ### cli keeps some inline spans together with non-breaking spaces, which
  ### "\\s" does not match.
  text  <- gsub("\u00a0", " ", text, fixed = TRUE)
  lines <- strsplit(text, "\r?\n")[[1]]

  ### cli's bullet glyphs (cross, info, bullet, tick, arrow, warning sign) and
  ### rlang's "!" and "*". Only at the start of a line, where cli puts them.
  glyph  <- "^\\s*(\u2716|\u2715|\u2139|\u2022|\u2714|\u2192|\u26a0|!|\\*)\\s+"
  bullet <- grepl(glyph, lines, perl = TRUE)
  lines  <- sub(glyph, "", lines, perl = TRUE)

  if (ascii_bullets && length(lines) > 1) {

    ascii <- "^(i|x|v|>) (?=\\S)"
    later <- seq_along(lines) > 1 & !bullet & grepl(ascii, lines, perl = TRUE)

    bullet[later] <- TRUE
    lines[later]  <- sub(ascii, "", lines[later], perl = TRUE)

  }

  lines <- trimws(lines)

  keep   <- nzchar(lines)
  lines  <- lines[keep]
  bullet <- bullet[keep]

  if (length(lines) == 0) return("")

  ### A bullet after a line that already ends a sentence needs no "; ".
  ends <- grepl("[.!?:;]$", lines)
  sep  <- c("", ifelse(bullet[-1] & !ends[-length(lines)], "; ", " "))

  trimws(gsub("\\s+", " ", paste0(sep, lines, collapse = "")))

}

#' Make text safe for the string functions
#'
#' @param x Character.
#' @return `x` in UTF-8, with any byte that is not valid UTF-8 written out as
#'   `"<e9>"` rather than left to make `strsplit()` or `nchar()` fail. An
#'   upstream message in another encoding is the case.
#' @keywords internal
#' @noRd
valid_utf8 <- function(x) {

  out <- iconv(enc2utf8(x), from = "UTF-8", to = "UTF-8", sub = "byte")
  out[is.na(out)] <- ""
  out

}

## ---------------------------------------------------------------------------
## Warning records: tune's notes and captured warnings, deduplicated
## ---------------------------------------------------------------------------
## A config's warnings arrive from several places: conditions safely_execute()
## caught (rendered by cli, possibly wrapped and bulleted), the notes tune kept
## for each resample, and the notes of the final fit on the test set. The same
## warning usually arrives from more than one of them, so they are gathered as
## records first and reduced to one line per distinct message when the config
## returns.

#' An empty set of warning records
#'
#' @return Tibble with one row per distinct text from one source: `cause`,
#'   `where` (backticked call or `NA`), `n` (how often it arrived), `scope`
#'   (`"cv"`, `"test set"`, or `NA` for a captured warning), `folds` (list of
#'   the resample ids that raised it), `n_folds` (resamples in that result)
#'   and `pinned` (a package notice, always shown).
#' @keywords internal
#' @noRd
new_warning_log <- function() {

  tibble::tibble(
    cause   = character(0),
    where   = character(0),
    n       = integer(0),
    scope   = character(0),
    folds   = list(),
    n_folds = integer(0),
    pinned  = logical(0)
  )

}

#' Warning records from captured text
#'
#' @param x Character (or a list of strings, as `safely_execute()` returns).
#' @param pinned Logical. A package notice that the cap must never hide,
#'   and the package's own text, so it is not cut at 300 characters either.
#' @return Records shaped like [new_warning_log()]'s, one per distinct text.
#' @keywords internal
#' @noRd
text_records <- function(x, pinned = FALSE) {

  x <- as.character(unlist(x))

  distinct <- unique(x)

  ### A pinned line is the package's own text, kept whole as a message
  ### horizons composed is; captured text from another package is capped.
  parts <- lapply(distinct, condition_parts, max_chars = if (pinned) Inf else 300L)

  tibble::tibble(
    cause   = vapply(parts, `[[`, character(1), "cause"),
    where   = vapply(parts, `[[`, character(1), "where"),
    n       = tabulate(match(x, distinct), nbins = length(distinct)),
    scope   = NA_character_,
    folds   = rep(list(character(0)), length(distinct)),
    n_folds = NA_integer_,
    pinned  = pinned
  )

}

#' Warning records from what a unit recorded
#'
#' @description
#' The warnings [run_unit()] caught in a unit, as records that merge with
#' tune's notes: a warning caught as it was raised and the same warning kept
#' in tune's notes are one line on the row. A horizons warning (one raised
#' with [warn_in_unit()]) is a package notice, so it is pinned. tune's own
#' "All models failed" warning is left out: it is raised beside a result in
#' which every model failed, and [check_tune_result()] records that as the
#' failure, with its cause.
#'
#' A note ([note_in_unit()]) is a package notice too, pinned the same way.
#'
#' @param records Records shaped like [new_unit_records()]'s.
#' @return Records shaped like [new_warning_log()]'s, one per warning or note.
#' @keywords internal
#' @noRd
unit_warning_records <- function(records) {

  warnings <- records[records$type %in% c("warning", "note") &
                        !startsWith(records$cause, "All models failed"), , drop = FALSE]

  tibble::tibble(
    cause   = warnings$cause,
    where   = warnings$where,
    n       = warnings$n,
    scope   = rep(NA_character_, nrow(warnings)),
    folds   = rep(list(character(0)), nrow(warnings)),
    n_folds = rep(NA_integer_, nrow(warnings)),
    pinned  = startsWith(warnings$class, "horizons_warning_") | warnings$type == "note"
  )

}

#' Warning or error records from the notes of a tune result
#'
#' @param tune_results A `tune_results` object (grid, Bayesian, resamples or
#'   `last_fit()`), or anything `tune::collect_notes()` reads.
#' @param type Character. Note types to keep: `"warning"`, `"error"`, or both.
#' @param scope Character(1). `"cv"` for notes from cross-validation
#'   resamples, `"test set"` for `last_fit()`'s.
#' @param candidate `NULL`, or the candidate the notes are read for, from
#'   [candidate_of()]: only the notes that belong to it are kept (#109).
#' @return Records shaped like [new_warning_log()]'s, one per distinct note
#'   text; empty when there are none. Notes that cannot be read are one
#'   pinned record saying so, with the cause, so a row never reads as clean
#'   when its notes were lost.
#' @keywords internal
#' @noRd
tune_note_records <- function(tune_results, type = "warning", scope = "cv",
                              candidate = NULL) {

  notes <- tryCatch(tune::collect_notes(tune_results), error = function(e) e)

  readable <- is.data.frame(notes) && all(c("type", "note") %in% names(notes))

  if (!readable) {

    why <- if (inherits(notes, "error")) condition_summary(notes) else "they have no type and note columns"

    return(text_records(paste0("tune's notes could not be read: ", why), pinned = TRUE))

  }

  notes <- notes[notes$type %in% type, , drop = FALSE]

  if (!is.null(candidate)) notes <- notes[notes_of(notes, candidate), , drop = FALSE]

  if (nrow(notes) == 0) return(new_warning_log())

  ## Parse each distinct note once, however many resamples raised it --------

  distinct <- unique(notes$note)
  idx      <- match(notes$note, distinct)
  parts    <- lapply(distinct, condition_parts)

  ## Which resample raised each note ------------------------------------------

  ### id, plus id2 for repeated CV. last_fit()'s notes carry no id.
  note_ids <- resample_ids(notes)

  folds <- if (is.null(note_ids)) {

    rep(list(character(0)), length(distinct))

  } else {

    lapply(split(note_ids, factor(idx, levels = seq_along(distinct))), unique)

  }

  n_folds <- length(unique(resample_ids(tune_results)))

  tibble::tibble(
    cause   = vapply(parts, `[[`, character(1), "cause"),
    where   = vapply(parts, `[[`, character(1), "where"),
    n       = tabulate(idx, nbins = length(distinct)),
    scope   = scope,
    folds   = unname(folds),
    n_folds = as.integer(n_folds),
    pinned  = FALSE
  )

}

#' The candidate a tune result's notes are read for
#'
#' @description
#' tune keeps the notes of every candidate it tuned, so without a filter a
#' configuration reports the warnings of hyperparameters it did not choose,
#' such as an `mtry` above the predictor count on a candidate that lost
#' (#109). A note belongs to the chosen candidate when:
#'
#' * its location has no model part (the recipe's prep, or its bake of the
#'   assessment rows), since the recipe is the same for every candidate;
#' * it is a grid note (`.iter` 0) at a model stage that holds the candidate.
#'   tune numbers the model stages of `tune::schedule_grid()` in the order
#'   of the grid it was given, which is why `evaluate_single_config()` draws
#'   the grid itself; one stage holds every candidate that differs only in a
#'   submodel parameter (glmnet's `penalty`, PLS's `num_comp`);
#' * it is a Bayesian note of the candidate's own iteration;
#' * it is tune's `internal` note (yardstick's, on a metric it could not
#'   compute) of the candidate's iteration, on a fold where the candidate has
#'   a metric missing.
#'
#' @param results The tune result the candidate was chosen from.
#' @param best_params One-row tibble of the chosen hyperparameters, with
#'   `.config`.
#' @param grid The grid `tune_grid()` was given.
#' @param workflow The workflow it tuned.
#' @return List: `iter` (the candidate's Bayesian iteration, 0 for a grid
#'   candidate), `model_stages` (the grid's model stages that hold it, or
#'   `NULL` for every stage), `na_folds` (the resamples where one of its
#'   metrics is missing) and `unmapped` (why the grid's schedule could not be
#'   read, so every grid note is kept; `NULL` when it was, or not needed).
#' @keywords internal
#' @noRd
candidate_of <- function(results, best_params, grid, workflow) {

  config   <- best_params$.config[[1]]
  per_fold <- tune::collect_metrics(results, summarize = FALSE)
  mine     <- per_fold[per_fold$.config == config, , drop = FALSE]
  iter     <- if (".iter" %in% names(mine)) as.integer(mine$.iter[1]) else 0L

  ## The model stages of the grid that hold it. The schedule is tune's
  ## developer interface, so a shape it no longer has keeps every grid note,
  ## with the reason, rather than failing the configuration: the filter is
  ## for reading the row, and the row is still right without it.
  stages   <- NULL
  unmapped <- NULL

  if (iter == 0L) {

    mapped <- tryCatch(grid_model_stages(grid, workflow, best_params),
                       error = function(e) structure(condition_summary(e), class = "unmapped"))

    if (inherits(mapped, "unmapped")) unmapped <- unclass(mapped) else stages <- mapped

  }

  list(
    iter         = iter,
    model_stages = stages,
    na_folds     = unique(resample_ids(mine[is.na(mine$.estimate), , drop = FALSE])),
    unmapped     = unmapped
  )

}

#' The row's line when a candidate's warnings could not be told apart
#'
#' @param candidate From [candidate_of()].
#' @return Character(1), or `NULL` when the grid's schedule was read and the
#'   row holds only the chosen candidate's warnings.
#' @keywords internal
#' @noRd
unmapped_line <- function(candidate) {

  if (is.null(candidate$unmapped)) return(NULL)

  paste0("The warnings of every grid candidate are shown, not only the chosen one's: ",
         candidate$unmapped, ".")

}

#' The model stages of a grid that hold a candidate
#'
#' @description
#' Matched on the parameters that are not submodels, since a stage carries a
#' submodel's largest value and covers every candidate that differs only in
#' it (glmnet's `penalty`, PLS's `num_comp`).
#'
#' @param grid The grid `tune_grid()` was given.
#' @param workflow The workflow it tuned.
#' @param best_params One-row tibble of the candidate's hyperparameters.
#' @return Integer: the stages, in the order tune numbers them; `NULL` when
#'   the recipe is tuned too (several preprocessor stages), for every stage.
#'   Errors when `tune::schedule_grid()` returns a shape it does not read.
#' @keywords internal
#' @noRd
grid_model_stages <- function(grid, workflow, best_params) {

  schedule <- tune::schedule_grid(grid, workflow)

  if (!is.data.frame(schedule) || !"model_stage" %in% names(schedule)) {
    stop("tune's grid schedule has no model_stage column", call. = FALSE)
  }

  if (nrow(schedule) != 1L) return(NULL)

  model_stage <- schedule$model_stage[[1]]

  if (!is.data.frame(model_stage) || !"predict_stage" %in% names(model_stage)) {
    stop("tune's grid schedule has no predict_stage column", call. = FALSE)
  }

  params  <- setdiff(names(model_stage),
                     c("predict_stage", names(model_stage$predict_stage[[1]])))
  missing <- setdiff(params, names(best_params))

  if (length(missing) > 0) {
    stop("tune's grid schedule has parameters the candidate lacks: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  holds <- rep(TRUE, nrow(model_stage))

  for (p in params) holds <- holds & model_stage[[p]] == best_params[[p]][[1]]

  which(holds)

}

#' Which notes belong to a candidate
#'
#' @param notes `tune::collect_notes()`'s table.
#' @param candidate From [candidate_of()].
#' @return Logical, one per note.
#' @keywords internal
#' @noRd
notes_of <- function(notes, candidate) {

  if (nrow(notes) == 0) return(logical(0))

  iter  <- if (".iter" %in% names(notes)) as.integer(notes$.iter) else rep(0L, nrow(notes))
  found <- regmatches(notes$location, regexec("model ([0-9]+)/[0-9]+", notes$location))
  stage <- vapply(found, function(x) if (length(x) == 2L) as.integer(x[2]) else NA_integer_,
                  integer(1))

  own_iter <- iter == candidate$iter
  internal <- notes$location %in% "internal"
  in_stage <- is.null(candidate$model_stages) | stage %in% candidate$model_stages

  ifelse(internal, own_iter & resample_ids(notes) %in% candidate$na_folds,
         ifelse(is.na(stage), TRUE,
                own_iter & (iter > 0L | in_stage)))

}

#' The resample each row of a tune table belongs to
#'
#' @param x A data frame with `id` (and, for repeated CV, `id2`) columns.
#' @return Character, one per row, or `NULL` when `x` has no id column.
#' @keywords internal
#' @noRd
resample_ids <- function(x) {

  id_cols <- grep("^id", names(x), value = TRUE)

  if (length(id_cols) == 0) return(NULL)

  do.call(paste, c(unname(as.list(x[id_cols])), sep = "/"))

}

#' Reduce warning records to one line per distinct message
#'
#' @description
#' Records are the same message when their text matches once numbers,
#' whitespace and bullet separators are set aside, and their location is the
#' same. That joins a warning captured in full from a fit tune did not run
#' with the note tune kept for the same warning, and the variants of a
#' message that carries a value ("40 samples were requested but there were
#' 20 rows", then 30 rows).
#'
#' Each line shows the variant seen most often, then in its parentheses the
#' location, how many cross-validation folds raised it, `"test set"` when the
#' final fit on the test set raised it, and `"numbers vary"` when the
#' variants differ in their values. A test-set signal is labelled so it can
#' never be read as a cross-validation one.
#'
#' @param records Records shaped like [new_warning_log()]'s.
#' @return Tibble with one row per distinct message, in order of first
#'   arrival: `line`, `n` (total arrivals) and `pinned`.
#' @keywords internal
#' @noRd
summarise_warning_records <- function(records) {

  if (nrow(records) == 0) {

    return(tibble::tibble(line = character(0), n = integer(0), pinned = logical(0)))

  }

  ## The grouping key and the text it is shown with ---------------------------

  ### ";" is set aside with whitespace because clean_message_text() joins a
  ### bullet with "; " or with a space depending on the text before it.
  spaced <- gsub("[[:space:];]+", " ", records$cause)
  where  <- ifelse(is.na(records$where), "", records$where)
  key    <- paste0(gsub("[0-9]+", "#", spaced), "\r", where)
  keys   <- unique(key)
  groups <- split(seq_len(nrow(records)), factor(key, levels = keys))

  lines <- vapply(groups, function(i) {

    n     <- records$n[i]
    texts <- spaced[i]

    ## The variant seen most often; among equals, the fullest -----------------

    totals <- vapply(unique(texts), function(t) sum(n[texts == t]), numeric(1))
    best   <- names(totals)[totals == max(totals)]
    shown  <- i[texts %in% best]
    shown  <- shown[which.max(nchar(records$cause[shown]))]

    ## Where it came from --------------------------------------------------------

    cv      <- i[records$scope[i] %in% "cv"]
    cv_ids  <- unique(unlist(records$folds[cv]))
    ### No cross-validation record, or none that knows its fold count, has no
    ### fold count to show.
    known   <- records$n_folds[cv][!is.na(records$n_folds[cv])]
    n_folds <- if (length(known) > 0) max(known) else NA_integer_

    detail <- c(
      if (length(cv_ids) > 0 && is.finite(n_folds) && n_folds > 1) {

        paste0(length(cv_ids), " of ", n_folds, " folds")

      },
      if (any(records$scope[i] %in% "test set")) "test set",
      if (length(totals) > 1) "numbers vary"
    )

    format_condition(records$cause[shown], records$where[shown], detail)

  }, character(1), USE.NAMES = FALSE)

  tibble::tibble(
    line   = lines,
    n      = vapply(groups, function(i) sum(records$n[i]), integer(1), USE.NAMES = FALSE),
    pinned = vapply(groups, function(i) any(records$pinned[i]), logical(1), USE.NAMES = FALSE)
  )

}

#' The warnings a config reports
#'
#' @description
#' One line per distinct message: the package's own notices first, then the
#' `max_shown` messages that arrived most often, then one line counting the
#' rest. A message carrying a value that differs per fold or per grid point
#' would otherwise fill the list, and a config's warnings are read in a
#' console tree and a results table.
#'
#' @param records Records shaped like [new_warning_log()]'s.
#' @param max_shown Integer. Distinct messages to show besides the notices.
#' @return Character, or `NULL` when there are no warnings.
#' @keywords internal
#' @noRd
render_warning_log <- function(records, max_shown = 5L) {

  lines <- summarise_warning_records(records)

  if (nrow(lines) == 0) return(NULL)

  notices <- lines$line[lines$pinned]
  rest    <- lines[!lines$pinned, , drop = FALSE]
  rest    <- rest$line[order(-rest$n, seq_len(nrow(rest)))]
  n_more  <- length(rest) - max_shown

  c(
    notices,
    utils::head(rest, max_shown),
    if (n_more > 0) {

      paste0(n_more, " more distinct warning", if (n_more > 1) "s", " not shown.")

    }
  )

}

## ---------------------------------------------------------------------------
## tune_note_messages
## ---------------------------------------------------------------------------

#' Distinct notes of one type from a tune result, as readable lines
#'
#' @description
#' tune catches the errors and warnings raised inside `tune_grid()`,
#' `tune_bayes()`, `fit_resamples()` and `last_fit()` and stores them in the
#' result's `.notes` column instead of signalling them, so a capture site
#' that only watches for conditions never sees them. A recipe step that fails
#' or warns during prep inside tuning leaves its only trace there. This
#' reads the notes back through [condition_summary()] and returns one line
#' per distinct message (see [summarise_warning_records()]), most frequent
#' first, each saying how many folds raised it (`"3 of 5 folds"`) when the
#' result has more than one.
#'
#' tune 2.1.0 stores a warning note as the warning's raw `$message` field,
#' not its rendered message. A warning whose own message is empty, with the
#' text in a parent or body (an rlang wrapper around another warning),
#' arrives as `""` and reads as `"no message"`. That text is lost before
#' horizons sees it and cannot be recovered here. Error notes are stored
#' rendered and keep their text.
#'
#' @param tune_results A `tune_results` object (or anything
#'   `tune::collect_notes()` reads).
#' @param type Character. Note types to keep: `"error"`, `"warning"`, or both.
#'
#' @return Character vector, empty when there are no notes of that type or
#'   the notes cannot be read.
#' @keywords internal
#' @noRd
tune_note_messages <- function(tune_results, type = "warning") {

  lines <- summarise_warning_records(tune_note_records(tune_results, type))

  lines$line[order(-lines$n, seq_len(nrow(lines)))]

}

#' The cause to append when every model in a tune result failed
#'
#' @param tune_results A `tune_results` object whose models all failed.
#' @return Character(1): the most frequent distinct error note, plus a count
#'   of the other distinct errors when there are any; `NULL` when the result
#'   carries no error notes.
#' @keywords internal
#' @noRd
tune_failure_cause <- function(tune_results) {

  causes <- tune_note_messages(tune_results, type = "error")

  if (length(causes) == 0) return(NULL)

  n_other <- length(causes) - 1L

  if (n_other == 0) return(causes[1])

  paste0(causes[1], ", plus ", n_other, " other distinct error",
         if (n_other > 1) "s")

}

#' Fail a unit on what a tune call kept as text
#'
#' @description
#' tune catches every error raised inside its resampling and returns, so a
#' failure there reaches the unit only as notes and an unclassed warning. Run
#' after each tune call, inside the call's step:
#'
#' * A recipe step that failed inside tune (an error note at a preprocessor
#'   location) is reproduced outside it, when `recipe` is given, by prepping
#'   the recipe once on the training part: the step's error then ends the
#'   unit with its own class, or propagates for the kinds no unit contains. A
#'   failure that does not reproduce there (one fold's rows only) is read
#'   from the notes, as below. Prepping only on failure costs a run that
#'   does not fail nothing, where a prep with feature selection can take
#'   minutes.
#' * A result in which every model failed ([tune::.is_cataclysmic()]) ends the
#'   unit: `failure` and the most frequent error note are its message, and it
#'   carries the trace tune kept for the first error (tune keeps one for an
#'   engine's error and none for a recipe step's).
#' * Under `debug` any error note ends the unit the same way, so the first
#'   failure inside tune stops the call.
#'
#' @param results A tune result.
#' @param failure Character(1). What a result in which every model failed
#'   means, e.g. `"all models failed during CV"`; `NULL` when the caller falls
#'   back instead (the Bayesian search).
#' @param recipe The unit's untrained recipe, to reproduce a step's failure;
#'   `NULL` when it was prepped before tuning.
#' @param debug Logical(1).
#' @param call The frame the failure reports.
#' @return `NULL`, invisibly; aborts with class `horizons_error_run`.
#' @keywords internal
#' @noRd
check_tune_result <- function(results,
                              failure,
                              recipe = NULL,
                              debug  = FALSE,
                              call   = rlang::caller_env()) {

  notes  <- tune::collect_notes(results)
  errors <- notes[notes$type %in% "error", , drop = FALSE]

  ## A recipe step that failed inside tune, reproduced outside it --------------

  ### A preprocessor location without a model part is the recipe's prep, or
  ### its bake of the assessment rows ("(prediction data)").
  step_failed <- any(grepl("^preprocessor [0-9]+/[0-9]+( \\(prediction data\\))?$",
                           errors[["location"]]))

  if (!is.null(recipe) && step_failed) unit_step("prep", recipes::prep(recipe))

  ## Every model failed, or, under debug, any did ---------------------------------

  every_model <- !is.null(failure) && tune::.is_cataclysmic(results)

  if (!every_model && !(debug && nrow(errors) > 0)) return(invisible(NULL))

  abort_tune_failure(results, if (every_model) failure else "a fit failed inside tune",
                     call = call)

}

#' Abort a unit on a failure tune kept as text
#'
#' @param results A tune result.
#' @param what Character(1). What failed, e.g. `"all models failed during
#'   CV"`.
#' @param call The frame the failure reports.
#' @return Does not return; aborts with class `horizons_error_run`, the most
#'   frequent error note as the cause, and the trace tune kept for the first
#'   error note that has one.
#' @keywords internal
#' @noRd
abort_tune_failure <- function(results, what, call = rlang::caller_env()) {

  notes  <- tune::collect_notes(results)
  errors <- notes[notes$type %in% "error", , drop = FALSE]
  cause  <- tune_failure_cause(results)

  abort_horizons(
    if (is.null(cause)) "{what}" else "{what} \u2014 {cause}",
    kind  = "run",
    trace = Find(Negate(is.null), errors[["trace"]]),
    call  = call
  )

}

## ---------------------------------------------------------------------------
## The fold rule (#101): only candidates scored on every fold are compared
## ---------------------------------------------------------------------------
## tune averages a candidate's metric over the folds it was scored on, so a
## candidate that failed on one fold is compared with the others on the folds
## that survived, which is a different and easier test. A candidate missing a
## fold is left out instead, as scikit-learn leaves out one whose mean is NaN.
## A recipe step that fails on a fold fails every candidate there, so its
## configuration then has none to choose from and fails.

#' A metric's mean and standard error over the folds
#'
#' @description
#' One row per group of per-fold estimates: the mean over the folds with a
#' value, its standard error (`sd / sqrt(n)`, as tune's), and `n`, the
#' distinct resamples with a value. Both verbs summarise their folds through
#' it: `evaluate()` a tune result's ([config_metrics()]), `fit()` a member's
#' out-of-fold predictions scored fold by fold.
#'
#' @param per_fold Data frame with `.metric`, `.estimate` and the resample's
#'   `id` (and `id2` for repeated folds), one row per fold and metric.
#' @param by Character. The columns a group is keyed by.
#' @return Tibble with the `by` columns, `mean` (`NA` when no fold has a
#'   value), `std_err` (`NA` below two) and `n`.
#' @keywords internal
#' @noRd
summarise_folds <- function(per_fold, by) {

  resample <- resample_ids(per_fold)
  scored   <- !is.na(per_fold$.estimate)
  groups   <- split(seq_len(nrow(per_fold)), unname(as.list(per_fold[by])), drop = TRUE)

  out <- lapply(by, function(col) {
    vapply(groups, function(i) as.character(per_fold[[col]][i[1]]), character(1), USE.NAMES = FALSE)
  })
  names(out) <- by

  estimates <- lapply(groups, function(i) per_fold$.estimate[i][scored[i]])

  tibble::as_tibble(c(out, list(
    mean    = vapply(estimates, function(e) if (length(e) > 0) mean(e) else NA_real_,
                     numeric(1), USE.NAMES = FALSE),
    std_err = vapply(estimates, function(e) {
      if (length(e) > 1) stats::sd(e) / sqrt(length(e)) else NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    n       = vapply(groups, function(i) length(unique(resample[i][scored[i]])), integer(1),
                     USE.NAMES = FALSE)
  )))

}

#' Each candidate's metrics over its own folds
#'
#' @description
#' `tune::collect_metrics()` pools the folds of candidates with equal
#' hyperparameters, so a Bayesian iteration that proposes a grid point again
#' reports both on twice the folds. Read from the per-fold metrics instead
#' ([summarise_folds()]), each `.config` is counted on the resamples it was
#' scored on, and its mean is its own.
#'
#' @param results A tune result (grid or Bayesian).
#' @return Tibble with one row per `.config` and metric: `.config`,
#'   `.metric`, `mean` (over the folds with a value; `NA` when none has one),
#'   `std_err` and `n` (the distinct resamples with a value).
#' @keywords internal
#' @noRd
config_metrics <- function(results) {

  summarise_folds(tune::collect_metrics(results, summarize = FALSE), c(".config", ".metric"))

}

#' Keep a cross-validated mean only where it covers every fold
#'
#' @description
#' The rule both verbs rank and report by: a mean over fewer folds than were
#' drawn (a correlation undefined on a fold whose predictions were constant,
#' say) would compare unlike quantities, so it is `NA`, and so is its
#' standard error.
#'
#' @param summary From [summarise_folds()].
#' @param n_folds Integer(1). The folds drawn.
#' @return `summary`, with `mean` and `std_err` `NA` where `n` is not
#'   `n_folds`.
#' @keywords internal
#' @noRd
over_every_fold <- function(summary, n_folds) {

  short <- is.na(summary$n) | summary$n != as.integer(n_folds)

  summary$mean[short]    <- NA_real_
  summary$std_err[short] <- NA_real_

  summary

}

#' The row's line on cross-validated metrics left NA
#'
#' @param metrics Character. The metrics whose cross-validated mean is `NA`.
#' @param n_metrics Integer(1). How many there are in all.
#' @return Character(1), or `NULL` when none is `NA`.
#' @keywords internal
#' @noRd
cv_na_line <- function(metrics, n_metrics) {

  if (length(metrics) == 0) return(NULL)

  if (length(metrics) == n_metrics) {
    return("CV metrics at the selected hyperparameters could not be recovered; cv_* columns are NA.")
  }

  one   <- length(metrics) == 1L
  named <- if (one) metrics else {
    paste0(paste(utils::head(metrics, -1), collapse = ", "), " and ", utils::tail(metrics, 1))
  }

  paste0("Cross-validated ", named, if (one) " is" else " are",
         " NA: ", if (one) "it" else "they", " could not be computed on every fold, and a",
         " cross-validated mean is kept only over every fold.")

}

#' The candidates of a tune result scored on every fold
#'
#' @param results A tune result (grid or Bayesian).
#' @param n_folds Integer(1). The folds drawn.
#' @return Character: their `.config`s. Counted on `rmse`, which is `NA` on a
#'   fold only when the fit made no predictions there; a metric undefined on
#'   a fold whose predictions were constant (`rsq`) is not a failure.
#' @keywords internal
#' @noRd
complete_configs <- function(results, n_folds) {

  panel <- config_metrics(results)

  panel$.config[panel$.metric == "rmse" & panel$n == n_folds]

}

#' The candidates scored on every fold, best first
#'
#' @description
#' tune's own order and tie rule, as `tune::select_best()` applies them
#' (`tune::show_best()` with every candidate), restricted to the candidates
#' scored on every fold. Under `debug` a candidate missing a fold stops the
#' call rather than being left out, since leaving it out is containment.
#'
#' @param results A tune result (grid or Bayesian).
#' @param metric Character(1). The metric to order by.
#' @param n_folds Integer(1). The folds drawn.
#' @param debug Logical(1).
#' @return `tune::show_best()`'s rows, possibly none.
#' @keywords internal
#' @noRd
rank_complete <- function(results, metric, n_folds, debug = FALSE) {

  ranked   <- tune::show_best(results, metric = metric, n = Inf)
  complete <- ranked$.config %in% complete_configs(results, n_folds)

  if (debug && !all(complete)) {

    abort_tune_failure(results, "a candidate was not scored on every fold")

  }

  ranked[complete, , drop = FALSE]

}

#' The best hyperparameters among the candidates scored on every fold
#'
#' @description
#' What `tune::select_best()` returns, chosen by [rank_complete()]: the
#' hyperparameters and `.config` of the first candidate.
#'
#' @inheritParams rank_complete
#' @return One-row tibble, or zero rows when no candidate was scored on every
#'   fold.
#' @keywords internal
#' @noRd
select_complete <- function(results, metric, n_folds, debug = FALSE) {

  ranked <- rank_complete(results, metric, n_folds, debug)

  utils::head(ranked[, c(tune::.get_tune_parameter_names(results), ".config"), drop = FALSE], 1)

}

#' The row's note on candidates left out for a missing fold
#'
#' @param results The tune result the hyperparameters were chosen from.
#' @param n_folds Integer(1). The folds drawn.
#' @param n_dropped Integer(1). Candidates the Bayesian search proposed and
#'   tune dropped, scored on no fold, which `results` does not hold.
#' @return Character(1), or `NULL` when every candidate was scored on every
#'   fold.
#' @keywords internal
#' @noRd
incomplete_note <- function(results, n_folds, n_dropped = 0L) {

  all   <- unique(config_metrics(results)$.config)
  n_out <- length(setdiff(all, complete_configs(results, n_folds))) + n_dropped
  n_all <- length(all) + n_dropped

  if (n_out == 0) return(NULL)

  cause <- tune_failure_cause(results)

  paste0(n_out, " of ", n_all, " candidate", if (n_all != 1) "s",
         if (n_out == 1) " was" else " were",
         " left out of the choice of hyperparameters, not being scored on every fold",
         if (!is.null(cause)) paste0(": ", cause), ".")

}

## ---------------------------------------------------------------------------
## check_rows_aligned
## ---------------------------------------------------------------------------

#' Assert that values attached by position line up with their rows
#'
#' @description
#' Several steps attach values to samples by position: point predictions,
#' interval bounds and applicability-domain columns to the rows of
#' `new_data`, calibration predictions to their truth, provenance columns to
#' the rows `select_training()` drew. Position is only right while every step
#' in between keeps the rows' count and order, and nothing promises that: a
#' recipe step that filters rows at bake drops rows from `predict()` and
#' `bake()` alike, without an error. This checks the invariant just before
#' the values are attached, so a change upstream fails loudly instead of
#' labelling samples with another sample's values.
#'
#' Called with `n` and `n_expected`, it checks the count. Called with `ids`
#' and `expected_ids` (`sample_id`s), it checks the count and the order. When
#' the rows line up it does nothing; otherwise it aborts through
#' [abort_misaligned()].
#'
#' @param what Character(1). The values being attached, for the message,
#'   e.g. `"Interval columns for config 'cfg_001'"`.
#' @param to Character(1). The rows they are attached to, e.g.
#'   `"the rows of new_data"`.
#' @param n Integer(1). Rows (or length) of the values. Defaults to
#'   `length(ids)`.
#' @param n_expected Integer(1). Number of rows they are attached to.
#'   Defaults to `length(expected_ids)`.
#' @param ids,expected_ids Character or `NULL`. Row keys of the values and of
#'   the rows. When both are given they must agree position by position.
#' @param call The frame the abort reports, the caller's by default.
#'
#' @return Invisibly `NULL`. Aborts with class `horizons_error_internal`.
#' @keywords internal
#' @noRd
check_rows_aligned <- function(what,
                               to,
                               n            = length(ids),
                               n_expected   = length(expected_ids),
                               ids          = NULL,
                               expected_ids = NULL,
                               call         = rlang::caller_env()) {

  ## Count -----------------------------------------------------------------

  if (!identical(as.integer(n), as.integer(n_expected))) {

    abort_misaligned(what, to,
                     cli::format_inline("Got {n} row{?s} for {n_expected}."),
                     call = call)

  }

  ## Order, when both sides carry a key --------------------------------------

  if (is.null(ids) || is.null(expected_ids)) return(invisible(NULL))

  ids          <- as.character(ids)
  expected_ids <- as.character(expected_ids)

  if (identical(ids, expected_ids)) return(invisible(NULL))

  differs <- which(is.na(ids) != is.na(expected_ids) |
                   (!is.na(ids) & !is.na(expected_ids) & ids != expected_ids))
  first   <- differs[1]

  abort_misaligned(what, to, cli::format_inline(
    "The {.field sample_id}s differ at {length(differs)} of {n} position{?s}; the first is row {first}, {.val {ids[first]}} where {.val {expected_ids[first]}} was expected."
  ), call = call)

}

#' Abort because values attached by position do not line up with their rows
#'
#' @description
#' The one abort behind [check_rows_aligned()] and the out-of-fold row check
#' in [fit_uq()]. A misalignment is a broken internal invariant, a bug in
#' horizons rather than something the user did, so it is an `"internal"`
#' error, whose footer asks for a report.
#'
#' @param what,to Character(1). The values, and the rows they are attached
#'   to, as in [check_rows_aligned()].
#' @param detail Character(1). What does not line up, already formatted
#'   (with [cli::format_inline()]); it is interpolated as a value.
#' @param call The frame the abort reports.
#'
#' @return Does not return.
#' @keywords internal
#' @noRd
abort_misaligned <- function(what, to, detail, call = rlang::caller_env()) {

  abort_horizons(c(
    "{what} do not line up with {to}.",
    "x" = "{detail}",
    "i" = "Values are attached by position here, so a step upstream dropped, added or reordered rows."
  ), kind = "internal", call = call)

}
