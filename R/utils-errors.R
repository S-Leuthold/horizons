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
#' @export
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

    warning(paste0(msg, ": ", result_list$error$message), call. = FALSE)

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
#' @export
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

    abort_msg <- c(abort_msg, x = safe_result$error$message)

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
#' @return Single-row tibble with config_id, status, error_message, metric
#'   columns (all NA), and runtime_secs (NA).
#' @keywords internal
#' @export
create_failed_result <- function(config_id, error = NULL) {

  ## Extract error message --------------------------------------------------

  error_msg <- if (is.null(error)) {

    NA_character_

  } else if (inherits(error, "condition")) {

    error$message

  } else if (is.character(error)) {

    error

  } else {

    as.character(error)

  }

  ## Build result row -------------------------------------------------------

  tibble::tibble(
    config_id     = config_id,
    status        = "failed",
    rmse          = NA_real_,
    rrmse         = NA_real_,
    rsq           = NA_real_,
    ccc           = NA_real_,
    rpd           = NA_real_,
    mae           = NA_real_,
    best_params   = list(NULL),
    error_message = error_msg,
    warnings      = list(NULL),
    runtime_secs  = NA_real_
  )

}
