#' Safely Evaluate an Expression with Optional Logging and Condition Capture
#'
#' Evaluates an expression using `purrr::safely()` to prevent errors from halting execution.
#' If an error occurs, it returns a default value, optionally logs a custom message,
#' and can capture the call trace for debugging. Can also capture warnings and messages.
#'
#' @param expr An expression to evaluate, passed unquoted.
#' @param default_value Value to return if the expression errors. Default is `NULL`.
#' @param error_message Optional string interpolated via `glue::glue()`, used as the log message.
#' @param log_error Logical. If `TRUE` (default), logs the error using `cli::cli_warn()`.
#' @param capture_trace Logical. If `TRUE`, captures a traceback using `rlang::trace_back()`.
#' @param trace_log_file Optional file path to write the trace, if `capture_trace = TRUE`.
#' @param capture_conditions Logical. If `TRUE`, captures warnings and messages. Default is `FALSE`.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`result`}{The evaluated result or `default_value` if an error occurred.}
#'   \item{`error`}{The error object, or `NULL` if the evaluation succeeded.}
#'   \item{`trace`}{A trace object from `rlang`, or `NULL` if not captured.}
#'   \item{`warnings`}{List of warning conditions if `capture_conditions = TRUE`.}
#'   \item{`messages`}{List of message conditions if `capture_conditions = TRUE`.}
#'   \item{`n_warnings`}{Count of warnings captured.}
#'   \item{`n_messages`}{Count of messages captured.}
#' }
#'
#' @details
#' This function is useful when running potentially fragile code inside mapping,
#' parallelization, or ensemble modeling workflows. By capturing and optionally logging
#' errors without interrupting the overall workflow, it supports robust batch execution.
#'
#' If `capture_trace = TRUE`, the call stack is stored and optionally written to disk.
#' This is helpful when debugging complex pipelines.
#'
#' @seealso \code{\link[purrr]{safely}}, \code{\link[rlang]{trace_back}}, \code{\link[cli]{cli_warn}}
#'
#' @examples
#' \dontrun{
#' safely_execute(log("oops"), default_value = NA, error_message = "Failed to take log")
#' }
#'
#' @importFrom rlang enquo caller_env eval_tidy trace_back last_trace %||%
#' @importFrom purrr safely
#' @importFrom cli cli_alert_warning cli_alert_info
#' @importFrom glue glue
#'
#' @export


safely_execute <- function(expr,
                           default_value      = NULL,
                           error_message      = NULL,
                           log_error          = TRUE,
                           capture_trace      = FALSE,
                           trace_log_file     = NULL,
                           capture_conditions = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Capture the incoming code block.
  ## ---------------------------------------------------------------------------

  expr_quo <- rlang::enquo(expr)
  caller_env <- rlang::caller_env()

  ## ---------------------------------------------------------------------------
  ## Step 2: Create storage for conditions if requested.
  ## ---------------------------------------------------------------------------

  error_trace <- NULL
  captured_warnings <- list()
  captured_messages <- list()

  ## ---------------------------------------------------------------------------
  ## Step 3: Create a safely-wrapped function to evaluate the code block.
  ## ---------------------------------------------------------------------------

  safe_eval <- purrr::safely(function() {

    if (capture_conditions) {
      # Capture warnings and messages in addition to errors
      withCallingHandlers(
        rlang::eval_tidy(expr_quo, env = caller_env),
        warning = function(w) {
          captured_warnings <<- append(captured_warnings, list(conditionMessage(w)))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          captured_messages <<- append(captured_messages, list(conditionMessage(m)))
          invokeRestart("muffleMessage")
        },
        error = function(cnd) {
          # Capture trace at the moment of error
          error_trace <<- rlang::trace_back()
          # Let the original error propagate - safely() will catch it
        }
      )
    } else {
      # Original behavior - just capture errors
      withCallingHandlers(
        rlang::eval_tidy(expr_quo, env = caller_env),
        error = function(cnd) {
          # Capture trace at the moment of error
          error_trace <<- rlang::trace_back()
          # Let the original error propagate - safely() will catch it
        }
      )
    }

  }, otherwise = default_value, quiet = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 3: Run wrapper and store results.
  ## ---------------------------------------------------------------------------

  result_list <- safe_eval()

  ## ---------------------------------------------------------------------------
  ## Step 4: Handle trace if error occurred.
  ## ---------------------------------------------------------------------------

  trace <- NULL

  if (!is.null(result_list$error)) {
    if (capture_trace) {
      # Use the trace we captured at error time, or extract from error object
      trace <- error_trace %||% result_list$error$trace

      ## -----------------------------------------------------------------------
      ## Step 4.1: Conditionally write traceback to disk
      ## -----------------------------------------------------------------------

      if (!is.null(trace_log_file) && !is.null(trace)) {
        try({
          cat(capture.output(print(trace)),
              file  = trace_log_file,
              sep    = "\n",
              append = TRUE)
        }, silent = TRUE)
      }
    }

    ## -------------------------------------------------------------------------
    ## Step 4.2: Conditionally log error via cli::cli_warn() using interpolated message
    ## -------------------------------------------------------------------------

    if (log_error) {

      msg <- if (!is.null(error_message)) {
        tryCatch(
          glue::glue(error_message, .envir = caller_env),
          error = function(e) paste0(error_message, " (error in message: ", e$message, ")")
        )
      } else {
        "An error occurred"
      }

      cli::cli_alert_warning("{msg}: {result_list$error$message}")
      if (!is.null(trace)) {
        cli::cli_alert_info("Run `rlang::last_error()` for full backtrace")
      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return result, error, trace, and captured conditions.
  ## ---------------------------------------------------------------------------

  return(list(result      = result_list$result,
              error       = result_list$error,
              trace       = trace,
              warnings    = captured_warnings,
              messages    = captured_messages,
              n_warnings  = length(captured_warnings),
              n_messages  = length(captured_messages)))
}

#' Safely Execute with Condition Capture
#'
#' @description
#' Convenience wrapper for safely_execute that always captures warnings and messages.
#' Useful for debugging and quality control where you need to track all conditions,
#' not just errors.
#'
#' @inheritParams safely_execute
#'
#' @return Same as safely_execute but with capture_conditions always TRUE
#'
#' @export
safely_execute_with_conditions <- function(expr,
                                          default_value  = NULL,
                                          error_message  = NULL,
                                          log_error      = TRUE,
                                          capture_trace  = FALSE,
                                          trace_log_file = NULL) {

  safely_execute(expr            = expr,
                default_value   = default_value,
                error_message   = error_message,
                log_error       = log_error,
                capture_trace   = capture_trace,
                trace_log_file  = trace_log_file,
                capture_conditions = TRUE)
}

#' Handle results from safely_execute with detailed error reporting
#'
#' @description
#' Processes the output from safely_execute, providing detailed error reporting
#' when operations fail. Extracts and formats errors, warnings, and messages
#' for clear user feedback.
#'
#' @param safe_result List from safely_execute containing result, error, warnings, messages
#' @param error_title Character. Main error message to display
#' @param error_hints Character vector. Troubleshooting suggestions (optional)
#' @param abort_on_null Logical. Whether to abort if result is NULL (default: TRUE)
#' @param silent Logical. Suppress non-error output (default: FALSE)
#'
#' @return The result from safe_result, or NULL if abort_on_null = FALSE
#'
#' @examples
#' \dontrun{
#' safe_result <- safely_execute(
#'   expr = {some_risky_operation()},
#'   default_value = NULL
#' )
#'
#' result <- handle_safe_result(
#'   safe_result,
#'   error_title = "Operation failed",
#'   error_hints = c(
#'     "Check input data format",
#'     "Ensure all dependencies are loaded"
#'   )
#' )
#' }
#'
#' @export
handle_results <- function(safe_result,
                           error_title    = "Operation failed",
                           error_hints    = NULL,
                           abort_on_null  = TRUE,
                           silent         = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Validate input structure
  ## ---------------------------------------------------------------------------

  if (!is.list(safe_result) || !all(c("result", "error") %in% names(safe_result))) {

    cli::cli_abort("Invalid safe_result structure. Expected output from safely_execute()")

  }

  result <- safe_result$result

  ## ---------------------------------------------------------------------------
  ## Success case - return result with optional warnings/messages
  ## ---------------------------------------------------------------------------

  if (!is.null(result)) {

    # Show any warnings/messages even on success (unless silent)
    if (!silent) {

      if (!is.null(safe_result$warnings) && length(safe_result$warnings) > 0) {

        for (w in safe_result$warnings) {
          cli::cli_alert_warning(w)
        }

      }

      if (!is.null(safe_result$messages) && length(safe_result$messages) > 0) {

        for (m in safe_result$messages) {
          cli::cli_alert_info(m)
        }

      }
    }

    return(result)
  }

  ## ---------------------------------------------------------------------------
  ## Handle NULL result - either return NULL or abort
  ## ---------------------------------------------------------------------------

  if (!abort_on_null) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Build comprehensive error message for cli_abort
  ## ---------------------------------------------------------------------------

  error_components <- list()

  # Extract main error if present
  if (!is.null(safe_result$error)) {

    error_components$error <- paste("Error:", safe_result$error$message)

  }

  # Extract warnings if present
  if (!is.null(safe_result$warnings) && length(safe_result$warnings) > 0) {

    error_components$warnings <- paste("Warning:", safe_result$warnings)

  }

  # Extract messages if present
  if (!is.null(safe_result$messages) && length(safe_result$messages) > 0) {

    error_components$messages <- paste("Info:", safe_result$messages)

  }

  ## ---------------------------------------------------------------------------
  ## Construct cli_abort message with proper formatting
  ## ---------------------------------------------------------------------------

  abort_msg <- error_title

  # Add hints with proper naming for cli formatting
  if (!is.null(error_hints) && length(error_hints) > 0) {

    abort_msg <- c(abort_msg, setNames(error_hints, rep("i", length(error_hints))))

  }

  # Add error details if present
  if (length(error_components) > 0) {

    # Add spacing for readability
    abort_msg <- c(abort_msg, " ")

    # Add error with x bullet
    if (!is.null(error_components$error)) {

      abort_msg <- c(abort_msg, setNames(error_components$error, "x"))

    }

    # Add warnings with ! bullet
    if (!is.null(error_components$warnings)) {

      abort_msg <- c(abort_msg,
                     setNames(error_components$warnings,
                             rep("!", length(error_components$warnings))))

    }

    # Add messages with * bullet
    if (!is.null(error_components$messages)) {

      abort_msg <- c(abort_msg,
                     setNames(error_components$messages,
                             rep("*", length(error_components$messages))))

    }
  }

  cli::cli_abort(abort_msg)
}
