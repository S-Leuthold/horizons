#' Safely Execute an Expression and Capture Errors
#'
#' This function wraps an expression using `purrr::safely()` to ensure errors
#' do not halt execution. It captures error messages, optionally logs them,
#' and can attach trace information for debugging.
#'
#' @param expr An R expression to evaluate.
#' @param default_value Value to return as `result` if an error occurs (default: `NULL`).
#' @param error_message Optional character string. If provided, it will be evaluated
#'        using `glue::glue()` and shown as part of the warning.
#' @param log_error Logical. Should errors be logged with `cli::cli_warn()`? (default: `TRUE`)
#' @param capture_trace Logical. If `TRUE`, captures the call trace using `rlang`.
#' @param trace_log_file Optional path to write the trace if `capture_trace = TRUE`.
#'
#' @return A list with elements:
#'   - `result`: the evaluated result or `default_value` on error
#'   - `error`: the error object (or `NULL` if none)
#'   - `trace`: call trace if `capture_trace = TRUE` and an error occurred
#'
#' @keywords internal

safely_execute <- function(expr,
                           default_value   = NULL,
                           error_message   = NULL,
                           log_error       = TRUE,
                           capture_trace   = FALSE,
                           trace_log_file  = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Capture the incoming code block.
  ## ---------------------------------------------------------------------------

  expr_quo <- rlang::enquo(expr)

  ## ---------------------------------------------------------------------------
  ## Step 2: Create a safely-wrapped function to evaluate the code block.
  ## ---------------------------------------------------------------------------

  safe_eval <- purrr::safely(function() {

    rlang::eval_tidy(expr_quo, env = rlang::caller_env())

    }, otherwise = default_value, quiet = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 3: Run wrapper and store results.
  ## ---------------------------------------------------------------------------

  result_list <- safe_eval()

  ## ---------------------------------------------------------------------------
  ## Step 4: Conditionally trace error back upstream.
  ## ---------------------------------------------------------------------------

  trace <- NULL

  if (!is.null(result_list$error)) {
    if (capture_trace) {
      trace <- tryCatch(
        if (exists("last_trace", asNamespace("rlang"), inherits = FALSE)) {
          rlang::last_trace()
          } else {
            rlang::trace_back()
          },
        error = function(e) NULL
      )

      ## -----------------------------------------------------------------------
      ## Step 4.1: Conditionally write traceback to disk
      ## -----------------------------------------------------------------------

      if (!is.null(trace_log_file)) {
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
          glue::glue(error_message, .envir = rlang::caller_env()),
          error = function(e) paste0(error_message, " (error in message: ", e$message, ")")
        )
      } else {
        "An error occurred"
      }

      cli::cli_warn("{msg}: {result_list$error$message}")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return result, error, and trace.
  ## ---------------------------------------------------------------------------

  return(list(result = result_list$result,
              error  = result_list$error,
              trace  = trace))
}
