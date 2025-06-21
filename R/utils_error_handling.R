#' Safely Evaluate an Expression with Optional Logging and Tracing
#'
#' Evaluates an expression using `purrr::safely()` to prevent errors from halting execution.
#' If an error occurs, it returns a default value, optionally logs a custom message,
#' and can capture the call trace for debugging.
#'
#' @param expr An expression to evaluate, passed unquoted.
#' @param default_value Value to return if the expression errors. Default is `NULL`.
#' @param error_message Optional string interpolated via `glue::glue()`, used as the log message.
#' @param log_error Logical. If `TRUE` (default), logs the error using `cli::cli_warn()`.
#' @param capture_trace Logical. If `TRUE`, captures a traceback using `rlang::trace_back()`.
#' @param trace_log_file Optional file path to write the trace, if `capture_trace = TRUE`.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`result`}{The evaluated result or `default_value` if an error occurred.}
#'   \item{`error`}{The error object, or `NULL` if the evaluation succeeded.}
#'   \item{`trace`}{A trace object from `rlang`, or `NULL` if not captured.}
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
#' @importFrom rlang enquo caller_env eval_tidy trace_back last_trace
#' @importFrom purrr safely
#' @importFrom cli cli_warn
#' @importFrom glue glue
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
