#' Safely Execute an Expression and Capture Errors
#'
#' This helper function wraps an expression or function call using `purrr::safely()`
#' to prevent errors from stopping execution. It optionally logs the error
#' and returns a specified default value on failure.
#'
#' @param expr An R expression or function call to execute safely.
#' @param default_value The value to return if `expr` results in an error (default: `NULL`).
#' @param error_message A character string. If provided, this message will be used
#'                      in a `cli::cli_warn` call if an error occurs. You can
#'                      embed variables from the calling environment using glue-style
#'                      syntax (e.g., `{my_var}`).
#' @param log_error Logical. If `TRUE`, an error message will be logged using `cli::cli_warn()`
#'                  if an error occurs (default: `TRUE`).
#' @param call The call to evaluate. Passed to `purrr::safely` for more informative
#'             error messages.
#' @param return_result_list Logical. If `TRUE`, the function returns a list
#'                           containing both the result and any caught error
#'                           (default is controlled by the option
#'                           `horizons.return_safely_result`, which is `FALSE`).
#' @param capture_trace Logical. If `TRUE`, capture the call stack for any error
#'                      using `rlang::last_trace()` (or `rlang::trace_back()` if
#'                      unavailable). The trace is attached to the error object
#'                      and returned when `return_result_list` is `TRUE`.
#' @param trace_log_file Optional file path. If provided and `capture_trace` is
#'                      `TRUE`, the captured trace will be appended to this file
#'                      for later inspection.
#'
#' @return If `return_result_list` is `FALSE` (default), the result of `expr` if
#'         successful, or `default_value` if an error occurs. If
#'         `return_result_list` is `TRUE`, a list with components `result`,
#'         `error`, and optionally `trace` (when `capture_trace` is `TRUE`) is
#'         returned.
#'
#' @importFrom purrr safely
#' @importFrom cli cli_warn
#' @importFrom rlang enquos eval_tidy
#'
#' @keywords internal


safely_execute <- function(expr,
                           default_value       = NULL,
                           error_message       = NULL,
                           log_error           = TRUE,
                           call                = rlang::caller_env(),
                           return_result_list  = getOption("horizons.return_safely_result", FALSE),
                           capture_trace       = getOption("horizons.capture_error_trace", FALSE),
                           trace_log_file      = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Capture the expression into a quosure.
  ## ---------------------------------------------------------------------------

  expr_quo <- rlang::enquo(expr)

  ## ---------------------------------------------------------------------------
  ## Step 2: Create a safe version of the function.
  ## ---------------------------------------------------------------------------

  safe_evaluator <- purrr::safely(function(){

    rlang::eval_tidy(expr_quo,
                     env = rlang::caller_env())
  },

  otherwise = default_value, quiet = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 3: Execute the safe evaluator and handle errors.
  ## ---------------------------------------------------------------------------

  result_list <- safe_evaluator()

  trace <- NULL

  ## ---------------------------------------------------------------------------
  ## Step 4: Check for errors and log them if necessary.
  ## ---------------------------------------------------------------------------

  if (!is.null(result_list$error)) {

    if (capture_trace) {
      if (exists("last_trace", asNamespace("rlang"), inherits = FALSE)) {
        trace <- tryCatch(rlang::last_trace(), error = function(e) NULL)
      } else {
        trace <- tryCatch(rlang::trace_back(), error = function(e) NULL)
      }
      attr(result_list$error, "trace") <- trace
      if (!is.null(trace_log_file)) {
        try({
          cat(capture.output(print(trace)), file = trace_log_file,
              sep = "\n", append = TRUE)
        }, silent = TRUE)
      }
    }

    if (log_error) {
      if (!is.null(error_message)) {

          evaluated_message <- tryCatch(

            glue::glue(error_message, .envir = rlang::caller_env()),
            error = function(e) {

            paste0(error_message, " (Error evaluating message: ", e$message, ")")

            }

          )

          cli::cli_warn("{evaluated_message}: {result_list$error$message}")

          } else {

          cli::cli_warn("An error occurred: {result_list$error$message}")

          }
      }

    if (return_result_list) {
      return(structure(list(result = default_value,
                           error  = result_list$error,
                           trace  = trace),
                       class = "horizons_safely_result"))
    }
    return(default_value)

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return the result if no error occurred.
  ## ---------------------------------------------------------------------------


  if (return_result_list) {
    return(structure(list(result = result_list$result,
                         error  = NULL,
                         trace  = NULL),
                     class = "horizons_safely_result"))
  }

  return(result_list$result)
}
