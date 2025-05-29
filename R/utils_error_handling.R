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
#'
#' @return The result of `expr` if successful, or `default_value` if an error occurs.
#'
#' @importFrom purrr safely
#' @importFrom cli cli_warn
#' @importFrom rlang enquos eval_tidy
#'
#' @keywords internal


safely_execute <- function(expr,
                           default_value = NULL,
                           error_message = NULL,
                           log_error     = TRUE,
                           call          = rlang::caller_env()) {

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

  ## ---------------------------------------------------------------------------
  ## Step 4: Check for errors and log them if necessary.
  ## ---------------------------------------------------------------------------

  if (!is.null(result_list$error)) {
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

    return(default_value)

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return the result if no error occurred.
  ## ---------------------------------------------------------------------------


  return(result_list$result)
}
