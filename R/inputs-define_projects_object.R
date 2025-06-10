#' Define a Named List of Projects for `create_input_data()`
#'
#' Wraps multiple `project_entry()` calls into a named list. Each name becomes the
#' project identifier in the returned dataset.
#'
#' @param ... Named calls to `project_entry()`. Each name will populate the `Project` column.
#'
#' @return A named list of project definitions.
#' @export

project_list <- function(...) {
  dots <- rlang::enquos(...)
  names <- names(dots)

  if (any(is.null(names) | names == "")) {
    cli::cli_abort("All entries in `project_list()` must be named using the syntax: NAME = project_entry(...).")
  }

  purrr::map(dots, rlang::eval_tidy)
}
