#' Define a Named List of Projects for Spectral Ingestion
#'
#' Wraps multiple \code{\link{project_entry}} calls into a named list, where each name
#' becomes the project identifier used by \code{\link{create_project_data}}. This enables
#' batch ingestion of multi-project spectral datasets.
#'
#' @param ... Named calls to \code{\link{project_entry}}. Each name will populate the \code{Project} column
#'   in the combined dataset.
#'
#' @return A named list of project definitions, validated for structure.
#'
#' @details
#' Each argument must be explicitly named. These names become keys used throughout
#' ingestion and downstream processing, including metadata joins and progress reporting.
#'
#' @seealso \code{\link{project_entry}}, \code{\link{create_project_data}}
#'
#' @examples
#' project_list(
#'   FFAR = project_entry("data/FFAR/spectra", "data/FFAR/soil.csv", "sampleid_fraction", "_"),
#'   AONR = project_entry("data/AONR/OPUS", "data/AONR/soil.csv", "sampleid_fraction", "_", "Clay")
#' )
#'
#' @importFrom rlang enquos eval_tidy
#' @importFrom cli cli_abort
#' @importFrom purrr map
#'
#' @family input_preparation
#' @export



project_list <- function(...) {
  dots <- rlang::enquos(...)
  names <- names(dots)

  if (any(is.null(names) | names == "")) {
    cli::cli_abort("All entries in `project_list()` must be named using the syntax: NAME = project_entry(...).")
  }

  purrr::map(dots, rlang::eval_tidy)
}
