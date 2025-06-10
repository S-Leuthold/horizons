#' Define a Project for Spectral Data Ingestion
#'
#' Creates a structured project entry used by \code{create_project_data()} to
#' locate and parse OPUS spectral files, join sample-level observations, and
#' assign metadata fields. Each project entry specifies the paths, file parsing
#' rules, and fallback defaults for a single dataset.
#'
#' @param spectra_path        Character. Path to the folder containing OPUS \code{.0} spectral files.
#'                            Must be a valid directory.
#' @param sample_obs          Character. Path to a \code{.csv} file containing sample-level observations
#'                            (e.g., physicochemical properties, site metadata). Must exist.
#' @param file_name_format    Character. A delimiter-separated string describing the position and role
#'                            of metadata fields in each file name (e.g., \code{"sampleid_fraction"}).
#'                            Required tokens include \code{"sampleid"}. Optional tokens include
#'                            \code{"fraction"}, \code{"project"}, or \code{"ignore"}.
#' @param file_name_delimiter Character. Delimiter used to separate tokens in the file name (e.g., \code{"_"} or \code{"-"}).
#'                            Defaults to \code{"_"}.
#' @param default_fraction    Character. Default value to assign if \code{fraction} is not provided or
#'                            cannot be parsed from the file name. Defaults to \code{"GroundBulk"}.
#'
#' @return A structured list with five fields used to define a project for ingestion.
#'
#' @details
#' This function is typically called inside a named list passed to \code{project_list()},
#' where each list element represents a project used in downstream ingestion workflows.
#' The \code{file_name_format} string is interpreted positionally and should match the structure
#' of all file names within \code{spectra_path}. If filenames deviate, parsing errors or warnings
#' will be raised at runtime.
#'
#' @seealso \code{\link{project_list}}, \code{\link{create_project_data}}, \code{\link{parse_filename_metadata}}
#'
#' @examples
#' project_entry(
#'   spectra_path        = "data/FFAR/spectra",
#'   sample_obs          = "data/FFAR/soil.csv",
#'   file_name_format    = "project_sampleid_fraction_ignore_ignore",
#'   file_name_delimiter = "_",
#'   default_fraction    = "Bulk"
#' )
#'
#' @export


project_entry <- function(spectra_path,
                          sample_obs,
                          file_name_format,
                          file_name_delimiter,
                          default_fraction = "GroundBulk") {

  ## ---------------------------------------------------------------------------
  ## Step 1: A bivy of defensive checks
  ## ---------------------------------------------------------------------------

  if (!dir.exists(spectra_path)) {
    cli::cli_abort("The provided `spectra_path` does not exist: {.path {spectra_path}}")
  }

  if (!file.exists(sample_obs)) {
    cli::cli_abort("The provided `sample_obs` file does not exist: {.path {sample_obs}}")
  }

  if (!is.character(file_name_format) || length(file_name_format) != 1) {
    cli::cli_abort("`file_name_format` must be a single character string.")
  }

  if (!is.character(file_name_delimiter) || length(file_name_delimiter) != 1) {
    cli::cli_abort("`file_name_delimiter` must be a single character string.")
  }

  if (!is.character(default_fraction) || length(default_fraction) != 1) {
    cli::cli_abort("`default_fraction` must be a single character string.")
  }

  tokens <- unlist(strsplit(file_name_format, file_name_delimiter))

  if (!"sampleid" %in% tolower(tokens)) {
    cli::cli_warn("The `file_name_format` does not include a `sampleid` token. Parsed Sample_IDs may be invalid.")
  }

  if (any(duplicated(tokens))) {
    cli::cli_abort("Duplicate tokens found in `file_name_format`: {.val {tokens[duplicated(tokens)]}}")
  }

  if (length(list.files(spectra_path, pattern = "\\.0$", full.names = TRUE)) == 0) {
    cli::cli_alert_info("No `.0` files found in {.path {spectra_path}}. You can still create the project entry.")
  }


  ## ---------------------------------------------------------------------------
  ## Step 2: Return project details
  ## ---------------------------------------------------------------------------
  list(spectra_path        = spectra_path,
       sample_obs          = sample_obs,
       file_name_format    = file_name_format,
       file_name_delimiter = file_name_delimiter,
       default_fraction    = default_fraction)

  }
