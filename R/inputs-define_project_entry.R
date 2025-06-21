#' Define a Project for Spectral Data Ingestion
#'
#' Creates a structured project entry used by \code{\link{create_project_data}} to
#' locate and parse OPUS spectral files, join sample-level observations, and
#' assign metadata fields. Each entry defines a dataset's input files and parsing rules.
#'
#' @param spectra_path Character. Path to a directory containing OPUS \code{.0} spectral files.
#' @param sample_obs Character. Path to a \code{.csv} file with sample-level observations.
#' @param file_name_format Character. Delimiter-separated format string describing metadata fields in the file name.
#'   Must include the \code{"sampleid"} token. Other valid tokens include \code{"fraction"}, \code{"project"}, and \code{"ignore"}.
#' @param file_name_delimiter Character. Delimiter used to split the filename string (default: \code{"_"}).
#' @param default_fraction Character. Default fraction to assign if no fraction is parsed (default: \code{"GroundBulk"}).
#'
#' @return A list with elements: \code{spectra_path}, \code{sample_obs}, \code{file_name_format},
#'   \code{file_name_delimiter}, and \code{default_fraction}.
#'
#' @details
#' This function is typically used inside a call to \code{\link{project_list}}, which assembles
#' multiple project entries into a single structure passed to \code{\link{create_project_data}}.
#' File name parsing is tokenized by position, and mismatches will cause runtime warnings.
#'
#' @seealso \code{\link{create_project_data}}, \code{\link{project_list}}, \code{\link{parse_filename_metadata}}
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
#' @family input_preparation
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
