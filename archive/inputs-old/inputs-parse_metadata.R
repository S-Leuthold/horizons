#' Parse Sample Metadata from File Name
#'
#' Interprets a file name using a format string and delimiter to extract sample-level metadata,
#' such as `Sample_ID` and `Fraction`. Useful for batch processing OPUS files where metadata
#' is embedded in the file name.
#'
#' @param file_name Character. The file name (e.g., \code{"FFAR_001_Bulk.0"}).
#' @param format_string Character. A format template describing each part of the file name (e.g., \code{"project_sampleid_fraction"}).
#'   Allowed tokens: \code{"sampleid"}, \code{"fraction"}, \code{"project"}, \code{"ignore"}, \code{"wellid"}, \code{"scanid"}.
#'   Tokens must appear in the same order as in the actual file name.
#' @param delimiter Character. Delimiter used to split the file name (default = \code{"_"}).
#' @param default_fraction Character. Value to assign if \code{fraction} is not provided or cannot be parsed.
#'
#' @return A tibble with columns \code{Sample_ID} and \code{Fraction}, and optionally other parsed tokens.
#'
#' @details
#' This function is typically used during OPUS spectral ingestion to extract metadata from filenames.
#' If a required token like \code{sampleid} is missing from the filename, a warning is issued and the
#' default value `"UNKNOWN"` is used.
#'
#' @seealso \code{\link{project_entry}}, \code{\link{create_project_data}}
#'
#' @importFrom purrr set_names map map_chr
#' @importFrom tibble as_tibble tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom cli cli_warn cli_alert cli_alert_danger
#' @importFrom rlang %||%
#'
#' @keywords internal



parse_filename_metadata <- function(file_name,
                                    format_string,
                                    delimiter = "_",
                                    default_fraction = "GroundBulk") {

  ## ---------------------------------------------------------------------------
  ## Step 1: Standardize input names
  ## ---------------------------------------------------------------------------

  # TODO: Consider adding a means to have more than one delimiter accepted.

  base_name     <- tools::file_path_sans_ext(file_name)
  name_parts    <- unlist(strsplit(base_name, delimiter))
  format_tokens <- tolower(unlist(strsplit(format_string, delimiter)))

  ## ---------------------------------------------------------------------------
  ## Step 2: Validate the number of parts in the file name against the format.
  ## ---------------------------------------------------------------------------

  if (length(name_parts) < length(format_tokens)) {
    cli::cli_warn("Filename {.file {file_name}} has fewer parts than expected by format {.val {format_string}}.")
    return(tibble::tibble(Sample_ID = "UNKNOWN", Fraction = default_fraction))
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Subset the sample metadata tokens.
  ## ---------------------------------------------------------------------------

  mapped <- purrr::set_names(name_parts[seq_along(format_tokens)], format_tokens)

  ## ---------------------------------------------------------------------------
  ## Step 4: Return the parsed metadata as a tibble.
  ## ---------------------------------------------------------------------------

  purrr::map(setNames(names(mapped), names(mapped)),
                         ~ mapped[[.x]] %||% NA_character_) %>%
    tibble::as_tibble() -> metadata

  token_map       <- c(project  = "Project",
                       sampleid = "Sample_ID",
                       fraction = "Fraction",
                       wellid   = "Well_ID",
                       scanid   = "Scan")

  names(metadata) <- purrr::map_chr(
    names(metadata),
    function(nm) {
      mapped <- token_map[nm]
      if (!is.na(mapped)) mapped else nm
    }
  )

  if (!"Fraction" %in% names(metadata)) {
    metadata$Fraction <- default_fraction
    cli::cli_alert("No fraction found in file name {.file {file_name}}. Using default: {.val {default_fraction}}.")
  }

  if (!"Sample_ID" %in% names(metadata)) {
    metadata$Sample_ID <- "UNKNOWN"
    cli::cli_alert_danger("No Sample_ID found in file name {.file {file_name}}. Using default: {.val UNKNOWN}. /n Samples will be averaged across all loaded scans.")
  }

  return(metadata)
}
