#' Read Spectral Data from OPUS or CSV Files
#'
#' @description
#' Reads spectral data from OPUS binary files or CSV text files.
#' Automatically detects and handles multiple channels in OPUS files.
#' Returns standardized spectral data in tidy format with metadata preserved.
#'
#' @param source Character. Type of file to read: "opus" or "csv"
#' @param spectra_path Character. Path to spectral data file(s).
#'   For OPUS: path to .0 file or directory containing .0 files
#'   For CSV: path to CSV file
#' @param spectra_type Character. Type of spectroscopy: "MIR" or "NIR".
#'   Affects expected wavelength/wavenumber ranges. Default: "MIR"
#' @param verbose Logical. Print informative messages about processing. Default: TRUE
#'
#' @return A tibble with spectral data in tidy format containing:
#'   \describe{
#'     \item{wavenumber}{Wavenumber values (cm⁻¹) for MIR}
#'     \item{wavelength}{Wavelength values (nm) for NIR}
#'     \item{absorbance}{Absorbance values}
#'   }
#'   With attributes:
#'   \describe{
#'     \item{spectra_type}{Type of spectroscopy (MIR/NIR)}
#'     \item{channel_used}{For OPUS files, which channel was selected}
#'     \item{source_path}{Original file path}
#'   }
#'
#' @examples
#' \dontrun{
#' # Read OPUS file
#' spectra <- read_spectra(
#'   source = "opus",
#'   spectra_path = "data/sample.0",
#'   spectra_type = "MIR"
#' )
#'
#' # Read CSV file
#' spectra <- read_spectra(
#'   source = "csv",
#'   spectra_path = "data/spectra.csv",
#'   spectra_type = "NIR"
#' )
#' }
#'
#' @export
read_spectra <- function(source       = c("opus", "csv"),
                        spectra_path  = NULL,
                        spectra_type  = c("MIR", "NIR"),
                        verbose       = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  source <- match.arg(source)

  ## ---------------------------------------------------------------------------

  spectra_type <- match.arg(spectra_type)

  ## ---------------------------------------------------------------------------

  if (!is.logical(verbose) || length(verbose) != 1) {

    cli::cli_abort("▶ read_spectra: verbose must be a single logical value")

  }

  ## ---------------------------------------------------------------------------

  if (is.null(spectra_path)) {

    cli::cli_abort("▶ read_spectra: spectra_path must be provided")

  }

  ## ---------------------------------------------------------------------------

  if (!file.exists(spectra_path)) {

    cli::cli_abort("▶ read_spectra: Path does not exist: {.path {spectra_path}}")

  }
  
  # Validate path is within working directory or subdirectories
  normalized_path <- normalizePath(spectra_path, mustWork = TRUE)
  working_dir <- normalizePath(getwd(), mustWork = TRUE)
  
  if (!startsWith(normalized_path, working_dir)) {
    
    cli::cli_abort("▶ read_spectra: Path must be within working directory")
    
  }

  ## ---------------------------------------------------------------------------

  if (source == "opus") {

    if (dir.exists(spectra_path)) {

      opus_files <- list.files(spectra_path,
                              pattern = "\\.[0-9]$",
                              full.names = TRUE)

      if (length(opus_files) == 0) {

        cli::cli_abort("▶ read_spectra: No OPUS files found in {.path {spectra_path}}")

      }

      if (verbose) {
        cli::cli_alert_info("Found {.val {length(opus_files)}} OPUS file{?s} in directory")
      }

    } else if (!grepl("\\.[0-9]$", spectra_path)) {

      cli::cli_abort("▶ read_spectra: File does not appear to be an OPUS file: {.path {spectra_path}}")

    }

  } else if (source == "csv") {

    if (dir.exists(spectra_path)) {

      cli::cli_abort("▶ read_spectra: CSV source requires a file path, not a directory: {.path {spectra_path}}")

    }

    if (!grepl("\\.csv$", tolower(spectra_path))) {

      cli::cli_alert_warning("▶ read_spectra: File does not have .csv extension: {.path {spectra_path}}")

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Branch to Source-Specific Reading
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_alert_info("Reading {.val {spectra_type}} spectra from {.val {source}} file")

  }

  ## ---------------------------------------------------------------------------

  if (source == "opus") {

    spectra_data <- read_opus_internal(path         = spectra_path,
                                       spectra_type = spectra_type,
                                       verbose      = verbose)

  } else if (source == "csv") {

    spectra_data <- read_csv_internal(path         = spectra_path,
                                      spectra_type = spectra_type,
                                      verbose      = verbose)

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Validate Output
  ## ---------------------------------------------------------------------------

  if (is.null(spectra_data) || !is.data.frame(spectra_data)) {

    cli::cli_abort("▶ read_spectra: Failed to read spectral data from {.path {spectra_path}}")

  }

  if (nrow(spectra_data) == 0) {

    cli::cli_abort("▶ read_spectra: No spectral data found in {.path {spectra_path}}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Add Metadata Attributes
  ## ---------------------------------------------------------------------------

  attr(spectra_data, "source")       <- source
  attr(spectra_data, "spectra_type") <- spectra_type
  attr(spectra_data, "source_path")  <- spectra_path

  if (verbose) {

    # Count samples and wavenumbers based on data structure
    n_samples <- nrow(spectra_data)
    
    # CSV data is already wide, OPUS data is still wide after pivot_wider
    # Count spectral columns (all except Sample_ID)
    n_wavenumber <- ncol(spectra_data) - 1
    
    cli::cli_alert_success("Successfully read {.val {n_samples}} sample{?s} with {.val {n_wavenumber}} wavenumber{?s}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return Spectral Data
  ## ---------------------------------------------------------------------------

  return(spectra_data)

}
