#' Read Spectral Data from OPUS or CSV Files
#'
#' @description
#' Reads spectral data from OPUS binary files or CSV text files and returns it in
#' wide format suitable for modeling workflows. For OPUS files, automatically detects
#' and selects the optimal channel. All data is returned with standardized column
#' names and preserved metadata attributes.
#'
#' @details
#' This function serves as the entry point for the spectroscopy workflow. It handles:
#'
#' * OPUS binary file reading with automatic channel selection
#' * CSV text file reading with format validation
#' * Sample ID extraction and standardization
#' * Metadata preservation via tibble attributes
#'
#' The returned data structure uses numeric column names representing wavenumber
#' values, making it compatible with downstream preprocessing and modeling functions.
#'
#' @param source `[character]` Type of file to read: `"opus"` or `"csv"`. Default: `"opus"`
#' @param spectra_path `[character]` Path to spectral data file(s).
#'   * For OPUS: path to `.0` file or directory containing `.0` files
#'   * For CSV: path to CSV file
#' @param spectra_type `[character]` Type of spectroscopy: `"MIR"` or `"NIR"`.
#'   Affects expected wavelength/wavenumber ranges. Default: `"MIR"`
#' @param verbose `[logical]` Print informative messages about processing. Default: `TRUE`
#'
#' @return A `[tibble]` with spectral data in wide format containing:
#'   * `Sample_ID`: Character. Unique sample identifier
#'   * `<wavenumber_cols>`: Numeric columns named with wavenumber values (e.g., `"600"`, `"602"`, `"604"`)
#'     containing absorbance values
#'
#'   With attributes:
#'   * `source`: Character. Source type (`"opus"` or `"csv"`)
#'   * `spectra_type`: Character. Spectroscopy type (`"MIR"` or `"NIR"`)
#'   * `source_path`: Character. Original file path
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
#' @seealso
#' [preprocess_spectra()] for spectral preprocessing,
#' [create_dataset()] for combining with response data
#'
#' @family inputs
#' @keywords spectroscopy data-import
#'
#' @importFrom cli cli_abort cli_warn cli_text
#'
#' @export
read_spectra <- function(source       = c("opus", "csv"),
                        spectra_path  = NULL,
                        spectra_type  = c("MIR", "NIR"),
                        verbose       = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  ## Validate and standardize source and spectra_type arguments ----------------

  source       <- match.arg(source)
  spectra_type <- match.arg(spectra_type)

  if (!is.logical(verbose) || length(verbose) != 1) cli::cli_abort("verbose must be a single logical value")

  ## Validate path argument exists and is accessible ---------------------------

  if (is.null(spectra_path)) cli::cli_abort("spectra_path must be provided")

  if (!file.exists(spectra_path)) cli::cli_abort("Path does not exist: {.path {spectra_path}}")

  ## Source-specific path validation -------------------------------------------

  if (source == "opus") {

    if (dir.exists(spectra_path)) {

      ## Check for OPUS files in directory -------------------------------------

      opus_files <- list.files(spectra_path,
                              pattern = "\\.[0-9]$",
                              full.names = TRUE)

      if (length(opus_files) == 0) cli::cli_abort("No OPUS files found in {.path {spectra_path}}")

    } else if (!grepl("\\.[0-9]$", spectra_path)) {

      cli::cli_abort("File does not appear to be an OPUS file: {.path {spectra_path}}")

    }

  } else if (source == "csv") {

    if (dir.exists(spectra_path)) cli::cli_abort("CSV source requires a file path, not a directory: {.path {spectra_path}}")

    if (!grepl("\\.csv$", tolower(spectra_path))) {

      cli::cli_warn("File does not have .csv extension: {.path {spectra_path}}")

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Display Configuration Summary
  ## ---------------------------------------------------------------------------

  if (verbose) {

    ## Determine source description ---------------------------------------------

    source_desc <- if (source == "opus") {
      if (dir.exists(spectra_path)) {
        paste0("OPUS directory (", length(list.files(spectra_path, pattern = "\\.[0-9]$")), " files)")
      } else {
        "OPUS file"
      }
    } else {
      "CSV file"
    }

    ## Display configuration ----------------------------------------------------

    cli::cli_text("{.strong Spectral Data Reading Pipeline}")
    cli::cli_text("├─ Source: {source_desc}")
    cli::cli_text("├─ Spectra type: {spectra_type}")
    cli::cli_text("├─ Input path: {.path {spectra_path}}")
    cli::cli_text("└─ Output format: Raw tibble")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: File Processing Pipeline
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_text("{.strong File Processing Pipeline}")

  }

  start_time <- Sys.time()

  if (source == "opus") {

    if (verbose) cli::cli_text("├─ OPUS Data Extraction")

    spectra_data <- read_opus_internal(path         = spectra_path,
                                       spectra_type = spectra_type,
                                       verbose      = verbose)

  } else if (source == "csv") {

    if (verbose) cli::cli_text("├─ CSV Data Loading")

    spectra_data <- read_csv_internal(path         = spectra_path,
                                      spectra_type = spectra_type,
                                      verbose      = verbose)

  }

  processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Step 4: Validate Output
  ## ---------------------------------------------------------------------------

  if (is.null(spectra_data) || !is.data.frame(spectra_data)) {

    cli::cli_abort("Failed to read spectral data from {.path {spectra_path}}")

  }

  if (nrow(spectra_data) == 0) cli::cli_abort("No spectral data found in {.path {spectra_path}}")

  ## ---------------------------------------------------------------------------
  ## Step 5: Add Metadata Attributes and Display Results
  ## ---------------------------------------------------------------------------

  attr(spectra_data, "source")       <- source
  attr(spectra_data, "spectra_type") <- spectra_type
  attr(spectra_data, "source_path")  <- spectra_path

  if (verbose) {

    ## Calculate summary statistics --------------------------------------------

    n_samples    <- nrow(spectra_data)
    n_wavenumber <- ncol(spectra_data) - 1

    ## Determine wavenumber range for display ----------------------------------

    spectral_cols <- setdiff(names(spectra_data), "Sample_ID")
    wn_values     <- suppressWarnings(as.numeric(spectral_cols))
    wn_values     <- wn_values[!is.na(wn_values)]

    wn_range <- if (length(wn_values) > 0) {
      paste0(round(min(wn_values)), "-", round(max(wn_values)), " cm⁻¹")
    } else {
      "various wavelengths"
    }

    ## Display results ---------------------------------------------------------

    cli::cli_text("└─ {.strong Summary}")
    cli::cli_text("   ├─ Samples: {n_samples} processed successfully")
    cli::cli_text("   ├─ Wavenumbers: {n_wavenumber} ({wn_range})")
    cli::cli_text("   ├─ Format: Tibble with Sample_ID + numeric columns")
    cli::cli_text("   └─ Processing time: {round(processing_time, 2)}s")

  }

  return(spectra_data)

}
