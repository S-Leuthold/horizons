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
#' \itemize{
#'   \item OPUS binary file reading with automatic channel selection
#'   \item CSV text file reading with format validation
#'   \item Sample ID extraction and standardization
#'   \item Metadata preservation via tibble attributes
#' }
#' 
#' The returned data structure uses numeric column names representing wavenumber
#' values, making it compatible with downstream preprocessing and modeling functions.
#'
#' @param source Character. Type of file to read: "opus" or "csv"
#' @param spectra_path Character. Path to spectral data file(s).
#'   For OPUS: path to .0 file or directory containing .0 files
#'   For CSV: path to CSV file
#' @param spectra_type Character. Type of spectroscopy: "MIR" or "NIR".
#'   Affects expected wavelength/wavenumber ranges. Default: "MIR"
#' @param verbose Logical. Print informative messages about processing. Default: TRUE
#'
#' @return A tibble with spectral data in wide format containing:
#'   \describe{
#'     \item{Sample_ID}{Character. Unique sample identifier}
#'     \item{<wavenumber_cols>}{Numeric columns named with wavenumber values (e.g., "600", "602", "604") containing absorbance values}
#'   }
#'   With attributes:
#'   \describe{
#'     \item{source}{Character. Source type ("opus" or "csv")}
#'     \item{spectra_type}{Character. Spectroscopy type ("MIR" or "NIR")}
#'     \item{source_path}{Character. Original file path}
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
#' @seealso 
#' \code{\link{preprocess_spectra}} for spectral preprocessing,
#' \code{\link{create_dataset}} for combining with response data
#' 
#' @family inputs
#' @keywords spectroscopy data-import
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
  ## Step 2: Display Configuration Summary
  ## ---------------------------------------------------------------------------
  
  # Prepare configuration information
  config_info <- list(
    "Source" = if (source == "opus") {
      if (dir.exists(spectra_path)) {
        paste0("OPUS directory (", length(list.files(spectra_path, pattern = "\\.[0-9]$")), " files)")
      } else {
        "OPUS file"
      }
    } else {
      "CSV file"
    },
    "Spectra type" = spectra_type,
    "Input path" = spectra_path,
    "Output format" = "Raw tibble"
  )
  
  display_config_summary("Spectral Data Reading Pipeline", config_info, verbose)

  ## ---------------------------------------------------------------------------
  ## Step 3: File Processing Pipeline
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text(format_header("File Processing Pipeline", style = "single", center = FALSE))
    cli::cli_text("")
  }

  # Track processing time
  start_time <- Sys.time()
  
  if (source == "opus") {

    if (verbose) {
      cli::cli_text(format_tree_item("OPUS Data Extraction", level = 0))
    }

    spectra_data <- read_opus_internal(path         = spectra_path,
                                       spectra_type = spectra_type,
                                       verbose      = verbose)

  } else if (source == "csv") {

    if (verbose) {
      cli::cli_text(format_tree_item("CSV Data Loading", level = 0))
    }

    spectra_data <- read_csv_internal(path         = spectra_path,
                                      spectra_type = spectra_type,
                                      verbose      = verbose)

  }
  
  processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Step 4: Validate Output
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

  # Display results summary
  if (verbose) {
    
    # Count samples and wavenumbers based on data structure  
    n_samples <- nrow(spectra_data)
    
    # Count spectral columns (all except Sample_ID)
    n_wavenumber <- ncol(spectra_data) - 1
    
    # Determine wavenumber range for display
    spectral_cols <- setdiff(names(spectra_data), "Sample_ID")
    wn_values <- suppressWarnings(as.numeric(spectral_cols))
    wn_values <- wn_values[!is.na(wn_values)]
    
    if (length(wn_values) > 0) {
      wn_range <- paste0(round(min(wn_values)), "-", round(max(wn_values)), " cm⁻¹")
    } else {
      wn_range <- "various wavelengths"
    }
    
    # Display results
    results_info <- list(
      "Samples" = paste0(format_metric(n_samples, "count"), " processed successfully"),
      "Wavenumbers" = paste0(format_metric(n_wavenumber, "count"), " (", wn_range, ")"),
      "Format" = "Tibble with Sample_ID + numeric columns",
      "Processing time" = format_time(processing_time)
    )
    
    display_operation_results("Data reading", results_info, processing_time, "success", verbose)

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return Spectral Data
  ## ---------------------------------------------------------------------------

  return(spectra_data)

}
