#' Internal Helper Functions for Reading Spectral Data
#'
#' @description
#' Collection of internal functions supporting the main read_spectra() function.
#' These handle format-specific reading of OPUS and CSV files.
#'
#' @keywords internal
#' @name inputs-helpers
NULL

## ---------------------------------------------------------------------------
## OPUS Reading Functions
## ---------------------------------------------------------------------------

#' Read OPUS Binary Files (Internal)
#'
#' @description
#' Internal function to read OPUS binary files. Handles both single files
#' and directories of OPUS files. Automatically selects the best available
#' channel.
#'
#' @param path `[character]` Path to OPUS file or directory
#' @param spectra_type `[character]` "MIR" or "NIR" (reserved for future use)
#' @param verbose `[logical]` Print progress messages
#'
#' @return `[tibble]` with spectral data
#'
#' @importFrom cli cli_abort cli_warn cli_text col_yellow
#' @importFrom glue glue
#' @importFrom opusreader2 read_opus
#' @importFrom purrr map keep map_chr
#' @importFrom tibble tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
#'
#' @name read_opus_internal
#' @keywords internal
read_opus_internal <- function(path, spectra_type, verbose) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Determine if single file or directory
  ## ---------------------------------------------------------------------------

  if (dir.exists(path)) {

    ## Get all OPUS files matching the pattern ----------------------------------

    opus_files <- list.files(path,
                            pattern = "\\.[0-9]$",
                            full.names = TRUE)

    if (length(opus_files) == 0) cli::cli_abort("No OPUS files found in directory")

    if (verbose) cli::cli_text("│  ├─ Processing {.val {length(opus_files)}} OPUS file{?s}")

  } else {

    ## Single file path provided ------------------------------------------------

    opus_files <- path

    if (verbose) cli::cli_text("│  ├─ Processing single OPUS file")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Read OPUS files using vectorized approach
  ## ---------------------------------------------------------------------------

  ## Read all files at once (opusreader2 handles both files and directories) --

  if (verbose) cli::cli_text("│  ├─ Reading OPUS data...")

  safely_execute(expr = {
                          suppressWarnings(
                            opusreader2::read_opus(dsn          = path,
                                                   progress_bar = FALSE)
                            )
                         },
    default_value = NULL,
    error_message = glue::glue("Failed to read OPUS file(s) from: {path}")) -> opus_result_safe


   handle_results(safe_result   = opus_result_safe,
                 error_title   = "OPUS reading failed",
                 error_hints   = c("Check if file path exists and is readable: {path}",
                                   "Check if files are valid OPUS format (.0 extension)",
                                   "Check if opusreader2 package is properly installed"),
                 abort_on_null = FALSE) -> raw_data

  if (is.null(raw_data)) return(NULL)

  ## ---------------------------------------------------------------------------
  ## Step 3: Extract spectral data from each file
  ## ---------------------------------------------------------------------------

  ## Channel priority order (prefer processed absorbance over raw reflectance) -

  channel_priority <- c("ab_no_atm_comp", "ab", "sc_sample", "sc_ref")

  ## Extract data using first available channel from each file -----------------

  spectra_info <- purrr::map(names(raw_data), function(file_name) {

    file_data <- raw_data[[file_name]]

    ## Try each channel in priority order --------------------------------------

    for (channel in channel_priority) {

      if (channel %in% names(file_data)) {

        spectral_data <- file_data[[channel]]$data

        if (!is.null(spectral_data)) {

          return(list(data = tibble::tibble(Sample_ID  = tools::file_path_sans_ext(file_name),
                                            Wavenumber = as.numeric(colnames(spectral_data)),
                                            Absorbance = as.numeric(spectral_data)),
                      channel = channel))

        }

      }

    }

    ## No valid data found in this file ----------------------------------------

    cli::cli_warn("No spectral data found in file: {.file {file_name}}")
    return(NULL)

  })

  ## Keep only files with valid data -------------------------------------------

  valid_spectra <- purrr::keep(spectra_info, ~!is.null(.))
  spectra_list  <- purrr::map(valid_spectra, "data")
  channels_used <- purrr::map_chr(valid_spectra, "channel")

  ## ---------------------------------------------------------------------------
  ## Step 4: Combine all spectra into single tibble
  ## ---------------------------------------------------------------------------

  if (length(spectra_list) == 0) cli::cli_abort("No valid spectral data found in any files")

  spectra_data <- dplyr::bind_rows(spectra_list)

  ## ---------------------------------------------------------------------------
  ## Step 5: Check channel consistency and add attributes
  ## ---------------------------------------------------------------------------

  unique_channels <- unique(channels_used)

  if (verbose) {

    if (length(unique_channels) > 1) {

      cli::cli_text("│  └─ ⚠ Multiple channels used: {.val {unique_channels}}")

    } else if (length(unique_channels) == 1) {

      cli::cli_text("│  └─ Channel used: {.val {unique_channels[1]}}")

    }

  }

  ## Add channel metadata as attribute -----------------------------------------

  attr(spectra_data, "channel_used") <- unique_channels[1]

  ## ---------------------------------------------------------------------------
  ## Step 6: Pivot to wide format and return
  ## ---------------------------------------------------------------------------

  ## Convert from long to wide (samples as rows, wavenumbers as columns) -------

  spectra_wide <- spectra_data %>%
    tidyr::pivot_wider(names_from  = Wavenumber,
                       values_from = Absorbance,
                       names_prefix = "")

  ## Preserve channel attribute and return -------------------------------------

  attr(spectra_wide, "channel_used") <- unique_channels[1]

  return(spectra_wide)

}

## ---------------------------------------------------------------------------
## CSV Reading Functions
## ---------------------------------------------------------------------------

#' Read CSV Files (Internal)
#'
#' @description
#' Internal function to read CSV files containing spectral data.
#' Expects first column to be Sample_ID and remaining columns to be
#' wavelength/wavenumber values.
#'
#' @param path `[character]` Path to CSV file
#' @param spectra_type `[character]` "MIR" or "NIR" (reserved for future use)
#' @param verbose `[logical]` Print progress messages
#'
#' @return `[tibble]` with spectral data in wide format
#'
#' @importFrom cli cli_abort cli_warn cli_text col_yellow
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across all_of
#'
#' @name read_csv_internal
#' @keywords internal
read_csv_internal <- function(path, spectra_type, verbose) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Read CSV file
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│  ├─ Reading CSV file...")

  safely_execute(expr = {
                          readr::read_csv(path,
                                          show_col_types = FALSE,
                                          progress       = FALSE)
                         },
                 default_value = NULL,
                 error_message = glue::glue("Failed to read CSV file: {path}")) -> csv_result_safe

  handle_results(safe_result   = csv_result_safe,
                 error_title   = "CSV reading failed",
                 error_hints   = c("Check if file exists and is readable: {path}",
                                   "Check if file is valid CSV format with proper delimiters",
                                   "Check if file encoding is correct (try UTF-8)",
                                   "Check if first column contains Sample_ID values"),
                 abort_on_null = FALSE) -> csv_data

  if (is.null(csv_data)) return(NULL)

  ## ---------------------------------------------------------------------------
  ## Step 2: Validate structure
  ## ---------------------------------------------------------------------------

  if (ncol(csv_data) < 2) cli::cli_abort("CSV must have at least 2 columns (Sample_ID + spectral data)")

  ## First column is assumed to be Sample_ID -----------------------------------

  id_col <- names(csv_data)[1]

  if (verbose) {

    cli::cli_text("│  ├─ Using {.val {id_col}} as Sample_ID column")
    cli::cli_text("│  ├─ Found {.val {nrow(csv_data)}} samples with {.val {ncol(csv_data) - 1}} wavelengths")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Standardize column names and data types
  ## ---------------------------------------------------------------------------

  ## Rename first column to Sample_ID for consistency --------------------------

  names(csv_data)[1] <- "Sample_ID"

  ## Convert spectral columns to numeric ---------------------------------------

  spectral_cols <- names(csv_data)[-1]

  csv_data <- csv_data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(spectral_cols), as.numeric))

  ## ---------------------------------------------------------------------------
  ## Step 4: Validate spectral column names and return
  ## ---------------------------------------------------------------------------

  ## Check if column names are numeric wavenumbers/wavelengths -----------------

  x_values <- suppressWarnings(as.numeric(spectral_cols))

  if (all(is.na(x_values))) {

    cli::cli_text("│  └─ {cli::col_yellow('⚠ Column names are not numeric wavenumbers - may cause downstream issues')}")

  }

  return(csv_data)

}

## =============================================================================
## Helper Functions
## =============================================================================

#' Parse Sample Metadata from File Name
#'
#' @description
#' Interprets a file name using a format string and delimiter to extract sample-level metadata,
#' such as `Sample_ID` and `Fraction`. Useful for batch processing OPUS files where metadata
#' is embedded in the file name.
#'
#' @param file_name `[character]` The file name (e.g., `"FFAR_001_Bulk.0"`)
#' @param format_string `[character]` A format template describing each part of the file name
#'   (e.g., `"project_sampleid_fraction"`). Allowed tokens: `"sampleid"`, `"fraction"`,
#'   `"project"`, `"ignore"`, `"wellid"`, `"scanid"`. Tokens must appear in the same order
#'   as in the actual file name.
#' @param delimiter `[character]` Delimiter used to split the file name. Default: `"_"`
#' @param default_fraction `[character]` Value to assign if `fraction` is not provided or
#'   cannot be parsed. Default: `"GroundBulk"`
#'
#' @return A `[tibble]` with columns `Sample_ID` and `Fraction`, and optionally other parsed tokens
#'
#' @details
#' This function is typically used during OPUS spectral ingestion to extract metadata from filenames.
#' If a required token like `sampleid` is missing from the filename, a warning is issued and the
#' default value `"UNKNOWN"` is used.
#'
#' @seealso [create_dataset()] for dataset creation with ID parsing
#'
#' @importFrom purrr set_names map map_chr
#' @importFrom tibble as_tibble tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom cli cli_warn cli_alert cli_alert_danger
#' @importFrom rlang %||%
#'
#' @name parse_filename_metadata
#' @keywords internal

parse_filename_metadata <- function(file_name,
                                    format_string,
                                    delimiter = "_",
                                    default_fraction = "GroundBulk") {

  ## ---------------------------------------------------------------------------
  ## Step 1: Parse filename into components
  ## ---------------------------------------------------------------------------

  ## TODO: Consider adding support for multiple delimiters

  base_name     <- tools::file_path_sans_ext(file_name)
  name_parts    <- unlist(strsplit(base_name, delimiter))
  format_tokens <- tolower(unlist(strsplit(format_string, delimiter)))

  ## ---------------------------------------------------------------------------
  ## Step 2: Validate filename structure
  ## ---------------------------------------------------------------------------

  if (length(name_parts) < length(format_tokens)) {

    cli::cli_warn("Filename {.file {file_name}} has fewer parts than expected by format {.val {format_string}}")
    return(tibble::tibble(Sample_ID = "UNKNOWN", Fraction = default_fraction))

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Map filename parts to format tokens
  ## ---------------------------------------------------------------------------

  mapped <- purrr::set_names(name_parts[seq_along(format_tokens)], format_tokens)

  ## ---------------------------------------------------------------------------
  ## Step 4: Convert to tibble with standardized column names
  ## ---------------------------------------------------------------------------

  ## Convert mapped tokens to tibble -------------------------------------------

  purrr::map(setNames(names(mapped), names(mapped)),
             ~ mapped[[.x]] %||% NA_character_) %>%
    tibble::as_tibble() -> metadata

  ## Map token names to standard column names ----------------------------------

  token_map <- c(project  = "Project",
                 sampleid = "Sample_ID",
                 fraction = "Fraction",
                 wellid   = "Well_ID",
                 scanid   = "Scan")

  names(metadata) <- purrr::map_chr(names(metadata),
                                    function(nm) {
                                      mapped <- token_map[nm]
                                      if (!is.na(mapped)) mapped else nm
                                    })

  ## Handle missing required fields -------------------------------------------

  if (!"Fraction" %in% names(metadata)) {

    metadata$Fraction <- default_fraction
    cli::cli_alert("No fraction found in file name {.file {file_name}}. Using default: {.val {default_fraction}}")

  }

  if (!"Sample_ID" %in% names(metadata)) {

    metadata$Sample_ID <- "UNKNOWN"
    cli::cli_alert_danger("No Sample_ID found in file name {.file {file_name}}. Using default: {.val UNKNOWN}\nSamples will be averaged across all loaded scans")

  }

  return(metadata)
}
