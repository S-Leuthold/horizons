#' Internal Helper Functions for Reading Spectral Data
#'
#' @description
#' Collection of internal functions supporting the main read_spectra() function.
#' These handle format-specific reading of OPUS and CSV files.
#'
#' @keywords internal
#' @name inputs-helpers

## ---------------------------------------------------------------------------
## OPUS Reading Functions
## ---------------------------------------------------------------------------

#' Read OPUS Files (Internal)
#'
#' @description
#' Internal function to read OPUS binary files. Handles both single files
#' and directories of OPUS files. Automatically selects the best available
#' channel.
#'
#' @param path Character. Path to OPUS file or directory
#' @param spectra_type Character. "MIR" or "NIR"
#' @param verbose Logical. Print progress messages
#'
#' @return Tibble with spectral data
#' @keywords internal
read_opus_internal <- function(path, spectra_type, verbose) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Determine if single file or directory
  ## ---------------------------------------------------------------------------
  
  if (dir.exists(path)) {
    
    # Get all OPUS files in directory
    opus_files <- list.files(path, 
                            pattern = "\\.[0-9]$", 
                            full.names = TRUE)
    
    if (length(opus_files) == 0) {
      
      cli::cli_abort("▶ read_opus_internal: No OPUS files found in directory")
      
    }
    
    if (verbose) {
      cli::cli_alert_info("Processing {.val {length(opus_files)}} OPUS file{?s}")
    }
    
  } else {
    
    # Single file
    opus_files <- path
    
    if (verbose) {
      cli::cli_alert_info("Processing single OPUS file")
    }
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Read OPUS files using vectorized approach
  ## ---------------------------------------------------------------------------
  
  # Note: read_opus can handle both single files and directories
  # When given a directory or vector of files, it returns a nested list
  
  if (verbose) {
    cli::cli_alert_info("Reading OPUS data...")
  }
  
  # Read all files at once (vectorized)
  safely_execute(
    expr = {
      suppressWarnings(
        opusreader2::read_opus(dsn = path)
      )
    },
    default_value = NULL,
    error_message = glue::glue("Failed to read OPUS file(s) from: {path}")
  ) -> opus_result
  
  raw_data <- opus_result$result
  
  if (is.null(raw_data)) {
    
    if (!is.null(opus_result$error)) {
      
      cli::cli_alert_danger("OPUS reading failed with error: {.emph {opus_result$error$message}}")
      cli::cli_alert_info("Check that:")
      cli::cli_ul(c(
        "File path exists and is readable: {.path {path}}",
        "Files are valid OPUS format (.0 extension)", 
        "opusreader2 package is properly installed"
      ))
      
    }
    
    return(NULL)
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Extract spectral data from each file
  ## ---------------------------------------------------------------------------
  
  # Priority order for channels (prefer processed absorbance data)
  channel_priority <- c("ab_no_atm_comp", "ab", "sc_sample", "sc_ref")
  
  # Extract spectral data from each file (returns list with data and channel)
  spectra_info <- purrr::map(names(raw_data), function(file_name) {
    
    file_data <- raw_data[[file_name]]
    
    # Find first available channel from priority list
    for (channel in channel_priority) {
      
      if (channel %in% names(file_data)) {
        
        # Extract the data (usually in a 'data' element)
        spectral_data <- file_data[[channel]]$data
        
        if (!is.null(spectral_data)) {
          
          # Convert to tibble with wavenumber and absorbance
          # The data is a matrix with wavenumbers as column names
          return(list(
            data = tibble::tibble(
              Sample_ID  = tools::file_path_sans_ext(file_name),
              Wavenumber = as.numeric(colnames(spectral_data)),
              Absorbance = as.numeric(spectral_data)
            ),
            channel = channel
          ))
          
        }
        
      }
      
    }
    
    # Return NULL if no data found
    cli::cli_warn("No spectral data found in file: {.file {file_name}}")
    return(NULL)
    
  })
  
  # Remove NULL entries and extract data and channels separately
  valid_spectra <- purrr::keep(spectra_info, ~!is.null(.))
  spectra_list  <- purrr::map(valid_spectra, "data")
  channels_used <- purrr::map_chr(valid_spectra, "channel")
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Combine all spectra into single tibble
  ## ---------------------------------------------------------------------------
  
  if (length(spectra_list) == 0) {
    
    cli::cli_abort("▶ read_opus_internal: No valid spectral data found in any files")
    
  }
  
  # Bind all spectra together
  spectra_data <- dplyr::bind_rows(spectra_list)
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Check channel consistency and add attributes
  ## ---------------------------------------------------------------------------
  
  unique_channels <- unique(channels_used)
  
  if (length(unique_channels) > 1) {
    
    if (verbose) {
      cli::cli_alert_warning("Multiple channels used across files: {.val {unique_channels}}")
    }
    
  } else if (length(unique_channels) == 1 && verbose) {
    
    cli::cli_alert_info("Channel used: {.val {unique_channels[1]}}")
    
  }
  
  # Add channel as attribute
  attr(spectra_data, "channel_used") <- unique_channels[1]
  
  ## ---------------------------------------------------------------------------
  ## Step 6: Pivot to wide format
  ## ---------------------------------------------------------------------------
  
  # Convert from long to wide format: samples as rows, wavenumbers as columns
  spectra_wide <- spectra_data %>%
    tidyr::pivot_wider(names_from  = Wavenumber,
                       values_from = Absorbance,
                       names_prefix = "")
  
  # Preserve the channel attribute
  attr(spectra_wide, "channel_used") <- unique_channels[1]
  
  ## ---------------------------------------------------------------------------
  ## Step 7: Return spectral data
  ## ---------------------------------------------------------------------------
  
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
#' @param path Character. Path to CSV file
#' @param spectra_type Character. "MIR" or "NIR"
#' @param verbose Logical. Print progress messages
#'
#' @return Tibble with spectral data in wide format
#' @keywords internal
read_csv_internal <- function(path, spectra_type, verbose) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Read CSV file
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_alert_info("Reading CSV file...")
  }
  
  # Read CSV with first row as headers
  safely_execute(
    expr = {
      readr::read_csv(path, 
                      show_col_types = FALSE,
                      progress = verbose)
    },
    default_value = NULL,
    error_message = glue::glue("Failed to read CSV file: {path}")
  ) -> csv_result
  
  csv_data <- csv_result$result
  
  if (is.null(csv_data)) {
    
    if (!is.null(csv_result$error)) {
      
      cli::cli_alert_danger("CSV reading failed with error: {.emph {csv_result$error$message}}")
      cli::cli_alert_info("Check that:")
      cli::cli_ul(c(
        "File exists and is readable: {.path {path}}",
        "File is valid CSV format with proper delimiters",
        "File encoding is correct (try UTF-8)",
        "First column contains Sample_ID values"
      ))
      
    }
    
    return(NULL)
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Validate structure
  ## ---------------------------------------------------------------------------
  
  if (ncol(csv_data) < 2) {
    
    cli::cli_abort("▶ read_csv_internal: CSV must have at least 2 columns (Sample_ID + spectral data)")
    
  }
  
  # Assume first column is Sample_ID
  id_col <- names(csv_data)[1]
  
  if (verbose) {
    cli::cli_alert_info("Using {.val {id_col}} as Sample_ID column")
    cli::cli_alert_info("Found {.val {nrow(csv_data)}} samples with {.val {ncol(csv_data) - 1}} wavelengths")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Rename first column to Sample_ID and validate data types
  ## ---------------------------------------------------------------------------
  
  # Rename first column to Sample_ID for consistency
  names(csv_data)[1] <- "Sample_ID"
  
  # Check that spectral columns are numeric
  spectral_cols <- names(csv_data)[-1]
  
  # Convert spectral columns to numeric if needed
  csv_data <- csv_data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(spectral_cols), as.numeric))
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Validate spectral range
  ## ---------------------------------------------------------------------------
  
  # Try to parse column names as numeric (wavenumbers/wavelengths)
  # Extract numeric wavenumbers from column names
  cleaned_cols <- spectral_cols  # Column names should already be numeric
  x_values <- suppressWarnings(as.numeric(cleaned_cols))
  
  if (all(is.na(x_values))) {
    
    cli::cli_alert_warning("Column names don't appear to be numeric wavenumbers/wavelengths")
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Return data (already in wide format)
  ## ---------------------------------------------------------------------------
  
  return(csv_data)
  
}

## =============================================================================
## Helper Functions
## =============================================================================

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