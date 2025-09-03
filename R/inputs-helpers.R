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
  raw_data <- suppressWarnings(
    opusreader2::read_opus(dsn = path)
  )
  
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
                       names_prefix = "wn_")
  
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
  )$result -> csv_data
  
  if (is.null(csv_data)) {
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
  # Handle "wn_" or "nm_" prefixes if present
  cleaned_cols <- gsub("^(wn_|nm_)", "", spectral_cols)
  x_values <- suppressWarnings(as.numeric(cleaned_cols))
  
  if (all(is.na(x_values))) {
    
    cli::cli_alert_warning("Column names don't appear to be numeric wavenumbers/wavelengths")
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Return data (already in wide format)
  ## ---------------------------------------------------------------------------
  
  return(csv_data)
  
}