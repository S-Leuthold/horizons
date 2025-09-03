#' Create Dataset for Modeling
#'
#' @description
#' Combines preprocessed spectral data with response variables to create
#' a dataset ready for modeling. Handles ID parsing, replicate averaging,
#' and coordinate inclusion.
#'
#' @param spectra_data Tibble. Preprocessed spectral data from preprocess_spectra()
#' @param response_data Character path or data.frame. Response variables with Sample_ID
#' @param response_variables Character vector. Which response columns to keep (NULL = all)
#' @param id_column Character. Column containing IDs to parse or join on. Default: "Sample_ID"
#' @param parse_ids Logical. Parse complex Sample_IDs into components? Default: FALSE
#' @param id_format Character. Format string like "project_sampleid_fraction_scan"
#' @param id_delimiter Character. Delimiter in IDs. Default: "_"
#' @param aggregate_by Character vector. Which parsed columns define unique samples for averaging.
#'   Default: NULL (uses all parsed columns except 'scan' if present)
#' @param join_by Character vector. Column(s) to join with response data.
#'   Default: NULL (auto-determined from aggregate_by or id_column)
#' @param include_coords Logical. Include coordinate columns? Default: TRUE
#' @param coord_columns Character vector. Explicit coord column names (NULL = auto-detect)
#' @param join_type Character. Type of join: "inner", "left", "right", "full". Default: "inner"
#' @param drop_na Logical. Drop rows with NA in response variables? Default: TRUE
#' @param verbose Logical. Print progress messages. Default: TRUE
#'
#' @return A tibble with joined spectral and response data
#'
#' @export
create_dataset <- function(spectra_data,
                          response_data,
                          response_variables = NULL,
                          id_column          = "Sample_ID",
                          parse_ids          = FALSE,
                          id_format          = NULL,
                          id_delimiter       = "_",
                          aggregate_by       = NULL,
                          join_by            = NULL,
                          include_coords     = TRUE,
                          coord_columns      = NULL,
                          join_type          = "inner",
                          drop_na            = TRUE,
                          verbose            = TRUE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------
  
  if (!is.data.frame(spectra_data)) {
    
    cli::cli_abort("▶ create_dataset: spectra_data must be a data frame or tibble")
    
  }
  
  if (!id_column %in% names(spectra_data)) {
    
    cli::cli_abort("▶ create_dataset: Column {.val {id_column}} not found in spectra_data")
    
  }
  
  # Handle response_data as either path or dataframe
  if (is.character(response_data)) {
    
    if (!file.exists(response_data)) {
      
      cli::cli_abort("▶ create_dataset: Response file not found: {.path {response_data}}")
      
    }
    
    safely_execute(
      expr = {
        readr::read_csv(response_data, show_col_types = FALSE)
      },
      default_value = NULL,
      error_message = glue::glue("Failed to read response data file: {response_data}")
    ) -> response_result
    
    if (is.null(response_result$result)) {
      
      if (!is.null(response_result$error)) {
        
        cli::cli_alert_danger("Response data reading failed: {.emph {response_result$error$message}}")
        cli::cli_alert_info("Check that:")
        cli::cli_ul(c(
          "Response file exists and is readable: {.path {response_data}}",
          "File is valid CSV format",
          "File contains required {.val {id_column}} column",
          "File encoding is correct (try UTF-8)"
        ))
        
      }
      
      cli::cli_abort("▶ create_dataset: Failed to read response data - cannot continue")
      
    }
    
    response_data <- response_result$result
    
  } else if (!is.data.frame(response_data)) {
    
    cli::cli_abort("▶ create_dataset: response_data must be a file path or data frame")
    
  }
  
  if (!id_column %in% names(response_data)) {
    
    cli::cli_abort("▶ create_dataset: Column {.val {id_column}} not found in response_data")
    
  }
  
  # Validate parse_ids parameters
  if (parse_ids && is.null(id_format)) {
    
    cli::cli_abort("▶ create_dataset: id_format must be provided when parse_ids = TRUE")
    
  }
  
  # Validate aggregate_by - only makes sense with parse_ids
  if (!is.null(aggregate_by) && !parse_ids) {
    
    cli::cli_abort("▶ create_dataset: aggregate_by requires parse_ids = TRUE")
    
  }
  
  # Validate join_type
  join_type <- match.arg(join_type, c("inner", "left", "right", "full"))
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Parse IDs if requested
  ## ---------------------------------------------------------------------------
  
  if (parse_ids) {
    
    if (verbose) {
      cli::cli_alert_info("Parsing Sample IDs using format: {.val {id_format}}")
    }
    
    # Preserve original IDs
    spectra_data$Original_ID <- spectra_data[[id_column]]
    
    # Parse each ID and get all components
    safely_execute(
      expr = {
        parsed_list <- purrr::map(spectra_data[[id_column]], function(id) {
          parse_filename_metadata(file_name        = id,
                                 format_string    = id_format,
                                 delimiter        = id_delimiter,
                                 default_fraction = "bulk")
        })
        
        # Bind all parsed metadata as new columns
        dplyr::bind_rows(parsed_list)
      },
      default_value = NULL,
      error_message = "ID parsing failed"
    ) -> parsing_result
    
    if (is.null(parsing_result$result)) {
      
      if (!is.null(parsing_result$error)) {
        
        cli::cli_alert_danger("ID parsing failed: {.emph {parsing_result$error$message}}")
        cli::cli_alert_info("Check that:")
        cli::cli_ul(c(
          "ID format string matches actual ID structure: {.val {id_format}}",
          "Delimiter is correct: {.val {id_delimiter}}",
          "All IDs follow consistent naming pattern",
          "Required tokens (e.g., sampleid) are present in format"
        ))
        cli::cli_alert_info("Example IDs: {.val {head(spectra_data[[id_column]], 3)}}")
        
      }
      
      cli::cli_abort("▶ create_dataset: ID parsing failed - cannot continue")
      
    }
    
    parsed_df <- parsing_result$result
    
    # Remove the original id_column to avoid conflicts (we have it in Original_ID)
    spectra_data <- spectra_data %>%
      dplyr::select(-dplyr::all_of(id_column))
    
    # Add parsed columns to spectra_data
    spectra_data <- dplyr::bind_cols(spectra_data, parsed_df)
    
    # Determine what to aggregate by
    if (is.null(aggregate_by)) {
      # Default: use all parsed columns except "Scan" if it exists
      parsed_cols <- names(parsed_df)
      aggregate_by <- parsed_cols[parsed_cols != "Scan"]
      
      if (verbose) {
        cli::cli_alert_info("Aggregating by: {.val {aggregate_by}}")
      }
    }
    
  } else {
    
    # No parsing - aggregate by the original id_column
    if (is.null(aggregate_by)) {
      aggregate_by <- id_column
    }
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Aggregate replicates by averaging spectra
  ## ---------------------------------------------------------------------------
  
  # Count replicates before aggregation
  if (verbose) {
    n_before <- nrow(spectra_data)
    
    replicate_counts <- spectra_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(aggregate_by))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1)
    
    if (nrow(replicate_counts) > 0) {
      cli::cli_alert_info("Found {.val {nrow(replicate_counts)}} sample groups with replicates to average")
    }
  }
  
  # Check that aggregate_by columns exist in the data
  missing_agg_cols <- aggregate_by[!aggregate_by %in% names(spectra_data)]
  if (length(missing_agg_cols) > 0) {
    cli::cli_abort("▶ create_dataset: Aggregation columns not found: {.val {missing_agg_cols}}")
  }
  
  # Identify other columns to preserve (not aggregation columns or wavenumbers)
  other_cols <- names(spectra_data)[!names(spectra_data) %in% c(aggregate_by, names(spectra_data)[grepl("^wn_", names(spectra_data))])]
  
  # Aggregate by averaging spectral columns
  spectra_data <- spectra_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(aggregate_by))) %>%
    dplyr::summarise(
      # Average all wavenumber columns
      dplyr::across(dplyr::starts_with("wn_"), ~mean(.x, na.rm = TRUE)),
      # Count replicates
      n_replicates = dplyr::n(),
      # Keep first value of other columns if they exist
      dplyr::across(dplyr::all_of(other_cols), ~dplyr::first(.x)),
      .groups = "drop"
    )
  
  if (verbose) {
    n_after <- nrow(spectra_data)
    cli::cli_alert_success("Aggregated {.val {n_before}} rows into {.val {n_after}} unique samples")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Join with response data
  ## ---------------------------------------------------------------------------
  
  # Determine join column - always Sample_ID if parsed, otherwise id_column
  join_column <- if (parse_ids) "Sample_ID" else id_column
  
  # Check that join column exists in both datasets
  if (!join_column %in% names(spectra_data)) {
    cli::cli_abort("▶ create_dataset: Join column {.val {join_column}} not found in spectra data after processing")
  }
  
  if (!join_column %in% names(response_data)) {
    cli::cli_abort("▶ create_dataset: Join column {.val {join_column}} not found in response data")
  }
  
  # Select response variables if specified
  if (!is.null(response_variables)) {
    
    # Check that requested variables exist
    missing_vars <- response_variables[!response_variables %in% names(response_data)]
    if (length(missing_vars) > 0) {
      cli::cli_warn("Variables not found in response data: {.val {missing_vars}}")
    }
    
    # Keep join column and requested variables
    keep_cols <- unique(c(join_column, response_variables))
    response_data <- response_data[, keep_cols[keep_cols %in% names(response_data)]]
    
  }
  
  # Handle coordinate columns if requested
  if (include_coords) {
    
    if (is.null(coord_columns)) {
      # Auto-detect coordinate columns
      coord_patterns <- c("Lat", "Lon", "lat", "lon", "Latitude", "Longitude", 
                         "latitude", "longitude", "X", "Y", "x", "y")
      coord_columns <- names(response_data)[names(response_data) %in% coord_patterns]
      
      if (length(coord_columns) > 0 && verbose) {
        cli::cli_alert_info("Auto-detected coordinate columns: {.val {coord_columns}}")
      }
    }
    
    # Add coord columns to keep list if not already included
    if (length(coord_columns) > 0 && !is.null(response_variables)) {
      response_data <- response_data[, unique(c(join_column, response_variables, coord_columns))]
    }
  }
  
  # Perform the join
  if (verbose) {
    cli::cli_alert_info("Joining spectra with response data using {.val {join_type}} join on {.val {join_column}}")
  }
  
  result <- switch(join_type,
    inner = dplyr::inner_join(spectra_data, response_data, by = join_column),
    left  = dplyr::left_join(spectra_data, response_data, by = join_column),
    right = dplyr::right_join(spectra_data, response_data, by = join_column),
    full  = dplyr::full_join(spectra_data, response_data, by = join_column)
  )
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Handle NAs and finalize
  ## ---------------------------------------------------------------------------
  
  # Drop rows with NA in response variables if requested
  if (drop_na && !is.null(response_variables)) {
    
    n_before_na <- nrow(result)
    result <- result %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(response_variables), ~!is.na(.x)))
    n_after_na <- nrow(result)
    
    if (verbose && n_before_na > n_after_na) {
      cli::cli_alert_warning("Dropped {.val {n_before_na - n_after_na}} rows with NA in response variables")
    }
    
  }
  
  # Final summary
  if (verbose) {
    cli::cli_alert_success("Created dataset with {.val {nrow(result)}} samples and {.val {ncol(result)}} columns")
    
    # Report on join results
    n_spectra_only <- sum(!spectra_data[[join_column]] %in% response_data[[join_column]])
    n_response_only <- sum(!response_data[[join_column]] %in% spectra_data[[join_column]])
    
    if (n_spectra_only > 0) {
      cli::cli_alert_info("{.val {n_spectra_only}} spectra samples had no matching response data")
    }
    if (n_response_only > 0) {
      cli::cli_alert_info("{.val {n_response_only}} response samples had no matching spectra")
    }
  }
  
  return(result)
  
}