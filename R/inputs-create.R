#' Create Dataset for Modeling
#'
#' @description
#' Combines preprocessed spectral data with response variables to create
#' a modeling-ready dataset. This function handles complex sample ID parsing,
#' replicate aggregation, and spatial coordinate management. It serves as the
#' final step in the data preparation pipeline before model evaluation.
#'
#' @details
#' The function provides flexible data integration with several key capabilities:
#' \itemize{
#'   \item Intelligent ID parsing using customizable format strings
#'   \item Automatic replicate detection and averaging of spectral data
#'   \item Flexible joining strategies (inner, left, right, full)
#'   \item Automatic coordinate column detection and inclusion
#'   \item Comprehensive data validation and quality control
#' }
#'
#' When \code{parse_ids = TRUE}, sample IDs are parsed according to the
#' \code{id_format} pattern. Common patterns include:
#' \itemize{
#'   \item \code{"project_sampleid_fraction_scan"} for soil fractionation studies
#'   \item \code{"site_plot_depth_replicate"} for field sampling designs
#'   \item \code{"experiment_treatment_timepoint"} for time-series studies
#' }
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
#' @param join_type Character. Join strategy for combining spectra and response data.
#'   Default: "inner" (recommended for modeling - only samples with both spectra and response).
#'   \itemize{
#'     \item \code{"inner"}: Keep only samples with both spectra AND response (safe for modeling)
#'     \item \code{"left"}: Keep all spectra, even without response (creates NAs - may cause downstream failures)
#'     \item \code{"right"}: Keep all response samples, even without spectra (creates NAs in predictors)
#'     \item \code{"full"}: Keep everything (creates NAs - use only for data auditing, not modeling)
#'   }
#'   Note: Non-inner joins may introduce NA values that will cause model evaluation to fail
#'   unless \code{drop_na = TRUE} or data is filtered manually.
#' @param drop_na Logical. Drop rows with NA in response variables? Default: TRUE
#' @param verbose Logical. Print progress messages. Default: TRUE
#'
#' @return A tibble containing the merged spectral and response data with:
#'   \describe{
#'     \item{Sample_ID}{Character. Primary sample identifier (post-aggregation)}
#'     \item{<spectral_cols>}{Numeric columns containing averaged spectral data}
#'     \item{<response_vars>}{Selected response variables from response_data}
#'     \item{<coord_cols>}{Spatial coordinates (if include_coords = TRUE)}
#'     \item{n_replicates}{Integer. Number of spectral replicates averaged (if applicable)}
#'     \item{<parsed_cols>}{Additional columns from ID parsing (if parse_ids = TRUE)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic dataset creation
#' dataset <- create_dataset(
#'   spectra_data = preprocessed_spectra,
#'   response_data = "soil_properties.csv",
#'   response_variables = c("SOC", "clay", "pH")
#' )
#'
#' # Advanced: ID parsing and coordinate inclusion
#' dataset <- create_dataset(
#'   spectra_data = spectra,
#'   response_data = response_df,
#'   parse_ids = TRUE,
#'   id_format = "site_plot_depth_rep",
#'   aggregate_by = c("site", "plot", "depth"),
#'   include_coords = TRUE,
#'   coord_columns = c("latitude", "longitude")
#' )
#' }
#'
#' @seealso
#' \code{\link{preprocess_spectra}} for spectral preprocessing,
#' \code{\link{create_configs}} for model configuration setup,
#' \code{\link{finalize_dataset}} for outlier detection
#'
#' @family inputs
#' @keywords spectroscopy data-integration
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
  ## Step 0: Input Validation
  ## ---------------------------------------------------------------------------

  ## Make sure spectra are a dataframe -----------------------------------------

  if (!is.data.frame(spectra_data)) cli::cli_abort("spectra_data must be a data frame or tibble")

  ## Check id_column exists in spectra_data ------------------------------------

  if (!id_column %in% names(spectra_data)) cli::cli_abort("Column {.val {id_column}} not found in spectra_data")

  ## Handle response_data as either path or dataframe --------------------------

  if (is.character(response_data)) {

    if (!file.exists(response_data)) cli::cli_abort("Response file not found: {.path {response_data}}")

    tryCatch({

      readr::read_csv(response_data, show_col_types = FALSE)

      }, error = function(e) {

        cli::cli_abort(c("Failed to read response data file: {.path {response_data}}",
                         "x" = e$message,
                         "i" = "Check that file exists and is valid CSV format",
                         "i" = "Ensure file contains {.val {id_column}} column",
                         "i" = "Try UTF-8 encoding if file appears corrupted"))
        }
      ) -> response_data

    } else if (!is.data.frame(response_data)) cli::cli_abort("response_data must be a file path or data frame")

  ## Make sure the response data has the sample_ids ----------------------------

  if (!id_column %in% names(response_data)) cli::cli_abort("Column {.val {id_column}} not found in response_data")

  ## Validate parse_ids and id_format ------------------------------------------

  if (parse_ids && is.null(id_format)) cli::cli_abort("id_format must be provided when parse_ids = TRUE")

  ## Validate aggregate_by - only makes sense with parse_ids -------------------

  ## This is kind of a specialized use case, I think maybe it's worth deliberating
  ## on if this type of thing is worth keeping? I think for the internal lab use, it is.

  if (!is.null(aggregate_by) && !parse_ids) cli::cli_abort("aggregate_by requires parse_ids = TRUE")

  ## Validate join_type --------------------------------------------------------

  join_type <- match.arg(join_type, c("inner", "left", "right", "full"))

  ## Display configuration summary ---------------------------------------------

  if (verbose) {
    cli::cli_text("{.strong Dataset Creation Pipeline}")
    cli::cli_text("├─ Input samples: {nrow(spectra_data)}")
    cli::cli_text("├─ Response source: {if (is.character(response_data)) 'CSV file' else 'Data frame'}")
    cli::cli_text("├─ ID parsing: {if (parse_ids) paste0('Enabled (', id_format, ')') else 'Disabled'}")
    cli::cli_text("├─ Join strategy: {join_type} join on {id_column}")
    cli::cli_text("└─ Coordinate inclusion: {if (include_coords) 'Enabled' else 'Disabled'}")
    cli::cli_text("")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Parse IDs if requested
  ## ---------------------------------------------------------------------------

  if (parse_ids) {

    if (verbose) {
      cli::cli_text("{.strong ID Processing Pipeline}")
      cli::cli_text("├─ ID Parsing")
      cli::cli_text("│  ├─ Format: {id_format}")
    }

    # Preserve original IDs
    spectra_data$Original_ID <- spectra_data[[id_column]]

    # Parse each ID and get all components
    # Note: parse_filename_metadata never errors, just returns defaults
    parsed_list <- purrr::map(spectra_data[[id_column]], function(id) {
      parse_filename_metadata(file_name        = id,
                             format_string    = id_format,
                             delimiter        = id_delimiter,
                             default_fraction = "bulk")
    })

    # Bind all parsed metadata as new columns
    parsed_df <- dplyr::bind_rows(parsed_list)

    # Check if parsing actually worked (all unknowns means format mismatch)
    if (all(parsed_df$Sample_ID == "UNKNOWN")) {
      example_ids <- head(spectra_data[[id_column]], 3)

      cli::cli_abort(c(
        "ID parsing failed - all samples marked as UNKNOWN",
        "x" = "Format string doesn't match ID structure",
        "i" = "Format string: {.val {id_format}}",
        "i" = "Delimiter: {.val {id_delimiter}}",
        "i" = "Example IDs: {.val {example_ids}}",
        "i" = "Ensure format tokens (e.g., 'sampleid') match your ID structure"
      ))
    }

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
        cli::cli_text("│  └─ Aggregating by: {paste(aggregate_by, collapse = ', ')}")
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

    cli::cli_text("├─ Data Aggregation")
    if (nrow(replicate_counts) > 0) {
      cli::cli_text("│  ├─ Found {nrow(replicate_counts)} sample groups with replicates to average")
    }
  }

  # Check that aggregate_by columns exist in the data
  missing_agg_cols <- aggregate_by[!aggregate_by %in% names(spectra_data)]
  if (length(missing_agg_cols) > 0) {
    cli::cli_abort("Aggregation columns not found: {.val {missing_agg_cols}}")
  }

  # Identify other columns to preserve (not aggregation columns or wavenumbers)
  # Identify spectral columns (numeric names)
  spectral_pattern <- "^[0-9]+(\\.[0-9]+)?$"
  other_cols <- names(spectra_data)[!names(spectra_data) %in% c(aggregate_by, names(spectra_data)[grepl(spectral_pattern, names(spectra_data))])]

  # Aggregate by averaging spectral columns
  spectra_data <- spectra_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(aggregate_by))) %>%
    dplyr::summarise(
      # Average all spectral columns (numeric names)
      dplyr::across(dplyr::matches(spectral_pattern), ~mean(.x, na.rm = TRUE)),
      # Count replicates
      n_replicates = dplyr::n(),
      # Keep first value of other columns if they exist
      dplyr::across(dplyr::all_of(other_cols), ~dplyr::first(.x)),
      .groups = "drop"
    )

  if (verbose) {
    n_after <- nrow(spectra_data)
    cli::cli_text("│  └─ ✓ Aggregated {n_before} rows into {n_after} unique samples")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Join with response data
  ## ---------------------------------------------------------------------------

  # Determine join column - always Sample_ID if parsed, otherwise id_column
  join_column <- if (parse_ids) "Sample_ID" else id_column

  # Check that join column exists in both datasets
  if (!join_column %in% names(spectra_data)) {
    cli::cli_abort("Join column {.val {join_column}} not found in spectra data after processing")
  }

  if (!join_column %in% names(response_data)) {
    cli::cli_abort("Join column {.val {join_column}} not found in response data")
  }

  # Handle coordinate columns FIRST (before subsetting)
  coord_columns_to_keep <- character(0)
  if (include_coords) {

    if (is.null(coord_columns)) {
      # Auto-detect coordinate columns
      coord_patterns <- c("Lat", "Lon", "lat", "lon", "Latitude", "Longitude",
                         "latitude", "longitude", "X", "Y", "x", "y")
      coord_columns <- names(response_data)[names(response_data) %in% coord_patterns]

      if (length(coord_columns) > 0 && verbose) {
        cli::cli_text("├─ Coordinate Detection")
        cli::cli_text("│  └─ Auto-detected columns: {paste(coord_columns, collapse = ', ')}")
      }
    }

    # Check which coordinate columns actually exist
    coord_columns_to_keep <- coord_columns[coord_columns %in% names(response_data)]

    if (length(coord_columns_to_keep) == 0 && length(coord_columns) > 0) {
      cli::cli_alert_warning("Coordinate columns {.val {coord_columns}} not found in response data")
      cli::cli_alert_info("Coordinates will not be included in final dataset")
    }
  }

  # Select response variables if specified
  if (!is.null(response_variables)) {

    # Check that requested variables exist
    missing_vars <- response_variables[!response_variables %in% names(response_data)]
    if (length(missing_vars) > 0) {
      cli::cli_warn("Variables not found in response data: {.val {missing_vars}}")
      # Update response_variables to only include existing ones for downstream use
      response_variables <- response_variables[response_variables %in% names(response_data)]

      if (length(response_variables) == 0) {
        cli::cli_warn("No valid response variables found - setting to NULL")
        response_variables <- NULL
      }
    }

    # Keep join column, requested variables, AND coordinate columns
    if (!is.null(response_variables)) {
      keep_cols <- unique(c(join_column, response_variables, coord_columns_to_keep))
      response_data <- response_data[, keep_cols[keep_cols %in% names(response_data)]]
    }

  }

  ## Perform the join -------------------------------------------------------------
  if (verbose) {
    cli::cli_text("├─ Data Integration")
    cli::cli_text("│  └─ Using {join_type} join on {join_column}")
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

  # Display final results summary
  if (verbose) {

    # Calculate join statistics
    n_spectra_only <- sum(!spectra_data[[join_column]] %in% response_data[[join_column]])
    n_response_only <- sum(!response_data[[join_column]] %in% spectra_data[[join_column]])

    # Count spectral and response columns
    spectral_cols <- sum(suppressWarnings(!is.na(as.numeric(names(result)))))
    response_cols <- ncol(result) - spectral_cols - 1  # Subtract Sample_ID

    cli::cli_text("")
    cli::cli_text("{.strong Dataset Creation Complete}")
    cli::cli_text("├─ Final samples: {nrow(result)}")
    cli::cli_text("├─ Total columns: {ncol(result)}")
    cli::cli_text("├─ Spectral features: {spectral_cols}")
    cli::cli_text("├─ Response variables: {response_cols}")
    if (n_spectra_only > 0) {
      cli::cli_text("├─ Spectra without response: {n_spectra_only}")
    }
    if (n_response_only > 0) {
      cli::cli_text("├─ Response without spectra: {n_response_only}")
    }
    cli::cli_text("└─ Status: Complete")
    cli::cli_text("")

  }

  return(result)

}
