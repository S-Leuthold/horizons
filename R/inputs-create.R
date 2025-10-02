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
#' * Intelligent ID parsing using customizable format strings
#' * Automatic replicate detection and averaging of spectral data
#' * Flexible joining strategies (inner, left, right, full)
#' * Automatic coordinate column detection and inclusion
#' * Comprehensive data validation and quality control
#'
#' When `parse_ids = TRUE`, sample IDs are parsed according to the
#' `id_format` pattern. Common patterns include:
#' * `"project_sampleid_fraction_scan"` for soil fractionation studies
#' * `"site_plot_depth_replicate"` for field sampling designs
#' * `"experiment_treatment_timepoint"` for time-series studies
#'
#' @param spectra_data `[tibble]` Preprocessed spectral data from preprocess_spectra()
#' @param response_data `[character|data.frame]` Response variables with Sample_ID (file path or data frame)
#' @param response_variables `[character]` (Optional) Which response columns to keep. Default: `NULL` (all)
#' @param id_column `[character]` Column containing IDs to parse or join on. Default: `"Sample_ID"`
#' @param parse_ids `[logical]` Parse complex Sample_IDs into components? Default: `FALSE`
#' @param id_format `[character]` (Optional) Format string like `"project_sampleid_fraction_scan"`
#' @param id_delimiter `[character]` Delimiter in IDs. Default: `"_"`
#' @param aggregate_by `[character]` (Optional) Which parsed columns define unique samples for averaging.
#'   Default: `NULL` (uses all parsed columns except 'scan' if present)
#' @param join_by `[character]` (Optional) Column(s) to join with response data.
#'   Default: `NULL` (auto-determined from aggregate_by or id_column)
#' @param include_coords `[logical]` Include coordinate columns? Default: `TRUE`
#' @param coord_columns `[character]` (Optional) Explicit coord column names. Default: `NULL` (auto-detect)
#' @param join_type `[character]` Join strategy for combining spectra and response data.
#'   Default: `"inner"` (recommended for modeling - only samples with both spectra and response).
#'   * `"inner"`: Keep only samples with both spectra AND response (safe for modeling)
#'   * `"left"`: Keep all spectra, even without response (creates NAs - may cause downstream failures)
#'   * `"right"`: Keep all response samples, even without spectra (creates NAs in predictors)
#'   * `"full"`: Keep everything (creates NAs - use only for data auditing, not modeling)
#'
#'   Note: Non-inner joins may introduce NA values that will cause model evaluation to fail
#'   unless `drop_na = TRUE` or data is filtered manually.
#' @param drop_na `[logical]` Drop rows with NA in response variables? Default: `TRUE`
#' @param verbose `[logical]` Print progress messages. Default: `TRUE`
#'
#' @return A `[tibble]` containing:
#'   * `Sample_ID`: Character. Primary sample identifier (post-aggregation)
#'   * `<spectral_cols>`: Numeric columns containing averaged spectral data
#'   * `<response_vars>`: Selected response variables from response_data
#'   * `<coord_cols>`: Spatial coordinates (if include_coords = TRUE)
#'   * `n_replicates`: Integer. Number of spectral replicates averaged (if applicable)
#'   * `<parsed_cols>`: Additional columns from ID parsing (if parse_ids = TRUE)
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
#' [preprocess_spectra()] for spectral preprocessing,
#' [create_configs()] for model configuration setup,
#' [finalize_dataset()] for outlier detection
#'
#' @family inputs
#' @keywords spectroscopy data-integration
#'
#' @importFrom dplyr select filter mutate group_by summarise across bind_cols bind_rows left_join right_join inner_join full_join all_of matches if_all first n
#' @importFrom purrr map compact
#' @importFrom readr read_csv
#' @importFrom cli cli_abort cli_alert_warning cli_text cli_alert_info
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
  ## This is a specialized parameter for lab workflows with replicate scans

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

    ## Preserve original IDs----------------------------------------------------

    spectra_data$Original_ID <- spectra_data[[id_column]]

    ## Parse each ID and get all components ------------------------------------

    purrr::map(spectra_data[[id_column]],
               function(id) {

                 parse_filename_metadata(file_name        = id,
                                         format_string    = id_format,
                                         delimiter        = id_delimiter,
                                         default_fraction = "bulk")
                 }
               ) -> parsed_list

    ## Bind all parsed metadata as new columns ---------------------------------

    parsed_df <- dplyr::bind_rows(parsed_list)

    ## Check if parsing actually worked (all unknowns means format mismatch) ---

    if (all(parsed_df$Sample_ID == "UNKNOWN")) {

      example_ids <- head(spectra_data[[id_column]], 3)

      cli::cli_abort(c("ID parsing failed - all samples marked as UNKNOWN",
                       "x" = "Format string doesn't match ID structure",
                       "i" = "Format string: {.val {id_format}}",
                       "i" = "Delimiter: {.val {id_delimiter}}",
                       "i" = "Example IDs: {.val {example_ids}}",
                       "i" = "Ensure format tokens (e.g., 'sampleid') match your ID structure"))
      }

    ## Remove the original id_column to avoid conflicts (we have it in Original_ID)

    spectra_data %>%
      dplyr::select(-dplyr::all_of(id_column)) -> spectra_data

    ## Add parsed columns to spectra_data --------------------------------------

    spectra_data <- dplyr::bind_cols(spectra_data, parsed_df)

    ## Determine what to aggregate by ------------------------------------------

    if (is.null(aggregate_by)) {

      parsed_cols  <- names(parsed_df)
      aggregate_by <- parsed_cols[parsed_cols != "Scan"]

      if (verbose) cli::cli_text("│  └─ Aggregating by: {paste(aggregate_by, collapse = ', ')}")

    }

  } else {

    if (is.null(aggregate_by)) aggregate_by <- id_column

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Aggregate replicates by averaging spectra
  ## ---------------------------------------------------------------------------

  ## Track sample counts for reporting -----------------------------------------

  n_before <- nrow(spectra_data)

  ## Report replicate detection ------------------------------------------------

  if (verbose) {

    spectra_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(aggregate_by))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1) -> replicate_counts

    cli::cli_text("├─ Data Aggregation")

    if (nrow(replicate_counts) > 0) cli::cli_text("│  ├─ Found {nrow(replicate_counts)} sample groups with replicates to average")

  }

  ## Validate aggregate_by columns exist ---------------------------------------

  missing_agg_cols <- aggregate_by[!aggregate_by %in% names(spectra_data)]

  if (length(missing_agg_cols) > 0) cli::cli_abort("Aggregation columns not found: {.val {missing_agg_cols}}")

  ## Identify column types for aggregation -------------------------------------

  spectral_pattern <- "^[0-9]+(\\.[0-9]+)?$"
  all_col_names    <- names(spectra_data)
  spectral_cols    <- all_col_names[grepl(spectral_pattern, all_col_names)]
  exclude_cols     <- c(aggregate_by, spectral_cols)
  other_cols       <- all_col_names[!all_col_names %in% exclude_cols]

  ## Aggregate by averaging spectral columns ----------------------------------

  spectra_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(aggregate_by))) %>%
    dplyr::summarise(dplyr::across(dplyr::matches(spectral_pattern), ~mean(.x, na.rm = TRUE)),
                     n_replicates = dplyr::n(),
                     dplyr::across(dplyr::all_of(other_cols), ~dplyr::first(.x)),
                     .groups = "drop") -> spectra_data

  ## Report aggregation results ------------------------------------------------

  if (verbose) {

    n_after <- nrow(spectra_data)
    cli::cli_text("│  └─ ✓ Aggregated {n_before} rows into {n_after} unique samples")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Join with response data
  ## ---------------------------------------------------------------------------

  ## Determine join column -----------------------------------------------------

  join_column <- if (parse_ids) "Sample_ID" else id_column

  ## Validate join column exists in both datasets ------------------------------

  if (!join_column %in% names(spectra_data)) cli::cli_abort("Join column {.val {join_column}} not found in spectra data after processing")


  if (!join_column %in% names(response_data)) cli::cli_abort("Join column {.val {join_column}} not found in response data")


    ## -------------------------------------------------------------------------
    ## Step 3.1: Handle coordinate columns
    ## -------------------------------------------------------------------------

    coord_columns_to_keep <- character(0)

    if (include_coords) {

      ## Auto-detect or use explicit coordinate columns ------------------------

      if (is.null(coord_columns)) {

        coord_patterns <- c("Lat", "Lon", "lat", "lon", "Latitude", "Longitude",
                           "latitude", "longitude", "X", "Y", "x", "y")

        coord_columns <- names(response_data)[names(response_data) %in% coord_patterns]

        if (length(coord_columns) > 0 && verbose) {

          cli::cli_text("├─ Coordinate Detection")
          cli::cli_text("│  └─ Auto-detected columns: {paste(coord_columns, collapse = ', ')}")

        }

      }

      ## Validate coordinate columns exist -------------------------------------

      coord_columns_to_keep <- coord_columns[coord_columns %in% names(response_data)]

      if (length(coord_columns_to_keep) == 0 && length(coord_columns) > 0) {

        cli::cli_alert_warning("Coordinate columns {.val {coord_columns}} not found in response data")
        cli::cli_alert_info("Coordinates will not be included in final dataset")

      }

    }

    ## -------------------------------------------------------------------------
    ## Step 3.2: Select response variables
    ## -------------------------------------------------------------------------

    if (!is.null(response_variables)) {

      ## Check for missing variables -------------------------------------------

      missing_vars <- response_variables[!response_variables %in% names(response_data)]

      if (length(missing_vars) > 0) {

        cli::cli_warn("Variables not found in response data: {.val {missing_vars}}")

        response_variables <- response_variables[response_variables %in% names(response_data)]

        if (length(response_variables) == 0) {
          cli::cli_warn("No valid response variables found - setting to NULL")
          response_variables <- NULL
        }

      }

      ## Subset to requested columns -------------------------------------------

      if (!is.null(response_variables)) {

        keep_cols     <- unique(c(join_column, response_variables, coord_columns_to_keep))
        response_data <- response_data[, keep_cols[keep_cols %in% names(response_data)]]

      }

    }

    ## -------------------------------------------------------------------------
    ## Step 3.3: Perform join
    ## -------------------------------------------------------------------------

    if (verbose) {

      cli::cli_text("├─ Data Integration")
      cli::cli_text("│  └─ Using {join_type} join on {join_column}")

    }

    switch(join_type,
           inner = dplyr::inner_join(spectra_data, response_data, by = join_column),
           left  = dplyr::left_join(spectra_data, response_data, by = join_column),
           right = dplyr::right_join(spectra_data, response_data, by = join_column),
           full  = dplyr::full_join(spectra_data, response_data, by = join_column)) -> result

  ## ---------------------------------------------------------------------------
  ## Step 5: Data Cleaning
  ## ---------------------------------------------------------------------------

  n_na_removed <- 0

  if (drop_na && !is.null(response_variables)) {

    if (verbose) {
      cli::cli_text("├─ Data cleaning")
    }

    n_before_na <- nrow(result)

    result %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(response_variables), ~!is.na(.x))) -> result

    n_na_removed <- n_before_na - nrow(result)

    if (verbose && n_na_removed > 0) {
      cli::cli_text("│  └─ Removed {n_na_removed} row{?s} with NA in response variables")
    }

  }

  ## Display final results summary --------------------------------------------

  if (verbose) {

    ## Calculate join statistics ----------------------------------------------

    n_spectra_dropped  <- sum(!spectra_data[[join_column]] %in% response_data[[join_column]])
    n_response_dropped <- sum(!response_data[[join_column]] %in% spectra_data[[join_column]])

    ## Count column types -----------------------------------------------------

    all_column_names <- names(result)
    is_numeric_name  <- !is.na(suppressWarnings(as.numeric(all_column_names)))
    spectral_cols    <- sum(is_numeric_name)
    response_cols    <- ncol(result) - spectral_cols - 1

    cli::cli_text("{.strong Dataset Creation Complete}")
    cli::cli_text("├─ Final samples: {nrow(result)}")
    cli::cli_text("├─ Total columns: {ncol(result)}")
    cli::cli_text("├─ Spectral features: {spectral_cols}")
    cli::cli_text("├─ Response variables: {response_cols}")

    if (join_type != "inner" && n_spectra_dropped > 0) {
      cli::cli_text("├─ Spectra dropped (no response match): {n_spectra_dropped}")
    }
    if (join_type != "inner" && n_response_dropped > 0) {
      cli::cli_text("├─ Response dropped (no spectra match): {n_response_dropped}")
    }
    if (n_na_removed > 0) {
      cli::cli_text("├─ Rows with NA removed: {n_na_removed}")
    }

    cli::cli_text("└─ Status: Complete")

  }

  return(result)

}
