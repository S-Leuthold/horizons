#' Fetch Environmental Covariates from Spectra and Coordinates
#'
#' Orchestrates prediction of soil properties from MIR spectra and retrieval of
#' climate data from geographic coordinates. This function serves as the central
#' hub for augmenting spectral datasets with environmental covariates, either
#' using pre-built configurations or direct specification.
#'
#' @param input_data `[data.frame]` Input dataset with required columns:
#'   * `Sample_ID`: Unique sample identifiers
#'   * Numeric columns: MIR wavenumbers (e.g., `"600"`, `"650"`)
#'   * `Longitude`: Decimal degrees (-180 to 180) if climate data needed
#'   * `Latitude`: Decimal degrees (-90 to 90) if climate data needed
#' @param configurations `[data.frame]` (Optional) Configuration table from
#'   `create_configs()`. Takes precedence over individual covariate parameters.
#'   Default: `NULL`.
#' @param soil_covariates `[character]` (Optional) Soil properties to predict
#'   (e.g., `"clay"`, `"sand"`, `"ph"`, `"oc"`, `"n"`). Ignored if configurations
#'   provided. Default: `NULL`.
#' @param climate_covariates `[character]` (Optional) Climate variables to fetch
#'   (e.g., `"MAT"`, `"MAP"`, `"AI"`, `"PET"`). Ignored if configurations
#'   provided. Default: `NULL`.
#' @param spatial_covariates `[character]` (Optional) Spatial covariates - not yet
#'   implemented. Default: `NULL`.
#' @param n_similar `[integer]` Number of OSSL samples for training soil models.
#'   Default: `35000`.
#' @param variance_threshold `[numeric]` PCA variance to retain (0-1).
#'   Default: `0.99`.
#' @param bayesian_iter `[integer]` Bayesian optimization iterations. Use 0 for
#'   grid search only. Default: `10`.
#' @param prop_train `[numeric]` Training proportion for train/val split (0-1).
#'   Default: `0.85`.
#' @param climate_start_year `[integer]` Start year for Daymet climate data.
#'   Default: `2003`.
#' @param climate_end_year `[integer]` End year for Daymet climate data.
#'   Default: `2024`.
#' @param allow_par `[logical]` Enable parallel processing? Default: `FALSE`.
#' @param n_workers `[integer]` (Optional) Number of parallel workers.
#'   Default: `NULL` (auto-detect).
#' @param cache_dir `[character]` Directory for caching predictions.
#'   Default: `tools::R_user_dir("horizons", "cache")`.
#' @param refresh_soil `[logical]` Force re-prediction of soil properties?
#'   Default: `FALSE`.
#' @param refresh_climate `[logical]` Force re-download of climate data?
#'   Default: `FALSE`.
#' @param verbose `[logical]` Print progress messages? Default: `TRUE`.
#'
#' @return A `[list]` containing:
#'   * `covariate_data`: Data frame with Sample_ID and all fetched covariates
#'   * `soil_predictions`: Full prediction results including models and metrics
#'   * `metadata`: Execution time, cache info, and diagnostic information
#'
#' @section Details:
#' The orchestration process follows these steps:
#' 1. Validates input data structure and coordinates
#' 2. Extracts covariates from configurations or direct specification
#' 3. Predicts soil properties using MIR spectra and OSSL library
#' 4. Fetches climate data from Daymet for given coordinates
#' 5. Combines all covariates into single data frame
#'
#' Soil predictions are cached based on spectral data and parameters. Climate
#' data fetching uses internal caching at the API level.
#'
#' @section Warning:
#' - Soil prediction requires significant memory (scales with n_similar)
#' - Climate data requires valid US coordinates for Daymet coverage
#' - Cache files can accumulate; periodic cleanup recommended
#'
#' @examples
#' # Direct covariate specification
#' fetch_covariates(input_data         = spectral_data,
#'                  soil_covariates    = c("clay", "sand", "ph"),
#'                  climate_covariates = c("MAT", "MAP"),
#'                  n_similar          = 20000,
#'                  allow_par          = TRUE) -> covariate_results
#'
#' # Using configurations
#' create_configs(soil_covariates    = c("clay", "ph"),
#'                climate_covariates = c("MAT", "MAP", "AI")) -> configs
#'
#' fetch_covariates(input_data     = spectral_data,
#'                  configurations = configs,
#'                  verbose        = TRUE) -> results
#'
#' \dontrun{
#' # Large-scale processing with caching
#' fetch_covariates(input_data         = large_dataset,
#'                  soil_covariates    = c("clay", "sand", "silt", "oc", "n", "ph"),
#'                  climate_covariates = c("MAT", "MAP", "PET", "AI"),
#'                  n_similar          = 30000,
#'                  bayesian_iter      = 20,
#'                  allow_par          = TRUE,
#'                  n_workers          = 10,
#'                  cache_dir          = "~/horizons_cache") -> full_covariates
#' }
#'
#' @importFrom dplyr select filter mutate bind_rows left_join distinct pull
#' @importFrom purrr map_lgl compact
#' @importFrom tibble tibble as_tibble
#' @importFrom cli cli_abort cli_text
#' @importFrom digest digest
#' @importFrom qs qsave qread
#' @export
#'

fetch_covariates <- function(input_data,
                             configurations     = NULL,
                             soil_covariates    = NULL,
                             climate_covariates = NULL,
                             spatial_covariates = NULL,
                             n_similar          = 35000,
                             variance_threshold = 0.99,
                             bayesian_iter      = 10,
                             prop_train         = 0.85,
                             climate_start_year = 2003,
                             climate_end_year   = 2024,
                             allow_par          = FALSE,
                             n_workers          = NULL,
                             cache_dir          = tools::R_user_dir("horizons", "cache"),
                             refresh_soil       = FALSE,
                             refresh_climate    = FALSE,
                             verbose            = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  start_time <- Sys.time()

  ## Check if input_data is a data frame ---------------------------------------

  if (!inherits(input_data, "data.frame")) cli::cli_abort("input_data must be a data.frame or tibble")

  ## Check for Sample_ID column ------------------------------------------------

  if (!"Sample_ID" %in% names(input_data)) cli::cli_abort("input_data must contain a Sample_ID column")

  ## Check for spectral columns (numeric column names) -------------------------

  spectral_cols <- grep("^[0-9]{3,4}$", names(input_data), value = TRUE)

  if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns found in input_data (expected numeric column names like 600, 650, etc.)")

  ## Validate configurations vs individual covariate parameters ----------------

  if (!is.null(configurations)) {

    if (!is.null(soil_covariates) || !is.null(climate_covariates) || !is.null(spatial_covariates)) cli::cli_text("├─ ⚠ Ignoring individual covariate parameters since configurations were provided")

    if (!inherits(configurations, "data.frame")) cli::cli_abort("configurations must be a data.frame or tibble from create_configs()")

    if (!"covariates" %in% names(configurations)) cli::cli_abort("configurations must have a 'covariates' column (use create_configs() to generate)")

  }

  ## Quick check to see if climate covariates are being called -----------------

  needs_coordinates <- FALSE

  if (!is.null(configurations)) {

    all_covariates    <- unlist(configurations$covariates)
    needs_coordinates <- any(all_covariates %in% KNOWN_CLIMATE_COVARIATES)

  } else if (!is.null(climate_covariates)) {

    needs_coordinates <- TRUE

  }

  ## Validate the coordinates if needed ----------------------------------------

  if (needs_coordinates) {

    if (!all(c("Longitude", "Latitude") %in% names(input_data))) cli::cli_abort("input_data must contain Longitude and Latitude columns for climate data fetching")

    if (!is.numeric(input_data$Longitude) || !is.numeric(input_data$Latitude)) cli::cli_abort("Longitude and Latitude must be numeric columns")

    if (any(is.na(input_data$Longitude)) || any(is.na(input_data$Latitude))) cli::cli_abort("Longitude and Latitude cannot contain NA values")

    if (any(input_data$Longitude < -180 | input_data$Longitude > 180, na.rm = TRUE)) cli::cli_abort("Longitude values must be between -180 and 180 degrees")

    if (any(input_data$Latitude < -90 | input_data$Latitude > 90, na.rm = TRUE)) cli::cli_abort("Latitude values must be between -90 and 90 degrees")

  }

  ## Validate the numeric parameters -------------------------------------------

  if (n_similar <= 0) cli::cli_abort("n_similar must be positive")

  if (variance_threshold <= 0 || variance_threshold > 1) cli::cli_abort("variance_threshold must be between 0 and 1")

  if (bayesian_iter < 0) cli::cli_abort("bayesian_iter must be non-negative (use 0 to skip Bayesian optimization)")

  if (prop_train <= 0 || prop_train >= 1) cli::cli_abort("prop_train must be between 0 and 1 (exclusive)")

  if (climate_start_year > climate_end_year) cli::cli_abort("climate_start_year cannot be after climate_end_year")




  ## Report out the set up to the user -----------------------------------------

  if (verbose) {

    cli::cli_text("")
    cli::cli_text("{cli::style_bold('Configuration')}")
    cli::cli_text("├─ Input samples: {nrow(input_data)}")
    cli::cli_text("├─ Spectral columns: {length(spectral_cols)}")
    cli::cli_text("└─ Configuration source: {if (!is.null(configurations)) 'Pre-built configurations' else 'Direct specification'}")
    cli::cli_text("")

  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Figure out what covariates we're dealing with
  ## ---------------------------------------------------------------------------

  requested_soil_covariates    <- NULL
  requested_climate_covariates <- NULL
  requested_spatial_covariates <- NULL

    ## ---------------------------------------------------------------------------
    ## Step 1.1: If configs exist, pull the covariates from there
    ## ---------------------------------------------------------------------------

    if (!is.null(configurations)) {

      all_covariates <- unique(unlist(purrr::compact(configurations$covariates)))

      if (length(all_covariates) > 0){

        all_covariates_lower <- tolower(all_covariates)

        list(soil    = all_covariates[all_covariates_lower %in% tolower(KNOWN_SOIL_COVARIATES)],
             climate = all_covariates[all_covariates_lower %in% tolower(KNOWN_CLIMATE_COVARIATES)],
             spatial = all_covariates[all_covariates_lower %in% tolower(KNOWN_SPATIAL_COVARIATES)],
             unknown = all_covariates[!all_covariates_lower %in% tolower(c(KNOWN_SOIL_COVARIATES,
                                                                   KNOWN_CLIMATE_COVARIATES,
                                                                   KNOWN_SPATIAL_COVARIATES))]) -> categorized

        if (length(categorized$unknown) > 0 && verbose){

          cli::cli_text("│  └─ {cli::col_red('!! Unknown covariates requested: {categorized$unknown}')}")
          cli::cli_text("│  └─ {cli::col_red('!! Run horizons::list_covariates() to see accepted values}")
          stop_quietly()

        }

        if(length(categorized$soil) > 0) requested_soil_covariates       <- categorized$soil
        if(length(categorized$climate) > 0) requested_climate_covariates <- categorized$climate
        if(length(categorized$spatial) > 0) requested_spatial_covariates <- categorized$spatial

      }

    ## -------------------------------------------------------------------------
    ## Step 1.2: Pull configs directly from user input if configs don't exist
    ## -------------------------------------------------------------------------

    } else {

      requested_soil_covariates    <- soil_covariates
      requested_climate_covariates <- climate_covariates
      requested_spatial_covariates <- spatial_covariates

    }

    ## -------------------------------------------------------------------------
    ## Step 1.3: Verbose reporting
    ## -------------------------------------------------------------------------

  cli::style_bold()

    if(verbose){

      cli::cli_text("{cli::style_bold('Covariates to fetch')}")
      cli::cli_text("├─ Soil")
      cli::cli_text("│  └─ {if(is.null(requested_soil_covariates) || length(requested_soil_covariates) == 0) cli::col_silver('None') else paste0('\"', paste(requested_soil_covariates, collapse = '\", \"'), '\"')}")
      cli::cli_text("├─ Climate")
      cli::cli_text("│  └─ {if(is.null(requested_climate_covariates) || length(requested_climate_covariates) == 0) cli::col_silver('None') else paste0('\"', paste(requested_climate_covariates, collapse = '\", \"'), '\"')}")
      cli::cli_text("└─ Spatial")
      cli::cli_text("   └─ {if(is.null(requested_spatial_covariates) || length(requested_spatial_covariates) == 0) cli::col_silver('None') else paste0('\"', paste(requested_spatial_covariates, collapse = '\", \"'), '\"')}")

    }

  ## ---------------------------------------------------------------------------
  ## Step 2: Fetch the soil covariates
  ## ---------------------------------------------------------------------------

  soil_predictions <- NULL
  soil_data        <- NULL

  if (length(requested_soil_covariates) > 0) {

    if(verbose) cli::cli_text("")
    if(verbose) cli::cli_text("{cli::style_bold('Fetching soil covariates')}")

    ## -------------------------------------------------------------------------
    ## Step 2.1: Setup the soil covariate caching infrastructure
    ## -------------------------------------------------------------------------

    ## Create spectral data hash -----------------------------------------------

    spectral_hash <- digest::digest(input_data[, spectral_cols], algo = "md5")

    ## Build cache key ---------------------------------------------------------

    list(spectral_hash      = spectral_hash,
         n_samples          = nrow(input_data),
         covariates         = sort(requested_soil_covariates),
         n_similar          = n_similar,
         variance_threshold = variance_threshold,
         bayesian_iter      = bayesian_iter,
         prop_train         = prop_train) -> cache_key

    # Create cache filename ----------------------------------------------------

    cache_hash      <- digest::digest(cache_key, algo = "md5")
    soil_cache_file <- file.path(cache_dir, paste0("soil_predictions_", cache_hash, ".qs"))

    ## -------------------------------------------------------------------------
    ## Step 2.2: Check the cache for saved results and load
    ## -------------------------------------------------------------------------

    soil_cache_data <- NULL
    use_cache       <- FALSE

    if (file.exists(soil_cache_file) && !refresh_soil) {

      ## If the cache exists, read in the files --------------------------------

      if (verbose) cli::cli_text("├─ Found cached soil predictions for current hash configuration ({.val {soil_cache_file}}).")

      tryCatch({

        soil_cache_data <- qs::qread(soil_cache_file)

      }, error = function(e){

         cli::cli_text("   └─ {cli::col_red('!! Error reading files from cache.')}")
         cli::cli_text("   └─ {cli::col_red('!! Predicting new set of soil covariates.')}")

      })

      ## Check that the read data is actually solid ----------------------------

      if (!is.null(soil_cache_data)) {

        if (!is.null(soil_cache_data$cache_key) && identical(soil_cache_data$cache_key, cache_key)) {

          soil_predictions <- soil_cache_data$predictions
          soil_data        <- soil_predictions$predictions
          use_cache        <- TRUE

          if(verbose) cli::cli_text("├─ Successfully loaded cached soil predictions.")

        }
      }

      ## Optionally report out the evaluation metrics --------------------------

      if (!is.null(soil_predictions$global_models) && verbose) {

        for (i in requested_soil_covariates){

          perf <- soil_predictions$global_models[[i]]$performance

          cli::cli_text("├─ {i}")

          if (!is.null(perf)) {
            cli::cli_text("│   ├─ R²: {.val {round(perf$val_r2, 3)}}")
            cli::cli_text("│   ├─ RMSE: {.val {round(perf$val_rmse, 2)}}")
            cli::cli_text("│   └─ RPD: {.val {round(perf$val_rpd, 3)}}")
          } else {
            cli::cli_text("│   └─ ⚠ Model fitting failed")
          }

        }

      }

    }

    ## -------------------------------------------------------------------------
    ## Step 2.3: Predict new soil covariates if no cache hits.
    ## -------------------------------------------------------------------------

    if (!use_cache) {

      ## Do some verbose reporting ---------------------------------------------

      if (verbose) {

        cli::cli_text("├─ No cache found for current hash configuration.")
        cli::cli_text("├─ Proceeding with covariate prediction from spectra.")

      }

      ## Predict the covariates ------------------------------------------------

      predict_soil_covariates(input_data         = input_data,
                              covariates         = requested_soil_covariates,
                              n_similar          = n_similar,
                              prop               = prop_train,
                              variance_threshold = variance_threshold,
                              bayesian_iter      = bayesian_iter,
                              allow_par          = allow_par,
                              n_workers          = n_workers,
                              refresh            = refresh_soil,
                              verbose            = verbose) -> soil_predictions

      soil_data <- soil_predictions$predictions

      if(verbose) cli::cli_text("├─ Soil covariate prediction complete.")

      ## Report out the evaluation metrics -------------------------------------

      if (!is.null(soil_predictions$global_models) && verbose) {

        for (i in requested_soil_covariates){

          perf <- soil_predictions$global_models[[i]]$performance

          if (!is.null(perf)) {
            cli::cli_text("├─ {i}")
            cli::cli_text("│   ├─ R²: {.val {round(perf$val_r2, 3)}}")
            cli::cli_text("│   ├─ RMSE: {.val {round(perf$val_rmse, 2)}}")
            cli::cli_text("│   └─ RPD: {.val {round(perf$val_rpd, 3)}}")
          } else {
            cli::cli_text("├─ {i}")
            cli::cli_text("│   └─ {cli::col_yellow('⚠')} Model fitting failed")
          }

        }

      }

      ## Cache the results -----------------------------------------------------

      # Check if any models actually succeeded
      any_model_succeeded <- FALSE
      if (!is.null(soil_predictions$global_models)) {
        for (model in soil_predictions$global_models) {
          if (!is.null(model) && !is.null(model$fitted_workflow)) {
            any_model_succeeded <- TRUE
            break
          }
        }
      }

      if (any_model_succeeded) {

        if (verbose) cli::cli_text("├─ Saving predictions to cache..")

        list(cache_key   = cache_key,
             predictions = soil_predictions,
             timestamp   = Sys.time()) -> cache_object

        tryCatch({

          if(!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

          qs::qsave(cache_object, soil_cache_file)

        }, error = function(e){

          if(verbose) cli::cli_text("│  └─ {cli::col_yellow('! Unable to save results ({.val {soil_cache_file}} to cache directory ({.val {cache_dir}}')}")

        })

        if (verbose) cli::cli_text("├─ Successfully cached soil predictions for future use.")

      } else {

        if (verbose) cli::cli_text("├─ Skipping cache - all models failed")

      }

    }

  } else {

    if (verbose && (is.null(configurations) || length(configurations$covariates) > 0)) cli::cli_text("No soil covariates requested - skipping soil prediction")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Fetch the climate covariates
  ## ---------------------------------------------------------------------------

  ##TODO: Move the cacheing logic out of the inner function and into the orchestrator.

  climate_data    <- NULL
  climate_summary <- NULL

  if (length(requested_climate_covariates) > 0) {

    if(verbose) cli::cli_text("")
    if(verbose) cli::cli_text("{cli::style_bold('Fetching climate covariates')}")

    ## -------------------------------------------------------------------------
    ## Step 3.1: Fetch climate data from Daymet
    ## -------------------------------------------------------------------------

    if (verbose) cli::cli_text("├─ Fetching climate data for {nrow(input_data)} locations ({climate_start_year}-{climate_end_year})")


    ## Call the climate fetching function directly ---------------------

    fetch_climate_covariates(input_data = input_data,
                             start_year = climate_start_year,
                             end_year   = climate_end_year,
                             cache_dir  = cache_dir,
                             refresh    = refresh_climate) -> climate_data

    ## -------------------------------------------------------------------------
    ## Step 3.2: Process and report results
    ## -------------------------------------------------------------------------

    if (!is.null(climate_data)) {

      ## Filter to only requested climate covariates ------------------------

      climate_data %>%
        dplyr::select("Sample_ID",
                      dplyr::all_of(requested_climate_covariates)) -> climate_data


      ## Report results ------------------------------------------------------

      if (verbose) {

        n_success <- sum(!is.na(climate_data[[requested_climate_covariates[1]]]))

        if (n_success == nrow(input_data)) {

          cli::cli_text("└─ Successfully fetched all climate variables")

        } else if (n_success > 0) {

          cli::cli_text("└─ Fetched climate data for {n_success}/{nrow(input_data)} locations")

        } else {

          cli::cli_text("└─ {cli::col_red('No climate data retrieved')}")

        }

      }

     } else {

      if (verbose) {

        cli::cli_text("├─ {cli::col_red('!! Climate data fetching failed for all grid cells.')}")
        cli::cli_text("└─ {cli::col_yellow('! Continuing without climate covariates.')}")

      }

    }

  } else {

    if (verbose && (is.null(configurations) || length(configurations$covariates) > 0)) cli::cli_text("No climate covariates requested - skipping climate data retrieval")

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Fetch the spatial covariates
  ## ---------------------------------------------------------------------------

  ## TODO: I still need to build this out.

  ## ---------------------------------------------------------------------------
  ## Step 5: Combine all covariates
  ## ---------------------------------------------------------------------------

  ## Start with Sample_ID from input data --------------------------------------

  input_data %>%
    dplyr::select(Sample_ID) -> covariate_data

  ## Add soil predictions if available -----------------------------------------

  if (!is.null(soil_data)) {
    dplyr::left_join(covariate_data, soil_data, by = "Sample_ID") -> covariate_data
  }

  ## Add climate data if available ---------------------------------------------

  if (!is.null(climate_data)) {
    # Remove Sample_ID and coords (already in input_data)
    climate_data %>%
      dplyr::select(-any_of(c("Longitude", "Latitude"))) %>%
      {dplyr::left_join(covariate_data, ., by = "Sample_ID")} -> covariate_data
  }

  ## Report combination results ------------------------------------------------

  if (verbose) {
    n_covariates <- ncol(covariate_data) - 1  # Minus Sample_ID
    if (n_covariates > 0) {
      cli::cli_text("")
      cli::cli_text("{cli::style_bold('Summary')}")
      cli::cli_text("└─ Combined: {nrow(covariate_data)} samples × {n_covariates} covariates")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Return results
  ## ---------------------------------------------------------------------------

  end_time   <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  ## Build metadata -------------------------------------------------------------

  list(
    execution_time           = total_time,
    execution_time_formatted = format_time(total_time),
    n_samples                = nrow(input_data),
    n_covariates_requested   = length(c(requested_soil_covariates, requested_climate_covariates, requested_spatial_covariates)),
    n_covariates_returned    = ncol(covariate_data) - 1,  # Minus Sample_ID
    soil_covariates          = requested_soil_covariates,
    climate_covariates       = requested_climate_covariates,
    spatial_covariates       = requested_spatial_covariates,
    cache_dir                = cache_dir) -> metadata

  ## Final reporting ------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("{cli::style_bold('Complete')}")
    cli::cli_text("├─ Execution time: {metadata$execution_time_formatted}")
    if (metadata$n_covariates_returned > 0) {
      cli::cli_text("└─ Output: {metadata$n_samples} samples × {metadata$n_covariates_returned} covariates")
    } else {
      cli::cli_text("└─ {cli::col_yellow('No covariates were successfully retrieved')}")
    }
  }

  ## Build and return result list -----------------------------------------------

  list(
    covariate_data   = covariate_data,
    soil_predictions = soil_predictions,
    metadata         = metadata
  )

}
