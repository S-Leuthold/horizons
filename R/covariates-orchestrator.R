#' Orchestrate Soil and Climate Covariate Prediction
#'
#' @description
#' Main orchestrator function that coordinates prediction of soil properties from MIR spectra
#' and fetching of climate data from geographic coordinates. Accepts either pre-built
#' configurations from create_configs() or individual covariate specifications.
#'
#' @param input_data Data frame or tibble with required columns:
#'   - Sample_ID: Unique sample identifiers
#'   - MIR spectral columns (wavenumbers as column names)
#'   - Longitude: Decimal degrees (-180 to 180)
#'   - Latitude: Decimal degrees (-90 to 90)
#' @param configurations Optional tibble from create_configs(). If provided, extracts
#'   unique covariates from all configuration rows. Takes precedence over individual
#'   covariate parameters.
#' @param soil_covariates Character vector of soil properties to predict
#'   (e.g., c("clay", "sand", "ph", "oc", "n")). Ignored if configurations provided.
#' @param climate_covariates Character vector of climate variables to fetch
#'   (e.g., c("MAT", "MAP", "AI", "PET", "GDD", "Precip_Seasonality")).
#'   Ignored if configurations provided.
#' @param spatial_covariates Character vector of spatial covariates (future implementation).
#'   Ignored if configurations provided.
#'
#' @param n_similar Integer. Number of OSSL samples for training soil models (default: 20000)
#' @param variance_threshold Numeric. PCA variance to retain (0-1, default: 0.85)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 3, use 0 for grid only)
#' @param prop_train Numeric. Proportion for training in train/val split (default: 0.85)
#'
#' @param climate_start_year Integer. Start year for Daymet climate data (default: 2003)
#' @param climate_end_year Integer. End year for Daymet climate data (default: 2024)
#'
#' @param allow_par Logical. Enable parallel processing (default: FALSE)
#' @param n_workers Integer. Number of parallel workers. NULL for auto-detect (default: NULL)
#'
#' @param cache_dir Character. Directory for caching data (default: tools::R_user_dir("horizons", "cache"))
#' @param refresh_soil Logical. Force re-prediction of soil properties (default: FALSE)
#' @param refresh_climate Logical. Force re-download of climate data (default: FALSE)
#'
#' @param return_configs Logical. Include configurations in output if built internally (default: FALSE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Named list containing:
#'   - covariate_data: Tibble with all predicted/fetched covariates joined to input data
#'   - soil_predictions: List with prediction details (models, metrics) if soil covariates requested
#'   - climate_summary: Tibble summarizing climate data fetching if climate covariates requested
#'   - configurations: Tibble of configurations if return_configs = TRUE
#'   - metadata: List with timing, cache usage, and other diagnostic information
#'
#' @examples
#' \dontrun{
#' # Using pre-built configurations
#' configs <- create_configs(
#'   soil_covariates = c("clay", "ph"),
#'   climate_covariates = c("MAT", "MAP")
#' )
#' results <- horizons_orchestrator(
#'   input_data = my_spectra_data,
#'   configurations = configs
#' )
#'
#' # Using direct covariate specification
#' results <- horizons_orchestrator(
#'   input_data = my_spectra_data,
#'   soil_covariates = c("clay", "sand", "ph"),
#'   climate_covariates = c("MAT", "MAP", "AI"),
#'   allow_par = TRUE
#' )
#' }
#'
#' @importFrom dplyr select filter mutate bind_rows left_join distinct pull
#' @importFrom purrr map_lgl compact
#' @importFrom tibble tibble as_tibble
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info cli_alert_success cli_text
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
                             return_configs     = FALSE,
                             verbose            = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  start_time <- Sys.time()

  # Validate input_data is a data frame
  if (!inherits(input_data, "data.frame")) {

    cli::cli_abort("input_data must be a data.frame or tibble")

  }

  # Check for Sample_ID column
  if (!"Sample_ID" %in% names(input_data)) {

    cli::cli_abort("input_data must contain a Sample_ID column")

  }

  # Check for spectral columns (numeric column names)
  spectral_cols <- grep("^[0-9]{3,4}$", names(input_data), value = TRUE)

  if (length(spectral_cols) == 0) {

    cli::cli_abort("No spectral columns found in input_data (expected numeric column names like 600, 650, etc.)")

  }

  # Validate configurations vs individual covariate parameters
  if (!is.null(configurations)) {

    # If configurations provided, individual parameters should be NULL
    if (!is.null(soil_covariates) || !is.null(climate_covariates) || !is.null(spatial_covariates)) {

      cli::cli_alert_warning("Ignoring individual covariate parameters since configurations were provided")

    }

    # Validate configurations structure
    if (!inherits(configurations, "data.frame")) {

      cli::cli_abort("configurations must be a data.frame or tibble from create_configs()")

    }

    if (!"covariates" %in% names(configurations)) {

      cli::cli_abort("configurations must have a 'covariates' column (use create_configs() to generate)")

    }

  }

  # Determine if we need coordinates (climate data requested)
  needs_coordinates <- FALSE

  if (!is.null(configurations)) {

    # Check if any config has climate covariates
    # TODO: Extract this logic after we implement Step 2

  } else if (!is.null(climate_covariates)) {

    needs_coordinates <- TRUE

  }

  # Validate coordinates if needed
  if (needs_coordinates) {

    if (!all(c("Longitude", "Latitude") %in% names(input_data))) {

      cli::cli_abort("input_data must contain Longitude and Latitude columns for climate data fetching")

    }

    if (!is.numeric(input_data$Longitude) || !is.numeric(input_data$Latitude)) {

      cli::cli_abort("Longitude and Latitude must be numeric columns")

    }

    if (any(is.na(input_data$Longitude)) || any(is.na(input_data$Latitude))) {

      cli::cli_abort("Longitude and Latitude cannot contain NA values")

    }

    if (any(input_data$Longitude < -180 | input_data$Longitude > 180, na.rm = TRUE)) {

      cli::cli_abort("Longitude values must be between -180 and 180 degrees")

    }

    if (any(input_data$Latitude < -90 | input_data$Latitude > 90, na.rm = TRUE)) {

      cli::cli_abort("Latitude values must be between -90 and 90 degrees")

    }

  }

  # Validate numeric parameters
  if (n_similar <= 0) {

    cli::cli_abort("n_similar must be positive")

  }

  if (variance_threshold <= 0 || variance_threshold > 1) {

    cli::cli_abort("variance_threshold must be between 0 and 1")

  }

  if (bayesian_iter < 0) {

    cli::cli_abort("bayesian_iter must be non-negative (use 0 to skip Bayesian optimization)")

  }

  if (prop_train <= 0 || prop_train >= 1) {

    cli::cli_abort("prop_train must be between 0 and 1 (exclusive)")

  }

  if (climate_start_year > climate_end_year) {

    cli::cli_abort("climate_start_year cannot be after climate_end_year")

  }

  if (verbose) {

    cli::cli_text("")
    cli::cli_text(format_header("Horizons Covariate Orchestrator", style = "double"))
    cli::cli_text("")

    config_info <- list(
      "Input samples" = format_metric(nrow(input_data), "count"),
      "Spectral columns" = format_metric(length(spectral_cols), "count"),
      "Configuration source" = if (!is.null(configurations)) "Pre-built configurations" else "Direct specification"
    )

    display_config_summary("Orchestrator Configuration", config_info, verbose)

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract covariates from configurations or use direct specification
  ## ---------------------------------------------------------------------------

  # Define known covariate types for categorization
  KNOWN_SOIL_COVARIATES <- c("clay", "sand", "silt", "ph", "phh2o", "oc", "ocd",
                             "n", "nitrogen", "cec", "bdod", "ca", "mg", "k", "na",
                             "p", "fe", "al", "mn", "zn", "cu")

  KNOWN_CLIMATE_COVARIATES <- c("MAT", "MAP", "PET", "AI", "GDD", "Precip_Seasonality",
                                "Temperature_Range", "Frost_Days")

  KNOWN_SPATIAL_COVARIATES <- c("elevation", "slope", "aspect", "twi", "curvature",
                                "hillshade", "roughness")

  # Initialize covariate lists
  requested_soil_covariates <- NULL
  requested_climate_covariates <- NULL
  requested_spatial_covariates <- NULL

  if (!is.null(configurations)) {

    if (verbose) {
      cli::cli_text("")
      cli::cli_text(format_header("Extracting Covariates from Configurations", style = "single", center = FALSE))
    }

    # Extract all covariates from configurations (handling NULL values)
    all_config_covariates <- configurations$covariates %>%
      purrr::compact() %>%  # Remove NULL entries
      unlist() %>%
      unique()

    if (length(all_config_covariates) > 0) {

      # Categorize covariates by type
      requested_soil_covariates <- all_config_covariates[tolower(all_config_covariates) %in% tolower(KNOWN_SOIL_COVARIATES)]
      requested_climate_covariates <- all_config_covariates[all_config_covariates %in% KNOWN_CLIMATE_COVARIATES]
      requested_spatial_covariates <- all_config_covariates[tolower(all_config_covariates) %in% tolower(KNOWN_SPATIAL_COVARIATES)]

      # Check for unknown covariates
      known_covariates <- c(KNOWN_SOIL_COVARIATES, KNOWN_CLIMATE_COVARIATES, KNOWN_SPATIAL_COVARIATES)
      unknown_covariates <- all_config_covariates[!tolower(all_config_covariates) %in% tolower(known_covariates)]

      if (length(unknown_covariates) > 0) {

        cli::cli_alert_warning("Unknown covariate type(s): {paste(unknown_covariates, collapse = ', ')}")
        cli::cli_alert_info("These will be treated as soil covariates")

        # Add unknown to soil covariates (most likely case)
        requested_soil_covariates <- unique(c(requested_soil_covariates, unknown_covariates))

      }

      if (verbose) {

        if (length(requested_soil_covariates) > 0) {
          cli::cli_text(format_tree_item(
            paste0("Soil covariates: ", paste(requested_soil_covariates, collapse = ", ")),
            level = 1, is_last = FALSE
          ))
        }

        if (length(requested_climate_covariates) > 0) {
          cli::cli_text(format_tree_item(
            paste0("Climate covariates: ", paste(requested_climate_covariates, collapse = ", ")),
            level = 1, is_last = FALSE
          ))
        }

        if (length(requested_spatial_covariates) > 0) {
          cli::cli_text(format_tree_item(
            paste0("Spatial covariates: ", paste(requested_spatial_covariates, collapse = ", ")),
            level = 1, is_last = TRUE
          ))
        } else {
          # Adjust the last marker
          cli::cli_text(format_tree_item(
            paste0("Total unique covariates: ", length(all_config_covariates)),
            level = 1, is_last = TRUE
          ))
        }

      }

    } else {

      if (verbose) {
        cli::cli_alert_info("No covariates found in configurations (spectra-only models)")
      }

    }

  } else {

    # Use direct specification
    requested_soil_covariates <- soil_covariates
    requested_climate_covariates <- climate_covariates
    requested_spatial_covariates <- spatial_covariates

    if (verbose && (length(c(soil_covariates, climate_covariates, spatial_covariates)) > 0)) {

      cli::cli_text("")
      cli::cli_text(format_header("Requested Covariates", style = "single", center = FALSE))

      if (length(requested_soil_covariates) > 0) {
        cli::cli_text(format_tree_item(
          paste0("Soil: ", paste(requested_soil_covariates, collapse = ", ")),
          level = 1, is_last = FALSE
        ))
      }

      if (length(requested_climate_covariates) > 0) {
        cli::cli_text(format_tree_item(
          paste0("Climate: ", paste(requested_climate_covariates, collapse = ", ")),
          level = 1, is_last = FALSE
        ))
      }

      if (length(requested_spatial_covariates) > 0) {
        cli::cli_text(format_tree_item(
          paste0("Spatial: ", paste(requested_spatial_covariates, collapse = ", ")),
          level = 1, is_last = TRUE
        ))
      } else if (length(requested_climate_covariates) == 0) {
        # Fix the tree ending
        cli::cli_text(format_tree_item("", level = 1, is_last = TRUE))
      }

    }

  }

  # Update needs_coordinates flag based on extracted covariates
  needs_coordinates <- length(requested_climate_covariates) > 0

  # Re-validate coordinates if we discovered we need them from configurations
  if (needs_coordinates && !all(c("Longitude", "Latitude") %in% names(input_data))) {

    cli::cli_abort("Climate covariates requested but input_data lacks Longitude/Latitude columns")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Predict soil properties if needed
  ## ---------------------------------------------------------------------------

  soil_predictions <- NULL
  soil_data <- NULL

  if (length(requested_soil_covariates) > 0) {

    ## -------------------------------------------------------------------------
    ## Create cache key for soil predictions
    ## -------------------------------------------------------------------------

    # Create a hash of the input spectra to detect data changes
    spectral_hash <- digest::digest(input_data[, spectral_cols], algo = "md5")

    # Build cache key from all parameters that affect results
    cache_key <- list(
      spectral_hash = spectral_hash,
      n_samples = nrow(input_data),
      covariates = sort(requested_soil_covariates),  # Sort for consistency
      n_similar = n_similar,
      variance_threshold = variance_threshold,
      bayesian_iter = bayesian_iter,
      prop_train = prop_train
    )

    # Create cache filename
    cache_hash <- digest::digest(cache_key, algo = "md5")
    soil_cache_file <- file.path(cache_dir, paste0("soil_predictions_", cache_hash, ".qs"))

    # Check for cached results
    use_cache <- FALSE
    if (file.exists(soil_cache_file) && !refresh_soil) {

      if (verbose) {
        cli::cli_alert_info("Found cached soil predictions")
      }

      cached_result <- safely_execute(
        expr = qs::qread(soil_cache_file),
        default_value = NULL,
        error_message = "Failed to read cached soil predictions"
      )

      if (!is.null(cached_result$result)) {

        # Verify cache validity
        if (!is.null(cached_result$result$cache_key) &&
            identical(cached_result$result$cache_key, cache_key)) {

          soil_predictions <- cached_result$result$predictions
          soil_data <- soil_predictions$predictions
          use_cache <- TRUE

          if (verbose) {
            cli::cli_alert_success("Using cached soil predictions")

            # Show cached metrics if available
            if (!is.null(soil_predictions$global_models)) {
              for (covariate in requested_soil_covariates) {
                if (!is.null(soil_predictions$global_models[[covariate]]$performance)) {
                  perf <- soil_predictions$global_models[[covariate]]$performance
                  cli::cli_text(format_tree_item(
                    paste0(covariate, " (cached): R² = ", round(perf$val_r2, 3),
                           ", RMSE = ", round(perf$val_rmse, 2)),
                    level = 1,
                    is_last = (covariate == tail(requested_soil_covariates, 1))
                  ))
                }
              }
            }
          }

        } else {

          if (verbose) {
            cli::cli_alert_warning("Cached soil predictions invalid (parameters changed)")
          }

        }
      }
    }

    if (!use_cache) {

      if (verbose) {
        cli::cli_text("")
        cli::cli_text(format_header("Soil Property Prediction", style = "single", center = FALSE))
        cli::cli_text(format_tree_item(
          paste0("Predicting ", length(requested_soil_covariates), " soil properties from MIR spectra"),
          level = 0
        ))
      }

      soil_result <- safely_execute(
      expr = {
        predict_soil_covariates(
          input_data = input_data,
          covariates = requested_soil_covariates,
          n_similar = n_similar,
          prop = prop_train,
          variance_threshold = variance_threshold,
          bayesian_iter = bayesian_iter,
          allow_par = allow_par,
          n_workers = n_workers,
          refresh = refresh_soil,
          verbose = verbose
        )
      },
      default_value = NULL,
      error_message = "Soil property prediction failed"
    )

    if (!is.null(soil_result$result)) {

      soil_predictions <- soil_result$result
      soil_data <- soil_predictions$predictions

      # Cache the results
      if (!is.null(soil_predictions)) {

        cache_object <- list(
          cache_key = cache_key,
          predictions = soil_predictions,
          timestamp = Sys.time()
        )

        safely_execute(
          expr = {
            if (!dir.exists(cache_dir)) {
              dir.create(cache_dir, recursive = TRUE)
            }
            qs::qsave(cache_object, soil_cache_file)
          },
          error_message = "Failed to cache soil predictions"
        )

        if (verbose) {
          cli::cli_alert_info("Cached soil predictions for future use")
        }

      }

      if (verbose) {

        # Report success and basic metrics
        n_predicted <- sum(!is.na(soil_data[[requested_soil_covariates[1]]]))
        cli::cli_alert_success(
          paste0("Successfully predicted soil properties for ", n_predicted, "/", nrow(input_data), " samples")
        )

        # Show validation metrics if available
        if (!is.null(soil_predictions$global_models)) {

          for (covariate in requested_soil_covariates) {

            if (!is.null(soil_predictions$global_models[[covariate]]$performance)) {

              perf <- soil_predictions$global_models[[covariate]]$performance
              cli::cli_text(format_tree_item(
                paste0(covariate, ": R² = ", round(perf$val_r2, 3), ", RMSE = ", round(perf$val_rmse, 2)),
                level = 1,
                is_last = (covariate == tail(requested_soil_covariates, 1))
              ))

            }

          }

        }

      }

      } else {

        cli::cli_alert_danger("Soil property prediction failed: {soil_result$error}")

        if (verbose) {
          cli::cli_alert_info("Continuing without soil covariates")
        }

      }

    }  # End of if (!use_cache)

  } else {

    if (verbose && (is.null(configurations) || length(configurations$covariates) > 0)) {
      cli::cli_alert_info("No soil covariates requested - skipping soil prediction")
    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Fetch climate data if needed
  ## ---------------------------------------------------------------------------

  climate_data <- NULL
  climate_summary <- NULL

  if (length(requested_climate_covariates) > 0) {

    if (verbose) {
      cli::cli_text("")
      cli::cli_text(format_header("Climate Data Retrieval", style = "single", center = FALSE))
      cli::cli_text(format_tree_item(
        paste0("Fetching climate data for ", nrow(input_data), " locations (",
               climate_start_year, "-", climate_end_year, ")"),
        level = 0
      ))
    }

    climate_result <- safely_execute(
      expr = {
        fetch_climate_covariates(
          input_data = input_data,
          start_year = climate_start_year,
          end_year = climate_end_year,
          cache_dir = cache_dir,
          refresh = refresh_climate
        )
      },
      default_value = NULL,
      error_message = "Climate data fetching failed"
    )

    if (!is.null(climate_result$result)) {

      climate_data <- climate_result$result

      # Filter to only requested climate covariates
      # The function returns all climate variables, but we only want what was requested
      climate_cols_to_keep <- c("Sample_ID", intersect(requested_climate_covariates, names(climate_data)))
      climate_data <- climate_data[, climate_cols_to_keep, drop = FALSE]

      # Create summary statistics
      n_success <- sum(!is.na(climate_data[[requested_climate_covariates[1]]]))

      climate_summary <- tibble::tibble(
        variable = requested_climate_covariates,
        n_retrieved = sapply(requested_climate_covariates, function(var) {
          if (var %in% names(climate_data)) {
            sum(!is.na(climate_data[[var]]))
          } else {
            0
          }
        }),
        n_missing = nrow(climate_data) - n_retrieved,
        coverage_pct = round(100 * n_retrieved / nrow(climate_data), 1)
      )

      if (verbose) {

        if (n_success == nrow(input_data)) {

          cli::cli_alert_success(
            paste0("Successfully retrieved climate data for all ", nrow(input_data), " samples")
          )

        } else if (n_success > 0) {

          cli::cli_alert_warning(
            paste0("Retrieved climate data for ", n_success, "/", nrow(input_data),
                   " samples (", round(100 * n_success / nrow(input_data), 1), "%)")
          )

        } else {

          cli::cli_alert_danger("Failed to retrieve climate data for any samples")

        }

        # Show which variables were retrieved
        for (i in seq_len(nrow(climate_summary))) {

          var_info <- climate_summary[i, ]
          status_symbol <- if (var_info$coverage_pct == 100) "✓" else "⚠"

          cli::cli_text(format_tree_item(
            paste0(status_symbol, " ", var_info$variable, ": ",
                   var_info$n_retrieved, "/", nrow(input_data),
                   " samples (", var_info$coverage_pct, "%)" ),
            level = 1,
            is_last = (i == nrow(climate_summary))
          ))

        }

      }

    } else {

      cli::cli_alert_danger("Climate data fetching failed: {climate_result$error}")

      if (verbose) {
        cli::cli_alert_info("Continuing without climate covariates")
      }

    }

  } else {

    if (verbose && (is.null(configurations) || length(configurations$covariates) > 0)) {
      cli::cli_alert_info("No climate covariates requested - skipping climate data retrieval")
    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Combine all covariates
  ## ---------------------------------------------------------------------------

  # Start with just Sample_ID
  covariate_data <- input_data %>%
    dplyr::select(Sample_ID)

  # Add soil predictions if available
  if (!is.null(soil_data)) {
    covariate_data <- covariate_data %>%
      dplyr::left_join(soil_data, by = "Sample_ID")
  }

  # Add climate data if available (excluding coordinates since they're in input_data)
  if (!is.null(climate_data)) {
    climate_cols <- setdiff(names(climate_data), c("Sample_ID", "Longitude", "Latitude"))
    if (length(climate_cols) > 0) {
      climate_to_join <- climate_data %>%
        dplyr::select(Sample_ID, dplyr::all_of(climate_cols))
      covariate_data <- covariate_data %>%
        dplyr::left_join(climate_to_join, by = "Sample_ID")
    }
  }

  # Note: Spatial covariates would be added here when implemented

  if (verbose) {
    n_covariates <- ncol(covariate_data) - 1  # Minus Sample_ID
    if (n_covariates > 0) {
      cli::cli_alert_success("Combined data: {nrow(covariate_data)} samples × {n_covariates} covariates")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Return results
  ## ---------------------------------------------------------------------------

  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Build metadata
  metadata <- list(
    execution_time = total_time,
    execution_time_formatted = format_time(total_time),
    n_samples = nrow(input_data),
    n_covariates_requested = length(c(requested_soil_covariates, requested_climate_covariates, requested_spatial_covariates)),
    n_covariates_returned = ncol(covariate_data) - 1,  # Minus Sample_ID
    soil_covariates = requested_soil_covariates,
    climate_covariates = requested_climate_covariates,
    spatial_covariates = requested_spatial_covariates,
    cache_dir = cache_dir,
    refresh_soil = refresh_soil,
    refresh_climate = refresh_climate
  )

  # Build configurations if requested and we created them internally
  if (return_configs && is.null(configurations)) {

    # Create a simple configuration table from the requested covariates
    configurations <- tibble::tibble(
      covariate_set = if (length(c(requested_soil_covariates, requested_climate_covariates)) > 0) {
        paste(c(requested_soil_covariates, requested_climate_covariates), collapse = "+")
      } else {
        "none"
      },
      covariates = list(c(requested_soil_covariates, requested_climate_covariates))
    )

  }

  # Final summary message
  if (verbose) {

    cli::cli_text("")
    cli::cli_text(format_header("Orchestration Complete", style = "double"))
    cli::cli_alert_success("Execution time: {metadata$execution_time_formatted}")

    if (metadata$n_covariates_returned > 0) {
      cli::cli_alert_info("Output: {metadata$n_samples} samples × {metadata$n_covariates_returned} covariates")
    } else {
      cli::cli_alert_warning("No covariates were successfully retrieved")
    }

  }

  # Build return list
  result <- list(
    covariate_data = covariate_data,
    soil_predictions = soil_predictions,
    climate_summary = climate_summary,
    metadata = metadata
  )

  # Add configurations if requested
  if (return_configs) {
    result$configurations <- configurations
  }

  return(result)

}
