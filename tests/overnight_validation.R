#!/usr/bin/env Rscript
## =============================================================================
## Overnight Validation: Test All 15 Properties
## =============================================================================
##
## Comprehensive validation of library prediction across all supported properties.
## Compares predictions against Ng et al. 2022 benchmark results.
##
## Usage: nohup Rscript tests/overnight_validation.R > validation_results.txt 2>&1 &
##
## Estimated time: 8-12 hours (full OSSL, all properties, larger sample sizes)
##

devtools::load_all()
library(horizons)
library(dplyr)
library(tidyr)

## =============================================================================
## Configuration
## =============================================================================

## All 15 supported properties
ALL_PROPERTIES <- c(
  ## Texture
  "clay", "sand", "silt",

  ## Carbon
  "total_carbon", "oc", "carbonate",

  ## Nitrogen
  "total_nitrogen",

  ## Chemistry
  "ph", "cec",

  ## Base cations
  "calcium", "magnesium", "potassium", "sodium",

  ## Elements
  "iron_total", "aluminum_total"
)

## Test configuration
N_UNKNOWNS <- 50  # Larger for robust statistics
MAX_LIBRARY_SIZE <- 10000  # Larger library for better models
N_WORKERS <- 8  # Use more cores overnight
REMOVE_WATER_BANDS <- TRUE  # Test with water band removal

cat("\n")
cat("===============================================\n")
cat("  OVERNIGHT VALIDATION - All 15 Properties\n")
cat("===============================================\n\n")

cat("Configuration:\n")
cat("  Unknowns per property:", N_UNKNOWNS, "\n")
cat("  Max library size:", MAX_LIBRARY_SIZE, "\n")
cat("  Parallel workers:", N_WORKERS, "\n")
cat("  Water bands removed:", REMOVE_WATER_BANDS, "\n\n")

## =============================================================================
## Results Storage
## =============================================================================

all_results <- list()
timing_results <- list()

## =============================================================================
## Main Validation Loop
## =============================================================================

for (prop in ALL_PROPERTIES) {

  cat("\n")
  cat("===============================================\n")
  cat("  Testing:", toupper(prop), "\n")
  cat("===============================================\n\n")

  start_time <- Sys.time()

  tryCatch({

    ## -----------------------------------------------------------------------
    ## Step 1: Load and split OSSL data
    ## -----------------------------------------------------------------------

    cat("Loading OSSL data for", prop, "...\n")

    full_data <- horizons:::load_ossl_raw(
      property = prop,
      max_samples = MAX_LIBRARY_SIZE + N_UNKNOWNS,
      verbose = FALSE
    )

    n_total <- nrow(full_data)
    cat("  Loaded", n_total, "samples\n")

    if (n_total < 100) {
      cat("  SKIPPED: Insufficient data (<100 samples)\n")
      next
    }

    ## Hold out unknowns
    n_test <- min(N_UNKNOWNS, round(n_total * 0.1))
    library_data <- full_data[1:(n_total - n_test), ]
    unknown_data <- full_data[(n_total - n_test + 1):n_total, ]

    cat("  Split:", nrow(library_data), "library,", nrow(unknown_data), "unknowns\n\n")

    ## -----------------------------------------------------------------------
    ## Step 2: Extract unknowns and true values
    ## -----------------------------------------------------------------------

    spectral_cols <- grep("^[0-9]{3,4}$", names(unknown_data), value = TRUE)

    unknown_spectra <- unknown_data %>%
      dplyr::select(Sample_ID = sample_id, dplyr::all_of(spectral_cols))

    ## Get true values (handle texture specially)
    if (prop %in% c("sand", "silt", "clay")) {
      ## For texture, get all 3 true values
      mapping <- horizons:::get_library_property_mapping()
      sand_col <- mapping$ossl_name[mapping$property == "sand"]
      silt_col <- mapping$ossl_name[mapping$property == "silt"]
      clay_col <- mapping$ossl_name[mapping$property == "clay"]

      true_values <- unknown_data %>%
        dplyr::select(Sample_ID = sample_id,
                     sand = !!sand_col,
                     silt = !!silt_col,
                     clay = !!clay_col) %>%
        dplyr::mutate(
          ## OSSL stores as % (0-100), convert to g/kg to match predictions
          sand = sand * 10,
          silt = silt * 10,
          clay = clay * 10
        ) %>%
        tidyr::pivot_longer(cols = c(sand, silt, clay),
                           names_to = "property",
                           values_to = "true_value")1
    } else {
      ## Standard property
      mapping <- horizons:::get_library_property_mapping()
      prop_col <- mapping$ossl_name[mapping$property == prop]

      true_values <- tibble::tibble(
        Sample_ID = unknown_data$sample_id,
        property = prop,
        true_value = unknown_data[[prop_col]]
      )
    }

    ## -----------------------------------------------------------------------
    ## Step 3: Run prediction
    ## -----------------------------------------------------------------------

    cat("Running prediction...\n")

    predictions <- horizons:::predict_library(
      spectra = unknown_spectra,
      property = prop,
      remove_water_bands = REMOVE_WATER_BANDS,
      debug_mode = FALSE,  # Use full OSSL
      allow_par = TRUE,
      n_workers = N_WORKERS,
      verbose = TRUE  # Suppress output for cleaner logs
    )

    cat("  Completed!\n\n")

    ## -----------------------------------------------------------------------
    ## Step 4: Calculate metrics
    ## -----------------------------------------------------------------------

    ## Join predictions with true values
    comparison <- predictions %>%
      dplyr::left_join(true_values, by = c("Sample_ID", "property")) %>%
      dplyr::mutate(
        error = pred - true_value,
        abs_error = abs(error),
        sq_error = error^2
      )

    ## Calculate performance metrics
    metrics <- comparison %>%
      dplyr::group_by(property) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mae = mean(abs_error, na.rm = TRUE),
        rmse = sqrt(mean(sq_error, na.rm = TRUE)),
        bias = mean(error, na.rm = TRUE),
        r2 = cor(pred, true_value, use = "complete.obs")^2,
        ## Spectroscopy metrics
        rpd = sd(true_value, na.rm = TRUE) / rmse,
        ccc = horizons::ccc_vec(truth = true_value, estimate = pred)[1],
        .groups = "drop"
      )

    ## Display results
    cat("Results for", prop, ":\n")
    print(metrics)
    cat("\n")

    ## Store
    all_results[[prop]] <- metrics

    ## -----------------------------------------------------------------------
    ## Step 5: Track timing
    ## -----------------------------------------------------------------------

    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))

    timing_results[[prop]] <- tibble::tibble(
      property = prop,
      n_samples = n_total,
      n_unknowns = nrow(predictions),
      time_minutes = elapsed
    )

    cat("  Time:", round(elapsed, 1), "minutes\n\n")

  }, error = function(e) {
    cat("\n  ERROR:", e$message, "\n\n")
    all_results[[prop]] <- tibble::tibble(
      property = prop,
      n = 0,
      mae = NA,
      rmse = NA,
      error = e$message
    )
  })
}

## =============================================================================
## Summary Report
## =============================================================================

cat("\n")
cat("===============================================\n")
cat("  VALIDATION COMPLETE\n")
cat("===============================================\n\n")

## Combine all results
summary <- dplyr::bind_rows(all_results)
timing <- dplyr::bind_rows(timing_results)

cat("Summary across all properties:\n")
print(summary)

cat("\n\nTiming:\n")
print(timing)

cat("\n\nTotal time:", round(sum(timing$time_minutes, na.rm = TRUE)/60, 1), "hours\n")

## Save results
saveRDS(list(
  metrics = summary,
  timing = timing,
  config = list(
    n_unknowns = N_UNKNOWNS,
    max_library = MAX_LIBRARY_SIZE,
    water_bands_removed = REMOVE_WATER_BANDS,
    date = Sys.time()
  )
), "validation_results.rds")

cat("\nResults saved to: validation_results.rds\n\n")

## =============================================================================
