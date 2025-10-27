## =============================================================================
## Manual Test Script: predict_library() E2E
## =============================================================================
##
## Run this interactively in RStudio to test the full predict_library() workflow
## with real OSSL data as unknowns.
##
## Usage:
##   1. Open in RStudio
##   2. Run line by line or source entire file
##   3. Inspect objects at each step
##

library(horizons)
devtools::load_all()

cat("\n")
cat("===============================================\n")
cat("  Manual Test: predict_library()\n")
cat("===============================================\n\n")

## =============================================================================
## Step 1: Create realistic "unknown" spectra from OSSL holdout
## =============================================================================

cat("Step 1: Creating test unknowns from OSSL data...\n")

## Load a small sample of real OSSL data to use as "unknowns" -----------------
## We'll use pH data and hold out a small subset

ph_library <- horizons:::load_ossl_raw(
  property    = "clay",
  max_samples = 100,  # Small for speed
  verbose     = TRUE
)

cat("  - Loaded", nrow(ph_library), "pH samples from OSSL\n")

## Hold out last 5 samples as "unknowns" ---------------------------------------
## (In real use, these would be user's samples with unknown pH)

n_total <- nrow(ph_library)
n_unknowns <- 5

library_samples <- ph_library[1:(n_total - n_unknowns), ]
unknown_samples <- ph_library[(n_total - n_unknowns + 1):n_total, ]

cat("  - Split: ", nrow(library_samples), "for library,", nrow(unknown_samples), "as unknowns\n")

## Extract just spectra and Sample_ID for unknowns -----------------------------

spectral_cols <- grep("^[0-9]{3,4}$", names(unknown_samples), value = TRUE)
unknown_spectra <- unknown_samples %>%
  dplyr::select(Sample_ID = sample_id, dplyr::all_of(spectral_cols))

cat("  - Unknown spectra: ", nrow(unknown_spectra), "samples ×", length(spectral_cols), "wavelengths\n\n")

## Save true pH values for validation ------------------------------------------

true_ph <- unknown_samples$clay.tot_usda.a334_w.pct
names(true_ph) <- unknown_spectra$Sample_ID

cat("  - True pH values (for validation):\n")
print(round(true_ph, 2))
cat("\n")

## =============================================================================
## Step 2: Run predict_library()
## =============================================================================

cat("Step 2: Running predict_library()...\n")
cat("  (This will take a few minutes - loading full OSSL, clustering, training)\n\n")

devtools::load_all()

predictions <- horizons:::predict_library(
  spectra    = unknown_spectra,
  property   = "clay",
  debug_mode = TRUE,  # Fast: 500 samples, K ∈ {5,7} only
  verbose    = TRUE
)

cat("\n")

## =============================================================================
## Step 3: Inspect Results
## =============================================================================

cat("Step 3: Results:\n\n")

print(predictions)

cat("\n")

## =============================================================================
## Step 4: Validate Predictions
## =============================================================================

cat("Step 4: Validation:\n\n")

## Compare predictions to true values ------------------------------------------

comparison <- tibble::tibble(
  Sample_ID = predictions$Sample_ID,
  true_ph   = true_ph[predictions$Sample_ID],
  pred_ph   = predictions$pred,
  error     = predictions$pred - true_ph[predictions$Sample_ID],
  abs_error = abs(predictions$pred - true_ph[predictions$Sample_ID])
)

print(comparison)

cat("\n")
cat("Summary Statistics:\n")
cat("  - Mean absolute error:", round(mean(comparison$abs_error, na.rm = TRUE), 3), "\n")
cat("  - RMSE:", round(sqrt(mean(comparison$error^2, na.rm = TRUE)), 3), "\n")
cat("  - Bias:", round(mean(comparison$error, na.rm = TRUE), 3), "\n\n")

## =============================================================================
## Step 5: Check Structure
## =============================================================================

cat("Step 5: Result Structure Check:\n\n")

cat("  - Columns:", paste(names(predictions), collapse = ", "), "\n")
cat("  - N rows:", nrow(predictions), "\n")
cat("  - N unique samples:", length(unique(predictions$Sample_ID)), "\n")
cat("  - N unique clusters:", length(unique(predictions$cluster_id)), "\n")
cat("  - Config used:", unique(predictions$config_id), "\n\n")

cat("===============================================\n")
cat("  ✓ Manual Test Complete!\n")
cat("===============================================\n\n")

## =============================================================================
## Optional: Test with texture property
## =============================================================================

cat("\nOptional: Test with texture (clay)...\n")
cat("  (Uncomment and run to test texture auto-expansion)\n\n")

# clay_library <- horizons:::load_ossl_raw("clay", max_samples = 100, verbose = FALSE)
# n_total <- nrow(clay_library)
# n_unknowns <- 3
#
# unknown_clay <- clay_library[(n_total - n_unknowns + 1):n_total, ]
# spectral_cols <- grep("^[0-9]{3,4}$", names(unknown_clay), value = TRUE)
#
# unknown_clay_spectra <- unknown_clay %>%
#   dplyr::select(Sample_ID = sample_id, dplyr::all_of(spectral_cols))
#
# clay_predictions <- horizons:::predict_library(
#   spectra = unknown_clay_spectra,
#   property = "clay",  # Should auto-expand to sand + silt + clay
#   verbose = TRUE
# )
#
# print(clay_predictions)
#
# ## Check mass balance
# texture_by_sample <- clay_predictions %>%
#   tidyr::pivot_wider(names_from = property, values_from = pred)
#
# texture_by_sample$total <- texture_by_sample$sand +
#                            texture_by_sample$silt +
#                            texture_by_sample$clay
#
# cat("\nMass balance check (should all be 1000 g/kg):\n")
# print(texture_by_sample %>% dplyr::select(Sample_ID, sand, silt, clay, total))

## =============================================================================
