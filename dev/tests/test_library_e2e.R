#!/usr/bin/env Rscript
## =============================================================================
## End-to-End Smoke Test: Library Prediction Pipeline
## =============================================================================
##
## Tests the full workflow from raw OSSL loading through clustering and training
## This is a minimal smoke test to verify the pipeline works end-to-end
##
## Usage: Rscript tests/test_library_e2e.R
##

library(horizons)

cat("\n")
cat("===============================================\n")
cat("  Library Prediction E2E Smoke Test\n")
cat("===============================================\n\n")

## ---------------------------------------------------------------------------
## Step 1: Load library data
## ---------------------------------------------------------------------------

cat("Step 1: Loading library data (clay, n=500 for speed)...\n")

lib_data <- horizons:::get_processed_library_data(
  property           = "clay",
  variance_threshold = 0.99,
  remove_water_bands = FALSE,
  max_samples        = 500,
  verbose            = FALSE
)

cat("  ✓ Loaded", lib_data$n_samples, "samples\n")
cat("  ✓ PCA components:", lib_data$n_components, "\n")
cat("  ✓ Variance explained:", round(lib_data$variance_explained * 100, 2), "%\n\n")

## ---------------------------------------------------------------------------
## Step 2: Perform clustering
## ---------------------------------------------------------------------------

cat("Step 2: Clustering library samples...\n")

gmm_result <- horizons:::fit_gmm_clustering(
  pca_scores      = lib_data$pca_scores,
  k_values        = c(5, 7),  # Test fewer K values for speed
  min_cluster_size = 50,      # Lower threshold for small test
  seed            = 123,
  verbose         = FALSE
)

cat("  ✓ Optimal K:", gmm_result$n_clusters, "\n")
cat("  ✓ Cluster sizes:", paste(table(gmm_result$cluster_assignments), collapse = ", "), "\n\n")

## ---------------------------------------------------------------------------
## Step 3: Add cluster IDs to raw data
## ---------------------------------------------------------------------------

cat("Step 3: Adding cluster IDs to library data...\n")

lib_data$library_data_raw$cluster_id <- gmm_result$cluster_assignments

cat("  ✓ Cluster column added\n\n")

## ---------------------------------------------------------------------------
## Step 4: Prepare training splits for one cluster
## ---------------------------------------------------------------------------

cat("Step 4: Preparing training split for cluster 1...\n")

cluster_1_data <- lib_data$library_data_raw %>%
  dplyr::filter(cluster_id == 1)

splits <- horizons:::prepare_cluster_splits(
  cluster_data = cluster_1_data,
  property     = "clay",
  train_prop   = 0.8,
  seed         = 456
)

cat("  ✓ Training pool:", nrow(splits$training_pool), "samples\n")
cat("  ✓ External test:", nrow(splits$external_test), "samples\n")
cat("  ✓ Column names:", paste(names(splits$training_pool)[1:5], collapse = ", "), "...\n\n")

## ---------------------------------------------------------------------------
## Step 5: Verify data structure for build_recipe
## ---------------------------------------------------------------------------

cat("Step 5: Verifying data structure...\n")

required_cols <- c("Sample_ID", "Project", "Response")
has_cols <- required_cols %in% names(splits$training_pool)

if (all(has_cols)) {
  cat("  ✓ All required columns present:", paste(required_cols, collapse = ", "), "\n")
} else {
  cat("  ✗ Missing columns:", paste(required_cols[!has_cols], collapse = ", "), "\n")
  stop("Data structure validation failed")
}

## Check spectral columns
spectral_cols <- grep("^[0-9]{3,4}$", names(splits$training_pool), value = TRUE)
cat("  ✓ Spectral columns:", length(spectral_cols), "(numeric format)\n")
cat("  ✓ Sample wavelengths:", paste(head(spectral_cols, 5), collapse = ", "), "...\n\n")

## ---------------------------------------------------------------------------
## Step 6: Test build_recipe compatibility
## ---------------------------------------------------------------------------

cat("Step 6: Testing build_recipe compatibility...\n")

tryCatch({

  recipe_test <- horizons::build_recipe(
    project_data = splits$training_pool,
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    response_variable = "Response",
    id_variable = "Sample_ID"
  )

  cat("  ✓ build_recipe succeeded\n")
  cat("  ✓ Recipe steps:", length(recipe_test$steps), "\n\n")

}, error = function(e) {
  cat("  ✗ build_recipe failed:", e$message, "\n\n")
  stop("build_recipe compatibility failed")
})

## ---------------------------------------------------------------------------
## Summary
## ---------------------------------------------------------------------------

cat("===============================================\n")
cat("  ✓ ALL TESTS PASSED\n")
cat("===============================================\n\n")

cat("Summary:\n")
cat("  - Library loading: PASS\n")
cat("  - GMM clustering: PASS\n")
cat("  - Data preparation: PASS\n")
cat("  - Column naming: PASS (Sample_ID, Response, numeric wavelengths)\n")
cat("  - build_recipe compatibility: PASS\n\n")

cat("Next steps for M1.5:\n")
cat("  1. Implement ILR transformation for texture\n")
cat("  2. Add pH bounds enforcement\n")
cat("  3. Build predict_library() API\n\n")
