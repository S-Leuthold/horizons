#' Test Suite for OSSL Data Integration and Processing
#' 
#' Comprehensive tests for data preprocessing, PCA, and OSSL training data functions.
#' Tests cover unit functionality, edge cases, performance, and statistical validity.

library(testthat)
library(horizons)
library(dplyr)
library(tibble)

# Test Property Mapping -------------------------------------------------------

test_that("get_ossl_property_mapping returns correct structure", {
  mapping <- get_ossl_property_mapping()
  
  # Check structure
  expect_s3_class(mapping, "tbl_df")
  expect_equal(
    names(mapping),
    c("property", "analyte", "description", "target_unit", "ossl_name")
  )
  
  # Check required properties are present
  required_props <- c("clay", "sand", "silt", "ph", "oc", "cec")
  expect_true(all(required_props %in% mapping$property))
  
  # Check data completeness
  expect_false(any(is.na(mapping$property)))
  expect_false(any(is.na(mapping$analyte)))
})

test_that("map_ossl_to_properties correctly maps OSSL columns", {
  # Create mock OSSL data with known columns
  mock_ossl <- tibble(
    sample_id = paste0("S", 1:10),
    clay.tot_usda.c60_w.pct = runif(10, 10, 50),
    sand.tot_usda.c60_w.pct = runif(10, 20, 80),
    ph.h2o_usda.a268_index = runif(10, 5, 8),
    oc_usda.c729_w.pct = runif(10, 0.5, 5)
  )
  
  # Map to properties
  mapped <- map_ossl_to_properties(mock_ossl)
  
  # Check renamed columns exist
  expect_true("clay" %in% names(mapped))
  expect_true("sand" %in% names(mapped))
  expect_true("ph" %in% names(mapped))
  expect_true("oc" %in% names(mapped))
  
  # Check values are preserved
  expect_equal(mapped$clay, mock_ossl$clay.tot_usda.c60_w.pct * 10) # Converted to g/kg
  expect_equal(mapped$ph, mock_ossl$ph.h2o_usda.a268_index) # No conversion
})

# Test Spectral Preprocessing -------------------------------------------------

test_that("preprocess_mir_spectra handles normal spectral data", {
  # Generate mock spectral data
  n_samples <- 50
  n_wavelengths <- 100
  
  spectral_data <- tibble(
    sample_id = paste0("S", 1:n_samples)
  )
  
  # Add wavelength columns with realistic names
  wave_cols <- paste0("scan_mir.", seq(600, 4000, length.out = n_wavelengths))
  for (col in wave_cols) {
    spectral_data[[col]] <- runif(n_samples, 0.1, 0.9) + rnorm(n_samples, 0, 0.05)
  }
  
  # Test preprocessing
  processed <- preprocess_mir_spectra(
    spectral_data,
    smooth_window = 9,
    smooth_poly = 1
  )
  
  # Check output structure
  expect_s3_class(processed, "tbl_df")
  expect_equal(nrow(processed), n_samples)
  expect_true("sample_id" %in% names(processed))
  
  # Check spectral columns are processed
  spec_cols <- grep("^scan_mir\\.", names(processed), value = TRUE)
  expect_true(length(spec_cols) > 0)
  
  # Check values are transformed (SNV should change scale)
  original_mean <- mean(as.matrix(spectral_data[wave_cols]), na.rm = TRUE)
  processed_mean <- mean(as.matrix(processed[spec_cols]), na.rm = TRUE)
  expect_false(isTRUE(all.equal(original_mean, processed_mean)))
})

test_that("preprocess_mir_spectra handles edge cases", {
  # Test empty data
  empty_data <- tibble(sample_id = character())
  expect_error(
    preprocess_mir_spectra(empty_data),
    "No spectral columns found"
  )
  
  # Test data with no spectral columns
  no_spectra <- tibble(
    sample_id = paste0("S", 1:10),
    other_col = 1:10
  )
  expect_error(
    preprocess_mir_spectra(no_spectra),
    "No spectral columns found"
  )
  
  # Test data with missing values
  spectra_with_na <- tibble(
    sample_id = paste0("S", 1:5),
    scan_mir.1000 = c(0.5, NA, 0.6, 0.7, NA),
    scan_mir.2000 = c(NA, 0.4, 0.5, NA, 0.6)
  )
  
  processed <- preprocess_mir_spectra(spectra_with_na)
  # Should handle NAs gracefully
  expect_s3_class(processed, "tbl_df")
})

test_that("preprocess_mir_spectra smoothing parameters work correctly", {
  # Create noisy spectral data
  n_samples <- 20
  n_waves <- 50
  
  spectral_data <- tibble(sample_id = paste0("S", 1:n_samples))
  wave_cols <- paste0("scan_mir.", seq(1000, 3000, length.out = n_waves))
  
  for (col in wave_cols) {
    # Add signal + noise
    signal <- sin(seq(0, 2*pi, length.out = n_samples))
    noise <- rnorm(n_samples, 0, 0.2)
    spectral_data[[col]] <- signal + noise
  }
  
  # Test different smoothing windows
  smooth_5 <- preprocess_mir_spectra(spectral_data, smooth_window = 5)
  smooth_11 <- preprocess_mir_spectra(spectral_data, smooth_window = 11)
  
  # Larger window should produce smoother output (less variance)
  spec_cols <- grep("^scan_mir\\.", names(spectral_data), value = TRUE)
  
  var_5 <- var(as.matrix(smooth_5[spec_cols]), na.rm = TRUE)
  var_11 <- var(as.matrix(smooth_11[spec_cols]), na.rm = TRUE)
  
  # More smoothing should reduce variance
  expect_true(var_11 < var_5)
})

# Test PCA Functions ----------------------------------------------------------

test_that("perform_pca_on_ossl works with valid data", {
  # Create mock OSSL data with spectra and properties
  n_samples <- 100
  n_pcs <- 20
  
  ossl_data <- tibble(
    sample_id = paste0("OSSL_", 1:n_samples),
    clay = runif(n_samples, 100, 600),
    ph = runif(n_samples, 4, 9),
    oc = runif(n_samples, 5, 50)
  )
  
  # Add spectral columns
  for (i in 1:50) {
    ossl_data[[paste0("scan_mir.", 600 + i*50)]] <- runif(n_samples)
  }
  
  # Perform PCA
  pca_result <- perform_pca_on_ossl(
    ossl_data, 
    variance_threshold = 0.95
  )
  
  # Check structure
  expect_type(pca_result, "list")
  expect_true("pca_model" %in% names(pca_result))
  expect_true("pca_scores" %in% names(pca_result))
  expect_true("n_components" %in% names(pca_result))
  expect_true("variance_explained" %in% names(pca_result))
  
  # Check PCA model
  expect_s3_class(pca_result$pca_model, "PCA")
  
  # Check scores
  expect_s3_class(pca_result$pca_scores, "tbl_df")
  expect_equal(nrow(pca_result$pca_scores), n_samples)
  expect_true("sample_id" %in% names(pca_result$pca_scores))
  
  # Check variance threshold is met
  cumvar <- cumsum(pca_result$pca_model$eig[, 2]) / 100
  n_comp_for_threshold <- which(cumvar >= 0.95)[1]
  expect_equal(pca_result$n_components, n_comp_for_threshold)
})

test_that("perform_pca_on_ossl handles variance thresholds correctly", {
  # Create data with known variance structure
  n_samples <- 50
  
  ossl_data <- tibble(sample_id = paste0("S", 1:n_samples))
  
  # Create columns with decreasing variance
  for (i in 1:20) {
    variance <- 10 / i  # Decreasing variance
    ossl_data[[paste0("scan_mir.", i)]] <- rnorm(n_samples, 0, sqrt(variance))
  }
  
  # Test different thresholds
  pca_80 <- perform_pca_on_ossl(ossl_data, variance_threshold = 0.80)
  pca_95 <- perform_pca_on_ossl(ossl_data, variance_threshold = 0.95)
  pca_99 <- perform_pca_on_ossl(ossl_data, variance_threshold = 0.99)
  
  # More components needed for higher threshold
  expect_true(pca_80$n_components <= pca_95$n_components)
  expect_true(pca_95$n_components <= pca_99$n_components)
})

test_that("apply_pca_to_unknown works with compatible data", {
  # First create PCA model from OSSL
  n_ossl <- 100
  n_unknown <- 30
  n_features <- 25
  
  # Create OSSL data
  ossl_data <- tibble(sample_id = paste0("OSSL_", 1:n_ossl))
  feature_names <- paste0("scan_mir.", seq(1000, 3000, length.out = n_features))
  
  for (feat in feature_names) {
    ossl_data[[feat]] <- rnorm(n_ossl)
  }
  
  # Create PCA model
  pca_result <- perform_pca_on_ossl(ossl_data, variance_threshold = 0.95)
  
  # Create unknown data with same features
  unknown_data <- tibble(sample_id = paste0("UNK_", 1:n_unknown))
  for (feat in feature_names) {
    unknown_data[[feat]] <- rnorm(n_unknown)
  }
  
  # Apply PCA
  unknown_scores <- apply_pca_to_unknown(
    unknown_data,
    pca_result$pca_model,
    pca_result$n_components
  )
  
  # Check output
  expect_s3_class(unknown_scores, "tbl_df")
  expect_equal(nrow(unknown_scores), n_unknown)
  expect_true("sample_id" %in% names(unknown_scores))
  
  # Check correct number of components
  pc_cols <- grep("^Dim\\.", names(unknown_scores), value = TRUE)
  expect_equal(length(pc_cols), pca_result$n_components)
})

test_that("apply_pca_to_unknown handles mismatched features", {
  # Create PCA model
  ossl_data <- tibble(
    sample_id = paste0("OSSL_", 1:50),
    scan_mir.1000 = rnorm(50),
    scan_mir.2000 = rnorm(50),
    scan_mir.3000 = rnorm(50)
  )
  
  pca_result <- perform_pca_on_ossl(ossl_data)
  
  # Unknown data with different features
  unknown_wrong <- tibble(
    sample_id = paste0("UNK_", 1:10),
    scan_mir.1500 = rnorm(10),  # Wrong wavelength
    scan_mir.2500 = rnorm(10)
  )
  
  expect_error(
    apply_pca_to_unknown(unknown_wrong, pca_result$pca_model, pca_result$n_components),
    "Missing required spectral columns"
  )
})

# Test Integrated Processing Function -----------------------------------------

test_that("get_processed_ossl_training_data integrates all steps", {
  skip("Requires OSSL data download - test with mock in CI/CD")
  
  # This would be tested with mocked OSSL data in CI/CD
  properties <- c("clay", "ph")
  
  with_mocked_ossl({
    result <- get_processed_ossl_training_data(
      properties = properties,
      variance_threshold = 0.95
    )
    
    # Check structure
    expect_type(result, "list")
    expect_true("data" %in% names(result))
    expect_true("pca_model" %in% names(result))
    expect_true("pca_scores" %in% names(result))
    expect_true("n_components" %in% names(result))
    expect_true("preprocessing_params" %in% names(result))
    
    # Check data has required properties
    expect_true(all(properties %in% names(result$data)))
    
    # Check preprocessing params are stored
    expect_true("smooth_window" %in% names(result$preprocessing_params))
    expect_true("smooth_poly" %in% names(result$preprocessing_params))
  })
})

# Test Performance and Memory -------------------------------------------------

test_that("preprocess_mir_spectra handles large datasets efficiently", {
  skip_on_cran()  # Skip on CRAN to save time
  
  # Create large dataset
  n_samples <- 10000
  n_wavelengths <- 500
  
  large_data <- tibble(sample_id = paste0("S", 1:n_samples))
  
  for (i in 1:n_wavelengths) {
    large_data[[paste0("scan_mir.", 600 + i*5)]] <- runif(n_samples)
  }
  
  # Measure time
  time_taken <- system.time({
    processed <- preprocess_mir_spectra(large_data)
  })
  
  # Should process in reasonable time (< 10 seconds for 10k samples)
  expect_true(time_taken["elapsed"] < 10)
  
  # Check memory usage doesn't explode
  object_size <- object.size(processed)
  original_size <- object.size(large_data)
  
  # Processed shouldn't be more than 2x original size
  expect_true(as.numeric(object_size) < as.numeric(original_size) * 2)
})

test_that("PCA handles high-dimensional data efficiently", {
  skip_on_cran()
  
  # Create high-dimensional dataset
  n_samples <- 5000
  n_features <- 1000
  
  high_dim_data <- tibble(sample_id = paste0("S", 1:n_samples))
  
  for (i in 1:n_features) {
    high_dim_data[[paste0("scan_mir.", i)]] <- rnorm(n_samples)
  }
  
  # Should complete PCA in reasonable time
  time_taken <- system.time({
    pca_result <- perform_pca_on_ossl(high_dim_data, variance_threshold = 0.95)
  })
  
  expect_true(time_taken["elapsed"] < 30)  # 30 seconds for 5k x 1k matrix
  
  # Should reduce dimensions significantly
  expect_true(pca_result$n_components < n_features / 2)
})

# Test Statistical Validity ---------------------------------------------------

test_that("PCA preserves statistical properties", {
  set.seed(123)
  n_samples <- 200
  
  # Create data with known correlation structure
  base1 <- rnorm(n_samples)
  base2 <- rnorm(n_samples)
  
  data <- tibble(
    sample_id = paste0("S", 1:n_samples),
    scan_mir.1 = base1,
    scan_mir.2 = base1 + rnorm(n_samples, 0, 0.1),  # Highly correlated with 1
    scan_mir.3 = base2,
    scan_mir.4 = base2 + rnorm(n_samples, 0, 0.1),  # Highly correlated with 3
    scan_mir.5 = rnorm(n_samples)  # Independent
  )
  
  pca_result <- perform_pca_on_ossl(data, variance_threshold = 0.99)
  
  # Should identify approximately 3 main components
  # (two correlated pairs + one independent)
  expect_true(pca_result$n_components <= 4)
  
  # First components should explain most variance
  first_two_var <- sum(pca_result$pca_model$eig[1:2, 2])
  expect_true(first_two_var > 60)  # First 2 PCs should explain >60% variance
})

test_that("preprocessing preserves relative distances", {
  set.seed(456)
  n_samples <- 50
  
  # Create spectral data with known groups
  data <- tibble(sample_id = paste0("S", 1:n_samples))
  
  # Create two distinct spectral groups
  for (i in 1:20) {
    if (i <= 25) {
      # Group 1: lower values
      data[[paste0("scan_mir.", i)]] <- c(
        rnorm(25, mean = 0, sd = 0.5),
        rnorm(25, mean = 3, sd = 0.5)
      )
    }
  }
  
  # Calculate distances before preprocessing
  spec_cols <- grep("^scan_mir\\.", names(data), value = TRUE)
  dist_before <- dist(as.matrix(data[spec_cols]))
  
  # Preprocess
  processed <- preprocess_mir_spectra(data)
  dist_after <- dist(as.matrix(processed[spec_cols]))
  
  # Correlation between distance matrices should be high
  cor_distances <- cor(as.vector(dist_before), as.vector(dist_after))
  expect_true(cor_distances > 0.8)  # Strong correlation preserved
})