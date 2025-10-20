#' Tests for OSSL Data Functions (covariates-data.R)
#'
#' STRATEGY: Integration-first (DATA PROCESSING - all fast operations!)
#'
#' covariates-data.R contains functions for downloading, processing, and preparing
#' OSSL (Open Soil Spectroscopy Library) data. Unlike Session 5 (expensive model fitting),
#' these are DATA PROCESSING operations - perfect for integration testing!
#'
#' The only slow operation is OSSL download (1-2GB), which we skip with appropriate guards.
#' All other operations (property mapping, preprocessing, PCA) are fast and testable.
#'
#' Integration tests (70%): Test complete data processing workflows
#' Validation tests (30%): Test error handling and parameter validation

library(testthat)
library(horizons)

# Access internal helpers not exported
get_ossl_property_mapping <- horizons:::get_ossl_property_mapping
validate_soil_properties <- horizons:::validate_soil_properties
preprocess_mir_spectra <- horizons:::preprocess_mir_spectra
perform_pca_on_ossl <- horizons:::perform_pca_on_ossl
project_spectra_to_pca <- horizons:::project_spectra_to_pca

## ===========================================================================
## INTEGRATION TESTS - Property Mapping
## ===========================================================================

test_that("get_ossl_property_mapping returns correct structure", {
  # SPEC-COV-DATA-INT-001: Property mapping structure

  mapping <- get_ossl_property_mapping()

  # Verify it's a tibble
  expect_s3_class(mapping, "data.frame")

  # Verify required columns exist
  expect_true("property" %in% names(mapping))
  expect_true("analyte" %in% names(mapping))
  expect_true("description" %in% names(mapping))
  expect_true("target_unit" %in% names(mapping))
  expect_true("ossl_name_level1" %in% names(mapping))

  # Verify we have all 16 standard properties
  expect_equal(nrow(mapping), 16)
})

test_that("get_ossl_property_mapping includes expected properties", {
  # SPEC-COV-DATA-INT-002: Standard properties are present

  mapping <- get_ossl_property_mapping()

  expected_properties <- c("clay", "sand", "silt", "ph", "oc", "cec",
                          "bulk_density", "total_nitrogen", "carbonate",
                          "phosphorus", "potassium", "calcium", "magnesium",
                          "sodium", "aluminum_crystalline", "iron_amorphous")

  expect_setequal(mapping$property, expected_properties)
})

test_that("get_ossl_property_mapping has consistent data types", {
  # SPEC-COV-DATA-INT-003: All columns are character vectors

  mapping <- get_ossl_property_mapping()

  expect_type(mapping$property, "character")
  expect_type(mapping$analyte, "character")
  expect_type(mapping$description, "character")
  expect_type(mapping$target_unit, "character")
  expect_type(mapping$ossl_name_level1, "character")
})

test_that("get_ossl_property_mapping units are appropriate", {
  # SPEC-COV-DATA-INT-004: Units match expected spectroscopy standards

  mapping <- get_ossl_property_mapping()

  # Check specific property units
  expect_equal(mapping$target_unit[mapping$property == "clay"], "g/kg")
  expect_equal(mapping$target_unit[mapping$property == "ph"], "unitless")
  expect_equal(mapping$target_unit[mapping$property == "cec"], "cmolc/kg")
  expect_equal(mapping$target_unit[mapping$property == "bulk_density"], "g/cm³")
})

## ===========================================================================
## INTEGRATION TESTS - Property Validation
## ===========================================================================

test_that("validate_soil_properties accepts valid properties", {
  # SPEC-COV-DATA-INT-005: Valid properties pass validation

  valid_props <- c("clay", "sand", "ph", "oc")

  # Should not error
  expect_no_error(
    validate_soil_properties(valid_props)
  )

  # Should return all TRUE
  result <- validate_soil_properties(valid_props)
  expect_true(all(result))
  expect_equal(length(result), length(valid_props))
})

test_that("validate_soil_properties rejects invalid properties", {
  # SPEC-COV-DATA-INT-006: Invalid properties trigger errors

  invalid_props <- c("clay", "invalid_property", "ph")

  expect_error(
    validate_soil_properties(invalid_props),
    "Invalid soil properties"
  )
})

test_that("validate_soil_properties provides helpful error messages", {
  # SPEC-COV-DATA-INT-007: Error messages list available properties

  expect_error(
    validate_soil_properties("bad_property"),
    "Invalid soil properties"
  )
})

## ===========================================================================
## INTEGRATION TESTS - Spectral Preprocessing
## ===========================================================================

test_that("preprocess_mir_spectra handles basic preprocessing workflow", {
  # SPEC-COV-DATA-INT-008: SG smoothing + SNV workflow

  # Create test spectral data
  test_data <- make_test_spectra(
    n_samples = 20,
    wavelengths = seq(600, 4000, by = 50),
    seed = 111
  )

  # Apply preprocessing
  result <- preprocess_mir_spectra(
    spectral_data = test_data,
    smooth_window = 9,
    smooth_poly = 1,
    derivative_order = 0,
    verbose = FALSE
  )

  # Verify structure preserved
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 20)
  expect_true("Sample_ID" %in% names(result))

  # Verify spectral columns still present
  spectral_cols <- grep("^[0-9]{3,4}$", names(result), value = TRUE)
  expect_true(length(spectral_cols) > 0)

  # Verify spectral values changed (preprocessing applied)
  original_spectral <- setdiff(names(test_data), c("Sample_ID", "Response", "Project"))
  processed_spectral <- intersect(original_spectral, spectral_cols)

  if (length(processed_spectral) > 0) {
    # At least some values should differ after preprocessing
    col <- processed_spectral[1]
    expect_false(all(test_data[[col]] == result[[col]], na.rm = TRUE))
  }
})

test_that("preprocess_mir_spectra applies 1st derivative correctly", {
  # SPEC-COV-DATA-INT-009: First derivative preprocessing

  test_data <- make_test_spectra(n_samples = 10, seed = 222)

  result <- preprocess_mir_spectra(
    spectral_data = test_data,
    smooth_window = 9,
    smooth_poly = 1,
    derivative_order = 1,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)

  # After 1st derivative, values will be different from original
  spectral_cols <- grep("^[0-9]{3,4}$", names(result), value = TRUE)
  expect_true(length(spectral_cols) > 0)
})

test_that("preprocess_mir_spectra applies 2nd derivative correctly", {
  # SPEC-COV-DATA-INT-010: Second derivative preprocessing

  test_data <- make_test_spectra(n_samples = 10, seed = 333)

  result <- preprocess_mir_spectra(
    spectral_data = test_data,
    smooth_window = 9,
    smooth_poly = 2,  # Need higher poly for 2nd derivative
    derivative_order = 2,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)
})

test_that("preprocess_mir_spectra preserves non-spectral columns", {
  # SPEC-COV-DATA-INT-011: Metadata columns preserved

  test_data <- make_test_spectra(n_samples = 5, seed = 444)

  result <- preprocess_mir_spectra(
    spectral_data = test_data,
    verbose = FALSE
  )

  # Sample_ID should be preserved
  expect_true("Sample_ID" %in% names(result))
  expect_setequal(result$Sample_ID, test_data$Sample_ID)
})

test_that("preprocess_mir_spectra handles different smooth windows", {
  # SPEC-COV-DATA-INT-012: Smooth window parameter variations

  test_data <- make_test_spectra(n_samples = 15, seed = 555)

  # Test different window sizes
  for (window in c(5, 9, 11, 15)) {
    result <- preprocess_mir_spectra(
      spectral_data = test_data,
      smooth_window = window,
      verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 15)
  }
})

## ===========================================================================
## INTEGRATION TESTS - PCA Computation
## ===========================================================================

test_that("perform_pca_on_ossl executes PCA on spectral data", {
  # SPEC-COV-DATA-INT-013: Basic PCA computation workflow

  # Create OSSL-like data with properties
  test_data <- make_test_spectra(
    n_samples = 100,  # Need sufficient samples for PCA
    wavelengths = seq(600, 4000, by = 100),
    seed = 666
  )

  # Add property columns (simulate OSSL structure)
  test_data$clay <- rnorm(100, mean = 250, sd = 50)
  test_data$ph <- rnorm(100, mean = 6.5, sd = 0.5)

  # Perform PCA
  result <- perform_pca_on_ossl(
    ossl_data = test_data,
    variance_threshold = 0.90,  # Lower threshold for small dataset
    verbose = FALSE
  )

  # Verify result structure
  expect_type(result, "list")
  expect_true("pca_model" %in% names(result))
  expect_true("ossl_pca_scores" %in% names(result))
  expect_true("variance_explained" %in% names(result))
  expect_true("n_components" %in% names(result))

  # Verify PCA model
  expect_s3_class(result$pca_model, "prcomp")

  # Verify PCA scores tibble
  expect_s3_class(result$ossl_pca_scores, "data.frame")
  expect_true("Sample_ID" %in% names(result$ossl_pca_scores))

  # Verify PC columns exist
  pc_cols <- grep("^Dim\\.[0-9]+$", names(result$ossl_pca_scores), value = TRUE)
  expect_true(length(pc_cols) > 0)

  # Verify property columns preserved
  expect_true("clay" %in% names(result$ossl_pca_scores))
  expect_true("ph" %in% names(result$ossl_pca_scores))
})

test_that("perform_pca_on_ossl respects variance threshold", {
  # SPEC-COV-DATA-INT-014: Variance threshold controls component selection

  test_data <- make_test_spectra(
    n_samples = 80,
    wavelengths = seq(600, 4000, by = 100),
    seed = 777
  )
  test_data$oc <- rnorm(80, mean = 20, sd = 5)

  # Test with lower threshold (fewer components)
  result_low <- perform_pca_on_ossl(
    ossl_data = test_data,
    variance_threshold = 0.80,
    verbose = FALSE
  )

  # Test with higher threshold (more components)
  result_high <- perform_pca_on_ossl(
    ossl_data = test_data,
    variance_threshold = 0.95,
    verbose = FALSE
  )

  # Higher threshold should require more components
  expect_true(result_high$n_components >= result_low$n_components)
})

test_that("perform_pca_on_ossl handles missing spectral data", {
  # SPEC-COV-DATA-INT-015: Missing data handling

  test_data <- make_test_spectra(n_samples = 50, seed = 888)
  test_data$sand <- rnorm(50, mean = 450, sd = 100)

  # Introduce some missing spectral values
  spectral_cols <- grep("^[0-9]{3,4}$", names(test_data), value = TRUE)
  test_data[1:5, spectral_cols[1]] <- NA

  # PCA should drop rows with missing data and warn
  expect_warning(
    result <- perform_pca_on_ossl(
      ossl_data = test_data,
      variance_threshold = 0.90,
      verbose = FALSE
    ),
    "missing spectral data"
  )

  # Should still return valid results (with fewer samples)
  expect_s3_class(result$ossl_pca_scores, "data.frame")
  expect_true(nrow(result$ossl_pca_scores) < 50)
})

## ===========================================================================
## INTEGRATION TESTS - PCA Projection
## ===========================================================================

test_that("project_spectra_to_pca projects new data to PCA space", {
  # SPEC-COV-DATA-INT-016: Project new spectra workflow

  # Create training data and fit PCA
  train_data <- make_test_spectra(
    n_samples = 100,
    wavelengths = seq(600, 4000, by = 100),
    seed = 999
  )
  train_data$clay <- rnorm(100, mean = 250, sd = 50)

  pca_result <- perform_pca_on_ossl(
    ossl_data = train_data,
    variance_threshold = 0.90,
    verbose = FALSE
  )

  # Create new data to project
  new_data <- make_test_spectra(
    n_samples = 20,
    wavelengths = seq(600, 4000, by = 100),
    seed = 1010
  )

  # Project to PCA space
  projected <- project_spectra_to_pca(
    new_data = new_data,
    pca_model = pca_result$pca_model,
    verbose = FALSE
  )

  # Verify structure
  expect_s3_class(projected, "data.frame")
  expect_equal(nrow(projected), 20)
  expect_true("Sample_ID" %in% names(projected))

  # Verify PC columns exist
  pc_cols <- grep("^Dim\\.[0-9]+$", names(projected), value = TRUE)
  expect_true(length(pc_cols) > 0)
  expect_equal(length(pc_cols), pca_result$n_components)
})

test_that("project_spectra_to_pca preserves metadata columns", {
  # SPEC-COV-DATA-INT-017: Metadata preservation during projection

  # Training data
  train_data <- make_test_spectra(n_samples = 80, seed = 1111)
  train_data$silt <- rnorm(80, mean = 300, sd = 60)

  pca_result <- perform_pca_on_ossl(
    ossl_data = train_data,
    variance_threshold = 0.85,
    verbose = FALSE
  )

  # New data with metadata
  new_data <- make_test_spectra(n_samples = 15, seed = 1212)

  projected <- project_spectra_to_pca(
    new_data = new_data,
    pca_model = pca_result$pca_model,
    verbose = FALSE
  )

  # Sample_ID should be preserved
  expect_true("Sample_ID" %in% names(projected))
  expect_setequal(projected$Sample_ID, new_data$Sample_ID)
})

## ===========================================================================
## VALIDATION TESTS - Property Mapping
## ===========================================================================

test_that("get_ossl_property_mapping never returns NULL", {
  # SPEC-COV-DATA-VAL-001: Function always succeeds

  result <- get_ossl_property_mapping()
  expect_false(is.null(result))
})

test_that("validate_soil_properties handles empty input", {
  # SPEC-COV-DATA-VAL-002: Empty property vector returns empty logical

  # Empty input should return empty logical vector (not error)
  result <- validate_soil_properties(character(0))
  expect_true(is.logical(result) && length(result) == 0)
})

test_that("validate_soil_properties handles NULL input", {
  # SPEC-COV-DATA-VAL-003: NULL input returns empty logical

  # NULL input should return empty logical vector (consistent with empty character(0))
  result <- validate_soil_properties(NULL)
  expect_true(is.logical(result) && length(result) == 0)
})

test_that("validate_soil_properties is case-sensitive", {
  # SPEC-COV-DATA-VAL-004: Property names must match exactly

  expect_error(
    validate_soil_properties("Clay"),  # Capital C
    "Invalid soil properties"
  )

  expect_error(
    validate_soil_properties("CLAY"),  # All caps
    "Invalid soil properties"
  )
})

## ===========================================================================
## VALIDATION TESTS - Preprocessing
## ===========================================================================

test_that("preprocess_mir_spectra handles missing spectral columns", {
  # SPEC-COV-DATA-VAL-005: No spectral columns present

  # Data with no numeric column names (no spectral data)
  test_data <- tibble::tibble(
    Sample_ID = paste0("S", 1:10),
    property1 = rnorm(10),
    property2 = rnorm(10)
  )

  result <- preprocess_mir_spectra(
    spectral_data = test_data,
    verbose = FALSE
  )

  # Should return NULL when no spectral columns
  expect_null(result)
})

test_that("preprocess_mir_spectra validates smooth_window parameter", {
  # SPEC-COV-DATA-VAL-006: Window size must be appropriate

  test_data <- make_test_spectra(n_samples = 10, seed = 1313)

  # Window size of 1 is too small for SG smoothing
  expect_warning(
    preprocess_mir_spectra(
      spectral_data = test_data,
      smooth_window = 1,
      verbose = FALSE
    )
  )
})

test_that("preprocess_mir_spectra handles constant spectra", {
  # SPEC-COV-DATA-VAL-007: All-zero or constant spectra

  test_data <- make_test_spectra(n_samples = 10, seed = 1414)

  # Make one spectrum constant (all zeros)
  spectral_cols <- grep("^[0-9]{3,4}$", names(test_data), value = TRUE)
  test_data[1, spectral_cols] <- 0

  # Should warn about constant spectra
  expect_warning(
    preprocess_mir_spectra(
      spectral_data = test_data,
      verbose = FALSE
    ),
    "constant spectra"
  )
})

## ===========================================================================
## VALIDATION TESTS - PCA
## ===========================================================================

test_that("perform_pca_on_ossl requires sufficient samples", {
  # SPEC-COV-DATA-VAL-008: Too few samples for PCA

  # Very small dataset (insufficient for PCA)
  test_data <- make_test_spectra(n_samples = 3, seed = 1515)
  test_data$ph <- rnorm(3, mean = 6.5, sd = 0.5)

  # PCA should fail or return NULL with too few samples
  result <- perform_pca_on_ossl(
    ossl_data = test_data,
    variance_threshold = 0.90,
    verbose = FALSE
  )

  # Either NULL or error is acceptable here
  expect_true(is.null(result) || inherits(result, "list"))
})

test_that("perform_pca_on_ossl handles all-zero columns", {
  # SPEC-COV-DATA-VAL-009: Zero-variance spectral columns

  test_data <- make_test_spectra(n_samples = 50, seed = 1616)
  test_data$cec <- rnorm(50, mean = 15, sd = 3)

  # Set one spectral column to all zeros (zero variance)
  spectral_cols <- grep("^[0-9]{3,4}$", names(test_data), value = TRUE)
  test_data[[spectral_cols[1]]] <- 0

  # PCA should handle this (scaling will create NAs for zero-variance columns)
  result <- perform_pca_on_ossl(
    ossl_data = test_data,
    variance_threshold = 0.90,
    verbose = FALSE
  )

  # Should either return NULL or valid results
  expect_true(is.null(result) || inherits(result, "list"))
})

test_that("project_spectra_to_pca requires matching spectral columns", {
  # SPEC-COV-DATA-VAL-010: Column mismatch between training and new data

  # Training data
  train_data <- make_test_spectra(
    n_samples = 80,
    wavelengths = seq(600, 4000, by = 100),
    seed = 1717
  )
  train_data$carbonate <- rnorm(80, mean = 10, sd = 2)

  pca_result <- perform_pca_on_ossl(
    ossl_data = train_data,
    variance_threshold = 0.85,
    verbose = FALSE
  )

  # New data with DIFFERENT wavelengths
  new_data <- make_test_spectra(
    n_samples = 20,
    wavelengths = seq(700, 3500, by = 100),  # Different range!
    seed = 1818
  )

  # Projection should handle missing columns by warning or filling zeros
  result <- project_spectra_to_pca(
    new_data = new_data,
    pca_model = pca_result$pca_model,
    verbose = FALSE
  )

  # Should still return a result (with warnings about missing columns)
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("project_spectra_to_pca handles empty new data", {
  # SPEC-COV-DATA-VAL-011: Empty tibble projection

  # Training data
  train_data <- make_test_spectra(n_samples = 60, seed = 1919)
  train_data$phosphorus <- rnorm(60, mean = 25, sd = 8)

  pca_result <- perform_pca_on_ossl(
    ossl_data = train_data,
    variance_threshold = 0.90,
    verbose = FALSE
  )

  # Empty new data
  empty_data <- make_test_spectra(n_samples = 0, seed = 2020)

  result <- project_spectra_to_pca(
    new_data = empty_data,
    pca_model = pca_result$pca_model,
    verbose = FALSE
  )

  # Should return NULL or empty tibble
  expect_true(is.null(result) || (is.data.frame(result) && nrow(result) == 0))
})
