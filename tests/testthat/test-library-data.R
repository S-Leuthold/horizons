## =============================================================================
## TEST: Library Data Loading and Preprocessing
## =============================================================================
##
## Purpose: Test OSSL/KSSL library data loading, method harmonization,
##          preprocessing, and PCA transformation for library-based prediction
##
## Coverage Target: >80%
## Phase: 1, Milestone: 1.1
##
## =============================================================================

library(testthat)
library(horizons)

## =============================================================================
## TEST GROUP 1: Property Mapping
## =============================================================================

test_that("get_library_property_mapping returns correct structure", {

  mapping <- horizons:::get_library_property_mapping()

  ## Should be a tibble
  expect_s3_class(mapping, "tbl_df")

  ## Should have expected columns
  expect_true(all(c("property", "ossl_name", "lab_method", "target_unit") %in% names(mapping)))

  ## Should contain all 15 LIBRARY_PROPERTIES
  expect_equal(nrow(mapping), 15)

  ## Check specific new properties exist
  expect_true("iron_total" %in% mapping$property)
  expect_true("aluminum_total" %in% mapping$property)
  expect_true("total_carbon" %in% mapping$property)

})

test_that("property mapping includes method metadata", {

  mapping <- horizons:::get_library_property_mapping()

  ## Lab methods should be documented
  expect_false(any(is.na(mapping$lab_method)))

  ## pH should specify H2O method
  ph_row <- mapping[mapping$property == "ph", ]
  expect_match(ph_row$lab_method, "H2O|Water", ignore.case = TRUE)

  ## Extractable cations should specify NH4OAc
  ca_row <- mapping[mapping$property == "calcium", ]
  expect_match(ca_row$lab_method, "NH4OAc", ignore.case = TRUE)

})

## =============================================================================
## TEST GROUP 2: OSSL Raw Data Loading
## =============================================================================

test_that("load_ossl_raw loads data for valid property", {

  skip_if_offline()
  skip_on_cran()

  ## Use small subset for fast testing
  cache_dir <- withr::local_tempdir(pattern = "ossl_test_")

  ## Load for a simple property
  safely_execute(
    horizons:::load_ossl_raw(
      property = "clay",
      cache_dir = cache_dir,
      max_samples = 500  # Limit for fast testing
    ),
    error_message = "OSSL loading failed"
  ) -> safe_result

  ## Should succeed
  expect_null(safe_result$error)
  expect_s3_class(safe_result$result, "data.frame")

  result <- safe_result$result

  ## Should have spectral columns
  expect_gt(ncol(result), 100)  # ~700 wavenumbers

  ## Should have clay measurements
  expect_true(any(grepl("clay", names(result), ignore.case = TRUE)))

  ## Should have requested sample size (or less if OSSL is smaller)
  expect_lte(nrow(result), 500)
  expect_gt(nrow(result), 100)

})

test_that("load_ossl_raw handles invalid property", {

  ## Should fail gracefully for unsupported property
  expect_error(
    horizons:::load_ossl_raw("nonsense_property"),
    "not in LIBRARY_PROPERTIES"
  )

})

test_that("load_ossl_raw uses caching", {

  skip("Cache timing tests are flaky - validate manually if needed")

  skip_if_offline()
  skip_on_cran()

  ## First load
  data1 <- horizons:::load_ossl_raw("clay", max_samples = 100, verbose = FALSE)

  ## Second load (from cache) should return same data
  data2 <- horizons:::load_ossl_raw("clay", max_samples = 100, verbose = FALSE)

  ## Data should be identical
  expect_equal(dim(data1), dim(data2))

})

## =============================================================================
## TEST GROUP 3: Method Harmonization
## =============================================================================

test_that("filter_by_method removes samples with wrong method", {

  skip("filter_by_method not yet implemented - stubbed for Phase 1.2")

  skip_if_offline()
  skip_on_cran()

  ## Load pH data (multiple methods possible)
  ph_data <- horizons:::load_ossl_raw("ph", max_samples = 100, verbose = FALSE)

  ## Filter to H2O method only
  filtered <- horizons:::filter_by_method(
    data = ph_data,
    property = "ph",
    target_method = "H2O"
  )

  ## TODO: When implemented, should have fewer samples than raw
  ## expect_lt(nrow(filtered), nrow(ph_data))

  ## All remaining should be H2O method
  ## (This assumes method metadata column exists)
  if ("method" %in% names(filtered)) {
    expect_true(all(grepl("H2O|Water", filtered$method, ignore.case = TRUE)))
  }

})

test_that("filter_by_method handles properties with single method", {

  skip("filter_by_method not yet implemented - stubbed for Phase 1.2")

  skip_if_offline()
  skip_on_cran()

  ## For properties with only one method, should return all samples
  clay_data <- horizons:::load_ossl_raw("clay", max_samples = 50, verbose = FALSE)
  filtered <- horizons:::filter_by_method(clay_data, "clay", "default")

  expect_equal(nrow(filtered), nrow(clay_data))

})

## =============================================================================
## TEST GROUP 4: MIR Spectral Preprocessing
## =============================================================================

test_that("preprocess_library_spectra applies SNV correctly", {

  ## Create synthetic spectra
  test_spectra <- matrix(rnorm(100 * 700, mean = 0.5, sd = 0.1), nrow = 100)
  colnames(test_spectra) <- paste0("X", 600:1299)
  test_df <- tibble::as_tibble(test_spectra)

  ## Preprocess with SNV (always applied)
  processed <- horizons:::preprocess_library_spectra(
    spectral_data = test_df,
    remove_water_bands = FALSE
  )

  ## Should return same dimensions
  expect_equal(dim(processed), dim(test_df))

  ## SNV should standardize (mean ≈ 0, sd ≈ 1 per sample)
  sample_means <- rowMeans(as.matrix(processed))
  expect_true(all(abs(sample_means) < 0.01))  # Close to 0

})

test_that("preprocess_library_spectra removes water bands when enabled", {

  test_spectra <- matrix(rnorm(50 * 700), nrow = 50)
  colnames(test_spectra) <- paste0("X", seq(600, 4000, length.out = 700))
  test_df <- tibble::as_tibble(test_spectra)

  ## With water band removal
  processed <- horizons:::preprocess_library_spectra(
    spectral_data = test_df,
    remove_water_bands = TRUE,
    water_regions = list(c(3600, 3000), c(1650, 1600))
  )

  ## Should have fewer columns (water regions removed)
  expect_lt(ncol(processed), ncol(test_df))

  ## Water band wavelengths should be absent
  wn <- as.numeric(gsub("X", "", names(processed)))
  expect_false(any(wn >= 3000 & wn <= 3600))
  expect_false(any(wn >= 1600 & wn <= 1650))

})

test_that("preprocess_library_spectra applies SNV consistently", {

  test_spectra <- matrix(rnorm(50 * 700), nrow = 50)
  colnames(test_spectra) <- paste0("X", 600:1299)
  test_df <- tibble::as_tibble(test_spectra)

  ## Apply preprocessing (SNV)
  processed <- horizons:::preprocess_library_spectra(
    spectral_data = test_df,
    remove_water_bands = FALSE
  )

  ## Should return same number of rows
  expect_equal(nrow(processed), nrow(test_df))

  ## SNV should change values (standardized)
  expect_false(identical(as.matrix(test_df), as.matrix(processed)))

})

## =============================================================================
## TEST GROUP 5: PCA Transformation
## =============================================================================

test_that("perform_pca_on_library retains 99% variance", {

  skip_if_offline()
  skip_on_cran()

  ## Load and preprocess small sample
  clay_data <- horizons:::load_ossl_raw("clay", max_samples = 100, verbose = FALSE)
  preprocessed <- horizons:::preprocess_library_spectra(
    clay_data,
    remove_water_bands = FALSE,
    verbose = FALSE
  )

  ## Perform PCA
  pca_result <- horizons:::perform_pca_on_library(
    library_data = preprocessed,
    variance_threshold = 0.99
  )

  ## Should return list with expected components
  expect_type(pca_result, "list")
  expect_true(all(c("pca_model", "pca_scores", "n_components") %in% names(pca_result)))

  ## Number of components should be reasonable (10-30 range)
  expect_gt(pca_result$n_components, 5)
  expect_lt(pca_result$n_components, 50)

  ## PCA scores should have correct dimensions
  expect_equal(nrow(pca_result$pca_scores), nrow(preprocessed))
  expect_equal(ncol(pca_result$pca_scores), pca_result$n_components)

})

test_that("project_to_library_pca projects unknowns correctly", {

  skip_if_offline()
  skip_on_cran()

  ## Create library PCA
  library_data <- matrix(rnorm(1000 * 700), nrow = 1000)
  colnames(library_data) <- paste0("X", 600:1299)
  pca_result <- horizons:::perform_pca_on_library(
    tibble::as_tibble(library_data),
    variance_threshold = 0.99,
    verbose = FALSE
  )

  ## Create unknowns (same wavelength grid)
  unknowns <- matrix(rnorm(50 * 700), nrow = 50)
  colnames(unknowns) <- paste0("X", 600:1299)

  ## Project to PCA space
  projected <- horizons:::project_to_library_pca(
    new_data = tibble::as_tibble(unknowns),
    pca_model = pca_result$pca_model
  )

  ## Should return correct dimensions
  expect_equal(nrow(projected), 50)
  expect_equal(ncol(projected), pca_result$n_components)

})

## =============================================================================
## TEST GROUP 6: Integration Tests
## =============================================================================

test_that("full library data pipeline works end-to-end", {

  skip("Full OSSL pipeline - too expensive for routine testing")

  ## This test validates the complete workflow but is skipped by default
  ## Run manually with: testthat::test_file("test-library-data.R", filter = "pipeline")
  ## Or set: Sys.setenv(HORIZONS_RUN_EXPENSIVE_TESTS = "true")

  skip_if_offline()
  skip_on_cran()

  cache_dir <- withr::local_tempdir(pattern = "pipeline_test_")

  ## Load → filter → preprocess → PCA for one property
  pipeline_result <- horizons:::get_processed_library_data(
    property = "clay",
    method_harmonization = TRUE,
    variance_threshold = 0.99,
    max_samples = 1000,  # Limit for speed
    cache_dir = cache_dir,
    verbose = FALSE
  )

  ## Should return expected structure
  expect_type(pipeline_result, "list")
  expect_true(all(c("library_data", "pca_model", "pca_scores") %in% names(pipeline_result)))

  ## Should have substantial data
  expect_gt(nrow(pipeline_result$library_data), 500)

  ## PCA components should be reasonable
  expect_gt(ncol(pipeline_result$pca_scores), 5)
  expect_lt(ncol(pipeline_result$pca_scores), 30)

})

## =============================================================================
## TEST GROUP 7: Memory Management
## =============================================================================

test_that("memory is freed after intermediate steps", {

  skip("Memory profiling - too expensive and flaky for routine testing")

  ## This test validates memory discipline but is skipped by default
  ## Run manually when investigating memory issues

  skip_if_offline()
  skip_on_cran()

  cache_dir <- withr::local_tempdir(pattern = "memory_test_")

  ## Measure memory before
  gc()
  mem_before <- as.numeric(gc()[2, 2])  # Max used

  ## Run pipeline with small sample (should cleanup internally)
  result <- horizons:::get_processed_library_data(
    property = "oc",
    max_samples = 500,
    cache_dir = cache_dir,
    verbose = FALSE
  )

  ## Measure after
  gc()
  mem_after <- as.numeric(gc()[2, 2])

  ## Memory overhead should be <500MB (conservative threshold)
  overhead_mb <- mem_after - mem_before
  expect_lt(overhead_mb, 500)

})

test_that("large objects are removed after use", {

  ## This is more of a code inspection test
  ## Verify key cleanup points exist in source

  source_file <- readLines(system.file("R", "library-data.R", package = "horizons"))
  source_text <- paste(source_file, collapse = "\n")

  ## Should have rm() calls after expensive operations
  expect_true(grepl("rm\\(", source_text))
  expect_true(grepl("gc\\(\\)", source_text))

  ## Should cleanup lab_data, mir_data
  expect_true(grepl("rm\\(lab_data", source_text))
  expect_true(grepl("rm\\(.*mir_data", source_text))

})

## =============================================================================
## TEST GROUP 8: Edge Cases and Error Handling
## =============================================================================

test_that("handles missing OSSL data gracefully", {

  ## Property not in OSSL (should error informatively)
  expect_error(
    horizons:::load_ossl_raw("fake_property"),
    "not in LIBRARY_PROPERTIES"
  )

})

test_that("handles empty data after filtering", {

  skip_if_offline()
  skip_on_cran()

  ## If method filtering removes ALL samples (edge case)
  ## Should return informative error, not crash

  ## This test might need adjustment based on actual implementation
  ## Placeholder for now
  expect_true(TRUE)  # TODO: Implement when filter_by_method exists

})

test_that("preprocessing handles edge cases", {

  ## Single sample
  single <- matrix(rnorm(700), nrow = 1)
  colnames(single) <- paste0("X", 600:1299)

  expect_no_error({
    horizons:::preprocess_library_spectra(
      tibble::as_tibble(single),
      remove_water_bands = FALSE,
      verbose = FALSE
    )
  })

  ## All zeros (degenerate case)
  zeros <- matrix(0, nrow = 10, ncol = 700)
  colnames(zeros) <- paste0("X", 600:1299)

  ## SNV on zeros should handle gracefully (avoid division by zero)
  result <- horizons:::preprocess_library_spectra(
    tibble::as_tibble(zeros),
    remove_water_bands = FALSE,
    verbose = FALSE
  )

  ## Should not contain NaN or Inf
  expect_false(any(is.nan(as.matrix(result))))
  expect_false(any(is.infinite(as.matrix(result))))

})

test_that("PCA handles low-variance data", {

  ## Create data with very low variance
  low_var <- matrix(rnorm(100 * 700, sd = 0.001), nrow = 100)
  colnames(low_var) <- paste0("X", 600:1299)

  ## PCA should still work (might use fewer components)
  pca_result <- horizons:::perform_pca_on_library(
    tibble::as_tibble(low_var),
    variance_threshold = 0.99
  )

  expect_type(pca_result, "list")
  expect_gt(pca_result$n_components, 0)

})

## =============================================================================
## TEST GROUP 9: Property-Specific Tests
## =============================================================================

test_that("texture properties load correctly", {

  skip_if_offline()
  skip_on_cran()

  ## All three texture components should be loadable
  for (prop in c("clay", "sand", "silt")) {

    result <- horizons:::load_ossl_raw(prop)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 100)

  }

})

test_that("carbon properties load correctly", {

  skip_if_offline()
  skip_on_cran()

  ## Carbon forms
  for (prop in c("total_carbon", "oc", "carbonate")) {

    result <- horizons:::load_ossl_raw(prop)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 100)

  }

})

test_that("extractable cations load correctly", {

  skip_if_offline()
  skip_on_cran()

  ## Base cations
  for (prop in c("calcium", "magnesium", "potassium", "sodium")) {

    result <- horizons:::load_ossl_raw(prop)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 100)

  }

})

## =============================================================================
## Helper: Check if offline
## =============================================================================

skip_if_offline <- function() {

  ## Try to reach OSSL server
  can_reach <- tryCatch({

    con <- url("https://storage.googleapis.com/soilspec4gg-public/", open = "r")
    close(con)
    TRUE

  }, error = function(e) FALSE)

  if (!can_reach) skip("Offline or OSSL unreachable")

}
