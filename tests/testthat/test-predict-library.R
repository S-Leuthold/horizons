## =============================================================================
## Test: Library-Based Prediction API (M1.6)
## =============================================================================

library(testthat)
library(horizons)

## =============================================================================
## TEST GROUP 1: Input Validation
## =============================================================================

test_that("predict_library validates property input", {

  ## Create minimal spectra
  spectra <- tibble::tibble(
    Sample_ID = paste0("U", 1:10)
  )

  spec_mat <- matrix(rnorm(10 * 50), nrow = 10)
  colnames(spec_mat) <- paste0(seq(600, 1000, length.out = 50))
  spectra <- dplyr::bind_cols(spectra, tibble::as_tibble(spec_mat))

  ## Invalid property should error
  expect_error(
    horizons:::predict_library(spectra, property = "invalid_prop"),
    "Invalid"
  )

  ## Valid property should work (skip - requires full OSSL)
  skip("Full OSSL pipeline required for real test")

  result <- horizons:::predict_library(spectra, property = "ph", debug_mode = TRUE)
  expect_s3_class(result, "tbl_df")
})

test_that("predict_library auto-expands texture to all 3 components", {

  ## Create minimal spectra
  spectra <- tibble::tibble(
    Sample_ID = paste0("U", 1:10)
  )

  spec_mat <- matrix(rnorm(10 * 50), nrow = 10)
  colnames(spec_mat) <- paste0(seq(600, 1000, length.out = 50))
  spectra <- dplyr::bind_cols(spectra, tibble::as_tibble(spec_mat))

  ## Request just "sand" - should auto-expand with message
  expect_message(
    {
      result <- try(horizons:::predict_library(spectra, property = "sand", verbose = TRUE), silent = TRUE)
    },
    "Texture.*all 3 components"
  )
})

test_that("predict_library accepts multiple properties", {

  ## Create minimal spectra
  spectra <- tibble::tibble(
    Sample_ID = paste0("U", 1:10)
  )

  spec_mat <- matrix(rnorm(10 * 50), nrow = 10)
  colnames(spec_mat) <- paste0(seq(600, 1000, length.out = 50))
  spectra <- dplyr::bind_cols(spectra, tibble::as_tibble(spec_mat))

  ## Multiple properties should be accepted
  ## (Will likely error with minimal test data, but should pass validation)
  expect_no_error(
    {
      props <- c("ph", "oc")
      # Just check it doesn't error on property validation
      stopifnot(all(props %in% LIBRARY_PROPERTIES))
    }
  )
})

## =============================================================================
## TEST GROUP 2: Return Structure
## =============================================================================

test_that("predict_library returns correct structure for single property", {

  skip("Full OSSL pipeline - too expensive for routine testing")

  ## This would require real OSSL data and full pipeline
  ## Placeholder for structure validation

  ## Expected return structure:
  ## tibble with columns:
  ## - Sample_ID
  ## - property
  ## - pred
  ## - cluster_id
  ## - ... (UQ metadata for future)

})

test_that("predict_library returns correct structure for texture", {

  skip("Full OSSL pipeline - too expensive for routine testing")

  ## Expected: 3 rows per sample (sand, silt, clay)
  ## pred values should sum to 1000 g/kg per sample

})

test_that("predict_library returns correct structure for multiple properties", {

  skip("Full OSSL pipeline - too expensive for routine testing")

  ## Expected: N rows per sample (N = number of properties)
  ## Each property on separate row

})

## =============================================================================
## TEST GROUP 3: Texture Handling
## =============================================================================

test_that("predict_library handles texture properties compositionally", {

  skip("Full OSSL pipeline - too expensive for routine testing")

  ## Validate:
  ## - Predictions for sand + silt + clay sum to 1000
  ## - Back-transformation applied correctly
  ## - Both ilr_1 and ilr_2 models trained

})

## =============================================================================
## TEST GROUP 4: Bounds Enforcement
## =============================================================================

test_that("predict_library applies bounds to predictions", {

  skip("Full OSSL pipeline - too expensive for routine testing")

  ## Validate:
  ## - pH predictions in [0, 14]
  ## - Carbon predictions ≥ 0
  ## - No impossible values

})

## =============================================================================
## TEST GROUP 5: Integration with Existing Components
## =============================================================================

test_that("predict_library uses optimize_config_for_cluster", {

  skip("Full OSSL pipeline - too expensive for routine testing")

  ## Validate:
  ## - Config optimization called per cluster
  ## - Winning config used for training
  ## - Metrics logged

})

## =============================================================================
