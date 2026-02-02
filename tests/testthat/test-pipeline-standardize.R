# tests/testthat/test-pipeline-standardize.R
# Tests for standardize() function


## =============================================================================
## Test Fixtures
## =============================================================================

#' Create test horizons_data object with spectral data
#' @noRd
create_test_spectra <- function(n_samples    = 10,
                                n_wavelengths = 100,
                                wn_min        = 600,
                                wn_max        = 4000,
                                resolution    = NULL) {

  ## Calculate resolution if not provided --------------------------------------

  if (is.null(resolution)) {

    resolution <- (wn_max - wn_min) / (n_wavelengths - 1)

  }

  ## Generate wavelengths (decreasing order) -----------------------------------

  wavelengths <- seq(from = wn_max, to = wn_min, by = -resolution)
  wavelengths <- wavelengths[1:min(n_wavelengths, length(wavelengths))]

  ## Generate spectral data ----------------------------------------------------

  set.seed(42)
  spectra <- matrix(
    runif(n_samples * length(wavelengths), min = 0.1, max = 0.8),
    nrow = n_samples,
    ncol = length(wavelengths)
  )

  ## Build tibble --------------------------------------------------------------

  col_names <- paste0("wn_", wavelengths)
  colnames(spectra) <- col_names

  data <- tibble::tibble(
    sample_id = paste0("sample_", seq_len(n_samples))
  )

  data <- dplyr::bind_cols(data, tibble::as_tibble(spectra))

  ## Create horizons_data object -----------------------------------------------

  spectra(data)

}


## =============================================================================
## Input Validation Tests
## =============================================================================

test_that("standardize() rejects non-horizons_data input", {

  expect_error(
    standardize(data.frame(x = 1:10)),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(list(a = 1)),
    class = "horizons_input_error"
  )

})


test_that("standardize() validates resample parameter", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, resample = "two"),
    class = "horizons_input_error"
 )

  expect_error(
    standardize(hd, resample = -1),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, resample = c(2, 4)),
    class = "horizons_input_error"
  )

})


test_that("standardize() validates trim parameter", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, trim = 600),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, trim = c("600", "4000")),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, trim = c(4000, 600)),
    class = "horizons_input_error"
  )

})


test_that("standardize() validates logical parameters", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, remove_water = "yes"),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, baseline = NA),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, force = 1),
    class = "horizons_input_error"
  )

})


## =============================================================================
## Resampling Tests
## =============================================================================

test_that("resample_spectra() changes wavelength count", {

  ## Create data at 4 cm⁻¹ resolution ------------------------------------------

  hd <- create_test_spectra(n_wavelengths = 850, wn_min = 600, wn_max = 4000)

  ## Get original info ---------------------------------------------------------

  original_n <- hd$data$n_predictors

  ## Resample to 2 cm⁻¹ --------------------------------------------------------

  hd_resampled <- standardize(hd, resample = 2, trim = NULL,
                               remove_water = FALSE, baseline = FALSE)

  ## Check wavelength count increased ------------------------------------------

  expect_gt(hd_resampled$data$n_predictors, original_n)

})


test_that("standardize() with resample = NULL skips resampling", {

  hd <- create_test_spectra(n_wavelengths = 100)

  original_n <- hd$data$n_predictors

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  expect_equal(hd_result$data$n_predictors, original_n)

})


## =============================================================================
## Trimming Tests
## =============================================================================

test_that("trim_spectra() removes wavelengths outside range", {

  hd <- create_test_spectra(n_wavelengths = 200, wn_min = 400, wn_max = 4500)

  original_n <- hd$data$n_predictors

  ## Trim to 600-4000 ----------------------------------------------------------

  hd_trimmed <- standardize(hd, resample = NULL, trim = c(600, 4000),
                            remove_water = FALSE, baseline = FALSE)

  ## Check wavelength count decreased ------------------------------------------

  expect_lt(hd_trimmed$data$n_predictors, original_n)

  ## Check all wavelengths are within range ------------------------------------

  predictor_cols <- hd_trimmed$data$role_map$variable[
    hd_trimmed$data$role_map$role == "predictor"
  ]
  wavelengths <- as.numeric(gsub("^wn_", "", predictor_cols))

  expect_true(all(wavelengths >= 600))
  expect_true(all(wavelengths <= 4000))

})


test_that("standardize() with trim = NULL skips trimming", {

  hd <- create_test_spectra(n_wavelengths = 100)

  original_n <- hd$data$n_predictors

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  expect_equal(hd_result$data$n_predictors, original_n)

})


## =============================================================================
## Water Band Removal Tests
## =============================================================================

test_that("remove_water_bands() removes water absorption regions", {

  ## Create data spanning water bands ------------------------------------------

  hd <- create_test_spectra(n_wavelengths = 400, wn_min = 1500, wn_max = 3700)

  original_n <- hd$data$n_predictors

  ## Remove water bands --------------------------------------------------------

  hd_filtered <- standardize(hd, resample = NULL, trim = NULL,
                             remove_water = TRUE, baseline = FALSE)

  ## Check wavelength count decreased ------------------------------------------

  expect_lt(hd_filtered$data$n_predictors, original_n)

  ## Check water band regions are gone -----------------------------------------

  predictor_cols <- hd_filtered$data$role_map$variable[
    hd_filtered$data$role_map$role == "predictor"
  ]
  wavelengths <- as.numeric(gsub("^wn_", "", predictor_cols))

  ## OH bending: 1580-1720 (OSSL standard) -------------------------------------

  in_oh_bending <- wavelengths >= 1580 & wavelengths <= 1720
  expect_false(any(in_oh_bending))

  ## OH stretching: 3100-3700 (OSSL standard) ----------------------------------

  in_oh_stretching <- wavelengths >= 3100 & wavelengths <= 3700
  expect_false(any(in_oh_stretching))

})


test_that("standardize() with remove_water = FALSE skips water removal", {

  hd <- create_test_spectra(n_wavelengths = 100, wn_min = 1500, wn_max = 3700)

  original_n <- hd$data$n_predictors

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  expect_equal(hd_result$data$n_predictors, original_n)

})


## =============================================================================
## Baseline Correction Tests
## =============================================================================

test_that("baseline correction modifies spectral values", {

  hd <- create_test_spectra(n_wavelengths = 100)

  ## Get original spectral values ----------------------------------------------

  predictor_cols <- hd$data$role_map$variable[
    hd$data$role_map$role == "predictor"
  ]
  original_values <- as.matrix(hd$data$analysis[, predictor_cols])

  ## Apply baseline correction -------------------------------------------------

  hd_corrected <- standardize(hd, resample = NULL, trim = NULL,
                              remove_water = FALSE, baseline = TRUE)

  corrected_values <- as.matrix(hd_corrected$data$analysis[, predictor_cols])

  ## Values should be different ------------------------------------------------

  expect_false(all(original_values == corrected_values))

})


test_that("standardize() with baseline = FALSE skips correction", {

  hd <- create_test_spectra(n_wavelengths = 100)

  predictor_cols <- hd$data$role_map$variable[
    hd$data$role_map$role == "predictor"
  ]
  original_values <- as.matrix(hd$data$analysis[, predictor_cols])

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  result_values <- as.matrix(hd_result$data$analysis[, predictor_cols])

  expect_equal(original_values, result_values)

})


## =============================================================================
## Provenance Tests
## =============================================================================

test_that("standardize() updates provenance", {

  hd <- create_test_spectra()

  expect_null(hd$provenance$standardization)

  hd_std <- standardize(hd, resample = 2, trim = c(600, 4000),
                        remove_water = TRUE, baseline = TRUE)

  expect_false(is.null(hd_std$provenance$standardization))
  expect_equal(hd_std$provenance$standardization$resample, 2)
  expect_equal(hd_std$provenance$standardization$trim, c(600, 4000))
  expect_true(hd_std$provenance$standardization$remove_water)
  expect_true(hd_std$provenance$standardization$baseline)
  expect_s3_class(hd_std$provenance$standardization$applied_at, "POSIXct")

})


## =============================================================================
## Idempotence Tests
## =============================================================================

test_that("standardize() warns on already-standardized object", {

  hd <- create_test_spectra()

  hd_std <- standardize(hd, resample = NULL, trim = NULL,
                        remove_water = FALSE, baseline = FALSE)

  expect_warning(
    standardize(hd_std),
    "already standardized"
  )

})


test_that("standardize() with force = TRUE re-standardizes", {

  hd <- create_test_spectra(n_wavelengths = 200, wn_min = 400, wn_max = 4500)

  ## First standardization -----------------------------------------------------

  hd_std1 <- standardize(hd, resample = NULL, trim = c(500, 4200),
                         remove_water = FALSE, baseline = FALSE)

  n_after_first <- hd_std1$data$n_predictors

  ## Second standardization with force -----------------------------------------

  expect_warning(
    hd_std2 <- standardize(hd_std1, resample = NULL, trim = c(600, 4000),
                           remove_water = FALSE, baseline = FALSE, force = TRUE),
    "Re-standardizing"
  )

  ## Should have fewer wavelengths after tighter trim --------------------------

  expect_lt(hd_std2$data$n_predictors, n_after_first)

})


## =============================================================================
## Combined Operations Tests
## =============================================================================

test_that("standardize() applies operations in correct order", {

  ## Create data with wide range and coarse resolution -------------------------

  hd <- create_test_spectra(n_wavelengths = 200, wn_min = 400, wn_max = 4500)

  ## Apply all operations ------------------------------------------------------

  hd_full <- standardize(hd,
                         resample     = 2,
                         trim         = c(600, 4000),
                         remove_water = TRUE,
                         baseline     = TRUE)

  ## Check final wavelengths are in expected range -----------------------------

  predictor_cols <- hd_full$data$role_map$variable[
    hd_full$data$role_map$role == "predictor"
  ]
  wavelengths <- as.numeric(gsub("^wn_", "", predictor_cols))

  expect_true(all(wavelengths >= 600))
  expect_true(all(wavelengths <= 4000))

  ## Check water bands are removed ---------------------------------------------
  ## Note: Uses OSSL standard ranges (1580-1720, 3100-3700)

  in_water <- (wavelengths >= 1580 & wavelengths <= 1720) |
              (wavelengths >= 3100 & wavelengths <= 3700)
  expect_false(any(in_water))

  ## Check resolution is approximately 2 cm⁻¹ ----------------------------------

  wn_diff <- abs(diff(sort(wavelengths)))
  median_resolution <- median(wn_diff)
  expect_equal(median_resolution, 2, tolerance = 0.1)

})


test_that("standardize() preserves non-predictor columns", {
 hd <- create_test_spectra()

  ## Add a meta column ---------------------------------------------------------

  hd$data$analysis$batch <- rep("A", nrow(hd$data$analysis))
  hd$data$role_map <- rbind(
    hd$data$role_map,
    tibble::tibble(variable = "batch", role = "meta")
  )

  ## Standardize ---------------------------------------------------------------

  hd_std <- standardize(hd, resample = NULL, trim = NULL,
                        remove_water = FALSE, baseline = FALSE)

  ## Check sample_id preserved -------------------------------------------------

  expect_true("sample_id" %in% names(hd_std$data$analysis))

  ## Check meta column preserved -----------------------------------------------

  expect_true("batch" %in% names(hd_std$data$analysis))
  expect_equal(hd_std$data$analysis$batch, rep("A", nrow(hd_std$data$analysis)))

})


## =============================================================================
## Edge Cases
## =============================================================================

test_that("standardize() handles all parameters as NULL/FALSE", {

  hd <- create_test_spectra()

  ## This should be a no-op but still mark as standardized ---------------------

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  ## Data unchanged ------------------------------------------------------------

  expect_equal(hd$data$n_predictors, hd_result$data$n_predictors)

  ## Provenance still set (marks as evaluated) ---------------------------------

  expect_false(is.null(hd_result$provenance$standardization))

})


test_that("standardize() maintains valid horizons_data structure", {

  hd <- create_test_spectra()

  hd_std <- standardize(hd, resample = 2, trim = c(600, 4000),
                        remove_water = TRUE, baseline = TRUE)

  ## Should pass validation (implicitly tested by returning without error) -----

  expect_s3_class(hd_std, "horizons_data")

  ## Check structure -----------------------------------------------------------

  expect_true(!is.null(hd_std$data$analysis))
  expect_true(!is.null(hd_std$data$role_map))
  expect_true(!is.null(hd_std$data$n_predictors))

  ## Check role_map has correct columns ----------------------------------------

  expect_true(all(c("variable", "role") %in% names(hd_std$data$role_map)))

  ## Check all analysis columns are in role_map --------------------------------

  expect_true(all(names(hd_std$data$analysis) %in% hd_std$data$role_map$variable))

})
