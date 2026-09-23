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


test_that("resample_spectra(target_resolution =) uses the multiples inside the data's range, bit for bit the new_wav path", {

  ## Arrange: an axis offset from the multiples of 2, as an instrument's is
  set.seed(7)
  wn   <- seq(3999.57, 599.57, by = -4)
  m    <- matrix(stats::runif(5 * length(wn)), nrow = 5)
  grid <- seq(3998, 600, by = -2)

  ## Act
  by_res  <- resample_spectra(m, wn, target_resolution = 2)
  by_grid <- resample_spectra(m, wn, new_wav = grid)

  ## Assert: the grid is the multiples, not a sequence stepped from 3999.57
  expect_identical(by_res$wavelengths, grid)
  expect_identical(by_grid$matrix,      by_res$matrix)
  expect_identical(by_grid$wavelengths, by_res$wavelengths)
  expect_identical(by_grid$n_after,     by_res$n_after)

})


test_that("resample_spectra(new_wav =) returns exactly the grid given, decreasing", {

  set.seed(7)
  wn   <- seq(4000, 600, by = -4)
  m    <- matrix(stats::runif(3 * length(wn)), nrow = 3)
  grid <- c(3990, 3500, 2222, 1000.5, 604)

  out <- resample_spectra(m, wn, new_wav = grid)

  expect_identical(out$wavelengths, grid)
  expect_identical(ncol(out$matrix), length(grid))
  expect_true(all(diff(out$wavelengths) < 0))

  ## An increasing grid is accepted and returned decreasing
  out2 <- resample_spectra(m, wn, new_wav = rev(grid))
  expect_identical(out2$matrix, out$matrix)
  expect_identical(out2$wavelengths, grid)

})


test_that("resample_spectra(new_wav =) reproduces the input at its own knots", {

  set.seed(7)
  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(3 * length(wn)), nrow = 3)

  ## Every other column of the source grid
  grid <- wn[seq(1, length(wn), by = 2)]
  out  <- resample_spectra(m, wn, new_wav = grid)

  expect_equal(unname(out$matrix), m[, seq(1, length(wn), by = 2)], tolerance = 1e-12)

})


test_that("resample_spectra(new_wav =) refuses to extrapolate", {

  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(2 * length(wn)), nrow = 2)

  expect_error(resample_spectra(m, wn, new_wav = seq(4100, 600, by = -4)),
               regexp = "4100", class = "horizons_input_error")
  expect_error(resample_spectra(m, wn, new_wav = seq(4000, 500, by = -4)),
               regexp = "500", class = "horizons_input_error")

})


test_that("resample_spectra() needs exactly one of target_resolution and new_wav", {

  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(2 * length(wn)), nrow = 2)

  expect_error(resample_spectra(m, wn), class = "horizons_input_error")
  expect_error(resample_spectra(m, wn, target_resolution = 2, new_wav = wn),
               class = "horizons_input_error")

})


test_that("standardize() with resample = NULL skips resampling", {

  hd <- create_test_spectra(n_wavelengths = 100)

  original_n <- hd$data$n_predictors

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  expect_equal(hd_result$data$n_predictors, original_n)

})


## =============================================================================
## Canonical Grid Tests (#64, #18)
## =============================================================================
## Synthetic stand-ins for the two sources #64 was found on: a KSSL-shaped
## library (600 to 4000 at 2 cm-1, stored increasing, as the snapshot is) and
## MOYS-shaped scans (from 599.74 at 1.93 cm-1, well past 4000).

#' Evaluate without the pipeline's console tree
#' @noRd
no_output <- function(expr) {

  utils::capture.output(value <- expr)
  value

}

#' A horizons_data on an explicit axis, in the order given
#' @noRd
make_axis_spectra <- function(wn, f = NULL, n = 3) {

  set.seed(11)

  m <- if (is.null(f)) {
    matrix(stats::runif(n * length(wn), 0.1, 0.8), nrow = n)
  } else {
    t(vapply(seq_len(n), function(i) i * f(wn), numeric(length(wn))))
  }

  colnames(m) <- paste0("wn_", wn)

  no_output(spectra(dplyr::bind_cols(tibble::tibble(sample_id = paste0("s", seq_len(n))),
                                     tibble::as_tibble(m))))

}

#' A smooth absorbance-like spectrum: sloped baseline plus Gaussian bands,
#' two of them close to the trim bounds so the ends carry curvature
#' @noRd
smooth_spectrum <- function(wn) {

  0.3 + 5e-5 * (wn - 600) +
    0.10 * exp(-((wn -  640) / 30)^2) +
    0.40 * exp(-((wn - 1030) / 40)^2) +
    0.15 * exp(-((wn - 1630) / 50)^2) +
    0.25 * exp(-((wn - 2920) / 60)^2) +
    0.20 * exp(-((wn - 3620) / 25)^2) +
    0.10 * exp(-((wn - 3960) / 30)^2)

}

predictor_names <- function(hd) hd$data$role_map$variable[hd$data$role_map$role == "predictor"]

KSSL_WN <- seq(600, 4000, by = 2)
MOYS_WN <- 599.74 + 1.93 * (0:3574)


test_that("a KSSL-shaped axis stored increasing comes out on wn_4000 ... wn_600", {

  kssl <- make_axis_spectra(KSSL_WN)

  at_2 <- no_output(standardize(kssl, resample = 2, trim = c(600, 4000)))
  at_4 <- no_output(standardize(kssl, resample = 4, trim = c(600, 4000)))

  expect_identical(predictor_names(at_2), paste0("wn_", seq(4000, 600, by = -2)))
  expect_identical(predictor_names(at_4), paste0("wn_", seq(4000, 600, by = -4)))
  expect_identical(at_2$data$n_predictors, 1701L)
  expect_identical(at_4$data$n_predictors, 851L)

})


test_that("a MOYS-shaped axis lands on the same columns as a KSSL-shaped one", {

  kssl <- no_output(standardize(make_axis_spectra(KSSL_WN), resample = 4, trim = c(600, 4000)))
  moys <- no_output(standardize(make_axis_spectra(MOYS_WN), resample = 4, trim = c(600, 4000)))

  ## 599.74 sits just below the bound, and it is what lets 600 be a column
  expect_identical(predictor_names(moys), predictor_names(kssl))
  expect_identical(predictor_names(moys)[c(1, 851)], c("wn_4000", "wn_600"))

  expect_true(moys$provenance$standardization$resampled)
  expect_identical(moys$provenance$standardization$grid,
                   list(min = 600, max = 4000, step = 4, n = 851L))

})


test_that("grid points the data does not reach are dropped with a warning, never extrapolated", {

  ## Covers 651.5 to 3500: short of the trim bounds at both ends
  short <- make_axis_spectra(seq(3500, 651.5, by = -1.5))

  caught <- list()
  out <- withCallingHandlers(
    no_output(standardize(short, resample = 4, trim = c(600, 4000))),
    horizons_standardize_warning = function(w) {
      caught[[length(caught) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  ## One warning, naming how many points went and where (cli wraps the
  ## message, so compare with the whitespace collapsed)
  expect_length(caught, 1L)
  msg <- gsub("\\s+", " ", conditionMessage(caught[[1]]))
  expect_match(msg, "Dropped 138 grid points", fixed = TRUE)
  expect_match(msg, "13 below the data's start at 651.5 (600 to 648 cm-1)", fixed = TRUE)
  expect_match(msg, "125 above the data's end at 3500 (3504 to 4000 cm-1)", fixed = TRUE)

  ## The covered multiples survive, and nothing beyond the data does
  expect_identical(predictor_names(out), paste0("wn_", seq(3500, 652, by = -4)))
  expect_identical(out$provenance$standardization$grid$n, 713L)

})


test_that("an axis already on the canonical grid is not re-interpolated", {

  kssl <- make_axis_spectra(rev(KSSL_WN))
  before <- as.matrix(kssl$data$analysis[, predictor_names(kssl)])

  expect_output(
    out <- standardize(kssl, resample = 2, trim = c(600, 4000)),
    regexp = "already on the 2 cm"
  )

  ## Values untouched, bit for bit; a spline through its own knots would only
  ## agree to about 1e-12
  after <- as.matrix(out$data$analysis[, predictor_names(out)])
  expect_identical(after, before)
  expect_false(out$provenance$standardization$resampled)
  expect_identical(out$provenance$standardization$grid$n, 1701L)

  ## Stored increasing, the same spectra only change column order
  inc     <- make_axis_spectra(KSSL_WN)
  inc_out <- no_output(standardize(inc, resample = 2, trim = c(600, 4000)))
  inc_in  <- as.matrix(inc$data$analysis[, predictor_names(inc)])

  expect_identical(unname(as.matrix(inc_out$data$analysis[, predictor_names(inc_out)])),
                   unname(inc_in[, rev(seq_len(ncol(inc_in)))]))

})


test_that("increasing-order input with baseline correction gives the decreasing-order result", {

  ## The baseline helper reverses its output on the assumption of decreasing
  ## input; before the sort this mirrored increasing spectra end to end and
  ## still passed validation
  inc <- make_axis_spectra(KSSL_WN, f = smooth_spectrum)
  dec <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum)

  out_inc <- no_output(standardize(inc, resample = 4, baseline = TRUE))
  out_dec <- no_output(standardize(dec, resample = 4, baseline = TRUE))

  expect_identical(predictor_names(out_inc), predictor_names(out_dec))
  expect_equal(as.matrix(out_inc$data$analysis[, predictor_names(out_inc)]),
               as.matrix(out_dec$data$analysis[, predictor_names(out_dec)]))

})


test_that("interpolating a smooth spectrum onto the shifted grid stays on the true curve, edges included", {

  moys <- make_axis_spectra(MOYS_WN, f = smooth_spectrum, n = 1)

  for (res in c(4, 2, 1.5)) {

    out <- no_output(standardize(moys, resample = res, trim = c(600, 4000)))
    wn  <- as.numeric(sub("^wn_", "", predictor_names(out)))
    got <- as.numeric(out$data$analysis[1, predictor_names(out)])

    expect_lt(max(abs(got - smooth_spectrum(wn))), 1e-5)

    ## Both ends by name: no end effect from the margin point or the bound
    ends <- c(1, length(wn))
    expect_lt(max(abs(got[ends] - smooth_spectrum(wn[ends]))), 1e-6)

  }

})


test_that("non-integer resolutions give clean, stable column names", {

  moys <- make_axis_spectra(MOYS_WN)

  ## 1.5 cm-1: 3999, 3997.5, ..., 600
  at_1.5 <- no_output(standardize(moys, resample = 1.5, trim = c(600, 4000)))
  expect_identical(predictor_names(at_1.5), paste0("wn_", seq(3999, 600, by = -1.5)))

  ## 0.1 cm-1: the expected names are built from integers, so no float
  ## arithmetic reaches them
  at_0.1 <- no_output(standardize(moys, resample = 0.1, trim = c(600, 610)))
  k      <- 6100:6000
  expect_identical(predictor_names(at_0.1),
                   paste0("wn_", k %/% 10, ifelse(k %% 10 == 0, "", paste0(".", k %% 10))))

  ## A step with no finite decimal form is snapped to six decimal places
  fine     <- make_axis_spectra(seq(610, 590, by = -0.5))
  at_third <- no_output(standardize(fine, resample = 1 / 3, trim = c(600, 601)))
  expect_identical(predictor_names(at_third),
                   c("wn_601", "wn_600.666667", "wn_600.333333", "wn_600"))

})


test_that("resample = NULL keeps trim-as-subset but still sorts", {

  kssl <- make_axis_spectra(KSSL_WN)
  out  <- no_output(standardize(kssl, resample = NULL, trim = c(1000, 2000)))

  expect_identical(predictor_names(out), paste0("wn_", seq(2000, 1000, by = -2)))
  expect_false(out$provenance$standardization$resampled)
  expect_null(out$provenance$standardization$grid)

  ## A subset, with no margin: 599.74 is outside c(600, 4000) and stays out
  moys <- no_output(standardize(make_axis_spectra(MOYS_WN), resample = NULL, trim = c(600, 4000)))
  wn   <- as.numeric(sub("^wn_", "", predictor_names(moys)))
  expect_gte(min(wn), 600)
  expect_lte(max(wn), 4000)

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
