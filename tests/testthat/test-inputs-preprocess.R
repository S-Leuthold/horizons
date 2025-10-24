#' Tests for preprocess_spectra() Function
#'
#' Integration tests for spectral preprocessing.

library(testthat)
library(horizons)

test_that("preprocess_spectra performs basic preprocessing", {
  data <- data.frame(
    Sample_ID = c("S1", "S2", "S3"),
    `600` = c(0.5, 0.6, 0.7),
    `800` = c(0.6, 0.7, 0.8),
    `1000` = c(0.7, 0.8, 0.9),
    check.names = FALSE
  )
  attr(data, "spectra_type") <- "MIR"

  mock_resample <- function(X, wav, new.wav, interpol = "spline") {
    matrix(rep(seq_along(new.wav), each = nrow(X)),
           nrow = nrow(X),
           ncol = length(new.wav))
  }

  result <- with_mocked_bindings(
    resample = mock_resample,
    preprocess_spectra(
      data,
      resample_interval = 4,
      baseline_method = "none",
      verbose = FALSE
    ),
    .package = "prospectr"
  )

  expect_s3_class(result, "data.frame")
  expect_true("Sample_ID" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("preprocess_spectra preserves Sample_ID", {
  data <- create_tiny_spectra()
  data <- data[, c("Sample_ID", grep("^[0-9]", names(data), value = TRUE))]
  attr(data, "spectra_type") <- "MIR"

  mock_resample <- function(X, wav, new.wav, interpol = "spline") {
    matrix(rep(seq_along(new.wav), each = nrow(X)),
           nrow = nrow(X),
           ncol = length(new.wav))
  }

  result <- with_mocked_bindings(
    resample = mock_resample,
    preprocess_spectra(data, verbose = FALSE),
    .package = "prospectr"
  )

  expect_true(all(data$Sample_ID %in% result$Sample_ID))
})

test_that("preprocess_spectra handles different baseline methods", {
  data <- create_tiny_spectra()
  data <- data[, c("Sample_ID", grep("^[0-9]", names(data), value = TRUE))]
  attr(data, "spectra_type") <- "MIR"

  calls <- new.env(parent = emptyenv())
  calls$baseline <- character()

  mock_baseline <- function(X, wav) {
    calls$baseline <- c(calls$baseline, "rubberband")
    X + 0.01
  }

  mock_detrend <- function(X, wav, p = 2) {
    calls$baseline <- c(calls$baseline, paste0("poly-", p))
    X - 0.01
  }

  mock_resample <- function(X, wav, new.wav, interpol = "spline") {
    matrix(rep(seq_along(new.wav), each = nrow(X)),
           nrow = nrow(X),
           ncol = length(new.wav))
  }

  result_rubber <- with_mocked_bindings(
    baseline = mock_baseline,
    resample = mock_resample,
    preprocess_spectra(data, baseline_method = "rubberband", verbose = FALSE),
    .package = "prospectr"
  )

  result_poly <- with_mocked_bindings(
    detrend = mock_detrend,
    resample = mock_resample,
    preprocess_spectra(data, baseline_method = "polynomial", verbose = FALSE),
    .package = "prospectr"
  )

  expect_s3_class(result_rubber, "data.frame")
  expect_s3_class(result_poly, "data.frame")
  expect_true(all(c("rubberband", "poly-2") %in% calls$baseline))
})

test_that("preprocess_spectra handles different resample intervals", {
  data <- create_tiny_spectra()
  data <- data[, c("Sample_ID", grep("^[0-9]", names(data), value = TRUE))]
  attr(data, "spectra_type") <- "MIR"

  mock_resample <- function(X, wav, new.wav, interpol = "spline") {
    matrix(rep(seq_along(new.wav), each = nrow(X)),
           nrow = nrow(X),
           ncol = length(new.wav))
  }

  result_2 <- with_mocked_bindings(
    resample = mock_resample,
    preprocess_spectra(data, resample_interval = 2, baseline_method = "none", verbose = FALSE),
    .package = "prospectr"
  )

  result_8 <- with_mocked_bindings(
    resample = mock_resample,
    preprocess_spectra(data, resample_interval = 8, baseline_method = "none", verbose = FALSE),
    .package = "prospectr"
  )

  # Smaller interval = more columns
  expect_gt(ncol(result_2), ncol(result_8))
})

test_that("preprocess_spectra validates input type", {
  expect_error(
    preprocess_spectra("not_dataframe"),
    "data frame|tibble"
  )
})

test_that("preprocess_spectra requires Sample_ID", {
  bad_data <- data.frame(
    `600` = c(0.5, 0.6),
    check.names = FALSE
  )

  expect_error(
    preprocess_spectra(bad_data),
    "Sample_ID"
  )
})

test_that("preprocess_spectra validates resample_interval", {
  data <- create_tiny_spectra()

  expect_error(
    preprocess_spectra(data, resample_interval = -1),
    "positive"
  )

  expect_error(
    preprocess_spectra(data, resample_interval = 0),
    "positive"
  )
})

test_that("preprocess_spectra validates baseline_method", {
  data <- create_tiny_spectra()

  expect_error(
    preprocess_spectra(data, baseline_method = "invalid"),
    "should be one of"
  )
})

test_that("preprocess_spectra warns when spectra_type attribute missing", {
  data <- create_tiny_spectra()
  data <- data[, c("Sample_ID", grep("^[0-9]", names(data), value = TRUE))]
  attr(data, "source") <- "unit-test"

  mock_resample <- function(X, wav, new.wav, interpol = "spline") {
    matrix(rep(seq_along(new.wav), each = nrow(X)),
           nrow = nrow(X),
           ncol = length(new.wav))
  }

  expect_warning(
    with_mocked_bindings(
      resample = mock_resample,
      preprocess_spectra(data, verbose = FALSE),
      .package = "prospectr"
    ),
    "assuming MIR"
  )
})

test_that("preprocess_spectra errors on unknown spectra_type", {
  data <- create_tiny_spectra()
  data <- data[, c("Sample_ID", grep("^[0-9]", names(data), value = TRUE))]
  attr(data, "spectra_type") <- "SWIR"

  mock_resample <- function(X, wav, new.wav, interpol = "spline") {
    matrix(rep(seq_along(new.wav), each = nrow(X)),
           nrow = nrow(X),
           ncol = length(new.wav))
  }

  expect_error(
    with_mocked_bindings(
      resample = mock_resample,
      preprocess_spectra(data, verbose = FALSE),
      .package = "prospectr"
    ),
    "Unknown spectra_type"
  )
})
