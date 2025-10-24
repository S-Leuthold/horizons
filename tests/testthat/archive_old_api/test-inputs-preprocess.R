#' Tests for preprocess_spectra() Function
#'
#' Tests for spectral preprocessing including baseline correction and resampling.

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## Test Group 1: Input Validation
## ---------------------------------------------------------------------------

test_that("preprocess_spectra validates input type", {
  expect_error(
    preprocess_spectra("not_dataframe"),
    "spectra_data must be a data frame or tibble"
  )
})

test_that("preprocess_spectra requires Sample_ID column", {
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
  test_data <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    check.names = FALSE
  )

  expect_error(
    preprocess_spectra(test_data, resample_interval = -1),
    "resample_interval must be a positive number"
  )

  expect_error(
    preprocess_spectra(test_data, resample_interval = 0),
    "resample_interval must be a positive number"
  )
})

test_that("preprocess_spectra validates baseline_method", {
  test_data <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    check.names = FALSE
  )

  expect_error(
    preprocess_spectra(test_data, baseline_method = "invalid"),
    "should be one of"
  )
})

## ---------------------------------------------------------------------------
## Test Group 2: Basic Preprocessing
## ---------------------------------------------------------------------------

test_that("preprocess_spectra performs basic preprocessing", {
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:10),
    `600` = runif(10, 0.3, 0.7),
    `800` = runif(10, 0.4, 0.8),
    `1000` = runif(10, 0.5, 0.9),
    check.names = FALSE
  )

  result <- preprocess_spectra(
    test_data,
    resample_interval = 4,
    baseline_method = "none",
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true("Sample_ID" %in% names(result))
  expect_equal(nrow(result), 10)
})

test_that("preprocess_spectra handles different resample intervals", {
  test_data <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    `800` = 0.6,
    `1000` = 0.7,
    check.names = FALSE
  )

  result_2 <- preprocess_spectra(test_data, resample_interval = 2, baseline_method = "none", verbose = FALSE)
  result_8 <- preprocess_spectra(test_data, resample_interval = 8, baseline_method = "none", verbose = FALSE)

  # Smaller interval = more columns
  n_cols_2 <- ncol(result_2) - 1
  n_cols_8 <- ncol(result_8) - 1

  expect_gt(n_cols_2, n_cols_8)
})

## ---------------------------------------------------------------------------
## Test Group 3: Verbose Output
## ---------------------------------------------------------------------------

test_that("preprocess_spectra produces verbose output", {
  test_data <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    check.names = FALSE
  )

  expect_message(
    preprocess_spectra(test_data, verbose = TRUE),
    "Preprocessing"
  )
})

test_that("preprocess_spectra silent mode", {
  test_data <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    check.names = FALSE
  )

  expect_silent(
    preprocess_spectra(test_data, verbose = FALSE)
  )
})

## ---------------------------------------------------------------------------
## Test Group 4: Edge Cases
## ---------------------------------------------------------------------------

test_that("preprocess_spectra handles single sample", {
  test_data <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    `700` = 0.6,
    check.names = FALSE
  )

  result <- preprocess_spectra(test_data, verbose = FALSE)

  expect_equal(nrow(result), 1)
})

test_that("preprocess_spectra preserves Sample_ID", {
  test_data <- data.frame(
    Sample_ID = c("A", "B", "C"),
    `600` = c(0.5, 0.6, 0.7),
    check.names = FALSE
  )

  result <- preprocess_spectra(test_data, verbose = FALSE)

  expect_true(all(c("A", "B", "C") %in% result$Sample_ID))
})
