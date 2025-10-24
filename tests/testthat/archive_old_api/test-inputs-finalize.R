#' Tests for finalize_dataset() Function
#'
#' Test suite for outlier detection and dataset finalization.
#' Covers Mahalanobis spectral outliers and IQR response outliers.

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## Test Group 1: Input Validation
## ---------------------------------------------------------------------------

test_that("finalize_dataset validates dataset type", {
  expect_error(
    finalize_dataset("not_dataframe", "SOC"),
    "dataset must be a data frame or tibble"
  )
})

test_that("finalize_dataset requires response_variable to exist", {
  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.5, 0.6),
    SOC = c(1.5, 2.0),
    check.names = FALSE
  )

  expect_error(
    finalize_dataset(test_data, "NonExistent"),
    "Response variable.*not found"
  )
})

test_that("finalize_dataset validates spectral_outlier_method", {
  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.5, 0.6),
    SOC = c(1.5, 2.0),
    check.names = FALSE
  )

  expect_error(
    finalize_dataset(test_data, "SOC", spectral_outlier_method = "invalid"),
    "should be one of"
  )
})

## ---------------------------------------------------------------------------
## Test Group 2: Basic Functionality
## ---------------------------------------------------------------------------

test_that("finalize_dataset adds outlier_flag column", {
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:20),
    `600` = rnorm(20, 0.5, 0.1),
    `650` = rnorm(20, 0.6, 0.1),
    `700` = rnorm(20, 0.7, 0.1),
    SOC = rnorm(20, 2, 0.5),
    check.names = FALSE
  )

  result <- finalize_dataset(
    dataset = test_data,
    response_variable = "SOC",
    verbose = FALSE
  )

  expect_true("outlier_flag" %in% names(result))
  expect_true(all(result$outlier_flag %in% c("good", "outlier")))
})

test_that("finalize_dataset returns same number of rows when remove_outliers = FALSE", {
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:15),
    `600` = c(rnorm(14, 0.5, 0.1), 5),  # One extreme value
    SOC = c(rnorm(14, 2, 0.3), 10),  # One extreme value
    check.names = FALSE
  )

  result <- finalize_dataset(
    dataset = test_data,
    response_variable = "SOC",
    remove_outliers = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(result), nrow(test_data))
})

test_that("finalize_dataset removes outliers when requested", {
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:20),
    `600` = rnorm(20, 0.5, 0.05),
    SOC = c(rnorm(18, 2, 0.2), 50, 100),  # Two clear outliers
    check.names = FALSE
  )

  result <- finalize_dataset(
    dataset = test_data,
    response_variable = "SOC",
    spectral_outlier_method = "none",  # Only response outliers
    remove_outliers = TRUE,
    verbose = FALSE
  )

  expect_lt(nrow(result), nrow(test_data))
})

## ---------------------------------------------------------------------------
## Test Group 3: Response Outlier Detection (IQR)
## ---------------------------------------------------------------------------

test_that("finalize_dataset detects response outliers with IQR method", {
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:20),
    `600` = rnorm(20, 0.5, 0.05),
    SOC = c(rnorm(18, 2, 0.2), 100, 200),  # Two extreme outliers
    check.names = FALSE
  )

  result <- finalize_dataset(
    dataset = test_data,
    response_variable = "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = TRUE,
    response_cutoff = 1.5,
    verbose = FALSE
  )

  # Should detect at least one outlier
  n_outliers <- sum(result$outlier_flag == "outlier")
  expect_gt(n_outliers, 0)
})

test_that("finalize_dataset handles different IQR cutoffs", {
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:20),
    `600` = rnorm(20),
    SOC = c(rnorm(19, 2, 0.3), 5),  # One moderate outlier
    check.names = FALSE
  )

  # Strict cutoff
  result_strict <- finalize_dataset(
    test_data, "SOC",
    spectral_outlier_method = "none",
    response_cutoff = 1.0,
    verbose = FALSE
  )

  # Lenient cutoff
  result_lenient <- finalize_dataset(
    test_data, "SOC",
    spectral_outlier_method = "none",
    response_cutoff = 3.0,
    verbose = FALSE
  )

  outliers_strict <- sum(result_strict$outlier_flag == "outlier")
  outliers_lenient <- sum(result_lenient$outlier_flag == "outlier")

  expect_gte(outliers_strict, outliers_lenient)
})

test_that("finalize_dataset can disable response outlier detection", {
  test_data <- data.frame(
    Sample_ID = c("S1", "S2", "S3"),
    `600` = c(0.5, 0.6, 0.7),
    SOC = c(1, 2, 100),  # Extreme outlier
    check.names = FALSE
  )

  result <- finalize_dataset(
    test_data, "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = FALSE,
    verbose = FALSE
  )

  # All should be marked good (detection disabled)
  expect_true(all(result$outlier_flag == "good"))
})

## ---------------------------------------------------------------------------
## Test Group 4: Spectral Outlier Detection
## ---------------------------------------------------------------------------

test_that("finalize_dataset can disable spectral outlier detection", {
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:10),
    `600` = rnorm(10),
    `650` = rnorm(10),
    SOC = rnorm(10, 2, 0.3),
    check.names = FALSE
  )

  result <- finalize_dataset(
    test_data, "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = FALSE,
    verbose = FALSE
  )

  # All should be good (both disabled)
  expect_true(all(result$outlier_flag == "good"))
})

## ---------------------------------------------------------------------------
## Test Group 5: Verbose Output
## ---------------------------------------------------------------------------

test_that("finalize_dataset produces verbose output", {
  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.5, 0.6),
    SOC = c(1.5, 2.0),
    check.names = FALSE
  )

  expect_message(
    finalize_dataset(test_data, "SOC", verbose = TRUE),
    "Dataset Finalization"
  )
})

test_that("finalize_dataset silent mode", {
  test_data <- data.frame(
    Sample_ID = c("S1"),
    `600` = c(0.5),
    SOC = c(1.5),
    check.names = FALSE
  )

  expect_silent(
    finalize_dataset(test_data, "SOC", verbose = FALSE)
  )
})

## ---------------------------------------------------------------------------
## Test Group 6: Edge Cases
## ---------------------------------------------------------------------------

test_that("finalize_dataset handles minimal dataset", {
  test_data <- data.frame(
    Sample_ID = c("S1", "S2", "S3"),
    `600` = c(0.5, 0.6, 0.7),
    SOC = c(1.5, 2.0, 2.5),
    check.names = FALSE
  )

  result <- finalize_dataset(test_data, "SOC", verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("finalize_dataset handles NA in response", {
  test_data <- data.frame(
    Sample_ID = c("S1", "S2", "S3"),
    `600` = c(0.5, 0.6, 0.7),
    SOC = c(1.5, NA, 2.5),
    check.names = FALSE
  )

  result <- finalize_dataset(
    test_data, "SOC",
    spectral_outlier_method = "none",
    verbose = FALSE
  )

  # Should handle NA gracefully
  expect_s3_class(result, "data.frame")
})

test_that("finalize_dataset preserves all original columns", {
  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    Project = c("P1", "P1"),
    `600` = c(0.5, 0.6),
    SOC = c(1.5, 2.0),
    pH = c(6.0, 6.5),
    check.names = FALSE
  )

  result <- finalize_dataset(
    test_data, "SOC",
    remove_outliers = FALSE,
    verbose = FALSE
  )

  # Should have all original columns plus outlier_flag
  expect_true(all(c("Sample_ID", "Project", "600", "SOC", "pH", "outlier_flag") %in% names(result)))
})
