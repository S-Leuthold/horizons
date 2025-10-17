#' Tests for finalize_dataset() Function
#'
#' Integration and validation tests for dataset finalization with outlier detection.

library(testthat)
library(horizons)

test_that("finalize_dataset adds outlier_flag column", {
  data <- create_eval_test_data()

  result <- finalize_dataset(
    dataset = data,
    response_variable = "Response",
    verbose = FALSE
  )

  expect_true("outlier_flag" %in% names(result))
  expect_true(all(result$outlier_flag %in% c("good", "outlier")))
})

test_that("finalize_dataset preserves row count with remove_outliers=FALSE", {
  data <- create_eval_test_data()

  result <- finalize_dataset(
    dataset = data,
    response_variable = "Response",
    remove_outliers = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(result), nrow(data))
})

test_that("finalize_dataset can remove outliers", {
  data <- create_eval_test_data()
  # Add extreme outlier
  data$Response[1] <- 1000

  result <- finalize_dataset(
    dataset = data,
    response_variable = "Response",
    remove_outliers = TRUE,
    verbose = FALSE
  )

  expect_lt(nrow(result), nrow(data))
})

test_that("finalize_dataset handles different spectral_outlier_methods", {
  data <- create_eval_test_data()

  # Method: none
  result_none <- finalize_dataset(
    data, "Response",
    spectral_outlier_method = "none",
    verbose = FALSE
  )

  # Method: mahalanobis
  result_maha <- finalize_dataset(
    data, "Response",
    spectral_outlier_method = "mahalanobis",
    verbose = FALSE
  )

  expect_s3_class(result_none, "data.frame")
  expect_s3_class(result_maha, "data.frame")
})

test_that("finalize_dataset handles different response_cutoff values", {
  data <- create_eval_test_data()

  result_strict <- finalize_dataset(
    data, "Response",
    response_cutoff = 1.0,
    verbose = FALSE
  )

  result_lenient <- finalize_dataset(
    data, "Response",
    response_cutoff = 3.0,
    verbose = FALSE
  )

  expect_s3_class(result_strict, "data.frame")
  expect_s3_class(result_lenient, "data.frame")
})

test_that("finalize_dataset can disable response outlier detection", {
  data <- create_eval_test_data()

  result <- finalize_dataset(
    data, "Response",
    detect_response_outliers = FALSE,
    verbose = FALSE
  )

  # All should be marked good if detection disabled
  expect_true(all(result$outlier_flag == "good"))
})

test_that("finalize_dataset preserves all columns", {
  data <- create_eval_test_data()

  result <- finalize_dataset(
    data, "Response",
    remove_outliers = FALSE,
    verbose = FALSE
  )

  # All original columns plus outlier_flag
  expect_gte(ncol(result), ncol(data))
  expect_true(all(names(data) %in% names(result)))
})

test_that("finalize_dataset handles NA in response", {
  data <- create_eval_test_data()
  data$Response[1:3] <- NA

  result <- finalize_dataset(
    data, "Response",
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

test_that("finalize_dataset validates dataset type", {
  expect_error(
    finalize_dataset("not_dataframe", "Response"),
    "dataset must be|data.frame"
  )
})

test_that("finalize_dataset requires response_variable to exist", {
  data <- create_eval_test_data()

  expect_error(
    finalize_dataset(data, "NonExistent"),
    "not found|Response variable"
  )
})