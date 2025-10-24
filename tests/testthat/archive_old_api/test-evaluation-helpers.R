#' Tests for Evaluation Helper Functions
#'
#' Unit tests for utility functions in R/evaluation-helpers.R
#' These are pure functions (no side effects) so tests run very fast.

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## Test Group 1: create_failed_result
## ---------------------------------------------------------------------------

test_that("create_failed_result returns proper structure", {
  result <- horizons:::create_failed_result(
    config_id = "test_001",
    config_clean = list(
      model = "plsr",
      transformation = "none",
      preprocessing = "raw",
      feature_selection = "none",
      covariates = NULL
    ),
    error_message = "Test error occurred",
    workflow_id = "plsr_none_raw_none"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$config_id, "test_001")
  expect_equal(result$status, "failed")
  expect_equal(result$error_message, "Test error occurred")
  expect_equal(result$workflow_id, "plsr_none_raw_none")

  # Metrics should be NA for failed result
  expect_true(is.na(result$rsq))
  expect_true(is.na(result$rmse))
})

## ---------------------------------------------------------------------------
## Test Group 2: get_readable_model_name
## ---------------------------------------------------------------------------

test_that("get_readable_model_name translates common models", {
  expect_equal(horizons:::get_readable_model_name("plsr"), "Partial Least Squares Regression")
  expect_equal(horizons:::get_readable_model_name("random_forest"), "Random Forest")
  expect_equal(horizons:::get_readable_model_name("cubist"), "Cubist")
  expect_equal(horizons:::get_readable_model_name("xgboost"), "XGBoost")
  expect_equal(horizons:::get_readable_model_name("svm"), "Support Vector Machine")
})

test_that("get_readable_model_name handles unknown models", {
  result <- horizons:::get_readable_model_name("unknown_model")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("get_readable_model_name handles NULL", {
  result <- horizons:::get_readable_model_name(NULL)
  expect_type(result, "character")
})

## ---------------------------------------------------------------------------
## Test Group 3: get_readable_preprocessing_name
## ---------------------------------------------------------------------------

test_that("get_readable_preprocessing_name translates preprocessing methods", {
  expect_equal(horizons:::get_readable_preprocessing_name("raw"), "Raw Spectra")
  expect_equal(horizons:::get_readable_preprocessing_name("snv"), "Standard Normal Variate")
  expect_equal(horizons:::get_readable_preprocessing_name("deriv1"), "First Derivative")
  expect_equal(horizons:::get_readable_preprocessing_name("deriv2"), "Second Derivative")
})

test_that("get_readable_preprocessing_name handles case variations", {
  expect_equal(
    horizons:::get_readable_preprocessing_name("SNV"),
    horizons:::get_readable_preprocessing_name("snv")
  )

  expect_equal(
    horizons:::get_readable_preprocessing_name("Raw"),
    horizons:::get_readable_preprocessing_name("raw")
  )
})

## ---------------------------------------------------------------------------
## Test Group 4: get_readable_transformation_name
## ---------------------------------------------------------------------------

test_that("get_readable_transformation_name translates transformations", {
  expect_equal(horizons:::get_readable_transformation_name("none"), "No Transformation")
  expect_equal(horizons:::get_readable_transformation_name("log"), "Logarithmic")
  expect_equal(horizons:::get_readable_transformation_name("sqrt"), "Square Root")
})

test_that("get_readable_transformation_name handles NULL", {
  result <- horizons:::get_readable_transformation_name(NULL)
  expect_type(result, "character")
})

## ---------------------------------------------------------------------------
## Test Group 5: get_readable_feature_selection_name
## ---------------------------------------------------------------------------

test_that("get_readable_feature_selection_name translates methods", {
  expect_equal(horizons:::get_readable_feature_selection_name("none"), "No Feature Selection")
  expect_equal(horizons:::get_readable_feature_selection_name("pca"), "Principal Component Analysis")
  expect_equal(horizons:::get_readable_feature_selection_name("correlation"), "Correlation-Based Selection")
  expect_equal(horizons:::get_readable_feature_selection_name("boruta"), "Boruta Feature Selection")
})

## ---------------------------------------------------------------------------
## Test Group 6: get_readable_covariate_name
## ---------------------------------------------------------------------------

test_that("get_readable_covariate_name translates covariate IDs", {
  expect_equal(horizons:::get_readable_covariate_name("clay"), "Clay Content")
  expect_equal(horizons:::get_readable_covariate_name("ph"), "Soil pH")
  expect_equal(horizons:::get_readable_covariate_name("MAT"), "Mean Annual Temperature")
  expect_equal(horizons:::get_readable_covariate_name("MAP"), "Mean Annual Precipitation")
})

test_that("get_readable_covariate_name handles NULL and unknown", {
  expect_type(horizons:::get_readable_covariate_name(NULL), "character")
  expect_type(horizons:::get_readable_covariate_name("unknown"), "character")
})

## ---------------------------------------------------------------------------
## Test Group 7: parse_workflow_id
## ---------------------------------------------------------------------------

test_that("parse_workflow_id extracts simple workflow components", {
  parsed <- horizons:::parse_workflow_id("plsr_none_raw_none")

  expect_type(parsed, "list")
  expect_true("model" %in% names(parsed))
  expect_true("transformation" %in% names(parsed))
  expect_true("preprocessing" %in% names(parsed))
  expect_true("feature_selection" %in% names(parsed))
})

test_that("parse_workflow_id handles workflow with covariates", {
  parsed <- horizons:::parse_workflow_id("random_forest_log_SNV_PCA_clay+ph")

  expect_type(parsed, "list")
  expect_true(grepl("Random Forest", parsed$model, ignore.case = TRUE))
  expect_true(grepl("log", parsed$transformation, ignore.case = TRUE))
})

test_that("parse_workflow_id handles complex workflow IDs", {
  parsed <- horizons:::parse_workflow_id("xgboost_sqrt_snv_deriv1_correlation_elevation+MAT")

  expect_type(parsed, "list")
  expect_true(all(c("model", "transformation", "preprocessing", "feature_selection") %in% names(parsed)))
})

## ---------------------------------------------------------------------------
## NOTE: Additional helper functions may exist. Run test file to verify
## all functions are covered, then check coverage report to identify gaps.
## ---------------------------------------------------------------------------
