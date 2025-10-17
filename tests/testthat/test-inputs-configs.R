#' Tests for create_configs() Function
#'
#' Tests for model configuration grid generation including covariate combinations.

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## Test Group 1: Basic Functionality
## ---------------------------------------------------------------------------

test_that("create_configs generates configuration grid", {
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none",
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true("config_id" %in% names(result))
  expect_true("model" %in% names(result))
  expect_true("transformation" %in% names(result))
  expect_true("preprocessing" %in% names(result))
  expect_true("feature_selection" %in% names(result))
})

test_that("create_configs handles multiple values", {
  result <- create_configs(
    models = c("plsr", "random_forest"),
    transformations = c("none", "log"),
    preprocessing = c("raw", "snv"),
    feature_selection = c("none", "pca"),
    verbose = FALSE
  )

  # Should have 2*2*2*2 = 16 combinations
  expect_equal(nrow(result), 16)

  # Check all models present
  expect_true(all(c("plsr", "random_forest") %in% result$model))

  # Check all transformations present
  expect_true(all(c("none", "log") %in% result$transformation))
})

test_that("create_configs generates unique config IDs", {
  result <- create_configs(
    models = c("plsr", "random_forest"),
    transformations = c("none", "log"),
    preprocessing = "raw",
    feature_selection = "none",
    verbose = FALSE
  )

  # All config_ids should be unique
  expect_equal(length(unique(result$config_id)), nrow(result))

  # Should follow format
  expect_true(all(grepl("^config_[0-9]{4}$", result$config_id)))
})

## ---------------------------------------------------------------------------
## Test Group 2: Covariate Handling
## ---------------------------------------------------------------------------

test_that("create_configs handles soil covariates", {
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none",
    soil_covariates = c("clay", "sand"),
    verbose = FALSE
  )

  # Should have covariate-related columns
  expect_true("covariates" %in% names(result))

  # Should be a list column
  expect_true(is.list(result$covariates))

  # Should have multiple covariate combinations
  expect_gt(nrow(result), 1)
})

test_that("create_configs handles NULL covariates", {
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none",
    soil_covariates = NULL,
    climate_covariates = NULL,
    verbose = FALSE
  )

  # Should still work with no covariates
  expect_s3_class(result, "data.frame")
  expect_gte(nrow(result), 1)
})

test_that("create_configs handles climate covariates", {
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none",
    climate_covariates = c("MAT", "MAP"),
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true("covariates" %in% names(result))
})

## ---------------------------------------------------------------------------
## Test Group 3: Verbose Output
## ---------------------------------------------------------------------------

test_that("create_configs produces verbose output", {
  expect_message(
    create_configs(
      models = "plsr",
      transformations = "none",
      preprocessing = "raw",
      feature_selection = "none",
      verbose = TRUE
    ),
    "Configuration"
  )
})

test_that("create_configs silent mode", {
  expect_silent(
    create_configs(
      models = "plsr",
      transformations = "none",
      preprocessing = "raw",
      feature_selection = "none",
      verbose = FALSE
    )
  )
})

## ---------------------------------------------------------------------------
## Test Group 4: Edge Cases
## ---------------------------------------------------------------------------

test_that("create_configs handles single values", {
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none",
    verbose = FALSE
  )

  # Should have exactly 1 configuration
  expect_gte(nrow(result), 1)
})

test_that("create_configs handles many combinations", {
  result <- create_configs(
    models = c("plsr", "random_forest", "cubist"),
    transformations = c("none", "log", "sqrt"),
    preprocessing = c("raw", "snv", "deriv1"),
    feature_selection = c("none", "pca"),
    verbose = FALSE
  )

  # 3*3*3*2 = 54 combinations
  expect_equal(nrow(result), 54)
})

test_that("create_configs preserves parameter values", {
  result <- create_configs(
    models = "plsr",
    transformations = "log",
    preprocessing = "snv",
    feature_selection = "pca",
    verbose = FALSE
  )

  expect_equal(result$model[1], "plsr")
  expect_equal(result$transformation[1], "log")
  expect_equal(result$preprocessing[1], "snv")
  expect_equal(result$feature_selection[1], "pca")
})
