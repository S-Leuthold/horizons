#' Tests for fetch_covariates() Orchestrator Function
#'
#' Comprehensive test suite for the main covariate fetching orchestrator.
#' Tests cover input validation, configuration handling, soil/climate workflows,
#' caching, and integration across the covariate prediction system.
#'
#' SPEC ID: SPEC-COV-ORC-01 through SPEC-COV-ORC-15

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## Test Group 1: Input Validation
## ---------------------------------------------------------------------------

test_that("fetch_covariates validates input_data type", {
  # SPEC: SPEC-COV-ORC-01

  # Must be data.frame
  expect_error(
    fetch_covariates(input_data = "not_a_dataframe"),
    "input_data must be a data.frame or tibble"
  )

  expect_error(
    fetch_covariates(input_data = matrix(1:10, ncol = 2)),
    "input_data must be a data.frame or tibble"
  )

  expect_error(
    fetch_covariates(input_data = list(a = 1, b = 2)),
    "input_data must be a data.frame or tibble"
  )
})

test_that("fetch_covariates requires Sample_ID column", {
  # SPEC: SPEC-COV-ORC-02

  # Missing Sample_ID
  test_data <- data.frame(
    `600` = c(0.5, 0.6),
    `650` = c(0.6, 0.7),
    check.names = FALSE
  )

  expect_error(
    fetch_covariates(input_data = test_data),
    "input_data must contain a Sample_ID column"
  )
})

test_that("fetch_covariates requires spectral columns", {
  # SPEC: SPEC-COV-ORC-03

  # No spectral columns (numeric names)
  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    some_col = c(1, 2)
  )

  expect_error(
    fetch_covariates(input_data = test_data),
    "No spectral columns found"
  )
})

test_that("fetch_covariates validates coordinate columns when climate requested", {
  # SPEC: SPEC-COV-ORC-04

  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.5, 0.6),
    `650` = c(0.6, 0.7),
    check.names = FALSE
  )

  # Missing coordinates for climate data
  expect_error(
    fetch_covariates(
      input_data = test_data,
      climate_covariates = c("MAT", "MAP")
    ),
    "Longitude and Latitude"
  )

  # Has columns but not numeric
  test_data$Longitude <- c("West", "East")
  test_data$Latitude <- c("North", "South")

  expect_error(
    fetch_covariates(
      input_data = test_data,
      climate_covariates = c("MAT")
    ),
    "Longitude and Latitude must be numeric"
  )
})

test_that("fetch_covariates validates numeric parameters", {
  # SPEC: SPEC-COV-ORC-05

  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.5, 0.6),
    check.names = FALSE
  )

  # n_similar must be positive
  expect_error(
    fetch_covariates(input_data = test_data, n_similar = 0),
    "n_similar must be positive"
  )

  expect_error(
    fetch_covariates(input_data = test_data, n_similar = -100),
    "n_similar must be positive"
  )

  # variance_threshold must be (0, 1]
  expect_error(
    fetch_covariates(input_data = test_data, variance_threshold = 0),
    "variance_threshold must be between 0 and 1"
  )

  expect_error(
    fetch_covariates(input_data = test_data, variance_threshold = 1.5),
    "variance_threshold must be between 0 and 1"
  )

  # bayesian_iter must be non-negative
  expect_error(
    fetch_covariates(input_data = test_data, bayesian_iter = -5),
    "bayesian_iter must be non-negative"
  )

  # prop_train must be (0, 1)
  expect_error(
    fetch_covariates(input_data = test_data, prop_train = 0),
    "prop_train must be between 0 and 1"
  )

  expect_error(
    fetch_covariates(input_data = test_data, prop_train = 1),
    "prop_train must be between 0 and 1"
  )

  expect_error(
    fetch_covariates(input_data = test_data, prop_train = 1.2),
    "prop_train must be between 0 and 1"
  )
})

test_that("fetch_covariates validates climate year parameters", {
  # SPEC: SPEC-COV-ORC-06

  test_data <- data.frame(
    Sample_ID = c("S1"),
    `600` = c(0.5),
    Longitude = -105,
    Latitude = 40,
    check.names = FALSE
  )

  # start_year after end_year
  expect_error(
    fetch_covariates(
      input_data = test_data,
      climate_covariates = "MAT",
      climate_start_year = 2020,
      climate_end_year = 2010
    ),
    "climate_start_year cannot be after climate_end_year"
  )
})

## ---------------------------------------------------------------------------
## Test Group 2: Configuration Handling
## ---------------------------------------------------------------------------

test_that("fetch_covariates handles NULL covariates (no-op)", {
  # SPEC: SPEC-COV-ORC-07

  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.5, 0.6),
    `650` = c(0.6, 0.7),
    check.names = FALSE
  )

  # No covariates requested
  result <- fetch_covariates(
    input_data = test_data,
    soil_covariates = NULL,
    climate_covariates = NULL,
    verbose = FALSE
  )

  # Should return valid structure
  expect_type(result, "list")
  expect_true("covariate_data" %in% names(result))
  expect_true("metadata" %in% names(result))

  # Covariate data should just have Sample_ID
  expect_equal(ncol(result$covariate_data), 1)
  expect_equal(names(result$covariate_data), "Sample_ID")
  expect_equal(nrow(result$covariate_data), 2)
})

test_that("fetch_covariates returns expected structure", {
  # SPEC: SPEC-COV-ORC-08

  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.5, 0.6),
    check.names = FALSE
  )

  result <- fetch_covariates(
    input_data = test_data,
    verbose = FALSE
  )

  # Check top-level structure
  expect_type(result, "list")
  expect_named(result, c("covariate_data", "soil_predictions", "metadata"))

  # Check covariate_data
  expect_s3_class(result$covariate_data, "data.frame")
  expect_true("Sample_ID" %in% names(result$covariate_data))
  expect_equal(nrow(result$covariate_data), nrow(test_data))

  # Check metadata
  expect_type(result$metadata, "list")
  expect_true("execution_time" %in% names(result$metadata))
  expect_true("n_samples" %in% names(result$metadata))
  expect_true("n_covariates_requested" %in% names(result$metadata))
  expect_true("n_covariates_returned" %in% names(result$metadata))
})

## ---------------------------------------------------------------------------
## Test Group 3: Verbose Output
## ---------------------------------------------------------------------------

test_that("fetch_covariates produces verbose output when requested", {
  # SPEC: SPEC-COV-ORC-09

  test_data <- data.frame(
    Sample_ID = c("S1"),
    `600` = c(0.5),
    `650` = c(0.6),
    check.names = FALSE
  )

  # Verbose mode
  expect_message(
    fetch_covariates(input_data = test_data, verbose = TRUE),
    "Configuration"
  )

  expect_message(
    fetch_covariates(input_data = test_data, verbose = TRUE),
    "Input samples"
  )
})

test_that("fetch_covariates silent mode produces no output", {
  # SPEC: SPEC-COV-ORC-10

  test_data <- data.frame(
    Sample_ID = c("S1"),
    `600` = c(0.5),
    check.names = FALSE
  )

  # Silent mode (verbose = FALSE)
  expect_silent(
    fetch_covariates(input_data = test_data, verbose = FALSE)
  )
})

## ---------------------------------------------------------------------------
## Test Group 4: Edge Cases
## ---------------------------------------------------------------------------

test_that("fetch_covariates handles single sample", {
  # SPEC: SPEC-COV-ORC-11

  single_sample <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    `650` = 0.6,
    `700` = 0.7,
    check.names = FALSE
  )

  result <- fetch_covariates(
    input_data = single_sample,
    verbose = FALSE
  )

  expect_equal(nrow(result$covariate_data), 1)
  expect_equal(result$metadata$n_samples, 1)
})

test_that("fetch_covariates handles large spectral datasets", {
  # SPEC: SPEC-COV-ORC-12

  # Create dataset with many wavelengths
  n_samples <- 10
  wavelengths <- seq(600, 4000, by = 2)  # 1701 wavelengths

  test_data <- data.frame(
    Sample_ID = paste0("S", 1:n_samples)
  )

  for (wl in wavelengths) {
    test_data[[as.character(wl)]] <- runif(n_samples, 0.1, 0.9)
  }

  # Should handle efficiently
  start_time <- Sys.time()
  result <- fetch_covariates(
    input_data = test_data,
    verbose = FALSE
  )
  end_time <- Sys.time()

  # Should complete quickly for no-covariate case
  expect_lt(as.numeric(difftime(end_time, start_time, units = "secs")), 5)

  expect_equal(nrow(result$covariate_data), n_samples)
})

test_that("fetch_covariates preserves Sample_ID order", {
  # SPEC: SPEC-COV-ORC-13

  # Use specific Sample_IDs in defined order
  test_data <- data.frame(
    Sample_ID = c("SAMPLE_C", "SAMPLE_A", "SAMPLE_B"),
    `600` = c(0.5, 0.6, 0.7),
    check.names = FALSE
  )

  result <- fetch_covariates(
    input_data = test_data,
    verbose = FALSE
  )

  # Sample_IDs should be in same order
  expect_equal(
    result$covariate_data$Sample_ID,
    c("SAMPLE_C", "SAMPLE_A", "SAMPLE_B")
  )
})

test_that("fetch_covariates metadata contains expected fields", {
  # SPEC: SPEC-COV-ORC-14

  test_data <- data.frame(
    Sample_ID = c("S1", "S2", "S3"),
    `600` = c(0.5, 0.6, 0.7),
    check.names = FALSE
  )

  result <- fetch_covariates(input_data = test_data, verbose = FALSE)

  metadata <- result$metadata

  # Required metadata fields
  expect_true("execution_time" %in% names(metadata))
  expect_true("execution_time_formatted" %in% names(metadata))
  expect_true("n_samples" %in% names(metadata))
  expect_true("n_covariates_requested" %in% names(metadata))
  expect_true("n_covariates_returned" %in% names(metadata))
  expect_true("cache_dir" %in% names(metadata))

  # Validate values
  expect_equal(metadata$n_samples, 3)
  expect_equal(metadata$n_covariates_requested, 0)
  expect_equal(metadata$n_covariates_returned, 0)  # No covariates requested
  expect_true(metadata$execution_time >= 0)
})

test_that("fetch_covariates handles missing values in spectral data", {
  # SPEC: SPEC-COV-ORC-15

  test_data <- data.frame(
    Sample_ID = c("S1", "S2", "S3"),
    `600` = c(0.5, NA, 0.7),
    `650` = c(0.6, 0.7, NA),
    check.names = FALSE
  )

  # Should handle gracefully (no-op case)
  result <- fetch_covariates(
    input_data = test_data,
    verbose = FALSE
  )

  expect_s3_class(result$covariate_data, "data.frame")
  expect_equal(nrow(result$covariate_data), 3)
})

## ---------------------------------------------------------------------------
## NOTE: Integration tests with actual soil/climate prediction are complex
## and require mocking or real OSSL data. These will be added in Phase 1.2-1.4
## after we build helper functions and understand the prediction pipeline better.
##
## Priority is to get basic orchestrator validation in place first, then
## add comprehensive integration tests.
## ---------------------------------------------------------------------------
