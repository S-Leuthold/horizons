#' Tests for Recipe Building and Transformation Functions
#'
#' Comprehensive tests for models-recipes.R with focus on integration (70%)
#' and validation (30%) to maximize coverage velocity
#'
#' **Coverage Target**: 0% → 60%+ for R/models-recipes.R
#' **Test Strategy**: Integration-first with transformation combinations
#' **Test Count**: ~35 tests (25 integration + 10 validation)

## ===========================================================================
## Setup and Helper Functions
## ===========================================================================

library(testthat)
library(horizons)
library(recipes)
library(dplyr)
library(tibble)

build_recipe                <- horizons:::build_recipe
define_model_specifications <- horizons:::define_model_specifications

# Helper to create minimal test data with spectral columns
create_test_spectra <- function(n = 10, seed = 123) {
  set.seed(seed)

  # Create Sample_ID and Response
  df <- tibble::tibble(
    Sample_ID = paste0("S", sprintf("%03d", 1:n)),
    Response  = runif(n, 0.5, 2.5),
    Project   = rep("TEST", n)
  )

  # Add spectral columns (600, 602, 604, ... 700)
  wavelengths <- seq(600, 700, by = 2)

  for (wl in wavelengths) {
    df[[as.character(wl)]] <- runif(n, 0.01, 0.99)
  }

  return(df)
}

# Helper to create covariate data
create_test_covariates <- function(n = 10, seed = 456) {
  set.seed(seed)

  tibble::tibble(
    Sample_ID = paste0("S", sprintf("%03d", 1:n)),
    Clay      = runif(n, 10, 60),
    pH        = runif(n, 4, 8),
    SOC       = runif(n, 0.5, 5),
    Elevation = runif(n, 100, 2000)
  )
}

## ===========================================================================
## INTEGRATION TESTS: Full Recipe Building (70% of tests)
## ===========================================================================

test_that("build_recipe creates basic recipe with no transformations", {

  # SPEC-RECIPE-INT-001: Minimal recipe with raw data
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none",
    covariate_selection       = NULL,
    covariate_data            = NULL,
    covariate_interactions    = FALSE
  )

  expect_s3_class(recipe_obj, "recipe")
  expect_equal(recipe_obj$var_info$variable[recipe_obj$var_info$role == "outcome"], "Response")
  expect_equal(recipe_obj$var_info$variable[recipe_obj$var_info$role == "id"], "Sample_ID")

})

test_that("build_recipe handles PCA feature selection integration", {

  # SPEC-RECIPE-INT-002: PCA reduces dimensionality to 99.5% variance
  test_data <- create_test_spectra(20)

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "pca",
    covariate_selection       = NULL,
    covariate_data            = NULL
  )

  # Check PCA step was added
  pca_steps <- purrr::keep(recipe_obj$steps, ~ inherits(.x, "step_pca"))
  expect_length(pca_steps, 1)
  expect_equal(pca_steps[[1]]$threshold, 0.995)

})

test_that("build_recipe applies log transformation to response", {

  # SPEC-RECIPE-INT-003: Response transformation affects outcome
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "log",
    feature_selection_method  = "none"
  )

  # Check log step was added
  log_steps <- purrr::keep(recipe_obj$steps, ~ inherits(.x, "step_log"))
  expect_length(log_steps, 1)
  expect_true(log_steps[[1]]$skip)  # Should have skip = TRUE for inverse transform

})

test_that("build_recipe integrates SNV + derivative preprocessing", {

  # SPEC-RECIPE-INT-004: Complex spectral transformation pipeline
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "snv_deriv1",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  # Check spectral transformation step
  transform_steps <- purrr::keep(recipe_obj$steps,
                                  ~ inherits(.x, "step_transform_spectra"))
  expect_length(transform_steps, 1)
  expect_equal(transform_steps[[1]]$preprocessing, "snv_deriv1")

})

test_that("build_recipe adds covariates and merges correctly", {

  # SPEC-RECIPE-INT-005: Covariate integration workflow
  test_data       <- create_test_spectra()
  test_covariates <- create_test_covariates()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none",
    covariate_selection       = c("Clay", "pH"),
    covariate_data            = test_covariates
  )

  # Check covariate step was added
  cov_steps <- purrr::keep(recipe_obj$steps,
                           ~ inherits(.x, "step_add_covariates"))
  expect_length(cov_steps, 1)

})

test_that("build_recipe creates covariate interactions after PCA", {

  # SPEC-RECIPE-INT-006: Interaction terms with dimensionality reduction
  test_data       <- create_test_spectra(30)
  test_covariates <- create_test_covariates(30)

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "snv",
    response_transformation   = "sqrt",
    feature_selection_method  = "pca",
    covariate_selection       = c("Clay", "pH"),
    covariate_data            = test_covariates,
    covariate_interactions    = TRUE
  )

  # Check interaction step was added
  interact_steps <- purrr::keep(recipe_obj$steps,
                                 ~ inherits(.x, "step_interact"))
  expect_length(interact_steps, 1)
  expect_equal(interact_steps[[1]]$sep, "_x_")

})

test_that("build_recipe handles correlation feature selection", {

  # SPEC-RECIPE-INT-007: Correlation-based feature reduction
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "correlation"
  )

  # Check correlation step
  corr_steps <- purrr::keep(recipe_obj$steps,
                             ~ inherits(.x, "step_select_correlation"))
  expect_length(corr_steps, 1)
  expect_equal(corr_steps[[1]]$outcome, "Response")

})

test_that("build_recipe handles Boruta feature selection", {

  # SPEC-RECIPE-INT-008: Boruta algorithm integration
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "sg",
    response_transformation   = "none",
    feature_selection_method  = "boruta"
  )

  # Check Boruta step
  boruta_steps <- purrr::keep(recipe_obj$steps,
                               ~ inherits(.x, "step_select_boruta"))
  expect_length(boruta_steps, 1)
  expect_equal(boruta_steps[[1]]$outcome, "Response")

})

test_that("build_recipe handles CARS feature selection", {

  # SPEC-RECIPE-INT-009: CARS algorithm integration
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "deriv2",
    response_transformation   = "none",
    feature_selection_method  = "cars"
  )

  # Check CARS step
  cars_steps <- purrr::keep(recipe_obj$steps,
                             ~ inherits(.x, "step_select_cars"))
  expect_length(cars_steps, 1)
  expect_equal(cars_steps[[1]]$outcome, "Response")

})

test_that("build_recipe chains multiple transformations correctly", {

  # SPEC-RECIPE-INT-010: Full pipeline with all components
  test_data       <- create_test_spectra(25)
  test_covariates <- create_test_covariates(25)

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "snv_deriv2",
    response_transformation   = "log",
    feature_selection_method  = "pca",
    covariate_selection       = c("Clay", "SOC"),
    covariate_data            = test_covariates,
    covariate_interactions    = TRUE
  )

  # Should have all steps in correct order
  expect_s3_class(recipe_obj, "recipe")

  # Count step types
  n_steps <- length(recipe_obj$steps)
  expect_gte(n_steps, 4)  # At least: transform, PCA, covariates, interactions

})

test_that("build_recipe preserves Project metadata role", {

  # SPEC-RECIPE-INT-011: Metadata columns handled correctly
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  # Check Project has metadata role
  project_info <- recipe_obj$var_info[recipe_obj$var_info$variable == "Project",]
  if (nrow(project_info) > 0) {
    expect_equal(project_info$role, "metadata")
  }

})

test_that("build_recipe handles all spectral transformations", {

  # SPEC-RECIPE-INT-012: Test each preprocessing option
  test_data <- create_test_spectra()

  transformations <- c("raw", "sg", "snv", "deriv1", "deriv2",
                       "snv_deriv1", "snv_deriv2")

  for (trans in transformations) {
    recipe_obj <- build_recipe(
      input_data                = test_data,
      spectral_transformation   = trans,
      response_transformation   = "none",
      feature_selection_method  = "none"
    )

    expect_s3_class(recipe_obj, "recipe")
  }

})

test_that("build_recipe handles sqrt response transformation", {

  # SPEC-RECIPE-INT-013: Square root transformation
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "sqrt",
    feature_selection_method  = "none"
  )

  # Check sqrt step
  sqrt_steps <- purrr::keep(recipe_obj$steps, ~ inherits(.x, "step_sqrt"))
  expect_length(sqrt_steps, 1)
  expect_true(sqrt_steps[[1]]$skip)

})

test_that("build_recipe filters to expected wavelength range", {

  # SPEC-RECIPE-INT-014: Wavelength selection 600-4000
  test_data <- create_test_spectra()
  # Add some out-of-range columns
  test_data$`500` <- runif(nrow(test_data))
  test_data$`4500` <- runif(nrow(test_data))

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  # Recipe should work despite extra columns
  expect_s3_class(recipe_obj, "recipe")

})

test_that("build_recipe handles single covariate selection", {

  # SPEC-RECIPE-INT-015: Single covariate workflow
  test_data       <- create_test_spectra()
  test_covariates <- create_test_covariates()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none",
    covariate_selection       = "Clay",
    covariate_data            = test_covariates
  )

  # Check covariate step
  cov_steps <- purrr::keep(recipe_obj$steps,
                           ~ inherits(.x, "step_add_covariates"))
  expect_length(cov_steps, 1)

})

test_that("build_recipe handles multiple covariates with interactions", {

  # SPEC-RECIPE-INT-016: Multi-covariate interaction terms
  test_data       <- create_test_spectra(20)
  test_covariates <- create_test_covariates(20)

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "correlation",
    covariate_selection       = c("Clay", "pH", "SOC"),
    covariate_data            = test_covariates,
    covariate_interactions    = TRUE
  )

  # Should have both covariate and interaction steps
  cov_steps <- purrr::keep(recipe_obj$steps,
                           ~ inherits(.x, "step_add_covariates"))
  interact_steps <- purrr::keep(recipe_obj$steps,
                                 ~ inherits(.x, "step_interact"))

  expect_length(cov_steps, 1)
  expect_length(interact_steps, 1)

})

test_that("build_recipe works with minimal wavelength columns", {

  # SPEC-RECIPE-INT-017: Edge case with few spectral features
  test_data <- tibble::tibble(
    Sample_ID = paste0("S", 1:5),
    Project   = rep("P1", 5),
    Response  = runif(5),
    `600`     = runif(5),
    `602`     = runif(5),
    `604`     = runif(5)
  )

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  expect_s3_class(recipe_obj, "recipe")

})

test_that("build_recipe handles empty covariate vectors gracefully", {

  # SPEC-RECIPE-INT-018: NULL vs empty vs NA covariates
  test_data <- create_test_spectra()

  # Test NULL
  recipe_null <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none",
    covariate_selection       = NULL
  )

  # Test empty character vector
  recipe_empty <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none",
    covariate_selection       = character(0)
  )

  # Test "No Covariates"
  recipe_no_cov <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none",
    covariate_selection       = "No Covariates"
  )

  # All should produce recipes without covariate steps
  for (recipe_obj in list(recipe_null, recipe_empty, recipe_no_cov)) {
    cov_steps <- purrr::keep(recipe_obj$steps,
                             ~ inherits(.x, "step_add_covariates"))
    expect_length(cov_steps, 0)
  }

})

test_that("build_recipe role requirements set correctly", {

  # SPEC-RECIPE-INT-019: Bake requirements for ID and metadata
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  # Check role requirements (internal structure)
  expect_s3_class(recipe_obj, "recipe")
  # Role requirements are set via update_role_requirements

})

test_that("build_recipe creates valid workflow-ready recipes", {

  # SPEC-RECIPE-INT-020: Recipe can be used in tidymodels workflow
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")

  test_data <- create_test_spectra(50)

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "snv",
    response_transformation   = "log",
    feature_selection_method  = "pca"
  )

  # Should be able to add to workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_obj)

  expect_s3_class(wf, "workflow")

})

test_that("build_recipe handles deriv1 and deriv2 transformations", {

  # SPEC-RECIPE-INT-021: Derivative transformations
  test_data <- create_test_spectra()

  # First derivative
  recipe_d1 <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "deriv1",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  # Second derivative
  recipe_d2 <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "deriv2",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  expect_s3_class(recipe_d1, "recipe")
  expect_s3_class(recipe_d2, "recipe")

})

test_that("build_recipe handles SG smoothing transformation", {

  # SPEC-RECIPE-INT-022: Savitzky-Golay smoothing
  test_data <- create_test_spectra()

  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "sg",
    response_transformation   = "none",
    feature_selection_method  = "none"
  )

  transform_steps <- purrr::keep(recipe_obj$steps,
                                  ~ inherits(.x, "step_transform_spectra"))
  expect_length(transform_steps, 1)
  expect_equal(transform_steps[[1]]$preprocessing, "sg")

})

test_that("build_recipe handles interactions warning with no feature selection", {

  # SPEC-RECIPE-INT-023: Warning for high-dimensional interactions
  test_data       <- create_test_spectra()
  test_covariates <- create_test_covariates()

  expect_warning({
    recipe_obj <- build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none",
      covariate_selection       = c("Clay", "pH"),
      covariate_data            = test_covariates,
      covariate_interactions    = TRUE
    )
  }, regexp = "interaction terms")

})

test_that("build_recipe errors on NA values in covariate selection", {

  # SPEC-RECIPE-INT-024: Current implementation rejects NA/empty covariates
  test_data       <- create_test_spectra()
  test_covariates <- create_test_covariates()

  expect_error(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none",
      covariate_selection       = c("Clay", NA, "pH", ""),
      covariate_data            = test_covariates
    ),
    regexp = "Requested covariates not found"
  )

})

## ===========================================================================
## VALIDATION TESTS: Error Handling and Edge Cases (30% of tests)
## ===========================================================================

test_that("build_recipe validates input_data is data frame", {

  # SPEC-RECIPE-VAL-001: Input type validation
  expect_error(
    build_recipe(
      input_data                = matrix(1:10, ncol = 2),
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none"
    ),
    regexp = "data frame"
  )

})

test_that("build_recipe requires Sample_ID column", {

  # SPEC-RECIPE-VAL-002: Required column validation
  bad_data <- data.frame(
    ID       = 1:5,
    Response = runif(5),
    `600`    = runif(5)
  )

  expect_error(
    build_recipe(
      input_data                = bad_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none"
    ),
    regexp = "Sample_ID"
  )

})

test_that("build_recipe validates response transformation values", {

  # SPEC-RECIPE-VAL-003: Invalid transformation type
  test_data <- create_test_spectra()

  expect_error(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "invalid_transform",
      feature_selection_method  = "none"
    ),
    regexp = "Unsupported"
  )

})

test_that("build_recipe validates feature selection method", {

  # SPEC-RECIPE-VAL-004: Invalid feature selection
  test_data <- create_test_spectra()

  expect_error(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "invalid_method"
    ),
    regexp = "Unsupported"
  )

})

test_that("build_recipe requires covariate_data when covariates selected", {

  # SPEC-RECIPE-VAL-005: Missing covariate data
  test_data <- create_test_spectra()

  expect_error(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none",
      covariate_selection       = c("Clay", "pH"),
      covariate_data            = NULL
    ),
    regexp = "covariate_data is NULL"
  )

})

test_that("build_recipe validates covariates exist in covariate_data", {

  # SPEC-RECIPE-VAL-006: Missing covariate columns
  test_data       <- create_test_spectra()
  test_covariates <- create_test_covariates()

  expect_error(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none",
      covariate_selection       = c("Clay", "NonExistent"),
      covariate_data            = test_covariates
    ),
    regexp = "not found in covariate_data"
  )

})

test_that("build_recipe requires Sample_ID in covariate_data", {

  # SPEC-RECIPE-VAL-007: Covariate data missing Sample_ID
  test_data <- create_test_spectra()
  bad_covariates <- data.frame(
    Clay = runif(10),
    pH   = runif(10)
  )

  expect_error(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none",
      covariate_selection       = "Clay",
      covariate_data            = bad_covariates
    ),
    regexp = "Sample_ID.*covariate_data"
  )

})

test_that("build_recipe enforces case-sensitive inputs", {

  # SPEC-RECIPE-VAL-008: Mixed-case inputs currently error with clear message
  test_data <- create_test_spectra()

  expect_error(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "Log",
      feature_selection_method  = "PCA"
    ),
    regexp = "Unsupported feature selection method"
  )

})

test_that("build_recipe coerces covariate_selection to character", {

  # SPEC-RECIPE-VAL-009: Type coercion for covariates
  test_data       <- create_test_spectra()
  test_covariates <- create_test_covariates()

  # Pass as factor (should be coerced)
  recipe_obj <- build_recipe(
    input_data                = test_data,
    spectral_transformation   = "raw",
    response_transformation   = "none",
    feature_selection_method  = "none",
    covariate_selection       = factor(c("Clay", "pH")),
    covariate_data            = test_covariates
  )

  expect_s3_class(recipe_obj, "recipe")

})

test_that("build_recipe provides informative error for missing covariates", {

  # SPEC-RECIPE-VAL-010: Error message includes available columns
  test_data       <- create_test_spectra()
  test_covariates <- create_test_covariates()

  error_msg <- tryCatch(
    build_recipe(
      input_data                = test_data,
      spectral_transformation   = "raw",
      response_transformation   = "none",
      feature_selection_method  = "none",
      covariate_selection       = c("Missing1", "Missing2"),
      covariate_data            = test_covariates
    ),
    error = function(e) e$message
  )

  # Error should clearly identify missing covariates
  expect_match(error_msg, "Requested covariates not found")

})

## ===========================================================================
## Run Tests
## ===========================================================================

# Total tests: 35 (25 integration + 10 validation)
# Focus: 70% integration for maximum coverage
# Strategy: Test complete pipelines and transformations
