# Unit Tests for Recipe Step Functions
# Tests the custom recipe steps that handle spectral preprocessing,
# feature selection, and covariate integration

library(testthat)
library(horizons)
library(recipes)
library(dplyr)

## ---------------------------------------------------------------------------
## Transform Spectra Tests - process_spectra() function
## ---------------------------------------------------------------------------

test_that("process_spectra handles raw processing correctly", {
  # Create test spectral vector with edge artifacts
  test_spectrum <- c(0.1, 0.2, 0.5, 0.7, 0.8, 0.9, 0.85, 0.6, 0.3, 0.1)
  window_size <- 5
  
  result <- process_spectra(test_spectrum, "raw", window_size = window_size)
  
  # Should trim edges: positions 3-8 (window_size=5, so trim 2 from each end)
  expected <- test_spectrum[3:8]
  expect_equal(result, expected)
  expect_equal(length(result), length(test_spectrum) - (window_size - 1))
})

test_that("process_spectra handles SNV transformation", {
  # Create test spectrum with known properties
  test_spectrum <- c(0.2, 0.4, 0.6, 0.8, 1.0, 0.8, 0.6, 0.4, 0.2)
  
  result <- process_spectra(test_spectrum, "snv", window_size = 5)
  
  # SNV should standardize to mean=0, sd=1, then trim edges
  expect_true(is.numeric(result))
  expect_equal(length(result), 5)  # 9 - (5-1) = 5
  expect_true(all(is.finite(result)))
})

test_that("process_spectra handles Savitzky-Golay smoothing", {
  # Create noisy spectrum
  set.seed(123)
  base_spectrum <- sin(seq(0, 2*pi, length.out = 20))
  noisy_spectrum <- base_spectrum + rnorm(20, 0, 0.1)
  
  result <- process_spectra(noisy_spectrum, "sg", window_size = 9)
  
  # Should smooth the spectrum  
  expect_true(is.numeric(result))
  expect_gt(length(result), 0)  # SG processing reduces length
  expect_true(all(is.finite(result)))
})

test_that("process_spectra handles first derivative", {
  # Create spectrum with clear peak
  x <- seq(1, 100, length.out = 50)
  spectrum <- exp(-(x - 25)^2 / 100)  # Gaussian peak at position 25
  
  result <- process_spectra(spectrum, "deriv1", window_size = 9)
  
  expect_true(is.numeric(result))
  expect_gt(length(result), 0)  # Derivative processing reduces length
  expect_true(all(is.finite(result)))
  
  # First derivative should have both positive and negative values
  expect_true(any(result > 0))
  expect_true(any(result < 0))
})

test_that("process_spectra handles second derivative", {
  # Create spectrum with multiple features
  x <- seq(1, 100, length.out = 50) 
  spectrum <- exp(-(x - 25)^2 / 100) + 0.5 * exp(-(x - 40)^2 / 50)
  
  result <- process_spectra(spectrum, "deriv2", window_size = 11)
  
  expect_true(is.numeric(result))
  expect_gt(length(result), 0)  # Second derivative processing reduces length
  expect_true(all(is.finite(result)))
})

test_that("process_spectra handles combined SNV + derivative", {
  test_spectrum <- seq(0.1, 1.0, length.out = 30)
  
  # Test SNV + first derivative
  result1 <- process_spectra(test_spectrum, "snv_deriv1", window_size = 9)
  expect_true(is.numeric(result1))
  expect_gt(length(result1), 0)
  
  # Test SNV + second derivative  
  result2 <- process_spectra(test_spectrum, "snv_deriv2", window_size = 11)
  expect_true(is.numeric(result2))
  expect_gt(length(result2), 0)
})

test_that("process_spectra errors on unknown transformation", {
  test_spectrum <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  
  expect_error(
    process_spectra(test_spectrum, "unknown_method"),
    "Unknown preprocessing type"
  )
})

## ---------------------------------------------------------------------------
## Transform Spectra Tests - Recipe Integration
## ---------------------------------------------------------------------------

test_that("step_transform_spectra creates proper recipe step", {
  # Create test spectral data
  test_data <- create_tiny_spectra(seed = 456)
  
  # Create recipe with spectral transformation
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Project, new_role = "metadata") %>%
    step_transform_spectra(all_predictors(), preprocessing = "snv")
  
  expect_s3_class(rec, "recipe")
  
  # Check that step was added
  step_names <- purrr::map_chr(rec$steps, ~ class(.x)[1])
  expect_true("step_transform_spectra" %in% step_names)
})

test_that("step_transform_spectra prep and bake work correctly", {
  # Create test data with consistent spectral columns
  test_data <- create_tiny_spectra(seed = 789)
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Project, new_role = "metadata") %>%
    step_transform_spectra(all_predictors(), preprocessing = "raw", window_size = 5)
  
  # Prep the recipe
  prepped_rec <- prep(rec, training = test_data)
  expect_true(prepped_rec$steps[[1]]$trained)
  
  # Bake the recipe
  baked_data <- bake(prepped_rec, new_data = test_data)
  
  expect_s3_class(baked_data, "data.frame")
  expect_true("SOC" %in% names(baked_data))
  expect_true("Sample_ID" %in% names(baked_data))
  
  # Should have fewer spectral columns due to edge trimming
  spectral_cols_orig <- sum(grepl("^[0-9]", names(test_data)))
  spectral_cols_baked <- sum(grepl("^spec", names(baked_data)))
  expect_lt(spectral_cols_baked, spectral_cols_orig)
})

test_that("step_transform_spectra handles edge cases", {
  test_data <- create_tiny_spectra(seed = 101112)
  
  # Test with large window size
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Project, new_role = "metadata") %>%
    step_transform_spectra(all_predictors(), preprocessing = "raw", window_size = 15)
  
  prepped_rec <- prep(rec, training = test_data)
  baked_data <- bake(prepped_rec, new_data = test_data)
  
  expect_s3_class(baked_data, "data.frame")
  expect_gt(nrow(baked_data), 0)
})

test_that("step_transform_spectra validates numeric columns", {
  # Create data with non-numeric spectral column
  bad_data <- data.frame(
    Sample_ID = 1:5,
    SOC = rnorm(5, 2, 0.5),
    "600" = c("a", "b", "c", "d", "e"),  # Non-numeric spectral column
    "700" = rnorm(5),
    check.names = FALSE
  )
  
  rec <- recipe(SOC ~ ., data = bad_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_transform_spectra(all_predictors(), preprocessing = "snv")
  
  expect_error(prep(rec, training = bad_data), "must be numeric")
})

## ---------------------------------------------------------------------------
## Feature Selection Tests - Correlation Method
## ---------------------------------------------------------------------------

test_that("step_select_correlation creates proper recipe step", {
  test_data <- create_tiny_spectra(seed = 131415)
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_select_correlation(all_predictors(), outcome = "SOC")
  
  expect_s3_class(rec, "recipe")
  
  step_names <- purrr::map_chr(rec$steps, ~ class(.x)[1])
  expect_true("step_select_correlation" %in% step_names)
})

test_that("step_select_correlation selects relevant features", {
  # Create data where some wavelengths are correlated with outcome
  set.seed(161718)
  n_samples <- 20
  n_wavelengths <- 30
  
  # Create predictive wavelengths (first 10) and noise wavelengths
  predictive_data <- matrix(rnorm(n_samples * 10), nrow = n_samples)
  noise_data <- matrix(rnorm(n_samples * 20, 0, 0.1), nrow = n_samples)
  
  # Create outcome correlated with predictive wavelengths
  soc_values <- rowMeans(predictive_data) + rnorm(n_samples, 0, 0.2)
  
  # Combine into test dataset
  spectral_matrix <- cbind(predictive_data, noise_data)
  colnames(spectral_matrix) <- paste0("X", 600:(600 + n_wavelengths - 1))
  
  test_data <- data.frame(
    Sample_ID = 1:n_samples,
    SOC = soc_values,
    spectral_matrix,
    check.names = FALSE
  )
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_select_correlation(all_predictors(), outcome = "SOC")
  
  prepped_rec <- prep(rec, training = test_data)
  baked_data <- bake(prepped_rec, new_data = test_data)
  
  expect_s3_class(baked_data, "data.frame")
  expect_true("SOC" %in% names(baked_data))
  expect_true("Sample_ID" %in% names(baked_data))
  
  # Should have fewer predictors than original
  orig_predictors <- sum(grepl("^X", names(test_data)))
  selected_predictors <- sum(grepl("^X", names(baked_data)))
  expect_lte(selected_predictors, orig_predictors)
})

## ---------------------------------------------------------------------------
## Covariate Integration Tests
## ---------------------------------------------------------------------------

test_that("step_add_covariates creates proper recipe step", {
  test_data <- create_tiny_spectra(seed = 192021)
  
  # Create mock covariate data
  covariate_data <- data.frame(
    Sample_ID = test_data$Sample_ID,
    elevation = rnorm(nrow(test_data), 1000, 200),
    temperature = rnorm(nrow(test_data), 15, 5)
  )
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_add_covariates(covariate_data = covariate_data, sample_id_column = Sample_ID)
  
  expect_s3_class(rec, "recipe")
  
  step_names <- purrr::map_chr(rec$steps, ~ class(.x)[1])
  expect_true("step_add_covariates" %in% step_names)
})

test_that("step_add_covariates joins and scales covariates correctly", {
  test_data <- create_tiny_spectra(seed = 222324)
  
  # Create covariate data with known scaling properties
  covariate_data <- data.frame(
    Sample_ID = test_data$Sample_ID,
    elevation = c(800, 900, 1000, 1100, 1200),  # Will scale to mean=0, sd=1
    slope = c(5, 10, 15, 20, 25)
  )
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_add_covariates(covariate_data = covariate_data, sample_id_column = Sample_ID)
  
  prepped_rec <- prep(rec, training = test_data)
  baked_data <- bake(prepped_rec, new_data = test_data)
  
  expect_s3_class(baked_data, "data.frame")
  expect_true("elevation" %in% names(baked_data))
  expect_true("slope" %in% names(baked_data))
  
  # Check that covariates are scaled (approximately mean=0, sd=1)
  expect_lt(abs(mean(baked_data$elevation)), 0.1)
  expect_lt(abs(sd(baked_data$elevation) - 1), 0.1)
})

test_that("step_add_covariates handles missing covariates", {
  test_data <- create_tiny_spectra(seed = 252627)
  
  # Create covariate data missing some samples
  covariate_data <- data.frame(
    Sample_ID = test_data$Sample_ID[1:3],  # Missing samples 4-5
    elevation = c(800, 900, 1000)
  )
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_add_covariates(covariate_data = covariate_data, sample_id_column = Sample_ID)
  
  prepped_rec <- prep(rec, training = test_data)
  baked_data <- bake(prepped_rec, new_data = test_data)
  
  # Should have NA values for missing covariate data
  expect_true(any(is.na(baked_data$elevation)))
  expect_equal(sum(is.na(baked_data$elevation)), 2)  # Samples 4-5 missing
})

## ---------------------------------------------------------------------------
## Recipe Integration Tests
## ---------------------------------------------------------------------------

test_that("multiple recipe steps work together", {
  test_data <- create_tiny_spectra(seed = 282930)
  
  # Create complete preprocessing pipeline
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Project, new_role = "metadata") %>%
    step_transform_spectra(all_predictors(), preprocessing = "snv") %>%
    step_select_correlation(all_predictors(), outcome = "SOC")
  
  prepped_rec <- prep(rec, training = test_data)
  baked_data <- bake(prepped_rec, new_data = test_data)
  
  expect_s3_class(baked_data, "data.frame")
  expect_true("SOC" %in% names(baked_data))
  expect_true("Sample_ID" %in% names(baked_data))
  
  # Should have some spectral predictors remaining
  spectral_predictors <- sum(grepl("^spec", names(baked_data)))
  expect_gt(spectral_predictors, 0)
})

test_that("recipe steps preserve data integrity", {
  test_data <- create_tiny_spectra(seed = 313233)
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Project, new_role = "metadata") %>%
    step_transform_spectra(all_predictors(), preprocessing = "raw", window_size = 5)
  
  prepped_rec <- prep(rec, training = test_data)
  baked_data <- bake(prepped_rec, new_data = test_data)
  
  # Check data integrity
  expect_equal(nrow(baked_data), nrow(test_data))
  expect_equal(baked_data$Sample_ID, test_data$Sample_ID)
  expect_equal(baked_data$SOC, test_data$SOC)
  
  # No missing values in processed spectral data
  spectral_cols <- grepl("^spec", names(baked_data))
  expect_false(any(is.na(baked_data[, spectral_cols])))
})

test_that("recipe steps work with different data sizes", {
  # Test with minimal data
  minimal_data <- data.frame(
    Sample_ID = 1:3,
    SOC = c(1.5, 2.0, 2.5),
    "600" = c(0.1, 0.2, 0.3),
    "700" = c(0.4, 0.5, 0.6),
    "800" = c(0.7, 0.8, 0.9),
    check.names = FALSE
  )
  
  rec <- recipe(SOC ~ ., data = minimal_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_transform_spectra(all_predictors(), preprocessing = "raw", window_size = 3)
  
  # Should work without error
  expect_no_error({
    prepped_rec <- prep(rec, training = minimal_data)
    baked_data <- bake(prepped_rec, new_data = minimal_data)
  })
})

test_that("recipe steps handle performance efficiently", {
  # Test with medium-sized data
  medium_data <- create_medium_spectra(seed = 343536)
  
  expect_within_time({
    rec <- recipe(SOC ~ ., data = medium_data) %>%
      update_role(Sample_ID, new_role = "id") %>%
      update_role(Project, new_role = "metadata") %>%
      step_transform_spectra(all_predictors(), preprocessing = "snv") %>%
      step_select_correlation(all_predictors(), outcome = "SOC")
    
    prepped_rec <- prep(rec, training = medium_data)
    baked_data <- bake(prepped_rec, new_data = medium_data)
  }, max_seconds = 5.0)  # Should complete reasonably quickly
})

test_that("recipe print methods work correctly", {
  test_data <- create_tiny_spectra(seed = 373839)
  
  rec <- recipe(SOC ~ ., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Project, new_role = "metadata") %>%
    step_transform_spectra(all_predictors(), preprocessing = "deriv1", window_size = 7)
  
  # Print method should work without error
  expect_output(print(rec$steps[[1]]), "Spectral transformation")
})