# Unit Tests for Back-transformation Utilities
# Tests the critical back-transformation functions that ensure predictions
# are correctly converted from transformed scale back to original scale

library(testthat)
library(horizons)

test_that("back_transform_predictions handles log transformation correctly", {
  # Test basic log transformation
  original_values <- c(1, 5, 10, 50, 100)
  log_values <- log(original_values)
  
  result <- back_transform_predictions(log_values, "log")
  expect_equal(result, original_values, tolerance = 1e-10)
  
  # Test mathematical property: exp(log(x)) = x
  expect_backtransform_properties(original_values, log_values, "log")
})

test_that("back_transform_predictions handles sqrt transformation correctly", {
  # Test basic sqrt transformation  
  original_values <- c(1, 4, 9, 16, 25, 100)
  sqrt_values <- sqrt(original_values)
  
  result <- back_transform_predictions(sqrt_values, "sqrt")
  expect_equal(result, original_values, tolerance = 1e-10)
  
  # Test mathematical property: (sqrt(x))^2 = x
  expect_backtransform_properties(original_values, sqrt_values, "sqrt")
})

test_that("back_transform_predictions handles none transformation correctly", {
  # Test identity transformation
  values <- c(-5, 0, 1, 10, 100)
  
  result <- back_transform_predictions(values, "none")
  expect_equal(result, values)
  
  # Test mathematical property: identity
  expect_backtransform_properties(values, values, "none")
})

test_that("back_transform_predictions preserves NA values", {
  # Test with NA values in different positions
  values_with_na <- c(log(10), NA, log(30), log(5))
  
  result <- back_transform_predictions(values_with_na, "log")
  
  expect_true(is.na(result[2]))
  expect_equal(result[c(1, 3, 4)], c(10, 30, 5), tolerance = 1e-10)
  expect_equal(length(result), length(values_with_na))
})

test_that("back_transform_predictions handles edge cases", {
  # Empty vector
  empty_result <- back_transform_predictions(numeric(0), "log")
  expect_equal(empty_result, numeric(0))
  
  # NULL input
  null_result <- back_transform_predictions(NULL, "log")
  expect_null(null_result)
  
  # Single value
  single_result <- back_transform_predictions(log(5), "log")
  expect_equal(single_result, 5, tolerance = 1e-10)
})

test_that("back_transform_predictions handles unknown transformation", {
  values <- c(1, 2, 3)
  
  # Should return original values for unknown transformation
  result <- back_transform_predictions(values, "unknown_transform", warn = FALSE)
  expect_equal(result, values)
  
  # Should warn by default
  expect_warning(
    back_transform_predictions(values, "unknown_transform", warn = TRUE),
    "Unknown transformation"
  )
})

test_that("back_transform_predictions warns for problematic values", {
  # Test warning for negative values with sqrt (though function should still work)
  negative_values <- c(-1, 0, 1, 4)
  
  # Should work but might warn about negative sqrt
  result <- back_transform_predictions(sqrt(abs(negative_values)), "sqrt", warn = TRUE)
  expect_equal(length(result), length(negative_values))
})

test_that("back_transform_cv_predictions works with CV prediction structure", {
  # Create mock CV predictions
  cv_preds <- create_mock_cv_predictions(20, n_folds = 5)
  
  # Transform the predictions
  cv_preds$.pred <- log(abs(cv_preds$.pred) + 1)  # Ensure positive
  
  result <- back_transform_cv_predictions(cv_preds, "log")
  
  expect_s3_class(result, "data.frame")
  expect_true(".pred" %in% names(result))
  expect_equal(nrow(result), nrow(cv_preds))
  
  # Check that other columns are preserved
  expect_true("id" %in% names(result))
  expect_equal(result$id, cv_preds$id)
})

test_that("back_transform_last_fit works with last_fit structure", {
  # Create mock last_fit results
  predictions <- create_mock_predictions(15)
  predictions$.pred <- log(abs(predictions$.pred) + 1)
  
  # Mock last_fit structure
  last_fit_mock <- list(
    .predictions = list(predictions)
  )
  
  result <- back_transform_last_fit(last_fit_mock, "log")
  
  expect_type(result, "list")
  expect_true(".predictions" %in% names(result))
  
  # Check predictions were back-transformed
  back_preds <- result$.predictions[[1]]
  expect_true(all(back_preds$.pred > 0))  # Should be positive after exp()
})

test_that("compute_original_scale_metrics calculates metrics correctly", {
  # Create predictions on original scale
  n <- 50
  set.seed(123)
  truth_values <- abs(rnorm(n, 10, 3))
  pred_values <- truth_values + rnorm(n, 0, 1)  # Add some error
  
  metrics <- compute_original_scale_metrics(
    truth = truth_values,
    estimate = pred_values,
    metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq, ccc)
  )
  
  expect_s3_class(metrics, "data.frame")
  expect_true(".metric" %in% names(metrics))
  expect_true(".estimate" %in% names(metrics))
  
  # Check we got the expected metrics
  metric_names <- metrics$.metric
  expect_true("rmse" %in% metric_names)
  expect_true("rsq" %in% metric_names)  
  expect_true("ccc" %in% metric_names)
  
  # Check metric bounds
  rmse_val <- metrics$.estimate[metrics$.metric == "rmse"]
  rsq_val <- metrics$.estimate[metrics$.metric == "rsq"]
  ccc_val <- metrics$.estimate[metrics$.metric == "ccc"]
  
  expect_metrics_in_bounds(rmse_val, "rmse")
  expect_metrics_in_bounds(rsq_val, "rsq")
  expect_metrics_in_bounds(ccc_val, "ccc")
})

test_that("back-transformation integrates correctly with yardstick", {
  # Test that back-transformed predictions work with yardstick metrics
  n <- 30
  set.seed(456)
  
  # Original scale data
  original_truth <- exp(rnorm(n, 2, 0.5))  # Log-normal
  original_pred <- original_truth * exp(rnorm(n, 0, 0.1))  # Add multiplicative error
  
  # Transform to log scale
  log_pred <- log(original_pred)
  
  # Back-transform
  recovered_pred <- back_transform_predictions(log_pred, "log")
  
  # Calculate metrics on back-transformed data
  rmse_original <- yardstick::rmse_vec(original_truth, original_pred)
  rmse_recovered <- yardstick::rmse_vec(original_truth, recovered_pred)
  
  # Should be very close (within numerical precision)
  expect_equal(rmse_original, rmse_recovered, tolerance = 1e-10)
})

test_that("back-transformation performance is reasonable", {
  # Test with larger dataset to ensure performance
  n <- 10000
  large_data <- rnorm(n, 0, 1)
  
  # Should complete quickly
  expect_within_time({
    result <- back_transform_predictions(large_data, "log")
  }, max_seconds = 0.1)
  
  expect_equal(length(result), n)
  expect_true(all(is.finite(result)))
})

test_that("back-transformation is case-insensitive for transformation names", {
  values <- log(c(1, 2, 3))
  
  # Test various cases
  expect_equal(
    back_transform_predictions(values, "LOG", warn = FALSE),
    back_transform_predictions(values, "log", warn = FALSE)
  )
  
  expect_equal(
    back_transform_predictions(values, "Log", warn = FALSE),
    back_transform_predictions(values, "log", warn = FALSE)  
  )
})

test_that("back-transformation works with different data types", {
  # Test with integer input
  int_values <- c(1L, 2L, 3L)
  log_int <- log(int_values)
  
  result <- back_transform_predictions(log_int, "log")
  expect_equal(result, as.numeric(int_values), tolerance = 1e-10)
  
  # Test with matrix (should work element-wise)
  matrix_values <- matrix(log(1:6), nrow = 2)
  matrix_result <- back_transform_predictions(matrix_values, "log")
  
  expect_true(is.matrix(matrix_result))
  expect_equal(dim(matrix_result), dim(matrix_values))
  expect_equal(as.vector(matrix_result), 1:6, tolerance = 1e-10)
})