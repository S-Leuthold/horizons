# Unit Tests for Custom Metrics
# Tests the custom spectroscopy metrics CCC, RPD, and RRMSE

library(testthat)
library(horizons)
library(yardstick)

## ---------------------------------------------------------------------------
## CCC (Concordance Correlation Coefficient) Tests
## ---------------------------------------------------------------------------

test_that("ccc_vec calculates perfect agreement correctly", {
  # Perfect agreement should give CCC = 1
  values <- c(1, 2, 3, 4, 5)
  
  result <- ccc_vec(truth = values, estimate = values)
  expect_equal(result, 1.0, tolerance = 1e-10)
})

test_that("ccc_vec calculates no correlation correctly", {
  # No correlation should give CCC near 0
  truth <- c(1, 2, 3, 4, 5)
  estimate <- c(5, 4, 3, 2, 1)  # Perfect negative correlation
  
  result <- ccc_vec(truth = truth, estimate = estimate)
  expect_lt(result, 0)  # Should be negative for negative correlation
  expect_gte(result, -1)  # Should be >= -1
})

test_that("ccc_vec handles edge cases", {
  # Single value
  single_result <- ccc_vec(truth = 5, estimate = 5)
  expect_true(is.na(single_result) || single_result == 1)  # Could be NA or 1
  
  # All same values (no variance)
  same_truth <- rep(5, 10)
  same_estimate <- rep(5, 10)
  same_result <- ccc_vec(truth = same_truth, estimate = same_estimate)
  expect_true(is.na(same_result) || same_result == 1)
  
  # With NA values
  truth_na <- c(1, 2, NA, 4, 5)
  estimate_na <- c(1.1, 2.1, 3.1, 4.1, NA)
  na_result <- ccc_vec(truth = truth_na, estimate = estimate_na, na_rm = TRUE)
  expect_true(is.numeric(na_result))
})

test_that("ccc integrates with yardstick framework", {
  # Test that ccc works as a yardstick metric
  data <- data.frame(
    truth = c(2.5, 3.1, 4.2, 5.8, 6.3),
    estimate = c(2.7, 3.0, 4.5, 5.5, 6.1)
  )
  
  # Test direct function
  direct_result <- ccc(data, truth = truth, estimate = estimate)
  expect_s3_class(direct_result, "data.frame")
  expect_true(".metric" %in% names(direct_result))
  expect_true(".estimate" %in% names(direct_result))
  expect_equal(direct_result$.metric, "ccc")
  
  # Test in metric_set
  metric_set_result <- yardstick::metric_set(ccc, yardstick::rmse)(data, truth, estimate)
  expect_true("ccc" %in% metric_set_result$.metric)
  expect_true("rmse" %in% metric_set_result$.metric)
})

## ---------------------------------------------------------------------------
## RPD (Ratio of Performance to Deviation) Tests
## ---------------------------------------------------------------------------

test_that("rpd_vec calculates ratio correctly", {
  # RPD = SD(truth) / RMSE(truth, estimate)
  truth <- c(10, 20, 30, 40, 50)
  estimate <- c(12, 22, 28, 38, 48)  # RMSE ≈ 2.83
  
  expected_sd <- sd(truth)
  expected_rmse <- sqrt(mean((truth - estimate)^2))
  expected_rpd <- expected_sd / expected_rmse
  
  result <- rpd_vec(truth = truth, estimate = estimate)
  expect_equal(result, expected_rpd, tolerance = 1e-10)
})

test_that("rpd_vec handles perfect predictions", {
  # Perfect predictions should give very high RPD
  truth <- c(1, 5, 10, 15, 20)
  estimate <- truth  # Perfect match
  
  # RPD should be infinite for perfect predictions
  result <- rpd_vec(truth = truth, estimate = estimate)
  expect_true(is.infinite(result) && result > 0)
})

test_that("rpd_vec handles constant truth values", {
  # Constant truth values (no deviation) should give RPD = 0
  truth <- rep(10, 5)
  estimate <- c(9, 10, 11, 10, 10)
  
  result <- rpd_vec(truth = truth, estimate = estimate)
  expect_equal(result, 0, tolerance = 1e-10)
})

test_that("rpd_vec gives meaningful interpretation values", {
  # Test interpretable RPD ranges
  set.seed(789)
  
  # Poor model (RPD < 1.5)
  truth_poor <- rnorm(50, 10, 2)
  estimate_poor <- truth_poor + rnorm(50, 0, 3)  # High error
  rpd_poor <- rpd_vec(truth_poor, estimate_poor)
  expect_lt(rpd_poor, 2.5)  # Should be relatively low
  
  # Good model (RPD > 2)
  truth_good <- rnorm(50, 10, 2)
  estimate_good <- truth_good + rnorm(50, 0, 0.5)  # Low error
  rpd_good <- rpd_vec(truth_good, estimate_good)
  expect_gt(rpd_good, 1.5)  # Should be higher
})

test_that("rpd integrates with yardstick framework", {
  data <- data.frame(
    truth = c(1, 5, 10, 15, 20),
    estimate = c(1.2, 4.8, 9.5, 15.5, 19.8)
  )
  
  # Test direct function
  direct_result <- rpd(data, truth = truth, estimate = estimate)
  expect_s3_class(direct_result, "data.frame")
  expect_equal(direct_result$.metric, "rpd")
  expect_gt(direct_result$.estimate, 0)  # RPD should be positive
  
  # Test in metric_set
  metrics <- yardstick::metric_set(rpd, yardstick::rsq)(data, truth, estimate)
  expect_true("rpd" %in% metrics$.metric)
})

## ---------------------------------------------------------------------------
## RRMSE (Relative Root Mean Square Error) Tests
## ---------------------------------------------------------------------------

test_that("rrmse_vec calculates relative error correctly", {
  # RRMSE = RMSE / mean(truth) * 100
  truth <- c(10, 20, 30, 40, 50)  # mean = 30
  estimate <- c(12, 18, 32, 38, 52)
  
  expected_rmse <- sqrt(mean((truth - estimate)^2))
  expected_rrmse <- (expected_rmse / mean(truth)) * 100
  
  result <- rrmse_vec(truth = truth, estimate = estimate)
  expect_equal(result, expected_rrmse, tolerance = 1e-10)
})

test_that("rrmse_vec gives percentage values", {
  # RRMSE should be in percentage (0-100+ range)
  truth <- abs(rnorm(20, 10, 3))  # Positive values
  estimate <- truth * rnorm(20, 1, 0.1)  # 10% relative error
  
  result <- rrmse_vec(truth = truth, estimate = estimate)
  expect_gt(result, 0)  # Should be positive
  expect_lt(result, 50)  # Should be reasonable percentage
})

test_that("rrmse_vec handles zero mean truth", {
  # Zero mean should give infinite RRMSE
  truth <- c(-5, 0, 5)  # mean = 0
  estimate <- c(-4, 1, 6)
  
  result <- rrmse_vec(truth = truth, estimate = estimate)
  expect_true(is.infinite(result))
})

test_that("rrmse_vec handles perfect predictions", {
  # Perfect predictions should give RRMSE = 0
  truth <- c(5, 10, 15, 20)
  estimate <- truth
  
  result <- rrmse_vec(truth = truth, estimate = estimate)
  expect_equal(result, 0, tolerance = 1e-10)
})

test_that("rrmse integrates with yardstick framework", {
  data <- data.frame(
    truth = c(5, 10, 15, 20, 25),
    estimate = c(5.5, 9.8, 14.5, 20.2, 24.8)
  )
  
  # Test direct function
  direct_result <- rrmse(data, truth = truth, estimate = estimate)
  expect_s3_class(direct_result, "data.frame")
  expect_equal(direct_result$.metric, "rrmse")
  expect_gte(direct_result$.estimate, 0)  # RRMSE should be non-negative
  
  # Test in metric_set with other metrics
  all_metrics <- yardstick::metric_set(rrmse, ccc, rpd)(data, truth, estimate)
  expect_setequal(all_metrics$.metric, c("rrmse", "ccc", "rpd"))
})

## ---------------------------------------------------------------------------
## Cross-Metric Consistency Tests
## ---------------------------------------------------------------------------

test_that("all custom metrics handle NA values consistently", {
  truth <- c(1, 2, NA, 4, 5)
  estimate <- c(1.1, 2.1, 3.1, NA, 5.1)
  
  # Test na_rm = TRUE
  ccc_result <- ccc_vec(truth, estimate, na_rm = TRUE)
  rpd_result <- rpd_vec(truth, estimate, na_rm = TRUE)
  rrmse_result <- rrmse_vec(truth, estimate, na_rm = TRUE)
  
  # All should return numeric values (not NA)
  expect_true(is.numeric(ccc_result) && !is.na(ccc_result))
  expect_true(is.numeric(rpd_result) && !is.na(rpd_result))
  expect_true(is.numeric(rrmse_result) && !is.na(rrmse_result))
  
  # Test na_rm = FALSE (default)
  ccc_na <- ccc_vec(truth, estimate, na_rm = FALSE)
  rpd_na <- rpd_vec(truth, estimate, na_rm = FALSE)
  rrmse_na <- rrmse_vec(truth, estimate, na_rm = FALSE)
  
  # All should return NA when na_rm = FALSE and NAs present
  expect_true(is.na(ccc_na))
  expect_true(is.na(rpd_na))
  expect_true(is.na(rrmse_na))
})

test_that("all custom metrics work with realistic soil data ranges", {
  # Simulate realistic soil property values
  set.seed(101112)
  
  # SOC (%) - typical range 0.5-10%
  soc_truth <- runif(50, 0.5, 10)
  soc_estimate <- soc_truth * exp(rnorm(50, 0, 0.2))  # Multiplicative error
  
  ccc_soc <- ccc_vec(soc_truth, soc_estimate)
  rpd_soc <- rpd_vec(soc_truth, soc_estimate)
  rrmse_soc <- rrmse_vec(soc_truth, soc_estimate)
  
  expect_metrics_in_bounds(ccc_soc, "ccc")
  expect_metrics_in_bounds(rpd_soc, "rpd")
  expect_metrics_in_bounds(rrmse_soc, "rrmse")
  
  # Clay (%) - typical range 5-60%
  clay_truth <- runif(50, 5, 60)
  clay_estimate <- clay_truth + rnorm(50, 0, 3)  # Additive error
  
  ccc_clay <- ccc_vec(clay_truth, clay_estimate)
  rpd_clay <- rpd_vec(clay_truth, clay_estimate)
  rrmse_clay <- rrmse_vec(clay_truth, clay_estimate)
  
  expect_metrics_in_bounds(ccc_clay, "ccc")
  expect_metrics_in_bounds(rpd_clay, "rpd")
  expect_metrics_in_bounds(rrmse_clay, "rrmse")
})

test_that("custom metrics are direction-aware", {
  # CCC should be maximized (higher = better)
  # RPD should be maximized (higher = better)
  # RRMSE should be minimized (lower = better)
  
  truth <- c(5, 10, 15, 20, 25)
  good_estimate <- c(5.1, 9.9, 15.1, 19.9, 25.1)  # Low error
  poor_estimate <- c(3, 12, 13, 22, 27)  # Higher error
  
  ccc_good <- ccc_vec(truth, good_estimate)
  ccc_poor <- ccc_vec(truth, poor_estimate)
  expect_gt(ccc_good, ccc_poor)  # Better model should have higher CCC
  
  rpd_good <- rpd_vec(truth, good_estimate)
  rpd_poor <- rpd_vec(truth, poor_estimate)
  expect_gt(rpd_good, rpd_poor)  # Better model should have higher RPD
  
  rrmse_good <- rrmse_vec(truth, good_estimate)
  rrmse_poor <- rrmse_vec(truth, poor_estimate)
  expect_lt(rrmse_good, rrmse_poor)  # Better model should have lower RRMSE
})

test_that("custom metrics work efficiently with large datasets", {
  # Test performance with larger datasets
  n <- 10000
  set.seed(131415)
  
  truth <- rnorm(n, 10, 5)
  estimate <- truth + rnorm(n, 0, 1)
  
  expect_within_time({
    ccc_large <- ccc_vec(truth, estimate)
    rpd_large <- rpd_vec(truth, estimate)
    rrmse_large <- rrmse_vec(truth, estimate)
  }, max_seconds = 0.5)  # Should be fast
  
  # Results should still be reasonable
  expect_true(ccc_large > 0.8)  # High correlation expected
  expect_true(rpd_large > 3)    # Good RPD expected
  expect_true(rrmse_large < 20)  # Reasonable RRMSE expected
})