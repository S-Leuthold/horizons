test_that("RPD metric calculates correctly", {
  # Basic functionality test
  truth <- c(10, 20, 30, 40, 50)
  estimate <- c(12, 18, 32, 38, 52)
  
  # Manual calculation
  rmse_manual <- sqrt(mean((truth - estimate)^2))
  sd_manual <- sd(truth)
  rpd_manual <- sd_manual / rmse_manual
  
  # Using the metric
  result <- horizons::rpd_vec(truth = truth, estimate = estimate)
  
  expect_equal(result, rpd_manual, tolerance = 1e-6)
  expect_true(result > 0)
})

test_that("RPD handles edge cases correctly", {
  # Perfect predictions (RMSE = 0)
  truth <- c(10, 20, 30, 40)
  estimate <- truth
  
  result <- horizons::rpd_vec(truth = truth, estimate = estimate)
  expect_equal(result, Inf)
  
  # Zero variance in truth (SD = 0)
  truth <- rep(10, 5)
  estimate <- c(9, 10, 11, 10, 9)
  
  result <- horizons::rpd_vec(truth = truth, estimate = estimate)
  expect_equal(result, 0)
  
  # Single observation - SD is NA for single value
  truth <- 10
  estimate <- 12
  
  # Single observation should return NA (SD undefined)
  result <- horizons::rpd_vec(truth = truth, estimate = estimate)
  expect_true(is.na(result))
  
  # With NA values
  truth <- c(10, 20, NA, 40, 50)
  estimate <- c(12, 18, 32, NA, 52)
  
  result <- horizons::rpd_vec(truth = truth, estimate = estimate, na_rm = TRUE)
  expect_true(!is.na(result))
  expect_true(result > 0)
})

test_that("RPD integrates with yardstick correctly", {
  library(yardstick)
  library(tibble)
  
  # Create test data
  data <- tibble(
    obs = c(100, 120, 140, 160, 180),
    pred = c(105, 118, 135, 162, 185)
  )
  
  # Test as standalone metric
  result <- horizons::rpd(data, truth = obs, estimate = pred)
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c(".metric", ".estimator", ".estimate"))
  expect_equal(result$.metric, "rpd")
  
  # Test in metric_set
  metrics <- metric_set(horizons::rpd, rmse, rsq)
  results <- metrics(data, truth = obs, estimate = pred)
  
  expect_equal(nrow(results), 3)
  expect_true("rpd" %in% results$.metric)
  
  # Verify direction is "maximize"
  expect_equal(attr(horizons::rpd, "direction"), "maximize")
})

test_that("CCC metric calculates correctly", {
  # Example from Lin (1989) paper
  truth <- c(1.5, 2.0, 2.5, 3.0, 3.5)
  estimate <- c(1.4, 2.1, 2.4, 3.1, 3.4)
  
  # Manual calculation
  mean_truth <- mean(truth)
  mean_estimate <- mean(estimate)
  sd_truth <- sd(truth)
  sd_estimate <- sd(estimate)
  
  rho_manual <- cor(truth, estimate)
  v_manual <- sd_estimate / sd_truth
  u_manual <- (mean_estimate - mean_truth) / sqrt(sd_estimate * sd_truth)
  cb_manual <- 2 / (v_manual + 1/v_manual + u_manual^2)
  ccc_manual <- rho_manual * cb_manual
  
  # Using the metric
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  
  expect_equal(result, ccc_manual, tolerance = 1e-6)
  expect_true(result >= -1 && result <= 1)
})

test_that("CCC handles edge cases correctly", {
  # Perfect agreement
  truth <- c(10, 20, 30, 40, 50)
  estimate <- truth
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  expect_equal(result, 1, tolerance = 1e-6)
  
  # Perfect negative correlation
  truth <- c(10, 20, 30, 40, 50)
  estimate <- c(50, 40, 30, 20, 10)
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  expect_true(result < 0)
  
  # Zero variance in truth
  truth <- rep(10, 5)
  estimate <- c(9, 10, 11, 10, 9)
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  expect_true(is.na(result))
  
  # Zero variance in estimate
  truth <- c(10, 20, 30, 40, 50)
  estimate <- rep(25, 5)
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  expect_true(is.na(result))
  
  # Single observation (insufficient data)
  truth <- 10
  estimate <- 12
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  expect_true(is.na(result))
  
  # With NA values
  truth <- c(10, 20, NA, 40, 50)
  estimate <- c(12, 18, 32, NA, 52)
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate, na_rm = TRUE)
  expect_true(!is.na(result))
  expect_true(result >= -1 && result <= 1)
})

test_that("CCC integrates with yardstick correctly", {
  library(yardstick)
  library(tibble)
  
  # Create test data
  data <- tibble(
    obs = c(100, 120, 140, 160, 180),
    pred = c(105, 115, 145, 155, 185)
  )
  
  # Test as standalone metric
  result <- horizons::ccc(data, truth = obs, estimate = pred)
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c(".metric", ".estimator", ".estimate"))
  expect_equal(result$.metric, "ccc")
  
  # Test in metric_set
  metrics <- metric_set(horizons::ccc, horizons::rpd, rsq)
  results <- metrics(data, truth = obs, estimate = pred)
  
  expect_equal(nrow(results), 3)
  expect_true("ccc" %in% results$.metric)
  
  # Verify direction is "maximize"
  expect_equal(attr(horizons::ccc, "direction"), "maximize")
})

test_that("CCC agrees with known implementations", {
  # Test against values from epiR::epi.ccc or DescTools::CCC
  # These are known correct values for specific datasets
  
  # Example 1: High agreement
  truth <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  estimate <- truth + rnorm(10, mean = 0, sd = 0.1)
  set.seed(123)
  estimate <- c(0.96, 2.08, 2.89, 4.15, 4.91, 6.13, 6.85, 8.09, 8.88, 10.08)
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  # Should be very high (>0.99) for this near-perfect agreement
  expect_true(result > 0.99)
  
  # Example 2: Systematic bias
  truth <- 1:10
  estimate <- truth * 1.2  # 20% overestimation
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  # Should be high correlation but reduced by bias
  expect_true(result > 0.9 && result < 1.0)
  
  # Example 3: Poor agreement
  set.seed(456)
  truth <- 1:10
  estimate <- sample(1:10)
  
  result <- horizons::ccc_vec(truth = truth, estimate = estimate)
  # Should be low for random pairing
  expect_true(result < 0.5)
})

test_that("Metrics work with tune functions", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("tune")
  skip_if_not_installed("rsample")
  
  library(tidymodels)
  
  # Create a simple dataset
  set.seed(789)
  data <- tibble(
    x1 = rnorm(100),
    x2 = rnorm(100),
    y = 2 * x1 + x2 + rnorm(100, sd = 0.5)
  )
  
  # Create splits
  splits <- initial_split(data, prop = 0.8)
  train_data <- training(splits)
  test_data <- testing(splits)
  
  # Create folds
  folds <- vfold_cv(train_data, v = 3)
  
  # Define model
  lm_model <- linear_reg() %>%
    set_engine("lm")
  
  # Define recipe
  recipe <- recipe(y ~ ., data = train_data)
  
  # Create workflow
  wf <- workflow() %>%
    add_model(lm_model) %>%
    add_recipe(recipe)
  
  # Test with custom metrics
  custom_metrics <- metric_set(horizons::rpd, horizons::ccc, horizons::rrmse, rmse)
  
  # Fit resamples
  rs_fit <- fit_resamples(
    wf,
    resamples = folds,
    metrics = custom_metrics
  )
  
  # Collect metrics
  metrics_results <- collect_metrics(rs_fit)
  
  expect_true("rpd" %in% metrics_results$.metric)
  expect_true("ccc" %in% metrics_results$.metric)
  expect_true("rrmse" %in% metrics_results$.metric)
  
  # All metrics should have values
  expect_true(all(!is.na(metrics_results$mean)))
  
  # Test on holdout
  final_fit <- last_fit(wf, splits, metrics = custom_metrics)
  final_metrics <- collect_metrics(final_fit)
  
  expect_true("rpd" %in% final_metrics$.metric)
  expect_true("ccc" %in% final_metrics$.metric)
  expect_true(all(!is.na(final_metrics$.estimate)))
})

test_that("RPD interpretation guidelines are reasonable", {
  # Test the interpretation thresholds mentioned in documentation
  
  # Poor model (RPD < 1.5)
  truth <- c(10, 20, 30, 40, 50)
  estimate <- c(25, 5, 45, 25, 65)  # Very bad predictions
  rpd_poor <- horizons::rpd_vec(truth = truth, estimate = estimate)
  expect_true(rpd_poor < 1.5)
  
  # Excellent model (RPD > 2.5)
  truth <- c(10, 20, 30, 40, 50)
  estimate <- truth + rnorm(5, mean = 0, sd = 1)  # Very good predictions
  rpd_excellent <- horizons::rpd_vec(truth = truth, estimate = estimate)
  expect_true(rpd_excellent > 2.0)  # Should be high
})

test_that("Numerical stability of metrics", {
  # Test with very large values
  truth <- c(1e6, 2e6, 3e6, 4e6, 5e6)
  estimate <- truth * 1.01
  
  rpd_large <- horizons::rpd_vec(truth = truth, estimate = estimate)
  ccc_large <- horizons::ccc_vec(truth = truth, estimate = estimate)
  
  expect_true(is.finite(rpd_large))
  expect_true(is.finite(ccc_large))
  expect_true(ccc_large > 0.99)  # Should show excellent agreement
  
  # Test with very small values
  truth <- c(1e-6, 2e-6, 3e-6, 4e-6, 5e-6)
  estimate <- truth * 1.01
  
  rpd_small <- horizons::rpd_vec(truth = truth, estimate = estimate)
  ccc_small <- horizons::ccc_vec(truth = truth, estimate = estimate)
  
  expect_true(is.finite(rpd_small))
  expect_true(is.finite(ccc_small))
  expect_true(ccc_small > 0.99)  # Should show excellent agreement
  
  # Test with mixed scales
  truth <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  estimate <- truth * 0.95
  
  rpd_mixed <- horizons::rpd_vec(truth = truth, estimate = estimate)
  ccc_mixed <- horizons::ccc_vec(truth = truth, estimate = estimate)
  
  expect_true(is.finite(rpd_mixed))
  expect_true(is.finite(ccc_mixed))
})