## =============================================================================
## Tests: Quantile Prediction Functions
## =============================================================================

test_that("predict_quantiles() extracts quantiles from ranger workflow", {

  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("ranger")

  ## Create test data ------------------------------------------------------------

  set.seed(123)
  n <- 200
  train_data <- tibble::tibble(
    y = rnorm(n, mean = 10, sd = 2),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  test_data <- tibble::tibble(
    y = rnorm(50, mean = 10, sd = 2),
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  ## Build and fit quantile workflow ---------------------------------------------

  spec <- define_quantile_specification()
  spec$args$mtry <- 2
  spec$args$min_n <- 10

  recipe <- recipes::recipe(y ~ ., data = train_data)

  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_recipe(recipe) %>%
    parsnip::fit(data = train_data)

  ## Predict quantiles -----------------------------------------------------------

  quantiles <- predict_quantiles(wf, new_data = test_data)

  # Check structure
  expect_s3_class(quantiles, "tbl_df")
  expect_equal(nrow(quantiles), nrow(test_data))

  # Check columns (default q05, q95 → .pred_lower, .pred_upper)
  expect_true(".pred_lower" %in% names(quantiles))
  expect_true(".pred_upper" %in% names(quantiles))

  # Check values are numeric
  expect_true(all(is.numeric(quantiles$.pred_lower)))
  expect_true(all(is.numeric(quantiles$.pred_upper)))

  # Check monotonicity (lower <= upper)
  expect_true(all(quantiles$.pred_lower <= quantiles$.pred_upper))
})

test_that("predict_quantiles() works with custom quantiles", {

  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("ranger")

  ## Create and fit workflow -----------------------------------------------------

  set.seed(456)
  train_data <- tibble::tibble(
    y = rnorm(100),
    x1 = rnorm(100)
  )

  spec <- define_quantile_specification()
  spec$args$mtry <- 1
  spec$args$min_n <- 5

  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_recipe(recipes::recipe(y ~ ., data = train_data)) %>%
    parsnip::fit(data = train_data)

  ## Test custom quantiles -------------------------------------------------------

  # Request q10, q50, q90
  quantiles <- predict_quantiles(wf, train_data, quantiles = c(0.10, 0.50, 0.90))

  # Should have 3 columns with q naming
  expect_equal(ncol(quantiles), 3)
  expect_true(".pred_q10" %in% names(quantiles))
  expect_true(".pred_q50" %in% names(quantiles))
  expect_true(".pred_q90" %in% names(quantiles))

  # Check monotonicity across quantiles
  expect_true(all(quantiles$.pred_q10 <= quantiles$.pred_q50))
  expect_true(all(quantiles$.pred_q50 <= quantiles$.pred_q90))
})

test_that("predict_quantiles() validates inputs", {

  ## Test error handling ---------------------------------------------------------

  # Should error if workflow not trained
  empty_wf <- workflows::workflow()

  expect_error(
    predict_quantiles(empty_wf, data.frame(x = 1)),
    "trained workflow"
  )

  # Should error with invalid quantiles
  skip("Need fitted workflow to test - implement after basic tests pass")
})

test_that("predict_quantiles() warns about crossings", {

  skip("Implement after basic functionality works")

  # TODO: Create scenario that produces crossings
  # - Small sample size
  # - Extreme quantiles
  # - Verify warning is issued
})

## =============================================================================
## Tests: Quantile Crossing Repair
## =============================================================================

test_that("repair_quantile_crossings() fixes monotonicity violations", {

  ## Create data with crossings --------------------------------------------------

  preds_with_crossings <- tibble::tibble(
    .pred_lower = c(5, 7, 10, 3),
    .pred_upper = c(8, 6, 12, 9)   # Crossing at row 2
  )

  ## Repair ----------------------------------------------------------------------

  repaired <- repair_quantile_crossings(preds_with_crossings)

  # Check all monotonic
  expect_true(all(repaired$.pred_lower <= repaired$.pred_upper))

  # Check row 2 was fixed
  expect_equal(repaired$.pred_lower[2], 6)  # min(7, 6)
  expect_equal(repaired$.pred_upper[2], 7)  # max(7, 6)

  # Check other rows unchanged
  expect_equal(repaired$.pred_lower[1], 5)
  expect_equal(repaired$.pred_upper[1], 8)
})

test_that("repair_quantile_crossings() handles no crossings", {

  ## Data without crossings ------------------------------------------------------

  preds_ok <- tibble::tibble(
    .pred_lower = c(5, 7, 10),
    .pred_upper = c(8, 9, 12)
  )

  ## Repair (should be no-op) ---------------------------------------------------

  repaired <- repair_quantile_crossings(preds_ok)

  # Should be unchanged
  expect_identical(repaired, preds_ok)
})

test_that("repair_quantile_crossings() preserves interval width", {

  ## Create crossing -------------------------------------------------------------

  preds <- tibble::tibble(
    .pred_lower = c(10),
    .pred_upper = c(8)   # Crossing!
  )

  ## Repair ----------------------------------------------------------------------

  repaired <- repair_quantile_crossings(preds)

  # Width before: abs(10 - 8) = 2
  # Width after: abs(8 - 10) = 2
  width_before <- abs(preds$.pred_upper - preds$.pred_lower)
  width_after <- abs(repaired$.pred_upper - repaired$.pred_lower)

  expect_equal(width_before, width_after)
})

test_that("repair_quantile_crossings() works with custom column names", {

  ## Custom columns --------------------------------------------------------------

  preds <- tibble::tibble(
    q05 = c(5, 10),
    q95 = c(8, 9)   # Crossing at row 2
  )

  ## Repair with custom names ----------------------------------------------------

  repaired <- repair_quantile_crossings(preds, lower_col = "q05", upper_col = "q95")

  # Check monotonic
  expect_true(all(repaired$q05 <= repaired$q95))

  # Check row 2 fixed
  expect_equal(repaired$q05[2], 9)
  expect_equal(repaired$q95[2], 10)
})
