#' TEMPLATE: Integration Tests for evaluate_models_local()
#'
#' This file demonstrates the INTEGRATION-FIRST testing pattern recommended
#' by testing-automation-engineer. Use this template for next session.
#'
#' KEY PRINCIPLES:
#' - 70% integration/behavior tests (test actual execution)
#' - 30% validation tests (parameter checks)
#' - Use real tidymodels objects (minimal mocking)
#' - Fast fixtures (25 samples, 100 wavelengths)
#' - Target 40-50% coverage per file

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## TEMPLATE: Happy Path Integration Test
## ---------------------------------------------------------------------------

test_that("TEMPLATE: minimal workflow executes end-to-end", {
  skip("TEMPLATE for next session - demonstrates integration test pattern")

  # ARRANGE
  config <- create_eval_test_config()
  data <- create_eval_test_data()

  # ACT
  result <- evaluate_models_local(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 2,      # Minimal for speed
    bayesian_iter = 0,  # Skip Bayesian for speed
    cv_folds = 3,       # Minimal reliable folds
    allow_par = FALSE,  # Sequential for reproducibility
    verbose = FALSE
  )

  # ASSERT - Behavior checks (not just structure)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)  # One config = one result
  expect_equal(result$status, "success")  # Should succeed
  expect_true(is.numeric(result$rsq))     # Has R² metric
  expect_true(result$rsq >= 0 & result$rsq <= 1)  # Valid R²
  expect_true(is.numeric(result$rmse))    # Has RMSE
  expect_true(result$rmse > 0)            # Positive error
  expect_true(result$grid_seconds > 0)    # Actually executed
})

## ---------------------------------------------------------------------------
## TEMPLATE: Multi-Model Integration Test
## ---------------------------------------------------------------------------

test_that("TEMPLATE: two-model workflow executes sequentially", {
  skip("TEMPLATE for next session")

  config <- data.frame(
    config_id = c("test_001", "test_002"),
    model = c("plsr", "random_forest"),
    transformation = c("none", "none"),
    preprocessing = c("raw", "raw"),
    feature_selection = c("none", "none"),
    covariates = I(list(NULL, NULL)),
    stringsAsFactors = FALSE
  )

  result <- evaluate_models_local(
    config = config,
    input_data = create_eval_test_data(),
    variable = "Response",
    grid_size = 2,
    bayesian_iter = 0,
    cv_folds = 3,
    allow_par = FALSE,
    verbose = FALSE
  )

  # Both models should succeed
  expect_equal(nrow(result), 2)
  expect_true(all(result$status == "success"))

  # Both should have metrics
  expect_true(all(!is.na(result$rsq)))
  expect_true(all(result$grid_seconds > 0))
})

## ---------------------------------------------------------------------------
## TEMPLATE: Transformation Integration Test
## ---------------------------------------------------------------------------

test_that("TEMPLATE: log transformation workflow executes", {
  skip("TEMPLATE for next session")

  config <- create_eval_test_config()
  config$transformation <- "log"

  data <- create_eval_test_data()
  data$Response <- abs(data$Response) + 0.1  # Ensure positive for log

  result <- evaluate_models_local(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 2,
    bayesian_iter = 0,
    cv_folds = 3,
    verbose = FALSE
  )

  expect_equal(result$status, "success")
  expect_equal(result$transformation, "log")
  expect_true(!is.na(result$rsq))
})

## ---------------------------------------------------------------------------
## NEXT SESSION TODO:
##
## 1. Remove skip() from these 3 tests above
## 2. Run them to verify they work
## 3. Add 15-20 more similar tests covering:
##    - Different preprocessing (snv, deriv1, deriv2)
##    - Different feature selection (pca, correlation, boruta)
##    - Workflow features (resume, parallel, pruning, bayesian)
##    - Edge cases (single config, min cv_folds, etc)
##    - ONLY 2-3 validation tests (missing variable, insufficient samples)
##
## 4. After each 5 tests, run:
##    library(covr)
##    file_cov <- file_coverage("R/evaluation-local.R",
##                               "tests/testthat/test-evaluation-local.R")
##    print(percent_coverage(file_cov))
##
## 5. Target: 40-50% coverage of evaluation-local.R (110-137 lines covered)
##    This should add +3-5% to total package coverage
##
## 6. Time estimate: 90-120 minutes for evaluation-local.R
##
## ---------------------------------------------------------------------------
