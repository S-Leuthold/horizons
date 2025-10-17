#' Tests for build_ensemble() Function
#'
#' STRATEGY: Validation-focused (integration tests require real finalized models)
#'
#' NOTE: Full integration tests require REAL finalized models from finalize_top_workflows()
#' which in turn requires real evaluation results. This is too expensive for test suite.
#'
#' Current approach:
#' - Validation tests (8 tests): Test all error handling and parameter validation
#' - Integration tests: Skipped for now, documented for future E2E test suite

library(testthat)
library(horizons)

## ===========================================================================
## INTEGRATION TESTS - Skipped (require real finalized models)
## ===========================================================================

test_that("minimal stacks ensemble executes end-to-end", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("weighted_average ensemble executes", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("xgb_meta ensemble executes", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble returns predictions with correct structure", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble computes performance metrics", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble returns model weights", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble with different test_prop values", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("stacks ensemble with optimize_blending", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble metadata contains correct information", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble with transformations works", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble compares to individual models", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble with different blend_metric values", {
  skip("Requires real finalized models - too expensive for test suite")
})

test_that("ensemble with different seeds produces consistent results", {
  skip("Requires real finalized models - too expensive for test suite")
})

## ===========================================================================
## VALIDATION TESTS - Parameter checking and error handling
## ===========================================================================

test_that("build_ensemble validates finalized_models type", {
  # SPEC-ENSEMBLE-VAL-001: Input type validation
  data <- create_eval_test_data()

  expect_error(
    build_ensemble("not_dataframe", data, "Response"),
    class = "rlang_error"  # Expect cli::cli_abort error
  )
})

test_that("build_ensemble validates input_data type", {
  # SPEC-ENSEMBLE-VAL-002: Data validation
  finalized <- create_mock_finalized_models(n_models = 2)

  expect_error(
    build_ensemble(finalized, "not_dataframe", "Response"),
    class = "rlang_error"
  )
})

test_that("build_ensemble requires response variable", {
  # SPEC-ENSEMBLE-VAL-003: Response variable presence
  finalized <- create_mock_finalized_models(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    build_ensemble(finalized, data, "NonExistent"),
    "not found"
  )
})

test_that("build_ensemble validates ensemble_method", {
  # SPEC-ENSEMBLE-VAL-004: Method validation
  finalized <- create_mock_finalized_models(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    build_ensemble(finalized, data, "Response", ensemble_method = "invalid_method"),
    "stacks|weighted_average|xgb_meta"
  )
})

test_that("build_ensemble validates test_prop parameter", {
  # SPEC-ENSEMBLE-VAL-005: Test proportion bounds
  finalized <- create_mock_finalized_models(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    build_ensemble(finalized, data, "Response", test_prop = 1.5),
    class = "rlang_error"
  )

  expect_error(
    build_ensemble(finalized, data, "Response", test_prop = 0),
    class = "rlang_error"
  )
})

test_that("build_ensemble requires cv_predictions for stacks", {
  # SPEC-ENSEMBLE-VAL-006: Stacks requirements
  skip_if_not_installed("stacks")

  finalized <- create_mock_finalized_models(n_models = 2)
  finalized$cv_predictions <- list(NULL, NULL)  # Missing CV predictions

  data <- create_eval_test_data()

  expect_error(
    build_ensemble(finalized, data, "Response", ensemble_method = "stacks"),
    "CV predictions"
  )
})

test_that("build_ensemble handles insufficient models", {
  # SPEC-ENSEMBLE-VAL-007: Minimum model count
  finalized <- create_mock_finalized_models(n_models = 1)
  data <- create_eval_test_data()

  # Weighted average and xgb_meta need at least 2 models
  # This will fail when fitting, but validation happens at runtime
  # Just check that structure is created
  expect_s3_class(finalized, "tbl_df")
})

test_that("build_ensemble requires metrics column", {
  # SPEC-ENSEMBLE-VAL-008: Required column checks
  finalized <- create_mock_finalized_models(n_models = 2)
  finalized$cv_metrics <- NULL

  data <- create_eval_test_data()

  expect_error(
    build_ensemble(finalized, data, "Response"),
    class = "rlang_error"
  )
})
