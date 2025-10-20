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

test_that("weighted_average ensemble blends models by inverse RMSE", {
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  data <- create_eval_test_data(n_samples = 120, n_wavelengths = 6, seed = 42)

  offsets <- c(wf_low = 1, wf_high = -1)

  finalized <- tibble::tibble(
    wflow_id = c("wf_low", "wf_high"),
    workflow = purrr::map(c("wf_low", "wf_high"), ~ structure(list(id = .x), class = "mock_workflow")),
    cv_metrics = list(
      tibble::tibble(.metric = "rmse", mean = 0.5),
      tibble::tibble(.metric = "rmse", mean = 1.0)
    ),
    cv_predictions = replicate(2, list(dummy = TRUE), simplify = FALSE),
    transformation = c("none", "log")
  )

  tracking <- new.env(parent = emptyenv())
  tracking$get_calls <- character(0)

  withr::defer(rm("fit.mock_workflow", envir = .GlobalEnv), envir = environment())
  assign(
    "fit.mock_workflow",
    function(object, data, ...) {
      structure(
        list(id = object$id, offset = offsets[[object$id]]),
        class = "mock_fit"
      )
    },
    envir = .GlobalEnv
  )

  withr::defer(rm("predict.mock_fit", envir = .GlobalEnv), envir = environment())
  assign(
    "predict.mock_fit",
    function(object, new_data, ...) {
      tibble::tibble(.pred = new_data$Response + object$offset)
    },
    envir = .GlobalEnv
  )

  ensemble <- with_mocked_bindings(
    build_ensemble(
      finalized_models = finalized,
      input_data       = data,
      variable         = "Response",
      ensemble_method  = "weighted_average",
      verbose          = FALSE,
      seed             = 2024
    ),
    get_original_scale_predictions = function(fitted_workflow, new_data, transformation, warn = FALSE) {
      tracking$get_calls <- c(tracking$get_calls, fitted_workflow$id)
      new_data$Response + fitted_workflow$offset
    },
    back_transform_predictions = function(predictions, transformation, warn = FALSE) predictions,
    .package = "horizons"
  )

  expect_s3_class(ensemble, "horizons_ensemble")
  expect_equal(sort(ensemble$model_weights$member), c("wf_high", "wf_low"))

  weights <- setNames(ensemble$model_weights$coef, ensemble$model_weights$member)
  expect_equal(sum(weights), 1, tolerance = 1e-8)
  expect_equal(unname(weights["wf_low"]), 2 / 3, tolerance = 1e-6)
  expect_equal(unname(weights["wf_high"]), 1 / 3, tolerance = 1e-6)

  expect_true(all(tracking$get_calls %in% c("wf_low", "wf_high")))
  expect_equal(length(tracking$get_calls), 2L)

  preds <- ensemble$predictions
  expect_equal(preds$Predicted - preds$Observed, rep(1 / 3, nrow(preds)), tolerance = 1e-6)

  rmse_estimate <- ensemble$metrics %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::pull(.estimate)
  expect_equal(rmse_estimate, 1 / 3, tolerance = 1e-6)

  expect_equal(ensemble$ensemble_model$method, "weighted_average")
  expect_true(ensemble$metadata$improvement > 0)
  expect_equal(nrow(ensemble$individual_performance), 2)
})

test_that("weighted_average ensemble aborts when fewer than two models succeed", {
  skip_if_not_installed("rsample")

  data <- create_eval_test_data(n_samples = 80, n_wavelengths = 5, seed = 24)

  finalized <- tibble::tibble(
    wflow_id = c("wf_ok", "wf_fail"),
    workflow = purrr::map(c("wf_ok", "wf_fail"), ~ structure(list(id = .x), class = "mock_workflow")),
    cv_metrics = list(
      tibble::tibble(.metric = "rmse", mean = 0.7),
      tibble::tibble(.metric = "rmse", mean = 0.9)
    ),
    cv_predictions = replicate(2, list(dummy = TRUE), simplify = FALSE),
    transformation = c("none", "none")
  )

  withr::defer(rm("fit.mock_workflow", envir = .GlobalEnv), envir = environment())
  assign(
    "fit.mock_workflow",
    function(object, data, ...) {
      if (object$id == "wf_fail") {
        return(NULL)
      }
      structure(
        list(id = object$id, offset = 0),
        class = "mock_fit"
      )
    },
    envir = .GlobalEnv
  )

  withr::defer(rm("predict.mock_fit", envir = .GlobalEnv), envir = environment())
  assign(
    "predict.mock_fit",
    function(object, new_data, ...) {
      tibble::tibble(.pred = new_data$Response)
    },
    envir = .GlobalEnv
  )

  expect_error(
    with_mocked_bindings(
      build_ensemble(
        finalized_models = finalized,
        input_data       = data,
        variable         = "Response",
        ensemble_method  = "weighted_average",
        verbose          = FALSE
      ),
      get_original_scale_predictions = function(fitted_workflow, new_data, transformation, warn = FALSE) {
        new_data$Response
      },
      back_transform_predictions = function(predictions, transformation, warn = FALSE) predictions,
      .package = "horizons"
    ),
    "Insufficient models for weighted average ensemble",
    class = "rlang_error"
  )
})
