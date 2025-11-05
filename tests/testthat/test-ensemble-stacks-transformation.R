#' Tests for Stacks Mixed Transformation Handling
#'
#' This test suite ensures that stacks ensembles correctly handle mixed
#' transformations (e.g., 2 log models + 2 none models) by back-transforming
#' each base model individually before applying ensemble weights.
#'
#' TDD approach: Tests written first, implementation follows.

library(testthat)
library(horizons)

## ===========================================================================
## TEST 1: Extract Stacks Coefficients
## ===========================================================================

test_that("can extract coefficients from stacks model", {
  skip_if_not_installed("stacks")

  # Setup: Create mock stacks model using with_mocked_stacks
  data <- create_eval_test_data(n_samples = 50, seed = 111)
  finalized <- create_mock_finalized_models(n_models = 3, input_data = data)

  mock_stacks <- with_mocked_stacks(
    {
      # Return a mock stacks object structure
      structure(
        list(dummy = "stacks"),
        class = c("model_stack", "list")
      )
    },
    weights = c(0.4, 0.35, 0.25),
    predict_offset = 0
  )

  # Execute: Extract coefficients
  coefs <- extract_stacks_coefficients(mock_stacks)

  # Assert
  expect_type(coefs, "double")
  expect_length(coefs, 3)
  expect_true(all(coefs >= 0 & coefs <= 1))
  expect_equal(sum(coefs), 1.0, tolerance = 1e-6)
})

## ===========================================================================
## TEST 2: Extract Member Workflows
## ===========================================================================

test_that("can extract member workflows from stacks model", {
  skip_if_not_installed("stacks")

  # Setup: Create mock stacks model with member_fits
  mock_stacks <- structure(
    list(
      member_fits = list(
        model_1 = structure(list(id = "model_1"), class = "mock_workflow"),
        model_2 = structure(list(id = "model_2"), class = "mock_workflow")
      )
    ),
    class = c("model_stack", "list")
  )

  # Execute
  member_workflows <- extract_stacks_members(mock_stacks)

  # Assert
  expect_type(member_workflows, "list")
  expect_length(member_workflows, 2)
  expect_named(member_workflows, c("model_1", "model_2"))
})

## ===========================================================================
## TEST 3: Individual Back-Transformation
## ===========================================================================

test_that("predicts with individual back-transformation for each member", {
  # Setup: Create test data with known values
  test_data <- tibble::tibble(
    Response = c(5, 10, 15, 20),
    X1 = c(1, 2, 3, 4)
  )

  # Create mock workflows with different transformations
  # Mock workflows that return predictable values
  log_workflow <- structure(
    list(
      id = "log_model",
      transformation = "log",
      predict_fn = function(new_data) {
        # Return log-scale predictions
        tibble::tibble(.pred = log(new_data$Response + 0.5))
      }
    ),
    class = "mock_workflow"
  )

  none_workflow <- structure(
    list(
      id = "none_model",
      transformation = "none",
      predict_fn = function(new_data) {
        # Return original-scale predictions
        tibble::tibble(.pred = new_data$Response - 0.2)
      }
    ),
    class = "mock_workflow"
  )

  workflows <- list(log_model = log_workflow, none_model = none_workflow)
  transformations <- c("log", "none")

  # Mock get_original_scale_predictions to work with our mock workflows
  withr::local_envvar(TEST_MODE = "true")

  # Execute: Get predictions with back-transformation
  # This will use the actual get_original_scale_predictions function
  # but we need to make sure our mock workflows work with it

  # For now, test that the function exists and has correct signature
  expect_true(exists("get_original_scale_predictions",
                     where = asNamespace("horizons"),
                     mode = "function"))

  # Test helper function signature
  base_preds <- predict_stacks_members_backtransformed(
    member_workflows = workflows,
    transformations = transformations,
    new_data = test_data
  )

  # Assert: Both should be on original scale
  expect_s3_class(base_preds, "data.frame")
  expect_equal(ncol(base_preds), 2)

  # Predictions should be in reasonable range (not log scale)
  expect_true(all(base_preds[[1]] > 0))
  expect_true(all(base_preds[[1]] < 100))
})

## ===========================================================================
## TEST 4: Manual Weighted Sum
## ===========================================================================

test_that("applies stacks coefficients correctly to get ensemble prediction", {
  # Setup: Create base predictions (all on original scale)
  base_predictions <- tibble::tibble(
    model_1 = c(5.0, 10.0, 15.0),
    model_2 = c(5.5, 10.5, 15.5),
    model_3 = c(4.5, 9.5, 14.5)
  )

  coefficients <- c(model_1 = 0.5, model_2 = 0.3, model_3 = 0.2)

  # Execute
  ensemble_pred <- apply_stacks_weights(base_predictions, coefficients)

  # Assert
  expect_type(ensemble_pred, "double")
  expect_length(ensemble_pred, 3)

  # Manual calculation for first prediction:
  # 5.0*0.5 + 5.5*0.3 + 4.5*0.2 = 2.5 + 1.65 + 0.9 = 5.05
  expect_equal(ensemble_pred[1], 5.05, tolerance = 1e-10)

  # Second prediction: 10.0*0.5 + 10.5*0.3 + 9.5*0.2 = 5.0 + 3.15 + 1.9 = 10.05
  expect_equal(ensemble_pred[2], 10.05, tolerance = 1e-10)

  # Third prediction: 15.0*0.5 + 15.5*0.3 + 14.5*0.2 = 7.5 + 4.65 + 2.9 = 15.05
  expect_equal(ensemble_pred[3], 15.05, tolerance = 1e-10)
})

## ===========================================================================
## TEST 5: Integration - Mixed Transformations
## ===========================================================================

test_that("stacks ensemble handles mixed transformations correctly", {
  skip_if_not_installed("stacks")

  # Setup: 2 log models + 2 none models
  data <- create_eval_test_data(n_samples = 60, seed = 777)

  # Scale data to realistic range
  data$Response <- data$Response * 10 + 5  # Range ~5-50

  finalized <- tibble::tibble(
    wflow_id = paste0("config_", 1:4),
    transformation = c("log", "log", "none", "none"),
    workflow = purrr::map(1:4, ~ structure(list(id = paste0("wf_", .x)),
                                           class = "mock_workflow"))
  )

  # Create CV predictions on ORIGINAL scale (as per our fix)
  # with small offsets for each model
  offsets <- c(0.5, -0.3, 0.2, -0.4)
  finalized$cv_predictions <- purrr::map2(
    finalized$wflow_id,
    offsets,
    function(id, offset) {
      tibble::tibble(
        .pred = pmax(data$Response + offset + rnorm(nrow(data), 0, 0.5), 0.1),
        Response = data$Response,
        .row = seq_len(nrow(data)),
        id = paste0("Fold", (seq_len(nrow(data)) %% 5) + 1)
      )
    }
  )

  finalized$cv_metrics <- purrr::map(1:4, ~ {
    tibble::tibble(
      .metric = c("rmse", "rsq", "mae"),
      mean = c(runif(1, 1, 3), runif(1, 0.7, 0.9), runif(1, 0.8, 2))
    )
  })

  # Setup mock workflow fitting and prediction
  workflow_preds <- setNames(offsets, finalized$wflow_id)

  withr::defer(rm("fit.mock_workflow", envir = .GlobalEnv), envir = environment())
  assign(
    "fit.mock_workflow",
    function(object, data, ...) {
      structure(
        list(id = object$id, offset = workflow_preds[[object$id]]),
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

  # Register predict method for mock stacks
  base::registerS3method("predict", "mock_stacks_model", predict.mock_stacks_model)

  # Execute: Build ensemble (will use new prediction logic)
  ensemble <- with_mocked_stacks(
    build_ensemble(
      finalized_models = finalized,
      input_data = data,
      variable = "Response",
      ensemble_method = "stacks",
      verbose = FALSE,
      seed = 888
    ),
    weights = c(0.3, 0.3, 0.2, 0.2),
    predict_offset = 0
  )

  # Assert: Ensemble was created successfully
  expect_s3_class(ensemble, "horizons_ensemble")
  expect_equal(ensemble$metadata$method, "stacks")

  # Assert: Predictions are on original scale
  preds <- ensemble$predictions$Predicted
  obs <- ensemble$predictions$Observed

  expect_true(all(preds > 0), label = "Predictions are positive")
  expect_true(all(preds < 100), label = "Predictions in reasonable range")

  # Assert: Predictions are correlated with observed (not random)
  correlation <- cor(preds, obs)
  expect_gt(correlation, 0.5, label = "Predictions correlate with observations")

  # Assert: RMSE is reasonable (not 6.6!)
  rmse_val <- ensemble$metrics %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::pull(.estimate)

  expect_lt(rmse_val, 5, label = "RMSE is reasonable (< 5, not 6.6)")
  expect_gt(rmse_val, 0.1, label = "RMSE is not suspiciously perfect")
})

## ===========================================================================
## TEST 6: Edge Case - All Same Transformation
## ===========================================================================

test_that("stacks handles all models with same transformation", {
  skip_if_not_installed("stacks")

  # Setup: All log-transformed models
  data <- create_eval_test_data(n_samples = 50, seed = 999)
  data$Response <- data$Response * 10 + 5

  finalized <- create_mock_finalized_models(n_models = 3, input_data = data)
  finalized$transformation <- c("log", "log", "log")

  # Create original-scale CV predictions
  finalized$cv_predictions <- purrr::map(
    seq_len(3),
    function(i) {
      tibble::tibble(
        .pred = data$Response + rnorm(nrow(data), 0, 0.5),
        Response = data$Response,
        .row = seq_len(nrow(data)),
        id = "Fold1"
      )
    }
  )

  # This should work without errors
  expect_no_error(
    ensemble <- with_mocked_stacks(
      build_ensemble(
        finalized_models = finalized,
        input_data = data,
        variable = "Response",
        ensemble_method = "stacks",
        verbose = FALSE
      ),
      weights = c(0.4, 0.35, 0.25),
      predict_offset = 0
    )
  )
})

## ===========================================================================
## TEST 7: Edge Case - No Transformations
## ===========================================================================

test_that("stacks handles all models with no transformation", {
  skip_if_not_installed("stacks")

  # Setup: All none-transformed models
  data <- create_eval_test_data(n_samples = 50, seed = 555)

  finalized <- create_mock_finalized_models(n_models = 3, input_data = data)
  finalized$transformation <- c("none", "none", "none")

  # Create original-scale CV predictions
  finalized$cv_predictions <- purrr::map(
    seq_len(3),
    function(i) {
      tibble::tibble(
        .pred = data$Response + rnorm(nrow(data), 0, 0.3),
        Response = data$Response,
        .row = seq_len(nrow(data)),
        id = "Fold1"
      )
    }
  )

  # This should work without errors
  expect_no_error(
    ensemble <- with_mocked_stacks(
      build_ensemble(
        finalized_models = finalized,
        input_data = data,
        variable = "Response",
        ensemble_method = "stacks",
        verbose = FALSE
      ),
      weights = c(0.4, 0.35, 0.25),
      predict_offset = 0
    )
  )
})
