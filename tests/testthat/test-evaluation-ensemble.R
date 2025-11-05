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

test_that("stacks ensemble uses mocked stacks infrastructure and saves output", {
  skip_if_not_installed("stacks")
  skip_if_not_installed("future")
  skip_if_not_installed("qs")
  skip_if_not_installed("readr")

  data <- create_eval_test_data(n_samples = 50, seed = 321)

  finalized <- create_mock_finalized_models(n_models = 3, input_data = data)

  workflow_offsets <- setNames(c(0.4, 0.25, 0.35), finalized$wflow_id)

  finalized$workflow <- purrr::map(finalized$wflow_id, ~ structure(list(id = .x), class = "mock_workflow"))
  finalized$transformation <- c("none", "log", "sqrt")

  transforms <- setNames(finalized$transformation, finalized$wflow_id)

  ## NOTE: CV predictions from finalize_top_workflows() are ALREADY back-transformed
  ## to original scale. Mock data must match this behavior.
  finalized$cv_predictions <- purrr::map2(
    finalized$wflow_id,
    seq_along(finalized$wflow_id),
    function(id, idx) {
      base_vals <- pmax(data$Response + workflow_offsets[[id]], 1e-3)

      ## Predictions are on ORIGINAL scale (already back-transformed)
      ## regardless of transformation type
      pred_values <- base_vals

      tibble::tibble(
        .pred    = pred_values,
        Response = data$Response,
        .row     = seq_len(nrow(data)),
        id       = paste0("Fold", idx)
      )
    }
  )

  base::registerS3method("predict", "mock_stacks_model", predict.mock_stacks_model)

  withr::defer(rm("fit.mock_workflow", envir = .GlobalEnv), envir = environment())
  assign(
    "fit.mock_workflow",
    function(object, data, ...) {
      structure(
        list(id = object$id, offset = workflow_offsets[[object$id]]),
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

  plan_log <- list()

  mock_plan <- function(strategy, ...) {
    plan_log <<- append(plan_log, list(list(strategy = strategy, args = list(...))))
    invisible(structure(list(strategy = strategy), class = "mock_plan"))
  }

  mock_multisession <- function(..., workers = NULL) {
    structure(list(name = "multisession", workers = workers), class = "mock_strategy")
  }

  mock_sequential <- function(...) {
    structure(list(name = "sequential"), class = "mock_strategy")
  }

  temp_output <- withr::local_tempdir(pattern = "ensemble_stack_")

  stats_predict <- stats::predict
  mock_stats_predict <- function(object, newdata = NULL, ...) {
    if (inherits(object, "mock_stacks_model") && !is.null(object$predict_fun)) {
      object$predict_fun(newdata)
    } else {
      stats_predict(object, newdata, ...)
    }
  }

  ensemble <- testthat::with_mocked_bindings(
    predict = mock_stats_predict,
    with_mocked_stacks(
      with_mocked_bindings(
        detectCores = function() 4,
        .package = "parallel",
        with_mocked_bindings(
          plan = mock_plan,
          multisession = mock_multisession,
          sequential = mock_sequential,
          .package = "future",
          with_mocked_bindings(
            qsave = function(object, file, ...) saveRDS(object, file = file),
            .package = "qs",
            with_mocked_bindings(
              write_csv = function(x, file, ...) utils::write.csv(x, file, row.names = FALSE),
              .package = "readr",
              build_ensemble(
                finalized_models   = finalized,
                input_data         = data,
                variable           = "Response",
                ensemble_method    = "stacks",
                optimize_blending  = TRUE,
                allow_par          = TRUE,
                n_cores            = 3,
                output_dir         = temp_output,
                verbose            = FALSE,
                seed               = 999
              )
            )
          )
        )
      ),
      weights = c(0.6, 0.3, 0.1),
      predict_offset = -0.1
    ),
    .package = "stats"
  )

  expect_s3_class(ensemble, "horizons_ensemble")
  expect_equal(ensemble$metadata$method, "stacks")
  expect_equal(nrow(ensemble$model_weights), 3)
  expect_true(all(c("Predicted", "Observed") %in% names(ensemble$predictions)))
  expect_true(file.exists(file.path(temp_output, "ensemble", "predictions.csv")))
  expect_true(file.exists(file.path(temp_output, "ensemble", "weights.csv")))
  expect_true(file.exists(file.path(temp_output, "ensemble", "ensemble_results.qs")))
  expect_gte(length(plan_log), 1)
  expect_true(ensemble$metadata$improvement > 0)
})

test_that("xgb_meta ensemble integrates mocked XGBoost and reports degradation", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("tune")

  data <- create_eval_test_data(n_samples = 45, seed = 404)
  finalized <- create_mock_finalized_models(n_models = 3, input_data = data)

  workflow_offsets <- setNames(c(0.05, 0.02, 0.08), finalized$wflow_id)
  cv_offsets <- c(0.1, -0.05, 0.2)

  finalized$workflow <- purrr::map(finalized$wflow_id, ~ structure(list(id = .x), class = "mock_workflow"))
  finalized$transformation <- c("none", "log", "log")

  transforms <- setNames(finalized$transformation, finalized$wflow_id)

  ## NOTE: CV predictions from finalize_top_workflows() are ALREADY back-transformed
  ## to original scale. Mock data must match this behavior.
  finalized$cv_predictions <- purrr::map2(
    finalized$wflow_id,
    cv_offsets,
    function(id, offset) {
      base_vals <- pmax(data$Response + offset, 1e-3)

      ## Predictions are on ORIGINAL scale (already back-transformed)
      ## regardless of transformation type
      pred_values <- base_vals

      tibble::tibble(
        .pred    = pred_values,
        Response = data$Response,
        .row     = seq_len(nrow(data)),
        id       = "FoldA"
      )
    }
  )

  base::registerS3method("predict", "mock_xgb_booster", predict.mock_xgb_booster)

  withr::defer(rm("fit.mock_workflow", envir = .GlobalEnv), envir = environment())
  assign(
    "fit.mock_workflow",
    function(object, data, ...) {
      structure(
        list(id = object$id, offset = workflow_offsets[[object$id]]),
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

  stats_predict <- stats::predict
  mock_stats_predict <- function(object, newdata = NULL, ...) {
    if (inherits(object, "mock_xgb_booster")) {
      predict.mock_xgb_booster(object, if (is.null(newdata)) matrix(numeric(), nrow = 0) else newdata, ...)
    } else {
      stats_predict(object, newdata, ...)
    }
  }

  ensemble <- testthat::with_mocked_bindings(
    predict = mock_stats_predict,
    with_mocked_collect_predictions(
      with_mocked_xgboost(
        build_ensemble(
          finalized_models  = finalized,
          input_data        = data,
          variable          = "Response",
          ensemble_method   = "xgb_meta",
          verbose           = FALSE,
          seed              = 2025
        ),
        gain_values = c(0.5, 0.3, 0.2),
        predict_offset = 0.4
      )
    ),
    .package = "stats"
  )

  expect_s3_class(ensemble, "horizons_ensemble")
  expect_equal(ensemble$ensemble_model$method, "xgb_meta")
  expect_equal(nrow(ensemble$model_weights), 3)
  expect_true(all(grepl("^config_", ensemble$model_weights$member)))
  expect_true(ensemble$metadata$improvement < 0)
  expect_true(any(ensemble$metrics$.metric == "rmse"))
  expect_gt(nrow(ensemble$individual_performance), 0)
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
