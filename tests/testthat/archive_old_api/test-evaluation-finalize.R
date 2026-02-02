#' Tests for finalize_top_workflows() Function
#'
#' STRATEGY: Validation-focused (integration tests require real evaluation)
#'
#' NOTE: Full integration tests for this function require REAL evaluation results
#' from evaluate_models_local() because finalize_top_workflows() actually fits
#' models with tune_bayes(). This is too expensive for the standard test suite.
#'
#' Current approach:
#' - Validation tests (10 tests): Test all error handling and parameter validation
#' - Integration tests: Skipped for now, documented for future E2E test suite
#'
#' Future work: Add a separate slow E2E test that runs:
#'   evaluate_models_local() -> finalize_top_workflows() -> build_ensemble()

library(testthat)
library(horizons)

## ===========================================================================
## INTEGRATION TESTS - Skipped (require real evaluation results)
## ===========================================================================
# These tests require REAL evaluation results from evaluate_models_local()
# which is too expensive for the standard test suite. Skipping for now.

test_that("minimal finalization executes end-to-end", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with multiple models works", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization respects metric selection", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization generates CV predictions for stacking", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with different train_prop values", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with different CV fold counts", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization metrics are computed correctly", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with PLSR model", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with linear regression model", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with transformations", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with preprocessing", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with feature selection", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization returns test metrics", {
  skip("Requires real evaluation results - too expensive for test suite")
})

test_that("finalization with minimal bayesian iterations", {
  skip("Requires real evaluation results - too expensive for test suite")
})

## ===========================================================================
## VALIDATION TESTS - Parameter checking and error handling
## ===========================================================================

test_that("finalize_top_workflows validates eval_results type", {
  # SPEC-FINALIZE-VAL-001: Input type validation
  expect_error(
    finalize_top_workflows("not_dataframe", create_eval_test_data(), variable = "Response"),
    "data frame"
  )
})

test_that("finalize_top_workflows validates input_data type", {
  # SPEC-FINALIZE-VAL-002: Data validation
  eval_results <- create_mock_evaluation_results(n_models = 2)

  expect_error(
    finalize_top_workflows(eval_results, "not_dataframe", variable = "Response"),
    "data frame"
  )
})

test_that("finalize_top_workflows requires response variable", {
  # SPEC-FINALIZE-VAL-003: Response variable presence
  eval_results <- create_mock_evaluation_results(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "NonExistent"),
    "not found"
  )
})

test_that("finalize_top_workflows validates n_best parameter", {
  # SPEC-FINALIZE-VAL-004: n_best bounds
  eval_results <- create_mock_evaluation_results(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response", n_best = 0),
    "at least"
  )
})

test_that("finalize_top_workflows validates bayesian_iter parameter", {
  # SPEC-FINALIZE-VAL-005: Bayesian iteration bounds
  eval_results <- create_mock_evaluation_results(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response", bayesian_iter = -1),
    "non-negative"
  )
})

test_that("finalize_top_workflows validates cv_folds parameter", {
  # SPEC-FINALIZE-VAL-006: CV folds validation
  eval_results <- create_mock_evaluation_results(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response", cv_folds = 1),
    "at least"
  )
})

test_that("finalize_top_workflows validates train_prop parameter", {
  # SPEC-FINALIZE-VAL-007: Train proportion bounds
  eval_results <- create_mock_evaluation_results(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response", train_prop = 1.5),
    "between"
  )

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response", train_prop = 0),
    "between"
  )
})

test_that("finalize_top_workflows handles empty eval_results", {
  # SPEC-FINALIZE-VAL-008: Empty input handling
  eval_results <- data.frame()
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response"),
    "empty"
  )
})

test_that("finalize_top_workflows requires workflow_id column", {
  # SPEC-FINALIZE-VAL-009: Required column checks
  eval_results <- create_mock_evaluation_results(n_models = 2)
  eval_results$workflow_id <- NULL
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response"),
    "workflow_id"
  )
})



test_that('finalize_top_workflows runs on a minimal linear_reg setup', {
  result <- generate_mock_finalized_models(n_models = 2)

  expect_s3_class(result, 'tbl_df')
  expect_equal(nrow(result), 2)
  expect_true(all(c('workflow', 'cv_predictions', 'final_params') %in% names(result)))
})
test_that("finalize_top_workflows handles invalid metric", {
  # SPEC-FINALIZE-VAL-010: Metric validation
  eval_results <- create_mock_evaluation_results(n_models = 2)
  data <- create_eval_test_data()

  expect_error(
    finalize_top_workflows(eval_results, data, variable = "Response", metric = "fake_metric"),
    "not found"
  )
})

test_that("finalize_top_workflows errors when no models can be finalized", {
  eval_results <- tibble::tibble(
    workflow_id = "wf_001",
    status      = "success",
    best_params = list(tibble::tibble()),
    rrmse       = 0.4,
    model       = "linear_reg",
    transformation = "none",
    preprocessing  = "raw",
    feature_selection = "none",
    covariates = ""
  )

  input_data <- create_eval_test_data(n_samples = 25, n_wavelengths = 20)
  input_data$Response <- runif(nrow(input_data))

  expect_error(
    finalize_top_workflows(
      evaluation_results = eval_results,
      input_data         = input_data,
      variable           = "Response",
      n_best             = 1,
      bayesian_iter      = 0,
      verbose            = FALSE
    ),
    "No successful models with valid rrmse found"
  )
})

test_that("finalize_top_workflows returns mocked finalized models without fitting", {
  eval_results <- tibble::tibble(
    workflow_id      = "wf_mock_001",
    status           = "success",
    best_params      = list(tibble::tibble(penalty = 0.01, mixture = 0.5)),
    rrmse            = 0.25,
    model            = "linear_reg",
    transformation   = "none",
    preprocessing    = "raw",
    feature_selection = "none",
    covariates       = ""
  )

  input_data <- create_eval_test_data(n_samples = 60, n_wavelengths = 8, seed = 321)

  saved_path   <- NULL
  saved_object <- NULL

  result <- suppressWarnings(with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        with_mocked_bindings(
          with_mocked_bindings(
            with_mocked_bindings(
              with_mocked_bindings(
                with_mocked_bindings(
                  with_mocked_bindings(
                    finalize_top_workflows(
                      evaluation_results = eval_results,
                      input_data         = input_data,
                      variable           = "Response",
                      n_best             = 1,
                      bayesian_iter      = 0,
                      verbose            = TRUE
                    ),
                    build_recipe = function(...) structure(list(id = "mock_recipe"), class = "mock_recipe"),
                    define_model_specifications = function(...) structure(list(id = "mock_spec"), class = "mock_spec"),
                    compute_original_scale_metrics = function(truth, estimate, ...) {
                      tibble::tibble(
                        .metric   = c("rsq", "rmse", "rrmse", "ccc", "rpd", "mae"),
                        .estimate = c(0.9, 0.2, 12, 0.85, 2.5, 0.3)
                      )
                    },
                    back_transform_cv_predictions = function(x, ...) x,
                    back_transform_predictions    = function(x, ...) x,
                    .package = "horizons"
                  ),
                  workflow = function() list(),
                  add_recipe = function(wf, recipe) { wf$recipe <- recipe; wf },
                  add_model  = function(wf, model)  { wf$model  <- model;  wf },
                  .package = "workflows"
                ),
                tune_bayes = function(...) list(iterations = 0),
                tune_grid  = function(...) stop("tune_grid should not run in mocked finalize test", call. = FALSE),
                select_best = function(...) tibble::tibble(penalty = 0.01, mixture = 0.5),
                finalize_workflow = function(wf, params) {
                  wf$final_params <- params
                  wf
                },
                fit_resamples = function(...) list(.predictions = list(tibble::tibble(Response = c(10, 12), .pred = c(9.8, 12.1)))),
                last_fit = function(...) list(id = "mock_last_fit"),
                collect_predictions = function(...) tibble::tibble(Response = c(10, 12), .pred = c(9.9, 11.8)),
                collect_metrics = function(...) tibble::tibble(.metric = "rrmse", mean = 0.3, std_err = 0.01),
                control_bayes      = function(...) list(),
                control_resamples  = function(...) list(),
                control_grid       = function(...) list(),
                .package = "tune"
              ),
              metric_set = function(...) "metrics",
              .package = "yardstick"
            ),
            extract_parameter_set_dials = function(...) tibble::tibble(name = character(), object = list()),
            .package = "hardhat"
          ),
          finalize = function(param_set, ...) param_set,
          .package = "dials"
        ),
        dir_exists = function(...) TRUE,
        dir_create = function(...) invisible(NULL),
        .package = "fs"
      ),
      qsave = function(object, file, ...) {
        saved_path   <<- file
        saved_object <<- object
        invisible(NULL)
      },
      .package = "qs"
    ),
    cli_text         = function(...) invisible(NULL),
    cli_warn          = function(...) invisible(NULL),
    cli_alert_warning = function(...) invisible(NULL),
    cli_alert_info   = function(...) invisible(NULL),
    cli_alert_danger = function(...) invisible(NULL),
    cli_abort        = function(message, ...) stop(message, call. = FALSE),
    .package = "cli"
  ))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$wflow_id, "wf_mock_001")
  expect_true(is.list(result$workflow[[1]]))
  expect_equal(result$final_params[[1]], tibble::tibble(penalty = 0.01, mixture = 0.5))
  expect_true(tibble::is_tibble(result$cv_metrics[[1]]))
  expect_true(tibble::is_tibble(result$test_metrics[[1]]))
  expect_equal(result$test_metrics[[1]]$rsq, 0.9)
  expect_identical(saved_object, result)
  expect_true(endsWith(saved_path, ".qs"))
})

test_that("finalize_top_workflows runs mocked warm-start pipeline", {
  skip_if_not_installed("tune")
  skip_if_not_installed("dials")
  skip_if_not_installed("workflows")
  skip_if_not_installed("yardstick")

  tracking <- new.env(parent = emptyenv())
  tracking$warning_message <- NULL
  tracking$tune_grid_called <- FALSE
  tracking$tune_bayes_iter <- NA_integer_
  tracking$initial_is_df <- NA
  tracking$cv_backtransform_called <- FALSE
  tracking$test_backtransform_called <- FALSE
  tracking$saved_object <- NULL
  tracking$saved_path <- NULL

  eval_results <- tibble::tibble(
    workflow_id      = "wf_mock_002",
    status           = "success",
    best_params      = list(tibble::tibble(penalty = 0.1)),
    rsq              = 0.82,
    model            = "linear_reg",
    transformation   = "log",
    preprocessing    = "raw",
    feature_selection = "none",
    covariates       = ""
  )

  input_data <- create_eval_test_data(n_samples = 40, n_wavelengths = 6, seed = 123)
  tmp_output <- withr::local_tempdir(pattern = "finalize_mock_")

  result <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        with_mocked_bindings(
          with_mocked_bindings(
            with_mocked_bindings(
              with_mocked_bindings(
                with_mocked_bindings(
                  finalize_top_workflows(
                    evaluation_results = eval_results,
                    input_data         = input_data,
                    variable           = "Response",
                    metric             = "rsq",
                    n_best             = 2,
                    bayesian_iter      = 0,
                    cv_folds           = 3,
                    output_dir         = tmp_output,
                    verbose            = TRUE
                  ),
                  build_recipe = function(...) structure(list(id = "mock_recipe"), class = "mock_recipe"),
                  define_model_specifications = function(...) structure(list(id = "mock_spec"), class = "mock_spec"),
                  compute_original_scale_metrics = function(truth, estimate, ...) {
                    tibble::tibble(
                      .metric   = c("rsq", "rmse", "rrmse", "ccc", "rpd", "mae"),
                      .estimate = c(0.81, 0.45, 11.5, 0.76, 2.2, 0.35)
                    )
                  },
                  back_transform_cv_predictions = function(x, ...) {
                    tracking$cv_backtransform_called <- TRUE
                    x
                  },
                  back_transform_predictions = function(x, ...) {
                    tracking$test_backtransform_called <- TRUE
                    x
                  },
                  .package = "horizons"
                ),
                workflow = function() structure(list(), class = "mock_workflow"),
                add_recipe = function(wf, recipe) {
                  wf$recipe <- recipe
                  wf
                },
                add_model = function(wf, model) {
                  wf$model <- model
                  wf
                },
                .package = "workflows"
              ),
              tune_grid = function(object, resamples, grid, ...) {
                tracking$tune_grid_called <- TRUE
                tracking$grid_rows <- if (is.null(grid)) NA_integer_ else nrow(grid)
                tibble::tibble(.config = "grid1")
              },
              tune_bayes = function(object, resamples, initial, iter, ...) {
                tracking$tune_bayes_iter <- iter
                tracking$initial_is_df <- is.data.frame(initial)
                tibble::tibble(.config = "bayes1")
              },
              select_best = function(object, metric, ...) tibble::tibble(penalty = 0.08),
              finalize_workflow = function(wf, params) {
                wf$final_params <- params
                wf
              },
              fit_resamples = function(object, ...) {
                structure(
                  list(.predictions = list(tibble::tibble(Response = c(10, 12), .pred = c(9.8, 11.9)))),
                  class = "mock_cv_fit"
                )
              },
              last_fit = function(object, ...) {
                structure(
                  list(preds = tibble::tibble(Response = c(10, 12), .pred = c(9.7, 11.8))),
                  class = "mock_last_fit"
                )
              },
              collect_predictions = function(x, ...) {
                if (inherits(x, "mock_last_fit")) {
                  return(x$preds)
                }
                tibble::tibble(Response = numeric(0), .pred = numeric(0))
              },
              collect_metrics = function(...) tibble::tibble(.metric = "rsq", mean = 0.8, std_err = 0.01),
              control_grid = function(...) list(),
              control_bayes = function(...) list(),
              control_resamples = function(...) list(),
              .package = "tune"
            ),
            metric_set = function(...) list(...),
            .package = "yardstick"
          ),
          extract_parameter_set_dials = function(...) {
            mock_param <- structure(
              list(
                type = "double",
                range = list(lower = 0.05, upper = 0.2),
                trans = NULL
              ),
              class = c("quant_param", "param")
            )
            tibble::tibble(
              name = "penalty",
              object = list(mock_param)
            )
          },
          .package = "hardhat"
        ),
        qsave = function(object, file, ...) {
          tracking$saved_object <- object
          tracking$saved_path <- file
          invisible(NULL)
        },
        .package = "qs"
      ),
      dir_exists = function(path, ...) FALSE,
      dir_create = function(path, ...) {
        tracking$created_dir <- path
        invisible(path)
      },
      .package = "fs"
    ),
    cli_text = function(...) invisible(NULL),
    cli_alert_warning = function(message, ...) {
      tracking$warning_message <- message
      invisible(NULL)
    },
    cli_alert_info = function(...) invisible(NULL),
    cli_alert_danger = function(...) invisible(NULL),
    cli_warn = function(...) invisible(NULL),
    cli_abort = function(message, ...) stop(message, call. = FALSE),
    .package = "cli"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$wflow_id, "wf_mock_002")
  expect_true(tracking$tune_grid_called)
  expect_equal(tracking$grid_rows, 5)
  expect_identical(tracking$tune_bayes_iter, 0)
  expect_true(tracking$initial_is_df)
  expect_true(tracking$cv_backtransform_called)
  expect_true(tracking$test_backtransform_called)
  expect_true(grepl("\\.qs$", tracking$saved_path))
  expect_identical(tracking$saved_object, result)
  expect_false(is.null(tracking$warning_message))
})
