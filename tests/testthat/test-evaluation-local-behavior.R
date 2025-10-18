#' Behavior Tests for evaluate_models_local(): resume, pruning, failures
#'
#' Focus: exercise high-value branches without modifying package code.

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## Resume behavior: skips existing results and proceeds without recomputation
## ---------------------------------------------------------------------------

test_that("evaluate_models_local resumes and skips completed configs", {
  config <- create_eval_test_config()
  data <- create_eval_test_data(n_samples = 30)

  od <- tempfile("eval_resume_")
  dir.create(od, recursive = TRUE)

  # First run creates checkpoint files
  invisible(
    evaluate_models_local(
      config = config,
      input_data = data,
      variable = "Response",
      grid_size = 2,
      bayesian_iter = 0,
      cv_folds = 2,
      allow_par = FALSE,
      output_dir = od,
      verbose = FALSE
    )
  )

  # Second run with resume=TRUE should report skipping
  expect_output(
    evaluate_models_local(
      config = config,
      input_data = data,
      variable = "Response",
      grid_size = 2,
      bayesian_iter = 0,
      cv_folds = 2,
      allow_par = FALSE,
      output_dir = od,
      resume = TRUE,
      verbose = TRUE
    ),
    regexp = "will skip|Skipping",
    fixed = FALSE
  )
})

## ---------------------------------------------------------------------------
## Pruned path: mock evaluate_configuration to return status = "pruned"
## ---------------------------------------------------------------------------

test_that("evaluate_models_local handles pruned result status", {
  config <- create_eval_test_config()
  data <- create_eval_test_data(n_samples = 30)

  pruned_result <- tibble::tibble(
    status = "pruned",
    rrmse = 999,
    rsq = NA_real_,
    ccc = NA_real_,
    rpd = NA_real_,
    mae = NA_real_,
    best_params = list(NULL),
    model = config$model,
    preprocessing = config$preprocessing,
    transformation = config$transformation,
    feature_selection = config$feature_selection,
    total_seconds = 0
  )

  res <- testthat::with_mocked_bindings(
    evaluate_configuration = function(...) pruned_result,
    {
      evaluate_models_local(
        config = config,
        input_data = data,
        variable = "Response",
        grid_size = 2,
        bayesian_iter = 0,
        cv_folds = 2,
        allow_par = FALSE,
        prune_models = TRUE,
        verbose = FALSE
      )
    },
    .package = "horizons"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$status, "pruned")
})

## ---------------------------------------------------------------------------
## Failure path: mock evaluate_configuration to return status = "failed"
## ---------------------------------------------------------------------------

test_that("evaluate_models_local records failure details from evaluate_configuration", {
  config <- create_eval_test_config()
  data <- create_eval_test_data(n_samples = 30)

  failed_result <- tibble::tibble(
    status = "failed",
    rrmse = NA_real_,
    rsq = NA_real_,
    ccc = NA_real_,
    rpd = NA_real_,
    mae = NA_real_,
    best_params = list(NULL),
    error_stage = "grid_search",
    error_class = "TestError",
    error_message = "Simulated failure at grid search",
    total_seconds = 0
  )

  res <- testthat::with_mocked_bindings(
    evaluate_configuration = function(...) failed_result,
    {
      evaluate_models_local(
        config = config,
        input_data = data,
        variable = "Response",
        grid_size = 2,
        bayesian_iter = 0,
        cv_folds = 2,
        allow_par = FALSE,
        verbose = FALSE
      )
    },
    .package = "horizons"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$status, "failed")
  expect_true("error_message" %in% names(res))
})

