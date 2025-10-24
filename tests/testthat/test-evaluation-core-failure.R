#' Failure-path tests for evaluate_configuration() using targeted mocks

library(testthat)
library(horizons)

config_row <- data.frame(
  model             = "plsr",
  transformation    = "none",
  preprocessing     = "raw",
  feature_selection = "none",
  covariates        = I(list(NULL)),
  stringsAsFactors  = FALSE
)[1, , drop = FALSE]

make_data_and_split <- function(seed = 101) {
  set.seed(seed)
  data <- create_eval_test_data(n_samples = 40)
  split <- rsample::initial_split(data, prop = 0.8)
  list(data = data, split = split)
}

run_eval <- function(cfg_id, data, split) {
  suppressMessages(
    horizons:::evaluate_configuration(
      config_row    = config_row,
      input_data    = data,
      data_split    = split,
      config_id     = cfg_id,
      variable      = "Response",
      grid_size     = 1,
      bayesian_iter = 0,
      cv_folds      = 2,
      allow_par     = FALSE,
      prune_models  = FALSE,
      verbose       = FALSE
    )
  )
}

test_that("grid search failure returns grid_tuning stage", {
  env <- make_data_and_split(111)

  result <- with_mocked_bindings(
    tune_grid = function(...) stop("boom-grid"),
    .package   = "tune",
    run_eval("cfg_grid_fail", env$data, env$split)
  )

  expect_equal(result$status, "failed")
  expect_equal(result$error_stage, "grid_tuning")
  expect_match(result$error_message, "Grid tuning failed")
})

test_that("select_best failure surfaces parameter_selection stage", {
  env <- make_data_and_split(222)

  result <- with_mocked_bindings(
    select_best = function(...) stop("boom-select"),
    .package     = "tune",
    run_eval("cfg_select_fail", env$data, env$split)
  )

  expect_equal(result$status, "failed")
  expect_equal(result$error_stage, "parameter_selection")
  expect_match(result$error_message, "Parameter selection failed")
})

test_that("finalize_workflow failure is caught as workflow_finalization", {
  env <- make_data_and_split(333)

  result <- with_mocked_bindings(
    finalize_workflow = function(...) stop("boom-finalize"),
    .package          = "tune",
    run_eval("cfg_finalize_fail", env$data, env$split)
  )

  expect_equal(result$status, "failed")
  expect_equal(result$error_stage, "workflow_finalization")
  expect_match(result$error_message, "Workflow finalization failed")
})

test_that("last_fit failure returns final_fitting stage", {
  env <- make_data_and_split(444)

  result <- with_mocked_bindings(
    last_fit = function(...) stop("boom-lastfit"),
    .package = "tune",
    run_eval("cfg_lastfit_fail", env$data, env$split)
  )

  expect_equal(result$status, "failed")
  expect_equal(result$error_stage, "final_fitting")
  expect_match(result$error_message, "Final model fitting failed")
})
