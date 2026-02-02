#' Behavior tests for evaluate_configuration() using real pipelines
#'
#' These tests exercise the primary success and pruning paths using
#' lightweight configurations that run quickly while still touching the
#' full orchestration stack.

library(testthat)
library(horizons)

make_config_row <- function(transformation = "none") {
  data.frame(
    model             = "plsr",
    transformation    = transformation,
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = I(list(NULL)),
    stringsAsFactors  = FALSE
  )[1, , drop = FALSE]
}

make_split <- function(data, prop = 0.8) {
  rsample::initial_split(data, prop = prop)
}

test_that("evaluate_configuration returns success summary for minimal run", {
  set.seed(123)

  config_row <- make_config_row("none")
  input_data <- create_eval_test_data(n_samples = 40)
  split <- make_split(input_data, prop = 0.8)

  result <- suppressMessages(
    horizons:::evaluate_configuration(
      config_row    = config_row,
      input_data    = input_data,
      data_split    = split,
      config_id     = "cfg_success",
      variable      = "Response",
      grid_size     = 1,
      bayesian_iter = 0,
      cv_folds      = 2,
      allow_par     = FALSE,
      prune_models  = FALSE,
      verbose       = FALSE
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$status, "success")
  expect_true(all(c("rsq", "rmse", "rrmse", "ccc", "rpd", "mae") %in% names(result)))
  expect_true(is.numeric(result$total_seconds))
  expect_false(is.na(result$workflow_id))
})

test_that("evaluate_configuration prunes models and skips Bayesian optimization", {
  set.seed(456)

  config_row <- make_config_row("none")
  input_data <- create_eval_test_data(n_samples = 40)
  split <- make_split(input_data, prop = 0.8)

  result <- suppressMessages(
    horizons:::evaluate_configuration(
      config_row      = config_row,
      input_data      = input_data,
      data_split      = split,
      config_id       = "cfg_pruned",
      variable        = "Response",
      grid_size       = 1,
      bayesian_iter   = 5,
      cv_folds        = 2,
      allow_par       = FALSE,
      prune_models    = TRUE,
      prune_threshold = 0.0001,  # force pruning
      verbose         = FALSE
    )
  )

  expect_equal(result$status, "pruned")
  expect_equal(result$bayes_seconds, 0)
  expect_true(result$grid_seconds > 0)
  expect_true(is.numeric(result$rrmse))
})
