#' Validation Tests for evaluate_configuration() in evaluation-core
#'
#' Covers input validation and constant checks without running heavy tuning.

library(testthat)
library(horizons)

make_valid_config_row <- function() {
  data.frame(
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = I(list(NULL)),
    stringsAsFactors = FALSE
  )[1, , drop = FALSE]
}

make_valid_split <- function() {
  data <- create_eval_test_data(n_samples = 30)
  rsample::initial_split(data, prop = 0.8)
}

test_that("config_row must be single-row data frame", {
  input_data <- create_eval_test_data(n_samples = 30)
  split <- make_valid_split()

  bad_config <- data.frame(a = 1:2)

  res <- horizons:::evaluate_configuration(
    config_row = bad_config,
    input_data = input_data,
    data_split = split,
    config_id = "cfg_001",
    variable = "Response",
    cv_folds = 2,
    grid_size = 2,
    bayesian_iter = 0,
    allow_par = FALSE,
    verbose = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(res$status, "failed")
  expect_true(grepl("config_row", res$error_message))
})

test_that("data_split must be rsplit", {
  input_data <- create_eval_test_data(n_samples = 30)
  config_row <- make_valid_config_row()

  res <- horizons:::evaluate_configuration(
    config_row = config_row,
    input_data = input_data,
    data_split = list(),  # Not an rsplit
    config_id = "cfg_002",
    variable = "Response",
    cv_folds = 2,
    grid_size = 2,
    bayesian_iter = 0,
    allow_par = FALSE,
    verbose = FALSE
  )

  expect_equal(res$status, "failed")
  expect_true(grepl("rsplit", res$error_message))
})

test_that("cv_folds must be at least 2", {
  input_data <- create_eval_test_data(n_samples = 30)
  split <- make_valid_split()
  config_row <- make_valid_config_row()

  res <- horizons:::evaluate_configuration(
    config_row = config_row,
    input_data = input_data,
    data_split = split,
    config_id = "cfg_003",
    variable = "Response",
    cv_folds = 1,
    grid_size = 2,
    bayesian_iter = 0,
    allow_par = FALSE,
    verbose = FALSE
  )

  expect_equal(res$status, "failed")
  expect_true(grepl("cv_folds", res$error_message))
})

test_that("grid_size must be positive", {
  input_data <- create_eval_test_data(n_samples = 30)
  split <- make_valid_split()
  config_row <- make_valid_config_row()

  res <- horizons:::evaluate_configuration(
    config_row = config_row,
    input_data = input_data,
    data_split = split,
    config_id = "cfg_004",
    variable = "Response",
    cv_folds = 2,
    grid_size = 0,
    bayesian_iter = 0,
    allow_par = FALSE,
    verbose = FALSE
  )

  expect_equal(res$status, "failed")
  expect_true(grepl("grid_size", res$error_message))
})

test_that("bayesian_iter must be non-negative", {
  input_data <- create_eval_test_data(n_samples = 30)
  split <- make_valid_split()
  config_row <- make_valid_config_row()

  res <- horizons:::evaluate_configuration(
    config_row = config_row,
    input_data = input_data,
    data_split = split,
    config_id = "cfg_005",
    variable = "Response",
    cv_folds = 2,
    grid_size = 2,
    bayesian_iter = -1,
    allow_par = FALSE,
    verbose = FALSE
  )

  expect_equal(res$status, "failed")
  expect_true(grepl("bayesian_iter", res$error_message))
})

test_that("prune_threshold must be between 0 and 1", {
  input_data <- create_eval_test_data(n_samples = 30)
  split <- make_valid_split()
  config_row <- make_valid_config_row()

  res <- horizons:::evaluate_configuration(
    config_row = config_row,
    input_data = input_data,
    data_split = split,
    config_id = "cfg_006",
    variable = "Response",
    cv_folds = 2,
    grid_size = 2,
    bayesian_iter = 0,
    prune_threshold = 1.5,
    allow_par = FALSE,
    verbose = FALSE
  )

  expect_equal(res$status, "failed")
  expect_true(grepl("prune_threshold", res$error_message))
})

test_that("n_cv_cores must be positive when specified", {
  input_data <- create_eval_test_data(n_samples = 30)
  split <- make_valid_split()
  config_row <- make_valid_config_row()

  res <- horizons:::evaluate_configuration(
    config_row = config_row,
    input_data = input_data,
    data_split = split,
    config_id = "cfg_007",
    variable = "Response",
    cv_folds = 2,
    grid_size = 2,
    bayesian_iter = 0,
    n_cv_cores = 0,
    allow_par = FALSE,
    verbose = FALSE
  )

  expect_equal(res$status, "failed")
  expect_true(grepl("n_cv_cores", res$error_message))
})

test_that("invalid model/transformation/preprocessing/feature_selection are rejected", {
  input_data <- create_eval_test_data(n_samples = 30)
  split <- make_valid_split()

  # Invalid model
  cfg <- make_valid_config_row()
  cfg$model <- "invalid_model"
  res1 <- horizons:::evaluate_configuration(cfg, input_data, split, "cfg_008", variable = "Response", cv_folds = 2, grid_size = 2, bayesian_iter = 0, allow_par = FALSE, verbose = FALSE)
  expect_equal(res1$status, "failed")
  expect_true(grepl("Invalid model", res1$error_message))

  # Invalid transformation
  cfg <- make_valid_config_row()
  cfg$transformation <- "BogusTrans"
  res2 <- horizons:::evaluate_configuration(cfg, input_data, split, "cfg_009", variable = "Response", cv_folds = 2, grid_size = 2, bayesian_iter = 0, allow_par = FALSE, verbose = FALSE)
  expect_equal(res2$status, "failed")
  expect_true(grepl("Invalid transformation", res2$error_message))

  # Invalid preprocessing
  cfg <- make_valid_config_row()
  cfg$preprocessing <- "not_a_method"
  res3 <- horizons:::evaluate_configuration(cfg, input_data, split, "cfg_010", variable = "Response", cv_folds = 2, grid_size = 2, bayesian_iter = 0, allow_par = FALSE, verbose = FALSE)
  expect_equal(res3$status, "failed")
  expect_true(grepl("Invalid preprocessing", res3$error_message))

  # Invalid feature selection
  cfg <- make_valid_config_row()
  cfg$feature_selection <- "not_a_selector"
  res4 <- horizons:::evaluate_configuration(cfg, input_data, split, "cfg_011", variable = "Response", cv_folds = 2, grid_size = 2, bayesian_iter = 0, allow_par = FALSE, verbose = FALSE)
  expect_equal(res4$status, "failed")
  expect_true(grepl("Invalid feature selection", res4$error_message))
})

