library(testthat)
library(horizons)
library(tibble)

test_that("create_failed_result builds fallback row", {
  config <- list(
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = c("clay", "sand")
  )

  failed <- horizons:::create_failed_result(
    config_id     = "cfg_001",
    config_clean  = config,
    error_message = "Failure",
    workflow_id   = "wf_001"
  )

  expect_s3_class(failed, "tbl_df")
  expect_equal(failed$config_id, "cfg_001")
  expect_equal(failed$model, "plsr")
  expect_equal(failed$status, "failed")
})

test_that("needs_back_transformation handles standard flags", {
  expect_false(horizons:::needs_back_transformation("none"))
  expect_false(horizons:::needs_back_transformation("notrans"))
  expect_false(horizons:::needs_back_transformation(""))
  expect_true(horizons:::needs_back_transformation("log"))
  expect_true(horizons:::needs_back_transformation("sqrt"))
})

test_that("evaluate_configuration validates config row structure", {
  skip_if_not_installed("rsample")

  base_data <- tibble::tibble(Sample_ID = c("S1", "S2"), Response = c(1, 2))
  dummy_split <- rsample::initial_split(base_data)

  failed <- horizons:::evaluate_configuration(
    config_row      = list(),
    input_data      = base_data,
    data_split      = dummy_split,
    config_id       = "cfg_bad",
    variable        = "Response",
    grid_size       = 1,
    bayesian_iter   = 0,
    cv_folds        = 2,
    allow_par       = FALSE,
    prune_models    = FALSE,
    prune_threshold = 0.9,
    verbose         = FALSE
  )

  expect_s3_class(failed, "tbl_df")
  expect_equal(failed$status, "failed")
  expect_match(failed$error_message, "config_row must be a single-row data frame")
})

test_that("evaluate_configuration validates data_split and variable inputs", {
  skip_if_not_installed("rsample")

  valid_config <- tibble::tibble(
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = list(NULL)
  )

  input_data <- tibble::tibble(Sample_ID = c("S1", "S2"), Response = c(1, 2))
  split <- rsample::initial_split(input_data, prop = 0.5)

  failed_split <- horizons:::evaluate_configuration(
    config_row      = valid_config,
    input_data      = input_data,
    data_split      = NULL,
    config_id       = "cfg_split",
    variable        = "Response",
    grid_size       = 1,
    bayesian_iter   = 0,
    cv_folds        = 2,
    allow_par       = FALSE,
    prune_models    = FALSE,
    prune_threshold = 0.9,
    verbose         = FALSE
  )

  expect_match(failed_split$error_message, "data_split must be an rsplit object")

  missing_var <- horizons:::evaluate_configuration(
    config_row      = valid_config,
    input_data      = tibble::tibble(Sample_ID = c("S1", "S2"), foo = c(1, 2)),
    data_split      = rsample::initial_split(tibble::tibble(Sample_ID = c("S1", "S2"), foo = c(1, 2))),
    config_id       = "cfg_var",
    variable        = "Response",
    grid_size       = 1,
    bayesian_iter   = 0,
    cv_folds        = 2,
    allow_par       = FALSE,
    prune_models    = FALSE,
    prune_threshold = 0.9,
    verbose         = FALSE
  )

  expect_match(missing_var$error_message, "variable 'Response' not found")
})

test_that("evaluate_configuration validates cv and grid parameters", {
  skip_if_not_installed("rsample")

  valid_config <- tibble::tibble(
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = list(NULL)
  )

  input_data <- tibble::tibble(Sample_ID = c("S1", "S2"), Response = c(1, 2))
  split <- rsample::initial_split(input_data, prop = 0.5)

  bad_cv <- horizons:::evaluate_configuration(
    config_row      = valid_config,
    input_data      = input_data,
    data_split      = split,
    config_id       = "cfg_cv",
    variable        = "Response",
    grid_size       = 1,
    bayesian_iter   = 0,
    cv_folds        = 1,
    allow_par       = FALSE,
    prune_models    = FALSE,
    prune_threshold = 0.9,
    verbose         = FALSE
  )

  expect_match(bad_cv$error_message, "cv_folds must be at least 2")

  bad_grid <- horizons:::evaluate_configuration(
    config_row      = valid_config,
    input_data      = input_data,
    data_split      = split,
    config_id       = "cfg_grid",
    variable        = "Response",
    grid_size       = 0,
    bayesian_iter   = 0,
    cv_folds        = 2,
    allow_par       = FALSE,
    prune_models    = FALSE,
    prune_threshold = 0.9,
    verbose         = FALSE
  )

  expect_match(bad_grid$error_message, "grid_size must be positive")
})
