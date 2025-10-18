#' Failure-path tests for evaluate_configuration() via targeted mocks

library(testthat)
library(horizons)

mk_cfg <- function() data.frame(
  model = "plsr",
  transformation = "none",
  preprocessing = "raw",
  feature_selection = "none",
  covariates = I(list(NULL)), stringsAsFactors = FALSE
)[1, , drop = FALSE]

mk_split <- function(n = 30) {
  d <- create_eval_test_data(n_samples = n)
  rsample::initial_split(d, prop = 0.8)
}

minimal_mocks <- function(input_data) list(
  workflows = list(
    workflow = function() list(),
    add_recipe = function(w, r) w,
    add_model = function(w, m) w,
    extract_parameter_set_dials = function(w) list()
  ),
  recipes = list(
    prep = function(...) list(prepped = TRUE),
    bake = function(prepped, new_data = NULL) input_data[1:10, ]
  ),
  dials = list(
    finalize = function(param_set, eval_data) list()
  )
)

with_pkg_mocks <- function(mocks, code) {
  with_mocked_bindings(
    workflow = mocks$workflows$workflow,
    add_recipe = mocks$workflows$add_recipe,
    add_model = mocks$workflows$add_model,
    extract_parameter_set_dials = mocks$workflows$extract_parameter_set_dials,
    .package = "workflows",
    with_mocked_bindings(
      prep = mocks$recipes$prep,
      bake = mocks$recipes$bake,
      .package = "recipes",
      with_mocked_bindings(
        finalize = mocks$dials$finalize,
        .package = "dials",
        code
      )
    )
  )
}

test_that("grid search failure returns failed with stage grid_tuning", {
  cfg <- mk_cfg(); input <- create_eval_test_data(40); split <- mk_split(40)
  mocks <- minimal_mocks(input)

  out <- with_pkg_mocks(mocks,
    with_mocked_bindings(
      tune_grid = function(...) stop("boom-grid"),
      .package = "tune",
      horizons:::evaluate_configuration(
        config_row = cfg,
        input_data = input,
        data_split = split,
        config_id = "cfg_grid_fail",
        variable = "Response",
        grid_size = 2,
        bayesian_iter = 0,
        cv_folds = 2,
        allow_par = FALSE,
        verbose = FALSE
      )
    )
  )

  expect_equal(out$status, "failed")
  expect_true(grepl("Grid tuning failed", out$error_message))
  expect_equal(out$error_stage, "grid_tuning")
})

test_that("select_best failure returns failed with stage parameter_selection", {
  cfg <- mk_cfg(); input <- create_eval_test_data(40); split <- mk_split(40)
  mocks <- minimal_mocks(input)

  out <- with_pkg_mocks(mocks,
    with_mocked_bindings(
      tune_grid = function(...) list(tag = "grid_ok"),
      collect_metrics = function(x) tibble::tibble(.metric = "rrmse", mean = 10),
      select_best = function(...) stop("boom-select"),
      .package = "tune",
      horizons:::evaluate_configuration(
        config_row = cfg,
        input_data = input,
        data_split = split,
        config_id = "cfg_select_fail",
        variable = "Response",
        grid_size = 2,
        bayesian_iter = 0,
        cv_folds = 2,
        allow_par = FALSE,
        verbose = FALSE
      )
    )
  )

  expect_equal(out$status, "failed")
  expect_true(grepl("Parameter selection failed", out$error_message))
  expect_equal(out$error_stage, "parameter_selection")
})

test_that("finalize_workflow failure returns failed with stage workflow_finalization", {
  cfg <- mk_cfg(); input <- create_eval_test_data(40); split <- mk_split(40)
  mocks <- minimal_mocks(input)

  out <- with_pkg_mocks(mocks,
    with_mocked_bindings(
      tune_grid = function(...) list(tag = "grid_ok"),
      collect_metrics = function(x) tibble::tibble(.metric = "rrmse", mean = 10),
      select_best = function(...) tibble::tibble(num_comp = 2),
      finalize_workflow = function(...) stop("boom-finalize"),
      .package = "tune",
      horizons:::evaluate_configuration(
        config_row = cfg,
        input_data = input,
        data_split = split,
        config_id = "cfg_finalize_fail",
        variable = "Response",
        grid_size = 2,
        bayesian_iter = 0,
        cv_folds = 2,
        allow_par = FALSE,
        verbose = FALSE
      )
    )
  )

  expect_equal(out$status, "failed")
  expect_true(grepl("Workflow finalization failed", out$error_message))
  expect_equal(out$error_stage, "workflow_finalization")
})

test_that("last_fit failure returns failed with stage final_fitting", {
  cfg <- mk_cfg(); input <- create_eval_test_data(40); split <- mk_split(40)
  mocks <- minimal_mocks(input)

  out <- with_pkg_mocks(mocks,
    with_mocked_bindings(
      tune_grid = function(...) list(tag = "grid_ok"),
      collect_metrics = function(x) tibble::tibble(.metric = "rrmse", mean = 10),
      select_best = function(...) tibble::tibble(num_comp = 2),
      finalize_workflow = function(...) list(),
      last_fit = function(...) stop("boom-lastfit"),
      .package = "tune",
      horizons:::evaluate_configuration(
        config_row = cfg,
        input_data = input,
        data_split = split,
        config_id = "cfg_lastfit_fail",
        variable = "Response",
        grid_size = 2,
        bayesian_iter = 0,
        cv_folds = 2,
        allow_par = FALSE,
        verbose = FALSE
      )
    )
  )

  expect_equal(out$status, "failed")
  expect_true(grepl("Final model fitting failed", out$error_message))
  expect_equal(out$error_stage, "final_fitting")
})

