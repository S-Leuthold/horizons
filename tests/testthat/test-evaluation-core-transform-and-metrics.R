#' Transformation and metric fallback tests for evaluate_configuration()

library(testthat)
library(horizons)

make_config <- function(transformation) {
  data.frame(
    model             = "plsr",
    transformation    = transformation,
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = I(list(NULL)),
    stringsAsFactors  = FALSE
  )[1, , drop = FALSE]
}

run_eval <- function(config_row, seed = 512) {
  set.seed(seed)
  data <- create_eval_test_data(n_samples = 40)
  split <- rsample::initial_split(data, prop = 0.8)

  suppressMessages(
    horizons:::evaluate_configuration(
      config_row    = config_row,
      input_data    = data,
      data_split    = split,
      config_id     = paste0("cfg_", config_row$transformation),
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

test_that("log transformation runs and produces metrics on original scale", {
  config_row <- make_config("log")
  result <- run_eval(config_row, seed = 777)

  expect_equal(result$status, "success")
  expect_equal(result$transformation, "log")
  expect_true(all(c("rsq", "rmse", "rrmse", "mae") %in% names(result)))
  expect_true(is.numeric(result$rrmse))
  expect_false(is.na(result$workflow_id))
})

test_that("metric calculation failure falls back to NA metrics", {
  config_row <- make_config("none")

  result <- with_mocked_bindings(
    compute_original_scale_metrics = function(...) stop("metrics boom"),
    .package = "horizons",
    run_eval(config_row, seed = 888)
  )

  expect_equal(result$status, "success")
  metric_cols <- c("rrmse", "rmse", "rsq", "mae", "rpd", "ccc")
  expect_true(all(metric_cols %in% names(result)))
  expect_true(all(is.na(result[metric_cols])))
})
