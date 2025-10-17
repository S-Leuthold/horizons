#' Tests for build_ensemble() Function

library(testthat)
library(horizons)

test_that("build_ensemble validates finalized_models type", {
  skip("Validation differs from expected - needs fixture investigation")

  expect_error(
    build_ensemble("not_dataframe", create_eval_test_data(), "Response"),
    "data.frame|tibble|list"
  )
})

test_that("build_ensemble validates input_data type", {
  skip("Need finalized models fixture")
})

test_that("build_ensemble validates ensemble_method", {
  skip("Need finalized models fixture")
})

test_that("build_ensemble validates test_prop parameter", {
  skip("Need finalized models fixture")
})
