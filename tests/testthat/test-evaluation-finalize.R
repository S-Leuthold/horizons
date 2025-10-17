#' Tests for finalize_top_workflows() Function

library(testthat)
library(horizons)

test_that("finalize_top_workflows validates eval_results type", {
  expect_error(
    finalize_top_workflows("not_dataframe", create_eval_test_data(), "Response"),
    "data.frame|tibble"
  )
})

test_that("finalize_top_workflows validates input_data type", {
  skip("Need valid eval_results fixture - create minimal")
})

test_that("finalize_top_workflows requires response variable", {
  skip("Need valid eval_results fixture")
})

test_that("finalize_top_workflows validates n_top parameter", {
  skip("Need valid eval_results fixture")
})

test_that("finalize_top_workflows validates bayesian_iter parameter", {
  skip("Need valid eval_results fixture")
})
