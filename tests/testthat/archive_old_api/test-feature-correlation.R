library(testthat)
library(horizons)
library(tibble)

test_that("step_select_correlation prep/bake maintain numerical outputs", {
  skip_if_not_installed("recipes")

  wavelengths <- tibble(
    Response = seq(1, 3, length.out = 6),
    `600` = Response * 1.0,
    `602` = Response * 0.9,
    `604` = Response * 0.8,
    `606` = rev(Response),
    `608` = rev(Response) * 0.9,
    `610` = rnorm(6, 0, 0.1)
  )

  rec <- recipes::recipe(Response ~ ., data = wavelengths) %>%
    step_select_correlation(
      tidyselect::matches("^[0-9]+$"),
      outcome = "Response"
    )

  prepped <- recipes::prep(rec, training = wavelengths)
  baked   <- recipes::bake(prepped, new_data = wavelengths)

  spec_cols <- setdiff(names(baked), "Response")
  expect_equal(length(spec_cols), 6)
  expect_true(all(sapply(baked[, spec_cols], is.numeric)))
})

test_that("step_select_correlation retains all when no window exceeds threshold", {
  skip_if_not_installed("recipes")

  neutral <- tibble(
    Response = seq_len(6),
    `600` = rep(1, 6),
    `602` = rep(1, 6),
    `604` = rep(1, 6),
    `606` = rep(1, 6),
    `608` = rep(1, 6)
  )

  rec <- recipes::recipe(Response ~ ., data = neutral) %>%
    step_select_correlation(tidyselect::matches("^[0-9]+$"), outcome = "Response")

  prepped <- suppressWarnings(recipes::prep(rec, training = neutral))
  baked <- recipes::bake(prepped, new_data = neutral)
  expect_equal(setdiff(names(baked), "Response"), c("600", "602", "604", "606", "608"))
})

test_that("step_select_correlation validates numeric spectra", {
  skip_if_not_installed("recipes")

  bad_data <- tibble(
    Response = 1:4,
    `600` = c("a", "b", "c", "d"),
    `602` = 1:4,
    `604` = 1:4
  )

  rec <- recipes::recipe(Response ~ ., data = bad_data) %>%
    step_select_correlation(tidyselect::matches("^[0-9]+$"), outcome = "Response")

  expect_error(recipes::prep(rec, training = bad_data), "must be numeric")
})
