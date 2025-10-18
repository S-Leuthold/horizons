#' finalize_dataset PCA failure path is graceful

library(testthat)
library(horizons)

test_that("PCA failure gracefully skips spectral outlier detection", {
  d <- create_eval_test_data(n_samples = 30)

  expect_output(
    testthat::with_mocked_bindings(
      prcomp = function(...) stop("Singular matrix"),
      {
        res <- finalize_dataset(
          dataset = d,
          response_variable = "Response",
          spectral_outlier_method = "mahalanobis",
          verbose = TRUE
        )
        expect_true("outlier_flag" %in% names(res))
      }
    ),
    regexp = "PCA computation failed|skipping spectral outlier detection",
    fixed = FALSE
  )
})

