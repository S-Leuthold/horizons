#' Tests for cluster_spectral_predictors()

library(testthat)
library(horizons)
library(tibble)

test_that("cluster_spectral_predictors rejects non-numeric columns", {
  spectra <- tibble(
    `600` = c(0.1, 0.2, 0.3),
    `650` = c("a", "b", "c")
  )

  expect_error(
    cluster_spectral_predictors(spectra, k = 2),
    "must be numeric"
  )
})

test_that("cluster_spectral_predictors returns original matrix when k exceeds columns", {
  spectra <- tibble(
    `600` = c(0.1, 0.2, 0.3),
    `650` = c(0.2, 0.3, 0.4)
  )

  result <- cluster_spectral_predictors(spectra, k = 5)

  expect_named(result, c("reduced_mat", "cluster_map", "selected_vars"))
  expect_equal(result$reduced_mat, as.data.frame(spectra))
  expect_equal(result$selected_vars, colnames(spectra))
})

test_that("cluster_spectral_predictors clusters using correlation distance", {
  spectra <- tibble(
    `600` = c(0.1, 0.2, 0.3, 0.4),
    `650` = c(0.12, 0.22, 0.32, 0.42),
    `700` = c(1.0, 0.9, 0.8, 0.7)
  )

  result <- cluster_spectral_predictors(spectra, k = 2, method = "correlation")

  expect_equal(length(result$selected_vars), 2)
  expect_equal(ncol(result$reduced_mat), 2)
  expect_true(all(result$selected_vars %in% colnames(spectra)))

  # All original columns should appear in cluster_map values
  expect_equal(
    sort(unname(unlist(result$cluster_map))),
    sort(colnames(spectra))
  )
})

test_that("cluster_spectral_predictors clusters using euclidean distance", {
  spectra <- tibble(
    `600` = c(0.1, 0.15, 0.2),
    `650` = c(0.5, 0.45, 0.4),
    `700` = c(1.0, 1.1, 1.2)
  )

  result <- cluster_spectral_predictors(spectra, k = 2, method = "euclidean")

  expect_equal(length(result$selected_vars), 2)
  expect_equal(ncol(result$reduced_mat), 2)
  expect_true(all(result$selected_vars %in% colnames(spectra)))
})
