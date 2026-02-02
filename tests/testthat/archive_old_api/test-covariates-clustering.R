#' Tests for covariate clustering utilities

library(testthat)
library(horizons)
library(tibble)

## -----------------------------------------------------------------------------
## cluster_ossl_samples
## -----------------------------------------------------------------------------

test_that("cluster_ossl_samples returns single cluster when insufficient samples", {
  tiny_scores <- tibble(
    Dim.1 = c(0.1, 0.2, 0.3),
    Dim.2 = c(0.4, 0.5, 0.6)
  )

  result <- horizons:::cluster_ossl_samples(
    ossl_pca_scores = tiny_scores,
    max_clusters    = 5,
    verbose         = FALSE
  )

  expect_null(result$kmeans_model)
  expect_equal(result$n_clusters, 1)
  expect_equal(result$cluster_assignments, rep(1L, nrow(tiny_scores)))
  expect_null(result$silhouette_scores)
})

test_that("cluster_ossl_samples selects optimal k via silhouette analysis", {
  set.seed(123)

  clustered <- tibble(
    Dim.1 = c(rnorm(10, mean = -3), rnorm(10, mean = 3)),
    Dim.2 = c(rnorm(10, mean = -3), rnorm(10, mean = 3))
  )

  result <- horizons:::cluster_ossl_samples(
    ossl_pca_scores = clustered,
    max_clusters    = 4,
    verbose         = FALSE
  )

  expect_equal(result$n_clusters, 2)
  expect_length(result$cluster_assignments, nrow(clustered))
  expect_true(all(result$cluster_assignments %in% c(1, 2)))
  expect_length(result$silhouette_scores, 3)  # evaluated for k = 2:4
})

## -----------------------------------------------------------------------------
## assign_unknowns_to_clusters
## -----------------------------------------------------------------------------

test_that("assign_unknowns_to_clusters handles single-cluster models", {
  unknown <- tibble(Dim.1 = c(0.1, 0.2), Dim.2 = c(-0.1, -0.2))

  assignment <- horizons:::assign_unknowns_to_clusters(
    unknown_pca_scores = unknown,
    cluster_model      = list(n_clusters = 1, kmeans_model = NULL),
    verbose            = FALSE
  )

  expect_equal(assignment, c(1L, 1L))
})

test_that("assign_unknowns_to_clusters maps to nearest centroid", {
  unknown <- tibble(
    Dim.1 = c(-1, 5),
    Dim.2 = c(-1, 5)
  )

  cluster_model <- list(
    n_clusters  = 2,
    kmeans_model = list(
      centers = matrix(c(-1, -1, 5, 5), nrow = 2, byrow = TRUE,
                       dimnames = list(NULL, c("Dim.1", "Dim.2")))
    )
  )

  assignment <- horizons:::assign_unknowns_to_clusters(
    unknown_pca_scores = unknown,
    cluster_model      = cluster_model,
    verbose            = FALSE
  )

  expect_equal(assignment, c(1L, 2L))
})

## -----------------------------------------------------------------------------
## create_clustered_subsets
## -----------------------------------------------------------------------------

test_that("create_clustered_subsets splits data per cluster", {
  set.seed(42)

  ossl_scores <- tibble(
    Dim.1 = rnorm(12),
    Dim.2 = rnorm(12),
    clay  = runif(12, 5, 35)
  )

  cluster_assignments <- c(rep(1, 6), rep(2, 6))
  cluster_model <- list(
    n_clusters          = 2,
    cluster_assignments = cluster_assignments
  )

  subsets <- horizons:::create_clustered_subsets(
    ossl_pca_scores = ossl_scores,
    cluster_model   = cluster_model,
    prop            = 0.7,
    verbose         = FALSE
  )

  expect_named(subsets, c("Cluster_1", "Cluster_2"))

  total_rows <- sum(vapply(subsets, function(x) nrow(x$train) + nrow(x$val), numeric(1)))
  expect_equal(total_rows, nrow(ossl_scores))

  expect_true(all(vapply(subsets, function(x) nrow(x$train), numeric(1)) > 0))
  expect_true(all(vapply(subsets, function(x) nrow(x$val), numeric(1)) > 0))
})
