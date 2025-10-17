#' Tests for Covariate Clustering Functions
#'
#' Tests for cluster_ossl_samples(), assign_unknowns_to_clusters(),
#' and create_clustered_subsets() functions used in covariate prediction.

library(testthat)
library(horizons)

## ---------------------------------------------------------------------------
## Test Group 1: cluster_ossl_samples
## ---------------------------------------------------------------------------

test_that("cluster_ossl_samples works with valid PCA scores", {
  # Create mock PCA scores
  n_samples <- 100
  pca_scores <- data.frame(
    Sample_ID = paste0("OSSL_", 1:n_samples),
    Dim.1 = rnorm(n_samples),
    Dim.2 = rnorm(n_samples),
    Dim.3 = rnorm(n_samples),
    Dim.4 = rnorm(n_samples),
    Dim.5 = rnorm(n_samples)
  )

  result <- horizons:::cluster_ossl_samples(
    ossl_pca_scores = pca_scores,
    n_clusters = 3,
    seed = 123
  )

  # Check structure
  expect_type(result, "list")
  expect_true("cluster_assignments" %in% names(result))
  expect_true("cluster_centers" %in% names(result))
  expect_true("n_clusters" %in% names(result))

  # Check assignments
  expect_equal(length(result$cluster_assignments), n_samples)
  expect_true(all(result$cluster_assignments %in% 1:3))

  # Check number of clusters
  expect_equal(result$n_clusters, 3)
})

test_that("cluster_ossl_samples is reproducible with seed", {
  pca_scores <- data.frame(
    Sample_ID = paste0("S", 1:50),
    Dim.1 = rnorm(50),
    Dim.2 = rnorm(50),
    Dim.3 = rnorm(50)
  )

  result1 <- horizons:::cluster_ossl_samples(pca_scores, n_clusters = 3, seed = 999)
  result2 <- horizons:::cluster_ossl_samples(pca_scores, n_clusters = 3, seed = 999)

  # Should be identical
  expect_equal(result1$cluster_assignments, result2$cluster_assignments)
})

test_that("cluster_ossl_samples handles different cluster counts", {
  pca_scores <- data.frame(
    Sample_ID = paste0("S", 1:100),
    Dim.1 = rnorm(100),
    Dim.2 = rnorm(100)
  )

  result_2 <- horizons:::cluster_ossl_samples(pca_scores, n_clusters = 2)
  result_5 <- horizons:::cluster_ossl_samples(pca_scores, n_clusters = 5)

  expect_equal(result_2$n_clusters, 2)
  expect_equal(result_5$n_clusters, 5)

  expect_equal(length(unique(result_2$cluster_assignments)), 2)
  expect_equal(length(unique(result_5$cluster_assignments)), 5)
})

## ---------------------------------------------------------------------------
## Test Group 2: assign_unknowns_to_clusters
## ---------------------------------------------------------------------------

test_that("assign_unknowns_to_clusters assigns to nearest cluster", {
  # Create cluster centers
  cluster_centers <- matrix(c(
    0, 0,     # Cluster 1 center
    5, 5,     # Cluster 2 center
    -5, -5    # Cluster 3 center
  ), byrow = TRUE, ncol = 2)

  # Create unknown samples near each cluster
  unknown_pca <- data.frame(
    Sample_ID = paste0("UNK_", 1:9),
    Dim.1 = c(0.1, 0.2, 0.1,  # Near cluster 1
              5.1, 4.9, 5.2,  # Near cluster 2
              -4.9, -5.1, -5.0),  # Near cluster 3
    Dim.2 = c(0.1, -0.1, 0.2,
              5.2, 4.8, 5.1,
              -5.1, -4.9, -5.0)
  )

  result <- horizons:::assign_unknowns_to_clusters(
    unknown_pca_scores = unknown_pca,
    cluster_centers = cluster_centers
  )

  # Check assignments
  expect_equal(length(result), 9)
  expect_true(all(result %in% 1:3))

  # Samples 1-3 should be assigned to cluster 1
  expect_true(all(result[1:3] == 1))
  # Samples 4-6 should be assigned to cluster 2
  expect_true(all(result[4:6] == 2))
  # Samples 7-9 should be assigned to cluster 3
  expect_true(all(result[7:9] == 3))
})

test_that("assign_unknowns_to_clusters handles single unknown sample", {
  cluster_centers <- matrix(c(0, 0, 5, 5), byrow = TRUE, ncol = 2)

  unknown_single <- data.frame(
    Sample_ID = "UNK_1",
    Dim.1 = 0.1,
    Dim.2 = 0.1
  )

  result <- horizons:::assign_unknowns_to_clusters(unknown_single, cluster_centers)

  expect_equal(length(result), 1)
  expect_equal(result[1], 1)  # Should assign to cluster 1 (closer to 0,0)
})

## ---------------------------------------------------------------------------
## Test Group 3: create_clustered_subsets
## ---------------------------------------------------------------------------

test_that("create_clustered_subsets creates correct number of subsets", {
  # Create PCA scores with cluster assignments
  n_samples <- 100
  pca_scores <- data.frame(
    Sample_ID = paste0("OSSL_", 1:n_samples),
    Dim.1 = rnorm(n_samples),
    Dim.2 = rnorm(n_samples),
    cluster = sample(1:3, n_samples, replace = TRUE),
    clay = runif(n_samples, 100, 500)
  )

  result <- horizons:::create_clustered_subsets(
    ossl_pca_scores = pca_scores,
    cluster_assignments = pca_scores$cluster,
    unknown_cluster_assignments = c(1, 1, 2, 3),  # 2 in cluster 1, 1 in cluster 2, 1 in cluster 3
    property = "clay"
  )

  # Should create 3 subsets (one per cluster)
  expect_type(result, "list")
  expect_equal(length(result), 3)

  # Each subset should have data
  for (i in 1:3) {
    expect_s3_class(result[[i]], "data.frame")
    expect_true("Sample_ID" %in% names(result[[i]]))
    expect_true("clay" %in% names(result[[i]]))
  }
})

test_that("create_clustered_subsets preserves property values", {
  pca_scores <- data.frame(
    Sample_ID = paste0("S", 1:30),
    Dim.1 = rnorm(30),
    cluster = rep(1:3, each = 10),
    ph = seq(4, 9, length.out = 30)
  )

  result <- horizons:::create_clustered_subsets(
    ossl_pca_scores = pca_scores,
    cluster_assignments = pca_scores$cluster,
    unknown_cluster_assignments = c(1, 2, 3),
    property = "ph"
  )

  # Property values should be preserved
  cluster1_ph <- result[[1]]$ph
  expect_true(all(cluster1_ph >= 4 & cluster1_ph <= 6))  # First 10 samples
})

## ---------------------------------------------------------------------------
## Test Group 4: Integration Tests
## ---------------------------------------------------------------------------

test_that("clustering pipeline integrates correctly", {
  # Full pipeline: cluster → assign → subset
  n_ossl <- 150
  n_unknown <- 30

  ossl_pca <- data.frame(
    Sample_ID = paste0("OSSL_", 1:n_ossl),
    Dim.1 = rnorm(n_ossl),
    Dim.2 = rnorm(n_ossl),
    Dim.3 = rnorm(n_ossl),
    clay = runif(n_ossl, 100, 600)
  )

  unknown_pca <- data.frame(
    Sample_ID = paste0("UNK_", 1:n_unknown),
    Dim.1 = rnorm(n_unknown),
    Dim.2 = rnorm(n_unknown),
    Dim.3 = rnorm(n_unknown)
  )

  # Step 1: Cluster OSSL
  cluster_result <- horizons:::cluster_ossl_samples(ossl_pca, n_clusters = 4, seed = 456)

  # Step 2: Assign unknowns
  unknown_assignments <- horizons:::assign_unknowns_to_clusters(
    unknown_pca_scores = unknown_pca,
    cluster_centers = cluster_result$cluster_centers
  )

  expect_equal(length(unknown_assignments), n_unknown)
  expect_true(all(unknown_assignments %in% 1:4))

  # Step 3: Create subsets
  subsets <- horizons:::create_clustered_subsets(
    ossl_pca_scores = ossl_pca,
    cluster_assignments = cluster_result$cluster_assignments,
    unknown_cluster_assignments = unknown_assignments,
    property = "clay"
  )

  expect_equal(length(subsets), 4)
})

test_that("clustering handles edge cases", {
  # Very small dataset
  small_pca <- data.frame(
    Sample_ID = c("S1", "S2", "S3"),
    Dim.1 = c(0, 1, 2)
  )

  # Should handle gracefully
  result <- horizons:::cluster_ossl_samples(small_pca, n_clusters = 2)

  expect_type(result, "list")
  expect_equal(length(result$cluster_assignments), 3)
})
