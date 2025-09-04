#' Test Suite for Stratified Kennard-Stone Similarity Selection
#' 
#' Tests for global training set selection using clustering and Kennard-Stone algorithm.
#' Covers clustering, distance calculations, stratified sampling, and edge cases.

library(testthat)
library(horizons)
library(dplyr)
library(tibble)

# Helper function to create mock PCA scores
create_mock_pca_scores <- function(n_samples, n_components = 10, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  scores <- tibble(sample_id = paste0("S", 1:n_samples))
  
  for (i in 1:n_components) {
    # Decreasing variance for each component
    scores[[paste0("Dim.", i)]] <- rnorm(n_samples, 0, sd = 10 / sqrt(i))
  }
  
  scores
}

# Test Clustering Functions ---------------------------------------------------

test_that("cluster_unknown_samples identifies optimal clusters", {
  set.seed(123)
  
  # Create data with 3 natural clusters
  n_per_cluster <- 30
  
  cluster1 <- create_mock_pca_scores(n_per_cluster, seed = 1)
  cluster1[grep("^Dim\\.", names(cluster1))] <- 
    cluster1[grep("^Dim\\.", names(cluster1))] + 10
  
  cluster2 <- create_mock_pca_scores(n_per_cluster, seed = 2)
  
  cluster3 <- create_mock_pca_scores(n_per_cluster, seed = 3)
  cluster3[grep("^Dim\\.", names(cluster3))] <- 
    cluster3[grep("^Dim\\.", names(cluster3))] - 10
  
  unknown_pca <- bind_rows(cluster1, cluster2, cluster3)
  
  # Test clustering
  result <- cluster_unknown_samples(
    unknown_pca_scores = unknown_pca,
    max_clusters = 4,
    verbose = FALSE
  )
  
  expect_type(result, "list")
  expect_true("clusters" %in% names(result))
  expect_true("n_clusters" %in% names(result))
  expect_true("cluster_centers" %in% names(result))
  expect_true("silhouette_scores" %in% names(result))
  
  # Should identify approximately 3 clusters
  expect_true(result$n_clusters >= 2 && result$n_clusters <= 4)
  
  # Check cluster assignments
  expect_equal(length(result$clusters), nrow(unknown_pca))
  expect_true(all(result$clusters %in% 1:result$n_clusters))
})

test_that("cluster_unknown_samples handles single cluster case", {
  # Create homogeneous data (should result in 1 cluster)
  unknown_pca <- create_mock_pca_scores(50, seed = 42)
  
  # Make data very similar
  pca_cols <- grep("^Dim\\.", names(unknown_pca), value = TRUE)
  for (col in pca_cols) {
    unknown_pca[[col]] <- rnorm(50, 0, 0.1)  # Very small variance
  }
  
  result <- cluster_unknown_samples(
    unknown_pca_scores = unknown_pca,
    max_clusters = 4,
    verbose = FALSE
  )
  
  # Should identify 1 cluster when data is homogeneous
  expect_equal(result$n_clusters, 1)
  expect_true(all(result$clusters == 1))
})

test_that("cluster_unknown_samples handles edge cases", {
  # Test with very few samples
  small_pca <- create_mock_pca_scores(3)
  
  result <- cluster_unknown_samples(
    unknown_pca_scores = small_pca,
    max_clusters = 4,
    verbose = FALSE
  )
  
  # Should return single cluster for < 4 samples
  expect_equal(result$n_clusters, 1)
  
  # Test with single sample
  single_pca <- create_mock_pca_scores(1)
  
  result_single <- cluster_unknown_samples(
    unknown_pca_scores = single_pca,
    verbose = FALSE
  )
  
  expect_null(result_single)  # Should return NULL for single sample
})

# Test Mahalanobis Distance Calculation --------------------------------------

test_that("calculate_mahalanobis_distances handles normal cases", {
  # Create reference and query data
  reference <- create_mock_pca_scores(100, n_components = 5, seed = 1)
  query <- create_mock_pca_scores(20, n_components = 5, seed = 2)
  
  pca_cols <- grep("^Dim\\.", names(reference), value = TRUE)
  ref_matrix <- as.matrix(reference[pca_cols])
  query_matrix <- as.matrix(query[pca_cols])
  
  # Calculate distances
  distances <- calculate_mahalanobis_distances(
    query_matrix = query_matrix,
    reference_matrix = ref_matrix
  )
  
  # Check output
  expect_type(distances, "double")
  expect_equal(nrow(distances), nrow(query_matrix))
  expect_equal(ncol(distances), nrow(ref_matrix))
  
  # All distances should be non-negative
  expect_true(all(distances >= 0))
  
  # Distance to self should be smallest (if query point is in reference)
  # Add a query point that's in reference
  query_with_ref <- rbind(query_matrix, ref_matrix[1, , drop = FALSE])
  distances_with_ref <- calculate_mahalanobis_distances(
    query_matrix = query_with_ref,
    reference_matrix = ref_matrix
  )
  
  # Last query point (same as first reference) should have smallest distance to first reference
  last_row_distances <- distances_with_ref[nrow(distances_with_ref), ]
  expect_equal(which.min(last_row_distances), 1)
})

test_that("calculate_mahalanobis_distances handles singular covariance", {
  # Create data with perfect correlation (singular covariance)
  n_samples <- 50
  base_values <- rnorm(n_samples)
  
  reference <- tibble(
    Dim.1 = base_values,
    Dim.2 = base_values * 2,  # Perfect correlation
    Dim.3 = base_values * 3   # Perfect correlation
  )
  
  query <- tibble(
    Dim.1 = rnorm(10),
    Dim.2 = rnorm(10),
    Dim.3 = rnorm(10)
  )
  
  # Should handle singular covariance with regularization
  distances <- calculate_mahalanobis_distances(
    query_matrix = as.matrix(query),
    reference_matrix = as.matrix(reference),
    regularization = 1e-6
  )
  
  # Should return valid distances despite singular covariance
  expect_false(any(is.na(distances)))
  expect_false(any(is.infinite(distances)))
  expect_true(all(distances >= 0))
})

# Test Kennard-Stone Algorithm ------------------------------------------------

test_that("kennard_stone_cluster selects diverse samples", {
  set.seed(789)
  
  # Create cluster center and members
  cluster_center <- matrix(c(0, 0, 0), nrow = 1)
  
  # Create cluster members around center
  n_members <- 30
  cluster_unknowns <- matrix(
    rnorm(n_members * 3, mean = 0, sd = 2),
    nrow = n_members,
    ncol = 3
  )
  
  # Create relevant reference samples
  n_relevant <- 100
  relevant_matrix <- matrix(
    rnorm(n_relevant * 3, mean = 0, sd = 3),
    nrow = n_relevant,
    ncol = 3
  )
  
  # Select samples
  n_select <- 20
  selected_idx <- kennard_stone_cluster(
    cluster_center = cluster_center,
    cluster_unknowns = cluster_unknowns,
    relevant_matrix = relevant_matrix,
    n_select = n_select
  )
  
  # Check output
  expect_type(selected_idx, "integer")
  expect_equal(length(selected_idx), n_select)
  expect_true(all(selected_idx >= 1 & selected_idx <= n_relevant))
  
  # Selected indices should be unique
  expect_equal(length(unique(selected_idx)), n_select)
  
  # First selected should be closest to cluster center
  distances_to_center <- sqrt(rowSums((relevant_matrix - cluster_center[1, ])^2))
  expect_equal(selected_idx[1], which.min(distances_to_center))
})

test_that("kennard_stone_cluster handles edge cases", {
  cluster_center <- matrix(c(0, 0), nrow = 1)
  cluster_unknowns <- matrix(rnorm(10), nrow = 5, ncol = 2)
  relevant_matrix <- matrix(rnorm(20), nrow = 10, ncol = 2)
  
  # Request more samples than available
  selected <- kennard_stone_cluster(
    cluster_center = cluster_center,
    cluster_unknowns = cluster_unknowns,
    relevant_matrix = relevant_matrix,
    n_select = 15  # More than 10 available
  )
  
  # Should return all available samples
  expect_equal(length(selected), nrow(relevant_matrix))
  
  # Request 0 samples
  selected_zero <- kennard_stone_cluster(
    cluster_center = cluster_center,
    cluster_unknowns = cluster_unknowns,
    relevant_matrix = relevant_matrix,
    n_select = 0
  )
  
  expect_equal(length(selected_zero), 0)
})

# Test Stratified Kennard-Stone ----------------------------------------------

test_that("stratified_kennard_stone proportionally allocates samples", {
  set.seed(456)
  
  # Create mock data with 3 clusters of different sizes
  cluster1_size <- 50
  cluster2_size <- 30
  cluster3_size <- 20
  
  unknown_clusters <- c(
    rep(1, cluster1_size),
    rep(2, cluster2_size),
    rep(3, cluster3_size)
  )
  
  n_unknowns <- length(unknown_clusters)
  unknown_matrix <- matrix(
    rnorm(n_unknowns * 5),
    nrow = n_unknowns,
    ncol = 5
  )
  
  # Add cluster-specific patterns
  unknown_matrix[unknown_clusters == 1, 1] <- unknown_matrix[unknown_clusters == 1, 1] + 5
  unknown_matrix[unknown_clusters == 2, 2] <- unknown_matrix[unknown_clusters == 2, 2] - 5
  unknown_matrix[unknown_clusters == 3, 3] <- unknown_matrix[unknown_clusters == 3, 3] + 3
  
  # Create relevant OSSL samples
  n_relevant <- 500
  relevant_ossl <- tibble(
    sample_id = paste0("OSSL_", 1:n_relevant)
  )
  
  for (i in 1:5) {
    relevant_ossl[[paste0("Dim.", i)]] <- rnorm(n_relevant, 0, 2)
  }
  
  # Run stratified selection
  n_select <- 100
  result <- stratified_kennard_stone(
    unknown_clusters = unknown_clusters,
    unknown_matrix = unknown_matrix,
    relevant_ossl = relevant_ossl,
    n_select = n_select,
    verbose = FALSE
  )
  
  # Check output structure
  expect_type(result, "list")
  expect_true("selected_indices" %in% names(result))
  expect_true("cluster_allocations" %in% names(result))
  expect_true("selected_data" %in% names(result))
  
  # Check total selection
  expect_equal(length(result$selected_indices), n_select)
  
  # Check proportional allocation (roughly)
  allocations <- result$cluster_allocations
  expect_true(allocations[1] > allocations[2])  # Cluster 1 is largest
  expect_true(allocations[2] > allocations[3])  # Cluster 2 is middle
  
  # Check proportions are approximately correct
  expected_prop_1 <- cluster1_size / n_unknowns
  actual_prop_1 <- allocations[1] / n_select
  expect_true(abs(expected_prop_1 - actual_prop_1) < 0.1)
})

test_that("stratified_kennard_stone handles single cluster", {
  # All samples in one cluster
  unknown_clusters <- rep(1, 50)
  unknown_matrix <- matrix(rnorm(50 * 3), nrow = 50, ncol = 3)
  
  relevant_ossl <- tibble(
    sample_id = paste0("OSSL_", 1:200),
    Dim.1 = rnorm(200),
    Dim.2 = rnorm(200),
    Dim.3 = rnorm(200)
  )
  
  result <- stratified_kennard_stone(
    unknown_clusters = unknown_clusters,
    unknown_matrix = unknown_matrix,
    relevant_ossl = relevant_ossl,
    n_select = 50,
    verbose = FALSE
  )
  
  # All samples should be allocated to single cluster
  expect_equal(length(result$cluster_allocations), 1)
  expect_equal(result$cluster_allocations[[1]], 50)
})

# Test Global Selection Function ----------------------------------------------

test_that("select_global_training_set integrates all components", {
  set.seed(999)
  
  # Create unknown samples with structure
  unknown_pca <- create_mock_pca_scores(100, n_components = 10, seed = 1)
  
  # Create OSSL samples
  ossl_pca <- create_mock_pca_scores(1000, n_components = 10, seed = 2)
  
  # Add property data to OSSL
  ossl_pca$clay <- runif(1000, 100, 600)
  ossl_pca$ph <- runif(1000, 4, 9)
  
  # Run global selection
  result <- select_global_training_set(
    unknown_pca_scores = unknown_pca,
    ossl_pca_scores = ossl_pca,
    n_select = 200,
    n_train = 160,
    n_val = 40,
    relevance_threshold = 0.6,
    verbose = FALSE
  )
  
  # Check structure
  expect_type(result, "list")
  expect_true("train_data" %in% names(result))
  expect_true("val_data" %in% names(result))
  expect_true("selection_quality" %in% names(result))
  expect_true("cluster_info" %in% names(result))
  
  # Check data splits
  expect_equal(nrow(result$train_data), 160)
  expect_equal(nrow(result$val_data), 40)
  
  # Check no overlap between train and validation
  train_ids <- result$train_data$sample_id
  val_ids <- result$val_data$sample_id
  expect_equal(length(intersect(train_ids, val_ids)), 0)
  
  # Check properties are preserved
  expect_true("clay" %in% names(result$train_data))
  expect_true("ph" %in% names(result$train_data))
})

test_that("select_global_training_set validates parameters", {
  unknown_pca <- create_mock_pca_scores(50)
  ossl_pca <- create_mock_pca_scores(500)
  
  # Test mismatched n_train + n_val != n_select
  expect_error(
    select_global_training_set(
      unknown_pca_scores = unknown_pca,
      ossl_pca_scores = ossl_pca,
      n_select = 100,
      n_train = 70,
      n_val = 20,  # 70 + 20 != 100
      verbose = FALSE
    ),
    "n_train .* \\+ n_val .* must equal n_select"
  )
  
  # Test relevance threshold out of bounds
  expect_error(
    select_global_training_set(
      unknown_pca_scores = unknown_pca,
      ossl_pca_scores = ossl_pca,
      relevance_threshold = 1.5,  # > 1
      verbose = FALSE
    )
  )
})

# Test Performance ------------------------------------------------------------

test_that("stratified_kennard_stone scales with data size", {
  skip_on_cran()
  
  # Test with larger dataset
  n_unknowns <- 1000
  n_ossl <- 10000
  
  unknown_clusters <- sample(1:5, n_unknowns, replace = TRUE)
  unknown_matrix <- matrix(rnorm(n_unknowns * 20), nrow = n_unknowns)
  
  relevant_ossl <- create_mock_pca_scores(n_ossl, n_components = 20)
  
  # Should complete in reasonable time
  time_taken <- system.time({
    result <- stratified_kennard_stone(
      unknown_clusters = unknown_clusters,
      unknown_matrix = unknown_matrix,
      relevant_ossl = relevant_ossl,
      n_select = 500,
      verbose = FALSE
    )
  })
  
  expect_true(time_taken["elapsed"] < 30)  # Should complete within 30 seconds
  expect_equal(length(result$selected_indices), 500)
})

# Test Statistical Properties -------------------------------------------------

test_that("selected samples are representative of spectral space", {
  set.seed(2024)
  
  # Create unknown samples with specific distribution
  n_unknowns <- 200
  unknown_pca <- tibble(sample_id = paste0("UNK_", 1:n_unknowns))
  
  # Create bi-modal distribution in first component
  unknown_pca$Dim.1 <- c(
    rnorm(100, mean = -3, sd = 1),
    rnorm(100, mean = 3, sd = 1)
  )
  
  # Other components normal
  for (i in 2:5) {
    unknown_pca[[paste0("Dim.", i)]] <- rnorm(n_unknowns)
  }
  
  # Create OSSL covering broader space
  n_ossl <- 2000
  ossl_pca <- tibble(
    sample_id = paste0("OSSL_", 1:n_ossl),
    Dim.1 = rnorm(n_ossl, 0, 4)  # Covers both modes
  )
  
  for (i in 2:5) {
    ossl_pca[[paste0("Dim.", i)]] <- rnorm(n_ossl, 0, 1.5)
  }
  
  # Select training set
  result <- select_global_training_set(
    unknown_pca_scores = unknown_pca,
    ossl_pca_scores = ossl_pca,
    n_select = 300,
    n_train = 250,
    n_val = 50,
    relevance_threshold = 0.7,
    verbose = FALSE
  )
  
  # Check selected samples cover both modes
  selected_dim1 <- result$train_data$Dim.1
  
  # Should have samples from both negative and positive regions
  expect_true(sum(selected_dim1 < -1) > 20)  # At least some from negative mode
  expect_true(sum(selected_dim1 > 1) > 20)   # At least some from positive mode
  
  # Check coverage metrics in selection_quality
  expect_true("coverage" %in% names(result$selection_quality))
  expect_true(result$selection_quality$coverage > 0.7)  # Good coverage
})

test_that("diversity metric correctly identifies sample diversity", {
  # Create two sets of selected samples
  
  # Set 1: Diverse samples
  diverse_samples <- tibble(
    sample_id = paste0("S", 1:50),
    Dim.1 = rnorm(50, 0, 3),
    Dim.2 = rnorm(50, 0, 3),
    Dim.3 = rnorm(50, 0, 3)
  )
  
  # Set 2: Similar samples (low diversity)
  similar_samples <- tibble(
    sample_id = paste0("S", 1:50),
    Dim.1 = rnorm(50, 0, 0.5),
    Dim.2 = rnorm(50, 0, 0.5),
    Dim.3 = rnorm(50, 0, 0.5)
  )
  
  # Calculate diversity (using pairwise distances)
  pca_cols <- c("Dim.1", "Dim.2", "Dim.3")
  
  diverse_dist <- dist(diverse_samples[pca_cols])
  similar_dist <- dist(similar_samples[pca_cols])
  
  diverse_metric <- mean(diverse_dist)
  similar_metric <- mean(similar_dist)
  
  # Diverse samples should have higher metric
  expect_true(diverse_metric > similar_metric * 2)
})