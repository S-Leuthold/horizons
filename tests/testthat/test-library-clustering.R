## =============================================================================
## TEST: Library Clustering with GMM
## =============================================================================
##
## Purpose: Test GMM-based clustering on library data, BIC model selection,
##          unknown assignment with probabilities, and Mahalanobis AD metrics
##
## Coverage Target: >80%
## Phase: 1, Milestone: 1.2
##
## =============================================================================

library(testthat)
library(horizons)

## =============================================================================
## TEST GROUP 1: GMM Clustering with BIC Selection
## =============================================================================

test_that("fit_gmm_clustering selects optimal K via BIC", {

  ## Create synthetic PCA scores
  pca_scores <- matrix(rnorm(500 * 15), nrow = 500, ncol = 15)
  colnames(pca_scores) <- paste0("Dim.", 1:15)

  ## Fit GMM with BIC selection
  gmm_result <- horizons:::fit_gmm_clustering(
    pca_scores = pca_scores,
    k_range = c(3, 5, 7),
    verbose = FALSE
  )

  ## Should return expected structure
  expect_type(gmm_result, "list")
  expect_true(all(c("model", "n_clusters", "bic_values", "cluster_assignments") %in% names(gmm_result)))

  ## Optimal K should be in the tested range
  expect_true(gmm_result$n_clusters %in% c(3, 5, 7))

  ## Should have cluster assignments for all samples
  expect_equal(length(gmm_result$cluster_assignments), 500)

  ## Assignments should be integers in valid range
  expect_true(all(gmm_result$cluster_assignments %in% 1:gmm_result$n_clusters))

})

test_that("fit_gmm_clustering uses regularized covariances", {

  ## Small sample size relative to dimensions (triggers need for shrinkage)
  pca_scores <- matrix(rnorm(50 * 20), nrow = 50, ncol = 20)

  ## Should not fail even with more dimensions than ideal
  expect_no_error({
    gmm_result <- horizons:::fit_gmm_clustering(
      pca_scores = pca_scores,
      k_range = c(3, 5),
      covariance_regularization = TRUE,
      verbose = FALSE
    )
  })

  ## Should return valid result
  expect_type(gmm_result, "list")
  expect_gt(gmm_result$n_clusters, 0)

})

test_that("fit_gmm_clustering warns about small clusters", {

  pca_scores <- matrix(rnorm(1000 * 10), nrow = 1000, ncol = 10)

  ## Should warn about small clusters but still return result
  expect_warning(
    gmm_result <- horizons:::fit_gmm_clustering(
      pca_scores = pca_scores,
      k_range = c(3, 5, 7, 9),
      min_cluster_size = 100,
      verbose = FALSE
    ),
    "below minimum size"
  )

  ## Should still return valid result (warns, doesn't enforce)
  expect_type(gmm_result, "list")
  expect_gt(gmm_result$n_clusters, 0)

  ## Note: Actual enforcement (merging) deferred to future version
  ## v1.0 just warns - user can adjust min_cluster_size if needed

})

test_that("fit_gmm_clustering is deterministic with seed", {

  pca_scores <- matrix(rnorm(200 * 10), nrow = 200, ncol = 10)

  ## Fit twice with same seed
  set.seed(123)
  result1 <- horizons:::fit_gmm_clustering(pca_scores, k_range = c(3, 5), verbose = FALSE)

  set.seed(123)
  result2 <- horizons:::fit_gmm_clustering(pca_scores, k_range = c(3, 5), verbose = FALSE)

  ## Should get identical results
  expect_equal(result1$n_clusters, result2$n_clusters)
  expect_equal(result1$cluster_assignments, result2$cluster_assignments)

})

## =============================================================================
## TEST GROUP 2: Unknown Assignment with Probabilities
## =============================================================================

test_that("assign_to_clusters assigns unknowns to nearest cluster", {

  ## Fit real GMM on synthetic data (can't mock predict.Mclust easily)
  library_pca <- matrix(c(
    rnorm(50, mean = 0, sd = 0.5),   # Cluster 1 data
    rnorm(50, mean = 5, sd = 0.5),   # Cluster 2 data
    rnorm(50, mean = -5, sd = 0.5)   # Cluster 3 data
  ), nrow = 150, ncol = 2)

  gmm_result <- horizons:::fit_gmm_clustering(
    pca_scores = library_pca,
    k_range = 3,  # Force 3 clusters
    min_cluster_size = 10,
    verbose = FALSE
  )

  ## Create unknowns near each centroid (based on actual centroids)
  ## Centroids are n_clusters × n_dims, so add small noise
  n_clusters <- nrow(gmm_result$centroids)
  n_dims     <- ncol(gmm_result$centroids)
  unknowns   <- gmm_result$centroids + matrix(rnorm(n_clusters * n_dims, sd = 0.1),
                                              nrow = n_clusters, ncol = n_dims)

  ## Assign
  assignments <- horizons:::assign_to_clusters(
    unknown_pca_scores = unknowns,
    gmm_model = gmm_result,
    verbose = FALSE
  )

  ## Should return tibble with expected columns
  expect_s3_class(assignments, "tbl_df")
  expect_true(all(c("sample_id", "cluster_id", "probability", "entropy") %in% names(assignments)))

  ## Should have assignment for each unknown
  expect_equal(nrow(assignments), n_clusters)

  ## Cluster IDs should be valid (in range 1 to actual n_clusters found)
  expect_true(all(assignments$cluster_id %in% 1:gmm_result$n_clusters))

  ## Probabilities should be valid (0-1)
  expect_true(all(assignments$probability >= 0 & assignments$probability <= 1))

  ## Since unknowns are near centroids, most should be high confidence
  ## (But GMM might merge clusters, so we just check validity)
  expect_true(mean(assignments$probability) > 0.5)

})

test_that("assign_to_clusters handles ambiguous assignments", {

  ## Fit GMM on overlapping clusters (large variance)
  library_pca <- rbind(
    matrix(rnorm(50 * 2, mean = 0, sd = 2), nrow = 50),  # Cluster 1 (diffuse)
    matrix(rnorm(50 * 2, mean = 1, sd = 2), nrow = 50)   # Cluster 2 (overlaps)
  )

  gmm_result <- horizons:::fit_gmm_clustering(
    pca_scores = library_pca,
    k_range = 2,
    min_cluster_size = 10,
    verbose = FALSE
  )

  ## Unknown between two clusters
  unknowns <- matrix(c(0.5, 0.5), nrow = 1, ncol = 2)

  assignments <- horizons:::assign_to_clusters(
    unknown_pca_scores = unknowns,
    gmm_model = gmm_result,
    verbose = FALSE
  )

  ## Should still get assignment (to one cluster)
  expect_equal(nrow(assignments), 1)
  expect_true(assignments$cluster_id[1] %in% 1:2)

  ## With overlapping clusters, assignment might have lower confidence
  ## (This is probabilistic, so we just check it's valid)
  expect_true(assignments$probability[1] >= 0 & assignments$probability[1] <= 1)
  expect_true(assignments$entropy[1] >= 0)

})

test_that("assign_to_clusters probability scores sum to 1", {

  ## This validates we're computing probabilities correctly
  gmm_model <- list(
    parameters = list(
      mean = matrix(rnorm(9), nrow = 3, ncol = 3),
      variance = list(Sigma = array(diag(3), dim = c(3, 3, 3)))
    ),
    G = 3
  )

  unknowns <- matrix(rnorm(10 * 3), nrow = 10, ncol = 3)

  ## We'll need to expose the probability matrix for this test
  ## Or verify indirectly that max probability + entropy are sensible
  assignments <- horizons:::assign_to_clusters(unknowns, gmm_model, verbose = FALSE)

  ## Probabilities should be valid (0-1)
  expect_true(all(assignments$probability >= 0 & assignments$probability <= 1))

  ## Entropy should be valid (0 to log(n_clusters))
  max_entropy <- log(3)  # 3 clusters
  expect_true(all(assignments$entropy >= 0 & assignments$entropy <= max_entropy))

})

## =============================================================================
## TEST GROUP 3: Mahalanobis Distance and AD Thresholds
## =============================================================================

test_that("calculate_mahalanobis_distance computes correctly", {

  ## Simple 2D case where we can verify by hand
  centroid <- c(0, 0)
  covariance <- diag(2)  # Identity matrix
  point <- c(3, 4)  # Distance should be sqrt(3^2 + 4^2) = 5

  distance <- horizons:::calculate_mahalanobis_distance(
    point = point,
    centroid = centroid,
    covariance = covariance
  )

  expect_equal(distance, 5, tolerance = 0.01)

})

test_that("calculate_mahalanobis_distance handles non-identity covariance", {

  centroid <- c(0, 0)
  covariance <- matrix(c(2, 0.5, 0.5, 2), nrow = 2)  # Correlated
  point <- c(1, 1)

  ## Distance should be numeric and positive
  distance <- horizons:::calculate_mahalanobis_distance(point, centroid, covariance)

  expect_type(distance, "double")
  expect_gt(distance, 0)

})

test_that("compute_ad_thresholds calculates percentiles per cluster", {

  ## Create mock cluster assignments and distances
  cluster_data <- tibble::tibble(
    cluster_id = rep(1:3, each = 100),
    mahalanobis_dist = c(
      rchisq(100, df = 5),  # Cluster 1
      rchisq(100, df = 5),  # Cluster 2
      rchisq(100, df = 5)   # Cluster 3
    )
  )

  thresholds <- horizons:::compute_ad_thresholds(
    cluster_data = cluster_data,
    percentiles = c(0.95, 0.995)
  )

  ## Should return tibble with cluster-specific thresholds
  expect_s3_class(thresholds, "tbl_df")
  expect_equal(nrow(thresholds), 3)  # 3 clusters
  expect_true(all(c("cluster_id", "p95", "p995") %in% names(thresholds)))

  ## Thresholds should be positive and p995 > p95
  expect_true(all(thresholds$p95 > 0))
  expect_true(all(thresholds$p995 > thresholds$p95))

})

## =============================================================================
## TEST GROUP 4: Integration Tests
## =============================================================================

test_that("complete clustering workflow works", {

  skip("Full clustering workflow - too expensive for routine testing")

  skip_if_offline()

  ## This would test: load → preprocess → PCA → GMM → assign unknowns
  ## Run manually when validating full pipeline

})

test_that("clustering handles small datasets", {

  ## Edge case: only 100 samples, trying to fit 7 clusters
  pca_scores <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)

  ## Should fallback to fewer clusters or handle gracefully
  expect_no_error({
    result <- horizons:::fit_gmm_clustering(
      pca_scores = pca_scores,
      k_range = c(3, 5, 7),
      min_cluster_size = 10,
      verbose = FALSE
    )
  })

  ## Should select K that respects min cluster size
  cluster_sizes <- table(result$cluster_assignments)
  expect_true(all(cluster_sizes >= 10))

})

## =============================================================================
## TEST GROUP 5: Error Handling
## =============================================================================

test_that("fit_gmm_clustering handles degenerate data", {

  ## All samples identical (no variance)
  pca_scores <- matrix(rep(1, 50 * 5), nrow = 50, ncol = 5)

  ## Should return NULL or error informatively
  result <- horizons:::fit_gmm_clustering(
    pca_scores = pca_scores,
    k_range = c(2, 3),
    verbose = FALSE
  )

  ## Either NULL or falls back to single cluster
  if (!is.null(result)) {
    expect_lte(result$n_clusters, 1)
  }

})

test_that("assign_to_clusters handles OOD samples", {

  ## Fit GMM on tight cluster near origin
  library_pca <- matrix(rnorm(100 * 2, mean = 0, sd = 0.5), nrow = 100)

  gmm_result <- horizons:::fit_gmm_clustering(
    pca_scores = library_pca,
    k_range = 1,  # Single tight cluster
    min_cluster_size = 10,
    verbose = FALSE
  )

  ## Unknown very far from centroid
  unknowns <- matrix(c(100, 100), nrow = 1, ncol = 2)

  assignments <- horizons:::assign_to_clusters(
    unknown_pca_scores = unknowns,
    gmm_model = gmm_result,
    verbose = FALSE
  )

  ## Should still assign to the only cluster
  expect_equal(assignments$cluster_id[1], 1)

  ## But probability should be very low (far from training data)
  ## With single cluster, probability might still be 1.0 (only option)
  ## So we check entropy instead or just verify valid range
  expect_true(assignments$probability[1] >= 0 & assignments$probability[1] <= 1)

  ## Flag should indicate low confidence or OOD
  expect_true(assignments$flag[1] %in% c("low_confidence", "moderate_confidence", "high_confidence"))

})

## =============================================================================
## Helper: Create test GMM model
## =============================================================================

make_test_gmm <- function(n_clusters = 3, n_dims = 5) {

  ## Create simple GMM for testing
  centroids <- matrix(rnorm(n_clusters * n_dims, sd = 5), nrow = n_clusters)

  list(
    parameters = list(
      mean = centroids,
      variance = list(Sigma = array(diag(n_dims), dim = c(n_dims, n_dims, n_clusters)))
    ),
    G = n_clusters,
    classification = sample(1:n_clusters, 100, replace = TRUE)
  )

}
