## =============================================================================
## TEST: Library Applicability Domain (AD) Metrics
## =============================================================================
##
## Purpose: Test Mahalanobis distance computation, AD bin assignment, and
##          metadata storage for library-based prediction.
##
## Coverage Target: >80%
## Phase: 4, Milestone: 4.1
##
## =============================================================================

library(testthat)
library(horizons)

## =============================================================================
## TEST GROUP 1: AD Metadata Computation (Training Phase)
## =============================================================================

test_that("compute_ad_metadata computes centroid and covariance", {

  ## Create synthetic feature matrix (post-preprocessing)
  set.seed(123)
  n_samples <- 100
  n_features <- 20

  feature_matrix <- matrix(
    rnorm(n_samples * n_features, mean = 5, sd = 2),
    nrow = n_samples,
    ncol = n_features
  )
  colnames(feature_matrix) <- paste0("PC", 1:n_features)

  ## Compute AD metadata
  ad_metadata <- horizons:::compute_ad_metadata(feature_matrix)

  ## Should return list with required components
  expect_type(ad_metadata, "list")
  expect_named(ad_metadata, c("centroid", "cov_matrix", "ad_thresholds"))

  ## Centroid should be vector of feature means
  expect_length(ad_metadata$centroid, n_features)
  expect_true(all(abs(ad_metadata$centroid - 5) < 1))  # Near mean of 5

  ## Covariance should be positive definite matrix
  expect_equal(dim(ad_metadata$cov_matrix), c(n_features, n_features))
  expect_true(all(eigen(ad_metadata$cov_matrix)$values > 0))  # Positive definite

  ## AD thresholds should have 4 values (Q1, Q2, Q3, p99)
  expect_length(ad_metadata$ad_thresholds, 4)
  expect_true(all(diff(ad_metadata$ad_thresholds) > 0))  # Increasing

})

test_that("compute_ad_metadata handles high-dimensional data (p > n)", {

  ## More features than samples (requires shrinkage)
  set.seed(456)
  n_samples <- 50
  n_features <- 100  # p > n

  feature_matrix <- matrix(
    rnorm(n_samples * n_features),
    nrow = n_samples,
    ncol = n_features
  )
  colnames(feature_matrix) <- paste0("feature_", 1:n_features)

  ## Should not error with shrinkage
  expect_no_error({
    ad_metadata <- horizons:::compute_ad_metadata(feature_matrix)
  })

  ## Should still produce valid covariance
  expect_equal(dim(ad_metadata$cov_matrix), c(n_features, n_features))

})

test_that("compute_ad_metadata validates inputs", {

  ## Missing column names
  bad_matrix <- matrix(rnorm(100), nrow = 10, ncol = 10)
  expect_error(
    horizons:::compute_ad_metadata(bad_matrix),
    "must have column names"
  )

  ## Too few samples
  small_matrix <- matrix(rnorm(30), nrow = 3, ncol = 10)
  colnames(small_matrix) <- paste0("PC", 1:10)
  expect_error(
    horizons:::compute_ad_metadata(small_matrix),
    "at least 10 samples"
  )

  ## Non-numeric data
  char_matrix <- matrix(as.character(rnorm(100)), nrow = 10, ncol = 10)
  colnames(char_matrix) <- paste0("PC", 1:10)
  expect_error(
    horizons:::compute_ad_metadata(char_matrix),
    "numeric"
  )

})

## =============================================================================
## TEST GROUP 2: AD Distance Calculation (Prediction Phase)
## =============================================================================

test_that("calculate_ad_distance computes Mahalanobis distances", {

  ## Create training data and metadata
  set.seed(789)
  train_matrix <- matrix(rnorm(100 * 10, mean = 0, sd = 1), nrow = 100, ncol = 10)
  colnames(train_matrix) <- paste0("PC", 1:10)

  ad_metadata <- horizons:::compute_ad_metadata(train_matrix)

  ## Create new samples (similar to training)
  new_matrix <- matrix(rnorm(20 * 10, mean = 0, sd = 1), nrow = 20, ncol = 10)
  colnames(new_matrix) <- paste0("PC", 1:10)

  ## Compute distances
  distances <- horizons:::calculate_ad_distance(new_matrix, ad_metadata)

  ## Should return numeric vector
  expect_type(distances, "double")
  expect_length(distances, 20)
  expect_true(all(distances >= 0))  # Distances are non-negative

  ## Similar samples should have low distances
  expect_true(median(distances) < 50)  # Reasonable for 10D

})

test_that("calculate_ad_distance detects out-of-distribution samples", {

  ## Training data centered at 0
  set.seed(101)
  train_matrix <- matrix(rnorm(100 * 10, mean = 0, sd = 1), nrow = 100, ncol = 10)
  colnames(train_matrix) <- paste0("PC", 1:10)

  ad_metadata <- horizons:::compute_ad_metadata(train_matrix)

  ## Create OOD samples (shifted far from centroid)
  ood_matrix <- matrix(rnorm(10 * 10, mean = 10, sd = 1), nrow = 10, ncol = 10)
  colnames(ood_matrix) <- paste0("PC", 1:10)

  ## OOD distances should be much larger
  ood_distances <- horizons:::calculate_ad_distance(ood_matrix, ad_metadata)
  train_distances <- mahalanobis(train_matrix,
                                 ad_metadata$centroid,
                                 ad_metadata$cov_matrix)

  expect_true(median(ood_distances) > quantile(train_distances, 0.99))

})

test_that("calculate_ad_distance validates inputs", {

  ## Create valid metadata
  train_matrix <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(train_matrix) <- paste0("PC", 1:10)
  ad_metadata <- horizons:::compute_ad_metadata(train_matrix)

  ## Mismatched column names
  bad_matrix <- matrix(rnorm(50), nrow = 5, ncol = 10)
  colnames(bad_matrix) <- paste0("feature_", 1:10)  # Different names

  expect_error(
    horizons:::calculate_ad_distance(bad_matrix, ad_metadata),
    "column names.*must match"
  )

  ## Wrong number of features
  wrong_matrix <- matrix(rnorm(40), nrow = 5, ncol = 8)
  colnames(wrong_matrix) <- paste0("PC", 1:8)

  expect_error(
    horizons:::calculate_ad_distance(wrong_matrix, ad_metadata),
    "features.*must match"
  )

})

## =============================================================================
## TEST GROUP 3: AD Bin Assignment
## =============================================================================

test_that("assign_ad_bin categorizes distances correctly", {

  ## Create distance vector
  distances <- c(5, 15, 25, 35, 50)  # Q1, Q2, Q3, Q4, OOD

  ## Create thresholds
  thresholds <- c(10, 20, 30, 45)  # 25th, 50th, 75th, 99th percentiles

  ## Assign bins
  bins <- horizons:::assign_ad_bin(distances, thresholds)

  ## Should return factor
  expect_s3_class(bins, "factor")
  expect_length(bins, 5)

  ## Check assignments
  expect_equal(as.character(bins), c("Q1", "Q2", "Q3", "Q4", "OOD"))

})

test_that("assign_ad_bin handles edge cases", {

  ## All samples in Q1
  low_distances <- rep(5, 10)
  thresholds <- c(10, 20, 30, 40)

  bins <- horizons:::assign_ad_bin(low_distances, thresholds)
  expect_true(all(bins == "Q1"))

  ## All samples OOD
  high_distances <- rep(50, 10)
  bins <- horizons:::assign_ad_bin(high_distances, thresholds)
  expect_true(all(bins == "OOD"))

  ## Exactly at threshold boundaries
  boundary_distances <- c(10, 20, 30, 40)
  bins <- horizons:::assign_ad_bin(boundary_distances, thresholds)
  expect_equal(as.character(bins), c("Q1", "Q2", "Q3", "Q4"))

})

test_that("assign_ad_bin validates inputs", {

  thresholds <- c(10, 20, 30, 40)

  ## Non-numeric distances
  expect_error(
    horizons:::assign_ad_bin(c("a", "b"), thresholds),
    "numeric"
  )

  ## Negative distances
  expect_error(
    horizons:::assign_ad_bin(c(-5, 10, 20), thresholds),
    "non-negative"
  )

  ## Wrong number of thresholds
  expect_error(
    horizons:::assign_ad_bin(c(5, 15), c(10, 20)),
    "numeric vector of length 4"
  )

})

## =============================================================================
## TEST GROUP 4: Integration Tests
## =============================================================================

test_that("AD workflow works end-to-end", {

  skip("Integration test - requires full pipeline")

  ## This will test the full workflow:
  ## 1. Train model with AD metadata
  ## 2. Predict with AD distance/bin in output
  ## 3. Verify AD columns present and valid

})
