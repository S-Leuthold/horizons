#' Performance and Benchmark Tests for Covariate Prediction System
#' 
#' Tests for computational performance, memory usage, and scalability.
#' These tests ensure the system performs efficiently with large datasets.

library(testthat)
library(horizons)
library(bench)
library(profmem)

# Performance Benchmarks ------------------------------------------------------

test_that("spectral preprocessing scales linearly with samples", {
  skip_on_cran()
  skip_on_ci()  # Skip in CI to save time
  
  # Test different sample sizes
  sizes <- c(100, 500, 1000, 2500)
  times <- numeric(length(sizes))
  
  for (i in seq_along(sizes)) {
    n <- sizes[i]
    
    # Create test data
    data <- tibble::tibble(sample_id = paste0("S", 1:n))
    for (j in 1:100) {
      data[[paste0("scan_mir.", 600 + j*30)]] <- runif(n)
    }
    
    # Measure time
    timing <- bench::mark(
      preprocess_mir_spectra(data, smooth_window = 9),
      iterations = 3,
      check = FALSE
    )
    
    times[i] <- median(timing$median)
  }
  
  # Check approximately linear scaling
  # Time should increase roughly proportionally to sample size
  time_ratios <- times[-1] / times[-length(times)]
  size_ratios <- sizes[-1] / sizes[-length(sizes)]
  
  # Allow 20% deviation from perfect linear scaling
  scaling_efficiency <- time_ratios / size_ratios
  expect_true(all(scaling_efficiency < 1.2))
})

test_that("PCA computation handles high dimensions efficiently", {
  skip_on_cran()
  
  # Create high-dimensional data
  n_samples <- 1000
  n_features <- 500
  
  data <- tibble::tibble(sample_id = paste0("S", 1:n_samples))
  for (i in 1:n_features) {
    data[[paste0("scan_mir.", i)]] <- rnorm(n_samples)
  }
  
  # Benchmark PCA
  timing <- bench::mark(
    perform_pca_on_ossl(data, variance_threshold = 0.95),
    iterations = 3,
    memory = FALSE  # Skip memory profiling for speed
  )
  
  # Should complete within reasonable time
  expect_true(as.numeric(timing$median) < 10)  # Less than 10 seconds
})

test_that("Kennard-Stone selection performs well with large datasets", {
  skip_on_cran()
  
  set.seed(123)
  
  # Create large datasets
  n_unknowns <- 500
  n_ossl <- 5000
  n_dims <- 20
  
  unknown_matrix <- matrix(rnorm(n_unknowns * n_dims), nrow = n_unknowns)
  unknown_clusters <- sample(1:3, n_unknowns, replace = TRUE)
  
  relevant_ossl <- tibble::tibble(sample_id = paste0("OSSL_", 1:n_ossl))
  for (i in 1:n_dims) {
    relevant_ossl[[paste0("Dim.", i)]] <- rnorm(n_ossl)
  }
  
  # Benchmark selection
  timing <- bench::mark(
    stratified_kennard_stone(
      unknown_clusters = unknown_clusters,
      unknown_matrix = unknown_matrix,
      relevant_ossl = relevant_ossl,
      n_select = 500,
      verbose = FALSE
    ),
    iterations = 1  # Single iteration for expensive operation
  )
  
  # Should complete in reasonable time even with 5000 candidates
  expect_true(as.numeric(timing$median) < 60)  # Less than 1 minute
})

# Memory Usage Tests ----------------------------------------------------------

test_that("memory usage is controlled during preprocessing", {
  skip_on_cran()
  skip_if_not_installed("profmem")
  
  # Create moderately large dataset
  n_samples <- 1000
  n_wavelengths <- 200
  
  data <- tibble::tibble(sample_id = paste0("S", 1:n_samples))
  for (i in 1:n_wavelengths) {
    data[[paste0("scan_mir.", 600 + i*15)]] <- runif(n_samples)
  }
  
  # Profile memory usage
  mem_profile <- profmem::profmem({
    processed <- preprocess_mir_spectra(data)
  })
  
  # Calculate total memory allocated
  total_bytes <- sum(mem_profile$bytes, na.rm = TRUE)
  data_size <- object.size(data)
  
  # Memory usage shouldn't exceed 3x the input data size
  # (accounting for intermediate calculations)
  expect_true(total_bytes < as.numeric(data_size) * 3)
})

test_that("PCA memory footprint is reasonable", {
  skip_on_cran()
  skip_if_not_installed("profmem")
  
  # Create test data
  n_samples <- 500
  n_features <- 300
  
  data <- tibble::tibble(sample_id = paste0("S", 1:n_samples))
  for (i in 1:n_features) {
    data[[paste0("scan_mir.", i)]] <- rnorm(n_samples)
  }
  
  # Profile PCA
  mem_profile <- profmem::profmem({
    pca_result <- perform_pca_on_ossl(data, variance_threshold = 0.95)
  })
  
  total_bytes <- sum(mem_profile$bytes, na.rm = TRUE)
  data_size <- n_samples * n_features * 8  # 8 bytes per double
  
  # PCA shouldn't use more than 5x the data matrix size
  expect_true(total_bytes < data_size * 5)
})

# Parallelization Tests -------------------------------------------------------

test_that("parallel processing improves performance when available", {
  skip_on_cran()
  skip_if_not(parallel::detectCores() > 1, "Single core system")
  
  # Create dataset for parallel processing
  n_samples <- 1000
  train_data <- tibble::tibble(sample_id = paste0("S", 1:n_samples))
  
  for (i in 1:10) {
    train_data[[paste0("Dim.", i)]] <- rnorm(n_samples)
  }
  train_data$clay <- 300 + 20 * train_data$Dim.1 + rnorm(n_samples, 0, 30)
  
  val_data <- train_data[801:1000, ]
  train_data <- train_data[1:800, ]
  
  # Test with single core
  options(horizons.parallel = FALSE)
  time_single <- system.time({
    result_single <- fit_cubist_model(
      train_data, val_data, "clay",
      bayesian_iter = 5, verbose = FALSE
    )
  })["elapsed"]
  
  # Test with parallel processing
  options(horizons.parallel = TRUE)
  time_parallel <- system.time({
    result_parallel <- fit_cubist_model(
      train_data, val_data, "clay",
      bayesian_iter = 5, verbose = FALSE
    )
  })["elapsed"]
  
  # Parallel should be faster (allow some overhead)
  expect_true(time_parallel < time_single * 1.2)
  
  # Reset option
  options(horizons.parallel = NULL)
})

# Caching Performance ---------------------------------------------------------

test_that("caching improves repeated operations", {
  skip_on_cran()
  
  with_test_cache({
    # Create test data
    data <- tibble::tibble(sample_id = paste0("S", 1:100))
    for (i in 1:50) {
      data[[paste0("scan_mir.", 600 + i*50)]] <- runif(100)
    }
    
    # First run - no cache
    time1 <- system.time({
      result1 <- preprocess_mir_spectra(data, cache = TRUE)
    })["elapsed"]
    
    # Second run - should use cache
    time2 <- system.time({
      result2 <- preprocess_mir_spectra(data, cache = TRUE)
    })["elapsed"]
    
    # Cached version should be much faster
    expect_true(time2 < time1 * 0.1)  # At least 10x faster
    
    # Results should be identical
    expect_identical(result1, result2)
  })
})

# Stress Tests ----------------------------------------------------------------

test_that("system handles maximum expected load", {
  skip_on_cran()
  skip("Manual stress test - run individually")
  
  # Maximum expected scenario:
  # - 10,000 unknown samples
  # - 50,000 OSSL samples
  # - 1000 wavelengths
  
  n_unknowns <- 10000
  n_ossl <- 50000
  n_wavelengths <- 1000
  
  # This would require significant memory and time
  # Only run manually for stress testing
  
  expect_true(TRUE)  # Placeholder
})

# Optimization Verification ---------------------------------------------------

test_that("Bayesian optimization converges efficiently", {
  set.seed(456)
  
  # Create training data with known optimal parameters
  n_samples <- 500
  train_data <- tibble::tibble(sample_id = paste0("S", 1:n_samples))
  
  for (i in 1:10) {
    train_data[[paste0("Dim.", i)]] <- rnorm(n_samples)
  }
  train_data$clay <- 300 + 20 * train_data$Dim.1 + 15 * train_data$Dim.2 + 
                     rnorm(n_samples, 0, 20)
  
  val_data <- train_data[401:500, ]
  train_data <- train_data[1:400, ]
  
  # Run optimization
  result <- fit_cubist_model(
    train_data, val_data, "clay",
    bayesian_iter = 15,
    verbose = FALSE
  )
  
  # Check convergence
  history <- result$optimization_history
  
  # Score should improve over iterations
  early_scores <- mean(history$score[1:5])
  late_scores <- mean(history$score[11:15])
  
  expect_true(late_scores > early_scores)
  
  # Should find good parameters
  expect_true(result$performance$val_r2 > 0.6)
})

# Profiling Helpers -----------------------------------------------------------

test_that("performance profiling utilities work", {
  skip_on_cran()
  
  # Helper function to profile a pipeline step
  profile_step <- function(step_name, step_function, ...) {
    timing <- bench::mark(
      step_function(...),
      iterations = 3,
      check = FALSE
    )
    
    list(
      step = step_name,
      median_time = timing$median,
      memory = timing$mem_alloc
    )
  }
  
  # Profile preprocessing
  data <- tibble::tibble(sample_id = paste0("S", 1:100))
  for (i in 1:50) {
    data[[paste0("scan_mir.", i)]] <- runif(100)
  }
  
  profile <- profile_step(
    "preprocessing",
    preprocess_mir_spectra,
    spectral_data = data
  )
  
  expect_type(profile, "list")
  expect_true("median_time" %in% names(profile))
})

# Resource Cleanup ------------------------------------------------------------

test_that("resources are properly cleaned up", {
  # Ensure temporary files are cleaned
  temp_files_before <- list.files(tempdir())
  
  with_test_cache({
    # Run some operations that create temp files
    data <- tibble::tibble(sample_id = "S1", scan_mir.1000 = 0.5)
    processed <- preprocess_mir_spectra(data)
  })
  
  temp_files_after <- list.files(tempdir())
  
  # No new permanent temp files should remain
  new_files <- setdiff(temp_files_after, temp_files_before)
  expect_equal(length(new_files), 0)
})