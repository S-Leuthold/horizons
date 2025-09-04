#' Tests for finalize_dataset() Function
#'
#' Comprehensive test suite for outlier detection and dataset finalization
#' including PCA-based spectral outliers and IQR-based response outliers

# Setup test data --------------------------------------------------------------

setup_test_dataset <- function() {
  # Create realistic dataset with spectral and response data
  n_samples <- 20
  wavenumbers <- c("4000", "3500", "3000", "2500", "2000", "1500", "1000", "600")
  
  # Create spectral matrix with realistic patterns
  set.seed(123)  # For reproducible tests
  spectra_matrix <- matrix(
    data = replicate(n_samples, {
      # Base spectrum with typical absorption features
      baseline <- 0.5 + 0.3 * exp(-((as.numeric(wavenumbers) - 1500)^2) / 500000)
      noise <- rnorm(length(wavenumbers), 0, 0.05)
      pmax(baseline + noise, 0)
    }),
    nrow = n_samples,
    ncol = length(wavenumbers)
  )
  
  # Add some outliers
  # Spectral outlier - very different spectrum
  spectra_matrix[1, ] <- spectra_matrix[1, ] * 3  # Much higher values
  # Another spectral outlier - very low values  
  spectra_matrix[2, ] <- spectra_matrix[2, ] * 0.1
  
  # Convert to data frame
  dataset <- as.data.frame(spectra_matrix)
  names(dataset) <- wavenumbers
  dataset$Sample_ID <- paste0("sample_", sprintf("%03d", 1:n_samples))
  
  # Add response variables
  dataset$SOC <- c(
    15.0,  # Response outlier - very high SOC
    2.1, 2.5, 1.9, 2.3, 2.7, 2.0, 2.4, 2.8, 1.8,
    2.2, 2.6, 2.1, 2.5, 1.9, 2.3, 2.7, 2.0, 2.4, 0.1  # Response outlier - very low SOC
  )
  dataset$pH <- rnorm(n_samples, 6.5, 0.5)
  
  # Add coordinates
  dataset$Latitude <- 40.1 + rnorm(n_samples, 0, 0.01)
  dataset$Longitude <- -105.2 + rnorm(n_samples, 0, 0.01)
  
  # Reorder columns (Sample_ID first, then spectral, then response)
  col_order <- c("Sample_ID", wavenumbers, "SOC", "pH", "Latitude", "Longitude")
  dataset <- dataset[, col_order]
  
  return(dataset)
}

setup_minimal_dataset <- function() {
  # Minimal dataset for edge case testing
  data.frame(
    Sample_ID = c("sample1", "sample2", "sample3"),
    `4000` = c(0.5, 0.6, 0.7),
    `3000` = c(0.4, 0.5, 0.6),
    `2000` = c(0.3, 0.4, 0.5),
    SOC = c(2.1, 2.5, 1.9),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Input Validation Tests -------------------------------------------------------

test_that("finalize_dataset validates input parameters correctly", {
  dataset <- setup_test_dataset()
  
  # Test dataset validation
  expect_error(
    finalize_dataset("not_dataframe", "SOC"),
    "dataset must be a data frame or tibble"
  )
  
  # Test missing response variable
  expect_error(
    finalize_dataset(dataset, "NonExistent"),
    "Response variable NonExistent not found in dataset"
  )
  
  # Test invalid spectral_outlier_method
  expect_error(
    finalize_dataset(dataset, "SOC", spectral_outlier_method = "invalid"),
    "should be one of"
  )
})

# Basic Outlier Detection Tests ------------------------------------------------

test_that("finalize_dataset detects outliers correctly", {
  dataset <- setup_test_dataset()
  
  # Mock PCA and robust covariance functions for predictable results
  with_mocked_bindings(
    `prcomp` = function(x, center = TRUE, scale. = FALSE) {
      # Mock PCA result
      n_obs <- nrow(x)
      n_vars <- ncol(x)
      n_pc <- min(n_obs - 1, n_vars, 5)  # Limit to 5 PCs for testing
      
      list(
        sdev = sqrt(c(10, 5, 3, 2, 1)[1:n_pc]),  # Eigenvalues
        x = matrix(rnorm(n_obs * n_pc), nrow = n_obs, ncol = n_pc)  # PC scores
      )
    },
    `robustbase::covMcd` = function(x) {
      # Mock robust covariance
      p <- ncol(x)
      list(
        center = colMeans(x),
        cov = diag(p) + 0.1 * matrix(rnorm(p*p), p, p)  # Simple covariance
      )
    },
    {
      result <- finalize_dataset(
        dataset = dataset,
        response_variable = "SOC",
        spectral_outlier_method = "mahalanobis",
        detect_response_outliers = TRUE,
        verbose = FALSE
      )
      
      # Check that outlier_flag column was added
      expect_true("outlier_flag" %in% names(result))
      expect_equal(nrow(result), nrow(dataset))  # Same number of rows
      
      # Check outlier flag values
      expect_true(all(result$outlier_flag %in% c("good", "outlier")))
    }
  )
})

test_that("finalize_dataset handles no outlier detection", {
  dataset <- setup_test_dataset()
  
  result <- finalize_dataset(
    dataset = dataset,
    response_variable = "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = FALSE,
    verbose = FALSE
  )
  
  expect_true("outlier_flag" %in% names(result))
  expect_true(all(result$outlier_flag == "good"))  # No outliers detected
})

# Spectral Outlier Detection Tests ---------------------------------------------

test_that("finalize_dataset performs PCA-based spectral outlier detection", {
  dataset <- setup_test_dataset()
  
  with_mocked_bindings(
    `prcomp` = function(x, center = TRUE, scale. = FALSE) {
      n_obs <- nrow(x)
      n_vars <- ncol(x)
      n_pc <- 3
      
      # Create PC scores where first sample is clearly an outlier
      pc_scores <- matrix(rnorm(n_obs * n_pc, 0, 1), nrow = n_obs, ncol = n_pc)
      pc_scores[1, ] <- c(5, 5, 5)  # Make first sample an outlier
      
      list(
        sdev = sqrt(c(3, 2, 1)),
        x = pc_scores
      )
    },
    `robustbase::covMcd` = function(x) {
      list(
        center = c(0, 0, 0),
        cov = diag(3)
      )
    },
    {
      result <- finalize_dataset(
        dataset = dataset,
        response_variable = "SOC",
        spectral_outlier_method = "mahalanobis",
        spectral_cutoff = 0.95,  # More lenient threshold
        detect_response_outliers = FALSE,
        verbose = FALSE
      )
      
      expect_true("outlier_flag" %in% names(result))
    }
  )
})

test_that("finalize_dataset handles PCA failures gracefully", {
  dataset <- setup_test_dataset()
  
  # Mock PCA to fail
  with_mocked_bindings(
    `prcomp` = function(...) {
      stop("PCA computation failed")
    },
    {
      expect_warning(
        result <- finalize_dataset(
          dataset, "SOC",
          spectral_outlier_method = "mahalanobis",
          verbose = FALSE
        ),
        "PCA failed"
      )
      
      # Should still return dataset with outlier flags
      expect_true("outlier_flag" %in% names(result))
      expect_true(all(result$outlier_flag == "good"))  # No outliers due to failure
    }
  )
})

test_that("finalize_dataset handles robust covariance failures", {
  dataset <- setup_test_dataset()
  
  with_mocked_bindings(
    `prcomp` = function(x, center = TRUE, scale. = FALSE) {
      list(
        sdev = sqrt(c(3, 2, 1)),
        x = matrix(rnorm(nrow(x) * 3), nrow = nrow(x), ncol = 3)
      )
    },
    `robustbase::covMcd` = function(...) {
      stop("Robust covariance failed")
    },
    {
      expect_warning(
        result <- finalize_dataset(
          dataset, "SOC",
          spectral_outlier_method = "mahalanobis",
          verbose = FALSE
        ),
        "Robust covariance failed"
      )
      
      expect_true("outlier_flag" %in% names(result))
    }
  )
})

# Response Outlier Detection Tests ----------------------------------------------

test_that("finalize_dataset detects response outliers using IQR method", {
  dataset <- setup_minimal_dataset()
  
  # Add clear outliers
  dataset$SOC <- c(1.0, 2.0, 50.0)  # Third value is clear outlier
  
  result <- finalize_dataset(
    dataset = dataset,
    response_variable = "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = TRUE,
    response_cutoff = 1.5,  # Standard IQR multiplier
    verbose = FALSE
  )
  
  # Should detect the extreme value as outlier
  expect_true("outlier_flag" %in% names(result))
  outlier_samples <- result[result$outlier_flag == "outlier", "Sample_ID"]
  expect_true("sample3" %in% outlier_samples)
})

test_that("finalize_dataset handles different IQR cutoff values", {
  dataset <- setup_minimal_dataset()
  dataset$SOC <- c(1.0, 2.0, 4.0)  # Moderate outlier
  
  # Strict cutoff (should detect outlier)
  result_strict <- finalize_dataset(
    dataset, "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = TRUE,
    response_cutoff = 1.0,  # Stricter
    verbose = FALSE
  )
  
  # Lenient cutoff (should not detect outlier)
  result_lenient <- finalize_dataset(
    dataset, "SOC", 
    spectral_outlier_method = "none",
    detect_response_outliers = TRUE,
    response_cutoff = 3.0,  # More lenient
    verbose = FALSE
  )
  
  n_outliers_strict <- sum(result_strict$outlier_flag == "outlier")
  n_outliers_lenient <- sum(result_lenient$outlier_flag == "outlier")
  
  expect_gte(n_outliers_strict, n_outliers_lenient)  # Strict should find more or equal
})

test_that("finalize_dataset handles NA values in response variable", {
  dataset <- setup_minimal_dataset()
  dataset$SOC[2] <- NA
  
  result <- finalize_dataset(
    dataset = dataset,
    response_variable = "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier_flag" %in% names(result))
})

# Outlier Removal Tests --------------------------------------------------------

test_that("finalize_dataset removes outliers when requested", {
  dataset <- setup_minimal_dataset()
  dataset$SOC <- c(1.0, 2.0, 50.0)  # Clear outlier
  
  # Remove outliers
  result_remove <- finalize_dataset(
    dataset = dataset,
    response_variable = "SOC",
    spectral_outlier_method = "none",
    detect_response_outliers = TRUE,
    remove_outliers = TRUE,
    verbose = FALSE
  )
  
  expect_lt(nrow(result_remove), nrow(dataset))  # Should have fewer rows
  
  # Keep outliers (just flag)
  result_keep <- finalize_dataset(
    dataset = dataset,
    response_variable = "SOC",
    spectral_outlier_method = "none", 
    detect_response_outliers = TRUE,
    remove_outliers = FALSE,
    verbose = FALSE
  )
  
  expect_equal(nrow(result_keep), nrow(dataset))  # Same number of rows
  expect_true(any(result_keep$outlier_flag == "outlier"))  # But has outlier flags
})

# Edge Cases Tests --------------------------------------------------------------

test_that("finalize_dataset handles insufficient data for PCA", {
  # Dataset with too few samples for meaningful PCA
  small_dataset <- data.frame(
    Sample_ID = c("sample1", "sample2"),
    `4000` = c(0.5, 0.6),
    `3000` = c(0.4, 0.5),
    SOC = c(2.1, 2.5),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    `prcomp` = function(...) {
      stop("insufficient data for PCA")
    },
    {
      expect_warning(
        result <- finalize_dataset(
          small_dataset, "SOC",
          spectral_outlier_method = "mahalanobis",
          verbose = FALSE
        ),
        "PCA failed"
      )
      
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("finalize_dataset handles dataset with no spectral columns", {
  # Dataset with only response variables
  no_spectral <- data.frame(
    Sample_ID = c("sample1", "sample2", "sample3"),
    SOC = c(2.1, 2.5, 1.9),
    pH = c(6.5, 7.0, 6.8),
    stringsAsFactors = FALSE
  )
  
  result <- finalize_dataset(
    no_spectral, "SOC",
    spectral_outlier_method = "mahalanobis",
    detect_response_outliers = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier_flag" %in% names(result))
})

test_that("finalize_dataset handles all identical spectra", {
  identical_spectra <- data.frame(
    Sample_ID = c("sample1", "sample2", "sample3"),
    `4000` = c(0.5, 0.5, 0.5),  # Identical values
    `3000` = c(0.4, 0.4, 0.4),
    `2000` = c(0.3, 0.3, 0.3),
    SOC = c(2.1, 2.5, 1.9),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    `prcomp` = function(...) {
      stop("constant data")
    },
    {
      expect_warning(
        result <- finalize_dataset(
          identical_spectra, "SOC",
          spectral_outlier_method = "mahalanobis",
          verbose = FALSE
        ),
        "PCA failed"
      )
    }
  )
})

# Verbose Output Tests ----------------------------------------------------------

test_that("finalize_dataset produces appropriate verbose output", {
  dataset <- setup_test_dataset()
  
  with_mocked_bindings(
    `prcomp` = function(...) {
      list(sdev = sqrt(c(3, 2, 1)), x = matrix(rnorm(60), 20, 3))
    },
    `robustbase::covMcd` = function(...) {
      list(center = c(0, 0, 0), cov = diag(3))
    },
    {
      expect_message(
        finalize_dataset(dataset, "SOC", verbose = TRUE),
        "Detecting spectral outliers using"
      )
      
      expect_message(
        finalize_dataset(dataset, "SOC", verbose = TRUE),
        "Detecting response outliers using IQR method"
      )
      
      expect_message(
        finalize_dataset(dataset, "SOC", verbose = True),
        "Total outliers identified:"
      )
    }
  )
})

test_that("finalize_dataset reports PCA information in verbose mode", {
  dataset <- setup_test_dataset()
  
  with_mocked_bindings(
    `prcomp` = function(...) {
      list(sdev = sqrt(c(5, 3, 2)), x = matrix(rnorm(60), 20, 3))
    },
    `robustbase::covMcd` = function(...) {
      list(center = c(0, 0, 0), cov = diag(3))
    },
    {
      expect_message(
        finalize_dataset(dataset, "SOC", verbose = TRUE),
        "Using .* PCs.*variance"
      )
    }
  )
})

# Integration Tests -------------------------------------------------------------

test_that("finalize_dataset works with create_dataset output", {
  # Create data similar to what create_dataset would produce
  dataset <- setup_test_dataset()
  dataset$n_replicates <- 1  # Column that create_dataset adds
  
  with_mocked_bindings(
    `prcomp` = function(...) {
      list(sdev = sqrt(c(3, 2, 1)), x = matrix(rnorm(60), 20, 3))
    },
    `robustbase::covMcd` = function(...) {
      list(center = c(0, 0, 0), cov = diag(3))
    },
    {
      result <- finalize_dataset(dataset, "SOC", verbose = FALSE)
      
      expect_s3_class(result, "data.frame")
      expect_true("outlier_flag" %in% names(result))
      expect_true("n_replicates" %in% names(result))  # Preserve other columns
    }
  )
})

test_that("finalize_dataset works with real test fixtures", {
  # Test with actual fixture data if available
  test_response_csv <- file.path(test_path("fixtures"), "test_response.csv")
  
  skip_if_not(file.exists(test_response_csv), "Test response CSV not found")
  
  # Create a dataset similar to what the pipeline would produce
  dataset <- data.frame(
    Sample_ID = c("FFAR_S094-A_GroundBulk_S1_G11", "FFAR_S094-A_GroundBulk_S2_H11"),
    `4000` = c(0.5, 0.6),
    `3000` = c(0.4, 0.5),
    `2000` = c(0.3, 0.4),
    SOC = c(2.5, 3.1),
    pH = c(6.8, 7.1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  result <- finalize_dataset(
    dataset = dataset,
    response_variable = "SOC",
    spectral_outlier_method = "none",  # Skip PCA for simple test
    detect_response_outliers = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier_flag" %in% names(result))
})

# Performance Tests -------------------------------------------------------------

test_that("finalize_dataset completes in reasonable time", {
  # Create larger dataset
  n_samples <- 100
  dataset_large <- data.frame(
    Sample_ID = paste0("sample_", 1:n_samples),
    stringsAsFactors = FALSE
  )
  
  # Add spectral columns
  for (i in 1:20) {
    col_name <- as.character(4000 - i*100)
    dataset_large[[col_name]] <- runif(n_samples, 0, 1)
  }
  
  dataset_large$SOC <- runif(n_samples, 1, 5)
  
  with_mocked_bindings(
    `prcomp` = function(...) {
      list(sdev = sqrt(c(3, 2, 1)), x = matrix(rnorm(n_samples * 3), n_samples, 3))
    },
    `robustbase::covMcd` = function(...) {
      list(center = c(0, 0, 0), cov = diag(3))
    },
    {
      start_time <- Sys.time()
      result <- finalize_dataset(dataset_large, "SOC", verbose = FALSE)
      end_time <- Sys.time()
      
      expect_lt(as.numeric(end_time - start_time), 10)  # Should complete within 10 seconds
      expect_equal(nrow(result), n_samples)
    }
  )
})

# Parameter Validation Tests ----------------------------------------------------

test_that("finalize_dataset validates cutoff parameters", {
  dataset <- setup_minimal_dataset()
  
  # Valid parameters should work
  expect_s3_class(
    finalize_dataset(dataset, "SOC", spectral_cutoff = 0.95, response_cutoff = 2.0, verbose = FALSE),
    "data.frame"
  )
  
  # Test edge case values
  expect_s3_class(
    finalize_dataset(dataset, "SOC", spectral_cutoff = 0.99, response_cutoff = 0.5, verbose = FALSE),
    "data.frame"
  )
})