#' Tests for preprocess_spectra() Function
#'
#' Comprehensive test suite for spectral preprocessing including baseline correction,
#' resampling, and data validation

# Setup test data --------------------------------------------------------------

setup_test_spectra <- function() {
  # Create realistic MIR spectral data
  wavenumbers <- seq(4000, 600, by = -10)  # Standard MIR range
  n_samples <- 10
  
  # Generate realistic spectral profiles with some noise
  spectra_matrix <- matrix(
    data = replicate(n_samples, {
      # Base spectrum with typical MIR features
      baseline <- 0.5 + 0.3 * exp(-(wavenumbers - 1500)^2 / 500000)
      noise <- rnorm(length(wavenumbers), 0, 0.05)
      pmax(baseline + noise, 0)  # Ensure non-negative values
    }),
    nrow = n_samples,
    ncol = length(wavenumbers)
  )
  
  # Convert to tibble format
  spectra_df <- as.data.frame(spectra_matrix)
  names(spectra_df) <- as.character(wavenumbers)
  spectra_df$Sample_ID <- paste0("sample_", sprintf("%03d", 1:n_samples))
  
  # Reorder columns (Sample_ID first)
  spectra_df <- spectra_df[, c("Sample_ID", as.character(wavenumbers))]
  
  # Add attributes like read_spectra() would
  attr(spectra_df, "spectra_type") <- "MIR"
  attr(spectra_df, "source") <- "test"
  
  return(spectra_df)
}

setup_nir_spectra <- function() {
  # Create NIR spectral data
  wavelengths <- seq(1000, 2500, by = 2)  # NIR range in nm
  n_samples <- 5
  
  spectra_matrix <- matrix(
    data = replicate(n_samples, {
      baseline <- 0.3 + 0.2 * sin((wavelengths - 1500) / 200)
      noise <- rnorm(length(wavelengths), 0, 0.02)
      pmax(baseline + noise, 0)
    }),
    nrow = n_samples,
    ncol = length(wavelengths)
  )
  
  spectra_df <- as.data.frame(spectra_matrix)
  names(spectra_df) <- as.character(wavelengths)
  spectra_df$Sample_ID <- paste0("nir_sample_", 1:n_samples)
  spectra_df <- spectra_df[, c("Sample_ID", as.character(wavelengths))]
  
  attr(spectra_df, "spectra_type") <- "NIR"
  
  return(spectra_df)
}

# Input Validation Tests -------------------------------------------------------

test_that("preprocess_spectra validates input parameters correctly", {
  test_data <- setup_test_spectra()
  
  # Test data.frame validation
  expect_error(
    preprocess_spectra("not_a_dataframe"),
    "spectra_data must be a data frame or tibble"
  )
  
  # Test Sample_ID column requirement
  test_data_no_id <- test_data
  test_data_no_id$Sample_ID <- NULL
  
  expect_error(
    preprocess_spectra(test_data_no_id),
    "spectra_data must have a Sample_ID column"
  )
  
  # Test resample_interval validation
  expect_error(
    preprocess_spectra(test_data, resample_interval = -1),
    "resample_interval must be a positive number"
  )
  
  expect_error(
    preprocess_spectra(test_data, resample_interval = 0),
    "resample_interval must be a positive number"
  )
  
  expect_error(
    preprocess_spectra(test_data, resample_interval = "invalid"),
    "resample_interval must be a positive number"
  )
  
  # Test baseline_method validation
  expect_error(
    preprocess_spectra(test_data, baseline_method = "invalid"),
    "should be one of"
  )
})

# Basic Preprocessing Tests -----------------------------------------------------

test_that("preprocess_spectra performs basic preprocessing correctly", {
  test_data <- setup_test_spectra()
  
  # Test basic preprocessing with no baseline correction
  result <- preprocess_spectra(
    test_data,
    resample_interval = 4,
    baseline_method = "none",
    verbose = FALSE
  )
  
  # Check basic structure
  expect_s3_class(result, "data.frame")
  expect_true("Sample_ID" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
  
  # Check that spectral columns are numeric wavenumbers
  spectral_cols <- setdiff(names(result), "Sample_ID")
  wn_values <- as.numeric(spectral_cols)
  expect_false(any(is.na(wn_values)))
  
  # Check resampling interval
  wn_diffs <- diff(sort(wn_values, decreasing = TRUE))
  expect_true(all(abs(wn_diffs - 4) < 0.01))  # Should be close to 4 cm-1 intervals
  
  # Check MIR range
  expect_true(min(wn_values) >= 600)
  expect_true(max(wn_values) <= 4000)
})

test_that("preprocess_spectra handles different resample intervals", {
  test_data <- setup_test_spectra()
  
  # Test different intervals
  result_2 <- preprocess_spectra(test_data, resample_interval = 2, verbose = FALSE)
  result_8 <- preprocess_spectra(test_data, resample_interval = 8, verbose = FALSE)
  
  # Should have different numbers of spectral columns
  n_cols_2 <- ncol(result_2) - 1  # Exclude Sample_ID
  n_cols_8 <- ncol(result_8) - 1
  
  expect_gt(n_cols_2, n_cols_8)  # Smaller interval = more columns
  
  # Check actual intervals
  wn_2 <- as.numeric(setdiff(names(result_2), "Sample_ID"))
  wn_8 <- as.numeric(setdiff(names(result_8), "Sample_ID"))
  
  expect_true(all(abs(diff(sort(wn_2, decreasing = TRUE)) - 2) < 0.01))
  expect_true(all(abs(diff(sort(wn_8, decreasing = TRUE)) - 8) < 0.01))
})

# Baseline Correction Tests ----------------------------------------------------

test_that("preprocess_spectra handles baseline correction methods", {
  test_data <- setup_test_spectra()
  
  # Test no baseline correction
  result_none <- preprocess_spectra(
    test_data,
    baseline_method = "none",
    verbose = FALSE
  )
  
  expect_s3_class(result_none, "data.frame")
  
  # Test rubberband baseline (mock prospectr functions)
  with_mocked_bindings(
    `prospectr::baseline` = function(X, wav) {
      # Mock successful baseline correction
      return(X * 0.8)  # Simulate baseline-corrected data
    },
    {
      result_rubber <- preprocess_spectra(
        test_data,
        baseline_method = "rubberband",
        verbose = FALSE
      )
      
      expect_s3_class(result_rubber, "data.frame")
    }
  )
  
  # Test polynomial baseline
  with_mocked_bindings(
    `prospectr::detrend` = function(X, wav, p) {
      # Mock successful polynomial detrending
      return(X * 0.9)
    },
    {
      result_poly <- preprocess_spectra(
        test_data,
        baseline_method = "polynomial",
        verbose = FALSE
      )
      
      expect_s3_class(result_poly, "data.frame")
    }
  )
})

test_that("preprocess_spectra handles baseline correction failures", {
  test_data <- setup_test_spectra()
  
  # Mock prospectr::baseline to fail
  with_mocked_bindings(
    `prospectr::baseline` = function(X, wav) {
      stop("Baseline correction failed")
    },
    {
      expect_error(
        preprocess_spectra(test_data, baseline_method = "rubberband", verbose = FALSE),
        "Baseline correction failed"
      )
    }
  )
  
  # Mock prospectr::detrend to fail
  with_mocked_bindings(
    `prospectr::detrend` = function(X, wav, p) {
      stop("Polynomial detrending failed")
    },
    {
      expect_error(
        preprocess_spectra(test_data, baseline_method = "polynomial", verbose = FALSE),
        "Baseline correction failed"
      )
    }
  )
})

# Resampling Tests --------------------------------------------------------------

test_that("preprocess_spectra performs resampling correctly", {
  test_data <- setup_test_spectra()
  
  # Mock prospectr::resample
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      # Simple linear interpolation mock
      n_samples <- nrow(X)
      n_new_wav <- length(new.wav)
      return(matrix(runif(n_samples * n_new_wav, 0, 1), 
                   nrow = n_samples, ncol = n_new_wav))
    },
    {
      result <- preprocess_spectra(test_data, resample_interval = 4, verbose = FALSE)
      
      expect_s3_class(result, "data.frame")
      
      # Check that resampling was applied
      spectral_cols <- setdiff(names(result), "Sample_ID")
      wn_values <- as.numeric(spectral_cols)
      expect_true(all(wn_values >= 600 & wn_values <= 4000))
    }
  )
})

test_that("preprocess_spectra handles resampling failures", {
  test_data <- setup_test_spectra()
  
  # Mock prospectr::resample to fail
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      stop("Resampling failed")
    },
    {
      expect_error(
        preprocess_spectra(test_data, verbose = FALSE),
        "Spectral resampling failed"
      )
    }
  )
})

# Data Validation Tests ---------------------------------------------------------

test_that("preprocess_spectra validates spectral data quality", {
  test_data <- setup_test_spectra()
  
  # Add problematic data
  test_data_problems <- test_data
  spectral_cols <- setdiff(names(test_data), "Sample_ID")
  
  # Add infinite values
  test_data_problems[1, spectral_cols[1]] <- Inf
  test_data_problems[2, spectral_cols[2]] <- -Inf
  
  # Add NaN values
  test_data_problems[3, spectral_cols[3]] <- NaN
  
  # Add NA values
  test_data_problems[4, spectral_cols[4]] <- NA
  
  # Mock resampling to return the problematic data
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(X)  # Return input unchanged for testing
    },
    {
      expect_warning(
        preprocess_spectra(test_data_problems, verbose = FALSE),
        "Non-finite values detected"
      )
    }
  )
})

test_that("preprocess_spectra detects zero spectra", {
  test_data <- setup_test_spectra()
  
  # Create all-zero spectrum
  spectral_cols <- setdiff(names(test_data), "Sample_ID")
  test_data[1, spectral_cols] <- 0
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(X)
    },
    {
      expect_warning(
        preprocess_spectra(test_data, verbose = FALSE),
        "Found .* spectra with all zero values"
      )
    }
  )
})

test_that("preprocess_spectra validates non-numeric column names", {
  test_data <- setup_test_spectra()
  
  # Add non-numeric column
  test_data$non_numeric_col <- 1
  
  expect_error(
    preprocess_spectra(test_data, verbose = FALSE),
    "Non-numeric column names found"
  )
})

# Spectra Type Handling --------------------------------------------------------

test_that("preprocess_spectra handles different spectra types correctly", {
  
  # Test MIR data
  mir_data <- setup_test_spectra()
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      expect_true(min(new.wav) >= 600)
      expect_true(max(new.wav) <= 4000)
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      result_mir <- preprocess_spectra(mir_data, verbose = FALSE)
      expect_s3_class(result_mir, "data.frame")
    }
  )
  
  # Test NIR data
  nir_data <- setup_nir_spectra()
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      # For NIR, should use actual data range, not fixed range
      original_range <- range(as.numeric(wav))
      expect_true(min(new.wav) >= floor(original_range[1]))
      expect_true(max(new.wav) <= ceiling(original_range[2]))
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      result_nir <- preprocess_spectra(nir_data, verbose = FALSE)
      expect_s3_class(result_nir, "data.frame")
    }
  )
})

test_that("preprocess_spectra handles missing spectra_type attribute", {
  test_data <- setup_test_spectra()
  attr(test_data, "spectra_type") <- NULL
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      expect_warning(
        preprocess_spectra(test_data, verbose = FALSE),
        "No spectra_type attribute found, assuming MIR"
      )
    }
  )
})

test_that("preprocess_spectra handles unknown spectra types", {
  test_data <- setup_test_spectra()
  attr(test_data, "spectra_type") <- "UNKNOWN"
  
  expect_error(
    preprocess_spectra(test_data, verbose = FALSE),
    "Unknown spectra_type"
  )
})

# Memory Management Tests -------------------------------------------------------

test_that("preprocess_spectra handles memory warnings", {
  # Create large dataset that would trigger memory warning
  large_data <- setup_test_spectra()
  
  # Simulate large dataset by mocking the memory calculation
  with_mocked_bindings(
    `preprocess_spectra` = function(...) {
      # Call the real function but expect memory warning
      horizons::preprocess_spectra(...)
    },
    {
      # For very large datasets, expect memory warning
      # Note: This is difficult to test without actually creating huge datasets
      skip("Memory warning tests require very large datasets")
    }
  )
})

# Verbose Output Tests ----------------------------------------------------------

test_that("preprocess_spectra produces appropriate verbose output", {
  test_data <- setup_test_spectra()
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      expect_message(
        preprocess_spectra(test_data, verbose = TRUE),
        "Processing .* spectra"
      )
      
      expect_message(
        preprocess_spectra(test_data, verbose = TRUE),
        "Original range:"
      )
      
      expect_message(
        preprocess_spectra(test_data, verbose = TRUE),
        "Resampling to .* points"
      )
      
      expect_message(
        preprocess_spectra(test_data, verbose = TRUE),
        "Preprocessing complete:"
      )
    }
  )
})

test_that("preprocess_spectra verbose output for baseline correction", {
  test_data <- setup_test_spectra()
  
  with_mocked_bindings(
    `prospectr::baseline` = function(X, wav) return(X),
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      expect_message(
        preprocess_spectra(test_data, baseline_method = "rubberband", verbose = TRUE),
        "Applying rubberband baseline correction"
      )
    }
  )
})

# Edge Cases Tests --------------------------------------------------------------

test_that("preprocess_spectra handles minimal datasets", {
  # Single sample
  single_sample <- setup_test_spectra()[1, ]
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(length(new.wav)), nrow = 1))
    },
    {
      result <- preprocess_spectra(single_sample, verbose = FALSE)
      expect_equal(nrow(result), 1)
    }
  )
  
  # Few spectral columns
  few_cols <- setup_test_spectra()[, c("Sample_ID", "4000", "3000", "2000")]
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      result <- preprocess_spectra(few_cols, verbose = FALSE)
      expect_s3_class(result, "data.frame")
    }
  )
})

# Integration Tests -------------------------------------------------------------

test_that("preprocess_spectra integrates with read_spectra output", {
  # Use actual test CSV if available
  test_csv <- file.path(test_path("fixtures"), "test_spectra.csv")
  
  skip_if_not(file.exists(test_csv), "Test CSV file not found")
  
  # Read spectra and then preprocess
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_csv,
    verbose = FALSE
  )
  
  # Convert wavenumber column names from wn_XXXX to XXXX format
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      result <- preprocess_spectra(spectra, verbose = FALSE)
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), nrow(spectra))
    }
  )
})

# Performance Tests -------------------------------------------------------------

test_that("preprocess_spectra completes in reasonable time", {
  test_data <- setup_test_spectra()
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      start_time <- Sys.time()
      result <- preprocess_spectra(test_data, verbose = FALSE)
      end_time <- Sys.time()
      
      # Should complete quickly for test data
      expect_lt(as.numeric(end_time - start_time), 5)
    }
  )
})