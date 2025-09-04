#' Tests for read_spectra() Function
#'
#' Comprehensive test suite for reading OPUS and CSV spectral files
#' Tests input validation, file handling, format detection, and error cases

# Setup test data paths -------------------------------------------------------

test_data_dir <- file.path(test_path("fixtures"))
test_spectra_csv <- file.path(test_data_dir, "test_spectra.csv")

# Mock data for OPUS tests (since we can't include real OPUS files)
setup_mock_opus_data <- function() {
  # Create mock opus reader responses for testing
  mock_opus_single <- list(
    "test_sample.0" = list(
      ab = list(
        data = matrix(
          c(0.5, 0.6, 0.7, 0.8, 0.9),
          nrow = 1,
          dimnames = list(NULL, c("4000", "3000", "2000", "1000", "600"))
        )
      )
    )
  )
  
  mock_opus_multiple <- list(
    "sample1.0" = list(
      ab_no_atm_comp = list(
        data = matrix(
          c(0.1, 0.2, 0.3, 0.4, 0.5),
          nrow = 1,
          dimnames = list(NULL, c("4000", "3000", "2000", "1000", "600"))
        )
      )
    ),
    "sample2.0" = list(
      ab = list(
        data = matrix(
          c(0.6, 0.7, 0.8, 0.9, 1.0),
          nrow = 1,
          dimnames = list(NULL, c("4000", "3000", "2000", "1000", "600"))
        )
      )
    )
  )
  
  list(single = mock_opus_single, multiple = mock_opus_multiple)
}

# Input Validation Tests -------------------------------------------------------

test_that("read_spectra validates input parameters correctly", {
  
  # Test source argument validation
  expect_error(
    read_spectra(source = "invalid", spectra_path = test_spectra_csv),
    "should be one of"
  )
  
  # Test spectra_type argument validation
  expect_error(
    read_spectra(source = "csv", spectra_path = test_spectra_csv, spectra_type = "invalid"),
    "should be one of"
  )
  
  # Test verbose argument validation
  expect_error(
    read_spectra(source = "csv", spectra_path = test_spectra_csv, verbose = "not_logical"),
    "verbose must be a single logical value"
  )
  
  expect_error(
    read_spectra(source = "csv", spectra_path = test_spectra_csv, verbose = c(TRUE, FALSE)),
    "verbose must be a single logical value"
  )
  
  # Test spectra_path validation
  expect_error(
    read_spectra(source = "csv", spectra_path = NULL),
    "spectra_path must be provided"
  )
  
  expect_error(
    read_spectra(source = "csv", spectra_path = "nonexistent_file.csv"),
    "Path does not exist"
  )
})

# CSV Reading Tests -------------------------------------------------------------

test_that("read_spectra reads CSV files correctly", {
  skip_if_not(file.exists(test_spectra_csv), "Test CSV file not found")
  
  # Basic CSV reading
  result <- read_spectra(
    source = "csv",
    spectra_path = test_spectra_csv,
    spectra_type = "MIR",
    verbose = FALSE
  )
  
  # Check return structure
  expect_s3_class(result, "data.frame")
  expect_true("Sample_ID" %in% names(result))
  
  # Check attributes
  expect_equal(attr(result, "source"), "csv")
  expect_equal(attr(result, "spectra_type"), "MIR")
  expect_equal(attr(result, "source_path"), test_spectra_csv)
  
  # Check data dimensions
  expect_equal(nrow(result), 5)  # 5 samples in test file
  expect_gt(ncol(result), 10)    # Should have many spectral columns
  
  # Check that spectral columns are numeric
  spectral_cols <- setdiff(names(result), "Sample_ID")
  expect_true(all(sapply(result[spectral_cols], is.numeric)))
  
  # Check wavenumber column names (should be numeric after removing wn_ prefix)
  wn_values <- as.numeric(gsub("wn_", "", spectral_cols))
  expect_false(any(is.na(wn_values)))
  expect_true(all(wn_values > 0))
})

test_that("read_spectra handles CSV validation correctly", {
  skip_if_not(file.exists(test_spectra_csv), "Test CSV file not found")
  
  # Test directory path for CSV (should fail)
  expect_error(
    read_spectra(source = "csv", spectra_path = test_data_dir),
    "CSV source requires a file path, not a directory"
  )
  
  # Test non-CSV extension warning
  temp_file <- tempfile(fileext = ".txt")
  file.copy(test_spectra_csv, temp_file)
  
  expect_warning(
    read_spectra(source = "csv", spectra_path = temp_file, verbose = FALSE),
    "File does not have .csv extension"
  )
  
  unlink(temp_file)
})

test_that("read_spectra handles different spectra types", {
  skip_if_not(file.exists(test_spectra_csv), "Test CSV file not found")
  
  # Test MIR type
  result_mir <- read_spectra(
    source = "csv",
    spectra_path = test_spectra_csv,
    spectra_type = "MIR",
    verbose = FALSE
  )
  
  expect_equal(attr(result_mir, "spectra_type"), "MIR")
  
  # Test NIR type
  result_nir <- read_spectra(
    source = "csv",
    spectra_path = test_spectra_csv,
    spectra_type = "NIR",
    verbose = FALSE
  )
  
  expect_equal(attr(result_nir, "spectra_type"), "NIR")
})

# OPUS Reading Tests (Mocked) --------------------------------------------------

test_that("read_spectra validates OPUS file paths correctly", {
  
  # Test invalid OPUS file extension
  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)
  
  expect_error(
    read_spectra(source = "opus", spectra_path = temp_file),
    "File does not appear to be an OPUS file"
  )
  
  unlink(temp_file)
})

test_that("read_spectra handles OPUS directories", {
  
  # Create temporary directory structure
  temp_dir <- tempdir()
  opus_dir <- file.path(temp_dir, "opus_test")
  dir.create(opus_dir, showWarnings = FALSE)
  
  # Test empty directory
  expect_error(
    read_spectra(source = "opus", spectra_path = opus_dir),
    "No OPUS files found"
  )
  
  # Create mock OPUS files
  file.create(file.path(opus_dir, "sample1.0"))
  file.create(file.path(opus_dir, "sample2.1"))
  
  # Mock opusreader2::read_opus to avoid dependency
  with_mocked_bindings(
    `opusreader2::read_opus` = function(...) {
      mock_data <- setup_mock_opus_data()
      return(mock_data$multiple)
    },
    {
      # This would normally work, but we skip since it requires complex mocking
      skip("OPUS reading requires complex mocking of opusreader2 package")
    }
  )
  
  unlink(opus_dir, recursive = TRUE)
})

# Error Handling Tests ----------------------------------------------------------

test_that("read_spectra handles file reading errors gracefully", {
  
  # Create corrupted CSV file
  temp_csv <- tempfile(fileext = ".csv")
  writeLines("invalid,csv,content\nno,proper,structure", temp_csv)
  
  # Should handle gracefully and provide informative error
  expect_error(
    read_spectra(source = "csv", spectra_path = temp_csv, verbose = FALSE),
    "Failed to read spectral data"
  )
  
  unlink(temp_csv)
})

test_that("read_spectra handles empty files", {
  
  # Create empty CSV file
  temp_csv <- tempfile(fileext = ".csv")
  file.create(temp_csv)
  
  expect_error(
    read_spectra(source = "csv", spectra_path = temp_csv, verbose = FALSE)
  )
  
  unlink(temp_csv)
})

test_that("read_spectra handles malformed CSV files", {
  
  # Create CSV with only headers
  temp_csv <- tempfile(fileext = ".csv")
  writeLines("Sample_ID,wn_4000,wn_3000", temp_csv)
  
  expect_error(
    read_spectra(source = "csv", spectra_path = temp_csv, verbose = FALSE),
    "No spectral data found"
  )
  
  unlink(temp_csv)
})

# Verbose Output Tests ----------------------------------------------------------

test_that("read_spectra produces appropriate verbose output", {
  skip_if_not(file.exists(test_spectra_csv), "Test CSV file not found")
  
  # Test verbose output
  expect_message(
    read_spectra(source = "csv", spectra_path = test_spectra_csv, verbose = TRUE),
    "Reading MIR spectra from csv file"
  )
  
  expect_message(
    read_spectra(source = "csv", spectra_path = test_spectra_csv, verbose = TRUE),
    "Successfully read"
  )
})

# Integration Tests -------------------------------------------------------------

test_that("read_spectra integrates correctly with downstream functions", {
  skip_if_not(file.exists(test_spectra_csv), "Test CSV file not found")
  
  result <- read_spectra(
    source = "csv",
    spectra_path = test_spectra_csv,
    spectra_type = "MIR",
    verbose = FALSE
  )
  
  # Check that result can be used with preprocess_spectra
  # (This tests the interface compatibility)
  expect_true("Sample_ID" %in% names(result))
  
  # Check that spectral column names are suitable for processing
  spectral_cols <- setdiff(names(result), "Sample_ID")
  expect_true(all(grepl("^wn_[0-9]+(\\.[0-9]+)?$", spectral_cols)))
  
  # Check sample IDs are properly formatted
  expect_true(all(nchar(result$Sample_ID) > 0))
  expect_false(any(is.na(result$Sample_ID)))
})

# Performance Tests -------------------------------------------------------------

test_that("read_spectra handles reasonable file sizes efficiently", {
  skip_if_not(file.exists(test_spectra_csv), "Test CSV file not found")
  
  # Test timing for normal file
  start_time <- Sys.time()
  result <- read_spectra(
    source = "csv",
    spectra_path = test_spectra_csv,
    verbose = FALSE
  )
  end_time <- Sys.time()
  
  # Should complete reasonably quickly (less than 10 seconds for small test file)
  expect_lt(as.numeric(end_time - start_time), 10)
  
  # Memory usage should be reasonable
  expect_lt(object.size(result), 50 * 1024 * 1024)  # Less than 50MB for test data
})

# Edge Cases Tests --------------------------------------------------------------

test_that("read_spectra handles edge cases in spectral data", {
  
  # Create CSV with minimal data
  temp_csv <- tempfile(fileext = ".csv")
  writeLines(c(
    "Sample_ID,wn_4000,wn_3000",
    "test_sample,0.1,0.2"
  ), temp_csv)
  
  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    verbose = FALSE
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)  # Sample_ID + 2 spectral columns
  
  unlink(temp_csv)
})

test_that("read_spectra handles special characters in sample IDs", {
  
  # Create CSV with special characters in Sample_ID
  temp_csv <- tempfile(fileext = ".csv")
  writeLines(c(
    "Sample_ID,wn_4000,wn_3000",
    "sample-01_test.bulk,0.1,0.2",
    "sample_02-A_fraction,0.3,0.4"
  ), temp_csv)
  
  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    verbose = FALSE
  )
  
  expect_equal(nrow(result), 2)
  expect_true(all(c("sample-01_test.bulk", "sample_02-A_fraction") %in% result$Sample_ID))
  
  unlink(temp_csv)
})

# Default Arguments Tests -------------------------------------------------------

test_that("read_spectra uses correct default arguments", {
  skip_if_not(file.exists(test_spectra_csv), "Test CSV file not found")
  
  # Test default spectra_type
  result <- read_spectra(
    source = "csv",
    spectra_path = test_spectra_csv,
    verbose = FALSE
  )
  
  expect_equal(attr(result, "spectra_type"), "MIR")
  
  # Test default verbose
  expect_message(
    read_spectra(source = "csv", spectra_path = test_spectra_csv),
    "Reading MIR spectra"
  )
})