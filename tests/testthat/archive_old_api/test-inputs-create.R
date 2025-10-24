#' Tests for create_dataset() Function
#'
#' Comprehensive test suite for creating modeling datasets by joining spectral 
#' data with response variables, handling ID parsing and replicate aggregation

# Setup test data --------------------------------------------------------------

setup_test_spectra_for_create <- function() {
  # Create spectral data with realistic Sample_IDs for testing
  wavenumbers <- c("4000", "3500", "3000", "2500", "2000", "1500", "1000")
  
  spectra_data <- data.frame(
    Sample_ID = c(
      "PROJ_001_bulk_scan1",
      "PROJ_001_bulk_scan2", 
      "PROJ_002_bulk_scan1",
      "PROJ_003_fraction_scan1",
      "PROJ_004_bulk_scan1"
    ),
    stringsAsFactors = FALSE
  )
  
  # Add spectral columns with realistic values
  for (wn in wavenumbers) {
    spectra_data[[wn]] <- runif(nrow(spectra_data), 0.1, 1.0)
  }
  
  return(spectra_data)
}

setup_simple_spectra <- function() {
  # Simple spectra without complex IDs
  data.frame(
    Sample_ID = c("sample1", "sample2", "sample3"),
    `4000` = c(0.5, 0.6, 0.7),
    `3000` = c(0.4, 0.5, 0.6),
    `2000` = c(0.3, 0.4, 0.5),
    `1000` = c(0.2, 0.3, 0.4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

setup_test_response_data <- function() {
  data.frame(
    Sample_ID = c("PROJ_001", "PROJ_002", "PROJ_003", "PROJ_004", "PROJ_005"),
    SOC = c(2.5, 3.1, 1.8, 2.2, 2.9),
    pH = c(6.8, 7.1, 6.5, 6.9, 7.3),
    Clay = c(25, 30, 20, 28, 32),
    Latitude = c(40.123, 40.124, 40.125, 40.126, 40.127),
    Longitude = c(-105.234, -105.235, -105.236, -105.237, -105.238),
    stringsAsFactors = FALSE
  )
}

setup_simple_response <- function() {
  data.frame(
    Sample_ID = c("sample1", "sample2", "sample3"),
    SOC = c(2.1, 2.5, 1.9),
    pH = c(6.5, 7.0, 6.8),
    stringsAsFactors = FALSE
  )
}

# Input Validation Tests -------------------------------------------------------

test_that("create_dataset validates input parameters correctly", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  # Test spectra_data validation
  expect_error(
    create_dataset("not_dataframe", response),
    "spectra_data must be a data frame or tibble"
  )
  
  # Test missing Sample_ID in spectra
  spectra_no_id <- spectra
  spectra_no_id$Sample_ID <- NULL
  
  expect_error(
    create_dataset(spectra_no_id, response),
    "Column.*Sample_ID.*not found in spectra_data"
  )
  
  # Test response_data as file path (non-existent file)
  expect_error(
    create_dataset(spectra, "nonexistent_file.csv"),
    "Response file not found"
  )
  
  # Test missing Sample_ID in response
  response_no_id <- response
  response_no_id$Sample_ID <- NULL
  
  expect_error(
    create_dataset(spectra, response_no_id),
    "Column.*Sample_ID.*not found in response_data"
  )
  
  # Test parse_ids without id_format
  expect_error(
    create_dataset(spectra, response, parse_ids = TRUE),
    "id_format must be provided when parse_ids = TRUE"
  )
  
  # Test aggregate_by without parse_ids
  expect_error(
    create_dataset(spectra, response, aggregate_by = c("sampleid")),
    "aggregate_by requires parse_ids = TRUE"
  )
  
  # Test invalid join_type
  expect_error(
    create_dataset(spectra, response, join_type = "invalid"),
    "should be one of"
  )
})

# Basic Dataset Creation Tests -------------------------------------------------

test_that("create_dataset creates basic dataset correctly", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  result <- create_dataset(
    spectra_data = spectra,
    response_data = response,
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true("Sample_ID" %in% names(result))
  
  # Check dimensions
  expect_equal(nrow(result), 3)  # All samples match
  expect_gt(ncol(result), ncol(response))  # Should include spectral columns
  
  # Check that both spectral and response columns are present
  expect_true(all(c("SOC", "pH") %in% names(result)))
  expect_true(all(c("4000", "3000", "2000", "1000") %in% names(result)))
})

test_that("create_dataset handles different join types", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  # Add sample not in spectra to response
  response_extra <- rbind(response, data.frame(Sample_ID = "sample4", SOC = 3.0, pH = 7.2))
  
  # Add sample not in response to spectra  
  spectra_extra <- rbind(spectra, data.frame(
    Sample_ID = "sample5",
    `4000` = 0.8, `3000` = 0.7, `2000` = 0.6, `1000` = 0.5,
    check.names = FALSE
  ))
  
  # Inner join (default)
  result_inner <- create_dataset(spectra_extra, response_extra, join_type = "inner", verbose = FALSE)
  expect_equal(nrow(result_inner), 3)  # Only matching samples
  
  # Left join
  result_left <- create_dataset(spectra_extra, response_extra, join_type = "left", verbose = FALSE)
  expect_equal(nrow(result_left), 4)  # All spectra samples
  
  # Right join
  result_right <- create_dataset(spectra_extra, response_extra, join_type = "right", verbose = FALSE)
  expect_equal(nrow(result_right), 4)  # All response samples
  
  # Full join
  result_full <- create_dataset(spectra_extra, response_extra, join_type = "full", verbose = FALSE)
  expect_equal(nrow(result_full), 5)  # All samples from both
})

# Response Data Handling Tests -------------------------------------------------

test_that("create_dataset reads response data from CSV file", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  # Write response to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(response, temp_csv, row.names = FALSE)
  
  result <- create_dataset(
    spectra_data = spectra,
    response_data = temp_csv,  # Use file path
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("SOC", "pH") %in% names(result)))
  
  unlink(temp_csv)
})

test_that("create_dataset handles response data reading errors", {
  spectra <- setup_simple_spectra()
  
  # Create malformed CSV
  temp_csv <- tempfile(fileext = ".csv")
  writeLines("invalid,csv,content\nno,proper,structure", temp_csv)
  
  expect_error(
    create_dataset(spectra, temp_csv, verbose = FALSE),
    "Column.*Sample_ID.*not found in response_data"
  )
  
  unlink(temp_csv)
})

test_that("create_dataset filters response variables correctly", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  # Select only SOC
  result <- create_dataset(
    spectra_data = spectra,
    response_data = response,
    response_variables = "SOC",
    verbose = FALSE
  )
  
  expect_true("SOC" %in% names(result))
  expect_false("pH" %in% names(result))
  
  # Select multiple variables
  result_multi <- create_dataset(
    spectra_data = spectra,
    response_data = response,
    response_variables = c("SOC", "pH"),
    verbose = FALSE
  )
  
  expect_true(all(c("SOC", "pH") %in% names(result_multi)))
  
  # Warn about missing variables
  expect_warning(
    create_dataset(spectra, response, response_variables = c("SOC", "NonExistent"), verbose = FALSE),
    "Variables not found in response data"
  )
})

# ID Parsing Tests -------------------------------------------------------------

test_that("create_dataset parses IDs correctly", {
  spectra <- setup_test_spectra_for_create()
  response <- setup_test_response_data()
  
  # Mock parse_filename_metadata
  with_mocked_bindings(
    `parse_filename_metadata` = function(file_name, format_string, delimiter, default_fraction) {
      # Parse based on expected format
      parts <- strsplit(file_name, "_")[[1]]
      return(data.frame(
        Project = parts[1],
        Sample_ID = paste(parts[1], parts[2], sep = "_"),
        Fraction = parts[3],
        Scan = parts[4],
        stringsAsFactors = FALSE
      ))
    },
    {
      result <- create_dataset(
        spectra_data = spectra,
        response_data = response,
        parse_ids = TRUE,
        id_format = "project_sampleid_fraction_scan",
        verbose = FALSE
      )
      
      expect_s3_class(result, "data.frame")
      expect_true("Sample_ID" %in% names(result))
      expect_true("Project" %in% names(result))
      expect_true("Fraction" %in% names(result))
    }
  )
})

test_that("create_dataset handles ID parsing errors", {
  spectra <- setup_test_spectra_for_create()
  response <- setup_test_response_data()
  
  # Mock parse_filename_metadata to fail
  with_mocked_bindings(
    `parse_filename_metadata` = function(...) {
      stop("ID parsing failed")
    },
    {
      expect_error(
        create_dataset(
          spectra, response,
          parse_ids = TRUE,
          id_format = "project_sampleid_fraction_scan",
          verbose = FALSE
        ),
        "ID parsing failed"
      )
    }
  )
})

# Replicate Aggregation Tests --------------------------------------------------

test_that("create_dataset aggregates replicates correctly", {
  # Create data with replicates
  spectra_with_reps <- data.frame(
    Sample_ID = c("sample1", "sample1", "sample2", "sample3"),  # sample1 has replicate
    `4000` = c(0.5, 0.6, 0.7, 0.8),  # Different values for replicates
    `3000` = c(0.4, 0.5, 0.6, 0.7),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  response <- data.frame(
    Sample_ID = c("sample1", "sample2", "sample3"),
    SOC = c(2.1, 2.5, 1.9),
    stringsAsFactors = FALSE
  )
  
  result <- create_dataset(
    spectra_data = spectra_with_reps,
    response_data = response,
    verbose = FALSE
  )
  
  # Should have 3 unique samples after aggregation
  expect_equal(nrow(result), 3)
  
  # Check that replicates were averaged
  sample1_4000 <- result[result$Sample_ID == "sample1", "4000"][[1]]
  expect_equal(sample1_4000, mean(c(0.5, 0.6)))  # Average of replicates
  
  # Should have n_replicates column
  expect_true("n_replicates" %in% names(result))
  expect_equal(result[result$Sample_ID == "sample1", "n_replicates"][[1]], 2)
  expect_equal(result[result$Sample_ID == "sample2", "n_replicates"][[1]], 1)
})

test_that("create_dataset handles custom aggregation columns", {
  spectra <- setup_test_spectra_for_create()
  response <- setup_test_response_data()
  
  # Mock ID parsing to return parsed components
  with_mocked_bindings(
    `parse_filename_metadata` = function(file_name, ...) {
      parts <- strsplit(file_name, "_")[[1]]
      return(data.frame(
        Project = parts[1],
        Sample_ID = paste(parts[1], parts[2], sep = "_"),
        Fraction = parts[3],
        Scan = parts[4],
        stringsAsFactors = FALSE
      ))
    },
    {
      result <- create_dataset(
        spectra_data = spectra,
        response_data = response,
        parse_ids = TRUE,
        id_format = "project_sampleid_fraction_scan",
        aggregate_by = c("Sample_ID", "Fraction"),  # Aggregate by sample and fraction
        verbose = FALSE
      )
      
      expect_s3_class(result, "data.frame")
      
      # Should aggregate scans but keep fractions separate
      # PROJ_001 has 2 scans of same fraction - should be aggregated
      proj_001_rows <- result[grepl("PROJ_001", result$Sample_ID), ]
      expect_equal(nrow(proj_001_rows), 1)
    }
  )
})

# Coordinate Handling Tests ----------------------------------------------------

test_that("create_dataset includes coordinates correctly", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  # Add coordinates to response
  response$Latitude <- c(40.1, 40.2, 40.3)
  response$Longitude <- c(-105.1, -105.2, -105.3)
  
  result <- create_dataset(
    spectra_data = spectra,
    response_data = response,
    include_coords = TRUE,
    verbose = FALSE
  )
  
  expect_true(all(c("Latitude", "Longitude") %in% names(result)))
})

test_that("create_dataset auto-detects coordinate columns", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  # Add coordinates with different naming conventions
  response$lat <- c(40.1, 40.2, 40.3)
  response$lon <- c(-105.1, -105.2, -105.3)
  response$X <- c(500000, 500100, 500200)
  response$Y <- c(4400000, 4400100, 4400200)
  
  expect_message(
    result <- create_dataset(spectra, response, include_coords = TRUE, verbose = TRUE),
    "Auto-detected coordinate columns"
  )
  
  expect_true(all(c("lat", "lon", "X", "Y") %in% names(result)))
})

test_that("create_dataset handles explicit coordinate columns", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  response$custom_lat <- c(40.1, 40.2, 40.3)
  response$custom_lon <- c(-105.1, -105.2, -105.3)
  
  result <- create_dataset(
    spectra_data = spectra,
    response_data = response,
    include_coords = TRUE,
    coord_columns = c("custom_lat", "custom_lon"),
    verbose = FALSE
  )
  
  expect_true(all(c("custom_lat", "custom_lon") %in% names(result)))
})

# NA Handling Tests ------------------------------------------------------------

test_that("create_dataset handles NA values correctly", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  # Add NAs to response
  response$SOC[2] <- NA
  
  # Test with drop_na = TRUE (default)
  result_drop <- create_dataset(
    spectra_data = spectra,
    response_data = response,
    response_variables = "SOC",
    drop_na = TRUE,
    verbose = FALSE
  )
  
  expect_equal(nrow(result_drop), 2)  # Should drop row with NA
  
  # Test with drop_na = FALSE
  result_keep <- create_dataset(
    spectra_data = spectra,
    response_data = response,
    response_variables = "SOC",
    drop_na = FALSE,
    verbose = FALSE
  )
  
  expect_equal(nrow(result_keep), 3)  # Should keep all rows
  expect_true(is.na(result_keep$SOC[result_keep$Sample_ID == "sample2"]))
})

test_that("create_dataset warns about dropped NA rows", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  response$SOC[1] <- NA
  
  expect_message(
    create_dataset(spectra, response, response_variables = "SOC", verbose = TRUE),
    "Dropped .* rows with NA in response variables"
  )
})

# Verbose Output Tests ----------------------------------------------------------

test_that("create_dataset produces appropriate verbose output", {
  spectra <- setup_simple_spectra()
  response <- setup_simple_response()
  
  expect_message(
    create_dataset(spectra, response, verbose = TRUE),
    "Joining spectra with response data"
  )
  
  expect_message(
    create_dataset(spectra, response, verbose = TRUE),
    "Created dataset with"
  )
})

test_that("create_dataset reports join statistics", {
  spectra <- rbind(setup_simple_spectra(), 
                   data.frame(Sample_ID = "sample4", `4000` = 0.8, `3000` = 0.7, 
                             `2000` = 0.6, `1000` = 0.5, check.names = FALSE))
  response <- rbind(setup_simple_response(),
                   data.frame(Sample_ID = "sample5", SOC = 3.0, pH = 7.2))
  
  expect_message(
    create_dataset(spectra, response, verbose = TRUE),
    "spectra samples had no matching response data"
  )
  
  expect_message(
    create_dataset(spectra, response, verbose = TRUE),
    "response samples had no matching spectra"
  )
})

# Edge Cases Tests --------------------------------------------------------------

test_that("create_dataset handles empty datasets", {
  empty_spectra <- setup_simple_spectra()[0, ]
  response <- setup_simple_response()
  
  result <- create_dataset(empty_spectra, response, verbose = FALSE)
  expect_equal(nrow(result), 0)
})

test_that("create_dataset handles single sample", {
  single_spectra <- setup_simple_spectra()[1, ]
  single_response <- setup_simple_response()[1, ]
  
  result <- create_dataset(single_spectra, single_response, verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_true(all(c("SOC", "pH") %in% names(result)))
})

test_that("create_dataset handles duplicate sample IDs in response", {
  spectra <- setup_simple_spectra()
  response <- rbind(setup_simple_response(), setup_simple_response()[1, ])
  
  # Should still work, but may produce unexpected results
  # This tests robustness rather than correct behavior
  result <- create_dataset(spectra, response, verbose = FALSE)
  expect_s3_class(result, "data.frame")
})

# Integration Tests -------------------------------------------------------------

test_that("create_dataset works with preprocess_spectra output", {
  # Create data in format that preprocess_spectra would produce
  preprocessed_spectra <- data.frame(
    Sample_ID = c("sample1", "sample2", "sample3"),
    `600` = c(0.1, 0.2, 0.3),
    `602` = c(0.11, 0.21, 0.31),
    `604` = c(0.12, 0.22, 0.32),
    `606` = c(0.13, 0.23, 0.33),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  response <- setup_simple_response()
  
  result <- create_dataset(preprocessed_spectra, response, verbose = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("600", "602", "604", "606") %in% names(result)))
})

test_that("create_dataset works with real test fixtures", {
  # Test with actual fixture files if available
  test_csv <- file.path(test_path("fixtures"), "test_spectra.csv")
  response_csv <- file.path(test_path("fixtures"), "test_response.csv")
  
  skip_if_not(file.exists(test_csv), "Test spectra CSV not found")
  skip_if_not(file.exists(response_csv), "Test response CSV not found")
  
  # Read the test data
  spectra <- read.csv(test_csv, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Convert column names from wn_XXXX to XXXX
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  # First check what's available in the response file
  response_data <- read.csv(response_csv, stringsAsFactors = FALSE)
  available_response_vars <- setdiff(names(response_data), "Sample_ID")
  
  result <- create_dataset(
    spectra_data = spectra,
    response_data = response_csv,
    response_variables = available_response_vars[1:min(2, length(available_response_vars))],
    include_coords = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("Sample_ID" %in% names(result))
  expect_gt(ncol(result), 5)  # Should have Sample_ID + spectral columns + some response columns
})

# Performance Tests -------------------------------------------------------------

test_that("create_dataset completes in reasonable time", {
  # Create larger dataset for performance testing
  n_samples <- 100
  spectra_large <- data.frame(
    Sample_ID = paste0("sample_", 1:n_samples),
    stringsAsFactors = FALSE
  )
  
  # Add spectral columns
  for (i in 1:50) {
    col_name <- as.character(4000 - i*10)
    spectra_large[[col_name]] <- runif(n_samples, 0, 1)
  }
  
  response_large <- data.frame(
    Sample_ID = paste0("sample_", 1:n_samples),
    SOC = runif(n_samples, 1, 5),
    pH = runif(n_samples, 5, 8),
    stringsAsFactors = FALSE
  )
  
  start_time <- Sys.time()
  result <- create_dataset(spectra_large, response_large, verbose = FALSE)
  end_time <- Sys.time()
  
  expect_lt(as.numeric(end_time - start_time), 10)  # Should complete within 10 seconds
  expect_equal(nrow(result), n_samples)
})