#' Integration Tests for Inputs Pipeline
#'
#' Comprehensive end-to-end testing of the complete inputs pipeline
#' from reading spectral files to creating finalized modeling datasets

# Setup test data --------------------------------------------------------------

setup_integration_test_data <- function() {
  # Create temporary directory for test files
  test_dir <- tempfile("horizons_integration_test")
  dir.create(test_dir, recursive = TRUE)
  
  # Create realistic spectral CSV data
  wavenumbers <- seq(4000, 600, by = -10)
  n_samples <- 15
  
  # Generate realistic spectral data with patterns and some outliers
  set.seed(42)  # Reproducible tests
  spectral_data <- data.frame(
    Sample_ID = c(
      # Regular samples with replicates
      "PROJ_001_bulk_scan1", "PROJ_001_bulk_scan2", "PROJ_001_bulk_scan3",
      "PROJ_002_bulk_scan1", "PROJ_002_bulk_scan2", 
      "PROJ_003_fraction_scan1", "PROJ_003_fraction_scan2",
      "PROJ_004_bulk_scan1",
      # Outlier samples
      "PROJ_005_bulk_scan1",  # Will be spectral outlier
      "PROJ_006_bulk_scan1",  # Will be response outlier
      # Additional samples
      "PROJ_007_bulk_scan1", "PROJ_008_bulk_scan1", 
      "PROJ_009_bulk_scan1", "PROJ_010_bulk_scan1", "PROJ_011_bulk_scan1"
    ),
    stringsAsFactors = FALSE
  )
  
  # Add spectral columns with realistic MIR patterns
  for (i in seq_along(wavenumbers)) {
    wn <- wavenumbers[i]
    
    # Base spectrum with typical MIR features
    base_absorption <- 0.3 + 0.4 * exp(-((wn - 1500)^2) / 500000) + 
                     0.2 * exp(-((wn - 2900)^2) / 100000) +  # C-H stretch
                     0.1 * exp(-((wn - 1050)^2) / 50000)     # Si-O stretch
    
    # Add sample-specific variation
    sample_values <- sapply(1:n_samples, function(j) {
      if (j == 9) {  # PROJ_005 - spectral outlier
        return(base_absorption * 3 + rnorm(1, 0, 0.1))  # Much higher absorption
      } else {
        return(pmax(0, base_absorption + rnorm(1, 0, 0.05)))  # Normal variation
      }
    })
    
    spectral_data[[paste0("wn_", wn)]] <- sample_values
  }
  
  # Write spectral CSV
  spectra_file <- file.path(test_dir, "test_spectra.csv")
  write.csv(spectral_data, spectra_file, row.names = FALSE)
  
  # Create response data
  response_data <- data.frame(
    Sample_ID = c("PROJ_001", "PROJ_002", "PROJ_003", "PROJ_004", "PROJ_005", 
                 "PROJ_006", "PROJ_007", "PROJ_008", "PROJ_009", "PROJ_010", 
                 "PROJ_011", "PROJ_012"),  # Extra sample to test join behavior
    SOC = c(2.1, 2.5, 1.8, 2.3, 2.7,   # Normal values
           50.0,                        # Response outlier (PROJ_006)
           2.0, 2.4, 1.9, 2.2, 2.6, 3.0),
    pH = c(6.5, 7.0, 6.3, 6.8, 7.2, 6.0, 6.7, 6.9, 6.4, 6.6, 7.1, 6.8),
    Clay = c(25, 30, 20, 28, 32, 22, 26, 29, 24, 27, 31, 23),
    Sand = c(45, 40, 55, 42, 35, 50, 44, 38, 48, 43, 36, 47),
    CEC = c(12.5, 15.2, 10.8, 14.1, 16.3, 11.2, 13.4, 15.8, 12.0, 13.9, 16.1, 12.7),
    Latitude = c(40.10, 40.12, 40.08, 40.15, 40.18, 40.05, 40.11, 40.14, 40.07, 40.13, 40.16, 40.09),
    Longitude = c(-105.20, -105.18, -105.25, -105.15, -105.12, -105.28, -105.22, -105.16, -105.24, -105.19, -105.13, -105.26),
    stringsAsFactors = FALSE
  )
  
  # Write response CSV
  response_file <- file.path(test_dir, "test_response.csv")
  write.csv(response_data, response_file, row.names = FALSE)
  
  return(list(
    test_dir = test_dir,
    spectra_file = spectra_file,
    response_file = response_file,
    spectral_data = spectral_data,
    response_data = response_data
  ))
}

# Full Pipeline Integration Tests ----------------------------------------------

test_that("complete inputs pipeline works end-to-end", {
  
  test_data <- setup_integration_test_data()
  on.exit(unlink(test_data$test_dir, recursive = TRUE))
  
  # Step 1: Read spectral data
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_data$spectra_file,
    spectra_type = "MIR",
    verbose = FALSE
  )
  
  expect_s3_class(spectra, "data.frame")
  expect_equal(nrow(spectra), 15)
  
  # Step 2: Preprocess spectra (mock the intensive functions)
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      # Simulate resampling to standard grid
      n_samples <- nrow(X)
      n_new_points <- length(new.wav)
      return(matrix(runif(n_samples * n_new_points, 0, 1), 
                   nrow = n_samples, ncol = n_new_points))
    },
    {
      # Convert wn_ prefixed columns to numeric column names
      names(spectra) <- gsub("^wn_", "", names(spectra))
      
      preprocessed <- preprocess_spectra(
        spectra_data = spectra,
        resample_interval = 4,
        baseline_method = "none",
        verbose = FALSE
      )
      
      expect_s3_class(preprocessed, "data.frame")
      expect_equal(nrow(preprocessed), 15)
    }
  )
  
  # Step 3: Create dataset with ID parsing and response joining
  with_mocked_bindings(
    `parse_filename_metadata` = function(file_name, format_string, delimiter, default_fraction) {
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
      # Use the original spectra for dataset creation (before preprocessing modifications)
      names(test_data$spectral_data) <- gsub("^wn_", "", names(test_data$spectral_data))
      
      dataset <- create_dataset(
        spectra_data = test_data$spectral_data,
        response_data = test_data$response_file,
        parse_ids = TRUE,
        id_format = "project_sampleid_fraction_scan",
        response_variables = c("SOC", "pH", "Clay"),
        include_coords = TRUE,
        verbose = FALSE
      )
      
      expect_s3_class(dataset, "data.frame")
      expect_true("SOC" %in% names(dataset))
      expect_true("pH" %in% names(dataset))
      expect_true("Clay" %in% names(dataset))
      expect_true("Latitude" %in% names(dataset))
      expect_true("n_replicates" %in% names(dataset))
      
      # Should have fewer rows due to replicate averaging
      expect_lt(nrow(dataset), nrow(test_data$spectral_data))
    }
  )
  
  # Step 4: Finalize with outlier detection
  with_mocked_bindings(
    `prcomp` = function(x, center = TRUE, scale. = FALSE) {
      n_obs <- nrow(x)
      n_pc <- 3
      # Create PC scores where sample 5 (PROJ_005) is an outlier
      pc_scores <- matrix(rnorm(n_obs * n_pc, 0, 1), nrow = n_obs, ncol = n_pc)
      if (n_obs >= 5) pc_scores[5, ] <- c(5, 5, 5)  # Make an outlier
      list(sdev = sqrt(c(3, 2, 1)), x = pc_scores)
    },
    `robustbase::covMcd` = function(x) {
      list(center = c(0, 0, 0), cov = diag(3))
    },
    {
      # Create a simple dataset for finalization
      final_dataset_input <- data.frame(
        Sample_ID = c("PROJ_001", "PROJ_002", "PROJ_003", "PROJ_004", "PROJ_005", "PROJ_006"),
        `4000` = c(0.5, 0.6, 0.7, 0.8, 2.5, 0.4),  # PROJ_005 is spectral outlier
        `3000` = c(0.4, 0.5, 0.6, 0.7, 2.0, 0.3),
        `2000` = c(0.3, 0.4, 0.5, 0.6, 1.5, 0.2),
        SOC = c(2.1, 2.5, 1.8, 2.3, 2.7, 50.0),    # PROJ_006 is response outlier
        pH = c(6.5, 7.0, 6.3, 6.8, 7.2, 6.0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      finalized <- finalize_dataset(
        dataset = final_dataset_input,
        response_variable = "SOC",
        spectral_outlier_method = "mahalanobis",
        detect_response_outliers = TRUE,
        remove_outliers = FALSE,  # Keep outliers but flag them
        verbose = FALSE
      )
      
      expect_s3_class(finalized, "data.frame")
      expect_true("outlier_flag" %in% names(finalized))
      expect_equal(nrow(finalized), 6)  # All samples retained
      expect_true(any(finalized$outlier_flag == "outlier"))  # Some outliers detected
    }
  )
})

test_that("pipeline handles different data scenarios correctly", {
  
  test_data <- setup_integration_test_data()
  on.exit(unlink(test_data$test_dir, recursive = TRUE))
  
  # Test scenario: MIR spectra with minimal preprocessing
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_data$spectra_file,
    spectra_type = "MIR",
    verbose = FALSE
  )
  
  # Convert column names for processing
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      # Simple mock that maintains relative data patterns
      return(X[, 1:min(ncol(X), length(new.wav))])  # Truncate to new wavelength range
    },
    {
      # Minimal preprocessing
      preprocessed <- preprocess_spectra(
        spectra_data = spectra,
        resample_interval = 8,  # Coarser resampling
        baseline_method = "none",
        verbose = FALSE
      )
      
      # Simple dataset creation without ID parsing
      dataset <- create_dataset(
        spectra_data = preprocessed,
        response_data = test_data$response_file,
        parse_ids = FALSE,
        response_variables = "SOC",
        include_coords = FALSE,
        verbose = FALSE
      )
      
      expect_s3_class(dataset, "data.frame")
      expect_true("SOC" %in% names(dataset))
      expect_false("Latitude" %in% names(dataset))  # Coordinates excluded
    }
  )
})

# Error Propagation Tests -------------------------------------------------------

test_that("pipeline handles errors gracefully at different stages", {
  
  test_data <- setup_integration_test_data()
  on.exit(unlink(test_data$test_dir, recursive = TRUE))
  
  # Test error in reading stage
  expect_error(
    read_spectra(source = "csv", spectra_path = "nonexistent.csv"),
    "Path does not exist"
  )
  
  # Test error in preprocessing stage
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_data$spectra_file,
    verbose = FALSE
  )
  
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(...) {
      stop("Resampling failed")
    },
    {
      expect_error(
        preprocess_spectra(spectra, verbose = FALSE),
        "Spectral resampling failed"
      )
    }
  )
  
  # Test error in dataset creation stage
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      preprocessed <- preprocess_spectra(spectra, verbose = FALSE)
      
      # Missing response file
      expect_error(
        create_dataset(preprocessed, "nonexistent_response.csv", verbose = FALSE),
        "Response file not found"
      )
    }
  )
})

# Data Flow Consistency Tests --------------------------------------------------

test_that("data flows consistently through pipeline stages", {
  
  test_data <- setup_integration_test_data()
  on.exit(unlink(test_data$test_dir, recursive = TRUE))
  
  # Track sample IDs through the pipeline
  original_spectra <- read_spectra(
    source = "csv",
    spectra_path = test_data$spectra_file,
    verbose = FALSE
  )
  original_sample_ids <- original_spectra$Sample_ID
  
  # After preprocessing
  names(original_spectra) <- gsub("^wn_", "", names(original_spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      preprocessed <- preprocess_spectra(original_spectra, verbose = FALSE)
      expect_equal(preprocessed$Sample_ID, original_sample_ids)
      
      # After dataset creation (with aggregation)
      with_mocked_bindings(
        `parse_filename_metadata` = function(file_name, ...) {
          parts <- strsplit(file_name, "_")[[1]]
          return(data.frame(
            Sample_ID = paste(parts[1], parts[2], sep = "_"),
            stringsAsFactors = FALSE
          ))
        },
        {
          dataset <- create_dataset(
            preprocessed,
            test_data$response_file,
            parse_ids = TRUE,
            id_format = "project_sampleid_fraction_scan",
            verbose = FALSE
          )
          
          # Should have unique sample IDs after aggregation
          expect_true(all(!duplicated(dataset$Sample_ID)))
          
          # All sample IDs should be present in response data
          response_ids <- read.csv(test_data$response_file)$Sample_ID
          expect_true(all(dataset$Sample_ID %in% response_ids))
        }
      )
    }
  )
})

# Performance Integration Tests ------------------------------------------------

test_that("pipeline completes efficiently end-to-end", {
  
  test_data <- setup_integration_test_data()
  on.exit(unlink(test_data$test_dir, recursive = TRUE))
  
  # Time the complete pipeline
  start_time <- Sys.time()
  
  # Read
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_data$spectra_file,
    verbose = FALSE
  )
  
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      Sys.sleep(0.01)  # Simulate some processing time
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    `prcomp` = function(x, ...) {
      list(sdev = sqrt(c(3, 2, 1)), x = matrix(rnorm(nrow(x) * 3), nrow(x), 3))
    },
    `robustbase::covMcd` = function(x) {
      list(center = rep(0, ncol(x)), cov = diag(ncol(x)))
    },
    {
      # Preprocess
      preprocessed <- preprocess_spectra(spectra, verbose = FALSE)
      
      # Create dataset
      dataset <- create_dataset(
        preprocessed,
        test_data$response_file,
        response_variables = "SOC",
        verbose = FALSE
      )
      
      # Finalize
      finalized <- finalize_dataset(
        dataset,
        response_variable = "SOC",
        verbose = FALSE
      )
      
      end_time <- Sys.time()
      
      # Should complete within reasonable time
      expect_lt(as.numeric(end_time - start_time), 10)
      expect_s3_class(finalized, "data.frame")
    }
  )
})

# Real Fixture Integration Tests -----------------------------------------------

test_that("pipeline works with actual test fixtures", {
  
  test_spectra_csv <- file.path(test_path("fixtures"), "test_spectra.csv")
  test_response_csv <- file.path(test_path("fixtures"), "test_response.csv")
  
  skip_if_not(file.exists(test_spectra_csv), "Test spectra CSV not found")
  skip_if_not(file.exists(test_response_csv), "Test response CSV not found")
  
  # Read actual fixture data
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_spectra_csv,
    verbose = FALSE
  )
  
  expect_s3_class(spectra, "data.frame")
  expect_gt(nrow(spectra), 0)
  
  # Convert column names for downstream processing
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      # Simple resampling simulation
      return(X[, 1:min(ncol(X), 100)])  # Keep first 100 columns
    },
    {
      # Test preprocessing
      preprocessed <- preprocess_spectra(
        spectra,
        resample_interval = 8,
        verbose = FALSE
      )
      
      expect_s3_class(preprocessed, "data.frame")
      expect_equal(nrow(preprocessed), nrow(spectra))
      
      # Test dataset creation
      dataset <- create_dataset(
        preprocessed,
        test_response_csv,
        response_variables = c("SOC", "pH"),
        include_coords = TRUE,
        verbose = FALSE
      )
      
      expect_s3_class(dataset, "data.frame")
      expect_true(all(c("SOC", "pH", "Latitude", "Longitude") %in% names(dataset)))
      expect_gt(nrow(dataset), 0)
    }
  )
})

# Configuration Integration Tests ----------------------------------------------

test_that("pipeline integrates with configuration generation", {
  
  test_data <- setup_integration_test_data()
  on.exit(unlink(test_data$test_dir, recursive = TRUE))
  
  # Create configurations
  configs <- create_configs(
    models = c("plsr", "random_forest"),
    transformations = c("none", "sqrt"),
    preprocessing = c("raw", "snv"),
    feature_selection = c("none"),
    soil_covariates = c("Clay", "Sand"),
    verbose = FALSE
  )
  
  expect_s3_class(configs, "data.frame")
  expect_gt(nrow(configs), 1)
  
  # Test that configurations are compatible with dataset
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_data$spectra_file,
    verbose = FALSE
  )
  
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      preprocessed <- preprocess_spectra(spectra, verbose = FALSE)
      
      dataset <- create_dataset(
        preprocessed,
        test_data$response_file,
        response_variables = c("SOC", "Clay", "Sand"),
        verbose = FALSE
      )
      
      # Check that covariates from configs are available in dataset
      test_config <- configs[1, ]  # First configuration
      config_covariates <- test_config$covariates[[1]]
      
      if (!is.null(config_covariates)) {
        expect_true(all(config_covariates %in% names(dataset)))
      }
      
      expect_s3_class(dataset, "data.frame")
    }
  )
})

# Memory Management Integration Tests ------------------------------------------

test_that("pipeline manages memory efficiently", {
  
  test_data <- setup_integration_test_data()
  on.exit(unlink(test_data$test_dir, recursive = TRUE))
  
  # Monitor memory usage through pipeline
  start_memory <- as.numeric(object.size(ls()))
  
  spectra <- read_spectra(
    source = "csv",
    spectra_path = test_data$spectra_file,
    verbose = FALSE
  )
  
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      return(matrix(runif(nrow(X) * length(new.wav)), nrow = nrow(X)))
    },
    {
      preprocessed <- preprocess_spectra(spectra, verbose = FALSE)
      
      dataset <- create_dataset(
        preprocessed,
        test_data$response_file,
        verbose = FALSE
      )
      
      # Final dataset should not be excessively large
      expect_lt(object.size(dataset), 50 * 1024 * 1024)  # Less than 50MB
    }
  )
  
  # Clean up intermediate objects
  rm(spectra)
  gc()
})

# Robustness Integration Tests -------------------------------------------------

test_that("pipeline handles edge cases robustly", {
  
  # Create minimal test data
  temp_dir <- tempfile("minimal_test")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Single sample with minimal spectral data
  minimal_spectra <- data.frame(
    Sample_ID = "test_sample",
    wn_4000 = 0.5,
    wn_3000 = 0.4,
    wn_2000 = 0.3,
    stringsAsFactors = FALSE
  )
  
  minimal_response <- data.frame(
    Sample_ID = "test_sample",
    SOC = 2.1,
    stringsAsFactors = FALSE
  )
  
  spectra_file <- file.path(temp_dir, "minimal_spectra.csv")
  response_file <- file.path(temp_dir, "minimal_response.csv")
  
  write.csv(minimal_spectra, spectra_file, row.names = FALSE)
  write.csv(minimal_response, response_file, row.names = FALSE)
  
  # Test pipeline with minimal data
  spectra <- read_spectra(
    source = "csv",
    spectra_path = spectra_file,
    verbose = FALSE
  )
  
  names(spectra) <- gsub("^wn_", "", names(spectra))
  
  with_mocked_bindings(
    `prospectr::resample` = function(X, wav, new.wav, interpol) {
      # Return minimal resampled data
      return(matrix(c(0.5, 0.4, 0.3), nrow = 1, ncol = 3))
    },
    {
      preprocessed <- preprocess_spectra(spectra, verbose = FALSE)
      expect_equal(nrow(preprocessed), 1)
      
      dataset <- create_dataset(
        preprocessed,
        response_file,
        verbose = FALSE
      )
      expect_equal(nrow(dataset), 1)
      expect_true("SOC" %in% names(dataset))
      
      # Finalization with minimal data (should handle gracefully)
      finalized <- finalize_dataset(
        dataset,
        "SOC",
        spectral_outlier_method = "none",  # Skip PCA with minimal data
        verbose = FALSE
      )
      expect_equal(nrow(finalized), 1)
      expect_true("outlier_flag" %in% names(finalized))
    }
  )
})