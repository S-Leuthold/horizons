#' Test Data Generation Utilities
#' 
#' Helper functions for creating synthetic test data that matches the expected
#' structure and format used throughout the horizons package.

#' Generate synthetic spectral data for testing
#'
#' Creates a data frame with synthetic spectral data that mimics the structure
#' expected by horizons functions. Includes Sample_ID, Response, and spectral
#' columns named as wavenumbers.
#'
#' @param n_samples Integer. Number of samples to generate. Default is 10.
#' @param wavelengths Numeric vector. Wavenumbers to include. Default is 600:4000 by 2.
#' @param response_range Numeric vector of length 2. Range for response values.
#' @param add_noise Logical. Whether to add realistic noise to spectra.
#' @param add_project Logical. Whether to add Project column (default TRUE).
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A data frame with Sample_ID, Response, and spectral columns.
make_test_spectra <- function(n_samples = 10,
                              wavelengths = seq(600, 4000, by = 2),
                              response_range = c(0.5, 5.0),
                              add_noise = TRUE,
                              add_project = TRUE,
                              seed = 123) {
  
  set.seed(seed)
  
  # Create sample IDs
  sample_ids <- paste0("TEST_", sprintf("%03d", seq_len(n_samples)))
  
  # Generate response values
  responses <- stats::runif(n_samples, 
                           min = response_range[1], 
                           max = response_range[2])
  
  # Generate synthetic spectra (simple gaussian peaks with noise)
  n_wavelengths <- length(wavelengths)
  spectra_matrix <- matrix(0, nrow = n_samples, ncol = n_wavelengths)
  
  for (i in seq_len(n_samples)) {
    # Create a few gaussian peaks at random locations
    n_peaks <- sample(2:5, 1)
    peak_locations <- sample(seq_len(n_wavelengths), n_peaks)
    peak_heights <- runif(n_peaks, 0.1, 1.0)
    peak_widths <- sample(50:200, n_peaks, replace = TRUE)
    
    spectrum <- rep(0.01, n_wavelengths)  # baseline
    
    for (j in seq_len(n_peaks)) {
      peak_idx <- peak_locations[j]
      width <- peak_widths[j]
      height <- peak_heights[j]
      
      # Gaussian peak
      indices <- seq_len(n_wavelengths)
      gaussian <- height * exp(-((indices - peak_idx)^2) / (2 * (width/4)^2))
      spectrum <- spectrum + gaussian
    }
    
    # Add noise if requested
    if (add_noise) {
      noise <- rnorm(n_wavelengths, 0, 0.01)
      spectrum <- spectrum + noise
    }
    
    spectra_matrix[i, ] <- spectrum
  }
  
  # Combine into data frame
  spectra_df <- as.data.frame(spectra_matrix)
  names(spectra_df) <- as.character(wavelengths)
  
  result <- data.frame(
    Sample_ID = sample_ids,
    Response = responses,
    stringsAsFactors = FALSE
  )
  
  # Add Project column if requested
  if (add_project) {
    result$Project <- "TEST_PROJECT"
  }
  
  result <- cbind(result, spectra_df)
  
  return(result)
}

#' Generate test covariate data
#'
#' Creates a data frame with synthetic covariate data that can be joined
#' with spectral data for testing.
#'
#' @param sample_ids Character vector of Sample_IDs to match spectral data.
#' @param covariates Character vector of covariate names to include.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A data frame with Sample_ID and covariate columns.
make_test_covariates <- function(sample_ids = paste0("TEST_", sprintf("%03d", 1:10)),
                                 covariates = c("Clay", "pH", "SOC", "Depth"),
                                 seed = 123) {
  
  set.seed(seed)
  n_samples <- length(sample_ids)
  
  result <- data.frame(
    Sample_ID = sample_ids,
    stringsAsFactors = FALSE
  )
  
  # Generate realistic covariate values
  covariate_data <- list(
    Clay = round(runif(n_samples, 5, 45), 1),      # Clay percentage
    pH = round(runif(n_samples, 4.5, 8.5), 2),     # pH
    SOC = round(runif(n_samples, 0.5, 4.0), 2),    # Soil organic carbon %
    Sand = round(runif(n_samples, 20, 80), 1),     # Sand percentage
    Silt = round(runif(n_samples, 10, 60), 1),     # Silt percentage
    Depth = round(runif(n_samples, 0, 30), 0),     # Depth cm
    BD = round(runif(n_samples, 0.8, 1.8), 2),     # Bulk density
    CEC = round(runif(n_samples, 5, 35), 1)        # Cation exchange capacity
  )
  
  # Add requested covariates
  for (covar in covariates) {
    if (covar %in% names(covariate_data)) {
      result[[covar]] <- covariate_data[[covar]]
    } else {
      # Generate generic numeric covariate
      result[[covar]] <- round(runif(n_samples, 0, 100), 2)
    }
  }
  
  return(result)
}

#' Create a minimal project configuration for testing
#'
#' Generates a basic project structure that can be used for testing
#' project-related functions.
#'
#' @param project_name Character. Name for the test project.
#' @param n_samples Integer. Number of samples to include.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A list containing project data and metadata.
make_test_project <- function(project_name = "TEST_PROJECT",
                              n_samples = 10,
                              seed = 123) {
  
  set.seed(seed)
  
  # Create spectral data
  spectra_data <- make_test_spectra(n_samples = n_samples, seed = seed)
  spectra_data$Project <- project_name
  
  # Create metadata
  metadata <- data.frame(
    Project = project_name,
    Sample_ID = spectra_data$Sample_ID,
    File_Path = paste0("test_data/", spectra_data$Sample_ID, ".0"),
    Date_Collected = as.Date("2023-01-01") + sample(0:365, n_samples),
    Location = paste0("Site_", sample(LETTERS[1:5], n_samples, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Create covariate data
  covariate_data <- make_test_covariates(
    sample_ids = spectra_data$Sample_ID,
    seed = seed
  )
  
  list(
    spectra = spectra_data,
    metadata = metadata,
    covariates = covariate_data,
    project_name = project_name
  )
}

#' Load small test OPUS file data
#'
#' Creates or loads minimal OPUS-like data for testing file reading functions.
#' This simulates the structure returned by opusreader2 functions.
#'
#' @param n_files Integer. Number of test files to simulate.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A list mimicking opusreader2 output structure.
load_test_opus <- function(n_files = 5, seed = 123) {
  
  set.seed(seed)
  
  # Generate test data that mimics opusreader2 structure
  test_files <- vector("list", n_files)
  names(test_files) <- paste0("test_file_", seq_len(n_files), ".0")
  
  for (i in seq_len(n_files)) {
    
    # Generate spectral data
    wavelengths <- seq(600, 4000, by = 2)
    spectrum <- make_test_spectra(n_samples = 1, 
                                  wavelengths = wavelengths, 
                                  seed = seed + i)
    
    # Remove Sample_ID and Response to match OPUS structure
    spectral_cols <- spectrum[, !names(spectrum) %in% c("Sample_ID", "Response")]
    
    test_files[[i]] <- list(
      ab = as.numeric(spectral_cols[1, ]),
      wavenumbers = wavelengths,
      metadata = list(
        filename = names(test_files)[i],
        instrument = "TEST_FTIR",
        date = as.Date("2023-01-01") + i
      )
    )
  }
  
  return(test_files)
}

#' Create test model results for ensemble testing
#'
#' Generates synthetic model results that can be used for testing
#' ensemble and stacking functions.
#'
#' @param n_models Integer. Number of models to simulate.
#' @param n_samples Integer. Number of samples in predictions.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A list of model results with predictions and metrics.
make_test_model_results <- function(n_models = 5, n_samples = 50, seed = 123) {
  
  set.seed(seed)
  
  # Generate true values
  true_values <- runif(n_samples, 0.5, 5.0)
  
  results <- vector("list", n_models)
  names(results) <- paste0("model_", seq_len(n_models))
  
  for (i in seq_len(n_models)) {
    # Generate predictions with some error
    error <- rnorm(n_samples, 0, 0.2)
    predictions <- true_values + error
    
    # Calculate metrics
    rmse <- sqrt(mean((predictions - true_values)^2))
    r2 <- cor(predictions, true_values)^2
    
    results[[i]] <- list(
      predictions = predictions,
      truth = true_values,
      metrics = list(
        rmse = rmse,
        r2 = r2,
        model_id = names(results)[i]
      )
    )
  }
  
  return(results)
}