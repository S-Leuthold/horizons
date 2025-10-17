# Test Data Fixtures and Generators
# Helper functions to create consistent test data across the test suite

library(dplyr)
library(tibble)

## ---------------------------------------------------------------------------
## Spectral Data Generators
## ---------------------------------------------------------------------------

#' Generate Tiny Spectral Dataset (5 samples, 50 wavelengths)
#' For fast unit tests
create_tiny_spectra <- function(seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # Use realistic MIR wavelength range
  wavelengths <- seq(600, 4000, length.out = 50)
  n_samples <- 5
  
  # Generate realistic spectral patterns with soil properties
  spectra_data <- tibble(
    Sample_ID = paste0("TINY_", sprintf("%03d", 1:n_samples)),
    Project = "TestProject",
    SOC = abs(rnorm(n_samples, 2.5, 0.8)),  # Soil Organic Carbon %
    Clay = abs(rnorm(n_samples, 25, 8)),     # Clay content %
    pH = rnorm(n_samples, 6.5, 0.5)         # Soil pH
  )
  
  # Add spectral columns with realistic patterns
  for (wl in wavelengths) {
    col_name <- as.character(round(wl))
    
    # Create absorption features at key wavelengths
    base_abs <- ifelse(wl < 1200, 0.3, 0.5)  # Different regions
    noise <- rnorm(n_samples, 0, 0.05)
    spectra_data[[col_name]] <- base_abs + noise
  }
  
  spectra_data
}

#' Generate Small Spectral Dataset (20 samples, 100 wavelengths) 
#' For integration tests
create_small_spectra <- function(seed = 456) {
  if (!is.null(seed)) set.seed(seed)
  
  wavelengths <- seq(600, 4000, length.out = 100)
  n_samples <- 20
  
  spectra_data <- tibble(
    Sample_ID = paste0("SMALL_", sprintf("%03d", 1:n_samples)),
    Project = rep(c("ProjectA", "ProjectB"), each = 10),
    SOC = abs(rnorm(n_samples, 2.5, 1.2)),
    Clay = abs(rnorm(n_samples, 25, 12)),
    pH = rnorm(n_samples, 6.5, 0.8)
  )
  
  # More complex spectral patterns
  for (wl in wavelengths) {
    col_name <- as.character(round(wl))
    
    # Create multiple absorption features
    base_abs <- 0.4
    if (wl > 2800 & wl < 3000) base_abs <- 0.7  # O-H stretch
    if (wl > 1600 & wl < 1700) base_abs <- 0.6  # C=O stretch
    
    noise <- rnorm(n_samples, 0, 0.03)
    spectra_data[[col_name]] <- pmax(0, base_abs + noise)
  }
  
  spectra_data
}

#' Generate Medium Spectral Dataset (50 samples, 200 wavelengths)
#' For E2E tests
create_medium_spectra <- function(seed = 789) {
  if (!is.null(seed)) set.seed(seed)
  
  wavelengths <- seq(600, 4000, length.out = 200)
  n_samples <- 50
  
  spectra_data <- tibble(
    Sample_ID = paste0("MED_", sprintf("%03d", 1:n_samples)),
    Project = rep(c("ProjectA", "ProjectB", "ProjectC"), length.out = n_samples),
    SOC = abs(rnorm(n_samples, 2.5, 1.5)),
    Clay = abs(rnorm(n_samples, 25, 15)),
    pH = rnorm(n_samples, 6.5, 1.0)
  )
  
  # Complex spectral patterns with outliers
  for (wl in wavelengths) {
    col_name <- as.character(round(wl))
    
    base_abs <- 0.4
    # Multiple realistic absorption features
    if (wl > 2800 & wl < 3000) base_abs <- 0.7  # O-H stretch
    if (wl > 1600 & wl < 1700) base_abs <- 0.6  # C=O stretch  
    if (wl > 1000 & wl < 1200) base_abs <- 0.5  # Silicate features
    
    noise <- rnorm(n_samples, 0, 0.03)
    
    # Add some outlier samples
    outlier_samples <- sample(1:n_samples, 3)
    noise[outlier_samples] <- noise[outlier_samples] + 0.2
    
    spectra_data[[col_name]] <- pmax(0, base_abs + noise)
  }
  
  spectra_data
}

## ---------------------------------------------------------------------------
## Response Variable Generators  
## ---------------------------------------------------------------------------

#' Add Response Variables to Spectral Data
#' Creates realistic soil property values
add_response_variables <- function(spectra_data, variables = c("SOC", "Clay"), seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  n_samples <- nrow(spectra_data)
  
  for (var in variables) {
    if (var == "SOC") {
      # Soil Organic Carbon (%) - log-normal distribution
      spectra_data[[var]] <- exp(rnorm(n_samples, mean = 1.5, sd = 0.5))
    } else if (var == "Clay") {
      # Clay content (%) - beta distribution scaled
      spectra_data[[var]] <- rbeta(n_samples, 2, 5) * 60 + 5
    } else if (var == "pH") {
      # Soil pH - normal around neutral
      spectra_data[[var]] <- rnorm(n_samples, mean = 6.5, sd = 1)
    } else {
      # Generic continuous variable
      spectra_data[[var]] <- rnorm(n_samples, mean = 10, sd = 3)
    }
  }
  
  spectra_data
}

## ---------------------------------------------------------------------------
## Configuration Generators
## ---------------------------------------------------------------------------

#' Create Test Model Configurations
create_test_configs <- function(n_configs = 3) {
  data.frame(
    model = c("linear_reg", "rand_forest", "pls"),
    transformation = c("none", "log", "sqrt"),
    preprocessing = c("raw", "snv", "derivative"),
    feature_selection = c("none", "correlation", "boruta"),
    stringsAsFactors = FALSE
  )[1:n_configs, ]
}

#' Create Minimal Test Configuration (for fast tests)
create_minimal_config <- function() {
  data.frame(
    model = "linear_reg",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )
}

#' Create Evaluation Test Configuration (for evaluation module tests)
create_eval_test_config <- function() {
  data.frame(
    config_id = "eval_test_001",
    model = "plsr",  # Fast for testing
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = I(list(NULL)),  # No covariates for simplicity
    stringsAsFactors = FALSE
  )
}

#' Create Evaluation Test Data (wrapper for create_small_spectra)
create_eval_test_data <- function(n_samples = 50, n_wavelengths = 100, seed = 123) {
  if (!is.null(seed)) set.seed(seed)

  # Use create_small_spectra if n_samples = 20, otherwise generate
  if (n_samples == 20 && n_wavelengths == 100) {
    data <- create_small_spectra(seed = seed)
  } else {
    # Generate custom size
    wavelengths <- seq(600, 4000, length.out = n_wavelengths)

    data <- tibble(
      Sample_ID = paste0("EVAL_", sprintf("%03d", 1:n_samples)),
      Project = rep("TestProject", n_samples)
    )

    for (wl in wavelengths) {
      col_name <- as.character(round(wl))
      base_abs <- 0.5
      if (wl > 2800 & wl < 3000) base_abs <- 0.7
      noise <- rnorm(n_samples, 0, 0.05)
      data[[col_name]] <- pmax(0.1, base_abs + noise)
    }

    data$SOC <- abs(rnorm(n_samples, 2.5, 1.0))
  }

  # Add Response variable for evaluation
  data$Response <- data$SOC

  data
}

## ---------------------------------------------------------------------------
## Mock tidymodels Objects
## ---------------------------------------------------------------------------

#' Create Mock rsample Split Object
create_mock_split <- function(data, prop = 0.8) {
  n <- nrow(data)
  train_idx <- sample(1:n, floor(n * prop))
  
  # Create a simple mock split object
  split_obj <- list(
    data = data,
    in_id = train_idx,
    out_id = setdiff(1:n, train_idx)
  )
  
  class(split_obj) <- c("rsplit", "list")
  split_obj
}

#' Create Mock CV Folds
create_mock_cv_folds <- function(data, v = 3) {
  n <- nrow(data)
  fold_id <- sample(rep(1:v, length.out = n))
  
  folds <- list()
  for (i in 1:v) {
    analysis_idx <- which(fold_id != i)
    assessment_idx <- which(fold_id == i)
    
    fold <- list(
      data = data,
      in_id = analysis_idx,
      out_id = assessment_idx
    )
    class(fold) <- c("rsplit", "list")
    folds[[i]] <- fold
  }
  
  class(folds) <- c("vfold_cv", "rset", "tbl_df", "tbl", "data.frame")
  folds
}

## ---------------------------------------------------------------------------
## Covariate Data Generators
## ---------------------------------------------------------------------------

#' Create Mock Climate Covariate Data
create_mock_climate_data <- function(sample_ids, seed = 111) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- length(sample_ids)
  
  tibble(
    Sample_ID = sample_ids,
    MAT = rnorm(n, 12, 5),      # Mean Annual Temperature
    MAP = rnorm(n, 800, 200),   # Mean Annual Precipitation  
    Elevation = runif(n, 100, 2000),
    Latitude = runif(n, 30, 50),
    Longitude = runif(n, -120, -80)
  )
}

#' Create Mock Soil Covariate Data  
create_mock_soil_data <- function(sample_ids, seed = 222) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- length(sample_ids)
  
  tibble(
    Sample_ID = sample_ids,
    USDA_Texture = sample(c("Sandy Loam", "Clay Loam", "Silty Clay"), n, replace = TRUE),
    Bulk_Density = rnorm(n, 1.3, 0.2),
    CEC = rnorm(n, 15, 5),
    Base_Saturation = runif(n, 50, 95)
  )
}

## ---------------------------------------------------------------------------
## Prediction Data Generators
## ---------------------------------------------------------------------------

#' Create Mock Prediction Results
create_mock_predictions <- function(n_obs, rmse = 2, bias = 0, seed = 333) {
  if (!is.null(seed)) set.seed(seed)
  
  # Generate "true" values
  truth <- rnorm(n_obs, mean = 10, sd = 3)
  
  # Generate predictions with specified RMSE and bias
  pred <- truth + rnorm(n_obs, mean = bias, sd = rmse)
  
  tibble(
    .pred = pred,
    truth = truth,
    .row = 1:n_obs
  )
}

#' Create Mock Cross-Validation Predictions
create_mock_cv_predictions <- function(n_obs, n_folds = 5, seed = 444) {
  if (!is.null(seed)) set.seed(seed)
  
  preds <- create_mock_predictions(n_obs, seed = seed)
  preds$id <- paste0("Fold", sample(1:n_folds, n_obs, replace = TRUE))
  preds$.row <- 1:n_obs
  
  preds
}

## ---------------------------------------------------------------------------
## Data Validation Helpers
## ---------------------------------------------------------------------------

#' Check if Data has Expected Spectral Structure
expect_spectral_data <- function(data) {
  testthat::expect_s3_class(data, "data.frame")
  testthat::expect_true("Sample_ID" %in% names(data))
  
  # Check for numeric wavelength columns
  numeric_cols <- sapply(data, is.numeric)
  wavelength_cols <- grepl("^[0-9]+\\.?[0-9]*$", names(data))
  spectral_cols <- names(data)[numeric_cols & wavelength_cols]
  
  testthat::expect_true(length(spectral_cols) > 0, 
                       info = "Expected numeric wavelength columns")
  
  invisible(TRUE)
}

#' Check if Config has Required Structure
expect_valid_config <- function(config) {
  testthat::expect_s3_class(config, "data.frame")
  required_cols <- c("model", "transformation", "preprocessing", "feature_selection")
  testthat::expect_true(all(required_cols %in% names(config)),
                       info = "Config missing required columns")
  
  invisible(TRUE)
}