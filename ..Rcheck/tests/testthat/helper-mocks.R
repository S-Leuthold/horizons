#' Mocking Utilities for horizons Package Testing
#' 
#' Helper functions for mocking external dependencies and creating controlled
#' test environments. These utilities allow testing of horizons functions
#' without relying on external data sources or expensive computations.

#' Create temporary test cache directory
#'
#' Sets up a temporary directory for testing caching functionality and
#' ensures it's cleaned up after tests complete.
#'
#' @param code Code to execute with temporary cache directory.
#'
#' @return Result of executing code with cache directory available.
with_test_cache <- function(code) {
  withr::with_tempdir({
    cache_dir <- file.path(getwd(), "test_cache")
    dir.create(cache_dir, recursive = TRUE)
    
    # Set cache directory for the duration of the test
    withr::with_envvar(
      c("HORIZONS_CACHE_DIR" = cache_dir),
      code
    )
  })
}

#' Mock OSSL data download
#'
#' Replaces OSSL data download functions with mock data for testing
#' without requiring actual downloads or network access.
#'
#' @param code Code to execute with mocked OSSL functions.
#' @param n_samples Integer. Number of samples in mock OSSL data.
#' @param covariates Character vector of covariates to include in mock data.
#'
#' @return Result of executing code with mocked OSSL functions.
with_mocked_ossl <- function(code, n_samples = 100, covariates = c("Clay", "pH", "SOC")) {
  
  # Create mock OSSL data
  mock_ossl_data <- make_test_spectra(n_samples = n_samples, seed = 456)
  mock_ossl_data$Project <- "OSSL_GLOBAL"
  
  # Add covariates
  covariate_data <- make_test_covariates(
    sample_ids = mock_ossl_data$Sample_ID,
    covariates = covariates,
    seed = 456
  )
  
  mock_ossl_data <- merge(mock_ossl_data, covariate_data, by = "Sample_ID")
  
  # Mock download function
  mock_download_ossl <- function(...) {
    return(mock_ossl_data)
  }
  
  # Mock file path functions
  mock_get_ossl_path <- function(...) {
    return(tempfile(fileext = ".qs"))
  }
  
  withr::with_mocked_bindings(
    download_ossl_data = mock_download_ossl,
    get_ossl_data_path = mock_get_ossl_path,
    code,
    .package = "horizons"
  )
}

#' Mock climate API calls
#'
#' Replaces climate data API calls with mock responses for testing
#' without requiring network access or API keys.
#'
#' @param code Code to execute with mocked climate functions.
#' @param n_samples Integer. Number of samples for mock climate data.
#'
#' @return Result of executing code with mocked climate functions.
with_mocked_climate <- function(code, n_samples = 50) {
  
  # Create mock climate data
  mock_climate_data <- data.frame(
    Sample_ID = paste0("TEST_", sprintf("%03d", seq_len(n_samples))),
    latitude = runif(n_samples, 25, 50),
    longitude = runif(n_samples, -125, -65),
    tmin_mean = runif(n_samples, -5, 15),
    tmax_mean = runif(n_samples, 15, 35),
    prcp_sum = runif(n_samples, 200, 1500),
    vpd_mean = runif(n_samples, 0.5, 3.0),
    pet_mean = runif(n_samples, 500, 1800),
    stringsAsFactors = FALSE
  )
  
  # Mock climate fetch function
  mock_fetch_climate <- function(input_data, ...) {
    # Return climate data matching input Sample_IDs
    sample_ids <- input_data$Sample_ID
    n_input <- length(sample_ids)
    
    result <- data.frame(
      Sample_ID = sample_ids,
      latitude = runif(n_input, 25, 50),
      longitude = runif(n_input, -125, -65),
      tmin_mean = runif(n_input, -5, 15),
      tmax_mean = runif(n_input, 15, 35),
      prcp_sum = runif(n_input, 200, 1500),
      vpd_mean = runif(n_input, 0.5, 3.0),
      pet_mean = runif(n_input, 500, 1800),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  withr::with_mocked_bindings(
    fetch_climate_covariates = mock_fetch_climate,
    code,
    .package = "horizons"
  )
}

#' Mock file system operations
#'
#' Creates a controlled file system environment for testing file
#' reading and writing operations.
#'
#' @param code Code to execute with mocked file system.
#' @param opus_files List of mock OPUS file contents.
#'
#' @return Result of executing code with mocked file operations.
with_mocked_filesystem <- function(code, opus_files = NULL) {
  
  withr::with_tempdir({
    # Create test directory structure
    test_dirs <- c("data", "spectra", "results")
    purrr::walk(test_dirs, ~ dir.create(.x, recursive = TRUE))
    
    # Create mock OPUS files if provided
    if (!is.null(opus_files)) {
      opus_dir <- file.path(getwd(), "spectra")
      
      for (i in seq_along(opus_files)) {
        file_path <- file.path(opus_dir, names(opus_files)[i])
        # Save as RDS for simplicity in testing
        saveRDS(opus_files[[i]], file_path)
      }
    }
    
    # Execute code in this environment
    code
  })
}

#' Mock parallel processing
#'
#' Replaces parallel processing functions with sequential versions
#' for deterministic testing and debugging.
#'
#' @param code Code to execute with sequential processing.
#'
#' @return Result of executing code with mocked parallel functions.
with_sequential_processing <- function(code) {
  
  # Mock future map functions to run sequentially
  mock_future_map <- function(.x, .f, ...) {
    purrr::map(.x, .f, ...)
  }
  
  mock_future_map_dfr <- function(.x, .f, ...) {
    purrr::map_dfr(.x, .f, ...)
  }
  
  withr::with_mocked_bindings(
    list(
      "future_map" = mock_future_map,
      "future_map_dfr" = mock_future_map_dfr
    ),
    code,
    .package = "furrr"
  )
}

#' Mock expensive computations
#'
#' Replaces computationally expensive functions with fast mock versions
#' for testing logic without waiting for actual computations.
#'
#' @param code Code to execute with mocked computations.
#'
#' @return Result of executing code with mocked expensive functions.
with_mocked_computations <- function(code) {
  
  # Mock PCA computation
  mock_reduce_dimensions <- function(training_data, ...) {
    # Return simplified PCA result
    n_components <- min(5, ncol(training_data) - 2)  # Exclude Sample_ID, Response
    
    list(
      pca_result = list(
        x = matrix(rnorm(nrow(training_data) * n_components), 
                   nrow = nrow(training_data),
                   ncol = n_components),
        rotation = matrix(rnorm(n_components * (ncol(training_data) - 2)),
                         nrow = ncol(training_data) - 2,
                         ncol = n_components)
      ),
      explained_variance = runif(n_components, 0.05, 0.3)
    )
  }
  
  # Mock model tuning
  mock_tune_grid <- function(...) {
    # Return mock tuning results
    tibble::tibble(
      .metric = "rmse",
      .estimator = "standard", 
      mean = runif(1, 0.5, 2.0),
      std_err = runif(1, 0.05, 0.2),
      .config = "Preprocessor1_Model1"
    )
  }
  
  withr::with_mocked_bindings(
    list(
      "reduce_dimensions_pca" = mock_reduce_dimensions
    ),
    code,
    .package = "horizons"
  )
}

#' Mock error conditions
#'
#' Injects controlled errors for testing error handling paths.
#'
#' @param code Code to execute with error injection.
#' @param error_functions Character vector of function names that should error.
#' @param error_message Character. Error message to throw.
#'
#' @return Result of executing code with error injection.
with_error_injection <- function(code, 
                                 error_functions = character(0),
                                 error_message = "Simulated test error") {
  
  # Create error-throwing functions
  error_makers <- stats::setNames(
    lapply(error_functions, function(x) {
      function(...) stop(error_message, call. = FALSE)
    }),
    error_functions
  )
  
  do.call(withr::with_mocked_bindings, c(error_makers, list(code)))
}

#' Create test environment with multiple mocks
#'
#' Convenience function that sets up a comprehensive test environment
#' with commonly needed mocks for integration testing.
#'
#' @param code Code to execute in mocked environment.
#' @param use_cache Logical. Whether to set up cache mocking.
#' @param use_ossl Logical. Whether to mock OSSL functions.
#' @param use_climate Logical. Whether to mock climate functions.
#' @param use_sequential Logical. Whether to force sequential processing.
#'
#' @return Result of executing code in comprehensive mock environment.
with_comprehensive_mocks <- function(code,
                                     use_cache = TRUE,
                                     use_ossl = TRUE,
                                     use_climate = TRUE,
                                     use_sequential = TRUE) {
  
  # Build nested mock environment
  mock_code <- code
  
  if (use_sequential) {
    mock_code <- quote(with_sequential_processing(!!mock_code))
  }
  
  if (use_climate) { 
    mock_code <- quote(with_mocked_climate(!!mock_code))
  }
  
  if (use_ossl) {
    mock_code <- quote(with_mocked_ossl(!!mock_code))
  }
  
  if (use_cache) {
    mock_code <- quote(with_test_cache(!!mock_code))
  }
  
  eval(mock_code)
}