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
  
  testthat::with_mocked_bindings(
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
  
  testthat::with_mocked_bindings(
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
  
  testthat::with_mocked_bindings(
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
  
  testthat::with_mocked_bindings(
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
  
  do.call(testthat::with_mocked_bindings, c(error_makers, list(code)))
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

#' Enhanced Mock OSSL for Covariate Testing
#'
#' Creates realistic mock OSSL data with proper structure for the
#' refactored covariate prediction system.
#'
#' @param code Code to execute with mocked OSSL.
#' @param n_samples Integer. Number of OSSL samples to generate.
#' @param n_wavelengths Integer. Number of spectral wavelengths.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return Result of executing code with enhanced OSSL mocking.
with_mocked_ossl_covariates <- function(code, 
                                       n_samples = 1000,
                                       n_wavelengths = 200,
                                       seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Generate wavelengths
  wavelengths <- seq(600, 4000, length.out = n_wavelengths)
  
  # Create OSSL structure
  mock_ossl <- tibble::tibble(
    sample_id = paste0("OSSL_", sprintf("%05d", 1:n_samples))
  )
  
  # Add realistic spectral data
  for (i in seq_along(wavelengths)) {
    wl <- wavelengths[i]
    col_name <- paste0("scan_mir.", round(wl, 1))
    
    # Create realistic absorption patterns
    base <- 0.5
    if (wl > 1400 && wl < 1500) base <- 0.7  # OH
    if (wl > 2800 && wl < 3000) base <- 0.8  # CH
    
    mock_ossl[[col_name]] <- base + rnorm(n_samples, 0, 0.1)
  }
  
  # Add soil properties in OSSL format
  mock_ossl$clay.tot_usda.c60_w.pct <- runif(n_samples, 5, 60)  # %
  mock_ossl$sand.tot_usda.c60_w.pct <- runif(n_samples, 10, 80)  # %
  mock_ossl$silt.tot_usda.c60_w.pct <- 100 - mock_ossl$clay.tot_usda.c60_w.pct - 
                                        mock_ossl$sand.tot_usda.c60_w.pct
  mock_ossl$ph.h2o_usda.a268_index <- runif(n_samples, 4, 9)
  mock_ossl$oc_usda.c729_w.pct <- runif(n_samples, 0.1, 10)  # %
  mock_ossl$cec_usda.a723_cmolc.kg <- runif(n_samples, 5, 50)
  
  # Mock the key functions
  mock_get_processed_ossl <- function(properties, variance_threshold = 0.95) {
    # Filter to requested properties
    prop_cols <- character()
    if ("clay" %in% properties) prop_cols <- c(prop_cols, "clay.tot_usda.c60_w.pct")
    if ("ph" %in% properties) prop_cols <- c(prop_cols, "ph.h2o_usda.a268_index")
    if ("oc" %in% properties) prop_cols <- c(prop_cols, "oc_usda.c729_w.pct")
    
    spec_cols <- grep("^scan_mir\\.", names(mock_ossl), value = TRUE)
    
    data <- mock_ossl[c("sample_id", spec_cols, prop_cols)]
    
    # Simple mock PCA
    spec_matrix <- as.matrix(data[spec_cols])
    n_components <- min(20, ncol(spec_matrix))
    
    pca_scores <- tibble::tibble(sample_id = data$sample_id)
    for (i in 1:n_components) {
      pca_scores[[paste0("Dim.", i)]] <- rnorm(n_samples, 0, 10/sqrt(i))
    }
    
    # Add properties
    for (prop in prop_cols) {
      pca_scores[[prop]] <- data[[prop]]
    }
    
    list(
      data = pca_scores,
      pca_model = list(
        var = matrix(rnorm(length(spec_cols) * n_components), 
                    nrow = length(spec_cols)),
        eig = data.frame(
          eigenvalue = seq(10, 0.1, length.out = n_components),
          percentage = seq(30, 0.5, length.out = n_components),
          cumulative = cumsum(seq(30, 0.5, length.out = n_components))
        )
      ),
      pca_scores = pca_scores,
      n_components = n_components,
      preprocessing_params = list(smooth_window = 9, smooth_poly = 1)
    )
  }
  
  testthat::with_mocked_bindings(
    get_processed_ossl_training_data = mock_get_processed_ossl,
    code,
    .package = "horizons"
  )
}

#' Mock Cubist Model Fitting
#'
#' Replaces Cubist model fitting with fast mock version for testing.
#'
#' @param code Code to execute with mocked Cubist.
#' @param fixed_performance Logical. Use fixed performance metrics.
#'
#' @return Result of executing code with mocked Cubist fitting.
with_mocked_cubist <- function(code, fixed_performance = FALSE) {
  
  mock_fit_cubist <- function(train_data, val_data, covariate, 
                             bayesian_iter = 10, ...) {
    
    # Create mock model object
    mock_model <- structure(
      list(
        call = match.call(),
        covariate = covariate,
        n_train = nrow(train_data),
        n_val = nrow(val_data)
      ),
      class = "cubist"
    )
    
    # Mock predict method
    attr(mock_model, "predict") <- function(object, newdata) {
      # Simple linear prediction based on first PC
      if ("Dim.1" %in% names(newdata)) {
        base_pred <- 300 + 20 * newdata$Dim.1 + rnorm(nrow(newdata), 0, 30)
      } else {
        base_pred <- rnorm(nrow(newdata), 300, 50)
      }
      pmax(0, pmin(1000, base_pred))
    }
    
    # Performance metrics
    if (fixed_performance) {
      performance <- list(
        train_rmse = 45.2,
        val_rmse = 52.3,
        train_r2 = 0.75,
        val_r2 = 0.68
      )
    } else {
      performance <- list(
        train_rmse = runif(1, 30, 70),
        val_rmse = runif(1, 40, 80),
        train_r2 = runif(1, 0.6, 0.85),
        val_r2 = runif(1, 0.5, 0.75)
      )
    }
    
    list(
      model = mock_model,
      best_params = list(
        committees = sample(10:50, 1),
        neighbors = sample(0:9, 1),
        max_rules = sample(50:200, 1)
      ),
      optimization_history = tibble::tibble(
        iteration = 1:bayesian_iter,
        committees = sample(10:50, bayesian_iter),
        neighbors = sample(0:9, bayesian_iter, replace = TRUE),
        max_rules = sample(50:200, bayesian_iter),
        score = cummax(runif(bayesian_iter, 0.5, 0.8))
      ),
      performance = performance,
      predictions = list(
        train = rnorm(nrow(train_data), 300, 50),
        val = rnorm(nrow(val_data), 300, 50)
      )
    )
  }
  
  testthat::with_mocked_bindings(
    fit_cubist_model = mock_fit_cubist,
    code,
    .package = "horizons"
  )
}