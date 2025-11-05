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

#' Mock covariate prediction pipeline components
#'
#' Provides a fast path through `predict_soil_covariates()` by mocking the heavy
#' OSSL, PCA, clustering, and Cubist training steps. Allows tests to exercise the
#' orchestration logic without spending minutes fitting models.
#'
#' @param code Code block that calls `predict_soil_covariates()`.
#' @param covariates Character vector of covariates to include in mock OSSL data.
#' @param target_clusters Integer target number of clusters to emulate.
#' @param prediction_values Named numeric vector giving default predictions per covariate.
#' @param validation_metrics Named numeric vector giving validation metric defaults.
#' @param n_ossl_samples Integer number of mock OSSL samples.
#' @param n_components Integer number of PCA components to expose.
#' @param seed Optional seed for reproducibility.
#'
#' @return Result of executing `code` with mocks active.
with_mocked_covariate_pipeline <- function(code,
                                           covariates = c("clay"),
                                           target_clusters = 2,
                                           prediction_values = NULL,
                                           validation_metrics = NULL,
                                           n_ossl_samples = 80,
                                           n_components = 3,
                                           seed = 123) {

  if (!is.null(seed)) set.seed(seed)

  spectral_cols <- as.character(seq(600, 780, by = 30))
  all_covariates <- unique(c("clay", "ph", covariates))

  make_mock_ossl <- function(n_samples, props) {
    base <- tibble::tibble(
      Sample_ID = paste0("OSSL_", sprintf("%04d", seq_len(n_samples))),
      Project   = "MockProject"
    )

    spectral_matrix <- matrix(
      runif(n_samples * length(spectral_cols), min = 0.2, max = 0.8),
      nrow = n_samples,
      ncol = length(spectral_cols)
    )

    for (i in seq_along(spectral_cols)) {
      base[[spectral_cols[i]]] <- spectral_matrix[, i]
    }

    for (prop in props) {
      base[[prop]] <- runif(n_samples, min = 5, max = 15)
    }

    base
  }

  ossl_full <- make_mock_ossl(n_ossl_samples, all_covariates)

  default_predictions <- stats::setNames(rep(10, length(all_covariates)), all_covariates)
  if (!is.null(prediction_values)) {
    default_predictions[names(prediction_values)] <- prediction_values
  }

  default_metrics <- c(
    rmse  = 0.25,
    mae   = 0.15,
    rsq   = 0.85,
    ccc   = 0.9,
    rpd   = 2.2,
    rrmse = 6.0
  )
  if (!is.null(validation_metrics)) {
    default_metrics[names(validation_metrics)] <- validation_metrics
  }

  cov_env <- new.env(parent = emptyenv())
  cov_env$target_k <- target_clusters

  mock_get_ossl <- function(properties, ...) {
    props <- intersect(properties, names(ossl_full))
    missing_props <- setdiff(properties, props)
    mock_data <- ossl_full
    for (prop in missing_props) {
      mock_data[[prop]] <- runif(nrow(mock_data), min = 5, max = 15)
    }
    mock_data
  }

  mock_preprocess <- function(spectral_data, ...) spectral_data

  mock_perform_pca <- function(ossl_data, variance_threshold = 0.985, verbose = TRUE, ...) {
    n <- nrow(ossl_data)
    scores <- tibble::tibble(Sample_ID = ossl_data$Sample_ID)
    for (i in seq_len(n_components)) {
      scores[[paste0("Dim.", i)]] <- seq_len(n) / (i + 5)
    }
    for (prop in all_covariates) {
      if (prop %in% names(ossl_data)) {
        scores[[prop]] <- ossl_data[[prop]]
      }
    }
    list(
      pca_model       = list(n_components = n_components),
      ossl_pca_scores = scores,
      n_components    = n_components
    )
  }

  mock_project_pca <- function(new_data, pca_model, verbose = TRUE, ...) {
    n <- nrow(new_data)
    scores <- tibble::tibble(Sample_ID = new_data$Sample_ID)
    for (i in seq_len(n_components)) {
      scores[[paste0("Dim.", i)]] <- seq_len(n) / (i + 10)
    }
    for (prop in all_covariates) {
      scores[[prop]] <- if (prop %in% names(new_data)) new_data[[prop]] else rep(NA_real_, n)
    }
    scores
  }

  mock_fit_cubist <- function(train_data, val_data, covariate, ..., bayesian_iter = 10) {
    pred_value <- default_predictions[[covariate]]
    metrics_tbl <- tibble::tibble(
      rmse  = default_metrics[["rmse"]],
      mae   = default_metrics[["mae"]],
      rsq   = default_metrics[["rsq"]],
      ccc   = default_metrics[["ccc"]],
      rpd   = default_metrics[["rpd"]],
      rrmse = default_metrics[["rrmse"]]
    )
    workflow <- structure(
      list(covariate = covariate, prediction = pred_value),
      class = "mock_covariate_workflow"
    )
    list(
      fitted_workflow    = workflow,
      validation_metrics = metrics_tbl,
      best_params        = list(committees = 5, neighbors = 2),
      optimization_history = tibble::tibble(
        iteration  = seq_len(max(1, bayesian_iter)),
        committees = 5,
        neighbors  = 2,
        score      = seq_len(max(1, bayesian_iter)) / max(1, bayesian_iter)
      )
    )
  }

  mock_kmeans <- function(x, centers, ...) {
    cluster <- rep(seq_len(centers), length.out = nrow(x))
    cov_env$last_cluster <- cluster
    list(
      cluster = cluster,
      centers = matrix(seq_len(centers), nrow = centers, ncol = ncol(x))
    )
  }

  mock_silhouette <- function(x, dist, ...) {
    cluster <- as.integer(x)
    k <- length(unique(cluster))
    value <- if (k == cov_env$target_k) 0.9 else 0.5
    matrix(
      c(cluster, cluster, rep(value, length(cluster))),
      ncol = 3,
      dimnames = list(NULL, c("cluster", "neighbor", "sil_width"))
    )
  }

  mock_hclust <- function(d, method = "ward.D2", ...) {
    structure(list(n = attr(d, "Size")), class = "mock_hclust")
  }

  mock_cutree <- function(tree, k) {
    n <- if (!is.null(tree$n)) tree$n else 1
    rep(seq_len(k), length.out = n)
  }

  cli_silence <- list(
    cli_text          = function(...) invisible(NULL),
    cli_warn          = function(...) invisible(NULL),
    cli_alert_info    = function(...) invisible(NULL),
    cli_alert_warning = function(...) invisible(NULL),
    cli_alert_success = function(...) invisible(NULL)
  )

  testthat::with_mocked_bindings({
    testthat::with_mocked_bindings({
      testthat::with_mocked_bindings({
        testthat::with_mocked_bindings({
          code
        },
        cli_text          = cli_silence$cli_text,
        cli_warn          = cli_silence$cli_warn,
        cli_alert_info    = cli_silence$cli_alert_info,
        cli_alert_warning = cli_silence$cli_alert_warning,
        cli_alert_success = cli_silence$cli_alert_success,
        .package = "cli")
      },
      silhouette = mock_silhouette,
      .package = "cluster")
    },
    kmeans = mock_kmeans,
    hclust = mock_hclust,
    cutree = mock_cutree,
    .package = "stats")
  },
  get_ossl_training_data = mock_get_ossl,
  preprocess_mir_spectra = mock_preprocess,
  perform_pca_on_ossl    = mock_perform_pca,
  project_spectra_to_pca = mock_project_pca,
  fit_cubist_model       = mock_fit_cubist,
  .package = "horizons")
}

#' Predict method for mocked covariate workflows
#'
#' Ensures mocked Cubist workflows return deterministic predictions.
predict.mock_covariate_workflow <- function(object, new_data, ...) {
  if (!is.null(object$covariate) && object$covariate %in% names(new_data)) {
    preds <- new_data[[object$covariate]]
    preds[is.na(preds)] <- object$prediction
  } else {
    preds <- rep(object$prediction, nrow(new_data))
  }
  tibble::tibble(.pred = preds)
}

#' Mock stacks (tidymodels) infrastructure for ensemble testing
#'
#' Provides lightweight replacements for stacks functions so that ensemble
#' tests can exercise control flow without requiring real model objects.
#'
#' @param code Code to execute with mocked stacks functions.
#' @param weights Numeric vector of weights to attach to mocked members.
#' @param predict_offset Numeric offset added to predictions for determinism.
#'
#' @return Result of executing code with stacks mocks active.
with_mocked_stacks <- function(code,
                               weights = c(0.7, 0.3),
                               predict_offset = 0.15) {

  base_predict <- stats::predict

  mock_stacks_constructor <- function() {
    structure(list(candidates = list(),
                   blended   = FALSE),
              class = "mock_stacks_state")
  }

  mock_add_candidates <- function(stack, candidates, name, ...) {
    stack$candidates[[name]] <- candidates
    stack
  }

  mock_blend_predictions <- function(stack, penalty, mixture, metric, control, ...) {
    stack$blended <- list(penalty = penalty, mixture = mixture, metric = metric)
    stack
  }

  mock_fit_members <- function(stack, ...) {
    predict_fun <- function(newdata) {
      baseline <- if (!is.null(newdata) && "Response" %in% names(newdata)) {
        newdata$Response
      } else if (!is.null(newdata) && nrow(newdata) > 0) {
        rep(0, nrow(newdata))
      } else {
        numeric()
      }

      tibble::tibble(.pred = baseline + predict_offset)
    }

    ## Create mock member_fits (fitted workflows) for each candidate ----------
    ## This allows extract_stacks_members() to work correctly

    member_names <- names(stack$candidates)
    member_fits <- purrr::map(member_names, function(name) {
      structure(
        list(id = name, mock = TRUE),
        class = c("mock_workflow", "workflow")
      )
    })
    names(member_fits) <- member_names

    structure(list(
      cols_map        = stack$candidates,
      member_fits     = member_fits,  # Added for extract_stacks_members()
      predict_offset  = predict_offset,
      blended         = stack$blended,
      predict_fun     = predict_fun
    ),
    class = c("mock_stacks_model", "stacks_model"))
  }

  mock_autoplot <- function(object, type = "weights", ...) {
    members <- names(object$cols_map)
    if (length(members) == 0) {
      members <- character()
    }

    raw_weights <- rep_len(weights, length(members))
    normalized  <- if (length(raw_weights) > 0) raw_weights / sum(raw_weights) else numeric()

    weight_data <- tibble::tibble(
      member = members,
      weight = normalized
    )

    structure(
      list(data = weight_data),
      class = c("mock_stacks_autoplot", "ggplot")
    )
  }
  mock_stats_predict <- function(object, newdata = NULL, ...) {
    if (inherits(object, "mock_stacks_model") && !is.null(object$predict_fun)) {
      object$predict_fun(newdata)
    } else {
      base_predict(object, newdata, ...)
    }
  }

  testthat::with_mocked_bindings(
    stacks            = mock_stacks_constructor,
    add_candidates    = mock_add_candidates,
    blend_predictions = mock_blend_predictions,
    fit_members       = mock_fit_members,
    autoplot          = mock_autoplot,
    code = code,
    .package = "stacks"
  )
}

#' Predict method for mocked stacks models
#'
#' @param object Mock stacks model created by with_mocked_stacks().
#' @param new_data New data frame containing Response column.
#'
#' @return Tibble with `.pred` column.
predict.mock_stacks_model <- function(object, new_data, ...) {
  offset <- object$predict_offset
  if (is.null(offset)) offset <- 0

  baseline <- if ("Response" %in% names(new_data)) {
    new_data$Response
  } else if (nrow(new_data) > 0) {
    rep(0, nrow(new_data))
  } else {
    numeric()
  }

  tibble::tibble(.pred = baseline + offset)
}

#' Predict method for mock workflows in stacks tests
#'
#' @param object Mock workflow created by with_mocked_stacks().
#' @param new_data New data frame for predictions.
#'
#' @return Tibble with `.pred` column.
predict.mock_workflow <- function(object, new_data, ...) {
  # Mock workflows return Response with small noise
  # This simulates predictions from fitted models

  if ("Response" %in% names(new_data)) {
    baseline <- new_data$Response
  } else {
    baseline <- rep(5, nrow(new_data))
  }

  # Add small random offset to simulate different models
  offset <- if (!is.null(object$id)) {
    # Use hash of id to get consistent but different offsets
    hash_val <- sum(utf8ToInt(object$id))
    (hash_val %% 100) / 100 - 0.5  # Range: -0.5 to 0.5
  } else {
    0
  }

  tibble::tibble(.pred = baseline + offset)
}

# Register the S3 method
base::registerS3method("predict", "mock_workflow", predict.mock_workflow)

#' Mock XGBoost training for ensemble meta-learner tests
#'
#' Replaces xgboost::xgboost with a deterministic stub that records feature
#' names and returns predictable predictions.
#'
#' @param code Code to execute with mocked XGBoost functions.
#' @param gain_values Optional named numeric vector of feature gains.
#' @param predict_offset Numeric offset applied to predictions.
#'
#' @return Result of executing code with XGBoost mocks active.
with_mocked_xgboost <- function(code,
                                gain_values = NULL,
                                predict_offset = 0.2) {

  mock_xgboost <- function(data,
                           label,
                           params,
                           nrounds,
                           verbose = 0,
                           ...) {
    feature_names <- colnames(data)
    if (is.null(feature_names)) {
      feature_names <- character()
    }

    structure(
      list(
        feature_names = feature_names,
        gain_values   = gain_values,
        predict_offset = predict_offset
      ),
      class = c("mock_xgb_booster", "xgb.Booster")
    )
  }

  mock_importance <- function(model, ...) {
    features <- model$feature_names
    if (length(features) == 0 && !is.null(model$gain_values)) {
      features <- names(model$gain_values)
    }

    if (length(features) == 0) {
      return(tibble::tibble(Feature = character(), Gain = numeric()))
    }

    gains <- model$gain_values
    if (is.null(gains) || length(gains) == 0) {
      gains <- rep(1 / length(features), length(features))
    } else {
      gains <- rep_len(gains, length(features))
      if (abs(sum(gains) - 1) > 1e-8) {
        gains <- gains / sum(gains)
      }
    }

    tibble::tibble(
      Feature = features,
      Gain    = gains
    )
  }

  testthat::with_mocked_bindings(
    xgboost             = mock_xgboost,
    xgb.importance      = mock_importance,
    code,
    .package = "xgboost"
  )
}

#' Predict method for mocked xgboost booster
#'
#' @param object Mock booster produced by with_mocked_xgboost().
#' @param new_data Numeric matrix of meta-features.
#'
#' @return Numeric vector of predictions.
predict.mock_xgb_booster <- function(object, newdata, ...) {
  offset <- object$predict_offset
  if (is.null(offset)) offset <- 0

  if (is.matrix(newdata) && nrow(newdata) > 0) {
    base <- rowMeans(newdata)
  } else if (is.vector(newdata)) {
    base <- rep(mean(newdata), length(newdata))
  } else {
    base <- 0
  }

  as.numeric(base + offset)
}

#' Mock tune::collect_predictions for list-backed CV results
#'
#' Converts the list structure produced by create_mock_finalized_models() into
#' a tibble so that ensemble tests can reuse the same fixtures.
#'
#' @param code Code to execute with mocked collect_predictions().
#'
#' @return Result of executing code with mocked tuning helpers.
with_mocked_collect_predictions <- function(code) {
  mock_collect <- function(x, ...) {
    if (inherits(x, "data.frame")) {
      tibble::as_tibble(x)
    } else if (is.list(x) && !is.null(x$.predictions)) {
      dplyr::bind_rows(x$.predictions)
    } else {
      stop("Unsupported object for collect_predictions mock", call. = FALSE)
    }
  }

  testthat::with_mocked_bindings(
    collect_predictions = mock_collect,
    code,
    .package = "tune"
  )
}
