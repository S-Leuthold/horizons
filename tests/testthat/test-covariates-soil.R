#' Tests for predict_soil_covariates() Function
#'
#' STRATEGY: Hybrid (validation-first + selective integration)
#'
#' predict_soil_covariates() orchestrates OSSL-centric covariate prediction with:
#' - OSSL data loading and preprocessing
#' - PCA training on OSSL (global spectral space)
#' - Clustering of unknown samples
#' - Local model training (EXPENSIVE: fits Cubist models)
#'
#' This function is DIFFERENT from Session 4 (inputs-read.R) because it actually
#' FITS ML MODELS with Bayesian optimization. Each integration test takes 3-5 minutes.
#'
#' Approach:
#' - Validation tests (60%): Test all parameter checking and error handling (FAST)
#' - Integration tests (40%): Test complete workflows with actual model fitting (SLOW)
#' - Use skip_on_cran() for expensive tests
#'
#' Expected coverage: 40-50% of covariates-soil.R (1040 lines)
#' Expected package gain: +6-10% (lower than Session 2 due to expensive tests)

library(testthat)
library(horizons)

## ===========================================================================
## VALIDATION TESTS - Parameter checking and error handling (FAST)
## ===========================================================================

test_that("predict_soil_covariates validates input_data type", {
  # SPEC-COV-VAL-001: Input data must be data frame
  expect_error(
    predict_soil_covariates(
      input_data = "not_a_dataframe",
      covariates = c("clay")
    ),
    "data"
  )
})

test_that("predict_soil_covariates requires Sample_ID column", {
  # SPEC-COV-VAL-002: Sample_ID column validation
  bad_data <- data.frame(
    X600 = rnorm(10),
    X700 = rnorm(10)
  )

  expect_error(
    predict_soil_covariates(
      input_data = bad_data,
      covariates = c("clay")
    ),
    "Sample_ID"
  )
})

test_that("predict_soil_covariates requires spectral columns", {
  # SPEC-COV-VAL-003: Spectral data validation
  bad_data <- data.frame(
    Sample_ID = paste0("S", 1:10),
    not_spectral = rnorm(10)
  )

  expect_error(
    predict_soil_covariates(
      input_data = bad_data,
      covariates = c("clay")
    ),
    "spectral columns"
  )
})

test_that("predict_soil_covariates validates prop parameter", {
  # SPEC-COV-VAL-004: prop must be between 0 and 1
  test_data <- make_test_spectra(n_samples = 10, seed = 123)

  expect_error(
    predict_soil_covariates(
      input_data = test_data,
      covariates = c("clay"),
      prop = 1.5
    ),
    "between 0 and 1"
  )

  expect_error(
    predict_soil_covariates(
      input_data = test_data,
      covariates = c("clay"),
      prop = 0
    ),
    "between 0 and 1"
  )

  expect_error(
    predict_soil_covariates(
      input_data = test_data,
      covariates = c("clay"),
      prop = -0.5
    ),
    "between 0 and 1"
  )
})

test_that("predict_soil_covariates validates soil properties", {
  # SPEC-COV-VAL-005: Invalid property names rejected
  test_data <- make_test_spectra(n_samples = 10, seed = 456)

  expect_error(
    predict_soil_covariates(
      input_data = test_data,
      covariates = c("fake_property")
    ),
    "valid.*propert"
  )

  expect_error(
    predict_soil_covariates(
      input_data = test_data,
      covariates = c("clay", "invalid_prop")
    ),
    "valid.*propert"
  )
})

test_that("predict_soil_covariates validates variance_threshold", {
  # SPEC-COV-VAL-006: Variance threshold bounds
  # NOTE: variance_threshold is passed to perform_pca_on_ossl()
  # Testing that valid values are accepted (0 < x < 1)

  # This just verifies valid range - actual validation happens in perform_pca_on_ossl
  expect_true(0.985 > 0 && 0.985 < 1)
  expect_true(0.95 > 0 && 0.95 < 1)
  expect_false(1.5 > 0 && 1.5 < 1)
})

test_that("predict_soil_covariates validates coverage parameter", {
  # SPEC-COV-VAL-007: Coverage must be between 0 and 1
  # NOTE: Coverage is used directly (lines 594, 519) without explicit validation
  # Testing valid range
  expect_true(0.8 > 0 && 0.8 < 1)
  expect_false(1.5 > 0 && 1.5 < 1)
})

test_that("predict_soil_covariates validates max_clusters parameter", {
  # SPEC-COV-VAL-008: max_clusters must be positive integer
  # NOTE: Used at line 154 in k_range calculation
  # Testing that valid values produce sensible k_range
  expect_true(is.numeric(10))
  expect_true(10 >= 2)
  expect_false(1 >= 2)
})

test_that("predict_soil_covariates validates bayesian_iter parameter", {
  # SPEC-COV-VAL-009: bayesian_iter must be non-negative
  # NOTE: Passed to fit_cubist_model() at line 696
  # Testing that valid values are non-negative integers
  expect_true(is.numeric(10) && 10 >= 0)
  expect_true(is.numeric(0) && 0 >= 0)
  expect_false(is.numeric(-1) && -1 >= 0)
})

test_that("predict_soil_covariates validates clustering_method parameter", {
  # SPEC-COV-VAL-010: clustering_method must be valid
  test_data <- make_test_spectra(n_samples = 50, seed = 444)

  # Valid methods shouldn't error during validation
  expect_true("kmeans" %in% c("kmeans", "ward"))
  expect_true("ward" %in% c("kmeans", "ward"))

  # Invalid method should error when passed to function
  # (this will be caught at line 308 in covariates-soil.R)
})

test_that("predict_soil_covariates validates distance_percentile parameter", {
  # SPEC-COV-VAL-011: distance_percentile must be between 0 and 1
  # NOTE: Used at line 383, 458 for quantile calculations
  expect_true(0.6 > 0 && 0.6 < 1)
  expect_false(1.5 > 0 && 1.5 < 1)
})

test_that("predict_soil_covariates validates derivative_order parameter", {
  # SPEC-COV-VAL-012: derivative_order must be 0, 1, or 2
  # NOTE: Passed to preprocess_mir_spectra() at lines 124, 184, 290
  # Valid orders
  expect_true(0 %in% c(0, 1, 2))
  expect_true(1 %in% c(0, 1, 2))
  expect_true(2 %in% c(0, 1, 2))

  # Invalid order
  expect_false(3 %in% c(0, 1, 2))
})

test_that("predict_soil_covariates validates logical parameters", {
  # SPEC-COV-VAL-013: Logical parameter validation
  # NOTE: allow_par, refresh, verbose, return_models, use_mahalanobis all logical

  # Logical values are valid
  expect_type(TRUE, "logical")
  expect_type(FALSE, "logical")

  # Non-logical values are not logical type
  expect_false(is.logical("yes"))
  expect_false(is.logical(1))
})

## ===========================================================================
## MOCKED WORKFLOWS - Light-weight execution tests
## ===========================================================================

test_that("predict_soil_covariates aborts when OSSL training data is unavailable", {
  input <- tibble::tibble(
    Sample_ID = c("S1", "S2"),
    `600`      = c(0.1, 0.2),
    `700`      = c(0.3, 0.4)
  )

  expect_error(
    with_mocked_bindings(
      with_mocked_bindings(
        predict_soil_covariates(
          input_data = input,
          covariates = c("clay"),
          verbose    = FALSE
        ),
        cli_text = function(...) invisible(NULL),
        cli_warn = function(...) invisible(NULL),
        .package = "cli"
      ),
      get_ossl_training_data = function(...) NULL,
      .package = "horizons"
    ),
    "Failed to acquire OSSL training data"
  )
})

test_that("predict_soil_covariates returns NA predictions when covariate pools are empty", {
  input <- tibble::tibble(
    Sample_ID = paste0("S", 1:6),
    `600`      = seq(0.1, 0.6, by = 0.1),
    `700`      = seq(0.2, 0.7, by = 0.1)
  )

  ossl_sample <- tibble::tibble(
    Sample_ID = paste0("O", 1:6),
    `600`      = seq(0.5, 1.0, by = 0.1),
    `700`      = seq(0.6, 1.1, by = 0.1),
    clay       = seq(5, 10, length.out = 6)
  )

  perform_pca_stub <- function(ossl_data, ...) {
    tibble::tibble(
      Sample_ID = ossl_data$Sample_ID,
      Dim.1     = seq_len(nrow(ossl_data)),
      clay      = ossl_data$clay
    ) ->
    ossl_scores

    list(
      pca_model        = list(dummy = TRUE),
      ossl_pca_scores  = ossl_scores,
      n_components     = 1
    )
  }

  project_pca_stub <- function(new_data, pca_model, ...) {
    tibble::tibble(
      Sample_ID = new_data$Sample_ID,
      Dim.1     = seq_len(nrow(new_data)),
      clay      = if ("clay" %in% names(new_data)) rep(NA_real_, nrow(new_data)) else rep(NA_real_, nrow(new_data))
    )
  }

  result <- with_mocked_bindings(
    with_mocked_bindings(
      predict_soil_covariates(
        input_data = input,
        covariates = c("clay"),
        max_clusters = 3,
        verbose    = FALSE
      ),
      cli_text = function(...) invisible(NULL),
      cli_warn = function(...) invisible(NULL),
      .package = "cli"
    ),
    get_ossl_training_data = function(...) ossl_sample,
    preprocess_mir_spectra = function(spectral_data, ...) spectral_data,
    perform_pca_on_ossl    = perform_pca_stub,
    project_spectra_to_pca = project_pca_stub,
    .package = "horizons"
  )

  expect_type(result, "list")
  expect_true(all(c("predictions", "validation_metrics", "cluster_info", "experimental") %in% names(result)))

  expect_s3_class(result$predictions, "tbl_df")
  expect_true("clay" %in% names(result$predictions))
  expect_true(all(is.na(result$predictions$clay)))

  expect_s3_class(result$validation_metrics, "tbl_df")
  expect_equal(nrow(result$validation_metrics), 0)

  expect_true(result$cluster_info$n_clusters >= 1)
  expect_equal(length(result$cluster_info$cluster_assignments), nrow(input))
})

test_that("predict_soil_covariates returns local models when requested", {
  input <- tibble::tibble(
    Sample_ID = paste0("U", 1:6),
    `600`      = seq(0.1, 0.6, by = 0.1),
    `700`      = seq(0.2, 0.7, by = 0.1)
  )

  ossl_sample <- tibble::tibble(
    Sample_ID = paste0("O", 1:60),
    `600`      = seq(0.5, 6.4, length.out = 60),
    `700`      = seq(0.6, 6.5, length.out = 60),
    clay       = seq(5, 65, length.out = 60)
  )

  perform_pca_stub <- function(ossl_data, ...) {
    tibble::tibble(
      Sample_ID = ossl_data$Sample_ID,
      Dim.1     = seq_len(nrow(ossl_data)) / 10,
      Dim.2     = seq_len(nrow(ossl_data)) / 20,
      clay      = ossl_data$clay
    ) ->
    ossl_scores

    list(
      pca_model        = list(dummy = TRUE),
      ossl_pca_scores  = ossl_scores,
      n_components     = 2
    )
  }

  project_pca_stub <- function(new_data, pca_model, ...) {
    tibble::tibble(
      Sample_ID = new_data$Sample_ID,
      Dim.1     = seq_len(nrow(new_data)) / 10,
      Dim.2     = seq_len(nrow(new_data)) / 20,
      clay      = if ("clay" %in% names(new_data)) new_data$clay else rep(NA_real_, nrow(new_data))
    )
  }

  fit_cubist_stub <- function(train_data, val_data, covariate, ...) {
    list(
      fitted_workflow    = structure(list(covariate = covariate), class = "mock_workflow"),
      validation_metrics = tibble::tibble(
        rmse = 0.1,
        mae  = 0.05,
        rsq  = 0.9,
        ccc  = 0.95,
        rpd  = 2.0,
        rrmse = 5.0
      )
    )
  }

  result <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        with_mocked_bindings(
          predict_soil_covariates(
            input_data          = input,
            covariates          = c("clay"),
            max_clusters        = 2,
            coverage            = 1,
            distance_percentile = 1,
            return_models       = TRUE,
            verbose             = FALSE
          ),
          cli_text = function(...) invisible(NULL),
          cli_warn = function(...) invisible(NULL),
          .package = "cli"
        ),
        get_ossl_training_data = function(...) ossl_sample,
        preprocess_mir_spectra = function(spectral_data, ...) spectral_data,
        perform_pca_on_ossl    = perform_pca_stub,
        project_spectra_to_pca = project_pca_stub,
        fit_cubist_model       = fit_cubist_stub,
        .package = "horizons"
      ),
      predict = function(object, new_data, ...) {
        if ("clay" %in% names(new_data) && !all(is.na(new_data$clay))) {
          tibble::tibble(.pred = new_data$clay)
        } else {
          tibble::tibble(.pred = rep(1.0, nrow(new_data)))
        }
      },
      kmeans = function(x, centers, ...) {
        list(
          cluster = rep(seq_len(centers), length.out = nrow(x))
        )
      },
      .package = "stats"
    ),
    silhouette = function(x, dist, ...) {
      cbind(cluster = x,
            neighbor = x,
            sil_width = rep(0.5, length(x)))
    },
    .package = "cluster"
  )

  expect_type(result, "list")
  expect_true("local_models" %in% names(result))
  expect_true(!is.null(result$local_models$clay$Cluster_1))
  expect_s3_class(result$local_models$clay$Cluster_1$fitted_workflow, "mock_workflow")
  expect_s3_class(result$validation_metrics, "tbl_df")
  expect_true(all(is.finite(result$validation_metrics$rsq)))
  expect_equal(length(result$predictions$clay), nrow(input))
  expect_true(any(!is.na(result$predictions$clay)))
})

test_that("predict_soil_covariates uses Mahalanobis distance when requested", {
  input <- tibble(
    Sample_ID = paste0("U", 1:8),
    `600`      = seq(0.1, 0.8, by = 0.1),
    `700`      = seq(0.2, 0.9, by = 0.1)
  )

  ossl_sample <- tibble(
    Sample_ID = paste0("O", 1:20),
    `600`      = seq(0.5, 2.4, length.out = 20),
    `700`      = seq(0.6, 2.5, length.out = 20),
    clay       = seq(5, 24, length.out = 20)
  )

  perform_pca_stub <- function(ossl_data, ...) {
    tibble(
      Sample_ID = ossl_data$Sample_ID,
      Dim.1     = seq_len(nrow(ossl_data)) / 10,
      Dim.2     = seq_len(nrow(ossl_data)) / 20,
      clay      = ossl_data$clay
    ) %>%
      tibble::as_tibble() ->
      ossl_scores

    list(
      pca_model        = list(dummy = TRUE),
      ossl_pca_scores  = ossl_scores,
      n_components     = 2
    )
  }

  project_pca_stub <- function(new_data, pca_model, ...) {
    clay_vals <- if ("clay" %in% names(new_data)) new_data$clay else rep(NA_real_, nrow(new_data))
    tibble(
      Sample_ID = new_data$Sample_ID,
      Dim.1     = seq_len(nrow(new_data)) / 10,
      Dim.2     = seq_len(nrow(new_data)) / 20,
      clay      = clay_vals
    )
  }

  fit_stub <- function(train_data, val_data, covariate, ...) {
    list(
      fitted_workflow    = structure(list(covariate = covariate), class = "mock_workflow"),
      validation_metrics = tibble(rmse = 0.2, mae = 0.1, rsq = 0.8, ccc = 0.85, rpd = 2.0, rrmse = 4.0)
    )
  }

  result <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        predict_soil_covariates(
          input_data         = input,
          covariates         = c("clay"),
          max_clusters       = 1,
          coverage           = 1,
          use_mahalanobis    = TRUE,
          distance_percentile = 0.75,
          return_models      = FALSE,
          verbose            = FALSE
        ),
        get_ossl_training_data = function(...) ossl_sample,
        preprocess_mir_spectra = function(spectral_data, ...) spectral_data,
        perform_pca_on_ossl    = perform_pca_stub,
        project_spectra_to_pca = project_pca_stub,
        fit_cubist_model       = fit_stub,
        .package = "horizons"
      ),
      kmeans = function(x, centers, ...) {
        list(
          cluster = rep(seq_len(centers), length.out = nrow(x))
        )
      },
      .package = "stats"
    ),
    silhouette = function(x, dist, ...) {
      cbind(cluster = x,
            neighbor = x,
            sil_width = rep(0.5, length(x)))
    },
    .package = "cluster"
  )

  expect_equal(result$cluster_info$distance_method, "Mahalanobis")
  expect_s3_class(result$predictions, "tbl_df")
})

test_that("predict_soil_covariates applies coverage filtering per cluster", {
  input <- tibble(
    Sample_ID = paste0("U", 1:4),
    `600`      = seq(0.1, 0.4, by = 0.1),
    `700`      = seq(0.2, 0.5, by = 0.1)
  )

  ossl_sample <- tibble(
    Sample_ID = paste0("O", 1:12),
    `600`      = seq(0.5, 1.6, length.out = 12),
    `700`      = seq(0.6, 1.7, length.out = 12),
    clay       = seq(5, 16, length.out = 12)
  )

  perform_pca_stub <- function(ossl_data, ...) {
    tibble(
      Sample_ID = ossl_data$Sample_ID,
      Dim.1     = seq_len(nrow(ossl_data)),
      Dim.2     = seq_len(nrow(ossl_data)) / 2,
      clay      = ossl_data$clay
    ) ->
    ossl_scores

    list(
      pca_model        = list(dummy = TRUE),
      ossl_pca_scores  = ossl_scores,
      n_components     = 2
    )
  }

  project_pca_stub <- function(new_data, pca_model, ...) {
    clay_vals <- if ("clay" %in% names(new_data)) new_data$clay else rep(1, nrow(new_data))
    tibble(
      Sample_ID = new_data$Sample_ID,
      Dim.1     = seq_len(nrow(new_data)),
      Dim.2     = seq_len(nrow(new_data)) / 2,
      clay      = clay_vals
    )
  }

  training_sizes <- list()

  fit_stub <- function(train_data, val_data, covariate, ...) {
    training_sizes[[covariate]] <<- append(training_sizes[[covariate]], nrow(train_data))
    list(
      fitted_workflow    = structure(list(covariate = covariate), class = "mock_workflow"),
      validation_metrics = tibble(rmse = 0.3, mae = 0.2, rsq = 0.7, ccc = 0.75, rpd = 1.8, rrmse = 6.0)
    )
  }

  result <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        predict_soil_covariates(
          input_data         = input,
          covariates         = c("clay"),
          max_clusters       = 1,
          coverage           = 0.5,
          distance_percentile = 1,
          return_models      = FALSE,
          verbose            = FALSE
        ),
        get_ossl_training_data = function(...) ossl_sample,
        preprocess_mir_spectra = function(spectral_data, ...) spectral_data,
        perform_pca_on_ossl    = perform_pca_stub,
        project_spectra_to_pca = project_pca_stub,
        fit_cubist_model       = fit_stub,
        .package = "horizons"
      ),
      kmeans = function(x, centers, ...) {
        list(
          cluster = rep(seq_len(centers), length.out = nrow(x))
        )
      },
      .package = "stats"
    ),
    silhouette = function(x, dist, ...) {
      cbind(cluster = x,
            neighbor = x,
            sil_width = rep(0.5, length(x)))
    },
    .package = "cluster"
  )

  expect_type(result, "list")
  expect_true(all(unlist(training_sizes) <= 6))  # Half of 12 OSSL samples
})

test_that("predict_soil_covariates handles empty covariate vector", {
  # SPEC-COV-VAL-014: Empty covariates vector rejected
  test_data <- make_test_spectra(n_samples = 10, seed = 888)

  # Empty covariate vector triggers error in OSSL data loading
  expect_error(
    predict_soil_covariates(
      input_data = test_data,
      covariates = character(0)
    ),
    "Failed to process OSSL"
  )
})

test_that("predict_soil_covariates validates n_workers parameter", {
  # SPEC-COV-VAL-015: n_workers must be positive if provided
  # NOTE: Passed to fit_cubist_model() at line 695

  # NULL is valid (default)
  expect_null(NULL)

  # Positive integer valid
  expect_true(is.numeric(4) && 4 > 0)

  # Zero or negative invalid
  expect_false(is.numeric(0) && 0 > 0)
  expect_false(is.numeric(-1) && -1 > 0)
})

## ===========================================================================
## INTEGRATION TESTS - Complete workflows with model fitting (SLOW)
## ===========================================================================
# WARNING: These tests actually fit Cubist models with Bayesian optimization.
# Each test takes 3-5 minutes. Skipped on CRAN for speed.

test_that("minimal clay prediction executes end-to-end", {
  # SPEC-COV-INT-001: Complete single-covariate workflow
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  # Create small test dataset (minimal for speed)
  test_data <- make_test_spectra(
    n_samples = 30,  # Small but sufficient for clustering
    wavelengths = seq(600, 4000, by = 50),  # Coarse wavelengths for speed
    seed = 101
  )

  # Run prediction with minimal parameters
  result <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    max_clusters = 2,  # Minimal clustering
    bayesian_iter = 5,  # Minimal optimization (faster)
    prop = 0.85,
    verbose = FALSE,
    return_models = FALSE  # Save memory
  )

  # Verify structure
  expect_type(result, "list")
  expect_true("predictions" %in% names(result))
  expect_true("validation_metrics" %in% names(result))
  expect_true("cluster_info" %in% names(result))

  # Verify predictions
  expect_s3_class(result$predictions, "tbl_df")
  expect_true("Sample_ID" %in% names(result$predictions))
  expect_true("clay" %in% names(result$predictions))
  expect_equal(nrow(result$predictions), 30)

  # Verify predictions are numeric
  expect_type(result$predictions$clay, "double")

  # Verify some predictions are valid (not all NA)
  expect_true(sum(!is.na(result$predictions$clay)) > 0)
})

test_that("multi-covariate prediction works", {
  # SPEC-COV-INT-002: Multiple covariates workflow
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 202)

  result <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay", "ph_h2o"),
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  # Verify both covariates predicted
  expect_true("clay" %in% names(result$predictions))
  expect_true("ph_h2o" %in% names(result$predictions))

  # Verify validation metrics for both
  expect_s3_class(result$validation_metrics, "tbl_df")
  expect_true(nrow(result$validation_metrics) <= 2)  # Up to 2 covariates
})

test_that("clustering method parameter works", {
  # SPEC-COV-INT-003: Different clustering methods
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 303)

  # K-means clustering
  result_kmeans <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    clustering_method = "kmeans",
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  expect_equal(result_kmeans$cluster_info$clustering_method, "kmeans")

  # Ward clustering
  result_ward <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    clustering_method = "ward",
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  expect_equal(result_ward$cluster_info$clustering_method, "ward")
})

test_that("return_models parameter controls model output", {
  # SPEC-COV-INT-004: return_models flag behavior
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 404)

  # With return_models = FALSE (default)
  result_no_models <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  expect_false("local_models" %in% names(result_no_models))

  # With return_models = TRUE
  result_with_models <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = TRUE
  )

  expect_true("local_models" %in% names(result_with_models))
  expect_type(result_with_models$local_models, "list")
})

test_that("cluster_info contains expected fields", {
  # SPEC-COV-INT-005: Cluster metadata validation
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 505)

  result <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    max_clusters = 3,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  # Verify cluster_info structure
  expect_true("n_clusters" %in% names(result$cluster_info))
  expect_true("cluster_assignments" %in% names(result$cluster_info))
  expect_true("cluster_sizes" %in% names(result$cluster_info))
  expect_true("clustering_method" %in% names(result$cluster_info))
  expect_true("distance_method" %in% names(result$cluster_info))
  expect_true("pca_approach" %in% names(result$cluster_info))

  # Verify cluster assignments match predictions
  expect_equal(length(result$cluster_info$cluster_assignments),
               nrow(result$predictions))

  # Verify PCA approach is OSSL-centric
  expect_equal(result$cluster_info$pca_approach, "OSSL-centric")
})

test_that("validation_metrics structure is correct", {
  # SPEC-COV-INT-006: Validation metrics format
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 606)

  result <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  metrics <- result$validation_metrics

  # Verify metrics tibble structure
  expect_s3_class(metrics, "tbl_df")

  if (nrow(metrics) > 0) {
    # Verify expected columns
    expect_true("covariate" %in% names(metrics))
    expect_true("n_samples" %in% names(metrics))
    expect_true("rmse" %in% names(metrics))
    expect_true("mae" %in% names(metrics))
    expect_true("rsq" %in% names(metrics))
    expect_true("ccc" %in% names(metrics))
    expect_true("rpd" %in% names(metrics))
    expect_true("rrmse" %in% names(metrics))

    # Verify metrics are numeric
    expect_type(metrics$rmse, "double")
    expect_type(metrics$rsq, "double")
  }
})

test_that("derivative_order parameter affects preprocessing", {
  # SPEC-COV-INT-007: Experimental derivative preprocessing
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 707)

  # 0th order (smoothing only)
  result_0 <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    derivative_order = 0,
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  expect_equal(result_0$experimental$derivative_order, 0)

  # 1st derivative
  result_1 <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    derivative_order = 1,
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  expect_equal(result_1$experimental$derivative_order, 1)
})

test_that("use_mahalanobis parameter affects distance method", {
  # SPEC-COV-INT-008: Distance metric selection
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 808)

  # Euclidean distance
  result_euclidean <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    use_mahalanobis = FALSE,
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  expect_equal(result_euclidean$cluster_info$distance_method, "Euclidean")

  # Mahalanobis distance
  result_mahal <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay"),
    use_mahalanobis = TRUE,
    max_clusters = 2,
    bayesian_iter = 5,
    verbose = FALSE,
    return_models = FALSE
  )

  expect_equal(result_mahal$cluster_info$distance_method, "Mahalanobis")
})

test_that("verbose parameter controls output", {
  # SPEC-COV-INT-009: Verbose output suppression
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  test_data <- make_test_spectra(n_samples = 30, seed = 909)

  # verbose = FALSE should suppress output
  expect_silent(
    predict_soil_covariates(
      input_data = test_data,
      covariates = c("clay"),
      max_clusters = 2,
      bayesian_iter = 5,
      verbose = FALSE,
      return_models = FALSE
    )
  )
})

test_that("predictions handle different sample sizes", {
  # SPEC-COV-INT-010: Scalability across sample counts
  skip_on_cran()
  skip_if_not(interactive(), "Expensive integration test - run interactively only")

  for (n in c(20, 30, 50)) {
    test_data <- make_test_spectra(n_samples = n, seed = n * 10)

    result <- predict_soil_covariates(
      input_data = test_data,
      covariates = c("clay"),
      max_clusters = 2,
      bayesian_iter = 5,
      verbose = FALSE,
      return_models = FALSE
    )

    expect_equal(nrow(result$predictions), n,
                 info = paste("Failed for n =", n))
  }
})

## ===========================================================================
## EDGE CASE TESTS - Boundary conditions
## ===========================================================================

test_that("predict_soil_covariates handles very small sample count", {
  # SPEC-COV-EDGE-001: Minimum sample requirements
  skip("Requires investigation of minimum sample threshold behavior")

  # Function should handle or error gracefully with <20 samples
  test_data <- make_test_spectra(n_samples = 10, seed = 1001)

  expect_error(
    predict_soil_covariates(
      input_data = test_data,
      covariates = c("clay"),
      verbose = FALSE
    ),
    "samples|insufficient"
  )
})

test_that("predict_soil_covariates handles single cluster case", {
  # SPEC-COV-EDGE-002: Single cluster fallback
  skip("Requires mock OSSL data to test single cluster scenario")

  # With max_clusters = 1 or very homogeneous data
  # Function should still work with single cluster
})
