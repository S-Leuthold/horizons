#' Tests for fetch_covariates orchestrator

library(testthat)
library(horizons)
library(tibble)
library(dplyr)

test_that("fetch_covariates uses cached soil predictions when available", {
  cache_dir <- withr::local_tempdir()

  input <- tibble(
    Sample_ID = c("S1", "S2"),
    `600`      = c(0.1, 0.2),
    `700`      = c(0.3, 0.4)
  )

  spectral_cols <- c("600", "700")
  spectral_hash <- digest::digest(input[, spectral_cols], algo = "md5")

  cache_key <- list(
    spectral_hash      = spectral_hash,
    n_samples          = nrow(input),
    covariates         = sort(c("clay")),
    n_similar          = 100,
    variance_threshold = 0.9,
    bayesian_iter      = 0,
    prop_train         = 0.75
  )

  cache_hash      <- digest::digest(cache_key, algo = "md5")
  soil_cache_file <- file.path(cache_dir, paste0("soil_predictions_", cache_hash, ".qs"))
  file.create(soil_cache_file)

  soil_predictions <- list(
    predictions = tibble(Sample_ID = input$Sample_ID, clay = c(1.1, 1.2)),
    validation_metrics = tibble(covariate = "clay", rsq = 0.85, rmse = 0.1, rpd = 2.1),
    cluster_info       = list(n_clusters = 1),
    local_models       = NULL
  )

  cache_object <- list(
    cache_key   = cache_key,
    predictions = soil_predictions,
    timestamp   = Sys.time()
  )

  result <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        fetch_covariates(
          input_data      = input,
          soil_covariates = c("clay"),
          n_similar       = 100,
          variance_threshold = 0.9,
          bayesian_iter   = 0,
          prop_train      = 0.75,
          cache_dir       = cache_dir,
          refresh_soil    = FALSE,
          verbose         = TRUE
        ),
        cli_text = function(...) invisible(NULL),
        cli_warn = function(...) invisible(NULL),
        cli_abort = function(message, ...) stop(message, call. = FALSE),
        .package = "cli"
      ),
      predict_soil_covariates = function(...) stop("predict_soil_covariates should not run when cache hits"),
      .package = "horizons"
    ),
    qread = function(path, ...) {
      expect_equal(path, soil_cache_file)
      cache_object
    },
    qsave = function(...) stop("qsave should not run when cache hits"),
    .package = "qs"
  )

  expect_s3_class(result$covariate_data, "tbl_df")
  expect_equal(result$covariate_data$clay, soil_predictions$predictions$clay)
  expect_identical(result$soil_predictions, soil_predictions)
})

test_that("fetch_covariates runs prediction workflow when cache missing", {
  cache_dir <- withr::local_tempdir()

  input <- tibble(
    Sample_ID = paste0("S", 1:3),
    `600`      = c(0.1, 0.2, 0.3),
    `700`      = c(0.4, 0.5, 0.6)
  )

  predict_calls <- 0
  saved_cache   <- NULL

  predict_stub <- function(input_data, covariates, ...) {
    predict_calls <<- predict_calls + 1

    predictions <- tibble(
      Sample_ID = input_data$Sample_ID,
      clay      = rep(1.5, nrow(input_data)),
      ph        = rep(6.2, nrow(input_data))
    )

    validation_metrics <- tibble(
      covariate = rep(c("clay", "ph"), each = 1),
      rsq       = c(0.9, 0.85),
      rmse      = c(0.1, 0.2),
      rpd       = c(2.5, 2.0)
    )

    list(
      predictions        = predictions,
      validation_metrics = validation_metrics,
      cluster_info       = list(n_clusters = 1,
                                cluster_assignments = rep(1, nrow(input_data)),
                                cluster_sizes = 1),
      local_models       = list(
        clay = list(Cluster_1 = list(fitted_workflow = structure(list(covariate = "clay"), class = "mock_workflow"))),
        ph   = list(Cluster_1 = list(fitted_workflow = structure(list(covariate = "ph"), class = "mock_workflow")))
      )
    )
  }

  result <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        fetch_covariates(
          input_data      = input,
          soil_covariates = c("clay", "ph"),
          n_similar       = 50,
          variance_threshold = 0.95,
          bayesian_iter   = 0,
          prop_train      = 0.8,
          cache_dir       = cache_dir,
          refresh_soil    = FALSE,
          verbose         = TRUE,
          return_models   = TRUE
        ),
        cli_text = function(...) invisible(NULL),
        cli_warn = function(...) invisible(NULL),
        cli_abort = function(message, ...) stop(message, call. = FALSE),
        .package = "cli"
      ),
      predict_soil_covariates = predict_stub,
      .package = "horizons"
    ),
    qread = function(...) stop("qread should not run when cache missing"),
    qsave = function(object, file, ...) {
      saved_cache <<- list(object = object, file = file)
      invisible(NULL)
    },
    .package = "qs"
  )

  expect_equal(predict_calls, 1)
  expect_s3_class(result$covariate_data, "tbl_df")
  expect_true(all(c("clay", "ph") %in% names(result$covariate_data)))
  expect_true(all(result$covariate_data$clay == 1.5))
  expect_true(all(result$covariate_data$ph == 6.2))
  expect_s3_class(result$soil_predictions$predictions, "tbl_df")
  expect_true(!is.null(saved_cache))
  expect_true(grepl("soil_predictions_", basename(saved_cache$file)))
  expect_true("predictions" %in% names(saved_cache$object))
})

test_that("fetch_covariates errors when climate covariates requested without coordinates", {
  input <- tibble(
    Sample_ID = c("S1", "S2"),
    `600`      = c(0.1, 0.2),
    `700`      = c(0.3, 0.4)
  )

  expect_error(
    fetch_covariates(
      input_data          = input,
      climate_covariates  = c("MAT"),
      refresh_climate     = TRUE,
      cache_dir           = withr::local_tempdir(),
      verbose             = FALSE
    ),
    "Longitude and Latitude columns"
  )
})

test_that("fetch_covariates merges climate data when requested", {
  cache_dir <- withr::local_tempdir()

  input <- tibble(
    Sample_ID = c("S1", "S2"),
    `600`      = c(0.1, 0.2),
    `700`      = c(0.3, 0.4),
    Longitude  = c(-100, -101),
    Latitude   = c(40, 41)
  )

  climate_stub <- tibble(
    Sample_ID = c("S1", "S2"),
    MAT       = c(12.5, 13.0)
  )

  result <- with_mocked_bindings(
    with_mocked_bindings(
      fetch_covariates(
        input_data          = input,
        soil_covariates     = NULL,
        climate_covariates  = c("MAT"),
        refresh_climate     = TRUE,
        cache_dir           = cache_dir,
        verbose             = FALSE
      ),
      cli_text = function(...) invisible(NULL),
      cli_warn = function(...) invisible(NULL),
      cli_abort = function(message, ...) stop(message, call. = FALSE),
      .package = "cli"
    ),
    fetch_climate_covariates = function(...) climate_stub,
    predict_soil_covariates  = function(...) list(predictions = tibble(Sample_ID = input$Sample_ID),
                                                  validation_metrics = tibble(),
                                                  cluster_info = list(n_clusters = 0),
                                                  local_models = NULL),
    .package = "horizons"
  )

  expect_s3_class(result$covariate_data, "tbl_df")
  expect_true("MAT" %in% names(result$covariate_data))
  expect_equal(result$covariate_data$MAT, climate_stub$MAT)
  expect_true(is.null(result$soil_predictions$local_models))
})

test_that("fetch_covariates handles configuration-driven mixed requests with climate failure", {
  cache_dir <- withr::local_tempdir()

  input <- tibble(
    Sample_ID = c("S1", "S2"),
    `600`      = c(0.12, 0.21),
    `620`      = c(0.35, 0.41),
    Longitude  = c(-95, -96),
    Latitude   = c(39, 40)
  )

  configurations <- tibble(
    config_id   = "cfg_001",
    covariates  = list(c("clay", "MAT"))
  )

  cache_called <- FALSE

  soil_stub <- function(...) {
    list(
      predictions = tibble(
        Sample_ID = input$Sample_ID,
        clay      = c(NA_real_, NA_real_)
      ),
      validation_metrics = tibble(),
      cluster_info       = list(n_clusters = 1),
      local_models       = NULL
    )
  }

  result <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        fetch_covariates(
          input_data      = input,
          configurations  = configurations,
          cache_dir       = cache_dir,
          refresh_soil    = TRUE,
          refresh_climate = TRUE,
          n_similar       = 25,
          variance_threshold = 0.9,
          bayesian_iter   = 0,
          prop_train      = 0.75,
          verbose         = FALSE
        ),
        cli_text = function(...) invisible(NULL),
        cli_alert_warning = function(...) invisible(NULL),
        cli_alert_info = function(...) invisible(NULL),
        cli_abort = function(message, ...) stop(message, call. = FALSE),
        .package = "cli"
      ),
      predict_soil_covariates = soil_stub,
      fetch_climate_covariates = function(...) NULL,
      .package = "horizons"
    ),
    qsave = function(...) {
      cache_called <<- TRUE
      invisible(NULL)
    },
    .package = "qs"
  )

  expect_s3_class(result$covariate_data, "tbl_df")
  expect_equal(names(result$covariate_data), c("Sample_ID", "clay"))
  expect_true(all(is.na(result$covariate_data$clay)))
  expect_equal(result$metadata$soil_covariates, "clay")
  expect_equal(result$metadata$climate_covariates, "MAT")
  expect_false(cache_called)
})
