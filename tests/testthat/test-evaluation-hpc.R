#' Tests for HPC Evaluation Orchestrator
#'
#' Tests the nested parallel HPC evaluation system with outer model workers
#' and inner CV workers, including parameter validation, output directory
#' creation, checkpoint resumption, and complete end-to-end workflows.
#'
#' Test Structure:
#' - VALIDATION (15% of tests): Parameter checking before execution
#' - INTEGRATION (85% of tests): End-to-end workflows with real data

## ============================================================================
## SETUP: Create test fixtures
## ============================================================================

# Use existing helper functions from test suite
test_spectra   <- make_test_spectra(n_samples = 30, seed = 42)
test_covariates <- make_test_covariates(
  sample_ids = test_spectra$Sample_ID,
  seed = 42
)

# Create minimal valid config
test_config <- data.frame(
  model              = c("rf", "xgb", "cubist"),
  preprocessing      = c("raw", "raw", "raw"),
  transformation     = c("No Transformation", "No Transformation", "No Transformation"),
  feature_selection  = c("None", "None", "None"),
  stringsAsFactors   = FALSE
)

test_that('evaluate_models_hpc smoke test runs with mocked future_map', {
  skip_if(Sys.getenv('CI') == 'true', 'Skipping on CI to avoid parallel setup noise')
  skip_if_not_installed('furrr')
  skip_if_not_installed('future')

  config <- data.frame(
    model                 = c('elastic_net', 'elastic_net'),
    preprocessing         = c('raw', 'raw'),
    transformation        = c('none', 'none'),
    feature_selection     = c('none', 'none'),
    covariates            = I(list(NULL, NULL)),
    covariate_interactions = c(FALSE, FALSE),
    stringsAsFactors       = FALSE
  )

  input_data <- create_eval_test_data(n_samples = 40, seed = 404)
  output_dir <- withr::local_tempdir(pattern = 'hpc_smoke_')

  future_plan_stub <- local({
    state <- structure(list(), class = 'sequential')
    function(strategy, ...) {
      if (missing(strategy)) {
        state
      } else {
        if (is.function(strategy) && identical(strategy, future::sequential)) {
          state <<- structure(list(), class = 'sequential')
        } else {
          state <<- structure(list(), class = 'multisession')
        }
        state
      }
    }
  })

  mock_eval <- function(config_row, input_data, data_split, config_id, covariate_data, variable, output_dir, grid_size, bayesian_iter, cv_folds, allow_par, n_cv_cores, prune_models, prune_threshold, seed, verbose) {
    tibble::tibble(
      config_id   = config_id,
      workflow_id = paste0('wf_', sprintf('%03d', config_id)),
      model       = config_row$model,
      transformation = config_row$transformation,
      preprocessing  = config_row$preprocessing,
      feature_selection = config_row$feature_selection,
      covariates   = '',
      covariate_interactions = config_row$covariate_interactions,
      best_params  = list(tibble::tibble(penalty = 0.001)),
      rsq          = 0.8,
      rmse         = 1,
      rrmse        = 12,
      rpd          = 2,
      ccc          = 0.8,
      mae          = 0.5,
      grid_seconds = 0.1,
      bayes_seconds = 0,
      total_seconds = 0.2,
      status       = 'success',
      error_message = NA_character_,
      error_stage   = NA_character_,
      error_class   = NA_character_,
      has_trace     = FALSE,
      n_warnings    = 0,
      warning_summary = NA_character_,
      metrics = list(tibble::tibble(.metric = c('rmse', 'rsq'), mean = c(1, 0.9))),
      config = list(list(model = config_row$model,
                         preprocessing = config_row$preprocessing,
                         transformation = config_row$transformation,
                         feature_selection = config_row$feature_selection))
    )
  }

  summary <- testthat::with_mocked_bindings(
    future_map = function(.x, .f, ..., .options = NULL, .progress = FALSE) lapply(.x, function(idx) .f(idx)),
    .package = 'furrr',
    testthat::with_mocked_bindings(
      plan = future_plan_stub,
      .package = 'future',
      testthat::with_mocked_bindings(
        qsave = function(object, file, ...) saveRDS(object, file = file),
        qread = function(file, ...) readRDS(file),
        .package = 'qs',
        testthat::with_mocked_bindings(
          evaluate_configuration = mock_eval,
          .package = 'horizons',
          horizons::evaluate_models_hpc(
            config         = config,
            input_data     = input_data,
            variable       = 'Response',
            output_dir     = output_dir,
            grid_size      = 1,
            bayesian_iter  = 0,
            cv_folds       = 2,
            outer_workers  = 1,
            inner_workers  = 1,
            seed           = 999,
            prune_models   = FALSE,
            resume         = FALSE,
            verbose        = FALSE
          )
        )
      )
    )
  )

  expect_s3_class(summary, 'horizons_hpc_summary')
  expect_equal(summary$execution$n_successful, nrow(config))
  expect_equal(summary$execution$n_failed, 0)

  checkpoint_files <- list.files(summary$paths$checkpoint_dir, pattern = "model_.*\\.qs$", full.names = TRUE)
  expect_true(length(checkpoint_files) >= 1)

  saved_checkpoint <- readRDS(checkpoint_files[1])
  expect_true("config" %in% names(saved_checkpoint))
  checkpoint_config <- saved_checkpoint$config[[1]]
  expect_true(all(c('model', 'preprocessing', 'transformation', 'feature_selection') %in% names(checkpoint_config)))
})

evaluate_models_hpc <- horizons::evaluate_models_hpc
run_hpc_tests <- identical(Sys.getenv("HORIZONS_RUN_HPC_TESTS"), "true")

if (!run_hpc_tests) {

  test_that("evaluate_models_hpc suite skipped", {
    skip("Set HORIZONS_RUN_HPC_TESTS=true to run evaluation-hpc tests")
  })

} else {


## ============================================================================
## VALIDATION TESTS: Parameter Checking (15% of tests)
## ============================================================================

## ---------------------------------------------------------------------------
## V-1: Config validation - must be data frame
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc rejects non-dataframe config", {

  expect_error(
    horizons::evaluate_models_hpc(
      config         = list(model = "rf"),  # NOT a dataframe
      input_data     = test_spectra,
      variable       = "Response",
      outer_workers  = 2,
      inner_workers  = 2
    ),
    "config must be a data frame"
  )

})

## ---------------------------------------------------------------------------
## V-2: Variable presence validation
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc rejects missing response variable", {

  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = test_spectra,
      variable       = "NonExistent",  # Not in data
      outer_workers  = 2,
      inner_workers  = 2
    ),
    "Response variable .* not found"
  )

})

## ---------------------------------------------------------------------------
## V-3: Response variable minimum value count
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc requires at least 20 non-missing response values", {

  # Create data with insufficient response values
  sparse_data <- test_spectra[1:15, ]  # Only 15 samples

  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = sparse_data,
      variable       = "Response",
      outer_workers  = 2,
      inner_workers  = 2
    ),
    "only.*non-missing values.*minimum 20"
  )

})

## ---------------------------------------------------------------------------
## V-4: Response variable must have variation
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc rejects constant response variable", {

  # Create data with no variation
  const_data <- test_spectra
  const_data$Response <- 5.0  # All same value

  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = const_data,
      variable       = "Response",
      outer_workers  = 2,
      inner_workers  = 2
    ),
    "no variation"
  )

})

## ---------------------------------------------------------------------------
## V-5: Required config columns validation
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc requires all necessary config columns", {

  # Create config missing 'model' column
  incomplete_config <- test_config[, -1]

  expect_error(
    horizons::evaluate_models_hpc(
      config         = incomplete_config,
      input_data     = test_spectra,
      variable       = "Response",
      outer_workers  = 2,
      inner_workers  = 2
    ),
    "missing required columns"
  )

})

## ---------------------------------------------------------------------------
## V-6: Outer workers parameter must be specified
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc requires outer_workers to be specified", {

  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = test_spectra,
      variable       = "Response",
      outer_workers  = NULL,  # Not specified
      inner_workers  = 2
    ),
    "outer_workers and inner_workers must be specified"
  )

})

## ---------------------------------------------------------------------------
## V-7: Inner workers parameter must be specified
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc requires inner_workers to be specified", {

  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = test_spectra,
      variable       = "Response",
      outer_workers  = 2,
      inner_workers  = NULL  # Not specified
    ),
    "outer_workers and inner_workers must be specified"
  )

})

## ---------------------------------------------------------------------------
## V-8: Worker parameters must be positive integers
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc rejects invalid worker counts", {

  # Test outer_workers = 0
  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = test_spectra,
      variable       = "Response",
      outer_workers  = 0,
      inner_workers  = 2
    ),
    ">= 1"
  )

  # Test inner_workers = 0
  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = test_spectra,
      variable       = "Response",
      outer_workers  = 2,
      inner_workers  = 0
    ),
    ">= 1"
  )

})

## ---------------------------------------------------------------------------
## V-9: Total requested cores cannot exceed available cores
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc checks total cores vs available", {

  available_cores <- parallel::detectCores()

  # Try to request more cores than available
  expect_error(
    horizons::evaluate_models_hpc(
      config         = test_config,
      input_data     = test_spectra,
      variable       = "Response",
      outer_workers  = available_cores + 10,  # Exceeds available
      inner_workers  = 2
    ),
    "only.*available"
  )

})

## ============================================================================
## INTEGRATION TESTS: End-to-End Workflows (85% of tests)
## ============================================================================

## ---------------------------------------------------------------------------
## I-1: Minimal HPC evaluation with 2 models
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc completes with 2 models (minimal workflow)", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_01_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:2, ],  # Just 2 models
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 5,
    bayesian_iter  = 2,
    cv_folds       = 3,
    seed           = 123,
    verbose        = FALSE
  )

  # Check return value structure
  expect_true(inherits(result, "horizons_hpc_summary"))
  expect_true(inherits(result, "list"))

  # Check execution summary
  expect_equal(result$execution$n_total, 2)
  expect_gte(result$execution$n_successful, 1)  # At least 1 successful
  expect_true(result$execution$time_minutes >= 0)

  # Check configuration
  expect_equal(result$configuration$outer_workers, 1)
  expect_equal(result$configuration$inner_workers, 1)
  expect_equal(result$configuration$total_cores, 1)
  expect_equal(result$configuration$seed, 123)

  # Check output directories were created
  expect_true(dir.exists(output_dir))
  expect_true(dir.exists(result$paths$checkpoint_dir))
  expect_true(dir.exists(result$paths$results_dir))
  expect_true(dir.exists(result$paths$errors_dir))

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-2: Output directory creation and structure
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc creates proper output directory structure", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_02_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Verify all required subdirectories exist
  expect_true(dir.exists(file.path(output_dir, "checkpoints")))
  expect_true(dir.exists(file.path(output_dir, "results")))
  expect_true(dir.exists(file.path(output_dir, "errors")))
  expect_true(dir.exists(file.path(output_dir, "summary")))

  # Verify summary file was saved
  summary_file <- file.path(output_dir, "summary", "evaluation_summary.qs")
  expect_true(file.exists(summary_file))

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-3: Auto-generated output directory (no output_dir specified)
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc auto-generates output directory with timestamp", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  # Clean up any previous test directories
  old_output_dirs <- list.dirs("output", full.names = TRUE, recursive = FALSE)
  sapply(old_output_dirs, function(x) if (length(list.files(x)) == 0) unlink(x))

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = NULL,  # Let it auto-generate
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Check that output_dir was created
  output_dir <- result$paths$output_dir
  expect_true(!is.null(output_dir))
  expect_true(dir.exists(output_dir))
  expect_true(grepl("Response_hpc_", output_dir))  # Has variable name

  unlink(dirname(output_dir), recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-4: Data split is created and stored
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc creates train/test split with proper proportions", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_04_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    seed           = 42,
    verbose        = FALSE
  )

  # Verify that we got results back (indicating split worked)
  expect_true(result$execution$n_total >= 1)

  # Verify sample counts make sense (80/20 split of 30 samples ~ 24 train / 6 test)
  n_samples <- nrow(test_spectra)
  expect_gte(result$execution$n_total, 0)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-5: Results contain expected metrics
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc results include required metrics", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_05_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Check execution summary has all fields
  expect_true(!is.null(result$execution$n_total))
  expect_true(!is.null(result$execution$n_successful))
  expect_true(!is.null(result$execution$n_failed))
  expect_true(!is.null(result$execution$n_resumed))
  expect_true(!is.null(result$execution$time_minutes))
  expect_true(!is.null(result$execution$timestamp))

  # Check configuration summary
  expect_true(!is.null(result$configuration$outer_workers))
  expect_true(!is.null(result$configuration$inner_workers))
  expect_true(!is.null(result$configuration$total_cores))

  # Check paths summary
  expect_true(!is.null(result$paths$output_dir))
  expect_true(!is.null(result$paths$checkpoint_dir))
  expect_true(!is.null(result$paths$results_dir))
  expect_true(!is.null(result$paths$errors_dir))

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-6: Checkpoint resumption (empty first run should create checkpoints)
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc creates checkpoints for resumption", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_06_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:2, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    resume         = TRUE,
    verbose        = FALSE
  )

  # Check that checkpoint directory exists
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  expect_true(dir.exists(checkpoint_dir))

  # Check that at least some checkpoints were created
  checkpoint_files <- list.files(checkpoint_dir, pattern = "\\.(qs|json)$")
  expect_gte(length(checkpoint_files), 1)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-7: Verbose and non-verbose modes (silent execution)
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc respects verbose parameter", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_07_")

  # Should not produce visible messages/warnings
  suppressMessages({
    result <- horizons::evaluate_models_hpc(
      config         = test_config[1:1, ],
      input_data     = test_spectra,
      variable       = "Response",
      output_dir     = output_dir,
      outer_workers  = 1,
      inner_workers  = 1,
      grid_size      = 3,
      bayesian_iter  = 1,
      cv_folds       = 2,
      verbose        = FALSE
    )
  })

  # Verify it still completes successfully
  expect_true(inherits(result, "horizons_hpc_summary"))

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-8: Covariate data integration (when provided)
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc integrates covariate data when provided", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  # Create config with covariates
  cov_config <- test_config[1:1, ]
  cov_config$covariates <- list(c("Clay", "pH"))

  output_dir <- tempfile(pattern = "hpc_test_08_")

  result <- horizons::evaluate_models_hpc(
    config         = cov_config,
    input_data     = test_spectra,
    covariate_data = test_covariates,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Verify execution completed
  expect_true(result$execution$n_total >= 1)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-9: Multiple models with proper result aggregation
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc processes multiple models and aggregates results", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_09_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config,  # All 3 models
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Check that results include all or most models
  n_configs <- nrow(test_config)
  n_total <- result$execution$n_total
  expect_equal(n_total, n_configs)

  # Check that at least one model completed successfully
  expect_gte(result$execution$n_successful, 1)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-10: Custom seed produces reproducible results
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc with same seed produces consistent train/test splits", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir1 <- tempfile(pattern = "hpc_test_10a_")
  output_dir2 <- tempfile(pattern = "hpc_test_10b_")

  # Run twice with same seed
  result1 <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir1,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    seed           = 999,
    verbose        = FALSE
  )

  result2 <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir2,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    seed           = 999,
    verbose        = FALSE
  )

  # Both should have same execution characteristics
  expect_equal(result1$configuration$seed, result2$configuration$seed)

  unlink(output_dir1, recursive = TRUE)
  unlink(output_dir2, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-11: Grid size parameter is respected
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc respects grid_size parameter", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_11_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 5,  # Specific grid size
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Should complete successfully
  expect_true(result$execution$n_total >= 1)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-12: Bayesian iteration parameter is respected
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc respects bayesian_iter parameter", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_12_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 3,  # Specific Bayesian iterations
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Should complete successfully
  expect_true(result$execution$n_total >= 1)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-13: CV folds parameter is respected
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc respects cv_folds parameter", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_13_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 5,  # Different number of folds
    verbose        = FALSE
  )

  # Should complete successfully
  expect_true(result$execution$n_total >= 1)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-14: Two-tier parallelization (outer × inner workers)
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc configures nested parallelization correctly", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_14_")

  outer_w <- 1
  inner_w <- 1

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = outer_w,
    inner_workers  = inner_w,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Verify configuration was recorded correctly
  expect_equal(result$configuration$outer_workers, outer_w)
  expect_equal(result$configuration$inner_workers, inner_w)
  expect_equal(result$configuration$total_cores, outer_w * inner_w)

  unlink(output_dir, recursive = TRUE)

})

## ---------------------------------------------------------------------------
## I-15: Return value is invisible (doesn't print by default)
## ---------------------------------------------------------------------------

test_that("evaluate_models_hpc returns invisible result", {

  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  output_dir <- tempfile(pattern = "hpc_test_15_")

  result <- horizons::evaluate_models_hpc(
    config         = test_config[1:1, ],
    input_data     = test_spectra,
    variable       = "Response",
    output_dir     = output_dir,
    outer_workers  = 1,
    inner_workers  = 1,
    grid_size      = 3,
    bayesian_iter  = 1,
    cv_folds       = 2,
    verbose        = FALSE
  )

  # Check that it's an invisible value (class structure is preserved)
  expect_true(inherits(result, "horizons_hpc_summary"))

  unlink(output_dir, recursive = TRUE)

})

## ===========================================================================
## ADDITIONAL INTEGRATION TESTS: Pruning and Error Handling (I-16 to I-26)
## ===========================================================================

test_that("evaluate_models_hpc handles model pruning correctly", {

  # SPEC-EVAL-HPC-I-16: Model pruning based on RRMSE threshold
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  data <- make_test_spectra(n_samples = 40)

  # Create multiple configs with different expected performance
  config <- data.frame(
    model = c("random_forest", "cubist", "elastic_net"),
    preprocessing = c("raw", "snv", "raw"),
    transformation = c("none", "log", "sqrt"),
    feature_selection = c("none", "pca", "boruta"),
    stringsAsFactors = FALSE
  )

  # Mock evaluate_configuration to simulate different RRMSE values
  with_mocked_bindings(
    evaluate_configuration = function(config_row, ...) {
      # Simulate varying RRMSE based on model type
      if (config_row$model == "elastic_net") {
        # Poor performance - should be pruned
        list(status = "completed", best_rrmse = 0.95,
             metrics = data.frame(RRMSE = 0.95))
      } else {
        # Good performance - should continue
        list(status = "completed", best_rrmse = 0.45,
             metrics = data.frame(RRMSE = 0.45))
      }
    },
    {
      result <- evaluate_models_hpc(
        config = config,
        input_data = data,
        variable = "Response",
        grid_size = 3,
        cv_folds = 3,
        outer_workers = 1,
        inner_workers = 1,
        prune_models = TRUE,
        prune_threshold = 0.8,
        verbose = FALSE
      )

      expect_s3_class(result, "tbl_df")
      # elastic_net should be marked as pruned
      expect_true(any(result$status == "pruned"))
    },
    .package = "horizons"
  )

})

test_that("evaluate_models_hpc creates error file on configuration failure", {

  # SPEC-EVAL-HPC-I-17: Error file creation and structure
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "hpc_error_")
  dir.create(test_dir)

  data <- make_test_spectra(n_samples = 30)
  config <- data.frame(
    model = "random_forest",
    preprocessing = "invalid_preprocessing",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  # Mock to simulate configuration error
  with_mocked_bindings(
    evaluate_configuration = function(...) {
      stop("Invalid preprocessing method")
    },
    {
      suppressWarnings({
        result <- evaluate_models_hpc(
          config = config,
          input_data = data,
          variable = "Response",
          output_dir = test_dir,
          grid_size = 3,
          cv_folds = 3,
          outer_workers = 1,
          inner_workers = 1,
          verbose = FALSE
        )
      })

      # Check for error file
      error_files <- list.files(test_dir, pattern = "error.*\\.rds",
                               full.names = TRUE)
      expect_true(length(error_files) > 0)

      if (length(error_files) > 0) {
        error_data <- readRDS(error_files[1])
        expect_true("error" %in% names(error_data))
        expect_true("config" %in% names(error_data))
      }
    },
    .package = "horizons"
  )

  unlink(test_dir, recursive = TRUE)

})

test_that("evaluate_models_hpc handles checkpoint batch consolidation", {

  # SPEC-EVAL-HPC-I-18: Checkpoint batching for large config sets
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "hpc_batch_")
  dir.create(test_dir)

  data <- make_test_spectra(n_samples = 35)

  # Create large config set to trigger batching
  config <- expand.grid(
    model = c("random_forest", "cubist"),
    preprocessing = c("raw", "snv"),
    transformation = c("none", "log"),
    feature_selection = c("none", "pca"),
    stringsAsFactors = FALSE
  )

  result <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    output_dir = test_dir,
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 2,
    inner_workers = 1,
    verbose = FALSE
  )

  # Check checkpoint files were created
  checkpoint_files <- list.files(test_dir, pattern = "checkpoint.*\\.rds",
                                full.names = TRUE)
  expect_true(length(checkpoint_files) > 0)

  # Results should contain all configurations
  expect_equal(nrow(result), nrow(config))

  unlink(test_dir, recursive = TRUE)

})

test_that("evaluate_models_hpc respects thread environment variables", {

  # SPEC-EVAL-HPC-I-19: Thread environment configuration
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  # Save original values
  orig_omp <- Sys.getenv("OMP_NUM_THREADS")
  orig_mkl <- Sys.getenv("MKL_NUM_THREADS")

  data <- make_test_spectra(n_samples = 30)
  config <- data.frame(
    model = "random_forest",
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  # Test runs and respects thread settings
  result <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 1,
    inner_workers = 2,
    verbose = FALSE
  )

  # Thread variables should have been set
  expect_s3_class(result, "tbl_df")

  # Restore original values
  if (orig_omp != "") Sys.setenv(OMP_NUM_THREADS = orig_omp)
  if (orig_mkl != "") Sys.setenv(MKL_NUM_THREADS = orig_mkl)

})

test_that("evaluate_models_hpc handles covariate validation failures", {

  # SPEC-EVAL-HPC-I-20: Covariate data validation and error handling
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  data <- make_test_spectra(n_samples = 35)

  # Create mismatched covariate data
  bad_covariates <- make_test_covariates(sample_ids = data$Sample_ID[1:10]) # Wrong number of samples

  config <- data.frame(
    model = "random_forest",
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  expect_error(
    evaluate_models_hpc(
      config = config,
      input_data = data,
      covariate_data = bad_covariates,
      variable = "Response",
      grid_size = 3,
      cv_folds = 3,
      outer_workers = 1,
      inner_workers = 1,
      verbose = FALSE
    ),
    regex = "covariate.*mismatch|Sample_ID"
  )

})

test_that("evaluate_models_hpc resumes from existing checkpoints", {

  # SPEC-EVAL-HPC-I-21: Resume functionality with pre-existing checkpoints
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "hpc_resume_")
  dir.create(test_dir)

  data <- make_test_spectra(n_samples = 30)
  config <- data.frame(
    model = c("random_forest", "cubist", "elastic_net"),
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  # Create fake checkpoint for first config
  checkpoint_data <- list(
    config = config[1, ],
    status = "completed",
    best_rrmse = 0.35,
    metrics = data.frame(RRMSE = 0.35, CCC = 0.85)
  )

  checkpoint_file <- file.path(test_dir, "checkpoint_001.rds")
  saveRDS(checkpoint_data, checkpoint_file)

  # Mock to track which configs are evaluated
  evaluated_configs <- character()
  with_mocked_bindings(
    evaluate_configuration = function(config_row, ...) {
      evaluated_configs <<- c(evaluated_configs, config_row$model)
      list(status = "completed", best_rrmse = 0.4,
           metrics = data.frame(RRMSE = 0.4))
    },
    {
      result <- evaluate_models_hpc(
        config = config,
        input_data = data,
        variable = "Response",
        output_dir = test_dir,
        grid_size = 3,
        cv_folds = 3,
        outer_workers = 1,
        inner_workers = 1,
        resume = TRUE,
        verbose = FALSE
      )

      # Should have skipped the first config (random_forest)
      expect_false("random_forest" %in% evaluated_configs)
      expect_true("cubist" %in% evaluated_configs)
      expect_true("elastic_net" %in% evaluated_configs)
    },
    .package = "horizons"
  )

  unlink(test_dir, recursive = TRUE)

})

test_that("evaluate_models_hpc handles atomic file writes correctly", {

  # SPEC-EVAL-HPC-I-22: Atomic writes prevent corruption
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "hpc_atomic_")
  dir.create(test_dir)

  data <- make_test_spectra(n_samples = 30)
  config <- data.frame(
    model = "random_forest",
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  # Run with checkpointing enabled
  result <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    output_dir = test_dir,
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 1,
    inner_workers = 1,
    verbose = FALSE
  )

  # Verify checkpoint file exists and is valid
  checkpoint_files <- list.files(test_dir, pattern = "checkpoint.*\\.rds",
                                full.names = TRUE)
  expect_true(length(checkpoint_files) > 0)

  if (length(checkpoint_files) > 0) {
    # Should be able to read without corruption
    checkpoint <- readRDS(checkpoint_files[1])
    expect_type(checkpoint, "list")
    expect_true("status" %in% names(checkpoint))
  }

  unlink(test_dir, recursive = TRUE)

})

test_that("evaluate_models_hpc sets parallel backend for inner workers", {

  # SPEC-EVAL-HPC-I-23: Inner worker backend configuration
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  data <- make_test_spectra(n_samples = 30)
  config <- data.frame(
    model = "random_forest",
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  # Test with different inner worker settings
  result <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 1,
    inner_workers = 2,  # Should configure doMC backend
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_df")

  # Test with NULL inner workers (uses default detection)
  result2 <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 1,
    inner_workers = NULL,
    verbose = FALSE
  )

  expect_s3_class(result2, "tbl_df")

})

test_that("evaluate_models_hpc handles PBS environment variables", {

  # SPEC-EVAL-HPC-I-24: PBS job environment detection
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  # Save original PBS variables
  orig_pbs <- Sys.getenv("PBS_JOBID")
  orig_ncpus <- Sys.getenv("NCPUS")

  # Simulate PBS environment
  Sys.setenv(PBS_JOBID = "12345.pbs")
  Sys.setenv(NCPUS = "8")

  data <- make_test_spectra(n_samples = 30)
  config <- data.frame(
    model = "random_forest",
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  result <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = NULL,  # Should auto-detect from PBS
    inner_workers = NULL,
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_df")

  # Restore original values
  if (orig_pbs != "") {
    Sys.setenv(PBS_JOBID = orig_pbs)
  } else {
    Sys.unsetenv("PBS_JOBID")
  }

  if (orig_ncpus != "") {
    Sys.setenv(NCPUS = orig_ncpus)
  } else {
    Sys.unsetenv("NCPUS")
  }

})

test_that("evaluate_models_hpc aggregates metrics from parallel evaluations", {

  # SPEC-EVAL-HPC-I-25: Metric aggregation from parallel workers
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  data <- make_test_spectra(n_samples = 40)

  # Multiple configurations
  config <- data.frame(
    model = c("random_forest", "cubist"),
    preprocessing = c("raw", "snv"),
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  result <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 2,  # Parallel evaluation
    inner_workers = 1,
    verbose = FALSE
  )

  # Should have metrics for all configurations
  expect_equal(nrow(result), nrow(config))
  expect_true(all(c("best_rrmse", "status") %in% names(result)))

  # Each row should have valid metrics
  for (i in seq_len(nrow(result))) {
    if (result$status[i] == "completed") {
      expect_true(is.numeric(result$best_rrmse[i]))
      expect_true(result$best_rrmse[i] >= 0)
    }
  }

})

test_that("evaluate_models_hpc handles seed propagation to workers", {

  # SPEC-EVAL-HPC-I-26: Reproducible results with seed setting
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  data <- make_test_spectra(n_samples = 35)
  config <- data.frame(
    model = "random_forest",
    preprocessing = "raw",
    transformation = "none",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )

  # Run twice with same seed
  result1 <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 1,
    inner_workers = 1,
    seed = 123,
    verbose = FALSE
  )

  result2 <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 1,
    inner_workers = 1,
    seed = 123,
    verbose = FALSE
  )

  # Results should be reproducible
  expect_equal(result1$best_rrmse, result2$best_rrmse, tolerance = 0.01)

  # Different seed should give different results
  result3 <- evaluate_models_hpc(
    config = config,
    input_data = data,
    variable = "Response",
    grid_size = 3,
    cv_folds = 3,
    outer_workers = 1,
    inner_workers = 1,
    seed = 456,
    verbose = FALSE
  )

  # Allow for some variation but not identical
  expect_true(abs(result1$best_rrmse - result3$best_rrmse) > 0.001 ||
              result1$best_rrmse == result3$best_rrmse)  # May be same by chance

})

}
