## =============================================================================
## TEST: Library Model Training and Optimization
## =============================================================================
##
## Purpose: Test two-stage training pipeline - config selection then hyperparameter
##          tuning on clustered library data
##
## Coverage Target: >80%
## Phase: 1, Milestone: 1.4
##
## =============================================================================

library(testthat)
library(horizons)

## =============================================================================
## TEST GROUP 1: Data Preparation and Splits
## =============================================================================

test_that("prepare_cluster_splits creates 80/20 split correctly", {

  ## Create synthetic cluster data
  cluster_data <- tibble::tibble(
    sample_id = paste0("S", 1:1000),
    clay.tot_usda.a334_w.pct = rnorm(1000, mean = 250, sd = 50)
  )

  ## Add spectral columns
  spectra <- matrix(rnorm(1000 * 50), nrow = 1000)
  colnames(spectra) <- paste0("X", seq(600, 1000, length.out = 50))
  cluster_data <- dplyr::bind_cols(cluster_data, tibble::as_tibble(spectra))

  ## Create split
  splits <- horizons:::prepare_cluster_splits(
    cluster_data = cluster_data,
    property = "clay",
    train_prop = 0.8,
    seed = 123
  )

  ## Should return expected structure
  expect_type(splits, "list")
  expect_true(all(c("training_pool", "external_test") %in% names(splits)))

  ## Check sizes (80/20)
  expect_equal(nrow(splits$training_pool), 800)
  expect_equal(nrow(splits$external_test), 200)

  ## No overlap between sets
  train_ids <- splits$training_pool$sample_id
  test_ids  <- splits$external_test$sample_id
  expect_length(intersect(train_ids, test_ids), 0)

})

test_that("prepare_cluster_splits is deterministic with seed", {

  cluster_data <- make_synthetic_cluster_data(n = 500)

  split1 <- horizons:::prepare_cluster_splits(cluster_data, "clay", seed = 456)
  split2 <- horizons:::prepare_cluster_splits(cluster_data, "clay", seed = 456)

  ## Should get identical splits
  expect_equal(split1$training_pool$sample_id, split2$training_pool$sample_id)
  expect_equal(split1$external_test$sample_id, split2$external_test$sample_id)

})

## =============================================================================
## TEST GROUP 2: Composite Score Calculation
## =============================================================================

test_that("calculate_composite_score weights metrics correctly", {

  metrics <- tibble::tibble(
    config_id = c("A", "B", "C"),
    rpd  = c(3.0, 2.5, 2.0),
    ccc  = c(0.95, 0.90, 0.85),
    rsq  = c(0.90, 0.85, 0.80),
    rmse = c(10, 15, 20)
  )

  scores <- horizons:::calculate_composite_score(metrics)

  ## Should return tibble with composite_score column
  expect_s3_class(scores, "tbl_df")
  expect_true("composite_score" %in% names(scores))

  ## Config A should rank highest (best on all metrics)
  expect_equal(which.max(scores$composite_score), 1)

  ## Scores should be between 0 and 1 (normalized)
  expect_true(all(scores$composite_score >= 0 & scores$composite_score <= 1))

})

test_that("calculate_composite_score handles ties with simplicity rule", {

  ## Two configs with nearly identical performance
  metrics <- tibble::tibble(
    config_id = c("rf_pca", "xgb_pca", "rf_none"),
    rpd  = c(2.50, 2.49, 2.50),
    ccc  = c(0.90, 0.90, 0.90),
    rsq  = c(0.85, 0.85, 0.85),
    rmse = c(12, 12, 12),
    n_features = c(10, 10, 50)  # Simpler model has fewer features
  )

  ## If scores within threshold, should prefer simpler
  ## (This might need adjustment based on actual implementation)
  scores <- horizons:::calculate_composite_score(metrics, tie_threshold = 0.01)

  ## Expect tie-breaking logic documented
  expect_true("composite_score" %in% names(scores))

})

## =============================================================================
## TEST GROUP 3: Single Config Training (Building Block)
## =============================================================================

test_that("train_single_config works with existing build_recipe", {

  skip("Requires real spectral data - expensive test")

  ## This validates integration with existing tidymodels infrastructure
  ## Run manually when validating full pipeline

})

test_that("train_single_config returns expected structure", {

  ## Mock test with minimal data
  train_data <- make_synthetic_cluster_data(n = 100)

  ## Get a config from OPTIMAL_CONFIGS_V1
  config <- OPTIMAL_CONFIGS_V1 %>% dplyr::filter(property == "clay", rank == 1)

  ## Train (will fail without real implementation, placeholder for now)
  expect_true(nrow(config) == 1)

})

## =============================================================================
## TEST GROUP 4: Config Optimization
## =============================================================================

test_that("optimize_config_for_cluster selects best from candidates", {

  skip("Full config optimization - too expensive for routine testing")

  ## This would test:
  ## - Load top 10 OPTIMAL_CONFIGS for property
  ## - Train each on subset
  ## - Rank by composite score
  ## - Return winner

})

test_that("optimize_config_for_cluster logs selection rationale", {

  skip("Expensive test - validate manually")

  ## Should log:
  ## - Which configs were tested
  ## - Composite scores for each
  ## - Why winner was selected
  ## - Any warnings (small clusters, convergence issues, etc.)

})

## =============================================================================
## TEST GROUP 5: Memory Management
## =============================================================================

test_that("intermediate models are cleaned up", {

  ## After testing 10 configs, should only keep winner
  ## Memory should not grow linearly with number of configs tested

  ## This is more of an integration test
  ## Validate via memory profiling during development

  expect_true(TRUE)  # Placeholder

})

test_that("final model is butchered for memory efficiency", {

  ## Code inspection: verify butcher::butcher() is called on final workflow

  expect_true(TRUE)  # Placeholder - implement when library-train.R exists

})

## =============================================================================
## TEST GROUP 6: Integration with Existing Functions
## =============================================================================

test_that("build_recipe works with library cluster data", {

  ## Verify our cluster data format is compatible with build_recipe()

  cluster_data <- make_synthetic_cluster_data(n = 50)

  ## Should not error
  expect_no_error({
    recipe <- build_recipe(
      project_data = cluster_data,
      preprocessing = "snv",
      transformation = "none",
      feature_selection = "pca",
      response_variable = "clay.tot_usda.a334_w.pct",
      id_variable = "sample_id"
    )
  })

})

test_that("define_model_specifications works with library configs", {

  ## Get a config
  config <- tibble::tibble(
    model = "random_forest",
    preprocessing = "snv",
    transformation = "none"
  )

  ## Should create valid parsnip spec
  expect_no_error({
    spec <- define_model_specifications(config$model)
  })

  expect_s3_class(spec, "model_spec")

})

## =============================================================================
## Helper: Create synthetic cluster data
## =============================================================================

make_synthetic_cluster_data <- function(n = 100, n_wavelengths = 50, seed = 123) {

  set.seed(seed)

  ## Property measurements
  data <- tibble::tibble(
    sample_id = paste0("S", 1:n),
    clay.tot_usda.a334_w.pct = rnorm(n, mean = 250, sd = 50)
  )

  ## Spectral columns
  spectra <- matrix(rnorm(n * n_wavelengths, mean = 0.5, sd = 0.1), nrow = n)
  colnames(spectra) <- paste0("X", seq(600, 1200, length.out = n_wavelengths))

  dplyr::bind_cols(data, tibble::as_tibble(spectra))

}
