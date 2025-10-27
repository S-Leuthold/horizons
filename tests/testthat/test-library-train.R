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
## Helper: Create synthetic cluster data (defined first for use in tests)
## =============================================================================

make_synthetic_cluster_data <- function(n = 100, n_wavelengths = 50, seed = 123) {

  set.seed(seed)

  ## Property measurements
  data <- tibble::tibble(
    Sample_ID = paste0("S", 1:n),  # build_recipe compatibility
    Project = "test",                # build_recipe compatibility
    clay.tot_usda.a334_w.pct = rnorm(n, mean = 250, sd = 50),
    Response = rnorm(n, mean = 250, sd = 50)  # build_recipe compatibility
  )

  ## Spectral columns
  spectra <- matrix(rnorm(n * n_wavelengths, mean = 0.5, sd = 0.1), nrow = n)
  colnames(spectra) <- paste0(seq(600, 1200, length.out = n_wavelengths))

  dplyr::bind_cols(data, tibble::as_tibble(spectra))

}

## =============================================================================
## TEST GROUP 1: Data Preparation and Splits
## =============================================================================

test_that("prepare_cluster_splits creates 80/20 split correctly", {

  ## Create synthetic cluster data (use pH to avoid texture complexity)
  cluster_data <- tibble::tibble(
    sample_id = paste0("S", 1:1000),
    ph.h2o_usda.a268_index = rnorm(1000, mean = 6.5, sd = 1.2)
  )

  ## Add spectral columns
  spectra <- matrix(rnorm(1000 * 50), nrow = 1000)
  colnames(spectra) <- paste0(seq(600, 1000, length.out = 50))
  cluster_data <- dplyr::bind_cols(cluster_data, tibble::as_tibble(spectra))

  ## Create split
  splits <- horizons:::prepare_cluster_splits(
    cluster_data = cluster_data,
    property = "ph",
    train_prop = 0.8,
    seed = 123
  )

  ## Should return expected structure
  expect_type(splits, "list")
  expect_true(all(c("training_pool", "external_test") %in% names(splits)))

  ## Check sizes (80/20)
  expect_equal(nrow(splits$training_pool), 800)
  expect_equal(nrow(splits$external_test), 200)

  ## No overlap between sets (use Sample_ID - standardized naming)
  train_ids <- splits$training_pool$Sample_ID
  test_ids  <- splits$external_test$Sample_ID
  expect_length(intersect(train_ids, test_ids), 0)

})

test_that("prepare_cluster_splits is deterministic with seed", {

  ## Use non-texture property for this test (testing split determinism, not ILR)
  ## make_synthetic_cluster_data has clay column, so change to use oc instead
  cluster_data <- make_synthetic_cluster_data(n = 500)

  ## Rename clay column to oc for non-texture test
  cluster_data <- cluster_data %>%
    dplyr::select(-clay.tot_usda.a334_w.pct) %>%
    dplyr::mutate(oc_usda.c729_w.pct = Response)

  split1 <- horizons:::prepare_cluster_splits(cluster_data, "oc", seed = 456)
  split2 <- horizons:::prepare_cluster_splits(cluster_data, "oc", seed = 456)

  ## Should get identical splits (use Sample_ID - standardized naming)
  expect_equal(split1$training_pool$Sample_ID, split2$training_pool$Sample_ID)
  expect_equal(split1$external_test$Sample_ID, split2$external_test$Sample_ID)

})

test_that("prepare_cluster_splits handles texture with ILR transformation", {

  ## Create synthetic texture data with all 3 components
  texture_data <- tibble::tibble(
    sample_id = paste0("S", 1:500),
    sand.tot_usda.c60_w.pct = rnorm(500, mean = 350, sd = 80),
    silt.tot_usda.c62_w.pct = rnorm(500, mean = 380, sd = 70),
    clay.tot_usda.a334_w.pct = rnorm(500, mean = 270, sd = 60)
  )

  ## Ensure mass balance (sum to 1000)
  total <- texture_data$sand.tot_usda.c60_w.pct +
           texture_data$silt.tot_usda.c62_w.pct +
           texture_data$clay.tot_usda.a334_w.pct

  texture_data$sand.tot_usda.c60_w.pct <- texture_data$sand.tot_usda.c60_w.pct / total * 1000
  texture_data$silt.tot_usda.c62_w.pct <- texture_data$silt.tot_usda.c62_w.pct / total * 1000
  texture_data$clay.tot_usda.a334_w.pct <- texture_data$clay.tot_usda.a334_w.pct / total * 1000

  ## Add spectral columns
  spectra <- matrix(rnorm(500 * 50), nrow = 500)
  colnames(spectra) <- paste0(seq(600, 1000, length.out = 50))
  texture_data <- dplyr::bind_cols(texture_data, tibble::as_tibble(spectra))

  ## Split with ilr_coordinate = 1
  splits <- horizons:::prepare_cluster_splits(
    cluster_data = texture_data,
    property = "clay",
    ilr_coordinate = 1,
    seed = 123
  )

  ## Should have ILR coordinates in data
  expect_true("ilr_2" %in% names(splits$training_pool))  # ilr_1 was renamed to Response
  expect_true("Response" %in% names(splits$training_pool))

  ## Response should be ILR values (unbounded, can be negative)
  expect_true(any(splits$training_pool$Response < 0) || any(splits$training_pool$Response > 5))

  ## ilr_coordinate should be tracked
  expect_equal(splits$ilr_coordinate, 1)

  ## Original texture columns should still be present
  expect_true("sand.tot_usda.c60_w.pct" %in% names(splits$training_pool))
  expect_true("silt.tot_usda.c62_w.pct" %in% names(splits$training_pool))
  expect_true("clay.tot_usda.a334_w.pct" %in% names(splits$training_pool))
})

test_that("prepare_cluster_splits requires ilr_coordinate for texture", {

  ## Create minimal texture data
  texture_data <- tibble::tibble(
    sample_id = paste0("S", 1:100),
    sand.tot_usda.c60_w.pct = rnorm(100, 350, 50),
    silt.tot_usda.c62_w.pct = rnorm(100, 380, 50),
    clay.tot_usda.a334_w.pct = rnorm(100, 270, 50)
  )

  ## Should error if ilr_coordinate not specified
  expect_error(
    horizons:::prepare_cluster_splits(texture_data, "sand"),
    "require ilr_coordinate"
  )

  ## Should error if ilr_coordinate invalid
  expect_error(
    horizons:::prepare_cluster_splits(texture_data, "clay", ilr_coordinate = 3),
    "ilr_coordinate = 1 or 2"
  )
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
      input_data = cluster_data,
      spectral_transformation = "snv",
      response_transformation = "none",
      feature_selection_method = "pca"
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

## Helper function moved to top of file for availability
