## =============================================================================
## Test Suite: Library Prediction with Uncertainty Quantification
## =============================================================================
## This file contains TDD test skeletons for Phase 3 (M3.1-M3.3)
## Tests are written FIRST, then we implement to make them pass
## =============================================================================

## -----------------------------------------------------------------------------
## Test Group 1: Quantile Model Specifications
## -----------------------------------------------------------------------------

test_that("define_quantile_specification() creates valid ranger spec", {

  spec <- define_quantile_specification()

  # Should return ranger spec
  expect_s3_class(spec, "rand_forest")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "ranger")

  # Should have quantreg enabled
  expect_true(spec$eng_args$quantreg)
  expect_true(spec$eng_args$keep.inbag)

  # Should have tunable parameters
  expect_true("tune" %in% class(spec$args$mtry))
  expect_true("tune" %in% class(spec$args$min_n))
})

test_that("quantile spec can train and predict quantiles", {

  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  ## Create test data ---------------------------------------------------------

  set.seed(123)
  n <- 200
  test_data <- tibble::tibble(
    y  = rnorm(n, mean = 10, sd = 2),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  ## Build and fit workflow ---------------------------------------------------

  spec <- define_quantile_specification()

  # Finalize hyperparameters (avoid tuning in test)
  spec$args$mtry  <- 2
  spec$args$min_n <- 10

  recipe <- recipes::recipe(y ~ ., data = test_data)

  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_recipe(recipe) %>%
    parsnip::fit(data = test_data)

  expect_true(workflows::is_trained_workflow(wf))

  ## Test quantile prediction -------------------------------------------------

  # TODO: Figure out how to extract quantiles from parsnip workflow
  # May need custom prediction wrapper function

  # Expected behavior:
  # preds <- predict_quantiles(wf, test_data, quantiles = c(0.05, 0.95))
  # expect_s3_class(preds, "tbl_df")
  # expect_true("q05" %in% names(preds))
  # expect_true("q95" %in% names(preds))
  # expect_true(all(preds$q05 <= preds$q95))  # No crossings

  skip("Quantile prediction wrapper not yet implemented")
})

## -----------------------------------------------------------------------------
## Test Group 2: Training Pipeline - Non-Texture Properties
## -----------------------------------------------------------------------------

test_that("train_cluster_models_with_uq() trains 2 models for non-texture", {

  skip("Function not yet implemented")

  # Expected signature:
  # train_cluster_models_with_uq(
  #   cluster_data,
  #   property,
  #   config,           # Winning config from optimization
  #   cv_folds = 10,
  #   grid_size = 10
  # )

  # Expected return structure:
  # list(
  #   point_model    = <workflow>,  # Best config (cubist, PLSR, etc.)
  #   quantile_model = <workflow>,  # Ranger quantile model
  #   config_used    = config,
  #   property       = property,
  #   cluster_id     = cluster_id
  # )

  # TODO: Create test data, call function, verify structure
})

test_that("point model can be ANY of the 9 model types", {

  skip("Function not yet implemented")

  # Test that point model respects config$model
  # Try: cubist, random_forest, PLSR, xgboost
  # Quantile model should ALWAYS be ranger

  # TODO: Loop through model types, verify independence
})

test_that("quantile model is always ranger regardless of point model", {

  skip("Function not yet implemented")

  # Even if point model is cubist, quantile should be ranger
  # Even if point model is PLSR, quantile should be ranger

  # TODO: Test decoupling
})

## -----------------------------------------------------------------------------
## Test Group 3: Training Pipeline - Texture Properties (ILR)
## -----------------------------------------------------------------------------

test_that("train_cluster_models_with_uq() trains 4 models for texture", {

  skip("Function not yet implemented")

  # For texture properties (sand, silt, clay):
  # Should train on ILR coordinates (ilr_1, ilr_2)

  # Expected return structure:
  # list(
  #   ilr_1_point    = <workflow>,
  #   ilr_1_quantile = <workflow>,
  #   ilr_2_point    = <workflow>,
  #   ilr_2_quantile = <workflow>,
  #   config_used    = config,
  #   property       = "clay",  # Original property requested
  #   is_texture     = TRUE
  # )

  # TODO: Create texture test data with ILR transform
  # TODO: Verify 4 models trained
})

test_that("ILR transformation applied correctly for texture", {

  skip("Function not yet implemented")

  # Verify that:
  # 1. load_ossl_raw("clay") returns all 3 texture columns
  # 2. texture_to_ilr() transforms to 2 coordinates
  # 3. Models trained on ilr_1, ilr_2 (NOT on clay directly)
  # 4. Predictions back-transformed to texture space

  # TODO: End-to-end texture UQ test
})

## -----------------------------------------------------------------------------
## Test Group 4: Quantile Prediction
## -----------------------------------------------------------------------------

test_that("predict_with_quantiles() returns point + intervals", {

  skip("Function not yet implemented")

  # Expected signature:
  # predict_with_quantiles(
  #   point_model,
  #   quantile_model,
  #   new_data,
  #   quantiles = c(0.05, 0.95)
  # )

  # Expected return:
  # tibble(
  #   .pred       = <point predictions>,
  #   .pred_lower = <q05 from quantile model>,
  #   .pred_upper = <q95 from quantile model>
  # )

  # TODO: Test on simple data
})

test_that("quantile predictions have no crossings", {

  skip("Function not yet implemented")

  # Verify: .pred_lower <= .pred_upper for all samples
  # If crossings exist, should be repaired

  # TODO: Test monotonicity enforcement
})

test_that("texture quantile predictions back-transform correctly", {

  skip("Function not yet implemented")

  # For texture:
  # 1. Predict ilr_1 quantiles
  # 2. Predict ilr_2 quantiles
  # 3. Back-transform to sand/silt/clay
  # 4. Verify point predictions sum to 100%
  # 5. Intervals are marginal (may not sum to 100% - document this)

  # TODO: Full texture UQ workflow
})

## -----------------------------------------------------------------------------
## Test Group 5: Conformal Calibration (M3.3)
## -----------------------------------------------------------------------------

test_that("compute_conformal_margin() calculates c_alpha", {

  skip("Function not yet implemented - M3.3")

  # Expected signature:
  # compute_conformal_margin(
  #   quantile_model,
  #   calibration_data,  # Held-out 20%
  #   alpha = 0.10       # For 90% coverage
  # )

  # Expected return:
  # list(
  #   c_alpha = <numeric>,  # Margin to add to quantiles
  #   coverage_pre = <numeric>,  # Coverage before adjustment
  #   coverage_post = <numeric>  # Should be ~0.90
  # )

  # TODO: Test on synthetic data with known quantiles
})

test_that("apply_conformal_adjustment() inflates intervals", {

  skip("Function not yet implemented - M3.3")

  # Input: quantile predictions + c_alpha
  # Output: adjusted intervals [q05 - c_alpha, q95 + c_alpha]

  # TODO: Verify adjustment applied correctly
})

test_that("conformal calibration achieves 90% coverage", {

  skip("Function not yet implemented - M3.3")

  # Integration test:
  # 1. Train quantile model
  # 2. Compute c_alpha on calibration set
  # 3. Predict on test set
  # 4. Verify 88-92% of true values fall in intervals

  # TODO: Full conformal workflow test
})

## -----------------------------------------------------------------------------
## Test Group 6: Integration Tests
## -----------------------------------------------------------------------------

test_that("full UQ workflow: non-texture property", {

  skip("Integration test - implement after components work")

  # End-to-end test for pH:
  # 1. Load OSSL data for pH
  # 2. Cluster library
  # 3. Train point + quantile models
  # 4. Compute conformal margin
  # 5. Predict on test samples with UQ
  # 6. Verify coverage

  # TODO: Full pipeline integration test
})

test_that("full UQ workflow: texture property", {

  skip("Integration test - implement after components work")

  # End-to-end test for clay:
  # 1. Load OSSL with all 3 texture columns
  # 2. Apply ILR transformation
  # 3. Train 4 models (2 coords × 2 model types)
  # 4. Compute conformal margins for each coordinate
  # 5. Predict on test samples
  # 6. Back-transform to texture space
  # 7. Verify point predictions sum to 100%
  # 8. Verify coverage

  # TODO: Full texture UQ integration test
})

test_that("UQ works with different winning point models", {

  skip("Integration test - implement after components work")

  # Test decoupling:
  # 1. Force point model to be cubist
  # 2. Train UQ pipeline
  # 3. Force point model to be PLSR
  # 4. Train UQ pipeline
  # 5. Verify quantile model always ranger
  # 6. Verify coverage similar for both

  # TODO: Test model independence
})

## -----------------------------------------------------------------------------
## Test Group 7: Edge Cases & Error Handling
## -----------------------------------------------------------------------------

test_that("UQ handles small sample sizes gracefully", {

  skip("Edge case - implement after main functionality")

  # What happens with n < 100 per cluster?
  # Should still work, but wider intervals expected

  # TODO: Test small sample behavior
})

test_that("UQ handles extreme predictions", {

  skip("Edge case - implement after main functionality")

  # Test bounds enforcement:
  # - pH: [0, 14]
  # - OC, N: >= 0
  # - Texture: each component [0, 100], sum = 100

  # TODO: Test bounds checking
})

test_that("UQ fails gracefully with insufficient calibration data", {

  skip("Error handling - implement after main functionality")

  # If calibration set < 50 samples, should warn
  # If < 20 samples, should error

  # TODO: Test calibration set size validation
})

## -----------------------------------------------------------------------------
## Test Group 6: Pinball Loss Helpers (M3.3.1)
## -----------------------------------------------------------------------------

test_that("calculate_pinball_loss() computes correct loss for q05", {

  ## Test data ----------------------------------------------------------------

  truth      <- c(0, 0, 0, 0)
  pred_lower <- c(-1, 0, 1, -0.5)  # q05 predictions
  pred_upper <- c(1, 2, 3, 1.5)    # q95 predictions (ignored for q05)

  ## Calculate loss -----------------------------------------------------------

  loss <- horizons:::calculate_pinball_loss(
    truth      = truth,
    pred_lower = pred_lower,
    pred_upper = pred_upper,
    tau_lower  = 0.05,
    tau_upper  = 0.95
  )

  ## Manual calculation -------------------------------------------------------
  ## For q05 (tau=0.05):
  ## Sample 1: error = 0 - (-1) = 1  → loss = max(0.05*1, -0.95*1) = 0.05
  ## Sample 2: error = 0 - 0 = 0     → loss = 0
  ## Sample 3: error = 0 - 1 = -1    → loss = max(0.05*(-1), -0.95*(-1)) = 0.95
  ## Sample 4: error = 0 - (-0.5) = 0.5 → loss = max(0.05*0.5, -0.95*0.5) = 0.025
  ##
  ## For q95 (tau=0.95):
  ## Sample 1: error = 0 - 1 = -1    → loss = max(0.95*(-1), -0.05*(-1)) = 0.05
  ## Sample 2: error = 0 - 2 = -2    → loss = max(0.95*(-2), -0.05*(-2)) = 0.10
  ## Sample 3: error = 0 - 3 = -3    → loss = max(0.95*(-3), -0.05*(-3)) = 0.15
  ## Sample 4: error = 0 - 1.5 = -1.5 → loss = max(0.95*(-1.5), -0.05*(-1.5)) = 0.075
  ##
  ## Average: (0.05 + 0 + 0.95 + 0.025 + 0.05 + 0.10 + 0.15 + 0.075) / 8 = 0.175

  expect_type(loss, "double")
  expect_length(loss, 1)
  expect_equal(loss, 0.175, tolerance = 0.001)
})

test_that("calculate_pinball_loss() penalizes q05 over-prediction heavily", {

  ## For q05, over-prediction (pred > truth) should have high penalty
  ## For q95, under-prediction (pred < truth) should have high penalty

  ## Test q05 asymmetry -------------------------------------------------------

  truth_q05 <- rep(0, 10)
  under_pred <- rep(-1, 10)  # Under-predict by 1 unit
  over_pred  <- rep(1, 10)   # Over-predict by 1 unit
  dummy_upper <- rep(2, 10)

  loss_under <- horizons:::calculate_pinball_loss(truth_q05, under_pred, dummy_upper, 0.05, 0.95)
  loss_over  <- horizons:::calculate_pinball_loss(truth_q05, over_pred, dummy_upper, 0.05, 0.95)

  ## Over-prediction should be penalized ~19× more for q05
  ## (Averaged with q95 which has opposite asymmetry, so factor is smaller)

  expect_gt(loss_over, loss_under)  # Over-prediction is worse
})

test_that("calculate_pinball_loss() handles NA values", {

  truth      <- c(0, 0, NA, 0)
  pred_lower <- c(-1, 0, 1, -0.5)
  pred_upper <- c(1, 2, 3, 1.5)

  loss <- horizons:::calculate_pinball_loss(truth, pred_lower, pred_upper)

  ## Should handle NAs gracefully (na.rm = TRUE in mean())

  expect_type(loss, "double")
  expect_false(is.na(loss))
})

test_that("extract_oof_quantiles() returns correct structure", {

  skip_if_not_installed("rsample")
  skip_if_not_installed("workflows")

  ## Create test data ---------------------------------------------------------

  set.seed(123)
  test_data <- tibble::tibble(
    Response  = rnorm(100, mean = 0, sd = 1),
    Sample_ID = paste0("S", 1:100),
    Project   = "test",
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  ## Create CV folds ----------------------------------------------------------

  cv_folds <- rsample::vfold_cv(test_data, v = 3, strata = Response)

  ## Create simple quantile workflow -----------------------------------------

  spec <- define_quantile_specification()
  spec$args$mtry  <- 2
  spec$args$min_n <- 5

  rec <- recipes::recipe(Response ~ x1 + x2, data = test_data)
  wf  <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(spec)

  wf_fitted <- parsnip::fit(wf, data = test_data)

  ## Extract OOF quantiles ----------------------------------------------------

  oof_quantiles <- horizons:::extract_oof_quantiles(
    workflow   = wf_fitted,
    train_data = test_data,
    resamples  = cv_folds,
    quantiles  = c(0.05, 0.95)
  )

  ## Validate structure -------------------------------------------------------

  expect_s3_class(oof_quantiles, "data.frame")
  expect_true(".row" %in% names(oof_quantiles))
  expect_true(".pred_lower" %in% names(oof_quantiles))
  expect_true(".pred_upper" %in% names(oof_quantiles))

  ## Should have predictions for all samples ----------------------------------

  expect_equal(nrow(oof_quantiles), 100)

  ## .row should match original indices ---------------------------------------

  expect_equal(sort(oof_quantiles$.row), 1:100)

  ## Quantiles should be ordered (no crossings) -------------------------------

  expect_true(all(oof_quantiles$.pred_lower <= oof_quantiles$.pred_upper))
})

test_that("extract_oof_quantiles() predictions are out-of-fold", {

  skip("Integration test - requires careful validation")

  ## This test verifies that predictions for each fold come from a model
  ## that was NOT trained on those samples (true OOF)
  ##
  ## Strategy:
  ## 1. Use a perfectly predictable dataset (y = x)
  ## 2. Extract OOF quantiles
  ## 3. Verify OOF predictions are less accurate than in-sample
  ##    (OOF error > in-sample error)

  # TODO: Implement careful OOF validation
})
