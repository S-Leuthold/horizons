#' Integration Test Suite for Complete Soil Covariate Prediction Pipeline
#' 
#' End-to-end tests for the full prediction workflow from raw spectra to predictions.
#' Tests the integration of all components and validates statistical properties.

library(testthat)
library(horizons)
library(dplyr)
library(tibble)

# Mock Data Generation --------------------------------------------------------

#' Generate realistic mock spectral dataset
create_mock_spectral_dataset <- function(n_samples = 100, 
                                       n_wavelengths = 200,
                                       seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Create base spectral patterns
  wavelengths <- seq(600, 4000, length.out = n_wavelengths)
  
  data <- tibble(sample_id = paste0("SAMPLE_", sprintf("%04d", 1:n_samples)))
  
  # Generate realistic spectral curves
  for (i in seq_along(wavelengths)) {
    wl <- wavelengths[i]
    col_name <- paste0("scan_mir.", round(wl, 1))
    
    # Create absorption features at specific wavelengths
    base_absorption <- 0.5
    
    # Add absorption peaks
    if (wl > 1400 && wl < 1500) base_absorption <- 0.7  # OH stretch
    if (wl > 2800 && wl < 3000) base_absorption <- 0.8  # CH stretch
    if (wl > 1600 && wl < 1700) base_absorption <- 0.65 # Amide
    
    # Add sample variation
    data[[col_name]] <- base_absorption + 
      rnorm(n_samples, 0, 0.1) +
      runif(n_samples, -0.05, 0.05)
  }
  
  # Add soil properties correlated with spectral features
  # Clay affects OH absorption
  oh_region <- data[, grep("scan_mir\\.14[0-9]{2}", names(data))]
  oh_mean <- rowMeans(as.matrix(oh_region), na.rm = TRUE)
  
  data$clay <- 200 + 500 * oh_mean + rnorm(n_samples, 0, 50)
  data$clay <- pmax(0, pmin(1000, data$clay))
  
  # pH affects overall baseline
  baseline <- rowMeans(as.matrix(data[, grep("^scan_mir\\.", names(data))]), na.rm = TRUE)
  data$ph <- 4 + 6 * baseline + rnorm(n_samples, 0, 0.5)
  data$ph <- pmax(3, pmin(11, data$ph))
  
  # OC affects CH absorption
  ch_region <- data[, grep("scan_mir\\.2[89][0-9]{2}", names(data))]
  if (ncol(ch_region) > 0) {
    ch_mean <- rowMeans(as.matrix(ch_region), na.rm = TRUE)
    data$oc <- 5 + 40 * ch_mean + rnorm(n_samples, 0, 5)
  } else {
    data$oc <- runif(n_samples, 5, 50)
  }
  data$oc <- pmax(0, pmin(200, data$oc))
  
  data
}

#' Create mock OSSL database
create_mock_ossl_database <- function(n_samples = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Generate diverse spectral library
  ossl_data <- create_mock_spectral_dataset(n_samples, n_wavelengths = 200, seed = seed)
  
  # Add OSSL-specific metadata
  ossl_data$dataset <- sample(
    c("LUCAS", "USDA", "AFRICA", "GLOBAL"),
    n_samples,
    replace = TRUE,
    prob = c(0.3, 0.3, 0.2, 0.2)
  )
  
  # Rename for OSSL format
  ossl_data <- ossl_data %>%
    rename(
      clay.tot_usda.c60_w.pct = clay,
      ph.h2o_usda.a268_index = ph,
      oc_usda.c729_w.pct = oc
    ) %>%
    mutate(
      clay.tot_usda.c60_w.pct = clay.tot_usda.c60_w.pct / 10,  # Convert to %
      oc_usda.c729_w.pct = oc_usda.c729_w.pct / 10  # Convert to %
    )
  
  ossl_data
}

# Integration Tests -----------------------------------------------------------

test_that("complete pipeline works end-to-end", {
  skip("Full integration test - run manually")
  
  set.seed(2024)
  
  # Create mock unknown samples
  unknown_samples <- create_mock_spectral_dataset(50, seed = 100)
  unknown_samples <- select(unknown_samples, -clay, -ph, -oc)  # Remove targets
  
  # Mock OSSL data
  with_mocked_ossl({
    # Run complete prediction
    predictions <- predict_soil_covariates(
      input_data = unknown_samples,
      covariates = c("clay", "ph"),
      n_similar = 200,
      n_train = 160,
      n_val = 40,
      variance_threshold = 0.95,
      bayesian_iter = 5,
      verbose = FALSE
    )
    
    # Check output structure
    expect_type(predictions, "list")
    expect_true("predictions" %in% names(predictions))
    expect_true("models" %in% names(predictions))
    expect_true("selection_quality" %in% names(predictions))
    expect_true("processing_info" %in% names(predictions))
    
    # Check predictions
    preds <- predictions$predictions
    expect_equal(nrow(preds), nrow(unknown_samples))
    expect_true("clay" %in% names(preds))
    expect_true("ph" %in% names(preds))
    
    # Check predictions are in valid ranges
    expect_true(all(preds$clay >= 0 & preds$clay <= 1000))
    expect_true(all(preds$ph >= 3 & preds$ph <= 11))
    
    # Check models were created
    expect_equal(length(predictions$models), 2)
    expect_s3_class(predictions$models$clay$model, "cubist")
    expect_s3_class(predictions$models$ph$model, "cubist")
  })
})

test_that("pipeline handles single unknown sample", {
  skip("Requires mock OSSL setup")
  
  # Single unknown sample
  unknown_single <- create_mock_spectral_dataset(1, seed = 200)
  unknown_single <- select(unknown_single, -clay, -ph, -oc)
  
  with_mocked_ossl({
    predictions <- predict_soil_covariates(
      input_data = unknown_single,
      covariates = c("clay"),
      n_similar = 100,
      verbose = FALSE
    )
    
    expect_equal(nrow(predictions$predictions), 1)
    expect_false(is.na(predictions$predictions$clay[1]))
  })
})

test_that("pipeline handles multiple batches correctly", {
  skip("Requires mock OSSL setup")
  
  # Create two batches of unknown samples
  batch1 <- create_mock_spectral_dataset(25, seed = 301)
  batch2 <- create_mock_spectral_dataset(25, seed = 302)
  
  # Remove targets
  batch1_input <- select(batch1, -clay, -ph, -oc)
  batch2_input <- select(batch2, -clay, -ph, -oc)
  
  with_mocked_ossl({
    # Process both batches with same training set
    pred1 <- predict_soil_covariates(
      input_data = batch1_input,
      covariates = "clay",
      n_similar = 150,
      verbose = FALSE
    )
    
    pred2 <- predict_soil_covariates(
      input_data = batch2_input,
      covariates = "clay",
      n_similar = 150,
      verbose = FALSE
    )
    
    # Both should produce valid predictions
    expect_equal(nrow(pred1$predictions), 25)
    expect_equal(nrow(pred2$predictions), 25)
    
    # Selection quality should be similar
    expect_true(abs(pred1$selection_quality$coverage - 
                   pred2$selection_quality$coverage) < 0.2)
  })
})

# Test Data Flow and Transformations -----------------------------------------

test_that("data transformations are consistent through pipeline", {
  set.seed(456)
  
  # Create test data
  unknown <- create_mock_spectral_dataset(30, n_wavelengths = 100, seed = 400)
  spectral_cols <- grep("^scan_mir\\.", names(unknown), value = TRUE)
  original_spectra <- as.matrix(unknown[spectral_cols])
  
  # Step 1: Preprocessing
  preprocessed <- preprocess_mir_spectra(unknown, smooth_window = 9)
  processed_spectra <- as.matrix(preprocessed[spectral_cols])
  
  # Check preprocessing changed the data
  expect_false(isTRUE(all.equal(original_spectra, processed_spectra)))
  
  # Step 2: PCA transformation
  mock_ossl <- create_mock_ossl_database(500, seed = 401)
  mock_ossl_processed <- preprocess_mir_spectra(mock_ossl, smooth_window = 9)
  
  pca_result <- perform_pca_on_ossl(mock_ossl_processed, variance_threshold = 0.95)
  unknown_pca <- apply_pca_to_unknown(
    preprocessed,
    pca_result$pca_model,
    pca_result$n_components
  )
  
  # Check PCA reduced dimensions
  pca_cols <- grep("^Dim\\.", names(unknown_pca), value = TRUE)
  expect_true(length(pca_cols) < length(spectral_cols))
  expect_equal(nrow(unknown_pca), nrow(unknown))
  
  # Check sample IDs are preserved
  expect_equal(unknown_pca$sample_id, unknown$sample_id)
})

test_that("training set selection maintains diversity", {
  set.seed(789)
  
  # Create unknown samples with distinct groups
  group1 <- create_mock_spectral_dataset(25, seed = 501)
  group2 <- create_mock_spectral_dataset(25, seed = 502)
  
  # Make groups spectrally different
  spectral_cols <- grep("^scan_mir\\.", names(group1), value = TRUE)
  group2[spectral_cols] <- group2[spectral_cols] * 1.5
  
  unknown_combined <- bind_rows(group1, group2)
  
  # Create mock OSSL spanning both groups
  mock_ossl <- create_mock_ossl_database(1000, seed = 503)
  
  # Process both datasets
  unknown_processed <- preprocess_mir_spectra(unknown_combined)
  ossl_processed <- preprocess_mir_spectra(mock_ossl)
  
  # PCA
  pca_result <- perform_pca_on_ossl(ossl_processed, variance_threshold = 0.95)
  unknown_pca <- apply_pca_to_unknown(
    unknown_processed, 
    pca_result$pca_model,
    pca_result$n_components
  )
  
  ossl_pca <- pca_result$pca_scores
  
  # Select training set
  selection <- select_global_training_set(
    unknown_pca_scores = unknown_pca,
    ossl_pca_scores = ossl_pca,
    n_select = 200,
    n_train = 160,
    n_val = 40,
    relevance_threshold = 0.6,
    verbose = FALSE
  )
  
  # Check that selection covers both groups
  # (This would be validated by checking PCA space coverage)
  expect_true(selection$selection_quality$coverage > 0.6)
  expect_true(selection$selection_quality$diversity > 0)
})

# Test Error Handling and Edge Cases -----------------------------------------

test_that("pipeline handles missing wavelengths gracefully", {
  # Create data with different wavelength coverage
  unknown <- create_mock_spectral_dataset(20, n_wavelengths = 150, seed = 600)
  
  # Remove some wavelength columns
  spectral_cols <- grep("^scan_mir\\.", names(unknown), value = TRUE)
  cols_to_remove <- sample(spectral_cols, 20)
  unknown_missing <- select(unknown, -all_of(cols_to_remove))
  
  # Should handle with interpolation or error appropriately
  expect_error(
    preprocess_mir_spectra(unknown_missing),
    NA  # Shouldn't error, should handle gracefully
  )
})

test_that("pipeline handles extreme values", {
  unknown <- create_mock_spectral_dataset(30, seed = 700)
  
  # Add extreme values
  spectral_cols <- grep("^scan_mir\\.", names(unknown), value = TRUE)
  unknown[[spectral_cols[1]]][1] <- 1000  # Extreme high
  unknown[[spectral_cols[2]]][2] <- -1000  # Extreme low
  unknown[[spectral_cols[3]]][3] <- NA  # Missing
  
  # Preprocessing should handle extremes
  preprocessed <- preprocess_mir_spectra(unknown)
  
  # Check no infinite values
  spec_matrix <- as.matrix(preprocessed[spectral_cols])
  expect_false(any(is.infinite(spec_matrix), na.rm = TRUE))
})

# Test Performance Metrics ----------------------------------------------------

test_that("prediction accuracy meets requirements", {
  skip("Requires full dataset for meaningful accuracy test")
  
  set.seed(1234)
  
  # Create test dataset with known relationships
  test_data <- create_mock_spectral_dataset(200, seed = 800)
  
  # Split into train and test
  train_idx <- sample(1:200, 150)
  train_data <- test_data[train_idx, ]
  test_data <- test_data[-train_idx, ]
  
  # Save true values
  true_clay <- test_data$clay
  true_ph <- test_data$ph
  
  # Remove targets from test
  test_input <- select(test_data, -clay, -ph, -oc)
  
  with_mocked_ossl({
    # Make predictions
    predictions <- predict_soil_covariates(
      input_data = test_input,
      covariates = c("clay", "ph"),
      n_similar = 200,
      bayesian_iter = 10,
      verbose = FALSE
    )
    
    # Calculate metrics
    clay_rmse <- sqrt(mean((predictions$predictions$clay - true_clay)^2))
    clay_r2 <- cor(predictions$predictions$clay, true_clay)^2
    
    ph_rmse <- sqrt(mean((predictions$predictions$ph - true_ph)^2))
    ph_r2 <- cor(predictions$predictions$ph, true_ph)^2
    
    # Check performance thresholds
    expect_true(clay_r2 > 0.5)  # R² > 0.5 for clay
    expect_true(ph_r2 > 0.5)  # R² > 0.5 for pH
    
    expect_true(clay_rmse < 100)  # RMSE < 100 g/kg for clay
    expect_true(ph_rmse < 1)  # RMSE < 1 for pH
  })
})

# Test Statistical Properties -------------------------------------------------

test_that("predictions have appropriate uncertainty", {
  skip("Requires uncertainty quantification implementation")
  
  # Create test data
  unknown <- create_mock_spectral_dataset(50, seed = 900)
  unknown_input <- select(unknown, -clay, -ph, -oc)
  
  with_mocked_ossl({
    # Get predictions with uncertainty
    predictions <- predict_soil_covariates(
      input_data = unknown_input,
      covariates = "clay",
      n_similar = 200,
      return_uncertainty = TRUE,  # If implemented
      verbose = FALSE
    )
    
    # Check uncertainty estimates
    if ("clay_sd" %in% names(predictions$predictions)) {
      # Uncertainty should be positive
      expect_true(all(predictions$predictions$clay_sd > 0))
      
      # Uncertainty should scale with prediction
      # (Higher predictions might have higher uncertainty)
      cor_pred_unc <- cor(
        predictions$predictions$clay,
        predictions$predictions$clay_sd
      )
      
      expect_true(abs(cor_pred_unc) < 0.8)  # Not perfectly correlated
    }
  })
})

test_that("cross-validation provides unbiased estimates", {
  skip("Requires full cross-validation implementation")
  
  set.seed(1111)
  
  # Create dataset
  full_data <- create_mock_spectral_dataset(300, seed = 1000)
  
  # Perform k-fold cross-validation
  k_folds <- 5
  cv_results <- list()
  
  for (fold in 1:k_folds) {
    # Create fold indices
    fold_size <- floor(nrow(full_data) / k_folds)
    test_idx <- ((fold - 1) * fold_size + 1):(fold * fold_size)
    
    train_fold <- full_data[-test_idx, ]
    test_fold <- full_data[test_idx, ]
    
    # Save true values
    true_values <- test_fold$clay
    test_input <- select(test_fold, -clay, -ph, -oc)
    
    with_mocked_ossl({
      # Predict
      pred <- predict_soil_covariates(
        input_data = test_input,
        covariates = "clay",
        n_similar = 150,
        verbose = FALSE
      )
      
      cv_results[[fold]] <- list(
        predictions = pred$predictions$clay,
        true = true_values
      )
    })
  }
  
  # Aggregate CV results
  all_pred <- unlist(lapply(cv_results, function(x) x$predictions))
  all_true <- unlist(lapply(cv_results, function(x) x$true))
  
  # Check for bias
  bias <- mean(all_pred - all_true)
  expect_true(abs(bias) < 20)  # Bias < 20 g/kg
  
  # Check variance
  cv_rmse <- sqrt(mean((all_pred - all_true)^2))
  expect_true(cv_rmse < 100)  # CV RMSE < 100 g/kg
})

# Test Reproducibility --------------------------------------------------------

test_that("pipeline produces reproducible results with seed", {
  unknown <- create_mock_spectral_dataset(25, seed = 1100)
  unknown_input <- select(unknown, -clay, -ph, -oc)
  
  # Run pipeline twice with same seed
  set.seed(99999)
  result1 <- with_mocked_ossl({
    predict_soil_covariates(
      input_data = unknown_input,
      covariates = "clay",
      n_similar = 100,
      bayesian_iter = 3,
      verbose = FALSE
    )
  })
  
  set.seed(99999)
  result2 <- with_mocked_ossl({
    predict_soil_covariates(
      input_data = unknown_input,
      covariates = "clay",
      n_similar = 100,
      bayesian_iter = 3,
      verbose = FALSE
    )
  })
  
  # Results should be identical
  expect_equal(
    result1$predictions$clay,
    result2$predictions$clay
  )
  
  expect_equal(
    result1$models$clay$best_params,
    result2$models$clay$best_params
  )
})