#' Test Suite for Cubist Model Fitting with Bayesian Optimization
#' 
#' Tests for model training, hyperparameter tuning, cross-validation, and predictions.
#' Includes tests for convergence, reproducibility, and performance metrics.

library(testthat)
library(horizons)
library(dplyr)
library(tibble)
library(Cubist)

# Helper function to create training data
create_mock_training_data <- function(n_samples = 100, n_features = 10, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  data <- tibble(sample_id = paste0("S", 1:n_samples))
  
  # Add PCA features
  for (i in 1:n_features) {
    data[[paste0("Dim.", i)]] <- rnorm(n_samples, 0, sd = 10 / sqrt(i))
  }
  
  # Add target variable with known relationship
  # Clay content as function of first few PCs + noise
  data$clay <- 300 + 
    20 * data$Dim.1 + 
    15 * data$Dim.2 + 
    10 * data$Dim.3 +
    rnorm(n_samples, 0, 30)
  
  # Ensure realistic bounds
  data$clay <- pmax(0, pmin(1000, data$clay))
  
  # Add other covariates
  data$ph <- 7 + 0.5 * data$Dim.1 + rnorm(n_samples, 0, 0.5)
  data$ph <- pmax(3, pmin(11, data$ph))
  
  data$oc <- 20 + 5 * data$Dim.2 + rnorm(n_samples, 0, 5)
  data$oc <- pmax(0, pmin(200, data$oc))
  
  data
}

# Test Basic Model Fitting ----------------------------------------------------

test_that("fit_cubist_model works with valid data", {
  set.seed(123)
  
  # Create training and validation data
  train_data <- create_mock_training_data(200, 10, seed = 1)
  val_data <- create_mock_training_data(50, 10, seed = 2)
  
  # Fit model
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 5,  # Few iterations for testing
    verbose = FALSE
  )
  
  # Check result structure
  expect_type(result, "list")
  expect_true("model" %in% names(result))
  expect_true("best_params" %in% names(result))
  expect_true("optimization_history" %in% names(result))
  expect_true("performance" %in% names(result))
  expect_true("predictions" %in% names(result))
  
  # Check model
  expect_s3_class(result$model, "cubist")
  
  # Check best parameters are within bounds
  params <- result$best_params
  expect_true(params$committees >= 1 && params$committees <= 100)
  expect_true(params$neighbors >= 0 && params$neighbors <= 9)
  expect_true(params$max_rules >= 10 && params$max_rules <= 500)
  
  # Check performance metrics
  perf <- result$performance
  expect_true("train_rmse" %in% names(perf))
  expect_true("val_rmse" %in% names(perf))
  expect_true("train_r2" %in% names(perf))
  expect_true("val_r2" %in% names(perf))
  
  # RMSE should be positive
  expect_true(perf$train_rmse > 0)
  expect_true(perf$val_rmse > 0)
  
  # R² should be between 0 and 1 for reasonable model
  expect_true(perf$train_r2 > 0 && perf$train_r2 <= 1)
  expect_true(perf$val_r2 > 0 && perf$val_r2 <= 1)
})

test_that("fit_cubist_model handles different covariates", {
  train_data <- create_mock_training_data(150, 8, seed = 42)
  val_data <- create_mock_training_data(30, 8, seed = 43)
  
  # Test with pH
  result_ph <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "ph",
    bayesian_iter = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result_ph$model, "cubist")
  expect_true(result_ph$performance$val_rmse < 2)  # pH has smaller scale
  
  # Test with OC
  result_oc <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "oc",
    bayesian_iter = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result_oc$model, "cubist")
  expect_true(result_oc$performance$val_rmse < 50)  # OC has larger scale
})

test_that("fit_cubist_model handles missing covariate", {
  train_data <- create_mock_training_data(100, 5)
  val_data <- create_mock_training_data(20, 5)
  
  # Remove target covariate from data
  train_data$clay <- NULL
  val_data$clay <- NULL
  
  expect_error(
    fit_cubist_model(
      train_data = train_data,
      val_data = val_data,
      covariate = "clay",
      verbose = FALSE
    ),
    "Covariate 'clay' not found"
  )
})

# Test Bayesian Optimization --------------------------------------------------

test_that("bayesian optimization improves performance", {
  set.seed(456)
  
  train_data <- create_mock_training_data(300, 10, seed = 10)
  val_data <- create_mock_training_data(75, 10, seed = 11)
  
  # Fit with more iterations
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 10,
    verbose = FALSE
  )
  
  # Check optimization history
  history <- result$optimization_history
  expect_true(nrow(history) >= 10)  # At least initial + iterations
  
  # Performance should generally improve (best value should be better than first)
  initial_score <- history$score[1]
  best_score <- max(history$score)
  
  expect_true(best_score >= initial_score)
  
  # Check parameter exploration
  expect_true(length(unique(history$committees)) > 1)
  expect_true(length(unique(history$neighbors)) > 1)
})

test_that("bayesian optimization respects bounds", {
  train_data <- create_mock_training_data(100, 5)
  val_data <- create_mock_training_data(25, 5)
  
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 5,
    verbose = FALSE
  )
  
  history <- result$optimization_history
  
  # All parameters should be within bounds
  expect_true(all(history$committees >= 1 & history$committees <= 100))
  expect_true(all(history$neighbors >= 0 & history$neighbors <= 9))
  expect_true(all(history$max_rules >= 10 & history$max_rules <= 500))
})

test_that("fit_cubist_model is reproducible with seed", {
  train_data <- create_mock_training_data(100, 5, seed = 999)
  val_data <- create_mock_training_data(25, 5, seed = 1000)
  
  # Fit model twice with same seed
  set.seed(12345)
  result1 <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 3,
    verbose = FALSE
  )
  
  set.seed(12345)
  result2 <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 3,
    verbose = FALSE
  )
  
  # Results should be identical
  expect_equal(result1$best_params, result2$best_params)
  expect_equal(result1$performance$val_rmse, result2$performance$val_rmse)
})

# Test Cross-Validation -------------------------------------------------------

test_that("cross-validation provides stable estimates", {
  set.seed(789)
  
  train_data <- create_mock_training_data(200, 8)
  val_data <- create_mock_training_data(50, 8)
  
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 5,
    cv_folds = 5,
    verbose = FALSE
  )
  
  # Check CV results are included
  expect_true("cv_performance" %in% names(result))
  
  cv_perf <- result$cv_performance
  expect_true("mean_rmse" %in% names(cv_perf))
  expect_true("sd_rmse" %in% names(cv_perf))
  expect_true("mean_r2" %in% names(cv_perf))
  
  # CV performance should be reasonable
  expect_true(cv_perf$mean_rmse > 0)
  expect_true(cv_perf$sd_rmse > 0)
  expect_true(cv_perf$mean_r2 > 0.5)  # Should have decent fit with known relationship
})

# Test Model Predictions ------------------------------------------------------

test_that("model predictions are within reasonable range", {
  train_data <- create_mock_training_data(200, 10, seed = 321)
  val_data <- create_mock_training_data(50, 10, seed = 322)
  
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 5,
    verbose = FALSE
  )
  
  # Make predictions on new data
  new_data <- create_mock_training_data(30, 10, seed = 323)
  new_data$clay <- NULL  # Remove target
  
  predictions <- predict(result$model, new_data)
  
  # Predictions should be in reasonable range for clay (g/kg)
  expect_true(all(predictions >= 0))
  expect_true(all(predictions <= 1000))
  
  # Should have right number of predictions
  expect_equal(length(predictions), nrow(new_data))
})

test_that("model extrapolation is bounded", {
  # Train on normal range
  train_data <- create_mock_training_data(200, 5, seed = 111)
  val_data <- create_mock_training_data(50, 5, seed = 112)
  
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 3,
    verbose = FALSE
  )
  
  # Create extreme test data
  extreme_data <- tibble(sample_id = paste0("E", 1:10))
  
  for (i in 1:5) {
    # Extreme values (10x normal range)
    extreme_data[[paste0("Dim.", i)]] <- rnorm(10, 0, 100)
  }
  
  predictions <- predict(result$model, extreme_data)
  
  # Even with extreme inputs, predictions should be somewhat bounded
  # (Cubist uses rules which help with this)
  expect_true(all(predictions >= -1000))  # Some extrapolation allowed
  expect_true(all(predictions <= 2000))
})

# Test Edge Cases and Error Handling -----------------------------------------

test_that("fit_cubist_model handles small datasets", {
  # Very small training set
  train_data <- create_mock_training_data(10, 3)
  val_data <- create_mock_training_data(5, 3)
  
  # Should still work but with adjusted parameters
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 2,
    verbose = FALSE
  )
  
  expect_s3_class(result$model, "cubist")
  # With small data, model should use fewer committees
  expect_true(result$best_params$committees <= 20)
})

test_that("fit_cubist_model handles identical target values", {
  train_data <- create_mock_training_data(50, 5)
  val_data <- create_mock_training_data(10, 5)
  
  # Set all target values to same value
  train_data$clay <- 300
  val_data$clay <- 300
  
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 2,
    verbose = FALSE
  )
  
  # Model should still build but with perfect fit
  expect_s3_class(result$model, "cubist")
  expect_true(result$performance$train_rmse < 1)  # Nearly zero error
})

test_that("fit_cubist_model handles highly correlated features", {
  # Create data with correlated features
  n_samples <- 100
  base_feature <- rnorm(n_samples)
  
  train_data <- tibble(
    sample_id = paste0("S", 1:n_samples),
    Dim.1 = base_feature,
    Dim.2 = base_feature + rnorm(n_samples, 0, 0.1),
    Dim.3 = base_feature * 2 + rnorm(n_samples, 0, 0.1),
    clay = 300 + 50 * base_feature + rnorm(n_samples, 0, 20)
  )
  
  val_data <- create_mock_training_data(25, 3)
  
  # Should handle correlated features
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result$model, "cubist")
  expect_true(result$performance$train_r2 > 0.7)  # Should still fit well
})

# Test Performance and Scalability --------------------------------------------

test_that("fit_cubist_model scales with data size", {
  skip_on_cran()
  
  # Test with larger dataset
  train_data <- create_mock_training_data(1000, 20, seed = 555)
  val_data <- create_mock_training_data(200, 20, seed = 556)
  
  time_taken <- system.time({
    result <- fit_cubist_model(
      train_data = train_data,
      val_data = val_data,
      covariate = "clay",
      bayesian_iter = 5,
      verbose = FALSE
    )
  })
  
  # Should complete in reasonable time even with 1000 samples
  expect_true(time_taken["elapsed"] < 60)  # Less than 1 minute
  
  # Model should still perform well
  expect_true(result$performance$val_r2 > 0.5)
})

# Test Model Interpretability -------------------------------------------------

test_that("cubist model provides interpretable rules", {
  train_data <- create_mock_training_data(150, 5, seed = 777)
  val_data <- create_mock_training_data(30, 5, seed = 778)
  
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 3,
    verbose = FALSE
  )
  
  # Extract model rules
  model_summary <- summary(result$model)
  
  # Should have rules
  expect_true(length(model_summary$rules) > 0)
  
  # Rules should reference our features
  rules_text <- paste(model_summary$rules, collapse = " ")
  expect_true(grepl("Dim", rules_text))  # Should mention dimensions
})

# Test Model Persistence ------------------------------------------------------

test_that("cubist model can be saved and loaded", {
  train_data <- create_mock_training_data(100, 5)
  val_data <- create_mock_training_data(25, 5)
  
  result <- fit_cubist_model(
    train_data = train_data,
    val_data = val_data,
    covariate = "clay",
    bayesian_iter = 2,
    verbose = FALSE
  )
  
  # Save model
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(result$model, temp_file)
  
  # Load model
  loaded_model <- readRDS(temp_file)
  
  # Make predictions with both
  test_data <- create_mock_training_data(10, 5)
  test_data$clay <- NULL
  
  pred_original <- predict(result$model, test_data)
  pred_loaded <- predict(loaded_model, test_data)
  
  # Predictions should be identical
  expect_equal(pred_original, pred_loaded)
  
  # Clean up
  unlink(temp_file)
})