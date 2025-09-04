# Simple diagnostic to check what's going wrong with clay prediction
# Run this after reloading the package

run_clay_diagnostic <- function(result) {
  
  cat("=== CLAY PREDICTION DIAGNOSTIC ===\n\n")
  
  # Check if we have the model
  if (is.null(result$global_models$clay)) {
    cat("ERROR: No clay model found in results\n")
    return()
  }
  
  model <- result$global_models$clay
  
  # 1. Check metrics
  cat("1. PERFORMANCE METRICS:\n")
  cat("   Training R²: ", round(model$train_metrics$r2, 3), "\n")
  cat("   Training RMSE: ", round(model$train_metrics$RMSE, 2), "\n")
  cat("   Validation R²: ", round(model$validation_metrics$r2, 3), "\n")
  cat("   Validation RMSE: ", round(model$validation_metrics$RMSE, 2), "\n")
  cat("   -> Overfitting ratio: ", round(model$validation_metrics$RMSE / model$train_metrics$RMSE, 2), "x\n\n")
  
  # 2. Check data (now that we're saving it)
  if (!is.null(model$train_data)) {
    cat("2. TRAINING DATA:\n")
    cat("   N samples: ", nrow(model$train_data), "\n")
    cat("   N predictors: ", ncol(model$train_data) - 1, "\n")
    cat("   Clay range: ", paste(round(range(model$train_data$Response, na.rm = TRUE), 1), collapse = " - "), "\n")
    cat("   Clay mean: ", round(mean(model$train_data$Response, na.rm = TRUE), 1), "\n")
    cat("   Clay SD: ", round(sd(model$train_data$Response, na.rm = TRUE), 1), "\n\n")
    
    cat("3. VALIDATION DATA:\n")
    cat("   N samples: ", nrow(model$val_data), "\n")
    cat("   Clay range: ", paste(round(range(model$val_data$Response, na.rm = TRUE), 1), collapse = " - "), "\n")
    cat("   Clay mean: ", round(mean(model$val_data$Response, na.rm = TRUE), 1), "\n")
    cat("   Clay SD: ", round(sd(model$val_data$Response, na.rm = TRUE), 1), "\n\n")
    
    # Check for data distribution issues
    cat("4. DATA DISTRIBUTION CHECK:\n")
    train_mean <- mean(model$train_data$Response, na.rm = TRUE)
    val_mean <- mean(model$val_data$Response, na.rm = TRUE)
    mean_diff <- abs(train_mean - val_mean)
    
    if (mean_diff > 50) {  # More than 5% difference in original units
      cat("   ⚠️  WARNING: Large difference in train/val means (", round(mean_diff, 1), ")\n")
      cat("   This suggests train/val sets have different distributions\n")
    } else {
      cat("   ✓ Train/val means are similar (diff = ", round(mean_diff, 1), ")\n")
    }
    
    # Check if values are in expected range
    if (max(model$train_data$Response, na.rm = TRUE) > 1000) {
      cat("   ⚠️  WARNING: Clay values > 1000 g/kg detected (impossible!)\n")
      cat("   Unit conversion may be applied incorrectly\n")
    } else if (max(model$train_data$Response, na.rm = TRUE) < 100) {
      cat("   ⚠️  WARNING: Maximum clay < 100 g/kg\n") 
      cat("   Values might still be in percent, not g/kg\n")
    } else {
      cat("   ✓ Clay values in expected range (0-1000 g/kg)\n")
    }
    
    # Check number of PCA components
    n_pca <- sum(grepl("^Dim\\.", names(model$train_data)))
    samples_per_dim <- nrow(model$train_data) / n_pca
    cat("\n5. MODEL COMPLEXITY:\n")
    cat("   PCA components: ", n_pca, "\n")
    cat("   Samples per component: ", round(samples_per_dim, 1), "\n")
    if (samples_per_dim < 10) {
      cat("   ⚠️  WARNING: Too few samples per PCA component (overfitting likely!)\n")
    } else {
      cat("   ✓ Adequate samples per component\n")
    }
    
  } else {
    cat("2-5. Cannot check data - not saved in model output\n")
    cat("     Reload package and rerun to see diagnostic data\n")
  }
  
  cat("\n=== RECOMMENDATIONS ===\n")
  if (model$validation_metrics$RMSE / model$train_metrics$RMSE > 1.5) {
    cat("1. Severe overfitting detected. Try:\n")
    cat("   - Reduce variance_threshold to 0.60 or 0.50\n")
    cat("   - Increase n_similar to get more training samples\n")
    cat("   - Reduce bayesian_iter to minimize overfitting\n")
  }
  
  cat("\nTo get back to basics, try:\n")
  cat("predict_soil_covariates(your_data, 'clay',\n")
  cat("  n_similar = 4000, prop = 0.70,\n")
  cat("  variance_threshold = 0.50, bayesian_iter = 3)\n")
}

# After reloading and rerunning your model:
# run_clay_diagnostic(result)