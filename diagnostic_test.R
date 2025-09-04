# Diagnostic script to investigate clay prediction accuracy drop
# Comparing different train/validation splits

library(horizons)
library(dplyr)

# Test different train/validation configurations
test_configurations <- list(
  current = list(n_similar = 6000, n_train = 5000, n_val = 1000),  # 83.3/16.7
  balanced = list(n_similar = 6000, n_train = 4200, n_val = 1800), # 70/30
  standard = list(n_similar = 6000, n_train = 4800, n_val = 1200), # 80/20
  larger_pool = list(n_similar = 8000, n_train = 6400, n_val = 1600), # 80/20 with more data
  conservative = list(n_similar = 4000, n_train = 3200, n_val = 800) # 80/20 with less data
)

# Function to run quick diagnostic test
run_diagnostic <- function(config_name, params) {
  
  cat("Testing configuration:", config_name, "\n")
  cat("  Train/Val split:", params$n_train, "/", params$n_val, 
      paste0("(", round(params$n_train/(params$n_train + params$n_val)*100, 1), "/", 
             round(params$n_val/(params$n_train + params$n_val)*100, 1), "%)"), "\n")
  
  # Run prediction with this configuration
  tryCatch({
    result <- predict_soil_covariates(
      input_data = your_spectra_data,  # Replace with actual data
      covariates = "clay",
      n_similar = params$n_similar,
      n_train = params$n_train,
      n_val = params$n_val,
      verbose = FALSE
    )
    
    # Extract key metrics
    if (!is.null(result$global_models$clay$validation_metrics)) {
      metrics <- result$global_models$clay$validation_metrics
      cat("  Validation R²:", round(metrics$r2, 3), "\n")
      cat("  Validation RMSE:", round(metrics$RMSE, 2), "\n")
      cat("  Training R²:", round(result$global_models$clay$train_metrics$r2, 3), "\n\n")
      
      return(list(
        config = config_name,
        val_r2 = metrics$r2,
        val_rmse = metrics$RMSE,
        train_r2 = result$global_models$clay$train_metrics$r2,
        overfitting = result$global_models$clay$train_metrics$r2 - metrics$r2
      ))
    }
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n\n")
    return(NULL)
  })
}

# Run diagnostics
cat("=== CLAY PREDICTION ACCURACY DIAGNOSTIC ===\n\n")

# Uncomment and run with your actual data:
# results <- map(names(test_configurations), function(name) {
#   run_diagnostic(name, test_configurations[[name]])
# })
# 
# # Compare results
# valid_results <- results[!sapply(results, is.null)]
# results_df <- do.call(rbind, lapply(valid_results, as.data.frame))
# 
# print(results_df)
# 
# # Find best configuration
# best_config <- results_df[which.max(results_df$val_r2), ]
# cat("\nBest performing configuration:\n")
# print(best_config)