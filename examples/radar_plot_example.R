# Example usage of plot_models_radar function
# This demonstrates how to use the robust radar plot visualization

library(horizons)
library(dplyr)

# =============================================================================
# Example 1: Basic Usage with Sample Data
# =============================================================================

# Create sample model evaluation results
set.seed(123)
sample_results <- data.frame(
  config_desc = c(
    "cubist_NoTrans_SNV_PCA_Clay-pH-CEC",
    "xgboost_Log_SNVD1_SHAP_GDD-MAP",
    "random_forest_Sqrt_Raw_Corr_",
    "plsr_NoTrans_MSC_PCA_Clay",
    "elastic_net_Log_SNV_Boruta_GDD-MAP-PET",
    "cubist_Log_SNVD1_PCA_Clay-pH",
    "xgboost_NoTrans_Raw_SHAP_",
    "random_forest_Log_SNV_Corr_GDD-MAP",
    "plsr_Sqrt_MSCD1_PCA_Clay-pH-CEC",
    "svm_rbf_NoTrans_SNV_SHAP_GDD"
  ),
  rrmse = c(0.45, 0.38, 0.52, 0.48, 0.41, 0.47, 0.36, 0.49, 0.50, 0.43),
  rsq = c(0.82, 0.89, 0.76, 0.79, 0.85, 0.80, 0.91, 0.78, 0.77, 0.83),
  rmse = c(12.5, 10.8, 14.2, 13.1, 11.5, 12.9, 10.2, 13.5, 13.8, 11.9),
  response_var = rep(c("MAOM_C", "Total_C"), each = 5),
  stringsAsFactors = FALSE
)

# Basic radar plot
radar_plot <- plot_models_radar(
  results_data = sample_results,
  metric = "rrmse",
  plot_type = "radar",
  title = "Component Effects on RRMSE"
)

print(radar_plot)

# =============================================================================
# Example 2: Bar Plot with Custom Components
# =============================================================================

bar_plot <- plot_models_radar(
  results_data = sample_results,
  metric = "rsq",
  plot_type = "bar",
  components = c("ModelType", "Preprocessing", "Covariates"),
  normalize = FALSE,
  color_scheme = "viridis",
  title = "Component Effects on R-squared"
)

print(bar_plot)

# =============================================================================
# Example 3: Grouped Analysis by Response Variable
# =============================================================================

grouped_plot <- plot_models_radar(
  results_data = sample_results,
  metric = "rrmse",
  plot_type = "bar",
  group_by = "response_var",
  components = c("ModelType", "Transformation", "Preprocessing"),
  title = "Component Effects by Response Variable"
)

print(grouped_plot)

# =============================================================================
# Example 4: Working with Your Original Code Pattern
# =============================================================================

# Convert your original pattern to work with the new function
original_results <- list(
  full_summary = sample_results
)

# Your original approach (simplified)
results_processed <- original_results$full_summary %>%
  # The parsing is now handled internally by the function
  identity()

# Create the radar plot
final_radar <- plot_models_radar(
  results_data = results_processed,
  metric = "rrmse",
  plot_type = "radar",
  normalize = TRUE,
  title = "Marginal Effects of Pipeline Components",
  color_scheme = "default"
)

# Create the bar plot alternative
final_bar <- plot_models_radar(
  results_data = results_processed,
  metric = "rrmse", 
  plot_type = "bar",
  normalize = TRUE,
  title = "Component Importance Analysis"
)

print(final_radar)
print(final_bar)

# =============================================================================
# Example 5: Advanced Usage with Multiple Metrics
# =============================================================================

# Create plots for different metrics
metrics <- c("rrmse", "rsq", "rmse")
plot_list <- list()

for (metric in metrics) {
  plot_list[[metric]] <- plot_models_radar(
    results_data = sample_results,
    metric = metric,
    plot_type = "bar",
    components = c("ModelType", "Preprocessing", "Feature_Selection"),
    title = paste("Component Effects on", toupper(metric))
  )
}

# Display all plots
lapply(plot_list, print)

# =============================================================================
# Notes on Usage
# =============================================================================

cat("
Usage Notes:
1. The function automatically parses config_desc strings using robust patterns
2. Use 'radar' for visualization of relative effects, 'bar' for ranking
3. Set normalize=TRUE to compare effects across different metrics
4. Use group_by for comparing effects across different datasets/variables
5. The function handles missing data and edge cases gracefully
6. Custom color schemes available: 'default', 'viridis', 'plasma'
")