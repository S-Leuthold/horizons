# Test enhanced component effects with your actual data
# 
# This script shows how to use the enhanced component effects function
# with your pom_results$full_summary data structure

library(horizons)
library(dplyr)

# Replace this with your actual data call:
# pom_data <- pom_results$full_summary %>% filter(status == "success")

# For testing, create sample data that matches your structure
pom_data <- data.frame(
  row = 1:15,
  config_desc = c(
    "svm_rbf_Sqrt_SNVD2_SHAP_Clay",
    "mlp_nn_NoTrans_SNV_PCA_Clay+pH", 
    "elastic_net_NoTrans_MSCD1_Corr_Clay+MAP",
    "mlp_nn_NoTrans_D1_Corr_CEC+MAP",
    "random_forest_NoTrans_SG_Corr_AI+Clay",
    "plsr_NoTrans_SG_SHAP_MAP",
    "mars_NoTrans_SNVD2_PCA_pH",
    "mars_Sqrt_SNVD1_PCA_CEC+Clay+MAP",
    "svm_rbf_NoTrans_SG_PCA_Clay+MAP",
    "mlp_nn_NoTrans_Raw_SHAP_CEC+Clay+pH",
    "cubist_Log_SNV_PCA_Clay",
    "xgboost_NoTrans_Raw_Boruta_",
    "random_forest_Sqrt_MSC_Corr_GDD+MAP",
    "plsr_Log_MSCD1_PCA_Clay+pH+CEC", 
    "elastic_net_NoTrans_D2_SHAP_GDD"
  ),
  wflow_id = paste0("wflow_", 1:15),
  rsq = runif(15, 0.6, 0.9),
  rmse = runif(15, 1.5, 4.0),
  rrmse = runif(15, 40, 100),
  saved_path = NA,
  error_log_path = NA,
  error_message = NA,
  status = "success",
  stringsAsFactors = FALSE
)

cat("=== Creating Component Effects Plots ===\n")

# Test 1: Basic radar plot 
cat("\n1. Creating basic radar plot...\n")
radar_plot <- plot_component_effects_enhanced(
  results_data = pom_data,
  metric = "rrmse",
  plot_type = "radar",
  include_ci = FALSE,
  effect_size_labels = FALSE,
  title = "Component Effects on RRMSE (Radar)"
)

if (!is.null(radar_plot)) {
  cat("✓ Radar plot created successfully\n")
  # Print the plot (will display in RStudio)
  print(radar_plot)
} else {
  cat("✗ Radar plot failed\n")
}

# Test 2: Bar plot with effect size labels
cat("\n2. Creating bar plot with effect sizes...\n")
bar_plot <- plot_component_effects_enhanced(
  results_data = pom_data,
  metric = "rrmse",
  plot_type = "bar", 
  include_ci = FALSE,
  effect_size_labels = TRUE,
  title = "Component Effects on RRMSE (Bar Chart)"
)

if (!is.null(bar_plot)) {
  cat("✓ Bar plot created successfully\n")
  print(bar_plot)
} else {
  cat("✗ Bar plot failed\n")
}

# Test 3: Basic radar plot using original function for comparison
cat("\n3. Creating original radar plot for comparison...\n")
original_plot <- plot_models_radar(
  results_data = pom_data,
  metric = "rrmse",
  plot_type = "radar",
  title = "Component Effects (Original Function)"
)

if (!is.null(original_plot)) {
  cat("✓ Original radar plot created successfully\n")
  print(original_plot)
} else {
  cat("✗ Original radar plot failed\n")
}

cat("\n=== Usage Tips ===\n")
cat("• The function now works with fully factorial designs (each config is unique)\n")
cat("• Uses ANOVA-style marginal effects when exact matching isn't possible\n")
cat("• Higher values = more important for predicting performance\n")
cat("• Try different metrics: 'rrmse', 'rsq', 'rmse'\n")
cat("• Set include_ci = TRUE for bootstrap confidence intervals (slower)\n")

cat("\n=== To use with your data ===\n")
cat("Replace the sample data creation above with:\n")
cat("pom_data <- pom_results$full_summary %>% filter(status == 'success')\n")
cat("\nThen run the plotting commands as shown above.\n")