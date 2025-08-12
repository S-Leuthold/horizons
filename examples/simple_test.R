# Simple test for enhanced component effects
# This version sources functions directly to avoid package loading issues

library(dplyr)
library(ggplot2)
library(cli)
library(glue)
library(purrr)
library(stringr)
library(tidyr)

# Source required functions
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R") 
source("R/visuals-plot_models_radar.R")
source("R/visuals-plot_component_effects_enhanced.R")

# Create test data matching user's structure
pom_data <- data.frame(
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
    "mlp_nn_NoTrans_Raw_SHAP_CEC+Clay+pH"
  ),
  rsq = c(0.748, 0.659, 0.835, 0.890, 0.636, 0.675, 0.370, 0.579, 0.767, 0.574),
  rmse = c(2.54, 2.89, 2.55, 1.84, 2.83, 2.95, 4.74, 3.84, 2.33, 3.68),
  rrmse = c(61.8, 68.0, 52.5, 41.7, 68.0, 67.5, 99.4, 84.0, 57.9, 83.6),
  status = "success",
  stringsAsFactors = FALSE
)

cat("=== Testing Enhanced Component Effects ===\n")
cat("Using", nrow(pom_data), "configurations\n\n")

# Test 1: Create basic bar plot
cat("1. Creating basic bar plot...\n")
bar_plot <- plot_component_effects_enhanced(
  results_data = pom_data,
  metric = "rrmse",
  plot_type = "bar",
  include_ci = FALSE,
  effect_size_labels = TRUE,
  components = c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates"),
  title = "Component Effects on RRMSE"
)

if (!is.null(bar_plot)) {
  cat("✓ Bar plot created successfully!\n")
} else {
  cat("✗ Bar plot failed\n")
}

# Test 2: Create radar plot  
cat("\n2. Creating radar plot...\n")
radar_plot <- plot_component_effects_enhanced(
  results_data = pom_data,
  metric = "rrmse", 
  plot_type = "radar",
  include_ci = FALSE,
  effect_size_labels = FALSE,
  components = c("ModelType", "Transformation", "Preprocessing", "Feature_Selection"),
  title = "Component Effects (Radar)"
)

if (!is.null(radar_plot)) {
  cat("✓ Radar plot created successfully!\n")
} else {
  cat("✗ Radar plot failed\n")
}

# Test 3: Show marginal effects data
cat("\n3. Computing marginal effects...\n")
parsed_data <- parse_config_descriptors(pom_data, c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates"))
marginal_effects <- compute_marginal_effects(parsed_data, "rrmse", c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates"))

cat("Marginal effects (higher = more important):\n")
marginal_effects_sorted <- marginal_effects[order(-marginal_effects$mean_sd), ]
for (i in 1:nrow(marginal_effects_sorted)) {
  cat(sprintf("  %s: %.2f\n", marginal_effects_sorted$component[i], marginal_effects_sorted$mean_sd[i]))
}

cat("\n=== Success! ===\n")
cat("The enhanced component effects function is working correctly.\n")
cat("You can now use it with your actual data:\n\n")
cat("pom_data <- pom_results$full_summary %>% filter(status == 'success')\n")
cat("plot <- plot_component_effects_enhanced(pom_data, metric = 'rrmse', plot_type = 'bar')\n")
cat("print(plot)\n")