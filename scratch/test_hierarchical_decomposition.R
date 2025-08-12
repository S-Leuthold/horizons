#!/usr/bin/env Rscript

# Test hierarchical interaction decomposition analysis

# Load required libraries
library(qs)
library(dplyr)
library(ggplot2)

# Source required functions  
source("R/analysis-hierarchical_interaction_decomposition.R")
source("R/visuals-plot_component_network_bundling.R")  # For parsing functions
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load the MAOM data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

if (!file.exists(data_path)) {
  cat("ERROR: Data file not found at expected path\n")
  quit(status = 1)
}

cat("Loading MAOM data for hierarchical decomposition analysis...\n")
batch_data <- qs::qread(data_path)

maom_batch_summary <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse) %>%
  # Remove any extreme outliers that might skew the analysis
  filter(rrmse < quantile(rrmse, 0.95, na.rm = TRUE))

cat("Dataset has", nrow(maom_batch_summary), "successful configurations\n")

# Run hierarchical decomposition analysis
cat("\nRunning hierarchical interaction decomposition...\n")

decomp_results <- hierarchical_interaction_decomposition(
  results_data = maom_batch_summary,
  metric = "rrmse",
  covariate_handling = "simplified",
  max_interaction_level = 4  # Go up to 4-way interactions
)

if (!is.null(decomp_results)) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("HIERARCHICAL INTERACTION DECOMPOSITION RESULTS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Print variance explained table
  cat("\nVariance Explained by Interaction Level:\n")
  variance_table <- decomp_results$variance_explained
  print(variance_table)
  
  # Print incremental gains
  cat("\nIncremental Variance Gains:\n")
  for (i in 1:nrow(variance_table)) {
    level <- variance_table$level[i]
    incremental <- round(variance_table$incremental_percent[i], 2)
    total <- round(variance_table$percent_variance[i], 2)
    
    cat(sprintf("  %s: +%s%% (Total: %s%%)\n", level, incremental, total))
  }
  
  # Identify most important interaction level
  max_incremental <- variance_table[which.max(variance_table$incremental_percent), ]
  cat("\nLargest incremental gain from:", max_incremental$level, 
      sprintf("(+%.1f%%)\n", max_incremental$incremental_percent))
  
  # Create visualization
  cat("\nCreating variance decomposition plot...\n")
  
  p1 <- plot_interaction_decomposition(decomp_results, plot_type = "incremental")
  
  ggsave("scratch/interaction_decomposition_incremental.png", p1, 
         width = 10, height = 6, dpi = 300, bg = "white")
  
  p2 <- plot_interaction_decomposition(decomp_results, plot_type = "cumulative")
  
  ggsave("scratch/interaction_decomposition_cumulative.png", p2,
         width = 10, height = 6, dpi = 300, bg = "white")
  
  cat("Plots saved to:\n")
  cat("  - scratch/interaction_decomposition_incremental.png\n")
  cat("  - scratch/interaction_decomposition_cumulative.png\n")
  
  # Print interpretation guidance
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat("INTERPRETATION GUIDANCE\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  main_effect_var <- variance_table[variance_table$level == "main_effects", "incremental_percent"]
  pairwise_var <- variance_table[variance_table$level == "pairwise", "incremental_percent"]
  
  if (main_effect_var > 50) {
    cat("\n✓ MAIN EFFECTS DOMINATE (", round(main_effect_var, 1), "%)\n")
    cat("  → Focus on selecting the best individual components\n")
    cat("  → Simple bar charts or tables may be most effective\n")
  }
  
  if (pairwise_var > 15) {
    cat("\n✓ PAIRWISE INTERACTIONS IMPORTANT (", round(pairwise_var, 1), "%)\n") 
    cat("  → Two-way interaction matrices/heatmaps recommended\n")
    cat("  → Look for synergistic component pairs\n")
  }
  
  if (nrow(variance_table) >= 4) {
    threeway_var <- variance_table[variance_table$level == "threeway", "incremental_percent"]
    if (threeway_var > 5) {
      cat("\n✓ THREE-WAY INTERACTIONS PRESENT (", round(threeway_var, 1), "%)\n")
      cat("  → Consider network visualizations\n")  
      cat("  → Complex interaction patterns exist\n")
    }
  }
  
} else {
  cat("ERROR: Hierarchical decomposition analysis failed\n")
}

cat("\nAnalysis complete!\n")