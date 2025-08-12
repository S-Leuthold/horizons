#!/usr/bin/env Rscript

## =============================================================================
## Test Fixed Component Value Interactions Function
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)

# Source the fixed function
source("R/visuals-plot_component_value_interactions.R")

# Load utility functions
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

if (file.exists(data_path)) {
  
  cat("Loading MAOM data...\n")
  batch_data <- qs::qread(data_path)
  
  results_data <- batch_data %>%
    filter(status == "success") %>%
    select(config_desc, rsq, rmse, rrmse)
  
  cat("Testing fixed interaction function...\n")
  
  # Test the fixed function
  fixed_plot <- safely_execute(
    expr = {
      plot_component_value_interactions(
        results_data = results_data,
        metric = "rrmse",
        covariate_handling = "aggregate",
        cluster_components = TRUE,
        significance_test = FALSE,
        title = "Component Interaction Strength - MAOM (RRMSE)"
      )
    },
    default_value = NULL,
    error_message = "Fixed function test failed"
  )
  
  if (!is.null(fixed_plot$result)) {
    cat("✓ Fixed function works!\n")
    
    # Save the plot
    ggsave("scratch/component_interactions_FIXED.png", 
           fixed_plot$result, 
           width = 10, height = 8, dpi = 300, bg = "white")
    
    cat("✓ Fixed plot saved to scratch/component_interactions_FIXED.png\n")
    
  } else {
    cat("✗ Fixed function still has issues\n")
    print(fixed_plot$error)
  }
  
} else {
  cat("ERROR: Data file not found\n")
}