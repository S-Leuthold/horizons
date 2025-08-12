#!/usr/bin/env Rscript

## =============================================================================
## Test Progress Bar Network Function
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)

# Source the updated function
source("R/visuals-plot_component_network_bundling.R")
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

if (file.exists(data_path)) {
  
  cat("Loading MAOM data for progress bar test...\n")
  batch_data <- qs::qread(data_path)
  
  results_data <- batch_data %>%
    filter(status == "success") %>%
    select(config_desc, rsq, rmse, rrmse)
  
  cat("Testing network function with progress bar...\n")
  
  # Test with progress bar
  network_plot <- plot_component_network_bundling(
    results_data = results_data,
    metric = "rrmse",
    performance_threshold = 0.75,
    covariate_handling = "simplified",
    show_significance = FALSE,  # Test without significance first
    title = "Component Network with Progress Bar"
  )
  
  if (!is.null(network_plot)) {
    cat("✓ Progress bar network function works!\n")
    
    # Save the plot
    ggsave("scratch/network_with_progress_bar.png", 
           network_plot, 
           width = 12, height = 10, dpi = 300, bg = "white")
    
    cat("✓ Plot saved to scratch/network_with_progress_bar.png\n")
    
  } else {
    cat("✗ Network function still has issues\n")
  }
  
} else {
  cat("ERROR: Data file not found\n")
}

cat("\nProgress bar test completed!\n")