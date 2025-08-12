#!/usr/bin/env Rscript

## =============================================================================
## Test Network Bundling Visualization Function
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)

# Source the network bundling function
source("R/visuals-plot_component_network_bundling.R")

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
  
  cat("Testing network bundling function...\n")
  
  # Test the network bundling function
  network_plot <- safely_execute(
    expr = {
      plot_component_network_bundling(
        results_data = results_data,
        metric = "rrmse",
        performance_threshold = 0.75,  # Top 25%
        min_co_occurrence = 3,
        covariate_handling = "simplified",
        component_sectors = TRUE,
        show_significance = FALSE,  # Skip for initial test
        title = "Multi-Way Component Interactions - MAOM (RRMSE)"
      )
    },
    default_value = NULL,
    error_message = "Network bundling function test failed"
  )
  
  if (!is.null(network_plot$result)) {
    cat("✓ Network bundling function works!\n")
    
    # Save the plot
    ggsave("scratch/component_network_bundling_MAOM.png", 
           network_plot$result, 
           width = 12, height = 10, dpi = 300, bg = "white")
    
    cat("✓ Network plot saved to scratch/component_network_bundling_MAOM.png\n")
    
    # Test with different parameters
    cat("\nTesting with different parameters...\n")
    
    # Test with higher performance threshold
    high_perf_plot <- safely_execute(
      expr = {
        plot_component_network_bundling(
          results_data = results_data,
          metric = "rrmse", 
          performance_threshold = 0.9,  # Top 10%
          min_co_occurrence = 2,
          covariate_handling = "simplified",
          bundling_strength = 0.6,
          node_size_metric = "centrality",
          title = "Elite Component Interactions - MAOM (Top 10%)"
        )
      },
      default_value = NULL,
      error_message = "High performance network test failed"
    )
    
    if (!is.null(high_perf_plot$result)) {
      ggsave("scratch/component_network_elite_MAOM.png", 
             high_perf_plot$result, 
             width = 12, height = 10, dpi = 300, bg = "white")
      cat("✓ Elite network plot saved\n")
    }
    
    # Test with aggregate covariate handling
    agg_plot <- safely_execute(
      expr = {
        plot_component_network_bundling(
          results_data = results_data,
          metric = "rrmse",
          performance_threshold = 0.75,
          covariate_handling = "aggregate",
          node_size_metric = "frequency",
          title = "Component Network - Aggregate Covariates"
        )
      },
      default_value = NULL,
      error_message = "Aggregate covariate test failed"
    )
    
    if (!is.null(agg_plot$result)) {
      ggsave("scratch/component_network_aggregate_MAOM.png", 
             agg_plot$result, 
             width = 12, height = 10, dpi = 300, bg = "white")
      cat("✓ Aggregate covariate plot saved\n")
    }
    
  } else {
    cat("✗ Network bundling function has issues\n")
    print(network_plot$error)
  }
  
} else {
  cat("ERROR: Data file not found\n")
}

cat("\nNetwork bundling test completed!\n")