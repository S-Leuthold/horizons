#!/usr/bin/env Rscript

## =============================================================================
## Test Component Value Interactions Function
## Following exact horizons workflow and style
## =============================================================================

# Load required packages
library(qs)
library(dplyr)
library(ggplot2)

# Source the new function (temporary - normally would be loaded via package)
source("R/visuals-plot_component_value_interactions.R")

# Load existing utility functions we need
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

## ---------------------------------------------------------------------------
## Step 1: Load real MAOM data
## ---------------------------------------------------------------------------

data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

if (file.exists(data_path)) {
  
  cat("Loading MAOM batch summary data...\n")
  batch_data <- qs::qread(data_path)
  
  # Filter successful runs only
  results_data <- batch_data %>%
    filter(status == "success") %>%
    select(config_desc, rsq, rmse, rrmse)
  
  cat("Working with", nrow(results_data), "successful configurations\n")
  
  ## -------------------------------------------------------------------------
  ## Step 2: Test basic functionality
  ## -------------------------------------------------------------------------
  
  cat("\n=== TESTING BASIC INTERACTION PLOT ===\n")
  
  # Test with simplified covariate handling (fastest)
  basic_plot <- safely_execute(
    expr = {
      plot_component_value_interactions(
        results_data = results_data,
        metric = "rrmse",
        covariate_handling = "simplified",
        cluster_components = TRUE,
        significance_test = FALSE,
        title = "Component Interactions for MAOM (RRMSE)"
      )
    },
    default_value = NULL,
    error_message = "Basic plot failed"
  )
  
  if (!is.null(basic_plot$result)) {
    cat("✓ Basic interaction plot created successfully\n")
    
    # Save plot
    ggsave("scratch/component_value_interactions_basic.png", 
           basic_plot$result, 
           width = 10, height = 8, dpi = 300)
    cat("✓ Plot saved to scratch/component_value_interactions_basic.png\n")
    
  } else {
    cat("✗ Basic plot failed\n")
  }
  
  ## -------------------------------------------------------------------------
  ## Step 3: Test with different settings
  ## -------------------------------------------------------------------------
  
  cat("\n=== TESTING DIFFERENT CONFIGURATIONS ===\n")
  
  # Test with aggregate covariate handling
  aggregate_plot <- safely_execute(
    expr = {
      plot_component_value_interactions(
        results_data = results_data,
        metric = "rsq",
        covariate_handling = "aggregate",
        cluster_components = FALSE,
        color_palette = "viridis",
        title = "Component Interactions for MAOM (R-squared) - Aggregate Covariates"
      )
    },
    default_value = NULL,
    error_message = "Aggregate plot failed"
  )
  
  if (!is.null(aggregate_plot$result)) {
    cat("✓ Aggregate covariate plot created successfully\n")
    
    # Save plot
    ggsave("scratch/component_value_interactions_aggregate.png", 
           aggregate_plot$result, 
           width = 10, height = 8, dpi = 300)
    cat("✓ Plot saved to scratch/component_value_interactions_aggregate.png\n")
    
  } else {
    cat("✗ Aggregate plot failed\n")
  }
  
  ## -------------------------------------------------------------------------
  ## Step 4: Test subset of components
  ## -------------------------------------------------------------------------
  
  cat("\n=== TESTING SUBSET ANALYSIS ===\n")
  
  # Test with just 3 components (faster)
  subset_plot <- safely_execute(
    expr = {
      plot_component_value_interactions(
        results_data = results_data,
        metric = "rrmse",
        components = c("ModelType", "Preprocessing", "Feature_Selection"),
        covariate_handling = "aggregate",
        cluster_components = TRUE,
        title = "Core Component Interactions (3-way)"
      )
    },
    default_value = NULL,
    error_message = "Subset plot failed"
  )
  
  if (!is.null(subset_plot$result)) {
    cat("✓ Subset component plot created successfully\n")
    
    # Save plot
    ggsave("scratch/component_value_interactions_subset.png", 
           subset_plot$result, 
           width = 8, height = 6, dpi = 300)
    cat("✓ Plot saved to scratch/component_value_interactions_subset.png\n")
    
  } else {
    cat("✗ Subset plot failed\n")
  }
  
  ## -------------------------------------------------------------------------
  ## Step 5: Summary insights
  ## -------------------------------------------------------------------------
  
  if (!is.null(basic_plot$result)) {
    cat("\n=== INSIGHTS FROM INTERACTION ANALYSIS ===\n")
    cat("The component interaction plots reveal:\n")
    cat("• Synergistic interactions (blue) show components that work better together\n")
    cat("• Antagonistic interactions (red) show components that interfere\n") 
    cat("• White areas indicate independent/additive effects\n")
    cat("• Clustering groups components with similar interaction patterns\n")
  }
  
  cat("\nTest completed successfully!\n")
  
} else {
  cat("ERROR: Data file not found at:", data_path, "\n")
}