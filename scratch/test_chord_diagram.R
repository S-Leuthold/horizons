#!/usr/bin/env Rscript

## =============================================================================
## Test Chord Diagram Implementation
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)
library(circlize)

# Source the functions
source("R/visuals-plot_component_chord_diagram.R")
source("R/visuals-plot_component_network_bundling.R")  # For helper functions
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

if (file.exists(data_path)) {
  
  cat("Loading MAOM data for chord diagram test...\n")
  batch_data <- qs::qread(data_path)
  
  results_data <- batch_data %>%
    filter(status == "success") %>%
    select(config_desc, rsq, rmse, rrmse)
  
  cat("Testing chord diagram function...\n")
  
  # Test basic chord diagram
  cat("Creating basic chord diagram...\n")
  
  # Use PNG device for chord diagram (they don't work well with ggsave)
  png("scratch/chord_diagram_basic.png", width = 12, height = 10, 
      units = "in", res = 300, bg = "white")
  
  chord_result <- plot_component_chord_diagram(
    results_data = results_data,
    metric = "rrmse",
    performance_threshold = 0.75,
    top_interactions = 15,
    covariate_handling = "simplified",
    show_significance = FALSE,
    title = "Component Interaction Chord Diagram - MAOM"
  )
  
  dev.off()
  
  if (!is.null(chord_result)) {
    cat("✓ Basic chord diagram created!\n")
  } else {
    cat("✗ Basic chord diagram failed\n")
  }
  
  # Test with fewer interactions for clarity
  cat("Creating focused chord diagram (top 10 interactions)...\n")
  
  png("scratch/chord_diagram_focused.png", width = 12, height = 10,
      units = "in", res = 300, bg = "white")
  
  focused_result <- plot_component_chord_diagram(
    results_data = results_data,
    metric = "rrmse",
    performance_threshold = 0.8,  # Top 20%
    top_interactions = 10,        # Even fewer for clarity
    min_co_occurrence = 8,        # Higher threshold
    covariate_handling = "simplified",
    chord_transparency = 0.6,
    title = "Top Component Synergies - MAOM (Elite Configurations)"
  )
  
  dev.off()
  
  if (!is.null(focused_result)) {
    cat("✓ Focused chord diagram created!\n")
  } else {
    cat("✗ Focused chord diagram failed\n")
  }
  
  # Test with significance testing
  cat("Creating chord diagram with significance testing...\n")
  
  png("scratch/chord_diagram_significance.png", width = 12, height = 10,
      units = "in", res = 300, bg = "white")
  
  significance_result <- plot_component_chord_diagram(
    results_data = results_data,
    metric = "rrmse",
    performance_threshold = 0.75,
    top_interactions = 12,
    show_significance = TRUE,
    title = "Statistically Significant Component Interactions"
  )
  
  dev.off()
  
  if (!is.null(significance_result)) {
    cat("✓ Significance chord diagram created!\n")
  } else {
    cat("✗ Significance chord diagram failed\n")
  }
  
} else {
  cat("ERROR: Data file not found\n")
}

cat("\nChord diagram test completed!\n")