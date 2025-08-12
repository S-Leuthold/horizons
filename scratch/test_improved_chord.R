#!/usr/bin/env Rscript

# Test improved chord diagram (no covariates, better colors, no significance)

# Load required libraries
library(qs)
library(dplyr)

# Source required functions  
source("R/visuals-plot_component_chord_diagram.R")
source("R/visuals-plot_component_network_bundling.R")  # For helper functions
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load the MAOM data from the correct path
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

if (!file.exists(data_path)) {
  cat("ERROR: Data file not found at expected path\n")
  quit(status = 1)
}

cat("Loading MAOM data for improved chord diagram test...\n")
batch_data <- qs::qread(data_path)

maom_batch_summary <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

# Test improved chord diagram - no covariates, better colors, no significance
cat("Creating improved chord diagram (no covariates, cleaner colors)...\n")

png("scratch/chord_diagram_improved.png", width = 12, height = 10, units = "in", res = 300)

result <- plot_component_chord_diagram(
  results_data = maom_batch_summary,
  metric = "rrmse",
  performance_threshold = 0.75,  # Top 25%
  top_interactions = 12,         # Good balance of detail vs clarity
  show_significance = FALSE,     # Skip slow significance testing
  chord_transparency = 0.6,      # Slightly more opaque
  title = "Core Component Interaction Patterns - MAOM"
)

dev.off()

cat("Improved chord diagram saved to scratch/chord_diagram_improved.png\n")
cat("Key improvements:\n")
cat("  - Covariates removed for clarity\n")
cat("  - Distinct color scheme by component type\n") 
cat("  - No slow significance testing\n")
cat("  - Focus on core modeling components only\n")