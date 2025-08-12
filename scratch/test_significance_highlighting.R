#!/usr/bin/env Rscript

# Test significance highlighting in chord diagrams

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

cat("Loading MAOM data for significance highlighting test...\n")
batch_data <- qs::qread(data_path)

maom_batch_summary <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

# Test chord diagram with significance highlighting
cat("Testing chord diagram with significance highlighting...\n")

# Create chord diagram with significance testing enabled
png("scratch/chord_diagram_significance_fixed.png", width = 12, height = 10, units = "in", res = 300)

result <- plot_component_chord_diagram(
  results_data = maom_batch_summary,
  metric = "rrmse",
  performance_threshold = 0.85,  # Top 15% for more selective filtering
  top_interactions = 13,         # Fewer interactions for clarity
  show_significance = TRUE,      # Enable significance testing
  chord_transparency = 0.7,
  title = "Statistically Significant Component Interactions"
)

dev.off()

cat("Chord diagram saved to scratch/chord_diagram_significance_fixed.png\n")
cat("Check the plot to see if significant interactions are now highlighted\n")