#!/usr/bin/env Rscript

## =============================================================================
## Debug Chord Diagram Step by Step
## =============================================================================

library(qs)
library(dplyr)
library(stringr)
library(circlize)

# Load utility functions 
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"
batch_data <- qs::qread(data_path)

results_data <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

cat("=== DEBUGGING CHORD DIAGRAM ===\n")
cat("Data loaded:", nrow(results_data), "configurations\n")

# Step 1: Filter to top performers (reuse existing function)
source("R/visuals-plot_component_network_bundling.R")

metric <- "rrmse"
performance_threshold <- 0.75

filtered_data <- filter_top_performers(results_data, metric, performance_threshold)
cat("Filtered data:", nrow(filtered_data), "top performers\n")

# Step 2: Parse components
parsed_data <- parse_components_for_network(filtered_data, "simplified")
cat("Components parsed\n")

# Step 3: Build network data
network_data <- build_component_network(parsed_data, metric, 5)
cat("Network built - Nodes:", nrow(network_data$nodes), "Edges:", nrow(network_data$edges), "\n")

# Step 4: Prepare chord data manually
cat("\n=== PREPARING CHORD DATA ===\n")

edges <- network_data$edges
nodes <- network_data$nodes

# Take top 10 interactions for simplicity
top_edges <- edges %>%
  arrange(desc(co_occurrence_count)) %>%
  slice_max(order_by = co_occurrence_count, n = 10)

cat("Top edges:\n")
print(top_edges)

# Get involved components
involved_components <- unique(c(top_edges$from, top_edges$to))
cat("\nInvolved components:", length(involved_components), "\n")
print(involved_components)

# Create simple chord matrix
component_names <- involved_components
n_components <- length(component_names)

chord_matrix <- matrix(0, nrow = n_components, ncol = n_components,
                      dimnames = list(component_names, component_names))

# Fill matrix
for (i in 1:nrow(top_edges)) {
  from_comp <- top_edges$from[i]
  to_comp <- top_edges$to[i]
  weight <- top_edges$co_occurrence_count[i]
  
  chord_matrix[from_comp, to_comp] <- weight
  chord_matrix[to_comp, from_comp] <- weight
}

cat("\nChord matrix dimensions:", dim(chord_matrix), "\n")
cat("Matrix summary:\n")
print(chord_matrix[1:min(5, nrow(chord_matrix)), 1:min(5, ncol(chord_matrix))])

# Step 5: Test basic circlize
cat("\n=== TESTING BASIC CIRCLIZE ===\n")

# Set up simple colors
component_colors <- rainbow(n_components)
names(component_colors) <- component_names

cat("Testing simple chord diagram...\n")

png("scratch/chord_debug_simple.png", width = 10, height = 10, 
    units = "in", res = 300, bg = "white")

tryCatch({
  circos.clear()
  
  # Use uniform gaps for all sectors
  circos.par(
    start.degree = 90,
    gap.degree = 4,  # Single value for all gaps
    track.margin = c(0.01, 0.01)
  )
  
  chordDiagram(
    chord_matrix,
    grid.col = component_colors,
    transparency = 0.6,
    directional = 0,
    annotationTrack = "grid",
    annotationTrackHeight = 0.05
  )
  
  title("Debug Chord Diagram - Simple Test", cex.main = 1.2)
  
  cat("✓ Basic chord diagram created successfully!\n")
  
}, error = function(e) {
  cat("✗ Error creating chord diagram:", e$message, "\n")
})

dev.off()

cat("\nDebug completed!\n")