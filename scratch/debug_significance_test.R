#!/usr/bin/env Rscript

# Debug significance testing to see why no interactions are significant

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

cat("Loading MAOM data for significance debugging...\n")
batch_data <- qs::qread(data_path)

maom_batch_summary <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

cat("Dataset has", nrow(maom_batch_summary), "successful configurations\n")

# Filter to top-performing configurations
filtered_data <- filter_top_performers(maom_batch_summary, "rrmse", 0.75)
cat("After filtering to top 25%:", nrow(filtered_data), "configurations\n")

# Parse components
parsed_data <- parse_components_for_network(filtered_data, "simplified")
cat("Parsed data dimensions:", nrow(parsed_data), "x", ncol(parsed_data), "\n")

# Build network without significance first
network_data <- build_component_network(parsed_data, "rrmse", 5)

cat("Network has", nrow(network_data$edges), "edges with co-occurrence >= 5\n")
cat("Top 5 strongest co-occurrences:\n")
print(network_data$edges %>% 
  arrange(desc(co_occurrence_count)) %>% 
  select(from, to, co_occurrence_count) %>%
  head(5))

# Now test significance manually with more permutations
cat("\nTesting significance with more permutations...\n")

# Test the top edge manually
top_edge <- network_data$edges %>% arrange(desc(co_occurrence_count)) %>% slice(1)
cat("Testing significance for:", top_edge$from, "->", top_edge$to, 
    "with observed count:", top_edge$co_occurrence_count, "\n")

# Run permutation test with more iterations
n_perms <- 500  # More permutations for better p-value resolution
null_counts <- numeric(n_perms)

for (i in 1:n_perms) {
  # Permute component assignments
  perm_data <- parsed_data
  component_columns <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")
  
  for (col in component_columns) {
    perm_data[[col]] <- sample(perm_data[[col]])
  }
  
  # Build network for permuted data
  perm_network <- build_component_network(perm_data, "rrmse", 1)  # Lower threshold for permutation
  
  # Find the same edge in permuted network
  matching_edge <- perm_network$edges %>%
    filter((from == top_edge$from & to == top_edge$to) |
           (from == top_edge$to & to == top_edge$from))
  
  if (nrow(matching_edge) > 0) {
    null_counts[i] <- matching_edge$co_occurrence_count[1]
  } else {
    null_counts[i] <- 0
  }
}

# Calculate p-value
obs_count <- top_edge$co_occurrence_count
p_value <- mean(null_counts >= obs_count)

cat("Permutation results:\n")
cat("  Observed co-occurrence:", obs_count, "\n")
cat("  Mean null co-occurrence:", round(mean(null_counts), 2), "\n")
cat("  Null distribution range:", min(null_counts), "-", max(null_counts), "\n")
cat("  P-value:", p_value, "\n")
cat("  Significant at 0.05?", p_value < 0.05, "\n")

# Test with different significance thresholds
cat("\nSignificance at different thresholds:\n")
for (alpha in c(0.10, 0.05, 0.01)) {
  cat("  α =", alpha, ":", p_value < alpha, "\n")
}