#!/usr/bin/env Rscript

## =============================================================================
## Debug Network Function Issues
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(tidygraph)

# Load utility functions
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"
batch_data <- qs::qread(data_path)

results_data <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

cat("=== DEBUGGING NETWORK FUNCTION ===\n")
cat("Total configurations:", nrow(results_data), "\n")

# Step 1: Filter to top performers
metric <- "rrmse"
performance_threshold <- 0.75

# Remove missing values
filtered_data <- results_data %>%
  filter(!is.na(.data[[metric]]))

# For error metrics, lower is better - take bottom percentile
cutoff <- quantile(filtered_data[[metric]], probs = 1 - performance_threshold, na.rm = TRUE)
filtered_data <- filtered_data %>%
  filter(.data[[metric]] <= cutoff)

cat("Top performers:", nrow(filtered_data), "\n")
cat("RRMSE cutoff:", cutoff, "\n")

# Step 2: Parse components (simplified version)
parse_components_debug <- function(data) {
  
  result <- data %>%
    mutate(
      ModelType = case_when(
        str_detect(config_desc, "^random_forest") ~ "Random Forest",
        str_detect(config_desc, "^cubist") ~ "Cubist", 
        str_detect(config_desc, "^xgboost") ~ "XGBoost",
        str_detect(config_desc, "^mars") ~ "MARS",
        str_detect(config_desc, "^plsr") ~ "PLSR",
        TRUE ~ "Other"
      ),
      Transformation = case_when(
        str_detect(config_desc, "_NoTrans_") ~ "None",
        str_detect(config_desc, "_Log_") ~ "Log",
        TRUE ~ "None"
      ),
      Preprocessing = case_when(
        str_detect(config_desc, "_SNVD1_") ~ "SNV+D1",
        str_detect(config_desc, "_SNV_") ~ "SNV",
        str_detect(config_desc, "_Raw_") ~ "Raw",
        TRUE ~ "Raw"
      ),
      Feature_Selection = case_when(
        str_detect(config_desc, "_SHAP_") ~ "SHAP",
        str_detect(config_desc, "_PCA_") ~ "PCA",
        TRUE ~ "PCA"
      ),
      Covariates = case_when(
        str_count(config_desc, "_") <= 3 ~ "Spectral Only",
        TRUE ~ "With Covariates"
      )
    )
  
  return(result)
}

parsed_data <- parse_components_debug(filtered_data)

# Check component distributions
cat("\n=== COMPONENT DISTRIBUTIONS ===\n")
component_columns <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")
for (comp in component_columns) {
  cat(comp, ":\n")
  print(table(parsed_data[[comp]]))
  cat("\n")
}

# Step 3: Build network manually
cat("=== BUILDING NETWORK ===\n")

# Create long format with all component values per configuration
component_values <- parsed_data %>%
  tidyr::pivot_longer(
    cols = all_of(component_columns),
    names_to = "component_type",
    values_to = "component_value"
  ) %>%
  select(config_desc, component_type, component_value, all_of(metric))

cat("Component values format:\n")
print(head(component_values))

# Get unique component values for nodes
nodes <- component_values %>%
  distinct(component_value, component_type) %>%
  arrange(component_type, component_value) %>%
  mutate(node_id = row_number())

cat("\nNodes:\n")
print(nodes)

# Create component co-occurrence matrix
config_components <- parsed_data %>%
  select(config_desc, all_of(component_columns), all_of(metric)) %>%
  tidyr::pivot_longer(
    cols = all_of(component_columns),
    names_to = "component_type", 
    values_to = "component_value"
  ) %>%
  group_by(config_desc) %>%
  summarise(
    components = list(component_value),
    performance = first(.data[[metric]]),
    .groups = "drop"
  )

cat("\nConfig components format:\n")
print(head(config_components))

# Generate all pairwise co-occurrences (simplified)
edges_list <- list()
min_co_occurrence <- 3

for (i in 1:nrow(config_components)) {
  config_comps <- config_components$components[[i]]
  config_perf <- config_components$performance[i]
  
  if (length(config_comps) >= 2) {
    pairs <- combn(config_comps, 2, simplify = FALSE)
    
    for (pair in pairs) {
      edges_list[[length(edges_list) + 1]] <- data.frame(
        from = pair[1],
        to = pair[2],
        performance = config_perf,
        config_desc = config_components$config_desc[i]
      )
    }
  }
}

# Combine edges
if (length(edges_list) > 0) {
  edges <- bind_rows(edges_list)
  cat("\nRaw edges:", nrow(edges), "\n")
  
  # Aggregate edge statistics
  edge_summary <- edges %>%
    group_by(from, to) %>%
    summarise(
      co_occurrence_count = n(),
      avg_performance = mean(performance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(co_occurrence_count >= min_co_occurrence) %>%
    arrange(desc(co_occurrence_count))
  
  cat("Aggregated edges:", nrow(edge_summary), "\n")
  print(head(edge_summary))
  
  if (nrow(edge_summary) > 0) {
    # Create igraph object
    graph <- edge_summary %>%
      select(from, to, weight = co_occurrence_count) %>%
      graph_from_data_frame(vertices = nodes$component_value, directed = FALSE)
    
    cat("\nGraph created successfully!\n")
    cat("Vertices:", vcount(graph), "\n")
    cat("Edges:", ecount(graph), "\n")
    
    # Create tidygraph and test plotting
    tidy_graph <- as_tbl_graph(graph)
    
    cat("\nTesting basic network plot...\n")
    
    # Simple test plot
    p <- tidy_graph %>%
      ggraph(layout = "circle") +
      geom_edge_link(aes(width = weight), alpha = 0.6) +
      geom_node_point(size = 5, color = "steelblue") +
      geom_node_text(aes(label = name), size = 3, repel = TRUE) +
      scale_edge_width_continuous(range = c(0.5, 2)) +
      theme_void() +
      labs(title = "Component Network Test")
    
    ggsave("scratch/network_debug_test.png", p, width = 10, height = 8, dpi = 300)
    cat("✓ Debug network plot saved!\n")
    
  } else {
    cat("✗ No edges meet minimum co-occurrence threshold\n")
  }
} else {
  cat("✗ No edges generated\n")
}