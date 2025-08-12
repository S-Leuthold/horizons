#!/usr/bin/env Rscript

## =============================================================================
## Debug Component Interaction Plot Issues
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Load data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"
batch_data <- qs::qread(data_path)

results_data <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

cat("=== DEBUGGING INTERACTION MATRIX ===\n")
cat("Total configurations:", nrow(results_data), "\n")

# Parse components manually to debug
parse_components_debug <- function(data) {
  result <- data %>%
    mutate(
      ModelType = case_when(
        str_detect(config_desc, "^random_forest") ~ "Random Forest",
        str_detect(config_desc, "^cubist") ~ "Cubist",
        str_detect(config_desc, "^xgboost") ~ "XGBoost",
        str_detect(config_desc, "^mars") ~ "MARS",
        str_detect(config_desc, "^plsr") ~ "PLSR",
        str_detect(config_desc, "^svm_rbf") ~ "SVM",
        str_detect(config_desc, "^elastic_net") ~ "Elastic Net",
        str_detect(config_desc, "^mlp_nn") ~ "Neural Net",
        TRUE ~ "Other"
      ),
      Transformation = case_when(
        str_detect(config_desc, "_NoTrans_") ~ "None",
        str_detect(config_desc, "_Log_") ~ "Log",
        str_detect(config_desc, "_Sqrt_") ~ "Square Root",
        TRUE ~ "None"
      ),
      Preprocessing = case_when(
        str_detect(config_desc, "_SNVD2_") ~ "SNV+D2",
        str_detect(config_desc, "_SNVD1_") ~ "SNV+D1",
        str_detect(config_desc, "_MSCD1_") ~ "MSC+D1",
        str_detect(config_desc, "_SNV_") ~ "SNV",
        str_detect(config_desc, "_MSC_") ~ "MSC",
        str_detect(config_desc, "_SG_") ~ "SavGol",
        str_detect(config_desc, "_D2_") ~ "D2",
        str_detect(config_desc, "_D1_") ~ "D1",
        str_detect(config_desc, "_Raw_") ~ "Raw",
        TRUE ~ "Raw"
      ),
      Feature_Selection = case_when(
        str_detect(config_desc, "_SHAP_") ~ "SHAP",
        str_detect(config_desc, "_PCA_") ~ "PCA",
        str_detect(config_desc, "_Corr_") ~ "Correlation",
        str_detect(config_desc, "_Boruta_") ~ "Boruta",
        TRUE ~ "PCA"
      ),
      # Simplified covariate handling
      Covariates = ifelse(
        str_detect(config_desc, "[A-Z]+\\+") | str_detect(config_desc, "[A-Z]+$"),
        "Present",
        "None"
      )
    )
  
  return(result)
}

parsed_data <- parse_components_debug(results_data)

# Check component distributions
cat("\n=== COMPONENT DISTRIBUTIONS ===\n")
for (comp in c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")) {
  cat(comp, ":\n")
  print(table(parsed_data[[comp]]))
  cat("\n")
}

# Create simplified interaction computation
compute_simple_interactions <- function(data, metric = "rrmse") {
  
  components <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")
  overall_mean <- mean(data[[metric]], na.rm = TRUE)
  
  # Generate all pairs
  pairs <- expand_grid(
    component_a = components,
    component_b = components
  ) %>%
    filter(component_a != component_b)
  
  # Compute interaction for each pair
  interactions <- map_dfr(1:nrow(pairs), function(i) {
    comp_a <- pairs$component_a[i]
    comp_b <- pairs$component_b[i]
    
    # Get all A×B combinations that exist in data
    combo_data <- data %>%
      group_by(.data[[comp_a]], .data[[comp_b]]) %>%
      summarise(
        observed_mean = mean(.data[[metric]], na.rm = TRUE),
        n_obs = n(),
        .groups = "drop"
      ) %>%
      filter(n_obs >= 2)
    
    if (nrow(combo_data) > 0) {
      # Simple interaction: variance within vs between
      interaction_effect <- var(combo_data$observed_mean, na.rm = TRUE)
      
      tibble(
        component_a = comp_a,
        component_b = comp_b,
        interaction_effect = interaction_effect,
        n_combinations = nrow(combo_data)
      )
    } else {
      tibble(
        component_a = comp_a,
        component_b = comp_b,
        interaction_effect = 0,
        n_combinations = 0
      )
    }
  })
  
  return(interactions)
}

# Compute interactions
cat("=== COMPUTING INTERACTIONS ===\n")
interactions <- compute_simple_interactions(parsed_data, "rrmse")

print(interactions)

# Create better heatmap
create_fixed_heatmap <- function(interaction_data, metric = "rrmse") {
  
  # Get all unique components
  all_components <- unique(c(interaction_data$component_a, interaction_data$component_b))
  
  # Create complete grid
  complete_grid <- expand_grid(
    component_a = all_components,
    component_b = all_components
  ) %>%
    left_join(interaction_data, by = c("component_a", "component_b")) %>%
    mutate(
      interaction_effect = ifelse(is.na(interaction_effect), 0, interaction_effect),
      # Set diagonal to 0 (component with itself)
      interaction_effect = ifelse(component_a == component_b, 0, interaction_effect)
    )
  
  # Clean names
  complete_grid <- complete_grid %>%
    mutate(
      component_a_clean = str_replace_all(component_a, "_", "\n"),
      component_b_clean = str_replace_all(component_b, "_", "\n")
    )
  
  # Create plot
  max_effect <- max(abs(complete_grid$interaction_effect), na.rm = TRUE)
  
  p <- ggplot(complete_grid,
              aes(x = component_a_clean, y = component_b_clean, fill = interaction_effect)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#67001F",
      mid = "white", 
      high = "#053061",
      midpoint = 0,
      limits = c(-max_effect, max_effect),
      name = "Interaction\nEffect"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      axis.title = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      legend.position = "right"
    ) +
    labs(
      title = paste("Component Interaction Effects", toupper(metric)),
      subtitle = "Blue = Synergistic | Red = Antagonistic | White = Independent",
      x = "Pipeline Component A",
      y = "Pipeline Component B"
    )
  
  return(p)
}

# Create and save fixed plot
cat("\n=== CREATING FIXED HEATMAP ===\n")
fixed_plot <- create_fixed_heatmap(interactions, "rrmse")

ggsave("scratch/component_interactions_fixed.png", 
       fixed_plot, 
       width = 10, height = 8, dpi = 300)

cat("Fixed plot saved to scratch/component_interactions_fixed.png\n")

# Show data summary
cat("\n=== INTERACTION SUMMARY ===\n")
interactions %>%
  arrange(desc(abs(interaction_effect))) %>%
  head(10) %>%
  print()