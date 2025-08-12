#!/usr/bin/env Rscript

## =============================================================================
## Fixed Component Interaction Heatmap
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)

# Load data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"
batch_data <- qs::qread(data_path)

results_data <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

# Simple component parsing
parse_components_simple <- function(data) {
  data %>%
    mutate(
      ModelType = case_when(
        str_detect(config_desc, "^random_forest") ~ "Random\nForest",
        str_detect(config_desc, "^cubist") ~ "Cubist",
        str_detect(config_desc, "^xgboost") ~ "XGBoost",
        str_detect(config_desc, "^mars") ~ "MARS",
        str_detect(config_desc, "^plsr") ~ "PLSR",
        str_detect(config_desc, "^svm_rbf") ~ "SVM",
        TRUE ~ "Other"
      ),
      Transformation = case_when(
        str_detect(config_desc, "_NoTrans_") ~ "None",
        str_detect(config_desc, "_Log_") ~ "Log",
        str_detect(config_desc, "_Sqrt_") ~ "Square\nRoot",
        TRUE ~ "None"
      ),
      Preprocessing = case_when(
        str_detect(config_desc, "_SNVD1_") ~ "SNV+D1",
        str_detect(config_desc, "_SNV_") ~ "SNV",
        str_detect(config_desc, "_SG_") ~ "SavGol",
        str_detect(config_desc, "_D1_") ~ "D1",
        str_detect(config_desc, "_Raw_") ~ "Raw",
        TRUE ~ str_extract(config_desc, "(?<=_)[A-Z]+(?=_)")
      ),
      Feature_Selection = case_when(
        str_detect(config_desc, "_SHAP_") ~ "SHAP",
        str_detect(config_desc, "_PCA_") ~ "PCA", 
        str_detect(config_desc, "_Corr_") ~ "Correlation",
        TRUE ~ "PCA"
      ),
      Covariates = case_when(
        str_count(config_desc, "_") <= 3 ~ "None",
        TRUE ~ "Present"
      )
    )
}

parsed_data <- parse_components_simple(results_data)

# Create interaction matrix using correlation-based approach
create_interaction_matrix <- function(data, metric = "rrmse") {
  
  components <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")
  
  # Create all pairs including diagonal
  all_pairs <- expand_grid(
    component_a = components,
    component_b = components
  )
  
  # For each pair, compute a simple interaction metric
  interaction_effects <- map_dfr(1:nrow(all_pairs), function(i) {
    comp_a <- all_pairs$component_a[i]
    comp_b <- all_pairs$component_b[i]
    
    if (comp_a == comp_b) {
      # Diagonal: set to 0
      effect <- 0
    } else {
      # Compute variance of performance within each A×B combination
      combo_stats <- data %>%
        group_by(.data[[comp_a]], .data[[comp_b]]) %>%
        summarise(
          mean_perf = mean(.data[[metric]], na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) %>%
        filter(n >= 2)
      
      if (nrow(combo_stats) > 1) {
        # Interaction effect = variance of combination means
        effect <- var(combo_stats$mean_perf, na.rm = TRUE)
      } else {
        effect <- 0
      }
    }
    
    tibble(
      component_a = comp_a,
      component_b = comp_b,
      interaction_effect = effect
    )
  })
  
  return(interaction_effects)
}

# Compute interactions
cat("Computing interaction matrix...\n")
interactions <- create_interaction_matrix(parsed_data, "rrmse")

# Create publication-quality heatmap
create_publication_heatmap <- function(interaction_data, metric = "rrmse") {
  
  # Normalize interaction effects to [-1, 1] range for better visualization
  max_effect <- max(abs(interaction_data$interaction_effect), na.rm = TRUE)
  
  plot_data <- interaction_data %>%
    mutate(
      # Normalize to make effects more visible
      normalized_effect = interaction_effect / max_effect,
      # Convert negatives to show antagonism (though our metric doesn't have negatives)
      display_effect = normalized_effect
    )
  
  p <- ggplot(plot_data,
              aes(x = component_a, y = component_b, fill = display_effect)) +
    geom_tile(color = "white", linewidth = 1) +
    
    # Use a better color scale
    scale_fill_gradient2(
      low = "#2166ac",       # Blue for low interaction
      mid = "#f7f7f7",       # Light gray for moderate  
      high = "#762a83",      # Purple for high interaction
      midpoint = 0.5,
      limits = c(0, 1),
      name = "Interaction\nStrength",
      labels = c("Low", "", "Moderate", "", "High")
    ) +
    
    # Clean theme
    theme_minimal(base_size = 13) +
    theme(
      # Panel styling
      panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1.2),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      
      # Axis styling
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11, color = "black"),
      axis.text.y = element_text(face = "bold", size = 11, color = "black"),
      axis.title = element_text(face = "bold", size = 13, color = "black"),
      
      # Title styling
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "black"),
      
      # Legend
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.position = "right"
    ) +
    
    labs(
      title = paste("Component Interaction Strength -", toupper(metric)),
      subtitle = "Darker colors indicate stronger interactions between pipeline components",
      x = "Pipeline Component A",
      y = "Pipeline Component B"
    ) +
    
    coord_fixed()  # Make squares actually square
  
  return(p)
}

# Create and save the fixed plot
cat("Creating publication-quality heatmap...\n")
publication_plot <- create_publication_heatmap(interactions, "rrmse")

ggsave("scratch/component_interactions_publication.png", 
       publication_plot, 
       width = 10, height = 8, dpi = 300, bg = "white")

cat("✓ Publication plot saved to scratch/component_interactions_publication.png\n")

# Show top interactions
cat("\n=== TOP COMPONENT INTERACTIONS ===\n")
interactions %>%
  filter(component_a != component_b) %>%
  arrange(desc(interaction_effect)) %>%
  head(8) %>%
  mutate(interaction_effect = round(interaction_effect, 4)) %>%
  print()

cat("\nFixed interaction plot created!\n")