#!/usr/bin/env Rscript

## =============================================================================
## Component Interaction Analysis - Prototype
## Following exact horizons coding style and structure
## =============================================================================

library(qs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)

## ---------------------------------------------------------------------------
## Step 1: Load and process data using exact horizons patterns
## ---------------------------------------------------------------------------

data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"
batch_data <- qs::qread(data_path)

# Filter successful runs only
results_data <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse)

cat("Working with", nrow(results_data), "successful configurations\n")

## ---------------------------------------------------------------------------
## Step 2: Parse components exactly like existing horizons functions
## ---------------------------------------------------------------------------

parse_all_component_values_comprehensive <- function(data) {
  
  result <- data %>%
    mutate(
      # Model Type - following exact pattern from existing code
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
      
      # Response Transformation  
      Transformation = case_when(
        str_detect(config_desc, "_NoTrans_") ~ "None",
        str_detect(config_desc, "_Log_") ~ "Log",
        str_detect(config_desc, "_Sqrt_") ~ "Square Root", 
        TRUE ~ "None"
      ),
      
      # Spectral Preprocessing - exact hierarchy from existing code
      Preprocessing = case_when(
        str_detect(config_desc, "_SNVD2_") ~ "SNV + Derivative 2",
        str_detect(config_desc, "_SNVD1_") ~ "SNV + Derivative 1",
        str_detect(config_desc, "_MSCD1_") ~ "MSC + Derivative 1", 
        str_detect(config_desc, "_SNV_") ~ "SNV",
        str_detect(config_desc, "_MSC_") ~ "MSC",
        str_detect(config_desc, "_SG_") ~ "Savitzky-Golay",
        str_detect(config_desc, "_D2_") ~ "Derivative 2",
        str_detect(config_desc, "_D1_") ~ "Derivative 1",
        str_detect(config_desc, "_Raw_") ~ "Raw",
        TRUE ~ "Raw"
      ),
      
      # Feature Selection
      Feature_Selection = case_when(
        str_detect(config_desc, "_SHAP_") ~ "SHAP",
        str_detect(config_desc, "_PCA_") ~ "PCA",
        str_detect(config_desc, "_Corr_") ~ "Correlation",
        str_detect(config_desc, "_Boruta_") ~ "Boruta",
        TRUE ~ "PCA"
      ),
      
      # Covariates - extract final part after last underscore
      covariate_part = str_extract(config_desc, "[^_]+$"),
      Covariates = case_when(
        is.na(covariate_part) | covariate_part == "" ~ "None",
        covariate_part == config_desc ~ "None", # No underscore found
        TRUE ~ covariate_part
      )
    ) %>%
    select(-covariate_part)
  
  return(result)
}

# Parse components
parsed_data <- parse_all_component_values_comprehensive(results_data)

## ---------------------------------------------------------------------------
## Step 3: Compute pairwise interaction effects using ANOVA approach
## ---------------------------------------------------------------------------

compute_all_pairwise_interactions <- function(data, metric = "rrmse") {
  
  # Get all component columns
  component_cols <- c("ModelType", "Transformation", "Preprocessing", 
                     "Feature_Selection", "Covariates")
  
  # Overall mean for baseline
  overall_mean <- mean(data[[metric]], na.rm = TRUE)
  
  # Compute main effects for each component
  main_effects <- map(component_cols, function(comp) {
    data %>%
      group_by(.data[[comp]]) %>%
      summarise(
        level_mean = mean(.data[[metric]], na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(main_effect = level_mean - overall_mean) %>%
      select(!!comp := 1, main_effect)
  })
  names(main_effects) <- component_cols
  
  # Generate all component pairs
  component_pairs <- combn(component_cols, 2, simplify = FALSE)
  
  # Compute interaction for each pair
  interaction_results <- map_dfr(component_pairs, function(pair) {
    comp_a <- pair[1]
    comp_b <- pair[2]
    
    # Compute observed means for all A×B combinations
    interaction_data <- data %>%
      group_by(.data[[comp_a]], .data[[comp_b]]) %>%
      summarise(
        observed_mean = mean(.data[[metric]], na.rm = TRUE),
        n_obs = n(),
        .groups = "drop"
      ) %>%
      filter(n_obs >= 2) # Need at least 2 observations
    
    if (nrow(interaction_data) == 0) {
      return(NULL)
    }
    
    # Add main effects
    interaction_data <- interaction_data %>%
      left_join(main_effects[[comp_a]], by = comp_a) %>%
      left_join(main_effects[[comp_b]], by = comp_b) %>%
      rename(main_effect_a = main_effect.x,
             main_effect_b = main_effect.y) %>%
      mutate(
        # Expected = Overall + Main_A + Main_B
        expected_mean = overall_mean + main_effect_a + main_effect_b,
        # Interaction = Observed - Expected  
        interaction_effect = observed_mean - expected_mean
      )
    
    # Aggregate interaction effect (weighted by sample size)
    overall_interaction <- weighted.mean(
      interaction_data$interaction_effect,
      w = interaction_data$n_obs,
      na.rm = TRUE
    )
    
    # Return summary
    tibble(
      component_a = comp_a,
      component_b = comp_b, 
      interaction_effect = overall_interaction,
      n_combinations = nrow(interaction_data),
      total_obs = sum(interaction_data$n_obs)
    )
  })
  
  # Remove NULL results
  interaction_results <- interaction_results[!map_lgl(interaction_results, is.null)]
  
  if (length(interaction_results) > 0) {
    return(bind_rows(interaction_results))
  } else {
    return(tibble(
      component_a = character(0),
      component_b = character(0),
      interaction_effect = numeric(0),
      n_combinations = numeric(0),
      total_obs = numeric(0)
    ))
  }
}

# Compute interactions
cat("Computing pairwise interactions...\n")
interactions <- compute_all_pairwise_interactions(parsed_data, "rrmse")

print(interactions)

## ---------------------------------------------------------------------------
## Step 4: Create interaction heatmap following horizons style exactly
## ---------------------------------------------------------------------------

create_enhanced_radar_plot <- function(interaction_data, metric = "rrmse") {
  
  if (nrow(interaction_data) == 0) {
    stop("No interaction data available for plotting")
  }
  
  # Create symmetric matrix (A×B = B×A)
  reverse_pairs <- interaction_data %>%
    mutate(
      temp_a = component_b,
      temp_b = component_a,
      component_a = temp_a,
      component_b = temp_b
    ) %>%
    select(-temp_a, -temp_b)
  
  # Combine
  full_data <- bind_rows(interaction_data, reverse_pairs)
  
  # Get all unique components
  all_components <- unique(c(interaction_data$component_a, interaction_data$component_b))
  
  # Create complete grid
  complete_grid <- expand_grid(
    component_a = all_components,
    component_b = all_components  
  ) %>%
    left_join(full_data, by = c("component_a", "component_b")) %>%
    mutate(
      interaction_effect = ifelse(is.na(interaction_effect), 0, interaction_effect)
    )
  
  # Clean component names for display
  complete_grid <- complete_grid %>%
    mutate(
      component_a_clean = str_replace_all(component_a, "_", " "),
      component_b_clean = str_replace_all(component_b, "_", " ")
    )
  
  # Create heatmap following exact horizons theme
  max_abs_effect <- max(abs(complete_grid$interaction_effect), na.rm = TRUE)
  
  p <- ggplot(complete_grid, 
              aes(x = component_a_clean, y = component_b_clean, 
                  fill = interaction_effect)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#67001F",     # Red for antagonistic (horizons colors)
      mid = "white",       # White for no interaction
      high = "#053061",    # Blue for synergistic  
      midpoint = 0,
      limits = c(-max_abs_effect, max_abs_effect),
      name = "Interaction\nEffect"
    ) +
    # Exact horizons theme
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      
      axis.title.y = element_text(color = "black", size = 15, angle = 90, face = "bold"),
      axis.title.x = element_text(color = "black", size = 15, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black", size = 12),
      axis.text.y = element_text(face = "bold", color = "black", size = 12),
      
      plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "black", size = 13, hjust = 0.5),
      
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(size = 12),
      legend.position = "right"
    ) +
    labs(
      title = glue::glue("Component Interaction Effects ({toupper(metric)})"),
      subtitle = "Blue = Synergistic | Red = Antagonistic | White = Independent",
      x = "Pipeline Component A", 
      y = "Pipeline Component B"
    )
  
  return(p)
}

# Create plot
cat("Creating interaction heatmap...\n")
interaction_plot <- create_enhanced_radar_plot(interactions, "rrmse")

# Save plot
ggsave("scratch/component_interactions_prototype.png", 
       interaction_plot, 
       width = 10, height = 8, dpi = 300)

cat("Prototype complete. Plot saved to scratch/component_interactions_prototype.png\n")

## ---------------------------------------------------------------------------
## Step 5: Summary insights
## ---------------------------------------------------------------------------

cat("\n=== INTERACTION INSIGHTS ===\n")
cat("Strongest synergistic interactions (Blue, Positive):\n")
top_synergistic <- interactions %>%
  arrange(desc(interaction_effect)) %>%
  head(3)
print(top_synergistic)

cat("\nStrongest antagonistic interactions (Red, Negative):\n") 
top_antagonistic <- interactions %>%
  arrange(interaction_effect) %>%
  head(3)
print(top_antagonistic)