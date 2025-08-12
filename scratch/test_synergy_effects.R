#!/usr/bin/env Rscript

# Test component synergy effects analysis

# Load required libraries
library(qs)
library(dplyr)
library(ggplot2)
library(tidyr)

# Source required functions  
source("R/analysis-hierarchical_interaction_decomposition.R")
source("R/analysis-component_synergy_effects.R")
source("R/visuals-plot_component_network_bundling.R")  # For parsing functions
source("R/utils_error_handling.R")
source("R/utils-visualization_theme.R")

# Load the MAOM data
data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

if (!file.exists(data_path)) {
  cat("ERROR: Data file not found at expected path\n")
  quit(status = 1)
}

cat("Loading MAOM data for synergy effects analysis...\n")
batch_data <- qs::qread(data_path)

maom_batch_summary <- batch_data %>%
  filter(status == "success") %>%
  select(config_desc, rsq, rmse, rrmse) %>%
  filter(rrmse < quantile(rrmse, 0.95, na.rm = TRUE))

# First run hierarchical decomposition (needed for synergy analysis)
cat("Running hierarchical decomposition...\n")
decomp_results <- hierarchical_interaction_decomposition(
  results_data = maom_batch_summary,
  metric = "rrmse",
  covariate_handling = "simplified",
  max_interaction_level = 4
)

if (is.null(decomp_results)) {
  cat("ERROR: Hierarchical decomposition failed\n")
  quit(status = 1)
}

# Now run synergy effects analysis
cat("Running component synergy effects analysis...\n")
synergy_results <- analyze_component_synergy_effects(
  decomp_results = decomp_results,
  top_interactions = 12
)

if (!is.null(synergy_results)) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("COMPONENT SYNERGY EFFECTS RESULTS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Print component synergy scores
  cat("\nComponent Synergy Scores:\n")
  synergy_scores <- synergy_results$component_synergy_scores
  print(synergy_scores)
  
  # Print top interactions
  cat("\nTop Specific Interactions:\n")
  top_interactions <- synergy_results$top_specific_interactions
  for (i in 1:min(8, nrow(top_interactions))) {
    interaction <- top_interactions[i, ]
    effect_type <- ifelse(interaction$effect_size > 0, "Synergistic", "Antagonistic")
    cat(sprintf("  %s: %s (%.3f)\n", 
                interaction$interaction_clean, 
                effect_type,
                interaction$effect_size))
  }
  
  # Create visualization
  cat("\nCreating synergy effects visualization...\n")
  
  # Create individual plots since patchwork might not be available
  synergy_data <- synergy_results$component_synergy_scores
  interaction_data <- synergy_results$top_specific_interactions
  
  # Component synergy plot
  synergy_plot_data <- synergy_data %>%
    dplyr::select(component, main_effect, synergy_score) %>%
    tidyr::pivot_longer(cols = c(main_effect, synergy_score), 
                       names_to = "effect_type", 
                       values_to = "effect_size") %>%
    dplyr::mutate(
      effect_type = factor(effect_type, 
                          levels = c("main_effect", "synergy_score"),
                          labels = c("Direct Effect", "Synergy Effect")),
      component = reorder(component, effect_size, sum)
    )
  
  p1 <- ggplot2::ggplot(synergy_plot_data, ggplot2::aes(x = component, y = effect_size, fill = effect_type)) +
    ggplot2::geom_col(position = "stack", alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c("Direct Effect" = "steelblue", "Synergy Effect" = "orange")) +
    ggplot2::labs(
      title = "Component Effect Decomposition",
      subtitle = "Direct effects vs synergistic contributions",
      x = "Pipeline Component",
      y = "Effect Magnitude",
      fill = "Effect Type"
    ) +
    horizons_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave("scratch/component_synergy_decomposition.png", p1, 
         width = 10, height = 6, dpi = 300, bg = "white")
  
  # Top interactions plot  
  interaction_plot_data <- interaction_data %>%
    dplyr::slice_head(n = 8) %>%
    dplyr::mutate(
      interaction_label = paste0(interaction_clean, " (", round(effect_size, 3), ")"),
      interaction_label = reorder(interaction_label, abs_effect),
      color_group = ifelse(effect_size > 0, "Synergistic", "Antagonistic")
    )
  
  p2 <- ggplot2::ggplot(interaction_plot_data, ggplot2::aes(x = interaction_label, y = effect_size, fill = color_group)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c("Synergistic" = "darkgreen", "Antagonistic" = "darkred")) +
    ggplot2::labs(
      title = "Top Specific Component Interactions",
      subtitle = "Strongest synergistic and antagonistic effects",
      x = "Component Interaction",
      y = "Interaction Effect Size",
      fill = "Effect Type"
    ) +
    ggplot2::coord_flip() +
    horizons_theme() +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  ggsave("scratch/specific_interactions.png", p2,
         width = 10, height = 6, dpi = 300, bg = "white")
  
  cat("Plots saved to:\n")
  cat("  - scratch/component_synergy_decomposition.png\n")
  cat("  - scratch/specific_interactions.png\n")
  
  # Summary insights
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat("KEY INSIGHTS\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # Most synergistic component
  most_synergistic <- synergy_scores[which.max(synergy_scores$synergy_score), ]
  cat("\nMost synergistic component:", most_synergistic$component, 
      sprintf("(%.2f synergy score)\n", most_synergistic$synergy_score))
  
  # Strongest specific interaction
  strongest_interaction <- top_interactions[1, ]
  cat("Strongest specific interaction:", strongest_interaction$interaction_clean,
      sprintf("(%.3f effect)\n", strongest_interaction$effect_size))
  
  # Synergy vs direct effects ratio
  total_synergy <- sum(synergy_scores$synergy_score)
  total_direct <- sum(synergy_scores$main_effect)
  synergy_ratio <- total_synergy / (total_synergy + total_direct) * 100
  
  cat(sprintf("\nOverall synergy contribution: %.1f%% of total effects\n", synergy_ratio))
  
} else {
  cat("ERROR: Component synergy effects analysis failed\n")
}

cat("\nAnalysis complete!\n")