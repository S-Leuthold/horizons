#' Component Synergy Effects Analysis
#'
#' Decomposes component performance into direct effects and synergistic effects,
#' then identifies the specific combinations driving those synergies.
#'
#' @param decomp_results Results from hierarchical_interaction_decomposition()
#' @param top_interactions Integer. Number of top specific interactions to show.
#'
#' @return List containing:
#'   \itemize{
#'     \item `component_synergy_scores`: Data frame with synergy contribution by component
#'     \item `top_specific_interactions`: Data frame with highest-impact specific combinations
#'     \item `interaction_matrix`: Full matrix of pairwise interaction effects
#'   }
#'
#' @examples
#' \dontrun{
#' synergy_results <- analyze_component_synergy_effects(decomp_results)
#' plot_component_synergy_effects(synergy_results)
#' }
#'
#' @family analysis
#' @export

analyze_component_synergy_effects <- function(decomp_results, top_interactions = 10) {
  
  cli::cli_alert_info("Analyzing component synergy effects...")
  
  # Extract the pairwise model (where most synergy happens)
  pairwise_model <- decomp_results$models$pairwise
  
  if (is.null(pairwise_model)) {
    cli::cli_abort("Pairwise interaction model not found in decomp_results")
  }
  
  # Get model coefficients
  coeffs <- coef(pairwise_model)
  coeff_names <- names(coeffs)
  
  # Separate main effects from interaction effects
  main_effects <- coeffs[!grepl(":", coeff_names)]
  interaction_effects <- coeffs[grepl(":", coeff_names)]
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Calculate Component Synergy Scores
  ## ---------------------------------------------------------------------------
  
  component_names <- c("ModelType", "Preprocessing", "Feature_Selection", "Transformation", "Covariates")
  synergy_scores <- data.frame(
    component = component_names,
    synergy_score = 0,
    n_interactions = 0,
    stringsAsFactors = FALSE
  )
  
  # Sum up interaction effects for each component
  for (comp in component_names) {
    # Find all interaction terms involving this component
    comp_interactions <- interaction_effects[grepl(comp, names(interaction_effects))]
    
    if (length(comp_interactions) > 0) {
      # Sum absolute values to get total synergy contribution
      synergy_scores[synergy_scores$component == comp, "synergy_score"] <- sum(abs(comp_interactions), na.rm = TRUE)
      synergy_scores[synergy_scores$component == comp, "n_interactions"] <- length(comp_interactions)
    }
  }
  
  # Normalize synergy scores to percentage of total variance
  total_synergy <- sum(synergy_scores$synergy_score)
  if (total_synergy > 0) {
    synergy_scores$synergy_percent <- (synergy_scores$synergy_score / total_synergy) * 100
  } else {
    synergy_scores$synergy_percent <- 0
  }
  
  # Add main effects for comparison
  main_effect_scores <- data.frame(
    component = component_names,
    main_effect = 0,
    stringsAsFactors = FALSE
  )
  
  for (comp in component_names) {
    comp_main_effects <- main_effects[grepl(comp, names(main_effects)) & !grepl(":", names(main_effects))]
    if (length(comp_main_effects) > 0) {
      main_effect_scores[main_effect_scores$component == comp, "main_effect"] <- sum(abs(comp_main_effects), na.rm = TRUE)
    }
  }
  
  # Combine main effects and synergy scores
  synergy_scores <- merge(synergy_scores, main_effect_scores, by = "component")
  synergy_scores$total_effect <- synergy_scores$synergy_score + synergy_scores$main_effect
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Extract Top Specific Interactions
  ## ---------------------------------------------------------------------------
  
  # Sort interaction effects by magnitude
  interaction_df <- data.frame(
    interaction = names(interaction_effects),
    effect_size = interaction_effects,
    abs_effect = abs(interaction_effects),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::arrange(desc(abs_effect)) %>%
    dplyr::slice_head(n = top_interactions)
  
  # Clean up interaction names for readability
  interaction_df$interaction_clean <- gsub("ModelType|Preprocessing|Feature_Selection|Transformation|Covariates", "", interaction_df$interaction)
  interaction_df$interaction_clean <- gsub(":", " × ", interaction_df$interaction_clean)
  interaction_df$interaction_clean <- trimws(interaction_df$interaction_clean)
  
  # Add interpretation
  interaction_df$effect_type <- ifelse(interaction_df$effect_size > 0, "Synergistic", "Antagonistic")
  interaction_df$effect_magnitude <- case_when(
    interaction_df$abs_effect > quantile(interaction_df$abs_effect, 0.8) ~ "Large",
    interaction_df$abs_effect > quantile(interaction_df$abs_effect, 0.5) ~ "Medium", 
    TRUE ~ "Small"
  )
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Create Interaction Matrix for Heatmap
  ## ---------------------------------------------------------------------------
  
  # Create pairwise interaction matrix
  interaction_matrix <- matrix(0, 
                              nrow = length(component_names), 
                              ncol = length(component_names),
                              dimnames = list(component_names, component_names))
  
  # Fill matrix with interaction effects
  for (i in 1:nrow(interaction_df)) {
    interaction_name <- interaction_df$interaction[i]
    effect_size <- interaction_df$effect_size[i]
    
    # Parse component names from interaction term
    components_in_interaction <- component_names[sapply(component_names, function(comp) grepl(comp, interaction_name))]
    
    if (length(components_in_interaction) == 2) {
      comp1 <- components_in_interaction[1]
      comp2 <- components_in_interaction[2]
      interaction_matrix[comp1, comp2] <- effect_size
      interaction_matrix[comp2, comp1] <- effect_size  # Symmetric
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Return Results
  ## ---------------------------------------------------------------------------
  
  results <- list(
    component_synergy_scores = synergy_scores,
    top_specific_interactions = interaction_df,
    interaction_matrix = interaction_matrix,
    model_info = list(
      r_squared = summary(pairwise_model)$r.squared,
      n_observations = nobs(pairwise_model),
      metric = decomp_results$metric
    )
  )
  
  class(results) <- c("synergy_analysis", "list")
  
  cli::cli_alert_info("Component synergy analysis complete!")
  return(results)
}


## =============================================================================
## Visualization Function
## =============================================================================

#' Plot Component Synergy Effects
#'
#' Creates a two-panel visualization showing component synergy scores and
#' top specific interactions.
#'
#' @param synergy_results Results from analyze_component_synergy_effects()
#' @param title Character. Optional plot title
#'
#' @return ggplot object
#' @export

plot_component_synergy_effects <- function(synergy_results, title = NULL) {
  
  if (!inherits(synergy_results, "synergy_analysis")) {
    cli::cli_abort("Input must be results from analyze_component_synergy_effects()")
  }
  
  synergy_data <- synergy_results$component_synergy_scores
  interaction_data <- synergy_results$top_specific_interactions
  
  if (is.null(title)) {
    title <- glue::glue("Component Synergy Effects ({toupper(synergy_results$model_info$metric)})")
  }
  
  ## ---------------------------------------------------------------------------
  ## Panel 1: Component Synergy Scores
  ## ---------------------------------------------------------------------------
  
  # Prepare data for stacked bar chart
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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ## ---------------------------------------------------------------------------
  ## Panel 2: Top Specific Interactions
  ## ---------------------------------------------------------------------------
  
  # Prepare interaction data
  interaction_plot_data <- interaction_data %>%
    dplyr::slice_head(n = 8) %>%  # Show top 8 for readability
    dplyr::mutate(
      interaction_label = paste0(interaction_clean, " (", round(effect_size, 3), ")"),
      interaction_label = reorder(interaction_label, abs_effect),
      color_group = ifelse(effect_size > 0, "Synergistic", "Antagonistic")
    )
  
  p2 <- ggplot2::ggplot(interaction_plot_data, ggplot2::aes(x = interaction_label, y = effect_size, fill = color_group)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c("Synergistic" = "darkgreen", "Antagonistic" = "darkred")) +
    ggplot2::labs(
      title = "Top Specific Interactions",
      subtitle = "Strongest synergistic and antagonistic effects",
      x = "Component Interaction",
      y = "Interaction Effect Size",
      fill = "Effect Type"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  ## ---------------------------------------------------------------------------
  ## Combine Panels
  ## ---------------------------------------------------------------------------
  
  # Check if patchwork is available for combining plots
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined_plot <- p1 / p2 + 
      patchwork::plot_annotation(title = title)
    return(combined_plot)
  } else {
    # Return list of plots if patchwork not available
    return(list(component_effects = p1, specific_interactions = p2))
  }
}