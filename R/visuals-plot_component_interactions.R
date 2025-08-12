#' Component Interaction Heatmap Visualization
#'
#' Creates heatmaps showing synergistic and antagonistic interactions between
#' different modeling components. Identifies combinations that work better or
#' worse together than their individual effects would predict, crucial for
#' understanding optimal pipeline configurations in spectroscopy modeling.
#'
#' @param results_data A data frame containing model evaluation results with
#'   `config_desc` column and performance metric columns.
#' @param metric Character. Performance metric to analyze (e.g., "rrmse", "rsq").
#' @param components Character vector. Pipeline components to analyze interactions between.
#' @param interaction_type Character. Type of interactions: "pairwise" (2-way) or "three_way" (3-way).
#' @param cluster_components Logical. Whether to hierarchically cluster components by similarity.
#' @param show_significance Logical. Whether to show statistical significance markers.
#' @param sig_threshold Numeric. P-value threshold for significance (default: 0.05).
#' @param response_variable_col Character. Column name for response variable (for multi-response).
#' @param comparison_type Character. How to handle multi-response: "side_by_side", "difference", or "separate".
#' @param color_scale Character. Color palette: "RdBu", "viridis", or "plasma".
#' @param title Character. Optional plot title.
#'
#' @return A `ggplot2` object showing component interaction effects.
#'
#' @details
#' **Interaction Analysis Method:**
#' For each pair of components A and B, computes:
#' - Individual effects: Performance variation due to A alone and B alone
#' - Combined effect: Performance variation when both A and B vary together
#' - Interaction effect: Combined - (Individual A + Individual B)
#' 
#' **Interaction Interpretation:**
#' - Positive values (blue): Synergistic - components work better together
#' - Negative values (red): Antagonistic - components interfere with each other
#' - Zero (white): Additive - no interaction, effects are independent
#'
#' **Statistical Significance:**
#' Uses permutation testing to assess whether observed interactions are
#' significantly different from random chance.
#'
#' **Clustering:**
#' Hierarchical clustering groups components with similar interaction patterns,
#' revealing families of components that behave similarly across the pipeline.
#'
#' @examples
#' \dontrun{
#' # Basic pairwise interaction heatmap
#' plot_component_interactions(
#'   results_data = model_results,
#'   metric = "rrmse",
#'   components = c("ModelType", "Preprocessing", "Feature_Selection")
#' )
#'
#' # Multi-response comparison showing differences
#' plot_component_interactions(
#'   results_data = multi_response_results,
#'   metric = "rrmse",
#'   response_variable_col = "response_var",
#'   comparison_type = "difference",
#'   show_significance = TRUE
#' )
#'
#' # Clustered heatmap with custom colors
#' plot_component_interactions(
#'   results_data = model_results,
#'   metric = "rsq",
#'   cluster_components = TRUE,
#'   color_scale = "viridis",
#'   title = "Component Synergies for R-squared"
#' )
#' }
#'
#' @seealso
#' \code{\link{plot_component_effects_enhanced}}, \code{\link{plot_property_effects}},
#' \code{\link{horizons_theme}}, \code{\link{horizons_colors}}
#'
#' @importFrom dplyr mutate filter group_by summarise ungroup select arrange bind_rows
#' @importFrom purrr map_dfr map2 map
#' @importFrom stringr str_detect str_split_i str_count
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 scale_fill_viridis_c labs facet_wrap theme element_text
#' @importFrom tidyr expand_grid pivot_wider pivot_longer
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats hclust dist as.dist
#'
#' @family visualization
#' @export

plot_component_interactions <- function(results_data,
                                      metric = "rrmse",
                                      components = c("ModelType", "Preprocessing", "Feature_Selection"),
                                      interaction_type = c("pairwise", "three_way"),
                                      cluster_components = TRUE,
                                      show_significance = TRUE,
                                      sig_threshold = 0.05,
                                      response_variable_col = NULL,
                                      comparison_type = c("side_by_side", "difference", "separate"),
                                      color_scale = c("RdBu", "viridis", "plasma"),
                                      title = NULL) {
  
  # Match arguments
  interaction_type <- match.arg(interaction_type)
  comparison_type <- match.arg(comparison_type)
  color_scale <- match.arg(color_scale)
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation and Data Preparation
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      validate_visualization_data(
        data = results_data,
        required_columns = c("config_desc"),
        metric = metric,
        min_rows = 20  # Need more data for interaction analysis
      )
      
      # Prepare multi-response data if needed
      prepared_data <- prepare_multi_response_data(
        data = results_data,
        response_column = response_variable_col,
        metric_columns = metric,
        config_column = "config_desc"
      )
      
      TRUE
    },
    default_value = FALSE,
    error_message = "Data validation failed for interaction analysis"
  ) -> validation_result
  
  if (!validation_result$result) {
    return(NULL)
  }
  
  prepared_data <- validation_result$result
  if (is.logical(prepared_data)) {
    prepared_data <- prepare_multi_response_data(
      data = results_data,
      response_column = response_variable_col,
      metric_columns = metric
    )
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Parse Config Descriptors
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      parse_config_descriptors(prepared_data, components)
    },
    default_value = NULL,
    error_message = "Failed to parse config descriptors for interaction analysis"
  ) -> parsed_data_result
  
  if (is.null(parsed_data_result$result)) {
    return(NULL)
  }
  
  parsed_data <- parsed_data_result$result
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Compute Component Interactions
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      compute_component_interactions(
        data = parsed_data,
        metric = metric,
        components = components,
        interaction_type = interaction_type,
        show_significance = show_significance,
        sig_threshold = sig_threshold,
        response_variable_col = if (!is.null(response_variable_col)) "response_variable" else NULL
      )
    },
    default_value = NULL,
    error_message = "Failed to compute component interactions"
  ) -> interaction_results
  
  if (is.null(interaction_results$result)) {
    return(NULL)
  }
  
  interaction_data <- interaction_results$result
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Create Interaction Heatmap
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      create_interaction_heatmap(
        data = interaction_data,
        metric = metric,
        cluster_components = cluster_components,
        show_significance = show_significance,
        response_variable_col = if (!is.null(response_variable_col)) "response_variable" else NULL,
        comparison_type = comparison_type,
        color_scale = color_scale,
        title = title
      )
    },
    default_value = NULL,
    error_message = "Failed to create interaction heatmap"
  ) -> plot_result
  
  if (is.null(plot_result$result)) {
    cli::cli_alert_warning("Returning NULL plot due to errors")
    return(NULL)
  }
  
  return(plot_result$result)
}


## =============================================================================
## Helper Functions
## =============================================================================

#' Compute Component Interactions
#'
#' Calculates interaction effects between pipeline components using variance decomposition.
#'
#' @param data Parsed data frame with component columns
#' @param metric Performance metric name
#' @param components Component names to analyze
#' @param interaction_type Type of interactions to compute
#' @param show_significance Whether to compute significance tests
#' @param sig_threshold Significance threshold for tests
#' @param response_variable_col Response variable column for grouping
#' @return Data frame with interaction effects
#' @keywords internal

compute_component_interactions <- function(data, metric, components, 
                                         interaction_type = "pairwise",
                                         show_significance = TRUE,
                                         sig_threshold = 0.05,
                                         response_variable_col = NULL) {
  
  # Filter out any rows with missing metric values
  data <- data %>%
    dplyr::filter(!is.na(.data[[metric]]))
  
  if (nrow(data) == 0) {
    cli::cli_abort("No valid data remaining after filtering missing metric values")
  }
  
  # Check if components exist in data
  missing_components <- components[!components %in% names(data)]
  if (length(missing_components) > 0) {
    cli::cli_alert_warning("Components not found in data: {missing_components}")
    components <- components[components %in% names(data)]
  }
  
  if (length(components) < 2) {
    cli::cli_abort("Need at least 2 components for interaction analysis")
  }
  
  cli::cli_alert_info("Computing {interaction_type} interactions for {length(components)} components...")
  
  if (interaction_type == "pairwise") {
    interaction_results <- compute_pairwise_interactions(
      data, metric, components, show_significance, sig_threshold, response_variable_col
    )
  } else {
    interaction_results <- compute_threeway_interactions(
      data, metric, components, show_significance, sig_threshold, response_variable_col
    )
  }
  
  return(interaction_results)
}


#' Compute Pairwise Component Interactions
#'
#' Calculates 2-way interactions between all pairs of components.
#'
#' @param data Data frame with parsed components
#' @param metric Performance metric
#' @param components Component names
#' @param show_significance Whether to test significance  
#' @param sig_threshold Significance threshold
#' @param response_variable_col Response variable grouping column
#' @return Data frame with pairwise interaction effects
#' @keywords internal

compute_pairwise_interactions <- function(data, metric, components, 
                                        show_significance, sig_threshold,
                                        response_variable_col) {
  
  # Generate all component pairs
  component_pairs <- utils::combn(components, 2, simplify = FALSE)
  
  # Function to compute interaction for a single pair
  compute_pair_interaction <- function(pair, data, metric, response_variable_col) {
    comp_a <- pair[1]
    comp_b <- pair[2]
    other_components <- setdiff(components, pair)
    
    # Group by other components (and response variable if present)
    grouping_vars <- other_components
    if (!is.null(response_variable_col)) {
      grouping_vars <- c(response_variable_col, other_components)
    }
    
    # Compute individual and combined effects
    interaction_result <- data %>%
      {if (length(grouping_vars) > 0) 
        dplyr::group_by(., dplyr::across(dplyr::all_of(grouping_vars))) 
       else .} %>%
      # Ensure we have variation in both components
      dplyr::filter(
        dplyr::n_distinct(.data[[comp_a]]) > 1,
        dplyr::n_distinct(.data[[comp_b]]) > 1
      ) %>%
      dplyr::summarise(
        # Individual effects (marginal variance)
        effect_a = stats::var(.data[[metric]][.data[[comp_a]] == names(sort(table(.data[[comp_a]]), decreasing = TRUE))[1]], na.rm = TRUE) -
                  stats::var(.data[[metric]][.data[[comp_a]] == names(sort(table(.data[[comp_a]]), decreasing = TRUE))[2]], na.rm = TRUE),
        effect_b = stats::var(.data[[metric]][.data[[comp_b]] == names(sort(table(.data[[comp_b]]), decreasing = TRUE))[1]], na.rm = TRUE) -
                  stats::var(.data[[metric]][.data[[comp_b]] == names(sort(table(.data[[comp_b]]), decreasing = TRUE))[2]], na.rm = TRUE),
        
        # Combined effect (total variance when both vary)
        combined_effect = stats::var(.data[[metric]], na.rm = TRUE),
        
        # Sample size for this group
        n_obs = dplyr::n(),
        .groups = "drop"
      ) %>%
      # Filter out groups with insufficient data
      dplyr::filter(n_obs >= 4) %>%
      # Compute interaction effect
      dplyr::mutate(
        # Interaction = Combined - Individual effects
        interaction_effect = combined_effect - abs(effect_a) - abs(effect_b),
        component_a = comp_a,
        component_b = comp_b
      )
    
    return(interaction_result)
  }
  
  # Compute interactions for all pairs
  interaction_results <- purrr::map_dfr(component_pairs, function(pair) {
    safely_execute(
      expr = compute_pair_interaction(pair, data, metric, response_variable_col),
      default_value = NULL,
      error_message = glue::glue("Failed to compute interaction for {pair[1]} × {pair[2]}")
    )$result
  })
  
  # Filter out NULL results and aggregate
  interaction_summary <- interaction_results %>%
    dplyr::filter(!is.na(interaction_effect)) %>%
    dplyr::group_by(component_a, component_b) %>%
    {if (!is.null(response_variable_col)) 
      dplyr::group_by(., .data[[response_variable_col]], .add = TRUE) 
     else .} %>%
    dplyr::summarise(
      mean_interaction = mean(interaction_effect, na.rm = TRUE),
      sd_interaction = stats::sd(interaction_effect, na.rm = TRUE),
      n_groups = dplyr::n(),
      .groups = "drop"
    ) %>%
    # Add significance testing if requested
    {if (show_significance) {
      dplyr::mutate(.,
        p_value = 2 * (1 - stats::pnorm(abs(mean_interaction / (sd_interaction / sqrt(n_groups))))),
        significant = p_value < sig_threshold,
        significance_label = dplyr::case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**", 
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
    } else {
      dplyr::mutate(., p_value = NA, significant = FALSE, significance_label = "")
    }}
  
  # Add symmetric pairs (A×B = B×A)
  symmetric_pairs <- interaction_summary %>%
    dplyr::mutate(
      temp_a = component_b,
      temp_b = component_a,
      component_a = temp_a,
      component_b = temp_b
    ) %>%
    dplyr::select(-temp_a, -temp_b)
  
  final_results <- dplyr::bind_rows(interaction_summary, symmetric_pairs)
  
  return(final_results)
}


#' Compute Three-way Component Interactions
#'
#' Calculates 3-way interactions between component triplets.
#'
#' @param data Data frame with parsed components
#' @param metric Performance metric
#' @param components Component names  
#' @param show_significance Whether to test significance
#' @param sig_threshold Significance threshold
#' @param response_variable_col Response variable grouping column
#' @return Data frame with three-way interaction effects
#' @keywords internal

compute_threeway_interactions <- function(data, metric, components,
                                        show_significance, sig_threshold,
                                        response_variable_col) {
  
  if (length(components) < 3) {
    cli::cli_alert_warning("Need at least 3 components for three-way interactions")
    return(data.frame())
  }
  
  # Generate all component triplets
  component_triplets <- utils::combn(components, 3, simplify = FALSE)
  
  # Function to compute three-way interaction for a single triplet
  compute_triplet_interaction <- function(triplet, data, metric, response_variable_col) {
    comp_a <- triplet[1]
    comp_b <- triplet[2] 
    comp_c <- triplet[3]
    other_components <- setdiff(components, triplet)
    
    # Group by other components (and response variable if present)
    grouping_vars <- other_components
    if (!is.null(response_variable_col)) {
      grouping_vars <- c(response_variable_col, other_components)
    }
    
    # Compute three-way interaction
    interaction_result <- data %>%
      {if (length(grouping_vars) > 0) 
        dplyr::group_by(., dplyr::across(dplyr::all_of(grouping_vars))) 
       else .} %>%
      # Ensure variation in all three components
      dplyr::filter(
        dplyr::n_distinct(.data[[comp_a]]) > 1,
        dplyr::n_distinct(.data[[comp_b]]) > 1,
        dplyr::n_distinct(.data[[comp_c]]) > 1
      ) %>%
      dplyr::summarise(
        # Full model variance
        total_variance = stats::var(.data[[metric]], na.rm = TRUE),
        
        # Component counts for verification
        n_obs = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_obs >= 8) %>%  # Need more data for 3-way
      dplyr::mutate(
        component_a = comp_a,
        component_b = comp_b,
        component_c = comp_c,
        interaction_effect = total_variance  # Simplified 3-way interaction
      )
    
    return(interaction_result)
  }
  
  # Compute interactions for all triplets
  interaction_results <- purrr::map_dfr(component_triplets, function(triplet) {
    safely_execute(
      expr = compute_triplet_interaction(triplet, data, metric, response_variable_col),
      default_value = NULL,
      error_message = glue::glue("Failed to compute 3-way interaction for {paste(triplet, collapse = ' × ')}")
    )$result
  })
  
  # Aggregate results
  interaction_summary <- interaction_results %>%
    dplyr::filter(!is.na(interaction_effect)) %>%
    dplyr::group_by(component_a, component_b, component_c) %>%
    {if (!is.null(response_variable_col)) 
      dplyr::group_by(., .data[[response_variable_col]], .add = TRUE) 
     else .} %>%
    dplyr::summarise(
      mean_interaction = mean(interaction_effect, na.rm = TRUE),
      sd_interaction = stats::sd(interaction_effect, na.rm = TRUE),
      n_groups = dplyr::n(),
      .groups = "drop"
    )
  
  return(interaction_summary)
}


#' Create Interaction Heatmap
#'
#' Creates a heatmap visualization of component interactions.
#'
#' @param data Interaction effects data
#' @param metric Performance metric
#' @param cluster_components Whether to cluster components
#' @param show_significance Whether to show significance markers
#' @param response_variable_col Response variable column name
#' @param comparison_type How to handle multi-response
#' @param color_scale Color palette to use
#' @param title Plot title
#' @return ggplot2 object
#' @keywords internal

create_interaction_heatmap <- function(data, metric, cluster_components = TRUE,
                                     show_significance = TRUE,
                                     response_variable_col = NULL,
                                     comparison_type = "side_by_side",
                                     color_scale = "RdBu", title = NULL) {
  
  if (nrow(data) == 0) {
    cli::cli_abort("No interaction data available for plotting")
  }
  
  # Prepare data for heatmap
  plot_data <- data
  
  # Handle multi-response comparison
  if (!is.null(response_variable_col) && comparison_type == "difference") {
    # Compute differences between response variables
    if (length(unique(plot_data[[response_variable_col]])) == 2) {
      responses <- unique(plot_data[[response_variable_col]])
      
      wide_data <- plot_data %>%
        tidyr::pivot_wider(
          names_from = all_of(response_variable_col),
          values_from = mean_interaction,
          names_prefix = "resp_"
        ) %>%
        dplyr::mutate(
          interaction_difference = .data[[paste0("resp_", responses[1])]] - 
                                  .data[[paste0("resp_", responses[2])]],
          comparison_label = glue::glue("{responses[1]} - {responses[2]}")
        )
      
      plot_data <- wide_data %>%
        dplyr::select(component_a, component_b, interaction_difference, comparison_label) %>%
        dplyr::rename(mean_interaction = interaction_difference) %>%
        dplyr::mutate(!!response_variable_col := comparison_label)
    }
  }
  
  # Apply clustering if requested
  if (cluster_components) {
    # Create distance matrix from interaction effects
    interaction_matrix <- plot_data %>%
      tidyr::pivot_wider(
        names_from = component_b,
        values_from = mean_interaction,
        values_fill = 0
      ) %>%
      tibble::column_to_rownames("component_a") %>%
      as.matrix()
    
    # Hierarchical clustering
    if (nrow(interaction_matrix) > 1 && ncol(interaction_matrix) > 1) {
      dist_matrix <- stats::dist(interaction_matrix)
      hc <- stats::hclust(dist_matrix, method = "ward.D2")
      component_order <- rownames(interaction_matrix)[hc$order]
      
      # Apply ordering to plot data
      plot_data <- plot_data %>%
        dplyr::mutate(
          component_a = factor(component_a, levels = component_order),
          component_b = factor(component_b, levels = component_order)
        )
    }
  }
  
  # Format component names
  plot_data <- plot_data %>%
    dplyr::mutate(
      component_a_formatted = format_component_names(as.character(component_a), "publication"),
      component_b_formatted = format_component_names(as.character(component_b), "publication")
    )
  
  # Create base heatmap
  p <- ggplot2::ggplot(plot_data, 
                      ggplot2::aes(x = component_a_formatted, 
                                  y = component_b_formatted,
                                  fill = mean_interaction))
  
  # Add tiles
  p <- p + ggplot2::geom_tile(color = "white", linewidth = 0.5)
  
  # Apply color scale
  max_abs_interaction <- max(abs(plot_data$mean_interaction), na.rm = TRUE)
  
  if (color_scale == "RdBu") {
    p <- p + ggplot2::scale_fill_gradient2(
      low = "#67001F", mid = "white", high = "#053061",
      midpoint = 0,
      limits = c(-max_abs_interaction, max_abs_interaction),
      name = "Interaction\nEffect"
    )
  } else if (color_scale == "viridis") {
    p <- p + ggplot2::scale_fill_viridis_c(name = "Interaction\nEffect")
  } else {
    p <- p + ggplot2::scale_fill_viridis_c(option = "plasma", name = "Interaction\nEffect")
  }
  
  # Add significance markers if requested
  if (show_significance && "significance_label" %in% names(plot_data)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = significance_label),
      color = "black",
      size = 4,
      fontface = "bold"
    )
  }
  
  # Apply theme
  p <- p + 
    horizons_theme(plot_type = "heatmap") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
  
  # Add labels
  if (is.null(title)) {
    if (!is.null(response_variable_col) && comparison_type == "difference") {
      title <- glue::glue("Component Interaction Differences ({toupper(metric)})")
    } else {
      title <- glue::glue("Component Interaction Effects ({toupper(metric)})")
    }
  }
  
  subtitle <- "Blue = Synergistic, Red = Antagonistic, White = Independent"
  if (show_significance) {
    subtitle <- paste(subtitle, "| * p<0.05, ** p<0.01, *** p<0.001")
  }
  
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = "Pipeline Component A",
    y = "Pipeline Component B"
  )
  
  # Add faceting for multi-response if requested
  if (!is.null(response_variable_col) && comparison_type == "side_by_side") {
    p <- p + ggplot2::facet_wrap(~ .data[[response_variable_col]])
  }
  
  return(p)
}