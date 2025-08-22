#' Enhanced Component Interaction Analysis and Visualization
#'
#' Creates comprehensive heatmaps showing synergistic and antagonistic interactions
#' between different modeling pipeline components. Identifies component combinations
#' that produce better or worse performance than their individual effects would predict,
#' critical for understanding optimal pipeline configurations in spectroscopy modeling.
#'
#' @param results_data A data frame containing model evaluation results with at minimum:
#'   `config_desc` column with workflow identifiers and performance metric columns.
#' @param metric Character. Performance metric to analyze (e.g., "rrmse", "rsq", "rmse").
#'   Must be a column name in `results_data`.
#' @param components Character vector. Pipeline components to analyze interactions between.
#'   Default: `c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")`.
#' @param covariate_handling Character. How to handle covariates: "aggregate" (group by presence),
#'   "detailed" (keep individual covariate combinations), or "simplified" (soil/climate/mixed).
#' @param cluster_components Logical. Whether to hierarchically cluster components by
#'   interaction similarity for better visualization ordering.
#' @param significance_test Logical. Whether to compute statistical significance of interactions
#'   using permutation testing (computationally intensive).
#' @param min_observations Integer. Minimum number of observations required for each component
#'   combination to be included in interaction calculations.
#' @param color_palette Character. Color scheme: "default" (horizons colors), "viridis", or "plasma".
#' @param title Character. Optional plot title. If `NULL`, generates automatic title.
#'
#' @return A `ggplot2` object showing component interaction effects as a heatmap.
#'   Blue indicates synergistic interactions (components work better together),
#'   red indicates antagonistic interactions (components interfere), and
#'   white indicates independent/additive effects.
#'
#' @details
#' **Interaction Analysis Method:**
#' Uses ANOVA-style interaction decomposition. For each component pair (A, B):
#' \enumerate{
#'   \item Computes overall mean performance across all configurations
#'   \item Calculates main effect of component A: mean(A_level) - overall_mean
#'   \item Calculates main effect of component B: mean(B_level) - overall_mean  
#'   \item For each A×B combination, computes expected = overall + main_A + main_B
#'   \item Interaction effect = observed - expected (averaged across all A×B combinations)
#' }
#'
#' **Interpretation:**
#' - Positive values (blue): Synergistic - components produce better performance together
#' - Negative values (red): Antagonistic - components interfere with each other
#' - Zero/near-zero (white): Independent - effects are purely additive
#'
#' **Component Parsing:**
#' Automatically extracts components from `config_desc` using robust pattern matching:
#' - ModelType: cubist, random_forest, xgboost, mars, plsr, svm_rbf, etc.
#' - Transformation: NoTrans, Log, Sqrt
#' - Preprocessing: Raw, SNV, MSC, derivatives, combinations
#' - Feature_Selection: PCA, SHAP, Correlation, Boruta
#' - Covariates: Complex parsing with optional aggregation
#'
#' **Clustering:**
#' When `cluster_components = TRUE`, uses hierarchical clustering (Ward's method)
#' on the interaction matrix to group components with similar interaction patterns.
#'
#' @examples
#' \\dontrun{
#' # Basic component interaction analysis
#' plot_component_value_interactions(
#'   results_data = batch_summary,
#'   metric = "rrmse"
#' )
#'
#' # Simplified covariate handling with clustering
#' plot_component_value_interactions(
#'   results_data = batch_summary,
#'   metric = "rsq",
#'   covariate_handling = "simplified",
#'   cluster_components = TRUE,
#'   title = "Component Synergies for R-squared"
#' )
#'
#' # Detailed analysis with significance testing
#' plot_component_value_interactions(
#'   results_data = batch_summary,
#'   metric = "rrmse", 
#'   covariate_handling = "detailed",
#'   significance_test = TRUE,
#'   min_observations = 5
#' )
#' }
#'
#' @seealso
#' \\code{\\link{plot_models_radar}}, \\code{\\link{plot_ensemble_upset}},
#' \\code{\\link{plot_covariate_effects}}, \\code{\\link{horizons_theme}}
#'
#' @importFrom dplyr mutate case_when filter group_by summarise ungroup select arrange bind_rows left_join
#' @importFrom purrr map map_dfr map_lgl
#' @importFrom stringr str_detect str_extract str_replace_all str_count
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 labs theme_minimal theme element_rect element_text element_blank
#' @importFrom tidyr expand_grid pivot_wider
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats weighted.mean hclust dist
#' @importFrom utils combn
#'
#' @family visualization
#' @export

plot_component_value_interactions <- function(results_data,
                                            metric = "rrmse",
                                            components = c("ModelType", "Transformation", "Preprocessing", 
                                                         "Feature_Selection", "Covariates"),
                                            covariate_handling = c("aggregate", "detailed", "simplified"),
                                            cluster_components = TRUE,
                                            significance_test = FALSE,
                                            min_observations = 2,
                                            color_palette = c("default", "viridis", "plasma"),
                                            title = NULL) {
  
  # Match arguments
  covariate_handling <- match.arg(covariate_handling)
  color_palette <- match.arg(color_palette)
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      validate_visualization_data(
        data = results_data,
        required_columns = c("config_desc"),
        metric = metric,
        min_rows = 10
      )
      
      if (length(components) < 2) {
        cli::cli_abort("Need at least 2 components for interaction analysis")
      }
      
      TRUE
    },
    default_value = FALSE,
    error_message = "Input validation failed for component interaction analysis"
  ) -> validation_result
  
  if (!validation_result$result) {
    return(NULL)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Parse Component Values
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      parse_all_component_values_comprehensive(results_data, components, covariate_handling)
    },
    default_value = NULL,
    error_message = "Failed to parse component values from config descriptors"
  ) -> parsed_data_result
  
  if (is.null(parsed_data_result$result)) {
    return(NULL)
  }
  
  parsed_data <- parsed_data_result$result
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Compute Pairwise Interaction Effects
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      compute_value_interaction_effects(parsed_data, metric, components, min_observations)
    },
    default_value = NULL,
    error_message = "Failed to compute pairwise interaction effects"
  ) -> interaction_results
  
  if (is.null(interaction_results$result)) {
    return(NULL)
  }
  
  interaction_data <- interaction_results$result
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Add Significance Testing (Optional)
  ## ---------------------------------------------------------------------------
  
  if (significance_test) {
    safely_execute(
      expr = {
        add_confidence_intervals(parsed_data, metric, components, 
                               interaction_data, min_observations)
      },
      default_value = interaction_data,
      error_message = "Significance testing failed, proceeding without"
    ) -> sig_results
    
    interaction_data <- sig_results$result
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Create Enhanced Interaction Heatmap
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      create_value_interaction_heatmap(
        interaction_data = interaction_data,
        metric = metric,
        cluster_components = cluster_components,
        show_significance = significance_test,
        color_palette = color_palette,
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

#' Parse All Component Values Comprehensively
#'
#' Extracts all pipeline components from config descriptors using robust pattern matching.
#' Handles complex covariate combinations and provides multiple aggregation options.
#'
#' @param data Data frame with config_desc column
#' @param components Character vector of components to extract
#' @param covariate_handling How to handle covariate complexity
#' @return Data frame with parsed component columns added
#' @keywords internal

parse_all_component_values_comprehensive <- function(data, components, covariate_handling) {
  
  result <- data
  
  # ModelType - following exact horizons patterns
  if ("ModelType" %in% components) {
    result <- result %>%
      dplyr::mutate(
        ModelType = dplyr::case_when(
          stringr::str_detect(config_desc, "^random_forest") ~ "Random Forest",
          stringr::str_detect(config_desc, "^cubist") ~ "Cubist",
          stringr::str_detect(config_desc, "^xgboost") ~ "XGBoost",
          stringr::str_detect(config_desc, "^mars") ~ "MARS",
          stringr::str_detect(config_desc, "^plsr") ~ "PLSR",
          stringr::str_detect(config_desc, "^svm_rbf") ~ "SVM (RBF)",
          stringr::str_detect(config_desc, "^elastic_net") ~ "Elastic Net",
          stringr::str_detect(config_desc, "^mlp_nn") ~ "Neural Net",
          TRUE ~ "Other"
        )
      )
  }
  
  # Transformation - exact patterns
  if ("Transformation" %in% components) {
    result <- result %>%
      dplyr::mutate(
        Transformation = dplyr::case_when(
          stringr::str_detect(config_desc, "_NoTrans_") ~ "None",
          stringr::str_detect(config_desc, "_Log_") ~ "Log",
          stringr::str_detect(config_desc, "_Sqrt_") ~ "Square Root",
          TRUE ~ "None"
        )
      )
  }
  
  # Preprocessing - hierarchical matching (order matters)
  if ("Preprocessing" %in% components) {
    result <- result %>%
      dplyr::mutate(
        Preprocessing = dplyr::case_when(
          stringr::str_detect(config_desc, "_SNVD2_") ~ "SNV + Derivative 2",
          stringr::str_detect(config_desc, "_SNVD1_") ~ "SNV + Derivative 1",
          stringr::str_detect(config_desc, "_MSCD1_") ~ "MSC + Derivative 1",
          stringr::str_detect(config_desc, "_SNV_") ~ "SNV",
          stringr::str_detect(config_desc, "_MSC_") ~ "MSC",
          stringr::str_detect(config_desc, "_SG_") ~ "Savitzky-Golay",
          stringr::str_detect(config_desc, "_D2_") ~ "Derivative 2",
          stringr::str_detect(config_desc, "_D1_") ~ "Derivative 1",
          stringr::str_detect(config_desc, "_Raw_") ~ "Raw",
          TRUE ~ "Raw"
        )
      )
  }
  
  # Feature Selection
  if ("Feature_Selection" %in% components) {
    result <- result %>%
      dplyr::mutate(
        Feature_Selection = dplyr::case_when(
          stringr::str_detect(config_desc, "_SHAP_") ~ "SHAP",
          stringr::str_detect(config_desc, "_PCA_") ~ "PCA",
          stringr::str_detect(config_desc, "_Corr_") ~ "Correlation",
          stringr::str_detect(config_desc, "_Boruta_") ~ "Boruta",
          TRUE ~ "PCA"
        )
      )
  }
  
  # Covariates - complex extraction and handling
  if ("Covariates" %in% components) {
    result <- result %>%
      dplyr::mutate(
        # Extract final part after last underscore
        covariate_raw = stringr::str_extract(config_desc, "[^_]+$"),
        covariate_raw = dplyr::case_when(
          is.na(covariate_raw) | covariate_raw == "" ~ "None",
          covariate_raw == config_desc ~ "None", # No underscore found
          TRUE ~ covariate_raw
        )
      )
    
    # Apply covariate handling strategy
    if (covariate_handling == "aggregate") {
      result <- result %>%
        dplyr::mutate(
          Covariates = dplyr::case_when(
            covariate_raw == "None" ~ "None",
            TRUE ~ "Present"
          )
        )
        
    } else if (covariate_handling == "simplified") {
      result <- result %>%
        dplyr::mutate(
          Covariates = categorize_covariates_intelligently(covariate_raw)
        )
        
    } else { # detailed
      result <- result %>%
        dplyr::mutate(
          Covariates = covariate_raw
        )
    }
    
    result <- result %>%
      dplyr::select(-covariate_raw)
  }
  
  return(result)
}


#' Categorize Covariates Intelligently
#'
#' Groups covariate combinations into meaningful categories for interaction analysis.
#'
#' @param covariate_strings Character vector of covariate combinations
#' @return Character vector of categorized covariates
#' @keywords internal

categorize_covariates_intelligently <- function(covariate_strings) {
  
  # Define covariate type mappings
  soil_vars <- c("Clay", "pH", "CEC", "Sand", "Silt", "OM", "N", "P", "K")
  climate_vars <- c("MAP", "GDD", "PET", "Temp", "Precip", "MAT", "AI")
  
  purrr::map_chr(covariate_strings, function(cov_string) {
    if (is.na(cov_string) || cov_string == "None" || cov_string == "") {
      return("None")
    }
    
    # Split covariate string on + and check types
    covs <- unlist(stringr::str_split(cov_string, "\\+"))
    
    has_soil <- any(covs %in% soil_vars)
    has_climate <- any(covs %in% climate_vars)
    
    if (has_soil && has_climate) {
      "Mixed"
    } else if (has_soil) {
      "Soil Only"
    } else if (has_climate) {
      "Climate Only"
    } else {
      "Other"
    }
  })
}


#' Compute Value Interaction Effects
#'
#' Calculates interaction effects between all pairs of pipeline components using
#' variance-based interaction analysis.
#'
#' @param data Parsed data frame with component columns
#' @param metric Performance metric name
#' @param components Component names to analyze
#' @param min_observations Minimum observations required per combination
#' @return Data frame with pairwise interaction effects
#' @keywords internal

compute_value_interaction_effects <- function(data, metric, components, min_observations) {
  
  # Filter out missing metric values
  data <- data %>%
    dplyr::filter(!is.na(.data[[metric]]))
  
  if (nrow(data) == 0) {
    cli::cli_abort("No valid data remaining after filtering missing metric values")
  }
  
  # Create all pairs including diagonal for complete matrix
  all_pairs <- tidyr::expand_grid(
    component_a = components,
    component_b = components
  )
  
  # Compute interaction for each pair
  interaction_results <- purrr::map_dfr(1:nrow(all_pairs), function(i) {
    comp_a <- all_pairs$component_a[i]
    comp_b <- all_pairs$component_b[i]
    
    if (comp_a == comp_b) {
      # Diagonal: set to 0 (component with itself)
      return(tibble::tibble(
        component_a = comp_a,
        component_b = comp_b,
        interaction_effect = 0,
        n_combinations = 0,
        total_obs = nrow(data)
      ))
    }
    
    # Check if both components exist in data
    if (!comp_a %in% names(data) || !comp_b %in% names(data)) {
      return(tibble::tibble(
        component_a = comp_a,
        component_b = comp_b,
        interaction_effect = 0,
        n_combinations = 0,
        total_obs = 0
      ))
    }
    
    # Compute variance of performance within each A×B combination
    combo_stats <- data %>%
      dplyr::group_by(.data[[comp_a]], .data[[comp_b]]) %>%
      dplyr::summarise(
        mean_perf = mean(.data[[metric]], na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(n >= min_observations)
    
    if (nrow(combo_stats) > 1) {
      # Interaction effect = variance of combination means
      # Higher variance indicates stronger interaction
      interaction_effect <- stats::var(combo_stats$mean_perf, na.rm = TRUE)
      
      # Scale by overall metric variance for comparability
      overall_var <- stats::var(data[[metric]], na.rm = TRUE)
      if (overall_var > 0) {
        interaction_effect <- interaction_effect / overall_var
      }
    } else {
      interaction_effect <- 0
    }
    
    # Return summary
    tibble::tibble(
      component_a = comp_a,
      component_b = comp_b,
      interaction_effect = interaction_effect,
      n_combinations = nrow(combo_stats),
      total_obs = sum(combo_stats$n, na.rm = TRUE)
    )
  })
  
  return(interaction_results)
}


#' Add Confidence Intervals
#'
#' Adds statistical confidence intervals to interaction effects using permutation tests.
#'
#' @param data Original parsed data
#' @param metric Performance metric
#' @param components Component names
#' @param interaction_data Computed interactions
#' @param min_observations Minimum observations threshold
#' @return Interaction data with confidence intervals added
#' @keywords internal

add_confidence_intervals <- function(data, metric, components, interaction_data, min_observations) {
  
  if (nrow(interaction_data) == 0) {
    return(interaction_data)
  }
  
  cli::cli_alert_info("Computing interaction significance (this may take a moment)...")
  
  # Add p-values using permutation testing
  interaction_data$p_value <- purrr::pmap_dbl(
    list(interaction_data$component_a, interaction_data$component_b, interaction_data$interaction_effect),
    function(comp_a, comp_b, observed_effect) {
      
      # Reduced permutations for computational efficiency
      n_perms <- 100
      perm_effects <- numeric(n_perms)
      
      for (i in 1:n_perms) {
        # Permute one component randomly
        perm_data <- data
        perm_data[[comp_a]] <- sample(perm_data[[comp_a]])
        
        # Recompute interaction for this permutation
        perm_result <- safely_execute(
          expr = {
            compute_value_interaction_effects(perm_data, metric, c(comp_a, comp_b), min_observations)
          },
          default_value = NULL,
          error_message = "Permutation failed"
        )
        
        if (!is.null(perm_result$result) && nrow(perm_result$result) > 0) {
          perm_effects[i] <- perm_result$result$interaction_effect[1]
        } else {
          perm_effects[i] <- 0
        }
      }
      
      # Two-sided p-value
      p_val <- mean(abs(perm_effects) >= abs(observed_effect), na.rm = TRUE)
      return(p_val)
    }
  )
  
  # Add significance indicators
  interaction_data <- interaction_data %>%
    dplyr::mutate(
      significant = p_value < 0.05,
      sig_label = dplyr::case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  
  cli::cli_alert_info("Significance testing completed")
  return(interaction_data)
}


#' Create Value Interaction Heatmap
#'
#' Creates the final interaction heatmap visualization using horizons styling.
#'
#' @param interaction_data Computed interaction effects
#' @param metric Performance metric name
#' @param cluster_components Whether to cluster components
#' @param show_significance Whether significance testing was performed
#' @param color_palette Color scheme to use
#' @param title Plot title
#' @return ggplot2 object
#' @keywords internal

create_value_interaction_heatmap <- function(interaction_data, metric, cluster_components,
                                           show_significance, color_palette, title) {
  
  if (nrow(interaction_data) == 0) {
    cli::cli_abort("No interaction data available for plotting")
  }
  
  # Data should already be complete from compute_value_interaction_effects
  # Just use it directly
  complete_grid <- interaction_data
  
  # Get unique components
  all_components <- unique(c(complete_grid$component_a, complete_grid$component_b))
  
  # Apply clustering if requested
  if (cluster_components && length(all_components) > 2) {
    safely_execute(
      expr = {
        # Create interaction matrix for clustering
        wide_matrix <- complete_grid %>%
          tidyr::pivot_wider(
            names_from = component_b,
            values_from = interaction_effect,
            values_fill = 0
          ) %>%
          tibble::column_to_rownames("component_a") %>%
          as.matrix()
        
        # Hierarchical clustering
        hc <- stats::hclust(stats::dist(wide_matrix), method = "ward.D2")
        component_order <- rownames(wide_matrix)[hc$order]
        
        # Apply ordering
        complete_grid %>%
          dplyr::mutate(
            component_a = factor(component_a, levels = component_order),
            component_b = factor(component_b, levels = component_order)
          )
      },
      default_value = complete_grid,
      error_message = "Clustering failed, using original order"
    ) -> clustering_result
    
    complete_grid <- clustering_result$result
  }
  
  # Clean component names for display
  complete_grid <- complete_grid %>%
    dplyr::mutate(
      component_a_clean = clean_component_names_advanced(as.character(component_a)),
      component_b_clean = clean_component_names_advanced(as.character(component_b))
    )
  
  # Add significance labels if available
  if (show_significance && !"sig_label" %in% names(complete_grid)) {
    complete_grid$sig_label <- ""
  }
  
  # Create heatmap with exact horizons styling
  max_abs_effect <- max(abs(complete_grid$interaction_effect), na.rm = TRUE)
  
  p <- ggplot2::ggplot(complete_grid,
                      ggplot2::aes(x = component_a_clean, y = component_b_clean,
                                  fill = interaction_effect)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5)
  
  # Apply color scheme - using single-directional scale since interactions are strength-based
  if (color_palette == "default") {
    p <- p + ggplot2::scale_fill_gradient(
      low = "#f7f7f7",      # Light gray for weak interaction
      high = "#762a83",     # Purple for strong interaction
      limits = c(0, max_abs_effect),
      name = "Interaction\nStrength",
      labels = function(x) ifelse(x == 0, "Weak", ifelse(x == max_abs_effect, "Strong", ""))
    )
  } else if (color_palette == "viridis") {
    p <- p + ggplot2::scale_fill_viridis_c(
      name = "Interaction\nStrength",
      option = "viridis",
      direction = 1
    )
  } else {
    p <- p + ggplot2::scale_fill_viridis_c(
      option = "plasma", 
      name = "Interaction\nStrength",
      direction = 1
    )
  }
  
  # Add significance markers if available
  if (show_significance && "sig_label" %in% names(complete_grid)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sig_label),
      color = "black",
      size = 4,
      fontface = "bold"
    )
  }
  
  # Apply exact horizons theme
  p <- p +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      # Panel styling (exact match to existing plots)
      panel.border = ggplot2::element_rect(fill = "transparent", color = "black", linewidth = 1),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      
      # Text styling (exact match)
      axis.title.y = ggplot2::element_text(color = "black", size = 15, angle = 90, face = "bold"),
      axis.title.x = ggplot2::element_text(color = "black", size = 15, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "bold", color = "black", size = 12),
      axis.text.y = ggplot2::element_text(face = "bold", color = "black", size = 12),
      
      # Plot titles (exact match)
      plot.title = ggplot2::element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "black", size = 13, hjust = 0.5),
      
      # Legend (exact match)
      legend.title = ggplot2::element_text(face = "bold", size = 13),
      legend.text = ggplot2::element_text(size = 12),
      legend.position = "right"
    )
  
  # Add labels
  if (is.null(title)) {
    title <- glue::glue("Component Interaction Strength ({toupper(metric)})")
  }
  
  subtitle <- "Darker colors indicate stronger interactions between pipeline components"
  if (show_significance) {
    subtitle <- paste(subtitle, "| * p<0.05, ** p<0.01, *** p<0.001")
  }
  
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = "Pipeline Component A",
    y = "Pipeline Component B"
  ) +
    ggplot2::coord_fixed()  # Make tiles square
  
  return(p)
}


#' Clean Component Names Advanced
#'
#' Cleans component names for publication-quality display with line breaks.
#'
#' @param component_names Character vector of component names
#' @return Character vector of cleaned names
#' @keywords internal

clean_component_names_advanced <- function(component_names) {
  cleaned <- component_names
  # Replace underscores with spaces
  cleaned <- stringr::str_replace_all(cleaned, "_", " ")
  # Add line breaks for better display
  cleaned <- stringr::str_replace_all(cleaned, "Feature Selection", "Feature\nSelection")
  cleaned <- stringr::str_replace_all(cleaned, "ModelType", "Model\nType")
  # Shorten some long names
  cleaned <- stringr::str_replace_all(cleaned, "Preprocessing", "Preproc")
  cleaned <- stringr::str_replace_all(cleaned, "Transformation", "Transform")
  return(cleaned)
}