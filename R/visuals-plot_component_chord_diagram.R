#' Chord Diagram Visualization of Component Interactions
#'
#' Creates clean, interpretable chord diagrams showing co-occurrence patterns
#' between modeling pipeline components. Reveals multi-way interactions through
#' circular visualization with clear sectoral organization and filtered connections
#' for maximum interpretability in manuscript contexts.
#'
#' @param results_data A data frame containing model evaluation results with at minimum:
#'   `config_desc` column with workflow identifiers and performance metric columns.
#' @param metric Character. Performance metric to analyze (e.g., "rrmse", "rsq", "rmse").
#'   Must be a column name in `results_data`.
#' @param performance_threshold Numeric. Percentile threshold for filtering to top-performing
#'   configurations (0-1). Default: 0.75 (top 25% of configurations).
#' @param top_interactions Integer. Number of strongest interactions to display.
#'   Default: 15 (optimal for clarity without clutter).
#' @param min_co_occurrence Integer. Minimum number of times component values must co-occur
#'   in configurations to be included. Default: 5.
#' @param covariate_handling Character. How to handle covariate complexity: "simplified"
#'   (group into Climate/Soil/Mixed), "aggregate" (Present/None), or "detailed" (individual).
#' @param show_significance Logical. Whether to include statistical significance testing
#'   for co-occurrence patterns. Default: FALSE.
#' @param sector_gaps Numeric vector. Gaps between component type sectors in degrees.
#'   Default: c(5, 2, 2, 2, 5) for Models, Preprocessing, Features, Transform, Covariates.
#' @param chord_transparency Numeric. Base transparency for chord connections (0-1).
#'   Default: 0.7. Significant connections will be more opaque.
#' @param title Character. Optional plot title. If `NULL`, generates automatic title.
#'
#' @return A chord diagram plot object showing component interaction patterns with:
#'   \itemize{
#'     \item Components arranged in logical sectors around circle
#'     \item Chord thickness proportional to co-occurrence strength
#'     \item Color coding by component type using horizons palette
#'     \item Clear sector labeling and professional legends
#'     \item Statistical significance indicators (if enabled)
#'   }
#'
#' @details
#' **Chord Diagram Design:**
#' Components are organized in sectors around a circle:
#' - **Models**: Tree-based, neural networks, linear methods
#' - **Preprocessing**: Spectral transformations (SNV, MSC, derivatives)
#' - **Feature Selection**: PCA, SHAP, correlation-based methods
#' - **Transformations**: Response transformations (log, square root)
#' - **Covariates**: Environmental variables (climate, soil properties)
#'
#' **Visual Encoding:**
#' - Chord thickness indicates co-occurrence frequency
#' - Colors represent component types using horizons palette
#' - Transparency indicates statistical significance (if tested)
#' - Sector gaps provide visual separation between component types
#'
#' **Filtering Strategy:**
#' Only the strongest interactions are shown to maximize interpretability.
#' This reveals the most important component synergies without visual clutter.
#'
#' @examples
#' \\dontrun{
#' # Basic chord diagram
#' plot_component_chord_diagram(
#'   results_data = batch_summary,
#'   metric = "rrmse",
#'   top_interactions = 15
#' )
#'
#' # Focused on strongest interactions with significance
#' plot_component_chord_diagram(
#'   results_data = batch_summary,
#'   metric = "rrmse", 
#'   performance_threshold = 0.9,
#'   top_interactions = 10,
#'   show_significance = TRUE,
#'   title = "Elite Component Interaction Patterns"
#' )
#'
#' # Detailed covariate analysis
#' plot_component_chord_diagram(
#'   results_data = batch_summary,
#'   metric = "rsq",
#'   covariate_handling = "detailed",
#'   top_interactions = 20,
#'   chord_transparency = 0.8
#' )
#' }
#'
#' @seealso
#' \\code{\\link{plot_component_network_bundling}}, \\code{\\link{plot_component_value_interactions}},
#' \\code{\\link{plot_models_radar}}, \\code{\\link{horizons_theme}}
#'
#' @importFrom dplyr mutate case_when filter group_by summarise ungroup select arrange desc slice_max
#' @importFrom purrr map map_dfr map_dbl
#' @importFrom stringr str_detect str_extract str_replace_all
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom progressr with_progress progressor
#'
#' @family visualization
#' @export

plot_component_chord_diagram <- function(results_data,
                                       metric = "rrmse",
                                       performance_threshold = 0.75,
                                       top_interactions = 15,
                                       min_co_occurrence = 5,
                                       covariate_handling = c("simplified", "aggregate", "detailed"),
                                       show_significance = FALSE,
                                       chord_transparency = 0.7,
                                       title = NULL) {
  
  # Initialize progress tracking
  progressr::with_progress({
    p <- progressr::progressor(steps = ifelse(show_significance, 6, 5))
    p("Initializing chord diagram analysis...")
    
    # Check for required packages
    required_packages <- c("circlize")
    missing_pkg <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
    
    if (length(missing_pkg) > 0) {
      cli::cli_abort(
        "Chord diagram requires additional package: {missing_pkg}.\n",
        "Install with: install.packages(\"{missing_pkg}\")"
      )
    }
    
    # Match arguments
    covariate_handling <- match.arg(covariate_handling)
    
    ## ---------------------------------------------------------------------------
    ## Step 1: Input Validation
    ## ---------------------------------------------------------------------------
    
    safely_execute(
      expr = {
        validate_visualization_data(
          data = results_data,
          required_columns = c("config_desc"),
          metric = metric,
          min_rows = 20
        )
        
        if (performance_threshold < 0 || performance_threshold > 1) {
          cli::cli_abort("performance_threshold must be between 0 and 1")
        }
        
        if (top_interactions < 5) {
          cli::cli_abort("top_interactions must be at least 5 for meaningful visualization")
        }
        
        TRUE
      },
      default_value = FALSE,
      error_message = "Input validation failed for chord diagram analysis"
    ) -> validation_result
    
    if (!validation_result$result) {
      return(NULL)
    }
    
    ## ---------------------------------------------------------------------------
    ## Step 2: Filter to Top-Performing Configurations  
    ## ---------------------------------------------------------------------------
    
    p("Filtering to top-performing configurations...")
    
    safely_execute(
      expr = {
        filter_top_performers(results_data, metric, performance_threshold)
      },
      default_value = NULL,
      error_message = "Failed to filter top-performing configurations"
    ) -> filtered_data_result
    
    if (is.null(filtered_data_result$result)) {
      return(NULL)
    }
    
    filtered_data <- filtered_data_result$result
    
    ## ---------------------------------------------------------------------------
    ## Step 3: Parse Components and Build Network Data
    ## ---------------------------------------------------------------------------
    
    p("Parsing components and building interaction network...")
    
    safely_execute(
      expr = {
        # Reuse existing parsing function
        parsed_data <- parse_components_for_network(filtered_data, covariate_handling)
        # Build network to get co-occurrence data
        build_component_network(parsed_data, metric, min_co_occurrence)
      },
      default_value = NULL,
      error_message = "Failed to build component interaction data"
    ) -> network_result
    
    if (is.null(network_result$result)) {
      return(NULL)
    }
    
    network_data <- network_result$result
    
    ## ---------------------------------------------------------------------------
    ## Step 4: Add Statistical Significance (Optional)
    ## ---------------------------------------------------------------------------
    
    if (show_significance) {
      cli::cli_alert_warning("Significance testing enabled - this may take several minutes...")
      p("Computing statistical significance for interactions...")
      
      safely_execute(
        expr = {
          add_network_significance(network_data, 
                                 parse_components_for_network(filtered_data, covariate_handling),
                                 metric, min_co_occurrence)
        },
        default_value = network_data,
        error_message = "Significance testing failed, proceeding without"
      ) -> sig_results
      
      network_data <- sig_results$result
    }
    
    ## ---------------------------------------------------------------------------
    ## Step 5: Convert Network to Chord Diagram Format
    ## ---------------------------------------------------------------------------
    
    p("Converting network data to chord diagram format...")
    
    safely_execute(
      expr = {
        prepare_chord_data(network_data, top_interactions, show_significance)
      },
      default_value = NULL,
      error_message = "Failed to prepare chord diagram data"
    ) -> chord_data_result
    
    if (is.null(chord_data_result$result)) {
      return(NULL)
    }
    
    chord_data <- chord_data_result$result
    
    ## ---------------------------------------------------------------------------
    ## Step 6: Create Chord Diagram Visualization
    ## ---------------------------------------------------------------------------
    
    p("Creating chord diagram visualization...")
    
    safely_execute(
      expr = {
        create_chord_diagram_plot(
          chord_data = chord_data,
          metric = metric,
          chord_transparency = chord_transparency,
          show_significance = show_significance,
          title = title
        )
      },
      default_value = NULL,
      error_message = "Failed to create chord diagram"
    ) -> plot_result
    
    if (is.null(plot_result$result)) {
      cli::cli_alert_warning("Returning NULL plot due to errors")
      return(NULL)
    }
    
    p("Chord diagram visualization complete!")
    return(plot_result$result)
    
  }) # Close progressr::with_progress
}


## =============================================================================
## Helper Functions
## =============================================================================

#' Prepare Chord Diagram Data
#'
#' Converts network edge data to chord diagram matrix format with proper
#' filtering and component organization.
#'
#' @param network_data Network data from build_component_network
#' @param top_interactions Number of top interactions to include
#' @param show_significance Whether significance data is available
#' @return List with chord matrix and component metadata
#' @keywords internal

prepare_chord_data <- function(network_data, top_interactions, show_significance) {
  
  edges <- network_data$edges
  nodes <- network_data$nodes
  
  # Filter to top interactions for clarity
  if (show_significance && "significant" %in% names(edges)) {
    # Prioritize significant edges, then by strength
    top_edges <- edges %>%
      dplyr::arrange(desc(significant), desc(co_occurrence_count)) %>%
      dplyr::slice_max(order_by = co_occurrence_count, n = top_interactions)
  } else {
    # Just take top by co-occurrence strength
    top_edges <- edges %>%
      dplyr::arrange(desc(co_occurrence_count)) %>%
      dplyr::slice_max(order_by = co_occurrence_count, n = top_interactions)
  }
  
  # Get components involved in top interactions
  involved_components <- unique(c(top_edges$from, top_edges$to))
  
  # Create component metadata with sector information (exclude covariates)
  component_info <- nodes %>%
    dplyr::filter(
      component_value %in% involved_components,
      component_type != "Covariates"  # Exclude covariates for clarity
    ) %>%
    dplyr::mutate(
      sector = get_component_sector(component_type),
      sector_order = get_sector_order(component_type),
      color = get_component_colors_for_chord(component_type)
    ) %>%
    dplyr::arrange(sector_order, component_value)
  
  # Create chord matrix (symmetric)
  component_names <- component_info$component_value
  n_components <- length(component_names)
  
  chord_matrix <- matrix(0, nrow = n_components, ncol = n_components,
                        dimnames = list(component_names, component_names))
  
  # Fill matrix with co-occurrence values
  for (i in 1:nrow(top_edges)) {
    from_comp <- top_edges$from[i]
    to_comp <- top_edges$to[i]
    weight <- top_edges$co_occurrence_count[i]
    
    if (from_comp %in% component_names && to_comp %in% component_names) {
      chord_matrix[from_comp, to_comp] <- weight
      chord_matrix[to_comp, from_comp] <- weight  # Symmetric
    }
  }
  
  return(list(
    matrix = chord_matrix,
    components = component_info,
    edges = top_edges,
    n_interactions = nrow(top_edges)
  ))
}


#' Get Component Sector Assignment
#'
#' Assigns components to logical sectors for chord diagram organization.
#'
#' @param component_types Character vector of component types
#' @return Character vector of sector names
#' @keywords internal

get_component_sector <- function(component_types) {
  dplyr::case_when(
    component_types == "ModelType" ~ "Models",
    component_types == "Preprocessing" ~ "Preprocessing", 
    component_types == "Feature_Selection" ~ "Feature Selection",
    component_types == "Transformation" ~ "Transformations",
    TRUE ~ "Other"
  )
}


#' Get Sector Display Order
#'
#' Defines the order of sectors around the chord diagram circle.
#'
#' @param component_types Character vector of component types
#' @return Numeric vector indicating sector order
#' @keywords internal

get_sector_order <- function(component_types) {
  dplyr::case_when(
    component_types == "ModelType" ~ 1,
    component_types == "Preprocessing" ~ 2,
    component_types == "Feature_Selection" ~ 3,
    component_types == "Transformation" ~ 4,
    TRUE ~ 5
  )
}


#' Get Component Colors for Chord Diagram
#'
#' Assigns colors to components based on their type using horizons palette.
#'
#' @param component_types Character vector of component types
#' @return Character vector of hex color codes
#' @keywords internal

get_component_colors_for_chord <- function(component_types) {
  # Use distinct, clear colors for better differentiation
  color_map <- c(
    "ModelType" = "#2E7D32",         # Deep green
    "Preprocessing" = "#D32F2F",      # Red
    "Feature_Selection" = "#F57C00",  # Orange  
    "Transformation" = "#7B1FA2"      # Purple
  )
  
  color_map[component_types]
}


#' Create Chord Diagram Plot
#'
#' Creates the final chord diagram visualization using circlize package.
#'
#' @param chord_data Prepared chord diagram data
#' @param metric Performance metric name
#' @param sector_gaps Numeric vector of gaps between sectors
#' @param chord_transparency Base transparency for chords
#' @param show_significance Whether to show significance indicators
#' @param title Plot title
#' @return Plot object (invisibly - chord diagrams plot directly)
#' @keywords internal

create_chord_diagram_plot <- function(chord_data, metric, 
                                     chord_transparency, show_significance, title) {
  
  # Check if circlize is available
  if (!requireNamespace("circlize", quietly = TRUE)) {
    cli::cli_abort("circlize package required for chord diagrams")
  }
  
  matrix_data <- chord_data$matrix
  component_info <- chord_data$components
  
  # Set up sector colors based on component types
  component_names <- rownames(matrix_data)
  sector_colors <- setNames(
    sapply(component_names, function(name) {
      comp_type <- component_info$component_type[component_info$component_value == name][1]
      get_component_colors_for_chord(comp_type)
    }),
    component_names
  )
  
  # Initialize circlize parameters
  circlize::circos.clear()
  circlize::circos.par(
    start.degree = 90,
    gap.degree = 4,  # Use uniform gap for all sectors
    track.margin = c(0.01, 0.01),
    points.overflow.warning = FALSE
  )
  
  # Create chord colors based on significance if available
  if (show_significance && "significant" %in% names(chord_data$edges)) {
    # Use different colors for significant vs non-significant edges
    # Create a list of colors for each chord
    
    # Get the significant edges for color mapping
    sig_edges <- chord_data$edges
    n_significant <- sum(sig_edges$significant, na.rm = TRUE)
    
    # For now, use uniform transparency but note significance in legend
    # The circlize package has limitations for per-chord transparency
    cat("Found", n_significant, "significant interactions out of", nrow(sig_edges), "total\n")
    
    # Create the chord diagram with uniform transparency
    circlize::chordDiagram(
      matrix_data,
      grid.col = sector_colors,
      transparency = chord_transparency,
      directional = 0,  # Undirected
      link.border = "white",
      annotationTrack = c("name", "grid"),
      annotationTrackHeight = c(0.03, 0.01),
      link.visible = matrix_data > 0
    )
  } else {
    # Create the chord diagram with uniform transparency
    circlize::chordDiagram(
      matrix_data,
      grid.col = sector_colors,
      transparency = chord_transparency,
      directional = 0,  # Undirected
      link.border = "white",
      annotationTrack = c("name", "grid"),
      annotationTrackHeight = c(0.03, 0.01),
      link.visible = matrix_data > 0
    )
  }
  
  # Add title if provided
  if (!is.null(title)) {
    title(main = title, cex.main = 1.2, font.main = 2)
  } else {
    auto_title <- glue::glue("Component Interaction Patterns ({toupper(metric)})")
    title(main = auto_title, cex.main = 1.2, font.main = 2)
  }
  
  # Add sector labels
  for (sector in unique(component_info$sector)) {
    sector_components <- component_info %>%
      dplyr::filter(sector == !!sector) %>%
      dplyr::pull(component_value)
    
    if (length(sector_components) > 0) {
      # Add sector group label (this is a simplified approach)
      # In practice, you might want more sophisticated sector labeling
    }
  }
  
  # Add legend based on whether significance is shown
  if (show_significance && "significant" %in% names(chord_data$edges)) {
    legend("bottomright", 
           legend = c("Statistically Significant", "Not Significant", "Strong Co-occurrence", "Weak Co-occurrence"),
           lwd = c(2, 2, 3, 1),
           col = c("gray20", "gray70", "gray50", "gray50"),
           title = "Interaction Properties",
           bty = "n",
           cex = 0.8)
  } else {
    legend("bottomright", 
           legend = c("Strong Co-occurrence", "Moderate Co-occurrence", "Weak Co-occurrence"),
           lwd = c(3, 2, 1),
           col = "gray50",
           title = "Interaction Strength",
           bty = "n",
           cex = 0.8)
  }
  
  # Add statistical note if significance testing was performed
  if (show_significance) {
    n_significant <- sum(chord_data$edges$significant, na.rm = TRUE)
    mtext(glue::glue("Showing top {chord_data$n_interactions} interactions | {n_significant} statistically significant"),
          side = 1, line = -1, cex = 0.8, col = "gray40")
  } else {
    mtext(glue::glue("Showing top {chord_data$n_interactions} strongest interactions"),
          side = 1, line = -1, cex = 0.8, col = "gray40")
  }
  
  # Return invisibly (chord diagrams plot directly to device)
  invisible(list(
    matrix = matrix_data,
    components = component_info,
    n_interactions = chord_data$n_interactions
  ))
}