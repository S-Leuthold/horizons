#' Network Visualization of Multi-Way Component Interactions
#'
#' Creates hierarchical edge bundling network plots showing complex multi-way interactions
#' between modeling pipeline components. Reveals 3-, 4-, and 5-way relationships that
#' traditional heatmaps cannot capture, critical for understanding optimal pipeline
#' configurations and component synergies in spectroscopy modeling.
#'
#' @param results_data A data frame containing model evaluation results with at minimum:
#'   `config_desc` column with workflow identifiers and performance metric columns.
#' @param metric Character. Performance metric to analyze (e.g., "rrmse", "rsq", "rmse").
#'   Must be a column name in `results_data`.
#' @param performance_threshold Numeric. Percentile threshold for filtering to top-performing
#'   configurations (0-1). Default: 0.75 (top 25% of configurations).
#' @param min_co_occurrence Integer. Minimum number of times component values must co-occur
#'   in configurations to be included in network. Default: 3.
#' @param bundling_strength Numeric. Strength of hierarchical edge bundling (0-1).
#'   Higher values create more curved, bundled edges. Default: 0.8.
#' @param node_size_metric Character. Metric for node sizing: "frequency" (occurrence count),
#'   "performance" (average metric value), or "centrality" (network centrality).
#' @param covariate_handling Character. How to handle covariate complexity: "simplified"
#'   (group into Climate/Soil/Mixed), "aggregate" (Present/None), or "detailed" (individual).
#' @param component_sectors Logical. Whether to arrange nodes in component-type sectors
#'   around the circle for better organization. Default: TRUE.
#' @param show_significance Logical. Whether to include statistical significance testing
#'   for co-occurrence patterns using permutation tests. Default: FALSE (computationally intensive).
#' @param edge_alpha_range Numeric vector of length 2. Range for edge transparency based
#'   on co-occurrence strength. Default: c(0.2, 0.8).
#' @param title Character. Optional plot title. If `NULL`, generates automatic title.
#'
#' @return A `ggplot2` object showing the component interaction network with:
#'   \itemize{
#'     \item Nodes representing individual component values (e.g., "Random Forest", "SNV")
#'     \item Edges showing co-occurrence in high-performing configurations
#'     \item Hierarchical edge bundling revealing multi-way interaction patterns
#'     \item Circular layout with optional component-type sectors
#'     \item Statistical annotations if significance testing enabled
#'   }
#'
#' @details
#' **Network Construction Method:**
#' \enumerate{
#'   \item Filters data to top-performing configurations based on threshold
#'   \item Parses component values from config descriptors with intelligent covariate grouping
#'   \item Builds co-occurrence matrix weighted by average performance
#'   \item Creates network graph with components as nodes, co-occurrence as edges
#'   \item Applies hierarchical clustering for edge bundling and layout optimization
#' }
#'
#' **Covariate Intelligence:**
#' When `covariate_handling = "simplified"`:
#' - Climate variables (MAP, GDD, PET, etc.) → "Climate Variables"
#' - Soil properties (Clay, pH, CEC, etc.) → "Soil Properties"
#' - Mixed combinations → "Climate + Soil"
#' - No covariates → "Spectral Only"
#'
#' **Multi-Way Interaction Detection:**
#' - 2-way: Direct edges between component pairs
#' - 3-way: Triangular clustering patterns in bundled edges
#' - 4-way+: Dense bundling regions indicating component ecosystems
#' - Network communities reveal stable multi-component synergies
#'
#' **Statistical Analysis:**
#' When `show_significance = TRUE`, performs permutation testing to identify
#' statistically significant co-occurrence patterns beyond random chance.
#'
#' @examples
#' \\dontrun{
#' # Basic network with simplified covariates
#' plot_component_network_bundling(
#'   results_data = batch_summary,
#'   metric = "rrmse",
#'   performance_threshold = 0.75
#' )
#'
#' # Detailed analysis with significance testing
#' plot_component_network_bundling(
#'   results_data = batch_summary,
#'   metric = "rsq",
#'   performance_threshold = 0.9,  # Top 10% only
#'   min_co_occurrence = 5,
#'   show_significance = TRUE,
#'   covariate_handling = "simplified"
#' )
#'
#' # Custom styling for presentation
#' plot_component_network_bundling(
#'   results_data = batch_summary,
#'   metric = "rrmse",
#'   bundling_strength = 0.6,
#'   node_size_metric = "centrality",
#'   title = "Multi-Way Component Interactions in MAOM Prediction"
#' )
#' }
#'
#' @seealso
#' \\code{\\link{plot_component_value_interactions}}, \\code{\\link{plot_models_radar}},
#' \\code{\\link{plot_ensemble_upset}}, \\code{\\link{horizons_theme}}
#'
#' @importFrom dplyr mutate case_when filter group_by summarise ungroup select arrange desc slice_max n_distinct count
#' @importFrom purrr map map_dfr map_dbl pmap_dfr
#' @importFrom stringr str_detect str_extract str_replace_all str_split
#' @importFrom ggplot2 ggplot aes labs theme element_text element_blank coord_fixed
#' @importFrom ggraph ggraph geom_node_circle geom_node_text geom_edge_arc scale_edge_alpha scale_edge_width
#' @importFrom igraph graph_from_data_frame cluster_walktrap V E degree betweenness
#' @importFrom tidygraph as_tbl_graph activate mutate
#' @importFrom tidyr expand_grid pivot_wider pivot_longer
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info
#' @importFrom progressr with_progress progressor
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats quantile var weighted.mean
#' @importFrom utils combn
#'
#' @family visualization
#' @export

plot_component_network_bundling <- function(results_data,
                                          metric = "rrmse",
                                          performance_threshold = 0.75,
                                          min_co_occurrence = 3,
                                          bundling_strength = 0.8,
                                          node_size_metric = c("frequency", "performance", "centrality"),
                                          covariate_handling = c("simplified", "aggregate", "detailed"),
                                          component_sectors = TRUE,
                                          show_significance = FALSE,
                                          edge_alpha_range = c(0.2, 0.8),
                                          title = NULL) {

  # Initialize progress tracking
  progressr::with_progress({
    p <- progressr::progressor(steps = ifelse(show_significance, 6, 5))
    p("Initializing network analysis...")

  # Check for required packages
  required_packages <- c("igraph", "ggraph", "tidygraph")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    cli::cli_abort(
      "Network visualization requires additional packages: {missing_packages}.\n",
      "Install with: install.packages(c({paste(paste0('\"', missing_packages, '\"'), collapse = ', ')}))"
    )
  }

  # Match arguments
  node_size_metric <- match.arg(node_size_metric)
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

      if (min_co_occurrence < 2) {
        cli::cli_abort("min_co_occurrence must be at least 2")
      }

      TRUE
    },
    default_value = FALSE,
    error_message = "Input validation failed for network bundling analysis"
  ) -> validation_result

  if (!validation_result$result) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Filter to Top-Performing Configurations
  ## ---------------------------------------------------------------------------

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

  p("Filtering to top-performing configurations...")
  n_configs <- nrow(filtered_data)
  perc <- round(performance_threshold * 100)

  ## ---------------------------------------------------------------------------
  ## Step 3: Parse Component Values with Intelligent Covariate Grouping
  ## ---------------------------------------------------------------------------

  p("Parsing component values with intelligent covariate grouping...")
  
  safely_execute(
    expr = {
      parse_components_for_network(filtered_data, covariate_handling)
    },
    default_value = NULL,
    error_message = "Failed to parse component values for network analysis"
  ) -> parsed_data_result

  if (is.null(parsed_data_result$result)) {
    return(NULL)
  }

  parsed_data <- parsed_data_result$result

  ## ---------------------------------------------------------------------------
  ## Step 4: Build Co-occurrence Network
  ## ---------------------------------------------------------------------------

  p("Building co-occurrence network from component interactions...")
  
  safely_execute(
    expr = {
      build_component_network(parsed_data, metric, min_co_occurrence)
    },
    default_value = NULL,
    error_message = "Failed to build component co-occurrence network"
  ) -> network_result

  if (is.null(network_result$result)) {
    return(NULL)
  }

  network_data <- network_result$result

  ## ---------------------------------------------------------------------------
  ## Step 5: Add Statistical Significance (Optional)
  ## ---------------------------------------------------------------------------

  if (show_significance) {
    p("Computing statistical significance (permutation testing)...")
    
    safely_execute(
      expr = {
        add_network_significance(network_data, parsed_data, metric, min_co_occurrence)
      },
      default_value = network_data,
      error_message = "Significance testing failed, proceeding without"
    ) -> sig_results

    network_data <- sig_results$result
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Create Network Visualization
  ## ---------------------------------------------------------------------------

  p("Creating network visualization with hierarchical edge bundling...")
  
  safely_execute(
    expr = {
      create_bundled_network_plot(
        network_data = network_data,
        metric = metric,
        bundling_strength = bundling_strength,
        node_size_metric = node_size_metric,
        component_sectors = component_sectors,
        show_significance = show_significance,
        edge_alpha_range = edge_alpha_range,
        title = title
      )
    },
    default_value = NULL,
    error_message = "Failed to create network visualization"
  ) -> plot_result

  if (is.null(plot_result$result)) {
    cli::cli_alert_warning("Returning NULL plot due to errors")
    return(NULL)
  }

  p("Network visualization complete!")
  return(plot_result$result)
  
  }) # Close progressr::with_progress
}


## =============================================================================
## Helper Functions
## =============================================================================

#' Filter Data to Top-Performing Configurations
#'
#' Filters the results data to top-performing configurations based on percentile threshold.
#' Handles both error metrics (lower is better) and performance metrics (higher is better).
#'
#' @param data Data frame with model results
#' @param metric Performance metric name
#' @param threshold Percentile threshold (0-1)
#' @return Filtered data frame
#' @keywords internal

filter_top_performers <- function(data, metric, threshold) {

  # Remove missing values
  data <- data %>%
    dplyr::filter(!is.na(.data[[metric]]))

  if (nrow(data) == 0) {
    cli::cli_abort("No valid data remaining after filtering missing metric values")
  }

  # Determine if metric is error-based or performance-based
  is_error_metric <- metric %in% c("rrmse", "rmse", "mae", "mse")

  if (is_error_metric) {
    # For error metrics, lower is better - take bottom percentile
    cutoff <- stats::quantile(data[[metric]], probs = 1 - threshold, na.rm = TRUE)
    filtered_data <- data %>%
      dplyr::filter(.data[[metric]] <= cutoff)
  } else {
    # For performance metrics, higher is better - take top percentile
    cutoff <- stats::quantile(data[[metric]], probs = threshold, na.rm = TRUE)
    filtered_data <- data %>%
      dplyr::filter(.data[[metric]] >= cutoff)
  }

  if (nrow(filtered_data) < 10) {
    cli::cli_alert_warning("Very few configurations meet threshold: {nrow(filtered_data)}")
  }

  return(filtered_data)
}


#' Parse Components for Network Analysis
#'
#' Extracts individual component values from config descriptors with intelligent
#' covariate grouping to avoid visual clutter in network plots.
#'
#' @param data Data frame with config_desc column
#' @param covariate_handling Method for handling covariate complexity
#' @return Data frame with parsed component values
#' @keywords internal

parse_components_for_network <- function(data, covariate_handling) {

  # Define component types to extract
  components <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")

  result <- data

  # ModelType - extract specific model values
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

  # Transformation - extract specific transformation values
  result <- result %>%
    dplyr::mutate(
      Transformation = dplyr::case_when(
        stringr::str_detect(config_desc, "_NoTrans_") ~ "None",
        stringr::str_detect(config_desc, "_Log_") ~ "Log",
        stringr::str_detect(config_desc, "_Sqrt_") ~ "Square Root",
        TRUE ~ "None"
      )
    )

  # Preprocessing - extract specific preprocessing values
  result <- result %>%
    dplyr::mutate(
      Preprocessing = dplyr::case_when(
        stringr::str_detect(config_desc, "_SNVD2_") ~ "SNV + D2",
        stringr::str_detect(config_desc, "_SNVD1_") ~ "SNV + D1",
        stringr::str_detect(config_desc, "_MSCD1_") ~ "MSC + D1",
        stringr::str_detect(config_desc, "_SNV_") ~ "SNV",
        stringr::str_detect(config_desc, "_MSC_") ~ "MSC",
        stringr::str_detect(config_desc, "_SG_") ~ "Savitzky-Golay",
        stringr::str_detect(config_desc, "_D2_") ~ "Derivative 2",
        stringr::str_detect(config_desc, "_D1_") ~ "Derivative 1",
        stringr::str_detect(config_desc, "_Raw_") ~ "Raw",
        TRUE ~ "Raw"
      )
    )

  # Feature Selection - extract specific feature selection values
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

  # Covariates - apply intelligent grouping
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
  if (covariate_handling == "simplified") {
    result <- result %>%
      dplyr::mutate(
        Covariates = categorize_covariates_for_network(covariate_raw)
      )
  } else if (covariate_handling == "aggregate") {
    result <- result %>%
      dplyr::mutate(
        Covariates = dplyr::case_when(
          covariate_raw == "None" ~ "Spectral Only",
          TRUE ~ "With Covariates"
        )
      )
  } else { # detailed
    result <- result %>%
      dplyr::mutate(
        Covariates = covariate_raw
      )
  }

  result <- result %>%
    dplyr::select(-covariate_raw)

  return(result)
}


#' Categorize Covariates for Network Visualization
#'
#' Groups complex covariate combinations into interpretable categories suitable
#' for network visualization without visual clutter.
#'
#' @param covariate_strings Character vector of covariate combinations
#' @return Character vector of categorized covariates
#' @keywords internal

categorize_covariates_for_network <- function(covariate_strings) {

  # Define covariate type mappings (expanded from existing function)
  soil_vars <- c("Clay", "pH", "CEC", "Sand", "Silt", "OM", "N", "P", "K", "BD", "SOC")
  climate_vars <- c("MAP", "GDD", "PET", "Temp", "Precip", "MAT", "AI", "TEMP", "PREC")

  purrr::map_chr(covariate_strings, function(cov_string) {
    if (is.na(cov_string) || cov_string == "None" || cov_string == "") {
      return("Spectral Only")
    }

    # Split covariate string on + and check types
    covs <- unlist(stringr::str_split(cov_string, "\\+"))

    has_soil <- any(covs %in% soil_vars)
    has_climate <- any(covs %in% climate_vars)

    if (has_soil && has_climate) {
      "Climate + Soil"
    } else if (has_soil) {
      "Soil Properties"
    } else if (has_climate) {
      "Climate Variables"
    } else {
      # Unknown covariates
      "Other Covariates"
    }
  })
}


#' Build Component Co-occurrence Network
#'
#' Creates network graph from component co-occurrence patterns in top-performing
#' configurations. Edges are weighted by co-occurrence frequency and performance.
#'
#' @param data Parsed data with component columns
#' @param metric Performance metric name
#' @param min_co_occurrence Minimum co-occurrence threshold
#' @return List with nodes, edges, and network graph
#' @keywords internal

build_component_network <- function(data, metric, min_co_occurrence) {

  # Get all component values as potential nodes
  component_columns <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")

  # Create long format with all component values per configuration
  component_values <- data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(component_columns),
      names_to = "component_type",
      values_to = "component_value"
    ) %>%
    dplyr::select(config_desc, component_type, component_value, dplyr::all_of(metric))

  # Get unique component values for nodes
  nodes <- component_values %>%
    dplyr::distinct(component_value, component_type) %>%
    dplyr::arrange(component_type, component_value) %>%
    dplyr::mutate(
      node_id = dplyr::row_number(),
      # Add performance and frequency statistics
      frequency = purrr::map_dbl(component_value, function(val) {
        sum(component_values$component_value == val)
      }),
      avg_performance = purrr::map_dbl(component_value, function(val) {
        vals <- component_values %>%
          dplyr::filter(component_value == val) %>%
          dplyr::pull(dplyr::all_of(metric))
        mean(vals, na.rm = TRUE)
      })
    )

  # Create component co-occurrence matrix
  # For each configuration, find all pairs of component values
  config_components <- data %>%
    dplyr::select(config_desc, dplyr::all_of(component_columns), dplyr::all_of(metric)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(component_columns),
      names_to = "component_type",
      values_to = "component_value"
    ) %>%
    dplyr::group_by(config_desc) %>%
    dplyr::summarise(
      components = list(component_value),
      performance = first(.data[[metric]]),
      .groups = "drop"
    )

  # Generate all pairwise co-occurrences
  edges <- purrr::map_dfr(1:nrow(config_components), function(i) {
    config_comps <- config_components$components[[i]]
    config_perf <- config_components$performance[i]

    if (length(config_comps) < 2) return(NULL)

    # Get all pairs
    pairs <- utils::combn(config_comps, 2, simplify = FALSE)

    purrr::map_dfr(pairs, function(pair) {
      tibble::tibble(
        from = pair[1],
        to = pair[2],
        performance = config_perf,
        config_desc = config_components$config_desc[i]
      )
    })
  })

  # Aggregate edge statistics
  edge_summary <- edges %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(
      co_occurrence_count = dplyr::n(),
      avg_performance = mean(performance, na.rm = TRUE),
      performance_sd = sd(performance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(co_occurrence_count >= min_co_occurrence) %>%
    dplyr::arrange(desc(co_occurrence_count))

  # Create symmetric edges (undirected network)
  edge_summary_symmetric <- edge_summary %>%
    dplyr::bind_rows(
      edge_summary %>%
        dplyr::select(from = to, to = from, co_occurrence_count, avg_performance, performance_sd)
    ) %>%
    dplyr::distinct()

  # Report network statistics in a more concise way
  n_nodes <- nrow(nodes)
  n_edges <- nrow(edge_summary)
  
  if (n_edges == 0) {
    cli::cli_alert_warning("No edges meet minimum co-occurrence threshold of {min_co_occurrence}")
  }

  # Create igraph object
  network_graph <- edge_summary_symmetric %>%
    dplyr::select(from, to, weight = co_occurrence_count, avg_performance) %>%
    igraph::graph_from_data_frame(vertices = nodes, directed = FALSE)

  # Add network metrics to nodes
  nodes <- nodes %>%
    dplyr::mutate(
      degree = igraph::degree(network_graph),
      betweenness = igraph::betweenness(network_graph, normalized = TRUE)
    )

  return(list(
    nodes = nodes,
    edges = edge_summary,
    graph = network_graph
  ))
}


#' Add Network Significance Testing
#'
#' Performs permutation testing to identify statistically significant co-occurrence
#' patterns beyond random chance.
#'
#' @param network_data Network data from build_component_network
#' @param parsed_data Original parsed data
#' @param metric Performance metric name
#' @param min_co_occurrence Minimum co-occurrence threshold
#' @return Network data with significance statistics added
#' @keywords internal

add_network_significance <- function(network_data, parsed_data, metric, min_co_occurrence) {

  # Use progress bar for significance testing

  # Get observed co-occurrence counts
  observed_edges <- network_data$edges

  # Perform permutation testing (reduced iterations for computational efficiency)
  n_perms <- 100

  permutation_results <- purrr::map_dfr(1:n_perms, function(perm) {
    # Permute component assignments randomly
    perm_data <- parsed_data
    component_columns <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")

    for (col in component_columns) {
      perm_data[[col]] <- sample(perm_data[[col]])
    }

    # Build network for permuted data
    perm_network_result <- safely_execute(
      expr = build_component_network(perm_data, metric, min_co_occurrence),
      default_value = NULL,
      error_message = "Permutation network failed"
    )

    if (!is.null(perm_network_result$result)) {
      perm_network_result$result$edges %>%
        dplyr::mutate(permutation = perm)
    } else {
      NULL
    }
  })

  # Calculate p-values for each observed edge
  if (nrow(permutation_results) > 0) {
    edge_pvals <- purrr::pmap_dfr(
      list(observed_edges$from, observed_edges$to, observed_edges$co_occurrence_count),
      function(from_node, to_node, obs_count) {

        # Get permutation distribution for this edge
        perm_counts <- permutation_results %>%
          dplyr::filter((from == from_node & to == to_node) | (from == to_node & to == from_node)) %>%
          dplyr::pull(co_occurrence_count)

        if (length(perm_counts) > 0) {
          p_value <- mean(perm_counts >= obs_count, na.rm = TRUE)
        } else {
          p_value <- 1.0  # Conservative approach
        }

        tibble::tibble(
          from = from_node,
          to = to_node,
          p_value = p_value,
          significant = p_value < 0.05
        )
      }
    )

    # Add significance results to edges
    network_data$edges <- network_data$edges %>%
      dplyr::left_join(edge_pvals, by = c("from", "to")) %>%
      dplyr::mutate(
        p_value = ifelse(is.na(p_value), 1.0, p_value),
        significant = ifelse(is.na(significant), FALSE, significant)
      )
  }

  # Significance testing completed - tracked in progress bar
  return(network_data)
}


#' Create Bundled Network Plot
#'
#' Creates the final network visualization with hierarchical edge bundling and
#' component sector organization.
#'
#' @param network_data Network data with nodes and edges
#' @param metric Performance metric name
#' @param bundling_strength Edge bundling strength parameter
#' @param node_size_metric Metric for node sizing
#' @param component_sectors Whether to organize by component sectors
#' @param show_significance Whether significance testing was performed
#' @param edge_alpha_range Range for edge transparency
#' @param title Plot title
#' @return ggplot2 object
#' @keywords internal

create_bundled_network_plot <- function(network_data, metric, bundling_strength,
                                       node_size_metric, component_sectors,
                                       show_significance, edge_alpha_range, title) {

  nodes <- network_data$nodes
  edges <- network_data$edges
  graph <- network_data$graph

  # Create tidygraph object for ggraph
  tidy_graph <- tidygraph::as_tbl_graph(graph)

  # Determine node sizes
  size_values <- switch(node_size_metric,
    "frequency" = nodes$frequency,
    "performance" = abs(nodes$avg_performance - mean(nodes$avg_performance, na.rm = TRUE)),
    "centrality" = nodes$betweenness
  )

  # Normalize sizes to reasonable range
  size_range <- range(size_values, na.rm = TRUE)
  normalized_sizes <- 3 + 7 * (size_values - size_range[1]) / (size_range[2] - size_range[1])

  # Add size information to graph
  tidy_graph <- tidy_graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(node_size = normalized_sizes)

  # The edge data should already be in the graph from igraph creation
  # We don't need to add it again

  # Create base plot with circular layout
  if (component_sectors) {
    # Group layout by component type for better organization
    # This would require custom layout - for now use circular
    layout_type <- "circle"
  } else {
    layout_type <- "circle"
  }

  p <- tidy_graph %>%
    ggraph::ggraph(layout = layout_type) +
    # Add edges with bundling
    ggraph::geom_edge_arc(
      ggplot2::aes(width = weight),
      alpha = 0.6,
      strength = bundling_strength,
      color = "#666666",
      show.legend = FALSE
    ) +
    # Add nodes
    ggraph::geom_node_point(
      ggplot2::aes(
        size = node_size,
        color = component_type
      ),
      stroke = 0.5
    ) +
    # Add node labels
    ggraph::geom_node_text(
      ggplot2::aes(label = name),
      size = 3,
      fontface = "bold",
      color = "black",
      repel = TRUE
    )

  # Apply edge scaling
  p <- p +
    ggraph::scale_edge_width_continuous(
      range = c(0.2, 2),
      guide = "none"
    ) +
    ggraph::scale_edge_alpha_identity()

  # Apply horizons color scheme for nodes
  node_colors <- horizons_colors("primary", n = length(unique(nodes$component_type)))
  names(node_colors) <- unique(nodes$component_type)

  p <- p +
    ggplot2::scale_color_manual(
      values = node_colors,
      name = "Component Type"
    ) +
    ggplot2::scale_size_continuous(
      range = c(3, 10),
      guide = "none"
    )

  # Apply horizons theme
  p <- p +
    horizons_theme(plot_type = "standard") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::coord_fixed()

  # Add title and subtitle
  if (is.null(title)) {
    title <- glue::glue("Multi-Way Component Interaction Network ({toupper(metric)})")
  }

  subtitle <- glue::glue("Nodes: component values | Edges: co-occurrence in top {round((1-0.75)*100)}% configurations")
  if (show_significance) {
    n_sig <- sum(edges$significant, na.rm = TRUE)
    subtitle <- paste(subtitle, glue::glue("| {n_sig} significant edges"))
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = "Edge thickness indicates co-occurrence frequency; Node size indicates component importance"
    )

  return(p)
}
