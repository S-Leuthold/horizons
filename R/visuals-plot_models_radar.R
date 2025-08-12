#' Visualize Marginal Effects of Model Pipeline Components
#'
#' Creates radar or bar plots showing the marginal contribution of different
#' modeling components (model type, preprocessing, transformations, feature selection,
#' covariates) to prediction performance variability. Useful for identifying which
#' pipeline components have the greatest impact on model performance.
#'
#' @param results_data A data frame containing model evaluation results with at minimum:
#'   a `config_desc` column with workflow identifiers and performance metric columns.
#' @param metric Character. Performance metric to analyze (e.g., "rrmse", "rsq", "rmse").
#'   Must be a column name in `results_data`.
#' @param components Character vector. Pipeline components to analyze. Default includes
#'   all standard components: `c("ModelType", "Transformation", "Preprocessing", 
#'   "Feature_Selection", "Covariates")`.
#' @param plot_type Character. Type of visualization: `"radar"` (default) for radar/spider
#'   plots, or `"bar"` for horizontal bar charts.
#' @param normalize Logical. If `TRUE` (default), normalize marginal standard deviations
#'   to 0-100 scale for easier comparison across metrics.
#' @param title Character. Optional plot title. If `NULL`, generates automatic title.
#' @param color_scheme Character. Color palette: `"default"` (package colors), 
#'   `"viridis"`, or `"plasma"`.
#' @param min_variation Numeric. Minimum coefficient of variation required for a component
#'   to be included in analysis. Default: 0.01 (1%).
#' @param group_by Character. Optional column name for creating faceted plots by group
#'   (e.g., different response variables or datasets).
#'
#' @return A `ggplot2` object. For radar plots, returns a polar coordinate plot.
#'   For bar plots, returns a horizontal bar chart. Can be further customized with
#'   standard `ggplot2` functions.
#'
#' @details
#' This function analyzes the marginal contribution of each pipeline component by:
#' \enumerate{
#'   \item Parsing config descriptors to extract component values
#'   \item Computing marginal standard deviation for each component while holding others constant
#'   \item Creating visualizations showing relative importance
#' }
#'
#' **Marginal Analysis Method:**
#' For each component, the function groups results by all other components and calculates
#' the standard deviation of the performance metric within each group. The average of
#' these standard deviations represents the marginal contribution of that component.
#'
#' **Config Descriptor Format:**
#' Expects workflow identifiers in the format: `model_transformation_preprocessing_featureselection_covariates`
#' (e.g., `"cubist_Log_SNVD1_PCA_Clay-pH-CEC"`). The function uses robust parsing to handle
#' variations in format.
#'
#' **Error Handling:**
#' Uses the package's `safely_execute()` framework for robust error handling.
#' Provides meaningful error messages and graceful degradation for partial failures.
#'
#' @examples
#' \dontrun{
#' # Basic radar plot
#' plot_models_radar(
#'   results_data = model_results,
#'   metric = "rrmse"
#' )
#'
#' # Bar chart with custom components
#' plot_models_radar(
#'   results_data = model_results,
#'   metric = "rsq",
#'   components = c("ModelType", "Preprocessing", "Covariates"),
#'   plot_type = "bar",
#'   title = "Component Effects on R-squared"
#' )
#'
#' # Faceted plot by response variable
#' plot_models_radar(
#'   results_data = multi_response_results,
#'   metric = "rrmse",
#'   group_by = "Response_Variable",
#'   normalize = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{plot_ensemble_upset}}, \code{\link{plot_ensemble_biplot}}, 
#' \code{\link{run_model_evaluation}}
#'
#' @importFrom dplyr mutate case_when filter group_by summarise ungroup select across all_of n_distinct arrange desc group_modify first
#' @importFrom purrr map_dfr map map_dfr
#' @importFrom stringr str_detect str_split_i str_count
#' @importFrom ggplot2 ggplot aes geom_polygon geom_col geom_path geom_line geom_point geom_text coord_fixed scale_fill_viridis_d scale_color_viridis_d scale_fill_manual scale_color_manual theme_minimal theme_void theme element_text element_blank labs facet_wrap
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info
#' @importFrom glue glue
#' @importFrom rlang sym eval_tidy .data
#' @importFrom viridis viridis plasma
#' @importFrom stats sd weighted.mean
#'
#' @family visualization
#' @export

plot_models_radar <- function(results_data,
                              metric = "rrmse",
                              components = c("ModelType", "Transformation", "Preprocessing", 
                                           "Feature_Selection", "Covariates"),
                              plot_type = c("radar", "bar"),
                              normalize = TRUE,
                              title = NULL,
                              color_scheme = c("default", "viridis", "plasma"),
                              min_variation = 0.01,
                              group_by = NULL) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------
  
  plot_type <- match.arg(plot_type)
  color_scheme <- match.arg(color_scheme)
  
  # Validate input data
  safely_execute(
    expr = {
      if (!is.data.frame(results_data)) {
        cli::cli_abort("results_data must be a data frame")
      }
      
      if (!"config_desc" %in% names(results_data)) {
        cli::cli_abort("results_data must contain a 'config_desc' column")
      }
      
      if (!metric %in% names(results_data)) {
        cli::cli_abort("Metric '{metric}' not found in results_data columns: {names(results_data)}")
      }
      
      if (nrow(results_data) == 0) {
        cli::cli_abort("results_data is empty")
      }
      
      if (!is.null(group_by) && !group_by %in% names(results_data)) {
        cli::cli_abort("group_by column '{group_by}' not found in results_data")
      }
      
      TRUE
    },
    default_value = FALSE,
    error_message = "Input validation failed",
    log_error = TRUE
  ) -> validation_result
  
  if (!validation_result$result) {
    return(NULL)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Parse Config Descriptors
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      parse_config_descriptors(results_data, components)
    },
    default_value = NULL,
    error_message = "Failed to parse config descriptors"
  ) -> parsed_data_result
  
  if (is.null(parsed_data_result$result)) {
    return(NULL)
  }
  
  parsed_data <- parsed_data_result$result
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Compute Marginal Standard Deviations
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      compute_marginal_effects(
        data = parsed_data,
        metric = metric,
        components = components,
        min_variation = min_variation,
        group_by = group_by
      )
    },
    default_value = NULL,
    error_message = "Failed to compute marginal effects"
  ) -> marginal_effects_result
  
  if (is.null(marginal_effects_result$result)) {
    return(NULL)
  }
  
  marginal_data <- marginal_effects_result$result
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Create Visualization
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      if (plot_type == "radar") {
        create_radar_plot(
          data = marginal_data,
          metric = metric,
          normalize = normalize,
          title = title,
          color_scheme = color_scheme,
          group_by = group_by
        )
      } else {
        create_bar_plot(
          data = marginal_data,
          metric = metric,
          normalize = normalize,
          title = title,
          color_scheme = color_scheme,
          group_by = group_by
        )
      }
    },
    default_value = NULL,
    error_message = "Failed to create visualization"
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

#' Parse Config Descriptors into Component Values
#'
#' Robustly extracts modeling components from workflow configuration descriptors.
#' Handles various descriptor formats and provides fallback logic for edge cases.
#'
#' @param data Data frame with config_desc column
#' @param components Character vector of components to extract
#' @return Data frame with original data plus parsed component columns
#' @keywords internal

parse_config_descriptors <- function(data, components) {
  
  # Create copy to avoid modifying original
  result_data <- data
  
  # Extract ModelType
  if ("ModelType" %in% components) {
    result_data <- result_data %>%
      dplyr::mutate(
        ModelType = dplyr::case_when(
          stringr::str_detect(.data$config_desc, "random_forest") ~ "Random Forest",
          stringr::str_detect(.data$config_desc, "cubist")        ~ "Cubist",
          stringr::str_detect(.data$config_desc, "xgboost")       ~ "XGBoost",
          stringr::str_detect(.data$config_desc, "lightgbm")      ~ "LightGBM",
          stringr::str_detect(.data$config_desc, "elastic_net")   ~ "Elastic Net",
          stringr::str_detect(.data$config_desc, "svm_rbf")       ~ "SVM (RBF)",
          stringr::str_detect(.data$config_desc, "svm_poly")      ~ "SVM (Poly)",
          stringr::str_detect(.data$config_desc, "mars")          ~ "MARS",
          stringr::str_detect(.data$config_desc, "plsr")          ~ "PLSR",
          stringr::str_detect(.data$config_desc, "mlp_nn")        ~ "MLP Neural Net",
          stringr::str_detect(.data$config_desc, "knn")           ~ "k-NN",
          stringr::str_detect(.data$config_desc, "linear_reg")    ~ "Linear Regression",
          TRUE ~ "Unknown Model"
        )
      )
  }
  
  # Extract Transformation
  if ("Transformation" %in% components) {
    result_data <- result_data %>%
      dplyr::mutate(
        Transformation = dplyr::case_when(
          stringr::str_detect(.data$config_desc, "NoTrans|None") ~ "None",
          stringr::str_detect(.data$config_desc, "Log")          ~ "Log",
          stringr::str_detect(.data$config_desc, "Sqrt")         ~ "Square Root",
          stringr::str_detect(.data$config_desc, "BoxCox")       ~ "Box-Cox",
          TRUE ~ "None"  # Default assumption
        )
      )
  }
  
  # Extract Preprocessing with improved pattern matching
  if ("Preprocessing" %in% components) {
    result_data <- result_data %>%
      dplyr::mutate(
        Preprocessing = dplyr::case_when(
          # Order matters here - more specific patterns first
          stringr::str_detect(.data$config_desc, "SNVD2")      ~ "SNV + Derivative 2",
          stringr::str_detect(.data$config_desc, "SNVD1")      ~ "SNV + Derivative 1",
          stringr::str_detect(.data$config_desc, "MSCD2")      ~ "MSC + Derivative 2",
          stringr::str_detect(.data$config_desc, "MSCD1")      ~ "MSC + Derivative 1",
          stringr::str_detect(.data$config_desc, "SGD2")       ~ "SG + Derivative 2",
          stringr::str_detect(.data$config_desc, "SGD1")       ~ "SG + Derivative 1",
          stringr::str_detect(.data$config_desc, "D2")         ~ "Derivative 2",
          stringr::str_detect(.data$config_desc, "D1")         ~ "Derivative 1",
          stringr::str_detect(.data$config_desc, "SNV")        ~ "SNV",
          stringr::str_detect(.data$config_desc, "MSC")        ~ "MSC",
          stringr::str_detect(.data$config_desc, "SG")         ~ "Savitzky-Golay",
          stringr::str_detect(.data$config_desc, "Raw")        ~ "Raw",
          TRUE ~ "Raw"  # Default assumption
        )
      )
  }
  
  # Extract Feature Selection
  if ("Feature_Selection" %in% components) {
    result_data <- result_data %>%
      dplyr::mutate(
        Feature_Selection = dplyr::case_when(
          stringr::str_detect(.data$config_desc, "SHAP")         ~ "SHAP",
          stringr::str_detect(.data$config_desc, "PCA")          ~ "PCA",
          stringr::str_detect(.data$config_desc, "Corr")         ~ "Correlation",
          stringr::str_detect(.data$config_desc, "Boruta")       ~ "Boruta",
          stringr::str_detect(.data$config_desc, "Variance")     ~ "Variance Filter",
          stringr::str_detect(.data$config_desc, "Univariate")   ~ "Univariate",
          TRUE ~ "PCA"  # Default assumption
        )
      )
  }
  
  # Extract Covariates using robust parsing
  if ("Covariates" %in% components) {
    result_data <- result_data %>%
      dplyr::mutate(
        # Count underscores to determine position
        underscore_count = stringr::str_count(.data$config_desc, "_"),
        # Extract covariates based on position
        Covariates = dplyr::case_when(
          .data$underscore_count == 3 ~ stringr::str_split_i(.data$config_desc, "_", i = 4),
          .data$underscore_count == 4 ~ stringr::str_split_i(.data$config_desc, "_", i = 5),
          .data$underscore_count == 5 ~ stringr::str_split_i(.data$config_desc, "_", i = 6),
          .data$underscore_count == 6 ~ stringr::str_split_i(.data$config_desc, "_", i = 7),
          .data$underscore_count == 7 ~ stringr::str_split_i(.data$config_desc, "_", i = 8),
          .data$underscore_count >= 8 ~ stringr::str_split_i(.data$config_desc, "_", i = -1), # Last element
          TRUE ~ ""
        ),
        # Clean up covariates
        Covariates = dplyr::case_when(
          is.na(.data$Covariates) | .data$Covariates == "" ~ "No Covariates",
          TRUE ~ .data$Covariates
        )
      ) %>%
      dplyr::select(-underscore_count)
  }
  
  # Validate that we successfully parsed components
  missing_components <- components[!components %in% names(result_data)]
  if (length(missing_components) > 0) {
    cli::cli_alert_warning("Failed to parse components: {missing_components}")
  }
  
  # Check for any components with only NA/Unknown values
  for (comp in components) {
    if (comp %in% names(result_data)) {
      unique_vals <- unique(result_data[[comp]])
      if (length(unique_vals) == 1 && (is.na(unique_vals[1]) || 
                                      unique_vals[1] %in% c("Unknown Model", "Unknown"))) {
        cli::cli_alert_warning("Component '{comp}' has only unknown/missing values")
      }
    }
  }
  
  return(result_data)
}


#' Compute Marginal Effects of Pipeline Components
#'
#' Calculates the marginal contribution of each modeling component to performance
#' variability by computing standard deviations within groups of other components.
#'
#' @param data Data frame with parsed component columns and metric
#' @param metric Character name of performance metric column
#' @param components Character vector of component names to analyze
#' @param min_variation Minimum coefficient of variation to include component
#' @param group_by Optional grouping variable for faceted analysis
#' @return Data frame with component names and marginal standard deviations
#' @keywords internal

compute_marginal_effects <- function(data, metric, components, min_variation = 0.01, group_by = NULL) {
  
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
  
  if (length(components) == 0) {
    cli::cli_abort("No valid components found in data")
  }
  
  # Function to compute marginal SD for a single component
  compute_single_marginal <- function(comp, data, metric, components, group_by) {
    
    # Get other components to group by
    other_components <- setdiff(components, comp)
    
    # Add group_by variable if specified
    grouping_vars <- other_components
    if (!is.null(group_by)) {
      grouping_vars <- c(group_by, other_components)
    }
    
    ## APPROACH 1: Try exact matching first (original approach)
    marginal_result <- data %>%
      # Group by all other components
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
      # Only keep groups where the focal component varies
      dplyr::filter(dplyr::n_distinct(.data[[comp]]) > 1) %>%
      # Calculate SD within each group
      dplyr::summarise(
        sd_value = stats::sd(.data[[metric]], na.rm = TRUE),
        n_configs = dplyr::n(),
        .groups = "drop"
      ) %>%
      # Average SDs across groups, weighted by group size
      dplyr::summarise(
        component = comp,
        mean_sd = stats::weighted.mean(sd_value, w = n_configs, na.rm = TRUE),
        n_groups = dplyr::n(),
        total_configs = sum(n_configs),
        .groups = "drop"
      )
    
    # If exact matching found no groups, use ANOVA-style approach
    if (nrow(marginal_result) == 0 || is.na(marginal_result$mean_sd[1])) {
      cli::cli_alert_info("No exact matches found for {comp}, using ANOVA approach")
      
      # APPROACH 2: ANOVA-style marginal effect
      # Compute overall mean
      overall_mean <- mean(data[[metric]], na.rm = TRUE)
      
      # Compute component means and sample sizes
      component_stats <- data %>%
        dplyr::group_by(.data[[comp]]) %>%
        dplyr::summarise(
          component_mean = mean(.data[[metric]], na.rm = TRUE),
          n_configs = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          deviation_from_overall = abs(component_mean - overall_mean)
        )
      
      # Weighted standard deviation of component means
      if (nrow(component_stats) > 1) {
        marginal_sd <- stats::weighted.mean(
          component_stats$deviation_from_overall, 
          w = component_stats$n_configs, 
          na.rm = TRUE
        )
        
        marginal_result <- data.frame(
          component = comp,
          mean_sd = marginal_sd,
          n_groups = nrow(component_stats),
          total_configs = sum(component_stats$n_configs)
        )
      } else {
        # Component has no variation
        marginal_result <- data.frame(
          component = comp,
          mean_sd = 0,
          n_groups = 1,
          total_configs = nrow(data)
        )
      }
    }
    
    # Add group_by information if provided
    if (!is.null(group_by)) {
      # For grouped analysis, compute within each group
      grouped_result <- data %>%
        dplyr::group_by(.data[[group_by]]) %>%
        dplyr::group_modify(~ {
          # Try exact matching first
          exact_result <- .x %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(other_components))) %>%
            dplyr::filter(dplyr::n_distinct(.data[[comp]]) > 1) %>%
            dplyr::summarise(
              sd_value = stats::sd(.data[[metric]], na.rm = TRUE),
              n_configs = dplyr::n(),
              .groups = "drop"
            )
          
          if (nrow(exact_result) > 0) {
            exact_result %>%
              dplyr::summarise(
                component = comp,
                mean_sd = stats::weighted.mean(sd_value, w = n_configs, na.rm = TRUE),
                n_groups = dplyr::n(),
                total_configs = sum(n_configs),
                .groups = "drop"
              )
          } else {
            # Use ANOVA approach for this group
            overall_mean <- mean(.x[[metric]], na.rm = TRUE)
            component_stats <- .x %>%
              dplyr::group_by(.data[[comp]]) %>%
              dplyr::summarise(
                component_mean = mean(.data[[metric]], na.rm = TRUE),
                n_configs = dplyr::n(),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                deviation_from_overall = abs(component_mean - overall_mean)
              )
            
            if (nrow(component_stats) > 1) {
              marginal_sd <- stats::weighted.mean(
                component_stats$deviation_from_overall, 
                w = component_stats$n_configs, 
                na.rm = TRUE
              )
            } else {
              marginal_sd <- 0
            }
            
            data.frame(
              component = comp,
              mean_sd = marginal_sd,
              n_groups = nrow(component_stats),
              total_configs = sum(component_stats$n_configs)
            )
          }
        }) %>%
        dplyr::ungroup()
      
      return(grouped_result)
      
    } else {
      return(marginal_result)
    }
  }
  
  # Compute marginal effects for all components
  marginal_results <- purrr::map_dfr(components, function(comp) {
    safely_execute(
      expr = compute_single_marginal(comp, data, metric, components, group_by),
      default_value = NULL,
      error_message = glue::glue("Failed to compute marginal effect for component: {comp}")
    )$result
  })
  
  # Filter out results that couldn't be computed
  marginal_results <- marginal_results %>%
    dplyr::filter(!is.na(mean_sd))
  
  if (nrow(marginal_results) == 0) {
    cli::cli_abort("Could not compute marginal effects for any components")
  }
  
  # Check for minimum variation
  if (min_variation > 0) {
    # Calculate coefficient of variation for each component
    overall_mean <- mean(data[[metric]], na.rm = TRUE)
    
    marginal_results <- marginal_results %>%
      dplyr::mutate(
        cv = mean_sd / overall_mean,
        meets_threshold = cv >= min_variation
      )
    
    # Warn about components below threshold
    low_variation <- marginal_results %>%
      dplyr::filter(!meets_threshold) %>%
      dplyr::pull(component)
    
    if (length(low_variation) > 0) {
      cli::cli_alert_warning("Components with low variation (CV < {min_variation}): {low_variation}")
    }
    
    # Filter to components meeting threshold
    marginal_results <- marginal_results %>%
      dplyr::filter(meets_threshold) %>%
      dplyr::select(-cv, -meets_threshold)
  }
  
  # Add summary information
  marginal_results <- marginal_results %>%
    dplyr::mutate(
      metric_name = metric,
      analysis_type = ifelse(is.null(group_by), "overall", "grouped")
    )
  
  return(marginal_results)
}


#' Create Radar Plot from Marginal Effects Data
#'
#' Creates a radar/spider plot showing the relative importance of different
#' modeling components based on their marginal effects.
#'
#' @param data Marginal effects data from compute_marginal_effects()
#' @param metric Character name of the performance metric
#' @param normalize Logical, whether to normalize values to 0-100 scale
#' @param title Optional plot title
#' @param color_scheme Color scheme to use
#' @param group_by Optional grouping variable for facets
#' @return ggplot2 object
#' @keywords internal

create_radar_plot <- function(data, metric, normalize = TRUE, title = NULL, 
                             color_scheme = "default", group_by = NULL) {
  
  # Prepare data for radar plot
  plot_data <- data
  
  # Normalize if requested
  if (normalize) {
    if (!is.null(group_by)) {
      plot_data <- plot_data %>%
        dplyr::group_by(.data[[group_by]]) %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd)) / (max(mean_sd) - min(mean_sd))
        ) %>%
        dplyr::ungroup()
    } else {
      plot_data <- plot_data %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd)) / (max(mean_sd) - min(mean_sd))
        )
    }
    value_col <- "normalized_value"
    value_label <- "Normalized Marginal Effect (0-100)"
  } else {
    plot_data$normalized_value <- plot_data$mean_sd
    value_col <- "normalized_value"
    value_label <- glue::glue("Marginal Standard Deviation ({toupper(metric)})")
  }
  
  # Create coordinates for radar plot
  n_components <- nrow(plot_data)
  if (!is.null(group_by)) {
    n_components <- length(unique(plot_data$component))
  }
  
  angles <- seq(0, 2 * pi, length.out = n_components + 1)[-1]
  
  # Add angles and convert to cartesian coordinates
  plot_data <- plot_data %>%
    dplyr::arrange(component) %>%
    dplyr::mutate(
      angle = rep(angles, length.out = nrow(.)),
      x = .data[[value_col]] * cos(angle),
      y = .data[[value_col]] * sin(angle)
    )
  
  # Get color scheme
  colors <- get_color_scheme(color_scheme, nrow(plot_data))
  
  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y))
  
  # Add grid circles (background)
  max_value <- max(plot_data[[value_col]], na.rm = TRUE)
  grid_values <- seq(0, max_value, length.out = 5)
  
  # Create grid circles
  circle_data <- purrr::map_dfr(grid_values, function(r) {
    angles_full <- seq(0, 2 * pi, length.out = 100)
    data.frame(
      x = r * cos(angles_full),
      y = r * sin(angles_full),
      radius = r
    )
  })
  
  p <- p + 
    ggplot2::geom_path(
      data = circle_data, 
      ggplot2::aes(x = x, y = y, group = radius),
      color = "grey90", 
      linewidth = 0.5
    )
  
  # Add radial lines
  radial_data <- purrr::map_dfr(angles, function(angle) {
    data.frame(
      x = c(0, max_value * cos(angle)),
      y = c(0, max_value * sin(angle)),
      line_id = angle
    )
  })
  
  p <- p + 
    ggplot2::geom_line(
      data = radial_data,
      ggplot2::aes(x = x, y = y, group = line_id),
      color = "grey90",
      linewidth = 0.5
    )
  
  # Add the radar polygon
  if (!is.null(group_by)) {
    # For grouped data, create separate polygons
    p <- p + 
      ggplot2::geom_polygon(
        ggplot2::aes(fill = .data[[group_by]], color = .data[[group_by]]),
        alpha = 0.3,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data[[group_by]]),
        size = 3
      )
  } else {
    # Single polygon
    p <- p + 
      ggplot2::geom_polygon(
        fill = colors[1],
        color = colors[1],
        alpha = 0.3,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        color = colors[1],
        size = 3
      )
  }
  
  # Add component labels
  label_data <- plot_data %>%
    dplyr::group_by(component) %>%
    dplyr::summarise(
      angle = first(angle),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      label_radius = max_value * 1.1,
      x_label = label_radius * cos(angle),
      y_label = label_radius * sin(angle)
    )
  
  p <- p + 
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = x_label, y = y_label, label = component),
      size = 3.5,
      fontface = "bold"
    )
  
  # Apply theme and styling
  p <- p + 
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = if (!is.null(group_by)) "bottom" else "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
    )
  
  # Apply color scheme
  if (!is.null(group_by)) {
    p <- p + apply_color_scheme(color_scheme, is_fill = TRUE)
  }
  
  # Add title
  if (is.null(title)) {
    title <- glue::glue("Marginal Effects of Pipeline Components ({toupper(metric)})")
  }
  
  p <- p + ggplot2::labs(title = title, subtitle = value_label)
  
  # Add faceting if grouped
  if (!is.null(group_by)) {
    p <- p + ggplot2::facet_wrap(~ .data[[group_by]])
  }
  
  return(p)
}


#' Create Bar Plot from Marginal Effects Data
#'
#' Creates a horizontal bar chart showing the relative importance of different
#' modeling components based on their marginal effects.
#'
#' @param data Marginal effects data from compute_marginal_effects()
#' @param metric Character name of the performance metric
#' @param normalize Logical, whether to normalize values to 0-100 scale
#' @param title Optional plot title
#' @param color_scheme Color scheme to use
#' @param group_by Optional grouping variable for facets
#' @return ggplot2 object
#' @keywords internal

create_bar_plot <- function(data, metric, normalize = TRUE, title = NULL, 
                           color_scheme = "default", group_by = NULL) {
  
  # Prepare data
  plot_data <- data
  
  # Normalize if requested
  if (normalize) {
    if (!is.null(group_by)) {
      plot_data <- plot_data %>%
        dplyr::group_by(.data[[group_by]]) %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd)) / (max(mean_sd) - min(mean_sd))
        ) %>%
        dplyr::ungroup()
    } else {
      plot_data <- plot_data %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd)) / (max(mean_sd) - min(mean_sd))
        )
    }
    value_col <- "normalized_value"
    value_label <- "Normalized Marginal Effect (0-100)"
  } else {
    plot_data$normalized_value <- plot_data$mean_sd
    value_col <- "normalized_value"
    value_label <- glue::glue("Marginal Standard Deviation ({toupper(metric)})")
  }
  
  # Order components by effect size
  plot_data <- plot_data %>%
    dplyr::arrange(desc(.data[[value_col]])) %>%
    dplyr::mutate(
      component = factor(component, levels = unique(component))
    )
  
  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[value_col]], y = component))
  
  # Add bars
  if (!is.null(group_by)) {
    p <- p + 
      ggplot2::geom_col(
        ggplot2::aes(fill = .data[[group_by]]),
        position = "dodge",
        color = "black",
        linewidth = 0.5
      )
  } else {
    colors <- get_color_scheme(color_scheme, 1)
    p <- p + 
      ggplot2::geom_col(
        fill = colors[1],
        color = "black",
        linewidth = 0.5
      )
  }
  
  # Apply theme
  p <- p + 
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold"),
      legend.position = if (!is.null(group_by)) "bottom" else "none",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  # Apply color scheme
  if (!is.null(group_by)) {
    p <- p + apply_color_scheme(color_scheme, is_fill = TRUE)
  }
  
  # Add labels
  if (is.null(title)) {
    title <- glue::glue("Marginal Effects of Pipeline Components ({toupper(metric)})")
  }
  
  p <- p + 
    ggplot2::labs(
      title = title,
      subtitle = value_label,
      x = value_label,
      y = "Pipeline Component"
    )
  
  # Add faceting if grouped
  if (!is.null(group_by)) {
    p <- p + ggplot2::facet_wrap(~ .data[[group_by]], scales = "free_x")
  }
  
  return(p)
}


#' Get Color Scheme for Plots
#'
#' Returns color values based on the specified color scheme.
#'
#' @param scheme Character name of color scheme
#' @param n Number of colors needed
#' @return Character vector of hex colors
#' @keywords internal

get_color_scheme <- function(scheme = "default", n = 1) {
  switch(scheme,
    "default" = rep(c("#818D5D", "#9D4920", "#E8B04B", "#7B68A6", "#A4B494"), length.out = n),
    "viridis" = viridis::viridis(n),
    "plasma" = viridis::plasma(n),
    rep("#818D5D", n)  # fallback
  )
}


#' Apply Color Scheme to ggplot
#'
#' Adds appropriate color/fill scales to a ggplot object.
#'
#' @param scheme Character name of color scheme
#' @param is_fill Logical, whether to apply fill scale (TRUE) or color scale (FALSE)
#' @return ggplot2 scale object
#' @keywords internal

apply_color_scheme <- function(scheme = "default", is_fill = TRUE) {
  if (scheme == "viridis") {
    if (is_fill) {
      return(ggplot2::scale_fill_viridis_d())
    } else {
      return(ggplot2::scale_color_viridis_d())
    }
  } else if (scheme == "plasma") {
    if (is_fill) {
      return(ggplot2::scale_fill_viridis_d(option = "plasma"))
    } else {
      return(ggplot2::scale_color_viridis_d(option = "plasma"))
    }
  } else {
    # Default colors
    colors <- get_color_scheme("default", 10)
    if (is_fill) {
      return(ggplot2::scale_fill_manual(values = colors))
    } else {
      return(ggplot2::scale_color_manual(values = colors))
    }
  }
}