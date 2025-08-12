#' Enhanced Component Effects Visualization with Confidence Intervals
#'
#' Creates radar or bar plots showing the marginal contribution of different
#' modeling components to prediction performance variability, enhanced with
#' bootstrap confidence intervals, effect size interpretation, and multi-response
#' variable support. Extends the basic radar plot with advanced statistical analysis.
#'
#' @param results_data A data frame containing model evaluation results with at minimum:
#'   a `config_desc` column with workflow identifiers and performance metric columns.
#' @param metric Character. Performance metric to analyze (e.g., "rrmse", "rsq", "rmse").
#' @param components Character vector. Pipeline components to analyze.
#' @param plot_type Character. Type of visualization: `"radar"` or `"bar"`.
#' @param include_ci Logical. Whether to include bootstrap confidence intervals.
#' @param ci_level Numeric. Confidence level for intervals (0-1).
#' @param n_bootstrap Integer. Number of bootstrap samples for CI calculation.
#' @param effect_size_labels Logical. Whether to add effect size interpretation labels.
#' @param response_variable_col Character. Column name for response variable (for multi-response).
#' @param comparison_type Character. How to handle multi-response: "facet", "overlay", or "separate".
#' @param normalize Logical. Whether to normalize marginal effects to 0-100 scale.
#' @param title Character. Optional plot title.
#' @param subtitle Character. Optional plot subtitle.
#'
#' @return A `ggplot2` object with enhanced marginal effects visualization.
#'
#' @details
#' This function extends the basic component effects analysis with:
#' \itemize{
#'   \item Bootstrap confidence intervals for statistical significance
#'   \item Effect size interpretation (Small, Medium, Large) based on Cohen's conventions
#'   \item Multi-response variable support via faceting or overlay
#'   \item Enhanced visual design matching manuscript standards
#' }
#'
#' **Bootstrap Method:** 
#' Resamples the data with replacement to estimate uncertainty in marginal effect calculations.
#' Provides robust confidence intervals even with non-normal distributions.
#'
#' **Effect Size Interpretation:**
#' Adapts Cohen's effect size conventions to model performance metrics:
#' - RRMSE/RMSE: Small ≥ 0.02, Medium ≥ 0.05, Large ≥ 0.10
#' - R-squared: Small ≥ 0.05, Medium ≥ 0.10, Large ≥ 0.20
#'
#' @examples
#' \dontrun{
#' # Basic enhanced radar plot
#' plot_component_effects_enhanced(
#'   results_data = model_results,
#'   metric = "rrmse",
#'   include_ci = TRUE
#' )
#'
#' # Multi-response comparison with faceting
#' plot_component_effects_enhanced(
#'   results_data = multi_response_results,
#'   metric = "rrmse",
#'   response_variable_col = "response_var",
#'   comparison_type = "facet",
#'   effect_size_labels = TRUE
#' )
#'
#' # Bar chart with custom components
#' plot_component_effects_enhanced(
#'   results_data = model_results,
#'   metric = "rsq",
#'   plot_type = "bar",
#'   components = c("ModelType", "Preprocessing", "Covariates"),
#'   title = "Component Importance for R-squared"
#' )
#' }
#'
#' @seealso
#' \code{\link{plot_models_radar}}, \code{\link{plot_component_interactions}},
#' \code{\link{horizons_theme}}, \code{\link{horizons_colors}}
#'
#' @importFrom dplyr mutate filter group_by summarise ungroup select arrange desc slice_sample left_join first
#' @importFrom purrr map_dfr map
#' @importFrom stringr str_detect str_split_i str_count
#' @importFrom ggplot2 ggplot aes geom_polygon geom_col geom_errorbar geom_path geom_line geom_point geom_text coord_fixed scale_fill_manual scale_color_manual labs facet_wrap position_dodge
#' @importFrom tidyr pivot_longer
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats quantile
#'
#' @family visualization
#' @export

plot_component_effects_enhanced <- function(results_data,
                                          metric = "rrmse",
                                          components = c("ModelType", "Transformation", "Preprocessing", 
                                                       "Feature_Selection", "Covariates"),
                                          plot_type = c("radar", "bar"),
                                          include_ci = TRUE,
                                          ci_level = 0.95,
                                          n_bootstrap = 1000,
                                          effect_size_labels = TRUE,
                                          response_variable_col = NULL,
                                          comparison_type = c("facet", "overlay", "separate"),
                                          normalize = TRUE,
                                          title = NULL,
                                          subtitle = NULL) {
  
  # Match arguments
  plot_type <- match.arg(plot_type)
  comparison_type <- match.arg(comparison_type)
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation and Data Preparation
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      validate_visualization_data(
        data = results_data,
        required_columns = c("config_desc"),
        metric = metric,
        min_rows = 10
      )
      
      # Prepare multi-response data if needed
      if (!is.null(response_variable_col) && response_variable_col %in% names(results_data)) {
        prepared_data <- prepare_multi_response_data(
          data = results_data,
          response_column = response_variable_col,
          metric_columns = metric,
          config_column = "config_desc"
        )
      } else {
        # Single response analysis - just add default response variable
        prepared_data <- results_data
        prepared_data$response_variable <- "Default_Response"
      }
      
      TRUE
    },
    default_value = FALSE,
    error_message = "Data validation failed"
  ) -> validation_result
  
  if (!validation_result$result) {
    return(NULL)
  }
  
  # Prepare data for analysis
  if (!is.null(response_variable_col) && response_variable_col %in% names(results_data)) {
    prepared_data <- prepare_multi_response_data(
      data = results_data,
      response_column = response_variable_col,
      metric_columns = metric,
      config_column = "config_desc"
    )
  } else {
    # Single response analysis
    prepared_data <- results_data
    prepared_data$response_variable <- "Default_Response"
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Parse Config Descriptors (if not already parsed)
  ## ---------------------------------------------------------------------------
  
  # Check if components are already in the data (manual parsing)
  existing_components <- intersect(components, names(prepared_data))
  
  if (length(existing_components) == length(components)) {
    # All components already exist - use data as is
    cli::cli_alert_info("Using existing component columns (manual parsing detected)")
    parsed_data <- prepared_data
  } else {
    # Need to parse config descriptors
    safely_execute(
      expr = {
        parse_config_descriptors(prepared_data, components)
      },
      default_value = NULL,
      error_message = "Failed to parse config descriptors"
    ) -> parsed_data_result
    
    if (is.null(parsed_data_result$result)) {
      return(NULL)
    }
    
    parsed_data <- parsed_data_result$result
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Compute Enhanced Marginal Effects
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      compute_enhanced_marginal_effects(
        data = parsed_data,
        metric = metric,
        components = components,
        include_ci = include_ci,
        ci_level = ci_level,
        n_bootstrap = n_bootstrap,
        effect_size_labels = effect_size_labels,
        response_variable_col = if (!is.null(response_variable_col)) "response_variable" else NULL
      )
    },
    default_value = NULL,
    error_message = "Failed to compute enhanced marginal effects"
  ) -> marginal_effects_result
  
  if (is.null(marginal_effects_result$result)) {
    return(NULL)
  }
  
  marginal_data <- marginal_effects_result$result
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Create Enhanced Visualization
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      if (plot_type == "radar") {
        create_enhanced_radar_plot(
          data = marginal_data,
          metric = metric,
          include_ci = include_ci,
          effect_size_labels = effect_size_labels,
          response_variable_col = if (!is.null(response_variable_col)) "response_variable" else NULL,
          comparison_type = comparison_type,
          normalize = normalize,
          title = title,
          subtitle = subtitle
        )
      } else {
        create_enhanced_bar_plot(
          data = marginal_data,
          metric = metric,
          include_ci = include_ci,
          effect_size_labels = effect_size_labels,
          response_variable_col = if (!is.null(response_variable_col)) "response_variable" else NULL,
          comparison_type = comparison_type,
          normalize = normalize,
          title = title,
          subtitle = subtitle
        )
      }
    },
    default_value = NULL,
    error_message = "Failed to create enhanced visualization"
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

#' Compute Enhanced Marginal Effects with Bootstrap Confidence Intervals
#'
#' Calculates marginal effects with bootstrap confidence intervals and effect size labels.
#'
#' @param data Parsed data frame with component columns
#' @param metric Performance metric name
#' @param components Component names to analyze
#' @param include_ci Whether to compute confidence intervals
#' @param ci_level Confidence level
#' @param n_bootstrap Number of bootstrap samples
#' @param effect_size_labels Whether to add effect size labels
#' @param response_variable_col Response variable column for grouping
#' @return Data frame with enhanced marginal effects
#' @keywords internal

compute_enhanced_marginal_effects <- function(data, metric, components, 
                                            include_ci = TRUE, ci_level = 0.95,
                                            n_bootstrap = 1000, effect_size_labels = TRUE,
                                            response_variable_col = NULL) {
  
  # Compute basic marginal effects
  basic_effects <- compute_marginal_effects(
    data = data,
    metric = metric,
    components = components,
    min_variation = 0.01,
    group_by = response_variable_col
  )
  
  if (nrow(basic_effects) == 0) {
    cli::cli_abort("No marginal effects could be computed")
  }
  
  # Add bootstrap confidence intervals if requested
  if (include_ci) {
    cli::cli_alert_info("Computing bootstrap confidence intervals ({n_bootstrap} samples)...")
    
    # Function to compute marginal effects for a single bootstrap sample
    bootstrap_marginal <- function(boot_data) {
      safely_execute(
        expr = {
          compute_marginal_effects(
            data = boot_data,
            metric = metric,
            components = components,
            min_variation = 0,
            group_by = response_variable_col
          )
        },
        default_value = NULL
      )$result
    }
    
    # Generate bootstrap samples and compute effects
    bootstrap_results <- list()
    
    for (i in 1:n_bootstrap) {
      # Stratified sampling by response variable if present
      if (!is.null(response_variable_col)) {
        boot_sample <- data %>%
          dplyr::group_by(.data[[response_variable_col]]) %>%
          dplyr::slice_sample(n = dplyr::n(), replace = TRUE) %>%
          dplyr::ungroup()
      } else {
        boot_sample <- data %>%
          dplyr::slice_sample(n = nrow(data), replace = TRUE)
      }
      
      boot_result <- bootstrap_marginal(boot_sample)
      if (!is.null(boot_result)) {
        boot_result$bootstrap_sample <- i
        bootstrap_results[[i]] <- boot_result
      }
    }
    
    # Combine bootstrap results
    if (length(bootstrap_results) > 0) {
      bootstrap_combined <- purrr::map_dfr(bootstrap_results, identity)
      
      # Calculate confidence intervals
      alpha <- 1 - ci_level
      lower_q <- alpha / 2
      upper_q <- 1 - (alpha / 2)
      
      ci_data <- bootstrap_combined %>%
        dplyr::group_by(component) %>%
        {if (!is.null(response_variable_col)) 
          dplyr::group_by(., .data[[response_variable_col]], .add = TRUE) 
         else .} %>%
        dplyr::summarise(
          ci_lower = stats::quantile(mean_sd, probs = lower_q, na.rm = TRUE),
          ci_upper = stats::quantile(mean_sd, probs = upper_q, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Merge CI data with basic effects
      if (!is.null(response_variable_col)) {
        basic_effects <- basic_effects %>%
          dplyr::left_join(ci_data, by = c("component", response_variable_col))
      } else {
        basic_effects <- basic_effects %>%
          dplyr::left_join(ci_data, by = "component")
      }
    } else {
      cli::cli_alert_warning("Bootstrap failed - proceeding without confidence intervals")
      basic_effects$ci_lower <- NA
      basic_effects$ci_upper <- NA
    }
  } else {
    basic_effects$ci_lower <- NA
    basic_effects$ci_upper <- NA
  }
  
  # Add effect size labels if requested
  if (effect_size_labels) {
    basic_effects$effect_size_label <- create_effect_size_labels(
      basic_effects$mean_sd, 
      metric = metric
    )
  }
  
  return(basic_effects)
}


#' Create Enhanced Radar Plot
#'
#' Creates radar plot with confidence intervals and effect size labels.
#'
#' @param data Enhanced marginal effects data
#' @param metric Performance metric
#' @param include_ci Whether to show confidence intervals
#' @param effect_size_labels Whether to show effect size labels
#' @param response_variable_col Response variable column name
#' @param comparison_type How to handle multi-response
#' @param normalize Whether to normalize values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return ggplot2 object
#' @keywords internal

create_enhanced_radar_plot <- function(data, metric, include_ci = TRUE, 
                                     effect_size_labels = TRUE,
                                     response_variable_col = NULL,
                                     comparison_type = "facet",
                                     normalize = TRUE, title = NULL, subtitle = NULL) {
  
  # Prepare data for radar plot
  plot_data <- data
  
  # Normalize if requested
  if (normalize) {
    if (!is.null(response_variable_col)) {
      plot_data <- plot_data %>%
        dplyr::group_by(.data[[response_variable_col]]) %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd, na.rm = TRUE)) / 
                            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE)),
          ci_lower_norm = if (!is.na(ci_lower[1])) {
            100 * (ci_lower - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA },
          ci_upper_norm = if (!is.na(ci_upper[1])) {
            100 * (ci_upper - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA }
        ) %>%
        dplyr::ungroup()
    } else {
      plot_data <- plot_data %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd, na.rm = TRUE)) / 
                            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE)),
          ci_lower_norm = if (!is.na(ci_lower[1])) {
            100 * (ci_lower - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA },
          ci_upper_norm = if (!is.na(ci_upper[1])) {
            100 * (ci_upper - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA }
        )
    }
    value_col <- "normalized_value"
    ci_lower_col <- "ci_lower_norm"
    ci_upper_col <- "ci_upper_norm"
    value_label <- "Normalized Marginal Effect (0-100)"
  } else {
    plot_data$normalized_value <- plot_data$mean_sd
    plot_data$ci_lower_norm <- plot_data$ci_lower
    plot_data$ci_upper_norm <- plot_data$ci_upper
    value_col <- "normalized_value"
    ci_lower_col <- "ci_lower_norm"
    ci_upper_col <- "ci_upper_norm"
    value_label <- glue::glue("Marginal Standard Deviation ({toupper(metric)})")
  }
  
  # Create coordinates for radar plot
  n_components <- length(unique(plot_data$component))
  angles <- seq(0, 2 * pi, length.out = n_components + 1)[-1]
  
  # Add angles and convert to cartesian coordinates
  plot_data <- plot_data %>%
    dplyr::arrange(component) %>%
    dplyr::mutate(
      angle = rep(angles, length.out = nrow(.)),
      x = .data[[value_col]] * cos(angle),
      y = .data[[value_col]] * sin(angle),
      x_ci_lower = if (!is.na(.data[[ci_lower_col]][1])) {
        .data[[ci_lower_col]] * cos(angle)
      } else { NA },
      y_ci_lower = if (!is.na(.data[[ci_lower_col]][1])) {
        .data[[ci_lower_col]] * sin(angle)
      } else { NA },
      x_ci_upper = if (!is.na(.data[[ci_upper_col]][1])) {
        .data[[ci_upper_col]] * cos(angle)
      } else { NA },
      y_ci_upper = if (!is.na(.data[[ci_upper_col]][1])) {
        .data[[ci_upper_col]] * sin(angle)
      } else { NA }
    )
  
  # Create base plot with grid
  max_value <- max(plot_data[[value_col]], na.rm = TRUE) * 1.1
  
  # Grid circles
  grid_values <- seq(0, max_value, length.out = 5)
  circle_data <- purrr::map_dfr(grid_values, function(r) {
    angles_full <- seq(0, 2 * pi, length.out = 100)
    data.frame(
      x = r * cos(angles_full),
      y = r * sin(angles_full),
      radius = r
    )
  })
  
  # Radial lines
  radial_data <- purrr::map_dfr(angles, function(angle) {
    data.frame(
      x = c(0, max_value * cos(angle)),
      y = c(0, max_value * sin(angle)),
      line_id = angle
    )
  })
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y))
  
  # Add grid
  p <- p + 
    ggplot2::geom_path(
      data = circle_data, 
      ggplot2::aes(x = x, y = y, group = radius),
      color = "grey90", 
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = radial_data,
      ggplot2::aes(x = x, y = y, group = line_id),
      color = "grey90",
      linewidth = 0.5,
      inherit.aes = FALSE
    )
  
  # Add confidence intervals if available
  if (include_ci && !all(is.na(plot_data$ci_lower_norm))) {
    # Create CI ribbons (simplified as lines for radar plot)
    p <- p + 
      ggplot2::geom_path(
        ggplot2::aes(x = x_ci_lower, y = y_ci_lower),
        color = "grey70",
        linetype = "dashed",
        na.rm = TRUE
      ) +
      ggplot2::geom_path(
        ggplot2::aes(x = x_ci_upper, y = y_ci_upper),
        color = "grey70",
        linetype = "dashed", 
        na.rm = TRUE
      )
  }
  
  # Add main radar polygon and points
  if (!is.null(response_variable_col) && comparison_type == "overlay") {
    # Multiple response variables with different colors
    colors <- horizons_colors("response", n = length(unique(plot_data[[response_variable_col]])))
    p <- p + 
      ggplot2::geom_polygon(
        ggplot2::aes(fill = .data[[response_variable_col]], 
                    color = .data[[response_variable_col]]),
        alpha = 0.3,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data[[response_variable_col]]),
        size = 3
      ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_color_manual(values = colors)
  } else {
    # Single color
    color <- horizons_colors("primary")[1]
    p <- p + 
      ggplot2::geom_polygon(
        fill = color,
        color = color,
        alpha = 0.3,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        color = color,
        size = 3
      )
  }
  
  # Add component labels
  label_data <- plot_data %>%
    dplyr::group_by(component) %>%
    dplyr::summarise(
      angle = dplyr::first(angle),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      label_radius = max_value * 1.15,
      x_label = label_radius * cos(angle),
      y_label = label_radius * sin(angle),
      formatted_component = format_component_names(component, "publication")
    )
  
  p <- p + 
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = x_label, y = y_label, label = formatted_component),
      size = 3.5,
      fontface = "bold",
      inherit.aes = FALSE
    )
  
  # Apply theme and styling
  p <- p + 
    horizons_theme(plot_type = "radar") +
    ggplot2::coord_fixed()
  
  # Add titles
  if (is.null(title)) {
    title <- glue::glue("Enhanced Component Effects Analysis ({toupper(metric)})")
  }
  if (is.null(subtitle)) {
    subtitle <- value_label
    if (include_ci) {
      subtitle <- paste(subtitle, glue::glue("({ci_level*100}% CI shown)"))
    }
  }
  
  p <- p + ggplot2::labs(title = title, subtitle = subtitle)
  
  # Add faceting for multi-response if requested
  if (!is.null(response_variable_col) && comparison_type == "facet") {
    p <- p + ggplot2::facet_wrap(~ .data[[response_variable_col]])
  }
  
  return(p)
}


#' Create Enhanced Bar Plot
#'
#' Creates horizontal bar plot with confidence intervals and effect size labels.
#'
#' @param data Enhanced marginal effects data
#' @param metric Performance metric
#' @param include_ci Whether to show confidence intervals
#' @param effect_size_labels Whether to show effect size labels
#' @param response_variable_col Response variable column name
#' @param comparison_type How to handle multi-response
#' @param normalize Whether to normalize values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return ggplot2 object
#' @keywords internal

create_enhanced_bar_plot <- function(data, metric, include_ci = TRUE,
                                   effect_size_labels = TRUE,
                                   response_variable_col = NULL,
                                   comparison_type = "facet",
                                   normalize = TRUE, title = NULL, subtitle = NULL) {
  
  # Prepare data
  plot_data <- data
  
  # Normalize if requested
  if (normalize) {
    if (!is.null(response_variable_col)) {
      plot_data <- plot_data %>%
        dplyr::group_by(.data[[response_variable_col]]) %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd, na.rm = TRUE)) / 
                            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE)),
          ci_lower_norm = if (!is.na(ci_lower[1])) {
            100 * (ci_lower - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA },
          ci_upper_norm = if (!is.na(ci_upper[1])) {
            100 * (ci_upper - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA }
        ) %>%
        dplyr::ungroup()
    } else {
      plot_data <- plot_data %>%
        dplyr::mutate(
          normalized_value = 100 * (mean_sd - min(mean_sd, na.rm = TRUE)) / 
                            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE)),
          ci_lower_norm = if (!is.na(ci_lower[1])) {
            100 * (ci_lower - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA },
          ci_upper_norm = if (!is.na(ci_upper[1])) {
            100 * (ci_upper - min(mean_sd, na.rm = TRUE)) / 
            (max(mean_sd, na.rm = TRUE) - min(mean_sd, na.rm = TRUE))
          } else { NA }
        )
    }
    value_col <- "normalized_value"
    ci_lower_col <- "ci_lower_norm"
    ci_upper_col <- "ci_upper_norm"
    value_label <- "Normalized Marginal Effect (0-100)"
  } else {
    plot_data$normalized_value <- plot_data$mean_sd
    plot_data$ci_lower_norm <- plot_data$ci_lower
    plot_data$ci_upper_norm <- plot_data$ci_upper
    value_col <- "normalized_value"
    ci_lower_col <- "ci_lower_norm"
    ci_upper_col <- "ci_upper_norm"
    value_label <- glue::glue("Marginal Standard Deviation ({toupper(metric)})")
  }
  
  # Order components by effect size
  plot_data <- plot_data %>%
    dplyr::arrange(desc(.data[[value_col]])) %>%
    dplyr::mutate(
      component_formatted = format_component_names(component, "publication"),
      component_formatted = factor(component_formatted, levels = unique(component_formatted))
    )
  
  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[value_col]], y = component_formatted))
  
  # Add bars
  if (!is.null(response_variable_col) && comparison_type == "overlay") {
    colors <- horizons_colors("response", n = length(unique(plot_data[[response_variable_col]])))
    p <- p + 
      ggplot2::geom_col(
        ggplot2::aes(fill = .data[[response_variable_col]]),
        position = "dodge",
        color = "black",
        linewidth = 0.5
      ) +
      ggplot2::scale_fill_manual(values = colors)
  } else {
    color <- horizons_colors("primary")[1]
    p <- p + 
      ggplot2::geom_col(
        fill = color,
        color = "black",
        linewidth = 0.5
      )
  }
  
  # Add confidence intervals if available
  if (include_ci && !all(is.na(plot_data[[ci_lower_col]]))) {
    if (!is.null(response_variable_col) && comparison_type == "overlay") {
      p <- p + 
        ggplot2::geom_errorbar(
          ggplot2::aes(xmin = .data[[ci_lower_col]], xmax = .data[[ci_upper_col]],
                      color = .data[[response_variable_col]]),
          position = ggplot2::position_dodge(width = 0.9),
          width = 0.2,
          linewidth = 1
        )
    } else {
      p <- p + 
        ggplot2::geom_errorbar(
          ggplot2::aes(xmin = .data[[ci_lower_col]], xmax = .data[[ci_upper_col]]),
          width = 0.2,
          linewidth = 1,
          color = "black"
        )
    }
  }
  
  # Add effect size labels if requested
  if (effect_size_labels && "effect_size_label" %in% names(plot_data)) {
    p <- p + 
      ggplot2::geom_text(
        ggplot2::aes(label = effect_size_label),
        hjust = -0.1,
        size = 3,
        fontface = "bold"
      )
  }
  
  # Apply theme
  p <- p + 
    horizons_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")
    )
  
  # Add labels
  if (is.null(title)) {
    title <- glue::glue("Enhanced Component Effects Analysis ({toupper(metric)})")
  }
  if (is.null(subtitle)) {
    subtitle <- value_label
    if (include_ci) {
      subtitle <- paste(subtitle, glue::glue("({ci_level*100}% CI shown)"))
    }
  }
  
  p <- p + 
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = value_label,
      y = "Pipeline Component"
    )
  
  # Add faceting for multi-response if requested
  if (!is.null(response_variable_col) && comparison_type == "facet") {
    p <- p + ggplot2::facet_wrap(~ .data[[response_variable_col]], scales = "free_x")
  }
  
  return(p)
}