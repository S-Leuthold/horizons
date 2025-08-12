#' Shared Design System for Horizons Visualization Suite
#'
#' Provides consistent theming, color palettes, and data preparation utilities
#' for all component analysis visualization functions. Ensures manuscript-quality
#' consistency across the entire suite.
#'
#' @name visualization_theme
#' @keywords internal

## =============================================================================
## Color Palettes
## =============================================================================

#' Get Horizons Color Palette
#'
#' Returns standardized color palettes for consistent visualization across
#' all component analysis functions. Matches existing biplot and upset plot aesthetics.
#'
#' @param palette Character. Name of color palette to retrieve.
#' @param n Integer. Number of colors needed (for gradients and sequences).
#' @param alpha Numeric. Transparency level (0-1).
#' @return Character vector of hex color codes.
#'
#' @examples
#' \dontrun{
#' # Get primary colors
#' horizons_colors("primary")
#' 
#' # Get response variable colors
#' horizons_colors("response", n = 3)
#' 
#' # Get interaction heatmap colors
#' horizons_colors("interaction", n = 11)
#' }
#'
#' @keywords internal

horizons_colors <- function(palette = "primary", n = NULL, alpha = 1) {
  
  # Define all color palettes
  color_palettes <- list(
    
    # Core brand colors (from existing plots)
    primary = c(
      main = "#818D5D",        # Main green from biplot
      secondary = "#9D4920",    # Orange accent
      tertiary = "#E8B04B",     # Yellow
      quaternary = "#7B68A6",   # Purple  
      neutral = "#A4B494"       # Light green
    ),
    
    # Response variable comparisons (up to 4 variables)
    response = c("#818D5D", "#9D4920", "#E8B04B", "#7B68A6"),
    
    # Component-specific colors
    models = c("#2E8B57", "#228B22", "#32CD32", "#90EE90", "#98FB98", "#F0FFF0"),
    preprocessing = c("#8B4513", "#D2691E", "#CD853F", "#F4A460", "#FFEFD5"),
    transformations = c("#4682B4", "#87CEEB", "#B0C4DE", "#E0F6FF"),
    covariates = c("#9932CC", "#BA55D3", "#DDA0DD", "#E6E6FA"),
    
    # Soil property colors (earth tones)
    soil_properties = c(
      carbon = "#8B4513",       # Saddle brown
      nitrogen = "#2E8B57",     # Sea green
      ph = "#4682B4",           # Steel blue
      texture = "#D2691E",      # Chocolate
      other = "#696969"         # Dim gray
    ),
    
    # Climate variable colors (blues/teals)
    climate = c("#4682B4", "#5F9EA0", "#87CEEB", "#B0C4DE", "#E0F6FF"),
    
    # Interaction heatmap (diverging)
    interaction = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                   "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
    
    # Effect sizes (sequential)
    effect_size = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A",
                   "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
    
    # Performance gradients
    performance_good = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476",
                        "#41AB5D", "#238B45", "#006D2C", "#00441B"),
    performance_bad = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A",
                       "#EF3B2C", "#CB181D", "#A50F15", "#67000D")
  )
  
  # Get requested palette
  if (!palette %in% names(color_palettes)) {
    cli::cli_alert_warning("Unknown palette '{palette}'. Using 'primary'.")
    palette <- "primary"
  }
  
  colors <- color_palettes[[palette]]
  
  # Handle number of colors requested
  if (!is.null(n)) {
    if (length(colors) < n) {
      # Interpolate if we need more colors
      colors <- colorRampPalette(colors)(n)
    } else if (length(colors) > n) {
      # Take first n colors
      colors <- colors[1:n]
    }
  }
  
  # Apply alpha transparency
  if (alpha < 1) {
    colors <- adjustcolor(colors, alpha.f = alpha)
  }
  
  return(unname(colors))
}


#' Get Horizons ggplot2 Theme
#'
#' Returns the standard ggplot2 theme used across all horizons visualizations.
#' Matches the aesthetic of existing biplot and upset plot functions.
#'
#' @param base_size Numeric. Base font size for all text elements.
#' @param base_family Character. Font family to use.
#' @param plot_type Character. Type of plot for minor adjustments ("standard", "heatmap", "radar").
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontrun{
#' ggplot(data, aes(x, y)) + 
#'   geom_point() +
#'   horizons_theme()
#' }
#'
#' @importFrom ggplot2 theme_minimal theme element_rect element_text element_blank element_line
#' @importFrom dplyr case_when
#' @importFrom cli cli_alert_warning cli_abort cli_alert_info
#' @importFrom stats sd
#' @keywords internal

horizons_theme <- function(base_size = 12, base_family = "", plot_type = "standard") {
  
  # Base theme matching existing plots
  base_theme <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Panel styling (matches biplot)
      panel.border = ggplot2::element_rect(fill = "transparent", 
                                          color = "black", 
                                          linewidth = 1),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      
      # Text styling
      axis.title.y = ggplot2::element_text(color = "black", 
                                          size = base_size + 3, 
                                          angle = 90, 
                                          face = "bold"),
      axis.title.x = ggplot2::element_text(color = "black", 
                                          size = base_size + 3, 
                                          face = "bold"),
      axis.text = ggplot2::element_text(color = "black", 
                                       size = base_size),
      
      # Plot titles
      plot.title = ggplot2::element_text(color = "black",
                                        size = base_size + 4,
                                        face = "bold",
                                        hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "black",
                                           size = base_size + 1,
                                           hjust = 0.5),
      
      # Strip text for facets
      strip.text = ggplot2::element_text(size = base_size + 2, 
                                        face = "bold",
                                        color = "black"),
      strip.background = ggplot2::element_rect(fill = "grey90", 
                                              color = "black"),
      
      # Legend
      legend.title = ggplot2::element_text(face = "bold", 
                                          size = base_size + 1),
      legend.text = ggplot2::element_text(size = base_size),
      legend.background = ggplot2::element_rect(fill = "white", 
                                               color = "black"),
      
      # Default legend position
      legend.position = "bottom"
    )
  
  # Plot-specific adjustments
  if (plot_type == "heatmap") {
    base_theme <- base_theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major = ggplot2::element_line(color = "grey95", linewidth = 0.5)
      )
  } else if (plot_type == "radar") {
    base_theme <- base_theme +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  }
  
  return(base_theme)
}


## =============================================================================
## Data Preparation Utilities
## =============================================================================

#' Format Component Names for Display
#'
#' Standardizes component names for consistent display across all visualizations.
#' Converts technical names to publication-ready labels.
#'
#' @param component_names Character vector of component names to format.
#' @param format Character. Output format ("publication", "abbreviated", "technical").
#' @return Character vector of formatted names.
#'
#' @examples
#' \dontrun{
#' format_component_names(c("ModelType", "Feature_Selection"), "publication")
#' }
#'
#' @keywords internal

format_component_names <- function(component_names, format = "publication") {
  
  # Define name mappings
  name_mappings <- list(
    publication = c(
      "ModelType" = "Model Type",
      "Transformation" = "Response Transformation", 
      "Preprocessing" = "Spectral Preprocessing",
      "Feature_Selection" = "Feature Selection",
      "Covariates" = "Environmental Covariates",
      "Response_Variable" = "Response Variable"
    ),
    abbreviated = c(
      "ModelType" = "Model",
      "Transformation" = "Transform",
      "Preprocessing" = "Preprocess", 
      "Feature_Selection" = "Features",
      "Covariates" = "Covariates",
      "Response_Variable" = "Response"
    ),
    technical = c(
      "ModelType" = "ModelType",
      "Transformation" = "Transformation",
      "Preprocessing" = "Preprocessing",
      "Feature_Selection" = "Feature_Selection", 
      "Covariates" = "Covariates",
      "Response_Variable" = "Response_Variable"
    )
  )
  
  # Apply formatting
  if (format %in% names(name_mappings)) {
    mapping <- name_mappings[[format]]
    formatted_names <- ifelse(component_names %in% names(mapping),
                             mapping[component_names],
                             component_names)
    return(unname(formatted_names))
  } else {
    cli::cli_alert_warning("Unknown format '{format}'. Using original names.")
    return(component_names)
  }
}


#' Prepare Multi-Response Data
#'
#' Prepares data for multi-response variable analysis by adding response variable
#' identifiers and ensuring consistent structure across visualization functions.
#'
#' @param data Data frame with model evaluation results.
#' @param response_column Character. Name of column containing response variable identifiers.
#' @param metric_columns Character vector. Names of metric columns to analyze.
#' @param config_column Character. Name of column containing config descriptors.
#' @return Data frame prepared for multi-response analysis.
#'
#' @examples
#' \dontrun{
#' prepared_data <- prepare_multi_response_data(
#'   data = results_data,
#'   response_column = "response_var",
#'   metric_columns = c("rrmse", "rsq", "rmse")
#' )
#' }
#'
#' @keywords internal

prepare_multi_response_data <- function(data, 
                                       response_column = NULL,
                                       metric_columns = c("rrmse", "rsq", "rmse"),
                                       config_column = "config_desc") {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("Data must be a data frame")
  }
  
  if (!config_column %in% names(data)) {
    cli::cli_abort("Config column '{config_column}' not found in data")
  }
  
  # Check for metric columns
  available_metrics <- intersect(metric_columns, names(data))
  if (length(available_metrics) == 0) {
    cli::cli_abort("No metric columns found in data: {metric_columns}")
  }
  
  # Add response variable column if not provided
  if (is.null(response_column) || !response_column %in% names(data)) {
    cli::cli_alert_info("No response variable column specified. Creating default.")
    data$response_variable <- "Default_Response"
    response_column <- "response_variable"
  }
  
  # Ensure response variable is a factor for consistent plotting
  data[[response_column]] <- as.factor(data[[response_column]])
  
  # Add formatted component names for consistency
  data$response_variable_formatted <- format_component_names(
    data[[response_column]], 
    "publication"
  )
  
  # Validate that we have sufficient data for each response variable
  response_counts <- table(data[[response_column]])
  low_count_responses <- names(response_counts)[response_counts < 5]
  
  if (length(low_count_responses) > 0) {
    cli::cli_alert_warning(
      "Low data counts for response variables: {low_count_responses}"
    )
  }
  
  return(data)
}


#' Get Component-Specific Colors
#'
#' Returns appropriate colors for specific component values (e.g., different model types).
#' Ensures consistent color mapping across all visualizations.
#'
#' @param component_values Character vector. Specific values within a component.
#' @param component_type Character. Type of component ("models", "preprocessing", etc.).
#' @return Named character vector of colors.
#'
#' @examples
#' \dontrun{
#' model_colors <- get_component_colors(
#'   c("Cubist", "XGBoost", "Random Forest"), 
#'   "models"
#' )
#' }
#'
#' @keywords internal

get_component_colors <- function(component_values, component_type = "primary") {
  
  # Get base colors for the component type
  base_colors <- horizons_colors(component_type, n = length(component_values))
  
  # Create named vector
  color_mapping <- setNames(base_colors, component_values)
  
  return(color_mapping)
}


#' Create Effect Size Labels
#'
#' Converts numeric effect sizes to interpretable labels following Cohen's conventions
#' adapted for model performance metrics.
#'
#' @param effect_sizes Numeric vector of effect sizes (marginal standard deviations).
#' @param metric Character. Performance metric being analyzed.
#' @param thresholds Named list. Custom thresholds for small/medium/large effects.
#' @return Character vector of effect size labels.
#'
#' @examples
#' \dontrun{
#' labels <- create_effect_size_labels(c(0.01, 0.05, 0.15), "rrmse")
#' }
#'
#' @keywords internal

create_effect_size_labels <- function(effect_sizes, 
                                     metric = "rrmse",
                                     thresholds = NULL) {
  
  # Default thresholds based on metric type
  if (is.null(thresholds)) {
    if (metric %in% c("rrmse", "rmse", "mae")) {
      # Error metrics: smaller effects are more meaningful
      thresholds <- list(small = 0.02, medium = 0.05, large = 0.10)
    } else if (metric %in% c("rsq", "r2")) {
      # R-squared: larger effects needed for meaningful differences  
      thresholds <- list(small = 0.05, medium = 0.10, large = 0.20)
    } else {
      # Generic thresholds
      thresholds <- list(small = 0.02, medium = 0.05, large = 0.10)
    }
  }
  
  # Classify effect sizes
  labels <- dplyr::case_when(
    effect_sizes < thresholds$small ~ "Negligible",
    effect_sizes < thresholds$medium ~ "Small", 
    effect_sizes < thresholds$large ~ "Medium",
    TRUE ~ "Large"
  )
  
  # Convert to ordered factor
  labels <- factor(labels, 
                  levels = c("Negligible", "Small", "Medium", "Large"),
                  ordered = TRUE)
  
  return(labels)
}


## =============================================================================
## Validation Utilities  
## =============================================================================

#' Validate Visualization Input Data
#'
#' Performs comprehensive validation of input data for visualization functions.
#' Ensures data meets requirements and provides helpful error messages.
#'
#' @param data Data frame to validate.
#' @param required_columns Character vector of required column names.
#' @param metric Character. Performance metric column name.
#' @param min_rows Integer. Minimum number of rows required.
#' @return Logical. TRUE if validation passes, throws error otherwise.
#'
#' @keywords internal

validate_visualization_data <- function(data, 
                                       required_columns = c("config_desc"),
                                       metric = "rrmse",
                                       min_rows = 5) {
  
  # Check data structure
  if (!is.data.frame(data)) {
    cli::cli_abort("Input must be a data frame")
  }
  
  # Check minimum rows
  if (nrow(data) < min_rows) {
    cli::cli_abort("Insufficient data: {nrow(data)} rows, minimum {min_rows} required")
  }
  
  # Check required columns
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {missing_cols}")
  }
  
  # Check metric column
  if (!metric %in% names(data)) {
    cli::cli_abort("Metric column '{metric}' not found. Available: {names(data)}")
  }
  
  # Check for missing values in metric
  na_count <- sum(is.na(data[[metric]]))
  if (na_count > 0) {
    cli::cli_alert_warning("{na_count} missing values found in metric '{metric}'")
  }
  
  # Check for sufficient variation in metric
  metric_sd <- sd(data[[metric]], na.rm = TRUE)
  if (metric_sd == 0 || is.na(metric_sd)) {
    cli::cli_abort("No variation in metric '{metric}' - cannot compute marginal effects")
  }
  
  return(TRUE)
}