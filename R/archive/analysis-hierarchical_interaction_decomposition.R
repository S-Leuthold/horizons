#' Hierarchical Interaction Decomposition Analysis
#'
#' Decomposes model performance into main effects, pairwise interactions, 
#' three-way interactions, etc. to understand which interaction levels
#' contribute most to performance variation across component combinations.
#'
#' @param results_data A data frame containing model evaluation results with
#'   `config_desc` column and performance metric columns.
#' @param metric Character. Performance metric to analyze (e.g., "rrmse", "rsq", "rmse").
#' @param covariate_handling Character. How to handle covariate complexity: 
#'   "simplified" (group into categories), "aggregate" (Present/None), or "detailed".
#' @param max_interaction_level Integer. Maximum interaction level to test (2-5).
#'   Higher levels require more data and computational time.
#'
#' @return List containing:
#'   \itemize{
#'     \item `variance_explained`: Data frame showing R² for each interaction level
#'     \item `models`: List of fitted lm() objects for each level
#'     \item `component_data`: Parsed component data used for analysis
#'     \item `anova_results`: ANOVA results showing significance of interaction terms
#'   }
#'
#' @details
#' **Analysis Strategy:**
#' Fits a series of increasingly complex regression models:
#' 1. **Null Model**: Intercept only (baseline)
#' 2. **Main Effects**: Each component individually  
#' 3. **Pairwise Interactions**: All A×B combinations
#' 4. **Three-way Interactions**: All A×B×C combinations
#' 5. **Four-way Interactions**: All A×B×C×D combinations
#' 6. **Five-way Interaction**: A×B×C×D×E (if data allows)
#'
#' **Variance Decomposition:**
#' Shows how much additional variance each interaction level explains:
#' - Main effects: 60% (components matter individually)
#' - Pairwise: +25% (some combinations are synergistic)  
#' - Three-way: +10% (complex interactions exist)
#' - Higher-order: +5% (diminishing returns)
#'
#' **Interpretation Guide:**
#' - High main effects → Focus on individual component selection
#' - High pairwise interactions → Visualize interaction matrices
#' - High three-way+ interactions → Consider network approaches
#'
#' @examples
#' \dontrun{
#' # Analyze interaction structure
#' decomp_results <- hierarchical_interaction_decomposition(
#'   results_data = batch_summary,
#'   metric = "rrmse",
#'   covariate_handling = "simplified",
#'   max_interaction_level = 4
#' )
#' 
#' # View variance explained by each level
#' print(decomp_results$variance_explained)
#' 
#' # Plot variance decomposition
#' plot_interaction_decomposition(decomp_results)
#' }
#'
#' @seealso
#' \code{\link{plot_interaction_decomposition}}, \code{\link{extract_top_interactions}}
#'
#' @importFrom dplyr mutate select filter arrange
#' @importFrom stats lm anova
#' @importFrom cli cli_alert_info cli_alert_warning
#' @importFrom glue glue
#' @importFrom broom glance tidy
#'
#' @family analysis
#' @export

hierarchical_interaction_decomposition <- function(results_data,
                                                 metric = "rrmse",
                                                 covariate_handling = c("simplified", "aggregate", "detailed"),
                                                 max_interaction_level = 4) {
  
  # Match arguments
  covariate_handling <- match.arg(covariate_handling)
  
  cli::cli_alert_info("Starting hierarchical interaction decomposition analysis...")
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation and Data Preparation
  ## ---------------------------------------------------------------------------
  
  safely_execute(
    expr = {
      validate_visualization_data(
        data = results_data,
        required_columns = c("config_desc"),
        metric = metric,
        min_rows = 50  # Need sufficient data for interaction analysis
      )
      
      if (max_interaction_level < 2 || max_interaction_level > 5) {
        cli::cli_abort("max_interaction_level must be between 2 and 5")
      }
      
      TRUE
    },
    default_value = FALSE,
    error_message = "Input validation failed for interaction decomposition"
  ) -> validation_result
  
  if (!validation_result$result) {
    return(NULL)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Parse Components into Analysis Format
  ## ---------------------------------------------------------------------------
  
  cli::cli_alert_info("Parsing component combinations for interaction analysis...")
  
  safely_execute(
    expr = {
      # Reuse existing parsing function but ensure all components are factors
      component_data <- parse_components_for_network(results_data, covariate_handling)
      
      # Convert to factors for proper interaction analysis
      component_data <- component_data %>%
        dplyr::mutate(
          ModelType = as.factor(ModelType),
          Preprocessing = as.factor(Preprocessing),
          Feature_Selection = as.factor(Feature_Selection),
          Transformation = as.factor(Transformation),
          Covariates = as.factor(Covariates),
          # Add performance metric
          Performance = .data[[metric]]
        ) %>%
        # Remove any rows with missing components or performance
        dplyr::filter(
          !is.na(ModelType), !is.na(Preprocessing), !is.na(Feature_Selection),
          !is.na(Transformation), !is.na(Covariates), !is.na(Performance)
        )
      
      component_data
    },
    default_value = NULL,
    error_message = "Failed to parse component combinations"
  ) -> parsing_result
  
  if (is.null(parsing_result$result)) {
    return(NULL)
  }
  
  component_data <- parsing_result$result
  
  cli::cli_alert_info("Parsed {nrow(component_data)} complete configurations")
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Fit Hierarchical Models
  ## ---------------------------------------------------------------------------
  
  cli::cli_alert_info("Fitting hierarchical interaction models...")
  
  # Define model formulas for each level
  formulas <- list(
    null = "Performance ~ 1",
    main_effects = "Performance ~ ModelType + Preprocessing + Feature_Selection + Transformation + Covariates",
    pairwise = "Performance ~ (ModelType + Preprocessing + Feature_Selection + Transformation + Covariates)^2",
    threeway = "Performance ~ (ModelType + Preprocessing + Feature_Selection + Transformation + Covariates)^3"
  )
  
  # Add higher-order interactions if requested
  if (max_interaction_level >= 4) {
    formulas$fourway <- "Performance ~ (ModelType + Preprocessing + Feature_Selection + Transformation + Covariates)^4"
  }
  if (max_interaction_level >= 5) {
    formulas$fiveway <- "Performance ~ (ModelType + Preprocessing + Feature_Selection + Transformation + Covariates)^5"
  }
  
  # Fit models
  models <- list()
  variance_explained <- data.frame()
  
  for (level_name in names(formulas)) {
    cli::cli_alert_info("Fitting {level_name} model...")
    
    model_result <- safely_execute(
      expr = {
        lm(as.formula(formulas[[level_name]]), data = component_data)
      },
      default_value = NULL,
      error_message = glue::glue("Failed to fit {level_name} model")
    )
    
    if (!is.null(model_result$result)) {
      models[[level_name]] <- model_result$result
      
      # Extract R-squared
      r_squared <- summary(model_result$result)$r.squared
      adj_r_squared <- summary(model_result$result)$adj.r.squared
      
      variance_explained <- rbind(variance_explained, data.frame(
        level = level_name,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        stringsAsFactors = FALSE
      ))
      
      cli::cli_alert_info("{level_name}: R² = {round(r_squared, 3)}")
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Calculate Incremental Variance Explained
  ## ---------------------------------------------------------------------------
  
  if (nrow(variance_explained) > 1) {
    variance_explained <- variance_explained %>%
      dplyr::arrange(match(level, names(formulas))) %>%
      dplyr::mutate(
        incremental_r_squared = r_squared - dplyr::lag(r_squared, default = 0),
        percent_variance = r_squared * 100,
        incremental_percent = incremental_r_squared * 100
      )
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 5: ANOVA Comparisons
  ## ---------------------------------------------------------------------------
  
  cli::cli_alert_info("Computing ANOVA comparisons between model levels...")
  
  anova_results <- list()
  
  # Compare each level to the previous level
  for (i in 2:length(models)) {
    if (length(models) >= i) {
      prev_model <- models[[i-1]]
      curr_model <- models[[i]]
      
      anova_result <- safely_execute(
        expr = {
          anova(prev_model, curr_model)
        },
        default_value = NULL,
        error_message = glue::glue("ANOVA comparison failed for {names(models)[i]}")
      )
      
      if (!is.null(anova_result$result)) {
        anova_results[[names(models)[i]]] <- anova_result$result
      }
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 6: Return Results
  ## ---------------------------------------------------------------------------
  
  cli::cli_alert_info("Hierarchical decomposition analysis complete!")
  
  results <- list(
    variance_explained = variance_explained,
    models = models,
    component_data = component_data,
    anova_results = anova_results,
    metric = metric,
    covariate_handling = covariate_handling,
    n_configurations = nrow(component_data)
  )
  
  class(results) <- c("hierarchical_decomp", "list")
  
  return(results)
}


## =============================================================================
## Plotting Function for Hierarchical Decomposition Results
## =============================================================================

#' Plot Hierarchical Interaction Decomposition Results
#'
#' Creates visualization showing variance explained by different interaction levels.
#'
#' @param decomp_results Results from hierarchical_interaction_decomposition()
#' @param plot_type Character. Type of plot: "incremental" (default), "cumulative", or "both"
#' @param title Character. Optional plot title
#'
#' @return ggplot object showing variance decomposition
#' @export

plot_interaction_decomposition <- function(decomp_results, 
                                         plot_type = c("incremental", "cumulative", "both"),
                                         title = NULL) {
  
  plot_type <- match.arg(plot_type)
  
  if (!inherits(decomp_results, "hierarchical_decomp")) {
    cli::cli_abort("Input must be results from hierarchical_interaction_decomposition()")
  }
  
  variance_data <- decomp_results$variance_explained
  
  if (is.null(title)) {
    title <- glue::glue("Interaction Effects on {toupper(decomp_results$metric)} Performance")
  }
  
  # Create level labels
  level_labels <- c(
    "null" = "Null\n(Baseline)",
    "main_effects" = "Main Effects\n(A + B + C + D + E)",
    "pairwise" = "Pairwise\n(A×B + A×C + ...)",
    "threeway" = "Three-way\n(A×B×C + ...)",
    "fourway" = "Four-way\n(A×B×C×D + ...)",
    "fiveway" = "Five-way\n(A×B×C×D×E)"
  )
  
  variance_data <- variance_data %>%
    dplyr::mutate(
      level_label = level_labels[level],
      level_order = match(level, names(level_labels))
    ) %>%
    dplyr::arrange(level_order)
  
  if (plot_type == "incremental") {
    p <- ggplot2::ggplot(variance_data, ggplot2::aes(x = reorder(level_label, level_order))) +
      ggplot2::geom_col(ggplot2::aes(y = incremental_percent), 
                       fill = "steelblue", alpha = 0.7, width = 0.6) +
      ggplot2::geom_text(ggplot2::aes(y = incremental_percent + 1, 
                                     label = paste0("+", round(incremental_percent, 1), "%")),
                        vjust = 0, size = 3.5) +
      ggplot2::labs(
        title = title,
        subtitle = paste("Additional variance explained by each interaction level"),
        x = "Model Complexity Level",
        y = "Additional Variance Explained (%)"
      )
  } else if (plot_type == "cumulative") {
    p <- ggplot2::ggplot(variance_data, ggplot2::aes(x = reorder(level_label, level_order))) +
      ggplot2::geom_col(ggplot2::aes(y = percent_variance), 
                       fill = "darkgreen", alpha = 0.7, width = 0.6) +
      ggplot2::geom_text(ggplot2::aes(y = percent_variance + 2, 
                                     label = paste0(round(percent_variance, 1), "%")),
                        vjust = 0, size = 3.5) +
      ggplot2::labs(
        title = title,
        subtitle = "Cumulative variance explained by interaction level",
        x = "Model Complexity Level", 
        y = "Total Variance Explained (%)"
      )
  }
  
  p <- p +
    horizons_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.subtitle = ggplot2::element_text(color = "gray40")
    )
  
  return(p)
}