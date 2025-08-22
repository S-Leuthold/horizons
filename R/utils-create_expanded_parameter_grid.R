#' Create Expanded Parameter Grid Around Best Values
#'
#' @description
#' Creates a small parameter grid centered around best parameter values from 
#' previous tuning results. Respects parameter bounds and generates reasonable
#' variations for warm-starting Bayesian optimization.
#'
#' @param best_params A single-row tibble containing the best parameter values
#' @param param_set A dials parameter set object defining parameter bounds and types
#' @param grid_size Integer. Target size for the expanded grid (default: 20)
#'
#' @return A tibble containing parameter combinations for grid search
#'
#' @details
#' This function takes optimal parameters from a previous tuning run and creates
#' a small grid of nearby parameter combinations. For each parameter type:
#' - **Numeric parameters**: Creates range around best value (±10-20%)
#' - **Integer parameters**: Creates discrete steps around best value
#' - **Choice parameters**: Includes best choice plus nearby options
#' 
#' The expansion respects parameter bounds defined in the dials parameter set.
#'
#' @export

create_expanded_parameter_grid <- function(best_params, param_set, grid_size = 20) {
  
  # Validate inputs
  if (!is.data.frame(best_params) || nrow(best_params) != 1) {
    cli::cli_abort("best_params must be a single-row data frame")
  }
  
  # Get parameter names and their properties
  param_names <- param_set$name
  
  # Check that all required parameters exist in best_params
  missing_params <- setdiff(param_names, names(best_params))
  if (length(missing_params) > 0) {
    cli::cli_abort("Missing parameters in best_params: {.val {missing_params}}")
  }
  
  # Initialize list to store parameter ranges
  param_ranges <- list()
  
  # Process each parameter
  for (param_name in param_names) {
    
    # Get the best value for this parameter
    best_value <- best_params[[param_name]]
    
    # Get parameter info from param_set
    param_info <- param_set[param_set$name == param_name, ]
    param_object <- param_info$object[[1]]
    
    # Handle different parameter types
    if (inherits(param_object, "quant_param")) {
      
      # Quantitative parameter (numeric or integer)
      param_range <- param_object$range
      param_min <- param_range$lower
      param_max <- param_range$upper
      
      if (param_object$type == "double") {
        
        # Numeric parameter - create range around best value
        expansion_factor <- 0.15  # ±15% around best value
        raw_min <- best_value * (1 - expansion_factor)
        raw_max <- best_value * (1 + expansion_factor)
        
        # Respect parameter bounds
        expanded_min <- max(raw_min, param_min)
        expanded_max <- min(raw_max, param_max)
        
        # Create 5 values in this range
        param_ranges[[param_name]] <- seq(expanded_min, expanded_max, length.out = 5)
        
      } else {
        
        # Integer parameter - create discrete steps
        step_size <- max(1, round(best_value * 0.1))  # 10% step or minimum 1
        
        raw_values <- c(
          best_value - 2 * step_size,
          best_value - step_size, 
          best_value,
          best_value + step_size,
          best_value + 2 * step_size
        )
        
        # Respect parameter bounds and ensure integers
        valid_values <- round(pmax(pmin(raw_values, param_max), param_min))
        param_ranges[[param_name]] <- unique(valid_values)
      }
      
    } else if (inherits(param_object, "qual_param")) {
      
      # Qualitative parameter - include best value and some alternatives
      all_values <- param_object$values
      best_index <- which(all_values == best_value)
      
      if (length(all_values) <= 3) {
        # If few options, use all
        param_ranges[[param_name]] <- all_values
      } else {
        # Include best value plus 2-3 nearby options
        n_extra <- min(2, length(all_values) - 1)
        
        # Get indices around the best value
        indices_to_include <- best_index
        for (i in 1:n_extra) {
          # Add values before and after if they exist
          if (best_index - i > 0) indices_to_include <- c(indices_to_include, best_index - i)
          if (best_index + i <= length(all_values)) indices_to_include <- c(indices_to_include, best_index + i)
          if (length(indices_to_include) >= 4) break  # Don't go overboard
        }
        
        param_ranges[[param_name]] <- all_values[sort(unique(indices_to_include))]
      }
    }
  }
  
  # Create expanded grid
  expanded_grid <- expand.grid(param_ranges, stringsAsFactors = FALSE)
  
  # Convert to tibble and ensure proper column types
  expanded_grid <- tibble::as_tibble(expanded_grid)
  
  # If grid is larger than target size, sample proportionally
  if (nrow(expanded_grid) > grid_size) {
    
    # Always include the exact best parameters as first row
    best_row <- best_params
    
    # Calculate how many additional rows to sample
    sample_prop <- (grid_size - 1) / nrow(expanded_grid)  # -1 to account for best_row
    
    # Sample remaining rows proportionally
    remaining_rows <- expanded_grid %>%
      # Remove the exact best combination if it exists
      dplyr::anti_join(best_params, by = param_names) %>%
      dplyr::slice_sample(prop = sample_prop)
    
    # Combine best row with sampled rows
    expanded_grid <- dplyr::bind_rows(best_row, remaining_rows)
  }
  
  return(expanded_grid)
}