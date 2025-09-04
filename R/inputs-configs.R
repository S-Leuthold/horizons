#' Create Model Configuration Grid
#'
#' @description
#' Generates a comprehensive grid of model configurations by creating all combinations
#' of model types, response transformations, spectral preprocessing methods, feature
#' selection techniques, and covariate sets. This function establishes the complete
#' experimental design space for systematic model evaluation and comparison.
#' 
#' @details
#' The function generates configurations through a multi-step process:
#' \enumerate{
#'   \item Creates all possible covariate combinations (including empty set)
#'   \item Forms the Cartesian product of all experimental factors
#'   \item Assigns unique identifiers to each configuration
#'   \item Returns a structured tibble ready for model evaluation workflows
#' }
#' 
#' The covariate combination logic generates all subsets of the provided covariates,
#' including the empty set (no covariates). For example, with covariates c("clay", "sand"),
#' the function creates: none, clay, sand, clay+sand.
#' 
#' This comprehensive approach ensures systematic evaluation across the full
#' experimental space while maintaining clear traceability of model configurations.
#'
#' @param models Character vector. Model types to include.
#'   Default: c("plsr", "random_forest", "cubist", "xgboost", "lightgbm")
#' @param transformations Character vector. Response variable transformations.
#'   Default: c("none", "sqrt", "log")
#' @param preprocessing Character vector. Spectral preprocessing methods.
#'   Default: c("raw", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2")
#' @param feature_selection Character vector. Feature selection methods.
#'   Default: c("none", "vip", "boruta", "rfe")
#' @param soil_covariates Character vector. Soil property covariates to include
#'   (e.g., c("clay", "sand", "pH", "CEC")). Use NULL for none. Default: NULL
#' @param climate_covariates Character vector. Climate covariates to include
#'   (e.g., c("MAT", "MAP", "AI", "PET")). Use NULL for none. Default: NULL
#' @param spatial_covariates Character vector. Spatial covariates to include
#'   (e.g., c("elevation", "slope", "aspect")). Use NULL for none. Default: NULL
#' @param verbose Logical. Print summary statistics. Default: TRUE
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{config_id}{Character. Unique identifier (e.g., "config_0001")}
#'     \item{model}{Character. Model algorithm (e.g., "plsr", "random_forest")}
#'     \item{transformation}{Character. Response variable transformation}
#'     \item{preprocessing}{Character. Spectral preprocessing method}
#'     \item{feature_selection}{Character. Feature selection algorithm}
#'     \item{covariate_set}{Character. Descriptive name of covariate combination}
#'     \item{covariates}{List. Vector of covariate names (NULL if none)}
#'   }
#'   
#'   The number of rows equals the product of all input vector lengths multiplied
#'   by the number of covariate combinations (2^n where n = total covariates).
#'
#' @examples
#' \dontrun{
#' # Simple configuration
#' configs <- create_configs(
#'   models = c("plsr", "random_forest"),
#'   transformations = c("none", "sqrt"),
#'   preprocessing = c("raw", "snv"),
#'   feature_selection = c("none"),
#'   soil_covariates = c("clay", "sand", "pH"),
#'   climate_covariates = c("MAT", "MAP", "AI")
#' )
#'
#' # All combinations of multiple covariate types
#' configs_expanded <- create_configs(
#'   models = c("plsr"),
#'   soil_covariates = c("clay", "sand"),
#'   climate_covariates = c("MAT", "MAP")
#' )
#' }
#'
#' @seealso 
#' \code{\link{create_dataset}} for preparing modeling data,
#' \code{\link{evaluate_models_local}} for running configurations,
#' \code{\link{finalize_dataset}} for data quality control
#' 
#' @family inputs
#' @keywords experimental-design model-configuration
#'
#' @export
create_configs <- function(models                  = c("plsr", "random_forest", "cubist",
                                                       "xgboost", "lightgbm"),
                          transformations         = c("none", "sqrt", "log"),
                          preprocessing           = c("raw", "snv", "deriv1", "deriv2",
                                                     "snv_deriv1", "snv_deriv2"),
                          feature_selection       = c("none", "vip", "boruta", "rfe"),
                          soil_covariates         = NULL,
                          climate_covariates      = NULL,
                          spatial_covariates      = NULL,
                          verbose                 = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  # Validate covariate inputs
  if (!is.null(soil_covariates) && (!is.character(soil_covariates) || length(soil_covariates) == 0)) {

    cli::cli_abort("▶ create_configs: soil_covariates must be a character vector or NULL")

  }

  if (!is.null(climate_covariates) && (!is.character(climate_covariates) || length(climate_covariates) == 0)) {

    cli::cli_abort("▶ create_configs: climate_covariates must be a character vector or NULL")

  }

  if (!is.null(spatial_covariates) && (!is.character(spatial_covariates) || length(spatial_covariates) == 0)) {

    cli::cli_abort("▶ create_configs: spatial_covariates must be a character vector or NULL")

  }

  # Display configuration summary
  config_info <- list(
    "Model types" = paste0(format_metric(length(models), "count"), " (", paste(head(models, 3), collapse = ", "), if (length(models) > 3) "..." else "", ")"),
    "Transformations" = paste0(format_metric(length(transformations), "count"), " types"),
    "Preprocessing" = paste0(format_metric(length(preprocessing), "count"), " methods"),
    "Feature selection" = paste0(format_metric(length(feature_selection), "count"), " methods"),
    "Covariate types" = if (length(c(soil_covariates, climate_covariates, spatial_covariates)) == 0) "None" else 
                       paste(c(if (!is.null(soil_covariates)) "soil", if (!is.null(climate_covariates)) "climate", if (!is.null(spatial_covariates)) "spatial"), collapse = ", "),
    "Total covariates" = format_metric(length(c(soil_covariates, climate_covariates, spatial_covariates)), "count")
  )
  
  display_config_summary("Model Configuration Grid Generation", config_info, verbose)

  ## ---------------------------------------------------------------------------
  ## Step 2: Generate All Covariate Combinations
  ## ---------------------------------------------------------------------------

  # Combine all covariates into single pool
  all_covariates <- c(soil_covariates, climate_covariates, spatial_covariates)

  if (length(all_covariates) == 0) {

    # No covariates specified - just use empty set
    covariate_sets <- list(none = NULL)

  } else {

    if (verbose) {

      cli::cli_text(format_tree_item(paste0("⟳ Generating all subsets of ", length(all_covariates), " covariates..."), 
                                   level = 0, is_last = FALSE, symbol = NULL))

    }

    # Generate all possible subsets (including empty set)
    covariate_sets <- list(none = NULL)

    # Generate all non-empty subsets
    for (n_covariates in 1:length(all_covariates)) {

      subsets <- utils::combn(all_covariates, n_covariates, simplify = FALSE)

      # Name each subset by concatenating covariate names
      subset_names <- sapply(subsets, function(x) paste(x, collapse = "_"))
      names(subsets) <- subset_names

      covariate_sets <- c(covariate_sets, subsets)

    }

    if (verbose) {

      cli::cli_text(format_tree_item(paste0("✓ Created ", length(covariate_sets), " covariate combinations"), 
                                   level = 0, is_last = TRUE))

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Create Base Configuration Grid
  ## ---------------------------------------------------------------------------

  base_grid <- tidyr::crossing(
    model             = models,
    transformation    = transformations,
    preprocessing     = preprocessing,
    feature_selection = feature_selection
  )

  ## ---------------------------------------------------------------------------
  ## Step 4: Add Covariate Sets
  ## ---------------------------------------------------------------------------

  # Create a tibble from covariate_sets
  covariate_df <- tibble::tibble(
    covariate_set = names(covariate_sets),
    covariates = covariate_sets
  )

  # Cross with base grid
  config_grid <- tidyr::crossing(base_grid, covariate_df)


  ## ---------------------------------------------------------------------------
  ## Step 5: Add Configuration IDs
  ## ---------------------------------------------------------------------------

  config_grid <- config_grid %>%
    dplyr::mutate(
      config_id = paste0("config_", sprintf("%04d", dplyr::row_number()))
    ) %>%
    dplyr::select(config_id, dplyr::everything())

  ## ---------------------------------------------------------------------------
  ## Step 6: Summary and Return
  ## ---------------------------------------------------------------------------

  # Display final results summary
  if (verbose) {
    
    # Calculate configuration space breakdown
    n_covariate_sets <- length(unique(config_grid$covariate_set))
    base_combinations <- length(models) * length(transformations) * length(preprocessing) * length(feature_selection)
    
    results_info <- list(
      "Total configurations" = format_metric(nrow(config_grid), "count"),
      "Base combinations" = format_metric(base_combinations, "count"),
      "Covariate sets" = format_metric(n_covariate_sets, "count"),
      "Grid structure" = paste0(length(models), "×", length(transformations), "×", 
                               length(preprocessing), "×", length(feature_selection), 
                               "×", n_covariate_sets)
    )
    
    display_operation_results("Configuration grid", results_info, timing = NULL, "complete", verbose)
    
  }

  return(config_grid)

}
