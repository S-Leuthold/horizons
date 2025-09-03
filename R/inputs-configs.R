#' Create Model Configuration Grid
#'
#' @description
#' Generates a structured grid of model configurations by combining model types,
#' response transformations, spectral preprocessing methods, feature selection,
#' and covariate sets. This creates the full experimental design for model evaluation.
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
#' @return A tibble with columns:
#'   - config_id: Unique identifier for each configuration
#'   - model: Model type
#'   - transformation: Response transformation
#'   - preprocessing: Spectral preprocessing
#'   - feature_selection: Feature selection method
#'   - covariate_set: Name of covariate set
#'   - covariates: List column with actual covariate names
#'
#' @examples
#' \dontrun{
#' # Simple configuration
#' configs <- create_configs(
#'   models = c("plsr", "random_forest"),
#'   transformations = c("none", "sqrt"),
#'   preprocessing = c("raw", "snv"),
#'   feature_selection = c("none"),
#'   covariate_sets = list(
#'     none = NULL,
#'     climate = c("MAT", "MAP", "AI"),
#'     soil = c("clay", "sand", "pH")
#'   )
#' )
#'
#' # Expand all covariate combinations
#' configs_expanded <- create_configs(
#'   models = c("plsr"),
#'   covariate_sets = list(
#'     all = c("MAT", "MAP", "clay", "sand")
#'   ),
#'   expand_covariates = TRUE
#' )
#' }
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

      cli::cli_alert_info("Generating all subsets of {.val {length(all_covariates)}} covariates")

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

      cli::cli_alert_info("Created {.val {length(covariate_sets)}} covariate combinations")

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

  if (verbose) {

    cli::cli_alert_success("Created {.val {nrow(config_grid)}} model configurations")

  }

  return(config_grid)

}
