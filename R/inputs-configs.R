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
#' @param models Character vector. Model types to include (required).
#'   Common choices: c("plsr", "random_forest", "cubist", "xgboost", "svm-rbf")
#' @param transformations Character vector. Response variable transformations (required).
#'   Common choices: c("none", "sqrt", "log")
#' @param preprocessing Character vector. Spectral preprocessing methods (required).
#'   Common choices: c("raw", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2")
#' @param feature_selection Character vector. Feature selection methods (required).
#'   Common choices: c("none", "vip", "boruta", "rfe")
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

create_configs <- function(models,
                           transformations,
                           preprocessing,
                           feature_selection,
                           soil_covariates         = NULL,
                           climate_covariates      = NULL,
                           spatial_covariates      = NULL,
                           verbose                 = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input Validation
  ## ---------------------------------------------------------------------------

  ## First, validate the soil covariates input ---------------------------------

  if (!is.null(soil_covariates) && (!is.character(soil_covariates) || length(soil_covariates) == 0)) cli::cli_abort("soil_covariates must be a character vector or NULL")

  ## Then, do the same for the climate -----------------------------------------

  if (!is.null(climate_covariates) && (!is.character(climate_covariates) || length(climate_covariates) == 0)) cli::cli_abort("climate_covariates must be a character vector or NULL")

  ## Finally, validate the spatial covariates ---------------------------------

  if (!is.null(spatial_covariates) && (!is.character(spatial_covariates) || length(spatial_covariates) == 0)) cli::cli_abort("spatial_covariates must be a character vector or NULL")

  ## Display configuration summary to the user ---------------------------------

  if (verbose) {
    cli::cli_text("{.strong Model Configuration Grid Generation}")
    cli::cli_text("├─ Model types: {length(models)} ({paste(head(models, 3), collapse = ', ')}{if (length(models) > 3) '...' else ''})")
    cli::cli_text("├─ Transformations: {length(transformations)} types")
    cli::cli_text("├─ Preprocessing: {length(preprocessing)} methods")
    cli::cli_text("├─ Feature selection: {length(feature_selection)} methods")
    cli::cli_text("├─ Covariate types: {if (length(c(soil_covariates, climate_covariates, spatial_covariates)) == 0) 'None' else paste(c(if (!is.null(soil_covariates)) 'soil', if (!is.null(climate_covariates)) 'climate', if (!is.null(spatial_covariates)) 'spatial'), collapse = ', ')}")
    cli::cli_text("└─ Total covariates: {length(c(soil_covariates, climate_covariates, spatial_covariates))}")
    cli::cli_text("")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Generate All Covariate Combinations
  ## ---------------------------------------------------------------------------

  ## Combine all covariates into single pool -----------------------------------

  all_covariates <- c(soil_covariates, climate_covariates, spatial_covariates)

  if (length(all_covariates) == 0) {

    ## If no covariates, easy peasy, return null -------------------------------

    covariate_sets <- list(none = NULL)

  } else {

    if (verbose) cli::cli_text("├─ Generating all subsets of {length(all_covariates)} covariates...")

    ## Start with empty set ---------------------------------------------------

    covariate_sets <- list(none = NULL)

    ## Loop through and generate all non-empty subsets -------------------------

    for (n_covariates in 1:length(all_covariates)) {

      utils::combn(all_covariates,
                   n_covariates,
                   simplify = FALSE) -> subsets

      ## Name each subset by concatenating covariate names ---------------------

      subset_names   <- sapply(subsets, function(x) paste(x, collapse = "_"))
      names(subsets) <- subset_names
      covariate_sets <- c(covariate_sets, subsets)

    }

    ## Report out --------------------------------------------------------------

    if (verbose) cli::cli_text("└─ Created {length(covariate_sets)} covariate combinations")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Create Base Configuration Grid
  ## ---------------------------------------------------------------------------

  tidyr::crossing(model             = models,
                  transformation    = transformations,
                  preprocessing     = preprocessing,
                  feature_selection = feature_selection) -> base_grid

  ## ---------------------------------------------------------------------------
  ## Step 3: Add Covariate Sets
  ## ---------------------------------------------------------------------------

  # Create tibble with just the covariates as a list column
  covariate_df <- tibble::tibble(
    covariates = unname(covariate_sets)  # Remove names to avoid confusion
  )

  # Cross with base grid
  tidyr::crossing(base_grid, covariate_df) -> config_grid

  ## ---------------------------------------------------------------------------
  ## Step 4: Add Configuration IDs
  ## ---------------------------------------------------------------------------

  config_grid %>%
    dplyr::mutate(config_id = paste0("config_", sprintf("%04d", dplyr::row_number()))) %>%
    dplyr::select(config_id, dplyr::everything()) -> config_grid

  ## ---------------------------------------------------------------------------
  ## Step 5: Summary and Return
  ## ---------------------------------------------------------------------------

  if (verbose) {

    n_covariate_sets  <- length(unique(lapply(config_grid$covariates, function(x) paste(x, collapse="_"))))
    base_combinations <- length(models) * length(transformations) * length(preprocessing) * length(feature_selection)

    cli::cli_text("")
    cli::cli_text("{.strong Configuration Grid Complete}")
    cli::cli_text("├─ Total configurations: {nrow(config_grid)}")
    cli::cli_text("├─ Base combinations: {base_combinations}")
    cli::cli_text("├─ Covariate sets: {n_covariate_sets}")
    cli::cli_text("└─ Grid structure: {length(models)}×{length(transformations)}×{length(preprocessing)}×{length(feature_selection)}×{n_covariate_sets}")
    cli::cli_text("")

  }

  return(config_grid)

}
