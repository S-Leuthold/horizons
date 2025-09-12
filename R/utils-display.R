#' Display Name Utilities for Model Configurations
#'
#' @description
#' Functions for converting technical model configuration identifiers into
#' human-readable display names. Provides a centralized, reliable approach
#' for consistent configuration display across the package.
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @keywords internal

#' Create Display Lookup for Configuration Grid
#'
#' @description
#' Generates human-readable display names for a configuration grid by applying
#' the readable name functions to each technical identifier. Creates a lookup
#' table that can be joined with configs for reliable display without parsing.
#'
#' @param config_grid A tibble with model configuration columns:
#'   model, transformation, preprocessing, feature_selection, covariates
#'
#' @return A tibble with workflow_id and corresponding display columns:
#'   workflow_id, model_display, transformation_display, preprocessing_display,
#'   feature_selection_display, covariates_display
#'
#' @keywords internal
create_display_lookup <- function(config_grid) {
  
  config_grid %>%
    dplyr::mutate(
      # Generate workflow_id for each config
      workflow_id = purrr::pmap_chr(
        list(model, transformation, preprocessing, feature_selection, covariates),
        ~ clean_workflow_id(
          model = ..1,
          transformation = ..2, 
          preprocessing = ..3,
          feature_selection = ..4,
          covariates = ..5
        )
      ),
      
      # Generate display names
      model_display = purrr::map_chr(model, get_readable_model_name),
      transformation_display = purrr::map_chr(transformation, get_readable_transformation_name),
      preprocessing_display = purrr::map_chr(preprocessing, get_readable_preprocessing_name),
      feature_selection_display = purrr::map_chr(feature_selection, get_readable_feature_selection_name),
      covariates_display = purrr::map_chr(covariates, create_readable_covariates_string)
    ) %>%
    dplyr::select(workflow_id, dplyr::ends_with("_display"))
}

#' Create Readable Covariates String
#'
#' @description  
#' Converts a vector of covariate identifiers to a human-readable string
#' with proper separator formatting. Handles NULL and empty inputs gracefully.
#'
#' @param covariate_vector Character vector of covariate identifiers, or NULL
#'
#' @return Character string with human-readable covariate names joined with " + "
#'
#' @keywords internal
create_readable_covariates_string <- function(covariate_vector) {
  
  if (is.null(covariate_vector) || length(covariate_vector) == 0) {
    return("None")
  }
  
  # Handle list column case (if covariates are stored as list)
  if (is.list(covariate_vector) && length(covariate_vector) == 1) {
    covariate_vector <- covariate_vector[[1]]
  }
  
  # Convert each covariate to readable name
  readable_names <- sapply(covariate_vector, get_readable_covariate_name, USE.NAMES = FALSE)
  
  # Join with + separator
  paste(readable_names, collapse = " + ")
}