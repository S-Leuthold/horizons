#' Build Tidymodels Recipe for Spectral + Covariate Modeling
#'
#' Constructs a preprocessing pipeline using the recipes framework. Applies user-specified
#' transformations to the response and spectra, optionally adds covariates, and includes PCA
#' dimensionality reduction. This function supports stacking workflows and harmonized modeling
#' across different preprocessing combinations.
#'
#' @param input_data A data frame containing a `Response` column, spectral predictors
#'   (named as wavenumbers, e.g., `600`, `602`, ...), and a `Sample_ID` column.
#' @param spectral_transformation Character. Spectral preprocessing pipeline. One of:
#' * `"raw"`
#' * `"sg"`
#' * `"snv"`
#' * `"deriv1"`
#' * `"deriv2"`
#' * `"snv_deriv1"`
#' * `"snv_deriv2"`
#' @param response_transformation Character. Transformation applied to response variable. One of:
#' * `"No Transformation"`
#' * `"Log Transformation"`
#' * `"Square Root Transformation"`
#' @param covariate_selection Optional character vector of covariates to include (e.g., `"Clay"`, `"pH"`).
#'   Use `"No Covariates"` or `NULL` to exclude.
#' @param covariate_data Optional data frame of covariates (must include `Sample_ID`). Required
#'   if any covariates are selected.
#' @param feature_selection_method Character. Feature selection approach. One of:
#' * `"pca"` - Principal component analysis (99.5% variance threshold)
#' * `"correlation"` - Correlation-based selection
#' * `"boruta"` - Boruta algorithm for feature importance
#' * `"cars"` - Competitive Adaptive Reweighted Sampling
#' * `"none"` - No feature selection
#'
#' @return A `recipes::recipe` object containing the full preprocessing pipeline.
#'
#' @details
#' Roles for `Sample_ID` and `Project` (if present) are set to `"id"` and
#' `"metadata"`, respectively. Response transformation steps are marked `skip = TRUE`
#' to preserve inverse-transform compatibility. Spectral preprocessing is handled by
#' [step_transform_spectra()]; covariates are injected using [step_add_covariates()].
#'
#' @seealso [step_transform_spectra()], [step_add_covariates()], [build_model_grid()]
#'
#' @importFrom recipes recipe update_role update_role_requirements step_log step_sqrt step_BoxCox step_pca all_outcomes all_predictors
#' @importFrom dplyr select any_of all_of rename
#' @importFrom rlang sym
#' @importFrom cli cli_abort
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' build_recipe(
#'   input_data              = df,
#'   spectral_transformation = "snv_deriv1",
#'   response_transformation = "Log Transformation",
#'   covariate_selection     = c("Clay", "pH"),
#'   covariate_data          = covariates
#' )
#' }
#'
#' @keywords internal



build_recipe <- function(input_data,
                         spectral_transformation,
                         response_transformation,
                         feature_selection_method,
                         covariate_selection = NULL,
                         covariate_data      = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation and Data Preparation
  ## ---------------------------------------------------------------------------

  if (!is.data.frame(input_data)) {
    cli::cli_abort("Input must be a data frame")
  }

  if (!"Sample_ID" %in% names(input_data)) {
    cli::cli_abort("Sample_ID column not found in input_data")
  }

  # Data already has "Response" column (renamed in calling function if needed)

  covariate_selection <- as.character(unlist(covariate_selection))

  if (length(covariate_selection) == 0 ||
      all(is.na(covariate_selection)) ||
      all(covariate_selection %in% c("No Covariates", "", "NA"))) {
    covariate_selection <- NULL
  }


  if (!is.null(covariate_selection)) {

    if (is.null(covariate_data)) {
      cli::cli_abort("Covariates requested but covariate_data is NULL")
    }

    missing_covars <- setdiff(covariate_selection, names(covariate_data))

    if (length(missing_covars) > 0) {
      cli::cli_abort(c(
        "Requested covariates not found in covariate_data",
        "x" = "Missing: {.val {missing_covars}}",
        "i" = "Available: {.val {setdiff(names(covariate_data), 'Sample_ID')}}"
      ))
    }

    if (!"Sample_ID" %in% names(covariate_data)) {
      cli::cli_abort("Sample_ID column not found in covariate_data")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Role Assignment
  ## ---------------------------------------------------------------------------

  input_data %>%
    dplyr::select(dplyr::any_of(c("Project",
                                  "Sample_ID",
                                  "Response",
                                  "transformation_applied")),
                  dplyr::any_of(as.character(seq(600, 4000, 2)))) -> input_data

  recipes::recipe(Response ~ ., data = input_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    recipes::update_role(Project, new_role = "metadata") %>%
    recipes::update_role_requirements(role = "id", bake = TRUE) %>%
    recipes::update_role_requirements(role = "metadata", bake = TRUE)-> model_recipe

  ## ---------------------------------------------------------------------------
  ## Step 3: Response Transformation
  ## ---------------------------------------------------------------------------

  switch(tolower(as.character(response_transformation)),
         "none"          = model_recipe,
         "log"           = model_recipe %>% recipes::step_log(recipes::all_outcomes(), skip = TRUE),
         "sqrt"          = model_recipe %>% recipes::step_sqrt(recipes::all_outcomes(), skip = TRUE),
         cli::cli_abort("Unsupported {.field response transformation}: {.val {response_transformation}}")) -> model_recipe

  ## ---------------------------------------------------------------------------
  ## Step 4: Spectral Transformation
  ## ---------------------------------------------------------------------------

  model_recipe %>%
    step_transform_spectra(recipes::all_predictors(),
                           preprocessing = spectral_transformation) -> model_recipe

  ## ---------------------------------------------------------------------------
  ## Step 5: Feature Selection
  ## ---------------------------------------------------------------------------

  switch(as.character(feature_selection_method),

         "pca" = model_recipe %>%
                  recipes::step_pca(recipes::all_predictors(),
                                    threshold = 0.995,
                                    options   = list(scale. = TRUE,
                                                     center = TRUE)),
         "correlation" = model_recipe %>%
                          step_select_correlation(recipes::all_predictors(),
                                                  outcome = "Response"),
         "boruta" = model_recipe %>%
                          step_select_boruta(recipes::all_predictors(),
                                             outcome = "Response"),
         "cars"   = model_recipe %>%
                          step_select_cars(recipes::all_predictors(),
                                           outcome = "Response"),
         "none"   = model_recipe,

         cli::cli_abort("Unsupported {.field feature selection method}: {.val {feature_selection_method}}")) -> model_recipe

  ## ---------------------------------------------------------------------------
  ## Step 6: Covariate Additions
  ## ---------------------------------------------------------------------------

  if (!is.null(covariate_selection)) {

    covariate_data %>%
      dplyr::select(Sample_ID, all_of(covariate_selection)) -> filtered_covariate_data

    model_recipe %>%
      step_add_covariates(covariate_data   = filtered_covariate_data,
                          sample_id_column = "Sample_ID") -> model_recipe
  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Return Final Recipe
  ## ---------------------------------------------------------------------------

  return(model_recipe)

}
