#' Build Tidymodels Recipe for Spectral + Covariate Modeling
#'
#' Constructs a preprocessing pipeline using the \pkg{recipes} framework. Applies user-specified
#' transformations to the response and spectra, optionally adds covariates, and includes PCA
#' dimensionality reduction. This function supports stacking workflows and harmonized modeling
#' across different preprocessing combinations.
#'
#' @param input_data A data frame containing a `Response` column, spectral predictors
#'   (named as wavenumbers, e.g., `600`, `602`, ...), and a `Sample_ID` column.
#' @param spectral_transformation Character. Spectral preprocessing pipeline. One of:
#'   \itemize{
#'     \item{"raw"}
#'     \item{"sg"}
#'     \item{"snv"}
#'     \item{"deriv1"}
#'     \item{"deriv2"}
#'     \item{"snv_deriv1"}
#'     \item{"snv_deriv2"}
#'   }
#' @param response_transformation Character. Transformation applied to response variable. One of:
#'   \itemize{
#'     \item{"No Transformation"}
#'     \item{"Log Transformation"}
#'     \item{"Square Root Transformation"}
#'   }
#' @param covariate_selection Optional character vector of covariates to include (e.g., `"Clay"`, `"pH"`).
#'   Use `"No Covariates"` or `NULL` to exclude.
#' @param covariate_data Optional data frame of covariates (must include `Sample_ID`). Required
#'   if any covariates are selected.
#'
#' @return A \code{recipes::recipe} object containing the full preprocessing pipeline.
#'
#' @details
#' Roles for \code{Sample_ID} and \code{Project} (if present) are set to \code{"id"} and
#' \code{"metadata"}, respectively. Response transformation steps are marked \code{skip = TRUE}
#' to preserve inverse-transform compatibility. Spectral preprocessing is handled by
#' \code{\link{step_transform_spectra}}; covariates are injected using \code{\link{step_add_covariates}}.
#'
#' @seealso \code{\link{step_transform_spectra}}, \code{\link{step_add_covariates}}, \code{\link{build_model_grid}}
#'
#' @importFrom recipes recipe update_role update_role_requirements step_log step_sqrt step_BoxCox step_pca all_outcomes all_predictors
#' @importFrom dplyr select any_of all_of rename
#' @importFrom rlang sym
#' @importFrom cli cli_alert_danger cli_alert_info cli_abort
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
    cli::cli_alert_danger("▶ build_recipe: Input is not a data frame.")
    stop("Aborting: Invalid input_data.")
  }

  if (!"Sample_ID" %in% names(input_data)) {
    cli::cli_alert_danger("▶ build_recipe: Sample_ID column is missing.")
    stop("Aborting: Sample_ID required.")
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
      cli::cli_alert_danger("▶ build_recipe: Covariates requested, but no covariate_data supplied.")
      stop("Aborting: Missing covariate_data.")
    }

    missing_covars <- setdiff(covariate_selection, names(covariate_data))

    if (length(missing_covars) > 0) {
      cli::cli_alert_danger("▶ build_recipe: Missing covariates: {.val {missing_covars}}")
      cli::cli_alert_info("▶ build_recipe: Available: {.val {names(covariate_data)}}")
      stop("Aborting: Covariate mismatch.")
    }

    if (!"Sample_ID" %in% names(covariate_data)) {
      cli::cli_alert_danger("▶ build_recipe: Sample_ID column missing in covariate_data.")
      stop("Aborting: Invalid covariate_data.")
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
                          step_select_correlation(all_predictors(),
                                                  outcome = "Response"),
         "boruta" = model_recipe %>%
                          step_select_boruta(all_predictors(),
                                             outcome = "Response"),
         "cars"   = model_recipe %>%
                          step_select_cars(all_predictors(),
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
