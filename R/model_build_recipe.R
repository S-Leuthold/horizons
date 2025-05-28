#' Build Modeling Recipe with Spectral and Covariate Transformations
#'
#' Constructs a `recipe` object for use in tidymodels workflows. This function applies
#' user-defined response transformations (e.g., log, square root), spectral preprocessing
#' via custom steps (e.g., SNV + SG), optional PCA-based dimensionality reduction, and
#' covariate injection from an external dataset. Designed to be modular and composable
#' for ensemble modeling workflows.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom recipes recipe update_role update_role_requirements has_role all_predictors all_outcomes
#' @importFrom recipes step_log step_sqrt step_BoxCox step_pca
#'
#' @param input_data A data frame with spectral predictors and a response variable named `Response`.
#'                   Must include a `Sample_ID` column to enable covariate alignment.
#' @param spectral_transformation Character string specifying spectral preprocessing method.
#'        Valid options include:
#'   \itemize{
#'     \item{"No Preprocessing"}
#'     \item{"Savitzky Golay - 0 Deriv"}
#'     \item{"Savitzky Golay - 1 Deriv"}
#'     \item{"Standard Normal Variate - Savitzky Golay - 0 Deriv"}
#'     \item{"Standard Normal Variate - Savitzky Golay - 1 Deriv"}
#'   }
#' @param response_transformation Character string specifying transformation to apply to the response variable.
#'        Valid options include:
#'   \itemize{
#'     \item{"No Transformation"}
#'     \item{"Log Transformation"}
#'     \item{"Square Root Transformation"}
#'   }
#' @param covariate_selection Character vector of covariate names to include. Use `"No Covariates"` or `NULL` to exclude.
#' @param covariate_data Optional data frame of covariates (must include a `Sample_ID` column). Required if covariates are selected.
#'
#' @return A `recipes::recipe` object with all preprocessing and transformation steps applied.
#'
#' @seealso \code{\link{step_transform_spectra}}, \code{\link{step_add_covariates}}, \code{\link{build_model_grid}}
#'
#' @examples
#' \dontrun{
#' # Example usage
#' input_df <- dplyr::tibble(
#'   Sample_ID = 1:3,
#'   `Dim.1` = rnorm(3),
#'   `Dim.2` = rnorm(3),
#'   Response = c(2.1, 3.5, 1.8)
#' )
#'
#' covariates_df <- dplyr::tibble(
#'   Sample_ID = 1:3,
#'   clay = c(10, 20, 30),
#'   pH = c(6.5, 6.8, 7)
#' )
#'
#' build_recipe(input_data              = input_df,
#'              spectral_transformation = "Savitzky Golay - 0 Deriv",
#'              response_transformation = "Log Transformation",
#'              covariate_selection     = c("clay", "pH"),
#'              covariate_data          = covariates_df)
#' }
#'
#' @keywords internal

build_recipe <- function(input_data,
                         spectral_transformation,
                         response_transformation,
                         covariate_selection = NULL,
                         covariate_data      = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  if (!is.data.frame(input_data)) {
    li::cli_alert_danger("▶ build_recipe: Input is not a data frame.")
    stop("Aborting: Invalid input_data.")
  }

  if (!"Sample_ID" %in% names(input_data)) {
    cli::cli_alert_danger("▶ build_recipe: Sample_ID column is missing.")
    stop("Aborting: Sample_ID required.")
  }

  if (is.null(covariate_selection) ||
      length(covariate_selection) == 0 ||
      identical(covariate_selection, "No Covariates")) {
    covariate_selection <- NULL
  } else {
    if (is.null(covariate_data)) {
      cli::cli_alert_danger("▶ build_recipe: Covariates requested, but no covariate_data supplied.")
      stop("Aborting: Missing covariate_data.")
    }

    covariate_selection <- as.character(unlist(covariate_selection))
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

  switch(response_transformation,
         "No Transformation"          = model_recipe,
         "Log Transformation"         = model_recipe %>% recipes::step_log(all_outcomes(), skip = TRUE),
         "Square Root Transformation" = model_recipe %>% recipes::step_sqrt(all_outcomes(), skip = TRUE),
         "Box-Cox Transformation"     = model_recipe %>% recipes::step_BoxCox(all_outcomes(), skip = TRUE),
         cli::cli_abort("Unsupported {.field response transformation}: {.val {response_transformation}}")) -> model_recipe

  ## ---------------------------------------------------------------------------
  ## Step 4: Spectral Transformation
  ## ---------------------------------------------------------------------------

  model_recipe %>%
    step_transform_spectra(all_predictors(),
                           preprocessing = spectral_transformation) %>%
    recipes::update_role_requirements(role = "predictor") -> model_recipe

  ## ---------------------------------------------------------------------------
  ## Step 5: PCA Normalization
  ## ---------------------------------------------------------------------------

  model_recipe %>%
    recipes::step_pca(recipes::all_predictors(),
                      threshold = 0.995,
                      options   = list(scale. = TRUE,
                                       center = TRUE)) -> model_recipe

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
