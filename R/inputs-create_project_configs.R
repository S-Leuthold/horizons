#' Create Model Configuration Grid for Horizons Workflows
#'
#' Builds a model configuration grid by combining model types, response
#' transformations, preprocessing pipelines, and covariate combinations.
#' Covariates may be predicted or fetched from soil, climate, and spatial domains.
#'
#' @param project_data A dataframe with Project and Sample_ID columns.
#' @param models Character vector of model names (e.g., "random_forest", "cubist").
#' @param transformations Character vector of outcome transformations.
#' @param preprocessing Character vector of preprocessing pipeline names.
#' @param soil_covariates Optional character vector (or "all") of soil covariates to predict.
#' @param climate_covariates Optional character vector (or "all") of climate covariates to fetch.
#' @param spatial_covariates Optional character vector of spatial covariates to fetch.
#' @param include_covariates Logical. Whether to include covariates in modeling grid.
#' @param expand_covariate_grid Logical. If TRUE, generate all possible covariate subsets.
#'
#' @return A tibble of model configurations, one per row, with a list-column of covariates.
#' @export
#'
#' @examples
#' create_project_configurations(
#'   project_data = project_data,
#'   models       = c("xgboost", "cubist"),
#'   transformations = c("none", "log"),
#'   preprocessing   = c("SNV", "SG-1D"),
#'   soil_covariates = "all",
#'   climate_covariates = c("MAT", "MAP"),
#'   spatial_covariates = NULL
#' )
create_project_configurations <- function(project_data,
                                          models,
                                          transformations,
                                          preprocessing,
                                          soil_covariates       = NULL,
                                          climate_covariates    = NULL,
                                          spatial_covariates    = NULL,
                                          expand_covariate_grid = TRUE,
                                          verbose               = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input Validation
  ## ---------------------------------------------------------------------------



  ## ---------------------------------------------------------------------------
  ## Step 1: Predict Soil Covariates
  ## ---------------------------------------------------------------------------

  if (is.character(soil_covariates) && length(soil_covariates) == 1 && soil_covariates == "all") {

    soil_covariates <- c("clay", "sand", "silt", "phh2o", "ocd", "cec", "bdod")

  }

  #-----------------------------------------------------------------------------

  if (!is.null(soil_covariates)) {

    cli::cli_h2("Predicting soil biophysical covariates: {.val {soil_covariates}}")

    safely_execute(expr = {predict_covariates(covariates = soil_covariates,
                                              input_data = project_data)},
                   default_value = NULL,
                   error_message = "Soil covariate prediction failed") -> soil_covs

    if (verbose){
      cli::cli_h2("Prediction Statistics")
      print(soil_covs[[2]])
    }
  }


  ## ---------------------------------------------------------------------------
  ## Step 2: Fetch Climate Covariates
  ## ---------------------------------------------------------------------------

  if (is.character(climate_covariates) && length(climate_covariates) == 1 && climate_covariates == "all") {

    climate_covariates <- c("MAT", "MAP", "PET", "AI", "GDD", "Precip_Seasonality")

  }

  # ----------------------------------------------------------------------------

  if (!is.null(climate_covariates)) {

  safely_execute(expr = {fetch_climate_covariates(input_data = project_data) %>%
                          dplyr::select(Project,
                                        Sample_ID,
                                        dplyr::all_of(climate_covariates))},
                 default_value = NULL,
                 error_message = "Failure fetching weather covariates") -> climate_covs
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Fetch Spatial Covariates
  ## ---------------------------------------------------------------------------

  spatial_covariates <- NULL

  # ----------------------------------------------------------------------------

  if (!is.null(spatial_covariates)) {

    safely_execute(expr = {fetch_spatial_covariates(input_data = project_data) %>%
        dplyr::select(Project,
                      Sample_ID,
                      dplyr::all_of(spatial_covariates))},
        default_value = NULL,
        error_message = "Failure fetching spatial covariates") -> spatial_covs

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Merge Covariates
  ## ---------------------------------------------------------------------------

  list(soil_covs[[1]],
       climate_covs,
       spatial_covs) %>%
    purrr::compact() %>%
    purrr::reduce(dplyr::left_join, by = c("Project", "Sample_ID")) -> covariate_data

  ## ---------------------------------------------------------------------------
  ## Step 5: Create Covariate Combinations
  ## ---------------------------------------------------------------------------

  if (isTRUE(include_covariates) && exists("covariate_data")) {

    covariate_names <- setdiff(names(covariate_data), c("Project", "Sample_ID"))

    if (isTRUE(expand_covariate_grid)) {

      purrr::map(0:length(covariate_names), ~ utils::combn(covariate_names, m = .x, simplify = FALSE)) %>%
        purrr::flatten() %>%
        purrr::map(~ if (length(.x) == 0) "No Covariates" else .x) -> covariate_combos

    } else {

      covariate_combos <- list("No Covariates", covariate_names)

    }
  } else {
    covariate_combos <- list("No Covariates")
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Build Configuration Grid
  ## ---------------------------------------------------------------------------

  tidyr::crossing(model          = models,
                  transformation = transformations,
                  preprocessing  = preprocessing,
                  covariates     = covariate_combos) %>%
    dplyr::mutate(covariates = purrr::map(covariates, ~ if (identical(.x, "No Covariates")) character(0) else .x)) -> model_configs

  return(list(project_configurations = model_configs,
              covariate_data         = covariate_data))
}

# create_project_configurations(project_data = test_data,
#                               models = c("random_forest", "cubist"),
#                               transformations = "Log Transformation",
#                               preprocessing = c("deriv2", "snv"),
#                               soil_covariates = c("pH", "Clay", "CEC"),
#                               climate_covariates = NULL,
#                               spatial_covariates = NULL,
#                               expand_covariate_grid = TRUE)
