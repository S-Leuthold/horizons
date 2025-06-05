#' Build Full Parameter Grid for Ensemble Model Evaluation
#'
#' Constructs a tibble of modeling configurations by expanding all combinations of:
#' model type, response transformation, spectral preprocessing, response variable,
#' and covariate inclusion logic. This function standardizes input validation,
#' supports optional covariate subset expansion, and returns a structure compatible
#' with `run_batch_models()`.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success
#' @importFrom rlang .data
#'
#' @param models Character vector of model types. Accepted values include:
#'   "Partial Least Squares Regression", "Cubist", "Memory Based Learning",
#'   "Random Forest", "Support Vector Machine", and "Bagged Neural Network".
#'
#' @param transformations Character vector of response transformations. Options include:
#'   "No Transformation", "Log Transformation", and "Square Root Transformation".
#'
#' @param preprocessing Character vector of spectral preprocessing methods. Options include:
#'   "No Preprocessing", "Savitzky Golay - 0 Deriv", "Savitzky Golay - 1 Deriv",
#'   "Savitzky Golay - 2 Deriv", "Standard Normal Variate - Savitzky Golay - 0 Deriv",
#'   and "Standard Normal Variate - Savitzky Golay - 1 Deriv".
#' @param variables Character vector of response variable names.
#' @param include_covariates Logical. If `TRUE`, covariate combinations will be included in the grid.
#' @param covariate_data Optional data frame of covariates (must include `Sample_ID`). Required if `include_covariates = TRUE`.
#' @param expand_covariate_grid Logical. If `TRUE`, generate all covariate subsets; if `FALSE`, use singletons + no covariates.
#'
#' @return A tibble of all modeling configurations with the following columns:
#' \describe{
#'   \item{model}{Character. Model type.}
#'   \item{transformation}{Character. Response transformation.}
#'   \item{preprocessing}{Character. Spectral preprocessing method.}
#'   \item{covariates}{List-column of covariates to include (or `NULL`).}
#'   \item{include_covariates}{Logical. Whether covariates are included.}
#'   \item{variable}{Character. Name of response variable.}
#' }
#'
#' @section Covariate Expansion Logic:
#' If `include_covariates = TRUE`, the function will extract covariate names
#' from `covariate_data` and generate:
#' \itemize{
#'   \item All subsets (if `expand_covariate_grid = TRUE`)
#'   \item Singleton + "No Covariates" (if `FALSE`)
#' }
#' If `include_covariates = FALSE`, the covariate column will be `NULL` for all rows.
#'
#' @seealso [run_batch_models()], [safe_run_model()]
#' @keywords internal

build_model_grid <- function(models,
                             transformations,
                             preprocessing,
                             variables,
                             include_covariates,
                             covariate_data,
                             expand_covariate_grid) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate Inputs
  ## ---------------------------------------------------------------------------

  accepted_models <- c("Partial Least Squares Regression",
                       "Cubist",
                       "Memory Based Learning",
                       "Random Forest",
                       "Support Vector Machine",
                       "Bagged Neural Network")

  accepted_preprocessing <- c("No Preprocessing",
                              "Savitzky Golay - 0 Deriv",
                              "Savitzky Golay - 1 Deriv",
                              "Savitzky Golay - 2 Deriv",
                              "Standard Normal Variate - Savitzky Golay - 0 Deriv",
                              "Standard Normal Variate - Savitzky Golay - 1 Deriv")

  accepted_transformations <- c("No Transformation",
                                "Log Transformation",
                                "Square Root Transformation")

  invalid_models          <- setdiff(models, accepted_models)
  invalid_transformations <- setdiff(transformations, accepted_transformations)
  invalid_preprocessing   <- setdiff(preprocessing, accepted_preprocessing)

  if (length(invalid_models) > 0) {
    cli::cli_alert_danger("Invalid model(s) detected: {.val {invalid_models}}")
    cli::cli_alert_info("Accepted models are: {.val {accepted_models}}")
    stop("Aborting: invalid model input.")
  }

  if (length(invalid_transformations) > 0) {
    cli::cli_alert_danger("Invalid transformation(s) detected: {.val {invalid_transformations}}")
    cli::cli_alert_info("Accepted transformations are: {.val {accepted_transformations}}")
    stop("Aborting: invalid transformation input.")
  }

  if (length(invalid_preprocessing) > 0) {
    cli::cli_alert_danger("Invalid preprocessing method(s): {.val {invalid_preprocessing}}")
    cli::cli_alert_info("Accepted methods are: {.val {accepted_preprocessing}}")
    stop("Aborting: invalid preprocessing input.")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Construct Covariate Combinations
  ## ---------------------------------------------------------------------------

  if (isTRUE(include_covariates)) {
    covariate_names <- setdiff(names(covariate_data), "Sample_ID")
    if (expand_covariate_grid) {
      covariate_combinations <- purrr::map(0:length(covariate_names), ~ combn(covariate_names, m = .x, simplify = FALSE)) %>%
        purrr::flatten() %>%
        purrr::map(~ if (length(.x) == 0) "No Covariates" else .x)
    } else {
      covariate_combinations <- c("No Covariates", as.list(covariate_names))
    }
  } else {
    covariate_combinations <- list("No Covariates")
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Expand Model Grid
  ## ---------------------------------------------------------------------------

  expand_grid(Model          = models,
              Transformation = transformations,
              Preprocessing  = preprocessing,
              Covariates     = covariate_combinations,
              Variable       = variables) %>%
    dplyr::mutate(Include_Covariates = purrr::map_lgl(Covariates, ~ !identical(.x, "No Covariates")),
                  Covariates         = purrr::map(Covariates, ~ if (identical(.x, "No Covariates")) NULL else .x)) %>%
    dplyr::rename(model              = Model,
                  transformation     = Transformation,
                  preprocessing      = Preprocessing,
                  covariates         = Covariates,
                  variable           = Variable,
                  include_covariates = Include_Covariates) -> model_grid

  ## ---------------------------------------------------------------------------
  ## Step 4: Output Message + Return
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Generated {.val {nrow(model_grid)}} model configurations across {.val {length(models)}} models, {.val {length(transformations)}} transformations, and {.val {length(covariate_combinations)}} covariate settings.")

  return(model_grid)
}
