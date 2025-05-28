#' Build Model Parameter Grid for Ensemble Evaluation
#'
#' Constructs a grid of modeling configurations by expanding all combinations of:
#' model type, response transformation, spectral preprocessing, and optional covariate
#' inclusion. This grid serves as the foundation for constructing workflow sets in
#' the full ensemble evaluation pipeline.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param models Character vector of model types. Supported values include:
#'   \itemize{
#'     \item{"Partial Least Squares Regression"}
#'     \item{"Cubist"}
#'     \item{"Memory Based Learning"}
#'     \item{"Random Forest"}
#'     \item{"Support Vector Machine"}
#'     \item{"Bagged Neural Network"}
#'   }
#' @param transformations Character vector of response transformations. Options include:
#'   \itemize{
#'     \item{"No Transformation"}
#'     \item{"Log Transformation"}
#'     \item{"Square Root Transformation"}
#'   }
#' @param preprocessing Character vector of spectral preprocessing methods. Options include:
#'   \itemize{
#'     \item{"No Preprocessing"}
#'     \item{"Savitzky Golay - 0 Deriv"}
#'     \item{"Savitzky Golay - 1 Deriv"}
#'     \item{"Standard Normal Variate - Savitzky Golay - 0 Deriv"}
#'     \item{"Standard Normal Variate - Savitzky Golay - 1 Deriv"}
#'   }
#' @param variables Character vector of response variable names.
#' @param include_covariates Logical. If TRUE, covariates will be included in the grid.
#' @param covariate_data Optional data frame of covariates (must contain a `Sample_ID` column).
#' @param expand_covariate_grid Logical. If TRUE, includes all combinations of covariates.
#'
#' @return A tibble of all model parameter combinations, including model type,
#' transformation, preprocessing, covariate subset (or NULL/"No Covariates"), and response variable.
#'
#' @seealso \code{\link{clean_workflow_id}}, \code{\link{build_recipe}}, \code{\link{full_model_evaluation}}
#'
##' @examples
#' \dontrun{
#' build_model_grid(
#'   models = c("Cubist", "Random Forest"),
#'   transformations = c("No Transformation"),
#'   preprocessing = c("Savitzky Golay - 0 Deriv", "Standard Normal Variate - Savitzky Golay - 0 Deriv"),
#'   variables = "SOC",
#'   include_covariates = TRUE,
#'   covariate_data = tibble::tibble(Sample_ID = 1:3, clay = c(10, 20, 30), pH = c(6.5, 6.8, 7)),
#'   expand_covariate_grid = FALSE
#' )
#' }
#'
#' @keywords internal

build_model_grid <- function(models,
                             transformations,
                             preprocessing,
                             variables,
                             include_covariates,
                             covariate_data,
                             expand_covariate_grid) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Check Inputs
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

  # ------------------------------------------------------------------------------
  # Validation: Check Models
  # ------------------------------------------------------------------------------

  invalid_models <- setdiff(models, accepted_models)

  if (length(invalid_models) > 0) {

    cli::cli_alert_danger("Invalid model(s) detected: {.val {invalid_models}}")
    cli::cli_alert_info("Accepted models are: {.val {accepted_models}}")
    stop("Model grid construction aborted due to invalid model input.")

  }

  # ------------------------------------------------------------------------------
  # Validation: Check Transformations
  # ------------------------------------------------------------------------------

  invalid_transformations <- setdiff(transformations, accepted_transformations)

  if (length(invalid_transformations) > 0) {

    cli::cli_alert_danger("Invalid transformation(s) detected: {.val {invalid_transformations}}")
    cli::cli_alert_info("Accepted transformations are: {.val {accepted_transformations}}")
    stop("Model grid construction aborted due to invalid transformation input.")

  }

  # ------------------------------------------------------------------------------
  # Validation: Check Preprocessing Methods
  # ------------------------------------------------------------------------------

  invalid_preprocessing <- setdiff(preprocessing, accepted_preprocessing)

  if (length(invalid_preprocessing) > 0) {

    cli::cli_alert_danger("Invalid preprocessing method(s) detected: {.val {invalid_preprocessing}}")
    cli::cli_alert_info("Accepted preprocessing methods are: {.val {accepted_preprocessing}}")
    stop("Model grid construction aborted due to invalid preprocessing input.")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Construct Covariate Combinations
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Stage 1: Expand covariate subsets
    ## -------------------------------------------------------------------------

    if (include_covariates == TRUE) {

      covariate_names <- setdiff(names(covariate_data), "Sample_ID")

      if (expand_covariate_grid == TRUE) {

        num_covar <- length(covariate_names)

        covariate_combinations <- purrr::map(0:num_covar, ~ combn(x = covariate_names,
                                                                  m = .x,
                                                                  simplify = FALSE)) %>%
          purrr::flatten() %>%
          purrr::map(~ if (length(.x) == 0) "No Covariates" else .x)

      } else {
        covariate_combinations <- c(list("No Covariates"),
                                    purrr::map(covariate_names, ~ .x))
      }

    } else {
      covariate_combinations <- list(NULL)
    }

    ## -------------------------------------------------------------------------
    ## Stage 2: Expand parameter grid
    ## -------------------------------------------------------------------------

    tidyr::expand_grid(Model          = models,
                       Transformation = transformations,
                       Preprocessing  = preprocessing,
                       Covariates     = covariate_combinations,
                       Variable       = variables) -> model_grid

  ## ---------------------------------------------------------------------------
  ## Step 3: Return Grid
  ## ---------------------------------------------------------------------------

  return(model_grid)

}
