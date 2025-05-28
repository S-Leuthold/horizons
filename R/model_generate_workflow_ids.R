#' Generate Clean Workflow Identifiers
#'
#' Constructs standardized and compact workflow ID strings from modeling metadata,
#' including model type, response transformation, spectral preprocessing, and covariate specification.
#' Useful for labeling workflows in `workflowsets::workflow_set()` and downstream model tracking.
#'
#' Each element in the output is formatted as:
#' \code{<index>_<model>_<transformation>_<preprocessing>_<covariates>}
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_replace_all
#'
#' @param model Character vector of model names. Supported values include:
#'   \itemize{
#'     \item{"Partial Least Squares Regression"}
#'     \item{"Cubist"}
#'     \item{"Random Forest"}
#'     \item{"Support Vector Machine"}
#'     \item{"Bagged Neural Network"}
#'   }
#' @param transformation Character vector of response transformation labels. Typical values:
#'   \itemize{
#'     \item{"No Transformation"}
#'     \item{"Log Transformation"}
#'     \item{"Square Root Transformation"}
#'     \item{"Box-Cox Transformation"}
#'   }
#' @param preprocessing Character vector of spectral preprocessing methods. Supported:
#'   \itemize{
#'     \item{"No Preprocessing"}
#'     \item{"Savitzky Golay - 0 Deriv"}
#'     \item{"Savitzky Golay - 1 Deriv"}
#'     \item{"Standard Normal Variate - Savitzky Golay - 0 Deriv"}
#'     \item{"Standard Normal Variate - Savitzky Golay - 1 Deriv"}
#'   }
#' @param covariates List-column of character vectors representing covariate combinations per workflow.
#'        Can include NULL or "No Covariates" to indicate absence of covariates.
#'
#' @return A character vector of unique workflow ID strings.
#'
#' @examples
#' \dontrun{
#' clean_workflow_id(
#'   model = c("Cubist", "Random Forest"),
#'   transformation = c("Log Transformation", "No Transformation"),
#'   preprocessing = c("Savitzky Golay - 0 Deriv", "No Preprocessing"),
#'   covariates = list(c("Soil_pH", "CEC"), NULL)
#' )
#' }
#' @seealso [workflowsets::workflow_set()]
#' @keywords internal

clean_workflow_id <- function(model,
                              transformation,
                              preprocessing,
                              covariates) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Clean up the model names
  ## ---------------------------------------------------------------------------

  model_abbrev <- dplyr::case_when(
    model == "Partial Least Squares Regression" ~ "PLSR",
    model == "Cubist"                           ~ "Cubist",
    model == "Random Forest"                    ~ "RF",
    model == "Support Vector Machine"           ~ "SVM",
    model == "Bagged Neural Network"            ~ "BNN",
    TRUE                                        ~ stringr::str_replace_all(model, "\\s+", "_")
  )

  ## ---------------------------------------------------------------------------
  ## Step 2: Clean up the transformations
  ## ---------------------------------------------------------------------------

  transf_abbrev <- dplyr::case_when(
    transformation == "No Transformation"          ~ "NoTrans",
    transformation == "Log Transformation"         ~ "Log",
    transformation == "Square Root Transformation" ~ "Sqrt",
    transformation == "Box-Cox Transformation"     ~ "BoxCox",
    TRUE                                           ~ stringr::str_replace_all(transformation, "\\s+", "_")
  )

  ## ---------------------------------------------------------------------------
  ## Step 3: Clean up the preprocessing
  ## ---------------------------------------------------------------------------

  preprocess_abbrev <- dplyr::case_when(
    preprocessing == "No Preprocessing"                                      ~ "NoPreprocess",
    preprocessing == "Savitzky Golay - 0 Deriv"                               ~ "SG0",
    preprocessing == "Savitzky Golay - 1 Deriv"                               ~ "SG1",
    preprocessing == "Savitzky Golay - 2 Deriv"                               ~ "SG2",
    preprocessing == "Standard Normal Variate - Savitzky Golay - 0 Deriv"     ~ "SNVSG0",
    preprocessing == "Standard Normal Variate - Savitzky Golay - 1 Deriv"     ~ "SNVSG1",
    TRUE                                                                      ~ stringr::str_replace_all(preprocessing, "\\s+", "_")
  )

  ## ---------------------------------------------------------------------------
  ## Step 4: Format covariate list into label-friendly strings
  ## ---------------------------------------------------------------------------

  purrr::map_chr(covariates, ~ {
    if (is.null(.x) || identical(.x, "No Covariates")) {
      "NoCovs"
    } else {
      paste(sort(make.names(.x)), collapse = "+")
    }
  }) -> covar_abbrev

  ## ---------------------------------------------------------------------------
  ## Step 5: Combine all pieces into indexed workflow ID
  ## ---------------------------------------------------------------------------

  index <- seq_along(model)

  paste0(index, "_",
         model_abbrev, "_",
         transf_abbrev, "_",
         preprocess_abbrev, "_",
         covar_abbrev) -> workflow_id

  return(workflow_id)
}
