#' Construct Compact Workflow Identifiers
#'
#' Generates standardized workflow ID strings from model configuration metadata:
#' model type, response transformation, spectral preprocessing, and covariates.
#' These IDs are useful for tracking workflow configurations in stacking pipelines,
#' model audits, and file naming conventions.
#'
#' Each ID follows the format:
#' \code{<model>_<transformation>_<preprocessing>_<covariates>}
#'
#' @param model Character vector of model names. Recommended values include:
#'   \itemize{
#'     \item{"random_forest"}
#'     \item{"cubist"}
#'     \item{"xgboost"}
#'     \item{"elastic_net"}
#'     \item{"svm_rbf"}
#'     \item{"mars"}
#'     \item{"plsr"}
#'     \item{"mlp_nn"}
#'   }
#' @param transformation Character vector of response transformation labels. Supported values:
#'   \itemize{
#'     \item{"No Transformation"}
#'     \item{"Log Transformation"}
#'     \item{"Square Root Transformation"}
#'     \item{"Box-Cox Transformation"}
#'   }
#' @param preprocessing Character vector of spectral preprocessing methods. Supported values:
#'   \itemize{
#'     \item{"raw"}
#'     \item{"sg"}
#'     \item{"snv"}
#'     \item{"deriv1"}
#'     \item{"deriv2"}
#'     \item{"snv_deriv1"}
#'     \item{"snv_deriv2"}
#'   }
#' @param covariates Character vector or list of character vectors naming covariates.
#'        If `NULL`, `"NoCovs"` will be used.
#'
#' @return A character vector of compact workflow ID strings.
#'
#' @seealso \code{\link{parse_workflow_id}}
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace_all
#' @importFrom purrr map_chr
#' @export



clean_workflow_id <- function(model,
                              transformation,
                              preprocessing,
                              feature_selection,
                              covariates) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Standardize transformation labels
  ## ---------------------------------------------------------------------------

  transf_abbrev <- dplyr::case_when(
    transformation == "No Transformation"          ~ "NoTrans",
    transformation == "Log Transformation"         ~ "Log",
    transformation == "Square Root Transformation" ~ "Sqrt",
    transformation == "Box-Cox Transformation"     ~ "BoxCox",
    TRUE                                           ~ stringr::str_replace_all(transformation, "\\s+", "_")
  )

  ## ---------------------------------------------------------------------------
  ## Step 2: Standardize preprocessing labels
  ## ---------------------------------------------------------------------------

  preprocess_abbrev <- dplyr::case_when(
    preprocessing == "raw"         ~ "Raw",
    preprocessing == "sg"          ~ "SG",
    preprocessing == "snv"         ~ "SNV",
    preprocessing == "deriv1"      ~ "D1",
    preprocessing == "deriv2"      ~ "D2",
    preprocessing == "snv_deriv1"  ~ "SNVD1",
    preprocessing == "snv_deriv2"  ~ "SNVD2",
    TRUE                           ~ stringr::str_replace_all(preprocessing, "\\s+", "_")
  )

  ## ---------------------------------------------------------------------------
  ## Step 3: Standardize feature selection
  ## ---------------------------------------------------------------------------

  feature_abbrev <- dplyr::case_when(
    feature_selection == "pca"         ~ "PCA",
    feature_selection == "correlation" ~ "Corr",
    feature_selection == "boruta"      ~ "Boruta",
    feature_selection == "shap"        ~ "SHAP",
    feature_selection == "cars"        ~ "CARS",
    feature_selection == "none"        ~ "NoFeatSel",
    TRUE                               ~ stringr::str_replace_all(feature_selection, "\\s+", "_")
  )


  ## ---------------------------------------------------------------------------
  ## Step 3: Format covariate label
  ## ---------------------------------------------------------------------------

  # Handle covariates as a single vector, not mapping over them
  if (is.null(covariates) || identical(covariates, "No Covariates") || length(covariates) == 0) {
    covar_abbrev <- "NoCovs"
  } else {
    covar_abbrev <- paste(sort(make.names(covariates)), collapse = "+")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Combine components into workflow IDs
  ## ---------------------------------------------------------------------------

  paste(model,
        transf_abbrev,
        preprocess_abbrev,
        feature_abbrev,
        covar_abbrev,
        sep = "_")
}

