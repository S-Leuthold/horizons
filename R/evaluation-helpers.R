#' Helper Functions for Local Model Evaluation
#'
#' @description
#' Collection of utility functions supporting local model evaluation.
#' These functions handle error formatting for failed evaluations.
#'
#' @keywords internal

## Error Handling Helpers -----------------------------------------------------

#' Create Failed Result Row
#' 
#' @description
#' Creates a properly formatted result row for failed model evaluations.
#' Returns a tibble row with NA metrics and detailed error information.
#' 
#' @param config_id Model configuration ID
#' @param config_clean Cleaned configuration data
#' @param error_message Descriptive error message
#' @param workflow_id Optional workflow ID if already created
#' @param error_detail Full error object from safely_execute
#' @param error_stage Stage where error occurred (e.g., "recipe_building", "tuning")
#' @param error_trace Optional trace object for debugging
#' @param warnings List of warning messages captured during execution
#' @param messages List of informational messages captured during execution
#' 
#' @return Tibble row with NA metrics and enriched error information
#' @keywords internal
create_failed_result <- function(config_id,
                                config_clean,
                                error_message,
                                workflow_id = NULL,
                                error_detail = NULL,
                                error_stage = NULL,
                                error_trace = NULL,
                                warnings = NULL,
                                messages = NULL) {
  
  tibble::tibble(
    config_id         = config_id,
    workflow_id       = workflow_id %||% NA_character_,
    model             = config_clean$model,
    transformation    = config_clean$transformation,
    preprocessing     = config_clean$preprocessing,
    feature_selection = config_clean$feature_selection,
    covariates        = if (!is.null(config_clean$covariates)) {
                          paste(config_clean$covariates, collapse = "-")
                        } else {
                          NA_character_
                        },
    best_params       = list(NULL),  # No params for failed models
    rsq               = NA_real_,
    rmse              = NA_real_,
    rrmse             = NA_real_,
    rpd               = NA_real_,
    ccc               = NA_real_,
    mae               = NA_real_,
    grid_seconds      = NA_real_,
    bayes_seconds     = NA_real_,
    total_seconds     = NA_real_,
    status            = "failed",
    error_message     = error_message,
    error_stage       = error_stage %||% "unknown",
    error_class       = if (!is.null(error_detail)) {
                          class(error_detail)[1]
                        } else {
                          NA_character_
                        },
    has_trace         = !is.null(error_trace),
    n_warnings        = length(warnings),
    warning_summary   = if (length(warnings) > 0) {
                          paste(head(warnings, 3), collapse = "; ")
                        } else {
                          NA_character_
                        }
    # Note: fitted_workflow removed to prevent memory leak
  )
}

## Name Translation Helpers ---------------------------------------------------

#' Get Human-Readable Model Name
#' 
#' @description
#' Translates technical model identifiers into human-readable names.
#' 
#' @param model_id Character string with model identifier
#' @return Character string with human-readable model name
#' @keywords internal
get_readable_model_name <- function(model_id) {
  
  model_names <- c(
    "plsr"           = "Partial Least Squares",
    "random_forest"  = "Random Forest",
    "cubist"         = "Cubist",
    "xgboost"        = "XGBoost",
    "lightgbm"       = "LightGBM",
    "elastic_net"    = "Elastic Net",
    "svm_rbf"        = "Support Vector Machine (RBF)",
    "mars"           = "MARS",
    "mlp_nn"         = "Neural Network (MLP)"
  )
  
  model_names[model_id] %||% model_id
}

#' Get Human-Readable Preprocessing Name
#' 
#' @description
#' Translates technical preprocessing identifiers into human-readable names.
#' 
#' @param preprocessing_id Character string with preprocessing identifier
#' @return Character string with human-readable preprocessing name
#' @keywords internal
get_readable_preprocessing_name <- function(preprocessing_id) {
  
  preprocessing_names <- c(
    "raw"          = "Raw Spectra",
    "Raw"          = "Raw Spectra",
    "sg"           = "Savitzky-Golay Smoothing",
    "SG"           = "Savitzky-Golay Smoothing",
    "snv"          = "Standard Normal Variate",
    "SNV"          = "Standard Normal Variate",
    "deriv1"       = "First Derivative",
    "D1"           = "First Derivative",
    "deriv2"       = "Second Derivative",
    "D2"           = "Second Derivative",
    "snv_deriv1"   = "SNV + First Derivative",
    "snv_deriv2"   = "SNV + Second Derivative",
    "SNVD1"        = "SNV + First Derivative",
    "SNVD2"        = "SNV + Second Derivative"
  )
  
  # First try exact match, then lowercase match
  result <- preprocessing_names[preprocessing_id]
  if (is.na(result)) {
    result <- preprocessing_names[tolower(preprocessing_id)]
  }
  result %||% preprocessing_id
}

#' Get Human-Readable Transformation Name
#' 
#' @description
#' Translates technical transformation identifiers into human-readable names.
#' 
#' @param transformation_id Character string with transformation identifier
#' @return Character string with human-readable transformation name
#' @keywords internal
get_readable_transformation_name <- function(transformation_id) {
  
  transformation_names <- c(
    "none"          = "None",
    "notrans"       = "None",
    "sqrt"          = "Square Root",
    "log"           = "Logarithmic",
    # Legacy mappings for backward compatibility
    "NoTrans"       = "None",
    "Sqrt"          = "Square Root", 
    "Log"           = "Logarithmic"
  )
  
  # First try exact match, then lowercase match
  result <- transformation_names[transformation_id]
  if (is.na(result)) {
    result <- transformation_names[tolower(transformation_id)]
  }
  result %||% transformation_id
}

#' Get Human-Readable Feature Selection Name
#' 
#' @description
#' Translates technical feature selection identifiers into human-readable names.
#' 
#' @param feature_selection_id Character string with feature selection identifier
#' @return Character string with human-readable feature selection name
#' @keywords internal
get_readable_feature_selection_name <- function(feature_selection_id) {
  
  feature_selection_names <- c(
    "none"                = "No Selection",
    "None"                = "No Selection",
    "NoFeatSel"           = "No Selection",
    "remove_correlated"   = "Correlation Filter",
    "correlation"         = "Correlation Filter",
    "Corr"                = "Correlation Filter",
    "vip"                 = "Variable Importance (VIP)",
    "rfe"                 = "Recursive Feature Elimination",
    "boruta"              = "Boruta Algorithm",
    "Boruta"              = "Boruta Algorithm",
    "cars"                = "CARS Selection",
    "CARS"                = "CARS Selection",
    "shap"                = "SHAP Values",
    "pca"                 = "Principal Component Analysis",
    "PCA"                 = "Principal Component Analysis"
  )
  
  feature_selection_names[feature_selection_id] %||% feature_selection_id
}

#' Get Human-Readable Covariate Name
#' 
#' @description
#' Translates technical covariate identifiers into human-readable names.
#' 
#' @param covariate_id Character string with covariate identifier
#' @return Character string with human-readable covariate name
#' @keywords internal
get_readable_covariate_name <- function(covariate_id) {
  
  covariate_names <- c(
    # Soil properties
    "clay"       = "Clay Content",
    "sand"       = "Sand Content",
    "silt"       = "Silt Content",
    "ph"         = "pH",
    "pH"         = "pH",
    "phh2o"      = "pH (H2O)",
    "oc"         = "Organic Carbon",
    "ocd"        = "Organic Carbon Density",
    "n"          = "Nitrogen",
    "nitrogen"   = "Nitrogen",
    "cec"        = "CEC",
    "bdod"       = "Bulk Density",
    # Climate variables
    "MAT"        = "Mean Annual Temperature",
    "MAP"        = "Mean Annual Precipitation",
    "PET"        = "Potential Evapotranspiration",
    "AI"         = "Aridity Index",
    "GDD"        = "Growing Degree Days",
    "Precip_Seasonality" = "Precipitation Seasonality",
    # Spatial/terrain
    "elevation"  = "Elevation",
    "Elevation"  = "Elevation",
    "slope"      = "Slope",
    "aspect"     = "Aspect",
    # Legacy/grouped
    "SoilGrids"  = "SoilGrids Data",
    "Temp"       = "Temperature",
    "Precip"     = "Precipitation",
    "Climate"    = "Climate Variables",
    "Soil"       = "Soil Properties",
    "Terrain"    = "Terrain Features",
    "All"        = "All Covariates",
    "AI"         = "Aridity Index",
    "NoCovs"     = "None"
  )
  
  covariate_names[covariate_id] %||% covariate_id
}

#' Parse and Format Workflow ID
#' 
#' @description
#' Parses a workflow ID string and returns human-readable components.
#' Handles the special case of "random_forest" which contains an underscore.
#' 
#' @param workflow_id Character string with workflow ID
#' @return Named list with readable components
#' @keywords internal
parse_workflow_id <- function(workflow_id) {
  
  # Define patterns for each component (including NA as a valid value)
  model_pattern <- "^(plsr|random_forest|elastic_net|cubist|xgboost|lightgbm|svm_rbf|mars|mlp_nn)"
  transformation_pattern <- "_(NA|NoTrans|Log|Sqrt)"
  preprocessing_pattern <- "_(NA|Raw|raw|SNV|snv|SNVD1|SNVD2|SG|sg|D1|D2|deriv1|deriv2|snv_deriv1|snv_deriv2)"
  feature_selection_pattern <- "_(NA|NoFeatSel|None|PCA|Boruta|CARS|Corr|VIP|RFE|remove_correlated|vip|rfe|boruta|cars)"
  covariates_pattern <- "_(NA|NoCovs|[^_]+)$"  # Everything after the last underscore including NA and NoCovs
  
  # Extract components using regex
  model <- stringr::str_extract(workflow_id, model_pattern)
  model <- stringr::str_replace(model, "^", "")  # Remove leading ^
  
  transformation <- stringr::str_extract(workflow_id, transformation_pattern)
  transformation <- stringr::str_replace(transformation, "^_", "")  # Remove leading _
  
  preprocessing <- stringr::str_extract(workflow_id, preprocessing_pattern)
  preprocessing <- stringr::str_replace(preprocessing, "^_", "")  # Remove leading _
  
  feature_selection <- stringr::str_extract(workflow_id, feature_selection_pattern)
  feature_selection <- stringr::str_replace(feature_selection, "^_", "")  # Remove leading _
  
  # For covariates, we need to be more careful since it's optional
  # Remove model, transformation, preprocessing, and feature_selection, then take what's left
  remaining <- workflow_id
  if (!is.na(model)) remaining <- stringr::str_replace(remaining, paste0("^", model, "_?"), "")
  if (!is.na(transformation)) remaining <- stringr::str_replace(remaining, paste0("^", transformation, "_?"), "")
  if (!is.na(preprocessing)) remaining <- stringr::str_replace(remaining, paste0("^", preprocessing, "_?"), "")
  if (!is.na(feature_selection)) remaining <- stringr::str_replace(remaining, paste0("^", feature_selection, "_?"), "")
  
  covariates <- if (nchar(remaining) > 0) remaining else NULL
  
  # Handle defaults for missing components
  # Use proper defaults when components are NA (either R's NA or the string "NA")
  if (is.na(model) || model == "NA") model <- "unknown"
  if (is.na(transformation) || transformation == "NA") transformation <- "NoTrans"
  if (is.na(preprocessing) || preprocessing == "NA") preprocessing <- "raw"
  if (is.na(feature_selection) || feature_selection == "NA") feature_selection <- "None"
  
  list(
    model             = get_readable_model_name(model),
    transformation    = get_readable_transformation_name(transformation),
    preprocessing     = get_readable_preprocessing_name(preprocessing),
    feature_selection = get_readable_feature_selection_name(feature_selection),
    covariates        = if (!is.null(covariates) && covariates != "NoCovs" && covariates != "NA") {
                          # Handle compound covariates with + signs
                          covariate_parts <- unlist(stringr::str_split(covariates, "\\+"))
                          # Filter out "NA" strings from the parts
                          covariate_parts <- covariate_parts[covariate_parts != "NA" & nzchar(covariate_parts)]
                          if (length(covariate_parts) > 0) {
                            readable_parts <- sapply(covariate_parts, get_readable_covariate_name)
                            paste(readable_parts, collapse = " + ")
                          } else {
                            "None"
                          }
                        } else {
                          "None"
                        }
  )
}