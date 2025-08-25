#' Custom Expectations for horizons Package Testing
#' 
#' Helper functions that provide domain-specific expectations for testing
#' horizons package functionality. These expectations encapsulate common
#' validation patterns used throughout the package.

#' Expect a valid recipe object
#'
#' Checks that an object is a proper recipes::recipe with expected structure
#' and steps appropriate for spectral modeling workflows.
#'
#' @param recipe A recipe object to validate.
#' @param has_spectral_step Logical. Whether to expect spectral transformation step.
#' @param has_covariate_step Logical. Whether to expect covariate addition step.
#' @param has_selection_step Logical. Whether to expect feature selection step.
#'
#' @return Invisibly returns the recipe if valid, otherwise throws expectation error.
expect_valid_recipe <- function(recipe, 
                                has_spectral_step = TRUE,
                                has_covariate_step = FALSE,
                                has_selection_step = FALSE) {
  
  # Check basic structure
  expect_s3_class(recipe, "recipe")
  
  # Check required components
  expect_true("template" %in% names(recipe))
  expect_true("var_info" %in% names(recipe))
  expect_true("steps" %in% names(recipe))
  expect_true("term_info" %in% names(recipe))
  
  # Check for expected steps
  step_classes <- purrr::map_chr(recipe$steps, ~ class(.x)[1])
  
  if (has_spectral_step) {
    expect_true("step_transform_spectra" %in% step_classes,
                info = "Recipe should include spectral transformation step")
  }
  
  if (has_covariate_step) {
    expect_true("step_add_covariates" %in% step_classes,
                info = "Recipe should include covariate addition step")
  }
  
  if (has_selection_step) {
    selection_steps <- c("step_select_boruta", "step_select_correlation", "step_select_shap")
    expect_true(any(selection_steps %in% step_classes),
                info = paste("Recipe should include one of:", paste(selection_steps, collapse = ", ")))
  }
  
  # Check variable roles
  var_info <- recipe$var_info
  expect_true("Sample_ID" %in% var_info$variable || 
              all(var_info$role != "predictor") || 
              nrow(var_info) == 0,
              info = "Recipe should handle Sample_ID appropriately")
  
  invisible(recipe)
}

#' Expect a valid workflow object
#'
#' Checks that an object is a proper workflows::workflow with model and
#' preprocessing components properly configured.
#'
#' @param workflow A workflow object to validate.
#' @param has_recipe Logical. Whether workflow should include a recipe.
#' @param has_model Logical. Whether workflow should include a model specification.
#'
#' @return Invisibly returns the workflow if valid, otherwise throws expectation error.
expect_valid_workflow <- function(workflow, 
                                  has_recipe = TRUE, 
                                  has_model = TRUE) {
  
  # Check basic structure
  expect_s3_class(workflow, "workflow")
  
  # Check components
  if (has_recipe) {
    expect_false(workflows::is_empty_preprocessor(workflow),
                 info = "Workflow should have preprocessing (recipe)")
    
    recipe <- workflows::extract_preprocessor(workflow)
    expect_valid_recipe(recipe)
  }
  
  if (has_model) {
    expect_false(workflows::is_empty_model(workflow),
                 info = "Workflow should have model specification")
    
    model_spec <- workflows::extract_spec_parsnip(workflow)
    expect_s3_class(model_spec, "model_spec")
  }
  
  invisible(workflow)
}

#' Expect valid prediction output
#'
#' Checks that prediction results have the expected structure and
#' reasonable values for spectral modeling context.
#'
#' @param predictions A data frame or vector of predictions.
#' @param truth Optional vector of true values for comparison.
#' @param min_value Numeric. Minimum reasonable prediction value.
#' @param max_value Numeric. Maximum reasonable prediction value.
#'
#' @return Invisibly returns predictions if valid, otherwise throws expectation error.
expect_valid_predictions <- function(predictions, 
                                     truth = NULL,
                                     min_value = 0,
                                     max_value = 100) {
  
  # Check basic structure
  expect_true(is.numeric(predictions) || is.data.frame(predictions),
              info = "Predictions should be numeric vector or data frame")
  
  if (is.data.frame(predictions)) {
    expect_true(".pred" %in% names(predictions) || 
                "predictions" %in% names(predictions) ||
                any(grepl("pred", names(predictions), ignore.case = TRUE)),
                info = "Prediction data frame should contain prediction column")
    
    pred_values <- predictions[[grep("pred", names(predictions), ignore.case = TRUE)[1]]]
  } else {
    pred_values <- predictions
  }
  
  # Check for missing values
  expect_false(any(is.na(pred_values)),
               info = "Predictions should not contain NA values")
  
  # Check reasonable range
  expect_true(all(pred_values >= min_value & pred_values <= max_value),
              info = paste("Predictions should be between", min_value, "and", max_value))
  
  # If truth values provided, check basic sanity
  if (!is.null(truth)) {
    expect_equal(length(pred_values), length(truth),
                 info = "Predictions and truth should have same length")
    
    # Check correlation is reasonable (> 0.1 for very loose validation)
    correlation <- cor(pred_values, truth, use = "complete.obs")
    expect_true(correlation > 0.1 || correlation < -0.1,
                info = paste("Predictions should have some correlation with truth. Got:", 
                           round(correlation, 3)))
  }
  
  invisible(predictions)
}

#' Expect valid spectral data
#'
#' Checks that spectral data has the expected structure with Sample_ID,
#' response variable, and spectral columns.
#'
#' @param data A data frame containing spectral data.
#' @param has_response Logical. Whether data should include Response column.
#' @param min_wavelength Numeric. Minimum expected wavelength.
#' @param max_wavelength Numeric. Maximum expected wavelength.
#'
#' @return Invisibly returns data if valid, otherwise throws expectation error.
expect_valid_spectral_data <- function(data,
                                       has_response = TRUE,
                                       min_wavelength = 400,
                                       max_wavelength = 4000) {
  
  # Check basic structure
  expect_s3_class(data, "data.frame")
  expect_true(nrow(data) > 0, info = "Data should have at least one row")
  
  # Check required columns
  expect_true("Sample_ID" %in% names(data),
              info = "Data should contain Sample_ID column")
  
  if (has_response) {
    expect_true("Response" %in% names(data),
                info = "Data should contain Response column")
  }
  
  # Check for spectral columns (numeric column names)
  spectral_cols <- names(data)[grepl("^[0-9.]+$", names(data))]
  expect_true(length(spectral_cols) > 0,
              info = "Data should contain spectral columns (numeric names)")
  
  # Check wavelength ranges
  wavelengths <- as.numeric(spectral_cols)
  expect_true(min(wavelengths) >= min_wavelength,
              info = paste("Minimum wavelength should be >=", min_wavelength))
  expect_true(max(wavelengths) <= max_wavelength,
              info = paste("Maximum wavelength should be <=", max_wavelength))
  
  # Check spectral values are reasonable
  spectral_data <- data[, spectral_cols]
  expect_true(all(is.numeric(as.matrix(spectral_data))),
              info = "All spectral values should be numeric")
  
  invisible(data)
}

#' Expect valid model configuration
#'
#' Checks that a model configuration has the expected structure for
#' horizons modeling workflows.
#'
#' @param config A data frame or list containing model configuration.
#' @param required_cols Character vector of required column names.
#'
#' @return Invisibly returns config if valid, otherwise throws expectation error.
expect_valid_model_config <- function(config, 
                                      required_cols = c("spectral_transformation", 
                                                       "response_transformation", 
                                                       "feature_selection_method")) {
  
  # Check basic structure
  expect_true(is.data.frame(config) || is.list(config),
              info = "Config should be data frame or list")
  
  if (is.data.frame(config)) {
    config_names <- names(config)
    expect_true(nrow(config) > 0, info = "Config should have at least one row")
  } else {
    config_names <- names(config)
  }
  
  # Check required columns/elements
  missing_cols <- setdiff(required_cols, config_names)
  expect_true(length(missing_cols) == 0,
              info = paste("Config missing required elements:", paste(missing_cols, collapse = ", ")))
  
  # Check transformation values are valid
  if ("spectral_transformation" %in% config_names) {
    valid_spectral <- c("raw", "sg", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2", "msc_deriv1")
    if (is.data.frame(config)) {
      spectral_vals <- unique(config$spectral_transformation)
    } else {
      spectral_vals <- config$spectral_transformation
    }
    expect_true(all(spectral_vals %in% valid_spectral),
                info = paste("Invalid spectral transformation values:", 
                           paste(setdiff(spectral_vals, valid_spectral), collapse = ", ")))
  }
  
  if ("response_transformation" %in% config_names) {
    valid_response <- c("No Transformation", "Log Transformation", "Square Root Transformation")
    if (is.data.frame(config)) {
      response_vals <- unique(config$response_transformation)
    } else {
      response_vals <- config$response_transformation
    }
    expect_true(all(response_vals %in% valid_response),
                info = paste("Invalid response transformation values:",
                           paste(setdiff(response_vals, valid_response), collapse = ", ")))
  }
  
  invisible(config)
}

#' Expect valid feature selection results
#'
#' Checks that feature selection results have appropriate structure
#' and reasonable feature counts.
#'
#' @param selected_features Character vector of selected feature names.
#' @param original_features Character vector of original feature names.
#' @param min_selected Integer. Minimum number of features that should be selected.
#' @param max_selected Integer. Maximum number of features that should be selected.
#'
#' @return Invisibly returns selected_features if valid, otherwise throws expectation error.
expect_valid_feature_selection <- function(selected_features,
                                           original_features,
                                           min_selected = 1,
                                           max_selected = length(original_features)) {
  
  expect_true(is.character(selected_features),
              info = "Selected features should be character vector")
  
  expect_true(length(selected_features) >= min_selected,
              info = paste("Should select at least", min_selected, "features"))
  
  expect_true(length(selected_features) <= max_selected,
              info = paste("Should select at most", max_selected, "features"))
  
  # Check that selected features are subset of original features
  invalid_features <- setdiff(selected_features, original_features)
  expect_true(length(invalid_features) == 0,
              info = paste("Selected features not in original set:", 
                         paste(invalid_features, collapse = ", ")))
  
  invisible(selected_features)
}