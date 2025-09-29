#' Model Specifications and Workflow Management
#'
#' @description
#' Functions for defining model specifications and generating workflow IDs
#' for the horizons modeling pipeline.
#'
#' @keywords internal

## -----------------------------------------------------------------------------
## Model Specifications
## -----------------------------------------------------------------------------

#' Define a Parsnip Model Specification for Soil Spectral Modeling
#'
#' Constructs a tunable `parsnip` model specification for one of several supported
#' regression algorithms commonly used in soil spectroscopy and ensemble modeling.
#' Each specification includes `tune()` placeholders for relevant hyperparameters,
#' and is intended for use in `workflow()` objects or within model grids.
#'
#' @param model_type Character. One of the following canonical model slugs:
#'   \itemize{
#'     \item{\code{"random_forest"}}
#'     \item{\code{"cubist"}}
#'     \item{\code{"xgboost"}}
#'     \item{\code{"lightgbm"}}
#'     \item{\code{"elastic_net"}}
#'     \item{\code{"svm_rbf"}}
#'     \item{\code{"mars"}}
#'     \item{\code{"plsr"}}
#'     \item{\code{"mlp_nn"}}
#'   }
#'
#' @return A `parsnip` model specification object with tunable hyperparameters.
#'
#' @details
#' This function standardizes model setup across workflows. Each model spec is returned
#' with regression mode enabled, an appropriate modeling engine selected, and all key
#' hyperparameters marked as tunable. Designed for compatibility with `tune_grid()`,
#' `workflowsets::workflow_set()`, and `stacks::stacks()`.
#'
#' @seealso
#'   \code{\link[parsnip]{set_engine}},
#'   \code{\link[tune]{tune}},
#'   \code{\link{build_model_grid}}
#'
#' @examples
#' \dontrun{
#' define_model_specifications("random_forest")
#' define_model_specifications("plsr")
#' define_model_specifications("mlp_nn")
#' }
#'
#' @importFrom parsnip rand_forest boost_tree linear_reg svm_rbf mars mlp pls cubist_rules set_engine set_mode
#' @importFrom tune tune
#' @importFrom cli cli_abort
#' @import plsmod
#'
#' @keywords internal
define_model_specifications <- function(model_type) {

  switch(model_type,
    
    "random_forest" = parsnip::rand_forest(
      trees  = 500,                  # Fixed: number of trees
      mtry   = tune::tune(),          # Tunable: number of predictors
      min_n  = tune::tune()           # Tunable: minimum node size
    ) %>%
      parsnip::set_engine("ranger", importance = "impurity") %>%
      parsnip::set_mode("regression"),
    
    "cubist" = parsnip::cubist_rules(
      committees = tune::tune(),       # Tunable: number of committees
      neighbors  = tune::tune(),       # Tunable: number of neighbors
      max_rules  = tune::tune()        # Tunable: maximum rules
    ) %>%
      parsnip::set_engine("Cubist") %>%
      parsnip::set_mode("regression"),
    
    "xgboost" = parsnip::boost_tree(
      trees          = 500,            # Fixed: number of trees
      tree_depth     = tune::tune(),   # Tunable: tree depth
      learn_rate     = tune::tune(),   # Tunable: learning rate
      mtry           = tune::tune(),   # Tunable: number of predictors
      min_n          = tune::tune(),   # Tunable: minimum node size
      loss_reduction = tune::tune(),   # Tunable: loss reduction
      sample_size    = tune::tune()    # Tunable: sample proportion
    ) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("regression"),
    
    "lightgbm" = parsnip::boost_tree(
      trees       = 500,                # Fixed: number of trees
      tree_depth  = tune::tune(),       # Tunable: tree depth
      learn_rate  = tune::tune(),       # Tunable: learning rate
      mtry        = tune::tune(),       # Tunable: number of predictors
      min_n       = tune::tune()        # Tunable: minimum node size
    ) %>%
      parsnip::set_engine("lightgbm") %>%
      parsnip::set_mode("regression"),
    
    "elastic_net" = parsnip::linear_reg(
      penalty = tune::tune(),           # Tunable: regularization strength
      mixture = tune::tune()            # Tunable: elastic net mixing
    ) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("regression"),
    
    "svm_rbf" = parsnip::svm_rbf(
      cost   = tune::tune(),            # Tunable: cost parameter
      rbf_sigma = tune::tune()          # Tunable: RBF kernel parameter
    ) %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("regression"),
    
    "mars" = parsnip::mars(
      prod_degree = tune::tune(),       # Tunable: product degree
      num_terms   = tune::tune()        # Tunable: number of terms
    ) %>%
      parsnip::set_engine("earth") %>%
      parsnip::set_mode("regression"),
    
    "plsr" = pls(
      num_comp  = tune::tune(),         # Tunable: number of components
      predictor_prop = 1                # Use all predictors
    ) %>%
      parsnip::set_engine("mixOmics") %>%
      parsnip::set_mode("regression"),
    
    "mlp_nn" = parsnip::mlp(
      hidden_units = tune::tune(),      # Tunable: hidden layer size
      penalty      = tune::tune(),      # Tunable: weight decay
      epochs       = tune::tune()       # Tunable: training epochs
    ) %>%
      parsnip::set_engine("nnet", MaxNWts = 10000) %>%  # Increased limit for high-dimensional spectral data
      parsnip::set_mode("regression"),
    
    cli::cli_abort("Unknown model type: {model_type}")
  )
}

## -----------------------------------------------------------------------------
## Workflow ID Generation
## -----------------------------------------------------------------------------

#' Generate Clean Workflow ID
#'
#' @description
#' Creates a standardized workflow identifier from model components.
#' Handles special cases like "No Transformation" and "No Selection".
#'
#' @param model Character. Model type (e.g., "random_forest", "cubist")
#' @param transformation Character. Response transformation (e.g., "none", "log", "sqrt")
#' @param preprocessing Character. Spectral preprocessing method
#' @param feature_selection Character. Feature selection method
#' @param covariates Character vector. Covariates to include
#'
#' @return Character. Clean workflow ID like "cubist_NoTrans_SNV_PCA_clay"
#'
#' @examples
#' \dontrun{
#' clean_workflow_id(
#'   model = "cubist",
#'   transformation = "none",
#'   preprocessing = "snv",
#'   feature_selection = "pca",
#'   covariates = c("clay", "sand")
#' )
#' }
#'
#' @export
clean_workflow_id <- function(model,
                             transformation,
                             preprocessing,
                             feature_selection,
                             covariates = NULL) {
  
  ## ---------------------------------------------------------------------------
  ## Input Validation (Closure Safety)
  ## ---------------------------------------------------------------------------
  
  # DEFENSIVE: Check for closure contamination in all inputs
  # This provides additional safety if closures somehow make it past upstream checks
  check_for_closures <- function(value, param_name) {
    if (is.function(value) || "closure" %in% class(value)) {
      cli::cli_abort("▶ clean_workflow_id: Closure detected in parameter '{param_name}'. Cannot generate workflow ID.")
    }
  }
  
  check_for_closures(model, "model")
  check_for_closures(transformation, "transformation")  
  check_for_closures(preprocessing, "preprocessing")
  check_for_closures(feature_selection, "feature_selection")
  if (!is.null(covariates)) check_for_closures(covariates, "covariates")
  
  ## ---------------------------------------------------------------------------
  ## Clean transformation name
  ## ---------------------------------------------------------------------------
  
  trans_clean <- switch(tolower(transformation),
    "none" = "NoTrans",
    "no transformation" = "NoTrans",
    "log" = "Log",
    "log transformation" = "Log",
    "sqrt" = "Sqrt",
    "square root transformation" = "Sqrt",
    transformation  # Default to original if not matched
  )
  
  ## ---------------------------------------------------------------------------
  ## Clean preprocessing name
  ## ---------------------------------------------------------------------------
  
  preproc_clean <- switch(tolower(preprocessing),
    "raw" = "Raw",
    "snv" = "SNV",
    "standard normal variate" = "SNV",
    "sg" = "SG",
    "savitzky-golay smoothing" = "SG",
    "deriv1" = "D1",
    "first derivative" = "D1",
    "deriv2" = "D2",
    "second derivative" = "D2",
    "snv_deriv1" = "SNVD1",
    "snv + first derivative" = "SNVD1",
    "snv_deriv2" = "SNVD2",
    "snv + second derivative" = "SNVD2",
    toupper(preprocessing)  # Default to uppercase original
  )
  
  ## ---------------------------------------------------------------------------
  ## Clean feature selection name
  ## ---------------------------------------------------------------------------
  
  feat_clean <- switch(tolower(feature_selection),
    "none" = "NoFeatSel",
    "no selection" = "NoFeatSel",
    "pca" = "PCA",
    "principal component analysis" = "PCA",
    "boruta" = "Boruta",
    "boruta algorithm" = "Boruta",
    "correlation" = "Corr",
    "correlation filter" = "Corr",
    "remove_correlated" = "Corr",
    "cars" = "CARS",
    "cars selection" = "CARS",
    "vip" = "VIP",
    "variable importance" = "VIP",
    "rfe" = "RFE",
    "recursive feature elimination" = "RFE",
    toupper(feature_selection)  # Default to uppercase original
  )
  
  ## ---------------------------------------------------------------------------
  ## Handle covariates
  ## ---------------------------------------------------------------------------
  
  if (!is.null(covariates) && length(covariates) > 0) {
    # Remove any NA or empty strings
    covariates <- covariates[!is.na(covariates) & nzchar(covariates)]
    
    if (length(covariates) > 0) {
      # Join with plus signs for multiple covariates
      cov_string <- paste(covariates, collapse = "+")
    } else {
      cov_string <- "NoCovs"
    }
  } else {
    cov_string <- "NoCovs"
  }
  
  ## ---------------------------------------------------------------------------
  ## Build workflow ID
  ## ---------------------------------------------------------------------------
  
  # Join all components with underscores
  workflow_id <- paste(
    model,
    trans_clean,
    preproc_clean,
    feat_clean,
    cov_string,
    sep = "_"
  )
  
  # Remove any double underscores that might have been created
  workflow_id <- gsub("__", "_", workflow_id)
  
  # Remove trailing underscore if present
  workflow_id <- gsub("_$", "", workflow_id)
  
  return(workflow_id)
}

## -----------------------------------------------------------------------------
## Back-transformation Utilities
## -----------------------------------------------------------------------------

#' Evaluate Final Models with Back-transformation
#'
#' @description
#' Applies finalized workflows to held-out data and performs back-transformation
#' of predictions based on the transformation type in the workflow ID.
#'
#' @param finalized_wf_sets Tibble with fitted workflows and metadata
#' @param holdout_data Data frame with Response column and predictors
#'
#' @return Tibble with evaluation metrics for each model
#'
#' @keywords internal
evaluate_final_models <- function(finalized_wf_sets, holdout_data) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate Inputs
  ## ---------------------------------------------------------------------------

  if (!"fitted_wf" %in% names(finalized_wf_sets)) {
    cli::cli_abort("Missing {.field fitted_wf} column in finalized_wf_sets.")
  }

  if (!"Response" %in% names(holdout_data)) {
    cli::cli_abort("Holdout data must include a {.field Response} column.")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Set up metric function
  ## ---------------------------------------------------------------------------

  eval_metrics <- yardstick::metric_set(rsq, rmse, rrmse)

  ## ---------------------------------------------------------------------------
  ## Step 3: Inner function: predict + back-transform
  ## ---------------------------------------------------------------------------

  predict_and_evaluate <- function(wf, id) {

    predict(wf, new_data = holdout_data) %>%
      dplyr::rename(estimate = .pred) %>%
      dplyr::mutate(estimate = dplyr::case_when(
        grepl("Log", id)  ~ exp(estimate),
        grepl("Sqrt", id) ~ estimate^2,
        TRUE ~ estimate
      )) %>%
      dplyr::bind_cols(holdout_data %>% dplyr::select(Response)) %>%
      dplyr::rename(truth = Response) %>%
      eval_metrics(truth = truth, estimate = estimate) %>%
      dplyr::mutate(workflow_id = id)

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Apply to all workflows
  ## ---------------------------------------------------------------------------

  purrr::map2_dfr(
    finalized_wf_sets$fitted_wf,
    finalized_wf_sets$workflow_id,
    ~ predict_and_evaluate(.x, .y)
  ) %>%
    tidyr::pivot_wider(
      names_from  = .metric,
      values_from = .estimate
    )

}

# Note: back_transform_predictions is now defined in utils-backtransform.R
# This duplicate definition has been removed to avoid conflicts with the warn parameter