#' SHAP-Based Feature Selection for Spectral Predictors
#'
#' Applies a SHAP (Shapley Additive Explanations) analysis to identify
#' important spectral bands using an XGBoost model. Retains predictors
#' with mean absolute SHAP values exceeding an adaptive threshold.
#'
#' @param recipe A `recipes::recipe()` object.
#' @param ... Selector functions to choose spectral predictor columns.
#' @param outcome Character. Name of the outcome variable to use for modeling.
#' @param role Character. Role for selected predictors. Default is `"predictor"`.
#' @param trained Logical. Indicates if the step has been prepped. Required by `recipes`.
#' @param skip Logical. Should the step be skipped during `bake()`? Default is `FALSE`.
#' @param id Character. Unique step ID.
#'
#' @return A `step_select_shap` object.
#'
#' @details
#' The step fits a shallow XGBoost model on the input spectral data (after optional
#' preprocessing) and computes SHAP values. Variables with SHAP importance ≥ mean + 1 SD
#' are retained. Requires `xgboost` and `SHAPforxgboost`.
#'
#' @importFrom xgboost xgboost xgb.DMatrix
#' @importFrom SHAPforxgboost shap.prep shap.importance
#' @importFrom recipes step add_step rand_id
#'
#' @export

## -----------------------------------------------------------------------------
## Step 1: User-facing step constructor
## -----------------------------------------------------------------------------

step_select_shap <- function(recipe,
                             ...,
                             outcome,
                             role    = "predictor",
                             trained = FALSE,
                             skip    = FALSE,
                             id      = recipes::rand_id("select_shap")) {

  terms <- rlang::enquos(...)

  recipes::add_step(
    recipe,
    step_select_shap_new(
      columns       = terms,
      outcome       = outcome,
      role          = role,
      trained       = trained,
      selected_vars = NULL,
      skip          = skip,
      id            = id
    )
  )
}

## -----------------------------------------------------------------------------
## Step 2: Constructor
## -----------------------------------------------------------------------------

#' @export
step_select_shap_new <- function(columns,
                                 outcome,
                                 role,
                                 trained,
                                 selected_vars,
                                 skip,
                                 id) {
  out <- list(
    columns        = columns,
    outcome        = outcome,
    role           = role,
    trained        = trained,
    selected_vars  = selected_vars,
    skip           = skip,
    id             = id
  )
  class(out) <- c("step_select_shap", "step")
  return(out)
}

## -----------------------------------------------------------------------------
## Step 3: prep() method
## -----------------------------------------------------------------------------

#' @export
prep.step_select_shap <- function(x, training, info = NULL, ...) {

  ## ---------------------------------------------------------------------------
  ## Stage 1: Evaluate and extract spectral predictors
  ## ---------------------------------------------------------------------------

  col_names <- recipes::recipes_eval_select(x$columns, training, info)

  if (!is.character(x$outcome) || length(x$outcome) != 1) {
    cli::cli_abort("The {.arg outcome} must be a single character string.")
  }

  if (!x$outcome %in% names(training)) {
    cli::cli_abort("Outcome column {.val {x$outcome}} not found in training data.")
  }

  outcome_vec <- training[[x$outcome]]
  spectra_mat <- as.matrix(training[, col_names, drop = FALSE])

  ## ---------------------------------------------------------------------------
  ## Stage 2: Cluster predictors before SHAP
  ## ---------------------------------------------------------------------------

  cluster_result <- cluster_spectral_predictors(
    spectra_mat,
    k      = 300,
    method = "correlation"
  )

  reduced_mat  <- cluster_result$reduced_mat
  cluster_map  <- cluster_result$cluster_map
  cluster_vars <- cluster_result$selected_vars

  cli::cli_alert_info("Training XGBoost model on {length(cluster_vars)} clustered predictors for SHAP scoring...")

  ## ---------------------------------------------------------------------------
  ## Stage 3: Fit a lightweight XGBoost model
  ## ---------------------------------------------------------------------------

  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(reduced_mat), label = outcome_vec)

  xgb_fit <- xgboost::xgboost(
    data      = dtrain,
    nrounds   = 50,
    max_depth = 3,
    objective = "reg:squarederror",
    verbose   = 0
  )

  ## ---------------------------------------------------------------------------
  ## Stage 4 (Revised): Use xgb.importance instead of SHAP
  ## ---------------------------------------------------------------------------

  cli::cli_alert_info("Calculating importance from XGBoost model...")

  # 1. Get feature importance from the model. 'Gain' is a good metric.
  importance_df <- xgboost::xgb.importance(model = xgb_fit)

  # 2. Extract the scores into a named vector
  # Note: The column names are "Feature" and "Gain"
  xgb_scores        <- importance_df$Gain
  names(xgb_scores) <- importance_df$Feature

  # 3. Apply the same thresholding logic to the importance scores
  threshold         <- mean(xgb_scores, na.rm = TRUE) + stats::sd(xgb_scores, na.rm = TRUE)
  kept_cluster_vars <- names(xgb_scores)[xgb_scores >= threshold]
  kept_wavenumbers  <- unique(unlist(cluster_map[kept_cluster_vars]))

  if (length(kept_wavenumbers) == 0) {
    cli::cli_alert_warning("No predictors retained by XGBoost importance. Retaining all.")
    kept_wavenumbers <- col_names
  }


  ## ---------------------------------------------------------------------------
  ## Stage 5: Return trained step object
  ## ---------------------------------------------------------------------------

  step_select_shap_new(
    columns        = col_names,
    outcome        = x$outcome,
    role           = x$role,
    trained        = TRUE,
    selected_vars  = kept_wavenumbers,
    skip           = x$skip,
    id             = x$id
  )
}

## -----------------------------------------------------------------------------
## Step 4: bake()
## -----------------------------------------------------------------------------

#' @export
bake.step_select_shap <- function(object, new_data, ...) {

  if (is.null(object$selected_vars)) {
    cli::cli_abort("This step has not been trained yet. Please call `prep()` first.")
  }

  if (!all(object$selected_vars %in% names(new_data))) {
    cli::cli_abort("Some selected wavenumbers are missing in `new_data`.")
  }

  keep_cols <- c(
    object$selected_vars,
    setdiff(names(new_data), object$columns)
  )

  dplyr::select(new_data, dplyr::all_of(keep_cols))
}


## -----------------------------------------------------------------------------
## Step 5: print()
## -----------------------------------------------------------------------------

#' @export
print.step_select_shap <- function(x,
                                   width = max(20, options()$width - 30),
                                   ...) {

  cat("SHAP-based spectral feature selection step\n")
  cat(glue::glue("• Outcome column: {x$outcome}\n"))

  if (x$trained) {
    cat(glue::glue("• {length(x$selected_vars)} wavenumbers retained after SHAP scoring\n"))
  } else {
    cat("• Step not yet trained\n")
  }

  invisible(x)
}
