#' Utility Functions for Stacks Ensemble Transformation Handling
#'
#' These functions support handling mixed transformations in stacks ensembles
#' by enabling individual back-transformation of each base model before
#' applying ensemble weights.
#'
#' @name utils-ensemble-stacks
#' @keywords internal
NULL

#' Extract Coefficients from Stacks Model
#'
#' @description
#' Extracts the blending coefficients (weights) from a fitted stacks model.
#' These coefficients are determined by the LASSO meta-learner and indicate
#' how much weight each base model receives in the final ensemble prediction.
#'
#' @param stacks_model A fitted model_stack from `stacks::fit_members()`
#'
#' @return Named numeric vector of coefficients/weights for each ensemble member.
#'   Names correspond to member identifiers, values sum to 1.0.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # After fitting a stacks model
#' coefs <- extract_stacks_coefficients(ensemble_model)
#' # Returns: c(model_1 = 0.4, model_2 = 0.35, model_3 = 0.25)
#' }
extract_stacks_coefficients <- function(stacks_model) {

  ## Use stacks autoplot to extract weights ---------------------------------
  ## This is the recommended way to access coefficients from stacks

  weights_data <- stacks::autoplot(stacks_model, type = "weights")$data

  ## Create named vector ----------------------------------------------------

  coefs <- weights_data$weight
  names(coefs) <- weights_data$member

  return(coefs)
}

#' Extract Member Workflows from Stacks Model
#'
#' @description
#' Extracts the fitted member workflows from a stacks ensemble. These are
#' the individual models that were selected by the LASSO meta-learner and
#' trained on the full training set.
#'
#' @param stacks_model A fitted model_stack from `stacks::fit_members()`
#'
#' @return List of fitted workflows, one for each ensemble member.
#'   Names correspond to member identifiers.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # After fitting a stacks model
#' members <- extract_stacks_members(ensemble_model)
#' # Returns: list(model_1 = <workflow>, model_2 = <workflow>, ...)
#' }
extract_stacks_members <- function(stacks_model) {

  ## Member fits are stored in stacks_model$member_fits --------------------

  member_workflows <- stacks_model$member_fits

  return(member_workflows)
}

#' Predict from Stacks Members with Individual Back-Transformation
#'
#' @description
#' Generates predictions from each stacks ensemble member, applying individual
#' back-transformation based on each model's transformation type. This ensures
#' all predictions are on the original scale before being combined by the
#' meta-learner, correctly handling mixed transformations (e.g., some log,
#' some none).
#'
#' @param member_workflows List of fitted workflows from `extract_stacks_members()`
#' @param transformations Character vector of transformation types for each member.
#'   Must be same length as `member_workflows`. Valid values: "log", "sqrt", "none".
#' @param new_data Data frame containing predictor variables for prediction
#'
#' @return Data frame with one column per member, all predictions on original scale.
#'   Column names match member workflow names.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # With mixed transformations
#' base_preds <- predict_stacks_members_backtransformed(
#'   member_workflows = list(log_model = wf1, none_model = wf2),
#'   transformations = c("log", "none"),
#'   new_data = test_data
#' )
#' # Returns: tibble with columns log_model and none_model (both original scale)
#' }
predict_stacks_members_backtransformed <- function(member_workflows,
                                                    transformations,
                                                    new_data) {

  ## Validate inputs --------------------------------------------------------

  if (length(member_workflows) != length(transformations)) {
    cli::cli_abort(
      c("Length mismatch between member_workflows and transformations",
        "i" = "member_workflows has length {length(member_workflows)}",
        "i" = "transformations has length {length(transformations)}")
    )
  }

  ## Predict from each member with individual back-transformation ----------

  # Get predictions for each member
  member_preds <- purrr::map2(
    member_workflows,
    transformations,
    function(fitted_wf, trans) {
      get_original_scale_predictions(fitted_wf, new_data, trans, warn = FALSE)
    }
  )

  # Combine into data frame with proper names
  member_df <- do.call(cbind, member_preds)
  colnames(member_df) <- names(member_workflows)

  # Convert to tibble
  tibble::as_tibble(member_df)
}

#' Apply Stacks Weights to Base Predictions
#'
#' @description
#' Applies ensemble weights (coefficients) to base model predictions to
#' generate the final ensemble prediction. This performs a weighted sum
#' of the base predictions using the coefficients determined by the
#' LASSO meta-learner.
#'
#' @param base_predictions Data frame of base model predictions. One column
#'   per model, all on the same scale (original scale after back-transformation).
#' @param coefficients Named numeric vector of weights. Names must match
#'   column names in `base_predictions`.
#'
#' @return Numeric vector of ensemble predictions, one per row in `base_predictions`.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' base_preds <- tibble(model_1 = c(5, 10), model_2 = c(6, 11))
#' coefs <- c(model_1 = 0.6, model_2 = 0.4)
#' ensemble_pred <- apply_stacks_weights(base_preds, coefs)
#' # Returns: c(5.4, 10.4)  # 5*0.6 + 6*0.4 = 5.4
#' }
apply_stacks_weights <- function(base_predictions, coefficients) {

  ## Validate inputs --------------------------------------------------------

  pred_names <- names(base_predictions)
  coef_names <- names(coefficients)

  if (!all(coef_names %in% pred_names)) {
    missing_cols <- setdiff(coef_names, pred_names)
    cli::cli_abort(
      c("Coefficient names do not match prediction columns",
        "i" = "Missing columns: {paste(missing_cols, collapse = ', ')}")
    )
  }

  ## Ensure column order matches coefficient order --------------------------

  base_predictions <- base_predictions[, names(coefficients), drop = FALSE]

  ## Convert to matrix ------------------------------------------------------

  pred_matrix <- as.matrix(base_predictions)

  ## Apply weighted sum -----------------------------------------------------

  ensemble_pred <- as.vector(pred_matrix %*% coefficients)

  return(ensemble_pred)
}
