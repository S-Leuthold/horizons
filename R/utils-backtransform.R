#' Back-transform Predictions to Original Scale
#'
#' @description
#' Simple utility to back-transform predictions from log or sqrt scale to original scale.
#' Handles NA values gracefully and provides clear warnings for edge cases.
#'
#' @param predictions Numeric vector of predictions to back-transform
#' @param transformation Character string: "none", "log", or "sqrt"
#' @param warn Logical. Should warnings be issued for edge cases?
#'
#' @return Numeric vector of back-transformed predictions
#' @export
#' @examples
#' # Log transformation
#' pred_log <- c(1, 2, 3, NA)
#' back_transform_predictions(pred_log, "log")
#' 
#' # Square root transformation
#' pred_sqrt <- c(1, 4, 9, NA)
#' back_transform_predictions(pred_sqrt, "sqrt")
back_transform_predictions <- function(predictions, transformation, warn = TRUE) {
  
  # Handle NULL or missing inputs
  if (is.null(predictions) || length(predictions) == 0) {
    return(predictions)
  }
  
  # Standardize transformation name to lowercase
  transformation <- tolower(as.character(transformation))
  
  # Apply appropriate back-transformation
  result <- switch(transformation,
    "none" = predictions,
    "log" = {
      if (warn && any(predictions > 50, na.rm = TRUE)) {
        cli::cli_alert_warning("Very large values detected in log-scale predictions (>50). Check for outliers.")
      }
      exp(predictions)
    },
    "sqrt" = {
      if (warn && any(predictions < 0, na.rm = TRUE)) {
        cli::cli_alert_warning("Negative values detected in sqrt-scale predictions. Setting to 0.")
        predictions[predictions < 0 & !is.na(predictions)] <- 0
      }
      predictions^2
    },
    # Default: no transformation (includes any unrecognized transformation)
    {
      if (warn && transformation != "") {
        cli::cli_alert_info("Unknown transformation '{transformation}'. Returning predictions unchanged.")
      }
      predictions
    }
  )
  
  return(result)
}

#' Back-transform Predictions in tune::last_fit Results
#'
#' @description
#' Applies back-transformation to predictions from tune::last_fit() results.
#' This is used in evaluation-core.R after model fitting.
#'
#' @param last_fit_results Results object from tune::last_fit()
#' @param transformation Character string: "none", "log", or "sqrt"
#'
#' @return Modified last_fit results with back-transformed predictions
#' @keywords internal
back_transform_last_fit <- function(last_fit_results, transformation) {
  
  # Extract predictions
  predictions_df <- tune::collect_predictions(last_fit_results)
  
  if (nrow(predictions_df) > 0 && ".pred" %in% names(predictions_df)) {
    # Back-transform the predictions
    predictions_df$.pred <- back_transform_predictions(
      predictions_df$.pred, 
      transformation,
      warn = FALSE  # Suppress warnings in pipeline
    )
    
    # Update the results object
    # Note: This modifies the predictions but metrics need to be recalculated externally
    last_fit_results$.predictions[[1]] <- predictions_df
  }
  
  return(last_fit_results)
}

#' Back-transform Predictions in fit_resamples Results
#'
#' @description
#' Applies back-transformation to CV predictions from tune::fit_resamples().
#' This is critical for ensemble stacking in evaluation-finalize.R.
#'
#' @param cv_results Results object from tune::fit_resamples()
#' @param transformation Character string: "none", "log", or "sqrt"
#'
#' @return Modified cv_results with back-transformed predictions
#' @keywords internal
back_transform_cv_predictions <- function(cv_results, transformation) {
  
  # Check if predictions exist
  if (!".predictions" %in% names(cv_results)) {
    return(cv_results)
  }
  
  # Back-transform predictions in each fold
  for (i in seq_len(nrow(cv_results))) {
    fold_preds <- cv_results$.predictions[[i]]
    
    if (!is.null(fold_preds) && ".pred" %in% names(fold_preds)) {
      fold_preds$.pred <- back_transform_predictions(
        fold_preds$.pred,
        transformation,
        warn = FALSE
      )
      cv_results$.predictions[[i]] <- fold_preds
    }
  }
  
  return(cv_results)
}

#' Compute Metrics on Original Scale
#'
#' @description
#' Helper function to compute metrics after back-transformation.
#' Used when we need to recalculate metrics on the original scale.
#'
#' @param truth Numeric vector of true values (original scale)
#' @param estimate Numeric vector of predicted values (after back-transformation)
#' @param metrics A yardstick metric_set object
#'
#' @return Tibble with computed metrics
#' @keywords internal
compute_original_scale_metrics <- function(truth, estimate, 
                                          metrics = yardstick::metric_set(
                                            rrmse, 
                                            yardstick::rmse, 
                                            yardstick::rsq, 
                                            yardstick::mae, 
                                            rpd, 
                                            ccc
                                          )) {
  
  # Create data frame for metric calculation
  metric_data <- tibble::tibble(
    truth = truth,
    estimate = estimate
  ) %>%
    tidyr::drop_na()  # Remove any NA values
  
  # Check if we have enough data
  if (nrow(metric_data) < 2) {
    cli::cli_alert_warning("Insufficient data for metric calculation after removing NAs")
    return(tibble::tibble())
  }
  
  # Calculate metrics
  metrics(metric_data, truth = truth, estimate = estimate)
}