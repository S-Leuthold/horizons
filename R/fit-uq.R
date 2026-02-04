#' Uncertainty Quantification via Conformalized Residual Quantiles (CQR)
#'
#' @description
#' Two functions for the UQ component of `fit()`:
#'
#' 1. `compute_c_alpha()` — Conformal finite-sample correction for a given
#'    coverage level. Pure math, no data dependencies.
#' 2. `fit_uq()` — Full UQ training pipeline: trains a quantile ranger on
#'    OOF residuals, then computes conformal nonconformity scores on
#'    calibration data.
#'
#' @keywords internal


## ===========================================================================
## compute_c_alpha() — conformal finite-sample correction
## ===========================================================================

#' Compute Conformal Correction Factor c_alpha
#'
#' Given a vector of nonconformity scores and a desired coverage level,
#' computes the correction factor c_alpha using the standard conformal
#' finite-sample correction.
#'
#' @param scores Numeric vector of nonconformity scores from calibration.
#' @param level Numeric in [0, 1]. Desired coverage level (e.g. 0.90).
#'
#' @return Single numeric value: the c_alpha correction factor.
#'
#' @details
#' Formula: `q_prob = min(1, ceiling((1 - alpha) * (n + 1)) / n)` where
#' `alpha = 1 - level`. Then `c_alpha = quantile(scores, probs = q_prob,
#' type = 7)`. The ceiling ensures conservative coverage for finite samples.
#'
#' @keywords internal
#' @export
compute_c_alpha <- function(scores, level) {

  alpha  <- 1 - level
  n      <- length(scores)

  ## Finite-sample correction: ceiling to be conservative
  q_prob <- min(1, ceiling((1 - alpha) * (n + 1)) / n)

  stats::quantile(scores, probs = q_prob, type = 7, names = FALSE)

}


## ===========================================================================
## fit_uq() — CQR-style uncertainty quantification
## ===========================================================================

#' Fit Uncertainty Quantification Components
#'
#' Trains a quantile random forest on OOF residuals from the point model,
#' then computes conformal nonconformity scores on the held-out calibration
#' data. Returns everything needed for `predict()` to produce prediction
#' intervals at any coverage level.
#'
#' This function is the **capture layer** — it runs silently and returns a
#' structured list. Messaging is the caller's responsibility.
#'
#' @param fitted_workflow A fitted tidymodels workflow (final model on
#'   train_Fit).
#' @param oof_predictions Tibble with columns `.pred`, `.pred_trans`, `truth`,
#'   `.row`, `.fold`, `config_id`. OOF predictions from `fit_resamples()`.
#' @param calib_data Data frame for conformal calibration (calib_Fit split).
#' @param role_map Tibble with `variable` and `role` columns.
#' @param transformation Character. Transformation applied to the response
#'   (e.g. "none", "log", "log10", "sqrt").
#' @param level_default Numeric. Default coverage level. Default 0.90.
#'
#' @return Named list with fields: `quantile_model`, `scores`, `n_calib`,
#'   `level_default`, `oof_coverage`, `mean_width`, `prepped_recipe`.
#'   Returns NULL if calibration set is too small (`n < N_CALIB_MIN`).
#'
#' @keywords internal
#' @export
fit_uq <- function(fitted_workflow,
                   oof_predictions,
                   calib_data,
                   role_map,
                   transformation  = "none",
                   level_default   = DEFAULT_UQ_LEVEL) {

  outcome_col <- role_map$variable[role_map$role == "outcome"]

  ## --- Guard: minimum calibration size -------------------------------------

  if (nrow(calib_data) < N_CALIB_MIN) {

    return(NULL)

  }

  ## -----------------------------------------------------------------------
  ## Phase 1: Train quantile model on OOF residuals
  ## -----------------------------------------------------------------------

  ## Extract prepped recipe from the fitted workflow
  prepped_recipe <- workflows::extract_recipe(fitted_workflow, estimated = TRUE)

  ## Bake OOF features (predictors only — same feature space the model sees)
  ## We use the original training data keyed by .row to get the feature matrix.
  ## KNOWN LEAKAGE CHOICE: The recipe was prepped on full train_Fit, not on

  ## fold-specific analysis sets. Coverage is enforced by conformal calibration
  ## on calib_data, which uses the same full-train-prepped recipe pipeline.
  train_data_for_bake <- workflows::extract_mold(fitted_workflow)$predictors

  ## OOF residuals: truth - .pred (original scale, standard sign convention)
  oof_residuals <- oof_predictions$truth - oof_predictions$.pred

  ## Match OOF features to OOF rows
  oof_features <- train_data_for_bake[oof_predictions$.row, , drop = FALSE]

  ## Train quantile forest
  qrf_result <- safely_execute(
    ranger::ranger(
      x         = as.data.frame(oof_features),
      y         = oof_residuals,
      quantreg  = TRUE,
      num.trees = UQ_QUANTILE_TREES
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(qrf_result$error)) {

    return(NULL)

  }

  quantile_model <- qrf_result$result

  ## -----------------------------------------------------------------------
  ## Phase 2: Conformal calibration on calib_data
  ## -----------------------------------------------------------------------

  ## Point predictions on calibration set
  calib_point_result <- safely_execute(
    stats::predict(fitted_workflow, new_data = calib_data),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(calib_point_result$error)) {

    return(NULL)

  }

  calib_point_preds <- calib_point_result$result$.pred

  ## Back-transform if response was transformed
  if (needs_back_transformation(transformation)) {

    bt_result <- safely_execute(
      back_transform_predictions(calib_point_preds, transformation, warn = FALSE),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    if (!is.null(bt_result$error)) {

      return(NULL)

    }

    calib_point_preds <- bt_result$result

  }

  ## Bake calibration features through the same prepped recipe
  calib_features <- recipes::bake(
    prepped_recipe,
    new_data = calib_data,
    recipes::all_predictors()
  )

  ## Quantile predictions on calibration set
  alpha <- 1 - level_default
  tau   <- c(alpha / 2, 1 - alpha / 2)

  calib_q_result <- safely_execute(
    stats::predict(
      quantile_model,
      data      = as.data.frame(calib_features),
      type      = "quantiles",
      quantiles = tau
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(calib_q_result$error)) {

    return(NULL)

  }

  q_low  <- calib_q_result$result$predictions[, 1]
  q_high <- calib_q_result$result$predictions[, 2]

  ## Calibration residuals (original scale)
  calib_truth     <- calib_data[[outcome_col]]
  calib_residuals <- calib_truth - calib_point_preds

  ## Nonconformity scores: how far does the actual residual fall outside
  ## the predicted residual interval? Zero means the interval covers it.
  scores <- pmax(0, pmax(q_low - calib_residuals,
                          calib_residuals - q_high))

  ## Remove NA scores (from NA truth values in calib_data)
  scores  <- scores[!is.na(scores)]
  n_calib <- length(scores)

  ## Re-check minimum after NA removal
  if (n_calib < N_CALIB_MIN) {

    return(NULL)

  }

  ## -----------------------------------------------------------------------
  ## Diagnostics: OOF coverage and mean interval width at default level
  ## -----------------------------------------------------------------------

  c_alpha <- compute_c_alpha(scores, level_default)

  ## OOF coverage: predict quantiles on OOF features, check coverage
  oof_q_result <- safely_execute(
    stats::predict(
      quantile_model,
      data      = as.data.frame(oof_features),
      type      = "quantiles",
      quantiles = tau
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(oof_q_result$error)) {

    ## Fallback: use NA for diagnostics, still return UQ object
    oof_coverage <- NA_real_
    mean_width   <- NA_real_

  } else {

    oof_q_low  <- oof_q_result$result$predictions[, 1]
    oof_q_high <- oof_q_result$result$predictions[, 2]

    ## Interval bounds (original scale)
    oof_lower <- oof_predictions$.pred + oof_q_low  - c_alpha
    oof_upper <- oof_predictions$.pred + oof_q_high + c_alpha

    ## Empirical coverage
    covered      <- (oof_predictions$truth >= oof_lower) &
                    (oof_predictions$truth <= oof_upper)
    oof_coverage <- mean(covered, na.rm = TRUE)

    ## Mean interval width
    mean_width <- mean(oof_upper - oof_lower, na.rm = TRUE)

  }

  ## -----------------------------------------------------------------------
  ## Return structured result
  ## -----------------------------------------------------------------------

  list(
    quantile_model = quantile_model,
    scores         = scores,
    n_calib        = n_calib,
    level_default  = level_default,
    oof_coverage   = oof_coverage,
    mean_width     = mean_width,
    prepped_recipe = prepped_recipe
  )

}
