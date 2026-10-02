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
#' @param level Numeric in (0, 1). Desired coverage level (e.g. 0.90).
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

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {

    rlang::abort("level must be a single numeric value in (0, 1).")

  }

  scores <- scores[is.finite(scores)]
  n      <- length(scores)

  if (n == 0) {

    rlang::abort("No finite scores available for conformal calibration.")

  }

  alpha  <- 1 - level

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
#' @param seed Integer or NULL. Seed for the quantile forest. `fit()` passes
#'   its own seed so the forest is reproducible regardless of how the
#'   preceding stages consumed the parent RNG stream; NULL keeps ranger's
#'   default of drawing a seed from that stream.
#' @param outcome_range Numeric length-2 vector. The outcome's physical range,
#'   which the back-transformed calibration predictions are clamped to, as
#'   `predict()` clamps the point predictions the intervals are served
#'   around. Default `c(0, Inf)`.
#'
#' @return Named list with fields: `quantile_model`, `scores`, `n_calib`,
#'   `level_default`, `oof_coverage`, `mean_width`, `prepped_recipe`.
#'   Returns NULL if calibration set is too small (`n < N_CALIB_MIN`).
#'   Aborts with class `horizons_internal_error` when an out-of-fold `.row`
#'   falls outside the rows the model was fit on, or when the calibration
#'   predictions or features do not have one row per row of `calib_data`.
#'
#' @keywords internal
#' @export
fit_uq <- function(fitted_workflow,
                   oof_predictions,
                   calib_data,
                   role_map,
                   transformation  = "none",
                   level_default   = DEFAULT_UQ_LEVEL,
                   seed            = NULL,
                   outcome_range   = DEFAULT_OUTCOME_RANGE) {

  outcome_col <- role_map$variable[role_map$role == "outcome"]

  ## --- Guard: NULL or insufficient calibration data -------------------------

  if (is.null(calib_data) || nrow(calib_data) < N_CALIB_MIN) {

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

  ## .row indexes the rows the resamples were cut from, which are the rows
  ## the final model was fit on, so it only lands on the right sample while
  ## the mold keeps one row per row of them. A recipe step that dropped rows
  ## at fit time shifts every later row; the folds between them cover every
  ## training row, so a .row past the end of the mold is the sign of it.
  oof_rows <- oof_predictions$.row
  n_mold   <- nrow(train_data_for_bake)
  bad_rows <- is.na(oof_rows) | oof_rows < 1L | oof_rows > n_mold

  if (any(bad_rows)) {

    abort_misaligned(
      what   = "Out-of-fold rows",
      to     = "the rows the model was fit on",
      detail = cli::format_inline(
        "{.field .row} falls outside the {n_mold} row{?s} the model was fit on for {sum(bad_rows)} of {length(oof_rows)} out-of-fold prediction{?s}; the first is {.val {oof_rows[bad_rows][1]}}."
      )
    )

  }

  ## Match OOF features to OOF rows
  oof_features <- train_data_for_bake[oof_rows, , drop = FALSE]

  ## Train quantile forest
  qrf_result <- safely_execute(
    ranger::ranger(
      x           = as.data.frame(oof_features),
      y           = oof_residuals,
      quantreg    = TRUE,
      num.trees   = UQ_QUANTILE_TREES,
      ## Pinned at the call site. Unset, ranger falls back to
      ## getOption("ranger.num.threads", detectCores()), i.e. every core,
      ## which oversubscribes under any parallel dispatch (M6, 2026-09-15).
      num.threads = 1L,
      ## Explicit seed: ranger's default draws from the parent R stream, whose
      ## position depends on how the preceding stages ran, so the quantile
      ## forest was not reproducible in its own right. NULL keeps ranger's
      ## default for callers that manage the stream themselves.
      seed        = seed
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

  ## The predictions, the baked features below and the truth are paired by
  ## position to form the scores. A recipe step that dropped rows at bake
  ## would leave the vectors short, and the arithmetic recycles them without
  ## an error when one length divides the other.
  check_rows_aligned(
    what       = "Calibration predictions",
    to         = "the rows of the calibration data",
    n          = length(calib_point_preds),
    n_expected = nrow(calib_data)
  )

  ## Back-transform, unconditionally: the clamp to the outcome's range inside
  ## back_transform_predictions() must reach the calibration residuals too,
  ## since the intervals are served around clamped point predictions (#53,
  ## #76).
  bt_result <- safely_execute(
    back_transform_predictions(calib_point_preds, transformation, warn = FALSE,
                               outcome_range = outcome_range),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(bt_result$error)) {

    return(NULL)

  }

  calib_point_preds <- bt_result$result

  ## Bake calibration features through the same prepped recipe
  calib_features <- recipes::bake(
    prepped_recipe,
    new_data = calib_data,
    recipes::all_predictors()
  )

  check_rows_aligned(
    what       = "Calibration features baked through the recipe",
    to         = "the rows of the calibration data",
    n          = nrow(calib_features),
    n_expected = nrow(calib_data)
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

  ## Nonconformity scores (signed, true CQR per Romano et al. 2019): the
  ## signed distance of the actual residual from the predicted residual
  ## interval. Positive means the residual falls outside the band; negative
  ## means the band already brackets it with room to spare. Keeping the sign
  ## (rather than flooring at 0) lets c_alpha be negative and tighten the
  ## interval where the quantile model is well-calibrated, instead of only
  ## ever widening it.
  scores <- pmax(q_low - calib_residuals,
                 calib_residuals - q_high)

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
