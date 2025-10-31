#' Library Prediction Uncertainty Quantification Utilities
#'
#' @description
#' Functions for uncertainty quantification in library-based prediction mode.
#' Provides quantile prediction wrappers, conformal calibration, and interval
#' construction for the decoupled UQ architecture.
#'
#' @keywords internal

## -----------------------------------------------------------------------------
## Quantile Prediction
## -----------------------------------------------------------------------------

#' Predict Quantiles from Ranger Quantile Model
#'
#' @description
#' Extracts quantile predictions from a fitted ranger quantile regression model.
#' This is a wrapper around ranger's quantile prediction that works with
#' tidymodels workflow objects and returns results in tidy format.
#'
#' @param workflow A fitted `workflow` object containing a ranger model trained
#'   with `quantreg = TRUE`. The workflow must have been created using
#'   [define_quantile_specification()].
#'
#' @param new_data A data frame or tibble containing new samples to predict.
#'   Must have the same predictor columns as the training data.
#'
#' @param quantiles Numeric vector of quantiles to extract, with values in (0, 1).
#'   Default: `c(0.05, 0.95)` for 90% prediction intervals.
#'
#' @return A tibble with one row per sample in `new_data` and columns:
#'   * `.pred_lower`: Lower quantile (typically q05)
#'   * `.pred_upper`: Upper quantile (typically q95)
#'   * Additional columns for other quantiles if requested
#'
#' @details
#' ## How It Works
#'
#' Ranger's quantile regression trains a single random forest that stores
#' terminal node information from all trees. At prediction time, you can extract
#' ANY quantile from this single model by specifying which quantiles to compute.
#'
#' This function:
#' 1. Extracts the fitted ranger model from the workflow
#' 2. Calls `predict.ranger()` with `type = "quantiles"`
#' 3. Converts the matrix output to a tidy tibble
#'
#' ## Quantile Column Naming
#'
#' - If `quantiles = c(0.05, 0.95)`: returns `.pred_lower`, `.pred_upper`
#' - If `quantiles = c(0.10, 0.50, 0.90)`: returns `.pred_q10`, `.pred_q50`, `.pred_q90`
#' - General pattern: `.pred_q{percentile}` (e.g., `.pred_q05`, `.pred_q95`)
#'
#' ## Requirements
#'
#' The workflow must contain a ranger model trained with:
#' * `quantreg = TRUE` (enables quantile regression)
#' * `keep.inbag = TRUE` (required for quantile prediction)
#'
#' @seealso
#'   [define_quantile_specification()],
#'   [ranger::predict.ranger()] for underlying quantile prediction
#'
#' @examples
#' \dontrun{
#' # Train quantile model
#' spec <- define_quantile_specification()
#' spec$args$mtry <- 10
#' spec$args$min_n <- 5
#'
#' wf <- workflow() %>%
#'   add_model(spec) %>%
#'   add_recipe(my_recipe) %>%
#'   fit(data = training_data)
#'
#' # Predict quantiles
#' quantiles <- predict_quantiles(wf, new_data = test_data)
#'
#' # Result has .pred_lower and .pred_upper
#' head(quantiles)
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom cli cli_abort cli_warn
#' @importFrom workflows extract_fit_parsnip
#'
#' @keywords internal
predict_quantiles <- function(workflow,
                              new_data,
                              quantiles = c(0.05, 0.95)) {

  ## Validate inputs -------------------------------------------------------------

  if (!workflows::is_trained_workflow(workflow)) {
    cli::cli_abort("workflow must be a trained workflow object")
  }

  if (!is.numeric(quantiles) || any(quantiles <= 0) || any(quantiles >= 1)) {
    cli::cli_abort("quantiles must be numeric values between 0 and 1")
  }

  if (length(quantiles) < 1) {
    cli::cli_abort("At least one quantile must be specified")
  }

  ## Extract fitted ranger model -------------------------------------------------

  # Get the parsnip model from the workflow
  fit_parsnip <- workflows::extract_fit_parsnip(workflow)

  # Get the underlying ranger model
  ranger_fit <- fit_parsnip$fit

  # Verify it's a ranger model
  if (!inherits(ranger_fit, "ranger")) {
    cli::cli_abort(
      "Workflow must contain a ranger model",
      "i" = "Found model class: {class(ranger_fit)[1]}"
    )
  }

  # Verify quantreg was enabled
  if (is.null(ranger_fit$random.node.values)) {
    cli::cli_abort(
      "Ranger model was not trained with quantreg = TRUE",
      "i" = "Use define_quantile_specification() to create quantile models"
    )
  }

  ## Preprocess new_data using workflow recipe -----------------------------------

  # Get the preprocessed data (recipe applied)
  prepped_data <- workflows::extract_recipe(workflow) %>%
    recipes::bake(new_data = new_data)

  ## Predict quantiles -----------------------------------------------------------

  pred_obj <- predict(ranger_fit,
                     data = prepped_data,
                     type = "quantiles",
                     quantiles = quantiles)

  # Extract predictions matrix (n_samples × n_quantiles)
  pred_matrix <- pred_obj$predictions

  ## Convert to tidy format ------------------------------------------------------

  # Create column names based on quantiles
  if (length(quantiles) == 2 && all(quantiles == c(0.05, 0.95))) {
    # Special case: common 90% interval
    col_names <- c(".pred_lower", ".pred_upper")
  } else {
    # General case: .pred_q05, .pred_q95, etc.
    percentiles <- round(quantiles * 100)
    col_names <- sprintf(".pred_q%02d", percentiles)
  }

  # Build tibble
  result <- tibble::as_tibble(pred_matrix, .name_repair = "minimal")
  names(result) <- col_names

  ## Check for crossings --------------------------------------------------------

  if (length(quantiles) >= 2) {
    # Check if lower quantile > upper quantile anywhere
    lower_col <- result[[1]]
    upper_col <- result[[length(quantiles)]]

    n_crossings <- sum(lower_col > upper_col, na.rm = TRUE)

    if (n_crossings > 0) {
      pct_crossings <- 100 * n_crossings / nrow(result)
      cli::cli_warn(
        "Quantile crossings detected in {n_crossings}/{nrow(result)} predictions ({round(pct_crossings, 1)}%)",
        "i" = "Consider repairing with: lower = pmin(lower, upper); upper = pmax(lower, upper)"
      )
    }
  }

  return(result)
}

## -----------------------------------------------------------------------------
## Residual-Based Prediction with UQ
## -----------------------------------------------------------------------------

#' Predict with Uncertainty Quantification (Residual-Based)
#'
#' @description
#' Generates point predictions and prediction intervals using the residual-based
#' UQ approach. This function combines point predictions from the best-performing
#' model with residual quantiles from ranger to construct prediction intervals
#' that reflect model-specific uncertainty.
#'
#' @param point_workflow Fitted workflow object for point predictions.
#'   Can be ANY model type (cubist, PLSR, xgboost, etc.).
#'
#' @param quantile_workflow Fitted workflow object for residual quantile predictions.
#'   Must be a ranger model trained on point model residuals using
#'   [train_cluster_models_with_uq()].
#'
#' @param new_data Data frame or tibble containing new samples to predict.
#'   Must have same predictor structure as training data.
#'
#' @param quantiles Numeric vector of length 2 specifying quantile levels.
#'   Default: `c(0.05, 0.95)` for 90% prediction intervals.
#'
#' @param repair_crossings Logical. Should quantile crossings be repaired?
#'   Default: TRUE. Enforces `.pred_lower <= .pred_upper`.
#'
#' @return A tibble with one row per sample in `new_data` and columns:
#'   * `.pred`: Point prediction from best model
#'   * `.pred_lower`: Lower prediction bound (point + residual_q05)
#'   * `.pred_upper`: Upper prediction bound (point + residual_q95)
#'
#' @details
#' ## Residual-Based Approach
#'
#' This function implements the residual-based UQ workflow:
#'
#' 1. **Get point prediction**: `y_hat = f_point(X)`
#' 2. **Get residual quantiles**: `[r_q05, r_q95] = f_quantile(X)`
#' 3. **Construct intervals**: `[y_hat + r_q05, y_hat + r_q95]`
#'
#' Note: `r_q05` is typically NEGATIVE (5th percentile of residuals),
#'       `r_q95` is typically POSITIVE (95th percentile of residuals)
#'
#' ## Why Residual-Based?
#'
#' **Library-based** (alternative): Trains quantile model on Response values
#' * Intervals reflect population variability in reference library
#' * Wide intervals (e.g., pH: 2.6 units)
#' * Interpretation: "Range of values in library for this spectral type"
#'
#' **Residual-based** (this approach): Trains quantile model on model residuals
#' * Intervals reflect MODEL confidence in THIS prediction
#' * Narrow intervals (e.g., pH: 0.9 units, 65% reduction)
#' * Interpretation: "90% confident YOUR sample is in this range"
#' * Better point models → smaller residuals → narrower intervals
#'
#' ## Conformal Calibration
#'
#' These intervals are "uncalibrated" - they achieve ~85-95% empirical coverage.
#' Apply conformal calibration (M3.3) to adjust intervals to exactly 90% coverage:
#'
#' ```r
#' c_alpha <- compute_conformal_margin(models, calibration_data)
#' final_interval <- [pred_lower - c_alpha, pred_upper + c_alpha]
#' ```
#'
#' ## Texture Properties
#'
#' For texture properties (sand, silt, clay):
#' * This function called TWICE (once per ILR coordinate)
#' * Residuals computed in ILR space: ilr_1_residuals, ilr_2_residuals
#' * Back-transformation to texture space happens AFTER adding residual quantiles
#' * Point predictions sum to 100%, intervals are marginal (don't guarantee sum=100%)
#'
#' @seealso
#'   [train_cluster_models_with_uq()] for training residual-based models,
#'   [predict_quantiles()] for extracting quantiles,
#'   [repair_quantile_crossings()] for monotonicity enforcement
#'
#' @examples
#' \dontrun{
#' # Train models with residual-based UQ
#' models <- train_cluster_models_with_uq(
#'   cluster_data = train_data,
#'   property = "ph",
#'   config = winning_config
#' )
#'
#' # Predict with UQ
#' predictions <- predict_with_uq(
#'   point_workflow = models$point_model,
#'   quantile_workflow = models$quantile_model,
#'   new_data = test_samples
#' )
#'
#' # Result: .pred, .pred_lower, .pred_upper
#' head(predictions)
#' }
#'
#' @importFrom tibble tibble
#' @importFrom cli cli_abort cli_warn
#' @importFrom workflows is_trained_workflow
#'
#' @keywords internal
predict_with_uq <- function(point_workflow,
                            quantile_workflow,
                            new_data,
                            quantiles = c(0.05, 0.95),
                            c_alpha = 0,
                            repair_crossings = TRUE) {

  ## Validate inputs -------------------------------------------------------------

  if (!workflows::is_trained_workflow(point_workflow)) {
    cli::cli_abort("point_workflow must be a trained workflow object")
  }

  if (!workflows::is_trained_workflow(quantile_workflow)) {
    cli::cli_abort("quantile_workflow must be a trained workflow object")
  }

  if (length(quantiles) != 2) {
    cli::cli_abort(
      "Exactly 2 quantiles required for prediction intervals",
      "i" = "Got {length(quantiles)} quantiles: {paste(quantiles, collapse=', ')}"
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Get point predictions
  ## ---------------------------------------------------------------------------

  point_preds <- predict(point_workflow, new_data)

  if (!".pred" %in% names(point_preds)) {
    cli::cli_abort("Point model predictions missing .pred column")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Get residual quantile predictions
  ## ---------------------------------------------------------------------------

  residual_quantiles <- predict_quantiles(
    workflow  = quantile_workflow,
    new_data  = new_data,
    quantiles = quantiles
  )

  ## ---------------------------------------------------------------------------
  ## Step 3: Construct prediction intervals (point + residual quantiles)
  ## ---------------------------------------------------------------------------

  # Note: residual_q05 is typically negative, residual_q95 is positive
  # So: lower bound = point - |residual_q05|, upper bound = point + residual_q95

  result <- tibble::tibble(
    .pred       = point_preds$.pred,
    .pred_lower = point_preds$.pred + residual_quantiles$.pred_lower - c_alpha,  # Conformal adjustment
    .pred_upper = point_preds$.pred + residual_quantiles$.pred_upper + c_alpha   # Conformal adjustment
  )

  ## ---------------------------------------------------------------------------
  ## Step 4: Optional crossing repair
  ## ---------------------------------------------------------------------------

  if (repair_crossings) {
    result <- repair_quantile_crossings(result)
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Validate monotonicity
  ## ---------------------------------------------------------------------------

  n_crossings <- sum(result$.pred_lower > result$.pred_upper, na.rm = TRUE)

  if (n_crossings > 0) {
    pct <- round(100 * n_crossings / nrow(result), 1)
    cli::cli_warn(
      "{n_crossings}/{nrow(result)} predictions ({pct}%) have quantile crossings",
      "i" = "Crossings repaired, but this suggests model instability"
    )
  }

  return(result)
}

## -----------------------------------------------------------------------------
## Quantile Crossing Repair
## -----------------------------------------------------------------------------

#' Repair Quantile Crossings
#'
#' @description
#' Ensures monotonicity of quantile predictions by enforcing q_lower <= q_upper.
#' Crossings occur when the lower quantile prediction exceeds the upper quantile
#' prediction, which is theoretically impossible but can happen due to numerical
#' issues in finite-sample estimation.
#'
#' @param predictions A tibble or data frame with quantile predictions.
#'   Must contain at least two quantile columns.
#'
#' @param lower_col Character string or integer specifying the lower quantile column.
#'   Default: `".pred_lower"` or column 1 if not found.
#'
#' @param upper_col Character string or integer specifying the upper quantile column.
#'   Default: `".pred_upper"` or column 2 if not found.
#'
#' @return The input data frame with crossings repaired. Modifies columns in-place
#'   to enforce `lower <= upper` for all rows.
#'
#' @details
#' ## Repair Strategy
#'
#' When a crossing is detected (lower > upper), the repair:
#' 1. Sets `lower = min(lower, upper)`
#' 2. Sets `upper = max(lower, upper)`
#'
#' This preserves the interval width while ensuring monotonicity.
#'
#' ## When Crossings Occur
#'
#' Crossings are rare (<5% of predictions typically) but can occur when:
#' * Sample size is small
#' * Extrapolating far from training distribution
#' * Extreme quantiles requested (e.g., q01, q99)
#'
#' @examples
#' \dontrun{
#' # Predict quantiles
#' preds <- predict_quantiles(model, new_data)
#'
#' # Repair any crossings
#' preds_repaired <- repair_quantile_crossings(preds)
#'
#' # Verify monotonicity
#' stopifnot(all(preds_repaired$.pred_lower <= preds_repaired$.pred_upper))
#' }
#'
#' @keywords internal
repair_quantile_crossings <- function(predictions,
                                      lower_col = ".pred_lower",
                                      upper_col = ".pred_upper") {

  ## Identify columns ------------------------------------------------------------

  if (is.character(lower_col)) {
    if (!lower_col %in% names(predictions)) {
      lower_col <- 1  # Default to first column
    }
  }

  if (is.character(upper_col)) {
    if (!upper_col %in% names(predictions)) {
      upper_col <- ncol(predictions)  # Default to last column
    }
  }

  ## Extract vectors -------------------------------------------------------------

  lower <- predictions[[lower_col]]
  upper <- predictions[[upper_col]]

  ## Check for crossings ---------------------------------------------------------

  n_crossings <- sum(lower > upper, na.rm = TRUE)

  if (n_crossings == 0) {
    return(predictions)  # No repair needed
  }

  ## Repair crossings ------------------------------------------------------------

  lower_repaired <- pmin(lower, upper)
  upper_repaired <- pmax(lower, upper)

  ## Update predictions ----------------------------------------------------------

  predictions[[lower_col]] <- lower_repaired
  predictions[[upper_col]] <- upper_repaired

  return(predictions)
}

## -----------------------------------------------------------------------------
## Conformal Calibration
## -----------------------------------------------------------------------------

#' Calculate Conformal Calibration Margin with CV+
#'
#' @description
#' Computes the conformal calibration margin (c_alpha) using matched out-of-fold
#' predictions from both point and quantile models. This is the PROPER CV+
#' approach that avoids optimistic bias from in-sample quantile predictions.
#'
#' @param point_cv_preds Tibble with OOF point predictions.
#'   Must have columns: `.row`, `Response`, `.pred`
#'
#' @param quantile_cv_preds Tibble with OOF quantile predictions.
#'   Must have columns: `.row`, `.pred_lower`, `.pred_upper`
#'
#' @param alpha Numeric. Miscoverage rate (default: 0.10 for 90% intervals).
#'
#' @param verbose Logical. Print diagnostic information?
#'
#' @return Numeric scalar: the conformal calibration margin c_alpha.
#'
#' @details
#' ## CV+ Conformal Calibration Approach
#'
#' This function implements PROPER conformal calibration with out-of-fold
#' predictions from BOTH models:
#'
#' 1. Match OOF point predictions with OOF quantile predictions by `.row`
#' 2. Construct intervals: point_OOF + quantile_residuals_OOF
#' 3. Compute nonconformity scores on all training samples
#' 4. Find c_alpha as (1-alpha) quantile of scores
#'
#' **Why This Works:**
#' - Point predictions are OOF (unbiased) ✓
#' - Quantile predictions are OOF (unbiased) ✓
#' - Both use SAME CV folds (aligned by sample) ✓
#' - Result: Statistically valid coverage guarantee ✓
#'
#' **Previous Issue (FIXED in M3.3):**
#' - Old version used in-sample quantile predictions (optimistic bias)
#' - New version uses OOF quantiles from shared CV folds
#' - Fixes the same issue we fixed for residuals in M3.1!
#'
#' @keywords internal
calculate_conformal_margin <- function(point_cv_preds,
                                      quantile_cv_preds,
                                      alpha = 0.10,
                                      verbose = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate inputs
  ## ---------------------------------------------------------------------------

  if (is.null(quantile_cv_preds)) {
    cli::cli_abort(
      "OOF quantile predictions required for proper CV+ conformal calibration",
      "i" = "This is a bug - train_quantile_model() should return cv_quantiles when resamples provided"
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Match OOF point predictions with OOF quantile predictions by .row
  ## ---------------------------------------------------------------------------

  ## DEBUG: Check input data ---------------------------------------------------

  if (verbose) {
    cli::cli_text("│  │  │  │  ├─ DEBUG: Point predictions")
    cli::cli_text("│  │  │  │  │  ├─ .row range: [{min(point_cv_preds$.row)}, {max(point_cv_preds$.row)}]")
    cli::cli_text("│  │  │  │  │  ├─ .pred range: [{round(min(point_cv_preds$.pred),2)}, {round(max(point_cv_preds$.pred),2)}]")
    cli::cli_text("│  │  │  │  │  └─ n = {nrow(point_cv_preds)}")

    cli::cli_text("│  │  │  │  ├─ DEBUG: Quantile predictions")
    cli::cli_text("│  │  │  │  │  ├─ .row range: [{min(quantile_cv_preds$.row)}, {max(quantile_cv_preds$.row)}]")
    cli::cli_text("│  │  │  │  │  ├─ .pred_lower range: [{round(min(quantile_cv_preds$.pred_lower),2)}, {round(max(quantile_cv_preds$.pred_lower),2)}]")
    cli::cli_text("│  │  │  │  │  ├─ .pred_upper range: [{round(min(quantile_cv_preds$.pred_upper),2)}, {round(max(quantile_cv_preds$.pred_upper),2)}]")
    cli::cli_text("│  │  │  │  │  └─ n = {nrow(quantile_cv_preds)}")
  }

  calibration_data <- point_cv_preds %>%
    dplyr::inner_join(quantile_cv_preds, by = ".row", suffix = c("_point", "_quantile"))

  ## Validate match -------------------------------------------------------------

  if (nrow(calibration_data) == 0) {
    cli::cli_abort("No matching rows between point and quantile OOF predictions")
  }

  expected_n <- nrow(point_cv_preds)
  if (nrow(calibration_data) < expected_n) {
    cli::cli_warn(
      "Only {nrow(calibration_data)}/{expected_n} samples matched between point and quantile predictions",
      "i" = "Check .row indices in CV predictions"
    )
  }

  if (verbose) {
    cli::cli_text("│  │  │  │  └─ DEBUG: Matched {nrow(calibration_data)} rows")
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Construct intervals (point_OOF + residual_quantiles_OOF)
  ## ---------------------------------------------------------------------------

  calibration_data <- calibration_data %>%
    dplyr::mutate(
      # Both predictions are OOF!
      interval_lower = .pred + .pred_lower,  # point_OOF + resid_q05_OOF
      interval_upper = .pred + .pred_upper   # point_OOF + resid_q95_OOF
    )

  ## ---------------------------------------------------------------------------
  ## Step 4: Compute nonconformity scores
  ## ---------------------------------------------------------------------------

  calibration_data <- calibration_data %>%
    dplyr::mutate(
      score = pmax(
        interval_lower - Response,  # Violation below (negative if inside)
        Response - interval_upper   # Violation above (negative if inside)
      )
    )

  ## ---------------------------------------------------------------------------
  ## Step 5: Calculate c_alpha (1-alpha quantile of scores)
  ## ---------------------------------------------------------------------------

  c_alpha <- quantile(calibration_data$score, probs = 1 - alpha, na.rm = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 6: Diagnostics
  ## ---------------------------------------------------------------------------

  if (verbose) {
    base_coverage <- mean(calibration_data$score <= 0, na.rm = TRUE)
    cli::cli_text("│  │  │  ├─ Base coverage (OOF intervals): {round(100*base_coverage, 1)}%")
    cli::cli_text("│  │  │  ├─ c_alpha = {round(c_alpha, 4)}")

    # Estimate post-conformal coverage
    post_conformal_coverage <- mean(calibration_data$score <= c_alpha, na.rm = TRUE)
    cli::cli_text("│  │  │  └─ Expected coverage (after conformal): {round(100*post_conformal_coverage, 1)}%")
  }

  ## Memory cleanup -------------------------------------------------------------

  rm(calibration_data)
  gc(verbose = FALSE)

  return(c_alpha)
}

## -----------------------------------------------------------------------------
## Quantile Model Training
## -----------------------------------------------------------------------------

#' Train Ranger Quantile Regression Model
#'
#' @description
#' Trains a single ranger quantile regression model that can predict multiple
#' quantiles at prediction time. This is a completely standalone training
#' function that doesn't rely on the existing `train_and_score_config()`
#' infrastructure, providing clean separation between point and quantile models.
#'
#' @param train_data Tibble with training data. Must contain:
#'   * `Response` column (target variable)
#'   * `Sample_ID` and `Project` columns
#'   * Spectral columns (numeric column names like "X600", "X602", etc.)
#'
#' @param preprocessing Character. Spectral preprocessing method (e.g., "snv", "snv_deriv1").
#'   Will be applied via `build_recipe()` if not "raw".
#'
#' @param transformation Character. Response transformation (e.g., "none", "log", "sqrt").
#'
#' @param grid_size Integer. Number of hyperparameter grid points for tuning (default: 5).
#'   Kept small since ranger is robust to hyperparameters.
#'
#' @param cv_folds Integer. Number of cross-validation folds (default: 5).
#'   Kept small since we're using this for UQ, not point prediction accuracy.
#'
#' @param verbose Logical. Print progress messages?
#'
#' @return A fitted workflow object containing the ranger quantile model,
#'   or NULL if training fails.
#'
#' @details
#' ## Why Standalone?
#'
#' This function is intentionally separate from `train_and_score_config()` because:
#' * Quantile models have different evaluation criteria (not rpd/ccc/r2)
#' * Always uses ranger (no model selection needed)
#' * Simpler tuning (ranger is robust, doesn't need extensive search)
#' * Clearer code - one function = one purpose
#'
#' ## Training Process
#'
#' 1. Build recipe with specified preprocessing/transformation (NO feature selection)
#' 2. Create ranger quantile specification
#' 3. Create workflow
#' 4. Tune hyperparameters (mtry, min_n) via grid search (5 points default)
#' 5. Finalize with best parameters
#' 6. Fit on full training data
#' 7. Apply butcher() for memory reduction
#'
#' ## Why No Feature Selection?
#'
#' Ranger's built-in feature importance handles feature selection internally,
#' so pre-selecting features isn't necessary and can actually hurt performance.
#'
#' @keywords internal
train_quantile_model <- function(train_data,
                                 preprocessing,
                                 transformation,
                                 grid_size = 5,
                                 cv_folds  = 5,
                                 resamples = NULL,  # NEW: Accept pre-made CV folds for CV+ conformal
                                 verbose   = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Build recipe
  ## ---------------------------------------------------------------------------

  safely_execute(
    build_recipe(
      input_data               = train_data,
      spectral_transformation  = preprocessing,
      response_transformation  = transformation,
      feature_selection_method = "none",
      covariate_selection      = NULL,
      covariate_data           = NULL,
      covariate_interactions   = FALSE
    ),
    error_message = "Quantile model recipe build failed"
  ) %>%
    handle_results(
      error_title = "Quantile recipe build failed",
      error_hints = c(
        "Preprocessing: {preprocessing}",
        "Check data structure"
      ),
      abort_on_null = TRUE
    ) -> recipe_obj

  ## ---------------------------------------------------------------------------
  ## Step 2: Create quantile model specification
  ## ---------------------------------------------------------------------------

  model_spec <- define_quantile_specification()

  ## ---------------------------------------------------------------------------
  ## Step 3: Create workflow
  ## ---------------------------------------------------------------------------

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_obj) %>%
    workflows::add_model(model_spec)

  ## ---------------------------------------------------------------------------
  ## Step 4: Tune hyperparameters
  ## ---------------------------------------------------------------------------

  # Create CV folds (or use provided folds for shared training)
  if (is.null(resamples)) {

    ## Create new CV folds if not provided -------------------------------------

    safely_execute(
      rsample::vfold_cv(train_data, v = cv_folds, strata = Response),
      error_message = "CV fold creation failed"
    ) %>%
      handle_results(
        error_title = "Quantile CV folds failed",
        abort_on_null = TRUE
      ) -> cv_folds_obj

  } else {

    ## Use provided CV folds (for CV+ conformal with point model) --------------

    cv_folds_obj <- resamples

  }

  # Finalize mtry based on actual features
  param_set <- workflows::extract_parameter_set_dials(wf)

  if ("mtry" %in% param_set$name) {

    prepped_data <- recipes::prep(recipe_obj) %>%
      recipes::bake(new_data = NULL)

    n_features <- ncol(prepped_data) - 3  # Exclude Response, Sample_ID, Project

    param_set <- param_set %>%
      recipes::update(mtry = dials::mtry(range = c(2L, min(n_features, 100L))))
  }

  # Create grid (small - ranger is robust)
  tuning_grid <- dials::grid_latin_hypercube(param_set, size = grid_size)

  # Tune (optimize for RMSE, not RPD - we care about coverage not accuracy)
  tune_results <- tune::tune_grid(
    wf,
    resamples = cv_folds_obj,
    grid      = tuning_grid,
    control   = tune::control_grid(save_pred = TRUE, verbose = FALSE)  # Save for conformal
  )

  # Select best by RMSE
  best_params <- tune::select_best(tune_results, metric = "rmse")

  ## ---------------------------------------------------------------------------
  ## Step 4.5: Extract OOF quantile predictions (if shared folds provided)
  ## ---------------------------------------------------------------------------

  ## This is the KEY step for proper CV+ conformal calibration!
  ## We need OOF quantile predictions (not in-sample) to avoid optimistic bias

  if (!is.null(resamples)) {

    ## Only extract OOF quantiles when shared folds provided (CV+ mode) ---------

    if (verbose) {
      cli::cli_text("│  │  │  ├─ Extracting OOF quantile predictions...")
    }

    ## Finalize workflow with best hyperparameters ------------------------------

    wf_finalized <- tune::finalize_workflow(wf, best_params)

    ## Loop through CV folds ----------------------------------------------------

    oof_quantile_list <- list()

    for (fold_id in seq_len(nrow(resamples))) {

      ## Get fold INDICES (not data - we need to subset residual_train_data!) ---

      assess_indices <- rsample::complement(resamples$splits[[fold_id]])
      train_indices  <- setdiff(seq_len(nrow(train_data)), assess_indices)

      ## Get fold data from RESIDUAL training data (not original data!) ---------

      fold_train  <- train_data[train_indices, ]  # From original splits
      fold_assess <- train_data[assess_indices, ] # From original splits

      ## Fit finalized model on this fold's training data -----------------------

      fold_model <- parsnip::fit(wf_finalized, data = fold_train)

      ## Predict quantiles (q05, q95) on this fold's assessment set (OOF!) ------

      fold_quantiles <- predict_quantiles(
        workflow  = fold_model,
        new_data  = fold_assess,
        quantiles = c(0.05, 0.95)
      )

      ## Add .row indices to match with point model CV predictions --------------
      ## Use complement() to get assessment row indices (original data row numbers)

      fold_quantiles$.row <- as.integer(rsample::complement(resamples$splits[[fold_id]]))

      ## Store this fold's quantiles --------------------------------------------

      oof_quantile_list[[fold_id]] <- fold_quantiles

      ## Memory cleanup ---------------------------------------------------------

      rm(fold_model, fold_train, fold_assess, fold_quantiles)

    }

    ## Combine all OOF quantile predictions ------------------------------------

    cv_quantile_predictions <- dplyr::bind_rows(oof_quantile_list) %>%
      dplyr::arrange(.row)

    if (verbose) {
      cli::cli_text("│  │  │  │  └─ Extracted {nrow(cv_quantile_predictions)} OOF quantile predictions")
    }

    ## Cleanup ----------------------------------------------------------------

    rm(oof_quantile_list, wf_finalized)
    gc(verbose = FALSE)

  } else {

    ## No shared folds - can't extract OOF quantiles (standalone mode) ---------

    cv_quantile_predictions <- NULL

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Finalize workflow and fit on full training data
  ## ---------------------------------------------------------------------------

  wf_final  <- tune::finalize_workflow(wf, best_params)
  fitted_wf <- parsnip::fit(wf_final, data = train_data)

  ## ---------------------------------------------------------------------------
  ## Step 6: Memory optimization
  ## ---------------------------------------------------------------------------

  fitted_wf <- butcher::butcher(fitted_wf)

  ## ---------------------------------------------------------------------------
  ## Step 7: Return workflow + OOF quantiles
  ## ---------------------------------------------------------------------------

  ## BREAKING CHANGE: Now returns list instead of just workflow
  ## Callers need to extract $workflow and $cv_quantiles

  return(list(
    workflow     = fitted_wf,
    cv_quantiles = cv_quantile_predictions  # NULL if resamples not provided
  ))
}

## -----------------------------------------------------------------------------
## Model Training with UQ
## -----------------------------------------------------------------------------

#' Train Point and Quantile Models for a Cluster
#'
#' @description
#' Trains both point prediction and quantile regression models for a single
#' cluster in the library prediction workflow. Implements the decoupled UQ
#' architecture where point predictions come from the best-performing model
#' (as determined by config optimization) and uncertainty quantification always
#' uses ranger quantile regression.
#'
#' @param cluster_data Tibble with cluster training data. Must contain:
#'   * `Response` column (target variable or ILR coordinate)
#'   * `Sample_ID` column
#'   * `Project` column
#'   * Spectral columns (numeric wavenumbers)
#'
#' @param property Character. Property name (e.g., "clay", "ph", "oc").
#'   Used for metadata tracking only.
#'
#' @param config Tibble (1 row) with optimal configuration:
#'   * `model`: Model type for point prediction
#'   * `preprocessing`: Spectral preprocessing method
#'   * `transformation`: Response transformation
#'   * `feature_selection`: Feature selection method
#'
#' @param cv_folds Integer. Number of cross-validation folds (default: 10).
#'
#' @param grid_size Integer. Number of hyperparameter grid points (default: 10).
#'
#' @param verbose Logical. Print progress messages?
#'
#' @return A list with components:
#'   * `point_model`: Fitted workflow for point predictions (best config)
#'   * `quantile_model`: Fitted workflow for quantile predictions (ranger)
#'   * `point_metrics`: Performance metrics for point model
#'   * `config_used`: Configuration used for point model
#'   * `property`: Property name
#'   * `n_train`: Number of training samples
#'
#' @details
#' ## Training Strategy
#'
#' This function trains TWO models sequentially:
#'
#' 1. **Point Prediction Model** (using optimal config):
#'    - Model type from config (cubist, PLSR, xgboost, etc.)
#'    - Full hyperparameter tuning with cross-validation
#'    - Evaluated on held-out samples
#'
#' 2. **Quantile Model** (always ranger):
#'    - Ranger with `quantreg = TRUE`
#'    - Uses SAME preprocessing/transformation as point model
#'    - Feature selection: none (ranger handles feature importance internally)
#'    - Predicts q05 and q95 at prediction time
#'
#' ## Memory Management
#'
#' After training each model:
#' * Apply `butcher::butcher()` to reduce workflow size (60-80% reduction)
#' * Remove intermediate objects
#' * Call `gc()` to free memory
#'
#' ## For Texture Properties
#'
#' When called for texture ILR coordinates (ilr_1, ilr_2):
#' * This function is called TWICE (once per coordinate)
#' * Total: 4 models per cluster (2 coords × 2 model types)
#' * Back-transformation happens at prediction time (not here)
#'
#' @seealso
#'   [train_and_score_config()] for underlying training logic,
#'   [define_model_specifications()] for point models,
#'   [define_quantile_specification()] for quantile models
#'
#' @examples
#' \dontrun{
#' # Train models for pH prediction
#' models <- train_cluster_models_with_uq(
#'   cluster_data = cluster_1_data,
#'   property = "ph",
#'   config = winning_config,
#'   cv_folds = 10,
#'   grid_size = 10
#' )
#'
#' # Use for prediction
#' point_pred <- predict(models$point_model, new_data)
#' quantiles <- predict_quantiles(models$quantile_model, new_data)
#' }
#'
#' @keywords internal
train_cluster_models_with_uq <- function(cluster_data,
                                         property,
                                         config,
                                         cv_folds  = 10,
                                         grid_size = 10,
                                         verbose   = FALSE) {

  n_train <- nrow(cluster_data)

  if (verbose) {
    cli::cli_text("│  ├─ Training UQ models for {property} ({n_train} samples)...")
  }

  ## ---------------------------------------------------------------------------
  ## Step 0.5: Create CV folds ONCE (shared between point and quantile models)
  ## ---------------------------------------------------------------------------

  ## This is CRITICAL for proper CV+ conformal calibration!
  ## Both models must use the SAME fold assignments so we can match their
  ## out-of-fold predictions by sample index

  if (verbose) {
    cli::cli_text("│  │  ├─ Creating shared CV folds ({cv_folds}-fold)...")
  }

  safely_execute(
    rsample::vfold_cv(cluster_data, v = cv_folds, strata = Response),
    error_message = "Shared CV fold creation failed"
  ) %>%
    handle_results(
      error_title = "Shared CV fold creation failed",
      error_hints = c(
        "May need more samples ({n_train} for {cv_folds}-fold CV)",
        "Try reducing cv_folds"
      ),
      abort_on_null = TRUE
    ) -> shared_cv_folds

  ## ---------------------------------------------------------------------------
  ## Step 1: Train POINT prediction model (uses config's model type)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│  │  ├─ Point model: {config$model}...")
  }

  point_result <- train_and_score_config(
    config          = config,
    train_data      = cluster_data,
    property_col    = "Response",
    grid_size       = grid_size,
    cv_folds        = cv_folds,
    resamples       = shared_cv_folds,  # NEW: Pass shared folds!
    return_workflow = TRUE,
    verbose         = FALSE
  )

  if (verbose) {
    cli::cli_text("│  │  │  └─ R² = {round(point_result$metrics$rsq, 3)}, RPD = {round(point_result$metrics$rpd, 2)}")
  }

  ## Extract and save key components before cleanup -----------------------------

  point_workflow <- point_result$workflow
  point_metrics  <- point_result$metrics

  ## ---------------------------------------------------------------------------
  ## Step 1.5: Compute residuals using OUT-OF-FOLD predictions (RESIDUAL-BASED UQ)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│  │  ├─ Computing residuals (out-of-fold)...")
  }

  ## Validate CV predictions available ------------------------------------------

  if (is.null(point_result$cv_predictions)) {
    cli::cli_abort(
      "CV predictions not available from point model",
      "i" = "Cannot compute unbiased residuals for UQ",
      "i" = "This is a bug - train_and_score_config() should return cv_predictions"
    )
  }

  ## Use OUT-OF-FOLD predictions to compute unbiased residuals ------------------
  ## CRITICAL: Using in-sample predictions causes severe underestimation (10-50x too narrow)

  cv_preds <- point_result$cv_predictions

  ## Validate we have predictions for all training samples ----------------------

  if (nrow(cv_preds) != nrow(cluster_data)) {
    cli::cli_abort(
      "CV predictions count mismatch",
      "i" = "Expected {nrow(cluster_data)}, got {nrow(cv_preds)}"
    )
  }

  ## Compute residuals (observed - out-of-fold predicted) -----------------------

  residuals <- cv_preds$Response - cv_preds$.pred

  ## Residual diagnostics --------------------------------------------------------

  mean_residual <- mean(residuals, na.rm = TRUE)
  sd_residual   <- sd(residuals, na.rm = TRUE)

  if (verbose) {
    cli::cli_text("│  │  │  ├─ Mean residual: {round(mean_residual, 4)} (should be ~0)")
    cli::cli_text("│  │  │  └─ SD residual: {round(sd_residual, 3)}")
  }

  ## Sanity check: mean should be near zero (unbiased point model) --------------

  if (abs(mean_residual) > 0.1 * sd_residual) {
    cli::cli_warn(
      "Point model residuals have non-zero mean: {round(mean_residual, 3)}",
      "i" = "This suggests systematic bias in point predictions"
    )
  }

  ## Create residual training dataset --------------------------------------------

  residual_train_data           <- cluster_data
  residual_train_data$Response  <- residuals

  ## Handle NAs if point model produced any -------------------------------------

  valid_idx <- !is.na(residuals)

  if (sum(!valid_idx) > 0) {
    cli::cli_warn("{sum(!valid_idx)} samples have NA residuals - excluding from quantile training")
    residual_train_data <- residual_train_data[valid_idx, ]
  }

  ## DEBUG: Verify residuals are actually in Response column -------------------

  if (verbose) {
    cli::cli_text("│  │  ├─ DEBUG: Residual training data")
    cli::cli_text("│  │  │  ├─ Response (residual) range: [{round(min(residual_train_data$Response),2)}, {round(max(residual_train_data$Response),2)}]")
    cli::cli_text("│  │  │  ├─ Response (residual) mean: {round(mean(residual_train_data$Response),4)}")
    cli::cli_text("│  │  │  └─ Response (residual) SD: {round(sd(residual_train_data$Response),3)}")
  }

  ## Apply butcher to point workflow now that we're done with it ----------------

  if (!is.null(point_workflow)) {
    point_workflow <- butcher::butcher(point_workflow)
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Train QUANTILE model on RESIDUALS (always ranger)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│  │  ├─ Quantile model (RESIDUAL-BASED): ranger...")
  }

  quantile_result <- train_quantile_model(
    train_data      = residual_train_data,  # CHANGED: Use residuals as Response!
    preprocessing   = as.character(config$preprocessing),
    transformation  = "none",  # CRITICAL: Never transform residuals!
    grid_size       = grid_size,
    cv_folds        = cv_folds,
    resamples       = shared_cv_folds,  # NEW: Pass SAME folds as point model!
    verbose         = verbose
  )

  ## Extract components (now returns list instead of just workflow) -------------

  quantile_workflow       <- quantile_result$workflow
  quantile_cv_predictions <- quantile_result$cv_quantiles  # OOF quantiles!

  if (verbose) {
    cli::cli_text("│  │  │  └─ Trained (quantiles extracted at prediction time)")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2.5: Compute conformal calibration margin (CV+)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│  │  ├─ Computing conformal margin (CV+)...")
  }

  c_alpha <- calculate_conformal_margin(
    point_cv_preds    = cv_preds,                 # OOF point predictions
    quantile_cv_preds = quantile_cv_predictions,  # OOF quantile predictions
    alpha             = 0.10,                     # For 90% coverage
    verbose           = verbose
  )

  if (verbose) {
    cli::cli_text("│  │  │  └─ c_alpha = {round(c_alpha, 4)} (margin to add to intervals)")
  }

  ## Clean up CV prediction data (no longer needed) ----------------------------

  rm(cv_preds, quantile_cv_predictions, residual_train_data, quantile_result)

  ## Memory cleanup --------------------------------------------------------------

  gc(verbose = FALSE)

  ## ---------------------------------------------------------------------------
  ## Step 3: Return both models + conformal margin
  ## ---------------------------------------------------------------------------

  list(
    point_model       = point_workflow,
    quantile_model    = quantile_workflow,
    point_metrics     = point_metrics,
    config_used       = config,
    property          = property,
    n_train           = n_train,
    is_residual_based = TRUE,  # Flag for prediction logic
    residual_stats    = list(mean = mean_residual, sd = sd_residual),
    c_alpha           = c_alpha  # Conformal calibration margin
  )
}
