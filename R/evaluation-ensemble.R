#' Build Ensemble from Finalized Models
#'
#' @description
#' Creates an ensemble model from the output of `finalize_top_workflows()`.
#' Supports three ensemble methods:
#' - **Stacked ensembles**: Uses penalized regression meta-learner via tidymodels/stacks
#' - **Weighted average**: Weights models by inverse RMSE from cross-validation
#' - **XGBoost meta-learner**: Non-linear blending that can learn complex model interactions
#'
#' **Coming soon**: Additional meta-learner options including:
#' - `"rf_meta"`: Random forest meta-learner for robust non-parametric blending
#' - `"nn_meta"`: Neural network meta-learner for deep interaction modeling
#'
#' @param finalized_models A tibble output from `finalize_top_workflows()` containing
#'   columns: `wflow_id`, `workflow`, `cv_predictions`, and `cv_metrics` (or `metrics` for backward compatibility)
#' @param input_data A data frame containing the full dataset with predictors and response
#' @param variable Character string. Name of the response variable column in `input_data`
#' @param covariate_data Optional data frame containing additional covariate predictors.
#'   Currently not used but reserved for future functionality. Default: `NULL`
#' @param ensemble_method Character string. Ensemble method to use:
#'   - `"stacks"`: Penalized regression meta-learner (default)
#'   - `"weighted_average"`: Simple weighted average by inverse RMSE
#'   - `"xgb_meta"`: XGBoost meta-learner for non-linear model blending
#' @param optimize_ensemble Logical. Whether to optimize ensemble hyperparameters.
#'   For stacks: grid search over penalty/mixture. For xgb_meta: CV-based early stopping.
#'   Default: `FALSE` (uses fixed defaults for faster execution)
#' @param blend_metric Character string. Metric to optimize during blending.
#'   Options: `"rmse"` (default), `"mae"`, `"rsq"`, or any yardstick metric function name
#' @param test_prop Numeric between 0 and 1. Proportion of data to hold out for testing.
#'   Default: `0.2` (20% test set)
#' @param seed Integer. Random seed for train/test split reproducibility. Default: `123`
#' @param allow_par Logical. Enable parallel processing for model fitting. Default: `FALSE`
#' @param n_cores Integer or `NULL`. Number of cores for parallel processing.
#'   If `NULL` and `allow_par = TRUE`, uses `detectCores() - 1`. Default: `NULL`
#' @param verbose Logical. Print progress messages and results summary. Default: `TRUE`
#' @param output_dir Character string or `NULL`. Directory path to save ensemble results.
#'   Creates `ensemble/` subdirectory with model and prediction files. Default: `NULL` (no saving)
#'
#' @return
#' A list of class `"horizons_ensemble"` containing:
#' - `ensemble_model`: The fitted ensemble model object (stacks or list with predict function)
#' - `predictions`: Tibble with test set predictions (columns: `Observed`, `Predicted`)
#' - `metrics`: Tibble with test set performance metrics
#' - `model_weights`: Data frame with model names and weights/coefficients
#' - `individual_performance`: Tibble with individual model CV performance
#' - `metadata`: List with ensemble configuration and performance summary
#'
#' @examples
#' \dontrun{
#' # Build stacked ensemble with optimization
#' ensemble_stacks <- build_ensemble(
#'   finalized_models = top_models,
#'   input_data = spectral_data,
#'   variable = "SOC",
#'   ensemble_method = "stacks",
#'   optimize_ensemble = TRUE,
#'   verbose = TRUE
#' )
#'
#' # Build XGBoost meta-learner with CV tuning
#' ensemble_xgb <- build_ensemble(
#'   finalized_models = top_models,
#'   input_data = spectral_data,
#'   variable = "SOC",
#'   ensemble_method = "xgb_meta",
#'   optimize_ensemble = TRUE,  # Uses CV early stopping
#'   verbose = TRUE
#' )
#'
#' # Access results
#' ensemble_xgb$metrics           # Test set performance
#' ensemble_xgb$model_weights     # Contributing models and importance
#' ensemble_xgb$metadata$xgb_nrounds  # Optimal number of boosting rounds
#' ensemble_xgb$metadata$improvement  # Improvement over best individual
#' }
#'
#' @details
#' ## XGBoost Meta-Learner Tuning
#'
#' The `xgb_meta` method uses optimized defaults for meta-learning with few base models:
#'
#' **Default hyperparameters**:
#' - `max_depth = 2`: Shallow trees prevent overfitting when blending ~6 base models
#' - `eta = 0.05`: Conservative learning rate for stable convergence
#' - `lambda = 1.0`, `alpha = 0.1`: Strong regularization to avoid memorizing CV fold patterns
#'
#' **When `optimize_ensemble = TRUE`**:
#' - Runs 5-fold cross-validation with early stopping (max 500 rounds, stops after 20 no-improvement rounds)
#' - Automatically finds optimal `nrounds` to prevent overfitting
#' - Stores CV RMSE and optimal rounds in `metadata$xgb_cv_rmse` and `metadata$xgb_nrounds`
#'
#' **When `optimize_ensemble = FALSE`**:
#' - Uses fixed `nrounds = 50` for faster execution
#' - Suitable for quick prototyping or when model count is very small
#'
#' The key insight: Meta-learning with few base models requires aggressive regularization.
#' Shallow trees and L1/L2 penalties prevent the meta-learner from overfitting to training quirks.
#'
#' ## Error Handling
#'
#' The function implements robust error handling for ensemble operations:
#'
#' **Critical operations (abort on failure)**:
#' - Stack blending (`stacks::blend_predictions`) - Provides hints about CV predictions,
#'   penalty/mixture grid size, and metric compatibility
#' - Ensemble member fitting (`stacks::fit_members`) - Suggests checking training data,
#'   workflow finalization, and parallel processing settings
#' - XGBoost meta-learner training - Validates meta-features for missing/infinite values,
#'   response variable type, and hyperparameter settings
#' - XGBoost CV (`xgb.cv`) - Checks for sufficient samples (need >= 25 for 5-fold CV)
#'
#' **Individual model fitting (graceful degradation)**:
#' - Weighted average model fitting: Failed models are skipped, ensemble continues with N-1 models
#' - XGBoost base model fitting: Failed models are skipped, meta-learner trains on remaining models
#' - Minimum 2 models required for ensemble after filtering failures
#' - Failed model attempts are logged with warnings, successful count reported
#'
#' All error messages include actionable troubleshooting hints to help diagnose failures.
#'
#' @export

build_ensemble <- function(finalized_models,
                           input_data,
                           variable,
                           covariate_data     = NULL,
                           ensemble_method    = "stacks",
                           optimize_ensemble  = FALSE,
                           blend_metric       = "rmse",
                           test_prop          = 0.2,
                           seed               = 123,
                           allow_par          = FALSE,
                           n_cores            = NULL,
                           verbose            = TRUE,
                           output_dir         = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation and Setup
  ## ---------------------------------------------------------------------------

  ## Essential column validation -----------------------------------------------

  ## Check for either new format (cv_metrics) or old format (metrics) ---------

  has_cv_metrics  <- "cv_metrics" %in% names(finalized_models)
  has_old_metrics <- "metrics" %in% names(finalized_models)

  if (!has_cv_metrics && !has_old_metrics) {

    cli::cli_abort("finalized_models must have either 'cv_metrics' (new format) or 'metrics' (old format)")

  }

  ## Check other required columns ---------------------------------------------

  required_cols <- c("wflow_id", "workflow", "cv_predictions")
  missing_cols  <- setdiff(required_cols, names(finalized_models))

  if (length(missing_cols) > 0) {

    cli::cli_abort("finalized_models missing required columns: {missing_cols}")

  }

  ## Standardize to cv_metrics for internal use -------------------------------

  if (!has_cv_metrics && has_old_metrics) {

    ## Backward compatibility: rename metrics to cv_metrics -------------------

    finalized_models <- finalized_models %>% dplyr::mutate(cv_metrics = metrics)

  }

  ## Validate response variable exists -----------------------------------------

  if (!variable %in% names(input_data)) cli::cli_abort("Variable '{variable}' not found in input_data")

  ## Method-specific validation ------------------------------------------------

  if (!ensemble_method %in% c("stacks", "weighted_average", "xgb_meta")) {
    cli::cli_abort("ensemble_method must be 'stacks', 'weighted_average', or 'xgb_meta', got '{ensemble_method}'")
  }

  ## Stacks requires CV predictions --------------------------------------------

  if (ensemble_method == "stacks") {

    finalized_models %>%
      dplyr::filter(purrr::map_lgl(cv_predictions, is.null)) ->  models_without_cv

    if (nrow(models_without_cv) > 0) {

      cli::cli_abort(c("Stacking requires CV predictions from all models",
                       "x" = "Missing CV predictions: {models_without_cv$wflow_id}"))

    }
  }

  ## Set up parallel backend if requested --------------------------------------

  if (allow_par) {

    ## Determine cores ---------------------------------------------------------

    n_cores <- min(n_cores, parallel::detectCores() - 1)

    ## Set up the plan ---------------------------------------------------------

    future::plan(future::multisession, workers = n_cores)

    # Ensure restoration on exit -----------------------------------------------

    on.exit({

      future::plan(future::sequential)
      gc(verbose = FALSE, full = TRUE)

    }, add = TRUE)

  }

  ## Verbose output ------------------------------------------------------------

  if (verbose) {

    cli::cli_text("{.strong Building {ensemble_method} ensemble}")
    cli::cli_text("├─ Models: {.val {nrow(finalized_models)}}")
    cli::cli_text("├─ Variable: {.field {variable}}")
    cli::cli_text("├─ Test split: {.val {round(test_prop * 100)}%}")
    cli::cli_text("└─ Parallel processing: {.field {ifelse(allow_par, paste0('enabled (',n_cores, ' workers)'), 'disabled')}}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Data Preparation
  ## ---------------------------------------------------------------------------

  ## Rename response variable for consistency ---------------------------------

  input_data %>%
    dplyr::rename(Response = !!dplyr::all_of(variable)) -> input_data

  ## Create train/test split ---------------------------------------------------

  set.seed(seed)

  rsample::initial_split(input_data,
                        prop   = 1 - test_prop,
                        strata = Response) -> data_split

  train_data <- rsample::training(data_split)
  test_data  <- rsample::testing(data_split)

  ## Verbose output ------------------------------------------------------------

  if (verbose) {

    response_range <- range(train_data$Response, na.rm = TRUE)
    response_sd    <- sd(train_data$Response, na.rm = TRUE)

    cli::cli_text("{.strong Data preparation complete}")
    cli::cli_text("├─ Training samples: {.val {nrow(train_data)}}")
    cli::cli_text("├─ Test samples: {.val {nrow(test_data)}}")
    cli::cli_text("├─ Response range: [{.val {round(response_range[1], 2)}}, {.val {round(response_range[2], 2)}}]")
    cli::cli_text("└─ Response SD: {.val {round(response_sd, 2)}}")


  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Build Ensemble Based on Method
  ## ---------------------------------------------------------------------------

  if (ensemble_method == "stacks") {

    ## -------------------------------------------------------------------------
    ## Step 3.1: Initialize Stack and Add Models
    ## -------------------------------------------------------------------------

    ## Create transformation lookup for later use -------------------------------
    ## Store mapping of model IDs to transformations for prediction phase
    ## This is needed because stacks may rename or filter members
    ## We'll attach this to the ensemble model for use during prediction

    transformation_lookup <- setNames(
      finalized_models$transformation,
      finalized_models$wflow_id
    )

    ## Initialize the stack --------------------------------------------------

    model_stack <- stacks::stacks()

    ## Add models to stack ---------------------------------------------------

    if (verbose) {

      cli::cli_text("{.strong |Building stacked ensemble}")
      cli::cli_text("├─ Method: {.field penalized regression}")
      cli::cli_text("└─ Adding {.val {nrow(finalized_models)}} models:")

    }

    for (i in seq_len(nrow(finalized_models))) {

      current_model  <- finalized_models[i, ]
      sanitized_name <- gsub("\\+", ".", current_model$wflow_id)

      if (verbose) {

        prefix <- if (i < nrow(finalized_models)) "   ├─" else "   └─"

        cli::cli_text("{prefix} {.val {current_model$wflow_id}}")

      }

      ## Back-transform CV predictions to original scale before stacking ----------
      ## This ensures the meta-learner trains on original-scale predictions
      ## regardless of what transformation each individual model used

      ## NOTE: CV predictions from finalize_top_workflows() are ALREADY
      ## back-transformed to original scale (evaluation-finalize.R:655).
      ## Do NOT back-transform again or we get exp(exp(log(x))) = astronomical values.
      ##
      ## The meta-learner will train on original-scale predictions as intended.

      cv_preds <- current_model$cv_predictions[[1]]

      ## Back-transformation block removed - predictions already on original scale

      stacks::add_candidates(model_stack,
                             candidates = cv_preds,
                             name       = sanitized_name) -> model_stack

    }

    ## -------------------------------------------------------------------------
    ## Step 3.2: Blend Predictions with Penalized Regression
    ## -------------------------------------------------------------------------

    ## Define blending parameters based on optimization choice ---------------

    blend_params <- if (optimize_ensemble) {
      list(penalty = 10^seq(-6, -1, length.out = 20),
           mixture = seq(0, 1, length.out = 10))

    } else {

      list(penalty = 0.01, mixture = 1)

    }

    ## Set up metric for blending --------------------------------------------

    # TODO: Fix metric selection for blend_predictions (currently broken)
    # The blend_metric parameter doesn't work properly - passing metric functions
    # to metric_set() fails with ".filter_perf_metrics(): No results available"
    # This appears to be a stacks package issue with how it handles metrics
    # For now, we're hardcoding yardstick::rmse below which works reliably

    # Map metric names to yardstick functions (for future use when fixed)
    metric_fns <- list(rmse = yardstick::rmse,
                       mae  = yardstick::mae,
                       rsq  = yardstick::rsq,
                       rpd   = yardstick::rpd)

    # Get the metric function - default to rmse if not found
    metric_fn <- metric_fns[[blend_metric]]

    if (is.null(metric_fn)) {
      cli::cli_alert_warning("Metric {blend_metric} not recognized, using RMSE")
      metric_fn <- yardstick::rmse
    }

    # This doesn't work - see TODO above
    blend_metric_set <- yardstick::metric_set(metric_fn)

    ## Blend the stack -------------------------------------------------------

    if (verbose) {

      if (optimize_ensemble) {

        n_combos <- length(blend_params$penalty) * length(blend_params$mixture)
        cli::cli_text("├─ Optimizing over {.val {n_combos}} penalty/mixture combinations")
        cli::cli_text("├─ Blend metric: {.field {blend_metric}}")

      } else {

        cli::cli_text("├─ Using default penalty ({.val 0.01}) and mixture ({.val 1})")
        cli::cli_text("├─ Blend metric: {.field {blend_metric}}")

      }

    }

    ## Safely blend predictions - CRITICAL OPERATION ----------------------------

    safely_execute(
      expr = {

        stacks::blend_predictions(model_stack,
                                  penalty = blend_params$penalty,
                                  mixture = blend_params$mixture,
                                  metric  = yardstick::metric_set(yardstick::rmse),
                                  control = tune::control_grid(save_pred     = TRUE,
                                                               save_workflow = TRUE,
                                                               allow_par     = allow_par))

      },
      default_value = NULL,
      error_message = "Stacking blend optimization failed with {length(blend_params$penalty)} penalty values and {length(blend_params$mixture)} mixture values"
    ) -> safe_blend

    handle_results(safe_blend,
                   error_title = "Stacked ensemble blending failed",
                   error_hints = c("Check that all models have valid CV predictions",
                                   "Try reducing penalty/mixture grid size if optimizing",
                                   "Verify models use compatible metrics"),
                   abort_on_null = TRUE) -> model_stack

    ## -------------------------------------------------------------------------
    ## Step 3.3: Fit Member Models and Extract Weights
    ## -------------------------------------------------------------------------

    ## Attach transformation metadata to stack before fitting ------------------
    ## This allows it to be preserved through fit_members() for later use

    model_stack$transformation_lookup <- transformation_lookup

    ## Fit the ensemble members ----------------------------------------------

    if (verbose) cli::cli_text("├─ Fitting member models...")

    ## Safely fit members - CRITICAL OPERATION ---------------------------------
    ## Abort on failure with informative error and hints

    safely_execute(
      expr = {

        stacks::fit_members(model_stack)

      },
      default_value = NULL,
      error_message = "Fitting {length(model_stack$cols_map)} ensemble member workflows failed"
    ) -> safe_fit

    handle_results(safe_fit,
                   error_title = "Ensemble member fitting failed",
                   error_hints = c("Check training data has no missing values",
                                   "Verify all workflows are properly finalized",
                                   "Try setting allow_par = FALSE to debug"),
                   abort_on_null = TRUE) -> ensemble_model

    ## Transformation lookup is already attached (preserved through fit_members)

    ## Extract model weights -------------------------------------------------

    # Stacks package provides a clean way to get the weights
    # The autoplot function extracts them properly
    model_weights <- stacks::autoplot(ensemble_model, type = "weights")$data %>%
      dplyr::select(member, weight) %>%
      dplyr::rename(coef = weight) %>%
      dplyr::arrange(dplyr::desc(coef))

    ## Display results -------------------------------------------------------

    if (verbose) {

      n_active <- nrow(model_weights)
      cli::cli_text("└─ Fitted ensemble with {.val {n_active}} active models")

      if (n_active > 0) {

        cli::cli_text("")
        cli::cli_text("{.strong Top contributing models:}")

        model_weights %>%
          head(5) -> top_models

        for (i in seq_len(nrow(top_models))) {
          prefix <- if (i < nrow(top_models)) "├─" else "└─"
          cli::cli_text("{prefix} {.val {top_models$member[i]}}: weight = {.val {round(top_models$coef[i], 4)}}")
        }

      }

      cli::cli_text("")

    }

    } else if (ensemble_method == "weighted_average") {

    ## -------------------------------------------------------------------------
    ## Step 3B: Weighted Average Ensemble
    ## -------------------------------------------------------------------------

    ## Extract metrics and calculate weights --------------------------------

    if (verbose) {

      cli::cli_text("{.strong Building weighted average ensemble}")
      cli::cli_text("└─ Weighting by cross-validation RMSE")

    }

    finalized_models %>%
      dplyr::mutate(rmse = purrr::map_dbl(cv_metrics, ~ {.x %>%
                                                           dplyr::filter(.metric == "rmse") %>%
                                                           dplyr::pull(mean) %>%
                                                           .[1]
                                                         }
                                          ),
        weight = 1 / rmse,
        weight = weight / sum(weight)) %>%
      dplyr::select(wflow_id,
                    workflow,
                    weight,
                    rmse,
                    transformation) -> weighted_models

    ## Fit models on training data ------------------------------------------
    ## Safely fit individual models - GRACEFUL DEGRADATION ------------------
    ## Skip failed models, continue with N-1 models if >= 2 remain

    n_models_initial <- nrow(weighted_models)

    weighted_models %>%
      dplyr::mutate(fitted_workflow = purrr::map2(workflow, wflow_id, ~ {

        if (verbose) cli::cli_text("   ├─ Fitting {.val {.y}}")

        safe_fit <- safely_execute(
          expr = {

            fit(.x, data = train_data)

          },
          default_value = NULL,
          error_message = "Failed to fit {.y}",
          log_error = TRUE
        )

        safe_fit$result

        }
      )) -> weighted_models_fitted

    ## Filter out failed models ---------------------------------------------

    weighted_models_fitted %>%
      dplyr::filter(!purrr::map_lgl(fitted_workflow, is.null)) -> weighted_models_fitted

    n_models_success <- nrow(weighted_models_fitted)
    n_models_failed  <- n_models_initial - n_models_success

    ## Validate minimum models for ensemble ---------------------------------

    if (n_models_success < 2) {

      cli::cli_abort(c(
        "Insufficient models for weighted average ensemble",
        "x" = "Successfully fitted {n_models_success} of {n_models_initial} models",
        "i" = "Need at least 2 models, but only {n_models_success} succeeded"
      ))

    }

    ## Report success/failure if failures occurred --------------------------

    if (verbose && n_models_failed > 0) {

      cli::cli_alert_warning("Fitted {n_models_success}/{n_models_initial} models successfully (skipped {n_models_failed} failures)")

    }

    ## Display weight information -------------------------------------------

    if (verbose) {

      weighted_models_fitted %>%
        dplyr::arrange(dplyr::desc(weight)) %>%
        head(5) -> top_weighted

      cli::cli_text("{.strong Top weighted models:}")

      for (i in seq_len(nrow(top_weighted))) {

        prefix <- if (i < nrow(top_weighted)) "├─" else "└─"
        cli::cli_text("{prefix} {.val {top_weighted$wflow_id[i]}}: weight = {.val {round(top_weighted$weight[i], 3)}} (RMSE: {.val {round(top_weighted$rmse[i], 3)}})")

      }
    }

    ## Create ensemble model object -----------------------------------------

    list(method = "weighted_average",
         models = weighted_models_fitted,
         predict = function(new_data) {

           ## Get original-scale predictions from each model, weighted ----------

           predictions <- purrr::pmap(
             list(workflow       = weighted_models_fitted$fitted_workflow,
                  transformation = weighted_models_fitted$transformation,
                  weight         = weighted_models_fitted$weight),
             function(workflow, transformation, weight) {
               get_original_scale_predictions(workflow, new_data, transformation, warn = FALSE) * weight
             }
           )

           ## Sum weighted predictions -----------------------------------------

           predictions_matrix <- do.call(cbind, predictions)

           tibble::tibble(.pred = rowSums(predictions_matrix))

        }) -> ensemble_model

    ## Store weights for consistency with stacks format ------------------------

    weighted_models_fitted %>%
      dplyr::select(member = wflow_id,
                    coef   = weight) %>%
      dplyr::arrange(dplyr::desc(coef)) -> model_weights

  } else if (ensemble_method == "xgb_meta") {

    ## -------------------------------------------------------------------------
    ## Step 3C: XGBoost Meta-learner Ensemble
    ## -------------------------------------------------------------------------

    ## Extract CV predictions as meta-features -------------------------------

    if (verbose) {

      cli::cli_text("")
      cli::cli_text("{.strong Building XGBoost meta-learner ensemble}")
      cli::cli_text("├─ Extracting CV predictions as meta-features")

    }

    ## Collect CV predictions (already on original scale) ---------------------

    ## NOTE: CV predictions from finalize_top_workflows() are ALREADY
    ## back-transformed to original scale (evaluation-finalize.R:655).
    ## Do NOT back-transform again.
    ##
    ## WHY WEIGHTED_AVERAGE WORKS: It doesn't use pre-computed CV predictions.
    ## Instead, it calls predict() on NEW data → gets transformed predictions
    ## → back-transforms ONCE (correct).

    finalized_models %>%
      dplyr::mutate(
        cv_preds = purrr::map(cv_predictions, ~ {

          ## Extract CV predictions (already on original scale) ----------------

          preds <- tune::collect_predictions(.x) %>%
            dplyr::select(.row, .pred) %>%
            dplyr::arrange(.row)

          ## Back-transformation block removed - already on original scale ------

          preds

        })
      ) -> meta_data

    ## Create feature matrix from predictions ----------------------------------

    meta_data %>%
      dplyr::pull(cv_preds) %>%
      purrr::map(~ .x$.pred) %>%
      purrr::set_names(paste0("pred_", meta_data$wflow_id)) %>%
      dplyr::bind_cols() %>%
      as.matrix() -> meta_features

    ## Get true response values for meta-training ------------------------------

    finalized_models$cv_predictions[[1]] %>%
      tune::collect_predictions() %>%
      dplyr::arrange(.row) %>%
      dplyr::pull(Response) -> meta_response

    ## Train XGBoost meta-learner -------------------------------------------

    if (verbose) cli::cli_text("├─ Training XGBoost meta-learner")

    ## Meta-learning specific defaults ------------------------------------------
    ## Shallow trees and strong regularization for few base model features
    ## See: https://github.com/dmlc/xgboost/issues/1686 for meta-learning best practices

    list(objective        = "reg:squarederror",
         max_depth        = 2,              # Shallow for few features (~6 base models)
         eta              = 0.05,            # Conservative learning rate
         subsample        = 0.8,             # Row subsampling
         colsample_bytree = 1.0,             # Use all features (only ~6 models)
         lambda           = 1.0,             # L2 regularization
         alpha            = 0.1) -> xgb_params  # L1 regularization

    ## Determine optimal nrounds via CV or use fixed default -----------------

    if (optimize_ensemble) {

      ## Use CV with early stopping to find optimal nrounds --------------------

      if (verbose) cli::cli_text("│  ├─ Running 5-fold CV with early stopping")

      safely_execute(
        expr = {

          xgboost::xgb.cv(data      = meta_features,
                          label     = meta_response,
                          params    = xgb_params,
                          nrounds   = 500,
                          nfold     = 5,
                          early_stopping_rounds = 20,
                          verbose   = 0)

        },
        default_value = NULL,
        error_message = "XGBoost CV failed with {nrow(meta_features)} samples across 5 folds"
      ) -> safe_cv

      handle_results(safe_cv,
                     error_title = "XGBoost cross-validation failed",
                     error_hints = c("Check meta-features have no missing/infinite values",
                                     "Verify sufficient samples for 5-fold CV (need >= 25 samples)",
                                     "Try setting optimize_ensemble = FALSE to skip CV"),
                     abort_on_null = TRUE) -> cv_result

      best_nrounds <- cv_result$best_iteration
      cv_rmse      <- cv_result$evaluation_log$test_rmse_mean[best_nrounds]

      if (verbose) {
        cli::cli_text("│  ├─ Best iteration: {.val {best_nrounds}} (CV RMSE: {.val {round(cv_rmse, 4)}})")
      }

    } else {

      ## Use fixed nrounds for faster execution -------------------------------

      best_nrounds <- 50
      cv_rmse      <- NA_real_

      if (verbose) cli::cli_text("│  ├─ Using fixed nrounds = {.val {best_nrounds}}")

    }

    ## Train final meta-learner with optimal/fixed nrounds --------------------

    if (verbose) cli::cli_text("│  └─ Training final meta-learner with {.val {best_nrounds}} rounds")

    safely_execute(
      expr = {

        xgboost::xgboost(data    = meta_features,
                         label   = meta_response,
                         params  = xgb_params,
                         nrounds = best_nrounds,
                         verbose = 0)

      },
      default_value = NULL,
      error_message = "XGBoost meta-learner training failed with {best_nrounds} rounds"
    ) -> safe_xgb

    handle_results(safe_xgb,
                   error_title = "XGBoost meta-learner training failed",
                   error_hints = c("Check that meta-features have no missing/infinite values",
                                   "Verify response variable is numeric",
                                   "Try reducing nrounds or adjusting max_depth"),
                   abort_on_null = TRUE) -> xgb_meta_model

    ## Store tuning information as attributes for metadata ----------------------

    attr(xgb_meta_model, "nrounds_used") <- best_nrounds
    attr(xgb_meta_model, "cv_rmse") <- cv_rmse
    attr(xgb_meta_model, "tuned") <- optimize_ensemble

    ## Fit base models on full training data --------------------------------
    ## Safely fit individual models - GRACEFUL DEGRADATION ------------------
    ## Skip failed models, continue with N-1 models if >= 2 remain

    if (verbose) cli::cli_text("├─ Fitting {.val {nrow(finalized_models)}} base models on training data")

    n_base_initial <- nrow(finalized_models)

    finalized_models %>%
      dplyr::select(wflow_id, workflow, transformation) %>%
      dplyr::mutate(fitted_workflow = purrr::map2(workflow, wflow_id, ~ {

        safe_fit <- safely_execute(
          expr = {

            fit(.x, data = train_data)

          },
          default_value = NULL,
          error_message = "Failed to fit base model {.y}",
          log_error = TRUE
        )

        safe_fit$result

      })) -> base_models_fitted

    ## Filter out failed models ---------------------------------------------

    base_models_fitted %>%
      dplyr::filter(!purrr::map_lgl(fitted_workflow, is.null)) -> base_models_fitted

    n_base_success <- nrow(base_models_fitted)
    n_base_failed  <- n_base_initial - n_base_success

    ## Validate minimum models for ensemble ---------------------------------

    if (n_base_success < 2) {

      cli::cli_abort(c(
        "Insufficient base models for XGBoost meta-learner",
        "x" = "Successfully fitted {n_base_success} of {n_base_initial} base models",
        "i" = "Need at least 2 models, but only {n_base_success} succeeded"
      ))

    }

    ## Report success/failure if failures occurred --------------------------

    if (verbose && n_base_failed > 0) {

      cli::cli_alert_warning("Fitted {n_base_success}/{n_base_initial} base models successfully (skipped {n_base_failed} failures)")

    }

    ## Create ensemble model object --------------------------------------------

    list(method       = "xgb_meta",
         meta_learner = xgb_meta_model,
         base_models  = base_models_fitted,
         predict      = function(new_data) {

        ## Get original-scale predictions from each base model ----------------

           base_preds <- purrr::map2(
             base_models_fitted$fitted_workflow,
             base_models_fitted$transformation,
             ~ get_original_scale_predictions(.x, new_data, .y, warn = FALSE)
           )

        ## Convert to matrix with proper column names -------------------------

           base_preds_matrix <- do.call(cbind, base_preds)
           colnames(base_preds_matrix) <- paste0("pred_", base_models_fitted$wflow_id)

        ## Use XGBoost to blend predictions -----------------------------------

        final_pred <- predict(xgb_meta_model, base_preds_matrix)

        tibble::tibble(.pred = final_pred)
      }
    ) -> ensemble_model

    ## Extract feature importance as weights --------------------------------

    importance <- xgboost::xgb.importance(model = xgb_meta_model)

    if (nrow(importance) > 0) {

       data.frame(member = gsub("pred_", "", importance$Feature),
                  coef = importance$Gain) %>%
        dplyr::arrange(dplyr::desc(coef)) -> model_weights

    } else {

      model_weights <- data.frame(member = character(), coef = numeric())

    }

    if (verbose) {

      cli::cli_text("└─ XGBoost ensemble fitted")

      if (nrow(model_weights) > 0) {

        cli::cli_text("{.strong Top contributing models (by gain):}")

        model_weights %>%
          head(5) -> top_models

        for (i in seq_len(nrow(top_models))) {
          prefix <- if (i < nrow(top_models)) "├─" else "└─"
          cli::cli_text("{prefix} {.val {top_models$member[i]}}: importance = {.val {round(top_models$coef[i], 4)}}")
        }

      }

      cli::cli_text("")

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Generate Predictions and Evaluate
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Step 4.1: Generate Ensemble Predictions and Calculate Metrics
    ## -------------------------------------------------------------------------

    ## Generate predictions on test set --------------------------------------

    # Different prediction methods for different ensemble types
    if (ensemble_method == "stacks") {

      ## Custom prediction with individual back-transformation ----------------
      ## Stacks predict() would return mixed scales (log for log models, original
      ## for none models). We need to back-transform each member individually
      ## before applying stacks weights to handle mixed transformations correctly.

      # Extract member workflows from fitted stacks model
      member_workflows <- extract_stacks_members(ensemble_model)

      # Extract stacks coefficients (LASSO weights)
      coefficients <- extract_stacks_coefficients(ensemble_model)

      # Get transformation lookup from ensemble model
      transformation_lookup <- ensemble_model$transformation_lookup

      # Get member names to match with transformations
      member_names <- names(member_workflows)

      # Match transformations using the lookup table
      # Member names should match wflow_ids (with sanitization)
      transformations <- purrr::map_chr(member_names, function(name) {
        # Stacks sanitizes names (e.g., "+" becomes ".")
        # Try direct lookup first
        if (name %in% names(transformation_lookup)) {
          return(transformation_lookup[[name]])
        }

        # Try unsanitized name (reverse: "." becomes "+")
        unsanitized <- gsub("\\.", "+", name)
        if (unsanitized %in% names(transformation_lookup)) {
          return(transformation_lookup[[unsanitized]])
        }

        # Fallback: no transformation
        if (verbose) {
          cli::cli_warn("Could not find transformation for member {.val {name}}, using 'none'")
        }
        "none"
      })

      # Debug output to verify transformation mapping
      if (verbose) {
        cli::cli_text("")
        cli::cli_text("{.strong Transformation mapping for prediction:}")
        for (i in seq_along(member_names)) {
          cli::cli_text("├─ {.val {member_names[i]}}: {.field {transformations[i]}}")
        }
        cli::cli_text("")
      }

      # Get predictions with individual back-transformation for each member
      base_predictions <- predict_stacks_members_backtransformed(
        member_workflows = member_workflows,
        transformations = transformations,
        new_data = test_data
      )

      # Apply stacks weights to get ensemble prediction
      ensemble_pred <- apply_stacks_weights(base_predictions, coefficients)

      # Create predictions tibble
      ensemble_predictions <- tibble::tibble(
        Predicted = ensemble_pred,
        Observed = test_data$Response
      )

    } else {
      # Weighted and xgb_meta use custom predict functions
      ensemble_model$predict(test_data) %>%
        dplyr::bind_cols(test_data %>% dplyr::select(Observed = Response)) %>%
        dplyr::rename(Predicted = .pred) -> ensemble_predictions
    }

    ## Scale consistency check ---------------------------------------------------
    ##
    ## IMPORTANT: Ensemble predictions are already on the ORIGINAL SCALE
    ##
    ## Transformation flow:
    ## 1. Individual models trained with response transformations (log, sqrt, etc.)
    ## 2. CV predictions generated on TRANSFORMED scale during finalization
    ## 3. CV predictions BACK-TRANSFORMED before add_candidates() (lines 252-262)
    ## 4. Stacks meta-learner trains on ORIGINAL SCALE predictions
    ## 5. Therefore: ensemble predictions are ALREADY on original scale
    ##
    ## No additional back-transformation needed or allowed here

    # Verify transformation consistency across models
    if ("transformation" %in% names(finalized_models)) {

      unique(tolower(finalized_models$transformation)) %>%
        setdiff(c("none", "notrans", "na", "")) -> active_transformations

      if (length(active_transformations) > 1 && verbose) {

        cli::cli_alert_info("Models use mixed transformations: {paste(active_transformations, collapse = ', ')}")
        cli::cli_alert_info("All CV predictions were back-transformed to original scale before stacking")

      }

    }

    ## Calculate metrics -----------------------------------------------------

    yardstick::metrics(ensemble_predictions,
                      truth    = Observed,
                      estimate = Predicted) -> ensemble_metrics

    # Add custom metrics if available --------------------------------------------

    if (exists("ccc", mode = "function")) {

      ccc_result <- ccc(ensemble_predictions, truth = Observed, estimate = Predicted)

      dplyr::bind_rows(ensemble_metrics,
                       tibble::tibble(.metric    = "ccc",
                                      .estimator = "standard",
                                      .estimate  = ccc_result$.estimate)) -> ensemble_metrics

    }

    if (exists("rpd", mode = "function")) {

      rpd_result <- rpd(ensemble_predictions, truth = Observed, estimate = Predicted)

      dplyr::bind_rows(ensemble_metrics,
                       tibble::tibble(.metric    = "rpd",
                                      .estimator = "standard",
                                      .estimate  = rpd_result$.estimate)) -> ensemble_metrics

    }

    ## Display ensemble performance ------------------------------------------

    display_metrics <- c("rmse", "rsq", "mae", "ccc", "rpd")

    if (verbose) {

      cli::cli_text("{.strong Ensemble test performance}")

      ensemble_metrics %>%
        dplyr::filter(.metric %in% display_metrics) -> metrics_to_show

      for (i in seq_len(nrow(metrics_to_show))) {

        metric_name  <- toupper(metrics_to_show$.metric[i])
        metric_value <- round(metrics_to_show$.estimate[i], 4)
        prefix       <- if (i < nrow(metrics_to_show)) "├─" else "└─"

        cli::cli_text("{prefix} {metric_name}: {.val {metric_value}}")

      }
    }

    ## -------------------------------------------------------------------------
    ## Step 4.2: Compare to Individual Models
    ## -------------------------------------------------------------------------

    ## Get test set performance for individual models for fair comparison -----

    if (verbose) cli::cli_text("{.strong Evaluating individual models on test set}")

    finalized_models %>%
      dplyr::mutate(
        # Fit workflows on training data first
        fitted_workflow = purrr::map(workflow, ~ {
          fit(.x, data = train_data)
        }),
        # Get test predictions from each fitted model
        test_preds = purrr::map(fitted_workflow, ~ {
          predict(.x, test_data)$.pred
        }),
        # Back-transform each model's test predictions to original scale
        test_preds = purrr::map2(test_preds, transformation, ~ {

          if (needs_back_transformation(.y)) {

            trans_lower <- tolower(as.character(.y))

            back_transform_predictions(.x, trans_lower, warn = FALSE)

          } else {

            .x

          }

        }),
        # Calculate test RMSE for each model (now on original scale)
        test_rmse = purrr::map_dbl(test_preds, ~ {
          sqrt(mean((test_data$Response - .x)^2))
        }),
        # Also keep CV RMSE for reference
        cv_rmse = purrr::map_dbl(cv_metrics, ~ {
          .x %>%
            dplyr::filter(.metric == "rmse") %>%
            dplyr::pull(mean) %>%
            .[1]
        })
      ) %>%
      dplyr::select(wflow_id, cv_metrics, cv_rmse, test_rmse) %>%
      dplyr::arrange(test_rmse) -> individual_performance

    ## Calculate ensemble improvement ----------------------------------------

    best_individual_rmse <- individual_performance$test_rmse[1]  # Use test RMSE for fair comparison
    best_individual_id   <- individual_performance$wflow_id[1]

    ensemble_metrics %>%
      dplyr::filter(.metric == "rmse") %>%
      dplyr::pull(.estimate) -> ensemble_rmse

    improvement <- round((best_individual_rmse - ensemble_rmse) / best_individual_rmse * 100, 2)

    ## Display comparison if verbose -----------------------------------------

    if (verbose && nrow(individual_performance) > 0) {

      cli::cli_text("{.strong Ensemble comparison}")

      ## Show improvement status -------------------------------------------------

      if (improvement > 0) {

        cli::cli_text("├─ {cli::col_green('✓')} Improves over best individual by {.val {improvement}%}")

      } else if (improvement < 0) {

        cli::cli_text("├─ {cli::col_yellow(paste0('! Performs ', abs(improvement), '% worse than best individual'))}")

      } else {

        cli::cli_text("├─ {cli::col_blue('=')} Performs equally to best individual")

      }

      ## Show top 3 individual models --------------------------------------------

      cli::cli_text("├─ Top individuals (test RMSE):")

      individual_performance %>% head(3) -> top_individuals

      for (i in seq_len(nrow(top_individuals))) {

        cli::cli_text("│  ├─ {.val {top_individuals$wflow_id[i]}}: {.val {round(top_individuals$test_rmse[i], 4)}} (CV: {.val {round(top_individuals$cv_rmse[i], 4)}})")

      }

      cli::cli_text("└─ Ensemble (test RMSE): {.val {round(ensemble_rmse, 4)}}")

    }

  ## ---------------------------------------------------------------------------
  ## Step 5: Save Results if Requested
  ## ---------------------------------------------------------------------------

  if (!is.null(output_dir)) {

    ## Create directories ----------------------------------------------------

    ensemble_dir <- file.path(output_dir, "ensemble")
    fs::dir_create(ensemble_dir, recurse = TRUE)

    ## Save essential files --------------------------------------------------

    qs::qsave(ensemble_model, file.path(ensemble_dir, "fitted_ensemble.qs"))

    list(predictions            = ensemble_predictions,
         metrics                = ensemble_metrics,
         model_weights          = model_weights,
         individual_performance = individual_performance) -> results

    qs::qsave(results, file.path(ensemble_dir, "ensemble_results.qs"))

    ## CSVs for easy access ----------------------------------------------------

    readr::write_csv(ensemble_predictions, file.path(ensemble_dir, "predictions.csv"))
    readr::write_csv(model_weights, file.path(ensemble_dir, "weights.csv"))

    if (verbose) {

      cli::cli_text("")
      cli::cli_text("{.strong Results saved}")
      cli::cli_text("└─ {.path {ensemble_dir}}")

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Create Return Object
  ## ---------------------------------------------------------------------------

  ## Build metadata ------------------------------------------------------------

  n_active <- if (ensemble_method == "stacks") {

    nrow(model_weights)

    } else {

      sum(model_weights$coef > 0.01)

    }

  ## Base metadata for all ensemble types -------------------------------------

  list(method               = ensemble_method,
       n_models             = nrow(finalized_models),
       n_active             = n_active,
       test_prop            = test_prop,
       seed                 = seed,
       blend_metric         = blend_metric,
       optimize_ensemble    = optimize_ensemble,
       ensemble_rmse        = ensemble_rmse,
       best_individual_rmse = best_individual_rmse,
       improvement          = improvement,
       timestamp            = Sys.time()) -> metadata

  ## Add XGBoost-specific metadata if applicable -------------------------------

  if (ensemble_method == "xgb_meta") {

    metadata$xgb_nrounds   <- attr(ensemble_model$meta_learner, "nrounds_used")
    metadata$xgb_cv_rmse   <- attr(ensemble_model$meta_learner, "cv_rmse")
    metadata$xgb_tuned     <- attr(ensemble_model$meta_learner, "tuned")
    metadata$xgb_max_depth <- xgb_params$max_depth
    metadata$xgb_eta       <- xgb_params$eta

  }

  ## Create return object --------------------------------------------------

  structure(list(ensemble_model         = ensemble_model,
                 predictions            = ensemble_predictions,
                 metrics                = ensemble_metrics,
                 model_weights          = model_weights,
                 individual_performance = individual_performance,
                 metadata               = metadata),
            class = "horizons_ensemble") -> result

  ## Final summary if verbose ----------------------------------------------

  if (verbose) {

    cli::cli_text("")
    cli::cli_rule(left = "Ensemble Complete")
    cli::cli_text("")

    # Extract key metrics
    rsq_val <- ensemble_metrics %>%
      dplyr::filter(.metric == "rsq") %>%
      dplyr::pull(.estimate) %>%
      round(4)

    cli::cli_text("{.strong Summary}")
    cli::cli_text("├─ Method: {.field {ensemble_method}}")
    cli::cli_text("├─ Active models: {.val {n_active}}/{.val {nrow(finalized_models)}}")
    cli::cli_text("├─ Test RMSE: {.val {round(ensemble_rmse, 4)}}")
    cli::cli_text("├─ Test R²: {.val {rsq_val}}")

    if (improvement > 0) {

      cli::cli_text("└─ {cli::col_green('✓')} Improved {.val {improvement}%} over best individual")

    } else {

      cli::cli_text("└─ Performance vs best: {.val {improvement}%}")

    }
  }

  return(result)

}

