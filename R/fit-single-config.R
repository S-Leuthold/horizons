#' Fit a Single Model Configuration (Capture Layer)
#'
#' @description
#' Inner-loop function for `fit()`. Takes a single config row from the
#' evaluate() leaderboard, re-tunes it with warm-start Bayesian optimization,
#' generates OOF predictions, fits a final deployable model, evaluates on
#' the held-out test set, and detects performance degradation.
#'
#' This function is the **capture layer** — it runs silently and returns a
#' structured list. All console output (tree rendering, progress messages) is
#' the responsibility of the calling `fit()` function.
#'
#' @param config_row Single-row tibble from `config$configs`.
#' @param split_F An `rsplit` object (Split F: train_F / test_F).
#' @param cv_resamples A `vfold_cv` object created from train_Fit (or train_F).
#' @param calib_data Data frame for UQ calibration (NULL if compute_uq = FALSE).
#' @param train_data Data frame for training. If NULL, defaults to
#'   `training(split_F)`. Pass `train_Fit` when `compute_uq = TRUE` to avoid
#'   calibration leakage.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param best_params_eval Single-row tibble of best params from evaluate().
#' @param final_bayesian_iter Integer. Bayesian iterations for re-tuning.
#' @param grid_size Integer. Grid size for warm-start exploration.
#' @param compute_uq Logical. Whether to train UQ components.
#' @param allow_par Logical. Passed to tune control functions.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return List with fields: config_id, status, degraded, degraded_reason,
#'   fitted_workflow, best_params, cv_predictions, test_metrics, cv_metrics,
#'   uq, warnings, error_message, runtime_secs.
#'
#' @keywords internal
#' @export
fit_single_config <- function(config_row,
                              split_F,
                              cv_resamples,
                              calib_data       = NULL,
                              train_data       = NULL,
                              role_map,
                              best_params_eval,
                              final_bayesian_iter = DEFAULT_FINAL_BAYES_ITER,
                              grid_size           = DEFAULT_GRID_SIZE,
                              compute_uq          = FALSE,
                              allow_par           = FALSE,
                              seed                = 42L) {

  start_time <- Sys.time()
  set.seed(seed)

  config_id      <- config_row$config_id
  outcome_col    <- role_map$variable[role_map$role == "outcome"]
  transformation <- tolower(as.character(config_row$transformation))
  train_data     <- train_data %||% rsample::training(split_F)
  test_data      <- rsample::testing(split_F)

  ## Accumulate warnings from all steps
  collected_warnings <- character(0)

  collect_from <- function(safe_result) {

    if (!is.null(safe_result$warnings)) {

      collected_warnings <<- c(collected_warnings, unlist(safe_result$warnings))

    }

  }

  ## --- Failed result helper ------------------------------------------------

  make_failed <- function(error_msg) {

    list(
      config_id        = config_id,
      status           = "failed",
      degraded         = NA,
      degraded_reason  = NA_character_,
      fitted_workflow  = NULL,
      best_params      = NULL,
      cv_predictions   = NULL,
      test_metrics     = NULL,
      cv_metrics       = NULL,
      uq               = NULL,
      warnings         = if (length(collected_warnings) > 0) collected_warnings else NULL,
      error_message    = error_msg,
      runtime_secs     = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )

  }

  ## -----------------------------------------------------------------------
  ## Step 1: Build recipe
  ## -----------------------------------------------------------------------

  recipe_result <- safely_execute(
    build_recipe(config_row, train_data, role_map),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(recipe_result$error)) {

    return(make_failed(
      paste0("Recipe building failed: ", recipe_result$error$message)
    ))

  }

  recipe <- recipe_result$result
  collect_from(recipe_result)

  ## -----------------------------------------------------------------------
  ## Step 2: Define model specification
  ## -----------------------------------------------------------------------

  model_result <- safely_execute(
    define_model_spec(config_row$model),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(model_result$error)) {

    return(make_failed(
      paste0("Model specification failed: ", model_result$error$message)
    ))

  }

  model_spec <- model_result$result
  collect_from(model_result)

  ## -----------------------------------------------------------------------
  ## Step 3: Create workflow and finalize param_set
  ## -----------------------------------------------------------------------

  wflow_result <- safely_execute(
    workflows::workflow() %>%
      workflows::add_recipe(recipe) %>%
      workflows::add_model(model_spec),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(wflow_result$error)) {

    return(make_failed(
      paste0("Workflow creation failed: ", wflow_result$error$message)
    ))

  }

  wflow <- wflow_result$result

  ## -----------------------------------------------------------------------
  ## Step 4: Finalize parameter set (mtry / num_comp upper bound)
  ## -----------------------------------------------------------------------

  param_set <- workflows::extract_parameter_set_dials(wflow)

  if ("mtry" %in% param_set$name || "num_comp" %in% param_set$name) {

    finalize_result <- safely_execute({

      prepped <- recipes::prep(recipe)
      baked   <- recipes::bake(prepped, new_data = NULL)

      non_pred <- c(outcome_col,
                    role_map$variable[role_map$role %in% c("id", "meta")])
      drop     <- intersect(non_pred, names(baked))
      eval_data <- baked[, setdiff(names(baked), drop), drop = FALSE]

      result <- dials::finalize(param_set, eval_data)

      rm(prepped, baked, eval_data)
      invisible(gc(verbose = FALSE))

      result

    }, log_error = FALSE, capture_conditions = TRUE)

    if (!is.null(finalize_result$error)) {

      return(make_failed(
        paste0("Parameter finalization failed: ", finalize_result$error$message)
      ))

    }

    param_set <- finalize_result$result
    collect_from(finalize_result)

  }

  ## -----------------------------------------------------------------------
  ## Step 5: Define tuning metric set
  ## -----------------------------------------------------------------------

  tune_metrics <- yardstick::metric_set(
    yardstick::rmse,
    yardstick::rsq
  )

  ## -----------------------------------------------------------------------
  ## Step 6: Warm-start Bayesian re-tuning
  ## -----------------------------------------------------------------------

  tune_result <- safely_execute(
    tune_warmstart_bayes(
      workflow      = wflow,
      cv_resamples  = cv_resamples,
      best_params   = best_params_eval,
      param_set     = param_set,
      bayesian_iter = final_bayesian_iter,
      grid_size     = grid_size,
      metric_set    = tune_metrics,
      allow_par     = allow_par
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(tune_result$error)) {

    return(make_failed(
      paste0("Warm-start tuning failed: ", tune_result$error$message)
    ))

  }

  warmstart <- tune_result$result
  collect_from(tune_result)

  ## Check for complete tuning failure (grid failed)
  if (is.null(warmstart$best_params)) {

    return(make_failed(
      paste0("Warm-start tuning failed: ", warmstart$error %||% "unknown error")
    ))

  }

  best_params <- warmstart$best_params

  ## -----------------------------------------------------------------------
  ## Step 7: Finalize workflow
  ## -----------------------------------------------------------------------

  finalized_wf <- tune::finalize_workflow(wflow, best_params)

  ## -----------------------------------------------------------------------
  ## Step 8: Generate OOF predictions via fit_resamples
  ## -----------------------------------------------------------------------

  resample_result <- safely_execute(
    tune::fit_resamples(
      finalized_wf,
      resamples = cv_resamples,
      control   = tune::control_resamples(
        save_pred = TRUE,
        allow_par = allow_par
      )
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(resample_result$error)) {

    return(make_failed(
      paste0("OOF predictions failed: ", resample_result$error$message)
    ))

  }

  cv_fit <- resample_result$result
  collect_from(resample_result)

  ## Extract raw OOF predictions
  oof_raw <- tune::collect_predictions(cv_fit)

  ## Shape cv_predictions: .row, .fold, config_id, .pred, .pred_trans, truth
  cv_predictions <- tibble::tibble(
    .row        = oof_raw$.row,
    .fold       = oof_raw$id,
    config_id   = config_id,
    .pred_trans  = oof_raw$.pred,
    truth       = oof_raw[[outcome_col]]
  )

  ## Back-transform .pred to original scale
  if (needs_back_transformation(transformation)) {

    bt_result <- safely_execute(
      back_transform_predictions(cv_predictions$.pred_trans, transformation,
                                 warn = FALSE),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    if (!is.null(bt_result$error)) {

      return(make_failed(
        paste0("OOF back-transformation failed: ", bt_result$error$message)
      ))

    }

    cv_predictions$.pred <- bt_result$result

    ## Note: truth from collect_predictions() is already on original scale.
    ## The recipe uses step_log/step_sqrt with skip = TRUE, so the outcome
    ## transformation is skipped during assessment baking.

  } else {

    cv_predictions$.pred <- cv_predictions$.pred_trans

  }

  ## Ensure .row is integer
  cv_predictions$.row <- as.integer(cv_predictions$.row)

  ## -----------------------------------------------------------------------
  ## Step 9: Compute fold-wise CV metrics (original scale)
  ## -----------------------------------------------------------------------

  fold_metrics <- cv_predictions %>%
    split(.$`.fold`) %>%
    purrr::map_dfr(
      ~ compute_original_scale_metrics(.x$truth, .x$.pred),
      .id = "fold"
    )

  cv_metrics <- fold_metrics %>%
    dplyr::group_by(.metric) %>%
    dplyr::summarize(
      mean    = mean(.estimate, na.rm = TRUE),
      std_err = stats::sd(.estimate, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  ## -----------------------------------------------------------------------
  ## Step 10: Fit final model on full training data
  ## -----------------------------------------------------------------------

  final_fit_result <- safely_execute(
    workflows::fit(finalized_wf, data = train_data),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(final_fit_result$error)) {

    return(make_failed(
      paste0("Final model fit failed: ", final_fit_result$error$message)
    ))

  }

  final_fit <- final_fit_result$result
  collect_from(final_fit_result)

  ## -----------------------------------------------------------------------
  ## Step 11: Evaluate on test_F (original scale)
  ## -----------------------------------------------------------------------

  test_pred_result <- safely_execute(
    stats::predict(final_fit, new_data = test_data),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(test_pred_result$error)) {

    return(make_failed(
      paste0("Test prediction failed: ", test_pred_result$error$message)
    ))

  }

  test_preds <- test_pred_result$result$.pred

  ## Back-transform if needed
  if (needs_back_transformation(transformation)) {

    bt_test <- safely_execute(
      back_transform_predictions(test_preds, transformation, warn = FALSE),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    if (!is.null(bt_test$error)) {

      return(make_failed(
        paste0("Test back-transformation failed: ", bt_test$error$message)
      ))

    }

    test_preds <- bt_test$result

  }

  test_truth <- test_data[[outcome_col]]

  ## Note: test_truth is already on original scale — test_data is raw
  ## (never passed through the recipe), so no back-transformation needed.

  test_metrics_raw <- safely_execute(
    compute_original_scale_metrics(test_truth, test_preds) %>%
      tidyr::pivot_wider(names_from = .metric, values_from = .estimate),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(test_metrics_raw$error) ||
      is.null(test_metrics_raw$result) ||
      nrow(test_metrics_raw$result) == 0) {

    test_metrics <- tibble::tibble(
      rmse = NA_real_, rrmse = NA_real_, rsq = NA_real_,
      ccc  = NA_real_, rpd   = NA_real_, mae = NA_real_
    )

  } else {

    test_metrics <- test_metrics_raw$result

    required <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")
    missing  <- setdiff(required, names(test_metrics))
    for (m in missing) test_metrics[[m]] <- NA_real_

  }

  ## -----------------------------------------------------------------------
  ## Step 12: Degradation detection
  ## -----------------------------------------------------------------------
  ## Compare test_F RPD to cv_mean_rpd - 2 * cv_se_rpd.
  ## If test is below that threshold, the model has degraded.

  degraded        <- FALSE
  degraded_reason <- NA_character_

  rpd_cv <- cv_metrics %>% dplyr::filter(.metric == "rpd")

  if (nrow(rpd_cv) == 1 && is.finite(rpd_cv$mean) &&
      is.finite(rpd_cv$std_err) && is.finite(test_metrics$rpd)) {

    rpd_threshold <- rpd_cv$mean - 2 * rpd_cv$std_err

    if (test_metrics$rpd < rpd_threshold) {

      degraded <- TRUE
      degraded_reason <- sprintf(
        "test_rpd (%.3f) below cv_mean - 2*cv_se (%.3f - 2*%.3f = %.3f)",
        test_metrics$rpd, rpd_cv$mean, rpd_cv$std_err, rpd_threshold
      )

    }

  }

  ## -----------------------------------------------------------------------
  ## Step 13: UQ (must run BEFORE butchering — needs extract_mold())
  ## -----------------------------------------------------------------------

  uq_result <- NULL

  if (compute_uq && !is.null(calib_data)) {

    uq_safe <- safely_execute(
      fit_uq(
        fitted_workflow = final_fit,
        oof_predictions = cv_predictions,
        calib_data      = calib_data,
        role_map        = role_map,
        transformation  = transformation,
        level_default   = DEFAULT_UQ_LEVEL
      ),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    if (is.null(uq_safe$error)) {

      uq_result <- uq_safe$result

    }

    collect_from(uq_safe)

  }

  ## -----------------------------------------------------------------------
  ## Step 14: Butcher the final fit for storage
  ## -----------------------------------------------------------------------

  butchered <- safely_execute(
    butcher::butcher(final_fit),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  fitted_workflow <- if (is.null(butchered$error)) {

    result <- butchered$result
    rm(final_fit, butchered)
    invisible(gc(verbose = FALSE))
    result

  } else {

    final_fit  ## fall back to unbutchered if butcher fails

  }

  ## -----------------------------------------------------------------------
  ## Step 15: Return structured result
  ## -----------------------------------------------------------------------

  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  list(
    config_id        = config_id,
    status           = "success",
    degraded         = degraded,
    degraded_reason  = degraded_reason,
    fitted_workflow  = fitted_workflow,
    best_params      = best_params,
    cv_predictions   = cv_predictions,
    test_metrics     = test_metrics,
    cv_metrics       = cv_metrics,
    uq               = uq_result,
    warnings         = if (length(collected_warnings) > 0) collected_warnings else NULL,
    error_message    = NA_character_,
    runtime_secs     = runtime
  )

}
