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
#' @param best_params_eval Single-row tibble of best params from evaluate(),
#'   or NULL (`fit()`'s cold start), in which case the re-tune starts from a
#'   space-filling grid.
#' @param final_bayesian_iter Integer. Bayesian iterations for re-tuning.
#' @param grid_size Integer. Grid size for warm-start exploration.
#' @param compute_uq Logical. Whether to train UQ components.
#' @param allow_par Logical. Passed to tune control functions.
#' @param seed Integer. Random seed for reproducibility.
#' @param sg_window Odd integer. Savitzky-Golay window in grid points, passed
#'   to [build_recipe()]. `fit()` reads it from `configure()`'s
#'   `config$recipe`, the value `evaluate()` ran with. Default 9.
#' @param pca_threshold Numeric. Variance share the `pca` feature selection
#'   keeps, passed to [build_recipe()]. Read the same way. Default 0.995.
#' @param outcome_range Numeric length-2 vector. The outcome's physical range,
#'   which the back-transformed OOF, test-set and UQ-calibration predictions
#'   (and, for a transformed response, the tuning predictions) are clamped
#'   to. `fit()` reads it from `configure()`'s `config$outcome_range`.
#'   Default `c(0, Inf)`.
#' @param response_fences Numeric `c(lower = , upper = )` or `NULL`. The
#'   training-partition fences `evaluate()` trimmed response outliers by
#'   (#77), passed by `fit()` when that trim removed rows. The CV folds then
#'   run on rows inside the fences while the test rows are untrimmed, so the
#'   degradation check takes its test RPD over the test rows inside the
#'   fences; the reported test metrics stay untrimmed. Default `NULL` (no
#'   trim: the check reads the test RPD).
#'
#' @return List with fields: config_id, status, degraded, degraded_reason,
#'   fitted_workflow, best_params, warm_start, start_grid_size,
#'   cv_predictions, test_metrics, cv_metrics, uq, warnings, error_message,
#'   runtime_secs. `warm_start` is `TRUE` when the re-tune started from
#'   `best_params_eval`, `FALSE` when it fell back to a space-filling grid of
#'   `start_grid_size` points, and `NA` (both) when the config failed
#'   before tuning.
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
                              compute_ad          = FALSE,
                              allow_par           = FALSE,
                              seed                = 42L,
                              sg_window           = DEFAULT_SG_WINDOW,
                              pca_threshold       = DEFAULT_PCA_THRESHOLD,
                              outcome_range       = DEFAULT_OUTCOME_RANGE,
                              response_fences     = NULL) {

  start_time <- Sys.time()

  ## Pin the RNG kind as well as the seed, for the same reason as
  ## evaluate_single_config(): a worker started under
  ## furrr_options(seed = TRUE) is on L'Ecuyer-CMRG, and set.seed() alone
  ## would not reset it.
  set.seed(seed, kind = "Mersenne-Twister")

  config_id      <- config_row$config_id
  outcome_col    <- role_map$variable[role_map$role == "outcome"]
  transformation <- tolower(as.character(config_row$transformation))
  train_data     <- train_data %||% rsample::training(split_F)
  test_data      <- rsample::testing(split_F)

  ## Accumulate warnings from all steps, as records that render_warning_log()
  ## reduces to one line per distinct message (#96)
  warning_log <- new_warning_log()

  collect_from <- function(safe_result) {

    warning_log <<- dplyr::bind_rows(warning_log, text_records(safe_result$warnings))

  }

  ## Warnings tune caught inside its resampling and kept in .notes, which
  ## safely_execute() never sees. Both the re-tune and the out-of-fold
  ## predictions resample cv_resamples, so their fold counts merge.
  collect_notes_from <- function(tune_results) {

    notes <- tune_note_records(tune_results, type = "warning", scope = "cv")
    warning_log <<- dplyr::bind_rows(warning_log, notes)

  }

  ## How the re-tune started, once it has: from evaluate()'s parameters, or
  ## from a space-filling grid when there were none to use (#45)
  warm_start        <- NA
  start_grid_size   <- NA_integer_

  ## --- Failed result helper ------------------------------------------------

  make_failed <- function(error_msg) {

    list(
      config_id        = config_id,
      status           = "failed",
      degraded         = NA,
      degraded_reason  = NA_character_,
      fitted_workflow  = NULL,
      best_params      = NULL,
      warm_start       = warm_start,
      start_grid_size  = start_grid_size,
      cv_predictions   = NULL,
      test_metrics     = NULL,
      cv_metrics       = NULL,
      uq               = NULL,
      ad               = NULL,
      warnings         = render_warning_log(warning_log),
      error_message    = error_msg,
      runtime_secs     = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )

  }

  ## -----------------------------------------------------------------------
  ## Step 1: Build recipe
  ## -----------------------------------------------------------------------

  recipe_result <- safely_execute(
    build_recipe(config_row, train_data, role_map,
                 sg_window     = sg_window,
                 pca_threshold = pca_threshold),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(recipe_result$error)) {

    return(make_failed(
      paste0("Recipe building failed: ", condition_summary(recipe_result$error))
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
      paste0("Model specification failed: ", condition_summary(model_result$error))
    ))

  }

  model_spec <- model_result$result
  collect_from(model_result)

  ## -----------------------------------------------------------------------
  ## Step 3: Create workflow and finalize param_set
  ## -----------------------------------------------------------------------

  wflow_result <- safely_execute(
    workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(model_spec),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(wflow_result$error)) {

    return(make_failed(
      paste0("Workflow creation failed: ", condition_summary(wflow_result$error))
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

      eval_data <- baked[, prepped_predictors(prepped, baked), drop = FALSE]

      result <- dials::finalize(param_set, eval_data)

      rm(prepped, baked, eval_data)
      invisible(gc(verbose = FALSE))

      result

    }, log_error = FALSE, capture_conditions = TRUE)

    if (!is.null(finalize_result$error)) {

      return(make_failed(
        paste0("Parameter finalization failed: ", condition_summary(finalize_result$error))
      ))

    }

    param_set <- finalize_result$result
    collect_from(finalize_result)

  }

  ## -----------------------------------------------------------------------
  ## Step 5: Define tuning metric set
  ## -----------------------------------------------------------------------

  ## Scored on the original response scale, for the same reason as in
  ## evaluate_single_config(): the skip = TRUE transform never reaches tune's
  ## assessment set. tune_warmstart_bayes() selects on "rmse" by name, which
  ## the factory preserves. See #49. Clamped to the outcome's range (#76).
  tune_metrics <- tuning_metric_set(transformation, metrics = c("rmse", "rsq"),
                                    outcome_range = outcome_range)

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
      paste0("Warm-start tuning failed: ", condition_summary(tune_result$error))
    ))

  }

  warmstart <- tune_result$result
  collect_from(tune_result)
  collect_notes_from(warmstart$tune_results)

  warm_start        <- !isTRUE(warmstart$fallback_used)
  start_grid_size   <- as.integer(warmstart$grid_points %||% NA_integer_)

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

  ## Re-pin before every stochastic stage so results do not depend on which
  ## axis the previous stage ran on (see evaluate_single_config(), Step 8).
  set.seed(seed, kind = "Mersenne-Twister")

  resample_result <- safely_execute(
    tune::fit_resamples(
      finalized_wf,
      resamples = cv_resamples,
      control   = tune::control_resamples(
        save_pred     = TRUE,
        allow_par     = allow_par,
        parallel_over = "resamples"
      )
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(resample_result$error)) {

    return(make_failed(
      paste0("OOF predictions failed: ", condition_summary(resample_result$error))
    ))

  }

  cv_fit <- resample_result$result
  collect_from(resample_result)
  collect_notes_from(cv_fit)

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

  ## Back-transform .pred to original scale. Unconditional, like predict():
  ## "none" is a passthrough, and the clamp to the outcome's range inside
  ## back_transform_predictions() must reach these OOF predictions because
  ## they are the meta-learner's training features, and predict() clamps the
  ## same members at serve time (#53, #76).
  bt_result <- safely_execute(
    back_transform_predictions(cv_predictions$.pred_trans, transformation,
                               warn = FALSE, outcome_range = outcome_range),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(bt_result$error)) {

    return(make_failed(
      paste0("OOF back-transformation failed: ", condition_summary(bt_result$error))
    ))

  }

  cv_predictions$.pred <- bt_result$result

  ## Note: truth from collect_predictions() is already on original scale.
  ## The recipe uses step_log/step_sqrt with skip = TRUE, so the outcome
  ## transformation is skipped during assessment baking.

  ## Ensure .row is integer
  cv_predictions$.row <- as.integer(cv_predictions$.row)

  ## -----------------------------------------------------------------------
  ## Step 9: Compute fold-wise CV metrics (original scale)
  ## -----------------------------------------------------------------------

  fold_metrics <- split(cv_predictions, cv_predictions$`.fold`) |>
    purrr::map_dfr(
      ~ compute_original_scale_metrics(.x$truth, .x$.pred),
      .id = "fold"
    )

  cv_metrics <- fold_metrics |>
    dplyr::group_by(.metric) |>
    dplyr::summarize(
      mean    = mean(.estimate, na.rm = TRUE),
      std_err = stats::sd(.estimate, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  ## -----------------------------------------------------------------------
  ## Step 10: Fit final model on full training data
  ## -----------------------------------------------------------------------

  set.seed(seed, kind = "Mersenne-Twister")

  final_fit_result <- safely_execute(
    workflows::fit(finalized_wf, data = train_data),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(final_fit_result$error)) {

    return(make_failed(
      paste0("Final model fit failed: ", condition_summary(final_fit_result$error))
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
      paste0("Test prediction failed: ", condition_summary(test_pred_result$error))
    ))

  }

  test_preds <- test_pred_result$result$.pred

  ## Back-transform, unconditionally (see the OOF block above, #53 and #76)
  bt_test <- safely_execute(
    back_transform_predictions(test_preds, transformation, warn = FALSE,
                               outcome_range = outcome_range),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(bt_test$error)) {

    return(make_failed(
      paste0("Test back-transformation failed: ", condition_summary(bt_test$error))
    ))

  }

  test_preds <- bt_test$result

  test_truth <- test_data[[outcome_col]]

  ## Note: test_truth is already on original scale — test_data is raw
  ## (never passed through the recipe), so no back-transformation needed.

  test_metrics_raw <- safely_execute(
    compute_original_scale_metrics(test_truth, test_preds) |>
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
  ##
  ## After a response trim (#77) the folds ran on rows inside the training
  ## fences while the test rows are untrimmed, so the two RPDs would describe
  ## two populations and the flag would fire whenever a trim removed an
  ## extreme. The diagnostic RPD is then taken over the test rows inside the
  ## same fences; test_metrics, which is what is reported, stays untrimmed.

  degradation     <- check_degradation(cv_metrics, test_metrics$rpd, test_truth,
                                       test_preds, response_fences)
  degraded        <- degradation$degraded
  degraded_reason <- degradation$reason

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
        level_default   = DEFAULT_UQ_LEVEL,
        seed            = seed,
        outcome_range   = outcome_range
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
  ## Step 13b: AD (must also run BEFORE butchering — needs extract_mold())
  ## -----------------------------------------------------------------------
  ## Shares the UQ calibration split (D7). Independent of UQ: fit_ad() returns
  ## NULL on any failure, so AD degrades without touching the config's fit.

  ad_result <- NULL

  if (compute_ad && !is.null(calib_data)) {

    ad_safe <- safely_execute(
      fit_ad(
        fitted_workflow = final_fit,
        calib_data      = calib_data,
        level           = DEFAULT_AD_LEVEL
      ),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    if (is.null(ad_safe$error)) {

      ad_result <- ad_safe$result

    }

    collect_from(ad_safe)

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
    warm_start       = warm_start,
    start_grid_size  = start_grid_size,
    cv_predictions   = cv_predictions,
    test_metrics     = test_metrics,
    cv_metrics       = cv_metrics,
    uq               = uq_result,
    ad               = ad_result,
    warnings         = render_warning_log(warning_log),
    error_message    = NA_character_,
    runtime_secs     = runtime
  )

}


## ---------------------------------------------------------------------------
## check_degradation(): test RPD against the CV band
## ---------------------------------------------------------------------------

#' Flag a fitted configuration whose test RPD falls below its CV band
#'
#' @description
#' Degraded means the test RPD is below the cross-validated mean minus two
#' standard errors. After a response trim (#77) the folds ran on rows inside
#' the training fences while the test rows are untrimmed, so the two would
#' describe two populations and the flag would fire whenever the trim removed
#' an extreme. With `response_fences`, the RPD compared is therefore the one
#' over the test rows inside the fences, and the reason says so; the test
#' metrics `fit()` reports are untouched. Fewer than two test rows inside the
#' fences leave nothing to compare, and nothing is flagged.
#'
#' @param cv_metrics Tibble with `.metric`, `mean` and `std_err`.
#' @param test_rpd Numeric(1). The RPD over every test row.
#' @param test_truth,test_preds Numeric. Test outcomes and back-transformed
#'   predictions, original scale.
#' @param response_fences Numeric `c(lower = , upper = )`, or `NULL` when no
#'   rows were trimmed.
#' @return List with `degraded` (logical) and `reason` (character, `NA` when
#'   not degraded).
#' @keywords internal
#' @noRd
check_degradation <- function(cv_metrics, test_rpd, test_truth, test_preds,
                              response_fences = NULL) {

  scope <- ""

  if (!is.null(response_fences)) {

    inside <- !is.na(test_truth) &
      test_truth >= response_fences[["lower"]] &
      test_truth <= response_fences[["upper"]]

    test_rpd <- if (sum(inside) >= 2) {
      rpd_vec(test_truth[inside], test_preds[inside])
    } else {
      NA_real_
    }

    scope <- sprintf(" within the training fences [%.4g, %.4g] (%d of %d test rows)",
                     response_fences[["lower"]], response_fences[["upper"]],
                     sum(inside), length(test_truth))

  }

  rpd_cv <- cv_metrics[cv_metrics$.metric == "rpd", , drop = FALSE]

  if (nrow(rpd_cv) != 1 || !is.finite(rpd_cv$mean) ||
      !is.finite(rpd_cv$std_err) || !is.finite(test_rpd)) {

    return(list(degraded = FALSE, reason = NA_character_))

  }

  rpd_threshold <- rpd_cv$mean - 2 * rpd_cv$std_err

  if (test_rpd >= rpd_threshold) {

    return(list(degraded = FALSE, reason = NA_character_))

  }

  list(
    degraded = TRUE,
    reason   = sprintf(
      "test_rpd%s (%.3f) below cv_mean - 2*cv_se (%.3f - 2*%.3f = %.3f)",
      scope, test_rpd, rpd_cv$mean, rpd_cv$std_err, rpd_threshold
    )
  )

}
