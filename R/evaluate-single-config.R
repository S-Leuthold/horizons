#' Evaluate a Single Model Configuration
#'
#' @description
#' Inner-loop function for `evaluate()`. Takes a single config row, builds a
#' recipe + model workflow, tunes hyperparameters via grid search (optionally
#' followed by Bayesian optimization), evaluates on the held-out test set, and
#' returns a single-row result tibble.
#'
#' This function is the **capture layer** — it runs silently and returns
#' structured data. All console output (tree rendering, progress messages) is
#' the responsibility of the calling `evaluate()` function.
#'
#' @param config_row Single-row tibble from `config$configs`. Must contain:
#'   `config_id`, `model`, `transformation`, `preprocessing`,
#'   `feature_selection`, `covariates`.
#' @param split An `rsplit` object from `rsample::initial_split()`.
#' @param cv_folds A `vfold_cv` object created from the training set.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param grid_size Integer. Number of grid points for `tune_grid()`.
#' @param bayesian_iter Integer. Iterations for `tune_bayes()`. Set to 0 to
#'   skip Bayesian optimization entirely.
#' @param prune Logical. If TRUE, record whether the best grid-search RPD
#'   falls below `prune_threshold` (`below_prune_threshold`), and when it
#'   does, skip Bayesian optimization and label the config `"pruned"`. With
#'   `bayesian_iter = 0` there is nothing to skip, so the config is labelled
#'   `"success"` and only `below_prune_threshold` carries the reading.
#' @param prune_threshold Numeric. RPD threshold for pruning, on the original
#'   response scale (tuning metrics are scored there via
#'   `tuning_metric_set()`). Only used when `prune = TRUE`. Default 1.0, as
#'   in `evaluate()`.
#' @param allow_par Logical. Passed to `tune::control_grid()` and
#'   `tune::control_bayes()` to enable parallel CV folds on the registered
#'   `future::plan()`. `evaluate()` sets this from the resolved axis: `FALSE`
#'   on the configs axis (tune ships nothing inward), `TRUE` on the
#'   resamples axis.
#' @param parallel_over Character. tune's `parallel_over`, passed through to
#'   the control objects. `"resamples"` (default) ships one rsplit per task;
#'   `"everything"` ships the whole rset to every task. tune may rewrite the
#'   value before dispatch (parameter-free configs become `"resamples"`;
#'   single-split rsets become `"everything"`).
#' @param seed Integer. Random seed for reproducibility.
#' @param sg_window Odd integer. Savitzky-Golay window in grid points, passed
#'   to [build_recipe()]. `evaluate()` reads it from `configure()`'s
#'   `config$recipe`. Default 9.
#' @param pca_threshold Numeric. Variance share the `pca` feature selection
#'   keeps, passed to [build_recipe()]. `evaluate()` reads it from
#'   `configure()`'s `config$recipe`. Default 0.995.
#'
#' @return Single-row tibble with columns: `config_id`, `status` (`"pruned"`
#'   means Bayesian refinement was skipped), `below_prune_threshold` (logical;
#'   `NA` when `prune = FALSE`), `prune_threshold` (the threshold that reading
#'   was taken against; `NA` when `prune = FALSE`), the six
#'   test-set metrics `rmse`, `rrmse`, `rsq`, `ccc`, `rpd`, `mae`, the six
#'   cross-validated means at the selected hyperparameters `cv_rmse`,
#'   `cv_rrmse`, `cv_rsq`, `cv_ccc`, `cv_rpd`, `cv_mae` (what `evaluate()` and
#'   `fit()` rank on), `best_params` (list-column), `error_message`,
#'   `warnings`, `runtime_secs`.
#'
#' @keywords internal
#' @export
evaluate_single_config <- function(config_row,
                                   split,
                                   cv_folds,
                                   role_map,
                                   grid_size       = DEFAULT_GRID_SIZE,
                                   bayesian_iter   = DEFAULT_BAYES_ITER,
                                   prune           = FALSE,
                                   prune_threshold = 1.0,
                                   allow_par       = FALSE,
                                   parallel_over   = "resamples",
                                   seed            = 42L,
                                   sg_window       = DEFAULT_SG_WINDOW,
                                   pca_threshold   = DEFAULT_PCA_THRESHOLD) {

  start_time <- Sys.time()

  parallel_over <- rlang::arg_match0(parallel_over, c("resamples", "everything"),
                                     arg_nm = "parallel_over")

  ## The RNG kind is pinned, not just the seed. furrr_options(seed = TRUE)
  ## switches a worker to L'Ecuyer-CMRG, and set.seed() with kind = NULL leaves
  ## that in place — so the same seed produced different streams in the parent
  ## (Mersenne-Twister) and in a worker, and sequential and parallel runs were
  ## not numerically equal. Naming the kind makes them agree.
  set.seed(seed, kind = "Mersenne-Twister")

  config_id      <- config_row$config_id
  outcome_col    <- role_map$variable[role_map$role == "outcome"]
  transformation <- tolower(as.character(config_row$transformation))
  train_data     <- rsample::training(split)

  ## Accumulate warnings from all steps for tree rendering
  collected_warnings <- character(0)

  collect_from <- function(safe_result) {
    if (!is.null(safe_result$warnings)) {
      collected_warnings <<- c(collected_warnings, unlist(safe_result$warnings))
    }
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

    return(create_failed_result(config_id,
      paste0("Recipe building failed: ", recipe_result$error$message)))

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

    return(create_failed_result(config_id,
      paste0("Model specification failed: ", model_result$error$message)))

  }

  model_spec <- model_result$result
  collect_from(model_result)

  ## -----------------------------------------------------------------------
  ## Step 3: Create workflow
  ## -----------------------------------------------------------------------

  wflow_result <- safely_execute(
    workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(model_spec),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(wflow_result$error)) {

    return(create_failed_result(config_id,
      paste0("Workflow creation failed: ", wflow_result$error$message)))

  }

  wflow <- wflow_result$result

  ## -----------------------------------------------------------------------
  ## Step 4: Finalize parameter set (mtry upper bound)
  ## -----------------------------------------------------------------------
  ## Models with mtry (rf, xgboost, lightgbm) need the upper bound set from
  ## the actual predictor count after recipe preprocessing. Without this,
  ## tune_grid() may sample mtry values larger than the number of predictors.

  param_set <- workflows::extract_parameter_set_dials(wflow)

  if ("mtry" %in% param_set$name) {

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

      return(create_failed_result(config_id,
        paste0("Parameter finalization failed: ", finalize_result$error$message)))

    }

    param_set <- finalize_result$result
    collect_from(finalize_result)

  }

  ## -----------------------------------------------------------------------
  ## Step 5: Define tuning metric set
  ## -----------------------------------------------------------------------

  ## Scored on the original response scale. The response transform is a
  ## skip = TRUE recipe step, so tune never applies it to the assessment set;
  ## tuning_metric_set() back-transforms the estimate inside each metric so
  ## select_best(), tune_bayes() and the prune gate all see the same scale the
  ## leaderboard reports. rmse stays first: tune_bayes() optimises the first
  ## metric in the set. See #49.
  tune_metrics <- tuning_metric_set(transformation)

  ## -----------------------------------------------------------------------
  ## Step 6: Grid search
  ## -----------------------------------------------------------------------

  grid_result <- safely_execute(
    suppressMessages(
      tune::tune_grid(
        object     = wflow,
        resamples  = cv_folds,
        grid       = grid_size,
        metrics    = tune_metrics,
        param_info = param_set,
        control    = tune::control_grid(
          save_pred     = FALSE,
          save_workflow = FALSE,
          verbose       = FALSE,
          allow_par     = allow_par,
          parallel_over = parallel_over
        )
      )
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(grid_result$error)) {

    return(create_failed_result(config_id,
      paste0("Grid search failed: ", grid_result$error$message)))

  }

  grid_results <- grid_result$result
  collect_from(grid_result)

  ## Check for "All models failed" warning from tune
  if (!is.null(grid_result$warnings)) {

    if (any(grepl("All models failed", unlist(grid_result$warnings)))) {

      return(create_failed_result(config_id,
        "Grid search failed: all models failed during CV"))

    }

  }

  gc(verbose = FALSE)

  ## -----------------------------------------------------------------------
  ## Step 7: Prune check
  ## -----------------------------------------------------------------------
  ## If the best grid-search RPD is below threshold, skip Bayesian
  ## optimization. RPD < 1.0 means the model is no better than the mean
  ## predictor — no point spending Bayesian iterations on it. This RPD is on
  ## the original response scale (tuning_metric_set()), so the threshold means
  ## the same thing for every transformation.
  ## The config still gets last_fit metrics from grid-search best.
  ##
  ## The quality signal and the status label are kept apart (#38). Whenever
  ## prune = TRUE the gate's reading is recorded as below_prune_threshold,
  ## which fit() reads to warn when every member it fits fell below it. The
  ## "pruned" status means only that Bayesian refinement was skipped, so with
  ## bayesian_iter = 0, where there is nothing to skip, a config below the
  ## threshold is a success. It used to be labelled "pruned" anyway, which
  ## evaluate() and fit() rank only as a fallback.

  skip_bayesian         <- FALSE
  below_prune_threshold <- NA

  if (prune) {

    best_grid <- tune::show_best(grid_results, metric = "rpd", n = 1)

    below_prune_threshold <- !is.finite(best_grid$mean[1]) ||
      best_grid$mean[1] < prune_threshold

    skip_bayesian <- below_prune_threshold && bayesian_iter > 0

  }

  ## -----------------------------------------------------------------------
  ## Step 8: Bayesian optimization
  ## -----------------------------------------------------------------------
  ## Falls back to grid results on failure — never aborts the config.

  final_tune_results <- grid_results

  if (!skip_bayesian && bayesian_iter > 0) {

    ## Re-pin before every stochastic stage. tune's future path advances the
    ## parent stream past where the sequential loop leaves it (future_lapply()
    ## draws worker seeds from it), so a stage seeded only by what came before
    ## would differ between allow_par = TRUE and FALSE. Pinning here makes
    ## the axis a pure performance choice (review finding, 2026-09-15).
    set.seed(seed, kind = "Mersenne-Twister")

    bayes_result <- safely_execute(
      suppressMessages(suppressWarnings(
        tune::tune_bayes(
          object     = wflow,
          resamples  = cv_folds,
          initial    = grid_results,
          iter       = bayesian_iter,
          param_info = param_set,
          metrics    = tune_metrics,
          control    = tune::control_bayes(
            save_pred     = FALSE,
            save_workflow = FALSE,
            verbose       = FALSE,
            no_improve    = BAYES_NO_IMPROVE_LIMIT,
            allow_par     = allow_par,
            parallel_over = parallel_over
          )
        )
      )),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    if (is.null(bayes_result$error)) {

      collect_from(bayes_result)

      ## Check for all-models-failed
      all_failed <- FALSE

      if (!is.null(bayes_result$warnings)) {
        all_failed <- any(grepl("All models failed", unlist(bayes_result$warnings)))
      }

      if (!all_failed) {
        final_tune_results <- bayes_result$result
      }

    }

    ## On failure or all-models-failed: final_tune_results stays as grid_results
    gc(verbose = FALSE)

  }

  ## -----------------------------------------------------------------------
  ## Step 9: Select best hyperparameters
  ## -----------------------------------------------------------------------

  best_result <- safely_execute(
    tune::select_best(final_tune_results, metric = "rmse"),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(best_result$error)) {

    return(create_failed_result(config_id,
      paste0("Parameter selection failed: ", best_result$error$message)))

  }

  best_params <- best_result$result

  ## -----------------------------------------------------------------------
  ## Step 9b: Record the CV panel at the selected hyperparameters
  ## -----------------------------------------------------------------------
  ## The cross-validated means tune scored for the chosen config, on the
  ## original scale via tuning_metric_set(). evaluate() and fit() rank on
  ## these rather than on the test-set metrics computed below, so the test
  ## set stays held out from selection and its metrics are honest (#50).

  cv_panel <- cv_panel_at(final_tune_results, best_params)

  if (all(is.na(unlist(cv_panel)))) {

    collected_warnings <- c(
      collected_warnings,
      "CV metrics at the selected hyperparameters could not be recovered; cv_* columns are NA."
    )

  }

  ## -----------------------------------------------------------------------
  ## Step 10: Finalize workflow and evaluate on test set
  ## -----------------------------------------------------------------------

  final_wflow_result <- safely_execute(
    tune::finalize_workflow(wflow, best_params),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(final_wflow_result$error)) {

    return(create_failed_result(config_id,
      paste0("Workflow finalization failed: ", final_wflow_result$error$message)))

  }

  final_wflow <- final_wflow_result$result

  ## -----------------------------------------------------------------------
  ## Step 11: Last fit on the held-out test set
  ## -----------------------------------------------------------------------

  ## Re-pin: last_fit() draws the engine seed from the parent stream, whose
  ## position now depends on which axis the tuning ran on (see Step 8).
  set.seed(seed, kind = "Mersenne-Twister")

  lastfit_result <- safely_execute(
    tune::last_fit(final_wflow, split = split),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(lastfit_result$error)) {

    return(create_failed_result(config_id,
      paste0("Test evaluation failed: ", lastfit_result$error$message)))

  }

  last_fit_obj <- lastfit_result$result
  collect_from(lastfit_result)

  ## -----------------------------------------------------------------------
  ## Step 12: Extract predictions and back-transform
  ## -----------------------------------------------------------------------

  test_predictions <- tune::collect_predictions(last_fit_obj)

  ## Unconditional, like predict(): the "none" branch is a passthrough, and
  ## the zero floor inside back_transform_predictions() must apply to every
  ## transformation so evaluation scores what deploy serves (#53).
  bt_result <- safely_execute(
    back_transform_predictions(test_predictions$.pred, transformation,
                               warn = FALSE),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(bt_result$error)) {

    return(create_failed_result(config_id,
      paste0("Back-transformation failed: ", bt_result$error$message)))

  }

  test_predictions$.pred <- bt_result$result

  ## -----------------------------------------------------------------------
  ## Step 13: Compute original-scale metrics
  ## -----------------------------------------------------------------------

  metrics_result <- safely_execute(
    compute_original_scale_metrics(
      truth    = test_predictions[[outcome_col]],
      estimate = test_predictions$.pred
    ) |>
      tidyr::pivot_wider(names_from = .metric, values_from = .estimate),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(metrics_result$error) ||
      is.null(metrics_result$result) ||
      nrow(metrics_result$result) == 0) {

    test_metrics <- tibble::tibble(
      rmse  = NA_real_, rsq = NA_real_, mae   = NA_real_,
      rrmse = NA_real_, rpd = NA_real_, ccc   = NA_real_
    )

  } else {

    test_metrics <- metrics_result$result

    ## Guard: ensure all 6 metrics present after pivot
    required <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")
    missing  <- setdiff(required, names(test_metrics))

    for (m in missing) test_metrics[[m]] <- NA_real_

  }

  ## -----------------------------------------------------------------------
  ## Step 14: Return result row
  ## -----------------------------------------------------------------------

  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  tibble::tibble(
    config_id     = config_id,
    status        = if (skip_bayesian) "pruned" else "success",
    below_prune_threshold = below_prune_threshold,
    prune_threshold       = if (prune) as.numeric(prune_threshold) else NA_real_,
    rmse          = test_metrics$rmse   %||% NA_real_,
    rrmse         = test_metrics$rrmse  %||% NA_real_,
    rsq           = test_metrics$rsq    %||% NA_real_,
    ccc           = test_metrics$ccc    %||% NA_real_,
    rpd           = test_metrics$rpd    %||% NA_real_,
    mae           = test_metrics$mae    %||% NA_real_,
    cv_rmse       = cv_panel$cv_rmse,
    cv_rrmse      = cv_panel$cv_rrmse,
    cv_rsq        = cv_panel$cv_rsq,
    cv_ccc        = cv_panel$cv_ccc,
    cv_rpd        = cv_panel$cv_rpd,
    cv_mae        = cv_panel$cv_mae,
    scoring_schema = SCORING_SCHEMA,
    best_params   = list(best_params),
    error_message = NA_character_,
    warnings      = list(if (length(collected_warnings) > 0) collected_warnings else NULL),
    runtime_secs  = runtime
  )

}

## ---------------------------------------------------------------------------
## prepped_predictors — the columns dials::finalize() may count
## ---------------------------------------------------------------------------

#' Name the predictor columns of a prepped recipe
#'
#' @description
#' Returns the columns of `baked` that the prepped recipe gives the
#' `predictor` role. This is the frame `dials::finalize()` should see when it
#' sets the `mtry` or `num_comp` upper bound, because that bound has to be
#' the number of columns the model will actually be handed.
#'
#' @details
#' The roles come from the recipe rather than from subtracting the known
#' non-predictors off the baked frame. `build_recipe()` holds sibling lab
#' measurements at role `response_hold` and unused covariates at
#' `covariate_hold`; both survive into the baked frame and neither is a
#' predictor, so subtraction counts them and the ceiling lands above the real
#' predictor count. `tune_grid()` can then sample an `mtry` larger than the
#' model matrix is wide, which fails the config for a reason that has nothing
#' to do with the config.
#'
#' @param prepped A prepped `recipe`.
#' @param baked The frame from `recipes::bake(prepped, new_data = NULL)`.
#'
#' @return Character. Predictor column names, in baked order.
#' @keywords internal
#' @noRd
prepped_predictors <- function(prepped, baked) {

  roles <- summary(prepped)

  intersect(names(baked), roles$variable[roles$role %in% "predictor"])

}


## ---------------------------------------------------------------------------
## cv_panel_at — the six CV means at one set of hyperparameters
## ---------------------------------------------------------------------------

#' Cross-validated metric means at the selected hyperparameters
#'
#' @description
#' Reads `tune::collect_metrics()` for the tuning result and returns the mean
#' of each metric at the `.config` that `tune::select_best()` chose. These are
#' the `cv_*` columns of `evaluation$results`, the quantities `evaluate()` and
#' `fit()` rank on. Missing or unrecoverable values are `NA_real_`; the
#' function never errors, so a bookkeeping failure cannot fail a config whose
#' fit succeeded.
#'
#' @param tune_results A `tune_results` object (grid or Bayesian).
#' @param best_params One-row tibble from `tune::select_best()`, carrying
#'   `.config`.
#' @param metrics Metric names to look up, in order.
#' @return One-row tibble with columns `cv_<metric>`.
#' @keywords internal
#' @noRd
cv_panel_at <- function(tune_results, best_params,
                        metrics = c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")) {

  out <- stats::setNames(as.list(rep(NA_real_, length(metrics))),
                         paste0("cv_", metrics))

  usable <- !is.null(tune_results) && !is.null(best_params) &&
    ".config" %in% names(best_params) && nrow(best_params) >= 1

  if (!usable) return(tibble::as_tibble(out))

  panel <- tryCatch(tune::collect_metrics(tune_results), error = function(e) NULL)

  if (is.null(panel) || !all(c(".config", ".metric", "mean") %in% names(panel))) {

    return(tibble::as_tibble(out))

  }

  panel <- panel[panel$.config == best_params$.config[[1]], , drop = FALSE]

  for (m in metrics) {

    v <- panel$mean[panel$.metric == m]

    if (length(v) == 1 && is.finite(v)) out[[paste0("cv_", m)]] <- v

  }

  tibble::as_tibble(out)

}
