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
#' @param prune Logical. If TRUE, skip Bayesian optimization when grid search
#'   RMSE exceeds `prune_threshold`.
#' @param prune_threshold Numeric. RMSE threshold for pruning (on transformed
#'   scale). Only used when `prune = TRUE`.
#' @param allow_par Logical. Passed to `tune::control_grid()` and
#'   `tune::control_bayes()` to enable parallel CV folds.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return Single-row tibble with columns: `config_id`, `status`, `rmse`,
#'   `rrmse`, `rsq`, `ccc`, `rpd`, `mae`, `best_params` (list-column),
#'   `error_message`, `runtime_secs`.
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
                                   prune_threshold = 100,
                                   allow_par       = FALSE,
                                   seed            = 42L) {

  start_time <- Sys.time()
  set.seed(seed)

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
    build_recipe(config_row, train_data, role_map),
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
    workflows::workflow() %>%
      workflows::add_recipe(recipe) %>%
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

      ## Keep only predictor columns (remove outcome, id, meta)
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

      return(create_failed_result(config_id,
        paste0("Parameter finalization failed: ", finalize_result$error$message)))

    }

    param_set <- finalize_result$result
    collect_from(finalize_result)

  }

  ## -----------------------------------------------------------------------
  ## Step 5: Define tuning metric set
  ## -----------------------------------------------------------------------

  tune_metrics <- yardstick::metric_set(
    yardstick::rmse,
    rrmse,
    yardstick::rsq,
    yardstick::mae,
    rpd,
    ccc
  )

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
          parallel_over = "everything"
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
  ## predictor — no point spending Bayesian iterations on it.
  ## The config still gets last_fit metrics from grid-search best.

  skip_bayesian <- FALSE

  if (prune) {

    best_grid <- tune::show_best(grid_results, metric = "rpd", n = 1)

    if (!is.finite(best_grid$mean[1]) || best_grid$mean[1] < prune_threshold) {

      skip_bayesian <- TRUE

    }

  }

  ## -----------------------------------------------------------------------
  ## Step 8: Bayesian optimization
  ## -----------------------------------------------------------------------
  ## Falls back to grid results on failure — never aborts the config.

  final_tune_results <- grid_results

  if (!skip_bayesian && bayesian_iter > 0) {

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
            parallel_over = "everything"
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

  if (needs_back_transformation(transformation)) {

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

  }

  ## -----------------------------------------------------------------------
  ## Step 13: Compute original-scale metrics
  ## -----------------------------------------------------------------------

  metrics_result <- safely_execute(
    compute_original_scale_metrics(
      truth    = test_predictions[[outcome_col]],
      estimate = test_predictions$.pred
    ) %>%
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
    rmse          = test_metrics$rmse   %||% NA_real_,
    rrmse         = test_metrics$rrmse  %||% NA_real_,
    rsq           = test_metrics$rsq    %||% NA_real_,
    ccc           = test_metrics$ccc    %||% NA_real_,
    rpd           = test_metrics$rpd    %||% NA_real_,
    mae           = test_metrics$mae    %||% NA_real_,
    best_params   = list(best_params),
    error_message = NA_character_,
    warnings      = list(if (length(collected_warnings) > 0) collected_warnings else NULL),
    runtime_secs  = runtime
  )

}
