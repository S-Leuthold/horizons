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
#' @param min_calib_rows,min_ad_rows,min_degradation_rows Integer. The
#'   control fields of the same names: the fewest rows UQ and AD calibrate
#'   on, AD's covariance is estimated from, and the degradation check
#'   bootstraps from.
#'
#' @return List with fields: config_id, status, degraded, degraded_reason,
#'   fitted_workflow, best_params, warm_start, start_grid_size,
#'   cv_predictions, test_metrics, cv_metrics, uq, warnings, error_message,
#'   runtime_secs. `uq`, when present, carries the measured coverage on the
#'   held-out test rows: `test_coverage`, `test_mean_width` and `n_test`.
#'   `warm_start` is `TRUE` when the re-tune started from
#'   `best_params_eval`, `FALSE` when it fell back to a space-filling grid of
#'   `start_grid_size` points, and `NA` (both) when the config failed
#'   before tuning.
#'
#' @keywords internal
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
                              response_fences     = NULL,
                              min_calib_rows       = N_CALIB_MIN,
                              min_ad_rows          = N_AD_TRAIN_MIN,
                              min_degradation_rows = DEGRADATION_MIN_N,
                              debug                = FALSE,
                              keep_traces          = FALSE) {

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
  n_folds        <- nrow(cv_resamples)

  ## The member's warnings, as records that render_warning_log() reduces to
  ## one line per distinct message (#96): tune's notes and the package's own
  ## lines, gathered as the unit runs, and, once it returns, the warnings the
  ## unit caught as they were raised.
  warning_log <- new_warning_log()

  ## tune catches the warnings raised inside its resampling and keeps them in
  ## .notes, where run_unit() never sees them. Both the re-tune and the
  ## out-of-fold predictions resample cv_resamples, so their fold counts
  ## merge.
  collect_notes_from <- function(tune_results, candidate = NULL) {
    notes <- tune_note_records(tune_results, type = "warning", scope = "cv",
                               candidate = candidate)
    warning_log <<- dplyr::bind_rows(warning_log, notes)
  }

  pin <- function(text) {
    warning_log <<- dplyr::bind_rows(warning_log, text_records(text, pinned = TRUE))
  }

  ## UQ, measured coverage, AD and slimming each run as a unit of their own
  ## inside the member's: one that fails costs the member only what it
  ## computes, and the member says so (on_failure). Their warnings and notes
  ## are the member's, so they join its records, under its unit, for its row
  ## and its verb's log; a failure is reported by the member instead.
  addon_records <- new_unit_records()

  run_addon <- function(expr, unit, on_failure) {
    out  <- run_unit(expr, unit = unit, id = config_id, debug = debug)
    kept <- out$records[out$records$type != "failure", , drop = FALSE]
    kept$unit <- rep("member", nrow(kept))
    addon_records <<- dplyr::bind_rows(addon_records, kept)
    if (!out$failed) return(out$value)
    on_failure(unit_failure_message(out$records, outside = NULL))
    NULL
  }

  ## An add-on that was asked for and not computed is skipped, with why
  skipped <- function(what) {
    function(cause) warn_in_unit("skipped", "{what} not computed: {cause}")
  }

  ## How the re-tune started, once it has: from evaluate()'s parameters, or
  ## from a space-filling grid when there were none to use (#45)
  warm_start      <- NA
  start_grid_size <- NA_integer_

  unit <- run_unit({

    ## -----------------------------------------------------------------------
    ## Step 1: Build recipe, model specification and workflow
    ## -----------------------------------------------------------------------

    recipe <- unit_step("recipe", build_recipe(config_row, train_data, role_map,
                                               sg_window     = sg_window,
                                               pca_threshold = pca_threshold))

    model_spec <- unit_step("model_spec", define_model_spec(config_row$model))

    wflow <- unit_step("workflow", workflows::workflow() |>
                         workflows::add_recipe(recipe) |>
                         workflows::add_model(model_spec))

    ## -----------------------------------------------------------------------
    ## Step 2: Finalize parameter set (mtry / num_comp upper bound; num_comp
    ## also by the smallest fold's rows, #216)
    ## -----------------------------------------------------------------------
    ## The prep rule evaluate_single_config() follows: that prep is the only
    ## one before tuning, so a step's error there ends the member with its
    ## class, and every other model's recipe is prepped only when tune's notes
    ## show a step failed (check_tune_result()).

    param_set   <- workflows::extract_parameter_set_dials(wflow)
    prepped_now <- any(c("mtry", "num_comp") %in% param_set$name)

    if (prepped_now) {

      prepped <- unit_step("prep", recipes::prep(recipe))
      baked   <- unit_step("prep", recipes::bake(prepped, new_data = NULL))

      param_set <- unit_step("finalize_parameters", {
        eval_data <- baked[, prepped_predictors(prepped, baked), drop = FALSE]
        finalized <- dials::finalize(param_set, eval_data)
        cap_pls_components(finalized, ncol(eval_data), min_analysis_rows(cv_resamples))
      })

      rm(prepped, baked)
      invisible(gc(verbose = FALSE))

    }

    ## -----------------------------------------------------------------------
    ## Step 3: Define tuning metric set
    ## -----------------------------------------------------------------------

    ## Scored on the original response scale, for the same reason as in
    ## evaluate_single_config(): the skip = TRUE transform never reaches tune's
    ## assessment set. The re-tune selects on "rmse" by name, which the
    ## factory preserves. See #49. Clamped to the outcome's range (#76).
    tune_metrics <- tuning_metric_set(transformation, metrics = c("rmse", "rsq"),
                                      outcome_range = outcome_range)

    ## -----------------------------------------------------------------------
    ## Step 4: Warm-start re-tuning
    ## -----------------------------------------------------------------------
    ## The grid is centred on evaluate()'s parameters, or space-filling when
    ## there are none to use; how it started is recorded before it runs, so a
    ## re-tune that fails still says.

    initial_grid    <- unit_step("warm_start_tune", build_warmstart_grid(
      best_params_eval, param_set, max_points = grid_size
    ))
    warm_start      <- usable_best_params(best_params_eval, param_set)
    start_grid_size <- nrow(initial_grid)

    retune <- unit_step("warm_start_tune", tune_warmstart_bayes(
      workflow      = wflow,
      cv_resamples  = cv_resamples,
      initial_grid  = initial_grid,
      param_set     = param_set,
      bayesian_iter = final_bayesian_iter,
      metric_set    = tune_metrics,
      allow_par     = allow_par,
      recipe        = if (!prepped_now) recipe,
      debug         = debug
    ))

    best_params <- retune$best_params

    ## The warnings tune kept for the chosen candidate (#109)
    collect_notes_from(retune$tune_results)

    ## -----------------------------------------------------------------------
    ## Step 5: Finalize workflow and generate OOF predictions
    ## -----------------------------------------------------------------------

    finalized_wf <- unit_step("finalize_workflow", tune::finalize_workflow(wflow, best_params))

    ## Re-pin before every stochastic stage so results do not depend on which
    ## axis the previous stage ran on (see evaluate_single_config(), Step 8).
    set.seed(seed, kind = "Mersenne-Twister")

    ## Messages muffled, as around the tuning calls: tune reports its notes
    ## on the console as it goes, and the member records them instead.
    cv_fit <- unit_step("oof_predictions", {
      results <- suppressMessages(tune::fit_resamples(
        finalized_wf,
        resamples = cv_resamples,
        control   = tune::control_resamples(
          save_pred     = TRUE,
          allow_par     = allow_par,
          parallel_over = "resamples"
        )
      ))
      check_tune_result(results, "every out-of-fold fit failed",
                        recipe = if (!prepped_now) recipe, debug = debug)
      results
    })

    collect_notes_from(cv_fit)

    oof_raw <- unit_step("oof_predictions", tune::collect_predictions(cv_fit))

    ## Shape cv_predictions: .row, sample_id, .fold, config_id, .pred_trans,
    ## truth, then .pred below. .row is a position in the frame the folds were
    ## cut from (train_Fit in fit()), and sample_id is read from that same
    ## frame, so each prediction keeps its link to a sample and .row is never
    ## the only key.
    fold_frame <- cv_resamples$splits[[1]]$data

    cv_predictions <- tibble::tibble(
      .row        = as.integer(oof_raw$.row),
      sample_id   = fold_frame[[id_column(role_map)]][oof_raw$.row],
      .fold       = oof_raw$id,
      config_id   = config_id,
      .pred_trans = oof_raw$.pred,
      truth       = oof_raw[[outcome_col]]
    )

    ## Back-transform .pred to original scale. Unconditional, like predict():
    ## "none" is a passthrough, and the clamp to the outcome's range inside
    ## back_transform_predictions() must reach these OOF predictions because
    ## they are the meta-learner's training features, and predict() clamps the
    ## same members at serve time (#53, #76). truth from collect_predictions()
    ## is already on the original scale: the outcome transform is a skip =
    ## TRUE step, skipped at assessment.
    cv_predictions$.pred <- unit_step("oof_back_transform", back_transform_predictions(
      cv_predictions$.pred_trans, transformation, warn = FALSE, outcome_range = outcome_range
    ))

    ## -----------------------------------------------------------------------
    ## Step 6: Fold-wise CV metrics (original scale)
    ## -----------------------------------------------------------------------
    ## Inside the unit, so a warning yardstick raises here is the member's
    ## (#102).

    cv_metrics <- unit_step("cv_panel", {

      fold_metrics <- split(cv_predictions, cv_predictions$`.fold`) |>
        purrr::map_dfr(
          ~ compute_original_scale_metrics(.x$truth, .x$.pred),
          .id = "fold"
        )

      ## A fold whose metric is NA (fewer than two complete pairs, or a metric
      ## undefined there) is left out of the mean, so it is left out of the
      ## count the standard error divides by too.
      fold_metrics |>
        dplyr::group_by(.metric) |>
        dplyr::summarize(
          mean    = mean(.estimate, na.rm = TRUE),
          std_err = stats::sd(.estimate, na.rm = TRUE) / sqrt(sum(!is.na(.estimate))),
          .groups = "drop"
        )

    })

    ## -----------------------------------------------------------------------
    ## Step 7: Fit final model on full training data
    ## -----------------------------------------------------------------------

    set.seed(seed, kind = "Mersenne-Twister")

    final_fit <- unit_step("final_fit", suppressMessages(
      workflows::fit(finalized_wf, data = train_data)
    ))

    ## -----------------------------------------------------------------------
    ## Step 8: Evaluate on test_F (original scale)
    ## -----------------------------------------------------------------------

    test_preds <- unit_step("test_predict", stats::predict(final_fit, new_data = test_data)$.pred)

    ## Back-transform, unconditionally (see the OOF block above, #53 and #76)
    test_preds <- unit_step("test_back_transform", back_transform_predictions(
      test_preds, transformation, warn = FALSE, outcome_range = outcome_range
    ))

    ## test_truth is already on the original scale: test_data is raw (never
    ## passed through the recipe)
    test_truth <- test_data[[outcome_col]]

    ## Six NAs when fewer than two test rows have a prediction, and the
    ## member's warnings say why
    scored       <- unit_step("test_metrics", compute_original_scale_metrics(test_truth, test_preds))
    test_metrics <- metrics_row(scored)
    na_reason    <- metrics_na_reason(scored)

    if (!is.null(na_reason)) pin(na_reason)

    ## -----------------------------------------------------------------------
    ## Step 9: Degradation detection
    ## -----------------------------------------------------------------------
    ## Bootstrap the test RPD and flag the config only when its whole interval
    ## lies below the CV mean RPD. The test RPD is one estimate from a few dozen
    ## rows, so comparing it to a band built from the CV standard error, which
    ## describes the CV mean and not a single held-out estimate, flagged healthy
    ## fits (#119).
    ##
    ## After a response trim (#77) the folds ran on rows inside the training
    ## fences while the test rows are untrimmed, so the two RPDs would describe
    ## two populations and the flag would fire whenever a trim removed an
    ## extreme. The diagnostic RPD is then taken over the test rows inside the
    ## same fences; test_metrics, which is what is reported, stays untrimmed.

    degradation <- unit_step("degradation", check_degradation(
      cv_metrics, test_truth, test_preds, response_fences, seed = seed,
      min_n = min_degradation_rows
    ))

    ## -----------------------------------------------------------------------
    ## Step 10: UQ (must run BEFORE butchering — needs the mold)
    ## -----------------------------------------------------------------------
    ## A unit of its own (#145): a failure leaves the member without intervals
    ## and names it in a skipped warning; a tripwire inside still stops the
    ## call.

    uq_result <- NULL

    if (compute_uq && !is.null(calib_data)) {

      uq_result <- run_addon(fit_uq(
        fitted_workflow = final_fit,
        oof_predictions = cv_predictions,
        calib_data      = calib_data,
        role_map        = role_map,
        transformation  = transformation,
        level_default   = DEFAULT_UQ_LEVEL,
        seed            = seed,
        outcome_range   = outcome_range,
        min_calib_rows  = min_calib_rows,
        debug           = debug
      ), "uq", skipped("Prediction intervals (UQ) were"))

      ## Measured coverage on the held-out test rows (#118). The intervals are
      ## built by predict_intervals(), the function predict() serves them with,
      ## so this is the coverage a user of the fitted object gets on data the
      ## model never saw. oof_coverage stays on the bundle under its own name:
      ## it is an in-sample diagnostic for the quantile forest, not coverage.
      if (!is.null(uq_result)) {

        coverage <- run_addon(
          test_interval_coverage(uq_result, test_preds, test_data, test_truth,
                                 config_id, outcome_range),
          "coverage", skipped("Measured test coverage was")
        )

        uq_result <- c(uq_result, coverage %||% test_interval_coverage_missing())

      }

    }

    ## -----------------------------------------------------------------------
    ## Step 10b: AD (must also run BEFORE butchering — needs the mold)
    ## -----------------------------------------------------------------------
    ## Shares the UQ calibration split (D7), and is a unit of its own,
    ## independent of UQ's.

    ad_result <- NULL

    if (compute_ad && !is.null(calib_data)) {

      ad_result <- run_addon(fit_ad(
        fitted_workflow = final_fit,
        calib_data      = calib_data,
        level           = DEFAULT_AD_LEVEL,
        min_calib_rows  = min_calib_rows,
        min_ad_rows     = min_ad_rows
      ), "ad", skipped("The applicability domain (AD) was"))

    }

    ## -----------------------------------------------------------------------
    ## Step 11: Butcher the final fit for storage
    ## -----------------------------------------------------------------------
    ## A unit of its own too. Slimming only saves memory, so when it fails the
    ## workflow is stored whole, and that is a note rather than a warning.

    fitted_workflow <- run_addon(butcher::butcher(final_fit), "slimming", function(cause) {
      note_in_unit("The fitted workflow is stored whole, not slimmed: {cause}")
    })

    if (is.null(fitted_workflow)) {

      fitted_workflow <- final_fit

    } else {

      rm(final_fit)
      invisible(gc(verbose = FALSE))

    }

    list(
      config_id        = config_id,
      status           = "success",
      degraded         = degradation$degraded,
      degraded_reason  = degradation$reason,
      fitted_workflow  = fitted_workflow,
      best_params      = best_params,
      warm_start       = warm_start,
      start_grid_size  = start_grid_size,
      cv_predictions   = cv_predictions,
      test_metrics     = test_metrics,
      cv_metrics       = cv_metrics,
      uq               = uq_result,
      ad               = ad_result,
      warnings         = NULL,
      error_message    = NA_character_,
      class            = NA_character_,
      trace            = NULL,
      runtime_secs     = NA_real_
    )

  }, unit = "member", id = config_id, debug = debug, keep_traces = keep_traces)

  ## -------------------------------------------------------------------------
  ## The member, fitted or failed
  ## -------------------------------------------------------------------------
  ## The warnings the unit caught join the notes, so a warning caught as it
  ## was raised and the same warning in tune's notes are one line.

  records     <- dplyr::bind_rows(unit$records, addon_records)
  warning_log <- dplyr::bind_rows(unit_warning_records(records), warning_log)

  result <- if (unit$failed) {

    failure <- unit$records[unit$records$type == "failure", , drop = FALSE]

    list(
      config_id        = config_id,
      status           = "failed",
      degraded         = NA,
      degraded_reason  = NA_character_,
      fitted_workflow  = NULL,
      best_params      = NULL,
      warm_start       = warm_start,
      start_grid_size  = as.integer(start_grid_size),
      cv_predictions   = NULL,
      test_metrics     = NULL,
      cv_metrics       = NULL,
      uq               = NULL,
      ad               = NULL,
      warnings         = NULL,
      error_message    = unit_failure_message(unit$records, "Fit"),
      class            = failure$class[1],
      trace            = failure$trace[[1]],
      runtime_secs     = NA_real_
    )

  } else {

    unit$value

  }

  ### `[<-` with a list, so a member without warnings keeps the field as NULL
  result["warnings"]  <- list(render_warning_log(warning_log))
  result$runtime_secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  attr(result, "unit_records") <- records

  result

}


## ---------------------------------------------------------------------------
## test_interval_coverage(): measured coverage on the held-out test rows
## ---------------------------------------------------------------------------

#' Measure interval coverage on the held-out test rows
#'
#' @description
#' Builds prediction intervals for the test rows with [predict_intervals()],
#' the function `predict()` serves intervals with, and reports how many of the
#' test outcomes they contain. This is the honest coverage figure: the test
#' rows took no part in tuning, the quantile forest or the conformal
#' calibration (#118). Run as a unit of its own by [fit_single_config()]:
#' when [predict_intervals()] warns and returns `NULL`, this fails, the
#' figures stay `NA`, and the member names it in a `skipped` warning with the
#' interval warning beside it.
#'
#' @param uq A UQ bundle from [fit_uq()].
#' @param test_preds Numeric. Back-transformed test predictions, original
#'   scale.
#' @param test_data Data frame. The test rows, raw (as passed to `predict()`).
#' @param test_truth Numeric. The test outcomes, original scale.
#' @param config_id Character(1). Named in any warning.
#' @param outcome_range Numeric length 2. The bounds intervals are clamped to.
#' @return List with `test_coverage`, `test_mean_width` and `n_test`. Aborts
#'   with class `horizons_error_run` when no intervals were built.
#' @keywords internal
#' @noRd
test_interval_coverage <- function(uq, test_preds, test_data, test_truth,
                                   config_id, outcome_range) {

  intervals <- predict_intervals(uq, test_preds, test_data,
                                 config_id     = config_id,
                                 outcome_range = outcome_range)

  if (is.null(intervals)) {

    abort_horizons(
      "No intervals could be built for the test rows; the member's warnings say why.",
      kind = "run"
    )

  }

  scored  <- !is.na(test_truth) & !is.na(intervals$.pred_lower) &
    !is.na(intervals$.pred_upper)

  covered <- test_truth[scored] >= intervals$.pred_lower[scored] &
    test_truth[scored] <= intervals$.pred_upper[scored]

  list(
    test_coverage   = if (any(scored)) mean(covered) else NA_real_,
    test_mean_width = if (any(scored)) mean(intervals$.interval_width[scored]) else NA_real_,
    n_test          = sum(scored)
  )

}

#' @noRd
test_interval_coverage_missing <- function() {

  list(test_coverage = NA_real_, test_mean_width = NA_real_, n_test = 0L)

}


## ---------------------------------------------------------------------------
## check_degradation(): a bootstrap interval for the test RPD
## ---------------------------------------------------------------------------

#' Flag a fitted configuration whose test RPD is credibly below its CV RPD
#'
#' @description
#' The test RPD is a single estimate from the held-out rows and carries real
#' sampling variance; the CV standard error describes the CV mean, not that
#' estimate. So the test RPD is bootstrapped over the test rows, and the
#' config is flagged only when the whole `DEGRADATION_LEVEL` interval lies
#' below the CV mean RPD (#119). The bootstrap draws from its own seed and
#' restores the caller's RNG state, so it changes nothing downstream.
#'
#' After a response trim (#77) the folds ran on rows inside the training
#' fences while the test rows are untrimmed, so with `response_fences` the
#' test rows compared are those inside the fences, and the reason says so;
#' the test metrics `fit()` reports are untouched. Fewer than `min_n` usable
#' test rows leave too little to bootstrap, and nothing is flagged.
#'
#' @param cv_metrics Tibble with `.metric` and `mean`.
#' @param test_truth,test_preds Numeric. Test outcomes and back-transformed
#'   predictions, original scale.
#' @param response_fences Numeric `c(lower = , upper = )`, or `NULL` when no
#'   rows were trimmed.
#' @param seed Integer. Seed for the bootstrap draws.
#' @param n_boot Integer. Number of bootstrap resamples.
#' @param level Numeric. Coverage of the bootstrap interval.
#' @param min_n Integer. The fewest usable test rows the check bootstraps
#'   from; `control_horizons(min_degradation_rows = )`. Default
#'   `DEGRADATION_MIN_N`.
#' @return List with `degraded` (logical) and `reason` (character, `NA` when
#'   not degraded).
#' @keywords internal
#' @noRd
check_degradation <- function(cv_metrics, test_truth, test_preds,
                              response_fences = NULL, seed = 42L,
                              n_boot = DEGRADATION_BOOT_N,
                              level  = DEGRADATION_LEVEL,
                              min_n  = DEGRADATION_MIN_N) {

  not_flagged <- list(degraded = FALSE, reason = NA_character_)
  fences_text <- NULL
  usable      <- !is.na(test_truth) & !is.na(test_preds)

  if (!is.null(response_fences)) {

    inside <- usable &
      test_truth >= response_fences[["lower"]] &
      test_truth <= response_fences[["upper"]]

    fences_text <- sprintf(" of %d within the training fences [%.4g, %.4g]",
                           length(test_truth), response_fences[["lower"]],
                           response_fences[["upper"]])

    usable <- inside

  }

  truth <- test_truth[usable]
  preds <- test_preds[usable]
  n     <- length(truth)

  rpd_cv <- cv_metrics[cv_metrics$.metric == "rpd", , drop = FALSE]

  ## Constant test outcomes give an RPD of 0 that says nothing about the fit
  if (n < min_n || nrow(rpd_cv) != 1 || !is.finite(rpd_cv$mean) ||
      stats::sd(truth) < .Machine$double.eps) {

    return(not_flagged)

  }

  test_rpd <- rpd_vec(truth, preds)

  if (!is.finite(test_rpd)) return(not_flagged)

  ## Bootstrap with a local seed; the caller's stream is restored on exit.
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister")

  ## The same RPD as rpd_vec(), inline: SD of the outcomes over the RMSE, Inf
  ## for a perfect fit, 0 for constant outcomes.
  boot_rpd <- vapply(seq_len(n_boot), function(b) {

    i    <- sample.int(n, n, replace = TRUE)
    rmse <- sqrt(mean((truth[i] - preds[i])^2))
    sdv  <- stats::sd(truth[i])

    if (rmse < .Machine$double.eps) Inf
    else if (sdv < .Machine$double.eps) 0
    else sdv / rmse

  }, numeric(1))

  bounds <- stats::quantile(boot_rpd, c((1 - level) / 2, 1 - (1 - level) / 2),
                            names = FALSE)

  if (bounds[2] >= rpd_cv$mean) return(not_flagged)

  list(
    degraded = TRUE,
    reason   = sprintf(
      "test RPD %.3f (%g%% bootstrap interval [%.3f, %.3f], %d test rows%s) is below the CV mean RPD %.3f",
      test_rpd, 100 * level, bounds[1], bounds[2], n, fences_text %||% "", rpd_cv$mean
    )
  )

}
