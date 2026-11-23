#' Fit a Single Model Configuration (Capture Layer)
#'
#' @description
#' Inner-loop function for `fit()`. Takes a single config row from the
#' evaluate() leaderboard, re-tunes it with warm-start Bayesian optimization
#' from evaluate()'s hyperparameters and one neighbour per tuned parameter,
#' keeping evaluate()'s unless the best candidate beats them by the margin
#' (`choose_member_params()`), generates OOF predictions, fits a final
#' deployable model, evaluates on the held-out test set, and detects
#' performance degradation.
#'
#' This function is the **capture layer** — it runs silently and returns a
#' structured list. All console output (tree rendering, progress messages) is
#' the responsibility of the calling `fit()` function.
#'
#' Each fold's recipe is prepped once, as in [evaluate_single_config()],
#' and members of one preprocessing and selection read one prep's entry
#' (`prep_fold_entry()`): the warm-start re-tune, its Bayesian iterations
#' and the out-of-fold predictions run a workflow of the model alone on the
#' folds assembled from them (`assemble_folds()`), which keep
#' the positions of the rows they were cut from, so each out-of-fold
#' prediction's `.row` names its sample. The final fit runs the whole
#' workflow on `train_data`, so the stored workflow, and with it `predict()`,
#' the prediction intervals and the applicability domain, bakes new rows
#' through the whole recipe.
#'
#' The member is one unit (`run_unit()`): a run failure in any step ends it,
#' and the member records the step (`UNIT_STEPS`), the cause, its root class
#' and, with `keep_traces`, its backtrace. A door, internal or dependency
#' error is not the member's and propagates. UQ, measured coverage, AD and
#' slimming run as units of their own inside it: one that fails costs the
#' member only what it computes, and the member says so with a `skipped`
#' warning (slimming: a note).
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
#' @param final_bayesian_iter Integer. Bayesian iterations for re-tuning, at
#'   least: raised as `evaluate_single_config()`'s are (`search_budget()`),
#'   counting the points the re-tune starts from. When every tuned parameter
#'   is a whole number and the space costs no more fits than the start and
#'   those iterations together (a submodel's values sharing one fit), the
#'   grid is every point and no Bayesian search runs (`exhaustive_grid()`).
#'   0 turns the re-tune off for a member with usable `best_params_eval`: it
#'   keeps them, clamped into its range, scored by the out-of-fold pass
#'   alone. A cold start at 0 still grids `grid_size` points.
#' @param grid_size Integer. The space-filling grid's size on a cold start
#'   (`best_params_eval` `NULL` or unusable). A warm start begins from
#'   `evaluate()`'s parameters and one neighbour per tuned parameter
#'   (`warm_start_size()`), whatever `grid_size` is.
#' @param tune_ranges Named list of `c(lower, upper)` by parameter id, in
#'   dials' units: the ranges the re-tune searches, set before the data cap
#'   them (`apply_tune_ranges()`). `fit()` reads them from `configure()`'s
#'   `config$tuning$ranges`, the ranges `evaluate()` searched. Default
#'   `TUNE_RANGES`.
#' @param no_improve A whole number of at least 1, or `FALSE`: the re-tune's
#'   Bayesian iterations without improvement after which it stops
#'   (`control_horizons(no_improve = )`, which `fit()` reads). Default
#'   `BAYES_NO_IMPROVE_DEFAULT`.
#' @param compute_uq Logical. Whether to train UQ components.
#' @param compute_ad Logical. Whether to fit the applicability domain, on the
#'   same calibration set as UQ.
#' @param allow_par Logical. Passed to tune control functions.
#' @param seed Integer. The run's seed, from which each stage's seed is
#'   derived with the stage's name (`with_unit_seed()`).
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
#' @param debug Logical. Contain nothing: the first failure, inside tune and
#'   the add-ons included, stops the call with its backtrace, and the
#'   warm-start Bayesian search's failure is not fallen back from. Warnings
#'   are still recorded.
#' @param keep_traces Logical. Keep a failure's backtrace, as text, in the
#'   member's `trace`.
#' @param isolate Logical. Fit a model whose engine `MODEL_SPECS` marks
#'   `isolate` through its isolated twin, each fit in a process of its own
#'   (`model_engine()`); `control_horizons(isolate = )`. Default `TRUE`.
#' @param fold_entries The entries of the member's fold preps, as `fit()`
#'   built them, shaped as [evaluate_single_config()] takes them. `NULL`
#'   (default) builds them here, with the same code.
#'
#' @return List with fields: config_id, status, degraded (`NA` when the
#'   check could not run), degraded_reason, fitted_workflow, best_params,
#'   warm_start, start_grid_size, bayes_iter, bayes_stop, retune_moved,
#'   retune_gain (a fitted member's), cv_predictions, test_metrics, cv_metrics,
#'   uq, ad, warnings, error_message (the failed step and the cause), class
#'   (the failure's root class), trace (the backtrace as text under
#'   `keep_traces`, else `NULL`), runtime_secs. `uq`, when present, carries
#'   the measured coverage on the held-out test rows: `test_coverage`,
#'   `test_mean_width` and `n_test`. The unit's records, with its add-ons'
#'   warnings and notes, ride on the list as the attribute
#'   `"unit_records"`, for `fit()` to lift its horizons warnings and notes
#'   onto the log.
#'   `warm_start` is `TRUE` when `best_params_eval` was usable, so the
#'   re-tune starts from it and one neighbour per tuned parameter, or from
#'   the whole space when that fits the budget (`exhaustive_grid()`), `FALSE`
#'   when it fell back to a
#'   space-filling grid, and `NA` (both) when the config failed before
#'   tuning. `start_grid_size` is the starting grid's points, `NA` with
#'   `warm_start` `TRUE` when the re-tune is off. `bayes_iter`
#'   and `bayes_stop` are the iterations the re-tune's Bayesian search began
#'   and why it ended (`"no_improve"`, `"budget"` or `"failed"`), both `NA`
#'   when no search ran. `retune_moved` is `FALSE` when the member kept
#'   `evaluate()`'s parameters, clamped into its range, `TRUE` when the
#'   re-tune chose another candidate (one past the margin, or any when
#'   `evaluate()`'s were not scored on every fold), and `NA` with none to
#'   keep (a cold start, unusable parameters, or a member that failed before
#'   the choice); `retune_gain` is the best candidate's lead over them in its
#'   own standard errors (`choose_member_params()`), for the tree.
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
                              tune_ranges         = TUNE_RANGES,
                              no_improve          = BAYES_NO_IMPROVE_DEFAULT,
                              compute_uq          = FALSE,
                              compute_ad          = FALSE,
                              allow_par           = FALSE,
                              seed,
                              sg_window           = DEFAULT_SG_WINDOW,
                              pca_threshold       = DEFAULT_PCA_THRESHOLD,
                              outcome_range       = DEFAULT_OUTCOME_RANGE,
                              response_fences     = NULL,
                              min_calib_rows       = N_CALIB_MIN,
                              min_ad_rows          = N_AD_TRAIN_MIN,
                              min_degradation_rows = DEGRADATION_MIN_N,
                              debug                = FALSE,
                              keep_traces          = FALSE,
                              isolate              = TRUE,
                              fold_entries         = NULL) {

  start_time <- Sys.time()

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

  ## Each stage that draws is a unit of its own, named under the member's
  ## config_id, as evaluate_single_config()'s are: its draws depend on the
  ## run's seed and the stage alone. "fit" in the name keeps them apart from
  ## the configuration's stages in evaluate().
  stage <- function(name) c("fit", config_id, name)

  ## tune catches the warnings raised inside its resampling and keeps them in
  ## .notes, where run_unit() never sees them. Both the re-tune and the
  ## out-of-fold predictions resample the same folds, so their fold counts
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
    out <- run_add_on(expr, unit, config_id, "member", on_failure, debug = debug)
    addon_records <<- dplyr::bind_rows(addon_records, out$records)
    out$value
  }

  ## How the re-tune started, once it has: from evaluate()'s parameters, or
  ## from a space-filling grid when there were none to use (#45)
  warm_start      <- NA
  start_grid_size <- NA_integer_

  ## How far the re-tune's Bayesian search went, once it has run
  bayes_iter <- NA_integer_
  bayes_stop <- NA_character_

  ## Whether the re-tune's choice left evaluate()'s parameters, once it has
  ## chosen, and by how much the best candidate led them
  retune_moved <- NA
  retune_gain  <- NA_real_

  unit <- run_unit({

    ## -----------------------------------------------------------------------
    ## Step 1: Build recipe, model specification and workflow
    ## -----------------------------------------------------------------------

    recipe <- unit_step("recipe", build_recipe(config_row, train_data, role_map,
                                               sg_window     = sg_window,
                                               pca_threshold = pca_threshold))

    model_spec <- unit_step("model_spec", define_model_spec(config_row$model, isolate = isolate))

    wflow <- unit_step("workflow", workflows::workflow() |>
                         workflows::add_recipe(recipe) |>
                         workflows::add_model(model_spec))

    ## -----------------------------------------------------------------------
    ## Step 1b: The training rows' spectra transformed once, the outcome the
    ## folds train on, and the workflow tune searches
    ## -----------------------------------------------------------------------
    ## As in evaluate_single_config(), from the frame the folds were cut from,
    ## whose positions the assembled folds keep: train_Fit in fit(), which is
    ## train_data too.

    fold_inputs <- fold_prep_inputs(config_row, cv_resamples$splits[[1]]$data, role_map,
                                    sg_window = sg_window)

    model_wflow <- unit_step("workflow", fold_workflow(model_spec, outcome_col))

    ## -----------------------------------------------------------------------
    ## Step 2: Finalize parameter set (the ranges, then the mtry / num_comp
    ## upper bound; num_comp also by the smallest fold's rows, #216)
    ## -----------------------------------------------------------------------
    ## The configured ranges first, as in evaluate_single_config(), so the
    ## re-tune searches the space evaluate() did. The prep rule
    ## evaluate_single_config() follows: each fold's recipe is
    ## prepped once, before the re-tune, and the re-tune and the out-of-fold
    ## predictions are scored on it; the bounds are read from one more prep,
    ## of the fold recipe on the whole training part. A step's error in either
    ## ends the member with its class.

    param_set   <- workflows::extract_parameter_set_dials(wflow)
    param_set   <- unit_step("finalize_parameters", apply_tune_ranges(param_set, tune_ranges))
    prepped_now <- bounded_model(config_row$model)

    ## The member's fold preps: fit()'s entries, or, called on its own,
    ## entries built here by fit()'s code as they are read
    entry_at <- fold_entry_reader(
      fold_entries,
      config_fold_units("fit", config_row, cv_resamples, role_map, data = NULL, seed = seed,
                        sg_window = sg_window, pca_threshold = pca_threshold, bounded = prepped_now),
      function(unit, rows) prep_fold_entry(unit, fold_inputs$data, rows, role_map, seed,
                                           sg_window, pca_threshold, debug = debug,
                                           keep_traces = keep_traces),
      cv_resamples
    )

    if (prepped_now) {

      ## The training part's prep sets the bounds, and is not the model
      ## returned, so the selection steps' warnings are not taken from it
      ## (the final fit's record is read instead)
      bound     <- unit_step("prep", fold_bound(entry_at("training"), fold_inputs))
      param_set <- unit_step("finalize_parameters", finalize_bound(param_set, bound, cv_resamples))

    }

    ## -----------------------------------------------------------------------
    ## Step 2b: The folds, from each fold's prep
    ## -----------------------------------------------------------------------
    ## Once for the member: the re-tune's grid, its Bayesian iterations and the
    ## out-of-fold predictions all run on these folds. The warnings another
    ## package raised in a fold's prep join the member's with their folds.

    prepped_folds <- unit_step("fold_prep", assemble_folds(entry_at, cv_resamples, fold_inputs, role_map))
    warning_log   <- dplyr::bind_rows(warning_log, prepped_folds$warnings)

    rm(fold_inputs, entry_at)

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
    ## The search starts from evaluate()'s parameters and one neighbour per
    ## tuned parameter, or from a space-filling grid of grid_size points when
    ## there are none to use; how it started is recorded before it runs, so a
    ## re-tune that fails still says. The budget counts that start. A search
    ## space that costs no more fits than the start and the Bayesian search
    ## would make together (plsr's num_comp, one fit a fold) is gridded whole,
    ## with no Bayesian stage, as evaluate() grids it:
    ## tune's search errors inside its loop once it has no value left to
    ## propose, which would read as a failed search although every value was
    ## tried. final_bayesian_iter = 0 turns the re-tune off for a member with
    ## evaluate()'s parameters to keep, the whole space included, as
    ## tune::fit_best() refits a choice; a cold start still grids.

    warm_start <- usable_best_params(best_params_eval, param_set)

    if (warm_start && final_bayesian_iter == 0) {

      ## The re-tune off: evaluate()'s parameters, clamped, scored by the
      ## out-of-fold pass alone, with no starting grid and no search, so
      ## start_grid_size, bayes_iter and bayes_stop stay NA. Their .config is
      ## evaluate()'s, a record of where they came from.
      point        <- clamp_best_params(best_params_eval, param_set)
      best_params  <- dplyr::bind_cols(point, best_params_eval[setdiff(names(best_params_eval), names(point))])
      retune_moved <- FALSE

    } else {

      start           <- if (warm_start) warm_start_size(nrow(param_set)) else grid_size
      iters           <- search_budget(final_bayesian_iter, nrow(param_set), start)
      whole_space     <- exhaustive_grid(param_set, start + iters, MODEL_SPECS[[config_row$model]]$submodel)
      search_iter     <- if (is.null(whole_space)) iters else 0L
      initial_grid    <- unit_step("warm_start_tune", whole_space %||% with_unit_seed(
        seed, stage("warmstart-grid"),
        build_warmstart_grid(best_params_eval, param_set, max_points = start)
      ))
      start_grid_size <- nrow(initial_grid)

      ## evaluate()'s point, clamped, is the incumbent on the search and on the
      ## whole space alike: kept unless a candidate beats it by the margin
      retune <- unit_step("warm_start_tune", tune_warmstart_bayes(
        workflow      = model_wflow,
        cv_resamples  = prepped_folds$folds,
        initial_grid  = initial_grid,
        param_set     = param_set,
        incumbent     = if (warm_start) clamp_best_params(best_params_eval, param_set),
        bayesian_iter = search_iter,
        no_improve    = no_improve,
        metric_set    = tune_metrics,
        allow_par     = allow_par,
        debug         = debug,
        seed          = seed,
        config_id     = config_id
      ))

      best_params  <- retune$best_params
      bayes_iter   <- as.integer(retune$bayes_iter)
      bayes_stop   <- retune$bayes_stop
      retune_moved <- as.logical(retune$moved %||% NA)
      retune_gain  <- as.numeric(retune$gain %||% NA_real_)

      ## The warnings tune kept for the chosen candidate (#109), as
      ## evaluate_single_config() reads them: the re-tune returns the grid
      ## tune_grid() ran, which tune numbers its notes' model stages by, and a
      ## Bayesian result carries that grid's notes as iteration 0
      candidate <- unit_step("select_best", candidate_of(retune$tune_results, best_params,
                                                         retune$grid, model_wflow))

      ## The PLS engine's cap is left off the re-tune's notes: one fit serves
      ## every count of a grid stage, so they name the stage's largest count.
      ## The out-of-fold fits below are at the chosen count, and their own
      ## notes say where a fold capped it (pls_cap_records()).
      warning_log <- dplyr::bind_rows(
        warning_log,
        drop_pls_caps(tune_note_records(retune$tune_results, type = "warning", scope = "cv",
                                        candidate = candidate))
      )

      unmapped <- unmapped_line(candidate)

      if (!is.null(unmapped)) pin(unmapped)

      ## The candidates left out of the choice for a missing fold, the Bayesian
      ## search's dropped ones among them, as evaluate()'s row says (#101)
      left_out <- unit_step("select_best", incomplete_note(retune$tune_results, n_folds,
                                                           nrow(retune$grid), retune$n_dropped))

      if (!is.null(left_out)) pin(left_out)

      ## evaluate()'s point missed a fold of fit()'s, so it took no part (#101)
      if (isTRUE(retune_moved) && is.na(retune_gain)) {

        pin("evaluate()'s hyperparameters were not scored on every fold of fit()'s, so the re-tune's best candidate was taken.")

      }

    }

    ## -----------------------------------------------------------------------
    ## Step 5: Finalize workflow and generate OOF predictions
    ## -----------------------------------------------------------------------

    ## The model alone for the out-of-fold fits, on the prepped folds; the
    ## whole workflow for the final fit (Step 7)
    finalized_wf <- unit_step("finalize_workflow", tune::finalize_workflow(wflow, best_params))
    oof_wf       <- unit_step("finalize_workflow", tune::finalize_workflow(model_wflow, best_params))

    ## Messages muffled, as around the tuning calls: tune reports its notes
    ## on the console as it goes, and the member records them instead.
    cv_fit <- unit_step("oof_predictions", with_unit_seed(seed, stage("oof"), {
      results <- suppressMessages(tune::fit_resamples(
        seed_engine(oof_wf, unit_seed(seed, stage("oof"))),
        resamples = prepped_folds$folds,
        control   = tune::control_resamples(
          save_pred     = TRUE,
          allow_par     = allow_par,
          parallel_over = "resamples"
        )
      ))
      check_tune_result(results, "every out-of-fold fit failed", debug = debug)

      ## The fold rule (#101): the run is one candidate, so a fold it was not
      ## scored on fails the member rather than leaving its out-of-fold
      ## predictions without that fold's rows
      if (length(complete_configs(results, n_folds)) == 0) {

        abort_tune_failure(results, "out-of-fold predictions are missing for a fold")

      }

      results
    }))

    collect_notes_from(cv_fit)

    oof_raw <- unit_step("oof_predictions", tune::collect_predictions(cv_fit))

    ## Shape cv_predictions: .row, sample_id, .fold, config_id, .pred_trans,
    ## truth, then .pred below. .row is a position in the frame the folds were
    ## cut from (train_Fit in fit()), which the prepped folds keep, and
    ## sample_id is read from that same frame, so each prediction keeps its
    ## link to a sample and .row is never the only key.
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
      cv_predictions$.pred_trans, transformation, outcome_range = outcome_range
    ))

    ## -----------------------------------------------------------------------
    ## Step 6: Fold-wise CV metrics (original scale)
    ## -----------------------------------------------------------------------
    ## Inside the unit, so a warning yardstick raises here is the member's
    ## (#102). Each metric averages every fold or is NA, with its standard
    ## error over every fold, as evaluate()'s cross-validated metrics do: a
    ## mean over the folds a metric happened to be defined on (rsq on a fold
    ## whose predictions were constant) would compare unlike quantities.

    cv_metrics <- unit_step("cv_panel", {

      fold_metrics <- split(cv_predictions, cv_predictions$`.fold`) |>
        purrr::map_dfr(
          ~ compute_original_scale_metrics(.x$truth, .x$.pred),
          .id = "id"
        )

      over_every_fold(summarise_folds(fold_metrics, ".metric"), n_folds)

    })

    ## The row says which of the cross-validated metrics models$results shows
    ## are NA, and why, with the line evaluate()'s rows use
    shown <- cv_metrics[cv_metrics$.metric %in% c("rmse", "rpd"), , drop = FALSE]
    cv_na <- cv_na_line(shown$.metric[is.na(shown$mean)], nrow(shown))

    if (!is.null(cv_na)) pin(cv_na)

    ## -----------------------------------------------------------------------
    ## Step 7: Fit final model on full training data
    ## -----------------------------------------------------------------------

    final_fit <- unit_step("final_fit", with_unit_seed(seed, stage("final-fit"),
      suppressMessages(quiet_selection_warnings(
        workflows::fit(seed_engine(finalized_wf, unit_seed(seed, stage("final-fit"))), data = train_data)
      ))))

    ## What the final fit's selection step did, raised once, from its trained
    ## recipe, as evaluate() raises it
    unit_step("selection_record", selection_reports(final_fit))

    ## The PCA rotation cut to the components the member uses, before
    ## anything predicts with it, so the test metrics, the intervals, the
    ## applicability domain and the stored workflow all run on the cut
    ## recipe. The products bake() takes are unchanged.
    final_fit <- cut_pca_rotation(final_fit)

    ## -----------------------------------------------------------------------
    ## Step 8: Evaluate on test_F (original scale)
    ## -----------------------------------------------------------------------

    test_preds <- unit_step("test_predict", stats::predict(final_fit, new_data = test_data)$.pred)

    ## Back-transform, unconditionally (see the OOF block above, #53 and #76)
    test_preds <- unit_step("test_back_transform", back_transform_predictions(
      test_preds, transformation, outcome_range = outcome_range
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

    degradation <- unit_step("degradation", with_unit_seed(seed, stage("degradation"), check_degradation(
      cv_metrics, test_truth, test_preds, response_fences,
      min_n = min_degradation_rows
    )))

    ## A check that could not run leaves degraded NA, not FALSE, which would
    ## read as checked and fine; the row and fit()'s log say why
    if (!is.null(degradation$cause)) {

      cause <- degradation$cause
      warn_in_unit("skipped", "The degradation check did not run: {cause}.")

    }

    ## -----------------------------------------------------------------------
    ## Step 10: UQ (must run BEFORE butchering — needs the mold)
    ## -----------------------------------------------------------------------
    ## A unit of its own (#145): a failure leaves the member without intervals
    ## and names it in a skipped warning; a tripwire inside still stops the
    ## call.

    uq_result <- NULL

    if (compute_uq && !is.null(calib_data)) {

      ## Nothing in it draws before the quantile forest, so the stage is the
      ## forest's
      uq_result <- run_addon(with_unit_seed(seed, stage("forest"), fit_uq(
        fitted_workflow = final_fit,
        oof_predictions = cv_predictions,
        calib_data      = calib_data,
        role_map        = role_map,
        transformation  = transformation,
        level_default   = DEFAULT_UQ_LEVEL,
        seed            = unit_seed(seed, stage("forest")),
        outcome_range   = outcome_range,
        min_calib_rows  = min_calib_rows,
        debug           = debug
      )), "uq", skipped_add_on("Prediction intervals (UQ) were"))

      ## Measured coverage on the held-out test rows (#118). The intervals are
      ## built by predict_intervals(), the function predict() serves them with,
      ## so this is the coverage a user of the fitted object gets on data the
      ## model never saw. oof_coverage stays on the bundle under its own name:
      ## it is an in-sample diagnostic for the quantile forest, not coverage.
      if (!is.null(uq_result)) {

        ## The test rows' predictors, baked through the member's trained
        ## recipe as predict() bakes new rows; the bundle holds no recipe of
        ## its own.
        coverage <- run_addon({
          test_features <- unit_step("uq_bake", as.data.frame(recipes::bake(
            workflows::extract_recipe(final_fit, estimated = TRUE),
            new_data = test_data, recipes::all_predictors()
          )))
          test_interval_coverage(uq_result, test_preds, test_features, test_truth,
                                 config_id, outcome_range)
        }, "coverage", skipped_add_on("Measured test coverage was"))

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
      ), "ad", skipped_add_on("The applicability domain (AD) was"))

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
      bayes_iter       = bayes_iter,
      bayes_stop       = bayes_stop,
      retune_moved     = retune_moved,
      retune_gain      = retune_gain,
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
      start_grid_size  = start_grid_size,
      bayes_iter       = bayes_iter,
      bayes_stop       = bayes_stop,
      retune_moved     = retune_moved,
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
#' when [predict_intervals()] fails, at the step its failure names, the
#' figures stay `NA` and the member names it in a `skipped` warning.
#'
#' @param uq A UQ bundle from [fit_uq()].
#' @param test_preds Numeric. Back-transformed test predictions, original
#'   scale.
#' @param test_features Data frame. The test rows' predictors, baked through
#'   the member's trained recipe.
#' @param test_truth Numeric. The test outcomes, original scale.
#' @param config_id Character(1). Named in any warning.
#' @param outcome_range Numeric length 2. The bounds intervals are clamped to.
#' @return List with `test_coverage`, `test_mean_width` and `n_test`.
#' @keywords internal
#' @noRd
test_interval_coverage <- function(uq, test_preds, test_features, test_truth,
                                   config_id, outcome_range) {

  intervals <- predict_intervals(uq, test_preds, test_features,
                                 config_id     = config_id,
                                 outcome_range = outcome_range)

  interval_coverage(test_truth, intervals)

}

#' The measured coverage of a set of intervals
#'
#' The one scorer behind a member's and an ensemble's measured coverage:
#' rows with an outcome and both bounds are scored, and the rest are not
#' counted.
#'
#' @param truth Numeric. The outcomes, aligned to `intervals`' rows.
#' @param intervals Tibble with `.pred_lower`, `.pred_upper` and
#'   `.interval_width`.
#' @return List with `test_coverage`, `test_mean_width` and `n_test`.
#' @keywords internal
#' @noRd
interval_coverage <- function(truth, intervals) {

  scored  <- !is.na(truth) & !is.na(intervals$.pred_lower) &
    !is.na(intervals$.pred_upper)

  covered <- truth[scored] >= intervals$.pred_lower[scored] &
    truth[scored] <= intervals$.pred_upper[scored]

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
#' below the CV mean RPD (#119). The bootstrap draws from the stream as it
#' finds it; `fit_single_config()` runs it in the member's `degradation`
#' stage ([with_unit_seed()]).
#'
#' After a response trim (#77) the folds ran on rows inside the training
#' fences while the test rows are untrimmed, so with `response_fences` the
#' test rows compared are those inside the fences, and the reason says so;
#' the test metrics `fit()` reports are untouched.
#'
#' The check cannot run with fewer than `min_n` usable test rows (too few to
#' bootstrap), without a finite cross-validated RPD to compare with, with
#' constant test outcomes (whose RPD of 0 says nothing about the fit), or
#' with a test RPD that is not finite. Then `degraded` is `NA`, not `FALSE`,
#' and `cause` says which.
#'
#' @param cv_metrics Tibble with `.metric` and `mean`.
#' @param test_truth,test_preds Numeric. Test outcomes and back-transformed
#'   predictions, original scale.
#' @param response_fences Numeric `c(lower = , upper = )`, or `NULL` when no
#'   rows were trimmed.
#' @param n_boot Integer. Number of bootstrap resamples.
#' @param level Numeric. Coverage of the bootstrap interval.
#' @param min_n Integer. The fewest usable test rows the check bootstraps
#'   from; `control_horizons(min_degradation_rows = )`. Default
#'   `DEGRADATION_MIN_N`.
#' @return List with `degraded` (`TRUE`, `FALSE`, or `NA` when the check
#'   could not run), `reason` (character, the record of a `TRUE` flag, `NA`
#'   otherwise) and `cause` (character, why the check could not run; `NULL`
#'   when it ran).
#' @keywords internal
#' @noRd
check_degradation <- function(cv_metrics, test_truth, test_preds,
                              response_fences = NULL,
                              n_boot = DEGRADATION_BOOT_N,
                              level  = DEGRADATION_LEVEL,
                              min_n  = DEGRADATION_MIN_N) {

  not_flagged <- list(degraded = FALSE, reason = NA_character_, cause = NULL)
  cannot_run  <- function(cause) list(degraded = NA, reason = NA_character_, cause = cause)
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

  if (n < min_n) {

    return(cannot_run(sprintf(
      "the test set has %d usable row%s%s, fewer than the %d it bootstraps from (`min_degradation_rows`)",
      n, if (n == 1) "" else "s", fences_text %||% "", min_n
    )))

  }

  if (nrow(rpd_cv) != 1 || !is.finite(rpd_cv$mean)) {

    return(cannot_run("there is no finite cross-validated RPD to compare the test RPD with"))

  }

  ## Constant test outcomes give an RPD of 0 that says nothing about the fit
  if (stats::sd(truth) < .Machine$double.eps) {

    return(cannot_run("the test outcomes are constant, so their RPD says nothing about the fit"))

  }

  test_rpd <- rpd_vec(truth, preds)

  if (!is.finite(test_rpd)) return(cannot_run("the test RPD is not finite"))

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
    cause    = NULL,
    reason   = sprintf(
      "test RPD %.3f (%g%% bootstrap interval [%.3f, %.3f], %d test rows%s) is below the CV mean RPD %.3f",
      test_rpd, 100 * level, bounds[1], bounds[2], n, fences_text %||% "", rpd_cv$mean
    )
  )

}
