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
#' The configuration is one unit (`run_unit()`): a run failure in any step
#' ends it, and the row records the step (`UNIT_STEPS`), the cause, its root
#' class and, with `keep_traces`, its backtrace. A door, internal or
#' dependency error is not the configuration's and propagates.
#'
#' Each fold's recipe is prepped once. The training rows' spectra are
#' transformed once, with the transform step's own code (`hoist_spectra()`);
#' each fold's recipe, the configuration's without that step, is prepped on
#' the fold's analysis rows under a seed named by what is prepped
#' (`fold_prep_scope()`), as a unit of its own whose entry every
#' configuration of the same preprocessing and selection reads
#' (`prep_fold_entry()`); and tune scores every grid and Bayesian candidate
#' on the folds assembled from them with a workflow of the model alone
#' (`assemble_folds()`). A feature selection therefore selects once per
#' fold, for every candidate. The final fit, `last_fit()` on the split, runs
#' the whole workflow on the raw rows. A step that fails in a fold's prep
#' ends the configuration with the step's own class, and another package's
#' warning there reaches the row with the folds that raised it.
#'
#' @param config_row Single-row tibble from `config$configs`. Must contain:
#'   `config_id`, `model`, `transformation`, `preprocessing`,
#'   `feature_selection`.
#' @param split An `rsplit` object from `rsample::initial_split()`.
#' @param cv_folds A `vfold_cv` object created from the training set.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param grid_size Integer. Number of grid points for `tune_grid()`.
#' @param bayesian_iter Integer. Iterations for `tune_bayes()`, at least:
#'   raised for a model tuning several parameters so that the grid and the
#'   search try `BAYES_POINTS_PER_PARAM` points a parameter
#'   (`search_budget()`). Set to 0 to skip Bayesian optimization entirely.
#'   When every tuned parameter is a whole number and the space costs no
#'   more fits than the grid and those iterations together (a submodel's
#'   values sharing one fit, so plsr's whole range at any budget), the grid
#'   is every point and the Bayesian stage does not run
#'   (`exhaustive_grid()`).
#' @param tune_ranges Named list of `c(lower, upper)` by parameter id, in
#'   dials' units: the ranges the model's parameters are searched over, set
#'   before the data cap them (`apply_tune_ranges()`). `evaluate()` reads
#'   them from `configure()`'s `config$tuning$ranges`. Default `TUNE_RANGES`.
#' @param no_improve A whole number of at least 1, or `FALSE`: the Bayesian
#'   iterations without improvement after which the search stops
#'   (`control_horizons(no_improve = )`, which `evaluate()` reads). Default
#'   `BAYES_NO_IMPROVE_DEFAULT`.
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
#'   `tune::control_bayes()` to run the CV folds on the user's mirai
#'   daemons. `evaluate()` sets this from the resolved axis: `FALSE`
#'   on the configs axis (tune ships nothing inward), `TRUE` on the
#'   resamples axis.
#' @param parallel_over Character. tune's `parallel_over`, passed through to
#'   the control objects. `"resamples"` (default) ships one rsplit per task;
#'   `"everything"` ships the whole rset to every task. tune may rewrite the
#'   value before dispatch (parameter-free configs become `"resamples"`;
#'   single-split rsets become `"everything"`).
#' @param seed Integer. The run's seed, from which each stage's seed is
#'   derived with the stage's name (`with_unit_seed()`).
#' @param sg_window Odd integer. Savitzky-Golay window in grid points, passed
#'   to [build_recipe()]. `evaluate()` reads it from `configure()`'s
#'   `config$recipe`. Default 9.
#' @param pca_threshold Numeric. Variance share the `pca` feature selection
#'   keeps, passed to [build_recipe()]. `evaluate()` reads it from
#'   `configure()`'s `config$recipe`. Default 0.995.
#' @param outcome_range Numeric length-2 vector. The outcome's physical range,
#'   which the back-transformed test-set predictions (and, for a transformed
#'   response, the tuning predictions) are clamped to before scoring.
#'   `evaluate()` reads it from `configure()`'s `config$outcome_range`.
#'   Default `c(0, Inf)`.
#' @param debug Logical. Contain nothing: the first failure, inside tune
#'   included, stops the call with its backtrace, and the Bayesian search's
#'   failure is not fallen back from. Warnings are still recorded.
#' @param keep_traces Logical. Keep a failure's backtrace, as text, in the
#'   row's `trace`.
#' @param isolate Logical. Fit a model whose engine `MODEL_SPECS` marks
#'   `isolate` through its isolated twin, each fit in a process of its own
#'   (`model_engine()`); `control_horizons(isolate = )`. Default `TRUE`.
#' @param outside_warnings Character. Warnings the caller caught outside the
#'   unit before it ran, as `condition_summary()` lines: the configs-axis
#'   worker's, from loading the configuration's packages and rebuilding its
#'   folds. They are rendered with the row's own, deduplicated and capped as
#'   one list (#96).
#' @param fold_entries The entries of the configuration's fold preps, as
#'   `evaluate()` built them (`prep_fold_entry()`): a list with `training`
#'   (the whole training part's, for a model whose parameters are bounded by
#'   the predictors kept, or `NULL`) and `folds` (one per fold of `cv_folds`,
#'   in order, or `NULL` with no feature selection). `NULL` (default) builds
#'   them here, with the same code.
#'
#' @return Single-row tibble with columns: `config_id`, `status` (`"pruned"`
#'   means Bayesian refinement was skipped), `below_prune_threshold` (logical;
#'   `NA` when `prune = FALSE`), `prune_threshold` (the threshold that reading
#'   was taken against; `NA` when `prune = FALSE`), `bayes_iter` and
#'   `bayes_stop` (the iterations the Bayesian search began, and why it
#'   ended: `"no_improve"`, `"budget"` or `"failed"`; both `NA` when no
#'   search ran), the six
#'   test-set metrics `rmse`, `rrmse`, `rsq`, `ccc`, `rpd`, `mae`, the six
#'   cross-validated means at the selected hyperparameters `cv_rmse`,
#'   `cv_rrmse`, `cv_rsq`, `cv_ccc`, `cv_rpd`, `cv_mae` (what `evaluate()` and
#'   `fit()` rank on), `best_params` (list-column), `error_message` (the
#'   failed step and the cause), `class` (the failure's root class), `trace`
#'   (list-column: the backtrace as text under `keep_traces`, else `NULL`),
#'   `warnings`, `runtime_secs`. The unit's records ride on the row as the
#'   attribute `"unit_records"`, for `evaluate()` to lift its horizons
#'   warnings onto the log.
#'
#' @keywords internal
evaluate_single_config <- function(config_row,
                                   split,
                                   cv_folds,
                                   role_map,
                                   grid_size       = DEFAULT_GRID_SIZE,
                                   bayesian_iter   = DEFAULT_BAYES_ITER,
                                   tune_ranges     = TUNE_RANGES,
                                   no_improve      = BAYES_NO_IMPROVE_DEFAULT,
                                   prune           = FALSE,
                                   prune_threshold = 1.0,
                                   allow_par       = FALSE,
                                   parallel_over   = "resamples",
                                   seed,
                                   sg_window       = DEFAULT_SG_WINDOW,
                                   pca_threshold   = DEFAULT_PCA_THRESHOLD,
                                   outcome_range   = DEFAULT_OUTCOME_RANGE,
                                   debug           = FALSE,
                                   keep_traces     = FALSE,
                                   isolate         = TRUE,
                                   outside_warnings = character(0),
                                   fold_entries     = NULL) {

  start_time <- Sys.time()

  parallel_over <- rlang::arg_match0(parallel_over, c("resamples", "everything"),
                                     arg_nm = "parallel_over")

  config_id      <- config_row$config_id
  outcome_col    <- role_map$variable[role_map$role == "outcome"]
  transformation <- tolower(as.character(config_row$transformation))
  n_folds        <- nrow(cv_folds)

  ## The training rows, read from the folds rather than copied out of the
  ## split again (#116): evaluate() cuts its folds from the training part,
  ## and its configs-axis worker rebuilds them from it, so this is that
  ## frame, one copy.
  train_data     <- cv_folds$splits[[1]]$data

  ## The row's warnings, as records that render_warning_log() reduces to one
  ## line per distinct message (#96): tune's notes and the package's own
  ## lines, gathered as the unit runs, and, once it returns, the warnings the
  ## unit caught as they were raised.
  warning_log <- new_warning_log()

  ## Each stage that draws is a unit of its own, named under the
  ## configuration's id (with_unit_seed()), so its draws depend on the run's
  ## seed and the stage alone: not on the stages before it, the order of the
  ## configurations, or the worker that runs them. The engines that take a
  ## seed get the stage's (seed_engine()).
  stage <- function(name) c("evaluate", config_id, name)

  ## tune catches the warnings raised inside its resampling (an engine's
  ## warning during a fit, say) and keeps them in .notes, where run_unit()
  ## never sees them.
  collect_notes_from <- function(tune_results, scope = "cv", candidate = NULL) {
    notes <- tune_note_records(tune_results, type = "warning", scope = scope,
                               candidate = candidate)
    warning_log <<- dplyr::bind_rows(warning_log, notes)
  }

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
    ## The folds are assembled over the transformed rows from each fold's
    ## prep (fold_prep_inputs(), assemble_folds()). tune searches a workflow
    ## of the model alone, its outcome and predictors the columns each fold
    ## holds; `wflow` stays for the parameter set and the final fit.

    fold_inputs <- fold_prep_inputs(config_row, train_data, role_map, sg_window = sg_window)

    model_wflow <- unit_step("workflow", fold_workflow(model_spec, outcome_col))

    ## -----------------------------------------------------------------------
    ## Step 2: Finalize parameter set (the ranges, then the mtry and num_comp
    ## upper bounds)
    ## -----------------------------------------------------------------------
    ## The configured ranges come first (configure(tune_ranges = )), so the
    ## bounds fill only an upper bound still unknown and the data cap narrows
    ## a range rather than replacing it. Models with mtry (rf, xgboost,
    ## lightgbm) need the upper bound set from the actual predictor count
    ## after recipe preprocessing. Without this, tune_grid() may sample mtry
    ## values larger than the number of predictors. plsr's num_comp is capped
    ## by the same count and by the smallest fold's rows (#216).
    ##
    ## The prep rule: each fold's recipe is prepped once, before tuning, and
    ## every candidate the grid and the Bayesian search try is scored on it.
    ## The bounds are read from one more prep, of the fold recipe on the whole
    ## training part. A step's error in either ends the configuration with its
    ## class; tune preps only inside the final fit (last_fit()).

    param_set   <- workflows::extract_parameter_set_dials(wflow)
    param_set   <- unit_step("finalize_parameters", apply_tune_ranges(param_set, tune_ranges))
    prepped_now <- bounded_model(config_row$model)

    ## The configuration's fold preps: the run's entries, or, called on its
    ## own, entries built here by the run's code as they are read
    entry_at <- fold_entry_reader(
      fold_entries,
      config_fold_units("evaluate", config_row, cv_folds, role_map, data = NULL, seed = seed,
                        sg_window = sg_window, pca_threshold = pca_threshold, bounded = prepped_now),
      function(unit, rows) prep_fold_entry(unit, fold_inputs$data, rows, role_map, seed,
                                           sg_window, pca_threshold, debug = debug,
                                           keep_traces = keep_traces),
      cv_folds
    )

    if (prepped_now) {

      ## The training part's prep sets the bounds, and is not the model
      ## returned, so the selection steps' warnings are not taken from it
      ## (the final fit's record is read instead, below)
      bound     <- unit_step("prep", fold_bound(entry_at("training"), fold_inputs))
      param_set <- unit_step("finalize_parameters", finalize_bound(param_set, bound, cv_folds))

    }

    ## -----------------------------------------------------------------------
    ## Step 2b: The folds, from each fold's prep
    ## -----------------------------------------------------------------------
    ## The warnings another package raised in a fold's prep join the row with
    ## the folds that raised them, as tune's notes did.

    prepped_folds <- unit_step("fold_prep", assemble_folds(entry_at, cv_folds, fold_inputs, role_map))
    warning_log   <- dplyr::bind_rows(warning_log, prepped_folds$warnings)

    rm(fold_inputs, entry_at)

    ## -----------------------------------------------------------------------
    ## Step 3: Define tuning metric set
    ## -----------------------------------------------------------------------

    ## Scored on the original response scale. The response transform is a
    ## skip = TRUE recipe step, so tune never applies it to the assessment set;
    ## tuning_metric_set() back-transforms the estimate inside each metric so
    ## select_best(), tune_bayes() and the prune gate all see the same scale the
    ## leaderboard reports. rmse stays first: tune_bayes() optimises the first
    ## metric in the set. See #49. The back-transform clamps to the outcome's
    ## range, as the test-set scoring below does (#76).
    tune_metrics <- tuning_metric_set(transformation, outcome_range = outcome_range)

    ## -----------------------------------------------------------------------
    ## Step 4: Grid search
    ## -----------------------------------------------------------------------
    ## A search space that costs no more fits than the grid and the Bayesian
    ## search would make together is gridded whole, and there is no Bayesian
    ## stage: tune's search runs out of values to propose and errors inside
    ## its loop, which would read as a failed search although every value
    ## was tried. A submodel's values share one fit, so plsr's whole range of
    ## components is one fit a fold (exhaustive_grid()).

    ## The iterations asked for are a floor, raised to the points a tuned
    ## parameter a search gives (search_budget())
    iters       <- search_budget(bayesian_iter, nrow(param_set), grid_size)
    whole_space <- exhaustive_grid(param_set, grid_size + iters, MODEL_SPECS[[config_row$model]]$submodel)
    search_iter <- if (is.null(whole_space)) iters else 0L

    grid_results <- unit_step("tune_grid", {

      ## Drawn here rather than by tune_grid(), with the same draw: tune
      ## numbers a note's model stage in the order of this grid, which is how
      ## a configuration's warnings are read for the candidate it chose
      ## (#109; candidate_of()).
      grid <- whole_space %||% with_unit_seed(seed, stage("grid"),
        tune::.check_grid(grid = grid_size, workflow = model_wflow, pset = param_set))

      results <- with_unit_seed(seed, stage("tune"), {

        results <- suppressMessages(
          tune::tune_grid(
            object     = seed_engine(model_wflow, unit_seed(seed, stage("tune"))),
            resamples  = prepped_folds$folds,
            grid       = grid,
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
        )

        check_tune_result(results, "all models failed during CV", debug = debug)

        results

      })

      ## A candidate scored on fewer folds than were drawn takes no part in
      ## the prune gate or the choice of hyperparameters (#101), so a grid
      ## with none scored on every fold has nothing to choose from.
      if (nrow(rank_complete(results, "rmse", n_folds, debug)) == 0) {

        abort_tune_failure(results, "no grid candidate was scored on every fold")

      }

      results

    })

    gc(verbose = FALSE)

    ## -----------------------------------------------------------------------
    ## Step 5: Prune check
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

      best_grid <- unit_step("prune", rank_complete(grid_results, "rpd", n_folds, debug))

      below_prune_threshold <- !is.finite(best_grid$mean[1]) ||
        best_grid$mean[1] < prune_threshold

      skip_bayesian <- below_prune_threshold && search_iter > 0

    }

    ## -----------------------------------------------------------------------
    ## Step 6: Bayesian optimization
    ## -----------------------------------------------------------------------
    ## A search that fails keeps what it finished: the iterations that ran,
    ## or the grid's choice when none did, and says so with a substitution
    ## warning that the row shows and evaluate() puts on the log (#209). That
    ## fallback is containment, so the kinds no unit contains still
    ## propagate, and under debug nothing is fallen back from. The search and
    ## what it keeps are bayes_search()'s, which fit()'s re-tune shares.

    final_tune_results <- grid_results
    n_dropped          <- 0L
    bayes_iter         <- NA_integer_
    bayes_stop         <- NA_character_

    if (!skip_bayesian && search_iter > 0) {

      bayes_seed <- unit_seed(seed, stage("bayes"))

      search <- unit_step("tune_bayes", with_unit_seed(seed, stage("bayes"), bayes_search(
        workflow      = seed_engine(model_wflow, bayes_seed),
        resamples     = prepped_folds$folds,
        initial       = grid_results,
        iter          = search_iter,
        param_info    = param_set,
        metrics       = tune_metrics,
        seed          = bayes_seed,
        no_improve    = no_improve,
        allow_par     = allow_par,
        parallel_over = parallel_over,
        debug         = debug
      )))

      final_tune_results <- search$results
      n_dropped          <- search$n_dropped
      bayes_iter         <- search$n_iter
      bayes_stop         <- search$stop

      gc(verbose = FALSE)

    }

    ## -----------------------------------------------------------------------
    ## Step 7: Select best hyperparameters, and the CV panel there
    ## -----------------------------------------------------------------------
    ## Chosen as tune::select_best() chooses, among the candidates scored on
    ## every fold (#101); the row says how many were left out, and why. The
    ## cross-validated means tune scored for the chosen config, on the
    ## original scale via tuning_metric_set(), are what evaluate() and fit()
    ## rank on rather than the test-set metrics computed below, so the test
    ## set stays held out from selection and its metrics are honest (#50). A
    ## mean is kept only when it covers every fold, so every ranked value is
    ## on the same support.

    best_params <- unit_step("select_best",
                             select_complete(final_tune_results, "rmse", n_folds, debug))

    ## The warnings tune kept for the chosen candidate (#109), read once from
    ## whichever result is kept: a tune_bayes() result carries its initial
    ## grid's notes as .iter 0, so this covers both stages.
    candidate <- unit_step("select_best", candidate_of(final_tune_results, best_params, grid, model_wflow))

    ## The PLS engine's cap is read for the chosen count, fold by fold: one
    ## fit serves every count of a grid stage, so the notes tune kept of it
    ## name the stage's largest count (pls_cap_records())
    warning_log <- dplyr::bind_rows(
      warning_log,
      drop_pls_caps(tune_note_records(final_tune_results, type = "warning", candidate = candidate)),
      pls_cap_records(prepped_folds$folds, best_params)
    )

    unmapped <- unmapped_line(candidate)

    if (!is.null(unmapped)) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(unmapped, pinned = TRUE))

    }

    left_out <- unit_step("select_best", incomplete_note(final_tune_results, n_folds, nrow(grid), n_dropped))

    if (!is.null(left_out)) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(left_out, pinned = TRUE))

    }

    cv_panel <- unit_step("cv_panel", cv_panel_at(final_tune_results, best_params, n_folds))
    cv_na    <- cv_na_line(sub("^cv_", "", names(cv_panel)[is.na(unlist(cv_panel))]),
                           length(cv_panel))

    if (!is.null(cv_na)) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(cv_na, pinned = TRUE))

    }

    ## -----------------------------------------------------------------------
    ## Step 8: Finalize workflow and fit it on the training part, scoring the
    ## held-out test set
    ## -----------------------------------------------------------------------

    final_wflow <- unit_step("finalize_workflow",
                             seed_engine(tune::finalize_workflow(wflow, best_params),
                                         unit_seed(seed, stage("final-fit"))))

    ## Messages muffled, as around the two tuning calls: tune reports its
    ## notes on the console as it goes, and the row records them instead.
    ## A final fit that fails inside last_fit() fails the configuration
    ## (#102): fit() would otherwise choose it and fail its final fit again.
    ## last_fit() is the one place tune preps the recipe, so a step's failure
    ## there is reproduced outside it, with its class (check_tune_result()),
    ## unless the bounds prep has already prepped it on the same rows.
    last_fit_obj <- unit_step("last_fit", with_unit_seed(seed, stage("final-fit"), {
      results <- suppressMessages(tune::last_fit(final_wflow, split = split))
      check_tune_result(results, "the final fit failed",
                        recipe = if (!prepped_now) recipe, debug = debug)
      results
    }))
    collect_notes_from(last_fit_obj, scope = "test set")

    ## What the final fit's selection step did, read from its trained recipe:
    ## inside last_fit() its warning reached tune's notes only as text, so it
    ## is raised again here, with its class, for the log
    unit_step("selection_record", selection_reports(tune::extract_workflow(last_fit_obj)))

    test_predictions <- unit_step("last_fit", tune::collect_predictions(last_fit_obj))

    ## -----------------------------------------------------------------------
    ## Step 9: Back-transform the predictions and score them
    ## -----------------------------------------------------------------------

    ## Unconditional, like predict(): the "none" branch is a passthrough, and
    ## the clamp to the outcome's range inside back_transform_predictions()
    ## must apply to every transformation so evaluation scores what deploy
    ## serves (#53, #76).
    test_predictions$.pred <- unit_step("back_transform", back_transform_predictions(
      test_predictions$.pred, transformation, outcome_range = outcome_range
    ))

    ## Six NAs when fewer than two test rows have a prediction, and the row
    ## says why
    scored <- unit_step("test_metrics", compute_original_scale_metrics(
      truth    = test_predictions[[outcome_col]],
      estimate = test_predictions$.pred
    ))

    test_metrics <- metrics_row(scored)
    na_reason    <- metrics_na_reason(scored)

    if (!is.null(na_reason)) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(na_reason, pinned = TRUE))

    }

    tibble::tibble(
      config_id     = config_id,
      status        = if (skip_bayesian) "pruned" else "success",
      below_prune_threshold = below_prune_threshold,
      prune_threshold       = if (prune) as.numeric(prune_threshold) else NA_real_,
      bayes_iter            = as.integer(bayes_iter),
      bayes_stop            = bayes_stop,
      rmse          = test_metrics$rmse,
      rrmse         = test_metrics$rrmse,
      rsq           = test_metrics$rsq,
      ccc           = test_metrics$ccc,
      rpd           = test_metrics$rpd,
      mae           = test_metrics$mae,
      cv_rmse       = cv_panel$cv_rmse,
      cv_rrmse      = cv_panel$cv_rrmse,
      cv_rsq        = cv_panel$cv_rsq,
      cv_ccc        = cv_panel$cv_ccc,
      cv_rpd        = cv_panel$cv_rpd,
      cv_mae        = cv_panel$cv_mae,
      scoring_schema = SCORING_SCHEMA,
      best_params   = list(best_params),
      error_message = NA_character_,
      class         = NA_character_,
      trace         = list(NULL),
      warnings      = list(NULL),
      runtime_secs  = NA_real_
    )

  }, unit = "configuration", id = config_id, debug = debug, keep_traces = keep_traces)

  ## -------------------------------------------------------------------------
  ## Step 10: The row, succeeded or failed
  ## -------------------------------------------------------------------------
  ## The warnings the unit caught join the notes, so a warning caught as it
  ## was raised and the same warning in tune's notes are one line, and so do
  ## the caller's from outside the unit.

  warning_log <- dplyr::bind_rows(unit_warning_records(unit$records), warning_log,
                                  text_records(outside_warnings))

  row <- if (unit$failed) {

    failure <- unit$records[unit$records$type == "failure", , drop = FALSE]

    failed <- create_failed_result(config_id, unit_failure_message(unit$records, "Evaluation"))
    failed$class <- failure$class[1]
    failed$trace <- failure$trace[1]
    failed

  } else {

    unit$value

  }

  row$warnings     <- list(render_warning_log(warning_log))
  row$runtime_secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  attr(row, "unit_records") <- unit$records

  row

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
#' measurements at role `response_hold`; they survive into the baked frame
#' and are not predictors, so subtraction counts them and the ceiling lands
#' above the real predictor count. `tune_grid()` can then sample an `mtry`
#' larger than the model matrix is wide, which fails the config for a reason
#' that has nothing to do with the config.
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
#' Returns the mean of each metric at the `.config` that was chosen, over
#' that candidate's own folds ([config_metrics()]). These are the `cv_*`
#' columns of `evaluation$results`, the quantities `evaluate()` and `fit()`
#' rank on. A value that is missing or not finite is `NA_real_`, and so is
#' every value when there is nothing to look up; a failure to read the
#' metrics is the configuration's failure, at its `cv_panel` step. With
#' `n_folds`, a mean over fewer folds than that is `NA_real_` too (an `rsq`
#' undefined on a fold whose predictions were constant, say), so every value
#' ranked covers every fold.
#'
#' @param tune_results A `tune_results` object (grid or Bayesian).
#' @param best_params One-row tibble of the chosen hyperparameters, carrying
#'   `.config`.
#' @param n_folds Integer or `NULL`. The folds drawn; `NULL` keeps a mean
#'   over any number of them.
#' @param metrics Metric names to look up, in order.
#' @return One-row tibble with columns `cv_<metric>`.
#' @keywords internal
#' @noRd
cv_panel_at <- function(tune_results, best_params, n_folds = NULL,
                        metrics = c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")) {

  out <- stats::setNames(as.list(rep(NA_real_, length(metrics))),
                         paste0("cv_", metrics))

  usable <- !is.null(tune_results) && !is.null(best_params) &&
    ".config" %in% names(best_params) && nrow(best_params) >= 1

  if (!usable) return(tibble::as_tibble(out))

  panel <- config_metrics(tune_results)
  panel <- panel[panel$.config == best_params$.config[[1]], , drop = FALSE]

  if (!is.null(n_folds)) panel <- over_every_fold(panel, n_folds)

  for (m in metrics) {

    v <- panel$mean[panel$.metric == m]

    if (length(v) == 1 && is.finite(v)) out[[paste0("cv_", m)]] <- v

  }

  tibble::as_tibble(out)

}
