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
#' ends it, and the row records the step (`EVAL_STEPS`), the cause, its root
#' class and, with `keep_traces`, its backtrace. A door, internal or
#' dependency error is not the configuration's and propagates.
#'
#' @param config_row Single-row tibble from `config$configs`. Must contain:
#'   `config_id`, `model`, `transformation`, `preprocessing`,
#'   `feature_selection`.
#' @param split An `rsplit` object from `rsample::initial_split()`.
#' @param cv_folds A `vfold_cv` object created from the training set.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param grid_size Integer. Number of grid points for `tune_grid()`.
#' @param bayesian_iter Integer. Iterations for `tune_bayes()`. Set to 0 to
#'   skip Bayesian optimization entirely. When every tuned parameter is a
#'   whole number and the space has no more points than `grid_size +
#'   bayesian_iter`, the grid is every point and the Bayesian stage does not
#'   run (`exhaustive_grid()`).
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
#'
#' @return Single-row tibble with columns: `config_id`, `status` (`"pruned"`
#'   means Bayesian refinement was skipped), `below_prune_threshold` (logical;
#'   `NA` when `prune = FALSE`), `prune_threshold` (the threshold that reading
#'   was taken against; `NA` when `prune = FALSE`), the six
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
                                   prune           = FALSE,
                                   prune_threshold = 1.0,
                                   allow_par       = FALSE,
                                   parallel_over   = "resamples",
                                   seed            = 42L,
                                   sg_window       = DEFAULT_SG_WINDOW,
                                   pca_threshold   = DEFAULT_PCA_THRESHOLD,
                                   outcome_range   = DEFAULT_OUTCOME_RANGE,
                                   debug           = FALSE,
                                   keep_traces     = FALSE) {

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
  n_folds        <- nrow(cv_folds)

  ## The row's warnings, as records that render_warning_log() reduces to one
  ## line per distinct message (#96): tune's notes and the package's own
  ## lines, gathered as the unit runs, and, once it returns, the warnings the
  ## unit caught as they were raised.
  warning_log <- new_warning_log()

  ## tune catches the warnings raised inside its resampling (a recipe step
  ## warning during prep, say) and keeps them in .notes, where run_unit()
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

    model_spec <- unit_step("model_spec", define_model_spec(config_row$model))

    wflow <- unit_step("workflow", workflows::workflow() |>
                         workflows::add_recipe(recipe) |>
                         workflows::add_model(model_spec))

    ## -----------------------------------------------------------------------
    ## Step 2: Finalize parameter set (mtry and num_comp upper bounds)
    ## -----------------------------------------------------------------------
    ## Models with mtry (rf, xgboost, lightgbm) need the upper bound set from
    ## the actual predictor count after recipe preprocessing. Without this,
    ## tune_grid() may sample mtry values larger than the number of predictors.
    ## plsr's num_comp is capped by the same count and by the smallest fold's
    ## rows (#216).
    ##
    ## That prep is the only one before tuning, since it costs nothing extra:
    ## a step's error there ends the configuration with its class. Every
    ## other model's recipe is prepped only when tune's notes show a step
    ## failed (check_tune_result()), since a prep with feature selection can
    ## take minutes (CARS on 2,000 rows took over five) and would otherwise
    ## be paid by every configuration that does not fail.

    param_set   <- workflows::extract_parameter_set_dials(wflow)
    prepped_now <- any(c("mtry", "num_comp") %in% param_set$name)

    if (prepped_now) {

      prepped <- unit_step("prep", recipes::prep(recipe))
      baked   <- unit_step("prep", recipes::bake(prepped, new_data = NULL))

      param_set <- unit_step("finalize_parameters", {
        eval_data <- baked[, prepped_predictors(prepped, baked), drop = FALSE]
        finalized <- dials::finalize(param_set, eval_data)
        cap_pls_components(finalized, ncol(eval_data), min_analysis_rows(cv_folds))
      })

      rm(prepped, baked)
      invisible(gc(verbose = FALSE))

    }

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
    ## A search space with no more points than the grid and the Bayesian
    ## search would try together (plsr's num_comp on a short spectrum) is
    ## gridded whole, and there is no Bayesian stage: tune's search runs out
    ## of values to propose and errors inside its loop, which would read as a
    ## failed search although every value was tried.

    whole_space <- exhaustive_grid(param_set, grid_size + bayesian_iter)
    search_iter <- if (is.null(whole_space)) bayesian_iter else 0L

    grid_results <- unit_step("tune_grid", {

      ## Drawn here rather than by tune_grid(), with the same draw: tune
      ## numbers a note's model stage in the order of this grid, which is how
      ## a configuration's warnings are read for the candidate it chose
      ## (#109; candidate_of()).
      grid <- whole_space %||% tune::.check_grid(grid = grid_size, workflow = wflow, pset = param_set)

      results <- suppressMessages(
        tune::tune_grid(
          object     = wflow,
          resamples  = cv_folds,
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

      check_tune_result(results, "all models failed during CV",
                        recipe = if (!prepped_now) recipe, debug = debug)

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
    ## propagate, and under debug nothing is fallen back from.
    ##
    ## tune recovers from an error inside its search loop itself: an
    ## on.exit() returns the iterations finished so far, and that return
    ## cancels the unwind of any handler outside it. So the first error to
    ## leave tune's own catches is recorded as it is raised, with its trace,
    ## and what to do about it is decided once tune_bayes() has returned. A
    ## proposed candidate that fails in every fold leaves no note and raises
    ## nothing: tune drops it and says only "All models failed for: ..." in
    ## its iteration log, which is read under debug, so that debug stops
    ## there too. A worker that dies during an iteration on the resamples
    ## axis is caught by tune's try() around each fit and is never raised
    ## here; it reads as a candidate that failed.

    final_tune_results <- grid_results

    if (!skip_bayesian && search_iter > 0) {

      ## Re-pin before every stochastic stage. tune's future path advances the
      ## parent stream past where the sequential loop leaves it (future_lapply()
      ## draws worker seeds from it), so a stage seeded only by what came before
      ## would differ between allow_par = TRUE and FALSE. Pinning here makes
      ## the axis a pure performance choice (review finding, 2026-09-15).
      set.seed(seed, kind = "Mersenne-Twister")

      unit_step("tune_bayes", {

        stopped    <- NULL
        all_failed <- NULL

        read_log <- function(m) {
          text <- cli::ansi_strip(conditionMessage(m))
          if (is.null(all_failed) && grepl("All models failed for:", text, fixed = TRUE)) {
            all_failed <<- trimws(sub("^.*?(All models failed for:)", "\\1", text, perl = TRUE))
          }
        }

        bayes_results <- tryCatch(
          withCallingHandlers(
            suppressMessages(withCallingHandlers(
              tune::tune_bayes(
                object     = wflow,
                resamples  = cv_folds,
                initial    = grid_results,
                iter       = search_iter,
                param_info = param_set,
                metrics    = tune_metrics,
                control    = tune::control_bayes(
                  save_pred     = FALSE,
                  save_workflow = FALSE,
                  verbose       = FALSE,
                  verbose_iter  = debug,
                  no_improve    = BAYES_NO_IMPROVE_LIMIT,
                  allow_par     = allow_par,
                  parallel_over = parallel_over
                )
              ),
              message = read_log
            )),
            error = function(e) if (is.null(stopped)) stopped <<- rlang::cnd_entrace(e)
          ),
          ## An error tune did not recover from (before its loop starts):
          ## recorded above, nothing to keep
          error = function(e) NULL
        )

        if (!is.null(stopped) && (debug || rlang::cnd_inherits(stopped, NEVER_CONTAINED))) {

          stop(stopped)

        }

        if (debug && !is.null(all_failed)) {

          abort_horizons(
            "a candidate the Bayesian search proposed failed in every fold, and tune keeps no record of why: {all_failed}",
            kind = "run"
          )

        }

        if (!is.null(bayes_results)) check_tune_result(bayes_results, failure = NULL, debug = debug)

        cause  <- if (!is.null(stopped)) condition_summary(stopped)
        n_iter <- if (!is.null(bayes_results) && bayes_iterations_ran(bayes_results)) {
          max(bayes_results$.iter, na.rm = TRUE)
        } else 0L

        if (n_iter > 0) {

          ## Candidates that failed in some folds leave their reasons in .notes,
          ## which collect_notes_from() reads below.
          final_tune_results <- bayes_results

          if (!is.null(cause)) {

            warn_in_unit("substitution",
                         "The Bayesian search stopped early, after {n_iter} iteration{?s}, so the hyperparameters were chosen from those and the grid: {cause}")

          }

        } else if (!is.null(cause)) {

          warn_in_unit("substitution",
                       "The Bayesian search failed, so the hyperparameters were chosen from the grid alone: {cause}")

        } else {

          ## The search returned without an iteration past the grid: every
          ## candidate failed in every fold, which tune drops without a note.
          warn_in_unit("substitution",
                       "The Bayesian search produced no results, so the hyperparameters were chosen from the grid alone.")

        }

      })

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
    candidate <- unit_step("select_best", candidate_of(final_tune_results, best_params, grid, wflow))
    collect_notes_from(final_tune_results, candidate = candidate)

    if (!is.null(candidate$unmapped)) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(paste0(
        "The warnings of every grid candidate are shown, not only the chosen one's: ",
        candidate$unmapped, "."
      ), pinned = TRUE))

    }

    left_out <- unit_step("select_best", incomplete_note(final_tune_results, n_folds))

    if (!is.null(left_out)) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(left_out, pinned = TRUE))

    }

    cv_panel <- unit_step("cv_panel", cv_panel_at(final_tune_results, best_params, n_folds))
    cv_na    <- names(cv_panel)[is.na(unlist(cv_panel))]

    if (length(cv_na) == length(cv_panel)) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(
        "CV metrics at the selected hyperparameters could not be recovered; cv_* columns are NA.",
        pinned = TRUE
      ))

    } else if (length(cv_na) > 0) {

      warning_log <- dplyr::bind_rows(warning_log, text_records(paste0(
        paste(cv_na, collapse = ", "), if (length(cv_na) == 1) " is" else " are",
        " NA: the selected hyperparameters were not scored on every fold for ",
        if (length(cv_na) == 1) "it" else "them",
        ", and a cross-validated mean is kept only over every fold."
      ), pinned = TRUE))

    }

    ## -----------------------------------------------------------------------
    ## Step 8: Finalize workflow and fit it on the training part, scoring the
    ## held-out test set
    ## -----------------------------------------------------------------------

    final_wflow <- unit_step("finalize_workflow", tune::finalize_workflow(wflow, best_params))

    ## Re-pin: last_fit() draws the engine seed from the parent stream, whose
    ## position now depends on which axis the tuning ran on (see Step 6).
    set.seed(seed, kind = "Mersenne-Twister")

    ## Messages muffled, as around the two tuning calls: tune reports its
    ## notes on the console as it goes, and the row records them instead.
    ## A final fit that fails inside last_fit() fails the configuration
    ## (#102): fit() would otherwise choose it and fail its final fit again.
    last_fit_obj <- unit_step("last_fit", {
      results <- suppressMessages(tune::last_fit(final_wflow, split = split))
      check_tune_result(results, "the final fit failed",
                        recipe = if (!prepped_now) recipe, debug = debug)
      results
    })
    collect_notes_from(last_fit_obj, scope = "test set")

    test_predictions <- unit_step("last_fit", tune::collect_predictions(last_fit_obj))

    ## -----------------------------------------------------------------------
    ## Step 9: Back-transform the predictions and score them
    ## -----------------------------------------------------------------------

    ## Unconditional, like predict(): the "none" branch is a passthrough, and
    ## the clamp to the outcome's range inside back_transform_predictions()
    ## must apply to every transformation so evaluation scores what deploy
    ## serves (#53, #76).
    test_predictions$.pred <- unit_step("back_transform", back_transform_predictions(
      test_predictions$.pred, transformation, warn = FALSE, outcome_range = outcome_range
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
  ## was raised and the same warning in tune's notes are one line.

  warning_log <- dplyr::bind_rows(unit_warning_records(unit$records), warning_log)

  row <- if (unit$failed) {

    failure <- unit$records[unit$records$type == "failure", , drop = FALSE]
    step    <- failure$step[1]

    failed <- create_failed_result(config_id, paste0(
      if (is.na(step)) "Evaluation" else EVAL_STEPS[[step]], " failed: ", failure$message[1]
    ))
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

#' The steps of a configuration's unit, and how a failed row names each
#'
#' @description
#' The [unit_step()] labels inside [evaluate_single_config()], each with the
#' words its failed row's `error_message` starts with ("Grid search failed:
#' ..."). A failure outside every step reads "Evaluation failed: ...".
#'
#' @keywords internal
#' @noRd
EVAL_STEPS <- c(
  recipe              = "Recipe building",
  model_spec          = "Model specification",
  workflow            = "Workflow creation",
  prep                = "Recipe preparation",
  finalize_parameters = "Parameter finalization",
  tune_grid           = "Grid search",
  prune               = "Prune check",
  tune_bayes          = "Bayesian search",
  select_best         = "Parameter selection",
  cv_panel            = "Cross-validated metrics",
  finalize_workflow   = "Workflow finalization",
  last_fit            = "Test evaluation",
  back_transform      = "Back-transformation",
  test_metrics        = "Test metrics"
)

## ---------------------------------------------------------------------------
## exhaustive_grid — a search space small enough to try whole
## ---------------------------------------------------------------------------

#' Every point of a search space that fits a budget
#'
#' @description
#' When every tuned parameter takes whole-number values over a known range
#' and the space has no more points than `budget`, the whole space as a grid;
#' otherwise `NULL`. A search whose budget covers the space is then
#' exhaustive: the grid is every point and no Bayesian stage runs, since
#' tune's search, once it has no value left to propose, errors inside its
#' loop rather than stopping.
#'
#' @param param_set A finalized `dials` parameter set.
#' @param budget Numeric(1). The points the search would try: the grid's
#'   size and the Bayesian iterations together.
#' @return A tibble with one column per parameter (named by its id) and one
#'   row per point, or `NULL`.
#' @keywords internal
#' @noRd
exhaustive_grid <- function(param_set, budget) {

  if (nrow(param_set) == 0) return(NULL)

  values <- lapply(param_set$object, function(p) {

    if (!inherits(p, "quant_param") || !identical(p$type, "integer") || !is.null(p$trans)) {
      return(NULL)
    }

    lower <- p$range$lower
    upper <- p$range$upper

    ### unknown() until finalized
    if (!is.numeric(lower) || !is.numeric(upper) || !is.finite(lower) || !is.finite(upper)) {
      return(NULL)
    }

    if (!isTRUE(p$inclusive[[1]])) lower <- lower + 1
    if (!isTRUE(p$inclusive[[2]])) upper <- upper - 1

    if (upper < lower) return(NULL)

    seq.int(as.integer(lower), as.integer(upper))

  })

  if (any(vapply(values, is.null, logical(1)))) return(NULL)
  if (prod(lengths(values)) > budget) return(NULL)

  names(values) <- param_set$id

  tibble::as_tibble(expand.grid(values, KEEP.OUT.ATTRS = FALSE))

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

  for (m in metrics) {

    v       <- panel$mean[panel$.metric == m]
    covered <- is.null(n_folds) || identical(panel$n[panel$.metric == m], as.integer(n_folds))

    if (length(v) == 1 && is.finite(v) && covered) out[[paste0("cv_", m)]] <- v

  }

  tibble::as_tibble(out)

}

## ---------------------------------------------------------------------------
## The fold rule (#101): only candidates scored on every fold are compared
## ---------------------------------------------------------------------------
## tune averages a candidate's metric over the folds it was scored on, so a
## candidate that failed on one fold is compared with the others on the folds
## that survived, which is a different and easier test. A candidate missing a
## fold is left out instead, as scikit-learn leaves out one whose mean is NaN.
## A recipe step that fails on a fold fails every candidate there, so its
## configuration then has none to choose from and fails.

#' Each candidate's metrics over its own folds
#'
#' @description
#' `tune::collect_metrics()` pools the folds of candidates with equal
#' hyperparameters, so a Bayesian iteration that proposes a grid point again
#' reports both on twice the folds. Read from the per-fold metrics instead,
#' each `.config` is counted on the resamples it was scored on, and its mean
#' is its own.
#'
#' @param results A tune result (grid or Bayesian).
#' @return Tibble with one row per `.config` and metric: `.config`,
#'   `.metric`, `mean` (over the folds with a value; `NA` when none has one)
#'   and `n` (the distinct resamples with a value).
#' @keywords internal
#' @noRd
config_metrics <- function(results) {

  per_fold <- tune::collect_metrics(results, summarize = FALSE)
  resample <- resample_ids(per_fold)
  scored   <- !is.na(per_fold$.estimate)
  groups   <- split(seq_len(nrow(per_fold)), list(per_fold$.config, per_fold$.metric), drop = TRUE)

  tibble::tibble(
    .config = vapply(groups, function(i) per_fold$.config[i[1]], character(1), USE.NAMES = FALSE),
    .metric = vapply(groups, function(i) per_fold$.metric[i[1]], character(1), USE.NAMES = FALSE),
    mean    = vapply(groups, function(i) {
      if (any(scored[i])) mean(per_fold$.estimate[i][scored[i]]) else NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    n       = vapply(groups, function(i) length(unique(resample[i][scored[i]])), integer(1),
                     USE.NAMES = FALSE)
  )

}

#' The candidates of a tune result scored on every fold
#'
#' @param results A tune result (grid or Bayesian).
#' @param n_folds Integer(1). The folds drawn.
#' @return Character: their `.config`s. Counted on `rmse`, which is `NA` on a
#'   fold only when the fit made no predictions there; a metric undefined on
#'   a fold whose predictions were constant (`rsq`) is not a failure.
#' @keywords internal
#' @noRd
complete_configs <- function(results, n_folds) {

  panel <- config_metrics(results)

  panel$.config[panel$.metric == "rmse" & panel$n == n_folds]

}

#' The candidates scored on every fold, best first
#'
#' @description
#' tune's own order and tie rule, as `tune::select_best()` applies them
#' (`tune::show_best()` with every candidate), restricted to the candidates
#' scored on every fold. Under `debug` a candidate missing a fold stops the
#' call rather than being left out, since leaving it out is containment.
#'
#' @param results A tune result (grid or Bayesian).
#' @param metric Character(1). The metric to order by.
#' @param n_folds Integer(1). The folds drawn.
#' @param debug Logical(1).
#' @return `tune::show_best()`'s rows, possibly none.
#' @keywords internal
#' @noRd
rank_complete <- function(results, metric, n_folds, debug = FALSE) {

  ranked   <- tune::show_best(results, metric = metric, n = Inf)
  complete <- ranked$.config %in% complete_configs(results, n_folds)

  if (debug && !all(complete)) {

    abort_tune_failure(results, "a candidate was not scored on every fold")

  }

  ranked[complete, , drop = FALSE]

}

#' The best hyperparameters among the candidates scored on every fold
#'
#' @description
#' What `tune::select_best()` returns, chosen by [rank_complete()]: the
#' hyperparameters and `.config` of the first candidate.
#'
#' @inheritParams rank_complete
#' @return One-row tibble, or zero rows when no candidate was scored on every
#'   fold.
#' @keywords internal
#' @noRd
select_complete <- function(results, metric, n_folds, debug = FALSE) {

  ranked <- rank_complete(results, metric, n_folds, debug)

  utils::head(ranked[, c(tune::.get_tune_parameter_names(results), ".config"), drop = FALSE], 1)

}

#' The row's note on candidates left out for a missing fold
#'
#' @param results The tune result the hyperparameters were chosen from.
#' @param n_folds Integer(1). The folds drawn.
#' @return Character(1), or `NULL` when every candidate was scored on every
#'   fold.
#' @keywords internal
#' @noRd
incomplete_note <- function(results, n_folds) {

  all   <- unique(config_metrics(results)$.config)
  n_out <- length(setdiff(all, complete_configs(results, n_folds)))

  if (n_out == 0) return(NULL)

  cause <- tune_failure_cause(results)

  paste0(n_out, " of ", length(all), " candidate", if (length(all) != 1) "s",
         if (n_out == 1) " was" else " were",
         " left out of the choice of hyperparameters, not being scored on every fold",
         if (!is.null(cause)) paste0(": ", cause), ".")

}
