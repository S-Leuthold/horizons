#' Evaluate Model Configurations
#'
#' @description
#' Evaluates all model configurations from `configure()` against held-out test
#' data. Each configuration is tuned via grid search (optionally followed by
#' Bayesian optimization), evaluated on the test set, and ranked by the
#' specified metric. The best configuration is stored for downstream use by
#' `fit()`.
#'
#' @param x A `horizons_data` object with `config$configs` populated by
#'   `configure()`.
#' @param metric Character. Metric for ranking configurations. One of
#'   `"rpd"`, `"rsq"`, `"rmse"`, `"rrmse"`, `"ccc"`, `"mae"`. Ranking uses
#'   the cross-validated value of that metric at each config's selected
#'   hyperparameters (`evaluation$results$cv_<metric>`); the test-set columns
#'   are reported but never used for selection, so they remain honest
#'   held-out estimates. Default `"rpd"`.
#' @param prune Logical. If TRUE, skip Bayesian optimization for configs
#'   whose grid-search RPD falls below `prune_threshold`. When `configure()`
#'   set `bayesian_iter = 0` there is nothing to skip, so no config is
#'   pruned. Either way, whether each config fell below the threshold is
#'   recorded in `evaluation$results$below_prune_threshold` (with the
#'   threshold in `prune_threshold`), and `fit()` warns when every member it
#'   fits did. Default TRUE.
#' @param prune_threshold Numeric. RPD threshold for pruning. Configs with
#'   grid-search RPD below this value skip Bayesian optimization but still
#'   receive test-set metrics from grid-search best. Default 1.0 (the
#'   "no better than the mean" line).
#' @param allow_par Logical. If `TRUE`, dispatch onto whatever
#'   `future::plan()` the caller has registered. If no usable backend is
#'   registered (fewer than two workers), warns naming the plan it found and
#'   runs sequentially. `FALSE` (the default) runs in the calling process and
#'   never inspects the plan. `evaluate()` never registers or alters a plan.
#' @param parallelize_over Character. Which axis of the work the registered
#'   plan is applied to; ignored when `allow_par = FALSE`. See the
#'   Parallelism section. Default `"auto"`.
#' @param output_dir Character or NULL. If provided, each config's result is
#'   written to `<output_dir>/checkpoints/<config_id>.rds` as it finishes, and
#'   a rerun into the same directory resumes from those files. Required when
#'   configs are dispatched to workers. A resumed row must have been scored on
#'   this run's training data (the outcome, the sample ids, which columns hold
#'   the `id`, `outcome`, `predictor` and `covariate` roles, and the outcome,
#'   predictor and covariate values) and tuned with this run's settings
#'   (`cv_folds`, `grid_size`, `bayesian_iter`, `prune`, `prune_threshold`
#'   when pruning, `seed`, and `configure()`'s `sg_window`, `pca_threshold`
#'   and `outcome_range`); either mismatch aborts, naming what differs.
#'   So re-standardized spectra, a rescaled outcome, or another outcome on the
#'   same samples is refused rather than resumed, while adding a sibling
#'   response with `add_response()` or a `meta` column resumes. Keep one
#'   `output_dir` per outcome. The ranking `metric` is not checked, since
#'   every row carries all six cross-validated metrics. Rows scored under an
#'   earlier scoring schema are dropped and re-evaluated; rows for configs no
#'   longer in the grid are dropped; a file that cannot be used warns and its
#'   config is re-evaluated; rows written before a fingerprint field existed
#'   warn once and resume. A whole-table `eval_checkpoint.rds` from earlier
#'   versions is read only for configs with no per-config file, and its rows
#'   are copied into `checkpoints/`, after which it can be deleted. Default
#'   NULL (no checkpointing).
#' @param seed Integer. Random seed for train/test split and CV folds.
#'   Default 307L.
#' @param verbose Logical. Print progress tree to console. Default TRUE.
#' @param workers Deprecated (2026-09-14) and ignored, with a warning. The
#'   earlier design took a core count and auto-split it into outer and inner
#'   levels while managing its own nested plan. Register a plan and use
#'   `allow_par` with `parallelize_over` instead.
#'
#' @section Parallelism:
#' The user owns the backend and the topology; `evaluate()` owns only which
#' axis the plan is applied to.
#'
#' * `"configs"`: one future per config on the registered plan, with tune
#'   running sequentially inside each. The per-worker payload is one config's
#'   worth and tune ships nothing to inner workers. The mode for a large
#'   config grid on a many-core machine.
#' * `"resamples"`: configs run sequentially; inside each, tune parallelises
#'   the CV folds on the registered plan. Useful width is capped at
#'   `cv_folds`. The mode for a few cores and a short grid.
#' * `"auto"` (default): `"configs"` when the number of configs is at least
#'   `cv_folds`, otherwise `"resamples"`. Derived from the measured cost
#'   model: configs wins whenever `n_configs` exceeds the effective inner
#'   speedup, which is bounded by `cv_folds`, and always once it exceeds the
#'   worker count, so the worker count cancels.
#'
#' A nested plan (configs across an outer level, folds across an inner one)
#' is not built or endorsed by the package in this version. A caller who
#' wants one registers it with `future::plan(list(...))` and uses
#' `"configs"`.
#'
#' ```
#' # Laptop: four cores, parallelise the folds inside each config.
#' future::plan(future::multisession, workers = 4)
#' hd |> evaluate(allow_par = TRUE, parallelize_over = "resamples")
#'
#' # Thirty cores, one config per worker, checkpointed.
#' future::plan(future::multisession, workers = 30)
#' hd |> evaluate(allow_par = TRUE, output_dir = "output/run")
#' ```
#'
#' Before dispatch, BLAS, OpenMP, data.table and ranger threads are pinned to
#' one in the calling process (via `RhpcBLASctl` when installed) and restored
#' on exit. Configs are dispatched to a top-level worker that loads the
#' *installed* package, so parallel dispatch refuses to run under
#' `devtools::load_all()`.
#'
#' @section When every configuration fails:
#' `best_config` is chosen from the configs that succeeded or, when none did,
#' from the pruned configs that carry a cross-validated value of `metric`.
#' When there are neither, or none of them has a value of `metric` to rank
#' on, `evaluate()` aborts with class `horizons_all_configs_failed`. The
#' message lists the distinct error messages (the first three, each with the
#' configs that raised it) and names any configs that were loaded from
#' checkpoints in `output_dir` rather than run, since calling `evaluate()`
#' again resumes them instead of re-running them. The per-config results,
#' `error_message` included, travel on the condition as `results`, because
#' `evaluate()` aborts before it assigns `x$evaluation`; after an uncaught
#' abort, `rlang::last_error()$results` recovers them without re-running. A
#' loop over subsets can catch the class, tolerate one subset whose every
#' config fails, and keep its errors:
#'
#' ```
#' runs <- lapply(subsets, function(hd) {
#'   tryCatch(
#'     evaluate(hd, verbose = FALSE),
#'     horizons_all_configs_failed = function(e) e$results
#'   )
#' })
#'
#' # A horizons_eval for each subset that ranked, and the failed configs'
#' # results table for each subset that did not.
#' failed <- runs[!vapply(runs, inherits, logical(1), "horizons_eval")]
#' ```
#'
#' @return A `horizons_eval` object (inherits from `horizons_data`) with
#'   `evaluation$results`, `evaluation$best_config`, `evaluation$split`, and
#'   associated metadata populated, including `evaluation$parallelize_over`
#'   (the axis actually used), `evaluation$workers` (the worker count the
#'   registered plan offered; 1 when sequential) and `evaluation$recipe` (the
#'   `sg_window`, its width in cm-1 on the evaluated axis as `sg_window_cm`,
#'   and the `pca_threshold` every config's recipe ran with). Test-set
#'   predictions are clamped to `configure()`'s `outcome_range` before they
#'   are scored, as are the tuning predictions of a transformed response,
#'   which are back-transformed first. Aborts, before any config runs, when
#'   `configure()`'s `sg_window` is not narrower than the spectrum, and, with class
#'   `horizons_input_error`, when an observed outcome lies outside
#'   `outcome_range` or is infinite, or when `metric = "rrmse"` under a range
#'   whose lower bound is negative (the mean outcome it divides by can then be
#'   zero or negative) (#76). Checkpoint rows written before the range was
#'   recorded resume only under the default range. Called on an object that
#'   has already been through `fit()` or `ensemble()`, it returns a
#'   `horizons_eval` whose `models` and `ensemble` slots are empty again,
#'   since both were built on the evaluation it replaces.
#'
#' @export
evaluate <- function(x,
                     metric           = "rpd",
                     prune            = TRUE,
                     prune_threshold  = 1.0,
                     allow_par        = FALSE,
                     parallelize_over = "auto",
                     output_dir       = NULL,
                     seed             = 307L,
                     verbose          = TRUE,
                     workers          = NULL) {

  start_time <- Sys.time()

  deprecate_workers_arg(workers)

  ## -----------------------------------------------------------------------
  ## Step 0: Structural validation
  ## -----------------------------------------------------------------------
  ## validate_horizons_eval() (at return) checks the evaluation slot, not the
  ## base data contract, so a column added after configure() without a
  ## role_map entry — by a later parse_ids(), or a direct
  ## `x$data$analysis$col <- ...` — reached build_recipe()'s `outcome ~ .` as
  ## an unregistered predictor, undetected, until this call closed that gap
  ## (#24). Full stage: an object this far into the pipeline is expected to
  ## have unique, non-NA sample ids.

  x <- validate_horizons_data(x)

  ## -----------------------------------------------------------------------
  ## Step 1: Gate checks
  ## -----------------------------------------------------------------------

  if (!metric %in% VALID_RANK_METRICS) {

    rlang::abort(paste0(
      "Invalid `metric`: '", metric, "'. ",
      "Must be one of: ", paste(VALID_RANK_METRICS, collapse = ", ")
    ))

  }

  configs <- x$config$configs

  if (is.null(configs) || nrow(configs) == 0) {

    rlang::abort(
      "No configurations found. Run `configure()` before `evaluate()`."
    )

  }

  role_map    <- x$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]
  analysis    <- x$data$analysis

  ## -----------------------------------------------------------------------
  ## Step 2: Handle NA outcome rows
  ## -----------------------------------------------------------------------
  ## fit() applies the same rule to the same table, so the two verbs model
  ## the same rows (#67).

  modelled  <- outcome_complete_rows(analysis, outcome_col)
  analysis  <- modelled$data
  n_dropped <- modelled$n_dropped

  ## -----------------------------------------------------------------------
  ## Step 2b: The outcome has to lie inside its range
  ## -----------------------------------------------------------------------
  ## Every test-set and tuning prediction is clamped to configure()'s
  ## outcome_range, so an outcome outside it would be scored against values
  ## the predictions cannot reach. configure() refuses that already, but an
  ## object configured before the range existed reads as c(0, Inf), and rows
  ## can change after configure(). Refused here, before any split or tuning,
  ## so it costs a second rather than the whole grid (#76).

  check_outcome_range(x, verb = "evaluate")
  outcome_range <- outcome_range_setting(x)

  ## rrmse divides by the mean outcome, which a range with a negative floor
  ## lets be zero or negative; ranking by its minimum would then pick the
  ## worst configuration.
  check_rank_metric_range(metric, outcome_range, verb = "evaluate")

  ## -----------------------------------------------------------------------
  ## Step 3: Validate minimum sample size
  ## -----------------------------------------------------------------------

  tuning    <- x$config$tuning
  cv_folds  <- tuning$cv_folds
  n_samples <- nrow(analysis)

  if (n_samples < cv_folds * 2) {

    rlang::abort(paste0(
      "Insufficient sample size for evaluation. ",
      "Need at least ", cv_folds * 2, " samples (cv_folds * 2), ",
      "but only ", n_samples, " available."
    ))

  }

  ## -----------------------------------------------------------------------
  ## Step 3a: The window has to fit the spectrum
  ## -----------------------------------------------------------------------
  ## Object-level recipe settings, the same for every config; see
  ## recipe_settings() for objects configured before they were recorded.
  ##
  ## configure() cannot check the window against the spectrum, because
  ## standardize() can still change the axis after it. Past this point a
  ## window as wide as the spectrum fails every config inside tune, as a
  ## "Grid search failed" or an "All configurations failed" that never names
  ## the window. The width in cm-1 is read from the axis the recipe will run
  ## on, and recorded in Step 11. The check is one helper, which fit()'s cold
  ## start (#45) applies too.

  recipe_cfg <- evaluation_recipe(x)

  ## -----------------------------------------------------------------------
  ## Step 3b: Resolve the parallel axis against the registered plan
  ## -----------------------------------------------------------------------
  ## The user owns the backend; evaluate() only chooses the axis. The
  ## resolver is pure; the backend check reads the plan and downgrades to
  ## sequential, with a warning, when there is nothing to run on.

  axis <- resolve_parallel_axis(parallelize_over, allow_par,
                                n_configs = nrow(configs), cv_folds = cv_folds)

  if (allow_par && !check_parallel_backend("evaluate()")) {

    axis <- resolve_parallel_axis(parallelize_over, allow_par = FALSE,
                                  n_configs = nrow(configs), cv_folds = cv_folds)

  }

  plan_label    <- if (axis$axis == "sequential") "sequential" else registered_plan_label()
  plan_workers  <- if (axis$axis == "sequential") 1L else registered_workers()

  if (axis$axis != "sequential") {

    warn_if_mirai_preferred()

    unpin_threads <- pin_parent_threads()
    on.exit(unpin_threads(), add = TRUE)

  }

  if (axis$dispatch_configs && is.null(output_dir)) {

    rlang::abort(paste0(
      "Dispatching configs to workers (parallelize_over = \"", axis$axis,
      "\") requires `output_dir` for checkpoint safety. ",
      "Provide an output directory or use parallelize_over = \"resamples\"."
    ))

  }

  ## -----------------------------------------------------------------------
  ## Step 4: Create train/test split
  ## -----------------------------------------------------------------------
  ## The draw is shared with fit()'s cold start (#45), which must hold out
  ## the rows this verb would at the same seed. `analysis` has had its NA
  ## outcomes dropped already; the helper's own pass over it then drops
  ## nothing and returns it uncopied, so the split shares its rows.

  drawn <- draw_eval_split(analysis, outcome_col, seed)
  split <- drawn$split

  train_data <- rsample::training(split)
  test_data  <- rsample::testing(split)
  n_train    <- nrow(train_data)
  n_test     <- nrow(test_data)

  ## -----------------------------------------------------------------------
  ## Step 5: Create CV folds
  ## -----------------------------------------------------------------------

  ## Drawn from the stream the split left. The fallback notes print inside
  ## the tree, after its header (#91).

  cv_drawn <- draw_stratified(
    outcome      = train_data[[outcome_col]],
    stratified   = function() rsample::vfold_cv(train_data, v = cv_folds,
                                                strata = dplyr::all_of(outcome_col),
                                                breaks = STRATA_BREAKS, pool = STRATA_POOL),
    unstratified = function() rsample::vfold_cv(train_data, v = cv_folds)
  )
  cv_fold_obj <- cv_drawn$draw

  ## -----------------------------------------------------------------------
  ## Step 5b: Fingerprint the training rows
  ## -----------------------------------------------------------------------
  ## Checkpoints are keyed by config_id alone, which says nothing about the
  ## rows a result was computed on. A dry run and a real run pointed at the
  ## same output_dir therefore used to resume each other's CV results and
  ## warm-start fit() from hyperparameters tuned on the wrong data, silently
  ## (2026-09-21). The fingerprint is what makes that visible. It covers the
  ## response as well as the rows: the config id does not hash the outcome, so
  ## two responses on one row set collide by id alone. It also covers the
  ## values on those rows (#42), since re-standardized spectra keep the ids.
  ## Computed once here; the parallel worker is sent it.

  data_fp <- eval_data_fingerprint(train_data, role_map)

  ## -----------------------------------------------------------------------
  ## Step 5c: Record the settings a result row depends on
  ## -----------------------------------------------------------------------
  ## The data fingerprint says which rows a checkpoint was scored on, not how
  ## it was tuned: the same rows re-run with another grid_size into the same
  ## output_dir resumed the old results silently (#42). This list is the one
  ## place a setting is added. The metric is absent on purpose: every row
  ## carries all six cv_ columns and the ranking is recomputed on each run, so
  ## changing it cannot make a row stale. So is the parallel axis, which the
  ## results do not depend on.

  settings <- eval_settings(
    cv_folds        = cv_folds,
    grid_size       = tuning$grid_size,
    bayesian_iter   = tuning$bayesian_iter,
    prune           = prune,
    ## Read only when pruning, so it cannot have changed a row otherwise.
    prune_threshold = if (isTRUE(prune)) prune_threshold else NA_real_,
    seed            = seed,
    ## configure()'s recipe settings (#62). pca_threshold is recorded for
    ## every row, PCA or not: conservative, since a row that never ran PCA
    ## re-runs rather than resumes when only the threshold changed.
    sg_window       = recipe_cfg$sg_window,
    pca_threshold   = recipe_cfg$pca_threshold,
    ## The range every scored prediction is clamped to (#76): the same fit
    ## scores differently under another range, so a resume under one refuses.
    outcome_range   = outcome_range
  )

  ## -----------------------------------------------------------------------
  ## Step 6: Load checkpoints (if any)
  ## -----------------------------------------------------------------------
  ## One store: a file per config under checkpoints/. Every row, from it or
  ## from a legacy eval_checkpoint.rds, passes the same gates in the same
  ## order (see load_eval_checkpoints()).

  checkpoint_dir     <- NULL
  checkpoint_results <- list()
  legacy_ids         <- character(0)
  checkpoint_notes   <- character(0)

  if (!is.null(output_dir)) {

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    checkpoint_dir <- file.path(output_dir, "checkpoints")

    if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir)

    loaded <- load_eval_checkpoints(
      output_dir = output_dir,
      config_ids = configs$config_id,
      data_fp    = data_fp,
      settings   = settings
    )

    ## The count of loaded results is the Configs line's "from checkpoint",
    ## and the notes on dropped rows print under it: both used to print
    ## here, above the tree's header (#91).
    checkpoint_results <- loaded$rows
    legacy_ids         <- loaded$legacy_ids
    checkpoint_notes   <- loaded$notes

  }

  ## Determine which configs still need evaluation
  completed_ids <- names(checkpoint_results)
  pending_ids   <- setdiff(configs$config_id, completed_ids)
  n_total       <- nrow(configs)
  n_pending     <- length(pending_ids)

  ## -----------------------------------------------------------------------
  ## Step 7: Render tree header
  ## -----------------------------------------------------------------------

  if (verbose) {

    cat("\n")
    cat(paste0("\u250C Evaluation ",
               paste(rep("\u2500", 50), collapse = ""), "\n"))
    cat("\u2502\n")
    if (n_dropped > 0) {

      cat(paste0("\u2502  ",
                 cli::col_yellow("Dropped ", n_dropped,
                                  " rows with NA outcome"), "\n"))

    }

    ## "stratified" only when the strata held: rsample draws unstratified
    ## below 40 rows without an error (#91; see draw_stratified()). The folds
    ## are drawn on the training part, so they can be unstratified under a
    ## stratified split.
    cat(paste0("\u2502  Split: ", n_train, " train / ", n_test, " test (",
               round(100 * SPLIT_PROP), "/", round(100 * (1 - SPLIT_PROP)), ", ",
               if (drawn$stratified) "stratified" else "unstratified", ")\n"))

    if (drawn$strata_failed) {

      cat(paste0("\u2502  ", cli::col_yellow("Stratified split failed, ",
                                             "retrying without strata"), "\n"))

    }

    cat(paste0("\u2502  Tuning: ", cv_folds, "-fold CV (",
               if (cv_drawn$stratified) "stratified" else "unstratified",
               "), grid = ", tuning$grid_size, ", bayesian = ",
               tuning$bayesian_iter, "\n"))

    if (cv_drawn$strata_failed) {

      cat(paste0("\u2502  ", cli::col_yellow("Stratified CV failed, ",
                                             "retrying without strata"), "\n"))

    }

    cat(paste0("\u2502  Configs: ", n_total, " total",
               if (n_pending < n_total) paste0(" (", n_total - n_pending,
                                                " from checkpoint)") else "",
               "\n"))

    for (note in checkpoint_notes) {

      cat(paste0("\u2502  ", cli::col_yellow(note), "\n"))

    }

    if (axis$axis != "sequential") {

      workers_label <- if (is.na(plan_workers)) "unbounded" else {
        paste0(plan_workers, " worker", if (plan_workers != 1) "s" else "")
      }

      cat(paste0("\u2502  Parallel: over ", axis$axis, " on ", plan_label,
                 " (", workers_label, ")\n"))

    }

    cat("\u2502\n")

  }

  ## -----------------------------------------------------------------------
  ## Step 7b: Manifest for monitor_evaluate()
  ## -----------------------------------------------------------------------
  ## Written on EVERY run with an output_dir, whichever axis, and refreshed
  ## each time, so the monitor can watch a resamples-axis or sequential run
  ## and never reports a stale axis from an earlier run in the same
  ## directory. Schema 2 (2026-09-15) records the axis and the user's plan;
  ## schema 3 (2026-09-21) records the training-data fingerprint, so the
  ## monitor can say which rows the run in this directory is scoring; schema
  ## 4 (#42) records the data fields and the tuning settings. The monitor
  ## gates checkpoint rows against them, as evaluate() does, and re-reads the
  ## manifest on every poll, so it is written atomically.

  if (!is.null(output_dir)) {

    manifest <- list(
      schema_version               = 4L,
      n_total                      = n_total,
      n_pending                    = n_pending,
      config_ids                   = configs$config_id,
      start_time                   = start_time,
      metric                       = metric,
      cv_folds                     = cv_folds,
      allow_par                    = allow_par,
      parallelize_over_requested   = parallelize_over,
      axis                         = axis$axis,
      tune_parallel_over_requested = axis$tune_parallel_over,
      plan                         = plan_label,
      workers                      = plan_workers,
      scoring_schema               = SCORING_SCHEMA,
      data_hash                    = data_fp$data_hash,
      data_n_rows                  = data_fp$data_n_rows,
      data_fields                  = data_fp$data_fields,
      settings                     = settings
    )
    tmp_manifest <- tempfile(tmpdir = output_dir, fileext = ".rds.tmp")
    saveRDS(manifest, tmp_manifest)
    file.rename(tmp_manifest, file.path(output_dir, "eval_manifest.rds"))

  }

  ## -----------------------------------------------------------------------
  ## Step 8: Config loop (sequential or parallel)
  ## -----------------------------------------------------------------------

  if (!axis$dispatch_configs) {

    ## -------------------------------------------------------------------
    ## Sequential path — identical to pre-workers behavior
    ## -------------------------------------------------------------------

    results_list <- list()

    for (i in seq_len(nrow(configs))) {

      cfg <- configs[i, ]

      ## Pretty config description — always show full pipeline
      model_name  <- MODEL_DISPLAY_NAMES[cfg$model] %||% cfg$model
      desc_parts  <- c(model_name, cfg$transformation, cfg$preprocessing,
                       cfg$feature_selection)

      if (!is.na(cfg$covariates)) {
        desc_parts <- c(desc_parts, paste0("+", cfg$covariates))
      }

      config_desc <- paste(desc_parts, collapse = " + ")
      is_last     <- i == nrow(configs)
      branch      <- if (is_last) "\u2514\u2500" else "\u251C\u2500"
      cont        <- if (is_last) "   " else "\u2502  "

      ## Skip if already checkpointed
      if (cfg$config_id %in% completed_ids) {

        if (verbose) {

          cat(paste0(
            "\u2502  ", branch, " [", i, "/", n_total, "] ",
            config_desc, "\n"
          ))
          cat(paste0("\u2502  ", cont, "\u2514\u2500 ",
                     cli::col_cyan("loaded from checkpoint"), "\n"))

        }

        results_list[[i]] <- checkpoint_results[[ cfg$config_id ]]
        next

      }

      ## Render config start
      if (verbose) {

        cat(paste0(
          "\u2502  ", branch, " [", i, "/", n_total, "] ",
          config_desc, "\n"
        ))

      }

      ## Evaluate this config
      result_row <- evaluate_single_config(
        config_row      = cfg,
        split           = split,
        cv_folds        = cv_fold_obj,
        role_map        = role_map,
        grid_size       = tuning$grid_size,
        bayesian_iter   = tuning$bayesian_iter,
        prune           = prune,
        prune_threshold = prune_threshold,
        allow_par       = axis$tune_allow_par,
        parallel_over   = axis$tune_parallel_over %||% "resamples",
        seed            = seed,
        sg_window       = recipe_cfg$sg_window,
        pca_threshold   = recipe_cfg$pca_threshold,
        outcome_range   = outcome_range
      )

      ## Stamp before anything else sees the row, so the in-memory results and
      ## the checkpointed copy carry the same provenance.
      result_row        <- stamp_data_fingerprint(result_row, data_fp)
      result_row        <- stamp_eval_settings(result_row, settings)
      results_list[[i]] <- result_row

      ## Render result
      if (verbose) {

        if (result_row$status == "success") {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 Test metrics: ",
            "RPD = ", round(result_row$rpd, 2),
            ", R\u00B2 = ", round(result_row$rsq, 2),
            ", RMSE = ", round(result_row$rmse, 3), "\n"
          ))

        } else if (result_row$status == "pruned") {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_yellow("Pruned (grid RPD below threshold)"), "\n"
          ))
          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 Test metrics: ",
            "RPD = ", round(result_row$rpd, 2),
            ", R\u00B2 = ", round(result_row$rsq, 2),
            ", RMSE = ", round(result_row$rmse, 3), "\n"
          ))

        } else {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_red("FAILED: ", result_row$error_message), "\n"
          ))

        }

        ## Render warnings (yellow, one per line)
        if (!is.null(result_row$warnings[[1]])) {

          for (w in result_row$warnings[[1]]) {
            cat(paste0(
              "\u2502  ", cont, "\u251C\u2500 ",
              cli::col_yellow(w), "\n"
            ))
          }

        }

        ## Render runtime
        cat(paste0(
          "\u2502  ", cont, "\u2514\u2500 ",
          if (result_row$status == "failed") {
            cli::col_red("\u2717")
          } else {
            cli::col_green("\u2713")
          },
          " ", round(result_row$runtime_secs, 1), "s\n"
        ))

      }

      ## Checkpoint: the per-config file is the only store (#42)
      if (!is.null(checkpoint_dir)) {

        write_checkpoint_row(result_row, checkpoint_dir)

      }

      ## Memory cleanup
      gc(verbose = FALSE)

    }

  } else {

    ## -------------------------------------------------------------------
    ## Parallel path — furrr::future_map() across configs
    ## -------------------------------------------------------------------

    ## The plan is the user's: evaluate() neither sets nor restores one.
    ## The globals ceiling is declared (EVAL_WORKER_PAYLOAD_LIMIT, from the
    ## measured payload) so a payload regression aborts with future's clear
    ## "size of the globals" error rather than R's long-vector message.
    old_max_size <- getOption("future.globals.maxSize")
    options(future.globals.maxSize = EVAL_WORKER_PAYLOAD_LIMIT)
    on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)

    if (verbose) {

      cat(paste0(
        "\u2502  Processing ", n_pending, " pending configs...\n"
      ))
      cat(paste0(
        "\u2502  Monitor: horizons::monitor_evaluate(\"",
        output_dir, "\")\n"
      ))
      cat("\u2502\n")

    }

    ## Filter to pending configs only
    pending_configs <- configs[configs$config_id %in% pending_ids, ]

    ## Assemble everything a worker needs into one value. Two serialization
    ## traps are avoided here; R/utils-resamples.R's header has the mechanism
    ## and the measurements.
    ##
    ## Do not inline the worker back into an anonymous function. A closure
    ## defined in this frame is serialized together with it, so every local —
    ## `split` and `cv_fold_obj` included — would ship to every worker no matter
    ## what is passed explicitly.

    ## Workers resolve the horizons namespace by NAME, which finds the INSTALLED
    ## library — not this source tree. Under pkgload the installed copy may not
    ## have these helpers at all; worse, a compatible-but-stale install runs old
    ## helper code while the worker body is new, silently, and the worker
    ## computes every metric. Refuse rather than produce quiet wrong numbers.
    if (exists(".__DEVTOOLS__", envir = asNamespace("horizons"),
               inherits = FALSE)) {

      rlang::abort(paste0(
        "Parallel `evaluate()` cannot run under `devtools::load_all()`. ",
        "Workers load the installed `horizons`, not this source tree, so they ",
        "would run stale code. Install the package first (`R CMD INSTALL`), ",
        "or call `evaluate()` without parallelism."
      ))

    }

    shared_args <- list(
      data            = split$data,
      resample_idx    = resample_indices(split, cv_fold_obj),
      configs         = pending_configs,
      role_map        = role_map,
      grid_size       = tuning$grid_size,
      bayesian_iter   = tuning$bayesian_iter,
      prune           = prune,
      prune_threshold = prune_threshold,
      seed            = seed,
      sg_window       = recipe_cfg$sg_window,
      pca_threshold   = recipe_cfg$pca_threshold,
      outcome_range   = outcome_range,
      data_fp         = data_fp,
      settings        = settings,
      checkpoint_dir  = checkpoint_dir,
      pkg_version     = as.character(utils::packageVersion("horizons"))
    )

    stopifnot(setequal(names(shared_args), SHARED_ARG_NAMES))

    parallel_results <- furrr::future_map(
      seq_len(nrow(pending_configs)),
      evaluate_config_worker,
      shared   = shared_args,
      .options = furrr::furrr_options(seed = TRUE, chunk_size = 1)
    )

    ## Combine: checkpoint results + parallel results
    results_list <- c(
      lapply(completed_ids, function(id) checkpoint_results[[id]]),
      parallel_results
    )

  }

  ## -----------------------------------------------------------------------
  ## Step 9: Aggregate results
  ## -----------------------------------------------------------------------

  all_results <- dplyr::bind_rows(results_list)

  ## Checkpoint rows written before the cv_* columns existed bind without
  ## them. Materialise the columns as NA so ranking can name what it skips
  ## rather than failing on a missing column.
  for (cv_col in paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))) {

    if (!cv_col %in% names(all_results)) all_results[[cv_col]] <- NA_real_

  }

  ## Likewise the prune gate's reading, which rows written before it was
  ## recorded (#38) do not carry.
  if (!"below_prune_threshold" %in% names(all_results)) all_results$below_prune_threshold <- NA
  if (!"prune_threshold" %in% names(all_results)) all_results$prune_threshold <- NA_real_

  ## Once checkpointed and new rows are combined, rather than in the loader;
  ## monitor_evaluate() applies the same helper to the same rows.
  all_results <- relabel_inert_pruned(all_results, tuning$bayesian_iter)

  ## -----------------------------------------------------------------------
  ## Step 10: Determine best config
  ## -----------------------------------------------------------------------
  ## Ranking uses the cross-validated metric at each config's selected
  ## hyperparameters (cv_<metric>), not the test-set metric. Selecting on the
  ## test set would make the reported test metric of the winner a maximum
  ## over N configs on the same rows rather than a held-out estimate (#50).
  ## `metric` and `rank_metric` keep the bare name; the column is derived.

  rank_column <- paste0("cv_", metric)

  ## The successes, or the pruned configs when none succeeded: the rule
  ## fit() applies to the same table.
  candidates <- ranking_candidates(all_results, metric)

  ## Nothing to rank: no candidates, or candidates none of which has a
  ## cv_<metric> value, which rank_configs_by_cv() would refuse unclassed and
  ## without the results. Rows resumed from checkpoints are named, since
  ## re-running evaluate() does not re-run them.
  if (nrow(candidates$rows) == 0 || all(is.na(candidates$rows[[rank_column]]))) {

    abort_all_configs_failed(
      all_results, metric,
      checkpoint_ids = intersect(all_results$config_id, completed_ids),
      legacy_ids     = intersect(all_results$config_id, legacy_ids),
      output_dir     = output_dir
    )

  }

  ranked <- rank_configs_by_cv(candidates$rows, metric)

  best_config_id <- ranked$config_id[1]

  ## -----------------------------------------------------------------------
  ## Step 11: Store evaluation metadata
  ## -----------------------------------------------------------------------

  total_runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  ## Re-evaluating a fitted or ensembled object replaces the evaluation the
  ## models were selected from, so the models and the ensemble go with it.
  x <- reset_slots(x, c("models", "ensemble"))

  x$evaluation <- list(
    results      = all_results,
    best_config  = best_config_id,
    rank_metric  = metric,
    screened     = TRUE,
    split        = split,
    n_train      = n_train,
    n_test       = n_test,
    workers      = plan_workers,
    parallelize_over = axis$axis,
    ## What every config's recipe ran with, the window's width included, so
    ## the results of an sg_window sweep can be told apart after the fact.
    recipe       = recipe_cfg,
    runtime_secs = total_runtime,
    timestamp    = Sys.time()
  )

  ## -----------------------------------------------------------------------
  ## Step 12: Promote class
  ## -----------------------------------------------------------------------

  class(x) <- c("horizons_eval", "horizons_data", "list")

  ## Certify the contract before returning: structural checks only (see
  ## validate_horizons_eval), so every evaluate() return matches invariant I5
  ## (results complete; best_config names a real config).
  x <- validate_horizons_eval(x)

  ## -----------------------------------------------------------------------
  ## Step 13: Render tree footer
  ## -----------------------------------------------------------------------

  if (verbose) {

    n_success <- sum(all_results$status == "success")
    n_pruned  <- sum(all_results$status == "pruned")
    n_failed  <- sum(all_results$status == "failed")

    ## Best config description
    best_cfg   <- configs[configs$config_id == best_config_id, ]
    best_model <- MODEL_DISPLAY_NAMES[best_cfg$model] %||% best_cfg$model
    best_row   <- all_results[all_results$config_id == best_config_id, ]

    cat("\u2502\n")
    cat(paste0(
      "\u2502  Results: ", n_success, " success, ",
      n_pruned, " pruned, ", n_failed, " failed\n"
    ))
    cat(paste0(
      "\u2514\u2500 Best: ", best_config_id, " (", best_model, ")",
      " \u2014 CV ", toupper(metric), " = ",
      round(best_row[[rank_column]], 3),
      " (test ", toupper(metric), " = ", round(best_row[[metric]], 3), ")\n"
    ))
    cat(paste0(
      paste(rep("\u2500", 62), collapse = ""), "\n"
    ))

  }

  x

}

## ---------------------------------------------------------------------------
## Checkpoint scoring schema
## ---------------------------------------------------------------------------

#' Scoring schema of a checkpoint row
#'
#' Rows written before the column existed are schema 1 (see `SCORING_SCHEMA`
#' in `R/constants.R`).
#' @param row One-row result tibble.
#' @return Integer schema.
#' @keywords internal
#' @noRd
checkpoint_row_schema <- function(row) {

  ## `[[` rather than `$`: tibble's `$` warns on a missing column, and rows
  ## written before the column existed are exactly the ones read here.
  s <- if ("scoring_schema" %in% names(row)) row[["scoring_schema"]] else NULL

  if (is.null(s) || length(s) != 1 || is.na(s)) return(1L)

  as.integer(s)

}

## ---------------------------------------------------------------------------
## Checkpoint store
## ---------------------------------------------------------------------------
## One file per config under <output_dir>/checkpoints/ is the only store
## (#42). Earlier versions also kept a whole-table eval_checkpoint.rds, written
## by the sequential path alone, read first, and left stale by the parallel
## path; it shadowed the per-config rows, and the two stores ran their gates
## in different orders. That file is still read, but only for configs with no
## per-config file. Every row from either store passes one gate order:
## training-data fingerprint, then tuning settings, then scoring schema.
## evaluate() and monitor_evaluate() both read through these helpers, so the
## monitor never counts or ranks a row evaluate() would reject.

#' Read one per-config checkpoint file
#'
#' @param path Path to the file.
#' @return List with `row` (the one-row result, or `NULL`) and `error` (why
#'   the file is not a usable checkpoint, or `NULL`).
#' @keywords internal
#' @noRd
read_checkpoint_file <- function(path) {

  row <- tryCatch(readRDS(path), error = function(e) e)

  if (inherits(row, "error")) {

    return(list(row = NULL, error = condition_summary(row)))

  }

  if (!is.data.frame(row) || nrow(row) != 1L || !"config_id" %in% names(row)) {

    return(list(row = NULL, error = "not a one-row result with a config_id"))

  }

  list(row = row, error = NULL)

}

#' Read every checkpoint row in an output directory
#'
#' Reads the per-config files, then the rows of a legacy single-file
#' checkpoint whose config has no per-config file. A per-config file counts
#' only for the config its name says: `<config_id>.rds` holding that config's
#' row. Anything else is reported, never read as a checkpoint, because older
#' versions wrote each row to a `file*.rds` temp name before renaming it, and
#' a leftover one sorted ahead of the real file and shadowed it. A config
#' counts as covered by its file name whether or not the file can be read, so
#' a legacy row never stands in for a damaged file: the config is
#' re-evaluated instead. Nothing is gated here; see [gate_checkpoint_rows()].
#'
#' @param output_dir The directory passed to `evaluate(output_dir = )`.
#' @return List with `rows`, a list of candidates, each a list of `row` (the
#'   one-row result), `source` (the file, relative to `output_dir`) and `path`
#'   (the per-config file, or `NA` for a legacy row); and `unreadable`, the
#'   reason each unusable file was refused, named by the file relative to
#'   `output_dir`.
#' @keywords internal
#' @noRd
read_checkpoint_store <- function(output_dir) {

  checkpoint_dir <- file.path(output_dir, "checkpoints")

  files <- if (dir.exists(checkpoint_dir)) {
    list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE)
  } else {
    character(0)
  }

  ## Pre-allocated and filled in place: a monitor polls a store of thousands
  ## of files, and growing a list per file is quadratic.
  covered <- sub("\\.rds$", "", basename(files))
  sources <- file.path("checkpoints", basename(files))
  rows    <- vector("list", length(files))
  reasons <- rep(NA_character_, length(files))

  for (i in seq_along(files)) {

    got <- read_checkpoint_file(files[i])

    if (!is.null(got$error)) {

      reasons[i] <- got$error
      next

    }

    id <- as.character(got$row$config_id)

    if (!identical(id, covered[i])) {

      reasons[i] <- paste0("holds config ", id, "; not a checkpoint name")
      next

    }

    rows[[i]] <- list(row = got$row, source = sources[i], path = files[i])

  }

  refused    <- !is.na(reasons)
  rows       <- rows[!refused]
  unreadable <- stats::setNames(reasons[refused], sources[refused])

  ## -------------------------------------------------------------------------
  ## Legacy single file: only its rows for configs with no per-config file
  ## -------------------------------------------------------------------------

  legacy_file <- "eval_checkpoint.rds"
  legacy_path <- file.path(output_dir, legacy_file)

  if (!file.exists(legacy_path)) {

    return(list(rows = rows, unreadable = unreadable))

  }

  legacy <- tryCatch(readRDS(legacy_path), error = function(e) e)

  if (inherits(legacy, "error") || !is.data.frame(legacy) ||
      !"config_id" %in% names(legacy)) {

    unreadable[[legacy_file]] <- if (inherits(legacy, "error")) {
      condition_summary(legacy)
    } else {
      "not a checkpoint table with a config_id"
    }

    return(list(rows = rows, unreadable = unreadable))

  }

  ### The file may carry its fingerprint only as table attributes, which do
  ### not survive splitting it into rows; move it onto the rows first.
  fps                <- checkpoint_tibble_fingerprints(legacy)
  legacy$data_hash   <- fps$data_hash
  legacy$data_n_rows <- fps$data_n_rows

  from_legacy <- lapply(which(!legacy$config_id %in% covered), function(j) {
    list(row = legacy[j, , drop = FALSE], source = legacy_file,
         path = NA_character_)
  })

  list(rows = c(rows, from_legacy), unreadable = unreadable)

}

#' Judge one checkpoint row against the run that would resume it
#'
#' The gate order is fixed and shared by both stores: the training-data
#' fingerprint, then the tuning settings, then the scoring schema. A row that
#' fails a fingerprint is refused whatever its schema, because it means
#' another run's results are in the directory.
#'
#' @param row One-row result.
#' @param data_fp This run's fingerprint from [eval_data_fingerprint()]. A
#'   `NA` hash, or `NULL` fields, means the run cannot be checked on that
#'   count, and the row counts as unverified.
#' @param settings This run's [eval_settings()], or `NULL` when unknown (a
#'   manifest older than schema 4).
#' @return List with `verdict` (`"keep"`, `"data_mismatch"`,
#'   `"settings_mismatch"` or `"foreign_schema"`), `data_verified` and
#'   `settings_verified` (logical), `fingerprint` (the row's, from
#'   [checkpoint_row_fingerprint()]), `data_differ` (names of data fields
#'   recorded with another value), `stored_settings` (the row's, or `NULL`)
#'   and `settings_differ` (names of settings recorded with another value).
#' @keywords internal
#' @noRd
checkpoint_row_verdict <- function(row, data_fp, settings) {

  row_fp   <- checkpoint_row_fingerprint(row)
  expected <- data_fp$data_hash %||% NA_character_
  data_cmp <- compare_record(row_fp$data_fields, data_fp$data_fields)
  stored   <- checkpoint_row_settings(row)
  cmp      <- compare_record(stored, settings)

  ## A row stamped before #76 records no outcome_range, and was scored with
  ## the zero floor. Under the default range that is this run's scoring, so
  ## the row is only unverified; under any other range it was clamped
  ## differently, and ranking it beside this run's rows, or warm-starting
  ## fit() from it, would mix the two. Refused like a changed setting.
  if (outcome_range_unrecorded(stored, settings$outcome_range)) {

    cmp$differ  <- c(cmp$differ, "outcome_range")
    cmp$missing <- setdiff(cmp$missing, "outcome_range")

  }

  hash_known <- !is.na(row_fp$data_hash) && !is.na(expected)

  verdict <- if ((hash_known && !identical(row_fp$data_hash, expected)) ||
                 length(data_cmp$differ) > 0) {
    "data_mismatch"
  } else if (length(cmp$differ) > 0) {
    "settings_mismatch"
  } else if (!identical(checkpoint_row_schema(row), SCORING_SCHEMA)) {
    "foreign_schema"
  } else {
    "keep"
  }

  list(
    verdict           = verdict,
    data_verified     = hash_known && !is.null(data_fp$data_fields) &&
                          length(data_cmp$missing) == 0,
    settings_verified = !is.null(settings) && length(cmp$missing) == 0,
    fingerprint       = row_fp,
    data_differ       = data_cmp$differ,
    stored_settings   = stored,
    settings_differ   = cmp$differ
  )

}

#' Gate checkpoint rows and keep the ones a run may resume
#'
#' @param candidates `rows` from [read_checkpoint_store()].
#' @param data_fp,settings This run's fingerprint and settings; see
#'   [checkpoint_row_verdict()].
#' @param config_ids The configs in this run's grid, or `NULL` to keep every
#'   id.
#' @return List with `kept` (surviving candidates named by config id, the
#'   first per id), `refused` (candidates that failed a fingerprint, each with
#'   its verdict fields added), and the counts `n_foreign` (earlier scoring
#'   schema), `n_stale` (config not in the grid), `n_unverified_data` and
#'   `n_unverified_settings` (among the kept rows).
#' @keywords internal
#' @noRd
gate_checkpoint_rows <- function(candidates, data_fp, settings, config_ids = NULL) {

  verdicts <- lapply(candidates, function(cand) {
    checkpoint_row_verdict(cand$row, data_fp, settings)
  })

  verdict <- vapply(verdicts, `[[`, character(1), "verdict")
  ids     <- vapply(candidates, function(cand) {
    as.character(cand$row$config_id)
  }, character(1))

  in_grid <- if (is.null(config_ids)) rep(TRUE, length(ids)) else ids %in% config_ids
  passed  <- verdict == "keep"

  ## First candidate per id: per-config files come before legacy rows.
  keep <- which(passed & in_grid)
  keep <- keep[!duplicated(ids[keep])]

  refused <- which(verdict %in% c("data_mismatch", "settings_mismatch"))

  list(
    kept                  = stats::setNames(candidates[keep], ids[keep]),
    refused               = Map(c, candidates[refused], verdicts[refused]),
    n_foreign             = sum(verdict == "foreign_schema"),
    n_stale               = sum(passed & !in_grid),
    n_unverified_data     = sum(!vapply(verdicts[keep], `[[`, logical(1), "data_verified")),
    n_unverified_settings = sum(!vapply(verdicts[keep], `[[`, logical(1), "settings_verified"))
  )

}

#' Load the checkpoint rows evaluate() may resume
#'
#' Reads the store, gates every row, and reports: aborts on the first row
#' written on other training data or under other settings, returns the drops
#' as notes for `evaluate()` to print inside its tree (they used to print
#' here, above the tree's header; #91), warns naming any file it could not
#' read, copies rows adopted from a legacy single file into the per-config
#' store (so the legacy file can then be deleted), and warns once for rows it
#' cannot verify.
#'
#' @param output_dir The run's output directory.
#' @param config_ids The configs in this run's grid.
#' @param data_fp,settings This run's fingerprint and settings.
#' @param call The frame to report a refusal from; the default is the
#'   caller's, so the error reads as `evaluate()`'s.
#' @return List with `rows` (one-row results named by config id),
#'   `legacy_ids` (the ids among them read from a legacy
#'   `eval_checkpoint.rds`, which still holds them after they are copied) and
#'   `notes` (character, one tree line per kind of dropped row; empty when
#'   none were dropped).
#' @keywords internal
#' @noRd
load_eval_checkpoints <- function(output_dir, config_ids, data_fp, settings,
                                  call = rlang::caller_env()) {

  store <- read_checkpoint_store(output_dir)
  gated <- gate_checkpoint_rows(store$rows, data_fp, settings, config_ids)

  ## -------------------------------------------------------------------------
  ## Refuse a directory holding another run's results
  ## -------------------------------------------------------------------------

  if (length(gated$refused) > 0) {

    first <- gated$refused[[1]]
    diffs <- describe_settings_diff(first$stored_settings, settings,
                                    first$settings_differ)

    if (first$verdict == "data_mismatch") {

      abort_checkpoint_data_mismatch(
        stored        = first$fingerprint,
        current       = data_fp,
        output_dir    = output_dir,
        source        = first$source,
        differ        = first$data_differ,
        settings_diff = diffs,
        call          = call
      )

    }

    abort_checkpoint_settings_mismatch(diffs, output_dir, first$source,
                                       call = call)

  }

  ## -------------------------------------------------------------------------
  ## Report what was dropped, and what could not be read
  ## -------------------------------------------------------------------------

  notes <- character(0)

  if (gated$n_stale > 0) {

    notes <- c(notes, paste0("Dropped ", gated$n_stale,
                             " stale checkpoint entries"))

  }

  ## Rows scored under a different regime (SCORING_SCHEMA) are not comparable
  ## to what this run produces, and ranking them together would make
  ## best_config an artifact of which regime scored each config.
  if (gated$n_foreign > 0) {

    notes <- c(notes, paste0(
      "Dropped ", gated$n_foreign, " checkpoint row",
      if (gated$n_foreign > 1) "s" else "",
      " scored under an earlier scoring schema (will be re-evaluated)"
    ))

  }

  if (length(store$unreadable) > 0) {

    warn_unreadable_checkpoints(store$unreadable, output_dir)

  }

  ## -------------------------------------------------------------------------
  ## Move adopted legacy rows into the per-config store
  ## -------------------------------------------------------------------------

  from_legacy <- Filter(function(k) is.na(k$path), gated$kept)

  if (length(from_legacy) > 0) {

    for (k in from_legacy) {
      write_checkpoint_row(k$row, file.path(output_dir, "checkpoints"))
    }

    n_legacy <- length(from_legacy)

    cli::cli_inform(c(
      "i" = "Resumed {n_legacy} config{?s} from the legacy {.file eval_checkpoint.rds} and copied {?it/them} into {.path checkpoints/}; delete {.file eval_checkpoint.rds} once this run has resumed."
    ), class = "horizons_checkpoint_message")

  }

  ## One warning per run, whatever the mix of old rows: they keep resuming,
  ## but never silently.
  if (gated$n_unverified_data > 0 || gated$n_unverified_settings > 0) {

    warn_unverified_checkpoints(gated$n_unverified_data,
                                gated$n_unverified_settings,
                                output_dir, data_fp, settings)

  }

  list(
    rows       = lapply(gated$kept, `[[`, "row"),
    legacy_ids = names(from_legacy) %||% character(0),
    notes      = notes
  )

}

#' Write one result row to the per-config store
#'
#' Written under a name the store does not list and renamed into place, so a
#' reader (the monitor polls while workers write) never sees half a file.
#'
#' @param row One-row result.
#' @param checkpoint_dir The `checkpoints/` directory.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
write_checkpoint_row <- function(row, checkpoint_dir) {

  tmp <- tempfile(tmpdir = checkpoint_dir, fileext = ".rds.tmp")
  saveRDS(row, tmp)
  file.rename(tmp, file.path(checkpoint_dir, paste0(row$config_id, ".rds")))

  invisible(NULL)

}

#' Warn about files in the store that could not be used
#'
#' @param unreadable Reasons, named by file relative to `output_dir`: a read
#'   error, not a result row, or a row under a name other than its config's.
#' @param output_dir The run's output directory.
#' @return `NULL`, invisibly; warns with class `horizons_checkpoint_warning`.
#' @keywords internal
#' @noRd
warn_unreadable_checkpoints <- function(unreadable, output_dir) {

  n_files <- length(unreadable)
  details <- paste0(names(unreadable), ": ", unname(unreadable))

  ## Each detail is substituted, not inlined, so braces in upstream error
  ## text are never read as cli markup.
  cli::cli_warn(c(
    "!" = "{n_files} file{?s} in {.path {output_dir}} could not be used as {cli::qty(n_files)}{?a checkpoint/checkpoints}.",
    stats::setNames(sprintf("{details[%d]}", seq_len(n_files)),
                    rep("x", n_files)),
    "i" = "A config with no usable checkpoint of its own is re-evaluated. Delete or replace {cli::qty(n_files)}{?the file/these files} once the run has finished."
  ), class = "horizons_checkpoint_warning")

  invisible(NULL)

}

#' Warn once about resumed rows that cannot be verified
#'
#' @param n_data Rows whose training-data fingerprint is absent or does not
#'   cover every field this run records.
#' @param n_settings Rows whose settings stamp is absent or does not cover
#'   every setting this run records.
#' @param output_dir The run's output directory.
#' @param data_fp,settings This run's fingerprint and settings.
#' @return `NULL`, invisibly; warns with class `horizons_checkpoint_warning`.
#' @keywords internal
#' @noRd
warn_unverified_checkpoints <- function(n_data, n_settings, output_dir,
                                        data_fp, settings) {

  n_rows      <- data_fp$data_n_rows
  data_nms    <- names(data_fp$data_fields)
  setting_nms <- names(settings)

  bullets <- c("!" = "Some resumed checkpoints in {.path {output_dir}} cannot be verified against this run.")

  if (n_data > 0) {

    bullets <- c(bullets, "*" = if (length(data_nms) > 0) {
      "{n_data} {?has/have} no training-data fingerprint covering {.field {data_nms}}, so {cli::qty(n_data)}{?it/they} cannot be confirmed to come from these {n_rows} training rows and their values."
    } else {
      "{n_data} {?has/have} no training-data fingerprint, or this run has no sample id column to check one against, so {cli::qty(n_data)}{?it/they} cannot be confirmed to come from these {n_rows} training rows."
    })

  }

  if (n_settings > 0) {

    bullets <- c(bullets, "*" = "{n_settings} {?has/have} no tuning-settings fingerprint covering {.field {setting_nms}}, so {cli::qty(n_settings)}{?it/they} cannot be confirmed to have been tuned the way this run tunes.")

  }

  bullets <- c(bullets, "i" = "Written before provenance was recorded. Resuming anyway; delete them, or use a fresh {.arg output_dir}, if the rows or settings may differ.")

  cli::cli_warn(bullets, class = "horizons_checkpoint_warning")

  invisible(NULL)

}

## ---------------------------------------------------------------------------
## Checkpoint settings provenance
## ---------------------------------------------------------------------------
## The data fingerprint identifies the rows a checkpoint was scored on; this
## records how it was tuned. Settings are compared by name rather than hashed
## whole, so a setting added to the list later makes older rows unverified
## (warned) rather than mismatched (refused), and a refusal can name what
## changed.

#' Record the settings a result row depends on
#'
#' @param ... Named settings. Numbers are stored as doubles, so `10L` and `10`
#'   record the same value.
#' @return Named list.
#' @keywords internal
#' @noRd
eval_settings <- function(...) {

  normalize_eval_settings(list(...))

}

#' @rdname eval_settings
#' @noRd
normalize_eval_settings <- function(settings) {

  lapply(settings, function(v) if (is.numeric(v)) as.double(v) else v)

}

#' Attach the settings record to a result row
#'
#' @param row One-row result.
#' @param settings Record from [eval_settings()].
#' @return `row` with a `settings` list-column.
#' @keywords internal
#' @noRd
stamp_eval_settings <- function(row, settings) {

  row$settings <- list(settings)
  row

}

#' Settings record carried by one checkpoint row
#'
#' @param row One-row result read back from a checkpoint.
#' @return Named list, or `NULL` for rows written before the stamp existed.
#' @keywords internal
#' @noRd
checkpoint_row_settings <- function(row) {

  if (!"settings" %in% names(row)) return(NULL)

  s <- row[["settings"]][[1]]

  if (!is.list(s)) return(NULL)

  normalize_eval_settings(s)

}

#' Compare a row's record with this run's, field by field
#'
#' Used for the tuning settings and for the training-data fields alike.
#'
#' @param stored The row's record, or `NULL`.
#' @param current This run's record, or `NULL` when unknown.
#' @return List with `differ` (fields both record, with different values)
#'   and `missing` (fields this run records and the row does not).
#' @keywords internal
#' @noRd
compare_record <- function(stored, current) {

  stored  <- normalize_eval_settings(stored %||% list())
  current <- normalize_eval_settings(current %||% list())

  ## `%in%` rather than intersect()/setdiff(): this runs once per checkpoint
  ## row on every monitor poll, and the set functions cost a tryCatch each.
  wanted  <- as.character(names(current))
  present <- wanted %in% names(stored)
  shared  <- wanted[present]

  same <- vapply(shared, function(nm) identical(stored[[nm]], current[[nm]]),
                 logical(1), USE.NAMES = FALSE)

  list(
    differ  = shared[!same],
    missing = wanted[!present]
  )

}

#' Is a result row's range unrecorded under a range other than the default?
#'
#' @description
#' Rows stamped before the outcome range existed (#76) carry no
#' `outcome_range` in their settings record; they were scored with the zero
#' floor, which is the default range. Under the default such a row is merely
#' unverified. Under any other range it was scored differently, and the
#' checkpoint gate and `fit()` refuse it.
#'
#' @param stored The row's settings record, or `NULL` when it has none.
#' @param current_range This run's range, or `NULL` when unknown (a manifest
#'   older than the range).
#' @return `TRUE` when the row has no recorded range and `current_range` is
#'   not `DEFAULT_OUTCOME_RANGE`.
#' @keywords internal
#' @noRd
outcome_range_unrecorded <- function(stored, current_range) {

  !is.null(current_range) && is.null(stored$outcome_range) &&
    !identical(as.double(current_range), DEFAULT_OUTCOME_RANGE)

}

#' Describe differing settings for a message
#'
#' @param stored,current Settings records.
#' @param differ Names of the settings that differ.
#' @return Character vector, one `"name = old (this run: new)"` per setting.
#' @keywords internal
#' @noRd
describe_settings_diff <- function(stored, current, differ) {

  vapply(differ, function(nm) {
    paste0(nm, " = ", format_setting_value(stored[[nm]]),
           " (this run: ", format_setting_value(current[[nm]]), ")")
  }, character(1), USE.NAMES = FALSE)

}

#' Format one setting's value for a message
#'
#' @param v A setting's value.
#' @return Character scalar.
#' @keywords internal
#' @noRd
format_setting_value <- function(v) {

  if (is.null(v)) return("unset")

  if (length(v) == 1 && is.na(v)) return("NA")

  ## trim: format() pads a vector's elements to one width, which put a
  ## second space into "-Inf,  Inf" for the outcome range (#76)
  paste(format(v, trim = TRUE), collapse = ", ")

}

#' Abort on checkpoints tuned under different settings
#'
#' @param diffs Differences from [describe_settings_diff()].
#' @param output_dir The checkpoint directory being resumed.
#' @param source Which file carried the mismatching settings.
#' @param call The frame to report the error from (`evaluate()`'s).
#' @return Never returns; aborts with class `horizons_input_error`.
#' @keywords internal
#' @noRd
abort_checkpoint_settings_mismatch <- function(diffs, output_dir, source,
                                               call = rlang::caller_env()) {

  cli::cli_abort(c(
    "Checkpoints in {.path {output_dir}} were tuned under different settings.",
    "x" = "{.file {source}} was tuned with {diffs}.",
    "i" = "Resuming would rank results tuned under two sets of settings together, and warm-start {.fn fit} from them.",
    "i" = "Use a different {.arg output_dir}, or delete the stale checkpoints in {.path {output_dir}}."
  ), class = "horizons_input_error", call = call)

}

## ---------------------------------------------------------------------------
## Checkpoint data provenance
## ---------------------------------------------------------------------------
## A checkpoint is keyed by config_id, which identifies the pipeline but not
## the rows it was scored on. These helpers attach the identity of the
## training rows to every checkpoint write and check it on every read.

#' Fingerprint the training data a run is scoring
#'
#' The hash is over the sorted identifier column plus the name of the outcome
#' column, so it is invariant to row order and to the split's RNG but changes
#' the moment either the row set or the response changes.
#'
#' The outcome is in the hash because `generate_config_id()` does not hash it:
#' `configure(outcome = "clay")` and `configure(outcome = "oc")` on the same
#' rows produce the same config ids, so without this the two runs would resume
#' each other's CV results in a shared `output_dir`. That became reachable when
#' `select_training()` started returning every pool response on one object. The
#' per-config transformation needs no such treatment — it is already part of
#' the config id, so it cannot collide.
#'
#' The hash says nothing about the values on those rows, so re-standardized
#' spectra (#64 moved the grid for the same samples) or a rescaled outcome
#' under the same name resumed stale results silently (#42). `data_fields`
#' records them by name: the outcome, the sample ids, the role-map rows whose
#' roles shape a row (`id`, `outcome`, `predictor`, `covariate`), and the
#' outcome, predictor and covariate values in id order. They are compared
#' field by field, like the tuning settings, so a row written before a field
#' existed is unverified (warned) rather than refused, and a refusal can say
#' what changed. Each value field hashes column by column, so the cost is one
#' pass over the matrix (about 0.3 s at 17,788 x 1,701) with one column in
#' memory at a time; it runs once per run, and the parallel worker is sent
#' the result rather than recomputing it.
#'
#' @param train_data Training rows (the analysis half of the split).
#' @param role_map The object's role map; the `"id"` role names the identifier
#'   column (falling back to `sample_id`), the `"outcome"` role the response,
#'   and the `"predictor"` and `"covariate"` roles the value columns hashed.
#' @return List with `data_hash` (character, `NA` when no identifier column is
#'   available), `data_n_rows` (integer) and `data_fields` (named list of
#'   character: `outcome`, `ids`, `roles`, `outcome_values`, `predictors`,
#'   `covariates`; `NULL` when no identifier column is available).
#' @keywords internal
#' @noRd
eval_data_fingerprint <- function(train_data, role_map = NULL) {

  has_roles <- !is.null(role_map) && "role" %in% names(role_map)

  id_col <- if (has_roles) {
    role_map$variable[role_map$role == "id"]
  } else {
    character(0)
  }

  id_col <- if (length(id_col) > 0) id_col[1] else "sample_id"

  outcome_col <- if (has_roles) {
    role_map$variable[role_map$role == "outcome"]
  } else {
    character(0)
  }

  outcome_col <- if (length(outcome_col) > 0) {
    sort(as.character(outcome_col))
  } else {
    NA_character_
  }

  ## No identifier column: the run is unverifiable rather than wrongly
  ## verified. Resume then behaves as it does for a legacy checkpoint.
  if (!id_col %in% names(train_data)) {

    return(list(
      data_hash   = NA_character_,
      data_n_rows = as.integer(nrow(train_data)),
      data_fields = NULL
    ))

  }

  ids <- as.character(train_data[[id_col]])

  ## Unchanged from its first version: every existing checkpoint carries it,
  ## so any change here would refuse them all.
  data_hash <- digest::digest(list(
    ids     = sort(ids),
    outcome = outcome_col
  ))

  ## -------------------------------------------------------------------------
  ## The values on those rows, by name
  ## -------------------------------------------------------------------------
  ## Radix order is locale-independent, so the laptop and the box agree.

  by_id <- order(ids, method = "radix")

  role_cols <- function(role) {
    if (has_roles) as.character(role_map$variable[role_map$role == role]) else character(0)
  }

  ## Only the roles that shape a row's contents. build_recipe() models the
  ## outcome on the predictors and on covariates (held, then promoted per
  ## config), and the id names the rows; a sibling `response` becomes
  ## `response_hold` and `meta` stays `meta`, neither reaching the model. So
  ## add_response() of another property, or a new meta column, between two
  ## runs does not refuse a resume.
  roles <- if (has_roles) {
    shaping <- role_map$role %in% c("id", "outcome", "predictor", "covariate")
    data.frame(variable = as.character(role_map$variable[shaping]),
               role     = as.character(role_map$role[shaping]))
  } else {
    NULL
  }

  data_fields <- list(
    outcome        = paste(outcome_col, collapse = ", "),
    ids            = digest::digest(ids[by_id], algo = "xxhash64"),
    roles          = digest::digest(roles, algo = "xxhash64"),
    outcome_values = hash_columns(train_data, role_cols("outcome"), by_id),
    predictors     = hash_columns(train_data, role_cols("predictor"), by_id),
    covariates     = hash_columns(train_data, role_cols("covariate"), by_id)
  )

  list(
    data_hash   = data_hash,
    data_n_rows = as.integer(nrow(train_data)),
    data_fields = data_fields
  )

}

#' Hash columns of a table, in a given row order
#'
#' One column at a time, so only one reordered column is in memory, and the
#' column names are part of the hash.
#'
#' @param data Data frame.
#' @param cols Columns to hash; any not in `data` are skipped.
#' @param ord Row order.
#' @return Character scalar.
#' @keywords internal
#' @noRd
hash_columns <- function(data, cols, ord) {

  cols <- intersect(cols, names(data))

  per_column <- vapply(cols, function(cl) {
    digest::digest(data[[cl]][ord], algo = "xxhash64")
  }, character(1))

  digest::digest(per_column, algo = "xxhash64")

}

#' Attach a fingerprint to a result row
#'
#' @param row One-row result tibble.
#' @param fp Fingerprint from [eval_data_fingerprint()].
#' @return `row` with `data_hash` and `data_n_rows` columns and a
#'   `data_fields` list-column.
#' @keywords internal
#' @noRd
stamp_data_fingerprint <- function(row, fp) {

  row$data_hash   <- fp$data_hash
  row$data_n_rows <- fp$data_n_rows
  row$data_fields <- list(fp$data_fields)
  row

}

#' Fingerprint carried by one checkpoint row
#'
#' @param row One-row result tibble read back from a checkpoint.
#' @return List with `data_hash` and `data_n_rows`, both `NA` for rows written
#'   before the fingerprint existed, and `data_fields`, `NULL` for rows
#'   written before the fields existed.
#' @keywords internal
#' @noRd
checkpoint_row_fingerprint <- function(row) {

  ## `[[`, as in checkpoint_row_schema(): tibble's `$` warns on the missing
  ## columns of the old rows this reads.
  h <- if ("data_hash" %in% names(row)) row[["data_hash"]] else NULL
  n <- if ("data_n_rows" %in% names(row)) row[["data_n_rows"]] else NULL
  f <- if ("data_fields" %in% names(row)) row[["data_fields"]][[1]] else NULL

  list(
    data_hash   = if (is.null(h) || length(h) != 1 || is.na(h)) {
      NA_character_
    } else {
      as.character(h)
    },
    data_n_rows = if (is.null(n) || length(n) != 1 || is.na(n)) {
      NA_integer_
    } else {
      as.integer(n)
    },
    data_fields = if (is.list(f)) f else NULL
  )

}

#' Per-row fingerprints carried by a single-file checkpoint
#'
#' Reads the per-row columns when present, and falls back to the tibble's
#' attributes, which is how a checkpoint written wholesale (rather than row by
#' row) carries its provenance. Rows with neither are `NA`.
#'
#' @param results Checkpoint tibble (a legacy `eval_checkpoint.rds`).
#' @return List of two vectors, `data_hash` and `data_n_rows`, each of length
#'   `nrow(results)`.
#' @keywords internal
#' @noRd
checkpoint_tibble_fingerprints <- function(results) {

  n <- nrow(results)

  hashes <- if ("data_hash" %in% names(results)) {
    as.character(results$data_hash)
  } else {
    rep(attr(results, "data_hash") %||% NA_character_, n)
  }

  rows <- if ("data_n_rows" %in% names(results)) {
    as.integer(results$data_n_rows)
  } else {
    rep(as.integer(attr(results, "data_n_rows") %||% NA_integer_), n)
  }

  ## Rows written before the columns existed bind in as NA; the tibble-level
  ## attribute is the only provenance they can have.
  attr_hash <- attr(results, "data_hash")

  if (!is.null(attr_hash) && any(is.na(hashes))) {

    hashes[is.na(hashes)] <- attr_hash

  }

  list(data_hash = hashes, data_n_rows = rows)

}

#' Abort on a checkpoint written against different training data
#'
#' @param stored,current Fingerprints from [checkpoint_row_fingerprint()] and
#'   [eval_data_fingerprint()].
#' @param output_dir The checkpoint directory being resumed.
#' @param source Which file carried the mismatching fingerprint.
#' @param differ Names of the `data_fields` that differ. Rows written before
#'   the fields existed can name none; the message then shows the two hashes.
#' @param settings_diff Settings the same row records with another value,
#'   from [describe_settings_diff()]. A changed `seed` moves the split, so it
#'   surfaces here as a data mismatch; naming it says why.
#' @param call The frame to report the error from (`evaluate()`'s).
#' @return Never returns; aborts with class `horizons_input_error`.
#' @keywords internal
#' @noRd
abort_checkpoint_data_mismatch <- function(stored, current, output_dir, source,
                                           differ = character(0),
                                           settings_diff = character(0),
                                           call = rlang::caller_env()) {

  what <- describe_data_diff(stored$data_fields, current$data_fields, differ)

  ## Descriptions are substituted, not inlined, so a brace in an outcome name
  ## is never read as cli markup.
  found <- if (length(what) > 0) {
    stats::setNames(sprintf("{.file {source}} %s", sprintf("{what[%d]}", seq_along(what))),
                    rep("x", length(what)))
  } else {
    c("x" = "{.file {source}} carries hash {.val {stored$data_hash}} over {stored$data_n_rows} training row{?s}.",
      "i" = "This run's training rows hash to {.val {current$data_hash}} over {current$data_n_rows} row{?s}.")
  }

  cli::cli_abort(c(
    "Checkpoints in {.path {output_dir}} were written on different training data.",
    found,
    if (length(settings_diff) > 0) c("i" = "That checkpoint was also tuned with {settings_diff}, which may be why."),
    "i" = "Resuming would reuse cross-validated results, and warm-start {.fn fit}, from hyperparameters tuned on the wrong rows.",
    if ("outcome" %in% differ) c("i" = "Keep one {.arg output_dir} per outcome."),
    "i" = "Use a different {.arg output_dir}, or delete the stale checkpoints in {.path {output_dir}}."
  ), class = "horizons_input_error", call = call)

}

#' Describe differing data fields for a message
#'
#' @param stored,current `data_fields` records.
#' @param differ Names of the fields that differ.
#' @return Character vector, one clause per field, each completing a sentence
#'   that starts with the checkpoint's file name.
#' @keywords internal
#' @noRd
describe_data_diff <- function(stored, current, differ) {

  clauses <- c(
    ids            = "was scored on a different set of training samples.",
    roles          = "was scored with different id, outcome, predictor or covariate columns (role_map).",
    outcome_values = "was scored on different outcome values for the same samples.",
    predictors     = "was scored on different predictor values for the same samples (re-standardized or re-processed spectra, for example).",
    covariates     = "was scored on different covariate values for the same samples."
  )

  out <- character(0)

  if ("outcome" %in% differ) {

    out <- paste0("was computed for outcome ", stored$outcome,
                  "; this run models ", current$outcome, ".")

  }

  c(out, unname(clauses[intersect(names(clauses), differ)]))

}

## ---------------------------------------------------------------------------
## abort_all_configs_failed — nothing to rank, with the reasons attached
## ---------------------------------------------------------------------------

#' Abort because no configuration can be ranked
#'
#' @description
#' Raised by `evaluate()` when no configuration can be ranked: every one
#' failed, or was pruned without a cross-validated value of the ranking
#' metric (#41), or succeeded without one (rows checkpointed before the
#' `cv_*` columns existed). `evaluate()` aborts before it assigns
#' `x$evaluation`, so the results table travels on the condition as
#' `results`, and the message lists the distinct error messages (the first
#' three, each with the configs that raised it, and a count of the rest). A
#' caller looping over subsets can catch the class and keep the per-config
#' errors; see `evaluate()`'s "When every configuration fails" section.
#'
#' Rows resumed from checkpoints are not re-run by calling `evaluate()`
#' again, so the message names them and the files to delete. They are
#' identified by `evaluate()` from the ids its loader returned, without
#' reading the checkpoint files again. Each lives in
#' `checkpoints/<config_id>.rds`; a row read from a legacy
#' `eval_checkpoint.rds` was copied there, but the legacy file still holds
#' it and would be read again once the copy is gone, so that file is named
#' too, and only then.
#'
#' @param results The aggregated result rows (`evaluation$results` shape).
#' @param metric Bare ranking metric name.
#' @param checkpoint_ids Character. Config ids among `results` that were
#'   loaded from checkpoints rather than run. Default none.
#' @param legacy_ids Character. The ids among `checkpoint_ids` read from a
#'   legacy `eval_checkpoint.rds`. Default none.
#' @param output_dir The checkpoint directory, or `NULL`.
#' @param call The call the condition is attributed to. Default: the caller,
#'   `evaluate()`.
#' @return Never returns; aborts with class `horizons_all_configs_failed`.
#' @keywords internal
#' @noRd
abort_all_configs_failed <- function(results, metric,
                                     checkpoint_ids = character(0),
                                     legacy_ids     = character(0),
                                     output_dir     = NULL,
                                     call           = rlang::caller_env()) {

  rank_column <- paste0("cv_", metric)
  n_total     <- nrow(results)
  n_success   <- sum(results$status == "success")
  n_failed    <- sum(results$status == "failed")
  n_pruned    <- sum(results$status == "pruned")

  header <- if (n_success > 0) {
    "No configuration can be ranked: {n_success} succeeded, but none has a {.field {rank_column}} value."
  } else {
    "All configurations failed or were pruned without a {.field {rank_column}} value, so none can be ranked."
  }

  ## Resumed rows are not re-run by calling evaluate() again, so a failure
  ## loaded from a checkpoint persists until its files are deleted. Built as
  ## plain text (the directory is the caller's), escaped for cli.
  checkpoint_note <- NULL

  if (length(checkpoint_ids) > 0 && !is.null(output_dir)) {

    n_ckpt    <- length(checkpoint_ids)
    ids_shown <- paste(utils::head(checkpoint_ids, 3), collapse = ", ")

    if (n_ckpt > 3) ids_shown <- paste0(ids_shown, " and ", n_ckpt - 3, " more")

    files <- if (n_ckpt == 1) {
      paste0("checkpoints/", checkpoint_ids, ".rds")
    } else {
      "their files, checkpoints/<config_id>.rds"
    }

    n_legacy    <- length(legacy_ids)
    legacy_note <- if (n_legacy == 0) {
      ""
    } else if (n_legacy == n_ckpt) {
      paste0(", and eval_checkpoint.rds, which ",
             if (n_ckpt == 1) "it was" else "they were", " read from")
    } else {
      paste0(", and eval_checkpoint.rds, which ", n_legacy, " of them ",
             if (n_legacy == 1) "was" else "were", " read from")
    }

    checkpoint_note <- c("i" = cli_escape(paste0(
      n_ckpt, if (n_ckpt == 1) " configuration was" else " configurations were",
      " loaded from checkpoints in ", output_dir, " rather than run (",
      ids_shown, "). To re-run ", if (n_ckpt == 1) "it" else "them",
      ", delete ", files, legacy_note, "."
    )))

  }

  cli::cli_abort(c(
    header,
    "i" = "Of {n_total} configuration{?s}: {n_success} succeeded, {n_failed} failed, {n_pruned} pruned.",
    distinct_config_errors(results),
    checkpoint_note,
    "i" = "The per-config results, error messages included, are on this condition as {.field results}. Recover them without re-running with {.code rlang::last_error()$results}, or catch the class: {.code tryCatch(evaluate(x), horizons_all_configs_failed = function(e) e$results)}."
  ), class = "horizons_all_configs_failed", results = results, call = call)

}

## ---------------------------------------------------------------------------
## relabel_inert_pruned \u2014 the one relabel evaluate() and the monitor share
## ---------------------------------------------------------------------------

#' Relabel "pruned" rows as successes when there was nothing to prune
#'
#' @description
#' At `bayesian_iter = 0` the prune gate skips nothing, so a "pruned" row ran
#' exactly what a success runs (#38). Rows checkpointed before that fix still
#' carry the label, and a resumed run would rank them only as a fallback
#' where a fresh run ranks them with the rest. `evaluate()` applies this once
#' checkpointed and new rows are combined, rather than in the loader, and
#' `monitor_evaluate()` applies it to the same rows with `bayesian_iter` from
#' the manifest's settings, so the two name the same best config.
#' `below_prune_threshold` keeps what the label said about quality.
#'
#' @param results Result rows.
#' @param bayesian_iter The run's Bayesian iterations; `NULL` (a manifest
#'   that does not record it) leaves the rows as they are.
#' @return `results`, relabelled when `bayesian_iter` is 0.
#' @keywords internal
#' @noRd
relabel_inert_pruned <- function(results, bayesian_iter) {

  if (!isTRUE(bayesian_iter == 0)) return(results)

  if (!"below_prune_threshold" %in% names(results)) results$below_prune_threshold <- NA

  inert_pruned <- results$status %in% "pruned"

  results$below_prune_threshold[inert_pruned] <- TRUE
  results$status[inert_pruned]                <- "success"

  results

}

## ---------------------------------------------------------------------------
## ranking_candidates \u2014 the one candidate rule evaluate() and fit() share
## ---------------------------------------------------------------------------

#' The result rows eligible for ranking
#'
#' @description
#' Returns the rows `evaluate()` picks `best_config` from and `fit()` picks
#' its members from: the configurations that succeeded, or, when none did,
#' the pruned configurations that carry a cross-validated value of the
#' ranking metric. [rank_configs_by_cv()] then orders them. `fit()` used to
#' keep successes only, so it refused an evaluation whose `best_config` was a
#' pruned configuration (#38).
#'
#' @param results Tibble of evaluation result rows (`evaluation$results`
#'   shape).
#' @param metric Bare metric name, e.g. `"rpd"`.
#' @return List with `rows` (the candidate rows, unranked; zero rows when
#'   nothing qualifies) and `fallback` (`TRUE` when the rows are pruned
#'   configurations because none succeeded).
#' @keywords internal
#' @noRd
ranking_candidates <- function(results, metric) {

  successes <- results[results$status %in% "success", , drop = FALSE]

  if (nrow(successes) > 0) return(list(rows = successes, fallback = FALSE))

  cv_values <- results[[paste0("cv_", metric)]]
  has_cv    <- if (is.null(cv_values)) rep(FALSE, nrow(results)) else !is.na(cv_values)

  pruned <- results[results$status %in% "pruned" & has_cv, , drop = FALSE]

  list(rows = pruned, fallback = nrow(pruned) > 0)

}

## ---------------------------------------------------------------------------
## rank_configs_by_cv \u2014 the one ranking rule evaluate() and fit() share
## ---------------------------------------------------------------------------

#' Rank evaluation result rows on the cross-validated metric
#'
#' @description
#' Orders result rows best-first by `cv_<metric>`, the cross-validated mean at
#' each config's selected hyperparameters. This is the single ranking rule for
#' `evaluate()`'s `best_config` and `fit()`'s member selection, so the two can
#' never disagree, and neither touches a test-set column (#50).
#'
#' Rows whose `cv_<metric>` is `NA` (checkpoints written before the column
#' existed, or a config whose CV panel could not be recovered) are dropped
#' with a warning naming them. If no row can be ranked, aborts with the
#' remedy: re-run `evaluate()`.
#'
#' @param results Tibble of evaluation result rows (`evaluation$results`
#'   shape).
#' @param metric Bare metric name, e.g. `"rpd"`. Direction comes from
#'   `HIGHER_BETTER_METRICS`.
#' @return `results` with unrankable rows removed, ordered best-first.
#' @keywords internal
rank_configs_by_cv <- function(results, metric) {

  rank_column <- paste0("cv_", metric)

  if (!rank_column %in% names(results)) {

    rlang::abort(paste0(
      "Evaluation results carry no `", rank_column, "` column. ",
      "Ranking uses the cross-validated metric; re-run evaluate() to record it."
    ))

  }

  vals <- results[[rank_column]]

  if (all(is.na(vals))) {

    rlang::abort(paste0(
      "All values of `", rank_column, "` are NA, so no config can be ranked. ",
      "Re-run evaluate() to record cross-validated metrics."
    ))

  }

  if (any(is.na(vals))) {

    skipped <- results$config_id[is.na(vals)]

    n_skipped <- length(skipped)

    cli::cli_warn(c(
      "!" = "{n_skipped} config{?s} skipped in ranking: `{rank_column}` is NA.",
      "i" = "Skipped: {.val {skipped}}.",
      "i" = "Usually a checkpoint from before cv_* columns existed; re-run evaluate() to include those configs."
    ))

    results <- results[!is.na(vals), , drop = FALSE]
    vals    <- results[[rank_column]]

  }

  ## Explicit tie-break on config_id so a fresh run, a resumed run (whose row
  ## order follows the checkpoint directory) and monitor_evaluate() all name
  ## the same winner when two configs tie.
  key <- if (metric %in% HIGHER_BETTER_METRICS) -vals else vals

  results[order(key, results$config_id), , drop = FALSE]

}

## ---------------------------------------------------------------------------
## outcome_complete_rows — the one row rule evaluate() and fit() share
## ---------------------------------------------------------------------------

#' Keep the analysis rows whose outcome is observed
#'
#' @description
#' Drops the rows whose outcome is `NA`. This is the single rule for which
#' rows `evaluate()` and `fit()` model (#67). `evaluate()` draws its split
#' from the rows it returns; `fit()` reuses that split and applies the same
#' rule to check that the split's ids and outcomes still match the object's.
#' Before this, `fit()` split the unfiltered table: its partition was over
#' different rows from `evaluate()`'s, and NA-outcome rows reached the fit.
#'
#' Rows are dropped per outcome, here, rather than when responses are joined.
#' An object can carry several responses (`select_training()` puts every pool
#' response on one object), and a row missing one of them may carry another.
#'
#' @param analysis Data frame. The object's analysis table.
#' @param outcome_col Character. Name of the outcome column.
#' @return List with `data` (the rows whose outcome is not `NA`, in their
#'   original order) and `n_dropped` (integer, the rows removed). Aborts with
#'   class `horizons_input_error` when the outcome column is absent from
#'   `analysis` or every outcome is `NA`.
#' @keywords internal
#' @noRd
outcome_complete_rows <- function(analysis, outcome_col) {

  ## An absent column reads as NULL, and is.na(NULL) is empty, so without
  ## this check a missing outcome was reported as "All outcome values are NA".
  absent <- setdiff(outcome_col, names(analysis))

  if (length(outcome_col) == 0 || length(absent) > 0) {

    cli::cli_abort(c(
      "The analysis table has no outcome column to model.",
      "x" = if (length(outcome_col) == 0) {
        "The role map gives no column the {.val outcome} role."
      } else {
        "The role map names {.field {absent}} as the outcome, and {.field data$analysis} has no such column."
      },
      "i" = "Add the column to the analysis table, or correct the role map."
    ), class = "horizons_input_error")

  }

  na_mask <- is.na(analysis[[outcome_col]])

  if (all(na_mask)) {

    cli::cli_abort(c(
      "All outcome values are NA, so there are no rows to model.",
      "i" = "Outcome column: {.field {outcome_col}}."
    ), class = "horizons_input_error")

  }

  ## Return the table untouched when nothing is dropped, so an object without
  ## NA outcomes splits exactly as it did before this helper existed.
  list(
    data      = if (any(na_mask)) analysis[!na_mask, , drop = FALSE] else analysis,
    n_dropped = sum(na_mask)
  )

}

## ---------------------------------------------------------------------------
## draw_eval_split — the one train/test draw evaluate() and fit() share
## ---------------------------------------------------------------------------

#' Draw evaluate()'s train/test split
#'
#' @description
#' The split `evaluate()` scores on and `fit()` reuses: the rows with an
#' observed outcome ([outcome_complete_rows()]), then `set.seed(seed)`, then
#' a `SPLIT_PROP` split stratified on the outcome, falling back to an
#' unstratified one when the stratified draw fails. The split can also come
#' out unstratified with no failure: rsample drops the strata itself when it
#' cannot bin the outcome (under 40 rows, warning "Too little data to
#' stratify"; or silently, when a few-valued outcome pools into one stratum or
#' tied quantiles leave one bin). The object's `strata` attribute names the
#' outcome either way, so `stratified` in the return value is the record of
#' which happened (#91; see [draw_stratified()]). `fit()` calls it when it
#' starts cold from a configured object with one configuration (#45), so that
#' fit holds out exactly the rows `evaluate()` would have at the same seed.
#' The draw was inline in `evaluate()` before, and it is unchanged.
#'
#' It seeds the global RNG, as `evaluate()` always has: `evaluate()`'s CV
#' folds are drawn from the state it leaves.
#'
#' @param analysis Data frame. The object's analysis table. Rows whose
#'   outcome is `NA` are dropped here; a table the callers have already
#'   filtered for their sample-size gate passes through uncopied.
#' @param outcome_col Character. Name of the outcome column.
#' @param seed Integer. The seed passed to `evaluate()` (or to `fit()` on a
#'   cold start).
#' @return List with `split` (the `rsplit`), `n_dropped` (integer, the rows
#'   whose outcome is `NA`), `stratified` (`FALSE` when the split is
#'   unstratified, because rsample dropped the strata or the stratified draw
#'   failed; see [draw_stratified()]) and `strata_failed` (`TRUE` when the
#'   stratified draw failed and was retried without strata). Aborts as
#'   [outcome_complete_rows()] does.
#' @keywords internal
#' @noRd
draw_eval_split <- function(analysis, outcome_col, seed) {

  modelled <- outcome_complete_rows(analysis, outcome_col)

  set.seed(seed)

  drawn <- draw_stratified(
    outcome      = modelled$data[[outcome_col]],
    stratified   = function() rsample::initial_split(modelled$data, prop = SPLIT_PROP,
                                                     strata = dplyr::all_of(outcome_col),
                                                     breaks = STRATA_BREAKS, pool = STRATA_POOL),
    unstratified = function() rsample::initial_split(modelled$data, prop = SPLIT_PROP)
  )

  list(split         = drawn$draw,
       n_dropped     = modelled$n_dropped,
       stratified    = drawn$stratified,
       strata_failed = drawn$strata_failed)

}

## ---------------------------------------------------------------------------
## evaluation_recipe — the window check and the evaluation$recipe record
## ---------------------------------------------------------------------------

#' Check the recipe settings against the spectrum and record them
#'
#' @description
#' Reads the object's recipe settings ([recipe_settings()]) and aborts when
#' the Savitzky-Golay window is at least as wide as the spectrum, which
#' `configure()` cannot check because `standardize()` may still change the
#' axis after it (#62). Past that point such a window fails every config
#' inside tune with a message that never names it. Returns the record
#' `evaluate()` stores as `evaluation$recipe`, the window's width in cm-1 on
#' the axis the recipe runs on included. `fit()`'s cold start (#45) calls it
#' too, so a configured object that skips `evaluate()` is held to the same
#' check and records the same settings.
#'
#' @param x A configured `horizons_data`.
#' @param call The call the condition is attributed to. Default: the caller,
#'   `evaluate()` or `fit()`.
#' @return List with `sg_window`, `sg_window_cm` (`NA` when the axis spacing
#'   cannot be read) and `pca_threshold`. Aborts with class
#'   `horizons_input_error` when the window does not fit the spectrum.
#' @keywords internal
#' @noRd
evaluation_recipe <- function(x, call = rlang::caller_env()) {

  recipe_cfg <- recipe_settings(x)
  n_spectral <- sum(x$data$role_map$role == "predictor")
  window_cm  <- recipe_cfg$sg_window * axis_spacing_cm(x)

  if (recipe_cfg$sg_window >= n_spectral) {

    width <- if (is.na(window_cm)) "" else {
      paste0(" (", signif(window_cm, 3), " cm\u207B\u00B9)")
    }

    cli::cli_abort(c(
      "The Savitzky-Golay window is at least as wide as the spectrum.",
      "x" = "{.arg sg_window} is {recipe_cfg$sg_window} grid points{width}; the object has {n_spectral} spectral column{?s}.",
      "i" = "The window must be narrower than the spectrum. Re-run {.fn configure} with a smaller {.arg sg_window}, or keep more of the spectrum."
    ), class = "horizons_input_error", call = call)

  }

  list(sg_window     = recipe_cfg$sg_window,
       sg_window_cm  = window_cm,
       pca_threshold = recipe_cfg$pca_threshold)

}

## -------------------------------------------------------------------------
## Parallel worker
## -------------------------------------------------------------------------

#' Evaluate One Configuration Inside a Parallel Worker
#'
#' @description
#' The body `evaluate()`'s parallel branch dispatches, kept at top level on
#' purpose.
#'
#' R serializes a closure together with its enclosing environment. A function
#' defined inside `evaluate()` therefore carries every local in that frame to
#' every worker — including the split and the resample object — regardless of
#' what is passed explicitly. Measured on the KSSL clay training set at 2 cm-1
#' (17,788 x 1,701), that was a 2.26 GiB payload per future against 232 MB of
#' genuinely needed inputs, and it was what tripped
#' `future.globals.maxSize` (1e9, installed by `tune` at load).
#'
#' Because this function lives in the package namespace, `future` resolves it by
#' name rather than serializing it, so the payload is exactly `shared`.
#'
#' Resolving by name has one cost, and it is the reason `evaluate()` refuses to
#' dispatch under `pkgload`: the worker loads the **installed** horizons, not the
#' source tree. A compatible-but-stale install would run old helper code behind a
#' new worker body, silently, and the worker computes every metric.
#'
#' @param config_i Integer row index into `shared$configs`. Named to avoid
#'   colliding with `shared$resample_idx`.
#' @param shared List of worker inputs assembled by `evaluate()`, whose keys are
#'   fixed by `SHARED_ARG_NAMES` in `R/constants.R` and asserted on entry:
#'   `data`, `resample_idx` (from `resample_indices()`), `configs`, `role_map`,
#'   `grid_size`, `bayesian_iter`, `prune`, `prune_threshold`, `seed`,
#'   `sg_window`, `pca_threshold` (the object's recipe settings),
#'   `outcome_range` (the range scored predictions are clamped to), `data_fp`
#'   and `settings` (the parent's [eval_data_fingerprint()] and
#'   [eval_settings()] records, stamped on the row as-is), `checkpoint_dir`,
#'   and `pkg_version`. There is no `allow_par`: on
#'   the configs axis tune always runs sequentially inside the worker.
#'
#' @return A one-row result tibble from [evaluate_single_config()].
#' @keywords internal
#' @noRd

evaluate_config_worker <- function(config_i, shared) {

  ## -------------------------------------------------------------------------
  ## Validate the cross-process contract
  ## -------------------------------------------------------------------------
  ## A mis-keyed entry arrives as NULL. Most then error, but `seed` and
  ## `grid_size` do not: set.seed(NULL) reseeds from the clock and
  ## tune_grid(grid = NULL) invents its own grid, so the run would succeed
  ## while being unreproducible or tuned over the wrong space.

  if (!setequal(names(shared), SHARED_ARG_NAMES)) {

    rlang::abort(paste0(
      "Malformed worker payload. Missing: ",
      paste(setdiff(SHARED_ARG_NAMES, names(shared)), collapse = ", ") %||% "-",
      "; unexpected: ",
      paste(setdiff(names(shared), SHARED_ARG_NAMES), collapse = ", ") %||% "-"
    ))

  }

  ## Partial guard on the installed-vs-source hazard: catches a worker loading a
  ## different horizons VERSION than the parent. It cannot catch same-version
  ## source edits, which is why evaluate() also refuses to dispatch under pkgload.
  worker_version <- as.character(utils::packageVersion("horizons"))

  if (!identical(worker_version, shared$pkg_version)) {

    rlang::abort(paste0(
      "Worker loaded horizons ", worker_version, " but the parent is ",
      shared$pkg_version, ". Reinstall the package so workers run matching code."
    ))

  }

  ## Thread pinning that works happens in the PARENT before dispatch
  ## (pin_parent_threads()): OpenBLAS reads its thread count when it loads,
  ## so setting OPENBLAS_NUM_THREADS here, after the library is loaded in
  ## this worker, changed nothing when measured (2026-09-15). What can be
  ## set at runtime in the worker is set here. future already sets
  ## mc.cores = 1 in every multisession worker.
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(1L)
  }
  options(ranger.num.threads = 1L)

  cfg <- shared$configs[config_i, ]

  ## Reconstruct the split and folds from indices. Fold membership is
  ## reproduced by construction, not by replaying the RNG.
  resamples <- rebuild_resamples(shared$data, shared$resample_idx)

  result_row <- evaluate_single_config(
    config_row      = cfg,
    split           = resamples$split,
    cv_folds        = resamples$cv_folds,
    role_map        = shared$role_map,
    grid_size       = shared$grid_size,
    bayesian_iter   = shared$bayesian_iter,
    prune           = shared$prune,
    prune_threshold = shared$prune_threshold,
    allow_par       = FALSE,     # configs axis: tune runs sequentially inside
    seed            = shared$seed,
    sg_window       = shared$sg_window,
    pca_threshold   = shared$pca_threshold,
    outcome_range   = shared$outcome_range
  )

  ## Both provenance records are sent rather than rebuilt. The data
  ## fingerprint hashes the whole predictor matrix, which is a once-per-run
  ## cost, not a per-config one; and a setting added to the list in
  ## evaluate() reaches the worker's rows with no change here.
  result_row <- stamp_data_fingerprint(result_row, shared$data_fp)
  result_row <- stamp_eval_settings(result_row, shared$settings)

  if (!is.null(shared$checkpoint_dir)) {

    write_checkpoint_row(result_row, shared$checkpoint_dir)

  }

  result_row

}
