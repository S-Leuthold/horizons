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
#'   pruned. Default TRUE.
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
#' @param output_dir Character or NULL. If provided, checkpoint results to
#'   disk after each config. Enables resuming interrupted runs. Required when
#'   configs are dispatched to workers. Default NULL (no checkpointing).
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
#' @return A `horizons_eval` object (inherits from `horizons_data`) with
#'   `evaluation$results`, `evaluation$best_config`, `evaluation$split`, and
#'   associated metadata populated, including `evaluation$parallelize_over`
#'   (the axis actually used) and `evaluation$workers` (the worker count the
#'   registered plan offered; 1 when sequential).
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
  ## Step 1: Gate checks
  ## -----------------------------------------------------------------------

  valid_metrics <- c("rpd", "rsq", "rmse", "rrmse", "ccc", "mae")

  if (!metric %in% valid_metrics) {

    rlang::abort(paste0(
      "Invalid `metric`: '", metric, "'. ",
      "Must be one of: ", paste(valid_metrics, collapse = ", ")
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

  set.seed(seed)

  split <- tryCatch(
    rsample::initial_split(analysis, prop = SPLIT_PROP, strata = dplyr::all_of(outcome_col)),
    error = function(e) {

      if (verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow("Stratified split failed, ",
                                       "retrying without strata"), "\n"
        ))

      }

      rsample::initial_split(analysis, prop = SPLIT_PROP)

    }
  )

  train_data <- rsample::training(split)
  test_data  <- rsample::testing(split)
  n_train    <- nrow(train_data)
  n_test     <- nrow(test_data)

  ## -----------------------------------------------------------------------
  ## Step 5: Create CV folds
  ## -----------------------------------------------------------------------

  cv_fold_obj <- tryCatch(
    rsample::vfold_cv(train_data, v = cv_folds, strata = dplyr::all_of(outcome_col)),
    error = function(e) {

      if (verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow("Stratified CV failed, ",
                                       "retrying without strata"), "\n"
        ))

      }

      rsample::vfold_cv(train_data, v = cv_folds)

    }
  )

  ## -----------------------------------------------------------------------
  ## Step 5b: Fingerprint the training rows
  ## -----------------------------------------------------------------------
  ## Checkpoints are keyed by config_id alone, which says nothing about the
  ## rows a result was computed on. A dry run and a real run pointed at the
  ## same output_dir therefore used to resume each other's CV results and
  ## warm-start fit() from hyperparameters tuned on the wrong data, silently
  ## (2026-09-21). The fingerprint is what makes that visible. It covers the
  ## response as well as the rows: the config id does not hash the outcome, so
  ## two responses on one row set collide by id alone.

  data_fp <- eval_data_fingerprint(train_data, role_map)

  ## -----------------------------------------------------------------------
  ## Step 6: Load checkpoints (if any)
  ## -----------------------------------------------------------------------

  checkpoint_path    <- NULL
  checkpoint_dir     <- NULL
  checkpoint_results <- list()
  n_unverified       <- 0L

  if (!is.null(output_dir)) {

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    checkpoint_path <- file.path(output_dir, "eval_checkpoint.rds")
    checkpoint_dir  <- file.path(output_dir, "checkpoints")

    if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir)

    ## Load from single-file checkpoint (legacy / sequential)
    if (file.exists(checkpoint_path)) {

      loaded <- readRDS(checkpoint_path)

      ## Provenance gate: refuse to resume results computed on other rows.
      ## Rows carry the fingerprint in columns; the tibble also carries it in
      ## attributes, which is the fallback for rows that predate the columns.
      ## A legacy checkpoint has neither, and is counted as unverified rather
      ## than trusted.
      stored_fps <- checkpoint_tibble_fingerprints(loaded)
      mismatched <- which(!is.na(stored_fps$data_hash) &
                            stored_fps$data_hash != data_fp$data_hash)

      if (length(mismatched) > 0) {

        abort_checkpoint_data_mismatch(
          stored     = list(
            data_hash   = stored_fps$data_hash[mismatched[1]],
            data_n_rows = stored_fps$data_n_rows[mismatched[1]]
          ),
          current    = data_fp,
          output_dir = output_dir,
          source     = "eval_checkpoint.rds"
        )

      }

      n_unverified <- n_unverified + sum(is.na(stored_fps$data_hash))

      ## Drop any config_ids not in current configs
      valid_mask <- loaded$config_id %in% configs$config_id
      n_stale    <- sum(!valid_mask)

      if (n_stale > 0) {

        loaded <- loaded[valid_mask, ]

        if (verbose) {

          cat(paste0(
            "\u2502  ", cli::col_yellow("Dropped ", n_stale,
                                         " stale checkpoint entries"), "\n"
          ))

        }

      }

      ## Drop rows scored under a different regime (SCORING_SCHEMA): their
      ## cv_* panel is not comparable to what this run will produce, and
      ## ranking them together would make best_config an artifact of which
      ## regime scored each config.
      loaded <- drop_foreign_schema_rows(loaded, verbose = verbose)

      if (nrow(loaded) > 0) {

        for (j in seq_len(nrow(loaded))) {
          checkpoint_results[[ loaded$config_id[j] ]] <- loaded[j, ]
        }

      }

    }

    ## Load from per-config checkpoint files (parallel-safe)
    per_config_files <- list.files(checkpoint_dir, pattern = "\\.rds$",
                                   full.names = TRUE)

    if (length(per_config_files) > 0) {

      n_foreign <- 0L

      for (f in per_config_files) {

        row <- tryCatch(readRDS(f), error = function(e) NULL)

        if (is.null(row)) next

        if (!identical(checkpoint_row_schema(row), SCORING_SCHEMA)) {

          n_foreign <- n_foreign + 1L
          next

        }

        ## Provenance gate, same rule as the single-file checkpoint.
        row_fp <- checkpoint_row_fingerprint(row)

        if (is.na(row_fp$data_hash)) {

          n_unverified <- n_unverified + 1L

        } else if (!identical(row_fp$data_hash, data_fp$data_hash)) {

          abort_checkpoint_data_mismatch(
            stored     = row_fp,
            current    = data_fp,
            output_dir = output_dir,
            source     = file.path("checkpoints", basename(f))
          )

        }

        if (row$config_id %in% configs$config_id &&
            !row$config_id %in% names(checkpoint_results)) {

          checkpoint_results[[ row$config_id ]] <- row

        }

      }

      if (n_foreign > 0 && verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Dropped ", n_foreign, " per-config checkpoint",
            if (n_foreign > 1) "s" else "",
            " scored under an earlier scoring schema (will be re-evaluated)"
          ), "\n"
        ))

      }

    }

    ## One warning per run, whatever the mix of legacy files: old runs keep
    ## resuming, but never silently.
    if (n_unverified > 0) {

      cli::cli_warn(c(
        "!" = "{n_unverified} checkpoint{?s} in {.path {output_dir}} carry no training-data fingerprint.",
        "i" = "They were written before provenance was recorded, so it cannot be confirmed they came from these {data_fp$data_n_rows} training rows.",
        "i" = "Resuming anyway. Delete them, or use a fresh {.arg output_dir}, if the rows may differ."
      ), class = "horizons_checkpoint_warning")

    }

    n_loaded <- length(checkpoint_results)

    if (n_loaded > 0 && verbose) {

      cat(paste0(
        "\u2502  Loaded ", n_loaded, " checkpointed results\n"
      ))

    }

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

    cat(paste0("\u2502  Split: ", n_train, " train / ", n_test, " test (",
               round(100 * SPLIT_PROP), "/", round(100 * (1 - SPLIT_PROP)), ", stratified)\n"))
    cat(paste0("\u2502  Tuning: ", cv_folds, "-fold CV, grid = ",
               tuning$grid_size, ", bayesian = ",
               tuning$bayesian_iter, "\n"))
    cat(paste0("\u2502  Configs: ", n_total, " total",
               if (n_pending < n_total) paste0(" (", n_total - n_pending,
                                                " from checkpoint)") else "",
               "\n"))

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
  ## monitor can say which rows the run in this directory is scoring.

  if (!is.null(output_dir)) {

    manifest <- list(
      schema_version               = 3L,
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
      data_n_rows                  = data_fp$data_n_rows
    )
    saveRDS(manifest, file.path(output_dir, "eval_manifest.rds"))

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
        seed            = seed
      )

      ## Stamp before anything else sees the row, so the in-memory results and
      ## the checkpointed copy carry the same provenance.
      result_row        <- stamp_data_fingerprint(result_row, data_fp)
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

      ## Checkpoint — dual write (single-file + per-config)
      if (!is.null(checkpoint_path)) {

        checkpoint_results[[ cfg$config_id ]] <- result_row

        ## Single-file checkpoint (atomic write, backward compatible)
        checkpoint_tibble <- dplyr::bind_rows(checkpoint_results)
        attr(checkpoint_tibble, "data_hash")   <- data_fp$data_hash
        attr(checkpoint_tibble, "data_n_rows") <- data_fp$data_n_rows
        tmp_ckpt <- tempfile(tmpdir = dirname(checkpoint_path), fileext = ".rds")
        saveRDS(checkpoint_tibble, tmp_ckpt)
        file.rename(tmp_ckpt, checkpoint_path)
        rm(checkpoint_tibble)

        ## Per-config checkpoint (atomic write for parallel safety)
        if (!is.null(checkpoint_dir)) {

          tmp <- tempfile(tmpdir = checkpoint_dir, fileext = ".rds")
          saveRDS(result_row, tmp)
          file.rename(tmp, file.path(checkpoint_dir,
                                     paste0(cfg$config_id, ".rds")))

        }

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

  if (nrow(candidates$rows) == 0) {

    n_failed <- sum(all_results$status == "failed")
    n_pruned <- sum(all_results$status == "pruned")

    rlang::abort(paste0(
      "All configurations failed or were pruned. ",
      "Failed: ", n_failed, ", Pruned: ", n_pruned, ". ",
      "Check evaluation$results for error messages."
    ))

  }

  ranked <- rank_configs_by_cv(candidates$rows, metric)

  best_config_id <- ranked$config_id[1]

  ## -----------------------------------------------------------------------
  ## Step 11: Store evaluation metadata
  ## -----------------------------------------------------------------------

  total_runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  x$evaluation <- list(
    results      = all_results,
    best_config  = best_config_id,
    rank_metric  = metric,
    split        = split,
    n_train      = n_train,
    n_test       = n_test,
    workers      = plan_workers,
    parallelize_over = axis$axis,
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

  s <- row$scoring_schema

  if (is.null(s) || length(s) != 1 || is.na(s)) return(1L)

  as.integer(s)

}

#' Drop result rows scored under a different regime
#'
#' @param results Result rows (possibly from an older checkpoint).
#' @param verbose Print the drop count in the tree.
#' @return `results` restricted to the current `SCORING_SCHEMA`.
#' @keywords internal
#' @noRd
drop_foreign_schema_rows <- function(results, verbose = TRUE) {

  if (nrow(results) == 0) return(results)

  schemas <- if ("scoring_schema" %in% names(results)) {
    ifelse(is.na(results$scoring_schema), 1L, as.integer(results$scoring_schema))
  } else {
    rep(1L, nrow(results))
  }

  keep      <- schemas == SCORING_SCHEMA
  n_foreign <- sum(!keep)

  if (n_foreign > 0 && verbose) {

    cat(paste0(
      "\u2502  ", cli::col_yellow(
        "Dropped ", n_foreign, " checkpoint row",
        if (n_foreign > 1) "s" else "",
        " scored under an earlier scoring schema (will be re-evaluated)"
      ), "\n"
    ))

  }

  results[keep, , drop = FALSE]

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
#' Both inputs are read from arguments the parallel worker also has, so the
#' worker recomputes an identical hash without being sent one.
#'
#' @param train_data Training rows (the analysis half of the split).
#' @param role_map The object's role map; the `"id"` role names the identifier
#'   column (falling back to `sample_id`) and the `"outcome"` role names the
#'   response.
#' @return List with `data_hash` (character, `NA` when no identifier column is
#'   available) and `data_n_rows` (integer).
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
  data_hash <- if (id_col %in% names(train_data)) {
    digest::digest(list(
      ids     = sort(as.character(train_data[[id_col]])),
      outcome = outcome_col
    ))
  } else {
    NA_character_
  }

  list(
    data_hash   = data_hash,
    data_n_rows = as.integer(nrow(train_data))
  )

}

#' Attach a fingerprint to a result row
#'
#' @param row One-row result tibble.
#' @param fp Fingerprint from [eval_data_fingerprint()].
#' @return `row` with `data_hash` and `data_n_rows` columns.
#' @keywords internal
#' @noRd
stamp_data_fingerprint <- function(row, fp) {

  row$data_hash   <- fp$data_hash
  row$data_n_rows <- fp$data_n_rows
  row

}

#' Fingerprint carried by one checkpoint row
#'
#' @param row One-row result tibble read back from a checkpoint.
#' @return List with `data_hash` and `data_n_rows`, both `NA` for rows written
#'   before the fingerprint existed.
#' @keywords internal
#' @noRd
checkpoint_row_fingerprint <- function(row) {

  h <- row$data_hash
  n <- row$data_n_rows

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
    }
  )

}

#' Per-row fingerprints carried by a single-file checkpoint
#'
#' Reads the per-row columns when present, and falls back to the tibble's
#' attributes, which is how a checkpoint written wholesale (rather than row by
#' row) carries its provenance. Rows with neither are `NA`.
#'
#' @param results Checkpoint tibble.
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
#' @param stored,current Fingerprints (`data_hash`, `data_n_rows`).
#' @param output_dir The checkpoint directory being resumed.
#' @param source Which file carried the mismatching fingerprint.
#' @return Never returns; aborts with class `horizons_input_error`.
#' @keywords internal
#' @noRd
abort_checkpoint_data_mismatch <- function(stored, current, output_dir, source) {

  cli::cli_abort(c(
    "Checkpoints in {.path {output_dir}} were written on different training data.",
    "x" = "{.file {source}} carries hash {.val {stored$data_hash}} over {stored$data_n_rows} training row{?s}.",
    "i" = "This run's training rows hash to {.val {current$data_hash}} over {current$data_n_rows} row{?s}.",
    "i" = "Resuming would reuse cross-validated results, and warm-start {.fn fit}, from hyperparameters tuned on the wrong rows.",
    "i" = "Use a different {.arg output_dir}, or delete the stale checkpoints in {.path {output_dir}}."
  ), class = "horizons_input_error")

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
#'   class `horizons_input_error` when every outcome is `NA`.
#' @keywords internal
#' @noRd
outcome_complete_rows <- function(analysis, outcome_col) {

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
#'   `checkpoint_dir`, and `pkg_version`. There is no `allow_par`: on the
#'   configs axis tune always runs sequentially inside the worker.
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
    seed            = shared$seed
  )

  ## Stamp the training-row fingerprint. The worker recomputes it from the
  ## rebuilt split rather than receiving it, so the cross-process contract
  ## (SHARED_ARG_NAMES) is unchanged; the indices came from the parent's
  ## split, so the value is identical by construction.
  result_row <- stamp_data_fingerprint(
    result_row,
    eval_data_fingerprint(rsample::training(resamples$split), shared$role_map)
  )

  ## Atomic per-config checkpoint
  if (!is.null(shared$checkpoint_dir)) {

    tmp <- tempfile(tmpdir = shared$checkpoint_dir, fileext = ".rds")
    saveRDS(result_row, tmp)
    file.rename(tmp, file.path(shared$checkpoint_dir,
                               paste0(cfg$config_id, ".rds")))

  }

  result_row

}
