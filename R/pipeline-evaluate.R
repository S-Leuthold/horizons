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
#'   whose grid-search RPD falls below `prune_threshold`. Default TRUE.
#' @param prune_threshold Numeric. RPD threshold for pruning. Configs with
#'   grid-search RPD below this value skip Bayesian optimization but still
#'   receive test-set metrics from grid-search best. Default 1.0 (the
#'   "no better than the mean" line).
#' @param workers Integer. Total number of cores to use. Default 1L
#'   (sequential). When `workers > 1`, evaluate() automatically splits cores
#'   between outer parallelism (across configs) and inner parallelism (CV
#'   folds). The split is: `inner = min(workers, cv_folds)`, `outer =
#'   floor(workers / inner)`. evaluate() manages its own `future::plan()` and
#'   restores the previous plan on exit.
#' @param output_dir Character or NULL. If provided, checkpoint results to
#'   disk after each config. Enables resuming interrupted runs. Default NULL
#'   (no checkpointing).
#' @param seed Integer. Random seed for train/test split and CV folds.
#'   Default 307L.
#' @param verbose Logical. Print progress tree to console. Default TRUE.
#'
#' @return A `horizons_eval` object (inherits from `horizons_data`) with
#'   `evaluation$results`, `evaluation$best_config`, `evaluation$split`, and
#'   associated metadata populated.
#'
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
#' @param idx Integer row index into `shared$configs`.
#' @param shared List of worker inputs assembled by `evaluate()`: `data`, `idx`
#'   (from [resample_indices()]), `configs`, `role_map`, `grid_size`,
#'   `bayesian_iter`, `prune`, `prune_threshold`, `allow_par`, `seed`, and
#'   `checkpoint_dir`.
#'
#' @return A one-row result tibble from [evaluate_single_config()].
#' @keywords internal
#' @noRd

evaluate_config_worker <- function(idx, shared) {

  ## Pin all threading libraries to 1 thread inside each worker
  Sys.setenv(
    OMP_NUM_THREADS        = 1,
    OPENBLAS_NUM_THREADS   = 1,
    MKL_NUM_THREADS        = 1,
    VECLIB_MAXIMUM_THREADS = 1,
    BLAS_NUM_THREADS       = 1,
    LAPACK_NUM_THREADS     = 1
  )
  options(mc.cores = 1L)

  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(1)
  }

  cfg <- shared$configs[idx, ]

  ## Reconstruct the split and folds from indices. Fold membership is
  ## reproduced by construction, not by replaying the RNG.
  resamples <- rebuild_resamples(shared$data, shared$idx)

  result_row <- evaluate_single_config(
    config_row      = cfg,
    split           = resamples$split,
    cv_folds        = resamples$cv_folds,
    role_map        = shared$role_map,
    grid_size       = shared$grid_size,
    bayesian_iter   = shared$bayesian_iter,
    prune           = shared$prune,
    prune_threshold = shared$prune_threshold,
    allow_par       = shared$allow_par,
    seed            = shared$seed
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


#' @export
evaluate <- function(x,
                     metric          = "rpd",
                     prune           = TRUE,
                     prune_threshold = 1.0,
                     workers         = 1L,
                     output_dir      = NULL,
                     seed            = 307L,
                     verbose         = TRUE) {

  start_time <- Sys.time()

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

  outcome_vals <- analysis[[outcome_col]]
  na_mask      <- is.na(outcome_vals)

  if (all(na_mask)) {

    rlang::abort(
      "All outcome values are NA. Cannot evaluate models."
    )

  }

  n_dropped <- 0L

  if (any(na_mask)) {

    n_dropped <- sum(na_mask)
    analysis  <- analysis[!na_mask, , drop = FALSE]

  }

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
  ## Step 3b: Validate workers + compute parallelism split
  ## -----------------------------------------------------------------------

  if (!is.numeric(workers) || length(workers) != 1 || workers < 1 ||
      workers != as.integer(workers)) {

    rlang::abort("`workers` must be a positive integer.")

  }

  workers <- as.integer(workers)
  inner   <- min(workers, cv_folds)
  outer   <- max(1L, as.integer(floor(workers / inner)))

  if (outer > 1L && is.null(output_dir)) {

    rlang::abort(paste0(
      "Parallel evaluation (workers > cv_folds) requires `output_dir` ",
      "for checkpoint safety. Provide an output directory or reduce workers."
    ))

  }

  ## -----------------------------------------------------------------------
  ## Step 4: Create train/test split
  ## -----------------------------------------------------------------------

  set.seed(seed)

  split <- tryCatch(
    rsample::initial_split(analysis, prop = 0.8, strata = outcome_col),
    error = function(e) {

      if (verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow("Stratified split failed, ",
                                       "retrying without strata"), "\n"
        ))

      }

      rsample::initial_split(analysis, prop = 0.8)

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
    rsample::vfold_cv(train_data, v = cv_folds, strata = outcome_col),
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
  ## Step 6: Load checkpoints (if any)
  ## -----------------------------------------------------------------------

  checkpoint_path    <- NULL
  checkpoint_dir     <- NULL
  checkpoint_results <- list()

  if (!is.null(output_dir)) {

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    checkpoint_path <- file.path(output_dir, "eval_checkpoint.rds")
    checkpoint_dir  <- file.path(output_dir, "checkpoints")

    if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir)

    ## Load from single-file checkpoint (legacy / sequential)
    if (file.exists(checkpoint_path)) {

      loaded <- readRDS(checkpoint_path)

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

      for (f in per_config_files) {

        row <- tryCatch(readRDS(f), error = function(e) NULL)

        if (!is.null(row) && row$config_id %in% configs$config_id &&
            !row$config_id %in% names(checkpoint_results)) {

          checkpoint_results[[ row$config_id ]] <- row

        }

      }

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

    cat(paste0("\u2502  Split: ", n_train, " train / ", n_test,
               " test (80/20, stratified)\n"))
    cat(paste0("\u2502  Tuning: ", cv_folds, "-fold CV, grid = ",
               tuning$grid_size, ", bayesian = ",
               tuning$bayesian_iter, "\n"))
    cat(paste0("\u2502  Configs: ", n_total, " total",
               if (n_pending < n_total) paste0(" (", n_total - n_pending,
                                                " from checkpoint)") else "",
               "\n"))

    if (outer > 1L) {

      cat(paste0("\u2502  Workers: ", workers,
                 " (", outer, " outer \u00D7 ", inner, " inner)\n"))

    }

    cat("\u2502\n")

  }

  ## -----------------------------------------------------------------------
  ## Step 8: Config loop (sequential or parallel)
  ## -----------------------------------------------------------------------

  if (outer <= 1L) {

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
        allow_par       = (inner > 1L),
        seed            = seed
      )

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

    ## Write manifest for monitor_evaluate()
    manifest <- list(
      n_total     = n_total,
      n_pending   = n_pending,
      config_ids  = configs$config_id,
      start_time  = start_time,
      workers     = workers,
      outer       = outer,
      inner       = inner,
      metric      = metric,
      cv_folds    = cv_folds
    )
    saveRDS(manifest, file.path(output_dir, "eval_manifest.rds"))

    ## Save and restore future plan on exit
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    ## Set up nested plan: outer (configs) × inner (CV folds)
    future::plan(list(
      future::tweak(future::multisession, workers = outer),
      future::tweak(future::multisession, workers = inner)
    ))

    ## Increase globals size limit for serialized data (restore on exit)
    old_max_size <- getOption("future.globals.maxSize")
    options(future.globals.maxSize = 4 * 1024^3)
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

    ## Assemble everything a worker needs into one value.
    ##
    ## Two separate serialization traps are avoided here, and both were measured
    ## on the KSSL clay training set at 2 cm-1 (17,788 x 1,701).
    ##
    ## 1. The split and the CV folds share one data frame by reference, so
    ##    sending them directly costs one + v serialized copies of the table:
    ##    1,158.7 MB against 231.7 MB of unique data. Send the table once with
    ##    integer indices and rebuild worker-side (R/utils-resamples.R).
    ##
    ## 2. A function defined here would close over THIS frame, and R serializes
    ##    a closure together with its enclosing environment. That shipped every
    ##    local in evaluate() — including `split` and `cv_fold_obj` again —
    ##    for a real payload of 2.26 GiB no matter what was passed explicitly.
    ##    So the worker body lives at top level (evaluate_config_worker), is
    ##    passed to future_map() by name, and receives its inputs as an
    ##    argument. Its environment is the package namespace, which future
    ##    resolves by name instead of serializing.
    ##
    ## Do not inline this back into an anonymous function.

    shared_args <- list(
      data            = split$data,
      idx             = resample_indices(split, cv_fold_obj),
      configs         = pending_configs,
      role_map        = role_map,
      grid_size       = tuning$grid_size,
      bayesian_iter   = tuning$bayesian_iter,
      prune           = prune,
      prune_threshold = prune_threshold,
      allow_par       = (inner > 1L),
      seed            = seed,
      checkpoint_dir  = checkpoint_dir
    )

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

  successes <- all_results[all_results$status == "success", ]

  if (nrow(successes) == 0) {

    ## Check if there are pruned configs with metrics for the ranking metric
    pruned <- all_results[all_results$status == "pruned" &
                            !is.na(all_results[[rank_column]]), ]

    if (nrow(pruned) == 0) {

      n_failed <- sum(all_results$status == "failed")
      n_pruned <- sum(all_results$status == "pruned")

      rlang::abort(paste0(
        "All configurations failed or were pruned. ",
        "Failed: ", n_failed, ", Pruned: ", n_pruned, ". ",
        "Check evaluation$results for error messages."
      ))

    }

    ## Use pruned configs as fallback
    successes <- pruned

  }

  ranked <- rank_configs_by_cv(successes, metric)

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
    workers      = workers,
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

  results[order(vals, decreasing = metric %in% HIGHER_BETTER_METRICS), , drop = FALSE]

}
