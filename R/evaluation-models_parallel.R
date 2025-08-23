#' Evaluate Multiple Model Configurations in Parallel
#'
#' @description
#' Orchestrates parallel evaluation of multiple spectral model configurations using
#' `evaluate_single_model_parallel()`. Each configuration is evaluated independently,
#' and results are logged to per-worker files in the run directory (`TMPDIR`).
#'
#' Parallel execution uses the \pkg{future} / \pkg{furrr} framework with work-stealing
#' for load balancing. Worker count is capped by system cores, config size, and the
#' environment variable `HORIZONS_WORKERS`. Node-local scratch space (`TMPDIR`) is
#' recommended for high performance.
#'
#' @param configs A tibble of model configurations
#' @param input_data Tibble of spectral features and response variable
#' @param covariate_data Optional tibble of external covariates
#' @param variable Character. Response variable name
#' @param n_workers Integer. Requested parallel workers (default: 4)
#' @param output_dir Directory for results/logs (default: `Sys.getenv("TMPDIR")`)
#' @param grid_size Integer. Grid search candidates per model (default: 10)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 15)
#' @param cv_folds Integer. Cross-validation folds (default: 5)
#' @param seed Integer. Base random seed (default: 123)
#' @param save_individual_models Logical. Save each successful model separately
#' @param verbose Logical. Print progress messages
#' @param parallel Logical. Enable top-level parallelization (default: TRUE)
#' @param allow_nested Logical. Allow nested parallelism (default: FALSE)
#'
#' @return A tibble with configuration, performance metrics, runtime, and error info
#'
#' @details
#' - Logs are written to per-worker files (`parallel_<PID>.log`) in `output_dir`
#' - Prevents nested parallelism by forcing workers to run sequentially
#' - Tracks memory usage, timing, and metrics validity
#'
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan nbrOfWorkers multicore sequential
#' @importFrom parallel detectCores
#' @importFrom dplyr mutate row_number bind_rows
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @importFrom qs qsave
#' @importFrom pryr mem_used
#' @importFrom RhpcBLASctl blas_set_num_threads omp_set_num_threads
#'
#' @export

evaluate_models_parallel <- function(configs,
                                     input_data,
                                     covariate_data         = NULL,
                                     variable,
                                     n_workers              = 4,
                                     output_dir             = NULL,
                                     chunk_size             = 50,  # DEPRECATED
                                     grid_size              = 10,
                                     bayesian_iter          = 15,
                                     cv_folds               = 5,
                                     seed                   = 123,
                                     save_individual_models = FALSE,
                                     verbose                = TRUE,
                                     parallel               = TRUE,
                                     allow_nested           = FALSE) {

  # ---- small helper ----------------------------------------------------------
  `%||%` <- function(x, y) if (is.null(x)) y else x

  start_time <- Sys.time()

  # Baseline memory
  baseline_memory_mb <- tryCatch(as.numeric(pryr::mem_used()) / 1048576, error = function(e) NULL)

  # ---------------------------------------------------------------------------
  # Step 1: Validation and Setup
  # ---------------------------------------------------------------------------

  if (!is.data.frame(configs) || nrow(configs) == 0) {
    cli::cli_abort("configs must be a non-empty data frame")
  }
  if (!variable %in% colnames(input_data)) {
    cli::cli_abort("Variable '{variable}' not found in input_data")
  }

  # CRITICAL: Use node-local scratch (set by launcher) for outputs/logs
  # Falls back to tempdir() if not provided by the environment
  Sys.setenv(TMPDIR = Sys.getenv("TMPDIR", unset = tempdir()))
  if (is.null(output_dir)) {
    output_dir <- file.path(Sys.getenv("TMPDIR"), "parallel_models")
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # CRITICAL: Cap workers sensibly; allow env override (HORIZONS_WORKERS)
  env_workers <- suppressWarnings(as.integer(Sys.getenv("HORIZONS_WORKERS", unset = NA)))
  host_cores  <- parallel::detectCores(logical = TRUE)
  if (!is.na(env_workers) && env_workers > 0) n_workers <- env_workers
  n_workers <- min(n_workers, max(1L, host_cores - 1L), nrow(configs))

  if (verbose) {
    cli::cli_h1("Parallel Model Evaluation")
    cli::cli_alert_info("Models to evaluate: {nrow(configs)}")
    cli::cli_alert_info("Parallel workers: {n_workers} (host cores: {host_cores})")
    cli::cli_alert_info("Variable: {variable}")
    cli::cli_alert_info("Output directory: {output_dir}")
    cli::cli_alert_info("TMPDIR: {Sys.getenv('TMPDIR')}")
  }

  # ---------------------------------------------------------------------------
  # Step 2: Setup Work-Stealing Parallelization
  # ---------------------------------------------------------------------------

  configs %>%
    dplyr::mutate(config_index = dplyr::row_number(), .before = 1) -> configs_indexed

  n_configs <- nrow(configs_indexed)

  if (verbose) {
    cli::cli_h2("Work-stealing parallel processing")
    cli::cli_alert_info("Total models: {n_configs}")
    cli::cli_alert_info("Workers: {n_workers}")
    cli::cli_alert_info("Per-worker logs will be written to: {file.path(output_dir, 'parallel_<PID>.log')}")
    cli::cli_alert_info("Monitor in R: horizons::monitor_parallel_progress('{output_dir}', refresh_seconds=2, show_recent=10)")
  }

  # Detect nested parallelism
  current_workers <- future::nbrOfWorkers()
  if (!allow_nested && current_workers > 1) {
    if (verbose) cli::cli_alert_warning("Nested parallelization detected ({current_workers}); forcing sequential inside.")
    parallel <- FALSE
  }

  # CRITICAL: Choose plan; we keep multicore (your preference) but quieter 🔇
  if (parallel && n_workers > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multicore, workers = n_workers)
    if (verbose) cli::cli_alert_info("Using multicore with {n_workers} workers")
  } else {
    future::plan(sequential)
    if (verbose) cli::cli_alert_info("Using sequential processing (parallel={parallel}, n_workers={n_workers})")
  }

  # Best practice: keep a generous globals cap (comment & value aligned)
  options(future.globals.maxSize = 10000 * 1024^2)  # 10 GB

  # ---------------------------------------------------------------------------
  # Step 3: Process All Models with Work-Stealing
  # ---------------------------------------------------------------------------

  # CRITICAL: Per-worker, node-local logger (no shared-file appends)
  .worker_id  <- function() sprintf("W%02d", as.integer(Sys.getpid()) %% 100L)
  .worker_log <- function() file.path(output_dir, paste0("parallel_", Sys.getpid(), ".log"))
  log_parallel_progress <- function(message, include_timestamp = TRUE) {
    ts <- if (include_timestamp) format(Sys.time(), "%F %T ") else ""
    line <- sprintf("%s[%s] %s", ts, .worker_id(), message)
    cat(line, "\n", file = .worker_log(), append = TRUE)
  }

  # Best practice: quieter furrr; batch tasks to reduce coordinator chatter
  furrr_opts <- furrr::furrr_options(
    seed       = NULL,
    stdout     = FALSE,
    conditions = NULL,
    chunk_size = 1,
    scheduling = Inf     # finite (not Inf) limits scheduler chatter
  )

  if (verbose) {
    cli::cli_alert_info("Starting parallel processing of {n_configs} models...")
  }

  all_results <- furrr::future_map(
    .x = seq_len(nrow(configs_indexed)),
    .f = function(row_idx) {

      # Best practice: enforce 1 thread inside worker (belt & suspenders)
      try({
        Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1")
        if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
          RhpcBLASctl::blas_set_num_threads(1L)
          RhpcBLASctl::omp_set_num_threads(1L)
        }
      }, silent = TRUE)

      config <- configs_indexed[row_idx, ]
      config_desc <- sprintf("%s | %s | %s | %s",
                             config$model,
                             config$preprocessing,
                             config$feature_selection,
                             if (length(config$covariates[[1]]) > 0 && !is.null(config$covariates[[1]])) {
                               paste(config$covariates[[1]], collapse = "+")
                             } else {"no_covs"})

      # CRITICAL: no shared log file
      log_parallel_progress(sprintf("START [%3d/%d]: %s", row_idx, nrow(configs_indexed), config_desc))

      result <- tryCatch({

        model_seed <- seed + config$config_index

        result <- evaluate_single_model_parallel(
          input_data         = input_data,
          covariate_data     = covariate_data,
          variable           = variable,
          model              = config$model,
          transformation     = config$transformation,
          preprocessing      = config$preprocessing,
          feature_selection  = config$feature_selection,
          covariates         = config$covariates[[1]],
          include_covariates = ifelse(is.null(config$covariates[[1]]), FALSE, length(config$covariates[[1]]) > 0),
          grid_size          = grid_size,
          bayesian_iter      = bayesian_iter,
          cv_folds           = cv_folds,
          seed               = model_seed,
          parallel           = FALSE,  # CRITICAL: no nested parallelism
          n_workers          = NULL,
          allow_nested       = FALSE
        )

        # Reattach config info (unchanged)
        result$config_index      <- config$config_index
        result$model             <- config$model
        result$transformation    <- config$transformation
        result$preprocessing     <- config$preprocessing
        result$feature_selection <- config$feature_selection
        result$covariates        <- config$covariates[[1]]

        # Summarize metrics defensively
        metrics_str <- if (!is.null(result$metrics) && is.data.frame(result$metrics) && nrow(result$metrics) > 0) {
          rsq_val  <- tryCatch(if ("rsq"  %in% names(result$metrics))  result$metrics$rsq[1]  else NA_real_, error = function(e) NA_real_)
          rmse_val <- tryCatch(if ("rmse" %in% names(result$metrics)) result$metrics$rmse[1] else NA_real_, error = function(e) NA_real_)
          if (!is.na(rsq_val) && !is.na(rmse_val)) sprintf("R²=%.3f, RMSE=%.2f", rsq_val, rmse_val)
          else if (!is.na(rsq_val)) sprintf("R²=%.3f", rsq_val)
          else if (!is.na(rmse_val)) sprintf("RMSE=%.2f", rmse_val)
          else "⚠️ metrics values NA"
        } else {
          "⚠️ metrics unavailable"
        }

        log_parallel_progress(sprintf("✓ DONE [%3d/%d]: %s (%s) - %.1fs",
                                      row_idx,
                                      nrow(configs_indexed),
                                      result$model_id %||% "<unknown>",
                                      metrics_str,
                                      result$runtime_seconds %||% 0))

        result

      }, error = function(e) {
        error_msg <- gsub("\n", " ", conditionMessage(e))
        if (nchar(error_msg) > 80) error_msg <- paste0(substr(error_msg, 1, 80), "…")

        log_parallel_progress(sprintf("✗ FAIL [%3d/%d]: %s | ERROR: %s",
                                      row_idx,
                                      nrow(configs_indexed),
                                      config_desc,
                                      error_msg))

        list(
          config_index      = config$config_index,
          model_id          = paste(config$model, config$transformation, config$preprocessing,
                                    config$feature_selection, sep = "_"),
          metrics           = NULL,
          best_params       = NULL,
          runtime_seconds   = 0,
          status            = "failed",
          error_info        = list(
            stage       = "parallel_wrapper",
            message     = paste("Parallel execution failed:", conditionMessage(e)),
            error_class = class(e),
            error_type  = "system"
          ),
          tuning_info       = NULL,
          model             = config$model,
          transformation    = config$transformation,
          preprocessing     = config$preprocessing,
          feature_selection = config$feature_selection,
          covariates        = config$covariates[[1]]
        )
      })

      return(result)
    },
    .options  = furrr_opts,
    .progress = FALSE
  )

  # ---------------------------------------------------------------------------
  # Step 4: Process Results  (unchanged except for messages)
  # ---------------------------------------------------------------------------

  success_count <- sum(vapply(all_results, function(x) !is.null(x$status) && x$status == "success", logical(1)))
  failed_count  <- sum(vapply(all_results, function(x) !is.null(x$status) && x$status == "failed",  logical(1)))

  if (verbose) {
    successful_results <- all_results[vapply(all_results, function(x) x$status == "success", logical(1))]
    metrics_valid_count <- sum(vapply(successful_results, function(x) {
      !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) > 0 &&
        "rsq" %in% names(x$metrics) && "rmse" %in% names(x$metrics)
    }, logical(1)))

    metrics_issues <- success_count - metrics_valid_count
    if (metrics_issues > 0) {
      cli::cli_alert_warning("⚠️ {metrics_issues} successful models have metrics extraction issues")
      problematic <- successful_results[sapply(successful_results, function(x) {
        is.null(x$metrics) || !is.data.frame(x$metrics) || nrow(x$metrics) == 0 ||
          !"rsq" %in% names(x$metrics) || !"rmse" %in% names(x$metrics)
      })]
      if (length(problematic) > 0) {
        cli::cli_alert_info("Metrics diagnostic summary:")
        null_metrics <- sum(sapply(problematic, function(x) is.null(x$metrics)))
        not_df       <- sum(sapply(problematic, function(x) !is.null(x$metrics) && !is.data.frame(x$metrics)))
        empty_df     <- sum(sapply(problematic, function(x) !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) == 0))
        missing_cols <- sum(sapply(problematic, function(x) !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) > 0 &&
                                     (!"rsq" %in% names(x$metrics) || !"rmse" %in% names(x$metrics))))
        if (null_metrics  > 0) cli::cli_alert_info("  • {null_metrics} models: metrics is NULL")
        if (not_df        > 0) cli::cli_alert_info("  • {not_df} models: metrics is not a data frame")
        if (empty_df      > 0) cli::cli_alert_info("  • {empty_df} models: metrics tibble is empty")
        if (missing_cols  > 0) cli::cli_alert_info("  • {missing_cols} models: missing rsq/rmse columns")
      }
    } else {
      cli::cli_alert_success("✅ All {success_count} successful models have valid metrics")
    }
  }

  total_elapsed <- difftime(Sys.time(), start_time, units = "mins")

  if (verbose) {
    cli::cli_h2("Processing complete")
    cli::cli_alert_success("Processed {n_configs} models in {round(total_elapsed, 1)} minutes")
    cli::cli_alert_info("Success: {success_count}, Failed: {failed_count}")
    cli::cli_alert_info("Average time per model: {round(as.numeric(total_elapsed) * 60 / n_configs, 1)} seconds")

    if (success_count > 0) {
      successful_results <- all_results[vapply(all_results, function(x) x$status == "success", logical(1))]
      metrics_valid_count <- sum(vapply(successful_results, function(x) {
        !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) > 0 &&
          "rsq" %in% names(x$metrics) && "rmse" %in% names(x$metrics)
      }, logical(1)))
      cli::cli_alert_info("Valid metrics extracted: {metrics_valid_count}/{success_count} successful models")
    }
  }

  # Save individual models (unchanged)
  if (save_individual_models) {
    for (result in all_results) {
      if (result$status == "success") {
        individual_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        individual_file <- file.path(output_dir, glue::glue("individual_model_{result$config_index}_{individual_timestamp}.qs"))
        qs::qsave(result, individual_file)
      }
    }
    if (verbose) cli::cli_alert_info("Saved {success_count} individual successful models")
  }

  # ---------------------------------------------------------------------------
  # Step 5: Format and Return Results  (unchanged)
  # ---------------------------------------------------------------------------

  if (verbose) cli::cli_h2("Finalizing results")

  results_tibble <- tryCatch({
    successful_results <- all_results[vapply(all_results, function(x) x$status == "success", logical(1))]
    if (length(successful_results) > 0) {
      results_df <- dplyr::bind_rows(lapply(successful_results, function(res) {
        safe_extract_metric <- function(metrics, column_name) {
          if (is.null(metrics) || !is.data.frame(metrics) || nrow(metrics) == 0) return(NA_real_)
          if (!column_name %in% names(metrics)) return(NA_real_)
          val <- tryCatch(metrics[[column_name]][1], error = function(e) NA_real_)
          if (is.null(val) || length(val) == 0) return(NA_real_)
          val
        }
        tibble::tibble(
          config_index      = res$config_index,
          model             = res$model,
          transformation    = res$transformation,
          preprocessing     = res$preprocessing,
          feature_selection = res$feature_selection,
          covariates        = list(res$covariates),
          model_id          = res$model_id,
          rmse              = safe_extract_metric(res$metrics, "rmse"),
          rrmse             = safe_extract_metric(res$metrics, "rrmse"),
          rsq               = safe_extract_metric(res$metrics, "rsq"),
          mae               = safe_extract_metric(res$metrics, "mae"),
          rpd               = safe_extract_metric(res$metrics, "rpd"),
          runtime_seconds   = res$runtime_seconds,
          status            = res$status,
          best_params       = list(res$best_params),
          tuning_info       = list(res$tuning_info)
        )
      }))
    } else {
      results_df <- tibble::tibble()
    }

    failed_results <- all_results[vapply(all_results, function(x) x$status == "failed", logical(1))]
    if (length(failed_results) > 0) {
      failed_df <- dplyr::bind_rows(lapply(failed_results, function(res) {
        tibble::tibble(
          config_index      = res$config_index,
          model             = res$model,
          transformation    = res$transformation,
          preprocessing     = res$preprocessing,
          feature_selection = res$feature_selection,
          covariates        = list(res$covariates),
          model_id          = res$model_id,
          rmse              = NA_real_,
          rrmse             = NA_real_,
          rsq               = NA_real_,
          mae               = NA_real_,
          rpd               = NA_real_,
          runtime_seconds   = res$runtime_seconds,
          status            = res$status,
          best_params       = list(NULL),
          tuning_info       = list(NULL),
          error_info        = list(res$error_info)
        )
      }))
      if (nrow(results_df) > 0) results_df <- dplyr::bind_rows(results_df, failed_df) else results_df <- failed_df
    }
    results_df
  }, error = function(e) {
    cli::cli_alert_warning("Could not convert results to tibble format")
    all_results
  })

  # Save final results
  final_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  final_file <- file.path(output_dir, glue::glue("all_results_{final_timestamp}.qs"))
  qs::qsave(results_tibble, final_file)
  if (verbose) {
    cli::cli_alert_success("Results saved to: {final_file}")
    gc(verbose = FALSE, full = TRUE)
    if (!is.null(baseline_memory_mb)) {
      current_memory_mb <- as.numeric(pryr::mem_used()) / 1048576
      memory_increase   <- current_memory_mb - baseline_memory_mb
      cli::cli_alert_info("Memory usage: {round(current_memory_mb, 1)} MB (increased by {round(memory_increase, 1)} MB)")
    }
  }

  return(results_tibble)
}

