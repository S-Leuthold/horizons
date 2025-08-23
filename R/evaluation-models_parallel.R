#' Evaluate Multiple Model Configurations in Parallel
#'
#' @description
#' Orchestrates parallel evaluation of multiple spectral model configurations using
#' `evaluate_single_model_parallel()`. Uses work-stealing parallelization for optimal
#' load balancing - each model is processed as an independent task, preventing idle
#' workers when some models take longer than others.
#'
#' @param configs A tibble of model configurations, typically from `create_project_configurations()`
#' @param input_data A tibble containing spectral features and response variable
#' @param covariate_data Optional tibble containing external covariates
#' @param variable Character. Name of the response variable to model
#' @param n_workers Integer. Number of parallel workers to use (default: 4)
#' @param output_dir Character. Directory to save results (default: tempdir())
#' @param chunk_size Integer. DEPRECATED - kept for backwards compatibility
#' @param grid_size Integer. Grid search size for each model (default: 10)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 15)
#' @param cv_folds Integer. Cross-validation folds (default: 5)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#' @param save_individual_models Logical. Save each successful model immediately (default: FALSE)
#' @param verbose Logical. Print progress information (default: TRUE)
#' @param parallel Logical. Enable parallel processing for model evaluation. Defaults to `TRUE` (top-level orchestrator).
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A tibble containing all model results with columns:
#' \itemize{
#'   \item Configuration columns from input configs
#'   \item \strong{rmse, rrmse, rsq, mae, rpd}: Performance metrics
#'   \item \strong{runtime_seconds}: Execution time per model
#'   \item \strong{status}: "success" or "failed"
#'   \item \strong{best_params}: Nested list of optimal hyperparameters
#'   \item \strong{error_info}: Error details for failed models
#'   \item \strong{tuning_info}: Timing and convergence details
#' }
#'
#' @details
#' This function provides a clean parallel interface that:
#' - Uses work-stealing for optimal load balancing
#' - Prevents nested parallelization issues
#' - Aggregates results into a single tibble
#' - Saves detailed error logs for debugging
#' - Manages memory efficiently across workers
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

  start_time <- Sys.time()

  ## Capture baseline memory for monitoring -----------------------------------

  baseline_memory_mb <- tryCatch({
    as.numeric(pryr::mem_used()) / 1048576
  }, error = function(e) NULL)

  ## ---------------------------------------------------------------------------
  ## Step 1: Validation and Setup
  ## ---------------------------------------------------------------------------

  ## Basic validation ----------------------------------------------------------

  if (!is.data.frame(configs) || nrow(configs) == 0) {
    cli::cli_abort("configs must be a non-empty data frame")
  }

  if (!variable %in% colnames(input_data)) {
    cli::cli_abort("Variable '{variable}' not found in input_data")
  }

  ## Setup output directory ----------------------------------------------------

  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), "parallel_models")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ## Adjust worker count to available cores -----------------------------------

  max_cores <- parallel::detectCores() - 1  # Leave one core free
  n_workers <- min(n_workers, max_cores, nrow(configs))

  if (verbose) {
    cli::cli_h1("Parallel Model Evaluation")
    cli::cli_alert_info("Models to evaluate: {nrow(configs)}")
    cli::cli_alert_info("Parallel workers: {n_workers}")
    cli::cli_alert_info("Variable: {variable}")
    cli::cli_alert_info("Output directory: {output_dir}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Setup Work-Stealing Parallelization
  ## ---------------------------------------------------------------------------

  ## Add row indices to configs for tracking -----------------------------------

  configs %>%
    dplyr::mutate(config_index = dplyr::row_number(),
                  .before      = 1) -> configs_indexed

  n_configs <- nrow(configs_indexed)

  if (verbose) {
    cli::cli_h2("Work-stealing parallel processing")
    cli::cli_alert_info("Total models: {n_configs}")
    cli::cli_alert_info("Workers: {n_workers}")
    cli::cli_alert_info("Using dynamic scheduling for optimal load balancing")
  }

  ## Setup parallel backend with safety controls -------------------------------

  # Check for nested parallelization using number of workers (simple and reliable)
  current_workers <- future::nbrOfWorkers()
  if (!allow_nested && current_workers > 1) {
    if(verbose) cli::cli_alert_warning("Nested parallelization detected ({current_workers} workers active). Setting parallel=FALSE for safety")
    parallel <- FALSE
  }

  # Set parallel plan with proper cleanup
  if (parallel && n_workers > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    # Use multisession for better stability across all platforms
    # Multicore can cause issues on some Linux systems with OpenBLAS
    future::plan(future::multicore, workers = n_workers)
    if (verbose) cli::cli_alert_info("Using multicore with {n_workers} workers")
  } else {
    future::plan(sequential)
    if(verbose) cli::cli_alert_info("Using sequential processing (parallel={parallel}, n_workers={n_workers})")
  }

  # Increase memory limits for large spectral data
  options(future.globals.maxSize = 10000 * 1024^2)  # 2GB limit
  if (verbose) cli::cli_alert_info("Set future.globals.maxSize to 2GB for large spectral matrices")

  ## ---------------------------------------------------------------------------
  ## Step 3: Process All Models with Work-Stealing
  ## ---------------------------------------------------------------------------

  # Initialize log file for progress tracking
  log_file <- file.path(output_dir, "parallel_progress.log")
  best_rsq_so_far <- 0  # Track best model

  if (verbose) {
    log_file <- init_parallel_log(log_file, n_models = n_configs)
    cli::cli_alert_info("Starting parallel processing of {n_configs} models...")
    cli::cli_alert_info("Progress log: {log_file}")
    cli::cli_alert_info("Monitor progress: tail -f {log_file}")
    cli::cli_alert_info("Or in R: monitor_parallel_progress('{log_file}')")
    # Removed CLI progress bar to avoid conflicts with parallel workers
  }

  # Process all models with work-stealing parallelization
  all_results <- furrr::future_map(
    .x = seq_len(nrow(configs_indexed)),
    .f = function(row_idx) {

      config <- configs_indexed[row_idx, ]

      # Log progress to file with cleaner format
      config_desc <- sprintf("%s | %s | %s | %s",
                            config$model,
                            config$preprocessing,
                            config$feature_selection,
                            if(length(config$covariates[[1]]) > 0 && !is.null(config$covariates[[1]])) {
                              paste(config$covariates[[1]], collapse="+")
                            } else {
                              "no_covs"
                            })

      log_parallel_progress(
        sprintf("START [%3d/%d]: %s", row_idx, nrow(configs_indexed), config_desc),
        log_file = log_file,
        worker_id = Sys.getpid() %% 100  # Keep worker ID for tracking
      )

      # Progress tracking handled by file logging only
      # Removed CLI progress updates to avoid parallel conflicts

      result <- tryCatch({

        # Generate unique seed for this model based on config index
        model_seed <- seed + config$config_index

        # Call the single model evaluation function
        result <- evaluate_single_model_parallel(
          input_data        = input_data,
          covariate_data    = covariate_data,
          variable          = variable,
          model             = config$model,
          transformation    = config$transformation,
          preprocessing     = config$preprocessing,
          feature_selection = config$feature_selection,
          covariates        = config$covariates[[1]],  # Extract from list column
          include_covariates = ifelse(is.null(config$covariates[[1]]), FALSE, length(config$covariates[[1]]) > 0),
          grid_size         = grid_size,
          bayesian_iter     = bayesian_iter,
          cv_folds          = cv_folds,
          seed              = model_seed,
          parallel          = FALSE,     # Already running in parallel context
          n_workers         = NULL,      # Not used when parallel=FALSE
          allow_nested      = FALSE      # Prevent any nested parallelization
        )

        # Add configuration information back to result
        result$config_index      <- config$config_index
        result$model             <- config$model
        result$transformation    <- config$transformation
        result$preprocessing     <- config$preprocessing
        result$feature_selection <- config$feature_selection
        result$covariates        <- config$covariates[[1]]

        # Log successful completion with cleaner format
        # Extract metrics from tibble with proper validation
        metrics_str <- if(!is.null(result$metrics) && is.data.frame(result$metrics) && nrow(result$metrics) > 0) {

          # Safely extract values from tibble columns
          rsq_val <- tryCatch({
            if("rsq" %in% names(result$metrics)) {
              result$metrics$rsq[1]
            } else {
              NA
            }
          }, error = function(e) NA)

          rmse_val <- tryCatch({
            if("rmse" %in% names(result$metrics)) {
              result$metrics$rmse[1]
            } else {
              NA
            }
          }, error = function(e) NA)

          # Create formatted string based on available metrics
          if(!is.na(rsq_val) && !is.na(rmse_val)) {
            sprintf("R²=%.3f, RMSE=%.2f", rsq_val, rmse_val)
          } else if(!is.na(rsq_val)) {
            sprintf("R²=%.3f", rsq_val)
          } else if(!is.na(rmse_val)) {
            sprintf("RMSE=%.2f", rmse_val)
          } else {
            "⚠️ metrics values NA"
          }
        } else {
          if(is.null(result$metrics)) {
            "⚠️ metrics NULL"
          } else if(!is.data.frame(result$metrics)) {
            "⚠️ metrics not tibble"
          } else {
            "⚠️ metrics empty tibble"
          }
        }

        log_parallel_progress(
          sprintf("✓ DONE [%3d/%d]: %s (%s) - %.1fs",
                  row_idx,
                  nrow(configs_indexed),
                  result$model_id,
                  metrics_str,
                  result$runtime_seconds %||% 0),
          log_file = log_file,
          worker_id = Sys.getpid() %% 100  # Keep worker ID for tracking
        )

        return(result)

      }, error = function(e) {

        # Log failure with cleaner format
        error_msg <- gsub("\n", " ", conditionMessage(e))  # Remove newlines
        error_msg <- if(nchar(error_msg) > 50) {
          paste0(substr(error_msg, 1, 50), "...")
        } else {
          error_msg
        }

        log_parallel_progress(
          sprintf("✗ FAIL [%3d/%d]: %s | ERROR: %s",
                  row_idx,
                  nrow(configs_indexed),
                  config_desc,  # Use same description as START
                  error_msg),
          log_file = log_file,
          worker_id = Sys.getpid() %% 100  # Keep worker ID for tracking
        )

        # Capture any errors that escape the single model function
        return(list(
          config_index    = config$config_index,
          model_id        = paste(config$model, config$transformation, config$preprocessing,
                                  config$feature_selection, sep = "_"),
          metrics         = NULL,
          best_params     = NULL,
          runtime_seconds = 0,
          status          = "failed",
          error_info      = list(
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
        ))
      })

      return(result)
    },
    .options = furrr::furrr_options(
      seed       = NULL,     # Let individual models control their own seeds
      stdout     = FALSE,     # Show console output (may be garbled with multiple workers)
      conditions = "message", # Capture messages but not warnings
      chunk_size = 10,
      scheduling = 1      # Maximum dynamic scheduling (work-stealing)
    ),
    .progress = FALSE  # We're handling progress ourselves
  )

  # Progress tracking completed via file logging

  ## ---------------------------------------------------------------------------
  ## Step 4: Process Results
  ## ---------------------------------------------------------------------------

  # Count successes and failures
  success_count <- sum(vapply(all_results, function(x)
    !is.null(x$status) && x$status == "success", logical(1)))
  failed_count <- sum(vapply(all_results, function(x)
    !is.null(x$status) && x$status == "failed", logical(1)))

  # Diagnostic: Count models with valid metrics
  if (verbose) {
    successful_results <- all_results[vapply(all_results, function(x) x$status == "success", logical(1))]

    metrics_valid_count <- sum(vapply(successful_results, function(x) {
      !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) > 0 &&
      "rsq" %in% names(x$metrics) && "rmse" %in% names(x$metrics)
    }, logical(1)))

    metrics_issues <- success_count - metrics_valid_count

    if (metrics_issues > 0) {
      cli::cli_alert_warning("⚠️ {metrics_issues} successful models have metrics extraction issues")

      # Detailed diagnostics for problematic metrics
      problematic <- successful_results[sapply(successful_results, function(x) {
        is.null(x$metrics) || !is.data.frame(x$metrics) || nrow(x$metrics) == 0 ||
        !"rsq" %in% names(x$metrics) || !"rmse" %in% names(x$metrics)
      })]

      if (length(problematic) > 0) {
        cli::cli_alert_info("Metrics diagnostic summary:")

        null_metrics <- sum(sapply(problematic, function(x) is.null(x$metrics)))
        not_df <- sum(sapply(problematic, function(x) !is.null(x$metrics) && !is.data.frame(x$metrics)))
        empty_df <- sum(sapply(problematic, function(x)
          !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) == 0))
        missing_cols <- sum(sapply(problematic, function(x)
          !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) > 0 &&
          (!"rsq" %in% names(x$metrics) || !"rmse" %in% names(x$metrics))))

        if (null_metrics > 0) cli::cli_alert_info("  • {null_metrics} models: metrics is NULL")
        if (not_df > 0) cli::cli_alert_info("  • {not_df} models: metrics is not a data frame")
        if (empty_df > 0) cli::cli_alert_info("  • {empty_df} models: metrics tibble is empty")
        if (missing_cols > 0) cli::cli_alert_info("  • {missing_cols} models: missing rsq/rmse columns")
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

    # Summary of metrics extraction success
    if (success_count > 0) {
      successful_results <- all_results[vapply(all_results, function(x) x$status == "success", logical(1))]
      metrics_valid_count <- sum(vapply(successful_results, function(x) {
        !is.null(x$metrics) && is.data.frame(x$metrics) && nrow(x$metrics) > 0 &&
        "rsq" %in% names(x$metrics) && "rmse" %in% names(x$metrics)
      }, logical(1)))

      cli::cli_alert_info("Valid metrics extracted: {metrics_valid_count}/{success_count} successful models")
    }
  }

  ## Save individual models if requested --------------------------------------

  if (save_individual_models) {
    for (result in all_results) {
      if (result$status == "success") {
        individual_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        individual_file <- file.path(output_dir,
                                   glue::glue("individual_model_{result$config_index}_{individual_timestamp}.qs"))
        qs::qsave(result, individual_file)
      }
    }

    if (verbose) {
      cli::cli_alert_info("Saved {success_count} individual successful models")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Format and Return Results
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_h2("Finalizing results")
  }

  # Convert list of results to tibble
  results_tibble <- tryCatch({

    # Extract successful results
    successful_results <- all_results[vapply(all_results, function(x) x$status == "success", logical(1))]

    if (length(successful_results) > 0) {
      # Convert to tibble with safe metrics extraction
      results_df <- dplyr::bind_rows(lapply(successful_results, function(res) {

        # Safely extract metrics with validation
        safe_extract_metric <- function(metrics, column_name) {
          if(is.null(metrics) || !is.data.frame(metrics) || nrow(metrics) == 0) {
            return(NA_real_)
          }

          if(!column_name %in% names(metrics)) {
            return(NA_real_)
          }

          value <- tryCatch({
            metrics[[column_name]][1]
          }, error = function(e) NA_real_)

          if(is.null(value) || length(value) == 0) {
            return(NA_real_)
          }

          return(value)
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

    # Add failed results
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

      # Combine successful and failed results
      if (nrow(results_df) > 0) {
        results_df <- dplyr::bind_rows(results_df, failed_df)
      } else {
        results_df <- failed_df
      }
    }

    results_df

  }, error = function(e) {
    cli::cli_alert_warning("Could not convert results to tibble format")
    return(all_results)  # Return raw list if conversion fails
  })

  # Save final results
  final_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  final_file <- file.path(output_dir, glue::glue("all_results_{final_timestamp}.qs"))
  qs::qsave(results_tibble, final_file)

  if (verbose) {
    cli::cli_alert_success("Results saved to: {final_file}")

    # Memory cleanup
    gc(verbose = FALSE, full = TRUE)

    # Final memory report
    if (!is.null(baseline_memory_mb)) {
      current_memory_mb <- as.numeric(pryr::mem_used()) / 1048576
      memory_increase <- current_memory_mb - baseline_memory_mb
      cli::cli_alert_info("Memory usage: {round(current_memory_mb, 1)} MB (increased by {round(memory_increase, 1)} MB)")
    }
  }

  return(results_tibble)
}
