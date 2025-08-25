#' Nested Parallel HPC Evaluation Orchestrator
#'
#' @description
#' Implements two-tier parallelization for optimal HPC resource utilization.
#' Outer loop runs N model configurations concurrently, each with M inner workers
#' for tuning, where N × M = total_cores. This dramatically improves throughput
#' compared to sequential model processing.
#'
#' @param config Model configuration tibble with columns:
#'   - model: Model type (e.g., "random_forest", "cubist", "xgboost")
#'   - transformation: Response transformation
#'   - preprocessing: Spectral preprocessing method
#'   - feature_selection: Feature selection method
#'   - covariates: List-column of covariate sets
#'   - include_covariates: Logical flag for covariate inclusion
#' @param input_data Spectral data tibble with Sample_ID and wavenumber columns
#' @param covariate_data Optional covariate tibble matched by Sample_ID
#' @param variable Character. Name of the response variable
#' @param total_cores Integer. Total cores to use (default: 50)
#' @param outer_workers Integer. Number of concurrent models (auto-calculated if NULL)
#' @param inner_workers Integer. Workers per model (auto-calculated if NULL)
#' @param memory_per_model_gb Numeric. Expected memory per model in GB (default: 15)
#' @param output_dir Character. Output directory path
#' @param grid_size_eval Integer. Grid search size (default: 10)
#' @param bayesian_iter_eval Integer. Bayesian optimization iterations (default: 15)
#' @param cv_folds_eval Integer. Cross-validation folds (default: 5)
#' @param checkpoint_dir Character. Checkpoint directory path
#' @param resume Logical. Resume from checkpoints (default: TRUE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return A tibble with evaluation results for all models
#'
#' @export
run_nested_hpc_evaluation <- function(config,
                                      input_data,
                                      covariate_data = NULL,
                                      variable,
                                      total_cores = 50,
                                      outer_workers = NULL,
                                      inner_workers = NULL,
                                      memory_per_model_gb = 15,
                                      output_dir = NULL,
                                      grid_size_eval = 10,
                                      bayesian_iter_eval = 15,
                                      cv_folds_eval = 5,
                                      checkpoint_dir = NULL,
                                      resume = TRUE,
                                      verbose = TRUE) {
  
  start_time <- Sys.time()
  
  # ---------------------------------------------------------------------------
  # Step 1: Calculate Optimal Worker Distribution
  # ---------------------------------------------------------------------------
  
  worker_allocation <- calculate_nested_workers(
    total_cores = total_cores,
    n_models = nrow(config),
    outer_workers = outer_workers,
    inner_workers = inner_workers,
    memory_per_model_gb = memory_per_model_gb,
    grid_size = grid_size_eval,
    cv_folds = cv_folds_eval,
    verbose = verbose
  )
  
  outer_workers <- worker_allocation$outer_workers
  inner_workers <- worker_allocation$inner_workers
  
  if (verbose) {
    cli::cli_h1("Nested Parallel HPC Evaluation")
    cli::cli_alert_info("Total cores allocated: {total_cores}")
    cli::cli_alert_info("Outer workers (concurrent models): {outer_workers}")
    cli::cli_alert_info("Inner workers (per model): {inner_workers}")
    cli::cli_alert_info("Effective parallelization: {outer_workers} × {inner_workers} = {outer_workers * inner_workers} cores")
    cli::cli_alert_info("Models to evaluate: {nrow(config)}")
  }
  
  # ---------------------------------------------------------------------------
  # Step 2: Setup Output and Checkpointing
  # ---------------------------------------------------------------------------
  
  if (is.null(output_dir)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_dir <- file.path("results", paste0(variable, "_nested_", timestamp))
  }
  fs::dir_create(output_dir, recurse = TRUE)
  
  if (is.null(checkpoint_dir)) {
    checkpoint_dir <- file.path(output_dir, "checkpoints")
  }
  fs::dir_create(checkpoint_dir, recurse = TRUE)
  
  # Load checkpoint state if resuming
  checkpoint_state <- load_nested_checkpoint(checkpoint_dir, resume)
  completed_models <- checkpoint_state$completed_models
  pending_models <- setdiff(seq_len(nrow(config)), completed_models)
  
  if (verbose && length(completed_models) > 0) {
    cli::cli_alert_success("Resuming: {length(completed_models)} models already completed")
    cli::cli_alert_info("Remaining models: {length(pending_models)}")
  }
  
  # ---------------------------------------------------------------------------
  # Step 3: Initialize Resource Manager
  # ---------------------------------------------------------------------------
  
  resource_manager <- initialize_resource_manager(
    total_cores = total_cores,
    outer_workers = outer_workers,
    inner_workers = inner_workers,
    memory_limit_gb = memory_per_model_gb * outer_workers * 1.2,
    output_dir = output_dir
  )
  
  # ---------------------------------------------------------------------------
  # Step 4: Setup Outer Parallel Backend
  # ---------------------------------------------------------------------------
  
  # Save current plan to restore later
  original_plan <- future::plan()
  
  # Setup outer parallelization with multicore (FORK on Linux)
  future::plan(
    future::multicore,
    workers = outer_workers
  )
  
  on.exit({
    # Restore original plan and cleanup
    future::plan(original_plan)
    cleanup_resource_manager(resource_manager)
  }, add = TRUE)
  
  # ---------------------------------------------------------------------------
  # Step 5: Process Models in Batches
  # ---------------------------------------------------------------------------
  
  all_results <- list()
  n_batches <- ceiling(length(pending_models) / outer_workers)
  
  if (verbose && n_batches > 0) {
    cli::cli_progress_bar(
      name = "Processing models",
      total = length(pending_models),
      clear = FALSE
    )
  }
  
  for (batch_idx in seq_len(n_batches)) {
    
    batch_start <- (batch_idx - 1) * outer_workers + 1
    batch_end <- min(batch_idx * outer_workers, length(pending_models))
    batch_models <- pending_models[batch_start:batch_end]
    
    if (verbose) {
      cli::cli_h3("Batch {batch_idx}/{n_batches}")
      cli::cli_alert_info("Processing models {min(batch_models)} to {max(batch_models)}")
    }
    
    # Check resource availability
    check_resource_availability(resource_manager, length(batch_models))
    
    # Process batch with outer parallelization
    batch_results <- future.apply::future_lapply(
      X = batch_models,
      FUN = function(model_idx) {
        
        # Set thread control for this outer worker
        set_nested_thread_control()
        
        # Evaluate model with inner workers
        result <- evaluate_model_with_inner_workers(
          config_row = config[model_idx, ],
          input_data = input_data,
          covariate_data = covariate_data,
          variable = variable,
          row_index = model_idx,
          inner_workers = inner_workers,
          output_dir = output_dir,
          grid_size = grid_size_eval,
          bayesian_iter = bayesian_iter_eval,
          cv_folds = cv_folds_eval,
          resource_manager = resource_manager
        )
        
        return(result)
      },
      future.scheduling = FALSE,  # Dynamic scheduling (work stealing)
      future.chunk.size = 1,  # One model per worker
      future.seed = TRUE
    )
    
    # Store results and checkpoint
    for (i in seq_along(batch_results)) {
      model_idx <- batch_models[i]
      all_results[[as.character(model_idx)]] <- batch_results[[i]]
      
      # Save individual model checkpoint
      save_model_checkpoint(
        result = batch_results[[i]],
        model_idx = model_idx,
        checkpoint_dir = checkpoint_dir
      )
    }
    
    # Update checkpoint state
    completed_models <- c(completed_models, batch_models)
    save_nested_checkpoint_state(
      completed_models = completed_models,
      checkpoint_dir = checkpoint_dir
    )
    
    # Update performance metrics
    update_performance_metrics(
      resource_manager = resource_manager,
      completed = length(completed_models),
      total = nrow(config)
    )
    
    # Memory cleanup between batches
    gc(verbose = FALSE, full = TRUE)
    
    if (verbose) {
      cli::cli_progress_update(length(completed_models))
    }
  }
  
  if (verbose) {
    cli::cli_progress_done()
  }
  
  # ---------------------------------------------------------------------------
  # Step 6: Compile Results
  # ---------------------------------------------------------------------------
  
  results_df <- compile_nested_results(
    results_list = all_results,
    config = config,
    output_dir = output_dir
  )
  
  # Save final results
  output_file <- file.path(output_dir, paste0(variable, "_nested_results.qs"))
  qs::qsave(results_df, output_file)
  
  # Mark completion
  file.create(file.path(output_dir, ".complete"))
  
  # Final summary
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "hours"))
  
  if (verbose) {
    cli::cli_h1("Evaluation Complete")
    cli::cli_alert_success("Total time: {round(total_time, 2)} hours")
    cli::cli_alert_info("Models evaluated: {nrow(results_df)}")
    cli::cli_alert_info("Average time per model: {round((total_time * 60) / nrow(results_df), 2)} minutes")
    cli::cli_alert_info("Results saved to: {output_file}")
  }
  
  return(results_df)
}


#' Calculate Optimal Nested Worker Distribution
#'
#' @description
#' Determines the optimal distribution of outer and inner workers based on
#' available resources and workload characteristics.
#'
#' @keywords internal
calculate_nested_workers <- function(total_cores,
                                    n_models,
                                    outer_workers = NULL,
                                    inner_workers = NULL,
                                    memory_per_model_gb = 15,
                                    grid_size = 10,
                                    cv_folds = 5,
                                    verbose = TRUE) {
  
  # If both specified, validate and return
  if (!is.null(outer_workers) && !is.null(inner_workers)) {
    if (outer_workers * inner_workers > total_cores) {
      cli::cli_abort(
        "outer_workers ({outer_workers}) × inner_workers ({inner_workers}) = {outer_workers * inner_workers} exceeds total_cores ({total_cores})"
      )
    }
    return(list(
      outer_workers = outer_workers,
      inner_workers = inner_workers,
      efficiency = (outer_workers * inner_workers) / total_cores
    ))
  }
  
  # Auto-calculate optimal distribution
  system_memory_gb <- as.numeric(system("free -g | awk '/^Mem:/ {print $2}'", intern = TRUE))
  max_concurrent <- floor(system_memory_gb * 0.7 / memory_per_model_gb)
  
  # Minimum inner workers should handle CV folds efficiently
  min_inner <- max(2, cv_folds)
  
  # Find best factorization
  best_efficiency <- 0
  best_outer <- 1
  best_inner <- total_cores
  
  for (outer in 1:min(max_concurrent, n_models, total_cores)) {
    inner <- floor(total_cores / outer)
    
    if (inner >= min_inner) {
      efficiency <- (outer * inner) / total_cores
      
      # Prefer configurations that don't oversubscribe
      if (outer <= n_models && efficiency > best_efficiency) {
        best_efficiency <- efficiency
        best_outer <- outer
        best_inner <- inner
      }
    }
  }
  
  if (verbose) {
    cli::cli_alert_info("Auto-calculated worker distribution:")
    cli::cli_alert_info("  Outer workers: {best_outer} (concurrent models)")
    cli::cli_alert_info("  Inner workers: {best_inner} (workers per model)")
    cli::cli_alert_info("  Efficiency: {round(best_efficiency * 100, 1)}%")
  }
  
  return(list(
    outer_workers = best_outer,
    inner_workers = best_inner,
    efficiency = best_efficiency
  ))
}


#' Load Nested Checkpoint State
#'
#' @keywords internal
load_nested_checkpoint <- function(checkpoint_dir, resume = TRUE) {
  
  checkpoint_file <- file.path(checkpoint_dir, "checkpoint_state.qs")
  progress_file   <- file.path(checkpoint_dir, ".progress")
  
  if (resume) {
    # Try to load main checkpoint file
    if (file.exists(checkpoint_file)) {
      state <- tryCatch(qs::qread(checkpoint_file), error = function(e) {
        cli::cli_alert_warning("Main checkpoint corrupted, trying backup...")
        NULL
      })
      
      if (!is.null(state)) {
        return(state)
      }
    }
    
    # Fallback to progress file
    if (file.exists(progress_file)) {
      progress_data    <- readLines(progress_file)
      completed_models <- as.integer(progress_data)
      
      cli::cli_alert_info("Recovered {length(completed_models)} completed models from progress file")
      return(list(
        completed_models = completed_models,
        timestamp        = Sys.time()
      ))
    }
    
    # Last resort: scan for individual model files
    model_files <- list.files(checkpoint_dir, pattern = "^model_[0-9]{6}\\.qs$", full.names = FALSE)
    if (length(model_files) > 0) {
      model_idxs       <- as.integer(gsub("^model_|.qs$", "", model_files))
      completed_models <- sort(model_idxs)
      
      cli::cli_alert_info("Recovered {length(completed_models)} models from checkpoint files")
      return(list(
        completed_models = completed_models,
        timestamp        = Sys.time()
      ))
    }
  }
  
  return(list(
    completed_models = integer(0),
    timestamp        = Sys.time()
  ))
}


#' Save Nested Checkpoint State
#'
#' @keywords internal
save_nested_checkpoint_state <- function(completed_models, checkpoint_dir) {
  
  state <- list(
    completed_models = completed_models,
    timestamp = Sys.time()
  )
  
  checkpoint_file <- file.path(checkpoint_dir, "checkpoint_state.qs")
  qs::qsave(state, checkpoint_file)
  
  invisible(state)
}


#' Save Individual Model Checkpoint
#'
#' @keywords internal
save_model_checkpoint <- function(result, model_idx, checkpoint_dir) {
  
  model_file <- file.path(
    checkpoint_dir,
    sprintf("model_%06d.qs", model_idx)
  )
  
  # Atomic write: save to temp file first, then rename
  temp_file <- paste0(model_file, ".tmp")
  
  tryCatch({
    # Save to temporary file
    qs::qsave(result, temp_file)
    
    # Verify the file was written correctly
    test_read <- tryCatch(qs::qread(temp_file), error = function(e) NULL)
    
    if (!is.null(test_read)) {
      # Atomic rename (on most filesystems)
      file.rename(temp_file, model_file)
      
      # Also save a lightweight progress marker
      progress_file <- file.path(checkpoint_dir, ".progress")
      if (file.exists(progress_file)) {
        progress_data        <- readLines(progress_file)
        completed_model_idxs <- as.integer(progress_data)
        completed_model_idxs <- unique(c(completed_model_idxs, model_idx))
      } else {
        completed_model_idxs <- model_idx
      }
      writeLines(as.character(sort(completed_model_idxs)), progress_file)
      
    } else {
      cli::cli_alert_warning("Failed to verify checkpoint for model {model_idx}")
      unlink(temp_file, force = TRUE)
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to save checkpoint for model {model_idx}: {e$message}")
    unlink(temp_file, force = TRUE)
  })
  
  invisible(model_file)
}


#' Compile Nested Results
#'
#' @keywords internal
compile_nested_results <- function(results_list, config, output_dir) {
  
  # Convert list to tibble
  results_df <- purrr::map_dfr(results_list, function(res) {
    if ("metrics" %in% names(res)) {
      tibble::tibble(
        row_index = res$row_index,
        wflow_id = res$wflow_id,
        rmse = res$metrics$rmse,
        rsq = res$metrics$rsq,
        mae = res$metrics$mae,
        timing_seconds = res$timing,
        memory_gb = res$memory_peak_gb,
        status = res$status
      )
    } else {
      tibble::tibble(
        row_index = res$row_index,
        wflow_id = res$wflow_id,
        error = res$error,
        timing_seconds = res$timing,
        status = res$status
      )
    }
  })
  
  # Join with config to get full details
  results_df <- results_df %>%
    dplyr::left_join(
      config %>% dplyr::mutate(row_index = dplyr::row_number()),
      by = "row_index"
    )
  
  return(results_df)
}


#' Set Thread Control for Nested Worker
#'
#' @keywords internal
set_nested_thread_control <- function() {
  
  # Single-threaded for outer workers to avoid oversubscription
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1"
  )
  
  options(
    ranger.num.threads = 1,
    xgboost.nthread = 1,
    mc.cores = 1
  )
  
  invisible(NULL)
}