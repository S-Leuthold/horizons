#' HPC Evaluation Orchestrator
#'
#' @description
#' Runs model evaluation using nested parallelization for HPC environments.
#' Respects resources allocated by the scheduler (SLURM/PBS) or uses available cores.
#'
#' @param config Data frame of model configurations
#' @param input_data Data frame with spectral data
#' @param covariate_data Optional data frame with covariates
#' @param variable Character. Name of response variable
#' @param output_dir Character. Directory for results
#' @param grid_size Integer. Grid search size (default: 10)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 15)
#' @param cv_folds Integer. Cross-validation folds (default: 10)
#' @param outer_workers Integer. Number of concurrent models (auto if NULL)
#' @param inner_workers Integer. Workers per model (auto if NULL)
#' @param seed Integer. Random seed (default: 307)
#' @param resume Logical. Resume from checkpoints (default: TRUE)
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return List with evaluation results
#'
#' @export
evaluate_models_hpc <- function(config,
                               input_data,
                               covariate_data = NULL,
                               variable,
                               output_dir     = NULL,
                               grid_size      = 10,
                               bayesian_iter  = 15,
                               cv_folds       = 10,
                               outer_workers  = NULL,
                               inner_workers  = NULL,
                               seed           = 307,
                               resume         = TRUE,
                               verbose        = TRUE) {
  
  ## Step 1: Input Validation --------------------------------------------------
  
  if (!is.data.frame(config)) {
    cli::cli_abort("▶ evaluate_models_hpc: config must be a data frame")
  }
  
  if (!variable %in% names(input_data)) {
    cli::cli_abort("▶ evaluate_models_hpc: Response variable '{variable}' not found in input_data")
  }
  
  # Validate response variable has sufficient data
  response_vals <- input_data[[variable]]
  n_valid <- sum(!is.na(response_vals))
  
  if (n_valid < 20) {
    cli::cli_abort("▶ evaluate_models_hpc: Response variable has only {n_valid} non-missing values (minimum 20 required)")
  }
  
  if (var(response_vals, na.rm = TRUE) < .Machine$double.eps) {
    cli::cli_abort("▶ evaluate_models_hpc: Response variable has no variation")
  }
  
  # Check required config columns
  required_cols <- c("model", "preprocessing", "transformation", "feature_selection")
  missing_cols <- setdiff(required_cols, names(config))
  if (length(missing_cols) > 0) {
    cli::cli_abort("▶ evaluate_models_hpc: config missing required columns: {missing_cols}")
  }
  
  ## Step 2: Validate Worker Specification -------------------------------------
  
  # CRITICAL: Require explicit worker specification for HPC
  if (is.null(outer_workers) || is.null(inner_workers)) {
    cli::cli_abort("▶ evaluate_models_hpc: Both outer_workers and inner_workers must be specified for HPC execution")
  }
  
  # Validate worker numbers
  if (outer_workers < 1 || inner_workers < 1) {
    cli::cli_abort("▶ evaluate_models_hpc: outer_workers and inner_workers must be >= 1")
  }
  
  # Check total cores don't exceed available
  total_requested <- outer_workers * inner_workers
  cores_available <- parallel::detectCores()
  
  if (total_requested > cores_available) {
    cli::cli_abort("▶ evaluate_models_hpc: Requested {total_requested} cores ({outer_workers} × {inner_workers}) but only {cores_available} available")
  }
  
  ## Step 3: Set Package Thread Controls ---------------------------------------
  
  # Control R package threading to prevent conflicts with nested parallelization
  # Users should set system-level thread controls (OMP_NUM_THREADS, etc.) in their SLURM script
  
  # Package-specific thread controls
  data.table::setDTthreads(1)
  
  options(
    mc.cores = 1,           # Prevent unintended forking
    ranger.num.threads = 1,  # Random forest threading
    xgboost.nthread = 1     # XGBoost threading
  )
  
  # Warn about system thread settings if not configured
  if (verbose) {
    current_omp <- Sys.getenv("OMP_NUM_THREADS", unset = NA)
    if (is.na(current_omp) || current_omp != "1") {
      cli::cli_alert_warning("OMP_NUM_THREADS not set to 1 - may cause thread oversubscription")
      cli::cli_alert_info("Recommend setting in SLURM script: export OMP_NUM_THREADS=1")
    }
  }
  
  if (verbose) {
    cli::cli_h1("HPC Evaluation")
    cli::cli_alert_info("Workers: {outer_workers} outer × {inner_workers} inner = {total_requested} cores")
    cli::cli_alert_info("Models to evaluate: {nrow(config)}")
    cli::cli_alert_success("Thread safety controls applied")
  }
  
  ## Step 4: Setup Output and Checkpoint Directories ---------------------------
  
  # Use provided output_dir or create default
  if (is.null(output_dir)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_dir <- file.path("output", paste0(variable, "_hpc_", timestamp))
  }
  
  # Create directory structure
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE)
  }
  
  # Also create subdirectories for organization
  dir.create(file.path(output_dir, "results"), showWarnings = FALSE)
  dir.create(file.path(output_dir, "errors"), showWarnings = FALSE)
  dir.create(file.path(output_dir, "summary"), showWarnings = FALSE)
  
  if (verbose) {
    cli::cli_alert_info("Output directory: {output_dir}")
  }
  
  # Check for existing checkpoints if resume = TRUE
  completed_models <- integer(0)
  if (resume && dir.exists(checkpoint_dir)) {
    # Check for batch files first
    batch_files <- list.files(checkpoint_dir, pattern = "^batch_.*\\.qs$", full.names = FALSE)
    if (length(batch_files) > 0) {
      # Each batch contains 100 models
      batch_nums <- as.integer(gsub("batch_|\\.qs", "", batch_files))
      batch_nums <- batch_nums[!is.na(batch_nums)]
      
      # Convert batch numbers to model indices
      for (batch in batch_nums) {
        start_idx <- (batch - 1) * 100 + 1
        end_idx <- batch * 100
        completed_models <- c(completed_models, start_idx:end_idx)
      }
    }
    
    # Then check for individual checkpoint files (not yet consolidated)
    checkpoint_files <- list.files(checkpoint_dir, pattern = "^model_.*\\.qs$", full.names = FALSE)
    if (length(checkpoint_files) > 0) {
      # Extract model indices from filenames
      individual_models <- as.integer(gsub("model_|\\.qs", "", checkpoint_files))
      individual_models <- individual_models[!is.na(individual_models)]
      completed_models <- c(completed_models, individual_models)
    }
    
    # Remove duplicates and validate
    completed_models <- unique(completed_models)
    completed_models <- completed_models[completed_models >= 1 & completed_models <= nrow(config)]
    completed_models <- sort(completed_models)
    
    if (length(completed_models) > 0 && verbose) {
      cli::cli_alert_info("Found {length(completed_models)} completed models to resume from")
      cli::cli_alert_info("Batches: {length(batch_files)}, Individual: {length(checkpoint_files)}")
    }
  }
  
  ## Step 5: Create Data Split -------------------------------------------------
  
  # Create train/test split (same for all models)
  set.seed(seed)
  data_split <- rsample::initial_split(
    data = input_data,
    prop = 0.8,
    strata = variable  # variable is already a character string
  )
  
  if (verbose) {
    train_n <- nrow(rsample::training(data_split))
    test_n <- nrow(rsample::testing(data_split))
    cli::cli_alert_info("Data split: {train_n} training, {test_n} testing samples")
  }
  
  ## Step 6: Configure Nested Parallelization ----------------------------------
  
  # Track start time for execution summary
  start_time <- Sys.time()
  
  # Set up cleanup on exit
  on.exit({
    # Reset future plan to sequential
    tryCatch(
      future::plan(future::sequential),
      error = function(e) NULL
    )
    
    # Clean up any temp files
    temp_files <- list.files(checkpoint_dir, pattern = "\\.tmp$", full.names = TRUE)
    if (length(temp_files) > 0) {
      file.remove(temp_files)
    }
  }, add = TRUE)
  
  # Set up outer parallel backend using future
  future::plan(future::multicore, workers = outer_workers)
  
  if (verbose) {
    cli::cli_alert_info("Configured outer parallelization with {outer_workers} workers")
  }
  
  ## Step 7: Process Models with Nested Parallelization ------------------------
  
  # Determine which models to process (excluding completed ones)
  models_to_process <- setdiff(seq_len(nrow(config)), completed_models)
  
  if (length(models_to_process) == 0) {
    cli::cli_alert_success("All models already completed! Loading results...")
    
    # Load all completed checkpoint files
    results <- lapply(seq_len(nrow(config)), function(i) {
      checkpoint_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", i))
      if (file.exists(checkpoint_file)) {
        qs::qread(checkpoint_file)
      } else {
        cli::cli_alert_warning("Missing checkpoint for model {i}")
        NULL
      }
    })
    
    # Filter out any NULL results
    results <- results[!sapply(results, is.null)]
    
    if (verbose) {
      cli::cli_alert_success("Loaded {length(results)} completed models")
    }
    
    return(results)
  }
  
  if (verbose) {
    cli::cli_alert_info("Processing {length(models_to_process)} models...")
  }
  
  # Process models in parallel outer loop
  results <- future.apply::future_lapply(
    models_to_process,
    function(i) {
      
      # Call evaluate_single_model_local with parallel CV enabled
      result <- tryCatch({
        evaluate_configuration(
          config_row = config[i, , drop = FALSE],
          input_data = input_data,
          data_split = data_split,
          config_id = i,
          covariate_data = covariate_data,
          variable = variable,
          output_dir = output_dir,
          grid_size = grid_size,
          bayesian_iter = bayesian_iter,
          cv_folds = cv_folds,
          parallel_cv = TRUE,           # Enable parallel CV
          n_cv_cores = inner_workers,   # Use inner workers for CV
          prune_models = FALSE,         # Can make this configurable
          seed = seed                   # Same seed for all models for fair comparison
        )
      }, error = function(e) {
        cli::cli_alert_danger("Model {i} failed: {e$message}")
        list(
          config_id = i,
          status = "failed",
          error = e$message,
          metrics = NULL
        )
      })
      
      # Save checkpoint for this model with atomic write
      if (!is.null(checkpoint_dir)) {
        checkpoint_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", i))
        
        # Atomic write: save to temp file first, then rename
        temp_file <- paste0(checkpoint_file, ".tmp")
        tryCatch({
          qs::qsave(result, temp_file)
          # Rename is atomic on most filesystems
          file.rename(temp_file, checkpoint_file)
        }, error = function(e) {
          # Clean up temp file on error
          if (file.exists(temp_file)) file.remove(temp_file)
          cli::cli_alert_warning("Failed to save checkpoint for model {i}: {e$message}")
        })
        
        # Consolidate every 100 models
        if (i %% 100 == 0) {
          batch_num <- i %/% 100
          batch_file <- file.path(checkpoint_dir, sprintf("batch_%06d.qs", batch_num))
          
          # Collect the last 100 individual checkpoints
          start_idx <- (batch_num - 1) * 100 + 1
          end_idx <- i
          
          batch_results <- lapply(start_idx:end_idx, function(idx) {
            ind_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", idx))
            if (file.exists(ind_file)) {
              qs::qread(ind_file)
            } else {
              NULL
            }
          })
          
          # Save batch file atomically
          batch_temp <- paste0(batch_file, ".tmp")
          tryCatch({
            qs::qsave(batch_results, batch_temp)
            file.rename(batch_temp, batch_file)
            
            # Remove individual files after successful batch save
            for (idx in start_idx:end_idx) {
              ind_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", idx))
              if (file.exists(ind_file)) file.remove(ind_file)
            }
            
            if (verbose) {
              cli::cli_alert_success("Consolidated models {start_idx}-{end_idx} into batch {batch_num}")
            }
          }, error = function(e) {
            if (file.exists(batch_temp)) file.remove(batch_temp)
            cli::cli_alert_warning("Failed to consolidate batch {batch_num}: {e$message}")
          })
        }
      }
      
      return(result)
    },
    future.seed = seed  # Base seed for reproducibility
  )
  
  ## Step 8: Compile Results and Create Summary --------------------------------
  
  # Load any previously completed models if resuming
  if (resume && length(completed_models) > 0) {
    previous_results <- list()
    
    # Load from batch files first
    batch_files <- list.files(checkpoint_dir, pattern = "^batch_.*\\.qs$", full.names = TRUE)
    for (batch_file in batch_files) {
      tryCatch({
        batch_data <- qs::qread(batch_file)
        previous_results <- c(previous_results, batch_data)
      }, error = function(e) {
        cli::cli_alert_warning("Failed to load batch file {basename(batch_file)}: {e$message}")
      })
    }
    
    # Then load any remaining individual checkpoints
    individual_files <- list.files(checkpoint_dir, pattern = "^model_.*\\.qs$", full.names = TRUE)
    for (ind_file in individual_files) {
      tryCatch({
        ind_data <- qs::qread(ind_file)
        previous_results <- c(previous_results, list(ind_data))
      }, error = function(e) {
        cli::cli_alert_warning("Failed to load checkpoint {basename(ind_file)}: {e$message}")
      })
    }
    
    # Combine with newly computed results
    all_results <- c(previous_results[!sapply(previous_results, is.null)], results)
  } else {
    all_results <- results
  }
  
  # Separate successful and failed models
  successful_results <- all_results[sapply(all_results, function(x) {
    !is.null(x) && (!is.null(x$status) && x$status != "failed")
  })]
  
  failed_results <- all_results[sapply(all_results, function(x) {
    !is.null(x) && (!is.null(x$status) && x$status == "failed")
  })]
  
  # Calculate execution time
  execution_time <- difftime(Sys.time(), start_time, units = "mins")
  
  # Extract top models if available
  top_models <- NULL
  if (length(successful_results) > 0) {
    # Extract metrics from successful results
    model_metrics <- purrr::map_dfr(successful_results, function(res) {
      if (!is.null(res$metrics)) {
        tibble::tibble(
          config_id = res$config_id,
          workflow_id = res$workflow_id,
          model = res$config$model,
          preprocessing = res$config$preprocessing,
          transformation = res$config$transformation,
          feature_selection = res$config$feature_selection,
          rrmse = res$metrics$.estimate[res$metrics$.metric == "rrmse"][1],
          rsq = res$metrics$.estimate[res$metrics$.metric == "rsq"][1],
          ccc = res$metrics$.estimate[res$metrics$.metric == "ccc"][1],
          rpd = res$metrics$.estimate[res$metrics$.metric == "rpd"][1]
        )
      }
    })
    
    if (nrow(model_metrics) > 0) {
      top_models <- model_metrics %>%
        dplyr::arrange(rrmse) %>%
        dplyr::slice_head(n = 5)
    }
  }
  
  # Print attractive summary
  if (verbose) {
    cli::cli_h1("HPC Evaluation Complete")
    
    # Execution summary
    cli::cli_h2("Execution Summary")
    cli::cli_alert_success("Total models: {nrow(config)}")
    cli::cli_alert_info("Completed: {length(successful_results)}")
    if (length(failed_results) > 0) {
      cli::cli_alert_warning("Failed: {length(failed_results)}")
    }
    if (length(completed_models) > 0) {
      cli::cli_alert_info("Previously completed (resumed): {length(completed_models)}")
    }
    cli::cli_alert_info("Execution time: {round(as.numeric(execution_time), 1)} minutes")
    cli::cli_alert_info("Parallel configuration: {outer_workers} outer × {inner_workers} inner workers")
    
    # Top models
    if (!is.null(top_models) && nrow(top_models) > 0) {
      cli::cli_h2("Top 5 Models by RRMSE")
      for (i in 1:nrow(top_models)) {
        m <- top_models[i, ]
        cli::cli_alert_info(
          "{i}. {m$model} | {m$preprocessing} | {m$transformation} | RRMSE: {round(m$rrmse, 4)}"
        )
      }
    }
    
    # Output directory structure
    cli::cli_h2("Output Directory Structure")
    cli::cli_text("{.path {output_dir}/}")
    cli::cli_text("├── {.file checkpoints/}")
    cli::cli_text("├── {.file results/}")  
    cli::cli_text("├── {.file errors/}")
    cli::cli_text("└── {.file summary/}")
  }
  
  # Return summary information
  summary_output <- list(
    execution = list(
      n_total = nrow(config),
      n_successful = length(successful_results),
      n_failed = length(failed_results),
      n_resumed = length(completed_models),
      time_minutes = as.numeric(execution_time),
      timestamp = Sys.time()
    ),
    configuration = list(
      outer_workers = outer_workers,
      inner_workers = inner_workers,
      total_cores = outer_workers * inner_workers,
      seed = seed
    ),
    paths = list(
      output_dir = output_dir,
      checkpoint_dir = checkpoint_dir,
      results_dir = file.path(output_dir, "results"),
      errors_dir = file.path(output_dir, "errors")
    ),
    top_models = top_models
  )
  
  class(summary_output) <- c("horizons_hpc_summary", "list")
  
  # Save summary to file for later reference
  summary_file <- file.path(output_dir, "summary", "evaluation_summary.qs")
  tryCatch({
    qs::qsave(summary_output, summary_file)
    if (verbose) {
      cli::cli_alert_success("Summary saved to: {summary_file}")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to save summary: {e$message}")
  })
  
  return(invisible(summary_output))
}