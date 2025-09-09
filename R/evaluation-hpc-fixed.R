#' Evaluate Models on HPC (Fixed Version)
#'
#' @description
#' Fixed version of HPC evaluation that avoids closure/serialization issues.
#' Uses multisession backend instead of multicore to prevent environment capture problems.
#'
#' @inheritParams evaluate_models_hpc
#'
#' @export
evaluate_models_hpc_fixed <- function(config,
                                      input_data,
                                      covariate_data = NULL,
                                      variable,
                                      output_dir = tempdir(),
                                      grid_size = 10,
                                      bayesian_iter = 15,
                                      cv_folds = 10,
                                      n_outer_workers = NULL,
                                      n_inner_workers = 2,
                                      seed = 123,
                                      verbose = TRUE) {
  
  # Get the original function
  source_env <- environment(evaluate_models_hpc)
  
  # Copy all the initial setup from the original function
  # (validation, directory creation, etc.)
  
  if (verbose) {
    cli::cli_h1("HPC Model Evaluation (Fixed)")
    cli::cli_alert_info("Using multisession backend to avoid closure issues")
  }
  
  # Detect parallel context
  context <- detect_parallel_context()
  
  # Determine number of workers
  if (is.null(n_outer_workers)) {
    n_outer_workers <- get_optimal_workers(context, level = "outer")
  }
  
  if (verbose) {
    cli::cli_alert_info("Outer workers: {n_outer_workers}")
    cli::cli_alert_info("Inner workers per model: {n_inner_workers}")
  }
  
  # Create data split
  set.seed(seed)
  data_split <- rsample::initial_split(input_data, prop = 0.8, strata = !!variable)
  
  # Set up checkpoint directory
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE)
  }
  
  # Check for completed models
  completed_models <- integer()
  if (dir.exists(checkpoint_dir)) {
    checkpoint_files <- list.files(checkpoint_dir, 
                                  pattern = "^model_[0-9]+\\.qs$", 
                                  full.names = FALSE)
    if (length(checkpoint_files) > 0) {
      completed_models <- as.integer(gsub("model_|\\.qs", "", checkpoint_files))
      if (verbose) {
        cli::cli_alert_info("Found {length(completed_models)} completed models")
      }
    }
  }
  
  # CRITICAL FIX: Use multisession instead of multicore
  # This avoids the environment serialization issues
  future::plan(
    future::multisession,  # Changed from multicore
    workers = n_outer_workers
  )
  
  if (verbose) {
    cli::cli_alert_success("Using multisession backend - avoids closure issues")
  }
  
  # Determine which models to process
  models_to_process <- setdiff(seq_len(nrow(config)), completed_models)
  
  if (length(models_to_process) == 0) {
    cli::cli_alert_success("All models already completed!")
    # Load and return results
    results <- load_checkpoint_results(checkpoint_dir, nrow(config))
    return(results)
  }
  
  # Process models with better error handling
  results <- future.apply::future_lapply(
    models_to_process,
    function(i) {
      # Create a clean evaluation function that doesn't capture environments
      result <- evaluate_model_clean(
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
        n_cv_cores = n_inner_workers,
        seed = seed
      )
      
      # Save checkpoint
      save_checkpoint_safe(result, i, checkpoint_dir)
      
      return(result)
    },
    future.seed = TRUE,  # Ensure reproducibility
    future.scheduling = 2L  # Work stealing
  )
  
  # Clean up
  future::plan(future::sequential)
  
  if (verbose) {
    cli::cli_alert_success("Evaluation complete!")
  }
  
  return(results)
}

#' Clean Model Evaluation Function
#' 
#' @description
#' Wrapper around evaluate_configuration that ensures clean environment.
#' Avoids capturing unnecessary environments that cause serialization issues.
#'
#' @keywords internal
evaluate_model_clean <- function(config_row,
                                input_data,
                                data_split,
                                config_id,
                                covariate_data,
                                variable,
                                output_dir,
                                grid_size,
                                bayesian_iter,
                                cv_folds,
                                n_cv_cores,
                                seed) {
  
  # Use tryCatch with standard evaluation instead of safely_execute
  tryCatch({
    # Call the evaluation function directly
    evaluate_configuration(
      config_row = config_row,
      input_data = input_data,
      data_split = data_split,
      config_id = config_id,
      covariate_data = covariate_data,
      variable = variable,
      output_dir = output_dir,
      grid_size = grid_size,
      bayesian_iter = bayesian_iter,
      cv_folds = cv_folds,
      parallel_cv = TRUE,
      n_cv_cores = n_cv_cores,
      prune_models = FALSE,
      seed = seed
    )
  }, error = function(e) {
    # Return a clean error result without capturing environments
    list(
      config_id = config_id,
      status = "failed",
      error_message = as.character(e$message),
      rsq = NA_real_,
      rmse = NA_real_,
      rrmse = NA_real_,
      rpd = NA_real_,
      ccc = NA_real_,
      mae = NA_real_
    )
  })
}

#' Save Checkpoint Safely
#'
#' @description
#' Atomic checkpoint saving without environment capture.
#'
#' @keywords internal
save_checkpoint_safe <- function(result, model_id, checkpoint_dir) {
  checkpoint_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", model_id))
  temp_file <- paste0(checkpoint_file, ".tmp")
  
  tryCatch({
    qs::qsave(result, temp_file, preset = "fast")
    file.rename(temp_file, checkpoint_file)
  }, error = function(e) {
    if (file.exists(temp_file)) file.remove(temp_file)
    warning(sprintf("Failed to save checkpoint for model %d: %s", model_id, e$message))
  })
}

#' Load Checkpoint Results
#'
#' @description
#' Load saved checkpoint files.
#'
#' @keywords internal
load_checkpoint_results <- function(checkpoint_dir, n_models) {
  results <- lapply(seq_len(n_models), function(i) {
    checkpoint_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", i))
    if (file.exists(checkpoint_file)) {
      qs::qread(checkpoint_file)
    } else {
      NULL
    }
  })
  
  # Filter out NULL results
  results[!sapply(results, is.null)]
}

#' Get Optimal Workers
#'
#' @description
#' Determine optimal number of workers based on context.
#'
#' @keywords internal
get_optimal_workers <- function(context, level = "outer") {
  if (context$context == "hpc") {
    # Use SLURM/PBS allocated cores
    slurm_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", NA))
    pbs_cpus <- as.integer(Sys.getenv("PBS_NUM_PPN", NA))
    
    allocated <- if (!is.na(slurm_cpus)) {
      slurm_cpus
    } else if (!is.na(pbs_cpus)) {
      pbs_cpus
    } else {
      parallel::detectCores()
    }
    
    if (level == "outer") {
      # Leave some cores for system
      max(1, allocated - 2)
    } else {
      # Inner workers
      max(1, min(4, allocated / 4))
    }
  } else {
    # Local machine
    if (level == "outer") {
      max(1, parallel::detectCores() - 2)
    } else {
      2
    }
  }
}