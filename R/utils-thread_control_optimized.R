#' Optimized Thread Control for HPC Environments
#'
#' @description
#' Intelligent thread management that balances parallelization at worker
#' and model levels for optimal HPC performance.
#'
#' @keywords thread control hpc optimization
#' @name thread_control_optimized
NULL


#' Calculate Optimal Thread Allocation
#'
#' @description
#' Determines optimal thread distribution between workers and internal
#' model threading based on available cores and task characteristics.
#'
#' @param total_cores Integer. Total CPU cores available
#' @param n_models Integer. Number of models to evaluate
#' @param model_type Character. Type of model being fitted
#' @param task_type Character. Either "grid_search" or "bayesian"
#'
#' @return List with optimal worker and thread settings
#' @export

calculate_optimal_threads <- function(total_cores, 
                                      n_models,
                                      model_type = "random_forest",
                                      task_type = "grid_search") {
  
  # Ensure we have valid inputs
  total_cores <- as.integer(total_cores)
  n_models <- as.integer(n_models)
  
  # Model-specific thread recommendations
  model_thread_efficiency <- list(
    random_forest = 4,    # Ranger scales well to 4 threads
    xgboost       = 2,    # XGBoost efficient at 2 threads
    cubist        = 1,    # Cubist doesn't benefit from threading
    elastic_net   = 1,    # glmnet is already optimized
    svm_radial    = 2,    # SVM can use 2 threads effectively
    cart          = 1     # Single-threaded is fine
  )
  
  # Get recommended threads for model type
  threads_per_model <- model_thread_efficiency[[model_type]] %||% 1
  
  # Calculate optimal worker distribution
  if (task_type == "grid_search") {
    # Grid search: maximize parallel evaluations
    if (n_models <= total_cores / threads_per_model) {
      # We can run all models in parallel with threading
      n_workers <- n_models
      model_threads <- min(threads_per_model, total_cores %/% n_workers)
    } else {
      # More models than we can run in parallel
      n_workers <- total_cores %/% threads_per_model
      model_threads <- threads_per_model
    }
  } else {
    # Bayesian optimization: balance CV fold parallelization
    # Typically want fewer workers but more threads per model
    n_workers <- min(n_models, total_cores %/% (threads_per_model * 2))
    n_workers <- max(n_workers, 10)  # At least 10 workers for CV
    model_threads <- min(threads_per_model, total_cores %/% n_workers)
  }
  
  # Ensure we don't oversubscribe
  n_workers <- min(n_workers, total_cores)
  model_threads <- max(1, min(model_threads, total_cores %/% n_workers))
  
  # BLAS threading (conservative to avoid conflicts)
  blas_threads <- if (model_threads > 1) 1 else 1
  
  list(
    n_workers = n_workers,
    model_threads = model_threads,
    blas_threads = blas_threads,
    total_threads_used = n_workers * model_threads,
    efficiency = (n_workers * model_threads) / total_cores
  )
}


#' Set Adaptive Thread Mode
#'
#' @description
#' Sets threading based on calculated optimal values rather than
#' forcing everything to single-threaded.
#'
#' @param model_threads Integer. Threads per model
#' @param blas_threads Integer. BLAS operation threads
#' @param verbose Logical. Print status messages
#'
#' @return Invisible NULL
#' @export

set_adaptive_thread_mode <- function(model_threads = 2,
                                      blas_threads = 1,
                                      verbose = TRUE) {
  
  # Set BLAS threading (conservative)
  Sys.setenv(
    OMP_NUM_THREADS        = as.character(blas_threads),
    OPENBLAS_NUM_THREADS   = as.character(blas_threads),
    MKL_NUM_THREADS        = as.character(blas_threads),
    VECLIB_MAXIMUM_THREADS = as.character(blas_threads),
    GOTO_NUM_THREADS       = as.character(blas_threads),
    BLIS_NUM_THREADS       = as.character(blas_threads)
  )
  
  # Set model-specific threading (allow controlled multi-threading)
  Sys.setenv(
    RANGER_NUM_THREADS = as.character(model_threads),
    XGBOOST_NTHREAD    = as.character(model_threads)
  )
  
  # Set R options
  options(
    ranger.num.threads = model_threads,
    ranger.num.cores   = model_threads,
    xgboost.nthread    = model_threads,
    mc.cores           = 1  # Keep mc.cores at 1 to avoid nested parallelism
  )
  
  # Use RhpcBLASctl if available
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    suppressMessages({
      RhpcBLASctl::blas_set_num_threads(blas_threads)
      RhpcBLASctl::omp_set_num_threads(blas_threads)
    })
  }
  
  if (verbose) {
    cli::cli_alert_success("Adaptive thread mode set:")
    cli::cli_alert_info("  Model threads: {model_threads}")
    cli::cli_alert_info("  BLAS threads: {blas_threads}")
  }
  
  invisible(NULL)
}


#' Configure HPC Threading Strategy
#'
#' @description
#' Main function to configure optimal threading for HPC runs based on
#' system resources and workload characteristics.
#'
#' @param n_models Integer. Number of models to evaluate
#' @param model_types Character vector. Types of models being evaluated
#' @param task_type Character. "grid_search" or "bayesian"
#' @param force_single_thread Logical. Override to force single-threading
#' @param verbose Logical. Print configuration details
#'
#' @return List with configuration details
#' @export

configure_hpc_threading <- function(n_models,
                                     model_types,
                                     task_type = "grid_search",
                                     force_single_thread = FALSE,
                                     verbose = TRUE) {
  
  # Get system info
  total_cores <- parallel::detectCores()
  
  if (verbose) {
    cli::cli_h3("HPC Threading Configuration")
    cli::cli_alert_info("System cores: {total_cores}")
    cli::cli_alert_info("Models to evaluate: {n_models}")
    cli::cli_alert_info("Task type: {task_type}")
  }
  
  # Check if forcing single-thread mode
  if (force_single_thread) {
    if (verbose) {
      cli::cli_alert_warning("Forcing single-threaded mode (conservative)")
    }
    set_single_thread_mode(verbose = FALSE)
    return(list(
      n_workers = min(n_models, total_cores),
      model_threads = 1,
      blas_threads = 1,
      strategy = "forced_single_thread"
    ))
  }
  
  # Calculate optimal configuration for each model type
  configs <- lapply(unique(model_types), function(model_type) {
    calculate_optimal_threads(
      total_cores = total_cores,
      n_models = sum(model_types == model_type),
      model_type = model_type,
      task_type = task_type
    )
  })
  
  # Use the most conservative setting across all model types
  optimal_config <- configs[[which.max(sapply(configs, function(x) x$n_workers))]]
  
  # Apply the configuration
  set_adaptive_thread_mode(
    model_threads = optimal_config$model_threads,
    blas_threads = optimal_config$blas_threads,
    verbose = verbose
  )
  
  if (verbose) {
    cli::cli_alert_success("Optimal configuration applied:")
    cli::cli_alert_info("  Workers: {optimal_config$n_workers}")
    cli::cli_alert_info("  Threads per model: {optimal_config$model_threads}")
    cli::cli_alert_info("  Total thread usage: {optimal_config$total_threads_used}/{total_cores}")
    cli::cli_alert_info("  Efficiency: {round(optimal_config$efficiency * 100, 1)}%")
  }
  
  return(c(
    optimal_config,
    list(
      total_cores = total_cores,
      strategy = "adaptive_threading"
    )
  ))
}


#' Monitor Thread Usage
#'
#' @description
#' Real-time monitoring of thread usage during HPC execution to verify
#' optimal configuration and detect issues.
#'
#' @param sample_seconds Integer. How long to sample CPU usage
#' @param verbose Logical. Print detailed output
#'
#' @return Data frame with thread usage statistics
#' @export

monitor_thread_usage <- function(sample_seconds = 5, verbose = TRUE) {
  
  if (verbose) {
    cli::cli_alert_info("Monitoring thread usage for {sample_seconds} seconds...")
  }
  
  # Get initial state
  start_time <- Sys.time()
  pid <- Sys.getpid()
  
  # Sample CPU usage (platform-specific)
  if (.Platform$OS.type == "unix") {
    # Use ps command on Unix-like systems
    samples <- list()
    
    while(difftime(Sys.time(), start_time, units = "secs") < sample_seconds) {
      cpu_info <- system(
        sprintf("ps -p %d -o %%cpu,nlwp", pid),
        intern = TRUE
      )
      
      if (length(cpu_info) > 1) {
        values <- strsplit(trimws(cpu_info[2]), "\\s+")[[1]]
        samples[[length(samples) + 1]] <- list(
          time = Sys.time(),
          cpu_percent = as.numeric(values[1]),
          n_threads = as.integer(values[2])
        )
      }
      
      Sys.sleep(0.5)
    }
    
    # Convert to data frame
    if (length(samples) > 0) {
      usage_df <- do.call(rbind, lapply(samples, as.data.frame))
      
      if (verbose) {
        cli::cli_alert_success("Monitoring complete:")
        cli::cli_alert_info("  Average CPU: {round(mean(usage_df$cpu_percent), 1)}%")
        cli::cli_alert_info("  Average threads: {round(mean(usage_df$n_threads), 1)}")
        cli::cli_alert_info("  Peak CPU: {round(max(usage_df$cpu_percent), 1)}%")
      }
      
      return(usage_df)
    }
  }
  
  if (verbose) {
    cli::cli_alert_warning("Thread monitoring not available on this platform")
  }
  
  return(NULL)
}


# Backward compatibility wrapper
`%||%` <- function(x, y) if (is.null(x)) y else x