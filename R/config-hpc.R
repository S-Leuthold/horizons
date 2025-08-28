#' Nested Parallelization Configuration
#'
#' @description
#' Functions for creating and validating nested parallelization configurations.
#' Provides intelligent defaults and optimization strategies for different
#' HPC environments and workloads.
#'
#' @name nested_parallel_config
NULL


#' Create Nested Parallelization Configuration
#'
#' @description
#' Creates an optimized configuration for nested parallel execution based on
#' system resources, workload characteristics, and optimization goals.
#'
#' @param total_cores Total cores to allocate (default: 50)
#' @param n_models Number of models to evaluate (optional)
#' @param optimization_goal Either "throughput" (maximize models/hour) or 
#'   "latency" (minimize time to first result)
#' @param memory_per_model_gb Expected memory per model in GB (default: 15)
#' @param avg_tuning_time_minutes Expected tuning time per model (default: 10)
#' @param force_outer_workers Force specific number of outer workers (optional)
#' @param force_inner_workers Force specific number of inner workers (optional)
#' @param verbose Print configuration details (default: TRUE)
#'
#' @return A nested_parallel_config object with optimization settings
#'
#' @examples
#' \dontrun{
#' # Auto-configure for 50 cores
#' config <- create_nested_config(total_cores = 50)
#' 
#' # Optimize for throughput with 192 cores
#' config <- create_nested_config(
#'   total_cores = 192,
#'   n_models = 6000,
#'   optimization_goal = "throughput"
#' )
#' 
#' # Manual configuration
#' config <- create_nested_config(
#'   total_cores = 50,
#'   force_outer_workers = 10,
#'   force_inner_workers = 5
#' )
#' }
#'
#' @export
create_nested_config <- function(total_cores = 50,
                                n_models = NULL,
                                optimization_goal = c("throughput", "latency"),
                                memory_per_model_gb = 15,
                                avg_tuning_time_minutes = 10,
                                force_outer_workers = NULL,
                                force_inner_workers = NULL,
                                verbose = TRUE) {
  
  optimization_goal <- match.arg(optimization_goal)
  
  # Get system information
  system_info <- get_system_info()
  
  # Handle forced configuration
  if (!is.null(force_outer_workers) && !is.null(force_inner_workers)) {
    
    if (force_outer_workers * force_inner_workers > total_cores) {
      cli::cli_abort(
        "Forced configuration exceeds total cores: {force_outer_workers} × {force_inner_workers} = {force_outer_workers * force_inner_workers} > {total_cores}"
      )
    }
    
    config <- list(
      total_cores = total_cores,
      outer_workers = force_outer_workers,
      inner_workers = force_inner_workers,
      strategy = "manual",
      efficiency = (force_outer_workers * force_inner_workers) / total_cores
    )
    
  } else {
    
    # Calculate optimal configuration
    if (optimization_goal == "throughput") {
      config <- optimize_for_throughput(
        total_cores = total_cores,
        n_models = n_models,
        memory_per_model_gb = memory_per_model_gb,
        system_memory_gb = system_info$memory_gb
      )
    } else {
      config <- optimize_for_latency(
        total_cores = total_cores,
        n_models = n_models,
        avg_tuning_time_minutes = avg_tuning_time_minutes
      )
    }
  }
  
  # Add metadata
  config$metadata <- list(
    created = Sys.time(),
    optimization_goal = optimization_goal,
    memory_per_model_gb = memory_per_model_gb,
    avg_tuning_time_minutes = avg_tuning_time_minutes,
    system_cores = system_info$cores,
    system_memory_gb = system_info$memory_gb,
    hostname = system_info$hostname
  )
  
  # Add advanced settings
  config$advanced <- list(
    numa_aware = system_info$numa_nodes > 1,
    memory_overcommit_factor = 1.2,
    checkpoint_frequency = max(10, config$outer_workers),
    batch_memory_cleanup = TRUE,
    enable_profiling = FALSE,
    thread_pinning = system_info$os == "Linux",
    use_fork = system_info$os != "Windows",
    load_balance_strategy = "static",
    retry_failed_models = TRUE,
    max_retries = 2
  )
  
  # Add tuning recommendations
  config$tuning <- list(
    recommended_grid_size = calculate_grid_size(config$inner_workers),
    recommended_bayes_iter = calculate_bayes_iter(config$inner_workers),
    recommended_cv_folds = min(10, max(5, config$inner_workers))
  )
  
  class(config) <- c("nested_parallel_config", "list")
  
  # Validate configuration
  validate_nested_config(config, system_info)
  
  if (verbose) {
    print(config)
  }
  
  return(config)
}


#' Optimize Configuration for Throughput
#'
#' @description
#' Calculates optimal worker distribution to maximize models processed per hour.
#'
#' @keywords internal
optimize_for_throughput <- function(total_cores,
                                    n_models,
                                    memory_per_model_gb,
                                    system_memory_gb) {
  
  # Maximum concurrent models based on memory
  max_concurrent <- floor(system_memory_gb * 0.7 / memory_per_model_gb)
  
  # If n_models not specified, assume large batch
  if (is.null(n_models)) {
    n_models <- 1000
  }
  
  # Find optimal factorization
  candidates <- find_optimal_factorizations(
    total_cores = total_cores,
    max_outer = min(max_concurrent, n_models),
    min_inner = 2
  )
  
  # Score based on throughput potential
  candidates$score <- calculate_throughput_score(
    candidates,
    n_models,
    memory_per_model_gb,
    system_memory_gb
  )
  
  # Select best
  best <- candidates[which.max(candidates$score), ]
  
  config <- list(
    total_cores = total_cores,
    outer_workers = best$outer,
    inner_workers = best$inner,
    strategy = "throughput_optimized",
    efficiency = best$efficiency,
    expected_throughput_factor = best$score
  )
  
  return(config)
}


#' Optimize Configuration for Latency
#'
#' @description
#' Calculates optimal worker distribution to minimize time to first result.
#'
#' @keywords internal
optimize_for_latency <- function(total_cores,
                                n_models,
                                avg_tuning_time_minutes) {
  
  # For latency, we want more inner workers per model
  # to complete individual models faster
  
  if (is.null(n_models)) {
    n_models <- 100
  }
  
  # Find factorizations favoring inner workers
  candidates <- find_optimal_factorizations(
    total_cores = total_cores,
    max_outer = min(total_cores / 4, n_models),
    min_inner = max(4, total_cores / 10)
  )
  
  # Score based on latency reduction
  candidates$score <- calculate_latency_score(
    candidates,
    avg_tuning_time_minutes
  )
  
  # Select best
  best <- candidates[which.max(candidates$score), ]
  
  config <- list(
    total_cores = total_cores,
    outer_workers = best$outer,
    inner_workers = best$inner,
    strategy = "latency_optimized",
    efficiency = best$efficiency,
    expected_speedup = best$inner / 2  # Rough speedup estimate
  )
  
  return(config)
}


#' Find Optimal Factorizations
#'
#' @description
#' Finds all valid factorizations of total cores into outer × inner workers.
#'
#' @keywords internal
find_optimal_factorizations <- function(total_cores, max_outer, min_inner) {
  
  factorizations <- data.frame(
    outer = integer(),
    inner = integer(),
    product = integer(),
    efficiency = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (outer in 1:min(max_outer, total_cores)) {
    inner <- floor(total_cores / outer)
    
    if (inner >= min_inner) {
      factorizations <- rbind(
        factorizations,
        data.frame(
          outer = outer,
          inner = inner,
          product = outer * inner,
          efficiency = (outer * inner) / total_cores,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # Sort by efficiency
  factorizations <- factorizations[order(factorizations$efficiency, decreasing = TRUE), ]
  
  return(factorizations)
}


#' Calculate Throughput Score
#'
#' @keywords internal
calculate_throughput_score <- function(candidates, n_models, memory_per_model_gb, system_memory_gb) {
  
  scores <- numeric(nrow(candidates))
  
  for (i in seq_len(nrow(candidates))) {
    outer <- candidates$outer[i]
    inner <- candidates$inner[i]
    
    # Base score is efficiency
    score <- candidates$efficiency[i] * 100
    
    # Bonus for good parallelization
    if (outer >= 5 && outer <= 20) {
      score <- score + 10
    }
    
    # Bonus for sufficient inner workers
    if (inner >= 5) {
      score <- score + 5
    }
    
    # Penalty for memory pressure
    mem_required <- outer * memory_per_model_gb
    if (mem_required > system_memory_gb * 0.7) {
      score <- score - 20
    }
    
    # Penalty for too many outer workers relative to models
    if (outer > n_models / 5) {
      score <- score - 10
    }
    
    scores[i] <- score
  }
  
  return(scores)
}


#' Calculate Latency Score
#'
#' @keywords internal
calculate_latency_score <- function(candidates, avg_tuning_time_minutes) {
  
  scores <- numeric(nrow(candidates))
  
  for (i in seq_len(nrow(candidates))) {
    inner <- candidates$inner[i]
    
    # Estimated speedup from inner parallelization
    # Assuming diminishing returns after 10 workers
    speedup <- min(inner / 1.5, 10)
    
    # Expected time reduction
    time_reduction <- 1 - (1 / speedup)
    
    # Score based on time savings
    score <- time_reduction * 100
    
    # Bonus for balanced configuration
    if (candidates$outer[i] >= 2 && candidates$outer[i] <= 10) {
      score <- score + 10
    }
    
    scores[i] <- score
  }
  
  return(scores)
}


#' Calculate Recommended Grid Size
#'
#' @keywords internal
calculate_grid_size <- function(inner_workers) {
  
  # Grid size should provide enough work for inner workers
  # but not be excessive
  if (inner_workers <= 5) {
    return(10)
  } else if (inner_workers <= 10) {
    return(15)
  } else if (inner_workers <= 20) {
    return(25)
  } else {
    return(50)
  }
}


#' Calculate Recommended Bayesian Iterations
#'
#' @keywords internal
calculate_bayes_iter <- function(inner_workers) {
  
  # More workers can handle more iterations efficiently
  if (inner_workers <= 5) {
    return(15)
  } else if (inner_workers <= 10) {
    return(20)
  } else if (inner_workers <= 20) {
    return(30)
  } else {
    return(40)
  }
}


#' Validate Nested Configuration
#'
#' @description
#' Validates a nested parallel configuration against system constraints.
#'
#' @keywords internal
validate_nested_config <- function(config, system_info) {
  
  # Check core allocation
  if (config$outer_workers * config$inner_workers > config$total_cores) {
    cli::cli_abort("Configuration exceeds allocated cores")
  }
  
  if (config$total_cores > system_info$cores) {
    cli::cli_warn(
      "Configuration requests {config$total_cores} cores but system has {system_info$cores}"
    )
  }
  
  # Check memory requirements
  min_memory_needed <- config$outer_workers * config$metadata$memory_per_model_gb
  if (min_memory_needed > system_info$memory_gb * 0.8) {
    cli::cli_warn(
      "Configuration may cause memory pressure: needs ~{round(min_memory_needed, 0)} GB, system has {round(system_info$memory_gb, 0)} GB"
    )
  }
  
  # Warn about efficiency
  if (config$efficiency < 0.8) {
    cli::cli_warn(
      "Configuration efficiency is {round(config$efficiency * 100, 1)}% (using {config$outer_workers * config$inner_workers} of {config$total_cores} cores)"
    )
  }
  
  invisible(TRUE)
}


#' Get System Information
#'
#' @description
#' Gathers system information for configuration optimization.
#'
#' @return List with system details
#'
#' @keywords internal
get_system_info <- function() {
  
  info <- list(
    cores = parallel::detectCores(),
    hostname = Sys.info()["nodename"],
    os = Sys.info()["sysname"],
    arch = Sys.info()["machine"]
  )
  
  # Get memory
  if (info$os == "Linux") {
    mem_kb <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE))
    info$memory_gb <- mem_kb / 1048576
  } else if (info$os == "Darwin") {
    mem_bytes <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
    info$memory_gb <- mem_bytes / 1073741824
  } else {
    info$memory_gb <- 8  # Conservative default
  }
  
  # Check for NUMA
  if (info$os == "Linux") {
    numa_nodes <- as.integer(system("lscpu | grep 'NUMA node(s):' | awk '{print $NF}'", intern = TRUE, ignore.stderr = TRUE))
    info$numa_nodes <- ifelse(length(numa_nodes) > 0, numa_nodes, 1)
  } else {
    info$numa_nodes <- 1
  }
  
  return(info)
}


#' Print Nested Parallel Configuration
#'
#' @param x A nested_parallel_config object
#' @param ... Additional arguments (unused)
#'
#' @export
print.nested_parallel_config <- function(x, ...) {
  
  cli::cli_h2("Nested Parallelization Configuration")
  
  cli::cli_h3("Core Allocation")
  cli::cli_alert_info("Total cores: {x$total_cores}")
  cli::cli_alert_info("Outer workers: {x$outer_workers} (concurrent models)")
  cli::cli_alert_info("Inner workers: {x$inner_workers} (per model)")
  cli::cli_alert_success("Parallelization: {x$outer_workers} × {x$inner_workers} = {x$outer_workers * x$inner_workers} cores")
  cli::cli_alert_info("Efficiency: {round(x$efficiency * 100, 1)}%")
  
  cli::cli_h3("Strategy")
  cli::cli_alert_info("Optimization: {x$metadata$optimization_goal}")
  cli::cli_alert_info("Strategy: {x$strategy}")
  
  if (!is.null(x$expected_throughput_factor)) {
    cli::cli_alert_info("Expected throughput factor: {round(x$expected_throughput_factor, 2)}x")
  }
  
  if (!is.null(x$expected_speedup)) {
    cli::cli_alert_info("Expected speedup per model: {round(x$expected_speedup, 1)}x")
  }
  
  cli::cli_h3("Tuning Recommendations")
  cli::cli_alert_info("Grid size: {x$tuning$recommended_grid_size}")
  cli::cli_alert_info("Bayesian iterations: {x$tuning$recommended_bayes_iter}")
  cli::cli_alert_info("CV folds: {x$tuning$recommended_cv_folds}")
  
  cli::cli_h3("Memory")
  cli::cli_alert_info("Memory per model: {x$metadata$memory_per_model_gb} GB")
  cli::cli_alert_info("Total memory needed: ~{round(x$outer_workers * x$metadata$memory_per_model_gb, 0)} GB")
  cli::cli_alert_info("System memory: {round(x$metadata$system_memory_gb, 0)} GB")
  
  invisible(x)
}