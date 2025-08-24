#' Resource Management for Nested Parallelization
#'
#' @description
#' Provides resource tracking, allocation, and monitoring functions for
#' nested parallel execution. Ensures efficient use of CPU and memory
#' resources while preventing oversubscription.
#'
#' @name resource_management
#' @keywords internal
NULL


#' Initialize Resource Manager
#'
#' @description
#' Creates a resource manager object to track and control resource usage
#' across nested parallel operations.
#'
#' @param total_cores Total CPU cores allocated
#' @param outer_workers Number of outer parallel workers
#' @param inner_workers Number of inner workers per model
#' @param memory_limit_gb Total memory limit in GB
#' @param output_dir Output directory for logs and metrics
#'
#' @return Resource manager object
#'
#' @keywords internal
initialize_resource_manager <- function(total_cores,
                                        outer_workers,
                                        inner_workers,
                                        memory_limit_gb,
                                        output_dir) {
  
  manager <- list(
    # Core allocation
    total_cores = total_cores,
    outer_workers = outer_workers,
    inner_workers = inner_workers,
    cores_in_use = 0,
    
    # Memory tracking
    memory_limit_gb = memory_limit_gb,
    memory_tokens = vector("list", outer_workers),
    current_memory_gb = 0,
    peak_memory_gb = 0,
    
    # Process tracking
    active_models = integer(0),
    model_start_times = list(),
    model_end_times = list(),
    
    # Output paths
    output_dir = output_dir,
    log_dir = file.path(output_dir, "logs"),
    metrics_file = file.path(output_dir, "performance_metrics.json"),
    
    # System info
    system_memory_gb = get_system_memory_gb(),
    system_cores = parallel::detectCores(),
    hostname = Sys.info()["nodename"],
    
    # Timing
    start_time = Sys.time(),
    last_update = Sys.time()
  )
  
  # Initialize memory tokens
  for (i in seq_len(outer_workers)) {
    manager$memory_tokens[[i]] <- list(
      worker_id = i,
      allocated = FALSE,
      model_id = NA,
      memory_gb = 0,
      peak_memory_gb = 0,
      start_time = NA
    )
  }
  
  # Create directories
  fs::dir_create(manager$log_dir, recurse = TRUE)
  
  # Set monitoring flag
  options(horizons.resource.monitor = TRUE)
  
  # Write initial metrics
  write_initial_metrics(manager)
  
  class(manager) <- c("resource_manager", "list")
  return(manager)
}


#' Check Resource Availability
#'
#' @description
#' Verifies sufficient resources are available before starting a batch.
#'
#' @param resource_manager Resource manager object
#' @param n_models Number of models in the batch
#'
#' @keywords internal
check_resource_availability <- function(resource_manager, n_models) {
  
  # Update current memory usage
  current_mem <- get_current_memory_gb()
  resource_manager$current_memory_gb <- current_mem
  
  # Check memory availability
  available_memory <- resource_manager$memory_limit_gb - current_mem
  required_memory <- n_models * (resource_manager$memory_limit_gb / resource_manager$outer_workers)
  
  if (required_memory > available_memory * 0.9) {
    cli::cli_alert_warning(
      "Memory pressure: {round(current_mem, 1)}/{round(resource_manager$memory_limit_gb, 1)} GB used"
    )
    
    # Trigger garbage collection
    gc(verbose = FALSE, full = TRUE, reset = TRUE)
    
    # Recheck
    current_mem <- get_current_memory_gb()
    available_memory <- resource_manager$memory_limit_gb - current_mem
    
    if (required_memory > available_memory * 0.95) {
      cli::cli_abort(
        "Insufficient memory for batch. Need {round(required_memory, 1)} GB, have {round(available_memory, 1)} GB"
      )
    }
  }
  
  # Check CPU load
  load_avg <- get_load_average()
  
  if (load_avg > resource_manager$total_cores * 1.5) {
    cli::cli_alert_warning("High system load: {round(load_avg, 1)}")
    
    # Brief pause to let system stabilize
    Sys.sleep(2)
  }
  
  # Check for zombie processes
  check_zombie_processes(resource_manager)
  
  invisible(TRUE)
}


#' Acquire Resource Token
#'
#' @description
#' Acquires a resource token for a model evaluation, tracking memory allocation.
#'
#' @param resource_manager Resource manager object
#' @param model_id Model identifier
#'
#' @return Resource token object
#'
#' @keywords internal
acquire_resource_token <- function(resource_manager, model_id) {
  
  # Find available token slot
  token_id <- NULL
  for (i in seq_along(resource_manager$memory_tokens)) {
    if (!resource_manager$memory_tokens[[i]]$allocated) {
      token_id <- i
      break
    }
  }
  
  if (is.null(token_id)) {
    # Wait for a token to become available
    Sys.sleep(1)
    return(acquire_resource_token(resource_manager, model_id))
  }
  
  # Allocate token
  token <- list(
    token_id = token_id,
    model_id = model_id,
    start_time = Sys.time(),
    start_memory_gb = get_current_memory_gb(),
    peak_memory_gb = 0
  )
  
  # Update manager
  resource_manager$memory_tokens[[token_id]]$allocated <- TRUE
  resource_manager$memory_tokens[[token_id]]$model_id <- model_id
  resource_manager$memory_tokens[[token_id]]$start_time <- token$start_time
  
  # Track active model
  resource_manager$active_models <- c(resource_manager$active_models, model_id)
  resource_manager$model_start_times[[as.character(model_id)]] <- token$start_time
  
  return(token)
}


#' Release Resource Token
#'
#' @description
#' Releases a resource token after model evaluation completes.
#'
#' @param resource_manager Resource manager object
#' @param token Resource token to release
#'
#' @keywords internal
release_resource_token <- function(resource_manager, token) {
  
  if (is.null(token) || is.null(token$token_id)) {
    return(invisible(NULL))
  }
  
  # Calculate memory usage
  end_memory_gb <- get_current_memory_gb()
  peak_memory_gb <- max(token$peak_memory_gb, end_memory_gb - token$start_memory_gb)
  
  # Update token
  resource_manager$memory_tokens[[token$token_id]]$allocated <- FALSE
  resource_manager$memory_tokens[[token$token_id]]$model_id <- NA
  resource_manager$memory_tokens[[token$token_id]]$peak_memory_gb <- peak_memory_gb
  
  # Update active models
  resource_manager$active_models <- setdiff(
    resource_manager$active_models,
    token$model_id
  )
  
  # Record end time
  resource_manager$model_end_times[[as.character(token$model_id)]] <- Sys.time()
  
  # Update peak memory if needed
  if (peak_memory_gb > resource_manager$peak_memory_gb) {
    resource_manager$peak_memory_gb <- peak_memory_gb
  }
  
  invisible(NULL)
}


#' Get Peak Memory Usage
#'
#' @description
#' Returns the peak memory usage for a resource token.
#'
#' @param token Resource token
#'
#' @return Peak memory in GB
#'
#' @keywords internal
get_peak_memory <- function(token) {
  
  if (is.null(token)) {
    return(NA_real_)
  }
  
  current_mem <- get_current_memory_gb()
  peak_mem <- max(token$peak_memory_gb, current_mem - token$start_memory_gb)
  
  return(peak_mem)
}


#' Update Performance Metrics
#'
#' @description
#' Updates and writes performance metrics to file.
#'
#' @param resource_manager Resource manager object
#' @param completed Number of completed models
#' @param total Total number of models
#'
#' @keywords internal
update_performance_metrics <- function(resource_manager, completed, total) {
  
  current_time <- Sys.time()
  runtime_hours <- as.numeric(difftime(current_time, resource_manager$start_time, units = "hours"))
  
  metrics <- list(
    timestamp = current_time,
    hostname = resource_manager$hostname,
    total_models = total,
    completed_models = completed,
    pending_models = total - completed,
    active_models = length(resource_manager$active_models),
    progress_percent = round((completed / total) * 100, 2),
    
    # Resource usage
    total_cores = resource_manager$total_cores,
    outer_workers = resource_manager$outer_workers,
    inner_workers = resource_manager$inner_workers,
    cores_in_use = length(resource_manager$active_models) * resource_manager$inner_workers,
    cpu_utilization = round((length(resource_manager$active_models) * resource_manager$inner_workers) / resource_manager$total_cores * 100, 1),
    
    # Memory
    current_memory_gb = round(get_current_memory_gb(), 2),
    memory_limit_gb = round(resource_manager$memory_limit_gb, 2),
    peak_memory_gb = round(resource_manager$peak_memory_gb, 2),
    memory_percent = round((get_current_memory_gb() / resource_manager$memory_limit_gb) * 100, 1),
    
    # Performance
    runtime_hours = round(runtime_hours, 3),
    models_per_hour = if (runtime_hours > 0) round(completed / runtime_hours, 2) else 0,
    avg_time_per_model_min = if (completed > 0) round((runtime_hours * 60) / completed, 2) else NA,
    estimated_remaining_hours = if (completed > 0) round(((total - completed) / completed) * runtime_hours, 2) else NA,
    
    # System
    load_average = get_load_average(),
    memory_pressure = get_current_memory_gb() > resource_manager$memory_limit_gb * 0.8,
    high_load = get_load_average() > resource_manager$total_cores * 1.2
  )
  
  # Write to JSON file
  jsonlite::write_json(
    metrics,
    resource_manager$metrics_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  # Update last update time
  resource_manager$last_update <- current_time
  
  invisible(metrics)
}


#' Cleanup Resource Manager
#'
#' @description
#' Performs cleanup when resource manager is no longer needed.
#'
#' @param resource_manager Resource manager object
#'
#' @keywords internal
cleanup_resource_manager <- function(resource_manager) {
  
  # Write final metrics
  if (!is.null(resource_manager)) {
    final_metrics <- list(
      end_time = Sys.time(),
      total_runtime_hours = as.numeric(
        difftime(Sys.time(), resource_manager$start_time, units = "hours")
      ),
      peak_memory_gb = resource_manager$peak_memory_gb,
      status = "completed"
    )
    
    # Append to metrics file
    metrics_file <- file.path(resource_manager$output_dir, "final_metrics.json")
    jsonlite::write_json(
      final_metrics,
      metrics_file,
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }
  
  # Reset options
  options(horizons.resource.monitor = FALSE)
  
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# System Information Functions
# -----------------------------------------------------------------------------

#' Get System Memory in GB
#'
#' @keywords internal
get_system_memory_gb <- function() {
  
  if (.Platform$OS.type == "unix") {
    # Linux/Mac
    if (Sys.info()["sysname"] == "Linux") {
      mem_kb <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE))
      return(mem_kb / 1048576)  # Convert KB to GB
    } else if (Sys.info()["sysname"] == "Darwin") {
      # macOS
      mem_bytes <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
      return(mem_bytes / 1073741824)  # Convert bytes to GB
    }
  }
  
  # Fallback
  return(8)  # Conservative default
}


#' Get Current Memory Usage in GB
#'
#' @keywords internal
get_current_memory_gb <- function() {
  
  # Try pryr if available
  if (requireNamespace("pryr", quietly = TRUE)) {
    return(as.numeric(pryr::mem_used()) / 1073741824)
  }
  
  # Try gc() approach
  gc_info <- gc()
  used_mb <- sum(gc_info[, "used"])
  return(used_mb / 1024)
}


#' Get System Load Average
#'
#' @keywords internal
get_load_average <- function() {
  
  if (.Platform$OS.type == "unix") {
    load_str <- system("uptime | awk -F'load average:' '{print $2}'", intern = TRUE)
    loads <- as.numeric(strsplit(gsub(" ", "", load_str), ",")[[1]])
    return(loads[1])  # 1-minute average
  }
  
  return(NA_real_)
}


#' Check for Zombie Processes
#'
#' @keywords internal
check_zombie_processes <- function(resource_manager) {
  
  if (.Platform$OS.type == "unix") {
    # Check for zombie R processes
    zombie_count <- as.integer(system(
      "ps aux | grep '[Rr]' | grep '<defunct>' | wc -l",
      intern = TRUE
    ))
    
    if (zombie_count > 0) {
      cli::cli_alert_warning("Found {zombie_count} zombie R processes")
    }
  }
  
  invisible(NULL)
}


#' Write Initial Metrics
#'
#' @keywords internal
write_initial_metrics <- function(resource_manager) {
  
  initial_metrics <- list(
    start_time = resource_manager$start_time,
    hostname = resource_manager$hostname,
    total_cores = resource_manager$total_cores,
    system_cores = resource_manager$system_cores,
    outer_workers = resource_manager$outer_workers,
    inner_workers = resource_manager$inner_workers,
    memory_limit_gb = resource_manager$memory_limit_gb,
    system_memory_gb = resource_manager$system_memory_gb,
    output_dir = resource_manager$output_dir,
    status = "started"
  )
  
  jsonlite::write_json(
    initial_metrics,
    file.path(resource_manager$output_dir, "initial_metrics.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  invisible(initial_metrics)
}