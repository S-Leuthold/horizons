#' Nested Parallelization Performance Monitoring
#'
#' @description
#' Provides real-time monitoring of both system resources and model progress
#' for nested parallel execution. Includes two separate monitoring functions
#' as requested: system monitoring and model progress monitoring.
#'
#' @name nested_monitoring
NULL


#' Monitor System Resources
#'
#' @description
#' Real-time monitoring of system resources including CPU, memory, load,
#' and process information during nested parallel execution.
#'
#' @param output_dir Output directory containing performance metrics
#' @param refresh_seconds Refresh interval in seconds (default: 5)
#' @param show_processes Show individual process details (default: TRUE)
#' @param max_iterations Maximum iterations before stopping (NULL for infinite)
#'
#' @export
monitor_system_resources <- function(output_dir,
                                    refresh_seconds = 5,
                                    show_processes = TRUE,
                                    max_iterations = NULL) {
  
  if (!dir.exists(output_dir)) {
    cli::cli_abort("Output directory not found: {output_dir}")
  }
  
  metrics_file <- file.path(output_dir, "performance_metrics.json")
  iteration <- 0
  
  cli::cli_alert_info("Starting system resource monitor. Press Ctrl+C to stop.")
  cli::cli_alert_info("Reading from: {metrics_file}")
  
  repeat {
    
    iteration <- iteration + 1
    
    # Clear console
    cat("\014")
    
    cli::cli_h1("System Resource Monitor")
    cli::cli_text("Time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
    cli::cli_text("Iteration: {iteration}")
    
    # Get current system stats
    system_stats <- get_current_system_stats()
    
    # Display system information
    cli::cli_h2("System Overview")
    cli::cli_alert_info("Hostname: {system_stats$hostname}")
    cli::cli_alert_info("CPU Cores: {system_stats$cpu_cores}")
    cli::cli_alert_info("Total Memory: {round(system_stats$total_memory_gb, 1)} GB")
    
    # CPU metrics
    cli::cli_h2("CPU Usage")
    cli::cli_alert_info("Load Average (1m): {system_stats$load_1m}")
    cli::cli_alert_info("Load Average (5m): {system_stats$load_5m}")
    cli::cli_alert_info("Load Average (15m): {system_stats$load_15m}")
    
    load_percent <- (system_stats$load_1m / system_stats$cpu_cores) * 100
    
    if (load_percent > 100) {
      cli::cli_alert_danger("CPU Load: {round(load_percent, 1)}% - OVERLOADED")
    } else if (load_percent > 80) {
      cli::cli_alert_warning("CPU Load: {round(load_percent, 1)}% - High")
    } else {
      cli::cli_alert_success("CPU Load: {round(load_percent, 1)}%")
    }
    
    # Memory metrics
    cli::cli_h2("Memory Usage")
    cli::cli_alert_info("Used: {round(system_stats$used_memory_gb, 1)} GB")
    cli::cli_alert_info("Free: {round(system_stats$free_memory_gb, 1)} GB")
    cli::cli_alert_info("Available: {round(system_stats$available_memory_gb, 1)} GB")
    
    mem_percent <- (system_stats$used_memory_gb / system_stats$total_memory_gb) * 100
    
    if (mem_percent > 90) {
      cli::cli_alert_danger("Memory Usage: {round(mem_percent, 1)}% - CRITICAL")
    } else if (mem_percent > 80) {
      cli::cli_alert_warning("Memory Usage: {round(mem_percent, 1)}% - High")
    } else {
      cli::cli_alert_success("Memory Usage: {round(mem_percent, 1)}%")
    }
    
    # Process information
    if (show_processes) {
      cli::cli_h2("R Processes")
      
      r_processes <- get_r_processes()
      
      if (nrow(r_processes) > 0) {
        cli::cli_alert_info("Active R processes: {nrow(r_processes)}")
        
        # Show top 5 by CPU
        top_processes <- r_processes[order(r_processes$cpu_percent, decreasing = TRUE), ]
        top_processes <- head(top_processes, 5)
        
        cli::cli_text("\nTop R Processes by CPU:")
        for (i in seq_len(nrow(top_processes))) {
          p <- top_processes[i, ]
          cli::cli_text(
            "  PID {p$pid}: CPU {round(p$cpu_percent, 1)}%, MEM {round(p$memory_mb / 1024, 1)}GB, Time {p$elapsed_time}"
          )
        }
        
        # Summary statistics
        total_cpu <- sum(r_processes$cpu_percent)
        total_mem_gb <- sum(r_processes$memory_mb) / 1024
        
        cli::cli_text("\nAggregate R Usage:")
        cli::cli_alert_info("Total CPU: {round(total_cpu, 1)}%")
        cli::cli_alert_info("Total Memory: {round(total_mem_gb, 1)} GB")
      } else {
        cli::cli_alert_warning("No R processes found")
      }
    }
    
    # Read metrics file if it exists
    if (file.exists(metrics_file)) {
      metrics <- jsonlite::read_json(metrics_file)
      
      cli::cli_h2("Nested Parallel Status")
      cli::cli_alert_info("Active Models: {metrics$active_models}")
      cli::cli_alert_info("Cores In Use: {metrics$cores_in_use}/{metrics$total_cores}")
      cli::cli_alert_info("Worker Utilization: {metrics$cpu_utilization}%")
    }
    
    # Check for completion
    if (file.exists(file.path(output_dir, ".complete"))) {
      cli::cli_alert_success("\nEvaluation complete!")
      break
    }
    
    # Check iteration limit
    if (!is.null(max_iterations) && iteration >= max_iterations) {
      cli::cli_alert_info("\nReached maximum iterations")
      break
    }
    
    Sys.sleep(refresh_seconds)
  }
}


#' Monitor Model Progress
#'
#' @description
#' Real-time monitoring of model evaluation progress including completion
#' status, performance metrics, and time estimates.
#'
#' @param output_dir Output directory containing evaluation results
#' @param refresh_seconds Refresh interval in seconds (default: 5)
#' @param show_recent Number of recent models to display (default: 10)
#' @param show_performance Show performance metrics (default: TRUE)
#'
#' @export
monitor_model_progress <- function(output_dir,
                                  refresh_seconds = 5,
                                  show_recent = 10,
                                  show_performance = TRUE) {
  
  if (!dir.exists(output_dir)) {
    cli::cli_abort("Output directory not found: {output_dir}")
  }
  
  metrics_file <- file.path(output_dir, "performance_metrics.json")
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  models_dir <- file.path(output_dir, "models")
  
  cli::cli_alert_info("Starting model progress monitor. Press Ctrl+C to stop.")
  
  repeat {
    
    # Clear console
    cat("\014")
    
    cli::cli_h1("Model Progress Monitor")
    cli::cli_text("Time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
    
    # Read current metrics
    if (file.exists(metrics_file)) {
      metrics <- jsonlite::read_json(metrics_file)
      
      # Overall progress
      cli::cli_h2("Overall Progress")
      
      progress_pct <- metrics$progress_percent
      completed <- metrics$completed_models
      total <- metrics$total_models
      
      # Create progress bar
      bar_width <- 50
      filled <- round(bar_width * progress_pct / 100)
      empty <- bar_width - filled
      
      bar <- paste0(
        "[",
        strrep("=", filled),
        strrep("-", empty),
        "]"
      )
      
      cli::cli_text("{bar} {round(progress_pct, 1)}%")
      cli::cli_alert_info("Completed: {completed}/{total} models")
      cli::cli_alert_info("Pending: {metrics$pending_models} models")
      cli::cli_alert_info("Active: {metrics$active_models} models")
      
      # Time estimates
      cli::cli_h2("Time Estimates")
      cli::cli_alert_info("Runtime: {round(metrics$runtime_hours, 2)} hours")
      cli::cli_alert_info("Average per model: {metrics$avg_time_per_model_min} minutes")
      
      if (!is.null(metrics$estimated_remaining_hours)) {
        cli::cli_alert_info("Estimated remaining: {round(metrics$estimated_remaining_hours, 2)} hours")
        
        # Calculate ETA
        eta <- Sys.time() + as.difftime(metrics$estimated_remaining_hours, units = "hours")
        cli::cli_alert_info("Expected completion: {format(eta, '%Y-%m-%d %H:%M')}")
      }
      
      # Performance metrics
      if (show_performance) {
        cli::cli_h2("Performance Metrics")
        cli::cli_alert_info("Models per hour: {metrics$models_per_hour}")
        cli::cli_alert_info("CPU utilization: {metrics$cpu_utilization}%")
        cli::cli_alert_info("Memory usage: {metrics$current_memory_gb}/{metrics$memory_limit_gb} GB ({metrics$memory_percent}%)")
        
        # Warnings
        if (metrics$memory_pressure) {
          cli::cli_alert_warning("Memory pressure detected")
        }
        if (metrics$high_load) {
          cli::cli_alert_warning("High system load detected")
        }
      }
    } else {
      cli::cli_alert_warning("Metrics file not found. Waiting for evaluation to start...")
    }
    
    # Show recent completed models
    if (show_recent > 0 && dir.exists(models_dir)) {
      
      model_files <- list.files(
        models_dir,
        pattern = "\\.qs$",
        full.names = TRUE
      )
      
      if (length(model_files) > 0) {
        # Sort by modification time
        model_files <- model_files[order(file.mtime(model_files), decreasing = TRUE)]
        model_files <- head(model_files, show_recent)
        
        cli::cli_h2("Recent Models")
        
        for (model_file in model_files) {
          tryCatch({
            model_result <- qs::qread(model_file)
            
            if (model_result$status == "success") {
              cli::cli_alert_success(
                "[{model_result$row_index}] {model_result$wflow_id}: RMSE={round(model_result$metrics$rmse, 3)}, R²={round(model_result$metrics$rsq, 3)} ({round(model_result$timing, 1)}s)"
              )
            } else if (model_result$status == "error") {
              cli::cli_alert_danger(
                "[{model_result$row_index}] {model_result$wflow_id}: ERROR - {substr(model_result$error, 1, 50)}"
              )
            }
          }, error = function(e) {
            # Skip if can't read file
          })
        }
      }
    }
    
    # Check for completion
    if (file.exists(file.path(output_dir, ".complete"))) {
      
      cli::cli_h2("Evaluation Complete!")
      
      # Try to load final results
      final_results_file <- list.files(
        output_dir,
        pattern = "_nested_results\\.qs$",
        full.names = TRUE
      )[1]
      
      if (length(final_results_file) > 0 && file.exists(final_results_file)) {
        final_results <- qs::qread(final_results_file)
        
        successful <- sum(final_results$status == "success", na.rm = TRUE)
        failed <- sum(final_results$status == "error", na.rm = TRUE)
        
        cli::cli_alert_success("Successful models: {successful}")
        if (failed > 0) {
          cli::cli_alert_warning("Failed models: {failed}")
        }
        
        # Best model
        if (successful > 0 && "rsq" %in% names(final_results)) {
          best_idx <- which.max(final_results$rsq)
          best_model <- final_results[best_idx, ]
          
          cli::cli_alert_success(
            "Best model: {best_model$wflow_id} (R²={round(best_model$rsq, 4)})"
          )
        }
      }
      
      break
    }
    
    Sys.sleep(refresh_seconds)
  }
}


#' Combined Monitoring Dashboard
#'
#' @description
#' Runs both system and model monitoring in a combined dashboard view.
#'
#' @param output_dir Output directory
#' @param refresh_seconds Refresh interval
#'
#' @export
monitor_nested_evaluation <- function(output_dir, refresh_seconds = 5) {
  
  cli::cli_alert_info("Starting combined monitor. Press Ctrl+C to stop.")
  
  repeat {
    
    # Clear console
    cat("\014")
    
    # Header
    cli::cli_h1("Nested Parallel Evaluation Monitor")
    cli::cli_rule()
    
    # Get both types of info
    system_stats <- get_current_system_stats()
    
    # System section (compact)
    cli::cli_h2("System")
    cli::cli_text(
      "CPU: {round((system_stats$load_1m / system_stats$cpu_cores) * 100, 1)}% | ",
      "Memory: {round(system_stats$used_memory_gb, 1)}/{round(system_stats$total_memory_gb, 1)}GB | ",
      "Load: {system_stats$load_1m}"
    )
    
    # Model progress section
    metrics_file <- file.path(output_dir, "performance_metrics.json")
    
    if (file.exists(metrics_file)) {
      metrics <- jsonlite::read_json(metrics_file)
      
      cli::cli_h2("Progress")
      
      # Progress bar
      bar_width <- 60
      filled <- round(bar_width * metrics$progress_percent / 100)
      bar <- paste0("[", strrep("█", filled), strrep("░", bar_width - filled), "]")
      
      cli::cli_text("{bar} {round(metrics$progress_percent, 1)}%")
      cli::cli_text(
        "Models: {metrics$completed_models}/{metrics$total_models} | ",
        "Active: {metrics$active_models} | ",
        "Rate: {metrics$models_per_hour}/hr"
      )
      
      if (!is.null(metrics$estimated_remaining_hours)) {
        eta <- Sys.time() + as.difftime(metrics$estimated_remaining_hours, units = "hours")
        cli::cli_text("ETA: {format(eta, '%H:%M')} ({round(metrics$estimated_remaining_hours, 1)}h remaining)")
      }
    }
    
    # Check completion
    if (file.exists(file.path(output_dir, ".complete"))) {
      cli::cli_alert_success("\n✓ Evaluation complete!")
      break
    }
    
    Sys.sleep(refresh_seconds)
  }
}


# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Get Current System Statistics
#'
#' @keywords internal
get_current_system_stats <- function() {
  
  stats <- list(
    hostname = Sys.info()["nodename"],
    cpu_cores = parallel::detectCores()
  )
  
  if (.Platform$OS.type == "unix") {
    
    # Load average
    uptime_output <- system("uptime", intern = TRUE)
    load_match <- regexpr("load average[s]?: ([0-9.]+)[, ]+([0-9.]+)[, ]+([0-9.]+)", uptime_output)
    
    if (load_match > 0) {
      loads <- regmatches(uptime_output, regexec("load average[s]?: ([0-9.]+)[, ]+([0-9.]+)[, ]+([0-9.]+)", uptime_output))[[1]]
      stats$load_1m <- as.numeric(loads[2])
      stats$load_5m <- as.numeric(loads[3])
      stats$load_15m <- as.numeric(loads[4])
    } else {
      stats$load_1m <- stats$load_5m <- stats$load_15m <- NA
    }
    
    # Memory (Linux)
    if (Sys.info()["sysname"] == "Linux") {
      mem_info <- system("free -m", intern = TRUE)
      mem_line <- grep("^Mem:", mem_info, value = TRUE)
      
      if (length(mem_line) > 0) {
        mem_values <- as.numeric(strsplit(gsub("\\s+", " ", mem_line), " ")[[1]][2:7])
        stats$total_memory_gb <- mem_values[1] / 1024
        stats$used_memory_gb <- mem_values[2] / 1024
        stats$free_memory_gb <- mem_values[3] / 1024
        stats$available_memory_gb <- mem_values[6] / 1024
      }
    } else {
      # macOS
      stats$total_memory_gb <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE)) / 1073741824
      stats$used_memory_gb <- NA
      stats$free_memory_gb <- NA
      stats$available_memory_gb <- NA
    }
  } else {
    # Windows fallback
    stats$load_1m <- stats$load_5m <- stats$load_15m <- NA
    stats$total_memory_gb <- 8
    stats$used_memory_gb <- NA
    stats$free_memory_gb <- NA
    stats$available_memory_gb <- NA
  }
  
  return(stats)
}


#' Get R Process Information
#'
#' @keywords internal
get_r_processes <- function() {
  
  if (.Platform$OS.type != "unix") {
    return(data.frame())
  }
  
  # Get R processes
  ps_output <- system(
    "ps aux | grep '[R]' | grep -v grep",
    intern = TRUE
  )
  
  if (length(ps_output) == 0) {
    return(data.frame())
  }
  
  # Parse ps output
  processes <- data.frame(
    pid = integer(),
    cpu_percent = numeric(),
    memory_mb = numeric(),
    elapsed_time = character(),
    stringsAsFactors = FALSE
  )
  
  for (line in ps_output) {
    fields <- strsplit(gsub("\\s+", " ", trimws(line)), " ")[[1]]
    
    if (length(fields) >= 11) {
      processes <- rbind(
        processes,
        data.frame(
          pid = as.integer(fields[2]),
          cpu_percent = as.numeric(fields[3]),
          memory_mb = as.numeric(fields[6]) / 1024,  # Convert KB to MB
          elapsed_time = fields[10],
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  return(processes)
}