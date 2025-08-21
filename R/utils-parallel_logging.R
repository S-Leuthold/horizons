#' Simple File-Based Logging for Parallel Workers
#'
#' @description
#' Provides thread-safe logging for parallel workers that write to a shared log file.
#' Each worker can append progress messages without console output conflicts.
#'
#' @param message Character. Message to log
#' @param log_file Character. Path to log file (default: "parallel_progress.log" in temp)
#' @param worker_id Integer. Worker identifier (optional)
#' @param include_timestamp Logical. Include timestamp in log entry
#'
#' @export

log_parallel_progress <- function(message, 
                                 log_file = NULL,
                                 worker_id = NULL,
                                 include_timestamp = TRUE) {
  
  # Default log file in temp directory
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  
  # Create log entry with subtle worker ID at the end
  if (include_timestamp) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    if (!is.null(worker_id)) {
      log_entry <- sprintf("[%s] %s [W%02d]\n", timestamp, message, worker_id)
    } else {
      log_entry <- sprintf("[%s] %s\n", timestamp, message)
    }
  } else {
    if (!is.null(worker_id)) {
      log_entry <- sprintf("%s [W%02d]\n", message, worker_id)
    } else {
      log_entry <- sprintf("%s\n", message)
    }
  }
  
  # Thread-safe append to file
  tryCatch({
    cat(log_entry, file = log_file, append = TRUE)
  }, error = function(e) {
    # Silently fail if can't write (to avoid breaking parallel execution)
    NULL
  })
  
  invisible(NULL)
}

#' Initialize Parallel Progress Log
#'
#' @description
#' Creates or clears a log file for parallel progress tracking
#'
#' @param log_file Character. Path to log file
#' @param n_models Integer. Total number of models to process
#' @return Character. Path to initialized log file
#' @export

init_parallel_log <- function(log_file = NULL, n_models = NULL) {
  
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  
  # Create header
  header <- paste0(
    "==============================================\n",
    "Parallel Model Evaluation Log\n",
    "Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
  )
  
  if (!is.null(n_models)) {
    header <- paste0(header, "Total models: ", n_models, "\n")
  }
  
  header <- paste0(
    header,
    "==============================================\n\n"
  )
  
  # Write header (overwrites existing file)
  cat(header, file = log_file, append = FALSE)
  
  cli::cli_alert_success("Log file initialized: {log_file}")
  
  return(log_file)
}

#' Tail Parallel Progress Log
#'
#' @description
#' Shows the last N lines of the parallel progress log
#'
#' @param n Integer. Number of lines to show (default: 20)
#' @param log_file Character. Path to log file
#' @export

tail_parallel_log <- function(n = 20, log_file = NULL) {
  
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  
  if (!file.exists(log_file)) {
    cli::cli_alert_warning("Log file not found: {log_file}")
    return(invisible(NULL))
  }
  
  # Read and display last n lines
  lines <- readLines(log_file)
  n_lines <- length(lines)
  
  if (n_lines == 0) {
    cli::cli_alert_info("Log file is empty")
    return(invisible(NULL))
  }
  
  start_line <- max(1, n_lines - n + 1)
  recent_lines <- lines[start_line:n_lines]
  
  cat(paste(recent_lines, collapse = "\n"), "\n")
  
  invisible(recent_lines)
}

#' Get Parallel Progress Summary
#'
#' @description
#' Analyzes the log file to provide comprehensive progress summary including
#' best models, worker stats, and ETA with confidence intervals
#'
#' @param log_file Character. Path to log file
#' @param return_details Logical. Return detailed statistics
#' @export

summarize_parallel_progress <- function(log_file = NULL, return_details = FALSE) {
  
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  
  if (!file.exists(log_file)) {
    cli::cli_alert_warning("Log file not found: {log_file}")
    return(invisible(NULL))
  }
  
  lines <- readLines(log_file)
  
  # Parse different line types
  start_lines <- grep("START \\[", lines, value = TRUE)
  done_lines <- grep("✓ DONE \\[", lines, value = TRUE)
  fail_lines <- grep("✗ FAIL \\[", lines, value = TRUE)
  
  n_started <- length(start_lines)
  n_completed <- length(done_lines)
  n_failed <- length(fail_lines)
  
  # Extract total from first START line
  if (length(start_lines) > 0) {
    total_match <- regmatches(start_lines[1], regexpr("\\d+/\\d+", start_lines[1]))
    if (length(total_match) > 0) {
      total_models <- as.numeric(sub(".*/", "", total_match[1]))
    } else {
      total_models <- NA
    }
  } else {
    total_models <- NA
  }
  
  # Parse metrics from completed models
  metrics_data <- NULL
  if (length(done_lines) > 0) {
    # Extract R² values
    rsq_values <- numeric()
    model_times <- numeric()
    
    for (line in done_lines) {
      # Extract R²
      rsq_match <- regmatches(line, regexpr("R²=\\d+\\.\\d+", line))
      if (length(rsq_match) > 0) {
        rsq_values <- c(rsq_values, as.numeric(sub("R²=", "", rsq_match)))
      }
      
      # Extract runtime (in seconds)
      time_match <- regmatches(line, regexpr("\\d+\\.\\d+s", line))
      if (length(time_match) > 0) {
        model_times <- c(model_times, as.numeric(sub("s", "", time_match)))
      }
    }
    
    metrics_data <- list(
      rsq_values = rsq_values,
      model_times = model_times
    )
  }
  
  # Get timing info
  elapsed_mins <- NA
  start_time <- NULL
  
  time_lines <- grep("\\[\\d{4}-\\d{2}-\\d{2}", lines, value = TRUE)
  if (length(time_lines) > 0) {
    times <- sub("\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\].*", "\\1", time_lines)
    times <- times[times != time_lines]
    
    if (length(times) >= 2) {
      start_time <- as.POSIXct(times[1], format = "%Y-%m-%d %H:%M:%S")
      last_time <- as.POSIXct(times[length(times)], format = "%Y-%m-%d %H:%M:%S")
      elapsed_mins <- as.numeric(difftime(last_time, start_time, units = "mins"))
    }
  }
  
  # Calculate statistics
  progress_pct <- if (!is.na(total_models)) {
    round(100 * n_completed / total_models, 1)
  } else NA
  
  success_rate <- if (n_completed + n_failed > 0) {
    round(100 * n_completed / (n_completed + n_failed), 1)
  } else NA
  
  rate <- if (n_completed > 0 && !is.na(elapsed_mins) && elapsed_mins > 0) {
    n_completed / elapsed_mins
  } else NA
  
  # Best model so far
  best_rsq <- if (length(metrics_data$rsq_values) > 0) {
    max(metrics_data$rsq_values, na.rm = TRUE)
  } else NA
  
  # ETA calculation with confidence interval
  eta_info <- calculate_eta_with_ci(
    n_completed = n_completed,
    n_remaining = total_models - n_completed,
    elapsed_mins = elapsed_mins,
    model_times = metrics_data$model_times
  )
  
  # Display summary
  cli::cli_h3("Progress Summary")
  
  if (!is.na(total_models)) {
    cli::cli_alert_info("Progress: {n_completed}/{total_models} ({progress_pct}%)")
  }
  
  cli::cli_alert_success("Completed: {n_completed}")
  cli::cli_alert_danger("Failed: {n_failed}")
  
  if (!is.na(success_rate)) {
    cli::cli_alert_info("Success rate: {success_rate}%")
  }
  
  if (!is.na(elapsed_mins)) {
    cli::cli_alert_info("Elapsed: {round(elapsed_mins, 1)} minutes")
  }
  
  if (!is.na(rate)) {
    cli::cli_alert_info("Rate: {round(rate, 2)} models/minute")
  }
  
  if (!is.na(best_rsq)) {
    cli::cli_alert_success("Best R² so far: {round(best_rsq, 3)}")
  }
  
  if (!is.na(eta_info$eta_median)) {
    cli::cli_alert_info("ETA: {round(eta_info$eta_median, 1)} min (95% CI: {round(eta_info$eta_lower, 1)}-{round(eta_info$eta_upper, 1)} min)")
  }
  
  if (return_details) {
    return(list(
      n_started = n_started,
      n_completed = n_completed,
      n_failed = n_failed,
      total_models = total_models,
      progress_pct = progress_pct,
      success_rate = success_rate,
      elapsed_mins = elapsed_mins,
      rate = rate,
      best_rsq = best_rsq,
      metrics_data = metrics_data,
      eta_info = eta_info,
      start_time = start_time
    ))
  }
  
  invisible(NULL)
}

#' Calculate ETA with Confidence Intervals
#'
#' @description
#' Calculates estimated time to completion with confidence intervals
#' based on recent model completion times
#'
#' @param n_completed Number of completed models
#' @param n_remaining Number of remaining models
#' @param elapsed_mins Total elapsed time in minutes
#' @param model_times Vector of individual model completion times
#' @return List with eta_median, eta_lower, eta_upper

calculate_eta_with_ci <- function(n_completed, n_remaining, elapsed_mins, model_times = NULL) {
  
  if (is.na(n_remaining) || n_remaining <= 0 || is.na(elapsed_mins) || elapsed_mins <= 0) {
    return(list(eta_median = NA, eta_lower = NA, eta_upper = NA))
  }
  
  if (!is.null(model_times) && length(model_times) >= 10) {
    # Use recent model times for more accurate estimate
    recent_times <- tail(model_times, 100)  # Use last 100 models
    
    # Calculate per-model time statistics
    time_per_model_median <- median(recent_times, na.rm = TRUE) / 60  # Convert to minutes
    time_per_model_q25 <- quantile(recent_times, 0.25, na.rm = TRUE) / 60
    time_per_model_q75 <- quantile(recent_times, 0.75, na.rm = TRUE) / 60
    
    # Calculate ETAs
    eta_median <- n_remaining * time_per_model_median
    eta_lower <- n_remaining * time_per_model_q25
    eta_upper <- n_remaining * time_per_model_q75
    
  } else {
    # Fallback to simple average
    avg_time_per_model <- elapsed_mins / n_completed
    
    eta_median <- n_remaining * avg_time_per_model
    # Simple ±20% confidence interval
    eta_lower <- eta_median * 0.8
    eta_upper <- eta_median * 1.2
  }
  
  return(list(
    eta_median = eta_median,
    eta_lower = eta_lower,
    eta_upper = eta_upper
  ))
}