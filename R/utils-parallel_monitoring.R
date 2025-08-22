#' Real-time Parallel Progress Monitor
#'
#' @description
#' Live dashboard for monitoring parallel model evaluation progress.
#' Updates automatically every few seconds with comprehensive statistics.
#'
#' @param log_file Character. Path to log file
#' @param refresh_seconds Numeric. Seconds between refreshes (default: 5)
#' @param show_recent Integer. Number of recent models to show (default: 5)
#' @export

monitor_parallel_progress <- function(log_file = NULL, 
                                     refresh_seconds = 5,
                                     show_recent = 5) {
  
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  
  if (!file.exists(log_file)) {
    cli::cli_alert_warning("Log file not found: {log_file}")
    cli::cli_alert_info("Will wait for file to be created...")
  }
  
  cli::cli_alert_info("Starting live monitor. Press Ctrl+C to stop.")
  Sys.sleep(2)
  
  last_best_rsq <- 0
  
  repeat {
    # Clear console (works on most terminals)
    cat("\014")  # or system("clear")
    
    cat("╔══════════════════════════════════════════════════════════════════════\n")
    cat("║              PARALLEL MODEL EVALUATION MONITOR                         \n")
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    
    if (!file.exists(log_file)) {
      cat("║ Waiting for log file...                                                \n")
      cat("╚══════════════════════════════════════════════════════════════════════\n")
      Sys.sleep(refresh_seconds)
      next
    }
    
    # Get detailed stats
    stats <- summarize_parallel_progress(log_file, return_details = TRUE)
    
    if (is.null(stats)) {
      cat("║ No data available yet...                                               \n")
      cat("╚══════════════════════════════════════════════════════════════════════\n")
      Sys.sleep(refresh_seconds)
      next
    }
    
    # Progress bar
    if (!is.na(stats$total_models) && stats$total_models > 0) {
      progress_bar <- create_ascii_progress_bar(
        stats$n_completed, 
        stats$total_models,
        width = 40
      )
      # Format progress line with proper padding
      progress_line <- sprintf("Progress: %s %5.1f%%", progress_bar, stats$progress_pct)
      cat(sprintf("║ %-71s \n", progress_line))
    }
    
    # Key metrics
    # Format metrics line with proper padding
    metrics_line <- sprintf("Complete: %4d │ Failed: %3d │ Success Rate: %5.1f%%",
                            stats$n_completed, stats$n_failed, stats$success_rate)
    cat(sprintf("║ %-71s \n", metrics_line))
    
    # Timing
    if (!is.na(stats$elapsed_mins)) {
      elapsed_str <- format_time(stats$elapsed_mins)
      timing_line <- sprintf("Elapsed: %s │ Rate: %4.1f models/min", elapsed_str, stats$rate)
      cat(sprintf("║ %-71s \n", timing_line))
    }
    
    # Best model tracking
    if (!is.na(stats$best_rsq)) {
      if (stats$best_rsq > last_best_rsq) {
        best_line <- sprintf("🏆 NEW BEST R²: %.3f", stats$best_rsq)
        cat(sprintf("║ %-71s \n", best_line))
        last_best_rsq <- stats$best_rsq
      } else {
        best_line <- sprintf("Best R² so far: %.3f", stats$best_rsq)
        cat(sprintf("║ %-71s \n", best_line))
      }
    }
    
    # ETA with confidence interval
    if (!is.na(stats$eta_info$eta_median)) {
      eta_str <- format_time(stats$eta_info$eta_median)
      ci_lower_str <- format_time(stats$eta_info$eta_lower)
      ci_upper_str <- format_time(stats$eta_info$eta_upper)
      
      # Create the ETA line with confidence interval
      eta_line <- sprintf("ETA: %s (95%% CI: %s-%s)", eta_str, ci_lower_str, ci_upper_str)
      cat(sprintf("║ %-71s \n", eta_line))
      
      # Expected completion time
      if (!is.null(stats$start_time)) {
        completion_time <- stats$start_time + (stats$elapsed_mins + stats$eta_info$eta_median) * 60
        completion_line <- sprintf("Expected completion: %s", format(completion_time, "%H:%M"))
        cat(sprintf("║ %-71s \n", completion_line))
      }
    }
    
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    
    # Recent models
    cat("║ Recent Models:                                                         \n")
    
    lines <- readLines(log_file)
    done_lines <- grep("✓ DONE", lines, value = TRUE)
    
    if (length(done_lines) > 0) {
      recent <- tail(done_lines, show_recent)
      for (line in recent) {
        # Extract key info
        model_match <- regmatches(line, regexpr("\\]: .* \\(", line))
        if (length(model_match) > 0) {
          model_info <- gsub("\\]: | \\(", "", model_match)
          model_info <- substr(model_info, 1, 50)  # Truncate if needed
          
          # Extract R²
          rsq_match <- regmatches(line, regexpr("R²=\\d+\\.\\d+", line))
          if (length(rsq_match) > 0) {
            rsq_val <- as.numeric(sub("R²=", "", rsq_match))
            # Format model info line with proper padding
            model_line <- sprintf("%-50s R²=%.3f", model_info, rsq_val)
            cat(sprintf("║   %-69s \n", model_line))
          }
        }
      }
    }
    
    # Worker statistics if available
    worker_stats <- get_worker_statistics(log_file)
    
    if (!is.null(worker_stats) && nrow(worker_stats) > 0) {
      cat("╠══════════════════════════════════════════════════════════════════════\n")
      cat("║ Worker Statistics:                                                     \n")
      
      # Show top 4 workers
      top_workers <- head(worker_stats, 4)
      for (i in 1:nrow(top_workers)) {
        w <- top_workers[i, ]
        fail_rate <- if (w$n_complete + w$n_failed > 0) {
          round(100 * w$n_failed / (w$n_complete + w$n_failed), 1)
        } else { 0 }
        
        worker_line <- sprintf("W%02d: %3d done, %2d fail (%4.1f%%), avg %.1fs",
                              w$worker_id, w$n_complete, w$n_failed, fail_rate, 
                              ifelse(is.na(w$avg_time), 0, w$avg_time))
        cat(sprintf("║   %-69s \n", worker_line))
      }
    }
    
    # Model timing statistics
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    cat("║ Model Timing:                                                          \n")
    
    if (length(stats$metrics_data$model_times) > 0) {
      fast_models <- sum(stats$metrics_data$model_times < 10, na.rm = TRUE)
      medium_models <- sum(stats$metrics_data$model_times >= 10 & stats$metrics_data$model_times < 60, na.rm = TRUE)
      slow_models <- sum(stats$metrics_data$model_times >= 60, na.rm = TRUE)
      
      total_timed <- fast_models + medium_models + slow_models
      if (total_timed > 0) {
        fast_pct <- round(100 * fast_models / total_timed, 1)
        medium_pct <- round(100 * medium_models / total_timed, 1)
        slow_pct <- round(100 * slow_models / total_timed, 1)
        
        fast_line <- sprintf("Fast (<10s): %3d models (%4.1f%%)", fast_models, fast_pct)
        medium_line <- sprintf("Medium (10-60s): %3d models (%4.1f%%)", medium_models, medium_pct)
        slow_line <- sprintf("Slow (>60s): %3d models (%4.1f%%)", slow_models, slow_pct)
        
        cat(sprintf("║   %-69s \n", fast_line))
        cat(sprintf("║   %-69s \n", medium_line))
        cat(sprintf("║   %-69s \n", slow_line))
      }
    }
    
    # Model-specific average times
    if (file.exists(log_file)) {
      # Parse log for model-specific timing
      lines <- readLines(log_file)
      done_lines <- grep("✓ DONE", lines, value = TRUE)
      
      if (length(done_lines) > 0) {
        model_times <- data.frame(model = character(0), time = numeric(0), stringsAsFactors = FALSE)
        
        for (line in done_lines) {
          # Extract model type (first part before underscore)
          model_match <- regmatches(line, regexpr("\\]: [^_]+", line))
          if (length(model_match) > 0) {
            model_type <- sub("\\]: ", "", model_match)
            
            # Extract timing (X.Xs format)
            time_match <- regmatches(line, regexpr("\\d+\\.\\d+s", line))
            if (length(time_match) > 0) {
              time_val <- as.numeric(sub("s", "", time_match))
              model_times <- rbind(model_times, data.frame(model = model_type, time = time_val))
            }
          }
        }
        
        if (nrow(model_times) > 0) {
          cat("╠══════════════════════════════════════════════════════════════════════\n")
          cat("║ Model Times (Average):                                                \n")
          
          # Calculate averages per model
          model_avgs <- aggregate(time ~ model, data = model_times, FUN = function(x) c(mean = mean(x), count = length(x)))
          model_avgs <- data.frame(model = model_avgs$model, 
                                 avg_time = model_avgs$time[, "mean"],
                                 count = model_avgs$time[, "count"])
          
          # Sort by average time (fastest first)
          model_avgs <- model_avgs[order(model_avgs$avg_time), ]
          
          # Display top models (limit to avoid clutter)
          top_models <- head(model_avgs, 6)
          for (i in 1:nrow(top_models)) {
            m <- top_models[i, ]
            model_line <- sprintf("%-12s: %.1fs avg (%d models)", m$model, m$avg_time, m$count)
            cat(sprintf("║   %-69s \n", model_line))
          }
        }
      }
    }
    
    cat("╚══════════════════════════════════════════════════════════════════════\n")
    cat("\n")
    cat(sprintf("Last updated: %s | Refreshing every %d seconds...\n", 
                format(Sys.time(), "%H:%M:%S"), refresh_seconds))
    
    Sys.sleep(refresh_seconds)
  }
}

#' Create ASCII Progress Bar
#'
#' @param current Current value
#' @param total Total value
#' @param width Width of progress bar in characters
#' @return Character string with progress bar

create_ascii_progress_bar <- function(current, total, width = 40) {
  if (is.na(current) || is.na(total) || total <= 0) {
    return(paste(rep("-", width), collapse = ""))
  }
  
  pct <- current / total
  filled <- round(pct * width)
  empty <- width - filled
  
  bar <- paste0(
    "[",
    paste(rep("█", filled), collapse = ""),
    paste(rep("░", empty), collapse = ""),
    "]"
  )
  
  return(bar)
}

#' Format Time Duration
#'
#' @param minutes Numeric minutes
#' @return Formatted string (e.g., "2h 15m" or "45m")

format_time <- function(minutes) {
  if (is.na(minutes)) return("--:--")
  
  hours <- as.integer(floor(minutes / 60))
  mins <- as.integer(round(minutes %% 60))
  
  if (hours > 0) {
    return(sprintf("%dh %dm", hours, mins))
  } else {
    return(sprintf("%dm", mins))
  }
}

#' Get Worker Statistics from Log
#'
#' @param log_file Path to log file
#' @return Data frame with worker statistics

get_worker_statistics <- function(log_file) {
  if (!file.exists(log_file)) return(NULL)
  
  lines <- readLines(log_file)
  
  # Extract worker IDs from log entries [W##]
  worker_pattern <- "\\[W(\\d+)\\]"
  
  # Parse START, DONE, and FAIL entries
  start_lines <- grep("START.*\\[W\\d+\\]", lines, value = TRUE)
  done_lines <- grep("✓ DONE.*\\[W\\d+\\]", lines, value = TRUE)
  fail_lines <- grep("✗ FAIL.*\\[W\\d+\\]", lines, value = TRUE)
  
  # Extract worker IDs and times
  worker_stats <- data.frame()
  
  # Get unique worker IDs
  all_workers <- c()
  
  for (line in c(start_lines, done_lines, fail_lines)) {
    worker_match <- regmatches(line, regexpr(worker_pattern, line))
    if (length(worker_match) > 0) {
      worker_id <- as.numeric(sub("\\[W", "", sub("\\]", "", worker_match)))
      all_workers <- c(all_workers, worker_id)
    }
  }
  
  unique_workers <- unique(all_workers)
  
  if (length(unique_workers) == 0) return(NULL)
  
  # Calculate stats for each worker
  worker_data <- lapply(unique_workers, function(wid) {
    wid_pattern <- sprintf("\\[W%02d\\]", wid)
    
    n_started <- sum(grepl(wid_pattern, start_lines))
    n_complete <- sum(grepl(wid_pattern, done_lines))
    n_failed <- sum(grepl(wid_pattern, fail_lines))
    
    # Extract completion times for this worker
    worker_done_lines <- done_lines[grepl(wid_pattern, done_lines)]
    times <- numeric()
    
    for (line in worker_done_lines) {
      time_match <- regmatches(line, regexpr("\\d+\\.\\d+s", line))
      if (length(time_match) > 0) {
        times <- c(times, as.numeric(sub("s", "", time_match)))
      }
    }
    
    avg_time <- if (length(times) > 0) mean(times) else NA
    
    data.frame(
      worker_id = wid,
      n_started = n_started,
      n_complete = n_complete,
      n_failed = n_failed,
      avg_time = avg_time
    )
  })
  
  worker_stats <- do.call(rbind, worker_data)
  
  # Sort by number completed
  worker_stats <- worker_stats[order(worker_stats$n_complete, decreasing = TRUE), ]
  
  return(worker_stats)
}

#' Track Best Model in Log
#'
#' @description
#' Logs when a new best model is found
#'
#' @param log_file Path to log file
#' @param model_id Model identifier
#' @param rsq R-squared value
#' @param previous_best Previous best R-squared
#' @export

log_new_best_model <- function(log_file, model_id, rsq, previous_best = NULL) {
  
  if (is.null(previous_best) || rsq > previous_best) {
    message <- sprintf("🏆 NEW BEST MODEL: %s (R²=%.3f)", model_id, rsq)
    
    if (!is.null(previous_best)) {
      message <- sprintf("%s - Previous best: %.3f", message, previous_best)
    }
    
    log_parallel_progress(
      message = message,
      log_file = log_file,
      include_timestamp = TRUE
    )
    
    return(TRUE)
  }
  
  return(FALSE)
}