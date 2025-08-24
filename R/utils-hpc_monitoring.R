#' Enhanced HPC Monitoring for Horizons Parallel Evaluation
#'
#' @description
#' Real-time monitoring utilities for HPC parallel model evaluation runs.
#' Reads checkpoint files and logs to provide comprehensive progress tracking,
#' performance metrics, and worker analytics.
#'
#' @section Usage:
#' ```r
#' # Monitor from another terminal while models are running:
#' horizons::monitor_hpc_evaluation(
#'   output_dir = "/scratch/samleuth/horizons/run_20250823",
#'   refresh_seconds = 2
#' )
#' ```
#'
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#' @importFrom fs path file_exists dir_exists
#' @importFrom qs qread
#' @importFrom stats median sd quantile
#' @importFrom utils tail head
#' @name hpc_monitoring
NULL


#' Monitor HPC Model Evaluation Progress
#'
#' @description
#' Live dashboard for monitoring HPC parallel evaluation with enhanced metrics
#' including worker performance, memory tracking, and detailed timing statistics.
#'
#' @param output_dir Character. Path to output directory from run_hpc_evaluation
#' @param refresh_seconds Numeric. Seconds between refreshes (default = 5)
#' @param show_recent Integer. Number of recent models to display (default = 5)
#' @param show_workers Logical. Display worker statistics (default = TRUE)
#'
#' @export

monitor_hpc_evaluation <- function(output_dir,
                                   refresh_seconds = 5,
                                   show_recent     = 5,
                                   show_workers    = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Validate Directory
  ## ---------------------------------------------------------------------------

  if (!fs::dir_exists(output_dir)) {
    cli::cli_alert_danger("Output directory not found: {output_dir}")
    return(invisible(NULL))
  }

  checkpoint_dir <- fs::path(output_dir, "checkpoints")
  
  cli::cli_alert_info("Starting HPC evaluation monitor for: {output_dir}")
  cli::cli_alert_info("Press Ctrl+C to stop monitoring")
  Sys.sleep(1)

  ## Initialize tracking variables ---------------------------------------------
  
  last_best_rsq    <- -Inf
  monitor_start    <- Sys.time()
  worker_stats     <- list()

  ## ---------------------------------------------------------------------------
  ## Main Monitoring Loop
  ## ---------------------------------------------------------------------------

  repeat {
    
    ## Clear console -----------------------------------------------------------
    
    cat("\014")
    
    ## -------------------------------------------------------------------------
    ## Step 1: Read Checkpoint Summary
    ## -------------------------------------------------------------------------
    
    checkpoint_summary <- get_checkpoint_summary(checkpoint_dir)
    n_completed        <- checkpoint_summary$n_completed
    completed_models   <- checkpoint_summary$completed_ids
    
    ## -------------------------------------------------------------------------
    ## Step 2: Read Model Results from Batches
    ## -------------------------------------------------------------------------
    
    model_results <- list()
    timing_data   <- numeric()
    memory_data   <- numeric()
    worker_timing <- list()
    
    if (n_completed > 0) {
      ## Read all results efficiently from batched files ----------------------
      
      all_results <- read_checkpoint_results(checkpoint_dir)
      
      for (idx_str in names(all_results)) {
        result <- all_results[[idx_str]]
        
        if (!is.null(result)) {
          model_results[[length(model_results) + 1]] <- result
          
          ## Collect timing data -----------------------------------------------
          
          if (!is.null(result$timing_info$total_seconds)) {
            timing_data <- c(timing_data, result$timing_info$total_seconds)
          }
          
          ## Collect memory data -----------------------------------------------
          
          if (!is.null(result$memory_info$peak_gb)) {
            memory_data <- c(memory_data, result$memory_info$peak_gb)
          }
        }
      }
    }
    
    ## -------------------------------------------------------------------------
    ## Step 3: Parse Results
    ## -------------------------------------------------------------------------
    
    n_success <- 0
    n_failed  <- 0
    n_pruned  <- 0
    recent_models <- list()
    best_model    <- NULL
    best_rsq      <- -Inf
    
    if (length(model_results) > 0) {
      for (result in model_results) {
        status <- result$status_summary$status[1]
        
        if (status == "success") {
          n_success <- n_success + 1
          
          ## Track successful models for recent display ------------------------
          
          if (!is.null(result$status_summary$rsq)) {
            model_info <- list(
              wflow_id = result$status_summary$wflow_id[1],
              rsq      = result$status_summary$rsq[1],
              rmse     = result$status_summary$rmse[1]
            )
            recent_models[[length(recent_models) + 1]] <- model_info
            
            ## Track best model ------------------------------------------------
            
            if (model_info$rsq > best_rsq) {
              best_rsq   <- model_info$rsq
              best_model <- model_info
            }
          }
        } else if (status == "error") {
          n_failed <- n_failed + 1
        } else if (status == "pruned") {
          n_pruned <- n_pruned + 1
        }
      }
    }
    
    ## -------------------------------------------------------------------------
    ## Step 4: Read Worker Logs (for worker statistics)
    ## -------------------------------------------------------------------------
    
    log_files <- list.files(output_dir, pattern = "^parallel_.*\\.log$", full.names = TRUE)
    worker_data <- list()
    
    if (length(log_files) > 0 && show_workers) {
      for (log_file in log_files) {
        ## Extract worker ID from filename -------------------------------------
        
        worker_id <- gsub(".*parallel_(\\d+)\\.log", "\\1", basename(log_file))
        
        ## Read log lines ------------------------------------------------------
        
        logs <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character(0))
        
        if (length(logs) > 0) {
          ## Count completions -------------------------------------------------
          
          done_lines <- grep("DONE", logs, value = TRUE)
          fail_lines <- grep("FAIL", logs, value = TRUE)
          
          ## Extract timing for this worker ------------------------------------
          
          times <- numeric()
          for (line in done_lines) {
            time_match <- regmatches(line, regexpr("\\d+\\.\\d+s", line))
            if (length(time_match) > 0) {
              times <- c(times, as.numeric(sub("s", "", time_match)))
            }
          }
          
          worker_data[[worker_id]] <- list(
            completed = length(done_lines),
            failed    = length(fail_lines),
            avg_time  = if (length(times) > 0) mean(times) else NA
          )
        }
      }
    }
    
    ## -------------------------------------------------------------------------
    ## Step 5: Get total models from config
    ## -------------------------------------------------------------------------
    
    total_models <- NA
    config_files <- list.files(output_dir, pattern = "config.*\\.qs$", full.names = TRUE)
    
    if (length(config_files) > 0) {
      config <- tryCatch(qs::qread(config_files[1]), error = function(e) NULL)
      if (!is.null(config)) {
        total_models <- nrow(config)
      }
    }
    
    ## -------------------------------------------------------------------------
    ## Step 6: Calculate Statistics
    ## -------------------------------------------------------------------------
    
    ## Progress metrics --------------------------------------------------------
    
    progress_pct <- if (!is.na(total_models) && total_models > 0) {
      round(100 * n_completed / total_models, 1)
    } else {
      NA
    }
    
    success_rate <- if (n_completed > 0) {
      round(100 * n_success / n_completed, 1)
    } else {
      0
    }
    
    ## Timing statistics -------------------------------------------------------
    
    elapsed_mins <- as.numeric(difftime(Sys.time(), monitor_start, units = "mins"))
    current_rate <- if (elapsed_mins > 0) n_completed / elapsed_mins else 0
    
    ## Calculate average rate over time ----------------------------------------
    
    avg_rate <- current_rate  # Simplified for now
    
    ## Detailed timing stats ---------------------------------------------------
    
    if (length(timing_data) > 0) {
      time_median <- median(timing_data)
      time_mean   <- mean(timing_data)
      time_sd     <- sd(timing_data)
      time_q95    <- quantile(timing_data, 0.95)
    } else {
      time_median <- time_mean <- time_sd <- time_q95 <- NA
    }
    
    ## Memory statistics -------------------------------------------------------
    
    if (length(memory_data) > 0) {
      mem_current <- tail(memory_data, 1)
      mem_peak    <- max(memory_data)
      mem_avg     <- mean(memory_data)
    } else {
      mem_current <- mem_peak <- mem_avg <- NA
    }
    
    ## ETA calculation ---------------------------------------------------------
    
    if (!is.na(total_models) && n_completed > 0 && n_completed < total_models) {
      remaining    <- total_models - n_completed
      eta_mins     <- remaining / avg_rate
      eta_lower    <- remaining / (avg_rate * 1.2)  # Optimistic
      eta_upper    <- remaining / (avg_rate * 0.8)  # Pessimistic
      eta_time     <- Sys.time() + (eta_mins * 60)
    } else {
      eta_mins <- eta_lower <- eta_upper <- NA
      eta_time <- NA
    }
    
    ## -------------------------------------------------------------------------
    ## Step 7: Display Dashboard
    ## -------------------------------------------------------------------------
    
    cat("╔══════════════════════════════════════════════════════════════════════\n")
    cat("║              PARALLEL MODEL EVALUATION MONITOR                       \n")
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    
    ## Progress bar ------------------------------------------------------------
    
    if (!is.na(total_models) && total_models > 0) {
      bar_width <- 40
      filled    <- round(bar_width * n_completed / total_models)
      empty     <- bar_width - filled
      
      progress_bar <- paste0(
        "[",
        paste(rep("█", filled), collapse = ""),
        paste(rep("░", empty), collapse = ""),
        "]"
      )
      
      cat(sprintf("║ Progress: %s %5.1f%% \n", progress_bar, progress_pct))
    }
    
    ## Status line -------------------------------------------------------------
    
    cat(sprintf("║ Complete: %4d │ Failed: %3d │ Success Rate: %5.1f%%                 \n",
                n_completed, n_failed, success_rate))
    
    ## Elapsed time and rate ---------------------------------------------------
    
    if (!is.na(elapsed_mins)) {
      hours <- floor(elapsed_mins / 60)
      mins  <- round(elapsed_mins %% 60)
      time_str <- if (hours > 0) sprintf("%dh %dm", hours, mins) else sprintf("%dm", mins)
      
      cat(sprintf("║ Elapsed: %s │ Rate: %4.1f models/min                                  \n",
                  time_str, avg_rate))
    }
    
    ## Best R² -----------------------------------------------------------------
    
    if (!is.null(best_model)) {
      if (best_rsq > last_best_rsq) {
        cat(sprintf("║ NEW BEST R²: %.3f                                                \n", best_rsq))
        last_best_rsq <- best_rsq
      } else {
        cat(sprintf("║ Best R² so far: %.3f                                               \n", best_rsq))
      }
    }
    
    ## Enhanced statistics section ---------------------------------------------
    
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    cat(sprintf("║ Status    │ Success: %3d │ Failed: %3d │ Pruned: %3d │ Rate: %5.1f%%\n",
                n_success, n_failed, n_pruned, success_rate))
    
    cat(sprintf("║ Speed     │ Current: %4.1f models/min │ Average: %4.1f models/min\n",
                current_rate, avg_rate))
    
    if (!is.na(time_median)) {
      cat(sprintf("║ Timing    │ Median: %5.1fs │ Mean: %5.1fs │ StdDev: %5.1fs │ 95%%: %5.1fs\n",
                  time_median, time_mean, time_sd, time_q95))
    }
    
    if (!is.na(mem_current)) {
      cat(sprintf("║ Memory    │ Current: %5.1f GB │ Peak: %5.1f GB │ Average: %5.1f GB\n",
                  mem_current, mem_peak, mem_avg))
    }
    
    ## ETA section -------------------------------------------------------------
    
    if (!is.na(eta_mins)) {
      cat("╠══════════════════════════════════════════════════════════════════════\n")
      
      eta_h <- floor(eta_mins / 60)
      eta_m <- round(eta_mins %% 60)
      
      ci_lower_h <- floor(eta_lower / 60)
      ci_lower_m <- round(eta_lower %% 60)
      ci_upper_h <- floor(eta_upper / 60)
      ci_upper_m <- round(eta_upper %% 60)
      
      cat(sprintf("║ ETA: %s (%dh %dm remaining)\n",
                  format(eta_time, "%Y-%m-%d %H:%M"), eta_h, eta_m))
      cat(sprintf("║ 95%% CI: [%dh %dm - %dh %dm]\n",
                  ci_lower_h, ci_lower_m, ci_upper_h, ci_upper_m))
    }
    
    ## Recent models section ---------------------------------------------------
    
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    cat("║ Recent Models:                                                         \n")
    
    if (length(recent_models) > 0) {
      ## Show last N models ----------------------------------------------------
      
      display_models <- tail(recent_models, show_recent)
      
      for (model in display_models) {
        ## Truncate workflow ID to fit -----------------------------------------
        
        wflow_display <- substr(model$wflow_id, 1, 50)
        cat(sprintf("║   %-50s R²=%.3f\n", wflow_display, model$rsq))
      }
    } else {
      cat("║   (no completed models yet)\n")
    }
    
    ## Model timing breakdown --------------------------------------------------
    
    if (length(timing_data) > 0) {
      cat("╠══════════════════════════════════════════════════════════════════════\n")
      cat("║ Model Timing:                                                          \n")
      
      fast   <- sum(timing_data < 10)
      medium <- sum(timing_data >= 10 & timing_data < 60)
      slow   <- sum(timing_data >= 60)
      total  <- length(timing_data)
      
      cat(sprintf("║   Fast (<10s): %3d (%.1f%%)\n", fast, 100 * fast / total))
      cat(sprintf("║   Medium (10-60s): %3d (%.1f%%)\n", medium, 100 * medium / total))
      cat(sprintf("║   Slow (>60s): %3d (%.1f%%)\n", slow, 100 * slow / total))
    }
    
    ## Worker statistics section -----------------------------------------------
    
    if (show_workers && length(worker_data) > 0) {
      cat("╠══════════════════════════════════════════════════════════════════════\n")
      cat("║ Worker Performance:                                                    \n")
      
      ## Sort workers by completed models --------------------------------------
      
      worker_sorted <- worker_data[order(sapply(worker_data, function(x) x$completed), 
                                         decreasing = TRUE)]
      worker_ids <- names(worker_sorted)
      
      ## Show top 3 workers ----------------------------------------------------
      
      cat("║ Top Workers:\n")
      for (i in 1:min(3, length(worker_sorted))) {
        w <- worker_sorted[[i]]
        cat(sprintf("║   Worker %s: %3d completed, %2d failed, avg %.1fs\n",
                    worker_ids[i], w$completed, w$failed, 
                    ifelse(is.na(w$avg_time), 0, w$avg_time)))
      }
      
      ## Show bottom 3 workers -------------------------------------------------
      
      if (length(worker_sorted) > 3) {
        cat("║ Bottom Workers:\n")
        start_idx <- max(length(worker_sorted) - 2, 4)
        
        for (i in start_idx:length(worker_sorted)) {
          w <- worker_sorted[[i]]
          cat(sprintf("║   Worker %s: %3d completed, %2d failed, avg %.1fs\n",
                      worker_ids[i], w$completed, w$failed,
                      ifelse(is.na(w$avg_time), 0, w$avg_time)))
        }
      }
    }
    
    ## Footer ------------------------------------------------------------------
    
    cat("╚══════════════════════════════════════════════════════════════════════\n")
    cat(sprintf("\nLast updated: %s | Refreshing every %d seconds...\n",
                format(Sys.time(), "%H:%M:%S"), refresh_seconds))
    
    ## Sleep before next refresh -----------------------------------------------
    
    Sys.sleep(refresh_seconds)
  }
}