#' Progress Reporting Utilities for Parallel Model Evaluation
#'
#' @description
#' Helper functions for enhanced progress reporting during parallel model evaluation.
#' Provides ASCII box formatting, metrics calculation, and resource monitoring
#' for clean, informative console output during long-running HPC jobs.
#'
#' @details
#' This module supports the enhanced progress reporting in `evaluate_models_parallel()`.
#' Functions are designed to work together to provide comprehensive status updates
#' without cluttering the console or interfering with parallel processing.
#'
#' @importFrom pryr mem_used
#' @importFrom cli cli_progress_step
#'
#'
#' @param chunk_idx Current chunk number
#' @param n_chunks Total number of chunks
#' @param metrics_list List containing performance metrics (from calculate_chunk_metrics)
#' @param resource_stats List containing throughput and memory information
#' @param duration Numeric chunk duration in minutes
#' @param counts List containing success and failure counts
#' @param global_best Optional global best result for display
#' @param width Console width for box formatting (default: 68)
#'
#' @return Invisible NULL (prints to console)
#' @export

print_chunk_summary <- function(chunk_idx,
                                n_chunks,
                                metrics_list,
                                resource_stats,
                                duration,
                                counts,
                                global_best     = NULL,
                                width          = 68) {

  ## Create box borders --------------------------------------------------------

  top_border    <- paste0("+", paste(rep("-", width - 2), collapse = ""), "+")
  middle_border <- paste0("+", paste(rep("-", width - 2), collapse = ""), "+")
  bottom_border <- paste0("+", paste(rep("-", width - 2), collapse = ""), "+")

  ## Header line ---------------------------------------------------------------

  header_text <- sprintf("Chunk %d/%d Complete (%.1f min)", chunk_idx, n_chunks, duration)
  header_padding <- width - nchar(header_text) - 4
  header_line <- sprintf("| %s%s |", header_text, paste(rep(" ", header_padding), collapse = ""))

  ## Performance metrics lines -------------------------------------------------

  performance_lines <- c()

  if (!is.null(metrics_list$rrmse)) {
    rrmse_text <- sprintf("RRMSE: Best %.2f | Avg %.2f | Worst %.2f",
                         metrics_list$rrmse$best,
                         metrics_list$rrmse$avg,
                         metrics_list$rrmse$worst)
    rrmse_padding <- width - nchar(rrmse_text) - 4
    performance_lines <- c(performance_lines,
                          sprintf("| %s%s |", rrmse_text, paste(rep(" ", rrmse_padding), collapse = "")))
  }

  if (!is.null(metrics_list$rsq)) {
    rsq_text <- sprintf("R-sq:  Best %.3f | Avg %.3f | Worst %.3f",
                       metrics_list$rsq$best,
                       metrics_list$rsq$avg,
                       metrics_list$rsq$worst)
    rsq_padding <- width - nchar(rsq_text) - 4
    performance_lines <- c(performance_lines,
                          sprintf("| %s%s |", rsq_text, paste(rep(" ", rsq_padding), collapse = "")))
  }

  ## Resource monitoring lines -------------------------------------------------

  throughput_text <- sprintf("Throughput: %.1f models/min | ETA: %.1f min",
                            resource_stats$models_per_min,
                            resource_stats$eta_minutes)
  throughput_padding <- width - nchar(throughput_text) - 4
  throughput_line <- sprintf("| %s%s |", throughput_text, paste(rep(" ", throughput_padding), collapse = ""))

  memory_text <- sprintf("Memory: Total %.0fMB | Per worker: ~%.0fMB",
                        resource_stats$memory$total_mb,
                        resource_stats$memory$estimated_per_worker_mb)
  memory_padding <- width - nchar(memory_text) - 4
  memory_line <- sprintf("| %s%s |", memory_text, paste(rep(" ", memory_padding), collapse = ""))

  ## Status line ---------------------------------------------------------------

  status_text <- sprintf("Success: %d | Failed: %d", counts$success, counts$failed)
  status_padding <- width - nchar(status_text) - 4
  status_line <- sprintf("| %s%s |", status_text, paste(rep(" ", status_padding), collapse = ""))

  ## Global best line (optional) -----------------------------------------------

  global_lines <- c()
  if (!is.null(global_best)) {

    # Performance line
    best_perf_text <- sprintf("Global Best: RRMSE %.2f | R-sq %.3f",
                             global_best$rrmse, global_best$rsq)
    best_perf_padding <- width - nchar(best_perf_text) - 4
    global_lines <- c(global_lines,
                     sprintf("| %s%s |", best_perf_text, paste(rep(" ", best_perf_padding), collapse = "")))

    # Configuration line
    config_text <- sprintf("  -> %s", global_best$config_summary)
    if (nchar(config_text) > width - 4) {
      config_text <- paste0(substr(config_text, 1, width - 7), "...")
    }
    config_padding <- width - nchar(config_text) - 4
    global_lines <- c(global_lines,
                     sprintf("| %s%s |", config_text, paste(rep(" ", config_padding), collapse = "")))
  }

  ## Print complete box --------------------------------------------------------

  cat("\n")
  cat(top_border, "\n")
  cat(header_line, "\n")
  cat(middle_border, "\n")

  for (line in performance_lines) {
    cat(line, "\n")
  }

  cat(middle_border, "\n")
  cat(throughput_line, "\n")
  cat(memory_line, "\n")
  cat(status_line, "\n")

  if (length(global_lines) > 0) {
    cat(middle_border, "\n")
    for (line in global_lines) {
      cat(line, "\n")
    }
  }

  cat(bottom_border, "\n")
  cat("\n")

  invisible(NULL)
}

## -----------------------------------------------------------------------------
## Performance Metrics Calculation
## -----------------------------------------------------------------------------

#' Calculate Performance Metrics from Chunk Results
#'
#' @param chunk_results List of results from parallel chunk processing
#' @param metrics Character vector of metrics to calculate (default: c("rrmse", "rsq"))
#'
#' @return List containing best/avg/worst for each requested metric
#' @export

calculate_chunk_metrics <- function(chunk_results,
                                   metrics = c("rrmse", "rsq")) {

  ## Extract successful results -----------------------------------------------

  successful_results <- chunk_results[sapply(chunk_results, function(x) x$status == "success")]

  if (length(successful_results) == 0) {
    return(NULL)
  }

  ## Calculate metrics for each requested type ---------------------------------

  metrics_list <- list()

  for (metric in metrics) {

    # Extract metric values from successful results
    values <- sapply(successful_results, function(x) {
      if (!is.null(x$metrics) && metric %in% names(x$metrics)) {
        return(x$metrics[[metric]])
      } else {
        return(NA_real_)
      }
    })

    # Remove any NA values
    values <- values[!is.na(values)]

    if (length(values) > 0) {

      # For R-squared, higher is better; for error metrics, lower is better
      if (metric %in% c("rsq", "r_sq", "r2")) {
        metrics_list[[metric]] <- list(
          best  = max(values, na.rm = TRUE),
          avg   = mean(values, na.rm = TRUE),
          worst = min(values, na.rm = TRUE)
        )
      } else {
        metrics_list[[metric]] <- list(
          best  = min(values, na.rm = TRUE),
          avg   = mean(values, na.rm = TRUE),
          worst = max(values, na.rm = TRUE)
        )
      }
    }
  }

  return(metrics_list)
}

## -----------------------------------------------------------------------------
## Resource Monitoring
## -----------------------------------------------------------------------------

#' Get Worker Memory Statistics
#'
#' @param n_workers Number of parallel workers
#' @param baseline_memory_mb Baseline memory usage at start (MB)
#'
#' @return List containing total memory, estimated per-worker memory, and growth
#' @export

get_worker_memory_stats <- function(n_workers, baseline_memory_mb = NULL) {

  ## Current total memory usage ------------------------------------------------

  current_memory_mb <- as.numeric(pryr::mem_used()) / 1048576  # Convert bytes to MB

  ## Estimate per-worker memory ------------------------------------------------

  # Rough estimation: assume equal distribution across workers plus overhead
  estimated_overhead_mb <- 200  # Base R session overhead

  if (!is.null(baseline_memory_mb)) {
    memory_growth_mb <- current_memory_mb - baseline_memory_mb
    estimated_per_worker_mb <- max(0, memory_growth_mb / n_workers)
  } else {
    available_for_workers_mb <- max(0, current_memory_mb - estimated_overhead_mb)
    estimated_per_worker_mb <- available_for_workers_mb / n_workers
  }

  ## Return comprehensive stats ------------------------------------------------

  return(list(
    total_mb                  = current_memory_mb,
    estimated_per_worker_mb   = estimated_per_worker_mb,
    baseline_mb               = baseline_memory_mb,
    growth_mb                 = if (!is.null(baseline_memory_mb)) current_memory_mb - baseline_memory_mb else NA_real_,
    overhead_mb               = estimated_overhead_mb
  ))
}

## -----------------------------------------------------------------------------
## Global Best Formatting
## -----------------------------------------------------------------------------

#' Format Global Best Model Information
#'
#' @param all_results List of all results from parallel processing
#'
#' @return List containing best model information for display, or NULL if no successful models
#' @export

format_global_best <- function(all_results) {

  ## Extract all successful results --------------------------------------------

  successful_results <- all_results[sapply(all_results, function(x) x$status == "success")]

  if (length(successful_results) == 0) {
    return(NULL)
  }

  ## Find the best model by RRMSE ---------------------------------------------

  rrmse_values <- sapply(successful_results, function(x) {
    if (!is.null(x$metrics) && "rrmse" %in% names(x$metrics)) {
      return(x$metrics$rrmse)
    } else {
      return(Inf)  # Worst possible value for RRMSE
    }
  })

  best_index <- which.min(rrmse_values)
  best_result <- successful_results[[best_index]]

  ## Extract performance metrics -----------------------------------------------

  best_rrmse <- best_result$metrics$rrmse
  best_rsq   <- if ("rsq" %in% names(best_result$metrics)) best_result$metrics$rsq else NA_real_

  ## Format configuration summary ----------------------------------------------

  config_parts <- c()

  # Core configuration
  if (!is.null(best_result$model)) config_parts <- c(config_parts, best_result$model)
  if (!is.null(best_result$preprocessing)) config_parts <- c(config_parts, best_result$preprocessing)
  if (!is.null(best_result$feature_selection)) config_parts <- c(config_parts, best_result$feature_selection)

  # Covariates (if any)
  if (!is.null(best_result$covariates) && length(best_result$covariates) > 0) {
    if (is.list(best_result$covariates)) {
      covariates_list <- best_result$covariates[[1]]
    } else {
      covariates_list <- best_result$covariates
    }

    if (length(covariates_list) > 0) {
      covariates_str <- paste0("[", paste(covariates_list, collapse = ", "), "]")
      config_parts <- c(config_parts, covariates_str)
    }
  }

  config_summary <- paste(config_parts, collapse = " + ")

  ## Return formatted information ----------------------------------------------

  return(list(
    rrmse          = best_rrmse,
    rsq            = best_rsq,
    config_summary = config_summary,
    model_id       = if (!is.null(best_result$model_id)) best_result$model_id else "unknown"
  ))
}

## -----------------------------------------------------------------------------
## Progress Level Utilities
## -----------------------------------------------------------------------------

#' Get Appropriate Metrics List Based on Progress Level
#'
#' @param progress_level Character. One of "minimal", "standard", "detailed"
#'
#' @return Character vector of metrics to calculate and display
#' @export

get_metrics_for_progress_level <- function(progress_level = "standard") {

  switch(progress_level,
    "minimal"  = c("rrmse"),
    "standard" = c("rrmse", "rsq"),
    "detailed" = c("rrmse", "rsq", "mae", "rpd"),
    c("rrmse", "rsq")  # Default fallback
  )
}
