#' Parallel Evaluation: File-Based Logging & Live Monitor (dir-aware)
#'
#' @description
#' Utilities for writing per-worker progress logs and for monitoring a running
#' parallel job in real time. The monitor can read either:
#' - a **directory** of per-worker logs (e.g., `parallel_*.log`), or
#' - a **single log file**.
#'
#' This is a drop-in script. Source it before launching the monitor:
#'
#' ```
#' horizons::monitor_parallel_progress(
#'   "/scratch/<user>/horizons/run_<stamp>/parallel_models",
#'   refresh_seconds = 2,
#'   show_recent = 10
#' )
#' ```
#'
#' @section Log line formats:
#' - START: `YYYY-mm-dd HH:MM:SS [W##] START [ 123/6300]: <desc>`
#' - DONE : `YYYY-mm-dd HH:MM:SS [W##] ✓ DONE [ 123/6300]: <model_id> (R²=0.734, RMSE=...) - 108.1s`
#' - FAIL : `YYYY-mm-dd HH:MM:SS [W##] ✗ FAIL [ 123/6300]: <desc> | ERROR: <msg>`
#'
#' @keywords logging monitoring parallel
#' @importFrom stats aggregate median quantile
#' @importFrom utils tail
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger cli_h3
#' @name parallel_monitoring
NULL


# ---------- helpers: read logs from a directory or a single file ----------

#' (internal) Read and merge progress lines
#' @keywords internal
.read_progress_lines <- function(path) {
  if (is.null(path)) return(character(0))
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "^parallel_.*\\.log$", full.names = TRUE)
    if (length(files) == 0) return(character(0))
    unlist(lapply(files, function(f) {
      tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    }), use.names = FALSE)
  } else if (file.exists(path)) {
    tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  } else {
    character(0)
  }
}


# ---------- simple logger (single-writer OR per-worker) ----------

#' Simple File-Based Logging for Parallel Workers
#'
#' @description
#' Append a single progress line to a log file (safe for concurrent appends).
#' If `log_file` is a directory, writes to `master.log` inside it.
#'
#' @param message Character. Message to log (already formatted)
#' @param log_file Character. Path to log file or a directory
#' @param worker_id Integer. Worker identifier (optional)
#' @param include_timestamp Logical. Include timestamp in log entry
#' @export
log_parallel_progress <- function(message,
                                  log_file = NULL,
                                  worker_id = NULL,
                                  include_timestamp = TRUE) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  # If a directory is given, write to master.log inside it
  target <- if (dir.exists(log_file)) file.path(log_file, "master.log") else log_file
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)

  # Build line
  if (include_timestamp) {
    ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    wid <- if (!is.null(worker_id)) sprintf(" [W%02d]", worker_id) else ""
    line <- sprintf("%s%s %s\n", ts, wid, message)
  } else {
    wid <- if (!is.null(worker_id)) sprintf(" [W%02d]", worker_id) else ""
    line <- sprintf("%s%s\n", message, wid)
  }

  # Append
  tryCatch({
    cat(line, file = target, append = TRUE)
  }, error = function(e) invisible(NULL))

  invisible(NULL)
}

#' Initialize Parallel Progress Log (single file or directory)
#'
#' @description
#' Creates/overwrites a header for a progress log. If `log_file` is a directory,
#' writes header to `master.log` inside it.
#'
#' @param log_file Character. Path to log file or directory
#' @param n_models Integer. Total number of models (optional)
#' @return Character. Path to the file that received the header
#' @export
init_parallel_log <- function(log_file = NULL, n_models = NULL) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  target <- if (dir.exists(log_file)) file.path(log_file, "master.log") else log_file
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)

  header <- paste0(
    "==============================================\n",
    "Parallel Model Evaluation Log\n",
    "Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
  )
  if (!is.null(n_models)) {
    header <- paste0(header, "Total models: ", n_models, "\n")
  }
  header <- paste0(header, "==============================================\n\n")

  cat(header, file = target, append = FALSE)
  cli::cli_alert_success("Log file initialized: {target}")
  target
}


# ---------- summarization ----------

#' Get Parallel Progress Summary (dir-aware)
#'
#' @description
#' Parse one or more log files and compute progress statistics, best R²,
#' timing summaries, and a coarse ETA with confidence bounds.
#'
#' @param log_file Path to a directory of per-worker logs or a single log file.
#'                 Default: `file.path(tempdir(), "parallel_progress.log")`.
#' @param return_details Logical. If `TRUE`, return a list of statistics
#'                       (otherwise prints a summary and returns `NULL`).
#' @return If `return_details=TRUE`, a list with keys:
#'   `n_started, n_completed, n_failed, total_models, progress_pct, success_rate,
#'    elapsed_mins, rate, best_rsq, metrics_data(list), eta_info(list), start_time`.
#' @export
summarize_parallel_progress <- function(log_file = NULL, return_details = FALSE) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }

  lines <- .read_progress_lines(log_file)
  if (length(lines) == 0) {
    cli::cli_alert_warning("No log lines found at: {log_file}")
    return(invisible(NULL))
  }

  # parse line types
  start_lines <- grep("START \\[", lines, value = TRUE)
  done_lines  <- grep("✓ DONE \\[", lines, value = TRUE)
  fail_lines  <- grep("✗ FAIL \\[", lines, value = TRUE)

  n_started   <- length(start_lines)
  n_completed <- length(done_lines)
  n_failed    <- length(fail_lines)

  # total models = max denominator across START lines
  total_models <- NA_real_
  if (length(start_lines) > 0) {
    denom <- suppressWarnings(as.numeric(sub("^.*\\[(\\s*\\d+)\\/(\\d+)\\].*$", "\\2", start_lines)))
    denom <- denom[is.finite(denom)]
    if (length(denom) > 0) total_models <- max(denom)
  }

  # metrics & times from DONE
  rsq_values  <- numeric(0)
  model_times <- numeric(0)
  if (length(done_lines) > 0) {
    # R²=...
    rsq_values <- suppressWarnings(as.numeric(sub(".*R²=(\\d+\\.?\\d*).*", "\\1", grep("R²=", done_lines, value = TRUE))))
    rsq_values <- rsq_values[is.finite(rsq_values)]
    # trailing " - 123.4s"
    secs <- suppressWarnings(as.numeric(sub(".* (\\d+\\.?\\d*)s\\s*$", "\\1", done_lines)))
    model_times <- secs[is.finite(secs)]
  }

  # elapsed minutes using timestamps like "[YYYY-mm-dd HH:MM:SS]"
  time_lines <- grep("\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]", lines, value = TRUE)
  start_time <- last_time <- NULL
  elapsed_mins <- NA_real_
  if (length(time_lines) >= 2) {
    stamps <- sub("^.*\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\].*$", "\\1", time_lines)
    stamps <- as.POSIXct(stamps, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    stamps <- stamps[order(stamps)]
    if (length(stamps) >= 2) {
      start_time  <- stamps[1]
      last_time   <- stamps[length(stamps)]
      elapsed_mins <- as.numeric(difftime(last_time, start_time, units = "mins"))
    }
  }

  progress_pct <- if (!is.na(total_models) && total_models > 0) round(100 * n_completed / total_models, 1) else NA_real_
  success_rate <- if ((n_completed + n_failed) > 0) round(100 * n_completed / (n_completed + n_failed), 1) else NA_real_
  rate         <- if (!is.na(elapsed_mins) && elapsed_mins > 0) n_completed / elapsed_mins else NA_real_
  best_rsq     <- if (length(rsq_values) > 0) max(rsq_values, na.rm = TRUE) else NA_real_

  eta_info <- calculate_eta_with_ci(
    n_completed  = n_completed,
    n_remaining  = if (is.na(total_models)) NA else max(total_models - n_completed, 0),
    elapsed_mins = elapsed_mins,
    model_times  = model_times
  )

  # console summary (tolerates NAs)
  cli::cli_h3("Progress Summary")
  if (!is.na(total_models)) cli::cli_alert_info("Progress: {n_completed}/{total_models} ({progress_pct}%)")
  cli::cli_alert_success("Completed: {n_completed}")
  cli::cli_alert_danger("Failed: {n_failed}")
  if (!is.na(success_rate)) cli::cli_alert_info("Success rate: {success_rate}%")
  if (!is.na(elapsed_mins)) cli::cli_alert_info("Elapsed: {round(elapsed_mins, 1)} minutes")
  if (!is.na(rate))         cli::cli_alert_info("Rate: {round(rate, 2)} models/minute")
  if (!is.na(best_rsq))     cli::cli_alert_success("Best R² so far: {round(best_rsq, 3)}")
  if (!is.na(eta_info$eta_median)) {
    cli::cli_alert_info("ETA: {round(eta_info$eta_median, 1)} min (95% CI: {round(eta_info$eta_lower, 1)}–{round(eta_info$eta_upper, 1)} min)")
  }

  if (return_details) {
    return(list(
      n_started     = n_started,
      n_completed   = n_completed,
      n_failed      = n_failed,
      total_models  = total_models,
      progress_pct  = progress_pct,
      success_rate  = success_rate,
      elapsed_mins  = elapsed_mins,
      rate          = rate,
      best_rsq      = best_rsq,
      metrics_data  = list(rsq_values = rsq_values, model_times = model_times),
      eta_info      = eta_info,
      start_time    = start_time
    ))
  }
  invisible(NULL)
}


# ---------- live monitor ----------

#' Real-time Parallel Progress Monitor (dir-aware)
#'
#' @description
#' Live dashboard for monitoring a running parallel evaluation. Accepts either a
#' directory of per-worker logs or a single log file.
#'
#' @param log_file Path to directory or single log file. If `NULL`, defaults to
#'   `file.path(tempdir(), "parallel_progress.log")`.
#' @param refresh_seconds Seconds between refreshes (default: 5)
#' @param show_recent How many recent "DONE" entries to display (default: 5)
#' @export
monitor_parallel_progress <- function(log_file = NULL, refresh_seconds = 5, show_recent = 5) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }
  cli::cli_alert_info("Starting live monitor. Press Ctrl+C to stop.")
  Sys.sleep(1)

  last_best_rsq <- -Inf
  repeat {
    cat("\014")
    cat("╔══════════════════════════════════════════════════════════════════════\n")
    cat("║              PARALLEL MODEL EVALUATION MONITOR                       \n")
    cat("╠══════════════════════════════════════════════════════════════════════\n")

    lines <- .read_progress_lines(log_file)
    if (length(lines) == 0) {
      cat(sprintf("║ Waiting for logs at: %s\n", log_file))
      cat("╚══════════════════════════════════════════════════════════════════════\n")
      Sys.sleep(refresh_seconds); next
    }

    stats <- summarize_parallel_progress(log_file, return_details = TRUE)
    if (is.null(stats)) {
      cat("║ No data available yet...\n")
      cat("╚══════════════════════════════════════════════════════════════════════\n")
      Sys.sleep(refresh_seconds); next
    }

    # progress line
    if (!is.na(stats$total_models) && stats$total_models > 0) {
      progress_bar <- create_ascii_progress_bar(stats$n_completed, stats$total_models, width = 40)
      cat(sprintf("║ Progress: %s %5.1f%% \n", progress_bar, stats$progress_pct))
    }
    cat(sprintf("║ Complete: %4d │ Failed: %3d │ Success Rate: %5.1f%%                 \n",
                stats$n_completed, stats$n_failed, ifelse(is.na(stats$success_rate), 0, stats$success_rate)))
    if (!is.na(stats$elapsed_mins)) {
      cat(sprintf("║ Elapsed: %s │ Rate: %4.1f models/min                                  \n",
                  format_time(stats$elapsed_mins), ifelse(is.na(stats$rate), 0, stats$rate)))
    }

    # best model line
    if (!is.na(stats$best_rsq)) {
      if (stats$best_rsq > last_best_rsq) {
        cat(sprintf("║ 🏆 NEW BEST R²: %.3f                                                \n", stats$best_rsq))
        last_best_rsq <- stats$best_rsq
      } else {
        cat(sprintf("║ Best R² so far: %.3f                                               \n", stats$best_rsq))
      }
    }

    # ETA
    if (!is.na(stats$eta_info$eta_median)) {
      eta_str <- format_time(stats$eta_info$eta_median)
      ci_lo   <- format_time(stats$eta_info$eta_lower)
      ci_hi   <- format_time(stats$eta_info$eta_upper)
      cat(sprintf("║ ETA: %s (95%% CI: %s-%s)\n", eta_str, ci_lo, ci_hi))
    }

    cat("╠══════════════════════════════════════════════════════════════════════\n")
    cat("║ Recent Models:                                                         \n")

    # Recent DONE lines across all logs
    done_lines <- grep("✓ DONE", lines, value = TRUE)
    if (length(done_lines) > 0) {
      recent <- utils::tail(done_lines, show_recent)
      for (line in recent) {
        model_match <- regmatches(line, regexpr("\\]: .* \\(", line))
        if (length(model_match) > 0) {
          model_info <- gsub("\\]: | \\(", "", model_match)
          model_info <- substr(model_info, 1, 50)
          rsq_match  <- regmatches(line, regexpr("R²=\\d+\\.\\d+", line))
          if (length(rsq_match) > 0) {
            rsq_val <- as.numeric(sub("R²=", "", rsq_match))
            cat(sprintf("║   %-50s R²=%.3f\n", model_info, rsq_val))
          } else {
            cat(sprintf("║   %-50s\n", model_info))
          }
        }
      }
    }

    # Model timing block (never aggregate on zero rows)
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    cat("║ Model Timing:                                                          \n")

    mt <- stats$metrics_data$model_times
    if (length(mt) > 0) {
      fast   <- sum(mt < 10, na.rm = TRUE)
      medium <- sum(mt >= 10 & mt < 60, na.rm = TRUE)
      slow   <- sum(mt >= 60, na.rm = TRUE)
      total  <- fast + medium + slow
      if (total > 0) {
        cat(sprintf("║   Fast (<10s): %3d (%.1f%%)\n", fast,   100*fast/total))
        cat(sprintf("║   Medium (10-60s): %3d (%.1f%%)\n", medium, 100*medium/total))
        cat(sprintf("║   Slow (>60s): %3d (%.1f%%)\n", slow,   100*slow/total))
      }

      # Per-model averages (guarded)
      model_times_df <- NULL
      if (length(done_lines) > 0) {
        model_times_df <- do.call(rbind, lapply(done_lines, function(line) {
          # Extract "<model_id> ... - 123.4s" → capture model token + time
          m <- sub("^.*\\]:\\s*([^\\s]+).*-(?:\\s*)(\\d+\\.?\\d*)s\\s*$", "\\1,\\2", line)
          if (is.na(m) || identical(m, line)) return(NULL)
          parts <- strsplit(m, ",", fixed = TRUE)[[1]]
          if (length(parts) != 2) return(NULL)
          data.frame(model = parts[1], time = suppressWarnings(as.numeric(parts[2])))
        }))
      }
      if (!is.null(model_times_df) && nrow(model_times_df) > 0) {
        model_times_df <- model_times_df[is.finite(model_times_df$time), , drop = FALSE]
        if (nrow(model_times_df) > 0) {
          ag <- stats::aggregate(time ~ model, data = model_times_df,
                                 FUN = function(x) c(mean = mean(x), n = length(x)))
          ag <- data.frame(model = ag$model,
                           avg_time = ag$time[, "mean"],
                           count    = ag$time[, "n"])
          ag <- ag[order(ag$avg_time), , drop = FALSE]
          top <- utils::tail(ag[order(ag$avg_time), , drop = FALSE], -max(0, nrow(ag) - 6))
          if (nrow(top) > 0) {
            cat("╠══════════════════════════════════════════════════════════════════════\n")
            cat("║ Model Times (Average):                                                \n")
            apply(top, 1, function(r) {
              cat(sprintf("║   %-12s: %.1fs avg (%s models)\n",
                          r[["model"]], as.numeric(r[["avg_time"]]), r[["count"]]))
            })
          }
        }
      }
    } else {
      cat("║   (no timing data yet)\n")
    }

    cat("╚══════════════════════════════════════════════════════════════════════\n")
    cat(sprintf("\nLast updated: %s | Refreshing every %d seconds...\n",
                format(Sys.time(), "%H:%M:%S"), refresh_seconds))
    Sys.sleep(refresh_seconds)
  }
}


# ---------- ETA helper ----------

#' Calculate ETA with Confidence Intervals
#'
#' @description
#' Estimate remaining time in minutes using either recent model times (if
#' available) or a fallback average rate.
#'
#' @param n_completed Number of completed models
#' @param n_remaining Number of remaining models
#' @param elapsed_mins Elapsed minutes since start
#' @param model_times Numeric vector of per-model times (seconds)
#' @return List with `eta_median`, `eta_lower`, `eta_upper` (minutes)
#' @export
calculate_eta_with_ci <- function(n_completed, n_remaining, elapsed_mins, model_times = NULL) {
  if (is.na(n_remaining) || n_remaining <= 0 || is.na(elapsed_mins) || elapsed_mins <= 0) {
    return(list(eta_median = NA_real_, eta_lower = NA_real_, eta_upper = NA_real_))
  }

  if (!is.null(model_times) && length(model_times) >= 10) {
    recent <- utils::tail(model_times, 100L)
    recent <- recent[is.finite(recent)]
    if (length(recent) >= 5) {
      med <- stats::median(recent) / 60
      q25 <- stats::quantile(recent, 0.25, na.rm = TRUE) / 60
      q75 <- stats::quantile(recent, 0.75, na.rm = TRUE) / 60
      return(list(
        eta_median = n_remaining * med,
        eta_lower  = n_remaining * q25,
        eta_upper  = n_remaining * q75
      ))
    }
  }

  # Fallback: average time per completed
  if (!is.na(n_completed) && n_completed > 0) {
    avg <- elapsed_mins / n_completed
    return(list(
      eta_median = n_remaining * avg,
      eta_lower  = n_remaining * avg * 0.8,
      eta_upper  = n_remaining * avg * 1.2
    ))
  }

  list(eta_median = NA_real_, eta_lower = NA_real_, eta_upper = NA_real_)
}


# ---------- small UI helpers ----------

#' Create ASCII Progress Bar
#' @param current Current value
#' @param total Total value
#' @param width Width in characters
#' @return Character string progress bar
#' @export
create_ascii_progress_bar <- function(current, total, width = 40) {
  if (is.na(current) || is.na(total) || total <= 0) {
    return(paste(rep("-", width), collapse = ""))
  }
  pct <- max(min(current / total, 1), 0)
  filled <- round(pct * width); empty <- width - filled
  paste0("[", paste(rep("█", filled), collapse = ""),
         paste(rep("░", empty), collapse = ""), "]")
}

#' Format Time Duration
#' @param minutes Numeric minutes
#' @return Formatted string like "2h 15m" or "45m"
#' @export
format_time <- function(minutes) {
  if (is.na(minutes)) return("--:--")
  hours <- as.integer(floor(minutes / 60))
  mins  <- as.integer(round(minutes %% 60))
  if (hours > 0) sprintf("%dh %dm", hours, mins) else sprintf("%dm", mins)
}


# ---------- worker stats ----------

#' Get Worker Statistics from Logs (dir-aware)
#'
#' @description
#' Summarize per-worker starts/completions/failures and average completion time.
#'
#' @param log_file Path to a single log file OR a directory of per-worker logs.
#' @return Data frame with columns: `worker_id, n_started, n_complete, n_failed, avg_time`
#' @export
get_worker_statistics <- function(log_file) {
  lines <- .read_progress_lines(log_file)
  if (!length(lines)) return(NULL)

  worker_pattern <- "\\[W(\\d+)\\]"

  start_lines <- grep("START.*\\[W\\d+\\]", lines, value = TRUE)
  done_lines  <- grep("✓ DONE.*\\[W\\d+\\]",  lines, value = TRUE)
  fail_lines  <- grep("✗ FAIL.*\\[W\\d+\\]",  lines, value = TRUE)

  # collect unique workers
  all_workers <- integer(0)
  for (ln in c(start_lines, done_lines, fail_lines)) {
    wm <- regmatches(ln, regexpr(worker_pattern, ln))
    if (length(wm) > 0) {
      wid <- suppressWarnings(as.integer(sub("\\[W", "", sub("\\]", "", wm))))
      if (!is.na(wid)) all_workers <- c(all_workers, wid)
    }
  }
  unique_workers <- sort(unique(all_workers))
  if (!length(unique_workers)) return(NULL)

  worker_data <- lapply(unique_workers, function(wid) {
    wid_pattern <- sprintf("\\[W%02d\\]", wid)
    n_started   <- sum(grepl(wid_pattern, start_lines))
    n_complete  <- sum(grepl(wid_pattern, done_lines))
    n_failed    <- sum(grepl(wid_pattern, fail_lines))

    worker_done_lines <- done_lines[grepl(wid_pattern, done_lines)]
    times <- suppressWarnings(as.numeric(sub("s", "", regmatches(worker_done_lines, regexpr("\\d+\\.?\\d*s", worker_done_lines)))))
    times <- times[is.finite(times)]
    avg_time <- if (length(times)) mean(times) else NA_real_

    data.frame(worker_id = wid,
               n_started = n_started,
               n_complete = n_complete,
               n_failed = n_failed,
               avg_time = avg_time)
  })

  worker_stats <- do.call(rbind, worker_data)
  worker_stats[order(worker_stats$n_complete, decreasing = TRUE), , drop = FALSE]
}


# ---------- best model tracker ----------

#' Track Best Model in Log (master-only writer, dir-aware)
#'
#' @description
#' If `log_file` is a directory, appends to `master.log` within it (single writer).
#' If it's a file, appends to that file. This is for the **orchestrator** only.
#'
#' @param log_file Path to a directory or a single file
#' @param model_id Model identifier (string)
#' @param rsq Numeric R² value
#' @param previous_best Previous best R² (optional)
#' @export
log_new_best_model <- function(log_file, model_id, rsq, previous_best = NULL) {
  target <- if (dir.exists(log_file)) file.path(log_file, "master.log") else log_file
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)

  msg <- sprintf("🏆 NEW BEST MODEL: %s (R²=%.3f)", model_id, rsq)
  if (!is.null(previous_best)) {
    msg <- sprintf("%s - Previous best: %.3f", msg, previous_best)
  }
  line <- sprintf("%s [W00] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
  cat(line, "\n", file = target, append = TRUE)
}
