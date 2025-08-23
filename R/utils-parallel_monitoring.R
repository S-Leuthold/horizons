#' Real-time Parallel Progress Monitor
#'
#' @description
#' Live dashboard for monitoring parallel model evaluation progress.
#' Accepts either a single log file or a directory containing multiple
#' per-worker logs (e.g., "parallel_<PID>.log"). Lines are merged in
#' chronological order by their leading timestamp.
#'
#' @param log_file Character. Path to a single log file OR a directory of logs.
#'   If NULL, defaults to `file.path(tempdir(), "parallel_progress.log")`.
#' @param refresh_seconds Numeric. Seconds between refreshes (default: 5)
#' @param show_recent Integer. Number of recent completed models to show (default: 5)
#'
#' @importFrom cli cli_alert_info cli_alert_warning
#' @export
monitor_parallel_progress <- function(log_file = NULL,
                                      refresh_seconds = 5,
                                      show_recent = 5) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_progress.log")
  }

  .banner_start <- function() {
    cat("╔══════════════════════════════════════════════════════════════════════\n")
    cat("║              PARALLEL MODEL EVALUATION MONITOR                       \n")
    cat("╠══════════════════════════════════════════════════════════════════════\n")
  }
  .banner_end <- function() {
    cat("╚══════════════════════════════════════════════════════════════════════\n")
  }

  # First notice
  if (!.path_has_logs(log_file)) {
    cli::cli_alert_warning("No log data yet at: {log_file}")
    cli::cli_alert_info("Will wait for logs to appear...")
  }
  cli::cli_alert_info("Starting live monitor. Press Ctrl+C to stop.")
  Sys.sleep(1.5)

  last_best_rsq <- -Inf

  repeat {
    # Clear console (works on most terminals)
    cat("\014")

    .banner_start()

    # Merge lines from file or directory
    lines <- .read_merged_log_lines(log_file)

    if (!length(lines)) {
      cat("║ Waiting for log data...                                               \n")
      .banner_end()
      Sys.sleep(refresh_seconds)
      next
    }

    # Compute stats from lines
    stats <- .summarize_from_lines(lines)

    if (is.null(stats)) {
      cat("║ No parsable data yet...                                               \n")
      .banner_end()
      Sys.sleep(refresh_seconds)
      next
    }

    # Progress bar (Critical: uses parsed total_models & n_completed)
    if (!is.na(stats$total_models) && stats$total_models > 0) {
      progress_bar <- create_ascii_progress_bar(stats$n_completed, stats$total_models, width = 40)
      progress_line <- sprintf("Progress: %s %5.1f%%", progress_bar, stats$progress_pct)
      cat(sprintf("║ %-71s \n", progress_line))
    }

    # Key metrics
    metrics_line <- sprintf("Complete: %4d │ Failed: %3d │ Success Rate: %5.1f%%",
                            stats$n_completed %||% 0,
                            stats$n_failed    %||% 0,
                            stats$success_rate %||% 0)
    cat(sprintf("║ %-71s \n", metrics_line))

    # Timing / rate
    if (!is.na(stats$elapsed_mins)) {
      elapsed_str <- format_time(stats$elapsed_mins)
      timing_line <- sprintf("Elapsed: %s │ Rate: %4.1f models/min",
                             elapsed_str, stats$rate %||% 0)
      cat(sprintf("║ %-71s \n", timing_line))
    }

    # Best model
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

    # ETA (Best practice: quantile-based from observed per-model times)
    if (!is.na(stats$eta_info$eta_median)) {
      eta_str      <- format_time(stats$eta_info$eta_median)
      ci_lower_str <- format_time(stats$eta_info$eta_lower)
      ci_upper_str <- format_time(stats$eta_info$eta_upper)
      eta_line <- sprintf("ETA: %s (95%% CI: %s-%s)", eta_str, ci_lower_str, ci_upper_str)
      cat(sprintf("║ %-71s \n", eta_line))

      if (!is.null(stats$start_time)) {
        completion_time <- stats$start_time + (stats$elapsed_mins + stats$eta_info$eta_median) * 60
        completion_line <- sprintf("Expected completion: %s", format(completion_time, "%H:%M"))
        cat(sprintf("║ %-71s \n", completion_line))
      }
    }

    cat("╠══════════════════════════════════════════════════════════════════════\n")
    cat("║ Recent Models:                                                        \n")

    # Recent DONE lines
    done_lines <- grep("✓ DONE", lines, value = TRUE)
    if (length(done_lines) > 0) {
      recent <- tail(done_lines, show_recent)
      for (line in recent) {
        model_info <- .extract_model_label(line)
        rsq_val    <- .extract_numeric(line, "R²=([0-9]+\\.[0-9]+)")
        if (!is.na(rsq_val)) {
          model_line <- sprintf("%-50s R²=%.3f", substr(model_info, 1, 50), rsq_val)
          cat(sprintf("║   %-69s \n", model_line))
        } else {
          cat(sprintf("║   %-69s \n", substr(model_info, 1, 69)))
        }
      }
    }

    # Worker statistics
    worker_stats <- get_worker_statistics(log_file)
    if (!is.null(worker_stats) && nrow(worker_stats) > 0) {
      cat("╠══════════════════════════════════════════════════════════════════════\n")
      cat("║ Worker Statistics:                                                    \n")
      top_workers <- head(worker_stats, 4)
      for (i in seq_len(nrow(top_workers))) {
        w <- top_workers[i, ]
        denom <- (w$n_complete + w$n_failed)
        fail_rate <- if (denom > 0) round(100 * w$n_failed / denom, 1) else 0
        worker_line <- sprintf("W%02d: %3d done, %2d fail (%4.1f%%), avg %.1fs",
                               w$worker_id, w$n_complete, w$n_failed, fail_rate,
                               ifelse(is.na(w$avg_time), 0, w$avg_time))
        cat(sprintf("║   %-69s \n", worker_line))
      }
    }

    # Model timing histogram buckets
    cat("╠══════════════════════════════════════════════════════════════════════\n")
    cat("║ Model Timing:                                                         \n")
    mt <- stats$metrics_data$model_times
    if (length(mt) > 0) {
      fast_models   <- sum(mt < 10, na.rm = TRUE)
      medium_models <- sum(mt >= 10 & mt < 60, na.rm = TRUE)
      slow_models   <- sum(mt >= 60, na.rm = TRUE)
      total_timed   <- fast_models + medium_models + slow_models
      if (total_timed > 0) {
        fast_pct   <- round(100 * fast_models   / total_timed, 1)
        medium_pct <- round(100 * medium_models / total_timed, 1)
        slow_pct   <- round(100 * slow_models   / total_timed, 1)
        cat(sprintf("║   %-69s \n", sprintf("Fast (<10s): %3d models (%4.1f%%)", fast_models, fast_pct)))
        cat(sprintf("║   %-69s \n", sprintf("Medium (10-60s): %3d models (%4.1f%%)", medium_models, medium_pct)))
        cat(sprintf("║   %-69s \n", sprintf("Slow (>60s): %3d models (%4.1f%%)", slow_models, slow_pct)))
      }
    }

    # Model-specific average times
    if (length(done_lines) > 0) {
      model_times <- .collect_model_times(done_lines)
      if (nrow(model_times) > 0) {
        cat("╠══════════════════════════════════════════════════════════════════════\n")
        cat("║ Model Times (Average):                                               \n")
        model_avgs <- aggregate(time ~ model, data = model_times,
                                FUN = function(x) c(mean = mean(x), count = length(x)))
        model_avgs <- data.frame(model = model_avgs$model,
                                 avg_time = model_avgs$time[, "mean"],
                                 count = model_avgs$time[, "count"])
        model_avgs <- model_avgs[order(model_avgs$avg_time), ]
        top_models <- head(model_avgs, 6)
        for (i in seq_len(nrow(top_models))) {
          m <- top_models[i, ]
          model_line <- sprintf("%-12s: %.1fs avg (%d models)", m$model, m$avg_time, m$count)
          cat(sprintf("║   %-69s \n", model_line))
        }
      }
    }

    .banner_end()
    cat("\n")
    cat(sprintf("Last updated: %s | Refreshing every %d seconds...\n",
                format(Sys.time(), "%H:%M:%S"), refresh_seconds))
    Sys.sleep(refresh_seconds)
  }
}

# ---------- Helpers & Internals (kept internal on purpose) --------------------

# Internal, tolerant null coalescer
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# Critical: robust merged view of logs from a directory or a single file
.read_merged_log_lines <- function(path_or_dir) {
  if (isTRUE(dir.exists(path_or_dir))) {
    files <- unique(c(
      Sys.glob(file.path(path_or_dir, "parallel_*.log")),
      Sys.glob(file.path(path_or_dir, "master.log"))        # optional master log
    ))
    if (!length(files)) return(character())
    lines <- unlist(lapply(files, function(f) readLines(f, warn = FALSE)), use.names = FALSE)
  } else {
    if (!file.exists(path_or_dir)) return(character())
    lines <- readLines(path_or_dir, warn = FALSE)
  }
  if (!length(lines)) return(lines)
  # Order by leading timestamp "YYYY-MM-DD HH:MM:SS"
  ord <- order(substr(lines, 1L, 19L), decreasing = FALSE, na.last = TRUE)
  lines[ord]
}

.path_has_logs <- function(path_or_dir) {
  if (dir.exists(path_or_dir)) {
    length(Sys.glob(file.path(path_or_dir, "parallel_*.log"))) > 0 ||
      file.exists(file.path(path_or_dir, "master.log"))
  } else {
    file.exists(path_or_dir)
  }
}

# Parse first 19 chars into POSIXct (YYYY-MM-DD HH:MM:SS)
.parse_ts <- function(line) {
  ts <- substr(line, 1L, 19L)
  suppressWarnings(as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%S", tz = ""))
}

.extract_numeric <- function(line, pattern) {
  m <- regexec(pattern, line)
  r <- regmatches(line, m)[[1]]
  if (length(r) >= 2) suppressWarnings(as.numeric(r[2])) else NA_real_
}

.extract_model_label <- function(line) {
  # After "]: " up to " (" or end; tolerant
  m <- regexpr("\\]: (.*?)( \\(|$)", line, perl = TRUE)
  if (m[1] > 0) {
    out <- regmatches(line, m)
    out <- sub("^\\]: ", "", out)
    out <- sub(" \\($", "", out)
    out
  } else {
    "<unknown>"
  }
}

.collect_model_times <- function(done_lines) {
  # model = token after "]: " until underscore; time = trailing "... - X.Xs"
  mdl <- character(0); tm <- numeric(0)
  for (ln in done_lines) {
    model_token <- regmatches(ln, regexpr("\\]: [^_]+", ln))
    if (length(model_token) > 0) {
      model_type <- sub("\\]: ", "", model_token)
    } else {
      model_type <- "<unknown>"
    }
    tmatch <- regmatches(ln, regexpr("\\d+\\.\\d+s$", ln))
    if (length(tmatch) > 0) {
      time_val <- suppressWarnings(as.numeric(sub("s", "", tmatch)))
    } else {
      time_val <- NA_real_
    }
    mdl <- c(mdl, model_type); tm <- c(tm, time_val)
  }
  data.frame(model = mdl, time = tm, stringsAsFactors = FALSE)
}

# Core summarizer (Critical): computes totals, rates, ETA, best R^2, etc.
.summarize_from_lines <- function(lines) {
  if (!length(lines)) return(NULL)

  # Identify lines
  start_lines <- grep("START \\[", lines, value = TRUE)
  done_lines  <- grep("✓ DONE", lines, value = TRUE)
  fail_lines  <- grep("✗ FAIL", lines, value = TRUE)

  # Total models (prefer "[x/n]" parsed from any START line)
  total_models <- NA_integer_
  if (length(start_lines)) {
    # Take the max denominator observed
    ns <- .extract_numeric_from_bracket(start_lines)
    if (!is.null(ns$denom)) total_models <- max(ns$denom, na.rm = TRUE)
  }

  n_completed <- length(done_lines)
  n_failed    <- length(fail_lines)
  progress_pct <- if (!is.na(total_models) && total_models > 0) {
    round(100 * n_completed / total_models, 1)
  } else NA_real_

  # Timing
  ts_all <- .parse_ts(lines)
  ts_all <- ts_all[!is.na(ts_all)]
  start_time <- if (length(ts_all)) min(ts_all) else NULL
  elapsed_mins <- if (!is.null(start_time)) as.numeric(difftime(max(ts_all), start_time, units = "mins")) else NA_real_

  # Per-model runtimes (from DONE "... - X.Xs")
  mt <- numeric(0)
  if (length(done_lines)) {
    mt <- as.numeric(sub("s", "", regmatches(done_lines, regexpr("\\d+\\.\\d+s$", done_lines))))
    mt <- mt[is.finite(mt)]
  }
  rate <- if (length(mt)) (length(mt) / max(elapsed_mins, 1e-9)) else NA_real_

  # Best R^2 from DONE "R²=..." tokens
  rsq_vals <- suppressWarnings(as.numeric(sub("R²=", "", regmatches(done_lines, regexpr("R²=\\d+\\.\\d+", done_lines)))))
  best_rsq <- if (length(rsq_vals)) max(rsq_vals, na.rm = TRUE) else NA_real_

  # ETA (Best practice): based on quantiles of observed per-model seconds
  remaining <- if (!is.na(total_models)) max(total_models - n_completed - n_failed, 0L) else NA_integer_
  eta_info <- list(eta_median = NA_real_, eta_lower = NA_real_, eta_upper = NA_real_)
  if (length(mt) >= 3 && is.finite(remaining) && remaining > 0) {
    q <- quantile(mt, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
    eta_info$eta_lower  <- (q[[1]] * remaining) / 60
    eta_info$eta_median <- (q[[2]] * remaining) / 60
    eta_info$eta_upper  <- (q[[3]] * remaining) / 60
  }

  # Success rate
  success_rate <- if ((n_completed + n_failed) > 0) {
    round(100 * n_completed / (n_completed + n_failed), 1)
  } else NA_real_

  list(
    total_models  = total_models,
    n_completed   = n_completed,
    n_failed      = n_failed,
    progress_pct  = progress_pct,
    success_rate  = success_rate,
    elapsed_mins  = elapsed_mins,
    rate          = rate,
    best_rsq      = best_rsq,
    eta_info      = eta_info,
    start_time    = start_time,
    metrics_data  = list(model_times = mt)
  )
}

.extract_numeric_from_bracket <- function(start_lines) {
  # Parse "START [  12/345]: ..." -> num=12, denom=345 (take all, then max denom)
  nums <- regmatches(start_lines, gregexpr("\\[(\\s*\\d+)\\/(\\d+)\\]", start_lines))
  denoms <- integer(0)
  for (m in nums) {
    if (length(m) && !identical(m, -1L)) {
      g <- regexec("\\[(\\s*\\d+)\\/(\\d+)\\]", m)
      # Not needed; simpler: extract denominators via second capture
    }
  }
  # Simpler robust extraction:
  denoms <- as.integer(sub("\\]", "", sub("^.*\\/(\\d+)\\].*$", "\\1", start_lines)))
  denoms[is.na(denoms)] <- NA_integer_
  list(denom = denoms)
}

# ---------- Public utilities you already expose (updated to be dir-aware) -----

#' Create ASCII Progress Bar
#' @param current Current value
#' @param total Total value
#' @param width Width of progress bar in characters
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
#' @return String (e.g., "2h 15m" or "45m")
#' @export
format_time <- function(minutes) {
  if (is.na(minutes)) return("--:--")
  hours <- as.integer(floor(minutes / 60))
  mins  <- as.integer(round(minutes %% 60))
  if (hours > 0) sprintf("%dh %dm", hours, mins) else sprintf("%dm", mins)
}

#' Get Worker Statistics from Logs
#'
#' @param log_file Path to a single log file OR a directory of per-worker logs.
#' @return Data frame with columns: worker_id, n_started, n_complete, n_failed, avg_time
#' @export
get_worker_statistics <- function(log_file) {
  lines <- .read_merged_log_lines(log_file)
  if (!length(lines)) return(NULL)

  worker_pattern <- "\\[W(\\d+)\\]"

  start_lines <- grep("START.*\\[W\\d+\\]", lines, value = TRUE)
  done_lines  <- grep("✓ DONE.*\\[W\\d+\\]",  lines, value = TRUE)
  fail_lines  <- grep("✗ FAIL.*\\[W\\d+\\]",  lines, value = TRUE)

  # Collect unique workers
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
    times <- as.numeric(sub("s", "", regmatches(worker_done_lines, regexpr("\\d+\\.\\d+s", worker_done_lines))))
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

#' Track Best Model in Log (master-only writer, dir-aware)
#'
#' @description
#' If `log_file` is a directory, appends to `master.log` within it (single writer).
#' If it's a file, appends to that file. This function does **not** attempt to
#' coordinate with worker logs; it's intended for the master/orchestrator.
#'
#' @param log_file Path to a directory or a single file
#' @param model_id Model identifier
#' @param rsq R-squared value
#' @param previous_best Previous best R-squared (optional)
#' @export
log_new_best_model <- function(log_file, model_id, rsq, previous_best = NULL) {
  target <- if (dir.exists(log_file)) file.path(log_file, "master.log") else log_file
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)

  msg <- sprintf("🏆 NEW BEST MODEL: %s (R²=%.3f)", model_id, rsq)
  if (!is.null(previous_best)) {
    msg <- sprintf("%s - Previous best: %.3f", msg, previous_best)
  }
  line <- sprintf("%s[%s] %s", format(Sys.time(), "%F %T "), "W00", msg)
  cat(line, "\n", file = target, append = TRUE)
}
