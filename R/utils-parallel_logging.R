# ---------- Shared helpers (internal) -----------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

.is_dir <- function(path) isTRUE(dir.exists(path))

.worker_log_path <- function(log_path) {
  # If a directory is given, write to per-PID file inside it
  if (.is_dir(log_path)) {
    file.path(log_path, sprintf("parallel_%d.log", Sys.getpid()))
  } else {
    # Back-compat: treat as a single file path
    log_path
  }
}

.master_log_path <- function(log_dir) {
  if (.is_dir(log_dir)) file.path(log_dir, "master.log") else log_dir
}

.read_merged_lines <- function(path_or_dir) {
  if (.is_dir(path_or_dir)) {
    files <- unique(c(
      Sys.glob(file.path(path_or_dir, "parallel_*.log")),
      file.path(path_or_dir, "master.log")
    ))
    files <- files[file.exists(files)]
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

.parse_ts <- function(line) {
  ts <- substr(line, 1L, 19L)
  suppressWarnings(as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%S", tz = ""))
}

# ---------- API ----------------------------------------------------------------

#' Initialize Parallel Progress Logs
#'
#' @description
#' Initializes a logging destination for parallel runs. If a **directory** is
#' provided, creates it (if needed) and writes a header to `master.log`. If a
#' **file** path is provided, initializes that single file (backward compatible).
#'
#' @param log_file Character. Path to **directory** (recommended) or single file.
#'   Default: `file.path(tempdir(), "parallel_models")`.
#' @param n_models Integer. Total number of models (optional, for header).
#' @return Character. The path you should pass around (directory or file).
#' @importFrom cli cli_alert_success
#' @export
init_parallel_log <- function(log_file = NULL, n_models = NULL) {
  if (is.null(log_file)) {
    # Default to a directory
    log_file <- file.path(tempdir(), "parallel_models")
  }

  if (.is_dir(log_file) || !grepl("\\.log$", basename(log_file), ignore.case = TRUE)) {
    # Treat as directory; ensure exists
    dir.create(log_file, recursive = TRUE, showWarnings = FALSE)
    target <- file.path(log_file, "master.log")
  } else {
    # Treat as single file
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    target <- log_file
  }

  header <- paste0(
    "==============================================\n",
    "Parallel Model Evaluation Log\n",
    "Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    if (!is.null(n_models)) paste0("Total models: ", n_models, "\n") else "",
    "==============================================\n\n"
  )

  cat(header, file = target, append = FALSE)

  cli::cli_alert_success("Log initialized at: {if (.is_dir(log_file)) paste0(log_file, '/master.log') else log_file}")
  return(log_file)
}

#' Simple, Directory-Aware Logger for Parallel Workers
#'
#' @description
#' Appends a log line to a per-worker file when given a directory, or to a single
#' file when given a file path (back-compat). Safe to call in parallel workers.
#'
#' @param message Character. Message to log.
#' @param log_file Character. Path to **directory** (recommended) or single file.
#'   Default: `file.path(tempdir(), "parallel_models")`.
#' @param worker_id Integer. Optional worker identifier to include (e.g., `Sys.getpid() %% 100`).
#' @param include_timestamp Logical. Include `YYYY-mm-dd HH:MM:SS` timestamp (default TRUE).
#' @export
log_parallel_progress <- function(message,
                                  log_file = NULL,
                                  worker_id = NULL,
                                  include_timestamp = TRUE) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_models")  # directory default
  }
  target <- .worker_log_path(log_file)

  ts <- if (isTRUE(include_timestamp)) sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")) else ""
  wid <- if (!is.null(worker_id)) sprintf(" [W%02d]", as.integer(worker_id)) else ""
  line <- sprintf("%s%s%s\n", ts, message, wid)

  # Best effort write; don't crash workers on I/O issues
  tryCatch({
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    cat(line, file = target, append = TRUE)
  }, error = function(e) invisible(NULL))
  invisible(NULL)
}

#' Tail Parallel Progress Logs
#'
#' @description
#' Shows the last N lines of the logs from a directory (merging all worker files)
#' or from a single file if a file path is given.
#'
#' @param n Integer. Number of lines to show (default 20)
#' @param log_file Character. Path to directory or file. Default: directory under `tempdir()`.
#' @export
tail_parallel_log <- function(n = 20, log_file = NULL) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_models")
  }
  lines <- .read_merged_lines(log_file)
  if (!length(lines)) {
    cli::cli_alert_success("No log data yet at: {log_file}")
    return(invisible(NULL))
  }
  n_lines <- length(lines)
  start_line <- max(1, n_lines - n + 1)
  recent <- lines[start_line:n_lines]
  cat(paste(recent, collapse = "\n"), "\n")
  invisible(recent)
}

#' Summarize Parallel Progress
#'
#' @description
#' Analyzes logs (directory or file) to produce counts, rates, ETA, and best R².
#' Compatible with per-worker logs written by `log_parallel_progress()`.
#'
#' @param log_file Character. Path to directory or file. Default: directory under `tempdir()`.
#' @param return_details Logical. Return a list of detailed stats (default FALSE).
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_alert_danger cli_h3
#' @export
summarize_parallel_progress <- function(log_file = NULL, return_details = FALSE) {
  if (is.null(log_file)) {
    log_file <- file.path(tempdir(), "parallel_models")
  }

  lines <- .read_merged_lines(log_file)
  if (!length(lines)) {
    cli::cli_alert_warning("No log data found at: {log_file}")
    return(invisible(NULL))
  }

  start_lines <- grep("START \\[", lines, value = TRUE)
  done_lines  <- grep("✓ DONE \\[", lines, value = TRUE)
  fail_lines  <- grep("✗ FAIL \\[", lines, value = TRUE)

  n_started   <- length(start_lines)
  n_completed <- length(done_lines)
  n_failed    <- length(fail_lines)

  # Total models: take max denominator from any START "[ i/Total ]"
  total_models <- NA_integer_
  if (length(start_lines)) {
    # extract "/Total]" piece
    totals <- suppressWarnings(as.integer(sub("^.*\\/(\\d+)\\].*$", "\\1", start_lines)))
    totals <- totals[is.finite(totals)]
    if (length(totals)) total_models <- max(totals, na.rm = TRUE)
  }

  # Metrics & times from DONE lines
  rsq_values <- suppressWarnings(as.numeric(sub("R²=", "", regmatches(done_lines, regexpr("R²=\\d+\\.\\d+", done_lines)))))
  rsq_values <- rsq_values[is.finite(rsq_values)]
  model_times <- suppressWarnings(as.numeric(sub("s", "", regmatches(done_lines, regexpr("\\d+\\.\\d+s", done_lines)))))
  model_times <- model_times[is.finite(model_times)]

  # Timing window
  ts <- .parse_ts(lines); ts <- ts[!is.na(ts)]
  start_time   <- if (length(ts)) min(ts) else NULL
  last_time    <- if (length(ts)) max(ts) else NULL
  elapsed_mins <- if (!is.null(start_time) && !is.null(last_time)) as.numeric(difftime(last_time, start_time, units = "mins")) else NA_real_

  progress_pct <- if (!is.na(total_models) && total_models > 0) round(100 * n_completed / total_models, 1) else NA_real_
  success_rate <- if ((n_completed + n_failed) > 0) round(100 * n_completed / (n_completed + n_failed), 1) else NA_real_
  rate         <- if (n_completed > 0 && is.finite(elapsed_mins) && elapsed_mins > 0) n_completed / elapsed_mins else NA_real_
  best_rsq     <- if (length(rsq_values)) max(rsq_values, na.rm = TRUE) else NA_real_

  # ETA via quantiles if enough samples; else ±20% bands around mean
  remaining <- if (!is.na(total_models)) max(total_models - n_completed - n_failed, 0L) else NA_integer_
  eta <- list(eta_median = NA_real_, eta_lower = NA_real_, eta_upper = NA_real_)
  if (length(model_times) >= 5 && is.finite(remaining) && remaining > 0) {
    qs  <- quantile(model_times, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
    eta$eta_lower  <- (qs[[1]] * remaining) / 60
    eta$eta_median <- (qs[[2]] * remaining) / 60
    eta$eta_upper  <- (qs[[3]] * remaining) / 60
  } else if (is.finite(remaining) && remaining > 0 && is.finite(elapsed_mins) && elapsed_mins > 0 && n_completed > 0) {
    avg <- (elapsed_mins * 60) / n_completed
    eta$eta_median <- (avg * remaining) / 60
    eta$eta_lower  <- eta$eta_median * 0.8
    eta$eta_upper  <- eta$eta_median * 1.2
  }

  # Display summary
  cli::cli_h3("Progress Summary")
  if (!is.na(total_models)) cli::cli_alert_info("Progress: {n_completed}/{total_models} ({progress_pct}%)")
  cli::cli_alert_success("Completed: {n_completed}")
  cli::cli_alert_danger("Failed: {n_failed}")
  if (!is.na(success_rate)) cli::cli_alert_info("Success rate: {success_rate}%")
  if (!is.na(elapsed_mins)) cli::cli_alert_info("Elapsed: {round(elapsed_mins, 1)} minutes")
  if (!is.na(rate))         cli::cli_alert_info("Rate: {round(rate, 2)} models/minute")
  if (!is.na(best_rsq))     cli::cli_alert_success("Best R² so far: {round(best_rsq, 3)}")
  if (!is.na(eta$eta_median))
    cli::cli_alert_info("ETA: {round(eta$eta_median,1)} min (95% CI: {round(eta$eta_lower,1)}–{round(eta$eta_upper,1)} min)")

  if (isTRUE(return_details)) {
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
      metrics_data = list(rsq_values = rsq_values, model_times = model_times),
      eta_info = eta,
      start_time = start_time
    ))
  }

  invisible(NULL)
}
