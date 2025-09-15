#!/usr/bin/env Rscript

# ==============================================================================
# Worker Debug Output Monitor for Horizons HPC
# ==============================================================================
# This script provides additional debugging utilities for monitoring parallel
# worker output when standard stdout/stderr capture fails.
# ==============================================================================

library(cli)

#' Monitor Worker Debug Log
#'
#' Continuously displays the debug log file created by workers
#'
#' @param output_dir The output directory containing debug_worker_output.log
#' @param refresh_interval Seconds between refresh (default: 2)
monitor_worker_debug <- function(output_dir, refresh_interval = 2) {

  log_file <- file.path(output_dir, "debug_worker_output.log")

  if (!file.exists(log_file)) {
    cli::cli_alert_warning("Debug log not found: {log_file}")
    cli::cli_alert_info("Make sure evaluate_models_hpc() has been started")
    return(invisible(NULL))
  }

  cli::cli_h1("Monitoring Worker Debug Output")
  cli::cli_alert_info("Log file: {log_file}")
  cli::cli_alert_info("Press Ctrl+C to stop monitoring")
  cli::cli_rule()

  last_position <- 0

  while(TRUE) {
    # Read new lines from the file
    con <- file(log_file, "r")
    seek(con, last_position)
    new_lines <- readLines(con)
    current_position <- seek(con)
    close(con)

    # Display new lines if any
    if (length(new_lines) > 0) {
      for (line in new_lines) {
        cat(line, "\n")
      }
      last_position <- current_position
    }

    Sys.sleep(refresh_interval)
  }
}

#' Check Active R Processes
#'
#' Shows all R processes and their resource usage
check_r_processes <- function() {
  cli::cli_h2("Active R Processes")

  # Get process info
  ps_output <- system("ps aux | grep '[R]' | grep -v grep", intern = TRUE)

  if (length(ps_output) == 0) {
    cli::cli_alert_info("No R processes found")
    return(invisible(NULL))
  }

  # Parse and display
  cli::cli_alert_info("Found {length(ps_output)} R processes:")

  for (line in ps_output) {
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(parts) >= 11) {
      user <- parts[1]
      pid <- parts[2]
      cpu <- parts[3]
      mem <- parts[4]
      cmd <- paste(parts[11:length(parts)], collapse = " ")

      # Format output
      if (grepl("Rscript", cmd) || grepl("R CMD", cmd)) {
        cli::cli_text("  PID {pid}: CPU {cpu}% | MEM {mem}% | {substr(cmd, 1, 60)}")
      }
    }
  }

  # Count by CPU usage
  high_cpu <- sum(as.numeric(sapply(strsplit(ps_output, "\\s+"), function(x) x[3])) > 50, na.rm = TRUE)
  if (high_cpu > 0) {
    cli::cli_alert_warning("{high_cpu} processes using >50% CPU")
  }
}

#' Alternative Debug Setup for Workers
#'
#' Creates a more robust debugging setup using file-based logging
#' This function should be called INSIDE each worker
create_worker_logger <- function(worker_id, output_dir) {

  # Create a unique log file for this worker
  worker_log <- file.path(output_dir, sprintf("worker_%03d.log", worker_id))

  # Return a logging function
  function(msg, ...) {
    formatted_msg <- sprintf("[%s] [W%03d-PID:%d] %s\n",
                            format(Sys.time(), "%H:%M:%S.%OS3"),
                            worker_id,
                            Sys.getpid(),
                            sprintf(msg, ...))

    # Write to worker-specific file
    cat(formatted_msg, file = worker_log, append = TRUE)

    # Also write to main debug log
    main_log <- file.path(output_dir, "debug_worker_output.log")
    cat(formatted_msg, file = main_log, append = TRUE)

    # Try to force output
    tryCatch({
      message(formatted_msg)
      cat(formatted_msg, file = stderr())
      flush.console()
    }, error = function(e) NULL)
  }
}

#' Test Parallel Debug Output
#'
#' Quick test to verify debug output is working
test_parallel_debug <- function(outer = 2, inner = 2, output_dir = "test_debug") {

  cli::cli_h1("Testing Parallel Debug Output")

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Initialize debug log
  debug_log <- file.path(output_dir, "debug_worker_output.log")
  cat(sprintf("\n==== Debug Test Started: %s ====\n", Sys.time()), file = debug_log)

  cli::cli_alert_info("Testing with {outer} outer x {inner} inner workers")
  cli::cli_alert_info("Debug log: {debug_log}")

  # Setup futures
  future::plan(future::multisession, workers = outer)

  # Run test
  results <- furrr::future_map(
    .x = 1:4,
    .f = function(i) {
      # Create logger for this worker
      log_debug <- create_worker_logger(i, output_dir)

      log_debug("Worker started")
      log_debug("Setting up inner parallelization with %d cores", inner)

      # Configure inner parallelization
      doMC::registerDoMC(cores = inner)

      # Run inner parallel task
      inner_results <- foreach::foreach(j = 1:4, .combine = c) %dopar% {
        log_debug("Inner task %d running on thread", j)
        Sys.sleep(0.1)
        j * 2
      }

      log_debug("Worker completed with results: %s", paste(inner_results, collapse = ","))

      return(list(worker = i, pid = Sys.getpid(), results = inner_results))
    },
    .options = furrr::furrr_options(
      stdout = TRUE,
      conditions = "message"
    )
  )

  # Check results
  cli::cli_h2("Test Results")
  cli::cli_alert_success("All workers completed")

  # Check log file
  if (file.exists(debug_log)) {
    log_lines <- readLines(debug_log)
    cli::cli_alert_info("Debug log contains {length(log_lines)} lines")

    worker_lines <- grep("Worker", log_lines, value = TRUE)
    cli::cli_alert_info("Found {length(worker_lines)} worker messages")

    if (length(worker_lines) == 0) {
      cli::cli_alert_warning("No worker messages found - debug output may not be working")
    } else {
      cli::cli_alert_success("Debug output is working!")
      cli::cli_text("\nSample output:")
      cat(head(worker_lines, 5), sep = "\n")
    }
  }

  future::plan(future::sequential)

  return(invisible(results))
}

# ==============================================================================
# Usage Examples
# ==============================================================================

if (interactive()) {
  cli::cli_h1("Worker Debug Monitor Utilities")
  cli::cli_alert_info("Available functions:")
  cli::cli_ul(c(
    "monitor_worker_debug(output_dir) - Monitor debug log in real-time",
    "check_r_processes() - Check active R processes",
    "test_parallel_debug() - Test if debug output is working",
    "create_worker_logger(worker_id, output_dir) - Create logger for use in workers"
  ))

  cli::cli_h2("Quick Test")
  cli::cli_alert_info("Run: test_parallel_debug() to verify debug output")

  cli::cli_h2("Monitor Existing Run")
  cli::cli_alert_info("Run: monitor_worker_debug('your_output_dir') to watch logs")
}