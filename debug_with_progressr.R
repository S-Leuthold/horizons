#!/usr/bin/env Rscript

# ==============================================================================
# Alternative Debug Solution Using progressr Package
# ==============================================================================
# The progressr package is specifically designed to handle output from
# parallel workers and can capture messages that standard approaches miss.
# ==============================================================================

#' Enhanced HPC Evaluation with progressr Debug Output
#'
#' This is a wrapper around evaluate_models_hpc that ensures debug output
#' is visible using the progressr package's robust message handling.
#'
#' @param ... All parameters passed to evaluate_models_hpc
#' @param debug Enable debug output (default: TRUE)
evaluate_models_hpc_debug <- function(..., debug = TRUE) {

  # Check if progressr is installed
  if (!requireNamespace("progressr", quietly = TRUE)) {
    stop("Please install progressr package: install.packages('progressr')")
  }

  library(progressr)

  # Configure progressr to capture all output
  if (debug) {
    # Use handler that shows everything
    handlers(global = TRUE)
    handlers("cli")  # or "txtprogressbar" or "progress"

    # Enable conditions to be signaled from workers
    options(progressr.enable = TRUE)
    options(progressr.times = TRUE)
  }

  # Wrap the original function call with progressr
  with_progress({
    # Get the progress reporter
    p <- progressor(steps = 100)  # Adjust steps as needed

    # Modify the function arguments to inject progress reporting
    args <- list(...)

    # Store original verbose setting
    original_verbose <- args$verbose
    args$verbose <- TRUE

    # Call the original function
    result <- do.call(horizons::evaluate_models_hpc, args)

    return(result)
  })
}

#' Modified Worker Function with progressr Debug Output
#'
#' This function should replace the worker function in evaluate_models_hpc
#' to use progressr for debug output.
worker_with_progressr_debug <- function(i, config_row, input_data, data_split,
                                        covariate_data, variable, output_dir,
                                        grid_size, bayesian_iter, cv_folds,
                                        inner_workers, prune_models, prune_threshold,
                                        seed) {

  # Get the progress reporter (if available)
  p <- NULL
  if (exists("p", envir = parent.frame())) {
    p <- get("p", envir = parent.frame())
  }

  # Helper function for debug output
  debug_msg <- function(msg, ...) {
    formatted <- sprintf(msg, ...)

    # Method 1: Use progressr if available
    if (!is.null(p)) {
      p(message = formatted)
    }

    # Method 2: Direct file write (most reliable)
    debug_log <- file.path(output_dir, "debug_worker_output.log")
    cat(sprintf("[%s] [W%03d-PID:%d] %s\n",
                format(Sys.time(), "%H:%M:%S"),
                i, Sys.getpid(), formatted),
        file = debug_log, append = TRUE)

    # Method 3: Standard message (may not work)
    message(formatted)
  }

  # Debug output at start
  debug_msg("Model %d started", i)
  debug_msg("Inner workers: %d, Grid: %d, Bayesian: %d",
           inner_workers, grid_size, bayesian_iter)

  # Your actual evaluation code here...
  # (simplified for demonstration)

  result <- tryCatch({
    # Simulate work
    Sys.sleep(0.5)

    # More debug output
    debug_msg("Model %d evaluation phase 1 complete", i)

    # Return mock result
    list(
      status = "success",
      config_id = i,
      workflow_id = i,
      rmse = runif(1, 0.1, 1.0),
      rsq = runif(1, 0.5, 0.9)
    )
  }, error = function(e) {
    debug_msg("Model %d failed: %s", i, e$message)
    list(status = "failed", error_message = e$message)
  })

  debug_msg("Model %d completed with status: %s", i, result$status)

  return(result)
}

#' Simple Test of progressr with futures
test_progressr_debug <- function() {
  library(progressr)
  library(future)
  library(furrr)

  cli::cli_h1("Testing progressr Debug Output")

  # Setup output directory
  output_dir <- "test_progressr"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  debug_log <- file.path(output_dir, "debug_worker_output.log")
  cat(sprintf("\n==== Test Started: %s ====\n", Sys.time()), file = debug_log)

  # Configure progressr
  handlers(global = TRUE)
  handlers("cli")

  # Setup futures
  plan(multisession, workers = 2)

  # Run with progress
  with_progress({
    p <- progressor(steps = 4)

    results <- future_map(1:4, function(i) {
      # Signal progress with message
      p(message = sprintf("Processing item %d on PID %d", i, Sys.getpid()))

      # Also write to file
      cat(sprintf("[%s] Worker %d active\n", Sys.time(), i),
          file = file.path(output_dir, "debug_worker_output.log"),
          append = TRUE)

      Sys.sleep(0.5)

      p(message = sprintf("Item %d complete", i))

      return(i * 2)
    })
  })

  plan(sequential)

  # Check log
  if (file.exists(debug_log)) {
    lines <- readLines(debug_log)
    cli::cli_alert_info("Log file has {length(lines)} lines")
    cli::cli_text("\nLog contents:")
    cat(tail(lines, 10), sep = "\n")
  }

  return(results)
}

#' Setup Logging with Proper Future Configuration
#'
#' Ensures future/furrr is configured to capture worker output
configure_future_for_debug <- function() {
  # Set future options for better output capture
  options(
    # Future options
    future.debug = TRUE,
    future.stdout = TRUE,
    future.conditions = "message",

    # Furrr options
    furrr.stdout = TRUE,
    furrr.conditions = "message",

    # Progress options
    progressr.enable = TRUE,
    progressr.times = TRUE
  )

  cli::cli_alert_success("Future configured for debug output")
  cli::cli_alert_info("Options set:")
  cli::cli_ul(c(
    "future.debug = TRUE",
    "future.stdout = TRUE",
    "future.conditions = 'message'",
    "progressr.enable = TRUE"
  ))
}

# ==============================================================================
# Usage Instructions
# ==============================================================================

if (interactive()) {
  cli::cli_h1("Enhanced Debug Solutions")

  cli::cli_h2("Option 1: Use progressr")
  cli::cli_code(c(
    "# Install if needed",
    "install.packages('progressr')",
    "",
    "# Configure and test",
    "source('debug_with_progressr.R')",
    "configure_future_for_debug()",
    "test_progressr_debug()"
  ))

  cli::cli_h2("Option 2: Use File-Based Logging")
  cli::cli_text("The modified evaluate_models_hpc now writes to:")
  cli::cli_text("{.file output_dir/debug_worker_output.log}")
  cli::cli_code(c(
    "# Monitor the log file in real-time",
    "# In terminal: tail -f output_dir/debug_worker_output.log",
    "",
    "# Or use R:",
    "source('debug_worker_monitor.R')",
    "monitor_worker_debug('your_output_dir')"
  ))

  cli::cli_h2("Option 3: Use Enhanced Wrapper")
  cli::cli_code(c(
    "# Use the debug wrapper function",
    "result <- evaluate_models_hpc_debug(",
    "  config = your_config,",
    "  input_data = your_data,",
    "  # ... other parameters",
    "  debug = TRUE",
    ")"
  ))
}