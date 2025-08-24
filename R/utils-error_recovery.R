#' Error Recovery for Nested Parallelization
#'
#' @description
#' Provides robust error handling and recovery mechanisms for nested parallel
#' execution. Includes error classification, retry strategies, and graceful
#' degradation.
#'
#' @name error_recovery
#' @keywords internal
NULL


#' Execute with Error Recovery
#'
#' @description
#' Wraps an expression with comprehensive error handling and retry logic.
#'
#' @param expr Expression to evaluate
#' @param model_id Model identifier for logging
#' @param max_retries Maximum retry attempts (default: 2)
#' @param retry_delay Delay between retries in seconds (default: 5)
#' @param resource_manager Optional resource manager for cleanup
#' @param verbose Print retry messages (default: TRUE)
#'
#' @return Result of expression or error object
#'
#' @keywords internal
with_error_recovery <- function(expr,
                                model_id,
                                max_retries = 2,
                                retry_delay = 5,
                                resource_manager = NULL,
                                verbose = TRUE) {
  
  retry_count <- 0
  last_error <- NULL
  
  while (retry_count <= max_retries) {
    
    result <- tryCatch({
      
      # Evaluate expression
      expr
      
    }, error = function(e) {
      
      # Classify error
      error_type <- classify_error(e)
      
      if (verbose) {
        cli::cli_alert_warning(
          "[Model {model_id}] Error ({error_type}): {conditionMessage(e)}"
        )
      }
      
      # Decide on recovery strategy
      recovery_strategy <- determine_recovery_strategy(
        error_type = error_type,
        retry_count = retry_count,
        max_retries = max_retries,
        resource_manager = resource_manager
      )
      
      if (recovery_strategy$should_retry) {
        
        if (verbose) {
          cli::cli_alert_info(
            "[Model {model_id}] Attempting recovery: {recovery_strategy$action}"
          )
        }
        
        # Execute recovery action
        execute_recovery_action(
          action = recovery_strategy$action,
          resource_manager = resource_manager
        )
        
        # Wait before retry
        Sys.sleep(retry_delay)
        
        # Store error for potential return
        last_error <<- e
        
        # Signal retry
        return(structure(list(), class = "retry_signal"))
        
      } else {
        
        # No retry - return error
        return(list(
          status = "error",
          error_type = error_type,
          error_message = conditionMessage(e),
          error_call = deparse(e$call),
          retry_count = retry_count
        ))
      }
    })
    
    # Check if we should retry
    if (!inherits(result, "retry_signal")) {
      return(result)
    }
    
    retry_count <- retry_count + 1
    
    if (verbose && retry_count <= max_retries) {
      cli::cli_alert_info(
        "[Model {model_id}] Retry attempt {retry_count}/{max_retries}"
      )
    }
  }
  
  # Max retries exceeded
  if (verbose) {
    cli::cli_alert_danger(
      "[Model {model_id}] Max retries exceeded. Failing."
    )
  }
  
  return(list(
    status = "failed",
    error_type = "max_retries",
    error_message = if (!is.null(last_error)) conditionMessage(last_error) else "Unknown error",
    retry_count = retry_count
  ))
}


#' Classify Error Type
#'
#' @description
#' Classifies an error into categories for appropriate handling.
#'
#' @param e Error condition
#'
#' @return Character string error type
#'
#' @keywords internal
classify_error <- function(e) {
  
  msg <- tolower(conditionMessage(e))
  call_str <- tolower(deparse(e$call))
  
  # Memory errors
  if (grepl("cannot allocate|memory|out of memory|vector size|protect|allocation", msg)) {
    return("memory")
  }
  
  # Timeout errors
  if (grepl("timeout|time limit|elapsed|timed out", msg)) {
    return("timeout")
  }
  
  # Connection/network errors
  if (grepl("connection|socket|port|network|refused", msg)) {
    return("network")
  }
  
  # File I/O errors
  if (grepl("cannot open|file|directory|permission|exist", msg)) {
    return("file_io")
  }
  
  # Numerical errors
  if (grepl("nan|inf|infinite|singular|convergence|numeric", msg)) {
    return("numerical")
  }
  
  # Package/function errors
  if (grepl("could not find|namespace|package|function", msg)) {
    return("package")
  }
  
  # Data errors
  if (grepl("missing|na|null|empty|no data|zero length", msg)) {
    return("data")
  }
  
  # Model-specific errors
  if (grepl("tune|recipe|workflow|model|fit", call_str)) {
    return("model")
  }
  
  # Default
  return("unknown")
}


#' Determine Recovery Strategy
#'
#' @description
#' Determines appropriate recovery strategy based on error type and context.
#'
#' @param error_type Character string error type
#' @param retry_count Current retry count
#' @param max_retries Maximum retries allowed
#' @param resource_manager Resource manager object
#'
#' @return List with recovery strategy
#'
#' @keywords internal
determine_recovery_strategy <- function(error_type,
                                       retry_count,
                                       max_retries,
                                       resource_manager = NULL) {
  
  # Don't retry if at max
  if (retry_count >= max_retries) {
    return(list(should_retry = FALSE))
  }
  
  strategy <- switch(
    error_type,
    
    memory = list(
      should_retry = TRUE,
      action = "memory_cleanup",
      delay_multiplier = 2  # Longer delay for memory issues
    ),
    
    timeout = list(
      should_retry = FALSE,  # Don't retry timeouts
      action = "skip"
    ),
    
    network = list(
      should_retry = retry_count < 1,  # Only retry once
      action = "wait_and_retry",
      delay_multiplier = 3
    ),
    
    file_io = list(
      should_retry = retry_count < 1,
      action = "check_paths",
      delay_multiplier = 1
    ),
    
    numerical = list(
      should_retry = FALSE,  # Likely a data issue
      action = "skip"
    ),
    
    package = list(
      should_retry = retry_count < 1,
      action = "reload_packages",
      delay_multiplier = 1
    ),
    
    data = list(
      should_retry = FALSE,  # Data issues won't fix themselves
      action = "skip"
    ),
    
    model = list(
      should_retry = retry_count < 1,
      action = "simplify_model",
      delay_multiplier = 1
    ),
    
    # Default
    list(
      should_retry = retry_count < 1,
      action = "general_cleanup",
      delay_multiplier = 1
    )
  )
  
  return(strategy)
}


#' Execute Recovery Action
#'
#' @description
#' Executes specific recovery actions based on strategy.
#'
#' @param action Character string action to execute
#' @param resource_manager Resource manager object
#'
#' @keywords internal
execute_recovery_action <- function(action, resource_manager = NULL) {
  
  switch(
    action,
    
    memory_cleanup = {
      # Aggressive memory cleanup
      gc(verbose = FALSE, full = TRUE, reset = TRUE)
      
      # Clear temporary objects
      if (exists(".Last.value", envir = .GlobalEnv)) {
        rm(.Last.value, envir = .GlobalEnv)
      }
      
      # Clear tempdir if too full
      temp_files <- list.files(tempdir(), full.names = TRUE)
      if (length(temp_files) > 100) {
        old_files <- temp_files[file.mtime(temp_files) < Sys.time() - 3600]
        unlink(old_files, recursive = TRUE, force = TRUE)
      }
      
      # If resource manager available, check memory
      if (!is.null(resource_manager)) {
        check_resource_availability(resource_manager, 1)
      }
    },
    
    wait_and_retry = {
      # Just wait - delay handled by caller
      Sys.sleep(1)
    },
    
    check_paths = {
      # Verify working directory
      if (!dir.exists(getwd())) {
        setwd("~")
      }
    },
    
    reload_packages = {
      # Try reloading key packages
      suppressMessages({
        if (requireNamespace("tidymodels", quietly = TRUE)) {
          # Key packages might need reloading
        }
      })
    },
    
    simplify_model = {
      # Could adjust model parameters here
      # For now, just cleanup
      gc(verbose = FALSE)
    },
    
    general_cleanup = {
      # General cleanup
      gc(verbose = FALSE)
    },
    
    skip = {
      # No action needed
    }
  )
  
  invisible(NULL)
}


#' Safe Model Evaluation Wrapper
#'
#' @description
#' Wraps model evaluation with comprehensive error handling tailored
#' for tidymodels workflows.
#'
#' @param workflow Tidymodels workflow object
#' @param data Training data
#' @param model_id Model identifier
#' @param ... Additional arguments passed to fit
#'
#' @return Fitted model or error object
#'
#' @keywords internal
safe_model_fit <- function(workflow, data, model_id, ...) {
  
  with_error_recovery(
    expr = {
      parsnip::fit(workflow, data = data, ...)
    },
    model_id = model_id,
    max_retries = 1,  # Usually don't retry model fits
    verbose = TRUE
  )
}


#' Safe Tuning Wrapper
#'
#' @description
#' Wraps hyperparameter tuning with error recovery.
#'
#' @param workflow Workflow to tune
#' @param resamples Resampling object
#' @param grid Grid of parameters or size
#' @param metrics Metric set
#' @param control Control object
#' @param model_id Model identifier
#'
#' @return Tuning results or error object
#'
#' @keywords internal
safe_tune_grid <- function(workflow,
                          resamples,
                          grid,
                          metrics,
                          control,
                          model_id) {
  
  with_error_recovery(
    expr = {
      tune::tune_grid(
        object = workflow,
        resamples = resamples,
        grid = grid,
        metrics = metrics,
        control = control
      )
    },
    model_id = model_id,
    max_retries = 2,
    retry_delay = 10,
    verbose = TRUE
  )
}


#' Create Error Report
#'
#' @description
#' Creates a detailed error report for failed models.
#'
#' @param errors List of error objects
#' @param output_dir Output directory for report
#'
#' @return Path to error report file
#'
#' @keywords internal
create_error_report <- function(errors, output_dir) {
  
  if (length(errors) == 0) {
    return(NULL)
  }
  
  # Summarize errors by type
  error_summary <- table(sapply(errors, function(e) e$error_type))
  
  # Create report
  report <- list(
    timestamp = Sys.time(),
    total_errors = length(errors),
    error_types = as.list(error_summary),
    errors = errors
  )
  
  # Save report
  report_file <- file.path(output_dir, "error_report.json")
  jsonlite::write_json(
    report,
    report_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  # Print summary
  cli::cli_h3("Error Summary")
  cli::cli_alert_warning("Total errors: {length(errors)}")
  
  for (error_type in names(error_summary)) {
    count <- error_summary[[error_type]]
    cli::cli_alert_info("{error_type}: {count}")
  }
  
  cli::cli_alert_info("Detailed report saved to: {report_file}")
  
  return(report_file)
}


#' Validate Model Results
#'
#' @description
#' Validates model results for common issues.
#'
#' @param results Model results object
#' @param model_id Model identifier
#'
#' @return Logical indicating validity
#'
#' @keywords internal
validate_model_results <- function(results, model_id) {
  
  # Check for required fields
  if (!all(c("status", "row_index", "wflow_id") %in% names(results))) {
    cli::cli_alert_warning("[Model {model_id}] Missing required fields in results")
    return(FALSE)
  }
  
  # Check metrics if successful
  if (results$status == "success") {
    if (!all(c("rmse", "rsq", "mae") %in% names(results$metrics))) {
      cli::cli_alert_warning("[Model {model_id}] Missing metrics in successful result")
      return(FALSE)
    }
    
    # Check for invalid metrics
    if (is.na(results$metrics$rmse) || 
        is.infinite(results$metrics$rmse) ||
        results$metrics$rmse < 0) {
      cli::cli_alert_warning("[Model {model_id}] Invalid RMSE value")
      return(FALSE)
    }
    
    if (is.na(results$metrics$rsq) || 
        results$metrics$rsq < 0 || 
        results$metrics$rsq > 1) {
      cli::cli_alert_warning("[Model {model_id}] Invalid R² value")
      return(FALSE)
    }
  }
  
  return(TRUE)
}


#' Graceful Degradation Handler
#'
#' @description
#' Handles graceful degradation when resources are constrained.
#'
#' @param resource_manager Resource manager object
#' @param requested_workers Requested number of workers
#'
#' @return Adjusted number of workers
#'
#' @keywords internal
handle_graceful_degradation <- function(resource_manager, requested_workers) {
  
  # Check current resource usage
  current_memory_gb <- get_current_memory_gb()
  available_memory_gb <- resource_manager$memory_limit_gb - current_memory_gb
  
  # Check if we need to reduce workers
  if (available_memory_gb < resource_manager$memory_limit_gb * 0.3) {
    
    # Reduce workers by half
    adjusted_workers <- max(1, floor(requested_workers / 2))
    
    cli::cli_alert_warning(
      "Memory pressure detected. Reducing workers from {requested_workers} to {adjusted_workers}"
    )
    
    return(adjusted_workers)
  }
  
  # Check CPU load
  load_avg <- get_load_average()
  
  if (load_avg > resource_manager$total_cores * 1.5) {
    
    # Reduce workers by 25%
    adjusted_workers <- max(1, floor(requested_workers * 0.75))
    
    cli::cli_alert_warning(
      "High CPU load detected. Reducing workers from {requested_workers} to {adjusted_workers}"
    )
    
    return(adjusted_workers)
  }
  
  return(requested_workers)
}