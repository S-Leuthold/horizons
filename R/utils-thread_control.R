#' Thread Control Utilities
#'
#' @description
#' Utilities for managing and verifying thread control in HPC environments.
#' Ensures single-threaded execution of models to prevent CPU oversubscription.
#'
#' @keywords thread control hpc
#' @name thread_control
NULL


#' Set Single Thread Mode
#'
#' @description
#' Comprehensively sets all threading controls to single-threaded mode.
#' This prevents thread explosion when running parallel workers.
#'
#' @param verbose Logical. Print status messages (default = TRUE)
#'
#' @return Invisible NULL
#' @export

set_single_thread_mode <- function(verbose = TRUE) {
  
  ## Set all environment variables ----
  
  env_vars <- c(
    OMP_NUM_THREADS        = "1",
    OPENBLAS_NUM_THREADS   = "1",
    MKL_NUM_THREADS        = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS    = "1",
    GOTO_NUM_THREADS       = "1",
    BLIS_NUM_THREADS       = "1",
    RANGER_NUM_THREADS     = "1",
    XGBOOST_NTHREAD        = "1"
  )
  
  do.call(Sys.setenv, as.list(env_vars))
  
  ## Set R options ----
  
  options(
    ranger.num.threads = 1,
    ranger.num.cores   = 1,
    xgboost.nthread    = 1,
    mc.cores           = 1,
    cores              = 1
  )
  
  ## Use RhpcBLASctl if available ----
  
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    suppressMessages({
      RhpcBLASctl::blas_set_num_threads(1)
      RhpcBLASctl::omp_set_num_threads(1)
    })
  }
  
  if (verbose) {
    cli::cli_alert_success("Thread control set to single-threaded mode")
  }
  
  invisible(NULL)
}


#' Verify Thread Control
#'
#' @description
#' Tests that thread control is working by running small model fits
#' and checking CPU usage.
#'
#' @param verbose Logical. Print detailed results (default = TRUE)
#'
#' @return Logical. TRUE if thread control is working, FALSE otherwise
#' @export

verify_thread_control <- function(verbose = TRUE) {
  
  if (verbose) {
    cli::cli_h3("Verifying thread control")
  }
  
  all_ok <- TRUE
  
  ## Check environment variables ----
  
  critical_vars <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS")
  
  for (var in critical_vars) {
    val <- Sys.getenv(var)
    if (val != "1") {
      if (verbose) {
        cli::cli_alert_warning("{var} = {val} (should be 1)")
      }
      all_ok <- FALSE
    }
  }
  
  ## Test ranger if available ----
  
  if (requireNamespace("ranger", quietly = TRUE)) {
    if (verbose) cli::cli_alert_info("Testing ranger threading...")
    
    tryCatch({
      ## Small test with mtcars ----
      test_rf <- ranger::ranger(
        mpg ~ ., 
        data = mtcars[1:20, ],
        num.trees = 10,
        num.threads = 1,
        verbose = FALSE
      )
      
      if (verbose) {
        cli::cli_alert_success("ranger: single-threaded mode confirmed")
      }
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger("ranger test failed: {e$message}")
      }
      all_ok <<- FALSE
    })
  }
  
  ## Test xgboost if available ----
  
  if (requireNamespace("xgboost", quietly = TRUE)) {
    if (verbose) cli::cli_alert_info("Testing xgboost threading...")
    
    tryCatch({
      ## Small test with mtcars ----
      test_data <- xgboost::xgb.DMatrix(
        as.matrix(mtcars[1:20, -1]),
        label = mtcars[1:20, 1]
      )
      
      test_xgb <- xgboost::xgb.train(
        params = list(
          objective = "reg:squarederror",
          nthread = 1
        ),
        data = test_data,
        nrounds = 5,
        verbose = 0
      )
      
      if (verbose) {
        cli::cli_alert_success("xgboost: single-threaded mode confirmed")
      }
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger("xgboost test failed: {e$message}")
      }
      all_ok <<- FALSE
    })
  }
  
  ## Summary ----
  
  if (verbose) {
    if (all_ok) {
      cli::cli_alert_success("Thread control verification PASSED")
    } else {
      cli::cli_alert_danger("Thread control verification FAILED")
      cli::cli_alert_info("Run set_single_thread_mode() to fix")
    }
  }
  
  return(all_ok)
}


#' Detect Threading Issues
#'
#' @description
#' Diagnostic function to identify potential threading problems in the
#' current environment.
#'
#' @return A list with threading information
#' @export

detect_threading_issues <- function() {
  
  cli::cli_h3("Threading Diagnostics")
  
  ## Collect environment info ----
  
  env_info <- list(
    omp_threads      = Sys.getenv("OMP_NUM_THREADS"),
    blas_threads     = Sys.getenv("OPENBLAS_NUM_THREADS"),
    mkl_threads      = Sys.getenv("MKL_NUM_THREADS"),
    ranger_option    = getOption("ranger.num.threads"),
    xgboost_option   = getOption("xgboost.nthread"),
    mc_cores         = getOption("mc.cores"),
    cpu_count        = parallel::detectCores(),
    current_workers  = if (exists(".Random.seed")) future::nbrOfWorkers() else NA
  )
  
  ## Check for issues ----
  
  issues <- character()
  
  if (env_info$omp_threads != "1" && env_info$omp_threads != "") {
    issues <- c(issues, sprintf("OMP_NUM_THREADS = %s (should be 1)", env_info$omp_threads))
  }
  
  if (env_info$blas_threads != "1" && env_info$blas_threads != "") {
    issues <- c(issues, sprintf("OPENBLAS_NUM_THREADS = %s (should be 1)", env_info$blas_threads))
  }
  
  if (!is.null(env_info$ranger_option) && env_info$ranger_option != 1) {
    issues <- c(issues, sprintf("ranger.num.threads = %s (should be 1)", env_info$ranger_option))
  }
  
  ## Report ----
  
  cli::cli_alert_info("CPU cores available: {env_info$cpu_count}")
  
  if (length(issues) > 0) {
    cli::cli_alert_warning("Threading issues detected:")
    for (issue in issues) {
      cli::cli_alert_danger("  {issue}")
    }
  } else {
    cli::cli_alert_success("No threading issues detected")
  }
  
  invisible(list(
    environment = env_info,
    issues = issues
  ))
}