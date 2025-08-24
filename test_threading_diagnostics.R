#!/usr/bin/env Rscript
#===============================================================================
# Script:  test_threading_diagnostics.R
# Purpose: Diagnose threading behavior in horizons package
# Author:  Threading Analysis
# Date:    2025-08-24
#===============================================================================

library(cli)

cli::cli_h1("Threading Diagnostics for Horizons Package")

# ------------------------------------------------------------------------------
# Step 1: Check current environment variables
# ------------------------------------------------------------------------------

cli::cli_h2("Current Thread Environment Variables")

thread_vars <- c(
  "OMP_NUM_THREADS",
  "OPENBLAS_NUM_THREADS", 
  "MKL_NUM_THREADS",
  "NUMEXPR_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS",
  "BLIS_NUM_THREADS",
  "GOTO_NUM_THREADS"
)

for (var in thread_vars) {
  val <- Sys.getenv(var, unset = "NOT SET")
  if (val != "NOT SET") {
    cli::cli_alert_warning("{var} = {val}")
  } else {
    cli::cli_alert_info("{var} = {val}")
  }
}

# ------------------------------------------------------------------------------
# Step 2: Check BLAS/LAPACK configuration
# ------------------------------------------------------------------------------

cli::cli_h2("BLAS/LAPACK Configuration")

# Check sessionInfo for BLAS/LAPACK
si <- sessionInfo()
cli::cli_alert_info("BLAS: {si$BLAS}")
cli::cli_alert_info("LAPACK: {si$LAPACK}")

# Check if RhpcBLASctl is available
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  cli::cli_alert_success("RhpcBLASctl available")
  # Note: get functions may not be available in all versions
  tryCatch({
    cli::cli_alert_info("Can set BLAS threads via RhpcBLASctl::blas_set_num_threads()")
    cli::cli_alert_info("Can set OMP threads via RhpcBLASctl::omp_set_num_threads()")
  }, error = function(e) {
    cli::cli_alert_info("RhpcBLASctl available for thread control")
  })
} else {
  cli::cli_alert_warning("RhpcBLASctl not available - cannot control thread counts")
}

# ------------------------------------------------------------------------------
# Step 3: Test ranger threading behavior
# ------------------------------------------------------------------------------

cli::cli_h2("Testing ranger Threading")

# Create small test data
set.seed(123)
n <- 100
p <- 50
x <- matrix(rnorm(n * p), n, p)
y <- rnorm(n)

# Test 1: Default ranger behavior
cli::cli_alert_info("Test 1: Default ranger (no num.threads specified)")
t1 <- system.time({
  rf1 <- ranger::ranger(
    y ~ ., 
    data = data.frame(y = y, x),
    num.trees = 100,
    verbose = FALSE
  )
})
cli::cli_alert_info("  Time: {round(t1[3], 3)}s")

# Test 2: Explicit single thread
cli::cli_alert_info("Test 2: ranger with num.threads = 1")
t2 <- system.time({
  rf2 <- ranger::ranger(
    y ~ ., 
    data = data.frame(y = y, x),
    num.trees = 100,
    num.threads = 1,
    verbose = FALSE
  )
})
cli::cli_alert_info("  Time: {round(t2[3], 3)}s")

# Test 3: Check what ranger reports as default
cli::cli_alert_info("Default ranger threads: {ranger::ranger(y ~ ., data = data.frame(y = y, x[,1:2]), num.trees = 1)$num.threads}")

# ------------------------------------------------------------------------------
# Step 4: Test Boruta threading
# ------------------------------------------------------------------------------

cli::cli_h2("Testing Boruta Threading")

if (requireNamespace("Boruta", quietly = TRUE)) {
  
  # Check Boruta's ranger usage
  cli::cli_alert_info("Testing Boruta with default settings...")
  
  # Temporarily set thread limits
  old_omp <- Sys.getenv("OMP_NUM_THREADS")
  old_blas <- Sys.getenv("OPENBLAS_NUM_THREADS")
  
  Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
  
  t3 <- system.time({
    suppressWarnings({
      boruta_test <- Boruta::Boruta(
        x = x,
        y = y,
        doTrace = 0,
        maxRuns = 15,  # Minimum is >10
        ntree = 50     # Few trees for testing
      )
    })
  })
  
  cli::cli_alert_info("  Boruta time with env vars set to 1: {round(t3[3], 3)}s")
  
  # Restore environment
  if (old_omp != "") Sys.setenv(OMP_NUM_THREADS = old_omp) else Sys.unsetenv("OMP_NUM_THREADS")
  if (old_blas != "") Sys.setenv(OPENBLAS_NUM_THREADS = old_blas) else Sys.unsetenv("OPENBLAS_NUM_THREADS")
  
} else {
  cli::cli_alert_warning("Boruta not installed - skipping test")
}

# ------------------------------------------------------------------------------
# Step 5: Test xgboost threading
# ------------------------------------------------------------------------------

cli::cli_h2("Testing xgboost Threading")

if (requireNamespace("xgboost", quietly = TRUE)) {
  
  # Test default xgboost
  cli::cli_alert_info("Test 1: Default xgboost (no nthread specified)")
  
  dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
  
  t4 <- system.time({
    xgb1 <- xgboost::xgb.train(
      params = list(objective = "reg:squarederror"),
      data = dtrain,
      nrounds = 10,
      verbose = 0
    )
  })
  cli::cli_alert_info("  Time: {round(t4[3], 3)}s")
  
  # Test with nthread = 1
  cli::cli_alert_info("Test 2: xgboost with nthread = 1")
  
  t5 <- system.time({
    xgb2 <- xgboost::xgb.train(
      params = list(objective = "reg:squarederror", nthread = 1),
      data = dtrain,
      nrounds = 10,
      verbose = 0
    )
  })
  cli::cli_alert_info("  Time: {round(t5[3], 3)}s")
  
} else {
  cli::cli_alert_warning("xgboost not installed - skipping test")
}

# ------------------------------------------------------------------------------
# Step 6: Test tidymodels/parsnip integration
# ------------------------------------------------------------------------------

cli::cli_h2("Testing tidymodels/parsnip Threading")

if (requireNamespace("tidymodels", quietly = TRUE)) {
  
  library(tidymodels)
  
  # Test ranger via parsnip
  cli::cli_alert_info("Testing ranger via parsnip...")
  
  rf_spec <- rand_forest(trees = 100) %>%
    set_engine("ranger") %>%
    set_mode("regression")
  
  t6 <- system.time({
    rf_fit <- rf_spec %>%
      fit(y ~ ., data = data.frame(y = y, x))
  })
  
  cli::cli_alert_info("  Default parsnip ranger time: {round(t6[3], 3)}s")
  
  # Check the actual ranger object
  ranger_obj <- rf_fit$fit
  cli::cli_alert_info("  Threads used: {ranger_obj$num.threads}")
  
  # Test with explicit thread control
  rf_spec_single <- rand_forest(trees = 100) %>%
    set_engine("ranger", num.threads = 1) %>%
    set_mode("regression")
  
  t7 <- system.time({
    rf_fit_single <- rf_spec_single %>%
      fit(y ~ ., data = data.frame(y = y, x))
  })
  
  cli::cli_alert_info("  Single-thread parsnip ranger time: {round(t7[3], 3)}s")
  cli::cli_alert_info("  Threads used: {rf_fit_single$fit$num.threads}")
  
} else {
  cli::cli_alert_warning("tidymodels not installed - skipping test")
}

# ------------------------------------------------------------------------------
# Step 7: Test parallel backend with future
# ------------------------------------------------------------------------------

cli::cli_h2("Testing future Parallel Backend")

if (requireNamespace("future", quietly = TRUE)) {
  
  library(future)
  
  # Check current plan
  current_plan <- plan()
  cli::cli_alert_info("Current future plan: {class(current_plan)[1]}")
  
  # Test multicore with thread limits
  if (Sys.info()["sysname"] == "Linux" || Sys.info()["sysname"] == "Darwin") {
    
    cli::cli_alert_info("Testing multicore backend...")
    
    plan(multicore, workers = 2)
    
    # Test if child processes inherit environment
    test_result <- future({
      list(
        omp = Sys.getenv("OMP_NUM_THREADS"),
        blas = Sys.getenv("OPENBLAS_NUM_THREADS"),
        pid = Sys.getpid()
      )
    })
    
    result <- value(test_result)
    cli::cli_alert_info("  Child process OMP_NUM_THREADS: {result$omp}")
    cli::cli_alert_info("  Child process OPENBLAS_NUM_THREADS: {result$blas}")
    cli::cli_alert_info("  Child PID: {result$pid}")
    
  }
  
  # Reset plan
  plan(sequential)
  
} else {
  cli::cli_alert_warning("future not installed - skipping test")
}

# ------------------------------------------------------------------------------
# Step 8: Summary and Recommendations
# ------------------------------------------------------------------------------

cli::cli_h1("Summary and Recommendations")

cli::cli_alert_info("Key Findings:")
cli::cli_bullets(c(
  "i" = "ranger defaults to using all available cores unless num.threads is specified",
  "i" = "Boruta uses ranger internally and inherits its threading behavior",
  "i" = "xgboost also defaults to all cores unless nthread is specified",
  "i" = "parsnip does not automatically set thread limits for engines",
  "i" = "Environment variables may not be sufficient for all packages"
))

cli::cli_alert_success("Diagnostics complete!")

# Output session info for reference
cli::cli_h2("Session Info")
print(sessionInfo())