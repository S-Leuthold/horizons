#!/usr/bin/env Rscript

# Test script to demonstrate and fix the closure issue

library(cli)

cli::cli_h1("Testing Closure Issue and Fix")

# The problematic pattern - capturing environments
cli::cli_h2("Problematic Pattern")

problematic_safely <- function(expr) {
  expr_quo <- rlang::enquo(expr)
  caller_env <- rlang::caller_env()  # This captures the entire calling environment!
  
  safe_eval <- purrr::safely(function() {
    rlang::eval_tidy(expr_quo, env = caller_env)
  })
  
  safe_eval()
}

# The fix - avoid capturing unnecessary environments
cli::cli_h2("Fixed Pattern")

fixed_safely <- function(expr, data = NULL) {
  # Instead of capturing the entire environment, pass data explicitly
  expr_quo <- rlang::enquo(expr)
  
  safe_eval <- purrr::safely(function() {
    if (!is.null(data)) {
      # Evaluate in a clean environment with only the data
      rlang::eval_tidy(expr_quo, data = data)
    } else {
      # Evaluate in a minimal environment
      rlang::eval_tidy(expr_quo, env = rlang::empty_env())
    }
  })
  
  safe_eval()
}

# Alternative fix - use standard evaluation without capturing environments
cli::cli_h2("Alternative Fix - Standard Evaluation")

standard_safely <- function(fn, ...) {
  # Use standard function calls without environment capture
  purrr::safely(fn, otherwise = NULL, quiet = TRUE)(...)
}

# Test in parallel context
cli::cli_h2("Testing in Parallel Context")

library(future)
library(future.apply)

# Create some test data
test_data <- data.frame(x = 1:10, y = rnorm(10))

# Test function that would be called in parallel
test_function <- function(data, method) {
  if (method == "problematic") {
    # This might fail in parallel due to environment capture
    result <- problematic_safely({
      mean(data$y)
    })
  } else if (method == "fixed") {
    # This should work in parallel
    result <- fixed_safely({
      mean(y)
    }, data = data)
  } else {
    # Standard approach
    result <- standard_safely(function(d) mean(d$y), data)
  }
  
  return(result)
}

# Test with multicore backend (where closure errors occur)
plan(multicore, workers = 2)

cli::cli_alert_info("Testing with multicore backend...")

tryCatch({
  # This might fail with closure error
  results_prob <- future_lapply(1:3, function(i) {
    test_function(test_data, "problematic")
  })
  cli::cli_alert_success("Problematic pattern worked (surprising!)")
}, error = function(e) {
  cli::cli_alert_danger("Problematic pattern failed: {e$message}")
})

tryCatch({
  # This should work
  results_fixed <- future_lapply(1:3, function(i) {
    test_function(test_data, "fixed")
  })
  cli::cli_alert_success("Fixed pattern worked!")
}, error = function(e) {
  cli::cli_alert_danger("Fixed pattern failed: {e$message}")
})

tryCatch({
  # This should also work
  results_standard <- future_lapply(1:3, function(i) {
    test_function(test_data, "standard")
  })
  cli::cli_alert_success("Standard pattern worked!")
}, error = function(e) {
  cli::cli_alert_danger("Standard pattern failed: {e$message}")
})

# Reset to sequential
plan(sequential)

cli::cli_rule()
cli::cli_h2("Recommended Fix for horizons Package")

cli::cli_text("The issue is in utils-errors.R where safely_execute captures caller_env.")
cli::cli_text("This environment capture causes serialization issues in parallel workers.")
cli::cli_text("")
cli::cli_text("Options to fix:")
cli::cli_bullets(c(
  "1" = "Modify safely_execute to avoid capturing caller_env",
  "2" = "Pass data explicitly rather than relying on environment capture",
  "3" = "Use standard evaluation for functions called in parallel",
  "4" = "Switch from multicore to multisession backend on HPC"
))

cli::cli_text("")
cli::cli_alert_info("The easiest fix is option 4: use multisession instead of multicore on HPC.")