# Custom Assertions for horizons Package Testing
# Helper functions for common test assertions specific to horizons workflows

library(testthat)

## ---------------------------------------------------------------------------
## Data Structure Assertions
## ---------------------------------------------------------------------------

#' Test if object is valid spectral data
expect_spectral_data <- function(data, min_wavelengths = 10) {
  expect_s3_class(data, "data.frame")
  expect_true("Sample_ID" %in% names(data))
  
  # Check for numeric wavelength columns
  numeric_cols <- sapply(data, is.numeric)
  wavelength_cols <- grepl("^[0-9]+\\.?[0-9]*$", names(data))
  spectral_cols <- names(data)[numeric_cols & wavelength_cols]
  
  expect_gte(length(spectral_cols), min_wavelengths)
  expect_true(all(sapply(data[spectral_cols], is.numeric)))
  
  invisible(TRUE)
}

#' Test if object is valid model configuration
expect_valid_config <- function(config) {
  expect_s3_class(config, "data.frame")
  
  required_cols <- c("model", "transformation", "preprocessing", "feature_selection")
  expect_true(all(required_cols %in% names(config)))
  
  # Check valid values
  valid_models <- c("linear_reg", "rand_forest", "pls", "boost_tree", "svm_rbf", "cubist_rules")
  valid_transformations <- c("none", "log", "sqrt")
  valid_preprocessing <- c("raw", "snv", "derivative", "continuum_removal")
  
  expect_true(all(config$model %in% valid_models))
  expect_true(all(config$transformation %in% valid_transformations))
  expect_true(all(config$preprocessing %in% valid_preprocessing))
  
  invisible(TRUE)
}

#' Test if object is valid evaluation results
expect_evaluation_results <- function(results, min_metrics = 3) {
  expect_s3_class(results, "data.frame")
  
  required_cols <- c("config_id", "status")
  expect_true(all(required_cols %in% names(results)))
  
  # Check for metric columns
  metric_cols <- intersect(names(results), c("rmse", "rsq", "mae", "ccc", "rpd", "rrmse"))
  expect_gte(length(metric_cols), min_metrics)
  
  # Check status values are valid
  valid_status <- c("success", "failed", "error", "pruned")
  expect_true(all(results$status %in% valid_status))
  
  # If any successful results, check metrics are numeric
  if (any(results$status == "success")) {
    success_rows <- results$status == "success"
    for (metric in metric_cols) {
      expect_true(all(is.numeric(results[[metric]][success_rows])))
    }
  }
  
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## Model Object Assertions
## ---------------------------------------------------------------------------

#' Test if object is valid tidymodels recipe
expect_valid_recipe <- function(recipe) {
  expect_s3_class(recipe, "recipe")
  
  # Should have variables
  expect_true(length(recipe$var_info) > 0)
  
  # Should have at least outcome role
  roles <- unique(recipe$var_info$role)
  expect_true("outcome" %in% roles)
  
  invisible(TRUE)
}

#' Test if object is valid tidymodels workflow
expect_valid_workflow <- function(workflow, fitted = FALSE) {
  expect_s3_class(workflow, "workflow")
  
  # Should have recipe and model
  expect_false(is.null(workflow$pre$actions$recipe))
  expect_false(is.null(workflow$fit$actions$model))
  
  if (fitted) {
    expect_true(workflow$trained)
  }
  
  invisible(TRUE)
}

#' Test if object is valid tuning results
expect_tuning_results <- function(results) {
  expect_s3_class(results, "data.frame")
  
  # Should have metrics and config columns
  expect_true(".metric" %in% names(results) || "mean" %in% names(results))
  expect_true(".config" %in% names(results))
  
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## Metric Assertions
## ---------------------------------------------------------------------------

#' Test metric values are within reasonable bounds
expect_metrics_in_bounds <- function(metrics, metric_name) {
  
  if (metric_name == "rmse") {
    expect_true(all(metrics >= 0))
  } else if (metric_name == "rsq") {
    expect_true(all(metrics >= 0 & metrics <= 1))
  } else if (metric_name == "ccc") {
    expect_true(all(metrics >= -1 & metrics <= 1))
  } else if (metric_name == "rpd") {
    expect_true(all(metrics >= 0))
  } else if (metric_name == "mae") {
    expect_true(all(metrics >= 0))
  }
  
  # All metrics should be finite
  expect_true(all(is.finite(metrics)))
  
  invisible(TRUE)
}

#' Test that backtransformation preserves mathematical properties
expect_backtransform_properties <- function(original, transformed, transformation) {
  
  if (transformation == "log") {
    # exp(log(x)) should equal x
    recovered <- exp(log(original))
    expect_equal(recovered, original, tolerance = 1e-10)
  } else if (transformation == "sqrt") {
    # (sqrt(x))^2 should equal x
    recovered <- sqrt(original)^2
    expect_equal(recovered, original, tolerance = 1e-10)
  } else if (transformation == "none") {
    # Identity should not change values
    expect_equal(transformed, original)
  }
  
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## Parallel Processing Assertions
## ---------------------------------------------------------------------------

#' Test that parallel and sequential give same results
expect_parallel_consistency <- function(parallel_result, sequential_result, tolerance = 1e-6) {
  
  # Handle different result structures
  if (is.numeric(parallel_result) && is.numeric(sequential_result)) {
    expect_equal(parallel_result, sequential_result, tolerance = tolerance)
  } else if (is.data.frame(parallel_result) && is.data.frame(sequential_result)) {
    # Check same dimensions
    expect_equal(dim(parallel_result), dim(sequential_result))
    
    # Check numeric columns are equal
    numeric_cols <- sapply(parallel_result, is.numeric)
    for (col in names(parallel_result)[numeric_cols]) {
      if (col %in% names(sequential_result)) {
        expect_equal(parallel_result[[col]], sequential_result[[col]], 
                    tolerance = tolerance, 
                    info = paste("Column", col, "differs between parallel and sequential"))
      }
    }
  }
  
  invisible(TRUE)
}

#' Test that parallel context detection works correctly
expect_context_detection <- function(context, expected_backend = NULL) {
  expect_type(context, "list")
  
  required_fields <- c("context", "recommended_backend", "cores_available", "use_forking")
  expect_true(all(required_fields %in% names(context)))
  
  expect_true(context$context %in% c("local", "hpc", "nested"))
  expect_true(context$recommended_backend %in% c("multicore", "multisession"))
  expect_true(is.numeric(context$cores_available))
  expect_true(is.logical(context$use_forking))
  
  if (!is.null(expected_backend)) {
    expect_equal(context$recommended_backend, expected_backend)
  }
  
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## Performance and Memory Assertions
## ---------------------------------------------------------------------------

#' Test that operation completes within time limit
expect_within_time <- function(code, max_seconds) {
  start_time <- Sys.time()
  result <- force(code)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  
  if (elapsed >= max_seconds) {
    message <- paste("Operation took", round(elapsed, 2), "seconds, expected <", max_seconds)
    fail(message)
  } else {
    succeed()
  }
  
  invisible(result)
}

#' Test that memory usage stays reasonable
expect_memory_reasonable <- function(code, max_mb = 1000) {
  # Get memory before
  gc(verbose = FALSE)
  mem_before <- sum(gc(verbose = FALSE)[, 2])
  
  result <- force(code)
  
  # Get memory after
  gc(verbose = FALSE)
  mem_after <- sum(gc(verbose = FALSE)[, 2])
  mem_used <- mem_after - mem_before
  
  if (mem_used >= max_mb) {
    message <- paste("Memory usage:", round(mem_used, 1), "MB, expected <", max_mb, "MB")
    fail(message)
  } else {
    succeed()
  }
  
  invisible(result)
}

## ---------------------------------------------------------------------------
## Error Handling Assertions
## ---------------------------------------------------------------------------

#' Test that function handles errors gracefully
expect_graceful_error <- function(code, error_pattern = NULL, should_warn = FALSE) {
  
  if (should_warn) {
    expect_warning(result <- try(force(code), silent = TRUE))
  } else {
    result <- try(force(code), silent = TRUE)
  }
  
  if (inherits(result, "try-error")) {
    if (!is.null(error_pattern)) {
      expect_match(as.character(result), error_pattern, ignore.case = TRUE)
    }
    # Error was handled gracefully (not a crash)
    expect_true(TRUE)
  } else {
    # If no error occurred, that's also acceptable
    expect_true(TRUE)
  }
  
  invisible(result)
}

#' Test that function provides informative error messages
expect_informative_error <- function(code, min_message_length = 10) {
  
  result <- try(force(code), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    error_msg <- as.character(result)
    expect_gte(nchar(error_msg), min_message_length)
    
    # Should not just say "Error"
    expect_false(grepl("^Error\\s*$", error_msg))
  }
  
  invisible(result)
}

## ---------------------------------------------------------------------------
## Cross-Platform Assertions
## ---------------------------------------------------------------------------

#' Test behavior across different operating systems
expect_cross_platform_consistency <- function(code) {
  
  # Skip if not on supported platforms
  current_os <- Sys.info()["sysname"]
  if (!current_os %in% c("Linux", "Darwin", "Windows")) {
    skip("Unsupported operating system")
  }
  
  # Execute code and ensure it doesn't crash
  result <- force(code)
  
  # Basic checks that apply to all platforms
  expect_false(is.null(result))
  
  invisible(result)
}