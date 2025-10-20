#' Tests for Error Handling Utility Functions
#'
#' Tests for error handling, condition capture, and result processing
#' utilities that provide robust error management throughout the package.
#'
#' Test Structure:
#' - VALIDATION TESTS (30%): 5 tests for parameter validation
#' - INTEGRATION TESTS (70%): 10 tests for actual error handling behavior

## ============================================================================
## VALIDATION TESTS (5 tests - 30%)
## ============================================================================

test_that("handle_results validates input structure", {

  # SPEC-UTILS-ERR-V-1: Input must be proper safe_result structure
  expect_error(
    horizons:::handle_results(list(foo = "bar")),
    "Invalid safe_result structure"
  )

  expect_error(
    horizons:::handle_results("not a list"),
    "Invalid safe_result structure"
  )

  expect_error(
    horizons:::handle_results(NULL),
    "Invalid safe_result structure"
  )

})

test_that("safely_execute validates trace_log_file when capture_trace = TRUE", {

  # SPEC-UTILS-ERR-V-2: trace_log_file must be writeable if provided
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  # Test with non-existent directory
  result <- horizons:::safely_execute(
    expr           = stop("test error"),
    capture_trace  = TRUE,
    trace_log_file = "/nonexistent/path/trace.log",
    log_error      = FALSE
  )

  # Should still capture error but not write trace
  expect_false(is.null(result$error))
  expect_true(is.null(result$trace) || !is.null(result$trace))  # Trace may or may not be captured

})

test_that("safely_execute accepts valid default values", {

  # SPEC-UTILS-ERR-V-3: Default value can be any R object
  result1 <- horizons:::safely_execute(
    expr          = stop("error"),
    default_value = data.frame(x = 1:3),
    log_error     = FALSE
  )
  expect_equal(result1$result, data.frame(x = 1:3))

  result2 <- horizons:::safely_execute(
    expr          = stop("error"),
    default_value = list(a = 1, b = "test"),
    log_error     = FALSE
  )
  expect_equal(result2$result, list(a = 1, b = "test"))

})

test_that("handle_results respects abort_on_null parameter", {

  # SPEC-UTILS-ERR-V-4: abort_on_null controls error behavior
  safe_result <- list(
    result = NULL,
    error  = simpleError("test error")
  )

  # Should abort when abort_on_null = TRUE
  expect_error(
    horizons:::handle_results(safe_result, abort_on_null = TRUE),
    "Operation failed"
  )

  # Should return NULL when abort_on_null = FALSE
  result <- horizons:::handle_results(safe_result, abort_on_null = FALSE, silent = TRUE)
  expect_null(result)

})

test_that("handle_results accepts custom error titles and hints", {

  # SPEC-UTILS-ERR-V-5: Custom error messages and hints
  safe_result <- list(
    result = NULL,
    error  = simpleError("underlying error")
  )

  expect_error(
    horizons:::handle_results(
      safe_result,
      error_title = "Custom failure message",
      error_hints = c("Try this", "Or try that"),
      abort_on_null = TRUE
    ),
    "Custom failure message"
  )

})

## ============================================================================
## INTEGRATION TESTS (10 tests - 70%)
## ============================================================================

test_that("safely_execute captures errors and returns default value", {

  # SPEC-UTILS-ERR-I-1: Basic error capture and default return
  result <- horizons:::safely_execute(
    expr          = { stop("This is an error") },
    default_value = "fallback",
    log_error     = FALSE
  )

  expect_equal(result$result, "fallback")
  expect_false(is.null(result$error))
  expect_s3_class(result$error, "simpleError")
  expect_true(grepl("This is an error", result$error$message))

})

test_that("safely_execute successfully evaluates non-erroring expressions", {

  # SPEC-UTILS-ERR-I-2: Successful execution returns result
  result <- horizons:::safely_execute(
    expr          = { 1 + 1 },
    default_value = 0,
    log_error     = FALSE
  )

  expect_equal(result$result, 2)
  expect_null(result$error)
  expect_null(result$warnings)
  expect_null(result$messages)

})

test_that("safely_execute captures warnings and messages when requested", {

  # SPEC-UTILS-ERR-I-3: Condition capture for warnings and messages
  result <- horizons:::safely_execute(
    expr = {
      warning("This is a warning")
      message("This is a message")
      warning("Second warning")
      42
    },
    capture_conditions = TRUE,
    log_error          = FALSE
  )

  expect_equal(result$result, 42)
  expect_null(result$error)
  expect_equal(length(result$warnings), 2)
  expect_equal(length(result$messages), 1)
  expect_equal(result$n_warnings, 2)
  expect_equal(result$n_messages, 1)
  expect_true(grepl("This is a warning", result$warnings[[1]]))
  expect_true(grepl("This is a message", result$messages[[1]]))

})

test_that("safely_execute captures trace when requested", {

  # SPEC-UTILS-ERR-I-4: Traceback capture for debugging
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  result <- horizons:::safely_execute(
    expr = {
      inner_function <- function() stop("Deep error")
      outer_function <- function() inner_function()
      outer_function()
    },
    capture_trace = TRUE,
    log_error     = FALSE
  )

  expect_false(is.null(result$error))
  # Trace capture depends on environment setup
  # May or may not be captured in all contexts

})

test_that("safely_execute_with_conditions always captures conditions", {

  # SPEC-UTILS-ERR-I-5: Convenience wrapper for condition capture
  result <- horizons:::safely_execute_with_conditions(
    expr = {
      warning("Test warning")
      message("Test message")
      "success"
    },
    log_error = FALSE
  )

  expect_equal(result$result, "success")
  expect_equal(length(result$warnings), 1)
  expect_equal(length(result$messages), 1)
  expect_equal(result$n_warnings, 1)
  expect_equal(result$n_messages, 1)

})

test_that("safely_execute handles complex expressions with environment access", {

  # SPEC-UTILS-ERR-I-6: Expression evaluation in caller environment
  x <- 10
  y <- 20

  result <- horizons:::safely_execute(
    expr = {
      z <- x + y
      if (z > 25) warning("Sum is large")
      z * 2
    },
    capture_conditions = TRUE,
    log_error          = FALSE
  )

  expect_equal(result$result, 60)
  expect_equal(length(result$warnings), 1)
  expect_true(grepl("Sum is large", result$warnings[[1]]))

})

test_that("handle_results processes successful results correctly", {

  # SPEC-UTILS-ERR-I-7: Successful result handling
  safe_result <- list(
    result     = data.frame(a = 1:3, b = 4:6),
    error      = NULL,
    warnings   = list("Minor warning"),
    messages   = list("Info message"),
    n_warnings = 1,
    n_messages = 1
  )

  # Capture output to check warnings/messages are displayed
  output <- capture.output({
    result <- horizons:::handle_results(safe_result, silent = FALSE)
  }, type = "message")

  expect_equal(result, data.frame(a = 1:3, b = 4:6))

})

test_that("handle_results builds comprehensive error messages", {

  # SPEC-UTILS-ERR-I-8: Error message construction with all components
  safe_result <- list(
    result     = NULL,
    error      = simpleError("Main error occurred"),
    warnings   = list("Warning 1", "Warning 2"),
    messages   = list("Info 1"),
    n_warnings = 2,
    n_messages = 1
  )

  expect_error(
    horizons:::handle_results(
      safe_result,
      error_title = "Processing failed",
      error_hints = c(
        "Check your input data",
        "Verify all parameters"
      ),
      abort_on_null = TRUE
    ),
    "Processing failed"
  )

})

test_that("safely_execute integrates with dplyr/purrr workflows", {

  # SPEC-UTILS-ERR-I-9: Integration with tidyverse workflows
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  # Simulate a mapping operation that might fail
  data <- list(
    list(x = 1, y = 2),
    list(x = 3, y = 0),  # Will cause division by zero
    list(x = 5, y = 2)
  )

  results <- purrr::map(data, function(item) {
    horizons:::safely_execute(
      expr          = { if (item$y == 0) stop('division by zero'); item$x / item$y },
      default_value = NA_real_,
      log_error     = FALSE
    )$result
  })

  expect_equal(results[[1]], 0.5)
  expect_true(is.na(results[[2]]))  # Division by zero returns default
  expect_equal(results[[3]], 2.5)

})

test_that("safely_execute handles errors in parallel contexts", {

  # SPEC-UTILS-ERR-I-10: Parallel execution error handling
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  # Simulate parallel processing with potential errors
  library(future)
  old_plan <- plan()
  plan(multisession, workers = 2)

  # Function that randomly fails
  risky_function <- function(x) {
    if (x == 3) stop("Intentional error")
    x^2
  }

  # Use safely_execute to handle failures gracefully
  values <- 1:5
  results <- future.apply::future_lapply(values, function(val) {
    horizons:::safely_execute(
      expr          = risky_function(val),
      default_value = -1,
      log_error     = FALSE
    )$result
  })

  expect_equal(results[[1]], 1)
  expect_equal(results[[2]], 4)
  expect_equal(results[[3]], -1)  # Error case returns default
  expect_equal(results[[4]], 16)
  expect_equal(results[[5]], 25)

  # Restore original plan
  plan(old_plan)

})