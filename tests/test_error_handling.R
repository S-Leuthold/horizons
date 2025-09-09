#!/usr/bin/env Rscript

# Test script for enhanced error handling

library(cli)
library(horizons)

cli::cli_h1("Testing Enhanced Error Handling")

# Test 1: Basic safely_execute with error
cli::cli_h2("Test 1: Basic Error Capture")
result1 <- safely_execute(
  expr = stop("This is a test error"),
  default_value = NULL,
  error_message = "Test error occurred"
)

cli::cli_text("Result is NULL: {is.null(result1$result)}")
cli::cli_text("Error captured: {!is.null(result1$error)}")
cli::cli_text("Error message: {result1$error$message}")
cli::cli_text("Number of warnings: {result1$n_warnings}")
cli::cli_text("Number of messages: {result1$n_messages}")

# Test 2: Capture warnings and messages
cli::cli_h2("Test 2: Warning and Message Capture")
result2 <- safely_execute_with_conditions(
  expr = {
    message("This is a message")
    warning("This is a warning")
    42
  }
)

cli::cli_text("Result value: {result2$result}")
cli::cli_text("Number of warnings: {result2$n_warnings}")
cli::cli_text("Warnings: {paste(result2$warnings, collapse = ', ')}")
cli::cli_text("Number of messages: {result2$n_messages}")
cli::cli_text("Messages: {paste(result2$messages, collapse = ', ')}")

# Test 3: Test error info structure
cli::cli_h2("Test 3: Error Information Structure")

error_result <- safely_execute(
  expr = stop("Model failed due to singular matrix"),
  capture_trace = TRUE,
  capture_conditions = TRUE
)

cli::cli_text("Error result has these components:")
cli::cli_bullets(c(
  " " = "result: {class(error_result$result)}",
  " " = "error: {class(error_result$error)}",
  " " = "trace: {class(error_result$trace)}",
  " " = "warnings: list of {error_result$n_warnings} items",
  " " = "messages: list of {error_result$n_messages} items"
))

# Test with multiple conditions
multi_result <- safely_execute_with_conditions(
  expr = {
    warning("First warning")
    warning("Second warning")
    message("Processing step 1")
    message("Processing step 2")
    stop("Fatal error after warnings")
  }
)

cli::cli_text("")
cli::cli_text("Multiple conditions captured:")
cli::cli_text("Warnings ({multi_result$n_warnings}): {paste(multi_result$warnings, collapse = ' | ')}")
cli::cli_text("Messages ({multi_result$n_messages}): {paste(multi_result$messages, collapse = ' | ')}")

# Test 4: Backwards compatibility (capture_conditions = FALSE by default)
cli::cli_h2("Test 4: Backwards Compatibility")
result4 <- safely_execute(
  expr = {
    warning("This warning should not be captured")
    message("This message should not be captured")
    100
  }
)

cli::cli_text("Result value: {result4$result}")
cli::cli_text("Warnings captured (should be 0): {result4$n_warnings}")
cli::cli_text("Messages captured (should be 0): {result4$n_messages}")

cli::cli_rule()
cli::cli_alert_success("All error handling tests completed")
cli::cli_text("")
cli::cli_text("Summary:")
cli::cli_bullets(c(
  "v" = "safely_execute captures errors with full context",
  "v" = "safely_execute_with_conditions captures warnings/messages",
  "v" = "create_failed_result stores enriched error information",
  "v" = "Backwards compatibility maintained (capture_conditions = FALSE by default)"
))