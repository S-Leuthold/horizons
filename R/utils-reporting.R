#' Reporting Utilities for Consistent Console Output
#'
#' @description
#' This module provides utilities for creating consistent, hierarchical console
#' output across all horizons functions. Inspired by the evaluation system's
#' tree-structured reporting with Unicode box drawing characters.
#'
#' @name utils-reporting
NULL

## =============================================================================
## Core Tree Formatting Functions
## =============================================================================

#' Format Tree Item with Box Drawing Characters
#'
#' @description
#' Creates hierarchical tree-style output using Unicode box drawing characters.
#' Handles nesting, connectors, and status symbols.
#'
#' @param text Character. The text content to display
#' @param level Integer. Indentation level (0 = no indent, 1 = first level, etc.)
#' @param is_last Logical. Whether this is the last item at this level
#' @param symbol Character. Optional status symbol to prepend
#' @param connector Character. Override default connector ("├─" or "└─")
#'
#' @return Character string with formatted tree item
#' @keywords internal
format_tree_item <- function(text,
                             level = 0,
                             is_last = TRUE,
                             symbol = NULL,
                             connector = NULL) {

  # Handle root level (no connectors)
  if (level == 0) {
    prefix <- ""
  } else {

    # Build indentation for nested levels
    indent <- ""
    if (level > 1) {
      indent <- strrep("│  ", level - 1)
    }

    # Choose connector
    if (is.null(connector)) {
      connector <- if (is_last) "└─" else "├─"
    }

    prefix <- paste0(indent, connector, " ")
  }

  # Add status symbol if provided
  if (!is.null(symbol)) {
    text <- paste(symbol, text)
  }

  paste0(prefix, text)
}

#' Format Summary Tree from Nested List
#'
#' @description
#' Converts a nested list structure into a formatted tree display.
#' Automatically handles nesting levels and connector logic.
#'
#' @param data List. Nested list structure to format
#' @param level Integer. Current nesting level (internal use)
#' @param parent_last Logical. Whether parent is last item (internal use)
#'
#' @return Character vector of formatted tree lines
#' @keywords internal
format_summary_tree <- function(data, level = 0, parent_last = TRUE) {

  result <- character()
  items <- names(data)
  n_items <- length(items)

  for (i in seq_along(items)) {

    item_name <- items[i]
    item_value <- data[[i]]
    is_last <- (i == n_items)

    if (is.list(item_value) && length(item_value) > 0) {

      # Nested list - add header and recurse
      result <- c(result, format_tree_item(item_name, level, is_last))

      # Recurse for nested items
      nested <- format_summary_tree(item_value, level + 1, is_last)
      result <- c(result, nested)

    } else {

      # Simple key-value pair
      display_text <- paste0(item_name, ": ", item_value)
      result <- c(result, format_tree_item(display_text, level, is_last))

    }
  }

  result
}

## =============================================================================
## Progress and Status Functions
## =============================================================================

#' Format Progress Bar with Unicode Characters
#'
#' @description
#' Creates visual progress bars using Unicode block characters.
#' Supports percentage display, ETA, and custom styling.
#'
#' @param current Numeric. Current progress value
#' @param total Numeric. Total/maximum progress value
#' @param width Integer. Width of progress bar in characters (default: 20)
#' @param show_percent Logical. Show percentage (default: TRUE)
#' @param show_eta Logical. Show estimated time remaining (default: FALSE)
#' @param eta_mins Numeric. ETA in minutes (required if show_eta = TRUE)
#' @param label Character. Optional label to prepend
#'
#' @return Character string with formatted progress bar
#' @keywords internal
format_progress_bar <- function(current,
                                total,
                                width = 20,
                                show_percent = TRUE,
                                show_eta = FALSE,
                                eta_mins = NULL,
                                label = NULL) {

  # Calculate progress
  pct <- min(100, max(0, round(current / total * 100)))
  filled <- round(width * current / total)
  empty <- width - filled

  # Build bar
  bar <- paste0(strrep("█", filled), strrep("░", empty))

  # Build components
  components <- character()

  # Add label if provided
  if (!is.null(label)) {
    components <- c(components, label)
  }

  # Add the bar itself
  components <- c(components, bar)

  # Add percentage
  if (show_percent) {
    components <- c(components, paste0(pct, "%"))
  }

  # Add ETA
  if (show_eta && !is.null(eta_mins)) {
    if (eta_mins < 1) {
      eta_text <- paste0("~", round(eta_mins * 60), " sec")
    } else {
      eta_text <- paste0("~", round(eta_mins), " min")
    }
    components <- c(components, eta_text)
  }

  paste(components, collapse = " | ")
}

#' Get Status Symbol
#'
#' @description
#' Returns appropriate Unicode symbols for different status types.
#' Ensures cross-platform compatibility by avoiding emojis.
#'
#' @param status Character. Status type ("success", "complete", "in_progress",
#'   "error", "warning", "info")
#'
#' @return Character. Unicode symbol for the status
#' @keywords internal
get_status_symbol <- function(status) {

  symbols <- list(
    "success" = "✓",      # Check mark
    "complete" = "✔",     # Heavy check mark
    "in_progress" = "⟳",  # Clockwise gapped circle arrow
    "error" = "✗",        # X mark
    "warning" = "⚠",      # Warning sign
    "info" = "ℹ",         # Information
    "pending" = "○"       # White circle
  )

  symbols[[status]] %||% "•"  # Default bullet if status not found
}

## =============================================================================
## Formatting Helper Functions
## =============================================================================

#' Format Time Duration
#'
#' @description
#' Converts numeric time values to human-readable format with appropriate units.
#' Automatically selects best unit (ms, s, min, hours).
#'
#' @param time_value Numeric. Time value in seconds
#' @param precision Integer. Number of decimal places (default: 1)
#'
#' @return Character string with formatted time
#' @keywords internal
format_time <- function(time_value, precision = 1) {

  if (is.na(time_value) || time_value < 0) {
    return("--")
  }

  if (time_value < 1) {
    # Milliseconds
    paste0(round(time_value * 1000, 0), "ms")
  } else if (time_value < 60) {
    # Seconds
    paste0(round(time_value, precision), "s")
  } else if (time_value < 3600) {
    # Minutes
    paste0(round(time_value / 60, precision), " min")
  } else {
    # Hours
    paste0(round(time_value / 3600, precision), " hours")
  }
}

#' Format Metric Values
#'
#' @description
#' Formats numeric metrics with appropriate precision and units.
#' Handles common spectroscopy metrics (R², RMSE, percentages).
#'
#' @param value Numeric. The metric value
#' @param metric_type Character. Type of metric for context-specific formatting
#' @param precision Integer. Decimal places (auto-selected if NULL)
#'
#' @return Character string with formatted metric
#' @keywords internal
format_metric <- function(value, metric_type = "auto", precision = NULL) {

  if (is.na(value)) {
    return("--")
  }

  # Auto-detect precision based on value range
  if (is.null(precision)) {
    if (abs(value) < 0.01) {
      precision <- 4
    } else if (abs(value) < 0.1) {
      precision <- 3
    } else if (abs(value) < 1) {
      precision <- 3
    } else if (abs(value) < 10) {
      precision <- 2
    } else {
      precision <- 1
    }
  }

  # Format based on metric type
  formatted <- switch(metric_type,
    "percentage" = paste0(round(value, precision), "%"),
    "r2" = round(value, 3),
    "rmse" = round(value, precision),
    "rrmse" = paste0(round(value, 1), "%"),
    "count" = formatC(value, big.mark = ",", format = "f", digits = 0),
    round(value, precision)  # default
  )

  as.character(formatted)
}

#' Format Section Header
#'
#' @description
#' Creates section headers with horizontal rules using Unicode characters.
#' Supports single/double lines and text centering.
#'
#' @param text Character. Header text
#' @param style Character. "single", "double", or "thick"
#' @param width Integer. Total width (default: 79)
#' @param center Logical. Center the text (default: TRUE)
#'
#' @return Character string with formatted header
#' @keywords internal
format_header <- function(text, style = "double", width = 79, center = TRUE) {

  # Choose rule character
  rule_char <- switch(style,
    "single" = "─",
    "double" = "═",
    "thick" = "━",
    "─"  # default
  )

  if (center) {
    # Calculate padding for centering
    text_width <- nchar(text)
    if (text_width >= width - 4) {
      # Text too long, just use padding
      result <- paste0(strrep(rule_char, 2), " ", text, " ", strrep(rule_char, 2))
    } else {
      # Center the text
      total_padding <- width - text_width
      left_padding <- floor(total_padding / 2)
      right_padding <- total_padding - left_padding
      result <- paste0(strrep(rule_char, left_padding), text, strrep(rule_char, right_padding))
    }
  } else {
    # Left-aligned with some padding
    remaining <- width - nchar(text) - 1
    result <- paste0(text, " ", strrep(rule_char, remaining))
  }

  result
}

## =============================================================================
## Convenience Functions for Common Patterns
## =============================================================================

#' Display Configuration Summary
#'
#' @description
#' Standardized way to display configuration information at start of operations.
#'
#' @param title Character. Main operation title
#' @param config List. Configuration parameters to display
#' @param verbose Logical. Whether to show detailed output
#'
#' @return NULL (side effect: prints to console)
#' @keywords internal
display_config_summary <- function(title, config, verbose = TRUE) {

  if (!verbose) return(invisible(NULL))

  # Main header
  cli::cli_text("")
  cli::cli_text(format_header(title, style = "double"))
  cli::cli_text("")

  # Configuration tree
  if (length(config) > 0) {
    config_lines <- format_summary_tree(list("Configuration" = config))
    for (line in config_lines) {
      cli::cli_text(line)
    }
    cli::cli_text("")
  }
}

#' Display Operation Results
#'
#' @description
#' Standardized way to display operation completion with metrics and timing.
#'
#' @param operation Character. Name of completed operation
#' @param metrics List. Named list of metrics to display
#' @param timing Numeric. Operation time in seconds
#' @param status Character. Operation status
#' @param verbose Logical. Whether to show detailed output
#'
#' @return NULL (side effect: prints to console)
#' @keywords internal
display_operation_results <- function(operation, metrics = NULL, timing = NULL,
                                     status = "success", verbose = TRUE) {

  if (!verbose) return(invisible(NULL))

  # Status line
  symbol <- get_status_symbol(status)
  status_text <- paste(symbol, operation, "complete")

  if (!is.null(timing)) {
    status_text <- paste0(status_text, " [", format_time(timing), "]")
  }

  cli::cli_text(status_text)

  # Metrics if provided
  if (!is.null(metrics) && length(metrics) > 0) {
    results_tree <- format_summary_tree(list("Results" = metrics))
    for (line in results_tree) {
      cli::cli_text(line)
    }
  }

  cli::cli_text("")
}


#' Stop quietyly
#'
#' @description
#' Supress the error message from stop() without compromising downstream reporting
#'

#' @return NULL
#' @keywords internal

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
