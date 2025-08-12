#!/usr/bin/env Rscript

## =============================================================================
## Data Exploration Script - Component Interaction Analysis
## =============================================================================

# Load required packages
library(qs)
library(dplyr)
library(ggplot2)

## ---------------------------------------------------------------------------
## Step 1: Load and examine the data structure
## ---------------------------------------------------------------------------

data_path <- "~/Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs"

# Load data safely
if (file.exists(data_path)) {
  
  cat("Loading MAOM batch summary data...\n")
  batch_data <- qs::qread(data_path)
  
  cat("\n=== DATA STRUCTURE ===\n")
  cat("Dimensions:", paste(dim(batch_data), collapse = " x "), "\n")
  cat("Column names:\n")
  print(names(batch_data))
  
  cat("\n=== FIRST FEW ROWS ===\n")
  print(head(batch_data, 3))
  
  # Examine config_desc patterns
  if ("config_desc" %in% names(batch_data)) {
    cat("\n=== CONFIG_DESC PATTERNS (first 10) ===\n")
    print(head(batch_data$config_desc, 10))
    
    cat("\n=== UNIQUE COMPONENT VALUES ===\n")
    
    # Extract models
    models <- batch_data$config_desc %>%
      stringr::str_extract("^[^_]+") %>%
      unique() %>%
      sort()
    cat("Models:", paste(models, collapse = ", "), "\n")
    
    # Extract transformations  
    transformations <- batch_data$config_desc %>%
      stringr::str_extract("_([^_]+)_") %>%
      stringr::str_remove_all("_") %>%
      unique() %>%
      sort()
    cat("Transformations:", paste(transformations[!is.na(transformations)], collapse = ", "), "\n")
  }
  
  # Check performance metrics
  cat("\n=== PERFORMANCE METRICS ===\n")
  metric_cols <- names(batch_data)[sapply(batch_data, is.numeric)]
  cat("Numeric columns:", paste(metric_cols, collapse = ", "), "\n")
  
  if ("rrmse" %in% names(batch_data)) {
    cat("RRMSE range:", paste(round(range(batch_data$rrmse, na.rm = TRUE), 3), collapse = " - "), "\n")
  }
  
  if ("rsq" %in% names(batch_data)) {
    cat("R-squared range:", paste(round(range(batch_data$rsq, na.rm = TRUE), 3), collapse = " - "), "\n")
  }
  
  # Success rate
  if ("status" %in% names(batch_data)) {
    cat("\n=== STATUS SUMMARY ===\n")
    print(table(batch_data$status))
  }
  
} else {
  cat("ERROR: Data file not found at:", data_path, "\n")
}

cat("\nData exploration complete.\n")