#!/usr/bin/env Rscript
#===============================================================================
# Script:  test_hpc_architecture.R
# Purpose: Test new HPC architecture with small subset
# Author:  Dr. Sam Leuthold
# Date:    2025-08-23
#===============================================================================

## Load package ----

library(horizons)
library(tidyverse)
library(cli)

cli::cli_h1("Testing HPC Architecture with Small Subset")

## Create mock data for testing ----

cli::cli_alert_info("Creating test data...")

## Small spectral dataset ----

set.seed(123)
n_samples    <- 100
n_wavenumbers <- 50

## Generate mock spectral data ----

wavenumber_cols <- paste0("wn_", seq(4000, 3000, length.out = n_wavenumbers))

spectral_data <- tibble(
  Sample_ID = paste0("S", 1:n_samples),
  MAOM_C_g_kg = runif(n_samples, 5, 50),
  POM_C_g_kg  = runif(n_samples, 2, 30)
)

## Add wavenumber columns ----

for (wn in wavenumber_cols) {
  spectral_data[[wn]] <- runif(n_samples, 0.1, 0.9)
}

cli::cli_alert_success("Created test data: {nrow(spectral_data)} samples, {n_wavenumbers} wavenumbers")

## Create minimal configuration ----

cli::cli_alert_info("Creating model configurations...")

test_configs <- tibble(
  model               = c("cubist", "rf", "xgboost"),
  transformation      = "No Transformation",
  preprocessing       = "raw",
  feature_selection   = "pca",
  covariates          = "none",
  include_covariates  = FALSE,
  wflow_id            = paste0("test_model_", 1:3)
)

cli::cli_alert_success("Created {nrow(test_configs)} test configurations")

## Test the HPC evaluation function ----

cli::cli_h2("Testing HPC Evaluation")

output_dir <- tempfile("test_hpc_")
dir.create(output_dir, recursive = TRUE)

cli::cli_alert_info("Output directory: {output_dir}")

## Run with minimal settings for speed ----

results <- tryCatch({
  
  run_hpc_evaluation(
    config              = test_configs,
    input_data          = spectral_data,
    covariate_data      = NULL,
    variable            = "MAOM_C_g_kg",
    output_dir          = output_dir,
    n_workers           = 2,        # Small number for testing
    grid_size_eval      = 3,        # Minimal grid
    bayesian_iter_eval  = 2,        # Minimal iterations
    cv_folds_eval       = 3,        # Minimal folds
    retrain_top_models  = FALSE,    # Skip refitting
    pruning             = TRUE,
    verbose             = TRUE
  )
  
}, error = function(e) {
  cli::cli_alert_danger("Error in run_hpc_evaluation: {e$message}")
  return(NULL)
})

## Check results ----

if (!is.null(results)) {
  
  cli::cli_h2("Test Results")
  
  n_success <- sum(results$status == "success", na.rm = TRUE)
  n_failed  <- sum(results$status == "error", na.rm = TRUE)
  n_pruned  <- sum(results$status == "pruned", na.rm = TRUE)
  
  cli::cli_alert_info("Models completed: {nrow(results)}")
  cli::cli_alert_success("Successful: {n_success}")
  cli::cli_alert_warning("Failed: {n_failed}")
  cli::cli_alert_info("Pruned: {n_pruned}")
  
  ## Show best model if any succeeded ----
  
  if (n_success > 0) {
    successful <- results[results$status == "success", ]
    
    if ("rsq" %in% names(successful)) {
      best_idx   <- which.max(successful$rsq)
      best_model <- successful[best_idx, ]
      
      cli::cli_alert_success("Best model: {best_model$wflow_id}")
      cli::cli_alert_info("  R²: {round(best_model$rsq, 3)}")
      
      if ("rmse" %in% names(best_model)) {
        cli::cli_alert_info("  RMSE: {round(best_model$rmse, 2)}")
      }
    }
  }
  
  ## Check checkpoint files ----
  
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  
  if (dir.exists(checkpoint_dir)) {
    checkpoint_files <- list.files(checkpoint_dir, pattern = "\\.qs$")
    cli::cli_alert_info("Checkpoint files created: {length(checkpoint_files)}")
  }
  
  cli::cli_h2("Test Complete")
  cli::cli_alert_success("HPC architecture test completed successfully!")
  
} else {
  
  cli::cli_alert_danger("Test failed - check error messages above")
  
}

## Clean up ----

if (dir.exists(output_dir)) {
  unlink(output_dir, recursive = TRUE)
  cli::cli_alert_info("Cleaned up temporary files")
}

cli::cli_alert_success("Test script finished")