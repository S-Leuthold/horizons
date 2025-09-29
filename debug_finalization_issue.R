## ---------------------------------------------------------------------------
## Debug Script: R² Degradation in Finalized Models
## ---------------------------------------------------------------------------

# This script helps diagnose why finalized models have ~0.2 lower R² than
# evaluation results

library(tidyverse)
library(horizons)

## ---------------------------------------------------------------------------
## Step 1: Create minimal test dataset
## ---------------------------------------------------------------------------

# Load example data if available
if (file.exists("dev/example_data.RData")) {
  load("dev/example_data.RData")
} else {
  cli::cli_alert_warning("No example data found, using simulated data")

  # Create simulated spectral data
  set.seed(123)
  n_samples <- 100
  n_wavelengths <- 50

  # Generate spectral matrix
  spectral_data <- matrix(runif(n_samples * n_wavelengths),
                          nrow = n_samples)
  # Use proper wavenumber format with 4 digits (e.g., 4000, 4001, 4002...)
  # This matches the regex "^[0-9]{3,4}(\\.[0-9]+)?$"
  colnames(spectral_data) <- as.character(seq(4000, 4000 + n_wavelengths - 1))

  # Create response correlated with spectra
  response <- rowMeans(spectral_data[, 1:10]) * 5 + rnorm(n_samples, 0, 0.1)

  # Combine into data frame
  input_data <- data.frame(
    Sample_ID = paste0("S", 1:n_samples),
    spectral_data,
    SOC = response
  )

  # Create covariate data
  covariate_data <- data.frame(
    Sample_ID = paste0("S", 1:n_samples),
    ph = runif(n_samples, 5, 8),
    MAP = runif(n_samples, 200, 1200),
    AI = runif(n_samples, 0.2, 0.8)
  )
}

## ---------------------------------------------------------------------------
## Step 2: Run evaluation with minimal config
## ---------------------------------------------------------------------------

cli::cli_h1("Testing Evaluation vs Finalization")

# Create minimal config with covariates
configs <- create_configs(
  models = "rf",
  transformations = "No Transformation",
  preprocessing = "raw",
  feature_selection = "None",
  soil_covariates = c("ph", "MAP")  # Test with covariates
)

cli::cli_alert_info("Created {nrow(configs)} configuration(s)")
print(configs)

## ---------------------------------------------------------------------------
## Step 3: Run evaluation to get baseline metrics
## ---------------------------------------------------------------------------

cli::cli_h2("Running evaluation")

evaluation_results <- evaluate_models_local(
  config = configs,  # Changed from configs to config
  input_data = input_data,
  covariate_data = covariate_data,
  variable = "SOC",
  seed = 123,
  allow_par = FALSE,
  verbose = TRUE
)

cli::cli_alert_success("Evaluation R²: {round(evaluation_results$rsq[1], 3)}")

## ---------------------------------------------------------------------------
## Step 4: Run finalization on same model
## ---------------------------------------------------------------------------

cli::cli_h2("Running finalization")

# Test with exact params (no optimization)
finalized_exact <- finalize_top_workflows(
  evaluation_results = evaluation_results,
  input_data = input_data,
  covariate_data = covariate_data,
  variable = "SOC",
  n_best = 1,
  metric = "rsq",
  bayesian_iter = 0,  # Skip optimization
  use_exact_params = TRUE,  # Use exact params from evaluation
  seed = 123,
  verbose = TRUE
)

if (!is.null(finalized_exact$metrics[[1]])) {
  cli::cli_alert_success("Finalized R² (exact params): {round(finalized_exact$metrics[[1]]$rsq, 3)}")
}

# Test with optimization
finalized_optimized <- finalize_top_workflows(
  evaluation_results = evaluation_results,
  input_data = input_data,
  covariate_data = covariate_data,
  variable = "SOC",
  n_best = 1,
  metric = "rsq",
  bayesian_iter = 5,  # Quick optimization
  seed = 123,
  verbose = TRUE
)

if (!is.null(finalized_optimized$metrics[[1]])) {
  cli::cli_alert_success("Finalized R² (optimized): {round(finalized_optimized$metrics[[1]]$rsq, 3)}")
}

## ---------------------------------------------------------------------------
## Step 5: Direct comparison of recipes
## ---------------------------------------------------------------------------

cli::cli_h2("Comparing recipes")

# Build recipe as in evaluation
cli::cli_alert_info("Building evaluation-style recipe...")

# Rename for consistency
eval_data <- input_data %>%
  rename(Response = SOC)

recipe_eval <- build_recipe(
  input_data = eval_data,
  spectral_transformation = "raw",
  response_transformation = "No Transformation",
  feature_selection_method = "None",
  covariate_selection = c("ph", "MAP"),  # Direct vector as in evaluation
  covariate_data = covariate_data
)

# Build recipe as in finalization
cli::cli_alert_info("Building finalization-style recipe...")

# Parse covariates as in finalization
covariate_string <- "ph-MAP"
covariate_list <- strsplit(covariate_string, "-")[[1]]

recipe_finalize <- build_recipe(
  input_data = eval_data,
  spectral_transformation = "raw",
  response_transformation = "No Transformation",
  feature_selection_method = "None",
  covariate_selection = covariate_list,  # Parsed from string
  covariate_data = covariate_data
)

# Compare recipes
cli::cli_alert_info("Comparing recipe outputs...")

# Prep and bake both recipes
eval_prepped <- recipes::prep(recipe_eval)
final_prepped <- recipes::prep(recipe_finalize)

eval_baked <- recipes::bake(eval_prepped, new_data = NULL)
final_baked <- recipes::bake(final_prepped, new_data = NULL)

# Check dimensions
cli::cli_alert_info("Evaluation recipe output: {nrow(eval_baked)} x {ncol(eval_baked)}")
cli::cli_alert_info("Finalization recipe output: {nrow(final_baked)} x {ncol(final_baked)}")

# Check column differences
eval_cols <- names(eval_baked)
final_cols <- names(final_baked)

missing_in_final <- setdiff(eval_cols, final_cols)
extra_in_final <- setdiff(final_cols, eval_cols)

if (length(missing_in_final) > 0) {
  cli::cli_alert_danger("Columns missing in finalization: {missing_in_final}")
}

if (length(extra_in_final) > 0) {
  cli::cli_alert_warning("Extra columns in finalization: {extra_in_final}")
}

# Check if covariates are present
covariates_in_eval <- c("ph", "MAP") %in% names(eval_baked)
covariates_in_final <- c("ph", "MAP") %in% names(final_baked)

cli::cli_alert_info("Covariates in evaluation output: ph={covariates_in_eval[1]}, MAP={covariates_in_eval[2]}")
cli::cli_alert_info("Covariates in finalization output: ph={covariates_in_final[1]}, MAP={covariates_in_final[2]}")

## ---------------------------------------------------------------------------
## Step 6: Summary
## ---------------------------------------------------------------------------

cli::cli_h1("Summary")

cli::cli_text("Evaluation R²: {round(evaluation_results$rsq[1], 3)}")

if (!is.null(finalized_exact$metrics[[1]])) {
  cli::cli_text("Finalized R² (exact): {round(finalized_exact$metrics[[1]]$rsq, 3)}")
  cli::cli_text("Difference (exact): {round(finalized_exact$metrics[[1]]$rsq - evaluation_results$rsq[1], 3)}")
}

if (!is.null(finalized_optimized$metrics[[1]])) {
  cli::cli_text("Finalized R² (optimized): {round(finalized_optimized$metrics[[1]]$rsq, 3)}")
  cli::cli_text("Difference (optimized): {round(finalized_optimized$metrics[[1]]$rsq - evaluation_results$rsq[1], 3)}")
}

cli::cli_alert_warning("If differences are significant, check:")
cli::cli_text("1. Covariate data joining/merging")
cli::cli_text("2. Data splits and random seeds")
cli::cli_text("3. Recipe preprocessing steps")
cli::cli_text("4. Metric calculation methods")