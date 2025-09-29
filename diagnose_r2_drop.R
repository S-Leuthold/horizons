## ---------------------------------------------------------------------------
## Focused Diagnosis: R² Drop in Finalization
## ---------------------------------------------------------------------------

library(tidyverse)
library(horizons)
library(cli)

cli::cli_h1("R² Degradation Diagnosis")

## ---------------------------------------------------------------------------
## Create Test Data (properly formatted)
## ---------------------------------------------------------------------------

set.seed(123)
n_samples <- 100
n_wavelengths <- 50

# Create base data
test_data <- tibble::tibble(
  Sample_ID = paste0("S", 1:n_samples),
  Project = rep("TestProject", n_samples)  # Add required Project column
)

# Add spectral columns with proper wavelength names
wavelengths <- round(seq(600, 4000, length.out = n_wavelengths))
for (wl in as.character(wavelengths)) {
  test_data[[wl]] <- runif(n_samples, 0.3, 0.7) + rnorm(n_samples, 0, 0.01)
}

# Add response variable correlated with some spectral features
# Get spectral columns (skip Sample_ID and Project)
spectral_cols <- grep("^[0-9]+$", names(test_data), value = TRUE)
test_data$SOC <- rowMeans(test_data[, spectral_cols[1:10]]) * 10 + rnorm(n_samples, 0, 0.5)

# Create covariate data
covariate_data <- tibble::tibble(
  Sample_ID = paste0("S", 1:n_samples),
  ph = runif(n_samples, 5, 8),
  MAP = runif(n_samples, 200, 1200),
  AI = runif(n_samples, 0.2, 0.8)
)

cli::cli_alert_success("Created test data: {nrow(test_data)} samples, {n_wavelengths} wavelengths")

## ---------------------------------------------------------------------------
## Direct Recipe Comparison
## ---------------------------------------------------------------------------

cli::cli_h2("Testing Recipe Building Consistency")

# Prepare data as in evaluation
eval_data <- test_data %>% rename(Response = SOC)

# Test 1: Recipe with covariates as vector (evaluation style)
cli::cli_text("Building evaluation-style recipe (covariates as vector)...")
recipe_eval <- horizons:::build_recipe(
  input_data = eval_data,
  spectral_transformation = "raw",
  response_transformation = "none",  # Use lowercase
  feature_selection_method = "none",  # Use lowercase
  covariate_selection = c("ph", "MAP"),  # Direct vector
  covariate_data = covariate_data
)

# Test 2: Recipe with covariates parsed from string (finalization style)
cli::cli_text("Building finalization-style recipe (covariates from string)...")
covariate_string <- "ph-MAP"
covariate_list <- strsplit(covariate_string, "-")[[1]]

recipe_final <- horizons:::build_recipe(
  input_data = eval_data,
  spectral_transformation = "raw",
  response_transformation = "none",  # Use lowercase
  feature_selection_method = "none",  # Use lowercase
  covariate_selection = covariate_list,
  covariate_data = covariate_data
)

# Compare prepped recipes
eval_prepped <- recipes::prep(recipe_eval)
final_prepped <- recipes::prep(recipe_final)

eval_baked <- recipes::bake(eval_prepped, new_data = NULL)
final_baked <- recipes::bake(final_prepped, new_data = NULL)

cli::cli_alert_info("Evaluation recipe: {ncol(eval_baked)} columns")
cli::cli_alert_info("Finalization recipe: {ncol(final_baked)} columns")

# Check for differences
if (!identical(names(eval_baked), names(final_baked))) {
  cli::cli_alert_danger("Column names differ!")
  cli::cli_text("Missing in final: {setdiff(names(eval_baked), names(final_baked))}")
  cli::cli_text("Extra in final: {setdiff(names(final_baked), names(eval_baked))}")
} else {
  cli::cli_alert_success("Column names match!")
}

## ---------------------------------------------------------------------------
## Test Model Training
## ---------------------------------------------------------------------------

cli::cli_h2("Testing Model Training")

# Create a simple RF model
model_spec <- parsnip::rand_forest(
  trees = 100,
  mtry = 10,
  min_n = 5
) %>%
  parsnip::set_engine("ranger") %>%
  parsnip::set_mode("regression")

# Train with evaluation recipe
workflow_eval <- workflows::workflow() %>%
  workflows::add_recipe(recipe_eval) %>%
  workflows::add_model(model_spec)

fit_eval <- workflows::fit(workflow_eval, data = eval_data)

# Train with finalization recipe
workflow_final <- workflows::workflow() %>%
  workflows::add_recipe(recipe_final) %>%
  workflows::add_model(model_spec)

fit_final <- workflows::fit(workflow_final, data = eval_data)

# Make predictions
pred_eval <- predict(fit_eval, eval_data)
pred_final <- predict(fit_final, eval_data)

# Calculate R²
rsq_eval <- yardstick::rsq_vec(eval_data$Response, pred_eval$.pred)
rsq_final <- yardstick::rsq_vec(eval_data$Response, pred_final$.pred)

cli::cli_h2("Results")
cli::cli_alert_info("R² with evaluation recipe: {round(rsq_eval, 3)}")
cli::cli_alert_info("R² with finalization recipe: {round(rsq_final, 3)}")
cli::cli_alert_info("Difference: {round(rsq_final - rsq_eval, 3)}")

if (abs(rsq_final - rsq_eval) > 0.01) {
  cli::cli_alert_danger("Significant difference detected!")
} else {
  cli::cli_alert_success("No significant difference in R²")
}

## ---------------------------------------------------------------------------
## Test Data Splitting Issue
## ---------------------------------------------------------------------------

cli::cli_h2("Testing Data Split Consistency")

# Test if the issue is in train/test splitting
set.seed(123)
split1 <- rsample::initial_split(eval_data, prop = 0.8, strata = Response)
train1 <- rsample::training(split1)
test1 <- rsample::testing(split1)

set.seed(123)
split2 <- rsample::initial_split(eval_data, prop = 0.8, strata = Response)
train2 <- rsample::training(split2)
test2 <- rsample::testing(split2)

if (identical(train1$Sample_ID, train2$Sample_ID)) {
  cli::cli_alert_success("Train/test splits are consistent")
} else {
  cli::cli_alert_danger("Train/test splits differ!")
}

## ---------------------------------------------------------------------------
## Check Covariate Integration
## ---------------------------------------------------------------------------

cli::cli_h2("Checking Covariate Integration")

# Check if covariates are properly joined
has_ph_eval <- "ph" %in% names(eval_baked)
has_MAP_eval <- "MAP" %in% names(eval_baked)
has_ph_final <- "ph" %in% names(final_baked)
has_MAP_final <- "MAP" %in% names(final_baked)

cli::cli_text("Evaluation recipe - ph: {has_ph_eval}, MAP: {has_MAP_eval}")
cli::cli_text("Finalization recipe - ph: {has_ph_final}, MAP: {has_MAP_final}")

if (has_ph_eval && has_MAP_eval && has_ph_final && has_MAP_final) {
  # Check if values match
  ph_match <- identical(eval_baked$ph, final_baked$ph)
  MAP_match <- identical(eval_baked$MAP, final_baked$MAP)

  cli::cli_text("Covariate values match - ph: {ph_match}, MAP: {MAP_match}")

  if (!ph_match || !MAP_match) {
    cli::cli_alert_danger("Covariate values differ between recipes!")
  }
}

## ---------------------------------------------------------------------------
## Summary
## ---------------------------------------------------------------------------

cli::cli_h1("Summary")

cli::cli_text("Recipe columns match: {identical(names(eval_baked), names(final_baked))}")
cli::cli_text("R² difference: {round(rsq_final - rsq_eval, 3)}")
cli::cli_text("Covariates present: ph={has_ph_final}, MAP={has_MAP_final}")

cli::cli_alert_info("Key findings:")
if (abs(rsq_final - rsq_eval) > 0.01) {
  cli::cli_text("• Significant R² difference detected between recipes")
}
if (!identical(names(eval_baked), names(final_baked))) {
  cli::cli_text("• Recipe outputs have different columns")
}
if (!has_ph_final || !has_MAP_final) {
  cli::cli_text("• Covariates missing in finalization recipe")
}