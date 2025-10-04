## ============================================================================
## Quick Test: Verify OLD method fix
## ============================================================================

## Load required packages
library(dplyr)
library(cli)
library(purrr)
library(tidyr)
library(rsample)
library(workflows)
library(parsnip)
library(rules)
library(tune)

source("tests/testthat/helper-data.R")
source("benchmark_covariate_methods/OLD_clustered_approach.R")

cli::cli_h1("Quick Test: OLD Method")

## Generate minimal test data
test_data <- make_test_spectra(n_samples = 30,
                              wavelengths = seq(600, 4000, by = 2),
                              response_range = c(0.5, 5.0),
                              add_noise = TRUE,
                              add_project = TRUE,
                              seed = 123)

cli::cli_alert_info("Test data: {nrow(test_data)} samples")

## Test with just clay to speed things up
results <- predict_covariates(
  covariates = c("clay"),
  input_data = test_data,
  verbose    = TRUE,
  refresh    = TRUE,
  parallel   = FALSE
)

if (!is.null(results$Predicted_Values)) {
  cli::cli_alert_success("OLD method succeeded!")
  cli::cli_alert_info("Predictions: {nrow(results$Predicted_Values)} rows")
} else {
  cli::cli_alert_danger("OLD method failed")
}
