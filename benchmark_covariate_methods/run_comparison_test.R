## ============================================================================
## Test Script: Run Covariate Method Comparison with Synthetic Data
## ============================================================================

## Load helper functions for creating test data
source("tests/testthat/helper-data.R")

## Load comparison framework
source("benchmark_covariate_methods/compare_methods.R")

## ---------------------------------------------------------------------------
## Step 1: Generate synthetic test data
## ---------------------------------------------------------------------------

cli::cli_h1("Generating synthetic test data")

test_data <- make_test_spectra(n_samples = 50,
                              wavelengths = seq(600, 4000, by = 2),
                              response_range = c(0.5, 5.0),
                              add_noise = TRUE,
                              add_project = TRUE,
                              seed = 123)

cli::cli_alert_success("Generated {nrow(test_data)} samples with {ncol(test_data)} columns")

## ---------------------------------------------------------------------------
## Step 2: Run comparison
## ---------------------------------------------------------------------------

results <- compare_covariate_methods(input_data = test_data,
                                    covariates = c("clay", "ph"),
                                    n_similar = 20000,
                                    verbose = TRUE)

## ---------------------------------------------------------------------------
## Step 3: Display results
## ---------------------------------------------------------------------------

cli::cli_h1("Comparison Complete")
print(results$summary)
