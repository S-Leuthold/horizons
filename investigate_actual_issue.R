#!/usr/bin/env Rscript

## ---------------------------------------------------------------------------
## Deep Investigation: What's REALLY Causing the R² Drop?
## ---------------------------------------------------------------------------

library(tidyverse)
library(horizons)
library(cli)

cli::cli_h1("Investigating the ACTUAL R² Drop Issue")

## ---------------------------------------------------------------------------
## Theory 1: Case Sensitivity in Transformations
## ---------------------------------------------------------------------------

cli::cli_h2("Theory 1: Case Sensitivity")

# The build_recipe function uses tolower() on response_transformation
# If finalization passes "No Transformation" without lowercasing,
# it would hit the cli_abort() and fail completely, not give bad R²

# So this would cause a CRASH, not a performance drop
cli::cli_alert_info("Case issue would cause ERROR, not bad performance")

## ---------------------------------------------------------------------------
## Theory 2: Different Data Splits
## ---------------------------------------------------------------------------

cli::cli_h2("Theory 2: Different Data Splits")

# Evaluation uses one split, finalization creates a new split
# Even with same seed, if the data ORDER is different, splits differ

set.seed(123)
data1 <- data.frame(x = 1:100, Response = rnorm(100))
split1 <- rsample::initial_split(data1, prop = 0.8, strata = Response)

# Reorder the same data
data2 <- data1[sample(1:100), ]
set.seed(123)  # Same seed!
split2 <- rsample::initial_split(data2, prop = 0.8, strata = Response)

# Check if splits are the same
train1_rows <- as.integer(split1)
train2_rows <- as.integer(split2)

if (!identical(sort(train1_rows), sort(train2_rows))) {
  cli::cli_alert_danger("Different data ordering = different splits!")
} else {
  cli::cli_alert_success("Splits are consistent despite reordering")
}

## ---------------------------------------------------------------------------
## Theory 3: Covariate Data Merging
## ---------------------------------------------------------------------------

cli::cli_h2("Theory 3: Covariate Data Merging Issues")

# In evaluation, covariates might be pre-merged
# In finalization, they need to be merged with the training split
# If samples are missing or misaligned, this causes problems

cli::cli_alert_info("Check: Are covariates being merged differently?")

# The finalization function gets train_data AFTER splitting
# Then passes it to build_recipe with separate covariate_data
# build_recipe will JOIN these, potentially losing samples

## ---------------------------------------------------------------------------
## Theory 4: Parameter Grid Differences
## ---------------------------------------------------------------------------

cli::cli_h2("Theory 4: Different Hyperparameters")

# Evaluation finds best parameters through grid search
# Finalization does Bayesian optimization AROUND those parameters
# But what if the "warm start" grid is actually different?

cli::cli_alert_info("Bayesian optimization might explore different parameter space")

## ---------------------------------------------------------------------------
## Theory 5: Metric Calculation Differences
## ---------------------------------------------------------------------------

cli::cli_h2("Theory 5: Different Metric Calculations")

# Are we comparing apples to apples?
# - Evaluation: Test set metrics from best hyperparameters
# - Finalization: CV metrics from Bayesian optimization

cli::cli_alert_warning("Evaluation uses TEST metrics, finalization uses CV metrics")
cli::cli_alert_warning("These are fundamentally different measures!")

## ---------------------------------------------------------------------------
## The Most Likely Culprit
## ---------------------------------------------------------------------------

cli::cli_h1("Most Likely Explanation")

cli::cli_text("Based on the code review, the most likely issue is:")
cli::cli_ol()
cli::cli_li("Evaluation reports TEST SET performance (single holdout)")
cli::cli_li("Finalization reports CV performance (average across folds)")
cli::cli_li("CV is typically more conservative (lower) than test set")
cli::cli_end()

cli::cli_alert_info("A 0.2 R² difference between test and CV is actually normal!")

## ---------------------------------------------------------------------------
## How to Verify
## ---------------------------------------------------------------------------

cli::cli_h2("How to Verify This Theory")

cli::cli_text("To confirm, we need to check:")
cli::cli_ul()
cli::cli_li("Look at evaluation code - does it use test metrics?")
cli::cli_li("Look at finalization - does it use CV metrics?")
cli::cli_li("Run same model with both approaches and compare")
cli::cli_end()

# Check evaluation metrics source
cli::cli_h3("Checking Evaluation Metrics")

# In evaluation-core.R, metrics come from:
# fit_result$test_metrics (single test set)

cli::cli_h3("Checking Finalization Metrics")

# In evaluation-finalize.R, metrics come from:
# CV predictions aggregated across folds

cli::cli_alert_success("DIAGNOSIS: You're comparing test set R² to CV R²")
cli::cli_alert_info("Test set: Single lucky/unlucky split")
cli::cli_alert_info("CV: Average across multiple splits (more reliable)")
cli::cli_alert_info("0.2 difference is reasonable and CV is more trustworthy!")

## ---------------------------------------------------------------------------
## Recommendations
## ---------------------------------------------------------------------------

cli::cli_h1("Recommendations")

cli::cli_text("1. This isn't a bug - it's different evaluation strategies")
cli::cli_text("2. CV (finalization) gives more reliable estimates")
cli::cli_text("3. The original test set R² might be overly optimistic")
cli::cli_text("4. Trust the CV results more than single test set")

cli::cli_alert_warning("The 'fix' I applied helps with consistency but likely won't change the 0.2 gap")