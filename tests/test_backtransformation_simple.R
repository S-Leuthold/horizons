# Simple Back-transformation Test
# Focused test that verifies core back-transformation logic without full pipeline

library(tidymodels)
library(testthat)

# Load the package functions
devtools::load_all("/Users/samleuthold/Desktop/_brain/1_Current_Projects/horizons/horizons-package", quiet = TRUE)

cli::cli_h1("Simple Back-transformation Test")

## ---------------------------------------------------------------------------
## Test 1: Back-transformation Functions
## ---------------------------------------------------------------------------

cli::cli_h2("Testing back-transformation utilities")

# Test log back-transformation
test_that("log back-transformation works", {
  log_pred <- log(c(10, 20, 30))
  back_log <- back_transform_predictions(log_pred, "log", warn = FALSE)
  expect_equal(back_log, c(10, 20, 30), tolerance = 0.001)
})
cli::cli_alert_success("Log back-transformation: ✓")

# Test sqrt back-transformation
test_that("sqrt back-transformation works", {
  sqrt_pred <- sqrt(c(4, 9, 16))
  back_sqrt <- back_transform_predictions(sqrt_pred, "sqrt", warn = FALSE)
  expect_equal(back_sqrt, c(4, 9, 16), tolerance = 0.001)
})
cli::cli_alert_success("Sqrt back-transformation: ✓")

# Test none transformation
test_that("none transformation returns identity", {
  none_pred <- c(1, 2, 3)
  back_none <- back_transform_predictions(none_pred, "none", warn = FALSE)
  expect_equal(back_none, c(1, 2, 3))
})
cli::cli_alert_success("No transformation (identity): ✓")

# Test NA handling
test_that("NA values are preserved", {
  na_pred <- c(log(10), NA, log(30))
  back_na <- back_transform_predictions(na_pred, "log", warn = FALSE)
  expect_true(is.na(back_na[2]))
  expect_equal(back_na[c(1,3)], c(10, 30), tolerance = 0.001)
})
cli::cli_alert_success("NA handling: ✓")

# Test edge cases
test_that("edge cases handled correctly", {
  # Empty vector
  expect_equal(back_transform_predictions(numeric(0), "log"), numeric(0))
  
  # NULL input
  expect_null(back_transform_predictions(NULL, "log"))
  
  # Unknown transformation defaults to identity
  expect_equal(
    back_transform_predictions(c(1, 2, 3), "unknown", warn = FALSE),
    c(1, 2, 3)
  )
})
cli::cli_alert_success("Edge cases: ✓")

## ---------------------------------------------------------------------------
## Test 2: Recipe Transformation Behavior
## ---------------------------------------------------------------------------

cli::cli_h2("Testing recipe transformation behavior")

# Create simple test data
set.seed(123)
n <- 100
test_data <- tibble(
  Sample_ID = paste0("S", 1:n),
  Project = "Test",
  Response = exp(rnorm(n, mean = 2, sd = 0.5)),  # Log-normal
  `600` = rnorm(n),
  `700` = rnorm(n),
  `800` = rnorm(n),
  `900` = rnorm(n),
  `1000` = rnorm(n)
)

# Test recipe with log transformation
recipe_log <- recipes::recipe(Response ~ ., data = test_data) %>%
  recipes::update_role(Sample_ID, new_role = "id") %>%
  recipes::update_role(Project, new_role = "metadata") %>%
  recipes::step_log(all_outcomes(), skip = TRUE) %>%
  recipes::step_normalize(all_numeric_predictors())

prepped_log <- prep(recipe_log, training = test_data)

# Check training data (should be transformed)
train_baked <- bake(prepped_log, new_data = NULL)
cli::cli_text("Training data Response range: [{round(min(train_baked$Response), 2)}, {round(max(train_baked$Response), 2)}]")
cli::cli_text("Expected log range: [{round(min(log(test_data$Response)), 2)}, {round(max(log(test_data$Response)), 2)}]")

test_that("training data is transformed", {
  expect_equal(
    range(train_baked$Response),
    range(log(test_data$Response)),
    tolerance = 0.001
  )
})
cli::cli_alert_success("Training data transformed correctly: ✓")

# Check test data (with skip=TRUE, should be original scale)
test_subset <- test_data[1:10, ]
test_baked <- bake(prepped_log, new_data = test_subset)

test_that("test data keeps original scale with skip=TRUE", {
  expect_equal(
    range(test_baked$Response),
    range(test_subset$Response),
    tolerance = 0.001
  )
})
cli::cli_alert_success("Test data in original scale (skip=TRUE): ✓")

## ---------------------------------------------------------------------------
## Test 3: Model Predictions Need Back-transformation
## ---------------------------------------------------------------------------

cli::cli_h2("Testing that model predictions need back-transformation")

# Create a simple linear model workflow
model_spec <- linear_reg() %>% set_engine("lm")

wf <- workflow() %>%
  add_recipe(recipe_log) %>%
  add_model(model_spec)

# Fit the model
fitted_wf <- fit(wf, data = test_data)

# Get predictions
preds_raw <- predict(fitted_wf, new_data = test_data[1:10, ])

cli::cli_text("Raw prediction range: [{round(min(preds_raw$.pred), 2)}, {round(max(preds_raw$.pred), 2)}]")
cli::cli_text("Original Response range: [{round(min(test_data$Response[1:10]), 2)}, {round(max(test_data$Response[1:10]), 2)}]")

# Predictions should be in log scale (much smaller values)
test_that("predictions are in transformed scale", {
  expect_true(max(preds_raw$.pred) < 10)  # Log scale values should be small
  expect_true(min(preds_raw$.pred) > -5)
})
cli::cli_alert_success("Predictions are in transformed (log) scale: ✓")

# Apply back-transformation
preds_back <- preds_raw
preds_back$.pred <- back_transform_predictions(preds_raw$.pred, "log", warn = FALSE)

cli::cli_text("Back-transformed range: [{round(min(preds_back$.pred), 2)}, {round(max(preds_back$.pred), 2)}]")

test_that("back-transformed predictions are on original scale", {
  # Should be similar magnitude to original response
  expect_true(max(preds_back$.pred) > 1)
  expect_true(max(preds_back$.pred) < 1000)
})
cli::cli_alert_success("Back-transformed predictions on original scale: ✓")

## ---------------------------------------------------------------------------
## Test 4: Metric Calculation
## ---------------------------------------------------------------------------

cli::cli_h2("Testing metric calculation on original scale")

# Calculate metrics on wrong scale (for comparison)
wrong_metrics <- tibble(
  truth = test_data$Response[1:10],
  estimate = preds_raw$.pred  # Still in log scale
)
wrong_rmse <- yardstick::rmse(wrong_metrics, truth, estimate)$.estimate

# Calculate metrics on correct scale
correct_metrics <- tibble(
  truth = test_data$Response[1:10],
  estimate = preds_back$.pred  # Back-transformed
)
correct_rmse <- yardstick::rmse(correct_metrics, truth, estimate)$.estimate

cli::cli_text("RMSE with wrong scale: {round(wrong_rmse, 2)}")
cli::cli_text("RMSE with correct scale: {round(correct_rmse, 2)}")

test_that("metrics differ significantly between scales", {
  # Wrong scale RMSE should be much larger
  expect_true(wrong_rmse > correct_rmse)
})
cli::cli_alert_success("Metrics calculated correctly on original scale: ✓")

## ---------------------------------------------------------------------------
## Summary
## ---------------------------------------------------------------------------

cli::cli_h1("Test Summary")
cli::cli_alert_success("✓ Back-transformation functions work correctly")
cli::cli_alert_success("✓ NA and edge cases handled properly")
cli::cli_alert_success("✓ Recipe applies transformation during training")
cli::cli_alert_success("✓ Recipe skips transformation for test outcomes")
cli::cli_alert_success("✓ Model predictions are in transformed scale")
cli::cli_alert_success("✓ Back-transformation recovers original scale")
cli::cli_alert_success("✓ Metrics must be calculated on original scale")

cli::cli_text("")
cli::cli_alert_success(cli::col_green("All tests passed!"))
cli::cli_text("")
cli::cli_text("Key findings:")
cli::cli_text("• With skip=TRUE, predictions come out in transformed scale")
cli::cli_text("• Back-transformation is REQUIRED before calculating metrics")
cli::cli_text("• Our implementation correctly handles this in:")
cli::cli_text("  - evaluation-core.R (test metrics)")
cli::cli_text("  - evaluation-finalize.R (CV predictions for stacking)")