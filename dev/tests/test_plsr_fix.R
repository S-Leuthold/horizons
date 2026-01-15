#!/usr/bin/env Rscript

# Test PLSR num_comp finalization fix

library(cli)
library(horizons)
library(dplyr)
library(workflows)
library(plsmod)
library(dials)

cli::cli_h1("Testing PLSR num_comp Finalization")

# Create a minimal test dataset with few predictors (to stress test)
set.seed(123)
test_data <- data.frame(
  Response = rnorm(30),
  feat1 = rnorm(30),
  feat2 = rnorm(30),
  feat3 = rnorm(30),
  feat4 = rnorm(30)
)

cli::cli_text("Test dataset: {nrow(test_data)} samples, {ncol(test_data)-1} predictors")

# Create a simple recipe (no feature selection first)
cli::cli_h2("Test 1: PLSR without feature selection")

recipe_simple <- recipes::recipe(Response ~ ., data = test_data)

# Create PLSR model with tunable num_comp
pls_spec <- pls(
  num_comp = tune(),
  predictor_prop = 1
) %>%
  parsnip::set_engine("mixOmics") %>%
  parsnip::set_mode("regression")

# Create workflow
wflow <- workflow() %>%
  add_recipe(recipe_simple) %>%
  add_model(pls_spec)

# Extract parameter set
param_set <- extract_parameter_set_dials(wflow)
cli::cli_text("Initial num_comp range: [{param_set$object[[1]]$range$lower}, {param_set$object[[1]]$range$upper}]")

# Finalize parameters (this is what happens in evaluate_configuration)
if (any(param_set$name %in% c("mtry", "num_comp"))) {
  eval_data <- recipe_simple %>%
    recipes::prep() %>%
    recipes::bake(new_data = NULL) %>%
    select(-Response)
  
  param_set_final <- param_set %>%
    dials::finalize(eval_data)
  
  cli::cli_text("Finalized num_comp range: [{param_set_final$object[[1]]$range$lower}, {param_set_final$object[[1]]$range$upper}]")
  
  # Try to generate a grid
  tryCatch({
    grid <- dials::grid_latin_hypercube(param_set_final, size = 5)
    cli::cli_alert_success("Grid generation successful! Values: {paste(grid$num_comp, collapse = ', ')}")
  }, error = function(e) {
    cli::cli_alert_danger("Grid generation failed: {e$message}")
  })
}

# Test with very aggressive feature selection (simulating Boruta removing most features)
cli::cli_h2("Test 2: PLSR with aggressive feature selection")

recipe_filtered <- recipes::recipe(Response ~ ., data = test_data) %>%
  recipes::step_select(recipes::all_predictors(), 
                      outcome = "Response",
                      top_p = 1)  # Keep only 1 predictor

# Prep the recipe to see how many predictors remain
prepped <- recipes::prep(recipe_filtered)
baked <- recipes::bake(prepped, new_data = NULL)
cli::cli_text("Predictors after feature selection: {ncol(baked) - 1}")

# Create new workflow with filtered recipe
wflow2 <- workflow() %>%
  add_recipe(recipe_filtered) %>%
  add_model(pls_spec)

# Test finalization with very few predictors
param_set2 <- extract_parameter_set_dials(wflow2)

if (any(param_set2$name %in% c("mtry", "num_comp"))) {
  # Get just the predictors (Response might already be removed by recipe)
  eval_data2 <- baked %>% select(-any_of("Response"))
  
  param_set_final2 <- param_set2 %>%
    dials::finalize(eval_data2)
  
  cli::cli_text("Finalized num_comp range with {ncol(eval_data2)} predictors: [{param_set_final2$object[[1]]$range$lower}, {param_set_final2$object[[1]]$range$upper}]")
  
  # Try to generate a grid
  tryCatch({
    grid2 <- dials::grid_latin_hypercube(param_set_final2, size = 5)
    cli::cli_alert_success("Grid generation successful! Values: {paste(grid2$num_comp, collapse = ', ')}")
    
    # Check for invalid values
    if (any(!is.finite(grid2$num_comp))) {
      cli::cli_alert_danger("Grid contains non-finite values!")
    }
  }, error = function(e) {
    cli::cli_alert_danger("Grid generation failed: {e$message}")
  })
}

cli::cli_rule()
cli::cli_alert_success("PLSR finalization test complete")
cli::cli_text("")
cli::cli_text("Key findings:")
cli::cli_bullets(c(
  "i" = "num_comp needs finalization based on number of predictors",
  "i" = "Maximum num_comp = min(n_predictors, n_samples - 1)",
  "i" = "With very few predictors, num_comp range becomes very restricted"
))