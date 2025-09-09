#!/usr/bin/env Rscript

# Minimal test script to diagnose model execution failures
library(horizons)
library(tidymodels)
library(dplyr)
library(cli)

cli::cli_h1("Minimal Model Execution Test")

# Create minimal test data
set.seed(123)
n_samples <- 100
n_wavelengths <- 50

test_data <- tibble::tibble(
  Sample_ID = paste0("S", 1:n_samples),
  Response = rnorm(n_samples, mean = 10, sd = 2)
)

# Add spectral columns (numeric names as expected)
wavelengths <- seq(600, 4000, length.out = n_wavelengths)
for (wl in as.character(wavelengths)) {
  test_data[[wl]] <- runif(n_samples, 0.3, 0.7) + rnorm(n_samples, 0, 0.01)
}

cli::cli_alert_success("Created test data: {nrow(test_data)} samples, {n_wavelengths} wavelengths")

# Test 1: Basic recipe building
cli::cli_h2("Test 1: Recipe Building")
tryCatch({
  recipe <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "none",
    feature_selection_method = "pca",
    covariate_selection = NULL,
    covariate_data = NULL
  )
  cli::cli_alert_success("Recipe created successfully")
  print(recipe)
}, error = function(e) {
  cli::cli_alert_danger("Recipe creation failed: {e$message}")
  print(e)
})

# Test 2: Model specification
cli::cli_h2("Test 2: Model Specification")
tryCatch({
  model_spec <- horizons:::define_model_specifications("random_forest")
  cli::cli_alert_success("Model spec created successfully")
  print(model_spec)
}, error = function(e) {
  cli::cli_alert_danger("Model spec failed: {e$message}")
  print(e)
})

# Test 3: Workflow creation
cli::cli_h2("Test 3: Workflow Creation")
tryCatch({
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    recipes::step_pca(recipes::all_predictors(), threshold = 0.95)
  
  model_spec <- parsnip::rand_forest(
    trees = 100,
    mtry = tune::tune(),
    min_n = tune::tune()
  ) %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("regression")
  
  workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec)
  
  cli::cli_alert_success("Workflow created successfully")
  print(workflow)
}, error = function(e) {
  cli::cli_alert_danger("Workflow creation failed: {e$message}")
  print(e)
})

# Test 4: Simple model fit (no tuning)
cli::cli_h2("Test 4: Simple Model Fit")
tryCatch({
  # Create a simple workflow without tuning parameters
  simple_model <- parsnip::rand_forest(
    trees = 100,
    mtry = 5,
    min_n = 10
  ) %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("regression")
  
  simple_recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id")
  
  simple_workflow <- workflows::workflow() %>%
    workflows::add_recipe(simple_recipe) %>%
    workflows::add_model(simple_model)
  
  # Fit the model
  fitted <- workflows::fit(simple_workflow, data = test_data)
  
  cli::cli_alert_success("Simple model fitted successfully")
  
  # Test prediction
  preds <- predict(fitted, test_data)
  cli::cli_alert_info("Predictions generated: {nrow(preds)} values")
  
}, error = function(e) {
  cli::cli_alert_danger("Simple fit failed: {e$message}")
  print(e)
  if (exists("e$trace")) print(e$trace)
})

# Test 5: Cross-validation with tuning (where closure errors often occur)
cli::cli_h2("Test 5: Cross-Validation with Tuning")
tryCatch({
  # Create CV splits
  cv_splits <- rsample::vfold_cv(test_data, v = 3, strata = Response)
  
  # Create workflow with tuning parameters
  tune_workflow <- workflows::workflow() %>%
    workflows::add_recipe(simple_recipe) %>%
    workflows::add_model(model_spec)
  
  # Grid search
  grid <- dials::grid_latin_hypercube(
    dials::mtry(range = c(2, 10)),
    dials::min_n(range = c(2, 20)),
    size = 5
  )
  
  cli::cli_alert_info("Starting grid search with {nrow(grid)} combinations...")
  
  # This is where closure errors often occur
  grid_results <- tune::tune_grid(
    object = tune_workflow,
    resamples = cv_splits,
    grid = grid,
    metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq),
    control = tune::control_grid(
      verbose = TRUE,
      save_pred = FALSE,
      save_workflow = FALSE
    )
  )
  
  cli::cli_alert_success("Grid search completed successfully")
  
  # Show results
  best <- tune::show_best(grid_results, metric = "rmse", n = 1)
  print(best)
  
}, error = function(e) {
  cli::cli_alert_danger("Cross-validation failed: {e$message}")
  print(e)
  
  # Try to get more details about the error
  if (!is.null(e$call)) {
    cli::cli_alert_info("Error occurred in: {deparse(e$call)}")
  }
  
  # Check for closure-specific errors
  if (grepl("closure|environment|serialize", e$message, ignore.case = TRUE)) {
    cli::cli_alert_warning("This appears to be a closure/serialization error")
    cli::cli_alert_info("Common causes:")
    cli::cli_bullets(c(
      "*" = "Functions capturing large environments",
      "*" = "Non-exportable objects in recipe steps",
      "*" = "Custom functions not available to workers"
    ))
  }
})

# Test 6: Check for environment issues
cli::cli_h2("Test 6: Environment Diagnostics")
tryCatch({
  # Check if we're in an HPC environment
  is_hpc <- Sys.getenv("SLURM_JOB_ID") != "" || 
            Sys.getenv("PBS_JOBID") != "" ||
            Sys.getenv("LSB_JOBID") != ""
  
  cli::cli_alert_info("Running on HPC: {is_hpc}")
  cli::cli_alert_info("R version: {R.version.string}")
  cli::cli_alert_info("Platform: {Sys.info()['sysname']}")
  cli::cli_alert_info("Number of cores: {parallel::detectCores()}")
  
  # Check loaded packages
  loaded_pkgs <- search()
  cli::cli_alert_info("Loaded packages: {length(loaded_pkgs)}")
  
  # Check for parallel backend
  if (requireNamespace("foreach", quietly = TRUE)) {
    backend <- foreach::getDoParName()
    workers <- foreach::getDoParWorkers()
    cli::cli_alert_info("Parallel backend: {backend} with {workers} workers")
  }
  
}, error = function(e) {
  cli::cli_alert_danger("Environment check failed: {e$message}")
})

cli::cli_rule()
cli::cli_alert_info("Test complete. Check output above for failures.")