test_that("complete modeling pipeline works end-to-end", {
  # Create comprehensive test data
  test_data <- make_test_spectra(n_samples = 30, wavelengths = seq(600, 800, by = 10))
  covariate_data <- make_test_covariates(
    sample_ids = test_data$Sample_ID,
    covariates = c("Clay", "pH", "SOC")
  )
  
  # Build complete recipe
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv_deriv1",
    response_transformation = "Log Transformation",
    feature_selection_method = "correlation",
    covariate_selection = c("Clay", "pH"),
    covariate_data = covariate_data
  )
  
  # Create model specification
  skip_if_not_installed("ranger")
  model_spec <- define_model_specifications("random_forest")
  
  # Finalize the model spec with concrete values instead of tuning
  model_spec_final <- model_spec %>%
    parsnip::set_args(
      mtry = 5,
      trees = 100,
      min_n = 10
    )
  
  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec_final)
  
  # Fit workflow
  set.seed(123)
  splits <- rsample::initial_split(test_data, prop = 0.7)
  train_data <- rsample::training(splits)
  test_data_split <- rsample::testing(splits)
  
  # Fit directly without tuning
  fitted_wf <- workflows::fit(wf, train_data)
  
  # Generate predictions
  predictions <- predict(fitted_wf, test_data_split)
  
  # Validate predictions
  expect_valid_predictions(predictions, min_value = 0.1, max_value = 10)
  expect_equal(nrow(predictions), nrow(test_data_split))
})

test_that("pipeline handles different preprocessing combinations", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 700, by = 20))
  
  # Test different combinations
  combinations <- list(
    list(spectral = "raw", response = "No Transformation", selection = "none"),
    list(spectral = "snv", response = "Log Transformation", selection = "correlation"),
    list(spectral = "deriv1", response = "Square Root Transformation", selection = "none")
  )
  
  for (combo in combinations) {
    recipe <- build_recipe(
      input_data = test_data,
      spectral_transformation = combo$spectral,
      response_transformation = combo$response,
      feature_selection_method = combo$selection
    )
    
    # Should be able to prep and bake
    prepped <- recipes::prep(recipe, training = test_data)
    result <- recipes::bake(prepped, new_data = test_data)
    
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(test_data))
    expect_true("Sample_ID" %in% names(result))
    expect_true("Response" %in% names(result))
  }
})

test_that("pipeline works with real fixture data", {
  # Load real fixture
  test_data <- qs::qread(test_path("fixtures", "small_spectra_fixture.qs"))
  
  # Use subset for speed
  spectral_cols <- names(test_data)[grepl("^[0-9]+$", names(test_data))]
  subset_cols <- spectral_cols[seq(1, length(spectral_cols), by = 20)]
  test_data_subset <- test_data[1:8, c("Project", "Sample_ID", "Response", subset_cols)]
  
  covariate_data <- make_test_covariates(
    sample_ids = test_data_subset$Sample_ID,
    covariates = c("Clay", "SOC")
  )
  
  # Build pipeline
  recipe <- build_recipe(
    input_data = test_data_subset,
    spectral_transformation = "snv",
    response_transformation = "No Transformation",
    feature_selection_method = "none",
    covariate_selection = c("Clay"),
    covariate_data = covariate_data
  )
  
  skip_if_not_installed("ranger")
  model_spec <- define_model_specifications("random_forest")
  
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec)
  
  # Should be able to fit
  fitted_wf <- workflows::fit(wf, test_data_subset)
  
  # Should be able to predict
  predictions <- predict(fitted_wf, test_data_subset)
  expect_valid_predictions(predictions)
})

test_that("pipeline handles edge cases gracefully", {
  # Test with minimal data
  minimal_data <- data.frame(
    Sample_ID = c("A", "B", "C", "D"),
    Response = c(1.0, 2.0, 3.0, 4.0),
    `600` = c(0.5, 0.6, 0.7, 0.8),
    `602` = c(0.6, 0.7, 0.8, 0.9),
    `604` = c(0.7, 0.8, 0.9, 1.0),
    check.names = FALSE
  )
  
  recipe <- build_recipe(
    input_data = minimal_data,
    spectral_transformation = "raw",
    response_transformation = "No Transformation", 
    feature_selection_method = "none"
  )
  
  prepped <- recipes::prep(recipe, training = minimal_data)
  result <- recipes::bake(prepped, new_data = minimal_data)
  
  expect_equal(nrow(result), 4)
  expect_true("Sample_ID" %in% names(result))
})

test_that("pipeline preserves sample tracking through all steps", {
  test_data <- make_test_spectra(n_samples = 15, wavelengths = seq(600, 650, by = 10), seed = 456)
  original_ids <- test_data$Sample_ID
  
  covariate_data <- make_test_covariates(
    sample_ids = test_data$Sample_ID,
    covariates = c("Clay", "pH")
  )
  
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv_deriv1",
    response_transformation = "Log Transformation",
    feature_selection_method = "correlation",
    covariate_selection = c("Clay"),
    covariate_data = covariate_data
  )
  
  prepped <- recipes::prep(recipe, training = test_data)
  result <- recipes::bake(prepped, new_data = test_data)
  
  # Sample IDs should be preserved and in same order
  expect_true("Sample_ID" %in% names(result))
  expect_equal(result$Sample_ID, original_ids)
  expect_equal(nrow(result), length(original_ids))
})

test_that("pipeline handles missing values appropriately", {
  # Create data with some missing values
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 650, by = 10))
  
  # Introduce missing values
  test_data[c(2, 5), "610"] <- NA
  test_data[c(3, 7), "620"] <- NA
  
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "No Transformation",
    feature_selection_method = "none"
  )
  
  # Should handle missing values without error
  expect_no_error({
    prepped <- recipes::prep(recipe, training = test_data)
    result <- recipes::bake(prepped, new_data = test_data)
  })
  
  expect_equal(nrow(result), nrow(test_data))
})

test_that("pipeline works with multiple models", {
  test_data <- make_test_spectra(n_samples = 25, wavelengths = seq(600, 700, by = 15))
  
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv",
    response_transformation = "No Transformation",
    feature_selection_method = "none"
  )
  
  # Test multiple model types that are commonly available
  model_types <- c("random_forest")
  
  # Add other models if packages available
  if (requireNamespace("Cubist", quietly = TRUE)) {
    model_types <- c(model_types, "cubist")
  }
  if (requireNamespace("glmnet", quietly = TRUE)) {
    model_types <- c(model_types, "elastic_net")
  }
  
  for (model_type in model_types) {
    model_spec <- define_model_specifications(model_type)
    
    wf <- workflows::workflow() %>%
      workflows::add_recipe(recipe) %>%
      workflows::add_model(model_spec)
    
    # Should be able to create workflow
    expect_s3_class(wf, "workflow")
    
    # Should be able to fit (test with subset for speed)
    train_subset <- test_data[1:10, ]
    fitted_wf <- workflows::fit(wf, train_subset)
    
    # Should be able to predict
    predictions <- predict(fitted_wf, train_subset)
    expect_valid_predictions(predictions)
  }
})

test_that("pipeline maintains reproducibility", {
  test_data <- make_test_spectra(n_samples = 15, wavelengths = seq(600, 650, by = 10), seed = 789)
  
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv",
    response_transformation = "No Transformation",
    feature_selection_method = "none"
  )
  
  # Run pipeline twice with same seed
  set.seed(123)
  prepped1 <- recipes::prep(recipe, training = test_data)
  result1 <- recipes::bake(prepped1, new_data = test_data)
  
  set.seed(123)
  prepped2 <- recipes::prep(recipe, training = test_data)
  result2 <- recipes::bake(prepped2, new_data = test_data)
  
  # Results should be identical
  expect_equal(result1, result2)
})

test_that("pipeline handles train/test split correctly", {
  # Create larger dataset for proper split
  test_data <- make_test_spectra(n_samples = 40, wavelengths = seq(600, 700, by = 15))
  
  # Split data
  set.seed(456)
  splits <- rsample::initial_split(test_data, prop = 0.6)
  train_data <- rsample::training(splits)
  test_data_split <- rsample::testing(splits)
  
  recipe <- build_recipe(
    input_data = train_data,  # Use only training data for recipe prep
    spectral_transformation = "snv",
    response_transformation = "No Transformation",
    feature_selection_method = "none"
  )
  
  skip_if_not_installed("ranger")
  model_spec <- define_model_specifications("random_forest")
  
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec)
  
  # Fit on training data
  fitted_wf <- workflows::fit(wf, train_data)
  
  # Predict on test data
  predictions <- predict(fitted_wf, test_data_split)
  
  expect_equal(nrow(predictions), nrow(test_data_split))
  expect_valid_predictions(predictions)
  
  # Predictions should be different from training responses
  train_responses <- train_data$Response
  test_predictions <- predictions$.pred
  
  # Should not be identical (would indicate overfitting/data leakage)
  expect_false(identical(train_responses[1:length(test_predictions)], test_predictions))
})