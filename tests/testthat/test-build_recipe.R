test_that("build_recipe creates valid recipe with basic parameters", {
  # Create test data
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 700, by = 10))
  
  # Test basic recipe creation
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "No Transformation",
    feature_selection_method = "none"
  )
  
  expect_valid_recipe(recipe)
  expect_s3_class(recipe, "recipe")
  
  # Check that Sample_ID has correct role
  var_info <- recipe$var_info
  sample_id_role <- var_info$role[var_info$variable == "Sample_ID"]
  expect_equal(sample_id_role, "id")
})

test_that("build_recipe handles all spectral transformations", {
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 650, by = 10))
  
  spectral_methods <- c("raw", "sg", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2", "msc_deriv1")
  
  for (method in spectral_methods) {
    recipe <- build_recipe(
      input_data = test_data,
      spectral_transformation = method,
      response_transformation = "No Transformation",
      feature_selection_method = "none"
    )
    
    expect_valid_recipe(recipe, has_spectral_step = TRUE)
    
    # Find spectral transformation step
    spectral_step <- purrr::keep(recipe$steps, ~ inherits(.x, "step_transform_spectra"))
    expect_length(spectral_step, 1)
    expect_equal(spectral_step[[1]]$preprocessing, method)
  }
})

test_that("build_recipe handles all response transformations", {
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 650, by = 10))
  
  response_methods <- c("No Transformation", "Log Transformation", "Square Root Transformation")
  
  for (method in response_methods) {
    recipe <- build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = method,
      feature_selection_method = "none"
    )
    
    expect_valid_recipe(recipe)
    
    # Check for appropriate transformation step
    if (method == "Log Transformation") {
      log_steps <- purrr::keep(recipe$steps, ~ inherits(.x, "step_log"))
      expect_length(log_steps, 1)
      expect_true(log_steps[[1]]$skip)  # Should have skip = TRUE
    } else if (method == "Square Root Transformation") {
      sqrt_steps <- purrr::keep(recipe$steps, ~ inherits(.x, "step_sqrt"))
      expect_length(sqrt_steps, 1)
      expect_true(sqrt_steps[[1]]$skip)  # Should have skip = TRUE
    }
  }
})

test_that("build_recipe handles feature selection methods", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 650, by = 10))
  
  # Test each feature selection method
  selection_configs <- list(
    list(method = "none", expected_step = NULL),
    list(method = "correlation", expected_step = "step_select_correlation"),
    list(method = "boruta", expected_step = "step_select_boruta"),
    list(method = "shap", expected_step = "step_select_shap")
  )
  
  for (config in selection_configs) {
    recipe <- build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = "No Transformation",
      feature_selection_method = config$method
    )
    
    expect_valid_recipe(recipe)
    
    if (!is.null(config$expected_step)) {
      selection_step <- purrr::keep(recipe$steps, ~ inherits(.x, config$expected_step))
      expect_length(selection_step, 1)
    }
  }
})

test_that("build_recipe integrates covariates correctly", {
  test_data <- make_test_spectra(n_samples = 15, wavelengths = seq(600, 650, by = 10))
  covariate_data <- make_test_covariates(
    sample_ids = test_data$Sample_ID,
    covariates = c("Clay", "pH", "SOC")
  )
  
  # Test with covariates
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv",
    response_transformation = "No Transformation",
    feature_selection_method = "none",
    covariate_selection = c("Clay", "pH"),
    covariate_data = covariate_data
  )
  
  expect_valid_recipe(recipe, has_spectral_step = TRUE, has_covariate_step = TRUE)
  
  # Check that covariate step exists
  covariate_step <- purrr::keep(recipe$steps, ~ inherits(.x, "step_add_covariates"))
  expect_length(covariate_step, 1)
})

test_that("build_recipe handles 'No Covariates' specification", {
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 650, by = 10))
  covariate_data <- make_test_covariates(sample_ids = test_data$Sample_ID)
  
  # Various ways to specify no covariates
  no_covariate_specs <- list(
    "No Covariates",
    c("No Covariates"),
    NULL,
    character(0),
    NA
  )
  
  for (spec in no_covariate_specs) {
    recipe <- build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = "No Transformation",
      feature_selection_method = "none",
      covariate_selection = spec,
      covariate_data = covariate_data
    )
    
    # Should not have covariate step
    covariate_steps <- purrr::keep(recipe$steps, ~ inherits(.x, "step_add_covariates"))
    expect_length(covariate_steps, 0)
  }
})

test_that("build_recipe includes PCA step", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 700, by = 5))
  
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv",
    response_transformation = "No Transformation",
    feature_selection_method = "none"
  )
  
  # Should have PCA step
  pca_steps <- purrr::keep(recipe$steps, ~ inherits(.x, "step_pca"))
  expect_length(pca_steps, 1)
  
  # PCA should be on predictors
  expect_true(any(grepl("all_predictors", as.character(pca_steps[[1]]$terms))))
})

test_that("build_recipe validates input data", {
  # Test with non-data.frame input
  expect_error(
    build_recipe(
      input_data = matrix(1:10, ncol = 2),
      spectral_transformation = "raw",
      response_transformation = "No Transformation",
      feature_selection_method = "none"
    ),
    regexp = "not a data frame"
  )
  
  # Test missing Sample_ID
  bad_data <- make_test_spectra(n_samples = 5)
  bad_data$Sample_ID <- NULL
  
  expect_error(
    build_recipe(
      input_data = bad_data,
      spectral_transformation = "raw",
      response_transformation = "No Transformation",
      feature_selection_method = "none"
    ),
    regexp = "Sample_ID.*missing"
  )
})

test_that("build_recipe validates covariate parameters", {
  test_data <- make_test_spectra(n_samples = 10)
  
  # Test requesting covariates without providing covariate_data
  expect_error(
    build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = "No Transformation",
      feature_selection_method = "none",
      covariate_selection = c("Clay", "pH"),
      covariate_data = NULL
    ),
    regexp = "covariate_data.*missing"
  )
  
  # Test requesting missing covariates
  covariate_data <- make_test_covariates(
    sample_ids = test_data$Sample_ID,
    covariates = c("Clay")  # Only Clay, not pH
  )
  
  expect_error(
    build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = "No Transformation",
      feature_selection_method = "none",
      covariate_selection = c("Clay", "pH"),
      covariate_data = covariate_data
    ),
    regexp = "not found.*pH"
  )
})

test_that("build_recipe creates complete pipeline", {
  # Test full pipeline with all components
  test_data <- make_test_spectra(n_samples = 25, wavelengths = seq(600, 700, by = 5))
  test_data$Project <- "TEST_PROJECT"
  
  covariate_data <- make_test_covariates(
    sample_ids = test_data$Sample_ID,
    covariates = c("Clay", "pH", "SOC", "Depth")
  )
  
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv_deriv1",
    response_transformation = "Log Transformation",
    feature_selection_method = "correlation",
    covariate_selection = c("Clay", "pH"),
    covariate_data = covariate_data
  )
  
  # Check recipe has all expected components
  expect_valid_recipe(recipe, 
                      has_spectral_step = TRUE, 
                      has_covariate_step = TRUE,
                      has_selection_step = TRUE)
  
  # Check roles are set correctly
  var_info <- recipe$var_info
  expect_equal(var_info$role[var_info$variable == "Sample_ID"], "id")
  expect_equal(var_info$role[var_info$variable == "Project"], "metadata")
  expect_equal(var_info$role[var_info$variable == "Response"], "outcome")
  
  # Check step order makes sense
  step_classes <- purrr::map_chr(recipe$steps, ~ class(.x)[1])
  
  # Response transformation should come early
  log_idx <- which(step_classes == "step_log")
  expect_true(length(log_idx) == 1)
  
  # Spectral transformation should come before feature selection
  spectral_idx <- which(step_classes == "step_transform_spectra")
  selection_idx <- which(step_classes == "step_select_correlation")
  expect_true(spectral_idx < selection_idx)
  
  # PCA should come last
  pca_idx <- which(step_classes == "step_pca")
  expect_equal(pca_idx, length(step_classes))
})

test_that("build_recipe works with real fixture data", {
  # Load real test fixture
  test_data <- qs::qread(test_path("fixtures", "small_spectra_fixture.qs"))
  
  # Create covariates for the fixture
  covariate_data <- make_test_covariates(
    sample_ids = test_data$Sample_ID,
    covariates = c("Clay", "pH", "SOC", "Sand")
  )
  
  recipe <- build_recipe(
    input_data = test_data,
    spectral_transformation = "snv_deriv1",
    response_transformation = "Square Root Transformation",
    feature_selection_method = "none",
    covariate_selection = c("Clay", "SOC"),
    covariate_data = covariate_data
  )
  
  # Prep the recipe
  prepped <- recipes::prep(recipe, training = test_data)
  
  # Bake the recipe
  result <- recipes::bake(prepped, new_data = test_data)
  
  # Validate result structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(test_data))
  
  # Check that transformations were applied
  expect_true("Sample_ID" %in% names(result))
  expect_true("Response" %in% names(result))
  expect_true("Clay" %in% names(result) || "SOC" %in% names(result))
  
  # Should have PCA components
  pca_cols <- grep("^PC", names(result), value = TRUE)
  expect_true(length(pca_cols) > 0)
})

test_that("build_recipe handles edge cases", {
  # Minimal data
  minimal_data <- data.frame(
    Sample_ID = c("A", "B", "C"),
    Response = c(1.0, 2.0, 3.0),
    `600` = c(0.5, 0.6, 0.7),
    `602` = c(0.6, 0.7, 0.8),
    `604` = c(0.7, 0.8, 0.9),
    check.names = FALSE
  )
  
  recipe <- build_recipe(
    input_data = minimal_data,
    spectral_transformation = "raw",
    response_transformation = "No Transformation",
    feature_selection_method = "none"
  )
  
  expect_valid_recipe(recipe)
  
  # Can prep and bake
  prepped <- recipes::prep(recipe, training = minimal_data)
  result <- recipes::bake(prepped, new_data = minimal_data)
  
  expect_equal(nrow(result), 3)
})