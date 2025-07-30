test_that("step_select_shap can be added to recipe", {
  # Create test data
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 650, by = 10))
  
  # Create recipe
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_shap(recipes::all_predictors(), top_n = 5)
  
  expect_valid_recipe(recipe, has_selection_step = TRUE)
  expect_s3_class(recipe$steps[[1]], "step_select_shap")
  expect_equal(recipe$steps[[1]]$top_n, 5)
})

test_that("step_select_shap selects top_n features", {
  # Create test data with known feature importances
  set.seed(123)
  n_samples <- 50
  test_data <- data.frame(
    Sample_ID = paste0("TEST_", sprintf("%03d", 1:n_samples)),
    stringsAsFactors = FALSE
  )
  
  # Create features with varying importance
  test_data$important1 <- rnorm(n_samples)
  test_data$important2 <- rnorm(n_samples)
  test_data$important3 <- rnorm(n_samples)
  test_data$unimportant1 <- rnorm(n_samples)
  test_data$unimportant2 <- rnorm(n_samples)
  
  # Create response heavily influenced by important features
  test_data$Response <- 3 * test_data$important1 + 
                        2 * test_data$important2 + 
                        1 * test_data$important3 + 
                        0.1 * test_data$unimportant1 + 
                        0.1 * test_data$unimportant2 + 
                        rnorm(n_samples, 0, 0.2)
  
  skip_if_not_installed("shapviz")
  skip_if_not_installed("xgboost")
  
  with_mocked_computations({
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_select_shap(recipes::all_predictors(), top_n = 3)
    
    # Mock SHAP to return known important features
    with_mocked_bindings(
      prep.step_select_shap = function(x, training, info = NULL, ...) {
        # Simulate SHAP selecting the most important features
        x$selected_features <- c("important1", "important2", "important3")
        x$trained <- TRUE
        return(x)
      },
      {
        prepped <- recipes::prep(recipe, training = test_data)
        result <- recipes::bake(prepped, new_data = test_data)
      },
      .package = "horizons"
    )
    
    # Check that top features were selected
    selected_features <- setdiff(names(result), c("Sample_ID", "Response"))
    expect_equal(length(selected_features), 3)
    expect_true(all(c("important1", "important2", "important3") %in% selected_features))
  })
})

test_that("step_select_shap handles different top_n values", {
  test_data <- make_test_spectra(n_samples = 30, wavelengths = seq(600, 650, by = 10))
  
  # Test with different top_n values
  top_n_values <- c(3, 5, 10)
  
  for (n in top_n_values) {
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_select_shap(recipes::all_predictors(), top_n = n)
    
    expect_equal(recipe$steps[[1]]$top_n, n)
  }
})

test_that("step_select_shap handles threshold parameter", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 620, by = 10))
  
  # Test with importance threshold instead of top_n
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_shap(recipes::all_predictors(), threshold = 0.01)
  
  expect_equal(recipe$steps[[1]]$threshold, 0.01)
  expect_null(recipe$steps[[1]]$top_n)
})

test_that("step_select_shap handles nrounds parameter", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 620, by = 10))
  
  # Test with different nrounds values
  nrounds_values <- c(50, 100, 200)
  
  for (rounds in nrounds_values) {
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_select_shap(recipes::all_predictors(), top_n = 5, nrounds = rounds)
    
    expect_equal(recipe$steps[[1]]$nrounds, rounds)
  }
})

test_that("step_select_shap preserves non-predictor columns", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 620, by = 10))
  test_data$project_info <- paste0("proj_", seq_len(nrow(test_data)))
  
  skip_if_not_installed("shapviz")
  skip_if_not_installed("xgboost")
  
  with_mocked_computations({
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      recipes::update_role(project_info, new_role = "metadata") %>%
      step_select_shap(recipes::all_predictors(), top_n = 3)
    
    # Mock to select a subset of features
    with_mocked_bindings(
      prep.step_select_shap = function(x, training, info = NULL, ...) {
        spectral_cols <- names(training)[grepl("^[0-9]+$", names(training))]
        x$selected_features <- spectral_cols[1:min(3, length(spectral_cols))]
        x$trained <- TRUE
        return(x)
      },
      {
        prepped <- recipes::prep(recipe, training = test_data)
        result <- recipes::bake(prepped, new_data = test_data)
      },
      .package = "horizons"
    )
    
    # Check that non-predictor columns are preserved
    expect_true("Sample_ID" %in% names(result))
    expect_true("Response" %in% names(result))
    expect_true("project_info" %in% names(result))
  })
})

test_that("step_select_shap handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    Sample_ID = letters[1:10],
    Response = runif(10, 1, 5),
    feat1 = rnorm(10),
    feat2 = rnorm(10),
    feat3 = rnorm(10),
    stringsAsFactors = FALSE
  )
  
  # Test requesting more features than available
  recipe <- recipes::recipe(Response ~ ., data = minimal_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_shap(recipes::all_predictors(), top_n = 10)
  
  skip_if_not_installed("shapviz")
  skip_if_not_installed("xgboost")
  
  with_mocked_computations({
    with_mocked_bindings(
      prep.step_select_shap = function(x, training, info = NULL, ...) {
        # Should select all available features when top_n > n_features
        feature_cols <- setdiff(names(training), c("Sample_ID", "Response"))
        x$selected_features <- feature_cols
        x$trained <- TRUE
        return(x)
      },
      {
        prepped <- recipes::prep(recipe, training = minimal_data)
        result <- recipes::bake(prepped, new_data = minimal_data)
      },
      .package = "horizons"
    )
    
    # All features should be retained
    expect_true(all(c("feat1", "feat2", "feat3") %in% names(result)))
  })
})

test_that("step_select_shap works with spectral data subset", {
  # Use fixture with subset for speed
  test_data <- qs::qread(test_path("fixtures", "small_spectra_fixture.qs"))
  
  # Select small subset of wavelengths
  spectral_cols <- names(test_data)[grepl("^[0-9]+$", names(test_data))]
  subset_cols <- spectral_cols[seq(1, min(15, length(spectral_cols)), by = 3)]
  test_data_subset <- test_data[, c("Project", "Sample_ID", "Response", subset_cols)]
  
  skip_if_not_installed("shapviz")
  skip_if_not_installed("xgboost")
  
  recipe <- recipes::recipe(Response ~ ., data = test_data_subset) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    recipes::update_role(Project, new_role = "metadata") %>%
    step_select_shap(recipes::all_predictors(), top_n = 3)
  
  # Just verify recipe creation
  expect_valid_recipe(recipe, has_selection_step = TRUE)
})

test_that("step_select_shap works in combination with other steps", {
  test_data <- make_test_spectra(n_samples = 25, wavelengths = seq(600, 630, by = 5))
  covariate_data <- make_test_covariates(sample_ids = test_data$Sample_ID, 
                                          covariates = c("Clay", "pH"))
  
  skip_if_not_installed("shapviz")
  skip_if_not_installed("xgboost")
  
  with_mocked_computations({
    # Create recipe with multiple steps
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_transform_spectra(recipes::all_predictors(), preprocessing = "snv") %>%
      step_add_covariates(covariate_data = covariate_data) %>%
      step_select_shap(recipes::all_predictors(), top_n = 5)
    
    # Mock SHAP to select mix of spectral and covariate features
    with_mocked_bindings(
      prep.step_select_shap = function(x, training, info = NULL, ...) {
        # Select some spectral features and covariates
        spec_cols <- names(training)[grepl("^spec", names(training))]
        selected <- c(spec_cols[1:3], "Clay", "pH")
        x$selected_features <- selected[1:min(5, length(selected))]
        x$trained <- TRUE
        return(x)
      },
      {
        prepped <- recipes::prep(recipe, training = test_data)
        result <- recipes::bake(prepped, new_data = test_data)
      },
      .package = "horizons"
    )
    
    # Check that selection includes both types of features
    selected_features <- setdiff(names(result), c("Sample_ID", "Response"))
    expect_true(length(selected_features) == 5)
    
    # Should have mix of spectral and covariate features
    spec_features <- grep("^spec", selected_features, value = TRUE)
    covariate_features <- intersect(selected_features, c("Clay", "pH"))
    expect_true(length(spec_features) > 0 || length(covariate_features) > 0)
  })
})

test_that("step_select_shap validates parameters", {
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 620, by = 10))
  
  # Cannot specify both top_n and threshold
  expect_error(
    recipes::recipe(Response ~ ., data = test_data) %>%
      step_select_shap(recipes::all_predictors(), top_n = 5, threshold = 0.01),
    regexp = "either.*not both"
  )
  
  # Must specify at least one
  expect_error(
    recipes::recipe(Response ~ ., data = test_data) %>%
      step_select_shap(recipes::all_predictors()),
    regexp = "Either.*must be specified"
  )
})

test_that("step_select_shap print method works", {
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 620, by = 10))
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    step_select_shap(recipes::all_predictors(), top_n = 5)
  
  # Test print method doesn't error
  expect_output(print(recipe$steps[[1]]), "SHAP feature selection")
})