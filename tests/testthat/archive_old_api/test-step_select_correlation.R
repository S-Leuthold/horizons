test_that("step_select_correlation can be added to recipe", {
  # Create test data
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 700, by = 10))
  
  # Create recipe
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.9)
  
  expect_valid_recipe(recipe, has_selection_step = TRUE)
  expect_s3_class(recipe$steps[[1]], "step_select_correlation")
  expect_equal(recipe$steps[[1]]$threshold, 0.9)
})

test_that("step_select_correlation removes highly correlated features", {
  # Create test data with some highly correlated features
  set.seed(123)
  test_data <- data.frame(
    Sample_ID = paste0("TEST_", sprintf("%03d", 1:20)),
    Response = runif(20, 0.5, 5.0),
    feat1 = rnorm(20),
    stringsAsFactors = FALSE
  )
  
  # Add correlated features
  test_data$feat2 <- test_data$feat1 + rnorm(20, 0, 0.1)  # Highly correlated with feat1
  test_data$feat3 <- test_data$feat1 + rnorm(20, 0, 0.5)  # Moderately correlated
  test_data$feat4 <- rnorm(20)  # Independent
  test_data$feat5 <- test_data$feat4 + rnorm(20, 0, 0.05)  # Highly correlated with feat4
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.9)
  
  prepped <- recipes::prep(recipe, training = test_data)
  result <- recipes::bake(prepped, new_data = test_data)
  
  # Check that some features were removed
  original_features <- c("feat1", "feat2", "feat3", "feat4", "feat5")
  remaining_features <- intersect(names(result), original_features)
  
  expect_true(length(remaining_features) < length(original_features))
  expect_true(length(remaining_features) >= 2)  # At least some features should remain
  
  # Check that result still has essential columns
  expect_true("Sample_ID" %in% names(result))
  expect_true("Response" %in% names(result))
})

test_that("step_select_correlation handles different thresholds", {
  # Create test data
  set.seed(456)
  test_data <- data.frame(
    Sample_ID = paste0("TEST_", sprintf("%03d", 1:30)),
    Response = runif(30, 0.5, 5.0),
    stringsAsFactors = FALSE
  )
  
  # Add features with varying correlations
  for (i in 1:10) {
    test_data[[paste0("feat", i)]] <- rnorm(30)
  }
  
  # Test different thresholds
  thresholds <- c(0.7, 0.8, 0.9, 0.95)
  n_features <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_select_correlation(recipes::all_predictors(), threshold = thresholds[i])
    
    prepped <- recipes::prep(recipe, training = test_data)
    result <- recipes::bake(prepped, new_data = test_data)
    
    feature_cols <- setdiff(names(result), c("Sample_ID", "Response"))
    n_features[i] <- length(feature_cols)
  }
  
  # Lower thresholds should result in fewer features (more removal)
  expect_true(all(diff(n_features) >= 0))
})

test_that("step_select_correlation preserves all features when no high correlations", {
  # Create test data with independent features
  set.seed(789)
  test_data <- data.frame(
    Sample_ID = paste0("TEST_", sprintf("%03d", 1:50)),
    Response = runif(50, 0.5, 5.0),
    stringsAsFactors = FALSE
  )
  
  # Add truly independent features
  for (i in 1:5) {
    test_data[[paste0("feat", i)]] <- rnorm(50)
  }
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.8)
  
  prepped <- recipes::prep(recipe, training = test_data)
  result <- recipes::bake(prepped, new_data = test_data)
  
  # All features should be preserved
  original_features <- paste0("feat", 1:5)
  remaining_features <- intersect(names(result), original_features)
  
  expect_equal(length(remaining_features), length(original_features))
})

test_that("step_select_correlation handles missing values", {
  # Create test data with missing values
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 650, by = 10))
  
  # Introduce some missing values
  test_data[c(1, 3, 5), "610"] <- NA
  test_data[c(2, 4), "620"] <- NA
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.9)
  
  # Should handle missing values gracefully
  expect_no_error({
    prepped <- recipes::prep(recipe, training = test_data)
    result <- recipes::bake(prepped, new_data = test_data)
  })
})

test_that("step_select_correlation works with spectral data", {
  # Use real fixture data
  test_data <- qs::qread(test_path("fixtures", "small_spectra_fixture.qs"))
  
  # Select subset of wavelengths for faster testing
  spectral_cols <- names(test_data)[grepl("^[0-9]+$", names(test_data))]
  subset_cols <- spectral_cols[seq(1, length(spectral_cols), by = 50)]
  test_data_subset <- test_data[, c("Project", "Sample_ID", "Response", subset_cols)]
  
  recipe <- recipes::recipe(Response ~ ., data = test_data_subset) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    recipes::update_role(Project, new_role = "metadata") %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.95)
  
  prepped <- recipes::prep(recipe, training = test_data_subset)
  result <- recipes::bake(prepped, new_data = test_data_subset)
  
  # Check that some spectral features were selected
  selected_features <- setdiff(names(result), c("Sample_ID", "Project", "Response"))
  expect_true(length(selected_features) > 0)
  expect_true(length(selected_features) < length(subset_cols))
  
  # Validate selected features
  expect_valid_feature_selection(
    selected_features,
    subset_cols,
    min_selected = 1,
    max_selected = length(subset_cols)
  )
})

test_that("step_select_correlation handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    Sample_ID = c("A", "B", "C"),
    Response = c(1.0, 2.0, 3.0),
    feat1 = c(0.5, 0.6, 0.7),
    feat2 = c(0.5, 0.6, 0.7),  # Perfectly correlated
    stringsAsFactors = FALSE
  )
  
  recipe <- recipes::recipe(Response ~ ., data = minimal_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.99)
  
  prepped <- recipes::prep(recipe, training = minimal_data)
  result <- recipes::bake(prepped, new_data = minimal_data)
  
  # One of the perfectly correlated features should be removed
  feature_cols <- setdiff(names(result), c("Sample_ID", "Response"))
  expect_equal(length(feature_cols), 1)
})

test_that("step_select_correlation works in combination with other steps", {
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 700, by = 5))
  covariate_data <- make_test_covariates(sample_ids = test_data$Sample_ID, 
                                          covariates = c("Clay", "pH"))
  
  # Create recipe with multiple steps
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_transform_spectra(recipes::all_predictors(), preprocessing = "snv") %>%
    step_add_covariates(covariate_data = covariate_data) %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.9)
  
  prepped <- recipes::prep(recipe, training = test_data)
  result <- recipes::bake(prepped, new_data = test_data)
  
  # Check that all steps were applied
  expect_true("Clay" %in% names(result) || "pH" %in% names(result))  # At least one covariate
  spec_cols <- names(result)[grepl("^spec", names(result))]
  expect_true(length(spec_cols) > 0)  # Some spectral features remain
})

test_that("step_select_correlation print method works", {
  test_data <- make_test_spectra(n_samples = 5, wavelengths = seq(600, 650, by = 10))
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    step_select_correlation(recipes::all_predictors(), threshold = 0.9)
  
  # Test print method doesn't error
  expect_output(print(recipe$steps[[1]]), "Correlation feature selection")
})