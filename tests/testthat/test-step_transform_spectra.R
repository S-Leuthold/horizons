test_that("step_transform_spectra can be added to recipe", {
  # Create test data
  test_data <- make_test_spectra(n_samples = 5, wavelengths = seq(600, 700, by = 2))
  
  # Create recipe
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_transform_spectra(recipes::all_predictors(), preprocessing = "raw")
  
  expect_valid_recipe(recipe, has_spectral_step = TRUE)
  expect_s3_class(recipe$steps[[1]], "step_transform_spectra")
  expect_equal(recipe$steps[[1]]$preprocessing, "raw")
})

test_that("step_transform_spectra handles all preprocessing methods", {
  # Create test data with fewer wavelengths for faster testing
  test_data <- make_test_spectra(n_samples = 3, wavelengths = seq(600, 800, by = 4))
  
  preprocessing_methods <- c("raw", "sg", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2", "msc_deriv1")
  
  for (method in preprocessing_methods) {
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_transform_spectra(recipes::all_predictors(), preprocessing = method)
    
    # Prep and bake the recipe
    prepped <- recipes::prep(recipe, training = test_data)
    result <- recipes::bake(prepped, new_data = test_data)
    
    # Check that result has expected structure
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) == 3)
    expect_true("Sample_ID" %in% names(result))
    expect_true("Response" %in% names(result))
    
    # Check that spectral columns are transformed
    spec_cols <- names(result)[grepl("^spec", names(result))]
    expect_true(length(spec_cols) > 0)
    
    # All spectral values should be numeric and finite
    spec_data <- result[, spec_cols]
    expect_true(all(sapply(spec_data, is.numeric)))
    expect_true(all(is.finite(as.matrix(spec_data))))
  }
})

test_that("step_transform_spectra validates input parameters", {
  test_data <- make_test_spectra(n_samples = 3, wavelengths = seq(600, 700, by = 2))
  
  # Test invalid preprocessing method
  expect_error(
    recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_transform_spectra(recipes::all_predictors(), preprocessing = "invalid_method") %>%
      recipes::prep(training = test_data),
    regexp = "Unknown preprocessing type"
  )
  
  # Test non-numeric columns
  test_data_bad <- test_data
  test_data_bad[["600"]] <- as.character(test_data_bad[["600"]])
  
  expect_error(
    recipes::recipe(Response ~ ., data = test_data_bad) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_transform_spectra(recipes::all_predictors(), preprocessing = "raw") %>%
      recipes::prep(training = test_data_bad),
    regexp = "All spectral columns must be numeric"
  )
})

test_that("step_transform_spectra handles different window sizes", {
  test_data <- make_test_spectra(n_samples = 3, wavelengths = seq(600, 700, by = 2))
  window_sizes <- c(5, 7, 9, 11)
  
  for (ws in window_sizes) {
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_transform_spectra(recipes::all_predictors(), 
                            preprocessing = "deriv1", 
                            window_size = ws)
    
    # Should not error
    prepped <- recipes::prep(recipe, training = test_data)
    result <- recipes::bake(prepped, new_data = test_data)
    
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) == 3)
    
    # Check that trimming occurred based on window size
    spec_cols <- names(result)[grepl("^spec", names(result))]
    original_spec_cols <- names(test_data)[grepl("^[0-9]+$", names(test_data))]
    expected_trim <- (ws - 1) / 2
    expected_cols <- length(original_spec_cols) - 2 * expected_trim
    
    expect_equal(length(spec_cols), expected_cols)
  }
})

test_that("process_spectra function works correctly", {
  # Create simple test vector
  test_vector <- c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0)
  
  # Test raw processing (should trim edges)
  result_raw <- horizons:::process_spectra(test_vector, "raw", window_size = 5)
  expected_length <- length(test_vector) - 4  # Remove 2 from each end
  expect_equal(length(result_raw), expected_length)
  expect_equal(result_raw, test_vector[3:9])
  
  # Test that other methods return vectors of appropriate length
  methods <- c("sg", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2", "msc_deriv1")
  
  for (method in methods) {
    result <- horizons:::process_spectra(test_vector, method, window_size = 5)
    expect_true(is.numeric(result))
    expect_true(length(result) > 0)
    expect_true(all(is.finite(result)))
  }
})

test_that("step_transform_spectra preserves non-spectral columns", {
  test_data <- make_test_spectra(n_samples = 5, wavelengths = seq(600, 700, by = 4))
  
  # Add additional non-spectral column
  test_data$Extra_Info <- paste0("info_", seq_len(nrow(test_data)))
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    recipes::update_role(Extra_Info, new_role = "metadata") %>%
    step_transform_spectra(recipes::all_predictors(), preprocessing = "snv")
  
  prepped <- recipes::prep(recipe, training = test_data)
  result <- recipes::bake(prepped, new_data = test_data)
  
  # Check that non-spectral columns are preserved
  expect_true("Sample_ID" %in% names(result))
  expect_true("Response" %in% names(result))
  expect_true("Extra_Info" %in% names(result))
  
  # Check that values are preserved
  expect_equal(result$Sample_ID, test_data$Sample_ID)
  expect_equal(result$Response, test_data$Response)
  expect_equal(result$Extra_Info, test_data$Extra_Info)
})

test_that("step_transform_spectra works with real fixture data", {
  # Load real test fixture
  test_data <- qs::qread(test_path("fixtures", "small_spectra_fixture.qs"))
  
  # Test with a subset of wavelengths for speed
  spectral_cols <- names(test_data)[grepl("^[0-9]+$", names(test_data))]
  subset_cols <- spectral_cols[seq(1, length(spectral_cols), by = 10)]  # Every 10th column
  test_data_subset <- test_data[, c("Project", "Sample_ID", "Response", subset_cols)]
  
  recipe <- recipes::recipe(Response ~ ., data = test_data_subset) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    recipes::update_role(Project, new_role = "metadata") %>%
    step_transform_spectra(recipes::all_predictors(), preprocessing = "snv_deriv1")
  
  prepped <- recipes::prep(recipe, training = test_data_subset)
  result <- recipes::bake(prepped, new_data = test_data_subset)
  
  expect_valid_spectral_data(result, has_response = TRUE)
  expect_equal(nrow(result), nrow(test_data_subset))
  
  # Check that spectral processing occurred
  spec_cols <- names(result)[grepl("^spec", names(result))]
  expect_true(length(spec_cols) > 0)
  expect_true(length(spec_cols) < length(subset_cols))  # Should be trimmed
})

test_that("step_transform_spectra handles edge cases", {
  # Test with minimal spectral data
  minimal_data <- data.frame(
    Sample_ID = c("A", "B"),
    Response = c(1.0, 2.0),
    `600` = c(0.5, 0.6),
    `602` = c(0.6, 0.7),
    `604` = c(0.7, 0.8),
    `606` = c(0.8, 0.9),
    `608` = c(0.9, 1.0),
    check.names = FALSE
  )
  
  # Should work with small window size
  recipe <- recipes::recipe(Response ~ ., data = minimal_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_transform_spectra(recipes::all_predictors(), 
                          preprocessing = "raw", 
                          window_size = 3)
  
  prepped <- recipes::prep(recipe, training = minimal_data)
  result <- recipes::bake(prepped, new_data = minimal_data)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  
  # Check that some spectral columns remain after trimming
  spec_cols <- names(result)[grepl("^spec", names(result))]
  expect_true(length(spec_cols) > 0)
})

test_that("step_transform_spectra print method works", {
  test_data <- make_test_spectra(n_samples = 3, wavelengths = seq(600, 700, by = 4))
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    step_transform_spectra(recipes::all_predictors(), preprocessing = "snv")
  
  # Test print method doesn't error
  expect_output(print(recipe$steps[[1]]), "Spectral transformation")
})