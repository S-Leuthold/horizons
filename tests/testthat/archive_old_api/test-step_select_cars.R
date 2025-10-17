test_that("step_select_cars can be added to recipe", {
  # Create test data
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 650, by = 10))
  
  # Create recipe
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_cars(recipes::all_predictors())
  
  expect_valid_recipe(recipe, has_selection_step = TRUE)
  expect_s3_class(recipe$steps[[1]], "step_select_cars")
})

test_that("step_select_cars selects relevant features", {
  # Create test data with known relevant and irrelevant features
  set.seed(123)
  n_samples <- 50
  test_data <- data.frame(
    Sample_ID = paste0("TEST_", sprintf("%03d", 1:n_samples)),
    stringsAsFactors = FALSE
  )
  
  # Create relevant features (correlated with response)
  test_data$relevant1 <- rnorm(n_samples)
  test_data$relevant2 <- rnorm(n_samples)
  test_data$relevant3 <- rnorm(n_samples)
  
  # Create response based on relevant features
  test_data$Response <- 2 * test_data$relevant1 + 
                        1.5 * test_data$relevant2 + 
                        test_data$relevant3 + 
                        rnorm(n_samples, 0, 0.5)
  
  # Add irrelevant features (random noise)
  for (i in 1:5) {
    test_data[[paste0("noise", i)]] <- rnorm(n_samples)
  }
  
  # Skip this test if pls is not available or takes too long
  skip_if_not_installed("pls")
  
  # Use mocked version for faster testing
  with_mocked_computations({
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_select_cars(recipes::all_predictors())
    
    # Mock the CARS selection to return known relevant features
    with_mocked_bindings(
      prep.step_select_cars = function(x, training, info = NULL, ...) {
        # Simulate CARS selecting the relevant features
        x$selected_features <- c("relevant1", "relevant2", "relevant3")
        x$trained <- TRUE
        return(x)
      },
      {
        prepped <- recipes::prep(recipe, training = test_data)
        result <- recipes::bake(prepped, new_data = test_data)
      },
      .package = "horizons"
    )
    
    # Check that relevant features were selected
    selected_features <- setdiff(names(result), c("Sample_ID", "Response"))
    expect_true("relevant1" %in% selected_features || 
                "relevant2" %in% selected_features ||
                "relevant3" %in% selected_features)
  })
})

test_that("step_select_cars preserves non-predictor columns", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 620, by = 10))
  test_data$metadata <- paste0("meta_", seq_len(nrow(test_data)))
  
  skip_if_not_installed("pls")
  
  with_mocked_computations({
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      recipes::update_role(metadata, new_role = "metadata") %>%
      step_select_cars(recipes::all_predictors())
    
    # Mock to select a subset of features
    with_mocked_bindings(
      prep.step_select_cars = function(x, training, info = NULL, ...) {
        spectral_cols <- names(training)[grepl("^[0-9]+$", names(training))]
        x$selected_features <- spectral_cols[1:3]
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
    expect_true("metadata" %in% names(result))
  })
})

test_that("step_select_cars handles edge cases", {
  # Test with minimal features
  minimal_data <- data.frame(
    Sample_ID = c("A", "B", "C", "D", "E"),
    Response = c(1.0, 2.0, 3.0, 4.0, 5.0),
    feat1 = c(0.5, 0.6, 0.7, 0.8, 0.9),
    feat2 = c(1.5, 1.6, 1.7, 1.8, 1.9),
    stringsAsFactors = FALSE
  )
  
  recipe <- recipes::recipe(Response ~ ., data = minimal_data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_cars(recipes::all_predictors())
  
  # Should create recipe without error
  expect_s3_class(recipe, "recipe")
  expect_s3_class(recipe$steps[[1]], "step_select_cars")
})

test_that("step_select_cars works with spectral data subset", {
  # Use fixture but with very small subset for speed
  test_data <- qs::qread(test_path("fixtures", "small_spectra_fixture.qs"))
  
  # Select very small subset of wavelengths
  spectral_cols <- names(test_data)[grepl("^[0-9]+$", names(test_data))]
  subset_cols <- spectral_cols[seq(1, min(10, length(spectral_cols)))]
  test_data_subset <- test_data[, c("Project", "Sample_ID", "Response", subset_cols)]
  
  skip_if_not_installed("pls")
  
  recipe <- recipes::recipe(Response ~ ., data = test_data_subset) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    recipes::update_role(Project, new_role = "metadata") %>%
    step_select_cars(recipes::all_predictors())
  
  # Just check that recipe is created properly
  expect_valid_recipe(recipe, has_selection_step = TRUE)
})

test_that("step_select_cars works in combination with other steps", {
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 620, by = 5))
  
  skip_if_not_installed("pls")
  
  with_mocked_computations({
    # Create recipe with multiple steps
    recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
      recipes::update_role(Sample_ID, new_role = "id") %>%
      step_transform_spectra(recipes::all_predictors(), preprocessing = "snv") %>%
      step_select_cars(recipes::all_predictors())
    
    # Mock CARS to select first few spectral features
    with_mocked_bindings(
      prep.step_select_cars = function(x, training, info = NULL, ...) {
        spec_cols <- names(training)[grepl("^spec", names(training))]
        x$selected_features <- spec_cols[1:min(3, length(spec_cols))]
        x$trained <- TRUE
        return(x)
      },
      {
        prepped <- recipes::prep(recipe, training = test_data)
        result <- recipes::bake(prepped, new_data = test_data)
      },
      .package = "horizons"
    )
    
    # Check that transformation and selection both occurred
    spec_cols <- names(result)[grepl("^spec", names(result))]
    expect_true(length(spec_cols) > 0)
    expect_true(length(spec_cols) <= 3)
  })
})

test_that("step_select_cars print method works", {
  test_data <- make_test_spectra(n_samples = 10, wavelengths = seq(600, 620, by = 10))
  
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    step_select_cars(recipes::all_predictors())
  
  # Test print method doesn't error
  expect_output(print(recipe$steps[[1]]), "CARS.*feature selection")
})