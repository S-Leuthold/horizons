test_that("define_model_specifications creates valid model specs", {
  # Test each supported model type
  model_types <- c("random_forest", "cubist", "xgboost", "lightgbm", 
                   "elastic_net", "svm_rbf", "mars", "plsr", "mlp_nn")
  
  for (model_type in model_types) {
    # Skip if package not available
    required_packages <- list(
      "random_forest" = "ranger",
      "xgboost" = "xgboost", 
      "lightgbm" = "lightgbm",
      "svm_rbf" = "kernlab",
      "mars" = "earth",
      "plsr" = "mixOmics",
      "mlp_nn" = "nnet"
    )
    
    if (model_type %in% names(required_packages)) {
      skip_if_not_installed(required_packages[[model_type]])
    }
    
    spec <- define_model_specifications(model_type)
    
    # Check basic structure
    expect_s3_class(spec, "model_spec")
    expect_equal(spec$mode, "regression")
    expect_true(length(spec$args) > 0)  # Should have tunable parameters
    
    # Check that parameters are set to tune()
    tune_params <- purrr::map_lgl(spec$args, ~ inherits(.x, "quosure") && 
                                              grepl("tune", rlang::quo_get_expr(.x)))
    expect_true(any(tune_params), 
                info = paste("Model", model_type, "should have tune() parameters"))
  }
})

test_that("define_model_specifications returns correct engines", {
  # Test specific engine assignments
  expected_engines <- list(
    "random_forest" = "ranger",
    "cubist" = "Cubist", 
    "xgboost" = "xgboost",
    "lightgbm" = "lightgbm",
    "elastic_net" = "glmnet",
    "svm_rbf" = "kernlab",
    "mars" = "earth",
    "plsr" = "mixOmics",
    "mlp_nn" = "nnet"
  )
  
  for (model_type in names(expected_engines)) {
    # Skip if package not available
    required_packages <- list(
      "random_forest" = "ranger",
      "xgboost" = "xgboost",
      "lightgbm" = "lightgbm", 
      "svm_rbf" = "kernlab",
      "mars" = "earth",
      "plsr" = "mixOmics",
      "mlp_nn" = "nnet"
    )
    
    if (model_type %in% names(required_packages)) {
      skip_if_not_installed(required_packages[[model_type]])
    }
    
    spec <- define_model_specifications(model_type)
    expect_equal(spec$engine, expected_engines[[model_type]])
  }
})

test_that("define_model_specifications handles random_forest correctly", {
  skip_if_not_installed("ranger")
  
  spec <- define_model_specifications("random_forest")
  
  expect_s3_class(spec, "rand_forest")
  expect_equal(spec$engine, "ranger")
  expect_equal(spec$mode, "regression")
  
  # Check tunable parameters
  expect_true("mtry" %in% names(spec$args))
  expect_true("trees" %in% names(spec$args))
  expect_true("min_n" %in% names(spec$args))
  
  # Parameters should be tune() placeholders
  expect_true(inherits(spec$args$mtry, "quosure"))
  expect_true(inherits(spec$args$trees, "quosure"))
  expect_true(inherits(spec$args$min_n, "quosure"))
})

test_that("define_model_specifications handles plsr correctly", {
  skip_if_not_installed("mixOmics")
  
  spec <- define_model_specifications("plsr")
  
  expect_s3_class(spec, "pls")
  expect_equal(spec$engine, "mixOmics")
  expect_equal(spec$mode, "regression")
  
  # Check tunable parameters
  expect_true("num_comp" %in% names(spec$args))
  expect_true("predictor_prop" %in% names(spec$args))
})

test_that("define_model_specifications handles cubist correctly", {
  skip_if_not_installed("Cubist")
  
  spec <- define_model_specifications("cubist")
  
  expect_s3_class(spec, "cubist_rules")
  expect_equal(spec$engine, "Cubist")
  expect_equal(spec$mode, "regression")
  
  # Check tunable parameters
  expect_true("committees" %in% names(spec$args))
  expect_true("neighbors" %in% names(spec$args))
  expect_true("max_rules" %in% names(spec$args))
})

test_that("define_model_specifications handles xgboost correctly", {
  skip_if_not_installed("xgboost")
  
  spec <- define_model_specifications("xgboost")
  
  expect_s3_class(spec, "boost_tree")
  expect_equal(spec$engine, "xgboost")
  expect_equal(spec$mode, "regression")
  
  # Check key tunable parameters
  expected_params <- c("trees", "tree_depth", "learn_rate", "loss_reduction", "sample_size", "mtry")
  for (param in expected_params) {
    expect_true(param %in% names(spec$args), 
                info = paste("XGBoost should have", param, "parameter"))
  }
})

test_that("define_model_specifications handles elastic_net correctly", {
  skip_if_not_installed("glmnet")
  
  spec <- define_model_specifications("elastic_net")
  
  expect_s3_class(spec, "linear_reg")
  expect_equal(spec$engine, "glmnet")
  expect_equal(spec$mode, "regression")
  
  # Check tunable parameters
  expect_true("penalty" %in% names(spec$args))
  expect_true("mixture" %in% names(spec$args))
})

test_that("define_model_specifications handles svm_rbf correctly", {
  skip_if_not_installed("kernlab")
  
  spec <- define_model_specifications("svm_rbf")
  
  expect_s3_class(spec, "svm_rbf")
  expect_equal(spec$engine, "kernlab")
  expect_equal(spec$mode, "regression")
  
  # Check tunable parameters
  expect_true("cost" %in% names(spec$args))
  expect_true("rbf_sigma" %in% names(spec$args))
})

test_that("define_model_specifications handles mars correctly", {
  skip_if_not_installed("earth")
  
  spec <- define_model_specifications("mars")
  
  expect_s3_class(spec, "mars")
  expect_equal(spec$engine, "earth")
  expect_equal(spec$mode, "regression")
  
  # Check tunable parameters
  expect_true("num_terms" %in% names(spec$args))
  expect_true("prod_degree" %in% names(spec$args))
})

test_that("define_model_specifications handles mlp_nn correctly", {
  skip_if_not_installed("nnet")
  
  spec <- define_model_specifications("mlp_nn")
  
  expect_s3_class(spec, "mlp")
  expect_equal(spec$engine, "nnet")
  expect_equal(spec$mode, "regression")
  
  # Check tunable parameters
  expect_true("hidden_units" %in% names(spec$args))
  expect_true("penalty" %in% names(spec$args))
  expect_true("epochs" %in% names(spec$args))
})

test_that("define_model_specifications handles lightgbm correctly", {
  skip_if_not_installed("lightgbm")
  
  spec <- define_model_specifications("lightgbm")
  
  expect_s3_class(spec, "boost_tree")
  expect_equal(spec$engine, "lightgbm")
  expect_equal(spec$mode, "regression")
  
  # Should have boosting tree parameters
  expected_params <- c("trees", "tree_depth", "learn_rate", "loss_reduction", "sample_size", "mtry")
  for (param in expected_params) {
    expect_true(param %in% names(spec$args))
  }
})

test_that("define_model_specifications validates input", {
  # Test invalid model type
  expect_error(
    define_model_specifications("invalid_model"),
    # Should error with unmatched switch case
    class = "error"
  )
  
  # Test with NULL input
  expect_error(
    define_model_specifications(NULL),
    class = "error"
  )
})

test_that("define_model_specifications can be used in workflows", {
  # Test that specs can be used in actual workflows
  skip_if_not_installed("ranger")
  
  # Create test data
  test_data <- make_test_spectra(n_samples = 20, wavelengths = seq(600, 650, by = 10))
  
  # Create recipe
  recipe <- recipes::recipe(Response ~ ., data = test_data) %>%
    recipes::update_role(Sample_ID, new_role = "id")
  
  # Create model spec
  spec <- define_model_specifications("random_forest")
  
  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)
  
  expect_s3_class(wf, "workflow")
  expect_false(workflows::is_empty_preprocessor(wf))
  expect_false(workflows::is_empty_model(wf))
})

test_that("define_model_specifications produces different specs for different models", {
  # Test that different model types produce different specifications
  skip_if_not_installed("ranger")
  skip_if_not_installed("Cubist")
  
  rf_spec <- define_model_specifications("random_forest")
  cubist_spec <- define_model_specifications("cubist")
  
  # Should have different classes
  expect_false(identical(class(rf_spec), class(cubist_spec)))
  
  # Should have different engines
  expect_false(identical(rf_spec$engine, cubist_spec$engine))
  
  # Should have different parameter names
  expect_false(identical(names(rf_spec$args), names(cubist_spec$args)))
})

test_that("define_model_specifications tune parameters work with tuning", {
  # Test that tune() parameters can be detected by tuning functions
  skip_if_not_installed("ranger")
  skip_if_not_installed("dials")
  
  spec <- define_model_specifications("random_forest")
  
  # Extract tune parameters
  tune_params <- tune::tune_args(spec)
  
  expect_true(length(tune_params) > 0)
  expect_true("mtry" %in% names(tune_params))
  expect_true("trees" %in% names(tune_params))
  expect_true("min_n" %in% names(tune_params))
})