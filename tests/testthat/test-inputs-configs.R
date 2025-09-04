#' Tests for create_configs() Function
#'
#' Comprehensive test suite for creating model configuration grids
#' including covariate combinations and parameter validation

# Setup test data --------------------------------------------------------------

setup_test_covariates <- function() {
  list(
    soil = c("clay", "sand", "pH", "CEC"),
    climate = c("MAT", "MAP", "AI", "PET"), 
    spatial = c("elevation", "slope", "aspect")
  )
}

# Input Validation Tests -------------------------------------------------------

test_that("create_configs validates covariate inputs correctly", {
  
  # Test invalid soil_covariates
  expect_error(
    create_configs(soil_covariates = 123),
    "soil_covariates must be a character vector or NULL"
  )
  
  expect_error(
    create_configs(soil_covariates = character(0)),
    "soil_covariates must be a character vector or NULL"
  )
  
  # Test invalid climate_covariates
  expect_error(
    create_configs(climate_covariates = list("MAT", "MAP")),
    "climate_covariates must be a character vector or NULL"
  )
  
  # Test invalid spatial_covariates
  expect_error(
    create_configs(spatial_covariates = TRUE),
    "spatial_covariates must be a character vector or NULL"
  )
})

# Basic Configuration Generation Tests -----------------------------------------

test_that("create_configs generates basic configuration grid correctly", {
  
  # Test with default parameters
  result <- create_configs(verbose = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("config_id", "model", "transformation", "preprocessing", 
                   "feature_selection", "covariate_set", "covariates") %in% names(result)))
  
  # Check default models
  default_models <- c("plsr", "random_forest", "cubist", "xgboost", "lightgbm")
  expect_true(all(default_models %in% unique(result$model)))
  
  # Check default transformations
  default_transforms <- c("none", "sqrt", "log")
  expect_true(all(default_transforms %in% unique(result$transformation)))
  
  # Check default preprocessing
  default_preproc <- c("raw", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2")
  expect_true(all(default_preproc %in% unique(result$preprocessing)))
  
  # Check default feature selection
  default_features <- c("none", "vip", "boruta", "rfe")
  expect_true(all(default_features %in% unique(result$feature_selection)))
  
  # Should have only "none" covariate set by default
  expect_equal(unique(result$covariate_set), "none")
})

test_that("create_configs generates correct number of configurations", {
  
  # Test with specific parameters
  result <- create_configs(
    models = c("plsr", "random_forest"),
    transformations = c("none", "sqrt"),
    preprocessing = c("raw", "snv"),
    feature_selection = c("none", "vip"),
    verbose = FALSE
  )
  
  # Should have 2 * 2 * 2 * 2 * 1 = 16 configurations (1 covariate set = none)
  expect_equal(nrow(result), 16)
  
  # Check unique combinations
  expect_equal(length(unique(result$model)), 2)
  expect_equal(length(unique(result$transformation)), 2)
  expect_equal(length(unique(result$preprocessing)), 2)
  expect_equal(length(unique(result$feature_selection)), 2)
})

# Covariate Handling Tests -----------------------------------------------------

test_that("create_configs handles single covariate types correctly", {
  
  # Test with only soil covariates
  result_soil <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw", 
    feature_selection = "none",
    soil_covariates = c("clay", "sand"),
    verbose = FALSE
  )
  
  # Should have 2^2 = 4 covariate combinations (including none)
  # none, clay, sand, clay_sand
  expect_equal(nrow(result_soil), 4)
  expect_true("none" %in% result_soil$covariate_set)
  expect_true("clay" %in% result_soil$covariate_set)
  expect_true("sand" %in% result_soil$covariate_set)
  expect_true("clay_sand" %in% result_soil$covariate_set)
  
  # Check covariates column is list-column
  expect_true(is.list(result_soil$covariates))
  
  # Check specific covariate combinations
  clay_sand_row <- result_soil[result_soil$covariate_set == "clay_sand", ]
  expect_equal(clay_sand_row$covariates[[1]], c("clay", "sand"))
})

test_that("create_configs handles multiple covariate types", {
  
  # Test with soil and climate covariates
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none", 
    soil_covariates = c("clay", "sand"),
    climate_covariates = c("MAT", "MAP"),
    verbose = FALSE
  )
  
  # Should have all combinations of 4 covariates
  # 2^4 = 16 combinations (including none)
  expect_equal(nrow(result), 16)
  
  # Check that mixed combinations exist
  mixed_sets <- result$covariate_set[result$covariate_set != "none"]
  expect_true(any(grepl("clay.*MAT", mixed_sets) | grepl("MAT.*clay", mixed_sets)))
})

test_that("create_configs handles large covariate sets efficiently", {
  
  # Test with many covariates (should warn about exponential growth)
  large_soil <- paste0("soil_var_", 1:8)  # 8 soil variables
  
  expect_message(
    result <- create_configs(
      models = "plsr",
      transformations = "none",
      preprocessing = "raw",
      feature_selection = "none",
      soil_covariates = large_soil,
      verbose = TRUE
    ),
    "Generating all subsets of .* covariates"
  )
  
  # Should have 2^8 = 256 covariate combinations (1*1*1*1*256)
  expect_equal(nrow(result), 256)
})

test_that("create_configs creates proper covariate list-column", {
  
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none",
    soil_covariates = c("clay", "pH"),
    verbose = FALSE
  )
  
  # Check that covariates column contains proper lists
  expect_true(is.list(result$covariates))
  
  # Check none case
  none_row <- result[result$covariate_set == "none", ]
  expect_null(none_row$covariates[[1]])
  
  # Check single covariate case
  clay_row <- result[result$covariate_set == "clay", ]
  expect_equal(clay_row$covariates[[1]], "clay")
  
  # Check multiple covariate case
  clay_ph_row <- result[result$covariate_set == "clay_pH", ]
  expect_equal(sort(clay_ph_row$covariates[[1]]), c("clay", "pH"))
})

# Configuration ID Tests -------------------------------------------------------

test_that("create_configs generates unique configuration IDs", {
  
  result <- create_configs(
    models = c("plsr", "random_forest"),
    transformations = c("none", "sqrt"),
    verbose = FALSE
  )
  
  # All config_ids should be unique
  expect_equal(length(unique(result$config_id)), nrow(result))
  
  # Config IDs should follow expected format
  expect_true(all(grepl("^config_[0-9]{4}$", result$config_id)))
  
  # Should be sequential
  config_numbers <- as.numeric(gsub("config_", "", result$config_id))
  expect_equal(config_numbers, 1:nrow(result))
})

# Verbose Output Tests ----------------------------------------------------------

test_that("create_configs produces appropriate verbose output", {
  
  # Test verbose output with covariates
  expect_message(
    create_configs(
      models = "plsr",
      soil_covariates = c("clay", "sand"),
      verbose = TRUE
    ),
    "Generating all subsets"
  )
  
  expect_message(
    create_configs(
      models = "plsr", 
      soil_covariates = c("clay", "sand"),
      verbose = TRUE
    ),
    "Created .* covariate combinations"
  )
  
  expect_message(
    create_configs(models = "plsr", verbose = TRUE),
    "Created .* model configurations"
  )
})

# Edge Cases Tests --------------------------------------------------------------

test_that("create_configs handles empty covariate inputs", {
  
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw", 
    feature_selection = "none",
    soil_covariates = NULL,
    climate_covariates = NULL,
    spatial_covariates = NULL,
    verbose = FALSE
  )
  
  expect_equal(nrow(result), 1)  # Only base configuration
  expect_equal(result$covariate_set, "none")
  expect_null(result$covariates[[1]])
})

test_that("create_configs handles single parameter values", {
  
  result <- create_configs(
    models = "plsr",
    transformations = "none", 
    preprocessing = "raw",
    feature_selection = "none",
    verbose = FALSE
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(result$model, "plsr")
  expect_equal(result$transformation, "none")
  expect_equal(result$preprocessing, "raw")
  expect_equal(result$feature_selection, "none")
})

test_that("create_configs handles duplicate covariate names", {
  
  # Duplicate across different types
  result <- create_configs(
    models = "plsr",
    transformations = "none",
    preprocessing = "raw",
    feature_selection = "none",
    soil_covariates = c("elevation", "clay"),
    spatial_covariates = c("elevation", "slope"),  # elevation appears twice
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  
  # Check that duplicate is handled appropriately
  # (behavior may vary - this tests that it doesn't crash)
  expect_gt(nrow(result), 1)
})

# Performance Tests -------------------------------------------------------------

test_that("create_configs handles reasonable parameter combinations efficiently", {
  
  start_time <- Sys.time()
  
  result <- create_configs(
    models = c("plsr", "random_forest", "cubist"),
    transformations = c("none", "sqrt", "log"), 
    preprocessing = c("raw", "snv", "deriv1"),
    feature_selection = c("none", "vip"),
    soil_covariates = c("clay", "sand", "pH"),
    verbose = FALSE
  )
  
  end_time <- Sys.time()
  
  # Should complete quickly
  expect_lt(as.numeric(end_time - start_time), 5)
  
  # Should have correct number of configurations
  # 3 models * 3 transforms * 3 preproc * 2 features * 2^3 covariates = 3*3*3*2*8 = 432
  expect_equal(nrow(result), 432)
})

test_that("create_configs warns about large configuration spaces", {
  
  # Create configuration that would generate many combinations
  large_models <- paste0("model_", 1:10)
  large_covariates <- paste0("cov_", 1:6)  # 2^6 = 64 combinations
  
  # This should complete but might be slow
  result <- create_configs(
    models = large_models,
    soil_covariates = large_covariates,
    verbose = FALSE
  )
  
  # Check that it completed successfully
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 1000)  # Should be quite large
})

# Integration Tests -------------------------------------------------------------

test_that("create_configs output can be used for model workflow", {
  
  result <- create_configs(
    models = c("plsr", "random_forest"),
    transformations = c("none", "sqrt"),
    preprocessing = c("raw", "snv"),
    feature_selection = c("none"),
    soil_covariates = c("clay", "pH"),
    verbose = FALSE
  )
  
  # Check that output has proper structure for downstream use
  expect_true(all(c("config_id", "model", "transformation", "preprocessing", 
                   "feature_selection", "covariate_set", "covariates") %in% names(result)))
  
  # Check that covariates can be extracted properly
  for (i in 1:nrow(result)) {
    covs <- result$covariates[[i]]
    expect_true(is.null(covs) || is.character(covs))
  }
  
  # Check that all model types are valid
  expect_true(all(result$model %in% c("plsr", "random_forest")))
})

test_that("create_configs works with common modeling workflows", {
  
  # Test configuration suitable for spectroscopy modeling
  result <- create_configs(
    models = c("plsr", "random_forest", "xgboost"),
    transformations = c("none", "log"),
    preprocessing = c("raw", "snv", "deriv1", "snv_deriv1"),
    feature_selection = c("none", "vip"),
    soil_covariates = c("clay", "sand", "silt"),
    climate_covariates = c("MAT", "MAP"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 100)  # Should have many configurations
  
  # Check that covariate combinations make sense
  covariate_sets <- unique(result$covariate_set)
  expect_true("none" %in% covariate_sets)
  expect_true(any(grepl("clay", covariate_sets)))
  expect_true(any(grepl("MAT", covariate_sets)))
})

# Parameter Combinations Tests -------------------------------------------------

test_that("create_configs creates all parameter combinations correctly", {
  
  result <- create_configs(
    models = c("plsr", "random_forest"),
    transformations = c("none", "sqrt"),
    preprocessing = c("raw", "snv"),
    feature_selection = c("none", "vip"),
    soil_covariates = c("clay"),  # Only one covariate for simplicity
    verbose = FALSE
  )
  
  # Should have 2*2*2*2*2 = 32 combinations (2 covariate sets: none, clay)
  expect_equal(nrow(result), 32)
  
  # Check that all combinations exist
  combinations <- unique(result[, c("model", "transformation", "preprocessing", "feature_selection")])
  expect_equal(nrow(combinations), 16)  # 2*2*2*2 = 16 unique base combinations
  
  # Each base combination should appear twice (once for each covariate set)
  for (i in 1:nrow(combinations)) {
    combo_rows <- result[
      result$model == combinations$model[i] &
      result$transformation == combinations$transformation[i] &
      result$preprocessing == combinations$preprocessing[i] &
      result$feature_selection == combinations$feature_selection[i], ]
    
    expect_equal(nrow(combo_rows), 2)  # Should appear exactly twice
  }
})

# Memory and Scaling Tests -----------------------------------------------------

test_that("create_configs handles memory efficiently", {
  
  # Test with moderate-sized configuration space
  result <- create_configs(
    models = c("plsr", "random_forest", "cubist", "xgboost", "lightgbm"),
    transformations = c("none", "sqrt", "log"),
    preprocessing = c("raw", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2"),
    feature_selection = c("none", "vip", "boruta", "rfe"),
    soil_covariates = c("clay", "sand"),  # 2^2 = 4 combinations
    verbose = FALSE
  )
  
  # Should have 5*3*6*4*4 = 1440 configurations
  expect_equal(nrow(result), 1440)
  
  # Check memory usage is reasonable
  expect_lt(object.size(result), 10 * 1024 * 1024)  # Less than 10MB
})