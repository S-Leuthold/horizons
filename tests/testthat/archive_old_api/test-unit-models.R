# Unit Tests for Model Specifications and Recipe Building
# Tests the core model definition functions and recipe construction
# for the tidymodels modeling pipeline

library(testthat)
library(horizons)
library(parsnip)
library(recipes)
library(tune)
library(dplyr)
library(rlang)

## ---------------------------------------------------------------------------
## Model Specification Tests - horizons:::define_model_specifications()
## ---------------------------------------------------------------------------

test_that("horizons:::define_model_specifications creates valid random_forest spec", {
  spec <- horizons:::define_model_specifications("random_forest")
  
  expect_s3_class(spec, "rand_forest")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "ranger")
  
  # Check tunable parameters (stored as quosures with tune() calls)
  expect_true(any(grepl("tune", rlang::as_label(spec$args$mtry))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$min_n))))
  # Fixed parameters are also stored as quosures
  expect_true(any(grepl("500", rlang::as_label(spec$args$trees))))
})

test_that("horizons:::define_model_specifications creates valid cubist spec", {
  spec <- horizons:::define_model_specifications("cubist")
  
  expect_s3_class(spec, "cubist_rules")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "Cubist")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$committees))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$neighbors))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$max_rules))))
})

test_that("horizons:::define_model_specifications creates valid xgboost spec", {
  spec <- horizons:::define_model_specifications("xgboost")
  
  expect_s3_class(spec, "boost_tree")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "xgboost")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$tree_depth))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$learn_rate))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$mtry))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$min_n))))
  expect_true(any(grepl("500", rlang::as_label(spec$args$trees))))  # Fixed parameter
})

test_that("horizons:::define_model_specifications creates valid lightgbm spec", {
  spec <- horizons:::define_model_specifications("lightgbm")
  
  expect_s3_class(spec, "boost_tree")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "lightgbm")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$tree_depth))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$learn_rate))))
  expect_true(any(grepl("500", rlang::as_label(spec$args$trees))))
})

test_that("horizons:::define_model_specifications creates valid elastic_net spec", {
  spec <- horizons:::define_model_specifications("elastic_net")
  
  expect_s3_class(spec, "linear_reg")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "glmnet")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$penalty))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$mixture))))
})

test_that("horizons:::define_model_specifications creates valid svm_rbf spec", {
  spec <- horizons:::define_model_specifications("svm_rbf")
  
  expect_s3_class(spec, "svm_rbf")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "kernlab")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$cost))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$rbf_sigma))))
})

test_that("horizons:::define_model_specifications creates valid mars spec", {
  spec <- horizons:::define_model_specifications("mars")
  
  expect_s3_class(spec, "mars")
  expect_s3_class(spec, "model_spec") 
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "earth")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$prod_degree))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$num_terms))))
})

test_that("horizons:::define_model_specifications creates valid plsr spec", {
  spec <- horizons:::define_model_specifications("plsr")
  
  expect_s3_class(spec, "pls")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "mixOmics")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$num_comp))))
  expect_true(any(grepl("1", rlang::as_label(spec$args$predictor_prop))))  # Fixed parameter
})

test_that("horizons:::define_model_specifications creates valid mlp_nn spec", {
  spec <- horizons:::define_model_specifications("mlp_nn")
  
  expect_s3_class(spec, "mlp")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "nnet")
  
  # Check tunable parameters
  expect_true(any(grepl("tune", rlang::as_label(spec$args$hidden_units))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$penalty))))
  expect_true(any(grepl("tune", rlang::as_label(spec$args$epochs))))
})

test_that("horizons:::define_model_specifications errors on unknown model type", {
  expect_error(
    horizons:::define_model_specifications("unknown_model"),
    "Unknown model type"
  )
})

test_that("all model specifications are in regression mode", {
  model_types <- c("random_forest", "cubist", "xgboost", "lightgbm", 
                   "elastic_net", "svm_rbf", "mars", "plsr", "mlp_nn")
  
  for (model_type in model_types) {
    spec <- horizons:::define_model_specifications(model_type)
    expect_equal(spec$mode, "regression", 
                info = paste("Model", model_type, "not in regression mode"))
  }
})

## ---------------------------------------------------------------------------
## Workflow ID Generation Tests - clean_workflow_id()
## ---------------------------------------------------------------------------

test_that("clean_workflow_id handles basic components correctly", {
  wf_id <- clean_workflow_id(
    model = "cubist",
    transformation = "none",
    preprocessing = "snv",
    feature_selection = "pca",
    covariates = NULL
  )
  
  expect_equal(wf_id, "cubist_NoTrans_SNV_PCA_NoCovs")
})

test_that("clean_workflow_id handles transformation name cleaning", {
  # Test lowercase transformations
  wf_id1 <- clean_workflow_id("cubist", "log", "raw", "none")
  expect_true(any(grepl("Log", wf_id1)))
  
  wf_id2 <- clean_workflow_id("cubist", "sqrt", "raw", "none")
  expect_true(any(grepl("Sqrt", wf_id2)))
  
  # Test verbose transformations
  wf_id3 <- clean_workflow_id("cubist", "Log Transformation", "raw", "none")
  expect_true(any(grepl("Log", wf_id3)))
})

test_that("clean_workflow_id handles preprocessing name cleaning", {
  # Test different preprocessing methods
  wf_id1 <- clean_workflow_id("cubist", "none", "snv", "none")
  expect_true(any(grepl("SNV", wf_id1)))
  
  wf_id2 <- clean_workflow_id("cubist", "none", "deriv1", "none")
  expect_true(any(grepl("D1", wf_id2)))
  
  wf_id3 <- clean_workflow_id("cubist", "none", "snv_deriv2", "none")
  expect_true(any(grepl("SNVD2", wf_id3)))
})

test_that("clean_workflow_id handles feature selection cleaning", {
  # Test different feature selection methods
  wf_id1 <- clean_workflow_id("cubist", "none", "raw", "correlation")
  expect_true(any(grepl("Corr", wf_id1)))
  
  wf_id2 <- clean_workflow_id("cubist", "none", "raw", "boruta")
  expect_true(any(grepl("Boruta", wf_id2)))
  
  wf_id3 <- clean_workflow_id("cubist", "none", "raw", "none")
  expect_true(any(grepl("NoFeatSel", wf_id3)))
})

test_that("clean_workflow_id handles covariates correctly", {
  # Single covariate
  wf_id1 <- clean_workflow_id("cubist", "none", "raw", "none", "Clay")
  expect_true(any(grepl("Clay", wf_id1)))
  
  # Multiple covariates
  wf_id2 <- clean_workflow_id("cubist", "none", "raw", "none", c("Clay", "pH"))
  expect_true(any(grepl("Clay\\+pH", wf_id2)))
  
  # No covariates
  wf_id3 <- clean_workflow_id("cubist", "none", "raw", "none", NULL)
  expect_true(any(grepl("NoCovs", wf_id3)))
  
  # Empty covariates
  wf_id4 <- clean_workflow_id("cubist", "none", "raw", "none", c())
  expect_true(any(grepl("NoCovs", wf_id4)))
})

test_that("clean_workflow_id produces consistent format", {
  wf_id <- clean_workflow_id(
    model = "random_forest",
    transformation = "log",
    preprocessing = "snv_deriv1", 
    feature_selection = "correlation",
    covariates = c("Clay", "pH", "CEC")
  )
  
  # Should have exactly 5 underscores (6 components)
  underscore_count <- length(strsplit(wf_id, "_")[[1]]) - 1
  expect_equal(underscore_count, 5)
  
  # Should not have double underscores
  expect_false(grepl("__", wf_id))
  
  # Should not end with underscore
  expect_false(grepl("_$", wf_id))
})

## ---------------------------------------------------------------------------
## Recipe Building Tests - horizons:::build_recipe()
## ---------------------------------------------------------------------------

test_that("horizons:::build_recipe creates basic recipe correctly", {
  test_data <- create_tiny_spectra(seed = 111213)
  test_data$Response <- test_data$SOC  # Add Response column
  
  rec <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "none", 
    feature_selection_method = "none"
  )
  
  expect_s3_class(rec, "recipe")
  
  # Check roles are set correctly
  variable_info <- rec$var_info
  expect_true("id" %in% variable_info$role)
  expect_true("metadata" %in% variable_info$role)
  expect_true("outcome" %in% variable_info$role)
  expect_true("predictor" %in% variable_info$role)
})

test_that("horizons:::build_recipe handles response transformations", {
  test_data <- create_tiny_spectra(seed = 141516)
  test_data$Response <- test_data$SOC
  
  # Test log transformation
  rec_log <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "log",
    feature_selection_method = "none"
  )
  
  # Check that step_log was added and has skip = TRUE
  step_classes <- purrr::map_chr(rec_log$steps, ~ class(.x)[1])
  expect_true("step_log" %in% step_classes)
  
  log_step <- rec_log$steps[step_classes == "step_log"][[1]]
  expect_true(log_step$skip)
  
  # Test sqrt transformation
  rec_sqrt <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "raw", 
    response_transformation = "sqrt",
    feature_selection_method = "none"
  )
  
  step_classes <- purrr::map_chr(rec_sqrt$steps, ~ class(.x)[1])
  expect_true("step_sqrt" %in% step_classes)
})

test_that("horizons:::build_recipe includes spectral transformation step", {
  test_data <- create_tiny_spectra(seed = 171819)
  test_data$Response <- test_data$SOC
  
  rec <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "snv",
    response_transformation = "none",
    feature_selection_method = "none"
  )
  
  # Check that spectral transformation step was added
  step_classes <- purrr::map_chr(rec$steps, ~ class(.x)[1])
  expect_true("step_transform_spectra" %in% step_classes)
  
  # Check preprocessing parameter
  spec_step <- rec$steps[step_classes == "step_transform_spectra"][[1]]
  expect_equal(spec_step$preprocessing, "snv")
})

test_that("horizons:::build_recipe handles feature selection methods", {
  test_data <- create_tiny_spectra(seed = 202122)
  test_data$Response <- test_data$SOC
  
  # Test PCA
  rec_pca <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "none",
    feature_selection_method = "pca"
  )
  
  step_classes <- purrr::map_chr(rec_pca$steps, ~ class(.x)[1])
  expect_true("step_pca" %in% step_classes)
  
  # Test correlation selection
  rec_corr <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "none", 
    feature_selection_method = "correlation"
  )
  
  step_classes <- purrr::map_chr(rec_corr$steps, ~ class(.x)[1])
  expect_true("step_select_correlation" %in% step_classes)
})

test_that("horizons:::build_recipe handles covariates correctly", {
  test_data <- create_tiny_spectra(seed = 232425)
  test_data$Response <- test_data$SOC
  
  # Create covariate data
  covariate_data <- data.frame(
    Sample_ID = test_data$Sample_ID,
    elevation = rnorm(nrow(test_data), 1000, 200),
    temperature = rnorm(nrow(test_data), 15, 5)
  )
  
  rec <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "raw",
    response_transformation = "none",
    feature_selection_method = "none",
    covariate_selection = c("elevation", "temperature"),
    covariate_data = covariate_data
  )
  
  # Check that covariate step was added
  step_classes <- purrr::map_chr(rec$steps, ~ class(.x)[1])
  expect_true("step_add_covariates" %in% step_classes)
})

test_that("horizons:::build_recipe validates input data", {
  # Test with missing Sample_ID
  bad_data <- data.frame(
    Response = c(1, 2, 3),
    "600" = c(0.1, 0.2, 0.3),
    check.names = FALSE
  )
  
  expect_error(
    horizons:::build_recipe(
      input_data = bad_data,
      spectral_transformation = "raw",
      response_transformation = "none",
      feature_selection_method = "none"
    ),
    "Sample_ID"
  )
})

test_that("horizons:::build_recipe validates covariate inputs", {
  test_data <- create_tiny_spectra(seed = 262728)
  test_data$Response <- test_data$SOC
  
  # Test with missing covariate_data when covariates requested
  expect_error(
    horizons:::build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = "none", 
      feature_selection_method = "none",
      covariate_selection = c("elevation"),
      covariate_data = NULL
    ),
    "covariate_data"
  )
  
  # Test with missing covariates in covariate_data
  incomplete_covs <- data.frame(
    Sample_ID = test_data$Sample_ID,
    elevation = rnorm(nrow(test_data))
  )
  
  expect_error(
    horizons:::build_recipe(
      input_data = test_data,
      spectral_transformation = "raw", 
      response_transformation = "none",
      feature_selection_method = "none",
      covariate_selection = c("elevation", "missing_covar"),
      covariate_data = incomplete_covs
    ),
    "Covariate mismatch"
  )
})

test_that("horizons:::build_recipe handles unsupported transformations", {
  test_data <- create_tiny_spectra(seed = 293031)
  test_data$Response <- test_data$SOC
  
  expect_error(
    horizons:::build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = "unknown_transform",
      feature_selection_method = "none"
    ),
    "Unsupported.*response transformation"
  )
  
  expect_error(
    horizons:::build_recipe(
      input_data = test_data,
      spectral_transformation = "raw",
      response_transformation = "none",
      feature_selection_method = "unknown_selection"
    ),
    "Unsupported.*feature selection method"
  )
})

test_that("horizons:::build_recipe creates complete preprocessing pipeline", {
  test_data <- create_tiny_spectra(seed = 323334)
  test_data$Response <- test_data$SOC
  
  covariate_data <- data.frame(
    Sample_ID = test_data$Sample_ID,
    elevation = rnorm(nrow(test_data), 1000, 200)
  )
  
  # Create a complex recipe with all components
  rec <- horizons:::build_recipe(
    input_data = test_data,
    spectral_transformation = "snv_deriv1",
    response_transformation = "log",
    feature_selection_method = "correlation", 
    covariate_selection = c("elevation"),
    covariate_data = covariate_data
  )
  
  step_classes <- purrr::map_chr(rec$steps, ~ class(.x)[1])
  
  # Should have all expected steps
  expect_true("step_log" %in% step_classes)
  expect_true("step_transform_spectra" %in% step_classes)
  expect_true("step_select_correlation" %in% step_classes)
  expect_true("step_add_covariates" %in% step_classes)
  
  # Steps should be in correct order
  step_order <- match(c("step_log", "step_transform_spectra", 
                       "step_select_correlation", "step_add_covariates"), 
                     step_classes)
  expect_true(all(diff(step_order[!is.na(step_order)]) > 0))
})

test_that("horizons:::build_recipe works with different data sizes", {
  # Test with small data
  small_data <- create_small_spectra(seed = 353637)
  small_data$Response <- small_data$SOC
  
  rec_small <- horizons:::build_recipe(
    input_data = small_data,
    spectral_transformation = "snv",
    response_transformation = "none",
    feature_selection_method = "pca"
  )
  
  expect_s3_class(rec_small, "recipe")
  
  # Test with medium data  
  medium_data <- create_medium_spectra(seed = 383940)
  medium_data$Response <- medium_data$SOC
  
  rec_medium <- horizons:::build_recipe(
    input_data = medium_data,
    spectral_transformation = "deriv2",
    response_transformation = "sqrt",
    feature_selection_method = "correlation"
  )
  
  expect_s3_class(rec_medium, "recipe")
})

test_that("horizons:::build_recipe performance is reasonable", {
  medium_data <- create_medium_spectra(seed = 414243)
  medium_data$Response <- medium_data$SOC
  
  # Recipe building should be fast
  expect_within_time({
    rec <- horizons:::build_recipe(
      input_data = medium_data,
      spectral_transformation = "snv_deriv1",
      response_transformation = "log",
      feature_selection_method = "pca"
    )
  }, max_seconds = 1.0)
})