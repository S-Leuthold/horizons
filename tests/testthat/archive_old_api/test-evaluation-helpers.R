library(testthat)
library(horizons)

test_that("create_failed_result formats failure rows with metadata", {
  config <- list(
    model = "plsr",
    transformation = "log",
    preprocessing = "snv",
    feature_selection = "boruta",
    covariates = c("clay", "sand")
  )

  result <- horizons:::create_failed_result(
    config_id     = "cfg_001",
    config_clean  = config,
    error_message = "Recipe step failed",
    workflow_id   = "plsr_log_snv_boruta",
    error_detail  = structure(list(message = "failure"), class = "mock_error"),
    error_stage   = "recipe_building",
    warnings      = c("warning_a", "warning_b")
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$config_id, "cfg_001")
  expect_equal(result$workflow_id, "plsr_log_snv_boruta")
  expect_equal(result$model, "plsr")
  expect_equal(result$covariates, "clay-sand")
  expect_true(is.na(result$rsq))
  expect_equal(result$error_stage, "recipe_building")
  expect_equal(result$error_class, "mock_error")
  expect_equal(result$n_warnings, 2)
  expect_match(result$warning_summary, "warning_a")
})

test_that("readable name helpers cover known and unknown identifiers", {
  expect_equal(unname(horizons:::get_readable_model_name("xgboost")), "XGBoost")
  expect_true(is.na(horizons:::get_readable_model_name("unknown_model")))

  expect_equal(unname(horizons:::get_readable_preprocessing_name("snv")), "Standard Normal Variate")
  expect_equal(unname(horizons:::get_readable_preprocessing_name("SG")), "Savitzky-Golay Smoothing")
  expect_true(is.na(horizons:::get_readable_preprocessing_name("mystery_step")))

  expect_equal(unname(horizons:::get_readable_transformation_name("log")), "Logarithmic")
  expect_equal(unname(horizons:::get_readable_transformation_name("NoTrans")), "None")
  expect_true(is.na(horizons:::get_readable_transformation_name("weird")))

  expect_equal(unname(horizons:::get_readable_feature_selection_name("boruta")), "Boruta Algorithm")
  expect_equal(unname(horizons:::get_readable_feature_selection_name("vip")), "Variable Importance (VIP)")
  expect_true(is.na(horizons:::get_readable_feature_selection_name("custom")))

  expect_equal(unname(horizons:::get_readable_covariate_name("MAT")), "Mean Annual Temperature")
  expect_true(is.na(horizons:::get_readable_covariate_name("custom_cov")))
})

test_that("parse_workflow_id returns readable components", {
  parsed <- horizons:::parse_workflow_id("random_forest_Log_SNV_Boruta_clay+sand")

  expect_equal(unname(parsed$model), "Random Forest")
  expect_equal(unname(parsed$transformation), "Logarithmic")
  expect_equal(unname(parsed$preprocessing), "Standard Normal Variate")
  expect_equal(unname(parsed$feature_selection), "Boruta Algorithm")
  expect_equal(parsed$covariates, "Clay Content + Sand Content")

  parsed_default <- horizons:::parse_workflow_id("plsr")
  expect_equal(unname(parsed_default$feature_selection), "No Selection")
  expect_equal(parsed_default$covariates, "None")
})
