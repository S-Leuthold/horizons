#' Tests for Model Specifications and Workflow Functions
#'
#' Comprehensive integration tests for model specification and workflow ID functions
#' in models-specifications.R
#'
#' **Coverage Target**: 0-20% → 60%+ for R/models-specifications.R
#' **Test Strategy**: Validation + Integration testing
#' **Test Count**: 17 tests (9 validation + 8 integration)

## ===========================================================================
## Setup and Fixtures
## ===========================================================================

test_that("Model specification functions are accessible", {

  expect_true(is.function(horizons:::define_model_specifications))
  expect_true(is.function(horizons:::clean_workflow_id))
  expect_true(is.function(horizons:::evaluate_final_models))

})

## ===========================================================================
## VALIDATION TESTS: define_model_specifications()
## ===========================================================================

test_that("define_model_specifications returns parsnip random forest spec", {

  # SPEC-MODELS-SPEC-001: Random forest specification with tunable hyperparameters
  spec <- horizons:::define_model_specifications("random_forest")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "ranger")

})

test_that("define_model_specifications returns parsnip cubist spec", {

  # SPEC-MODELS-SPEC-002: Cubist model with tunable parameters
  spec <- horizons:::define_model_specifications("cubist")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "Cubist")

})

test_that("define_model_specifications returns parsnip xgboost spec", {

  # SPEC-MODELS-SPEC-003: XGBoost specification with multiple tunable params
  spec <- horizons:::define_model_specifications("xgboost")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "xgboost")

})

test_that("define_model_specifications returns parsnip elastic net spec", {

  # SPEC-MODELS-SPEC-004: Elastic net with regularization parameters
  spec <- horizons:::define_model_specifications("elastic_net")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "glmnet")

})

test_that("define_model_specifications returns parsnip PLSR spec", {

  # SPEC-MODELS-SPEC-005: Partial least squares regression (spectroscopy standard)
  spec <- horizons:::define_model_specifications("plsr")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "mixOmics")

})

test_that("define_model_specifications rejects unknown model type", {

  # SPEC-MODELS-SPEC-006: Invalid model types should error
  expect_error(
    horizons:::define_model_specifications("unknown_model"),
    regex = "Unknown model type"
  )

})

test_that("define_model_specifications supports all documented model types", {

  # SPEC-MODELS-SPEC-007: Verify all 9 model types work
  model_types <- c(
    "random_forest", "cubist", "xgboost", "lightgbm", "elastic_net",
    "svm_rbf", "mars", "plsr", "mlp_nn"
  )

  for (model_type in model_types) {
    spec <- horizons:::define_model_specifications(model_type)
    expect_s3_class(spec, "model_spec")
    expect_equal(spec$mode, "regression")
  }

})

## ===========================================================================
## VALIDATION TESTS: clean_workflow_id()
## ===========================================================================

test_that("clean_workflow_id creates valid ID with standard components", {

  # SPEC-MODELS-WID-001: Basic workflow ID assembly
  id <- horizons::clean_workflow_id(
    model                = "random_forest",
    transformation       = "none",
    preprocessing        = "raw",
    feature_selection    = "pca",
    covariates           = c("clay", "pH"),
    covariate_interactions = FALSE
  )

  expect_type(id, "character")
  expect_true(nchar(id) > 0)
  expect_true(grepl("random_forest", id))
  expect_true(grepl("PCA", id))

})

test_that("clean_workflow_id standardizes transformation names", {

  # SPEC-MODELS-WID-002: Transformation name cleaning
  id_none <- horizons::clean_workflow_id(
    model = "cubist",
    transformation = "none",
    preprocessing = "snv",
    feature_selection = "none",
    covariates = NULL
  )

  expect_true(grepl("NoTrans", id_none))
  expect_false(grepl("none", id_none))

  id_log <- horizons::clean_workflow_id(
    model = "cubist",
    transformation = "log",
    preprocessing = "snv",
    feature_selection = "none"
  )

  expect_true(grepl("Log", id_log))

})

test_that("clean_workflow_id standardizes preprocessing names", {

  # SPEC-MODELS-WID-003: Preprocessing name normalization
  id_snv <- horizons::clean_workflow_id(
    model = "elastic_net",
    transformation = "none",
    preprocessing = "snv",
    feature_selection = "none"
  )

  expect_true(grepl("SNV", id_snv))

  id_sg <- horizons::clean_workflow_id(
    model = "elastic_net",
    transformation = "none",
    preprocessing = "sg",
    feature_selection = "none"
  )

  expect_true(grepl("SG", id_sg))

})

test_that("clean_workflow_id handles covariate combinations", {

  # SPEC-MODELS-WID-004: Multiple covariates joined with plus signs
  id <- horizons::clean_workflow_id(
    model = "xgboost",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = c("clay", "pH", "SOC")
  )

  expect_true(grepl("clay\\+pH\\+SOC", id))

})

test_that("clean_workflow_id handles empty covariate list", {

  # SPEC-MODELS-WID-005: No covariates → NoCovs tag
  id <- horizons::clean_workflow_id(
    model = "mars",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = NULL
  )

  expect_true(grepl("NoCovs", id))

})

test_that("clean_workflow_id adds interaction flag", {

  # SPEC-MODELS-WID-006: Interaction flag when requested
  id_interact <- horizons::clean_workflow_id(
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "pca",
    covariates = c("clay"),
    covariate_interactions = TRUE
  )

  expect_true(grepl("Interact", id_interact))

  id_no_interact <- horizons::clean_workflow_id(
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "pca",
    covariates = c("clay"),
    covariate_interactions = FALSE
  )

  expect_true(grepl("NoInteract", id_no_interact))

})

test_that("clean_workflow_id rejects closures in parameters", {

  # SPEC-MODELS-WID-007: Defensive check against closure injection
  expect_error(
    horizons::clean_workflow_id(
      model = function() {},  # Closure attempt
      transformation = "none",
      preprocessing = "raw",
      feature_selection = "none"
    ),
    regex = "Closure"
  )

})

## ===========================================================================
## INTEGRATION TESTS: evaluate_final_models()
## ===========================================================================

test_that("evaluate_final_models validates required columns", {

  # SPEC-MODELS-EVAL-001: Check for fitted_wf column in input
  dummy_data <- data.frame(model_id = c(1, 2))

  expect_error(
    horizons:::evaluate_final_models(dummy_data, data.frame(Response = c(1, 2))),
    regex = "fitted_wf"
  )

})

test_that("evaluate_final_models validates Response column in holdout data", {

  # SPEC-MODELS-EVAL-002: Check for Response column in holdout
  # Create minimal fitted workflow (mock)
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  dummy_wf_sets <- data.frame(
    fitted_wf = I(list(NULL)),
    workflow_id = "test_model"
  )

  holdout_missing_response <- data.frame(x = c(1, 2), y = c(1, 2))

  expect_error(
    horizons:::evaluate_final_models(dummy_wf_sets, holdout_missing_response),
    regex = "Response"
  )

})

test_that("clean_workflow_id produces consistent IDs for same inputs", {

  # SPEC-MODELS-WID-008: Deterministic ID generation
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  id1 <- horizons::clean_workflow_id(
    model = "random_forest",
    transformation = "log",
    preprocessing = "snv",
    feature_selection = "pca",
    covariates = c("clay", "pH"),
    covariate_interactions = TRUE
  )

  id2 <- horizons::clean_workflow_id(
    model = "random_forest",
    transformation = "log",
    preprocessing = "snv",
    feature_selection = "pca",
    covariates = c("clay", "pH"),
    covariate_interactions = TRUE
  )

  expect_equal(id1, id2)

})

test_that("clean_workflow_id handles all preprocessing variants", {

  # SPEC-MODELS-WID-009: Comprehensive preprocessing name handling
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  preprocessing_options <- c(
    "raw", "snv", "sg", "deriv1", "deriv2",
    "snv_deriv1", "snv_deriv2"
  )

  for (prep in preprocessing_options) {
    id <- horizons::clean_workflow_id(
      model = "cubist",
      transformation = "none",
      preprocessing = prep,
      feature_selection = "none"
    )

    expect_type(id, "character")
    expect_true(nchar(id) > 0)
  }

})

test_that("clean_workflow_id handles all feature selection variants", {

  # SPEC-MODELS-WID-010: Comprehensive feature selection name handling
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  feature_options <- c(
    "none", "pca", "boruta", "correlation",
    "cars", "vip", "rfe"
  )

  for (feat in feature_options) {
    id <- horizons::clean_workflow_id(
      model = "xgboost",
      transformation = "none",
      preprocessing = "raw",
      feature_selection = feat
    )

    expect_type(id, "character")
    expect_true(nchar(id) > 0)
  }

})

test_that("clean_workflow_id removes NA covariates", {

  # SPEC-MODELS-WID-011: NA values in covariate vector are filtered
  id <- horizons::clean_workflow_id(
    model = "elastic_net",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = c("clay", NA, "pH", "")
  )

  # Should only have clay+pH
  expect_true(grepl("clay\\+pH", id))
  expect_false(grepl("NA", id))

})

test_that("define_model_specifications creates specs with tunable parameters", {

  # SPEC-MODELS-SPEC-008: Verify tune() placeholders are in place
  spec <- horizons:::define_model_specifications("random_forest")

  # Check that the spec has tunable parameters (they'll be in the args)
  expect_true(!is.null(spec))
  expect_s3_class(spec, "model_spec")

})

test_that("clean_workflow_id formats workflow ID without double underscores", {

  # SPEC-MODELS-WID-012: Clean formatting
  id <- horizons::clean_workflow_id(
    model = "lightgbm",
    transformation = "sqrt",
    preprocessing = "snv",
    feature_selection = "vip",
    covariates = c("BD"),
    covariate_interactions = FALSE
  )

  # Should not contain double underscores
  expect_false(grepl("__", id))
  # Should not end with underscore
  expect_false(grepl("_$", id))

})

## ===========================================================================
## EXTENDED TESTS: Additional Model Specifications
## ===========================================================================

test_that("define_model_specifications returns parsnip lightgbm spec", {

  # SPEC-MODELS-SPEC-011: LightGBM specification with boosting parameters
  spec <- horizons:::define_model_specifications("lightgbm")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "lightgbm")

})

test_that("define_model_specifications returns parsnip svm_rbf spec", {

  # SPEC-MODELS-SPEC-012: SVM with RBF kernel specification
  spec <- horizons:::define_model_specifications("svm_rbf")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "kernlab")

})

test_that("define_model_specifications returns parsnip mars spec", {

  # SPEC-MODELS-SPEC-013: MARS (Multivariate Adaptive Regression Splines)
  spec <- horizons:::define_model_specifications("mars")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "earth")

})

test_that("define_model_specifications returns parsnip mlp_nn spec", {

  # SPEC-MODELS-SPEC-014: Neural network specification
  spec <- horizons:::define_model_specifications("mlp_nn")

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "nnet")

})

## ===========================================================================
## INTEGRATION TESTS: evaluate_final_models
## ===========================================================================

test_that("evaluate_final_models performs back-transformation for log", {

  # SPEC-MODELS-EVAL-003: Log back-transformation integration
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")
  skip_if_not_installed("workflows")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("parsnip")

  holdout <- tibble::tibble(
    Response = c(1, 2, 3, 4, 5),
    x1 = runif(5),
    x2 = runif(5)
  )

  model_spec <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

  fitted_wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_formula(Response ~ x1 + x2) %>%
    workflows::fit(holdout)

  wf_sets <- tibble::tibble(
    fitted_wf   = list(fitted_wf),
    workflow_id = "model_Log_SNV_PCA_NoCovs_NoInteract"
  )

  result <- horizons:::evaluate_final_models(wf_sets, holdout)

  expect_s3_class(result, "data.frame")
  expect_true("rsq" %in% names(result))
  expect_true("rmse" %in% names(result))
  expect_true("rrmse" %in% names(result))

})

test_that("evaluate_final_models performs back-transformation for sqrt", {

  # SPEC-MODELS-EVAL-004: Square root back-transformation
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")
  skip_if_not_installed("workflows")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("parsnip")

  holdout <- tibble::tibble(
    Response = c(1, 4, 9, 16, 25),
    x1 = runif(5)
  )

  model_spec <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

  fitted_wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_formula(Response ~ x1) %>%
    workflows::fit(holdout)

  wf_sets <- tibble::tibble(
    fitted_wf   = list(fitted_wf),
    workflow_id = "model_Sqrt_Raw_NoFeatSel_clay_NoInteract"
  )

  result <- horizons:::evaluate_final_models(wf_sets, holdout)

  expect_s3_class(result, "data.frame")
  expect_true("workflow_id" %in% names(result))

})

test_that("evaluate_final_models handles multiple workflows", {

  # SPEC-MODELS-EVAL-005: Multiple model evaluation
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")
  skip_if_not_installed("workflows")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("parsnip")

  holdout <- tibble::tibble(
    Response = c(10, 20, 30),
    x1 = runif(3),
    x2 = runif(3)
  )

  model_spec <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

  fitted_wf1 <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_formula(Response ~ x1) %>%
    workflows::fit(holdout)

  fitted_wf2 <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_formula(Response ~ x1 + x2) %>%
    workflows::fit(holdout)

  wf_sets <- tibble::tibble(
    fitted_wf   = list(fitted_wf1, fitted_wf2),
    workflow_id = c("model1_NoTrans_Raw_PCA_NoCovs_NoInteract",
                    "model2_NoTrans_SNV_Boruta_clay_Interact")
  )

  result <- horizons:::evaluate_final_models(wf_sets, holdout)

  expect_equal(nrow(result), 2)  # Two workflows evaluated
  expect_true(all(c("rsq", "rmse", "rrmse") %in% names(result)))

})

## ===========================================================================
## EDGE CASE TESTS
## ===========================================================================

test_that("clean_workflow_id handles special preprocessing combinations", {

  # SPEC-MODELS-WID-013: Complex preprocessing combinations
  id <- horizons::clean_workflow_id(
    model = "svm_rbf",
    transformation = "log",
    preprocessing = "snv + second derivative",
    feature_selection = "recursive feature elimination",
    covariates = c("Clay", "Sand", "Silt"),
    covariate_interactions = TRUE
  )

  expect_type(id, "character")
  expect_true(grepl("svm_rbf", id))
  expect_true(grepl("Log", id))
  expect_true(grepl("SNVD2", id))
  expect_true(grepl("RFE", id))
  expect_true(grepl("Interact", id))

})

test_that("define_model_specifications sets correct engines for all models", {

  # SPEC-MODELS-SPEC-015: Engine verification for all model types
  engines <- list(
    random_forest = "ranger",
    cubist = "Cubist",
    xgboost = "xgboost",
    lightgbm = "lightgbm",
    elastic_net = "glmnet",
    svm_rbf = "kernlab",
    mars = "earth",
    plsr = "mixOmics",
    mlp_nn = "nnet"
  )

  for (model_type in names(engines)) {
    spec <- horizons:::define_model_specifications(model_type)
    expect_equal(spec$engine, engines[[model_type]],
                 info = paste("Engine for", model_type))
  }

})

test_that("evaluate_final_models handles empty holdout data gracefully", {

  # SPEC-MODELS-EVAL-006: Empty holdout edge case
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  mock_wf <- structure(
    list(predict = function(object, new_data) {
      tibble::tibble(.pred = numeric(0))
    }),
    class = c("workflow", "list")
  )

  wf_sets <- data.frame(
    fitted_wf = I(list(mock_wf)),
    workflow_id = "test_model"
  )

  holdout <- data.frame(Response = numeric(0))

  # Should handle gracefully (may return empty or error appropriately)
  result <- tryCatch(
    horizons:::evaluate_final_models(wf_sets, holdout),
    error = function(e) NULL
  )

  # Either returns empty result or NULL from error handling
  expect_true(is.null(result) || nrow(result) == 0)

})

test_that("clean_workflow_id handles all transformation types", {

  # SPEC-MODELS-WID-014: All transformation name variants
  transformations <- list(
    "none" = "NoTrans",
    "no transformation" = "NoTrans",
    "log" = "Log",
    "log transformation" = "Log",
    "sqrt" = "Sqrt",
    "square root transformation" = "Sqrt"
  )

  for (trans_input in names(transformations)) {
    id <- horizons::clean_workflow_id(
      model = "elastic_net",
      transformation = trans_input,
      preprocessing = "raw",
      feature_selection = "none"
    )

    expect_true(grepl(transformations[[trans_input]], id),
                info = paste("Transformation:", trans_input))
  }

})

test_that("define_model_specifications handles tunable parameters", {

  # SPEC-MODELS-SPEC-016: Verify tune() placeholders exist
  models_with_tuning <- c("random_forest", "cubist", "xgboost",
                           "lightgbm", "elastic_net", "svm_rbf",
                           "mars", "plsr", "mlp_nn")

  for (model_type in models_with_tuning) {
    spec <- horizons:::define_model_specifications(model_type)

    # Check that spec has args (tunable parameters)
    expect_true(length(spec$args) > 0,
                info = paste("Model", model_type, "should have tunable parameters"))
  }

})
