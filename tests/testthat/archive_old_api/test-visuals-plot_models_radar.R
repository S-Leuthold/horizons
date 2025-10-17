# Test file for plot_models_radar function
# Tests cover parsing, computation, visualization, and error handling

library(testthat)
library(dplyr)

# ==============================================================================
# Test Data Setup
# ==============================================================================

# Create sample test data with various config descriptor formats
create_test_data <- function(n = 50) {
  set.seed(42)
  
  models <- c("cubist", "xgboost", "random_forest", "plsr")
  transformations <- c("NoTrans", "Log", "Sqrt")
  preprocessing <- c("Raw", "SNV", "SNVD1")
  feature_selection <- c("PCA", "SHAP", "Corr")
  covariates <- c("Clay-pH", "GDD-MAP", "")
  
  data.frame(
    config_desc = paste(
      sample(models, n, replace = TRUE),
      sample(transformations, n, replace = TRUE),
      sample(preprocessing, n, replace = TRUE),
      sample(feature_selection, n, replace = TRUE),
      sample(covariates, n, replace = TRUE),
      sep = "_"
    ),
    rrmse = runif(n, 0.1, 0.8),
    rsq = runif(n, 0.2, 0.9),
    rmse = runif(n, 5, 25),
    group = sample(c("Group1", "Group2"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Create minimal valid test data
create_minimal_data <- function() {
  data.frame(
    config_desc = c("cubist_NoTrans_SNV_PCA_Clay", "xgboost_Log_Raw_SHAP_"),
    rrmse = c(0.5, 0.7),
    rsq = c(0.8, 0.6),
    stringsAsFactors = FALSE
  )
}

# Create problematic test data for edge cases
create_edge_case_data <- function() {
  data.frame(
    config_desc = c(
      "unknown_model_transformation_preprocessing_feature_covariates",
      "cubist_NoTrans",  # Too few components
      "a_b_c_d_e_f_g_h_i_j_k",  # Too many components
      ""  # Empty string
    ),
    rrmse = c(0.5, 0.6, 0.7, 0.8),
    rsq = c(0.7, 0.6, 0.5, 0.4),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Test Config Descriptor Parsing
# ==============================================================================

test_that("parse_config_descriptors works with standard format", {
  test_data <- create_test_data(20)
  components <- c("ModelType", "Transformation", "Preprocessing", "Feature_Selection", "Covariates")
  
  result <- parse_config_descriptors(test_data, components)
  
  # Check that all components were parsed
  expect_true(all(components %in% names(result)))
  
  # Check that no values are NA for standard formats
  expect_false(any(is.na(result$ModelType)))
  expect_false(any(is.na(result$Transformation)))
  expect_false(any(is.na(result$Preprocessing)))
  expect_false(any(is.na(result$Feature_Selection)))
  
  # Check specific mappings
  expect_true(any(result$ModelType == "Cubist"))
  expect_true(any(result$Transformation == "None"))
  expect_true(any(result$Preprocessing == "SNV"))
})

test_that("parse_config_descriptors handles edge cases gracefully", {
  edge_data <- create_edge_case_data()
  components <- c("ModelType", "Transformation", "Preprocessing")
  
  expect_warning(
    result <- parse_config_descriptors(edge_data, components),
    "Component.*has only unknown/missing values"
  )
  
  # Should still return a data frame with the requested components
  expect_true(all(components %in% names(result)))
  expect_equal(nrow(result), nrow(edge_data))
})

test_that("parse_config_descriptors handles missing components", {
  test_data <- create_minimal_data()
  components <- c("ModelType", "NonexistentComponent")
  
  expect_warning(
    result <- parse_config_descriptors(test_data, components),
    "Failed to parse components"
  )
  
  # Should still parse the components it can
  expect_true("ModelType" %in% names(result))
})

# ==============================================================================
# Test Marginal Effects Computation
# ==============================================================================

test_that("compute_marginal_effects works with valid data", {
  test_data <- create_test_data(50)
  parsed_data <- parse_config_descriptors(test_data, c("ModelType", "Preprocessing", "Transformation"))
  
  result <- compute_marginal_effects(
    data = parsed_data,
    metric = "rrmse",
    components = c("ModelType", "Preprocessing", "Transformation"),
    min_variation = 0
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("component" %in% names(result))
  expect_true("mean_sd" %in% names(result))
  expect_true(all(!is.na(result$mean_sd)))
  expect_true(all(result$mean_sd >= 0))
})

test_that("compute_marginal_effects handles grouped data", {
  test_data <- create_test_data(50)
  parsed_data <- parse_config_descriptors(test_data, c("ModelType", "Preprocessing"))
  
  result <- compute_marginal_effects(
    data = parsed_data,
    metric = "rrmse",
    components = c("ModelType", "Preprocessing"),
    group_by = "group",
    min_variation = 0
  )
  
  expect_true("group" %in% names(result))
  expect_true(all(c("Group1", "Group2") %in% result$group))
})

test_that("compute_marginal_effects filters low variation components", {
  test_data <- create_minimal_data()
  test_data$rrmse <- c(0.5, 0.5)  # No variation
  parsed_data <- parse_config_descriptors(test_data, c("ModelType"))
  
  expect_warning(
    result <- compute_marginal_effects(
      data = parsed_data,
      metric = "rrmse",
      components = c("ModelType"),
      min_variation = 0.1
    ),
    "Components with low variation"
  )
})

test_that("compute_marginal_effects handles missing metric values", {
  test_data <- create_minimal_data()
  test_data$rrmse[1] <- NA
  parsed_data <- parse_config_descriptors(test_data, c("ModelType"))
  
  result <- compute_marginal_effects(
    data = parsed_data,
    metric = "rrmse",
    components = c("ModelType"),
    min_variation = 0
  )
  
  expect_equal(nrow(result), 0)  # Should filter out the NA row, leaving insufficient data
})

# ==============================================================================
# Test Main Plot Function
# ==============================================================================

test_that("plot_models_radar creates radar plot successfully", {
  test_data <- create_test_data(30)
  
  p <- plot_models_radar(
    results_data = test_data,
    metric = "rrmse",
    plot_type = "radar",
    components = c("ModelType", "Preprocessing", "Transformation")
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check that it's using polar coordinates (radar plot)
  expect_true("CoordFixed" %in% class(p$coordinates))
})

test_that("plot_models_radar creates bar plot successfully", {
  test_data <- create_test_data(30)
  
  p <- plot_models_radar(
    results_data = test_data,
    metric = "rrmse",
    plot_type = "bar",
    components = c("ModelType", "Preprocessing")
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check basic plot structure
  expect_true(length(p$layers) > 0)
})

test_that("plot_models_radar handles grouped data", {
  test_data <- create_test_data(40)
  
  p <- plot_models_radar(
    results_data = test_data,
    metric = "rrmse",
    plot_type = "bar",
    group_by = "group",
    components = c("ModelType", "Preprocessing")
  )
  
  expect_s3_class(p, "ggplot")
  
  # Should have faceting for grouped data
  expect_true(!is.null(p$facet))
})

test_that("plot_models_radar works with different color schemes", {
  test_data <- create_test_data(20)
  
  # Test each color scheme
  color_schemes <- c("default", "viridis", "plasma")
  
  for (scheme in color_schemes) {
    p <- plot_models_radar(
      results_data = test_data,
      metric = "rrmse",
      color_scheme = scheme,
      components = c("ModelType", "Preprocessing")
    )
    
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_models_radar handles normalization", {
  test_data <- create_test_data(20)
  
  # Test with normalization
  p1 <- plot_models_radar(
    results_data = test_data,
    metric = "rrmse",
    normalize = TRUE,
    components = c("ModelType", "Preprocessing")
  )
  
  # Test without normalization
  p2 <- plot_models_radar(
    results_data = test_data,
    metric = "rrmse",
    normalize = FALSE,
    components = c("ModelType", "Preprocessing")
  )
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

# ==============================================================================
# Test Error Handling
# ==============================================================================

test_that("plot_models_radar handles invalid input data", {
  # Test with non-data.frame input
  expect_null(plot_models_radar(results_data = "not a data frame", metric = "rrmse"))
  
  # Test with missing config_desc column
  bad_data <- data.frame(rrmse = c(0.1, 0.2))
  expect_null(plot_models_radar(results_data = bad_data, metric = "rrmse"))
  
  # Test with missing metric column
  bad_data2 <- data.frame(config_desc = c("a_b_c_d_e", "f_g_h_i_j"))
  expect_null(plot_models_radar(results_data = bad_data2, metric = "nonexistent_metric"))
  
  # Test with empty data
  empty_data <- data.frame(config_desc = character(0), rrmse = numeric(0))
  expect_null(plot_models_radar(results_data = empty_data, metric = "rrmse"))
})

test_that("plot_models_radar handles invalid parameters", {
  test_data <- create_test_data(10)
  
  # Test invalid plot_type
  expect_error(
    plot_models_radar(results_data = test_data, metric = "rrmse", plot_type = "invalid"),
    "should be one of"
  )
  
  # Test invalid color_scheme
  expect_error(
    plot_models_radar(results_data = test_data, metric = "rrmse", color_scheme = "invalid"),
    "should be one of"
  )
  
  # Test invalid group_by column
  expect_null(
    plot_models_radar(results_data = test_data, metric = "rrmse", group_by = "nonexistent_column")
  )
})

test_that("plot_models_radar handles insufficient data", {
  # Create data with only one unique value per component
  minimal_data <- data.frame(
    config_desc = c("cubist_NoTrans_SNV_PCA_Clay", "cubist_NoTrans_SNV_PCA_Clay"),
    rrmse = c(0.5, 0.6),
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    result <- plot_models_radar(
      results_data = minimal_data, 
      metric = "rrmse",
      components = c("ModelType", "Preprocessing")
    ),
    "Components with low variation"
  )
})

# ==============================================================================
# Test Helper Functions
# ==============================================================================

test_that("get_color_scheme returns correct number of colors", {
  colors_default <- get_color_scheme("default", 5)
  expect_length(colors_default, 5)
  expect_type(colors_default, "character")
  
  colors_viridis <- get_color_scheme("viridis", 3)
  expect_length(colors_viridis, 3)
  
  colors_plasma <- get_color_scheme("plasma", 7)
  expect_length(colors_plasma, 7)
})

test_that("apply_color_scheme returns ggplot scale objects", {
  scale_fill <- apply_color_scheme("default", is_fill = TRUE)
  scale_color <- apply_color_scheme("viridis", is_fill = FALSE)
  
  expect_s3_class(scale_fill, "ScaleDiscrete")
  expect_s3_class(scale_color, "Scale")
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("full workflow integration test", {
  # Test the complete workflow from raw data to plot
  test_data <- create_test_data(100)
  
  # Test both plot types
  radar_plot <- plot_models_radar(
    results_data = test_data,
    metric = "rrmse",
    plot_type = "radar",
    components = c("ModelType", "Transformation", "Preprocessing", "Feature_Selection"),
    normalize = TRUE,
    title = "Integration Test Radar Plot"
  )
  
  bar_plot <- plot_models_radar(
    results_data = test_data,
    metric = "rsq",
    plot_type = "bar",
    components = c("ModelType", "Preprocessing", "Covariates"),
    normalize = FALSE,
    color_scheme = "viridis"
  )
  
  expect_s3_class(radar_plot, "ggplot")
  expect_s3_class(bar_plot, "ggplot")
})

test_that("performance with large dataset", {
  # Test with larger dataset to ensure reasonable performance
  large_data <- create_test_data(500)
  
  start_time <- Sys.time()
  result <- plot_models_radar(
    results_data = large_data,
    metric = "rrmse",
    components = c("ModelType", "Transformation", "Preprocessing")
  )
  end_time <- Sys.time()
  
  expect_s3_class(result, "ggplot")
  
  # Should complete within reasonable time (5 seconds)
  expect_lt(as.numeric(end_time - start_time), 5)
})