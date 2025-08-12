# Test script for enhanced component effects visualization

library(horizons)
library(dplyr)

# Create realistic test data
set.seed(123)
test_data <- data.frame(
  config_desc = c(
    "cubist_NoTrans_SNV_PCA_Clay-pH-CEC",
    "xgboost_Log_SNVD1_SHAP_GDD-MAP", 
    "random_forest_Sqrt_Raw_Corr_",
    "plsr_NoTrans_MSC_PCA_Clay",
    "elastic_net_Log_SNV_Boruta_GDD-MAP-PET",
    "cubist_Log_SNVD1_PCA_Clay-pH",
    "xgboost_NoTrans_Raw_SHAP_",
    "random_forest_Log_SNV_Corr_GDD-MAP",
    "plsr_Sqrt_MSCD1_PCA_Clay-pH-CEC", 
    "svm_rbf_NoTrans_SNV_SHAP_GDD",
    # Add more variations for better statistical power
    "cubist_NoTrans_SNVD1_Boruta_Clay",
    "xgboost_Sqrt_MSC_Corr_GDD",
    "random_forest_NoTrans_Raw_PCA_pH-CEC",
    "plsr_Log_SNV_SHAP_MAP-PET",
    "elastic_net_NoTrans_MSCD1_Boruta_"
  ),
  rrmse = c(0.45, 0.38, 0.52, 0.48, 0.41, 0.47, 0.36, 0.49, 0.50, 0.43,
           0.42, 0.39, 0.51, 0.46, 0.44),
  rsq = c(0.82, 0.89, 0.76, 0.79, 0.85, 0.80, 0.91, 0.78, 0.77, 0.83,
          0.84, 0.88, 0.77, 0.81, 0.83),
  response_variable = rep(c("MAOM_C", "Total_C"), length.out = 15),
  stringsAsFactors = FALSE
)

cat("Test data created with", nrow(test_data), "rows\n")
print(head(test_data))

# Test 1: Basic enhanced radar plot (no CI to avoid bootstrap time)
cat("\n=== Test 1: Basic Enhanced Radar Plot ===\n")
try({
  plot1 <- plot_component_effects_enhanced(
    results_data = test_data,
    metric = "rrmse",
    include_ci = FALSE,
    effect_size_labels = TRUE,
    title = "Test 1: Basic Enhanced Radar"
  )
  
  if (!is.null(plot1)) {
    cat("✓ Basic radar plot created successfully\n")
    print(class(plot1))
  } else {
    cat("✗ Basic radar plot returned NULL\n")
  }
})

# Test 2: Enhanced bar plot
cat("\n=== Test 2: Enhanced Bar Plot ===\n")
try({
  plot2 <- plot_component_effects_enhanced(
    results_data = test_data,
    metric = "rrmse", 
    plot_type = "bar",
    include_ci = FALSE,
    effect_size_labels = TRUE,
    title = "Test 2: Enhanced Bar Plot"
  )
  
  if (!is.null(plot2)) {
    cat("✓ Bar plot created successfully\n")
    print(class(plot2))
  } else {
    cat("✗ Bar plot returned NULL\n")
  }
})

# Test 3: Multi-response faceted plot
cat("\n=== Test 3: Multi-Response Faceted Plot ===\n")
try({
  plot3 <- plot_component_effects_enhanced(
    results_data = test_data,
    metric = "rrmse",
    plot_type = "bar",
    include_ci = FALSE,
    response_variable_col = "response_variable",
    comparison_type = "facet",
    title = "Test 3: Multi-Response Faceted"
  )
  
  if (!is.null(plot3)) {
    cat("✓ Multi-response plot created successfully\n")
    print(class(plot3))
  } else {
    cat("✗ Multi-response plot returned NULL\n")
  }
})

# Test 4: Component parsing validation
cat("\n=== Test 4: Component Parsing Validation ===\n")
try({
  # Test the component parsing directly
  parsed_data <- parse_config_descriptors(test_data, 
                                         c("ModelType", "Transformation", "Preprocessing"))
  
  cat("Parsed components:\n")
  print(table(parsed_data$ModelType))
  print(table(parsed_data$Transformation))
  print(table(parsed_data$Preprocessing))
  
  if (any(table(parsed_data$ModelType) > 1)) {
    cat("✓ Component parsing shows variation\n")
  } else {
    cat("✗ Component parsing shows no variation\n")
  }
})

cat("\n=== Summary ===\n")
cat("Test script completed. Check above for any errors.\n")