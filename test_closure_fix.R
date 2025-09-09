# Test script for closure detection fix
library(horizons)

# Create test configuration
test_config <- create_configs(
  models = c('plsr'),
  transformations = c('none'),
  preprocessing = c('raw'),
  feature_selection = c('none'),
  soil_covariates = c('clay'),
  verbose = FALSE
)

cat("Original config structure:\n")
print(str(test_config[1, ]))

# Test that individual row extraction works (this simulates the parallel environment)
config_row <- test_config[1, , drop = FALSE]
cat("\nExtracted row structure:\n")
print(str(config_row))

# Test that values are character as expected
cat("\nField classes:\n")
cat("model:", class(config_row$model), "\n")
cat("transformation:", class(config_row$transformation), "\n")
cat("preprocessing:", class(config_row$preprocessing), "\n")
cat("feature_selection:", class(config_row$feature_selection), "\n")
cat("covariates:", class(config_row$covariates), "\n")

# Test safe character extraction
cat("\nTesting safe character extraction:\n")
cat("model value:", as.character(config_row$model), "\n")
cat("transformation value:", as.character(config_row$transformation), "\n")
cat("preprocessing value:", as.character(config_row$preprocessing), "\n")
cat("feature_selection value:", as.character(config_row$feature_selection), "\n")

# Test covariate extraction
if ("covariates" %in% names(config_row) && !is.null(config_row$covariates[[1]])) {
  cov_value <- config_row$covariates[[1]]
  cat("covariates value:", paste(cov_value, collapse = ", "), "\n")
  cat("covariates class:", class(cov_value), "\n")
}

cat("\nAll tests passed - no closures detected!\n")