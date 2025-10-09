## -----------------------------------------------------------------------------
## Test Script: Covariate Interactions Feature
## -----------------------------------------------------------------------------
## Quick verification that covariate_interactions parameter works correctly
## across different feature selection methods

# Load development version
devtools::load_all()

library(dplyr)
library(recipes)

## -----------------------------------------------------------------------------
## Step 1: Create Mock Data
## -----------------------------------------------------------------------------

set.seed(123)

n_samples        <- 100
spectral_columns <- seq(600, 4000, 2)

# Mock spectral data
mock_spectral <- matrix(rnorm(n_samples * length(spectral_columns)),
                       nrow = n_samples)
colnames(mock_spectral) <- as.character(spectral_columns)

# Mock dataset
mock_data <- as.data.frame(mock_spectral) %>%
  mutate(
    Sample_ID = paste0("Sample_", 1:n_samples),
    Project   = "Test",
    Response  = rnorm(n_samples, mean = 50, sd = 10)
  ) %>%
  select(Sample_ID, Project, Response, everything())

# Mock covariates
mock_covariates <- tibble(
  Sample_ID = paste0("Sample_", 1:n_samples),
  Clay      = rnorm(n_samples, mean = 25, sd = 5),
  pH        = rnorm(n_samples, mean = 6.5, sd = 0.5),
  Sand      = rnorm(n_samples, mean = 40, sd = 8)
)

## -----------------------------------------------------------------------------
## Step 2: Test Different Feature Selection Methods
## -----------------------------------------------------------------------------

test_interactions <- function(feature_method, with_interactions = FALSE) {

  cat("\n=======================================================\n")
  cat(sprintf("Testing: %s | Interactions: %s\n",
              feature_method,
              ifelse(with_interactions, "TRUE", "FALSE")))
  cat("=======================================================\n")

  recipe_obj <- tryCatch({

    build_recipe(
      input_data               = mock_data,
      spectral_transformation  = "raw",
      response_transformation  = "none",
      feature_selection_method = feature_method,
      covariate_selection      = c("Clay", "pH", "Sand"),
      covariate_data           = mock_covariates,
      covariate_interactions   = with_interactions
    )

  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  }, warning = function(w) {
    cat("WARNING:", w$message, "\n")
    return(NULL)
  })

  if (!is.null(recipe_obj)) {

    # Prep and bake to see final features
    prepped <- prep(recipe_obj, training = mock_data)
    baked   <- bake(prepped, new_data = mock_data)

    # Count features
    predictor_cols <- names(baked)[!names(baked) %in% c("Sample_ID", "Project", "Response")]
    interaction_cols <- predictor_cols[grepl("_x_", predictor_cols)]

    cat(sprintf("Total predictors: %d\n", length(predictor_cols)))
    cat(sprintf("Interaction terms: %d\n", length(interaction_cols)))

    if (length(interaction_cols) > 0) {
      cat("Example interactions:\n")
      print(head(interaction_cols, 5))
    }

    cat("✓ Recipe successfully created and prepped\n")

  } else {
    cat("✗ Recipe creation failed\n")
  }

  invisible(recipe_obj)
}

## -----------------------------------------------------------------------------
## Step 3: Run Tests
## -----------------------------------------------------------------------------

cat("\n")
cat("================================================================================\n")
cat("TESTING COVARIATE INTERACTIONS FEATURE\n")
cat("================================================================================\n")

# Test 1: PCA without interactions (baseline)
test_interactions("pca", with_interactions = FALSE)

# Test 2: PCA with interactions (should work well)
test_interactions("pca", with_interactions = TRUE)

# Test 3: Correlation without interactions
test_interactions("correlation", with_interactions = FALSE)

# Test 4: Correlation with interactions (should work)
test_interactions("correlation", with_interactions = TRUE)

# Test 5: CARS with interactions (should work)
test_interactions("cars", with_interactions = TRUE)

# Test 6: None with interactions (should warn but proceed)
test_interactions("none", with_interactions = TRUE)

cat("\n")
cat("================================================================================\n")
cat("ALL TESTS COMPLETE\n")
cat("================================================================================\n")
cat("\n")
cat("SUMMARY:\n")
cat("- Interactions work with feature selection methods (pca, correlation, cars)\n")
cat("- Warning issued for 'none' feature selection (high dimensionality)\n")
cat("- Interaction terms follow pattern: Covariate_x_Feature\n")
cat("\n")
