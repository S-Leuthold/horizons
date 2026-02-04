# Test Data Fixtures and Generators
# Helper functions to create consistent test data across the test suite


## ---------------------------------------------------------------------------
## Evaluate Test Object
## ---------------------------------------------------------------------------

#' Build a minimal horizons_data object ready for evaluate()
make_eval_object <- function(n = 40, n_wn = 10, n_configs = 2,
                             covariates = NULL,
                             add_validation = TRUE) {

  set.seed(42)

  ## Spectral data
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Optionally add covariates
  if (!is.null(covariates)) {

    for (cov in covariates) {
      df[[cov]] <- runif(n, 0, 100)
      roles <- rbind(roles, tibble::tibble(variable = cov, role = "covariate"))
    }

  }

  ## Build configs
  models <- rep(c("rf", "cubist"), length.out = n_configs)
  configs <- tibble::tibble(
    config_id         = paste0("cfg_", sprintf("%03d", seq_len(n_configs))),
    model             = models,
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = NA_character_
  )

  ## Build horizons_data-like structure
  obj <- list(

    data = list(
      analysis     = df,
      role_map     = roles,
      n_rows       = nrow(df),
      n_predictors = n_wn,
      n_covariates = 0L,
      n_responses  = 1L
    ),

    provenance = list(
      spectra_source = "test",
      spectra_type   = "mir",
      schema_version = 1L
    ),

    config = list(
      configs   = configs,
      n_configs = n_configs,
      tuning    = list(
        cv_folds      = 3L,
        grid_size     = 2L,
        bayesian_iter = 0L
      )
    ),

    validation = list(
      passed    = if (add_validation) TRUE else NULL,
      checks    = NULL,
      timestamp = if (add_validation) Sys.time() else NULL,
      outliers  = list(
        spectral_ids   = NULL,
        response_ids   = NULL,
        removed_ids    = NULL,
        removal_detail = NULL,
        removed        = FALSE
      )
    ),

    evaluation = list(
      results     = NULL,
      best_config = NULL,
      rank_metric = NULL,
      backend     = NULL,
      runtime     = NULL,
      timestamp   = NULL
    ),

    models   = list(workflows = NULL, n_models = NULL,
                    uq = list(enabled = FALSE)),
    ensemble = list(stack = NULL),
    artifacts = list(cache_dir = NULL)

  )

  class(obj) <- c("horizons_data", "list")
  obj

}

library(dplyr)
library(tibble)

## ---------------------------------------------------------------------------
## Spectral Data Generators
## ---------------------------------------------------------------------------

#' Generate Tiny Spectral Dataset (5 samples, 50 wavelengths)
#' For fast unit tests
create_tiny_spectra <- function(seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # Use realistic MIR wavelength range
  wavelengths <- seq(600, 4000, length.out = 50)
  n_samples <- 5
  
  # Generate realistic spectral patterns with soil properties
  spectra_data <- tibble(
    Sample_ID = paste0("TINY_", sprintf("%03d", 1:n_samples)),
    Project = "TestProject",
    SOC = abs(rnorm(n_samples, 2.5, 0.8)),  # Soil Organic Carbon %
    Clay = abs(rnorm(n_samples, 25, 8)),     # Clay content %
    pH = rnorm(n_samples, 6.5, 0.5)         # Soil pH
  )
  
  # Add spectral columns with realistic patterns
  for (wl in wavelengths) {
    col_name <- as.character(round(wl))
    
    # Create absorption features at key wavelengths
    base_abs <- ifelse(wl < 1200, 0.3, 0.5)  # Different regions
    noise <- rnorm(n_samples, 0, 0.05)
    spectra_data[[col_name]] <- base_abs + noise
  }
  
  spectra_data
}

#' Generate Small Spectral Dataset (20 samples, 100 wavelengths) 
#' For integration tests
create_small_spectra <- function(seed = 456) {
  if (!is.null(seed)) set.seed(seed)
  
  wavelengths <- seq(600, 4000, length.out = 100)
  n_samples <- 20
  
  spectra_data <- tibble(
    Sample_ID = paste0("SMALL_", sprintf("%03d", 1:n_samples)),
    Project = rep(c("ProjectA", "ProjectB"), each = 10),
    SOC = abs(rnorm(n_samples, 2.5, 1.2)),
    Clay = abs(rnorm(n_samples, 25, 12)),
    pH = rnorm(n_samples, 6.5, 0.8)
  )
  
  # More complex spectral patterns
  for (wl in wavelengths) {
    col_name <- as.character(round(wl))
    
    # Create multiple absorption features
    base_abs <- 0.4
    if (wl > 2800 & wl < 3000) base_abs <- 0.7  # O-H stretch
    if (wl > 1600 & wl < 1700) base_abs <- 0.6  # C=O stretch
    
    noise <- rnorm(n_samples, 0, 0.03)
    spectra_data[[col_name]] <- pmax(0, base_abs + noise)
  }
  
  spectra_data
}

#' Generate Medium Spectral Dataset (50 samples, 200 wavelengths)
#' For E2E tests
create_medium_spectra <- function(seed = 789) {
  if (!is.null(seed)) set.seed(seed)
  
  wavelengths <- seq(600, 4000, length.out = 200)
  n_samples <- 50
  
  spectra_data <- tibble(
    Sample_ID = paste0("MED_", sprintf("%03d", 1:n_samples)),
    Project = rep(c("ProjectA", "ProjectB", "ProjectC"), length.out = n_samples),
    SOC = abs(rnorm(n_samples, 2.5, 1.5)),
    Clay = abs(rnorm(n_samples, 25, 15)),
    pH = rnorm(n_samples, 6.5, 1.0)
  )
  
  # Complex spectral patterns with outliers
  for (wl in wavelengths) {
    col_name <- as.character(round(wl))
    
    base_abs <- 0.4
    # Multiple realistic absorption features
    if (wl > 2800 & wl < 3000) base_abs <- 0.7  # O-H stretch
    if (wl > 1600 & wl < 1700) base_abs <- 0.6  # C=O stretch  
    if (wl > 1000 & wl < 1200) base_abs <- 0.5  # Silicate features
    
    noise <- rnorm(n_samples, 0, 0.03)
    
    # Add some outlier samples
    outlier_samples <- sample(1:n_samples, 3)
    noise[outlier_samples] <- noise[outlier_samples] + 0.2
    
    spectra_data[[col_name]] <- pmax(0, base_abs + noise)
  }
  
  spectra_data
}

## ---------------------------------------------------------------------------
## Response Variable Generators  
## ---------------------------------------------------------------------------

#' Add Response Variables to Spectral Data
#' Creates realistic soil property values
add_response_variables <- function(spectra_data, variables = c("SOC", "Clay"), seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  n_samples <- nrow(spectra_data)
  
  for (var in variables) {
    if (var == "SOC") {
      # Soil Organic Carbon (%) - log-normal distribution
      spectra_data[[var]] <- exp(rnorm(n_samples, mean = 1.5, sd = 0.5))
    } else if (var == "Clay") {
      # Clay content (%) - beta distribution scaled
      spectra_data[[var]] <- rbeta(n_samples, 2, 5) * 60 + 5
    } else if (var == "pH") {
      # Soil pH - normal around neutral
      spectra_data[[var]] <- rnorm(n_samples, mean = 6.5, sd = 1)
    } else {
      # Generic continuous variable
      spectra_data[[var]] <- rnorm(n_samples, mean = 10, sd = 3)
    }
  }
  
  spectra_data
}

## ---------------------------------------------------------------------------
## Configuration Generators
## ---------------------------------------------------------------------------

#' Create Test Model Configurations
create_test_configs <- function(n_configs = 3) {
  data.frame(
    model = c("linear_reg", "rand_forest", "pls"),
    transformation = c("none", "log", "sqrt"),
    preprocessing = c("raw", "snv", "derivative"),
    feature_selection = c("none", "correlation", "boruta"),
    stringsAsFactors = FALSE
  )[1:n_configs, ]
}

#' Create Minimal Test Configuration (for fast tests)
create_minimal_config <- function() {
  data.frame(
    model = "linear_reg",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    stringsAsFactors = FALSE
  )
}

#' Create Evaluation Test Configuration (for evaluation module tests)
create_eval_test_config <- function() {
  data.frame(
    config_id = "eval_test_001",
    model = "plsr",  # Fast for testing
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = I(list(NULL)),  # No covariates for simplicity
    stringsAsFactors = FALSE
  )
}

#' Create Evaluation Test Data (wrapper for create_small_spectra)
create_eval_test_data <- function(n_samples = 50, n_wavelengths = 100, seed = 123) {
  if (!is.null(seed)) set.seed(seed)

  # Use create_small_spectra if n_samples = 20, otherwise generate
  if (n_samples == 20 && n_wavelengths == 100) {
    data <- create_small_spectra(seed = seed)
  } else {
    # Generate custom size
    wavelengths <- seq(600, 4000, length.out = n_wavelengths)

    data <- tibble(
      Sample_ID = paste0("EVAL_", sprintf("%03d", 1:n_samples)),
      Project = rep("TestProject", n_samples)
    )

    for (wl in wavelengths) {
      col_name <- as.character(round(wl))
      base_abs <- 0.5
      if (wl > 2800 & wl < 3000) base_abs <- 0.7
      noise <- rnorm(n_samples, 0, 0.05)
      data[[col_name]] <- pmax(0.1, base_abs + noise)
    }

    data$SOC <- abs(rnorm(n_samples, 2.5, 1.0))
  }

  # Add Response variable for evaluation
  data$Response <- data$SOC

  data
}

## ---------------------------------------------------------------------------
## Mock tidymodels Objects
## ---------------------------------------------------------------------------

#' Create Mock rsample Split Object
create_mock_split <- function(data, prop = 0.8) {
  n <- nrow(data)
  train_idx <- sample(1:n, floor(n * prop))
  
  # Create a simple mock split object
  split_obj <- list(
    data = data,
    in_id = train_idx,
    out_id = setdiff(1:n, train_idx)
  )
  
  class(split_obj) <- c("rsplit", "list")
  split_obj
}

#' Create Mock CV Folds
create_mock_cv_folds <- function(data, v = 3) {
  n <- nrow(data)
  fold_id <- sample(rep(1:v, length.out = n))
  
  folds <- list()
  for (i in 1:v) {
    analysis_idx <- which(fold_id != i)
    assessment_idx <- which(fold_id == i)
    
    fold <- list(
      data = data,
      in_id = analysis_idx,
      out_id = assessment_idx
    )
    class(fold) <- c("rsplit", "list")
    folds[[i]] <- fold
  }
  
  class(folds) <- c("vfold_cv", "rset", "tbl_df", "tbl", "data.frame")
  folds
}

## ---------------------------------------------------------------------------
## Covariate Data Generators
## ---------------------------------------------------------------------------

#' Create Mock Climate Covariate Data
create_mock_climate_data <- function(sample_ids, seed = 111) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- length(sample_ids)
  
  tibble(
    Sample_ID = sample_ids,
    MAT = rnorm(n, 12, 5),      # Mean Annual Temperature
    MAP = rnorm(n, 800, 200),   # Mean Annual Precipitation  
    Elevation = runif(n, 100, 2000),
    Latitude = runif(n, 30, 50),
    Longitude = runif(n, -120, -80)
  )
}

#' Create Mock Soil Covariate Data  
create_mock_soil_data <- function(sample_ids, seed = 222) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- length(sample_ids)
  
  tibble(
    Sample_ID = sample_ids,
    USDA_Texture = sample(c("Sandy Loam", "Clay Loam", "Silty Clay"), n, replace = TRUE),
    Bulk_Density = rnorm(n, 1.3, 0.2),
    CEC = rnorm(n, 15, 5),
    Base_Saturation = runif(n, 50, 95)
  )
}

## ---------------------------------------------------------------------------
## Prediction Data Generators
## ---------------------------------------------------------------------------

#' Create Mock Prediction Results
create_mock_predictions <- function(n_obs, rmse = 2, bias = 0, seed = 333) {
  if (!is.null(seed)) set.seed(seed)
  
  # Generate "true" values
  truth <- rnorm(n_obs, mean = 10, sd = 3)
  
  # Generate predictions with specified RMSE and bias
  pred <- truth + rnorm(n_obs, mean = bias, sd = rmse)
  
  tibble(
    .pred = pred,
    truth = truth,
    .row = 1:n_obs
  )
}

#' Create Mock Cross-Validation Predictions
create_mock_cv_predictions <- function(n_obs, n_folds = 5, seed = 444) {
  if (!is.null(seed)) set.seed(seed)
  
  preds <- create_mock_predictions(n_obs, seed = seed)
  preds$id <- paste0("Fold", sample(1:n_folds, n_obs, replace = TRUE))
  preds$.row <- 1:n_obs
  
  preds
}

## ---------------------------------------------------------------------------
## Data Validation Helpers
## ---------------------------------------------------------------------------

#' Check if Data has Expected Spectral Structure
expect_spectral_data <- function(data) {
  testthat::expect_s3_class(data, "data.frame")
  testthat::expect_true("Sample_ID" %in% names(data))
  
  # Check for numeric wavelength columns
  numeric_cols <- sapply(data, is.numeric)
  wavelength_cols <- grepl("^[0-9]+\\.?[0-9]*$", names(data))
  spectral_cols <- names(data)[numeric_cols & wavelength_cols]
  
  testthat::expect_true(length(spectral_cols) > 0, 
                       info = "Expected numeric wavelength columns")
  
  invisible(TRUE)
}

#' Check if Config has Required Structure
expect_valid_config <- function(config) {
  testthat::expect_s3_class(config, "data.frame")
  required_cols <- c("model", "transformation", "preprocessing", "feature_selection")
  testthat::expect_true(all(required_cols %in% names(config)),
                       info = "Config missing required columns")

  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## Evaluation Results Fixtures (for finalize/ensemble tests)
## ---------------------------------------------------------------------------

#' Create Mock Evaluation Results
#' Simulates output from evaluate_models_local() for testing finalize/ensemble
create_mock_evaluation_results <- function(n_models = 3, seed = 555) {
  if (!is.null(seed)) set.seed(seed)

  # Create mock best_params for different model types
  models <- c("plsr", "linear_reg", "rand_forest")[1:n_models]

  results <- tibble::tibble(
    workflow_id = paste0("config_", sprintf("%03d", 1:n_models)),
    model = models,
    transformation = sample(c("none", "log", "sqrt"), n_models, replace = TRUE),
    preprocessing = sample(c("raw", "snv", "derivative"), n_models, replace = TRUE),
    feature_selection = sample(c("none", "correlation", "pca"), n_models, replace = TRUE),
    covariates = "",
    status = "success",
    # Add realistic performance metrics
    rmse = runif(n_models, 0.5, 2.0),
    rsq = runif(n_models, 0.6, 0.9),
    rrmse = runif(n_models, 10, 30),
    ccc = runif(n_models, 0.7, 0.95),
    rpd = runif(n_models, 1.5, 3.0),
    mae = runif(n_models, 0.4, 1.5)
  )

  # Add best_params list column (required for finalization)
  results$best_params <- lapply(1:n_models, function(i) {
    if (models[i] == "plsr") {
      data.frame(num_comp = sample(2:10, 1))
    } else if (models[i] == "rand_forest") {
      data.frame(mtry = sample(5:50, 1), min_n = sample(2:20, 1))
    } else {
      data.frame(penalty = 10^runif(1, -3, 0), mixture = runif(1, 0, 1))
    }
  })

  results
}


generate_mock_finalized_models <- function(n_models = 2, seed = 777) {
  if (!is.null(seed)) set.seed(seed)

  create_mock_workflow <- function(id, position) {
    structure(list(id = id, position = position), class = "horizons_mock_workflow")
  }

  create_mock_cv_predictions <- function(n_rows = 5) {
    tibble::tibble(
      .predictions = list(tibble::tibble(
        Response = runif(n_rows, 8, 12),
        .pred    = runif(n_rows, 8, 12),
        .row     = seq_len(n_rows),
        id       = "Fold1"
      ))
    )
  }

  create_mock_cv_metrics <- function() {
    tibble::tibble(
      .metric = c("rmse", "rsq", "rrmse", "ccc", "rpd", "mae"),
      mean    = c(runif(1, 0.5, 1.5),
                  runif(1, 0.7, 0.9),
                  runif(1, 12, 22),
                  runif(1, 0.75, 0.95),
                  runif(1, 2, 3),
                  runif(1, 0.4, 1.1)),
      std_err = rep(0.05, 6)
    )
  }

  create_mock_test_metrics <- function() {
    tibble::tibble(
      rsq  = runif(1, 0.7, 0.9),
      rmse = runif(1, 0.5, 1.5),
      rrmse = runif(1, 12, 22),
      ccc  = runif(1, 0.75, 0.95),
      rpd  = runif(1, 2, 3),
      mae  = runif(1, 0.4, 1.1)
    )
  }

  ids <- paste0("wf_", sprintf("%03d", seq_len(n_models)))
  transformations <- sample(c("none", "log", "sqrt"), n_models, replace = TRUE)

  tibble::tibble(
    wflow_id       = ids,
    workflow       = purrr::map2(ids, seq_along(ids), create_mock_workflow),
    cv_predictions = replicate(n_models, create_mock_cv_predictions(), simplify = FALSE),
    test_results   = replicate(n_models, list(NULL), simplify = FALSE),
    final_params   = replicate(n_models, tibble::tibble(parameter = "penalty", value = 0.001), simplify = FALSE),
    transformation = transformations,
    cv_metrics     = replicate(n_models, create_mock_cv_metrics(), simplify = FALSE),
    test_metrics   = replicate(n_models, create_mock_test_metrics(), simplify = FALSE)
  )
}


#' Create Finalized Models Fixture
#' Simulates output from finalize_top_workflows() for ensemble testing
#' This is a FAST version that skips actual fitting
create_mock_finalized_models <- function(n_models = 2, input_data = NULL, seed = 666) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(input_data)) {
    input_data <- create_eval_test_data(n_samples = 50, seed = seed)
  }

  # Create minimal mock workflows and CV predictions
  results <- tibble::tibble(
    wflow_id = paste0("config_", sprintf("%03d", 1:n_models)),
    workflow = replicate(n_models, list(NULL), simplify = FALSE),  # Placeholder
    cv_predictions = replicate(n_models, list(NULL), simplify = FALSE),  # Placeholder
    transformation = sample(c("none", "log"), n_models, replace = TRUE)
  )

  # Create mock CV metrics
  results$cv_metrics <- lapply(1:n_models, function(i) {
    tibble::tibble(
      .metric = c("rmse", "rsq", "rrmse", "ccc", "rpd", "mae"),
      mean = c(runif(1, 0.5, 1.5), runif(1, 0.7, 0.9), runif(1, 15, 25),
               runif(1, 0.75, 0.95), runif(1, 2, 3), runif(1, 0.4, 1.2)),
      std_err = abs(rnorm(6, 0.05, 0.02))
    )
  })

  # Create mock CV predictions (needed for stacking)
  n_samples <- nrow(input_data)
  results$cv_predictions <- lapply(1:n_models, function(i) {
    # Create mock predictions for all samples
    mock_preds <- tibble::tibble(
      .pred = input_data$Response + rnorm(n_samples, 0, 1),
      Response = input_data$Response,
      .row = 1:n_samples,
      id = paste0("Fold", sample(1:5, n_samples, replace = TRUE))
    )

    # Wrap in list format that stacks expects
    list(.predictions = split(mock_preds, mock_preds$id))
  })

  results
}
