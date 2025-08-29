#' Evaluate a Single Model Configuration
#'
#' @description
#' Core function for evaluating a single model configuration.
#' Implements a complete modeling pipeline: recipe building → model specification →
#' hyperparameter tuning (grid + Bayesian) → final evaluation on test set.
#' Designed for sequential model evaluation with optional parallel cross-validation.
#'
#' @param config_row Single-row data frame or tibble containing model configuration with columns:
#'   \itemize{
#'     \item \code{model}: Character. Model type (e.g., "random_forest", "cubist", "xgboost")
#'     \item \code{transformation}: Character. Response transformation (e.g., "Log Transformation")
#'     \item \code{preprocessing}: Character. Spectral preprocessing (e.g., "snv", "raw")
#'     \item \code{feature_selection}: Character. Feature selection method (e.g., "pca", "none")
#'     \item \code{covariates}: List column or NULL. Covariate names to include
#'   }
#' @param input_data Data frame containing full dataset with spectral features and response.
#'   Used for recipe preparation and preprocessing steps.
#' @param data_split An \code{rsplit} object from rsample package defining train/test split.
#'   Ensures consistent data partitioning across all model configurations.
#' @param config_id Character or numeric. Unique identifier for this configuration,
#'   used in progress messages and error reporting.
#' @param covariate_data Optional data frame containing covariate predictions.
#'   Must have same row ordering as \code{input_data}. NULL if no covariates.
#' @param variable Character. Name of response variable column in \code{input_data}.
#' @param output_dir Character path to directory for saving outputs (checkpoints, logs).
#'   NULL skips file output.
#' @param grid_size Integer. Number of hyperparameter combinations for initial grid search.
#'   Uses Latin hypercube sampling for better parameter space coverage. Default: 10.
#' @param bayesian_iter Integer. Number of Bayesian optimization iterations after grid search.
#'   Set to 0 to skip Bayesian optimization. Default: 15.
#' @param cv_folds Integer. Number of cross-validation folds for hyperparameter tuning.
#'   Minimum 2, recommended 5-10. Uses stratified sampling on response. Default: 10.
#' @param parallel_cv Logical. Enable parallel processing for cross-validation folds.
#'   Uses doMC on Unix/macOS, doParallel on Windows. Default: TRUE.
#' @param n_cv_cores Integer or NULL. Number of cores for parallel CV.
#'   NULL auto-detects (total cores - 2). Ignored if parallel_cv = FALSE.
#' @param prune_models Logical. Skip Bayesian optimization for models that don't beat
#'   baseline (mean predictor) by required margin. Saves computation. Default: FALSE.
#' @param prune_threshold Numeric between 0 and 1. Multiplier for baseline RRMSE.
#'   Model must achieve RRMSE < baseline_RRMSE * threshold to proceed to Bayesian.
#'   Example: 0.9 = must beat baseline by 10%. Default: 0.9.
#' @param seed Integer. Random seed for reproducibility of CV splits and tuning. Default: 123.
#'
#' @return A single-row tibble containing:
#'   \describe{
#'     \item{config_id}{Configuration identifier}
#'     \item{workflow_id}{Unique workflow identifier string}
#'     \item{model}{Model type used}
#'     \item{transformation}{Response transformation applied}
#'     \item{preprocessing}{Spectral preprocessing method}
#'     \item{feature_selection}{Feature selection method}
#'     \item{covariates}{Concatenated covariate names or NA}
#'     \item{rsq}{R-squared on test set}
#'     \item{rmse}{Root mean squared error on test set}
#'     \item{rrmse}{Relative RMSE as percentage}
#'     \item{rpd}{Ratio of performance to deviation}
#'     \item{ccc}{Lin's concordance correlation coefficient}
#'     \item{mae}{Mean absolute error on test set}
#'     \item{grid_seconds}{Time for grid search in seconds}
#'     \item{bayes_seconds}{Time for Bayesian optimization in seconds}
#'     \item{total_seconds}{Total execution time in seconds}
#'     \item{status}{"success", "failed", or "pruned"}
#'     \item{error_message}{Error description if failed, NA otherwise}
#'     \item{fitted_workflow}{List column with fitted workflow object for stacking}
#'   }
#'
#' @details
#' The function executes the following pipeline:
#' \enumerate{
#'   \item Input validation and configuration extraction
#'   \item Recipe building with specified preprocessing and feature selection
#'   \item Model specification from configuration
#'   \item Workflow creation (recipe + model)
#'   \item Cross-validation split generation (stratified on response)
#'   \item Optional parallel backend setup (OS-aware)
#'   \item Grid search with Latin hypercube sampling
#'   \item Aggressive memory cleanup after grid search
#'   \item Optional pruning based on baseline comparison
#'   \item Bayesian optimization (if not pruned) with early stopping
#'   \item Final model fit on full training set
#'   \item Test set evaluation with comprehensive metrics
#' }
#'
#' Baseline comparison for pruning uses RMSE of mean predictor. Models must beat
#' baseline * threshold to proceed to Bayesian optimization.
#'
#' All errors are caught and returned as failed results with descriptive messages,
#' ensuring batch evaluation continues even if individual models fail.
#'
#' @seealso
#' \code{\link{build_recipe}} for recipe construction details
#' \code{\link{define_model_specifications}} for supported model types
#' \code{\link{rrmse}}, \code{\link{rpd}}, \code{\link{ccc}} for custom metrics
#'
#' @examples
#' \dontrun{
#' # Create configuration
#' config <- tibble::tibble(
#'   model = "random_forest",
#'   transformation = "Log Transformation",
#'   preprocessing = "snv",
#'   feature_selection = "pca",
#'   covariates = list(c("clay", "sand"))
#' )
#' 
#' # Create train/test split
#' split <- rsample::initial_split(spectral_data, prop = 0.8)
#' 
#' # Evaluate single model
#' result <- evaluate_single_model_local(
#'   config_row = config[1, ],
#'   input_data = spectral_data,
#'   data_split = split,
#'   config_id = "RF_001",
#'   variable = "SOC",
#'   parallel_cv = TRUE,
#'   prune_models = TRUE
#' )
#' }
#'
#' @keywords internal
evaluate_configuration <- function(config_row,
                                        input_data,
                                        data_split,
                                        config_id,
                                        covariate_data = NULL,
                                        variable,
                                        output_dir     = NULL,
                                        grid_size      = 10,
                                        bayesian_iter  = 15,
                                        cv_folds       = 5,
                                        parallel_cv    = TRUE,
                                        n_cv_cores     = NULL,
                                        prune_models   = FALSE,
                                        prune_threshold = 0.9,
                                        seed           = 123) {
  
  ## Setup ---------------------------------------------------------------------
  
  start_time <- Sys.time()
  
  ## Input Validation ----------------------------------------------------------
  
  if (!is.data.frame(config_row) || nrow(config_row) != 1) {
    cli::cli_abort("▶ evaluate_single_model_local: config_row must be a single-row data frame")
  }
  
  if (!inherits(data_split, "rsplit")) {
    cli::cli_abort("▶ evaluate_single_model_local: data_split must be an rsplit object from rsample")
  }
  
  if (!is.character(variable) || length(variable) != 1) {
    cli::cli_abort("▶ evaluate_single_model_local: variable must be a single character string")
  }
  
  if (!variable %in% names(input_data)) {
    cli::cli_abort("▶ evaluate_single_model_local: variable '{variable}' not found in input_data")
  }
  
  if (!is.numeric(cv_folds) || cv_folds < 2) {
    cli::cli_abort("▶ evaluate_single_model_local: cv_folds must be at least 2, got {cv_folds}")
  }
  
  if (!is.numeric(grid_size) || grid_size <= 0) {
    cli::cli_abort("▶ evaluate_single_model_local: grid_size must be positive, got {grid_size}")
  }
  
  if (!is.numeric(bayesian_iter) || bayesian_iter < 0) {
    cli::cli_abort("▶ evaluate_single_model_local: bayesian_iter must be non-negative, got {bayesian_iter}")
  }
  
  if (!is.numeric(prune_threshold) || prune_threshold <= 0 || prune_threshold > 1) {
    cli::cli_abort("▶ evaluate_single_model_local: prune_threshold must be between 0 and 1, got {prune_threshold}")
  }
  
  if (!is.null(n_cv_cores) && (!is.numeric(n_cv_cores) || n_cv_cores <= 0)) {
    cli::cli_abort("▶ evaluate_single_model_local: n_cv_cores must be positive if specified, got {n_cv_cores}")
  }
  
  ## Extract Configuration -----------------------------------------------------
  
  config_row %>%
    dplyr::mutate(
      model             = as.character(model),
      transformation    = as.character(transformation),
      preprocessing     = as.character(preprocessing),
      feature_selection = as.character(feature_selection %||% "none"),
      covariates        = covariates %||% NULL  # NULL = no covariates
    ) ->
  config_clean
  
  # Warn if using defaults
  if (is.null(config_row$feature_selection)) {
    cli::cli_alert_warning("▶ Model {config_id}: No feature_selection specified, using 'none'")
  }
  
  # Validate against package constants (assumed to exist)
  if (!config_clean$model %in% VALID_MODELS) {
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Invalid model type '{config_clean$model}'")
      )
    )
  }
  
  if (!config_clean$transformation %in% VALID_TRANSFORMATIONS) {
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Invalid transformation '{config_clean$transformation}'")
      )
    )
  }
  
  ## Create Workflow ID --------------------------------------------------------
  
  workflow_id <- clean_workflow_id(
    model             = config_clean$model,
    transformation    = config_clean$transformation,
    preprocessing     = config_clean$preprocessing,
    feature_selection = config_clean$feature_selection,
    covariates        = config_clean$covariates
  )
  
  ## Build Recipe --------------------------------------------------------------
  
  recipe_result <- safely_execute(
    expr = {
      build_recipe(
        input_data               = input_data,  # Full data for preprocessing
        spectral_transformation  = config_clean$preprocessing,
        response_transformation  = config_clean$transformation,
        feature_selection_method = config_clean$feature_selection,
        covariate_selection      = config_clean$covariates,  # NULL = no covariates
        covariate_data          = covariate_data
      )
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to build recipe for {workflow_id}")
  )
  
  if (is.null(recipe_result$result)) {
    actual_error <- recipe_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Recipe building failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  recipe <- recipe_result$result
  
  ## Extract Training Data -----------------------------------------------------
  
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)
  
  ## Define Model Specification ------------------------------------------------
  
  model_spec_result <- safely_execute(
    expr = {
      define_model_specifications(config_clean$model)
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to define model spec for {config_clean$model}")
  )
  
  if (is.null(model_spec_result$result)) {
    actual_error <- model_spec_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Model specification failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  model_spec <- model_spec_result$result
  
  ## Create Workflow -----------------------------------------------------------
  
  wflow_result <- safely_execute(
    expr = {
      workflows::workflow() %>%
        workflows::add_recipe(recipe) %>%
        workflows::add_model(model_spec)
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to create workflow for {workflow_id}")
  )
  
  if (is.null(wflow_result$result)) {
    actual_error <- wflow_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Workflow creation failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  wflow <- wflow_result$result
  
  ## Create CV Splits from Training Data ---------------------------------------
  
  # Standard k-fold CV with stratification on response variable
  # Future enhancement: Support leave-one-site-out or spatial CV strategies
  cv_splits_result <- safely_execute(
    expr = {
      # Try stratified CV first, fall back to random if it fails
      tryCatch(
        {
          rsample::vfold_cv(
            data   = train_data,
            v      = cv_folds,
            strata = !!variable  # Stratify on response for balanced folds
          )
        },
        error = function(e) {
          cli::cli_alert_warning("▶ Stratification failed, using random CV splits: {conditionMessage(e)}")
          rsample::vfold_cv(
            data = train_data,
            v    = cv_folds
          )
        }
      )
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to create {cv_folds}-fold CV splits")
  )
  
  if (is.null(cv_splits_result$result)) {
    actual_error <- cv_splits_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("CV split creation failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  cv_splits <- cv_splits_result$result
  
  ## Step 7: Setup Parallel Backend (if requested) ----------------------------
  
  cluster_object <- NULL  # Track cluster for cleanup
  
  if (parallel_cv) {
    # Detect cores if not specified
    if (is.null(n_cv_cores)) {
      n_cv_cores <- max(1, parallel::detectCores() - DEFAULT_CORE_BUFFER)
    }
    
    # Ensure cores request is reasonable
    n_cv_cores <- min(n_cv_cores, parallel::detectCores())
    
    # OS-specific backend registration with proper checks
    parallel_success <- FALSE
    
    if (Sys.info()["sysname"] == "Darwin" || .Platform$OS.type == "unix") {
      # macOS and Linux - use doMC if available
      if (requireNamespace("doMC", quietly = TRUE)) {
        tryCatch({
          doMC::registerDoMC(cores = n_cv_cores)
          parallel_success <- TRUE
          cli::cli_alert_info("▶ Parallel backend: doMC with {n_cv_cores} cores")
        }, error = function(e) {
          cli::cli_alert_warning("doMC registration failed: {e$message}")
        })
      } else {
        cli::cli_alert_warning("doMC package not available")
      }
      
      # Fallback to doParallel if doMC failed
      if (!parallel_success && requireNamespace("doParallel", quietly = TRUE)) {
        tryCatch({
          cluster_object <- parallel::makeCluster(n_cv_cores, type = "FORK")
          doParallel::registerDoParallel(cluster_object)
          parallel_success <- TRUE
          cli::cli_alert_info("▶ Parallel backend: doParallel (FORK) with {n_cv_cores} cores")
        }, error = function(e) {
          cli::cli_alert_warning("doParallel registration failed: {e$message}")
        })
      }
    } else {  
      # Windows - use doParallel with PSOCK cluster
      if (requireNamespace("doParallel", quietly = TRUE)) {
        tryCatch({
          cluster_object <- parallel::makeCluster(n_cv_cores, type = "PSOCK")
          doParallel::registerDoParallel(cluster_object)
          parallel_success <- TRUE
          cli::cli_alert_info("▶ Parallel backend: doParallel (PSOCK) with {n_cv_cores} cores")
        }, error = function(e) {
          cli::cli_alert_warning("doParallel registration failed: {e$message}")
        })
      }
    }
    
    # Fall back to sequential if parallel setup failed
    if (!parallel_success) {
      cli::cli_alert_warning("Parallel setup failed, falling back to sequential processing")
      foreach::registerDoSEQ()
      parallel_cv <- FALSE  # Update flag to reflect actual state
    }
    
    # Setup cleanup for any cluster objects
    if (!is.null(cluster_object)) {
      on.exit({
        try(parallel::stopCluster(cluster_object), silent = TRUE)
        try(foreach::registerDoSEQ(), silent = TRUE)
      }, add = TRUE)
    }
  } else {
    foreach::registerDoSEQ()
    cli::cli_alert_info("▶ Sequential processing (no parallelization)")
  }
  
  ## Step 8a: Grid Search ------------------------------------------------------
  
  # Generate Latin hypercube grid for better parameter space coverage
  grid_result <- safely_execute(
    expr = {
      # Extract parameter set from workflow
      param_set <- workflows::extract_parameter_set_dials(wflow)
      
      # Generate Latin hypercube design
      dials::grid_latin_hypercube(
        param_set,
        size = grid_size
      )
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to generate parameter grid")
  )
  
  if (is.null(grid_result$result)) {
    # Fall back to auto-grid if Latin hypercube fails
    cli::cli_alert_warning("▶ Latin hypercube failed, using auto-grid")
    grid <- grid_size  # Let tune auto-generate
  } else {
    grid <- grid_result$result
  }
  
  # Run grid search with RRMSE as primary metric
  grid_start_time <- Sys.time()
  
  grid_tune_result <- safely_execute(
    expr = {
      tune::tune_grid(
        object    = wflow,
        resamples = cv_splits,
        grid      = grid,
        metrics   = yardstick::metric_set(rrmse, rmse, rsq, mae, rpd, ccc),
        control   = tune::control_grid(
          save_pred     = FALSE,      # Save memory
          save_workflow = FALSE,      # Save memory
          verbose       = FALSE,
          extract       = NULL,
          allow_par     = parallel_cv,  # Match our parallel flag
          parallel_over = "resamples"    # Parallelize over CV folds
        )
      )
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Grid search failed for {workflow_id}")
  )
  
  if (is.null(grid_tune_result$result)) {
    actual_error <- grid_tune_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Grid tuning failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  grid_results <- grid_tune_result$result
  grid_seconds <- as.numeric(difftime(Sys.time(), grid_start_time, units = "secs"))
  
  # Aggressive memory cleanup after grid search
  invisible(gc(verbose = FALSE, full = TRUE))
  
  ## Step 8b: Prune Grid Results (Optional) -----------------------------------
  
  skip_bayesian <- FALSE
  
  if (prune_models) {
    # Calculate baseline RRMSE (if we always predicted the mean)
    baseline_rmse <- sd(train_data[[variable]], na.rm = TRUE)
    baseline_mean <- mean(train_data[[variable]], na.rm = TRUE)
    
    # Check for near-zero mean to avoid division issues
    if (abs(baseline_mean) < .Machine$double.eps) {
      cli::cli_alert_warning("▶ Response mean too close to zero, skipping pruning")
      skip_bayesian <- FALSE
    } else {
      baseline_rrmse <- (baseline_rmse / abs(baseline_mean)) * 100
    
    # Extract best RRMSE from grid search
    best_rrmse <- tune::collect_metrics(grid_results) %>%
      dplyr::filter(.metric == "rrmse") %>%
      dplyr::summarise(min_rrmse = min(mean, na.rm = TRUE)) %>%
      dplyr::pull(min_rrmse)
    
      # Check if model beats baseline by required margin
      threshold_rrmse <- baseline_rrmse * prune_threshold
      
      if (is.infinite(best_rrmse) || best_rrmse > threshold_rrmse) {
        skip_bayesian <- TRUE
        cli::cli_alert_warning(
          "▶ Skipping Bayesian: RRMSE ({round(best_rrmse, 1)}%) doesn't beat baseline ({round(baseline_rrmse, 1)}%) * {prune_threshold}"
        )
      }
    }
  }
  
  ## Step 8c: Bayesian Optimization -------------------------------------------
  
  bayes_seconds <- 0
  
  if (!skip_bayesian && bayesian_iter > 0) {
    bayes_start_time <- Sys.time()
    
    bayes_tune_result <- safely_execute(
      expr = {
        tune::tune_bayes(
          object    = wflow,
          resamples = cv_splits,
          initial   = grid_results,  # Start from grid search results
          iter      = bayesian_iter,
          metrics   = yardstick::metric_set(rrmse, rmse, rsq, mae, rpd, ccc),
          control   = tune::control_bayes(
            save_pred     = FALSE,
            save_workflow = FALSE,
            verbose       = FALSE,
            no_improve    = BAYES_NO_IMPROVE_LIMIT,  # Stop after N iterations without improvement
            allow_par     = parallel_cv,
            parallel_over = "resamples"
          )
        )
      },
      default_value = NULL,
      error_message = glue::glue("▶ evaluate_single_model_local: Bayesian optimization failed for {workflow_id}")
    )
    
    if (is.null(bayes_tune_result$result)) {
      # If Bayesian fails, keep grid results
      cli::cli_alert_warning("▶ Bayesian optimization failed, using grid results")
      final_tune_results <- grid_results
    } else {
      final_tune_results <- bayes_tune_result$result
    }
    
    bayes_seconds <- as.numeric(difftime(Sys.time(), bayes_start_time, units = "secs"))
  } else {
    # Use grid results if Bayesian was skipped
    final_tune_results <- grid_results
  }
  
  ## Step 9: Final Model Fit ---------------------------------------------------
  
  # Select best hyperparameters based on RRMSE
  best_params_result <- safely_execute(
    expr = {
      tune::select_best(
        final_tune_results,
        metric = "rrmse"
      )
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to select best parameters")
  )
  
  if (is.null(best_params_result$result)) {
    actual_error <- best_params_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Parameter selection failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  best_params <- best_params_result$result
  
  # Finalize workflow with best parameters
  final_wflow_result <- safely_execute(
    expr = {
      tune::finalize_workflow(wflow, best_params)
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to finalize workflow")
  )
  
  if (is.null(final_wflow_result$result)) {
    actual_error <- final_wflow_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Workflow finalization failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  final_wflow <- final_wflow_result$result
  
  ## Step 10: Test Set Evaluation ----------------------------------------------
  
  # Fit on full training set and evaluate on test set
  final_fit_result <- safely_execute(
    expr = {
      tune::last_fit(
        final_wflow,
        split   = data_split,
        metrics = yardstick::metric_set(rrmse, rmse, rsq, mae, rpd, ccc)
      )
    },
    default_value = NULL,
    error_message = glue::glue("▶ evaluate_single_model_local: Failed to fit final model")
  )
  
  if (is.null(final_fit_result$result)) {
    actual_error <- final_fit_result$error$message %||% "Unknown error"
    
    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Final model fitting failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )
  }
  
  final_fit <- final_fit_result$result
  
  ## Step 11: Extract Metrics and Return Results ------------------------------
  
  # Extract test set metrics and pivot to wide format for efficiency
  test_metrics <- tune::collect_metrics(final_fit) %>%
    dplyr::select(.metric, .estimate) %>%
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate)
  
  # Create result row
  tibble::tibble(
    config_id         = config_id,
    workflow_id       = workflow_id,
    model             = config_clean$model,
    transformation    = config_clean$transformation,
    preprocessing     = config_clean$preprocessing,
    feature_selection = config_clean$feature_selection,
    covariates        = if (!is.null(config_clean$covariates)) {
                          paste(config_clean$covariates, collapse = "-")
                        } else {
                          NA_character_
                        },
    # Metrics (now accessing from wide format)
    rsq               = test_metrics$rsq %||% NA_real_,
    rmse              = test_metrics$rmse %||% NA_real_,
    rrmse             = test_metrics$rrmse %||% NA_real_,
    rpd               = test_metrics$rpd %||% NA_real_,
    ccc               = test_metrics$ccc %||% NA_real_,
    mae               = test_metrics$mae %||% NA_real_,
    # Timing
    grid_seconds      = grid_seconds,
    bayes_seconds     = bayes_seconds,
    total_seconds     = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    # Status
    status            = if (skip_bayesian) "pruned" else "success",
    error_message     = NA_character_
    # Note: fitted_workflow removed to prevent memory leak
    # Use fit_final_models() function separately for model stacking
  )
}