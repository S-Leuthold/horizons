#' Evaluate Model with Inner Worker Pool
#'
#' @description
#' Evaluates a single model configuration using a dedicated pool of inner workers
#' for parallel hyperparameter tuning. This function is called by outer workers
#' in the nested parallelization scheme.
#'
#' @param config_row Single row from configuration tibble
#' @param input_data Spectral data tibble
#' @param covariate_data Optional covariate data
#' @param variable Response variable name
#' @param row_index Index of this model in the full configuration
#' @param inner_workers Number of workers for tuning
#' @param output_dir Output directory path
#' @param grid_size Grid search size
#' @param bayesian_iter Bayesian optimization iterations
#' @param cv_folds Cross-validation folds
#' @param resource_manager Resource manager object
#'
#' @return List with evaluation results and metadata
#'
#' @keywords internal
evaluate_model_with_inner_workers <- function(config_row,
                                              input_data,
                                              covariate_data,
                                              variable,
                                              row_index,
                                              inner_workers,
                                              output_dir,
                                              grid_size,
                                              bayesian_iter,
                                              cv_folds,
                                              resource_manager) {
  
  model_start <- Sys.time()
  
  # Mark as inner worker context
  Sys.setenv(HORIZONS_PARALLEL_LEVEL = "inner")
  
  # Save current plan
  original_inner_plan <- future::plan()
  
  # Setup inner parallelization
  future::plan(
    future::multicore,
    workers = inner_workers
  )
  
  # Ensure cleanup on exit
  on.exit({
    future::plan(original_inner_plan)
    Sys.setenv(HORIZONS_PARALLEL_LEVEL = "")
  }, add = TRUE)
  
  # Configure thread control for inner context
  configure_inner_thread_control(inner_workers)
  
  # Acquire resource token
  resource_token <- acquire_resource_token(resource_manager, row_index)
  
  # Create workflow ID
  wflow_id <- create_workflow_id(config_row)
  
  # Initialize result structure
  result <- list(
    row_index = row_index,
    wflow_id = wflow_id,
    status = "started",
    start_time = model_start
  )
  
  tryCatch({
    
    # ---------------------------------------------------------------------------
    # Build Recipe and Workflow
    # ---------------------------------------------------------------------------
    
    cli::cli_alert_info("[Model {row_index}] Building recipe: {wflow_id}")
    
    # Extract covariate names
    covariate_selection <- if (!is.null(config_row$covariates)) {
      unlist(config_row$covariates)
    } else {
      NULL
    }
    
    # ---------------------------------------------------------------------------
    # Prepare Data Splits (following stable version pattern)
    # ---------------------------------------------------------------------------
    
    set.seed(row_index * 123)  # Reproducible splits
    
    # Rename response column to "Response" FIRST (like stable version)
    if (variable != "Response" && variable %in% names(input_data)) {
      input_data <- input_data %>%
        dplyr::rename(Response = !!sym(variable))
    }
    
    # Filter to valid observations (now using "Response")
    model_data <- input_data %>%
      dplyr::filter(!is.na(Response)) %>%
      dplyr::filter(is.finite(Response))
    
    # Create train/test split
    split <- rsample::initial_split(model_data, prop = 0.8, strata = Response)
    train_data <- rsample::training(split)
    test_data <- rsample::testing(split)
    
    # Create CV folds
    cv_folds_obj <- rsample::vfold_cv(train_data, v = cv_folds)
    
    cli::cli_alert_info("[Model {row_index}] Data: {nrow(train_data)} train, {nrow(test_data)} test")
    
    # ---------------------------------------------------------------------------
    # Build Recipe and Workflow
    # ---------------------------------------------------------------------------
    
    # Build recipe using training data (like stable version)
    recipe <- build_recipe(
      input_data = train_data,
      spectral_transformation = config_row$preprocessing,
      response_transformation = config_row$transformation,
      feature_selection_method = config_row$feature_selection,
      covariate_selection = covariate_selection,
      covariate_data = covariate_data
    )
    
    # Define model specification
    model_spec <- define_model_specifications(
      model_type = config_row$model
    )
    
    # Create workflow
    workflow <- workflows::workflow() %>%
      workflows::add_recipe(recipe) %>%
      workflows::add_model(model_spec)
    
    # ---------------------------------------------------------------------------
    # Grid Search with Inner Parallelization
    # ---------------------------------------------------------------------------
    
    cli::cli_alert_info("[Model {row_index}] Starting grid search ({grid_size} combinations)")
    
    # Configure parallel grid search
    grid_control <- tune::control_grid(
      save_pred = FALSE,
      save_workflow = FALSE,
      verbose = FALSE,
      allow_par = TRUE,
      parallel_over = "everything",
      pkgs = c("horizons", "tidymodels")
    )
    
    # Perform grid search
    grid_results <- tune::tune_grid(
      object = workflow,
      resamples = cv_folds_obj,
      grid = grid_size,
      metrics = yardstick::metric_set(rmse, rsq, mae),
      control = grid_control
    )
    
    # ---------------------------------------------------------------------------
    # Bayesian Optimization with Inner Parallelization
    # ---------------------------------------------------------------------------
    
    if (bayesian_iter > 0) {
      cli::cli_alert_info("[Model {row_index}] Starting Bayesian optimization ({bayesian_iter} iterations)")
      
      # Configure parallel Bayesian optimization
      bayes_control <- tune::control_bayes(
        save_pred = FALSE,
        save_workflow = FALSE,
        verbose = FALSE,
        no_improve = 10L,
        allow_par = TRUE,
        parallel_over = "everything",
        pkgs = c("horizons", "tidymodels")
      )
      
      # Perform Bayesian optimization
      bayes_results <- tune::tune_bayes(
        object = workflow,
        resamples = cv_folds_obj,
        initial = grid_results,
        iter = bayesian_iter,
        metrics = yardstick::metric_set(rmse, rsq, mae),
        control = bayes_control
      )
      
      final_results <- bayes_results
    } else {
      final_results <- grid_results
    }
    
    # ---------------------------------------------------------------------------
    # Finalize and Evaluate
    # ---------------------------------------------------------------------------
    
    cli::cli_alert_info("[Model {row_index}] Finalizing model")
    
    # Select best parameters
    best_params <- tune::select_best(final_results, metric = "rmse")
    
    # Finalize workflow
    final_workflow <- tune::finalize_workflow(workflow, best_params)
    
    # Fit on full training data
    final_fit <- parsnip::fit(final_workflow, data = train_data)
    
    # ---------------------------------------------------------------------------
    # Test Set Evaluation
    # ---------------------------------------------------------------------------
    
    # Make predictions
    test_pred <- predict(final_fit, test_data) %>%
      dplyr::bind_cols(test_data %>% dplyr::select(all_of(variable)))
    
    # Calculate metrics
    test_metrics <- yardstick::metrics(
      test_pred,
      truth = !!sym(variable),
      estimate = .pred
    )
    
    # Extract key metrics
    rmse_val <- test_metrics %>%
      dplyr::filter(.metric == "rmse") %>%
      dplyr::pull(.estimate)
    
    rsq_val <- test_metrics %>%
      dplyr::filter(.metric == "rsq") %>%
      dplyr::pull(.estimate)
    
    mae_val <- test_metrics %>%
      dplyr::filter(.metric == "mae") %>%
      dplyr::pull(.estimate)
    
    # ---------------------------------------------------------------------------
    # Compile Results
    # ---------------------------------------------------------------------------
    
    result$metrics <- list(
      rmse = rmse_val,
      rsq = rsq_val,
      mae = mae_val
    )
    result$best_params <- best_params
    result$cv_metrics <- tune::collect_metrics(final_results)
    result$n_train <- nrow(train_data)
    result$n_test <- nrow(test_data)
    result$timing <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))
    result$memory_peak_gb <- get_peak_memory(resource_token)
    result$status <- "success"
    
    cli::cli_alert_success(
      "[Model {row_index}] Complete: RMSE={round(rmse_val, 3)}, R²={round(rsq_val, 3)}"
    )
    
  }, error = function(e) {
    
    cli::cli_alert_danger("[Model {row_index}] Error: {e$message}")
    
    result$error <- conditionMessage(e)
    result$error_call <- as.character(e$call)
    result$timing <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))
    result$status <- "error"
    
  })
  
  # Release resources
  release_resource_token(resource_manager, resource_token)
  
  # Save individual model results
  if (!is.null(output_dir)) {
    model_file <- file.path(
      output_dir,
      "models",
      sprintf("model_%06d_%s.qs", row_index, wflow_id)
    )
    fs::dir_create(dirname(model_file))
    qs::qsave(result, model_file)
  }
  
  return(result)
}


#' Configure Thread Control for Inner Workers
#'
#' @description
#' Sets appropriate thread control for inner worker processes to prevent
#' oversubscription when using nested parallelization.
#'
#' @param inner_workers Number of inner workers
#'
#' @keywords internal
configure_inner_thread_control <- function(inner_workers) {
  
  # For inner workers, we want single-threaded operations
  # because parallelization happens at the CV/tuning level
  threads_per_operation <- 1
  
  # Set comprehensive thread controls
  Sys.setenv(
    OMP_NUM_THREADS = as.character(threads_per_operation),
    OPENBLAS_NUM_THREADS = as.character(threads_per_operation),
    MKL_NUM_THREADS = as.character(threads_per_operation),
    VECLIB_MAXIMUM_THREADS = as.character(threads_per_operation),
    NUMEXPR_NUM_THREADS = as.character(threads_per_operation),
    GOTO_NUM_THREADS = as.character(threads_per_operation),
    BLIS_NUM_THREADS = as.character(threads_per_operation)
  )
  
  # Set package-specific options
  options(
    ranger.num.threads = threads_per_operation,
    ranger.num.cores = threads_per_operation,
    xgboost.nthread = threads_per_operation,
    mc.cores = 1  # Prevent nested forking
  )
  
  # Use RhpcBLASctl if available
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    suppressMessages({
      RhpcBLASctl::blas_set_num_threads(threads_per_operation)
      RhpcBLASctl::omp_set_num_threads(threads_per_operation)
    })
  }
  
  invisible(NULL)
}


#' Create Workflow ID
#'
#' @description
#' Creates a unique identifier string for a model configuration.
#'
#' @param config_row Single row from configuration tibble
#'
#' @return Character string workflow ID
#'
#' @keywords internal
create_workflow_id <- function(config_row) {
  
  # Extract components
  model <- config_row$model %||% "unknown"
  trans <- config_row$transformation %||% "none"
  preproc <- config_row$preprocessing %||% "raw"
  feat_sel <- config_row$feature_selection %||% "none"
  
  # Shorten names for compact ID
  model_short <- substr(model, 1, 3)
  trans_short <- ifelse(trans == "No Transformation", "raw", substr(trans, 1, 3))
  preproc_short <- substr(preproc, 1, 3)
  feat_short <- substr(feat_sel, 1, 3)
  
  # Include covariate indicator
  has_covs <- !is.null(config_row$covariates) && length(unlist(config_row$covariates)) > 0
  cov_flag <- ifelse(has_covs, "cov", "nocov")
  
  # Combine into ID
  wflow_id <- paste(
    model_short,
    trans_short,
    preproc_short,
    feat_short,
    cov_flag,
    sep = "_"
  )
  
  return(wflow_id)
}


# Utility operator
`%||%` <- function(x, y) if (is.null(x)) y else x