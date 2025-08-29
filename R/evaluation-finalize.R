#' Finalize Top Models for Ensemble Stacking
#'
#' @description
#' Takes evaluation results and refits the top-performing models with additional
#' Bayesian optimization iterations, then saves them with CV predictions for ensemble stacking.
#'
#' @param evaluation_results Tibble from evaluate_models_local() or evaluate_models_hpc() 
#'   containing model metrics and best_params
#' @param input_data Tibble containing spectral features and response variable
#' @param covariate_data Optional tibble containing external covariates
#' @param variable Character. Name of the response variable
#' @param n_best Integer. Number of top models to finalize (default: 10)
#' @param metric Character. Metric to use for selecting top models: "rrmse", "ccc", "rsq" (default: "rrmse")
#' @param output_dir Character. Directory to save finalized workflows (default: tempdir())
#' @param train_prop Numeric. Proportion of data for training (default: 0.8)
#' @param bayesian_iter Integer. Additional Bayesian optimization iterations (default: 15)
#' @param cv_folds Integer. Cross-validation folds for stacking predictions (default: 10)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#' @param parallel_cv Logical. Enable parallel processing for CV (default: FALSE)
#' @param verbose Logical. Print progress information (default: TRUE)
#'
#' @return Tibble with finalized model information and saved paths
#' @export
finalize_top_workflows <- function(evaluation_results,
                                  input_data,
                                  covariate_data = NULL,
                                  variable,
                                  n_best = 10,
                                  metric = "rrmse",
                                  output_dir = NULL,
                                  train_prop = 0.8,
                                  bayesian_iter = 15,
                                  cv_folds = 10,
                                  seed = 123,
                                  parallel_cv = FALSE,
                                  verbose = TRUE) {
  
  ## Step 0: Input Validation --------------------------------------------------
  
  # Check evaluation_results
  if (!is.data.frame(evaluation_results)) {
    cli::cli_abort("▶ finalize_top_workflows: evaluation_results must be a data frame")
  }
  
  if (nrow(evaluation_results) == 0) {
    cli::cli_abort("▶ finalize_top_workflows: evaluation_results is empty")
  }
  
  required_cols <- c("config_id", "workflow_id", "model", "transformation", 
                     "preprocessing", "feature_selection", "best_params", 
                     "rrmse", "ccc", "rsq", "status")
  
  missing_cols <- setdiff(required_cols, names(evaluation_results))
  if (length(missing_cols) > 0) {
    cli::cli_abort("▶ finalize_top_workflows: Missing required columns: {missing_cols}")
  }
  
  # Check input_data
  if (!is.data.frame(input_data)) {
    cli::cli_abort("▶ finalize_top_workflows: input_data must be a data frame")
  }
  
  if (!variable %in% names(input_data)) {
    cli::cli_abort("▶ finalize_top_workflows: Variable '{variable}' not found in input_data")
  }
  
  # Check metric
  valid_metrics <- c("rrmse", "ccc", "rsq", "rmse", "mae")
  if (!metric %in% valid_metrics) {
    cli::cli_abort("▶ finalize_top_workflows: metric must be one of: {valid_metrics}")
  }
  
  # Check numeric parameters
  if (n_best < 1) {
    cli::cli_abort("▶ finalize_top_workflows: n_best must be at least 1")
  }
  
  if (bayesian_iter < 0) {
    cli::cli_abort("▶ finalize_top_workflows: bayesian_iter must be non-negative")
  }
  
  if (cv_folds < 2) {
    cli::cli_abort("▶ finalize_top_workflows: cv_folds must be at least 2")
  }
  
  if (train_prop <= 0 || train_prop >= 1) {
    cli::cli_abort("▶ finalize_top_workflows: train_prop must be between 0 and 1")
  }
  
  # Setup output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), paste0("finalized_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Setup parallel backend for CV if requested
  cluster_object <- NULL  # Track cluster for cleanup
  
  if (parallel_cv) {
    if (.Platform$OS.type == "unix") {
      # Unix/macOS: Use doMC
      if (requireNamespace("doMC", quietly = TRUE)) {
        n_cores <- min(cv_folds, parallel::detectCores() - 1)
        doMC::registerDoMC(cores = n_cores)
        # Register cleanup for Unix/macOS
        on.exit({
          foreach::registerDoSEQ()  # Reset to sequential
        }, add = TRUE)
        if (verbose) {
          cli::cli_alert_success("Parallel CV enabled with {n_cores} cores (doMC)")
        }
      } else {
        cli::cli_alert_warning("doMC not available, falling back to sequential CV")
        parallel_cv <- FALSE
      }
    } else {
      # Windows: Use doParallel
      if (requireNamespace("doParallel", quietly = TRUE)) {
        n_cores <- min(cv_folds, parallel::detectCores() - 1)
        tryCatch({
          cluster_object <- parallel::makeCluster(n_cores)
          doParallel::registerDoParallel(cluster_object)
          # Register cleanup for Windows
          on.exit({
            if (!is.null(cluster_object)) {
              try(parallel::stopCluster(cluster_object), silent = TRUE)
            }
            foreach::registerDoSEQ()  # Reset to sequential
          }, add = TRUE)
          if (verbose) {
            cli::cli_alert_success("Parallel CV enabled with {n_cores} cores (doParallel)")
          }
        }, error = function(e) {
          cli::cli_alert_warning("Failed to create parallel cluster: {conditionMessage(e)}")
          cli::cli_alert_warning("Falling back to sequential CV")
          parallel_cv <- FALSE
          if (!is.null(cluster_object)) {
            try(parallel::stopCluster(cluster_object), silent = TRUE)
          }
        })
      } else {
        cli::cli_alert_warning("doParallel not available, falling back to sequential CV")
        parallel_cv <- FALSE
      }
    }
  }
  
  # Start timing
  start_time <- Sys.time()
  
  if (verbose) {
    cli::cli_h1("Finalizing Top Models for Ensemble Stacking")
    cli::cli_alert_info("Evaluation results: {nrow(evaluation_results)} models")
    cli::cli_alert_info("Selection metric: {metric}")
    cli::cli_alert_info("Target models: {n_best}")
    cli::cli_alert_info("Additional Bayes iterations: {bayesian_iter}")
    cli::cli_alert_info("CV folds for stacking: {cv_folds}")
    cli::cli_alert_info("Output directory: {output_dir}")
  }
  
  ## Step 1: Select Top Models -------------------------------------------------
  
  # Filter for successful models with valid metrics
  successful_models <- evaluation_results %>%
    dplyr::filter(
      status == "success",                 # Only fully successful models
      !is.na(!!sym(metric)),              # Metric must exist
      !is.null(best_params),              # Must have parameters
      lengths(best_params) > 0            # Parameters can't be empty
    )
  
  if (nrow(successful_models) == 0) {
    cli::cli_abort("▶ finalize_top_workflows: No successful models with valid {metric} found")
  }
  
  # Determine sort direction based on metric
  lower_is_better <- metric %in% c("rrmse", "rmse", "mae")
  
  # Select top N models
  top_models <- if (lower_is_better) {
    successful_models %>%
      dplyr::arrange(!!sym(metric)) %>%
      dplyr::slice_head(n = n_best)
  } else {
    successful_models %>%
      dplyr::arrange(dplyr::desc(!!sym(metric))) %>%
      dplyr::slice_head(n = n_best)
  }
  
  # Warn if fewer models available than requested
  if (nrow(top_models) < n_best) {
    cli::cli_alert_warning("Only {nrow(top_models)} models available (requested {n_best})")
  }
  
  if (verbose) {
    cli::cli_h2("Selected Top Models")
    cli::cli_alert_success("Selected {nrow(top_models)} models by {metric}")
    
    # Show metric range
    best_metric <- if (lower_is_better) min(top_models[[metric]]) else max(top_models[[metric]])
    worst_metric <- if (lower_is_better) max(top_models[[metric]]) else min(top_models[[metric]])
    
    cli::cli_alert_info("Best {metric}: {round(best_metric, 3)}")
    cli::cli_alert_info("Worst selected {metric}: {round(worst_metric, 3)}")
  }
  
  ## Step 2: Data Preparation --------------------------------------------------
  
  # Rename response variable for consistency with horizons patterns
  input_data_clean <- input_data %>%
    dplyr::rename(Response = !!rlang::sym(variable))
  
  # Create train/test split with stratification fallback
  set.seed(seed)
  
  split_result <- safely_execute(
    expr = {
      rsample::initial_split(
        input_data_clean, 
        prop = train_prop,
        strata = Response
      )
    },
    default_value = NULL,
    error_message = "▶ finalize_top_workflows: Stratified split failed, using random sampling"
  )
  
  # If stratification failed, fallback to random
  if (is.null(split_result$result)) {
    data_split <- rsample::initial_split(
      input_data_clean,
      prop = train_prop
    )
    if (verbose) {
      cli::cli_alert_warning("Using random train/test split (stratification failed)")
    }
  } else {
    data_split <- split_result$result
  }
  
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)
  
  # Create CV folds with stratification fallback
  set.seed(seed + 1)  # Different seed for CV
  
  cv_result <- safely_execute(
    expr = {
      rsample::vfold_cv(
        train_data,
        v = cv_folds,
        strata = Response
      )
    },
    default_value = NULL,
    error_message = "▶ finalize_top_workflows: Stratified CV failed, using random folds"
  )
  
  # If stratification failed, fallback to random
  if (is.null(cv_result$result)) {
    cv_resamples <- rsample::vfold_cv(
      train_data,
      v = cv_folds
    )
    if (verbose) {
      cli::cli_alert_warning("Using random CV folds (stratification failed)")
    }
  } else {
    cv_resamples <- cv_result$result
  }
  
  if (verbose) {
    cli::cli_h2("Data Preparation")
    cli::cli_alert_success("Train: {nrow(train_data)} samples | Test: {nrow(test_data)} samples")
    cli::cli_alert_success("Created {cv_folds} fold cross-validation for stacking")
    
    # Show response variable statistics
    response_range <- range(train_data$Response, na.rm = TRUE)
    response_sd <- sd(train_data$Response, na.rm = TRUE)
    cli::cli_alert_info("Response range: [{round(response_range[1], 2)}, {round(response_range[2], 2)}]")
    cli::cli_alert_info("Response SD: {round(response_sd, 2)}")
  }
  
  # Clean up
  rm(input_data_clean)
  gc(verbose = FALSE, full = TRUE)
  
  ## Step 3: Process Each Model ------------------------------------------------
  
  # Preprocess covariates from concatenated strings back to list column
  top_models <- top_models %>%
    dplyr::mutate(
      covariate_list = purrr::map(covariates, function(x) {
        if (is.na(x) || x == "NA") {
          NULL
        } else {
          strsplit(x, "-")[[1]]
        }
      })
    )
  
  # Initialize results storage
  finalized_results <- vector("list", nrow(top_models))
  
  # Create progress bar
  if (verbose) {
    cli::cli_progress_bar(
      "Finalizing models",
      total = nrow(top_models),
      clear = FALSE
    )
  }
  
  # Process each model sequentially
  for (i in seq_len(nrow(top_models))) {
    
    model_start_time <- Sys.time()
    current_model <- top_models[i, ]
    
    if (verbose) {
      cli::cli_progress_update()
      cli::cli_h3("Model {i}/{nrow(top_models)}: {current_model$workflow_id}")
      cli::cli_alert_info("Type: {current_model$model} | Preprocessing: {current_model$preprocessing}")
      cli::cli_alert_info("Current {metric}: {round(current_model[[metric]], 3)}")
    }
    
    ## Step 3.1: Rebuild Recipe ------------------------------------------------
    
    recipe_result <- safely_execute(
      expr = {
        build_recipe(
          input_data               = train_data,
          response_transformation  = current_model$transformation,
          spectral_transformation  = current_model$preprocessing,
          feature_selection_method = current_model$feature_selection,
          covariate_selection      = current_model$covariate_list[[1]],
          covariate_data          = covariate_data
        )
      },
      default_value = NULL,
      error_message = glue::glue("▶ finalize_top_workflows: Recipe building failed for {current_model$workflow_id}")
    )
    
    if (is.null(recipe_result$result)) {
      cli::cli_alert_danger("Failed to build recipe for model {i}, skipping")
      finalized_results[[i]] <- list(
        workflow_id = current_model$workflow_id,
        status = "failed", 
        reason = "recipe_building"
      )
      next
    }
    
    recipe <- recipe_result$result
    
    ## Step 3.2: Rebuild Model Specification -----------------------------------
    
    model_spec_result <- safely_execute(
      expr = {
        define_model_specifications(current_model$model)
      },
      default_value = NULL,
      error_message = glue::glue("▶ finalize_top_workflows: Model spec failed for {current_model$model}")
    )
    
    if (is.null(model_spec_result$result)) {
      cli::cli_alert_danger("Failed to create model spec for model {i}, skipping")
      finalized_results[[i]] <- list(
        workflow_id = current_model$workflow_id,
        status = "failed", 
        reason = "model_spec"
      )
      next
    }
    
    model_spec <- model_spec_result$result
    
    ## Step 3.3: Create Workflow -----------------------------------------------
    
    workflow_result <- safely_execute(
      expr = {
        workflows::workflow() %>%
          workflows::add_recipe(recipe) %>%
          workflows::add_model(model_spec)
      },
      default_value = NULL,
      error_message = glue::glue("▶ finalize_top_workflows: Workflow creation failed for model {i}")
    )
    
    if (is.null(workflow_result$result)) {
      cli::cli_alert_danger("Failed to create workflow for model {i}, skipping")
      finalized_results[[i]] <- list(
        workflow_id = current_model$workflow_id,
        status = "failed", 
        reason = "workflow_creation"
      )
      next
    }
    
    workflow <- workflow_result$result
    
    if (verbose) {
      cli::cli_alert_success("Workflow rebuilt successfully")
    }
    
    ## Step 3.4: Extract and prepare parameters -------------------------------
    
    # Validate best_params structure
    if (is.null(current_model$best_params[[1]]) || 
        !is.data.frame(current_model$best_params[[1]]) || 
        nrow(current_model$best_params[[1]]) == 0) {
      cli::cli_alert_danger("Invalid parameter structure for model {i}, skipping")
      finalized_results[[i]] <- list(
        workflow_id = current_model$workflow_id,
        status = "failed", 
        reason = "invalid_parameters"
      )
      next
    }
    
    # Extract parameter set and finalize if needed (for mtry in random forest)
    param_set <- hardhat::extract_parameter_set_dials(workflow)
    
    if ("mtry" %in% param_set$name) {
      # Need to finalize mtry based on number of predictors
      finalize_data <- recipe %>%
        recipes::prep() %>%
        recipes::bake(new_data = NULL) %>%
        dplyr::select(-Response)
      
      param_set <- param_set %>%
        dials::finalize(finalize_data)
      
      rm(finalize_data)
      gc(verbose = FALSE, full = TRUE)
    }
    
    ## Step 3.5: Bayesian optimization with warm start ------------------------
    
    # Extract best parameters from evaluation
    best_params <- current_model$best_params[[1]]
    
    if (verbose) {
      cli::cli_alert_info("Running Bayesian optimization ({bayesian_iter} iterations)")
    }
    
    # Run Bayesian optimization starting from best params
    bayes_result <- safely_execute(
      expr = {
        tune::tune_bayes(
          object = workflow,
          resamples = cv_resamples,
          initial = best_params,  # Use saved params as starting point
          iter = bayesian_iter,
          metrics = yardstick::metric_set(rrmse, rsq, rmse, ccc, rpd, mae),
          param_info = param_set,
          control = tune::control_bayes(
            save_pred = FALSE,
            save_workflow = FALSE,
            verbose = FALSE,
            no_improve = 5L,
            allow_par = parallel_cv,
            seed = seed + i  # Different seed per model for diversity
          )
        )
      },
      default_value = NULL,
      error_message = glue::glue("▶ finalize_top_workflows: Bayesian optimization failed for model {i}")
    )
    
    if (is.null(bayes_result$result)) {
      cli::cli_alert_danger("Bayesian optimization failed for model {i}")
      cli::cli_alert_warning("Falling back to original parameters from evaluation")
      # Fallback: just use the original best params
      final_best_params <- best_params
    } else {
      # Get the best parameters from additional tuning
      bayes_tuning <- bayes_result$result
      final_best_params <- tune::select_best(bayes_tuning, metric = metric)
    }
    
    ## Step 3.6: Finalize workflow with best parameters -----------------------
    
    finalized_workflow <- workflow %>%
      tune::finalize_workflow(final_best_params)
    
    if (verbose) {
      cli::cli_alert_success("Workflow finalized with best parameters")
    }
    
    ## Step 3.7: Generate CV predictions for stacking -------------------------
    
    if (verbose) {
      cli::cli_alert_info("Generating CV predictions for stacking")
    }
    
    # Fit on CV folds to generate predictions for stacking
    cv_fit_result <- safely_execute(
      expr = {
        tune::fit_resamples(
          finalized_workflow,
          resamples = cv_resamples,
          metrics = yardstick::metric_set(rrmse, rsq, rmse, ccc, rpd, mae),
          control = tune::control_resamples(
            save_pred = TRUE,      # Critical for stacking!
            save_workflow = TRUE,  # Need this for ensemble
            allow_par = parallel_cv
          )
        )
      },
      default_value = NULL,
      error_message = glue::glue("▶ finalize_top_workflows: CV fitting failed for model {i}")
    )
    
    if (is.null(cv_fit_result$result)) {
      cli::cli_alert_danger("Failed to generate CV predictions for model {i}")
      finalized_results[[i]] <- list(
        workflow_id = current_model$workflow_id,
        status = "failed",
        reason = "cv_predictions"
      )
      next
    }
    
    cv_fit <- cv_fit_result$result
    
    ## Step 3.8: Store results for stacking -----------------------------------
    
    # Store successful result with CV predictions
    finalized_results[[i]] <- list(
      workflow_id = current_model$workflow_id,
      workflow = finalized_workflow,
      cv_results = cv_fit,  # Contains the CV predictions for stacking
      final_params = final_best_params,
      original_metric = current_model[[metric]],
      status = "success"
    )
    
    if (verbose) {
      cli::cli_alert_success("Model {i} finalized with CV predictions")
    }
    
    # Track timing
    model_duration <- as.numeric(difftime(Sys.time(), model_start_time, units = "secs"))
    
    if (verbose) {
      cli::cli_alert_info("Total processing time: {round(model_duration, 1)} seconds")
    }
    
    # Clean up memory after each model
    rm(recipe, model_spec, workflow, param_set, finalized_workflow)
    if (exists("finalize_data")) rm(finalize_data)
    if (exists("bayes_tuning")) rm(bayes_tuning)
    if (exists("cv_fit")) rm(cv_fit)
    gc(verbose = FALSE, full = TRUE)
  }
  
  # Close progress bar
  if (verbose) {
    cli::cli_progress_done()
  }
  
  ## Step 4: Save and Return Results ------------------------------------------
  
  # Filter successful results
  successful_results <- purrr::keep(finalized_results, ~ .x$status == "success")
  
  if (length(successful_results) == 0) {
    cli::cli_abort("▶ finalize_top_workflows: No models successfully finalized")
  }
  
  # Create summary tibble for return (in memory for immediate use)
  results_tibble <- tibble::tibble(
    workflow_id     = purrr::map_chr(successful_results, "workflow_id"),
    workflow        = purrr::map(successful_results, "workflow"),
    cv_results      = purrr::map(successful_results, "cv_results"),
    final_params    = purrr::map(successful_results, "final_params"),
    original_metric = purrr::map_dbl(successful_results, "original_metric")
  )
  
  # Also save to disk for persistence/sharing
  save_path <- file.path(output_dir, 
                        paste0("finalized_models_", 
                               format(Sys.time(), "%Y%m%d_%H%M%S"), 
                               ".qs"))
  
  safely_execute(
    expr = {
      qs::qsave(results_tibble, save_path)
    },
    default_value = NULL,
    error_message = "▶ finalize_top_workflows: Failed to save results to disk"
  )
  
  # Calculate final metrics from CV results
  final_metrics <- results_tibble %>%
    dplyr::mutate(
      cv_metrics = purrr::map(cv_results, ~ tune::collect_metrics(.x))
    ) %>%
    tidyr::unnest(cv_metrics) %>%
    dplyr::filter(.metric == metric) %>%
    dplyr::select(workflow_id, mean, std_err)
  
  # Final summary
  total_time <- difftime(Sys.time(), start_time, units = "mins")
  
  if (verbose) {
    cli::cli_h1("Finalization Complete")
    cli::cli_alert_success("{nrow(results_tibble)} models successfully finalized")
    cli::cli_alert_success("Total time: {round(total_time, 1)} minutes")
    cli::cli_alert_success("Average per model: {round(total_time/nrow(results_tibble), 1)} minutes")
    
    # Show final performance
    cli::cli_h2("Final Performance ({metric})")
    best_idx <- if (lower_is_better) which.min(final_metrics$mean) else which.max(final_metrics$mean)
    worst_idx <- if (lower_is_better) which.max(final_metrics$mean) else which.min(final_metrics$mean)
    
    cli::cli_alert_info("Best model: {final_metrics$workflow_id[best_idx]}")
    cli::cli_alert_info("  • {metric}: {round(final_metrics$mean[best_idx], 3)} ± {round(final_metrics$std_err[best_idx], 3)}")
    cli::cli_alert_info("Worst model: {final_metrics$workflow_id[worst_idx]}")  
    cli::cli_alert_info("  • {metric}: {round(final_metrics$mean[worst_idx], 3)} ± {round(final_metrics$std_err[worst_idx], 3)}")
    
    cli::cli_alert_success("Saved to: {save_path}")
    cli::cli_alert_info("Ready for ensemble stacking!")
  }
  
  # Final cleanup
  gc(verbose = FALSE, full = TRUE)
  
  return(results_tibble)
}