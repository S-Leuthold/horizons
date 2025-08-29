#' Local Model Evaluation - Main Entry Point
#'
#' @description
#' Executes model evaluation workflows on local machines with sequential model processing
#' and optional parallel cross-validation. This is the main entry point for desktop/server
#' execution outside of HPC environments.
#'
#' @param config A tibble of model configurations to evaluate
#' @param input_data Tibble with preprocessed spectral data
#' @param covariate_data Optional tibble of predicted covariates
#' @param variable Character. Name of the response variable
#' @param output_dir Path to output directory (auto-generated if NULL)
#' @param grid_size Integer. Grid search combinations (default: 10)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 15)
#' @param cv_folds Integer. Cross-validation folds (default: 10)
#' @param parallel_cv Logical. Use parallel processing for CV (default: TRUE)
#' @param n_cv_cores Integer. Cores for parallel CV (default: detectCores()-2)
#' @param prune_models Logical. Prune models that don't beat baseline (default: TRUE)
#' @param prune_threshold Numeric. Performance threshold vs baseline (default: 0.9)
#' @param seed Integer. Random seed for reproducibility (default: 307)
#' @param resume Logical. Resume from existing checkpoint files (default: TRUE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Tibble with evaluation results for all configurations
#'
#' @export
evaluate_models_local <- function(config,
                                  input_data,
                                  covariate_data = NULL,
                                  variable,
                                  output_dir     = NULL,
                                  grid_size      = 10,
                                  bayesian_iter  = 15,
                                  cv_folds       = 10,
                                  parallel_cv    = TRUE,
                                  n_cv_cores     = NULL,
                                  prune_models   = TRUE,
                                  prune_threshold = 0.9,
                                  seed           = 307,
                                  resume         = TRUE,
                                  verbose        = TRUE) {
  
  ## Step 1: Input Validation ---------------------------------------------------
  
  if (!is.data.frame(config)) {
    cli::cli_abort("config must be a data frame")
  }
  
  if (!variable %in% names(input_data)) {
    cli::cli_abort("Response variable '{variable}' not found in input_data")
  }
  
  # Validate response variable has sufficient data
  response_vals <- input_data[[variable]]
  n_valid <- sum(!is.na(response_vals))
  
  if (n_valid < 20) {
    cli::cli_abort("Response variable has only {n_valid} non-missing values (minimum 20 required)")
  }
  
  if (var(response_vals, na.rm = TRUE) < .Machine$double.eps) {
    cli::cli_abort("Response variable has no variation (all values are the same)")
  }
  
  # Check required config columns
  required_cols <- c("model", "preprocessing", "transformation", "feature_selection")
  missing_cols <- setdiff(required_cols, names(config))
  if (length(missing_cols) > 0) {
    cli::cli_abort("config missing required columns: {missing_cols}")
  }
  
  # Check for spectral columns (wavenumber columns)
  spectral_cols <- grep("^[0-9]{3,4}(\\.[0-9]+)?$", names(input_data), value = TRUE)
  if (length(spectral_cols) == 0) {
    cli::cli_abort("No spectral columns found in input_data (expected wavenumber column names)")
  }
  
  # Validate covariate alignment if provided
  if (!is.null(covariate_data)) {
    if (!"Sample_ID" %in% names(input_data) || !"Sample_ID" %in% names(covariate_data)) {
      cli::cli_abort("Both input_data and covariate_data must have 'Sample_ID' column")
    }
    
    # Check for sample alignment
    missing_samples <- setdiff(input_data$Sample_ID, covariate_data$Sample_ID)
    if (length(missing_samples) > 0) {
      cli::cli_alert_warning("Warning: {length(missing_samples)} samples in input_data missing from covariate_data")
    }
  }
  
  n_models <- nrow(config)
  
  ## Step 2: Setup Output Directory ---------------------------------------------
  
  if (is.null(output_dir)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_dir <- fs::path(variable, paste0("local_eval_", timestamp))
  }
  
  # Create directory structure
  fs::dir_create(output_dir, recurse = TRUE)
  fs::dir_create(fs::path(output_dir, "results"))
  fs::dir_create(fs::path(output_dir, "errors"))
  fs::dir_create(fs::path(output_dir, "summary"))
  
  if (verbose) {
    cli::cli_h1("Local Model Evaluation")
    cli::cli_alert_info("Evaluating {n_models} model configuration{?s}")
    cli::cli_alert_info("Output directory: {.path {output_dir}}")
    cli::cli_alert_info("Strategy: Sequential models, {ifelse(parallel_cv, 'parallel', 'sequential')} CV")
    if (parallel_cv) {
      actual_cores <- if(is.null(n_cv_cores)) parallel::detectCores() - 2 else n_cv_cores
      cli::cli_alert_info("Using {actual_cores} cores for cross-validation")
    }
  }
  
  ## Step 3: Create Data Split -------------------------------------------------
  
  # Create train/test split for consistent evaluation across all models
  # Using 80/20 split with stratification on response variable
  set.seed(seed)  # For reproducibility
  
  data_split <- rsample::initial_split(
    data = input_data,
    prop = 0.8,
    strata = dplyr::all_of(variable)  # Stratify on response to maintain distribution
  )
  
  if (verbose) {
    train_n <- nrow(rsample::training(data_split))
    test_n <- nrow(rsample::testing(data_split))
    cli::cli_alert_success("Data split created: {train_n} training, {test_n} testing samples")
  }
  
  ## Step 4: Initialize Progress Tracking ---------------------------------------
  
  # Initialize results collector
  all_results <- list()
  
  # Timing tracker
  start_time <- Sys.time()
  model_times <- numeric(n_models)
  
  # Check for existing results if resume enabled
  existing_results <- character()
  if (resume) {
    result_files <- fs::dir_ls(
      fs::path(output_dir, "results"),
      glob = "*.qs",
      type = "file"
    )
    if (length(result_files) > 0) {
      existing_results <- fs::path_file(result_files)
      existing_results <- gsub("\\.qs$", "", existing_results)
      
      if (verbose && length(existing_results) > 0) {
        cli::cli_alert_info("Found {length(existing_results)} existing result{?s}, will skip")
      }
    }
  }
  
  ## Step 5: Model Iteration Loop -----------------------------------------------
  
  if (verbose) {
    cli::cli_h2("Model Evaluation Progress")
  }
  
  # Initialize best model tracking (using CCC as primary metric)
  best_ccc <- NA_real_
  best_model <- NA_character_
  
  for (i in seq_len(n_models)) {
    
    ## Part 1: Loop Setup and Config ID -----------------------------------------
    
    config_row <- config[i, , drop = FALSE]
    model_start <- Sys.time()
    
    # Extract covariates if specified (for workflow ID)
    covariate_cols <- if ("covariates" %in% names(config_row) && !is.null(config_row$covariates[[1]])) {
      config_row$covariates[[1]]  # Extract vector from list column
    } else {
      NULL
    }
    
    # Create standardized workflow ID using existing function
    workflow_id <- clean_workflow_id(
      model             = config_row$model,
      transformation    = config_row$transformation,
      preprocessing     = config_row$preprocessing,
      feature_selection = config_row$feature_selection,
      covariates        = covariate_cols
    )
    
    # Add index for uniqueness (in case of duplicate configs)
    config_id <- sprintf("%03d_%s", i, workflow_id)
    
    ## Part 2: Checkpointing Logic ----------------------------------------------
    
    # Define result file path
    result_file <- fs::path(output_dir, "results", paste0(config_id, ".qs"))
    
    # Check if already completed (resume enabled and file exists)
    if (resume && fs::file_exists(result_file)) {
      
      if (verbose) {
        cli::cli_alert_info("[{i}/{n_models}] Skipping {workflow_id} (already completed)")
      }
      
      # Try to load existing result with error handling
      existing_result <- tryCatch({
        qs::qread(result_file)
      }, error = function(e) {
        cli::cli_alert_warning("Could not load existing result file {config_id}.qs: {conditionMessage(e)}")
        cli::cli_alert_info("Will re-run this configuration")
        NULL
      })
      
      # If successfully loaded, store and continue to next iteration
      if (!is.null(existing_result)) {
        all_results[[i]] <- existing_result
        
        # Update timing from saved result
        if (!is.null(existing_result$timing$elapsed)) {
          model_times[i] <- existing_result$timing$elapsed / 60  # Convert to minutes
        }
        
        next  # Skip to next iteration
      }
    }
    
    # If we get here, we need to run the model
    if (verbose) {
      cli::cli_alert_info("[{i}/{n_models}] Evaluating: {workflow_id}")
    }
    
    ## Part 3: Covariate Data Preparation ---------------------------------------
    
    # Prepare covariate data subset if needed
    model_covariate_data <- NULL
    
    if (!is.null(covariate_cols) && !is.null(covariate_data)) {
      # Check that requested covariates exist
      missing_covs <- setdiff(covariate_cols, names(covariate_data))
      
      if (length(missing_covs) > 0) {
        cli::cli_alert_warning("Missing covariates for config {i}: {missing_covs}")
        cli::cli_alert_info("Available covariates: {names(covariate_data)}")
        # Continue without covariates rather than failing
        model_covariate_data <- NULL
      } else {
        # Subset to just the needed covariates plus Sample_ID
        model_covariate_data <- covariate_data[, c("Sample_ID", covariate_cols), drop = FALSE]
        
        if (verbose) {
          cli::cli_alert_info("Using {length(covariate_cols)} covariate{?s}: {covariate_cols}")
        }
      }
    }
    
    ## Part 4: Call evaluate_single_model_local ---------------------------------
    
    result_safe <- safely_execute(
      expr = {
        evaluate_configuration(
          config_row      = config_row,
          input_data      = input_data,
          data_split      = data_split,
          config_id       = config_id,
          covariate_data  = model_covariate_data,
          variable        = variable,
          output_dir      = output_dir,  # Pass through for any model artifacts
          grid_size       = grid_size,
          bayesian_iter   = bayesian_iter,
          cv_folds        = cv_folds,
          parallel_cv     = parallel_cv,
          n_cv_cores      = n_cv_cores,
          prune_models    = prune_models,
          prune_threshold = prune_threshold,
          seed            = seed
        )
      },
      default_value = NULL,
      error_message = "Failed to evaluate config {config_id}: {workflow_id}",
      log_error = TRUE
    )
    
    ## Part 5: Process Result and Save Checkpoint -------------------------------
    
    # Extract result (should be a tibble with metrics and metadata)
    if (!is.null(result_safe$result)) {
      result <- result_safe$result
      
      # Convert tibble result to list format expected by aggregation
      result <- list(
        config_id = result$config_id,
        workflow_id = result$workflow_id,
        config = list(
          model = result$model,
          preprocessing = result$preprocessing,
          transformation = result$transformation,
          feature_selection = result$feature_selection,
          covariates = if(!is.na(result$covariates)) strsplit(result$covariates, "-")[[1]] else NULL
        ),
        metrics = tibble::tibble(
          .metric = c("rmse", "rsq", "ccc", "rpd", "rrmse", "mae"),
          .estimate = c(result$rmse, result$rsq, result$ccc, result$rpd, result$rrmse, result$mae)
        ),
        timing = list(
          elapsed = result$total_seconds,
          timestamp = Sys.time()
        ),
        status = result$status
      )
      
    } else {
      # Create error result structure
      error_msg <- if (!is.null(result_safe$error)) {
        conditionMessage(result_safe$error)
      } else {
        "Unknown error occurred"
      }
      
      # Save error details to separate error file using atomic write
      error_file <- fs::path(output_dir, "errors", paste0("error_", config_id, ".json"))
      error_obj <- list(
        config_id = config_id,
        workflow_id = workflow_id,
        config = as.list(config_row),
        error_message = error_msg,
        timestamp = as.character(Sys.time())
      )
      
      # Atomic write for error file
      temp_error_file <- tempfile(tmpdir = dirname(error_file), fileext = ".json.tmp")
      tryCatch({
        jsonlite::write_json(
          error_obj, 
          temp_error_file, 
          pretty = TRUE, 
          auto_unbox = TRUE
        )
        file.rename(temp_error_file, error_file)
      }, error = function(e) {
        if (file.exists(temp_error_file)) unlink(temp_error_file)
        # Don't warn here since we're already in error handling
      })
      
      # Create standardized error result
      result <- list(
        config_id = config_id,
        workflow_id = workflow_id,
        config = as.list(config_row),
        metrics = tibble::tibble(
          .metric = c("rmse", "rsq", "ccc", "rpd", "rrmse"),
          .estimate = rep(NA_real_, 5)
        ),
        timing = list(
          elapsed = as.numeric(difftime(Sys.time(), model_start, units = "secs")),
          timestamp = Sys.time()
        ),
        status = "error",
        error_message = error_msg
      )
    }
    
    # Store result in memory
    all_results[[i]] <- result
    
    # Save result to disk using atomic write pattern
    result_file <- fs::path(output_dir, "results", paste0(config_id, ".qs"))
    temp_file <- tempfile(tmpdir = dirname(result_file), fileext = ".qs.tmp")
    
    tryCatch({
      qs::qsave(result, temp_file)
      file.rename(temp_file, result_file)  # Atomic operation
    }, error = function(e) {
      # Clean up temp file if rename failed
      if (file.exists(temp_file)) unlink(temp_file)
      cli::cli_alert_warning("Failed to save checkpoint for {config_id}: {e$message}")
    })
    
    # Track timing
    model_times[i] <- as.numeric(difftime(Sys.time(), model_start, units = "mins"))
    
    ## Part 6: Progress Reporting and Memory Cleanup ----------------------------
    
    # Track best model so far using CCC (for progress reporting)
    if (result$status %in% c("completed", "success")) {
      current_ccc <- result$metrics$.estimate[result$metrics$.metric == "ccc"][1]
      if (!is.na(current_ccc)) {
        if (is.na(best_ccc) || current_ccc > best_ccc) {
          best_ccc <- current_ccc
          best_model <- workflow_id
        }
      }
    }
    
    # Progress reporting (every model + ETA every 10 or first 5)
    if (verbose) {
      # Extract metrics for display
      if (result$status == "completed" || result$status == "success") {
        metrics_df <- result$metrics
        rrmse_val <- metrics_df$.estimate[metrics_df$.metric == "rrmse"][1]
        rsq_val <- metrics_df$.estimate[metrics_df$.metric == "rsq"][1]
        ccc_val <- metrics_df$.estimate[metrics_df$.metric == "ccc"][1]
        rpd_val <- metrics_df$.estimate[metrics_df$.metric == "rpd"][1]
        
        # Success message with model details
        cli::cli_alert_success("[{i}/{n_models}] {workflow_id}")
        cli::cli_text("  └─ RRMSE: {round(rrmse_val, 1)}%, R²: {round(rsq_val, 3)}, CCC: {round(ccc_val, 3)}, RPD: {round(rpd_val, 2)} | Time: {round(model_times[i], 2)} min")
        
      } else if (result$status == "pruned") {
        # Get pruning reason if available
        metrics_df <- result$metrics
        rrmse_val <- metrics_df$.estimate[metrics_df$.metric == "rrmse"][1]
        
        cli::cli_alert_warning("[{i}/{n_models}] {workflow_id}")
        cli::cli_text("  └─ Pruned: RRMSE {round(rrmse_val, 1)}% > {prune_threshold*100}% threshold | Time: {round(model_times[i], 2)} min")
        
      } else {
        # Error message with reason
        error_msg <- result$error_message %||% "Unknown error"
        error_preview <- if(nchar(error_msg) > 50) {
          paste0(substr(error_msg, 1, 50), "...")
        } else {
          error_msg
        }
        
        cli::cli_alert_danger("[{i}/{n_models}] {workflow_id}")
        cli::cli_text("  └─ Error: {error_preview} | Time: {round(model_times[i], 2)} min")
      }
      
      # Progress summary (every 10 models or first 5)
      if (i %% 10 == 0 || i <= 5) {
        cli::cli_rule(left = "Progress Summary", right = "{i}/{n_models}")
        
        # Count statuses
        statuses <- sapply(all_results[1:i], function(x) x$status %||% "unknown")
        n_success_so_far <- sum(statuses %in% c("completed", "success"))
        n_pruned_so_far <- sum(statuses == "pruned")
        n_failed_so_far <- sum(statuses %in% c("error", "failed"))
        
        # Status summary
        cli::cli_alert_info("Status: ✔ {n_success_so_far} completed, ⚠ {n_pruned_so_far} pruned, ✖ {n_failed_so_far} failed")
        
        # Best model so far
        if (!is.na(best_ccc)) {
          cli::cli_alert_success("Best CCC so far: {round(best_ccc, 3)} ({best_model})")
        }
        
        # ETA calculation
        completed_times <- model_times[1:i][!is.na(model_times[1:i]) & model_times[1:i] > 0]
        if (length(completed_times) > 0) {
          avg_time <- mean(completed_times)
          remaining <- n_models - i
          eta_mins <- remaining * avg_time
          eta_time <- Sys.time() + (eta_mins * 60)
          
          cli::cli_alert_info("Average: {round(avg_time, 2)} min/model | ETA: {format(eta_time, '%H:%M')} ({round(eta_mins, 0)} min remaining)")
        }
        
        cli::cli_rule()
      }
    }
    
    # Memory cleanup after each model
    invisible(gc(verbose = FALSE, full = TRUE))
  }
  
  ## Step 6: Result Aggregation -------------------------------------------------
  
  if (verbose) {
    cli::cli_h2("Aggregating Results")
  }
  
  # Convert list of results to tibble
  results_tbl <- purrr::map_dfr(all_results, function(res) {
    
    # Extract metrics in wide format
    metrics_wide <- if (!is.null(res$metrics)) {
      res$metrics %>%
        tidyr::pivot_wider(
          names_from = .metric,
          values_from = .estimate,
          names_prefix = ""
        )
    } else {
      tibble::tibble(
        rmse = NA_real_,
        rsq = NA_real_,
        ccc = NA_real_,
        rpd = NA_real_,
        rrmse = NA_real_
      )
    }
    
    # Combine all information
    tibble::tibble(
      config_id = res$config_id,
      workflow_id = res$workflow_id,
      model = res$config$model,
      preprocessing = res$config$preprocessing,
      transformation = res$config$transformation,
      feature_selection = res$config$feature_selection,
      covariates = list(res$config$covariates),
      status = res$status %||% "unknown",
      error_message = res$error_message %||% NA_character_,
      elapsed_secs = res$timing$elapsed %||% NA_real_,
      timestamp = res$timing$timestamp %||% NA
    ) %>%
      dplyr::bind_cols(metrics_wide)
  })
  
  # Calculate summary statistics
  n_completed <- sum(results_tbl$status == "completed", na.rm = TRUE)
  n_pruned <- sum(results_tbl$status == "pruned", na.rm = TRUE)
  n_errors <- sum(results_tbl$status == "error", na.rm = TRUE)
  
  if (verbose) {
    cli::cli_alert_success("Completed: {n_completed} models")
    if (n_pruned > 0) {
      cli::cli_alert_warning("Pruned: {n_pruned} models (failed baseline)")
    }
    if (n_errors > 0) {
      cli::cli_alert_danger("Failed: {n_errors} models")
    }
  }
  
  # Rank models by performance (using RRMSE as primary metric)
  results_tbl <- results_tbl %>%
    dplyr::arrange(rrmse) %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  ## Step 7: Output Generation --------------------------------------------------
  
  if (verbose) {
    cli::cli_h2("Saving Outputs")
  }
  
  # Save final aggregated results using atomic writes
  summary_file_qs <- fs::path(output_dir, "summary", "final_results.qs")
  temp_qs <- tempfile(tmpdir = dirname(summary_file_qs), fileext = ".qs.tmp")
  tryCatch({
    qs::qsave(results_tbl, temp_qs)
    file.rename(temp_qs, summary_file_qs)
  }, error = function(e) {
    if (file.exists(temp_qs)) unlink(temp_qs)
    cli::cli_alert_warning("Failed to save QS summary: {e$message}")
  })
  
  # Also save as CSV for easy viewing (atomic)
  summary_file_csv <- fs::path(output_dir, "summary", "final_results.csv")
  temp_csv <- tempfile(tmpdir = dirname(summary_file_csv), fileext = ".csv.tmp")
  tryCatch({
    readr::write_csv(results_tbl, temp_csv)
    file.rename(temp_csv, summary_file_csv)
  }, error = function(e) {
    if (file.exists(temp_csv)) unlink(temp_csv)
    cli::cli_alert_warning("Failed to save CSV summary: {e$message}")
  })
  
  # Save metadata about the run
  metadata <- list(
    n_models = n_models,
    n_completed = n_completed,
    n_pruned = n_pruned,
    n_errors = n_errors,
    total_time_mins = as.numeric(difftime(Sys.time(), start_time, units = "mins")),
    avg_time_per_model = mean(model_times[model_times > 0], na.rm = TRUE),
    parameters = list(
      variable = variable,
      grid_size = grid_size,
      bayesian_iter = bayesian_iter,
      cv_folds = cv_folds,
      parallel_cv = parallel_cv,
      prune_models = prune_models,
      prune_threshold = prune_threshold,
      seed = seed
    ),
    timestamp = Sys.time()
  )
  
  # Save metadata using atomic write
  metadata_file <- fs::path(output_dir, "summary", "run_metadata.json")
  temp_meta <- tempfile(tmpdir = dirname(metadata_file), fileext = ".json.tmp")
  tryCatch({
    jsonlite::write_json(metadata, temp_meta, pretty = TRUE, auto_unbox = TRUE)
    file.rename(temp_meta, metadata_file)
  }, error = function(e) {
    if (file.exists(temp_meta)) unlink(temp_meta)
    cli::cli_alert_warning("Failed to save metadata: {e$message}")
  })
  
  if (verbose) {
    cli::cli_alert_success("Results saved to: {.path {output_dir}}")
    cli::cli_alert_info("Summary: {.path {summary_file_csv}}")
    
    # Show top models
    top_models <- results_tbl %>%
      dplyr::filter(status == "completed") %>%
      dplyr::slice_head(n = 5)
    
    if (nrow(top_models) > 0) {
      cli::cli_h3("Top 5 Models by RRMSE")
      for (i in 1:nrow(top_models)) {
        m <- top_models[i, ]
        cli::cli_alert_info(
          "{i}. {m$model} | {m$preprocessing} | RRMSE: {round(m$rrmse, 3)} | R²: {round(m$rsq, 3)}"
        )
      }
    }
    
    cli::cli_alert_success(
      "Total evaluation time: {round(metadata$total_time_mins, 1)} minutes"
    )
  }
  
  # Return the results tibble
  return(results_tbl)
}