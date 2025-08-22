#' Evaluate Multiple Model Configurations in Parallel
#'
#' @description
#' Orchestrates parallel evaluation of multiple spectral model configurations using 
#' `evaluate_single_model_parallel()`. Uses work-stealing parallelization for optimal
#' load balancing - each model is processed as an independent task, preventing idle
#' workers when some models take longer than others.
#'
#' @param configs A tibble of model configurations, typically from `create_project_configurations()`
#' @param input_data A tibble containing spectral features and response variable
#' @param covariate_data Optional tibble containing external covariates
#' @param variable Character. Name of the response variable to model
#' @param n_workers Integer. Number of parallel workers to use (default: 4)
#' @param output_dir Character. Directory to save results (default: tempdir())
#' @param chunk_size Integer. Process models in chunks of this size for progress tracking and intermediate saves (default: 50)
#' @param grid_size Integer. Grid search size for each model (default: 10)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 15)
#' @param cv_folds Integer. Cross-validation folds (default: 5)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#' @param save_individual_models Logical. Save each successful model immediately (default: FALSE)
#' @param verbose Logical. Print progress information (default: TRUE)
#'
#' @return A tibble containing all model results with columns:
#' \itemize{
#'   \item Configuration columns from input configs
#'   \item \strong{rmse, rrmse, rsq, mae, rpd}: Performance metrics
#'   \item \strong{runtime_seconds}: Execution time per model
#'   \item \strong{status}: "success" or "failed" 
#'   \item \strong{best_params}: Nested list of optimal hyperparameters
#'   \item \strong{error_info}: Error details for failed models
#'   \item \strong{tuning_info}: Timing and convergence details
#' }
#'
#' @details
#' This function provides a clean parallel interface that:
#' - Distributes model configurations across workers
#' - Prevents nested parallelization issues
#' - Aggregates results into a single tibble
#' - Saves detailed error logs for debugging
#' - Manages memory efficiently across workers
#'
#' @export

evaluate_models_parallel <- function(configs,
                                     input_data,
                                     covariate_data         = NULL,
                                     variable,
                                     n_workers              = 4,
                                     output_dir             = NULL,
                                     chunk_size             = 50,
                                     grid_size              = 10,
                                     bayesian_iter          = 15,
                                     cv_folds               = 5,
                                     seed                   = 123,
                                     save_individual_models = FALSE,
                                     verbose                = TRUE) {
  
  start_time <- Sys.time()
  
  ## Capture baseline memory for monitoring -----------------------------------
  
  baseline_memory_mb <- tryCatch({
    as.numeric(pryr::mem_used()) / 1048576
  }, error = function(e) NULL)
  
  ## Initialize global best tracking ------------------------------------------
  
  global_best_result <- NULL
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Validation and Setup
  ## ---------------------------------------------------------------------------
  
  ## Basic validation ----------------------------------------------------------
  
  if (!is.data.frame(configs) || nrow(configs) == 0) {
    cli::cli_abort("configs must be a non-empty data frame")
  }
  
  if (!variable %in% colnames(input_data)) {
    cli::cli_abort("Variable '{variable}' not found in input_data")
  }
  
  ## Setup output directory ----------------------------------------------------
  
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), "parallel_models")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  ## Adjust worker count to available cores -----------------------------------
  
  max_cores <- parallel::detectCores() - 1  # Leave one core free
  n_workers <- min(n_workers, max_cores, nrow(configs))
  
  if (verbose) {
    cli::cli_h1("Parallel Model Evaluation")
    cli::cli_alert_info("Models to evaluate: {nrow(configs)}")
    cli::cli_alert_info("Parallel workers: {n_workers}")
    cli::cli_alert_info("Variable: {variable}")
    cli::cli_alert_info("Output directory: {output_dir}")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Setup Chunking and Progress Tracking
  ## ---------------------------------------------------------------------------
  
  ## Add row indices to configs for tracking -----------------------------------
  
  configs %>%
    dplyr::mutate(config_index = dplyr::row_number(),
                  .before      = 1) -> configs_indexed
  
  ## Setup for work-stealing parallelization ----------------------------------
  
  n_configs <- nrow(configs_indexed)
  
  if (verbose) {
    cli::cli_h2("Work-stealing parallel processing")
    cli::cli_alert_info("Total models: {n_configs}")
    cli::cli_alert_info("Workers: {n_workers}")
    cli::cli_alert_info("Using dynamic scheduling for optimal load balancing")
  }
  
  ## Setup parallel backend ----------------------------------------------------
  
  # OPTIMIZED: Use multicore for shared memory on Unix/Mac, fallback to multisession on Windows/RStudio
  if (.Platform$OS.type == "windows" || Sys.getenv("RSTUDIO") == "1") {
    future::plan(future::multisession, workers = n_workers)
    if (verbose) cli::cli_alert_info("Using multisession with {n_workers} workers")
  } else {
    future::plan(future::multicore, workers = n_workers)
    if (verbose) cli::cli_alert_info("Using multicore (shared memory) with {n_workers} workers")
  }
  
  # OPTIMIZED: Increase memory limits for large spectral data (default is 500MB)
  options(future.globals.maxSize = 2000 * 1024^2)  # 2GB limit
  if (verbose) cli::cli_alert_info("Set future.globals.maxSize to 2GB for large spectral matrices")
  
  ## Initialize results storage ------------------------------------------------
  
  all_results      <- list()
  models_completed <- 0
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Process All Models with Work-Stealing
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_alert_info("Starting parallel processing of {n_configs} models...")
    start_time_processing <- Sys.time()
  }
  
  # Process all models with work-stealing parallelization
  all_results <- furrr::future_map(
    .x = seq_len(nrow(configs_indexed)),
    .f = function(row_idx) {
      
      ## Process single config with work-stealing ----------------------
      
      config <- configs_indexed[row_idx, ]
    
    ## Enhanced progress indicator with fallback ----------------------------
    
    tryCatch({
      if (verbose) {
        cli::cli_progress_step("Chunk {chunk_idx}/{n_chunks}: {nrow(chunk_configs)} models on {n_workers} workers")
      }
    }, error = function(e) {
      # Fallback to existing simple reporting
      if (verbose) {
        cli::cli_h2("Processing chunk {chunk_idx} of {n_chunks}")
        cli::cli_alert_info("Models {min(current_indices)} to {max(current_indices)}")
        cli::cli_alert_info("Chunk size: {nrow(chunk_configs)} models")
      }
    })
    
    ## Animated inchworm crawler while chunk processes ------------------------
    
    if (verbose) {
      # Show animated inchworm crawling across the console
      base_msg <- sprintf("Chunk %d/%d: %d models on %d workers", 
                         chunk_idx, n_chunks, nrow(chunk_configs), n_workers)
      
      # Inchworm animation using braille dots
      # The worm stretches and contracts as it moves
      worm_frames <- c(
        "⠋⠙⠸",       # Contracted
        "⠋⠉⠙⠸",     # Stretching
        "⠋⠉⠉⠙⠸",   # Stretched
        "⠄⠋⠉⠙⠸",   # Moving forward
        "⠄⠄⠋⠙⠸",   # Contracting
        "⠄⠄⠄⠙⠸",   # Moved
        "⠄⠄⠄⠄⠸"    # Continuing
      )
      
      # Create a crawling effect across the console
      track_width <- 30  # Width of the track
      
      for(i in 1:20) {
        # Calculate position and worm state
        position <- (i - 1) %% track_width
        worm_state <- ((i - 1) %/% 3) %% 3  # Changes every 3 frames
        
        # Build the track with the worm at current position
        track <- rep(" ", track_width)
        
        # Place the worm based on its state
        if(worm_state == 0) {
          worm <- "⠋⠙⠸"  # Contracted
        } else if(worm_state == 1) {
          worm <- "⠋⠉⠙⠸"  # Stretched
        } else {
          worm <- "⠙⠸⠇"  # Different shape
        }
        
        # Create the display line with padding
        left_pad <- paste(rep("·", position), collapse = "")
        right_pad <- paste(rep("·", max(0, track_width - position - nchar(worm))), collapse = "")
        
        # Display the crawling worm
        cat(sprintf("\r  %s %s [%s%s%s]", 
                   base_msg,
                   "Processing",
                   left_pad,
                   worm,
                   right_pad))
        
        flush.console()
        Sys.sleep(0.15)
      }
      
      # Final message
      cat(sprintf("\r  %s [RUNNING]                                \n", base_msg))
      flush.console()
    }
    
    ## Execute current chunk in parallel --------------------------------------
    
    # WORK-STEALING: Process each model as a separate task for dynamic load balancing
    # This ensures no worker sits idle while others are still processing
    
    furrr::future_map(
      .x = seq_len(nrow(chunk_configs)),
      .f = function(row_idx) {
        
        ## Process single config with work-stealing ----------------------
        
        config <- chunk_configs[row_idx, ]
        
        result <- tryCatch({
          
          ## Call our single model parallel function -------------------------
          
          # Generate unique seed for this model based on config index
          model_seed <- seed + config$config_index
          
          result <- evaluate_single_model_parallel(
            input_data       = input_data,
            covariate_data   = covariate_data,
            variable         = variable,
            model            = config$model,
            transformation   = config$transformation,
            preprocessing    = config$preprocessing,
            feature_selection = config$feature_selection,
            covariates       = config$covariates[[1]],  # Extract from list column
            grid_size        = grid_size,
            bayesian_iter    = bayesian_iter,
            cv_folds         = cv_folds,
            seed             = model_seed  # Pass unique seed for reproducibility
          )
          
          ## Add configuration information back to result -------------------
          
          result$config_index      <- config$config_index
          result$model             <- config$model
          result$transformation    <- config$transformation
          result$preprocessing     <- config$preprocessing
          result$feature_selection <- config$feature_selection
          result$covariates        <- config$covariates[[1]]
          
          return(result)
          
        }, error = function(e) {
          
          ## Capture any errors that escape the single model function -------
          
          return(list(config_index    = config$config_index,
                      model_id        = paste(config$model, config$transformation, config$preprocessing, 
                                              config$feature_selection, sep = "_"),
                      metrics         = NULL,
                      best_params     = NULL,
                      runtime_seconds = 0,
                      status          = "failed",
                      error_info      = list(stage      = "parallel_wrapper",
                                             message    = paste("Parallel execution failed:", conditionMessage(e)),
                                             error_class = class(e),
                                             error_type = "system"),
                      tuning_info            = NULL,
                      model                  = config$model,
                      transformation         = config$transformation,
                      preprocessing          = config$preprocessing,
                      feature_selection      = config$feature_selection,
                      covariates             = config$covariates[[1]]))
        })
        
        return(result)
      },
      .options = furrr::furrr_options(seed       = NULL,  # Let individual models control their own seeds
                                      stdout     = FALSE,  # Reduce console collision
                                      conditions = "message",  # Capture messages but not warnings
                                      scheduling = 1.0,  # WORK-STEALING: Dynamic scheduling
                                      chunk.size = 1)  # Process one model at a time
    ) -> chunk_results
    
    # DEBUG: Check what chunk_results contains
    if (verbose) {
      cli::cli_alert_info("DEBUG: chunk_results class: {class(chunk_results)}, length: {length(chunk_results)}")
      if (is.list(chunk_results) && length(chunk_results) > 0) {
        cli::cli_alert_info("DEBUG: First element class: {class(chunk_results[[1]])}")
        if (is.list(chunk_results[[1]])) {
          cli::cli_alert_info("DEBUG: First element length: {length(chunk_results[[1]])}")
          cli::cli_alert_info("DEBUG: First element names: {paste(names(chunk_results[[1]]), collapse = ', ')}")
        }
      }
    }
    
    ## Store results from this chunk -------------------------------------------
    
    # WORK-STEALING: Now each element in chunk_results is a single model result
    # No need to flatten batches since we're processing models individually
    chunk_results_flat <- chunk_results  # Results are already flat
    
    # Add all results from this chunk
    all_results <- c(all_results, chunk_results)
    models_completed <- models_completed + nrow(chunk_configs)  # Count actual configs processed
    
    ## Save individual models if requested -------------------------------------
    
    if (save_individual_models) {
      
      # WORK-STEALING: Now iterate directly through results (no batches)
      for (result in chunk_results) {
        if (result$status == "success") {
          individual_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          individual_file      <- file.path(output_dir, glue::glue("individual_model_{result$config_index}_{individual_timestamp}.qs"))
          
          qs::qsave(result, individual_file)
        }
      }
      
      if (verbose) {
        # WORK-STEALING: Count successes directly (no nested structure)
        success_models <- sum(sapply(chunk_results, function(x) x$status == "success"))
        cli::cli_alert_info("Saved {success_models} individual successful models")
      }
    }
    
    ## Calculate progress and timing -------------------------------------------
    
    chunk_duration <- difftime(Sys.time(), chunk_start_time, units = "mins")
    total_elapsed  <- difftime(Sys.time(), start_time, units = "mins")
    
    ## Estimate remaining time ------------------------------------------------
    
    avg_time_per_model  <- as.numeric(total_elapsed) / models_completed
    remaining_models    <- n_configs - models_completed
    estimated_remaining <- remaining_models * avg_time_per_model
    
    ## Always calculate progress metrics (not just when verbose) --------------
    
    # WORK-STEALING: Count successes and failures from flat structure
    success_count <- sum(sapply(chunk_results, function(x) 
      !is.null(x$status) && x$status == "success"))
    failed_count <- sum(sapply(chunk_results, function(x) 
      !is.null(x$status) && x$status == "failed"))
    progress_pct  <- round(100*models_completed/n_configs, 1)
    
    ## Enhanced chunk completion reporting with fallback ---------------------
    
    if (verbose) {
      
      # Try enhanced reporting first, fallback to simple on error
      enhanced_reporting_success <- tryCatch({
        
        # Calculate metrics and display fancy box
        chunk_metrics     <- calculate_chunk_metrics(chunk_results)
        memory_stats      <- get_worker_memory_stats(n_workers, baseline_memory_mb)
        global_best_result <<- format_global_best(all_results)  # Update global tracker
        
        print_chunk_summary(
          chunk_idx      = chunk_idx,
          n_chunks       = n_chunks, 
          metrics_list   = chunk_metrics,
          resource_stats = list(
            models_per_min = models_completed / as.numeric(total_elapsed),
            eta_minutes    = estimated_remaining,
            memory         = memory_stats
          ),
          duration = as.numeric(chunk_duration),
          counts   = list(success = success_count, failed = failed_count),
          global_best = global_best_result
        )
        
        TRUE  # Success flag
      }, error = function(e) {
        FALSE  # Failed flag
      })
      
      # Fallback to existing simple reporting if enhanced failed
      if (!enhanced_reporting_success) {
        cli::cli_alert_success("Chunk {chunk_idx} completed in {round(chunk_duration, 1)} minutes")
        cli::cli_alert_info("Success: {success_count}, Failed: {failed_count}")
        cli::cli_alert_info("Progress: {models_completed}/{n_configs} models ({progress_pct}%)")
        cli::cli_alert_info("Estimated time remaining: {round(estimated_remaining, 1)} minutes")
      }
    }
    
    ## Save intermediate results with unique identifier -----------------------
    
    timestamp         <- format(Sys.time(), "%Y%m%d_%H%M%S")
    process_id        <- Sys.getpid()
    # Count how many results we're saving for debugging
    n_results_in_chunk <- success_count + failed_count
    intermediate_file <- file.path(output_dir, glue::glue("intermediate_chunk_{chunk_idx}_{process_id}_{timestamp}.qs"))
    
    # Save the flattened results from this chunk (already extracted above)
    qs::qsave(chunk_results_flat, intermediate_file)
    
    if (verbose) {
      cli::cli_alert_info("Saved intermediate results: {basename(intermediate_file)}")
    }
    
    ## Memory cleanup after each chunk ----------------------------------------
    
    gc(verbose = FALSE, full = TRUE)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Process and Save Final Results
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_h2("Finalizing results")
  }
  
  ## Convert results to tibble format ----------------------------------------
  
  # Enhanced validation with recovery mechanism
  if (length(all_results) == 0) {
    cli::cli_alert_warning("No results were generated")
    return(tibble::tibble())
  }
  
  # Check first element is a proper result object
  first_result <- all_results[[1]]
  if (!is.list(first_result) || is.null(names(first_result))) {
    cli::cli_alert_warning("Results structure invalid - attempting recovery")
    cli::cli_alert_info("all_results length: {length(all_results)}")
    cli::cli_alert_info("First element class: {class(first_result)}")
    
    # Try to extract valid results
    valid_results <- list()
    for (i in seq_along(all_results)) {
      item <- all_results[[i]]
      if (is.list(item) && "metrics" %in% names(item)) {
        valid_results <- c(valid_results, list(item))
      }
    }
    
    if (length(valid_results) > 0) {
      all_results <- valid_results
      cli::cli_alert_success("Recovered {length(valid_results)} valid results from {length(all_results)} items")
    } else {
      # Last resort: try to recover from intermediate files
      cli::cli_alert_warning("Cannot recover from memory - checking intermediate files")
      
      intermediate_files <- list.files(output_dir, pattern = "intermediate.*\\.qs", full.names = TRUE)
      if (length(intermediate_files) > 0) {
        all_results <- list()
        for (file in intermediate_files) {
          chunk_data <- qs::qread(file)
          if (is.list(chunk_data)) {
            for (item in chunk_data) {
              if (is.list(item) && "metrics" %in% names(item)) {
                all_results <- c(all_results, list(item))
              }
            }
          }
        }
        
        if (length(all_results) > 0) {
          cli::cli_alert_success("Recovered {length(all_results)} results from intermediate files")
        } else {
          stop("Cannot recover any valid results from intermediate files")
        }
      } else {
        stop("No intermediate files found for recovery")
      }
    }
  }
  
  all_results %>%
    purrr::map_dfr(function(result) {
      
      ## Extract metrics if available -----------------------------------------
      
      if (!is.null(result$metrics)) {
        metrics_df <- result$metrics
      } else {
        metrics_df <- tibble::tibble(rmse   = NA_real_,
                                     rrmse  = NA_real_,
                                     rsq    = NA_real_,
                                     mae    = NA_real_,
                                     rpd    = NA_real_,
                                     n_test = NA_integer_)
      }
      
      ## Create row with all information --------------------------------------
      
      tibble::tibble(config_index      = result$config_index %||% NA_integer_,
                     model_id          = result$model_id,
                     model             = result$model,
                     transformation    = result$transformation,
                     preprocessing     = result$preprocessing,
                     feature_selection = result$feature_selection,
                     covariates        = list(result$covariates),  # Keep as list column
                     status            = result$status,
                     runtime_seconds   = result$runtime_seconds,
                     rmse              = metrics_df$rmse,
                     rrmse             = metrics_df$rrmse,
                     rsq               = metrics_df$rsq,
                     mae               = metrics_df$mae,
                     rpd               = metrics_df$rpd,
                     n_test            = metrics_df$n_test,
                     best_params       = list(result$best_params),  # Keep as list column
                     error_info        = list(result$error_info),    # Keep as list column
                     tuning_info       = list(result$tuning_info))   # Keep as list column
    }) %>%
    dplyr::arrange(config_index) -> results_df
  
  ## Calculate final summary statistics --------------------------------------
  
  total_time    <- difftime(Sys.time(), start_time, units = "mins")
  n_success     <- sum(results_df$status == "success", na.rm = TRUE)
  n_failed      <- sum(results_df$status == "failed", na.rm = TRUE)
  success_rate  <- round(100 * n_success / nrow(results_df), 1)
  
  if (verbose) {
    cli::cli_h2("Final Summary")
    cli::cli_alert_success("Total models processed: {nrow(results_df)}")
    cli::cli_alert_success("Successful models: {n_success} ({success_rate}%)")
    cli::cli_alert_info("Failed models: {n_failed}")
    cli::cli_alert_info("Total runtime: {round(total_time, 1)} minutes")
    cli::cli_alert_info("Average time per model: {round(as.numeric(total_time)/nrow(results_df), 2)} minutes")
  }
  
  ## Save final combined results ---------------------------------------------
  
  timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  final_file <- file.path(output_dir, glue::glue("parallel_results_{variable}_{timestamp}.qs"))
  
  qs::qsave(results_df, final_file)
  
  if (verbose) {
    cli::cli_alert_success("Final results saved: {basename(final_file)}")
  }
  
  ## Cleanup parallel backend ------------------------------------------------
  
  future::plan(future::sequential)
  
  return(results_df)
}