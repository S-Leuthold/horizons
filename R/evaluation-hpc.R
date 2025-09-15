#' HPC Evaluation Orchestrator
#'
#' @description
#' Runs model evaluation using nested parallelization for HPC environments.
#' Respects resources allocated by the scheduler (SLURM/PBS) or uses available cores.
#'
#' @param config Data frame of model configurations
#' @param input_data Data frame with spectral data
#' @param covariate_data Optional data frame with covariates
#' @param variable Character. Name of response variable
#' @param output_dir Character. Directory for results
#' @param grid_size Integer. Grid search size (default: 10)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 15)
#' @param cv_folds Integer. Cross-validation folds (default: 10)
#' @param outer_workers Integer. Number of concurrent models (auto if NULL)
#' @param inner_workers Integer. Workers per model (auto if NULL)
#' @param seed Integer. Random seed (default: 307)
#' @param resume Logical. Resume from checkpoints (default: TRUE)
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return List with evaluation results
#'
#' @export

evaluate_models_hpc <- function(config,
                                input_data,
                                covariate_data = NULL,
                                variable,
                                output_dir      = NULL,
                                grid_size       = 10,
                                bayesian_iter   = 15,
                                cv_folds        = 10,
                                outer_workers   = NULL,
                                inner_workers   = NULL,
                                seed            = 307,
                                prune_models    = FALSE,
                                prune_threshold = 0.8,
                                resume          = TRUE,
                                verbose         = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  ## Make sure we're getting a dataframe in ------------------------------------

  if (!is.data.frame(config)) cli::cli_abort("▶ evaluate_models_hpc: config must be a data frame")


  ## Check that variable is present --------------------------------------------

  if (!variable %in% names(input_data)) cli::cli_abort("▶ evaluate_models_hpc: Response variable '{variable}' not found in input_data")

  ## Validate response variable has enough non-missing values -------------------

  response_vals <- input_data[[variable]]
  n_valid       <- sum(!is.na(response_vals))

  if (n_valid < 20) cli::cli_abort("▶ evaluate_models_hpc: Response variable has only {n_valid} non-missing values (minimum 20 required)")

  ## Check for variation in response variable ----------------------------------

  if (var(response_vals, na.rm = TRUE) < .Machine$double.eps) cli::cli_abort("▶ evaluate_models_hpc: Response variable has no variation")

  ## Check required config columns ---------------------------------------------

  required_cols <- c("model", "preprocessing", "transformation", "feature_selection")
  missing_cols  <- setdiff(required_cols, names(config))

  if (length(missing_cols) > 0) cli::cli_abort("▶ evaluate_models_hpc: config missing required columns: {missing_cols}")

  ## ---------------------------------------------------------------------------
  ## Step 2: Parallelization checks
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Step 2.1: Validate worker parameters
    ## ---------------------------------------------------------------------------

    ## Make sure the worker breakdown is specified -------------------------------

    if (is.null(outer_workers) || is.null(inner_workers)) cli::cli_abort("▶ evaluate_models_hpc: Both outer_workers and inner_workers must be specified for HPC execution")

    ## Validate worker values ----------------------------------------------------

    if (outer_workers < 1 || inner_workers < 1) cli::cli_abort("▶ evaluate_models_hpc: outer_workers and inner_workers must be >= 1")

    ## Check total requested cores against available --------------------------------

    total_requested <- outer_workers * inner_workers
    cores_available <- parallel::detectCores()

    if (total_requested > cores_available) cli::cli_abort("▶ evaluate_models_hpc: Requested {total_requested} cores ({outer_workers} × {inner_workers}) but only {cores_available} available")

    ## -------------------------------------------------------------------------
    ## Step 2.2: Thread control verification
    ## -------------------------------------------------------------------------

    ## Check critical thread environment variables ------------------------------

    list(OMP_NUM_THREADS        = Sys.getenv("OMP_NUM_THREADS", unset = NA),
         OPENBLAS_NUM_THREADS   = Sys.getenv("OPENBLAS_NUM_THREADS", unset = NA),
         MKL_NUM_THREADS        = Sys.getenv("MKL_NUM_THREADS", unset = NA),
         VECLIB_MAXIMUM_THREADS = Sys.getenv("VECLIB_MAXIMUM_THREADS", unset = NA)) -> thread_vars

    ## Check if any are unset or not equal to 1 --------------------------------

    unset_vars <- names(thread_vars)[is.na(thread_vars)]
    bad_vars   <- names(thread_vars)[!is.na(thread_vars) & thread_vars != "1"]

    ## Warn user if thread control is not properly configured ------------------

    if (verbose && (length(unset_vars) > 0 || length(bad_vars) > 0)) {

      cli::cli_alert_warning("Thread control not properly configured for HPC")

      if (length(unset_vars) > 0) {

        cli::cli_alert_danger("Unset variables: {paste(unset_vars, collapse = ', ')}")

      }

      if (length(bad_vars) > 0) {

        cli::cli_alert_danger("Variables not set to 1: {paste(bad_vars, '=', thread_vars[bad_vars], collapse = ', ')}")

      }

      cli::cli_alert_info("Add to your HPC submission script:")

      cli::cli_code(c("export OMP_NUM_THREADS=1",
                      "export OPENBLAS_NUM_THREADS=1",
                      "export MKL_NUM_THREADS=1",
                      "export VECLIB_MAXIMUM_THREADS=1"))

      cli::cli_alert_warning("Proceeding anyway - thread oversubscription may occur")

    } else if (verbose) {

      cli::cli_alert_success("Thread control properly configured (all set to 1)")

    }

    ## Set package-specific threading ------------------------------------------

    data.table::setDTthreads(1)

    options(mc.cores           = inner_workers,
            ranger.num.threads = 1,
            xgboost.nthread    = 1)

    ## Update status before moving on ------------------------------------------

    if (verbose) {

      cli::cli_h1("HPC Evaluation")
      cli::cli_alert_info("Workers: {outer_workers} outer × {inner_workers} inner = {total_requested} cores")
      cli::cli_alert_info("Models to evaluate: {nrow(config)}")

    }

  ## ---------------------------------------------------------------------------
  ## Step 3: Output and resumption setup
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Step 3.1: Create output directories
    ## -------------------------------------------------------------------------

    ## If no directory is specified, create one with timestamp -----------------

    if (is.null(output_dir)) {

      timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
      output_dir <- file.path("output", paste0(variable, "_hpc_", timestamp))

    }

    if (!dir.exists(output_dir)) {

      dir.create(output_dir, recursive = TRUE)

    }

    ## Create checkpoint directory ------------------------------------------------

    checkpoint_dir <- file.path(output_dir, "checkpoints")

    if (!dir.exists(checkpoint_dir)) {

      dir.create(checkpoint_dir, recursive = TRUE)

    }

    ## Create subdirectories for organization -------------------------------------

    dir.create(file.path(output_dir, "results"), showWarnings = FALSE)
    dir.create(file.path(output_dir, "errors"), showWarnings = FALSE)
    dir.create(file.path(output_dir, "summary"), showWarnings = FALSE)

    ## Report successful creation ------------------------------------------------

    if (verbose) {
      cli::cli_alert_info("Output directory: {output_dir}")
    }

    ## ---------------------------------------------------------------------------
    ## Step 3.2: Resume previous runs if requested
    ## ---------------------------------------------------------------------------

    completed_models <- integer(0)

    ## Check for previously completed models -------------------------------------

    if (resume && dir.exists(checkpoint_dir)) {

      ## Look through the batch files (groups of 100) ----------------------------

      batch_files <- list.files(checkpoint_dir, pattern = "^batch_.*\\.qs$", full.names = FALSE)

      if (length(batch_files) > 0) {

        batch_nums <- as.integer(gsub("batch_|\\.qs", "", batch_files))
        batch_nums <- batch_nums[!is.na(batch_nums)]

        ## Aggregate and report completed models -------------------------------

        for (batch in batch_nums) {

          start_idx        <- (batch - 1) * 100 + 1
          end_idx          <- batch * 100
          completed_models <- c(completed_models, start_idx:end_idx)

        }
      }

      ## Check for models not yet batched ----------------------------------------

      checkpoint_files <- list.files(checkpoint_dir, pattern = "^model_.*\\.qs$", full.names = FALSE)

      if (length(checkpoint_files) > 0) {

        individual_models <- as.integer(gsub("model_|\\.qs", "", checkpoint_files))
        individual_models <- individual_models[!is.na(individual_models)]
        completed_models  <- c(completed_models, individual_models)

      }

      # Remove duplicates and validate -------------------------------------------
      completed_models <- unique(completed_models)
      completed_models <- completed_models[completed_models >= 1 & completed_models <= nrow(config)]
      completed_models <- sort(completed_models)

      ## Report resumption status ------------------------------------------------

      if (length(completed_models) > 0 && verbose) {

        cli::cli_alert_info("Found {length(completed_models)} completed models to resume from")
        cli::cli_alert_info("Batches: {length(batch_files)}, Individual: {length(checkpoint_files)}")

        }

    }

  ## ---------------------------------------------------------------------------
  ## Step 4: Data splitting
  ## ---------------------------------------------------------------------------

  ## Standardize response variable name ----------------------------------------

  input_data %>%
    dplyr::rename(Response = !!rlang::sym(variable)) -> input_data

  ## Update variable name for downstream use -----------------------------------

  variable <- "Response"

  ## Create train/test split (same for all models) -----------------------------

  set.seed(seed)

  rsample::initial_split(data = input_data,
                         prop = 0.8,
                         strata = !!sym(variable)) -> data_split

  ## Report split summary ------------------------------------------------------

  if (verbose) {

    train_n <- nrow(rsample::training(data_split))
    test_n  <- nrow(rsample::testing(data_split))

    cli::cli_alert_info("Data split: {train_n} training, {test_n} testing samples")

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Nested parallelization configuration
  ## ---------------------------------------------------------------------------

  ## Log start time ------------------------------------------------------------

  start_time <- Sys.time()

  ## Enusre future plan is reset on exit --------------------------------------

  on.exit({

    tryCatch(future::plan(future::sequential),
             error = function(e) NULL)

    ## Clean up any leftover temp files --------------------------------------

    temp_files <- list.files(checkpoint_dir, pattern = "\\.tmp$", full.names = TRUE)

    if (length(temp_files) > 0) {

      file.remove(temp_files)

    }

  }, add = TRUE)

  ## Setup parallel backend ----------------------------------------------------

  ## Store and increase globals size limit for large spectral data
  old_maxSize <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = 2 * 1024^3)  # Set to 2GB for spectral workflows

  ## Restore original limit on exit
  on.exit({
    options(future.globals.maxSize = old_maxSize)
  }, add = TRUE)

  future::plan(future::multisession, workers = outer_workers)

  ## Report parallelization setup ----------------------------------------------

  if (verbose) {

    cli::cli_alert_info("Configured outer parallelization with {outer_workers} workers")

    ## ========== DEBUG: Worker spawn verification ==========
    cli::cli_alert_info("[DEBUG-OUTER] Backend type: {class(future::plan())[1]}")
    cli::cli_alert_info("[DEBUG-OUTER] Workers available: {future::nbrOfWorkers()}")
    cli::cli_alert_info("[DEBUG-OUTER] R processes before spawn: {system('ps aux | grep \"[R]\" | wc -l', intern = TRUE)}")
    ## ========== END DEBUG ==========

    ## Initialize debug log file
    debug_log_file <- file.path(output_dir, "debug_worker_output.log")
    cat(sprintf("\n==== HPC Debug Log Started: %s ====\n", Sys.time()), file = debug_log_file)
    cli::cli_alert_info("Debug log file: {debug_log_file}")
    cli::cli_alert_info("Monitor with: tail -f {debug_log_file}")

  }


  ## ---------------------------------------------------------------------------
  ## Step 6: Process all the models using nested setup
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Step 6.1: Identify models to process
    ## -------------------------------------------------------------------------

    if(resume){

      models_to_process <- setdiff(seq_len(nrow(config)), completed_models)

      if (length(models_to_process) == 0) {

        cli::cli_alert_success("All models already completed! Loading results...")

        lapply(seq_len(nrow(config)), function(i) {

           checkpoint_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", i))

           if (file.exists(checkpoint_file)) {

             qs::qread(checkpoint_file)

           } else {

             cli::cli_alert_warning("Missing checkpoint for model {i}")

             NULL
           }
        }) -> results

        results <- results[!sapply(results, is.null)]

        ## Report loaded models ---------------------------------------------------

        if (verbose) {

          cli::cli_alert_success("Loaded {length(results)} completed models")

        }

        return(results)

      }

    } else {

      models_to_process <- seq_len(nrow(config))

  }

  ## ---------------------------------------------------------------------------
  ## Step 6.2: Evaluate models in parallel with nested CV
  ## ---------------------------------------------------------------------------

  ## Set furrr options -------------------------------------------------------

  ## Force output capture for debugging
  furrr_opts <- furrr::furrr_options(
    seed       = NULL,
    stdout     = TRUE,  # Enable to see debug output from workers
    conditions = "message",  # Capture ALL conditions including messages
    chunk_size = 1,
    scheduling = 1
  )

  ## Run the model evaluation ---------------------------------------------------

  furrr::future_map(.x = models_to_process,
                    .f = function(i) {

                      ## ========== DEBUG: Worker process info ==========
                      ## Method 1: Direct file logging (most reliable)
                      debug_log_file <- file.path(output_dir, "debug_worker_output.log")
                      debug_msg <- sprintf("[%s] [WORKER-%d] Model %d started on PID: %d | Inner: %d | mc.cores: %s | Plan: %s\n",
                                          format(Sys.time(), "%H:%M:%S"),
                                          i, i, Sys.getpid(),
                                          inner_workers,
                                          getOption('mc.cores'),
                                          class(future::plan())[1])
                      cat(debug_msg, file = debug_log_file, append = TRUE)

                      ## Method 2: Write to stderr (often works better than message)
                      cat(debug_msg, file = stderr())

                      ## Method 3: Force immediate flush
                      message(debug_msg)
                      flush.console()
                      ## ========== END DEBUG ==========

                      ## Extract and validate covariates for this model --------

                      config_row <- config[i, , drop = FALSE]

                      covariate_cols <- if ("covariates" %in% names(config_row) && !is.null(config_row$covariates[[1]])) {
                        config_row$covariates[[1]]
                      } else {
                        NULL
                      }

                      ## -------------------------------------------------------
                      ## Step X: Validate that requested covariates exist
                      ## -------------------------------------------------------

                      if (!is.null(covariate_cols) && !is.null(covariate_data)) {

                        ## ========== DEBUG: Covariate validation ==========
                        cli::cli_alert_info("[DEBUG-COV-{i}] Requested covariates: {paste(covariate_cols, collapse=', ')}")
                        cli::cli_alert_info("[DEBUG-COV-{i}] Available columns: {paste(names(covariate_data), collapse=', ')}")
                        ## ========== END DEBUG ==========

                        missing_covs <- setdiff(covariate_cols, names(covariate_data))

                        if (length(missing_covs) > 0) {

                          ## Create the json file path--------------------------

                          error_file <-  fs::path(output_dir, "errors", paste0(i, ".json"))

                          ## Create a failed result entry ----------------------------------------

                          create_failed_result(config_id     = i,
                                               config_clean  = NULL,
                                               error_message = glue::glue("Covariate data requested but not found for {missing_covs}"),
                                               workflow_id   = i,
                                               error_detail  = NULL,
                                               error_stage   = "HPC - Covariate data assembly (Step X)",
                                               error_trace   = NULL,
                                               warnings      = NULL,
                                               messages      = NULL) -> failed_result

                          ## Create a structured error file --------------------------------------

                          list(workflow_id   = i,
                               config        = as.list(config_row),
                               error_stage   = failed_result$error_stage,
                               error_class   = failed_result$error_class,
                               error_message = failed_result$error_message,
                               has_trace     = failed_result$has_trace,
                               n_warnings    = failed_result$n_warnings,
                               warnings      = failed_result$warnings %||% NULL,
                               messages      = failed_result$messages %||% NULL,
                               total_seconds = failed_result$total_seconds,
                               timestamp     = as.character(Sys.time())) -> error_object

                          ## Save the error file using an atomic write ---------------------------

                          temp_error <- tempfile(tmpdir = dirname(error_file), fileext = ".json.tmp")

                          tryCatch({

                            jsonlite::write_json(x          = error_object,
                                                 path       = temp_error,
                                                 pretty     = TRUE,
                                                 auto_unbox = TRUE)

                            file.rename(temp_error, error_file)

                          }, error = function(e) {

                            if (file.exists(temp_error)) unlink(temp_error)

                          })

                          cat(paste0("Model: ", i, " failed during covariate joining."))

                          return(failed_result)

                        }

                        # Subset covariates to only what this model needs ------

                        model_covariate_data <- covariate_data[, c("Sample_ID", covariate_cols), drop = FALSE]

                      } else {

                        model_covariate_data <- covariate_data

                      }

                      ## -------------------------------------------------------
                      ## Run model evaluation
                      ## -------------------------------------------------------

                      ## ========== DEBUG: Inner parallelization setup ==========
                      ## File logging for inner setup
                      inner_msg <- sprintf("[%s] [EVAL-%d] Starting evaluation | inner_workers: %d | grid: %d | bayesian: %d | CV: %d\n",
                                          format(Sys.time(), "%H:%M:%S"),
                                          i, inner_workers, grid_size, bayesian_iter, cv_folds)
                      cat(inner_msg, file = debug_log_file, append = TRUE)
                      cat(inner_msg, file = stderr())
                      ## ========== END DEBUG ==========

                      evaluate_configuration(config_row      = config_row,
                                             input_data      = input_data,
                                             data_split      = data_split,
                                             config_id       = i,
                                             covariate_data  = model_covariate_data,
                                             variable        = variable,
                                             output_dir      = output_dir,
                                             grid_size       = grid_size,
                                             bayesian_iter   = bayesian_iter,
                                             cv_folds        = cv_folds,
                                             allow_par       = TRUE,
                                             n_cv_cores      = inner_workers,
                                             prune_models    = prune_models,
                                             prune_threshold = prune_threshold,
                                             seed            = seed,
                                             verbose         = FALSE) -> result

                      ## -------------------------------------------------------
                      ## Store and evaluate the model results ------------------
                      ## -------------------------------------------------------

                      if (result$status == "failed") {

                        error_file <-  fs::path(output_dir, "errors", paste0(i, ".json"))

                        ## Create a structured error file --------------------------------------

                        list(workflow_id   = i,
                             config        = as.list(config_row),
                             error_stage   = result$error_stage,
                             error_class   = result$error_class,
                             error_message = result$error_message,
                             has_trace     = result$has_trace,
                             n_warnings    = result$n_warnings,
                             warnings      = result$warnings %||% NULL,
                             messages      = result$messages %||% NULL,
                             total_seconds = result$total_seconds,
                             timestamp     = as.character(Sys.time())) -> error_object

                        ## Save the error file using an atomic write ---------------------------

                        temp_error <- tempfile(tmpdir = dirname(error_file), fileext = ".json.tmp")

                        tryCatch({

                          jsonlite::write_json(x          = error_object,
                                               path       = temp_error,
                                               pretty     = TRUE,
                                               auto_unbox = TRUE)

                          file.rename(temp_error, error_file)

                        }, error = function(e) {

                          if (file.exists(temp_error)) unlink(temp_error)

                        })

                        cat(paste0("Model: ", i, " failed-- check error log."))

                        return(result)

                        } else {

                          final_result <- result
                      }


                      ## -------------------------------------------------------
                      ## Step 6.3: Checkpointing and consolidation
                      ## -------------------------------------------------------

                      ## Save checkpoint for this model with atomic write ------

                      if (!is.null(checkpoint_dir)) {

                        checkpoint_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", i))
                        temp_file       <- paste0(checkpoint_file, ".tmp")


                        qs::qsave(final_result, temp_file)
                        file.rename(temp_file, checkpoint_file)

                      }

                      ## Consolidate every 100 models --------------------------

                      if (i %% 100 == 0) {

                        batch_num  <- i %/% 100
                        batch_file <- file.path(checkpoint_dir, sprintf("batch_%06d.qs", batch_num))

                        ## Collect the last 100 individual checkpoints ---------

                        start_idx <- (batch_num - 1) * 100 + 1
                        end_idx   <- i


                        lapply(start_idx:end_idx,
                               function(idx) {

                                 ind_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", idx))
                                 if (file.exists(ind_file)) {

                                   qs::qread(ind_file)

                                   } else {

                                     NULL
                                  }
                        }) -> batch_results

                        # Save batch file atomically ---------------------------

                        batch_temp <- paste0(batch_file, ".tmp")

                        qs::qsave(batch_results, batch_temp)

                        file.rename(batch_temp, batch_file)

                        # Remove individual files after successful batch save --

                        for (idx in start_idx:end_idx) {

                          ind_file <- file.path(checkpoint_dir, sprintf("model_%06d.qs", idx))

                          if (file.exists(ind_file)) file.remove(ind_file)

                        }

                        if (verbose) {

                          cli::cli_alert_success("Consolidated models {start_idx}-{end_idx} into batch {batch_num}")
                        }
                      }

                      ## Return the final results ------------------------------

                      ## ========== DEBUG: Model completion ==========
                      if (!is.null(final_result) && final_result$status == "success") {
                        completion_msg <- sprintf("[%s] [COMPLETE-%d] Model %d completed successfully (RMSE: %.3f, R²: %.3f)\n",
                                                 format(Sys.time(), "%H:%M:%S"),
                                                 i, i, final_result$rmse, final_result$rsq)
                      } else if (!is.null(final_result) && final_result$status == "pruned") {
                        completion_msg <- sprintf("[%s] [COMPLETE-%d] Model %d was pruned (insufficient performance)\n",
                                                 format(Sys.time(), "%H:%M:%S"), i, i)
                      } else {
                        completion_msg <- sprintf("[%s] [COMPLETE-%d] Model %d failed\n",
                                                 format(Sys.time(), "%H:%M:%S"), i, i)
                      }
                      cat(completion_msg, file = debug_log_file, append = TRUE)
                      cat(completion_msg, file = stderr())
                      message(completion_msg)
                      ## ========== END DEBUG ==========

                      return(final_result)
          },
          .options = furrr_opts,
          .progress = FALSE) -> results


  ## ---------------------------------------------------------------------------
  ## Step 7: Compile and summarize results
  ## ---------------------------------------------------------------------------

  ## Load resumption models ----------------------------------------------------

  if (resume && length(completed_models) > 0) {

    previous_results <- list()

    ## Batch files -------------------------------------------------------------

    batch_files <- list.files(checkpoint_dir, pattern = "^batch_.*\\.qs$", full.names = TRUE)

    for (batch_file in batch_files) {

      tryCatch({

        batch_data       <- qs::qread(batch_file)
        previous_results <- c(previous_results, batch_data)

      }, error = function(e) {

        cli::cli_alert_warning("Failed to load batch file {basename(batch_file)}: {e$message}")

      })
    }

    ## Checkpoint files --------------------------------------------------------

    individual_files <- list.files(checkpoint_dir, pattern = "^model_.*\\.qs$", full.names = TRUE)

    for (ind_file in individual_files) {

      tryCatch({

        ind_data         <- qs::qread(ind_file)
        previous_results <- c(previous_results, list(ind_data))

      }, error = function(e) {

        cli::cli_alert_warning("Failed to load checkpoint {basename(ind_file)}: {e$message}")

      })
    }

    ## Combine with computed results -------------------------------------------

    all_results <- c(previous_results[!sapply(previous_results, is.null)], results)

    } else {

      all_results <- results
  }


  ## Separate successful and failed models -------------------------------------

  successful_results <- all_results[sapply(all_results, function(x) {

    !is.null(x) && (!is.null(x$status) && x$status != "failed")

  })]

  failed_results <- all_results[sapply(all_results, function(x) {

    !is.null(x) && (!is.null(x$status) && x$status == "failed")

  })]

  ## Calculate execution time --------------------------------------------------

  execution_time <- difftime(Sys.time(), start_time, units = "mins")

  ## Extract top models --------------------------------------------------------

  top_models <- NULL

  if (length(successful_results) > 0) {

    purrr::map_dfr(successful_results,
                   function(res) {

                     if (!is.null(res$metrics)) {

                       tibble::tibble(config_id         = res$config_id,
                                      workflow_id       = res$workflow_id,
                                      model             = res$config$model,
                                      preprocessing     = res$config$preprocessing,
                                      transformation    = res$config$transformation,
                                      feature_selection = res$config$feature_selection,
                                      rrmse             = res$rrmse,
                                      rsq               = res$rsq,
                                      ccc               = res$ccc,
                                      rpd               = res$rpd,
                                      rmse              = res$rmse,
                                      mae               = res$mae)
                     }
                   }) -> model_metrics


    if (nrow(model_metrics) > 0) {

      model_metrics %>%
        dplyr::arrange(rrmse) %>%
        dplyr::slice_head(n = 5) -> top_models

    }
  }

  ## Print a nice summary ------------------------------------------------------

  if (verbose) {

    cli::cli_h1("HPC Evaluation Complete")

    cli::cli_h2("Execution Summary")
    cli::cli_alert_success("Total models: {nrow(config)}")
    cli::cli_alert_info("Completed: {length(successful_results)}")

    if (length(failed_results) > 0) {

      cli::cli_alert_warning("Failed: {length(failed_results)}")

    }

    if (length(completed_models) > 0) {

      cli::cli_alert_info("Previously completed (resumed): {length(completed_models)}")

    }

    cli::cli_alert_info("Execution time: {round(as.numeric(execution_time), 1)} minutes")
    cli::cli_alert_info("Parallel configuration: {outer_workers} outer × {inner_workers} inner workers")

    # Top models----------------------------------------------------------------

    if (!is.null(top_models) && nrow(top_models) > 0) {

      cli::cli_h2("Top 5 Models by RRMSE")

      for (i in 1:nrow(top_models)) {

        m <- top_models[i, ]
        cli::cli_alert_info("{i}. {m$model} | {m$preprocessing} | {m$transformation} | RRMSE: {round(m$rrmse, 4)}")
      }
    }

    ## Output directory structure ----------------------------------------------

    cli::cli_h2("Output Directory Structure")
    cli::cli_text("{.path {output_dir}/}")
    cli::cli_text("├── {.file checkpoints/}")
    cli::cli_text("├── {.file results/}")
    cli::cli_text("├── {.file errors/}")
    cli::cli_text("└── {.file summary/}")
  }

  ## Return summary information ------------------------------------------------

  list(execution = list(n_total      = nrow(config),
                        n_successful = length(successful_results),
                        n_failed     = length(failed_results),
                        n_resumed    = length(completed_models),
                        time_minutes = as.numeric(execution_time),
                        timestamp    = Sys.time()),

       configuration = list(outer_workers = outer_workers,
                            inner_workers = inner_workers,
                            total_cores   = outer_workers * inner_workers,
                            seed          = seed),

       paths = list(output_dir     = output_dir,
                    checkpoint_dir = checkpoint_dir,
                    results_dir    = file.path(output_dir, "results"),
                    errors_dir     = file.path(output_dir, "errors")),

       top_models = top_models) -> summary_output

  class(summary_output) <- c("horizons_hpc_summary", "list")

  ## Save summary to file for later reference ----------------------------------

  summary_file <- file.path(output_dir, "summary", "evaluation_summary.qs")

  qs::qsave(summary_output, summary_file)

  if (verbose) {

    cli::cli_alert_success("Summary saved to: {summary_file}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 8: fin
  ## ---------------------------------------------------------------------------

  return(invisible(summary_output))
}
