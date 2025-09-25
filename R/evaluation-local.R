#' Evaluate Model Configurations on Local Machines
#'
#' Executes comprehensive model evaluation workflows locally with sequential model processing
#' and optional parallel cross-validation. This is the primary entry point for desktop and
#' server-based model comparison outside of HPC environments, providing automated hyperparameter
#' tuning, checkpointing, and performance tracking.
#'
#' @param config `[data.frame]` Configuration grid with required columns:
#'   * `model`: Model type (e.g., "random_forest", "cubist", "xgboost")
#'   * `preprocessing`: Spectral preprocessing method
#'   * `transformation`: Response transformation method
#'   * `feature_selection`: Feature selection approach
#'   * `covariates`: (Optional) List column of covariate names
#' @param input_data `[data.frame]` Preprocessed spectral data with columns:
#'   * `Sample_ID`: Unique sample identifiers
#'   * `Response`: Target variable (will be renamed from variable param)
#'   * Numeric columns: Wavenumbers (e.g., `"600"`, `"602"`, `"604"`)
#' @param covariate_data `[data.frame]` (Optional) Predicted covariate data with `Sample_ID`
#'   and covariate columns. Default: `NULL`.
#' @param variable `[character]` Name of response variable column in input_data.
#' @param output_dir `[character]` (Optional) Output directory path.
#'   Default: Auto-generated with timestamp.
#' @param grid_size `[integer]` Grid search combinations per model. Default: `10`.
#' @param bayesian_iter `[integer]` Bayesian optimization iterations. Default: `15`.
#' @param cv_folds `[integer]` Cross-validation folds. Default: `10`.
#' @param allow_par `[logical]` Use parallel processing for CV? Default: `TRUE`. Uses future::plan(multisession) for local runs.
#' @param n_cv_cores `[integer]` (Optional) Cores for parallel CV.
#'   Default: `parallel::detectCores() - 2`.
#' @param prune_models `[logical]` Prune models that don't beat baseline? Default: `TRUE`.
#' @param prune_threshold `[numeric]` Performance threshold vs baseline (0-1). Default: `0.9`.
#' @param seed `[integer]` Random seed for reproducibility. Default: `307`.
#' @param resume `[logical]` Resume from existing checkpoint files? Default: `TRUE`.
#' @param verbose `[logical]` Print progress messages? Default: `TRUE`.
#'
#' @return A `[tibble]` with evaluation results containing:
#'   * `config_id`: Configuration identifier (e.g., "001_rf_Log_SNV_PCA_ph")
#'   * `workflow_id`: Human-readable model description
#'   * `model`, `preprocessing`, `transformation`, `feature_selection`: Configuration details
#'   * `covariates`: List column of covariate names used
#'   * `rsq`, `rmse`, `rrmse`, `ccc`, `rpd`, `mae`: Performance metrics on test set
#'   * `status`: Result status ("success", "failed", "pruned")
#'   * `elapsed_secs`: Total computation time per model
#'   * `best_params`: List column with optimal hyperparameters
#'   * `error_message`: Error details for failed models
#'
#' @section Details:
#' The evaluation pipeline executes these stages for each configuration:
#' 1. **Recipe building**: Applies spectral preprocessing and feature selection
#' 2. **Model specification**: Defines model with tunable hyperparameters
#' 3. **Grid search**: Latin hypercube sampling across parameter space
#' 4. **Bayesian optimization**: Refines best grid search results
#' 5. **Final fitting**: Trains on full training set, evaluates on test set
#' 6. **Checkpointing**: Saves results for resume capability
#'
#' Models are processed sequentially to avoid memory issues with large spectral datasets.
#' Cross-validation within each model can be parallelized for faster tuning.
#'
#' @section Warning:
#' - Memory usage scales with spectral data size and number of features
#' - Large configuration grids (>100 models) may require hours to complete
#' - Parallel CV may conflict with system-level parallelization
#' - Results depend on random seed for reproducible comparisons
#'
#' @examples
#' # Basic evaluation workflow
#' configs <- create_configs(models = c("random_forest", "cubist"))
#'
#' evaluate_models_local(config      = configs,
#'                       input_data  = spectral_data,
#'                       variable    = "SOC",
#'                       grid_size   = 5,
#'                       verbose     = TRUE) -> results
#'
#' # With covariates and custom settings
#' evaluate_models_local(config           = extended_configs,
#'                       input_data       = preprocessed_spectra,
#'                       covariate_data   = climate_data,
#'                       variable         = "clay",
#'                       bayesian_iter    = 20,
#'                       prune_threshold  = 0.8,
#'                       n_cv_cores       = 4) -> detailed_results
#'
#' \dontrun{
#' # Large-scale evaluation with error handling
#' safely_execute(expr = {
#'   evaluate_models_local(config      = full_config_grid,
#'                         input_data  = large_dataset,
#'                         variable    = "SOC",
#'                         allow_par   = TRUE,
#'                         verbose     = TRUE)
#' },
#' default_value = NULL,
#' error_message = "Model evaluation failed") -> eval_safe
#'
#' results <- eval_safe$result
#' }
#'
#' @seealso
#' * [evaluate_models_hpc()] for HPC cluster evaluation
#' * [create_configs()] to generate configuration grids
#' * [finalize_top_workflows()] for ensemble model creation
#' * `vignette("model-evaluation")` for complete workflow guide
#'
#' @export

evaluate_models_local <- function(config,
                                  input_data,
                                  covariate_data  = NULL,
                                  variable,
                                  output_dir      = NULL,
                                  grid_size       = 10,
                                  bayesian_iter   = 15,
                                  cv_folds        = 10,
                                  allow_par       = TRUE,
                                  n_cv_cores      = NULL, #TODO: Change to n_cores
                                  prune_models    = TRUE,
                                  prune_threshold = 0.9,
                                  seed            = 307,
                                  resume          = TRUE,
                                  verbose         = TRUE) {



  ## ---------------------------------------------------------------------------
  ## Step 0: Setup
  ## ---------------------------------------------------------------------------

  set.seed(seed)

   ## ---------------------------------------------------------------------------
   ## Step 0.1: Input validation
   ## ---------------------------------------------------------------------------

   ## Check that the config is the right format ---------------------------------

   if (!is.data.frame(config)) cli::cli_abort("{.val {config}} must be a data frame")

   ## Check that the configuration has the required components ------------------

   required_cols <- c("model", "preprocessing", "transformation", "feature_selection")
   missing_cols  <- setdiff(required_cols, names(config))

   if (length(missing_cols) > 0) cli::cli_abort("config missing required columns: {.val {missing_cols}}")

   ## Make sure the response variable is actually in the data -------------------

   if (!variable %in% names(input_data)) cli::cli_abort("Response variable '{variable}' not found in input_data")

   ## Check there's enough response variable to make sense to run ---------------

   response_vals <- input_data[[variable]]
   n_valid       <- sum(!is.na(response_vals))

   if (n_valid < 20) cli::cli_abort("Response variable has only {n_valid} non-missing values (minimum 20 required, > 100 recommended).")

   ## Check that the response variable is a constant ----------------------------

   if (var(response_vals, na.rm = TRUE) < .Machine$double.eps) cli::cli_abort("Response variable has no variation (all values are the same)")

   ## Check that the spectra are in the right place -----------------------------

   spectral_cols <- grep("^[0-9]{3,4}(\\.[0-9]+)?$", names(input_data), value = TRUE)

   if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns found in input_data (expected wavenumber column names)")

   ## Check that the covariate data is set up and contains the data -------------

   if (!is.null(covariate_data)) {

     if (!"Sample_ID" %in% names(input_data) || !"Sample_ID" %in% names(covariate_data)) cli::cli_abort("Both input_data and covariate_data must have 'Sample_ID' column")

     missing_samples <- setdiff(input_data$Sample_ID, covariate_data$Sample_ID)

     if (length(missing_samples) > 0) cli::cli_alert_warning("Warning: {length(missing_samples)} samples in input_data missing from covariate_data")

   }

   ## Define the number of models -----------------------------------------------

   n_models <- nrow(config)

   ## ---------------------------------------------------------------------------
   ## Step 0.2: Build out results directory structure
   ## ---------------------------------------------------------------------------

   ## Create a general run directory --------------------------------------------

   if (is.null(output_dir)) {

     timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
     output_dir <- fs::path(variable, paste0("local_eval_", timestamp))

   }

   ## Create directories for each output type -----------------------------------

   fs::dir_create(output_dir, recurse = TRUE)
   fs::dir_create(fs::path(output_dir, "results"))
   fs::dir_create(fs::path(output_dir, "errors"))
   fs::dir_create(fs::path(output_dir, "summary"))

   ## Report general setup for the run ------------------------------------------

   if (verbose) {

     cli::cli_h1("Model Evaluation Setup")
     cli::cli_div(theme = list(rule = list(`line-type` = "single")))
     cli::cli_h2("Configuration")
     cli::cli_alert_success("Models: {n_models} configurations")
     cli::cli_alert_info("Strategy: Sequential models, {ifelse(allow_par, 'parallel', 'sequential')} CV{ifelse(allow_par, paste0(' (', ifelse(is.null(n_cv_cores), parallel::detectCores() - 2, n_cv_cores), ' cores)'), '')}")
     cli::cli_alert_info("Output: {.path {output_dir}}")

   }

  ## ---------------------------------------------------------------------------
  ## Step 1: Split the input data
  ## ---------------------------------------------------------------------------

  ## Rename response variable to "Response" -------------------------------------

  input_data %>%
    dplyr::rename(Response = !!rlang::sym(variable)) -> input_data

  ## Update variable name for downstream use -----------------------------------

  variable <- "Response"

  ## Split the input data, stratified by response ------------------------------

  ##TODO: Make split proportion configurable

  rsample::initial_split(data   = input_data,
                         prop   = 0.8,
                         strata = dplyr::all_of(variable)) -> data_split

  ## Report out the data split -------------------------------------------------

  if (verbose) {

    train_n <- nrow(rsample::training(data_split))
    test_n  <- nrow(rsample::testing(data_split))
    cli::cli_alert_success("Data: {train_n} training, {test_n} testing samples")
    cli::cli_text("")
    cli::cli_alert_info("Starting evaluation...")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Initialize Progress Tracking and Checkpointing
  ## ---------------------------------------------------------------------------

  all_results <- list()
  start_time  <- Sys.time()
  model_times <- numeric(n_models)

  ## ---------------------------------------------------------------------------

  existing_results <- character()

  if (resume) {

    ## Check for existing files in the results directory -----------------------

    fs::dir_ls(fs::path(output_dir, "results"),
               glob = "*.qs",
               type = "file") -> result_files

    ## If files exist, append them ---------------------------------------------

    if (length(result_files) > 0) {

      existing_results <- fs::path_file(result_files)
      existing_results <- gsub("\\.qs$", "", existing_results)


      if (verbose && length(existing_results) > 0) cli::cli_alert_info("Found {length(existing_results)} existing result{?s}, will skip")

    }
  }

  ## ---------------------------------------------------------------------------
  ## Step ??: Setup Parallel Backend (if requested)
  ## ---------------------------------------------------------------------------

  if (allow_par && !is.null(n_cv_cores) && n_cv_cores > 1) {

    options(mc.cores = n_cv_cores)

    old_plan <- future::plan()

    ## Store and increase globals size limit for large spectral data
    old_maxSize <- getOption("future.globals.maxSize")
    options(future.globals.maxSize = 2 * 1024^3)  # Set to 2GB for spectral workflows

    on.exit({
      future::plan(old_plan)
      options(future.globals.maxSize = old_maxSize)  # Restore original limit
    }, add = TRUE)

    future::plan(future::multisession, workers = n_cv_cores)

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Initiate model evaluation loop
  ## ---------------------------------------------------------------------------

  best_ccc   <- NA_real_
  best_model <- NA_character_

  for (i in seq_len(n_models)) {

    ## -------------------------------------------------------------------------
    ## Step 4: Setup the configuration and timing for this iteration
    ## -------------------------------------------------------------------------

    config_row  <- config[i, , drop = FALSE]
    model_start <- Sys.time()

    ## Pull the covariate names in ---------------------------------------------

    covariate_cols <- if ("covariates" %in% names(config_row) && !is.null(config_row$covariates[[1]])) {

      config_row$covariates[[1]]

      } else {

        NULL

    }

    ## Create a clean workflow id ----------------------------------------------

    clean_workflow_id(model             = config_row$model,
                      transformation    = config_row$transformation,
                      preprocessing     = config_row$preprocessing,
                      feature_selection = config_row$feature_selection,
                      covariates        = covariate_cols)  -> workflow_id

    ## -------------------------------------------------------------------------
    ## Step 5: Check for existing results
    ## -------------------------------------------------------------------------

    ## Read the existing results for this config, if it exists -----------------

    result_file <- fs::path(output_dir, "results", paste0(workflow_id, ".qs"))

    if (resume && fs::file_exists(result_file)) {

      if (verbose) cli::cli_alert_info("[{i}/{n_models}] Skipping {workflow_id} (already completed)")

      ## Rerun if the file is corrupted ----------------------------------------

      tryCatch({

        qs::qread(result_file)

        }, error = function(e) {

           cli::cli_alert_warning("Could not load existing result file {workflow_id}.qs: {conditionMessage(e)}")
           cli::cli_alert_info("Will re-run this configuration")

           NULL

      }) -> existing_result

      ## If load is successful, add it to the results --------------------------

      if (!is.null(existing_result)) {

        all_results[[i]] <- existing_result

        # Update time estimate with saved result -------------------------------

        if (!is.null(existing_result$timing$elapsed)) model_times[i] <- existing_result$timing$elapsed / 60

        ## Move on to the next iteration ---------------------------------------

        next
      }
    }

    ## -------------------------------------------------------------------------
    ## Step 6: Update the user as to this model config
    ## -------------------------------------------------------------------------

    if (verbose) {

      cli::cli_text("")
      cli::cli_rule(left = "Model Configuration {i}/{n_models}")

      # Create display names directly from config row (no parsing needed) ------

      model_display      <- get_readable_model_name(config_row$model)
      transform_display  <- get_readable_transformation_name(config_row$transformation)
      preprocess_display <- get_readable_preprocessing_name(config_row$preprocessing)
      feature_display    <- get_readable_feature_selection_name(config_row$feature_selection)
      covariates_display <- create_readable_covariates_string(covariate_cols)

      # Display the components with human-readable names
      cli::cli_text("├─ Model: {.field {model_display}}")
      cli::cli_text("├─ Transformation: {.field {transform_display}}")
      cli::cli_text("├─ Preprocessing: {.field {preprocess_display}}")
      cli::cli_text("├─ Feature Selection: {.field {feature_display}}")
      cli::cli_text("└─ Covariates: {.field {covariates_display}}")
      cli::cli_text("")
    }

    ## -------------------------------------------------------------------------
    ## Step 7: Set up config specific covariate data
    ## -------------------------------------------------------------------------

    # Create config_clean for error reporting
    config_clean <- list(
      model             = as.character(config_row$model),
      transformation    = as.character(config_row$transformation),
      preprocessing     = as.character(config_row$preprocessing),
      feature_selection = as.character(config_row$feature_selection),
      covariates        = covariate_cols
    )

    ## Make sure that all of the requested covariates for this config exist ----

    if (!is.null(covariate_cols) && !is.null(covariate_data)) {

      missing_covs <- setdiff(covariate_cols, names(covariate_data))

      ## If the covariate data doesn't exist, fail right away ------------------

      if (length(missing_covs) > 0) {

        ## Log the time --------------------------------------------------------

        model_times[i] <- as.numeric(difftime(Sys.time(), model_start, units = "mins"))

        ## Create the json file path -------------------------------------------

        error_file <-  fs::path(output_dir, "errors", paste0(workflow_id, ".json"))

        ## Update the user -----------------------------------------------------

        cli::cli_text("")
        cli::cli_text("{.strong Results:}")
        cli::cli_text("├─ Status: {cli::col_red('✗ FAILED')}")
        cli::cli_text("├─ Error level: local-orchestrator")
        cli::cli_text("├─ Error stage: Covariate data assembly (Step 7)")
        cli::cli_text("├─ Error message: Covariate data requested but not found for {missing_covs}")
        cli::cli_text("├─ Error log saved to: {error_file}")
        cli::cli_text("└─ Total time: {round(model_times[i], 1)} min")
        cli::cli_rule()

        ## Create a failed result entry ----------------------------------------

        create_failed_result(config_id     = workflow_id,
                                    config_clean  = config_clean,
                                    error_message = glue::glue("Covariate data requested but not found for {missing_covs}"),
                                    workflow_id   = workflow_id,
                                    error_detail  = NULL,
        #TODO: Add this             error_level   = "local-orchestrator",
                                    error_stage   = "Covariate data assembly (Step 7)",
                                    error_trace   = NULL,
                                    warnings      = NULL,
                                    messages      = NULL) -> failed_result

        ## Log the results -----------------------------------------------------

        all_results[[i]] <-  failed_result

        ## Create a structured error file --------------------------------------

        list(workflow_id   = workflow_id,
             config        = as.list(config_row),
           # error_level   = result$error_level,
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

        }, error = function(e){

          if (file.exists(temp_error)) unlink(temp_error)

        })

      next

      }

      ## If the covariates do exist, subset the relevant columns ---------------

      model_covariate_data <- covariate_data[, c("Sample_ID", covariate_cols), drop = FALSE]

      } else {

      ## If the configuration doesn't call for covariates, it's a moot point ---

      model_covariate_data <- NULL

    }

    ## -------------------------------------------------------------------------
    ## Step 8: Run the model (woohoo!)
    ## -------------------------------------------------------------------------

    evaluate_configuration(config_row      = config_row,
                           input_data      = input_data,
                           data_split      = data_split,
                           config_id       = workflow_id,
                           covariate_data  = model_covariate_data,
                           variable        = variable,
                           output_dir      = output_dir,
                           grid_size       = grid_size,
                           bayesian_iter   = bayesian_iter,
                           cv_folds        = cv_folds,
                           allow_par       = allow_par,
                           n_cv_cores      = n_cv_cores,
                           prune_models    = prune_models,
                           prune_threshold = prune_threshold,
                           verbose         = verbose) -> result

    ## -------------------------------------------------------------------------
    ## Step 9: Process the results and save checkpoint files
    ## -------------------------------------------------------------------------

    ## Save the results to the checkpoint file ---------------------------------

    result_file <- fs::path(output_dir, "results", paste0(workflow_id, ".qs"))
    temp_file   <- tempfile(tmpdir = dirname(result_file), fileext = ".qs.tmp")

    tryCatch({

      qs::qsave(result, temp_file)
      file.rename(temp_file, result_file)

    }, error = function(e) {

      if (file.exists(temp_file)) unlink(temp_file)
      cli::cli_alert_warning("Failed to save checkpoint for {workflow_id}: {e$message}")

    })

      ## -----------------------------------------------------------------------
      ## Step 9.1: If the model fails... :(
      ## -----------------------------------------------------------------------

      if(result$status == "failed"){

        ## Log the time --------------------------------------------------------

        model_times[i] <- as.numeric(difftime(Sys.time(), model_start, units = "mins"))

        ## Log the results -----------------------------------------------------

        all_results[[i]] <-  result

        ## Create a structured error file --------------------------------------

        list(workflow_id   = workflow_id,
             config        = as.list(config_row),
           # error_level   = result$error_level,
             error_stage   = result$error_stage,
             error_class   = result$error_class,
             error_message = result$error_message,
             has_trace     = result$has_trace,
             n_warnings    = result$n_warnings,
             warnings      = result$warnings %||% NULL,
             messages      = result$messages %||% NULL,
             total_seconds = result$total_seconds,
             timestamp     = as.character(Sys.time())) -> error_object

        ## Create the json file path -------------------------------------------

        error_file <-  fs::path(output_dir, "errors", paste0(workflow_id, ".json"))

        ## Save the error file using an atomic write ---------------------------

        temp_error <- tempfile(tmpdir = dirname(error_file), fileext = ".json.tmp")

        tryCatch({

          jsonlite::write_json(x          = error_object,
                               path       = temp_error,
                               pretty     = TRUE,
                               auto_unbox = TRUE)

          file.rename(temp_error, error_file)

        }, error = function(e){

          if (file.exists(temp_error)) unlink(temp_error)

        })

        if(verbose){

          ## Grab the error message and shorten if needed ------------------------

          error_msg     <- result$error_message %||% "Ghost in the machine (i.e., unknown error)."
          error_preview <- if(nchar(error_msg) > 50) {

            paste0(substr(error_msg, 1, 50), "...")

            } else {

            error_msg

            }
        }

          ## Grab the error stage and level --------------------------------------

          error_stage <- result$error_stage
        # error_level <- result$error_level

        ## Return message to user ----------------------------------------------

        if(verbose){

            cli::cli_text("")
            cli::cli_text("{.strong Results:}")
            cli::cli_text("├─ Status: {cli::col_red('✗ FAILED')}")
      #     cli::cli_text("├─ Error level: {error_level}")
            cli::cli_text("├─ Error stage: {error_stage}")
            cli::cli_text("├─ Error message: {error_preview}")
            cli::cli_text("├─ Error log saved to: {error_file}")
            cli::cli_text("└─ Total time: {round(model_times[i], 1)} min")
            cli::cli_rule()
        }


          ## Clean things up -----------------------------------------------------

          invisible(gc(verbose = FALSE, full = TRUE))

      }

      ## -----------------------------------------------------------------------
      ## Step 9.2: If the model succeeds! (Yay!)
      ## -----------------------------------------------------------------------

      if(result$status == "success"){

        ## Store result ----------------------------------------------------------

        all_results[[i]] <- result

        ## Track model timing ----------------------------------------------------

        model_times[i] <- as.numeric(difftime(Sys.time(), model_start, units = "mins"))

        # Update best model tracking ---------------------------------------------

        current_ccc <- result$ccc

        if (!is.na(current_ccc) && (is.na(best_ccc) || current_ccc > best_ccc)) {

          best_ccc   <- current_ccc
          best_model <- workflow_id

        }

        ## Provide and update to the useer -------------------------------------

        if (verbose) {

          cli::cli_text("")
          cli::cli_text("{.strong Results:}")
          cli::cli_text("├─ Status: {cli::col_green('✓ SUCCESS')}")
          cli::cli_text("├─ RRMSE: {round(result$rrmse, 1)}% | R²: {round(result$rsq, 3)}")
          cli::cli_text("├─ CCC: {round(result$ccc, 3)} | RPD: {round(result$rpd, 2)}")
          cli::cli_text("└─ Total time: {round(model_times[i], 1)} min")
          cli::cli_rule()

        }

        ## Clean up ------------------------------------------------------------

        invisible(gc(verbose = FALSE, full = TRUE))

      }

      ## -----------------------------------------------------------------------
      ## Step 9.3: Optional if pruned case
      ## -----------------------------------------------------------------------

      if(result$status == "pruned") {

        ## Store result ----------------------------------------------------------

        all_results[[i]] <- result

        ## Track model timing ----------------------------------------------------

        model_times[i] <- as.numeric(difftime(Sys.time(), model_start, units = "mins"))

        ## Provide and update to the useer -------------------------------------

        if (verbose) {

          actual_threshold <- round(prune_threshold * 100, 1)
          cli::cli_text("")
          cli::cli_text("{.strong Results:}")
          cli::cli_text("├─ Status: {cli::col_yellow('⚠ PRUNED')}")
          cli::cli_text("├─ RRMSE: {round(result$rrmse, 1)}% (threshold: {actual_threshold}%)")
          cli::cli_text("└─ Total time: {round(model_times[i], 1)} min")
          cli::cli_rule()

        }

        ## Clean up ------------------------------------------------------------

        invisible(gc(verbose = FALSE, full = TRUE))

      }

    ## ---------------------------------------------------------------------------
    ## Step 10: Progress tracking
    ## ---------------------------------------------------------------------------

    if (verbose && (i %% 10 == 0 || i <= 5)) {

      ## Count statuses from results so far --------------------------------------

      statuses         <- sapply(all_results[1:i], function(x) x$status %||% "unknown")
      n_success_so_far <- sum(statuses %in% c("success"))
      n_pruned_so_far  <- sum(statuses == "pruned")
      n_failed_so_far  <- sum(statuses == "failed")

      ## Create progress bar -----------------------------------------------------

      pct_complete <- round(i / n_models * 100)
      bar_width    <- 20
      filled       <- round(bar_width * i / n_models)
      empty        <- bar_width - filled
      progress_bar <- paste0(strrep("█", filled), strrep("░", empty))

      ## ETA calculation -------------------------------------------------------

      completed_times <- model_times[1:i][!is.na(model_times[1:i]) & model_times[1:i] > 0]

      eta_text <- ""

      if (length(completed_times) > 0) {

        avg_time  <- mean(completed_times)
        remaining <- n_models - i
        eta_mins  <- round(remaining * avg_time, 0)
        eta_text  <- paste0(" | ~", eta_mins, " min remaining")

      }

    ## Return progress to user -------------------------------------------------

    cli::cli_text("")
    cli::cli_text("Progress: {progress_bar} {pct_complete}% | {n_success_so_far} complete, {n_pruned_so_far} pruned, {n_failed_so_far} failed{eta_text}")


    ## Show best model so far --------------------------------------------------

      if (!is.na(best_ccc)) {

        cli::cli_text("          Best CCC: {round(best_ccc, 3)} ({best_model})")

      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 12: Save results and prepare output
  ## ---------------------------------------------------------------------------

  ## Simple aggregation --------------------------------------------------------

  # Ensure all results have consistent column structure before binding
  standardized_results <- lapply(all_results, function(result) {
    # Add missing columns with NULL defaults if they don't exist
    if (!"warnings" %in% names(result)) result$warnings <- list(NULL)
    if (!"messages" %in% names(result)) result$messages <- list(NULL)
    if (!"warning_summary" %in% names(result)) result$warning_summary <- NA_character_
    result
  })

  results <- dplyr::bind_rows(standardized_results)

  ## Save final aggregated results using atomic writes -------------------------

  summary_file_qs <- fs::path(output_dir, "summary", "final_results.qs")
  temp_qs         <- tempfile(tmpdir = dirname(summary_file_qs), fileext = ".qs.tmp")

  tryCatch({

    qs::qsave(results, temp_qs)
    file.rename(temp_qs, summary_file_qs)

  }, error = function(e) {

      if (file.exists(temp_qs)) unlink(temp_qs)
      cli::cli_alert_warning("Failed to save QS summary: {e$message}")

  })

  ## Also save as a csv --------------------------------------------------------

  summary_file_csv <- fs::path(output_dir, "summary", "final_results.csv")
  temp_csv         <- tempfile(tmpdir = dirname(summary_file_csv), fileext = ".csv.tmp")

  tryCatch({

    readr::write_csv(results, temp_csv)
    file.rename(temp_csv, summary_file_csv)

  }, error = function(e) {

    if (file.exists(temp_csv)) unlink(temp_csv)
    cli::cli_alert_warning("Failed to save CSV summary: {e$message}")

  })

  ## Collect metadata about the run --------------------------------------------

  n_completed <- sum(results$status == "success", na.rm = TRUE)
  n_pruned    <- sum(results$status == "pruned", na.rm = TRUE)
  n_errors    <- sum(results$status == "failed", na.rm = TRUE)

  list(n_models           = n_models,
       n_completed        = n_completed,
       n_pruned           = n_pruned,
       n_errors           = n_errors,
       total_time_mins    = as.numeric(difftime(Sys.time(), start_time, units = "mins")),
       avg_time_per_model = mean(model_times[model_times > 0], na.rm = TRUE),
       parameters         = list(variable        = variable,
                                 grid_size       = grid_size,
                                 bayesian_iter   = bayesian_iter,
                                 cv_folds        = cv_folds,
                                 allow_par       = allow_par,
                                 prune_models    = prune_models,
                                 prune_threshold = prune_threshold,
                                 seed = seed),
       timestamp          = Sys.time()) -> metadata

  ## Save metadata using atomic write ------------------------------------------

  metadata_file <- fs::path(output_dir, "summary", "run_metadata.json")
  temp_meta     <- tempfile(tmpdir = dirname(metadata_file), fileext = ".json.tmp")

  tryCatch({

    jsonlite::write_json(metadata, temp_meta, pretty = TRUE, auto_unbox = TRUE)
    file.rename(temp_meta, metadata_file)

  }, error = function(e) {

    if (file.exists(temp_meta)) unlink(temp_meta)
    cli::cli_alert_warning("Failed to save metadata: {e$message}")

  })

  ## Calculate summary statistics ----------------------------------------------

  results %>%
    dplyr::filter(status == "success") %>%
    dplyr::arrange(rrmse) %>%
    dplyr::slice_head(n = 5) -> top_models

  ## Do some final reporting ---------------------------------------------------

  if (verbose) {

    cli::cli_h1("Model evaluation complete")

    cli::cli_alert_success("Completed: {n_completed} models")

    if (n_pruned > 0) {

      cli::cli_alert_warning("Pruned: {n_pruned} models (failed baseline)")

    }

    if (n_errors > 0) {

      cli::cli_alert_danger("Failed: {n_errors} models")

    }

    cli::cli_alert_success("Results saved to: {.path {output_dir}}")
    cli::cli_alert_info("Summary: {.path {summary_file_csv}}")



    if (nrow(top_models) > 0) {

      cli::cli_h3("Top 5 Models by RRMSE")

      for (i in 1:nrow(top_models)) {
        m <- top_models[i, ]
        cli::cli_alert_info("{i}. {m$model} | {m$preprocessing} | RRMSE: {round(m$rrmse, 3)} | R²: {round(m$rsq, 3)}")

      }

    }

    cli::cli_alert_success("Total evaluation time: {round(metadata$total_time_mins, 1)} minutes")
  }


  return(results)

}

