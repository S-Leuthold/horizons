#' HPC-Optimized Model Evaluation Orchestrator
#'
#' @description
#' Orchestrates sequential model evaluation with parallel tuning operations optimized
#' for HPC environments with many cores (50-190). Each model configuration is evaluated
#' sequentially, but within each model, grid search and Bayesian optimization leverage
#' all available workers for maximum throughput.
#'
#' This function replaces `run_model_evaluation()` for HPC environments, providing:
#' - Configurable worker allocation (1-190 cores)
#' - Sequential model processing with parallel fits
#' - Work-stealing for efficient resource utilization
#' - Memory-aware processing with minimal overhead
#' - Comprehensive progress tracking and checkpointing
#'
#' @param config A tibble of model configurations to evaluate. Must include columns:
#'   \itemize{
#'     \item `model`: Model type (e.g., "random_forest", "cubist", "xgboost")
#'     \item `transformation`: Response transformation (e.g., "No Transformation", "Log Transformation")
#'     \item `preprocessing`: Spectral preprocessing (e.g., "raw", "snv", "deriv1")
#'     \item `feature_selection`: Feature selection method (e.g., "none", "pca", "boruta", "cars")
#'     \item `covariates`: List-column of covariate sets
#'     \item `include_covariates`: Logical flag for covariate inclusion
#'   }
#' @param input_data A tibble with preprocessed spectral data, including `Sample_ID`,
#'   wavenumber columns, and the target response variable
#' @param covariate_data Optional tibble of predicted covariates matched by `Sample_ID`.
#'   Required if `include_covariates = TRUE` in any row of `config`
#' @param variable Character. Name of the response variable (must exist in `input_data`)
#' @param output_dir Path to output directory. Defaults to timestamped folder under `variable`
#' @param n_workers Integer. Number of parallel workers (1-190). Defaults to 50
#' @param grid_size_eval Integer. Number of combinations in initial grid search (default = 10)
#' @param bayesian_iter_eval Integer. Number of Bayesian optimization iterations (default = 15)
#' @param cv_folds_eval Integer. Number of CV folds during evaluation phase (default = 5)
#' @param retrain_top_models Logical. Whether to refit top N models after screening (default = TRUE)
#' @param number_models_retained Integer. Number of top models to refit if `retrain_top_models = TRUE` (default = 15)
#' @param grid_size_final Integer. Grid size for refitting stage (default = 25)
#' @param bayesian_iter_final Integer. Bayesian iterations during refitting (default = 20)
#' @param cv_folds_final Integer. Number of CV folds for refitting phase (default = 15)
#' @param pruning Logical. Whether to enable early pruning of poor configurations (default = FALSE)
#' @param checkpoint_dir Character. Directory for checkpoint files. If NULL, uses `output_dir/checkpoints/`
#' @param resume Logical. Whether to resume from existing checkpoints (default = TRUE)
#' @param verbose Logical. Print detailed progress messages (default = TRUE)
#'
#' @return If `retrain_top_models = FALSE`, returns a tibble summarizing evaluation metrics
#'   for each configuration. If `TRUE`, returns a list with:
#' \itemize{
#'   \item \strong{full_summary}: A tibble of metrics and metadata from initial evaluation
#'   \item \strong{refit_summary}: A tibble of metrics from the final refitting stage
#' }
#'
#' @details
#' ## Parallel Processing Strategy
#'
#' The orchestrator implements a two-level parallelization strategy optimized for HPC:
#'
#' \strong{Level 1: Sequential Model Processing}
#' - Each model configuration runs sequentially to avoid memory competition
#' - Allows full resource dedication to each model
#' - Simplifies debugging and monitoring
#'
#' \strong{Level 2: Parallel Tuning Operations}
#' - Grid search: Parallelizes across hyperparameter combinations
#' - Bayesian optimization: Parallelizes across CV folds within each iteration
#' - Uses work-stealing for automatic load balancing
#'
#' ## Memory Management
#'
#' - Minimal garbage collection (only when needed)
#' - Automatic cleanup between models
#' - Memory monitoring with configurable thresholds
#' - Efficient data passing to workers
#'
#' ## Checkpointing and Recovery
#'
#' - Saves progress after each model completion
#' - Automatic resume from last successful model
#' - Preserves random seeds for reproducibility
#' - Checkpoint files in node-local storage for speed
#'
#' @examples
#' \dontrun{
#' # Run on HPC with 50 cores
#' results <- run_hpc_evaluation(
#'   config             = model_config_grid,
#'   input_data         = spectral_data,
#'   covariate_data     = predicted_covs,
#'   variable           = "MAOM_C_g_kg",
#'   n_workers          = 50,
#'   grid_size_eval     = 10,
#'   bayesian_iter_eval = 15
#' )
#'
#' # Run with more cores for larger grids
#' results <- run_hpc_evaluation(
#'   config             = large_config_grid,
#'   input_data         = spectral_data,
#'   variable           = "SOC_g_kg",
#'   n_workers          = 190,
#'   grid_size_eval     = 50,
#'   bayesian_iter_eval = 30
#' )
#' }
#'
#' @seealso
#' \code{\link{evaluate_model_fit_parallel}}, \code{\link{manage_worker_pool}},
#' \code{\link{run_model_evaluation}}, \code{\link{evaluate_single_model_parallel}}
#'
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#' @importFrom dplyr bind_rows filter arrange slice mutate
#' @importFrom fs dir_create path
#' @importFrom qs qsave qread
#' @importFrom future plan multicore multisession sequential nbrOfWorkers
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @export

run_hpc_evaluation <- function(config,
                               input_data,
                               covariate_data             = NULL,
                               variable,
                               output_dir                 = NULL,
                               n_workers                  = 50,
                               grid_size_eval             = 10,
                               bayesian_iter_eval         = 15,
                               cv_folds_eval              = 5,
                               retrain_top_models         = TRUE,
                               number_models_retained     = 15,
                               grid_size_final            = 25,
                               bayesian_iter_final        = 20,
                               cv_folds_final             = 15,
                               pruning                    = FALSE,
                               checkpoint_dir             = NULL,
                               resume                     = TRUE,
                               verbose                    = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Validation and Setup
  ## ---------------------------------------------------------------------------

  start_time <- Sys.time()

  ## Validate inputs -----------------------------------------------------------

  if (!is.data.frame(config) || nrow(config) == 0) {
    cli::cli_abort("config must be a non-empty data frame")
  }

  if (!variable %in% colnames(input_data)) {
    cli::cli_abort("Variable '{variable}' not found in input_data")
  }

  ## Set up parallel environment -----------------------------------------------

  Sys.setenv(HORIZONS_PARALLEL_LEVEL = "0")  # Mark as top-level orchestrator

  ## Determine optimal worker count --------------------------------------------

  available_cores <- parallel::detectCores(logical = TRUE)
  n_workers       <- min(n_workers, available_cores - 2, nrow(config) * grid_size_eval * cv_folds_eval)
  n_workers       <- max(1, n_workers)  # Ensure at least 1 worker

  ## Set up output directory ---------------------------------------------------

  if (is.null(output_dir)) {
    output_dir <- paste0(variable, "_hpc_outputs_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  fs::dir_create(output_dir)

  ## Set up checkpoint directory -----------------------------------------------

  if (is.null(checkpoint_dir)) {
    checkpoint_dir <- fs::path(output_dir, "checkpoints")
  }

  fs::dir_create(checkpoint_dir)

  ## ---------------------------------------------------------------------------
  ## Step 1: Display Configuration
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_h1("HPC Model Evaluation Orchestrator")
    cli::cli_alert_info("Models to evaluate: {nrow(config)}")
    cli::cli_alert_info("Workers available: {n_workers} (of {available_cores} total cores)")
    cli::cli_alert_info("Variable: {variable}")
    cli::cli_alert_info("Output directory: {output_dir}")
    cli::cli_alert_info("Checkpoint directory: {checkpoint_dir}")
    cli::cli_alert_info("System: {Sys.info()['sysname']}")

    ## Memory information ------------------------------------------------------
    
    if (Sys.info()["sysname"] == "Linux") {
      tryCatch({
        mem_info <- system2("free", args = c("-h"), stdout = TRUE)
        mem_line <- grep("^Mem:", mem_info, value = TRUE)
        if (length(mem_line) > 0) {
          cli::cli_alert_info("System memory: {mem_line}")
        }
      }, error = function(e) {
        cli::cli_alert_info("Memory info unavailable")
      })
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Check for Existing Checkpoints
  ## ---------------------------------------------------------------------------

  completed_models <- integer(0)
  checkpoint_file  <- fs::path(checkpoint_dir, "completed_models.qs")

  if (resume && file.exists(checkpoint_file)) {

    completed_models <- qs::qread(checkpoint_file)

    if (verbose && length(completed_models) > 0) {
      cli::cli_alert_success("Resuming from checkpoint: {length(completed_models)} models already completed")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Initialize Results Storage
  ## ---------------------------------------------------------------------------

  n_models      <- nrow(config)
  all_results   <- vector("list", length = n_models)
  summary_rows  <- vector("list", length = n_models)
  timing_log    <- numeric(n_models)

  ## Load existing results if resuming ----------------------------------------

  if (length(completed_models) > 0) {

    for (idx in completed_models) {

      result_file <- fs::path(checkpoint_dir, glue::glue("model_{idx}_result.qs"))

      if (file.exists(result_file)) {
        all_results[[idx]] <- qs::qread(result_file)
        summary_rows[[idx]] <- all_results[[idx]]$status_summary
      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Sequential Model Processing with Parallel Tuning
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_h2("Starting Sequential Model Evaluation")
  }

  for (i in seq_len(n_models)) {

    ## Skip if already completed ---------------------------------------------

    if (i %in% completed_models) {
      if (verbose) {
        cli::cli_alert_info("Model {i}/{n_models}: Already completed, skipping")
      }
      next
    }

    ## Extract configuration -------------------------------------------------

    config_row <- config[i, , drop = FALSE]
    model_start_time <- Sys.time()

    ## Display progress ------------------------------------------------------

    if (verbose) {

      config_desc <- clean_workflow_id(
        model             = config_row$model,
        transformation    = config_row$transformation,
        preprocessing     = config_row$preprocessing,
        feature_selection = config_row$feature_selection,
        covariates        = config_row$covariates
      )

      cli::cli_h2("Model {i}/{n_models}: {config_desc}")
    }

    ## Run model with parallel tuning ----------------------------------------

    result <- evaluate_model_fit_parallel(
      config_row         = config_row,
      input_data         = input_data,
      covariate_data     = covariate_data,
      variable           = variable,
      row_index          = i,
      output_dir         = output_dir,
      n_workers          = n_workers,
      grid_size          = grid_size_eval,
      bayesian_iter      = bayesian_iter_eval,
      cv_folds           = cv_folds_eval,
      pruning            = pruning,
      save_output        = FALSE,
      verbose            = verbose
    )

    ## Store results ---------------------------------------------------------

    all_results[[i]]  <- result
    summary_rows[[i]] <- result$status_summary

    ## Save checkpoint using batched system ----------------------------------
    
    save_checkpoint_batch(
      result         = result,
      model_index    = i,
      checkpoint_dir = checkpoint_dir,
      batch_size     = 100  # 100 models per file
    )
    
    completed_models <- c(completed_models, i)

    ## Calculate timing and ETA ----------------------------------------------

    model_end_time <- Sys.time()
    model_duration <- as.numeric(difftime(model_end_time, model_start_time, units = "mins"))
    timing_log[i]  <- model_duration

    ## Memory cleanup --------------------------------------------------------

    gc(verbose = FALSE, full = FALSE)  # Single lightweight GC

    ## Report progress -------------------------------------------------------

    if (verbose) {

      ## Memory usage --------------------------------------------------------
      
      mem_used_gb <- as.numeric(pryr::mem_used()) / 1073741824
      
      ## Adaptive thresholds based on 755GB total memory ---------------------
      
      mem_warning_threshold <- 113  # 15% of 755GB
      mem_danger_threshold  <- 302  # 40% of 755GB
      
      if (mem_used_gb < mem_warning_threshold) {
        cli::cli_alert_success("Memory usage: {round(mem_used_gb, 1)} GB")
      } else if (mem_used_gb < mem_danger_threshold) {
        cli::cli_alert_warning("Memory usage: {round(mem_used_gb, 1)} GB")
      } else {
        cli::cli_alert_danger("High memory usage: {round(mem_used_gb, 1)} GB")
      }

      ## Timing and ETA ------------------------------------------------------

      cli::cli_alert_success("Model completed in {round(model_duration, 2)} minutes")

      if (i %% 5 == 0 || i <= 5) {  # Report ETA every 5 models or for first 5

        completed_count  <- sum(!is.na(timing_log))
        mean_duration    <- mean(timing_log[!is.na(timing_log)])
        remaining_models <- n_models - i
        eta_minutes      <- remaining_models * mean_duration
        eta_time         <- Sys.time() + (eta_minutes * 60)

        cli::cli_alert_info("Average time per model: {round(mean_duration, 2)} minutes")
        cli::cli_alert_info("Estimated completion: {format(eta_time, '%Y-%m-%d %H:%M:%S')}")
      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Compile Results Summary
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_h2("Compiling Results")
  }

  summary_tbl <- dplyr::bind_rows(summary_rows)

  ## Save summary --------------------------------------------------------------

  timestamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
  summary_file <- fs::path(output_dir, glue::glue("hpc_summary_{variable}_{timestamp}.qs"))
  qs::qsave(summary_tbl, summary_file)

  if (verbose) {
    cli::cli_alert_success("Summary saved to: {summary_file}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Retrain Top Models (if requested)
  ## ---------------------------------------------------------------------------

  if (isTRUE(retrain_top_models)) {

    if (verbose) {
      cli::cli_h1("Retraining Top {number_models_retained} Models")
    }

    ## Filter and rank top models --------------------------------------------

    summary_tbl %>%
      filter(status == "success", !is.na(rrmse)) %>%
      arrange(rrmse) %>%
      dplyr::slice(1:min(number_models_retained, n())) -> top_models

    if (nrow(top_models) == 0) {
      cli::cli_alert_warning("No successful models to retrain")

      return(list(
        full_summary  = summary_tbl,
        refit_summary = tibble::tibble()
      ))
    }

    ## Initialize refit storage ----------------------------------------------

    refit_results <- vector("list", length = nrow(top_models))
    refit_summary <- vector("list", length = nrow(top_models))

    ## Retrain top models ----------------------------------------------------

    for (j in seq_len(nrow(top_models))) {

      refit_start_time <- Sys.time()

      idx        <- top_models$row[j]
      config_row <- config[idx, , drop = FALSE]

      if (verbose) {
        cli::cli_h2("Refitting model {j}/{nrow(top_models)}: {top_models$wflow_id[j]}")
      }

      ## Run refitting with higher resolution --------------------------------

      result <- evaluate_model_fit_parallel(
        config_row     = config_row,
        input_data     = input_data,
        covariate_data = covariate_data,
        variable       = variable,
        row_index      = idx,
        output_dir     = output_dir,
        n_workers      = n_workers,
        grid_size      = grid_size_final,
        bayesian_iter  = bayesian_iter_final,
        cv_folds       = cv_folds_final,
        pruning        = FALSE,
        save_output    = TRUE,
        verbose        = FALSE  # Less verbose for refits
      )

      refit_results[[j]] <- result
      refit_summary[[j]] <- result$status_summary

      ## Report progress -----------------------------------------------------

      refit_duration <- as.numeric(difftime(Sys.time(), refit_start_time, units = "mins"))

      if (verbose) {
        cli::cli_alert_success("Refit completed in {round(refit_duration, 2)} minutes")
      }

      ## Memory cleanup ------------------------------------------------------

      gc(verbose = FALSE, full = FALSE)
    }

    ## Compile refit results -------------------------------------------------

    refit_tbl <- dplyr::bind_rows(refit_summary)

    refit_file <- fs::path(output_dir, glue::glue("refit_summary_{variable}_{timestamp}.qs"))
    qs::qsave(refit_tbl, refit_file)

    if (verbose) {
      cli::cli_alert_success("Refit summary saved to: {refit_file}")
    }

    ## Return both summaries -------------------------------------------------

    total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "hours"))

    if (verbose) {
      cli::cli_h1("HPC evaluation completed in {round(total_duration, 2)} hours")
    }

    return(list(
      full_summary  = summary_tbl,
      refit_summary = refit_tbl
    ))

  } else {

    ## Return summary only ---------------------------------------------------

    total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "hours"))

    if (verbose) {
      cli::cli_h1("HPC evaluation completed in {round(total_duration, 2)} hours")
    }

    return(summary_tbl)
  }
}

