#' Run Full Batch Model Evaluation Across Configuration Grid
#'
#' Executes ensemble modeling workflows for a grid of model configurations, applying
#' grid + Bayesian tuning, evaluation, pruning, and optional refitting of top-performing models.
#' Each model run is isolated and fault-tolerant, using `safe_run_model()` for robust logging and
#' `prune_model_output()` to minimize memory. Supports caching, parallel execution, and ETA tracking.
#'
#' @param config A `tibble` of model configurations to evaluate. Must include columns:
#'   \itemize{
#'     \item `model`: Name of model (e.g., `"PLSR"`, `"Cubist"`).
#'     \item `transformation`: Outcome transformation label (e.g., `"Log Transformation"`).
#'     \item `preprocessing`: Spectral preprocessing method (e.g., `"SNV + SG1"`).
#'     \item `covariates`: List-column of covariate sets (e.g., `c("Clay", "pH")`).
#'     \item `include_covariates`: Logical flag.
#'   }
#' @param input_data A `tibble` with preprocessed spectral data, including `Sample_ID`,
#'   wavenumber columns, and the target response variable.
#' @param covariate_data Optional `tibble` of predicted covariates matched by `Sample_ID`.
#'   Required if `include_covariates = TRUE` in any row of `config`.
#' @param variable Character. Name of the response variable (must be in `input_data`).
#' @param output_dir Path to output directory. Defaults to timestamped folder under `variable`.
#' @param grid_size_eval Integer. Number of combinations in the initial grid search (default = 10).
#' @param bayesian_iter_eval Integer. Number of iterations in Bayesian tuning (default = 15).
#' @param cv_folds_eval Integer. Number of CV folds during evaluation phase (default = 5).
#' @param retrain_top_models Logical. Whether to refit the top N models after screening (default = `TRUE`).
#' @param number_models_retained Integer. Number of top models to refit if `retrain_top_models = TRUE` (default = 15).
#' @param grid_size_final Integer. Grid size for refitting stage (default = 25).
#' @param bayesian_iter_final Integer. Bayesian iterations during refitting (default = 20).
#' @param cv_folds_final Integer. Number of CV folds for refitting phase (default = 15).
#' @param pruning Logical. Whether to enable pruning of poor configurations early (default = `FALSE`).
#' @param parallel_strategy Character. Parallelization strategy: `"cv_folds"` (default, legacy)
#'   parallelizes CV within each model, or `"models"` parallelizes across model configurations.
#' @param workers Integer. Number of parallel workers. If `NULL`, defaults to `detectCores() - 3`
#'   for `"cv_folds"` strategy or `detectCores() - 1` for `"models"` strategy.
#' @param chunk_size Integer. When using `"models"` strategy, process models in chunks of this size
#'   to manage memory (default = 50). Ignored for `"cv_folds"` strategy.
#' @param checkpoint_dir Character. Directory for checkpoint files when using `"models"` strategy.
#'   Enables resuming interrupted runs. If `NULL`, uses `output_dir/checkpoints/`.
#' @param resume Logical. Whether to resume from existing checkpoints (default = `TRUE`).
#'
#' @return If `retrain_top_models = FALSE`, returns a `tibble` summarizing evaluation metrics
#'   for each configuration. If `TRUE`, returns a `list` with:
#' \itemize{
#'   \item \strong{full_summary}: A `tibble` of metrics and metadata from the initial evaluation.
#'   \item \strong{refit_summary}: A `tibble` of metrics from the final refitting stage.
#' }
#'
#' @details
#' This function is designed for large-scale, reproducible modeling across 100s of candidate
#' configurations. Each model run:
#' \enumerate{
#'   \item Is isolated and wrapped in `safe_run_model()` to prevent failure propagation.
#'   \item Produces a pruned `.qs` file with only workflows and metrics needed for stacking.
#'   \item Logs run metadata (time, memory, status) and handles memory cleanup between runs.
#'   \item Returns a summary table for all configurations, and optionally retrains the top-N models.
#' }
#'
#' Memory usage is tracked with thresholds, and parallel evaluation is handled via `future::plan(multisession)`.
#' Outputs are written to disk in `output_dir`, including timestamped summary files for downstream stacking.
#'
#' ETA messages are printed every 25 configurations or 5 refits.
#'
#' @examples
#' \dontrun{
#' results <- run_model_evaluation(
#'   config                = model_config_grid,
#'   input_data            = spectral_data,
#'   covariate_data        = predicted_covs,
#'   variable              = "MAOM_C_g_kg",
#'   grid_size_eval        = 10,
#'   bayesian_iter_eval    = 15,
#'   number_models_retained = 20
#' )
#' }
#'
#' @seealso
#' \code{\link{safe_run_model}}, \code{\link{prune_model_output}}, \code{\link{full_model_evaluation}},
#' \code{\link[qs]{qread}}, \code{\link[stacks]{add_candidates}}
#'
#' @importFrom dplyr bind_rows filter arrange slice mutate
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning cli_alert_danger cli_inform
#' @importFrom fs dir_create path
#' @importFrom qs qsave
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map furrr_options
#' @include evaluation-chunk_configurations.R
#' @importFrom pryr mem_used
#' @importFrom glue glue
#' @export

run_model_evaluation <- function(config,
                                 input_data,
                                 covariate_data,
                                 variable,
                                 output_dir             = NULL,
                                 grid_size_eval         = 10,
                                 bayesian_iter_eval     = 15,
                                 cv_folds_eval          = 5,
                                 retrain_top_models     = TRUE,
                                 number_models_retained = 15,
                                 grid_size_final        = 25,
                                 bayesian_iter_final    = 20,
                                 cv_folds_final         = 15,
                                 pruning                = FALSE,
                                 parallel_strategy      = c("cv_folds", "models"),
                                 workers                = NULL,
                                 chunk_size             = 50,
                                 checkpoint_dir         = NULL,
                                 resume                 = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Parameter validation and setup
  ## ---------------------------------------------------------------------------

  ## Set up parallel strategy --------------------------------------------------

  parallel_strategy <- match.arg(parallel_strategy)

  if (is.null(workers)) {

    if (parallel_strategy == "cv_folds") {

      workers <- parallel::detectCores() - 3

    } else {

      workers <- parallel::detectCores() - 1

      }
  }

  ## Set up checkpointing ------------------------------------------------------

  if (is.null(checkpoint_dir) && parallel_strategy == "models") {

    checkpoint_dir <- fs::path(output_dir, "checkpoints")

  }

  cli::cli_h1("Starting full model evaluation across {.val {nrow(config)}} model combinations")
  cli::cli_alert_info("Parallel strategy: {.val {parallel_strategy}} with {.val {workers}} workers")
  if (parallel_strategy == "models") {
    cli::cli_alert_info("Chunk size: {.val {chunk_size}} models per batch")
    if (!is.null(checkpoint_dir)) {
      cli::cli_alert_info("Checkpoints: {.path {checkpoint_dir}}")
    }
  }

  start_time <- Sys.time()

  ## ---------------------------------------------------------------------------
  ## Step 1: Create a repository for the model output.
  ## ---------------------------------------------------------------------------

  if (is.null(output_dir)) {

    output_dir <- paste0(variable, "_model_outputs_", format(Sys.time(), "%Y-%m-%d_%H:%M"))

  }

  fs::dir_create(output_dir)

  cli::cli_alert_success("Output directory created at {.path {output_dir}}")

  raw_outputs  <- vector("list", length = nrow(config))
  summary_rows <- vector("list", length = nrow(config))


  ## ---------------------------------------------------------------------------
  ## Step 1.5: Write some aggresive memory saver functions.
  ## ---------------------------------------------------------------------------

  defragment_memory <- function() {


    for(i in 1:3) {
      gc(verbose = FALSE, full = TRUE)
      Sys.sleep(0.1)
    }

    if(exists(".Random.seed")) rm(.Random.seed, envir = .GlobalEnv)
    gc(verbose = FALSE, full = TRUE)

  }

  aggressive_cleanup <- function() {

    defragment_memory()
    rm(list = ls(pattern = "^temp_|^tmp_"), envir = parent.frame())
    if(exists("flush.console")) flush.console()
    invisible(gc(verbose = FALSE, full = TRUE))

  }

  time_log <- numeric(nrow(config))

  ## ---------------------------------------------------------------------------
  ## Step 2: Iterate over configurations
  ## ---------------------------------------------------------------------------

  if (parallel_strategy == "cv_folds") {

    future::plan(multisession, workers = workers)
    cli::cli_alert_success("Parallel backend registered with {.val {workers}} workers.")

    for (i in seq_len(nrow(config))) {

    start_time_i <- Sys.time()

    config_row_i <- config[i, , drop = FALSE]

    result_i <- safe_run_model(config_row        = config_row_i,
                               input_data        = input_data,
                               covariate_data    = covariate_data,
                               variable          = variable,
                               row_index         = i,
                               output_dir        = output_dir,
                               grid_size         = grid_size_eval,
                               bayesian_iter     = bayesian_iter_eval,
                               cv_folds          = cv_folds_eval,
                               pruning           = pruning,
                               save_output       = FALSE,
                               parallel_strategy = parallel_strategy)

    raw_outputs[[i]]  <- result_i
    summary_rows[[i]] <- result_i$status_summary

    cli::cli_alert_success("Quick snooze and taking out the trash. 💤")

    aggressive_cleanup()
    Sys.sleep(1)

    BYTES_PER_GB <- 1073741824
    mem_usage <- round(pryr::mem_used() / BYTES_PER_GB, 2)

    if(mem_usage < 2) {
      cli::cli_alert_success("Current Memory Usage: {.val {mem_usage}} GB")
    } else if (mem_usage < 4) {
      cli::cli_alert_warning("Current Memory Usage: {.val {mem_usage}} GB")
    } else {
      cli::cli_alert_danger("Current Memory Usage: {.val {mem_usage}} GB")
    }

    ## -------------------------------------------------------------------------

    end_time_i  <- Sys.time()
    duration_i  <- difftime(end_time_i, start_time_i, units = "mins")
    time_log[i] <- duration_i

    dplyr::case_when(time_log == 0 ~ NA_real_,
                     TRUE          ~ time_log) -> time_log

    if(i %in% seq(0, 10000, 5)){

      mean_duration    <- mean(as.numeric(time_log), na.rm = TRUE)
      remaining_models <- nrow(config) - i
      eta_mins         <- remaining_models * mean_duration
      eta_time         <- Sys.time() + (eta_mins * 60)
      rounded_eta      <- format(eta_time, "%D at %I:%M %p")

      cli::cli_alert_success("Model evaluation finished in {.val {round(duration_i, 3)}} minutes.")
      cli::cli_alert_warning("Mean Run Duration: {.val {round(mean_duration, 3)}} minutes.")
      cli::cli_alert_warning("Estimated run completion: {.val {rounded_eta}}")

    } else {

      cli::cli_alert_success("Model evaluation finished in {.val {round(duration_i, 3)}} minutes.")

    }

  }  # Close the for loop

  } else {

    ## -------------------------------------------------------------------------
    ## Model level parallelization with work-stealing
    ## -------------------------------------------------------------------------

    cli::cli_alert_info("Using work-stealing parallelization via evaluate_models_parallel()")
    
    # Call the fixed evaluate_models_parallel function
    parallel_results <- evaluate_models_parallel(
      configs               = config,
      input_data            = input_data,
      covariate_data        = covariate_data,
      variable              = variable,
      n_workers             = workers,
      output_dir            = output_dir,
      chunk_size            = chunk_size,  # This is deprecated in the new function
      grid_size             = grid_size_eval,
      bayesian_iter         = bayesian_iter_eval,
      cv_folds              = cv_folds_eval,
      seed                  = 123,
      save_individual_models = FALSE,
      verbose               = TRUE,
      parallel              = TRUE,      # Top-level orchestrator should parallelize
      allow_nested          = FALSE      # Prevent nested parallelization
    )
    
    # Convert results to expected format
    for (i in seq_len(nrow(parallel_results))) {
      raw_outputs[[i]] <- list(
        status_summary = parallel_results[i, ],
        model_output = parallel_results$best_params[[i]]
      )
      summary_rows[[i]] <- parallel_results[i, ]
    }
    #
    # cli::cli_alert_info("Split {nrow(config)} configurations into {n_chunks} chunks of ~{chunk_size} models each")
    #
    # if (!is.null(checkpoint_dir)) {
    #
    #   setup_success <- setup_checkpoint_directory(checkpoint_dir)
    #
    #   if (!setup_success) {
    #
    #     cli::cli_alert_warning("Checkpoint directory setup failed. Proceeding without checkpointing.")
    #     checkpoint_dir <- NULL
    #     }
    #   }
    #
    # # Check for existing checkpoints and resume
    # existing_checkpoints <- if (!is.null(checkpoint_dir)) {
    #   load_existing_checkpoints(checkpoint_dir, n_chunks)
    # } else {
    #   list(completed_results = list(), completed_indices = integer(0),
    #        next_chunk = 1L, n_completed = 0L)
    # }
    #
    # # Initialize results with existing data
    #
    # raw_outputs     <- vector("list", nrow(config))
    # chunk_durations <- numeric()
    #
    # # Fill in existing results
    #
    # for (idx in existing_checkpoints$completed_indices) {
    #
    #   chunk_start <- attr(config_chunks[[idx]], "start_config")
    #   chunk_end <- attr(config_chunks[[idx]], "end_config")
    #   chunk_results <- existing_checkpoints$completed_results[[idx]]
    #
    #   for (i in seq_along(chunk_results)) {
    #     raw_outputs[[chunk_start + i - 1]] <- chunk_results[[i]]
    #   }
    # }
    #
    # # Process remaining chunks
    #
    # start_chunk <- existing_checkpoints$next_chunk
    #
    # if (start_chunk <= n_chunks) {
    #   cli::cli_h2("Processing chunks {start_chunk} through {n_chunks}")
    #
    #   for (chunk_i in start_chunk:n_chunks) {
    #
    #     chunk_start_time <- Sys.time()
    #     current_chunk    <- config_chunks[[chunk_i]]
    #     chunk_start_idx  <- attr(current_chunk, "start_config")
    #     chunk_end_idx    <- attr(current_chunk, "end_config")
    #
    #     cli::cli_h2("Chunk {chunk_i}/{n_chunks}: Models {chunk_start_idx}-{chunk_end_idx} ({nrow(current_chunk)} configs)")
    #
    #     # Process chunk in parallel
    #     chunk_outputs <- furrr::future_map(seq_len(nrow(current_chunk)), function(i) {
    #
    #       global_i <- chunk_start_idx + i - 1
    #       config_row_i <- current_chunk[i, , drop = FALSE]
    #
    #       result_i <- safe_run_model(config_row        = config_row_i,
    #                                  input_data        = input_data,
    #                                  covariate_data    = covariate_data,
    #                                  variable          = variable,
    #                                  row_index         = global_i,
    #                                  output_dir        = output_dir,
    #                                  grid_size         = grid_size_eval,
    #                                  bayesian_iter     = bayesian_iter_eval,
    #                                  cv_folds          = cv_folds_eval,
    #                                  pruning           = pruning,
    #                                  save_output       = FALSE,
    #                                  parallel_strategy = "models")
    #
    #       return(result_i)
    #
    #     }, .options = furrr::furrr_options(seed = TRUE))
    #
    #     # Store results in main output list
    #     for (i in seq_along(chunk_outputs)) {
    #       raw_outputs[[chunk_start_idx + i - 1]] <- chunk_outputs[[i]]
    #     }
    #
    #     # Save checkpoint
    #     if (!is.null(checkpoint_dir)) {
    #       save_checkpoint(chunk_outputs, chunk_i, checkpoint_dir, current_chunk)
    #     }
    #
    #     # Chunk completion reporting
    #     chunk_end_time <- Sys.time()
    #     chunk_duration <- as.numeric(difftime(chunk_end_time, chunk_start_time, units = "mins"))
    #     chunk_durations <- c(chunk_durations, chunk_duration)
    #
    #     # Memory cleanup and reporting
    #     mem_usage <- chunk_memory_cleanup()
    #
    #     cli::cli_alert_success("Chunk {chunk_i} completed in {round(chunk_duration, 2)} minutes")
    #
    #     if (mem_usage < 2) {
    #       cli::cli_alert_success("Memory usage: {mem_usage} GB")
    #     } else if (mem_usage < 4) {
    #       cli::cli_alert_warning("Memory usage: {mem_usage} GB")
    #     } else {
    #       cli::cli_alert_danger("High memory usage: {mem_usage} GB")
    #     }
    #
    #     # ETA calculation and reporting
    #     if (chunk_i < n_chunks) {
    #       eta_info <- calculate_chunk_eta(chunk_i, n_chunks, chunk_durations)
    #       if (!is.na(eta_info$eta_formatted)) {
    #         cli::cli_alert_info("Estimated completion: {eta_info$eta_formatted}")
    #         cli::cli_alert_info("Average chunk duration: {round(eta_info$mean_duration, 2)} minutes")
    #       }
    #     }
    #
    #     Sys.sleep(2)
    #   }
    # } else {
    #   cli::cli_alert_success("All chunks already completed. Loading existing results.")
    # }
    #
    # # Extract summary rows
    # summary_rows <- purrr::map(raw_outputs, ~ .x$status_summary)
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Assemble and save summary
  ## ---------------------------------------------------------------------------

  summary_tbl  <- dplyr::bind_rows(summary_rows)

  timestamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
  summary_file <- fs::path(output_dir, glue::glue("batch_summary_{variable}_{timestamp}.qs"))
  qs::qsave(summary_tbl, summary_file)

  cli::cli_h2("Saved full evaluation summary table to: {.path {summary_file}}")

  ## ---------------------------------------------------------------------------
  ## Step 5: Retrain top models
  ## ---------------------------------------------------------------------------

  if(isTRUE(retrain_top_models)){

    cli::cli_h1("Retraining and saving the top {.val {number_models_retained}} model configurations.")

    ## -------------------------------------------------------------------------

    summary_tbl %>%
      filter(status == "success",
             !is.na(rrmse)) %>%
      arrange(rrmse) %>%
      dplyr::slice(1:number_models_retained) -> top_models

    refit_outputs  <- vector("list", length = nrow(top_models))
    refit_summary  <- vector("list", length = nrow(top_models))
    time_log_refit <- numeric(nrow(top_models))

    ## -------------------------------------------------------------------------

    for (j in seq_len(nrow(top_models))) {

      start_time_j <- Sys.time()

      idx          <- top_models$row[j]
      config_row_j <- config[idx, , drop = FALSE]

      safe_run_model(config_row     = config_row_j,
                     input_data     = input_data,
                     covariate_data = covariate_data,
                     variable       = variable,
                     row_index      = idx,
                     output_dir     = output_dir,
                     grid_size      = grid_size_final,
                     bayesian_iter  = bayesian_iter_final,
                     cv_folds       = cv_folds_final,
                     pruning        = FALSE,
                     save_output    = TRUE)  -> result_j

      refit_outputs[[j]] <- result_j
      refit_summary[[j]] <- result_j$status_summary

      cli::cli_alert_success("Quick snooze and taking out the trash. 💤")

      aggressive_cleanup()
      Sys.sleep(1)

      mem_usage <- round(pryr::mem_used() / BYTES_PER_GB, 2)

      if(mem_usage < 2) {
        cli::cli_alert_success("Current Memory Usage: {.val {mem_usage}} GB")
      } else if (mem_usage < 4) {
        cli::cli_alert_warning("Current Memory Usage: {.val {mem_usage}} GB")
      } else {
        cli::cli_alert_danger("Current Memory Usage: {.val {mem_usage}} GB")
      }

      ## -------------------------------------------------------------------------

      end_time_j  <- Sys.time()
      duration_j  <- difftime(end_time_j, start_time_j, units = "mins")
      time_log_refit[j] <- duration_j

      if(j %in% seq(0, 100, 5)){

        mean_duration    <- mean(as.numeric(time_log))
        remaining_models <- nrow(config) - j
        eta_mins         <- remaining_models * mean_duration
        eta_time         <- Sys.time() + (eta_mins*60)
        rounded_eta      <- format(eta_time, "%D at %I:%M %p")

        cli::cli_alert_success("Model refitting finished in {.val {round(duration_j, 3)}} minutes.")
        cli::cli_alert_success("Estimated refitting completion: {.val {rounded_eta}}")

      } else {

        cli::cli_alert_success("Model evaluation finished in {.val {round(duration_j, 3)}} minutes.")

      }
    }

    future::plan(sequential)

    refit_tbl <- dplyr::bind_rows(refit_summary)
    qs::qsave(refit_tbl, fs::path(output_dir, glue::glue("refit_summary_{variable}_{timestamp}.qs")))

    duration <- difftime(Sys.time(), start_time, units = "mins")

    cli::cli_h1("🌱 horizons model evaluation and refitting finished in {.val {round(duration/60, 2)}} hours.")

    return(list(full_summary  = summary_tbl,
                refit_summary = refit_tbl))

  } else {

    future::plan(sequential)

    duration <- difftime(Sys.time(), start_time, units = "mins")

    cli::cli_h2("Full model evaluation completed for {.val {nrow(config)}} configurations. Logs saved to {.path {output_dir}}.")
    cli::cli_h1("🌱 horizons model evaluation finished in {.val {round(duration/60, 2)}} hours.")

    return(summary_tbl)
  }
}

