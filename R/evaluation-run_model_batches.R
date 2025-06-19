#' Run Full Batch Model Evaluation Across a Configuration Grid
#'
#' Iterates over a tibble of model configurations to safely execute the full ensemble modeling
#' pipeline for each combination. For each configuration, this function:
#' (1) calls `full_model_evaluation()` with atomic inputs,
#' (2) wraps the run in error handling and structured logging via `safe_run_model()`,
#' (3) prunes the output to retain only evaluation metrics and workflows required for stacking,
#' and (4) aggregates a run-level summary table of model performance, file paths, and error status.
#'
#' Designed for high-throughput batch modeling of spectral data with optional covariates.
#' Can optionally return full raw results for interactive inspection, or save only a lightweight
#' summary table for downstream filtering and stacking.
#'
#' @import dplyr
#' @importFrom cli cli_h1 cli_alert_success cli_inform
#' @importFrom fs dir_create path
#' @importFrom qs qsave
#'
#' @param config A tibble of model configurations to evaluate. Must include the following columns:
#'   \describe{
#'     \item{model}{Character. Name of the model to run (e.g., `"PLSR"`, `"Cubist"`).}
#'     \item{transformation}{Character. Outcome transformation label (e.g., `"Log Transformation"`).}
#'     \item{preprocessing}{Character. Spectral preprocessing method (e.g., `"SNV + SG1"`).}
#'     \item{covariates}{List-column of covariates to include (e.g., `c("Clay", "pH")`).}
#'     \item{include_covariates}{Logical. Whether to include external covariates in modeling.}
#'   }
#' @param input_data A preprocessed spectral tibble containing columns `Sample_ID`, `Wavenumber`, and `Absorbance`,
#'   along with the target response variable.
#' @param covariate_data A tibble of predicted covariate values, matched by `Sample_ID`. Required if `include_covariates = TRUE`.
#' @param variable Character string specifying the name of the response variable to model (must be present in `input_data`).
#' @param output_dir Directory where all pruned results and error logs will be saved. Default is `"batch_model_outputs"`.
#' @param grid_size Integer. Number of hyperparameter combinations to evaluate per model in the initial grid search (default = 10).
#' @param bayesian_iter Integer. Number of iterations to run in the Bayesian tuning phase (default = 15).
#' @param cv_folds Integer. Number of cross-validation folds used during resampling (default = 5).
#' @param return_outputs Logical. If `TRUE`, also return the full list of raw outputs from each `safe_run_model()` call. Default = `FALSE`.
#' @param save_summary Logical. If `TRUE`, save the final summary table to disk as a `.qs` file. Default = `TRUE`.
#' @param summary_file Optional. Full file path for saving the summary table. If `NULL`, a timestamped file will be created in `output_dir`.
#'
#' @return Either:
#' \describe{
#'   \item{Tibble (default)}{A summary table with one row per model configuration. Includes workflow ID, performance metrics, status flags, and file paths.}
#'   \item{List (if `return_outputs = TRUE`)}{A list with two elements:
#'     \describe{
#'       \item{summary}{Tibble as above.}
#'       \item{raw_results}{List of all outputs returned by `safe_run_model()` for further inspection.}
#'     }
#'   }
#' }
#'
#' @details
#' This function is optimized for large-scale, memory-safe model screening over 100s of configurations.
#' It is designed to work with atomic modeling inputs — no internal covariate grid expansion is performed.
#' Covariates should be precomputed using `predict_covariates()` and passed in full.
#'
#' Each model run is safely wrapped via `safe_run_model()`, and results are pruned via `prune_model_output()`
#' to retain only metrics and fitted workflows. Errors are logged to JSON files and do not interrupt the loop.
#' Garbage collection and timing gaps are included by default to reduce memory pressure.
#'
#' Output files are saved in the specified `output_dir`. A timestamped summary file is written automatically
#' unless `save_summary = FALSE`. These files can be reloaded using `qs::qread()` for further analysis or stacking.
#'
#' @section Run Lifecycle:
#' \enumerate{
#'   \item Loop over each row of `config`
#'   \item Call `safe_run_model()` with filtered inputs
#'   \item Save pruned output as `.qs` file
#'   \item Aggregate evaluation metrics and paths into `summary`
#'   \item Optionally return raw results and/or write summary file
#' }
#'
#' @seealso
#' [safe_run_model()], [prune_model_output()], [full_model_evaluation()],
#' [qs::qread()], [stacks::add_candidates()]
#'
#' @examples
#' \dontrun{
#' results <- run_batch_models(
#'   config         = expanded_model_grid,
#'   input_data     = Input_Data,
#'   covariate_data = Covariate_Data$Predicted_Values,
#'   variable       = "MAOM_C_g_kg"
#' )
#' }
#' @export

run_model_evaluation <- function(config,
                                 input_data,
                                 covariate_data,
                                 variable,
                                 output_dir     = NULL,
                                 grid_size      = 25,
                                 bayesian_iter  = 20,
                                 cv_folds       = 10,
                                 return_outputs = FALSE,
                                 pruning        = TRUE,
                                 summary_file   = NULL,
                                 parallel       = TRUE) {

  cli::cli_h1("Starting full model evaluation across {.val {nrow(config)}} model combinations")

  start_time <- Sys.time()

  ## ---------------------------------------------------------------------------
  ## Step 1: Create Output Directory
  ## ---------------------------------------------------------------------------

  if (is.null(output_dir)) {
    output_dir <- paste0(variable, "_model_outputs_", format(Sys.time(), "%Y-%m-%d_%H-%M"))
  }

  fs::dir_create(output_dir)
  cli::cli_alert_success("Output directory created at {.path {output_dir}}")

  # ---------------------------------------------------------------------------
  # Step 3: Define a CLI-based Polling Progress Bar (Your brilliant idea)
  # ---------------------------------------------------------------------------

  # This function will run in the background on its own core
  poll_output_dir <- function(dir, total, interval = 2) {

    # Use the more robust cli progress bar
    cli::cli_progress_bar(
      name = "Running Models",
      total = total,
      format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )

    done <- 0
    while (done < total) {
      # Count the files created by the workers
      done <- length(list.files(dir, pattern = "^result_.*\\.qs$"))
      cli::cli_progress_update(set = done)
      Sys.sleep(interval)
    }
    # The bar will auto-terminate when the loop ends
  }

  # ---------------------------------------------------------------------------
  # Step 4: Add the missing return statement to your model runner
  # ---------------------------------------------------------------------------

  # Your original function was perfect, just needed to return the result
  run_one_model <- function(config_row, row_index) {
    result <- safe_run_model(
      config_row     = config_row,
      input_data     = input_data,
      covariate_data = covariate_data,
      variable       = variable,
      row_index      = row_index,
      output_dir     = output_dir,
      grid_size      = grid_size,
      bayesian_iter  = bayesian_iter,
      cv_folds       = cv_folds,
      pruning        = pruning,
      verbose        = FALSE
    )

    # The missing piece: return the result object
    return(result)
  }


  # ---------------------------------------------------------------------------
  # Step 5: Run Models with Polling
  # ---------------------------------------------------------------------------

  if (parallel) {

    future::plan(future::multisession, workers = parallel::detectCores() - 2)
    cli::cli_alert_success("Initialized parallel workflow with {.val {future::nbrOfWorkers()}} workers.")

    # -------------------------------------------------------------------------
    # Step 1: FIRE a non-blocking set of futures.
    # This dispatches all jobs to the workers but DOES NOT wait for them.
    # The main R session remains free.
    # -------------------------------------------------------------------------
    cli::cli_alert_success("Dispatching models to background workers...")

    # We manually create a list of promises ("futures"), one for each model.
    # This does not block the main R session.
    futures <- purrr::map2(
      .x = split(config, seq_len(nrow(config))),
      .y = seq_len(nrow(config)),
      .f = ~ future::future(run_one_model(.x, .y), seed = TRUE)
    )

    # -------------------------------------------------------------------------
    # Step 2: MONITOR progress by polling from the Main R Session.
    # Now that the main session is free, we can run the progress bar here.
    # -------------------------------------------------------------------------

    # This function now runs in the main R session, where it can draw to the console.
    poll_output_dir(output_dir, total = nrow(config))

    # -------------------------------------------------------------------------
    # Step 3: COLLECT the results.
    # The polling is done, which means all the .qs files exist and the workers
    # have finished their calculations. Now we resolve the promises.
    # -------------------------------------------------------------------------
    cli::cli_alert_success("All models complete. Collecting results...")

    results <- future::value(futures) # This collects the return values from all futures.

    # Process the results as before
    summary_tbl <- dplyr::bind_rows(purrr::map(results, "status_summary"))

    future::plan(sequential)
    cli::cli_alert_success("Workflow complete.")


  } else {

    raw_outputs  <- vector("list", length = nrow(config))
    summary_rows <- vector("list", length = nrow(config))

    for (i in seq_len(nrow(config))) {
      config_row <- config[i, , drop = FALSE]
      result <- run_one_model(config_row = config_row, row_index = i)
      raw_outputs[[i]]  <- result
      summary_rows[[i]] <- result$status_summary
      cli::cli_inform("Sleeping and collecting garbage...")
    }

    summary_tbl <- dplyr::bind_rows(summary_rows)
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Summarize Results
  ## ---------------------------------------------------------------------------

  if (parallel) {
    summary_tbl <- purrr::map_dfr(results, "status_summary")
    raw_outputs <- results
  }

  if (is.null(summary_file)) {
    timestamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
    summary_file <- fs::path(output_dir, glue::glue("batch_summary_{variable}_{timestamp}.qs"))
  }

  qs::qsave(summary_tbl, summary_file)
  cli::cli_h2("Saved summary table to: {.path {summary_file}}")

  ## ---------------------------------------------------------------------------
  ## Step 6: Error Summary
  ## ---------------------------------------------------------------------------

  error_logs <- list.files(output_dir, pattern = "^error_.*\\.json$")
  if (length(error_logs) > 0) {
    cli::cli_alert_warning("⚠️ {length(error_logs)} model failures logged.")
  } else {
    cli::cli_alert_success("No model failures detected.")
  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Return
  ## ---------------------------------------------------------------------------

  duration <- difftime(Sys.time(), start_time, units = "mins")
  cli::cli_h2("Full evaluation completed in {.val {round(duration, 1)}} minutes.")

  if (return_outputs) {
    return(list(summary = summary_tbl, raw_results = raw_outputs))
  } else {
    return(summary_tbl)
  }
}

