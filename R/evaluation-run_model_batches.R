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
#' @param pruning Logical. Whether to skip poor models early using RRMSE thresholds.
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
                                 output_dir             = NULL,
                                 grid_size_eval         = 10,
                                 bayesian_iter_eval     = 15,
                                 cv_folds_eval          = 5,
                                 retrain_top_models     = TRUE,
                                 number_models_retained = 15,
                                 grid_size_final        = 25,
                                 bayesian_iter_final    = 20,
                                 cv_folds_final         = 15,
                                 pruning                = FALSE) {

  cli::cli_h1("Starting full model evaluation across {.val {nrow(config)}} model combinations")

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

  future::plan(multisession, workers = parallel::detectCores() - 3)
  cli::cli_alert_success("Parallel backend registered with {.val {parallel::detectCores() - 3}} workers.")

  for (i in seq_len(nrow(config))) {

    start_time_i <- Sys.time()

    config_row_i <- config[i, , drop = FALSE]

    result_i <- safe_run_model(config_row     = config_row_i,
                               input_data     = input_data,
                               covariate_data = covariate_data,
                               variable       = variable,
                               row_index      = i,
                               output_dir     = output_dir,
                               grid_size      = grid_size_eval,
                               bayesian_iter  = bayesian_iter_eval,
                               cv_folds       = cv_folds_eval,
                               pruning        = pruning,
                               save_output    = FALSE)

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

    if(i %in% seq(0, 10000, 25)){

      mean_duration    <- median(as.numeric(time_log)*60)
      remaining_models <- nrow(config) - i
      eta_mins         <- remaining_models * mean_duration
      eta_time         <- Sys.time() + (eta_mins*60)
      rounded_eta      <- format(eta_time, "%D at %I:%M %p")

      cli::cli_alert_success("Model evaluation finished in {.val {round(duration_i, 3)}} minutes.")
      cli::cli_alert_success("Estimated run completion: {.val {rounded_eta}}")

    } else {

      cli::cli_alert_success("Model evaluation finished in {.val {round(duration_i, 3)}} minutes.")

    }
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

