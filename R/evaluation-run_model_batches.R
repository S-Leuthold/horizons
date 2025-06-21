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

