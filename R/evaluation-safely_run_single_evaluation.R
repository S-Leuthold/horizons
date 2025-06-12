#' Safely Run a Single Model Configuration and Log Results
#'
#' Executes a full ensemble model evaluation for a single configuration row,
#' with error handling, output pruning, and structured logging. Intended for
#' internal use within batch modeling workflows. Wraps `full_model_evaluation()`
#' and saves output summaries for downstream stacking and diagnostics.
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @importFrom cli cli_h2 cli_alert_success cli_alert_danger cli_alert_info
#' @importFrom fs dir_create path
#' @importFrom jsonlite write_json
#' @importFrom qs qsave
#'
#' @param config_row A single-row tibble with configuration details. Must contain:
#'   `model`, `transformation`, `preprocessing`, `covariates`, and `include_covariates`.
#' @param input_data A preprocessed tibble of spectral data with `Sample_ID`, `Wavenumber`, and `Absorbance`.
#' @param covariate_data Optional tibble of predicted covariates (if `include_covariates = TRUE`).
#' @param variable Character name of the outcome variable to predict.
#' @param row_index Integer indicating the row number (used for logging and filenames).
#' @param output_dir Directory for saving pruned results and error logs.
#' @param grid_size Number of grid search candidates per model (default = 10).
#' @param bayesian_iter Number of Bayesian tuning iterations (default = 15).
#' @param cv_folds Number of cross-validation folds (default = 5).
#'
#' @return A named list with:
#' \describe{
#'   \item{status_summary}{A one-row tibble with model ID, RMSE, R², pruned file path, error path, error message, and status flag.}
#'   \item{pruned_output_path}{Path to `.qs` file with pruned model result (if successful).}
#' }
#'
#' @details
#' This function is designed for internal use in `run_batch_models()`, where it is
#' called iteratively across a grid of model configurations. The returned summary row
#' supports monitoring and downstream filtering, while pruned results retain key
#' evaluation metrics and stacking-ready workflows.
#'
#' Errors are caught and written to `.json` files in the specified `output_dir`.
#' CLI progress messages are displayed throughout the run.
#'
#' @seealso [full_model_evaluation()], [prune_model_output()], [run_batch_models()]
#' @keywords internal

safe_run_model <- function(config_row,
                           input_data,
                           covariate_data,
                           variable,
                           row_index,
                           output_dir,
                           grid_size     = 10,
                           bayesian_iter = 15,
                           cv_folds      = 5,
                           pruning       = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Setup
  ## ---------------------------------------------------------------------------

  clean_workflow_id(model          = config_row$model,
                    transformation = config_row$transformation,
                    preprocessing  = config_row$preprocessing,
                    covariates     = config_row$covariates) -> config_desc

  cli::cli_h2("Running model configuration {row_index}: {config_desc}")

  ## ---------------------------------------------------------------------------
  ## Step 2: Safely run model evaluation
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {evaluate_model_config(input_data         = input_data,
                                               covariate_data     = covariate_data,
                                               variable           = variable,
                                               model              = config_row$model,
                                               transformation     = config_row$transformation,
                                               preprocessing      = config_row$preprocessing,
                                               covariates         = config_row$covariates[[1]],
                                               include_covariates = config_row$include_covariates,
                                               pruning            = pruning,
                                               grid_size          = grid_size,
                                               bayesian_iter      = bayesian_iter,
                                               cv_folds           = cv_folds)},
                 default_value     = NULL,
                 error_message     = "Failed model run at config row {row_index}: {config_desc}",
                 return_result_list = TRUE) -> model_res_safe

  model_res  <- model_res_safe$result
  run_error  <- model_res_safe$error

  ## ---------------------------------------------------------------------------
  ## Step 3: Handle null result (true error, not just pruned)
  ## ---------------------------------------------------------------------------

  if (is.null(model_res)) {

    error_file <- fs::path(output_dir, paste0("error_", row_index, "_", config_desc, ".json"))

    error_obj <- list(row           = row_index,
                      config        = config_row,
                      error_message = if (!is.null(run_error)) run_error$message else "evaluate_model_config() returned NULL",
                      call          = if (!is.null(run_error)) deparse(run_error$call) else NULL,
                      time          = as.character(Sys.time()))

    jsonlite::write_json(error_obj, error_file, pretty = TRUE, auto_unbox = TRUE)

    tibble::tibble(row                = row_index,
                   wflow_id           = config_desc,
                   rsq                = NA_real_,
                   rmse               = NA_real_,
                   rrmse              = NA_real_,
                   output_path        = NA_character_,
                   error_log_path     = error_file,
                   error_message      = error_obj$error_message,
                   status             = "error") -> status_summary

    cli::cli_alert_danger("Model run failed at: {row_index} - {config_desc}")
    cli::cli_alert_info("Logged error to: {.path {error_file}}")

    return(list(status_summary     = status_summary,
                output_path        = NA_character_))

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Build initial status summary
  ## ---------------------------------------------------------------------------

  tibble::tibble(row                = row_index,
                 config_desc        = config_desc,
                 wflow_id           = model_res$wflow_id,
                 rsq                = NA_real_,
                 rmse               = NA_real_,
                 rrmse              = NA_real_,
                 output_path        = NA_character_,
                 error_log_path     = NA_character_,
                 error_message      = if (isTRUE(model_res$error)) model_res$reason else NA_character_,
                 status             = if (isTRUE(model_res$error)) "error"
                                      else if (isTRUE(model_res$pruned)) "pruned"
                                      else "success") -> status_summary

  ## ---------------------------------------------------------------------------
  ## Step 5: Save result if not an error
  ## ---------------------------------------------------------------------------

  if (!isTRUE(model_res$error)) {

    file_name                  <- paste0("result_", row_index, "_", config_desc, ".qs")
    output_path                <- fs::path(output_dir, file_name)
    status_summary$output_path <- output_path

    qs::qsave(model_res, output_path)

    if (!isTRUE(model_res$pruned) &&
        !is.null(model_res$evaluation_results) &&
        nrow(model_res$evaluation_results) > 0) {

      model_res$evaluation_results %>%
        dplyr::filter(!is.na(rrmse)) %>%
        dplyr::arrange(rrmse) %>%
        dplyr::slice(1) -> best

      status_summary$rsq   <- best$rsq
      status_summary$rmse  <- best$rmse
      status_summary$rrmse <- best$rrmse
    }
  }


  ## ---------------------------------------------------------------------------
  ## Step 6: Done
  ## ---------------------------------------------------------------------------

  return(list(status_summary = status_summary,
              output_path    = status_summary$output_path))

}

