#' Safely Run a Single Model Configuration and Log Results
#'
#' Evaluates a single model configuration row using the `evaluate_model_config()` function,
#' with robust error handling, logging, output pruning, and optional result caching.
#' Designed for internal use within `run_model_evaluation()` or other batch modeling workflows.
#'
#' @param config_row A single-row `tibble` containing a model configuration. Must include:
#'   `model`, `transformation`, `preprocessing`, `covariates`, and `include_covariates`.
#' @param input_data A `tibble` with preprocessed spectral data. Must include `Sample_ID`,
#'   `Wavenumber`, `Absorbance`, and the target response variable.
#' @param covariate_data Optional `tibble` of predicted covariates (must include `Sample_ID`),
#'   required if `include_covariates = TRUE`.
#' @param variable Character. Name of the outcome variable to predict.
#' @param row_index Integer. The row index of the configuration in the batch grid, used for labeling files and logging.
#' @param output_dir Character path to a directory for saving model output and error logs.
#' @param grid_size Integer. Number of candidates for initial grid tuning (default = 10).
#' @param bayesian_iter Integer. Number of iterations for Bayesian tuning (default = 15).
#' @param cv_folds Integer. Number of cross-validation folds used in tuning (default = 5).
#' @param pruning Logical. Whether to skip poorly performing models based on RRMSE after grid tuning (default = `TRUE`).
#' @param save_output Logical. Whether to save the full model result to disk as a `.qs` file (default = `FALSE`).
#'
#' @return A named `list` with:
#' \itemize{
#'   \item \strong{status_summary}: A one-row `tibble` containing:
#'     \code{row}, \code{wflow_id}, \code{rrmse}, \code{rmse}, \code{rsq},
#'     \code{status} (`"success"`, `"pruned"`, or `"error"`),
#'     file paths (if saved), and any captured error messages.
#'   \item \strong{saved_path}: File path to the pruned model result (`.qs`), or `NA` if not saved.
#' }
#'
#' @details
#' This function isolates evaluation of a single configuration for safe execution in parallelized batch loops.
#' On success, model outputs are pruned and optionally saved. If an error occurs, the error object is written to a
#' JSON file and logged to the console via `cli`. Pruned workflows are intended for use in stacking via `stacks::add_candidates()`.
#'
#' It is typically called inside `run_model_evaluation()` or a `purrr::map()` workflow.
#'
#' @examples
#' \dontrun{
#' result <- safe_run_model(
#'   config_row     = config[1, ],
#'   input_data     = spectral_data,
#'   covariate_data = predicted_covs,
#'   variable       = "MAOM_C_g_kg",
#'   row_index      = 1,
#'   output_dir     = "outputs"
#' )
#'
#' result$status_summary
#' }
#'
#' @seealso
#' \code{\link{evaluate_model_config}}, \code{\link{run_model_evaluation}}, \code{\link{prune_model_output}}
#'
#' @importFrom dplyr tibble mutate filter slice case_when
#' @importFrom cli cli_h2 cli_alert_success cli_alert_danger cli_alert_info cli_alert_warning
#' @importFrom fs path dir_create
#' @importFrom qs qsave
#' @importFrom jsonlite write_json
#' @export


safe_run_model <- function(config_row,
                           input_data,
                           covariate_data,
                           variable,
                           row_index,
                           output_dir,
                           grid_size     = 10,
                           bayesian_iter = 15,
                           cv_folds      = 5,
                           pruning       = TRUE,
                           save_output   = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Setup
  ## ---------------------------------------------------------------------------

  clean_workflow_id(model             = config_row$model,
                    transformation    = config_row$transformation,
                    preprocessing     = config_row$preprocessing,
                    feature_selection = config_row$feature_selection,
                    covariates        = config_row$covariates) -> config_desc

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
                                               feature_selection  = config_row$feature_selection,
                                               covariates         = config_row$covariates,
                                               pruning            = pruning,
                                               grid_size          = grid_size,
                                               bayesian_iter      = bayesian_iter,
                                               cv_folds           = cv_folds)},
                 default_value     = NULL,
                 error_message     = "Failed model run at config row {row_index}: {config_desc}",
                 log_error         = TRUE,
                 capture_trace     = FALSE) -> model_res_safe

  model_res  <- model_res_safe$result

  ## ---------------------------------------------------------------------------
  ## Step 3: Handle and log error result
  ## ---------------------------------------------------------------------------

  if (is.null(model_res) || isTRUE(model_res$error)) {

    error_file <- fs::path(output_dir, paste0("error_", row_index, "_", config_desc, ".json"))

    error_msg <- if (!is.null(model_res_safe$error)) {
      conditionMessage(model_res_safe$error)
    } else if (!is.null(model_res$reason)) {
      model_res$reason
    } else {
      "evaluate_model_config() returned NULL with no error message"
    }

    error_obj <- list(row           = row_index,
                      config        = config_row,
                      error_message = error_msg,
                      call          = NULL,
                      time          = as.character(Sys.time()))

    jsonlite::write_json(error_obj,
                         error_file,
                         pretty = TRUE,
                         auto_unbox = TRUE)

    tibble::tibble(row              = row_index,
                   wflow_id         = config_desc,
                   rsq              = NA_real_,
                   rmse             = NA_real_,
                   rrmse            = NA_real_,
                   saved_path       = NA_character_,
                   error_log_path   = error_file,
                   error_message    = error_msg,
                   status           = "error") -> status_summary

    cli::cli_alert_danger("Model run failed at: {row_index} - {config_desc}")
    cli::cli_alert_info("Logged error to: {.path {error_file}}")

    return(list(status_summary = status_summary,
                output_path    = NA_character_))
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Build status summary
  ## ---------------------------------------------------------------------------

  tibble::tibble(row                = row_index,
                 config_desc        = config_desc,
                 wflow_id           = model_res$wflow_id,
                 rsq                = NA_real_,
                 rmse               = NA_real_,
                 rrmse              = NA_real_,
                 saved_path         = NA_character_,
                 error_log_path     = NA_character_,
                 error_message      = NA_character_,
                 status             =  dplyr::case_when(isTRUE(model_res$pruned) ~ "pruned",
                                                        isTRUE(model_res$error)  ~ "error",
                                                        TRUE                     ~ "success")) -> status_summary

  if (!isTRUE(model_res$pruned) &&
      !is.null(model_res$evaluation_results) &&
      nrow(model_res$evaluation_results) > 0) {

    model_res$evaluation_results %>%
      dplyr::filter(!is.na(rrmse)) %>%
      dplyr::arrange(rrmse) %>%
      dplyr::slice(1) -> best_workflow

    status_summary$rsq   <- best_workflow$rsq
    status_summary$rmse  <- best_workflow$rmse
    status_summary$rrmse <- best_workflow$rrmse

    }

  ## ---------------------------------------------------------------------------
  ## Step 5: Save result if not an error
  ## ---------------------------------------------------------------------------

  if(isTRUE(save_output)){

  file_name                  <- paste0("result_", row_index, "_", config_desc, ".qs")
  saved_path                 <- fs::path(output_dir, file_name)
  status_summary$saved_path  <- saved_path

  qs::qsave(model_res, saved_path)

  }


  ## ---------------------------------------------------------------------------
  ## Step 6: Done
  ## ---------------------------------------------------------------------------

  return(list(status_summary = status_summary,
              saved_path     = status_summary$saved_path))

}



