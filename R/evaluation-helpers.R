#' Helper Functions for Local Model Evaluation
#'
#' @description
#' Collection of utility functions supporting local model evaluation.
#' These functions handle error formatting for failed evaluations.
#'
#' @keywords internal

## Error Handling Helpers -----------------------------------------------------

#' Create Failed Result Row
#' 
#' @description
#' Creates a properly formatted result row for failed model evaluations.
#' Returns a tibble row with NA metrics and error information.
#' 
#' @param config_id Model configuration ID
#' @param config_clean Cleaned configuration data
#' @param error_message Descriptive error message
#' @param workflow_id Optional workflow ID if already created
#' 
#' @return Tibble row with NA metrics and error information
#' @keywords internal
create_failed_result <- function(config_id,
                                config_clean,
                                error_message,
                                workflow_id = NULL) {
  
  tibble::tibble(
    config_id         = config_id,
    workflow_id       = workflow_id %||% NA_character_,
    model             = config_clean$model,
    transformation    = config_clean$transformation,
    preprocessing     = config_clean$preprocessing,
    feature_selection = config_clean$feature_selection,
    covariates        = if (!is.null(config_clean$covariates)) {
                          paste(config_clean$covariates, collapse = "-")
                        } else {
                          NA_character_
                        },
    best_params       = list(NULL),  # No params for failed models
    rsq               = NA_real_,
    rmse              = NA_real_,
    rrmse             = NA_real_,
    rpd               = NA_real_,
    ccc               = NA_real_,
    mae               = NA_real_,
    grid_seconds      = NA_real_,
    bayes_seconds     = NA_real_,
    total_seconds     = NA_real_,
    status            = "failed",
    error_message     = error_message
    # Note: fitted_workflow removed to prevent memory leak
  )
}