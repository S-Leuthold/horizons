#' Extract Evaluation Metrics and Workflow Info for Stacking
#'
#' Reduces a full model evaluation object to only the components required for downstream
#' stacking and performance review. Intended for use inside batch modeling workflows
#' to support lightweight result storage and compatibility with `stacks::add_candidates()`.
#'
#' @importFrom dplyr select mutate
#' @importFrom purrr map
#' @importFrom tibble tibble
#'
#' @param fem_result A named list returned by `full_model_evaluation()`. Must include:
#'   - `evaluation_results`: tibble of model metrics
#'   - `tuned_models`: a `workflow_map()`-like tibble containing `wflow_id` and `info$workflow`.
#'
#' @return A named list with:
#' \describe{
#'   \item{evaluation_results}{A tibble with performance metrics for each final model.}
#'   \item{stacking_info}{A tibble containing `wflow_id` and fitted `workflow` objects suitable for use with `stacks::add_candidates()`.}
#' }
#'
#' @details
#' This helper supports internal batch evaluation by pruning large modeling objects down to
#' their essential components. If the structure of `tuned_models` is malformed or missing,
#' the `stacking_info` field will return `NULL`, but the function will not error.
#'
#' @seealso
#' [safe_run_model()], [run_batch_models()], [stacks::add_candidates()]
#'
#' @keywords internal

prune_model_output <- function(fme_result) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate the input
  ## ---------------------------------------------------------------------------

  if (is.null(fme_result) || !is.list(fme_result)) {
    warning("Invalid full_model_evaluation() result provided to prune_model_output().")
    return(list(evaluation_results = NULL, stacking_info = NULL))
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract evaluation results and tuned workflows
  ## ---------------------------------------------------------------------------

  evaluation_results <- fme_result$evaluation_results
  stacking_info      <- NULL

  if (!is.null(fme_result$tuned_models) &&
      "wflow_id" %in% names(fme_result$tuned_models) &&
      "info" %in% names(fme_result$tuned_models)) {

    tryCatch({

      fme_result$tuned_models %>%
        dplyr::select(wflow_id, workflow = info) %>%
        dplyr::mutate(workflow = purrr::map(workflow, ~ .x$workflow)) -> stacking_info

    }, error = function(e) {

      cli::cli_warn("Could not extract {.field workflow} objects from {.val tuned_models$info}. Returning {.val NULL}.")
      stacking_info <- NULL

    })
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Return the pruned results
  ## ---------------------------------------------------------------------------

  return(list(evaluation_results = evaluation_results,
              stacking_info      = stacking_info))
}
