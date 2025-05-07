#' Evaluate Finalized Workflows on Hold-Out Data
#'
#' Applies each finalized model workflow to a held-out evaluation dataset, computes predictions,
#' performs any required back-transformations (e.g., inverse log or square-root), and evaluates
#' model performance using a standard set of regression metrics: R², RMSE, and RRMSE.
#'
#' This function is designed to be called after `fit()` has been applied to workflows created
#' via `workflow_set()` and filtered using `filter_workflows()`. It assumes a consistent
#' structure of fitted workflows with a `Response` column as the target.
#'
#' @importFrom dplyr mutate select rename case_when
#' @importFrom purrr map map2
#' @importFrom tidyr pivot_wider unnest
#' @importFrom tibble tibble
#' @importFrom yardstick metric_set rsq rmse
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @importFrom cli cli_abort
#'
#' @param finalized_wf_sets A tibble of fitted workflows, typically from `full_model_evaluation()`,
#'        containing at least `fitted_wf` (workflow object) and `wflow_id` (identifier).
#' @param holdout_data A data frame containing `Sample_ID`, `Response`, and any required covariates
#'        or predictors. Used for final model evaluation.
#'
#' @return A tibble with one row per model/workflow ID, containing:
#' \describe{
#'   \item{wflow_id}{Identifier string describing the model configuration.}
#'   \item{rsq}{Coefficient of determination (R²).}
#'   \item{rmse}{Root Mean Squared Error.}
#'   \item{rrmse}{Relative RMSE as a percentage.}
#' }
#'
#' @seealso \code{\link[yardstick]{metric_set}}, \code{\link[yardstick]{rsq}},
#'   \code{\link[yardstick]{rmse}}, \code{\link{rrmse_vec}}, \code{\link{full_model_evaluation}}
#'
#' @examples
#' \dontrun{
#' evaluate_final_models(finalized_wf_sets = finalized_models,
#'                       holdout_data = test_data)
#' }
#'
#' @keywords internal

evaluate_final_models <- function(finalized_wf_sets, holdout_data) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate Inputs
  ## ---------------------------------------------------------------------------

  if (!"fitted_wf" %in% names(finalized_wf_sets)) {
    cli::cli_abort("Missing {.field fitted_wf} column in finalized_wf_sets.")
  }

  if (!"Response" %in% names(holdout_data)) {
    cli::cli_abort("Holdout data must include a {.field Response} column.")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Set up metric function
  ## ---------------------------------------------------------------------------

  eval_metrics <- yardstick::metric_set(rsq, rmse, rrmse)

  ## ---------------------------------------------------------------------------
  ## Step 3: Inner function: predict + back-transform
  ## ---------------------------------------------------------------------------

  predict_and_evaluate <- function(wf, id) {

    predict(wf, new_data = holdout_data) %>%
      dplyr::rename(estimate = .pred) %>%
      dplyr::mutate(estimate = dplyr::case_when(
        grepl("Log", id)  ~ exp(estimate),
        grepl("Sqrt", id) ~ estimate^2,
        TRUE              ~ estimate
      )) -> preds

    tibble::tibble(
      Sample_ID = holdout_data$Sample_ID,
      estimate  = preds$estimate,
      truth     = holdout_data$Response
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Run predictions + evaluate metrics for each model
  ##  --------------------------------------------------------------------------

  finalized_wf_sets %>%
    dplyr::mutate(
      predictions = purrr::map2(.x = fitted_wf,
                                .y = wflow_id,
                                .f = predict_and_evaluate),

      final_metrics = purrr::map(
        predictions,
        ~ eval_metrics(data     = .x,
                       truth    = truth,
                       estimate = estimate) %>%
          dplyr::select(-.estimator) %>%
          tidyr::pivot_wider(names_from  = .metric,
                             values_from = .estimate)
      )
    ) %>%
    dplyr::select(wflow_id, final_metrics) %>%
    tidyr::unnest(final_metrics) -> result

  ## ---------------------------------------------------------------------------
  ## Step 5: Return result tibble
  ## ---------------------------------------------------------------------------

  return(result)

}
