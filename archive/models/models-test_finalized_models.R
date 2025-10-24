#' Evaluate Final Models on Holdout Data
#'
#' Applies each finalized workflow to a held-out dataset, optionally back-transforms
#' predictions (e.g., exponentiating log-transformed outputs), and computes evaluation
#' metrics including R², RMSE, and RRMSE. Assumes the target variable is named \code{Response}.
#'
#' @param finalized_wf_sets A tibble of fitted workflows, typically returned by
#'        \code{full_model_evaluation()}, with at least two columns:
#'        \code{fitted_wf} (a fitted workflow object) and \code{wflow_id} (string ID).
#' @param holdout_data A data frame including columns \code{Sample_ID} and \code{Response},
#'        along with any required predictors or covariates.
#'
#' @return A tibble with one row per workflow ID and columns:
#'   \itemize{
#'     \item{\code{wflow_id}}{Identifier for the workflow}
#'     \item{\code{rsq}}{R²: Coefficient of determination}
#'     \item{\code{rmse}}{Root Mean Squared Error}
#'     \item{\code{rrmse}}{Relative RMSE as a percentage}
#'   }
#'
#' @details
#' Back-transformations are inferred from the workflow ID string:
#' \code{"Log"} triggers \code{exp()}, and \code{"Sqrt"} triggers squaring.
#' This function assumes a consistent naming scheme from \code{clean_workflow_id()}.
#'
#' @seealso
#'   \code{\link{rrmse_vec}}, \code{\link[yardstick]{metric_set}},
#'   \code{\link{full_model_evaluation}}, \code{\link[workflowsets]{workflow_set}}
#'
#' @examples
#' \dontrun{
#' evaluate_final_models(
#'   finalized_wf_sets = fitted_model_tbl,
#'   holdout_data      = test_samples
#' )
#' }
#'
#' @importFrom dplyr mutate select rename case_when
#' @importFrom purrr map map2
#' @importFrom tidyr pivot_wider unnest
#' @importFrom yardstick metric_set rsq rmse
#' @importFrom cli cli_abort
#' @importFrom tibble tibble

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
