#' Filter Tuned Workflows Based on RRMSE and R² Thresholds
#'
#' Evaluates and filters tuned workflows based on quantile-derived thresholds for
#' Relative Root Mean Squared Error (RRMSE) and R-squared (R²). This function is typically
#' used after an initial `tune_grid()` search within a `workflow_set()`. It computes
#' the mean RRMSE and R² across resamples for each workflow, determines dynamic thresholds
#' based on user-specified quantiles, and filters out models that do not meet both criteria.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import workflowsets
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_extract
#' @importFrom stats quantile
#'
#' @param wf_set_tuned A tuned `workflow_set` object (from `workflow_map(tune_grid)`)
#'   containing resampling results for multiple model/recipe configurations.
#' @param rrmse_quintile A numeric value between 0 and 1 indicating the RRMSE quantile
#'   to use as the upper threshold (e.g., 0.6 means keeping the top 60% of performers).
#' @param r2_quintile A numeric value between 0 and 1 indicating the R² quantile
#'   to use as the lower threshold (e.g., 0.4 means keeping the top 60% of performers).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{passed_workflows}{A `workflow_set` object containing only workflows that met both criteria.}
#'   \item{rejected_workflows}{A tibble of workflows that were filtered out.}
#'   \item{metrics_summary}{A tibble summarizing average RRMSE and R² for each workflow.}
#'   \item{threshold_summary}{A tibble containing the computed RRMSE and R² thresholds.}
#' }
#'
#' @seealso \code{\link[tune]{tune_grid}}, \code{\link[workflowsets]{workflow_map}}, \code{\link{rrmse_vec}}
#'
#' @examples
#' \dontrun{
#' filtered <- filter_workflows(wf_set_tuned = my_tuned_workflows,
#'                              rrmse_quintile = 0.6,
#'                              r2_quintile = 0.4)
#' }
#'
#' @keywords internal

filter_workflows <- function(wf_set_tuned,
                             rrmse_quintile = 0.6,
                             r2_quintile    = 0.4) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Compute average cross-validation metrics per workflow
  ## ---------------------------------------------------------------------------

  wf_set_tuned %>%
    dplyr::mutate(metrics = purrr::map(.x = .data$result, .f = collect_metrics)) %>%
    tidyr::unnest(.data$metrics) %>%
    dplyr::filter(.data$.metric %in% c("rrmse", "rsq")) %>%
    dplyr::group_by(.data$wflow_id, .data$.metric) %>%
    dplyr::summarize(mean = mean(.data$mean, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$.metric, values_from = .data$mean) -> workflow_metrics

  ## ---------------------------------------------------------------------------
  ## Step 2: Define dynamic filtering thresholds from quantiles
  ## ---------------------------------------------------------------------------

  rrmse_threshold <- stats::quantile(workflow_metrics$rrmse,
                                     probs = rrmse_quintile,
                                     na.rm = TRUE)

  r2_threshold <- stats::quantile(workflow_metrics$rsq,
                                  probs = r2_quintile,
                                  na.rm = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 3: Tag each workflow as passed or rejected based on thresholds
  ## ---------------------------------------------------------------------------

  wf_set_tuned %>%
    dplyr::mutate(metrics = purrr::map(.x = .data$result,
                                       .f = collect_metrics),
                  passed = purrr::map_lgl(.x = .data$metrics,
                                          .f = ~ {rrmse <- dplyr::filter(.x, .data$.metric == "rrmse") %>%
                                                            dplyr::pull(.data$mean) %>%
                                                            mean(na.rm = TRUE)

                                                  r2    <- dplyr::filter(.x, .data$.metric == "rsq") %>%
                                                            dplyr::pull(.data$mean) %>%
                                                            mean(na.rm = TRUE)

                    !is.na(rrmse) && !is.na(r2) && rrmse <= rrmse_threshold && r2 >= r2_threshold})) -> workflow_summary

  ## ---------------------------------------------------------------------------
  ## Step 4: Subset passed and rejected workflows
  ## ---------------------------------------------------------------------------

  passed_workflows   <- dplyr::filter(workflow_summary, .data$passed)
  rejected_workflows <- dplyr::filter(workflow_summary, !.data$passed)

  ## ---------------------------------------------------------------------------
  ## Step 5: Preserve workflow_set class and construct summary
  ## ---------------------------------------------------------------------------

  class(passed_workflows) <- c("workflow_set", class(passed_workflows))

  tibble::tibble(Variable        = stringr::str_extract(string = unique(workflow_metrics$wflow_id), pattern = "^[^_]+"),
                 RRMSE_Threshold = rrmse_threshold,
                 RSQ_Threshold   = r2_threshold) -> threshold_summary

  ## ---------------------------------------------------------------------------
  ## Step 6: Return filtered results and summary
  ## ---------------------------------------------------------------------------

  return(list(
    passed_workflows   = passed_workflows,
    rejected_workflows = rejected_workflows,
    metrics_summary    = workflow_metrics,
    threshold_summary  = threshold_summary
  ))
}
