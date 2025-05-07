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
#' @importFrom hardhat extract_parameter_set_dials
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
  ## Step 1: Remove any workflows with unknown parameters
  ## ---------------------------------------------------------------------------


  wf_set_tuned <- wf_set_tuned %>%
    dplyr::mutate(
      has_unknown_params = purrr::map_lgl(info, ~ {
        wf <- try(.x$workflow[[1]], silent = TRUE)
        param_set <- try(hardhat::extract_parameter_set_dials(wf), silent = TRUE)
        if (inherits(param_set, "try-error")) return(TRUE)
        any(purrr::map_lgl(param_set$object, ~ inherits(.x, "unknown")))
      })
    )

  # Remove them
  wf_set_tuned <- wf_set_tuned %>%
    dplyr::filter(!has_unknown_params)

  dplyr::filter(wf_set_tuned, has_unknown_params) %>%
    dplyr::pull(wflow_id) -> unknowns

  if (length(unknowns) > 0) {
    cli::cli_alert_warning("Workflows with unknown parameters will be excluded:")
    cli::cli_ul()
    cli::cli_li(unknowns)
    cli::cli_end()
  }

  wf_set_tuned %>%
    dplyr::filter(!.data$has_unknown_params) -> wf_set_tuned

  ## ---------------------------------------------------------------------------
  ## Step 2: Safely compute average cross-validation metrics per workflow
  ## ---------------------------------------------------------------------------

  safe_collect <- purrr::possibly(collect_metrics, otherwise = NULL)

  wf_set_tuned %>%
    dplyr::mutate(metrics = purrr::map(.x = .data$result, .f = safe_collect)) %>%
    dplyr::filter(!purrr::map_lgl(.data$metrics, is.null)) %>%
    tidyr::unnest(.data$metrics) %>%
    dplyr::filter(.data$.metric %in% c("rrmse", "rsq")) %>%
    dplyr::group_by(.data$wflow_id, .data$.metric) %>%
    dplyr::summarize(mean = mean(.data$mean, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$.metric, values_from = .data$mean) -> workflow_metrics

  ## ---------------------------------------------------------------------------
  ## Step 3: Define dynamic filtering thresholds from quantiles
  ## ---------------------------------------------------------------------------

  rrmse_threshold <- stats::quantile(workflow_metrics$rrmse,
                                     probs = rrmse_quintile,
                                     na.rm = TRUE)

  r2_threshold <- stats::quantile(workflow_metrics$rsq,
                                  probs = r2_quintile,
                                  na.rm = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 4: Tag each workflow as passed or rejected based on thresholds
  ## ---------------------------------------------------------------------------

  workflow_summary <- wf_set_tuned %>%
    dplyr::mutate(metrics = purrr::map(.x = .data$result, .f = safe_collect),
                  passed = purrr::map_lgl(.x = .data$metrics,
                                          .f = ~ {
                                            if (is.null(.x)) return(FALSE)
                                            rrmse <- dplyr::filter(.x, .data$.metric == "rrmse") %>%
                                              dplyr::pull(.data$mean) %>%
                                              mean(na.rm = TRUE)

                                            r2 <- dplyr::filter(.x, .data$.metric == "rsq") %>%
                                              dplyr::pull(.data$mean) %>%
                                              mean(na.rm = TRUE)

                                            !is.na(rrmse) && !is.na(r2) &&
                                              rrmse <= rrmse_threshold &&
                                              r2 >= r2_threshold
                                          }))

  ## ---------------------------------------------------------------------------
  ## Step 5: Subset passed and rejected workflows
  ## ---------------------------------------------------------------------------

  passed_workflows   <- dplyr::filter(workflow_summary, .data$passed)
  rejected_workflows <- dplyr::filter(workflow_summary, !.data$passed)

  ## ---------------------------------------------------------------------------
  ## Step 5: Preserve workflow_set class and construct summary
  ## ---------------------------------------------------------------------------

  class(passed_workflows) <- c("workflow_set", class(passed_workflows))

  tibble::tibble(
    Variable        = stringr::str_extract(string = unique(workflow_metrics$wflow_id), pattern = "^[^_]+"),
    RRMSE_Threshold = rrmse_threshold,
    RSQ_Threshold   = r2_threshold
  ) -> threshold_summary

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

