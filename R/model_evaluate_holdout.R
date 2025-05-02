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
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import yardstick
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
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
#' @seealso \code{\link[yardstick]{metric_set}}, \code{\link{rrmse_vec}}, \code{\link{full_model_evaluation}}
#'
#' @examples
#' \dontrun{
#' evaluate_final_models(finalized_wf_sets = finalized_models,
#'                       holdout_data = test_data)
#' }
#'
#' @keywords internal

evaluate_final_models <- function(finalized_wf_sets,
                                  holdout_data) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  if (!"fitted_wf" %in% names(finalized_wf_sets)) {
    cli::cli_alert_danger("Input {.arg finalized_wf_sets} must include a {.field fitted_wf} column.")
    stop("Aborting: Missing `fitted_wf` in finalized workflow set.")
  }

  if (!"Response" %in% colnames(holdout_data)) {
    cli::cli_alert_danger("Input {.arg holdout_data} must include a {.field Response} column.")
    stop("Aborting: Missing `Response` column in holdout data.")
  }

  eval_metrics <- yardstick::metric_set(rsq, rrmse, rmse)

  ## ---------------------------------------------------------------------------
  ## Step 2: Predict and Evaluate
  ## ---------------------------------------------------------------------------

  finalized_wf_sets %>%

    ## -------------------------------------------------------------------------
    ## Stage 1: Bake holdout data using each recipe
    ## -------------------------------------------------------------------------

    dplyr::mutate(baked_data = purrr::map(.x = fitted_wf,
                                          .f = ~ { recipe <- hardhat::extract_recipe(.x)
                                     recipes::bake(recipe, new_data = holdout_data)})) %>%

    ## -------------------------------------------------------------------------
    ## Stage 2: Predict from each model
    ## -------------------------------------------------------------------------

    dplyr::mutate(predictions = purrr::map2(.x = fitted_wf,
                                            .y = baked_data,
                                            .f = ~ {stats::predict(hardhat::extract_fit_parsnip(.x), new_data = .y) %>%
                                      dplyr::rename(estimate = .pred)})) %>%

    ## -------------------------------------------------------------------------
    ## Stage 3: Back-transform predictions based on workflow ID
    ## -------------------------------------------------------------------------

    dplyr::mutate(predictions = purrr::map2(.x = predictions,
                                            .y = wflow_id,
                                            .f = ~ {.x$estimate <- dplyr::case_when(grepl("Log", .y)  ~ exp(.x$estimate),
                                                                                    grepl("Sqrt", .y) ~ (.x$estimate)^2,
                                                                                    TRUE              ~ .x$estimate)

                                                    tibble::tibble(Sample_ID = holdout_data$Sample_ID,
                                                                   estimate  = .x$estimate,
                                                                    truth     = holdout_data$Response)})) %>%

    ## -------------------------------------------------------------------------
    ## Stage 4: Compute final metrics
    ## -------------------------------------------------------------------------

    dplyr::mutate(final_metrics = purrr::map(.x = predictions,
                                             .f = ~ {eval_metrics(data     = .x,
                                                                  truth    = truth,
                                                                  estimate = estimate) %>%
                                       dplyr::select(-.estimator) %>%
                                       tidyr::pivot_wider(names_from  = .metric,
                                                          values_from = .estimate)})) %>%

  ## -------------------------------------------------------------------------
  ## Step 3: Return summary table
  ## -------------------------------------------------------------------------

  dplyr::select(wflow_id, final_metrics) %>%
  tidyr::unnest(final_metrics) -> model_eval_stats

  return(model_eval_stats)

}
