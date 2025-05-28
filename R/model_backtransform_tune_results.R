#' Back-transform Predictions in tune_results Object
#'
#' Applies inverse transformations to `.pred` column in each tuning result based on `wflow_id`.
#' Designed for use in stacked ensembles where predictions must be on the original scale.
#'
#' @param tune_result A `tune_results` object (e.g., from `tune_grid()` or `tune_bayes()`).
#' @param wflow_id A character string indicating the transformation (e.g., "Log", "Sqrt").
#'
#' @return A `tune_results` object with updated predictions (if present).
backtransform_tune_results <- function(tune_result, wflow_id) {
  if (!"predictions" %in% names(tune_result)) return(tune_result)

  tune_result$.predictions <- tune_result$.predictions %>%
    dplyr::mutate(.pred = dplyr::case_when(
      grepl("Log", wflow_id)  ~ exp(.pred),
      grepl("Sqrt", wflow_id) ~ (.pred)^2,
      TRUE                    ~ .pred
    ))

  return(tune_result)
}
