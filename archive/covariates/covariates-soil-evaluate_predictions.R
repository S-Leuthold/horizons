#' Evaluate Predicted Covariates Against Measured Values
#'
#' Compares predicted and observed covariate values by `Sample_ID`, computes evaluation metrics
#' for each covariate independently, and returns a tidy summary of model performance. Metrics include
#' RMSE, R², CCC, RPIQ, and others via `soilspec::eval()`. Errors or non-numeric columns are gracefully skipped.
#'
#' @param measured_data A `data.frame` or `tibble` containing observed covariate values.
#'   Must include a `Sample_ID` column and one or more numeric columns representing measured covariates.
#' @param modeled_data A `data.frame` or `tibble` containing predicted covariate values.
#'   Must also include `Sample_ID` and matching column names for predicted covariates.
#'
#' @return A `tibble` summarizing evaluation metrics for each covariate. Includes columns:
#' \itemize{
#'   \item \strong{Covariate}: Name of the covariate being evaluated
#'   \item \strong{RMSE}: Root Mean Squared Error
#'   \item \strong{R2}: Coefficient of Determination
#'   \item \strong{CCC}: Concordance Correlation Coefficient
#'   \item \strong{RPIQ}: Ratio of Performance to Interquartile Distance
#'   \item Other metrics computed by `soilspec::eval()`
#' }
#'
#' @details
#' The function performs an inner join on `Sample_ID`, appending `_measured` and `_modeled` suffixes
#' to corresponding columns. Only numeric covariates with valid observed and predicted values are evaluated.
#' Any covariate with missing data or evaluation errors is skipped, with a warning logged via `cli::cli_alert_warning()`.
#'
#' Metrics are computed using `soilspec::eval()` with `obj = "quant"` and `na.rm = TRUE`.
#' This function is especially useful for assessing external validation or covariate prediction workflows.
#'
#' @examples
#' \dontrun{
#' results <- evaluate_predictions(
#'   measured_data = my_measured_data,
#'   modeled_data  = my_predicted_data
#' )
#' }
#'
#' @seealso
#' \code{\link[soilspec]{eval}}
#'
#' @importFrom dplyr distinct inner_join mutate bind_rows
#' @importFrom purrr map
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom cli cli_alert_warning
#' @export

evaluate_predictions <- function(measured_data,
                                 modeled_data) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Ensure unique entries by Sample_ID
  ## ---------------------------------------------------------------------------

  measured_data <- dplyr::distinct(measured_data, Sample_ID, .keep_all = TRUE)
  modeled_data  <- dplyr::distinct(modeled_data,  Sample_ID, .keep_all = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 2: Join modeled and measured data
  ## ---------------------------------------------------------------------------

  dplyr::inner_join(measured_data,
                    modeled_data,
                    by     = "Sample_ID",
                    suffix = c("_measured", "_modeled")) -> eval_data

  ## ---------------------------------------------------------------------------
  ## Step 3: Evaluate each covariate independently
  ## ---------------------------------------------------------------------------

  covariate_names <- setdiff(names(measured_data), "Sample_ID")

  purrr::map(covariate_names, function(covar) {

  observed  <- eval_data[[paste0(covar, "_measured")]]
  predicted <- eval_data[[paste0(covar, "_modeled")]]

  # Skip if either column is missing or non-numeric

  if (is.null(observed) || is.null(predicted)) return(NULL)

  if (!is.numeric(observed) || !is.numeric(predicted)) return(NULL)

  tibble::tibble(observed  = observed,
                 predicted = predicted) %>%
        tidyr::drop_na() -> eval_combo

  tryCatch({
    soilspec::eval(pred = eval_combo$predicted,
                   obs  = eval_combo$observed,
                   obj  = "quant",
                   na.rm = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Covariate = covar, .before = 1)

  }, error = function(e) {

    cli::cli_alert_warning("Skipping covariate {.val {covar}} due to error: {e$message}")
    return(NULL)

    })

}) %>% dplyr::bind_rows() -> results

  return(results)
}
