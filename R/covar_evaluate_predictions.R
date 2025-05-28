#' Evaluate Predicted Covariate Values Against Measured Data
#'
#' Joins modeled and measured covariate values by \code{Sample_ID}, computes evaluation metrics
#' for each covariate independently, and returns a tidy summary of model performance. Metrics are
#' computed using \code{\link[soilspec]{eval}}, including RMSE, R², CCC, and RPIQ.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import cli
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param measured_data A data frame containing observed covariate values with columns:
#'   \itemize{
#'     \item \code{Sample_ID} (character or integer)
#'     \item One or more numeric columns corresponding to measured covariates.
#'   }
#' @param modeled_data A data frame containing model predictions with matching \code{Sample_ID}
#' and corresponding predicted covariate columns.
#'
#' @return A tibble summarizing evaluation metrics per covariate, with columns including:
#' \item{Covariate}{The covariate name}
#' \item{RMSE}{Root Mean Squared Error}
#' \item{R2}{Coefficient of Determination}
#' \item{CCC}{Concordance Correlation Coefficient}
#' \item{RPIQ}{Ratio of Performance to Inter-Quartile Distance}
#' and others.
#'
#' @details
#' Non-numeric columns or missing predictions are automatically skipped, and a warning
#' is printed using \code{\link[cli]{cli_alert_warning}} if evaluation cannot be performed
#' for a particular covariate.
#'
#' @examples
#' \dontrun{
#' results <- evaluate_predictions(measured_data = my_measured_data,
#'                                 modeled_data  = my_predicted_data)
#' }
#'
#' @seealso
#' \code{\link[soilspec]{eval}}
#'
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
