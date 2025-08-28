#' Ratio of Performance to Deviation (RPD)
#'
#' @description
#' A dimensionless regression metric commonly used in spectroscopy and soil science.
#' RPD is the ratio of the standard deviation of observed values to the root mean
#' squared error (RMSE). Higher values indicate better model performance.
#'
#' @param truth For \code{rpd_vec()}, a numeric vector of observed values. 
#'   For \code{rpd()}, the column identifier for observed values (quoted or unquoted).
#' @param estimate For \code{rpd_vec()}, a numeric vector of predicted values.
#'   For \code{rpd()}, the column identifier for predicted values (quoted or unquoted).
#' @param data (Only for \code{rpd()}) A data frame containing columns for observed and predicted values.
#' @param na_rm Logical. Should missing values be removed before computation? Default is \code{TRUE}.
#' @param ... Additional arguments passed to \code{rmse_vec()}.
#'
#' @return
#' \code{rpd_vec()} returns a numeric value.
#' \code{rpd} is a yardstick metric usable in \code{metric_set()} pipelines.
#'
#' @details
#' RPD is widely used in spectroscopy for model quality assessment:
#' \itemize{
#'   \item RPD < 1.5: Poor model, not recommended for prediction
#'   \item RPD 1.5-2.0: Fair model, may distinguish between high and low values
#'   \item RPD 2.0-2.5: Good model, suitable for quantitative predictions
#'   \item RPD > 2.5: Excellent model, suitable for quality control
#' }
#'
#' The formula is: RPD = SD(observed) / RMSE(observed, predicted)
#'
#' @seealso
#' \code{\link[yardstick]{rmse_vec}}, \code{\link[yardstick]{metric_set}}, \code{\link{rrmse}}
#'
#' @examples
#' \dontrun{
#' library(yardstick)
#' data <- tibble::tibble(obs = c(100, 120, 140, 160), pred = c(105, 118, 135, 162))
#' rpd_vec(data, truth = obs, estimate = pred)
#' 
#' # Use in metric_set
#' metric_set(rpd, rsq, rmse)(data, truth = obs, estimate = pred)
#' }
#'
#' @importFrom yardstick rmse_vec new_numeric_metric numeric_metric_summarizer
#' @importFrom rlang enquo
#' @importFrom stats sd
#'
#' @name rpd_vec
#' @aliases rpd
#' @export

rpd_vec <- function(truth,
                    estimate,
                    na_rm = TRUE,
                    ...) {
  
  # Remove NAs if requested
  if (na_rm) {
    complete_cases <- complete.cases(truth, estimate)
    truth <- truth[complete_cases]
    estimate <- estimate[complete_cases]
  }
  
  # Calculate RMSE and SD
  rmse_val <- yardstick::rmse_vec(truth, estimate, na_rm = FALSE)
  sd_val   <- sd(truth, na.rm = FALSE)  # Already removed NAs above
  
  # Handle edge case where RMSE is zero (perfect predictions)
  if (rmse_val < .Machine$double.eps) {
    return(Inf)
  }
  
  sd_val / rmse_val
}

rpd_impl <- function(data,
                     truth,
                     estimate,
                     na_rm = TRUE,
                     ...) {
  
  yardstick::numeric_metric_summarizer(
    name  = "rpd",
    fn    = rpd_vec,
    data      = data,
    truth     = !!rlang::enquo(truth),
    estimate  = !!rlang::enquo(estimate),
    na_rm     = na_rm,
    ...
  )
}

#' @rdname rpd_vec
#' @export
rpd <- yardstick::new_numeric_metric(
  fn        = rpd_impl,
  direction = "maximize"  # Higher RPD is better
)