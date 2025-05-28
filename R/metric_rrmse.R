#' Relative Root Mean Squared Error (RRMSE)
#'
#' A custom regression metric for evaluating model accuracy relative to the scale
#' of the true values. RRMSE is computed as RMSE divided by the mean of the observed
#' values and expressed as a percentage. It is unitless, bounded below by 0, and useful
#' for comparing model performance across differently scaled variables.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data enquo
#' @importFrom yardstick numeric_metric_summarizer rmse_vec new_numeric_metric
#'
#' @param data A data frame with columns for observed and predicted values.
#' @param truth The column identifier for observed values (quoted or unquoted).
#' @param estimate The column identifier for predicted values (quoted or unquoted).
#' @param na_rm Logical; should missing values be removed before computation? Default is TRUE.
#' @param ... Additional arguments passed to `rmse_vec()`.
#'
#' @return \code{rrmse_vec()} returns a numeric value indicating the relative RMSE as a percentage.
#' \code{rrmse} returns a \code{yardstick} metric function that can be used in metric sets.
#'
#' @details
#' This function is useful for evaluating model performance on variables that span different
#' units or orders of magnitude. It provides an interpretable, unitless error measure that
#' facilitates fair model comparison across multiple targets.
#'
#' @seealso
#' \code{\link[yardstick]{rmse_vec}} for root mean squared error,
#' \code{\link[yardstick]{metric_set}} for bundling metrics.
#'
#' @examples
#' rrmse_vec(
#'   data = data.frame(truth = c(1, 2, 3), estimate = c(1.1, 1.9, 3.2)),
#'   truth = truth,
#'   estimate = estimate
#' )
#'
#' library(yardstick)
#' data <- tibble::tibble(obs = c(100, 120, 140), pred = c(105, 118, 135))
#' rrmse_vec(data, truth = obs, estimate = pred)
#'
#' # Use as part of a yardstick metric set
#' metric_set(rrmse, rsq)(data, truth = obs, estimate = pred)
#'
#' @name rrmse_vec
#' @aliases rrmse
#' @keywords internal
#' @export
#'
rrmse_vec <- function(data,
                      truth,
                      estimate,
                      na_rm = TRUE,
                      ...) {

  yardstick::numeric_metric_summarizer(
    name  = "rrmse",
    fn    = function(truth, estimate, ...) {
      rmse_val <- yardstick::rmse_vec(truth, estimate, ...)
      mean_val <- mean(truth, na.rm = na_rm)
      100 * (rmse_val / mean_val)
    },
    data      = data,
    truth     = !!rlang::enquo(truth),
    estimate  = !!rlang::enquo(estimate),
    na_rm     = na_rm,
    ...
  )
}

#' @rdname rrmse_vec
#' @export
rrmse <- yardstick::new_numeric_metric(
  fn        = rrmse_vec,
  direction = "minimize"
)
