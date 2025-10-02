#' Relative Root Mean Squared Error (RRMSE)
#'
#' A unitless regression metric defined as the RMSE divided by the mean of the observed values,
#' expressed as a percentage. Useful for comparing prediction accuracy across differently scaled
#' soil properties or other variables with varying magnitudes.
#'
#' @param data A data frame containing columns for observed and predicted values.
#' @param truth The column identifier for observed values (quoted or unquoted).
#' @param estimate The column identifier for predicted values (quoted or unquoted).
#' @param na_rm Logical. Should missing values be removed before computation? Default is `TRUE`.
#' @param ... Additional arguments passed to `rmse_vec()`.
#'
#' @return
#' `rrmse_vec()` returns a numeric value (percentage).
#' `rrmse` is a yardstick metric usable in `metric_set()` pipelines.
#'
#' @details
#' RRMSE is especially appropriate in spectral modeling contexts where different soil properties
#' (e.g., clay %, SOC, CEC) vary in scale. By expressing RMSE as a proportion of the mean of the
#' true values, it facilitates fairer performance comparisons across models and targets.
#'
#' @seealso
#' [yardstick::rmse_vec()], [yardstick::metric_set()]
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
#' metric_set(rrmse, rsq)(data, truth = obs, estimate = pred)
#'
#' @importFrom yardstick rmse_vec new_numeric_metric numeric_metric_summarizer
#' @importFrom rlang enquo
#'
#' @name rrmse_vec
#' @aliases rrmse
#' @keywords internal
#' @export

## -----------------------------------------------------------------------------
## rrmse_vec - Core Vector Function with Inline Implementation
## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------
## rrmse - Metric Constructor
## -----------------------------------------------------------------------------

#' @rdname rrmse_vec
#' @export
rrmse <- yardstick::new_numeric_metric(
  fn        = rrmse_vec,
  direction = "minimize"
)
