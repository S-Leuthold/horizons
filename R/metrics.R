# R/metrics.R
# Custom yardstick metrics for spectroscopic modeling: RPD, RRMSE, CCC.
# All follow the yardstick three-layer pattern: _vec, _impl, constructor.

## ===========================================================================
## RPD — Ratio of Performance to Deviation
## ===========================================================================

#' Custom yardstick metrics for spectroscopic modeling
#'
#' @description
#' Regression metrics commonly used to evaluate spectroscopic calibrations,
#' implemented as `yardstick` metrics so they compose with the rest of the
#' tidymodels evaluation stack. Each metric provides the standard three-layer
#' interface: a vectorized `*_vec()` function, a data-frame method, and a
#' `yardstick` metric object.
#'
#' * **RPD** (ratio of performance to deviation): the standard deviation of
#'   the observed values divided by the RMSE of prediction. Higher is better.
#' * **RRMSE** (relative root mean squared error): the RMSE expressed relative
#'   to the mean of the observed values. Lower is better.
#' * **CCC** (Lin's concordance correlation coefficient): agreement between
#'   observed and predicted values, combining precision and accuracy. Ranges
#'   from -1 to 1, with 1 indicating perfect concordance.
#'
#' @return
#' The `*_vec()` functions return a single numeric value. The metric objects
#' (`rpd`, `rrmse`, `ccc`) and their data-frame methods return a one-row
#' tibble in the standard `yardstick` format (`.metric`, `.estimator`,
#' `.estimate`).
#'
#' @name horizons_metrics
NULL

#' @rdname horizons_metrics
#' @param truth Numeric vector of observed values.
#' @param estimate Numeric vector of predicted values.
#' @param na_rm Logical. Remove NAs before computation? Default TRUE.
#' @param ... Not used.
#' @export
rpd_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  if (na_rm) {

    complete <- stats::complete.cases(truth, estimate)
    truth    <- truth[complete]
    estimate <- estimate[complete]

  }

  rmse_val <- yardstick::rmse_vec(truth, estimate, na_rm = FALSE)
  sd_val   <- stats::sd(truth, na.rm = FALSE)

  ## NA propagation (NAs present and na_rm = FALSE)
  if (is.na(rmse_val) || is.na(sd_val)) return(NA_real_)

  ## Perfect predictions → Inf (RMSE = 0)
  if (rmse_val < .Machine$double.eps) return(Inf)

  ## Constant truth → SD = 0 → RPD = 0
  if (sd_val < .Machine$double.eps) return(0)

  sd_val / rmse_val

}

## -------------------------------------------------------------------------

rpd_impl <- function(data, truth, estimate, na_rm = TRUE, ...) {

  yardstick::numeric_metric_summarizer(
    name     = "rpd",
    fn       = rpd_vec,
    data     = data,
    truth    = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm    = na_rm,
    ...
  )

}

## -------------------------------------------------------------------------

#' @rdname horizons_metrics
#' @param data A data frame with truth and estimate columns.
#' @export
rpd <- yardstick::new_numeric_metric(
  fn        = rpd_impl,
  direction = "maximize"
)

## ===========================================================================
## RRMSE — Relative Root Mean Squared Error
## ===========================================================================

#' @rdname horizons_metrics
#' @export
rrmse_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  if (na_rm) {

    complete <- stats::complete.cases(truth, estimate)
    truth    <- truth[complete]
    estimate <- estimate[complete]

  }

  rmse_val <- yardstick::rmse_vec(truth, estimate, na_rm = FALSE)
  mean_val <- mean(truth, na.rm = FALSE)

  ## Guard against division by zero (mean of truth is 0)
  if (is.na(mean_val) || abs(mean_val) < .Machine$double.eps) return(NA_real_)

  100 * rmse_val / mean_val

}

## -------------------------------------------------------------------------

rrmse_impl <- function(data, truth, estimate, na_rm = TRUE, ...) {

  yardstick::numeric_metric_summarizer(
    name     = "rrmse",
    fn       = rrmse_vec,
    data     = data,
    truth    = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm    = na_rm,
    ...
  )

}

## -------------------------------------------------------------------------

#' @rdname horizons_metrics
#' @export
rrmse <- yardstick::new_numeric_metric(
  fn        = rrmse_impl,
  direction = "minimize"
)

## ===========================================================================
## CCC — Lin's Concordance Correlation Coefficient
## ===========================================================================

#' @rdname horizons_metrics
#' @export
ccc_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  if (na_rm) {

    complete <- stats::complete.cases(truth, estimate)
    truth    <- truth[complete]
    estimate <- estimate[complete]

  }

  if (length(truth) < 2) return(NA_real_)

  mean_truth    <- mean(truth)
  mean_estimate <- mean(estimate)
  sd_truth      <- stats::sd(truth)
  sd_estimate   <- stats::sd(estimate)

  ## Either vector constant → agreement undefined
  if (sd_truth < .Machine$double.eps || sd_estimate < .Machine$double.eps) {

    return(NA_real_)

  }

  ## Pearson r (precision)
  rho <- stats::cor(truth, estimate)

  ## Bias correction factor (accuracy)
  v   <- sd_estimate / sd_truth
  u   <- (mean_estimate - mean_truth) / sqrt(sd_estimate * sd_truth)
  C_b <- 2 / (v + 1 / v + u^2)

  ## CCC = precision * accuracy
  rho * C_b

}

## -------------------------------------------------------------------------

ccc_impl <- function(data, truth, estimate, na_rm = TRUE, ...) {

  yardstick::numeric_metric_summarizer(
    name     = "ccc",
    fn       = ccc_vec,
    data     = data,
    truth    = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm    = na_rm,
    ...
  )

}

## -------------------------------------------------------------------------

#' @rdname horizons_metrics
#' @export
ccc <- yardstick::new_numeric_metric(
  fn        = ccc_impl,
  direction = "maximize"
)
