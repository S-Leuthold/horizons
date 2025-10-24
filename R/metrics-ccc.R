#' Lin's Concordance Correlation Coefficient (CCC)
#'
#' @description
#' A measure of agreement between observed and predicted values that combines
#' measures of both precision (correlation) and accuracy (deviation from the 45-degree line).
#' CCC is particularly useful in spectroscopy and soil science for evaluating model predictions.
#'
#' @param truth For `ccc_vec()`, a numeric vector of observed values.
#'   For `ccc()`, the column identifier for observed values (quoted or unquoted).
#' @param estimate For `ccc_vec()`, a numeric vector of predicted values.
#'   For `ccc()`, the column identifier for predicted values (quoted or unquoted).
#' @param data (Only for `ccc()`) A data frame containing columns for observed and predicted values.
#' @param na_rm Logical. Should missing values be removed before computation? Default is `TRUE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' `ccc_vec()` returns a numeric value between -1 and 1.
#' `ccc` is a yardstick metric usable in `metric_set()` pipelines.
#'
#' @details
#' Lin's CCC combines measures of precision and accuracy:
#' * CCC = ρ × C_b
#' * ρ (rho) = Pearson correlation coefficient (precision)
#' * C_b = Bias correction factor (accuracy)
#'
#' Interpretation:
#' * CCC = 1: Perfect agreement
#' * CCC > 0.90: Excellent agreement
#' * CCC 0.80-0.90: Good agreement
#' * CCC 0.65-0.80: Moderate agreement
#' * CCC < 0.65: Poor agreement
#'
#' The bias correction factor is calculated as:
#' C_b = 2 / ((v + 1/v + u²))
#' where v = σ_x/σ_y, u = (μ_x - μ_y)/√(σ_x × σ_y)
#'
#' @references
#' Lin, L. I. (1989). A concordance correlation coefficient to evaluate reproducibility.
#' Biometrics, 45(1), 255-268.
#'
#' @seealso
#' [yardstick::metric_set()], [rpd()], [rrmse()]
#'
#' @examples
#' \dontrun{
#' library(yardstick)
#' data <- tibble::tibble(obs = c(100, 120, 140, 160), pred = c(105, 118, 135, 162))
#' ccc_vec(data, truth = obs, estimate = pred)
#' 
#' # Use in metric_set
#' metric_set(ccc, rpd, rsq)(data, truth = obs, estimate = pred)
#' }
#'
#' @importFrom yardstick new_numeric_metric numeric_metric_summarizer
#' @importFrom rlang enquo
#' @importFrom stats cor sd var
#'
#' @name ccc_vec
#' @aliases ccc
#' @export

## -----------------------------------------------------------------------------
## ccc_vec - Core Vector Function
## -----------------------------------------------------------------------------

ccc_vec <- function(truth,
                    estimate,
                    na_rm = TRUE,
                    ...) {
  
  # Remove NAs if requested
  if (na_rm) {
    complete_cases <- complete.cases(truth, estimate)
    truth <- truth[complete_cases]
    estimate <- estimate[complete_cases]
  }
  
  # Check for sufficient data
  if (length(truth) < 2) {
    return(NA_real_)
  }
  
  # Calculate means and standard deviations
  mean_truth    <- mean(truth)
  mean_estimate <- mean(estimate)
  sd_truth      <- sd(truth)
  sd_estimate   <- sd(estimate)
  
  # Handle edge cases
  if (sd_truth < .Machine$double.eps || sd_estimate < .Machine$double.eps) {
    return(NA_real_)
  }
  
  # Calculate Pearson correlation coefficient (precision)
  rho <- cor(truth, estimate)
  
  # Calculate bias correction factor (accuracy)
  # v = ratio of standard deviations
  v <- sd_estimate / sd_truth
  
  # u = normalized difference in means
  u <- (mean_estimate - mean_truth) / sqrt(sd_estimate * sd_truth)
  
  # Bias correction factor
  C_b <- 2 / (v + 1/v + u^2)
  
  # Lin's CCC = precision × accuracy
  ccc_value <- rho * C_b
  
  return(ccc_value)
}

## -----------------------------------------------------------------------------
## ccc_impl - Yardstick Wrapper Implementation
## -----------------------------------------------------------------------------

ccc_impl <- function(data,
                     truth,
                     estimate,
                     na_rm = TRUE,
                     ...) {
  
  yardstick::numeric_metric_summarizer(
    name  = "ccc",
    fn    = ccc_vec,
    data      = data,
    truth     = !!rlang::enquo(truth),
    estimate  = !!rlang::enquo(estimate),
    na_rm     = na_rm,
    ...
  )
}

## -----------------------------------------------------------------------------
## ccc - Metric Constructor
## -----------------------------------------------------------------------------

#' @rdname ccc_vec
#' @export
ccc <- yardstick::new_numeric_metric(
  fn        = ccc_impl,
  direction = "maximize"  # Higher CCC is better (max = 1)
)