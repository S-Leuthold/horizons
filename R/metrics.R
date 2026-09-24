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

## ===========================================================================
## tuning_metric_set — the metric set handed to tune, scored on the original
## response scale
## ===========================================================================

#' Build the Metric Set Used During Hyperparameter Tuning
#'
#' @description
#' Returns the `yardstick::metric_set()` that `evaluate_single_config()` and
#' `fit_single_config()` hand to `tune::tune_grid()` and `tune::tune_bayes()`.
#' For `transformation = "none"` it is the plain set of the requested metrics.
#' For `"log"`, `"log10"` and `"sqrt"` every metric is wrapped so that the
#' estimate is back-transformed with [back_transform_predictions()] before it
#' is compared to the truth, and the whole set is therefore scored on the
#' original response scale.
#'
#' @details
#' **Why this exists.** `build_recipe()` applies the response transformation
#' as a recipe step with `skip = TRUE`, which is the correct setting for a
#' transformation that must not run at predict time. But `skip = TRUE` also
#' means the step never runs when tune bakes an assessment set, so the
#' `truth` column tune scores against stays on the original scale while the
#' model's predictions are on the transformed scale. Without this wrapper every
#' tuning metric is a cross-scale comparison: `select_best()`, the
#' `tune_bayes()` acquisition target, and the prune gate all optimise a number
#' dominated by the scale offset rather than by model quality (issue #49).
#'
#' Scoring on the original scale, rather than forward-transforming `truth` to
#' score on the transformed scale, is a deliberate choice: the leaderboard, the
#' prune threshold and the reported test metrics are all original-scale, so
#' hyperparameters are selected on the same quantity that is reported. It also
#' means selection sees retransformation bias, which a transformed-scale score
#' never would.
#'
#' The structural alternative, transforming the outcome column before the
#' recipe so tune sees one scale by construction, is the tidymodels-canonical
#' shape and is deferred until after the JOSS submission because it changes
#' every reader of `collect_predictions()`, including the conformal residuals.
#' This factory compensates for the seam; it does not remove it.
#'
#' Metric names and directions are preserved exactly (`.metric == "rmse"`
#' stays `"rmse"`, `rpd` stays `"maximize"`), so callers that select or rank
#' by name need no change. The first metric in `metrics` is the one
#' `tune_bayes()` optimises; keep `"rmse"` first where that matters.
#'
#' @param transformation Character scalar: `"none"`, `"log"`, `"log10"` or
#'   `"sqrt"`. The same vocabulary as [back_transform_predictions()]. Unknown
#'   values abort rather than silently scoring cross-scale.
#' @param metrics Character vector naming the metrics to include, in order.
#'   Any of `"rmse"`, `"rrmse"`, `"rsq"`, `"mae"`, `"rpd"`, `"ccc"`.
#' @param outcome_range Numeric length-2 vector. The outcome's physical range,
#'   which the back-transformed estimate is clamped to, as
#'   [back_transform_predictions()] clamps every scored prediction. Read only
#'   for a transformed response; the `"none"` set scores the estimate as tune
#'   produced it, as it did before the range existed. Default `c(0, Inf)`.
#'
#' @return A `yardstick` metric set.
#' @seealso [back_transform_predictions()], [compute_original_scale_metrics()]
#' @keywords internal
tuning_metric_set <- function(transformation,
                              metrics = c("rmse", "rrmse", "rsq", "mae", "rpd", "ccc"),
                              outcome_range = DEFAULT_OUTCOME_RANGE) {

  ## -------------------------------------------------------------------------
  ## Validate inputs
  ## -------------------------------------------------------------------------

  if (length(transformation) != 1 || is.na(transformation)) {

    cli::cli_abort(
      "{.arg transformation} must be a single non-missing string, not {.val {transformation}}."
    )

  }

  transformation <- tolower(as.character(transformation))
  valid_transformations <- c("none", "log", "log10", "sqrt")

  if (!transformation %in% valid_transformations) {

    cli::cli_abort(c(
      "Unknown {.arg transformation} {.val {transformation}}.",
      "i" = "Expected one of {.val {valid_transformations}}."
    ))

  }

  registry <- list(
    rmse  = list(vec = yardstick::rmse_vec, direction = "minimize", plain = yardstick::rmse),
    rrmse = list(vec = rrmse_vec,           direction = "minimize", plain = rrmse),
    rsq   = list(vec = yardstick::rsq_vec,  direction = "maximize", plain = yardstick::rsq),
    mae   = list(vec = yardstick::mae_vec,  direction = "minimize", plain = yardstick::mae),
    rpd   = list(vec = rpd_vec,             direction = "maximize", plain = rpd),
    ccc   = list(vec = ccc_vec,             direction = "maximize", plain = ccc)
  )

  unknown_metrics <- setdiff(metrics, names(registry))

  if (length(unknown_metrics) > 0) {

    cli::cli_abort(c(
      "Unknown metric{?s} {.val {unknown_metrics}} in {.arg metrics}.",
      "i" = "Available: {.val {names(registry)}}."
    ))

  }

  if (length(metrics) == 0) {

    cli::cli_abort("{.arg metrics} must name at least one metric.")

  }

  ## Checking the range also forces it. The wrapped metrics close over it, and
  ## tune may serialize them to workers: an unforced promise would carry the
  ## caller's frame, training data and all, along with it.
  if (!is_valid_outcome_range(outcome_range)) {

    cli::cli_abort(c(
      "{.arg outcome_range} must be a numeric vector of length 2 with lower < upper.",
      "x" = "Got {.val {outcome_range}}."
    ))

  }

  ## -------------------------------------------------------------------------
  ## No transformation: the plain metric set, exactly as before
  ## -------------------------------------------------------------------------

  if (!needs_back_transformation(transformation)) {

    plain <- lapply(registry[metrics], function(entry) entry$plain)
    names(plain) <- metrics

    return(do.call(yardstick::metric_set, plain))

  }

  ## -------------------------------------------------------------------------
  ## Transformed response: wrap each metric to back-transform the estimate
  ## -------------------------------------------------------------------------

  wrapped <- lapply(metrics, function(name) {

    make_original_scale_metric(
      name           = name,
      vec_fn         = registry[[name]]$vec,
      direction      = registry[[name]]$direction,
      transformation = transformation,
      outcome_range  = outcome_range
    )

  })

  names(wrapped) <- metrics

  do.call(yardstick::metric_set, wrapped)

}

## -------------------------------------------------------------------------

#' Wrap a *_vec metric so it scores a back-transformed estimate
#'
#' @param name Metric name, used verbatim as `.metric` in the output.
#' @param vec_fn The `*_vec()` implementation to score with.
#' @param direction `"minimize"` or `"maximize"`, as `tune` expects.
#' @param transformation Transformation to invert on the estimate.
#' @param outcome_range The range the back-transformed estimate is clamped to.
#' @return A `yardstick` numeric metric.
#' @keywords internal
#' @noRd
make_original_scale_metric <- function(name, vec_fn, direction, transformation,
                                       outcome_range = DEFAULT_OUTCOME_RANGE) {

  force(outcome_range)

  ### The estimate arrives on the transformed scale; the truth does not.
  ### Back-transform the estimate only, then score as usual.
  scaled_vec <- function(truth, estimate, na_rm = TRUE, ...) {

    estimate <- back_transform_predictions(estimate, transformation, warn = FALSE,
                                           outcome_range = outcome_range)
    vec_fn(truth, estimate, na_rm = na_rm, ...)

  }

  impl <- function(data, truth, estimate, na_rm = TRUE, ...) {

    yardstick::numeric_metric_summarizer(
      name     = name,
      fn       = scaled_vec,
      data     = data,
      truth    = !!rlang::enquo(truth),
      estimate = !!rlang::enquo(estimate),
      na_rm    = na_rm,
      ...
    )

  }

  yardstick::new_numeric_metric(fn = impl, direction = direction)

}
