#' Back-transform Predictions to Original Scale
#'
#' @description
#' Inverts response transformations applied by `step_log(offset = 1)` or
#' `step_sqrt()` during recipe preprocessing. This is the single source of
#' truth for back-transformation logic in horizons.
#'
#' **Important:** `step_log(offset = 1)` computes `log(x + 1)`, so the correct
#' inverse is `exp(x) - 1`, NOT `exp(x)`. Similarly, `step_log(base = 10,
#' offset = 1)` computes `log10(x + 1)`, so the inverse is `10^x - 1`.
#'
#' Original-scale output is clamped to `outcome_range` for every
#' transformation, including `"none"`. The range is the outcome's physical
#' range, set by [configure()]; its default, `c(0, Inf)`, is the non-negative
#' floor the package has always applied, which is a physical constraint only
#' for non-negative properties. A signed property (δ13C, say) is configured
#' with `outcome_range = c(-Inf, Inf)` and is not floored at all (#76).
#' Clamping here means the evaluation, OOF and calibration paths score the
#' same predictions a deployed model would serve, rather than a vector that
#' differs from it wherever a model extrapolated past the range (#53). The
#' response bound is a different thing, applied by `predict()` to served
#' point predictions only ([apply_response_bound()]), because fit-time
#' ranking must see raw upper-tail behaviour.
#'
#' A negative value on the square-root scale is set to 0 before squaring, so
#' it cannot square into a wrong positive. That is correctness, not a
#' departure from what was asked, so it is silent, as the clamp is.
#'
#' @param predictions Numeric vector of predictions on the transformed scale.
#' @param transformation Character: "none", "log", "sqrt", or "log10".
#' @param outcome_range Numeric length-2 vector, lower < upper, either end
#'   possibly infinite: the outcome's physical range, which every
#'   original-scale value is clamped to. A `-Inf` lower bound applies no
#'   floor and an `Inf` upper bound no cap. Default `DEFAULT_OUTCOME_RANGE`,
#'   `c(0, Inf)`, the non-negative floor.
#'
#' @return Numeric vector on the original response scale, clamped to
#'   `outcome_range`.
#' @keywords internal
back_transform_predictions <- function(predictions, transformation,
                                       outcome_range = DEFAULT_OUTCOME_RANGE) {

  if (is.null(predictions) || length(predictions) == 0) return(predictions)

  ### outcome_range and transformation come from outcome_range_setting() and
  ### the configuration grid, which every verb's door validator has checked.

  transformation <- tolower(as.character(transformation))

  out <- switch(transformation,

    "none" = predictions,

    "log" = exp(predictions) - 1,

    "sqrt" = {

      ## A negative value on the sqrt scale would square into an impossible
      ## positive original value (-3 -> +9), so it is set to 0 first.
      neg <- predictions < 0 & !is.na(predictions)

      predictions[neg] <- 0

      predictions^2

    },

    "log10" = 10^predictions - 1,

    ## Default: the door validators refuse a transformation outside
    ## VALID_TRANSFORMATIONS (#234), so reaching here is a bug. Serving the
    ## predictions untransformed would be silently wrong.
    abort_horizons(
      "{.arg transformation} {.val {transformation}} is not one horizons back-transforms.",
      kind = "internal"
    )

  )

  ## Physical range: a value outside the outcome's range is not a model
  ## behaviour worth scoring or serving. The default range is non-negative,
  ## the floor #53 applied; a signed property configures a range with no
  ## floor (#76). Applied uniformly after the switch so every transform
  ## (including "none") is covered, and
  ## applied here rather than at each caller so evaluation scores exactly what
  ## predict() serves (#53). The sqrt clamp above is a different thing: it
  ## prevents a negative sqrt-scale value from squaring into a wrong positive.
  clamp_to_outcome_range(out, outcome_range)

}

## ---------------------------------------------------------------------------
## apply_response_bound
## ---------------------------------------------------------------------------

#' Winsorize Predictions to the Deploy-Time Response Upper Bound
#'
#' @description
#' The guardrail `predict()` applies to the point predictions it serves: each
#' configuration's in `predict_one_config()`, and the combined output in
#' `predict.horizons_ensemble()`. Values above `upper_bound` are clamped to
#' it; `upper_bound = NULL` is a no-op. Silent itself: it returns which
#' values it clamped, and `predict()` reports them once per call, in one
#' `horizons_warning_applicability` naming the samples.
#'
#' @param values Numeric vector, original response scale.
#' @param upper_bound Single finite numeric (negative for a signed outcome
#'   whose training values are all negative), or NULL (no clamp).
#' @return List: `values`, winsorized to `upper_bound` where it was exceeded,
#'   and `over`, a logical vector marking the values that were.
#' @keywords internal
apply_response_bound <- function(values, upper_bound) {

  if (is.null(upper_bound)) {

    return(list(values = values, over = rep(FALSE, length(values))))

  }

  over <- !is.na(values) & values > upper_bound

  values[over] <- upper_bound

  list(values = values, over = over)

}

## ---------------------------------------------------------------------------
## clamp_to_outcome_range
## ---------------------------------------------------------------------------

#' Clamp Predictions or Interval Bounds to the Outcome's Physical Range
#'
#' @description
#' The one clamp every scoring and serving path applies (#76): values below
#' the lower bound of `outcome_range` are raised to it, values above the upper
#' bound lowered to it, and `NA`s pass through. An infinite end clamps
#' nothing, so the default range, `c(0, Inf)`, is the zero floor the package
#' applied before the range existed, and `c(-Inf, Inf)` leaves a signed
#' property untouched.
#'
#' This is the physical constraint, applied silently and everywhere. The
#' response bound ([apply_response_bound()]) is a different thing: a
#' deploy-time guardrail against blow-ups, applied to served point
#' predictions only, and reported by `predict()` with a warning.
#'
#' @param x Numeric vector, original response scale.
#' @param outcome_range Numeric length-2 vector from
#'   [outcome_range_setting()]. Default `DEFAULT_OUTCOME_RANGE`.
#' @return `x`, clamped to `outcome_range`.
#' @keywords internal
#' @noRd
clamp_to_outcome_range <- function(x, outcome_range = DEFAULT_OUTCOME_RANGE) {

  x[!is.na(x) & x < outcome_range[1]] <- outcome_range[1]
  x[!is.na(x) & x > outcome_range[2]] <- outcome_range[2]
  x

}

## ---------------------------------------------------------------------------
## compute_response_bound
## ---------------------------------------------------------------------------

#' The Deploy-Time Response Bound for a Set of Training Outcomes
#'
#' @description
#' `fit()` stores this bound, and `predict()` winsorizes served point
#' predictions to it. It lies strictly above the training maximum whatever
#' the outcome's sign:
#'
#' `bound = max + (RESPONSE_BOUND_MARGIN - 1) * (max - anchor)`
#'
#' where the anchor is the lower bound of `outcome_range` when that bound is
#' finite, and the training minimum when it is `-Inf`. The bound therefore
#' allows half the span from the anchor to the maximum above the maximum.
#' Under the default range the anchor is 0 and the bound is `max * 1.5`,
#' identical to the double for every normal double (`0.5 * max` is then
#' exact, so both expressions round the same real number once), so objects
#' and tests pinned to the old formula are unchanged. The
#' old formula itself was wrong for a signed outcome: at a negative maximum
#' `max * 1.5` lies below the maximum, and at a zero maximum it is zero. A
#' finite upper bound of `outcome_range` caps the result, since nothing above
#' it is served anyway.
#'
#' @param y Numeric vector of training outcomes; `NA`s are dropped.
#' @param outcome_range Numeric length-2 vector. Default
#'   `DEFAULT_OUTCOME_RANGE`.
#' @return Single numeric: the bound.
#' @keywords internal
#' @noRd
compute_response_bound <- function(y, outcome_range = DEFAULT_OUTCOME_RANGE) {

  y      <- y[!is.na(y)]
  top    <- max(y)
  anchor <- if (is.finite(outcome_range[1])) outcome_range[1] else min(y)

  min(top + (RESPONSE_BOUND_MARGIN - 1) * (top - anchor), outcome_range[2])

}

## ---------------------------------------------------------------------------
## needs_back_transformation
## ---------------------------------------------------------------------------

#' Check Whether a Transformation Requires Back-transformation
#'
#' @description
#' `"none"` is the only value that means no transformation. `"log"`,
#' `"log10"` and `"sqrt"` return `TRUE`, and so does any other value,
#' including `""` and `NA`, so an unknown transformation is never mistaken for
#' none. [back_transform_predictions()] treats the same values as unknown, as
#' a bug, since the door validators refuse them first (#234).
#'
#' @param transformation Character string indicating transformation type,
#'   matched case-insensitively.
#' @return Logical: `FALSE` for `"none"` and `NULL`, `TRUE` otherwise.
#' @keywords internal
needs_back_transformation <- function(transformation) {

  if (is.null(transformation)) return(FALSE)

  trans_lower <- tolower(as.character(transformation))

  !trans_lower %in% "none"

}

## ---------------------------------------------------------------------------
## compute_original_scale_metrics
## ---------------------------------------------------------------------------

#' Compute All Six Metrics on the Original Response Scale
#'
#' @description
#' After back-transforming predictions, compute the full metric set used in
#' `evaluation$results`. This is the canonical metric computation point —
#' called once per config after last_fit + back-transform.
#'
#' A metric undefined on the rows it is given is `NA`, and the run goes on:
#' with fewer than two complete pairs every metric is, so all six come back
#' `NA` rather than as an empty table, and the caller records why.
#'
#' @param truth Numeric vector of observed values (original scale).
#' @param estimate Numeric vector of predicted values (back-transformed).
#'
#' @return Tibble with one row per metric (`rmse`, `rrmse`, `rsq`, `ccc`,
#'   `rpd`, `mae`) and the columns `.metric`, `.estimator`, `.estimate` and
#'   `n`, the complete pairs the metrics were scored on. Every `.estimate` is
#'   `NA` when `n` is below 2.
#' @keywords internal
compute_original_scale_metrics <- function(truth, estimate) {

  ## Drop NA pairs --------------------------------------------------------

  metric_data <- tibble::tibble(
    truth    = truth,
    estimate = estimate
  )

  metric_data <- metric_data[stats::complete.cases(metric_data), ]
  n_pairs     <- nrow(metric_data)

  if (n_pairs < 2) {

    return(tibble::tibble(
      .metric    = c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"),
      .estimator = "standard",
      .estimate  = NA_real_,
      n          = n_pairs
    ))

  }

  ## Compute all six metrics ----------------------------------------------

  metric_fn <- yardstick::metric_set(
    yardstick::rmse,
    rrmse,
    yardstick::rsq,
    ccc,
    rpd,
    yardstick::mae
  )

  scored   <- metric_fn(metric_data, truth = truth, estimate = estimate)
  scored$n <- n_pairs

  scored

}

#' The six metrics as one row, one column each
#'
#' @param metrics A table from [compute_original_scale_metrics()].
#' @return One-row tibble with a column per metric.
#' @keywords internal
#' @noRd
metrics_row <- function(metrics) {

  tidyr::pivot_wider(metrics[c(".metric", ".estimate")],
                     names_from = .metric, values_from = .estimate)

}

#' Why a configuration's or member's test-set metrics are NA
#'
#' @param metrics The test set's table from [compute_original_scale_metrics()].
#' @return Character(1) for its warnings, or `NULL` when the metrics were
#'   computed.
#' @keywords internal
#' @noRd
metrics_na_reason <- function(metrics) {

  n <- metrics$n[1]

  if (n >= 2) return(NULL)

  paste0("The test-set metrics are NA: ", n, " test row",
         if (n == 1) " has" else "s have",
         " both an observed value and a prediction, and they need two.")

}
