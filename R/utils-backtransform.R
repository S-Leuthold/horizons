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
#' `upper_bound` guardrail is deliberately not applied the same way, because
#' fit-time ranking must see raw upper-tail behaviour.
#'
#' @param predictions Numeric vector of predictions on the transformed scale.
#' @param transformation Character: "none", "log", "sqrt", or "log10".
#' @param warn Logical. Warn on edge cases (very large values, negatives)?
#'   Note: `warn = FALSE` suppresses these diagnostic warnings only — the
#'   `upper_bound` winsorization warning is a guardrail and always fires. All
#'   conditions signal via `cli` (uniform `rlang_warning` class).
#' @param upper_bound Optional single finite numeric above the lower bound of
#'   `outcome_range` (so positive under the default range). When supplied,
#'   the back-transformed (original-scale) predictions are winsorized to this
#'   bound, uniformly across all transformations. `NULL` (the default) applies
#'   no upper clamp. The winsorization warning is NOT gated by `warn` — a
#'   caller that passes a bound has opted into the guardrail and must see it
#'   trip (visible recoverable failure over silent drift).
#' @param outcome_range Numeric length-2 vector, lower < upper, either end
#'   possibly infinite: the outcome's physical range, which every
#'   original-scale value is clamped to. A `-Inf` lower bound applies no
#'   floor and an `Inf` upper bound no cap. Default `c(0, Inf)`, the
#'   non-negative floor.
#'
#' @return Numeric vector on the original response scale, clamped to
#'   `outcome_range`.
#' @export
back_transform_predictions <- function(predictions, transformation, warn = TRUE,
                                       upper_bound   = NULL,
                                       outcome_range = c(0, Inf)) {

  if (is.null(predictions) || length(predictions) == 0) return(predictions)

  if (!is_valid_outcome_range(outcome_range)) {

    cli::cli_abort(c(
      "{.arg outcome_range} must be a numeric vector of length 2 with lower < upper.",
      "x" = "Got {.val {outcome_range}}."
    ))

  }

  ## The bound caps predictions from above, so it has to sit above the floor;
  ## under the default range that is the "single positive finite" rule the
  ## guardrail has always had.
  if (!is.null(upper_bound)) {

    if (!is.numeric(upper_bound) || length(upper_bound) != 1 ||
        !is.finite(upper_bound) || upper_bound <= outcome_range[1]) {

      cli::cli_abort(c(
        "{.arg upper_bound} must be a single finite numeric above the lower bound of {.arg outcome_range} ({outcome_range[1]}).",
        "x" = "Got {.val {upper_bound}}."
      ))

    }

  }

  transformation <- tolower(as.character(transformation))

  out <- switch(transformation,

    "none" = predictions,

    "log" = {

      if (warn && any(predictions > 50, na.rm = TRUE)) {

        cli::cli_warn("Very large values detected in log-scale predictions (>50). Check for outliers.")

      }

      exp(predictions) - 1

    },

    "sqrt" = {

      ## A negative value on the sqrt scale back-transforms to an impossible
      ## negative original value once squared would flip its sign (e.g. -3 ->
      ## +9). Clamp to 0 ALWAYS — this is correctness, not a warning. The
      ## warning about it is what's gated by `warn`. (Previously the clamp
      ## itself sat inside `if (warn)`, so production callers passing
      ## warn = FALSE silently squared negatives into wrong positives.)
      neg <- predictions < 0 & !is.na(predictions)

      if (warn && any(neg)) {

        cli::cli_warn("Negative values detected in sqrt-scale predictions. Setting to 0.")

      }

      predictions[neg] <- 0

      predictions^2

    },

    "log10" = {

      if (warn && any(predictions > 50, na.rm = TRUE)) {

        cli::cli_warn("Very large values detected in log10-scale predictions (>50). Check for outliers.")

      }

      10^predictions - 1

    },

    ## Default: unrecognized transformation → return unchanged
    {

      if (warn && transformation != "") {

        cli::cli_warn("Unknown transformation {.val {transformation}}. Returning predictions unchanged.")

      }

      predictions

    }

  )

  ## Physical range: a value outside the outcome's range is not a model
  ## behaviour worth scoring or serving. The default range is non-negative,
  ## the floor #53 applied; a signed property configures a range with no
  ## floor (#76). Applied uniformly after the switch so every transform
  ## (including "none" and the unknown-transform passthrough) is covered, and
  ## applied here rather than at each caller so evaluation scores exactly what
  ## predict() serves (#53). The sqrt clamp above is a different thing: it
  ## prevents a negative sqrt-scale value from squaring into a wrong positive.
  out <- clamp_to_outcome_range(out, outcome_range)

  ## Deploy-time guardrail: winsorize the original-scale output to the caller's
  ## bound. Unlike the floor this is opt-in, because fit-time ranking must see
  ## raw upper-tail behaviour.
  apply_response_bound(out, upper_bound)

}

## ---------------------------------------------------------------------------
## apply_response_bound
## ---------------------------------------------------------------------------

#' Winsorize Predictions to the Deploy-Time Response Upper Bound
#'
#' @description
#' The guardrail primitive shared by [back_transform_predictions()] (single-
#' model deploy path) and `predict.horizons_ensemble()` (the combined ensemble
#' output). Values above `upper_bound` are clamped to it with a visible
#' warning; `upper_bound = NULL` is a no-op. The warning is deliberately
#' unconditional — a guardrail that fires silently defeats its purpose — so
#' callers that suppress diagnostic warnings still surface this one.
#'
#' @param values Numeric vector, original response scale.
#' @param upper_bound Single finite numeric (negative for a signed outcome
#'   whose training values are all negative), or NULL (no clamp).
#' @return `values`, winsorized to `upper_bound` where it was exceeded.
#' @keywords internal
apply_response_bound <- function(values, upper_bound) {

  if (is.null(upper_bound)) {

    return(values)

  }

  over <- !is.na(values) & values > upper_bound

  if (any(over)) {

    cli::cli_warn(c(
      "!" = "{sum(over)} prediction{?s} exceeded the response upper bound and {?was/were} winsorized.",
      "i" = "Max pre-clamp value: {round(max(values[over]), 2)}; bound: {round(upper_bound, 2)}.",
      "i" = "Large overshoots usually indicate extrapolation beyond the training domain."
    ))

    values[over] <- upper_bound

  }

  values

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
#' predictions only, with a warning.
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
#' @param transformation Character string indicating transformation type.
#' @return Logical.
#' @keywords internal
#' @export
needs_back_transformation <- function(transformation) {

  if (is.null(transformation) || is.na(transformation)) return(FALSE)

  trans_lower <- tolower(as.character(transformation))

  !trans_lower %in% c("none", "notrans", "na", "")

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
#' @param truth Numeric vector of observed values (original scale).
#' @param estimate Numeric vector of predicted values (back-transformed).
#'
#' @return Tibble with `.metric`, `.estimator`, `.estimate` columns (6 rows).
#'   Returns empty tibble if fewer than 2 complete cases.
#' @keywords internal
#' @export
compute_original_scale_metrics <- function(truth, estimate) {

  ## Drop NA pairs --------------------------------------------------------

  metric_data <- tibble::tibble(
    truth    = truth,
    estimate = estimate
  )

  metric_data <- metric_data[stats::complete.cases(metric_data), ]

  if (nrow(metric_data) < 2) {

    warning("Insufficient data for metric calculation after removing NAs",
            call. = FALSE)
    return(tibble::tibble())

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

  metric_fn(metric_data, truth = truth, estimate = estimate)

}
