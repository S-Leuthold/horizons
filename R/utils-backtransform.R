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
#' @param predictions Numeric vector of predictions on the transformed scale.
#' @param transformation Character: "none", "log", "sqrt", or "log10".
#' @param warn Logical. Warn on edge cases (very large values, negatives)?
#' @param upper_bound Optional single positive finite numeric. When supplied,
#'   the back-transformed (original-scale) predictions are winsorized to this
#'   bound, uniformly across all transformations. `NULL` (the default) applies
#'   no upper clamp. The winsorization warning is NOT gated by `warn` — a
#'   caller that passes a bound has opted into the guardrail and must see it
#'   trip (visible recoverable failure over silent drift).
#'
#' @return Numeric vector on the original response scale.
#' @export
back_transform_predictions <- function(predictions, transformation, warn = TRUE,
                                       upper_bound = NULL) {

  if (is.null(predictions) || length(predictions) == 0) return(predictions)

  if (!is.null(upper_bound)) {

    if (!is.numeric(upper_bound) || length(upper_bound) != 1 ||
        !is.finite(upper_bound) || upper_bound <= 0) {

      cli::cli_abort(c(
        "{.arg upper_bound} must be a single positive finite numeric.",
        "x" = "Got {.val {upper_bound}}."
      ))

    }

  }

  transformation <- tolower(as.character(transformation))

  out <- switch(transformation,

    "none" = predictions,

    "log" = {

      if (warn && any(predictions > 50, na.rm = TRUE)) {

        warning("Very large values detected in log-scale predictions (>50). Check for outliers.",
                call. = FALSE)

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

        warning("Negative values detected in sqrt-scale predictions. Setting to 0.",
                call. = FALSE)

      }

      predictions[neg] <- 0

      predictions^2

    },

    "log10" = {

      if (warn && any(predictions > 50, na.rm = TRUE)) {

        warning("Very large values detected in log10-scale predictions (>50). Check for outliers.",
                call. = FALSE)

      }

      10^predictions - 1

    },

    ## Default: unrecognized transformation → return unchanged
    {

      if (warn && transformation != "") {

        warning(paste0("Unknown transformation '", transformation,
                       "'. Returning predictions unchanged."),
                call. = FALSE)

      }

      predictions

    }

  )

  ## Deploy-time guardrail: winsorize the original-scale output to the caller's
  ## bound. Applied uniformly after the switch so every transform (including
  ## "none" and the unknown-transform passthrough) is covered. The warning is
  ## deliberately NOT gated by `warn` — same reasoning as the sqrt clamp above:
  ## a guardrail that fires silently in production (warn = FALSE callers)
  ## defeats its purpose.
  if (!is.null(upper_bound)) {

    over <- !is.na(out) & out > upper_bound

    if (any(over)) {

      cli::cli_warn(c(
        "!" = "{sum(over)} prediction{?s} exceeded the response upper bound and {?was/were} winsorized.",
        "i" = "Max pre-clamp value: {round(max(out[over]), 2)}; bound: {round(upper_bound, 2)}.",
        "i" = "Large overshoots usually indicate extrapolation beyond the training domain."
      ))

      out[over] <- upper_bound

    }

  }

  out

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
