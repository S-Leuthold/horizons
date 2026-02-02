#' Back-transform Predictions to Original Scale
#'
#' @description
#' Inverts response transformations applied by `step_log(offset = 1)` or
#' `step_sqrt()` during recipe preprocessing. This is the single source of
#' truth for back-transformation logic in horizons.
#'
#' **Important:** `step_log(offset = 1)` computes `log(x + 1)`, so the correct
#' inverse is `exp(x) - 1`, NOT `exp(x)`. The legacy code had this wrong.
#'
#' @param predictions Numeric vector of predictions on the transformed scale.
#' @param transformation Character: "none", "log", "sqrt", or "log10".
#' @param warn Logical. Warn on edge cases (very large values, negatives)?
#'
#' @return Numeric vector on the original response scale.
#' @export
back_transform_predictions <- function(predictions, transformation, warn = TRUE) {

  if (is.null(predictions) || length(predictions) == 0) return(predictions)

  transformation <- tolower(as.character(transformation))

  switch(transformation,

    "none" = predictions,

    "log" = {

      if (warn && any(predictions > 50, na.rm = TRUE)) {

        warning("Very large values detected in log-scale predictions (>50). Check for outliers.",
                call. = FALSE)

      }

      exp(predictions) - 1

    },

    "sqrt" = {

      if (warn && any(predictions < 0, na.rm = TRUE)) {

        warning("Negative values detected in sqrt-scale predictions. Setting to 0.",
                call. = FALSE)
        predictions[predictions < 0 & !is.na(predictions)] <- 0

      }

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
