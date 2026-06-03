# Predict from a fitted horizons object
#
# predict.horizons_fit() — point predictions and (optionally) conformal
# prediction intervals for new spectra, using the model(s) trained by fit().
#
# Capture / render: the heavy lifting per config lives in the silent helper
# predict_one_config(); predict.horizons_fit() owns input validation, config
# resolution, and assembly of the final tibble.

#' @importFrom rlang %||%
NULL

## ---------------------------------------------------------------------------
## predict.horizons_fit()
## ---------------------------------------------------------------------------

#' Predict Soil Properties from a Fitted Horizons Object
#'
#' @description
#' Generates predictions for new spectra using the model(s) trained by
#' [fit()]. By default it uses the single best-ranked configuration; pass
#' `config` to select a specific model or to return predictions from every
#' fitted model. When uncertainty quantification was computed during `fit()`
#' (`compute_uq = TRUE`), prediction intervals are returned by default.
#'
#' @param object A `horizons_fit` object (the output of [fit()]).
#' @param new_data Spectra to predict. Either a `horizons_data` object or a
#'   tibble/data.frame whose predictor (wavelength) columns match the training
#'   schema. **The spectra must already be on the training axis** — `predict()`
#'   replays per-config recipe steps (preprocessing, feature selection) but
#'   does not re-run object-level `standardize()`. Resample/trim/water-band
#'   alignment is the caller's responsibility (see Details).
#' @param config Which fitted model to use. One of:
#'   - `"best"` (default): the top-ranked config by the object's rank metric.
#'   - a `config_id` string: a specific fitted model.
#'   - `"all"`: every successfully fitted model (long output, one block per
#'     config, with a `config_id` column).
#' @param interval Logical. Return conformal prediction intervals when UQ is
#'   available? Default `TRUE`. Ignored (with a note) for configs that have no
#'   UQ bundle.
#' @param level Numeric coverage level in (0, 1). If `NULL` (default), uses the
#'   level stored during `fit()` (`uq$level_default`, typically 0.90).
#'   Supplying a different level recomputes the conformal margin from the
#'   stored calibration scores.
#' @param clamp_nonneg Logical. Floor predictions and interval bounds at 0?
#'   Default `TRUE` — nearly all soil properties are non-negative. Set `FALSE`
#'   for outcomes that can legitimately be negative (e.g. some ratios).
#' @param ... Unused; present for S3 method consistency.
#'
#' @return A tibble in long format, one row per sample (per config when
#'   `config = "all"`):
#'   \describe{
#'     \item{sample_id}{Sample identifier from `new_data`.}
#'     \item{config_id}{Config used (present when `config = "all"`).}
#'     \item{.pred}{Point prediction, original response scale.}
#'     \item{.pred_lower}{Lower interval bound (if `interval = TRUE` and UQ
#'       available), original scale.}
#'     \item{.pred_upper}{Upper interval bound.}
#'     \item{.interval_width}{`.pred_upper - .pred_lower`.}
#'   }
#'
#' @details
#' **Axis alignment (v1 limitation).** The fitted workflow's recipe applies the
#' per-config preprocessing (SNV, derivatives, feature selection) but not the
#' object-level operations performed by `standardize()` (resampling to a common
#' wavenumber grid, range trimming, water-band removal). `new_data` must
#' therefore arrive on the same wavenumber axis the model was trained on.
#' `predict()` validates the predictor schema and errors if it does not match.
#'
#' Applicability-domain flags (`.ad_flag`, `.ad_distance`) are not yet emitted;
#' applicability domain is deferred in v1 `fit()`, so the object stores nothing
#' to populate them.
#'
#' @examples
#' \dontrun{
#' fitted <- fit(evaluated, n_best = 5, compute_uq = TRUE)
#'
#' # Best model, with intervals
#' predict(fitted, new_spectra)
#'
#' # A specific config, point predictions only
#' predict(fitted, new_spectra, config = "cubist_log_snv_cars", interval = FALSE)
#'
#' # Every fitted model
#' predict(fitted, new_spectra, config = "all")
#' }
#'
#' @exportS3Method stats::predict horizons_fit
predict.horizons_fit <- function(object,
                                 new_data,
                                 config       = "best",
                                 interval     = TRUE,
                                 level        = NULL,
                                 clamp_nonneg = TRUE,
                                 ...) {

  ## -------------------------------------------------------------------------
  ## Step 0: Preflight
  ## -------------------------------------------------------------------------

  if (!inherits(object, "horizons_fit")) {

    cli::cli_abort(c(
      "{.arg object} must be a {.cls horizons_fit} object.",
      "i" = "Run {.fn fit} first to produce a fitted object."
    ))

  }

  workflows_list <- object$models$workflows

  if (is.null(workflows_list) || length(workflows_list) == 0) {

    cli::cli_abort("No fitted workflows found on this object.")

  }

  ## -------------------------------------------------------------------------
  ## Step 0b: Resolve new_data, then validate it carries the training axis
  ## -------------------------------------------------------------------------

  new_spectra <- resolve_new_data(new_data)

  ## All configs share the same training predictor axis (they differ only in
  ## per-config recipe steps applied downstream of a common input schema).
  ## Validate new_data against the first workflow's expected predictors.
  check_predictor_schema(workflows_list[[1]], new_spectra)

  ## -------------------------------------------------------------------------
  ## Step 1: Resolve `config` → one or more config_ids
  ## -------------------------------------------------------------------------

  config_ids <- resolve_config_ids(object, config)

  ## -------------------------------------------------------------------------
  ## Step 2-3: Predict per config (point + optional intervals)
  ## -------------------------------------------------------------------------

  preds <- purrr::map(
    config_ids,
    function(cid) {

      predict_one_config(
        object       = object,
        config_id    = cid,
        new_spectra  = new_spectra,
        interval     = interval,
        level        = level,
        clamp_nonneg = clamp_nonneg
      )

    }
  )

  ## -------------------------------------------------------------------------
  ## Step 4: Assemble output
  ## -------------------------------------------------------------------------

  if (identical(config, "all")) {

    ## Keep the config_id column for disambiguation
    dplyr::bind_rows(preds)

  } else {

    ## Single config: drop the config_id column for a cleaner result
    out <- preds[[1]]
    out[, setdiff(names(out), "config_id"), drop = FALSE]

  }

}

## ---------------------------------------------------------------------------
## resolve_new_data() — new_data contract + schema gate (silent helper)
## ---------------------------------------------------------------------------

#' Resolve new_data to a predictor tibble with a sample_id
#'
#' Accepts a `horizons_data` object or a bare tibble/data.frame. Returns a
#' tibble carrying `sample_id` plus the predictor (wavelength) columns. Pulls
#' predictor columns by role (the accessor API does not exist yet, so this is
#' done inline via the role map).
#'
#' @param new_data A `horizons_data` or tibble/data.frame.
#' @return A tibble with `sample_id` and predictor columns.
#' @keywords internal
#' @noRd
resolve_new_data <- function(new_data) {

  if (inherits(new_data, "horizons_data")) {

    analysis <- new_data$data$analysis
    role_map <- new_data$data$role_map

    pred_cols <- role_map$variable[role_map$role == "predictor"]
    id_col    <- role_map$variable[role_map$role == "id"][1]

    if (length(pred_cols) == 0) {

      cli::cli_abort("{.arg new_data} has no predictor columns in its role map.")

    }

    tibble::as_tibble(analysis[, c(id_col, pred_cols), drop = FALSE]) |>
      dplyr::rename(sample_id = dplyr::all_of(id_col))

  } else if (is.data.frame(new_data)) {

    ## Bare frame: require a sample identifier, synthesize one if absent.
    df <- tibble::as_tibble(new_data)

    if (!"sample_id" %in% names(df)) {

      df$sample_id <- as.character(seq_len(nrow(df)))

    }

    df

  } else {

    cli::cli_abort(c(
      "{.arg new_data} must be a {.cls horizons_data} object or a data frame.",
      "x" = "Got {.cls {class(new_data)}}."
    ))

  }

}

## ---------------------------------------------------------------------------
## check_predictor_schema() — axis-alignment gate
## ---------------------------------------------------------------------------

#' Validate that new_data carries the model's training predictor axis
#'
#' The fitted workflow's recipe replays per-config steps but NOT object-level
#' `standardize()` (resampling, trimming, water-band removal). So `new_data`
#' must already be on the training wavenumber axis. This compares the predictor
#' columns the recipe expects against what `new_data` supplies and aborts with
#' an actionable message on mismatch.
#'
#' @param workflow A fitted workflow (any config — all share the input axis).
#' @param new_spectra Tibble from [resolve_new_data()].
#' @return Invisibly TRUE; aborts on mismatch.
#' @keywords internal
#' @noRd
check_predictor_schema <- function(workflow, new_spectra) {

  recipe_obj <- safely_execute(
    workflows::extract_recipe(workflow, estimated = TRUE),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  ## If we can't introspect the recipe (e.g. butchered beyond recovery), skip
  ## the gate rather than block prediction — bake() will surface its own error.
  if (!is.null(recipe_obj$error) || is.null(recipe_obj$result)) {

    return(invisible(TRUE))

  }

  expected <- recipe_obj$result$var_info$variable[
    recipe_obj$result$var_info$role == "predictor"
  ]

  supplied <- setdiff(names(new_spectra), "sample_id")

  missing <- setdiff(expected, supplied)

  if (length(missing) > 0) {

    cli::cli_abort(c(
      "{.arg new_data} is missing {length(missing)} predictor column{?s} the model expects.",
      "x" = "Missing (first few): {.val {utils::head(missing, 5)}}",
      "i" = "new_data must be on the training wavenumber axis. Run {.fn standardize} \\
             to the same grid the model was trained on (e.g. 600-4000 at 2 cm^-1) before predicting."
    ))

  }

  invisible(TRUE)

}

## ---------------------------------------------------------------------------
## resolve_config_ids() — map `config` argument to config_id(s)
## ---------------------------------------------------------------------------

#' Resolve the `config` argument to one or more config_ids
#'
#' @param object A `horizons_fit`.
#' @param config `"best"`, a specific config_id, or `"all"`.
#' @return Character vector of config_ids present in `models$workflows`.
#' @keywords internal
#' @noRd
resolve_config_ids <- function(object, config) {

  available <- names(object$models$workflows)

  if (identical(config, "all")) {

    return(available)

  }

  if (identical(config, "best")) {

    return(rank_best_config(object))

  }

  ## A specific config_id
  if (length(config) != 1 || !is.character(config)) {

    cli::cli_abort('{.arg config} must be "best", "all", or a single config_id string.')

  }

  if (!config %in% available) {

    cli::cli_abort(c(
      "Config {.val {config}} is not among the fitted models.",
      "i" = "Available: {.val {available}}."
    ))

  }

  config

}

## ---------------------------------------------------------------------------
## rank_best_config() — top config by the object's rank metric
## ---------------------------------------------------------------------------

#' Identify the best config_id by the stored rank metric
#'
#' Mirrors `fit()`'s ranking: rank metric defaults to the value stored by
#' `evaluate()` (or "rpd"); rpd/rsq/ccc are higher-better, the rest
#' lower-better. Restricted to successfully fitted configs.
#'
#' @param object A `horizons_fit`.
#' @return A length-1 character config_id.
#' @keywords internal
#' @noRd
rank_best_config <- function(object) {

  results       <- object$models$results
  rank_metric   <- object$evaluation$rank_metric %||% "rpd"
  higher_better <- c("rpd", "rsq", "ccc")

  available <- names(object$models$workflows)

  ## Only rank among configs that have a fitted workflow and a usable metric.
  candidates <- results[results$config_id %in% available, , drop = FALSE]

  if (!rank_metric %in% names(candidates)) {

    cli::cli_abort(c(
      "Rank metric {.val {rank_metric}} is not present in the fit results.",
      "i" = "Available metric columns: {.val {names(candidates)}}."
    ))

  }

  metric_vals <- candidates[[rank_metric]]

  if (all(is.na(metric_vals))) {

    cli::cli_abort("All values for rank metric {.val {rank_metric}} are NA; cannot pick a best config.")

  }

  best_idx <- if (rank_metric %in% higher_better) {

    which.max(metric_vals)

  } else {

    which.min(metric_vals)

  }

  candidates$config_id[best_idx]

}

## ---------------------------------------------------------------------------
## predict_one_config() — point (+ interval) prediction for one config
## ---------------------------------------------------------------------------

#' Predict from a single fitted config
#'
#' Point predictions on the original response scale (back-transformed once),
#' plus conformal intervals when a UQ bundle is present and `interval = TRUE`.
#'
#' @param object A `horizons_fit`.
#' @param config_id The config to predict from.
#' @param new_spectra Tibble from [resolve_new_data()] (sample_id + predictors).
#' @param interval Logical; return intervals when UQ is available.
#' @param level Coverage level or NULL.
#' @param clamp_nonneg Logical; floor predictions and interval bounds at 0.
#' @return A tibble: sample_id, config_id, .pred (+ interval columns).
#' @keywords internal
#' @noRd
predict_one_config <- function(object, config_id, new_spectra, interval, level,
                               clamp_nonneg = TRUE) {

  workflow <- object$models$workflows[[config_id]]

  ## Per-config response transformation (for back-transforming to original scale)
  cfg_row        <- object$config$configs[object$config$configs$config_id == config_id, ]
  transformation <- if (nrow(cfg_row) == 1) {

    tolower(as.character(cfg_row$transformation))

  } else {

    "none"

  }

  ## -------------------------------------------------------------------------
  ## Point prediction (transformed scale → back-transform ONCE to original)
  ## -------------------------------------------------------------------------

  point_trans <- stats::predict(workflow, new_data = new_spectra)$.pred

  point_pred <- if (needs_back_transformation(transformation)) {

    back_transform_predictions(point_trans, transformation, warn = FALSE)

  } else {

    point_trans

  }

  ## Non-negativity floor: nearly all soil properties are non-negative. Opt out
  ## via clamp_nonneg = FALSE for outcomes that can legitimately go negative.
  if (clamp_nonneg) {

    point_pred[!is.na(point_pred) & point_pred < 0] <- 0

  }

  out <- tibble::tibble(
    sample_id = new_spectra$sample_id,
    config_id = config_id,
    .pred     = point_pred
  )

  ## -------------------------------------------------------------------------
  ## Intervals (conformal CQR) — only if requested and a UQ bundle exists
  ## -------------------------------------------------------------------------

  uq <- object$models$uq[[config_id]]

  if (!interval || is.null(uq)) {

    return(out)

  }

  interval_cols <- predict_intervals(
    uq           = uq,
    point_pred   = point_pred,
    new_spectra  = new_spectra,
    level        = level,
    clamp_nonneg = clamp_nonneg
  )

  ## predict_intervals() returns NULL if quantile prediction fails — degrade
  ## gracefully to point-only rather than erroring.
  if (is.null(interval_cols)) {

    return(out)

  }

  dplyr::bind_cols(out, interval_cols)

}

## ---------------------------------------------------------------------------
## predict_intervals() — conformal prediction intervals for one config
## ---------------------------------------------------------------------------

#' Assemble conformal prediction intervals
#'
#' SCALE INVARIANT: the quantile forest was trained on original-scale OOF
#' residuals (see `fit_uq()`), so `q_low`/`q_high` and `c_alpha` are already on
#' the original response scale. `point_pred` is also already back-transformed.
#' The bounds are therefore assembled directly and **never back-transformed
#' again** — re-transforming here would be the classic "applied twice" bug.
#'
#' @param uq A UQ bundle from `models$uq[[config_id]]`.
#' @param point_pred Numeric point predictions, original scale.
#' @param new_spectra Tibble of new data (sample_id + predictors).
#' @param level Coverage level or NULL (-> `uq$level_default`).
#' @param clamp_nonneg Logical; floor interval bounds at 0.
#' @return Tibble of interval columns, or NULL if quantile prediction fails.
#' @keywords internal
#' @noRd
predict_intervals <- function(uq, point_pred, new_spectra, level,
                              clamp_nonneg = TRUE) {

  level <- level %||% uq$level_default
  alpha <- 1 - level

  ## Bake new_data through the UQ recipe (predictors only — the same feature
  ## space the quantile forest was trained on).
  bake_result <- safely_execute(
    recipes::bake(uq$prepped_recipe, new_data = new_spectra,
                  recipes::all_predictors()),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(bake_result$error) || is.null(bake_result$result)) {

    return(NULL)

  }

  new_features <- as.data.frame(bake_result$result)

  ## Residual quantiles from the quantile forest (original scale).
  q_result <- safely_execute(
    stats::predict(
      uq$quantile_model,
      data      = new_features,
      type      = "quantiles",
      quantiles = c(alpha / 2, 1 - alpha / 2)
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(q_result$error) || is.null(q_result$result)) {

    return(NULL)

  }

  q_low  <- q_result$result$predictions[, 1]
  q_high <- q_result$result$predictions[, 2]

  ## Conformal margin from the (signed) calibration scores at the requested
  ## level. May be negative — that is the point of signed CQR.
  c_alpha <- compute_c_alpha(uq$scores, level)

  ## Assemble bounds. Every term is original-scale; do NOT back-transform.
  lower <- point_pred + q_low  - c_alpha
  upper <- point_pred + q_high + c_alpha

  ## Crossing / negative-width repair (signed c_alpha makes this necessary).
  lo <- pmin(lower, upper)
  hi <- pmax(lower, upper)

  ## Non-negativity floor on the bounds (same opt-out as the point prediction).
  ## Applied after crossing-repair so ordering is preserved.
  if (clamp_nonneg) {

    lo[!is.na(lo) & lo < 0] <- 0
    hi[!is.na(hi) & hi < 0] <- 0

  }

  tibble::tibble(
    .pred_lower     = lo,
    .pred_upper     = hi,
    .interval_width = hi - lo
  )

}
