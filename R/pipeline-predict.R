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
#' **Response upper bound (guardrail).** Point predictions are winsorized to
#' `models$response_bound` (max training outcome times 1.5, stored by [fit()])
#' with a visible warning when any value is clamped. This catches physically
#' impossible back-transform blow-ups (e.g. an unconstrained log-scale
#' prediction inflating through `exp()`) while permitting modest extrapolation.
#' Interval bounds are deliberately NOT clamped — truncating the interval would
#' overstate confidence exactly where the model is least trustworthy. Objects
#' fitted before this field existed predict without a clamp.
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
                                 config   = "best",
                                 interval = TRUE,
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

  ## Validate new_data carries the training-axis predictor columns fit() stored.
  check_predictor_schema(object, new_spectra)

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
        object      = object,
        config_id   = cid,
        new_spectra = new_spectra,
        interval    = interval
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
#' columns `fit()` recorded (`models$predictor_schema`) against what `new_data`
#' supplies and aborts with an actionable message on mismatch.
#'
#' @param object A `horizons_fit` carrying `models$predictor_schema`.
#' @param new_spectra Tibble from [resolve_new_data()].
#' @return Invisibly TRUE; aborts on mismatch.
#' @keywords internal
#' @noRd
check_predictor_schema <- function(object, new_spectra) {

  ## fit() stored the training-axis predictor columns; validate against them
  ## directly (no recipe re-introspection, which a butchered workflow can break).
  expected <- object$models$predictor_schema

  ## Objects written before predictor_schema existed: skip the gate, let bake()
  ## surface any mismatch.
  if (is.null(expected)) {

    return(invisible(TRUE))

  }

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

    ## fit() recorded the top config (by the metric it actually ranked on) in
    ## best-first order; read it rather than re-deriving. Fall back to the first
    ## fitted workflow for objects written before this field existed.
    best <- object$models$best_config %||% available[1]
    return(best)

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
#' @return A tibble: sample_id, config_id, .pred (+ interval columns).
#' @keywords internal
#' @noRd
predict_one_config <- function(object, config_id, new_spectra, interval) {

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

  ## Routed through the safely_execute -> handle_results cascade: a butchered
  ## workflow can fail at predict time (e.g. recipe re-introspection), and the
  ## raw parsnip/workflows error carries no horizons context. Wrap so the
  ## failure names the offending config.
  pred_safe <- safely_execute(
    stats::predict(workflow, new_data = new_spectra),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  point_trans <- handle_results(
    pred_safe,
    error_title = paste0("Prediction failed for config '", config_id, "'.")
  )$.pred

  ## Unconditional funnel call: the "none" branch is a passthrough, and the
  ## deploy-time winsorization guardrail applies after the switch regardless of
  ## transform. Old objects without response_bound degrade gracefully (NULL →
  ## no clamp), mirroring the predictor_schema NULL-skip above. Fit-time paths
  ## deliberately do NOT pass a bound — ranking must see raw model behavior.
  point_pred <- back_transform_predictions(
    point_trans,
    transformation,
    warn        = FALSE,
    upper_bound = object$models$response_bound %||% NULL
  )

  ## Soil properties predicted from MIR are non-negative; floor at 0.
  point_pred <- floor_at_zero(point_pred)

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
    uq          = uq,
    point_pred  = point_pred,
    new_spectra = new_spectra
  )

  ## predict_intervals() returns NULL if quantile prediction fails — degrade
  ## gracefully to point-only rather than erroring.
  if (is.null(interval_cols)) {

    return(out)

  }

  dplyr::bind_cols(out, interval_cols)

}

## ---------------------------------------------------------------------------
## predict_members() — every ensemble member predicts new_data (silent helper)
## ---------------------------------------------------------------------------

#' Predict new spectra from each ensemble member
#'
#' Runs every member config through the same per-config primitive the single
#' fit path uses ([predict_one_config()] — predict once, back-transform once)
#' and stacks the results into one long frame. This is the predict-time analog
#' of the train-time `predict_members_on_test()`: same structure, minus the
#' truth column (new data carries no outcome).
#'
#' Members are predicted point-only (`interval = FALSE`); the ensemble's own
#' uncertainty is conformalized on the meta-learner's residuals downstream, not
#' assembled from member intervals.
#'
#' @param object A `horizons_ensemble` (or any object carrying
#'   `models$workflows`).
#' @param members Character vector of member `config_id`s to predict.
#' @param new_spectra Tibble from [resolve_new_data()] (sample_id + predictors).
#' @return A long tibble: `config_id`, `sample_id`, `.pred` (original scale),
#'   one block per member.
#' @keywords internal
#' @noRd
predict_members <- function(object, members, new_spectra) {

  ## sample_id is the join key for every downstream combine (weighted average
  ## groups on it; the wide pivot keys rows on it). A duplicate would silently
  ## fan out each member's predictions and corrupt the combination, so assert
  ## uniqueness loudly here — the predict-time mirror of the train-time gate in
  ## predict_members_on_test().
  if (anyDuplicated(new_spectra$sample_id)) {

    cli::cli_abort(c(
      "{.field sample_id}s in {.arg new_data} are not unique.",
      "x" = "Duplicate keys would fan out the member-prediction combine.",
      "i" = "Expected one row per sample."
    ))

  }

  dplyr::bind_rows(lapply(members, function(m) {

    pc <- predict_one_config(object, config_id = m, new_spectra = new_spectra,
                             interval = FALSE)

    tibble::tibble(config_id = m, sample_id = pc$sample_id, .pred = pc$.pred)

  }))

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
#' @return Tibble of interval columns, or NULL if quantile prediction fails.
#' @keywords internal
#' @noRd
predict_intervals <- function(uq, point_pred, new_spectra) {

  ## Intervals are returned at the coverage level the UQ was calibrated for.
  level <- uq$level_default
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

  ## Conformal margin from the (signed) calibration scores. May be negative —
  ## that is the point of signed CQR.
  c_alpha <- compute_c_alpha(uq$scores, level)

  ## Assemble bounds. Every term is original-scale; do NOT back-transform.
  lower <- point_pred + q_low  - c_alpha
  upper <- point_pred + q_high + c_alpha

  ## Crossing / negative-width repair (signed c_alpha makes this necessary),
  ## then floor at 0 — applied after repair so ordering is preserved.
  lo <- floor_at_zero(pmin(lower, upper))
  hi <- floor_at_zero(pmax(lower, upper))

  tibble::tibble(
    .pred_lower     = lo,
    .pred_upper     = hi,
    .interval_width = hi - lo
  )

}

## ---------------------------------------------------------------------------
## floor_at_zero() — non-negativity floor for predictions and bounds
## ---------------------------------------------------------------------------

#' Floor a numeric vector at zero, preserving NAs
#'
#' Soil properties predicted from MIR spectra are non-negative, so predictions
#' and interval bounds are floored at 0. NAs pass through untouched.
#'
#' @param x Numeric vector.
#' @return `x` with negative (non-NA) entries set to 0.
#' @keywords internal
#' @noRd
floor_at_zero <- function(x) {

  x[!is.na(x) & x < 0] <- 0
  x

}
