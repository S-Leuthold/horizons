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
#' @param interval Conformal prediction intervals. `NULL` (the default):
#'   intervals for each configuration that has a UQ bundle (`fit(compute_uq =
#'   TRUE)`), and none, silently, for one that has none; the interval columns
#'   appear when any configuration predicted has a bundle. `TRUE`: intervals
#'   asked for; a configuration without a bundle gets `NA` bounds and is named
#'   in one warning of class `horizons_warning_skipped`. `FALSE`: none. The
#'   intervals are at the level they were calibrated at in `fit()`, 0.90 by
#'   default (see Details). When intervals are returned and the model was fit
#'   on a training object carrying a `$selection` (from `select_training()`),
#'   this warns once, with class `horizons_warning_quality`, that conformal
#'   coverage is not guaranteed: the calibration rows were chosen for
#'   proximity to the targets, so they are not exchangeable with arbitrary
#'   prediction data.
#' @param abstain_ood Logical. When `TRUE`, `.pred` (and any interval bounds)
#'   are set to `NA` for samples flagged out-of-domain (`.ad_flag == "OOD"`),
#'   while `.ad_distance`/`.ad_flag` are preserved so the caller can see how far
#'   out each sample was. Default `FALSE` (report AD, predict for all samples).
#'   Every configuration predicted needs an AD bundle: one without is refused
#'   with an error of class `horizons_error_input`, since a prediction that is
#'   not `NA` under abstention claims to be in domain.
#' @param ... Not used; present for S3 method consistency. A `level` passed
#'   here is ignored with a warning (see Details), and any other argument is
#'   an error.
#'
#' @return A tibble in long format, one row per row of `new_data` (per config
#'   when `config = "all"`), in its order; zero rows of `new_data` give zero
#'   rows with the same columns:
#'   \describe{
#'     \item{sample_id}{Sample identifier from `new_data`.}
#'     \item{config_id}{Config used (present when `config = "all"`).}
#'     \item{.pred}{Point prediction, original response scale.}
#'     \item{.pred_lower}{Lower interval bound, original scale: present when
#'       `interval = TRUE`, or `NULL` and a configuration predicted has a UQ
#'       bundle; `NA` where a configuration has none.}
#'     \item{.pred_upper}{Upper interval bound.}
#'     \item{.interval_width}{`.pred_upper - .pred_lower`.}
#'     \item{.ad_distance}{Squared Mahalanobis distance to the training centroid
#'       in model feature space (if the config has an AD bundle).}
#'     \item{.ad_flag}{Applicability-domain bin: `Q1`-`Q4` (within domain,
#'       increasing distance) or `OOD` (out-of-domain).}
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
#' Applicability-domain columns (`.ad_distance`, `.ad_flag`) are emitted per
#' config when the object carries an AD bundle (`fit(compute_ad = TRUE)`). The
#' distance is a squared Mahalanobis distance in the model's feature space; the
#' flag bins it against held-out-calibrated thresholds. Set `abstain_ood = TRUE`
#' to `NA` predictions for out-of-domain samples while keeping the AD columns.
#'
#' **Rows that cannot be predicted.** `new_data` is baked through each
#' configuration's recipe first. A row flagged on arrival (a `.flag` that is
#' not `NA`: a step kept the sample but could not use it) or
#' non-finite after the recipe is not predicted: it is returned as `NA` in
#' every column, and the rows are named in one warning of class
#' `horizons_warning_flagged`. The other rows are predicted as usual.
#'
#' **When something cannot be computed.** Each configuration's predictions
#' are computed on their own, and its intervals and applicability domain on
#' their own within them. A configuration that cannot predict comes back as
#' `NA` rows, named in one warning of class
#' `horizons_warning_partial_failure`; when nothing can be predicted,
#' `predict()` aborts with `horizons_error_prediction_failed`, the failures in
#' its `results` field. Intervals or an applicability domain that cannot be
#' computed leave their columns `NA`, and the configurations are named in one
#' `horizons_warning_skipped`. Every warning is raised once, at the end of
#' the call, and carries its record as the field `log`; `predict()` changes
#' nothing on `object`.
#'
#' **Interval level.** Intervals are at the coverage level the uncertainty was
#' calibrated at in `fit()`, 0.90 by default. `predict()` has no `level`
#' argument: a `level` is ignored with a warning of class
#' `horizons_warning_input`, and any other argument it does not take is an
#' error of class `horizons_error_input`.
#'
#' **Outcome range.** Point predictions and interval bounds are clamped to
#' the `outcome_range` given to [configure()], silently, since the range is
#' the outcome's physical range. Under the default, `c(0, Inf)`, that is a
#' floor at zero, and objects configured before the range existed predict
#' under it; a signed property configured with `c(-Inf, Inf)` is not clamped
#' at all (#76). Clamping an interval keeps its coverage only for a truth
#' inside the range: coverage is at least the nominal level minus the
#' probability that the truth lies outside the range, so a range narrower
#' than the property's physical bounds costs coverage. A finite upper bound
#' clamps a blow-up silently, before the response bound below sees it, so
#' when the bound is capped at the range (see [fit()]) the guardrail's
#' warning does not fire.
#'
#' **Response upper bound (guardrail).** Point predictions are winsorized to
#' `models$response_bound` (stored by [fit()]: the largest training outcome
#' times 1.5 under the default range, and in general a margin above it
#' scaled on the outcome's span; see [fit()])
#' with a visible warning when any value is clamped. This catches physically
#' impossible back-transform blow-ups (e.g. an unconstrained log-scale
#' prediction inflating through `exp()`) while permitting modest extrapolation.
#' Interval bounds are deliberately NOT clamped — truncating the interval would
#' overstate confidence exactly where the model is least trustworthy. The
#' interval is built around the unclamped prediction, so a clamped `.pred` can
#' sit outside its own interval, below `.pred_lower`. Objects fitted before
#' this field existed predict without a clamp.
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
                                 config      = "best",
                                 interval    = NULL,
                                 abstain_ood = FALSE,
                                 ...) {

  ## -------------------------------------------------------------------------
  ## Step 0: Preflight
  ## -------------------------------------------------------------------------

  ## The object as it arrives (#234): a fit edited by hand, or saved by an
  ## earlier version, is refused with the remedy rather than served. The
  ## validator's gate covers the class, and it refuses an empty or unnamed
  ## workflows slot.
  object <- validate_horizons_fit(object)

  ## What the call reports is collected here and raised once per topic at
  ## its end (finish_predict()). predict() writes nothing to the object: each
  ## warning carries its own record.
  collector <- log_open("predict")

  check_arg(rlang::check_bool, interval, allow_null = TRUE)
  check_arg(rlang::check_bool, abstain_ood)
  check_arg(rlang::check_string, config)

  ## -------------------------------------------------------------------------
  ## Step 0b: Resolve new_data, then validate it carries the training axis
  ## -------------------------------------------------------------------------

  ## The configs being predicted, resolved first: the check of `...` names
  ## their UQ level, and the namespace check loads their engines.
  ## resolve_config_ids() needs no namespace loading itself.
  config_ids <- resolve_config_ids(object, config)

  ## Abstention is all or nothing (#145): a non-NA prediction under
  ## abstain_ood claims to be in domain, so every configuration asked for
  ## needs an applicability domain to check it against.
  check_abstention(object, config_ids, abstain_ood)

  ## Nothing in `...` is used. A `level` warns, naming the level the predicted
  ## configs' UQ bundles were calibrated at; anything else aborts (#141).
  ## Checked here, once the configs are known and before any work is done.
  check_predict_dots(
    ...,
    .level_default = unlist(lapply(object$models$uq[config_ids],
                                  function(uq) uq$level_default)),
    .collector     = collector
  )

  ## `library(horizons)` does not load workflows, most modeling engines, or
  ## ranger (#65) — none of them are referenced via NAMESPACE import
  ## directives, only `::`. A stored workflow's S3 predict method therefore
  ## fails to dispatch in a fresh session unless the caller has separately
  ## loaded the right namespace. Scoped to config_ids (not every config the
  ## object stores): a fit holding both an rf and a mars config should not
  ## need earth installed just to predict the rf one.
  ensure_predict_namespaces(object, config_ids)

  new_spectra <- resolve_new_data(new_data)

  ## Validate new_data carries the training-axis predictor columns fit() stored.
  check_predictor_schema(object, new_spectra)

  ## Intervals asked for where there is no UQ bundle to build them from (#145)
  if (isTRUE(interval)) {

    log_missing_intervals(collector, config_ids[vapply(
      config_ids, function(id) is.null(object$models$uq[[id]]), logical(1)
    )])

  }

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
        interval    = interval,
        abstain_ood = abstain_ood
      )

    }
  )

  ## -------------------------------------------------------------------------
  ## Step 4: Assemble output, then report
  ## -------------------------------------------------------------------------

  ## Nothing usable: every configuration asked for failed
  if (all(vapply(preds, attr, logical(1), "failed"))) {

    n_failed <- length(preds)

    abort_prediction_failed(
      dplyr::bind_rows(lapply(preds, attr, "unit_records")),
      "Nothing could be predicted: {cli::qty(n_failed)}{?the/every} configuration asked for failed."
    )

  }

  log_prediction_units(collector, preds)

  preds <- lapply(preds, drop_block_reports)

  out <- if (identical(config, "all")) {

    ## Keep the config_id column for disambiguation
    dplyr::bind_rows(preds)

  } else {

    ## Single config: drop the config_id column for a cleaner result
    preds[[1]][, setdiff(names(preds[[1]]), "config_id"), drop = FALSE]

  }

  ## Conformal coverage on a selected training set, said only when intervals
  ## came back (#145)
  log_interval_quality(collector, object, intervals_returned(out))

  finish_predict(collector, out)

}

## ---------------------------------------------------------------------------
## finish_predict() — predict()'s end: what it collected, raised once
## ---------------------------------------------------------------------------

#' Raise what a predict() call collected, once per topic, and return
#'
#' The end both predict methods share. `predict()` reads the object and
#' writes nothing to it, so the collector's rows go nowhere but the warnings:
#' [signal_log()] raises each topic once, carrying its row as the field `log`.
#' Under `options(warn = 2)` the warnings no handler takes end the call as
#' `horizons_error_promoted_warnings`, with the predictions as `object`.
#'
#' @param collector The call's collector, from `log_open("predict")`.
#' @param predictions The tibble the call returns.
#' @return `predictions`.
#' @keywords internal
#' @noRd
finish_predict <- function(collector, predictions) {

  log_close(collector)
  signal_log(collector, predictions)

  predictions

}

#' Whether a prediction tibble carries any interval
#'
#' @param predictions A tibble `predict()` is about to return.
#' @return Logical(1): `TRUE` when an interval bound holds a value.
#' @keywords internal
#' @noRd
intervals_returned <- function(predictions) {

  ".pred_lower" %in% names(predictions) && any(!is.na(predictions$.pred_lower))

}

#' The reports a configuration's block carries to predict()'s end
#'
#' [predict_one_config()] returns its block with what the call must report
#' about it as attributes; `predict()` reads them and drops them before the
#' blocks are bound.
#'
#' @keywords internal
#' @noRd
BLOCK_REPORTS <- c("failed", "unit_records", "winsorized", "flagged")

#' @noRd
drop_block_reports <- function(block) {

  for (a in BLOCK_REPORTS) attr(block, a) <- NULL
  block

}

#' Collect one warning for intervals asked for where there is no bundle
#'
#' Under `interval = TRUE` a configuration (or an ensemble) without a UQ bundle
#' returns `NA` bounds; one `skipped` warning names them (#145). Under the
#' default, `interval = NULL`, there is nothing to say: intervals were asked
#' for only where they exist, and `fit()` (or `ensemble()`) said when it
#' skipped them.
#'
#' @param collector The call's collector.
#' @param ids Character. The configurations without a bundle, or
#'   `"ensemble"`.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_missing_intervals <- function(collector, ids) {

  if (length(ids) == 0) return(invisible(collector))

  ensemble <- identical(ids, "ensemble")
  n        <- length(ids)

  log_warning(collector, "skipped", c(
    if (ensemble) {
      "Prediction intervals were asked for, but the ensemble has no CV+ bundle, so its bounds are {.code NA}."
    } else {
      "Prediction intervals were asked for, but {n} configuration{?s} {?has/have} no UQ bundle, so {?its/their} bounds are {.code NA}: {.val {ids}}."
    },
    "i" = if (ensemble) {
      "{.fn ensemble} computed none ({.code compute_uq = FALSE}, or too few calibration rows; its log says which)."
    } else {
      "{.fn fit} computed none ({.code compute_uq = FALSE}, or too few calibration rows; its log says which)."
    },
    "i" = "Leave {.arg interval} at {.code NULL} for intervals only where they exist, or set it to {.code FALSE}."
  ), ids = if (ensemble) character(0) else ids)

}

#' Collect one warning for the predictions the response bound winsorized
#'
#' @param collector The call's collector.
#' @param reports List, one per block: `NULL`, or a list with `ids` (the
#'   samples winsorized), `n`, `max` (the largest value before the bound) and
#'   `bound`.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_winsorized <- function(collector, reports) {

  reports <- Filter(Negate(is.null), reports)

  if (length(reports) == 0) return(invisible(collector))

  ids   <- unique(unlist(lapply(reports, `[[`, "ids")))
  n     <- sum(vapply(reports, `[[`, integer(1), "n"))
  top   <- max(vapply(reports, `[[`, numeric(1), "max"))
  bound <- reports[[1]]$bound

  log_warning(collector, "applicability", c(
    "{n} prediction{?s} exceeded the response upper bound and {?was/were} winsorized.",
    "i" = "Max pre-clamp value: {round(top, 2)}; bound: {round(bound, 2)}.",
    "i" = "Large overshoots usually indicate extrapolation beyond the training domain."
  ), ids = ids, n = n)

}

#' The record of what the response bound winsorized in one block
#'
#' @param sample_id The block's sample ids.
#' @param values The values before the bound.
#' @param bounded From [apply_response_bound()].
#' @param bound The bound.
#' @return `NULL` when nothing was winsorized, else the list
#'   [log_winsorized()] reads.
#' @keywords internal
#' @noRd
winsorized_report <- function(sample_id, values, bounded, bound) {

  if (!any(bounded$over)) return(NULL)

  list(ids   = sample_id[bounded$over],
       n     = sum(bounded$over),
       max   = max(values[bounded$over]),
       bound = bound)

}

## ---------------------------------------------------------------------------
## check_predict_dots() — a level is ignored, any other argument is refused
## ---------------------------------------------------------------------------

#' Check the arguments that reached predict()'s dots
#'
#' Both predict methods take `...` because the [stats::predict()] generic
#' does, and neither uses it. Unchecked, anything passed there is dropped
#' silently: a misspelled argument, or a `level`, which looks like it should
#' choose the interval level and does not (#141). Intervals are at the level
#' the uncertainty was calibrated at, so a `level` warns that it is ignored
#' and names that level, and any other argument aborts, naming it. The dots
#' are read by name and never evaluated.
#'
#' @param ... The predict method's dots, forwarded.
#' @param .level_default Numeric or `NULL`. The level the returned intervals
#'   are at: `uq$level_default` of the bundle(s) in use. `NULL` (no bundle)
#'   names `DEFAULT_UQ_LEVEL`.
#' @param .collector The call's collector, which the `level` warning is added
#'   to (`horizons_warning_input`, raised at the end of the call).
#' @param .call The predict method's frame, for the error's call. The
#'   arguments are dot-prefixed so a user argument named `level_default`,
#'   `collector` or `call` lands in `...` and is checked, not bound to them.
#' @return Invisibly `NULL`; aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_predict_dots <- function(..., .level_default = NULL, .collector,
                               .call = rlang::caller_env()) {

  dots      <- rlang::enquos(...)
  dot_names <- names(dots) %||% rep("", length(dots))

  ## -------------------------------------------------------------------------
  ## Anything but `level` is an error
  ## -------------------------------------------------------------------------

  unknown <- dots[dot_names != "level"]

  if (length(unknown) > 0) {

    unknown_names <- dot_names[dot_names != "level"]

    named   <- unknown_names[nzchar(unknown_names)]
    unnamed <- vapply(unknown[!nzchar(unknown_names)], rlang::as_label,
                      character(1), USE.NAMES = FALSE)

    abort_horizons(c(
      "{.fn predict} got {length(unknown)} argument{?s} it does not take.",
      "x" = if (length(named) > 0) "Unknown: {.arg {named}}.",
      "x" = if (length(unnamed) > 0) "Unnamed: {.code {unnamed}}.",
      "i" = "Did you misspell an argument name?"
    ), kind = "input", call = .call)

  }

  ## -------------------------------------------------------------------------
  ## `level` is ignored, and says so
  ## -------------------------------------------------------------------------

  if ("level" %in% dot_names) {

    level <- unique(.level_default) %||% DEFAULT_UQ_LEVEL

    log_warning(.collector, "input", c(
      "{.arg level} is ignored. Prediction intervals are at the level they were calibrated at, {.val {level}}.",
      "i" = "{.fn predict} has no {.arg level} argument."
    ))

  }

  invisible(NULL)

}

## ---------------------------------------------------------------------------
## ensure_predict_namespaces() — predict-time namespace loading (silent helper)
## ---------------------------------------------------------------------------

#' Load the namespaces predict() needs for a fitted object
#'
#' A `horizons_fit` (or `horizons_ensemble`) stores a `butcher::butcher()`ed
#' `workflows` object per config. `stats::predict()` S3 dispatch on that
#' stored object only resolves if `workflows` (and `parsnip`, `recipes`) are
#' already loaded in the session — none of them are referenced via a
#' NAMESPACE import directive in this package (only `::`), so `library(
#' horizons)` alone does not load them. A fresh session predicting a
#' deserialized fit then fails with "no applicable method for 'predict'"
#' (#65) instead of a clear message about a missing package. Called at the
#' top of [predict.horizons_fit()] and [predict.horizons_ensemble()], with
#' `config_ids` already resolved to the configs actually being predicted (not
#' every config the object stores — a fit holding both an `rf` and a `mars`
#' config should not need `earth` installed just to predict the `rf` one).
#'
#' Most of this is an availability preflight, not a dispatch fix. Once
#' `workflows`/`parsnip` are loaded, `parsnip`'s own `predict.model_fit()`
#' dispatch loads each model's own engine package itself; horizons does not
#' need to preload it for dispatch to work (see the rationale comment on
#' [MODEL_PREDICT_PACKAGES], `R/constants.R`). Checking here instead means a
#' genuinely missing (Suggested) engine package aborts with an actionable,
#' package-naming message up front, rather than surfacing as an obscure
#' failure partway through prediction. `ranger` for the UQ quantile forest is
#' the one case that is genuinely dispatch-critical, not just availability:
#' `stats::predict()` there dispatches on a bare `ranger::ranger` object
#' directly, with no `parsnip`/`workflows` layer to auto-load it.
#'
#' @param object A `horizons_fit` or `horizons_ensemble`.
#' @param config_ids Character vector of config_ids actually being predicted
#'   (the resolved `config` argument for a fit, or the member set for an
#'   ensemble — see the call sites).
#' @param call The frame a refusal reports: the verb's.
#' @return Invisibly `NULL`. Called for the side effect of loading
#'   namespaces (and aborting when a required Suggested package is missing).
#' @keywords internal
#' @noRd
ensure_predict_namespaces <- function(object, config_ids, call = rlang::caller_env()) {

  configs <- object$config$configs

  models <- if (!is.null(configs) && "model" %in% names(configs)) {

    configs$model[configs$config_id %in% config_ids]

  } else {

    character(0)

  }

  ## ranger for the UQ quantile forest is only relevant to a plain
  ## horizons_fit: an ensemble's own intervals come from CV+ fold refits
  ## (fit_ensemble_uq()), not a quantile forest, and its members always
  ## predict with interval = FALSE (predict_members()), so the per-config UQ
  ## bundles inherited from the underlying fit are never consulted at
  ## ensemble predict time.
  needs_ranger <- !inherits(object, "horizons_ensemble") &&
    any(!vapply(object$models$uq[config_ids], is.null, logical(1)))

  needed <- compute_needed_predict_packages(models, needs_ranger)

  ## workflows/parsnip/recipes are needed to dispatch on ANY stored workflow
  ## at all, regardless of model — folded into the same collect-then-abort-
  ## once path below, rather than requireNamespace()d separately with their
  ## result silently discarded on failure.
  base_reason <- "predicting from a stored workflow"
  needed[["workflows"]] <- unique(c(needed[["workflows"]], base_reason))
  needed[["parsnip"]]   <- unique(c(needed[["parsnip"]],   base_reason))
  needed[["recipes"]]   <- unique(c(needed[["recipes"]],   base_reason))

  abort_on_missing_predict_packages(needed, call = call)

}

## ---------------------------------------------------------------------------
## compute_needed_predict_packages() — model(s) -> package(s) (pure helper)
## ---------------------------------------------------------------------------

#' Compute the predict-time packages a set of models needs
#'
#' Pure: no `requireNamespace()` calls, no side effects. Factored out of
#' [ensure_predict_namespaces()] so "which packages, and why" is directly
#' testable without touching the search path or building a fitted object.
#'
#' @param models Character vector of model short names in use (the `model`
#'   values of the config rows being predicted; may repeat).
#' @param needs_ranger_for_uq Logical. Add `ranger` (reason: "prediction
#'   intervals") when `TRUE` — the caller sets this when a per-config UQ
#'   bundle is present, since [fit_uq()]'s quantile forest is always a bare
#'   `ranger::ranger` object regardless of the config's own model.
#' @return Named list: package name -> character vector of reasons (model
#'   names, or a UQ note) that need it.
#' @keywords internal
#' @noRd
compute_needed_predict_packages <- function(models, needs_ranger_for_uq = FALSE) {

  needed <- list()

  for (m in unique(models)) {

    for (pkg in MODEL_PREDICT_PACKAGES[[m]] %||% character(0)) {

      needed[[pkg]] <- unique(c(needed[[pkg]], m))

    }

  }

  if (isTRUE(needs_ranger_for_uq)) {

    needed[["ranger"]] <- unique(c(needed[["ranger"]], "prediction intervals"))

  }

  needed

}

## ---------------------------------------------------------------------------
## abort_on_missing_predict_packages() — missing-package gate (silent helper)
## ---------------------------------------------------------------------------

#' Abort informatively when a predict-time package is not installed
#'
#' Factored out of [ensure_predict_namespaces()] so the missing-package abort
#' path is unit-testable against a synthetic `needed` mapping, without a real
#' fitted object or mocking `requireNamespace()`. Checks every needed package
#' and aborts once, naming every missing one and what needs it — not on the
#' first miss, which would hide the rest behind a fix-one-rerun-find-the-next
#' cycle.
#'
#' @param needed Named list: package name -> character vector of reasons
#'   (model names, or a UQ note) that need it.
#' @param call The frame the refusal reports: the verb's.
#' @return Invisibly `NULL`; aborts with class
#'   `horizons_error_missing_predict_package` (kind `dependency`).
#' @keywords internal
#' @noRd
abort_on_missing_predict_packages <- function(needed, call = rlang::caller_env()) {

  missing <- names(needed)[
    !vapply(names(needed), requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) == 0) {

    return(invisible(NULL))

  }

  ## Built with paste0(), not glue interpolation: pkg/reasons/hint are all
  ## package- or model-controlled strings (never an upstream error message),
  ## so there is no brace-injection risk here — but the resulting
  ## {.pkg ...} / {.code ...} spans are still literal cli markup, rendered
  ## when the abort parses the finished vector below.
  bullets <- vapply(missing, function(pkg) {

    reasons <- paste(needed[[pkg]], collapse = ", ")
    ## Every predict-time package is on CRAN
    hint    <- sprintf('install.packages("%s")', pkg)

    paste0("{.pkg ", pkg, "}, needed for ", reasons, ": {.code ", hint, "}")

  }, character(1))

  names(bullets) <- rep("x", length(bullets))

  abort_horizons(c(
    "Predicting from this object needs {length(missing)} package{?s} that {?is/are} not installed.",
    bullets
  ), kind = "dependency", class = "horizons_error_missing_predict_package", call = call)

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
#' @param call The frame a refusal reports: the verb's.
#' @return A tibble with `sample_id`, the predictor columns and `.flag` when
#'   the object has one; a bare data frame is returned whole, with a
#'   `sample_id` added when it has none.
#'   Aborts with class `horizons_error_input` when `new_data` is neither, or
#'   a `horizons_data` without predictors.
#' @keywords internal
#' @noRd
resolve_new_data <- function(new_data, call = rlang::caller_env()) {

  if (inherits(new_data, "horizons_data")) {

    analysis <- new_data$data$analysis
    role_map <- new_data$data$role_map

    pred_cols <- role_map$variable[role_map$role == "predictor"]
    id_col    <- role_map$variable[role_map$role == "id"][1]

    ## A row a step flagged is carried with its flag, so predict() returns it
    ## as NA rather than predicting it (flagged_rows())
    flag_col  <- intersect(".flag", names(analysis))

    if (length(pred_cols) == 0) {

      abort_horizons(c(
        "{.arg new_data} has no predictor columns in its role map.",
        "i" = "Give the spectral columns the {.val predictor} role, as {.fn spectra} does."
      ), kind = "input", call = call)

    }

    tibble::as_tibble(
      analysis[, c(id_col, pred_cols, flag_col), drop = FALSE]
    ) |>
      dplyr::rename(sample_id = dplyr::all_of(id_col))

  } else if (is.data.frame(new_data)) {

    ## Bare frame: require a sample identifier, synthesize one if absent.
    df <- tibble::as_tibble(new_data)

    if (!"sample_id" %in% names(df)) {

      df$sample_id <- as.character(seq_len(nrow(df)))

    }

    df

  } else {

    abort_horizons(
      "{.arg new_data} must be a {.cls horizons_data} object or a data frame, not {.obj_type_friendly {new_data}}.",
      kind = "input", call = call
    )

  }

}

## ---------------------------------------------------------------------------
## log_interval_quality() — intervals weaker than they look
## ---------------------------------------------------------------------------

#' Collect the warnings that the returned intervals are weaker than they look
#'
#' Two cases, each a `quality` warning raised once per `predict()` call, and
#' only when intervals came back (#145): a call that returns none has no
#' coverage claim to qualify.
#'
#' - **A selected training set.** `fit()`'s calibration rows are drawn from
#'   the training object; when it came from [select_training()] they were
#'   chosen for proximity to the targets, so they are not exchangeable with
#'   arbitrary prediction data and the conformal guarantee does not transfer.
#'   The ensemble's CV+ intervals inherit the same rows.
#' - **An ensemble over a response-trimmed fit.** When `validate()` asked for
#'   a response trim, `fit()` trains its members without the training rows
#'   outside the fences (#77), so the out-of-fold predictions the CV+
#'   intervals are built from cover only the rows inside them, and the
#'   intervals can undercover samples like those set aside (#103). A single
#'   fit draws its conformal calibration set from the untrimmed training
#'   part, so it is not affected.
#'
#' @param collector The call's collector.
#' @param object A `horizons_fit` or `horizons_ensemble`.
#' @param returned Logical(1). Whether the call returns any interval.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_interval_quality <- function(collector, object, returned) {

  if (!returned) return(invisible(collector))

  if (isTRUE(object$models$selection_present)) {

    log_warning(collector, "quality", c(
      "Conformal coverage is not guaranteed: this model was fit on a selected training set.",
      "i" = "{.fn select_training} chose the calibration rows for proximity to the targets, so exchangeability with {.arg new_data} does not hold.",
      "i" = "Use {.code $selection$target_distances} as the applicability signal for these predictions."
    ))

  }

  trim      <- object$evaluation$response_trim
  n_trimmed <- length(trim$trimmed_ids)

  if (inherits(object, "horizons_ensemble") && n_trimmed > 0) {

    fences <- paste0("[", signif(trim$lower, 4), ", ", signif(trim$upper, 4), "]")

    log_warning(collector, "quality", c(
      "The ensemble's intervals were calibrated within the training fences {fences}.",
      "i" = "{n_trimmed} training row{?s} outside them {cli::qty(n_trimmed)}{?was/were} trimmed as response outliers, so the members' out-of-fold predictions, which the CV+ intervals are built from, cover only the rows inside.",
      "i" = "The intervals can undercover samples outside the fences."
    ))

  }

  invisible(collector)

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
#' @param call The frame the error reports: `predict()`'s.
#' @return Invisibly TRUE; aborts with class `horizons_error_input` on a
#'   mismatch.
#' @keywords internal
#' @noRd
check_predictor_schema <- function(object, new_spectra, call = rlang::caller_env()) {

  ## fit() stored the training-axis predictor columns, which the fit validator
  ## at predict()'s door requires; check against them directly (no recipe
  ## re-introspection, which a butchered workflow can break).
  expected <- object$models$predictor_schema

  supplied <- setdiff(names(new_spectra), "sample_id")
  missing  <- setdiff(expected, supplied)

  if (length(missing) > 0) {

    abort_horizons(c(
      "{.arg new_data} is missing {length(missing)} predictor column{?s} the model expects.",
      "x" = "Missing (first few): {.val {utils::head(missing, 5)}}",
      "i" = "new_data must be on the training wavenumber axis. Run {.fn standardize} \\
             to the same grid the model was trained on (e.g. 600-4000 at 2 cm^-1) before predicting."
    ), kind = "input", call = call)

  }

  invisible(TRUE)

}

## ---------------------------------------------------------------------------
## resolve_config_ids() — map `config` argument to config_id(s)
## ---------------------------------------------------------------------------

#' Resolve the `config` argument to one or more config_ids
#'
#' @param object A `horizons_fit`.
#' @param config `"best"`, a specific config_id, or `"all"`: a single string,
#'   which `predict()` has checked.
#' @param call The frame a refusal reports: the verb's.
#' @return Character vector of config_ids present in `models$workflows`.
#'   Aborts with class `horizons_error_input` when `config` names none.
#' @keywords internal
#' @noRd
resolve_config_ids <- function(object, config, call = rlang::caller_env()) {

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
  if (!config %in% available) {

    abort_horizons(c(
      "Config {.val {config}} is not among the fitted models.",
      "i" = "Available: {.val {available}}, or {.val best} or {.val all}."
    ), kind = "input", call = call)

  }

  config

}

## ---------------------------------------------------------------------------
## check_abstention() — abstention needs every configuration's AD
## ---------------------------------------------------------------------------

#' Refuse abstention for a configuration without an applicability domain
#'
#' Under `abstain_ood = TRUE` a prediction that is not `NA` claims to be in
#' domain, and `predict()` returns no column saying which configurations were
#' checked, so a configuration without an AD bundle cannot take part.
#'
#' @param object A `horizons_fit`.
#' @param config_ids The configurations being predicted.
#' @param abstain_ood The call's `abstain_ood`.
#' @param call The frame the refusal reports: the verb's.
#' @return Invisibly `NULL`; aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_abstention <- function(object, config_ids, abstain_ood, call = rlang::caller_env()) {

  if (!abstain_ood) return(invisible(NULL))

  without <- config_ids[vapply(config_ids, function(id) is.null(object$models$ad[[id]]),
                               logical(1))]

  if (length(without) == 0) return(invisible(NULL))

  with_ad <- setdiff(config_ids, without)

  abort_horizons(c(
    "{.arg abstain_ood} needs an applicability domain for every configuration predicted.",
    "x" = "{length(without)} configuration{?s} {?has/have} none: {.val {without}}.",
    "i" = if (length(with_ad) > 0) {
      "Predict the ones that have one ({.val {with_ad}}) with {.arg config}, refit with {.code fit(compute_ad = TRUE)}, or drop {.arg abstain_ood}."
    } else {
      "Refit with {.code fit(compute_ad = TRUE)} and enough calibration rows, or drop {.arg abstain_ood}."
    }
  ), kind = "input", call = call)

}

## ---------------------------------------------------------------------------
## predict_one_config() — one configuration's predictions, as one unit
## ---------------------------------------------------------------------------

#' Predict from a single fitted config, as one unit
#'
#' Point predictions on the original response scale (back-transformed once),
#' plus conformal intervals when a UQ bundle is present and `interval = TRUE`,
#' plus the applicability-domain columns when an AD bundle is present and
#' `ad = TRUE`.
#'
#' The configuration's predictions are one unit ([run_unit()]): a run failure
#' in any step leaves its block `NA`, and the caller reports it. Its intervals
#' and its AD are units of their own inside it ([run_add_on()]): one that
#' fails leaves its columns `NA` and the predictions standing, and the
#' configuration names it in a `skipped` warning. What can never be contained
#' (a row-alignment tripwire, a bug) stops the call.
#'
#' The block's columns are fixed before anything runs, by the bundles and the
#' arguments: a column the configuration cannot fill is `NA`, never absent.
#'
#' @param object A `horizons_fit`.
#' @param config_id The config to predict from.
#' @param new_spectra Tibble from [resolve_new_data()] (sample_id + predictors).
#' @param interval `NULL`, `TRUE` or `FALSE`, as `predict()`'s: `NULL`
#'   computes intervals when the configuration has a UQ bundle; `TRUE` also
#'   gives a configuration without one `NA` interval columns.
#' @param clamp Logical; apply the deploy-time response-bound winsorization
#'   (`models$response_bound`). `TRUE` for user-facing single-model predictions;
#'   `FALSE` inside ensemble machinery, where member predictions are features —
#'   they must match the raw member OOF the meta-learner trained and calibrated
#'   on, and the guardrail is applied once at the ensemble output instead.
#' @param abstain_ood Logical; when `TRUE`, `.pred` and interval bounds are set
#'   to `NA` for samples flagged out-of-domain (`.ad_flag == "OOD"`), and for
#'   every sample when the AD could not be computed, while `.ad_distance` and
#'   `.ad_flag` are preserved. `FALSE` (default) leaves predictions untouched
#'   and only reports the AD columns.
#' @param ad Logical; compute the applicability-domain columns. `TRUE`
#'   (default) for user-facing predictions. `FALSE` for the ensemble member
#'   helpers, which keep only `.pred`. With `ad = FALSE` there is nothing to
#'   abstain on, so `abstain_ood` is not applied.
#' @return A tibble: sample_id, config_id, .pred (+ interval + AD columns),
#'   with what `predict()` reports about it as attributes (`BLOCK_REPORTS`):
#'   `"failed"` (logical(1)), `"unit_records"` (the unit's records, its
#'   add-ons' warnings among them) and `"winsorized"`. Aborts with class
#'   `horizons_error_internal` when the predictions, the interval columns or
#'   the AD columns do not have one row per row of `new_spectra`, since all
#'   three are attached to its rows by position.
#' @keywords internal
#' @noRd
predict_one_config <- function(object, config_id, new_spectra, interval,
                               clamp = TRUE, abstain_ood = FALSE, ad = TRUE) {

  workflow <- object$models$workflows[[config_id]]

  ## Per-config response transformation (for back-transforming to original
  ## scale). The fit validator at predict()'s door requires every workflow to
  ## name a configuration, and the configuration ids to be unique.
  cfg_row        <- object$config$configs[object$config$configs$config_id == config_id, ]
  transformation <- tolower(as.character(cfg_row$transformation))
  outcome_range  <- outcome_range_setting(object)
  bound          <- if (clamp) object$models$response_bound else NULL

  ## The add-ons asked for, and so the block's columns, are fixed before
  ## anything runs
  uq        <- if (!isFALSE(interval)) object$models$uq[[config_id]]
  ad_bundle <- if (ad) object$models$ad[[config_id]]

  block <- prediction_block(new_spectra$sample_id, config_id,
                            intervals = isTRUE(interval) || !is.null(uq),
                            ad        = !is.null(ad_bundle))

  winsorized     <- NULL
  add_on_records <- new_unit_records()

  ## The intervals and the AD are units of their own: their warnings are the
  ## configuration's, and a failure is reported by the configuration
  add_on <- function(expr, unit, on_failure) {
    out <- run_add_on(expr, unit, config_id, "configuration", on_failure)
    add_on_records <<- dplyr::bind_rows(add_on_records, out$records)
    out$value
  }

  ## A row flagged on arrival is not predicted (flagged_rows())
  arrived_flagged <- flagged_rows(new_spectra)
  after_recipe    <- rep(FALSE, nrow(new_spectra))

  unit <- run_unit({

    ## -----------------------------------------------------------------------
    ## Bake first: a row non-finite after the recipe is not predicted either
    ## -----------------------------------------------------------------------
    ## The engine, the quantile forest and the AD distance see only the rows
    ## that bake to finite values; the rest are NA in every column. The baked
    ## matrix feeds the AD distance, so the AD does not bake again.

    usable <- !arrived_flagged
    baked  <- NULL

    if (any(usable)) {

      baked <- unit_step("bake", bake_predictors(workflow, new_spectra[usable, , drop = FALSE]))

      check_rows_aligned(
        what       = paste0("Features baked for config '", config_id, "'"),
        to         = "the rows of new_data it was given",
        n          = nrow(baked),
        n_expected = sum(usable)
      )

      after_recipe[usable] <- rowSums(!is.finite(baked)) > 0

    }

    good  <- usable & !after_recipe
    rows  <- new_spectra[good, , drop = FALSE]
    baked <- baked[!after_recipe[usable], , drop = FALSE]

    out <- block

    if (any(good)) {

      ## --------------------------------------------------------------------
      ## Point prediction (transformed scale → back-transform ONCE to original)
      ## --------------------------------------------------------------------

      point_trans <- unit_step("predict", stats::predict(workflow, new_data = rows))$.pred

      ## Everything below is attached to the good rows by position: .pred, the
      ## interval columns and the AD columns, and abstain_ood blanks rows by the
      ## same alignment. parsnip returns one prediction per baked row, so a
      ## recipe step that dropped rows at bake would shift every label
      ## silently.
      check_rows_aligned(
        what       = paste0("Point predictions for config '", config_id, "'"),
        to         = "the rows of new_data",
        n          = length(point_trans),
        n_expected = nrow(rows)
      )

      ## Unconditional funnel call: the "none" branch is a passthrough, and the
      ## funnel clamps to the outcome's range, a floor at 0 under the default
      ## and no floor for a signed outcome (#76). The intervals below are built
      ## around the point before the response bound, so winsorizing first would
      ## drag both interval bounds down by the overshoot, which the documented
      ## contract ("interval bounds are not clamped") and the ensemble path both
      ## rule out.
      point_pred <- unit_step("back_transform", back_transform_predictions(
        point_trans, transformation, outcome_range = outcome_range
      ))

      ## Deploy-time winsorization guardrail, on .pred only. Ensemble machinery
      ## passes clamp = FALSE: the guardrail applies once at the ensemble
      ## output, keeping member features consistent with the raw member OOF the
      ## meta-learner trained and calibrated on.
      bounded <- apply_response_bound(point_pred, bound)

      out$.pred[good] <- bounded$values

      ## --------------------------------------------------------------------
      ## Intervals (conformal CQR), a unit of their own
      ## --------------------------------------------------------------------

      if (!is.null(uq)) {

        interval_cols <- add_on(
          predict_intervals(uq, point_pred, rows, config_id = config_id,
                            outcome_range = outcome_range),
          "intervals", skipped_add_on("Prediction intervals (UQ) were")
        )

        if (!is.null(interval_cols)) {

          check_rows_aligned(
            what       = paste0("Interval columns for config '", config_id, "'"),
            to         = "the rows of new_data",
            n          = nrow(interval_cols),
            n_expected = nrow(rows)
          )

          for (col in names(interval_cols)) out[[col]][good] <- interval_cols[[col]]

        }

      }

      ## --------------------------------------------------------------------
      ## Applicability domain, a unit of its own
      ## --------------------------------------------------------------------
      ## Independent of intervals: AD reports even for point-only predictions.
      ## Under abstention an AD that could not be computed blanks every
      ## prediction: an unknown domain is not in domain (decision 8).

      if (!is.null(ad_bundle)) {

        ad_failed <- if (abstain_ood) {
          function(cause) warn_in_unit("skipped", c(
            "The applicability domain (AD) was not computed, so the predictions are {.code NA} under {.code abstain_ood = TRUE}: {cause}"
          ))
        } else {
          skipped_add_on("The applicability domain (AD) was")
        }

        ad_cols <- add_on(predict_ad(baked, ad_bundle), "ad", ad_failed)

        if (!is.null(ad_cols)) {

          ## Checked before the assignment, and so before abstention blanks rows
          ## by it.
          check_rows_aligned(
            what       = paste0("Applicability-domain columns for config '", config_id, "'"),
            to         = "the rows of new_data",
            n          = nrow(ad_cols),
            n_expected = nrow(rows)
          )

          out$.ad_distance[good] <- ad_cols$.ad_distance
          out$.ad_flag[good]     <- ad_cols$.ad_flag

        }

        ## Abstention: NA out predictions for out-of-domain samples, keeping the
        ## AD columns so the caller can see how far out each sample was. Interval
        ## bounds are NA'd too — an interval on an abstained prediction is
        ## meaningless. An NA flag is never out of domain by itself (a row the
        ## AD could not score).
        if (abstain_ood) {

          ood <- if (is.null(ad_cols)) {
            good
          } else {
            !is.na(out$.ad_flag) & out$.ad_flag == "OOD"
          }

          pred_cols <- intersect(c(".pred", ".pred_lower", ".pred_upper", ".interval_width"),
                                 names(out))

          for (col in pred_cols) out[[col]][ood] <- NA_real_

        }

      }

      ## What the bound changed in the predictions served: a row abstained on
      ## is returned NA, so it is not reported as winsorized
      bounded$over <- bounded$over & !is.na(out$.pred[good])
      winsorized   <- winsorized_report(rows$sample_id, point_pred, bounded, bound)

    }

    out

  }, unit = "configuration", id = config_id)

  result <- if (unit$failed) block else unit$value

  attr(result, "failed")       <- unit$failed
  attr(result, "unit_records") <- dplyr::bind_rows(unit$records, add_on_records)
  attr(result, "winsorized")   <- if (!unit$failed) winsorized
  attr(result, "flagged")      <- list(
    arrived = new_spectra$sample_id[arrived_flagged],
    recipe  = if (!unit$failed) new_spectra$sample_id[after_recipe] else character(0)
  )

  result

}

#' Bake new data through a fitted workflow's recipe
#'
#' The predictors the model sees, from the recipe the workflow was fit with:
#' what `predict()` checks for non-finite rows, and what the applicability
#' domain is measured in.
#'
#' @param workflow A fitted (possibly butchered) workflow.
#' @param new_data The rows to bake.
#' @return Numeric matrix, one row per row of `new_data`.
#' @keywords internal
#' @noRd
bake_predictors <- function(workflow, new_data) {

  prepped <- workflows::extract_recipe(workflow, estimated = TRUE)

  as.matrix(recipes::bake(prepped, new_data = new_data, recipes::all_predictors()))

}

#' An empty block of one configuration's predictions
#'
#' The columns a configuration's block has, every value `NA`: what a failed
#' configuration returns, and what a successful one fills.
#'
#' @param sample_id The rows' sample ids.
#' @param config_id Character(1).
#' @param intervals,ad Logical(1). Whether the block has the interval and the
#'   applicability-domain columns.
#' @return A tibble.
#' @keywords internal
#' @noRd
prediction_block <- function(sample_id, config_id, intervals = FALSE, ad = FALSE) {

  n <- length(sample_id)

  block <- tibble::tibble(
    sample_id = sample_id,
    config_id = rep(config_id, n),
    .pred     = rep(NA_real_, n)
  )

  if (intervals) {

    block$.pred_lower     <- rep(NA_real_, n)
    block$.pred_upper     <- rep(NA_real_, n)
    block$.interval_width <- rep(NA_real_, n)

  }

  if (ad) {

    block$.ad_distance <- rep(NA_real_, n)
    block$.ad_flag     <- factor(rep(NA_character_, n), levels = AD_BINS)

  }

  block

}

## ---------------------------------------------------------------------------
## What the configurations' units report at predict()'s end
## ---------------------------------------------------------------------------

#' One row per failed unit, as `horizons_error_prediction_failed` carries them
#'
#' @param records Unit records, shaped like [new_unit_records()]'s.
#' @return Tibble: `unit` (`"configuration"`, or `"combination"` for an
#'   ensemble's), `config_id` (`NA` for a unit that is not a configuration),
#'   `step`, `class`, `error_message` (the step's words and the cause), one
#'   row per failure.
#' @keywords internal
#' @noRd
prediction_failures <- function(records) {

  failures <- records[records$type == "failure", , drop = FALSE]

  tibble::tibble(
    unit          = failures$unit,
    config_id     = ifelse(failures$unit == "configuration", failures$id, NA_character_),
    step          = failures$step,
    class         = failures$class,
    error_message = vapply(seq_len(nrow(failures)), function(i) {
      unit_failure_message(failures[i, , drop = FALSE], outside = "Prediction")
    }, character(1))
  )

}

#' Abort because nothing could be predicted
#'
#' Raised when every configuration asked for failed, or when an ensemble's
#' member or its combination failed (the ensemble combines every member, and
#' renormalising its weights would be a silent substitution). The failures
#' travel on the condition as `results`, as `evaluate()`'s and `fit()`'s do.
#'
#' @param records The units' records, holding their failures.
#' @param header Character(1). The problem statement, a cli message.
#' @param call The frame the error reports: the verb's.
#' @param .envir The environment `header` is interpolated in.
#' @return Does not return; aborts with class
#'   `horizons_error_prediction_failed` (kind `run`).
#' @keywords internal
#' @noRd
abort_prediction_failed <- function(records, header, call = rlang::caller_env(),
                                    .envir = rlang::caller_env()) {

  results <- prediction_failures(records)
  header  <- cli::format_inline(header, .envir = .envir)

  ## Each failure named by its configuration, or by its unit when it is not
  ## one (an ensemble's combination)
  named <- results
  named$config_id <- ifelse(is.na(named$config_id), named$unit, named$config_id)

  abort_horizons(c(
    cli_escape(header),
    distinct_config_errors(named),
    "i" = "The failures are on this condition as {.field results}: {.code rlang::last_error()$results}."
  ), kind = "run", class = "horizons_error_prediction_failed", results = results,
  call = call)

}

#' Collect what the configurations' units reported
#'
#' One `partial_failure` warning when some configurations could not predict
#' and others did, then what every unit reported, through the tail both
#' methods share ([log_prediction_reports()]).
#'
#' @param collector The call's collector.
#' @param blocks The configurations' blocks, from [predict_one_config()].
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_prediction_units <- function(collector, blocks) {

  records <- dplyr::bind_rows(lapply(blocks, attr, "unit_records"))
  failed  <- vapply(blocks, attr, logical(1), "failed")

  if (any(failed)) {

    results  <- prediction_failures(records)
    n_failed <- sum(failed)
    n_total  <- length(blocks)

    log_warning(collector, "partial_failure", c(
      "{n_failed} of {n_total} configuration{?s} could not predict, so {?its/their} rows are {.code NA}.",
      distinct_config_errors(results)
    ), ids = results$config_id)

  }

  log_prediction_reports(
    collector,
    records    = records,
    flagged    = stats::setNames(lapply(blocks, attr, "flagged"),
                                 vapply(blocks, function(b) b$config_id[1] %||% NA_character_,
                                        character(1))),
    winsorized = lapply(blocks, attr, "winsorized")
  )

}

#' Collect what predict()'s units reported, the same way for both methods
#'
#' The tail `predict()` on a fit and on an ensemble share: the warnings and
#' notes horizons raised inside a unit ([log_unit_records()]), another
#' package's warnings ([log_foreign_warnings()]), the rows that could not be
#' predicted ([log_flagged()]) and the predictions the response bound
#' winsorized ([log_winsorized()]).
#'
#' @param collector The call's collector.
#' @param records The units' records, shaped like [new_unit_records()]'s.
#' @param flagged Named list of the rows each configuration or member could
#'   not use, as [log_flagged()] reads it.
#' @param winsorized List of the winsorized reports, as [log_winsorized()]
#'   reads it.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_prediction_reports <- function(collector, records, flagged, winsorized) {

  log_unit_records(collector, records)
  log_foreign_warnings(collector, records)
  log_flagged(collector, flagged)
  log_winsorized(collector, winsorized)

  invisible(collector)

}

#' Collect one warning for the rows that could not be predicted
#'
#' A row flagged on arrival, or non-finite after a configuration's recipe, is
#' returned as `NA` (conditions.md: rows are removed only where modelling
#' happens). One `flagged` warning names them once, whatever the number of
#' configurations: `ids` the samples, and the configurations whose recipe
#' left them non-finite when more than one was predicted.
#'
#' @param collector The call's collector.
#' @param reports Named list, one per configuration (or member): `NULL`, or
#'   a list with `arrived` and `recipe`, the sample ids flagged on arrival and
#'   after the recipe.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_flagged <- function(collector, reports) {

  reports <- Filter(Negate(is.null), reports)

  arrived   <- unique(unlist(lapply(reports, `[[`, "arrived")))
  recipe_by <- lapply(reports, `[[`, "recipe")
  recipe    <- setdiff(unique(unlist(recipe_by)), arrived)
  ids       <- c(arrived, recipe)

  if (length(ids) == 0) return(invisible(collector))

  n         <- length(ids)
  n_arrived <- length(arrived)
  n_recipe  <- length(recipe)
  configs   <- names(recipe_by)[lengths(recipe_by) > 0]
  of        <- if (length(reports) > 1 && length(configs) > 0) {
    cli::format_inline(" of {.val {configs}}")
  } else {
    ""
  }

  log_warning(collector, "flagged", c(
    "{n} sample{?s} could not be predicted, so {?its row is/their rows are} {.code NA}.",
    "i" = if (n_arrived > 0) "{n_arrived} {?was/were} flagged on arrival ({.field .flag}): {shown_ids(arrived)}.",
    "i" = if (n_recipe > 0) "{n_recipe} {?was/were} not finite after the recipe{of}: {shown_ids(recipe)}."
  ), ids = ids, n = n)

}

#' The first ids of a set, and how many more
#'
#' @param ids Character.
#' @param n_show Integer. How many to name.
#' @return Character(1), ids quoted and comma-separated.
#' @keywords internal
#' @noRd
shown_ids <- function(ids, n_show = 5L) {

  shown <- paste0('"', utils::head(ids, n_show), '"', collapse = ", ")

  if (length(ids) > n_show) paste0(shown, " and ", length(ids) - n_show, " more") else shown

}

#' Collect the warnings another package raised inside a prediction unit
#'
#' `evaluate()` and `fit()` keep such a warning on the unit's row. `predict()`
#' returns predictions only, with no row to keep it on (decision 1), so each
#' distinct one is collected as a `quality` warning naming the units that
#' raised it, and raised once at the end of the call with the rest.
#'
#' @param collector The call's collector.
#' @param records Unit records, shaped like [new_unit_records()]'s.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_foreign_warnings <- function(collector, records) {

  foreign <- records[records$type == "warning" &
                       !startsWith(records$class, "horizons_warning_"), , drop = FALSE]

  for (text in unique(foreign$message)) {

    these   <- foreign$message == text
    unit    <- foreign$unit[these][1]
    ids     <- unique(foreign$id[these])
    n_units <- length(ids)

    ### The text is substituted, not inlined, so braces in it stay text.
    log_warning(collector, "quality", c(
      "A warning arose while predicting: {text}",
      "i" = "In {n_units} {unit}{cli::qty(n_units)}{?s}: {.val {ids}}."
    ), ids = ids)

  }

  invisible(collector)

}

## ---------------------------------------------------------------------------
## predict_members() — every ensemble member predicts new_data (silent helper)
## ---------------------------------------------------------------------------

#' Predict new spectra from each ensemble member
#'
#' Runs every member config through the same per-config unit the single fit
#' path uses ([predict_one_config()] — predict once, back-transform once) and
#' stacks the results into one long frame. Shared by `predict()` on an
#' ensemble and by `ensemble()`'s scoring of the test rows
#' (`predict_members_on_test()`).
#'
#' Members are predicted point-only (`interval = FALSE`); the ensemble's own
#' uncertainty is conformalized on the meta-learner's residuals downstream, not
#' assembled from member intervals.
#'
#' A member that cannot predict aborts with `horizons_error_prediction_failed`,
#' every failed member in its `results`: the meta-learner's combination is
#' only valid over the exact member set it was trained on, so dropping a
#' member would silently change what is being predicted.
#'
#' @param object A `horizons_ensemble` (or any object carrying
#'   `models$workflows`).
#' @param members Character vector of member `config_id`s to predict.
#' @param new_spectra Tibble from [resolve_new_data()] (sample_id + predictors).
#' @param call The frame the abort reports.
#' @return A long tibble: `config_id`, `sample_id`, `.pred` (original scale),
#'   one block per member, `NA` for the rows a member could not use, with the
#'   members' unit records as the attribute `"unit_records"` and the rows
#'   each could not use as `"flagged"`.
#' @keywords internal
#' @noRd
predict_members <- function(object, members, new_spectra, call = rlang::caller_env()) {

  ## sample_id is the join key for every downstream combine (weighted average
  ## groups on it; the wide pivot keys rows on it): predict()'s door has
  ## refused new_data whose sample_ids repeat, and the fit validator refuses
  ## a split whose test rows repeat one.

  ## clamp = FALSE: member predictions are meta-learner FEATURES here, and
  ## must match the raw (unclamped) member OOF the meta-learner trained and
  ## the UQ fold models calibrated on. The response-bound guardrail applies
  ## once, at the combined ensemble output in predict.horizons_ensemble().
  ## ad = FALSE: only .pred is kept, so a member's AD is not computed.
  blocks <- lapply(members, function(m) {
    predict_one_config(object, config_id = m, new_spectra = new_spectra,
                       interval = FALSE, clamp = FALSE, ad = FALSE)
  })

  records <- dplyr::bind_rows(lapply(blocks, attr, "unit_records"))
  failed  <- vapply(blocks, attr, logical(1), "failed")

  if (any(failed)) {

    n_failed <- sum(failed)
    n_total  <- length(members)

    abort_prediction_failed(
      records,
      "The ensemble cannot predict: {n_failed} of its {n_total} member{?s} failed, and it combines every member.",
      call = call
    )

  }

  out <- dplyr::bind_rows(lapply(seq_along(members), function(i) {
    tibble::tibble(config_id = members[i], sample_id = blocks[[i]]$sample_id,
                   .pred = blocks[[i]]$.pred)
  }))

  attr(out, "unit_records") <- records
  attr(out, "flagged")      <- stats::setNames(lapply(blocks, attr, "flagged"), members)

  out

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
#' A failure in the bake or the forest raises, labelled with its step
#' (`uq_bake`, `uq_quantiles`): the caller runs this as a unit of its own (a
#' configuration's intervals in `predict()`, a member's measured coverage in
#' `fit()`), which records it and says the intervals were not computed. A
#' missing `ranger` is refused at `predict()`'s door, so it cannot arrive
#' here (#65).
#'
#' The intervals are at the level the bundle was calibrated at,
#' `uq$level_default`. There is no `level` argument: the body sets `level`
#' from the bundle on its first line, which is where a level chosen at
#' predict time would enter (#142).
#'
#' @param uq A UQ bundle from `models$uq[[config_id]]`.
#' @param point_pred Numeric point predictions, original scale.
#' @param new_spectra Tibble of new data (sample_id + predictors).
#' @param config_id Character(1) or `NULL`. The config this bundle belongs
#'   to, named in a row-alignment failure. `NULL` (default) omits it.
#' @param outcome_range Numeric length-2 vector the bounds are clamped to,
#'   from [outcome_range_setting()]. Default `DEFAULT_OUTCOME_RANGE`.
#' @return Tibble of interval columns, one row per row of `new_spectra`.
#'   Aborts with class `horizons_error_internal` when `point_pred` or the
#'   baked features do not have one row per row of `new_spectra`.
#' @keywords internal
#' @noRd
predict_intervals <- function(uq, point_pred, new_spectra, config_id = NULL,
                              outcome_range = DEFAULT_OUTCOME_RANGE) {

  ## Intervals are returned at the coverage level the UQ was calibrated for.
  level <- uq$level_default
  alpha <- 1 - level

  ## The bounds are point_pred + q_low and point_pred + q_high, by position,
  ## and the result is attached to new_data's rows by the caller. Vectors of
  ## different lengths recycle without an error when one length divides the
  ## other, so both sides are checked against new_data before they meet.
  for_config <- if (is.null(config_id)) "" else paste0(" for config '", config_id, "'")

  check_rows_aligned(
    what       = paste0("Point predictions", for_config),
    to         = "the rows of new_data",
    n          = length(point_pred),
    n_expected = nrow(new_spectra)
  )

  ## Bake new_data through the UQ recipe (predictors only — the same feature
  ## space the quantile forest was trained on).
  new_features <- unit_step("uq_bake", as.data.frame(
    recipes::bake(uq$prepped_recipe, new_data = new_spectra, recipes::all_predictors())
  ))

  ## A bake that ran but dropped rows is a bug, not a reason to return point
  ## predictions only.
  check_rows_aligned(
    what       = paste0("Features baked through the UQ recipe", for_config),
    to         = "the rows of new_data",
    n          = nrow(new_features),
    n_expected = nrow(new_spectra)
  )

  ## Residual quantiles from the quantile forest (original scale).
  q <- unit_step("uq_quantiles", stats::predict(
    uq$quantile_model,
    data      = new_features,
    type      = "quantiles",
    quantiles = c(alpha / 2, 1 - alpha / 2)
  ))

  q_low  <- q$predictions[, 1]
  q_high <- q$predictions[, 2]

  ## Conformal margin from the (signed) calibration scores. May be negative —
  ## that is the point of signed CQR.
  c_alpha <- compute_c_alpha(uq$scores, level)

  ## Assemble bounds. Every term is original-scale; do NOT back-transform.
  lower <- point_pred + q_low  - c_alpha
  upper <- point_pred + q_high + c_alpha

  ## Crossing / negative-width repair (signed c_alpha makes this necessary),
  ## then clamp to the outcome's range (a floor at 0 under the default, #76)
  ## — applied after repair so ordering is preserved.
  lo <- clamp_to_outcome_range(pmin(lower, upper), outcome_range)
  hi <- clamp_to_outcome_range(pmax(lower, upper), outcome_range)

  tibble::tibble(
    .pred_lower     = lo,
    .pred_upper     = hi,
    .interval_width = hi - lo
  )

}
