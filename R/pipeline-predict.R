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
#'   UQ bundle. When intervals are actually available and the model was fit on
#'   a training object carrying a `$selection` (from `select_training()`), this
#'   warns once that conformal coverage is not guaranteed: the calibration rows
#'   were chosen for proximity to the targets, so they are not exchangeable
#'   with arbitrary prediction data.
#' @param abstain_ood Logical. When `TRUE`, `.pred` (and any interval bounds)
#'   are set to `NA` for samples flagged out-of-domain (`.ad_flag == "OOD"`),
#'   while `.ad_distance`/`.ad_flag` are preserved so the caller can see how far
#'   out each sample was. Default `FALSE` (report AD, predict for all samples).
#'   Has no effect on configs without an AD bundle.
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
#' **Outcome range.** Point predictions and interval bounds are clamped to
#' the `outcome_range` given to [configure()], silently, since the range is
#' the outcome's physical range. Under the default, `c(0, Inf)`, that is a
#' floor at zero, and objects configured before the range existed predict
#' under it; a signed property configured with `c(-Inf, Inf)` is not clamped
#' at all (#76).
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
                                 interval    = TRUE,
                                 abstain_ood = FALSE,
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

  ## Which configs are being predicted decides which covariates are required:
  ## a covariate a config uses is a genuine predictor in that workflow's
  ## blueprint, so it must survive into new_spectra and be validated. Resolved
  ## here, ahead of the data gate, rather than in Step 1. resolve_config_ids()
  ## needs no namespace loading itself, so it runs first.
  config_ids <- resolve_config_ids(object, config)

  ## `library(horizons)` does not load workflows, most modeling engines, or
  ## ranger (#65) — none of them are referenced via NAMESPACE import
  ## directives, only `::`. A stored workflow's S3 predict method therefore
  ## fails to dispatch in a fresh session unless the caller has separately
  ## loaded the right namespace. Scoped to config_ids (not every config the
  ## object stores): a fit holding both an rf and a mars config should not
  ## need earth installed just to predict the rf one. Also has to run before
  ## fitted_extra_predictors() below — its extract_mold() call silently
  ## returns nothing (via a tryCatch) when workflows is not yet loaded.
  ensure_predict_namespaces(object, config_ids)

  keep_extra <- fitted_extra_predictors(object, config_ids)

  new_spectra <- resolve_new_data(new_data, keep_extra = keep_extra)

  ## Validate new_data carries the training-axis predictor columns fit() stored,
  ## plus any covariate those configs promoted to predictor. What is *required*
  ## comes from the config rows only; see fitted_extra_predictors().
  check_predictor_schema(
    object,
    new_spectra,
    required_extra = fitted_extra_predictors(object, config_ids,
                                             include_blueprint = FALSE)
  )

  ## -------------------------------------------------------------------------
  ## Step 0c: Conformal coverage on a selected training set
  ## -------------------------------------------------------------------------

  warn_selection_intervals(object, interval)

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
#' config should not need `earth` installed just to predict the `rf` one),
#' and before [fitted_extra_predictors()] — its `extract_mold()` call
#' silently returns nothing (via a `tryCatch`) when `workflows` is not yet
#' loaded, so this has to run first for that function's answer to be right.
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
#' @return Invisibly `NULL`. Called for the side effect of loading
#'   namespaces (and aborting when a required Suggested package is missing).
#' @keywords internal
#' @noRd
ensure_predict_namespaces <- function(object, config_ids) {

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

  abort_on_missing_predict_packages(needed)

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
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
abort_on_missing_predict_packages <- function(needed) {

  missing <- names(needed)[
    !vapply(names(needed), requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) == 0) {

    return(invisible(NULL))

  }

  ## Built with paste0(), not glue interpolation: pkg/reasons/hint are all
  ## package- or model-controlled strings (never an upstream error message),
  ## so there is no brace-injection risk here the way there is for
  ## warn_interval_failure()'s `detail` — but the resulting {.pkg ...} /
  ## {.code ...} spans are still literal cli markup, rendered when cli_abort()
  ## parses the finished vector below.
  bullets <- vapply(missing, function(pkg) {

    reasons <- paste(needed[[pkg]], collapse = ", ")
    hint    <- predict_package_install_hint(pkg)

    paste0("{.pkg ", pkg, "}, needed for ", reasons, ": {.code ", hint, "}")

  }, character(1))

  names(bullets) <- rep("x", length(bullets))

  cli::cli_abort(c(
    "Predicting from this object needs {length(missing)} package{?s} that {?is/are} not installed.",
    bullets
  ), class = "horizons_missing_predict_package")

}

## ---------------------------------------------------------------------------
## predict_package_install_hint() — CRAN vs. Bioconductor install() call
## ---------------------------------------------------------------------------

#' Install hint for a missing predict-time package
#'
#' Nearly every predict-time package installs with `install.packages()`.
#' `mixOmics` (the `plsr` engine) is the one exception, on Bioconductor
#' rather than CRAN — `install.packages("mixOmics")` cannot find it, so the
#' generic hint would be actively wrong there (see README.md's Dependencies
#' section for the same guidance).
#'
#' @param pkg Character(1). Package name.
#' @return Character(1). An `install.packages()` or `BiocManager::install()`
#'   call, as a string.
#' @keywords internal
#' @noRd
predict_package_install_hint <- function(pkg) {

  PREDICT_PACKAGE_INSTALL_HINT[[pkg]] %||% sprintf('install.packages("%s")', pkg)

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
#' Columns named in `keep_extra` are carried through even when their role in
#' `new_data` is `covariate`. A covariate a config requested is promoted to
#' `predictor` inside that config's recipe, so it is a genuine predictor in the
#' workflow's blueprint and is required at forge time; stripping it here on the
#' strength of its role in the *new* object aborted `predict()` for every
#' config that used one. Covariates the fitted configs do not use are still
#' dropped.
#'
#' @param new_data A `horizons_data` or tibble/data.frame.
#' @param keep_extra Character vector of non-`predictor` columns the fitted
#'   models need. Default none, which is the historical behaviour.
#' @return A tibble with `sample_id`, the predictor columns, and any
#'   `keep_extra` columns present in `new_data`.
#' @keywords internal
#' @noRd
resolve_new_data <- function(new_data, keep_extra = character(0)) {

  if (inherits(new_data, "horizons_data")) {

    analysis <- new_data$data$analysis
    role_map <- new_data$data$role_map

    pred_cols <- role_map$variable[role_map$role == "predictor"]
    id_col    <- role_map$variable[role_map$role == "id"][1]

    if (length(pred_cols) == 0) {

      cli::cli_abort("{.arg new_data} has no predictor columns in its role map.")

    }

    ## Missing ones are not backfilled here; check_predictor_schema() names
    ## them, which is a better error than hardhat's forge failure.
    extra_cols <- intersect(setdiff(keep_extra, pred_cols), names(analysis))

    tibble::as_tibble(
      analysis[, c(id_col, pred_cols, extra_cols), drop = FALSE]
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

    cli::cli_abort(c(
      "{.arg new_data} must be a {.cls horizons_data} object or a data frame.",
      "x" = "Got {.cls {class(new_data)}}."
    ))

  }

}

## ---------------------------------------------------------------------------
## warn_selection_intervals() — conformal coverage on a selected training set
## ---------------------------------------------------------------------------

#' Warn that conformal coverage does not transfer from a selected training set
#'
#' `fit()`'s calibration rows are drawn from the training object. When that
#' object came from [select_training()], those rows were chosen for proximity
#' to the targets, so they are not exchangeable with arbitrary prediction data
#' and the conformal guarantee does not transfer. Warned once per `predict()`
#' call, before any per-config or per-member loop, so a `config = "all"`
#' prediction (or an ensemble over many members) says it once rather than once
#' per model. Nothing about the computation changes.
#'
#' Gated on intervals actually being produced. With no UQ bundle no intervals
#' are returned at all and `interval` defaults to `TRUE`, so warning there
#' would fire on every point prediction from a selected fit. Which slot decides
#' that differs by class: a `horizons_fit` returns intervals from its per-config
#' bundles (`models$uq`, via [has_uq()]), while an ensemble returns them only
#' from its own conformal slot (`ensemble$uq`) — member UQ bundles are not used
#' at the ensemble output.
#'
#' @param object A `horizons_fit` or `horizons_ensemble`.
#' @param interval The call's `interval` argument.
#' @return Invisibly `NULL`. Called for the warning.
#' @keywords internal
#' @noRd
warn_selection_intervals <- function(object, interval) {

  has_intervals <- if (inherits(object, "horizons_ensemble")) {

    !is.null(object$ensemble$uq)

  } else {

    has_uq(object)

  }

  if (isTRUE(interval) && isTRUE(object$models$selection_present) &&
      has_intervals) {

    cli::cli_warn(c(
      "!" = "Conformal coverage is not guaranteed: this model was fit on a selected training set.",
      "i" = "{.fn select_training} chose the calibration rows for proximity to the targets, so exchangeability with {.arg new_data} does not hold.",
      "i" = "Use {.code $selection$target_distances} as the applicability signal for these predictions."
    ), class = "horizons_select_warning")

  }

  invisible(NULL)

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
#' @param required_extra Character vector of non-spectral columns the configs
#'   being predicted need (covariates promoted to predictor). Default none.
#' @return Invisibly TRUE; aborts on mismatch.
#' @keywords internal
#' @noRd
check_predictor_schema <- function(object, new_spectra,
                                   required_extra = character(0)) {

  ## fit() stored the training-axis predictor columns; validate against them
  ## directly (no recipe re-introspection, which a butchered workflow can break).
  expected <- object$models$predictor_schema

  supplied <- setdiff(names(new_spectra), "sample_id")

  ## Covariates a config promoted to predictor are required at forge time but
  ## are not in predictor_schema, which records the spectral axis only. Name
  ## them here rather than letting hardhat's forge error surface instead.
  ##
  ## Checked BEFORE the predictor_schema NULL escape below: the covariate
  ## requirement comes from the config rows, not from the schema, so it holds
  ## for objects written before predictor_schema existed too. Behind the escape
  ## it silently did not run on exactly those objects.
  missing_extra <- setdiff(required_extra, supplied)

  if (length(missing_extra) > 0) {

    cli::cli_abort(c(
      "{.arg new_data} is missing {length(missing_extra)} covariate column{?s} the fitted model uses as a predictor.",
      "x" = "Missing: {.val {missing_extra}}",
      "i" = "{cli::qty(length(missing_extra))}The config being predicted was trained with {?this covariate/these covariates}, so {?it is/they are} part of its predictor set.",
      "i" = "{cli::qty(length(missing_extra))}Supply {?it/them} in {.arg new_data} (see {.fn add_covariates}), or predict with a config that does not use {?it/them}."
    ))

  }

  ## Objects written before predictor_schema existed: skip the axis gate, let
  ## bake() surface any mismatch.
  if (is.null(expected)) {

    return(invisible(TRUE))

  }

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
## fitted_extra_predictors() — covariates a fitted config needs at predict time
## ---------------------------------------------------------------------------

#' Non-spectral predictor columns the fitted configs require
#'
#' `fit()` records `models$predictor_schema` from the role map's `predictor`
#' role, which is the spectral axis; a covariate this config requested carries
#' the `covariate` role there and is promoted to `predictor` only inside
#' `build_recipe()`. It is nonetheless in the workflow blueprint's predictor
#' ptype and required at forge time, so `predict()` has to know about it.
#'
#' Two sources, unioned. The blueprint is authoritative but often unavailable:
#' `butcher()` strips the mold from a stored workflow. The config row's
#' `covariates` field is what `build_recipe()` promoted in the first place, and
#' survives butchering, so it is the dependable one.
#'
#' @param object A `horizons_fit`.
#' @param config_ids Configs being predicted. Default all fitted workflows.
#' @param include_blueprint Read the blueprint too? `TRUE` when deciding which
#'   columns to keep, where being generous costs nothing. `FALSE` when deciding
#'   what to *require*, so a blueprint listing something unexpected cannot
#'   abort a prediction that would have worked.
#' @return Character vector of required columns outside `predictor_schema`.
#' @keywords internal
#' @noRd
fitted_extra_predictors <- function(object, config_ids = NULL,
                                    include_blueprint = TRUE) {

  ids    <- config_ids %||% names(object$models$workflows)
  schema <- object$models$predictor_schema

  from_blueprint <- if (include_blueprint) {

    unlist(
      lapply(object$models$workflows[ids], function(wf) {

        tryCatch(
          names(hardhat::extract_mold(wf)$blueprint$ptypes$predictors),
          error = function(e) character(0)
        )

      }),
      use.names = FALSE
    )

  } else {

    character(0)

  }

  configs <- object$config$configs

  from_configs <- if (!is.null(configs) && "covariates" %in% names(configs)) {

    rows <- configs[configs$config_id %in% ids, , drop = FALSE]

    unlist(
      lapply(rows$covariates, function(v) {

        tryCatch(parse_config_covariates(v), error = function(e) NULL)

      }),
      use.names = FALSE
    )

  } else {

    character(0)

  }

  extra <- setdiff(unique(c(from_blueprint, from_configs)), schema)

  extra[!is.na(extra)]

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
#' @param clamp Logical; apply the deploy-time response-bound winsorization
#'   (`models$response_bound`). `TRUE` for user-facing single-model predictions;
#'   `FALSE` inside ensemble machinery, where member predictions are features —
#'   they must match the raw member OOF the meta-learner trained and calibrated
#'   on, and the guardrail is applied once at the ensemble output instead.
#' @param abstain_ood Logical; when `TRUE`, `.pred` and interval bounds are set
#'   to `NA` for samples flagged out-of-domain (`.ad_flag == "OOD"`), while
#'   `.ad_distance`/`.ad_flag` are preserved. `FALSE` (default) leaves
#'   predictions untouched and only reports the AD columns.
#' @param ad Logical; compute the applicability-domain columns. `TRUE`
#'   (default) for user-facing predictions. `FALSE` for the ensemble member
#'   helpers, which keep only `.pred`: computing a member's AD there wasted a
#'   bake per member and raised AD warnings about columns the ensemble never
#'   returns. With `ad = FALSE` there is nothing to abstain on, so
#'   `abstain_ood` is not applied.
#' @return A tibble: sample_id, config_id, .pred (+ interval + AD columns).
#' @keywords internal
#' @noRd
predict_one_config <- function(object, config_id, new_spectra, interval,
                               clamp = TRUE, abstain_ood = FALSE, ad = TRUE) {

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
  ## funnel clamps to the outcome's range, a floor at 0 under the default and
  ## no floor for a signed outcome (#76). It takes no response bound here.
  ## The intervals below are built around the point before the guardrail, so
  ## winsorizing first would drag both interval bounds down by the overshoot,
  ## which the documented contract ("interval bounds are not clamped") and the
  ## ensemble path both rule out.
  outcome_range <- outcome_range_setting(object)

  point_pred <- back_transform_predictions(
    point_trans,
    transformation,
    warn          = FALSE,
    outcome_range = outcome_range
  )

  ## Deploy-time winsorization guardrail, on .pred only. Old objects without
  ## response_bound degrade gracefully (NULL → no clamp), mirroring the
  ## predictor_schema NULL-skip above. Fit-time paths deliberately do NOT pass
  ## a bound — ranking must see raw model behavior — and ensemble machinery
  ## passes clamp = FALSE (guardrail applies once at the ensemble output,
  ## keeping member features consistent with the raw member OOF the
  ## meta-learner trained and calibrated on).
  out <- tibble::tibble(
    sample_id = new_spectra$sample_id,
    config_id = config_id,
    .pred     = apply_response_bound(
      point_pred,
      if (clamp) object$models$response_bound else NULL
    )
  )

  ## -------------------------------------------------------------------------
  ## Intervals (conformal CQR) — only if requested and a UQ bundle exists
  ## -------------------------------------------------------------------------

  uq <- object$models$uq[[config_id]]

  if (interval && !is.null(uq)) {

    interval_cols <- predict_intervals(
      uq            = uq,
      point_pred    = point_pred,
      new_spectra   = new_spectra,
      config_id     = config_id,
      outcome_range = outcome_range
    )

    ## predict_intervals() returns NULL if quantile prediction fails — degrade
    ## gracefully to point-only rather than erroring.
    if (!is.null(interval_cols)) {

      out <- dplyr::bind_cols(out, interval_cols)

    }

  }

  ## -------------------------------------------------------------------------
  ## Applicability domain — .ad_distance / .ad_flag when a bundle exists
  ## -------------------------------------------------------------------------
  ## Independent of intervals: AD reports even for point-only predictions. Old
  ## objects without an AD bundle degrade to no AD columns quietly; a failed
  ## bake or distance degrades the same way, with a horizons_ad_warning.
  ## Callers that discard the AD columns (the ensemble member helpers) skip
  ## the computation, and so its warnings.

  if (!ad) return(out)

  ad_cols <- predict_ad(
    workflow    = workflow,
    ad_bundle   = object$models$ad[[config_id]],
    new_spectra = new_spectra,
    config_id   = config_id
  )

  if (!is.null(ad_cols)) {

    out <- dplyr::bind_cols(out, ad_cols)

    ## Abstention: NA out predictions for out-of-domain samples, keeping the AD
    ## columns so the caller can see how far out each sample was. Interval
    ## bounds are NA'd too — an interval on an abstained prediction is
    ## meaningless. .ad_distance / .ad_flag are deliberately preserved.
    if (abstain_ood) {

      ## Guard against an NA flag: assign_ad_bin() never produces one today
      ## (every finite distance bins, Inf -> OOD), but an NA in the mask would
      ## silently leave that row un-abstained, so exclude it explicitly.
      ood <- !is.na(out$.ad_flag) & out$.ad_flag == "OOD"

      if (any(ood)) {

        pred_cols <- intersect(
          c(".pred", ".pred_lower", ".pred_upper", ".interval_width"),
          names(out)
        )

        for (col in pred_cols) out[[col]][ood] <- NA_real_

      }

    }

  } else if (isTRUE(abstain_ood)) {

    ## Abstention was asked for and cannot happen: no AD bundle on this config,
    ## or predict_ad() could not score the batch. Saying so is the point — a
    ## silent pass-through returns exactly the unabstained predictions the
    ## caller asked not to get.
    cli::cli_warn(c(
      "!" = "{.arg abstain_ood} was requested but no applicability-domain information is available for config {.val {config_id}}.",
      "i" = "Either {.fn fit} was run with {.code compute_ad = FALSE} (or without enough calibration data), or the AD computation failed; see any warning above.",
      "i" = "Predictions are returned unabstained."
    ), class = "horizons_ad_warning")

  }

  out

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

    ## clamp = FALSE: member predictions are meta-learner FEATURES here, and
    ## must match the raw (unclamped) member OOF the meta-learner trained and
    ## the UQ fold models calibrated on. The response-bound guardrail applies
    ## once, at the combined ensemble output in predict.horizons_ensemble().
    ## ad = FALSE: only .pred is kept, so a member's AD is not computed.
    pc <- predict_one_config(object, config_id = m, new_spectra = new_spectra,
                             interval = FALSE, clamp = FALSE, ad = FALSE)

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
#' Degrades to `NULL` (point-only output) when either step fails — a missing
#' `ranger` namespace on the quantile forest (#65) is exactly this path — but
#' warns naming the failing step rather than degrading silently, via
#' [warn_interval_failure()].
#'
#' @param uq A UQ bundle from `models$uq[[config_id]]`.
#' @param point_pred Numeric point predictions, original scale.
#' @param new_spectra Tibble of new data (sample_id + predictors).
#' @param config_id Character(1) or `NULL`. The config this bundle belongs
#'   to, named in the warning on failure. `NULL` (default) omits it.
#' @param outcome_range Numeric length-2 vector the bounds are clamped to,
#'   from [outcome_range_setting()]. Default `DEFAULT_OUTCOME_RANGE`.
#' @param level Coverage level or NULL (-> `uq$level_default`).
#' @return Tibble of interval columns, or NULL if quantile prediction fails.
#' @keywords internal
#' @noRd
predict_intervals <- function(uq, point_pred, new_spectra, config_id = NULL,
                              outcome_range = DEFAULT_OUTCOME_RANGE) {

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

    warn_interval_failure("baking new data through the UQ recipe",
                          bake_result$error, config_id = config_id)
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

    warn_interval_failure("predicting quantiles from the UQ model",
                          q_result$error, config_id = config_id)
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

## ---------------------------------------------------------------------------
## warn_interval_failure() — name a silent interval-degrade (silent helper)
## ---------------------------------------------------------------------------

#' Warn that interval computation failed and predictions are point-only
#'
#' Before #65, [predict_intervals()] (and, for the ensemble path,
#' [predict_ensemble_intervals()]) degraded to `NULL` (point-only output) on
#' any failure with no message: a missing `ranger` namespace made
#' `stats::predict()` on the quantile forest error, the error was swallowed
#' by `safely_execute()`, and `interval = TRUE` came back with no interval
#' columns and no explanation. Warn instead, naming the step that failed and
#' the underlying reason.
#'
#' @param stage Character(1). Which step failed (for the warning text).
#' @param error The reason the step failed: an error condition (from a
#'   failing `safely_execute()` call), a plain character string (for a
#'   degrade path that has no caught condition, e.g. a structural check), or
#'   `NULL`.
#' @param config_id Character(1) or `NULL`. The config the failure belongs
#'   to, when there is a single one to name (there is not, for the
#'   ensemble's whole-bundle degrade points). `NULL` (default) omits it.
#' @return Invisibly `NULL`. Called for the warning.
#' @keywords internal
#' @noRd
warn_interval_failure <- function(stage, error, config_id = NULL) {

  detail <- if (is.null(error)) {

    "unknown error"

  } else if (inherits(error, "condition")) {

    conditionMessage(error)

  } else {

    as.character(error)

  }

  header <- if (is.null(config_id)) {

    "Prediction intervals could not be computed ({stage}); returning point predictions only."

  } else {

    "Prediction intervals could not be computed for config {.val {config_id}} ({stage}); returning point predictions only."

  }

  ## `detail` is upstream, possibly attacker- or data-derived text (a column
  ## name like a bare "{wn_600}" is a realistic example) — interpolated as a
  ## VALUE via "{detail}", never handed to cli as a template directly, or a
  ## brace or unmatched quote in the message would crash predict() here
  ## instead of just being reported (mirrors ad.R's bake_msg handling).
  cli::cli_warn(c(
    "!" = header,
    "x" = "{detail}"
  ), class = "horizons_interval_warning")

  invisible(NULL)

}
