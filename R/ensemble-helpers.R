# Internal helpers for the ensemble() verb
#
# These establish the contract every meta-learner engine fills. The load-bearing
# piece is build_oof_matrix(): it turns the flat cv_predictions tibble that fit()
# stores into the single canonical artifact — a wide [.row x member] matrix of
# out-of-fold predictions plus the aligned truth vector — that all three engines
# (linear / weighted / xgb) train on. No engine may read cv_predictions directly;
# they consume the matrix. That uniform input is what makes the meta-learner set
# extensible: a new engine is "fit a combiner on the matrix", nothing more.

## ---------------------------------------------------------------------------
## gather_members()
## ---------------------------------------------------------------------------

#' Identify Ensemble Members from a Fitted Object
#'
#' @description
#' Reads the fitted object and returns the configs eligible to be ensemble
#' members: those that fit successfully and have both a stored workflow and
#' out-of-fold predictions. This is a pure read of slots `fit()` already
#' populated — it does not refit, rerank, or recompute success.
#'
#' @param object A `horizons_fit` object (output of [fit()]).
#'
#' @return A list with:
#'   \describe{
#'     \item{members}{Character vector of member `config_id`s.}
#'     \item{workflows}{Named list of fitted workflows, one per member.}
#'     \item{best_config}{The top-ranked single config (the baseline the
#'       ensemble's improvement is measured against).}
#'     \item{rank_metric}{The metric the object was ranked by.}
#'   }
#'
#' @keywords internal
gather_members <- function(object) {

  if (!inherits(object, "horizons_fit")) {

    cli::cli_abort(c(
      "{.arg object} must be a {.cls horizons_fit} object.",
      "i" = "Run {.fn fit} first to produce a fitted object."
    ))

  }

  models  <- object$models
  results <- models$results

  ## A member needs all three: success status, a stored workflow, and OOF
  ## predictions. fit() only stores workflows for successes, but a config can
  ## succeed at fit and still lack cv_predictions (e.g. CV collection failed),
  ## so we intersect all three rather than trusting status alone.

  succeeded <- results$config_id[results$status == "success"]
  have_wf   <- names(models$workflows)
  have_oof  <- unique(models$cv_predictions$config_id)

  members <- intersect(intersect(succeeded, have_wf), have_oof)

  if (length(members) < 2) {

    cli::cli_abort(c(
      "An ensemble needs at least 2 members; found {length(members)}.",
      "i" = "Members must have a fitted workflow and out-of-fold predictions."
    ))

  }

  list(
    members     = members,
    workflows   = models$workflows[members],
    best_config = models$best_config,
    rank_metric = models$rank_metric
  )

}

## ---------------------------------------------------------------------------
## build_oof_matrix()
## ---------------------------------------------------------------------------

#' Build the Canonical Out-of-Fold Prediction Matrix
#'
#' @description
#' The single artifact every meta-learner engine consumes. Reshapes the flat
#' `cv_predictions` tibble (one row per sample-per-config) into a wide matrix
#' where each column is one member's out-of-fold prediction and each row is a
#' training sample, aligned by `.row`. The aligned `truth` vector travels with
#' it.
#'
#' Predictions are taken from `.pred` — the original response scale. The
#' transformed-scale `.pred_trans` column is deliberately not used: the
#' meta-learner trains on original-scale member predictions so that members
#' fit under different response transformations are combined on a common
#' scale. (This is the cardinal back-transform invariant: `fit()` has already
#' back-transformed `.pred` exactly once.)
#'
#' @param object A `horizons_fit` object.
#' @param members Character vector of member `config_id`s (from
#'   [gather_members()]).
#'
#' @return A list with:
#'   \describe{
#'     \item{predictors}{Tibble, one row per `.row`, one column per member
#'       (named `member_<config_id>`), holding original-scale OOF predictions.}
#'     \item{truth}{Numeric vector of observed values, aligned to
#'       `predictors` row order.}
#'     \item{row}{Integer vector of `.row` ids, aligned to `predictors`.}
#'     \item{members}{The member `config_id`s, in column order.}
#'   }
#'
#' @keywords internal
build_oof_matrix <- function(object, members) {

  cv <- object$models$cv_predictions

  cv <- cv[cv$config_id %in% members, c(".row", "config_id", ".pred", "truth")]

  ## Completeness guard: every member must cover the same .row set. A member
  ## missing rows would leave silent NA holes in the meta-feature matrix and
  ## produce a plausible-but-wrong ensemble. Fail loudly instead.

  per_member_rows <- split(cv$.row, cv$config_id)
  row_union       <- sort(unique(cv$.row))

  incomplete <- names(per_member_rows)[
    vapply(per_member_rows,
           function(r) !setequal(r, row_union),
           logical(1))
  ]

  if (length(incomplete) > 0) {

    cli::cli_abort(c(
      "Out-of-fold predictions are not row-aligned across members.",
      "x" = "Member{?s} with missing rows: {.val {incomplete}}.",
      "i" = "Every member must have an OOF prediction for each training sample."
    ))

  }

  ## Wide reshape: one column per member, rows keyed by .row. Sort by .row so
  ## the predictor matrix, truth, and row index are all in the same order.
  ## Routed through the safely_execute -> handle_results cascade: pivot_wider
  ## fails (silently building list-columns) on duplicate .row/config_id pairs,
  ## which the completeness guard above does not catch.

  ## Carry truth through the pivot — it is constant per .row, so it rides as an
  ## id column and comes out aligned, avoiding a separate dedup-and-reorder.

  wide_safe <- safely_execute(
    tidyr::pivot_wider(
      cv[, c(".row", "truth", "config_id", ".pred")],
      names_from   = "config_id",
      values_from  = ".pred",
      names_prefix = "member_"
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  wide <- handle_results(
    wide_safe,
    error_title = "Failed to reshape out-of-fold predictions into the member matrix.",
    error_hints = "Duplicate .row/config_id pairs in cv_predictions can cause this."
  )

  wide <- wide[order(wide$.row), ]

  ## Column order follows the members argument (stable, caller-controlled).
  member_cols <- paste0("member_", members)

  list(
    predictors = wide[, member_cols, drop = FALSE],
    truth      = wide$truth,
    row        = wide$.row,
    members    = members
  )

}

## ---------------------------------------------------------------------------
## horizons_metric_set()
## ---------------------------------------------------------------------------

#' The Package Regression Metric Set
#'
#' @description
#' The canonical six-metric panel (rmse, rrmse, rsq, ccc, rpd, mae) used for
#' regression scoring throughout the package. Returned as a `yardstick`
#' `metric_set` so it can drive `tune_grid()`; the data-consuming scorer
#' [compute_original_scale_metrics()] uses the same six metrics.
#'
#' @return A `yardstick` metric set.
#' @keywords internal
horizons_metric_set <- function() {

  yardstick::metric_set(yardstick::rmse, rrmse, yardstick::rsq, ccc, rpd,
                        yardstick::mae)

}

## ---------------------------------------------------------------------------
## predict_members_on_test()
## ---------------------------------------------------------------------------

#' Predict Each Member on the Held-Out Test Set
#'
#' @description
#' Has every ensemble member predict the Split-F assessment set (the held-out
#' evaluation data `fit()` reserved), via [predict_one_config()] — predict once,
#' back-transform once. Shared by all engines so the meaning of "test_F" and the
#' per-member prediction contract live in one place.
#'
#' @param object A `horizons_fit` object.
#' @param members Character vector of member `config_id`s.
#'
#' @return A long tibble: `config_id`, `sample_id`, `.pred`, `truth` (original
#'   scale), one block per member.
#'
#' @keywords internal
predict_members_on_test <- function(object, members) {

  test_data   <- rsample::assessment(object$models$split)
  role_map    <- object$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]

  ## Carry truth keyed by sample_id and join it on, rather than pairing it
  ## positionally — predict_one_config()'s output is keyed to sample_id, and
  ## recipes::bake() does not contract row-order preservation.
  truth_df <- tibble::tibble(
    sample_id = test_data$sample_id,
    truth     = test_data[[outcome_col]]
  )

  dplyr::bind_rows(lapply(members, function(m) {

    pc <- predict_one_config(object, config_id = m, new_spectra = test_data,
                             interval = FALSE)

    dplyr::left_join(
      tibble::tibble(config_id = m, sample_id = pc$sample_id, .pred = pc$.pred),
      truth_df,
      by = "sample_id"
    )

  }))

}

## ---------------------------------------------------------------------------
## fit_tuned_meta_learner()
## ---------------------------------------------------------------------------

#' Fit a Tuned Meta-Learner Over the Out-of-Fold Matrix
#'
#' @description
#' The shared scaffold behind the `penalized` and `xgb` engines: build the
#' meta-training frame from the OOF matrix, optionally tune the meta-learner's
#' hyperparameters by CV on that matrix (the tuning resamples come only from the
#' OOF matrix — `test_F` never enters the tuning path, so the reported
#' performance is honest), refit the winner on the full matrix, then have the
#' members predict `test_F` and combine those predictions *through* the fitted
#' meta-model. The engine supplies the model spec, the tuning grid, and a
#' closure that extracts member weights from the fitted model.
#'
#' @param object A `horizons_fit` object.
#' @param members Character vector of member `config_id`s.
#' @param oof The out-of-fold matrix list from [build_oof_matrix()].
#' @param rank_metric Character. Metric to tune and rank on.
#' @param optimize Logical. Tune hyperparameters (`TRUE`) or fit the spec as-is
#'   (`FALSE`, spec carries fixed values).
#' @param method Character. Method label for the contract and error messages.
#' @param spec A `parsnip` model spec (with `tune()` placeholders when
#'   `optimize = TRUE`, fixed values otherwise).
#' @param grid The tuning grid (ignored when `optimize = FALSE`).
#' @param extract_weights A function `(meta_fit, members) -> tibble(member,
#'   coef)` that reads the member weighting from the fitted model.
#'
#' @return The ensemble contract list from [build_ensemble_contract()].
#'
#' @keywords internal
#' @importFrom rlang .data
fit_tuned_meta_learner <- function(object,
                                   members,
                                   oof,
                                   rank_metric,
                                   optimize,
                                   method,
                                   spec,
                                   grid,
                                   extract_weights) {

  started <- Sys.time()

  member_cols <- paste0("member_", oof$members)

  meta_frame        <- oof$predictors
  meta_frame$.truth <- oof$truth

  meta_wflow <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_formula(.truth ~ .)

  ## Tune (CV on the OOF matrix only) then refit, or refit the fixed spec ----

  if (optimize) {

    folds <- rsample::vfold_cv(meta_frame, v = 5)

    tune_safe <- safely_execute(
      tune::tune_grid(meta_wflow,
                      resamples = folds,
                      grid      = grid,
                      metrics   = horizons_metric_set(),
                      control   = tune::control_grid(save_pred = FALSE)),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    tune_res <- handle_results(
      tune_safe,
      error_title = paste0("Meta-learner tuning failed for the ", method,
                           " ensemble.")
    )

    best        <- tune::select_best(tune_res, metric = rank_metric)
    final_wflow <- tune::finalize_workflow(meta_wflow, best)

  } else {

    final_wflow <- meta_wflow

  }

  fit_safe <- safely_execute(
    parsnip::fit(final_wflow, data = meta_frame),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  meta_fit <- handle_results(
    fit_safe,
    error_title = paste0("Meta-learner refit failed for the ", method,
                         " ensemble.")
  )

  ## Member weights (engine-specific extraction) ---------------------------

  weights <- extract_weights(meta_fit, oof$members)

  ## Combined out-of-fold predictions (Phase-2 UQ by-product). NOTE: this is
  ## the IN-SAMPLE meta combination (meta_fit predicting its own training
  ## frame), NOT a genuine meta-OOF. It is NOT valid for conformal calibration
  ## as-is — see FIT_REVIEW_FINDINGS I1 / the diagnostic task. Phase-2 UQ must
  ## replace this with held-out-fold meta predictions before calibrating.

  oof_combined <- stats::predict(meta_fit, new_data = meta_frame)$.pred

  oof_pred <- tibble::tibble(
    .row  = oof$row,
    .pred = floor_at_zero(oof_combined),
    truth = oof$truth
  )

  ## Members predict test_F; combine THROUGH the fitted meta-model ----------

  member_pred <- predict_members_on_test(object, members)

  ## Wide member-prediction matrix with the SAME column names the meta-model
  ## trained on; truth rides through the pivot (constant per sample).
  test_wide <- member_pred %>%
    dplyr::select("sample_id", "truth", "config_id", ".pred") %>%
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  combined <- stats::predict(
    meta_fit,
    new_data = test_wide[, member_cols, drop = FALSE]
  )$.pred

  ensemble_pred <- tibble::tibble(
    sample_id = test_wide$sample_id,
    .pred     = floor_at_zero(combined),
    truth     = test_wide$truth
  )

  build_ensemble_contract(
    method        = method,
    model         = meta_fit,
    weights       = weights,
    ensemble_pred = ensemble_pred,
    member_pred   = member_pred,
    rank_metric   = rank_metric,
    runtime_secs  = as.numeric(difftime(Sys.time(), started, units = "secs")),
    oof_pred      = oof_pred
  )

}

## ---------------------------------------------------------------------------
## build_ensemble_contract()
## ---------------------------------------------------------------------------

#' Assemble the Unified Ensemble Contract
#'
#' @description
#' Packs an engine's outputs into the canonical `ensemble` slot that every
#' meta-learner method populates identically. This is pure assembly plus
#' centralized scoring — it fits nothing. Engines fit their combiner and hand
#' back the fitted model, the member weights, and `test_F` predictions; all
#' performance scoring happens here so that metrics, member metrics, and the
#' improvement comparison are computed the same way for every method.
#'
#' Scoring reuses [compute_original_scale_metrics()] — the same six-metric
#' computer the single-model path uses — so an ensemble's metrics are directly
#' comparable to its members' metrics.
#'
#' @param method Character. The meta-learner method (`"penalized"`,
#'   `"weighted"`, `"xgb"`).
#' @param model The fitted meta-learner object (engine-specific).
#' @param weights Tibble with one row per member: `member` (config_id) and
#'   `coef` (the normalized contribution — stacks coefficient, inverse-RMSE
#'   weight, or xgb importance, depending on method).
#' @param ensemble_pred Tibble of ensemble `test_F` predictions: `sample_id`,
#'   `.pred`, `truth` (original scale).
#' @param member_pred Tibble of per-member `test_F` predictions: `config_id`,
#'   `sample_id`, `.pred`, `truth` (original scale). Used for `member_metrics`
#'   and the improvement baseline.
#' @param rank_metric Character. The metric the object was ranked by; the
#'   axis on which improvement is measured.
#' @param runtime_secs Numeric. Engine wall-clock, for the record.
#' @param oof_pred Tibble or NULL. The ensemble's combined out-of-fold
#'   predictions (`.row`, `.pred`, `truth`, original scale) — the by-product
#'   each engine already computes to derive its weights. Carried for Phase-2
#'   ensemble UQ (stacked conformal calibrates on these residuals); unused in
#'   Phase 1. Default NULL.
#'
#' @return A list matching the `ensemble` slot contract: `method`, `model`,
#'   `weights`, `predictions`, `metrics`, `member_metrics`, `improvement`,
#'   `oof_predictions`, `uq` (NULL in Phase 1), `timestamp`, `runtime_secs`.
#'
#' @keywords internal
build_ensemble_contract <- function(method,
                                    model,
                                    weights,
                                    ensemble_pred,
                                    member_pred,
                                    rank_metric,
                                    runtime_secs,
                                    oof_pred = NULL) {

  ## Ensemble performance on test_F (original scale, shared metric computer) --

  metrics <- compute_original_scale_metrics(
    ensemble_pred$truth,
    ensemble_pred$.pred
  )

  ## Per-member performance on the same test_F set --------------------------

  member_metrics <- dplyr::bind_rows(lapply(
    split(member_pred, member_pred$config_id),
    function(mp) {

      m <- compute_original_scale_metrics(mp$truth, mp$.pred)
      m$config_id <- mp$config_id[1]
      m

    }
  ))

  ## Improvement = ensemble vs best member on the rank metric, sign-aware ----
  ## (negative is a real outcome — xgb can degrade; we report it, not hide it.)

  ens_val <- metrics$.estimate[metrics$.metric == rank_metric]

  member_vals <- member_metrics$.estimate[member_metrics$.metric == rank_metric]

  best_member <- if (rank_metric %in% HIGHER_BETTER_METRICS) {
    max(member_vals, na.rm = TRUE)
  } else {
    min(member_vals, na.rm = TRUE)
  }

  improvement <- if (rank_metric %in% HIGHER_BETTER_METRICS) {
    ens_val - best_member
  } else {
    best_member - ens_val
  }

  list(
    method          = method,
    model           = model,
    weights         = weights,
    predictions     = ensemble_pred,
    metrics         = metrics,
    member_metrics  = member_metrics,
    improvement     = improvement,
    oof_predictions = oof_pred,
    uq              = NULL,
    timestamp       = Sys.time(),
    runtime_secs    = runtime_secs
  )

}

## ---------------------------------------------------------------------------
## predict.horizons_ensemble() — skeleton
## ---------------------------------------------------------------------------

#' Predict from a Fitted Ensemble (skeleton)
#'
#' @description
#' Predicts new spectra from a fitted ensemble. Each member predicts via the
#' same per-config primitive `predict.horizons_fit()` uses
#' ([predict_one_config()] — predict once, back-transform once), and the
#' member predictions are combined by the meta-learner stored in
#' `object$ensemble`. The combination differs by method: `penalized`/`weighted`
#' take a (weighted) linear combination of member predictions; `xgb` feeds the
#' member predictions through the boosted meta-model.
#'
#' This is a Phase-1 skeleton: the combination logic lands with the engines,
#' which populate `object$ensemble$model` and `$weights`. It establishes the
#' method signature and the contract reference, and errors clearly until an
#' ensemble has actually been built.
#'
#' @param object A `horizons_ensemble` object.
#' @param new_data Spectra to predict, on the training axis (same requirement
#'   as [predict.horizons_fit()]).
#' @param interval Logical. Return ensemble prediction intervals when Phase-2
#'   UQ is present (`object$ensemble$uq`). Default `TRUE`.
#' @param ... Unused; present for S3 method consistency.
#'
#' @return A tibble of ensemble predictions (skeleton: not yet implemented).
#'
#' @keywords internal
#' @exportS3Method stats::predict horizons_ensemble
predict.horizons_ensemble <- function(object,
                                      new_data,
                                      interval = TRUE,
                                      ...) {

  if (!inherits(object, "horizons_ensemble")) {

    cli::cli_abort(c(
      "{.arg object} must be a {.cls horizons_ensemble} object.",
      "i" = "Run {.fn ensemble} first to produce a fitted ensemble."
    ))

  }

  if (is.null(object$ensemble) || is.null(object$ensemble$model)) {

    cli::cli_abort("No fitted ensemble found on this object.")

  }

  cli::cli_abort(c(
    "{.fn predict.horizons_ensemble} is not implemented yet.",
    "i" = "The combination logic lands with the meta-learner engines."
  ))

}
