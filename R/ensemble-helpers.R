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
#' evaluation data `fit()` reserved), via `predict_one_config()` — predict once,
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

  ## The sample_id-keyed join below is load-bearing: a duplicate key would
  ## silently fan out each member's predictions and corrupt every downstream
  ## metric. average() guarantees uniqueness upstream; assert it here so a
  ## violation fails loudly at the join rather than inflating scores quietly.
  if (anyDuplicated(truth_df$sample_id)) {

    cli::cli_abort(c(
      "Test-set {.field sample_id}s are not unique.",
      "x" = "Duplicate keys would fan out the member-prediction join.",
      "i" = "Expected one row per sample after {.fn average}."
    ))

  }

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
#' @param seed Integer. Seed set immediately before the CV folds are drawn, so
#'   the tuning resamples and the genuine meta-OOF are reproducible.
#'
#' @return The ensemble contract list from [build_ensemble_contract()].
#'
#' @keywords internal
fit_tuned_meta_learner <- function(object,
                                   members,
                                   oof,
                                   rank_metric,
                                   optimize,
                                   method,
                                   spec,
                                   grid,
                                   extract_weights,
                                   seed = 307L) {

  started <- Sys.time()

  member_cols <- paste0("member_", oof$members)

  meta_frame        <- oof$predictors
  meta_frame$.truth <- oof$truth

  meta_wflow <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_formula(.truth ~ .)

  ## CV folds on the OOF matrix. Built once: the same resamples drive both
  ## hyperparameter tuning (when optimize) and the genuine meta-OOF below, so
  ## the held-out-fold meta predictions are produced on the identical splits.
  ## Stratify on the (original-scale) outcome so folds stay balanced on skewed
  ## soil properties; rsample gracefully reduces breaks (with a warning) when
  ## the meta_frame is too small to stratify finely.
  ## Seed set here (not via a side-effect in the caller) so the folds — which
  ## seed the Phase-2 conformal calibration — are reproducible regardless of
  ## any RNG use upstream of this point.

  set.seed(seed)

  folds <- rsample::vfold_cv(meta_frame, v = 5, strata = ".truth")

  ## Tune (CV on the OOF matrix only) then refit, or refit the fixed spec ----

  if (optimize) {

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

    ## Guard rank_metric before select_best(): if it is not one of the metrics
    ## actually collected during tuning, select_best() aborts with an opaque
    ## "no results with metric X" error. Fail with a clear, actionable message
    ## naming the available metrics instead.
    available_metrics <- unique(tune::collect_metrics(tune_res)$.metric)

    if (!rank_metric %in% available_metrics) {

      cli::cli_abort(c(
        "Cannot rank the {method} ensemble by {.val {rank_metric}}.",
        "x" = "{.val {rank_metric}} is not among the tuned metrics.",
        "i" = "Available: {.val {available_metrics}}."
      ))

    }

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

  ## Genuine meta out-of-fold predictions (the Phase-2 conformal-calibration
  ## seed). Each row is predicted by a meta-model that never saw it in training:
  ## fit_resamples refits final_wflow on every fold's analysis set and predicts
  ## its held-out assessment set, so collect_predictions returns one honest OOF
  ## prediction per training row. This replaces the earlier in-sample
  ## combination (meta_fit predicting its own training frame), which was 26x
  ## overconfident and invalid for conformal calibration (FIT_REVIEW_FINDINGS
  ## I1). collect_predictions' .row indexes into meta_frame (1..n); we map it
  ## back to the object's true .row ids and reorder to oof$row.
  ##
  ## CALIBRATION CAVEAT (why ensemble UQ does NOT use these residuals): the
  ## OOF predictions are out-of-fold w.r.t. the meta-model COEFFICIENTS, but in
  ## the optimize = TRUE path the hyperparameters were selected by tune_grid on
  ## these same folds, so the residuals carry hyperparameter-selection bias.
  ## Route B (implemented in ensemble-uq.R): fit_ensemble_uq() draws a FRESH
  ## fold partition at seed + 1000, refits the finalized workflow per fold with
  ## the fold models retained, and aggregates via genuine CV+ order statistics
  ## (cv_plus_bounds()) — never through compute_c_alpha(). These contract
  ## oof_predictions remain for diagnostics and the fold-honesty regression
  ## tests, not for calibration.

  oof_safe <- safely_execute(
    tune::fit_resamples(
      final_wflow,
      resamples = folds,
      metrics   = horizons_metric_set(),
      control   = tune::control_resamples(save_pred = TRUE)
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  oof_res <- handle_results(
    oof_safe,
    error_title = paste0("Meta out-of-fold prediction failed for the ", method,
                         " ensemble.")
  )

  oof_collected <- tune::collect_predictions(oof_res)

  ## Map fold-held-out predictions onto the object's true .row, in oof$row
  ## order. collect_predictions$.row is the meta_frame position; oof$row[pos]
  ## recovers the real id, and a position-keyed lookup reorders to oof$row.
  oof_by_pos <- oof_collected$.pred[order(oof_collected$.row)]

  oof_pred <- tibble::tibble(
    .row  = oof$row,
    .pred = floor_at_zero(oof_by_pos),
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
    oof_pred      = oof_pred,
    optimize      = optimize,
    seed          = seed
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
#'   each engine already computes to derive its weights. Carried for
#'   diagnostics and the fold-honesty regression tests; ensemble UQ calibrates
#'   on its own fresh-partition residuals (see [fit_ensemble_uq()]), not on
#'   these. Default NULL.
#' @param optimize Logical or NULL. The build-time optimize flag, recorded so
#'   ensemble UQ can re-derive fold weights (weighted method) by the same
#'   rule. Default NULL.
#' @param seed Integer or NULL. The build-time seed, recorded so ensemble UQ
#'   can derive a calibration partition disjoint from the tuning folds.
#'   Default NULL.
#'
#' @return A list matching the `ensemble` slot contract: `method`, `model`,
#'   `weights`, `predictions`, `metrics`, `member_metrics`, `improvement`,
#'   `oof_predictions`, `optimize`, `seed`, `uq` (populated by
#'   [fit_ensemble_uq()]), `timestamp`, `runtime_secs`.
#'
#' @details
#' **The `model` slot is typed per method:**
#'
#' | method      | `$model` contains                                          |
#' |-------------|------------------------------------------------------------|
#' | `weighted`  | the weights tibble (`member`, `coef`) — same object as `$weights` |
#' | `penalized` | the trained meta `workflow` (glmnet)                       |
#' | `xgb`       | the trained meta `workflow` (xgboost)                      |
#'
#' Both metamodel methods store the *workflow*, not an extracted engine —
#' `combine_ensemble_metamodel()` and ensemble UQ's fold refits
#' (`workflows::extract_spec_parsnip()`) depend on this.
#'
#' @keywords internal
build_ensemble_contract <- function(method,
                                    model,
                                    weights,
                                    ensemble_pred,
                                    member_pred,
                                    rank_metric,
                                    runtime_secs,
                                    oof_pred = NULL,
                                    optimize = NULL,
                                    seed     = NULL) {

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
    optimize        = optimize,
    seed            = seed,
    uq              = NULL,
    timestamp       = Sys.time(),
    runtime_secs    = runtime_secs
  )

}

## ---------------------------------------------------------------------------
## predict.horizons_ensemble()
## ---------------------------------------------------------------------------

#' Predict Soil Properties from a Fitted Ensemble
#'
#' @description
#' Generates stacked-ensemble predictions for new spectra. Every member predicts
#' via the same per-config primitive [predict.horizons_fit()] uses
#' (`predict_one_config()` — predict once, back-transform once), and the member
#' predictions are combined by the meta-learner stored in `object$ensemble`. The
#' combination differs by method: `weighted` takes a fixed weighted average of
#' member predictions; `penalized` and `xgb` feed the member predictions through
#' the fitted meta-model.
#'
#' @param object A `horizons_ensemble` object (the output of [ensemble()]).
#' @param new_data Spectra to predict. Either a `horizons_data` object or a
#'   tibble/data.frame whose predictor (wavelength) columns match the training
#'   schema. **The spectra must already be on the training axis** — the same
#'   requirement as [predict.horizons_fit()]; see its Details for the axis-
#'   alignment limitation.
#' @param interval Logical. Return CV+ conformal prediction intervals when
#'   ensemble uncertainty quantification is available (`object$ensemble$uq`,
#'   calibrated by [fit_ensemble_uq()] — on by default in [ensemble()]).
#'   Default `TRUE`. When no UQ bundle is present, point predictions are
#'   returned with a one-time note.
#' @param ... Unused; present for S3 method consistency.
#'
#' @return A tibble, one row per sample:
#'   \describe{
#'     \item{sample_id}{Sample identifier from `new_data`.}
#'     \item{.pred}{Ensemble point prediction, original response scale.}
#'   }
#'   Interval columns (`.pred_lower`, `.pred_upper`, `.interval_width`) are
#'   joined on by `sample_id` when ensemble UQ is present and
#'   `interval = TRUE`.
#'
#' @details
#' All ensemble members must predict successfully. A member that fails to
#' predict on `new_data` aborts the call naming the offending config — the
#' meta-learner's combination is only valid over the exact member set it was
#' trained on, so dropping a member would silently change what is being
#' predicted.
#'
#' **Interval semantics.** Bounds are genuine CV+ aggregates (Barber et al.
#' 2021) over the retained fold models and signed calibration residuals — not
#' `point +/- margin`. The deployed point prediction can therefore
#' occasionally fall outside its own interval (a known property of
#' jackknife+/CV+; the bounds come from fold-model predictions, not the
#' full-data refit). This is deliberate and not repaired.
#'
#' @examples
#' \dontrun{
#' ens <- ensemble(fit(evaluated, n_best = 5))
#' predict(ens, new_spectra)
#' }
#'
#' @exportS3Method stats::predict horizons_ensemble
predict.horizons_ensemble <- function(object,
                                      new_data,
                                      interval = TRUE,
                                      ...) {

  ## -------------------------------------------------------------------------
  ## Step 0: Preflight
  ## -------------------------------------------------------------------------

  if (!inherits(object, "horizons_ensemble")) {

    cli::cli_abort(c(
      "{.arg object} must be a {.cls horizons_ensemble} object.",
      "i" = "Run {.fn ensemble} first to produce a fitted ensemble."
    ))

  }

  if (is.null(object$ensemble) || is.null(object$ensemble$model)) {

    cli::cli_abort("No fitted ensemble found on this object.")

  }

  ## -------------------------------------------------------------------------
  ## Step 1: Resolve new_data, then validate it carries the training axis
  ## -------------------------------------------------------------------------

  new_spectra <- resolve_new_data(new_data)

  check_predictor_schema(object, new_spectra)

  ## -------------------------------------------------------------------------
  ## Step 2: Resolve the authoritative member set
  ## -------------------------------------------------------------------------

  ## The members the meta-learner actually trained on are exactly the rows of
  ## the weights tibble. Read them directly rather than re-deriving via
  ## gather_members(), which intersects train-time slots (cv_predictions) and
  ## could disagree with what the fitted meta-model saw.
  members <- object$ensemble$weights$member

  if (is.null(members) || length(members) < 2) {

    cli::cli_abort(c(
      "The fitted ensemble carries no member set.",
      "i" = "{.fn ensemble} should record at least two members in its weights."
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 3: Every member predicts new_data (long frame, original scale)
  ## -------------------------------------------------------------------------

  member_pred <- predict_members(object, members, new_spectra)

  ## -------------------------------------------------------------------------
  ## Step 4: Combine member predictions by the meta-learner (per method)
  ## -------------------------------------------------------------------------

  method <- object$ensemble$method

  point <- switch(
    method,

    weighted = combine_ensemble_weighted(member_pred, object$ensemble$weights),

    penalized = ,
    xgb       = combine_ensemble_metamodel(member_pred, members,
                                           object$ensemble$model),

    cli::cli_abort(c(
      "Unknown ensemble method {.val {method}}.",
      "i" = "Expected one of {.val {c('weighted', 'penalized', 'xgb')}}."
    ))
  )

  ## -------------------------------------------------------------------------
  ## Step 5: Intervals (only when ensemble UQ is available) + assemble output
  ## -------------------------------------------------------------------------

  ## Mirror the single-model seam (predict_one_config): point predictions are
  ## complete here; intervals are a pure append when a UQ bundle exists. The
  ## CV+ assembler reuses the Step-3 member predictions (members are never
  ## re-predicted) and returns sample_id-keyed interval columns — joined, not
  ## positionally bound, so row-order drift cannot silently misalign them.
  uq <- object$ensemble$uq

  if (!isTRUE(interval) || is.null(uq)) {

    if (isTRUE(interval) && is.null(uq)) {

      cli::cli_inform(c(
        "i" = "Ensemble prediction intervals are not available; \\
               returning point predictions."
      ))

    }

    return(point)

  }

  interval_cols <- predict_ensemble_intervals(
    uq          = uq,
    member_pred = member_pred
  )

  if (is.null(interval_cols)) {

    return(point)

  }

  dplyr::left_join(point, interval_cols, by = "sample_id")

}

## ---------------------------------------------------------------------------
## combine_ensemble_weighted() — weighted-average combine (silent helper)
## ---------------------------------------------------------------------------

#' Combine member predictions by the ensemble weights
#'
#' The predict-time generalization of the `weighted` engine's train-time
#' combine (`fit_ensemble_weighted()`): join the per-member weights, then take
#' the weighted sum per sample. Floored at zero — soil properties are
#' non-negative.
#'
#' @param member_pred Long tibble from [predict_members()] (`config_id`,
#'   `sample_id`, `.pred`).
#' @param weights The ensemble weights tibble (`member`, `coef`).
#' @return A tibble: `sample_id`, `.pred` (ensemble point prediction).
#' @noRd
combine_ensemble_weighted <- function(member_pred, weights) {

  joined <- member_pred %>%
    dplyr::left_join(weights, by = c("config_id" = "member"))

  ## A member with predictions but no matching weight leaves `coef` NA, which
  ## would propagate through sum() to an NA ensemble prediction returned as a
  ## valid tibble. predict_members() guarantees the member set matches, so this
  ## is unreachable on the normal path — but the join is the one spot a future
  ## member-set drift could corrupt silently, so fail loud here. This mirrors the
  ## explicit member-presence gate in combine_ensemble_metamodel().
  if (anyNA(joined$coef)) {

    unmatched <- unique(joined$config_id[is.na(joined$coef)])

    cli::cli_abort(c(
      "Weighted combine has no weight for {length(unmatched)} member{?s}.",
      "x" = "Unweighted: {.val {unmatched}}",
      "i" = "The member set must match the ensemble weights."
    ))

  }

  out <- joined %>%
    dplyr::group_by(.data$sample_id) %>%
    dplyr::summarise(.pred = sum(.data$.pred * .data$coef),
                     .groups = "drop")

  out$.pred <- floor_at_zero(out$.pred)

  out

}

## ---------------------------------------------------------------------------
## combine_ensemble_metamodel() — meta-model combine (silent helper)
## ---------------------------------------------------------------------------

#' Combine member predictions through a fitted meta-model
#'
#' The predict-time generalization of the `penalized`/`xgb` engines' train-time
#' combine: widen the member predictions into the `member_<config_id>` matrix
#' the meta-model trained on, then run them THROUGH the fitted meta-workflow.
#' Floored at zero.
#'
#' The wide-frame columns are selected explicitly from the trained-on member
#' set (not from whatever the pivot happens to produce), and a missing member
#' column aborts — the meta-model's combination is only valid over the exact
#' member set it saw, so an incomplete frame must fail loudly rather than
#' silently mispredict.
#'
#' @param member_pred Long tibble from [predict_members()].
#' @param members Character vector of the trained-on member `config_id`s.
#' @param model The fitted meta-workflow (`object$ensemble$model`).
#' @return A tibble: `sample_id`, `.pred` (ensemble point prediction).
#' @noRd
combine_ensemble_metamodel <- function(member_pred, members, model) {

  member_cols <- paste0("member_", members)

  wide <- member_pred %>%
    dplyr::select("sample_id", "config_id", ".pred") %>%
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  missing <- setdiff(member_cols, names(wide))

  if (length(missing) > 0) {

    cli::cli_abort(c(
      "Member prediction{?s} missing for {length(missing)} ensemble member{?s}.",
      "x" = "Absent: {.val {sub('^member_', '', missing)}}",
      "i" = "Every ensemble member must predict {.arg new_data}."
    ))

  }

  combined <- stats::predict(
    model,
    new_data = wide[, member_cols, drop = FALSE]
  )$.pred

  tibble::tibble(
    sample_id = wide$sample_id,
    .pred     = floor_at_zero(combined)
  )

}
