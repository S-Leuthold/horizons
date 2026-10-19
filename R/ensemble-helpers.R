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

  ### ensemble() checks the class at its door, and its fit validator again.

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

  ### ensemble() refuses fewer than two at its door.

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

  ## Every member covers the same .row set, with no row twice: fit() fails a
  ## member that missed a fold (#101), and the door validator refuses a fit
  ## whose cv_predictions do not line up (validate_horizons_fit()).

  ## Wide reshape: one column per member, rows keyed by .row. Sort by .row so
  ## the predictor matrix, truth, and row index are all in the same order.
  ## The validator's refusal of a duplicate .row for a member is what keeps
  ## pivot_wider() from building list-columns here; inside ensemble() this
  ## runs as the meta-learner unit's first step (oof_matrix).

  ## Carry truth through the pivot — it is constant per .row, so it rides as an
  ## id column and comes out aligned, avoiding a separate dedup-and-reorder.

  wide <- tidyr::pivot_wider(
    cv[, c(".row", "truth", "config_id", ".pred")],
    names_from   = "config_id",
    values_from  = ".pred",
    names_prefix = "member_"
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
## meta_tune_control()
## ---------------------------------------------------------------------------

#' tune control objects for the meta-learner, with allow_par explicit
#'
#' @description
#' tune's control constructors default to `allow_par = TRUE`, so before
#' 2026-09-15 the meta-learner tuning and OOF pass dispatched onto any
#' registered `future::plan()` with no argument to say otherwise. Both calls
#' now build their control here, from the `allow_par` field of `ensemble()`'s
#' control, and the result is what the tests assert on.
#'
#' @param kind `"grid"` (for `tune_grid()`) or `"resamples"` (for
#'   `fit_resamples()`).
#' @param allow_par Logical, from `ensemble()`'s control.
#' @return A `control_grid` or `control_resamples` object.
#' @keywords internal
#' @noRd
meta_tune_control <- function(kind = c("grid", "resamples"), allow_par = FALSE) {

  kind <- match.arg(kind)

  if (kind == "grid") {

    tune::control_grid(save_pred = FALSE, allow_par = allow_par,
                       parallel_over = "resamples")

  } else {

    tune::control_resamples(save_pred = TRUE, allow_par = allow_par,
                            parallel_over = "resamples")

  }

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
#' evaluation data `fit()` reserved), via `predict_members()` — predict once,
#' back-transform once, one unit per member. Shared by all engines so the
#' meaning of "test_F" and the per-member prediction contract live in one
#' place. A member that cannot predict the test rows aborts with
#' `horizons_error_prediction_failed`, reporting `call`; inside `ensemble()`
#' the meta-learner's unit contains it. What the members' units recorded
#' (their warnings, the rows they could not use) rides on the result as the
#' attributes `"unit_records"` and `"flagged"`, for `ensemble()` to report.
#'
#' @param object A `horizons_fit` object.
#' @param members Character vector of member `config_id`s.
#' @param call The frame a member's failure to predict the test rows reports
#'   (`ensemble()`'s).
#'
#' @return A long tibble: `config_id`, `sample_id`, `.pred`, `truth` (original
#'   scale), one block per member, with the attributes `"unit_records"` and
#'   `"flagged"`.
#'
#' @keywords internal
predict_members_on_test <- function(object, members, call = rlang::caller_env()) {

  ## Load the namespaces the members' predict methods live in, as
  ## predict.horizons_ensemble() does. An installed package loads its Imports
  ## lazily, so on a freshly read fitted object the workflows S3 methods are
  ## not registered until something loads them, and predict() on a member
  ## finds no method.
  ensure_predict_namespaces(object, members)

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
  ## fan out each member's predictions. The fit validator at ensemble()'s door
  ## refuses a split whose rows do not carry unique sample_ids.
  ##
  ## The members predict through the same helper predict() uses, unclamped:
  ## these predictions feed the ensemble's own test_F scoring (metrics,
  ## member_metrics, improvement) and the meta-combine, so ranking and
  ## comparison must see raw model behavior — a blow-up member must score
  ## terribly, not be laundered by the deploy guardrail. A member that cannot
  ## predict the test rows stops the build with the same error.
  member_pred <- predict_members(
    object, members, test_data,
    header = "The ensemble could not be built: {n_failed} of its {n_total} member{?s} could not predict the test rows.",
    call   = call
  )

  ## What the members' units recorded travels as data to ensemble(), which
  ## reports it with the meta-learner's
  out <- dplyr::left_join(drop_block_reports(member_pred), truth_df, by = "sample_id")

  attr(out, "unit_records") <- attr(member_pred, "unit_records")
  attr(out, "flagged")      <- attr(member_pred, "flagged")

  out

}

#' Carry what a meta-learner's run recorded on its contract
#'
#' @description
#' The engines run inside `ensemble()`'s meta-learner unit and return the
#' contract. What the run recorded that the unit cannot see travels on it as
#' attributes, which `ensemble()` reads and drops: the members' unit records
#' and the test rows they could not use (from [predict_members_on_test()]),
#' and the warning records tune kept as notes.
#'
#' @param contract From [build_ensemble_contract()].
#' @param member_pred From [predict_members_on_test()].
#' @param warning_log Records shaped like [new_warning_log()]'s.
#' @return `contract`, with the attributes `"unit_records"`, `"flagged"` and
#'   `"warning_log"`.
#' @keywords internal
#' @noRd
with_run_records <- function(contract, member_pred, warning_log = new_warning_log()) {

  attr(contract, "unit_records") <- attr(member_pred, "unit_records")
  attr(contract, "flagged")      <- attr(member_pred, "flagged")
  attr(contract, "warning_log")  <- warning_log

  contract

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
#' @param allow_par Logical. From `ensemble()`'s control; both tune calls
#'   build their control through `meta_tune_control()` with it, so the
#'   meta-learner never dispatches onto a registered plan unless asked.
#'   Default `FALSE`.
#' @param seed Integer. Seed set immediately before the CV folds are drawn, so
#'   the tuning resamples and the genuine meta-OOF are reproducible.
#' @param debug Logical. From `ensemble()`'s control: a failure inside tune,
#'   or a candidate left out for a missing fold, stops the call.
#' @param call The frame a member's failure to predict the test rows reports
#'   (`ensemble()`'s).
#'
#' @return The ensemble contract list from [build_ensemble_contract()], with
#'   what the run recorded as attributes (`with_run_records()`). Runs inside
#'   `ensemble()`'s meta-learner unit, each step labelled: every candidate
#'   failing, no candidate scored on every fold, or an out-of-fold pass that
#'   misses a fold is a run failure of the unit. Aborts with class
#'   `horizons_error_internal` when the meta-model returns a different number
#'   of out-of-fold predictions than there are OOF rows, or of `test_F`
#'   predictions than there are test samples.
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
                                   seed      = DEFAULT_ENSEMBLE_SEED,
                                   allow_par = FALSE,
                                   debug     = FALSE,
                                   call      = rlang::caller_env()) {

  ## `allow_par` (from `ensemble()`'s control) reaches both tune calls via
  ## meta_tune_control(); documented on the roxygen block above.

  started <- Sys.time()

  member_cols <- paste0("member_", oof$members)

  meta_frame        <- oof$predictors
  meta_frame$.truth <- oof$truth

  meta_wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(.truth ~ .)

  ## The warnings tune keeps as notes: the chosen candidate's (#109) and the
  ## out-of-fold pass's. They are the ensemble's own, for its `warnings`.
  warning_log <- new_warning_log()

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

  folds   <- rsample::vfold_cv(meta_frame, v = 5, strata = ".truth")
  n_folds <- nrow(folds)

  ## Tune (CV on the OOF matrix only) then refit, or refit the fixed spec ----
  ## As evaluate() tunes: tune's console notes muffled and read from the
  ## result instead; every candidate failing fails the meta-learner; and only
  ## a candidate scored on every fold takes part in the choice (#101).

  if (optimize) {

    tune_res <- unit_step("tune_grid", {
      results <- suppressMessages(tune::tune_grid(
        meta_wflow,
        resamples = folds,
        grid      = grid,
        metrics   = horizons_metric_set(),
        control   = meta_tune_control("grid", allow_par)
      ))
      check_tune_result(results, "every meta-learner candidate failed", debug = debug)
      results
    })

    ## rank_metric is one of VALID_RANK_METRICS, which the fit validator at
    ## ensemble()'s door checks.

    best <- unit_step("select_best", select_complete(tune_res, rank_metric, n_folds, debug))

    if (nrow(best) == 0) {

      unit_step("select_best", abort_tune_failure(
        tune_res, "no meta-learner candidate was scored on every fold"
      ))

    }

    left_out <- incomplete_note(tune_res, n_folds)

    if (!is.null(left_out)) note_in_unit("The meta-learner's tuning: {left_out}")

    candidate   <- candidate_of(tune_res, best, grid, meta_wflow)
    warning_log <- dplyr::bind_rows(
      warning_log,
      tune_note_records(tune_res, type = "warning", scope = "cv", candidate = candidate)
    )

    final_wflow <- unit_step("finalize_workflow", tune::finalize_workflow(meta_wflow, best))

  } else {

    final_wflow <- meta_wflow

  }

  meta_fit <- unit_step("final_fit", parsnip::fit(final_wflow, data = meta_frame))

  ## Member weights (engine-specific extraction) ---------------------------

  weights <- unit_step("member_weights", extract_weights(meta_fit, oof$members))

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

  oof_by_pos <- unit_step("oof_predictions", {

    oof_res <- suppressMessages(tune::fit_resamples(
      final_wflow,
      resamples = folds,
      metrics   = horizons_metric_set(),
      control   = meta_tune_control("resamples", allow_par)
    ))

    check_tune_result(oof_res, "the meta-learner's out-of-fold pass failed", debug = debug)

    warning_log <- dplyr::bind_rows(
      warning_log, tune_note_records(oof_res, type = "warning", scope = "cv")
    )

    oof_collected <- tune::collect_predictions(oof_res)

    ## A fold that failed leaves its rows unpredicted: the meta-learner is
    ## then not scored on every row, which fails it, as a member whose
    ## out-of-fold predictions miss a fold fails (#101)
    if (nrow(oof_collected) < length(oof$row)) {

      abort_tune_failure(oof_res, cli::format_inline(
        "the meta-learner's out-of-fold pass predicted {nrow(oof_collected)} of the {length(oof$row)} rows"
      ))

    }

    ## Map fold-held-out predictions onto the object's true .row, in oof$row
    ## order. collect_predictions$.row is the meta_frame position; oof$row[pos]
    ## recovers the real id, and a position-keyed lookup reorders to oof$row.
    oof_collected$.pred[order(oof_collected$.row)]

  })

  ## Bound to oof$row and oof$truth by position below. A fold that failed to
  ## predict is refused above, so a short vector here is a bug, and a single
  ## value would be recycled across every row.
  check_rows_aligned(
    what       = "Meta-model out-of-fold predictions",
    to         = "the out-of-fold rows",
    n          = length(oof_by_pos),
    n_expected = length(oof$row)
  )

  ## Combined predictions are clamped to the outcome's range, as every served
  ## prediction is (#76); a floor at 0 under the default range.
  outcome_range <- outcome_range_setting(object)

  oof_pred <- tibble::tibble(
    .row  = oof$row,
    .pred = clamp_to_outcome_range(oof_by_pos, outcome_range),
    truth = oof$truth
  )

  ## Members predict test_F; combine THROUGH the fitted meta-model ----------

  member_pred <- unit_step("test_predict", predict_members_on_test(object, members, call = call))

  ## Wide member-prediction matrix with the SAME column names the meta-model
  ## trained on; truth rides through the pivot (constant per sample).
  test_wide <- member_pred |>
    dplyr::select("sample_id", "truth", "config_id", ".pred") |>
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  combined <- unit_step("combine", stats::predict(
    meta_fit,
    new_data = test_wide[, member_cols, drop = FALSE]
  )$.pred)

  ## Attached to test_wide$sample_id and truth by position, as at predict
  ## time in combine_ensemble_metamodel(): a short prediction would be
  ## recycled and the test metrics scored on the wrong samples.
  check_rows_aligned(
    what       = "Meta-model predictions",
    to         = "the test samples the members predicted",
    n          = length(combined),
    n_expected = nrow(test_wide)
  )

  ensemble_pred <- tibble::tibble(
    sample_id = test_wide$sample_id,
    .pred     = clamp_to_outcome_range(combined, outcome_range),
    truth     = test_wide$truth
  )

  contract <- unit_step("test_metrics", build_ensemble_contract(
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
  ))

  with_run_records(contract, member_pred, warning_log)

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
#' @param optimize Logical. The build-time optimize flag, recorded so
#'   ensemble UQ can re-derive fold weights (weighted method) by the same
#'   rule.
#' @param seed Integer. The build-time seed, recorded so ensemble UQ can
#'   derive a calibration partition disjoint from the tuning folds.
#'
#' @return A list matching the `ensemble` slot contract: `method`, `model`,
#'   `weights`, `predictions`, `metrics`, `member_metrics`, `improvement`,
#'   `oof_predictions`, `optimize`, `seed`, `uq` (populated by
#'   [fit_ensemble_uq()]), `control` and `warnings` (filled by [ensemble()]),
#'   `timestamp`, `runtime_secs`.
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
                                    optimize,
                                    seed) {

  ## Ensemble performance on test_F (original scale, shared metric computer) --

  ## The pair count each set was scored on is not stored with the metrics.
  metrics   <- compute_original_scale_metrics(
    ensemble_pred$truth,
    ensemble_pred$.pred
  )
  metrics$n <- NULL

  ## Per-member performance on the same test_F set --------------------------

  member_metrics <- dplyr::bind_rows(lapply(
    split(member_pred, member_pred$config_id),
    function(mp) {

      m <- compute_original_scale_metrics(mp$truth, mp$.pred)
      m$n <- NULL
      m$config_id <- mp$config_id[1]
      m

    }
  ))

  ## Improvement = ensemble vs best member on the rank metric, sign-aware ----
  ## (negative is a real outcome — xgb can degrade; we report it, not hide it.)
  ## A metric undefined on the test rows is NA, for the ensemble or a member;
  ## the improvement is NA when the ensemble's is, or no member has one.

  ens_val <- metrics$.estimate[metrics$.metric == rank_metric]

  member_vals <- member_metrics$.estimate[member_metrics$.metric == rank_metric]
  member_vals <- member_vals[!is.na(member_vals)]

  improvement <- if (is.na(ens_val) || length(member_vals) == 0) {

    NA_real_

  } else if (rank_metric %in% HIGHER_BETTER_METRICS) {

    ens_val - max(member_vals)

  } else {

    min(member_vals) - ens_val

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
    control         = NULL,
    warnings        = NULL,
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
#' @param interval CV+ conformal prediction intervals, from the ensemble's own
#'   uncertainty bundle (`object$ensemble$uq`, calibrated by
#'   [fit_ensemble_uq()] — on by default in [ensemble()]). `NULL` (the
#'   default): intervals when the bundle exists, and none, silently, when it
#'   does not. `TRUE`: intervals asked for; without a bundle the bounds are
#'   `NA`, with one warning of class `horizons_warning_skipped`. `FALSE`:
#'   none. The intervals are at the level they were calibrated at in
#'   [ensemble()], 0.90 by default (see Details). When the
#'   members were fit on a response-trimmed training partition
#'   (`validate(remove_outliers =
#'   "response")`, #77), the intervals come from out-of-fold predictions on
#'   the rows inside the training fences, so this warns once, with class
#'   `horizons_warning_quality`, that they were calibrated within those
#'   fences and can undercover samples outside them.
#' @param ... Not used; present for S3 method consistency. A `level` passed
#'   here is ignored with a warning (see Details), and any other argument is
#'   an error.
#'
#' @return A tibble, one row per row of `new_data`, in its order (zero rows
#'   give zero rows with the same columns):
#'   \describe{
#'     \item{sample_id}{Sample identifier from `new_data`.}
#'     \item{.pred}{Ensemble point prediction, original response scale.}
#'   }
#'   Interval columns (`.pred_lower`, `.pred_upper`, `.interval_width`) are
#'   put in place by `sample_id` when `interval = TRUE`, or `NULL` and the
#'   ensemble has a CV+ bundle.
#'
#' @details
#' All ensemble members must predict successfully. A member that fails to
#' predict on `new_data`, or a combination that fails, aborts the call with
#' `horizons_error_prediction_failed`, the failures in its `results` field —
#' the meta-learner's combination is only valid over the exact member set it
#' was trained on, so dropping a member would silently change what is being
#' predicted. A row any member could not use (flagged on arrival, or
#' non-finite after its recipe) is returned as `NA` and named in one
#' `horizons_warning_flagged`. CV+ intervals that cannot be computed leave
#' their columns `NA`, with one `horizons_warning_skipped`. The predictions
#' come back one row per row of `new_data`, in its order.
#'
#' **Interval semantics.** Bounds are genuine CV+ aggregates (Barber et al.
#' 2021) over the retained fold models and signed calibration residuals — not
#' `point +/- margin`. The deployed point prediction can therefore
#' occasionally fall outside its own interval (a known property of
#' jackknife+/CV+; the bounds come from fold-model predictions, not the
#' full-data refit). This is deliberate and not repaired.
#'
#' **Interval level.** Intervals are at the coverage level the CV+ bundle was
#' calibrated at in [ensemble()], 0.90 by default. `predict()` has no `level`
#' argument: a `level` is ignored with a warning of class
#' `horizons_warning_input`, and any other argument it does not take is an
#' error of class `horizons_error_input`.
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
                                      interval = NULL,
                                      ...) {

  ## -------------------------------------------------------------------------
  ## Step 0: Preflight
  ## -------------------------------------------------------------------------

  ## The object as it arrives (#234). S3 dispatch guarantees the class; the
  ## validator's gate refuses a direct ::: call, an ensemble slot with no
  ## model, a member set of fewer than two, and an unknown method.
  object <- validate_horizons_ensemble(object)

  ## What the call reports is collected here and raised once per topic at
  ## its end (finish_predict()). predict() writes nothing to the object.
  collector <- log_open("predict")

  check_arg(rlang::check_bool, interval, allow_null = TRUE)

  ## Nothing in `...` is used. A `level` warns, naming the level the CV+
  ## bundle was calibrated at; anything else aborts (#141).
  check_predict_dots(..., .level_default = object$ensemble$uq$level_default,
                     .collector = collector)

  ## -------------------------------------------------------------------------
  ## Step 1: Resolve the authoritative member set
  ## -------------------------------------------------------------------------

  ## The members the meta-learner actually trained on are exactly the rows of
  ## the weights tibble. Read them directly rather than re-deriving via
  ## gather_members(), which intersects train-time slots (cv_predictions) and
  ## could disagree with what the fitted meta-model saw.
  ##
  ## Every step below reads this member set, starting with
  ## ensure_predict_namespaces(), which needs it to know which engines are
  ## actually in play; the door validator requires at least two members.
  members <- object$ensemble$weights$member

  ## `library(horizons)` does not load workflows, most modeling engines, or
  ## ranger (#65); see ensure_predict_namespaces() (R/pipeline-predict.R) for
  ## why. Scoped to the member set, not every config the underlying fit
  ## stored — gather_members() can drop a stored config that lacks
  ## out-of-fold predictions, so "stored" and "member" are not always the
  ## same set.
  ensure_predict_namespaces(object, members)

  ## -------------------------------------------------------------------------
  ## Step 2: Resolve new_data, then validate it carries the training axis
  ## -------------------------------------------------------------------------

  new_spectra <- resolve_new_data(new_data)

  check_predictor_schema(object, new_spectra)

  ## sample_id keys every combine below (the weighted average groups on it,
  ## the wide pivot keys rows on it, the intervals join on it), so a repeated
  ## id would fan out each member's predictions.
  if (anyDuplicated(new_spectra$sample_id)) {

    dupes <- unique(new_spectra$sample_id[duplicated(new_spectra$sample_id)])

    abort_horizons(c(
      "{.arg new_data}'s {.field sample_id}s must be unique to predict from an ensemble.",
      "x" = "{length(dupes)} repeat{?s}: {.val {utils::head(dupes, 5)}}.",
      "i" = "Give each row its own {.field sample_id}, or {.fn average} replicate scans first."
    ), kind = "input")

  }

  ## -------------------------------------------------------------------------
  ## Step 3: Every member predicts new_data (long frame, original scale)
  ## -------------------------------------------------------------------------
  ## Each member is one unit, as each configuration is in predict() on a fit;
  ## one that cannot predict stops the call (predict_members()).

  member_pred <- predict_members(object, members, new_spectra)
  records     <- attr(member_pred, "unit_records")
  flagged     <- attr(member_pred, "flagged")
  member_pred <- drop_block_reports(member_pred)

  ## A row any member could not use (flagged on arrival, or non-finite after
  ## its recipe) is not predicted by the ensemble: an xgb meta-model would
  ## read the member's NA as missing and return a number.
  bad    <- unique(unlist(flagged))
  usable <- member_pred[!member_pred$sample_id %in% bad, , drop = FALSE]

  ## -------------------------------------------------------------------------
  ## Step 4: Combine member predictions by the meta-learner (per method)
  ## -------------------------------------------------------------------------
  ## One row per row of new_data, in its order; NA where nothing could be
  ## predicted. The interval columns are there whenever intervals were asked
  ## for and the ensemble has a CV+ bundle.

  method        <- object$ensemble$method
  outcome_range <- outcome_range_setting(object)
  uq            <- object$ensemble$uq
  intervals     <- !isFALSE(interval) && !is.null(uq)
  winsorized    <- NULL

  out <- tibble::tibble(sample_id = new_spectra$sample_id,
                        .pred     = rep(NA_real_, nrow(new_spectra)))

  if (intervals || isTRUE(interval)) {

    out$.pred_lower     <- rep(NA_real_, nrow(out))
    out$.pred_upper     <- rep(NA_real_, nrow(out))
    out$.interval_width <- rep(NA_real_, nrow(out))

  }

  ## Intervals asked for, and no CV+ bundle to build them from (#145)
  if (isTRUE(interval) && is.null(uq)) log_missing_intervals(collector, ensemble = TRUE)

  if (nrow(usable) > 0) {

    ## The combination is one unit: when it fails there is nothing to return
    combined <- run_unit(unit_step("combine", switch(
      method,

      weighted = combine_ensemble_weighted(usable, object$ensemble$weights,
                                           outcome_range),

      penalized = ,
      xgb       = combine_ensemble_metamodel(usable, members,
                                             object$ensemble$model,
                                             outcome_range)

      ### No default: the door validator refuses any other method.
    )), unit = "combination", id = method)

    records <- dplyr::bind_rows(records, combined$records)

    if (combined$failed) {

      abort_prediction_failed(records, "The ensemble cannot predict: its combination failed.")

    }

    point <- combined$value

    ## Deploy-time response-bound guardrail, applied ONCE at the combined
    ## output. Members predict unclamped above (their predictions are
    ## meta-learner features and must match the raw member OOF the
    ## meta-learner trained and the UQ fold models calibrated on); the
    ## physically-impossible-blow-up protection lands here, on the value a
    ## user actually receives. The 257 g/kg escape this guards against was an
    ## ensemble output. Interval bounds are deliberately not clamped
    ## (uncertainty must not be truncated), so a clamped point can sit below
    ## an unclamped upper bound — coherent, and documented.
    bound      <- object$models$response_bound
    bounded    <- apply_response_bound(point$.pred, bound)
    winsorized <- winsorized_report(point$sample_id, point$.pred, bounded, bound)

    out$.pred[match(point$sample_id, out$sample_id)] <- bounded$values

    ## -----------------------------------------------------------------------
    ## Step 5: CV+ intervals, a unit of their own
    ## -----------------------------------------------------------------------
    ## The CV+ assembler reuses the Step-3 member predictions (members are
    ## never re-predicted) and returns sample_id-keyed interval columns, put
    ## in place by key, not by position. A failure leaves the bounds NA.

    if (intervals) {

      cv_plus <- run_unit(predict_ensemble_intervals(uq, usable, outcome_range),
                          unit = "intervals", id = "ensemble")

      records <- dplyr::bind_rows(records,
                                  cv_plus$records[cv_plus$records$type != "failure", , drop = FALSE])

      if (cv_plus$failed) {

        cause <- unit_failure_message(cv_plus$records, outside = NULL)

        log_warning(collector, "skipped",
                    "Prediction intervals (CV+) were not computed: {cause}")

      } else {

        bounds <- cv_plus$value
        at     <- match(bounds$sample_id, out$sample_id)

        out$.pred_lower[at]     <- bounds$.pred_lower
        out$.pred_upper[at]     <- bounds$.pred_upper
        out$.interval_width[at] <- bounds$.interval_width

        log_missing_bounds(collector, bounds)

      }

    }

  }

  ## -------------------------------------------------------------------------
  ## Step 6: Report, once per topic
  ## -------------------------------------------------------------------------

  log_prediction_reports(collector, records, flagged, list(winsorized))

  ## Conformal coverage on a selected training set, and CV+ intervals
  ## calibrated within the training fences, said only when intervals came
  ## back (#145)
  log_interval_quality(collector, object, intervals_returned(out))

  finish_predict(collector, out)

}

#' Collect a warning for the rows whose CV+ bounds are NA
#'
#' A row the members could predict but whose CV+ augmented set held a missing
#' value gets `NA` bounds (#145). Named in one `skipped` warning, beside the
#' rows that are `NA` because they were flagged.
#'
#' @param collector The call's collector.
#' @param bounds The CV+ interval columns, with `sample_id`.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_missing_bounds <- function(collector, bounds) {

  missing <- bounds$sample_id[is.na(bounds$.pred_lower) | is.na(bounds$.pred_upper)]

  if (length(missing) == 0) return(invisible(collector))

  n <- length(missing)

  log_warning(collector, "skipped", c(
    "Prediction intervals (CV+) were not computed for {n} sample{?s}: a fold model's prediction or a calibration residual was missing.",
    "i" = "{shown_ids(missing)}."
  ), ids = missing)

}

## ---------------------------------------------------------------------------
## combine_ensemble_weighted() — weighted-average combine (silent helper)
## ---------------------------------------------------------------------------

#' Combine member predictions by the ensemble weights
#'
#' The predict-time generalization of the `weighted` engine's train-time
#' combine (`fit_ensemble_weighted()`): join the per-member weights, then take
#' the weighted sum per sample. Clamped to the outcome's range, a floor at
#' zero under the default (#76).
#'
#' @param member_pred Long tibble from [predict_members()] (`config_id`,
#'   `sample_id`, `.pred`).
#' @param weights The ensemble weights tibble (`member`, `coef`).
#' @param outcome_range Numeric length-2 vector from
#'   [outcome_range_setting()]. Default `DEFAULT_OUTCOME_RANGE`.
#' @return A tibble: `sample_id`, `.pred` (ensemble point prediction).
#' @noRd
combine_ensemble_weighted <- function(member_pred, weights,
                                      outcome_range = DEFAULT_OUTCOME_RANGE) {

  joined <- member_pred |>
    dplyr::left_join(weights, by = c("config_id" = "member"))

  ## Every member has a weight: member_pred is built from weights$member, and
  ## the ensemble validator at predict()'s door refuses duplicate members and
  ## NA coefficients.

  out <- joined |>
    dplyr::group_by(.data$sample_id) |>
    dplyr::summarise(.pred = sum(.data$.pred * .data$coef),
                     .groups = "drop")

  out$.pred <- clamp_to_outcome_range(out$.pred, outcome_range)

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
#' Clamped to the outcome's range, a floor at zero under the default (#76).
#'
#' The wide-frame columns are selected explicitly from the trained-on member
#' set (not from whatever the pivot happens to produce). Every member is
#' there: `predict_members()` returns every member or aborts, and `predict()`
#' combines only when there are rows to combine.
#'
#' @param member_pred Long tibble from [predict_members()].
#' @param members Character vector of the trained-on member `config_id`s.
#' @param model The fitted meta-workflow (`object$ensemble$model`).
#' @param outcome_range Numeric length-2 vector from
#'   [outcome_range_setting()]. Default `DEFAULT_OUTCOME_RANGE`.
#' @return A tibble: `sample_id`, `.pred` (ensemble point prediction).
#'   Aborts with class `horizons_error_internal` when the meta-model returns
#'   a different number of predictions than there are samples.
#' @noRd
combine_ensemble_metamodel <- function(member_pred, members, model,
                                       outcome_range = DEFAULT_OUTCOME_RANGE) {

  member_cols <- paste0("member_", members)

  wide <- member_pred |>
    dplyr::select("sample_id", "config_id", ".pred") |>
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  combined <- stats::predict(
    model,
    new_data = wide[, member_cols, drop = FALSE]
  )$.pred

  ## Attached to wide$sample_id by position: the meta-model's predictions
  ## carry no key of their own.
  check_rows_aligned(
    what       = "Meta-model predictions",
    to         = "the samples the members predicted",
    n          = length(combined),
    n_expected = nrow(wide)
  )

  tibble::tibble(
    sample_id = wide$sample_id,
    .pred     = clamp_to_outcome_range(combined, outcome_range)
  )

}
