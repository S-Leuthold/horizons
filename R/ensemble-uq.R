# Ensemble uncertainty quantification: CV+ conformal prediction intervals
#
# Owns both sides of ensemble UQ. The build side (fit_ensemble_uq ->
# compute_ensemble_uq -> fit_uq_fold_models) refits the finalized meta-learner
# on a fresh 5-fold partition of the meta frame — disjoint from the tuning
# folds (Route B) — retaining the per-fold models and the signed out-of-fold
# residuals in the $ensemble$uq bundle. The predict side
# (predict_ensemble_intervals -> predict_ensemble_uq_matrix -> cv_plus_bounds)
# aggregates fold-model predictions and calibration residuals into CV+-style
# interval bounds — signed-score aggregation motivated by jackknife+/CV+
# (Barber et al. 2021) with a CQR-style signed residual (Romano et al. 2019)
# — joined to point predictions by sample_id. See cv_plus_bounds() for the
# honest statement of what is and is not guaranteed.
#
# This is deliberately NOT the single-model split-conformal machinery in
# fit-uq.R: a CV+ bound cannot be expressed as point +/- (quantile + scalar
# c_alpha), and pooling the OOF residuals into compute_c_alpha() would be
# split-conformal dressed up in CV clothing — the exact construction the
# CALIBRATION CAVEAT in ensemble-helpers.R forbids.

## ---------------------------------------------------------------------------
## cv_plus_indices()
## ---------------------------------------------------------------------------

#' Finite-Sample Order-Statistic Indices for the CV+ Aggregation
#'
#' @description
#' The single source of the index formula, shared by `cv_plus_bounds()` (the
#' aggregator), `compute_ensemble_uq()`'s calibration-size gate, and the
#' leave-self-out diagnostic (which calls it with `n - 1`). Centralized so the
#' gate and the aggregator cannot silently disagree about validity if the
#' formula ever changes.
#'
#' @param n Integer. Number of calibration samples in the augmented set.
#' @param level Numeric in (0, 1). Target two-sided coverage.
#' @return `list(l, u)` — the lower/upper order-statistic indices — or `NULL`
#'   when n is too small for valid indices at `level` (needs
#'   \eqn{n \ge 2/\alpha - 1}).
#' @noRd
cv_plus_indices <- function(n, level) {

  alpha <- 1 - level

  ## Epsilon guard: the formula is exact rational arithmetic, but alpha
  ## carries floating-point error (1 - 0.9 = 0.09999...), so at exact-integer
  ## boundaries floor((alpha/2)(n+1)) lands one BELOW the intended index
  ## (e.g. 0.05 * 40 computes as 1.9999... -> 1, not 2). Nudge toward the
  ## intended integer; non-boundary values are unaffected.
  eps <- 1e-9

  l <- floor((alpha / 2) * (n + 1) + eps)
  u <- ceiling((1 - alpha / 2) * (n + 1) - eps)

  if (l < 1 || u > n) {

    return(NULL)

  }

  list(l = l, u = u)

}

## ---------------------------------------------------------------------------
## predict_fold_model()
## ---------------------------------------------------------------------------

#' Predict a Member Matrix Through One Retained Fold Model
#'
#' @description
#' The single fold-prediction primitive: a weighted fold model (weights
#' tibble) combines by matrix product; a penalized/xgb fold model (trained
#' workflow) predicts through [stats::predict()]. Floored at zero to match
#' deployed-combine semantics. Shared by `fit_uq_fold_models()` (assessment
#' predictions), `compute_ensemble_uq()` (leave-self-out diagnostics), and
#' `predict_ensemble_uq_matrix()` (new-data intervals) so the dispatch and
#' floor semantics cannot drift across the three call sites.
#'
#' @param fold_model A weights tibble (`member`, `coef`) or a trained workflow.
#' @param method Character. The ensemble method (`"weighted"` combines by
#'   product; anything else predicts through the workflow).
#' @param member_mat Tibble/data.frame of `member_<config_id>` columns, in the
#'   fold model's training column order.
#' @return Numeric vector of floored fold predictions, one per row.
#' @noRd
predict_fold_model <- function(fold_model, method, member_mat) {

  pred <- if (method == "weighted") {

    as.numeric(as.matrix(member_mat) %*% fold_model$coef)

  } else {

    stats::predict(fold_model, new_data = member_mat)$.pred

  }

  floor_at_zero(pred)

}

## ---------------------------------------------------------------------------
## cv_plus_bounds()
## ---------------------------------------------------------------------------

#' Compute CV+ Interval Bounds from Fold Predictions and Signed Residuals
#'
#' @description
#' The pure aggregation core of ensemble UQ. For each new point x, forms the
#' augmented values \eqn{V_i(x) = \hat{\mu}_{-k(i)}(x) + r_i} — the prediction
#' of the fold model that did NOT train on calibration sample i, shifted by
#' i's signed out-of-fold residual — and reads the interval bounds off the
#' order statistics of that set:
#'
#' \itemize{
#'   \item lower = the \eqn{\lfloor (\alpha/2)(n+1) \rfloor}-th smallest of
#'     \eqn{\{V_i(x)\}}
#'   \item upper = the \eqn{\lceil (1-\alpha/2)(n+1) \rceil}-th smallest of
#'     \eqn{\{V_i(x)\}}
#' }
#'
#' with \eqn{\alpha = 1 - \text{level}} and n the number of calibration
#' samples. Raw sorted-vector indexing, not [stats::quantile()] — the
#' finite-sample indices are the guarantee.
#'
#' **What is and is not guaranteed.** This construction keeps the sign of the
#' residual (CQR-style signed score, Romano et al. 2019) and reads BOTH tails
#' off the single augmented set \eqn{\{V_i(x)\}}, giving asymmetric bounds
#' under skewed residuals. It is *motivated by* the jackknife+/CV+ rank
#' argument (Barber et al. 2021), applied per tail — but Barber's
#' \eqn{\ge 1 - 2\alpha} theorem is proven for the two-set construction with
#' unsigned nonconformity scores, not for this signed single-set variant. No
#' finite-sample distribution-free guarantee is claimed here. Coverage is
#' validated empirically instead: the leave-self-out diagnostic
#' (`oof_coverage`, computed at build time) and the coverage regression tests
#' in `test-ensemble-uq.R` are the operative evidence, and both sit near
#' \eqn{1 - \alpha} on the reference data. The signed variant is a deliberate
#' choice — the symmetric two-set construction cannot produce asymmetric
#' intervals under skewed soil-property residuals.
#'
#' @param fold_matrix Numeric matrix, `n_new x K`: column k is fold model k's
#'   prediction for each new point (original response scale).
#' @param fold_id Integer vector, length `n_calib`, values in `1..K`: the fold
#'   each calibration sample was held out from.
#' @param residuals Numeric vector, length `n_calib`: signed out-of-fold
#'   residuals `truth - .pred_oof` (original scale).
#' @param level Numeric in (0, 1). Target two-sided coverage.
#'
#' @return A list with `lower` and `upper` (numeric, length `n_new`), or
#'   `NULL` when n is too small for valid order-statistic indices at `level`
#'   (needs \eqn{n \ge 2/\alpha - 1}). New points whose augmented set contains
#'   any NA get NA bounds.
#'
#' @references
#' Barber, R. F., Candes, E. J., Ramdas, A., & Tibshirani, R. J. (2021).
#' Predictive inference with the jackknife+. *Annals of Statistics*, 49(1).
#'
#' Romano, Y., Patterson, E., & Candes, E. J. (2019). Conformalized quantile
#' regression. *NeurIPS 32*.
#'
#' @noRd
cv_plus_bounds <- function(fold_matrix, fold_id, residuals, level) {

  n <- length(residuals)

  ## Finite-sample order-statistic indices (shared source: cv_plus_indices).
  ## NULL means n is too small to support the requested coverage.
  idx <- cv_plus_indices(n, level)

  if (is.null(idx)) {

    return(NULL)

  }

  l <- idx$l
  u <- idx$u

  ## Fail clearly on a corrupted fold vector: out-of-range matrix indexing in
  ## R returns NA columns rather than erroring, which would silently degrade
  ## every bound to NA instead of surfacing the corruption.
  if (max(fold_id) > ncol(fold_matrix) || min(fold_id) < 1) {

    cli::cli_abort(c(
      "Calibration fold ids exceed the retained fold models.",
      "x" = "fold_id range: {min(fold_id)}..{max(fold_id)}; fold models: {ncol(fold_matrix)}.",
      "i" = "The uq bundle's calib$fold and fold_models are out of sync."
    ))

  }

  ## Column i of P is the prediction, for every new point, from the fold model
  ## that never saw calibration sample i. Adding r_i per column yields the
  ## augmented matrix V (n_new x n_calib).
  P <- fold_matrix[, fold_id, drop = FALSE]
  V <- sweep(P, 2, residuals, "+")

  lower <- rep(NA_real_, nrow(V))
  upper <- rep(NA_real_, nrow(V))

  for (i in seq_len(nrow(V))) {

    v <- V[i, ]

    if (anyNA(v)) {

      next

    }

    s        <- sort(v)
    lower[i] <- s[l]
    upper[i] <- s[u]

  }

  list(lower = lower, upper = upper)

}

## ---------------------------------------------------------------------------
## fit_uq_fold_models()
## ---------------------------------------------------------------------------

#' Refit the Finalized Meta-Learner Per Fold, Retaining the Fold Models
#'
#' @description
#' The Route-B calibration pass. Refits the deployed combiner on each fold's
#' analysis set and predicts its held-out assessment set, keeping BOTH the
#' fitted fold models (CV+ needs them at predict time) and the signed
#' out-of-fold residuals. A manual loop over the splits, not
#' [tune::fit_resamples()], precisely because fit_resamples discards the fold
#' models.
#'
#' For `penalized`/`xgb` the fold model is the finalized workflow (spec
#' extracted from the deployed `contract$model`, hyperparameters already
#' substituted) refit on the fold. For `weighted` the "fold model" is the
#' weights tibble re-derived within the fold's analysis set — the same
#' inverse-OOF-RMSE (or equal) rule as the engine, so the fold algorithm
#' matches the deployed one.
#'
#' @param meta_frame The meta training frame: `member_<config_id>` columns
#'   plus `.truth` (original scale).
#' @param oof_row Integer vector: the object's true `.row` ids, aligned to
#'   `meta_frame` rows.
#' @param folds An `rsample` vfold_cv over `meta_frame`.
#' @param contract The `$ensemble` contract list (for `method` and `model`).
#' @param optimize Logical. The build-time optimize flag (drives the weighted
#'   fold-weight rule).
#'
#' @return A list with:
#'   \describe{
#'     \item{fold_models}{List, one per fold: a fitted workflow
#'       (penalized/xgb) or a weights tibble (weighted).}
#'     \item{calib}{Tibble: `.row`, `fold`, `.pred_oof`, `truth`, `residual`
#'       (signed, original scale), one row per meta-frame sample.}
#'   }
#'
#' @noRd
fit_uq_fold_models <- function(meta_frame, oof_row, folds, contract, optimize) {

  method      <- contract$method
  members     <- contract$weights$member
  member_cols <- paste0("member_", members)

  ## Rebuild the unfitted finalized workflow once (penalized/xgb). The spec
  ## extracted from the deployed fit carries the finalized hyperparameters —
  ## finalize_workflow() substituted them before the full-data refit.
  base_wflow <- if (method != "weighted") {

    workflows::workflow() %>%
      workflows::add_model(workflows::extract_spec_parsnip(contract$model)) %>%
      workflows::add_formula(.truth ~ .)

  } else {

    NULL

  }

  fold_models <- vector("list", nrow(folds))
  calib_rows  <- vector("list", nrow(folds))

  for (k in seq_len(nrow(folds))) {

    split      <- folds$splits[[k]]
    analysis   <- rsample::analysis(split)
    assessment <- rsample::assessment(split)
    assess_idx <- as.integer(split, data = "assessment")

    fold_models[[k]] <- if (method == "weighted") {

      ## Fold model = weights re-derived inside the fold's analysis set.
      derive_member_weights(
        predictors = analysis[, member_cols, drop = FALSE],
        truth      = analysis$.truth,
        members    = members,
        optimize   = optimize
      )

    } else {

      parsnip::fit(base_wflow, data = analysis)

    }

    ## Shared fold-prediction primitive: dispatch + floor semantics live in
    ## one place. Flooring before the residual is deliberate — the deployed
    ## prediction rule floors, and conformal validity requires the calibration
    ## score function to match the deployed one (residuals must be measured
    ## against the prediction the deployed path would produce).
    fold_pred <- predict_fold_model(
      fold_models[[k]],
      method,
      assessment[, member_cols, drop = FALSE]
    )

    calib_rows[[k]] <- tibble::tibble(
      .row      = oof_row[assess_idx],
      fold      = k,
      .pred_oof = fold_pred,
      truth     = assessment$.truth,
      residual  = assessment$.truth - fold_pred
    )

  }

  list(
    fold_models = fold_models,
    calib       = dplyr::bind_rows(calib_rows)
  )

}

## ---------------------------------------------------------------------------
## compute_ensemble_uq()
## ---------------------------------------------------------------------------

#' Compute the CV+ Ensemble UQ Bundle (Silent Core)
#'
#' @description
#' The capture layer of ensemble UQ: draws a fresh fold partition of the meta
#' frame at `conformal_seed` (disjoint-by-construction from the tuning folds —
#' Route B), runs the per-fold calibration pass, gates on calibration size and
#' order-statistic validity, computes leave-self-out diagnostics, and packs
#' the bundle. Returns `NULL` — never errors to the caller — when any gate
#' fails, so the ensemble degrades to point-only predictions.
#'
#' **Route B honesty caveat.** Under `optimize = TRUE` the hyperparameter
#' VALUES were selected using all meta-frame rows, so exchangeability is
#' approximate; full purity would need nested CV, which the architecture does
#' not provide. What the fresh partition guarantees is that no calibration
#' residual is evaluated on the exact fold split the hyperparameters were
#' optimized against — the direct selection-bias mechanism is broken.
#'
#' @param oof The out-of-fold matrix list from [build_oof_matrix()].
#' @param contract The `$ensemble` contract list.
#' @param optimize Logical. Build-time optimize flag.
#' @param conformal_seed Integer. Seed for the fresh calibration partition.
#' @param level Numeric in (0, 1). Target coverage stored as the bundle's
#'   `level_default`.
#'
#' @return The uq bundle list (see [fit_ensemble_uq()]), or `NULL`.
#'
#' @noRd
compute_ensemble_uq <- function(oof,
                                contract,
                                optimize,
                                conformal_seed,
                                level = DEFAULT_UQ_LEVEL) {

  meta_frame        <- oof$predictors
  meta_frame$.truth <- oof$truth

  ## Gate 1: enough rows to calibrate at all.
  if (nrow(meta_frame) < N_CALIB_MIN) {

    return(NULL)

  }

  ## Fresh partition, disjoint-by-construction from the tuning folds (which
  ## were drawn at the base seed). Stratify on .truth, mirroring the
  ## meta-learner's own fold draw.
  set.seed(conformal_seed)

  folds_cal <- rsample::vfold_cv(meta_frame, v = 5, strata = ".truth")

  fold_safe <- safely_execute(
    fit_uq_fold_models(
      meta_frame = meta_frame,
      oof_row    = oof$row,
      folds      = folds_cal,
      contract   = contract,
      optimize   = optimize
    ),
    log_error = FALSE
  )

  if (is.null(fold_safe$result)) {

    return(NULL)

  }

  fold_models <- fold_safe$result$fold_models
  calib       <- fold_safe$result$calib

  ## Drop rows whose residual is not finite, then re-gate: both the size
  ## minimum and the order-statistic indices must survive the drop. The index
  ## validity check shares its formula with cv_plus_bounds() via
  ## cv_plus_indices() — gate and aggregator cannot disagree.
  calib   <- calib[is.finite(calib$residual), ]
  n_calib <- nrow(calib)

  if (n_calib < N_CALIB_MIN || is.null(cv_plus_indices(n_calib, level))) {

    return(NULL)

  }

  ## Leave-self-out diagnostics: for each calibration row i, form the CV+
  ## bound over the other n-1 rows and check whether i's truth is covered.
  ## Every fold model predicts the full meta frame once (n x K, shared
  ## fold-prediction primitive); row lookups then index into that matrix by
  ## meta-frame position.
  meta_pos    <- match(calib$.row, oof$row)
  member_cols <- paste0("member_", contract$weights$member)

  P_full <- vapply(
    fold_models,
    function(fm) {
      predict_fold_model(fm, contract$method,
                         meta_frame[, member_cols, drop = FALSE])
    },
    numeric(nrow(meta_frame))
  )

  covered <- rep(NA, n_calib)
  widths  <- rep(NA_real_, n_calib)

  ## Indices for the leave-self-out set: row i stands in for the new point, so
  ## the augmented set has n_calib - 1 members and the shared formula applies
  ## with n = n_calib - 1 (its (n+1) adjustment then reads n_calib).
  idx_loo <- cv_plus_indices(n_calib - 1L, level)

  if (!is.null(idx_loo)) {

    for (i in seq_len(n_calib)) {

      v <- P_full[meta_pos[i], calib$fold[-i]] + calib$residual[-i]

      if (anyNA(v)) {

        next

      }

      s          <- sort(v)
      covered[i] <- calib$truth[i] >= s[idx_loo$l] && calib$truth[i] <= s[idx_loo$u]
      widths[i]  <- s[idx_loo$u] - s[idx_loo$l]

    }

  }

  list(
    method          = "cv_plus",
    ensemble_method = contract$method,
    members         = contract$weights$member,
    fold_models     = fold_models,
    calib           = calib,
    n_calib         = n_calib,
    level_default   = level,
    seed            = conformal_seed,
    oof_coverage    = mean(covered, na.rm = TRUE),
    mean_width      = mean(widths, na.rm = TRUE),
    timestamp       = Sys.time()
  )

}

## ---------------------------------------------------------------------------
## fit_ensemble_uq()
## ---------------------------------------------------------------------------

#' Fit CV+ Conformal Uncertainty Quantification for a Stacked Ensemble
#'
#' @description
#' Populates `$ensemble$uq` with a CV+ conformal bundle calibrated on the
#' meta-learner's honest out-of-fold residuals: a fresh 5-fold partition of
#' the meta frame (drawn at `ensemble seed + 1000`, disjoint from the tuning
#' folds), per-fold refits of the finalized combiner with the fold models
#' retained, and the signed residuals CQR-style. Consumed by
#' [predict.horizons_ensemble()] when `interval = TRUE`.
#'
#' Runs automatically inside [ensemble()] (`compute_uq = TRUE`); call it
#' directly only to retrofit UQ onto an ensemble built with
#' `compute_uq = FALSE` or to recalibrate at a different level.
#'
#' Degrades gracefully: when calibration is impossible (fewer than
#' `N_CALIB_MIN` rows, invalid order-statistic indices, or a fold refit
#' failure), the bundle stays `NULL`, a one-line note is emitted, and the
#' ensemble continues to predict point-only.
#'
#' @details
#' **Route B honesty caveat.** The fresh calibration partition guarantees that
#' no calibration residual is scored on the exact fold split the meta-learner's
#' hyperparameters were optimized against — the direct selection-bias
#' mechanism is broken. It does NOT restore full independence: under
#' `optimize = TRUE` the hyperparameter *values* were selected using all
#' meta-frame rows, including those now in the calibration folds, so
#' exchangeability is approximate. Full purity would require nested CV, which
#' the architecture does not provide. Interpret the intervals accordingly, and
#' see `cv_plus_bounds()` for what the aggregation itself does and does not
#' guarantee.
#'
#' @param x A `horizons_ensemble` object (output of [ensemble()]).
#' @param level Numeric in (0, 1). Target two-sided coverage. Default
#'   `DEFAULT_UQ_LEVEL` (0.90).
#' @param seed Integer or NULL. Base seed for the calibration partition
#'   (`+ 1000` is applied internally). `NULL` (default) resolves to the seed
#'   recorded on the ensemble contract.
#' @param verbose Logical. Emit the degradation note when UQ cannot be
#'   computed. Default `TRUE`.
#'
#' @return `x` with `$ensemble$uq` populated (or unchanged when calibration
#'   is not possible). The bundle carries: `method` (`"cv_plus"`),
#'   `ensemble_method`, `members`, `fold_models`, `calib` (`.row`, `fold`,
#'   `.pred_oof`, `truth`, `residual`), `n_calib`, `level_default`, `seed`,
#'   `oof_coverage`, `mean_width`, `timestamp`.
#'
#' @keywords internal
fit_ensemble_uq <- function(x,
                            level   = DEFAULT_UQ_LEVEL,
                            seed    = NULL,
                            verbose = TRUE) {

  if (!inherits(x, "horizons_ensemble")) {

    cli::cli_abort(c(
      "{.arg x} must be a {.cls horizons_ensemble} object.",
      "i" = "Run {.fn ensemble} first to produce a fitted ensemble."
    ))

  }

  if (is.null(x$ensemble) || is.null(x$ensemble$weights)) {

    cli::cli_abort("No fitted ensemble found on this object.")

  }

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {

    cli::cli_abort(c(
      "{.arg level} must be a single numeric in (0, 1).",
      "x" = "Got {.val {level}}."
    ))

  }

  members <- x$ensemble$weights$member

  oof <- build_oof_matrix(x, members)

  base_seed      <- seed %||% x$ensemble$seed %||% DEFAULT_ENSEMBLE_SEED
  conformal_seed <- base_seed + 1000L

  ## Legacy retrofit: ensembles built before optimize was recorded on the
  ## contract carry optimize = NULL. Defaulting blindly to TRUE would
  ## re-derive fold weights under the WRONG rule for an equal-weights
  ## (optimize = FALSE) weighted ensemble — a silent deployed-vs-fold-model
  ## mismatch. Infer from the stored weights instead: all-equal coefficients
  ## can only come from the equal-weights rule. For penalized/xgb the flag is
  ## unused by the fold refit (the finalized spec already encodes tuning), so
  ## the inference is only load-bearing for weighted.
  optimize <- x$ensemble$optimize %||% {
    coefs <- x$ensemble$weights$coef
    !all(abs(coefs - coefs[1]) < 1e-12)
  }

  uq_safe <- safely_execute(
    compute_ensemble_uq(
      oof            = oof,
      contract       = x$ensemble,
      optimize       = optimize,
      conformal_seed = conformal_seed,
      level          = level
    ),
    log_error = FALSE
  )

  if (is.null(uq_safe$result)) {

    if (verbose) {

      cli::cli_inform(c(
        "i" = "Ensemble UQ could not be calibrated (needs >= {N_CALIB_MIN} \\
               calibration rows and a successful fold refit); predictions \\
               will be point-only."
      ))

    }

    return(x)

  }

  ## Guardrail on the diagnostic itself: a bundle whose leave-self-out
  ## coverage sits far from the target is exactly the look-correct-but-wrong
  ## failure the 7%-coverage bug taught us to surface. Warn (not abort) —
  ## the bundle still ships, but never silently.
  oof_cov <- uq_safe$result$oof_coverage

  if (is.finite(oof_cov) && abs(oof_cov - level) > 0.15) {

    cli::cli_warn(c(
      "!" = "Ensemble UQ leave-self-out coverage ({round(oof_cov, 3)}) is far \\
             from the target level ({level}).",
      "i" = "Inspect $ensemble$uq$calib before trusting these intervals."
    ))

  }

  x$ensemble$uq <- uq_safe$result

  x

}

## ---------------------------------------------------------------------------
## predict_ensemble_uq_matrix()
## ---------------------------------------------------------------------------

#' Predict New Points with Every Retained Fold Model
#'
#' @description
#' Builds the `n_new x K` fold-prediction matrix `cv_plus_bounds()` consumes:
#' each retained fold model predicts the widened member matrix. Weighted fold
#' models (weights tibbles) combine by matrix product; penalized/xgb fold
#' models (workflows) predict through [stats::predict()]. Columns are floored
#' at zero to match deployed-combine semantics.
#'
#' @param uq The `$ensemble$uq` bundle.
#' @param wide Tibble with `member_<config_id>` columns in `uq$members` order
#'   (one row per new sample).
#'
#' @return Numeric matrix, `n_new x K`.
#'
#' @noRd
predict_ensemble_uq_matrix <- function(uq, wide) {

  member_cols <- paste0("member_", uq$members)
  member_mat  <- wide[, member_cols, drop = FALSE]

  vapply(
    uq$fold_models,
    function(fm) predict_fold_model(fm, uq$ensemble_method, member_mat),
    numeric(nrow(wide))
  )

}

## ---------------------------------------------------------------------------
## predict_ensemble_intervals()
## ---------------------------------------------------------------------------

#' Assemble CV+ Prediction Intervals for New Ensemble Predictions
#'
#' @description
#' The predict-side consumer of the `$ensemble$uq` bundle. Widens the member
#' predictions the point path already computed (members are never
#' re-predicted), runs every retained fold model over them, and aggregates
#' fold predictions with the calibration residuals into CV+ bounds. Interval
#' columns are keyed by `sample_id` so the caller joins them to point
#' predictions — never a positional bind.
#'
#' Degrades to `NULL` (point-only predictions) when the bundle is not a
#' recognizable CV+ bundle, when a fold model fails to predict, or when the
#' calibration set cannot support the requested level. A missing member
#' column aborts loudly — the fold models are only valid over the exact
#' member set they trained on.
#'
#' The deployed point prediction may occasionally fall outside its own CV+
#' interval; this is a known property of jackknife+/CV+ aggregation (the
#' bounds come from fold-model predictions, not the full-data refit) and is
#' deliberately not "repaired".
#'
#' @param uq The `$ensemble$uq` bundle (from [fit_ensemble_uq()]).
#' @param member_pred Long tibble from `predict_members()` (`config_id`,
#'   `sample_id`, `.pred`).
#'
#' @return A tibble — `sample_id`, `.pred_lower`, `.pred_upper`,
#'   `.interval_width` — or `NULL` (degrade to point-only).
#'
#' @noRd
predict_ensemble_intervals <- function(uq, member_pred) {

  ## Guard: only consume a bundle this assembler understands. Anything else
  ## (a corrupt slot, a future bundle format) degrades to point-only.
  if (!is.list(uq) || !identical(uq$method, "cv_plus")) {

    return(NULL)

  }

  ## Widen member predictions by sample_id into the canonical member order.
  wide <- member_pred %>%
    dplyr::select("sample_id", "config_id", ".pred") %>%
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  member_cols <- paste0("member_", uq$members)
  missing     <- setdiff(member_cols, names(wide))

  if (length(missing) > 0) {

    cli::cli_abort(c(
      "Member prediction{?s} missing for {length(missing)} ensemble member{?s}.",
      "x" = "Absent: {.val {sub('^member_', '', missing)}}",
      "i" = "Ensemble UQ fold models are only valid over the exact member set."
    ))

  }

  ## Fold-model predictions on the new points. A fold model that cannot
  ## predict degrades the whole interval request rather than erroring the
  ## point path.
  matrix_safe <- safely_execute(
    predict_ensemble_uq_matrix(uq, wide),
    log_error = FALSE
  )

  if (is.null(matrix_safe$result)) {

    return(NULL)

  }

  bounds <- cv_plus_bounds(
    fold_matrix = matrix_safe$result,
    fold_id     = uq$calib$fold,
    residuals   = uq$calib$residual,
    level       = uq$level_default
  )

  if (is.null(bounds)) {

    return(NULL)

  }

  ## Crossing repair then floor at 0 (repair-then-floor, mirroring the
  ## single-model assembler). Crossings cannot occur by construction — l <= u
  ## on one sorted vector — so this is defensive only.
  lo <- floor_at_zero(pmin(bounds$lower, bounds$upper))
  hi <- floor_at_zero(pmax(bounds$lower, bounds$upper))

  tibble::tibble(
    sample_id       = wide$sample_id,
    .pred_lower     = lo,
    .pred_upper     = hi,
    .interval_width = hi - lo
  )

}
