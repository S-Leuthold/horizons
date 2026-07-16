# Ensemble uncertainty quantification: CV+ conformal prediction intervals
#
# Owns both sides of ensemble UQ. The build side (fit_ensemble_uq ->
# compute_ensemble_uq -> fit_uq_fold_models) refits the finalized meta-learner
# on a fresh 5-fold partition of the meta frame — disjoint from the tuning
# folds (Route B) — retaining the per-fold models and the signed out-of-fold
# residuals in the $ensemble$uq bundle. The predict side
# (predict_ensemble_intervals -> predict_ensemble_uq_matrix -> cv_plus_bounds)
# aggregates fold-model predictions and calibration residuals into genuine CV+
# interval bounds (Barber et al. 2021) with a CQR-style signed score (Romano
# et al. 2019), joined to point predictions by sample_id.
#
# This is deliberately NOT the single-model split-conformal machinery in
# fit-uq.R: a CV+ bound cannot be expressed as point +/- (quantile + scalar
# c_alpha), and pooling the OOF residuals into compute_c_alpha() would be
# split-conformal dressed up in CV clothing — the exact construction the
# PHASE-2 caveat in ensemble-helpers.R forbids.

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
#' This is CV+ (Barber et al. 2021) with the sign of the residual kept
#' (CQR-style signed score, Romano et al. 2019) and alpha split per tail on
#' the single augmented set, giving asymmetric bounds under skewed residuals.
#' Worst-case distribution-free two-sided coverage is \eqn{\ge 1 - 2\alpha}
#' (plus small K-fold slack); empirically coverage sits near \eqn{1 - \alpha}.
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

  n     <- length(residuals)
  alpha <- 1 - level

  ## Finite-sample order-statistic indices. Invalid indices (l < 1 or u > n)
  ## mean n is too small to support the requested coverage — no bound exists.
  l <- floor((alpha / 2) * (n + 1))
  u <- ceiling((1 - alpha / 2) * (n + 1))

  if (l < 1 || u > n) {

    return(NULL)

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

    if (method == "weighted") {

      ## Fold model = weights re-derived inside the fold's analysis set.
      fold_weights <- derive_member_weights(
        predictors = analysis[, member_cols, drop = FALSE],
        truth      = analysis$.truth,
        members    = members,
        optimize   = optimize
      )

      fold_models[[k]] <- fold_weights

      fold_pred <- as.numeric(
        as.matrix(assessment[, member_cols, drop = FALSE]) %*% fold_weights$coef
      )

    } else {

      fold_fit <- parsnip::fit(base_wflow, data = analysis)

      fold_models[[k]] <- fold_fit

      fold_pred <- stats::predict(
        fold_fit,
        new_data = assessment[, member_cols, drop = FALSE]
      )$.pred

    }

    ## Floor fold predictions at zero to match deployed-combine semantics —
    ## residuals must be measured against the same prediction the deployed
    ## path would produce.
    fold_pred <- floor_at_zero(fold_pred)

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
  ## minimum and the order-statistic indices must survive the drop.
  calib   <- calib[is.finite(calib$residual), ]
  n_calib <- nrow(calib)

  alpha <- 1 - level
  l     <- floor((alpha / 2) * (n_calib + 1))
  u     <- ceiling((1 - alpha / 2) * (n_calib + 1))

  if (n_calib < N_CALIB_MIN || l < 1 || u > n_calib) {

    return(NULL)

  }

  ## Leave-self-out diagnostics: for each calibration row i, form the CV+
  ## bound over the other n-1 rows and check whether i's truth is covered.
  ## Every fold model predicts the full meta frame once (n x K); row lookups
  ## then index into that matrix by meta-frame position.
  meta_pos <- match(calib$.row, oof$row)

  P_full <- vapply(
    fold_models,
    function(fm) {

      if (contract$method == "weighted") {

        floor_at_zero(as.numeric(
          as.matrix(meta_frame[, paste0("member_", contract$weights$member),
                               drop = FALSE]) %*% fm$coef
        ))

      } else {

        floor_at_zero(stats::predict(
          fm,
          new_data = meta_frame[, paste0("member_", contract$weights$member),
                                drop = FALSE]
        )$.pred)

      }

    },
    numeric(nrow(meta_frame))
  )

  covered <- rep(NA, n_calib)
  widths  <- rep(NA_real_, n_calib)

  ## Indices for the leave-self-out set (size n_calib - 1).
  l_loo <- floor((alpha / 2) * n_calib)
  u_loo <- ceiling((1 - alpha / 2) * n_calib)

  if (l_loo >= 1 && u_loo <= n_calib - 1) {

    for (i in seq_len(n_calib)) {

      v <- P_full[meta_pos[i], calib$fold[-i]] + calib$residual[-i]

      if (anyNA(v)) {

        next

      }

      s          <- sort(v)
      covered[i] <- calib$truth[i] >= s[l_loo] && calib$truth[i] <= s[u_loo]
      widths[i]  <- s[u_loo] - s[l_loo]

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

  base_seed      <- seed %||% x$ensemble$seed %||% 307L
  conformal_seed <- base_seed + 1000L

  uq_safe <- safely_execute(
    compute_ensemble_uq(
      oof            = oof,
      contract       = x$ensemble,
      optimize       = x$ensemble$optimize %||% TRUE,
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
    function(fm) {

      if (uq$ensemble_method == "weighted") {

        floor_at_zero(as.numeric(as.matrix(member_mat) %*% fm$coef))

      } else {

        floor_at_zero(stats::predict(fm, new_data = member_mat)$.pred)

      }

    },
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
