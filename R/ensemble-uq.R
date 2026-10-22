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

#' The fewest calibration rows CV+ needs at a level
#'
#' The smallest `n` for which [cv_plus_indices()] gives both order
#' statistics: 19 at 0.90.
#'
#' @param level Numeric in (0, 1).
#' @return Integer.
#' @keywords internal
#' @noRd
cv_plus_min_rows <- function(level) {

  n <- 2L
  while (is.null(cv_plus_indices(n, level))) n <- n + 1L
  n

}

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
#' workflow) predicts through [stats::predict()]. Clamped to the outcome's
#' range (a floor at zero under the default, #76) to match deployed-combine
#' semantics. Shared by `fit_uq_fold_models()` (assessment predictions),
#' `compute_ensemble_uq()` (leave-self-out diagnostics), and
#' `predict_ensemble_uq_matrix()` (new-data intervals) so the dispatch and
#' clamp semantics cannot drift across the three call sites.
#'
#' @param fold_model A weights tibble (`member`, `coef`) or a trained workflow.
#' @param method Character. The ensemble method (`"weighted"` combines by
#'   product; anything else predicts through the workflow).
#' @param member_mat Tibble/data.frame of `member_<config_id>` columns, in the
#'   fold model's training column order.
#' @param outcome_range Numeric length-2 vector from
#'   [outcome_range_setting()]. Default `DEFAULT_OUTCOME_RANGE`.
#' @return Numeric vector of clamped fold predictions, one per row.
#' @noRd
predict_fold_model <- function(fold_model, method, member_mat,
                               outcome_range = DEFAULT_OUTCOME_RANGE) {

  pred <- if (method == "weighted") {

    as.numeric(as.matrix(member_mat) %*% fold_model$coef)

  } else {

    stats::predict(fold_model, new_data = member_mat)$.pred

  }

  clamp_to_outcome_range(pred, outcome_range)

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

  ## Out-of-range matrix indexing returns NA columns rather than erroring, so
  ## the fold ids matter: the ensemble validator at predict()'s door refuses
  ## calibration folds that name no retained fold model.

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
#' @param oof_sample_id The rows' `sample_id`s, aligned to `meta_frame` rows.
#' @param folds An `rsample` vfold_cv over `meta_frame`.
#' @param contract The `$ensemble` contract list (for `method` and `model`).
#' @param optimize Logical. The build-time optimize flag (drives the weighted
#'   fold-weight rule).
#' @param outcome_range Numeric length-2 vector the fold predictions are
#'   clamped to. Default `DEFAULT_OUTCOME_RANGE`.
#'
#' @return A list with:
#'   \describe{
#'     \item{fold_models}{List, one per fold: a fitted workflow
#'       (penalized/xgb) or a weights tibble (weighted).}
#'     \item{calib}{Tibble: `.row`, `sample_id`, `fold`, `.pred_oof`,
#'       `truth`, `residual` (signed, original scale), one row per meta-frame
#'       sample.}
#'   }
#'
#' @noRd
fit_uq_fold_models <- function(meta_frame, oof_row, oof_sample_id, folds, contract,
                               optimize, outcome_range = DEFAULT_OUTCOME_RANGE) {

  method      <- contract$method
  members     <- contract$weights$member
  member_cols <- paste0("member_", members)

  ## Rebuild the unfitted finalized workflow once (penalized/xgb). The spec
  ## extracted from the deployed fit carries the finalized hyperparameters —
  ## finalize_workflow() substituted them before the full-data refit.
  base_wflow <- if (method != "weighted") {

    workflows::workflow() |>
      workflows::add_model(workflows::extract_spec_parsnip(contract$model)) |>
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

    ## Shared fold-prediction primitive: dispatch + clamp semantics live in
    ## one place. Clamping before the residual is deliberate — the deployed
    ## prediction rule clamps, and conformal validity requires the calibration
    ## score function to match the deployed one (residuals must be measured
    ## against the prediction the deployed path would produce).
    fold_pred <- predict_fold_model(
      fold_models[[k]],
      method,
      assessment[, member_cols, drop = FALSE],
      outcome_range
    )

    calib_rows[[k]] <- tibble::tibble(
      .row      = oof_row[assess_idx],
      sample_id = oof_sample_id[assess_idx],
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
#' The core of ensemble UQ: draws a fresh fold partition of the meta frame at
#' `conformal_seed` (disjoint-by-construction from the tuning folds — Route
#' B), runs the per-fold calibration pass, gates on calibration size and
#' order-statistic validity, computes leave-self-out diagnostics, and packs
#' the bundle. It runs inside `ensemble()`'s CV+ unit: a gate that fails is a
#' run failure with the counts and the remedy, and a fold refit that fails
#' fails at its step (`cv_plus_fold_models`), so the ensemble is point-only
#' and says why.
#'
#' **Route B honesty caveat.** Under `optimize = TRUE` the hyperparameter
#' VALUES were selected using all meta-frame rows, so exchangeability is
#' approximate; full purity would need nested CV, which the architecture does
#' not provide. What the fresh partition guarantees is that no calibration
#' residual is evaluated on the exact fold split the hyperparameters were
#' optimized against — the direct selection-bias mechanism is broken.
#'
#' **The rows a response trim set aside** calibrate too, as they calibrate a
#' single fit's intervals (#99): without them the calibration set holds only
#' rows inside the training fences, while the test set and new data keep
#' their extremes, and coverage over new data falls below the level (#103).
#' With them the calibration rows are exchangeable with new data, extremes
#' included, and coverage over new data as a whole is at the level. The
#' coverage is marginal: a sample whose outcome lies far outside the fences
#' is rarely inside its interval. Their member
#' predictions come from the final members, which never trained on them, and
#' each is given a fold at random, balanced across the folds, whose model
#' scores it. No fold model trained on any of them, so each residual is out
#' of sample, as a meta row's is. They are drawn into folds only when there
#' are some, so an ensemble without a trim calibrates exactly as before.
#'
#' @param oof The out-of-fold matrix list from [build_oof_matrix()].
#' @param contract The `$ensemble` contract list.
#' @param optimize Logical. Build-time optimize flag.
#' @param conformal_seed Integer. Seed for the fresh calibration partition.
#' @param level Numeric in (0, 1). Target coverage stored as the bundle's
#'   `level_default`.
#' @param outcome_range Numeric length-2 vector the fold predictions are
#'   clamped to. Default `DEFAULT_OUTCOME_RANGE`.
#' @param min_calib_rows Integer. The fewest rows the bundle calibrates on;
#'   `control_horizons(min_calib_rows = )`. Default `N_CALIB_MIN`.
#' @param trimmed The trimmed rows from [ensemble_trimmed_rows()]
#'   (`predictors`, `truth`, `sample_id`), or `NULL` when there are none.
#'   Default `NULL`.
#'
#' @return The uq bundle list (see [fit_ensemble_uq()]), with the tree's line
#'   for its folds as the attribute `"strata_line"` ([strata_tree_line()],
#'   `NULL` when they stratified as asked). Aborts with class
#'   `horizons_error_run` when a size gate fails.
#'
#' @noRd
compute_ensemble_uq <- function(oof,
                                contract,
                                optimize,
                                conformal_seed,
                                level          = DEFAULT_UQ_LEVEL,
                                outcome_range  = DEFAULT_OUTCOME_RANGE,
                                min_calib_rows = N_CALIB_MIN,
                                trimmed        = NULL) {

  meta_frame        <- oof$predictors
  meta_frame$.truth <- oof$truth

  ## CV+ needs cv_plus_min_rows(level) rows whatever the minimum (19 at
  ## 0.90). The gates name the constraint that binds: lowering
  ## min_calib_rows is offered only when the rows reach CV+'s floor, since
  ## below it no minimum lets the intervals run.
  floor_rows <- cv_plus_min_rows(level)

  ### A template, interpolated where the gate aborts, which sets n_have.
  remedy <- function(n_have) {

    if (n_have >= floor_rows) {
      "Fit on more rows, or lower {.code control_horizons(min_calib_rows = )} to {n_have} or fewer; CV+ at the {level} level needs {floor_rows}."
    } else {
      "Fit on more rows: CV+ at the {level} level needs {floor_rows} whatever {.code min_calib_rows} is."
    }

  }

  ## Gate 1: enough rows to calibrate at all. The rows that calibrate are the
  ## meta frame's and the trimmed rows, as fit()'s gate counts its
  ## calibration rows, trimmed rows included, and as the gate after the drop
  ## below counts them.
  n_meta    <- nrow(meta_frame)
  n_trimmed <- if (is.null(trimmed)) 0L else length(trimmed$truth)
  n_rows    <- n_meta + n_trimmed

  if (n_rows < min_calib_rows) {

    n_have <- n_rows

    what <- if (n_trimmed > 0) {
      "The meta frame's {n_meta} row{?s} and the {n_trimmed} training row{?s} the response trim set aside give {n_rows} to calibrate on, fewer than {.code min_calib_rows} ({min_calib_rows})."
    } else {
      "The meta frame has {n_rows} row{?s} to calibrate on, fewer than {.code min_calib_rows} ({min_calib_rows})."
    }

    abort_horizons(c(what, "i" = remedy(n_rows)), kind = "run")

  }

  ## Fresh partition, disjoint-by-construction from the tuning folds (which
  ## were drawn at the base seed). Stratify on .truth, mirroring the
  ## meta-learner's own fold draw, and reported from this unit as it is.
  set.seed(conformal_seed)

  drawn_cal <- draw_stratified(
    outcome      = meta_frame$.truth,
    stratified   = function() rsample::vfold_cv(meta_frame, v = 5, strata = ".truth",
                                                breaks = STRATA_BREAKS, pool = STRATA_POOL),
    unstratified = function() rsample::vfold_cv(meta_frame, v = 5)
  )
  report_strata_in_unit(drawn_cal, "CV+ calibration folds")

  folds_cal <- drawn_cal$draw

  fold_fit <- unit_step("cv_plus_fold_models", fit_uq_fold_models(
    meta_frame    = meta_frame,
    oof_row       = oof$row,
    oof_sample_id = oof$sample_id,
    folds         = folds_cal,
    contract      = contract,
    optimize      = optimize,
    outcome_range = outcome_range
  ))

  fold_models <- fold_fit$fold_models
  calib       <- fold_fit$calib

  ## Every calibration row's member predictions, for the leave-self-out
  ## diagnostic below: the meta frame's rows, then the trimmed rows'.
  ## calib_pos is each calibration row's position among them.
  member_cols <- paste0("member_", contract$weights$member)
  members_all <- meta_frame[, member_cols, drop = FALSE]
  calib_pos   <- match(calib$.row, oof$row)

  ## The rows the response trim set aside, scored by fold models that never
  ## trained on them (#103). Drawn after the fold models, in the same
  ## stream, and only when there are some.
  if (!is.null(trimmed)) {

    trimmed_calib <- unit_step("cv_plus_trimmed", calibrate_trimmed_rows(
      trimmed, fold_models, contract$method, outcome_range
    ))

    calib_pos   <- c(calib_pos, nrow(members_all) + seq_len(nrow(trimmed_calib)))
    members_all <- dplyr::bind_rows(members_all, trimmed$predictors[, member_cols, drop = FALSE])
    calib       <- dplyr::bind_rows(calib, trimmed_calib)

  }

  ## Drop rows whose residual is not finite, then re-gate: both the size
  ## minimum and the order-statistic indices must survive the drop. The index
  ## validity check shares its formula with cv_plus_bounds() via
  ## cv_plus_indices() — gate and aggregator cannot disagree.
  n_rows    <- nrow(calib)
  finite    <- is.finite(calib$residual)
  calib     <- calib[finite, ]
  calib_pos <- calib_pos[finite]
  n_calib   <- nrow(calib)
  needed  <- max(min_calib_rows, floor_rows)

  if (n_calib < needed || is.null(cv_plus_indices(n_calib, level))) {

    n_have   <- n_calib
    short_of <- if (n_calib < floor_rows) {
      "fewer than the {floor_rows} CV+ needs at the {level} level."
    } else {
      "fewer than {.code min_calib_rows} ({min_calib_rows})."
    }

    abort_horizons(c(
      paste("{n_calib} of the {n_rows} calibration row{?s} have a finite residual,", short_of),
      "i" = remedy(n_calib)
    ), kind = "run")

  }

  ## Leave-self-out diagnostics: for each calibration row i, form the CV+
  ## bound over the other n-1 rows and check whether i's truth is covered.
  ## Every fold model predicts every calibration row's members once (shared
  ## fold-prediction primitive); row lookups then index into that matrix by
  ## calib_pos.
  covered <- rep(NA, n_calib)
  widths  <- rep(NA_real_, n_calib)

  unit_step("cv_plus_diagnostics", {

    P_full <- vapply(
      fold_models,
      function(fm) {
        predict_fold_model(fm, contract$method, members_all, outcome_range)
      },
      numeric(nrow(members_all))
    )

    ## Indices for the leave-self-out set: row i stands in for the new point,
    ## so the augmented set has n_calib - 1 members and the shared formula
    ## applies with n = n_calib - 1 (its (n+1) adjustment then reads n_calib).
    idx_loo <- cv_plus_indices(n_calib - 1L, level)

    ### A calibration row kept above has a finite residual, which its own
    ### fold model's finite prediction gave it, so no augmented set here can
    ### hold an NA.
    if (!is.null(idx_loo)) {

      for (i in seq_len(n_calib)) {

        v          <- P_full[calib_pos[i], calib$fold[-i]] + calib$residual[-i]
        s          <- sort(v)
        covered[i] <- calib$truth[i] >= s[idx_loo$l] && calib$truth[i] <= s[idx_loo$u]
        widths[i]  <- s[idx_loo$u] - s[idx_loo$l]

      }

    }

  })

  ## The tree's line for the folds travels on the bundle, for ensemble() to
  ## read and drop
  bundle <- list(
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

  attr(bundle, "strata_line") <- strata_tree_line(drawn_cal, "CV+ folds")

  bundle

}

## ---------------------------------------------------------------------------
## The rows the response trim set aside (#103)
## ---------------------------------------------------------------------------

#' The members' predictions for the training rows a response trim set aside
#'
#' @description
#' `evaluate()`'s response trim (#77) keeps the training rows outside the
#' fences out of every fit, so they are in no member's out-of-fold
#' predictions and no fold model's training rows. The final members predict
#' them here, as they predict the test rows ([predict_members_on_test()]):
#' the response bound off, the outcome's range applied, which is how the
#' out-of-fold predictions the meta frame holds were made. A member that
#' cannot predict them aborts with `horizons_error_prediction_failed`, which
#' `ensemble()`'s CV+ unit records. A row any member could not use (not
#' finite after its recipe) is left out, as `predict()` leaves it out.
#'
#' @param x A `horizons_ensemble`.
#' @param members Character vector of member `config_id`s.
#' @param call The frame a member's failure reports.
#' @return `NULL` when the trim set no row aside, or no member could use
#'   one; otherwise a list with
#'   `predictors` (one `member_<config_id>` column per member, in `members`
#'   order), `truth` and `sample_id`, aligned by row.
#' @keywords internal
#' @noRd
ensemble_trimmed_rows <- function(x, members, call = rlang::caller_env()) {

  trimmed_ids <- x$evaluation$response_trim$trimmed_ids %||% character(0)

  if (length(trimmed_ids) == 0) return(NULL)

  role_map    <- x$data$role_map
  id_col      <- id_column(role_map)
  outcome_col <- role_map$variable[role_map$role == "outcome"]

  ## The rows themselves, as fit() reads them: training rows, never test
  ## rows, each with its outcome
  rows <- x$data$analysis[x$data$analysis[[id_col]] %in% trimmed_ids, , drop = FALSE]

  member_pred <- predict_members(
    x, members, rows,
    header = "The ensemble's intervals could not be calibrated: {n_failed} of its {n_total} member{?s} could not predict the training rows the response trim set aside.",
    call   = call
  )

  ## A row any member could not use is left out, as predict() leaves it out:
  ## an xgb meta-model would read the member's NA as missing and give the
  ## row a residual from a combination predict() never serves
  bad         <- unique(unlist(attr(member_pred, "flagged")))
  member_pred <- member_pred[!member_pred$sample_id %in% bad, , drop = FALSE]

  if (nrow(member_pred) == 0) return(NULL)

  wide <- member_pred |>
    dplyr::select("sample_id", "config_id", ".pred") |>
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  list(
    predictors = wide[, paste0("member_", members), drop = FALSE],
    truth      = rows[[outcome_col]][match(wide$sample_id, rows$sample_id)],
    sample_id  = wide$sample_id
  )

}

#' Score the trimmed rows with the CV+ fold models
#'
#' @description
#' Each trimmed row is given a fold, at random and balanced across the folds
#' (`sample(rep_len(seq_len(K), n))`, from the stream as the caller left it),
#' and its residual is its truth less that fold model's prediction of its
#' member predictions. No fold model trained on a trimmed row, so whichever
#' fold scores it, the residual is out of sample; the CV+ bounds then pair
#' the row's residual with the same fold model's prediction of a new point,
#' as they do for a meta row.
#'
#' @param trimmed The list from [ensemble_trimmed_rows()].
#' @param fold_models The retained fold models.
#' @param method Character. The ensemble method.
#' @param outcome_range Numeric length-2 vector the fold predictions are
#'   clamped to.
#' @return Tibble shaped as `fit_uq_fold_models()`'s `calib`, with `.row`
#'   `NA`: a trimmed row has no position among the rows the members were fit
#'   on.
#' @keywords internal
#' @noRd
calibrate_trimmed_rows <- function(trimmed, fold_models, method, outcome_range) {

  n_trim <- length(trimmed$truth)
  n_fold <- length(fold_models)
  fold   <- sample(rep_len(seq_len(n_fold), n_trim))

  preds <- vapply(
    fold_models,
    function(fm) predict_fold_model(fm, method, trimmed$predictors, outcome_range),
    numeric(n_trim)
  )

  ## vapply() drops a one-row result to a vector, one value per fold model
  dim(preds) <- c(n_trim, n_fold)

  pred_oof <- preds[cbind(seq_len(n_trim), fold)]

  tibble::tibble(
    .row      = NA_integer_,
    sample_id = trimmed$sample_id,
    fold      = fold,
    .pred_oof = pred_oof,
    truth     = trimmed$truth,
    residual  = trimmed$truth - pred_oof
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
#' retained, and the signed residuals CQR-style. After a response trim, the
#' training rows it set aside calibrate as well (see
#' `compute_ensemble_uq()`, #103). Consumed by
#' [predict.horizons_ensemble()] unless `interval = FALSE`.
#'
#' Runs inside [ensemble()] when `compute_uq = TRUE`, at `level`
#' (`DEFAULT_UQ_LEVEL`, 0.90). It is internal: to add UQ to an ensemble built
#' with `compute_uq = FALSE`, re-run [ensemble()]. The level is fixed here, as
#' `uq$level_default`; [predict.horizons_ensemble()] returns intervals at that
#' level and takes no level of its own.
#'
#' Runs inside `ensemble()`'s CV+ unit. When calibration is impossible
#' (fewer than `min_calib_rows` rows, too few finite residuals for CV+ at the
#' level, or a fold refit that fails), it raises, and `ensemble()` leaves the
#' ensemble point-only with one warning of class `horizons_warning_skipped`
#' saying why. A leave-self-out coverage more than 0.15 from the level warns
#' with class `horizons_warning_quality`; the bundle still ships.
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
#' @param min_calib_rows Integer. The fewest rows the bundle calibrates on;
#'   `control_horizons(min_calib_rows = )`. Default `N_CALIB_MIN`.
#'
#' @return `x` with `$ensemble$uq` populated, and the tree's line for the CV+
#'   folds as the attribute `"strata_line"`, which [ensemble()] reads and
#'   drops (`strata_tree_line()`, `NULL` when they stratified as asked).
#'   [ensemble()] then adds the
#'   measured coverage on the test rows, `test_coverage`, `test_mean_width`
#'   and `n_test` (`ensemble_test_coverage()`). The bundle carries: `method`
#'   (`"cv_plus"`), `ensemble_method`, `members`, `fold_models`, `calib`
#'   (`.row`, `sample_id`, `fold`, `.pred_oof`, `truth`, `residual`; `.row`
#'   is `NA` for a trimmed row), `n_calib`,
#'   `level_default`, `seed`, `oof_coverage`, `mean_width`, `timestamp`.
#'
#' @keywords internal
fit_ensemble_uq <- function(x,
                            level          = DEFAULT_UQ_LEVEL,
                            min_calib_rows = N_CALIB_MIN) {

  members <- x$ensemble$weights$member

  oof <- unit_step("oof_matrix", build_oof_matrix(x, members))

  trimmed <- unit_step("cv_plus_trimmed", ensemble_trimmed_rows(x, members))

  ## The seed ensemble() recorded, which the ensemble validator requires
  conformal_seed <- x$ensemble$seed + 1000L

  ## The build-time flag, so a weighted ensemble's fold weights are
  ## re-derived by the rule the deployed weights came from. For
  ## penalized/xgb the fold refit does not read it (the finalized spec
  ## already encodes tuning).
  optimize <- x$ensemble$optimize

  ## The door validator has refused an unusable stored range.
  outcome_range <- outcome_range_setting(x)

  bundle <- compute_ensemble_uq(
    oof            = oof,
    contract       = x$ensemble,
    optimize       = optimize,
    conformal_seed = conformal_seed,
    level          = level,
    outcome_range  = outcome_range,
    min_calib_rows = min_calib_rows,
    trimmed        = trimmed
  )

  ## Guardrail on the diagnostic itself: a bundle whose leave-self-out
  ## coverage sits far from the target is exactly the look-correct-but-wrong
  ## failure the 7%-coverage bug taught us to surface. The bundle still ships,
  ## but never silently.
  oof_cov <- bundle$oof_coverage

  if (is.finite(oof_cov) && abs(oof_cov - level) > 0.15) {

    warn_in_unit("quality", c(
      "Ensemble UQ leave-self-out coverage ({round(oof_cov, 3)}) is far from the target level ({level}).",
      "i" = "Inspect {.field ensemble$uq$calib} before trusting these intervals."
    ))

  }

  ## The tree's line for the folds moves from the bundle to the unit's
  ## value, for ensemble() to read and drop
  strata_line <- attr(bundle, "strata_line")
  attr(bundle, "strata_line") <- NULL

  x$ensemble$uq <- bundle
  attr(x, "strata_line") <- strata_line

  x

}

## ---------------------------------------------------------------------------
## ensemble_test_coverage()
## ---------------------------------------------------------------------------

#' Measure the ensemble's interval coverage on the held-out test rows
#'
#' @description
#' The members predict the test rows `fit()` held out ([predict_members_on_test()]),
#' and [predict_ensemble_intervals()], the function `predict()` serves an
#' ensemble's intervals with, builds the CV+ bounds over the rows every member
#' could use. The share of the test outcomes inside them is the coverage a
#' user of the ensemble gets on data it never saw: no test row took part in
#' the members' fits, the meta-learner or the calibration. Scored as a
#' member's coverage is ([interval_coverage()]).
#'
#' Run by [ensemble()] as a unit of its own, after the CV+ unit and only when
#' it succeeded, as `fit()`'s coverage unit follows its UQ unit: a failure
#' leaves the figures `NA` and is named in a `skipped` warning.
#'
#' @param x A `horizons_ensemble` whose `ensemble$uq` holds a CV+ bundle.
#' @param call The frame a member's failure to predict the test rows reports.
#' @return List with `test_coverage`, `test_mean_width` and `n_test`.
#' @keywords internal
#' @noRd
ensemble_test_coverage <- function(x, call = rlang::caller_env()) {

  uq <- x$ensemble$uq

  member_pred <- unit_step("test_predict", predict_members_on_test(x, uq$members, call = call))

  ## The rows every member could use, as predict() combines them
  bad    <- unique(unlist(attr(member_pred, "flagged")))
  usable <- member_pred[!member_pred$sample_id %in% bad, , drop = FALSE]

  intervals <- predict_ensemble_intervals(uq, usable, outcome_range_setting(x))

  interval_coverage(usable$truth[match(intervals$sample_id, usable$sample_id)], intervals)

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
#' models (workflows) predict through [stats::predict()]. Columns are clamped
#' to the outcome's range (a floor at zero under the default, #76) to match
#' deployed-combine semantics. The matrix's shape is stated, so one new row
#' gives a one-row matrix rather than a vector (#235).
#'
#' @param uq The `$ensemble$uq` bundle.
#' @param wide Tibble with `member_<config_id>` columns in `uq$members` order
#'   (one row per new sample).
#' @param outcome_range Numeric length-2 vector from
#'   [outcome_range_setting()]. Default `DEFAULT_OUTCOME_RANGE`.
#'
#' @return Numeric matrix, `n_new x K`.
#'
#' @noRd
predict_ensemble_uq_matrix <- function(uq, wide,
                                       outcome_range = DEFAULT_OUTCOME_RANGE) {

  member_cols <- paste0("member_", uq$members)
  member_mat  <- wide[, member_cols, drop = FALSE]

  fold_matrix <- vapply(
    uq$fold_models,
    function(fm) predict_fold_model(fm, uq$ensemble_method, member_mat,
                                    outcome_range),
    numeric(nrow(wide))
  )

  ## vapply() drops a one-row result to a vector, one value per fold model
  dim(fold_matrix) <- c(nrow(wide), length(uq$fold_models))

  fold_matrix

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
#' columns are keyed by `sample_id` so the caller puts them in place by key —
#' never a positional bind.
#'
#' The ensemble validator at `predict()`'s door has refused a bundle that is
#' not a CV+ bundle, whose members are not the weights', or whose
#' calibration set cannot support the default level (#65, #234). Run by
#' `predict()` as a unit of its own: a failure raises, labelled with its step
#' (`cv_plus_folds`, `cv_plus_bounds`), and the bounds are `NA`, with one
#' `skipped` warning.
#'
#' The deployed point prediction may occasionally fall outside its own CV+
#' interval; this is a known property of jackknife+/CV+ aggregation (the
#' bounds come from fold-model predictions, not the full-data refit) and is
#' deliberately not "repaired".
#'
#' @param uq The `$ensemble$uq` bundle (from [fit_ensemble_uq()]).
#' @param member_pred Long tibble from `predict_members()` (`config_id`,
#'   `sample_id`, `.pred`), for the rows every member could predict.
#' @param outcome_range Numeric length-2 vector the fold predictions and the
#'   bounds are clamped to, from [outcome_range_setting()]. Default
#'   `DEFAULT_OUTCOME_RANGE`.
#'
#' @return A tibble — `sample_id`, `.pred_lower`, `.pred_upper`,
#'   `.interval_width` — one row per sample.
#'
#' @noRd
predict_ensemble_intervals <- function(uq, member_pred,
                                       outcome_range = DEFAULT_OUTCOME_RANGE) {

  ## Widen member predictions by sample_id into the canonical member order.
  wide <- member_pred |>
    dplyr::select("sample_id", "config_id", ".pred") |>
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  ### member_pred holds every weighted member, and uq$members is that set.

  fold_matrix <- unit_step("cv_plus_folds", predict_ensemble_uq_matrix(uq, wide, outcome_range))

  bounds <- unit_step("cv_plus_bounds", cv_plus_bounds(
    fold_matrix = fold_matrix,
    fold_id     = uq$calib$fold,
    residuals   = uq$calib$residual,
    level       = uq$level_default
  ))

  ## Crossing repair then clamp to the outcome's range (repair-then-clamp,
  ## mirroring the single-model assembler; a floor at 0 under the default
  ## range, #76). Crossings cannot occur by construction — l <= u on one
  ## sorted vector — so the repair is defensive only.
  lo <- clamp_to_outcome_range(pmin(bounds$lower, bounds$upper), outcome_range)
  hi <- clamp_to_outcome_range(pmax(bounds$lower, bounds$upper), outcome_range)

  tibble::tibble(
    sample_id       = wide$sample_id,
    .pred_lower     = lo,
    .pred_upper     = hi,
    .interval_width = hi - lo
  )

}
