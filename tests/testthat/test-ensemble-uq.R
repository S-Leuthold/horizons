## ---------------------------------------------------------------------------
## Tests: ensemble UQ (CV+ conformal prediction intervals)
## ---------------------------------------------------------------------------
## Exercises both sides of ensemble-uq.R: the build side (fit_ensemble_uq ->
## compute_ensemble_uq -> fit_uq_fold_models, the Route-B fresh-partition
## calibration) and the predict side (predict_ensemble_intervals ->
## cv_plus_bounds, genuine CV+ aggregation joined by sample_id). Uses the same
## real fixture as test-pipeline-ensemble.R: a small horizons_fit on real
## (anonymized) MIR spectra (43 meta rows — clears N_CALIB_MIN — and a 12-row
## held-out test set).
##
## ens_fitted(), ens_test_set() and ens_built() come from helper-ensemble.R.
## The reference ensemble the read-only tests share, ens_ref, is
## ens_built("weighted") (weighted at optimize = FALSE: fast, deterministic),
## built once, on first use.
## ---------------------------------------------------------------------------


## ---------------------------------------------------------------------------
## cv_plus_bounds() — the pure aggregation core
## ---------------------------------------------------------------------------

describe("cv_plus_bounds()", {

  it("reproduces hand-computed CV+ order statistics", {

    ## 39 calibration samples, K = 2 fold models, 1 new point. The expected
    ## bounds are derived independently below by explicit sort-and-index —
    ## the same construction the function implements, computed by hand.
    set.seed(11)
    n_calib   <- 39
    fold_id   <- rep(1:2, length.out = n_calib)
    residuals <- rnorm(n_calib)
    fold_pred <- c(10, 12)                      # fold model predictions at x
    fold_mat  <- matrix(fold_pred, nrow = 1)

    ## Explicit integer indices (exact rational math: .05 * 40 = 2,
    ## .95 * 40 = 38) — deliberately NOT recomputed with the fp formula,
    ## which lands one low at exact boundaries (the epsilon-guard regression).
    level <- 0.90
    l     <- 2L
    u     <- 38L

    v_hand <- sort(fold_pred[fold_id] + residuals)

    result <- cv_plus_bounds(fold_mat, fold_id, residuals, level)

    expect_equal(result$lower, v_hand[l])
    expect_equal(result$upper, v_hand[u])

  })

  it("returns NULL when n is too small for valid indices", {

    ## n = 10 at level .90: l = floor(.05 * 11) = 0 -> no valid bound.
    result <- cv_plus_bounds(
      matrix(c(1, 2), nrow = 1),
      rep(1:2, 5),
      rnorm(10),
      level = 0.90
    )

    expect_null(result)

  })

  it("gives NA bounds to new points whose augmented set contains NA", {

    ## matrix() fills column-wise: row 1 = (1, 2) complete, row 2 = (NA, 3).
    fold_mat <- matrix(c(1, NA_real_, 2, 3), nrow = 2)

    result <- cv_plus_bounds(
      fold_mat,
      rep(1:2, length.out = 39),
      rnorm(39),
      level = 0.90
    )

    ## One bound per new point, the last one included
    expect_length(result$lower, 2L)
    expect_length(result$upper, 2L)
    expect_false(is.na(result$lower[1]))
    expect_true(is.na(result$lower[2]))
    expect_true(is.na(result$upper[2]))

  })

  it("bounds are ordered by construction (l <= u on one sorted vector)", {

    set.seed(12)
    result <- cv_plus_bounds(
      matrix(rnorm(50), nrow = 10),
      rep(1:5, each = 8),
      rnorm(40),
      level = 0.90
    )

    expect_true(all(result$upper >= result$lower))

  })

})


## ---------------------------------------------------------------------------
## Bundle contract (field regression, mirrors test-fit-uq.R's pattern)
## ---------------------------------------------------------------------------

describe("fit_ensemble_uq() - bundle contract", {

  it("populates $ensemble$uq with the exact field set", {

    uq <- ens_built("weighted")$ensemble$uq

    expect_setequal(
      names(uq),
      c("method", "ensemble_method", "members", "fold_models", "calib",
        "n_calib", "level_default", "seed", "oof_coverage", "mean_width",
        "timestamp")
    )

  })

  it("carries the CV+ discriminator and build facts", {

    ens_ref <- ens_built("weighted")
    uq      <- ens_ref$ensemble$uq

    expect_identical(uq$method, "cv_plus")
    expect_identical(uq$ensemble_method, "weighted")
    expect_equal(length(uq$fold_models), 5)
    expect_equal(uq$level_default, DEFAULT_UQ_LEVEL)
    expect_equal(uq$seed, 307L + 1000L)
    expect_setequal(uq$members, ens_ref$ensemble$weights$member)

  })

  it("calib carries signed original-scale residuals for every meta row", {

    uq <- ens_built("weighted")$ensemble$uq

    expect_setequal(names(uq$calib),
                    c(".row", "fold", ".pred_oof", "truth", "residual"))
    expect_equal(uq$n_calib, nrow(uq$calib))
    expect_true(any(uq$calib$residual < 0) && any(uq$calib$residual > 0))
    expect_equal(uq$calib$residual, uq$calib$truth - uq$calib$.pred_oof)

  })

  it("records optimize and seed on the ensemble contract", {

    ens_ref <- ens_built("weighted")

    expect_identical(ens_ref$ensemble$optimize, FALSE)
    expect_identical(ens_ref$ensemble$seed, 307L)

  })

})


## ---------------------------------------------------------------------------
## Fold honesty (the 26x-overconfidence regression) + Route B disjointness
## ---------------------------------------------------------------------------

describe("fit_ensemble_uq() - fold honesty and Route B", {

  it("calib partitions every meta row into exactly one fold", {

    ens_ref <- ens_built("weighted")
    uq  <- ens_ref$ensemble$uq
    oof <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    expect_setequal(uq$calib$.row, oof$row)
    expect_equal(anyDuplicated(uq$calib$.row), 0L)
    expect_true(all(uq$calib$fold %in% 1:5))

  })

  it("the stored seed reproduces the fold assignment exactly", {

    ens_ref <- ens_built("weighted")
    uq  <- ens_ref$ensemble$uq
    oof <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    meta_frame        <- oof$predictors
    meta_frame$.truth <- oof$truth

    set.seed(uq$seed)
    folds_redrawn <- suppressWarnings(
      rsample::vfold_cv(meta_frame, v = 5, strata = ".truth")
    )

    for (k in seq_len(nrow(folds_redrawn))) {

      redrawn_rows <- oof$row[
        as.integer(folds_redrawn$splits[[k]], data = "assessment")
      ]

      expect_setequal(redrawn_rows, uq$calib$.row[uq$calib$fold == k])

    }

  })

  it("calibration residuals are honest OOF, not in-sample (26x regression)", {

    ## xgboost overfits its training frame hard, so in-sample residuals of
    ## the deployed meta-model are much smaller than honest fold-held-out
    ## residuals. In-sample gave 7% coverage at nominal 90% once; never again.
    ens_xgb <- ens_built("xgb")

    uq  <- ens_xgb$ensemble$uq
    oof <- build_oof_matrix(ens_xgb, ens_xgb$ensemble$weights$member)

    meta_frame <- oof$predictors

    in_sample_pred <- clamp_to_outcome_range(
      stats::predict(ens_xgb$ensemble$model, new_data = meta_frame)$.pred
    )
    in_sample_resid <- oof$truth - in_sample_pred

    calib_resid <- uq$calib$residual[match(oof$row, uq$calib$.row)]

    expect_false(isTRUE(all.equal(calib_resid, in_sample_resid)))
    expect_gte(mean(abs(calib_resid)), mean(abs(in_sample_resid)))

  })

  it("the calibration partition differs from the tuning partition (Route B)", {

    ens_ref <- ens_built("weighted")
    uq  <- ens_ref$ensemble$uq
    oof <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    meta_frame        <- oof$predictors
    meta_frame$.truth <- oof$truth

    fold_of <- function(seed) {

      set.seed(seed)
      folds <- suppressWarnings(
        rsample::vfold_cv(meta_frame, v = 5, strata = ".truth")
      )

      assign <- integer(nrow(meta_frame))
      for (k in seq_len(nrow(folds))) {
        assign[as.integer(folds$splits[[k]], data = "assessment")] <- k
      }
      assign

    }

    expect_false(identical(fold_of(307L), fold_of(uq$seed)))

  })

})


## ---------------------------------------------------------------------------
## Coverage
## ---------------------------------------------------------------------------

describe("fit_ensemble_uq() - coverage", {

  it("leave-self-out calibration coverage is near nominal", {

    ## The diagnostic that catches a broken score or dishonest residuals:
    ## at level .90, coverage collapsing toward 0 (the in-sample failure
    ## mode) or far below nominal fails here. 43 points is noisy; the band
    ## is wide but a real break lands far outside it.
    ens_ref <- ens_built("weighted")

    expect_gt(ens_ref$ensemble$uq$oof_coverage, 0.70)
    expect_lte(ens_ref$ensemble$uq$oof_coverage, 1.00)
    expect_gt(ens_ref$ensemble$uq$mean_width, 0)

  })

  it("computes the leave-self-out coverage and width from its own calibration set, row by row", {

    ## Each calibration row stands in for a new point: its CV+ bound is
    ## formed from the other rows' fold models and residuals, at the
    ## n_calib - 1 indices, and the bundle's figures are the means
    ens_ref <- ens_built("weighted")
    uq      <- ens_ref$ensemble$uq
    oof     <- build_oof_matrix(ens_ref, uq$members)

    member_mat <- oof$predictors[, paste0("member_", uq$members), drop = FALSE]
    P <- vapply(uq$fold_models, function(fm) {
      predict_fold_model(fm, uq$ensemble_method, member_mat,
                         outcome_range_setting(ens_ref))
    }, numeric(nrow(member_mat)))

    pos <- match(uq$calib$.row, oof$row)
    idx <- cv_plus_indices(uq$n_calib - 1L, uq$level_default)

    covered <- logical(uq$n_calib)
    width   <- numeric(uq$n_calib)

    for (i in seq_len(uq$n_calib)) {
      s <- sort(P[pos[i], uq$calib$fold[-i]] + uq$calib$residual[-i])
      covered[i] <- uq$calib$truth[i] >= s[idx$l] && uq$calib$truth[i] <= s[idx$u]
      width[i]   <- s[idx$u] - s[idx$l]
    }

    expect_equal(uq$oof_coverage, mean(covered))
    expect_equal(uq$mean_width, mean(width))

  })

  it("warns when the leave-self-out coverage is far from the target, and only then", {

    ## The guardrail added after the 7%-coverage bug: a bundle whose own
    ## diagnostic sits more than 0.15 from the level still ships, with a warning
    ens_ref <- ens_built("weighted")
    bundle  <- ens_ref$ensemble$uq

    bundle$oof_coverage <- 0.60
    local_mocked_bindings(compute_ensemble_uq = function(...) bundle)

    expect_warning(fit_ensemble_uq(ens_ref, level = 0.90),
                   "is far from the target level", class = "horizons_warning_quality")

    bundle$oof_coverage <- 0.80
    expect_no_warning(fit_ensemble_uq(ens_ref, level = 0.90))

    ## Inside ensemble() it is recorded, written to the log and raised once,
    ## at the end
    bundle$oof_coverage <- 0.60
    w <- expect_warning(
      ens <- suppressWarnings(ensemble(ens_fitted(), method = "weighted", optimize = FALSE),
                              classes = "simpleWarning"),
      "is far from the target level", class = "horizons_warning_quality"
    )
    expect_identical(w$log$verb, "ensemble")
    expect_identical(ens$log$topic[ens$log$verb == "ensemble"], "quality")

  })

  it("covers new points at about the nominal level on exchangeable data", {

    ## CV+ on its own, at a size where coverage can be read: fold models that
    ## predict the signal exactly, calibration residuals and new errors from
    ## one distribution, so the bounds should cover 90% of new truths. Over
    ## 30 seeds coverage ran from 0.876 to 0.938. (The ensemble's 12 held-out
    ## points are too few for a coverage claim; the leave-self-out test above
    ## is the claim on the fitted bundle.)
    withr::local_seed(3)
    n_calib <- 400; n_new <- 4000; k <- 5

    residuals   <- stats::rnorm(n_calib)
    fold_id     <- sample(rep_len(seq_len(k), n_calib))
    signal_new  <- stats::runif(n_new, 10, 30)
    fold_matrix <- matrix(signal_new, n_new, k)

    bounds <- cv_plus_bounds(fold_matrix, fold_id, residuals, level = 0.90)
    truth  <- signal_new + stats::rnorm(n_new)

    covered <- mean(truth >= bounds$lower & truth <= bounds$upper)

    expect_gte(covered, 0.85)
    expect_lte(covered, 0.95)

  })

})


## ---------------------------------------------------------------------------
## Predict side: sample_id join, ordering, method x optimize grid
## ---------------------------------------------------------------------------

describe("predict.horizons_ensemble() - CV+ intervals", {

  it("returns a prediction and its interval for a single row of new_data (#235)", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    p <- predict(ens_ref, test_set[1, ], interval = TRUE)

    expect_equal(nrow(p), 1L)
    expect_identical(p$sample_id, test_set$sample_id[1])
    expect_true(is.finite(p$.pred_lower) && is.finite(p$.pred_upper))
    expect_lte(p$.pred_lower, p$.pred_upper)

  })

  it("interval columns key by sample_id: shuffled rows give identical bounds", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    p1 <- suppressWarnings(predict(ens_ref, test_set, interval = TRUE))

    set.seed(7)
    shuffled <- test_set[sample(nrow(test_set)), ]
    p2 <- suppressWarnings(predict(ens_ref, shuffled, interval = TRUE))

    joined <- dplyr::inner_join(p1, p2, by = "sample_id",
                                suffix = c("_a", "_b"))

    expect_equal(nrow(joined), nrow(p1))
    expect_equal(joined$.pred_lower_a, joined$.pred_lower_b)
    expect_equal(joined$.pred_upper_a, joined$.pred_upper_b)

  })

  it("bounds are ordered, non-negative, and width-consistent", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    p <- suppressWarnings(predict(ens_ref, test_set, interval = TRUE))

    expect_true(all(p$.pred_upper >= p$.pred_lower))
    expect_true(all(p$.pred_lower >= 0))
    expect_equal(p$.interval_width, p$.pred_upper - p$.pred_lower)

  })

  for (m in c("weighted", "penalized", "xgb")) {

    for (opt in c(TRUE, FALSE)) {

      it(paste0("method = '", m, "', optimize = ", opt,
                " produces a bundle and interval columns"), {

        if (opt && m != "weighted") skip_unless_slow_tier()

        test_set <- ens_test_set()
        ens      <- ens_built(m, optimize = opt)

        expect_false(is.null(ens$ensemble$uq))
        expect_identical(ens$ensemble$uq$method, "cv_plus")

        p <- suppressWarnings(predict(ens, test_set, interval = TRUE))

        expect_true(all(c(".pred_lower", ".pred_upper") %in% names(p)))
        expect_true(all(p$.pred_upper >= p$.pred_lower, na.rm = TRUE))

      })

    }

  }

  it("weighted optimize = FALSE fold models are equal weights summing to 1", {

    ens <- ens_built("weighted")

    for (fm in ens$ensemble$uq$fold_models) {

      expect_s3_class(fm, "tbl_df")
      expect_equal(sum(fm$coef), 1)
      expect_true(all(abs(fm$coef - fm$coef[1]) < 1e-12))

    }

  })

})


## ---------------------------------------------------------------------------
## TEETH — non-degeneracy: the meta-learner learns a real combination
## ---------------------------------------------------------------------------
## On real signal the penalized engine must assign non-zero member weights;
## intercept-only (all coefs 0) is the degenerate failure the synthetic data
## produced and is NOT acceptable here. These read the grid's penalized
## optimize = TRUE build; a memoised build is shared only within one process,
## so every reader of it sits in this file.

describe("ensemble() penalized - non-degenerate on real signal", {

  it("learns at least one non-zero member coefficient, at a tuned penalty glmnet can use", {

    skip_unless_slow_tier()

    ens   <- ens_built("penalized", optimize = TRUE)
    coefs <- ens$ensemble$weights$coef
    expect_gt(sum(abs(coefs) > 1e-8), 0)

    ## The tuning chose a real amount of regularization and a mixing share
    args    <- workflows::extract_spec_parsnip(ens$ensemble$model)$args
    penalty <- rlang::eval_tidy(args$penalty)
    mixture <- rlang::eval_tidy(args$mixture)

    expect_true(is.finite(penalty) && penalty > 0)
    expect_true(mixture >= 0 && mixture <= 1)

  })

  it("produces ensemble test predictions correlated with truth", {

    skip_unless_slow_tier()

    ens <- ens_built("penalized", optimize = TRUE)
    ep  <- ens$ensemble$predictions
    expect_gt(stats::cor(ep$.pred, ep$truth), 0.3)

  })

})


## ---------------------------------------------------------------------------
## Predict side: arguments in `...` (#141)
## ---------------------------------------------------------------------------
## The CV+ intervals are at the level fit_ensemble_uq() calibrated. A `level`
## warns that it is ignored, and any other argument aborts.

describe("predict.horizons_ensemble() - arguments in ...", {

  it("warns that level is ignored and returns the calibration-level intervals", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    expect_equal(ens_ref$ensemble$uq$level_default, 0.90)

    reference <- predict(ens_ref, test_set)

    w <- expect_warning(
      p <- predict(ens_ref, test_set, level = 0.95),
      class = "horizons_warning_input"
    )

    expect_match(conditionMessage(w), "0.9.", fixed = TRUE)
    expect_equal(p, reference)

  })

  it("names the default level when the ensemble has no UQ bundle", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    no_uq <- ens_ref
    no_uq$ensemble["uq"] <- list(NULL)

    w <- expect_warning(
      predict(no_uq, test_set, level = 0.95),
      class = "horizons_warning_input"
    )

    expect_match(conditionMessage(w), paste0(DEFAULT_UQ_LEVEL, "."), fixed = TRUE)

  })

  it("errors on any other argument, naming it", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    expect_error(
      predict(ens_ref, test_set, config = "all"),
      class  = "horizons_error_input",
      regexp = "config"
    )

  })

  it("a call with neither is unchanged", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    expect_no_warning(p <- predict(ens_ref, test_set))
    expect_true(all(c(".pred", ".pred_lower", ".pred_upper") %in% names(p)))

  })

})


## ---------------------------------------------------------------------------
## Degradation and gates
## ---------------------------------------------------------------------------

describe("ensemble UQ - degradation and gates", {

  it("compute_ensemble_uq refuses below min_calib_rows, with the counts and the remedy", {

    ens_ref <- ens_built("weighted")
    oof     <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    keep <- seq_len(N_CALIB_MIN - 5L)

    oof_small <- list(
      predictors = oof$predictors[keep, , drop = FALSE],
      truth      = oof$truth[keep],
      row        = oof$row[keep],
      members    = oof$members
    )

    err <- expect_error(
      compute_ensemble_uq(oof = oof_small, contract = ens_ref$ensemble,
                          optimize = FALSE, conformal_seed = 1307L),
      class = "horizons_error_run"
    )
    expect_match(conditionMessage(err), "has 25 rows to calibrate on, fewer than `min_calib_rows` (30)",
                 fixed = TRUE)
    expect_match(conditionMessage(err), "to 19 at least", fixed = TRUE)

    ## The same rows calibrate at a minimum below them. suppressWarnings():
    ## rsample's note that 25 rows are too few for its default strata breaks
    expect_false(is.null(suppressWarnings(compute_ensemble_uq(
      oof = oof_small, contract = ens_ref$ensemble, optimize = FALSE,
      conformal_seed = 1307L, min_calib_rows = 10L
    ))))

  })

  it("compute_ensemble_uq drops non-finite residuals, and refuses when too few are left", {

    ens_ref <- ens_built("weighted")
    oof     <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    ## The real fold pass, with chosen calibration residuals made non-finite
    real_folds <- fit_uq_fold_models
    spoil      <- NULL
    local_mocked_bindings(fit_uq_fold_models = function(...) {
      out <- real_folds(...)
      out$calib$residual[spoil] <- c(Inf, NaN, NA)[(seq_along(spoil) - 1L) %% 3L + 1L]
      out
    })

    run <- function() {
      compute_ensemble_uq(oof = oof, contract = ens_ref$ensemble,
                          optimize = FALSE, conformal_seed = 1307L)
    }

    spoil  <- 1:3
    result <- run()
    expect_identical(result$n_calib, length(oof$truth) - 3L)
    expect_identical(nrow(result$calib), result$n_calib)
    expect_true(all(is.finite(result$calib$residual)))

    ## One more than the rows above the minimum: the size gate runs again
    ## after the drop
    spoil <- seq_len(length(oof$truth) - N_CALIB_MIN + 1L)
    err   <- expect_error(run(), class = "horizons_error_run")
    expect_match(conditionMessage(err),
                 sprintf("29 of the %d calibration rows have a finite residual, fewer than the 30", length(oof$truth)),
                 fixed = TRUE)

  })

  it("compute_ensemble_uq fails at its fold step when a fold refit fails", {

    ens_ref <- ens_built("weighted")
    oof     <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    local_mocked_bindings(fit_uq_fold_models = function(...) stop("a fold refit failed"))

    ## suppressWarnings(): rsample's note that the 43 meta rows are too few
    ## for its default strata breaks
    out <- suppressWarnings(run_unit(
      compute_ensemble_uq(oof = oof, contract = ens_ref$ensemble,
                          optimize = FALSE, conformal_seed = 1307L),
      unit = "cv_plus", id = "weighted"
    ))

    expect_true(out$failed)
    expect_identical(unit_failure_message(out$records, outside = NULL),
                     "CV+ fold models failed: a fold refit failed")

  })

  it("leaves the ensemble point-only, and says why once whatever verbose is, when calibration cannot run", {

    ## UQ never fails the ensemble: a gate or a fold refit that fails leaves
    ## $ensemble$uq empty, the log says why, and one skipped warning is raised
    local_mocked_bindings(fit_uq_fold_models = function(...) stop("a fold refit failed"))

    for (verbose in c(FALSE, TRUE)) {

      out <- utils::capture.output(run <- collect_warnings(
        ensemble(ens_fitted(), method = "weighted", optimize = FALSE,
                 control = control_horizons(verbose = verbose)),
        "Prediction intervals \\(CV\\+\\) were not computed"
      ))

      ens <- run$value

      expect_null(ens$ensemble$uq)
      expect_length(run$warnings, 1L)
      expect_s3_class(run$warnings[[1]], "horizons_warning_skipped")
      expect_match(conditionMessage(run$warnings[[1]]),
                   "CV+ fold models failed: a fold refit failed", fixed = TRUE)

      skipped <- ens$log[ens$log$verb == "ensemble" & ens$log$topic %in% "skipped", , drop = FALSE]
      expect_identical(nrow(skipped), 1L)

    }

  })

  it("stops the call when an invariant breaks inside the CV+ calibration", {

    local_mocked_bindings(fit_uq_fold_models = function(...) {
      abort_horizons("a broken invariant", kind = "internal")
    })

    expect_error(
      suppressWarnings(ensemble(ens_fitted(), method = "weighted", optimize = FALSE)),
      class = "horizons_error_internal"
    )

  })

  it("fit_ensemble_uq hands its minimum to the calibration", {

    ens_ref <- ens_built("weighted")
    no_uq   <- ens_ref
    no_uq$ensemble["uq"] <- list(NULL)

    seen <- NULL
    local_mocked_bindings(compute_ensemble_uq = function(..., min_calib_rows) {
      seen <<- min_calib_rows
      ens_ref$ensemble$uq
    })

    fit_ensemble_uq(no_uq, min_calib_rows = 12L)
    expect_identical(seen, 12L)

  })

  ## The bounds are recomputed by hand from the bundle and the member
  ## predictions predict() passed to the assembler: each fold model's
  ## predictions, then CV+'s order statistics. The penalized bundle's fold
  ## models differ, so a fault in the residuals, the fold ids or the fold
  ## models' order shows as a mismatch; the weighted bundle's, built without
  ## tuning, are all the same average.
  for (method in c("weighted", "penalized")) {

    it(paste("serves the CV+ bounds of the calibration residuals, wired through predict(), for", method), {

      ens      <- ens_built(method)
      test_set <- ens_test_set()
      uq       <- ens$ensemble$uq
      range    <- outcome_range_setting(ens)
      expect_false(is.null(uq))

      seen     <- NULL
      assemble <- predict_ensemble_intervals
      local_mocked_bindings(predict_ensemble_intervals = function(uq, member_pred, outcome_range) {
        seen <<- list(member_pred = member_pred, outcome_range = outcome_range)
        assemble(uq, member_pred, outcome_range)
      })

      p <- suppressWarnings(predict(ens, test_set, interval = TRUE))
      expect_identical(seen$outcome_range, range)

      wide <- tidyr::pivot_wider(
        dplyr::select(seen$member_pred, "sample_id", "config_id", ".pred"),
        names_from = "config_id", values_from = ".pred", names_prefix = "member_"
      )
      member_mat <- as.matrix(wide[, paste0("member_", uq$members)])

      P   <- vapply(uq$fold_models, function(fm) {
        predict_fold_model(fm, uq$ensemble_method, member_mat, range)
      }, numeric(nrow(wide)))
      idx <- cv_plus_indices(uq$n_calib, uq$level_default)

      lo <- hi <- numeric(nrow(wide))
      for (i in seq_len(nrow(wide))) {
        sorted <- sort(P[i, uq$calib$fold] + uq$calib$residual)
        lo[i]  <- sorted[idx$l]
        hi[i]  <- sorted[idx$u]
      }

      got <- p[match(wide$sample_id, p$sample_id), ]
      expect_equal(got$.pred_lower, clamp_to_outcome_range(lo, range))
      expect_equal(got$.pred_upper, clamp_to_outcome_range(hi, range))

      if (method == "penalized") {
        expect_gt(max(apply(P, 1, function(r) diff(range(r)))), 0)
      }

    })

  }

  it("refuses at predict()'s door a UQ bundle it cannot use (#65, #234)", {

    ## These used to degrade to point predictions inside the assembler, or
    ## index fold models that are not there; the ensemble validator now
    ## refuses each at the door, naming it.
    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    refuses <- function(edit, finding) {
      broken <- ens_ref
      broken$ensemble$uq <- edit(broken$ensemble$uq)
      err <- expect_error(predict(broken, test_set, interval = TRUE),
                          class = "horizons_error_validation")
      expect_true(any(grepl(finding, err$findings, fixed = TRUE)), label = finding)
    }

    ## Not a CV+ bundle
    refuses(function(uq) { uq$method <- "bogus"; uq }, "must be NULL or a CV+ bundle")

    ## A calibration set too small for the default level (n >= 19 at 0.90)
    refuses(function(uq) { uq$calib <- uq$calib[1:2, ]; uq$n_calib <- 2L; uq },
            "cannot support intervals at level_default")

    ## A member set that is not the weights'
    refuses(function(uq) { uq$members <- uq$members[-1]; uq }, "does not match weights$member")
    refuses(function(uq) { uq$members <- NULL; uq }, "names no members")

    ## Calibration folds that name no retained fold model, fold 0 included
    refuses(function(uq) { uq$calib$fold[1] <- length(uq$fold_models) + 1L; uq },
            "references fold models that do not exist")
    refuses(function(uq) { uq$calib$fold[1] <- 0L; uq },
            "references fold models that do not exist")

  })

  it("predict_ensemble_intervals fails at its fold step when a fold model cannot predict", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")
    members  <- ens_ref$ensemble$weights$member
    new_spec <- resolve_new_data(test_set)
    mp       <- predict_members(ens_ref, members, new_spec)

    broken_uq <- ens_ref$ensemble$uq
    ## "weighted" fold models predict via member_mat %*% coef; a wrong-length
    ## coef (one row instead of one per member) makes that matrix multiply
    ## error with "non-conformable arguments". `$coef <- $coef[1]` alone
    ## would not do it: assigning a length-1 value into a tibble column
    ## recycles it back to the original row count instead of shortening it.
    broken_uq$fold_models[[1]] <- broken_uq$fold_models[[1]][1, ]

    out <- run_unit(predict_ensemble_intervals(broken_uq, mp), "intervals", "ensemble")

    expect_true(out$failed)
    expect_identical(out$records$step[out$records$type == "failure"], "cv_plus_folds")

  })

  it("predict.horizons_ensemble(interval = TRUE) keeps the predictions, leaves the bounds NA, and says so once", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")

    broken_ens <- ens_ref
    broken_ens$ensemble$uq$fold_models[[1]] <-
      broken_ens$ensemble$uq$fold_models[[1]][1, ]

    p <- NULL
    w <- expect_warning(
      p <- predict(broken_ens, test_set, interval = TRUE),
      class = "horizons_warning_skipped"
    )

    expect_match(conditionMessage(w),
                 "Prediction intervals (CV+) were not computed: CV+ fold predictions failed:",
                 fixed = TRUE)
    expect_true(all(is.finite(p$.pred)))
    expect_true(all(c(".pred_lower", ".pred_upper", ".interval_width") %in% names(p)))
    expect_true(all(is.na(p$.pred_lower)))

  })

  it("names the rows whose CV+ bounds are NA, keeping the rest (#145)", {

    test_set <- ens_test_set()
    ens_ref  <- ens_built("weighted")
    real     <- predict_fold_model

    ## The first fold model cannot predict the first new row
    calls <- 0L
    local_mocked_bindings(predict_fold_model = function(fold_model, method, member_mat, ...) {
      out <- real(fold_model, method, member_mat, ...)
      calls <<- calls + 1L
      if (calls == 1L) out[1] <- NA_real_
      out
    })

    w <- collect_warnings(p <- predict(ens_ref, test_set, interval = TRUE),
                          "CV\\+\\) were not computed for")$warnings

    expect_length(w, 1L)
    expect_s3_class(w[[1]], "horizons_warning_skipped")

    missing <- p$sample_id[is.na(p$.pred_lower)]
    expect_length(missing, 1L)
    expect_identical(w[[1]]$log$ids[[1]], missing)
    expect_true(all(is.finite(p$.pred)))

  })

})


## ---------------------------------------------------------------------------
## Review-fix regressions: shared indices
## ---------------------------------------------------------------------------

describe("cv_plus_indices()", {

  it("pins the finite-sample index formula for known (n, level) pairs", {

    ## n = 43 at level .90: l = floor(.05 * 44) = 2, u = ceiling(.95 * 44) = 42
    idx <- cv_plus_indices(43L, 0.90)
    expect_equal(idx$l, 2)
    expect_equal(idx$u, 42)

    ## n = 39: l = floor(.05 * 40) = 2, u = ceiling(.95 * 40) = 38
    idx <- cv_plus_indices(39L, 0.90)
    expect_equal(idx$l, 2)
    expect_equal(idx$u, 38)

    ## The leave-self-out path calls with n - 1; at n_calib = 43 that is
    ## n = 42: l = floor(.05 * 43) = 2, u = ceiling(.95 * 43) = 41.
    idx <- cv_plus_indices(42L, 0.90)
    expect_equal(idx$l, 2)
    expect_equal(idx$u, 41)

  })

  it("returns NULL below the validity threshold", {

    expect_null(cv_plus_indices(10L, 0.90))   # l = floor(.05 * 11) = 0
    expect_null(cv_plus_indices(5L, 0.95))

  })

})

## Fold ids outside the retained fold models are refused at predict()'s door
## (the test above), so cv_plus_bounds() no longer checks them.
