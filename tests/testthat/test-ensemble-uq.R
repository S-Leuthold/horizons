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
## ---------------------------------------------------------------------------

fitted   <- readRDS(test_path("fixtures", "ensemble_fit.rds"))
test_set <- rsample::assessment(fitted$models$split)

outcome_col <- fitted$data$role_map$variable[
  fitted$data$role_map$role == "outcome"
]

## One reference ensemble reused across read-only tests (weighted: fast,
## deterministic).
ens_ref <- suppressWarnings(
  ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
)


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

  uq <- ens_ref$ensemble$uq

  it("populates $ensemble$uq with the exact field set", {

    expect_setequal(
      names(uq),
      c("method", "ensemble_method", "members", "fold_models", "calib",
        "n_calib", "level_default", "seed", "oof_coverage", "mean_width",
        "timestamp")
    )

  })

  it("carries the CV+ discriminator and build facts", {

    expect_identical(uq$method, "cv_plus")
    expect_identical(uq$ensemble_method, "weighted")
    expect_equal(length(uq$fold_models), 5)
    expect_equal(uq$level_default, DEFAULT_UQ_LEVEL)
    expect_equal(uq$seed, 307L + 1000L)
    expect_setequal(uq$members, ens_ref$ensemble$weights$member)

  })

  it("calib carries signed original-scale residuals for every meta row", {

    expect_setequal(names(uq$calib),
                    c(".row", "fold", ".pred_oof", "truth", "residual"))
    expect_equal(uq$n_calib, nrow(uq$calib))
    expect_true(any(uq$calib$residual < 0) && any(uq$calib$residual > 0))
    expect_equal(uq$calib$residual, uq$calib$truth - uq$calib$.pred_oof)

  })

  it("records optimize and seed on the ensemble contract", {

    expect_identical(ens_ref$ensemble$optimize, FALSE)
    expect_identical(ens_ref$ensemble$seed, 307L)

  })

})


## ---------------------------------------------------------------------------
## Fold honesty (the 26x-overconfidence regression) + Route B disjointness
## ---------------------------------------------------------------------------

describe("fit_ensemble_uq() - fold honesty and Route B", {

  it("calib partitions every meta row into exactly one fold", {

    uq  <- ens_ref$ensemble$uq
    oof <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    expect_setequal(uq$calib$.row, oof$row)
    expect_equal(anyDuplicated(uq$calib$.row), 0L)
    expect_true(all(uq$calib$fold %in% 1:5))

  })

  it("the stored seed reproduces the fold assignment exactly", {

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
    ens_xgb <- suppressWarnings(
      ensemble(fitted, method = "xgb", optimize = FALSE, verbose = FALSE)
    )

    uq  <- ens_xgb$ensemble$uq
    oof <- build_oof_matrix(ens_xgb, ens_xgb$ensemble$weights$member)

    meta_frame <- oof$predictors

    in_sample_pred <- floor_at_zero(
      stats::predict(ens_xgb$ensemble$model, new_data = meta_frame)$.pred
    )
    in_sample_resid <- oof$truth - in_sample_pred

    calib_resid <- uq$calib$residual[match(oof$row, uq$calib$.row)]

    expect_false(isTRUE(all.equal(calib_resid, in_sample_resid)))
    expect_gte(mean(abs(calib_resid)), mean(abs(in_sample_resid)))

  })

  it("the calibration partition differs from the tuning partition (Route B)", {

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
    expect_gt(ens_ref$ensemble$uq$oof_coverage, 0.70)
    expect_lte(ens_ref$ensemble$uq$oof_coverage, 1.00)
    expect_gt(ens_ref$ensemble$uq$mean_width, 0)

  })

  it("end-to-end coverage on the held-out test set is plausible", {

    ## Only 12 held-out points — a loose sanity band, not a coverage claim.
    p <- suppressWarnings(predict(ens_ref, test_set, interval = TRUE))

    joined <- dplyr::inner_join(
      p,
      tibble::tibble(sample_id = test_set$sample_id,
                     truth     = test_set[[outcome_col]]),
      by = "sample_id"
    )

    covered <- mean(joined$truth >= joined$.pred_lower &
                      joined$truth <= joined$.pred_upper)

    expect_gte(covered, 0.60)

  })

})


## ---------------------------------------------------------------------------
## Predict side: sample_id join, ordering, method x optimize grid
## ---------------------------------------------------------------------------

describe("predict.horizons_ensemble() - CV+ intervals", {

  it("interval columns key by sample_id: shuffled rows give identical bounds", {

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

    p <- suppressWarnings(predict(ens_ref, test_set, interval = TRUE))

    expect_true(all(p$.pred_upper >= p$.pred_lower))
    expect_true(all(p$.pred_lower >= 0))
    expect_equal(p$.interval_width, p$.pred_upper - p$.pred_lower)

  })

  for (m in c("weighted", "penalized", "xgb")) {

    for (opt in c(TRUE, FALSE)) {

      it(paste0("method = '", m, "', optimize = ", opt,
                " produces a bundle and interval columns"), {

        ens <- suppressWarnings(
          ensemble(fitted, method = m, optimize = opt, verbose = FALSE)
        )

        expect_false(is.null(ens$ensemble$uq))
        expect_identical(ens$ensemble$uq$method, "cv_plus")

        p <- suppressWarnings(predict(ens, test_set, interval = TRUE))

        expect_true(all(c(".pred_lower", ".pred_upper") %in% names(p)))
        expect_true(all(p$.pred_upper >= p$.pred_lower, na.rm = TRUE))

      })

    }

  }

  it("weighted optimize = FALSE fold models are equal weights summing to 1", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    for (fm in ens$ensemble$uq$fold_models) {

      expect_s3_class(fm, "tbl_df")
      expect_equal(sum(fm$coef), 1)
      expect_true(all(abs(fm$coef - fm$coef[1]) < 1e-12))

    }

  })

})


## ---------------------------------------------------------------------------
## Degradation and gates
## ---------------------------------------------------------------------------

describe("ensemble UQ - degradation and gates", {

  it("compute_ensemble_uq returns NULL below N_CALIB_MIN", {

    oof <- build_oof_matrix(ens_ref, ens_ref$ensemble$weights$member)

    keep <- seq_len(N_CALIB_MIN - 5L)

    oof_small <- list(
      predictors = oof$predictors[keep, , drop = FALSE],
      truth      = oof$truth[keep],
      row        = oof$row[keep],
      members    = oof$members
    )

    result <- compute_ensemble_uq(
      oof            = oof_small,
      contract       = ens_ref$ensemble,
      optimize       = FALSE,
      conformal_seed = 1307L
    )

    expect_null(result)

  })

  it("predict_ensemble_intervals returns NULL for an unrecognized bundle", {

    expect_null(predict_ensemble_intervals(list(method = "bogus"), NULL))
    expect_null(predict_ensemble_intervals("not a list", NULL))

  })

  it("fit_ensemble_uq validates its inputs", {

    expect_error(fit_ensemble_uq(fitted), class = "rlang_error")
    expect_error(fit_ensemble_uq(ens_ref, level = 1.5), class = "rlang_error")

  })

})


## ---------------------------------------------------------------------------
## Review-fix regressions: shared indices, floor semantics, legacy retrofit
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

describe("predict_fold_model()", {

  it("floors negative fold predictions to zero (deploy-consistency semantics)", {

    ## A weights tibble with a negative coefficient can produce negative
    ## combos; the deployed combine floors, so the fold primitive must too —
    ## conformal validity requires the calibration score function to match
    ## the deployed prediction rule.
    fm <- tibble::tibble(member = c("a", "b"), coef = c(1, -2))
    member_mat <- tibble::tibble(member_a = c(1, 5), member_b = c(2, 1))

    pred <- predict_fold_model(fm, "weighted", member_mat)

    expect_equal(pred, c(0, 3))   # 1 - 4 = -3 -> floored; 5 - 2 = 3

  })

})

describe("cv_plus_bounds() - corrupted fold ids fail loudly", {

  it("aborts when fold ids exceed the retained fold models", {

    expect_error(
      cv_plus_bounds(
        matrix(rnorm(10), nrow = 2),          # 5 fold-model columns
        rep(c(1, 7), length.out = 39),        # fold 7 does not exist
        rnorm(39),
        level = 0.90
      ),
      "out of sync"
    )

  })

})

describe("fit_ensemble_uq() - legacy retrofit", {

  it("infers optimize = FALSE from equal weights when the contract predates the field", {

    ## Simulate a pre-contract object: equal-weights ensemble with the
    ## optimize/seed keys removed entirely (the legacy serialized shape).
    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE,
               compute_uq = FALSE, verbose = FALSE)
    )

    ens$ensemble$optimize <- NULL   # $<- NULL removes the key
    ens$ensemble$seed     <- NULL

    retro <- suppressWarnings(fit_ensemble_uq(ens, verbose = FALSE))

    expect_false(is.null(retro$ensemble$uq))

    ## The inference must have re-derived EQUAL fold weights — a blind
    ## optimize = TRUE default would produce inverse-RMSE (unequal) weights,
    ## a silent deployed-vs-fold-model rule mismatch.
    for (fm in retro$ensemble$uq$fold_models) {

      expect_true(all(abs(fm$coef - fm$coef[1]) < 1e-12))

    }

  })

})
