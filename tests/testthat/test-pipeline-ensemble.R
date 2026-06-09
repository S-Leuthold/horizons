## ---------------------------------------------------------------------------
## Tests: ensemble()
## ---------------------------------------------------------------------------
## Integration tests for the ensemble() pipeline verb and its three meta-learner
## engines (penalized / weighted / xgb).
##
## Fixture: tests/testthat/fixtures/ensemble_fit.rds — a small horizons_fit on
## REAL (anonymized) AONR MIR spectra predicting Bulk_C. Real signal matters:
## a degenerate fixture (members that don't predict) would make every
## "did the meta-learner learn something" assertion vacuously pass. The fixture
## also spans THREE response transforms (log / sqrt / none) across its members
## so the back-transform double-application regression is actually exercisable —
## under transformation = "none" the inverse is identity and a double-apply is
## invisible.
##
## Built once by dev-build-fixture.R; see fixtures/README.md.

fitted <- readRDS(test_path("fixtures", "ensemble_fit.rds"))

## Sanity: the fixture is what the tests assume (real signal, >= 2 members,
## transform diversity). If this block fails, rebuild the fixture — the tests
## below are only meaningful on top of it.
describe("ensemble() fixture preconditions", {

  it("is a horizons_fit with at least 2 members carrying cv_predictions", {

    expect_true(inherits(fitted, "horizons_fit"))
    expect_gte(length(unique(fitted$models$cv_predictions$config_id)), 2)

  })

  it("spans more than one response transform across members", {

    members      <- unique(fitted$models$cv_predictions$config_id)
    member_cfg   <- fitted$config$configs[
      fitted$config$configs$config_id %in% members, ]
    member_trans <- unique(member_cfg$transformation)

    ## At least one non-identity transform must be present, or the §9
    ## double-back-transform regression below is vacuous.
    expect_gt(length(member_trans), 1)
    expect_true(any(member_trans != "none"))

  })

})

## =========================================================================
## TEETH #1 — non-degeneracy: the meta-learner learns a real combination
## =========================================================================
## On real signal the penalized engine must assign non-zero member weights;
## intercept-only (all coefs 0) is the degenerate failure the synthetic data
## produced and is NOT acceptable here.

describe("ensemble() penalized - non-degenerate on real signal", {

  ens <- suppressWarnings(
    ensemble(fitted, method = "penalized", optimize = TRUE, verbose = FALSE)
  )

  it("learns at least one non-zero member coefficient", {

    coefs <- ens$ensemble$weights$coef
    expect_gt(sum(abs(coefs) > 1e-8), 0)

  })

  it("produces ensemble test predictions correlated with truth", {

    ep <- ens$ensemble$predictions
    expect_gt(stats::cor(ep$.pred, ep$truth), 0.3)

  })

})

## =========================================================================
## TEETH #2 — the .row -> oof$row mapping is correct, not just shaped
## =========================================================================
## collect_predictions()$.row indexes meta_frame positions; the engine maps it
## back to the object's true .row. A row-count check passes on a fully
## scrambled .pred vector, so we instead (a) re-derive the OOF independently and
## (b) assert the OOF predictions correlate with truth — a scramble breaks both.
## Run at optimize = FALSE so there is no tuning (fast, deterministic spec).

describe("ensemble() oof_predictions - correct row alignment", {

  ens <- suppressWarnings(
    ensemble(fitted, method = "penalized", optimize = FALSE, verbose = FALSE)
  )
  oof <- ens$ensemble$oof_predictions

  it("has one OOF prediction per training row, keyed to the object's .row", {

    members <- unique(fitted$models$cv_predictions$config_id)
    truth_rows <- unique(
      fitted$models$cv_predictions[
        fitted$models$cv_predictions$config_id == members[1],
        c(".row", "truth")
      ]
    )
    expect_setequal(oof$.row, truth_rows$.row)

  })

  it("carries the truth value that belongs to each .row (not a scramble)", {

    members <- unique(fitted$models$cv_predictions$config_id)
    ref <- unique(
      fitted$models$cv_predictions[
        fitted$models$cv_predictions$config_id == members[1],
        c(".row", "truth")
      ]
    )
    joined <- merge(oof[, c(".row", "truth")], ref, by = ".row",
                    suffixes = c("_oof", "_ref"))
    expect_equal(joined$truth_oof, joined$truth_ref)

  })

  it("OOF predictions correlate with truth (a scrambled .pred would not)", {

    expect_gt(stats::cor(oof$.pred, oof$truth), 0.3)

  })

})

## =========================================================================
## TEETH #3 — no double back-transform (§9 regression)
## =========================================================================
## Members are fit under log / sqrt / none. If any stage back-transforms an
## already-back-transformed prediction, values explode out of physical range.
## Bulk_C (g/kg) is a small positive quantity; assert ensemble + OOF predictions
## stay in a sane range and are non-negative.

describe("ensemble() - no double back-transformation", {

  ens <- suppressWarnings(
    ensemble(fitted, method = "penalized", optimize = FALSE, verbose = FALSE)
  )

  it("ensemble test predictions stay in physical range for Bulk_C", {

    p <- ens$ensemble$predictions$.pred
    expect_true(all(p >= 0))
    expect_true(all(p < 1000))   # Bulk_C g/kg is O(10); 1000 = clear blow-up

  })

  it("OOF predictions stay in physical range", {

    p <- ens$ensemble$oof_predictions$.pred
    expect_true(all(p >= 0))
    expect_true(all(p < 1000))

  })

})

## =========================================================================
## REGRESSION GUARD — the lazy-quosure / "2 arguments tagged for tuning" bug
## =========================================================================
## optimize = FALSE is the path that failed (FIT_REVIEW_FINDINGS I1): an inline
## `if (optimize) tune() else <value>` inside the parsnip spec call left a lazy
## quosure that tune_args/fit_resamples misread as tunable, aborting
## fit_resamples. The fix builds the spec with an explicit if/else BRANCH. These
## cases guard it — if anyone re-inlines the conditional, optimize = FALSE breaks
## here. Do NOT replace this with a reimplementation of the spec construction;
## drive it through the real engines.

describe("ensemble() - optimize = FALSE runs (lazy-quosure regression guard)", {

  it("penalized fixed-spec path completes and yields genuine OOF", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "penalized", optimize = FALSE, verbose = FALSE)
    )
    expect_true(inherits(ens, "horizons_ensemble"))
    expect_equal(nrow(ens$ensemble$oof_predictions),
                 length(unique(fitted$models$cv_predictions$.row)))

  })

  it("xgb fixed-spec path completes", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "xgb", optimize = FALSE, verbose = FALSE)
    )
    expect_true(inherits(ens, "horizons_ensemble"))

  })

})

## =========================================================================
## SMOKE MATRIX — all three engines x both modes dispatch and promote class
## =========================================================================

describe("ensemble() - engine x mode dispatch", {

  for (method in c("penalized", "weighted", "xgb")) {

    for (optimize in c(TRUE, FALSE)) {

      it(paste0(method, " optimize=", optimize, " returns a horizons_ensemble"), {

        ens <- suppressWarnings(
          ensemble(fitted, method = method, optimize = optimize, verbose = FALSE)
        )
        expect_true(inherits(ens, "horizons_ensemble"))
        expect_true(inherits(ens, "horizons_fit"))
        expect_false(is.null(ens$ensemble))

      })

    }

  }

})

## =========================================================================
## PREFLIGHT — input validation
## =========================================================================

describe("ensemble() - preflight validation", {

  it("aborts on non-horizons_fit input", {

    expect_error(ensemble(list(a = 1)), class = "rlang_error")

  })

  it("aborts on an unknown method", {

    expect_error(ensemble(fitted, method = "bogus"), class = "rlang_error")

  })

})
