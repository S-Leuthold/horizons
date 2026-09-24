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

## =========================================================================
## predict.horizons_ensemble() — output contract
## =========================================================================
## Predicting the held-out Split-F test set is the natural round-trip: it is
## the same data the train-time combine scored, so predict() should reproduce
## the stored ensemble predictions. test_F also carries truth, but predict()
## must IGNORE it and return only sample_id + .pred (no config_id, no truth).

test_set <- rsample::assessment(fitted$models$split)

describe("predict.horizons_ensemble() - output contract", {

  it("returns sample_id + .pred only, one row per sample, for every method", {

    for (m in c("weighted", "penalized", "xgb")) {

      ens <- suppressWarnings(
        ensemble(fitted, method = m, optimize = FALSE, verbose = FALSE)
      )

      p <- predict(ens, test_set, interval = FALSE)

      expect_setequal(names(p), c("sample_id", ".pred"))
      expect_equal(nrow(p), dplyr::n_distinct(test_set$sample_id))
      expect_false("config_id" %in% names(p))
      expect_false(".pred_lower" %in% names(p))
      expect_type(p$.pred, "double")

    }

  })

})

## =========================================================================
## predict.horizons_ensemble() — round-trip reproduces stored predictions
## =========================================================================
## A round-trip consistency check: predicting test_F is the identical data path
## the engine used to build $ensemble$predictions, so the two must match to
## numerical precision. This catches a double back-transform, a member-order
## bug, a scale mismatch between predict() and the stored combine, or a wrong
## combine. It does NOT validate original-scale accuracy or interval coverage —
## both predict() and the stored values share the same machinery, so an error
## common to both would survive. optimize = FALSE keeps the meta-model
## deterministic across the fit and the predict path.

describe("predict.horizons_ensemble() - round-trips the stored predictions", {

  for (m in c("weighted", "penalized", "xgb")) {

    it(paste0("method = '", m, "' reproduces $ensemble$predictions on test_F"), {

      ens <- suppressWarnings(
        ensemble(fitted, method = m, optimize = FALSE, verbose = FALSE)
      )

      p      <- predict(ens, test_set, interval = FALSE)
      stored <- ens$ensemble$predictions

      joined <- dplyr::inner_join(
        p, stored[, c("sample_id", ".pred")],
        by = "sample_id", suffix = c("_new", "_stored")
      )

      expect_equal(nrow(joined), nrow(stored))
      expect_equal(joined$.pred_new, joined$.pred_stored, tolerance = 1e-8)

    })

  }

})

## =========================================================================
## predict.horizons_ensemble() — weighted combine equals the documented math
## =========================================================================
## Pin the weighted method to its definition (sum of member .pred * coef),
## computed independently of the function under test, so the assertion fails if
## the implementation drifts from the documented combination.

describe("predict.horizons_ensemble() - weighted combine is the documented sum", {

  it("equals sum(member .pred * coef) per sample, floored at 0", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    members  <- ens$ensemble$weights$member
    new_spec <- resolve_new_data(test_set)
    mp       <- predict_members(ens, members, new_spec)

    by_hand <- mp %>%
      dplyr::left_join(ens$ensemble$weights,
                       by = c("config_id" = "member")) %>%
      dplyr::group_by(.data$sample_id) %>%
      dplyr::summarise(by_hand = sum(.data$.pred * .data$coef),
                       .groups = "drop")

    by_hand$by_hand <- clamp_to_outcome_range(by_hand$by_hand)

    p <- predict(ens, test_set, interval = FALSE)

    joined <- dplyr::inner_join(p, by_hand, by = "sample_id")

    expect_equal(joined$.pred, joined$by_hand, tolerance = 1e-10)

  })

})

## =========================================================================
## predict.horizons_ensemble() — reuses the training-axis schema gate
## =========================================================================

describe("predict.horizons_ensemble() - schema gate", {

  it("aborts when new_data is missing training-axis predictor columns", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    pred_cols <- fitted$models$predictor_schema
    broken    <- test_set[, setdiff(names(test_set), pred_cols[1]),
                          drop = FALSE]

    expect_error(predict(ens, broken, interval = FALSE),
                 class = "rlang_error")

  })

})

## =========================================================================
## predict.horizons_ensemble() — intervals present by default, degrade cleanly
## =========================================================================
## ensemble() calibrates CV+ UQ by default, so interval = TRUE returns interval
## columns. The graceful-degradation path (point-only + one-time note) now
## belongs to compute_uq = FALSE builds and corrupt bundles.

describe("predict.horizons_ensemble() - intervals and degradation", {

  it("default build returns interval columns with no note", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    expect_no_message(p <- predict(ens, test_set, interval = TRUE))

    expect_true(all(c(".pred_lower", ".pred_upper", ".interval_width") %in%
                      names(p)))
    expect_true(all(p$.pred_upper >= p$.pred_lower))
    expect_true(all(p$.pred_lower >= 0))

  })

  it("compute_uq = FALSE returns point-only with an informative message", {

    ens0 <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE,
               compute_uq = FALSE, verbose = FALSE)
    )

    expect_message(
      p <- predict(ens0, test_set, interval = TRUE),
      regexp = "intervals are not available"
    )
    expect_false(".pred_lower" %in% names(p))

  })

  it("a corrupt uq bundle warns and degrades to point-only without erroring", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    ens$ensemble$uq$method <- "bogus"

    ## #65: predict_ensemble_intervals() used to degrade silently here; it now
    ## warns, naming the bundle as unrecognized, via warn_interval_failure().
    p <- NULL
    expect_warning(
      p <- predict(ens, test_set, interval = TRUE),
      class = "horizons_interval_warning"
    )

    expect_false(".pred_lower" %in% names(p))
    expect_true(".pred" %in% names(p))

  })

  it("interval = FALSE returns point-only with no message", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    expect_no_message(predict(ens, test_set, interval = FALSE))

  })

})

## =========================================================================
## predict.horizons_ensemble() — aborts when a member cannot predict
## =========================================================================
## The meta-learner's combine is only valid over the exact member set it was
## trained on. If a member's workflow cannot predict, the call must abort naming
## the failure rather than dropping the member and silently changing the
## estimand. Corrupt one member's stored workflow to force the failure.

describe("predict.horizons_ensemble() - aborts on a failing member", {

  it("does not renormalize; aborts when a member workflow cannot predict", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    broken_member <- ens$ensemble$weights$member[1]
    ens$models$workflows[[broken_member]] <- "not a workflow"

    expect_error(predict(ens, test_set, interval = FALSE),
                 class = "rlang_error")

  })

})

## =========================================================================
## Members' applicability domain is not computed
## =========================================================================
## The member helpers keep only .pred, so a member's AD was baked, scored and
## dropped: wasted work, and AD warnings about columns the ensemble never
## returns. A member AD bundle that no longer matches the member's features
## makes the distance fail, which is what would warn if it were computed.

describe("ensemble() and predict.horizons_ensemble() - member AD", {

  it("raises no AD warning from a broken member AD bundle", {

    broken <- fitted
    member <- names(broken$models$workflows)[1]

    broken$models$ad <- stats::setNames(list(list(
      centroid      = c(wn_0 = 0),
      cov_matrix    = matrix(1, dimnames = list("wn_0", "wn_0")),
      ad_thresholds = c(q25 = 1, q50 = 2, q75 = 3, ood = 4)
    )), member)

    expect_no_warning(
      ens <- keep_only_warning(
        ensemble(broken, method = "weighted", optimize = FALSE, verbose = FALSE),
        "horizons_ad_warning"
      ),
      class = "horizons_ad_warning"
    )

    expect_no_warning(
      keep_only_warning(predict(ens, test_set, interval = FALSE), "horizons_ad_warning"),
      class = "horizons_ad_warning"
    )

  })

})

## =========================================================================
## predict.horizons_ensemble() — preflight
## =========================================================================

describe("predict.horizons_ensemble() - preflight", {

  it("aborts on a non-ensemble object", {

    expect_error(predict.horizons_ensemble(fitted, test_set),
                 class = "rlang_error")

  })

})

## =========================================================================
## predict.horizons_ensemble() — conformal coverage on a selected training set
## =========================================================================
## Same warning, same conditions, same wording as predict.horizons_fit(): the
## ensemble's CV+ intervals inherit the calibration rows' non-exchangeability
## exactly as the member intervals do. Both paths call
## warn_selection_intervals(), so the two cannot drift apart.

describe("predict.horizons_ensemble() - selected training set", {

  ens_selected <- function(uq = TRUE) {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE,
               compute_uq = uq, verbose = FALSE)
    )

    ens$models$selection_present <- TRUE
    ens

  }

  it("warns once when intervals are requested on a selected ensemble", {

    ens <- ens_selected()

    warns <- testthat::capture_warnings(
      p <- suppressMessages(predict(ens, test_set, interval = TRUE))
    )

    expect_equal(sum(grepl("Conformal coverage is not guaranteed", warns)), 1L)
    expect_true(any(grepl("target_distances", warns)))
    expect_equal(nrow(p), dplyr::n_distinct(test_set$sample_id))

  })

  it("is silent when intervals are not requested", {

    warns <- testthat::capture_warnings(
      predict(ens_selected(), test_set, interval = FALSE)
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

  it("is silent with no ensemble UQ, where no intervals are returned", {

    warns <- testthat::capture_warnings(
      suppressMessages(predict(ens_selected(uq = FALSE), test_set,
                               interval = TRUE))
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

  it("is silent on an unselected ensemble", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    warns <- testthat::capture_warnings(
      suppressMessages(predict(ens, test_set, interval = TRUE))
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

})

## =========================================================================
## predict.horizons_ensemble() - members fit on a response-trimmed partition
## =========================================================================
## The ensemble's CV+ intervals come from the members' out-of-fold
## predictions, which after a response trim (#77) cover only the rows inside
## the training fences. The mechanism is a follow-up; predict() says so.

describe("predict.horizons_ensemble() - trimmed members (#77)", {

  trimmed_ensemble <- function(uq = TRUE, trimmed_ids = c("S001", "S002")) {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE,
               compute_uq = uq, verbose = FALSE)
    )

    ens$evaluation$response_trim <- list(
      outcome = "SOC", method = "iqr", threshold = 1.5, fences_from = "training",
      lower = 0.5, upper = 4.5, n_training = 40L, trimmed_ids = trimmed_ids,
      skipped = NA_character_
    )

    ens

  }

  it("warns once that the intervals were calibrated within the training fences", {

    caught <- list()

    withCallingHandlers(
      suppressMessages(predict(trimmed_ensemble(), test_set, interval = TRUE)),
      warning = function(w) {
        caught[[length(caught) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    )

    trim_warnings <- Filter(function(w) inherits(w, "horizons_response_trim_warning"), caught)

    expect_length(trim_warnings, 1L)
    expect_match(conditionMessage(trim_warnings[[1]]),
                 "calibrated within the training fences [0.5, 4.5]", fixed = TRUE)

  })

  it("is silent without intervals, without ensemble UQ, or without a trim", {

    quiet <- function(ens, interval) {
      warns <- testthat::capture_warnings(
        suppressMessages(predict(ens, test_set, interval = interval))
      )
      !any(grepl("training fences", warns))
    }

    expect_true(quiet(trimmed_ensemble(), interval = FALSE))
    expect_true(quiet(trimmed_ensemble(uq = FALSE), interval = TRUE))
    expect_true(quiet(trimmed_ensemble(trimmed_ids = character(0)), interval = TRUE))

  })

})

## =========================================================================
## predict.horizons_ensemble() — a malformed ensemble is diagnosed as such
## =========================================================================
## The member-set gate runs before the covariate resolution, so an ensemble
## with no members reports the missing member set rather than something about
## covariates derived from that same empty set.

describe("predict.horizons_ensemble() - member-set gate order", {

  it("reports the missing member set, not a covariate problem", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    ens$ensemble$weights <- ens$ensemble$weights[0, ]

    expect_error(predict(ens, test_set, interval = FALSE),
                 "carries no member set")

  })

})

## =========================================================================
## predict.horizons_ensemble() — response bound guardrail at the OUTPUT
## =========================================================================
## Members predict UNCLAMPED (their predictions are meta-learner features and
## must match the raw member OOF the meta trained/calibrated on); the
## guardrail applies exactly once, to the combined ensemble output. Build-time
## test_F scoring (metrics/member_metrics/improvement) is also unclamped, so
## ranking sees raw model behavior.

describe("predict.horizons_ensemble() - response bound guardrail", {

  it("clamps the combined output exactly once, one row per sample", {

    ens <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, verbose = FALSE)
    )

    p_raw <- predict(ens, test_set, interval = FALSE)
    bound <- stats::median(p_raw$.pred)   # force the clamp

    ens$models$response_bound <- bound

    ## Output-level clamp: exactly ONE winsorization warning (not one per
    ## member) — the single-warning count is the regression assertion that
    ## members stayed raw.
    warns <- character()
    p <- withCallingHandlers(
      predict(ens, test_set, interval = FALSE),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_equal(sum(grepl("winsorized", warns)), 1)
    expect_true(all(p$.pred <= bound + 1e-10))
    expect_equal(nrow(p), dplyr::n_distinct(test_set$sample_id))

  })

  it("member features and build-time scoring stay unclamped", {

    ## Inject a bound BEFORE building the ensemble: build-time member scoring
    ## must not warn (raw path), and the stored member_metrics must equal the
    ## metrics of raw member predictions.
    low_fit <- fitted
    low_fit$models$response_bound <- 1e-3   # would clamp everything if applied

    expect_no_warning(
      ens <- ensemble(low_fit, method = "weighted", optimize = FALSE,
                      compute_uq = FALSE, verbose = FALSE)
    )

    ## Stored test_F predictions are raw combine output (values far above the
    ## injected bound prove no member was clamped during the build).
    expect_true(any(ens$ensemble$predictions$.pred > 1e-3))

  })

})

## =========================================================================
## validate_horizons_ensemble() — real contracts from all three engines pass
## =========================================================================
## The validator is wired into ensemble() itself, so these builds double as
## wire-up proof; the explicit calls assert idempotent re-validation of a
## real contract (including a populated CV+ uq bundle).

describe("validate_horizons_ensemble() - real fixture contracts", {

  for (m in c("weighted", "penalized", "xgb")) {

    it(paste0("method = '", m, "' contract validates (uq populated)"), {

      ens <- suppressWarnings(
        ensemble(fitted, method = m, optimize = FALSE, verbose = FALSE)
      )

      expect_identical(validate_horizons_ensemble(ens), ens)
      expect_false(is.null(ens$ensemble$uq))

    })

  }

  it("a compute_uq = FALSE contract validates (uq NULL)", {

    ens0 <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE,
               compute_uq = FALSE, verbose = FALSE)
    )

    expect_identical(validate_horizons_ensemble(ens0), ens0)

  })

})


## =========================================================================
## allow_par is explicit and off by default (M2e, 2026-09-15)
## =========================================================================
## tune's control constructors default to allow_par = TRUE, so before this
## the meta-learner tuning and OOF pass dispatched onto any registered
## future::plan() with no argument to say otherwise.

describe("ensemble() - allow_par", {

  it("builds both tune controls with allow_par = FALSE by default", {

    grid <- horizons:::meta_tune_control("grid")
    oof  <- horizons:::meta_tune_control("resamples")

    expect_false(grid$allow_par)
    expect_false(oof$allow_par)
    expect_equal(grid$parallel_over, "resamples")
    expect_equal(oof$parallel_over, "resamples")

  })

  it("does not dispatch onto a registered plan unless asked", {

    skip_on_cran()
    local_plan(future::multisession, workers = 2)

    ## tune's own framework choice for the control ensemble() builds: with
    ## allow_par = FALSE it is sequential even though a plan is registered.
    expect_equal(tune:::choose_framework(control = horizons:::meta_tune_control("grid")),
                 "sequential")
    expect_equal(tune:::choose_framework(control = horizons:::meta_tune_control("grid", TRUE)),
                 "future")

  })

  it("warns and runs sequentially when allow_par = TRUE has no backend", {

    local_plan(future::sequential)

    expect_warning(
      ens <- keep_only_warning(
        ensemble(fitted, method = "penalized", optimize = FALSE,
                 allow_par = TRUE, compute_uq = FALSE, verbose = FALSE),
        "offers 1 worker"
      ),
      "ensemble\\(\\).*offers 1 worker"
    )

    expect_true(inherits(ens, "horizons_ensemble"))

  })

})
