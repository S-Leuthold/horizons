## ---------------------------------------------------------------------------
## Tests: ensemble()
## ---------------------------------------------------------------------------
## Integration tests for the ensemble() pipeline verb and its three meta-learner
## engines (penalized / weighted / xgb).
##
## Fixture: tests/testthat/fixtures/ensemble_fit.rds — a small horizons_fit on
## REAL (anonymized) MIR spectra predicting Bulk_C. Real signal matters:
## a degenerate fixture (members that don't predict) would make every
## "did the meta-learner learn something" assertion vacuously pass. The fixture
## also spans THREE response transforms (log / sqrt / none) across its members
## so the back-transform double-application regression is actually exercisable —
## under transformation = "none" the inverse is identity and a double-apply is
## invisible.
##
## Built once by dev-build-fixture.R; see fixtures/README.md.
##
## ens_fitted(), ens_test_set() and ens_built() come from helper-ensemble.R:
## the fixture is read once, and each ensemble the tests only read is built
## once, on first use.

## Sanity: the fixture is what the tests assume (real signal, >= 2 members,
## transform diversity). If this block fails, rebuild the fixture — the tests
## below are only meaningful on top of it.
describe("ensemble() fixture preconditions", {

  it("is a horizons_fit with at least 2 members carrying cv_predictions", {

    fitted <- ens_fitted()

    expect_true(inherits(fitted, "horizons_fit"))
    expect_gte(length(unique(fitted$models$cv_predictions$config_id)), 2)

  })

  it("spans more than one response transform across members", {

    fitted       <- ens_fitted()
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
## TEETH — the .row -> oof$row mapping is correct, not just shaped
## =========================================================================
## collect_predictions()$.row indexes meta_frame positions; the engine maps it
## back to the object's true .row. A row-count check passes on a fully
## scrambled .pred vector, so we instead (a) re-derive the OOF independently and
## (b) assert the OOF predictions correlate with truth — a scramble breaks both.
## Run at optimize = FALSE so there is no tuning (fast, deterministic spec).

describe("ensemble() oof_predictions - correct row alignment", {

  it("has one OOF prediction per training row, keyed to the object's .row", {

    fitted  <- ens_fitted()
    oof     <- ens_built("penalized")$ensemble$oof_predictions
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

    fitted  <- ens_fitted()
    oof     <- ens_built("penalized")$ensemble$oof_predictions
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

    oof <- ens_built("penalized")$ensemble$oof_predictions

    expect_gt(stats::cor(oof$.pred, oof$truth), 0.3)

  })

})

## =========================================================================
## TEETH — no double back-transform (§9 regression)
## =========================================================================
## Members are fit under log / sqrt / none. If any stage back-transforms an
## already-back-transformed prediction, values explode out of physical range.
## Bulk_C (g/kg) is a small positive quantity; assert ensemble + OOF predictions
## stay in a sane range and are non-negative.

describe("ensemble() - no double back-transformation", {

  it("ensemble test predictions stay in physical range for Bulk_C", {

    ens <- ens_built("penalized")
    p   <- ens$ensemble$predictions$.pred
    expect_true(all(p >= 0))
    expect_true(all(p < 1000))   # Bulk_C g/kg is O(10); 1000 = clear blow-up

  })

  it("OOF predictions stay in physical range", {

    ens <- ens_built("penalized")
    p   <- ens$ensemble$oof_predictions$.pred
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
## drive it through the real engines. The builds are the shared ones the other
## tests read, made on first use inside a test, so a regression fails these
## two by name (and every other test that reads them), not the file.

describe("ensemble() - optimize = FALSE runs (lazy-quosure regression guard)", {

  it("penalized fixed-spec path completes and yields genuine OOF", {

    fitted <- ens_fitted()
    ens    <- ens_built("penalized")
    expect_true(inherits(ens, "horizons_ensemble"))
    expect_equal(nrow(ens$ensemble$oof_predictions),
                 length(unique(fitted$models$cv_predictions$.row)))

  })

  it("xgb fixed-spec path completes", {

    ens <- ens_built("xgb")
    expect_true(inherits(ens, "horizons_ensemble"))

  })

})

## =========================================================================
## PREFLIGHT — input validation
## =========================================================================

describe("ensemble() - preflight validation", {

  it("aborts on non-horizons_fit input", {

    ## The abort carries no package class. validate_horizons_fit() refuses
    ## the same input with the same headline but no hint, so the hint is what
    ## shows this check fired.
    err <- expect_error(ensemble(list(a = 1)),
                        "`x` must be a <horizons_fit> object", fixed = TRUE)
    expect_match(conditionMessage(err), "Run `fit()` first to produce a fitted object",
                 fixed = TRUE)

  })

  it("aborts on an unknown method", {

    fitted <- ens_fitted()

    ## The abort carries no package class.
    expect_error(ensemble(fitted, method = "bogus"),
                 "`method` must be one of", fixed = TRUE)

  })

  it("refuses a fitted object that breaks its contract, before fitting (#129)", {

    fitted <- ens_fitted()

    ## ensemble() used to check only the class and the outcome range, so
    ## each of these reached the meta-learner.

    ## The base contract: the id role moved off sample_id
    off_id <- fitted
    rm     <- off_id$data$role_map
    rm$role[rm$variable == "sample_id"] <- "meta"
    rm$role[rm$variable == "project"]   <- "id"
    off_id$data$role_map <- rm

    expect_error(
      suppressMessages(capture.output(
        ensemble(off_id, method = "weighted", optimize = FALSE,
                 compute_uq = FALSE, verbose = FALSE)
      )),
      "must be on sample_id",
      class = "horizons_validation_error"
    )

    ## The fit contract: a models slot missing a key
    no_ad <- fitted
    no_ad$models$ad <- NULL

    expect_error(
      suppressMessages(capture.output(
        ensemble(no_ad, method = "weighted", optimize = FALSE,
                 compute_uq = FALSE, verbose = FALSE)
      )),
      "missing from models: ad",
      class = "horizons_validation_error"
    )

  })

  it("refuses a bad seed or optimize before any engine runs (#130)", {

    fitted <- ens_fitted()

    ## Both are recorded on the ensemble and must not be NULL there, so a bad
    ## value is refused on entry rather than by the validator once the
    ## meta-learner is built.
    local_mocked_bindings(
      fit_ensemble_weighted = function(...) stop("fit_ensemble_weighted() was called")
    )

    bad_seeds <- list(NULL, NA_real_, c(1, 2), "307")

    for (s in bad_seeds) {

      expect_error(
        ensemble(fitted, method = "weighted", optimize = FALSE, seed = s,
                 compute_uq = FALSE, verbose = FALSE),
        "seed",
        class = "horizons_input_error"
      )

    }

    bad_optimize <- list(NULL, NA, c(TRUE, FALSE), "yes")

    for (o in bad_optimize) {

      expect_error(
        ensemble(fitted, method = "weighted", optimize = o,
                 compute_uq = FALSE, verbose = FALSE),
        "optimize",
        class = "horizons_input_error"
      )

    }

  })

})

describe("ensemble() - predict namespaces", {

  it("loads the members' predict namespaces before scoring them on the test rows", {

    fitted <- ens_fitted()

    ## Under an installed package the Imports load lazily, so a fitted object
    ## read into a fresh session has no workflows predict method registered
    ## until something loads it. The member scoring must ask for it, as
    ## predict.horizons_ensemble() does.
    requested <- NULL
    local_mocked_bindings(
      ensure_predict_namespaces = function(object, config_ids) {
        requested <<- config_ids
        invisible(NULL)
      }
    )

    members <- fitted$models$best_config
    invisible(tryCatch(predict_members_on_test(fitted, members), error = function(e) NULL))

    expect_identical(requested, members)

  })

})

## =========================================================================
## predict.horizons_ensemble() — output contract
## =========================================================================
## Predicting the held-out Split-F test set is the natural round-trip: it is
## the same data the train-time combine scored, so predict() should reproduce
## the stored ensemble predictions. test_F also carries truth, but predict()
## must IGNORE it and return only sample_id + .pred (no config_id, no truth).

describe("predict.horizons_ensemble() - output contract", {

  it("returns sample_id + .pred only, one row per sample, for every method", {

    test_set <- ens_test_set()

    for (m in c("weighted", "penalized", "xgb")) {

      ens <- ens_built(m)

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

      test_set <- ens_test_set()
      ens      <- ens_built(m)

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

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

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

    fitted   <- ens_fitted()
    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    pred_cols <- fitted$models$predictor_schema
    broken    <- test_set[, setdiff(names(test_set), pred_cols[1]),
                          drop = FALSE]

    ## The abort carries no package class.
    expect_error(predict(ens, broken, interval = FALSE),
                 "is missing 1 predictor column the model expects", fixed = TRUE)

  })

})

## =========================================================================
## predict.horizons_ensemble() — intervals present by default, degrade cleanly
## =========================================================================
## ensemble() calibrates CV+ UQ by default, so interval = TRUE returns interval
## columns. The graceful-degradation path (point-only + one-time note) now
## belongs to compute_uq = FALSE builds and corrupt bundles; the corrupt
## bundles' warnings are asserted in test-ensemble-uq.R.

describe("predict.horizons_ensemble() - intervals and degradation", {

  it("default build returns interval columns with no note", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    expect_no_message(p <- predict(ens, test_set, interval = TRUE))

    expect_true(all(c(".pred_lower", ".pred_upper", ".interval_width") %in%
                      names(p)))
    expect_true(all(p$.pred_upper >= p$.pred_lower))
    expect_true(all(p$.pred_lower >= 0))

  })

  it("compute_uq = FALSE returns point-only with an informative message", {

    test_set <- ens_test_set()
    ens0     <- ens_built("weighted", compute_uq = FALSE)

    expect_message(
      p <- predict(ens0, test_set, interval = TRUE),
      regexp = "intervals are not available"
    )
    expect_false(".pred_lower" %in% names(p))

  })

  it("interval = FALSE returns point-only with no message", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

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

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    broken_member <- ens$ensemble$weights$member[1]
    ens$models$workflows[[broken_member]] <- "not a workflow"

    ## The abort carries no package class.
    expect_error(predict(ens, test_set, interval = FALSE),
                 paste0("Prediction failed for config '", broken_member, "'"),
                 fixed = TRUE)

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

    fitted   <- ens_fitted()
    test_set <- ens_test_set()

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

    fitted   <- ens_fitted()
    test_set <- ens_test_set()

    ## The abort carries no package class.
    expect_error(predict.horizons_ensemble(fitted, test_set),
                 "`object` must be a <horizons_ensemble> object", fixed = TRUE)

  })

  it("aborts when the object carries no fitted ensemble", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    no_model <- ens
    no_model$ensemble$model <- NULL

    no_slot <- ens
    no_slot$ensemble <- NULL

    ## The abort carries no package class.
    expect_error(predict(no_model, test_set, interval = FALSE),
                 "No fitted ensemble found on this object", fixed = TRUE)
    expect_error(predict(no_slot, test_set, interval = FALSE),
                 "No fitted ensemble found on this object", fixed = TRUE)

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

    ens <- ens_built("weighted", compute_uq = uq)

    ens$models$selection_present <- TRUE
    ens

  }

  it("warns once when intervals are requested on a selected ensemble", {

    test_set <- ens_test_set()
    ens      <- ens_selected()

    warns <- testthat::capture_warnings(
      p <- suppressMessages(predict(ens, test_set, interval = TRUE))
    )

    expect_equal(sum(grepl("Conformal coverage is not guaranteed", warns)), 1L)
    expect_true(any(grepl("target_distances", warns)))
    expect_equal(nrow(p), dplyr::n_distinct(test_set$sample_id))

  })

  it("is silent when intervals are not requested", {

    test_set <- ens_test_set()

    warns <- testthat::capture_warnings(
      predict(ens_selected(), test_set, interval = FALSE)
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

  it("is silent with no ensemble UQ, where no intervals are returned", {

    test_set <- ens_test_set()

    warns <- testthat::capture_warnings(
      suppressMessages(predict(ens_selected(uq = FALSE), test_set,
                               interval = TRUE))
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

  it("is silent on an unselected ensemble", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

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

    ens <- ens_built("weighted", compute_uq = uq)

    ens$evaluation$response_trim <- list(
      outcome = "SOC", method = "iqr", threshold = 1.5, fences_from = "training",
      lower = 0.5, upper = 4.5, n_training = 40L, trimmed_ids = trimmed_ids,
      skipped = NA_character_
    )

    ens

  }

  it("warns once that the intervals were calibrated within the training fences", {

    test_set <- ens_test_set()
    caught   <- list()

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

    test_set <- ens_test_set()

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
## predict.horizons_ensemble() — an ensemble with no members
## =========================================================================

describe("predict.horizons_ensemble() - no member set", {

  it("aborts naming the missing member set when the ensemble has no weights", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

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

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

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

    fitted <- ens_fitted()

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

    fitted <- ens_fitted()
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

  it("refuses an allow_par that is not TRUE or FALSE", {

    fitted <- ens_fitted()

    ## The abort carries no package class.
    expect_error(
      ensemble(fitted, method = "weighted", allow_par = "yes", verbose = FALSE),
      "`allow_par` must be TRUE or FALSE", fixed = TRUE
    )

  })

  it("warns when a mirai daemon pool would take the run from the future plan", {

    skip_on_cran()
    skip_if_not_installed("mirai")

    if (isTRUE(tryCatch(mirai::status()$connections >= 1, error = function(e) FALSE))) {
      skip("mirai daemons are live in this session")
    }

    fitted <- ens_fitted()
    local_plan(future::multisession, workers = 2)
    mirai::daemons(2)
    withr::defer(mirai::daemons(0))

    ## The daemons dial in shortly after launch; the warning reads the live
    ## connection count.
    for (i in seq_len(100)) {
      if (isTRUE(mirai::status()$connections >= 2)) break
      Sys.sleep(0.05)
    }
    expect_gte(mirai::status()$connections, 2)

    ## The warning carries no package class. The weighted engine at
    ## optimize = FALSE tunes nothing, so neither pool is given any work.
    expect_warning(
      ens <- keep_only_warning(
        ensemble(fitted, method = "weighted", optimize = FALSE,
                 allow_par = TRUE, compute_uq = FALSE, verbose = FALSE),
        "A mirai daemon pool"
      ),
      "A mirai daemon pool", fixed = TRUE
    )

    expect_true(inherits(ens, "horizons_ensemble"))

  })

})


## ---------------------------------------------------------------------------
## combine_ensemble_metamodel() - row alignment before the positional bind (#140)
## ---------------------------------------------------------------------------
## The meta-model's predictions carry no key and are attached to the widened
## frame's sample_id by position. A single prediction used to be recycled
## across every sample by tibble() without a word.

describe("combine_ensemble_metamodel() - row alignment", {

  member_pred <- tibble::tibble(
    config_id = rep(c("a", "b"), each = 4),
    sample_id = rep(paste0("s", 1:4), 2),
    .pred     = c(1, 2, 3, 4, 2, 3, 4, 5)
  )
  meta <- structure(list(), class = "horizons_test_meta")

  it("aborts when the meta-model returns fewer predictions than samples", {

    local_mocked_s3_method("predict", "horizons_test_meta", function(object, new_data, ...) {
      tibble::tibble(.pred = 3)
    })

    expect_error(combine_ensemble_metamodel(member_pred, c("a", "b"), meta),
                 "Meta-model predictions", class = "horizons_internal_error")

  })

  it("is silent when there is one prediction per sample", {

    local_mocked_s3_method("predict", "horizons_test_meta", function(object, new_data, ...) {
      tibble::tibble(.pred = rowMeans(new_data))
    })

    out <- combine_ensemble_metamodel(member_pred, c("a", "b"), meta)

    expect_identical(out$sample_id, paste0("s", 1:4))
    expect_equal(out$.pred, c(1.5, 2.5, 3.5, 4.5))

  })

  it("aborts at fit time when the meta-model's test predictions are short", {

    fitted <- ens_fitted()

    ## fit_tuned_meta_learner() binds the meta-model's test_F predictions to
    ## the test samples and their truth the same way; one prediction would be
    ## recycled and the ensemble's test metrics scored on a constant. Only the
    ## meta-model's call is shortened: its new_data is the member_ matrix,
    ## while the members themselves predict spectra.
    predict_workflow <- utils::getS3method("predict", "workflow")

    local_mocked_s3_method("predict", "workflow", function(object, new_data, ...) {
      if (all(startsWith(names(new_data), "member_"))) {
        tibble::tibble(.pred = 3)
      } else {
        predict_workflow(object, new_data, ...)
      }
    })

    ## suppressWarnings(): rsample's note that the fixture is too small for
    ## the default strata breaks is not what this test is about.
    expect_error(
      suppressWarnings(ensemble(fitted, method = "penalized", optimize = FALSE,
                                compute_uq = FALSE, verbose = FALSE)),
      "Meta-model predictions", class = "horizons_internal_error"
    )

  })

  it("aborts at fit time when the meta-model's out-of-fold predictions are short", {

    fitted <- ens_fitted()

    ## The OOF predictions are bound to oof$row and truth by position; a
    ## single collected prediction used to be recycled across every row.
    collect_predictions <- tune::collect_predictions

    local_mocked_bindings(
      collect_predictions = function(x, ...) collect_predictions(x, ...)[1, ],
      .package = "tune"
    )

    expect_error(
      suppressWarnings(ensemble(fitted, method = "penalized", optimize = FALSE,
                                compute_uq = FALSE, verbose = FALSE)),
      "Meta-model out-of-fold predictions", class = "horizons_internal_error"
    )

  })

})


## ---------------------------------------------------------------------------
## The summary ensemble() prints
## ---------------------------------------------------------------------------

describe("ensemble() - the summary it prints", {

  it("shows the top members, the score, the improvement over the best member, the UQ and the runtime", {

    ## render_ensemble_summary() is what ensemble(verbose = TRUE) ends with;
    ## the shared ensemble was built quietly, so its contract is rendered
    ## here. The second call takes the branches the fixture does not: members
    ## ordered by the size of their weight, sign aside; an ensemble worse
    ## than its best member; no UQ bundle; a runtime in minutes.
    contract    <- ens_built("weighted")$ensemble
    rank_metric <- ens_fitted()$models$rank_metric

    expect_snapshot(
      render_ensemble_summary(contract, rank_metric),
      transform = function(x) sub("Runtime: [0-9.]+s", "Runtime: <time>", x)
    )

    contract$weights$coef <- c(0.2, -0.5, 0.3)
    contract$improvement  <- -0.0123
    contract$uq           <- NULL
    contract$runtime_secs <- 125

    expect_snapshot(render_ensemble_summary(contract, rank_metric))

  })

})
