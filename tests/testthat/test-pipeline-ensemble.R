## ---------------------------------------------------------------------------
## Tests: ensemble()
## ---------------------------------------------------------------------------
## Integration tests for the ensemble() pipeline verb and its three meta-learner
## engines (penalized / weighted / xgb).
##
## Fixture: tests/testthat/fixtures/ensemble_fit.rds — a small horizons_fit on
## REAL (anonymized) MIR spectra predicting Bulk_C, so its members carry real
## signal. Its members span two response transforms (log / sqrt), so the
## back-transform double-application regression is actually exercisable —
## under transformation = "none" the inverse is identity and a double-apply is
## invisible. Whether the meta-learner learns is tested on synthetic data made
## for it, not on this fixture.
##
## See fixtures/README.md.
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

  it("xgb fixed-spec path completes, with its fixed settings", {

    ens <- ens_built("xgb")
    expect_true(inherits(ens, "horizons_ensemble"))

    ## Built here rather than read from the shared build, so that this file
    ## fits the meta-learner itself
    fixed <- suppressWarnings(
      ensemble(ens_fitted(), method = "xgb", optimize = FALSE, compute_uq = FALSE)
    )

    booster <- hardhat::extract_fit_engine(fixed$ensemble$model)
    tree    <- xgboost::xgb.config(booster)$learner$gradient_booster$tree_train_param

    expect_equal(xgboost::xgb.get.num.boosted.rounds(booster), 500)
    expect_equal(as.numeric(tree$max_depth), 3)
    expect_equal(as.numeric(tree$eta), 0.05)

  })

})

## =========================================================================
## PREFLIGHT — input validation
## =========================================================================

describe("ensemble() - its control", {

  it("records the control it ran with, defaults filled in", {

    ctrl <- ens_built("weighted")$ensemble$control

    expect_s3_class(ctrl, "horizons_control")
    expect_identical(names(ctrl), names(resolve_control(NULL, "ensemble")))
    expect_identical(ctrl$min_calib_rows, N_CALIB_MIN)

  })

  it("hands min_calib_rows to fit_ensemble_uq()", {

    seen <- NULL
    local_mocked_bindings(fit_ensemble_uq = function(x, ..., min_calib_rows) {
      seen <<- min_calib_rows
      x
    })

    suppressWarnings(ensemble(ens_fitted(), method = "weighted", optimize = FALSE,
                              control = control_horizons(min_calib_rows = 40L)))
    expect_identical(seen, 40L)

  })

})


describe("ensemble() - preflight validation", {

  it("aborts on non-horizons_fit input", {

    ## validate_horizons_fit() refuses the same input with the same headline
    ## but no hint and the validation class, so the input class and the hint
    ## are what show this check fired.
    err <- expect_error(ensemble(list(a = 1)),
                        "`x` must be a <horizons_fit> object", fixed = TRUE,
                        class = "horizons_error_input")
    expect_match(conditionMessage(err), "Run `fit()` first to produce a fitted object",
                 fixed = TRUE)

  })

  it("aborts on an unknown method", {

    fitted <- ens_fitted()

    expect_error(ensemble(fitted, method = "bogus"),
                 "`method` must be one of", fixed = TRUE,
                 class = "horizons_error_input")
    expect_error(ensemble(fitted, method = c("weighted", "xgb")),
                 "`method` must be one of", fixed = TRUE,
                 class = "horizons_error_input")

  })

  it("refuses a fitted object that breaks its contract, before fitting (#129)", {

    fitted <- ens_fitted()

    ## ensemble() used to check only the class and the outcome range, so
    ## each of these reached the meta-learner.

    ## The base contract: the id role moved off sample_id, onto a meta column
    off_id <- add_columns(fitted, tibble::tibble(site = rep("a", fitted$data$n_rows)), role = "meta")
    rm     <- off_id$data$role_map
    rm$role[rm$variable == "sample_id"] <- "meta"
    rm$role[rm$variable == "site"]      <- "id"
    off_id$data$role_map <- rm

    expect_error(
      suppressMessages(capture.output(
        ensemble(off_id, method = "weighted", optimize = FALSE,
                 compute_uq = FALSE)
      )),
      "must be on sample_id",
      class = "horizons_error_validation"
    )

    ## The fit contract: a models slot missing a key
    no_ad <- fitted
    no_ad$models$ad <- NULL

    expect_error(
      suppressMessages(capture.output(
        ensemble(no_ad, method = "weighted", optimize = FALSE,
                 compute_uq = FALSE)
      )),
      "missing from models: ad",
      class = "horizons_error_validation"
    )

  })

  it("refuses an unusable stored outcome range as a validation finding, not by reading it", {

    fitted <- ens_fitted()

    for (bad in list(c(0, NA), c(5, 1), "a")) {

      broken <- fitted
      broken$config$outcome_range <- bad

      err <- expect_error(
        suppressMessages(capture.output(
          ensemble(broken, method = "weighted", optimize = FALSE,
                   compute_uq = FALSE)
        )),
        class = "horizons_error_validation"
      )
      expect_true(any(grepl("is not a usable range", err$findings, fixed = TRUE)),
                  label = deparse(bad))

    }

  })

  it("refuses a bad seed, optimize or compute_uq before any engine runs (#130)", {

    fitted <- ens_fitted()

    ## Both are recorded on the ensemble and must not be NULL there, so a bad
    ## value is refused on entry rather than by the validator once the
    ## meta-learner is built.
    local_mocked_bindings(
      fit_ensemble_weighted = function(...) stop("fit_ensemble_weighted() was called")
    )

    bad_seeds <- list(NULL, NA_real_, c(1, 2), "307", Inf, 1.5)

    for (s in bad_seeds) {

      expect_error(
        ensemble(fitted, method = "weighted", optimize = FALSE, seed = s,
                 compute_uq = FALSE),
        "seed",
        class = "horizons_error_input"
      )

    }

    bad_flags <- list(NULL, NA, c(TRUE, FALSE), "yes")

    for (o in bad_flags) {

      expect_error(
        ensemble(fitted, method = "weighted", optimize = o,
                 compute_uq = FALSE),
        "optimize",
        class = "horizons_error_input"
      )

      expect_error(
        ensemble(fitted, method = "weighted", optimize = FALSE, compute_uq = o),
        "compute_uq",
        class = "horizons_error_input"
      )

    }

  })

})

describe("ensemble() - predict namespaces", {

  it("loads the members' predict namespaces before scoring them on the test rows, and before predicting", {

    fitted   <- ens_fitted()
    ens      <- ens_built("weighted")
    test_set <- ens_test_set()

    ## Under an installed package the Imports load lazily, so a fitted object
    ## read into a fresh session has no workflows predict method registered
    ## until something loads it. The member scoring and
    ## predict.horizons_ensemble() must both ask for it.
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

    requested <- NULL
    predict(ens, test_set, interval = FALSE)

    expect_identical(requested, ens$ensemble$weights$member)

  })

})

## =========================================================================
## ensemble() — a member's warning while it scores the test rows
## =========================================================================
## The members predict the test rows as units, which record their warnings;
## until ensemble() records them itself, an engine's warning reaches the
## caller as it did before.

describe("ensemble() - a member's warning on the test rows", {

  it("is kept once in the ensemble's warnings, as a member's are kept, not raised per member", {

    fitted <- ens_fitted()
    real   <- utils::getS3method("predict", "workflow")

    local_mocked_s3_method("predict", "workflow", function(object, new_data, ...) {
      warning("the member engine says hello", call. = FALSE)
      real(object, new_data, ...)
    })

    run <- collect_warnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, compute_uq = FALSE),
      "the member engine says hello"
    )

    kept <- run$value$ensemble$warnings

    expect_length(run$warnings, 0L)
    expect_identical(sum(grepl("the member engine says hello", kept, fixed = TRUE)), 1L)

  })

})

describe("ensemble() - the meta-learner as one unit", {

  it("stops at the first failure under debug, and keeps the backtrace under keep_traces", {

    fitted <- ens_fitted()

    local_mocked_bindings(extract_weights_penalized = function(...) stop("the weights broke"))

    run <- function(...) {
      suppressWarnings(ensemble(fitted, method = "penalized", optimize = FALSE,
                                compute_uq = FALSE, control = control_horizons(...)))
    }

    ## Contained: the record, its step, and no trace unless asked for
    err <- expect_error(run(), class = "horizons_error_ensemble_failed")
    expect_identical(err$results$step, "member_weights")
    expect_null(err$results$trace[[1]])

    traced <- expect_error(run(keep_traces = TRUE), class = "horizons_error_ensemble_failed")
    expect_type(traced$results$trace[[1]], "character")

    ## Under debug nothing is contained: the engine's own error
    raw <- expect_error(run(debug = TRUE), "the weights broke", fixed = TRUE)
    expect_false(inherits(raw, "horizons_error"))

  })

  it("stops as systemic when a parallel worker dies", {

    fitted <- ens_fitted()

    local_mocked_bindings(build_oof_matrix = function(...) {
      stop(structure(class = c("FutureError", "error", "condition"),
                     list(message = "the worker died", call = NULL)))
    })

    err <- expect_error(
      ensemble(fitted, method = "weighted", optimize = FALSE, compute_uq = FALSE),
      class = "horizons_error_systemic"
    )

    ## ensemble() has no rows before its meta-learner is built, so it
    ## carries none and promises none
    expect_null(err$results)
    expect_no_match(conditionMessage(err), "rows finished so far", fixed = TRUE)

  })

  it("chooses the meta-learner's hyperparameters among the candidates scored on every fold (#101)", {

    fitted <- ens_fitted()
    real   <- select_complete
    asked  <- NULL

    local_mocked_bindings(
      select_complete = function(results, metric, n_folds, debug = FALSE) {
        asked <<- n_folds
        real(results, metric, n_folds, debug)
      },
      incomplete_note = function(...) "3 of 200 candidates were left out of the choice of hyperparameters."
    )

    ens <- suppressWarnings(ensemble(fitted, method = "penalized", optimize = TRUE,
                                     compute_uq = FALSE))

    expect_identical(asked, 5L)

    ## The candidates left out are pinned on the ensemble's row, as on a
    ## configuration's, and are not a note on the log
    expect_true(any(grepl("3 of 200 candidates were left out", ens$ensemble$warnings, fixed = TRUE)))

    notes <- ens$log[ens$log$verb == "ensemble" & ens$log$type == "note", , drop = FALSE]
    expect_false(any(grepl("candidates were left out", notes$message, fixed = TRUE)))

  })

  it("counts a meta-learner grid candidate that failed on every fold among those left out (#249)", {

    fitted <- ens_fitted()

    ## glmnet fits one path per mixture, so failing mixture 0 fails its
    ## twenty penalties on every fold; tune keeps no metrics for them
    real_glmnet <- glmnet::glmnet
    local_mocked_bindings(glmnet = function(...) {
      if (isTRUE(all.equal(list(...)$alpha, 0))) stop("the engine broke on every fold")
      real_glmnet(...)
    }, .package = "glmnet")

    ens <- suppressWarnings(ensemble(fitted, method = "penalized", optimize = TRUE,
                                     compute_uq = FALSE))

    expect_true(any(grepl(
      "20 of 200 candidates were left out of the choice of hyperparameters, not being scored on every fold: the engine broke on every fold",
      ens$ensemble$warnings, fixed = TRUE
    )))

  })

  it("fails when no meta-learner candidate was scored on every fold", {

    fitted <- ens_fitted()

    local_mocked_bindings(select_complete = function(results, metric, n_folds, debug = FALSE) {
      tibble::tibble(penalty = numeric(0), mixture = numeric(0), .config = character(0))
    })

    err <- expect_error(
      suppressWarnings(ensemble(fitted, method = "penalized", optimize = TRUE, compute_uq = FALSE)),
      class = "horizons_error_ensemble_failed"
    )

    expect_identical(err$results$step, "select_best")
    expect_match(err$results$error_message, "no meta-learner candidate was scored on every fold",
                 fixed = TRUE)

    ## Shaped as predict()'s failures are: the unit, and no configuration
    expect_identical(names(err$results),
                     c("unit", "config_id", "step", "class", "error_message", "trace"))
    expect_identical(err$results$unit, "meta_learner")
    expect_identical(err$results$config_id, NA_character_)

  })

  it("searches grids whose every candidate fits and keeps a member, so tune keeps no note on the search", {

    skip_unless_slow_tier()

    fitted   <- ens_fitted()
    real     <- select_complete
    searched <- NULL

    local_mocked_bindings(select_complete = function(results, metric, n_folds, debug = FALSE) {
      searched <<- results
      real(results, metric, n_folds, debug)
    })

    ## tune notes a candidate that fails to fit (an xgboost setting it
    ## refuses) as an error, and one whose predictions are constant (a
    ## penalty at which glmnet keeps no member) as a warning that the
    ## correlation is undefined. ensemble() muffles tune's console and keeps
    ## only the chosen candidate's warnings, so the search is read here.
    for (method in c("penalized", "xgb")) {

      searched <- NULL
      suppressWarnings(ensemble(fitted, method = method, optimize = TRUE, compute_uq = FALSE))

      expect_s3_class(searched, "tune_results")
      expect_gt(length(unique(tune::collect_metrics(searched)$.config)), 1L)
      expect_identical(nrow(tune::collect_notes(searched)), 0L, info = method)

    }

  })

  it("keeps only the chosen candidate's tune warnings when it tunes, and says when it cannot tell them apart (#109)", {

    fitted <- ens_fitted()

    ## Each of the grid's ten model stages (one per mixture; penalty is a
    ## submodel) leaves a warning of its own on the first fold, named by a
    ## letter (the warnings shown group texts that differ only in their
    ## numbers), and the choice is fixed at a mixture of 1
    real_grid <- tune::tune_grid
    local_mocked_bindings(tune_grid = function(...) {
      res <- real_grid(...)
      res$.notes[[1]] <- dplyr::bind_rows(res$.notes[[1]], tibble::tibble(
        location = sprintf("preprocessor 1/1, model %d/10", 1:10),
        type     = "warning",
        note     = paste("stage", LETTERS[1:10], "warned"),
        trace    = rep(list(NULL), 10)
      ))
      res
    }, .package = "tune")

    local_mocked_bindings(select_complete = function(results, metric, n_folds, debug = FALSE) {
      cm <- tune::collect_metrics(results)
      utils::head(cm[cm$mixture == 1, c("penalty", "mixture", ".config")], 1)
    })

    build <- function() {
      suppressWarnings(ensemble(fitted, method = "penalized", optimize = TRUE, compute_uq = FALSE))
    }

    expect_length(grep("stage [A-J] warned", build()$ensemble$warnings), 1L)

    ## When tune's grid schedule cannot be read, every stage's warning is
    ## kept (the lines shown are capped, so more than the chosen one's is
    ## enough), and the ensemble's warnings say why, as a configuration's row
    ## does
    local_mocked_bindings(grid_model_stages = function(...) {
      stop("tune's grid schedule has no model_stage column", call. = FALSE)
    })

    warnings <- build()$ensemble$warnings

    expect_gt(length(grep("stage [A-J] warned", warnings)), 1L)
    expect_true(paste0("The warnings of every grid candidate are shown, not only the chosen one's: ",
                       "tune's grid schedule has no model_stage column.") %in% warnings)

  })

  it("keeps the warnings tune noted for the meta-learner in the ensemble's warnings", {

    fitted <- ens_fitted()
    real   <- tune_note_records

    local_mocked_bindings(tune_note_records = function(tune_results, type = "warning", scope = "cv", ...) {
      if (identical(type, "warning")) text_records("glmnet said something about the fold") else
        real(tune_results, type = type, scope = scope, ...)
    })

    ens <- suppressWarnings(ensemble(fitted, method = "penalized", optimize = FALSE,
                                     compute_uq = FALSE))

    expect_true(any(grepl("glmnet said something about the fold", ens$ensemble$warnings, fixed = TRUE)))

  })

  it("names a test row a member could not use in one flagged warning", {

    fitted  <- ens_fitted()
    real    <- predict_members
    test_id <- ens_test_set()$sample_id[1]

    local_mocked_bindings(predict_members = function(...) {
      out <- real(...)
      attr(out, "flagged") <- list(m = list(arrived = character(0), recipe = test_id))
      out
    })

    run <- collect_warnings(
      ensemble(fitted, method = "weighted", optimize = FALSE, compute_uq = FALSE),
      "could not be predicted"
    )

    expect_length(run$warnings, 1L)

    flagged <- run$value$log[run$value$log$topic %in% "flagged", , drop = FALSE]

    expect_identical(nrow(flagged), 1L)
    expect_identical(flagged$ids[[1]], test_id)

  })

  it("says in its own words that the meta-learner could not be built, and when CV+ could not be calibrated", {

    fitted <- ens_fitted()

    local_mocked_bindings(extract_weights_penalized = function(...) stop("the weights broke"))

    expect_snapshot(
      ensemble(fitted, method = "penalized", optimize = FALSE, compute_uq = FALSE),
      error = TRUE
    )

    local_mocked_bindings(fit_uq_fold_models = function(...) stop("a fold refit failed"))

    expect_snapshot(
      invisible(ensemble(fitted, method = "weighted", optimize = FALSE))
    )

  })

})

describe("ensemble() - a member that cannot predict the test rows", {

  ## The meta-learner is ensemble()'s unit, and the members' test predictions
  ## one of its steps: the build stops with the record, in ensemble()'s words
  ## and frame, for the weighted engine and for the tuned meta-learners'
  ## shared scaffold

  for (method in c("weighted", "penalized")) {

    it(sprintf("stops the build with the meta-learner's record and reports ensemble()'s call (%s)", method), {

      fitted <- ens_fitted()
      broken <- fitted$models$best_config
      fitted$models$workflows[[broken]] <- "not a workflow"

      ## The meta-learner's folds warn on the fixture's small n (rsample)
      err <- expect_error(
        suppressWarnings(ensemble(fitted, method = method, optimize = FALSE, compute_uq = FALSE)),
        class = "horizons_error_ensemble_failed"
      )

      expect_s3_class(err, "horizons_error_run")
      expect_identical(rlang::call_name(err$call), "ensemble")
      expect_match(conditionMessage(err), sprintf("The %s ensemble could not be built", method), fixed = TRUE)
      expect_identical(err$results$step, "test_predict")
      expect_identical(err$results$class, "horizons_error_prediction_failed")
      expect_match(err$results$error_message, "Test prediction failed:", fixed = TRUE)
      expect_match(err$results$error_message, broken, fixed = TRUE)

    })

  }

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

  it("compute_uq = FALSE returns point-only, silently, by default", {

    test_set <- ens_test_set()
    ens0     <- ens_built("weighted", compute_uq = FALSE)

    expect_no_message(expect_no_warning(p <- predict(ens0, test_set)))
    expect_false(".pred_lower" %in% names(p))

  })

  it("compute_uq = FALSE gives NA bounds, and says so once, when interval = TRUE asks for them (#145)", {

    test_set <- ens_test_set()
    ens0     <- ens_built("weighted", compute_uq = FALSE)

    w <- expect_warning(p <- predict(ens0, test_set, interval = TRUE),
                        class = "horizons_warning_skipped")

    expect_match(conditionMessage(w), "the ensemble has no CV+ bundle", fixed = TRUE)
    expect_true(all(is.na(p$.pred_lower)))
    expect_true(all(is.finite(p$.pred)))

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

  it("names every member that cannot predict", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    broken <- ens$ensemble$weights$member[1:2]
    for (m in broken) ens$models$workflows[[m]] <- "not a workflow"

    err <- expect_error(predict(ens, test_set, interval = FALSE),
                        class = "horizons_error_prediction_failed")

    expect_setequal(err$results$config_id, broken)

  })

  it("does not renormalize; aborts when a member workflow cannot predict", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    broken_member <- ens$ensemble$weights$member[1]
    ens$models$workflows[[broken_member]] <- "not a workflow"

    err <- expect_error(predict(ens, test_set, interval = FALSE),
                        class = "horizons_error_prediction_failed")

    expect_identical(err$results$config_id, broken_member)
    expect_identical(err$results$unit, "configuration")
    expect_identical(err$results$step, "bake")
    expect_match(conditionMessage(err), "it combines every member", fixed = TRUE)

  })

})

## =========================================================================
## predict.horizons_ensemble() — rows a member cannot use, and the combine
## =========================================================================
## A row any member could not use is NA in the ensemble's output, for every
## method: an xgb meta-model would read the member's NA as missing and return
## a number. The combination is one unit; when it fails there is nothing to
## return.

describe("predict.horizons_ensemble() - flagged rows and the combination", {

  pred_cols <- function(test_set) setdiff(names(test_set), "sample_id")

  for (m in c("weighted", "xgb")) {

    it(paste0("returns NA for a row a member cannot use, and the rest as without it, for ", m), {

      test_set <- ens_test_set()
      ens      <- ens_built(m)

      bad <- test_set
      bad[2, intersect(pred_cols(test_set), ens$models$predictor_schema)] <- NA_real_

      w <- collect_warnings(p <- predict(ens, bad, interval = TRUE),
                            "could not be predicted")$warnings

      expect_length(w, 1L)
      expect_identical(w[[1]]$log$ids[[1]], bad$sample_id[2])

      expect_equal(nrow(p), nrow(bad))
      expect_true(is.na(p$.pred[2]) && is.na(p$.pred_lower[2]))

      alone <- suppressWarnings(predict(ens, test_set[-2, ], interval = TRUE))
      expect_equal(p[-2, ], alone, ignore_attr = TRUE)

    })

  }

  it("returns a row flagged on arrival as NA", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    flagged       <- test_set
    flagged$.flag <- c("standardize", rep(NA_character_, nrow(test_set) - 1L))

    w <- expect_warning(p <- predict(ens, flagged, interval = FALSE),
                        class = "horizons_warning_flagged")

    expect_match(conditionMessage(w), "flagged on arrival", fixed = TRUE)
    expect_true(is.na(p$.pred[1]))
    expect_true(all(is.finite(p$.pred[-1])))

  })

  for (m in c("weighted", "penalized")) {

    it(paste0("returns zero rows with its columns for zero rows of new_data, for ", m), {

      test_set <- ens_test_set()
      ens      <- ens_built(m)

      p <- predict(ens, test_set[0, ], interval = TRUE)

      expect_equal(nrow(p), 0L)
      expect_named(p, c("sample_id", ".pred", ".pred_lower", ".pred_upper", ".interval_width"))

    })

  }

  it("returns its predictions in new_data's row order", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    set.seed(11)
    shuffled <- test_set[sample(nrow(test_set)), ]

    p <- predict(ens, shuffled, interval = FALSE)

    expect_identical(p$sample_id, shuffled$sample_id)

  })

  it("aborts with the combination's failure when the combination cannot run", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    local_mocked_bindings(combine_ensemble_weighted = function(...) stop("the weights broke"))

    err <- expect_error(predict(ens, test_set, interval = FALSE),
                        class = "horizons_error_prediction_failed")

    expect_identical(err$results$step, "combine")
    expect_identical(err$results$unit, "combination")
    expect_identical(err$results$config_id, NA_character_)
    expect_match(conditionMessage(err), "its combination failed", fixed = TRUE)
    expect_match(conditionMessage(err), "the weights broke (combination)", fixed = TRUE)

  })

})

## =========================================================================
## Members' applicability domain and intervals are not computed
## =========================================================================
## The member helpers keep only .pred, so a member's AD was baked, scored and
## dropped: wasted work. A member's UQ bundle is likewise never read: the
## ensemble's intervals come from its own CV+ bundle. Each member here carries
## an AD bundle and a UQ bundle, so a member helper that asked for either
## would compute it.

describe("predict.horizons_ensemble() - what its units report", {

  it("raises a horizons warning a member raised while predicting, once, naming the members, as on a fit", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")
    real     <- utils::getS3method("predict", "workflow")

    local_mocked_s3_method("predict", "workflow", function(object, new_data, ...) {
      warn_in_unit("substitution", "A member substituted something.")
      real(object, new_data, ...)
    })

    w <- collect_warnings(predict(ens, test_set, interval = FALSE),
                          "A member substituted something")$warnings

    expect_length(w, 1L)
    expect_s3_class(w[[1]], "horizons_warning_substitution")
    expect_setequal(w[[1]]$log$ids[[1]], ens$ensemble$weights$member)

  })

})

describe("ensemble() and predict.horizons_ensemble() - member AD and intervals", {

  it("never computes a member's applicability domain or intervals, in ensemble() or predict()", {

    fitted   <- ens_fitted()
    test_set <- ens_test_set()

    with_bundles <- fitted
    member       <- names(with_bundles$models$workflows)[1]

    with_bundles$models$ad <- stats::setNames(list(list(
      centroid      = c(wn_0 = 0),
      cov_matrix    = matrix(1, dimnames = list("wn_0", "wn_0")),
      ad_thresholds = c(q25 = 1, q50 = 2, q75 = 3, ood = 4)
    )), member)

    ## Not a UQ bundle beyond the level and the scores the fit validator
    ## checks: reading it would fail the intervals.
    with_bundles$models$uq <- stats::setNames(list(list(method = "not a bundle", level_default = 0.9,
                                                        scores = seq_len(9))), member)

    calls <- c(intervals = 0L, ad = 0L)
    local_mocked_bindings(
      predict_intervals = function(...) {
        calls[["intervals"]] <<- calls[["intervals"]] + 1L
        stop("a member's intervals were computed")
      },
      predict_ad = function(...) {
        calls[["ad"]] <<- calls[["ad"]] + 1L
        stop("a member's applicability domain was computed")
      }
    )

    ens <- suppressWarnings(ensemble(with_bundles, method = "weighted", optimize = FALSE))
    p   <- suppressWarnings(predict(ens, test_set))

    expect_identical(calls, c(intervals = 0L, ad = 0L))
    expect_true(all(is.finite(p$.pred)))

  })

})

## =========================================================================
## predict.horizons_ensemble() — preflight
## =========================================================================

describe("predict.horizons_ensemble() - preflight", {

  it("aborts on a non-ensemble object", {

    fitted   <- ens_fitted()
    test_set <- ens_test_set()

    ## A direct call skips S3 dispatch; the door's gate still refuses it
    err <- expect_error(predict.horizons_ensemble(fitted, test_set),
                        class = "horizons_error_validation")
    expect_true(any(grepl("is not a <horizons_ensemble> object", err$findings, fixed = TRUE)))

  })

  it("refuses new_data whose sample_ids repeat, before predicting any member", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    dup <- rbind(test_set, test_set[1, ])

    ## A member that would fail shows that no member is predicted first
    ens$models$workflows[[ens$ensemble$weights$member[1]]] <- "not a workflow"

    err <- expect_error(predict(ens, dup, interval = FALSE), class = "horizons_error_input")
    expect_match(conditionMessage(err), "must be unique to predict from an ensemble", fixed = TRUE)
    expect_match(conditionMessage(err), test_set$sample_id[1], fixed = TRUE)
    expect_identical(rlang::call_name(err$call), "predict")

  })

  it("refuses an interval that is not TRUE, FALSE or NULL", {

    expect_error(predict(ens_built("weighted"), ens_test_set(), interval = 1),
                 "`interval` must be `TRUE`, `FALSE`, or `NULL`", fixed = TRUE,
                 class = "horizons_error_input")

  })

  it("aborts when the object carries no fitted ensemble", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    no_model <- ens
    no_model$ensemble$model <- NULL

    no_slot <- ens
    no_slot$ensemble <- NULL

    ## Refused at the door, naming what is missing (#234)
    err <- expect_error(predict(no_model, test_set, interval = FALSE),
                        class = "horizons_error_validation")
    expect_true(any(grepl("Contract keys missing from ensemble: model", err$findings, fixed = TRUE)))

    err <- expect_error(predict(no_slot, test_set, interval = FALSE),
                        class = "horizons_error_validation")
    expect_true(any(grepl("no ensemble slot", err$findings, fixed = TRUE)))

  })

})

## =========================================================================
## predict.horizons_ensemble() — conformal coverage on a selected training set
## =========================================================================
## Same warning, same conditions, same wording as predict.horizons_fit(): the
## ensemble's CV+ intervals inherit the calibration rows' non-exchangeability
## exactly as the member intervals do. Both paths call
## log_interval_quality(), so the two cannot drift apart.

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
      p <- predict(ens, test_set, interval = TRUE)
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
      predict(ens_selected(uq = FALSE), test_set, interval = TRUE)
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

  it("is silent on an unselected ensemble", {

    test_set <- ens_test_set()
    ens      <- ens_built("weighted")

    warns <- testthat::capture_warnings(
      predict(ens, test_set, interval = TRUE)
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

})

## =========================================================================
## predict.horizons_ensemble() - members fit on a response-trimmed partition
## =========================================================================
## The ensemble's CV+ intervals were built from the members' out-of-fold
## predictions only, which after a response trim (#77) cover only the rows
## inside the training fences, and predict() warned so. The trimmed rows now
## calibrate the intervals too (#103, test-ensemble-uq.R), and the warning
## is gone.

describe("predict.horizons_ensemble() - trimmed members (#77, #103)", {

  trimmed_ensemble <- function(uq = TRUE) {

    ens <- ens_built("weighted", compute_uq = uq)

    ens$evaluation$response_trim <- list(
      outcome = "SOC", method = "iqr", threshold = 1.5, fences_from = "training",
      lower = 0.5, upper = 4.5, n_training = 40L, trimmed_ids = c("S001", "S002"),
      skipped = NA_character_
    )

    ens

  }

  it("says nothing about the fences: the trimmed rows calibrate the intervals", {

    test_set <- ens_test_set()

    warns <- function(ens, interval) {
      testthat::capture_warnings(predict(ens, test_set, interval = interval))
    }

    ## With intervals, and with intervals asked of an ensemble without UQ,
    ## whose one warning is that there are none
    expect_length(warns(trimmed_ensemble(), interval = TRUE), 0L)
    expect_false(any(grepl("training fences", warns(trimmed_ensemble(uq = FALSE), interval = TRUE))))

  })

})

## =========================================================================
## ensemble() - CV+ coverage after a response trim (#103)
## =========================================================================
## Empirical. An outcome with heavy tails (t, 2 df), so the training fences
## trim real extremes (16 to 24 of the 300 training rows here); two fast
## members; a weighted ensemble with CV+; and coverage on 2,000 fresh,
## untrimmed rows from the same generator, pooled over three seeds. At these
## seeds the intervals cover 0.891 of them; calibrated on the kept rows alone
## they covered 0.843. Over 20 seeds, pooled three at a time, they cover 0.875
## to 0.924, so the bounds leave 0.015 below the worst case and 0.016 above
## the best.

describe("ensemble() - CV+ coverage after a response trim (#103)", {

  it("covers fresh untrimmed rows at about the nominal level, pooled over three seeds", {

    skip_unless_slow_tier()

    wn <- paste0("wn_", seq(4000, by = -2, length.out = 30))

    ## The raw step keeps bands 5 to 26; the signal is in three of them
    draw <- function(n, seed) {
      withr::with_seed(seed, {
        x <- matrix(stats::rnorm(n * 30), n)
        colnames(x) <- wn
        d <- tibble::as_tibble(x)
        d$SOC <- 10 + 0.8 * x[, 10] + 0.6 * x[, 14] + 0.4 * x[, 18] + 1.5 * stats::rt(n, df = 2)
        d
      })
    }

    covered_at <- function(seed) {

      d <- draw(400L, seed)
      d$sample_id <- sprintf("S%04d", seq_len(nrow(d)))

      obj <- new_horizons_data(
        analysis = d,
        role_map = tibble::tibble(variable = c("sample_id", wn, "SOC"),
                                  role     = c("id", rep("predictor", length(wn)), "outcome"))
      )
      obj$config$configs <- tibble::tibble(
        config_id = c("cfg_001", "cfg_002"), model = c("elastic_net", "plsr"),
        transformation = "none", preprocessing = "raw", feature_selection = "none"
      )
      obj$config$n_configs     <- 2L
      obj$config$tuning        <- list(cv_folds = 3L, grid_size = 2L, bayesian_iter = 0L,
                                       final_bayesian_iter = 0L)
      obj$config$recipe        <- list(sg_window     = DEFAULT_SG_WINDOW,
                                       pca_threshold = DEFAULT_PCA_THRESHOLD)
      obj$config$outcome_range <- c(-Inf, Inf)

      utils::capture.output(
        v <- suppressWarnings(validate(obj, remove_outliers = "response"))
      )
      ev  <- suppressWarnings(evaluate(v, prune = FALSE, seed = seed))
      fx  <- suppressWarnings(fit(ev, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = seed))
      ens <- suppressWarnings(ensemble(fx, method = "weighted", compute_uq = TRUE))

      expect_gt(length(ev$evaluation$response_trim$trimmed_ids), 10L)

      fresh <- draw(2000L, seed + 10000L)
      fresh$sample_id <- sprintf("F%04d", seq_len(nrow(fresh)))

      p <- predict(ens, fresh[, c("sample_id", wn)], interval = TRUE)
      p <- p[match(fresh$sample_id, p$sample_id), ]

      fresh$SOC >= p$.pred_lower & fresh$SOC <= p$.pred_upper

    }

    covered <- unlist(lapply(1:3, covered_at))

    expect_false(anyNA(covered))
    expect_gte(mean(covered), 0.86)
    expect_lte(mean(covered), 0.94)

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

    err <- expect_error(predict(ens, test_set, interval = FALSE),
                        class = "horizons_error_validation")
    expect_true(any(grepl("weights needs at least 2 members; found 0", err$findings, fixed = TRUE)))

  })

})

## =========================================================================
## ensemble() weighted — the test-set combine is clamped to the outcome's range
## =========================================================================
## The members' test predictions stay raw (they are scored as members); the
## weighted sum the ensemble is scored on is clamped, as every served
## prediction is (#76).

describe("ensemble() weighted - test-set combine", {

  it("clamps the combined test predictions to the outcome's range", {

    fitted <- ens_fitted()

    ## The members predict the test rows 20 lower, so some combined
    ## predictions fall below the floor at 0 and some stay above it
    real_members <- predict_members_on_test
    shifted      <- NULL
    local_mocked_bindings(predict_members_on_test = function(object, members, ...) {
      mp       <- real_members(object, members, ...)
      mp$.pred <- mp$.pred - 20
      shifted <<- mp
      mp
    })

    ens <- ensemble(fitted, method = "weighted", optimize = FALSE, compute_uq = FALSE)

    by_hand <- shifted |>
      dplyr::left_join(ens$ensemble$weights, by = c("config_id" = "member")) |>
      dplyr::summarise(by_hand = sum(.data$.pred * .data$coef), .by = "sample_id")

    expect_true(any(by_hand$by_hand < 0) && any(by_hand$by_hand > 0))

    joined <- dplyr::inner_join(ens$ensemble$predictions, by_hand, by = "sample_id")

    expect_identical(nrow(joined), nrow(by_hand))
    expect_equal(joined$.pred, pmax(joined$by_hand, 0))

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
                      compute_uq = FALSE)
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

  it("runs sequentially when allow_par = TRUE has no backend, and says so once, on the log too", {

    fitted <- ens_fitted()
    local_plan(future::sequential)

    got <- collect_warnings(
      ensemble(fitted, method = "penalized", optimize = FALSE, compute_uq = FALSE,
               control = control_horizons(allow_par = TRUE)),
      "offers 1 worker"
    )
    ens <- got$value
    w   <- got$warnings[[1]]

    expect_length(got$warnings, 1L)
    expect_s3_class(w, "horizons_warning_substitution")
    expect_match(conditionMessage(w), "ensemble\\(\\).*offers 1 worker")
    expect_true(inherits(ens, "horizons_ensemble"))
    added <- log_rows_added(fitted, ens)
    expect_identical(added$verb, "ensemble")
    expect_identical(added$topic, "substitution")
    expect_identical(w$log, added)

  })

  it("pins the parent's threads while a backend it can use runs, and restores them on exit", {

    fitted <- ens_fitted()
    local_plan(future::sequential)

    calls     <- character()
    tune_par  <- logical()
    real_ctrl <- meta_tune_control
    local_mocked_bindings(
      check_parallel_backend = function(...) TRUE,
      pin_parent_threads     = function() {
        calls <<- c(calls, "pin")
        function() calls <<- c(calls, "restore")
      },
      meta_tune_control      = function(kind, allow_par = FALSE) {
        tune_par <<- c(tune_par, allow_par)
        real_ctrl(kind, allow_par)
      }
    )

    ens <- ensemble(fitted, method = "penalized", optimize = FALSE, compute_uq = FALSE,
                    control = control_horizons(allow_par = TRUE))

    expect_true(inherits(ens, "horizons_ensemble"))
    expect_identical(calls, c("pin", "restore"))

    ## The engines' tune control is told to use the plan
    expect_gt(length(tune_par), 0)
    expect_true(all(tune_par))

  })

  it("refuses a control field it does not use", {

    expect_error(
      ensemble(ens_fitted(), method = "weighted", control = control_horizons(output_dir = "out")),
      "output_dir: evaluate()", fixed = TRUE, class = "horizons_error_input"
    )

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
                 "Meta-model predictions", class = "horizons_error_internal")

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
                                compute_uq = FALSE)),
      "Meta-model predictions", class = "horizons_error_internal"
    )

  })

  it("aborts at fit time when the meta-model's out-of-fold predictions are too many", {

    fitted <- ens_fitted()

    ## The OOF predictions are bound to oof$row and truth by position; one
    ## too many would shift every row after it.
    collect_predictions <- tune::collect_predictions

    local_mocked_bindings(
      collect_predictions = function(x, ...) {
        p <- collect_predictions(x, ...)
        rbind(p, p[1, ])
      },
      .package = "tune"
    )

    expect_error(
      suppressWarnings(ensemble(fitted, method = "penalized", optimize = FALSE,
                                compute_uq = FALSE)),
      "Meta-model out-of-fold predictions", class = "horizons_error_internal"
    )

  })

  it("fails the meta-learner when its out-of-fold pass leaves rows unpredicted (#101)", {

    fitted <- ens_fitted()

    ## A fold that failed leaves its rows without a prediction; the
    ## meta-learner is then not scored on every row, a run failure rather
    ## than a broken invariant
    collect_predictions <- tune::collect_predictions

    local_mocked_bindings(
      collect_predictions = function(x, ...) collect_predictions(x, ...)[-(1:3), ],
      .package = "tune"
    )

    err <- expect_error(
      suppressWarnings(ensemble(fitted, method = "penalized", optimize = FALSE,
                                compute_uq = FALSE)),
      class = "horizons_error_ensemble_failed"
    )

    expect_identical(err$results$step, "oof_predictions")
    expect_match(err$results$error_message, "out-of-fold pass predicted", fixed = TRUE)

  })

})


## ---------------------------------------------------------------------------
## The summary ensemble() prints
## ---------------------------------------------------------------------------

describe("print() and summary() of an ensemble - its own intervals", {

  ## The intervals predict() serves for an ensemble are its CV+ ones, not its
  ## members', so they get a line of their own with their measured coverage
  ## on the held-out test rows beside nominal, through the one formatter
  ## (its wording is tested with it)
  line_of <- function(uq) paste0("Ensemble intervals (CV+): ", format_test_coverage(uq))

  it("reports the CV+ intervals' measured coverage beside nominal, after the members' UQ line", {

    ens <- ens_built("weighted")
    uq  <- ens$ensemble$uq

    printed <- utils::capture.output(print(ens))
    summed  <- utils::capture.output(summary(ens))

    expect_identical(uq$n_test, 12L)
    expect_identical(which(printed == paste0("   └─ ", line_of(uq))),
                     which(grepl("├─ UQ: ", printed, fixed = TRUE)) + 1L)
    expect_identical(which(summed == paste0("   ├─ ", line_of(uq))),
                     which(grepl("├─ UQ: ", summed, fixed = TRUE)) + 1L)

  })

  it("says the ensemble's intervals are disabled when it has none", {

    ens0 <- ens_built("weighted", compute_uq = FALSE)

    expect_true("   └─ Ensemble intervals (CV+): disabled" %in% utils::capture.output(print(ens0)))
    expect_true("   ├─ Ensemble intervals (CV+): disabled" %in% utils::capture.output(summary(ens0)))

  })

  it("reports a fit's members without an ensemble line", {

    fitted <- ens_fitted()

    expect_false(any(grepl("Ensemble intervals", utils::capture.output(print(fitted)), fixed = TRUE)))
    expect_false(any(grepl("Ensemble intervals", utils::capture.output(summary(fitted)), fixed = TRUE)))

  })

})


describe("ensemble() - the summary it prints", {

  ## render_ensemble_summary() is what ensemble(verbose = TRUE) ends with. It
  ## renders what a contract holds, so it is read on contracts built here,
  ## which take every branch whatever members a fit happens to choose: an
  ## ensemble that beats its best member and one that does not, members
  ## ordered by the size of their weight with signs mixed, with and without a
  ## UQ bundle, a runtime in seconds and in minutes.
  summary_contract <- function(improvement, coef, uq = TRUE, runtime_secs = 12.4) {
    list(
      weights      = tibble::tibble(member = c("cubist_log", "plsr_log", "plsr_sqrt"),
                                    coef   = coef),
      metrics      = tibble::tibble(.metric = c("rmse", "rpd"), .estimator = "standard",
                                    .estimate = c(2.31, 1.87)),
      improvement  = improvement,
      uq           = if (uq) list(level_default = 0.9, n_calib = 24L, test_coverage = 0.8333,
                                  test_mean_width = 3.04, n_test = 12L),
      runtime_secs = runtime_secs
    )
  }

  ## Snapshots record no colour, so the improvement line is also read with
  ## colours on: yellow when the ensemble does not beat its best member
  improvement_line <- function(contract) {
    withr::local_options(cli.num_colors = 256L)
    out <- utils::capture.output(render_ensemble_summary(contract, "rpd"))
    grep("Improvement over best member", out, value = TRUE, fixed = TRUE)
  }

  it("shows the top members, the score, an improvement over the best member, the UQ and the runtime", {

    better <- summary_contract(improvement = 0.0412, coef = c(0.5, 0.3, 0.2))

    expect_snapshot(render_ensemble_summary(better, "rpd"))
    expect_false(cli::ansi_has_any(improvement_line(better)))

  })

  it("orders members by the size of their weight, and flags an ensemble worse than its best member", {

    worse <- summary_contract(improvement = -0.0123, coef = c(0.2, -0.5, 0.3),
                              uq = FALSE, runtime_secs = 125)

    expect_snapshot(render_ensemble_summary(worse, "rpd"))
    expect_match(improvement_line(worse), "\033[33mImprovement over best member: -0.0123",
                 fixed = TRUE)

  })

  it("says there is no improvement to measure when it is NA, in yellow", {

    unmeasured <- summary_contract(improvement = NA_real_, coef = c(0.5, 0.3, 0.2))

    expect_match(improvement_line(unmeasured),
                 "\033[33mImprovement over best member: none to measure, without a rpd to compare",
                 fixed = TRUE)

  })

})

## =========================================================================
## The weighted meta-learner learns
## =========================================================================
## The summary tests above read contracts built by hand; this one checks the
## weighted rule end to end on data where averaging should help: members
## whose errors are independent. The most accurate member is listed last, so
## equal weights would fail the argmax check. Over 50 seeds the improvement in
## test RMSE was at least 0.027 (median 0.61), and the most accurate member
## weighed most in 47; the seed below is one of those.

describe("the weighted meta-learner", {

  it("beats its best member when the members' errors are independent", {

    withr::local_seed(11)
    n_oof <- 60; n_test <- 30
    truth_oof  <- stats::runif(n_oof, 10, 30)
    truth_test <- stats::runif(n_test, 10, 30)
    sds        <- c(c = 3, b = 2.5, a = 2)

    oof  <- vapply(sds, function(s) truth_oof + stats::rnorm(n_oof, sd = s), numeric(n_oof))
    test <- vapply(sds, function(s) truth_test + stats::rnorm(n_test, sd = s), numeric(n_test))

    oof_tbl <- tibble::as_tibble(stats::setNames(as.data.frame(oof), paste0("member_", names(sds))))
    weights <- derive_member_weights(oof_tbl, truth_oof, names(sds), optimize = TRUE)

    ## The most accurate member weighs most
    expect_identical(weights$member[which.max(weights$coef)], "a")

    member_pred <- do.call(rbind, lapply(names(sds), function(m) {
      tibble::tibble(config_id = m, truth = truth_test, .pred = test[, m])
    }))
    ensemble_pred <- tibble::tibble(truth = truth_test,
                                    .pred = as.vector(test[, weights$member] %*% weights$coef))

    contract <- build_ensemble_contract("weighted", weights, weights, ensemble_pred,
                                        member_pred, rank_metric = "rmse",
                                        runtime_secs = 0, optimize = TRUE, seed = 1L)

    expect_gt(contract$improvement, 0)

  })

})
