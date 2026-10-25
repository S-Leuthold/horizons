## ---------------------------------------------------------------------------
## Tests: ensemble-helpers.R
## ---------------------------------------------------------------------------
## The out-of-fold matrix's row order; the refusals inside the meta-learner
## scaffold and the weighted combine, the step each of the scaffold's and the
## weighted engine's failures is named by, and the scope of the warnings tune noted for it; and the
## engines' parts that the fixture cannot reach: the improvement on a
## metric where lower is better (the fixture ranks by rpd), the weights of the
## weighted engine's tuned rule and of a member xgboost never splits on, and
## the run time of an engine that runs past a minute. All are called
## directly: predict() always hands the combine a member set that matches the
## weights. ensemble() and predict.horizons_ensemble() are tested
## in test-pipeline-ensemble.R.
##
## ens_fitted() comes from helper-ensemble.R.


## =========================================================================
## build_oof_matrix() - row order
## =========================================================================

describe("build_oof_matrix() - row order", {

  it("orders the matrix by .row, whatever order the out-of-fold predictions were stored in", {

    fitted  <- ens_fitted()
    members <- gather_members(fitted)$members
    cv      <- fitted$models$cv_predictions

    ## fit() stores them fold by fold, not in .row order
    expect_true(is.unsorted(cv$.row[cv$config_id == members[1]]))

    oof <- build_oof_matrix(fitted, members)

    expect_identical(oof$row, sort(unique(cv$.row)))

    ## Each member's column, the truth and the sample ids line up with it
    for (m in members) {

      mine <- cv[cv$config_id == m, , drop = FALSE]
      at   <- match(oof$row, mine$.row)

      expect_identical(oof$predictors[[paste0("member_", m)]], mine$.pred[at], info = m)
      expect_identical(oof$truth, mine$truth[at], info = m)
      expect_identical(oof$sample_id, mine$sample_id[at], info = m)

    }

    ## The same matrix from the rows stored in another order
    shuffled <- fitted
    shuffled$models$cv_predictions <- withr::with_seed(3, cv[sample(nrow(cv)), ])

    expect_identical(build_oof_matrix(shuffled, members), oof)

  })

})


## =========================================================================
## fit_tuned_meta_learner() - refusals
## =========================================================================

describe("fit_tuned_meta_learner() - refusals", {

  it("is never handed a rank metric the tuning did not collect: ensemble() refuses it at the door", {

    ## The check that sat between tuning and select_best() moved to the fit
    ## validator, which ensemble() runs before anything is gathered (#234).
    fitted <- ens_fitted()
    fitted$models$rank_metric <- "rpiq"

    err <- expect_error(ensemble(fitted, method = "penalized"),
                        class = "horizons_error_validation")
    expect_true(any(grepl("rank_metric must be one of", err$findings, fixed = TRUE)))

  })

  it("fails the meta-learner's unit at its refit when the meta-learner cannot be refit", {

    fitted  <- ens_fitted()
    members <- gather_members(fitted)$members
    oof     <- build_oof_matrix(fitted, members)

    ## parsnip refuses a negative penalty when it fits, so the refit of this
    ## fixed spec fails: a run failure of the meta-learner's unit, at its
    ## refit
    spec <- parsnip::linear_reg(penalty = -1, mixture = 1) |>
      parsnip::set_engine("glmnet")

    out <- suppressWarnings(run_unit(fit_tuned_meta_learner(
      fitted, members, oof,
      rank_metric     = "rpd",
      optimize        = FALSE,
      method          = "penalized",
      spec            = spec,
      grid            = NULL,
      extract_weights = extract_weights_penalized
    ), unit = "meta_learner", id = "penalized"))

    failure <- out$records[out$records$type == "failure", , drop = FALSE]

    expect_true(out$failed)
    expect_identical(failure$step, "final_fit")
    expect_match(unit_failure_message(failure, "Meta-learner"), "Final model fit failed:", fixed = TRUE)
    expect_match(failure$message, "penalty", fixed = TRUE)

  })

})


## =========================================================================
## fit_tuned_meta_learner() - the step a failure is named by
## =========================================================================

describe("fit_tuned_meta_learner() - the step a failure is named by", {

  ## Runs it as ensemble() does, inside the meta-learner's unit, and returns
  ## the unit's failure record. `fitted` is taken before any mock.
  meta_failure <- function(fitted, optimize) {

    members <- gather_members(fitted)$members
    oof     <- build_oof_matrix(fitted, members)

    spec <- if (optimize) {
      parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune())
    } else {
      parsnip::linear_reg(penalty = 0.01, mixture = 1)
    }

    ## suppressWarnings(): rsample's note that the meta rows are too few for
    ## its default strata breaks, and tune's that every model failed
    out <- suppressWarnings(run_unit(fit_tuned_meta_learner(
      fitted, members, oof,
      rank_metric     = "rpd",
      optimize        = optimize,
      method          = "penalized",
      spec            = parsnip::set_engine(spec, "glmnet"),
      grid            = tidyr::expand_grid(penalty = c(1e-4, 1e-2), mixture = 1),
      extract_weights = extract_weights_penalized
    ), unit = "meta_learner", id = "penalized"))

    expect_true(out$failed)

    out$records[out$records$type == "failure", , drop = FALSE]

  }

  it("names the grid search when every candidate fails", {

    fitted <- ens_fitted()

    local_mocked_bindings(glmnet = function(...) stop("the engine broke"), .package = "glmnet")

    failure <- meta_failure(fitted, optimize = TRUE)

    expect_identical(failure$step, "tune_grid")
    expect_match(unit_failure_message(failure, "Meta-learner"),
                 "Grid search failed: every meta-learner candidate failed", fixed = TRUE)
    expect_match(failure$message, "the engine broke", fixed = TRUE)

  })

  it("names the workflow finalization when it errors", {

    fitted <- ens_fitted()

    local_mocked_bindings(finalize_workflow = function(...) stop("cannot finalize"),
                          .package = "tune")

    failure <- meta_failure(fitted, optimize = TRUE)

    expect_identical(failure$step, "finalize_workflow")
    expect_match(unit_failure_message(failure, "Meta-learner"),
                 "Workflow finalization failed: cannot finalize", fixed = TRUE)

  })

  it("names the out-of-fold pass when every fold of it fails", {

    fitted <- ens_fitted()

    ## The refit on the whole matrix succeeds; the pass refits a spec parsnip
    ## refuses (a negative penalty) on every fold
    real_resamples <- tune::fit_resamples
    refused        <- parsnip::linear_reg(penalty = -1, mixture = 1) |>
      parsnip::set_engine("glmnet")

    local_mocked_bindings(fit_resamples = function(object, ...) {
      real_resamples(workflows::update_model(object, refused), ...)
    }, .package = "tune")

    failure <- meta_failure(fitted, optimize = FALSE)

    expect_identical(failure$step, "oof_predictions")
    expect_match(unit_failure_message(failure, "Meta-learner"),
                 "OOF predictions failed: the meta-learner's out-of-fold pass failed", fixed = TRUE)
    expect_match(failure$message, "penalty", fixed = TRUE)

  })

  it("names the test metrics when the contract cannot be built", {

    fitted <- ens_fitted()

    local_mocked_bindings(build_ensemble_contract = function(...) stop("the contract broke"))

    failure <- meta_failure(fitted, optimize = FALSE)

    expect_identical(failure$step, "test_metrics")
    expect_match(unit_failure_message(failure, "Meta-learner"),
                 "Test metrics failed: the contract broke", fixed = TRUE)

  })

})


## =========================================================================
## fit_ensemble_weighted() - the step a failure is named by
## =========================================================================

describe("fit_ensemble_weighted() - the step a failure is named by", {

  ## Runs it as ensemble() does, inside the meta-learner's unit, and returns
  ## the unit's failure record. `fitted` is taken before any mock.
  weighted_failure <- function(fitted) {

    members <- gather_members(fitted)$members
    oof     <- build_oof_matrix(fitted, members)

    out <- run_unit(
      fit_ensemble_weighted(fitted, members, oof, rank_metric = "rpd", optimize = FALSE),
      unit = "meta_learner", id = "weighted"
    )

    expect_true(out$failed)

    out$records[out$records$type == "failure", , drop = FALSE]

  }

  it("names the member weights when they cannot be derived", {

    fitted <- ens_fitted()

    local_mocked_bindings(derive_member_weights = function(...) stop("the weights broke"))

    failure <- weighted_failure(fitted)

    expect_identical(failure$step, "member_weights")
    expect_match(unit_failure_message(failure, "Meta-learner"),
                 "Member weights failed: the weights broke", fixed = TRUE)

  })

  it("names the combination when the members' test predictions cannot be combined", {

    fitted <- ens_fitted()

    ## Test predictions without the sample ids the combination groups by
    local_mocked_bindings(predict_members_on_test = function(object, members, ...) {
      tibble::tibble(config_id = members, .pred = 1, truth = 1)
    })

    failure <- weighted_failure(fitted)

    expect_identical(failure$step, "combine")
    expect_match(unit_failure_message(failure, "Meta-learner"),
                 "Ensemble combination failed:", fixed = TRUE)
    expect_match(failure$message, "sample_id", fixed = TRUE)

  })

  it("names the test metrics when the contract cannot be built", {

    fitted <- ens_fitted()

    local_mocked_bindings(build_ensemble_contract = function(...) stop("the contract broke"))

    failure <- weighted_failure(fitted)

    expect_identical(failure$step, "test_metrics")
    expect_match(unit_failure_message(failure, "Meta-learner"),
                 "Test metrics failed: the contract broke", fixed = TRUE)

  })

})


## =========================================================================
## fit_tuned_meta_learner() - the warnings tune noted
## =========================================================================

describe("fit_tuned_meta_learner() - the warnings tune noted", {

  it("records the chosen candidate's and the out-of-fold pass's warnings as cross-validation warnings", {

    fitted  <- ens_fitted()
    members <- gather_members(fitted)$members
    oof     <- build_oof_matrix(fitted, members)

    ## A warning on the first fold of the grid's one candidate, and one on
    ## the first fold of the out-of-fold pass
    noted <- function(real, text) {
      force(real)
      function(...) {
        res <- real(...)
        res$.notes[[1]] <- dplyr::bind_rows(res$.notes[[1]], tibble::tibble(
          location = "preprocessor 1/1, model 1/1",
          type     = "warning",
          note     = text,
          trace    = list(NULL)
        ))
        res
      }
    }

    local_mocked_bindings(
      tune_grid     = noted(tune::tune_grid, "the tuning warned"),
      fit_resamples = noted(tune::fit_resamples, "the out-of-fold pass warned"),
      .package      = "tune"
    )

    spec <- parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) |>
      parsnip::set_engine("glmnet")

    ## suppressWarnings(): rsample's note that the meta rows are too few for
    ## its default strata breaks
    contract <- suppressWarnings(fit_tuned_meta_learner(
      fitted, members, oof,
      rank_metric     = "rpd",
      optimize        = TRUE,
      method          = "penalized",
      spec            = spec,
      grid            = tibble::tibble(penalty = 1e-2, mixture = 1),
      extract_weights = extract_weights_penalized
    ))

    log <- attr(contract, "warning_log")

    for (text in c("the tuning warned", "the out-of-fold pass warned")) {

      mine <- log[grepl(text, log$cause, fixed = TRUE), , drop = FALSE]

      expect_identical(nrow(mine), 1L, info = text)
      expect_identical(mine$scope, "cv", info = text)
      expect_identical(mine$n_folds, 5L, info = text)

    }

  })

})


## =========================================================================
## fit_tuned_meta_learner() - the clamped combination (#104)
## =========================================================================

describe("fit_tuned_meta_learner() - the clamped combination (#104)", {

  it("scores its candidates and its out-of-fold pass on the combination clamped to the range", {

    fitted  <- ens_fitted()
    members <- gather_members(fitted)$members
    oof     <- build_oof_matrix(fitted, members)

    ## Truth and members moved down by the median, and the truth floored at
    ## zero: a linear combination of the members fits that hinge with values
    ## below the default range's floor at the low end
    shift <- stats::median(oof$truth)
    oof$truth <- pmax(oof$truth - shift, 0)
    oof$predictors[] <- lapply(oof$predictors, function(v) v - shift)

    ## tune's own results, with the grid's fold predictions kept
    real_grid      <- tune::tune_grid
    real_resamples <- tune::fit_resamples
    seen <- list()
    local_mocked_bindings(
      tune_grid = function(object, ..., control) {
        control$save_pred <- TRUE
        seen$grid <<- real_grid(object, ..., control = control)
        seen$grid
      },
      fit_resamples = function(...) {
        seen$oof <<- real_resamples(...)
        seen$oof
      },
      .package = "tune"
    )

    spec <- parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) |>
      parsnip::set_engine("glmnet")
    grid <- tidyr::expand_grid(penalty = c(1e-4, 1e-2), mixture = c(0, 1))

    ## suppressWarnings(): rsample's note that the meta rows are too few for
    ## its default strata breaks
    suppressWarnings(fit_tuned_meta_learner(
      fitted, members, oof,
      rank_metric     = "rpd",
      optimize        = TRUE,
      method          = "penalized",
      spec            = spec,
      grid            = grid,
      extract_weights = extract_weights_penalized
    ))

    expect_named(seen, c("grid", "oof"), ignore.order = TRUE)

    for (stage in names(seen)) {

      preds <- tune::collect_predictions(seen[[stage]], summarize = FALSE)
      expect_true(any(preds$.pred < 0), info = stage)

      per_fold <- dplyr::summarise(
        preds,
        clamped = yardstick::rmse_vec(.data$.truth, pmax(.data$.pred, 0)),
        raw     = yardstick::rmse_vec(.data$.truth, .data$.pred),
        .by     = c("id", ".config")
      )

      scored <- tune::collect_metrics(seen[[stage]], summarize = FALSE)
      scored <- scored[scored$.metric == "rmse", c("id", ".config", ".estimate")]
      joined <- dplyr::inner_join(per_fold, scored, by = c("id", ".config"))

      expect_equal(nrow(joined), nrow(per_fold), info = stage)
      expect_equal(joined$.estimate, joined$clamped, tolerance = 1e-12, info = stage)
      expect_true(any(abs(joined$raw - joined$clamped) > 1e-6), info = stage)

    }

  })

})


## =========================================================================
## The engines' run time
## =========================================================================

describe("ensemble engines - run time", {

  it("records the run time in seconds when an engine runs past a minute", {

    fitted  <- ens_fitted()
    members <- gather_members(fitted)$members
    oof     <- build_oof_matrix(fitted, members)

    ## A clock that moves 90 s on after the engine's first reading of it
    on_the_clock <- function(code) {
      t0    <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
      reads <- 0L
      with_mocked_bindings(
        code,
        Sys.time = function() {
          reads <<- reads + 1L
          if (reads == 1L) t0 else t0 + 90
        },
        .package = "base"
      )
    }

    weighted <- on_the_clock(
      fit_ensemble_weighted(fitted, members, oof, rank_metric = "rpd", optimize = FALSE)
    )

    spec <- parsnip::linear_reg(penalty = 0.01, mixture = 1) |>
      parsnip::set_engine("glmnet")

    ## suppressWarnings(): rsample's note that the 43 meta rows are too few
    ## for its default strata breaks
    tuned <- on_the_clock(suppressWarnings(fit_tuned_meta_learner(
      fitted, members, oof,
      rank_metric     = "rpd",
      optimize        = FALSE,
      method          = "penalized",
      spec            = spec,
      grid            = NULL,
      extract_weights = extract_weights_penalized
    )))

    expect_equal(weighted$runtime_secs, 90)
    expect_equal(tuned$runtime_secs, 90)

  })

})


## =========================================================================
## Member weights: the weighted engine's rule, and xgboost's importance
## =========================================================================

describe("member weights", {

  it("weights members by their inverse out-of-fold RMSE, normalized to sum to 1", {

    ## Out-of-fold RMSE 1 for member a and 2 for member b
    predictors <- tibble::tibble(member_a = c(1, -1, 1, -1),
                                 member_b = c(2, -2, 2, -2))

    w <- derive_member_weights(predictors, truth = c(0, 0, 0, 0),
                               members = c("a", "b"), optimize = TRUE)

    expect_identical(w$member, c("a", "b"))
    expect_equal(w$coef, c(2 / 3, 1 / 3))

  })

  it("gives a member xgboost never splits on a weight of 0", {

    ## A constant member offers no split
    meta <- withr::with_seed(1, tibble::tibble(member_a = stats::rnorm(40),
                                               member_b = 5))
    meta$.truth <- withr::with_seed(2, 2 * meta$member_a + stats::rnorm(40, sd = 0.1))

    wf <- workflows::workflow() |>
      workflows::add_model(parsnip::boost_tree(trees = 20) |>
                             parsnip::set_engine("xgboost") |>
                             parsnip::set_mode("regression")) |>
      workflows::add_formula(.truth ~ .)

    meta_fit <- parsnip::fit(wf, data = meta)

    ## xgboost's importance table leaves the member out
    imp <- xgboost::xgb.importance(model = hardhat::extract_fit_engine(meta_fit))
    expect_identical(imp$Feature, "member_a")

    w <- extract_weights_xgb(meta_fit, c("a", "b"))

    expect_identical(w$member, c("a", "b"))
    expect_gt(w$coef[1], 0)
    expect_identical(w$coef[2], 0)

  })

})


## =========================================================================
## build_ensemble_contract() - the improvement over the best member
## =========================================================================

describe("build_ensemble_contract() - improvement", {

  it("measures the improvement on the rank metric's side: lower is better for rmse, higher for rpd", {

    truth <- c(10, 20, 30, 40)

    ## Test-set errors: member a +-1 and +-2 (RMSE sqrt(2.5)), member b +-4
    ## (RMSE 4), the ensemble +-0.5 (RMSE 0.5)
    member_pred <- tibble::tibble(
      config_id = rep(c("a", "b"), each = 4),
      sample_id = rep(paste0("s", 1:4), 2),
      .pred     = c(11, 19, 32, 38, 14, 16, 34, 36),
      truth     = rep(truth, 2)
    )
    ensemble_pred <- tibble::tibble(sample_id = paste0("s", 1:4),
                                    .pred     = truth + c(0.5, -0.5, 0.5, -0.5),
                                    truth     = truth)

    contract <- function(rank_metric) {
      build_ensemble_contract(
        method        = "weighted",
        model         = NULL,
        weights       = tibble::tibble(member = c("a", "b"), coef = c(0.5, 0.5)),
        ensemble_pred = ensemble_pred,
        member_pred   = member_pred,
        rank_metric   = rank_metric,
        runtime_secs  = 1,
        optimize      = FALSE,
        seed          = 1L
      )
    }

    expect_equal(contract("rmse")$improvement, sqrt(2.5) - 0.5)
    expect_equal(contract("rpd")$improvement, stats::sd(truth) / 0.5 - stats::sd(truth) / sqrt(2.5))

    ## The stored metrics keep their three columns
    expect_named(contract("rmse")$metrics, c(".metric", ".estimator", ".estimate"))
    expect_named(contract("rmse")$member_metrics, c(".metric", ".estimator", ".estimate", "config_id"))

  })

  it("measures against the members with a value, and is NA when the ensemble or every member has none", {

    truth <- c(10, 20, 30, 40)
    contract <- function(member_pred, ensemble_pred) {
      build_ensemble_contract(
        method = "weighted", model = NULL,
        weights = tibble::tibble(member = c("a", "b"), coef = c(0.5, 0.5)),
        ensemble_pred = ensemble_pred, member_pred = member_pred,
        rank_metric = "rpd", runtime_secs = 1, optimize = FALSE, seed = 1L
      )
    }
    members  <- tibble::tibble(config_id = rep(c("a", "b"), each = 4),
                               sample_id = rep(paste0("s", 1:4), 2),
                               .pred     = c(11, 19, 32, 38, NA, NA, NA, 36),
                               truth     = rep(truth, 2))
    ensemble <- tibble::tibble(sample_id = paste0("s", 1:4),
                               .pred = truth + c(0.5, -0.5, 0.5, -0.5), truth = truth)

    ## Member b has one complete pair, so no rpd: a is the best member, and
    ## nothing warns about an empty max()
    expect_no_warning(got <- contract(members, ensemble))
    expect_equal(got$improvement, stats::sd(truth) / 0.5 - stats::sd(truth) / sqrt(2.5))

    ## No member has a value
    none <- members
    none$.pred[1:3] <- NA
    expect_no_warning(got <- contract(none, ensemble))
    expect_identical(got$improvement, NA_real_)

    ## The ensemble has none, a member does
    unscored <- ensemble
    unscored$.pred[1:3] <- NA
    expect_identical(contract(members, unscored)$improvement, NA_real_)

  })

})


## combine_ensemble_weighted() always finds a weight for every member: its
## predictions are built from weights$member, and the ensemble validator at
## predict()'s door refuses duplicate members and NA coefficients.
