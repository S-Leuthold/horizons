## ---------------------------------------------------------------------------
## Tests: ensemble-helpers.R
## ---------------------------------------------------------------------------
## The refusals inside the meta-learner scaffold and the weighted combine, and
## the engines' parts that the fixture cannot reach: the improvement on a
## metric where lower is better (the fixture ranks by rpd), the weights of the
## weighted engine's tuned rule and of a member xgboost never splits on, and
## the run time of an engine that runs past a minute. All are called
## directly: predict() always hands the combine a member set that matches the
## weights. ensemble() and predict.horizons_ensemble() are tested
## in test-pipeline-ensemble.R.
##
## ens_fitted() comes from helper-ensemble.R.


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
