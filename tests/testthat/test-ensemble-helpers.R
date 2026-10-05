## ---------------------------------------------------------------------------
## Tests: ensemble-helpers.R
## ---------------------------------------------------------------------------
## The refusals inside the meta-learner scaffold and the weighted combine.
## Both are called directly: ensemble() reaches the rank-metric check only
## through a full tuning run, and predict() always hands the combine a member
## set that matches the weights. ensemble() and predict.horizons_ensemble()
## are tested in test-pipeline-ensemble.R.
##
## ens_fitted() comes from helper-ensemble.R.


## =========================================================================
## fit_tuned_meta_learner() - rank metric
## =========================================================================

describe("fit_tuned_meta_learner() - rank metric", {

  it("refuses a rank metric the tuning did not collect", {

    fitted  <- ens_fitted()
    members <- gather_members(fitted)$members
    oof     <- build_oof_matrix(fitted, members)

    ## One grid point is enough: the check sits between tuning and
    ## select_best().
    spec <- parsnip::linear_reg(penalty = tune::tune(), mixture = 1) |>
      parsnip::set_engine("glmnet")

    ## The abort carries no package class. suppressWarnings(): rsample's note
    ## that the 43 meta rows are too few for its default strata breaks.
    expect_error(
      suppressWarnings(fit_tuned_meta_learner(
        fitted, members, oof,
        rank_metric     = "rpiq",
        optimize        = TRUE,
        method          = "penalized",
        spec            = spec,
        grid            = tibble::tibble(penalty = 0.01),
        extract_weights = extract_weights_penalized
      )),
      'Cannot rank the penalized ensemble by "rpiq"', fixed = TRUE
    )

  })

})


## =========================================================================
## combine_ensemble_weighted() - member set
## =========================================================================

describe("combine_ensemble_weighted() - member set", {

  it("refuses a member that has no weight rather than returning NA", {

    member_pred <- tibble::tibble(
      config_id = rep(c("a", "b"), each = 2),
      sample_id = rep(c("s1", "s2"), 2),
      .pred     = c(10, 12, 11, 13)
    )
    weights <- tibble::tibble(member = "a", coef = 1)

    ## The abort carries no package class.
    expect_error(combine_ensemble_weighted(member_pred, weights),
                 "Weighted combine has no weight for 1 member", fixed = TRUE)

  })

})
