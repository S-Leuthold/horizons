## ---------------------------------------------------------------------------
## Tests: resample_indices() and rebuild_resamples()
## ---------------------------------------------------------------------------
##
## These helpers exist to stop evaluate() paying a 5x serialization tax when it
## sends resamples to workers. rsample objects share one data frame by
## reference, but R's serializer does not deduplicate data frames, so each
## reference becomes a full copy on the wire. Measured at KSSL scale
## (17,788 x 1,701): 418 MB resident, 1,158.7 MB serialized, against 231.7 MB
## of unique data.
##
## Two things must hold. The round trip has to be faithful — including the
## attributes tune_grid_loop() reads via rsample::.get_split_args() — and the
## transported payload has to stay small. The ratio is scale-invariant, so
## small fixtures catch a regression.

## Helper: a stratified split + folds, the shape evaluate() builds
make_resample_fixture <- function(n = 400, n_wn = 750, v = 5, strata = TRUE) {

  ## Seed first: the outcome values drive the stratification quantiles, so
  ## seeding only the split would leave the fixture data varying run to run and
  ## the measured footprint ratios computed on a different matrix each time.
  set.seed(307)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%04d", seq_len(n)))
  df$SOC       <- runif(n, 0.5, 10)

  split <- if (strata) {
    rsample::initial_split(df, prop = 0.8, strata = "SOC")
  } else {
    rsample::initial_split(df, prop = 0.8)
  }

  folds <- if (strata) {
    rsample::vfold_cv(rsample::training(split), v = v, strata = "SOC")
  } else {
    rsample::vfold_cv(rsample::training(split), v = v)
  }

  list(data = df, split = split, folds = folds)

}



## =========================================================================
## Round-trip fidelity
## =========================================================================

describe("resample_indices() / rebuild_resamples()", {

  it("reproduces the split rows exactly", {

    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)
    rb  <- rebuild_resamples(fx$split$data, idx)

    expect_equal(rsample::training(rb$split), rsample::training(fx$split))
    expect_equal(rsample::testing(rb$split),  rsample::testing(fx$split))

  })

  it("reproduces every fold's membership exactly", {

    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)
    rb  <- rebuild_resamples(fx$split$data, idx)

    expect_equal(length(rb$cv_folds$splits), length(fx$folds$splits))

    for (i in seq_along(fx$folds$splits)) {

      expect_equal(rsample::analysis(rb$cv_folds$splits[[i]]),
                   rsample::analysis(fx$folds$splits[[i]]))
      expect_equal(rsample::assessment(rb$cv_folds$splits[[i]]),
                   rsample::assessment(fx$folds$splits[[i]]))

    }

  })

  it("restores the attributes tune reads via .get_split_args()", {

    ## tune_grid_loop() passes these to rsample::internal_calibration_split().
    ## manual_rset() does not carry them, so rebuild_resamples() puts them back.
    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)
    rb  <- rebuild_resamples(fx$split$data, idx)

    expect_equal(rsample::.get_split_args(rb$cv_folds),
                 rsample::.get_split_args(fx$folds))

  })

  it("preserves the rset class and ids", {

    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)
    rb  <- rebuild_resamples(fx$split$data, idx)

    expect_equal(class(rb$cv_folds), class(fx$folds))
    expect_equal(rb$cv_folds$id, fx$folds$id)

  })

  it("preserves the SPLIT subclasses, not just the rset's", {

    ## make_splits() returns a bare rsplit unless told otherwise, and
    ## rsample::internal_calibration_split() — the function split_args is
    ## transported for — dispatches on the split subclass with a hard-aborting
    ## default method. A bare rsplit makes the transported attributes unusable.
    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)
    rb  <- rebuild_resamples(fx$split$data, idx)

    expect_equal(class(rb$split), class(fx$split))
    expect_equal(class(rb$cv_folds$splits[[1]]), class(fx$folds$splits[[1]]))

  })

  it("round-trips without strata", {

    fx  <- make_resample_fixture(strata = FALSE)
    idx <- resample_indices(fx$split, fx$folds)
    rb  <- rebuild_resamples(fx$split$data, idx)

    expect_equal(rsample::training(rb$split), rsample::training(fx$split))
    expect_equal(rsample::.get_split_args(rb$cv_folds),
                 rsample::.get_split_args(fx$folds))

  })

  it("round-trips a different fold count", {

    fx  <- make_resample_fixture(v = 3)
    idx <- resample_indices(fx$split, fx$folds)
    rb  <- rebuild_resamples(fx$split$data, idx)

    expect_equal(length(rb$cv_folds$splits), 3)
    expect_equal(rsample::.get_split_args(rb$cv_folds)$v, 3)

  })

})


## =========================================================================
## Partition invariants — absolute, not relative to the original
## =========================================================================
##
## Every test above compares the rebuilt objects to `fx$folds`, so they inherit
## whatever the original does. These assert the properties that must hold of the
## rebuilt objects on their own terms. An index bug in `rebuild_resamples()`
## would produce analysis/assessment contamination — silent, metric-inflating,
## and invisible to an equality test if the original were ever also wrong.

describe("rebuilt resamples satisfy the partition invariants", {

  it("keeps analysis and assessment disjoint at both levels", {

    fx <- make_resample_fixture()
    rb <- rebuild_resamples(fx$split$data,
                            resample_indices(fx$split, fx$folds))

    expect_length(intersect(rb$split$in_id,
                            setdiff(seq_len(nrow(fx$data)), rb$split$in_id)), 0)

    n_train <- nrow(rsample::training(rb$split))

    for (s in rb$cv_folds$splits) {

      expect_length(intersect(s$in_id, setdiff(seq_len(n_train), s$in_id)), 0)

    }

  })

  it("never puts an outer test row inside a fold", {

    ## The load-bearing invariant: the held-out test set must not reach the
    ## tuning resamples through the reconstruction.
    fx <- make_resample_fixture()
    rb <- rebuild_resamples(fx$split$data,
                            resample_indices(fx$split, fx$folds))

    test_ids  <- rsample::testing(rb$split)$sample_id
    train_ids <- rsample::training(rb$split)$sample_id

    for (s in rb$cv_folds$splits) {

      fold_ids <- train_ids[s$in_id]
      expect_length(intersect(fold_ids, test_ids), 0)

    }

  })

  it("covers every training row exactly once across fold assessment sets", {

    fx <- make_resample_fixture()
    rb <- rebuild_resamples(fx$split$data,
                            resample_indices(fx$split, fx$folds))

    n_train  <- nrow(rsample::training(rb$split))
    assessed <- unlist(lapply(rb$cv_folds$splits, function(s) {
      setdiff(seq_len(n_train), s$in_id)
    }))

    expect_equal(sort(assessed), seq_len(n_train))

  })

})


## =========================================================================
## Guards on the assumptions the transport rests on
## =========================================================================

describe("resample_indices() refuses what it cannot round-trip", {

  it("rejects repeated CV rather than dropping id2", {

    ## manual_rset() takes one `ids` vector, so a repeated rset's id2 would be
    ## lost and v * repeats resamples would collapse onto v identifiers.
    fx    <- make_resample_fixture()
    folds <- rsample::vfold_cv(rsample::training(fx$split), v = 3, repeats = 2)

    expect_error(resample_indices(fx$split, folds), "single-id")

  })

  it("rejects a split whose assessment set is not its complement", {

    ## The rebuild derives assessment as setdiff(all, analysis), which would
    ## silently re-partition a gap-carrying rset and hand the model rows it
    ## trained on.
    skip_if_not_installed("rsample")

    sw <- rsample::sliding_window(data.frame(y = rnorm(60)),
                                  lookback = 5, assess_start = 3, assess_stop = 4)

    expect_error(resample_indices(sw$splits[[1]], sw), "complement")

  })

})

describe("rebuild_resamples() fails at the boundary", {

  it("rejects data whose row count disagrees with the indices", {

    ## Otherwise an out-of-range index surfaces as "Grid search failed" for
    ## every config, from inside a worker, with the cause unrecoverable.
    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)

    expect_error(rebuild_resamples(fx$data[1:100, ], idx), "same split")

  })

})


## =========================================================================
## Transfer footprint — regression guard
## =========================================================================

describe("resample transfer footprint", {

  it("carries no data in the index payload", {

    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)

    ## Indices are integers; they must not scale with spectral width.
    expect_lt(serialized_size(idx) / serialized_size(fx$data), 0.05)

  })

  it("sends the data once instead of six times", {

    fx <- make_resample_fixture()

    ## What evaluate() used to export: the split plus the rset, which share one
    ## table in memory and serialize as one + v copies.
    before <- serialized_size(list(fx$split, fx$folds))

    ## What it exports now.
    after  <- serialized_size(list(fx$split$data,
                                   resample_indices(fx$split, fx$folds)))

    data_bytes <- serialized_size(fx$data)

    ## One copy plus change. Two would mean a reference crept back in.
    expect_lt(after / data_bytes, 1.5)

    ## And the new form must actually be a large improvement, not a wash.
    expect_lt(after, before / 2)

  })

  it("does not grow with fold count", {

    ## The old payload grew with v, because every fold carried the table.
    ## The new one must not.
    fx3 <- make_resample_fixture(v = 3)
    fx9 <- make_resample_fixture(v = 9)

    p3 <- serialized_size(list(fx3$split$data, resample_indices(fx3$split, fx3$folds)))
    p9 <- serialized_size(list(fx9$split$data, resample_indices(fx9$split, fx9$folds)))

    expect_lt(p9 / p3, 1.1)

  })

})
