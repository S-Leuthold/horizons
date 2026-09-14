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

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%04d", seq_len(n)))
  df$SOC       <- runif(n, 0.5, 10)

  set.seed(307)
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

serialized_size <- function(x) length(serialize(x, NULL))


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
