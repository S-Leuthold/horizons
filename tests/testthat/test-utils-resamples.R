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


## ---------------------------------------------------------------------------
## outcome_stratifies() and draw_stratified(): whether the strata held (#91)
## ---------------------------------------------------------------------------
## rsample draws unstratified without an error in several cases, and the
## returned object's `strata` attribute names the outcome either way, so the
## console said "stratified" regardless. These pin the cases the helper has to
## call unstratified, against the rsample the package imports.

describe("outcome_stratifies()", {

  it("is TRUE for a continuous outcome with enough rows to bin", {

    set.seed(1)

    expect_true(outcome_stratifies(rnorm(100)))

    ## 40 to 79 rows: rsample warns and uses fewer bins, but still stratifies
    expect_true(outcome_stratifies(rnorm(40)))

  })

  it("is FALSE below 40 rows, where rsample warns and draws unstratified", {

    set.seed(1)
    y <- rnorm(39)

    expect_false(outcome_stratifies(y))

    ## The draw itself says so only as a warning (after one about the bin
    ## count, muffled here); its strata attribute names the outcome all the same
    expect_warning(
      keep_only_warning(
        split <- rsample::initial_split(data.frame(y = y), prop = 0.8, strata = "y"),
        "Too little data to stratify"
      ),
      "Too little data to stratify"
    )
    expect_identical(attr(split, "strata"), "y")

  })

  it("is FALSE when rsample pools a few-valued outcome into one stratum, with no warning", {

    y <- c(rep(1, 46), rep(2, 4))

    expect_false(outcome_stratifies(y))
    expect_no_warning(rsample::vfold_cv(data.frame(y = y), v = 5, strata = "y"))

  })

  it("is FALSE when tied quantiles leave one bin (a zero-inflated outcome)", {

    set.seed(1)

    expect_false(outcome_stratifies(c(rep(0, 90), runif(10))))

  })

  ## Pooling draws a random stratum for each pooled row inside make_strata().
  ## 5 is under 10 % of these rows and pools into four strata; pooling into
  ## one stratum samples from one level and draws nothing, so it would not
  ## show a missing restore.
  pooled_y <- c(rep(1:4, each = 23), rep(5, 4))

  it("leaves the caller's RNG stream where it found it", {

    ## Precondition: make_strata() itself moves the stream on this outcome
    set.seed(42)
    before <- .Random.seed
    invisible(rsample::make_strata(pooled_y))
    expect_false(identical(.Random.seed, before))

    set.seed(42)
    before <- .Random.seed
    outcome_stratifies(pooled_y)

    expect_identical(.Random.seed, before)

  })

  it("leaves .Random.seed absent when it was absent", {

    y <- pooled_y

    ## Restore the session's stream after the test, whatever it was
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
    withr::defer(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()))

    if (had_seed) rm(".Random.seed", envir = globalenv())

    outcome_stratifies(y)

    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

  })

})

describe("draw_stratified()", {

  df <- data.frame(y = seq_len(100) / 10)

  it("reports a stratified draw when the strata held", {

    drawn <- draw_stratified(
      outcome      = df$y,
      stratified   = function() rsample::vfold_cv(df, v = 5, strata = "y"),
      unstratified = function() rsample::vfold_cv(df, v = 5)
    )

    expect_s3_class(drawn$draw, "vfold_cv")
    expect_true(drawn$stratified)
    expect_false(drawn$strata_failed)

  })

  it("reports an unstratified draw when rsample dropped the strata, with no failure", {

    small <- df[1:30, , drop = FALSE]

    drawn <- suppressWarnings(draw_stratified(
      outcome      = small$y,
      stratified   = function() rsample::vfold_cv(small, v = 5, strata = "y"),
      unstratified = function() rsample::vfold_cv(small, v = 5)
    ))

    expect_false(drawn$stratified)
    expect_false(drawn$strata_failed)

  })

  it("falls back to the unstratified draw when the stratified one fails", {

    drawn <- draw_stratified(
      outcome      = df$y,
      stratified   = function() stop("stratification refused"),
      unstratified = function() "the unstratified draw"
    )

    expect_identical(drawn$draw, "the unstratified draw")
    expect_false(drawn$stratified)
    expect_true(drawn$strata_failed)

  })

  it("draws from the stream the caller seeded, as the inline draws did", {

    ## A pooled outcome (5 is under 10 % of the rows), so make_strata() draws
    ## random numbers: without the restore, the check would move the stream
    ## the draw then reads, and the folds would differ.
    pooled <- data.frame(y = c(rep(1:4, each = 23), rep(5, 4)))

    set.seed(1)
    before <- .Random.seed
    invisible(rsample::make_strata(pooled$y))
    expect_false(identical(.Random.seed, before))

    ## The inline draw as it was, at rsample's defaults
    set.seed(307)
    inline <- rsample::vfold_cv(pooled, v = 5, strata = "y")

    set.seed(307)
    drawn <- draw_stratified(
      outcome      = pooled$y,
      stratified   = function() rsample::vfold_cv(pooled, v = 5, strata = "y",
                                                  breaks = STRATA_BREAKS, pool = STRATA_POOL),
      unstratified = function() rsample::vfold_cv(pooled, v = 5)
    )

    expect_true(drawn$stratified)
    expect_identical(lapply(drawn$draw$splits, `[[`, "in_id"),
                     lapply(inline$splits, `[[`, "in_id"))

  })

  it("counts an error from the check as unstratified, and still draws", {

    ## The check used to run outside the fallback, so its error aborted where
    ## the inline draw retried without strata
    local_mocked_bindings(outcome_stratifies = function(outcome) stop("make_strata refused"))

    drawn <- draw_stratified(
      outcome      = df$y,
      stratified   = function() rsample::vfold_cv(df, v = 5, strata = "y"),
      unstratified = function() rsample::vfold_cv(df, v = 5)
    )

    expect_s3_class(drawn$draw, "vfold_cv")
    expect_false(drawn$stratified)
    expect_false(drawn$strata_failed)

  })

})
