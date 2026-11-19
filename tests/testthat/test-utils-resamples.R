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

  it("round-trips a split that holds out the last row", {

    fx <- make_resample_fixture(n = 60, n_wn = 5, strata = FALSE)

    ## Precondition: the highest training index is below the row count
    expect_lt(max(fx$split$in_id), nrow(fx$data))

    rb <- rebuild_resamples(fx$split$data, resample_indices(fx$split, fx$folds))

    expect_equal(rsample::training(rb$split), rsample::training(fx$split))
    expect_equal(rsample::testing(rb$split),  rsample::testing(fx$split))

  })

  it("cuts the folds from the split's training part itself, the frame a configuration reads (#116)", {

    ## evaluate_single_config() reads its training rows from the folds
    ## rather than copying them out of the split again, so on the configs
    ## axis the rebuilt folds must index that training part, row for row
    fx <- make_resample_fixture(n = 60, n_wn = 5, strata = FALSE)
    rb <- rebuild_resamples(fx$split$data, resample_indices(fx$split, fx$folds))

    expect_identical(rb$cv_folds$splits[[1]]$data, rsample::training(rb$split))

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

  it("never puts an outer test row inside a fold", {

    ## The load-bearing invariant: the held-out test set must not reach the
    ## tuning resamples through the reconstruction. Each fold's rows are read
    ## with analysis(), from the data the fold carries: mapping its indices
    ## through the training frame would pass folds that index the full data.
    fx <- make_resample_fixture()
    rb <- rebuild_resamples(fx$split$data,
                            resample_indices(fx$split, fx$folds))

    test_ids <- rsample::testing(rb$split)$sample_id

    for (s in rb$cv_folds$splits) {

      expect_length(intersect(rsample::analysis(s)$sample_id, test_ids), 0)
      expect_length(intersect(rsample::assessment(s)$sample_id, test_ids), 0)

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

    expect_error(resample_indices(fx$split, folds), "single-id", class = "horizons_error_internal")

  })

  it("rejects a split whose assessment set is not its complement", {

    ## The rebuild derives assessment as setdiff(all, analysis), which would
    ## silently re-partition a gap-carrying rset and hand the model rows it
    ## trained on.
    skip_if_not_installed("rsample")

    sw <- rsample::sliding_window(data.frame(y = rnorm(60)),
                                  lookback = 5, assess_start = 3, assess_stop = 4)

    expect_error(resample_indices(sw$splits[[1]], sw), "complement", class = "horizons_error_internal")

  })

})

describe("rebuild_resamples() fails at the boundary", {

  it("rejects data whose row count disagrees with the indices", {

    ## Otherwise an out-of-range index surfaces as "Grid search failed" for
    ## every config, from inside a worker, with the cause unrecoverable.
    fx  <- make_resample_fixture()
    idx <- resample_indices(fx$split, fx$folds)

    expect_error(rebuild_resamples(fx$data[1:100, ], idx), "same split", class = "horizons_error_internal")

  })

  it("rejects a training index past the data, and a fold index past the training rows", {

    fx  <- make_resample_fixture(n = 60, n_wn = 5, strata = FALSE)
    idx <- resample_indices(fx$split, fx$folds)

    past_data <- idx
    past_data$train_idx[1] <- idx$n_rows + 1L

    expect_error(rebuild_resamples(fx$split$data, past_data),
                 sprintf("got a training index past the data (%d of %d rows).", idx$n_rows + 1L, idx$n_rows),
                 fixed = TRUE, class = "horizons_error_internal")

    past_train <- idx
    past_train$fold_idx[[1]][1] <- length(idx$train_idx) + 1L

    expect_error(rebuild_resamples(fx$split$data, past_train),
                 sprintf("got a fold index past the training rows (%d of %d).",
                         length(idx$train_idx) + 1L, length(idx$train_idx)),
                 fixed = TRUE, class = "horizons_error_internal")

  })

})


## =========================================================================
## Transfer footprint — regression guard
## =========================================================================

describe("rebuild_split() points evaluate()'s split at the object's rows", {

  ## An evaluation with both kinds of row the split leaves out: one with no
  ## measured outcome (#67) and the training rows the response trim (#77)
  ## recorded.
  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$data$analysis$SOC[c(1:4, 31:34)] <- c(20, 25, 30, 35, -15, -20, -25, -30)
  obj$data$analysis$SOC[50] <- NA_real_
  obj$config$outcome_range <- c(-Inf, Inf)

  utils::capture.output(
    v <- suppressWarnings(validate(obj, remove_outliers = "response"))
  )
  ev <- suppressWarnings(evaluate(v, prune = FALSE, seed = 307L))

  it("rebuilds the training and test rows by position from the object's rows", {

    trimmed <- ev$evaluation$response_trim$trimmed_ids
    expect_gt(length(trimmed), 0)

    ## The rows by hand: the measured ones, less the trimmed, in table order;
    ## the split's positions index them.
    full <- ev$data$analysis
    rows <- full[!is.na(full$SOC) & !full$sample_id %in% trimmed, , drop = FALSE]
    in_id <- ev$evaluation$split$in_id

    rebuilt <- rebuild_split(ev)

    expect_identical(rsample::training(rebuilt$split), rows[in_id, , drop = FALSE])
    expect_identical(rsample::testing(rebuilt$split), rows[-in_id, , drop = FALSE])
    expect_identical(rebuilt$trimmed_rows, full[full$sample_id %in% trimmed, , drop = FALSE])
    expect_identical(rebuilt$modelled$n_dropped, 1L)

  })

  it("refuses a split whose rows no longer name the same samples and outcomes", {

    moved <- ev
    moved$data$analysis$SOC[10] <- moved$data$analysis$SOC[10] + 1

    expect_error(rebuild_split(moved), "does not index the rows this object models",
                 class = "horizons_error_input")

  })

})


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
    expect_identical(drawn$strata_error, NA_character_)

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
    expect_identical(drawn$strata_error, "stratification refused")

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

  it("keeps rsample's warnings on a draw it had too little data to stratify, rather than letting them print", {

    ## Under 40 rows rsample cannot bin the outcome and warns twice as it
    ## draws without strata
    small <- df[1:30, , drop = FALSE]

    expect_no_warning(drawn <- draw_stratified(
      outcome      = small$y,
      stratified   = function() rsample::vfold_cv(small, v = 5, strata = "y",
                                                  breaks = STRATA_BREAKS, pool = STRATA_POOL),
      unstratified = function() rsample::vfold_cv(small, v = 5)
    ))

    expect_false(drawn$stratified)
    expect_false(drawn$strata_failed)
    expect_length(drawn$strata_warnings, 2L)
    expect_true(any(grepl("Too little data to stratify", drawn$strata_warnings, fixed = TRUE)))

  })

  it("keeps rsample's warning on a draw it stratified on fewer bins than asked", {

    ## Between 40 and 79 rows the quartiles hold under 20 rows each, so
    ## rsample uses fewer bins, and still stratifies
    mid <- df[1:60, , drop = FALSE]

    expect_no_warning(drawn <- draw_stratified(
      outcome      = mid$y,
      stratified   = function() rsample::vfold_cv(mid, v = 5, strata = "y",
                                                  breaks = STRATA_BREAKS, pool = STRATA_POOL),
      unstratified = function() rsample::vfold_cv(mid, v = 5)
    ))

    expect_true(drawn$stratified)
    expect_length(drawn$strata_warnings, 1L)
    expect_match(drawn$strata_warnings, "Stratification will use 3 breaks instead", fixed = TRUE)

  })

  it("keeps no warning from a stratified draw that failed, whose failure is the record", {

    drawn <- draw_stratified(
      outcome      = df$y,
      stratified   = function() {
        warning("on the way to failing")
        stop("stratification refused")
      },
      unstratified = function() "the unstratified draw"
    )

    expect_true(drawn$strata_failed)
    expect_identical(drawn$strata_warnings, character(0))

  })

})

describe("log_strata_fallback()", {

  logged <- function(drawn) {
    collector <- log_open("evaluate")
    log_strata_fallback(collector, drawn, "train/test split")
    log_close(collector)
  }

  it("records a draw rsample had too little data to stratify as the fallback's substitution, in rsample's words", {

    rows <- logged(list(stratified = FALSE, strata_failed = FALSE, strata_error = NA_character_,
                        strata_warnings = c("first {word} of rsample's", "Too little data to stratify.")))

    expect_identical(rows$type, "warning")
    expect_identical(rows$topic, "substitution")
    expect_match(rows$message, "The stratified draw of the train/test split found one stratum in the outcome, so it was drawn without strata.",
                 fixed = TRUE)
    expect_match(rows$message, "first {word} of rsample's", fixed = TRUE)
    expect_match(rows$message, "Too little data to stratify.", fixed = TRUE)

  })

  ## The real draws, as the verbs make them: rsample leaves each unstratified,
  ## two of them without a word
  drawn_on <- function(y) {
    df <- data.frame(y = y)
    draw_stratified(
      outcome      = y,
      stratified   = function() rsample::vfold_cv(df, v = 5, strata = "y",
                                                  breaks = STRATA_BREAKS, pool = STRATA_POOL),
      unstratified = function() rsample::vfold_cv(df, v = 5)
    )
  }

  it("records a draw rsample left unstratified without a warning as the substitution: a zero-inflated outcome", {

    ## 85 % zeros: every quartile but the top is 0, so the bins collapse to one
    set.seed(1)
    drawn <- drawn_on(c(rep(0, 85), runif(15)))

    expect_false(drawn$stratified)
    expect_false(drawn$strata_failed)
    expect_identical(drawn$strata_warnings, character(0))

    rows <- logged(drawn)

    expect_identical(rows$type, "warning")
    expect_identical(rows$topic, "substitution")
    expect_identical(rows$message, "The stratified draw of the train/test split found one stratum in the outcome, so it was drawn without strata.")

  })

  it("records a draw rsample left unstratified without a warning as the substitution: a few-valued outcome pooled into one stratum", {

    ## 2 is under 10 % of the 30 rows, so it is pooled into 1
    drawn <- drawn_on(c(rep(1, 28), rep(2, 2)))

    expect_false(drawn$stratified)
    expect_identical(drawn$strata_warnings, character(0))

    rows <- logged(drawn)

    expect_identical(rows$topic, "substitution")
    expect_match(rows$message, "found one stratum in the outcome", fixed = TRUE)

  })

  it("claims no shortage of data for a draw tied quantiles left unstratified, and gives rsample's words", {

    ## 60 rows ask rsample for fewer bins, and 70 % zeros tie them into one:
    ## its warning is about the bins, not about too little data to stratify
    set.seed(1)
    drawn <- drawn_on(c(rep(0, 42), runif(18)))

    expect_false(drawn$stratified)
    expect_length(drawn$strata_warnings, 1L)

    rows <- logged(drawn)

    expect_identical(rows$topic, "substitution")
    expect_match(rows$message, "found one stratum in the outcome, so it was drawn without strata.", fixed = TRUE)
    expect_match(rows$message, "Stratification will use 3 breaks instead", fixed = TRUE)
    expect_no_match(rows$message, "too little data", fixed = TRUE)

  })

  it("records a draw rsample stratified on fewer bins as a note, since it still stratified", {

    rows <- logged(list(stratified = TRUE, strata_failed = FALSE, strata_error = NA_character_,
                        strata_warnings = "Stratification will use 3 breaks instead."))

    expect_identical(rows$type, "note")
    expect_identical(rows$topic, NA_character_)
    expect_match(rows$message, "had too little data for 4 strata, so rsample drew it on fewer.", fixed = TRUE)
    expect_match(rows$message, "Stratification will use 3 breaks instead.", fixed = TRUE)

  })

  it("records nothing for a draw that stratified as asked", {

    rows <- logged(list(stratified = TRUE, strata_failed = FALSE, strata_error = NA_character_,
                        strata_warnings = character(0)))

    expect_identical(nrow(rows), 0L)

  })

})

describe("report_strata_in_unit()", {

  ## A unit cannot reach the verb's collector, so ensemble()'s draws report
  ## from inside theirs; the verb lifts what they report onto its log
  unstratified <- list(stratified = FALSE, strata_failed = FALSE, strata_error = NA_character_,
                       strata_warnings = "Too little {data} to stratify.")
  fewer        <- list(stratified = TRUE, strata_failed = FALSE, strata_error = NA_character_,
                       strata_warnings = "Stratification will use 3 breaks instead.")

  it("reports from inside a unit, with its class, what log_strata_fallback() records, in the same words", {

    out <- run_unit({
      report_strata_in_unit(unstratified, "CV+ calibration folds")
      report_strata_in_unit(fewer, "CV+ calibration folds")
    }, unit = "intervals", id = "ensemble")

    expect_identical(out$records$type, c("warning", "note"))
    expect_identical(out$records$class, c("horizons_warning_substitution", "horizons_note"))

    lifted <- log_open("ensemble")
    log_unit_records(lifted, out$records)
    lifted <- log_close(lifted)

    direct <- log_open("ensemble")
    log_strata_fallback(direct, unstratified, "CV+ calibration folds")
    log_strata_fallback(direct, fewer, "CV+ calibration folds")
    direct <- log_close(direct)

    expect_identical(lifted$type, direct$type)
    expect_identical(lifted$topic, direct$topic)

    ## The same header and rsample's words, and the unit's line
    for (said in c("The stratified draw of the CV+ calibration folds found one stratum in the outcome, so it was drawn without strata.",
                   "Too little {data} to stratify.")) {
      expect_match(direct$message[1], said, fixed = TRUE)
      expect_match(lifted$message[1], said, fixed = TRUE)
    }
    for (said in c("The stratified draw of the CV+ calibration folds had too little data for 4 strata, so rsample drew it on fewer.",
                   "Stratification will use 3 breaks instead.")) {
      expect_match(direct$message[2], said, fixed = TRUE)
      expect_match(lifted$message[2], said, fixed = TRUE)
    }
    expect_true(all(grepl("In the ensemble's prediction intervals.", lifted$message, fixed = TRUE)))

  })

  it("reports nothing for a draw that stratified as asked", {

    out <- run_unit(
      report_strata_in_unit(list(stratified = TRUE, strata_failed = FALSE, strata_error = NA_character_,
                                 strata_warnings = character(0)), "CV+ calibration folds"),
      unit = "intervals", id = "ensemble"
    )

    expect_identical(nrow(out$records), 0L)

  })

})

describe("strata_tree_line()", {

  it("gives no line for a draw that stratified as asked", {

    expect_null(strata_tree_line(list(stratified = TRUE, strata_failed = FALSE, strata_error = NA_character_,
                                      strata_warnings = character(0)), "split"))

  })

  it("gives no line for a draw rsample left unstratified, even with its warnings", {

    ## Its own line in the tree says "unstratified"
    expect_null(strata_tree_line(list(stratified = FALSE, strata_failed = FALSE, strata_error = NA_character_,
                                      strata_warnings = "Too little data to stratify."), "split"))

  })

  it("names a draw rsample stratified on fewer bins, and one whose stratified draw failed", {

    expect_identical(
      strata_tree_line(list(stratified = TRUE, strata_failed = FALSE, strata_error = NA_character_,
                            strata_warnings = "Stratification will use 3 breaks instead."), "CV"),
      "Stratified CV on fewer strata: too little data for 4"
    )
    expect_identical(
      strata_tree_line(list(stratified = FALSE, strata_failed = TRUE, strata_error = "stratification refused",
                            strata_warnings = character(0)), "split"),
      "Stratified split failed, retrying without strata"
    )

  })

})


## =========================================================================
## prep_folds(): each fold's recipe prepped once, the folds tune searches
## =========================================================================
##
## A fold's frame is built from the transformed training rows (the hoist):
## the outcome and the prepped predictors, each placed from the fold's bakes,
## and a column identical to the transformed rows' column of the same name
## (a band a selection kept) is that column itself, shared. These hold it to
## the fold baked whole, to sharing what it left as it was, and to holding
## nothing a model could take as a predictor besides.

## Helper: training rows with spectra, an id, a meta column and a sibling
## response; three folds; and one configuration's fold preps
prep_fold_case <- function(selection, transformation = "none", n = 60, n_wn = 30) {

  set.seed(8)

  wn <- paste0("wn_", seq(4000, by = -4, length.out = n_wn))
  x  <- matrix(stats::rnorm(n * n_wn), n, dimnames = list(NULL, wn))

  train           <- tibble::as_tibble(x)
  train$SOC       <- 5 + 1.5 * x[, 5] + x[, 12] + stats::rnorm(n, sd = 0.5)
  train$sample_id <- sprintf("S%03d", seq_len(n))
  train$batch     <- rep(c("a", "b", "c"), length.out = n)
  train$clay      <- stats::runif(n)

  role_map <- tibble::tibble(
    variable = c(wn, "SOC", "sample_id", "batch", "clay"),
    role     = c(rep("predictor", n_wn), "outcome", "id", "meta", "response")
  )

  folds  <- rsample::vfold_cv(train, v = 3)
  config <- tibble::tibble(config_id = "cfg", model = "elastic_net", transformation = transformation,
                           preprocessing = "snv", feature_selection = selection)

  inputs <- fold_prep_inputs(config, train, role_map, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD)
  scope  <- fold_prep_scope("evaluate", config)

  list(train = train, role_map = role_map, folds = folds, inputs = inputs, scope = scope,
       prepped = prep_folds(inputs$recipe, folds, inputs$data, role_map, seed = 8L, scope = scope))

}

## The case's folds, each fold's frame against the same prep's bakes
expect_folds_baked_whole <- function(case, label) {

  for (i in seq_len(nrow(case$folds))) {

    in_id  <- case$folds$splits[[i]]$in_id
    out_id <- rsample::complement(case$folds$splits[[i]])
    split  <- case$prepped$folds$splits[[i]]
    cols   <- names(split$data)

    p  <- with_unit_seed(8L, c(case$scope, case$folds$id[[i]]), quiet_selection_warnings(
      recipes::prep(case$inputs$recipe, training = case$inputs$data[in_id, , drop = FALSE])
    ))
    an <- recipes::bake(p, new_data = NULL)
    as <- recipes::bake(p, new_data = case$inputs$data[out_id, , drop = FALSE])

    expect_identical(as.data.frame(rsample::analysis(split)), as.data.frame(an[, cols]),
                     label = paste(label, case$folds$id[[i]], "analysis"))
    expect_identical(as.data.frame(rsample::assessment(split)), as.data.frame(as[, cols]),
                     label = paste(label, case$folds$id[[i]], "assessment"))

  }

}

describe("prep_folds()", {

  it("assembles every fold as the fold baked whole, for no selection, PCA and correlation", {

    for (selection in c("none", "pca", "correlation")) {

      expect_folds_baked_whole(prep_fold_case(selection), selection)

    }

    ## Under a transformed response too: the analysis rows' outcome
    ## transformed, the assessment rows' as measured
    logged <- prep_fold_case("correlation", transformation = "log")
    expect_folds_baked_whole(logged, "correlation, log")

    for (i in seq_len(nrow(logged$folds))) {
      out_id <- rsample::complement(logged$folds$splits[[i]])
      expect_identical(rsample::assessment(logged$prepped$folds$splits[[i]])$SOC, logged$train$SOC[out_id])
    }

  })

  it("assembles every fold as the fold baked whole, for Boruta and CARS", {

    skip_unless_slow_tier()
    skip_if_not_installed("Boruta")

    for (selection in c("boruta", "cars")) {

      expect_folds_baked_whole(prep_fold_case(selection), selection)

    }

  })

  it("assembles a fold as the fold baked whole when a step rewrites a band in place", {

    ## A step that changes a band under its own name: the fold's frame holds
    ## the fold's bake of it, not the transformed rows' column of that name
    case <- prep_fold_case("none")
    case$inputs$recipe <- case$inputs$recipe |> recipes::step_mutate(spec01 = spec01 * 2)
    case$prepped <- prep_folds(case$inputs$recipe, case$folds, case$inputs$data, case$role_map,
                               seed = 8L, scope = case$scope)

    expect_folds_baked_whole(case, "spec01 doubled")

  })

  it("shares each column a fold left as it was with the transformed rows, rather than copying it", {

    ## Ten folds of a library-sized frame hold one copy of its bands, not ten
    addresses <- function(frame, cols) vapply(cols, function(col) rlang::obj_address(frame[[col]]), character(1))

    for (selection in c("none", "correlation")) {

      case <- prep_fold_case(selection)

      for (i in seq_len(nrow(case$folds))) {

        frame <- case$prepped$folds$splits[[i]]$data
        kept  <- intersect(names(frame), names(case$inputs$data))

        expect_gt(length(kept), 1L)
        expect_identical(addresses(frame, kept), addresses(case$inputs$data, kept),
                         label = paste(selection, case$folds$id[[i]]))

      }

    }

  })

  it("holds the outcome and the prepped predictors only, at the training rows' positions, with the folds' ids", {

    case <- prep_fold_case("none")
    spec <- grep("^spec[0-9]+$", names(case$inputs$data), value = TRUE)

    expect_identical(case$prepped$folds$id, case$folds$id)

    for (i in seq_len(nrow(case$folds))) {

      split <- case$prepped$folds$splits[[i]]

      ## No id, meta or held response for a model to take as a predictor
      expect_identical(names(split$data), c("SOC", spec))
      expect_identical(nrow(split$data), nrow(case$train))
      expect_identical(split$in_id, case$folds$splits[[i]]$in_id)
      expect_identical(as.integer(split, data = "assessment"), rsample::complement(case$folds$splits[[i]]))

    }

  })

  it("records another package's warning with the folds that raised it", {

    ## Two indicator columns, each constant on one fold's analysis rows only:
    ## step_corr() warns of the correlation it cannot compute on those two
    ## folds, and changes no column
    case   <- prep_fold_case("none")
    ids_of <- function(i) case$train$sample_id[rsample::complement(case$folds$splits[[i]])]
    recipe <- case$inputs$recipe |>
      recipes::step_mutate(z1 = as.numeric(sample_id %in% !!ids_of(1)),
                           z2 = as.numeric(sample_id %in% !!ids_of(2))) |>
      recipes::step_corr(recipes::all_predictors())

    out <- prep_folds(recipe, case$folds, case$inputs$data, case$role_map, seed = 8L, scope = case$scope)

    excluded <- grepl("The correlation matrix has missing values", out$warnings$cause, fixed = TRUE)

    expect_identical(sum(excluded), 1L)
    expect_identical(out$warnings$scope[excluded], "cv")
    expect_identical(out$warnings$folds[excluded][[1]], case$folds$id[1:2])
    expect_identical(out$warnings$n[excluded], 2L)
    expect_identical(out$warnings$n_folds[excluded], 3L)
    expect_match(render_warning_log(out$warnings), "The correlation matrix has missing values.*2 of 3 folds",
                 all = FALSE)

  })

})

describe("fold_prep_inputs()", {

  it("builds the transformed rows and the fold recipe without moving the caller's stream", {

    ## The whole recipe, built before them, draws its step ids from the
    ## stream (recipes::rand_id()), so the recipe stored is the one it was.
    ## The case's training rows and role map; a log response and PCA, whose
    ## steps draw ids.
    case   <- prep_fold_case("none")
    config <- tibble::tibble(config_id = "cfg", model = "elastic_net", transformation = "log",
                             preprocessing = "snv", feature_selection = "pca")

    withr::local_seed(3)
    before <- .Random.seed

    fold_prep_inputs(config, case$train, case$role_map, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD)

    expect_identical(.Random.seed, before)

  })

})

describe("fold_prep_scope()", {

  it("names a fold's prep by the response transformation only for a selection that reads the outcome", {

    ## Two configurations that differ only in the transformation must draw
    ## other seeds for a selection that selects on the outcome, and the same
    ## for one that does not
    reads_outcome <- c(none = FALSE, pca = FALSE, correlation = TRUE, boruta = TRUE, cars = TRUE)

    expect_setequal(names(reads_outcome), VALID_FEATURE_SELECTION)

    for (verb in c("evaluate", "fit")) {

      for (selection in names(reads_outcome)) {

        config <- tibble::tibble(config_id = "cfg", model = "rf", transformation = "log",
                                 preprocessing = "snv", feature_selection = selection)

        expect_identical(fold_prep_scope(verb, config),
                         c(verb, "prep", "snv", selection, if (reads_outcome[[selection]]) "log"),
                         label = paste(verb, selection))

      }

    }

  })

})

describe("fold_workflow()", {

  it("has the parameter set of the configuration's recipe workflow, since no recipe step is tuned", {

    ## tune searches the model alone over prepped folds, while the grid and
    ## the bounds are read from the workflow with the whole recipe, so the
    ## two must tune the same parameters: no recipe step is tuned, under any
    ## selection or transformation
    withr::local_seed(1)
    wn <- paste0("wn_", seq(4000, by = -4, length.out = 20))
    d  <- tibble::as_tibble(matrix(stats::rnorm(30 * 20), 30, dimnames = list(NULL, wn)))
    d$SOC       <- stats::runif(30, 1, 5)
    d$sample_id <- sprintf("S%02d", 1:30)
    role_map <- tibble::tibble(variable = c(wn, "SOC", "sample_id"),
                               role     = c(rep("predictor", 20), "outcome", "id"))

    config_of <- function(selection, transformation) {
      tibble::tibble(model = "rf", transformation = transformation, preprocessing = "snv",
                     feature_selection = selection)
    }

    for (selection in VALID_FEATURE_SELECTION) {

      for (transformation in VALID_TRANSFORMATIONS) {

        expect_identical(nrow(tune::tune_args(build_recipe(config_of(selection, transformation), d, role_map))),
                         0L, label = paste(selection, transformation))

      }

    }

    ## So the model alone tunes what the whole workflow tunes: rf's mtry,
    ## whose bounds are read from the recipe, among them
    spec  <- define_model_spec("rf")
    whole <- workflows::workflow() |>
      workflows::add_recipe(build_recipe(config_of("pca", "log"), d, role_map)) |>
      workflows::add_model(spec)

    expect_identical(workflows::extract_parameter_set_dials(fold_workflow(spec, "SOC")),
                     workflows::extract_parameter_set_dials(whole))

  })

})
