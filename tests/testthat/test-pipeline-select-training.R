# tests/testthat/test-pipeline-select-training.R
# Tests for select_training(): the verb end to end.


## =============================================================================
## Helpers
## =============================================================================

#' Run the verb quietly on the fixture
#' @noRd
quiet_select <- function(fx, ...) {

  select_training(fx$targets, fx$pool, ..., verbose = FALSE)

}


## =============================================================================
## Input validation
## =============================================================================

test_that("select_training() rejects inputs that are not horizons_data", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(select_training(data.frame(a = 1), fx$pool, verbose = FALSE),
               class = "horizons_input_error")
  expect_error(select_training(fx$targets, data.frame(a = 1), verbose = FALSE),
               class = "horizons_input_error")

})


test_that("select_training() needs a pool with at least one response", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(select_training(fx$targets, fx$targets, verbose = FALSE),
               regexp = "response", class = "horizons_input_error")

})


test_that("select_training() rejects unknown properties", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, properties = "ph"),
               regexp = "ph", class = "horizons_input_error")

})


test_that("select_training() rejects a bad k", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 0),           class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 2.5),         class = "horizons_input_error")
  expect_error(quiet_select(fx, k = c(clay = 5)), regexp = "oc", class = "horizons_input_error")

})


test_that("select_training() rejects bad scope, metric and space", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 5, scope  = "nope"), class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, metric = "nope"), class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, space  = "nope"), class = "horizons_input_error")

})


test_that("select_training() rejects a twin_ratio outside (0, 1)", {

  ## At 1 or more the threshold reaches the reference distance and flags
  ## ordinary neighbours; global's claim to see every twin rests on it
  ## sitting below.

  fx <- make_select_fixture(n_pool = 60)

  for (bad in list(1, 1.5, NA_real_, NA, c(0.05, 0.1))) {

    expect_error(quiet_select(fx, k = 5, twin_ratio = bad),
                 regexp = "twin_ratio", class = "horizons_input_error")

  }

  expect_no_error(quiet_select(fx, k = 5, properties = "clay", twin_ratio = 0.5))

})


test_that("select_training(space = 'pls') needs exactly one property and an integer ncomp", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 5, space = "pls", ncomp = 3L),
               regexp = "one property", class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, space = "pls", ncomp = 0.99, properties = "clay"),
               class = "horizons_input_error")

})


test_that("select_training() refuses a pool that carries state from later verbs", {

  fx <- make_select_fixture(n_pool = 60)

  ## Selection subsets the pool's rows, and a subset keeps the pool's class
  ## and sections. A promoted pool would come out still claiming to be
  ## validated or fitted, with row_index and split keyed to a row order that
  ## no longer exists.
  fitted <- fx$pool
  class(fitted) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  expect_error(select_training(fx$targets, fitted, k = 5, verbose = FALSE),
               regexp = "horizons_fit", class = "horizons_input_error")

  validated <- fx$pool
  validated$validation$passed <- TRUE

  expect_error(select_training(fx$targets, validated, k = 5, verbose = FALSE),
               regexp = "validation\\$passed", class = "horizons_input_error")

})


test_that("select_training() refuses a pool that is itself a selection", {

  fx    <- make_select_fixture(n_pool = 60)
  prior <- quiet_select(fx, k = 5, properties = "clay")

  ## The provenance columns would collide under bind_cols and then surface as
  ## a confusing role-map abort. Say what is actually wrong instead.
  expect_error(select_training(fx$targets, prior, k = 5, verbose = FALSE),
               regexp = "\\.drawn_by", class = "horizons_input_error")

})


test_that("select_training() warns when pool and targets are in different units", {

  ## Arrange: the same targets scaled by 100, the fractional-against-percent
  ## absorbance mismatch. SNV removes exactly this, so the resemblance check
  ## downstream is structurally blind to it.
  fx <- make_select_fixture(n_pool = 60, target_scale = 100)

  expect_warning(out <- select_training(fx$targets, fx$pool, k = 5, verbose = FALSE),
                 regexp = "photometric units", class = "horizons_select_warning")

  u <- out$selection$units
  expect_true(u$mismatch)
  expect_gt(u$target_iqr / u$pool_iqr, 30)

  ## The same fixture in its own units does not warn. Its spectra carry a
  ## per-sample baseline offset straddling zero, so the pooled median's sign
  ## is a coin flip; only the scale is load-bearing here.
  same <- make_select_fixture(n_pool = 60)
  u_ok <- suppressWarnings(select_training(same$targets, same$pool, k = 5,
                                           verbose = FALSE))$selection$units

  expect_false(u_ok$mismatch)
  expect_identical(u_ok$basis, "iqr")

})


test_that("the unit check catches a log-base difference and lets an instrument gain pass", {

  ## The threshold is 2, not 3, for one reason: natural-log against base-10
  ## absorbance is 2.303, and two libraries that disagree about it look like
  ## one library. A 1.5x gain difference is the band this check is not for,
  ## and it has to stay silent or the warning stops meaning anything.

  ## One matrix against a scaled copy of itself, so the fold difference is
  ## the scale factor and nothing else.
  fx <- make_select_fixture(n_pool = 30)
  M  <- predictor_matrix(fx$pool)$matrix

  expect_true(check_photometric_units(M,  M * 100)$mismatch)
  expect_true(check_photometric_units(M,  M * 2.303)$mismatch)
  expect_false(check_photometric_units(M, M * 1.5)$mismatch)

  ## The ratio is the lever, and 2 is where it sits by default: a threefold
  ## bar is what missed the log-base case.
  expect_false(check_photometric_units(M, M * 2.303, ratio = 3)$mismatch)
  expect_true(check_photometric_units(M,  M * 1.5, ratio = 1.2)$mismatch)

  ## And the verb warns on a log-base mismatch end to end
  log_base <- make_select_fixture(n_pool = 30, target_scale = 2.303)

  expect_warning(select_training(log_base$targets, log_base$pool, k = 5, verbose = FALSE),
                 regexp = "photometric units", class = "horizons_select_warning")

})


test_that("the resemblance check is seeded and leaves the caller's RNG alone", {

  ## Arrange: a pool over the 2,000-row reference cap would be slow to build,
  ## so exercise the helper directly on a pool that is over it.
  fx <- make_select_fixture(n_pool = 60)
  rc <- reconcile_axes(fx$pool, fx$targets)
  tm <- predictor_matrix(fx$targets)
  sp <- build_similarity_space(rc$matrix, rc$wavenumbers, ncomp = 4L)
  st <- project_similarity(sp, tm$matrix, tm$wavenumbers)

  set.seed(5)
  big        <- sp
  idx        <- sample(nrow(sp$scores), 2500, replace = TRUE)
  big$scores <- sp$scores[idx, , drop = FALSE] +
                matrix(stats::rnorm(2500 * ncol(sp$scores), sd = 0.3),
                       nrow = 2500)
  rownames(big$scores) <- sprintf("B%04d", seq_len(2500))

  ## Act: two identical calls, with the caller's stream checkpointed
  set.seed(99)
  invisible(stats::runif(1))
  state <- get(".Random.seed", envir = globalenv())

  a <- check_resemblance(big, st, metric = "euclidean", chunk_size = 500L, seed = 1L)
  b <- check_resemblance(big, st, metric = "euclidean", chunk_size = 500L, seed = 1L)

  ## Assert: reproducible, and the caller's stream is exactly where it was
  expect_equal(a$threshold, b$threshold)
  expect_identical(a$beyond, b$beyond)
  expect_identical(get(".Random.seed", envir = globalenv()), state)

  ## And the seed is a real lever
  c2 <- check_resemblance(big, st, metric = "euclidean", chunk_size = 500L, seed = 7L)
  expect_false(isTRUE(all.equal(a$threshold, c2$threshold)))

})


test_that("a draw that cannot reach k is recorded and warned about once", {

  ## Arrange: k equal to the measured rows leaves no spare to replace a twin.
  fx <- make_select_fixture(n_pool = 60)
  k  <- 60L

  expect_warning(out <- select_training(fx$targets, fx$pool, k = k, properties = "clay",
                                        verbose = FALSE),
                 regexp = "could not reach k", class = "horizons_select_warning")

  sd_tbl <- out$selection$short_draws
  expect_named(sd_tbl, c("target_id", "property", "k_requested", "k_drawn", "reason"))
  expect_gte(nrow(sd_tbl), 1L)
  expect_true(fx$twin_id %in% sd_tbl$target_id)
  expect_true(all(sd_tbl$k_requested == k))
  expect_true(all(sd_tbl$k_drawn < k))

  ## An ordinary draw records nothing and warns not at all
  ok <- quiet_select(fx, k = 10, properties = "clay")
  expect_identical(nrow(ok$selection$short_draws), 0L)

})


## =============================================================================
## scope = "global"
## =============================================================================

test_that("scope = 'global' returns every pool row on the targets' grid", {

  fx  <- make_select_fixture(n_pool = 60)
  out <- quiet_select(fx, scope = "global")

  expect_s3_class(out, "horizons_data")
  expect_identical(out$data$n_rows, 60L)
  expect_setequal(out$data$analysis$sample_id, fx$pool$data$analysis$sample_id)

  ## On the targets' grid, not the pool's
  pm <- predictor_matrix(out)
  expect_identical(pm$wavenumbers, fx$target_wn)

  ## Record and groups
  expect_identical(out$selection$settings$scope, "global")
  expect_identical(nrow(out$selection$groups), 1L)
  expect_identical(out$selection$groups$n_rows, 60L)
  expect_identical(nrow(out$selection$membership), 0L)
  expect_true(all(is.na(out$data$analysis$.drawn_by)))

})


## =============================================================================
## scope = "batch"
## =============================================================================

test_that("scope = 'batch' draws k per target per property into one training set", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10)

  m <- out$selection$membership
  expect_true(all(table(m$target_id, m$property) == 10L))

  ## Rows enter once
  expect_false(any(duplicated(out$data$analysis$sample_id)))
  expect_setequal(out$data$analysis$sample_id, unique(m$pool_id))

  ## One group, holding every row
  g <- out$selection$groups
  expect_identical(nrow(g), 1L)
  expect_identical(g$n_rows, out$data$n_rows)
  expect_identical(g$n_targets, 8L)
  expect_setequal(g$pool_ids[[1]], out$data$analysis$sample_id)

})


test_that("provenance columns are meta and agree with the membership", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10)

  rm <- out$data$role_map
  expect_identical(rm$role[rm$variable == ".drawn_by"],     "meta")
  expect_identical(rm$role[rm$variable == ".min_distance"], "meta")
  expect_identical(rm$role[rm$variable == ".group"],        "meta")

  m  <- out$selection$membership
  a  <- out$data$analysis
  by <- tapply(m$target_id, m$pool_id, function(t) length(unique(t)))
  md <- tapply(m$distance,  m$pool_id, min)

  expect_identical(unname(a$.drawn_by), as.integer(by[a$sample_id]))
  expect_equal(unname(a$.min_distance), as.numeric(md[a$sample_id]), tolerance = 1e-12)
  expect_true(all(a$.group == 1L))

})


test_that("the return keeps every pool response and carries no similarity columns", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "clay")

  expect_true(all(c("clay", "oc") %in% names(out$data$analysis)))
  expect_identical(out$data$n_responses, 2L)

  ## Only the targets' wavenumbers, nothing from the derivative space
  pm <- predictor_matrix(out)
  expect_identical(pm$wavenumbers, fx$target_wn)
  expect_identical(out$data$n_predictors, length(fx$target_wn))

})


test_that("the sparse property draws k rows that all have it measured", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "oc")

  m  <- out$selection$membership
  oc <- fx$pool$data$analysis$oc[match(m$pool_id, fx$pool$data$analysis$sample_id)]
  expect_false(any(is.na(oc)))

  ps <- out$selection$pool_sizes
  expect_identical(ps$property, "oc")
  expect_identical(ps$available, sum(!is.na(fx$pool$data$analysis$oc)))
  expect_identical(ps$drawn, out$data$n_rows)

})


test_that("a named k is honoured per property", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = c(clay = 12, oc = 6))

  m <- out$selection$membership
  expect_true(all(table(m$target_id[m$property == "clay"]) == 12L))
  expect_true(all(table(m$target_id[m$property == "oc"])   == 6L))
  expect_identical(out$selection$settings$k, c(clay = 12L, oc = 6L))

})


test_that("the twin is excluded from its target's rows, reported, and out of the union", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "clay")

  m    <- out$selection$membership
  mine <- m$pool_id[m$target_id == fx$twin_id]
  expect_false(fx$twin_pool_id %in% mine)

  ex <- out$selection$exclusions
  expect_identical(ex$target_id, fx$twin_id)
  expect_identical(ex$pool_id,   fx$twin_pool_id)

  ## The union, not just the neighbourhood: a row excluded from one target's
  ## draw must not walk back in through another target that drew it. Here no
  ## other target drew it, so nothing had to be subtracted, and the count
  ## says so rather than overstating the work.
  expect_false(fx$twin_pool_id %in% out$data$analysis$sample_id)
  expect_identical(out$selection$n_excluded_union,
                   length(intersect(fx$twin_pool_id, unique(m$pool_id))))

  ## membership still records the pre-subtraction draw, so the two differ by
  ## exactly the excluded rows.
  expect_setequal(out$data$analysis$sample_id,
                  setdiff(unique(m$pool_id), fx$twin_pool_id))

})


test_that("a replicate cluster is excluded entirely and absent from the union", {

  ## Arrange: three replicate scans of the twinned pool row. Under the old
  ## first-to-second-nearest gap rule none of these was flagged, because
  ## every replicate distance is tiny and no gap opens.
  fx  <- make_select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
  out <- quiet_select(fx, k = 10, properties = "clay")

  self <- c(fx$twin_pool_id, fx$replicate_pool_ids)

  ## Act / Assert: all four flagged, none in the twin's neighbourhood, none
  ## in the returned training set, and the target still drew its full k.
  ex <- out$selection$exclusions
  expect_setequal(ex$pool_id[ex$target_id == fx$twin_id], self)

  m <- out$selection$membership
  expect_false(any(self %in% m$pool_id[m$target_id == fx$twin_id]))
  expect_identical(sum(m$target_id == fx$twin_id), 10L)

  expect_false(any(self %in% out$data$analysis$sample_id))
  expect_identical(out$selection$n_excluded_union, length(self))
  expect_identical(nrow(out$selection$short_draws), 0L)

})


test_that("scope = 'cluster' subtracts twins from the union too", {

  fx  <- make_select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
  out <- quiet_select(fx, k = 10, scope = "cluster", cluster_min = 2, properties = "clay")

  self <- c(fx$twin_pool_id, fx$replicate_pool_ids)

  expect_false(any(self %in% out$data$analysis$sample_id))
  expect_identical(out$selection$n_excluded_union, length(self))

  ## And out of every group's pool_ids, not just the analysis table
  expect_false(any(self %in% unlist(out$selection$groups$pool_ids)))

})


test_that("scope = 'sample' subtracts the twins from the returned object too", {

  ## Nothing downstream consumes selection$groups yet, so the object a user
  ## pipes into configure() is the union whatever the scope. Leaving the
  ## twins in it under sample would train the default pipeline on every
  ## target's own replicates.

  fx  <- make_select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
  out <- quiet_select(fx, k = 10, scope = "sample", properties = "clay")

  self <- c(fx$twin_pool_id, fx$replicate_pool_ids)

  expect_identical(out$selection$n_excluded_union, length(self))
  expect_false(any(self %in% out$data$analysis$sample_id))

  ## Each group is still that target's own draw, after the subtraction
  g    <- out$selection$groups
  mine <- g$pool_ids[[which(vapply(g$target_ids, function(t) fx$twin_id %in% t, logical(1)))]]
  expect_false(any(self %in% mine))
  expect_false(any(self %in% unlist(g$pool_ids)))

})


test_that("membership keeps the subtracted twins, marked retained = FALSE", {

  ## The record is the only place the exclusion survives: membership is the
  ## statement of what each neighbourhood was, and a row dropped from it
  ## rather than marked would take the evidence with it the first time
  ## anything filters the object's rows.

  fx  <- make_select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
  out <- quiet_select(fx, k = 10, properties = "clay")

  m    <- out$selection$membership
  self <- c(fx$twin_pool_id, fx$replicate_pool_ids)

  expect_true("retained" %in% names(m))
  expect_type(m$retained, "logical")

  ## Every subtracted row is still in the table, and marked
  expect_true(any(m$pool_id %in% self))
  expect_true(all(!m$retained[m$pool_id %in% self]))

  ## retained is exactly membership against the object's own rows
  expect_setequal(unique(m$pool_id[m$retained]),
                  intersect(unique(m$pool_id), out$data$analysis$sample_id))

})


test_that("scope = 'global' runs the twin check, reports it, and keeps the rows", {

  fx  <- make_select_fixture(n_pool = 60, seed = 3, n_replicates = 3)
  out <- quiet_select(fx, k = 10, scope = "global")

  self <- c(fx$twin_pool_id, fx$replicate_pool_ids)

  ## The control arm of the batch-versus-global comparison has to have its
  ## leakage measured, or the comparison is biased in a fixed direction.
  ex <- out$selection$exclusions
  expect_gt(nrow(ex), 0L)
  expect_true(all(self %in% ex$pool_id[ex$target_id == fx$twin_id]))

  ## Reported, but global returns the whole pool, so nothing is removed.
  expect_identical(out$selection$n_excluded_union, 0L)
  expect_true(all(self %in% out$data$analysis$sample_id))
  expect_identical(out$data$n_rows, 63L)

  txt <- utils::capture.output(select_training(fx$targets, fx$pool, k = 10, scope = "global"))
  expect_true(any(grepl("Twins flagged", txt)))
  expect_true(any(grepl("scope = global", txt)))

})


test_that("scope = 'global' records mean_k as the mean of k, not the nearest again", {

  ## The column has to mean the same thing in every branch: global set it to
  ## the first column, so mean_k equalled nearest and the applicability
  ## signal the control arm reports was not the one batch reports.

  fx <- make_select_fixture(n_pool = 60, seed = 3)

  g <- quiet_select(fx, k = 10, scope = "global", properties = "clay")$selection$target_distances
  b <- quiet_select(fx, k = 10, scope = "batch",  properties = "clay")$selection$target_distances

  expect_named(g, c("target_id", "property", "space", "nearest", "mean_k"))
  expect_true(all(g$mean_k >= g$nearest))
  expect_false(isTRUE(all.equal(g$mean_k, g$nearest)))

  ## The two branches measure the same thing over the same k rows, the twin
  ## target included: global drops its twins from the distances as batch does.
  expect_equal(g$mean_k, b$mean_k[match(g$target_id, b$target_id)], tolerance = 1e-10)

})


test_that("scope = 'global' records the exclusions batch records, under the same rule", {

  ## The control arm's twin rule has to be the rule the other arms run, or a
  ## scope sweep measures the rule as well as the scope. On these 63 rows
  ## batch takes its reference over a quarter of them, 15; global used to
  ## take max(k, 50) with no cap, a wider reference and so a looser rule,
  ## which at twin_ratio = 0.4 flagged three rows batch does not (#72).

  fx <- make_select_fixture(n_pool = 60, seed = 3, n_replicates = 3)

  for (ratio in c(SELECT_TWIN_RATIO, 0.4)) {

    b <- quiet_select(fx, k = 10, properties = "clay", twin_ratio = ratio)$selection
    g <- quiet_select(fx, k = 10, properties = "clay", twin_ratio = ratio, scope = "global")$selection

    ## The twins fall inside k, so batch recorded every row it flagged
    expect_gt(nrow(b$exclusions), 0L)
    expect_true(all(b$exclusions$rank <= 10L))

    ## Same rows, same reference distance, same property, row for row
    expect_identical(g$exclusions, b$exclusions)

    ## And the applicability signal is the one batch reports
    expect_identical(g$target_distances, b$target_distances)

  }

})


test_that("scope = 'global' records each exclusion with its property, on that property's rows", {

  fx  <- make_select_fixture(n_pool = 60, seed = 3, n_replicates = 3)
  out <- quiet_select(fx, k = 10, scope = "global")

  ex   <- out$selection$exclusions
  a    <- fx$pool$data$analysis
  self <- c(fx$twin_pool_id, fx$replicate_pool_ids)

  expect_false(anyNA(ex$property))
  expect_setequal(unique(ex$property), c("clay", "oc"))

  ## Clay is measured on every row, so the whole cluster is the twin
  ## target's exclusion for clay; for oc only the members that have oc
  ## measured are, because the check runs on each property's own rows.
  oc_self <- self[!is.na(a$oc[match(self, a$sample_id)])]
  expect_lt(length(oc_self), length(self))
  expect_gt(length(oc_self), 0L)

  mine <- ex[ex$target_id == fx$twin_id, ]
  expect_setequal(mine$pool_id[mine$property == "clay"], self)
  expect_setequal(mine$pool_id[mine$property == "oc"],   oc_self)

  ## One distance row per target per property, as batch writes them
  td <- out$selection$target_distances
  expect_false(anyNA(td$property))
  expect_identical(nrow(td), 2L * fx$targets$data$n_rows)

})


test_that("report_selection() prints a record with missing tables rather than erroring", {

  ## A record built by hand, or one a future field has not been added to,
  ## has to print. nrow(NULL) is NULL and if (NULL) is an error naming
  ## neither the field nor the verb.

  sel <- list(settings = list(scope = "batch", k = c(clay = 10L)))

  expect_silent(txt <- utils::capture.output(report_selection(sel, n_targets = 8L)))
  expect_true(any(grepl("Twins flagged: 0", txt)))
  expect_true(any(grepl("unknown", txt)))
  expect_true(any(grepl("scope = batch", txt)))

  ## And an entirely empty one
  expect_silent(utils::capture.output(report_selection(list(), n_targets = 0L)))

})


test_that("the record carries settings, reconciliation, pool identity and distances", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10)

  s <- out$selection
  expect_identical(s$settings$scope,  "batch")
  expect_identical(s$settings$metric, "mahalanobis")
  expect_identical(s$settings$space,  "pca")
  expect_true(s$settings$ncomp_retained >= 1L)
  expect_identical(s$reconciliation$operation, "resampled")
  expect_identical(s$pool$n_rows, 100L)
  expect_match(s$pool$id_hash, "^[0-9a-f]+$")
  expect_named(s$target_distances, c("target_id", "property", "space", "nearest", "mean_k"))
  expect_identical(nrow(s$target_distances), 16L)
  expect_true(all(s$target_distances$space == "all"))
  expect_s3_class(s$timestamp, "POSIXct")

})


test_that("the record carries the SG window in cm-1 and the space's floor", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- suppressWarnings(quiet_select(fx, k = 10))

  s <- out$selection$settings

  ## window is in points and the grid is the targets', so the physical width
  ## is the product. The fixture's targets are on an 8 cm-1 grid.
  expect_identical(s$window, 11L)
  expect_equal(s$window_cm, 11 * out$selection$reconciliation$target_grid$resolution)
  expect_equal(s$window_cm, 88)

  ## derivative = 0 means no filter and so no width
  flat <- suppressWarnings(quiet_select(fx, k = 10, derivative = 0L))
  expect_true(is.na(flat$selection$settings$window_cm))

  ## The similarity space's noise floor, as applied
  expect_equal(s$sdev_floor, SELECT_SDEV_FLOOR)
  expect_true(s$ncomp_retained <= s$ncomp_variance)
  ## The decay is recorded over the variance rule's set, not the floored one,
  ## so the record can be asked what the floor cut
  expect_length(s$sdev_ratio, s$ncomp_variance)
  expect_true(all(s$sdev_ratio[seq_len(s$ncomp_retained)] >= SELECT_SDEV_FLOOR))
  expect_true(all(s$sdev_ratio[-seq_len(s$ncomp_retained)] < SELECT_SDEV_FLOOR))

  ## And it is a lever, not a constant
  off <- suppressWarnings(quiet_select(fx, k = 10, sdev_floor = 0))
  expect_equal(off$selection$settings$sdev_floor, 0)
  expect_gt(off$selection$settings$ncomp_retained, s$ncomp_retained)

  expect_error(quiet_select(fx, k = 10, sdev_floor = 1),   class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 10, sdev_floor = -0.1), class = "horizons_input_error")

})


test_that("a PLS space records and prints that it selected on the pool's own responses", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "clay", space = "pls", ncomp = 3L)

  note <- out$selection$settings$space_note
  expect_true(is.character(note))
  expect_match(note, "optimistic")
  expect_match(note, "clay")

  ## A PCA run carries no such note
  expect_null(quiet_select(fx, k = 10, properties = "clay")$selection$settings$space_note)

  txt <- utils::capture.output(select_training(fx$targets, fx$pool, k = 10, properties = "clay",
                                               space = "pls", ncomp = 3L))
  expect_true(any(grepl("optimistic", txt)))

})


test_that("the return validates and runs through the ordinary chain", {

  skip_on_cran()

  fx  <- make_select_fixture(n_pool = 300)
  out <- quiet_select(fx, k = 30, properties = "clay")

  expect_no_error(validate_horizons_data(out))

  utils::capture.output({
    cfg <- configure(out, outcome = "clay", models = "rf", cv_folds = 3L)
    cfg <- validate(cfg)
  })

  expect_s3_class(cfg, "horizons_data")
  expect_true(any(cfg$data$role_map$role == "outcome"))

  ## prune = FALSE: on the synthetic fixture the one config can be pruned,
  ## leaving fit() nothing to fit; the chain is what is under test, not the
  ## pruning rule.
  ev <- suppressWarnings(evaluate(cfg, prune = FALSE, verbose = FALSE))
  expect_s3_class(ev, "horizons_eval")
  expect_true(nrow(ev$evaluation$results) >= 1L)
  expect_true(any(ev$evaluation$results$status == "success"))

  ## The targets carry none of the provenance columns (.drawn_by,
  ## .min_distance, .group) the training set does. predict() must not
  ## require them: the recipe's meta role is not baked (2026-09-21).
  f <- suppressWarnings(fit(ev, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE))
  expect_s3_class(f, "horizons_fit")
  expect_false(any(c(".drawn_by", ".min_distance", ".group") %in% names(fx$targets$data$analysis)))

  p <- predict(f, fx$targets, interval = FALSE)
  expect_identical(nrow(p), fx$targets$data$n_rows)
  expect_true(all(is.finite(p$.pred)))

})


## =============================================================================
## scope = "cluster"
## =============================================================================

test_that("scope = 'cluster' yields one group per target cluster", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, scope = "cluster", cluster_min = 2)

  g <- out$selection$groups
  expect_identical(nrow(g), 2L)
  expect_identical(sum(g$n_targets), 8L)

  ## Clusters follow the families
  cl  <- out$selection$clustering$assignment
  fam <- fx$family_of_target[names(cl)]
  expect_identical(length(unique(paste(fam, cl))), 2L)

  ## Every group's rows are the union of its targets' neighbourhoods
  m <- out$selection$membership
  for (i in seq_len(nrow(g))) {
    expect_setequal(g$pool_ids[[i]], unique(m$pool_id[m$target_id %in% g$target_ids[[i]]]))
  }

  ## .group is populated and the return is still the union, rows once
  expect_true(all(out$data$analysis$.group %in% 1:2))
  expect_false(any(duplicated(out$data$analysis$sample_id)))
  expect_setequal(out$data$analysis$sample_id, unique(m$pool_id))

})


test_that("scope = 'cluster' falls back to one group below the floor, through the tree", {

  fx <- make_select_fixture(n_pool = 100)

  ## The fallback goes through the verb's own console layer, so it obeys
  ## verbose. Routing it through cli made select_training(verbose = FALSE)
  ## print regardless.
  txt <- utils::capture.output(
    out <- select_training(fx$targets, fx$pool, k = 10, scope = "cluster", cluster_min = 5)
  )

  expect_true(any(grepl("one group", txt)))
  expect_identical(nrow(out$selection$groups), 1L)
  expect_identical(out$selection$clustering$k, 1L)

  expect_silent(select_training(fx$targets, fx$pool, k = 10, scope = "cluster",
                                cluster_min = 5, verbose = FALSE))

})


## =============================================================================
## scope = "sample"
## =============================================================================

test_that("scope = 'sample' gives one group per target over the same union", {

  fx    <- make_select_fixture(n_pool = 100)
  batch <- quiet_select(fx, k = 10)
  samp  <- quiet_select(fx, k = 10, scope = "sample")

  expect_setequal(samp$data$analysis$sample_id, batch$data$analysis$sample_id)

  g <- samp$selection$groups
  expect_identical(nrow(g), 8L)
  expect_true(all(g$n_targets == 1L))
  expect_setequal(unlist(g$target_ids), fx$targets$data$analysis$sample_id)

  m <- samp$selection$membership
  for (i in seq_len(nrow(g))) {
    expect_setequal(g$pool_ids[[i]], unique(m$pool_id[m$target_id == g$target_ids[[i]]]))
  }

  expect_false(".group" %in% names(samp$data$analysis))

})


## =============================================================================
## Levers reach the draw
## =============================================================================

test_that("metric and space levers change which rows are drawn", {

  fx <- make_select_fixture(n_pool = 100)

  base <- quiet_select(fx, k = 10, properties = "clay")
  eucl <- quiet_select(fx, k = 10, properties = "clay", metric = "euclidean")
  cosn <- quiet_select(fx, k = 10, properties = "clay", metric = "cosine")
  pls  <- quiet_select(fx, k = 10, properties = "clay", space = "pls", ncomp = 3L)

  ids <- function(o) o$selection$membership$pool_id

  expect_false(identical(ids(base), ids(eucl)))
  expect_false(identical(ids(base), ids(cosn)))
  expect_false(identical(ids(base), ids(pls)))
  expect_identical(pls$selection$settings$space, "pls")

})


test_that("space_rows = 'measured' fits a space per property on its measured rows", {

  fx  <- make_select_fixture(n_pool = 100)
  all <- quiet_select(fx, k = 10)
  mea <- quiet_select(fx, k = 10, space_rows = "measured")

  s <- mea$selection$settings
  expect_identical(s$space_rows, "measured")
  expect_named(s$ncomp_by_property, c("clay", "oc"))
  expect_named(s$space_n_rows,      c("clay", "oc"))
  expect_identical(unname(s$space_n_rows["clay"]), 100L)
  expect_identical(unname(s$space_n_rows["oc"]),   sum(!is.na(fx$pool$data$analysis$oc)))
  expect_null(all$selection$settings$ncomp_by_property)

  ## clay is measured on every row, so its space is the all-rows space and
  ## the draw agrees; oc's space is fit on half the rows and can differ
  ids <- function(o, p) { m <- o$selection$membership; m$pool_id[m$property == p] }
  expect_identical(ids(mea, "clay"), ids(all, "clay"))

  ## Still k per target per property, every oc row measured
  m <- mea$selection$membership
  expect_true(all(table(m$target_id, m$property) == 10L))
  oc <- fx$pool$data$analysis$oc[match(m$pool_id[m$property == "oc"], fx$pool$data$analysis$sample_id)]
  expect_false(any(is.na(oc)))

})


test_that("space_rows = 'measured' marks the space and refuses to pool distances across them", {

  fx  <- make_select_fixture(n_pool = 100)
  mea <- quiet_select(fx, k = 10, space_rows = "measured")

  ## Each property's distances come from its own PCA, with its own rotation
  ## and sdev, so the space column is what says they are not comparable.
  m <- mea$selection$membership
  expect_setequal(unique(m$space), c("clay", "oc"))
  expect_true(all(m$space == m$property))
  expect_true(all(mea$selection$target_distances$space ==
                  mea$selection$target_distances$property))

  ## A minimum over two incommensurable scales is a number with no meaning,
  ## so it is not written at all.
  expect_true(all(is.na(mea$data$analysis$.min_distance)))
  expect_true(all(mea$data$analysis$.drawn_by >= 1L))

  ## And .group, which picks the nearest drawing target, is NA under cluster
  ## for the same reason. Under batch every target is group 1, so no
  ## comparison is made and the value stands.
  expect_true(all(mea$data$analysis$.group == 1L))

  cl <- quiet_select(fx, k = 10, space_rows = "measured", scope = "cluster", cluster_min = 2)
  expect_true(all(is.na(cl$data$analysis$.group)))

  ## One property is one space, so the distances stay comparable
  one <- quiet_select(fx, k = 10, space_rows = "measured", properties = "clay")
  expect_true(all(one$selection$membership$space == "clay"))
  expect_false(any(is.na(one$data$analysis$.min_distance)))

})


test_that("space_rows = 'measured' is ignored under scope = 'global' and rejected when invalid", {

  fx  <- make_select_fixture(n_pool = 60)
  out <- quiet_select(fx, scope = "global", space_rows = "measured")

  expect_null(out$selection$settings$ncomp_by_property)
  expect_error(quiet_select(fx, k = 5, space_rows = "some"), class = "horizons_input_error")

})


## =============================================================================
## Output
## =============================================================================

test_that("verbose = FALSE prints nothing", {

  fx <- make_select_fixture(n_pool = 60)

  expect_silent(select_training(fx$targets, fx$pool, k = 5, verbose = FALSE))

})


test_that("verbose = TRUE reports the pool, the space and the draw", {

  fx  <- make_select_fixture(n_pool = 60)
  txt <- utils::capture.output(out <- select_training(fx$targets, fx$pool, k = 5))

  expect_true(any(grepl("Selecting", txt)))
  expect_true(any(grepl("clay", txt)))
  expect_true(any(grepl("components", txt)))
  expect_true(any(grepl("twin", txt, ignore.case = TRUE)))

})


## =============================================================================
## Leakage detector
## =============================================================================

test_that("permuting the pool's responses collapses the evaluated CV", {

  ## The single highest-leverage leakage detector on this path: if the
  ## selection or the recipe were carrying any information about the outcome
  ## that it should not, a model trained on shuffled labels would still
  ## score. It must not. RPD near 1 is the honest answer for noise.

  skip_on_cran()

  fx <- make_select_fixture(n_pool = 300)

  cv_rpd <- function(pool) {

    out <- select_training(fx$targets, pool, k = 40, properties = "clay", verbose = FALSE)

    utils::capture.output({
      cfg <- configure(out, outcome = "clay", models = "rf", cv_folds = 3L)
      cfg <- validate(cfg)
    })

    ev  <- suppressWarnings(evaluate(cfg, prune = FALSE, verbose = FALSE))
    res <- ev$evaluation$results
    res <- res[res$status == "success", , drop = FALSE]

    if (!nrow(res) || !"rpd" %in% names(res)) skip("evaluate() did not return an rpd column")

    max(res$rpd, na.rm = TRUE)

  }

  set.seed(4)
  shuffled <- fx$pool
  shuffled$data$analysis$clay <- sample(shuffled$data$analysis$clay)

  permuted <- cv_rpd(shuffled)

  ## Tolerant on the upper side: this is a synthetic fixture and a small
  ## forest, so the point is that the permuted model has no signal at all,
  ## not where exactly the real one lands.
  expect_lt(permuted, 1.3)

})


test_that("print() shows a Selection section", {

  fx  <- make_select_fixture(n_pool = 60)
  out <- quiet_select(fx, k = 5)
  txt <- utils::capture.output(print(out))

  expect_true(any(grepl("^Selection", txt)))
  expect_true(any(grepl("batch", txt)))
  expect_true(any(grepl("k: 5", txt)))

})
