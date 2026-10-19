# tests/testthat/test-pipeline-select-training.R
# Tests for select_training(): the verb end to end.


## =============================================================================
## Helpers
## =============================================================================

#' Run the verb quietly on the fixture
#' @noRd
quiet_select <- function(fx, ...) {

  select_training(fx$targets, fx$pool, ...)

}


#' Give the fixture pool a clay its spectra predict
#'
#' @description
#' `make_select_fixture()` draws clay independently of the spectra, so no
#' model can learn it. This replaces it with a known linear function of the
#' spectra plus noise: the sum of the band means (within 20 cm-1) at one peak
#' centre of each of the fixture's two families (1630 and 1420 cm-1),
#' rescaled to mean 35 and SD 8, plus Gaussian noise sized for an R² of
#' `r2`. A band is several grid steps wide, so it survives the resampling of
#' the drawn rows to the targets' 8 cm-1 grid. Each family contributes one
#' peak to the sum, so the two families' clay means are close against the
#' clay SD: the signal is in each row's own spectrum, not in which family
#' the row belongs to, and pairing the responses with the wrong rows
#' destroys it.
#'
#' @param pool [horizons_data.] `make_select_fixture()$pool`.
#' @param r2 [Numeric.] R² of the returned clay on its noise-free part.
#'   Default: `0.8`.
#' @param seed [Integer.] Seed for the noise, applied through
#'   `withr::with_seed()` so the global RNG stream is left as it was.
#'   Default: `1`.
#'
#' @return [horizons_data.] `pool` with `clay` replaced.
#' @noRd
learnable_clay_pool <- function(pool, r2 = 0.8, seed = 1) {

  ## One peak centre from each of make_select_fixture()'s two families
  centres <- c(1630, 1420)

  pm   <- predictor_matrix(pool)
  band <- vapply(centres, function(cc) {
    rowMeans(pm$matrix[, abs(pm$wavenumbers - cc) <= 20, drop = FALSE])
  }, numeric(nrow(pm$matrix)))

  f      <- rowSums(band)
  signal <- 35 + 8 * (f - mean(f)) / stats::sd(f)
  noise  <- withr::with_seed(seed, stats::rnorm(length(f), sd = 8 * sqrt((1 - r2) / r2)))

  pool$data$analysis$clay <- round(signal + noise, 1)

  pool

}


## =============================================================================
## Input validation
## =============================================================================

test_that("select_training() rejects inputs that are not horizons_data", {

  fx <- select_fixture(n_pool = 60)

  expect_error(select_training(data.frame(a = 1), fx$pool),
               regexp = "`x` must be a horizons_data object", fixed = TRUE,
               class = "horizons_input_error")
  expect_error(select_training(fx$targets, data.frame(a = 1)),
               regexp = "`library` must be a horizons_data", fixed = TRUE,
               class = "horizons_input_error")

})


test_that("select_training() needs a pool with at least one response", {

  fx <- select_fixture(n_pool = 60)

  expect_error(select_training(fx$targets, fx$targets),
               regexp = "response", class = "horizons_input_error")

})


test_that("select_training() rejects unknown properties", {

  fx <- select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, properties = "ph"),
               regexp = "ph", class = "horizons_input_error")

})


test_that("select_training() rejects a bad k", {

  fx <- select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 0),   regexp = "`k` must be", fixed = TRUE,
               class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 2.5), regexp = "`k` must be", fixed = TRUE,
               class = "horizons_input_error")
  expect_error(quiet_select(fx, k = c(clay = 5)), regexp = "oc", class = "horizons_input_error")

})


test_that("select_training() rejects bad scope, metric and space", {

  fx <- select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 5, scope  = "nope"), regexp = "`scope` must be",
               fixed = TRUE, class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, metric = "nope"), regexp = "`metric` must be",
               fixed = TRUE, class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, space  = "nope"), regexp = "`space` must be",
               fixed = TRUE, class = "horizons_input_error")

})


test_that("select_training() rejects a twin_ratio outside (0, 1)", {

  ## At 1 or more the threshold reaches the reference distance and flags
  ## ordinary neighbours; global's claim to see every twin rests on it
  ## sitting below.

  fx <- select_fixture(n_pool = 60)

  for (bad in list(1, 1.5, NA_real_, NA, c(0.05, 0.1))) {

    expect_error(quiet_select(fx, k = 5, twin_ratio = bad),
                 regexp = "twin_ratio", class = "horizons_input_error")

  }

  expect_no_error(quiet_select(fx, k = 5, properties = "clay", twin_ratio = 0.5))

})


test_that("select_training(space = 'pls') needs exactly one property and an integer ncomp", {

  fx <- select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 5, space = "pls", ncomp = 3L),
               regexp = "one property", class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, space = "pls", ncomp = 0.99, properties = "clay"),
               regexp = "needs a whole-number `ncomp`", fixed = TRUE,
               class = "horizons_input_error")

})


test_that("select_training() records its control and its seed, 307 by default", {

  fx  <- select_fixture(n_pool = 60)
  out <- quiet_select(fx, k = 5, properties = "clay")

  expect_s3_class(out$selection$settings$control, "horizons_control")
  expect_identical(names(out$selection$settings$control), names(resolve_control(NULL, "select_training")))
  expect_identical(out$selection$settings$seed, 307L)

})


test_that("select_training() takes the resemblance check's minimum and the chunk size from its control", {

  fx <- select_fixture(n_pool = 60)

  seen <- NULL
  draw_chunk <- NULL
  real       <- check_resemblance
  real_draw  <- draw_neighbours
  local_mocked_bindings(
    check_resemblance = function(sp, St, metric, chunk_size, seed, eligible, min_rows) {
      seen <<- c(chunk_size = chunk_size, min_rows = min_rows)
      real(sp, St, metric, chunk_size, seed = seed, eligible = eligible, min_rows = min_rows)
    },
    draw_neighbours = function(..., chunk_size) {
      draw_chunk <<- chunk_size
      real_draw(..., chunk_size = chunk_size)
    }
  )

  out <- suppressWarnings(quiet_select(fx, k = 5, properties = "clay",
                                       control = control_horizons(chunk_size = 7L, min_resemblance_rows = 1000L)))

  expect_identical(seen, c(chunk_size = 7L, min_rows = 1000L))
  expect_identical(draw_chunk, 7L)
  expect_match(out$selection$resemblance$skipped, "the check needs 1000", fixed = TRUE)

})


test_that("select_training() returns the pool's log", {

  fx <- select_fixture(n_pool = 60)
  fx$pool$log <- tibble::add_row(new_log(), verb = "spectra", type = "note", topic = NA_character_,
                                 message = "a note", ids = list("P001"), n = 1L)

  expect_identical(quiet_select(fx, k = 5, properties = "clay")$log, fx$pool$log)

})


test_that("select_training() writes its coverage warning to the log before raising it, once", {

  ## Targets trimmed to 1000-4000: the search is the overlap, 400 cm-1 short
  ## at the low end, which the 50 cm-1 limit used to refuse
  fx <- select_fixture(n_pool = 60, target_range = c(4000, 1000))

  warned <- list()
  out <- withCallingHandlers(
    select_training(fx$targets, fx$pool, k = 5, properties = "clay"),
    horizons_warning = function(w) {
      warned[[length(warned) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  ## One warning, carrying the row it wrote
  expect_length(warned, 1L)
  expect_s3_class(warned[[1]], "horizons_warning_coverage")
  expect_match(conditionMessage(warned[[1]]), "Short by 400 cm-1 at the low end", fixed = TRUE)

  ## The row is on the object, after the pool's
  row <- out$log[out$log$verb == "select_training", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$type, "warning")
  expect_identical(row$topic, "coverage")
  expect_identical(warned[[1]]$log, row)

  ## Its ids are the library columns the search left out, 600 to 996 cm-1
  left_out <- paste0("wn_", seq(996, 600, by = -4))
  expect_setequal(row$ids[[1]], left_out)
  expect_identical(row$n, length(left_out))

  ## The record does not depend on the signal
  quiet <- suppressWarnings(select_training(fx$targets, fx$pool, k = 5, properties = "clay"))
  expect_identical(quiet$log, out$log)

})


test_that("select_training() under warn = 2 ends in one error that keeps the selection", {

  fx <- select_fixture(n_pool = 60, target_range = c(4000, 1000))

  withr::local_options(warn = 2)

  err <- expect_error(select_training(fx$targets, fx$pool, k = 5, properties = "clay"),
                      class = "horizons_error_promoted_warnings")

  expect_s3_class(err$object, "horizons_data")
  expect_identical(err$log$topic, "coverage")
  expect_identical(err$object$log[err$object$log$verb == "select_training", ]$topic, "coverage")

})


test_that("select_training() refuses a pool that carries state from later verbs", {

  fx <- select_fixture(n_pool = 60)

  ## Selection subsets the pool's rows, and a subset keeps the pool's class
  ## and sections. A promoted pool would come out still claiming to be
  ## validated or fitted, with predictions and a split keyed to a row order
  ## that no longer exists.
  fitted <- fx$pool
  class(fitted) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  expect_error(select_training(fx$targets, fitted, k = 5),
               "`library` is a <horizons_fit>; it must be a plain horizons_data", fixed = TRUE,
               class = "horizons_input_error")

  validated <- fx$pool
  validated$validation$passed <- TRUE

  expect_error(select_training(fx$targets, validated, k = 5),
               regexp = "validation\\$passed", class = "horizons_input_error")

})


test_that("the pool check reads models$workflows, not a stray row_index (#131)", {

  fx <- select_fixture(n_pool = 60)

  stray <- fx$pool
  stray$models$row_index <- tibble::tibble(.row = 1L, sample_id = "P001")

  expect_length(check_pool_unpromoted(stray), 0L)

  fitted <- fx$pool
  fitted$models$workflows <- list(cfg_a = "a fitted workflow")

  expect_match(check_pool_unpromoted(fitted), "models$workflows", fixed = TRUE)

})


test_that("select_training() refuses a configured pool and says to use it before configure()", {

  fx <- select_fixture(n_pool = 60)

  ## The training set is a subset of the pool, so a configuration or an
  ## outcome chosen for the library would ride into it.
  utils::capture.output(configured <- configure(fx$pool, outcome = "clay", models = "plsr"))

  expect_error(select_training(fx$targets, configured, k = 5),
               regexp = "config\\$configs", class = "horizons_input_error")
  expect_error(select_training(fx$targets, configured, k = 5),
               regexp = "before `configure\\(\\)` and `validate\\(\\)`")

  ## An outcome role alone, without a configuration
  role_map <- fx$pool$data$role_map
  role_map$role[role_map$variable == "clay"] <- "outcome"
  outcome_only <- set_analysis(fx$pool, fx$pool$data$analysis, role_map)

  expect_error(select_training(fx$targets, outcome_only, k = 5),
               regexp = "outcome role \\(clay\\)", class = "horizons_input_error")

})


test_that("select_training() refuses a pool whose validation failed or that carries a removal record", {

  fx <- select_fixture(n_pool = 60)

  failed <- fx$pool
  failed$validation$passed <- FALSE

  expect_error(select_training(fx$targets, failed, k = 5),
               regexp = "validation\\$passed", class = "horizons_input_error")

  removed <- fx$pool
  removed$validation$outliers$removed_ids <- "P999"
  removed$validation$outliers$removed     <- TRUE

  expect_error(select_training(fx$targets, removed, k = 5),
               regexp = "removal record", class = "horizons_input_error")

  ## Any one part of a removal record is enough
  parts <- list(
    removed        = TRUE,
    removed_ids    = "P999",
    removal_detail = tibble::tibble(sample_id = "P999", reason = "spectral", spectral_threshold = 1)
  )

  for (part in names(parts)) {

    one <- fx$pool
    one$validation$outliers[part] <- parts[part]

    expect_match(check_pool_unpromoted(one), "a removal record in validation$outliers",
                 fixed = TRUE, info = part)

  }

})


test_that("select_training() refuses targets that fail the base contract", {

  fx <- select_fixture(n_pool = 60)

  ## Replicate scans sharing an id: the record names targets by sample_id,
  ## so they would be merged there.
  replicates <- fx$targets
  replicates$data$analysis$sample_id[2] <- replicates$data$analysis$sample_id[1]

  utils::capture.output(
    expect_error(select_training(replicates, fx$pool, k = 5),
                 regexp = "`x`, the targets.*Duplicate", class = "horizons_validation_error")
  )

  ## A non-finite spectrum, which the similarity space cannot take
  non_finite <- fx$targets
  non_finite$data$analysis$wn_4000[1] <- NA_real_

  utils::capture.output(
    expect_error(select_training(non_finite, fx$pool, k = 5),
                 regexp = "NA values in predictor", class = "horizons_validation_error")
  )

  ## Checked before the library resolves, so a registered library is not
  ## fetched for targets the verb would refuse.
  utils::capture.output(
    expect_error(select_training(replicates, "kssl", k = 5),
                 regexp = "the targets", class = "horizons_validation_error")
  )

})


test_that("select_training() refuses a pool that fails the base contract", {

  fx <- select_fixture(n_pool = 60)

  broken <- fx$pool
  broken$data$analysis$wn_4000[3] <- Inf

  utils::capture.output(
    expect_error(select_training(fx$targets, broken, k = 5),
                 regexp = "`library`, the pool.*Infinite", class = "horizons_validation_error")
  )

})


test_that("select_training() refuses a target axis it cannot search, before the library resolves", {

  fx <- select_fixture(n_pool = 60)

  one    <- make_axis_object(4000, prefix = "T")
  gapped <- seq(4000, 600, by = -8)
  gapped <- make_axis_object(gapped[!(gapped >= 1580 & gapped <= 1720)], prefix = "H")

  ## The targets' axis needs nothing from the library, so a registered
  ## library is not fetched for targets the verb would refuse
  local_mocked_bindings(resolve_source = function(...) stop("the library was resolved"))

  err <- expect_error(select_training(one, fx$pool, k = 5),
                      regexp = "at least two wavenumbers", class = "horizons_error_input")
  expect_match(conditionMessage(err), "`x`, the targets", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "select_training")

  err <- expect_error(select_training(gapped, "kssl", k = 5),
                      regexp = "gap", class = "horizons_error_input")
  expect_match(conditionMessage(err), "`x`, the targets", fixed = TRUE)

})


test_that("select_training() refuses a library axis it cannot search, naming library", {

  fx <- select_fixture(n_pool = 60)

  one    <- make_axis_object(4000, n = 12L, prefix = "L", clay = TRUE)
  gapped <- seq(4000, 600, by = -4)
  gapped <- make_axis_object(gapped[!(gapped >= 1580 & gapped <= 1720)], n = 12L,
                             prefix = "G", clay = TRUE)

  err <- expect_error(select_training(fx$targets, one, k = 5),
                      regexp = "at least two wavenumbers", class = "horizons_error_input")
  expect_match(conditionMessage(err), "`library`, the pool", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "select_training")

  err <- expect_error(select_training(fx$targets, gapped, k = 5),
                      regexp = "1576|1724", class = "horizons_error_input")
  expect_match(conditionMessage(err), "`library`, the pool", fixed = TRUE)

})


test_that("select_training() refuses a pool that is itself a selection", {

  fx    <- select_fixture(n_pool = 60)
  prior <- quiet_select(fx, k = 5, properties = "clay")

  ## The provenance columns would collide under bind_cols and then surface as
  ## a confusing role-map abort. Say what is actually wrong instead.
  expect_error(select_training(fx$targets, prior, k = 5),
               regexp = "itself a selection", class = "horizons_input_error")

  ## Any one of the three is enough, and is named
  for (col in c(".drawn_by", ".min_distance", ".group")) {

    pool <- fx$pool
    pool$data$analysis[[col]] <- NA
    pool$data$role_map <- rbind(pool$data$role_map, tibble::tibble(variable = col, role = "meta"))

    err <- expect_error(select_training(fx$targets, pool, k = 5),
                        class = "horizons_input_error")
    expect_match(gsub("\\s+", " ", conditionMessage(err)),
                 sprintf("provenance column \"%s\", so it is itself a selection", col),
                 fixed = TRUE, info = col)

  }

})


test_that("select_training() warns when pool and targets are in different units", {

  ## Arrange: the same targets scaled by 100, the fractional-against-percent
  ## absorbance mismatch. SNV removes exactly this, so the resemblance check
  ## downstream is structurally blind to it.
  fx <- select_fixture(n_pool = 60, target_scale = 100)

  w <- expect_warning(out <- select_training(fx$targets, fx$pool, k = 5),
                      regexp = "photometric units", class = "horizons_select_warning")

  u <- out$selection$units
  expect_true(u$mismatch)
  expect_gt(u$target_iqr / u$pool_iqr, 30)

  ## The warning carries both scales
  msg <- gsub("\\s+", " ", conditionMessage(w))
  expect_match(msg, paste0("Pool absorbance: median ", signif(u$pool_median, 3),
                           ", IQR ", signif(u$pool_iqr, 3)), fixed = TRUE)
  expect_match(msg, paste0("Targets: median ", signif(u$target_median, 3),
                           ", IQR ", signif(u$target_iqr, 3)), fixed = TRUE)

  ## The same fixture in its own units does not warn. Its spectra carry a
  ## per-sample baseline offset straddling zero, so the pooled median's sign
  ## is a coin flip; only the scale is load-bearing here.
  same <- select_fixture(n_pool = 60)
  u_ok <- suppressWarnings(select_training(same$targets, same$pool, k = 5))$selection$units

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
  fx <- select_fixture(n_pool = 30)
  M  <- predictor_matrix(fx$pool)$matrix

  expect_true(check_photometric_units(M,  M * 100)$mismatch)
  expect_true(check_photometric_units(M,  M * 2.303)$mismatch)
  expect_false(check_photometric_units(M, M * 1.5)$mismatch)

  ## The ratio is the lever, and 2 is where it sits by default: a threefold
  ## bar is what missed the log-base case.
  expect_false(check_photometric_units(M, M * 2.303, ratio = 3)$mismatch)
  expect_true(check_photometric_units(M,  M * 1.5, ratio = 1.2)$mismatch)

  ## Medians that dominate their spread are compared as well, by fold and by
  ## sign. Offset by 10, the fixture's median sits far clear of its IQR, and
  ## the offsets below leave every IQR as it was.
  L    <- M + 10
  same <- check_photometric_units(L, L)

  expect_identical(same$basis, "median and iqr")
  expect_false(same$mismatch)
  expect_true(check_photometric_units(L, M + 30)$mismatch)
  expect_true(check_photometric_units(L, M - 10)$mismatch)

  ## And the verb warns on a log-base mismatch end to end
  log_base <- select_fixture(n_pool = 30, target_scale = 2.303)

  expect_warning(select_training(log_base$targets, log_base$pool, k = 5),
                 regexp = "photometric units", class = "horizons_select_warning")

})


test_that("the resemblance check is seeded and leaves the caller's RNG alone", {

  ## Arrange: a pool over the 2,000-row reference cap would be slow to build,
  ## so exercise the helper directly on a pool that is over it.
  fx <- select_fixture(n_pool = 60)
  rc <- pool_on_targets(fx)
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

  ## A session with no RNG state is left with none, whether the reference was
  ## sampled (over the cap) or not
  withr::local_preserve_seed()

  for (space in list(big, sp)) {

    rm(list = intersect(".Random.seed", ls(globalenv(), all.names = TRUE)), envir = globalenv())

    expect_no_warning(r <- check_resemblance(space, st, metric = "euclidean",
                                             chunk_size = 500L, seed = 1L))
    expect_identical(r$n_reference, min(nrow(space$scores), 2000L))
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

  }

  ## A one-component space over the cap: the sampled reference is still a
  ## matrix of rows
  one        <- big
  one$scores <- big$scores[, 1, drop = FALSE]
  one$sdev   <- big$sdev[1]

  r1 <- check_resemblance(one, st[, 1, drop = FALSE], metric = "euclidean",
                          chunk_size = 500L, seed = 1L)

  expect_identical(r1$n_reference, 2000L)
  expect_true(is.finite(r1$threshold))

})


test_that("a draw that cannot reach k is recorded and warned about once", {

  ## Arrange: k equal to the measured rows leaves no spare to replace a twin.
  fx <- select_fixture(n_pool = 60)
  k  <- 60L

  w <- expect_warning(out <- select_training(fx$targets, fx$pool, k = k, properties = "clay"),
                      regexp = "could not reach k", class = "horizons_select_warning")

  sd_tbl <- out$selection$short_draws
  expect_named(sd_tbl, c("target_id", "property", "k_requested", "k_drawn", "reason"))
  expect_gte(nrow(sd_tbl), 1L)
  expect_true(fx$twin_id %in% sd_tbl$target_id)
  expect_true(all(sd_tbl$k_requested == k))
  expect_true(all(sd_tbl$k_drawn < k))

  ## The warning names the shortest draw
  expect_match(gsub("\\s+", " ", conditionMessage(w)),
               sprintf("Smallest: %d of %d rows, on clay", min(sd_tbl$k_drawn), k), fixed = TRUE)

  ## An ordinary draw records nothing and warns not at all
  ok <- quiet_select(fx, k = 10, properties = "clay")
  expect_identical(nrow(ok$selection$short_draws), 0L)

  ## Nor does global, which draws nothing, at the same k
  expect_no_warning(all_rows <- select_training(fx$targets, fx$pool, k = k, scope = "global",
                                                properties = "clay"))
  expect_identical(nrow(all_rows$selection$short_draws), 0L)

})


## =============================================================================
## scope = "global"
## =============================================================================

test_that("scope = 'global' returns every pool row on the targets' grid", {

  fx  <- select_fixture(n_pool = 60)
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

  ## The provenance columns are there, and empty: global draws nothing. They
  ## keep the drawing scopes' types, so the control arm binds with them.
  expect_true(all(c(".drawn_by", ".min_distance") %in% names(out$data$analysis)))
  expect_identical(out$data$analysis$.drawn_by,     rep(NA_integer_, 60L))
  expect_identical(out$data$analysis$.min_distance, rep(NA_real_,    60L))

})


## =============================================================================
## scope = "batch"
## =============================================================================

test_that("scope = 'batch' draws k per target per property into one training set", {

  fx  <- select_fixture(n_pool = 100)
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

  fx  <- select_fixture(n_pool = 100)
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

  fx  <- select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "clay")

  expect_true(all(c("clay", "oc") %in% names(out$data$analysis)))
  expect_identical(out$data$n_responses, 2L)

  ## Only the targets' wavenumbers, nothing from the derivative space
  pm <- predictor_matrix(out)
  expect_identical(pm$wavenumbers, fx$target_wn)
  expect_identical(out$data$n_predictors, length(fx$target_wn))

})


test_that("the sparse property draws k rows that all have it measured", {

  fx  <- select_fixture(n_pool = 100)
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

  fx  <- select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = c(clay = 12, oc = 6))

  m <- out$selection$membership
  expect_true(all(table(m$target_id[m$property == "clay"]) == 12L))
  expect_true(all(table(m$target_id[m$property == "oc"])   == 6L))
  expect_identical(out$selection$settings$k, c(clay = 12L, oc = 6L))

})


test_that("the twin is excluded from its target's rows, reported, and out of the union", {

  fx  <- select_fixture(n_pool = 100)
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
  fx  <- select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
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

  fx  <- select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
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

  fx  <- select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
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

  fx  <- select_fixture(n_pool = 100, seed = 3, n_replicates = 3)
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

  fx  <- select_fixture(n_pool = 60, seed = 3, n_replicates = 3)
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

  txt <- utils::capture.output(select_training(fx$targets, fx$pool, k = 10, scope = "global",
                                               control = control_horizons(verbose = TRUE)))
  expect_true(any(grepl("Twins flagged", txt)))
  expect_true(any(grepl("scope = global", txt)))

})


test_that("scope = 'global' records the exclusions batch records, under the same rule, and mean_k as the mean of k", {

  ## The control arm's twin rule has to be the rule the other arms run, or a
  ## scope sweep measures the rule as well as the scope. On these 63 rows
  ## batch takes its reference over a quarter of them, 15; global used to
  ## take max(k, 50) with no cap, a wider reference and so a looser rule,
  ## which at twin_ratio = 0.4 flagged three rows batch does not (#72).

  fx <- select_fixture(n_pool = 60, seed = 3, n_replicates = 3)

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

    ## mean_k has to mean the same thing in every branch: global set it to
    ## the first column, so mean_k equalled nearest and the applicability
    ## signal the control arm reports was not the one batch reports. The
    ## identity above holds global to batch, over the same k rows with the
    ## twins dropped; these hold mean_k to its own definition.
    td <- g$target_distances
    expect_named(td, c("target_id", "property", "space", "nearest", "mean_k"))
    expect_true(all(td$mean_k >= td$nearest))
    expect_false(isTRUE(all.equal(td$mean_k, td$nearest)))

  }

})


test_that("at k = 1 batch and global record every twin in the measured pool, with one distance row per target per property", {

  ## The parity tests cannot see a fault inside draw_neighbours() that both
  ## scopes share. This one fetches every measured row, flags them all, and
  ## asks both scopes to have recorded exactly that set at the smallest k,
  ## where the fetch is narrowest: the reference width, 15 of the 63 rows for
  ## clay and 8 of the 32 for oc.

  fx <- select_fixture(n_pool = 60, seed = 3, n_replicates = 3)
  rc <- pool_on_targets(fx)
  tm <- predictor_matrix(fx$targets)
  sp <- build_similarity_space(rc$matrix, rc$wavenumbers, sdev_floor = SELECT_SDEV_FLOOR)
  St <- project_similarity(sp, tm$matrix, tm$wavenumbers)
  a  <- fx$pool$data$analysis

  brute_force <- function(p, ratio) {

    measured <- intersect(rownames(sp$scores), a$sample_id[!is.na(a[[p]])])
    nn  <- nearest_neighbours(St, sp$scores[measured, , drop = FALSE], k = length(measured),
                              metric = "mahalanobis", sdev = sp$sdev)
    ref <- twin_reference(nn$dist, k_ref = twin_reference_width(length(measured)))
    hit <- which(twin_flags(nn$dist, ref, ratio = ratio), arr.ind = TRUE)

    paste(p, rownames(nn$ids)[hit[, "row"]], nn$ids[hit])

  }

  key <- function(ex) paste(ex$property, ex$target_id, ex$pool_id)

  for (ratio in c(SELECT_TWIN_RATIO, 0.4)) {

    truth <- c(brute_force("clay", ratio), brute_force("oc", ratio))
    expect_gt(length(truth), 0L)

    for (scope in c("batch", "global")) {

      sel <- quiet_select(fx, k = 1, scope = scope, twin_ratio = ratio)$selection
      ex  <- sel$exclusions
      expect_setequal(key(ex), truth)

      ## One distance row per target per property, as batch writes them
      td <- sel$target_distances
      expect_false(anyNA(td$property))
      expect_identical(nrow(td), 2L * fx$targets$data$n_rows)

    }

  }

})


test_that("a draw the twin subtraction empties stops with the property and the cause", {

  ## At a twin_ratio this close to 1 every row any target drew is some
  ## other target's twin. The subset used to fail on an empty keep, naming
  ## neither the property nor the twin rule.

  ## The scenario depends on the fixture's exact geometry, so the filter is
  ## pinned to the one it was built under: 11 points on the targets' 8 cm-1
  ## grid, 80 cm-1 now that the window is a width.

  fx <- select_fixture(n_pool = 60, seed = 3, n_replicates = 3)

  err <- expect_error(quiet_select(fx, k = 1, metric = "cosine", twin_ratio = 0.99, properties = "clay",
                                   window = 80),
                      regexp = "Every row drawn for clay was flagged", class = "horizons_input_error")

  ## Ordinary neighbours were flagged, so the ratio is the lever
  expect_match(conditionMessage(err), "lower it")

  ## Global keeps the pool, so there is nothing to empty
  expect_no_error(quiet_select(fx, k = 1, metric = "cosine", twin_ratio = 0.99,
                               properties = "clay", scope = "global", window = 80))

})


#' Pool rows as targets, on the pool's own grid, ids prefixed "T"; every row
#' unless `rows` picks some
#' @noRd
pool_as_targets <- function(fx, rows = NULL) {

  pa <- fx$pool$data$analysis
  if (!is.null(rows)) pa <- pa[rows, ]
  tg <- pa[, c("sample_id", grep("^wn_", names(pa), value = TRUE))]
  tg$sample_id <- paste0("T", tg$sample_id)

  utils::capture.output(targets <- spectra(tg))
  targets

}


test_that("a draw emptied by the targets' own copies says so, not twin_ratio", {

  ## Targets that are the pool's own rows: every row a target draws is
  ## another target's copy, at distance zero to rounding, and no twin_ratio
  ## brings it back.

  fx      <- select_fixture(n_pool = 60, seed = 3)
  targets <- pool_as_targets(fx)

  err <- expect_error(select_training(targets, fx$pool, k = 1, properties = "clay"),
                      regexp = "Every row drawn for clay was flagged", class = "horizons_input_error")

  expect_match(conditionMessage(err), "targets are in the pool")
  expect_no_match(conditionMessage(err), "lower it")

})


test_that("a target's own copy is recorded as exact, at distance zero to rounding", {

  ## The same spectrum reaches the space as a pool row and as a projected
  ## target. With the whole pool as targets the projection repeats the fit
  ## and the copies land at exactly 0; with a subset the matrix product
  ## rounds differently and they land near 1e-16, which the record called
  ## "neighbourhood" while the emptied-draw error treated them as copies.
  ## Whether a subset rounds to 0 depends on the BLAS, so the test asserts
  ## the outcome, not the distance. On the replicate fixture's first eight
  ## rows seven copies land above 0 on the reference box.

  fx <- select_fixture(n_pool = 60, seed = 3, n_replicates = 3)

  for (rows in list(NULL, 1:8)) {

    targets <- pool_as_targets(fx, rows)
    n_t     <- targets$data$n_rows

    ex <- select_training(targets, fx$pool, k = 1, properties = "clay", scope = "global")$selection$exclusions

    own <- ex$pool_id == sub("^T", "", ex$target_id)

    ## Every target's copy is flagged and recorded exact
    expect_identical(sum(own), n_t)
    expect_true(all(ex$reason[own] == "exact"))

    ## And nothing that is not a copy is called exact
    expect_true(all(ex$reason[!own] == "neighbourhood"))

  }

})


test_that("a property with no measured pool row stops every scope", {

  ## Global used to skip such a property; batch failed inside the draw.

  fx <- select_fixture(n_pool = 60)
  fx$pool$data$analysis$oc <- NA_real_

  for (scope in c("batch", "global")) {

    err <- expect_error(quiet_select(fx, k = 5, scope = scope),
                        regexp = "measured rows for oc", class = "horizons_input_error")

    ## No depth column, so no depth was applied and no hint about it
    expect_no_match(conditionMessage(err), "depth = \"all\"", fixed = TRUE)

  }

  ## oc measured only below 30 cm: topsoil leaves it unmeasured, and the
  ## refusal says depth = "all" would reach it
  deep <- select_fixture(n_pool = 60)
  upper <- rep(c(0, 60), length.out = 60)
  deep$pool$data$analysis$upper_depth_cm <- upper
  deep$pool$data$analysis$oc[upper < 30] <- NA_real_
  deep$pool$data$role_map <- rbind(deep$pool$data$role_map,
                                   tibble::tibble(variable = "upper_depth_cm", role = "meta"))

  err <- expect_error(quiet_select(deep, k = 5),
                      regexp = "measured rows for oc", class = "horizons_input_error")
  expect_match(conditionMessage(err), "depth = \"all\"", fixed = TRUE)

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

  fx  <- select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10)

  s <- out$selection
  expect_identical(s$settings$scope,  "batch")
  expect_identical(s$settings$metric, "euclidean")
  expect_identical(s$settings$space,  "pca")
  expect_true(s$settings$ncomp_retained >= 1L)
  expect_identical(s$reconciliation$operation, "subset")
  expect_identical(s$reconciliation$n_shared, length(fx$target_wn))
  expect_identical(s$reconciliation$shared_range, c(600, 4000))
  expect_identical(s$reconciliation$target_only, numeric(0))
  expect_identical(s$pool$n_rows, 100L)
  expect_match(s$pool$id_hash, "^[0-9a-f]+$")
  expect_named(s$target_distances, c("target_id", "property", "space", "nearest", "mean_k"))
  expect_identical(nrow(s$target_distances), 16L)
  expect_true(all(s$target_distances$space == "all"))
  expect_s3_class(s$timestamp, "POSIXct")

})


test_that("the record carries the targets' source, size and id hash", {

  fx <- select_fixture(n_pool = 60)
  fx$targets$provenance$spectra_source <- "batch_07.csv"

  out <- quiet_select(fx, k = 5)

  ## Hashed the way selection$pool$id_hash is: ids in radix order, so
  ## neither row order nor the session's collation locale changes it.
  ids <- fx$targets$data$analysis$sample_id
  rec <- out$selection$targets

  expect_named(rec, c("source", "n_rows", "id_hash"))
  expect_identical(rec$source,  "batch_07.csv")
  expect_identical(rec$n_rows,  8L)
  expect_identical(rec$id_hash, digest::digest(sort(ids, method = "radix")))

  reordered <- select_training(subset_rows(fx$targets, rev(ids)), fx$pool, k = 5)

  expect_identical(reordered$selection$targets$id_hash, rec$id_hash)

  ## It describes the draw, so rows leaving afterwards leave it alone; the
  ## object's provenance stays the library's.
  kept <- subset_rows(out, out$data$analysis$sample_id[-1])

  expect_identical(kept$selection$targets, rec)
  expect_identical(out$provenance$spectra_source, "tibble")

})


test_that("the id hashes take radix order under a collation that orders case otherwise", {

  fx  <- select_fixture(n_pool = 60)
  ids <- fx$targets$data$analysis$sample_id

  mixed <- fx
  lower_every_other <- function(x) ifelse(seq_along(x) %% 2 == 0, tolower(x), x)
  mixed$targets$data$analysis$sample_id <- lower_every_other(ids)
  mixed$pool$data$analysis$sample_id    <- lower_every_other(fx$pool$data$analysis$sample_id)

  t_ids <- mixed$targets$data$analysis$sample_id
  p_ids <- mixed$pool$data$analysis$sample_id

  suppressWarnings(withr::local_collate("en_US.UTF-8"))
  skip_if(identical(sort(c(t_ids, p_ids)), sort(c(t_ids, p_ids), method = "radix")),
          "no locale here that collates case apart from radix order")

  hashed <- quiet_select(mixed, k = 5)

  expect_identical(hashed$selection$targets$id_hash, digest::digest(sort(t_ids, method = "radix")))
  expect_identical(hashed$selection$pool$id_hash, digest::digest(sort(p_ids, method = "radix")))

})


## The training set is the library's own columns at the wavenumbers the
## targets share (#236), and its standardization record describes those
## columns (#90). The fixture already sits on the two lattices, so
## standardize() records each grid without re-interpolating.

test_that("targets on a coarser grid get the library's own values there, and the record says step 8 (#236, #90)", {

  fx <- select_fixture(n_pool = 100)

  utils::capture.output({
    pool    <- standardize(fx$pool,    resample = 4, trim = c(600, 4000))
    targets <- standardize(fx$targets, resample = 8, trim = c(600, 4000))
  })

  out <- select_training(targets, pool, k = 10)
  rec <- out$provenance$standardization
  wn  <- predictor_matrix(out)$wavenumbers

  ## Every target column is shared, so nothing is warned and none is missing
  expect_identical(out$selection$reconciliation$operation, "subset")
  expect_identical(out$selection$reconciliation$n_shared, 426L)
  expect_identical(out$selection$reconciliation$target_only, numeric(0))

  ## The library's own values at the targets' wavenumbers, not a spline's
  pred <- function(x) x$data$role_map$variable[x$data$role_map$role == "predictor"]
  expect_identical(pred(out), pred(targets))
  expect_identical(predictor_matrix(out)$matrix,
                   predictor_matrix(subset_rows(pool, out$data$analysis$sample_id))$matrix[, match(pred(out), pred(pool))])

  ## The recorded step is the axis's spacing, the targets' 8, not the pool's 4
  expect_equal(rec$grid$step, stats::median(abs(diff(wn))))
  expect_equal(rec$grid$step, 8)
  expect_identical(rec$grid, targets$provenance$standardization$grid)
  expect_identical(rec$n_wavelengths, length(wn))
  expect_equal(rec$wavelength_range, range(wn))

  ## So axis_spacing_cm()'s fallback agrees with the axis it reads first
  expect_equal(rec$grid$step, axis_spacing_cm(out))

  ## Nothing was moved, so there is no record of a move
  expect_false("reconciliation" %in% names(rec))

  ## The standardize() arguments that produced the values stay the pool's,
  ## and so do whether it resampled and when
  expect_identical(rec$resample, 4)
  expect_identical(rec$trim, c(600, 4000))
  expect_identical(rec$remove_water, FALSE)
  expect_identical(rec$baseline, FALSE)
  expect_identical(rec$resampled, pool$provenance$standardization$resampled)
  expect_identical(rec$applied_at, pool$provenance$standardization$applied_at)

})


test_that("a pool already on the targets' grid keeps its standardization record (#90)", {

  fx <- select_fixture(n_pool = 100)

  utils::capture.output({
    pool    <- standardize(fx$pool,    resample = 8, trim = c(600, 4000))
    targets <- standardize(fx$targets, resample = 8, trim = c(600, 4000))
  })

  out <- select_training(targets, pool, k = 10)

  expect_identical(out$selection$reconciliation$operation, "none")
  expect_identical(out$provenance$standardization, pool$provenance$standardization)

})


test_that("the record describes the columns returned whatever the targets' record, and an unstandardized pool gives none (#90)", {

  fx <- select_fixture(n_pool = 100)

  ## The targets' axis is the 8 cm-1 lattice, but resample = NULL records no
  ## grid for it. The columns returned are the library's at those
  ## wavenumbers, evenly spaced on a library that had a grid, so the record
  ## gives them one.
  utils::capture.output({
    pool    <- standardize(fx$pool,    resample = 4,    trim = c(600, 4000))
    targets <- standardize(fx$targets, resample = NULL, trim = c(600, 4000))
  })

  rec <- select_training(targets, pool, k = 10)$provenance$standardization

  expect_null(targets$provenance$standardization$grid)
  expect_identical(rec$grid, list(min = 600, max = 4000, step = 8, n = 426L))
  expect_identical(rec$n_wavelengths, fx$targets$data$n_predictors)

  ## A pool standardize() never saw is not marked standardized
  expect_null(quiet_select(fx, k = 10)$provenance$standardization)

})


test_that("targets on an instrument's own axis share no column and are refused, naming the standardize() call (#236)", {

  fx <- select_fixture(n_pool = 100)

  ## Off-grid, 599.7431 + 1.928571 k: on no canonical grid, and no column
  ## named as the pool's 4 cm-1 grid names them
  offgrid_wn <- rev(599.7431 + 1.928571 * (0:1762))

  set.seed(3)
  m <- rbind(gaussian_family(4, offgrid_wn, centres = c(3400, 2920, 1630, 1030)),
             gaussian_family(4, offgrid_wn, centres = c(3620, 2515, 1420, 870)))
  colnames(m) <- paste0("wn_", offgrid_wn)

  tbl <- dplyr::bind_cols(tibble::tibble(sample_id = sprintf("M%02d", 1:8)),
                          tibble::as_tibble(m))

  ## resample = NULL and trim = NULL keep the instrument's axis as it came
  utils::capture.output({
    pool    <- standardize(fx$pool, resample = 4, trim = c(600, 4000))
    targets <- standardize(spectra(tbl), resample = NULL, trim = NULL)
  })

  err <- expect_error(select_training(targets, pool, k = 10), class = "horizons_error_input")
  expect_identical(rlang::call_name(err$call), "select_training")

  ## Standardized already, so the remedy carries force = TRUE
  expect_match(gsub("\\s+", " ", conditionMessage(err)),
               "standardize(x, resample = 4, trim = c(600, 4000), force = TRUE)", fixed = TRUE)

  expect_snapshot(select_training(targets, pool, k = 10), error = TRUE)

})


test_that("targets on an instrument's own axis that share a few columns by coincidence are refused (#236)", {

  ## 599.74 + 1.93 k shares four column names with the pool's 4 cm-1 grid
  ## (1144, 1916, 2688 and 3460), which without the alignment rule would be a
  ## training set of four columns behind a coverage warning
  fx <- select_fixture(n_pool = 60)

  coincident <- rev(599.74 + 1.93 * (0:1761))
  set.seed(3)
  m <- gaussian_family(4, coincident, centres = c(3400, 2920, 1630, 1030))
  colnames(m) <- paste0("wn_", coincident)
  utils::capture.output(targets <- spectra(dplyr::bind_cols(tibble::tibble(sample_id = sprintf("C%02d", 1:4)),
                                                            tibble::as_tibble(m))))

  expect_length(intersect(colnames(m), fx$pool$data$role_map$variable), 4L)

  err <- expect_error(select_training(targets, fx$pool, k = 5), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_identical(rlang::call_name(err$call), "select_training")
  expect_match(msg, "`x`'s wavenumbers are not on a grid aligned with `library`'s", fixed = TRUE)
  expect_match(msg, "4 of the targets' 1761 columns inside the library's range coincide", fixed = TRUE)

})


test_that("targets finer than the library get its own columns, with one coverage warning naming the rest (#236)", {

  fx      <- select_fixture(n_pool = 60)
  targets <- make_axis_object(seq(4000, 600, by = -2), n = 4L, prefix = "F")

  w <- expect_warning(out <- select_training(targets, fx$pool, k = 5, properties = "clay"),
                      class = "horizons_warning_coverage")

  pred <- function(x) x$data$role_map$variable[x$data$role_map$role == "predictor"]

  ## The pool's whole axis comes back, as it was: nothing interpolated up
  expect_identical(pred(out), pred(fx$pool))
  expect_identical(out$selection$reconciliation$operation, "none")
  expect_identical(predictor_matrix(out)$matrix,
                   predictor_matrix(subset_rows(fx$pool, out$data$analysis$sample_id))$matrix)

  ## The 850 target columns between the pool's are named, in the record and
  ## on the log row the warning carries
  only <- setdiff(pred(targets), pred(fx$pool))
  expect_length(only, 850L)
  expect_identical(out$selection$reconciliation$target_only, as.numeric(sub("^wn_", "", only)))

  msg <- gsub("\\s+", " ", conditionMessage(w))
  expect_match(msg, "850 of the targets' 1701 wavenumber columns are not in the library", fixed = TRUE)
  expect_match(msg, "850 fall between the library's columns, which are 4 cm-1 apart", fixed = TRUE)
  expect_false(grepl("outside", msg))
  expect_identical(w$log$ids[[1]], only)
  expect_identical(w$log$n, 850L)

})


test_that("targets wider than the library get the columns they share, with the rest named (#236)", {

  ## 4096 to 504 at 8 cm-1 against a pool at 600 to 4000: twelve columns past
  ## each end, which was a refusal
  fx <- select_fixture(n_pool = 60, target_range = c(4096, 504))

  w <- expect_warning(out <- select_training(fx$targets, fx$pool, k = 5, properties = "clay"),
                      class = "horizons_warning_coverage")

  wn <- predictor_matrix(out)$wavenumbers
  expect_identical(range(wn), c(600, 4000))
  expect_identical(wn, seq(4000, 600, by = -8))
  expect_identical(sort(out$selection$reconciliation$target_only),
                   sort(c(seq(4096, 4008, by = -8), seq(592, 504, by = -8))))

  msg <- gsub("\\s+", " ", conditionMessage(w))
  expect_match(msg, "24 of the targets' 450 wavenumber columns are not in the library", fixed = TRUE)
  expect_match(msg, "24 lie outside the library's 600 to 4000 cm-1", fixed = TRUE)

})


test_that("targets on an offset step get the columns both grids share, at their common step (#236)", {

  ## Targets at 6 cm-1 against a pool at 4: they share every 12 cm-1
  fx <- select_fixture(n_pool = 60)

  utils::capture.output(pool <- standardize(fx$pool, resample = 4, trim = c(600, 4000)))
  targets <- make_axis_object(seq(3996, 600, by = -6), n = 4L, prefix = "S")

  w <- expect_warning(out <- select_training(targets, pool, k = 5, properties = "clay"),
                      class = "horizons_warning_coverage")

  wn <- predictor_matrix(out)$wavenumbers
  expect_identical(wn, seq(3996, 600, by = -12))

  rec <- out$provenance$standardization
  expect_identical(rec$grid, list(min = 600, max = 3996, step = 12, n = length(wn)))
  expect_identical(rec$resample, 4)
  expect_match(gsub("\\s+", " ", conditionMessage(w)),
               "standardize the targets at a multiple of 4 cm-1 to share every column", fixed = TRUE)

})


## Standardized alike (#107): the training set is the library's spectra as
## standardize() left them, so a hull on one side only, or hulls fitted over
## different ranges, is refused before any work.

test_that("a baseline on one side only is refused, either way round (#107)", {

  fx <- select_fixture(n_pool = 60)

  utils::capture.output({
    pool_hull    <- standardize(fx$pool,    resample = 4, baseline = TRUE)
    pool_plain   <- standardize(fx$pool,    resample = 4)
    target_hull  <- standardize(fx$targets, resample = 8, baseline = TRUE)
    target_plain <- standardize(fx$targets, resample = 8)
  })

  ## The library corrected, the targets not
  err <- expect_error(select_training(target_plain, pool_hull, k = 5), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_identical(rlang::call_name(err$call), "select_training")
  expect_match(msg, "were standardized differently", fixed = TRUE)
  expect_match(msg, "The library was baseline-corrected", fixed = TRUE)
  expect_match(msg, "standardize(x, resample = 8, trim = c(600, 4000), baseline = TRUE, force = TRUE)", fixed = TRUE)
  ## The library-side remedy starts from spectra(): standardizing the corrected
  ## library again would record no baseline over values that still carry it.
  ## It keeps the library's step, which resample's default would change.
  expect_match(msg, "Or standardize the library from `spectra()` without `baseline`, `standardize(library, resample = 4, trim = c(600, 4000))`; a hull already subtracted cannot be put back", fixed = TRUE)

  ## The targets corrected, the library not
  err <- expect_error(select_training(target_hull, pool_plain, k = 5), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_match(msg, "The targets were baseline-corrected", fixed = TRUE)
  expect_match(msg, "a hull already subtracted cannot be put back", fixed = TRUE)
  expect_match(msg, "`standardize(library, resample = 4, trim = c(600, 4000), baseline = TRUE, force = TRUE)`", fixed = TRUE)

  ## Corrected on both sides over the same range, or on neither, runs
  expect_s3_class(select_training(target_hull, pool_hull, k = 5), "horizons_data")
  expect_s3_class(select_training(target_plain, pool_plain, k = 5), "horizons_data")

})


test_that("a side never standardized counts as not baseline-corrected (#107)", {

  fx <- select_fixture(n_pool = 60)

  utils::capture.output({
    pool_hull   <- standardize(fx$pool,    resample = 4, baseline = TRUE)
    target_hull <- standardize(fx$targets, resample = 8, baseline = TRUE)
  })

  expect_null(fx$pool$provenance$standardization)
  expect_null(fx$targets$provenance$standardization)

  expect_error(select_training(target_hull, fx$pool, k = 5),
               "The targets were baseline-corrected", class = "horizons_error_input")
  expect_error(select_training(fx$targets, pool_hull, k = 5),
               "The library was baseline-corrected", class = "horizons_error_input")

  ## Unstandardized targets need no force = TRUE to be corrected
  err <- expect_error(select_training(fx$targets, pool_hull, k = 5), class = "horizons_error_input")
  expect_match(gsub("\\s+", " ", conditionMessage(err)),
               "standardize(x, resample = 8, trim = c(600, 4000), baseline = TRUE)`", fixed = TRUE)

})


test_that("hulls fitted over ranges an end apart by a step or more are refused (#107)", {

  fx <- select_fixture(n_pool = 60)

  utils::capture.output({
    pool    <- standardize(fx$pool,    resample = 4, baseline = TRUE)
    trimmed <- standardize(fx$targets, resample = 8, trim = c(1000, 3000), baseline = TRUE)
    one_off <- standardize(fx$targets, resample = 8, trim = c(608, 4000),  baseline = TRUE)
  })

  err <- expect_error(select_training(trimmed, pool, k = 5), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_match(msg, "the library's hull over 600 to 4000 cm-1 and the targets' over 1000 to 3000 cm-1", fixed = TRUE)
  expect_snapshot(select_training(trimmed, pool, k = 5), error = TRUE)

  ## One end off by the coarser grid's step, 8 cm-1, is a different hull
  expect_error(select_training(one_off, pool, k = 5),
               "hulls are pinned at different ends", class = "horizons_error_input")

})


test_that("hulls over ranges an end apart by less than the coarser step pass (#107)", {

  ## A 6 cm-1 grid over 600 to 4000 ends at 3996: 4 cm-1 short of the 4 cm-1
  ## library's end, under the coarser 6 cm-1 step, so it is the same hull as
  ## close as the two grids land. It shares every 12 cm-1, so it warns.
  fx <- select_fixture(n_pool = 60)

  utils::capture.output({
    pool    <- standardize(fx$pool,    resample = 4, baseline = TRUE)
    targets <- standardize(fx$targets, resample = 6, baseline = TRUE)
  })

  expect_identical(targets$provenance$standardization$wavelength_range, c(600, 3996))

  expect_warning(out <- select_training(targets, pool, k = 5), class = "horizons_warning_coverage")
  expect_s3_class(out, "horizons_data")

})


test_that("hulls over different ranges name the range both reach, which a library file can meet too (#107)", {

  ## Targets from an instrument whose axis stops at 601.67 and 3998.47,
  ## standardized exactly as the 600 to 4000 library was: the grid points
  ## 600 and 4000 lie outside their data, so their hull runs 604 to 3996.
  ## Asking them to match the library's 600 to 4000 cannot be met; fitting
  ## the library's hull over the range both reach can.
  fx <- select_fixture(n_pool = 120)

  tm     <- predictor_matrix(fx$targets)
  raw_wn <- round(rev(599.74 + 1.93 * (0:1761)), 2)
  raw_wn <- raw_wn[raw_wn >= 600 & raw_wn <= 4000]
  rs     <- resample_spectra(tm$matrix, tm$wavenumbers, new_wav = raw_wn)
  m      <- rs$matrix
  colnames(m) <- paste0("wn_", rs$wavelengths)

  utils::capture.output({
    raw     <- spectra(dplyr::bind_cols(tibble::tibble(sample_id = rownames(tm$matrix)), tibble::as_tibble(m)))
    targets <- suppressWarnings(standardize(raw, resample = 4, trim = c(600, 4000), baseline = TRUE))
    pool    <- standardize(fx$pool, resample = 4, trim = c(600, 4000), baseline = TRUE)
  })

  expect_identical(targets$provenance$standardization$wavelength_range, c(604, 3996))

  err <- expect_error(select_training(targets, pool, k = 10, properties = "clay"), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_match(msg, "Fit both hulls over the range both reach, 604 to 3996 cm-1", fixed = TRUE)
  expect_match(msg, "the library with `standardize(library, resample = 4, trim = c(604, 3996), baseline = TRUE)`", fixed = TRUE)

  ## The targets already fit theirs there, so they are not asked to move
  expect_false(grepl("the targets with", msg, fixed = TRUE))

  ## A library file is the user's to standardize again, so it is offered the
  ## same remedy
  path <- file.path(withr::local_tempdir(), "library.qs2")
  qs2::qs_save(pool, path)
  err <- expect_error(select_training(targets, path, k = 10, properties = "clay"), class = "horizons_error_input")
  expect_match(gsub("\\s+", " ", conditionMessage(err)), "the library with `standardize(library, resample = 4", fixed = TRUE)

  ## Following it from spectra() is accepted
  utils::capture.output(pool_both <- standardize(fx$pool, resample = 4, trim = c(604, 3996), baseline = TRUE))
  expect_s3_class(select_training(targets, pool_both, k = 10, properties = "clay"), "horizons_data")

})


test_that("the record carries the SG window in cm-1 and the space's floor", {

  fx  <- select_fixture(n_pool = 100)
  out <- suppressWarnings(quiet_select(fx, k = 10))

  s <- out$selection$settings

  ## window is a width in cm-1 and becomes the nearest odd point count on
  ## the grid the space is built on, the library's. The fixture's pool is on
  ## a 4 cm-1 grid (its targets on 8), where 40 cm-1 is 11 points exactly.
  expect_equal(s$window, SELECT_SG_WINDOW_CM)
  expect_identical(s$window_points, 11L)
  expect_equal(s$window_cm, (11 - 1) * out$selection$search$library_grid$resolution)
  expect_equal(s$window_cm, 40)

  ## derivative = 0 means no filter and so no width
  flat <- suppressWarnings(quiet_select(fx, k = 10, derivative = 0L))
  expect_true(is.na(flat$selection$settings$window_cm))
  expect_true(is.na(flat$selection$settings$window_points))

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

  expect_error(quiet_select(fx, k = 10, sdev_floor = 1),   regexp = "`sdev_floor` must be",
               fixed = TRUE, class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 10, sdev_floor = -0.1), regexp = "`sdev_floor` must be",
               fixed = TRUE, class = "horizons_input_error")

})


test_that("a PLS space records and prints that it selected on the pool's own responses", {

  fx  <- select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "clay", space = "pls", ncomp = 3L)

  note <- out$selection$settings$space_note
  expect_true(is.character(note))
  expect_match(note, "optimistic")
  expect_match(note, "clay")

  ## A PCA run carries no such note
  expect_null(quiet_select(fx, k = 10, properties = "clay")$selection$settings$space_note)

  txt <- utils::capture.output(select_training(fx$targets, fx$pool, k = 10, properties = "clay",
                                               space = "pls", ncomp = 3L,
                                               control = control_horizons(verbose = TRUE)))
  expect_true(any(grepl("optimistic", txt)))

})


test_that("the return validates and runs through the ordinary chain, and predicts targets with columns it lacks", {

  skip_unless_slow_tier()
  skip_on_cran()

  ## Targets 4096 to 504 cm-1 against a pool at 600 to 4000: the training
  ## set has the 426 columns they share, and the targets carry 24 more,
  ## which predict() must leave alone (#236)
  fx <- select_fixture(n_pool = 300, target_range = c(4096, 504))

  expect_warning(out <- quiet_select(fx, k = 30, properties = "clay"),
                 class = "horizons_warning_coverage")

  expect_no_error(validate_horizons_data(out))

  ## A small tuning budget: the chain is under test, not the search.
  utils::capture.output({
    cfg <- configure(out, outcome = "clay", models = "rf", cv_folds = 3L,
                     grid_size = 2L, bayesian_iter = 0L, final_bayesian_iter = 0L)
    cfg <- validate(cfg)
  })

  expect_s3_class(cfg, "horizons_data")
  expect_true(any(cfg$data$role_map$role == "outcome"))

  ## prune = FALSE keeps the chain independent of the pruning rule. With no
  ## Bayesian stage nothing is pruned today, but the chain is what is under
  ## test, so it shouldn't start depending on that.
  ev <- suppressWarnings(evaluate(cfg, prune = FALSE))
  expect_s3_class(ev, "horizons_eval")
  expect_true(nrow(ev$evaluation$results) >= 1L)
  expect_true(any(ev$evaluation$results$status == "success"))

  ## The targets carry none of the provenance columns (.drawn_by,
  ## .min_distance, .group) the training set does. predict() must not
  ## require them: the recipe's meta role is not baked (2026-09-21).
  ## The drawn set leaves 29 calibration rows, so the floor is lowered for the
  ## applicability domain to run
  f <- suppressWarnings(fit(ev, n_best = 1L, compute_uq = FALSE, compute_ad = TRUE,
                            control = control_horizons(min_calib_rows = 20L)))
  expect_s3_class(f, "horizons_fit")
  expect_identical(f$models$results$status, "success")
  expect_false(any(c(".drawn_by", ".min_distance", ".group") %in% names(fx$targets$data$analysis)))

  ## The applicability domain ran, so its bake and distance meet the
  ## targets' extra columns too
  expect_false(is.null(f$models$ad[[f$models$best_config]]))
  extra <- setdiff(names(fx$targets$data$analysis), f$models$predictor_schema)
  expect_length(setdiff(extra, "sample_id"), 24L)

  p <- predict(f, fx$targets, interval = FALSE)
  expect_identical(nrow(p), fx$targets$data$n_rows)
  expect_true(all(is.finite(p$.pred)))
  expect_true(all(is.finite(p$.ad_distance)))

})


## =============================================================================
## scope = "cluster"
## =============================================================================

test_that("scope = 'cluster' yields one group per target cluster", {

  fx  <- select_fixture(n_pool = 100)
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

  fx <- select_fixture(n_pool = 100)

  ## The fallback goes through the verb's own console layer, so it obeys
  ## verbose. Routing it through cli made select_training(verbose = FALSE)
  ## print regardless.
  txt <- utils::capture.output(
    out <- select_training(fx$targets, fx$pool, k = 10, scope = "cluster", cluster_min = 5,
                           control = control_horizons(verbose = TRUE))
  )

  expect_true(any(grepl("one group", txt)))
  expect_identical(nrow(out$selection$groups), 1L)
  expect_identical(out$selection$clustering$k, 1L)

  expect_silent(select_training(fx$targets, fx$pool, k = 10, scope = "cluster",
                                cluster_min = 5))

})


## =============================================================================
## scope = "sample"
## =============================================================================

test_that("scope = 'sample' gives one group per target over the same union", {

  fx    <- select_fixture(n_pool = 100)
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

  fx <- select_fixture(n_pool = 100)

  base <- quiet_select(fx, k = 10, properties = "clay")
  maha <- quiet_select(fx, k = 10, properties = "clay", metric = "mahalanobis")
  cosn <- quiet_select(fx, k = 10, properties = "clay", metric = "cosine")
  pls  <- quiet_select(fx, k = 10, properties = "clay", space = "pls", ncomp = 3L)

  ids <- function(o) o$selection$membership$pool_id

  expect_identical(base$selection$settings$metric, "euclidean")
  expect_false(identical(ids(base), ids(maha)))
  expect_false(identical(ids(base), ids(cosn)))
  expect_false(identical(ids(base), ids(pls)))
  expect_identical(pls$selection$settings$space, "pls")

  ## A mask over the fixture's two peaks, drawing for two properties
  mask   <- rbind(c(1350, 1700))
  both   <- quiet_select(fx, k = 10, properties = c("clay", "oc"))
  masked <- quiet_select(fx, k = 10, properties = c("clay", "oc"), mask = mask)

  expect_identical(masked$selection$settings$mask, mask)
  expect_false(identical(ids(both), ids(masked)))

})


test_that("the space is always the library's: space_rows is gone", {

  ## A space per property on its measured rows was removed on 2026-09-30:
  ## on KSSL it moved the drawn training set by about 6 %.

  fx  <- select_fixture(n_pool = 60)
  out <- quiet_select(fx, k = 5)

  expect_null(out$selection$settings$space_rows)
  expect_true(all(out$selection$membership$space == "all"))
  expect_error(quiet_select(fx, k = 5, space_rows = "measured"), "unused argument")

})


## =============================================================================
## Output
## =============================================================================

test_that("verbose = TRUE reports the pool, the space and the draw", {

  fx <- select_fixture(n_pool = 60)

  expect_snapshot(out <- select_training(fx$targets, fx$pool, k = 5,
                                         control = control_horizons(verbose = TRUE)))

})


## =============================================================================
## Leakage detector
## =============================================================================

test_that("permuting the pool's responses collapses the evaluated CV", {

  skip_unless_slow_tier()

  ## The single highest-leverage leakage detector on this path: if the
  ## selection or the recipe were carrying any information about the outcome
  ## that it should not, a model trained on shuffled labels would still
  ## score. It must not. RPD near 1 is the honest answer for noise.
  ##
  ## The pool's clay is a known function of its spectra, so the same pipeline
  ## on the unshuffled pool has to learn it. That is what makes the collapse
  ## mean something, and it is what fails if the selection pairs responses
  ## with the wrong spectra. The floor of 1.1 sits at least 0.4 below the
  ## lowest CV RPD over twenty runs varying the clay noise and evaluate()'s
  ## seed. A forest, because it also finds a leak that is not linear in the
  ## predictors.

  skip_on_cran()

  fx      <- select_fixture(n_pool = 300)
  fx$pool <- learnable_clay_pool(fx$pool)

  cv_rpd <- function(pool) {

    out <- select_training(fx$targets, pool, k = 40, properties = "clay")

    utils::capture.output({
      cfg <- configure(out, outcome = "clay", models = "rf", cv_folds = 3L,
                       grid_size = 2L, bayesian_iter = 0L, final_bayesian_iter = 0L)
      cfg <- validate(cfg)
    })

    ev  <- suppressWarnings(evaluate(cfg, prune = FALSE))
    res <- ev$evaluation$results
    res <- res[res$status == "success", , drop = FALSE]

    expect_identical(nrow(res), 1L)
    expect_true(all(c("rpd", "cv_rpd") %in% names(res)))

    c(cv = max(res$cv_rpd, na.rm = TRUE), test = max(res$rpd, na.rm = TRUE))

  }

  learned <- cv_rpd(fx$pool)

  expect_gte(learned[["cv"]], 1.1)

  set.seed(4)
  shuffled <- fx$pool
  shuffled$data$analysis$clay <- sample(shuffled$data$analysis$clay)

  scores   <- cv_rpd(shuffled)
  permuted <- scores[["test"]]

  expect_lt(scores[["cv"]], 1.3)

  ## Tolerant on the upper side: this is a synthetic fixture and a small
  ## forest, so the point is that the permuted model has no signal at all,
  ## not where exactly the real one lands.
  expect_lt(permuted, 1.3)

})


## =============================================================================
## Row alignment before positional binds (#140)
## =============================================================================

test_that("select_training() refuses to bind the provenance columns to reordered rows", {

  fx               <- select_fixture(n_pool = 60)
  real_subset_rows <- subset_rows

  ## A subset that returned the drawn rows out of keep order would put each
  ## row's .drawn_by, .min_distance and .group on another row, silently.
  local_mocked_bindings(subset_rows = function(x, keep) {
    real_subset_rows(x, if (is.character(keep)) rev(keep) else keep)
  })

  expect_error(quiet_select(fx, k = 5), "Provenance columns",
               class = "horizons_error_internal")

})


test_that("select_training() refuses to return a record of the wrong shape", {

  ## The exit validation holds the verb's own output to the record's shape
  fx <- select_fixture(n_pool = 60)

  local_mocked_bindings(build_groups = function(...) tibble::tibble(group = 1L))

  utils::capture.output(
    expect_error(quiet_select(fx, k = 5), "selection\\$groups",
                 class = "horizons_error_validation")
  )

})
