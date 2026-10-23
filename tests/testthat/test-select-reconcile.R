# tests/testthat/test-select-reconcile.R
# Tests for select_training()'s two axes: predictor_matrix(), grid_summary()
# and the axis door (check_select_axis()); shared_columns(), the door for
# targets that share no column, and returned_standardization(), the training
# set's record (#236, #90); and search_axis(), which puts the targets on the
# library's grid for the search.


## =============================================================================
## predictor_matrix()
## =============================================================================

test_that("predictor_matrix() returns the spectra with decreasing wavenumbers", {

  fx  <- select_fixture(n_pool = 20)
  out <- predictor_matrix(fx$pool)

  expect_type(out, "list")
  expect_identical(dim(out$matrix), c(20L, length(fx$pool_wn)))
  expect_identical(out$wavenumbers, fx$pool_wn)
  expect_identical(rownames(out$matrix), fx$pool$data$analysis$sample_id)
  expect_true(all(diff(out$wavenumbers) < 0))

})


## =============================================================================
## Contiguity — a deleted band is a refusal, not a spline fill
## =============================================================================

test_that("grid_summary() reports a contiguous grid as contiguous", {

  fx <- select_fixture(n_pool = 20)

  pool_grid   <- grid_summary(fx$pool_wn)
  target_grid <- grid_summary(fx$target_wn)

  expect_true(pool_grid$contiguous)
  expect_true(target_grid$contiguous)
  expect_null(pool_grid$gaps)
  expect_equal(pool_grid$max_spacing, 4)

})


test_that("check_select_axis() refuses an axis of one wavenumber, and passes a usable one", {

  ## One column has no spacing for grid_summary() to take a median of, and
  ## nothing for the derivative to run along
  expect_error(check_select_axis(4000, "x", "the targets"),
               regexp = "at least two wavenumbers", class = "horizons_error_input")
  expect_error(check_select_axis(numeric(0), "library", "the pool"),
               regexp = "has 0 predictor columns", class = "horizons_error_input")

  ## Two columns are an axis; a usable one returns its summary
  expect_identical(check_select_axis(c(4000, 3996), "x", "the targets")$n, 2L)
  expect_true(check_select_axis(seq(4000, 600, by = -4), "x", "the targets")$contiguous)

})


test_that("grid_summary() passes a gap-free axis that is merely non-uniform", {

  ## Two axes that a ratio-only rule calls gapped and that have no hole in
  ## them at all. A deleted band is an absolute width, not a ratio, so the
  ## rule needs both: more than three times the median *and* more than
  ## 30 cm-1.

  ## An NIR grid sampled evenly in nm: 20 cm-1 apart at the blue end,
  ## 3.2 at the red, with every column in between present.
  nir <- rev(1e7 / seq(1000, 2500, by = 2))
  g   <- grid_summary(nir)

  expect_gt(max(diff(sort(nir))), 3 * stats::median(diff(sort(nir))))
  expect_lt(g$max_spacing, 30)
  expect_true(g$contiguous)
  expect_null(g$gaps)

  ## A mid-IR axis merged from a 2 cm-1 and an 8 cm-1 source
  merged <- sort(unique(c(seq(4000, 2500, by = -8), seq(2500, 600, by = -2))))
  m      <- grid_summary(merged)

  expect_identical(m$max_spacing, 8)
  expect_true(m$contiguous)
  expect_null(m$gaps)

})


test_that("grid_summary() still catches a deleted band on either side of it", {

  ## The 140 cm-1 water range, which is both more than three times the median
  ## and far more than 30 cm-1 wide, on an axis that is otherwise regular.
  for (step in c(2, 4, 8)) {

    wn <- seq(4000, 600, by = -step)
    wn <- wn[!(wn >= 1580 & wn <= 1720)]
    g  <- grid_summary(wn)

    expect_false(g$contiguous)
    expect_identical(nrow(g$gaps), 1L)
    expect_gt(g$max_spacing, 140)

  }

  ## And a hole just over the absolute bar is still a hole, at any resolution
  wn <- seq(4000, 600, by = -4)
  wn <- wn[!(wn > 2000 & wn < 2036)]

  expect_false(grid_summary(wn)$contiguous)

})


## =============================================================================
## shared_columns() and its door: the library's own columns the targets share
## =============================================================================

test_that("shared_columns() keeps the library's columns the targets name, in the library's order", {

  lib <- paste0("wn_", seq(4000, 600, by = -2))

  ## Coarser targets share every one of their columns
  coarse <- paste0("wn_", seq(4000, 600, by = -8))
  out    <- shared_columns(coarse, lib)

  expect_identical(out$shared, coarse)
  expect_identical(out$target_only, character())

  ## Finer and wider targets: the library's columns inside, the rest named
  fine <- paste0("wn_", seq(4100, 600, by = -1))
  out  <- shared_columns(fine, lib)

  expect_identical(out$shared, lib)
  expect_identical(out$target_only, setdiff(fine, lib))

  ## A name, not a nearby value: an instrument's own axis, 0.26 cm-1 off the
  ## grid at its low end, shares nothing, and neither does a grid offset by
  ## a whole wavenumber
  instrument <- paste0("wn_", rev(599.7431 + 1.928571 * (0:1762)))
  expect_length(shared_columns(instrument, lib)$shared, 0L)
  expect_length(shared_columns(paste0("wn_", seq(3999, 601, by = -2)), lib)$shared, 0L)

})


test_that("axes_aligned() passes grids standardize() makes and refuses an instrument's own axis", {

  lib <- seq(4000, 600, by = -2)

  ## Coarser, finer and offset canonical grids lie on one lattice with it
  expect_true(axes_aligned(seq(4000, 600, by = -8), lib))
  expect_true(axes_aligned(seq(4000, 600, by = -1), lib))
  expect_true(axes_aligned(seq(3999, 600, by = -3), lib))
  expect_true(axes_aligned(seq(4096, 504, by = -8), lib))

  ## So do targets on the library's own axis, canonical or not
  native <- rev(599.7431 + 1.928571 * (0:1762))
  expect_true(axes_aligned(native[100:900], native))

  ## An instrument's own axis is refused, though it shares four columns with
  ## a 4 cm-1 grid by coincidence, and though 1.93 and 4 have a common
  ## divisor in decimal; so is a grid offset by a whole wavenumber
  coincident <- rev(599.74 + 1.93 * (0:1761))
  expect_length(intersect(coincident, seq(4000, 600, by = -4)), 4L)
  expect_false(axes_aligned(coincident, seq(4000, 600, by = -4)))
  expect_false(axes_aligned(seq(3999, 601, by = -2), lib))

  ## And the other way round: canonical targets against a library left on
  ## an instrument's axis
  expect_false(axes_aligned(seq(4000, 600, by = -4), coincident))

  ## A 7 cm-1 grid with one stray column at either end of the library's
  ## range is not a grid standardize() makes: the library's end columns are
  ## inside its range, so the stray one is read
  seventh <- seq(3997, 602, by = -7)
  expect_true(axes_aligned(seventh, lib))
  expect_false(axes_aligned(c(seventh, 600), lib))
  expect_false(axes_aligned(c(4000, seventh), lib))

})


test_that("check_shared_columns() refuses targets that share no column, naming the standardize() call", {

  lib <- paste0("wn_", seq(4000, 600, by = -2))
  off <- paste0("wn_", rev(599.7431 + 1.928571 * (0:1762)))

  err <- expect_error(check_shared_columns(off, lib), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_match(msg, "share no wavenumber column", fixed = TRUE)
  expect_match(msg, "standardize(x, resample = 2, trim = c(600, 4000))", fixed = TRUE)

  ## Targets standardized already need force = TRUE to be put on the grid
  err <- expect_error(check_shared_columns(off, lib, restandardize = TRUE),
                      class = "horizons_error_input")
  expect_match(gsub("\\s+", " ", conditionMessage(err)),
               "standardize(x, resample = 2, trim = c(600, 4000), force = TRUE)", fixed = TRUE)

  ## An instrument's own axis that shares a few columns by coincidence is
  ## refused too, saying so
  coincident <- paste0("wn_", rev(599.74 + 1.93 * (0:1761)))
  err <- expect_error(check_shared_columns(coincident, lib), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_match(msg, "not on a grid aligned with", fixed = TRUE)
  expect_match(msg, "9 of the targets' 1761 columns inside the library's range coincide with library columns", fixed = TRUE)
  expect_match(msg, "standardize(x, resample = 2, trim = c(600, 4000))", fixed = TRUE)

  ## Anything shared on an aligned grid passes, with the split
  expect_identical(check_shared_columns(lib[1:3], lib)$shared, lib[1:3])

})


test_that("check_shared_columns() names the library's call when it is off its grid, keeping an end on the step", {

  ## A library on an instrument's axis whose top column is exactly 4000: the
  ## trim it is named keeps 4000, a whole multiple of the step
  lib <- paste0("wn_", 4000 - 1.928571 * (0:1762))

  ## Targets on a 4 cm-1 grid of their own: the library's call alone, on
  ## their step
  err <- expect_error(check_shared_columns(paste0("wn_", seq(4000, 600, by = -4)), lib),
                      class = "horizons_error_input")
  expect_match(gsub("\\s+", " ", conditionMessage(err)),
               "Put it on one: `standardize(library, resample = 4, trim = c(604, 4000))`.", fixed = TRUE)

  ## Targets on no grid either, and standardized already: both calls, the
  ## targets' with force = TRUE
  off <- paste0("wn_", rev(599.74 + 1.93 * (0:1761)))
  err <- expect_error(check_shared_columns(off, lib, restandardize = TRUE), class = "horizons_error_input")
  expect_match(gsub("\\s+", " ", conditionMessage(err)),
               paste("Put it and the targets on one: `standardize(library, resample = 2, trim = c(602, 4000))`",
                     "and `standardize(x, resample = 2, trim = c(602, 4000), force = TRUE)`."),
               fixed = TRUE)

})


## =============================================================================
## returned_standardization(): the library's record, for the columns kept (#90)
## =============================================================================

#' A standardize()-shaped record on the canonical grid at `step`
#'
#' @param step [Numeric.] Grid step in cm-1.
#' @param ... Further keys, appended.
#'
#' @return [List.] Shaped as `standardize()` writes `data$standardization`.
#' @noRd
std_record <- function(step, ...) {

  n <- length(seq(4000, 600, by = -step))

  c(list(baseline         = TRUE,
         baseline_range   = c(600, 4000),
         wavelength_range = c(600, 4000),
         grid             = list(min = 600, max = 4000, step = step, n = n)),
    list(...))

}


test_that("returned_standardization() describes the columns kept and keeps the rest of the library's record (#90)", {

  ## A library at 4 cm-1, its columns kept at the targets' 8: the record says
  ## step 8, the axis's spacing, not the library's 4
  pool <- std_record(4, extra = "kept")
  wn   <- seq(4000, 600, by = -8)

  rec <- returned_standardization(pool, wn, n_library = pool$grid$n)

  expect_identical(rec$grid, std_record(8)$grid)
  expect_identical(rec$grid$n, length(wn))
  expect_identical(rec$wavelength_range, c(600, 4000))

  ## The library's hull, and anything else the library carried
  for (key in c("baseline", "baseline_range", "extra")) {

    expect_identical(rec[[key]], pool[[key]], info = key)

  }

  expect_false("reconciliation" %in% names(rec))

})


test_that("returned_standardization() keeps a whole axis's record, writes a NULL grid off a grid, and gives none for none", {

  pool <- std_record(4)

  ## Every library column kept: the record as it was
  expect_identical(returned_standardization(pool, seq(4000, 600, by = -4), pool$grid$n), pool)

  ## Columns not evenly spaced are on no grid, and the key stays
  uneven <- c(seq(4000, 2000, by = -4), seq(1992, 600, by = -8))
  rec    <- returned_standardization(pool, uneven, pool$grid$n)

  expect_true("grid" %in% names(rec))
  expect_null(rec$grid)
  expect_identical(names(rec), names(pool))
  expect_identical(rec$wavelength_range, c(600, 4000))

  ## A library on no grid gives its subset none either
  nogrid <- pool
  nogrid["grid"] <- list(NULL)
  rec <- returned_standardization(nogrid, seq(4000, 600, by = -8), pool$grid$n)

  expect_true("grid" %in% names(rec))
  expect_null(rec$grid)

  ## A library standardize() never saw is not given a record
  expect_null(returned_standardization(NULL, seq(4000, 600, by = -8), 851L))

})


## =============================================================================
## search_axis(): the targets on the library's grid, for the search only
## =============================================================================

test_that("search_axis() records each coverage tier with its shortfall and its grid", {

  ## A library at 2 cm-1 from 600 to 700, so half its spacing is 1 cm-1
  lib_wn <- seq(700, 600, by = -2)
  on     <- function(wn) outer(c(T1 = 1, T2 = 2), wn, function(i, w) i + sin(w / 25))

  ## On the library's own grid: the whole grid, nothing resampled
  wn  <- lib_wn
  out <- search_axis(lib_wn, on(wn), wn)

  expect_identical(out$record$mode, "full")
  expect_identical(out$record$operation, "none")
  expect_identical(out$record$missing, c(low = 0, high = 0))
  expect_identical(out$wavenumbers, lib_wn)
  expect_identical(out$matrix, on(wn))

  ## Overshooting the low end within half a spacing: full, and nothing missing
  ## there
  wn  <- seq(699.5, 599.5, by = -4)
  out <- search_axis(lib_wn, on(wn), wn)

  expect_identical(out$record$mode, "full")
  expect_identical(out$record$operation, "resampled")
  expect_identical(out$record$missing, c(low = 0, high = 0.5))

  ## Reaching past the high end: nothing missing there, not a negative
  ## shortfall
  wn  <- seq(704, 600, by = -2)
  out <- search_axis(lib_wn, on(wn), wn)

  expect_identical(out$record$mode, "full")
  expect_identical(out$record$missing, c(low = 0, high = 0))

  ## 30 cm-1 short at the high end: the overlap, the library's columns inside
  ## the targets' range only. The verb warns from the record; the helper
  ## raises nothing.
  wn <- seq(670, 600, by = -2)

  expect_no_warning(out <- search_axis(lib_wn, on(wn), wn))
  expect_identical(out$record$mode, "overlap")
  expect_identical(out$record$missing, c(low = 0, high = 30))
  expect_identical(out$wavenumbers, seq(670, 600, by = -2))
  expect_identical(out$pool_cols, which(lib_wn <= 670))

  ## A coarse batch short by exactly one of its own 16 cm-1 steps is not
  ## within it, so it is the overlap too (under one step is full)
  wn <- seq(696, 616, by = -16)

  expect_no_warning(out <- search_axis(lib_wn, on(wn), wn))
  expect_identical(out$record$mode, "overlap")
  expect_identical(out$record$missing, c(low = 16, high = 4))

  ## Short by more than the 50 cm-1 that used to be refused: still the
  ## overlap, with nothing raised
  wn <- seq(700, 680, by = -2)

  expect_no_warning(out <- search_axis(lib_wn, on(wn), wn))
  expect_identical(out$record$mode, "overlap")
  expect_identical(out$record$missing, c(low = 80, high = 0))
  expect_identical(out$wavenumbers, seq(700, 680, by = -2))

})
