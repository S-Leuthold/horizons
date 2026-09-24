# tests/testthat/test-select-reconcile.R
# Tests for predictor_matrix() and reconcile_axes(), the first act of
# select_training(): bring the pool onto the targets' wavenumber axis.


## =============================================================================
## A horizons_data on an arbitrary wavenumber axis
## =============================================================================

#' Build a minimal horizons_data on the wavenumbers given
#'
#' @description
#' The fixture's grids are regular and contiguous by construction, which is
#' exactly what the gap and clamp checks need to vary. This builds a small
#' object on any axis, spectra included, with no responses.
#'
#' @param wn [Numeric.] Wavenumbers, decreasing.
#' @param n [Integer.] Rows. Default: `6`.
#' @param prefix [Character.] Sample id prefix. Default: `"A"`.
#'
#' @return [horizons_data.]
#' @noRd
make_axis_object <- function(wn, n = 6L, prefix = "A", seed = 7L) {

  set.seed(seed)

  m <- gaussian_family(n, wn, centres = c(3400, 2920, 1630, 1030))
  colnames(m) <- paste0("wn_", wn)

  tbl <- dplyr::bind_cols(
    tibble::tibble(sample_id = sprintf("%s%03d", prefix, seq_len(n))),
    tibble::as_tibble(m)
  )

  utils::capture.output(obj <- spectra(tbl))
  obj

}


## =============================================================================
## predictor_matrix()
## =============================================================================

test_that("predictor_matrix() returns the spectra with decreasing wavenumbers", {

  fx  <- make_select_fixture(n_pool = 20)
  out <- predictor_matrix(fx$pool)

  expect_type(out, "list")
  expect_identical(dim(out$matrix), c(20L, length(fx$pool_wn)))
  expect_identical(out$wavenumbers, fx$pool_wn)
  expect_identical(rownames(out$matrix), fx$pool$data$analysis$sample_id)
  expect_true(all(diff(out$wavenumbers) < 0))

})


## =============================================================================
## reconcile_axes() — the targets' grid is the reference
## =============================================================================

test_that("reconcile_axes() puts the pool on the targets' wavenumbers", {

  ## Arrange: pool at 4 cm-1, targets at 8 cm-1
  fx <- make_select_fixture(n_pool = 20)

  ## Act
  out <- reconcile_axes(fx$pool, fx$targets)

  ## Assert
  expect_identical(out$wavenumbers, fx$target_wn)
  expect_identical(dim(out$matrix), c(20L, length(fx$target_wn)))
  expect_identical(rownames(out$matrix), fx$pool$data$analysis$sample_id)
  expect_identical(out$record$operation, "resampled")
  expect_identical(out$record$target_grid$n, length(fx$target_wn))
  expect_identical(out$record$pool_grid$n,   length(fx$pool_wn))
  expect_equal(out$record$target_grid$resolution, 8)
  expect_equal(out$record$pool_grid$resolution,   4)
  expect_length(out$record$warnings, 0L)

})


test_that("reconcile_axes() reproduces the twin exactly on a nested grid", {

  ## The target grid is every other pool column, so the spline passes
  ## through the pool's own values at those knots.
  fx  <- make_select_fixture(n_pool = 20)
  out <- reconcile_axes(fx$pool, fx$targets)

  twin_target <- predictor_matrix(fx$targets)$matrix[fx$twin_id, ]
  twin_pool   <- out$matrix[fx$twin_pool_id, ]

  expect_equal(unname(twin_pool), unname(twin_target), tolerance = 1e-12)

})


test_that("reconcile_axes() passes an identical grid through untouched", {

  fx <- make_select_fixture(n_pool = 20)

  ## Targets on the pool's own grid: use pool rows as targets
  same <- subset_rows(fx$pool, c("P001", "P002"), record = FALSE)

  out <- reconcile_axes(fx$pool, same)

  expect_identical(out$record$operation, "none")
  expect_identical(out$wavenumbers, fx$pool_wn)
  expect_identical(unname(out$matrix), unname(predictor_matrix(fx$pool)$matrix))

})


test_that("reconcile_axes() stops when the pool does not cover the targets' high end", {

  fx <- make_select_fixture(n_pool = 20, target_range = c(4096, 600))

  expect_error(reconcile_axes(fx$pool, fx$targets),
               regexp = "high end.*4000", class = "horizons_input_error")

})


test_that("reconcile_axes() stops when the pool does not cover the targets' low end", {

  fx <- make_select_fixture(n_pool = 20, target_range = c(4000, 504))

  expect_error(reconcile_axes(fx$pool, fx$targets),
               regexp = "low end.*600", class = "horizons_input_error")

})


test_that("reconcile_axes() warns when the targets are finer than the pool", {

  ## Arrange: swap roles so the 4 cm-1 pool becomes the targets of an
  ## 8 cm-1 pool. Give the coarse side responses so it is a valid pool.
  fx <- make_select_fixture(n_pool = 20)

  coarse <- fx$targets
  lab    <- tibble::tibble(sample_id = coarse$data$analysis$sample_id,
                           clay = seq_len(coarse$data$n_rows))
  utils::capture.output(coarse <- add_response(coarse, lab, variable = "clay"))

  ## Act / Assert
  expect_warning(out <- reconcile_axes(coarse, fx$pool),
                 regexp = "finer", class = "horizons_select_warning")

  expect_identical(out$wavenumbers, fx$pool_wn)
  expect_identical(out$record$operation, "resampled")
  expect_length(out$record$warnings, 1L)
  expect_match(out$record$warnings, "finer")

})


## =============================================================================
## Contiguity — a deleted band is a refusal, not a spline fill
## =============================================================================

test_that("grid_summary() reports a contiguous grid as contiguous", {

  fx  <- make_select_fixture(n_pool = 20)
  out <- reconcile_axes(fx$pool, fx$targets)

  expect_true(out$record$pool_grid$contiguous)
  expect_true(out$record$target_grid$contiguous)
  expect_null(out$record$pool_grid$gaps)
  expect_equal(out$record$pool_grid$max_spacing, 4)
  expect_null(out$record$clamp)

})


test_that("reconcile_axes() stops on a gapped pool axis and names the gap", {

  ## A 140 cm-1 hole, the narrower of the two ranges standardize(remove_water)
  ## deletes. It clears the endpoint coverage test and would be spline-filled.
  fx      <- make_select_fixture(n_pool = 20)
  gapped  <- seq(4000, 600, by = -4)
  gapped  <- gapped[!(gapped >= 1580 & gapped <= 1720)]

  pool <- make_axis_object(gapped, prefix = "G")

  expect_error(reconcile_axes(pool, fx$targets),
               regexp = "pool", class = "horizons_input_error")
  expect_error(reconcile_axes(pool, fx$targets),
               regexp = "1576|1724", class = "horizons_input_error")
  expect_error(reconcile_axes(pool, fx$targets),
               regexp = "mask", class = "horizons_input_error")

})


test_that("reconcile_axes() stops on a gapped target axis too", {

  fx     <- make_select_fixture(n_pool = 20)
  gapped <- seq(4000, 600, by = -8)
  gapped <- gapped[!(gapped >= 1580 & gapped <= 1720)]

  targets <- make_axis_object(gapped, prefix = "H")

  expect_error(reconcile_axes(fx$pool, targets),
               regexp = "targets", class = "horizons_input_error")

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
## Coverage tolerance — half a pool spacing is clamped, not refused
## =============================================================================

test_that("reconcile_axes() clamps a target grid overshooting by less than half a spacing", {

  ## Targets on an axis standardize() did not put on the canonical grid, as
  ## with resample = NULL, overshooting the pool's end by a fraction.
  ## The pool is at 4 cm-1, so the tolerance is 2 cm-1 and a 1 cm-1
  ## overshoot at the high end is inside it.
  fx      <- make_select_fixture(n_pool = 20)
  over_wn <- seq(4001, 601, by = -8)
  targets <- make_axis_object(over_wn, prefix = "O")

  expect_warning(out <- reconcile_axes(fx$pool, targets),
                 regexp = "overshoot", class = "horizons_select_warning")

  ## The axis returned is the targets' own, overshooting column included
  expect_identical(out$wavenumbers, over_wn)
  expect_identical(ncol(out$matrix), length(over_wn))
  expect_true(all(is.finite(out$matrix)))

  ## And the clamp is on the record, with the tolerance it was judged against
  expect_equal(out$record$clamp$high, 1)
  expect_equal(out$record$clamp$low, 0)
  expect_equal(out$record$clamp$tolerance, 2)
  expect_length(out$record$warnings, 1L)
  expect_match(out$record$warnings, "overshoot")

})


test_that("reconcile_axes() still stops beyond the tolerance, stating the overshoot", {

  fx      <- make_select_fixture(n_pool = 20)
  targets <- make_axis_object(seq(4005, 605, by = -8), prefix = "P")

  expect_error(reconcile_axes(fx$pool, targets),
               regexp = "high end", class = "horizons_input_error")
  expect_error(reconcile_axes(fx$pool, targets),
               regexp = "5 cm-1", class = "horizons_input_error")

})


test_that("reconcile_axes() clamps the low end the same way", {

  fx      <- make_select_fixture(n_pool = 20)
  under   <- seq(3999, 599, by = -8)
  targets <- make_axis_object(under, prefix = "Q")

  expect_warning(out <- reconcile_axes(fx$pool, targets),
                 class = "horizons_select_warning")

  expect_equal(out$record$clamp$low, 1)
  expect_equal(out$record$clamp$high, 0)
  expect_identical(out$wavenumbers, under)

})


test_that("reconcile_axes() rejects inputs that are not horizons_data", {

  fx <- make_select_fixture(n_pool = 20)

  expect_error(reconcile_axes(data.frame(a = 1), fx$targets),
               class = "horizons_input_error")
  expect_error(reconcile_axes(fx$pool, data.frame(a = 1)),
               class = "horizons_input_error")

})
