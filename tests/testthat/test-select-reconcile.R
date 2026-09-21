# tests/testthat/test-select-reconcile.R
# Tests for predictor_matrix() and reconcile_axes(), the first act of
# select_training(): bring the pool onto the targets' wavenumber axis.


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


test_that("reconcile_axes() rejects inputs that are not horizons_data", {

  fx <- make_select_fixture(n_pool = 20)

  expect_error(reconcile_axes(data.frame(a = 1), fx$targets),
               class = "horizons_input_error")
  expect_error(reconcile_axes(fx$pool, data.frame(a = 1)),
               class = "horizons_input_error")

})
