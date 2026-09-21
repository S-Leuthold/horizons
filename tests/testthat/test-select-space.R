# tests/testthat/test-select-space.R
# Tests for the similarity space select_training() measures distances in:
# transform_similarity(), build_similarity_space(), project_similarity().


## =============================================================================
## Reference implementation (the experiment's chain, inline)
## =============================================================================

#' The default chain as 05-coherent-batch.R computed it, without masking
#' @noRd
reference_space <- function(M, wn, threshold = 0.99, cap = 100L) {

  m  <- prospectr::standardNormalVariate(M)
  m  <- prospectr::savitzkyGolay(m, m = 1, p = 2, w = 11)
  h  <- 5L
  wn <- wn[(h + 1):(length(wn) - h)]

  pca <- stats::prcomp(m, center = TRUE, scale. = FALSE)
  cum <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k   <- min(which(cum >= threshold)[1], cap)

  list(
    wn     = wn,
    ncomp  = k,
    sdev   = pca$sdev[seq_len(k)],
    scores = pca$x[, seq_len(k), drop = FALSE],
    pca    = pca
  )

}


## =============================================================================
## transform_similarity()
## =============================================================================

test_that("transform_similarity() default chain matches SNV then SG(1, 2, 11)", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  out <- transform_similarity(pm$matrix, pm$wavenumbers)
  ref <- reference_space(pm$matrix, pm$wavenumbers)

  expect_identical(out$wavenumbers, ref$wn)
  expect_equal(unname(out$matrix),
               unname(prospectr::savitzkyGolay(prospectr::standardNormalVariate(pm$matrix), 1, 2, 11)),
               tolerance = 1e-12)
  expect_identical(rownames(out$matrix), rownames(pm$matrix))

})


test_that("transform_similarity(derivative = 0) skips the filter and keeps the width", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  out <- transform_similarity(pm$matrix, pm$wavenumbers, derivative = 0L)

  expect_identical(out$wavenumbers, pm$wavenumbers)
  expect_equal(unname(out$matrix), unname(prospectr::standardNormalVariate(pm$matrix)), tolerance = 1e-12)

})


test_that("transform_similarity(snv = FALSE, derivative = 0) is the identity", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  out <- transform_similarity(pm$matrix, pm$wavenumbers, snv = FALSE, derivative = 0L)

  expect_equal(unname(out$matrix), unname(pm$matrix))

})


test_that("transform_similarity() honours window, poly and the second derivative", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  out <- transform_similarity(pm$matrix, pm$wavenumbers, derivative = 2L, window = 21L, poly = 3L)

  expect_identical(length(out$wavenumbers), length(pm$wavenumbers) - 20L)
  expect_equal(unname(out$matrix),
               unname(prospectr::savitzkyGolay(prospectr::standardNormalVariate(pm$matrix), 2, 3, 21)),
               tolerance = 1e-12)

})


test_that("transform_similarity() masks ranges after the derivative", {

  fx   <- make_select_fixture(n_pool = 30)
  pm   <- predictor_matrix(fx$pool)
  mask <- rbind(c(1580, 1720), c(3100, 3700))

  out <- transform_similarity(pm$matrix, pm$wavenumbers, mask = mask)
  ref <- reference_space(pm$matrix, pm$wavenumbers)

  in_mask <- (ref$wn >= 1580 & ref$wn <= 1720) | (ref$wn >= 3100 & ref$wn <= 3700)

  expect_identical(out$wavenumbers, ref$wn[!in_mask])
  expect_identical(ncol(out$matrix), sum(!in_mask))
  expect_false(any(out$wavenumbers >= 3100 & out$wavenumbers <= 3700))

})


test_that("transform_similarity() aborts on a row that goes non-finite, naming it", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)
  pm$matrix["P004", 3] <- Inf   # propagates through SNV and the filter

  expect_error(transform_similarity(pm$matrix, pm$wavenumbers),
               regexp = "P004", class = "horizons_input_error")

})


test_that("transform_similarity() rejects a bad mask", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  expect_error(transform_similarity(pm$matrix, pm$wavenumbers, mask = c(1580, 1720)),
               class = "horizons_input_error")
  expect_error(transform_similarity(pm$matrix, pm$wavenumbers, mask = rbind(c(1720, 1580))),
               class = "horizons_input_error")

})


## =============================================================================
## build_similarity_space() — PCA
## =============================================================================

test_that("build_similarity_space() default reproduces the experiment's PCA space", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp  <- build_similarity_space(pm$matrix, pm$wavenumbers)
  ref <- reference_space(pm$matrix, pm$wavenumbers)

  expect_s3_class(sp, "horizons_similarity_space")
  expect_identical(sp$ncomp, ref$ncomp)
  expect_identical(sp$wavenumbers, ref$wn)
  expect_equal(sp$sdev, ref$sdev, tolerance = 1e-10)
  expect_equal(abs(unname(sp$scores)), abs(unname(ref$scores)), tolerance = 1e-10)
  expect_identical(rownames(sp$scores), rownames(pm$matrix))

})


test_that("build_similarity_space() records its settings and the variance retained", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers)

  expect_identical(sp$settings$space, "pca")
  expect_identical(sp$settings$snv, TRUE)
  expect_identical(sp$settings$derivative, 1L)
  expect_identical(sp$settings$window, 11L)
  expect_identical(sp$settings$poly, 2L)
  expect_null(sp$settings$mask)
  expect_equal(sp$settings$ncomp, 0.99)
  expect_gte(sp$variance_retained, 0.99)
  expect_identical(sp$input_wavenumbers, pm$wavenumbers)

})


test_that("build_similarity_space(ncomp = integer) retains exactly that many", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 5L)

  expect_identical(sp$ncomp, 5L)
  expect_identical(ncol(sp$scores), 5L)
  expect_length(sp$sdev, 5L)

})


test_that("build_similarity_space(ncomp = proportion) retains fewer at a lower proportion", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  hi <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 0.99)
  lo <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 0.50)

  expect_lt(lo$ncomp, hi$ncomp)
  expect_gte(lo$variance_retained, 0.50)

})


test_that("build_similarity_space() caps proportional ncomp at max_comp", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 0.999999, max_comp = 3L)

  expect_identical(sp$ncomp, 3L)

})


test_that("each spectral lever changes the space", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  base <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L)

  variants <- list(
    snv  = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, snv = FALSE),
    der  = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, derivative = 0L),
    win  = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, window = 21L),
    mask = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, mask = rbind(c(3100, 3700)))
  )

  for (nm in names(variants)) {

    expect_false(isTRUE(all.equal(abs(unname(variants[[nm]]$scores)), abs(unname(base$scores)))),
                 label = paste("lever", nm, "changes the scores"))

  }

})


## =============================================================================
## build_similarity_space() — PLS
## =============================================================================

test_that("build_similarity_space(space = 'pls') needs y and an integer ncomp", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  expect_error(build_similarity_space(pm$matrix, pm$wavenumbers, space = "pls", ncomp = 5L),
               class = "horizons_input_error")
  expect_error(build_similarity_space(pm$matrix, pm$wavenumbers, space = "pls", ncomp = 0.99,
                                      y = fx$pool$data$analysis$clay),
               class = "horizons_input_error")

})


test_that("build_similarity_space(space = 'pls') fits on measured rows and scores every row", {

  skip_if_not_installed("mixOmics")

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)
  y  <- fx$pool$data$analysis$oc   # half NA

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers, space = "pls", ncomp = 4L, y = y)

  expect_identical(sp$settings$space, "pls")
  expect_identical(sp$ncomp, 4L)
  expect_identical(nrow(sp$scores), nrow(pm$matrix))
  expect_identical(sp$n_fit, sum(!is.na(y)))
  expect_true(all(is.finite(sp$scores)))
  expect_length(sp$sdev, 4L)

  pca <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L)
  expect_false(isTRUE(all.equal(abs(unname(sp$scores)), abs(unname(pca$scores)))))

})


## =============================================================================
## project_similarity()
## =============================================================================

test_that("project_similarity() of the pool onto its own space returns the stored scores", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers)
  pr <- project_similarity(sp, pm$matrix, pm$wavenumbers)

  expect_equal(unname(pr), unname(sp$scores), tolerance = 1e-10)
  expect_identical(rownames(pr), rownames(pm$matrix))

})


test_that("project_similarity() works for the PLS space too", {

  skip_if_not_installed("mixOmics")

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers, space = "pls", ncomp = 3L,
                               y = fx$pool$data$analysis$clay)
  pr <- project_similarity(sp, pm$matrix, pm$wavenumbers)

  expect_equal(unname(pr), unname(sp$scores), tolerance = 1e-10)

})


test_that("project_similarity() places the exact twin on its pool row", {

  fx <- make_select_fixture(n_pool = 60)
  rc <- reconcile_axes(fx$pool, fx$targets)
  tm <- predictor_matrix(fx$targets)

  sp <- build_similarity_space(rc$matrix, rc$wavenumbers)
  st <- project_similarity(sp, tm$matrix, tm$wavenumbers)

  expect_equal(unname(st[fx$twin_id, ]), unname(sp$scores[fx$twin_pool_id, ]), tolerance = 1e-10)

})


test_that("project_similarity() aborts on a wavenumber mismatch", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)
  tm <- predictor_matrix(fx$targets)   # not reconciled: different grid

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers)

  expect_error(project_similarity(sp, tm$matrix, tm$wavenumbers),
               class = "horizons_input_error")

})
