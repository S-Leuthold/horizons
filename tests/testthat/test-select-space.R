# tests/testthat/test-select-space.R
# Tests for the similarity space select_training() measures distances in:
# transform_similarity(), build_similarity_space(), project_similarity().


## =============================================================================
## Reference implementation (the experiment's chain, inline)
## =============================================================================

#' The default chain as 05-coherent-batch.R computed it, without masking
#'
#' Components are chosen by cumulative variance alone, which is what
#' `build_similarity_space(sdev_floor = 0)` does. The default floor trims
#' that set; the tests that use this oracle disable it explicitly.
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


test_that("transform_similarity() validates the filter arguments itself", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  ## Even window: the edge-trim bookkeeping assumes an odd one
  expect_error(transform_similarity(pm$matrix, pm$wavenumbers, window = 10L),
               regexp = "odd", class = "horizons_input_error")

  ## Polynomial order at or above the window
  expect_error(transform_similarity(pm$matrix, pm$wavenumbers, window = 5L, poly = 5L),
               class = "horizons_input_error")

  ## Polynomial order below the derivative asked of it
  expect_error(transform_similarity(pm$matrix, pm$wavenumbers, derivative = 2L, poly = 1L),
               class = "horizons_input_error")

  ## Window wider than the spectra
  expect_error(transform_similarity(pm$matrix[, 1:9, drop = FALSE], pm$wavenumbers[1:9],
                                    window = 11L),
               class = "horizons_input_error")

  ## None of this fires when the derivative is off
  expect_silent(transform_similarity(pm$matrix, pm$wavenumbers, derivative = 0L, window = 10L))

})


test_that("transform_similarity() aborts when the mask leaves too few columns", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  ## Everything masked
  expect_error(transform_similarity(pm$matrix, pm$wavenumbers, mask = rbind(c(0, 5000))),
               regexp = "column", class = "horizons_input_error")

  ## The floor is an argument, so a caller that needs room can ask for it
  expect_error(transform_similarity(pm$matrix, pm$wavenumbers, min_cols = 10000L),
               class = "horizons_input_error")

})


## =============================================================================
## build_similarity_space() — PCA
## =============================================================================

test_that("build_similarity_space() default reproduces the experiment's PCA space", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp  <- build_similarity_space(pm$matrix, pm$wavenumbers, sdev_floor = 0)
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

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers, sdev_floor = 0)

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

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 5L, sdev_floor = 0)

  expect_identical(sp$ncomp, 5L)
  expect_identical(ncol(sp$scores), 5L)
  expect_length(sp$sdev, 5L)

})


test_that("build_similarity_space(ncomp = proportion) retains fewer at a lower proportion", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  hi <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 0.99, sdev_floor = 0)
  lo <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 0.50, sdev_floor = 0)

  expect_lt(lo$ncomp, hi$ncomp)
  expect_gte(lo$variance_retained, 0.50)

})


test_that("build_similarity_space() caps proportional ncomp at max_comp", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  sp <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 0.999999, max_comp = 3L,
                               sdev_floor = 0)

  expect_identical(sp$ncomp, 3L)

})


test_that("each spectral lever changes the space", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  base <- build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, sdev_floor = 0)

  variants <- list(
    snv  = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, sdev_floor = 0, snv = FALSE),
    der  = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, sdev_floor = 0, derivative = 0L),
    win  = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, sdev_floor = 0, window = 21L),
    mask = build_similarity_space(pm$matrix, pm$wavenumbers, ncomp = 4L, sdev_floor = 0, mask = rbind(c(3100, 3700)))
  )

  for (nm in names(variants)) {

    expect_false(isTRUE(all.equal(abs(unname(variants[[nm]]$scores)), abs(unname(base$scores)))),
                 label = paste("lever", nm, "changes the scores"))

  }

})


test_that("build_similarity_space() accepts a coarse but gap-free pool", {

  ## The regression: the column floor used to be twice the component cap,
  ## and the cap under a variance proportion is 100 whether or not the rule
  ## goes anywhere near it. A 24 cm-1 axis is 142 columns, 132 after the
  ## filter's edge trim, and the verb refused it for a cap the user never set.

  set.seed(4)
  wn <- seq(4000, 600, by = -24)
  M  <- gaussian_family(30, wn, centres = c(3400, 2920, 1630, 1030))
  dimnames(M) <- list(sprintf("P%02d", seq_len(nrow(M))), NULL)

  expect_length(wn, 142L)

  sp <- build_similarity_space(M, wn)

  expect_s3_class(sp, "horizons_similarity_space")
  expect_length(sp$wavenumbers, 132L)
  expect_gte(sp$ncomp, 1L)

  ## An integer ncomp still has to fit in the columns it is given
  expect_error(build_similarity_space(M, wn, ncomp = 200L),
               regexp = "column", class = "horizons_input_error")

})


test_that("build_similarity_space() aborts when a mask takes almost the whole spectrum", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  ## The message names the mask, because the mask is the fault
  expect_error(build_similarity_space(pm$matrix, pm$wavenumbers, mask = rbind(c(0, 5000))),
               regexp = "masked range", class = "horizons_input_error")

  ## And it does not name a component cap the caller never set
  err <- tryCatch(build_similarity_space(pm$matrix, pm$wavenumbers, mask = rbind(c(0, 5000))),
                  horizons_input_error = function(e) conditionMessage(e))
  expect_false(grepl("cap", err))

})


## =============================================================================
## build_similarity_space() — the noise floor
## =============================================================================

#' A matrix with singular values we chose, so the retained count is known
#'
#' Column-centred by construction (the left singular vectors of a centred
#' matrix are themselves mean-zero), so `prcomp()`'s sdev is `d` up to the
#' shared factor `sqrt(n - 1)` and `sdev / sdev[1]` is exactly `d / d[1]`.
#' @noRd
known_decay_matrix <- function(d, n = 60L, p = 50L, seed = 42L) {

  set.seed(seed)

  X <- scale(matrix(stats::rnorm(n * p), nrow = n), center = TRUE, scale = FALSE)
  s <- svd(X)

  M <- s$u %*% diag(d) %*% t(s$v)
  rownames(M) <- sprintf("S%03d", seq_len(n))
  M

}


test_that("sdev_floor drops the components below the floor on a known decay", {

  ## sdev ratios: 1, 0.5, 0.3, 0.2, 0.15, 0.09, 0.05, 0.02, then a flat tail
  d <- c(100, 50, 30, 20, 15, 9, 5, 2, rep(1, 42))
  M <- known_decay_matrix(d)
  wn <- seq(4000, by = -4, length.out = ncol(M))

  none <- build_similarity_space(M, wn, snv = FALSE, derivative = 0L,
                                 ncomp = 20L, sdev_floor = 0)
  ten  <- build_similarity_space(M, wn, snv = FALSE, derivative = 0L,
                                 ncomp = 20L, sdev_floor = 0.10)
  five <- build_similarity_space(M, wn, snv = FALSE, derivative = 0L,
                                 ncomp = 20L, sdev_floor = 0.05)

  expect_identical(none$ncomp, 20L)
  expect_identical(ten$ncomp,   5L)   # 0.09 is below the floor
  expect_identical(five$ncomp,  7L)   # 0.05 is not

  ## The variance rule's own count survives alongside the floored one
  expect_identical(ten$ncomp_variance, 20L)
  expect_identical(none$ncomp_variance, 20L)

  ## The decay is recorded as the ratio to the first component, over the
  ## variance rule's set rather than the floored one, so it shows what the
  ## floor cut and by how far.
  expect_equal(ten$sdev_ratio, (d / d[1])[seq_len(20)], tolerance = 1e-10)
  expect_equal(none$sdev_ratio, (d / d[1])[seq_len(20)], tolerance = 1e-10)
  expect_length(ten$sdev_ratio, ten$ncomp_variance)
  expect_true(all(ten$sdev_ratio[seq_len(ten$ncomp)] >= 0.10))
  expect_true(all(ten$sdev_ratio[-seq_len(ten$ncomp)] < 0.10))

  ## Scores, sdev and loadings are all cut to the floored count
  expect_identical(ncol(ten$scores), 5L)
  expect_identical(ncol(ten$rotation), 5L)
  expect_length(ten$sdev, 5L)

  ## And the floor is on the object, not just in the settings
  expect_identical(ten$sdev_floor, 0.10)
  expect_identical(ten$settings$sdev_floor, 0.10)

})


test_that("sdev_floor never empties the space and never adds components", {

  d <- c(100, rep(0.01, 49))
  M <- known_decay_matrix(d)
  wn <- seq(4000, by = -4, length.out = ncol(M))

  sp <- build_similarity_space(M, wn, snv = FALSE, derivative = 0L,
                               ncomp = 20L, sdev_floor = 0.9)

  expect_identical(sp$ncomp, 1L)
  expect_lte(sp$ncomp, sp$ncomp_variance)

})


test_that("the default floor trims the fixture's variance-chosen set", {

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)

  none <- build_similarity_space(pm$matrix, pm$wavenumbers, sdev_floor = 0)
  dflt <- build_similarity_space(pm$matrix, pm$wavenumbers)

  ## Today's behaviour, exactly, at sdev_floor = 0
  ref <- reference_space(pm$matrix, pm$wavenumbers)
  expect_identical(none$ncomp, ref$ncomp)
  expect_identical(none$ncomp_variance, ref$ncomp)
  expect_identical(none$sdev_floor, 0)

  ## And the default floor of 0.10 as a stated expectation
  expect_identical(dflt$settings$sdev_floor, 0.10)
  expect_identical(dflt$ncomp_variance, none$ncomp)
  expect_identical(dflt$ncomp, 11L)
  expect_lt(dflt$ncomp, dflt$ncomp_variance)
  expect_length(dflt$sdev_ratio, dflt$ncomp_variance)
  expect_true(all(dflt$sdev_ratio[seq_len(dflt$ncomp)] >= 0.10))
  expect_true(all(dflt$sdev_ratio[-seq_len(dflt$ncomp)] < 0.10))
  expect_lt(dflt$variance_retained, none$variance_retained)

  ## The floor only truncates: the components it keeps are the same ones
  expect_equal(dflt$sdev, none$sdev[seq_len(dflt$ncomp)], tolerance = 1e-10)
  expect_equal(abs(unname(dflt$scores)),
               abs(unname(none$scores[, seq_len(dflt$ncomp), drop = FALSE])),
               tolerance = 1e-10)

})


test_that("build_similarity_space() rejects a nonsense sdev_floor", {

  fx <- make_select_fixture(n_pool = 30)
  pm <- predictor_matrix(fx$pool)

  expect_error(build_similarity_space(pm$matrix, pm$wavenumbers, sdev_floor = 1),
               class = "horizons_input_error")
  expect_error(build_similarity_space(pm$matrix, pm$wavenumbers, sdev_floor = -0.1),
               class = "horizons_input_error")
  expect_error(build_similarity_space(pm$matrix, pm$wavenumbers, sdev_floor = c(0.1, 0.2)),
               class = "horizons_input_error")

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


test_that("the PLS branch records the decay but is not floored by it", {

  skip_if_not_installed("mixOmics")

  fx <- make_select_fixture(n_pool = 60)
  pm <- predictor_matrix(fx$pool)
  y  <- fx$pool$data$analysis$clay

  none <- build_similarity_space(pm$matrix, pm$wavenumbers, space = "pls", ncomp = 6L,
                                 y = y, sdev_floor = 0)
  hard <- build_similarity_space(pm$matrix, pm$wavenumbers, space = "pls", ncomp = 6L,
                                 y = y, sdev_floor = 0.9)

  expect_identical(none$ncomp, 6L)
  expect_identical(hard$ncomp, 6L)
  expect_identical(ncol(hard$scores), 6L)
  expect_equal(hard$scores, none$scores, tolerance = 1e-10)

  ## The decay is there to be read, and the applied floor is zero either way
  expect_length(hard$sdev_ratio, 6L)
  expect_equal(unname(hard$sdev_ratio[1]), 1)
  expect_identical(hard$sdev_floor, 0)
  expect_identical(hard$settings$sdev_floor, 0.9)
  expect_null(hard$ncomp_variance)

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
