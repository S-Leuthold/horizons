## ---------------------------------------------------------------------------
## Tests for ad.R: applicability domain in model feature space
## ---------------------------------------------------------------------------
##
## The load-bearing test is CALIBRATION: fresh in-distribution samples must be
## flagged OOD at approximately the nominal rate. Thresholding on the training
## rows' own distances (the historical behavior) over-flagged badly in the p>n
## regime; the held-out conformal thresholds (compute_ad_thresholds) fix it.
## ---------------------------------------------------------------------------

## Helper: a named Gaussian feature matrix.
ad_matrix <- function(n, p, mean = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  m <- matrix(stats::rnorm(n * p, mean = mean), nrow = n, ncol = p)
  colnames(m) <- paste0("f", seq_len(p))
  m
}

## ---------------------------------------------------------------------------
## compute_ad_metadata()
## ---------------------------------------------------------------------------

test_that("compute_ad_metadata returns centroid and positive-definite covariance", {

  X  <- ad_matrix(100, 20, mean = 5, seed = 123)
  md <- compute_ad_metadata(X)

  expect_named(md, c("centroid", "cov_matrix"))
  expect_length(md$centroid, 20)
  expect_true(all(abs(md$centroid - 5) < 1))
  expect_equal(dim(md$cov_matrix), c(20, 20))
  expect_true(all(eigen(md$cov_matrix)$values > 0))   # invertible
  expect_identical(names(md$centroid), colnames(X))

})

test_that("compute_ad_metadata handles p > n via shrinkage (with a warning)", {

  X <- ad_matrix(50, 100, seed = 456)               # p > n

  expect_warning(md <- compute_ad_metadata(X), "fewer samples than features")
  expect_equal(dim(md$cov_matrix), c(100, 100))
  expect_true(all(eigen(md$cov_matrix)$values > 0))

})

test_that("compute_ad_metadata validates inputs", {

  no_names <- matrix(stats::rnorm(100), 10, 10)
  expect_error(compute_ad_metadata(no_names), "column names")

  small <- ad_matrix(3, 10, seed = 1)
  expect_error(compute_ad_metadata(small), "at least")

  chars <- matrix(as.character(stats::rnorm(100)), 10, 10)
  colnames(chars) <- paste0("f", 1:10)
  expect_error(compute_ad_metadata(chars), "numeric")

  ## NA is caught, not silently propagated to an all-NA centroid.
  na_mat <- ad_matrix(20, 10, seed = 2); na_mat[1, 1] <- NA
  expect_error(compute_ad_metadata(na_mat), "NA")

})

## ---------------------------------------------------------------------------
## calculate_ad_distance()
## ---------------------------------------------------------------------------

test_that("calculate_ad_distance returns non-negative squared distances", {

  X  <- ad_matrix(100, 10, seed = 789)
  md <- compute_ad_metadata(X)
  Xn <- ad_matrix(20, 10, seed = 790)

  d <- calculate_ad_distance(Xn, md)
  expect_type(d, "double")
  expect_length(d, 20)
  expect_true(all(d >= 0))

})

test_that("calculate_ad_distance validates feature alignment", {

  md <- compute_ad_metadata(ad_matrix(100, 10, seed = 11))

  wrong_names <- ad_matrix(5, 10, seed = 12)
  colnames(wrong_names) <- paste0("z", 1:10)
  expect_error(calculate_ad_distance(wrong_names, md), "column names")

  wrong_count <- ad_matrix(5, 8, seed = 13)
  expect_error(calculate_ad_distance(wrong_count, md), "features must match")

})

## ---------------------------------------------------------------------------
## assign_ad_bin()
## ---------------------------------------------------------------------------

test_that("assign_ad_bin categorizes distances into Q1-Q4/OOD", {

  distances  <- c(5, 15, 25, 35, 50)
  thresholds <- c(10, 20, 30, 45)

  bins <- assign_ad_bin(distances, thresholds)
  expect_s3_class(bins, "factor")
  expect_equal(as.character(bins), c("Q1", "Q2", "Q3", "Q4", "OOD"))
  expect_equal(levels(bins), c("Q1", "Q2", "Q3", "Q4", "OOD"))

})

test_that("assign_ad_bin validates inputs", {

  thr <- c(10, 20, 30, 40)
  expect_error(assign_ad_bin(c("a", "b"), thr), "numeric")
  expect_error(assign_ad_bin(c(-5, 10), thr), "non-negative")
  expect_error(assign_ad_bin(c(5, 15), c(10, 20)), "length 4")

})

## ---------------------------------------------------------------------------
## compute_ad_thresholds() — the D6 rewrite
## ---------------------------------------------------------------------------

test_that("compute_ad_thresholds returns 4 increasing thresholds on held-out data", {

  md    <- compute_ad_metadata(ad_matrix(200, 10, seed = 21))
  calib <- ad_matrix(200, 10, seed = 22)

  thr <- compute_ad_thresholds(calib, md, level = 0.99)
  expect_length(thr, 4)
  expect_true(all(diff(thr) > 0))       # Q1 < Q2 < Q3 < OOD

})

## ---------------------------------------------------------------------------
## CALIBRATION — the acceptance criterion for the whole port
## ---------------------------------------------------------------------------

test_that("held-out thresholds flag in-distribution samples at ~nominal rate", {

  ## Build AD (metadata on train, thresholds on a DISJOINT calib set), then
  ## measure the OOD-flag rate on fresh samples from the IDENTICAL distribution.
  ## Target = 1 - level = 1%. The historical train-quantile thresholds hit
  ## 3-14% here (worse as p grows); held-out + conformal is near nominal.
  ## Averaged over reps to keep the assertion band tight but non-flaky.

  measure_rate <- function(n, p, n_test = 5000, reps = 15, level = 0.99) {
    rates <- numeric(reps)
    for (r in seq_len(reps)) {
      Xtr   <- ad_matrix(n, p, seed = 5000 + r)
      Xcal  <- ad_matrix(n, p, seed = 6000 + r)
      md    <- suppressWarnings(compute_ad_metadata(Xtr))
      thr   <- compute_ad_thresholds(Xcal, md, level = level)
      Xte   <- ad_matrix(n_test, p, seed = 7000 + r)
      bins  <- assign_ad_bin(calculate_ad_distance(Xte, md), thr)
      rates[r] <- mean(bins == "OOD")
    }
    mean(rates)
  }

  ## Well-conditioned regime: should be very close to 1%.
  expect_lt(measure_rate(100, 10), 0.03)

  ## p >= n regime (the spectroscopy case the old code broke on): still bounded.
  ## The old train-quantile thresholding produced ~14% here.
  expect_lt(measure_rate(50, 100), 0.05)

})

test_that("far-out-of-distribution samples are flagged OOD", {

  md   <- compute_ad_metadata(ad_matrix(200, 10, seed = 31))
  thr  <- compute_ad_thresholds(ad_matrix(200, 10, seed = 32), md)
  Xood <- ad_matrix(300, 10, mean = 8, seed = 33)         # far from centroid

  bins <- assign_ad_bin(calculate_ad_distance(Xood, md), thr)
  expect_gt(mean(bins == "OOD"), 0.95)

})
