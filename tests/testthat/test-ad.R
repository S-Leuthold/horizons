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


## ---------------------------------------------------------------------------
## predict_ad() — one bad spectrum degrades one row, not the batch
## ---------------------------------------------------------------------------
## A spectrum that bakes to all-NA is step_transform_spectra's documented
## failure path. The earlier whole-matrix anyNA() check turned that single row
## into a NULL AD result for the whole call, which also silently disabled
## abstention. A bake that ABORTS is a different thing — a bug or a schema
## problem — and is warned about rather than degraded.
## ---------------------------------------------------------------------------

## A fitted workflow whose recipe carries the real transform step, so the NA
## path under test is the production one rather than a hand-built matrix.
ad_fitted_workflow <- function(n = 60, n_wn = 12, seed = 41) {

  set.seed(seed)

  wn  <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  mat <- matrix(stats::rnorm(n * n_wn), nrow = n)
  colnames(mat) <- wn

  df   <- tibble::as_tibble(mat)
  df$y <- 5 + rowMeans(mat[, 1:3]) + stats::rnorm(n, sd = 0.2)

  rec <- recipes::recipe(y ~ ., data = df) |>
    step_transform_spectra(dplyr::all_of(wn), preprocessing = "raw")

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(parsnip::linear_reg()) |>
    parsnip::fit(data = df)

  list(workflow = wf, data = df, wn = wn)

}

ad_new_spectra <- function(wn, n = 5, shift = 0, seed = 42) {

  set.seed(seed)
  m <- matrix(stats::rnorm(n * length(wn)) + shift, nrow = n)
  colnames(m) <- wn
  df <- tibble::as_tibble(m)
  df$sample_id <- paste0("NEW", seq_len(n))
  df

}

test_that("predict_ad returns per-row NA for one bad spectrum and scores the rest", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  skip_if(is.null(bundle), "AD bundle could not be fit on the tiny fixture")

  new_df           <- ad_new_spectra(fx$wn, n = 5)
  new_df[3, fx$wn] <- NA_real_                   # one malformed spectrum

  ad <- suppressWarnings(
    predict_ad(fx$workflow, bundle, new_df)
  )

  expect_s3_class(ad, "tbl_df")
  expect_equal(nrow(ad), 5L)

  expect_true(is.na(ad$.ad_distance[3]))
  expect_true(is.na(ad$.ad_flag[3]))

  ## The other four are scored normally — the whole batch is not lost.
  expect_equal(sum(!is.na(ad$.ad_distance)), 4L)
  expect_true(all(ad$.ad_distance[-3] >= 0))
  expect_true(all(!is.na(ad$.ad_flag[-3])))

})

test_that("predict_ad warns about the degraded rows rather than dropping AD", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  skip_if(is.null(bundle), "AD bundle could not be fit on the tiny fixture")

  new_df           <- ad_new_spectra(fx$wn, n = 5)
  new_df[3, fx$wn] <- NA_real_

  warns <- testthat::capture_warnings(ad <- predict_ad(fx$workflow, bundle, new_df))

  expect_true(any(grepl("Applicability domain is NA for 1 of 5", warns)))
  expect_false(is.null(ad))

})

test_that("predict_ad warns and returns NULL when the bake aborts", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  skip_if(is.null(bundle), "AD bundle could not be fit on the tiny fixture")

  new_df <- ad_new_spectra(fx$wn, n = 5)

  ## Not a workflow: extract_recipe() aborts, which is the bug / schema case.
  expect_warning(
    ad <- predict_ad(list(), bundle, new_df),
    "baking"
  )

  expect_null(ad)

})

test_that("predict_ad returns NULL, with a warning, when every spectrum is bad", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  skip_if(is.null(bundle), "AD bundle could not be fit on the tiny fixture")

  new_df         <- ad_new_spectra(fx$wn, n = 5)
  new_df[, fx$wn] <- NA_real_

  warns <- testthat::capture_warnings(ad <- predict_ad(fx$workflow, bundle, new_df))

  expect_true(any(grepl("unavailable for all 5", warns)))
  expect_null(ad)

})

test_that("predict_ad warns with the cause and returns NULL when the distance fails", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  skip_if(is.null(bundle), "AD bundle could not be fit on the tiny fixture")

  ## The bake succeeds, but the bundle's centroid no longer matches the
  ## recipe's features, so calculate_ad_distance() aborts. This used to
  ## return NULL with no signal, and the AD columns vanished.
  bundle$centroid <- bundle$centroid[-1]

  new_df <- ad_new_spectra(fx$wn, n = 5)

  w <- expect_warning(
    ad <- predict_ad(fx$workflow, bundle, new_df),
    class = "horizons_ad_warning"
  )

  expect_match(conditionMessage(w), "Number of features must match", fixed = TRUE)
  expect_null(ad)

})
