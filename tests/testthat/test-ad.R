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

  expect_named(md, c("form", "centroid", "cov_matrix"))
  expect_identical(md$form, "full")
  expect_length(md$centroid, 20)
  expect_true(all(abs(md$centroid - 5) < 1))
  expect_equal(dim(md$cov_matrix), c(20, 20))
  expect_true(all(eigen(md$cov_matrix)$values > 0))   # invertible
  expect_identical(names(md$centroid), colnames(X))

})

test_that("compute_ad_metadata estimates a positive-definite covariance at p > n without a warning", {

  ## Fewer samples than features is the regime the shrinkage estimator is
  ## built for, not a degraded one. Its estimate is stored in low-rank form
  ## there; the full matrix is rebuilt here to check it.
  X <- ad_matrix(50, 100, seed = 456)               # p > n

  expect_no_warning(md <- compute_ad_metadata(X))
  expect_identical(md$form, "low_rank")

  S    <- diag(md$scale)
  full <- S %*% (md$lambda * diag(100) +
                   (1 - md$lambda) * md$basis %*% diag(md$values) %*% t(md$basis)) %*% S
  expect_equal(full, unclass(corpcor::cov.shrink(X, verbose = FALSE)),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_true(all(eigen(full, symmetric = TRUE)$values > 0))

})

## ---------------------------------------------------------------------------
## The covariance in whichever exact form is smaller
## ---------------------------------------------------------------------------

describe("compute_ad_metadata() - the two forms", {

  ## Correlated features, as baked spectra are: a few latent factors
  wide <- function(n, p, seed, noise = 1) {
    set.seed(seed)
    m <- matrix(stats::rnorm(n * 3), n) %*% matrix(stats::rnorm(3 * p), 3) +
      matrix(stats::rnorm(n * p, sd = noise), n)
    colnames(m) <- paste0("f", seq_len(p))
    m
  }

  it("stores the low-rank form at fewer rows than features, with the distances mahalanobis() gives", {

    X  <- wide(40, 300, seed = 1)
    Xn <- wide(25, 300, seed = 2) + 0.3
    md <- compute_ad_metadata(X)

    expect_identical(md$form, "low_rank")
    expect_named(md, c("form", "centroid", "scale", "lambda", "basis", "values"))
    expect_identical(dim(md$basis), c(300L, 39L))
    expect_length(md$values, 39L)

    reference <- stats::mahalanobis(Xn, colMeans(X), corpcor::cov.shrink(X, verbose = FALSE))
    expect_equal(calculate_ad_distance(Xn, md), reference, tolerance = 1e-8)

    ## And the thresholds follow the distances
    calib <- wide(60, 300, seed = 3)
    full  <- list(form = "full", centroid = colMeans(X),
                  cov_matrix = corpcor::cov.shrink(X, verbose = FALSE))
    expect_equal(compute_ad_thresholds(calib, md), compute_ad_thresholds(calib, full),
                 tolerance = 1e-8)

    ## Smaller than the matrix it stands for
    expect_lt(utils::object.size(md), utils::object.size(full) / 4)

  })

  it("agrees with mahalanobis() when the correlations are barely shrunk", {

    ## Strongly correlated features over few rows: a small intensity, where
    ## the Woodbury weights are largest
    X  <- wide(60, 120, seed = 4, noise = 0.05)
    Xn <- wide(10, 120, seed = 5, noise = 0.05)
    md <- compute_ad_metadata(X)

    expect_identical(md$form, "low_rank")
    expect_lt(md$lambda, 0.1)

    reference <- stats::mahalanobis(Xn, colMeans(X), corpcor::cov.shrink(X, verbose = FALSE))
    expect_equal(calculate_ad_distance(Xn, md), reference, tolerance = 1e-8)

  })

  it("stores today's matrix, and today's distances, at more rows than features", {

    X  <- wide(200, 30, seed = 6)
    Xn <- wide(20, 30, seed = 7)
    md <- compute_ad_metadata(X)

    expect_identical(md$form, "full")
    expect_identical(md$cov_matrix, corpcor::cov.shrink(X, verbose = FALSE))
    expect_identical(calculate_ad_distance(Xn, md),
                     stats::mahalanobis(Xn, colMeans(X), corpcor::cov.shrink(X, verbose = FALSE)))

  })

  it("keeps the matrix when the low-rank form would not be smaller, or cannot hold", {

    ## r + 4 = p: the same size either way
    expect_identical(compute_ad_metadata(wide(27, 30, seed = 8))$form, "full")
    expect_identical(compute_ad_metadata(wide(25, 30, seed = 8))$form, "low_rank")

    ## A constant feature has no empirical correlation to factor; the full
    ## form takes it as it always has, corpcor's warning included
    X <- wide(20, 60, seed = 9)
    X[, 5] <- 1
    expect_identical(suppressWarnings(compute_ad_metadata(X))$form, "full")

  })

})

test_that("compute_ad_metadata refuses a matrix without column names, a broken invariant", {

  ## The mold's predictors come named; a matrix without names is a broken
  ## invariant, not the user's input. Too few rows and values that are not
  ## finite are fit_ad()'s to refuse, below.
  no_names <- matrix(stats::rnorm(100), 10, 10)
  expect_error(compute_ad_metadata(no_names), "column names",
               class = "horizons_error_internal")

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
  expect_error(calculate_ad_distance(wrong_names, md), "column names",
               class = "horizons_error_internal")

  ## A matrix with fewer columns is refused by the same names check; the
  ## saved bundle's own shape is checked at predict()'s door.
  wrong_count <- ad_matrix(5, 8, seed = 13)
  expect_error(calculate_ad_distance(wrong_count, md), "column names",
               class = "horizons_error_internal")

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

test_that("assign_ad_bin bins tied thresholds by the lower-bin rule, leaving the bins between them empty (#240)", {

  ## The issue's case: the second and third quartiles tie
  bins <- assign_ad_bin(c(1, 2, 5), c(1, 2, 2, 4))
  expect_identical(as.character(bins), c("Q1", "Q2", "OOD"))
  expect_identical(levels(bins), AD_BINS)

  ## Every quartile tied: a distance at the tie falls in Q1, Q2 and Q3 stay
  ## empty, and OOD is exactly a distance above the cutoff
  expect_identical(as.character(assign_ad_bin(c(0, 3, 4, 8, 9), c(3, 3, 3, 8))),
                   c("Q1", "Q1", "Q4", "Q4", "OOD"))

  ## The cutoff tied to the third quartile leaves Q4 empty
  expect_identical(as.character(assign_ad_bin(c(4, 4.5), c(1, 2, 4, 4))),
                   c("Q3", "OOD"))

  ## A first threshold of 0 bins a zero distance Q1, and a missing distance
  ## stays a missing flag
  expect_identical(as.character(assign_ad_bin(c(0, NA, 0.5), c(0, 1, 2, 3))),
                   c("Q1", NA, "Q2"))

})

test_that("assign_ad_bin validates inputs", {

  thr <- c(10, 20, 30, 40)

  ## A negative squared distance would bin as Q1 silently, so it stops the
  ## call
  expect_error(assign_ad_bin(c(-5, 10), thr), "non-negative", class = "horizons_error_internal")

  ## The thresholds come from a saved bundle, whose four non-decreasing
  ## values predict()'s door validator checks (ad_bundle_ok()).

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

  ## The OOD cutoff is the conformal ceiling(level * (n + 1))-th smallest
  ## calibration distance: the 199th of 200 here
  expect_identical(thr[4], sort(calculate_ad_distance(calib, md))[199])

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
## predict_ad() — the distance on the features predict() baked
## ---------------------------------------------------------------------------
## predict() bakes new data once per configuration and hands the AD the rows
## that baked to finite values; a row that did not is returned as NA and
## flagged (test-pipeline-predict.R). A failure here is the AD unit's.
## ---------------------------------------------------------------------------

## A fitted workflow whose recipe carries the real transform step, so the
## features are the production ones rather than a hand-built matrix. Its 60
## rows clear N_CALIB_MIN, so fit_ad() must return a bundle; the tests below
## assert it.
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

test_that("fit_ad() takes its minimums from min_calib_rows and min_ad_rows", {

  fx <- ad_fitted_workflow()

  ## The 60 rows clear the defaults; a minimum above them is a run failure
  ## of the AD unit, with the counts
  expect_false(is.null(fit_ad(fx$workflow, calib_data = fx$data)))
  expect_error(fit_ad(fx$workflow, calib_data = fx$data, min_calib_rows = 61L),
               "The calibration set has 60 rows, fewer than `min_calib_rows` (61).",
               fixed = TRUE, class = "horizons_error_run")

  ## A model trained on 8 rows is below the default covariance minimum of 10,
  ## and above a minimum of 5
  small <- ad_fitted_workflow(n = 8, seed = 43)
  expect_error(fit_ad(small$workflow, calib_data = fx$data),
               "The model was fit on 8 rows, fewer than the 10 the covariance needs",
               fixed = TRUE, class = "horizons_error_run")
  expect_false(is.null(fit_ad(small$workflow, calib_data = fx$data, min_ad_rows = 5L)))

})

test_that("predict_ad scores a matrix of baked features", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  expect_false(is.null(bundle))

  baked <- bake_predictors(fx$workflow, ad_new_spectra(fx$wn, n = 5))
  ad    <- predict_ad(baked, bundle)

  expect_s3_class(ad, "tbl_df")
  expect_equal(nrow(ad), 5L)
  expect_true(all(ad$.ad_distance >= 0))
  expect_identical(levels(ad$.ad_flag), AD_BINS)
  expect_false(anyNA(ad$.ad_flag))

})

test_that("predict_ad stops on the column-name tripwire when the bundle no longer matches the features", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  expect_false(is.null(bundle))

  ## The bundle's centroid no longer names the recipe's features, so
  ## distances would bind to the wrong ones. This used to return NULL with a
  ## warning; it is a bug, and stops the call.
  bundle$centroid <- bundle$centroid[-1]

  baked <- bake_predictors(fx$workflow, ad_new_spectra(fx$wn, n = 5))

  expect_error(predict_ad(baked, bundle),
               "Feature column names must match", fixed = TRUE,
               class = "horizons_error_internal")

})

test_that("predict_ad fails at its distance step when the distance cannot be computed", {

  fx     <- ad_fitted_workflow()
  bundle <- fit_ad(fx$workflow, calib_data = fx$data)

  expect_false(is.null(bundle))

  local_mocked_bindings(calculate_ad_distance = function(...) stop("covariance is singular"))

  baked <- bake_predictors(fx$workflow, ad_new_spectra(fx$wn, n = 5))
  out   <- run_unit(predict_ad(baked, bundle), "ad", "cfg_ad")

  expect_true(out$failed)
  expect_identical(out$records$step[out$records$type == "failure"], "ad_distance")
  expect_match(out$records$message[out$records$type == "failure"], "covariance is singular",
               fixed = TRUE)

})


## ---------------------------------------------------------------------------
## fit_ad() runs as a unit of its own inside the member, so each condition it
## cannot compute from raises, and the member names it in a skipped warning
## rather than holding a partial bundle or none, silently.
## ---------------------------------------------------------------------------

test_that("fit_ad refuses a calibration set under N_CALIB_MIN rows, saying so", {

  fx <- ad_fitted_workflow()

  expect_false(is.null(fit_ad(fx$workflow, calib_data = fx$data[seq_len(N_CALIB_MIN), ])))
  expect_error(fit_ad(fx$workflow, calib_data = fx$data[seq_len(N_CALIB_MIN - 1L), ]),
               "The calibration set has 29 rows", fixed = TRUE, class = "horizons_error_run")
  expect_error(fit_ad(fx$workflow, calib_data = NULL),
               "The calibration set has 0 rows", fixed = TRUE, class = "horizons_error_run")

})

test_that("fit_ad refuses a training matrix under N_AD_TRAIN_MIN rows, saying so", {

  fx    <- ad_fitted_workflow()
  short <- ad_fitted_workflow(n = N_AD_TRAIN_MIN - 1L)

  expect_equal(nrow(workflows::extract_mold(short$workflow)$predictors),
               N_AD_TRAIN_MIN - 1L)
  expect_error(fit_ad(short$workflow, calib_data = fx$data),
               "The model was fit on 9 rows", fixed = TRUE, class = "horizons_error_run")

})

test_that("fit_ad refuses training or calibration features that are not finite, Inf as well as NA", {

  fx <- ad_fitted_workflow()

  ## The training features, read from the mold
  real_mold <- mold_predictors
  local_mocked_bindings(mold_predictors = function(wf) {
    m <- real_mold(wf)
    m[2, 1] <- Inf
    m
  })
  expect_error(fit_ad(fx$workflow, calib_data = fx$data),
               "1 of the 60 training rows has a feature that is not finite (NA, NaN or Inf).",
               fixed = TRUE, class = "horizons_error_run")

})

test_that("fit_ad refuses a calibration spectrum that bakes to a value that is not finite", {

  fx <- ad_fitted_workflow()

  ## One calibration spectrum bakes to NA (step_transform_spectra's malformed
  ## spectrum path)
  one_na <- fx$data
  one_na[1, fx$wn] <- NA_real_
  expect_warning(
    expect_error(fit_ad(fx$workflow, calib_data = one_na),
                 "1 of the 60 calibration rows has a feature that is not finite", fixed = TRUE,
                 class = "horizons_error_run"),
    "could not be used by `step_transform_spectra()`", fixed = TRUE,
    class = "horizons_warning_flagged"
  )

})

## ---------------------------------------------------------------------------
## fit_ad() as the unit it runs in: each step's failure is recorded with its
## label, and the covariance's warning is recorded rather than muffled
## ---------------------------------------------------------------------------

run_ad <- function(fx, calib_data = fx$data, ...) {

  run_unit(fit_ad(fx$workflow, calib_data = calib_data, ...), unit = "ad", id = "cfg_001")

}

test_that("fit_ad fails at the covariance, the calibration bake or the thresholds, with the step", {

  fx <- ad_fitted_workflow()

  covariance <- local({
    local_mocked_bindings(compute_ad_metadata = function(...) stop("covariance failed"))
    run_ad(fx)
  })
  expect_true(covariance$failed)
  expect_identical(covariance$records$step, "ad_covariance")
  expect_match(unit_failure_message(covariance$records, NULL), "^AD covariance failed: covariance failed")

  ## The bake aborts on a missing spectral column
  bake <- run_ad(fx, calib_data = fx$data[, -1])
  expect_true(bake$failed)
  expect_identical(bake$records$step[bake$records$type == "failure"], "ad_calibration_bake")

  thresholds <- local({
    local_mocked_bindings(compute_ad_thresholds = function(...) stop("thresholds failed"))
    run_ad(fx)
  })
  expect_true(thresholds$failed)
  expect_identical(thresholds$records$step, "ad_thresholds")
  expect_match(unit_failure_message(thresholds$records, NULL), "^AD thresholds failed: thresholds failed")

})

test_that("fit_ad keeps the bundle at p > n and records no warning", {

  ## 12 training rows against 30 features
  fx    <- ad_fitted_workflow(n = 12, n_wn = 30, seed = 44)
  calib <- ad_fitted_workflow(n = 40, n_wn = 30, seed = 45)

  out <- run_ad(fx, calib_data = calib$data, min_ad_rows = 10L)

  expect_false(out$failed)
  expect_named(out$value, c("form", "centroid", "scale", "lambda", "basis", "values",
                            "ad_thresholds", "n_calib"))
  p <- length(out$value$centroid)
  expect_gt(p, 12L)
  expect_identical(out$value$form, "low_rank")
  expect_identical(dim(out$value$basis), c(p, 11L))
  expect_false(any(out$records$type == "warning"))

})
