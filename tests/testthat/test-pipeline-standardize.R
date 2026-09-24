# tests/testthat/test-pipeline-standardize.R
# Tests for standardize() function


## =============================================================================
## Test Fixtures
## =============================================================================

#' Create test horizons_data object with spectral data
#' @noRd
create_test_spectra <- function(n_samples    = 10,
                                n_wavelengths = 100,
                                wn_min        = 600,
                                wn_max        = 4000,
                                resolution    = NULL) {

  ## Calculate resolution if not provided --------------------------------------

  if (is.null(resolution)) {

    resolution <- (wn_max - wn_min) / (n_wavelengths - 1)

  }

  ## Generate wavelengths (decreasing order) -----------------------------------

  wavelengths <- seq(from = wn_max, to = wn_min, by = -resolution)
  wavelengths <- wavelengths[1:min(n_wavelengths, length(wavelengths))]

  ## Generate spectral data ----------------------------------------------------

  set.seed(42)
  spectra <- matrix(
    runif(n_samples * length(wavelengths), min = 0.1, max = 0.8),
    nrow = n_samples,
    ncol = length(wavelengths)
  )

  ## Build tibble --------------------------------------------------------------

  col_names <- paste0("wn_", wavelengths)
  colnames(spectra) <- col_names

  data <- tibble::tibble(
    sample_id = paste0("sample_", seq_len(n_samples))
  )

  data <- dplyr::bind_cols(data, tibble::as_tibble(spectra))

  ## Create horizons_data object -----------------------------------------------

  spectra(data)

}


## =============================================================================
## Input Validation Tests
## =============================================================================

test_that("standardize() rejects non-horizons_data input", {

  expect_error(
    standardize(data.frame(x = 1:10)),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(list(a = 1)),
    class = "horizons_input_error"
  )

})


test_that("standardize() validates resample parameter", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, resample = "two"),
    class = "horizons_input_error"
 )

  expect_error(
    standardize(hd, resample = -1),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, resample = c(2, 4)),
    class = "horizons_input_error"
  )

})


test_that("standardize() validates trim parameter", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, trim = 600),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, trim = c("600", "4000")),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, trim = c(4000, 600)),
    class = "horizons_input_error"
  )

})


test_that("standardize() validates logical parameters", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, remove_water = "yes"),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, baseline = NA),
    class = "horizons_input_error"
  )

  expect_error(
    standardize(hd, force = 1),
    class = "horizons_input_error"
  )

})


## =============================================================================
## Resampling Tests
## =============================================================================

test_that("resample_spectra() changes wavelength count", {

  ## Create data at 4 cm⁻¹ resolution ------------------------------------------

  hd <- create_test_spectra(n_wavelengths = 850, wn_min = 600, wn_max = 4000)

  ## Get original info ---------------------------------------------------------

  original_n <- hd$data$n_predictors

  ## Resample to 2 cm⁻¹ --------------------------------------------------------

  hd_resampled <- standardize(hd, resample = 2, trim = NULL,
                               remove_water = FALSE, baseline = FALSE)

  ## Check wavelength count increased ------------------------------------------

  expect_gt(hd_resampled$data$n_predictors, original_n)

})


test_that("resample_spectra(target_resolution =) uses the multiples inside the data's range, bit for bit the new_wav path", {

  ## Arrange: an axis offset from the multiples of 2, as an instrument's is
  set.seed(7)
  wn   <- seq(3999.57, 599.57, by = -4)
  m    <- matrix(stats::runif(5 * length(wn)), nrow = 5)
  grid <- seq(3998, 600, by = -2)

  ## Act
  by_res  <- resample_spectra(m, wn, target_resolution = 2)
  by_grid <- resample_spectra(m, wn, new_wav = grid)

  ## Assert: the grid is the multiples, not a sequence stepped from 3999.57
  expect_identical(by_res$wavelengths, grid)
  expect_identical(by_grid$matrix,      by_res$matrix)
  expect_identical(by_grid$wavelengths, by_res$wavelengths)
  expect_identical(by_grid$n_after,     by_res$n_after)

})


test_that("resample_spectra(new_wav =) returns exactly the grid given, decreasing", {

  set.seed(7)
  wn   <- seq(4000, 600, by = -4)
  m    <- matrix(stats::runif(3 * length(wn)), nrow = 3)
  grid <- c(3990, 3500, 2222, 1000.5, 604)

  out <- resample_spectra(m, wn, new_wav = grid)

  expect_identical(out$wavelengths, grid)
  expect_identical(ncol(out$matrix), length(grid))
  expect_true(all(diff(out$wavelengths) < 0))

  ## An increasing grid is accepted and returned decreasing
  out2 <- resample_spectra(m, wn, new_wav = rev(grid))
  expect_identical(out2$matrix, out$matrix)
  expect_identical(out2$wavelengths, grid)

})


test_that("resample_spectra(new_wav =) reproduces the input at its own knots", {

  set.seed(7)
  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(3 * length(wn)), nrow = 3)

  ## Every other column of the source grid
  grid <- wn[seq(1, length(wn), by = 2)]
  out  <- resample_spectra(m, wn, new_wav = grid)

  expect_equal(unname(out$matrix), m[, seq(1, length(wn), by = 2)], tolerance = 1e-12)

})


test_that("resample_spectra(new_wav =) refuses to extrapolate", {

  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(2 * length(wn)), nrow = 2)

  expect_error(resample_spectra(m, wn, new_wav = seq(4100, 600, by = -4)),
               regexp = "4100", class = "horizons_input_error")
  expect_error(resample_spectra(m, wn, new_wav = seq(4000, 500, by = -4)),
               regexp = "500", class = "horizons_input_error")

})


test_that("resample_spectra() needs exactly one of target_resolution and new_wav", {

  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(2 * length(wn)), nrow = 2)

  expect_error(resample_spectra(m, wn), class = "horizons_input_error")
  expect_error(resample_spectra(m, wn, target_resolution = 2, new_wav = wn),
               class = "horizons_input_error")

})


test_that("standardize() with resample = NULL skips resampling", {

  hd <- create_test_spectra(n_wavelengths = 100)

  original_n <- hd$data$n_predictors

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  expect_equal(hd_result$data$n_predictors, original_n)

})


## =============================================================================
## Canonical Grid Tests (#64, #18)
## =============================================================================
## Synthetic stand-ins for the two sources #64 was found on: a KSSL-shaped
## library (600 to 4000 at 2 cm-1, stored increasing, as the snapshot is) and
## MOYS-shaped scans (from 599.74 at 1.93 cm-1, well past 4000).

#' Evaluate without the pipeline's console tree
#' @noRd
no_output <- function(expr) {

  utils::capture.output(value <- expr)
  value

}

#' A horizons_data on an explicit axis, in the order given
#'
#' @description
#' Builds directly through `new_horizons_data()`, bypassing `spectra()`.
#' `spectra()` now sorts its predictor axis to decreasing and validates at
#' `stage = "raw"` before returning (#24), so routing through it here would
#' make every "given increasing" fixture below arrive already sorted, and
#' the standardize()-level sort and validation checks these tests exist for
#' would silently stop exercising anything.
#' @noRd
make_axis_spectra <- function(wn, f = NULL, n = 3) {

  set.seed(11)

  m <- if (is.null(f)) {
    matrix(stats::runif(n * length(wn), 0.1, 0.8), nrow = n)
  } else {
    t(vapply(seq_len(n), function(i) i * f(wn), numeric(length(wn))))
  }

  colnames(m) <- paste0("wn_", wn)

  analysis <- dplyr::bind_cols(
    tibble::tibble(sample_id = paste0("s", seq_len(n))),
    tibble::as_tibble(m)
  )

  role_map <- tibble::tibble(
    variable = names(analysis),
    role     = c("id", rep("predictor", ncol(m)))
  )

  new_horizons_data(
    analysis       = analysis,
    role_map       = role_map,
    spectra_source = "test",
    spectra_type   = "tibble"
  )

}

#' A smooth absorbance-like spectrum: sloped baseline plus Gaussian bands,
#' two of them close to the trim bounds so the ends carry curvature
#' @noRd
smooth_spectrum <- function(wn) {

  0.3 + 5e-5 * (wn - 600) +
    0.10 * exp(-((wn -  640) / 30)^2) +
    0.40 * exp(-((wn - 1030) / 40)^2) +
    0.15 * exp(-((wn - 1630) / 50)^2) +
    0.25 * exp(-((wn - 2920) / 60)^2) +
    0.20 * exp(-((wn - 3620) / 25)^2) +
    0.10 * exp(-((wn - 3960) / 30)^2)

}

predictor_names <- function(hd) hd$data$role_map$variable[hd$data$role_map$role == "predictor"]

KSSL_WN <- seq(600, 4000, by = 2)
MOYS_WN <- 599.74 + 1.93 * (0:3574)


test_that("a KSSL-shaped axis stored increasing comes out on wn_4000 ... wn_600", {

  kssl <- make_axis_spectra(KSSL_WN)

  at_2 <- no_output(standardize(kssl, resample = 2, trim = c(600, 4000)))
  at_4 <- no_output(standardize(kssl, resample = 4, trim = c(600, 4000)))

  expect_identical(predictor_names(at_2), paste0("wn_", seq(4000, 600, by = -2)))
  expect_identical(predictor_names(at_4), paste0("wn_", seq(4000, 600, by = -4)))
  expect_identical(at_2$data$n_predictors, 1701L)
  expect_identical(at_4$data$n_predictors, 851L)

})


test_that("a MOYS-shaped axis lands on the same columns as a KSSL-shaped one", {

  kssl <- no_output(standardize(make_axis_spectra(KSSL_WN), resample = 4, trim = c(600, 4000)))
  moys <- no_output(standardize(make_axis_spectra(MOYS_WN), resample = 4, trim = c(600, 4000)))

  ## 599.74 sits just below the bound, and it is what lets 600 be a column
  expect_identical(predictor_names(moys), predictor_names(kssl))
  expect_identical(predictor_names(moys)[c(1, 851)], c("wn_4000", "wn_600"))

  expect_true(moys$provenance$standardization$resampled)
  expect_identical(moys$provenance$standardization$grid,
                   list(min = 600, max = 4000, step = 4, n = 851L))

})


test_that("grid points the data does not reach are dropped with a warning, never extrapolated", {

  ## Covers 651.5 to 3500: short of the trim bounds at both ends
  short <- make_axis_spectra(seq(3500, 651.5, by = -1.5))

  caught <- list()
  out <- withCallingHandlers(
    no_output(standardize(short, resample = 4, trim = c(600, 4000))),
    horizons_standardize_warning = function(w) {
      caught[[length(caught) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  ## One warning, naming how many points went and where (cli wraps the
  ## message, so compare with the whitespace collapsed)
  expect_length(caught, 1L)
  msg <- gsub("\\s+", " ", conditionMessage(caught[[1]]))
  expect_match(msg, "Dropped 138 grid points", fixed = TRUE)
  expect_match(msg, "13 below the data's start at 651.5 (600 to 648 cm-1)", fixed = TRUE)
  expect_match(msg, "125 above the data's end at 3500 (3504 to 4000 cm-1)", fixed = TRUE)

  ## The covered multiples survive, and nothing beyond the data does
  expect_identical(predictor_names(out), paste0("wn_", seq(3500, 652, by = -4)))
  expect_identical(out$provenance$standardization$grid$n, 713L)

})


test_that("an axis already on the canonical grid is not re-interpolated", {

  kssl <- make_axis_spectra(rev(KSSL_WN))
  before <- as.matrix(kssl$data$analysis[, predictor_names(kssl)])

  expect_output(
    out <- standardize(kssl, resample = 2, trim = c(600, 4000)),
    regexp = "already on the 2 cm"
  )

  ## Values untouched, bit for bit; a spline through its own knots would only
  ## agree to about 1e-12
  after <- as.matrix(out$data$analysis[, predictor_names(out)])
  expect_identical(after, before)
  expect_false(out$provenance$standardization$resampled)
  expect_identical(out$provenance$standardization$grid$n, 1701L)

  ## Stored increasing (make_axis_spectra() bypasses spectra()'s own sort,
  ## #24, so this is genuinely unsorted input): standardize() sorts it, and
  ## the same spectra only change column order.
  inc     <- make_axis_spectra(KSSL_WN)
  inc_out <- no_output(standardize(inc, resample = 2, trim = c(600, 4000)))
  inc_in  <- as.matrix(inc$data$analysis[, predictor_names(inc)])

  expect_identical(unname(as.matrix(inc_out$data$analysis[, predictor_names(inc_out)])),
                   unname(inc_in[, rev(seq_len(ncol(inc_in)))]))

})


## ---------------------------------------------------------------------------
## Raw-stage validation: warns rather than aborts before average() (#24)
## ---------------------------------------------------------------------------

test_that("standardize() neither aborts nor warns on duplicate sample ids", {

  ## Arrange — replicate scans that haven't reached average() yet. This is
  ## legitimate before replicates collapse, the same as at spectra() and
  ## parse_ids(); standardize() validates at stage = "raw" for exactly this
  ## reason (#24 rework). spectra() and parse_ids() are where the duplicate
  ## first appears and warns; standardize() calls the validator with
  ## warn_ids = FALSE so its own entry/exit structural checks do not
  ## re-warn about the same rows on every call.
  hd <- make_axis_spectra(KSSL_WN, n = 2)
  hd$data$analysis$sample_id <- c("S1", "S1")

  ## Act & Assert --------------------------------------------------------------

  expect_no_warning(
    out <- no_output(standardize(hd, resample = 2, trim = c(600, 4000)))
  )

  expect_identical(out$data$analysis$sample_id, c("S1", "S1"))

})


test_that("standardize() refuses a column with no role_map entry instead of silently dropping it (#24)", {

  ## Arrange — Steps 7-9 rebuild data$analysis from role_map's predictor and
  ## non-predictor columns; a column present in analysis but absent from
  ## role_map is referenced by neither set, so it used to disappear from the
  ## output without a trace. The entry-stage validate_horizons_data(x, stage
  ## = "raw") call (#24) refuses it before the rebuild happens.
  ##
  ## rev(KSSL_WN) (already decreasing) is deliberate: an increasing axis
  ## would reorder in Step 1b, and sort_axis_decreasing() catches an
  ## unregistered column through set_analysis()'s own check on that path,
  ## which would pass even without the Step 1d entry check this test exists
  ## to exercise.
  hd <- make_axis_spectra(rev(KSSL_WN), n = 2)
  hd$data$analysis$stray_column <- seq_len(nrow(hd$data$analysis))

  expect_error(
    standardize(hd, resample = 2, trim = c(600, 4000)),
    regexp = "[Mm]issing from.*role_map",
    class  = "horizons_validation_error"
  )

})


test_that("an NA outside the trim range passes spectra() |> standardize(trim = )", {

  ## Arrange — a wide axis with one bad value outside where trim will cut it
  wn <- seq(4500, 600, by = -100)
  df <- tibble::tibble(sample_id = c("s1", "s2"))

  for (w in wn) {

    df[[as.character(w)]] <- c(0.3, 0.4)

  }

  df[["4500"]][1] <- NA_real_

  ## Act -------------------------------------------------------------------
  ## spectra() (raw stage) does not check predictor NAs at all; standardize()
  ## drops the wn_4500 column via trim before its own finiteness check
  ## (Step 6b) ever sees it.

  hd  <- no_output(spectra(df))
  out <- no_output(standardize(hd, trim = c(600, 4000)))

  ## Assert ----------------------------------------------------------------

  expect_false("wn_4500" %in% predictor_names(out))
  expect_false(anyNA(as.matrix(out$data$analysis[, predictor_names(out)])))

})


test_that("increasing-order input with baseline correction gives the decreasing-order result", {

  ## The baseline helper reverses its output on the assumption of decreasing
  ## input; before the sort this mirrored increasing spectra end to end and
  ## still passed validation. resample = NULL is deliberate: with resampling
  ## engaged, both inc and dec are re-interpolated onto the same canonical
  ## grid regardless of their original order, which can mask a sort
  ## regression here; with resample = NULL, baseline correction runs
  ## directly on the trimmed native axis, in whatever order the object
  ## carries, so this is the version that actually depends on the sort.
  inc <- make_axis_spectra(KSSL_WN, f = smooth_spectrum)
  dec <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum)

  out_inc <- no_output(standardize(inc, resample = NULL, baseline = TRUE))
  out_dec <- no_output(standardize(dec, resample = NULL, baseline = TRUE))

  expect_identical(predictor_names(out_inc), predictor_names(out_dec))
  expect_equal(as.matrix(out_inc$data$analysis[, predictor_names(out_inc)]),
               as.matrix(out_dec$data$analysis[, predictor_names(out_dec)]))

})


test_that("interpolating a smooth spectrum onto the shifted grid stays on the true curve, edges included", {

  moys <- make_axis_spectra(MOYS_WN, f = smooth_spectrum, n = 1)

  for (res in c(4, 2, 1.5)) {

    out <- no_output(standardize(moys, resample = res, trim = c(600, 4000)))
    wn  <- as.numeric(sub("^wn_", "", predictor_names(out)))
    got <- as.numeric(out$data$analysis[1, predictor_names(out)])

    expect_lt(max(abs(got - smooth_spectrum(wn))), 1e-5)

    ## Both ends by name: no end effect from the margin point or the bound
    ends <- c(1, length(wn))
    expect_lt(max(abs(got[ends] - smooth_spectrum(wn[ends]))), 1e-6)

  }

})


test_that("non-integer resolutions give clean, stable column names", {

  moys <- make_axis_spectra(MOYS_WN)

  ## 1.5 cm-1: 3999, 3997.5, ..., 600
  at_1.5 <- no_output(standardize(moys, resample = 1.5, trim = c(600, 4000)))
  expect_identical(predictor_names(at_1.5), paste0("wn_", seq(3999, 600, by = -1.5)))

  ## 0.1 cm-1: the expected names are built from integers, so no float
  ## arithmetic reaches them
  at_0.1 <- no_output(standardize(moys, resample = 0.1, trim = c(600, 610)))
  k      <- 6100:6000
  expect_identical(predictor_names(at_0.1),
                   paste0("wn_", k %/% 10, ifelse(k %% 10 == 0, "", paste0(".", k %% 10))))

  ## A step with no finite decimal form is snapped to six decimal places
  fine     <- make_axis_spectra(seq(610, 590, by = -0.5))
  at_third <- no_output(standardize(fine, resample = 1 / 3, trim = c(600, 601)))
  expect_identical(predictor_names(at_third),
                   c("wn_601", "wn_600.666667", "wn_600.333333", "wn_600"))

})


test_that("resample = NULL keeps trim-as-subset but still sorts", {

  kssl <- make_axis_spectra(KSSL_WN)
  out  <- no_output(standardize(kssl, resample = NULL, trim = c(1000, 2000)))

  expect_identical(predictor_names(out), paste0("wn_", seq(2000, 1000, by = -2)))
  expect_false(out$provenance$standardization$resampled)
  expect_null(out$provenance$standardization$grid)

  ## A subset, with no margin: 599.74 is outside c(600, 4000) and stays out
  moys <- no_output(standardize(make_axis_spectra(MOYS_WN), resample = NULL, trim = c(600, 4000)))
  wn   <- as.numeric(sub("^wn_", "", predictor_names(moys)))
  expect_gte(min(wn), 600)
  expect_lte(max(wn), 4000)

})


test_that("a call with every operation off still sorts, validates, and changes nothing else", {

  inc    <- make_axis_spectra(KSSL_WN)
  before <- as.matrix(inc$data$analysis[, predictor_names(inc)])

  out <- no_output(standardize(inc, resample = NULL, trim = NULL,
                               remove_water = FALSE, baseline = FALSE))

  ## Decreasing columns, a valid object, marked standardized
  expect_identical(predictor_names(out), paste0("wn_", rev(KSSL_WN)))
  expect_no_error(validate_horizons_data(out))
  expect_false(is.null(out$provenance$standardization))

  ## The same value at every wavenumber: only the column order moved
  after <- as.matrix(out$data$analysis[, predictor_names(out)])
  expect_identical(after[, colnames(before)], before)
  expect_identical(out$data$analysis$sample_id, inc$data$analysis$sample_id)

  ## standardize() validates at "raw" stage (#24 rework): a stray NA in a
  ## predictor column is legitimate before replicates are collapsed by
  ## average() (it stays legal until a real trim/resample runs and Step 6b's
  ## own non-finite check applies), so the no-op path no longer refuses it.
  bad <- inc
  bad$data$analysis$wn_1000[1] <- NA_real_
  bad_out <- no_output(standardize(bad, resample = NULL, trim = NULL))
  expect_true(is.na(bad_out$data$analysis$wn_1000[1]))

  ## A value that survives an actual trim/resample as non-finite is still
  ## caught, by standardize()'s own Step 6b check rather than the validator.
  expect_error(
    no_output(standardize(bad, resample = 2, trim = c(600, 4000))),
    "non-finite"
  )

})


test_that("force = TRUE with every operation off keeps the grid an earlier call recorded", {

  on_4 <- no_output(standardize(make_axis_spectra(MOYS_WN), resample = 4))

  expect_warning(
    again <- no_output(standardize(on_4, resample = NULL, trim = NULL, force = TRUE)),
    regexp = "Re-standardizing"
  )

  expect_true(again$provenance$standardization$resampled)
  expect_identical(again$provenance$standardization$grid,
                   on_4$provenance$standardization$grid)
  expect_identical(predictor_names(again), predictor_names(on_4))

})


test_that("the trim line counts the columns inside the bounds, not the interpolation margin", {

  ## MOYS-shaped: 1761 points inside 600-4000, plus 599.74 and one above 4000
  expect_output(
    standardize(make_axis_spectra(MOYS_WN), resample = 4, trim = c(600, 4000)),
    regexp = "Trimming: 600-4000 cm⁻¹ \\(3575 → 1761\\)"
  )

})


test_that("trim = NULL keeps each source's extent, and two sources share names where they overlap", {

  a_wn <- MOYS_WN[MOYS_WN >= 700 & MOYS_WN <= 3000]
  b_wn <- seq(3500, 650, by = -2)

  a <- no_output(standardize(make_axis_spectra(a_wn), resample = 4, trim = NULL))
  b <- no_output(standardize(make_axis_spectra(b_wn), resample = 4, trim = NULL))

  wn_of <- function(nm) as.numeric(sub("^wn_", "", nm))
  lo    <- max(min(a_wn), min(b_wn))
  hi    <- min(max(a_wn), max(b_wn))

  a_overlap <- predictor_names(a)[wn_of(predictor_names(a)) >= lo & wn_of(predictor_names(a)) <= hi]
  b_overlap <- predictor_names(b)[wn_of(predictor_names(b)) >= lo & wn_of(predictor_names(b)) <= hi]

  ## a runs 700.10 to 2998.73, so its lattice runs 704 to 2996
  expect_identical(a_overlap, b_overlap)
  expect_identical(a_overlap[c(1, length(a_overlap))], c("wn_2996", "wn_704"))

  ## Beyond the overlap each keeps its own extent: the two do not share all columns
  expect_false(identical(predictor_names(a), predictor_names(b)))

})


## =============================================================================
## Gaps in the Input Axis
## =============================================================================
## A band deleted upstream, or by an earlier standardize(remove_water = TRUE),
## is a hole the spline must never fill.

#' A KSSL-shaped library already standardized with its water bands removed
#' @noRd
water_removed_library <- function() {

  kssl <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum)
  no_output(standardize(kssl, resample = 2, remove_water = TRUE))

}

#' Collect every warning an expression raises, and its value
#' @noRd
with_warnings <- function(expr) {

  caught <- list()
  value  <- withCallingHandlers(
    expr,
    warning = function(w) {
      caught[[length(caught) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = caught)

}

in_water_band <- function(wn) (wn >= 1580 & wn <= 1720) | (wn >= 3100 & wn <= 3700)


test_that("an axis with the water bands cut upstream is not spline-filled, and stays as it was", {

  cut <- water_removed_library()
  cut_values <- as.matrix(cut$data$analysis[, predictor_names(cut)])

  ## Rebuilt from its table, as spectra cut by another tool would arrive
  upstream <- make_axis_spectra(as.numeric(sub("^wn_", "", predictor_names(cut))))
  upstream$data$analysis[, predictor_names(cut)] <- cut$data$analysis[, predictor_names(cut)]

  res <- with_warnings(no_output(standardize(upstream, resample = 2)))
  out <- res$value

  ## One warning, naming both gaps
  gap_warnings <- Filter(function(w) inherits(w, "horizons_standardize_warning"), res$warnings)
  expect_length(gap_warnings, 1L)
  msg <- gsub("\\s+", " ", conditionMessage(gap_warnings[[1]]))
  expect_match(msg, "2 gaps in it: 1578 to 1722, 3098 to 3702 cm-1", fixed = TRUE)

  ## The same 1329 columns and the same values: on the grid apart from the gaps
  expect_identical(predictor_names(out), predictor_names(cut))
  expect_identical(as.matrix(out$data$analysis[, predictor_names(out)]), cut_values)
  expect_false(out$provenance$standardization$resampled)
  expect_false(any(in_water_band(as.numeric(sub("^wn_", "", predictor_names(out))))))

})


test_that("force = TRUE on an object standardized with remove_water = TRUE leaves the bands empty", {

  cut <- water_removed_library()

  res <- with_warnings(no_output(standardize(cut, force = TRUE)))
  out <- res$value

  classes <- vapply(res$warnings, function(w) class(w)[1], character(1))
  expect_true("horizons_standardize_warning" %in% classes)

  expect_identical(predictor_names(out), predictor_names(cut))
  expect_identical(as.matrix(out$data$analysis[, predictor_names(out)]),
                   as.matrix(cut$data$analysis[, predictor_names(cut)]))

})


test_that("resampling a gapped axis coarser keeps the bands empty and the rest on its knots", {

  cut <- water_removed_library()

  res <- suppressWarnings(no_output(standardize(cut, resample = 4, force = TRUE)))
  wn  <- as.numeric(sub("^wn_", "", predictor_names(res)))

  ## Every multiple of 4 outside the bands, and none inside them
  grid <- seq(4000, 600, by = -4)
  expect_identical(wn, grid[!in_water_band(grid)])

  ## Multiples of 4 are knots of the 2 cm-1 input, so the values are the
  ## input's own, to spline round-off
  expect_equal(unname(as.matrix(res$data$analysis[, predictor_names(res)])),
               unname(as.matrix(cut$data$analysis[, paste0("wn_", wn)])),
               tolerance = 1e-10)

})


test_that("a CO2 gap in an off-grid axis is dropped, and each side is interpolated on its own", {

  co2_cut <- MOYS_WN[MOYS_WN < 2300 | MOYS_WN > 2400]
  moys    <- make_axis_spectra(co2_cut, f = smooth_spectrum, n = 1)

  expect_warning(
    out <- no_output(standardize(moys, resample = 4)),
    regexp = "gap",
    class  = "horizons_standardize_warning"
  )

  wn  <- as.numeric(sub("^wn_", "", predictor_names(out)))
  got <- as.numeric(out$data$analysis[1, predictor_names(out)])

  gap_low  <- max(co2_cut[co2_cut < 2300])
  gap_high <- min(co2_cut[co2_cut > 2400])

  ## Nothing inside the hole, everything else on the 4 cm-1 lattice
  expect_false(any(wn > gap_low & wn < gap_high))
  grid <- seq(4000, 600, by = -4)
  expect_identical(wn, grid[grid <= gap_low | grid >= gap_high])

  ## On the true curve everywhere, the columns beside the gap included
  expect_lt(max(abs(got - smooth_spectrum(wn))), 1e-5)

})


## =============================================================================
## Baseline Correction on the Canonical Grid
## =============================================================================

test_that("baseline correction of data already on the grid is the hull of the data itself", {

  ## Exactly 600 to 4000 at 2: nothing beyond the bounds, nothing to resample,
  ## so the result is the one the order change must not move
  kssl <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum)
  raw  <- as.matrix(kssl$data$analysis[, predictor_names(kssl)])

  out <- no_output(standardize(kssl, resample = 2, trim = c(600, 4000), baseline = TRUE))

  expect_identical(unname(as.matrix(out$data$analysis[, predictor_names(out)])),
                   unname(apply_baseline_correction(raw, rev(KSSL_WN))))

})


test_that("a point beyond the trim bound does not move the baseline inside it", {

  ## Two sources, bit-identical inside 600-4000; one also carries 598, set
  ## low so that a hull reaching it would tilt
  inside <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum)

  wider_wn <- c(rev(KSSL_WN), 598)
  wider    <- make_axis_spectra(wider_wn, f = smooth_spectrum)
  wider$data$analysis[, predictor_names(inside)] <- inside$data$analysis[, predictor_names(inside)]
  wider$data$analysis$wn_598 <- wider$data$analysis$wn_598 - 0.2

  for (res in c(2, 4)) {

    a <- no_output(standardize(inside, resample = res, trim = c(600, 4000), baseline = TRUE))
    b <- no_output(standardize(wider,  resample = res, trim = c(600, 4000), baseline = TRUE))

    expect_identical(predictor_names(a), predictor_names(b))
    expect_equal(as.matrix(a$data$analysis[, predictor_names(a)]),
                 as.matrix(b$data$analysis[, predictor_names(b)]),
                 tolerance = 1e-10)

  }

})


## =============================================================================
## Single-Sample Objects (#78)
## =============================================================================
## Predicting one new sample means standardizing one spectrum the way the
## training spectra were. Every operation here is row-wise, so a sample
## standardized alone has to come out as its row of a batch standardized alike.

#' The predictor block of a horizons_data, as a matrix
#' @noRd
predictor_block <- function(hd) as.matrix(hd$data$analysis[, predictor_names(hd)])


test_that("standardize(baseline = TRUE) runs on a single sample, with and without resampling (#78)", {

  ## MOYS-shaped: stored increasing and off the grid, so resample = 4 really
  ## interpolates; three random spectra, so the rows differ in shape
  batch  <- make_axis_spectra(MOYS_WN)
  single <- subset_rows(batch, "s2", record = FALSE)

  for (res in list(NULL, 4)) {

    one  <- no_output(standardize(single, resample = res, trim = c(600, 4000), baseline = TRUE))
    many <- no_output(standardize(batch,  resample = res, trim = c(600, 4000), baseline = TRUE))

    expect_identical(one$data$n_rows, 1L)
    expect_identical(predictor_names(one), predictor_names(many))
    expect_identical(unname(predictor_block(one)), unname(predictor_block(many)[2, , drop = FALSE]))

  }

})


test_that("a single sample standardizes as its row of a batch, under every option (#78)", {

  batch  <- make_axis_spectra(MOYS_WN)
  single <- subset_rows(batch, "s2", record = FALSE)

  opts <- expand.grid(resample     = c(NA, 4),
                      trim         = c(FALSE, TRUE),
                      remove_water = c(FALSE, TRUE),
                      baseline     = c(FALSE, TRUE))

  for (i in seq_len(nrow(opts))) {

    o    <- opts[i, ]
    args <- list(resample     = if (is.na(o$resample)) NULL else o$resample,
                 trim         = if (o$trim) c(600, 4000) else NULL,
                 remove_water = o$remove_water,
                 baseline     = o$baseline)
    what <- paste(names(o), vapply(o, format, character(1)), sep = " = ", collapse = ", ")

    one  <- no_output(do.call(standardize, c(list(single), args)))
    many <- no_output(do.call(standardize, c(list(batch),  args)))

    expect_identical(predictor_names(one), predictor_names(many), info = what)
    expect_identical(unname(predictor_block(one)), unname(predictor_block(many)[2, , drop = FALSE]),
                     info = what)

  }

})


## =============================================================================
## Helper Edge Cases
## =============================================================================

test_that("resample_spectra() returns a single grid point as samples by one, in sample order", {

  set.seed(3)
  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(3 * length(wn)), nrow = 3)

  out <- resample_spectra(m, wn, new_wav = 2000)

  expect_identical(dim(out$matrix), c(3L, 1L))
  expect_equal(as.numeric(out$matrix), m[, wn == 2000], tolerance = 1e-12)

})


test_that("apply_baseline_correction() returns columns in the order it was given them", {

  wn <- seq(4000, 600, by = -4)
  m  <- rbind(smooth_spectrum(wn), 2 * smooth_spectrum(wn))

  dec <- apply_baseline_correction(m, wn)
  inc <- apply_baseline_correction(m[, rev(seq_along(wn))], rev(wn))

  expect_equal(unname(inc[, rev(seq_along(wn))]), unname(dec))

})


test_that("apply_baseline_correction() returns one spectrum as a 1 x p matrix named by wavenumber (#78)", {

  ## prospectr::baseline() returns a vector for one row
  wn <- seq(4000, 600, by = -4)
  m  <- rbind(smooth_spectrum(wn), 2 * smooth_spectrum(wn) + 1e-4 * (wn - 600))

  one  <- apply_baseline_correction(m[2, , drop = FALSE], wn)
  both <- apply_baseline_correction(m, wn)

  expect_true(is.matrix(one))
  expect_identical(dim(one), c(1L, length(wn)))
  expect_identical(colnames(one), colnames(both))
  expect_identical(colnames(one), as.character(wn))
  expect_identical(one, both[2, , drop = FALSE])

})


## =============================================================================
## Trimming Tests
## =============================================================================

test_that("trim_spectra() removes wavelengths outside range", {

  hd <- create_test_spectra(n_wavelengths = 200, wn_min = 400, wn_max = 4500)

  original_n <- hd$data$n_predictors

  ## Trim to 600-4000 ----------------------------------------------------------

  hd_trimmed <- standardize(hd, resample = NULL, trim = c(600, 4000),
                            remove_water = FALSE, baseline = FALSE)

  ## Check wavelength count decreased ------------------------------------------

  expect_lt(hd_trimmed$data$n_predictors, original_n)

  ## Check all wavelengths are within range ------------------------------------

  predictor_cols <- hd_trimmed$data$role_map$variable[
    hd_trimmed$data$role_map$role == "predictor"
  ]
  wavelengths <- as.numeric(gsub("^wn_", "", predictor_cols))

  expect_true(all(wavelengths >= 600))
  expect_true(all(wavelengths <= 4000))

})


test_that("standardize() with trim = NULL skips trimming", {

  hd <- create_test_spectra(n_wavelengths = 100)

  original_n <- hd$data$n_predictors

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  expect_equal(hd_result$data$n_predictors, original_n)

})


## =============================================================================
## Water Band Removal Tests
## =============================================================================

test_that("remove_water_bands() removes water absorption regions", {

  ## Create data spanning water bands ------------------------------------------

  hd <- create_test_spectra(n_wavelengths = 400, wn_min = 1500, wn_max = 3700)

  original_n <- hd$data$n_predictors

  ## Remove water bands --------------------------------------------------------

  hd_filtered <- standardize(hd, resample = NULL, trim = NULL,
                             remove_water = TRUE, baseline = FALSE)

  ## Check wavelength count decreased ------------------------------------------

  expect_lt(hd_filtered$data$n_predictors, original_n)

  ## Check water band regions are gone -----------------------------------------

  predictor_cols <- hd_filtered$data$role_map$variable[
    hd_filtered$data$role_map$role == "predictor"
  ]
  wavelengths <- as.numeric(gsub("^wn_", "", predictor_cols))

  ## OH bending: 1580-1720 (OSSL standard) -------------------------------------

  in_oh_bending <- wavelengths >= 1580 & wavelengths <= 1720
  expect_false(any(in_oh_bending))

  ## OH stretching: 3100-3700 (OSSL standard) ----------------------------------

  in_oh_stretching <- wavelengths >= 3100 & wavelengths <= 3700
  expect_false(any(in_oh_stretching))

})


test_that("standardize() with remove_water = FALSE skips water removal", {

  hd <- create_test_spectra(n_wavelengths = 100, wn_min = 1500, wn_max = 3700)

  original_n <- hd$data$n_predictors

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  expect_equal(hd_result$data$n_predictors, original_n)

})


## =============================================================================
## Baseline Correction Tests
## =============================================================================

test_that("baseline correction modifies spectral values", {

  hd <- create_test_spectra(n_wavelengths = 100)

  ## Get original spectral values ----------------------------------------------

  predictor_cols <- hd$data$role_map$variable[
    hd$data$role_map$role == "predictor"
  ]
  original_values <- as.matrix(hd$data$analysis[, predictor_cols])

  ## Apply baseline correction -------------------------------------------------

  hd_corrected <- standardize(hd, resample = NULL, trim = NULL,
                              remove_water = FALSE, baseline = TRUE)

  corrected_values <- as.matrix(hd_corrected$data$analysis[, predictor_cols])

  ## Values should be different ------------------------------------------------

  expect_false(all(original_values == corrected_values))

})


test_that("standardize() with baseline = FALSE skips correction", {

  hd <- create_test_spectra(n_wavelengths = 100)

  predictor_cols <- hd$data$role_map$variable[
    hd$data$role_map$role == "predictor"
  ]
  original_values <- as.matrix(hd$data$analysis[, predictor_cols])

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  result_values <- as.matrix(hd_result$data$analysis[, predictor_cols])

  expect_equal(original_values, result_values)

})


## =============================================================================
## Provenance Tests
## =============================================================================

test_that("standardize() updates provenance", {

  hd <- create_test_spectra()

  expect_null(hd$provenance$standardization)

  hd_std <- standardize(hd, resample = 2, trim = c(600, 4000),
                        remove_water = TRUE, baseline = TRUE)

  expect_false(is.null(hd_std$provenance$standardization))
  expect_equal(hd_std$provenance$standardization$resample, 2)
  expect_equal(hd_std$provenance$standardization$trim, c(600, 4000))
  expect_true(hd_std$provenance$standardization$remove_water)
  expect_true(hd_std$provenance$standardization$baseline)
  expect_s3_class(hd_std$provenance$standardization$applied_at, "POSIXct")

})


## =============================================================================
## Idempotence Tests
## =============================================================================

test_that("standardize() warns on already-standardized object", {

  hd <- create_test_spectra()

  hd_std <- standardize(hd, resample = NULL, trim = NULL,
                        remove_water = FALSE, baseline = FALSE)

  expect_warning(
    standardize(hd_std),
    "already standardized"
  )

})


test_that("standardize() with force = TRUE re-standardizes", {

  hd <- create_test_spectra(n_wavelengths = 200, wn_min = 400, wn_max = 4500)

  ## First standardization -----------------------------------------------------

  hd_std1 <- standardize(hd, resample = NULL, trim = c(500, 4200),
                         remove_water = FALSE, baseline = FALSE)

  n_after_first <- hd_std1$data$n_predictors

  ## Second standardization with force -----------------------------------------

  expect_warning(
    hd_std2 <- standardize(hd_std1, resample = NULL, trim = c(600, 4000),
                           remove_water = FALSE, baseline = FALSE, force = TRUE),
    "Re-standardizing"
  )

  ## Should have fewer wavelengths after tighter trim --------------------------

  expect_lt(hd_std2$data$n_predictors, n_after_first)

})


## =============================================================================
## Combined Operations Tests
## =============================================================================

test_that("standardize() applies operations in correct order", {

  ## Create data with wide range and coarse resolution -------------------------

  hd <- create_test_spectra(n_wavelengths = 200, wn_min = 400, wn_max = 4500)

  ## Apply all operations ------------------------------------------------------

  hd_full <- standardize(hd,
                         resample     = 2,
                         trim         = c(600, 4000),
                         remove_water = TRUE,
                         baseline     = TRUE)

  ## Check final wavelengths are in expected range -----------------------------

  predictor_cols <- hd_full$data$role_map$variable[
    hd_full$data$role_map$role == "predictor"
  ]
  wavelengths <- as.numeric(gsub("^wn_", "", predictor_cols))

  expect_true(all(wavelengths >= 600))
  expect_true(all(wavelengths <= 4000))

  ## Check water bands are removed ---------------------------------------------
  ## Note: Uses OSSL standard ranges (1580-1720, 3100-3700)

  in_water <- (wavelengths >= 1580 & wavelengths <= 1720) |
              (wavelengths >= 3100 & wavelengths <= 3700)
  expect_false(any(in_water))

  ## Check resolution is approximately 2 cm⁻¹ ----------------------------------

  wn_diff <- abs(diff(sort(wavelengths)))
  median_resolution <- median(wn_diff)
  expect_equal(median_resolution, 2, tolerance = 0.1)

})


test_that("standardize() preserves non-predictor columns", {
 hd <- create_test_spectra()

  ## Add a meta column ---------------------------------------------------------

  hd$data$analysis$batch <- rep("A", nrow(hd$data$analysis))
  hd$data$role_map <- rbind(
    hd$data$role_map,
    tibble::tibble(variable = "batch", role = "meta")
  )

  ## Standardize ---------------------------------------------------------------

  hd_std <- standardize(hd, resample = NULL, trim = NULL,
                        remove_water = FALSE, baseline = FALSE)

  ## Check sample_id preserved -------------------------------------------------

  expect_true("sample_id" %in% names(hd_std$data$analysis))

  ## Check meta column preserved -----------------------------------------------

  expect_true("batch" %in% names(hd_std$data$analysis))
  expect_equal(hd_std$data$analysis$batch, rep("A", nrow(hd_std$data$analysis)))

})


## =============================================================================
## Edge Cases
## =============================================================================

test_that("standardize() handles all parameters as NULL/FALSE", {

  hd <- create_test_spectra()

  ## This should be a no-op but still mark as standardized ---------------------

  hd_result <- standardize(hd, resample = NULL, trim = NULL,
                           remove_water = FALSE, baseline = FALSE)

  ## Data unchanged ------------------------------------------------------------

  expect_equal(hd$data$n_predictors, hd_result$data$n_predictors)

  ## Provenance still set (marks as evaluated) ---------------------------------

  expect_false(is.null(hd_result$provenance$standardization))

})


test_that("standardize() maintains valid horizons_data structure", {

  hd <- create_test_spectra()

  hd_std <- standardize(hd, resample = 2, trim = c(600, 4000),
                        remove_water = TRUE, baseline = TRUE)

  ## Should pass validation (implicitly tested by returning without error) -----

  expect_s3_class(hd_std, "horizons_data")

  ## Check structure -----------------------------------------------------------

  expect_true(!is.null(hd_std$data$analysis))
  expect_true(!is.null(hd_std$data$role_map))
  expect_true(!is.null(hd_std$data$n_predictors))

  ## Check role_map has correct columns ----------------------------------------

  expect_true(all(c("variable", "role") %in% names(hd_std$data$role_map)))

  ## Check all analysis columns are in role_map --------------------------------

  expect_true(all(names(hd_std$data$analysis) %in% hd_std$data$role_map$variable))

})
