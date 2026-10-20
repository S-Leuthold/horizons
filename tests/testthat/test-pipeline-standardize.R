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
    class = "horizons_error_input"
  )

  expect_error(
    standardize(list(a = 1)),
    class = "horizons_error_input"
  )

})


test_that("standardize() validates resample parameter", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, resample = "two"),
    class = "horizons_error_input"
 )

  expect_error(
    standardize(hd, resample = -1),
    class = "horizons_error_input"
  )

  expect_error(
    standardize(hd, resample = c(2, 4)),
    class = "horizons_error_input"
  )

})


test_that("standardize() validates trim parameter", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, trim = 600),
    class = "horizons_error_input"
  )

  expect_error(
    standardize(hd, trim = c("600", "4000")),
    class = "horizons_error_input"
  )

  expect_error(
    standardize(hd, trim = c(4000, 600)),
    class = "horizons_error_input"
  )

  ## A bound that is not finite is refused at the door, not inside seq()
  for (bad in list(c(600, Inf), c(NA, 4000))) {
    expect_error(standardize(hd, trim = bad), "`trim` must be two finite wavenumbers",
                 fixed = TRUE, class = "horizons_error_input")
  }

})


test_that("standardize() validates logical parameters", {

  hd <- create_test_spectra()

  expect_error(
    standardize(hd, remove_water = "yes"),
    class = "horizons_error_input"
  )

  expect_error(
    standardize(hd, baseline = NA),
    class = "horizons_error_input"
  )

  expect_error(
    standardize(hd, force = 1),
    class = "horizons_error_input"
  )

})


test_that("standardize() refuses a trim that misses the data, naming both ranges, before anything is written", {

  hd <- create_test_spectra(wn_min = 600, wn_max = 4000)

  err <- expect_error(standardize(hd, resample = NULL, trim = c(5000, 6000)),
                      "`trim` does not overlap the data's axis", fixed = TRUE,
                      class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))
  expect_match(msg, "`trim` is 5000 to 6000 cm-1; the data covers 600 to 4000 cm-1", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "standardize")

})


test_that("standardize() refuses water-band removal that would leave nothing, and resampling one column, saying what to change", {

  ## An axis wholly inside the 3100-3700 band
  hd <- create_test_spectra(n_wavelengths = 50, wn_min = 3200, wn_max = 3600)

  err <- expect_error(standardize(hd, resample = NULL, trim = NULL, remove_water = TRUE),
                      "Water band removal would delete all wavelength data", fixed = TRUE,
                      class = "horizons_error_input")
  expect_match(conditionMessage(err), "Use `remove_water = FALSE`, or a wider `trim`", fixed = TRUE)
  expect_match(gsub("\\s+", " ", conditionMessage(err)), "(1580 to 1720 and 3100 to 3700 cm-1)", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "standardize")

  ## A spline needs two points: one column cannot be resampled
  one <- spectra(tibble::tibble(sample_id = c("a", "b"), wn_2000 = c(0.1, 0.2)))
  err <- expect_error(standardize(one, resample = 2, trim = NULL),
                       "Resampling requires at least two wavelength columns", fixed = TRUE,
                       class = "horizons_error_input")
  expect_match(conditionMessage(err), "Use `resample = NULL`, or a wider `trim`", fixed = TRUE)

})


test_that("standardize() refuses a resample of zero or below, with one refusal", {

  expect_error(standardize(create_test_spectra(), resample = 0),
               "`resample` must be a positive number or `NULL`, not 0", fixed = TRUE,
               class = "horizons_error_input")
  expect_error(standardize(create_test_spectra(), resample = -1),
               "`resample` must be a positive number or `NULL`, not -1", fixed = TRUE,
               class = "horizons_error_input")

})


## =============================================================================
## Resampling Tests
## =============================================================================

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


test_that("resample_spectra(new_wav =) refuses to extrapolate, as a bug in its caller", {

  ## Every caller builds the grid inside the data, so a grid beyond it is a
  ## broken invariant: without the check the spline would extrapolate
  wn <- seq(4000, 600, by = -4)
  m  <- matrix(stats::runif(2 * length(wn)), nrow = 2)

  expect_error(resample_spectra(m, wn, new_wav = seq(4100, 600, by = -4)),
               regexp = "4100", class = "horizons_error_internal")
  expect_error(resample_spectra(m, wn, new_wav = seq(4000, 500, by = -4)),
               regexp = "500", class = "horizons_error_internal")

})


## =============================================================================
## Canonical Grid Tests (#64, #18)
## =============================================================================
## Synthetic stand-ins for the two sources #64 was found on: a KSSL-shaped
## library (600 to 4000 at 2 cm-1, stored increasing, as the snapshot is) and
## Off-grid scans (from 599.74 at 1.93 cm-1, well past 4000).

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
OFFGRID_WN <- 599.74 + 1.93 * (0:3574)


test_that("a KSSL-shaped axis stored increasing comes out on wn_4000 ... wn_600", {

  kssl <- make_axis_spectra(KSSL_WN)

  at_2 <- no_output(standardize(kssl, resample = 2, trim = c(600, 4000)))
  at_4 <- no_output(standardize(kssl, resample = 4, trim = c(600, 4000)))

  expect_identical(predictor_names(at_2), paste0("wn_", seq(4000, 600, by = -2)))
  expect_identical(predictor_names(at_4), paste0("wn_", seq(4000, 600, by = -4)))
  expect_identical(at_2$data$n_predictors, 1701L)
  expect_identical(at_4$data$n_predictors, 851L)

})


test_that("an off-grid axis lands on the same columns as a KSSL-shaped one", {

  kssl <- no_output(standardize(make_axis_spectra(KSSL_WN), resample = 4, trim = c(600, 4000)))
  offgrid <- no_output(standardize(make_axis_spectra(OFFGRID_WN), resample = 4, trim = c(600, 4000)))

  ## 599.74 sits just below the bound, and it is what lets 600 be a column
  expect_identical(predictor_names(offgrid), predictor_names(kssl))
  expect_identical(predictor_names(offgrid)[c(1, 851)], c("wn_4000", "wn_600"))

  expect_true(offgrid$provenance$standardization$resampled)
  expect_identical(offgrid$provenance$standardization$grid,
                   list(min = 600, max = 4000, step = 4, n = 851L))

})


test_that("grid points the data does not reach are dropped with a warning, never extrapolated", {

  ## Covers 651.5 to 3500: short of the trim bounds at both ends
  short <- make_axis_spectra(seq(3500, 651.5, by = -1.5))

  caught <- list()
  out <- withCallingHandlers(
    no_output(standardize(short, resample = 4, trim = c(600, 4000))),
    horizons_warning_coverage = function(w) {
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


test_that("an uncovered grid and a gap are one coverage warning with two bullets, written to the log first", {

  ## Short of the trim at both ends, with a CO2 band cut out
  short <- seq(3500, 651.5, by = -1.5)
  hd    <- make_axis_spectra(short[short < 2300 | short > 2400])

  w <- expect_warning(out <- no_output(standardize(hd, resample = 4, trim = c(600, 4000))),
                      class = "horizons_warning_coverage")

  msg <- gsub("\\s+", " ", conditionMessage(w))
  expect_match(msg, "2 coverage warnings in `standardize()`", fixed = TRUE)
  expect_match(msg, "does not cover the 4 cm-1 grid", fixed = TRUE)
  expect_match(msg, "gap in it", fixed = TRUE)

  rows <- out$log[out$log$verb == "standardize", ]
  expect_identical(rows$topic, "coverage")
  expect_identical(rows$n, 2L)

})


test_that("a trim window holding no multiple of the resample step is refused", {

  ## Arrange — 1001 to 1003 holds no multiple of 4, so the grid is empty
  kssl <- make_axis_spectra(KSSL_WN)

  ## Act & Assert
  err <- expect_error(
    no_output(standardize(kssl, resample = 4, trim = c(1001, 1003))),
    "No multiple of 4 cm-1 between 1001 and 1003 cm-1", fixed = TRUE,
    class = "horizons_error_input"
  )

  ## The data's own range, not the trimmed one, and what to change (#197)
  msg <- gsub("\\s+", " ", conditionMessage(err))
  expect_match(msg, "The data covers 600 to 4000 cm-1", fixed = TRUE)
  expect_match(msg, "narrower than one 4 cm-1 step: widen `trim` or reduce `resample`", fixed = TRUE)

  ## A window wider than a step: the trim is not the cause, so not named
  ## as it. 1001 to 1003.5 holds no multiple of 4, inside trim = c(600, 4000)
  narrow <- make_axis_spectra(c(1003.5, 1001))
  err <- expect_error(no_output(standardize(narrow, resample = 4)),
                      "No multiple of 4 cm-1 between 600 and 4000 cm-1", fixed = TRUE,
                      class = "horizons_error_input")
  expect_no_match(conditionMessage(err), "narrower than one", fixed = TRUE)

})


test_that("an axis already on the canonical grid is not re-interpolated", {

  kssl <- make_axis_spectra(rev(KSSL_WN))
  before <- as.matrix(kssl$data$analysis[, predictor_names(kssl)])

  ## The console says so: the tree snapshot
  out <- no_output(standardize(kssl, resample = 2, trim = c(600, 4000)))

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
    class  = "horizons_error_validation"
  )

})


test_that("an NA outside the trim range passes spectra() |> standardize(trim = )", {

  ## Arrange — a wide axis with one bad value outside where trim will cut it
  wn <- seq(4500, 600, by = -100)
  df <- tibble::tibble(sample_id = c("s1", "s2"))

  for (w in wn) {

    df[[as.character(w)]] <- c(0.3, 0.4) + w / 1e5

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
  expect_false(".flag" %in% names(out$data$analysis))

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

  offgrid <- make_axis_spectra(OFFGRID_WN, f = smooth_spectrum, n = 1)

  for (res in c(4, 2, 1.5)) {

    out <- no_output(standardize(offgrid, resample = res, trim = c(600, 4000)))
    wn  <- as.numeric(sub("^wn_", "", predictor_names(out)))
    got <- as.numeric(out$data$analysis[1, predictor_names(out)])

    expect_lt(max(abs(got - smooth_spectrum(wn))), 1e-5)

    ## Both ends by name: no end effect from the margin point or the bound
    ends <- c(1, length(wn))
    expect_lt(max(abs(got[ends] - smooth_spectrum(wn[ends]))), 1e-6)

  }

})


test_that("non-integer resolutions give clean, stable column names", {

  offgrid <- make_axis_spectra(OFFGRID_WN)

  ## 1.5 cm-1: 3999, 3997.5, ..., 600
  at_1.5 <- no_output(standardize(offgrid, resample = 1.5, trim = c(600, 4000)))
  expect_identical(predictor_names(at_1.5), paste0("wn_", seq(3999, 600, by = -1.5)))

  ## 0.1 cm-1: the expected names are built from integers, so no float
  ## arithmetic reaches them
  at_0.1 <- no_output(standardize(offgrid, resample = 0.1, trim = c(600, 610)))
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
  offgrid <- no_output(standardize(make_axis_spectra(OFFGRID_WN), resample = NULL, trim = c(600, 4000)))
  wn   <- as.numeric(sub("^wn_", "", predictor_names(offgrid)))
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

  ## A stray NA is kept, not refused: with every operation off the row is
  ## flagged, its value left as it was (the full path is tested below)
  bad <- inc
  bad$data$analysis$wn_1000[1] <- NA_real_
  expect_warning(bad_out <- no_output(standardize(bad, resample = NULL, trim = NULL)),
                 class = "horizons_warning_flagged")
  expect_true(is.na(bad_out$data$analysis$wn_1000[1]))
  expect_identical(bad_out$data$analysis$.flag, c("standardize", NA, NA))

})


test_that("force = TRUE with every operation off keeps the grid an earlier call recorded", {

  on_4 <- no_output(standardize(make_axis_spectra(OFFGRID_WN), resample = 4))

  ## force = TRUE is the user's choice: a note on the log and a line in the
  ## tree, not a warning
  expect_no_warning(
    again <- no_output(standardize(on_4, resample = NULL, trim = NULL, force = TRUE))
  )

  note <- again$log[again$log$verb == "standardize", ]
  expect_identical(note$type, "note")
  expect_match(note$message, "Re-standardizing previously standardized data", fixed = TRUE)

  expect_true(again$provenance$standardization$resampled)
  expect_identical(again$provenance$standardization$grid,
                   on_4$provenance$standardization$grid)
  expect_identical(predictor_names(again), predictor_names(on_4))

  ## The no-op record describes the axis the way the full path does
  expect_identical(again$provenance$standardization$n_wavelengths,
                   on_4$provenance$standardization$n_wavelengths)
  expect_equal(again$provenance$standardization$wavelength_range,
               on_4$provenance$standardization$wavelength_range)

})


test_that("force = TRUE keeps the record of a hull an earlier call fitted, and the range it was fitted over", {

  hulled <- no_output(standardize(make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum),
                                  resample = 4, trim = c(600, 4000), baseline = TRUE))
  expect_true(hulled$provenance$standardization$baseline)
  expect_identical(hulled$provenance$standardization$baseline_range, c(600, 4000))

  ## Every operation off: the values still carry the hull
  again <- no_output(standardize(hulled, resample = NULL, trim = NULL, force = TRUE))
  expect_true(again$provenance$standardization$baseline)
  expect_identical(again$provenance$standardization$baseline_range, c(600, 4000))

  ## A narrower trim without a hull: the hull is the first call's, over its range
  narrower <- no_output(standardize(hulled, resample = 4, trim = c(1000, 3000), force = TRUE))
  expect_true(narrower$provenance$standardization$baseline)
  expect_identical(narrower$provenance$standardization$wavelength_range, c(1000, 3000))
  expect_identical(narrower$provenance$standardization$baseline_range, c(600, 4000))

  ## Without a hull anywhere there is no range
  plain <- no_output(standardize(make_axis_spectra(rev(KSSL_WN)), resample = 4))
  expect_false(plain$provenance$standardization$baseline)
  expect_true("baseline_range" %in% names(plain$provenance$standardization))
  expect_null(plain$provenance$standardization$baseline_range)

})


test_that("standardize() prints nothing under verbose = FALSE, on either path, and refuses a verbose that is not TRUE or FALSE", {

  kssl <- make_axis_spectra(KSSL_WN)

  expect_silent(standardize(kssl, resample = 4, verbose = FALSE))
  expect_silent(standardize(kssl, resample = NULL, trim = NULL, verbose = FALSE))

  expect_error(standardize(kssl, verbose = c(TRUE, FALSE)),
               "`verbose` must be `TRUE` or `FALSE`", fixed = TRUE, class = "horizons_error_input")

})


test_that("standardize()'s tree names each operation that ran, and says when none did", {

  kssl    <- make_axis_spectra(KSSL_WN)
  dec     <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum)
  offgrid <- make_axis_spectra(OFFGRID_WN)
  on_4    <- no_output(standardize(dec, resample = 4))

  expect_snapshot({
    ## Sorted, and nothing else
    x <- standardize(kssl, resample = NULL, trim = NULL, verbose = TRUE)
    x <- standardize(dec, resample = NULL, trim = NULL, verbose = TRUE)
    ## Every operation; the trim counts the columns inside the bounds, not
    ## the interpolation margin
    x <- standardize(dec, resample = 4, remove_water = TRUE, baseline = TRUE, verbose = TRUE)
    x <- standardize(offgrid, resample = 4, trim = c(600, 4000), verbose = TRUE)
    ## Already on the grid, so not re-interpolated
    x <- standardize(dec, resample = 2, trim = c(600, 4000), verbose = TRUE)
    x <- standardize(on_4, resample = NULL, trim = NULL, force = TRUE, verbose = TRUE)
  })

})


test_that("trim = NULL keeps each source's extent, and two sources share names where they overlap", {

  a_wn <- OFFGRID_WN[OFFGRID_WN >= 700 & OFFGRID_WN <= 3000]
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
  gap_warnings <- Filter(function(w) inherits(w, "horizons_warning_coverage"), res$warnings)
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
  expect_true("horizons_warning_coverage" %in% classes)

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

  co2_cut <- OFFGRID_WN[OFFGRID_WN < 2300 | OFFGRID_WN > 2400]
  offgrid <- make_axis_spectra(co2_cut, f = smooth_spectrum, n = 1)

  expect_warning(
    out <- no_output(standardize(offgrid, resample = 4)),
    regexp = "gap",
    class  = "horizons_warning_coverage"
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


test_that("a single sample standardizes as its row of a batch, under every option (#78)", {

  batch  <- make_axis_spectra(OFFGRID_WN)
  single <- subset_rows(batch, "s2")

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

    expect_identical(one$data$n_rows, 1L, info = what)
    expect_identical(predictor_names(one), predictor_names(many), info = what)
    expect_identical(unname(predictor_block(one)), unname(predictor_block(many)[2, , drop = FALSE]),
                     info = what)

  }

})


## =============================================================================
## Spectra standardize() cannot use: kept and flagged
## =============================================================================
## Rows are removed only where modelling happens, so a spectrum standardize()
## cannot use is flagged in .flag, which every verb that computes over
## predictors reads (flagged_rows()), and one flagged warning names it.

#' A batch of smooth spectra on the KSSL axis, with row 2 flat and row 3
#' carrying one missing value
#' @noRd
unusable_batch <- function() {
  hd <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum, n = 4)
  pred <- predictor_names(hd)
  hd$data$analysis[2, pred] <- 0.5
  hd$data$analysis$wn_2000[3] <- NA_real_
  hd
}

test_that("standardize() keeps and flags a flat spectrum and one with a missing value, with one warning", {

  ## Arrange ---------------------------------------------------------------

  hd <- unusable_batch()

  ## Act -------------------------------------------------------------------

  w <- expect_warning(out <- no_output(standardize(hd, resample = NULL, trim = c(600, 4000))),
                      class = "horizons_warning_flagged")

  ## Assert ----------------------------------------------------------------

  ## Every row kept, two flagged
  expect_identical(out$data$analysis$sample_id, paste0("s", 1:4))
  expect_identical(out$data$analysis$.flag, c(NA, "standardize", "standardize", NA))
  expect_identical(out$data$role_map$role[out$data$role_map$variable == ".flag"], "meta")
  expect_identical(flagged_rows(out$data$analysis), c(FALSE, TRUE, TRUE, FALSE))

  ## One warning, counting each reason, written to the log first
  msg <- gsub("\\s+", " ", conditionMessage(w))
  expect_match(msg, "2 spectra could not be used and are flagged (.flag)", fixed = TRUE)
  expect_match(msg, '1 has a value that is not finite: "s3"', fixed = TRUE)
  expect_match(msg, '1 has no spread (every value equal): "s2"', fixed = TRUE)

  row <- out$log[out$log$topic %in% "flagged", ]
  expect_identical(row$n, 2L)
  expect_identical(row$ids[[1]], c("s2", "s3"))

  ## The clean rows are as they standardize alone
  alone <- no_output(standardize(subset_rows(hd, c("s1", "s4")), resample = NULL, trim = c(600, 4000)))
  expect_identical(unname(predictor_block(out)[c(1, 4), ]), unname(predictor_block(alone)))

  ## Valid at the full stage once replicates are averaged: the non-finite
  ## value is in a flagged row
  avg <- average(out, quality_check = FALSE)
  expect_no_error(validate_horizons_data(avg))

})

test_that("the flat spectrum is flagged on the resampled path too, where SNV would turn it into zeros", {

  hd <- unusable_batch()
  hd$data$analysis$wn_2000[3] <- 0.4

  expect_warning(out <- no_output(standardize(hd, resample = 4, trim = c(600, 4000))),
                 "1 has no spread", class = "horizons_warning_flagged")
  expect_identical(out$data$analysis$.flag, c(NA, "standardize", NA, NA))

  ## Resampling keeps a flat spectrum exactly flat
  expect_true(all(predictor_block(out)[2, ] == 0.5))

})

test_that("a spectrum with no finite value is flagged, and the others resampled", {

  ## prospectr fails the whole call on a row with fewer than two finite
  ## values, so that row is left out of the resample
  hd <- make_axis_spectra(OFFGRID_WN, f = smooth_spectrum, n = 3)
  hd$data$analysis[2, predictor_names(hd)] <- NA_real_

  expect_warning(out <- no_output(standardize(hd, resample = 4, trim = c(600, 4000))),
                 class = "horizons_warning_flagged")

  expect_identical(out$data$analysis$.flag, c(NA, "standardize", NA))
  expect_true(all(is.na(predictor_block(out)[2, ])))
  expect_false(anyNA(predictor_block(out)[c(1, 3), ]))

})

test_that("the hull is fitted to the spectra not flagged, and a failure of the hull stops the call", {

  ## Arrange ---------------------------------------------------------------

  hd    <- unusable_batch()
  given <- NULL
  real  <- prospectr::baseline
  local_mocked_bindings(
    baseline = function(X, wav, ...) {
      given <<- nrow(X)
      real(X, wav, ...)
    },
    .package = "prospectr"
  )

  ## Act -------------------------------------------------------------------

  out <- suppressWarnings(no_output(standardize(hd, resample = NULL, trim = c(600, 4000),
                                                baseline = TRUE)))

  ## Assert ----------------------------------------------------------------

  ## prospectr saw the two clean rows only; the flagged ones keep their values
  expect_identical(given, 2L)
  expect_identical(out$data$analysis$.flag, c(NA, "standardize", "standardize", NA))
  expect_true(all(predictor_block(out)[2, ] == 0.5))
  expect_identical(unname(predictor_block(out)[3, ]), unname(predictor_block(hd)[3, ]))

  alone <- no_output(standardize(subset_rows(hd, c("s1", "s4")), resample = NULL,
                                 trim = c(600, 4000), baseline = TRUE))
  expect_identical(unname(predictor_block(out)[c(1, 4), ]), unname(predictor_block(alone)))

  ## A failure on the rest is the run's, with prospectr's as its parent
  local_mocked_bindings(baseline = function(X, wav, ...) stop("hull exploded"),
                        .package = "prospectr")

  err <- expect_error(suppressWarnings(no_output(standardize(hd, resample = NULL, baseline = TRUE))),
                      "Baseline correction failed on the 2 spectra", class = "horizons_error_run")
  expect_match(conditionMessage(err$parent), "hull exploded", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "standardize")

})

test_that("a spectrum with a hole, or short at one end, is flagged rather than filled or extrapolated", {

  ## Arrange ---------------------------------------------------------------

  ## Off-grid spectra; row 2 lacks 61 points mid-axis, row 3 its lowest 40
  hd   <- make_axis_spectra(OFFGRID_WN, f = smooth_spectrum, n = 4)
  pred <- predictor_names(hd)
  wn   <- as.numeric(sub("^wn_", "", pred))
  hd$data$analysis[2, pred[wn > 2000 & wn < 2000 + 61 * 1.93]] <- NA_real_
  hd$data$analysis[3, pred[order(wn)[1:40]]] <- NA_real_

  ## Act -------------------------------------------------------------------

  w <- expect_warning(out <- no_output(standardize(hd, resample = 4, trim = c(600, 4000))),
                      class = "horizons_warning_flagged")

  ## Assert ----------------------------------------------------------------

  expect_identical(out$data$analysis$.flag, c(NA, "standardize", "standardize", NA))
  expect_match(conditionMessage(w), '2 do not reach every point of the grid from their own points, short at an end or with a hole: "s2", "s3"',
               fixed = TRUE)
  expect_no_match(conditionMessage(w), "not finite", fixed = TRUE)

  ## The grid points inside the hole, and below row 3's own start, are NA
  out_wn <- as.numeric(sub("^wn_", "", predictor_names(out)))
  block  <- predictor_block(out)
  expect_true(all(is.na(block[2, out_wn > 2004 & out_wn < 2116])))
  expect_true(all(is.na(block[3, out_wn < min(wn[order(wn)[41]]) - 1e-6])))

  ## Everywhere else they are on the curve, the rows with all their points too
  expect_lt(max(abs(block[2, ] - 2 * smooth_spectrum(out_wn)), na.rm = TRUE), 1e-5)
  expect_lt(max(abs(block[3, ] - 3 * smooth_spectrum(out_wn)), na.rm = TRUE), 1e-5)
  expect_false(anyNA(block[c(1, 4), ]))

})

test_that("a spectrum short of the grid is flagged for that, with the range it covers and trim as the remedy", {

  ## Off-grid and on-grid alike: row 2 starts at 700, as a narrower file's
  ## scans do once bound with wider ones
  for (axis in list(OFFGRID_WN, rev(KSSL_WN))) {

    hd   <- make_axis_spectra(axis, f = smooth_spectrum, n = 2)
    pred <- predictor_names(hd)
    wn   <- as.numeric(sub("^wn_", "", pred))
    hd$data$analysis[2, pred[wn < 700]] <- NA_real_
    lo   <- min(wn[wn >= 700])

    w <- expect_warning(out <- no_output(standardize(hd, resample = 2, trim = c(600, 4000))),
                        class = "horizons_warning_flagged")

    expect_identical(out$data$analysis$.flag, c(NA, "standardize"))

    msg <- gsub("\\s+", " ", conditionMessage(w))
    expect_match(msg, '1 does not reach every point of the grid from its own points, short at an end or with a hole: "s2".',
                 fixed = TRUE)
    expect_match(msg, paste0("The range it covers is ", lo, " to 4000 cm-1; a `trim` inside it keeps it."),
                 fixed = TRUE)
    expect_no_match(msg, "not finite", fixed = TRUE)

    ## The remedy works
    expect_no_warning(out <- no_output(standardize(hd, resample = 2, trim = c(ceiling(lo / 2) * 2, 4000))))
    expect_false(".flag" %in% names(out$data$analysis))

  }

  ## An Inf is a value that is not finite, short of nothing
  hd <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum, n = 2)
  hd$data$analysis$wn_2000[2] <- Inf

  w <- expect_warning(out <- no_output(standardize(hd, resample = 2, trim = c(600, 4000))),
                      class = "horizons_warning_flagged")
  expect_match(conditionMessage(w), '1 has a value that is not finite: "s2".', fixed = TRUE)
  expect_no_match(conditionMessage(w), "reach", fixed = TRUE)

})

test_that("a value missing only inside a water band it removes flags nothing, unless the hull needs it", {

  hd <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum, n = 2)
  hd$data$analysis$wn_1650[2] <- NA_real_

  ## The values returned are all finite
  expect_no_warning(out <- no_output(standardize(hd, resample = 2, trim = c(600, 4000), remove_water = TRUE)))
  expect_false(".flag" %in% names(out$data$analysis))
  expect_false(anyNA(predictor_block(out)))

  ## The hull is fitted across the band, before it is removed
  w <- expect_warning(out <- no_output(standardize(hd, resample = 2, trim = c(600, 4000),
                                                   remove_water = TRUE, baseline = TRUE)),
                      class = "horizons_warning_flagged")
  expect_identical(out$data$analysis$.flag, c(NA, "standardize"))

})

test_that("a spectrum missing the point beyond a trim bound leaves the others' values unchanged, to the bit", {

  hd   <- make_axis_spectra(OFFGRID_WN, f = smooth_spectrum, n = 3)
  pred <- predictor_names(hd)

  ## Row 2 lacks 599.74, the point beyond 600, so it reaches for 597.81:
  ## standardized alone it would; the others must not use it
  hd$data$analysis$wn_599.74[2] <- NA_real_
  hd <- set_analysis(hd, dplyr::bind_cols(tibble::tibble(sample_id = hd$data$analysis$sample_id, wn_597.81 = 1:3 / 3),
                                          hd$data$analysis[, pred]),
                     tibble::tibble(variable = c("sample_id", "wn_597.81", pred),
                                    role = c("id", rep("predictor", length(pred) + 1))))
  hd <- sort_axis_decreasing(hd)$x

  batch  <- no_output(standardize(hd, resample = 4, trim = c(600, 4000)))
  others <- no_output(standardize(subset_rows(hd, c("s1", "s3")), resample = 4, trim = c(600, 4000)))

  expect_identical(predictor_block(batch)[c(1, 3), ], predictor_block(others))
  expect_false(".flag" %in% names(batch$data$analysis))
  expect_false(anyNA(predictor_block(batch)))

})

test_that("unusable_spectra() gives one rule over a matrix or a data frame, and judges nothing with no columns", {

  M <- rbind(c(1, 2, 3), c(5, 5, 5), c(1, NA, 3), c(Inf, 2, 3))

  expect_identical(unusable_spectra(M), c(NA, "no spread", "non-finite", "non-finite"))
  expect_identical(unusable_spectra(as.data.frame(M)), unusable_spectra(M))

  ## One value has no spread to judge; no columns, nothing to judge
  expect_identical(unusable_spectra(M[, 1, drop = FALSE]), c(NA, NA, NA, "non-finite"))
  expect_identical(unusable_spectra(M[, 0, drop = FALSE]), rep(NA_character_, 4))

})

test_that("a point standing alone between two holes in a spectrum carries the grid point it sits on", {

  ## On a 2 cm-1 axis resampled to 4, row 2 has holes either side of 2000
  hd   <- make_axis_spectra(rev(KSSL_WN), f = smooth_spectrum, n = 2)
  pred <- predictor_names(hd)
  wn   <- as.numeric(sub("^wn_", "", pred))
  hd$data$analysis[2, pred[(wn > 1900 & wn < 2000) | (wn > 2000 & wn < 2100)]] <- NA_real_

  out <- suppressWarnings(no_output(standardize(hd, resample = 4, trim = c(600, 4000))))

  ## Its own value at 2000, unmoved; the grid points in its holes NA
  expect_identical(out$data$analysis$wn_2000[2], hd$data$analysis$wn_2000[2])
  expect_true(all(is.na(unlist(out$data$analysis[2, c("wn_1996", "wn_2004")]))))
  expect_identical(out$data$analysis$.flag, c(NA, "standardize"))

})

test_that("a single missing point is interpolated over, not flagged", {

  hd   <- make_axis_spectra(OFFGRID_WN, f = smooth_spectrum, n = 2)
  pred <- predictor_names(hd)
  hd$data$analysis[[pred[1000]]][2] <- NA_real_

  expect_no_warning(out <- no_output(standardize(hd, resample = 4, trim = c(600, 4000))))

  out_wn <- as.numeric(sub("^wn_", "", predictor_names(out)))
  expect_false(".flag" %in% names(out$data$analysis))
  expect_lt(max(abs(predictor_block(out)[2, ] - 2 * smooth_spectrum(out_wn))), 1e-5)

})

test_that("spectra read from files on different grids are each resampled from their own points", {

  ## Arrange ---------------------------------------------------------------

  ## Two scans at 1.93 cm-1 and one at 3.86, bound into one table: on the
  ## union axis each row is NA at the other grid's points
  fine   <- OFFGRID_WN
  coarse <- 600.5 + 3.86 * (0:900)
  union  <- sort(unique(c(fine, coarse)), decreasing = TRUE)

  m <- matrix(NA_real_, nrow = 3, ncol = length(union), dimnames = list(NULL, paste0("wn_", union)))
  m[1, match(fine, union)]   <- smooth_spectrum(fine)
  m[2, match(fine, union)]   <- 2 * smooth_spectrum(fine)
  m[3, match(coarse, union)] <- 3 * smooth_spectrum(coarse)

  hd <- new_horizons_data(
    analysis = dplyr::bind_cols(tibble::tibble(sample_id = c("f1", "f2", "c1")), tibble::as_tibble(m)),
    role_map = tibble::tibble(variable = c("sample_id", colnames(m)),
                              role     = c("id", rep("predictor", ncol(m)))))

  ## Act -------------------------------------------------------------------

  out <- no_output(standardize(hd, resample = 4, trim = c(700, 3800)))

  ## Assert ----------------------------------------------------------------

  ## Nothing flagged, and each row is what its own scan standardizes to alone
  expect_false(".flag" %in% names(out$data$analysis))

  alone <- function(wn, values, id) {
    mm <- matrix(values, nrow = 1, dimnames = list(NULL, paste0("wn_", wn)))
    no_output(standardize(new_horizons_data(
      analysis = dplyr::bind_cols(tibble::tibble(sample_id = id), tibble::as_tibble(mm)),
      role_map = tibble::tibble(variable = c("sample_id", colnames(mm)),
                                role     = c("id", rep("predictor", ncol(mm))))),
      resample = 4, trim = c(700, 3800)))
  }

  expect_identical(unname(predictor_block(out)[3, ]),
                   unname(predictor_block(alone(coarse, 3 * smooth_spectrum(coarse), "c1"))[1, ]))
  expect_identical(unname(predictor_block(out)[1:2, ]),
                   unname(predictor_block(no_output(standardize(subset_rows(
                     make_axis_spectra(fine, f = smooth_spectrum, n = 2), c("s1", "s2")),
                     resample = 4, trim = c(700, 3800))))))

})

test_that("a row an earlier step flagged keeps its flag and is not counted again", {

  hd <- unusable_batch()
  hd$data$analysis$.flag <- c(NA, "average", NA, NA)
  hd$data$role_map <- dplyr::bind_rows(tibble::tibble(variable = ".flag", role = "meta"),
                                       hd$data$role_map)

  w <- expect_warning(out <- no_output(standardize(hd, resample = NULL, trim = c(600, 4000))),
                      class = "horizons_warning_flagged")

  expect_identical(out$data$analysis$.flag, c(NA, "average", "standardize", NA))
  expect_match(conditionMessage(w), "1 spectrum could not be used", fixed = TRUE)

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


## =============================================================================
## Idempotence Tests
## =============================================================================

test_that("standardize() warns on already-standardized object, once, after writing it to the log", {

  hd <- create_test_spectra()

  hd_std <- standardize(hd, resample = NULL, trim = NULL,
                        remove_water = FALSE, baseline = FALSE)

  expect_warning(
    again <- standardize(hd_std),
    "already standardized", class = "horizons_warning_skipped"
  )

  ## The object is unchanged but for its log
  row <- again$log[again$log$verb == "standardize", ]
  expect_identical(row$topic, "skipped")
  again$log <- hd_std$log
  expect_identical(again, hd_std)

  ## Recorded whether or not the warning is seen
  quiet <- suppressWarnings(standardize(hd_std))
  expect_identical(quiet$log$topic[quiet$log$verb == "standardize"], "skipped")

})

test_that("standardize() validates an already-standardized object before returning it (#234)", {

  hd_std <- standardize(create_test_spectra(), resample = NULL, trim = NULL,
                        remove_water = FALSE, baseline = FALSE)

  ## A column with no role_map row, as a hand edit leaves it
  hd_std$data$analysis$stray <- 1

  err <- expect_error(standardize(hd_std), class = "horizons_error_validation")
  expect_true(any(grepl("stray", err$findings, fixed = TRUE)))

})


## =============================================================================
## Combined Operations Tests
## =============================================================================

test_that("standardize() with every operation on trims, drops the water bands, resamples, and records its arguments", {

  ## Create data with wide range and coarse resolution -------------------------

  hd <- create_test_spectra(n_wavelengths = 200, wn_min = 400, wn_max = 4500)

  expect_null(hd$provenance$standardization)

  ## Apply all operations ------------------------------------------------------

  hd_full <- standardize(hd,
                         resample     = 2,
                         trim         = c(600, 4000),
                         remove_water = TRUE,
                         baseline     = TRUE)

  ## Check the provenance records each argument --------------------------------

  std <- hd_full$provenance$standardization
  expect_false(is.null(std))
  expect_equal(std$resample, 2)
  expect_equal(std$trim, c(600, 4000))
  expect_true(std$remove_water)
  expect_true(std$baseline)
  expect_s3_class(std$applied_at, "POSIXct")

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
