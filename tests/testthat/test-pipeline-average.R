## ---------------------------------------------------------------------------
## Tests for pipeline-average.R: average() function
## ---------------------------------------------------------------------------

library(testthat)
library(horizons)


## ---------------------------------------------------------------------------
## Helper: Create horizons_data for testing
## ---------------------------------------------------------------------------

#' Create horizons_data object with replicate spectra for testing
#'
#' @param n_samples Number of unique samples.
#' @param n_reps Number of replicates per sample.
#' @param n_wavelengths Number of wavelength columns.
#' @param add_outlier Logical. Add an outlier replicate to sample 1.
#' @param all_outliers Logical. Make all replicates of sample 1 outliers.
#' @param add_meta Logical. Add metadata columns.
#' @param uniform_meta Logical. Make metadata uniform within samples.
#' @return A horizons_data object.
#' @noRd
make_test_hd_average <- function(n_samples      = 3,
                                 n_reps         = 3,
                                 n_wavelengths  = 10,
                                 add_outlier    = FALSE,
                                 all_outliers   = FALSE,
                                 add_meta       = FALSE,
                                 uniform_meta   = TRUE,
                                 add_responses  = FALSE,
                                 conflict_texture = FALSE) {

  set.seed(42)

  ## Build sample IDs and filenames ------------------------------------------

  sample_ids <- rep(paste0("S", seq_len(n_samples)), each = n_reps)
  filenames <- paste0(sample_ids, "_rep", rep(seq_len(n_reps), n_samples))
  n_total <- n_samples * n_reps

  ## Build spectral data (correlated within samples) -------------------------

  ## Decreasing, per invariant I2 (MIR convention: 4000 -> 400). The validator
  ## enforces this now, so the fixture has to honour it.
  wn_cols <- paste0("wn_", rev(seq_len(n_wavelengths)) * 100)

  ## Base spectra per sample (rows = samples)
  base_spectra <- matrix(
    runif(n_samples * n_wavelengths, 0.5, 1.5),
    nrow = n_samples, ncol = n_wavelengths
  )

  ## Replicate with small noise
  spectra_data <- matrix(nrow = n_total, ncol = n_wavelengths)

  for (i in seq_len(n_samples)) {

    for (j in seq_len(n_reps)) {

      row_idx <- (i - 1) * n_reps + j
      spectra_data[row_idx, ] <- base_spectra[i, ] + rnorm(n_wavelengths, 0, 0.001)

    }

  }

  ## Add outlier if requested ------------------------------------------------

  if (add_outlier && !all_outliers) {

    ## Make first replicate of first sample an outlier
    ## Use completely different pattern (inverted + offset) to ensure low correlation
    spectra_data[1, ] <- 2 - base_spectra[1, ] + rnorm(n_wavelengths, 0, 0.01)

  }

  if (all_outliers) {

    ## Make all replicates of first sample outliers (uncorrelated with each other)
    for (j in seq_len(n_reps)) {

      ## Each replicate gets a completely random pattern
      spectra_data[j, ] <- runif(n_wavelengths, 0, 2)

    }

  }

  ## Build analysis tibble ---------------------------------------------------

  analysis <- tibble::tibble(
    sample_id = sample_ids,
    filename  = filenames
  )

  ## Add metadata if requested -----------------------------------------------

  if (add_meta) {

    if (uniform_meta) {

      ## Uniform within samples
      analysis$project <- rep(paste0("PROJ", seq_len(n_samples)), each = n_reps)
      analysis$site <- rep(c("SiteA", "SiteB", "SiteC")[seq_len(n_samples)], each = n_reps)

    } else {

      ## Non-uniform (different per replicate)
      analysis$project <- paste0("PROJ", seq_len(n_total))
      analysis$rep_num <- rep(seq_len(n_reps), n_samples)

    }

  }

  ## Add response and outcome columns if requested ---------------------------

  if (add_responses) {

    ## clay varies a little between replicates of a sample, so the collapse
    ## has to be a mean rather than a first-value pick; texture is a
    ## non-numeric lab call that should be uniform within a sample.
    analysis$clay    <- rep(seq_len(n_samples) * 10, each = n_reps) +
                        rep(seq_len(n_reps) - 1, n_samples)
    analysis$soc     <- rep(seq_len(n_samples), each = n_reps) + 0.5
    analysis$texture <- rep(c("silt", "loam", "clay")[seq_len(n_samples)], each = n_reps)

    if (conflict_texture) {

      analysis$texture[1] <- "sand"

    }

  }

  ## Add spectral columns ----------------------------------------------------

  for (i in seq_len(n_wavelengths)) {

    analysis[[wn_cols[i]]] <- spectra_data[, i]

  }

  ## Build role_map ----------------------------------------------------------

  meta_vars     <- intersect(c("filename", "project", "site", "rep_num"), names(analysis))
  response_vars <- intersect(c("clay", "texture"), names(analysis))
  outcome_vars  <- intersect("soc", names(analysis))

  role_map <- tibble::tibble(
    variable = c("sample_id", meta_vars, response_vars, outcome_vars, wn_cols),
    role     = c("id",
                 rep("meta",      length(meta_vars)),
                 rep("response",  length(response_vars)),
                 rep("outcome",   length(outcome_vars)),
                 rep("predictor", n_wavelengths))
  )

  ## Build horizons_data -----------------------------------------------------

  new_horizons_data(analysis = analysis, role_map = role_map,
                    spectra_source = "test", spectra_type = "opus")

}


## ---------------------------------------------------------------------------
## average() — Basic functionality
## ---------------------------------------------------------------------------

test_that("average() reduces rows to number of unique samples", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 3, n_reps = 3)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_s3_class(result, "horizons_data")
  expect_equal(nrow(result$data$analysis), 3)
  expect_equal(result$data$analysis$sample_id, c("S1", "S2", "S3"))

})

test_that("average() computes mean of predictor columns", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 1, n_reps = 3, n_wavelengths = 2)

  ## Get original values for first wavelength column
  wn_col <- "wn_100"
  original_values <- hd$data$analysis[[wn_col]]
  expected_mean <- mean(original_values)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis[[wn_col]], expected_mean, tolerance = 1e-10)

})


## ---------------------------------------------------------------------------
## average() — Quality control
## ---------------------------------------------------------------------------

test_that("average() removes outlier replicate when QC enabled", {

 ## Arrange ---------------------------------------------------------------

  ## With iterative removal, the outlier is removed first, then the good
  ## replicates pass easily with the default threshold
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_outlier = TRUE)

  ## Act -------------------------------------------------------------------

  ## Default threshold works now thanks to iterative removal
  result <- average(hd, quality_check = TRUE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## Should still have 2 samples
  expect_equal(nrow(result$data$analysis), 2)

  ## Provenance should show 1 dropped replicate (the outlier from sample 1)
  expect_equal(result$provenance$average$n_replicates_dropped, 1)

})

test_that("average() with quality_check=FALSE keeps all replicates", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_outlier = TRUE)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## Provenance should show no dropped replicates
  expect_equal(result$provenance$average$n_replicates_dropped, 0)

})

test_that("average() single replicate groups skip QC", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 3, n_reps = 1)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = TRUE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_equal(nrow(result$data$analysis), 3)
  expect_equal(result$provenance$average$n_replicates_dropped, 0)

})


## ---------------------------------------------------------------------------
## average() — All outliers handling
## ---------------------------------------------------------------------------

test_that("average() on_all_outliers='warn' keeps all and warns", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  ## Act -------------------------------------------------------------------

  w <- expect_warning(
    result <- average(hd, quality_check = TRUE, on_all_outliers = "warn", verbose = FALSE),
    "All replicates failed QC for 1 sample, averaged anyway.", fixed = TRUE,
    class = "horizons_warning_quality"
  )

  ## Recorded on the object before it was raised, naming the sample
  row <- result$log[result$log$verb == "average", ]
  expect_identical(row$topic, "quality")
  expect_identical(row$ids[[1]], "S1")
  expect_identical(w$log, row)

  ## Assert ----------------------------------------------------------------

  ## Sample 1 should still be in results
  expect_true("S1" %in% result$data$analysis$sample_id)

  ## The sample is on the log, not in provenance (#231)
  expect_null(result$provenance$average$samples_all_outliers)

})

test_that("average() names every sample whose replicates all failed, on the log, in every mode (#231)", {

  ## Two of three samples fail on every replicate
  hd <- make_test_hd_average(n_samples = 3, n_reps = 3, all_outliers = TRUE)
  set.seed(7)
  for (col in grep("^wn_", names(hd$data$analysis), value = TRUE)) {
    hd$data$analysis[[col]][4:6] <- stats::runif(3, 0, 2)
  }

  for (mode in c("warn", "keep_best", "drop")) {

    result <- suppressWarnings(average(hd, on_all_outliers = mode, verbose = FALSE))
    row    <- result$log[result$log$verb == "average", ]

    expect_identical(nrow(row), 1L, info = mode)
    expect_setequal(row$ids[[1]], c("S1", "S2"))
    expect_identical(row$type, if (mode == "warn") "warning" else "note", info = mode)

  }

})

test_that("average() on_all_outliers='drop' removes sample", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## Sample 1 should be dropped
  expect_false("S1" %in% result$data$analysis$sample_id)
  expect_equal(nrow(result$data$analysis), 1)

  ## Dropped as asked: a note naming it, not a warning, and not in
  ## provenance (#231)
  expect_no_warning(average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE))
  row <- result$log[result$log$verb == "average", ]
  expect_identical(row$type, "note")
  expect_identical(row$ids[[1]], "S1")
  expect_match(row$message, 'on_all_outliers = "drop"` dropped', fixed = TRUE)
  expect_null(result$provenance$average$samples_dropped)

})

test_that("average() on_all_outliers='keep_best' keeps the replicate with the highest mean correlation", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = TRUE, on_all_outliers = "keep_best", verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## Sample 1 should still exist (with 1 replicate)
  expect_true("S1" %in% result$data$analysis$sample_id)

  ## Should have dropped 2 replicates from S1
  expect_equal(result$provenance$average$n_replicates_dropped, 2)

  ## Kept as asked: a note naming the sample, not a warning (#231)
  expect_no_warning(average(hd, quality_check = TRUE, on_all_outliers = "keep_best", verbose = FALSE))
  row <- result$log[result$log$verb == "average", ]
  expect_identical(row$type, "note")
  expect_identical(row$ids[[1]], "S1")

  ## S1 is exactly the scan whose mean correlation with the other two is
  ## highest, measured before QC removes any
  s1_result <- result$data$analysis[result$data$analysis$sample_id == "S1", ]
  wn_cols   <- grep("^wn_", names(s1_result), value = TRUE)
  scans     <- as.matrix(hd$data$analysis[1:3, wn_cols])
  cm        <- stats::cor(t(scans))
  diag(cm)  <- NA
  best      <- which.max(rowMeans(cm, na.rm = TRUE))
  expect_equal(unlist(s1_result[, wn_cols]), scans[best, ])

})

test_that("average() under keep_best keeps the best replicate, not the one QC's removal leaves last", {

  ## Arrange — A and B correlate well, C correlates with A alone: their mean
  ## correlations are 0.713, 0.661 and 0.387. The removal takes C, then one
  ## of A and B, whose last correlation ties, and used to leave B
  p  <- 50
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, n_wavelengths = p)
  wn <- grep("^wn_", names(hd$data$analysis), value = TRUE)

  set.seed(1)
  z1 <- rnorm(p); z2 <- rnorm(p); z3 <- rnorm(p)
  A  <- z1 + 0.10 * z2
  B  <- z1 + 0.10 * z3
  C  <- z1 * 0.4 + 0.9 * (0.9 * z2 + 0.1 * rnorm(p))
  for (k in seq_len(p)) hd$data$analysis[[wn[k]]][1:3] <- c(A[k], B[k], C[k])

  cm <- stats::cor(t(rbind(A, B, C)))
  diag(cm) <- NA
  expect_equal(unname(round(rowMeans(cm, na.rm = TRUE), 3)), c(0.713, 0.661, 0.387))

  ## Act
  result <- average(hd, on_all_outliers = "keep_best", verbose = FALSE)

  ## Assert — S1 is A
  s1 <- result$data$analysis[result$data$analysis$sample_id == "S1", wn]
  expect_equal(unname(unlist(s1)), A)

})

test_that("average() errors when all samples dropped", {

  ## Arrange ---------------------------------------------------------------

  ## Single sample with all outliers
  hd <- make_test_hd_average(n_samples = 1, n_reps = 3, all_outliers = TRUE)

  ## Act & Assert ----------------------------------------------------------

  err <- expect_error(
    average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE),
    "Every sample failed quality control on every replicate", fixed = TRUE,
    class = "horizons_error_qc"
  )
  expect_s3_class(err, "horizons_error_run")

  ## Nothing printed beside it, and the samples travel on it
  expect_silent(tryCatch(average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE),
                         error = function(e) NULL))
  expect_identical(err$results, tibble::tibble(sample_id = "S1", n_replicates = 3L))
  expect_snapshot(average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE),
                  error = TRUE, cnd_class = TRUE)

})


## ---------------------------------------------------------------------------
## average() — The QC report counts a group that failed QC (#89)
## ---------------------------------------------------------------------------
## Under "warn" a group whose every replicate failed QC is averaged with all
## its replicates, so it drops none. The report counted clean groups as the
## groups without drops, and printed "all 2 groups clean" under the warning
## that said the group had failed. Each mode's report has to say what it did.

## The QC sub-tree of average()'s console output
qc_report <- function(out) {

  out[grepl("QC \\(r >|^│  │  ", out)]

}

test_that("average() on_all_outliers='warn' reports the failed group as averaged anyway, not clean (#89)", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  ## Act -------------------------------------------------------------------

  expect_warning(
    out <- utils::capture.output(average(hd, verbose = TRUE, on_all_outliers = "warn")),
    "All replicates failed QC"
  )

  ## Assert ----------------------------------------------------------------

  report <- qc_report(out)

  expect_false(any(grepl("all 2 groups clean", out, fixed = TRUE)))
  expect_identical(report, c(
    "│  ├─ QC (r > 0.996)",
    "│  │  ├─ 1/2 groups clean",
    "│  │  ├─ 1 group failed QC on every replicate → averaged anyway (on_all_outliers = 'warn')",
    "│  │  └─ 2 samples retained"
  ))

})

test_that("average() on_all_outliers='keep_best' reports the failed group and the replicates it lost (#89)", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  ## Act -------------------------------------------------------------------

  out <- utils::capture.output(average(hd, verbose = TRUE, on_all_outliers = "keep_best"))

  ## Assert ----------------------------------------------------------------

  expect_identical(qc_report(out), c(
    "│  ├─ QC (r > 0.996)",
    "│  │  ├─ 1/2 groups clean",
    "│  │  ├─ 1 group failed QC on every replicate → 1 replicate kept, 2 removed (on_all_outliers = 'keep_best')",
    "│  │  └─ 2 samples retained"
  ))

})

test_that("average() on_all_outliers='drop' reports the failed group as dropped (#89)", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  ## Act -------------------------------------------------------------------

  out <- utils::capture.output(average(hd, verbose = TRUE, on_all_outliers = "drop"))

  ## Assert ----------------------------------------------------------------

  expect_identical(qc_report(out), c(
    "│  ├─ QC (r > 0.996)",
    "│  │  ├─ 1/2 groups clean",
    "│  │  ├─ 1 group failed QC on every replicate → dropped, 3 replicates removed (on_all_outliers = 'drop')",
    "│  │  └─ 1 sample retained"
  ))

})

test_that("average() reports a partly failed group apart from a wholly failed one (#89)", {

  ## Arrange ---------------------------------------------------------------

  ## S1: every replicate fails. S2: one replicate is the mirror image of a
  ## sibling, so it alone fails. S3: clean.
  hd      <- make_test_hd_average(n_samples = 3, n_reps = 3, all_outliers = TRUE)
  wn_cols <- grep("^wn_", names(hd$data$analysis), value = TRUE)
  hd$data$analysis[4, wn_cols] <- as.list(2 - unlist(hd$data$analysis[5, wn_cols]))

  ## Act -------------------------------------------------------------------

  expect_warning(
    out <- utils::capture.output(average(hd, verbose = TRUE, on_all_outliers = "warn")),
    "All replicates failed QC"
  )

  ## Assert ----------------------------------------------------------------

  ## One line per kind of group, adding up to the three groups
  expect_identical(qc_report(out), c(
    "│  ├─ QC (r > 0.996)",
    "│  │  ├─ 1/3 groups clean",
    "│  │  ├─ 1 group → 1 replicate removed",
    "│  │  ├─ 1 group failed QC on every replicate → averaged anyway (on_all_outliers = 'warn')",
    "│  │  └─ 3 samples retained"
  ))

})

test_that("average() counts every wholly failed group, in each on_all_outliers mode (#89)", {

  ## Arrange ---------------------------------------------------------------

  ## S1 and S2: every replicate a random pattern, so each fails QC whole.
  ## S3: clean. The keep_best line once said "one replicate kept" whatever
  ## the number of groups.
  hd      <- make_test_hd_average(n_samples = 3, n_reps = 3, all_outliers = TRUE)
  wn_cols <- grep("^wn_", names(hd$data$analysis), value = TRUE)

  set.seed(7)

  for (row in 4:6) {
    hd$data$analysis[row, wn_cols] <- as.list(runif(length(wn_cols), 0, 2))
  }

  expected <- list(
    warn      = c("│  │  ├─ 2 groups failed QC on every replicate → averaged anyway (on_all_outliers = 'warn')",
                  "│  │  └─ 3 samples retained"),
    keep_best = c("│  │  ├─ 2 groups failed QC on every replicate → 2 replicates kept, 4 removed (on_all_outliers = 'keep_best')",
                  "│  │  └─ 3 samples retained"),
    drop      = c("│  │  ├─ 2 groups failed QC on every replicate → dropped, 6 replicates removed (on_all_outliers = 'drop')",
                  "│  │  └─ 1 sample retained")
  )

  for (mode in names(expected)) {

    ## Act -----------------------------------------------------------------

    out <- utils::capture.output(
      result <- suppressWarnings(average(hd, on_all_outliers = mode, verbose = TRUE))
    )

    ## Assert --------------------------------------------------------------

    expect_identical(qc_report(out), c(
      "│  ├─ QC (r > 0.996)",
      "│  │  ├─ 1/3 groups clean",
      expected[[mode]]
    ), info = mode)

    expect_identical(nrow(result$data$analysis), if (mode == "drop") 1L else 3L,
                     info = mode)

  }

})

test_that("average() reports single-replicate groups as not QC'd, not clean (#89)", {

  ## Arrange ---------------------------------------------------------------

  ## S3 keeps one scan of three: nothing to correlate it against
  hd <- make_test_hd_average(n_samples = 3, n_reps = 3)
  hd$data$analysis <- hd$data$analysis[-c(8, 9), ]
  hd$data$n_rows   <- nrow(hd$data$analysis)

  ## Act -------------------------------------------------------------------

  out <- utils::capture.output(average(hd, verbose = TRUE))

  ## Assert ----------------------------------------------------------------

  expect_false(any(grepl("all 3 groups clean", out, fixed = TRUE)))
  expect_identical(qc_report(out), c(
    "│  ├─ QC (r > 0.996)",
    "│  │  ├─ 2/3 groups clean",
    "│  │  ├─ 1 group with fewer than two usable replicates (not QC'd)",
    "│  │  └─ 3 samples retained"
  ))

})

test_that("average() still reports all groups clean when none failed", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 3, n_reps = 3)

  ## Act -------------------------------------------------------------------

  out <- utils::capture.output(average(hd, verbose = TRUE))

  ## Assert ----------------------------------------------------------------

  expect_identical(qc_report(out),
                   "│  ├─ QC (r > 0.996): all 3 groups clean")

})


## ---------------------------------------------------------------------------
## average() — Metadata handling
## ---------------------------------------------------------------------------

test_that("average() is quiet under horizons.verbose = FALSE, and prints when asked", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  withr::local_options(horizons.verbose = FALSE)
  expect_silent(average(hd, quality_check = FALSE))
  expect_output(average(hd, quality_check = FALSE, verbose = TRUE), "Averaging replicates")

  withr::local_options(horizons.verbose = TRUE)
  expect_output(average(hd, quality_check = FALSE), "Averaging replicates")

})


test_that("average() preserves uniform metadata", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE, uniform_meta = TRUE)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_true("project" %in% names(result$data$analysis))
  expect_true("site" %in% names(result$data$analysis))
  expect_equal(result$data$analysis$project, c("PROJ1", "PROJ2"))

})

test_that("average() keeps a uniform metadata column's value when the first replicate lacks it", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE)
  hd$data$analysis$site[1] <- NA
  hd$data$analysis$site[4:6] <- NA

  out <- average(hd, verbose = FALSE)$data$analysis

  ## and stays NA where no replicate has it
  expect_identical(out$site, c("SiteA", NA))

})

test_that("average() drops non-uniform metadata", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE, uniform_meta = FALSE)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## rep_num should be dropped (different per replicate)
  expect_false("rep_num" %in% names(result$data$analysis))

  ## project should be dropped (different per row)
  expect_false("project" %in% names(result$data$analysis))

  ## Named in a note on the log, not in provenance
  row <- result$log[result$log$verb == "average", ]
  expect_identical(row$type, "note")
  expect_setequal(row$ids[[1]], c("project", "rep_num"))
  expect_null(result$provenance$average$meta_columns_dropped)

})

test_that("average() drops a metadata column that varies within any sample from every row (#226)", {

  ## Arrange — site is uniform within S1 and S2, but varies within S3
  hd <- make_test_hd_average(n_samples = 3, n_reps = 3, add_meta = TRUE)
  hd$data$analysis$site[9] <- "SiteZ"

  ## Act
  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert — gone from every row, not kept for S1 and S2 and NA for S3
  expect_false("site" %in% names(result$data$analysis))
  expect_false("site" %in% result$data$role_map$variable)
  expect_true("project" %in% names(result$data$analysis))

  row <- result$log[result$log$verb == "average", ]
  expect_identical(row$ids[[1]], "site")
  expect_no_error(validate_horizons_data(result))

})

test_that("average() under keep_best keeps the first of two replicates QC cannot tell apart (#109)", {

  ## Arrange — two uncorrelated replicates: both mean correlations are their
  ## one correlation
  hd <- make_test_hd_average(n_samples = 2, n_reps = 2, all_outliers = TRUE)

  ## Act
  result <- average(hd, on_all_outliers = "keep_best", verbose = FALSE)

  ## Assert — S1 is its first scan, exactly
  wn  <- grep("^wn_", names(hd$data$analysis), value = TRUE)
  s1  <- result$data$analysis[result$data$analysis$sample_id == "S1", wn]
  expect_equal(unlist(s1), unlist(hd$data$analysis[1, wn]))

})

test_that("average() leaves a flagged replicate out of QC and the mean, and flags a sample only when every replicate is (#226)", {

  ## Arrange — S2's first scan is flagged, though QC would pass it: it is
  ## its second scan shifted, so averaged in it would move the mean; every
  ## scan of S3 is flagged
  hd  <- make_test_hd_average(n_samples = 3, n_reps = 3)
  wn  <- grep("^wn_", names(hd$data$analysis), value = TRUE)
  hd$data$analysis[4, wn] <- hd$data$analysis[5, wn] + 0.3
  hd$data$analysis$.flag  <- c(NA, NA, NA, "standardize", NA, NA, rep("standardize", 3))
  hd$data$role_map <- tibble::add_row(hd$data$role_map, variable = ".flag", role = "meta")

  ## Act — no warning: standardize() warned about those spectra
  expect_no_warning(result <- average(hd, verbose = FALSE))
  out <- result$data$analysis

  ## Assert — S2 is exactly the mean of its two clean scans, unflagged
  expect_identical(unname(unlist(out[out$sample_id == "S2", wn])),
                   unname(vapply(hd$data$analysis[5:6, wn], mean, numeric(1))))
  expect_true(is.na(out$.flag[out$sample_id == "S2"]))

  ## S3 is one row, flagged with the step, and validates
  expect_identical(out$.flag[out$sample_id == "S3"], "standardize")
  expect_no_error(validate_horizons_data(result))

  ## Nothing on the log, and the tree counts both
  expect_identical(nrow(result$log[result$log$verb == "average", ]), 0L)
  expect_output(average(hd, verbose = TRUE),
                "Flagged (.flag): 1 replicate left out, 1 sample kept flagged", fixed = TRUE)

})

test_that("average() writes no .flag column when no sample is flagged", {

  ## A flagged replicate among clean ones leaves its sample unflagged; it is
  ## one QC would pass, so only the flag keeps it out of S1's mean
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3)
  wn <- grep("^wn_", names(hd$data$analysis), value = TRUE)
  hd$data$analysis[1, wn] <- hd$data$analysis[2, wn] + 0.3
  hd$data$analysis$.flag <- c("standardize", rep(NA, 5))
  hd$data$role_map <- tibble::add_row(hd$data$role_map, variable = ".flag", role = "meta")

  result <- average(hd, verbose = FALSE)

  s1 <- result$data$analysis[result$data$analysis$sample_id == "S1", wn]
  expect_identical(unname(unlist(s1)),
                   unname(vapply(hd$data$analysis[2:3, wn], mean, numeric(1))))
  expect_false(".flag" %in% names(result$data$analysis))
  expect_false(".flag" %in% result$data$role_map$variable)
  expect_identical(nrow(result$log[result$log$verb == "average", ]), 0L)

  ## and none when the input had none
  expect_false(".flag" %in% names(average(make_test_hd_average(), verbose = FALSE)$data$analysis))

})

test_that("average() always drops filename column", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_false("filename" %in% names(result$data$analysis))

  ## Dropped by design on every call, so nothing is logged
  expect_identical(nrow(result$log[result$log$verb == "average", ]), 0L)

})


## ---------------------------------------------------------------------------
## average() — Provenance
## ---------------------------------------------------------------------------

test_that("average() records provenance correctly", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 3, n_reps = 4)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = TRUE, correlation_threshold = 0.99, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  prov <- result$provenance$average

  expect_equal(prov$by, "sample_id")
  expect_true(prov$quality_check)
  expect_equal(prov$correlation_threshold, 0.99)
  expect_equal(prov$on_all_outliers, "warn")
  expect_equal(prov$n_groups, 3)
  expect_equal(prov$n_replicates_before, 12)
  expect_equal(prov$n_replicates_after, 3)
  expect_s3_class(prov$applied_at, "POSIXct")

})


## ---------------------------------------------------------------------------
## average() — Custom grouping column
## ---------------------------------------------------------------------------

test_that("average() works with custom by column", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE, uniform_meta = TRUE)

  ## Use project as grouping column (2 projects)
  ## Note: with our test data, project maps 1:1 with sample_id

  ## Act -------------------------------------------------------------------

  result <- average(hd, by = "project", quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_equal(nrow(result$data$analysis), 2)
  expect_equal(result$provenance$average$by, "project")

  ## The grouping values are the new sample ids; the object is whole
  expect_true("sample_id" %in% names(result$data$analysis))
  expect_false("project" %in% names(result$data$analysis))
  expect_setequal(result$data$analysis$sample_id, unique(hd$data$analysis$project))
  expect_identical(result$data$role_map$role[result$data$role_map$variable == "sample_id"], "id")
  expect_identical(sum(result$data$role_map$role == "id"), 1L)
  expect_no_error(validate_horizons_data(result))
  expect_equal(result$provenance$aggregation_by, "project")

  ## An object without a selection record does not gain one
  expect_null(result$selection)

})


## ---------------------------------------------------------------------------
## average() — Input validation
## ---------------------------------------------------------------------------

test_that("average() errors on non-horizons_data input", {

  expect_error(
    average(data.frame(x = 1)),
    "`x` must be a horizons_data object, not a data frame.", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("average() errors when by column doesn't exist", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  expect_error(
    average(hd, by = "nonexistent"),
    "`by` must name a column of the data, and \"nonexistent\" is not one.", fixed = TRUE,
    class = "horizons_error_input"
  )
  expect_error(average(hd, by = c("sample_id", "filename")),
               "`by` must be a single string", fixed = TRUE,
               class = "horizons_error_input")

})

test_that("average() errors on invalid quality_check", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  expect_error(
    average(hd, quality_check = "yes"),
    "`quality_check` must be `TRUE` or `FALSE`", fixed = TRUE,
    class = "horizons_error_input"
  )
  expect_error(average(hd, verbose = NA),
               "`verbose` must be `TRUE` or `FALSE`", fixed = TRUE,
               class = "horizons_error_input")

})

test_that("average() errors on invalid correlation_threshold", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  expect_error(
    average(hd, correlation_threshold = 1.5),
    "`correlation_threshold` must be a number between 0 and 1", fixed = TRUE,
    class = "horizons_error_input"
  )

  expect_error(
    average(hd, correlation_threshold = -0.1),
    "`correlation_threshold` must be a number between 0 and 1", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("average() refuses an on_all_outliers it does not know, and takes the first by default", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  expect_error(average(hd, on_all_outliers = "keep_all"),
               "`on_all_outliers` must be one of \"warn\", \"drop\", and \"keep_best\".", fixed = TRUE,
               class = "horizons_error_input")

  ## The default is "warn"
  expect_warning(average(hd, verbose = FALSE), class = "horizons_warning_quality")

})

test_that("average() refuses an object with no spectra", {

  ## Arrange — the predictor columns and their roles gone
  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)
  wn <- hd$data$role_map$variable[hd$data$role_map$role == "predictor"]
  hd$data$analysis <- hd$data$analysis[, setdiff(names(hd$data$analysis), wn)]
  hd$data$role_map <- hd$data$role_map[!hd$data$role_map$variable %in% wn, ]
  hd$data$n_predictors <- 0L

  ## Act & Assert
  expect_error(average(hd, verbose = FALSE),
               "Average an object with spectra, from `spectra()`.", fixed = TRUE,
               class = "horizons_error_input")

})

test_that("average() refuses a grouping column with NA values", {

  ## Arrange — a scan whose sample id could not be parsed
  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)
  hd$data$analysis$sample_id[1] <- NA

  ## Act & Assert
  expect_error(
    average(hd, quality_check = FALSE, verbose = FALSE),
    "The grouping column sample_id has 1 `NA` value", fixed = TRUE,
    class = "horizons_error_input"
  )

})


test_that("average() refuses an evaluated or fitted object at its door", {

  ## Arrange — averaging rebuilds the rows a promotion's splits are keyed to
  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)
  hd$evaluation$results <- tibble::tibble(config_id = "cfg_a")

  ## Act & Assert — the verb's input error, not the row helper's bug report
  err <- expect_error(average(hd, verbose = FALSE),
                      "It carries evaluation results.", fixed = TRUE,
                      class = "horizons_error_input")
  expect_identical(rlang::call_name(err$call), "average")

})


## ---------------------------------------------------------------------------
## Helper function tests
## ---------------------------------------------------------------------------

test_that("compute_replicate_quality() identifies outliers via iterative removal", {

  ## Arrange ---------------------------------------------------------------

  set.seed(123)

  ## With iterative removal:
  ## 1. First pass: bad spectrum has lowest mean cor, gets removed
  ## 2. Second pass: good spectra now correlate ~1.0 with each other, all pass
  ## Result: only the bad spectrum is flagged as outlier

  ## Base spectrum (use a clear pattern)
  base <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  ## 3 highly correlated spectra (same pattern, tiny noise)
  good_spectra <- matrix(nrow = 3, ncol = 10)
  good_spectra[1, ] <- base + rnorm(10, 0, 0.001)
  good_spectra[2, ] <- base + rnorm(10, 0, 0.001)
  good_spectra[3, ] <- base + rnorm(10, 0, 0.001)

  ## 1 outlier (inverted pattern - will have negative correlation)
  bad_spectrum <- rev(base) + rnorm(10, 0, 0.001)

  all_spectra <- rbind(good_spectra, bad_spectrum)

  ## Act -------------------------------------------------------------------

  ## With iterative removal and high threshold, bad one is removed first,

  ## then good ones pass easily
  qc <- horizons:::compute_replicate_quality(all_spectra, threshold = 0.99)

  ## Assert ----------------------------------------------------------------

  ## Only spectrum 4 should be flagged as outlier
  expect_equal(qc$outliers, 4L)
  expect_false(qc$all_fail)


})

test_that("compute_replicate_quality() returns empty for n=1", {

  ## Arrange ---------------------------------------------------------------

  single_spectrum <- matrix(c(1, 2, 3), nrow = 1)

  ## Act -------------------------------------------------------------------

  qc <- horizons:::compute_replicate_quality(single_spectrum, threshold = 0.99)

  ## Assert ----------------------------------------------------------------

  expect_length(qc$outliers, 0)
  expect_false(qc$all_fail)

})

test_that("check_uniformity() correctly identifies uniform values", {

  ## All same
  expect_true(horizons:::check_uniformity(c("A", "A", "A")))

  ## All NA
  expect_true(horizons:::check_uniformity(c(NA, NA, NA)))

  ## Mixed with NA (uniform non-NA)
  expect_true(horizons:::check_uniformity(c("A", NA, "A")))

  ## Not uniform
  expect_false(horizons:::check_uniformity(c("A", "B", "A")))

  ## Single value
  expect_true(horizons:::check_uniformity("A"))

})


## ---------------------------------------------------------------------------
## average() — Response and outcome columns
## ---------------------------------------------------------------------------

test_that("average() carries numeric responses through as within-group means", {

  ## Arrange
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_responses = TRUE)

  ## Act
  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert — clay is 10, 11, 12 for S1 and 20, 21, 22 for S2
  averaged <- result$data$analysis
  expect_true(all(c("clay", "soc", "texture") %in% names(averaged)))
  expect_equal(averaged$clay[averaged$sample_id == "S1"], 11)
  expect_equal(averaged$clay[averaged$sample_id == "S2"], 21)
  expect_equal(averaged$soc[averaged$sample_id == "S1"], 1.5)

  ## The roles and their counts come through the collapse
  expect_identical(result$data$n_responses, 2L)
  expect_identical(sum(result$data$role_map$role == "outcome"), 1L)

})


test_that("average() averages a response over the replicates that measured it, NA when none did", {

  ## Arrange — clay is 10, 11, 12 for S1 and 20, 21, 22 for S2; S1 loses one
  ## measurement and S2 all three
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_responses = TRUE)
  hd$data$analysis$clay[c(1, 4, 5, 6)] <- NA_real_

  ## Act
  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert
  averaged <- result$data$analysis
  expect_equal(averaged$clay[averaged$sample_id == "S1"], 11.5)
  expect_identical(averaged$clay[averaged$sample_id == "S2"], NA_real_)

})


test_that("average() keeps a uniform non-numeric response", {

  hd     <- make_test_hd_average(n_samples = 2, n_reps = 3, add_responses = TRUE)
  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  averaged <- result$data$analysis

  expect_identical(averaged$texture[averaged$sample_id == "S1"], "silt")
  expect_identical(averaged$texture[averaged$sample_id == "S2"], "loam")

})


test_that("average() aborts on a non-numeric response that disagrees within a group", {

  ## Arrange — one replicate of S1 was called sand, the others silt
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3,
                             add_responses = TRUE, conflict_texture = TRUE)

  ## Act & Assert
  expect_error(average(hd, quality_check = FALSE, verbose = FALSE),
               "texture in S1: sand, silt", fixed = TRUE,
               class = "horizons_error_input")

})


test_that("average() names every sample whose non-numeric response disagrees, in one refusal", {

  ## Arrange — S1 and S2 each have a replicate called something else
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3,
                             add_responses = TRUE, conflict_texture = TRUE)
  hd$data$analysis$texture[4] <- "clay"

  ## Act & Assert — both, rather than the first alone
  err <- expect_error(average(hd, quality_check = FALSE, verbose = FALSE),
                      "2 samples have response values that disagree between replicates.", fixed = TRUE,
                      class = "horizons_error_input")
  expect_match(conditionMessage(err), "texture in S1: sand, silt", fixed = TRUE)
  expect_match(conditionMessage(err), "texture in S2: clay, loam", fixed = TRUE)

  ## One sample disagreeing on two columns is one sample, with a bullet each
  one <- make_test_hd_average(n_samples = 2, n_reps = 3,
                              add_responses = TRUE, conflict_texture = TRUE)
  one$data$analysis$horizon    <- rep(c("A", "B"), each = 3)
  one$data$analysis$horizon[2] <- "Bt"
  one$data$role_map <- tibble::add_row(one$data$role_map, variable = "horizon", role = "response")
  one$data$n_responses <- one$data$n_responses + 1L

  err <- expect_error(average(one, quality_check = FALSE, verbose = FALSE),
                      "1 sample has response values that disagree between replicates.", fixed = TRUE,
                      class = "horizons_error_input")
  expect_match(conditionMessage(err), "texture in S1: sand, silt", fixed = TRUE)
  expect_match(conditionMessage(err), "horizon in S1: A, Bt", fixed = TRUE)

})


test_that("average() averages responses over the QC-surviving replicates only", {

  ## Arrange — the first replicate of S1 is a spectral outlier and is dropped,
  ## so clay should be the mean of 11 and 12, not of 10, 11 and 12
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3,
                             add_outlier = TRUE, add_responses = TRUE)

  ## Act
  result <- average(hd, quality_check = TRUE, verbose = FALSE)

  ## Assert
  averaged <- result$data$analysis
  expect_equal(averaged$clay[averaged$sample_id == "S1"], 11.5)

})


## ---------------------------------------------------------------------------
## average() — Promoting a custom `by` to sample_id
## ---------------------------------------------------------------------------

test_that("average() coerces a numeric by column to a character sample_id", {

  ## Arrange
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3)

  hd$data$analysis$plot_number <- rep(c(101L, 202L), each = 3)
  hd$data$role_map <- dplyr::bind_rows(
    hd$data$role_map,
    tibble::tibble(variable = "plot_number", role = "meta")
  )

  ## Act
  result <- average(hd, by = "plot_number", quality_check = FALSE, verbose = FALSE)

  ## Assert — I2 says sample_id is character
  expect_type(result$data$analysis$sample_id, "character")
  expect_setequal(result$data$analysis$sample_id, c("101", "202"))
  expect_no_error(validate_horizons_data(result))

})


test_that("average() aborts when promoting `by` would produce duplicate ids", {

  ## Arrange — two distinct doubles that print to the same string, which is
  ## exactly how a numeric `by` can mint a duplicate id
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3)

  hd$data$analysis$plot_number <- rep(c(0.1 + 0.2, 0.3), each = 3)
  hd$data$role_map <- dplyr::bind_rows(
    hd$data$role_map,
    tibble::tibble(variable = "plot_number", role = "meta")
  )

  ## Act & Assert — refused before any group is averaged, with the remedy
  local_mocked_bindings(average_group = function(...) stop("averaged a group"))
  expect_error(average(hd, by = "plot_number", quality_check = FALSE, verbose = FALSE),
               "Repeated as text: \"0.3\".", fixed = TRUE,
               class  = "horizons_error_input")
  expect_error(average(hd, by = "plot_number", quality_check = FALSE, verbose = FALSE),
               "Make plot_number character, or round it, before averaging.", fixed = TRUE,
               class  = "horizons_error_input")

})


test_that("average() refuses to promote `by` over an existing sample_id column", {

  ## Arrange — force the condition the rename previously assumed away
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE)

  ## sample_id survives the collapse only if something carries it through;
  ## give it the meta role so it does.
  hd$data$role_map$role[hd$data$role_map$variable == "sample_id"] <- "meta"
  hd$data$role_map <- dplyr::bind_rows(
    hd$data$role_map,
    tibble::tibble(variable = "row_id", role = "id")
  )
  hd$data$analysis$row_id <- paste0("R", seq_len(nrow(hd$data$analysis)))

  ## Act & Assert — the id role off sample_id is refused at entry (#129), so
  ## a valid object cannot carry sample_id into the averaged table, and the
  ## guard average() used to keep behind the validator is gone.
  expect_error(average(hd, by = "project", quality_check = FALSE, verbose = FALSE),
               regexp = "must be on sample_id",
               class  = "horizons_error_validation")

})


## ---------------------------------------------------------------------------
## average() — Source id provenance
## ---------------------------------------------------------------------------

test_that("average() records which scans each averaged row came from", {

  ## Arrange
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3)

  ## Act
  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert
  src <- result$provenance$average$source_ids

  expect_s3_class(src, "tbl_df")
  expect_identical(names(src), c("sample_id", "source_sample_id"))
  expect_identical(nrow(src), 6L)
  expect_setequal(src$sample_id[src$source_sample_id == "S1"], "S1")

})


test_that("average() maps a custom by value back to its source scans", {

  ## Arrange
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE)

  ## Act
  result <- average(hd, by = "project", quality_check = FALSE, verbose = FALSE)

  ## Assert — the original ids are gone from the table but not from the record
  src <- result$provenance$average$source_ids

  expect_false("S1" %in% result$data$analysis$sample_id)
  expect_setequal(src$source_sample_id[src$sample_id == "PROJ1"], "S1")
  expect_setequal(unique(src$sample_id), c("PROJ1", "PROJ2"))

})


test_that("average() writes aggregation_by on every call", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  expect_identical(result$provenance$aggregation_by, "sample_id")

})


## ---------------------------------------------------------------------------
## The selection record across an average
## ---------------------------------------------------------------------------

test_that("average() refilters the selection record when a sample is dropped", {

  ## Arrange — one sample's replicates all fail QC, so it leaves the object
  hd <- make_test_hd_average(n_samples = 3, n_reps = 3, all_outliers = TRUE)
  hd$selection <- make_selection_record(pool_ids = unique(hd$data$analysis$sample_id))

  ## Act
  result <- average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE)

  ## Assert — the record describes the rows that are actually there
  expect_setequal(unique(result$selection$membership$pool_id),
                  result$data$analysis$sample_id)
  expect_identical(result$selection$pool_sizes$drawn,
                   rep(length(result$data$analysis$sample_id), 2L))
  expect_identical(result$selection$rows_removed, 1L)

  for (ids in result$selection$groups$pool_ids) {

    expect_true(all(ids %in% result$data$analysis$sample_id))

  }

})


test_that("average() refuses a custom by on an object carrying a record", {

  ## Arrange — promoting `project` renames the rows the draw is keyed to,
  ## and a renamed row cannot be matched back to the record
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE)
  hd$selection <- make_selection_record(pool_ids = unique(hd$data$analysis$sample_id))

  ## Act & Assert
  expect_error(average(hd, by = "project", quality_check = FALSE, verbose = FALSE),
               "Average first, then run `select_training()`.", fixed = TRUE,
               class  = "horizons_error_input")

})


## ---------------------------------------------------------------------------
## average() — Structural validation on return (#24)
## ---------------------------------------------------------------------------

test_that("average() catches a corrupt input the verb itself never checks (#24)", {

  ## Arrange — an increasing wavenumber axis. average() neither reads nor
  ## enforces predictor order, so nothing about grouping or collapsing the
  ## replicates would ever catch this; the new end-of-verb
  ## validate_horizons_data() call (#24) is what catches it.
  hd <- make_test_hd_average(n_samples = 2, n_reps = 2, n_wavelengths = 3)

  pred_rows <- which(hd$data$role_map$role == "predictor")
  hd$data$role_map[pred_rows, ] <- hd$data$role_map[rev(pred_rows), ]

  ## Act & Assert --------------------------------------------------------------
  ## average()'s own input-validation abort (Step 1) also carries class
  ## horizons_error_validation, so the message is checked too — otherwise
  ## this could pass without ever exercising the new end-of-verb call.

  expect_error(
    average(hd, quality_check = FALSE, verbose = FALSE),
    regexp = "strictly decreasing",
    class  = "horizons_error_validation"
  )

})

test_that("average() validates the averaged object on return (#24)", {

  ## Arrange — a group's average that breaks the full stage the entry check
  ## (raw) allows: every averaged row given one id
  hd   <- make_test_hd_average(n_samples = 2, n_reps = 2)
  real <- average_group
  local_mocked_bindings(average_group = function(...) {
    out <- real(...)
    out$averaged$sample_id <- "S1"
    out
  })

  ## Act & Assert --------------------------------------------------------------

  expect_error(
    average(hd, quality_check = FALSE, verbose = FALSE),
    "Duplicate sample_id values: S1", fixed = TRUE,
    class = "horizons_error_validation"
  )

})

test_that("average() leaves an unusable spectrum to standardize(), which flags it, in the documented order", {

  ## Arrange — a lone scan holding an NA, and a lone flat scan: before
  ## standardize() runs, nothing has flagged either
  hd <- make_test_hd_average(n_samples = 3, n_reps = 1)
  wn <- grep("^wn_", names(hd$data$analysis), value = TRUE)
  hd$data$analysis$wn_500[1] <- NA
  hd$data$analysis[2, wn]    <- 0.7

  ## Act — average() returns them, and standardize() flags them
  averaged <- average(hd, verbose = FALSE)
  expect_identical(averaged$data$analysis$sample_id, c("S1", "S2", "S3"))

  flagged <- suppressWarnings(standardize(averaged, resample = NULL, trim = NULL,
                                          remove_water = FALSE, baseline = FALSE,
                                          verbose = FALSE))
  expect_identical(flagged$data$analysis$.flag, c("standardize", "standardize", NA))
  expect_no_error(validate_horizons_data(flagged))

})

test_that("average()'s quality check fails a replicate it cannot correlate, rather than averaging it in", {

  ## Arrange — S1's first scan is flat
  hd <- make_test_hd_average(n_samples = 2, n_reps = 3)
  wn <- grep("^wn_", names(hd$data$analysis), value = TRUE)
  hd$data$analysis[1, wn] <- 0.5

  ## Act — no base R warning about a zero standard deviation
  expect_no_warning(result <- average(hd, verbose = FALSE))

  ## Assert — S1 is the mean of its two other scans
  s1 <- result$data$analysis[result$data$analysis$sample_id == "S1", wn]
  expect_equal(unlist(s1), colMeans(as.matrix(hd$data$analysis[2:3, wn])))

  ## With two scans, one flat, the flat one is set aside before QC's own
  ## rule, so the group has not failed: S1 is its other scan, in every mode
  two <- make_test_hd_average(n_samples = 2, n_reps = 2)
  two$data$analysis[1, wn] <- 0.5

  for (mode in c("warn", "keep_best", "drop")) {

    expect_no_warning(kept <- average(two, on_all_outliers = mode, verbose = FALSE))
    kept <- kept$data$analysis
    expect_equal(unlist(kept[kept$sample_id == "S1", wn]), unlist(two$data$analysis[2, wn]),
                 info = mode)

  }

})

test_that("average() gives the same sample whether standardize() runs before or after it, when a replicate is flat", {

  ## Arrange — scans QC cannot correlate, here flat
  wn  <- grep("^wn_", names(make_test_hd_average()$data$analysis), value = TRUE)
  std <- function(h) suppressWarnings(standardize(h, resample = NULL, trim = NULL,
                                                  remove_water = FALSE, baseline = FALSE,
                                                  verbose = FALSE))
  s1_of <- function(h) unlist(h$data$analysis[h$data$analysis$sample_id == "S1", wn])

  cases <- list(
    "two scans, one flat"   = list(n_reps = 2, dead = 1,   good = 2),
    "three scans, two flat" = list(n_reps = 3, dead = 1:2, good = 3)
  )

  for (name in names(cases)) {

    cs <- cases[[name]]
    hd <- make_test_hd_average(n_samples = 2, n_reps = cs$n_reps)
    hd$data$analysis[cs$dead, wn] <- 0
    good <- unlist(hd$data$analysis[cs$good, wn])

    ## Act — the documented order, and standardize() first
    expect_no_warning(averaged <- average(hd, verbose = FALSE))
    readme_order <- std(averaged)
    expect_no_warning(std_first <- average(std(hd), verbose = FALSE))

    ## Assert — both are the good scan, unflagged
    expect_equal(s1_of(readme_order), good, info = name)
    expect_equal(s1_of(std_first), good, info = name)
    expect_false(any(flagged_rows(readme_order$data$analysis)), info = name)
    expect_false(any(flagged_rows(std_first$data$analysis)), info = name)

  }

})

test_that("average() averages a sample whose every replicate is unusable, for standardize() to flag", {

  ## Arrange — both of S1's scans are flat
  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)
  wn <- grep("^wn_", names(hd$data$analysis), value = TRUE)
  hd$data$analysis[1, wn] <- 0.2
  hd$data$analysis[2, wn] <- 0.4

  ## Act — no warning: QC could not look at the group, so it has not failed
  expect_no_warning(averaged <- average(hd, verbose = FALSE))
  s1 <- unlist(averaged$data$analysis[averaged$data$analysis$sample_id == "S1", wn])

  ## Assert — the mean of both, which standardize() flags
  expect_equal(unname(s1), rep(0.3, length(wn)))
  flagged <- suppressWarnings(standardize(averaged, resample = NULL, trim = NULL,
                                          remove_water = FALSE, baseline = FALSE,
                                          verbose = FALSE))
  expect_identical(flagged$data$analysis$.flag, c("standardize", NA))

  ## The tree counts it among the groups QC could not look at, not clean
  expect_identical(qc_report(utils::capture.output(average(hd, verbose = TRUE))), c(
    "│  ├─ QC (r > 0.996)",
    "│  │  ├─ 1/2 groups clean",
    "│  │  ├─ 1 group with fewer than two usable replicates (not QC'd)",
    "│  │  └─ 2 samples retained"
  ))

})

test_that("average() refuses a column with no role_map entry instead of silently dropping it (#24)", {

  ## Arrange — Step 8 rebuilds the averaged table from role_map's predictor,
  ## meta and response columns; a column present in analysis but absent from
  ## role_map is referenced by none of them, so it used to disappear from the
  ## output without a trace. The entry-stage validate_horizons_data(x, stage
  ## = "raw") call (#24) refuses it instead.
  hd <- make_test_hd_average(n_samples = 2, n_reps = 2, n_wavelengths = 3)
  hd$data$analysis$stray_column <- seq_len(nrow(hd$data$analysis))

  expect_error(
    average(hd, quality_check = FALSE, verbose = FALSE),
    regexp = "[Mm]issing from.*role_map",
    class  = "horizons_error_validation"
  )

})
