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

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = n_total,
      n_predictors = n_wavelengths,
      n_covariates = 0
    ),
    provenance = list(
      spectra_source   = "test",
      spectra_type     = "opus",
      created          = Sys.time(),
      horizons_version = utils::packageVersion("horizons"),
      schema_version   = 1L,
      aggregation_by   = NULL
    )
  )

  class(obj) <- c("horizons_data", "list")
  obj

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

test_that("average() works with single replicate groups", {


  ## Arrange ---------------------------------------------------------------

  ## Create data with 1 replicate per sample
  hd <- make_test_hd_average(n_samples = 3, n_reps = 1)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## Should pass through unchanged (except filename column)
  expect_equal(nrow(result$data$analysis), 3)

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

  expect_warning(
    result <- average(hd, quality_check = TRUE, on_all_outliers = "warn", verbose = FALSE),
    "All replicates failed"
  )

  ## Assert ----------------------------------------------------------------

  ## Sample 1 should still be in results
  expect_true("S1" %in% result$data$analysis$sample_id)
  expect_equal(length(result$provenance$average$samples_all_outliers), 1)

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
  expect_equal(result$provenance$average$samples_dropped, "S1")

})

test_that("average() on_all_outliers='keep_best' keeps highest correlation replicate", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, all_outliers = TRUE)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = TRUE, on_all_outliers = "keep_best", verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## Sample 1 should still exist (with 1 replicate)
  expect_true("S1" %in% result$data$analysis$sample_id)

  ## Should have dropped 2 replicates from S1
  expect_equal(result$provenance$average$n_replicates_dropped, 2)

  ## Verify keep_best retained the QC survivor (not an all-NA artifact)
  s1_result <- result$data$analysis[result$data$analysis$sample_id == "S1", ]
  wn_cols   <- grep("^wn_", names(s1_result), value = TRUE)
  expect_true(all(!is.na(s1_result[, wn_cols])))

})

test_that("average() errors when all samples dropped", {

  ## Arrange ---------------------------------------------------------------

  ## Single sample with all outliers
  hd <- make_test_hd_average(n_samples = 1, n_reps = 3, all_outliers = TRUE)

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE),
    "All samples dropped"
  )

})


## ---------------------------------------------------------------------------
## average() — Metadata handling
## ---------------------------------------------------------------------------

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

  ## Should be tracked in provenance
  expect_true("rep_num" %in% result$provenance$average$meta_columns_dropped)

})

test_that("average() always drops filename column", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_false("filename" %in% names(result$data$analysis))
  expect_true("filename" %in% result$provenance$average$meta_columns_dropped)

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

test_that("average() updates aggregation_by in provenance", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  ## Act -------------------------------------------------------------------

  result <- average(hd, by = "sample_id", quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  expect_equal(result$provenance$aggregation_by, "sample_id")

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

})


## ---------------------------------------------------------------------------
## average() — Input validation
## ---------------------------------------------------------------------------

test_that("average() errors on non-horizons_data input", {

  expect_error(
    average(data.frame(x = 1)),
    "must be a horizons_data"
  )

})

test_that("average() errors when by column doesn't exist", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  expect_error(
    average(hd, by = "nonexistent"),
    "not found"
  )

})

test_that("average() errors on invalid quality_check", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  expect_error(
    average(hd, quality_check = "yes"),
    "must be TRUE or FALSE"
  )

})

test_that("average() errors on invalid correlation_threshold", {

  hd <- make_test_hd_average(n_samples = 2, n_reps = 2)

  expect_error(
    average(hd, correlation_threshold = 1.5),
    "between 0 and 1"
  )

  expect_error(
    average(hd, correlation_threshold = -0.1),
    "between 0 and 1"
  )

})


## ---------------------------------------------------------------------------
## average() — Role map updates
## ---------------------------------------------------------------------------

test_that("average() updates role_map to remove dropped columns", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE, uniform_meta = FALSE)

  ## Act -------------------------------------------------------------------

  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  ## Assert ----------------------------------------------------------------

  ## Dropped columns should not be in role_map
  expect_false("filename" %in% result$data$role_map$variable)
  expect_false("rep_num" %in% result$data$role_map$variable)

  ## Retained columns should still be there
  expect_true("sample_id" %in% result$data$role_map$variable)
  expect_true(any(grepl("^wn_", result$data$role_map$variable)))

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

  ## Good spectra (1-3) should have high mean correlations after outlier removal
  expect_true(all(qc$mean_cors[1:3] > 0.99, na.rm = TRUE))

  ## Outlier's mean_cors is NA (it was removed)
  expect_true(is.na(qc$mean_cors[4]))

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
               regexp = "texture",
               class  = "horizons_data_error")

})


test_that("average() with responses leaves a whole object", {

  hd     <- make_test_hd_average(n_samples = 3, n_reps = 3, add_responses = TRUE)
  result <- average(hd, quality_check = FALSE, verbose = FALSE)

  expect_no_error(validate_horizons_data(result))
  expect_identical(result$data$n_responses, 2L)
  expect_identical(sum(result$data$role_map$role == "outcome"), 1L)

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

  ## Act & Assert
  expect_error(average(hd, by = "plot_number", quality_check = FALSE, verbose = FALSE),
               regexp = "duplicate",
               class  = "horizons_data_error")

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

  ## Act & Assert
  expect_error(average(hd, by = "project", quality_check = FALSE, verbose = FALSE),
               regexp = "sample_id",
               class  = "horizons_data_error")

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
               regexp = "selection record",
               class  = "horizons_validation_error")

})


test_that("average() leaves an object without a record alone", {

  hd     <- make_test_hd_average(n_samples = 2, n_reps = 3, add_meta = TRUE)
  result <- average(hd, by = "project", quality_check = FALSE, verbose = FALSE)

  expect_null(result$selection)

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
  ## horizons_validation_error, so the message is checked too — otherwise
  ## this could pass without ever exercising the new end-of-verb call.

  expect_error(
    average(hd, quality_check = FALSE, verbose = FALSE),
    regexp = "strictly decreasing",
    class  = "horizons_validation_error"
  )

})
