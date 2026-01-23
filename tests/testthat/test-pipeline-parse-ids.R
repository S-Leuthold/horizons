## ---------------------------------------------------------------------------
## Tests for pipeline-parse-ids.R: parse_ids() function
## ---------------------------------------------------------------------------

library(testthat)
library(horizons)


## ---------------------------------------------------------------------------
## Helper: Create horizons_data for testing
## ---------------------------------------------------------------------------

#' Create minimal horizons_data object for testing parse_ids
#'
#' Simulates OPUS file input (with filename column and provenance type "opus")
#'
#' @param filenames Character vector of filenames.
#' @param spectra_type Provenance type, default "opus" for testing parse_ids.
#' @return A horizons_data object.
#' @noRd
make_test_hd <- function(filenames, spectra_type = "opus") {

  n <- length(filenames)

  analysis <- tibble::tibble(
    sample_id = filenames,
    filename  = filenames,
    wn_4000   = runif(n),
    wn_3000   = runif(n)
  )

  role_map <- tibble::tibble(
    variable = c("sample_id", "filename", "wn_4000", "wn_3000"),
    role     = c("id", "meta", "predictor", "predictor")
  )

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = n,
      n_predictors = 2,
      n_covariates = 0
    ),
    provenance = list(
      spectra_source   = "test",
      spectra_type     = spectra_type,
      created          = Sys.time(),
      horizons_version = utils::packageVersion("horizons"),
      schema_version   = 1L,
      id_pattern       = NULL
    )
  )

  class(obj) <- c("horizons_data", "list")
  obj

}


## ---------------------------------------------------------------------------
## parse_ids() — Basic format string parsing
## ---------------------------------------------------------------------------

test_that("parse_ids() extracts values from simple format", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("PROJ_S001_bulk", "PROJ_S002_bulk"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}")

  ## Assert ----------------------------------------------------------------

  expect_s3_class(result, "horizons_data")
  expect_equal(result$data$analysis$sample_id, c("S001", "S002"))
  expect_true("project" %in% names(result$data$analysis))
  expect_true("fraction" %in% names(result$data$analysis))
  expect_equal(result$data$analysis$project, c("PROJ", "PROJ"))
  expect_equal(result$data$analysis$fraction, c("bulk", "bulk"))

})

test_that("parse_ids() handles mixed delimiters", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("PROJ-A.S001_rep1", "PROJ-B.S002_rep2"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}.{sampleid}_{replicate}")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id, c("S001", "S002"))
  expect_equal(result$data$analysis$project, c("PROJ-A", "PROJ-B"))
  expect_equal(result$data$analysis$replicate, c("rep1", "rep2"))

})

test_that("parse_ids() handles complex filenames", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c(
    "AONR-F_S100-1_GroundBulk_S1_A2.0",
    "AONR-G_S200-2_IntactBulk_S2_B3.0"
  ))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}_{scan}_{position}")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id, c("S100-1", "S200-2"))
  expect_equal(result$data$analysis$project, c("AONR-F", "AONR-G"))
  expect_equal(result$data$analysis$fraction, c("GroundBulk", "IntactBulk"))
  expect_equal(result$data$analysis$scan, c("S1", "S2"))
  expect_equal(result$data$analysis$position, c("A2", "B3"))

})


## ---------------------------------------------------------------------------
## parse_ids() — sampleid token handling
## ---------------------------------------------------------------------------

test_that("parse_ids() sampleid token overwrites sample_id column", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("PROJ_ABC123_extra", "PROJ_DEF456_extra"))

  ## Assert original sample_id ----------------------------------------------

  expect_equal(hd$data$analysis$sample_id, c("PROJ_ABC123_extra", "PROJ_DEF456_extra"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}_{sampleid}_{other}")

  ## Assert updated sample_id ----------------------------------------------

  expect_equal(result$data$analysis$sample_id, c("ABC123", "DEF456"))

})

test_that("parse_ids() case-insensitive sampleid variants work", {

  variants <- c(
    "{project}_{sampleid}_{x}",
    "{project}_{SampleID}_{x}",
    "{project}_{sample_id}_{x}"
  )

  for (fmt in variants) {

    hd     <- make_test_hd(c("PROJ_ID001_extra"))
    result <- parse_ids(hd, format = fmt)

    expect_equal(result$data$analysis$sample_id, "ID001",
                 info = paste("Failed for format:", fmt))

  }

})

test_that("parse_ids() removes sampleid column from meta (no duplicate)", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("PROJ_S001"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}_{sampleid}")

  ## Assert ----------------------------------------------------------------

  ## sampleid should not appear as separate column

  expect_false("sampleid" %in% names(result$data$analysis))
  expect_true("sample_id" %in% names(result$data$analysis))

})


## ---------------------------------------------------------------------------
## parse_ids() — Other tokens create new meta columns
## ---------------------------------------------------------------------------

test_that("parse_ids() adds new columns to meta", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B_C"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{first}_{second}_{third}")

  ## Assert ----------------------------------------------------------------

  expect_true("first" %in% names(result$data$analysis))
  expect_true("second" %in% names(result$data$analysis))
  expect_true("third" %in% names(result$data$analysis))

  ## Check role_map --------------------------------------------------------

  roles <- result$data$role_map

  expect_equal(roles$role[roles$variable == "first"], "meta")
  expect_equal(roles$role[roles$variable == "second"], "meta")
  expect_equal(roles$role[roles$variable == "third"], "meta")

})


## ---------------------------------------------------------------------------
## parse_ids() — Non-match handling
## ---------------------------------------------------------------------------

test_that("parse_ids() too_few='keep_original' keeps filename as sample_id", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("PROJ_S001_bulk", "weird_format", "PROJ_S002_bulk"))

  ## Act -------------------------------------------------------------------

  expect_warning(
    result <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}",
                        too_few = "keep_original"),
    "did not match"
  )

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id[1], "S001")
  expect_equal(result$data$analysis$sample_id[2], "weird_format")  # kept original
  expect_equal(result$data$analysis$sample_id[3], "S002")

})

test_that("parse_ids() too_few='na' sets sample_id to NA", {

  ## Arrange ---------------------------------------------------------------

  ## "nomatch" has no underscore, so won't match "{project}_{sampleid}" -----

  hd <- make_test_hd(c("PROJ_S001", "nomatch"))

  ## Act -------------------------------------------------------------------

  expect_warning(
    result <- parse_ids(hd, format = "{project}_{sampleid}",
                        too_few = "na"),
    "did not match"
  )

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id[1], "S001")
  expect_true(is.na(result$data$analysis$sample_id[2]))

})

test_that("parse_ids() too_few='error' aborts on non-match", {

  ## Arrange ---------------------------------------------------------------

  ## "nomatch" has no underscore, so won't match "{project}_{sampleid}" -----

  hd <- make_test_hd(c("PROJ_S001", "nomatch"))

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    parse_ids(hd, format = "{project}_{sampleid}", too_few = "error"),
    "did not match"
  )

})


## ---------------------------------------------------------------------------
## parse_ids() — Regex patterns mode
## ---------------------------------------------------------------------------

test_that("parse_ids() works with explicit patterns", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("PROJ-A_S100-1_rest", "PROJ-B_S200-2_rest"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, patterns = c(
    project  = "[^_]+",
    "_",
    sampleid = "S\\d+-\\d+",
    "_.*"
  ))

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id, c("S100-1", "S200-2"))
  expect_equal(result$data$analysis$project, c("PROJ-A", "PROJ-B"))

})

test_that("parse_ids() patterns mode ignores unnamed patterns", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("prefix_VALUE_suffix"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, patterns = c(
    "prefix_",
    value = "[A-Z]+",
    "_suffix"
  ))

  ## Assert ----------------------------------------------------------------

  expect_true("value" %in% names(result$data$analysis))
  expect_equal(result$data$analysis$value, "VALUE")

  ## prefix and suffix should not be columns --------------------------------

  expect_false("prefix_" %in% names(result$data$analysis))

})


## ---------------------------------------------------------------------------
## parse_ids() — Provenance tracking
## ---------------------------------------------------------------------------

test_that("parse_ids() records provenance", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B", "C_D"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{first}_{second}")

  ## Assert ----------------------------------------------------------------

  prov <- result$provenance$parse_ids

  expect_equal(prov$format, "{first}_{second}")
  expect_equal(prov$matched, 2)
  expect_equal(prov$unmatched, 0)
  expect_true("first" %in% prov$columns_added)
  expect_true("second" %in% prov$columns_added)
  expect_s3_class(prov$applied_at, "POSIXct")

})

test_that("parse_ids() provenance tracks unmatched count", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B", "no-match", "C_D"))

  ## Act -------------------------------------------------------------------

  expect_warning(
    result <- parse_ids(hd, format = "{first}_{second}"),
    "did not match"
  )

  ## Assert ----------------------------------------------------------------

  prov <- result$provenance$parse_ids

  expect_equal(prov$matched, 2)
  expect_equal(prov$unmatched, 1)

})


## ---------------------------------------------------------------------------
## parse_ids() — Original filename preserved
## ---------------------------------------------------------------------------

test_that("parse_ids() preserves original filename column", {

  ## Arrange ---------------------------------------------------------------

  original_names <- c("PROJ_S001_bulk", "PROJ_S002_bulk")
  hd <- make_test_hd(original_names)

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$filename, original_names)

})


## ---------------------------------------------------------------------------
## parse_ids() — Extension stripping
## ---------------------------------------------------------------------------

test_that("parse_ids() strips file extensions before parsing", {

  ## Arrange ---------------------------------------------------------------

  ## Filenames with extensions (as if from OPUS files) ----------------------

  hd <- make_test_hd(c("PROJ_S001_bulk.0", "PROJ_S002_bulk.0"))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id, c("S001", "S002"))
  expect_equal(result$data$analysis$fraction, c("bulk", "bulk"))

})


## ---------------------------------------------------------------------------
## parse_ids() — Warns on non-OPUS sources
## ---------------------------------------------------------------------------

test_that("parse_ids() warns when used with CSV source", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B"), spectra_type = "csv")

  ## Act & Assert ----------------------------------------------------------

  expect_warning(
    parse_ids(hd, format = "{first}_{second}"),
    "designed for OPUS"
  )

})

test_that("parse_ids() warns when used with tibble source", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B"), spectra_type = "tibble")

  ## Act & Assert ----------------------------------------------------------

  expect_warning(
    parse_ids(hd, format = "{first}_{second}"),
    "designed for OPUS"
  )

})

test_that("parse_ids() does NOT warn for OPUS source", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B"), spectra_type = "opus")

  ## Act & Assert ----------------------------------------------------------

  expect_no_warning(
    parse_ids(hd, format = "{first}_{second}")
  )

})


## ---------------------------------------------------------------------------
## parse_ids() — Input validation
## ---------------------------------------------------------------------------

test_that("parse_ids() errors on non-horizons_data input", {

  expect_error(
    parse_ids(data.frame(x = 1), format = "{a}"),
    "must be a horizons_data"
  )

})

test_that("parse_ids() errors when neither format nor patterns provided", {

  hd <- make_test_hd(c("test"))

  expect_error(
    parse_ids(hd),
    "Must provide either"
  )

})

test_that("parse_ids() errors when both format and patterns provided", {

  hd <- make_test_hd(c("test"))

  expect_error(
    parse_ids(hd, format = "{a}", patterns = c(a = ".+")),
    "Cannot provide both"
  )

})

test_that("parse_ids() errors on empty format string", {

  hd <- make_test_hd(c("test"))

  expect_error(
    parse_ids(hd, format = ""),
    "cannot be empty"
  )

})

test_that("parse_ids() errors when format has no tokens", {

  hd <- make_test_hd(c("test"))

  expect_error(
    parse_ids(hd, format = "literal_only"),
    "No tokens"
  )

})

test_that("parse_ids() errors when filename column missing", {

  ## Create broken horizons_data without filename column --------------------

  obj <- list(
    data = list(
      analysis = tibble::tibble(sample_id = "A", wn_4000 = 0.1),
      role_map = tibble::tibble(variable = c("sample_id", "wn_4000"),
                                role = c("id", "predictor"))
    ),
    provenance = list()
  )
  class(obj) <- c("horizons_data", "list")

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    parse_ids(obj, format = "{a}"),
    "filename"
  )

})


## ---------------------------------------------------------------------------
## Helper function tests
## ---------------------------------------------------------------------------

test_that("format_to_patterns() converts simple format", {

  ## Act -------------------------------------------------------------------
  patterns <- horizons:::format_to_patterns("{a}_{b}")

  ## Assert ----------------------------------------------------------------

  expect_length(patterns, 3)
  expect_equal(names(patterns)[1], "a")
  expect_equal(names(patterns)[3], "b")

  ## Separator should be "_" (may or may not have name) --------------------

  expect_equal(as.character(patterns[2]), "_")

})

test_that("format_to_patterns() uses non-greedy except last token", {

  ## Act -------------------------------------------------------------------

  patterns <- horizons:::format_to_patterns("{a}_{b}_{c}")

  ## Assert ----------------------------------------------------------------

  ## First two tokens should be non-greedy (.+?) ---------------------------

  expect_equal(as.character(patterns[1]), "(.+?)")
  expect_equal(as.character(patterns[3]), "(.+?)")

  ## Last token should be greedy (.+) --------------------------------------

  expect_equal(as.character(patterns[5]), "(.+)")

})

test_that("apply_patterns() extracts named groups", {

  ## Arrange ---------------------------------------------------------------

  filenames <- c("A_B_C", "X_Y_Z")
  patterns  <- c(first = "(.+?)", "_", second = "(.+?)", "_", third = "(.+)")

  ## Act -------------------------------------------------------------------

  result <- horizons:::apply_patterns(filenames, patterns)

  ## Assert ----------------------------------------------------------------

  expect_equal(result$first, c("A", "X"))
  expect_equal(result$second, c("B", "Y"))
  expect_equal(result$third, c("C", "Z"))

})

test_that("apply_patterns() returns NA for non-matches", {

  ## Arrange ---------------------------------------------------------------

  filenames <- c("A_B", "no-underscore")
  patterns  <- c(first = "(.+?)", "_", second = "(.+)")

  ## Act -------------------------------------------------------------------

  result <- horizons:::apply_patterns(filenames, patterns)

  ## Assert ----------------------------------------------------------------

  expect_equal(result$first[1], "A")
  expect_true(is.na(result$first[2]))

})
