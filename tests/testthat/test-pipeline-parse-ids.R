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

  new_horizons_data(analysis = analysis, role_map = role_map,
                    spectra_source = "test", spectra_type = spectra_type)

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
    "SITE-F_S100-1_GroundBulk_S1_A2.0",
    "SITE-G_S200-2_IntactBulk_S2_B3.0"
  ))

  ## Act -------------------------------------------------------------------

  result <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}_{scan}_{position}")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id, c("S100-1", "S200-2"))
  expect_equal(result$data$analysis$project, c("SITE-F", "SITE-G"))
  expect_equal(result$data$analysis$fraction, c("GroundBulk", "IntactBulk"))
  expect_equal(result$data$analysis$scan, c("S1", "S2"))
  expect_equal(result$data$analysis$position, c("A2", "B3"))

})


## ---------------------------------------------------------------------------
## parse_ids() — sampleid token handling
## ---------------------------------------------------------------------------

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
    "did not match", class = "horizons_warning_substitution"
  )

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id[1], "S001")
  expect_equal(result$data$analysis$sample_id[2], "weird_format")  # kept original
  expect_equal(result$data$analysis$sample_id[3], "S002")

  ## Recorded on the object, naming the file, whether or not it is seen
  quiet <- suppressWarnings(parse_ids(hd, format = "{project}_{sampleid}_{fraction}"))
  row   <- quiet$log[quiet$log$verb == "parse_ids" & quiet$log$type == "warning", ]
  expect_identical(row$topic, "substitution")
  expect_identical(row$ids[[1]], "weird_format")

})

test_that("parse_ids() too_few='na' sets sample_id to NA, with or without a sampleid token", {

  ## Arrange ---------------------------------------------------------------

  ## "nomatch" has no underscore, so won't match "{project}_{sampleid}" -----

  hd <- make_test_hd(c("PROJ_S001", "nomatch"))

  ## Act -------------------------------------------------------------------
  ## The NA ids are what the user asked for, so they are reported, not
  ## warned: a note on the log and a line in the tree. The NA id passes the
  ## raw-stage validator silently: average() and the full stage refuse it.

  expect_no_warning(
    result <- parse_ids(hd, format = "{project}_{sampleid}", too_few = "na")
  )

  ### The event, then the call's record
  rows <- log_rows_added(hd, result)
  expect_identical(rows$type, c("note", "note"))
  note <- rows[1, ]
  expect_identical(note$ids[[1]], "nomatch")
  expect_match(note$message, "Pattern did not match 1 filename", fixed = TRUE)

  ## Its tree line: the snapshot of "records and reports the unmatched count"

  ## Without a sampleid token the matched rows keep their ids and the
  ## unmatched one still loses its own
  expect_no_warning(
    no_token <- parse_ids(hd, format = "{project}_{plate}", too_few = "na")
  )

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id[1], "S001")
  expect_true(is.na(result$data$analysis$sample_id[2]))

  expect_identical(no_token$data$analysis$sample_id, c("PROJ_S001", NA))

})

test_that("parse_ids() too_few='error' aborts on non-match", {

  ## Arrange ---------------------------------------------------------------

  ## "nomatch" has no underscore, so won't match "{project}_{sampleid}" -----

  hd <- make_test_hd(c("PROJ_S001", "nomatch"))

  ## Act & Assert ----------------------------------------------------------

  err <- expect_error(
    parse_ids(hd, format = "{project}_{sampleid}", too_few = "error"),
    "did not match", class = "horizons_error_input"
  )
  expect_match(conditionMessage(err), "or set `too_few` to \"keep_original\" or \"na\"", fixed = TRUE)

  expect_error(parse_ids(hd, format = "{project}_{sampleid}", too_few = "drop"),
               "`too_few` must be one of", fixed = TRUE, class = "horizons_error_input")

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

test_that("parse_ids() with patterns keeps the id_pattern key, set to NULL", {

  hd <- make_test_hd(c("PROJ-A_S100-1_rest", "PROJ-B_S200-2_rest"))

  result <- parse_ids(hd, patterns = c(project = "[^_]+", "_", sampleid = "S\\d+-\\d+", "_.*"))

  expect_true("id_pattern" %in% names(result$provenance))
  expect_null(result$provenance$id_pattern)

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

test_that("parse_ids() records the call as its last row, a note with the format or patterns, the counts and the columns added", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B", "no-match", "C_D"))

  ## Act -------------------------------------------------------------------

  result <- suppressWarnings(parse_ids(hd, format = "{first}_{second}", verbose = FALSE))

  ## Assert ----------------------------------------------------------------

  ### The substitution warning first, then the record
  rows <- log_rows_added(hd, result)
  expect_identical(rows$type, c("warning", "note"))
  note <- rows[2, ]
  expect_identical(note$verb, "parse_ids")
  expect_true(is.na(note$topic))
  expect_identical(note$ids[[1]], c("first", "second"))
  expect_identical(note$message,
                   'Parsed sample ids with the format "{first}_{second}": 2 of 3 filenames matched, the columns first and second added.')

  ### Patterns are recorded in place of a format; a clean call warns nothing
  hd <- make_test_hd(c("PROJ-A_S100-1_rest", "PROJ-B_S200-2_rest"))
  expect_no_warning(
    result <- parse_ids(hd, patterns = c(project = "[^_]+", "_", sampleid = "S\\d+-\\d+", "_.*"),
                        verbose = FALSE)
  )
  note <- log_rows_added(hd, result)
  expect_identical(note$type, "note")
  expect_identical(note$ids[[1]], "project")
  expect_identical(note$message,
                   'Parsed sample ids with the patterns "[^_]+", "_", "S\\\\d+-\\\\d+", and "_.*": 2 of 2 filenames matched, the column project added.')

  ### A parse that adds no column says so
  hd     <- make_test_hd(c("X1", "X2"))
  result <- parse_ids(hd, format = "{sampleid}", verbose = FALSE)
  expect_identical(result$log$ids[[1]], character(0))
  expect_match(result$log$message, "2 of 2 filenames matched, no column added.", fixed = TRUE)

})

test_that("parse_ids() prints nothing under verbose = FALSE, and refuses a verbose that is not TRUE or FALSE", {

  hd <- make_test_hd(c("A_B", "C_D"))

  expect_silent(parse_ids(hd, format = "{first}_{second}", verbose = FALSE))

  expect_error(parse_ids(hd, format = "{first}_{second}", verbose = NA),
               "`verbose` must be `TRUE` or `FALSE`", fixed = TRUE, class = "horizons_error_input")

})

test_that("parse_ids() records and reports the unmatched count", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd(c("A_B", "no-match", "C_D"))

  ## Act -------------------------------------------------------------------

  ## The warning and the tree line both carry the count; under too_few =
  ## "na" the tree says the ids were set to NA, and nothing is warned
  expect_snapshot({
    result <- parse_ids(hd, format = "{first}_{second}", verbose = TRUE)
    invisible(parse_ids(hd, format = "{first}_{second}", too_few = "na", verbose = TRUE))
  })

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
## parse_ids() — Any source with a filename column
## ---------------------------------------------------------------------------

test_that("parse_ids() reads CSV, tibble and OPUS data alike, without a warning", {

  for (type in c("csv", "tibble", "opus")) {

    hd <- make_test_hd(c("A_B"), spectra_type = type)

    expect_no_warning(result <- parse_ids(hd, format = "{first}_{second}"))
    expect_identical(result$data$analysis$first, "A", info = type)
    ### Nothing on the log but the call's record
    expect_identical(nrow(result$log), 1L, info = type)
    expect_match(result$log$message, "^Parsed sample ids", info = type)

  }

})


## ---------------------------------------------------------------------------
## parse_ids() — Input validation
## ---------------------------------------------------------------------------

test_that("parse_ids() errors on non-horizons_data input", {

  expect_error(
    parse_ids(data.frame(x = 1), format = "{a}"),
    "must be a horizons_data", class = "horizons_error_input"
  )

})

test_that("parse_ids() errors when neither format nor patterns provided", {

  hd <- make_test_hd(c("test"))

  expect_error(
    parse_ids(hd),
    "Must provide either", class = "horizons_error_input"
  )

})

test_that("parse_ids() errors when both format and patterns provided", {

  hd <- make_test_hd(c("test"))

  expect_error(
    parse_ids(hd, format = "{a}", patterns = c(a = ".+")),
    "Cannot provide both", class = "horizons_error_input"
  )

})

test_that("parse_ids() refuses patterns that are not a character vector", {

  ## Arrange — a list where c() was meant
  hd <- make_test_hd(c("PROJ_S1-1_a", "PROJ_S2-1_b"))

  ## Act & Assert
  expect_error(
    parse_ids(hd, patterns = list(sampleid = "S\\d+-\\d+")),
    "`patterns` must be a character vector", fixed = TRUE, class = "horizons_error_input"
  )

})

test_that("parse_ids() refuses a format that is empty or not a single string", {

  hd <- make_test_hd(c("test"))

  expect_error(
    parse_ids(hd, format = ""),
    "`format` must be a single string, not the empty string", fixed = TRUE,
    class = "horizons_error_input"
  )

  expect_error(
    parse_ids(hd, format = c("{a}", "{b}")),
    "`format` must be a single string, not a character vector", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("parse_ids() refuses a format with no token, naming parse_ids()", {

  hd <- make_test_hd(c("test"))

  err <- expect_error(
    parse_ids(hd, format = "literal_only"),
    "`format` must hold at least one `{token}`", fixed = TRUE,
    class = "horizons_error_input"
  )
  expect_identical(rlang::call_name(err$call), "parse_ids")

})

test_that("parse_ids() errors when filename column missing", {

  ## Create broken horizons_data without filename column --------------------

  obj <- new_horizons_data(
    analysis = tibble::tibble(sample_id = "A", wn_4000 = 0.1),
    role_map = tibble::tibble(variable = c("sample_id", "wn_4000"),
                              role = c("id", "predictor"))
  )

  ## Act & Assert ----------------------------------------------------------

  err <- expect_error(
    parse_ids(obj, format = "{a}"),
    "No filename column found in data", fixed = TRUE, class = "horizons_error_input"
  )
  expect_match(conditionMessage(err), "`spectra()` creates it when it reads OPUS files", fixed = TRUE)

})

test_that("parse_ids() passes ids that collapse to duplicates silently, rather than aborting (#24)", {

  ## Arrange — two distinct filenames whose sampleid token is identical once
  ## the trailing replicate marker is discarded. That is exactly what
  ## replicate scans look like before average() collapses them, so
  ## parse_ids() (raw stage) neither aborts nor warns: a hard error here
  ## would break the documented
  ## spectra() |> standardize() |> parse_ids() |> average() chain (#24), and
  ## data legal at one stage is not warned about for what a later verb
  ## refuses.
  hd <- make_test_hd(c("A_rep1", "A_rep2"))

  ## Act & Assert ------------------------------------------------------------

  expect_no_warning(result <- parse_ids(hd, format = "{sampleid}_{junk}"))

  expect_identical(result$data$analysis$sample_id, c("A", "A"))

})

test_that("the regression this fixes: parse_ids() |> average() survives replicate scans sharing an id (#24)", {

  ## Arrange — a synthetic stand-in for the real chain
  ## (spectra() |> standardize() |> parse_ids() |> average()) on a set of
  ## real OPUS files: 15 scans, 4 samples, aborting at parse_ids() was the
  ## critical finding this rework addresses. Those files are not shipped,
  ## so this rebuilds the same shape post-spectra(): OPUS
  ## filenames that share a sampleid token once parsed — replicate scans,
  ## the state average() exists to collapse. standardize() is exercised
  ## separately (test-pipeline-standardize.R); this covers parse_ids() and
  ## average() end to end on the object shape spectra(type = "opus") builds.
  filenames <- c(
    "SITE-F_S100-1_GroundBulk_S1_A2", "SITE-F_S100-1_GroundBulk_S2_B2",
    "SITE-F_S100-1_GroundBulk_S3_C2",
    "SITE-F_S100-2_GroundBulk_S1_E2", "SITE-F_S100-2_GroundBulk_S2_F2"
  )

  hd <- make_test_hd(filenames)

  ## Act -------------------------------------------------------------------

  expect_no_warning(
    hd_parsed <- parse_ids(hd, patterns = c("SITE-F_", sampleid = "S\\d+-\\d+", "_.*"))
  )

  expect_identical(hd_parsed$data$analysis$sample_id,
                   c("S100-1", "S100-1", "S100-1", "S100-2", "S100-2"))

  utils::capture.output(
    hd_avg <- average(hd_parsed, by = "sample_id", quality_check = FALSE, verbose = FALSE)
  )

  ## Assert — no error anywhere in the chain, replicates collapsed to
  ## samples, and the result is fully valid (full stage: unique ids)
  ## ---------------------------------------------------------------------

  expect_identical(sort(hd_avg$data$analysis$sample_id), c("S100-1", "S100-2"))
  expect_identical(hd_avg$data$n_rows, 2L)
  expect_no_error(validate_horizons_data(hd_avg))

})


## ---------------------------------------------------------------------------
## parse_ids() — Writing the analysis table (#135)
## ---------------------------------------------------------------------------

test_that("parse_ids() refuses an evaluated, fitted or ensembled object (#135)", {

  ## Arrange — rewriting sample_id would leave the stored splits keyed to ids
  ## the object no longer has
  hd <- make_test_hd(c("PROJ_S001_bulk", "PROJ_S002_bulk"))

  ev <- hd
  ev$evaluation <- list(results = tibble::tibble(config_id = "cfg_a"))

  ft <- hd
  class(ft) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  ## Act & Assert — refused at parse_ids()'s door, as the user's input
  err <- expect_error(parse_ids(ev, format = "{project}_{sampleid}_{fraction}"),
                      regexp = "evaluation results",
                      class  = "horizons_error_input")
  expect_identical(rlang::call_name(err$call), "parse_ids")

  expect_error(parse_ids(ft, format = "{project}_{sampleid}_{fraction}"),
               regexp = "horizons_fit",
               class  = "horizons_error_input")

})

test_that("parse_ids() refuses to rewrite the ids of a select_training() result", {

  ## Arrange — the record is keyed to the ids it drew
  hd <- make_test_hd(c("PROJ_S001_bulk", "PROJ_S002_bulk"))
  hd$selection <- make_selection_record(pool_ids = hd$data$analysis$sample_id)

  ## Act & Assert
  err <- expect_error(parse_ids(hd, format = "{project}_{sampleid}_{fraction}"),
                      "Parse the ids before `select_training()`", fixed = TRUE,
                      class = "horizons_error_input")
  expect_identical(rlang::call_name(err$call), "parse_ids")

  ## A parse that leaves the ids alone adds its columns
  out <- parse_ids(hd, format = "{project}_{id}_{fraction}")
  expect_identical(out$data$analysis$sample_id, hd$data$analysis$sample_id)
  expect_true("project" %in% names(out$data$analysis))

})

test_that("parse_ids() puts new columns after filename and recomputes the counts", {

  ## Arrange
  hd <- make_test_hd(c("PROJ_S001_bulk", "PROJ_S002_bulk"))

  ## Act
  result <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}")

  ## Assert — meta columns ahead of the spectra, counts from the role map
  expect_identical(names(result$data$analysis),
                   c("sample_id", "filename", "project", "fraction", "wn_4000", "wn_3000"))
  expect_identical(result$data$n_rows,       2L)
  expect_identical(result$data$n_predictors, 2L)
  expect_identical(result$data$n_responses,  0L)

  ## The token columns are meta
  roles <- result$data$role_map
  expect_identical(roles$role[match(c("project", "fraction"), roles$variable)],
                   c("meta", "meta"))

})

test_that("parse_ids() refuses a token that names an existing column (#135)", {

  ## Arrange — a second parse with the same format would overwrite project
  hd     <- make_test_hd(c("PROJ_S001_bulk", "PROJ_S002_bulk"))
  parsed <- parse_ids(hd, format = "{project}_{sampleid}_{fraction}")

  ## Act & Assert — refused at the door, naming the token
  expect_error(parse_ids(parsed, format = "{project}_{sampleid}_{fraction}"),
               "Tokens project and fraction name columns already in the data.", fixed = TRUE,
               class = "horizons_error_input")

})


## ---------------------------------------------------------------------------
## Helper function tests
## ---------------------------------------------------------------------------

test_that("format_to_patterns() names each token, keeps the separators, and is non-greedy except the last token", {

  ## Act -------------------------------------------------------------------

  patterns <- horizons:::format_to_patterns("{a}_{b}_{c}")

  ## Assert ----------------------------------------------------------------

  ## Tokens named by their placeholders, separators in between -------------

  expect_length(patterns, 5)
  expect_identical(names(patterns)[c(1, 3, 5)], c("a", "b", "c"))
  expect_identical(as.character(patterns[c(2, 4)]), c("_", "_"))

  ## First two tokens should be non-greedy (.+?) ---------------------------

  expect_equal(as.character(patterns[1]), "(.+?)")
  expect_equal(as.character(patterns[3]), "(.+?)")

  ## Last token should be greedy (.+) --------------------------------------

  expect_equal(as.character(patterns[5]), "(.+)")

  ## Text after the last token is kept, escaped ----------------------------

  trailing <- horizons:::format_to_patterns("{a}_{b}.raw")

  expect_identical(unname(trailing), c("(.+?)", "_", "(.+)", "\\.raw"))
  expect_identical(names(trailing), c("a", "", "b", ""))

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
