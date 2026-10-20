## ---------------------------------------------------------------------------
## Tests for pipeline-spectra.R: spectra() entry point
## ---------------------------------------------------------------------------

library(testthat)
library(horizons)


## ---------------------------------------------------------------------------
## spectra() — Basic functionality
## ---------------------------------------------------------------------------

test_that("spectra() creates horizons_data from tibble", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002", "S003"),
    `4000`    = c(0.1, 0.2, 0.3),
    `3000`    = c(0.2, 0.3, 0.4),
    `2000`    = c(0.3, 0.4, 0.5)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  expect_s3_class(result, "horizons_data")
  expect_equal(result$data$n_rows, 3)
  expect_equal(result$data$n_predictors, 3)

  ## The table carries a tibble's attributes and nothing else
  expect_setequal(names(attributes(result$data$analysis)), c("names", "row.names", "class"))

})

test_that("spectra() prints its banner, with the sorting line only when it sorts, and scans under ids when they repeat", {

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002", "S003"),
    `4000`    = c(0.1, 0.2, 0.3),
    `3000`    = c(0.2, 0.3, 0.4),
    `2000`    = c(0.3, 0.4, 0.5)
  )

  ## Replicate scans share an id until average(), and one has none
  scans <- tibble::tibble(Sample_ID = c("A", "A", "B", "B", NA),
                          `4000` = 1:5 / 10, `3998` = 1:5 / 10)

  expect_snapshot({
    invisible(spectra(test_data, verbose = TRUE))
    invisible(spectra(test_data[, c("Sample_ID", "2000", "4000", "3000")], verbose = TRUE))
    invisible(spectra(scans, verbose = TRUE))
  })

})

test_that("spectra() prints nothing under verbose = FALSE, and refuses a verbose that is not TRUE or FALSE", {

  test_data <- tibble::tibble(Sample_ID = c("S001", "S002"), `4000` = c(0.1, 0.2), `3000` = c(0.2, 0.3))

  expect_silent(spectra(test_data, verbose = FALSE))

  expect_error(spectra(test_data, verbose = "yes"), "`verbose` must be `TRUE` or `FALSE`",
               fixed = TRUE, class = "horizons_error_input")

})

test_that("spectra() does NOT add filename column for tibble input", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002"),
    `4000`    = c(0.1, 0.2),
    `3000`    = c(0.2, 0.3)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  ## No filename column for tibble (no actual files) -----------------------

  expect_false("filename" %in% names(result$data$analysis))

})

test_that("spectra() prefixes wavelength columns with wn_", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001"),
    `4000`    = c(0.1),
    `3000`    = c(0.2)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  col_names <- names(result$data$analysis)

  expect_true("wn_4000" %in% col_names)
  expect_true("wn_3000" %in% col_names)
  expect_false("4000" %in% col_names)

})

test_that("spectra() preserves existing wn_ prefix", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001"),
    wn_4000   = c(0.1),
    wn_3000   = c(0.2)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  col_names <- names(result$data$analysis)

  expect_true("wn_4000" %in% col_names)
  expect_false("wn_wn_4000" %in% col_names)

})


## ---------------------------------------------------------------------------
## spectra() — Column detection
## ---------------------------------------------------------------------------

test_that("spectra() detects ID column from aliases", {

  ## Every alias, each on its own ------------------------------------------

  aliases_to_test <- c("sample_id", "Sample_ID", "SampleID", "sampleid",
                       "id", "ID", "Id", "sample", "Sample",
                       "filename", "Filename", "name", "Name")

  for (alias in aliases_to_test) {

    test_data <- tibble::tibble(`4000` = c(0.1, 0.2))
    test_data[[alias]] <- c("A", "B")

    result <- spectra(test_data)

    expect_equal(result$data$analysis$sample_id, c("A", "B"),
                 info = paste("Failed for alias:", alias))

  }

})

test_that("spectra() allows explicit id_col override", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID    = c("wrong1", "wrong2"),
    custom_id    = c("correct1", "correct2"),
    `4000`       = c(0.1, 0.2)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data, id_col = "custom_id")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$sample_id, c("correct1", "correct2"))

})

test_that("spectra() detects wavelength columns with numeric names", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S1"),
    meta_col  = c("info"),
    `4000`    = c(0.1),
    `3500`    = c(0.2),
    `3000`    = c(0.3)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$n_predictors, 3)

  predictor_cols <- result$data$role_map$variable[
    result$data$role_map$role == "predictor"
  ]

  expect_setequal(predictor_cols, c("wn_4000", "wn_3500", "wn_3000"))

})

test_that("spectra() takes the wavelength columns it is given, and refuses one the data lacks", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S1", "S2"),
    `4000`    = c(0.1, 0.2),
    `3500`    = c(0.2, 0.3),
    `3000`    = c(0.3, 0.4)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data, wavelength_cols = c("4000", "3000"))

  ## Assert ----------------------------------------------------------------

  predictor_cols <- result$data$role_map$variable[result$data$role_map$role == "predictor"]

  expect_identical(predictor_cols, c("wn_4000", "wn_3000"))
  expect_identical(result$data$analysis$wn_3000, c(0.3, 0.4))

  expect_error(
    spectra(test_data, wavelength_cols = c("4000", "9999")),
    "Specified wavelength columns not found in data",
    fixed = TRUE, class = "horizons_error_input"
  )

})

test_that("spectra() preserves meta columns", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID  = c("S1", "S2"),
    project    = c("A", "B"),
    date       = c("2024-01-01", "2024-01-02"),
    `4000`     = c(0.1, 0.2)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  expect_true("project" %in% names(result$data$analysis))
  expect_true("date" %in% names(result$data$analysis))

  meta_vars <- result$data$role_map$variable[
    result$data$role_map$role == "meta"
  ]

  expect_true("project" %in% meta_vars)
  expect_true("date" %in% meta_vars)

  expect_s3_class(result$data$role_map, "tbl_df")

})


## ---------------------------------------------------------------------------
## spectra() — Provenance tracking
## ---------------------------------------------------------------------------

test_that("spectra() records provenance for tibble input", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S1"),
    `4000`    = c(0.1)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  expect_equal(result$provenance$spectra_source, "tibble")
  expect_equal(result$provenance$spectra_type, "tibble")
  expect_s3_class(result$provenance$created, "POSIXct")
  expect_false("schema_version" %in% names(result$provenance))
  expect_false("artifacts" %in% names(result))

})


## ---------------------------------------------------------------------------
## spectra() — The object contract (#71)
## ---------------------------------------------------------------------------

## spectra() used to build its object with a constructor of its own whose
## models$uq stub was a non-empty list, so has_uq() answered TRUE on a raw
## object. It now goes through new_horizons_data(), and the shape has to match
## key for key, section by section.
expect_contract_shape <- function(result) {

  contract <- new_horizons_data()

  expect_identical(names(result), names(contract))

  for (section in names(contract)) {

    expect_identical(names(result[[section]]), names(contract[[section]]),
                     info = paste0("section ", section))

  }

  expect_null(result$selection)
  expect_false(has_uq(result))
  expect_identical(class(result), c("horizons_data", "list"))

}

test_that("spectra() refuses a source column named .flag, which the steps reserve", {

  df <- tibble::tibble(sample_id = c("A", "B"), .flag = c(NA, "mine"),
                       `4000` = c(0.1, 0.2), `3998` = c(0.2, 0.3))

  err <- expect_error(utils::capture.output(spectra(df)), class = "horizons_error_input")
  expect_match(conditionMessage(err), "column named .flag", fixed = TRUE)

})

test_that("spectra() from a tibble builds the new_horizons_data() shape", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002", "S003"),
    site      = c("a", "b", "c"),
    `4000`    = c(0.1, 0.2, 0.3),
    `3000`    = c(0.2, 0.3, 0.4)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  expect_contract_shape(result)

  expect_identical(result$data$n_responses, 0L)
  expect_identical(result$config$tuning,
                   list(cv_folds = 5L, grid_size = 10L, bayesian_iter = 15L,
                        final_bayesian_iter = DEFAULT_FINAL_BAYES_ITER))
  expect_no_error(validate_horizons_data(result))

})

test_that("spectra() from a CSV builds the new_horizons_data() shape", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002"),
    site      = c("north", "south"),
    `4000`    = c(0.1, 0.2),
    `3000`    = c(0.2, 0.3)
  )

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)
  readr::write_csv(test_data, temp_csv)

  ## Act -------------------------------------------------------------------

  result <- spectra(temp_csv)

  ## Assert ----------------------------------------------------------------

  expect_contract_shape(result)
  expect_identical(result$provenance$spectra_source, temp_csv)
  expect_equal(result$provenance$spectra_type, "csv")
  expect_equal(result$data$n_rows, 2)

  ## No filename column for CSV (no per-sample files)
  expect_false("filename" %in% names(result$data$analysis))

  ## Other columns are carried as meta
  expect_identical(result$data$analysis$site, c("north", "south"))
  expect_identical(result$data$role_map$role[result$data$role_map$variable == "site"], "meta")

  ## Wavelength columns named by the caller are the ones taken
  picked <- spectra(temp_csv, wavelength_cols = "4000")
  expect_identical(picked$data$role_map$variable[picked$data$role_map$role == "predictor"],
                   "wn_4000")

})


## ---------------------------------------------------------------------------
## spectra() — OPUS files
## ---------------------------------------------------------------------------
## fixtures/opus/ holds three real scans (see fixtures/README.md): S01-1.0 and
## S01-1.1 are two scans of one sample, S02-1.0 a scan of another. Their ids
## repeat once the extension is stripped, which spectra() passes silently, as
## it does for any Bruker run before average() collapses the replicates.

opus_dir <- function() testthat::test_path("fixtures", "opus")

read_opus_dir <- function() {
  expect_no_warning(result <- spectra(opus_dir(), type = "opus"))
  result
}

predictor_names <- function(x) {
  x$data$role_map$variable[x$data$role_map$role == "predictor"]
}

test_that("spectra() reads a directory of OPUS files, one row per file", {

  ## Act -------------------------------------------------------------------

  result <- read_opus_dir()

  ## Assert ----------------------------------------------------------------

  expect_contract_shape(result)
  expect_equal(result$provenance$spectra_type, "opus")
  expect_identical(result$data$analysis$sample_id, c("S01-1", "S01-1", "S02-1"))
  expect_identical(result$data$analysis$filename, c("S01-1", "S01-1", "S02-1"))

  wavenumbers <- as.numeric(sub("^wn_", "", predictor_names(result)))

  expect_length(wavenumbers, 3578L)
  expect_false(is.unsorted(rev(wavenumbers), strictly = TRUE))
  expect_equal(range(wavenumbers), c(599.742, 7497.741), tolerance = 1e-6)

})

test_that("spectra() takes each OPUS file's absorbance block, not a single-channel one", {

  ## Arrange ---------------------------------------------------------------

  ## The absorbance block in this instrument's files is not named "ab", so
  ## spectra() has to pick it out from the single-channel and interferogram
  ## blocks beside it.
  blocks <- opusreader2::read_opus(file.path(opus_dir(), "S02-1.0"))[[1]]
  absorbance <- stats::setNames(as.numeric(blocks$ab_no_atm_comp$data),
                                paste0("wn_", blocks$ab_no_atm_comp$wavenumbers))
  single_channel <- as.numeric(blocks$sc_sample$data)

  ## Act -------------------------------------------------------------------

  result <- read_opus_dir()
  row <- unlist(result$data$analysis[result$data$analysis$sample_id == "S02-1", names(absorbance)])

  ## Assert ----------------------------------------------------------------

  expect_equal(row, absorbance)
  expect_false(isTRUE(all.equal(unname(row), single_channel)))

})

test_that("spectra() reads a single OPUS file and detects its type", {

  ## Act -------------------------------------------------------------------

  result <- spectra(file.path(opus_dir(), "S02-1.0"))

  ## Assert ----------------------------------------------------------------

  expect_equal(result$provenance$spectra_type, "opus")
  expect_identical(result$data$analysis$sample_id, "S02-1")
  expect_equal(result$data$n_rows, 1)

})

test_that("spectra() detects a directory of OPUS files without being told the type", {

  ## Act -------------------------------------------------------------------

  expect_no_warning(result <- spectra(opus_dir()))

  ## Assert ----------------------------------------------------------------

  expect_equal(result$provenance$spectra_type, "opus")
  expect_equal(result$data$n_rows, 3)

})

test_that("spectra() refuses a directory with no OPUS files", {

  ## Arrange ---------------------------------------------------------------

  empty <- withr::local_tempdir()
  writeLines("not a spectrum", file.path(empty, "notes.txt"))

  ## Act and assert --------------------------------------------------------

  expect_error(
    spectra(empty, type = "opus"),
    "No OPUS files found in directory",
    fixed = TRUE, class = "horizons_error_input"
  )

})

test_that("spectra() refuses OPUS input when opusreader2 is not installed, saying so", {

  ## Arrange ---------------------------------------------------------------

  real <- base::requireNamespace
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      !identical(package, "opusreader2") && real(package, ...)
    },
    .package = "base"
  )

  ## Act and assert --------------------------------------------------------

  expect_error(
    spectra(opus_dir(), type = "opus"),
    "Package opusreader2 required for OPUS files",
    fixed = TRUE, class = "horizons_error_dependency"
  )

})

test_that("spectra() reads OPUS files in a fresh session, loading opusreader2 itself", {

  ## load_all() loads opusreader2 up front, and an earlier test in the same
  ## session may have too, so only a fresh process shows the first load
  skip_if_dev_package()
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("opusreader2")

  ## Act -------------------------------------------------------------------

  read <- callr::r(function(dir) {
    suppressPackageStartupMessages(library(horizons))
    loaded_before <- "opusreader2" %in% loadedNamespaces()
    x <- suppressWarnings(spectra(dir, type = "opus"))
    list(loaded_before = loaded_before, ids = x$data$analysis$sample_id)
  }, args = list(normalizePath(opus_dir())))

  ## Assert ----------------------------------------------------------------

  expect_false(read$loaded_before)
  expect_identical(read$ids, c("S01-1", "S01-1", "S02-1"))

})


## ---------------------------------------------------------------------------
## spectra() — Several files, each read on its own (#224, #191)
## ---------------------------------------------------------------------------
## A file that cannot be read is dropped by name and the rest are returned.
## Files opusreader2 would fail on, or that hold other blocks, are not cut
## from real scans: read_opus() is stubbed, keyed by the file's name, over
## empty files that only have to exist.

#' An OPUS block as opusreader2 returns one: a 1-row matrix and its axis
#' @noRd
fake_block <- function(wn, values) {
  list(data = matrix(values, nrow = 1, dimnames = list(NULL, as.character(wn))),
       wavenumbers = wn)
}

#' Empty files named `names` in a temporary directory, which is made the
#' working directory, so the paths spectra() reports are the bare names
#' @noRd
local_opus_files <- function(names, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.create(file.path(dir, names))
  withr::local_dir(dir, .local_envir = env)
  names
}

#' Stub read_opus() with one reader per file name: a list of blocks, or a
#' function run in its place
#' @noRd
local_reads <- function(reads, env = parent.frame()) {
  local_mocked_bindings(
    read_opus = function(dsn, ...) {
      read <- reads[[basename(dsn)]]
      if (is.function(read)) read() else list(file = read)
    },
    .package = "opusreader2", .env = env
  )
}

FAKE_WN <- c(4000, 3998, 3996)

test_that("spectra() reads a vector of OPUS files as it reads their directory, and a file and a directory together", {

  paths <- file.path(opus_dir(), c("S01-1.0", "S01-1.1", "S02-1.0"))

  by_vector <- spectra(paths)
  by_dir    <- read_opus_dir()

  expect_identical(by_vector$data$analysis, by_dir$data$analysis)
  expect_identical(by_vector$provenance$spectra_source, paths)

  ## A file, then a directory: the file's row first
  both <- spectra(c(file.path(opus_dir(), "S02-1.0"), opus_dir()))
  expect_identical(both$data$analysis$sample_id, c("S02-1", "S01-1", "S01-1", "S02-1"))

  ## print() and summary() show the first path and count the rest: the
  ## selection snapshots in test-class-core.R

})

test_that("spectra() reads a vector of CSV files, and a directory of them", {

  ## Arrange ---------------------------------------------------------------

  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(Sample_ID = c("A", "B"), `4000` = c(0.1, 0.2), `3998` = c(0.3, 0.4)),
                   file.path(dir, "one.csv"))
  readr::write_csv(tibble::tibble(Sample_ID = "C", `4000` = 0.5, `3998` = 0.6),
                   file.path(dir, "two.csv"))

  ## Act -------------------------------------------------------------------

  by_vector <- spectra(file.path(dir, c("one.csv", "two.csv")))
  by_dir    <- spectra(dir)

  ## Assert ----------------------------------------------------------------

  expect_identical(by_vector$data$analysis$sample_id, c("A", "B", "C"))
  expect_identical(by_vector$data$analysis$wn_3998, c(0.3, 0.4, 0.6))
  expect_identical(by_vector$provenance$spectra_type, "csv")

  ## A directory read as CSV, told or detected, is its files
  expect_identical(by_dir$data$analysis, by_vector$data$analysis)
  expect_identical(spectra(dir, type = "csv")$data$analysis, by_vector$data$analysis)

})

test_that("spectra(recursive = TRUE) detects the type of a directory whose files sit in subdirectories", {

  top <- withr::local_tempdir()
  dir.create(file.path(top, "batch_1"))
  readr::write_csv(tibble::tibble(Sample_ID = "A", `4000` = 0.1, `3998` = 0.2),
                   file.path(top, "batch_1", "one.csv"))

  out <- spectra(top, recursive = TRUE)

  expect_identical(out$data$analysis$sample_id, "A")
  expect_identical(out$provenance$spectra_type, "csv")

  expect_error(spectra(top, recursive = "yes"), "`recursive` must be `TRUE` or `FALSE`",
               fixed = TRUE, class = "horizons_error_input")

})

test_that("spectra() refuses a source that mixes OPUS and CSV files, and names a missing path before reading any", {

  csv <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(Sample_ID = "A", `4000` = 0.1), csv)
  opus <- file.path(opus_dir(), "S02-1.0")

  expect_error(spectra(c(opus, csv)), "mixes OPUS and CSV files",
               fixed = TRUE, class = "horizons_error_input")

  ## Every path is checked before the first is read
  n_reads <- 0
  local_mocked_bindings(read_opus = function(dsn, ...) { n_reads <<- n_reads + 1; stop("read") },
                        .package = "opusreader2")

  err <- expect_error(spectra(c(opus, "/nonexistent/a.0", "/nonexistent/b.0")), "2 paths in `source` do not exist",
                      fixed = TRUE, class = "horizons_error_input")
  expect_match(conditionMessage(err), "/nonexistent/b.0", fixed = TRUE)
  expect_identical(n_reads, 0)

})

test_that("the progress bar of a read shows only under verbose = TRUE", {

  paths <- file.path(opus_dir(), c("S01-1.0", "S02-1.0"))

  ## Whether cli prints a bar depends on the terminal, so the calls are counted
  calls <- character(0)
  local_mocked_bindings(
    cli_progress_bar    = function(...) { calls <<- c(calls, "bar"); invisible("id") },
    cli_progress_update = function(...) { calls <<- c(calls, "update"); invisible("id") },
    cli_progress_done   = function(...) { calls <<- c(calls, "done"); invisible(TRUE) },
    .package = "cli"
  )

  utils::capture.output(spectra(paths, verbose = TRUE))
  expect_identical(calls, c("bar", "update", "update", "done"))

  calls <- character(0)
  spectra(paths, verbose = FALSE)
  expect_identical(calls, character(0))

})

test_that("a file that cannot be read is dropped by name, and the rest returned, with one partial_failure warning", {

  ## Arrange ---------------------------------------------------------------

  files <- local_opus_files(c("a.0", "b.0", "c.0"))
  local_reads(list(
    a.0 = list(ab = fake_block(FAKE_WN, c(1, 2, 3))),
    b.0 = function() stop("corrupt header"),
    c.0 = list(ab = fake_block(FAKE_WN, c(4, 5, 6)))
  ))

  ## Act -------------------------------------------------------------------

  w <- expect_warning(out <- spectra(files), "1 of 3 OPUS files could not be read",
                      class = "horizons_warning_partial_failure")

  ## Assert ----------------------------------------------------------------

  expect_identical(out$data$analysis$sample_id, c("a", "c"))
  expect_identical(out$data$analysis$wn_3998, c(2, 5))
  expect_match(conditionMessage(w), "File read failed: corrupt header (b.0)", fixed = TRUE)

  ## Recorded on the object, whether or not the warning is seen
  out <- suppressWarnings(spectra(files))
  row <- out$log[out$log$topic %in% "partial_failure", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$verb, "spectra")
  expect_identical(row$ids[[1]], "b.0")

})

test_that("spectra() aborts with every file's failure when none can be read, naming the blocks a file holds", {

  ## Arrange ---------------------------------------------------------------

  ## One file opusreader2 cannot read, one holding only a single-channel block
  files <- local_opus_files(c("a.0", "b.0"))
  local_reads(list(a.0 = function() stop("corrupt header"),
                   b.0 = list(sc_sample = fake_block(FAKE_WN, 1:3))))

  ## Act -------------------------------------------------------------------

  err <- expect_error(spectra(files), "None of the 2 OPUS files in `source` could be read",
                      fixed = TRUE, class = "horizons_error_read_failed")

  ## Assert ----------------------------------------------------------------

  expect_true(inherits(err, "horizons_error_run"))
  expect_identical(err$results$file, files)
  expect_identical(err$results$step, c("file_read", "file_block"))
  expect_identical(err$results$class[1], "simpleError")
  expect_identical(err$results$cause[1], "File read failed: corrupt header")
  expect_match(err$results$cause[2], 'holds no absorbance block ("ab" or "ab_no_atm_comp"). Its blocks: "sc_sample"',
               fixed = TRUE)

  ## The causes are in the message, not only on the condition
  expect_snapshot(spectra(files), error = TRUE)

})

test_that("a warning opusreader2 raises while reading is passed on once, as quality, not muffled", {

  files <- local_opus_files(c("a.0", "b.0"))
  read  <- function() {
    warning("odd byte at offset 12")
    list(file = list(ab = fake_block(FAKE_WN, c(1, 2, 3))))
  }
  local_reads(list(a.0 = read, b.0 = read))

  w <- expect_warning(out <- spectra(files), "A warning arose while reading: odd byte at offset 12",
                      class = "horizons_warning_quality")
  expect_match(conditionMessage(w), "In 2 files", fixed = TRUE)

  expect_identical(out$data$n_rows, 2L)
  expect_identical(out$log$ids[out$log$topic %in% "quality"][[1]], files)

})

test_that("the absorbance block is taken by name, ab before ab_no_atm_comp, never the first on the list (#191)", {

  ## Arrange ---------------------------------------------------------------

  ## A single-channel block first, then a parameter block whose name starts
  ## with ab, then the absorbance blocks in the other order
  files <- local_opus_files(c("a.0", "b.0", "c.0", "d.0"))
  local_reads(list(
    a.0 = list(sc_sample = fake_block(FAKE_WN, c(9, 9, 9)),
               ab_no_atm_comp_data_param = list(data = "parameters"),
               ab_no_atm_comp = fake_block(FAKE_WN, c(1, 2, 3))),
    b.0 = list(ab_no_atm_comp = fake_block(FAKE_WN, c(7, 7, 7)),
               AB = fake_block(FAKE_WN, c(4, 5, 6))),
    c.0 = list(sc_sample = fake_block(FAKE_WN, c(9, 9, 9)),
               ab_no_atm_comp_data_param = list(data = "parameters")),
    d.0 = list(ab = "not a block")
  ))

  ## Act -------------------------------------------------------------------

  w <- expect_warning(out <- spectra(files), class = "horizons_warning_partial_failure")

  ## Assert ----------------------------------------------------------------

  expect_identical(out$data$analysis$wn_4000, c(1, 4))
  expect_identical(out$data$analysis$wn_3996, c(3, 6))

  ## The file with neither is dropped, its blocks named
  expect_match(conditionMessage(w),
               'The file holds no absorbance block ("ab" or "ab_no_atm_comp"). Its blocks: "sc_sample" and "ab_no_atm_comp_data_param".',
               fixed = TRUE)

  ## A block in a form horizons does not read is dropped the same way
  expect_match(conditionMessage(w), 'The absorbance block "ab" is a <character>, which horizons does not read. (d.0)',
               fixed = TRUE)

})

test_that("scans whose wavenumbers drift by a fraction of the step are put on one grid, and average() then runs (#224)", {

  ## Arrange ---------------------------------------------------------------

  ## Two sessions on one instrument: the second axis 0.00066 cm-1 off the
  ## first at a 1.93 cm-1 step, and one point longer at the low end
  wn_1  <- 4000 - 1.93 * (0:9)
  wn_2  <- c(wn_1 + 0.00066, min(wn_1) - 1.93)
  files <- local_opus_files(c("S1-1.0", "S1-1.1"))
  local_reads(list(`S1-1.0` = list(ab = fake_block(wn_1, seq_along(wn_1))),
                   `S1-1.1` = list(ab = fake_block(wn_2, seq_along(wn_2) + 0.5))))

  ## Act -------------------------------------------------------------------

  w <- expect_warning(out <- spectra(files), class = "horizons_warning_substitution")

  ## Assert ----------------------------------------------------------------

  ## One column set: the first file's ten names, and the second's extra point
  expect_identical(predictor_names(out), paste0("wn_", c(wn_1, min(wn_1) - 1.93)))
  expect_false(anyNA(out$data$analysis[2, predictor_names(out)]))
  expect_identical(unname(unlist(out$data$analysis[2, paste0("wn_", wn_1)])), seq_along(wn_1) + 0.5)

  msg <- conditionMessage(w)
  expect_match(msg, "by at most 0.00066 cm-1", fixed = TRUE)
  expect_match(msg, '"S1-1.1"', fixed = TRUE)
  expect_identical(out$log$ids[out$log$topic %in% "substitution"][[1]], "S1-1.1")

  ## The documented order now runs: the two scans are one sample's replicates
  avg <- out |>
    parse_ids(format = "{sampleid}") |>
    average(quality_check = FALSE)
  expect_identical(avg$data$n_rows, 1L)
  expect_identical(avg$data$n_predictors, 11L)

})

test_that("the snap is a fraction of each file's step, and a drift past it keeps its own columns (#224)", {

  ## At a 4 cm-1 step, 0.03 cm-1 is under 1% of the step, so the names snap
  wn    <- seq(4000, by = -4, length.out = 6)
  files <- local_opus_files(c("a.0", "b.0", "c.0"))
  local_reads(list(a.0 = list(ab = fake_block(wn, 1:6)),
                   b.0 = list(ab = fake_block(wn + 0.03, 1:6)),
                   c.0 = list(ab = fake_block(wn + 0.12, 1:6))))

  ## 0.12 cm-1 is 3% of the step: c.0 keeps its own columns, NA for the others
  w   <- expect_warning(out <- spectra(files), class = "horizons_warning_substitution")
  ids <- out$log$ids[out$log$topic %in% "substitution"][[1]]

  expect_identical(ids, "b.0")
  expect_identical(out$data$n_predictors, 12L)
  expect_identical(sum(is.na(out$data$analysis[3, ])), 6L)

})

test_that("a file at another resolution keeps every name, even where its points come close to the grid's (#224)", {

  ## Arrange ---------------------------------------------------------------

  ## A 1.93 cm-1 axis and a 2 cm-1 one: four of the second's points fall
  ## within 1% of a step of a point of the first, by coincidence
  wn_1  <- 4000 - 1.93 * (0:199)
  wn_2  <- 4000.01 - 2 * (0:199)
  near  <- vapply(wn_2, function(v) min(abs(v - wn_1)), numeric(1)) <= 0.02
  expect_identical(sum(near), 4L)

  files <- local_opus_files(c("a.0", "b.0"))
  local_reads(list(a.0 = list(ab = fake_block(wn_1, seq_along(wn_1))),
                   b.0 = list(ab = fake_block(wn_2, seq_along(wn_2)))))

  ## Act -------------------------------------------------------------------

  out <- expect_no_warning(spectra(files))

  ## Assert ----------------------------------------------------------------

  ## Both axes whole, each file's values under its own names (one point,
  ## 3724.01, the two axes share exactly)
  expect_identical(out$data$n_predictors, length(union(paste0("wn_", wn_1), paste0("wn_", wn_2))))
  expect_identical(unname(unlist(out$data$analysis[2, paste0("wn_", wn_2)])), seq_along(wn_2))
  expect_false("substitution" %in% out$log$topic)

})

## ---------------------------------------------------------------------------
## spectra() — Validation and errors
## ---------------------------------------------------------------------------

test_that("spectra() errors on NULL source", {

  expect_error(
    spectra(NULL),
    "must be provided", class = "horizons_error_input"
  )

})

test_that("spectra() errors on non-existent file", {

  expect_error(
    spectra("/nonexistent/path/to/file.csv"),
    "does not exist", class = "horizons_error_input"
  )

})

test_that("spectra() errors when ID column not found", {

  test_data <- tibble::tibble(
    weird_column = c("A", "B"),
    `4000`       = c(0.1, 0.2)
  )

  expect_error(
    spectra(test_data),
    "Could not identify sample ID column", class = "horizons_error_input"
  )

})

test_that("spectra() errors when wavelength columns not found", {

  test_data <- tibble::tibble(
    Sample_ID = c("A", "B"),
    col_a     = c("x", "y"),
    col_b     = c("z", "w")
  )

  expect_error(
    spectra(test_data),
    "Could not identify wavelength columns", class = "horizons_error_input"
  )

})

test_that("spectra() errors on invalid id_col", {

  test_data <- tibble::tibble(
    Sample_ID = c("A"),
    `4000`    = c(0.1)
  )

  expect_error(
    spectra(test_data, id_col = "nonexistent"),
    "not found", class = "horizons_error_input"
  )

})

test_that("spectra() errors when wavelength columns contain non-numeric data", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("A", "B"),
    `4000`    = c("high", "low"),
    `3000`    = c(0.2, 0.3)
  )

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    spectra(test_data),
    "Wavelength columns must contain numeric data", class = "horizons_error_input"
  )

})

test_that("spectra() refuses a wavelength column whose name is not a wavenumber, given or detected (#121)", {

  ## The issue's case: an axis out of order, with one name no parser reads
  d <- tibble::tibble(Sample_ID = c("A", "B"), `602` = c(0.1, 0.2), `600` = c(0.3, 0.4),
                      X4000 = c(0.5, 0.6))

  err <- expect_error(spectra(d, wavelength_cols = c("602", "600", "X4000")),
                      "Wavelength column names must be wavenumbers", fixed = TRUE,
                      class = "horizons_error_input")
  expect_match(conditionMessage(err), "Not a wavenumber: \"X4000\"", fixed = TRUE)

  ## Detected by its prefix
  d2 <- tibble::tibble(Sample_ID = c("A", "B"), wn_4002 = c(0.1, 0.2), wn_X4000 = c(0.3, 0.4))
  expect_error(spectra(d2), "Not a wavenumber: \"wn_X4000\"", fixed = TRUE,
               class = "horizons_error_input")

})

test_that("spectra() refuses a source that is neither a path nor a data frame, and a type it does not read", {

  expect_error(spectra(42), "`source` must be a path or a data frame, not a number",
               fixed = TRUE, class = "horizons_error_input")

  ## Refused before the path is looked at: this one does not exist
  expect_error(spectra("/nonexistent/x.spa", type = "spa"), "`type` must be one of",
               fixed = TRUE, class = "horizons_error_input")

})

test_that("a CSV file whose columns cannot be read fails as a file, each cause keeping its remedy", {

  ## Arrange ---------------------------------------------------------------

  dir   <- withr::local_tempdir()
  write <- function(name, d) {
    path <- file.path(dir, name)
    readr::write_csv(d, path)
    path
  }

  chr   <- write("chr.csv", tibble::tibble(Sample_ID = c("A", "B"), `4000` = c("high", "low"), `3000` = c(0.2, 0.3)))
  noid  <- write("noid.csv", tibble::tibble(label = c("A", "B"), `4000` = c(0.1, 0.2)))
  nowl  <- write("nowl.csv", tibble::tibble(Sample_ID = c("A", "B"), col_a = c(1, 2)))
  ok    <- write("ok.csv", tibble::tibble(Sample_ID = c("A", "B"), `4000` = c(0.1, 0.2), X4000 = c(0.3, 0.4)))

  cases <- list(
    list(args = list(chr, id_col = "nonexistent"), cause = "Specified `id_col` not found in data.",
         remedy = 'Available columns: "Sample_ID", "4000", and "3000".'),
    list(args = list(chr, wavelength_cols = "9999"), cause = "Specified wavelength columns not found in data.",
         remedy = 'Missing: "9999".'),
    list(args = list(chr), cause = "Wavelength columns must contain numeric data.",
         remedy = "Check that the spectral columns hold absorbance or reflectance values."),
    list(args = list(noid), cause = "Could not identify sample ID column.",
         remedy = "Specify it with `id_col`."),
    list(args = list(nowl), cause = "Could not identify wavelength columns.",
         remedy = "Specify them with `wavelength_cols`."),
    list(args = list(ok, wavelength_cols = c("4000", "X4000")), cause = "Wavelength column names must be wavenumbers.",
         remedy = 'Not a wavenumber: "X4000".')
  )

  ## Act and assert --------------------------------------------------------

  ## One file is the common case: its abort is the file's failure, with the
  ## cause, its remedy and the file in the message
  for (case in cases) {

    err <- expect_error(do.call("spectra", c(case$args, verbose = FALSE)),
                        class = "horizons_error_read_failed")
    msg <- gsub("\\s+", " ", conditionMessage(err))

    expect_true(inherits(err, "horizons_error_run"), info = case$cause)
    expect_identical(rlang::call_name(err$call), "spectra")
    expect_match(msg, paste("Column check failed:", case$cause), fixed = TRUE, info = case$cause)
    expect_match(msg, case$remedy, fixed = TRUE, info = case$cause)
    expect_identical(err$results$file, case$args[[1]], info = case$cause)
    expect_identical(err$results$step, "file_columns", info = case$cause)

  }

  expect_snapshot(spectra(noid, verbose = FALSE), error = TRUE,
                  transform = function(x) gsub(dir, "<dir>", x, fixed = TRUE))

  ## A data frame's columns are the caller's, refused at the door
  expect_error(spectra(readr::read_csv(chr, show_col_types = FALSE)),
               "Wavelength columns must contain numeric data", fixed = TRUE, class = "horizons_error_input")

})

test_that("a CSV file whose columns cannot be read, of several, is dropped by name and the rest returned", {

  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(Sample_ID = "A", `4000` = 0.1, `3998` = 0.2), file.path(dir, "one.csv"))
  readr::write_csv(tibble::tibble(label = "B", `4000` = 0.3, `3998` = 0.4), file.path(dir, "two.csv"))

  ## Its id column is not one horizons knows
  w <- expect_warning(out <- spectra(dir, verbose = FALSE), "1 of 2 CSV files could not be read",
                      fixed = TRUE, class = "horizons_warning_partial_failure")
  msg <- gsub("\\s+", " ", conditionMessage(w))

  expect_identical(out$data$analysis$sample_id, "A")
  expect_match(msg, "Column check failed: Could not identify sample ID column.", fixed = TRUE)
  expect_match(msg, "Specify it with `id_col`.", fixed = TRUE)
  expect_match(msg, "two.csv)", fixed = TRUE)
  expect_no_match(msg, "File: ", fixed = TRUE)

  row <- out$log[out$log$topic %in% "partial_failure", ]
  expect_identical(row$ids[[1]], file.path(dir, "two.csv"))

  ## The id column named lies in one file only
  expect_warning(out <- spectra(dir, id_col = "Sample_ID", verbose = FALSE),
                 "Specified `id_col` not found in data", fixed = TRUE,
                 class = "horizons_warning_partial_failure")
  expect_identical(out$data$analysis$sample_id, "A")

})

test_that("a meta column of different types in different files is refused before the bind, naming it and the files", {

  ## Arrange ---------------------------------------------------------------

  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(Sample_ID = "A", depth = "0-10", `4000` = 0.1), file.path(dir, "one.csv"))
  readr::write_csv(tibble::tibble(Sample_ID = "B", depth = 10, `4000` = 0.2), file.path(dir, "two.csv"))
  readr::write_csv(tibble::tibble(Sample_ID = "C", depth = 20, `4000` = 0.3), file.path(dir, "three.csv"))

  ## Act and assert --------------------------------------------------------

  err <- expect_error(spectra(dir), "Column depth holds a different type in different files",
                      fixed = TRUE, class = "horizons_error_input")
  expect_identical(rlang::call_name(err$call), "spectra")

  msg <- conditionMessage(err)
  expect_match(msg, "<character> in .*one\\.csv")
  expect_match(msg, "<numeric> in .*three\\.csv.*two\\.csv")

  ## Types that bind (a column empty in one file reads as logical) pass
  readr::write_csv(tibble::tibble(Sample_ID = "A", depth = NA, `4000` = 0.1), file.path(dir, "one.csv"))
  expect_identical(spectra(dir)$data$analysis$depth, c(NA, 20, 10))

})

test_that("spectra() refuses a directory with neither OPUS nor CSV files, and a file whose type it cannot tell", {

  empty <- withr::local_tempdir()
  writeLines("not a spectrum", file.path(empty, "notes.txt"))

  ## Naming the type cannot help here, so the refusal does not suggest it
  err <- expect_error(spectra(empty), "holds no OPUS (.0, .1, ...) or CSV files",
                      fixed = TRUE, class = "horizons_error_input")
  expect_false(grepl("type", conditionMessage(err), fixed = TRUE))
  expect_identical(rlang::call_name(err$call), "spectra")

  err <- expect_error(spectra(file.path(empty, "notes.txt")), "Could not detect the type",
                      fixed = TRUE, class = "horizons_error_input")
  expect_match(conditionMessage(err), "Specify `type` explicitly", fixed = TRUE)

})


## ---------------------------------------------------------------------------
## spectra() — Structural validation at construction (#24)
## ---------------------------------------------------------------------------

test_that("spectra() passes duplicate sample ids silently, rather than aborting", {

  ## Arrange — two rows share the same id. This is normal at spectra(): two
  ## Bruker OPUS filenames like "S100-1.0" and "S100-1.1" both become
  ## "S100-1" once the extension is stripped, and average() is what collapses
  ## replicates. A hard error here would break the documented
  ## spectra() |> standardize() |> parse_ids() |> average() chain, so
  ## validate_horizons_data(stage = "raw") lets them pass (#24), and a later
  ## verb's refusal is not warned about here.
  test_data <- tibble::tibble(
    Sample_ID = c("A", "A"),
    `4000`    = c(0.1, 0.2),
    `3000`    = c(0.2, 0.3)
  )

  expect_no_warning(result <- spectra(test_data))

  ## The duplicates are kept, not dropped — average() is what resolves them
  expect_identical(result$data$analysis$sample_id, c("A", "A"))

})

test_that("spectra() sorts predictor columns into decreasing order when given increasing input", {

  ## Arrange — a KSSL-shaped snapshot, stored increasing
  test_data <- tibble::tibble(
    Sample_ID = c("A", "B"),
    `600`     = c(0.1, 0.2),
    `601`     = c(0.3, 0.4),
    `602`     = c(0.5, 0.6)
  )

  ## Act ---------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert — sorted rather than refused, and immediately valid
  predictor_vars <- result$data$role_map$variable[result$data$role_map$role == "predictor"]

  expect_identical(predictor_vars, c("wn_602", "wn_601", "wn_600"))
  expect_identical(names(result$data$analysis)[2:4], c("wn_602", "wn_601", "wn_600"))
  expect_no_error(validate_horizons_data(result))

  ## The values travelled with their names through the sort
  expect_identical(result$data$analysis$wn_600, c(0.1, 0.2))
  expect_identical(result$data$analysis$wn_602, c(0.5, 0.6))

})


## ---------------------------------------------------------------------------
## spectra() — S3 methods
## ---------------------------------------------------------------------------

test_that("summary.horizons_data produces output", {

  test_data <- tibble::tibble(
    Sample_ID = c("S1"),
    `4000`    = c(0.1)
  )

  result <- spectra(test_data)

  ## summary method uses cat() — capture stdout ------------------------------

  output <- capture.output(summary(result))

  expect_true(any(grepl("horizons_data", output)))
  expect_true(any(grepl("Samples", output)))
  expect_true(any(grepl("Predictors", output)))

})

test_that("wn_values() reads wavenumber names, with or without the prefix, and NA for the rest without warning", {

  names <- c("wn_600", "4000", "wn_600.5", "wn_1e3", "Sample_ID", "wn_", "wn_abc", NA)

  expect_no_warning(values <- wn_values(names))
  expect_identical(values, c(600, 4000, 600.5, 1000, NA, NA, NA, NA))

})
