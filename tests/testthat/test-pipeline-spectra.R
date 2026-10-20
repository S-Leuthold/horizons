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

})

test_that("spectra() prints its banner, with the sorting line only when it sorts", {

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002", "S003"),
    `4000`    = c(0.1, 0.2, 0.3),
    `3000`    = c(0.2, 0.3, 0.4),
    `2000`    = c(0.3, 0.4, 0.5)
  )

  expect_snapshot({
    invisible(spectra(test_data, verbose = TRUE))
    invisible(spectra(test_data[, c("Sample_ID", "2000", "4000", "3000")], verbose = TRUE))
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

test_that("spectra() refuses OPUS files that hold no spectral block", {

  ## Arrange ---------------------------------------------------------------

  ## A file opusreader2 reads but that carries only metadata cannot be cut
  ## from a real scan cheaply, so read_opus() is stubbed to return that.
  local_mocked_bindings(
    read_opus = function(dsn, ...) {
      list(file = list(basic_metadata = data.frame(dsn_filename = basename(dsn))))
    },
    .package = "opusreader2"
  )

  ## Act and assert --------------------------------------------------------

  ## Each file warns that it has no data block; then the read is refused.
  ## This abort carries no package class, so the message is the check.
  expect_error(
    suppressWarnings(spectra(opus_dir(), type = "opus")),
    "No valid spectral data could be extracted",
    fixed = TRUE
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

test_that("spectra() refuses a vector of paths, and a directory read as a CSV", {

  ## These aborts carry no package class, so the message is the check.
  expect_error(
    spectra(c("a.csv", "b.csv")),
    "`source` must be a single path, not a vector of length 2",
    fixed = TRUE
  )

  expect_error(
    spectra(withr::local_tempdir(), type = "csv"),
    "Expected CSV file, got directory",
    fixed = TRUE
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

test_that("spectra() refuses a source that is neither a path nor a data frame, and a type it does not read", {

  expect_error(spectra(42), "`source` must be a path or a data frame, not a number",
               fixed = TRUE, class = "horizons_error_input")

  ## Refused before the path is looked at: this one does not exist
  expect_error(spectra("/nonexistent/x.spa", type = "spa"), "`type` must be one of",
               fixed = TRUE, class = "horizons_error_input")

})

test_that("a CSV's columns are checked as a data frame's are, and the refusal names spectra()", {

  ## Arrange ---------------------------------------------------------------

  csv <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(Sample_ID = c("A", "B"), `4000` = c("high", "low"),
                                  `3000` = c(0.2, 0.3)), csv)

  ## Act & Assert ----------------------------------------------------------

  err <- expect_error(spectra(csv, id_col = "nonexistent"), "Specified `id_col` not found in data",
                      fixed = TRUE, class = "horizons_error_input")
  expect_identical(rlang::call_name(err$call), "spectra")

  expect_error(spectra(csv, wavelength_cols = "9999"), "Specified wavelength columns not found in data",
               fixed = TRUE, class = "horizons_error_input")

  err <- expect_error(spectra(csv), "Wavelength columns must contain numeric data",
                      fixed = TRUE, class = "horizons_error_input")
  expect_identical(rlang::call_name(err$call), "spectra")

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
