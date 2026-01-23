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

  ## Test different aliases ------------------------------------------------

  aliases_to_test <- c("sample_id", "Sample_ID", "ID", "id", "Sample", "filename")

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
  expect_equal(result$provenance$schema_version, 1L)

})


## ---------------------------------------------------------------------------
## spectra() — Role map structure
## ---------------------------------------------------------------------------

test_that("spectra() creates correct role_map structure", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S1"),
    meta1     = c("x"),
    `4000`    = c(0.1),
    `3000`    = c(0.2)
  )

  ## Act -------------------------------------------------------------------

  result <- spectra(test_data)

  ## Assert ----------------------------------------------------------------

  role_map <- result$data$role_map

  expect_s3_class(role_map, "tbl_df")
  expect_true("variable" %in% names(role_map))
  expect_true("role" %in% names(role_map))

  ## Check role assignments ------------------------------------------------

  expect_equal(role_map$role[role_map$variable == "sample_id"], "id")
  expect_equal(role_map$role[role_map$variable == "wn_4000"], "predictor")
  expect_equal(role_map$role[role_map$variable == "meta1"], "meta")

})


## ---------------------------------------------------------------------------
## spectra() — Validation and errors
## ---------------------------------------------------------------------------

test_that("spectra() errors on NULL source", {

  expect_error(
    spectra(NULL),
    "must be provided"
  )

})

test_that("spectra() errors on non-existent file", {

  expect_error(
    spectra("/nonexistent/path/to/file.csv"),
    "does not exist"
  )

})

test_that("spectra() errors when ID column not found", {

  test_data <- tibble::tibble(
    weird_column = c("A", "B"),
    `4000`       = c(0.1, 0.2)
  )

  expect_error(
    spectra(test_data),
    "Could not identify sample ID column"
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
    "Could not identify wavelength columns"
  )

})

test_that("spectra() errors on invalid id_col", {

  test_data <- tibble::tibble(
    Sample_ID = c("A"),
    `4000`    = c(0.1)
  )

  expect_error(
    spectra(test_data, id_col = "nonexistent"),
    "not found"
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
    "Wavelength columns must contain numeric data"
  )

})


## ---------------------------------------------------------------------------
## spectra() — S3 methods
## ---------------------------------------------------------------------------

test_that("print.horizons_data produces output", {

  test_data <- tibble::tibble(
    Sample_ID = c("S1", "S2"),
    `4000`    = c(0.1, 0.2),
    `3000`    = c(0.2, 0.3)
  )

  result <- spectra(test_data)

  ## Use capture.output since cli doesn't go to standard output ---------------

  output <- capture.output(print(result), type = "message")

  expect_true(any(grepl("horizons_data", output)))
  expect_true(any(grepl("2 samples", output)))
  expect_true(any(grepl("2 wavelengths", output)))

})

test_that("summary.horizons_data produces output", {

  test_data <- tibble::tibble(
    Sample_ID = c("S1"),
    `4000`    = c(0.1)
  )

  result <- spectra(test_data)

  ## Use capture.output since cli doesn't go to standard output ---------------

  output <- capture.output(summary(result), type = "message")

  expect_true(any(grepl("horizons_data", output)))
  expect_true(any(grepl("Samples", output)))
  expect_true(any(grepl("Wavelengths", output)))

})


## ---------------------------------------------------------------------------
## spectra() — CSV file loading
## ---------------------------------------------------------------------------

test_that("spectra() loads CSV file", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002", "S003"),
    `4000`    = c(0.1, 0.2, 0.3),
    `3000`    = c(0.2, 0.3, 0.4)
  )

  temp_csv <- tempfile(fileext = ".csv")
  readr::write_csv(test_data, temp_csv)

  ## Act -------------------------------------------------------------------

  result <- spectra(temp_csv)

  ## Assert ----------------------------------------------------------------

  expect_s3_class(result, "horizons_data")
  expect_equal(result$data$n_rows, 3)
  expect_equal(result$provenance$spectra_type, "csv")

  ## Cleanup ---------------------------------------------------------------

  unlink(temp_csv)

})

test_that("spectra() CSV does NOT add filename column", {

  ## Arrange ---------------------------------------------------------------

  test_data <- tibble::tibble(
    Sample_ID = c("S001", "S002"),
    `4000`    = c(0.1, 0.2)
  )

  temp_csv <- tempfile(fileext = ".csv")
  readr::write_csv(test_data, temp_csv)

  ## Act -------------------------------------------------------------------

  result <- spectra(temp_csv)

  ## Assert ----------------------------------------------------------------

  ## No filename column for CSV (no per-sample files) ----------------------

  expect_false("filename" %in% names(result$data$analysis))

  ## Cleanup ---------------------------------------------------------------

  unlink(temp_csv)

})
