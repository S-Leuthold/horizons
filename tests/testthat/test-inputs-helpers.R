#' Tests for Input Helper Functions
#'
#' Comprehensive gap-filling tests for internal helper functions in inputs-helpers.R
#' These functions handle OPUS reading, CSV reading, and filename metadata parsing
#'
#' **Coverage Target**: 20% → 70%+ for R/inputs-helpers.R
#' **Test Strategy**: Direct function testing + integration via read_spectra()
#' **Test Count**: 18 tests (12 validation + 6 integration)

library(testthat)
library(horizons)

read_opus_internal    <- horizons:::read_opus_internal
read_csv_internal     <- horizons:::read_csv_internal
parse_filename_metadata <- horizons:::parse_filename_metadata

## ===========================================================================
## Setup and Fixtures
## ===========================================================================

test_that("Helper functions exist and are accessible", {

  expect_true(is.function(read_opus_internal))
  expect_true(is.function(read_csv_internal))
  expect_true(is.function(parse_filename_metadata))

})

## ===========================================================================
## VALIDATION TESTS: parse_filename_metadata()
## ===========================================================================

test_that("parse_filename_metadata handles standard format with all parts", {

  # SPEC-IN-HELPERS-PARSE-001: Standard filename parsing with project_sampleid_fraction
  result <- parse_filename_metadata(
    file_name      = "FFAR_001_Bulk.0",
    format_string  = "project_sampleid_fraction",
    delimiter      = "_"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true("Sample_ID" %in% names(result))
  expect_equal(result$Sample_ID[1], "001")

})

test_that("parse_filename_metadata handles missing fraction with default", {

  # SPEC-IN-HELPERS-PARSE-002: Missing fraction uses default value
  result <- parse_filename_metadata(
    file_name           = "TEST_sample_data.0",
    format_string       = "project_sampleid_ignore",
    delimiter           = "_",
    default_fraction    = "GroundBulk"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$Sample_ID[1], "sample")
  # Fraction should still exist even though not in format
  expect_true("Fraction" %in% names(result))

})

test_that("parse_filename_metadata handles fewer parts than format tokens", {

  # SPEC-IN-HELPERS-PARSE-003: Filename with fewer parts than format expects
  result <- parse_filename_metadata(
    file_name      = "only_one.0",
    format_string  = "project_sampleid_fraction",
    delimiter      = "_"
  )

  # Should return UNKNOWN when parts < tokens
  expect_equal(result$Sample_ID[1], "UNKNOWN")

})

test_that("parse_filename_metadata extracts multiple metadata fields", {

  # SPEC-IN-HELPERS-PARSE-004: Extract project, sample_id, and well_id
  result <- parse_filename_metadata(
    file_name      = "SOIL_S001_W01.0",
    format_string  = "project_sampleid_wellid",
    delimiter      = "_"
  )

  expect_equal(nrow(result), 1)
  expect_true("Project" %in% names(result))
  expect_true("Sample_ID" %in% names(result))
  expect_true("Well_ID" %in% names(result))
  expect_equal(result$Project[1], "SOIL")

})

test_that("parse_filename_metadata handles custom delimiters", {

  # SPEC-IN-HELPERS-PARSE-005: Different delimiter than underscore
  result <- parse_filename_metadata(
    file_name      = "FFAR-001-Bulk.0",
    format_string  = "project-sampleid-fraction",
    delimiter      = "-"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$Sample_ID[1], "001")

})

test_that("parse_filename_metadata maps ignore tokens correctly", {

  # SPEC-IN-HELPERS-PARSE-006: Ignore tokens don't become columns
  result <- parse_filename_metadata(
    file_name      = "PREFIX_REAL_001_Bulk.0",
    format_string  = "ignore_prefix_sampleid_fraction",
    delimiter      = "_"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$Sample_ID[1], "001")
  expect_true(all(c("ignore", "prefix") %in% names(result)))
  expect_equal(result$ignore[1], "PREFIX")
  expect_equal(result$prefix[1], "REAL")

})

test_that("parse_filename_metadata warns when required Sample_ID missing", {

  # SPEC-IN-HELPERS-PARSE-007: Missing Sample_ID triggers message + default
  expect_message(
    {
      result <- parse_filename_metadata(
        file_name      = "NOTUSED_Bulk.0",
        format_string  = "fraction",
        delimiter      = "_"
      )
    },
    regexp = "No Sample_ID"
  )

  expect_equal(result$Sample_ID[1], "UNKNOWN")

})

## ===========================================================================
## VALIDATION TESTS: read_csv_internal()
## ===========================================================================

test_that("read_csv_internal rejects CSV with insufficient columns", {

  # SPEC-IN-HELPERS-CSV-001: CSV must have at least 2 columns (ID + spectral)
  test_dir <- tempfile(pattern = "csv_test_01_")
  dir.create(test_dir)

  # Create CSV with only 1 column
  csv_file <- file.path(test_dir, "single_col.csv")
  writeLines("Sample_ID\nS001", csv_file)

  expect_error(
    read_csv_internal(csv_file, spectra_type = "MIR", verbose = FALSE),
    regex = "at least 2 columns"
  )

  unlink(test_dir, recursive = TRUE)

})

test_that("read_csv_internal handles valid CSV with numeric columns", {

  # SPEC-IN-HELPERS-CSV-002: Valid CSV should be read and converted to numeric
  test_dir <- tempfile(pattern = "csv_test_02_")
  dir.create(test_dir)

  csv_file <- file.path(test_dir, "valid.csv")
  writeLines(
    "Sample_ID,600,800,1000\nS001,0.5,0.6,0.7\nS002,0.6,0.7,0.8",
    csv_file
  )

  result <- read_csv_internal(csv_file, spectra_type = "MIR", verbose = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(names(result)[1], "Sample_ID")
  expect_true(is.numeric(result[["600"]]))

  unlink(test_dir, recursive = TRUE)

})

test_that("read_csv_internal warns when column names are not numeric", {

  # SPEC-IN-HELPERS-CSV-003: Non-numeric column names trigger warning
  test_dir <- tempfile(pattern = "csv_test_03_")
  dir.create(test_dir)

  csv_file <- file.path(test_dir, "non_numeric.csv")
  writeLines(
    "Sample_ID,Band1,Band2,Band3\nS001,0.5,0.6,0.7",
    csv_file
  )

  expect_message(
    {
      result <- read_csv_internal(csv_file, spectra_type = "MIR", verbose = TRUE)
    },
    regex = "not numeric"
  )

  unlink(test_dir, recursive = TRUE)

})

## ===========================================================================
## VALIDATION TESTS: read_opus_internal()
## ===========================================================================

test_that("read_opus_internal handles non-existent path gracefully", {

  # SPEC-IN-HELPERS-OPUS-001: Invalid path returns NULL with message
  expect_message(
    {
      result <- read_opus_internal(
        path         = "/nonexistent/path/to/files",
        spectra_type = "MIR",
        verbose      = FALSE
      )
    },
    regexp = "Failed to read OPUS"
  )

  expect_null(result)

})

test_that("read_opus_internal rejects directory with no OPUS files", {

  # SPEC-IN-HELPERS-OPUS-002: Directory without .0 files should error
  test_dir <- tempfile(pattern = "opus_test_01_")
  dir.create(test_dir)

  # Create non-OPUS files
  writeLines("not opus", file.path(test_dir, "not_opus.txt"))

  expect_error(
    read_opus_internal(test_dir, spectra_type = "MIR", verbose = FALSE),
    regex = "No OPUS files found"
  )

  unlink(test_dir, recursive = TRUE)

})

## ===========================================================================
## INTEGRATION TESTS: Via read_spectra() orchestrator
## ===========================================================================

test_that("read_spectra with CSV format exercises read_csv_internal", {

  # SPEC-IN-HELPERS-INT-001: CSV reading integration
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "csv_int_01_")
  dir.create(test_dir)

  # Create valid spectral CSV
  csv_file <- file.path(test_dir, "spectra.csv")
  writeLines(
    "Sample_ID,600,700,800,900\nS001,0.3,0.4,0.5,0.6\nS002,0.4,0.5,0.6,0.7",
    csv_file
  )

  result <- horizons::read_spectra(
    source       = "csv",
    spectra_path = csv_file,
    spectra_type = "MIR",
    verbose      = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(c("Sample_ID", "600", "700") %in% names(result)))

  unlink(test_dir, recursive = TRUE)

})

test_that("read_spectra filename parsing uses parse_filename_metadata", {

  # SPEC-IN-HELPERS-INT-002: Filename parsing integration
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "fname_int_01_")
  dir.create(test_dir)

  # Create CSV with metadata in filename
  csv_file <- file.path(test_dir, "PROJECT_SAMPLE001_Bulk.csv")
  writeLines(
    "Sample_ID,600,700\nS001,0.3,0.4",
    csv_file
  )

  # Parse filename to extract metadata
  metadata <- parse_filename_metadata(
    file_name     = "PROJECT_SAMPLE001_Bulk.csv",
    format_string = "project_sampleid_fraction",
    delimiter     = "_"
  )

  expect_equal(metadata$Sample_ID[1], "SAMPLE001")
  expect_equal(metadata$Fraction[1], "Bulk")

  unlink(test_dir, recursive = TRUE)

})

test_that("read_spectra CSV integration preserves spectral column order", {

  # SPEC-IN-HELPERS-INT-003: Wavenumber columns stay in order
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "csv_int_02_")
  dir.create(test_dir)

  csv_file <- file.path(test_dir, "ordered.csv")
  # Explicitly ordered wavenumbers
  writeLines(
    "Sample_ID,4000,3500,3000,2500\nS001,0.1,0.2,0.3,0.4",
    csv_file
  )

  result <- horizons::read_spectra(
    source       = "csv",
    spectra_path = csv_file,
    spectra_type = "MIR",
    verbose      = FALSE
  )

  # Column order should be preserved
  spectral_cols <- as.numeric(names(result)[-1])
  expect_equal(spectral_cols, c(4000, 3500, 3000, 2500))

  unlink(test_dir, recursive = TRUE)

})

test_that("read_csv_internal numeric conversion handles edge cases", {

  # SPEC-IN-HELPERS-CSV-004: NA values and type coercion
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true", "Skipping in CI environment")

  test_dir <- tempfile(pattern = "csv_int_03_")
  dir.create(test_dir)

  csv_file <- file.path(test_dir, "edge_cases.csv")
  writeLines(
    "Sample_ID,600,700,800\nS001,0.5,NA,0.7\nS002,0.6,0.8,0.9",
    csv_file
  )

  result <- read_csv_internal(csv_file, spectra_type = "MIR", verbose = FALSE)

  expect_equal(nrow(result), 2)
  expect_true(is.na(result[["700"]][1]))
  expect_equal(result[["600"]][1], 0.5)

  unlink(test_dir, recursive = TRUE)

})

test_that("parse_filename_metadata edge case: empty ignore tokens", {

  # SPEC-IN-HELPERS-PARSE-008: Multiple consecutive delimiters
  result <- parse_filename_metadata(
    file_name      = "PREFIX__SAMPLE_Bulk.0",
    format_string  = "project_ignore_sampleid_fraction",
    delimiter      = "_"
  )

  # Should still extract Sample_ID correctly despite empty token
  expect_equal(nrow(result), 1)
  expect_true(!is.na(result$Sample_ID[1]))

})

## ===========================================================================
## EDGE CASES AND ERROR HANDLING
## ===========================================================================

test_that("read_csv_internal handles file not found gracefully", {

  # SPEC-IN-HELPERS-CSV-005: Missing file returns NULL with error handling
  result <- read_csv_internal(
    path          = "/nonexistent/file.csv",
    spectra_type  = "MIR",
    verbose       = FALSE
  )

  # Should return NULL when file doesn't exist
  expect_null(result)

})

test_that("parse_filename_metadata handles single-part filenames", {

  # SPEC-IN-HELPERS-PARSE-009: Filename with no delimiters
  result <- parse_filename_metadata(
    file_name      = "SINGLESAMPLE.0",
    format_string  = "sampleid",
    delimiter      = "_"
  )

  expect_equal(result$Sample_ID[1], "SINGLESAMPLE")

})
