#' Tests for read_spectra() Function
#'
#' STRATEGY: Integration-first (CSV workflow is fast and testable end-to-end)
#'
#' read_spectra() is an orchestrator that coordinates file reading workflows,
#' making it perfect for integration testing. Unlike finalize/ensemble functions,
#' file reading is fast (~1-2 seconds), so we can test complete workflows.
#'
#' Integration tests (70%): Test complete CSV/OPUS reading pipelines
#' Validation tests (30%): Test error handling and parameter validation

library(testthat)
library(horizons)

## ===========================================================================
## INTEGRATION TESTS - CSV Reading Workflow
## ===========================================================================

test_that("minimal CSV reading executes end-to-end", {
  # SPEC-READ-INT-001: Complete CSV reading workflow

  # Create test spectral data
  test_data <- make_test_spectra(
    n_samples = 10,
    wavelengths = seq(600, 4000, by = 100),  # Coarse for speed
    seed = 123
  )

  # Write to temporary CSV file
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  # Read with read_spectra
  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    spectra_type = "MIR",
    verbose = FALSE
  )

  # Verify structure
  expect_s3_class(result, "tbl_df")
  expect_true("Sample_ID" %in% names(result))
  expect_equal(nrow(result), 10)

  # Verify spectral columns exist
  spectral_cols <- setdiff(names(result), c("Sample_ID", "Response", "Project"))
  expect_true(length(spectral_cols) > 0)

  # Verify all spectral columns are numeric
  expect_true(all(sapply(result[spectral_cols], is.numeric)))

  # Cleanup
  unlink(temp_csv)
})

test_that("CSV reading preserves data integrity", {
  # SPEC-READ-INT-002: Data values are preserved accurately

  test_data <- make_test_spectra(n_samples = 5, seed = 456)
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    verbose = FALSE
  )

  # Verify Sample_IDs preserved
  expect_setequal(result$Sample_ID, test_data$Sample_ID)

  # Verify spectral values preserved (within numerical tolerance)
  spectral_cols <- setdiff(names(test_data), c("Sample_ID", "Response", "Project"))
  for (col in spectral_cols) {
    if (col %in% names(result)) {
      expect_equal(result[[col]], test_data[[col]], tolerance = 1e-6)
    }
  }

  unlink(temp_csv)
})

test_that("CSV reading adds correct attributes", {
  # SPEC-READ-INT-003: Metadata attributes are added correctly

  test_data <- make_test_spectra(n_samples = 5, seed = 789)
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    spectra_type = "MIR",
    verbose = FALSE
  )

  # Verify attributes
  expect_equal(attr(result, "source"), "csv")
  expect_equal(attr(result, "spectra_type"), "MIR")
  expect_equal(attr(result, "source_path"), temp_csv)

  unlink(temp_csv)
})

test_that("CSV reading works with MIR spectra type", {
  # SPEC-READ-INT-004: MIR spectroscopy workflow

  test_data <- make_test_spectra(
    n_samples = 8,
    wavelengths = seq(400, 4000, by = 4),  # Typical MIR range
    seed = 111
  )

  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    spectra_type = "MIR",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(attr(result, "spectra_type"), "MIR")
  expect_equal(nrow(result), 8)

  unlink(temp_csv)
})

test_that("CSV reading works with NIR spectra type", {
  # SPEC-READ-INT-005: NIR spectroscopy workflow

  test_data <- make_test_spectra(
    n_samples = 6,
    wavelengths = seq(800, 2500, by = 2),  # Typical NIR range
    seed = 222
  )

  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    spectra_type = "NIR",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(attr(result, "spectra_type"), "NIR")
  expect_equal(nrow(result), 6)

  unlink(temp_csv)
})

test_that("CSV reading handles different sample counts", {
  # SPEC-READ-INT-006: Scalability across sample sizes

  for (n in c(1, 5, 20, 50)) {
    test_data <- make_test_spectra(n_samples = n, seed = n * 100)
    temp_csv <- tempfile(fileext = ".csv")
    write.csv(test_data, temp_csv, row.names = FALSE)

    result <- read_spectra(
      source = "csv",
      spectra_path = temp_csv,
      verbose = FALSE
    )

    expect_equal(nrow(result), n,
                 info = paste("Failed for n =", n))

    unlink(temp_csv)
  }
})

test_that("CSV reading verbose mode produces output", {
  # SPEC-READ-INT-007: Verbose output is generated
  skip("Verbose output testing requires verification of read_csv_internal implementation")

  test_data <- make_test_spectra(n_samples = 5, seed = 333)
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  # Capture output
  expect_output(
    read_spectra(
      source = "csv",
      spectra_path = temp_csv,
      verbose = TRUE
    ),
    "Spectral Data Reading Pipeline"
  )

  expect_output(
    read_spectra(
      source = "csv",
      spectra_path = temp_csv,
      verbose = TRUE
    ),
    "Samples.*processed"
  )

  unlink(temp_csv)
})

test_that("CSV reading non-verbose mode suppresses output", {
  # SPEC-READ-INT-008: verbose=FALSE suppresses messages
  skip("Verbose output testing requires verification of read_csv_internal implementation")

  test_data <- make_test_spectra(n_samples = 5, seed = 444)
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  # Should not produce output
  expect_silent(
    read_spectra(
      source = "csv",
      spectra_path = temp_csv,
      verbose = FALSE
    )
  )

  unlink(temp_csv)
})

test_that("CSV reading returns tibble with correct column types", {
  # SPEC-READ-INT-009: Output data types are correct

  test_data <- make_test_spectra(n_samples = 10, seed = 555)
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    verbose = FALSE
  )

  # Sample_ID should be character
  expect_type(result$Sample_ID, "character")

  # All spectral columns should be numeric
  spectral_cols <- setdiff(names(result), c("Sample_ID", "Response", "Project"))
  for (col in spectral_cols) {
    expect_true(is.numeric(result[[col]]),
                info = paste("Column", col, "should be numeric"))
  }

  unlink(temp_csv)
})

test_that("CSV reading handles wide spectral data", {
  # SPEC-READ-INT-010: Large number of wavelengths

  test_data <- make_test_spectra(
    n_samples = 10,
    wavelengths = seq(600, 4000, by = 2),  # ~1700 wavelengths
    seed = 666
  )

  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  result <- read_spectra(
    source = "csv",
    spectra_path = temp_csv,
    verbose = FALSE
  )

  # Should handle wide data
  expect_true(ncol(result) > 1000)
  expect_equal(nrow(result), 10)

  unlink(temp_csv)
})

## ===========================================================================
## VALIDATION TESTS - Parameter checking and error handling
## ===========================================================================

test_that("read_spectra validates source parameter", {
  # SPEC-READ-VAL-001: Source parameter validation
  expect_error(
    read_spectra(source = "invalid", spectra_path = "dummy.csv"),
    "should be one of"
  )
})

test_that("read_spectra validates spectra_type parameter", {
  # SPEC-READ-VAL-002: Spectra type validation
  expect_error(
    read_spectra(source = "csv", spectra_path = "dummy.csv", spectra_type = "invalid"),
    "should be one of"
  )
})

test_that("read_spectra handles missing file", {
  # SPEC-READ-VAL-003: File existence checking
  expect_error(
    read_spectra(source = "csv", spectra_path = "nonexistent.csv"),
    "does not exist"
  )
})

test_that("read_spectra requires spectra_path", {
  # SPEC-READ-VAL-004: Required parameter check
  expect_error(
    read_spectra(source = "csv", spectra_path = NULL),
    "must be provided"
  )
})

test_that("read_spectra validates verbose parameter", {
  # SPEC-READ-VAL-005: verbose must be logical

  test_data <- make_test_spectra(n_samples = 3, seed = 777)
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  expect_error(
    read_spectra(
      source = "csv",
      spectra_path = temp_csv,
      verbose = "yes"
    ),
    "logical"
  )

  expect_error(
    read_spectra(
      source = "csv",
      spectra_path = temp_csv,
      verbose = c(TRUE, FALSE)
    ),
    "single"
  )

  unlink(temp_csv)
})

test_that("read_spectra validates CSV has .csv extension warning", {
  # SPEC-READ-VAL-006: Extension validation for CSV

  test_data <- make_test_spectra(n_samples = 3, seed = 888)
  temp_file <- tempfile(fileext = ".txt")  # Wrong extension
  write.csv(test_data, temp_file, row.names = FALSE)

  expect_warning(
    read_spectra(
      source = "csv",
      spectra_path = temp_file,
      verbose = FALSE
    ),
    "does not have .csv extension"
  )

  unlink(temp_file)
})

test_that("read_spectra rejects directory for CSV source", {
  # SPEC-READ-VAL-007: CSV source requires file, not directory

  temp_dir <- tempdir()

  expect_error(
    read_spectra(
      source = "csv",
      spectra_path = temp_dir,
      verbose = FALSE
    ),
    "requires a file path, not a directory"
  )
})

## ===========================================================================
## INTEGRATION TESTS - OPUS Reading Workflow (Skipped)
## ===========================================================================
# NOTE: OPUS integration tests require actual OPUS binary files (.0, .1, etc.)
# which are not included in the test suite. These tests document expected
# behavior for future testing with real OPUS files.

test_that("OPUS single file reading executes end-to-end", {
  # SPEC-READ-OPUS-INT-001: Complete OPUS single file workflow
  skip("Requires actual OPUS binary files - not available in test suite")

  # Expected workflow:
  # 1. read_spectra() validates OPUS file exists
  # 2. Calls read_opus_internal() to extract spectra
  # 3. Returns tibble with Sample_ID + numeric wavenumber columns
  # 4. Attaches attributes: source="opus", spectra_type, source_path
})

test_that("OPUS directory reading executes end-to-end", {
  # SPEC-READ-OPUS-INT-002: Directory with multiple OPUS files
  skip("Requires actual OPUS binary files - not available in test suite")

  # Expected workflow:
  # 1. read_spectra() detects directory with .0 files
  # 2. Processes each OPUS file in directory
  # 3. Combines into single tibble
  # 4. Each Sample_ID extracted from filename
})

test_that("OPUS reading preserves spectral integrity", {
  # SPEC-READ-OPUS-INT-003: Spectral values preserved accurately
  skip("Requires actual OPUS binary files - not available in test suite")

  # Expected behavior:
  # - Absorbance values preserved from binary format
  # - Wavenumber columns correctly labeled
  # - No data loss or corruption during extraction
})

test_that("OPUS reading adds correct attributes", {
  # SPEC-READ-OPUS-INT-004: Metadata attributes for OPUS
  skip("Requires actual OPUS binary files - not available in test suite")

  # Expected attributes:
  # - source = "opus"
  # - spectra_type = "MIR" or "NIR"
  # - source_path = path to OPUS file/directory
})

test_that("OPUS reading works with MIR spectra", {
  # SPEC-READ-OPUS-INT-005: MIR OPUS workflow
  skip("Requires actual OPUS binary files - not available in test suite")

  # Expected workflow for MIR OPUS files:
  # - Reads wavenumbers in MIR range (typically 400-4000 cm⁻¹)
  # - Extracts absorbance channel
  # - Returns properly formatted tibble
})

test_that("OPUS reading works with NIR spectra", {
  # SPEC-READ-OPUS-INT-006: NIR OPUS workflow
  skip("Requires actual OPUS binary files - not available in test suite")

  # Expected workflow for NIR OPUS files:
  # - Reads wavenumbers in NIR range (typically 800-2500 cm⁻¹)
  # - Extracts absorbance channel
  # - Returns properly formatted tibble
})

test_that("OPUS reading handles channel selection", {
  # SPEC-READ-OPUS-INT-007: Automatic channel selection
  skip("Requires actual OPUS binary files - not available in test suite")

  # Expected behavior:
  # - OPUS files contain multiple channels (ScSm, ScRf, etc.)
  # - read_opus_internal() selects optimal channel (typically absorbance)
  # - User doesn't need to specify channel manually
})

## ===========================================================================
## VALIDATION TESTS - OPUS-specific error handling
## ===========================================================================

test_that("read_spectra validates OPUS file extension", {
  # SPEC-READ-OPUS-VAL-001: OPUS file extension validation

  temp_file <- tempfile(fileext = ".txt")
  writeLines("fake opus data", temp_file)

  expect_error(
    read_spectra(
      source = "opus",
      spectra_path = temp_file,
      verbose = FALSE
    ),
    "does not appear to be an OPUS file"
  )

  unlink(temp_file)
})

test_that("read_spectra handles empty OPUS directory", {
  # SPEC-READ-OPUS-VAL-002: Empty directory validation

  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_error(
    read_spectra(
      source = "opus",
      spectra_path = temp_dir,
      verbose = FALSE
    ),
    "No OPUS files found"
  )

  unlink(temp_dir, recursive = TRUE)
})
