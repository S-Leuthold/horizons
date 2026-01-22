# tests/testthat/test-pipeline-spectra.R
# Tests for spectra() — the entry point for loading spectral data
#
# Test organization:
#   - Input type detection
#   - OPUS file reading
#   - CSV file reading
#   - Tibble input handling
#   - ID column detection
#   - Wavelength column detection
#   - Column prefixing (wn_)
#   - Error handling
#   - CLI output


# ==============================================================================
# Test Fixtures
# ==============================================================================

#' Create a minimal test tibble with spectral data
#'
#' @param n_samples Number of samples (rows)
#' @param n_wavelengths Number of wavelength columns
#' @param id_col Name for the ID column
#' @param wn_prefix Whether to use wn_ prefix on wavelength columns
#' @param extra_cols Character vector of extra column names to add
make_test_spectra_tibble <- function(n_samples = 10,
                                     n_wavelengths = 50,
                                     id_col = "Sample_ID",
                                     wn_prefix = FALSE,
                                     extra_cols = NULL) {

 # Generate wavelength column names
 wn_values <- seq(4000, by = -2, length.out = n_wavelengths)

 if (wn_prefix) {
   wn_names <- paste0("wn_", wn_values)
 } else {
   wn_names <- as.character(wn_values)
 }

 # Create spectral data matrix
 spectra_matrix <- matrix(
   runif(n_samples * n_wavelengths, min = 0, max = 2),
   nrow = n_samples,
   ncol = n_wavelengths
 )
 colnames(spectra_matrix) <- wn_names

 # Create tibble
 result <- tibble::tibble(!!id_col := paste0("sample_", seq_len(n_samples)))
 result <- dplyr::bind_cols(result, tibble::as_tibble(spectra_matrix))

 # Add extra columns if requested
 if (!is.null(extra_cols)) {
   for (col in extra_cols) {
     result[[col]] <- paste0(col, "_value_", seq_len(n_samples))
   }
 }

 result
}


# ==============================================================================
# read_csv_spectra() — CSV File Reading
# ==============================================================================

test_that("read_csv_spectra() reads valid CSV file", {

  # Arrange
  tmp_file <- tempfile(fileext = ".csv")
  test_data <- make_test_spectra_tibble(n_samples = 5, wn_prefix = TRUE)
  readr::write_csv(test_data, tmp_file)

  # Act
  result <- read_csv_spectra(tmp_file)

  # Assert

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  expect_true("Sample_ID" %in% names(result))

  # Cleanup
  unlink(tmp_file)

})

test_that("read_csv_spectra() errors on non-existent file", {

  # Arrange
  fake_path <- "/nonexistent/path/to/file.csv"

  # Act & Assert
  expect_error(
    read_csv_spectra(fake_path),
    class = "horizons_read_error"
  )

})

test_that("read_csv_spectra() errors on non-.csv extension", {

  # Arrange
  tmp_file <- tempfile(fileext = ".txt")
  writeLines("a,b,c\n1,2,3", tmp_file)

  # Act & Assert
  expect_error(
    read_csv_spectra(tmp_file),
    class = "horizons_read_error"
  )

  # Cleanup
  unlink(tmp_file)

})

test_that("read_csv_spectra() errors on empty file", {

  # Arrange
  tmp_file <- tempfile(fileext = ".csv")
  writeLines("Sample_ID,wn_4000,wn_3998", tmp_file)  # Header only, no data

  # Act & Assert
  expect_error(
    read_csv_spectra(tmp_file),
    class = "horizons_read_error"
  )

  # Cleanup
  unlink(tmp_file)

})

test_that("read_csv_spectra() coerces numeric column names to numeric", {

  # Arrange
  tmp_file <- tempfile(fileext = ".csv")
  test_data <- tibble::tibble(
    Sample_ID = c("a", "b", "c"),
    `4000` = c("1.5", "2.0", "2.5"),
    `3998` = c("1.2", "1.8", "2.1")
  )
  readr::write_csv(test_data, tmp_file)

  # Act
  result <- read_csv_spectra(tmp_file)

  # Assert — columns should be numeric
  expect_type(result$`4000`, "double")
  expect_type(result$`3998`, "double")

  # Cleanup
  unlink(tmp_file)

})

test_that("read_csv_spectra() coerces wn_ prefixed columns to numeric", {

  # Arrange
  tmp_file <- tempfile(fileext = ".csv")
  test_data <- tibble::tibble(
    Sample_ID = c("a", "b", "c"),
    wn_4000 = c("1.5", "2.0", "2.5"),
    wn_3998 = c("1.2", "1.8", "2.1")
  )
  readr::write_csv(test_data, tmp_file)

  # Act
  result <- read_csv_spectra(tmp_file)

  # Assert — columns should be numeric
  expect_type(result$wn_4000, "double")
  expect_type(result$wn_3998, "double")

  # Cleanup
  unlink(tmp_file)

})

test_that("read_csv_spectra() warns when coercion introduces NAs", {

  # Arrange
  tmp_file <- tempfile(fileext = ".csv")
  test_data <- tibble::tibble(
    Sample_ID = c("a", "b", "c"),
    wn_4000 = c("1.5", "bad_value", "2.5"),
    wn_3998 = c("1.2", "1.8", "also_bad")
  )
  readr::write_csv(test_data, tmp_file)

  # Act & Assert — should message about NAs (cli_alert_warning uses message)
  expect_message(
    result <- read_csv_spectra(tmp_file),
    regexp = "coerced to NA"
  )

  # Verify NAs were created
  expect_true(is.na(result$wn_4000[2]))
  expect_true(is.na(result$wn_3998[3]))

  # Cleanup
  unlink(tmp_file)

})

test_that("read_csv_spectra() preserves non-spectral columns", {

  # Arrange
  tmp_file <- tempfile(fileext = ".csv")
  test_data <- tibble::tibble(
    Sample_ID = c("a", "b", "c"),
    pH = c(6.5, 7.0, 5.8),
    notes = c("good", "ok", "poor"),
    wn_4000 = c(1.5, 2.0, 2.5)
  )
  readr::write_csv(test_data, tmp_file)

  # Act
  result <- read_csv_spectra(tmp_file)

  # Assert — all columns preserved
  expect_true("Sample_ID" %in% names(result))
  expect_true("pH" %in% names(result))
  expect_true("notes" %in% names(result))
  expect_true("wn_4000" %in% names(result))

  # Assert — types preserved appropriately
  expect_type(result$Sample_ID, "character")
  expect_type(result$pH, "double")
  expect_type(result$notes, "character")

  # Cleanup
  unlink(tmp_file)

})

test_that("read_csv_spectra() handles decimal wavenumbers", {

  # Arrange
  tmp_file <- tempfile(fileext = ".csv")
  test_data <- tibble::tibble(
    Sample_ID = c("a", "b"),
    `4000.5` = c(1.5, 2.0),
    wn_3998.25 = c(1.2, 1.8)
  )
  readr::write_csv(test_data, tmp_file)

  # Act
  result <- read_csv_spectra(tmp_file)

  # Assert — decimal columns recognized and coerced
  expect_type(result$`4000.5`, "double")
  expect_type(result$wn_3998.25, "double")

  # Cleanup
  unlink(tmp_file)

})


# ==============================================================================
# Input Type Detection
# ==============================================================================

test_that("spectra() detects tibble input", {

 # Arrange
 test_data <- make_test_spectra_tibble(n_samples = 5, wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert
 expect_s3_class(result, "horizons_data")
 expect_equal(result$provenance$spectra_type, "tibble")

})

test_that("spectra() detects CSV file input", {

 # Arrange
 tmp_file <- tempfile(fileext = ".csv")
 test_data <- make_test_spectra_tibble(n_samples = 5, wn_prefix = TRUE)
 readr::write_csv(test_data, tmp_file)

 # Act
 result <- spectra(tmp_file)

 # Assert
 expect_s3_class(result, "horizons_data")
 expect_equal(result$provenance$spectra_type, "csv")

 # Cleanup
 unlink(tmp_file)

})

test_that("spectra() detects OPUS directory input", {

 skip("OPUS reading not yet implemented")

})


# ==============================================================================
# ID Column Detection
# ==============================================================================

test_that("spectra() detects Sample_ID column and standardizes to sample_id", {

 # Arrange — input uses Sample_ID (common format)
 test_data <- make_test_spectra_tibble(id_col = "Sample_ID", wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert — output standardized to sample_id
 expect_true("sample_id" %in% names(result$data$analysis))
 id_role <- result$data$role_map$role[result$data$role_map$variable == "sample_id"]
 expect_equal(id_role, "id")

})

test_that("spectra() detects sample_id column (already correct)", {

 # Arrange — input already uses sample_id
 test_data <- make_test_spectra_tibble(id_col = "sample_id", wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert — stays as sample_id
 expect_true("sample_id" %in% names(result$data$analysis))

})

test_that("spectra() detects id column and standardizes to sample_id", {

 # Arrange — input uses generic "id"
 test_data <- make_test_spectra_tibble(id_col = "id", wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert — output standardized to sample_id
 expect_true("sample_id" %in% names(result$data$analysis))

})

test_that("spectra() uses id_col override when provided", {

 # Arrange — custom column name not in alias list
 test_data <- make_test_spectra_tibble(id_col = "my_custom_id", wn_prefix = TRUE)

 # Act
 result <- spectra(test_data, id_col = "my_custom_id")

 # Assert — output standardized to sample_id
 expect_true("sample_id" %in% names(result$data$analysis))

})

test_that("spectra() errors when no ID column detected and none specified", {

 # Arrange — no recognizable ID column
 test_data <- make_test_spectra_tibble(id_col = "monkey_arms", wn_prefix = TRUE)

 # Act & Assert
 expect_error(
   spectra(test_data),
   regexp = "(?i)could not identify sample id|id_col",
 )

})

test_that("spectra() warns when multiple ID candidates found", {

 # Arrange — both sample_id and filename present
 test_data <- make_test_spectra_tibble(id_col = "sample_id", wn_prefix = TRUE)
 test_data$filename <- paste0("file_", seq_len(nrow(test_data)))

 # Act & Assert — should message about multiple candidates
 expect_message(
   result <- spectra(test_data),
   regexp = "(?i)also found|multiple"
 )

 # Should use higher priority (sample_id) and standardize
 expect_s3_class(result, "horizons_data")
 expect_true("sample_id" %in% names(result$data$analysis))

})


# ==============================================================================
# Wavelength Column Detection
# ==============================================================================
test_that("spectra() detects wn_ prefixed columns", {

 # Arrange
 test_data <- make_test_spectra_tibble(n_wavelengths = 100, wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert
 wn_cols <- names(result$data$analysis)[grepl("^wn_", names(result$data$analysis))]
 expect_equal(length(wn_cols), 100)

 # Check role_map
 predictor_count <- sum(result$data$role_map$role == "predictor")
 expect_equal(predictor_count, 100)

})

test_that("spectra() detects numeric column names and adds wn_ prefix", {

 # Arrange — numeric column names without prefix
 test_data <- make_test_spectra_tibble(n_wavelengths = 50, wn_prefix = FALSE)

 # Act
 result <- spectra(test_data)

 # Assert — should have wn_ prefix now
 wn_cols <- names(result$data$analysis)[grepl("^wn_", names(result$data$analysis))]
 expect_equal(length(wn_cols), 50)

 # Original numeric names should not exist
 numeric_cols <- names(result$data$analysis)[grepl("^[0-9]+$", names(result$data$analysis))]
 expect_equal(length(numeric_cols), 0)

})

test_that("spectra() uses wavelength_cols override when provided", {

 # Arrange — weird column names
 test_data <- tibble::tibble(
   Sample_ID = c("a", "b", "c"),
   spec_600 = c(1, 2, 3),
   spec_602 = c(4, 5, 6),
   spec_604 = c(7, 8, 9),
   notes = c("x", "y", "z")
 )

 # Act
 result <- spectra(test_data, wavelength_cols = c("spec_600", "spec_602", "spec_604"))

 # Assert
 wn_cols <- names(result$data$analysis)[grepl("^wn_", names(result$data$analysis))]
 expect_equal(length(wn_cols), 3)

})

test_that("spectra() errors when no wavelength columns detected", {

 # Arrange — no wavelength-like columns
 test_data <- tibble::tibble(
   Sample_ID = c("a", "b", "c"),
   notes = c("x", "y", "z"),
   value = c(1, 2, 3)
 )

 # Act & Assert
 expect_error(
   spectra(test_data),
   regexp = "(?i)could not identify wavelength|wavelength_cols"
 )

})


# ==============================================================================
# Extra Columns (Meta)
# ==============================================================================

test_that("spectra() preserves extra columns as meta", {

 # Arrange
 test_data <- make_test_spectra_tibble(
   n_samples = 5,
   wn_prefix = TRUE,
   extra_cols = c("notes", "date", "lab")
 )

 # Act
 result <- spectra(test_data)

 # Assert — columns preserved
 expect_true("notes" %in% names(result$data$analysis))
 expect_true("date" %in% names(result$data$analysis))
 expect_true("lab" %in% names(result$data$analysis))

 # Assert — role is meta
 notes_role <- result$data$role_map$role[result$data$role_map$variable == "notes"]
 expect_equal(notes_role, "meta")

})


# ==============================================================================
# Return Value Structure
# ==============================================================================

test_that("spectra() returns valid horizons_data object", {

 # Arrange
 test_data <- make_test_spectra_tibble(n_samples = 10, wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert — class
 expect_s3_class(result, "horizons_data")

 # Assert — required slots populated
 expect_false(is.null(result$data$analysis))
 expect_false(is.null(result$data$role_map))
 expect_false(is.null(result$provenance$spectra_type))
 expect_false(is.null(result$provenance$created))
 expect_false(is.null(result$provenance$horizons_version))

 # Assert — counts correct
 expect_equal(result$data$n_rows, 10)

})

test_that("spectra() populates provenance correctly for tibble input", {

 # Arrange
 test_data <- make_test_spectra_tibble(n_samples = 5, wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert
 expect_equal(result$provenance$spectra_type, "tibble")
 expect_equal(result$provenance$spectra_source, "tibble")
 expect_s3_class(result$provenance$created, "POSIXct")

})


# ==============================================================================
# Wavelength Ordering
# ==============================================================================

test_that("spectra() orders wavelengths in decreasing order", {

 # Arrange — wavelengths in random order
 test_data <- tibble::tibble(
   Sample_ID = c("a", "b"),
   wn_600 = c(1, 2),
   wn_800 = c(3, 4),
   wn_400 = c(5, 6),
   wn_1000 = c(7, 8)
 )

 # Act
 result <- spectra(test_data)

 # Assert — wavelengths should be in decreasing order (1000, 800, 600, 400)
 wn_cols <- names(result$data$analysis)[grepl("^wn_", names(result$data$analysis))]
 wn_values <- as.numeric(gsub("^wn_", "", wn_cols))
 expect_equal(wn_values, sort(wn_values, decreasing = TRUE))

})


# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("spectra() handles single sample", {

 # Arrange
 test_data <- make_test_spectra_tibble(n_samples = 1, wn_prefix = TRUE)

 # Act
 result <- spectra(test_data)

 # Assert
 expect_equal(result$data$n_rows, 1)

})

test_that("spectra() handles single wavelength", {

 # Arrange
 test_data <- tibble::tibble(
   Sample_ID = c("a", "b", "c"),
   wn_4000 = c(1, 2, 3)
 )

 # Act
 result <- spectra(test_data)

 # Assert
 expect_equal(result$data$n_predictors, 1)

})

test_that("spectra() errors on empty input", {

 # Arrange
 test_data <- tibble::tibble()

 # Act & Assert
 expect_error(spectra(test_data))

})

test_that("spectra() detects long format and provides helpful error", {

 # Arrange — long format data (wavelength as a column, not column names)
 test_data <- tibble::tibble(
   Sample_ID = rep(c("a", "b"), each = 100),
   wavelength = rep(seq(4000, 3802, by = -2), 2),
   absorbance = runif(200)
 )

 # Act & Assert
 expect_error(
   spectra(test_data),
   regexp = "(?i)long format|pivot_wider"
 )

})


# ==============================================================================
# Input Validation
# ==============================================================================

test_that("spectra() errors on invalid id_col type", {

  # Arrange
  test_data <- make_test_spectra_tibble(n_samples = 5, wn_prefix = TRUE)

  # Act & Assert
  expect_error(
    spectra(test_data, id_col = 123),
    class = "horizons_input_error"
  )

})

test_that("spectra() errors on invalid recursive type", {

  # Arrange
  test_data <- make_test_spectra_tibble(n_samples = 5, wn_prefix = TRUE)

  # Act & Assert
  expect_error(
    spectra(test_data, recursive = "yes"),
    class = "horizons_input_error"
  )

})

test_that("spectra() errors on duplicate sample IDs", {

  # Arrange — tibble with duplicate IDs
  test_data <- tibble::tibble(
    sample_id = c("a", "b", "a", "c"),
    wn_4000 = c(1, 2, 3, 4),
    wn_3998 = c(5, 6, 7, 8)
  )

  # Act & Assert
  expect_error(
    spectra(test_data),
    class = "horizons_input_error"
  )

})
