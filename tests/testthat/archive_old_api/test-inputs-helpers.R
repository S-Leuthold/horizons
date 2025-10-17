#' Tests for Helper Functions
#'
#' Comprehensive test suite for helper functions including parse_filename_metadata()
#' and internal reading functions

# Setup test data --------------------------------------------------------------

setup_test_filenames <- function() {
  list(
    simple = c(
      "PROJ_001_bulk.0",
      "PROJ_002_fraction.0",
      "PROJ_003_bulk.1"
    ),
    complex = c(
      "FFAR_S094-A_GroundBulk_S1_G11.0",
      "FFAR_S094-B_GroundBulk_S2_H12.1",
      "PROJ_001_Fraction_Scan1.2"
    ),
    irregular = c(
      "sample001.0",
      "test_sample_bulk_scan1_extra.0",
      "short.0"
    )
  )
}

# parse_filename_metadata() Tests ----------------------------------------------

test_that("parse_filename_metadata parses simple format correctly", {
  
  result <- parse_filename_metadata(
    file_name = "PROJ_001_bulk.0",
    format_string = "project_sampleid_fraction",
    delimiter = "_"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Project", "Sample_ID", "Fraction") %in% names(result)))
  expect_equal(result$Project, "PROJ")
  expect_equal(result$Sample_ID, "001")
  expect_equal(result$Fraction, "bulk")
  expect_equal(nrow(result), 1)
})

test_that("parse_filename_metadata handles complex format strings", {
  
  result <- parse_filename_metadata(
    file_name = "FFAR_S094-A_GroundBulk_S1_G11.0",
    format_string = "project_sampleid_fraction_wellid_scanid",
    delimiter = "_"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Project", "Sample_ID", "Fraction", "Well_ID", "Scan") %in% names(result)))
  expect_equal(result$Project, "FFAR")
  expect_equal(result$Sample_ID, "S094-A")
  expect_equal(result$Fraction, "GroundBulk")
  expect_equal(result$Well_ID, "S1")
  expect_equal(result$Scan, "G11")
})

test_that("parse_filename_metadata handles different delimiters", {
  
  # Test with hyphen delimiter
  result_hyphen <- parse_filename_metadata(
    file_name = "PROJ-001-bulk.0",
    format_string = "project-sampleid-fraction",
    delimiter = "-"
  )
  
  expect_equal(result_hyphen$Project, "PROJ")
  expect_equal(result_hyphen$Sample_ID, "001")
  expect_equal(result_hyphen$Fraction, "bulk")
  
  # Test with dot delimiter
  result_dot <- parse_filename_metadata(
    file_name = "PROJ.001.bulk.0",
    format_string = "project.sampleid.fraction",
    delimiter = "."
  )
  
  expect_equal(result_dot$Project, "PROJ")
  expect_equal(result_dot$Sample_ID, "001")
  expect_equal(result_dot$Fraction, "bulk")
})

test_that("parse_filename_metadata handles file extensions correctly", {
  
  # Test with various extensions
  extensions <- c(".0", ".1", ".2", ".txt", ".csv")
  
  for (ext in extensions) {
    filename <- paste0("PROJ_001_bulk", ext)
    
    result <- parse_filename_metadata(
      file_name = filename,
      format_string = "project_sampleid_fraction",
      delimiter = "_"
    )
    
    expect_equal(result$Project, "PROJ")
    expect_equal(result$Sample_ID, "001")
    expect_equal(result$Fraction, "bulk")
  }
})

test_that("parse_filename_metadata uses default fraction correctly", {
  
  # Test with missing fraction token
  result_default <- parse_filename_metadata(
    file_name = "PROJ_001.0",
    format_string = "project_sampleid",
    delimiter = "_",
    default_fraction = "GroundBulk"
  )
  
  expect_equal(result_default$Fraction, "GroundBulk")
  expect_message(
    parse_filename_metadata("PROJ_001.0", "project_sampleid", "_", "TestFraction"),
    "No fraction found.*Using default.*TestFraction"
  )
  
  # Test with custom default
  result_custom <- parse_filename_metadata(
    file_name = "PROJ_001.0",
    format_string = "project_sampleid", 
    delimiter = "_",
    default_fraction = "CustomFraction"
  )
  
  expect_equal(result_custom$Fraction, "CustomFraction")
})

test_that("parse_filename_metadata handles missing Sample_ID gracefully", {
  
  # Test with format that doesn't include sampleid
  expect_message(
    result <- parse_filename_metadata(
      file_name = "PROJ_fraction.0",
      format_string = "project_fraction",
      delimiter = "_"
    ),
    "No Sample_ID found.*Using default.*UNKNOWN"
  )
  
  expect_equal(result$Sample_ID, "UNKNOWN")
})

test_that("parse_filename_metadata handles insufficient filename parts", {
  
  expect_warning(
    result <- parse_filename_metadata(
      file_name = "PROJ.0",  # Only one part
      format_string = "project_sampleid_fraction",
      delimiter = "_"
    ),
    "has fewer parts than expected"
  )
  
  expect_equal(result$Sample_ID, "UNKNOWN")
  expect_equal(result$Fraction, "GroundBulk")  # default
})

test_that("parse_filename_metadata handles ignore token", {
  
  result <- parse_filename_metadata(
    file_name = "PROJ_001_ignore_bulk_scan1.0",
    format_string = "project_sampleid_ignore_fraction_scanid",
    delimiter = "_"
  )
  
  # Should have all tokens except ignore
  expected_cols <- c("Project", "Sample_ID", "ignore", "Fraction", "Scan")
  expect_true(all(expected_cols %in% names(result)))
  expect_equal(result$Project, "PROJ")
  expect_equal(result$Sample_ID, "001")
  expect_equal(result$ignore, "ignore")  # Keep the actual value
  expect_equal(result$Fraction, "bulk")
  expect_equal(result$Scan, "scan1")
})

test_that("parse_filename_metadata handles edge cases", {
  
  # Empty filename
  expect_warning(
    result_empty <- parse_filename_metadata("", "project_sampleid", "_"),
    "has fewer parts than expected"
  )
  
  # Filename with no delimiter
  result_no_delim <- parse_filename_metadata(
    "singlepart.0",
    "sampleid",
    "_"
  )
  expect_equal(result_no_delim$Sample_ID, "singlepart")
  
  # Very long filename
  long_filename <- paste(rep("part", 10), collapse = "_")
  result_long <- parse_filename_metadata(
    paste0(long_filename, ".0"),
    "project_sampleid",
    "_"
  )
  expect_equal(result_long$Project, "part")
  expect_equal(result_long$Sample_ID, "part")
})

# Token Mapping Tests ----------------------------------------------------------

test_that("parse_filename_metadata maps tokens to correct column names", {
  
  result <- parse_filename_metadata(
    file_name = "PROJ_001_bulk_W1_S1.0",
    format_string = "project_sampleid_fraction_wellid_scanid",
    delimiter = "_"
  )
  
  # Check that tokens are mapped to proper column names
  expect_true("Project" %in% names(result))    # project -> Project
  expect_true("Sample_ID" %in% names(result))  # sampleid -> Sample_ID
  expect_true("Fraction" %in% names(result))   # fraction -> Fraction
  expect_true("Well_ID" %in% names(result))    # wellid -> Well_ID
  expect_true("Scan" %in% names(result))       # scanid -> Scan
  
  # Check values
  expect_equal(result$Project, "PROJ")
  expect_equal(result$Sample_ID, "001")
  expect_equal(result$Fraction, "bulk")
  expect_equal(result$Well_ID, "W1")
  expect_equal(result$Scan, "S1")
})

test_that("parse_filename_metadata handles unknown tokens", {
  
  result <- parse_filename_metadata(
    file_name = "PROJ_001_bulk_custom.0",
    format_string = "project_sampleid_fraction_customtoken",
    delimiter = "_"
  )
  
  # Unknown tokens should be kept as-is
  expect_true("customtoken" %in% names(result))
  expect_equal(result$customtoken, "custom")
})

# Integration with Other Functions Tests ---------------------------------------

test_that("parse_filename_metadata integrates with create_dataset", {
  
  # Test that output format is compatible with create_dataset's expectations
  filenames <- c(
    "PROJ_001_bulk_S1.0",
    "PROJ_002_bulk_S1.0",
    "PROJ_003_fraction_S1.0"
  )
  
  results <- lapply(filenames, function(fn) {
    parse_filename_metadata(
      fn,
      "project_sampleid_fraction_scanid",
      "_"
    )
  })
  
  combined <- do.call(rbind, results)
  
  expect_s3_class(combined, "data.frame")
  expect_equal(nrow(combined), 3)
  expect_true("Sample_ID" %in% names(combined))
  expect_true("Fraction" %in% names(combined))
  expect_true(all(!is.na(combined$Sample_ID)))
})

# Performance Tests -------------------------------------------------------------

test_that("parse_filename_metadata performs efficiently", {
  
  # Test with many filenames
  n_files <- 1000
  filenames <- paste0("PROJ_", sprintf("%04d", 1:n_files), "_bulk_S1.0")
  
  start_time <- Sys.time()
  
  results <- lapply(filenames, function(fn) {
    parse_filename_metadata(fn, "project_sampleid_fraction_scanid", "_")
  })
  
  end_time <- Sys.time()
  
  # Should complete reasonably quickly
  expect_lt(as.numeric(end_time - start_time), 5)
  
  # Check results
  expect_equal(length(results), n_files)
  expect_true(all(sapply(results, function(x) "Sample_ID" %in% names(x))))
})

# Real-world Examples Tests ----------------------------------------------------

test_that("parse_filename_metadata handles real spectroscopy naming conventions", {
  
  # Common soil spectroscopy naming patterns
  test_cases <- list(
    # ICRAF naming
    list(
      filename = "KE001_12345_Soil_Bulk_Rep1.0",
      format = "project_sampleid_ignore_fraction_scanid",
      expected = list(Project = "KE001", Sample_ID = "12345", Fraction = "Bulk", Scan = "Rep1")
    ),
    
    # USDA naming
    list(
      filename = "USDA_SSL_12345_GroundBulk_A1.0", 
      format = "project_ignore_sampleid_fraction_wellid",
      expected = list(Project = "USDA", Sample_ID = "12345", Fraction = "GroundBulk", Well_ID = "A1")
    ),
    
    # Research project naming
    list(
      filename = "FFAR_S094-A_GroundBulk_S1_G11.0",
      format = "project_sampleid_fraction_ignore_scanid",
      expected = list(Project = "FFAR", Sample_ID = "S094-A", Fraction = "GroundBulk", Scan = "G11")
    )
  )
  
  for (case in test_cases) {
    result <- parse_filename_metadata(
      case$filename,
      case$format,
      "_"
    )
    
    for (col_name in names(case$expected)) {
      expect_equal(result[[col_name]], case$expected[[col_name]], 
                  info = paste("Failed for", case$filename, "column", col_name))
    }
  }
})

# Error Handling Tests ----------------------------------------------------------

test_that("parse_filename_metadata handles various error conditions gracefully", {
  
  # Test with NULL inputs
  expect_error(
    parse_filename_metadata(NULL, "project_sampleid", "_"),
    # Should handle gracefully or give clear error
  )
  
  # Test with empty format string
  result_empty_format <- parse_filename_metadata(
    "PROJ_001.0",
    "",
    "_"
  )
  expect_s3_class(result_empty_format, "data.frame")
  
  # Test with unusual delimiters
  result_space <- parse_filename_metadata(
    "PROJ 001 bulk.0",
    "project sampleid fraction",
    " "
  )
  expect_equal(result_space$Project, "PROJ")
  
  # Test with numeric parts
  result_numeric <- parse_filename_metadata(
    "123_456_789.0",
    "project_sampleid_fraction",
    "_"
  )
  expect_equal(result_numeric$Project, "123")
  expect_equal(result_numeric$Sample_ID, "456")
})

# Consistency Tests -------------------------------------------------------------

test_that("parse_filename_metadata produces consistent output format", {
  
  # Test various inputs to ensure consistent output structure
  test_filenames <- c(
    "A_B_C.0",
    "X_Y.0", 
    "P_Q_R_S.0"
  )
  
  results <- lapply(test_filenames, function(fn) {
    parse_filename_metadata(fn, "project_sampleid_fraction", "_")
  })
  
  # All results should be data.frames
  expect_true(all(sapply(results, is.data.frame)))
  
  # All should have same essential columns (some may be default values)
  essential_cols <- c("Sample_ID", "Fraction")
  for (result in results) {
    expect_true(all(essential_cols %in% names(result)))
  }
  
  # All should have exactly 1 row
  expect_true(all(sapply(results, nrow) == 1))
})

# Documentation Examples Tests -------------------------------------------------

test_that("parse_filename_metadata works with documentation examples", {
  
  # Test example from function documentation
  result <- parse_filename_metadata(
    file_name = "FFAR_001_Bulk.0",
    format_string = "project_sampleid_fraction",
    delimiter = "_"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(result$Project, "FFAR")
  expect_equal(result$Sample_ID, "001")
  expect_equal(result$Fraction, "Bulk")
  
  # Test more complex example
  result_complex <- parse_filename_metadata(
    file_name = "LAB_S001_GroundBulk_Replicate1.0",
    format_string = "project_sampleid_fraction_scanid",
    delimiter = "_",
    default_fraction = "Bulk"
  )
  
  expect_equal(result_complex$Project, "LAB")
  expect_equal(result_complex$Sample_ID, "S001")
  expect_equal(result_complex$Fraction, "GroundBulk")
  expect_equal(result_complex$Scan, "Replicate1")
})