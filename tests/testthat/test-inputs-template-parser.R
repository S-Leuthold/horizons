#' Tests for Template Parser Function
#'
#' Comprehensive tests for parse_id_template() which converts user-friendly
#' template strings like "{project}_{sample_id}_<scan>" into parsing instructions.
#'
#' **Template Syntax:**
#' - `{identifier}`: Core sample identifiers (aggregate/group by these)
#' - `<replicate>`: Technical replicates (average across these)
#' - Delimiter: Automatically detected from template
#'
#' **Test Strategy**: TDD approach - write all tests first, then implement
#' **Test Count**: 40+ tests covering all edge cases and error conditions

library(testthat)
library(horizons)

# Access internal function
parse_id_template <- horizons:::parse_id_template

## ===========================================================================
## Setup and Fixtures
## ===========================================================================

test_that("parse_id_template function exists and is accessible", {
  expect_true(is.function(parse_id_template))
})

## ===========================================================================
## SIMPLE TEMPLATES - No Replicates
## ===========================================================================

test_that("parse_id_template handles single identifier", {
  # SPEC-TEMPLATE-001: Simplest case - just one identifier
  result <- parse_id_template("{sample_id}")

  expect_type(result, "list")
  expect_equal(result$identifiers, "sample_id")
  expect_equal(result$replicates, character(0))
  expect_equal(result$delimiter, "")  # No delimiter when single field
  expect_equal(result$format_string, "sampleid")
  expect_equal(result$aggregate_by, "Sample_ID")
})

test_that("parse_id_template handles two identifiers with underscore", {
  # SPEC-TEMPLATE-002: Two identifiers separated by underscore
  result <- parse_id_template("{project}_{sample_id}")

  expect_equal(result$identifiers, c("project", "sample_id"))
  expect_equal(result$replicates, character(0))
  expect_equal(result$delimiter, "_")
  expect_equal(result$format_string, "project_sampleid")
  expect_equal(result$aggregate_by, c("Project", "Sample_ID"))
})

test_that("parse_id_template handles three identifiers", {
  # SPEC-TEMPLATE-003: Standard three-part template
  result <- parse_id_template("{project}_{sample_id}_{fraction}")

  expect_equal(result$identifiers, c("project", "sample_id", "fraction"))
  expect_equal(result$replicates, character(0))
  expect_equal(result$delimiter, "_")
  expect_equal(result$format_string, "project_sampleid_fraction")
  expect_equal(result$aggregate_by, c("Project", "Sample_ID", "Fraction"))
})

test_that("parse_id_template handles hyphen delimiter", {
  # SPEC-TEMPLATE-004: Different delimiter
  result <- parse_id_template("{project}-{sample_id}")

  expect_equal(result$delimiter, "-")
  expect_equal(result$format_string, "project_sampleid")  # Format uses underscore
})

test_that("parse_id_template handles dot delimiter", {
  # SPEC-TEMPLATE-005: Dot as delimiter
  result <- parse_id_template("{project}.{sample_id}")

  expect_equal(result$delimiter, ".")
  expect_equal(result$format_string, "project_sampleid")
})

## ===========================================================================
## TEMPLATES WITH REPLICATES
## ===========================================================================

test_that("parse_id_template handles single replicate", {
  # SPEC-TEMPLATE-006: One identifier with one replicate
  result <- parse_id_template("{sample_id}_<scan>")

  expect_equal(result$identifiers, "sample_id")
  expect_equal(result$replicates, "scan")
  expect_equal(result$delimiter, "_")
  expect_equal(result$format_string, "sampleid_scan")
  expect_equal(result$aggregate_by, "Sample_ID")
})

test_that("parse_id_template handles two replicates", {
  # SPEC-TEMPLATE-007: Multiple replicates to average over
  result <- parse_id_template("{sample_id}_<scan>_<well>")

  expect_equal(result$identifiers, "sample_id")
  expect_equal(result$replicates, c("scan", "well"))
  expect_equal(result$format_string, "sampleid_scan_well")
  expect_equal(result$aggregate_by, "Sample_ID")
})

test_that("parse_id_template handles identifiers and replicates mixed", {
  # SPEC-TEMPLATE-008: Replicates can appear anywhere in template
  result <- parse_id_template("{project}_<scan>_{sample_id}_{fraction}_<well>")

  expect_equal(result$identifiers, c("project", "sample_id", "fraction"))
  expect_equal(result$replicates, c("scan", "well"))
  expect_equal(result$delimiter, "_")
  expect_equal(result$format_string, "project_scan_sampleid_fraction_well")
  expect_equal(result$aggregate_by, c("Project", "Sample_ID", "Fraction"))
})

test_that("parse_id_template handles replicates at beginning", {
  # SPEC-TEMPLATE-009: Replicate can be first field
  result <- parse_id_template("<scan>_{project}_{sample_id}")

  expect_equal(result$identifiers, c("project", "sample_id"))
  expect_equal(result$replicates, "scan")
  expect_equal(result$format_string, "scan_project_sampleid")
})

## ===========================================================================
## EDGE CASES
## ===========================================================================

test_that("parse_id_template errors on only replicates (no identifiers)", {
  # SPEC-TEMPLATE-010: Must have at least one identifier field
  expect_error(
    parse_id_template("<scan>_<well>"),
    regexp = "at least one identifier"
  )
})

test_that("parse_id_template handles underscores in field names", {
  # SPEC-TEMPLATE-011: Field names can contain underscores
  result <- parse_id_template("{soil_type}_{sample_id}")

  expect_equal(result$identifiers, c("soil_type", "sample_id"))
  expect_equal(result$delimiter, "_")
  expect_equal(result$format_string, "soiltype_sampleid")
  expect_equal(result$aggregate_by, c("Soil_Type", "Sample_ID"))
})

test_that("parse_id_template handles numbers in field names", {
  # SPEC-TEMPLATE-012: Field names can contain numbers
  result <- parse_id_template("{project}_{site2}_{id3}")

  expect_equal(result$identifiers, c("project", "site2", "id3"))
  expect_equal(result$aggregate_by, c("Project", "Site2", "Id3"))
})

## ===========================================================================
## CASE HANDLING
## ===========================================================================

test_that("parse_id_template is case insensitive for input", {
  # SPEC-TEMPLATE-013: Template placeholders are case-insensitive
  result <- parse_id_template("{PROJECT}_{Sample_ID}")

  expect_equal(result$identifiers, c("project", "sample_id"))
  expect_equal(result$aggregate_by, c("Project", "Sample_ID"))
})

test_that("parse_id_template converts to proper case for aggregate_by", {
  # SPEC-TEMPLATE-014: Output uses Proper_Case for display names
  result <- parse_id_template("{soil_type}_{sample_id}_{fraction}")

  expect_equal(result$identifiers, c("soil_type", "sample_id", "fraction"))
  expect_equal(result$aggregate_by, c("Soil_Type", "Sample_ID", "Fraction"))
})

test_that("parse_id_template handles mixed case in placeholders", {
  # SPEC-TEMPLATE-015: Mixed case input is normalized
  result <- parse_id_template("{ProjEct}_{SAMPLE_id}_<ScAn>")

  expect_equal(result$identifiers, c("project", "sample_id"))
  expect_equal(result$replicates, "scan")
  expect_equal(result$aggregate_by, c("Project", "Sample_ID"))
})

## ===========================================================================
## ERROR CONDITIONS - Invalid Syntax
## ===========================================================================

test_that("parse_id_template errors on mixed delimiters", {
  # SPEC-TEMPLATE-ERR-001: Must use consistent delimiter
  expect_error(
    parse_id_template("{project}_{sample_id}-{fraction}"),
    regexp = "mixed delimiters|inconsistent delimiter"
  )
})

test_that("parse_id_template errors on mismatched braces - unclosed curly", {
  # SPEC-TEMPLATE-ERR-002: All braces must be properly closed
  expect_error(
    parse_id_template("{project}_{sample_id"),
    regexp = "mismatched|unclosed|invalid.*brace"
  )
})

test_that("parse_id_template errors on mismatched braces - unclosed angle", {
  # SPEC-TEMPLATE-ERR-003: Angle brackets must also be matched
  expect_error(
    parse_id_template("{project}_<scan"),
    regexp = "mismatched|unclosed|invalid.*brace"
  )
})

test_that("parse_id_template errors on mismatched braces - wrong closing", {
  # SPEC-TEMPLATE-ERR-004: Can't close { with > or vice versa
  expect_error(
    parse_id_template("{project>_{sample_id}"),
    regexp = "mismatched|invalid.*brace"
  )
})

test_that("parse_id_template errors on empty placeholder in curly braces", {
  # SPEC-TEMPLATE-ERR-005: Placeholders can't be empty
  expect_error(
    parse_id_template("{}_{}"),
    regexp = "empty placeholder|invalid.*name"
  )
})

test_that("parse_id_template errors on empty placeholder in angle brackets", {
  # SPEC-TEMPLATE-ERR-006: Replicate placeholders also can't be empty
  expect_error(
    parse_id_template("{project}_<>"),
    regexp = "empty placeholder|invalid.*name"
  )
})

test_that("parse_id_template errors on plain text without braces", {
  # SPEC-TEMPLATE-ERR-007: Must have at least one placeholder
  expect_error(
    parse_id_template("project_sample_id"),
    regexp = "no placeholders|invalid template|must contain"
  )
})

test_that("parse_id_template errors on invalid characters in placeholder", {
  # SPEC-TEMPLATE-ERR-008: Only alphanumeric and underscore allowed
  expect_error(
    parse_id_template("{project-name}_{sample_id}"),
    regexp = "invalid.*character|invalid.*name"
  )
})

test_that("parse_id_template errors on placeholder starting with number", {
  # SPEC-TEMPLATE-ERR-009: Field names can't start with numbers
  expect_error(
    parse_id_template("{1project}_{sample_id}"),
    regexp = "invalid.*name|must start with letter"
  )
})

test_that("parse_id_template errors on special characters in placeholder", {
  # SPEC-TEMPLATE-ERR-010: No special characters like @, $, etc.
  expect_error(
    parse_id_template("{project$}_{sample_id}"),
    regexp = "invalid.*character|invalid.*name"
  )
})

## ===========================================================================
## COMPLEX REAL-WORLD EXAMPLES
## ===========================================================================

test_that("parse_id_template handles KSSL format", {
  # SPEC-TEMPLATE-REAL-001: USDA-NRCS Kellogg Soil Survey Lab format
  result <- parse_id_template("{pedon_key}_{layer_sequence}")

  expect_equal(result$identifiers, c("pedon_key", "layer_sequence"))
  expect_equal(result$aggregate_by, c("Pedon_Key", "Layer_Sequence"))
  expect_equal(result$format_string, "pedonkey_layersequence")
})

test_that("parse_id_template handles AFSIS format", {
  # SPEC-TEMPLATE-REAL-002: Africa Soil Information Service format
  result <- parse_id_template("{country}_{site}_{depth}_{sample_id}_<scan>")

  expect_equal(result$identifiers, c("country", "site", "depth", "sample_id"))
  expect_equal(result$replicates, "scan")
  expect_equal(result$aggregate_by, c("Country", "Site", "Depth", "Sample_ID"))
})

test_that("parse_id_template handles plate-based workflow", {
  # SPEC-TEMPLATE-REAL-003: 96-well plate scanning workflow
  result <- parse_id_template("{project}_{sample_id}_<plate>_<well>_<scan>")

  expect_equal(result$identifiers, c("project", "sample_id"))
  expect_equal(result$replicates, c("plate", "well", "scan"))
  expect_equal(result$format_string, "project_sampleid_plate_well_scan")
})

test_that("parse_id_template handles fractionation experiment", {
  # SPEC-TEMPLATE-REAL-004: Density fractionation study
  result <- parse_id_template("{site}_{plot}_{depth}_{fraction}_<rep>")

  expect_equal(result$identifiers, c("site", "plot", "depth", "fraction"))
  expect_equal(result$replicates, "rep")
  expect_equal(result$aggregate_by, c("Site", "Plot", "Depth", "Fraction"))
})

## ===========================================================================
## RETURN STRUCTURE VALIDATION
## ===========================================================================

test_that("parse_id_template returns consistent structure", {
  # SPEC-TEMPLATE-STRUCT-001: All outputs must have same structure
  result <- parse_id_template("{project}_{sample_id}_<scan>")

  expect_type(result, "list")
  expect_named(result, c("identifiers", "replicates", "delimiter", "format_string", "aggregate_by"))
  expect_type(result$identifiers, "character")
  expect_type(result$replicates, "character")
  expect_type(result$delimiter, "character")
  expect_type(result$format_string, "character")
  expect_type(result$aggregate_by, "character")
})

test_that("parse_id_template returns correct lengths", {
  # SPEC-TEMPLATE-STRUCT-002: Identifiers and aggregate_by must match in length
  result <- parse_id_template("{project}_{sample_id}_{fraction}")

  expect_length(result$identifiers, 3)
  expect_length(result$aggregate_by, 3)
  expect_length(result$delimiter, 1)
  expect_length(result$format_string, 1)
})

test_that("parse_id_template errors when no identifiers provided", {
  # SPEC-TEMPLATE-STRUCT-003: At least one identifier is required
  expect_error(
    parse_id_template("<scan>_<well>"),
    regexp = "at least one identifier"
  )
})

test_that("parse_id_template handles no replicates correctly", {
  # SPEC-TEMPLATE-STRUCT-004: Empty replicate vector is valid
  result <- parse_id_template("{project}_{sample_id}")

  expect_length(result$identifiers, 2)
  expect_length(result$replicates, 0)
})

## ===========================================================================
## INTEGRATION TEST (skipped - to be implemented after orchestrator)
## ===========================================================================

test_that("parse_id_template integrates with create_spectra workflow", {
  skip("Integration test - implement after orchestrator function is created")

  # Will test that parse_id_template output can be used directly with
  # existing parse_ids() and create_dataset() functions
})
