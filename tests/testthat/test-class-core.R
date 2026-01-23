## ---------------------------------------------------------------------------
## Tests for class-core.R: S3 class constructors
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## new_horizons_data() — Internal Constructor
## ---------------------------------------------------------------------------

test_that("new_horizons_data creates object with correct class", {

  obj <- new_horizons_data()

  expect_s3_class(obj, "horizons_data")
  expect_s3_class(obj, "list")
  expect_equal(class(obj), c("horizons_data", "list"))

})

test_that("new_horizons_data creates object with all 8 sections", {

  obj <- new_horizons_data()

  expected_sections <- c(
    "data",
    "provenance",
    "config",
    "validation",
    "evaluation",
    "models",
    "ensemble",
    "artifacts"
  )

  expect_true(all(expected_sections %in% names(obj)))
  expect_equal(length(obj), 8)

})

test_that("new_horizons_data initializes data section correctly", {

  obj <- new_horizons_data()

  expect_true(is.list(obj$data))
  expect_true("analysis" %in% names(obj$data))
  expect_true("role_map" %in% names(obj$data))
  expect_true("n_rows" %in% names(obj$data))
  expect_true("n_predictors" %in% names(obj$data))
  expect_true("n_covariates" %in% names(obj$data))

})

test_that("new_horizons_data initializes provenance section correctly", {

  obj <- new_horizons_data()

  expect_true(is.list(obj$provenance))
  expect_true("spectra_source" %in% names(obj$provenance))
  expect_true("spectra_type" %in% names(obj$provenance))
  expect_true("response_source" %in% names(obj$provenance))
  expect_true("created" %in% names(obj$provenance))
  expect_true("horizons_version" %in% names(obj$provenance))
  expect_true("schema_version" %in% names(obj$provenance))

})

test_that("new_horizons_data sets provenance defaults", {

  obj <- new_horizons_data()

  expect_s3_class(obj$provenance$created, "POSIXct")
  expect_equal(obj$provenance$schema_version, 1L)
  expect_true(inherits(obj$provenance$horizons_version, "package_version"))

})

test_that("new_horizons_data accepts data argument", {

  # Create minimal test data
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, 0.2, 0.3),
    `3998` = c(0.2, 0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998"),
    role = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  expect_equal(obj$data$analysis, test_analysis)
  expect_equal(obj$data$role_map, test_role_map)
  expect_equal(obj$data$n_rows, 3L)
  expect_equal(obj$data$n_predictors, 2L)
  expect_equal(obj$data$n_covariates, 0L)

})

test_that("new_horizons_data counts covariates correctly", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2),
    pH = c(6.5, 7.0),
    clay = c(20, 30)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "pH", "clay"),
    role = c("id", "predictor", "covariate", "covariate")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  expect_equal(obj$data$n_predictors, 1L)
  expect_equal(obj$data$n_covariates, 2L)

})

test_that("new_horizons_data accepts provenance arguments", {

  obj <- new_horizons_data(
    spectra_source = "/path/to/opus",
    spectra_type = "opus"
  )

  expect_equal(obj$provenance$spectra_source, "/path/to/opus")
  expect_equal(obj$provenance$spectra_type, "opus")

})


## ---------------------------------------------------------------------------
## validate_horizons_data() — Structural Validator
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Valid objects should pass
## ---------------------------------------------------------------------------

test_that("validate_horizons_data passes for empty object", {


  ## Arrange
  obj <- new_horizons_data()

  ## Act
  result <- validate_horizons_data(obj)

  ## Assert
  expect_identical(result, obj)

})

test_that("validate_horizons_data passes for valid object with data", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, 0.2, 0.3),
    `3998` = c(0.2, 0.3, 0.4),
    `3996` = c(0.3, 0.4, 0.5)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998", "3996"),
    role = c("id", "predictor", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  result <- validate_horizons_data(obj)

  ## Assert
  expect_identical(result, obj)

})

test_that("validate_horizons_data passes with outcome column", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, 0.2, 0.3),
    Response = c(1.0, 2.0, 3.0)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "Response"),
    role = c("id", "predictor", "outcome")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  result <- validate_horizons_data(obj)

  ## Assert
  expect_identical(result, obj)

})

## ---------------------------------------------------------------------------
## analysis and role_map must be paired
## ---------------------------------------------------------------------------

test_that("validate_horizons_data errors when analysis exists but role_map missing", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2)
  )

  obj <- new_horizons_data(analysis = test_analysis)

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "role_map"
  )

})

test_that("validate_horizons_data errors when role_map exists but analysis missing", {

  ## Arrange
  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(role_map = test_role_map)

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "analysis"
  )

})

## ---------------------------------------------------------------------------
## sample_id checks
## ---------------------------------------------------------------------------

test_that("validate_horizons_data errors when sample_id column missing", {

  ## Arrange
  test_analysis <- tibble::tibble(
    id = c("A", "B", "C"),
    `4000` = c(0.1, 0.2, 0.3)
  )

  test_role_map <- tibble::tibble(
    variable = c("id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "sample_id"
  )

})

test_that("validate_horizons_data errors when sample_id has duplicates", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "A", "B"),
    `4000` = c(0.1, 0.2, 0.3)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "(?i)duplicate"
  )

})

test_that("validate_horizons_data error message includes duplicate sample_ids", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "A", "B", "C", "C"),
    `4000` = c(0.1, 0.2, 0.3, 0.4, 0.5)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert — error should mention which IDs are duplicated
  expect_error(
    validate_horizons_data(obj),
    "A.*C|C.*A"
  )

})

## ---------------------------------------------------------------------------
## Wavelength column checks
## ---------------------------------------------------------------------------

test_that("validate_horizons_data errors when wavelength columns contain NA", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, NA, 0.3),
    `3998` = c(0.2, 0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998"),
    role = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "NA|missing"
  )

})

test_that("validate_horizons_data errors when wavelength columns contain Inf", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, Inf, 0.3),
    `3998` = c(0.2, 0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998"),
    role = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "Inf|infinite"
  )

})

test_that("validate_horizons_data errors when wavelength columns not decreasing", {

  ## Arrange — wavelengths in increasing order (wrong)
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `3996` = c(0.1, 0.2, 0.3),
    `3998` = c(0.2, 0.3, 0.4),
    `4000` = c(0.3, 0.4, 0.5)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "3996", "3998", "4000"),
    role = c("id", "predictor", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "decreasing|order"
  )

})

## ---------------------------------------------------------------------------
## role_map checks
## ---------------------------------------------------------------------------

test_that("validate_horizons_data errors when role_map missing columns from analysis", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2),
    `3998` = c(0.2, 0.3)
  )

  ## role_map is missing "3998"
  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "3998|missing.*role"
  )

})

test_that("validate_horizons_data errors when no id role in role_map", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2)
  )

  ## No "id" role
  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("meta", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "id.*role|role.*id"
  )

})

test_that("validate_horizons_data errors when multiple id roles", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    other_id = c("X", "Y"),
    `4000` = c(0.1, 0.2)
  )

  ## Two "id" roles
  test_role_map <- tibble::tibble(
    variable = c("sample_id", "other_id", "4000"),
    role = c("id", "id", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act & Assert
  expect_error(
    validate_horizons_data(obj),
    "(?i)multiple.*id|one.*id|exactly.*id"
  )

})


## ---------------------------------------------------------------------------
## print.horizons_data() — Print Method
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Empty object display
## ---------------------------------------------------------------------------

test_that("print.horizons_data shows empty state for empty object", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  output <- capture.output(print(obj))

  ## Assert
  expect_true(any(grepl("horizons_data", output)))
  expect_true(any(grepl("(?i)empty", output)))

})

test_that("print.horizons_data shows hint for empty object", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  output <- capture.output(print(obj))

  ## Assert
  expect_true(any(grepl("spectra", output)))

})

## ---------------------------------------------------------------------------
## Object with data
## ---------------------------------------------------------------------------

test_that("print.horizons_data shows sample count", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, 0.2, 0.3),
    `3998` = c(0.2, 0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998"),
    role = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(print(obj))

  ## Assert
  expect_true(any(grepl("(?i)sample", output)))
  expect_true(any(grepl("3", output)))

})

test_that("print.horizons_data shows predictor count", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, 0.2, 0.3),
    `3998` = c(0.2, 0.3, 0.4),
    `3996` = c(0.3, 0.4, 0.5)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998", "3996"),
    role = c("id", "predictor", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(print(obj))

  ## Assert
  expect_true(any(grepl("(?i)predictor", output)))
  expect_true(any(grepl("3", output)))

})

test_that("print.horizons_data shows covariate count when present", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, 0.2, 0.3),
    clay = c(20, 30, 40),
    pH = c(6.5, 7.0, 7.5)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "clay", "pH"),
    role = c("id", "predictor", "covariate", "covariate")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(print(obj))

  ## Assert
  expect_true(any(grepl("(?i)covariate", output)))
  expect_true(any(grepl("2", output)))

})

## ---------------------------------------------------------------------------
## Provenance display
## ---------------------------------------------------------------------------

test_that("print.horizons_data shows provenance when present", {

  ## Arrange
  obj <- new_horizons_data(
    spectra_source = "/path/to/spectra",
    spectra_type = "opus"
  )

  ## Act
  output <- capture.output(print(obj))

  ## Assert
  expect_true(any(grepl("(?i)provenance|source", output)))
  expect_true(any(grepl("opus", output)))

})

## ---------------------------------------------------------------------------
## Return behavior
## ---------------------------------------------------------------------------

test_that("print.horizons_data returns object invisibly", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  result <- withVisible(print(obj))

  ## Assert
  expect_false(result$visible)
  expect_identical(result$value, obj)

})


## ---------------------------------------------------------------------------
## summary.horizons_data() — Summary Method
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Header and structure
## ---------------------------------------------------------------------------

test_that("summary.horizons_data shows header", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("horizons_data", output)))
  expect_true(any(grepl("summary", output)))

})

test_that("summary.horizons_data returns object invisibly", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  result <- withVisible(summary(obj))

  ## Assert
  expect_false(result$visible)
  expect_identical(result$value, obj)

})

## ---------------------------------------------------------------------------
## Data section details
## ---------------------------------------------------------------------------

test_that("summary.horizons_data shows sample IDs preview", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("SAMPLE_001", "SAMPLE_002", "SAMPLE_003"),
    `4000` = c(0.1, 0.2, 0.3)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("SAMPLE_001", output)))

})

test_that("summary.horizons_data shows wavenumber range and step", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2),
    `3998` = c(0.2, 0.3),
    `3996` = c(0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998", "3996"),
    role = c("id", "predictor", "predictor", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("(?i)range", output)))
  expect_true(any(grepl("4000", output)))
  expect_true(any(grepl("3996", output)))
  expect_true(any(grepl("(?i)step", output)))

})

test_that("summary.horizons_data shows covariate names", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2),
    clay = c(20, 30),
    pH = c(6.5, 7.0)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "clay", "pH"),
    role = c("id", "predictor", "covariate", "covariate")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("clay", output)))
  expect_true(any(grepl("pH", output)))

})

test_that("summary.horizons_data shows outcome when present", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2),
    SOC = c(1.5, 2.5)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "SOC"),
    role = c("id", "predictor", "outcome")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("(?i)outcome", output)))
  expect_true(any(grepl("SOC", output)))

})

test_that("summary.horizons_data shows memory footprint", {

  ## Arrange
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000` = c(0.1, 0.2)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(
    analysis = test_analysis,
    role_map = test_role_map
  )

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("(?i)memory|size|bytes|KB|MB", output)))

})

## ---------------------------------------------------------------------------
## Provenance section details
## ---------------------------------------------------------------------------

test_that("summary.horizons_data shows full provenance", {

  ## Arrange
  obj <- new_horizons_data(
    spectra_source = "/path/to/spectra",
    spectra_type = "opus",
    response_source = "/path/to/response.csv"
  )

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("(?i)provenance", output)))
  expect_true(any(grepl("/path/to/spectra", output)))
  expect_true(any(grepl("opus", output)))
  expect_true(any(grepl("response", output)))

})

test_that("summary.horizons_data shows version info", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("(?i)version", output)))
  expect_true(any(grepl("(?i)created", output)))

})

## ---------------------------------------------------------------------------
## Configuration section
## ---------------------------------------------------------------------------

test_that("summary.horizons_data shows tuning defaults", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("(?i)config", output)))
  expect_true(any(grepl("(?i)grid|tuning", output)))

})

## ---------------------------------------------------------------------------
## Pipeline status
## ---------------------------------------------------------------------------

test_that("summary.horizons_data shows pipeline status", {

  ## Arrange
  obj <- new_horizons_data()

  ## Act
  output <- capture.output(summary(obj))

  ## Assert
  expect_true(any(grepl("(?i)status|next|step", output)))

})
