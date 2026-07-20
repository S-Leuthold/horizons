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


## ----------------------------------------------------------------------------
## validate_horizons_ensemble()
## ----------------------------------------------------------------------------

## Minimal valid weighted contract, built inline (the test-class-core idiom).
## A tiny trained workflow for the metamodel checks is fit once below.
make_valid_ensemble <- function() {

  weights <- tibble::tibble(
    member = c("cfg_a", "cfg_b"),
    coef   = c(0.6, 0.4)
  )

  obj <- list(
    ensemble = list(
      method          = "weighted",
      model           = weights,
      weights         = weights,
      predictions     = tibble::tibble(sample_id = c("S1", "S2"),
                                       .pred     = c(1.1, 2.2),
                                       truth     = c(1.0, 2.0)),
      metrics         = tibble::tibble(.metric    = "rmse",
                                       .estimator = "standard",
                                       .estimate  = 0.15),
      member_metrics  = tibble::tibble(.metric   = c("rmse", "rmse"),
                                       .estimate = c(0.2, 0.3),
                                       config_id = c("cfg_a", "cfg_b")),
      improvement     = 0.05,
      oof_predictions = tibble::tibble(.row  = 1:2,
                                       .pred = c(1.2, 2.1),
                                       truth = c(1.0, 2.0)),
      optimize        = TRUE,
      seed            = 307L,
      uq              = NULL,
      timestamp       = Sys.time(),
      runtime_secs    = 1.5
    )
  )

  class(obj) <- c("horizons_ensemble", "horizons_fit", "horizons_eval",
                  "horizons_data", "list")
  obj

}

## One tiny trained workflow, reused by the metamodel-typed checks.
make_tiny_workflow <- function() {

  df <- tibble::tibble(.truth   = c(1, 2, 3, 4),
                       member_a = c(1.1, 1.9, 3.2, 3.8))

  parsnip::fit(
    workflows::workflow() %>%
      workflows::add_model(parsnip::linear_reg() %>%
                             parsnip::set_engine("lm")) %>%
      workflows::add_formula(.truth ~ .),
    data = df
  )

}

test_that("validate_horizons_ensemble passes a well-formed weighted contract", {

  ## Arrange
  obj <- make_valid_ensemble()

  ## Act & Assert
  expect_identical(validate_horizons_ensemble(obj), obj)

})

test_that("validate_horizons_ensemble passes a trained workflow for penalized", {

  ## Arrange
  obj                  <- make_valid_ensemble()
  obj$ensemble$method  <- "penalized"
  obj$ensemble$model   <- make_tiny_workflow()

  ## Act & Assert
  expect_identical(validate_horizons_ensemble(obj)$ensemble$method, "penalized")

})

test_that("validate_horizons_ensemble tolerates NULL oof_predictions, uq, optimize, seed", {

  ## Arrange. `[<-` with list(NULL) sets the value to NULL while KEEPING the
  ## key ($<- NULL would remove it and trip the completeness check) — this is
  ## exactly how build_ensemble_contract() constructs NULL-valued keys.
  obj <- make_valid_ensemble()
  obj$ensemble["oof_predictions"] <- list(NULL)
  obj$ensemble["uq"]              <- list(NULL)
  obj$ensemble["optimize"]        <- list(NULL)
  obj$ensemble["seed"]            <- list(NULL)

  ## Act & Assert
  expect_identical(validate_horizons_ensemble(obj), obj)

})

test_that("validate_horizons_ensemble accepts a well-formed CV+ uq bundle", {

  ## Arrange
  obj <- make_valid_ensemble()
  obj$ensemble$uq <- list(
    method        = "cv_plus",
    fold_models   = list(tibble::tibble(member = "cfg_a", coef = 1)),
    calib         = tibble::tibble(.row = 1L, fold = 1L, .pred_oof = 1,
                                   truth = 1, residual = 0),
    n_calib       = 1L,
    level_default = 0.90
  )

  ## Act & Assert
  expect_identical(validate_horizons_ensemble(obj), obj)

})

test_that("validate_horizons_ensemble gates on class and slot presence", {

  ## Not a horizons_ensemble
  expect_error(validate_horizons_ensemble(list()), "horizons_ensemble")

  ## Classed but no ensemble slot
  hollow <- structure(list(), class = c("horizons_ensemble", "list"))
  expect_error(validate_horizons_ensemble(hollow), "ensemble")

  ## Slot without a method
  no_method <- make_valid_ensemble()
  no_method$ensemble$method <- NULL
  expect_error(validate_horizons_ensemble(no_method), "method")

})

test_that("validate_horizons_ensemble names a missing contract key", {

  ## Arrange
  obj <- make_valid_ensemble()
  obj$ensemble$weights <- NULL   # NULL removes the key from the list

  ## Act & Assert
  expect_error(
    suppressMessages(validate_horizons_ensemble(obj)),
    "weights",
    class = "horizons_validation_error"
  )

})

test_that("validate_horizons_ensemble rejects an unknown method", {

  obj <- make_valid_ensemble()
  obj$ensemble$method <- "bogus"

  expect_error(validate_horizons_ensemble(obj),
               class = "horizons_validation_error")

})

test_that("validate_horizons_ensemble enforces per-method model typing", {

  ## weighted carrying a workflow
  wrong_weighted <- make_valid_ensemble()
  wrong_weighted$ensemble$model <- make_tiny_workflow()
  expect_error(validate_horizons_ensemble(wrong_weighted),
               class = "horizons_validation_error")

  ## penalized carrying a tibble
  wrong_penalized <- make_valid_ensemble()
  wrong_penalized$ensemble$method <- "penalized"
  expect_error(validate_horizons_ensemble(wrong_penalized),
               class = "horizons_validation_error")

  ## penalized carrying an untrained workflow
  untrained <- make_valid_ensemble()
  untrained$ensemble$method <- "penalized"
  untrained$ensemble$model  <- workflows::workflow() %>%
    workflows::add_model(parsnip::linear_reg() %>%
                           parsnip::set_engine("lm")) %>%
    workflows::add_formula(.truth ~ .)
  expect_error(validate_horizons_ensemble(untrained),
               class = "horizons_validation_error")

})

test_that("validate_horizons_ensemble rejects malformed weights", {

  ## Missing coef column
  no_coef <- make_valid_ensemble()
  no_coef$ensemble$weights <- tibble::tibble(member = c("a", "b"))
  expect_error(validate_horizons_ensemble(no_coef),
               class = "horizons_validation_error")

  ## Single member
  one_row <- make_valid_ensemble()
  one_row$ensemble$weights <- tibble::tibble(member = "a", coef = 1)
  expect_error(validate_horizons_ensemble(one_row),
               class = "horizons_validation_error")

  ## NA coefficient
  na_coef <- make_valid_ensemble()
  na_coef$ensemble$weights$coef[1] <- NA_real_
  expect_error(validate_horizons_ensemble(na_coef),
               class = "horizons_validation_error")

  ## Duplicate members
  dup <- make_valid_ensemble()
  dup$ensemble$weights$member <- c("a", "a")
  dup$ensemble$member_metrics$config_id <- c("a", "a")
  expect_error(validate_horizons_ensemble(dup),
               class = "horizons_validation_error")

})

test_that("validate_horizons_ensemble rejects malformed data-frame slots", {

  ## predictions missing truth
  no_truth <- make_valid_ensemble()
  no_truth$ensemble$predictions <- tibble::tibble(sample_id = "S1", .pred = 1)
  expect_error(validate_horizons_ensemble(no_truth),
               class = "horizons_validation_error")

  ## oof_predictions missing .row
  no_row <- make_valid_ensemble()
  no_row$ensemble$oof_predictions <- tibble::tibble(.pred = 1, truth = 1)
  expect_error(validate_horizons_ensemble(no_row),
               class = "horizons_validation_error")

  ## member_metrics referencing a non-member
  stranger <- make_valid_ensemble()
  stranger$ensemble$member_metrics$config_id[1] <- "cfg_zz"
  expect_error(validate_horizons_ensemble(stranger),
               class = "horizons_validation_error")

})

test_that("validate_horizons_ensemble rejects a malformed uq bundle", {

  ## Not a list
  not_list <- make_valid_ensemble()
  not_list$ensemble$uq <- "not a list"
  expect_error(validate_horizons_ensemble(not_list),
               class = "horizons_validation_error")

  ## Wrong discriminator
  wrong_method <- make_valid_ensemble()
  wrong_method$ensemble$uq <- list(method = "split_conformal",
                                   fold_models = list(), calib = NULL,
                                   n_calib = 0L, level_default = 0.9)
  expect_error(validate_horizons_ensemble(wrong_method),
               class = "horizons_validation_error")

  ## Missing core fields
  hollow_uq <- make_valid_ensemble()
  hollow_uq$ensemble$uq <- list(method = "cv_plus")
  expect_error(validate_horizons_ensemble(hollow_uq),
               class = "horizons_validation_error")

})


## ----------------------------------------------------------------------------
## validate_horizons_eval()
## ----------------------------------------------------------------------------

## A minimal valid horizons_eval, built inline (the test-class-core idiom). The
## split slot only needs to satisfy inherits(., "rsplit"); the validator does
## not inspect its contents, so a classed empty list stands in for a real one.
make_valid_eval <- function() {

  results <- tibble::tibble(
    config_id = c("cfg_a", "cfg_b"),
    status    = c("success", "success"),
    rmse      = c(1.0, 2.0),  rrmse = c(0.1, 0.2),
    rsq       = c(0.9, 0.8),  ccc   = c(0.9, 0.8),
    rpd       = c(2.0, 1.5),  mae   = c(0.8, 1.1)
  )

  obj <- list(
    config = list(configs = tibble::tibble(config_id = c("cfg_a", "cfg_b"))),
    evaluation = list(
      results      = results,
      best_config  = "cfg_a",
      rank_metric  = "rpd",
      split        = structure(list(), class = c("rsplit", "list")),
      n_train      = 80L,
      n_test       = 20L,
      workers      = 4L,
      runtime_secs = 12.3,
      timestamp    = Sys.time()
    )
  )

  class(obj) <- c("horizons_eval", "horizons_data", "list")
  obj

}

test_that("validate_horizons_eval passes a well-formed evaluation slot", {

  ## Arrange
  obj <- make_valid_eval()

  ## Act & Assert
  expect_identical(validate_horizons_eval(obj), obj)

})

test_that("validate_horizons_eval tolerates the optional workers key being absent", {

  ## `workers` is not a required contract key — its absence must not fail.
  obj <- make_valid_eval()
  obj$evaluation$workers <- NULL

  expect_identical(validate_horizons_eval(obj), obj)

})

test_that("validate_horizons_eval gates on class and slot presence", {

  ## Not a horizons_eval
  expect_error(validate_horizons_eval(list()), "horizons_eval")

  ## Classed but no evaluation slot
  hollow <- structure(list(), class = c("horizons_eval", "list"))
  expect_error(validate_horizons_eval(hollow), "evaluation")

})

test_that("validate_horizons_eval enforces I5: best_config names a real config", {

  ## best_config absent from results
  ghost_result <- make_valid_eval()
  ghost_result$evaluation$best_config <- "cfg_ghost"
  expect_error(
    suppressMessages(validate_horizons_eval(ghost_result)),
    class = "horizons_validation_error"
  )

  ## best_config present in results but not in the config catalog
  ghost_config <- make_valid_eval()
  ghost_config$evaluation$results$config_id <- c("cfg_x", "cfg_b")
  ghost_config$evaluation$best_config       <- "cfg_x"
  expect_error(
    suppressMessages(validate_horizons_eval(ghost_config)),
    class = "horizons_validation_error"
  )

})

test_that("validate_horizons_eval rejects a malformed results table", {

  ## Missing a metric column
  no_rpd <- make_valid_eval()
  no_rpd$evaluation$results$rpd <- NULL
  expect_error(suppressMessages(validate_horizons_eval(no_rpd)),
               class = "horizons_validation_error")

  ## Zero rows
  empty <- make_valid_eval()
  empty$evaluation$results <- empty$evaluation$results[0, ]
  empty$evaluation$best_config <- "cfg_a"
  expect_error(suppressMessages(validate_horizons_eval(empty)),
               class = "horizons_validation_error")

  ## Duplicate config_id
  dup <- make_valid_eval()
  dup$evaluation$results$config_id <- c("cfg_a", "cfg_a")
  expect_error(suppressMessages(validate_horizons_eval(dup)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_eval rejects an unknown rank_metric", {

  obj <- make_valid_eval()
  obj$evaluation$rank_metric <- "banana"
  expect_error(suppressMessages(validate_horizons_eval(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_eval rejects a non-rsplit split", {

  obj <- make_valid_eval()
  obj$evaluation$split <- list()
  expect_error(suppressMessages(validate_horizons_eval(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_eval rejects fractional or negative sample counts", {

  frac <- make_valid_eval()
  frac$evaluation$n_train <- 80.5
  expect_error(suppressMessages(validate_horizons_eval(frac)),
               class = "horizons_validation_error")

  neg <- make_valid_eval()
  neg$evaluation$n_test <- -1L
  expect_error(suppressMessages(validate_horizons_eval(neg)),
               class = "horizons_validation_error")

})


## ----------------------------------------------------------------------------
## validate_horizons_fit()
## ----------------------------------------------------------------------------

## A minimal valid horizons_fit built on top of make_valid_eval() — a fit is
## also an eval, so it carries a valid evaluation slot (the validator delegates
## to validate_horizons_eval first). Workflows are structural stubs: the fit
## validator checks names + list-ness, not workflow-trained-ness.
make_valid_fit <- function() {

  obj <- make_valid_eval()

  results <- obj$evaluation$results

  obj$models <- list(
    workflows        = list(cfg_a = structure(list(), class = "workflow"),
                            cfg_b = structure(list(), class = "workflow")),
    n_models         = 2L,
    best_config      = "cfg_a",
    rank_metric      = "rpd",
    predictor_schema = c("4000", "3999", "3998"),
    response_bound   = 45.2,
    cv_predictions   = tibble::tibble(),
    results          = results,
    split            = obj$evaluation$split,
    row_index        = tibble::tibble(.row = 1:80,
                                      sample_id = paste0("S", 1:80)),
    uq               = list(cfg_a = list(quantile_model = 1)),
    timestamp        = Sys.time(),
    runtime_secs     = 30.1
  )

  class(obj) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")
  obj

}

test_that("validate_horizons_fit passes a well-formed models slot", {

  obj <- make_valid_fit()
  expect_identical(validate_horizons_fit(obj), obj)

})

test_that("validate_horizons_fit tolerates a NULL response_bound (pre-clamp objects)", {

  ## Objects fitted before the winsorization guardrail shipped carry no bound
  ## and predict without a clamp — the validator must accept that. `[<-` with
  ## list(NULL) sets NULL while KEEPING the key (so the completeness check still
  ## sees it).
  obj <- make_valid_fit()
  obj$models["response_bound"] <- list(NULL)

  expect_identical(validate_horizons_fit(obj), obj)

})

test_that("validate_horizons_fit tolerates a NULL uq slot (compute_uq = FALSE)", {

  obj <- make_valid_fit()
  obj$models["uq"] <- list(NULL)

  expect_identical(validate_horizons_fit(obj), obj)

})

test_that("validate_horizons_fit gates on class and slot presence", {

  ## Not a horizons_fit
  expect_error(validate_horizons_fit(list()), "horizons_fit")

  ## Classed but no models slot
  hollow <- structure(list(), class = c("horizons_fit", "list"))
  expect_error(validate_horizons_fit(hollow), "models")

})

test_that("validate_horizons_fit delegates to the evaluation contract", {

  ## A broken parent evaluation slot must surface through the fit validator.
  obj <- make_valid_fit()
  obj$evaluation$best_config <- "cfg_ghost"
  expect_error(suppressMessages(validate_horizons_fit(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_fit enforces the response_bound guardrail contract", {

  ## Negative bound: the clamp would compare predictions against a nonsense
  ## threshold, silently winsorizing everything.
  neg <- make_valid_fit()
  neg$models$response_bound <- -5
  expect_error(suppressMessages(validate_horizons_fit(neg)),
               class = "horizons_validation_error")

  ## Non-finite bound
  inf <- make_valid_fit()
  inf$models$response_bound <- Inf
  expect_error(suppressMessages(validate_horizons_fit(inf)),
               class = "horizons_validation_error")

  ## Length > 1
  vec <- make_valid_fit()
  vec$models$response_bound <- c(45.2, 90.4)
  expect_error(suppressMessages(validate_horizons_fit(vec)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_fit enforces I6: workflow keys are a subset of config ids", {

  obj <- make_valid_fit()
  names(obj$models$workflows) <- c("cfg_a", "cfg_ghost")
  obj$models$n_models <- 2L
  expect_error(suppressMessages(validate_horizons_fit(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_fit enforces I7: uq keys are a subset of workflow keys", {

  ## A UQ bundle keyed by a config with no fitted workflow would be unreachable
  ## at predict time.
  obj <- make_valid_fit()
  obj$models$uq <- list(cfg_ghost = list(quantile_model = 1))
  expect_error(suppressMessages(validate_horizons_fit(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_fit rejects an n_models / workflows mismatch", {

  obj <- make_valid_fit()
  obj$models$n_models <- 5L
  expect_error(suppressMessages(validate_horizons_fit(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_fit rejects an empty predictor_schema", {

  obj <- make_valid_fit()
  obj$models$predictor_schema <- character(0)
  expect_error(suppressMessages(validate_horizons_fit(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_fit rejects a best_config not among workflows", {

  obj <- make_valid_fit()
  obj$models$best_config <- "cfg_b"
  names(obj$models$workflows) <- c("cfg_a", "cfg_c")   # cfg_b now absent
  obj$models$uq <- NULL                                # avoid an unrelated I7 hit
  expect_error(suppressMessages(validate_horizons_fit(obj)),
               class = "horizons_validation_error")

})


## ----------------------------------------------------------------------------
## Stage predicates + has_uq()
## ----------------------------------------------------------------------------

test_that("stage predicates track the class hierarchy", {

  data <- structure(list(),
                    class = c("horizons_data", "list"))
  eval <- structure(list(),
                    class = c("horizons_eval", "horizons_data", "list"))
  fit  <- structure(list(),
                    class = c("horizons_fit", "horizons_eval",
                              "horizons_data", "list"))
  ens  <- structure(list(),
                    class = c("horizons_ensemble", "horizons_fit",
                              "horizons_eval", "horizons_data", "list"))

  ## is_evaluated: TRUE from horizons_eval down
  expect_false(is_evaluated(data))
  expect_true(is_evaluated(eval))
  expect_true(is_evaluated(fit))
  expect_true(is_evaluated(ens))

  ## is_fitted: TRUE from horizons_fit down
  expect_false(is_fitted(eval))
  expect_true(is_fitted(fit))
  expect_true(is_fitted(ens))

  ## is_ensembled: TRUE only for horizons_ensemble
  expect_false(is_ensembled(fit))
  expect_true(is_ensembled(ens))

})

test_that("stage predicates are safe on non-horizons objects", {

  expect_false(is_evaluated(42))
  expect_false(is_fitted("x"))
  expect_false(is_ensembled(list()))

})

test_that("has_uq reflects the fitted UQ slot", {

  with_uq <- structure(list(models = list(uq = list(cfg_a = 1))),
                       class = c("horizons_fit", "horizons_eval",
                                 "horizons_data", "list"))
  expect_true(has_uq(with_uq))

  ## NULL uq (compute_uq = FALSE)
  no_uq <- structure(list(models = list(uq = NULL)),
                     class = c("horizons_fit", "horizons_eval",
                               "horizons_data", "list"))
  expect_false(has_uq(no_uq))

  ## empty-list uq
  empty_uq <- structure(list(models = list(uq = list())),
                        class = c("horizons_fit", "horizons_eval",
                                  "horizons_data", "list"))
  expect_false(has_uq(empty_uq))

})

test_that("has_uq is safe on objects without a models slot", {

  expect_false(has_uq(list()))
  expect_false(has_uq(structure(list(), class = c("horizons_data", "list"))))

})

test_that("has_uq is safe on atomic (non-list) inputs", {

  ## `$` errors on atomic vectors, so the is.list() guard is what keeps the
  ## "safe on any object" contract honest.
  expect_false(has_uq(42))
  expect_false(has_uq("x"))
  expect_false(has_uq(TRUE))
  expect_false(has_uq(NULL))

})
