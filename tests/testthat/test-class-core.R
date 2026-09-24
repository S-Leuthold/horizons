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

test_that("new_horizons_data creates object with all 9 sections", {

  obj <- new_horizons_data()

  expected_sections <- c(
    "data",
    "provenance",
    "config",
    "validation",
    "evaluation",
    "models",
    "ensemble",
    "artifacts",
    "selection"
  )

  expect_true(all(expected_sections %in% names(obj)))
  expect_equal(length(obj), 9)
  expect_null(obj$selection)

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

test_that("validate_horizons_data truncates a long duplicate-sample_id list to 5 and a count (#24)", {

  ## Arrange — 8 duplicated ids; at library scale (thousands of replicate
  ## groups) the full list is unreadable, so the message names the first 5
  ## and counts the rest.
  dup_letters <- LETTERS[1:8]
  test_analysis <- tibble::tibble(
    sample_id = c(rep(dup_letters, each = 2), "Z"),
    `4000`    = seq_len(length(dup_letters) * 2 + 1) / 10
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role     = c("id", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  err <- tryCatch(validate_horizons_data(obj), error = function(e) e)

  expect_s3_class(err, "horizons_validation_error")
  msg <- conditionMessage(err)

  ## First 5 named, the remaining 3 counted rather than listed
  expect_match(msg, paste(dup_letters[1:5], collapse = ", "), fixed = TRUE)
  expect_match(msg, "and 3 more", fixed = TRUE)
  expect_false(grepl(dup_letters[6], msg, fixed = TRUE))

})

## ---------------------------------------------------------------------------
## stage = "raw" vs "full" (#24 rework)
## ---------------------------------------------------------------------------
## Duplicate or NA sample ids are the normal state before average() collapses
## replicate scans, so spectra(), parse_ids() and standardize() validate at
## stage = "raw", where the same conditions warn instead of aborting. Every
## other caller (average() onward) keeps the default stage = "full".

test_that("stage = \"raw\" warns instead of aborting on duplicate sample_id", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", "A", "B"),
    `4000` = c(0.1, 0.2, 0.3)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_warning(
    result <- validate_horizons_data(obj, stage = "raw"),
    class = "horizons_validation_warning"
  )

  ## Object is returned unchanged, duplicates and all
  expect_identical(result$data$analysis$sample_id, c("A", "A", "B"))

})

test_that("stage = \"full\" (the default) still aborts on duplicate sample_id", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", "A", "B"),
    `4000` = c(0.1, 0.2, 0.3)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_error(
    validate_horizons_data(obj),
    class = "horizons_validation_error"
  )

  ## Explicit stage = "full" behaves identically to the default
  expect_error(
    validate_horizons_data(obj, stage = "full"),
    class = "horizons_validation_error"
  )

})

test_that("NA sample_id warns at stage = \"raw\", aborts at stage = \"full\"", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", NA_character_, "C"),
    `4000` = c(0.1, 0.2, 0.3)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_warning(
    raw_result <- validate_horizons_data(obj, stage = "raw"),
    class = "horizons_validation_warning"
  )
  expect_true(anyNA(raw_result$data$analysis$sample_id))

  expect_error(
    validate_horizons_data(obj),
    class = "horizons_validation_error"
  )

})

test_that("an NA sample_id is not also reported as a duplicate of itself", {

  ## Two NA ids plus one real duplicate: the NA check and the duplicate
  ## check should each fire once, not have the NAs double-counted as a
  ## "duplicate" pair on top of being individually NA.
  test_analysis <- tibble::tibble(
    sample_id = c("A", "A", NA_character_, NA_character_, "C"),
    `4000` = c(0.1, 0.2, 0.3, 0.4, 0.5)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role = c("id", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  err <- tryCatch(validate_horizons_data(obj), error = function(e) e)

  expect_s3_class(err, "horizons_validation_error")
  expect_match(conditionMessage(err), "2 NA", fixed = TRUE)
  expect_match(conditionMessage(err), "Duplicate.*A", perl = TRUE)
  expect_no_match(conditionMessage(err), "NA.*NA", perl = TRUE)

})

test_that("stage must be \"full\" or \"raw\"", {

  obj <- new_horizons_data()

  expect_error(validate_horizons_data(obj, stage = "partial"))

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

test_that("stage = \"raw\" does not check predictor NA or Inf at all", {

  ## Arrange — same fixture as the NA-predictor test above, but a value
  ## outside standardize()'s eventual trim range is legitimately still NA
  ## before spectra()/parse_ids()/standardize() run (#24 rework); raw stage
  ## skips the check entirely rather than downgrading it to a warning.
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B", "C"),
    `4000` = c(0.1, NA, 0.3),
    `3998` = c(0.2, 0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "3998"),
    role = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_no_warning(expect_no_error(validate_horizons_data(obj, stage = "raw")))

  ## Inf is treated the same as NA at raw stage: a non-finite value outside
  ## standardize()'s eventual trim range is legitimately still there before
  ## the trim runs, and standardize() itself aborts on any non-finite value
  ## that survives trimming and resampling, so this is only enforced from
  ## average() on (#24 rework).
  test_analysis$`4000` <- c(0.1, Inf, 0.3)
  obj_inf <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_no_warning(expect_no_error(validate_horizons_data(obj_inf, stage = "raw")))

  ## Full stage (the default) still catches it.
  expect_error(validate_horizons_data(obj_inf), "Inf|infinite")

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

test_that("validate_horizons_data names duplicate wavenumber columns rather than a generic order failure (#24)", {

  ## Arrange — "600" and "600.0" both parse to the same wavenumber; sorted,
  ## they trivially fail strict decrease, but the message should name them
  ## rather than just saying "must be strictly decreasing".
  test_analysis <- tibble::tibble(
    sample_id  = c("A", "B"),
    `4000`     = c(0.1, 0.2),
    `600`      = c(0.3, 0.4),
    `600.0`    = c(0.5, 0.6)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "600", "600.0"),
    role     = c("id", "predictor", "predictor", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  err <- tryCatch(validate_horizons_data(obj), error = function(e) e)

  expect_s3_class(err, "horizons_validation_error")
  expect_match(conditionMessage(err), "Duplicate wavenumber", fixed = TRUE)

  ## Both columns of the pair are named, not just the later occurrence
  ## duplicated() flags — "600" alone would also match as a substring of
  ## "600.0", so check the exact paired listing rather than each in
  ## isolation.
  expect_match(conditionMessage(err), "600, 600.0", fixed = TRUE)

})

test_that("validate_horizons_data names an unparseable wavenumber column rather than crashing (#24)", {

  ## Arrange — "600.600.1" matches the wn_<digits and dots> pattern but does
  ## not parse as a number. Before this check existed, `as.numeric()` on the
  ## whole vector produced an NA, and `diff()`/`all()` on that NA raised the
  ## raw R error "missing value where TRUE/FALSE needed" instead of a
  ## validator message.
  test_analysis <- tibble::tibble(
    sample_id     = c("A", "B"),
    `4000`        = c(0.1, 0.2),
    `600.600.1`   = c(0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "600.600.1"),
    role     = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  err <- tryCatch(validate_horizons_data(obj), error = function(e) e)

  expect_s3_class(err, "horizons_validation_error")
  ## Singular subject, singular verb: one unparseable column reads "does not
  ## parse", not "do not parse" (#24 grammar fix).
  expect_match(conditionMessage(err), "does not parse", fixed = TRUE)
  expect_match(conditionMessage(err), "600.600.1", fixed = TRUE)

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

test_that("validate_horizons_data refuses a column with more than one role_map row, naming it", {

  ## Arrange — "elevation" has two role_map rows (meta and covariate). Every
  ## consumer that reads role_map$role[role_map$variable == "elevation"]
  ## expecting one value gets an ambiguous answer, and build_recipe()'s
  ## `outcome ~ .` would silently treat it as an unregistered predictor
  ## rather than failing loudly. Presence in role_map was already checked;
  ## this is about uniqueness.
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    `4000`    = c(0.1, 0.2),
    elevation = c(100, 200)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000", "elevation", "elevation"),
    role     = c("id", "predictor", "meta", "covariate")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  err <- tryCatch(validate_horizons_data(obj), error = function(e) e)

  expect_s3_class(err, "horizons_validation_error")
  expect_match(conditionMessage(err), "more than one", fixed = TRUE)
  expect_match(conditionMessage(err), "elevation", fixed = TRUE)

})

test_that("an unregistered analysis column is refused, naming it", {

  ## Arrange — "mystery_col" has zero role_map rows. This is the presence
  ## check (distinct from the uniqueness check above): a column omitted from
  ## role_map entirely falls through build_recipe()'s `outcome ~ .` as an
  ## unintended predictor.
  test_analysis <- tibble::tibble(
    sample_id   = c("A", "B"),
    `4000`      = c(0.1, 0.2),
    mystery_col = c(1, 2)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "4000"),
    role     = c("id", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  err <- tryCatch(validate_horizons_data(obj), error = function(e) e)

  expect_s3_class(err, "horizons_validation_error")
  expect_match(conditionMessage(err), "mystery_col", fixed = TRUE)

})


## ---------------------------------------------------------------------------
## Wavelength order, wn_ prefixed
## ---------------------------------------------------------------------------

test_that("validate_horizons_data catches out-of-order wn_ prefixed predictors", {

  ## Arrange — the naming every verb in the package actually mints
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    wn_3996   = c(0.1, 0.2),
    wn_4000   = c(0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "wn_3996", "wn_4000"),
    role     = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  ## Act & Assert
  expect_error(validate_horizons_data(obj), "decreasing")

})


test_that("validate_horizons_data accepts wn_ prefixed predictors in decreasing order", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    wn_4000   = c(0.3, 0.4),
    wn_3996   = c(0.1, 0.2)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000", "wn_3996"),
    role     = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_no_error(validate_horizons_data(obj))

})


## ---------------------------------------------------------------------------
## Role vocabulary, outcome cardinality and stored counts
## ---------------------------------------------------------------------------

test_that("validate_horizons_data rejects a role outside the vocabulary", {

  ## Arrange — a typo'd role would otherwise make the column invisible
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    wn_4000   = c(0.1, 0.2),
    clay      = c(10, 20)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000", "clay"),
    role     = c("id", "predictor", "responce")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  ## Act & Assert
  expect_error(validate_horizons_data(obj),
               regexp = "responce",
               class  = "horizons_validation_error")

})


test_that("validate_horizons_data accepts every role in the vocabulary", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    wn_4000   = c(0.1, 0.2),
    clay      = c(10, 20),
    oc        = c(1.1, 1.2),
    elevation = c(300, 310),
    plot      = c("p1", "p2")
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000", "clay", "oc", "elevation", "plot"),
    role     = c("id", "predictor", "outcome", "response", "covariate", "meta")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_no_error(validate_horizons_data(obj))

})


test_that("validate_horizons_data rejects more than one outcome role", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    wn_4000   = c(0.1, 0.2),
    clay      = c(10, 20),
    oc        = c(1.1, 1.2)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000", "clay", "oc"),
    role     = c("id", "predictor", "outcome", "outcome")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  expect_error(validate_horizons_data(obj),
               regexp = "(?i)outcome",
               class  = "horizons_validation_error")

})


test_that("validate_horizons_data catches stored counts that drifted from the data", {

  ## Arrange — the failure mode when a verb assigns counts by hand
  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    wn_4000   = c(0.1, 0.2),
    wn_3996   = c(0.3, 0.4)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000", "wn_3996"),
    role     = c("id", "predictor", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  obj$data$n_predictors <- 426L
  obj$data$n_rows       <- 99L

  ## Act & Assert
  expect_error(validate_horizons_data(obj),
               regexp = "n_predictors",
               class  = "horizons_validation_error")

  expect_error(validate_horizons_data(obj), regexp = "n_rows")

})


test_that("validate_horizons_data tolerates absent stored counts", {

  test_analysis <- tibble::tibble(
    sample_id = c("A", "B"),
    wn_4000   = c(0.1, 0.2)
  )

  test_role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000"),
    role     = c("id", "predictor")
  )

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)

  obj$data$n_covariates <- NULL
  obj$data$n_responses  <- NULL

  expect_no_error(validate_horizons_data(obj))

})


## ---------------------------------------------------------------------------
## Selection record shape
## ---------------------------------------------------------------------------

test_that("validate_horizons_data accepts a well-formed selection record", {

  ## Arrange
  obj <- new_horizons_data()
  obj$selection <- make_selection_record()

  ## Act & Assert
  expect_no_error(validate_horizons_data(obj))

})


test_that("validate_horizons_data names every missing piece of a selection record", {

  ## Arrange
  obj <- new_horizons_data()
  obj$selection <- make_selection_record()

  obj$selection$membership <- NULL
  obj$selection$groups     <- NULL

  ## Act & Assert
  expect_error(validate_horizons_data(obj),
               regexp = "membership",
               class  = "horizons_validation_error")

  expect_error(validate_horizons_data(obj), regexp = "groups")

})


test_that("validate_horizons_data rejects a selection record of the wrong type", {

  obj <- new_horizons_data()
  obj$selection <- make_selection_record()
  obj$selection$settings   <- "batch"
  obj$selection$pool_sizes <- list(property = "clay")

  expect_error(validate_horizons_data(obj),
               regexp = "settings",
               class  = "horizons_validation_error")

  expect_error(validate_horizons_data(obj), regexp = "pool_sizes")

})


test_that("validate_horizons_data names a selection table missing a column", {

  ## Arrange — a membership without pool_id is indexed by name by every
  ## consumer of the record
  obj <- new_horizons_data()
  obj$selection <- make_selection_record()
  obj$selection$membership$pool_id <- NULL

  ## Act & Assert
  expect_error(validate_horizons_data(obj),
               regexp = "pool_id",
               class  = "horizons_validation_error")

})


test_that("validate_horizons_data enforces I4b containment on membership", {

  ## Arrange — a record naming a pool id the analysis table does not have
  ids      <- sprintf("P%03d", 1:4)
  analysis <- tibble::tibble(sample_id = ids, wn_4000 = seq(0.1, 0.4, by = 0.1))
  role_map <- tibble::tibble(variable = c("sample_id", "wn_4000"),
                             role     = c("id", "predictor"))

  obj <- new_horizons_data(analysis = analysis, role_map = role_map)
  obj$selection <- make_selection_record(pool_ids = ids)

  expect_no_error(validate_horizons_data(obj))

  obj$selection$membership$pool_id[1] <- "P999"

  ## Act & Assert
  expect_error(validate_horizons_data(obj),
               regexp = "P999",
               class  = "horizons_validation_error")

})


test_that("I4b exempts membership rows marked retained = FALSE", {

  ## Arrange — the union subtraction keeps the row in the record, and it was
  ## never in the analysis table
  ids      <- sprintf("P%03d", 1:4)
  analysis <- tibble::tibble(sample_id = ids, wn_4000 = seq(0.1, 0.4, by = 0.1))
  role_map <- tibble::tibble(variable = c("sample_id", "wn_4000"),
                             role     = c("id", "predictor"))

  obj <- new_horizons_data(analysis = analysis, role_map = role_map)
  obj$selection <- make_selection_record(pool_ids = ids)

  obj$selection$membership$pool_id[1]  <- "P999"
  obj$selection$membership$retained[1] <- FALSE

  ## Act & Assert
  expect_no_error(validate_horizons_data(obj))

})


test_that("validate_horizons_data enforces I4b containment on groups$pool_ids", {

  ids      <- sprintf("P%03d", 1:4)
  analysis <- tibble::tibble(sample_id = ids, wn_4000 = seq(0.1, 0.4, by = 0.1))
  role_map <- tibble::tibble(variable = c("sample_id", "wn_4000"),
                             role     = c("id", "predictor"))

  obj <- new_horizons_data(analysis = analysis, role_map = role_map)
  obj$selection <- make_selection_record(pool_ids = ids)

  obj$selection$groups$pool_ids[[1]] <- c(obj$selection$groups$pool_ids[[1]], "P999")

  expect_error(validate_horizons_data(obj),
               regexp = "P999",
               class  = "horizons_validation_error")

})


test_that("the synthetic selection record matches the real one", {

  ## A synthetic record that has drifted from the verb's output is not
  ## evidence about anything, so the two are compared directly.

  skip_on_cran()

  ## Arrange
  fx  <- make_select_fixture()
  out <- suppressWarnings(select_training(fx$targets, fx$pool, k = 5, verbose = FALSE))

  ## Act
  synthetic <- make_selection_record()

  ## Assert
  expect_identical(check_selection_shape(synthetic), character())
  expect_identical(names(synthetic), names(out$selection))

})


test_that("validate_horizons_data checks the selection record alongside the data", {

  ## Arrange — a data problem and a selection problem in one object
  test_analysis <- tibble::tibble(sample_id = c("A", "A"), wn_4000 = c(0.1, 0.2))
  test_role_map <- tibble::tibble(variable = c("sample_id", "wn_4000"),
                                  role     = c("id", "predictor"))

  obj <- new_horizons_data(analysis = test_analysis, role_map = test_role_map)
  obj$selection <- make_selection_record()
  obj$selection$exclusions <- NULL

  ## Act & Assert
  expect_error(validate_horizons_data(obj), regexp = "exclusions")
  expect_error(validate_horizons_data(obj), regexp = "Duplicate")

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
    rpd       = c(2.0, 1.5),  mae   = c(0.8, 1.1),
    ## cross-validated means at the selected hyperparameters (#50)
    cv_rmse   = c(1.1, 2.1),  cv_rrmse = c(0.11, 0.21),
    cv_rsq    = c(0.88, 0.78), cv_ccc  = c(0.88, 0.78),
    cv_rpd    = c(1.9, 1.4),  cv_mae   = c(0.9, 1.2)
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
      parallelize_over = "configs",
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
    ad               = list(cfg_a = list(centroid = 1, cov_matrix = 1,
                                         ad_thresholds = 1:4)),
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

test_that("validate_horizons_fit takes its required keys from the constructor", {

  ## Keys added after objects without them were saved may be absent
  ## (CONTRACT_KEYS_OPTIONAL); every other key new_horizons_data() declares
  ## for the slot is required, ad included.
  old <- make_valid_fit()
  old$models$response_bound    <- NULL   # NULL removes the key
  old$models$selection_present <- NULL

  expect_identical(validate_horizons_fit(old), old)

  no_ad <- make_valid_fit()
  no_ad$models$ad <- NULL

  expect_error(suppressMessages(validate_horizons_fit(no_ad)),
               "missing from models: ad",
               class = "horizons_validation_error")

  expect_identical(contract_keys("models"),
                   setdiff(names(new_horizons_data()$models),
                           c("response_bound", "selection_present")))

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

test_that("validate_horizons_fit tolerates a NULL ad slot (compute_ad = FALSE)", {

  obj <- make_valid_fit()
  obj$models["ad"] <- list(NULL)

  expect_identical(validate_horizons_fit(obj), obj)

})

test_that("validate_horizons_fit enforces the AD-subset invariant", {

  ## AD metadata keyed by a config with no fitted workflow is unreachable at
  ## predict time — same contract as uq.
  obj <- make_valid_fit()
  obj$models$ad <- list(cfg_ghost = list(centroid = 1, cov_matrix = 1,
                                         ad_thresholds = 1:4))
  expect_error(suppressMessages(validate_horizons_fit(obj)),
               class = "horizons_validation_error")

})

test_that("validate_horizons_fit rejects a non-list ad slot", {

  obj <- make_valid_fit()
  obj$models$ad <- "not a list"
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

test_that("has_ad reflects the fitted AD slot", {

  with_ad <- structure(list(models = list(ad = list(cfg_a = 1))),
                       class = c("horizons_fit", "horizons_eval",
                                 "horizons_data", "list"))
  expect_true(has_ad(with_ad))

  ## NULL ad (compute_ad = FALSE)
  no_ad <- structure(list(models = list(ad = NULL)),
                     class = c("horizons_fit", "horizons_eval",
                               "horizons_data", "list"))
  expect_false(has_ad(no_ad))

  ## empty-list ad
  empty_ad <- structure(list(models = list(ad = list())),
                        class = c("horizons_fit", "horizons_eval",
                                  "horizons_data", "list"))
  expect_false(has_ad(empty_ad))

})

test_that("has_ad is safe on atomic and model-less inputs", {

  expect_false(has_ad(42))
  expect_false(has_ad(NULL))
  expect_false(has_ad(list()))
  expect_false(has_ad(structure(list(), class = c("horizons_data", "list"))))

})

test_that("has_ad and has_uq are independent", {

  ## AD present, UQ absent
  ad_only <- structure(list(models = list(ad = list(cfg_a = 1), uq = NULL)),
                       class = c("horizons_fit", "horizons_eval",
                                 "horizons_data", "list"))
  expect_true(has_ad(ad_only))
  expect_false(has_uq(ad_only))

})


## =========================================================================
## Run-provenance slots: tolerated when absent, validated when present
## (I7c, 2026-09-15)
## =========================================================================

test_that("validate_horizons_eval tolerates parallelize_over being absent (pre-2026-09-15 objects)", {

  obj <- make_valid_eval()
  obj$evaluation$parallelize_over <- NULL
  obj$evaluation$workers          <- 4L      # the OLD meaning: requested count

  expect_identical(validate_horizons_eval(obj), obj)

})

test_that("validate_horizons_eval rejects a present-but-NULL parallelize_over", {

  obj <- make_valid_eval()
  obj$evaluation["parallelize_over"] <- list(NULL)

  expect_error(validate_horizons_eval(obj), "parallelize_over")

})

test_that("validate_horizons_eval rejects an unknown parallelize_over value", {

  obj <- make_valid_eval()
  obj$evaluation$parallelize_over <- "both"

  expect_error(validate_horizons_eval(obj), "parallelize_over")

})

test_that("validate_horizons_eval validates workers under the new meaning when parallelize_over is present", {

  obj <- make_valid_eval()

  obj$evaluation$workers <- 0L
  expect_error(validate_horizons_eval(obj), "workers")

  obj$evaluation$workers <- 2.5
  expect_error(validate_horizons_eval(obj), "workers")

  obj$evaluation$workers <- NA_integer_     # unbounded backend
  expect_identical(validate_horizons_eval(obj), obj)

})

test_that("validate_horizons_eval tolerates screened being absent and checks it when present (#45)", {

  ## make_valid_eval() carries no `screened`, as objects evaluated before the
  ## key existed do not.
  obj <- make_valid_eval()

  expect_false("screened" %in% names(obj$evaluation))
  expect_false("screened" %in% contract_keys("evaluation"))
  expect_identical(validate_horizons_eval(obj), obj)

  ## fit()'s cold start writes FALSE, evaluate() TRUE
  obj$evaluation$screened <- FALSE
  expect_identical(validate_horizons_eval(obj), obj)

  obj$evaluation$screened <- NA
  expect_error(suppressMessages(validate_horizons_eval(obj)), "screened",
               class = "horizons_validation_error")

  obj$evaluation$screened <- c(TRUE, FALSE)
  expect_error(suppressMessages(validate_horizons_eval(obj)), "screened",
               class = "horizons_validation_error")

})

test_that("validate_horizons_eval accepts evaluation$recipe, and tolerates its absence (#62)", {

  ## The fixture has no recipe key, which is every object evaluated before
  ## evaluate() recorded the recipe settings.
  obj <- make_valid_eval()
  expect_false("recipe" %in% names(obj$evaluation))
  expect_identical(validate_horizons_eval(obj), obj)

  obj$evaluation$recipe <- list(sg_window = 9L, sg_window_cm = 18, pca_threshold = 0.995)
  expect_identical(validate_horizons_eval(obj), obj)

  ## The width is NA when the axis carries no wavenumbers
  obj$evaluation$recipe$sg_window_cm <- NA_real_
  expect_identical(validate_horizons_eval(obj), obj)

})

test_that("validate_horizons_eval tolerates response_trim absent or NULL, and checks a record (#77)", {

  ## The fixture has no response_trim key, which is every object evaluated
  ## before evaluate() trimmed the training partition.
  obj <- make_valid_eval()
  expect_false("response_trim" %in% names(obj$evaluation))
  expect_false("response_trim" %in% contract_keys("evaluation"))
  expect_identical(validate_horizons_eval(obj), obj)

  ## No trim requested
  obj$evaluation["response_trim"] <- list(NULL)
  expect_identical(validate_horizons_eval(obj), obj)

  obj$evaluation$response_trim <- list(outcome = "SOC", trimmed_ids = c("S001", "S002"))
  expect_identical(validate_horizons_eval(obj), obj)

  ## fit() drops the recorded rows, so a record without them is refused
  obj$evaluation$response_trim <- list(outcome = "SOC", trimmed_ids = 1:2)
  expect_error(suppressMessages(validate_horizons_eval(obj)), "response_trim",
               class = "horizons_validation_error")

  obj$evaluation$response_trim <- "S001"
  expect_error(suppressMessages(validate_horizons_eval(obj)), "response_trim",
               class = "horizons_validation_error")

})

test_that("validate_horizons_eval rejects a malformed evaluation$recipe", {

  obj <- make_valid_eval()

  obj$evaluation$recipe <- list(sg_window = 9.5, pca_threshold = 0.995)
  expect_error(suppressMessages(validate_horizons_eval(obj)), "recipe")

  obj$evaluation$recipe <- list(sg_window = 9L)
  expect_error(suppressMessages(validate_horizons_eval(obj)), "recipe")

  obj$evaluation$recipe <- "sg9"
  expect_error(suppressMessages(validate_horizons_eval(obj)), "recipe")

})

test_that("the committed ensemble fixture (evaluated before the slot existed) still validates", {

  ## The fixture also predates the response_bound / ad model slots, so the
  ## fit-level validator rejects it for reasons unrelated to this contract;
  ## the eval-level validator is what the tolerance rule governs.
  fx <- readRDS(test_path("fixtures", "ensemble_fit.rds"))

  expect_false("parallelize_over" %in% names(fx$evaluation))
  expect_identical(validate_horizons_eval(fx), fx)

})


## ---------------------------------------------------------------------------
## Selection section in print() and summary()
## ---------------------------------------------------------------------------

test_that("print.horizons_data shows the selection section", {

  ## Arrange
  obj <- make_selected_object()

  ## Act
  output <- capture.output(print(obj))

  ## Assert
  expect_true(any(grepl("Selection", output)))
  expect_true(any(grepl("Scope: batch", output)))
  expect_true(any(grepl("clay 12/12", output)))
  expect_true(any(grepl("twins excluded: 0", output)))

})


test_that("print.horizons_data survives a partially-formed selection record", {

  ## Arrange — a record missing everything print() dereferences
  obj <- make_selected_object()
  obj$selection <- list(settings = list())

  ## Act
  output <- capture.output(expect_no_error(print(obj)))

  ## Assert
  expect_true(any(grepl("Selection", output)))
  expect_true(any(grepl("unknown", output)))

})


test_that("summary.horizons_data mirrors the selection block", {

  obj    <- make_selected_object()
  output <- capture.output(summary(obj))

  expect_true(any(grepl("Selection", output)))
  expect_true(any(grepl("Properties: clay, oc", output)))
  expect_true(any(grepl("Drawn per property: clay 12/12", output)))
  expect_true(any(grepl("Twins excluded: 0", output)))

})


test_that("summary.horizons_data pipeline status names the selection", {

  obj    <- make_selected_object()
  output <- capture.output(summary(obj))

  expect_true(any(grepl("drawn from a pool by select_training", output)))

})


test_that("summary.horizons_data reports rows removed since the draw", {

  ## Arrange — drop two rows, which routes through subset_rows()
  obj  <- make_selected_object()
  keep <- obj$data$analysis$sample_id[1:10]

  ## Act
  out    <- subset_rows(obj, keep, record = FALSE)
  output <- capture.output(summary(out))

  ## Assert
  expect_true(any(grepl("Rows removed since the draw: 2", output)))

})


test_that("summary.horizons_data has no selection block without a record", {

  obj    <- make_selected_object()
  obj$selection <- NULL
  output <- capture.output(summary(obj))

  expect_false(any(grepl("Selection", output)))
  expect_false(any(grepl("drawn from a pool", output)))

})
