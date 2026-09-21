# tests/testthat/test-utils-subset.R
# Tests for subset_rows() and set_analysis() (issue #43)


## =============================================================================
## subset_rows() — Row selection
## =============================================================================

test_that("subset_rows() keeps the named ids in the order given", {

  ## Arrange
  fx   <- make_select_fixture(n_pool = 40)
  keep <- c("P010", "P003", "P025")

  ## Act
  out <- subset_rows(fx$pool, keep)

  ## Assert
  expect_s3_class(out, "horizons_data")
  expect_identical(out$data$analysis$sample_id, keep)
  expect_identical(out$data$n_rows, 3L)

})


test_that("subset_rows() accepts a logical keep of length n_rows", {

  fx   <- make_select_fixture(n_pool = 40)
  keep <- fx$pool$data$analysis$family == 1L

  out <- subset_rows(fx$pool, keep)

  expect_identical(out$data$n_rows, sum(keep))
  expect_true(all(out$data$analysis$family == 1L))

})


test_that("subset_rows() leaves role_map and column counts untouched", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P001", "P002"))

  expect_identical(out$data$role_map,     fx$pool$data$role_map)
  expect_identical(out$data$n_predictors, fx$pool$data$n_predictors)
  expect_identical(out$data$n_responses,  fx$pool$data$n_responses)
  expect_identical(names(out$data$analysis), names(fx$pool$data$analysis))

})


test_that("subset_rows() output passes structural validation", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P005", "P006", "P007"))

  expect_no_error(validate_horizons_data(out))

})


test_that("subset_rows() records provenance by default", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P001", "P002"), reason = "test")

  prov <- out$provenance$subset_rows
  expect_type(prov, "list")
  expect_length(prov, 1L)
  expect_identical(prov[[1]]$n_before, 40L)
  expect_identical(prov[[1]]$n_after,  2L)
  expect_identical(prov[[1]]$reason,   "test")
  expect_s3_class(prov[[1]]$applied_at, "POSIXct")

})


test_that("subset_rows() appends a second provenance entry on repeat", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, sprintf("P%03d", 1:10), reason = "first")
  out <- subset_rows(out,     sprintf("P%03d", 1:3),  reason = "second")

  expect_length(out$provenance$subset_rows, 2L)
  expect_identical(out$provenance$subset_rows[[2]]$n_before, 10L)
  expect_identical(out$provenance$subset_rows[[2]]$reason,   "second")

})


test_that("subset_rows(record = FALSE) writes no provenance", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P001", "P002"), record = FALSE)

  expect_null(out$provenance$subset_rows)

})


## =============================================================================
## subset_rows() — Input validation
## =============================================================================

test_that("subset_rows() rejects non-horizons_data input", {

  expect_error(subset_rows(data.frame(sample_id = "a"), "a"),
               class = "horizons_input_error")

})


test_that("subset_rows() aborts on unknown ids and names them", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, c("P001", "nope", "also_nope")),
               regexp = "nope", class = "horizons_input_error")

})


test_that("subset_rows() aborts on duplicated ids in keep", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, c("P001", "P001")),
               class = "horizons_input_error")

})


test_that("subset_rows() aborts on a logical keep of the wrong length", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, c(TRUE, FALSE)),
               class = "horizons_input_error")

})


test_that("subset_rows() aborts when keep selects no rows", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, character(0)),
               class = "horizons_input_error")

})


## =============================================================================
## set_analysis() — Replace the analysis table and recompute counts
## =============================================================================

test_that("set_analysis() recomputes every count from the role_map", {

  fx <- make_select_fixture(n_pool = 40)

  ## Drop the family column and the oc response
  analysis <- fx$pool$data$analysis
  analysis <- analysis[1:5, setdiff(names(analysis), c("family", "oc"))]
  role_map <- fx$pool$data$role_map
  role_map <- role_map[!role_map$variable %in% c("family", "oc"), ]

  out <- set_analysis(fx$pool, analysis, role_map)

  expect_identical(out$data$n_rows,       5L)
  expect_identical(out$data$n_responses,  1L)
  expect_identical(out$data$n_covariates, 0L)
  expect_identical(out$data$n_predictors, sum(role_map$role == "predictor"))
  expect_no_error(validate_horizons_data(out))

})


test_that("set_analysis() keeps the existing role_map when none is given", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- set_analysis(fx$pool, fx$pool$data$analysis[1:3, ])

  expect_identical(out$data$role_map, fx$pool$data$role_map)
  expect_identical(out$data$n_rows, 3L)

})


test_that("set_analysis() aborts when analysis columns and role_map disagree", {

  fx       <- make_select_fixture(n_pool = 40)
  analysis <- fx$pool$data$analysis
  analysis$extra <- 1

  expect_error(set_analysis(fx$pool, analysis),
               class = "horizons_input_error")

})
