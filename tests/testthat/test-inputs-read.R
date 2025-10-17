#' Tests for read_spectra() Function

library(testthat)
library(horizons)

test_that("read_spectra validates source parameter", {
  expect_error(
    read_spectra(source = "invalid", spectra_path = "dummy.csv"),
    "should be one of"
  )
})

test_that("read_spectra validates spectra_type parameter", {
  expect_error(
    read_spectra(source = "csv", spectra_path = "dummy.csv", spectra_type = "invalid"),
    "should be one of"
  )
})

test_that("read_spectra handles missing file", {
  expect_error(
    read_spectra(source = "csv", spectra_path = "nonexistent.csv"),
    "not found|does not exist"
  )
})

test_that("read_spectra requires spectra_path", {
  expect_error(
    read_spectra(source = "csv", spectra_path = NULL)
  )
})
