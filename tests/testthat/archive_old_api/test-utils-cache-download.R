#' Tests for cache and download utilities

library(testthat)
library(horizons)

## -----------------------------------------------------------------------------
## get_ossl_data_path()
## -----------------------------------------------------------------------------

test_that("get_ossl_data_path returns cached file path", {
  cache_dir <- withr::local_tempdir()
  lab_file <- file.path(cache_dir, "ossl_lab_data.qs")
  writeLines("cached", lab_file)

  result <- with_mocked_bindings(
    R_user_dir = function(...) cache_dir,
    .package = "tools",
    horizons:::get_ossl_data_path("lab")
  )

  expect_equal(result, lab_file)
})

test_that("get_ossl_data_path returns NULL when file missing", {
  cache_dir <- withr::local_tempdir()

  result <- with_mocked_bindings(
    R_user_dir = function(...) cache_dir,
    .package = "tools",
    horizons:::get_ossl_data_path("mir")
  )

  expect_null(result)
})

## -----------------------------------------------------------------------------
## download_horizons_data()
## -----------------------------------------------------------------------------

stub_cli_silence <- function(expr) {
  with_mocked_bindings(
    cli_alert_success = function(...) invisible(NULL),
    cli_alert_info    = function(...) invisible(NULL),
    cli_progress_step = function(...) invisible(NULL),
    .package = "cli",
    expr
  )
}

test_that("download_horizons_data reuses cache when files exist", {
  cache_dir <- withr::local_tempdir()
  location_file <- file.path(cache_dir, "ossl_location_data.qs")
  lab_file      <- file.path(cache_dir, "ossl_lab_data.qs")
  mir_file      <- file.path(cache_dir, "ossl_mir_raw.qs")

  writeLines("x", location_file)
  writeLines("y", lab_file)
  writeLines("z", mir_file)

  result <- with_mocked_bindings(
    R_user_dir = function(...) cache_dir,
    .package = "tools",
    stub_cli_silence(
      with_mocked_bindings(
        qread_url = function(...) stop("Download not expected"),
        qsave     = function(...) stop("Saving not expected"),
        .package  = "qs",
        horizons::download_horizons_data(force = FALSE, ask = FALSE)
      )
    )
  )

  expect_type(result, "list")
  expect_equal(result$location, location_file)
  expect_equal(result$lab, lab_file)
  expect_equal(result$mir, mir_file)
})

test_that("download_horizons_data fetches files when cache missing", {
  cache_dir <- withr::local_tempdir()

  saved_objects <- list()

  result <- with_mocked_bindings(
    R_user_dir = function(...) cache_dir,
    .package = "tools",
    stub_cli_silence(
      with_mocked_bindings(
        qread_url = function(url, ...) tibble::tibble(source = basename(url), value = 1),
        qsave = function(object, file, ...) {
          saved_objects[[basename(file)]] <<- object
          saveRDS(object, file)
          invisible(NULL)
        },
        .package = "qs",
        horizons::download_horizons_data(force = TRUE, ask = FALSE)
      )
    )
  )

  expect_type(result, "list")
  expect_true(file.exists(result$location))
  expect_true(file.exists(result$lab))
  expect_true(file.exists(result$mir))
  expect_equal(names(saved_objects), c("ossl_location_data.qs", "ossl_lab_data.qs", "ossl_mir_raw.qs"))
})

## -----------------------------------------------------------------------------
## get_processed_mir_path()
## -----------------------------------------------------------------------------

test_that("get_processed_mir_path returns cached processed MIR file", {
  cache_dir <- withr::local_tempdir()
  processed_file <- file.path(cache_dir, "ossl_mir_processed.qs")
  writeLines("spectra", processed_file)

  result <- with_mocked_bindings(
    horizons::get_processed_mir_path(),
    R_user_dir = function(...) cache_dir,
    .package = "tools"
  )

  expect_equal(result, processed_file)
})

test_that("get_processed_mir_path returns NULL when cache missing", {
  cache_dir <- withr::local_tempdir()

  result <- with_mocked_bindings(
    horizons::get_processed_mir_path(),
    R_user_dir = function(...) cache_dir,
    .package = "tools"
  )

  expect_null(result)
})
