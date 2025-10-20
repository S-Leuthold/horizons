#' Tests for fetch_climate_covariates() helper
#'
#' Focus: behavior of caching logic and Daymet summarization rebuild using mocks.

library(testthat)
library(horizons)
library(dplyr)

## -----------------------------------------------------------------------------
## Helper: create minimal Daymet tibble for deterministic summaries
## -----------------------------------------------------------------------------

make_daymet_sample <- function(years = 2003:2004, lat = 0, seed = 123) {
  if (!is.null(seed)) set.seed(seed)

  expand.grid(
    year = years,
    yday = c(15, 150, 300)
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      tmin..deg.c. = 5 + runif(dplyr::n(), -1, 1),
      tmax..deg.c. = 15 + runif(dplyr::n(), -1, 1),
      prcp..mm.day. = runif(dplyr::n(), 0, 3)
    )
}

## -----------------------------------------------------------------------------
## Test: cached data path short-circuits download
## -----------------------------------------------------------------------------

test_that("fetch_climate_covariates reuses cached Daymet summaries", {
  skip_if_not_installed("qs")

  input <- tibble::tibble(
    Sample_ID = c("A", "B"),
    Longitude = c(0.02, 0.02),
    Latitude  = c(0.03, 0.03)
  )

  local_cache <- withr::local_tempdir()

  daymet_res <- horizons:::DAYMET_RESOLUTION_DEG

  grid_id <- paste0(
    round(input$Longitude[1] / daymet_res) * daymet_res,
    "_",
    round(input$Latitude[1] / daymet_res) * daymet_res
  )

  cache_file <- file.path(local_cache, paste0("daymet_", grid_id, "_2003_2004.qs"))
  file.create(cache_file)

  cached_result <- tibble::tibble(
    Daymet_GridID      = grid_id,
    MAT                = 12,
    MAP                = 900,
    PET                = 700,
    AI                 = 1.2,
    GDD                = 1500,
    Precip_Seasonality = 40
  )

  with_mocked_bindings(
    R_user_dir = function(...) local_cache,
    .package = "tools",
    with_mocked_bindings(
      qread = function(file, ...) {
        expect_equal(file, cache_file)
        cached_result
      },
      qsave = function(...) stop("Caching should not be triggered when file exists"),
      .package = "qs",
      with_mocked_bindings(
        download_daymet = function(...) stop("Download should be skipped when cache is present"),
        .package = "daymetr",
        with_mocked_bindings(
          cli_text = function(...) invisible(NULL),
          .package = "cli",
          result <- horizons::fetch_climate_covariates(
            input_data = input,
            start_year = 2003,
            end_year   = 2004,
            cache_dir  = local_cache,
            refresh    = FALSE
          )
        )
      )
    )
  )

  expect_equal(nrow(result), nrow(input))
  expect_true(all(c("MAT", "MAP", "PET", "AI", "GDD", "Precip_Seasonality") %in% names(result)))
  expect_true(all(!is.na(result$MAT)))
})

## -----------------------------------------------------------------------------
## Test: Daymet download path summarises data and caches output
## -----------------------------------------------------------------------------

test_that("fetch_climate_covariates downloads and summarizes Daymet data when cache missing", {
  skip_if_not_installed("qs")

  input <- tibble::tibble(
    Sample_ID = "C",
    Longitude = 0.04,
    Latitude  = 0.04
  )

  local_cache <- withr::local_tempdir()

  daymet_data <- make_daymet_sample()

  saved_payloads <- list()

  with_mocked_bindings(
    R_user_dir = function(...) local_cache,
    .package = "tools",
    with_mocked_bindings(
      qsave = function(object, file, ...) {
        saved_payloads[[file]] <<- object
        invisible(NULL)
      },
      qread = function(...) stop("No cache expected during download path"),
      .package = "qs",
      with_mocked_bindings(
        download_daymet = function(...) daymet_data,
        .package = "daymetr",
        with_mocked_bindings(
          cli_text = function(...) invisible(NULL),
          .package = "cli",
          result <- horizons::fetch_climate_covariates(
            input_data = input,
            start_year = 2003,
            end_year   = 2004,
            cache_dir  = local_cache,
            refresh    = TRUE
          )
        )
      )
    )
  )

  expect_equal(nrow(result), 1)
  expect_true(all(c("MAT", "MAP", "PET", "AI", "GDD", "Precip_Seasonality") %in% names(result)))
  expect_true(all(is.finite(result$MAT)))

  expect_true(length(saved_payloads) >= 1)
  stored_values <- saved_payloads[[1]]
  expect_true(all(c("MAT", "MAP", "PET", "AI", "GDD") %in% names(stored_values)))
})
