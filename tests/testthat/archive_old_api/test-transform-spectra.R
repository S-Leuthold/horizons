library(testthat)
library(horizons)
library(tibble)

test_that("process_spectra applies raw trimming and derivatives", {
  skip_if_not_installed("prospectr")

  wavelengths <- seq_len(15)

  raw_result <- process_spectra(wavelengths, preprocessing = "raw", window_size = 5)
  expect_equal(raw_result, wavelengths[3:13])

  sg_result <- process_spectra(wavelengths, preprocessing = "sg", window_size = 5)
  expect_equal(length(sg_result), length(wavelengths) - 4)

  snv_deriv_result <- process_spectra(wavelengths, preprocessing = "snv_deriv1", window_size = 7)
  expect_equal(length(snv_deriv_result), length(wavelengths) - 6)

  expect_error(process_spectra(wavelengths, preprocessing = "unknown"), "Unknown preprocessing")
})

test_that("step_transform_spectra prep and bake convert spectra columns", {
  skip_if_not_installed("prospectr")
  skip_if_not_installed("recipes")

  specta_tbl <- tibble(
    Sample_ID = c("S1", "S2", "S3"),
    Response  = c(10, 12, 11),
    `600`     = c(0.1, 0.2, 0.3),
    `602`     = c(0.4, 0.5, 0.6),
    `604`     = c(0.7, 0.8, 0.9),
    `606`     = c(1.0, 1.1, 1.2),
    `608`     = c(1.3, 1.4, 1.5)
  )

  rec <- recipes::recipe(Response ~ ., data = specta_tbl) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_transform_spectra(
      tidyselect::starts_with("6"),
      preprocessing = "snv",
      window_size   = 5
    )

  prepped <- recipes::prep(rec, training = specta_tbl, retain = TRUE)

  transformed <- recipes::bake(prepped, new_data = specta_tbl)

  spectral_cols <- setdiff(names(transformed), c("Sample_ID", "Response"))
  expect_true(length(spectral_cols) < 5)
  expect_true(all(sapply(transformed[, spectral_cols], is.numeric)))

  expect_true(all(transformed$Sample_ID == specta_tbl$Sample_ID))
})
