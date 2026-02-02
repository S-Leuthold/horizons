#' inputs-helpers CLI path verification

library(testthat)
library(horizons)

test_that("read_csv_internal warns for non-numeric spectral column names", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("Sample_ID,BandA,BandB\nS001,0.1,0.2", con = tmp)

  expect_message(
    horizons:::read_csv_internal(tmp, spectra_type = "MIR", verbose = TRUE),
    regexp = "not numeric",
    fixed = FALSE
  )

  unlink(tmp)
})

