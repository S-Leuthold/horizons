library(testthat)
library(horizons)

test_that(".onLoad respects thread control environment variable", {
  skip_if_not_installed("tidymodels")
  withr::local_envvar(list(
    HORIZONS_THREAD_CONTROL = "FALSE",
    OMP_NUM_THREADS = "",
    OPENBLAS_NUM_THREADS = "",
    MKL_NUM_THREADS = ""
  ))

  horizons:::.onLoad(NULL, "horizons")
  expect_equal(Sys.getenv("OMP_NUM_THREADS"), "")

  withr::local_envvar(list(HORIZONS_THREAD_CONTROL = "TRUE"))
  withr::local_options(
    ranger.num.threads = NULL,
    xgboost.nthread    = NULL
  )

  horizons:::.onLoad(NULL, "horizons")
  expect_equal(Sys.getenv("OMP_NUM_THREADS"), "1")
  expect_equal(Sys.getenv("OPENBLAS_NUM_THREADS"), "1")
  expect_equal(getOption("ranger.num.threads"), 1)
})

test_that(".onAttach emits startup message", {
  message_store <- character()
  withCallingHandlers(
    horizons:::.onAttach(NULL, "horizons"),
    packageStartupMessage = function(m) {
      message_store <<- c(message_store, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("horizons v", message_store)))
})
