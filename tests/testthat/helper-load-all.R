## ---------------------------------------------------------------------------
## Test helper: detect devtools::load_all()
## ---------------------------------------------------------------------------
## Shared by test-evaluate-parallel.R (parallel dispatch resolves workers by
## name in the INSTALLED horizons, so it refuses under load_all()) and
## test-pipeline-predict.R's fresh-process round trip (a callr child that
## pkgload::load_all()s the dev tree eagerly loads workflows/ranger/tune/
## xgboost/butcher regardless of the #65 fix, so it cannot independently
## re-trigger the missing-namespace defect there either). Both skip under
## load_all() and run only against an installed build: R CMD check, or
## devtools::test() after devtools::install().

skip_if_dev_package <- function() {
  testthat::skip_if(
    exists(".__DEVTOOLS__", envir = asNamespace("horizons"), inherits = FALSE),
    "this behavior is not exercised under load_all(); run via R CMD check or an installed build"
  )
}
