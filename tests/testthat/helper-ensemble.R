## ---------------------------------------------------------------------------
## Test helper: the stored ensemble fixture and the ensembles built from it
## ---------------------------------------------------------------------------
## Shared by test-pipeline-ensemble.R and test-ensemble-uq.R. The fixture is
## fixtures/ensemble_fit.rds, a small horizons_fit on real (anonymized) MIR
## spectra; see fixtures/README.md.
##
## Every accessor here goes through helper-memo.R, so each object is read or
## built once per process, on first use inside a test, and never at file
## level: a build that fails (the lazy-quosure regression, say) fails the
## tests that use that build, by name, rather than the whole file. The rules
## in helper-memo.R apply: call the accessors inside it(), after any skip and
## before any mock, and treat what they return as read-only (edit a copy).


## ---------------------------------------------------------------------------
## The fitted object
## ---------------------------------------------------------------------------

#' The stored fitted object, read and validated once
#'
#' @return `horizons_fit`. The fixture, after validate_horizons_fit().
#' @noRd
ens_fitted <- function() memo_fixture("ensemble_fit.rds", build_ens_fitted)

build_ens_fitted <- function() {

  ### A fixture that no longer meets the fit contract fails every user here,
  ### naming the broken field, rather than somewhere inside ensemble().
  validate_horizons_fit(readRDS(testthat::test_path("fixtures", "ensemble_fit.rds")))

}

#' The fixture's held-out test set (Split F's assessment rows)
#'
#' @return Tibble. Cheap to take, so not memoised.
#' @noRd
ens_test_set <- function() rsample::assessment(ens_fitted()$models$split)


## ---------------------------------------------------------------------------
## Ensembles built from it
## ---------------------------------------------------------------------------

#' An ensemble of the fixture, built once per argument set
#'
#' @description
#' `ensemble(fitted, method, optimize, compute_uq, verbose = FALSE)` at the
#' default seed. The four the files use most, all at `optimize = FALSE`:
#' weighted (W), weighted without UQ (W0), penalized (P) and xgb (X).
#'
#' @param method [Character.] `"weighted"`, `"penalized"` or `"xgb"`.
#' @param compute_uq [Logical.] Default: `TRUE`, as ensemble().
#' @param optimize [Logical.] Default: `FALSE`.
#'
#' @return `horizons_ensemble`. Read-only; edit a copy.
#' @noRd
ens_built <- function(method, compute_uq = TRUE, optimize = FALSE) {

  memo_fixture(
    sprintf("ensemble(method = %s, optimize = %s, compute_uq = %s)",
            method, optimize, compute_uq),
    build_ens,
    method = method, compute_uq = compute_uq, optimize = optimize
  )

}

build_ens <- function(method, compute_uq, optimize) {

  ### suppressWarnings(): rsample's note that the 43 meta rows are too few
  ### for its default strata breaks, which no test here is about.
  suppressWarnings(
    ensemble(ens_fitted(), method = method, optimize = optimize,
             compute_uq = compute_uq, verbose = FALSE)
  )

}
