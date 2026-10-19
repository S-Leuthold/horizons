# tests/testthat/helper-selection-steps.R
# Mocks for the spectral selection steps (step_select_cars(),
# step_select_boruta(), step_select_correlation()) that more than one test
# file reads.


## ---------------------------------------------------------------------------
## mock_boruta_rejecting() — Boruta rejecting every attribute
## ---------------------------------------------------------------------------

#' Boruta, mocked to reject every attribute it is given
#'
#' @description
#' Stands in for `Boruta::Boruta()` through `local_mocked_bindings(.package =
#' "Boruta")`. With every attribute rejected, `step_select_boruta()` keeps
#' every band, records `retained_all`, and warns with class
#' `horizons_warning_substitution`.
#'
#' @param x [Matrix.] The attributes Boruta is given.
#' @param y The outcome; not read.
#' @param ... Not read.
#'
#' @return [List.] A `Boruta` object with only `finalDecision`.
#' @noRd
mock_boruta_rejecting <- function(x, y, ...) {

  decision <- factor(rep("Rejected", ncol(x)), levels = c("Tentative", "Confirmed", "Rejected"))
  names(decision) <- colnames(x)

  structure(list(finalDecision = decision), class = "Boruta")

}
