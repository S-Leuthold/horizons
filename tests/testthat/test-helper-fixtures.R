## ---------------------------------------------------------------------------
## Tests: synthetic spectra with signal (helper-fixtures.R)
## ---------------------------------------------------------------------------
## make_signal_spectra() is what gives the models the suite fits something to
## find, so it is held to an outcome every preprocessing method keeps: put
## its signal back where a step trims it, or take away what SNV needs, and
## this fails.

describe("make_signal_spectra()", {

  ## The least-squares R² of the outcome on one method's columns at the
  ## default window, the columns a configuration's model is given
  kept_rsq <- function(d, preprocessing) {
    z <- transform_spectra_matrix(d$spectra, preprocessing, window_size = DEFAULT_SG_WINDOW)
    summary(stats::lm(d$outcome ~ z))$r.squared
  }

  it("carries its outcome through every preprocessing method at the fixtures' width and window", {

    for (n in c(40, 60, 200)) {

      d <- withr::with_seed(42, make_signal_spectra(n))

      for (method in VALID_PREPROCESSING) {
        expect_gt(kept_rsq(d, method), 0.6, label = sprintf("R² on %s's columns at %d rows", method, n))
      }

    }

  })

  it("gives an outcome inside the default outcome_range at every size the fixtures use", {

    for (n in c(40, 60, 200)) {

      y <- withr::with_seed(42, make_signal_spectra(n))$outcome

      expect_true(all(y > 0.6 & y < 3.4), label = sprintf("the outcome at %d rows within (0.6, 3.4)", n))
      expect_true(all(y >= DEFAULT_OUTCOME_RANGE[1] & y <= DEFAULT_OUTCOME_RANGE[2]))

    }

  })

  it("names its bands on the fixtures' wavenumber axis, one spectrum per outcome", {

    d <- withr::with_seed(1, make_signal_spectra(7, n_wn = 12))

    expect_identical(dim(d$spectra), c(7L, 12L))
    expect_identical(colnames(d$spectra), paste0("wn_", seq(4000, by = -2, length.out = 12)))
    expect_length(d$outcome, 7L)

  })

})
