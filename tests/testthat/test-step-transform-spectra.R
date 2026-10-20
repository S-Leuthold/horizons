## ---------------------------------------------------------------------------
## Tests: step_transform_spectra()
## ---------------------------------------------------------------------------
##
## What the step refuses. Its transforms, window rules, name collisions and
## unusable-spectrum warning are tested in test-utils-recipes.R. Each test
## asserts the class and matches the finding's own text.

## Helper: n spectra of p bands named wn_<wavenumber>, and an outcome.
## Drawn under the caller's seed.
transform_data <- function(n = 20, p = 30) {

  m <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = p))

  d     <- tibble::as_tibble(m)
  d$SOC <- stats::runif(n, 0.5, 10)

  d

}

describe("step_transform_spectra() refuses input it cannot use", {

  it("aborts at prep, naming the column, when a spectral column is not numeric", {

    ## Arrange: one band read in as text
    withr::local_seed(1)
    d         <- transform_data()
    d$wn_3998 <- as.character(d$wn_3998)

    rec <- recipes::recipe(SOC ~ ., data = d) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "raw")

    ## Act & Assert: wrapped by recipes, with the class in its chain
    err <- expect_error(recipes::prep(rec), "All spectral columns must be numeric",
                        fixed = TRUE, class = "horizons_error_input")
    expect_match(conditionMessage(err$parent), "Non-numeric: wn_3998", fixed = TRUE)

  })

  it("refuses an unknown method when the step is added, listing the methods (#199)", {

    withr::local_seed(1)
    d <- transform_data()

    err <- expect_error(
      recipes::recipe(SOC ~ ., data = d) |>
        step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "nope"),
      "`preprocessing` must be one of", fixed = TRUE, class = "horizons_error_input"
    )
    expect_match(conditionMessage(err), '"snv_deriv2"', fixed = TRUE)
    expect_match(conditionMessage(err), 'Got "nope"', fixed = TRUE)

  })

  it("names the method, the matrix and the window when preprocessing fails (#192)", {

    ## Arrange: the method is checked when the step is added, so the failure
    ## is the transform's own, made here at its seam
    withr::local_seed(1)
    d <- transform_data()

    rec <- recipes::recipe(SOC ~ ., data = d) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "deriv1")

    local_mocked_bindings(transform_spectra_matrix = function(...) stop("convolution failed"))

    ## Act & Assert: a run error, the cause its parent
    err <- expect_error(recipes::prep(rec), class = "horizons_error_run")
    run <- err$parent
    msg <- conditionMessage(run)
    expect_match(msg, "could not transform its spectra", fixed = TRUE)
    expect_match(msg, 'Method: "deriv1"', fixed = TRUE)
    expect_match(msg, "Spectral matrix: 20 x 30", fixed = TRUE)
    expect_match(msg, "Window size: 9", fixed = TRUE)
    expect_match(conditionMessage(run$parent), "convolution failed", fixed = TRUE)

  })

})
