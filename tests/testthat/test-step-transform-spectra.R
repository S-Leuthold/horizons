## ---------------------------------------------------------------------------
## Tests: step_transform_spectra()
## ---------------------------------------------------------------------------
##
## What the step refuses. Its transforms, window rules, name collisions and
## unusable-spectrum warning are tested in test-utils-recipes.R. These aborts
## carry no package class, so each test matches the finding's own text.

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

    ## Act & Assert
    expect_error(recipes::prep(rec),
                 "All spectral columns must be numeric. Non-numeric: wn_3998",
                 fixed = TRUE)

  })

  it("names the method, the matrix and the window when preprocessing fails", {

    ## Arrange: the constructor does not check the method, so an unknown one
    ## first fails when prep() bakes the training data.
    withr::local_seed(1)
    d <- transform_data()

    rec <- recipes::recipe(SOC ~ ., data = d) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "nope")

    ## Act & Assert: the step's message, with the cause appended
    expect_error(
      recipes::prep(rec),
      "preprocessing 'nope' failed on a 20 x 30 spectral matrix (window_size = 9): Unknown preprocessing type: nope",
      fixed = TRUE
    )

  })

})
