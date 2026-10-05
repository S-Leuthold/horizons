## ---------------------------------------------------------------------------
## Tests: cluster_spectral_predictors()
## ---------------------------------------------------------------------------
##
## What the clustering refuses. The CARS and Boruta steps call it on their
## selected columns at prep. These aborts carry no package class, so each
## test matches the finding's own text.

## Helper: n spectra of p bands named w1 ... wp. Drawn under the caller's
## seed.
cluster_data <- function(n = 20, p = 4) {

  m <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(m) <- paste0("w", seq_len(p))

  m

}

describe("cluster_spectral_predictors() refuses input it cannot cluster", {

  it("aborts when a column is not numeric", {

    ## Arrange
    withr::local_seed(1)
    spectra    <- as.data.frame(cluster_data())
    spectra$w2 <- as.character(spectra$w2)

    ## Act & Assert
    expect_error(cluster_spectral_predictors(spectra, k = 2),
                 "All columns in `spectra_mat` must be numeric.", fixed = TRUE)

  })

  it("aborts when k is not a positive whole number", {

    ## Arrange
    withr::local_seed(1)
    spectra <- cluster_data()

    ## Act & Assert
    for (bad in list(0, -1, 2.5, NA_real_, "2", c(1, 2))) {

      expect_error(cluster_spectral_predictors(spectra, k = bad),
                   "must be a positive integer.", fixed = TRUE,
                   info = paste("k =", deparse(bad)))

    }

  })

  it("aborts when the correlation matrix has NA values", {

    ## Arrange: a band with no observed values has no correlation with any
    ## other
    withr::local_seed(1)
    spectra      <- cluster_data()
    spectra[, 2] <- NA_real_

    ## Act & Assert
    expect_error(cluster_spectral_predictors(spectra, k = 2, method = "correlation"),
                 "Correlation matrix contains NA values", fixed = TRUE)

  })

})
