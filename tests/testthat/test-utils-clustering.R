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

    ## Act & Assert: the finding, and the remedy
    err <- expect_error(cluster_spectral_predictors(spectra, k = 2, method = "correlation"),
                        "Correlation matrix contains NA values", fixed = TRUE)
    expect_match(conditionMessage(err), "Remove zero-variance predictors", fixed = TRUE)

  })

})


## Two families of three bands named by wavenumber: 4000 to 3996 follow one
## signal, 2000 to 1996 another
band_families <- function() {

  m <- withr::with_seed(1, {
    a <- stats::rnorm(30)
    b <- stats::rnorm(30)
    cbind(sapply(1:3, function(i) a + stats::rnorm(30, sd = 0.05)),
          sapply(1:3, function(i) b + stats::rnorm(30, sd = 0.05)))
  })
  colnames(m) <- c("4000", "3998", "3996", "2000", "1998", "1996")

  m

}

describe("cluster_spectral_predictors() clusters", {

  it("groups each family and represents it by its median band, under every method", {

    m <- band_families()

    for (method in list(NULL, "correlation", "euclidean")) {

      res <- if (is.null(method)) {
        cluster_spectral_predictors(m, k = 2)
      } else {
        cluster_spectral_predictors(m, k = 2, method = method)
      }

      info <- if (is.null(method)) "default" else method

      expect_equal(unname(res$cluster_map), list(c("4000", "3998", "3996"), c("2000", "1998", "1996")),
                   info = info)
      expect_identical(unname(res$selected_vars), c("3998", "1998"), info = info)
      expect_identical(names(res$cluster_map), unname(res$selected_vars), info = info)
      expect_identical(res$reduced_mat, as.data.frame(m)[, c("3998", "1998")], info = info)

    }

    one <- cluster_spectral_predictors(m, k = 1)
    expect_s3_class(one$reduced_mat, "data.frame")
    expect_identical(names(one$reduced_mat), unname(one$selected_vars))

  })

  it("returns every band, each its own cluster, when k is at least the band count", {

    m   <- band_families()
    res <- suppressMessages(cluster_spectral_predictors(m, k = 10))

    expect_identical(res$selected_vars, colnames(m))
    expect_identical(names(res$cluster_map), colnames(m))
    expect_identical(unname(unlist(res$cluster_map)), colnames(m))

  })

})
