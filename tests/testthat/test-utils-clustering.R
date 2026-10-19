## ---------------------------------------------------------------------------
## Tests: cluster_spectral_predictors()
## ---------------------------------------------------------------------------
##
## How the bands are clustered and represented. The CARS and Boruta steps
## call it on their selected columns at prep.

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


describe("cluster_spectral_predictors() represents each cluster by its middle band", {

  it("takes the middle band by position under names that are not numbers, as the steps see them", {

    ## The same two families, named as step_transform_spectra() names its
    ## output. Read as numbers these are NA, which made the first band of
    ## each cluster its representative.
    m <- band_families()
    colnames(m) <- sprintf("spec%03d", 1:6)

    res <- cluster_spectral_predictors(m, k = 2)

    expect_identical(unname(res$selected_vars), c("spec002", "spec005"))
    expect_equal(unname(res$cluster_map), list(sprintf("spec%03d", 1:3), sprintf("spec%03d", 4:6)))

  })

  it("takes the first of the two middle bands of a cluster with an even count", {

    m <- band_families()[, c(1:3, 4:6)]
    m <- cbind(m[, 1:3], m[, 3] + withr::with_seed(3, stats::rnorm(30, sd = 0.05)), m[, 4:6])
    colnames(m) <- sprintf("spec%03d", 1:7)

    res <- cluster_spectral_predictors(m, k = 2)

    expect_identical(unname(res$selected_vars), c("spec002", "spec006"))

  })

})
