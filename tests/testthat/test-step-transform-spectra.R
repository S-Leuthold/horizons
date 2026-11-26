## ---------------------------------------------------------------------------
## Tests: step_transform_spectra()
## ---------------------------------------------------------------------------
##
## What the step refuses, and the storage type of what it bakes. Its
## transforms, window rules, name collisions and unusable-spectrum warning are
## tested in test-utils-recipes.R. Each refusal test asserts the class and
## matches the finding's own text.

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

describe("step_transform_spectra() bakes doubles", {

  it("returns double spectra from integer columns under every method, raw included", {

    ## Arrange: integer counts, varied enough that no spectrum is flat
    withr::local_seed(1)
    d <- transform_data()
    for (col in grep("^wn_", names(d), value = TRUE)) d[[col]] <- sample.int(1000L, nrow(d))

    for (method in c("raw", "snv", "deriv1")) {

      rec <- recipes::recipe(SOC ~ ., data = d) |>
        step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = method, window_size = 5)

      ## Act
      baked <- recipes::bake(recipes::prep(rec), new_data = NULL)
      spec  <- baked[, grep("^spec", names(baked)), drop = FALSE]

      ## Assert: the step ran, trimming two bands from each edge
      expect_identical(ncol(spec), 26L, info = method)
      expect_true(all(vapply(spec, is.double, logical(1))), info = method)

    }

    ## And the raw method keeps the values it was given
    raw <- recipes::bake(recipes::prep(
      recipes::recipe(SOC ~ ., data = d) |>
        step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "raw", window_size = 5)
    ), new_data = NULL)
    expect_equal(raw$spec01, as.double(d$wn_3996))

  })

})

describe("hoist_spectra()", {

  it("is the step's bake of the whole training part for every preprocessing: names, order, values", {

    ## Arrange: an id, a meta column and a sibling response around the
    ## spectra, so the non-spectral columns' place in the frame shows
    withr::local_seed(1)
    d           <- transform_data()
    d$sample_id <- sprintf("S%02d", seq_len(nrow(d)))
    d$batch     <- rep(c("a", "b"), length.out = nrow(d))
    d$clay      <- stats::runif(nrow(d))
    role_map <- tibble::tibble(
      variable = c(grep("^wn_", names(d), value = TRUE), "SOC", "sample_id", "batch", "clay"),
      role     = c(rep("predictor", 30), "outcome", "id", "meta", "response")
    )

    for (method in VALID_PREPROCESSING) {

      config <- tibble::tibble(transformation = "none", preprocessing = method,
                               feature_selection = "none")

      ## The whole recipe with nothing but the transform step
      whole <- recipes::bake(recipes::prep(build_recipe(config, d, role_map)), new_data = NULL)

      ## Act & Assert
      expect_identical(hoist_spectra(d, config, role_map), whole, label = method)

    }

  })

  it("is the step's bake on an axis with masked columns too, which it leaves out", {

    withr::local_seed(1)
    d           <- transform_data()
    d$sample_id <- sprintf("S%02d", seq_len(nrow(d)))
    role_map <- tibble::tibble(
      variable = c(grep("^wn_", names(d), value = TRUE), "SOC", "sample_id"),
      role     = c(rep("predictor", 12), rep("masked", 6), rep("predictor", 12), "outcome", "id")
    )

    for (method in VALID_PREPROCESSING) {

      config <- tibble::tibble(transformation = "none", preprocessing = method,
                               feature_selection = "none")

      whole   <- recipes::bake(recipes::prep(build_recipe(config, d, role_map)), new_data = NULL)
      hoisted <- hoist_spectra(d, config, role_map)

      ## 30 columns less 8 trimmed and the 6 masked
      expect_identical(hoisted, whole, label = method)
      expect_identical(sum(grepl("^spec", names(hoisted))), 16L, label = method)
      expect_false(any(grepl("^wn_", names(hoisted))), label = method)

    }

  })

  it("leaves the caller's stream as it was", {

    withr::local_seed(2)
    d           <- transform_data()
    d$sample_id <- sprintf("S%02d", seq_len(nrow(d)))
    role_map <- tibble::tibble(variable = c(grep("^wn_", names(d), value = TRUE), "SOC", "sample_id"),
                               role     = c(rep("predictor", 30), "outcome", "id"))
    config   <- tibble::tibble(transformation = "none", preprocessing = "snv",
                               feature_selection = "none")

    before <- .Random.seed
    hoist_spectra(d, config, role_map)

    expect_identical(.Random.seed, before)

  })

})
