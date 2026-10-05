## ---------------------------------------------------------------------------
## Tests: step_select_cars()
## ---------------------------------------------------------------------------
##
## What the step refuses. Its selection, re-prep and round trip through
## build_recipe() are tested in test-utils-recipes.R. These aborts carry no
## package class, so each test matches the finding's own text.

## Helper: n spectra of p bands named wn_<wavenumber>, and an outcome
## carried by one band. Drawn under the caller's seed.
cars_data <- function(n = 30, p = 20) {

  m <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = p))

  d     <- tibble::as_tibble(m)
  d$SOC <- 2 * d$wn_3980 + stats::rnorm(n, sd = 0.5)

  d

}

## Helper: the step alone on the raw bands
cars_recipe <- function(d, outcome = "SOC") {

  recipes::recipe(SOC ~ ., data = d) |>
    step_select_cars(dplyr::starts_with("wn_"), outcome = outcome)

}

describe("step_select_cars() refuses input it cannot use", {

  it("aborts at prep when the outcome is not a single string", {

    ## Arrange
    withr::local_seed(1)
    d <- cars_data()

    ## Act & Assert
    expect_error(recipes::prep(cars_recipe(d, outcome = c("SOC", "clay"))),
                 "must be a single character string", fixed = TRUE)

  })

  it("aborts at prep when the outcome is not in the training data", {

    ## Arrange
    withr::local_seed(1)
    d <- cars_data()

    ## Act & Assert
    expect_error(recipes::prep(cars_recipe(d, outcome = "clay")),
                 "not found in training data", fixed = TRUE)

  })

  it("aborts at bake when the step has not been trained", {

    ## Arrange
    withr::local_seed(1)
    d   <- cars_data()
    rec <- cars_recipe(d)

    ## Act & Assert. bake() on the recipe refuses an untrained recipe before
    ## any step runs, so the step itself is baked.
    expect_error(recipes::bake(rec$steps[[1]], new_data = d),
                 "This step has not been trained yet", fixed = TRUE)

  })

  it("aborts at bake when new data lacks a wavenumber it selected", {

    skip_if_not_installed("pls")

    ## Arrange: recipes refuses new data missing an original predictor before
    ## any step bakes, so the step selects the transform step's output, and
    ## the transform is skipped at bake.
    withr::local_seed(1)
    d <- cars_data()

    rec <- recipes::recipe(SOC ~ ., data = d) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "raw",
                             skip = TRUE) |>
      step_select_cars(dplyr::matches("^spec[0-9]+$"), outcome = "SOC")

    ## CARS says, as a message, that it clusters fewer columns than it asks
    ## for.
    prepped  <- suppressMessages(recipes::prep(rec, training = d))
    selected <- prepped$steps[[2]]$selected_vars

    expect_gt(length(selected), 0L)
    expect_true(all(grepl("^spec[0-9]+$", selected)))

    ## Act & Assert
    expect_error(recipes::bake(prepped, new_data = d),
                 "Some selected wavenumbers are missing in new_data.", fixed = TRUE)

  })

})
