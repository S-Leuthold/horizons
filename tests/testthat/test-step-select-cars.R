## ---------------------------------------------------------------------------
## Tests: step_select_cars()
## ---------------------------------------------------------------------------
##
## What the step refuses, and that it keeps the bands carrying the outcome.
## Its re-prep and round trip through build_recipe() are tested in
## test-utils-recipes.R. Its refusals are horizons_error_input; raised in
## prep(), recipes wraps them in recipes_error_step, which expect_error()'s
## class matches through.

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
    err <- expect_error(recipes::prep(cars_recipe(d, outcome = c("SOC", "clay"))),
                        "`outcome` must be a single string", fixed = TRUE,
                        class = "horizons_error_input")
    expect_true(rlang::cnd_inherits(err, "horizons_error_input"))

  })

  it("aborts at prep when the outcome is not in the training data", {

    ## Arrange
    withr::local_seed(1)
    d <- cars_data()

    ## Act & Assert
    expect_error(recipes::prep(cars_recipe(d, outcome = "clay")),
                 "not found in training data", fixed = TRUE,
                 class = "horizons_error_input")
    expect_error(recipes::prep(cars_recipe(d, outcome = "clay")),
                 "Set `outcome` to the name of a column of the training data", fixed = TRUE)

  })

  it("aborts at bake when the step has not been trained", {

    ## Arrange
    withr::local_seed(1)
    d   <- cars_data()
    rec <- cars_recipe(d)

    ## Act & Assert. bake() on the recipe refuses an untrained recipe before
    ## any step runs, so the step itself is baked.
    expect_error(recipes::bake(rec$steps[[1]], new_data = d),
                 "This step has not been trained yet", fixed = TRUE,
                 class = "horizons_error_input")

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

    ## Act & Assert: horizons' class, and recipes' check, which names the
    ## missing columns, as the parent (#109)
    err <- expect_error(recipes::bake(prepped, new_data = d),
                        "`new_data` lacks columns `step_select_cars()` selected.",
                        fixed = TRUE, class = "horizons_error_input")
    expect_match(rlang::cnd_message(err), selected[1], fixed = TRUE)
    expect_match(rlang::cnd_message(err), "missing from `new_data`", fixed = TRUE)

  })

})


describe("step_select_cars() selects", {

  it("keeps both bands that carry the outcome and drops some of the rest", {

    skip_if_not_installed("pls")

    ## 40 noise bands, the outcome on two of them. The original keeps both
    ## at every one of 20 seeds, with 6 to 29 bands.
    d <- withr::with_seed(1, {
      m <- matrix(stats::rnorm(60 * 40), 60)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 40))
      d <- tibble::as_tibble(m)
      d$SOC <- 2 * d$wn_3980 - 2 * d$wn_3950 + stats::rnorm(60, sd = 0.3)
      d
    })

    prepped  <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))
    selected <- prepped$steps[[1]]$selected_vars

    expect_true(all(c("wn_3980", "wn_3950") %in% selected))
    expect_lt(length(selected), 40L)

  })

})
