## ---------------------------------------------------------------------------
## Tests: step_select_correlation()
## ---------------------------------------------------------------------------
##
## Which windows the step keeps, and what it refuses. Its re-prep and round
## trip through build_recipe() are tested in test-utils-recipes.R. The aborts
## carry no package class, so each test matches the finding's own text.

## Helper: n spectra of p bands named wn_<wavenumber>, and an outcome
## carried by one band. Drawn under the caller's seed.
correlation_data <- function(n = 30, p = 20, signal = "wn_3980") {

  m <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = p))

  d     <- tibble::as_tibble(m)
  d$SOC <- 2 * d[[signal]] + stats::rnorm(n, sd = 0.5)

  d

}

## Helper: the step alone on the raw bands
correlation_recipe <- function(d, outcome = "SOC") {

  recipes::recipe(SOC ~ ., data = d) |>
    step_select_correlation(dplyr::starts_with("wn_"), outcome = outcome)

}

describe("step_select_correlation() keeps the windows that score", {

  ## The outcome follows wn_3980, so the three 3-band windows holding it,
  ## wn_3984 to wn_3976, score high. At n = 100 the selection is exactly
  ## those five bands at 100 of 100 seeds, with or without a constant band
  ## at wn_4000. At seed 1 the lowest of the three windows sits 0.106 above
  ## the threshold (0.233) and the best noise window 0.119 below it.
  signal_windows <- paste0("wn_", seq(3984, 3976, by = -2))

  it("keeps the bands of each window whose mean absolute correlation clears the mean plus one sd", {

    withr::local_seed(1)
    d <- correlation_data(n = 100)

    step <- recipes::prep(correlation_recipe(d), training = d)$steps[[1]]

    expect_identical(step$selected_vars, signal_windows)

  })

  it("scores the windows around a constant band without letting it void the threshold", {

    withr::local_seed(1)
    d         <- correlation_data(n = 100)
    d$wn_4000 <- 0.5

    ## A constant band has no correlation, and stats::cor() says so
    expect_warning(prepped <- recipes::prep(correlation_recipe(d), training = d),
                   "the standard deviation is zero", fixed = TRUE)

    expect_identical(prepped$steps[[1]]$selected_vars, signal_windows)

  })

  it("keeps every band, and says so, when no window clears the threshold", {

    ## Two windows: the mean plus one sd of two unequal scores lies above
    ## the higher of them
    withr::local_seed(1)
    d <- correlation_data(p = 4, signal = "wn_3996")

    expect_message(prepped <- recipes::prep(correlation_recipe(d), training = d),
                   "No windows met the threshold. Retaining all predictors.", fixed = TRUE)

    ## Kept whole, the selection carries the names recipes gave the columns
    expect_identical(unname(prepped$steps[[1]]$selected_vars),
                     c("wn_4000", "wn_3998", "wn_3996", "wn_3994"))

  })

})

describe("step_select_correlation() refuses input it cannot use", {

  it("aborts at prep when the outcome is not one column of the training data", {

    ## Arrange
    withr::local_seed(1)
    d <- correlation_data()

    ## Act & Assert
    expect_error(recipes::prep(correlation_recipe(d, outcome = c("SOC", "clay"))),
                 "must be a single character string", fixed = TRUE)
    expect_error(recipes::prep(correlation_recipe(d, outcome = "clay")),
                 "not found in training data", fixed = TRUE)

  })

  it("aborts at bake when the step has not been trained", {

    ## Arrange
    withr::local_seed(1)
    d   <- correlation_data()
    rec <- correlation_recipe(d)

    ## Act & Assert. bake() on the recipe refuses an untrained recipe before
    ## any step runs, so the step itself is baked.
    expect_error(recipes::bake(rec$steps[[1]], new_data = d),
                 "This step has not been trained yet", fixed = TRUE)

  })

  it("aborts at bake when new data lacks a wavenumber it selected", {

    ## Arrange: recipes refuses new data missing an original predictor before
    ## any step bakes, so the step selects the transform step's output, and
    ## the transform is skipped at bake. The step used to keep whichever
    ## selected columns were present and return the rest of the data as is.
    withr::local_seed(1)
    d <- correlation_data()

    rec <- recipes::recipe(SOC ~ ., data = d) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "raw",
                             skip = TRUE) |>
      step_select_correlation(dplyr::matches("^spec[0-9]+$"), outcome = "SOC")

    prepped  <- recipes::prep(rec, training = d)
    selected <- prepped$steps[[2]]$selected_vars

    expect_gt(length(selected), 0L)
    expect_true(all(grepl("^spec[0-9]+$", selected)))

    ## Act & Assert
    expect_error(recipes::bake(prepped, new_data = d),
                 "Some selected wavenumbers are missing in new_data.", fixed = TRUE)

  })

})
