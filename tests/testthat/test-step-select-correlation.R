## ---------------------------------------------------------------------------
## Tests: step_select_correlation()
## ---------------------------------------------------------------------------
##
## Which windows the step keeps, and what it refuses. Its re-prep and round
## trip through build_recipe() are tested in test-utils-recipes.R. Its
## refusals are horizons_error_input; raised in prep(), recipes wraps them in
## recipes_error_step, which expect_error()'s class matches through.

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
    expect_false(step$correlation$retained_all)
    expect_false(step$correlation$skipped)

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

    ## A substitution, said once and recorded on the trained step (#105)
    w <- expect_warning(prepped <- recipes::prep(correlation_recipe(d), training = d),
                        "No window cleared `step_select_correlation()`'s threshold, so it selected nothing.",
                        fixed = TRUE, class = "horizons_warning_substitution")

    expect_identical(w$step, "step_select_correlation")
    expect_match(conditionMessage(w), "Retaining all 4 wavenumbers", fixed = TRUE)
    expect_true(prepped$steps[[1]]$correlation$retained_all)
    expect_identical(prepped$steps[[1]]$correlation$n_windows_kept, 0L)

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
                 "`outcome` must be a single string", fixed = TRUE,
                 class = "horizons_error_input")
    expect_error(recipes::prep(correlation_recipe(d, outcome = "clay")),
                 "not found in training data", fixed = TRUE,
                 class = "horizons_error_input")

  })

  it("aborts at bake when the step has not been trained", {

    ## Arrange
    withr::local_seed(1)
    d   <- correlation_data()
    rec <- correlation_recipe(d)

    ## Act & Assert. bake() on the recipe refuses an untrained recipe before
    ## any step runs, so the step itself is baked.
    expect_error(recipes::bake(rec$steps[[1]], new_data = d),
                 "This step has not been trained yet", fixed = TRUE,
                 class = "horizons_error_input")

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

    ## Act & Assert: the missing columns named, through recipes' check (#109)
    err <- expect_error(recipes::bake(prepped, new_data = d),
                        "`new_data` lacks columns `step_select_correlation()` selected.",
                        fixed = TRUE, class = "horizons_error_input")
    expect_match(rlang::cnd_message(err), selected[1], fixed = TRUE)

  })

})


describe("step_select_correlation() with too few bands to select from", {

  it("keeps one or two bands, scores no window, and warns skipped, with no warning from base R", {

    for (p in 1:2) {

      d <- withr::with_seed(1, {
        m <- matrix(stats::rnorm(30 * p), 30)
        colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = p))
        d <- tibble::as_tibble(m)
        d$SOC <- d$wn_4000 + stats::rnorm(30, sd = 0.3)
        d
      })

      w <- testthat::capture_warnings(
        prepped <- recipes::prep(correlation_recipe(d), training = d)
      )

      step <- prepped$steps[[1]]

      expect_length(w, 1L)
      expect_identical(unname(step$selected_vars), colnames(d)[seq_len(p)], label = paste(p, "bands"))
      expect_true(step$correlation$skipped)
      expect_warning(recipes::prep(correlation_recipe(d), training = d),
                     "too few to score a three-band window", fixed = TRUE,
                     class = "horizons_warning_skipped")

    }

  })

})
