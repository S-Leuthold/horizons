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

  it("scores the windows around a constant band without letting it void the threshold, and drops it as CARS and Boruta do", {

    withr::local_seed(1)
    d         <- correlation_data(n = 100)
    d$wn_4000 <- 0.5

    ## A constant band has no correlation; it is dropped before scoring, so
    ## stats::cor() has nothing to warn of
    expect_no_warning(prepped <- recipes::prep(correlation_recipe(d), training = d))

    expect_identical(prepped$steps[[1]]$selected_vars, signal_windows)
    expect_identical(prepped$steps[[1]]$correlation$n_constant, 1L)
    expect_output(print(prepped$steps[[1]]), "1 near-constant band dropped before selection",
                  fixed = TRUE)

  })

  it("keeps every band, and says so, when no window clears the threshold", {

    ## Six bands, four windows: a noise band, then five copies of the band
    ## the outcome follows. The windows score s, r, r, r with s below r, so
    ## the threshold, their mean plus one sd, is r + (r - s) / 4, above all
    d <- withr::with_seed(1, {
      signal <- stats::rnorm(30)
      m      <- cbind(stats::rnorm(30), matrix(signal, 30, 5))
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 6))
      d     <- tibble::as_tibble(m)
      d$SOC <- 2 * signal + stats::rnorm(30, sd = 0.5)
      d
    })

    ## A substitution, said once and recorded on the trained step (#105)
    w <- expect_warning(prepped <- recipes::prep(correlation_recipe(d), training = d),
                        "No window cleared `step_select_correlation()`'s threshold, so it selected nothing.",
                        fixed = TRUE, class = "horizons_warning_substitution")

    expect_identical(w$step, "step_select_correlation")
    expect_match(conditionMessage(w), "Retaining all 6 wavenumbers", fixed = TRUE)
    expect_true(prepped$steps[[1]]$correlation$retained_all)
    expect_identical(prepped$steps[[1]]$correlation$n_windows, 4L)
    expect_identical(prepped$steps[[1]]$correlation$n_windows_kept, 0L)

    ## Kept whole, the selection carries the names recipes gave the columns
    expect_identical(unname(prepped$steps[[1]]$selected_vars), colnames(d)[1:6])

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

  it("keeps up to four bands, too few for the three windows its threshold needs, and warns skipped, with no warning from base R", {

    ## With three or four bands the windows can be scored, but one window has
    ## no sd and two always fall below their mean plus one sd, so the step
    ## would report a substitution it could not have avoided
    for (p in 1:4) {

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
      expect_identical(step$correlation$n_windows, 0L, label = paste(p, "bands"))
      expect_warning(recipes::prep(correlation_recipe(d), training = d),
                     "fewer than the 5 it needs", fixed = TRUE,
                     class = "horizons_warning_skipped")
      expect_output(print(step), sprintf("`step_select_correlation()` had %d band", p), fixed = TRUE)

    }

  })

})


describe("step_select_correlation()'s tidy() and print() (#109)", {

  it("tidies to its selectors before prep and the columns it removed after, and prints one line per fact", {

    withr::local_seed(1)
    d   <- correlation_data(n = 100)
    rec <- correlation_recipe(d)

    expect_identical(recipes::tidy(rec, number = 1)$terms, "dplyr::starts_with(\"wn_\")")

    prepped <- recipes::prep(rec, training = d)
    kept    <- prepped$steps[[1]]$selected_vars

    expect_setequal(recipes::tidy(prepped, number = 1)$terms,
                    setdiff(grep("^wn_", names(d), value = TRUE), kept))

    lines <- utils::capture.output(print(prepped$steps[[1]]))

    expect_length(lines, 3L)
    expect_identical(lines[2], "\u2022 Outcome column: SOC")

  })

})
