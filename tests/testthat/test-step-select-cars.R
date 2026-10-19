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


## Helper: 40 noise bands, the outcome on two of them (wn_3980 and
## wn_3950), 60 rows, drawn at seed 1
cars_signal_data <- function() {

  withr::with_seed(1, {
    m <- matrix(stats::rnorm(60 * 40), 60)
    colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 40))
    d <- tibble::as_tibble(m)
    d$SOC <- 2 * d$wn_3980 - 2 * d$wn_3950 + stats::rnorm(60, sd = 0.3)
    d
  })

}

describe("step_select_cars() selects", {

  it("keeps both bands that carry the outcome and drops some of the rest", {

    skip_if_not_installed("pls")

    ## Over seeds 1 to 20 (the recipe built under the seed, as here) both
    ## signal bands survive at every seed, with 3 to 40 bands kept (median
    ## 21); at seed 1 the set is pinned below.
    d <- cars_signal_data()

    prepped  <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))
    selected <- prepped$steps[[1]]$selected_vars

    expect_true(all(c("wn_3980", "wn_3950") %in% selected))
    expect_lt(length(selected), 40L)
    expect_setequal(selected, paste0("wn_", c(3922, 3926, 3928, 3930, 3932, 3934, 3936, 3938,
                                              3944, 3946, 3948, 3950, 3956, 3958, 3960, 3964,
                                              3968, 3972, 3974, 3978, 3980, 3990, 3994, 3996,
                                              3998, 4000)))

  })

})


describe("step_select_cars() returns the set it scored (#106)", {

  it("returns the bands the lowest-scoring iteration's fit was given, scored at its best component count", {

    skip_if_not_installed("pls")

    d <- cars_signal_data()

    ## Each iteration's fit, and the bands it was given
    seen <- list()
    fits <- list()
    real <- pls::plsr
    local_mocked_bindings(plsr = function(formula, data, ...) {
      fit <- real(formula, data = data, ...)
      seen[[length(seen) + 1L]] <<- setdiff(names(data), "y")
      fits[[length(fits) + 1L]] <<- fit
      fit
    }, .package = "pls")

    step <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]
    rec  <- step$cars

    expect_length(seen, 50L)
    expect_identical(rec$iteration, which.min(rec$scores))

    ## Fewer than 150 bands, so each band is its own cluster and the bands
    ## kept are the representatives the chosen fit was given: not those of a
    ## rerun with new draws, nor the set left after that iteration's cut
    expect_setequal(step$selected_vars, seen[[rec$iteration]])
    expect_identical(rec$n_kept, length(step$selected_vars))

    ## Each score is its fit's cross-validated RMSEP at the best number of
    ## components, the intercept-only model left out
    best <- vapply(fits, function(f) {
      min(pls::RMSEP(f, estimate = "CV", intercept = FALSE)$val)
    }, numeric(1))
    expect_equal(rec$scores, best)

  })

  it("fails when PLS cannot score an iteration, rather than score it another way or keep every band (#105)", {

    skip_if_not_installed("pls")

    d <- cars_signal_data()

    local_mocked_bindings(RMSEP = function(...) stop("PLS could not score this fit"), .package = "pls")

    expect_error(withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d))),
                 "PLS could not score this fit", fixed = TRUE)

  })

  it("leaves the intercept-only model out of the score, which would win on an outcome no band carries", {

    skip_if_not_installed("pls")

    ## A noise outcome: at seed 1, the intercept-only model's RMSEP is the
    ## lowest at 8 of the 50 iterations
    d <- withr::with_seed(2, {
      m <- matrix(stats::rnorm(60 * 40), 60)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 40))
      d <- tibble::as_tibble(m)
      d$SOC <- stats::rnorm(60)
      d
    })

    fits <- list()
    real <- pls::plsr
    local_mocked_bindings(plsr = function(formula, data, ...) {
      fit <- real(formula, data = data, ...)
      fits[[length(fits) + 1L]] <<- fit
      fit
    }, .package = "pls")

    rec <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]$cars

    with_intercept <- vapply(fits, function(f) min(pls::RMSEP(f, estimate = "CV")$val), numeric(1))
    components     <- vapply(fits, function(f) min(pls::RMSEP(f, estimate = "CV", intercept = FALSE)$val), numeric(1))

    expect_true(any(with_intercept < components))
    expect_equal(rec$scores, components)

  })

})
