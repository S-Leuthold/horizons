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
    ## signal bands survive at every seed, with 2 to 8 bands kept (median 3);
    ## at seed 1 the set is pinned below.
    d <- cars_signal_data()

    prepped  <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))
    selected <- prepped$steps[[1]]$selected_vars

    expect_true(all(c("wn_3980", "wn_3950") %in% selected))
    expect_lt(length(selected), 40L)
    expect_setequal(selected, paste0("wn_", c(3926, 3938, 3940, 3950, 3954, 3978, 3980, 4000)))

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


describe("step_select_cars() weights the bands as published (Li et al. 2009)", {

  ## Each iteration's fit and the bands it was given, at seed 1
  cars_traced <- function(seed = 1) {

    seen <- list()
    fits <- list()
    real <- pls::plsr
    local_mocked_bindings(plsr = function(formula, data, ...) {
      fit <- real(formula, data = data, ...)
      seen[[length(seen) + 1L]] <<- setdiff(names(data), "y")
      fits[[length(fits) + 1L]] <<- list(fit = fit, data = data)
      fit
    }, .package = "pls")

    d    <- cars_signal_data()
    step <- withr::with_seed(seed, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]

    list(step = step, seen = seen, fits = fits)

  }

  it("returns no band its scoring fit gave no weight", {

    skip_if_not_installed("pls")

    tr     <- cars_traced()
    chosen <- tr$fits[[tr$step$cars$iteration]]
    bands  <- setdiff(names(chosen$data), "y")
    coefs  <- abs(as.vector(stats::coef(chosen$fit, ncomp = chosen$fit$ncomp)))

    sds    <- vapply(chosen$data[bands], stats::sd, numeric(1))

    ## The fit is given the bands as they are, so none is shrunk towards
    ## nothing; a running product of weights drove most to 1e-80 and below
    expect_setequal(tr$step$selected_vars, bands)
    expect_gt(min(sds) / max(sds), 1e-6)
    expect_gt(min(coefs) / max(coefs), 1e-6)

  })

  it("shrinks the set every iteration within the exponential schedule, drawing from the bands it had", {

    skip_if_not_installed("pls")

    tr    <- cars_traced()
    decay <- exp(log(2 / 40) / 49)
    sizes <- lengths(tr$seen)

    for (i in seq_len(length(tr$seen) - 1L)) {

      n_keep <- max(2, floor(40 * decay^(i - 1)))

      expect_true(all(tr$seen[[i + 1L]] %in% tr$seen[[i]]), label = paste("iteration", i))
      expect_lte(sizes[i + 1L], min(n_keep, sizes[i]), label = paste("iteration", i))

    }

  })

  it("draws, at every iteration, as many times as there are bands, as carspls does", {

    skip_if_not_installed("pls")

    d     <- cars_signal_data()
    real  <- base::sample.int
    draws <- integer(0)

    ## The Monte Carlo row samples draw without weights; the adaptive
    ## reweighted sampling draws by weight, with replacement
    local_mocked_bindings(sample.int = function(n, size = n, replace = FALSE, prob = NULL, ...) {
      if (!is.null(prob)) draws <<- c(draws, if (isTRUE(replace)) size else NA_integer_)
      real(n, size, replace, prob, ...)
    }, .package = "base")

    withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))

    expect_length(draws, 50L)
    expect_true(all(draws %in% 40L))

  })

  it("draws the same bands under the same seed, and others under another", {

    skip_if_not_installed("pls")

    d    <- cars_signal_data()
    prep <- function(seed) {
      withr::with_seed(seed, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]
    }

    a <- prep(1)
    b <- prep(1)

    expect_identical(a$selected_vars, b$selected_vars)
    expect_identical(a$cars, b$cars)

    ## The draws differ by seed: of seeds 2 to 4, at least one keeps a set
    ## other than seed 1's
    others <- lapply(2:4, function(s) sort(prep(s)$selected_vars))
    expect_false(all(vapply(others, identical, logical(1), sort(a$selected_vars))))

  })

})


describe("step_select_cars() and near-constant bands", {

  it("drops a near-constant band before clustering, as step_select_boruta() does", {

    skip_if_not_installed("pls")

    ## 160 bands, more than the 150 representatives, so the clustering runs;
    ## a constant band has no correlation with the others and stopped it
    d <- withr::with_seed(1, {
      m <- matrix(stats::rnorm(60 * 160), 60)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 160))
      m[, "wn_3900"] <- 1
      d <- tibble::as_tibble(m)
      d$SOC <- 2 * d$wn_3980 - 2 * d$wn_3950 + stats::rnorm(60, sd = 0.3)
      d
    })

    expect_message(
      prepped <- withr::with_seed(1, recipes::prep(cars_recipe(d), training = d)),
      "Filtering 1 near-constant features before clustering", fixed = TRUE
    )

    step <- prepped$steps[[1]]

    expect_identical(step$cars$n_representatives, 150L)
    expect_false("wn_3900" %in% step$selected_vars)
    expect_gt(length(step$selected_vars), 0L)

  })

})


describe("step_select_cars() with too few bands to select from", {

  it("keeps a lone band, runs no PLS, and warns skipped", {

    skip_if_not_installed("pls")

    ## Two bands, one constant: one is left after the near-constant filter
    d <- withr::with_seed(1, tibble::tibble(wn_4000 = stats::rnorm(30), wn_3998 = 1))
    d$SOC <- 2 * d$wn_4000 + withr::with_seed(2, stats::rnorm(30, sd = 0.3))

    calls <- 0L
    local_mocked_bindings(plsr = function(...) { calls <<- calls + 1L; stop("no PLS here") },
                          .package = "pls")

    w <- expect_warning(
      prepped <- suppressMessages(recipes::prep(cars_recipe(d), training = d)),
      class = "horizons_warning_skipped"
    )

    step <- prepped$steps[[1]]

    expect_identical(step$selected_vars, "wn_4000")
    expect_true(step$cars$skipped)
    expect_identical(step$cars$n_bands, 1L)
    expect_identical(calls, 0L)
    expect_identical(w$step, "step_select_cars")
    expect_match(conditionMessage(w), "had 1 band to select from, too few to run CARS, so it is kept",
                 fixed = TRUE)

  })

})
