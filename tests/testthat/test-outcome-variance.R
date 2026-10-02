## ---------------------------------------------------------------------------
## Tests: evaluate() and fit() refuse an outcome with zero variance (#132)
## ---------------------------------------------------------------------------
## validate() reports a constant outcome (check P003), but nothing gates on
## its verdict and running it is optional. The modelling verbs refuse one
## themselves: on the rows they model, before any split, and on the rows a
## model is fitted on, which the split can leave constant when the modelled
## rows are not. With the constant at the lower bound of outcome_range, that
## used to fail the fit validator only after every model had been fitted.


## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

## A condition's message on one line and without cli's backticks: cli wraps
## long messages, which would split the phrases the tests look for.
one_line <- function(e) gsub("`", "", gsub("\\s+", " ", conditionMessage(e)))

## Replace each verb's expensive steps with recorders, so a test can say
## which of them ran. `split` = FALSE leaves the split draw real.
mock_expensive_steps <- function(split = TRUE, env = parent.frame()) {

  reached <- new.env()
  reached$steps <- character()

  record <- function(step) {
    function(...) {
      reached$steps <- c(reached$steps, step)
      stop("reached ", step, call. = FALSE)
    }
  }

  local_mocked_bindings(
    evaluate_single_config = record("evaluate_single_config"),
    fit_single_config      = record("fit_single_config"),
    .package = "horizons",
    .env     = env
  )

  if (split) {

    local_mocked_bindings(
      draw_eval_split = record("draw_eval_split"),
      .package = "horizons",
      .env     = env
    )

  }

  reached

}

## An outcome at the default range's floor, 0, on every row but one, which
## has `odd`. The modelled rows vary; whether the training rows do depends
## on where the split puts the odd row.
make_one_odd_row <- function(n = 40, odd = 5) {

  obj <- make_eval_object(n = n, n_configs = 1)
  obj$data$analysis$SOC <- c(odd, rep(0, n - 1))
  obj

}

## The first seed whose evaluate() split puts the odd row (the first row) in
## the test set (`in_test = TRUE`) or the training set.
seed_placing_odd_row <- function(obj, in_test) {

  for (seed in 1:200) {

    split <- suppressWarnings(draw_eval_split(obj$data$analysis, "SOC", seed)$split)

    if (identical("S001" %in% rsample::testing(split)$sample_id, in_test)) return(seed)

  }

  stop("no seed in 1:200 placed the odd row")

}


## ===========================================================================
## The rule
## ===========================================================================

describe("constant_outcome()", {

  it("finds the one value and counts the rows that have it, ignoring NA", {

    expect_identical(constant_outcome(c(3, NA, 3, 3)), list(value = 3, n = 3L))

  })

  it("is NULL for any spread, however small", {

    expect_null(constant_outcome(c(3, 3, 3 + 1e-12)))
    expect_null(constant_outcome(c(0, 0, 0, 5)))

  })

  it("leaves fewer than two values to the row floors", {

    expect_null(constant_outcome(c(3, NA)))
    expect_null(constant_outcome(numeric(0)))

  })

})


describe("check_training_outcome_variance()", {

  it("passes training rows that vary", {

    expect_no_error(check_training_outcome_variance(c(0, 0, 1), list(), "SOC", "fit"))

  })

  it("names the outcome, the value, the count, and where the other rows went", {

    err <- expect_error(
      check_training_outcome_variance(
        rep(0, 12),
        held_out = list("in the test set"              = c(0, 4, 0),
                        "in the calibration set"       = c(2, 3),
                        "trimmed as response outliers" = NULL),
        outcome_col = "SOC",
        verb        = "fit"
      ),
      class = "horizons_input_error"
    )

    msg <- one_line(err)
    expect_match(msg, "fit() cannot fit SOC: all 12 training rows have the value 0", fixed = TRUE)
    expect_match(msg, "The 3 modelled rows with another value are all outside the training rows: 1 in the test set, 2 in the calibration set", fixed = TRUE)
    expect_no_match(msg, "trimmed")

  })

})


## ===========================================================================
## A constant outcome: refused before any split, by both verbs
## ===========================================================================

describe("an outcome with zero variance on the modelled rows", {

  obj <- make_eval_object(n = 40, n_configs = 1)
  obj$data$analysis$SOC <- 2.5
  obj$data$analysis$SOC[1:3] <- NA

  it("evaluate() aborts, classed, naming the outcome and the rows, before a split", {

    reached <- mock_expensive_steps()

    err <- expect_error(evaluate(obj, verbose = FALSE), class = "horizons_input_error")

    msg <- one_line(err)
    expect_match(msg, "evaluate() cannot model SOC: it has zero variance", fixed = TRUE)
    expect_match(msg, "All 37 rows with an observed SOC have the value 2.5", fixed = TRUE)
    expect_length(reached$steps, 0)

  })

  it("a cold-start fit() aborts the same way, before a split", {

    reached <- mock_expensive_steps()

    err <- expect_error(fit(obj, verbose = FALSE), class = "horizons_input_error")

    expect_match(one_line(err), "fit() cannot model SOC: it has zero variance", fixed = TRUE)
    expect_match(one_line(err), "All 37 rows", fixed = TRUE)
    expect_length(reached$steps, 0)

  })

  it("fit() of an evaluated object aborts the same way, before fitting", {

    ## The stored fit is an evaluated object; its outcome made constant
    ## after evaluate() reaches fit() on the evaluate() path.
    fx      <- readRDS(test_path("fixtures", "ensemble_fit.rds"))
    outcome <- fx$data$role_map$variable[fx$data$role_map$role == "outcome"]
    fx$data$analysis[[outcome]] <- 1

    reached <- mock_expensive_steps()

    err <- expect_error(fit(fx, n_best = 1L, verbose = FALSE),
                        class = "horizons_input_error")

    expect_false(isFALSE(fx$evaluation$screened))
    expect_match(one_line(err), "fit() cannot model", fixed = TRUE)
    expect_match(one_line(err), "zero variance", fixed = TRUE)
    expect_length(reached$steps, 0)

  })

})


## ===========================================================================
## Training rows left constant by the split (the bound-at-the-floor case)
## ===========================================================================

describe("an outcome whose only other value falls in the test set", {

  obj  <- make_one_odd_row()
  seed <- seed_placing_odd_row(obj, in_test = TRUE)

  it("evaluate() aborts before the folds, saying where the other row went", {

    reached <- mock_expensive_steps(split = FALSE)

    err <- expect_error(suppressWarnings(evaluate(obj, seed = seed, verbose = FALSE)),
                        class = "horizons_input_error")

    msg <- one_line(err)
    expect_match(msg, "evaluate() cannot fit SOC: all 32 training rows have the value 0", fixed = TRUE)
    expect_match(msg, "The 1 modelled row with another value is outside the training rows: 1 in the test set", fixed = TRUE)
    expect_length(reached$steps, 0)

  })

  it("a cold-start fit() aborts before any model, instead of failing the bound after", {

    reached <- mock_expensive_steps(split = FALSE)

    err <- expect_error(
      suppressWarnings(fit(obj, seed = seed, compute_uq = FALSE, compute_ad = FALSE,
                           verbose = FALSE)),
      class = "horizons_input_error"
    )

    expect_match(one_line(err), "fit() cannot fit SOC: all 32 training rows have the value 0", fixed = TRUE)
    expect_match(one_line(err), "1 in the test set", fixed = TRUE)
    expect_length(reached$steps, 0)

  })

})


describe("an outcome whose only other value is drawn into the calibration set", {

  ## N_CALIB_MIN calibration rows need a training part of about 150 rows.
  obj <- make_one_odd_row(n = 200)

  it("fit() aborts before any model, naming the calibration set", {

    ## fit() draws the calibration set itself, so the first seed that puts
    ## the odd row there is found through fit(), with every model fit mocked
    ## away: any other seed reaches the mock, or refuses for the test set.
    reached <- mock_expensive_steps(split = FALSE)
    found   <- NULL

    for (seed in 1:200) {

      err <- tryCatch(
        suppressWarnings(fit(obj, seed = seed, compute_uq = TRUE, compute_ad = FALSE,
                             verbose = FALSE)),
        horizons_input_error = function(e) e,
        error                = function(e) NULL
      )

      if (!is.null(err) && grepl("in the calibration set", one_line(err), fixed = TRUE)) {
        found <- err
        break
      }

    }

    expect_false(is.null(found))
    expect_match(one_line(found), "fit() cannot fit SOC: all", fixed = TRUE)
    expect_match(one_line(found), "1 in the calibration set", fixed = TRUE)
    expect_no_match(one_line(found), "in the test set", fixed = TRUE)

  })

})


## ===========================================================================
## A near-constant but valid outcome still runs
## ===========================================================================

describe("an outcome whose only other value falls in the training rows", {

  obj  <- make_one_odd_row()
  seed <- seed_placing_odd_row(obj, in_test = FALSE)

  it("passes both guards: evaluate() reaches its configs", {

    reached <- mock_expensive_steps(split = FALSE)

    expect_error(suppressWarnings(evaluate(obj, seed = seed, verbose = FALSE)),
                 "evaluate_single_config")
    expect_identical(reached$steps, "evaluate_single_config")

  })

  it("passes both guards: a cold-start fit() reaches its member", {

    reached <- mock_expensive_steps(split = FALSE)

    expect_error(
      suppressWarnings(fit(obj, seed = seed, compute_uq = FALSE, compute_ad = FALSE,
                           verbose = FALSE)),
      "fit_single_config"
    )
    expect_identical(reached$steps, "fit_single_config")

  })

})


describe("an outcome with a tiny spread on every row", {

  obj <- make_eval_object(n = 40, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L
  obj$data$analysis$SOC <- 2 + 1e-6 * obj$data$analysis$SOC

  it("evaluate() runs", {

    ev <- suppressWarnings(evaluate(obj, verbose = FALSE))

    expect_s3_class(ev, "horizons_eval")

  })

  it("a cold-start fit() runs", {

    fx <- suppressWarnings(fit(obj, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE))

    expect_s3_class(fx, "horizons_fit")
    expect_gt(fx$models$response_bound, max(obj$data$analysis$SOC))

  })

})
