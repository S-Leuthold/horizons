## ---------------------------------------------------------------------------
## Tests: the randomness rule's helpers
## ---------------------------------------------------------------------------
##
## Every draw site runs under with_unit_seed(): a seed that is a pure function
## of the run's seed and the unit's name, Mersenne-Twister with all three
## kinds pinned, and the caller's state restored on exit. These test the
## helpers alone; test-randomness.R tests the rule through the verbs.

has_stream <- function() exists(".Random.seed", envir = globalenv(), inherits = FALSE)

describe("local_rng_state()", {

  ## Seeds and draws inside, after an exit handler of its own, as the sites
  ## that call the helper do
  exited <- FALSE
  seeds_and_draws <- function(fail = FALSE) {
    on.exit(exited <<- TRUE, add = TRUE)
    local_rng_state()
    set.seed(1, kind = "Mersenne-Twister")
    stats::runif(5)
    if (fail) stop("failed after seeding")
    invisible(NULL)
  }

  it("restores the caller's stream and kind from a function frame, on return and on error", {

    withr::local_seed(3, .rng_kind = "L'Ecuyer-CMRG")
    before <- .Random.seed

    exited <<- FALSE
    seeds_and_draws()
    expect_identical(.Random.seed, before)
    expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
    expect_true(exited)

    exited <<- FALSE
    expect_error(seeds_and_draws(fail = TRUE), "failed after seeding")
    expect_identical(.Random.seed, before)
    expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
    expect_true(exited)

  })

  it("restores the stream when a local() block ends, before the function goes on", {

    in_block <- function() {
      local({
        local_rng_state()
        set.seed(1)
        stats::runif(5)
      })
      .Random.seed
    }

    withr::local_seed(4)
    before <- .Random.seed

    expect_identical(in_block(), before)

  })

  it("leaves an absent stream absent", {

    withr::with_preserve_seed({

      if (has_stream()) rm(".Random.seed", envir = globalenv())

      seeds_and_draws()

      expect_false(has_stream())

    })

  })

})

describe("unit_seed()", {

  it("is a pure function of the root and the name", {

    expect_identical(unit_seed(307, "evaluate/split"), unit_seed(307, "evaluate/split"))

    ## Its value is fixed under rule version 1: a change here changes every
    ## result, and bumps SEED_RULE
    expect_identical(unit_seed(307, "evaluate/split"),
                     strtoi(substr(digest::digest("307/evaluate/split"), 1, 7), 16L))

  })

  it("gives one seed for a whole-number double and its integer, and for a name as one string or its parts", {

    expect_identical(unit_seed(307, c("evaluate", "rf_ab12cd", "tune")),
                     unit_seed(307L, "evaluate/rf_ab12cd/tune"))

    ## paste() writes 1e5 as "1e+05", so the root is keyed as an integer
    expect_identical(unit_seed(1e5, "evaluate/split"), unit_seed(100000L, "evaluate/split"))

  })

  it("gives different units and different roots different seeds, within R's integer range", {

    seeds <- c(unit_seed(307, "evaluate/split"),  unit_seed(307, "evaluate/folds"),
               unit_seed(308, "evaluate/split"),  unit_seed(307, c("fit", "rf_ab12cd", "oof")),
               unit_seed(307, c("evaluate", "rf_ab12cd", "oof")))

    expect_false(anyDuplicated(seeds) > 0)
    expect_true(is.integer(seeds))
    expect_true(all(seeds >= 0L & seeds < 2^28))

  })

  it("refuses a root that is not one whole number, as an internal error", {

    expect_error(unit_seed(NULL, "evaluate/split"), class = "horizons_error_internal")
    expect_error(unit_seed(NA_integer_, "evaluate/split"), class = "horizons_error_internal")
    expect_error(unit_seed(1.5, "evaluate/split"), class = "horizons_error_internal")
    expect_error(unit_seed(c(1, 2), "evaluate/split"), class = "horizons_error_internal")

  })

})

describe("with_unit_seed()", {

  it("draws from the unit's seed under all three default kinds, whatever the session's", {

    expected <- withr::with_seed(unit_seed(307, "a/b"), stats::rnorm(3) + sample(10, 3),
                                 .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
                                 .rng_sample_kind = "Rejection")

    withr::local_seed(1, .rng_kind = "L'Ecuyer-CMRG", .rng_normal_kind = "Box-Muller")
    suppressWarnings(RNGkind(sample.kind = "Rounding"))

    kinds <- with_unit_seed(307, "a/b", RNGkind())
    drawn <- with_unit_seed(307, "a/b", stats::rnorm(3) + sample(10, 3))

    expect_identical(kinds, c("Mersenne-Twister", "Inversion", "Rejection"))
    expect_identical(drawn, expected)

  })

  it("restores the caller's stream and kinds on return and on error", {

    withr::local_seed(3, .rng_kind = "L'Ecuyer-CMRG", .rng_normal_kind = "Box-Muller")
    before       <- .Random.seed
    kinds_before <- RNGkind()

    with_unit_seed(307, "a/b", stats::runif(5))
    expect_identical(.Random.seed, before)
    expect_identical(RNGkind(), kinds_before)

    expect_error(with_unit_seed(307, "a/b", {
      stats::runif(5)
      stop("failed inside the unit")
    }), "failed inside the unit")
    expect_identical(.Random.seed, before)
    expect_identical(RNGkind(), kinds_before)

  })

  it("leaves an absent stream absent", {

    withr::with_preserve_seed({

      if (has_stream()) rm(".Random.seed", envir = globalenv())

      with_unit_seed(307, "a/b", stats::runif(5))

      expect_false(has_stream())

    })

  })

  it("leaves an outer unit's stream where an inner unit found it", {

    outer_alone <- with_unit_seed(307, "outer", c(stats::runif(2), stats::runif(2)))

    outer_nested <- with_unit_seed(307, "outer", {
      first <- stats::runif(2)
      with_unit_seed(307, "inner", stats::runif(50))
      c(first, stats::runif(2))
    })

    expect_identical(outer_nested, outer_alone)

  })

})

describe("seed_engine()", {

  skip_if_not_installed("ranger")

  recipe <- recipes::recipe(mpg ~ ., data = mtcars)

  workflow_for <- function(spec) workflows::add_model(workflows::add_recipe(workflows::workflow(), recipe), spec)

  engine_args <- function(workflow) {
    lapply(workflows::extract_spec_parsnip(workflow)$eng_args, rlang::eval_tidy)
  }

  it("sets the seed and keeps the other engine arguments", {

    rf     <- workflow_for(define_model_spec("rf"))
    seeded <- seed_engine(rf, 4123L)

    expect_identical(engine_args(seeded)$seed, 4123L)
    expect_identical(engine_args(seeded)$importance, engine_args(rf)$importance)
    expect_identical(names(engine_args(seeded)), c(names(engine_args(rf)), "seed"))

  })

  it("keeps the model's tuning parameters", {

    rf     <- workflow_for(define_model_spec("rf"))
    seeded <- seed_engine(rf, 4123L)

    expect_identical(hardhat::extract_parameter_set_dials(seeded)$id,
                     hardhat::extract_parameter_set_dials(rf)$id)

  })

  it("gives xgboost its seed, and leaves an engine without one unchanged", {

    skip_if_not_installed("nnet")

    xgb <- seed_engine(workflow_for(define_model_spec("xgboost")), 11L)
    expect_identical(engine_args(xgb)$seed, 11L)

    mlp <- workflow_for(define_model_spec("mlp"))
    expect_identical(seed_engine(mlp, 11L), mlp)
    expect_identical(engine_args(seed_engine(mlp, 11L))$MaxNWts, 10000)

  })

  it("names the engines that take a seed", {

    expect_setequal(SEEDED_ENGINES, c("ranger", "xgboost", "lightgbm"))

  })

})
