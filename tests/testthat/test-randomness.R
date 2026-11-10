## ---------------------------------------------------------------------------
## Tests: the randomness rule, through the verbs
## ---------------------------------------------------------------------------
##
## Every unit of work is seeded from the run's seed and its own name
## (with_unit_seed(); test-utils-rng.R tests the helpers alone), and no verb,
## predict() included, changes the caller's random state.


## The caller's stream and all three kinds as found after `run()`, from a
## stream under kinds no unit uses; and an absent stream left absent.
## `run()` is called once for each.
expect_rng_left_alone <- function(run) {

  withr::local_preserve_seed()

  ## suppressWarnings(): R's note that the Rounding sampler is not uniform
  suppressWarnings(RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rounding"))
  set.seed(11)
  before <- get(".Random.seed", envir = globalenv())

  run()

  expect_identical(RNGkind(), c("L'Ecuyer-CMRG", "Box-Muller", "Rounding"))
  expect_identical(get(".Random.seed", envir = globalenv()), before)

  rm(".Random.seed", envir = globalenv())

  run()

  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

}

quietly <- control_horizons(verbose = FALSE)

## Two members whose engines draw as they predict (ranger), and an ensemble
## of them with CV+ intervals
rng_fitted <- function() {
  memo_fixture("rng_fitted", function() {
    obj <- make_fit_object(n = 60, n_configs = 2, models = c("rf", "xgboost"))
    obj$config$tuning$final_bayesian_iter <- 0L
    suppressWarnings(fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
                         seed = 42L, control = quietly))
  })
}

rng_ensembled <- function() {
  memo_fixture("rng_ensembled", function() {
    suppressWarnings(ensemble(rng_fitted(), method = "weighted", optimize = FALSE,
                              compute_uq = TRUE, control = quietly))
  })
}

rng_new_data <- function(x) {
  predictors <- x$data$role_map$variable[x$data$role_map$role == "predictor"]
  x$data$analysis[1:8, c("sample_id", predictors)]
}


describe("the verbs leave the caller's random state as they found it", {

  it("evaluate()", {

    obj <- make_eval_object(n = 40, n_configs = 1)

    expect_rng_left_alone(function() {
      suppressWarnings(evaluate(obj, prune = FALSE, seed = 307L, control = quietly))
    })

  })

  it("fit()", {

    obj <- make_fit_object(n = 60, n_configs = 1)
    obj$config$tuning$final_bayesian_iter <- 0L

    expect_rng_left_alone(function() {
      suppressWarnings(fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                           seed = 42L, control = quietly))
    })

  })

  it("ensemble()", {

    fitted <- rng_fitted()

    expect_rng_left_alone(function() {
      suppressWarnings(ensemble(fitted, method = "weighted", optimize = FALSE,
                                compute_uq = TRUE, control = quietly))
    })

  })

  it("select_training(), with its targets clustered", {

    fx <- select_fixture(n_pool = 100)

    expect_rng_left_alone(function() {
      suppressWarnings(select_training(fx$targets, fx$pool, k = 10, scope = "cluster",
                                       cluster_min = 2, control = quietly))
    })

  })

  it("predict() on a fit", {

    fitted <- rng_fitted()

    expect_rng_left_alone(function() predict(fitted, rng_new_data(fitted)))

  })

  it("predict() on an ensemble", {

    ens <- rng_ensembled()

    expect_rng_left_alone(function() predict(ens, rng_new_data(ens)))

  })

})
