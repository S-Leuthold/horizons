## ---------------------------------------------------------------------------
## Tests: the plsr model's engine (R/pls-engine.R)
## ---------------------------------------------------------------------------
## horizons registers a "pls" engine for parsnip's pls() model, fitting with
## pls::plsr(). These check the fit against pls::plsr() directly, the
## component cap, tuning through tune, and the num_comp range the runners
## hand to tune (#216).


## Predictors with signal in a handful of columns, so several components help
make_pls_data <- function(n = 60, p = 40, seed = 11) {

  withr::local_seed(seed)

  x <- matrix(stats::rnorm(n * p), n)
  colnames(x) <- paste0("wn_", seq(4000, by = -2, length.out = p))
  y <- 2 * x[, 1] - x[, 5] + 0.5 * x[, min(12, p)] + stats::rnorm(n, sd = 0.5)

  list(x = x, y = y, df = data.frame(y = y, x))

}


describe("the pls engine", {

  it("is registered for parsnip's pls() model, in regression", {

    engines <- parsnip::show_engines("pls")
    expect_true(any(engines$engine == "pls" & engines$mode == "regression"))

  })

  it("fits pls::plsr() on centred, scaled predictors and predicts at its components", {

    d   <- make_pls_data()
    fit <- pls_fit(d$x, d$y, ncomp = 5L)
    ref <- pls::plsr(y ~ x, data = data.frame(y = d$y, x = I(d$x)), ncomp = 5,
                     scale = TRUE, method = "oscorespls")

    expect_identical(fit$horizons_ncomp, 5L)
    expect_equal(pls_predict(fit, d$x[1:10, ]),
                 as.numeric(stats::predict(ref, newdata = data.frame(x = I(d$x[1:10, ])), ncomp = 5)))

  })

  it("caps the components at the predictors and at the rows less one, and says so", {

    d <- make_pls_data(n = 12, p = 40)
    expect_warning(fit <- pls_fit(d$x, d$y, ncomp = 30L),
                   "11 components, not the 30", class = "horizons_pls_warning")
    expect_identical(fit$horizons_ncomp, 11L)

    d <- make_pls_data(n = 60, p = 6)
    expect_warning(fit <- pls_fit(d$x, d$y, ncomp = 30L),
                   "6 components, not the 30", class = "horizons_pls_warning")
    expect_identical(fit$horizons_ncomp, 6L)

    expect_no_warning(pls_fit(d$x, d$y, ncomp = 6L))

  })

  it("predicts finitely with a constant predictor, which it leaves unscaled", {

    d <- make_pls_data()
    d$x[, 3] <- 0.5

    expect_no_warning(fit <- pls_fit(d$x, d$y, ncomp = 5L))

    expect_false(anyNA(pls_predict(fit, d$x)))
    expect_equal(unname(fit$scale[3]), 1)

  })

  it("keeps the training data out of the fit", {

    ## The formula's environment would carry x into the fit's terms
    d   <- make_pls_data(n = 200, p = 400)
    fit <- pls_fit(d$x, d$y, ncomp = 5L)

    expect_lt(length(serialize(fit, NULL)), length(serialize(d$x, NULL)))

  })

  it("names horizons as a package the fit needs, so tune's workers load the engine", {

    expect_true("horizons" %in% parsnip::required_pkgs(define_model_spec("plsr")))

  })

  it("tunes num_comp through tune_grid() and tune_bayes(), over 1 to 30 by default", {

    d     <- make_pls_data()
    folds <- withr::with_seed(3, rsample::vfold_cv(d$df, v = 3))
    wf    <- workflows::workflow() |>
      workflows::add_formula(y ~ .) |>
      workflows::add_model(define_model_spec("plsr"))

    range <- workflows::extract_parameter_set_dials(wf)$object[[1]]$range
    expect_equal(c(range$lower, range$upper), c(1, PLS_MAX_COMP))

    ## A cap that binds, as the runners apply it: 8 predictors' worth
    params <- cap_pls_components(workflows::extract_parameter_set_dials(wf), 8L,
                                 min_analysis_rows(folds))

    grid  <- withr::with_seed(5, tune::tune_grid(wf, folds, grid = 3, param_info = params))
    bayes <- withr::with_seed(5, suppressMessages(
      tune::tune_bayes(wf, folds, initial = grid, iter = 1, param_info = params)
    ))

    expect_true(all(tune::collect_metrics(bayes)$num_comp <= 8))
    expect_gt(max(tune::collect_metrics(bayes)$.iter), 0)
    expect_false(anyNA(tune::collect_metrics(bayes)$mean))

  })

})


describe("cap_pls_components()", {

  pls_params <- function() {
    workflows::workflow() |>
      workflows::add_formula(y ~ .) |>
      workflows::add_model(define_model_spec("plsr")) |>
      workflows::extract_parameter_set_dials()
  }

  upper <- function(params) params$object[[which(params$name == "num_comp")]]$range$upper

  it("caps num_comp at PLS_MAX_COMP, the predictors and the rows less one, whichever is least", {

    expect_equal(upper(cap_pls_components(pls_params(), 500L, 200L)), 30)
    expect_equal(upper(cap_pls_components(pls_params(), 12L, 200L)), 12)
    expect_equal(upper(cap_pls_components(pls_params(), 500L, 20L)), 19)
    expect_equal(upper(cap_pls_components(pls_params(), 500L, 1L)), 1)

  })

  it("leaves a parameter set without num_comp alone", {

    rf <- workflows::workflow() |>
      workflows::add_formula(y ~ .) |>
      workflows::add_model(define_model_spec("rf")) |>
      workflows::extract_parameter_set_dials()

    expect_identical(cap_pls_components(rf, 3L, 3L), rf)

  })

})
