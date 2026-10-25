## ---------------------------------------------------------------------------
## Tests: the plsr model's engine (R/pls-engine.R)
## ---------------------------------------------------------------------------
## horizons registers a "pls" engine for parsnip's pls() model, fitting with
## pls::plsr(). These check the registration beside another package's engine,
## a fit with horizons loaded but not attached, the fit against pls::plsr()
## directly, the component cap, tuning through tune, and the num_comp range
## the runners hand to tune (#216). That the load registers the engine is
## tested in test-zzz.R.


## Predictors with signal in a handful of columns, so several components help
make_pls_data <- function(n = 60, p = 40, seed = 11) {

  withr::local_seed(seed)

  x <- matrix(stats::rnorm(n * p), n)
  colnames(x) <- paste0("wn_", seq(4000, by = -2, length.out = p))
  y <- 2 * x[, 1] - x[, 5] + 0.5 * x[, min(12, p)] + stats::rnorm(n, sd = 0.5)

  list(x = x, y = y, df = data.frame(y = y, x))

}


describe("the pls engine", {

  it("registers its engine when another package has registered one for pls() first", {

    ## plsmod, for one, registers a mixOmics engine for pls(). parsnip's
    ## tables are shared by the session, so they are put back afterwards.
    env    <- parsnip::get_model_env()
    tables <- c("pls", "pls_fit", "pls_predict", "pls_args", "pls_pkgs", "pls_encoding")
    saved  <- mget(tables, envir = env)
    withr::defer(list2env(saved, envir = env))

    ## As if the other package had loaded first and horizons not yet
    assign("pls", tibble::tibble(engine = "mixOmics", mode = c("classification", "regression")),
           envir = env)
    for (tb in tables[-1]) {
      assign(tb, saved[[tb]][saved[[tb]]$engine != "pls", ], envir = env)
    }

    register_pls_engine()

    engines <- parsnip::show_engines("pls")
    expect_true(any(engines$engine == "pls" & engines$mode == "regression"))
    expect_true(any(engines$engine == "mixOmics" & engines$mode == "regression"))

    d    <- make_pls_data(n = 30, p = 6)
    spec <- parsnip::pls(num_comp = 2) |>
      parsnip::set_engine("pls") |>
      parsnip::set_mode("regression")

    expect_equal(predict(parsnip::fit_xy(spec, x = d$x, y = d$y), new_data = as.data.frame(d$x))$.pred,
                 pls_predict(pls_fit(d$x, d$y, ncomp = 2L), d$x))

  })

  it("fits and predicts with horizons loaded but not attached", {

    ## The engine names horizons' namespace for its fit and predict
    ## functions, so it works for horizons:: calls and in workers that load
    ## the namespace without attaching it. Only an installed build can show
    ## it: load_all() attaches the package it loads.
    skip_if_dev_package()
    skip_on_cran()
    skip_if_not_installed("callr")

    d <- make_pls_data(n = 30, p = 6)

    out <- callr::r(function(x, y) {
      loadNamespace("horizons")
      spec <- parsnip::pls(num_comp = 2) |>
        parsnip::set_engine("pls") |>
        parsnip::set_mode("regression")
      fit <- parsnip::fit_xy(spec, x = x, y = y)
      list(attached = "package:horizons" %in% search(),
           pred     = predict(fit, new_data = as.data.frame(x))$.pred)
    }, args = list(x = d$x, y = d$y))

    expect_false(out$attached)
    expect_equal(out$pred, pls_predict(pls_fit(d$x, d$y, ncomp = 2L), d$x))

  })

  it("fits pls::plsr() by NIPALS on centred, scaled predictors and predicts at its components", {

    d   <- make_pls_data()
    fit <- pls_fit(d$x, d$y, ncomp = 5L)
    ref <- pls::plsr(y ~ x, data = data.frame(y = d$y, x = I(d$x)), ncomp = 5,
                     scale = TRUE, method = "oscorespls")

    ## pls's default kernel algorithm predicts the same to rounding, so only
    ## the fit's record of its algorithm tells them apart
    expect_identical(fit$method, "oscorespls")
    expect_identical(fit$horizons_ncomp, 5L)
    expect_equal(pls_predict(fit, d$x[1:10, ]),
                 as.numeric(stats::predict(ref, newdata = data.frame(x = I(d$x[1:10, ])), ncomp = 5)))

    ## The predictors may come as a data frame
    expect_equal(pls_predict(pls_fit(as.data.frame(d$x), d$y, ncomp = 5L), d$x[1:10, ]),
                 pls_predict(fit, d$x[1:10, ]))

  })

  it("caps the components at the predictors and at the rows less one, says so, and fits at least one", {

    d <- make_pls_data(n = 12, p = 40)
    expect_warning(fit <- pls_fit(d$x, d$y, ncomp = 30L),
                   "11 components, not the 30", class = "horizons_warning_substitution")
    expect_identical(fit$horizons_ncomp, 11L)

    d <- make_pls_data(n = 60, p = 6)
    expect_warning(fit <- pls_fit(d$x, d$y, ncomp = 30L),
                   "6 components, not the 30", class = "horizons_warning_substitution")
    expect_identical(fit$horizons_ncomp, 6L)

    expect_no_warning(pls_fit(d$x, d$y, ncomp = 6L))

    ## pls::plsr() refuses zero components
    expect_identical(pls_fit(d$x, d$y, ncomp = 0L)$horizons_ncomp, 1L)

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
