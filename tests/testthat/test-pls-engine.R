## ---------------------------------------------------------------------------
## Tests: the plsr model's engine (R/pls-engine.R)
## ---------------------------------------------------------------------------
## horizons registers a "pls" engine for parsnip's pls() model, fitting with
## pls::plsr(). These check the registration beside another package's engine,
## a fit with horizons loaded but not attached, the fit against pls::plsr()
## directly, the component cap, the stop at the components that carry
## information (#278), tuning through tune, and the num_comp range the
## runners hand to tune (#216). That the load registers the engine is tested
## in test-zzz.R.


## Predictors with signal in a handful of columns, so several components help
make_pls_data <- function(n = 60, p = 40, seed = 11) {

  withr::local_seed(seed)

  x <- matrix(stats::rnorm(n * p), n)
  colnames(x) <- paste0("wn_", seq(4000, by = -2, length.out = p))
  y <- 2 * x[, 1] - x[, 5] + 0.5 * x[, min(12, p)] + stats::rnorm(n, sd = 0.5)

  list(x = x, y = y, df = data.frame(y = y, x))

}

## Smooth, correlated spectra-like predictors (five latent spectra under a
## moving average, a little noise), an outcome read from two bands, and the
## PCA scores of the spectra computed on these same rows, as a fold's
## step_pca() gives them: uncorrelated, so scaled they have one variance
make_pls_spectra <- function(n = 60, p = 40, n_pcs = 8, seed = 1) {

  withr::local_seed(seed)

  latent <- matrix(stats::rnorm(n * 5), n) %*% matrix(stats::rnorm(5 * p), 5)
  x      <- t(apply(latent, 1, function(r) stats::filter(r, rep(1 / 5, 5), circular = TRUE))) +
    matrix(stats::rnorm(n * p, sd = 0.01), n)
  colnames(x) <- paste0("wn_", seq(4000, by = -2, length.out = p))
  y      <- x[, 3] - 2 * x[, 20] + stats::rnorm(n, sd = 0.1)
  scores <- stats::prcomp(x, scale. = TRUE)$x[, seq_len(n_pcs)]

  list(x = x, y = y, scores = scores)

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

  it("registers num_comp as a submodel, as MODEL_SPECS names it", {

    args <- parsnip::get_from_env("pls_args")
    arg  <- args[args$engine == "pls" & args$parsnip == "num_comp", , drop = FALSE]

    expect_identical(nrow(arg), 1L)
    expect_true(arg$has_submodel)
    expect_identical(MODEL_SPECS$plsr$submodel, "num_comp")

  })

  it("scores a grid of counts from one fit a fold, each count as a fit of its own scores it", {

    ## 40 predictors, more than the largest count: each count is predicted
    ## from a larger fit's matrices, so it equals a fit of its own to rounding
    d     <- make_pls_data()
    folds <- withr::with_seed(3, rsample::vfold_cv(d$df, v = 3))
    wf    <- fold_workflow(define_model_spec("plsr"), "y")
    grid  <- tibble::tibble(num_comp = c(1L, 2L, 4L, 7L, 10L))

    real_fit <- pls_fit
    n_fits   <- 0L
    local_mocked_bindings(pls_fit = function(...) {
      n_fits <<- n_fits + 1L
      real_fit(...)
    })

    shared <- tune::collect_metrics(withr::with_seed(5, tune::tune_grid(wf, folds, grid = grid)))

    expect_identical(n_fits, nrow(folds))

    for (k in grid$num_comp) {
      own <- tune::collect_metrics(withr::with_seed(5, tune::fit_resamples(
        tune::finalize_workflow(wf, tibble::tibble(num_comp = k)), folds
      )))
      at  <- shared[shared$num_comp == k, , drop = FALSE]
      expect_equal(at$mean[match(own$.metric, at$.metric)], own$mean, tolerance = 1e-10)
    }

  })

  it("tunes num_comp through tune_grid() and tune_bayes(), over 1 to 30 by default", {

    d     <- make_pls_data()
    folds <- withr::with_seed(3, rsample::vfold_cv(d$df, v = 3))
    wf    <- workflows::workflow() |>
      workflows::add_formula(y ~ .) |>
      workflows::add_model(define_model_spec("plsr"))

    ## The range is the default ranges' (TUNE_RANGES), as the runners set it
    ## before the bounds, not the registration's
    params <- apply_tune_ranges(workflows::extract_parameter_set_dials(wf), TUNE_RANGES)
    range  <- params$object[[1]]$range
    expect_equal(c(range$lower, range$upper), c(1, PLS_MAX_COMP))
    expect_true(dials::has_unknowns(workflows::extract_parameter_set_dials(wf)$object[[1]]))

    ## A cap that binds, as the runners apply it: 8 predictors' worth
    params <- cap_by_data(params, 8L, min_analysis_rows(folds))

    grid  <- withr::with_seed(5, tune::tune_grid(wf, folds, grid = 3, param_info = params))
    bayes <- withr::with_seed(5, suppressMessages(
      tune::tune_bayes(wf, folds, initial = grid, iter = 1, param_info = params)
    ))

    expect_true(all(tune::collect_metrics(bayes)$num_comp <= 8))
    expect_gt(max(tune::collect_metrics(bayes)$.iter), 0)
    expect_false(anyNA(tune::collect_metrics(bayes)$mean))

  })

})


describe("pls_fit() - the stop at the components that carry information", {

  plsr_spec <- function(k) {
    parsnip::pls(num_comp = k) |>
      parsnip::set_engine("pls") |>
      parsnip::set_mode("regression")
  }

  it("fits one component on PCA scores, the least-squares fit on them, and predicts every count as that one", {

    d <- make_pls_spectra()

    expect_warning(fit <- pls_fit(d$scores, d$y, ncomp = 8L),
                   "PLS fitted 1 component, not the 8 asked for: the first already fits",
                   class = "horizons_warning_substitution")
    expect_identical(fit$horizons_ncomp, 1L)
    expect_identical(fit$ncomp, 1L)

    ## pls's coefficients are on the scaled predictors
    ls_fit <- stats::lm(d$y ~ d$scores)
    expect_equal(unname(drop(stats::coef(fit, ncomp = 1)) / fit$scale), unname(stats::coef(ls_fit)[-1]))
    expect_equal(pls_predict(fit, d$scores), unname(stats::fitted(ls_fit)))

    new  <- as.data.frame(d$scores[1:10, ])
    pred <- suppressWarnings(parsnip::multi_predict(parsnip::fit_xy(plsr_spec(8), x = d$scores, y = d$y),
                                                    new, num_comp = 1:8))
    one  <- vapply(pred$.pred, function(p) p$.pred[p$num_comp == 1L], numeric(1))

    for (k in 2:8) {
      expect_identical(vapply(pred$.pred, function(p) p$.pred[p$num_comp == k], numeric(1)), one)
    }

  })

  it("leaves a fit with no degenerate component exactly as pls::plsr() gives it, with no warning", {

    d <- make_pls_spectra()

    expect_no_warning(fit <- pls_fit(d$x, d$y, ncomp = 10L))

    sds <- apply(d$x, 2, stats::sd)
    ref <- pls::plsr(y ~ x, data = data.frame(y = d$y, x = I(d$x)), ncomp = 10,
                     scale = sds, method = "oscorespls")

    expect_identical(fit$horizons_ncomp, 10L)
    expect_identical(unname(unclass(fit$coefficients)), unname(unclass(ref$coefficients)))

  })

  it("stops where a constant column spends the predictors' rank, with coefficients of the data's size", {

    ## Centred, the constant column is zero, so the centred predictors have
    ## rank 4 against the cap of 5: the fifth component would be formed from
    ## the rounding left after four, its weight unit length and its score
    ## rounding too
    withr::local_seed(2)
    x      <- matrix(stats::rnorm(30 * 5), 30)
    x[, 3] <- 0.5
    y      <- stats::rnorm(30)

    expect_warning(fit <- pls_fit(x, y, ncomp = 5L),
                   "PLS fitted 4 components, not the 5 asked for: the first 4 already fit",
                   class = "horizons_warning_substitution")
    expect_identical(fit$horizons_ncomp, 4L)

    coefs <- stats::coef(fit, ncomp = 4)
    expect_true(all(is.finite(coefs)))
    expect_lt(max(abs(coefs)), 10)

  })

  it("stops at the same component whatever the units of the outcome and of a predictor", {

    d <- make_pls_spectra()

    ### One predictor in units 1e12 times smaller, so the predictors' norm the
    ### stop measures against holds only when it is taken after scaling
    rescaled <- function(x, k) {
      x[, k] <- x[, k] * 1e12
      x
    }

    ### Outcome in micro-units on the PCA scores, in units a hundred million
    ### times larger on the spectra
    big   <- suppressWarnings(pls_fit(rescaled(d$scores, 2), d$y * 1e6, ncomp = 8L))
    small <- suppressWarnings(pls_fit(rescaled(d$x, 7), d$y * 1e-8, ncomp = 10L))

    expect_identical(big$horizons_ncomp, 1L)
    expect_identical(small$horizons_ncomp, 10L)

  })

  it("keeps a real component whose covariance with the outcome is a few orders of magnitude above rounding", {

    ## Eight orthogonal directions with distinct variances from 1 to 1e-9 of
    ## the first, rotated by a Hadamard matrix so that every column has the
    ## same variance and scaling keeps those variances, and an outcome that
    ## loads on every direction: the eighth component's covariance with what
    ## is left of the outcome is about 3e-5 of the norms it is measured
    ## against, three orders of magnitude above PLS_TOL
    withr::local_seed(4)
    n <- 60
    h <- matrix(1, 1, 1)
    for (i in 1:3) h <- rbind(cbind(h, h), cbind(h, -h))
    u <- qr.Q(qr(scale(matrix(stats::rnorm(n * 8), n), scale = FALSE)))
    x <- u %*% diag(sqrt(10^seq(0, -9, length.out = 8) * (n - 1))) %*% t(h / sqrt(8))
    y <- drop(u %*% rep(1, 8)) + stats::rnorm(n, sd = 0.01)

    expect_no_warning(fit <- pls_fit(x, y, ncomp = 8L))
    expect_identical(fit$horizons_ncomp, 8L)

  })

})


describe("tune's choice among tied counts", {

  it("takes the smallest of counts that tie exactly, as select_complete() reads tune's ranking", {

    ## Canary (#278): on PCA scores every count from the stop on predicts as
    ## the stop does, so their cross-validated means tie exactly, and the
    ## choice relies on tune numbering a submodel's values in ascending order
    ## and keeping tied rows in that order when it ranks them. Here the cap
    ## makes the tie: two correlated predictors that carry the outcome with
    ## opposite signs, so counts 2 to 5 are all fitted at 2, the
    ## least-squares fit, and beat count 1.
    withr::local_seed(3)
    d    <- data.frame(x1 = stats::rnorm(45))
    d$x2 <- 0.8 * d$x1 + stats::rnorm(45, sd = 0.6)
    d$y  <- 2 * d$x1 - 2 * d$x2 + stats::rnorm(45, sd = 0.3)

    folds <- rsample::vfold_cv(d, v = 3)
    wf    <- fold_workflow(define_model_spec("plsr"), "y")
    res   <- suppressMessages(tune::tune_grid(wf, folds, grid = tibble::tibble(num_comp = 1:5),
                                              control = tune::control_grid(verbose = FALSE)))

    cm <- tune::collect_metrics(res)
    cm <- cm[cm$.metric == "rmse", ]
    expect_identical(length(unique(cm$mean[cm$num_comp >= 2])), 1L)
    expect_lt(cm$mean[cm$num_comp == 2], cm$mean[cm$num_comp == 1])

    expect_identical(select_complete(res, "rmse", 3L)$num_comp, 2L)

  })

})


describe("multi_predict() for a plsr fit", {

  it("predicts every count asked for from one fit, a count above the fit's own at the fit's", {

    d    <- make_pls_data(n = 40, p = 6)
    d$x  <- d$x[, 1:3]
    spec <- parsnip::pls(num_comp = 5) |>
      parsnip::set_engine("pls") |>
      parsnip::set_mode("regression")

    expect_warning(fit <- parsnip::fit_xy(spec, x = d$x, y = d$y),
                   "3 components, not the 5", class = "horizons_warning_substitution")
    expect_s3_class(fit, "_horizons_pls")

    new  <- as.data.frame(d$x[1:4, ])
    pred <- parsnip::multi_predict(fit, new, num_comp = c(4L, 1L, 5L))

    expect_identical(nrow(pred), 4L)
    expect_identical(pred$.pred[[1]]$num_comp, c(4L, 1L, 5L))

    at <- function(k) vapply(pred$.pred, function(p) p$.pred[p$num_comp == k], numeric(1))

    ## 4 and 5 at the three components the fit holds, as predict() gives
    ## them; 1 as a fit of one component predicts
    expect_equal(at(4L), predict(fit, new)$.pred)
    expect_equal(at(5L), predict(fit, new)$.pred)
    expect_equal(at(1L), pls_predict(pls_fit(d$x, d$y, ncomp = 1L), new))

  })

  it("stops before a component the outcome, already fitted exactly, leaves nothing to form, and predicts every count as the one it holds", {

    ## Two orthogonal predictors, each scaled to a sd of exactly 2, and an
    ## outcome the first component fits exactly, in numbers every BLAS
    ## sums exactly: the second component's weight would be 0, which pls
    ## returns as NaN, and through its projection NaN at every count
    x <- cbind(a = c(2, -2, 2, -2, 0), b = c(2, 2, -2, -2, 0))
    y <- 5 + x[, "a"]

    expect_warning(two <- pls_fit(x, y, ncomp = 2L),
                   "PLS fitted 1 component, not the 2 asked for: the first already fits",
                   class = "horizons_warning_substitution")
    expect_identical(two$horizons_ncomp, 1L)
    expect_identical(two$ncomp, 1L)

    new <- as.data.frame(x)
    expect_equal(pls_predict(two, new), y)

    ## So a grid stage's one fit scores both counts as the fit it holds
    spec <- parsnip::pls(num_comp = 2) |>
      parsnip::set_engine("pls") |>
      parsnip::set_mode("regression")
    pred <- suppressWarnings(parsnip::multi_predict(parsnip::fit_xy(spec, x = x, y = y), new, num_comp = 1:2))
    at   <- function(k) vapply(pred$.pred, function(p) p$.pred[p$num_comp == k], numeric(1))

    expect_equal(at(1L), y)
    expect_equal(at(2L), y)

  })

  it("stops where the first component fits the outcome to rounding but not exactly, on the residual's norm alone", {

    ## An outcome along the first singular direction of the scaled
    ## predictors: the first component fits it, and what is left is rounding
    ## but not zero. The second weight is then finite, and its score, a real
    ## direction of the predictors, covaries with that residual as any
    ## direction would, so only the test on the residual's norm stops here.
    withr::local_seed(6)
    x  <- matrix(stats::rnorm(40 * 6), 40) %*% matrix(stats::rnorm(36), 6)
    xs <- scale(x)
    y  <- 2 + drop(xs %*% svd(xs)$v[, 1])

    full <- pls_plsr(x, y, 6L, pls_scale(x)$sds)
    expect_true(all(is.finite(unclass(full$loading.weights))))
    expect_gt(sqrt(sum(full$residuals[, 1, 1]^2)), 0)

    expect_warning(fit <- pls_fit(x, y, ncomp = 6L),
                   "PLS fitted 1 component, not the 6 asked for: the first already fits",
                   class = "horizons_warning_substitution")
    expect_identical(fit$horizons_ncomp, 1L)
    expect_equal(pls_predict(fit, x), y)

  })

})


describe("pls_cap_records()", {

  ## Folds as assemble_folds() makes them, the outcome and each fold's
  ## predictors, which a selection can make differ by fold
  fold_of <- function(n_predictors, n_rows, n_out = 5L) {
    data   <- as.data.frame(matrix(stats::rnorm((n_rows + n_out) * n_predictors), ncol = n_predictors))
    data$y <- stats::rnorm(n_rows + n_out)
    rsample::make_splits(list(analysis = seq_len(n_rows), assessment = n_rows + seq_len(n_out)),
                         data = data)
  }

  folds <- withr::with_seed(7, rsample::manual_rset(
    list(fold_of(4L, 30L), fold_of(10L, 30L), fold_of(4L, 30L), fold_of(10L, 5L)),
    ids = paste0("Fold", 1:4)
  ))

  it("says, as the fits at the chosen count would, which folds cannot hold it", {

    recs <- pls_cap_records(folds, tibble::tibble(num_comp = 6L, .config = "pre0_mod1_post0"), "y")

    expect_identical(recs$cause, c(
      "PLS fitted 4 components, not the 6 asked for: the data have 4 predictors and 30 rows.",
      "PLS fitted 4 components, not the 6 asked for: the data have 10 predictors and 5 rows."
    ))
    expect_identical(recs$folds, list(c("Fold1", "Fold3"), "Fold4"))
    expect_identical(recs$scope, c("cv", "cv"))

    ## The line a fit at that count on each fold reads, from tune's notes
    oof <- withr::with_seed(5, suppressMessages(tune::fit_resamples(
      tune::finalize_workflow(fold_workflow(define_model_spec("plsr"), "y"), tibble::tibble(num_comp = 6L)),
      folds, control = tune::control_resamples(verbose = FALSE)
    )))
    expect_identical(render_warning_log(recs), render_warning_log(tune_note_records(oof)))

  })

  it("is empty when the count fits every fold, or no count was tuned", {

    expect_identical(nrow(pls_cap_records(folds, tibble::tibble(num_comp = 4L), "y")), 0L)
    expect_identical(nrow(pls_cap_records(folds, tibble::tibble(mtry = 3L), "y")), 0L)

  })

  it("says which folds' fits stop below the chosen count, counted on each fold's own rows, as the fits at that count would", {

    ## Folds as assemble_folds() makes them, the outcome first. Fold2 holds
    ## PCA scores computed on its own analysis rows, uncorrelated there, so a
    ## fit on them stops at one component; the other folds' predictors carry
    ## the outcome over several components. The dimensions alone cannot tell
    ## the folds apart: each has 8 predictors and 40 analysis rows.
    withr::local_seed(8)
    fold_with <- function(scores) {
      x <- matrix(stats::rnorm(48 * 8), 48)
      if (scores) {
        pca     <- stats::prcomp(x[1:40, ], scale. = TRUE)
        x[1:40, ] <- pca$x
        x[41:48, ] <- stats::predict(pca, x[41:48, ])
      }
      data <- data.frame(y = drop(x %*% seq(1, 0.3, length.out = 8)) + stats::rnorm(48, sd = 0.1), x)
      rsample::make_splits(list(analysis = 1:40, assessment = 41:48), data = data)
    }
    folds <- rsample::manual_rset(list(fold_with(FALSE), fold_with(TRUE), fold_with(FALSE)),
                                  ids = paste0("Fold", 1:3))

    recs <- pls_cap_records(folds, tibble::tibble(num_comp = 3L, .config = "pre0_mod1_post0"), "y")

    expect_identical(recs$cause, pls_cap_message(1L, 3L, 8L, 40L, "stop"))
    expect_identical(recs$folds, list("Fold2"))

    ## The line a fit at that count on each fold reads, from tune's notes
    oof <- withr::with_seed(5, suppressMessages(tune::fit_resamples(
      tune::finalize_workflow(fold_workflow(define_model_spec("plsr"), "y"), tibble::tibble(num_comp = 3L)),
      folds, control = tune::control_resamples(verbose = FALSE)
    )))
    expect_identical(render_warning_log(recs), render_warning_log(tune_note_records(oof)))

    ## A count of one cannot stop, so it is not fitted
    n_fits <- 0L
    real_plsr <- pls::plsr
    local_mocked_bindings(plsr = function(...) {
      n_fits <<- n_fits + 1L
      real_plsr(...)
    }, .package = "pls")

    expect_identical(nrow(pls_cap_records(folds, tibble::tibble(num_comp = 1L), "y")), 0L)
    expect_identical(n_fits, 0L)

  })

  it("leaves off pls_fit()'s warning, and nothing else, with drop_pls_caps()", {

    recs <- text_records(c(pls_cap_message(4L, 6L, 4L, 30L), "PLS fitted the data: another warning"))

    expect_identical(drop_pls_caps(recs)$cause, "PLS fitted the data: another warning")

  })

})
