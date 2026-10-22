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
    ## signal bands survive at every seed, with exactly those two kept at 18
    ## and three or four at the other two; at seed 1 the set is pinned
    ## below.
    d <- cars_signal_data()

    prepped  <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))
    selected <- prepped$steps[[1]]$selected_vars

    expect_true(all(c("wn_3980", "wn_3950") %in% selected))
    expect_lt(length(selected), 40L)
    expect_setequal(selected, c("wn_3980", "wn_3950"))

  })

})


## Helper: every PLS fit a prep makes, split as the step makes them. The
## weight fits (one per iteration, on its Monte Carlo sample, with no
## validation) and the scoring fits (one per distinct set, cross-validated
## on the shared folds, over every row).
cars_fits <- function(d, seed = 1) {

  weight  <- list()
  scoring <- list()

  real <- pls::plsr
  local_mocked_bindings(plsr = function(formula, data, ...) {
    fit  <- real(formula, data = data, ...)
    args <- list(...)
    one  <- list(fit = fit, bands = setdiff(names(data), "y"), rows = nrow(data),
                 segments = args$segments, ncomp = args$ncomp)
    if (identical(args$validation, "CV")) {
      scoring[[length(scoring) + 1L]] <<- one
    } else {
      weight[[length(weight) + 1L]] <<- one
    }
    fit
  }, .package = "pls")

  step <- withr::with_seed(seed, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]

  list(step = step, weight = weight, scoring = scoring)

}

## Helper: carspls's folds, written out: rows sorted by the outcome and dealt
## to five folds in turn (plscv, order = 0)
dealt_folds <- function(y) {
  sorted <- order(y)
  unname(split(sorted, 1 + (seq_along(sorted) - 1) %% 5))
}


describe("step_select_cars() scores every set after the loop, as carspls does (#106)", {

  it("returns the set with the lowest score, each iteration's set scored on every row and on one partition", {

    skip_if_not_installed("pls")

    d  <- cars_signal_data()
    tr <- cars_fits(d)

    rec <- tr$step$cars
    key <- vapply(tr$weight, function(w) paste(w$bands, collapse = ","), character(1))

    ## Fifty weight fits, each on the 48-row Monte Carlo sample of the 60
    ## rows; one scoring fit per distinct set, each on all 60 rows
    expect_length(tr$weight, 50L)
    expect_true(all(vapply(tr$weight, `[[`, integer(1), "rows") == 48L))
    expect_length(tr$scoring, length(unique(key)))
    expect_true(all(vapply(tr$scoring, `[[`, integer(1), "rows") == 60L))

    ## The one partition, the same for every set: rows sorted by the outcome
    ## and dealt to the folds in turn
    for (s in tr$scoring) expect_identical(s$segments, dealt_folds(d$SOC))

    ## Every iteration's score is its set's, and the set returned is the
    ## lowest-scoring iteration's, every band of which its fits were given
    scored <- vapply(tr$scoring, function(s) {
      min(pls::RMSEP(s$fit, estimate = "CV", intercept = FALSE)$val)
    }, numeric(1))
    names(scored) <- vapply(tr$scoring, function(s) paste(s$bands, collapse = ","), character(1))

    expect_equal(rec$path$rmsecv, unname(scored[key]))
    expect_identical(rec$iteration, which.min(rec$path$rmsecv))
    expect_setequal(tr$step$selected_vars, tr$weight[[rec$iteration]]$bands)
    expect_identical(rec$n_kept, length(tr$step$selected_vars))
    expect_gt(rec$iteration, 1L)
    expect_false(rec$retained_all)

  })

  it("scores a set by carspls's arithmetic: the pooled RMSECV over the dealt folds at its best component count", {

    skip_if_not_installed("pls")

    ## Six bands, so every band is its own representative and the first
    ## iteration's set is all six
    d <- withr::with_seed(3, {
      m <- matrix(stats::rnorm(30 * 6), 30)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 6))
      d <- tibble::as_tibble(m)
      d$SOC <- m[, 1] - 0.5 * m[, 4] + stats::rnorm(30, sd = 0.5)
      d
    })

    rec <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]$cars

    ## By hand: each fold predicted by a PLS fit on the other four, at every
    ## component count up to the cap (the six bands, under the PLS cap of 30
    ## and the 23 rows a fold's fit has less one); the score is the root of
    ## the pooled squared error at the count where it is lowest
    X     <- as.matrix(d[, 1:6])
    folds <- dealt_folds(d$SOC)
    A     <- min(PLS_MAX_COMP, 6L, 30L - max(lengths(folds)) - 1L)
    press <- matrix(NA_real_, 30, A)

    for (f in folds) {
      fit <- pls::plsr(SOC ~ ., data = d[-f, ], ncomp = A)
      for (a in seq_len(A)) press[f, a] <- (predict(fit, newdata = d[f, ], ncomp = a)[, 1, 1] - d$SOC[f])^2
    }

    rmsecv <- sqrt(colMeans(press))
    lv     <- which.min(rmsecv)
    fold_e <- vapply(folds, function(f) sqrt(mean(press[f, lv])), numeric(1))

    expect_equal(rec$path$rmsecv[1], min(rmsecv))
    expect_identical(rec$path$ncomp[1], lv)
    expect_equal(rec$path$se[1], stats::sd(fold_e) / sqrt(5))

  })

  it("returns the first of the sets tied at the lowest score, as carspls takes the first minimum", {

    skip_if_not_installed("pls")

    d <- cars_signal_data()

    ## Every set scored alike: the first iteration's, every band, is returned
    local_mocked_bindings(cars_score_path = function(X, y, sets, folds, max_comp, ...) {
      tibble::tibble(rmsecv = rep(1, length(sets)), se = rep(0, length(sets)),
                     ncomp = rep(1L, length(sets)))
    })

    ## CARS's scored answer is every band: recorded, and no warning, since
    ## it is not a fallback
    expect_no_warning(
      step <- withr::with_seed(1, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]
    )

    expect_identical(step$cars$iteration, 1L)
    expect_length(step$selected_vars, 40L)
    expect_true(step$cars$retained_all)

  })

  it("takes each set's minimum over component counts, not libPLS's one-standard-deviation rule", {

    skip_if_not_installed("pls")

    d  <- cars_signal_data()
    tr <- cars_fits(d)

    ## The first set, every band: its error at each component count, and the
    ## count libPLS's default rule would take (the fewest within one standard
    ## deviation of the per-row squared errors at the minimum)
    first <- tr$scoring[[1]]
    e2    <- (first$fit$validation$pred[, 1, ] - d$SOC)^2
    mse   <- colMeans(e2)
    lv    <- which.min(mse)
    lv_sd <- min(which(mse <= min(mse) + stats::sd(e2[, lv])))

    expect_lt(lv_sd, lv)
    expect_identical(tr$step$cars$path$ncomp[1], unname(lv))
    expect_equal(tr$step$cars$path$rmsecv[1], unname(sqrt(mse[lv])))
    expect_gt(unname(sqrt(mse[lv_sd])), tr$step$cars$path$rmsecv[1])

  })

  it("records the path: each iteration's representatives and bands, score, standard error and components", {

    skip_if_not_installed("pls")

    ## 160 bands, so 150 representatives and clusters to expand
    d <- withr::with_seed(5, {
      m <- matrix(stats::rnorm(60 * 160), 60)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 160))
      d <- tibble::as_tibble(m)
      d$SOC <- 2 * d$wn_3980 - 2 * d$wn_3950 + stats::rnorm(60, sd = 0.3)
      d
    })

    tr   <- cars_fits(d)
    rec  <- tr$step$cars
    path <- rec$path

    expect_identical(names(path), c("iteration", "representatives", "n_bands", "rmsecv", "se", "ncomp"))
    expect_identical(path$iteration, 1:50)
    expect_identical(lengths(path$representatives), vapply(tr$weight, function(w) length(w$bands), integer(1)))
    expect_identical(path$representatives[[1]], tr$weight[[1]]$bands)
    expect_length(path$representatives[[1]], 150L)

    ## The clusters are kept once, one per representative; a set's bands are
    ## its representatives' clusters, as many as n_bands says
    bands_of <- function(i) unlist(rec$clusters[path$representatives[[i]]], use.names = FALSE)

    expect_identical(names(rec$clusters), path$representatives[[1]])
    expect_setequal(bands_of(1), paste0("wn_", seq(4000, by = -2, length.out = 160)))
    expect_identical(path$n_bands, vapply(seq_len(50), function(i) length(bands_of(i)), integer(1)))
    expect_true(all(path$n_bands >= lengths(path$representatives)))

    ## The score, its count and its standard error are the scoring fit's
    key  <- vapply(path$representatives, paste, character(1), collapse = ",")
    fits <- stats::setNames(tr$scoring, vapply(tr$scoring, function(s) paste(s$bands, collapse = ","), character(1)))

    for (i in c(1L, 25L, 50L)) {
      fit  <- fits[[key[i]]]$fit
      cv   <- pls::RMSEP(fit, estimate = "CV", intercept = FALSE)$val[1, 1, ]
      e2   <- (fit$validation$pred[, 1, which.min(cv)] - d$SOC)^2
      fold <- vapply(dealt_folds(d$SOC), function(f) sqrt(mean(e2[f])), numeric(1))
      expect_equal(path$rmsecv[i], unname(min(cv)), label = paste("iteration", i))
      expect_identical(path$ncomp[i], unname(which.min(cv)), label = paste("iteration", i))
      expect_equal(path$se[i], stats::sd(fold) / sqrt(5), label = paste("iteration", i))
    }

    ## The set returned is the chosen iteration's bands
    expect_identical(rec$iteration, which.min(path$rmsecv))
    expect_setequal(tr$step$selected_vars, bands_of(rec$iteration))

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

    ## A noise outcome, where the intercept-only model's error is the lowest
    ## for some sets
    d <- withr::with_seed(2, {
      m <- matrix(stats::rnorm(60 * 40), 60)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 40))
      d <- tibble::as_tibble(m)
      d$SOC <- stats::rnorm(60)
      d
    })

    tr <- cars_fits(d)

    with_intercept <- vapply(tr$scoring, function(s) min(pls::RMSEP(s$fit, estimate = "CV")$val), numeric(1))
    components     <- vapply(tr$scoring, function(s) min(pls::RMSEP(s$fit, estimate = "CV", intercept = FALSE)$val), numeric(1))

    expect_true(any(with_intercept < components))
    expect_true(all(tr$step$cars$path$rmsecv %in% components))

  })

})


describe("step_select_cars() weights the bands as published (Li et al. 2009)", {

  ## Each iteration's fit and the bands it was given, at seed 1, and each
  ## adaptive reweighted draw: the size of the pool it drew from and what it
  ## drew. The Monte Carlo row samples draw without weights, so the draws by
  ## weight are the reweighted sampling's.
  cars_traced <- function(seed = 1) {

    seen  <- list()
    fits  <- list()
    pools <- integer(0)
    draws <- list()

    ## The weight fits only: the scoring fits after the loop are
    ## cross-validated
    real <- pls::plsr
    local_mocked_bindings(plsr = function(formula, data, ...) {
      fit <- real(formula, data = data, ...)
      if (!identical(list(...)$validation, "CV")) {
        seen[[length(seen) + 1L]] <<- setdiff(names(data), "y")
        fits[[length(fits) + 1L]] <<- list(fit = fit, data = data)
      }
      fit
    }, .package = "pls")

    real_sample <- base::sample.int
    local_mocked_bindings(sample.int = function(n, size = n, replace = FALSE, prob = NULL, ...) {
      out <- real_sample(n, size, replace, prob, ...)
      if (!is.null(prob)) {
        pools <<- c(pools, as.integer(n))
        draws[[length(draws) + 1L]] <<- out
      }
      out
    }, .package = "base")

    d    <- cars_signal_data()
    step <- withr::with_seed(seed, suppressMessages(recipes::prep(cars_recipe(d), training = d)))$steps[[1]]

    list(step = step, seen = seen, fits = fits, pools = pools, draws = draws)

  }

  it("returns no band its weight fit gave no weight", {

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

      n_keep <- max(2, floor(40 * decay^i))

      expect_true(all(tr$seen[[i + 1L]] %in% tr$seen[[i]]), label = paste("iteration", i))
      expect_lte(sizes[i + 1L], min(n_keep, sizes[i]), label = paste("iteration", i))

    }

  })

  it("keeps, as each next set, exactly the bands its weighted draw picked from the pool", {

    skip_if_not_installed("pls")

    tr <- cars_traced()

    ## The pool is the top bands by this fit's weight; the next set is the
    ## bands the reweighted sampling drew from it, each once, in their order
    for (i in seq_len(49L)) {

      fit  <- tr$fits[[i]]$fit
      w    <- abs(as.vector(stats::coef(fit, ncomp = fit$ncomp)))
      pool <- order(w, decreasing = TRUE)[seq_len(tr$pools[i])]

      expect_identical(tr$seen[[i + 1L]], tr$seen[[i]][sort(unique(pool[tr$draws[[i]]]))],
                       label = paste("iteration", i))

    }

    ## Drawn with replacement, it leaves pooled bands out: at some iteration
    ## the next set is smaller than the pool it was drawn from
    expect_true(any(lengths(tr$seen)[-1] < tr$pools[-50]))

  })

  it("cuts after each fit to the share the next iteration may hold, as carspls does", {

    skip_if_not_installed("pls")

    tr    <- cars_traced()
    decay <- exp(log(2 / 40) / 49)
    sizes <- lengths(tr$seen)

    ## carspls computes the share a * exp(-b * (iter + 1)) after fit iter:
    ## each draw's pool is the top K by weight, K the next iteration's share
    ## of the 40 bands (at least two), or every band the fit had when fewer
    expected <- as.integer(pmin(pmax(2, floor(40 * decay^seq_len(50))), sizes))

    expect_identical(tr$pools, expected)
    expect_identical(sizes[1], 40L)
    expect_lte(sizes[50], 2L)

  })

  it("fits the weights at the full set's CV-chosen count, and scores up to the PLS cap of 30", {

    skip_if_not_installed("pls")

    ## 60 rows of 40 bands: the weight fits have 48 rows, the scoring fits'
    ## folds 48. The full set, scored first, before the loop, chooses a
    ## count well under 30; the weight fits take it, limited by their bands,
    ## and the scoring fits take up to 30, limited by theirs
    tr <- cars_fits(cars_signal_data())

    full    <- tr$scoring[[1]]
    chosen  <- unname(which.min(pls::RMSEP(full$fit, estimate = "CV", intercept = FALSE)$val[1, 1, ]))
    w_bands <- vapply(tr$weight, function(w) length(w$bands), integer(1))
    w_comp  <- vapply(tr$weight, `[[`, numeric(1), "ncomp")
    s_bands <- vapply(tr$scoring, function(s) length(s$bands), integer(1))
    s_comp  <- vapply(tr$scoring, `[[`, numeric(1), "ncomp")

    expect_length(full$bands, 40L)
    expect_lt(chosen, 30L)
    expect_identical(w_comp, as.numeric(pmin(chosen, 47L, w_bands)))
    expect_identical(s_comp[1], 30)
    expect_identical(s_comp, as.numeric(pmin(PLS_MAX_COMP, 47L, s_bands)))
    expect_identical(tr$step$cars$path$ncomp[1], chosen)

    ## 25 rows: the scoring fits' 20-row folds allow 19, and the weight fits'
    ## 20-row samples allow 19 too, above the count chosen
    small <- withr::with_seed(4, {
      m <- matrix(stats::rnorm(25 * 40), 25)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = 40))
      d <- tibble::as_tibble(m)
      d$SOC <- 2 * d$wn_3980 + stats::rnorm(25, sd = 0.3)
      d
    })

    tr     <- cars_fits(small)
    chosen <- unname(which.min(pls::RMSEP(tr$scoring[[1]]$fit, estimate = "CV", intercept = FALSE)$val[1, 1, ]))

    expect_identical(tr$scoring[[1]]$ncomp, 19)
    expect_identical(tr$weight[[1]]$ncomp, as.numeric(min(chosen, 19L)))

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

    ## Counted on the record and shown by print(), not said on the console,
    ## which tune and verbose = FALSE cannot quiet
    expect_no_message(
      prepped <- withr::with_seed(1, recipes::prep(cars_recipe(d), training = d)),
      message = "near-constant"
    )

    step <- prepped$steps[[1]]

    expect_identical(step$cars$n_constant, 1L)
    expect_identical(step$cars$n_bands, 159L)
    expect_identical(step$cars$n_representatives, 150L)
    expect_true(step$cars$clustered)
    expect_false("wn_3900" %in% step$selected_vars)
    expect_gt(length(step$selected_vars), 0L)
    expect_output(print(step), "1 near-constant band dropped before selection", fixed = TRUE)

  })

  it("keeps a band small in absolute terms but not for its spectrum: second derivatives at a wide window", {

    skip_if_not_installed("pls")

    ## Smooth spectra: a sharp peak that varies and carries nothing, and a
    ## broad shallow one whose height is the outcome
    d <- withr::with_seed(1, {
      n <- 60; p <- 120; j <- seq_len(p)
      sharp  <- exp(-((j - 30) / 6)^2)
      broad  <- exp(-((j - 90) / 20)^2)
      height <- stats::rnorm(n, 1, 0.3)
      m <- outer(stats::rnorm(n, 0.5, 0.05), rep(1, p)) +
        outer(stats::rnorm(n, 1, 0.3), sharp) +
        0.002 * outer(height, broad) +
        matrix(stats::rnorm(n * p, sd = 1e-4), n)
      colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = p))
      d     <- tibble::as_tibble(m)
      d$SOC <- 10 * height + stats::rnorm(n, sd = 0.1)
      d
    })

    transform <- recipes::recipe(SOC ~ ., data = d) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "deriv2", window_size = 21)

    ## The band that carries the outcome has a variance far below 1e-10, but
    ## ordinary for a second-derivative spectrum
    baked <- recipes::bake(recipes::prep(transform, training = d), new_data = NULL)
    spec  <- as.matrix(baked[, grepl("^spec[0-9]+$", names(baked))])
    best  <- which.max(abs(stats::cor(spec, baked$SOC))[, 1])

    expect_gt(abs(stats::cor(spec[, best], baked$SOC)), 0.8)
    expect_lt(stats::var(spec[, best]), 1e-10)

    prepped <- withr::with_seed(1, suppressMessages(recipes::prep(
      transform |> step_select_cars(dplyr::matches("^spec[0-9]+$"), outcome = "SOC"),
      training = d
    )))

    step <- prepped$steps[[2]]

    expect_identical(step$cars$n_constant, 0L)
    expect_identical(step$cars$n_bands, ncol(spec))

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
    expect_match(conditionMessage(w), "had 1 band to select from, fewer than the 2 it needs, so it is kept",
                 fixed = TRUE)

    ## print() says it in the warning's words
    expect_output(print(step), "`step_select_cars()` had 1 band to select from, fewer than the 2 it needs, so it is kept",
                  fixed = TRUE)

  })

})


describe("step_select_cars()'s tidy() and print() (#109)", {

  it("tidies to its selectors before prep and the columns it removed after", {

    skip_if_not_installed("pls")

    d   <- cars_signal_data()
    rec <- cars_recipe(d)

    untrained <- recipes::tidy(rec, number = 1)

    expect_identical(untrained$terms, "dplyr::starts_with(\"wn_\")")
    expect_identical(untrained$id, rec$steps[[1]]$id)

    prepped <- withr::with_seed(1, suppressMessages(recipes::prep(rec, training = d)))
    trained <- recipes::tidy(prepped, number = 1)
    kept    <- prepped$steps[[1]]$selected_vars

    expect_setequal(trained$terms, setdiff(grep("^wn_", names(d), value = TRUE), kept))
    expect_false(any(kept %in% trained$terms))

  })

  it("prints one line per fact", {

    skip_if_not_installed("pls")

    d <- cars_signal_data()

    ## Forty bands, fewer than the 150 representatives: recorded and printed,
    ## not said on the console at every prep
    expect_no_message(step <- withr::with_seed(1, recipes::prep(cars_recipe(d), training = d))$steps[[1]])
    expect_false(step$cars$clustered)

    lines <- utils::capture.output(print(step))

    expect_length(lines, 4L)
    expect_identical(lines[2], "\u2022 Outcome column: SOC")
    expect_identical(lines[3], paste0("\u2022 ", length(step$selected_vars), " wavenumbers retained after CARS"))
    expect_identical(lines[4], "\u2022 Fewer bands than 150, so each band was its own representative")

  })

})
