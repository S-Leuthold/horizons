## ---------------------------------------------------------------------------
## Tests: the randomness rule, through the verbs
## ---------------------------------------------------------------------------
##
## Every unit of work is seeded from the run's seed and its own name
## (with_unit_seed(); test-utils-rng.R tests the helpers alone), and no verb,
## predict() included, changes the caller's random state. Six properties
## follow from it, each tested below: the order of the configurations and
## which others are present change no configuration's results; neither does
## the parallel backend; the caller's state is left alone and moves no
## result; set.seed() governs a run that is given no seed; and a unit re-run
## from its recomputed seed is identical. Results are compared as scores,
## chosen parameters and predictions, never as stored workflows, whose recipe
## step ids follow the order of work.


## The caller's stream and all three kinds as found after `run()`, from a
## stream under kinds no unit uses. Then a session with no stream: a verb
## that resolves a seed (`resolves_seed`) gives it one before it saves the
## caller's state, as R's own random functions would, so two unseeded calls
## in it take the same seed, read from what `run()` returns; anything else
## leaves an absent stream absent.
expect_rng_left_alone <- function(run, resolves_seed = FALSE) {

  ## The session's stream and kinds as they were, after the test: restoring a
  ## stream that was absent leaves the kinds as set, so they are put back too
  withr::local_preserve_seed()
  kinds <- RNGkind()
  withr::defer(suppressWarnings(RNGkind(kinds[1], kinds[2], kinds[3])))

  ## suppressWarnings(): R's note that the Rounding sampler is not uniform
  suppressWarnings(RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rounding"))
  set.seed(11)
  before <- get(".Random.seed", envir = globalenv())

  run()

  expect_identical(RNGkind(), c("L'Ecuyer-CMRG", "Box-Muller", "Rounding"))
  expect_identical(get(".Random.seed", envir = globalenv()), before)

  rm(".Random.seed", envir = globalenv())

  first <- run()

  if (!resolves_seed) {

    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    return(invisible())

  }

  expect_true(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  expect_type(recorded_seed(first), "integer")
  expect_identical(recorded_seed(run()), recorded_seed(first))

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

  it("the data verbs, which draw nothing", {

    wn   <- seq(4000, 600, by = -8)
    spec <- withr::with_seed(3, tibble::as_tibble(
      matrix(stats::runif(12 * length(wn), 0.2, 0.8), 12, dimnames = list(NULL, wn))
    ))
    spec$sample_id <- sprintf("P_S%02d_%d", rep(1:6, each = 2), 1:2)
    spec$filename  <- spec$sample_id

    response <- tibble::tibble(sample_id = sprintf("S%02d", 1:6), SOC = c(1.2, 2.5, 3.1, 0.8, 1.9, 2.2))

    expect_rng_left_alone(function() {
      utils::capture.output(suppressWarnings({
        hd <- spectra(spec)
        hd <- parse_ids(hd, format = "{project}_{sampleid}_{rep}")
        hd <- average(hd, by = "sample_id", verbose = FALSE)
        hd <- standardize(hd)
        hd <- add_response(hd, response, variable = "SOC")
        hd <- configure(hd, outcome = "SOC", models = "rf")
        validate(hd)
      }))
    })

  })

  ## Unseeded, so evaluate() and select_training() draw their seed from the
  ## session; fit() and ensemble() take the one their object records
  it("evaluate()", {

    obj <- make_eval_object(n = 40, n_configs = 1)

    expect_rng_left_alone(resolves_seed = TRUE, function() {
      suppressWarnings(evaluate(obj, prune = FALSE, control = quietly))
    })

  })

  it("fit()", {

    obj <- make_fit_object(n = 60, n_configs = 1)
    obj$config$tuning$final_bayesian_iter <- 0L

    expect_rng_left_alone(resolves_seed = TRUE, function() {
      suppressWarnings(fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                           control = quietly))
    })

  })

  it("ensemble()", {

    fitted <- rng_fitted()

    expect_rng_left_alone(resolves_seed = TRUE, function() {
      suppressWarnings(ensemble(fitted, method = "weighted", optimize = FALSE,
                                compute_uq = TRUE, control = quietly))
    })

  })

  it("select_training(), with its targets clustered", {

    fx <- select_fixture(n_pool = 100)

    expect_rng_left_alone(resolves_seed = TRUE, function() {
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


## ---------------------------------------------------------------------------
## The properties' fixture
## ---------------------------------------------------------------------------
## 80 rows by 40 wavenumbers with signal in three bands. rf without
## selection and plsr with CARS: an engine given the stage's seed, and a
## selection step that draws a data-dependent amount. Three folds, a grid of
## three, two Bayesian iterations in evaluate() and one in fit(), so every
## stage runs; UQ and AD on with their floors lowered, so the forest and the
## degradation check run; an xgb ensemble, so the meta-learner's engine seed
## and CV+ run. A third configuration, elastic_net with PCA, joins it for the
## presence property.

rng_eval_control <- control_horizons(verbose = FALSE)
rng_fit_control  <- control_horizons(verbose = FALSE, min_calib_rows = 10L, min_ad_rows = 10L,
                                     min_degradation_rows = 10L)
rng_ens_control  <- control_horizons(verbose = FALSE, min_calib_rows = 10L)

rng_object <- function(configs = c("rf", "plsr_cars"), bayesian_iter = 2L, final_bayesian_iter = 1L) {

  ## The data under R's default kinds whatever the session's, so a test that
  ## changes them changes the generator the verbs meet and not their data
  wn <- paste0("wn_", seq(4000, by = -4, length.out = 40))
  d  <- withr::with_seed(11, .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
                         .rng_sample_kind = "Rejection", {
    x   <- matrix(stats::rnorm(80 * 40), 80, dimnames = list(NULL, wn))
    out <- tibble::as_tibble(x)
    out$SOC <- 10 + 1.5 * x[, 5] + x[, 12] - 0.8 * x[, 30] + stats::rnorm(80, sd = 0.5)
    out
  })
  d$sample_id <- sprintf("S%03d", seq_len(80))

  obj <- new_horizons_data(
    analysis = d,
    role_map = tibble::tibble(variable = c(wn, "SOC", "sample_id"),
                              role     = c(rep("predictor", 40), "outcome", "id"))
  )

  spec <- list(rf        = c("rf", "none"),
               plsr_cars = c("plsr", "cars"),
               enet_pca  = c("elastic_net", "pca"))[configs]
  model <- unname(vapply(spec, `[[`, "", 1))
  fs    <- unname(vapply(spec, `[[`, "", 2))

  obj$config$configs <- tibble::tibble(
    config_id         = unname(mapply(generate_config_id, model, "raw", "none", fs)),
    model             = model,
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = fs
  )
  obj$config$n_configs     <- length(configs)
  obj$config$tuning        <- list(cv_folds = 3L, grid_size = 3L, bayesian_iter = bayesian_iter,
                                   final_bayesian_iter = final_bayesian_iter)
  obj$config$recipe        <- list(sg_window = DEFAULT_SG_WINDOW, pca_threshold = DEFAULT_PCA_THRESHOLD)
  obj$config$outcome_range <- DEFAULT_OUTCOME_RANGE
  obj$validation$passed    <- TRUE

  obj

}

rng_id <- function(which) rng_object(which)$config$configs$config_id

rng_evaluate <- function(obj, seed = 307L, control = rng_eval_control) {
  suppressMessages(suppressWarnings(evaluate(obj, prune = FALSE, seed = seed, control = control)))
}

rng_fit <- function(ev, ...) {
  suppressMessages(suppressWarnings(fit(ev, n_best = nrow(ev$config$configs), control = rng_fit_control, ...)))
}

## The configurations' results in evaluate(), by config_id
eval_rows <- function(ev, ids = ev$evaluation$results$config_id) {
  r <- ev$evaluation$results
  r <- r[r$config_id %in% ids, c("config_id", "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                                  "cv_rmse", "cv_rpd", "best_params")]
  r[order(r$config_id), ]
}

## The members' results in fit(), by config_id: their scores and parameters,
## their out-of-fold predictions, and their predictions on new spectra with
## intervals and the applicability domain
fit_rows <- function(ft, ids = names(ft$models$workflows)) {
  r   <- ft$models$results
  r   <- r[r$config_id %in% ids, c("config_id", "rmse", "rpd", "cv_rmse_mean", "cv_rpd_mean",
                                    "best_params", "degraded")]
  oof <- ft$models$cv_predictions
  oof <- oof[oof$config_id %in% ids, c("config_id", "sample_id", ".fold", ".pred")]
  new <- suppressWarnings(predict(ft, rng_spectra(), config = "all"))
  new <- new[new$config_id %in% ids, ]
  list(results     = r[order(r$config_id), ],
       oof         = oof[order(oof$config_id, oof$sample_id), ],
       predictions = new[order(new$config_id, new$sample_id), ])
}

## The ensemble's results: its weights, out-of-fold predictions, CV+
## calibration rows, and predictions on new spectra with intervals
ens_rows <- function(ens) {
  list(weights     = ens$ensemble$weights,
       oof         = ens$ensemble$oof_predictions,
       calib       = ens$ensemble$uq$calib,
       predictions = suppressWarnings(predict(ens, rng_spectra())))
}

rng_spectra <- function() {
  wn <- paste0("wn_", seq(4000, by = -4, length.out = 40))
  x  <- withr::with_seed(12, matrix(stats::rnorm(10 * 40), 10, dimnames = list(NULL, wn)),
                         .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
                         .rng_sample_kind = "Rejection")
  out <- tibble::as_tibble(x)
  out$sample_id <- sprintf("N%02d", 1:10)
  out
}

## The two-configuration run at seed 307, through the ensemble, built once
rng_base <- function() {
  memo_fixture("rng_base", function() {
    ev  <- rng_evaluate(rng_object())
    ft  <- rng_fit(ev)
    ens <- suppressMessages(suppressWarnings(
      ensemble(ft, method = "xgb", optimize = FALSE, control = rng_ens_control)
    ))
    list(ev = ev, ft = ft, ens = ens)
  })
}


## ---------------------------------------------------------------------------
## 1. Order
## ---------------------------------------------------------------------------

describe("the order of the configurations", {

  it("changes no configuration's results in evaluate() or fit()", {

    skip_unless_slow_tier()

    base <- rng_base()
    ev   <- rng_evaluate(rng_object(c("plsr_cars", "rf")))
    ft   <- rng_fit(ev)
    ens  <- suppressWarnings(ensemble(ft, method = "xgb", optimize = FALSE, control = rng_ens_control))

    expect_identical(eval_rows(ev), eval_rows(base$ev))
    expect_identical(fit_rows(ft), fit_rows(base$ft))
    expect_identical(ens_rows(ens), ens_rows(base$ens))

  })

})


## ---------------------------------------------------------------------------
## 2. Presence
## ---------------------------------------------------------------------------

describe("the configurations present", {

  it("change no other configuration's results when one is removed or another added", {

    skip_unless_slow_tier()

    base <- rng_base()
    kept <- rng_id("plsr_cars")
    both <- rng_id(c("rf", "plsr_cars"))

    alone <- rng_evaluate(rng_object("plsr_cars"))
    three <- rng_evaluate(rng_object(c("rf", "plsr_cars", "enet_pca")))

    expect_identical(eval_rows(alone), eval_rows(base$ev, kept))
    expect_identical(eval_rows(three, both), eval_rows(base$ev))

    expect_identical(fit_rows(rng_fit(alone)), fit_rows(base$ft, kept))
    expect_identical(fit_rows(rng_fit(three), both), fit_rows(base$ft))

  })

})


## ---------------------------------------------------------------------------
## 3. Backend
## ---------------------------------------------------------------------------
## What horizons dispatches today: the configs axis on a future plan, and
## tune's folds on the resamples axis, which tune runs on mirai when daemons
## are set. Workers load the installed package (R_LIBS on the build).

describe("the parallel backend", {

  it("changes no result on the configs axis, at 2 or 3 workers, and leaves the caller's stream alone", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    base <- rng_base()

    for (workers in 2:3) {

      local_plan(future::multisession, workers = workers)
      withr::local_seed(5)
      before <- .Random.seed

      ev <- rng_evaluate(rng_object(), control = control_horizons(
        verbose = FALSE, allow_par = TRUE, parallelize_over = "configs", output_dir = withr::local_tempdir()
      ))

      expect_identical(ev$evaluation$parallelize_over, "configs", info = workers)
      expect_identical(eval_rows(ev), eval_rows(base$ev), info = workers)
      expect_identical(.Random.seed, before, info = workers)

    }

  })

  it("changes no result with tune's folds on mirai, at 2 or 3 daemons, with or without daemons(seed =)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("mirai")

    base <- rng_base()

    ## horizons reads the future plan for its own backend check, which a
    ## mirai-only session would fail; tune then finds mirai's daemons
    local_mocked_bindings(
      check_parallel_backend = function(...) TRUE,
      registered_workers     = function() 2L,
      registered_plan_label  = function() "mirai"
    )

    withr::defer(mirai::daemons(0))

    for (setting in list(list(n = 2L, seed = NULL), list(n = 3L, seed = NULL),
                         list(n = 2L, seed = 1L),   list(n = 3L, seed = 1L))) {

      ## autoexit = NA: a daemon finishes exiting when the reset drops its
      ## connection, where the default kills it after 200 ms, cutting off the
      ## trace file a daemon writes on exit under covr
      mirai::daemons(setting$n, seed = setting$seed, autoexit = NA)

      withr::local_seed(5)
      before <- .Random.seed

      ev <- rng_evaluate(rng_object(), control = control_horizons(
        verbose = FALSE, allow_par = TRUE, parallelize_over = "resamples"
      ))

      label <- paste(setting$n, "daemons, seed", format(setting$seed))
      expect_identical(ev$evaluation$parallelize_over, "resamples", info = label)
      expect_identical(eval_rows(ev), eval_rows(base$ev), info = label)
      expect_identical(.Random.seed, before, info = label)

      ## The folds ran on the daemons, not in this process
      expect_gt(mirai::info()[["completed"]], 0L, label = paste("the tasks completed with", label))

      mirai::daemons(0)

    }

  })

})


## ---------------------------------------------------------------------------
## 4. The caller's state (the verbs' own tests are above)
## ---------------------------------------------------------------------------

describe("the session's generator", {

  it("moves no result: the same under L'Ecuyer-CMRG and under RNGversion(\"3.5.0\")", {

    skip_unless_slow_tier()

    base <- rng_base()

    withr::local_preserve_seed()
    kinds <- RNGkind()
    withr::defer(suppressWarnings(RNGkind(kinds[1], kinds[2], kinds[3])))

    for (kind in c("L'Ecuyer-CMRG", "3.5.0")) {


      ## suppressWarnings(): R's note that the 3.5.0 sampler is not uniform
      if (kind == "3.5.0") suppressWarnings(RNGversion("3.5.0")) else RNGkind(kind)
      set.seed(1)

      ev <- rng_evaluate(rng_object())
      ft <- rng_fit(ev)

      expect_identical(eval_rows(ev), eval_rows(base$ev), info = kind)
      expect_identical(fit_rows(ft), fit_rows(base$ft), info = kind)

      RNGkind("default", "default", "default")

    }

  })

})


## ---------------------------------------------------------------------------
## 5. set.seed() governs
## ---------------------------------------------------------------------------
## One configuration, grid only: cheap enough for the fast tier.

describe("set.seed()", {

  cheap <- function() rng_object("rf", bayesian_iter = 0L, final_bayesian_iter = 0L)

  unseeded <- function(session_seed) {
    withr::with_seed(session_seed, suppressWarnings(evaluate(cheap(), prune = FALSE, control = rng_eval_control)))
  }

  it("chooses an unseeded run's seed: the same twice, and 1 and 2 differ", {

    one   <- unseeded(1)
    again <- unseeded(1)
    two   <- unseeded(2)

    expect_identical(eval_rows(again), eval_rows(one))
    expect_identical(again$evaluation$seed, one$evaluation$seed)
    expect_false(identical(two$evaluation$seed, one$evaluation$seed))
    expect_false(identical(eval_rows(two), eval_rows(one)))

  })

  it("is overridden by an explicit seed", {

    at <- function(session_seed) {
      withr::with_seed(session_seed, rng_evaluate(cheap(), seed = 5L))
    }

    expect_identical(eval_rows(at(2)), eval_rows(at(1)))
    expect_identical(at(1)$evaluation$seed, 5L)

  })

  it("prints the seed it drew, which repeats the run", {

    loud <- control_horizons(verbose = TRUE)
    tree <- withr::with_seed(3, utils::capture.output(
      ev <- suppressWarnings(evaluate(cheap(), prune = FALSE, control = loud))
    ))

    line  <- grep("Seed: [0-9]+, drawn from the session", tree, value = TRUE)
    drawn <- as.integer(sub("^.*Seed: ([0-9]+),.*$", "\\1", line))

    expect_length(line, 1L)
    expect_match(line, paste0("`seed = ", drawn, "` repeats this run"), fixed = TRUE)
    expect_identical(drawn, ev$evaluation$seed)
    expect_identical(eval_rows(rng_evaluate(cheap(), seed = drawn)), eval_rows(ev))

    ## A seed given is not printed
    given <- utils::capture.output(suppressWarnings(evaluate(cheap(), prune = FALSE, seed = 5L, control = loud)))
    expect_false(any(grepl("Seed:", given, fixed = TRUE)))

  })

  it("is not read by fit() or ensemble() on an object that records a seed; a seed given there overrides it and is recorded", {

    ev <- rng_evaluate(cheap(), seed = 7L)

    taken <- withr::with_seed(1, rng_fit(ev, compute_uq = FALSE, compute_ad = FALSE))
    again <- withr::with_seed(2, rng_fit(ev, compute_uq = FALSE, compute_ad = FALSE))
    given <- rng_fit(ev, compute_uq = FALSE, compute_ad = FALSE, seed = 8L)

    expect_identical(taken$models$seed, 7L)
    expect_identical(taken$models$cv_predictions, again$models$cv_predictions)
    expect_identical(given$models$seed, 8L)
    expect_false(identical(given$models$cv_predictions, taken$models$cv_predictions))

    ## A re-fit takes the most recent seed, the one given to the fit
    expect_identical(rng_fit(given, compute_uq = FALSE, compute_ad = FALSE)$models$seed, 8L)

    ## ensemble() takes the fit's; one given overrides it and moves the CV+
    ## partition
    fitted <- ens_fitted()
    build  <- function(...) {
      suppressWarnings(ensemble(fitted, method = "weighted", optimize = FALSE, control = rng_ens_control, ...))$ensemble
    }

    taken_ens <- withr::with_seed(1, build())
    given_ens <- build(seed = 9L)

    expect_identical(taken_ens$seed, fitted$models$seed)
    expect_identical(given_ens$seed, 9L)
    folds <- function(ens) split(ens$uq$calib$.row, ens$uq$calib$fold)
    expect_false(identical(folds(given_ens), folds(taken_ens)))

  })

})


## ---------------------------------------------------------------------------
## 6. A unit re-run from its recomputed seed
## ---------------------------------------------------------------------------

describe("a unit re-run from its recomputed seed", {

  it("draws evaluate()'s split again", {

    skip_unless_slow_tier()

    ev   <- rng_base()$ev
    rows <- rng_object()$data$analysis

    split <- with_unit_seed(ev$evaluation$seed, "evaluate/split", rsample::initial_split(
      rows, prop = SPLIT_PROP, strata = "SOC", breaks = STRATA_BREAKS, pool = STRATA_POOL
    ))

    expect_identical(split$in_id, ev$evaluation$split$in_id)

  })

  it("regrows a member's quantile forest, its trees and node values", {

    ## One member, grid only; the forest's inputs are captured as fit()
    ## passes them
    ev <- rng_evaluate(rng_object("rf", bayesian_iter = 0L, final_bayesian_iter = 0L), seed = 7L)

    grown <- NULL
    real  <- fit_quantile_forest
    local_mocked_bindings(fit_quantile_forest = function(features, residuals, seed = NULL) {
      grown <<- list(features = features, residuals = residuals, seed = seed)
      real(features, residuals, seed = seed)
    })

    ft <- rng_fit(ev)
    id <- names(ft$models$workflows)

    unit    <- c("fit", id, "forest")
    regrown <- with_unit_seed(7L, unit, fit_quantile_forest(grown$features, grown$residuals,
                                                            seed = unit_seed(7L, unit)))

    ## ranger's seed fixes the trees; the stage's stream fixes the node values
    ## quantreg stores beside them, so both are compared
    stored <- ft$models$uq[[id]]$quantile_model
    expect_identical(grown$seed, unit_seed(7L, unit))
    expect_identical(regrown$forest, stored$forest)
    expect_identical(regrown$random.node.values, stored$random.node.values)

  })

})
