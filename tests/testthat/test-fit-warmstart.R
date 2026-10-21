## ---------------------------------------------------------------------------
## Tests: fit-warmstart.R
## ---------------------------------------------------------------------------
## TDD: These tests define the contract for build_warmstart_grid() and
## tune_warmstart_bayes() BEFORE implementation.

## Helper: create a param_set with known structure for testing
## Uses rf params (mtry, trees, min_n) which cover integer, log-scale, and
## standard integer cases.
make_warmstart_param_set <- function(n_predictors = 20) {

  spec <- parsnip::rand_forest(
    mtry  = hardhat::tune(),
    trees = hardhat::tune(),
    min_n = hardhat::tune()
  ) %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("regression")

  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_variables(
      outcomes   = "SOC",
      predictors = paste0("wn_", seq_len(n_predictors))
    )

  ps <- workflows::extract_parameter_set_dials(wf)

  ## Finalize mtry upper bound (needs actual predictor count)
  ps <- dials::finalize(ps, tibble::tibble(
    !!!stats::setNames(
      as.list(rep(0, n_predictors)),
      paste0("wn_", seq_len(n_predictors))
    )
  ))

  ps

}

## Helper: create a param_set for xgboost (has log-scale params)
make_xgb_param_set <- function(n_predictors = 20) {

  spec <- parsnip::boost_tree(
    mtry           = hardhat::tune(),
    trees          = hardhat::tune(),
    min_n          = hardhat::tune(),
    learn_rate     = hardhat::tune(),
    tree_depth     = hardhat::tune()
  ) %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("regression")

  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_variables(
      outcomes   = "SOC",
      predictors = paste0("wn_", seq_len(n_predictors))
    )

  ps <- workflows::extract_parameter_set_dials(wf)

  dials::finalize(ps, tibble::tibble(
    !!!stats::setNames(
      as.list(rep(0, n_predictors)),
      paste0("wn_", seq_len(n_predictors))
    )
  ))

}

## Helper: create best_params tibble matching tune::select_best() output
make_best_params <- function(mtry = 10L, trees = 500L, min_n = 5L) {

  tibble::tibble(
    mtry  = as.integer(mtry),
    trees = as.integer(trees),
    min_n = as.integer(min_n)
  )

}

make_xgb_best_params <- function(mtry = 10L, trees = 200L, min_n = 5L,
                                  learn_rate = 0.01, tree_depth = 6L) {

  tibble::tibble(
    mtry       = as.integer(mtry),
    trees      = as.integer(trees),
    min_n      = as.integer(min_n),
    learn_rate = learn_rate,
    tree_depth = as.integer(tree_depth)
  )

}


## =========================================================================
## generate_candidates() — one parameter's candidates
## =========================================================================

describe("generate_candidates()", {

  ## The order is part of the result: expand.grid() lays the grid out in it,
  ## and a grid over max_points is sampled by row position.
  it("spreads around the best value in the parameter's own space, clamped, in order", {

    lr   <- dials::learn_rate()
    mtry <- dials::mtry(c(1L, 20L))
    prop <- dials::sample_prop()

    ## log10 space for a transformed parameter
    expect_equal(generate_candidates(0.01, lr), 10^(-2 + c(-0.5, -0.25, 0, 0.25, 0.5)))

    ## whole steps for an integer one, kept integer
    expect_identical(generate_candidates(10L, mtry), 8:12)
    expect_identical(generate_candidates(20L, mtry), 18:20)

    ## proportional steps for a continuous one, clamped and deduplicated
    expect_equal(generate_candidates(0.5, prop), 0.5 * c(0.8, 0.9, 1, 1.1, 1.2))
    expect_equal(generate_candidates(1, prop), c(0.8, 0.9, 1))
    expect_equal(generate_candidates(0.11, prop), c(0.1, 0.11, 0.121, 0.132))

  })

  it("spreads five points across the range when the best value is missing", {

    expect_equal(generate_candidates(NA, dials::learn_rate()), 10^seq(-10, -1, length.out = 5))
    expect_identical(generate_candidates(NA, dials::mtry(c(1L, 20L))), c(1L, 6L, 10L, 15L, 20L))
    expect_equal(generate_candidates(NA, dials::sample_prop()), seq(0.1, 1, length.out = 5))

  })

})


## =========================================================================
## build_warmstart_grid() — grid construction
## =========================================================================

describe("build_warmstart_grid() - basic contract", {

  param_set   <- make_warmstart_param_set()
  best_params <- make_best_params()

  grid <- build_warmstart_grid(best_params, param_set, max_points = 25)

  it("is a tibble with columns matching the param_set names", {

    expect_s3_class(grid, "tbl_df")
    expect_true(all(param_set$name %in% names(grid)))

  })

})


describe("build_warmstart_grid() - integer params", {

  param_set   <- make_warmstart_param_set()
  best_params <- make_best_params(mtry = 10L, trees = 500L, min_n = 5L)

  grid <- build_warmstart_grid(best_params, param_set, max_points = 25)

  it("produces integer values for mtry, trees and min_n", {

    expect_true(all(grid$mtry == as.integer(grid$mtry)))
    expect_true(all(grid$trees == as.integer(grid$trees)))
    expect_true(all(grid$min_n == as.integer(grid$min_n)))

  })

})


describe("build_warmstart_grid() - range clamping", {

  ## Best params on the range edges (mtry at its upper bound of 20, min_n at
  ## its lower bound of 2, trees at its lower bound of 1), so the +/- 2
  ## candidates around each fall outside the range unless they are clamped.
  param_set   <- make_warmstart_param_set()
  best_params <- make_best_params(mtry = 20L, trees = 1L, min_n = 2L)

  grid <- build_warmstart_grid(best_params, param_set, max_points = 50)

  param_range <- function(name) {
    param_set$object[[which(param_set$name == name)]]$range
  }

  it("keeps mtry, min_n and trees within their ranges from a best point on the bounds", {

    ## Extract the finalized mtry param from our param_set
    mtry_param <- param_set$object[[which(param_set$name == "mtry")]]
    expect_equal(best_params$mtry, mtry_param$range$upper)
    expect_true(all(grid$mtry >= mtry_param$range$lower))
    expect_true(all(grid$mtry <= mtry_param$range$upper))

    expect_equal(best_params$min_n, param_range("min_n")$lower)
    expect_true(all(grid$min_n >= 1))
    expect_true(all(grid$min_n >= param_range("min_n")$lower))

    expect_equal(best_params$trees, param_range("trees")$lower)
    expect_true(all(grid$trees >= 1))

  })

  it("clamps a best point outside the range into it, rather than adding it as found", {

    ## fit()'s range can be narrower than the one evaluate() chose from: a
    ## best mtry of 27 against fit()'s 1 to 20, and a learn_rate above its
    ## log10 range
    outside <- build_warmstart_grid(make_best_params(mtry = 27L), param_set, max_points = 50)

    expect_true(all(outside$mtry <= param_range("mtry")$upper))
    expect_true(any(outside$mtry == param_range("mtry")$upper))
    expect_type(outside$mtry, "integer")

    ## An integer parameter's clamped value is a whole number of integer type,
    ## whatever type the best point gave it; at max_points = 10 the grid
    ## overflows, so the best point's own row is bound to the sampled ones
    as_double <- build_warmstart_grid(tibble::tibble(mtry = 27, trees = 500, min_n = 5),
                                      param_set, max_points = 10)
    expect_type(as_double$mtry, "integer")

    xgb_set  <- make_xgb_param_set()
    lr_param <- xgb_set$object[[which(xgb_set$name == "learn_rate")]]
    lr_upper <- lr_param$trans$inverse(lr_param$range$upper)
    xgb_grid <- build_warmstart_grid(make_xgb_best_params(learn_rate = lr_upper * 10), xgb_set,
                                     max_points = 25)

    expect_true(all(xgb_grid$learn_rate <= lr_upper * (1 + 1e-8)))

  })

})


describe("build_warmstart_grid() - log-scale params (xgboost)", {

  param_set   <- make_xgb_param_set()
  best_params <- make_xgb_best_params()

  grid <- build_warmstart_grid(best_params, param_set, max_points = 25)

  it("produces strictly positive learn_rate values", {

    expect_true(all(grid$learn_rate > 0))

  })

  it("keeps learn_rate within dials range (original scale)", {

    ## learn_rate range is stored in log10 space; inverse to get original.
    ## Read from the param set, which carries the engine's tuning range and is
    ## the range the grid is clamped to.
    lr_param <- param_set$object[[which(param_set$name == "learn_rate")]]
    orig_lower <- lr_param$trans$inverse(lr_param$range$lower)
    orig_upper <- lr_param$trans$inverse(lr_param$range$upper)

    ## Best learn_rate at each bound in turn, so the candidates beyond it in
    ## log10 space fall outside the range unless they are clamped
    upper_params <- make_xgb_best_params(learn_rate = orig_upper)
    upper_grid <- build_warmstart_grid(upper_params, param_set, max_points = 25)

    expect_true(all(upper_grid$learn_rate >= orig_lower))
    expect_true(all(upper_grid$learn_rate <= orig_upper))

    lower_params <- make_xgb_best_params(learn_rate = orig_lower)
    lower_grid <- build_warmstart_grid(lower_params, param_set, max_points = 25)

    expect_true(all(lower_grid$learn_rate >= orig_lower))
    expect_true(all(lower_grid$learn_rate <= orig_upper))

  })

})


describe("build_warmstart_grid() - NULL/NA fallback", {

  param_set <- make_warmstart_param_set()

  it("returns a grid when best_params is NULL", {

    grid <- build_warmstart_grid(NULL, param_set, max_points = 25)
    expect_s3_class(grid, "tbl_df")
    expect_gte(nrow(grid), 1)
    expect_true(all(param_set$name %in% names(grid)))

  })

  it("returns a grid when best_params has NA values", {

    bad_params <- tibble::tibble(mtry = NA_integer_, trees = NA_integer_,
                                  min_n = NA_integer_)
    grid <- build_warmstart_grid(bad_params, param_set, max_points = 25)
    expect_s3_class(grid, "tbl_df")
    expect_gte(nrow(grid), 1)

  })

  it("returns a grid when best_params has wrong columns", {

    wrong_params <- tibble::tibble(alpha = 0.5, lambda = 0.01)
    grid <- build_warmstart_grid(wrong_params, param_set, max_points = 25)
    expect_s3_class(grid, "tbl_df")
    expect_gte(nrow(grid), 1)

  })

})


describe("build_warmstart_grid() - the caller's random stream (#51)", {

  it("samples an overflowing grid without moving the caller's random stream", {

    ## 125 combinations against max_points = 10, so the grid is sampled
    param_set   <- make_warmstart_param_set()
    best_params <- make_best_params()

    set.seed(9)
    expected <- stats::runif(3)

    set.seed(9)
    grid  <- build_warmstart_grid(best_params, param_set, max_points = 10)
    after <- stats::runif(3)

    expect_identical(nrow(grid), 10L)
    expect_identical(after, expected)

    ## The sample is the same whatever the caller's stream
    set.seed(10)
    expect_identical(build_warmstart_grid(best_params, param_set, max_points = 10), grid)

  })

})


describe("build_warmstart_grid() - max_points truncation", {

  param_set   <- make_warmstart_param_set()
  best_params <- make_best_params()

  it("caps grid at max_points when expansion would exceed it", {

    ## 5 values per param × 3 params = 125 combos > max_points = 10
    grid <- build_warmstart_grid(best_params, param_set, max_points = 10)
    expect_lte(nrow(grid), 10)

  })

  it("includes the best_params point in the grid", {

    grid <- build_warmstart_grid(best_params, param_set, max_points = 25)

    ## The exact best values should appear in the grid
    has_best <- any(
      grid$mtry == best_params$mtry &
      grid$trees == best_params$trees &
      grid$min_n == best_params$min_n
    )
    expect_true(has_best)

    ## A best value inside its range is kept exactly as given, not as its
    ## round trip through the log10 scale its candidates are spread on
    xgb_best <- make_xgb_best_params(learn_rate = 0.05)
    xgb_grid <- build_warmstart_grid(xgb_best, make_xgb_param_set(), max_points = 25)

    expect_identical(as.list(xgb_grid[1, names(xgb_best)]), as.list(xgb_best))

  })

  it("adds a best point its candidates miss, and still holds at most max_points", {

    ## fit() passes tune::select_best() output, which always lies on its own
    ## candidate lattice; a direct call to this exported function need not.
    ## mtry = 10.5 rounds to the candidates 8, 10 and 12, so the 75 combinations
    ## lack the best point. At max_points = 75 they are not sampled, and the
    ## best point bound to them makes 76 rows, one over.
    off_lattice <- make_best_params()
    off_lattice$mtry <- 10.5

    grid <- build_warmstart_grid(off_lattice, param_set, max_points = 75)

    expect_identical(nrow(grid), 75L)
    has_best <- grid$mtry == off_lattice$mtry &
      grid$trees == off_lattice$trees &
      grid$min_n == off_lattice$min_n
    expect_true(any(has_best))

  })

})


## =========================================================================
## tune_warmstart_bayes() — orchestration
## =========================================================================

## Helper: create minimal data + workflow for tune tests.
## Self-contained — does NOT depend on helpers from other test files.
make_tune_setup <- function(n = 40, n_wn = 10, seed = 42) {

  set.seed(seed)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  role_map <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  split <- suppressWarnings(rsample::initial_split(df, prop = 0.75, strata = "SOC"))
  train <- rsample::training(split)
  folds <- suppressWarnings(rsample::vfold_cv(train, v = 3, strata = "SOC"))

  config <- tibble::tibble(
    config_id         = "tune_test_001",
    model             = "rf",
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none"
  )

  recipe    <- build_recipe(config, train, role_map)
  spec      <- define_model_spec("rf")
  wf        <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)

  param_set <- workflows::extract_parameter_set_dials(wf)

  ## Finalize mtry from prepped data
  prepped   <- recipes::prep(recipe)
  baked     <- recipes::bake(prepped, new_data = NULL)
  pred_cols <- setdiff(names(baked), c("SOC", "sample_id"))
  param_set <- dials::finalize(param_set, baked[, pred_cols, drop = FALSE])

  list(
    folds      = folds,
    wf         = wf,
    param_set  = param_set,
    metric_set = yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  )

}


describe("tune_warmstart_bayes() - return contract", {

  ts <- make_tune_setup()

  best_params <- tibble::tibble(
    mtry  = 5L,
    trees = 100L,
    min_n = 5L
  )

  grid <- build_warmstart_grid(best_params, ts$param_set)

  result <- tune_warmstart_bayes(
    workflow      = ts$wf,
    cv_resamples  = ts$folds,
    initial_grid  = grid,
    param_set     = ts$param_set,
    bayesian_iter = 0L,
    metric_set    = ts$metric_set,
    allow_par     = FALSE
  )

  it("is a list with tune_results, a single-row best_params tibble, the grid it started from and the dropped count", {

    expect_true(is.list(result))
    expect_true("tune_results" %in% names(result))
    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)
    expect_true(".config" %in% names(result$best_params))

    ## The grid comes back, for the member to read its notes through (#109)
    expect_identical(result$grid, grid)

    ## No Bayesian stage, so no candidate it proposed was dropped
    expect_identical(result$n_dropped, 0L)

  })

  it("starts from evaluate()'s parameters when they are usable", {

    expect_true(usable_best_params(best_params, ts$param_set))

  })

})


describe("tune_warmstart_bayes() - a Bayesian search that fails (#209)", {

  it("keeps the grid's choice and says why, with a substitution warning", {

    ts <- make_tune_setup()

    ## A starting grid of one point: tune_bayes() needs two, so it aborts.
    ## Outside a member's unit the warning is an ordinary one of its topic.
    w <- expect_warning(
      result <- keep_only_warning(tune_warmstart_bayes(
        workflow      = ts$wf,
        cv_resamples  = ts$folds,
        initial_grid  = build_warmstart_grid(tibble::tibble(mtry = 5L, trees = 100L, min_n = 5L),
                                             ts$param_set, max_points = 1L),
        param_set     = ts$param_set,
        bayesian_iter = 2L,
        metric_set    = ts$metric_set,
        allow_par     = FALSE
      ), "horizons_warning_substitution"),
      class = "horizons_warning_substitution"
    )

    expect_match(conditionMessage(w),
                 "The warm-start Bayesian search failed, so the hyperparameters were chosen from its starting grid alone",
                 fixed = TRUE)
    expect_equal(nrow(result$best_params), 1)

  })

  it("keeps the grid's choice and says why when the search returns no iteration", {

    ts <- make_tune_setup()

    ## What tune_bayes() returns when every candidate fails: the starting
    ## grid's results alone
    local_mocked_bindings(tune_bayes = function(object, resamples, initial, ...) initial,
                          .package = "tune")

    w <- expect_warning(
      result <- tune_warmstart_bayes(
        workflow      = ts$wf,
        cv_resamples  = ts$folds,
        initial_grid  = build_warmstart_grid(tibble::tibble(mtry = 5L, trees = 100L, min_n = 5L),
                                             ts$param_set, max_points = 3L),
        param_set     = ts$param_set,
        bayesian_iter = 2L,
        metric_set    = ts$metric_set,
        allow_par     = FALSE
      ),
      class = "horizons_warning_substitution"
    )

    expect_match(conditionMessage(w),
                 "The warm-start Bayesian search produced no results, so the hyperparameters were chosen from its starting grid alone.",
                 fixed = TRUE)
    expect_equal(nrow(result$best_params), 1)

  })

  it("does not fall back under debug, stopping with the search's error", {

    ts <- make_tune_setup()

    expect_error(
      tune_warmstart_bayes(
        workflow      = ts$wf,
        cv_resamples  = ts$folds,
        initial_grid  = build_warmstart_grid(tibble::tibble(mtry = 5L, trees = 100L, min_n = 5L),
                                             ts$param_set, max_points = 1L),
        param_set     = ts$param_set,
        bayesian_iter = 2L,
        metric_set    = ts$metric_set,
        allow_par     = FALSE,
        debug         = TRUE
      ),
      "The GP model requires 2+ initial points", fixed = TRUE
    )

  })

})


describe("tune_warmstart_bayes() - a starting grid that errors", {

  it("raises the cause, for the member's unit to record", {

    ts <- make_tune_setup()

    local_mocked_bindings(tune_grid = function(...) stop("the grid broke"), .package = "tune")

    expect_error(
      tune_warmstart_bayes(
        workflow      = ts$wf,
        cv_resamples  = ts$folds,
        initial_grid  = build_warmstart_grid(tibble::tibble(mtry = 5L, trees = 100L, min_n = 5L),
                                             ts$param_set, max_points = 3L),
        param_set     = ts$param_set,
        bayesian_iter = 0L,
        metric_set    = ts$metric_set,
        allow_par     = FALSE
      ),
      "the grid broke", fixed = TRUE
    )

  })

})


describe("tune_warmstart_bayes() - fallback behavior", {

  ts <- make_tune_setup()

  it("still returns results from the space-filling grid when best_params is NULL", {

    ## A three-point fallback grid: the fallback does not depend on its size
    expect_false(usable_best_params(NULL, ts$param_set))

    result <- tune_warmstart_bayes(
      workflow      = ts$wf,
      cv_resamples  = ts$folds,
      initial_grid  = build_warmstart_grid(NULL, ts$param_set, max_points = 3L),
      param_set     = ts$param_set,
      bayesian_iter = 0L,
      metric_set    = ts$metric_set,
      allow_par     = FALSE
    )

    expect_true(is.list(result))
    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)

  })

})


describe("tune_warmstart_bayes() - Bayesian stage", {

  it("runs Bayesian iterations after the grid when bayesian_iter > 0", {

    ts <- make_tune_setup()

    best_params <- tibble::tibble(
      mtry  = 2L,
      trees = 100L,
      min_n = 5L
    )

    budget <- 2L
    warm   <- build_warmstart_grid(best_params, ts$param_set, max_points = 5L)

    ## tune's control seed and the model fits draw from the session's random
    ## stream, so pin it. A search that ran raises no substitution warning.
    expect_no_warning(
      result <- withr::with_seed(42, tune_warmstart_bayes(
        workflow      = ts$wf,
        cv_resamples  = ts$folds,
        initial_grid  = warm,
        param_set     = ts$param_set,
        bayesian_iter = budget,
        metric_set    = ts$metric_set,
        allow_par     = FALSE
      )),
      class = "horizons_warning_substitution"
    )

    ## The warm start was usable, so the re-tune did not fall back to a
    ## space-filling grid
    expect_true(usable_best_params(best_params, ts$param_set))

    ## Its results replaced the grid's: tune_bayes() returns iteration results,
    ## with .iter 0 for the starting grid and 1 onwards for each iteration. The
    ## whole budget runs, since it is well under the no-improvement limit.
    expect_s3_class(result$tune_results, "iteration_results")
    metrics <- tune::collect_metrics(result$tune_results)
    expect_true(".iter" %in% names(metrics))
    expect_equal(max(metrics$.iter), budget)

    ## The Bayesian stage started from the warm-start grid, which carries the
    ## best point, not from a starting design of its own
    tuned <- ts$param_set$name
    start <- dplyr::distinct(metrics[metrics$.iter == 0, tuned])
    expect_true(any(start$mtry == 2L & start$min_n == 5L))
    expect_equal(
      dplyr::arrange(start, dplyr::across(dplyr::all_of(tuned))),
      dplyr::arrange(warm[, tuned], dplyr::across(dplyr::all_of(tuned))),
      ignore_attr = TRUE
    )

    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)

  })

  it("does not depend on the caller's random stream (#151)", {

    ## An elastic net with an interior optimum, which glmnet fits
    ## deterministically, so only the search's own draws can differ. Without
    ## the re-pin the search reads whatever stream the grid stage left, which
    ## differs between the sequential and parallel paths; here three caller
    ## seeds stand in for that, and choose three different points.
    dat <- withr::with_seed(11, {
      x <- matrix(stats::rnorm(120 * 30), 120)
      colnames(x) <- paste0("x", 1:30)
      d <- tibble::as_tibble(x)
      d$y <- x[, 1] + 0.5 * x[, 2] + 0.3 * x[, 3] + stats::rnorm(120)
      d
    })
    folds <- withr::with_seed(12, rsample::vfold_cv(dat, v = 5))

    wf <- workflows::workflow(
      y ~ .,
      parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) |>
        parsnip::set_engine("glmnet")
    )
    ps   <- workflows::extract_parameter_set_dials(wf)
    grid <- build_warmstart_grid(NULL, ps, max_points = 3L)
    ms   <- tuning_metric_set("none", metrics = c("rmse", "rsq"))

    run <- function(caller_seed) {

      withr::local_seed(caller_seed)
      r <- tune_warmstart_bayes(wf, folds, grid, ps, bayesian_iter = 3L,
                                metric_set = ms, seed = 42L)
      scored <- tune::collect_metrics(r$tune_results)

      list(best     = r$best_params[c("penalty", "mixture")],
           proposed = scored[scored$.metric == "rmse" & scored$.iter > 0, c("penalty", "mixture")])

    }

    runs <- lapply(1:3, run)

    expect_equal(nrow(runs[[1]]$proposed), 3L)
    expect_identical(runs[[2]], runs[[1]])
    expect_identical(runs[[3]], runs[[1]])

  })

})


describe("bayes_search()", {

  it("caps the line tune logged for a candidate it dropped, as another package's text, when debug stops on it", {

    long <- paste0("All models failed for: ", strrep("penalty=1e-04, ", 70))

    local_mocked_bindings(tune_bayes = function(...) {
      message(long)
      NULL
    }, .package = "tune")

    err <- expect_error(
      bayes_search(workflow = NULL, resamples = NULL, initial = NULL, iter = 1L,
                   param_info = NULL, metrics = NULL, debug = TRUE),
      class = "horizons_error_run"
    )

    msg <- conditionMessage(err)

    expect_match(msg, "tune keeps no record of why: All models failed for: penalty", fixed = TRUE)
    expect_match(msg, "...", fixed = TRUE)
    expect_lt(nchar(msg), 500)

  })

})
