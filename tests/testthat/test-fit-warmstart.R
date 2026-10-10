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

  result <- tune_warmstart_bayes(
    workflow      = ts$wf,
    cv_resamples  = ts$folds,
    best_params   = best_params,
    param_set     = ts$param_set,
    bayesian_iter = 0L,
    metric_set    = ts$metric_set,
    allow_par     = FALSE
  )

  it("is a list with tune_results, fallback_used and a single-row best_params tibble", {

    expect_true(is.list(result))
    expect_true("tune_results" %in% names(result))
    expect_true("best_params" %in% names(result))
    expect_true("fallback_used" %in% names(result))
    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)

  })

  it("fallback_used is FALSE when valid best_params provided", {

    expect_false(result$fallback_used)

  })

})


describe("tune_warmstart_bayes() - a Bayesian search that fails (#209)", {

  it("keeps the grid's choice and says why", {

    ts <- make_tune_setup()

    ## A starting grid of one point: tune_bayes() needs two, so it aborts.
    result <- suppressWarnings(tune_warmstart_bayes(
      workflow      = ts$wf,
      cv_resamples  = ts$folds,
      best_params   = tibble::tibble(mtry = 5L, trees = 100L, min_n = 5L),
      param_set     = ts$param_set,
      bayesian_iter = 2L,
      grid_size     = 1L,
      metric_set    = ts$metric_set,
      allow_par     = FALSE
    ))

    expect_true(result$bayes_failed)
    expect_type(result$bayes_error, "character")
    expect_equal(nrow(result$best_params), 1)

  })

})


describe("tune_warmstart_bayes() - fallback behavior", {

  ts <- make_tune_setup()

  it("still returns results when best_params is NULL (fallback grid)", {

    ## A three-point fallback grid: the fallback does not depend on its size
    result <- tune_warmstart_bayes(
      workflow      = ts$wf,
      cv_resamples  = ts$folds,
      best_params   = NULL,
      param_set     = ts$param_set,
      bayesian_iter = 0L,
      grid_size     = 3L,
      metric_set    = ts$metric_set,
      allow_par     = FALSE
    )

    expect_true(is.list(result))
    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)
    expect_true(result$fallback_used)

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

    ## tune's control seed and the model fits draw from the session's random
    ## stream, so pin it
    result <- withr::with_seed(42, tune_warmstart_bayes(
      workflow      = ts$wf,
      cv_resamples  = ts$folds,
      best_params   = best_params,
      param_set     = ts$param_set,
      bayesian_iter = budget,
      grid_size     = 5L,
      metric_set    = ts$metric_set,
      allow_par     = FALSE
    ))

    ## The Bayesian stage neither errored back to the grid results nor started
    ## from a fallback grid
    expect_false(result$bayes_failed)
    expect_false(result$fallback_used)

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
    warm  <- build_warmstart_grid(best_params, ts$param_set, max_points = 5L)
    expect_true(any(start$mtry == 2L & start$min_n == 5L))
    expect_equal(
      dplyr::arrange(start, dplyr::across(dplyr::all_of(tuned))),
      dplyr::arrange(warm[, tuned], dplyr::across(dplyr::all_of(tuned))),
      ignore_attr = TRUE
    )

    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)

  })

})
