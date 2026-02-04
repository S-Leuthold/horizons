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
## build_warmstart_grid() — grid construction
## =========================================================================

describe("build_warmstart_grid() - basic contract", {

  param_set   <- make_warmstart_param_set()
  best_params <- make_best_params()

  grid <- build_warmstart_grid(best_params, param_set, max_points = 25)

  it("returns a tibble", {

    expect_s3_class(grid, "tbl_df")

  })

  it("has columns matching the param_set names", {

    expect_true(all(param_set$name %in% names(grid)))

  })

  it("respects max_points", {

    expect_lte(nrow(grid), 25)

  })

  it("has at least 1 row", {

    expect_gte(nrow(grid), 1)

  })

})


describe("build_warmstart_grid() - integer params", {

  param_set   <- make_warmstart_param_set()
  best_params <- make_best_params(mtry = 10L, trees = 500L, min_n = 5L)

  grid <- build_warmstart_grid(best_params, param_set, max_points = 25)

  it("produces integer values for mtry", {

    expect_true(all(grid$mtry == as.integer(grid$mtry)))

  })

  it("produces integer values for trees", {

    expect_true(all(grid$trees == as.integer(grid$trees)))

  })

  it("produces integer values for min_n", {

    expect_true(all(grid$min_n == as.integer(grid$min_n)))

  })

})


describe("build_warmstart_grid() - range clamping", {

  param_set   <- make_warmstart_param_set()
  best_params <- make_best_params()

  grid <- build_warmstart_grid(best_params, param_set, max_points = 50)

  it("keeps mtry within valid range", {

    ## Extract the finalized mtry param from our param_set
    mtry_param <- param_set$object[[which(param_set$name == "mtry")]]
    expect_true(all(grid$mtry >= mtry_param$range$lower))
    expect_true(all(grid$mtry <= mtry_param$range$upper))

  })

  it("keeps min_n >= 1", {

    expect_true(all(grid$min_n >= 1))

  })

  it("keeps trees >= 1", {

    expect_true(all(grid$trees >= 1))

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

    ## learn_rate range is stored in log10 space; inverse to get original
    lr_param <- dials::learn_rate()
    orig_lower <- lr_param$trans$inverse(lr_param$range$lower)
    orig_upper <- lr_param$trans$inverse(lr_param$range$upper)
    expect_true(all(grid$learn_rate >= orig_lower))
    expect_true(all(grid$learn_rate <= orig_upper))

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
    feature_selection = "none",
    covariates        = NA_character_
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

  it("returns a list", {

    expect_true(is.list(result))

  })

  it("has required fields: tune_results, best_params, fallback_used", {

    expect_true("tune_results" %in% names(result))
    expect_true("best_params" %in% names(result))
    expect_true("fallback_used" %in% names(result))

  })

  it("best_params is a single-row tibble", {

    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)

  })

  it("fallback_used is logical", {

    expect_true(is.logical(result$fallback_used))

  })

  it("fallback_used is FALSE when valid best_params provided", {

    expect_false(result$fallback_used)

  })

})


describe("tune_warmstart_bayes() - fallback behavior", {

  ts <- make_tune_setup()

  it("still returns results when best_params is NULL (fallback grid)", {

    result <- tune_warmstart_bayes(
      workflow      = ts$wf,
      cv_resamples  = ts$folds,
      best_params   = NULL,
      param_set     = ts$param_set,
      bayesian_iter = 0L,
      metric_set    = ts$metric_set,
      allow_par     = FALSE
    )

    expect_true(is.list(result))
    expect_s3_class(result$best_params, "tbl_df")
    expect_equal(nrow(result$best_params), 1)
    expect_true(result$fallback_used)

  })

})
