## ---------------------------------------------------------------------------
## Tests: evaluate_single_config()
## ---------------------------------------------------------------------------

## Helper: create minimal data + split + folds + role_map for evaluation
make_eval_setup <- function(n = 40, n_wn = 10, covariates = NULL) {

  set.seed(42)

  ## Spectral data: n samples x n_wn wavelengths
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal from first 3 predictors
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Optionally add covariates
  if (!is.null(covariates)) {

    for (cov in covariates) {
      df[[cov]] <- runif(n, 0, 100)
      roles <- rbind(roles, tibble::tibble(variable = cov, role = "covariate"))
    }

  }

  ## Create split and folds (suppress stratification warnings for tiny data)
  split <- suppressWarnings(rsample::initial_split(df, prop = 0.75, strata = "SOC"))
  train <- rsample::training(split)
  folds <- suppressWarnings(rsample::vfold_cv(train, v = 3, strata = "SOC"))

  list(data = df, split = split, folds = folds, role_map = roles, train_data = train)

}

## Helper: create a config row (matching config$configs schema)
make_eval_config <- function(model             = "rf",
                             transformation    = "none",
                             preprocessing     = "raw",
                             feature_selection = "none",
                             covariates        = NA_character_,
                             config_id         = "test_cfg_001") {

  tibble::tibble(
    config_id         = config_id,
    model             = model,
    transformation    = transformation,
    preprocessing     = preprocessing,
    feature_selection = feature_selection,
    covariates        = covariates
  )

}

## Expected columns in the result tibble
EXPECTED_RESULT_COLS <- c(
  "config_id", "status", "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
  "best_params", "error_message", "warnings", "runtime_secs"
)

## =========================================================================
## Success path (single model fit shared across tests)
## =========================================================================

describe("evaluate_single_config() - success path", {

  setup  <- make_eval_setup()
  config <- make_eval_config()

  result <- evaluate_single_config(
    config_row    = config,
    split         = setup$split,
    cv_folds      = setup$folds,
    role_map      = setup$role_map,
    grid_size     = 2,
    bayesian_iter = 0,
    seed          = 42L
  )

  it("returns a single-row tibble", {

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1)

  })

  it("has all expected columns", {

    expect_true(all(EXPECTED_RESULT_COLS %in% names(result)))

  })

  it("sets status to 'success'", {

    expect_equal(result$status, "success")

  })

  it("has non-NA values for all six metrics", {

    expect_false(is.na(result$rmse))
    expect_false(is.na(result$rrmse))
    expect_false(is.na(result$rsq))
    expect_false(is.na(result$ccc))
    expect_false(is.na(result$rpd))
    expect_false(is.na(result$mae))

  })

  it("stores best_params as a list", {

    expect_true(is.list(result$best_params))
    expect_false(is.null(result$best_params[[1]]))

  })

  it("has NA error_message on success", {

    expect_true(is.na(result$error_message))

  })

  it("records positive runtime", {

    expect_true(result$runtime_secs > 0)

  })

  it("preserves the config_id", {

    expect_equal(result$config_id, "test_cfg_001")

  })

  it("records the six cross-validated means at the selected hyperparameters (#50)", {

    cv_cols <- paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))

    expect_true(all(cv_cols %in% names(result)))

    ## rmse / rpd are always defined; rsq / ccc may be NA on a degenerate fold
    expect_true(is.finite(result$cv_rmse))
    expect_true(is.finite(result$cv_rpd))

    ## The CV panel and the test panel are different quantities; if they were
    ## ever identical the lookup would be reading the wrong thing.
    expect_false(isTRUE(all.equal(result$cv_rmse, result$rmse)))

  })

  it("keeps .config on best_params so the CV panel can be looked up", {

    expect_true(".config" %in% names(result$best_params[[1]]))

  })

})

## =========================================================================
## cv_panel_at()
## =========================================================================

describe("cv_panel_at()", {

  ## A real tune_grid() result on a tiny workflow, so the panel lookup is
  ## checked against tune's own collect_metrics() rather than a mock.
  set.seed(4903)
  d <- tibble::tibble(x1 = stats::rnorm(40), x2 = stats::rnorm(40))
  d$y <- 1 + 2 * d$x1 + stats::rnorm(40, sd = 0.3)
  folds <- rsample::vfold_cv(d, v = 3)

  wf <- workflows::workflow() |>
    workflows::add_recipe(recipes::recipe(y ~ ., d)) |>
    workflows::add_model(parsnip::rand_forest(mtry = tune::tune(), trees = 50) |>
                           parsnip::set_engine("ranger") |>
                           parsnip::set_mode("regression"))

  ps <- dials::finalize(workflows::extract_parameter_set_dials(wf), d[, 1:2])

  set.seed(4904)
  tr <- tune::tune_grid(wf, folds, grid = 2, param_info = ps,
                        metrics = tuning_metric_set("none"))
  best <- tune::select_best(tr, metric = "rmse")

  it("returns the collect_metrics() means at best_params$.config", {

    panel <- cv_panel_at(tr, best)
    cm    <- tune::collect_metrics(tr)
    cm    <- cm[cm$.config == best$.config, ]

    for (m in c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")) {
      expect_equal(panel[[paste0("cv_", m)]], cm$mean[cm$.metric == m],
                   info = m)
    }

  })

  it("returns all-NA rather than erroring when the lookup is impossible", {

    expect_true(all(is.na(unlist(cv_panel_at(NULL, best)))))
    expect_true(all(is.na(unlist(cv_panel_at(tr, best[, setdiff(names(best), ".config")])))))
    expect_true(all(is.na(unlist(cv_panel_at(tr, NULL)))))

  })

})

## =========================================================================
## Failure paths
## =========================================================================

describe("evaluate_single_config() - failure paths", {

  setup <- make_eval_setup()

  it("returns 'failed' for invalid model name", {

    config <- make_eval_config(model = "deep_learning_9000")

    result <- evaluate_single_config(
      config_row    = config,
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0
    )

    expect_equal(result$status, "failed")
    expect_true(grepl("Model specification failed", result$error_message))

  })

  it("returns 'failed' for invalid feature_selection", {

    config <- make_eval_config(feature_selection = "quantum_entanglement")

    result <- evaluate_single_config(
      config_row    = config,
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0
    )

    expect_equal(result$status, "failed")
    expect_true(grepl("Recipe building failed", result$error_message))

  })

  it("has NA metrics on failure", {

    config <- make_eval_config(model = "nope")

    result <- evaluate_single_config(
      config_row    = config,
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0
    )

    expect_true(is.na(result$rmse))
    expect_true(is.na(result$rpd))
    expect_true(is.na(result$rsq))

  })

  it("returns matching column structure on failure", {

    config <- make_eval_config(model = "nope")

    result <- evaluate_single_config(
      config_row    = config,
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0
    )

    expect_true(all(EXPECTED_RESULT_COLS %in% names(result)))

    ## cv_* columns exist on failure too, as NA, so bind_rows() is clean
    cv_cols <- paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))
    expect_true(all(cv_cols %in% names(result)))
    expect_true(all(is.na(unlist(result[cv_cols]))))

  })

})

## =========================================================================
## Pruning
## =========================================================================

describe("evaluate_single_config() - pruning", {

  setup  <- make_eval_setup()
  config <- make_eval_config()

  ## Prune threshold of 9999 will prune any realistic model (RPD < 9999)
  result <- evaluate_single_config(
    config_row      = config,
    split           = setup$split,
    cv_folds        = setup$folds,
    role_map        = setup$role_map,
    grid_size       = 2,
    bayesian_iter   = 5,
    prune           = TRUE,
    prune_threshold = 9999,
    seed            = 42L
  )

  it("returns 'pruned' status when grid RPD is below threshold", {

    expect_equal(result$status, "pruned")

  })

  it("still computes metrics when pruned (from grid search)", {

    ## Pruned configs still get last_fit metrics — they just skip Bayesian
    expect_false(is.na(result$rmse))
    expect_false(is.na(result$rsq))

  })

})

## =========================================================================
## Back-transformation
## =========================================================================

describe("evaluate_single_config() - back-transformation", {

  setup  <- make_eval_setup()
  config <- make_eval_config(transformation = "log")

  result <- evaluate_single_config(
    config_row    = config,
    split         = setup$split,
    cv_folds      = setup$folds,
    role_map      = setup$role_map,
    grid_size     = 2,
    bayesian_iter = 0,
    seed          = 42L
  )

  it("succeeds with log transformation", {

    expect_equal(result$status, "success")

  })

  it("computes metrics on original scale (positive RMSE)", {

    expect_false(is.na(result$rmse))
    expect_true(result$rmse > 0)

  })

})

## =========================================================================
## Tuning metrics are on the original scale (#49 / #38)
## =========================================================================
## The response transform is a skip = TRUE recipe step, so tune never applies
## it to the assessment set. Before tuning_metric_set() the prune gate
## compared original-scale truth to log-scale predictions and stamped healthy
## log models "pruned". This fixture has a strong log-linear signal, so a
## correctly scored grid search must clear prune_threshold = 1.0 comfortably.

describe("evaluate_single_config() - tuning on the original scale", {

  setup <- make_eval_setup(n = 100, n_wn = 30)

  ## Overwrite the weak-signal outcome with a strong log-linear one, keeping
  ## the split/fold row membership from make_eval_setup(). The signal sits in
  ## interior wavenumbers so the SG window (9) does not trim it away. plsr is
  ## linear in log space, so original-scale test RPD lands around 6 when the
  ## grid is scored honestly; scored cross-scale the same grid reads below 1
  ## and the config is pruned.
  set.seed(4902)
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = 30))
  signal   <- rowMeans(as.matrix(setup$data[, wn_names[10:15]]))
  setup$data$SOC <- expm1(1.5 + 1.2 * signal + stats::rnorm(nrow(setup$data), sd = 0.05))
  setup$split$data <- setup$data
  setup$folds <- suppressWarnings(
    rsample::vfold_cv(rsample::training(setup$split), v = 3, strata = "SOC")
  )

  config <- make_eval_config(model = "plsr", transformation = "log")

  ## bayesian_iter = 1: the prune gate only runs when there is a Bayesian
  ## stage to skip (#38), and this test is about the gate.
  result <- evaluate_single_config(
    config_row      = config,
    split           = setup$split,
    cv_folds        = setup$folds,
    role_map        = setup$role_map,
    grid_size       = 3,
    bayesian_iter   = 1,
    prune           = TRUE,
    prune_threshold = 1.0,
    seed            = 42L
  )

  it("does not prune a healthy log-transformed config at prune_threshold = 1.0", {

    expect_equal(result$status, "success")

  })

  it("reports an original-scale test RPD consistent with the strong signal", {

    expect_gt(result$rpd, 1.5)

  })

})

## =========================================================================
## Bayesian optimization skipped (bayesian_iter = 0)
## =========================================================================

describe("evaluate_single_config() - bayesian_iter = 0", {

  setup  <- make_eval_setup()
  config <- make_eval_config()

  result <- evaluate_single_config(
    config_row    = config,
    split         = setup$split,
    cv_folds      = setup$folds,
    role_map      = setup$role_map,
    grid_size     = 2,
    bayesian_iter = 0,
    seed          = 42L
  )

  it("succeeds with grid results only", {

    expect_equal(result$status, "success")
    expect_false(is.na(result$rmse))

  })

})


## =========================================================================
## The prune gate is a no-op without a Bayesian stage (#38)
## =========================================================================
## The gate decides whether to skip Bayesian optimization. With
## bayesian_iter = 0 there is none to skip, yet a config below the threshold
## was labelled "pruned", and evaluate() and fit() treat that label as a
## fallback rank.

describe("evaluate_single_config() - prune gate at bayesian_iter = 0 (#38)", {

  setup  <- make_eval_setup()
  config <- make_eval_config()

  ## A threshold no model clears: with a Bayesian stage this config is pruned
  ## (see the pruning block above).
  result <- suppressWarnings(evaluate_single_config(
    config_row      = config,
    split           = setup$split,
    cv_folds        = setup$folds,
    role_map        = setup$role_map,
    grid_size       = 2,
    bayesian_iter   = 0,
    prune           = TRUE,
    prune_threshold = 9999,
    seed            = 42L
  ))

  it("labels the config a success, since no Bayesian stage was skipped", {

    expect_equal(result$status, "success")

  })

  it("defaults prune_threshold to 1.0, the value evaluate() passes", {

    expect_identical(formals(evaluate_single_config)$prune_threshold, 1.0)
    expect_identical(formals(evaluate_single_config)$prune_threshold,
                     formals(evaluate)$prune_threshold)

  })

})


## =========================================================================
## mtry ceiling — the predictor count comes from the recipe's roles
## =========================================================================

describe("the mtry upper bound", {

  ## A sibling lab measurement is held at role `response_hold`: it is in the
  ## baked frame and it is not a predictor. Subtracting outcome, id and meta
  ## off the baked frame therefore counted it, and tune_grid() could sample
  ## an mtry one larger than the model matrix is wide.

  make_setup_with_sibling <- function(n = 40, n_wn = 20) {

    setup <- make_eval_setup(n = n, n_wn = n_wn)

    setup$train_data$clay <- stats::runif(nrow(setup$train_data), 5, 60)
    setup$role_map        <- rbind(setup$role_map,
                                   tibble::tibble(variable = "clay", role = "response"))

    setup

  }

  ## The frame the old subtraction would have handed dials::finalize()
  subtracted_width <- function(baked) {

    length(setdiff(names(baked), c("SOC", "sample_id")))

  }

  it("counts only the predictor-role columns of the prepped recipe", {

    setup   <- make_setup_with_sibling()
    recipe  <- build_recipe(make_eval_config(), setup$train_data, setup$role_map)
    prepped <- recipes::prep(recipe)
    baked   <- recipes::bake(prepped, new_data = NULL)

    pred_vars <- prepped_predictors(prepped, baked)

    ## The sibling survives into the baked frame, and is not a predictor
    expect_true("clay" %in% names(baked))
    expect_false("clay" %in% pred_vars)
    expect_identical(length(pred_vars), subtracted_width(baked) - 1L)

  })

  it("finalizes mtry at the true predictor count, not the baked width", {

    setup   <- make_setup_with_sibling()
    recipe  <- build_recipe(make_eval_config(), setup$train_data, setup$role_map)
    prepped <- recipes::prep(recipe)
    baked   <- recipes::bake(prepped, new_data = NULL)

    pred_vars <- prepped_predictors(prepped, baked)

    wflow <- workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(parsnip::rand_forest(mtry = hardhat::tune()) |>
                             parsnip::set_engine("ranger") |>
                             parsnip::set_mode("regression"))

    param_set <- workflows::extract_parameter_set_dials(wflow)

    finalized <- dials::finalize(param_set, baked[, pred_vars, drop = FALSE])

    mtry_range <- dials::range_get(finalized$object[[which(finalized$name == "mtry")]])

    expect_identical(as.integer(mtry_range$upper), length(pred_vars))
    expect_lt(as.integer(mtry_range$upper), subtracted_width(baked))

  })

})
