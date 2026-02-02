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
