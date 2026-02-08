## ---------------------------------------------------------------------------
## Tests: fit_single_config()
## ---------------------------------------------------------------------------
## TDD: Defines the output contract for the fit() capture layer BEFORE
## implementation.

## ---------------------------------------------------------------------------
## Helper: build minimal data, split, folds, and role_map for fit testing
## ---------------------------------------------------------------------------

make_fit_setup <- function(n = 60, n_wn = 10, seed = 42) {

  set.seed(seed)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal from first 3 predictors
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  role_map <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Split F (train/test for fit)
  split_F <- suppressWarnings(
    rsample::initial_split(df, prop = 0.75, strata = "SOC")
  )
  train_F <- rsample::training(split_F)

  ## No UQ split — train_Fit = train_F for simplicity
  folds <- suppressWarnings(
    rsample::vfold_cv(train_F, v = 3, strata = "SOC")
  )

  list(
    data       = df,
    split_F    = split_F,
    train_F    = train_F,
    folds      = folds,
    role_map   = role_map
  )

}

## Helper: config row matching configure() output
make_fit_config <- function(model             = "rf",
                            transformation    = "none",
                            preprocessing     = "raw",
                            feature_selection = "none",
                            covariates        = NA_character_,
                            config_id         = "fit_test_001") {

  tibble::tibble(
    config_id         = config_id,
    model             = model,
    transformation    = transformation,
    preprocessing     = preprocessing,
    feature_selection = feature_selection,
    covariates        = covariates
  )

}

## Helper: mock best_params from evaluate (rf defaults)
make_fit_best_params <- function(mtry = 5L, trees = 200L, min_n = 5L) {

  tibble::tibble(
    mtry  = as.integer(mtry),
    trees = as.integer(trees),
    min_n = as.integer(min_n)
  )

}


## Expected fields in the output list
EXPECTED_FIT_FIELDS <- c(
  "config_id", "status", "degraded", "degraded_reason",
  "fitted_workflow", "best_params",
  "cv_predictions", "test_metrics", "cv_metrics",
  "uq", "warnings", "error_message", "runtime_secs"
)

## Expected columns in cv_predictions
EXPECTED_CV_PRED_COLS <- c(
  ".row", ".fold", "config_id", ".pred", ".pred_trans", "truth"
)


## =========================================================================
## Success path
## =========================================================================

describe("fit_single_config() - success path", {

  setup  <- make_fit_setup()
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  it("returns a list", {

    expect_true(is.list(result))

  })

  it("has all expected fields", {

    expect_true(all(EXPECTED_FIT_FIELDS %in% names(result)))

  })

  it("sets status to 'success'", {

    expect_equal(result$status, "success")

  })

  it("preserves config_id", {

    expect_equal(result$config_id, "fit_test_001")

  })

  it("degraded is logical", {

    expect_true(is.logical(result$degraded))

  })

  it("records positive runtime", {

    expect_true(result$runtime_secs > 0)

  })

  it("has NA error_message on success", {

    expect_true(is.na(result$error_message))

  })

})


## =========================================================================
## Fitted workflow
## =========================================================================

describe("fit_single_config() - fitted workflow", {

  setup  <- make_fit_setup()
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  it("returns a fitted workflow (butchered)", {

    expect_true(!is.null(result$fitted_workflow))

  })

  it("fitted workflow can still predict on new data", {

    test_data <- rsample::testing(setup$split_F)
    preds <- stats::predict(result$fitted_workflow, new_data = test_data)

    expect_s3_class(preds, "tbl_df")
    expect_true(".pred" %in% names(preds))
    expect_equal(nrow(preds), nrow(test_data))

  })

})


## =========================================================================
## OOF predictions shape
## =========================================================================

describe("fit_single_config() - cv_predictions", {

  setup  <- make_fit_setup()
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  cv_preds <- result$cv_predictions

  it("is a tibble", {

    expect_s3_class(cv_preds, "tbl_df")

  })

  it("has all expected columns", {

    expect_true(all(EXPECTED_CV_PRED_COLS %in% names(cv_preds)))

  })

  it(".pred is on original scale (not transformed)", {

    ## With transformation = "none", .pred == .pred_trans
    expect_equal(cv_preds$.pred, cv_preds$.pred_trans)

  })

  it("truth is on original scale", {

    ## truth should be positive SOC values (our test data has SOC > 0)
    expect_true(all(is.finite(cv_preds$truth)))

  })

  it(".row is integer", {

    expect_true(is.integer(cv_preds$.row) || is.numeric(cv_preds$.row))

  })

  it(".fold is character", {

    expect_true(is.character(cv_preds$.fold))

  })

  it("config_id is consistent", {

    expect_true(all(cv_preds$config_id == "fit_test_001"))

  })

  it("has one row per sample in training data (each sample appears once)", {

    ## OOF: each training sample appears exactly once across folds
    n_train <- nrow(rsample::training(setup$split_F))
    expect_equal(nrow(cv_preds), n_train)

  })

})


## =========================================================================
## CV metrics
## =========================================================================

describe("fit_single_config() - cv_metrics", {

  setup  <- make_fit_setup()
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  cv_met <- result$cv_metrics

  it("is a tibble with mean and std_err columns", {

    expect_s3_class(cv_met, "tbl_df")
    expect_true("mean" %in% names(cv_met))
    expect_true("std_err" %in% names(cv_met))

  })

  it("contains the standard 6 metrics", {

    expected_metrics <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")
    expect_true(all(expected_metrics %in% cv_met$.metric))

  })

  it("mean values are finite", {

    expect_true(all(is.finite(cv_met$mean)))

  })

  it("std_err values are non-negative", {

    expect_true(all(cv_met$std_err >= 0))

  })

})


## =========================================================================
## Test metrics
## =========================================================================

describe("fit_single_config() - test_metrics", {

  setup  <- make_fit_setup()
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  test_met <- result$test_metrics

  it("is a single-row tibble with 6 metrics", {

    expect_s3_class(test_met, "tbl_df")
    expect_equal(nrow(test_met), 1)

  })

  it("has all 6 standard metric columns", {

    expected <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")
    expect_true(all(expected %in% names(test_met)))

  })

  it("metrics are on original scale (finite, reasonable)", {

    expect_true(is.finite(test_met$rmse))
    expect_true(test_met$rmse > 0)

  })

})


## =========================================================================
## Back-transformation
## =========================================================================

describe("fit_single_config() - log transformation", {

  setup  <- make_fit_setup()

  ## Ensure SOC is positive for log transform
  setup$train_F$SOC <- abs(setup$train_F$SOC) + 0.1
  test_data <- rsample::testing(setup$split_F)

  config <- make_fit_config(transformation = "log")
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  it("succeeds with log transformation", {

    expect_equal(result$status, "success")

  })

  it(".pred and .pred_trans differ when transformation is applied", {

    cv_preds <- result$cv_predictions

    ## .pred_trans should be on log scale, .pred on original scale
    ## They should NOT be identical
    expect_false(all(cv_preds$.pred == cv_preds$.pred_trans))

  })

})


## =========================================================================
## Failure paths
## =========================================================================

describe("fit_single_config() - failure paths", {

  setup <- make_fit_setup()

  it("returns 'failed' for invalid model name", {

    config <- make_fit_config(model = "deep_learning_9000")
    best_p <- make_fit_best_params()

    result <- fit_single_config(
      config_row       = config,
      split_F          = setup$split_F,
      cv_resamples     = setup$folds,
      calib_data       = NULL,
      role_map         = setup$role_map,
      best_params_eval = best_p,
      final_bayesian_iter = 0L,
      grid_size        = 2L,
      compute_uq       = FALSE,
      allow_par        = FALSE
    )

    expect_equal(result$status, "failed")
    expect_false(is.na(result$error_message))

  })

  it("has all expected fields on failure", {

    config <- make_fit_config(model = "nope")
    best_p <- make_fit_best_params()

    result <- fit_single_config(
      config_row       = config,
      split_F          = setup$split_F,
      cv_resamples     = setup$folds,
      calib_data       = NULL,
      role_map         = setup$role_map,
      best_params_eval = best_p,
      final_bayesian_iter = 0L,
      grid_size        = 2L,
      compute_uq       = FALSE,
      allow_par        = FALSE
    )

    expect_true(all(EXPECTED_FIT_FIELDS %in% names(result)))

  })

  it("has NULL fitted_workflow on failure", {

    config <- make_fit_config(model = "nope")
    best_p <- make_fit_best_params()

    result <- fit_single_config(
      config_row       = config,
      split_F          = setup$split_F,
      cv_resamples     = setup$folds,
      calib_data       = NULL,
      role_map         = setup$role_map,
      best_params_eval = best_p,
      final_bayesian_iter = 0L,
      grid_size        = 2L,
      compute_uq       = FALSE,
      allow_par        = FALSE
    )

    expect_null(result$fitted_workflow)
    expect_null(result$cv_predictions)
    expect_null(result$test_metrics)
    expect_null(result$cv_metrics)

  })

})


## =========================================================================
## Degradation detection
## =========================================================================

describe("fit_single_config() - degradation detection", {

  setup  <- make_fit_setup()
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  it("degraded is FALSE or TRUE (never NA for success)", {

    expect_true(!is.na(result$degraded))

  })

  it("degraded_reason is NA when not degraded", {

    if (!result$degraded) {

      expect_true(is.na(result$degraded_reason))

    }

  })

  it("degraded_reason is a string when degraded", {

    if (result$degraded) {

      expect_true(is.character(result$degraded_reason))
      expect_true(nchar(result$degraded_reason) > 0)

    }

  })

})


## =========================================================================
## UQ skipped when compute_uq = FALSE
## =========================================================================

describe("fit_single_config() - UQ disabled", {

  setup  <- make_fit_setup()
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  result <- fit_single_config(
    config_row       = config,
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = best_p,
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  it("uq is NULL when compute_uq = FALSE", {

    expect_null(result$uq)

  })

})
