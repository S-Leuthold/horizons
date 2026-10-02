## ---------------------------------------------------------------------------
## Tests: fit_single_config()
## ---------------------------------------------------------------------------
## TDD: Defines the output contract for the fit() capture layer BEFORE
## implementation.

## ---------------------------------------------------------------------------
## Helper: build minimal data, split, folds, and role_map for fit testing
## ---------------------------------------------------------------------------

make_fit_setup <- function(n = 60, n_wn = 10, seed = 42,
                           signal_cols = seq_len(min(3, n_wn)),
                           signal = 0.5, noise_sd = 0.5) {

  set.seed(seed)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with signal from signal_cols (by default, weak signal from the
  ## first 3 predictors)
  df$SOC <- 2 + rowMeans(spec_mat[, signal_cols, drop = FALSE]) * signal +
    rnorm(n, sd = noise_sd)

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
## Shared fit (helper-memo.R)
## =========================================================================
## One successful fit_single_config() result, built on its first use in a
## process and read by every block below that asserts on a successful fit.
## The fixture learns: its strong signal sits on columns 5 and 6, the two
## that survive the raw step's edge trim at 10 points, so the bootstrap
## interval of its test RPD reaches well past its CV mean RPD and the fit is
## not flagged as degraded. Returns the setup with the result, since some
## tests read the split.

build_fsc <- function() {

  setup <- make_fit_setup(signal_cols = 5:6, signal = 1, noise_sd = 0.1)

  result <- fit_single_config(
    config_row       = make_fit_config(),
    split_F          = setup$split_F,
    cv_resamples     = setup$folds,
    calib_data       = NULL,
    role_map         = setup$role_map,
    best_params_eval = make_fit_best_params(),
    final_bayesian_iter = 0L,
    grid_size        = 2L,
    compute_uq       = FALSE,
    allow_par        = FALSE,
    seed             = 42L
  )

  list(setup = setup, result = result)

}

fsc <- function() memo_fixture("fsc", build_fsc)


## =========================================================================
## Success path
## =========================================================================

describe("fit_single_config() - success path", {

  it("returns a list", {

    result <- fsc()$result
    expect_true(is.list(result))

  })

  it("has all expected fields", {

    result <- fsc()$result
    expect_true(all(EXPECTED_FIT_FIELDS %in% names(result)))

  })

  it("sets status to 'success'", {

    result <- fsc()$result
    expect_equal(result$status, "success")

  })

  it("preserves config_id", {

    result <- fsc()$result
    expect_equal(result$config_id, "fit_test_001")

  })

  it("degraded is logical", {

    result <- fsc()$result
    expect_true(is.logical(result$degraded))

  })

  it("records positive runtime", {

    result <- fsc()$result
    expect_true(result$runtime_secs > 0)

  })

  it("has NA error_message on success", {

    result <- fsc()$result
    expect_true(is.na(result$error_message))

  })

})


## =========================================================================
## Fitted workflow
## =========================================================================

describe("fit_single_config() - fitted workflow", {

  it("returns a fitted workflow (butchered)", {

    result <- fsc()$result
    expect_true(!is.null(result$fitted_workflow))

  })

  it("fitted workflow can still predict on new data", {

    shared <- fsc()
    result <- shared$result

    test_data <- rsample::testing(shared$setup$split_F)
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

  it("is a tibble", {

    cv_preds <- fsc()$result$cv_predictions
    expect_s3_class(cv_preds, "tbl_df")

  })

  it("has all expected columns", {

    cv_preds <- fsc()$result$cv_predictions
    expect_true(all(EXPECTED_CV_PRED_COLS %in% names(cv_preds)))

  })

  it(".pred is on original scale (not transformed)", {

    cv_preds <- fsc()$result$cv_predictions

    ## With transformation = "none", .pred == .pred_trans
    expect_equal(cv_preds$.pred, cv_preds$.pred_trans)

  })

  it("truth is on original scale", {

    cv_preds <- fsc()$result$cv_predictions

    ## truth should be positive SOC values (our test data has SOC > 0)
    expect_true(all(is.finite(cv_preds$truth)))

  })

  it(".row is integer", {

    cv_preds <- fsc()$result$cv_predictions
    expect_true(is.integer(cv_preds$.row) || is.numeric(cv_preds$.row))

  })

  it(".fold is character", {

    cv_preds <- fsc()$result$cv_predictions
    expect_true(is.character(cv_preds$.fold))

  })

  it("config_id is consistent", {

    cv_preds <- fsc()$result$cv_predictions
    expect_true(all(cv_preds$config_id == "fit_test_001"))

  })

  it("has one row per sample in training data (each sample appears once)", {

    shared   <- fsc()
    cv_preds <- shared$result$cv_predictions

    ## OOF: each training sample appears exactly once across folds
    n_train <- nrow(rsample::training(shared$setup$split_F))
    expect_equal(nrow(cv_preds), n_train)

  })

})


## =========================================================================
## CV metrics
## =========================================================================

describe("fit_single_config() - cv_metrics", {

  it("is a tibble with mean and std_err columns", {

    cv_met <- fsc()$result$cv_metrics
    expect_s3_class(cv_met, "tbl_df")
    expect_true("mean" %in% names(cv_met))
    expect_true("std_err" %in% names(cv_met))

  })

  it("contains the standard 6 metrics", {

    cv_met <- fsc()$result$cv_metrics
    expected_metrics <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")
    expect_true(all(expected_metrics %in% cv_met$.metric))

  })

  it("mean values are finite", {

    cv_met <- fsc()$result$cv_metrics
    expect_true(all(is.finite(cv_met$mean)))

  })

  it("std_err values are non-negative", {

    cv_met <- fsc()$result$cv_metrics
    expect_true(all(cv_met$std_err >= 0))

  })

})


## =========================================================================
## Test metrics
## =========================================================================

describe("fit_single_config() - test_metrics", {

  it("is a single-row tibble with 6 metrics", {

    test_met <- fsc()$result$test_metrics
    expect_s3_class(test_met, "tbl_df")
    expect_equal(nrow(test_met), 1)

  })

  it("has all 6 standard metric columns", {

    test_met <- fsc()$result$test_metrics
    expected <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")
    expect_true(all(expected %in% names(test_met)))

  })

  it("metrics are on original scale (finite, reasonable)", {

    test_met <- fsc()$result$test_metrics
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

  it("selects best_params through the original-scale tuning metric set (#49)", {

    ## tune_warmstart_bayes() selects on metric = "rmse" by name. The
    ## transformed-config metric set comes from tuning_metric_set(), which must
    ## keep that name for selection to succeed at all.
    expect_s3_class(result$best_params, "data.frame")
    expect_equal(nrow(result$best_params), 1L)

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

  ## The shared fit learns, so it is not flagged (see build_fsc())
  config <- make_fit_config()
  best_p <- make_fit_best_params()

  it("degraded is FALSE or TRUE (never NA for success)", {

    result <- fsc()$result
    expect_true(!is.na(result$degraded))

  })

  it("degraded_reason is NA when not degraded", {

    result <- fsc()$result
    expect_false(result$degraded)
    expect_true(is.na(result$degraded_reason))

  })

  it("degraded_reason is a string when degraded", {

    ## The shared fit's setup with its test rows' outcomes replaced by noise,
    ## so the test RPD falls well below the CV RPD
    deg_setup <- fsc()$setup

    test_rows <- rsample::complement(deg_setup$split_F)
    train_soc <- deg_setup$train_F$SOC
    deg_setup$split_F$data$SOC[test_rows] <- withr::with_seed(
      7, stats::rnorm(length(test_rows), mean(train_soc), stats::sd(train_soc))
    )

    degraded_result <- fit_single_config(
      config_row       = config,
      split_F          = deg_setup$split_F,
      cv_resamples     = deg_setup$folds,
      calib_data       = NULL,
      role_map         = deg_setup$role_map,
      best_params_eval = best_p,
      final_bayesian_iter = 0L,
      grid_size        = 2L,
      compute_uq       = FALSE,
      allow_par        = FALSE,
      seed             = 42L
    )

    expect_equal(degraded_result$status, "success")
    expect_true(degraded_result$degraded)
    expect_true(is.character(degraded_result$degraded_reason))
    expect_true(nchar(degraded_result$degraded_reason) > 0)
    expect_match(degraded_result$degraded_reason, "below the CV mean RPD",
                 fixed = TRUE)

  })

})


## =========================================================================
## UQ skipped when compute_uq = FALSE
## =========================================================================

describe("fit_single_config() - UQ disabled", {

  it("uq is NULL when compute_uq = FALSE", {

    result <- fsc()$result
    expect_null(result$uq)

  })

})
