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
                            config_id         = "fit_test_001") {

  tibble::tibble(
    config_id         = config_id,
    model             = model,
    transformation    = transformation,
    preprocessing     = preprocessing,
    feature_selection = feature_selection
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

  it("is a list with every expected field: its config_id, a positive runtime, no error, and degraded FALSE with no reason", {

    result <- fsc()$result
    expect_true(is.list(result))
    expect_true(all(EXPECTED_FIT_FIELDS %in% names(result)))
    expect_equal(result$config_id, "fit_test_001")
    expect_true(is.logical(result$degraded))
    expect_true(result$runtime_secs > 0)
    expect_true(is.na(result$error_message))

    ## The shared fit learns, so it is not flagged (see build_fsc())
    expect_true(!is.na(result$degraded))
    expect_false(result$degraded)
    expect_true(is.na(result$degraded_reason))

  })

  it("sets status to 'success'", {

    result <- fsc()$result
    expect_equal(result$status, "success")

  })

})


## =========================================================================
## Fitted workflow
## =========================================================================

describe("fit_single_config() - fitted workflow", {

  it("is butchered for storage and can still predict on new data", {

    shared <- fsc()
    result <- shared$result

    expect_s3_class(result$fitted_workflow, "butchered_workflow")

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

  it("is a tibble with every expected column, finite truth, a numeric .row, a character .fold and one config_id", {

    cv_preds <- fsc()$result$cv_predictions
    expect_s3_class(cv_preds, "tbl_df")
    expect_true(all(EXPECTED_CV_PRED_COLS %in% names(cv_preds)))
    expect_true(all(is.finite(cv_preds$truth)))
    expect_true(is.integer(cv_preds$.row) || is.numeric(cv_preds$.row))
    expect_true(is.character(cv_preds$.fold))
    expect_true(all(cv_preds$config_id == "fit_test_001"))

  })

  it(".pred is on original scale (not transformed)", {

    cv_preds <- fsc()$result$cv_predictions

    ## With transformation = "none", .pred == .pred_trans
    expect_equal(cv_preds$.pred, cv_preds$.pred_trans)

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

  it("holds each of the 6 metrics' mean over the folds and its standard error", {

    result <- fsc()$result
    cv_met <- result$cv_metrics
    expect_s3_class(cv_met, "tbl_df")
    expect_setequal(cv_met$.metric, c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))

    ## Recomputed from the OOF predictions, fold by fold: the standard error
    ## is the folds' sd over sqrt(folds). fit() reports it as cv_rmse_se and
    ## cv_rpd_se.
    cv_preds <- result$cv_predictions
    per_fold <- purrr::map_dfr(
      split(cv_preds, cv_preds$.fold),
      ~ compute_original_scale_metrics(.x$truth, .x$.pred)
    )
    expected <- per_fold |>
      dplyr::group_by(.metric) |>
      dplyr::summarise(mean    = mean(.estimate),
                       std_err = stats::sd(.estimate) / sqrt(dplyr::n()))

    cv_met <- cv_met[match(expected$.metric, cv_met$.metric), ]
    expect_equal(cv_met$mean, expected$mean)
    expect_equal(cv_met$std_err, expected$std_err)

  })

})


## =========================================================================
## Test metrics
## =========================================================================

describe("fit_single_config() - test_metrics", {

  it("is a single-row tibble with the 6 standard metric columns, on the original scale (finite, reasonable)", {

    test_met <- fsc()$result$test_metrics
    expect_s3_class(test_met, "tbl_df")
    expect_equal(nrow(test_met), 1)
    expected <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")
    expect_true(all(expected %in% names(test_met)))
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

  it("scores the test rows on the original scale, from the back-transformed predictions", {

    preds <- stats::predict(result$fitted_workflow, new_data = test_data)$.pred
    preds <- back_transform_predictions(preds, "log", warn = FALSE)

    expect_equal(result$test_metrics$rmse, yardstick::rmse_vec(test_data$SOC, preds))

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

## Every stage runs inside safely_execute() and, when it errors, returns the
## failed result naming the stage. With a stage's exit removed the config
## still fails, but at a later stage and under that stage's name, so each
## case asserts the stage and the cause. Stages are made to fail by mocking a
## horizons step or tune::fit_resamples(), or, for the final fit, by missing
## predictor values that only train_data holds (the folds carry their own
## copy of the rows).

describe("fit_single_config() - failure paths", {

  ## The runner on the shared fit's setup. Each test reads fsc() before it
  ## mocks anything, so the shared fit is never built under a mock
  ## (helper-memo.R).
  run_on_shared <- function(setup,
                            config_row       = make_fit_config(),
                            train_data       = NULL,
                            best_params_eval = make_fit_best_params()) {

    fit_single_config(
      config_row          = config_row,
      split_F             = setup$split_F,
      cv_resamples        = setup$folds,
      calib_data          = NULL,
      train_data          = train_data,
      role_map            = setup$role_map,
      best_params_eval    = best_params_eval,
      final_bayesian_iter = 0L,
      grid_size           = 2L,
      compute_uq          = FALSE,
      allow_par           = FALSE,
      seed                = 42L
    )

  }

  ## The failed result: the stage named ahead of the cause, the documented
  ## values of a failed config (warm_start and start_grid_size NA when it
  ## failed before the re-tune), and no fitted parts
  expect_failed_at <- function(result, stage, cause, warm_start) {

    expect_equal(result$status, "failed")
    expect_match(result$error_message, paste0("^", stage, " failed: "))
    expect_match(result$error_message, cause, fixed = TRUE)
    expect_true(all(EXPECTED_FIT_FIELDS %in% names(result)))
    expect_identical(result$degraded, NA)
    expect_identical(result$degraded_reason, NA_character_)
    expect_identical(result$warm_start, warm_start)

    if (is.na(warm_start)) {
      expect_identical(result$start_grid_size, NA_integer_)
    } else {
      expect_true(is.integer(result$start_grid_size) && !is.na(result$start_grid_size))
    }

    expect_null(result$fitted_workflow)
    expect_null(result$cv_predictions)
    expect_null(result$test_metrics)
    expect_null(result$cv_metrics)

  }

  it("names the recipe stage when the recipe cannot be built", {

    setup <- fsc()$setup
    local_mocked_bindings(build_recipe = function(...) stop("no recipe for this config"))

    expect_failed_at(run_on_shared(setup), "Recipe building", "no recipe for this config",
                     warm_start = NA)

  })

  it("returns 'failed' for invalid model name, with every expected field and no fitted parts", {

    setup <- fsc()$setup
    expect_failed_at(run_on_shared(setup, make_fit_config(model = "deep_learning_9000")),
                     "Model specification", "Unknown model type: 'deep_learning_9000'",
                     warm_start = NA)

  })

  it("names the workflow stage when the model cannot join the workflow", {

    setup <- fsc()$setup
    local_mocked_bindings(define_model_spec = function(...) "not a model specification")

    expect_failed_at(run_on_shared(setup), "Workflow creation", "model_spec", warm_start = NA)

  })

  it("names the re-tune when the warm-start search errors", {

    setup <- fsc()$setup
    local_mocked_bindings(tune_warmstart_bayes = function(...) stop("the search broke"))

    expect_failed_at(run_on_shared(setup), "Warm-start tuning", "the search broke",
                     warm_start = NA)

  })

  it("names the OOF stage when the resampled fits error", {

    setup <- fsc()$setup
    local_mocked_bindings(fit_resamples = function(...) stop("the resampled fits broke"),
                          .package = "tune")

    expect_failed_at(run_on_shared(setup), "OOF predictions", "the resampled fits broke",
                     warm_start = TRUE)

  })

  it("names the OOF back-transformation when it errors", {

    setup <- fsc()$setup
    ## The OOF call is the only one with a prediction per training row; the
    ## tuning metrics back-transform one fold's assessment rows at a time
    n_train <- nrow(setup$train_F)
    real_bt <- back_transform_predictions
    local_mocked_bindings(back_transform_predictions = function(predictions, ...) {
      if (length(predictions) == n_train) stop("the OOF inverse broke")
      real_bt(predictions, ...)
    })

    expect_failed_at(run_on_shared(setup), "OOF back-transformation", "the OOF inverse broke",
                     warm_start = TRUE)

  })

  it("names the final fit when it errors", {

    setup <- fsc()$setup
    ## glmnet refuses missing predictor values; the column is one the raw
    ## step's edge trim keeps
    train <- rsample::training(setup$split_F)
    train[[grep("^wn_", names(train), value = TRUE)[5]]][1:3] <- NA

    expect_failed_at(run_on_shared(setup, make_fit_config(model = "elastic_net"),
                                   train_data = train, best_params_eval = NULL),
                     "Final model fit", "missing values", warm_start = FALSE)

  })

  it("names the test prediction when it errors", {

    setup <- fsc()$setup
    ## The runner's call is the only one that predicts from a whole workflow
    real_predict <- stats::predict
    local_mocked_bindings(predict = function(object, ...) {
      if (inherits(object, "workflow")) stop("the test rows could not be predicted")
      real_predict(object, ...)
    }, .package = "stats")

    expect_failed_at(run_on_shared(setup), "Test prediction",
                     "the test rows could not be predicted", warm_start = TRUE)

  })

  it("names the test back-transformation when it errors", {

    setup <- fsc()$setup
    ## The next back-transformation after the OOF one is the test rows'
    n_train  <- nrow(fsc()$setup$train_F)
    oof_done <- FALSE
    real_bt  <- back_transform_predictions
    local_mocked_bindings(back_transform_predictions = function(predictions, ...) {
      if (oof_done) stop("the test inverse broke")
      if (length(predictions) == n_train) oof_done <<- TRUE
      real_bt(predictions, ...)
    })

    expect_failed_at(run_on_shared(setup), "Test back-transformation", "the test inverse broke",
                     warm_start = TRUE)

  })

})


## =========================================================================
## Degradation detection
## =========================================================================

describe("fit_single_config() - degradation detection", {

  config <- make_fit_config()
  best_p <- make_fit_best_params()

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
## UQ and AD skipped when compute_uq and compute_ad are FALSE
## =========================================================================
## fit_single_config() fits UQ only when compute_uq is TRUE and a calibration
## set is given, and AD only when compute_ad is TRUE and one is given. fit()
## also drops the bundles when its flags are FALSE, so these gates are visible
## only here, with a calibration set present.

describe("fit_single_config() - a warm-start search that fails (#209)", {

  it("keeps the starting grid's choice and records on the member that the search failed", {

    shared <- fsc()

    ## A starting grid of one point: the warm-start search cannot start.
    result <- suppressWarnings(fit_single_config(
      config_row          = make_fit_config(),
      split_F             = shared$setup$split_F,
      cv_resamples        = shared$setup$folds,
      calib_data          = NULL,
      role_map            = shared$setup$role_map,
      best_params_eval    = make_fit_best_params(),
      final_bayesian_iter = 2L,
      grid_size           = 1L,
      compute_uq          = FALSE,
      compute_ad          = FALSE,
      allow_par           = FALSE,
      seed                = 42L
    ))

    expect_equal(result$status, "success")
    expect_true(any(grepl(
      "The warm-start Bayesian search failed, so the hyperparameters were chosen from its starting grid alone",
      result$warnings, fixed = TRUE
    )))

  })

})

describe("fit_single_config() - UQ and AD disabled", {

  it("uq and ad are NULL when compute_uq and compute_ad are FALSE, even with a calibration set", {

    shared <- fsc()

    ## fit_uq() and fit_ad() return sentinels, so a gate that let either run
    ## would leave a bundle whatever the calibration set's size. The test rows
    ## stand in for the calibration set: with both mocked, nothing reads them.
    local_mocked_bindings(
      fit_uq = function(...) list(sentinel = "fit_uq() ran"),
      fit_ad = function(...) list(sentinel = "fit_ad() ran")
    )

    result <- fit_single_config(
      config_row       = make_fit_config(),
      split_F          = shared$setup$split_F,
      cv_resamples     = shared$setup$folds,
      calib_data       = rsample::testing(shared$setup$split_F),
      role_map         = shared$setup$role_map,
      best_params_eval = make_fit_best_params(),
      final_bayesian_iter = 0L,
      grid_size        = 2L,
      compute_uq       = FALSE,
      compute_ad       = FALSE,
      allow_par        = FALSE,
      seed             = 42L
    )

    ## The fit reached the UQ and AD steps, so only the gates kept the
    ## bundles out
    expect_equal(result$status, "success")
    expect_null(result$uq)
    expect_null(result$ad)

  })

})


## =========================================================================
## A run whose stages warn and whose last steps fail
## =========================================================================
## One run, read by three tests: every stage's warnings reach the member's
## log (#96); test metrics that cannot be computed come back as six NAs; a
## butcher() failure stores the unbutchered workflow. render_warning_log()
## shows five lines, most frequent first; each stage's warning is raised ten
## times, more than any of tune's notes can arrive, and mtry is the two
## predictors the raw step keeps, so tune adds no note of its own.

build_fsc_mocked <- function() {

  setup      <- make_fit_setup(signal_cols = 5:6, signal = 1, noise_sd = 0.1)
  test_truth <- rsample::testing(setup$split_F)$SOC

  real_recipe    <- build_recipe
  real_spec      <- define_model_spec
  real_preds     <- prepped_predictors
  real_tune      <- tune_warmstart_bayes
  real_resamples <- tune::fit_resamples
  real_metrics   <- compute_original_scale_metrics

  warn_then <- function(message, f) {
    function(...) {
      for (i in 1:10) warning(message, call. = FALSE)
      f(...)
    }
  }

  run <- function() {
    fit_single_config(
      config_row          = make_fit_config(),
      split_F             = setup$split_F,
      cv_resamples        = setup$folds,
      calib_data          = NULL,
      role_map            = setup$role_map,
      best_params_eval    = make_fit_best_params(mtry = 2L),
      final_bayesian_iter = 0L,
      grid_size           = 2L,
      compute_uq          = FALSE,
      allow_par           = FALSE,
      seed                = 42L
    )
  }

  result <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(
        run(),
        fit_resamples = warn_then("the OOF stage warned", real_resamples),
        .package = "tune"
      ),
      butcher = function(x, ...) stop("butcher failed"),
      .package = "butcher"
    ),
    build_recipe         = warn_then("the recipe stage warned", real_recipe),
    define_model_spec    = warn_then("the model stage warned", real_spec),
    prepped_predictors   = warn_then("the finalize stage warned", real_preds),
    tune_warmstart_bayes = warn_then("the tuning stage warned", real_tune),
    ## What compute_original_scale_metrics() returns, with a warning, when
    ## fewer than two test rows have both a truth and a prediction
    compute_original_scale_metrics = function(truth, estimate) {
      if (identical(truth, test_truth)) return(tibble::tibble())
      real_metrics(truth, estimate)
    }
  )

  list(setup = setup, result = result)

}

fsc_mocked <- function() memo_fixture("fsc_mocked", build_fsc_mocked)

describe("fit_single_config() - a run whose stages warn and whose last steps fail", {

  it("carries every stage's warnings into the member's log (#96)", {

    result <- fsc_mocked()$result
    expect_equal(result$status, "success")

    for (message in c("the recipe stage warned", "the model stage warned",
                      "the finalize stage warned", "the tuning stage warned",
                      "the OOF stage warned")) {
      expect_true(any(grepl(message, result$warnings, fixed = TRUE)), label = message)
    }

  })

  it("reports all six test metrics as NA when they cannot be computed", {

    expect_identical(
      fsc_mocked()$result$test_metrics,
      tibble::tibble(rmse = NA_real_, rrmse = NA_real_, rsq = NA_real_,
                     ccc  = NA_real_, rpd   = NA_real_, mae = NA_real_)
    )

  })

  it("stores the unbutchered workflow when butcher() fails, and it predicts", {

    shared <- fsc_mocked()
    wf     <- shared$result$fitted_workflow

    expect_s3_class(wf, "workflow")
    expect_false(inherits(wf, "butchered_workflow"))

    test_data <- rsample::testing(shared$setup$split_F)
    expect_equal(nrow(stats::predict(wf, new_data = test_data)), nrow(test_data))

  })

})


## =========================================================================
## Measured coverage when it cannot be measured (#118)
## =========================================================================
## fit_uq() returns a sentinel bundle, so the coverage step is reached without
## a quantile forest; the test rows stand in for the calibration set.

describe("fit_single_config() - measured coverage when it cannot be measured (#118)", {

  run_with_uq <- function(setup) {

    fit_single_config(
      config_row          = make_fit_config(),
      split_F             = setup$split_F,
      cv_resamples        = setup$folds,
      calib_data          = rsample::testing(setup$split_F),
      role_map            = setup$role_map,
      best_params_eval    = make_fit_best_params(mtry = 2L),
      final_bayesian_iter = 0L,
      grid_size           = 2L,
      compute_uq          = TRUE,
      allow_par           = FALSE,
      seed                = 42L
    )

  }

  no_coverage <- list(test_coverage = NA_real_, test_mean_width = NA_real_, n_test = 0L)

  it("leaves the figures NA and records the warning when no intervals can be built", {

    setup <- fsc()$setup
    ## predict_intervals() warns and returns NULL when it fails
    local_mocked_bindings(
      fit_uq            = function(...) list(sentinel = "fit_uq() ran"),
      predict_intervals = function(...) {
        warning("intervals could not be built", call. = FALSE)
        NULL
      }
    )

    result <- run_with_uq(setup)

    expect_identical(result$uq[names(no_coverage)], no_coverage)
    expect_true(any(grepl("intervals could not be built", result$warnings, fixed = TRUE)))

  })

  it("leaves the figures NA, and keeps the bundle, when measuring errors", {

    setup <- fsc()$setup
    local_mocked_bindings(
      fit_uq                 = function(...) list(sentinel = "fit_uq() ran"),
      test_interval_coverage = function(...) stop("coverage could not be measured")
    )

    result <- run_with_uq(setup)

    expect_equal(result$uq$sentinel, "fit_uq() ran")
    expect_identical(result$uq[names(no_coverage)], no_coverage)

  })

})


## =========================================================================
## The RNG pin
## =========================================================================

describe("fit_single_config() - the RNG pin", {

  it("returns the same fit whatever the caller's RNG kind and state", {

    skip_unless_slow_tier()

    ## A worker started under furrr_options(seed = TRUE) is on L'Ecuyer-CMRG.
    ## The runner pins kind and seed on entry. The OOF fits and the final fit
    ## re-pin, so the pin shows only through the re-tune's choice: a cold
    ## start's Bayesian stage draws its candidates from the stream.
    setup <- fsc()$setup

    run <- function() {
      fit_single_config(
        config_row          = make_fit_config(),
        split_F             = setup$split_F,
        cv_resamples        = setup$folds,
        calib_data          = NULL,
        role_map            = setup$role_map,
        best_params_eval    = NULL,
        final_bayesian_iter = 1L,
        grid_size           = 2L,
        compute_uq          = FALSE,
        allow_par           = FALSE,
        seed                = 42L
      )
    }

    mersenne <- withr::with_seed(1, run())
    lecuyer  <- withr::with_seed(99, .rng_kind = "L'Ecuyer-CMRG", run())

    ## The test sees the pin only if the Bayesian candidate is the one chosen
    expect_match(mersenne$best_params$.config, "^iter")
    expect_identical(lecuyer$best_params, mersenne$best_params)
    expect_identical(lecuyer$cv_predictions, mersenne$cv_predictions)

  })

})



## =========================================================================
## A PLS config's component range (#216)
## =========================================================================

describe("fit_single_config() - a PLS config", {

  it("tunes num_comp up to the predictors and the smallest fold's rows less one, at most 30", {

    ## 60 wavenumbers keep 52 predictors past the raw step's edge trim; the
    ## 45 training rows make analysis sets of 29 to 31 rows, so the smallest
    ## set's rows less one bind, below 30. The re-tune is stopped once it has
    ## the parameter set.
    setup     <- make_fit_setup(n_wn = 60)
    param_set <- NULL
    local_mocked_bindings(tune_warmstart_bayes = function(..., param_set) {
      param_set <<- param_set
      stop("stopped after the parameter set")
    })

    fit_single_config(
      config_row          = make_fit_config(model = "plsr"),
      split_F             = setup$split_F,
      cv_resamples        = setup$folds,
      calib_data          = NULL,
      role_map            = setup$role_map,
      best_params_eval    = NULL,
      final_bayesian_iter = 0L,
      grid_size           = 2L,
      compute_uq          = FALSE,
      allow_par           = FALSE,
      seed                = 42L
    )

    num_comp <- param_set$object[[which(param_set$name == "num_comp")]]
    expect_equal(c(num_comp$range$lower, num_comp$range$upper),
                 c(1, min_analysis_rows(setup$folds) - 1))
    expect_lt(min_analysis_rows(setup$folds) - 1, 30)

  })

})
