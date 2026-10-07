## ---------------------------------------------------------------------------
## Tests: fit-uq.R
## ---------------------------------------------------------------------------
## TDD: Tests for fit_uq() (CQR-style uncertainty quantification) and
## compute_c_alpha() (conformal finite-sample correction).


## =========================================================================
## compute_c_alpha() — pure math, no data dependencies
## =========================================================================

describe("compute_c_alpha() - basic contract", {

  ## Simple known scores
  scores <- seq(0, 1, length.out = 100)

  it("increases with higher coverage level", {

    c90 <- compute_c_alpha(scores, level = 0.90)
    c95 <- compute_c_alpha(scores, level = 0.95)
    expect_true(c95 > c90)

  })

  it("rejects level = 0 and level = 1 (outside valid range)", {

    expect_error(
      compute_c_alpha(scores, level = 0),
      "level must be"
    )

    expect_error(
      compute_c_alpha(scores, level = 1),
      "level must be"
    )

  })

  it("refuses scores with no finite value", {

    ## The abort carries no package class. Without it the quantile of an
    ## empty set is NA, returned as the correction.
    expect_error(
      compute_c_alpha(c(NA, NaN, Inf, -Inf), level = 0.90),
      "No finite scores available for conformal calibration", fixed = TRUE
    )

  })

})


describe("compute_c_alpha() - finite-sample correction", {

  it("uses ceiling((1-alpha)*(n+1))/n formula", {

    ## For n = 100, level = 0.90:
    ##   alpha = 0.1
    ##   q_prob = min(1, ceiling(0.9 * 101) / 100) = min(1, ceiling(90.9) / 100)
    ##         = min(1, 91/100) = 0.91
    scores <- seq(0, 1, length.out = 100)
    result <- compute_c_alpha(scores, level = 0.90)

    ## Manual calculation
    expected <- stats::quantile(scores, probs = 0.91, type = 7, names = FALSE)
    expect_equal(result, expected)

  })

  it("caps q_prob at 1 for very small calibration sets", {

    ## For n = 5, level = 0.95:
    ##   q_prob = min(1, ceiling(0.95 * 6) / 5) = min(1, ceiling(5.7) / 5)
    ##         = min(1, 6/5) = min(1, 1.2) = 1.0
    scores <- c(0.1, 0.2, 0.3, 0.4, 0.5)
    result <- compute_c_alpha(scores, level = 0.95)

    expected <- stats::quantile(scores, probs = 1.0, type = 7, names = FALSE)
    expect_equal(result, expected)

  })

})


## =========================================================================
## fit_uq() — needs a fitted workflow + OOF predictions + calib data
## =========================================================================

## Helper: create a minimal setup for UQ testing.
## We need: (1) a fitted workflow, (2) OOF predictions in the right shape,
## (3) calibration data, (4) role_map.
## `edit_recipe` takes the built recipe and returns the one to fit, for tests
## that need a step build_recipe() never adds.
make_uq_setup <- function(n_train = 60, n_calib = 40, n_wn = 10, seed = 42,
                          edit_recipe = identity) {

  set.seed(seed)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  outcome  <- "SOC"

  ## --- Training data ---
  spec_mat_train <- matrix(rnorm(n_train * n_wn), nrow = n_train)
  colnames(spec_mat_train) <- wn_names

  train_data <- tibble::as_tibble(spec_mat_train)
  train_data$sample_id <- paste0("T", sprintf("%03d", seq_len(n_train)))
  train_data[[outcome]] <- 2 + rowMeans(spec_mat_train[, 1:3]) * 0.5 +
    rnorm(n_train, sd = 0.5)

  ## --- Calibration data ---
  spec_mat_calib <- matrix(rnorm(n_calib * n_wn), nrow = n_calib)
  colnames(spec_mat_calib) <- wn_names

  calib_data <- tibble::as_tibble(spec_mat_calib)
  calib_data$sample_id <- paste0("C", sprintf("%03d", seq_len(n_calib)))
  calib_data[[outcome]] <- 2 + rowMeans(spec_mat_calib[, 1:3]) * 0.5 +
    rnorm(n_calib, sd = 0.5)

  ## --- Role map ---
  role_map <- tibble::tibble(
    variable = c("sample_id", wn_names, outcome),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## --- Build and fit a simple workflow ---
  config <- tibble::tibble(
    config_id = "uq_test", model = "rf", transformation = "none",
    preprocessing = "raw", feature_selection = "none",
    covariates = NA_character_
  )

  recipe <- edit_recipe(build_recipe(config, train_data, role_map))
  spec   <- parsnip::rand_forest(mtry = 5L, trees = 50L, min_n = 5L) %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("regression")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)

  fitted_wf <- workflows::fit(wf, data = train_data)

  ## --- Fake OOF predictions (mimicking fit_single_config output) ---
  ## In real use, these come from fit_resamples. For testing, we generate
  ## plausible predictions.
  set.seed(seed + 1)
  oof_preds_raw <- stats::predict(fitted_wf, new_data = train_data)$.pred
  oof_noise     <- rnorm(n_train, sd = 0.3)

  oof_predictions <- tibble::tibble(
    .row        = seq_len(n_train),
    .fold       = rep(paste0("Fold", 1:3), length.out = n_train),
    config_id   = "uq_test",
    .pred       = oof_preds_raw + oof_noise,
    .pred_trans  = oof_preds_raw + oof_noise,
    truth       = train_data[[outcome]]
  )

  list(
    fitted_wf       = fitted_wf,
    oof_predictions = oof_predictions,
    calib_data      = calib_data,
    train_data      = train_data,
    role_map        = role_map,
    outcome_col     = outcome
  )

}


describe("fit_uq() - return contract", {

  setup <- make_uq_setup()

  result <- fit_uq(
    fitted_workflow = setup$fitted_wf,
    oof_predictions = setup$oof_predictions,
    calib_data      = setup$calib_data,
    role_map        = setup$role_map,
    transformation  = "none",
    level_default   = 0.90
  )

  it("is a list with every expected field, each of the right kind", {

    expect_true(is.list(result))

    expected_fields <- c(
      "quantile_model", "scores", "n_calib", "level_default",
      "oof_coverage", "mean_width", "prepped_recipe"
    )
    expect_true(all(expected_fields %in% names(result)))

    expect_s3_class(result$quantile_model, "ranger")

    expect_true(is.numeric(result$scores))
    expect_true(length(result$scores) > 0)
    expect_equal(result$n_calib, length(result$scores))

    expect_equal(result$level_default, 0.90)

    expect_true(result$oof_coverage >= 0)
    expect_true(result$oof_coverage <= 1)

    expect_true(result$mean_width > 0)

    expect_s3_class(result$prepped_recipe, "recipe")

  })

  it("scores are signed and finite (true CQR convention)", {

    ## Signed nonconformity scores (Romano et al. 2019): may be negative
    ## where the predicted quantile band already brackets the residual.
    ## They must be finite (NA scores are dropped upstream). Most calibration
    ## residuals at this fixture fall inside the band, so some scores are
    ## negative; scores taken as absolute values would have none.
    expect_true(all(is.finite(result$scores)))
    expect_true(any(result$scores < 0))

  })

  it("oof_coverage and mean_width describe the out-of-fold bands, recomputed from the bundle", {

    ## At the fixture's own calibration set the bands cover every out-of-fold
    ## truth, and a coverage of 1 cannot tell the two-sided check from one
    ## that accepts a truth inside either bound. Calibration truths close to
    ## the point predictions give negative scores, so c_alpha narrows the
    ## bands until some truths fall outside them.
    calib <- setup$calib_data
    calib[[setup$outcome_col]] <- withr::with_seed(1, {
      stats::predict(setup$fitted_wf, new_data = calib)$.pred +
        stats::rnorm(nrow(calib), sd = 0.1)
    })

    narrow <- fit_uq(
      fitted_workflow = setup$fitted_wf,
      oof_predictions = setup$oof_predictions,
      calib_data      = calib,
      role_map        = setup$role_map,
      transformation  = "none",
      level_default   = 0.90
    )

    oof      <- setup$oof_predictions
    features <- workflows::extract_mold(setup$fitted_wf)$predictors[oof$.row, , drop = FALSE]
    alpha    <- 1 - 0.90
    q        <- stats::predict(narrow$quantile_model, data = as.data.frame(features),
                               type = "quantiles",
                               quantiles = c(alpha / 2, 1 - alpha / 2))$predictions
    c_alpha  <- compute_c_alpha(narrow$scores, 0.90)
    lower    <- oof$.pred + q[, 1] - c_alpha
    upper    <- oof$.pred + q[, 2] + c_alpha
    covered  <- oof$truth >= lower & oof$truth <= upper

    expect_true(any(covered))
    expect_true(any(!covered))

    expect_equal(narrow$oof_coverage, mean(covered))
    expect_equal(narrow$mean_width, mean(upper - lower))

  })

})


describe("fit_uq() - conformal scores properties", {

  setup <- make_uq_setup()

  result <- fit_uq(
    fitted_workflow = setup$fitted_wf,
    oof_predictions = setup$oof_predictions,
    calib_data      = setup$calib_data,
    role_map        = setup$role_map,
    transformation  = "none",
    level_default   = 0.90
  )

  it("scores are computed on calibration data (not training)", {

    expect_equal(result$n_calib, nrow(setup$calib_data))

  })

  it("scores are the signed CQR scores of the calibration residuals, taken after the clamp to the outcome range", {

    ## The intervals are served around clamped point predictions, so the
    ## clamp must reach the calibration residuals too. An upper bound at the
    ## median calibration prediction binds on half the set. The scores are
    ## recomputed from the bundle's own quantile model on the baked
    ## calibration features.
    preds <- stats::predict(setup$fitted_wf, new_data = setup$calib_data)$.pred
    cap   <- stats::median(preds)

    expect_true(any(preds > cap))

    clamped <- fit_uq(
      fitted_workflow = setup$fitted_wf,
      oof_predictions = setup$oof_predictions,
      calib_data      = setup$calib_data,
      role_map        = setup$role_map,
      transformation  = "none",
      level_default   = 0.90,
      outcome_range   = c(0, cap)
    )

    features <- recipes::bake(clamped$prepped_recipe, new_data = setup$calib_data,
                              recipes::all_predictors())
    alpha    <- 1 - 0.90
    q        <- stats::predict(clamped$quantile_model, data = as.data.frame(features),
                               type = "quantiles",
                               quantiles = c(alpha / 2, 1 - alpha / 2))$predictions
    r        <- setup$calib_data[[setup$outcome_col]] - pmin(preds, cap)

    expect_equal(clamped$scores, pmax(q[, 1] - r, r - q[, 2]))

  })

  it("scores leave out calibration rows with no outcome, in n_calib and in the minimum", {

    ## Five missing outcomes leave 35 rows to score; eleven leave 29, under
    ## N_CALIB_MIN, though the set itself still has 40 rows.
    fit_missing <- function(n_missing) {
      calib <- setup$calib_data
      calib[[setup$outcome_col]][seq_len(n_missing)] <- NA_real_
      fit_uq(
        fitted_workflow = setup$fitted_wf,
        oof_predictions = setup$oof_predictions,
        calib_data      = calib,
        role_map        = setup$role_map
      )
    }

    some <- fit_missing(5L)

    expect_false(is.null(some))
    expect_equal(some$n_calib, nrow(setup$calib_data) - 5L)
    expect_length(some$scores, nrow(setup$calib_data) - 5L)
    expect_false(anyNA(some$scores))

    n_missing <- nrow(setup$calib_data) - N_CALIB_MIN + 1L

    expect_gte(nrow(setup$calib_data), N_CALIB_MIN)
    expect_null(fit_missing(n_missing))

  })

})


describe("fit_uq() - too few calibration points", {

  ## Create setup with only 10 calibration points (below N_CALIB_MIN = 30)
  setup <- make_uq_setup(n_calib = 10)

  it("returns NULL when calibration set is too small", {

    result <- fit_uq(
      fitted_workflow = setup$fitted_wf,
      oof_predictions = setup$oof_predictions,
      calib_data      = setup$calib_data,
      role_map        = setup$role_map,
      transformation  = "none",
      level_default   = 0.90
    )

    expect_null(result)

  })

})


describe("fit_uq() - prepped recipe can bake new data", {

  setup <- make_uq_setup()

  result <- fit_uq(
    fitted_workflow = setup$fitted_wf,
    oof_predictions = setup$oof_predictions,
    calib_data      = setup$calib_data,
    role_map        = setup$role_map,
    transformation  = "none",
    level_default   = 0.90
  )

  it("prepped_recipe can bake calibration data (predictors only)", {

    baked <- recipes::bake(result$prepped_recipe,
                            new_data = setup$calib_data,
                            recipes::all_predictors())
    expect_s3_class(baked, "tbl_df")
    expect_true(nrow(baked) == nrow(setup$calib_data))

    ## Should NOT contain outcome or id columns
    expect_false(setup$outcome_col %in% names(baked))
    expect_false("sample_id" %in% names(baked))

  })

})


## =========================================================================
## A prediction step that fails
## =========================================================================
## Each failing step is caught: a calibration prediction that fails returns
## NULL (the config degrades to no UQ), and an out-of-fold one leaves the
## diagnostics NA. The quantile predictions go through ranger's predict()
## method, which the point model also calls, so the mock below fails only
## the quantile prediction on the rows it is given.

quantile_prediction_failing_on <- function(n_rows) {

  real_predict <- getS3method("predict", "ranger")

  function(object, data, ...) {

    if (identical(list(...)$type, "quantiles") && nrow(data) == n_rows) {
      stop("quantile prediction failed")
    }

    real_predict(object, data, ...)

  }

}

describe("fit_uq() - a prediction step that fails", {

  ## suppressWarnings() around the setup only, as in the row-alignment block
  setup <- suppressWarnings(make_uq_setup())

  it("returns NULL, rather than aborting, when a calibration prediction fails", {

    ## Each call mocks one method for its own duration
    fit_mocked <- function(class, method) {
      local_mocked_s3_method("predict", class, method)
      fit_uq(
        fitted_workflow = setup$fitted_wf,
        oof_predictions = setup$oof_predictions,
        calib_data      = setup$calib_data,
        role_map        = setup$role_map
      )
    }

    ## The point prediction on the calibration set
    expect_null(fit_mocked("workflow", function(object, new_data, ...) {
      stop("point prediction failed")
    }))

    ## The quantile prediction on the calibration set
    expect_null(fit_mocked("ranger", quantile_prediction_failing_on(nrow(setup$calib_data))))

  })

  it("keeps the bundle, with NA diagnostics, when the out-of-fold quantile prediction fails", {

    local_mocked_s3_method(
      "predict", "ranger",
      quantile_prediction_failing_on(nrow(setup$oof_predictions))
    )

    result <- fit_uq(
      fitted_workflow = setup$fitted_wf,
      oof_predictions = setup$oof_predictions,
      calib_data      = setup$calib_data,
      role_map        = setup$role_map
    )

    expect_false(is.null(result))
    expect_equal(result$n_calib, nrow(setup$calib_data))
    expect_identical(result$oof_coverage, NA_real_)
    expect_identical(result$mean_width, NA_real_)

  })

})


## =========================================================================
## Thread pinning at the call site (M6, 2026-09-15)
## =========================================================================

describe("fit_uq() - quantile forest thread pinning", {

  it("calls ranger with num.threads = 1", {

    setup    <- make_uq_setup()
    captured <- NULL

    ## Capture ranger's arguments without fitting: fit_uq() wraps the call in
    ## safely_execute(), so the mocked error is absorbed and returns NULL.
    testthat::with_mocked_bindings(
      ranger = function(...) { captured <<- list(...); stop("captured") },
      suppressWarnings(fit_uq(
        fitted_workflow = setup$fitted_wf,
        oof_predictions = setup$oof_predictions,
        calib_data      = setup$calib_data,
        role_map        = setup$role_map,
        transformation  = "none",
        level_default   = 0.90
      )),
      .package = "ranger"
    )

    expect_false(is.null(captured))
    expect_identical(captured$num.threads, 1L)

  })

})


## =========================================================================
## Row alignment before positional pairing (#140)
## =========================================================================
## The OOF features are indexed by .row into the mold, and the calibration
## predictions, features and truth are paired by position. A recipe step that
## dropped rows would misalign them; the length mismatch recycled with at most
## a base R warning and produced a bundle with wrong scores.

describe("fit_uq() - row alignment", {

  ## suppressWarnings() around the setup only: the fixture's forest asks for
  ## more mtry than the recipe leaves predictors, and ranger says so.

  it("aborts when an out-of-fold .row is past the rows the model was fit on, below 1 or NA", {

    setup <- suppressWarnings(make_uq_setup())
    oof   <- setup$oof_predictions
    oof$.row[1] <- nrow(setup$train_data) + 1L

    expect_error(
      fit_uq(
        fitted_workflow = setup$fitted_wf,
        oof_predictions = oof,
        calib_data      = setup$calib_data,
        role_map        = setup$role_map
      ),
      "Out-of-fold rows", class = "horizons_internal_error"
    )

    ## A zero or NA .row indexes no row of the mold, so the forest's features
    ## would come out a row short of its residuals or blank
    fit_with_row <- function(row) {
      oof <- setup$oof_predictions
      oof$.row[1] <- row
      fit_uq(
        fitted_workflow = setup$fitted_wf,
        oof_predictions = oof,
        calib_data      = setup$calib_data,
        role_map        = setup$role_map
      )
    }

    expect_error(fit_with_row(0L), "Out-of-fold rows",
                 class = "horizons_internal_error")
    expect_error(fit_with_row(NA_integer_), "Out-of-fold rows",
                 class = "horizons_internal_error")

  })

  it("aborts when the recipe drops a calibration row at bake", {

    ## The filter keeps every training row (ids "T...") and drops one
    ## calibration row, from predict() and bake() alike.
    setup <- suppressWarnings(make_uq_setup(edit_recipe = function(rec) {
      recipes::step_filter(rec, sample_id != "C001", skip = FALSE)
    }))

    expect_error(
      fit_uq(
        fitted_workflow = setup$fitted_wf,
        oof_predictions = setup$oof_predictions,
        calib_data      = setup$calib_data,
        role_map        = setup$role_map
      ),
      "Calibration predictions", class = "horizons_internal_error"
    )

  })

  it("aborts when the calibration features are baked a row short", {

    ## Predictions served whole, so only the feature bake is short.
    setup     <- suppressWarnings(make_uq_setup())
    real_bake <- recipes::bake

    local_mocked_s3_method("predict", "workflow", function(object, new_data, ...) {
      tibble::tibble(.pred = rep(2, nrow(new_data)))
    })

    local_mocked_bindings(
      bake = function(object, new_data, ...) {
        real_bake(object, new_data = new_data[-1, ], ...)
      },
      .package = "recipes"
    )

    expect_error(
      fit_uq(
        fitted_workflow = setup$fitted_wf,
        oof_predictions = setup$oof_predictions,
        calib_data      = setup$calib_data,
        role_map        = setup$role_map
      ),
      "Calibration features baked through the recipe",
      class = "horizons_internal_error"
    )

  })

})
