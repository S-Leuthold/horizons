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

  it("returns a single numeric value", {

    result <- compute_c_alpha(scores, level = 0.90)
    expect_true(is.numeric(result))
    expect_length(result, 1)

  })

  it("increases with higher coverage level", {

    c90 <- compute_c_alpha(scores, level = 0.90)
    c95 <- compute_c_alpha(scores, level = 0.95)
    expect_true(c95 > c90)

  })

  it("returns 0 for level = 0 (no coverage requested)", {

    ## At level = 0, alpha = 1, q_prob = ceiling(0 * (n+1)) / n = 0
    ## This is a degenerate case but should not error
    result <- compute_c_alpha(scores, level = 0)
    expect_true(is.numeric(result))

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
make_uq_setup <- function(n_train = 60, n_calib = 40, n_wn = 10, seed = 42) {

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

  recipe <- build_recipe(config, train_data, role_map)
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

  it("returns a list", {

    expect_true(is.list(result))

  })

  it("has all expected fields", {

    expected_fields <- c(
      "quantile_model", "scores", "n_calib", "level_default",
      "oof_coverage", "mean_width", "prepped_recipe"
    )
    expect_true(all(expected_fields %in% names(result)))

  })

  it("quantile_model is a ranger object", {

    expect_s3_class(result$quantile_model, "ranger")

  })

  it("scores is a numeric vector", {

    expect_true(is.numeric(result$scores))
    expect_true(length(result$scores) > 0)

  })

  it("scores are non-negative (CQR convention)", {

    expect_true(all(result$scores >= 0))

  })

  it("n_calib matches score length", {

    expect_equal(result$n_calib, length(result$scores))

  })

  it("level_default is stored correctly", {

    expect_equal(result$level_default, 0.90)

  })

  it("oof_coverage is between 0 and 1", {

    expect_true(result$oof_coverage >= 0)
    expect_true(result$oof_coverage <= 1)

  })

  it("mean_width is positive", {

    expect_true(result$mean_width > 0)

  })

  it("prepped_recipe is a prepped recipe", {

    expect_s3_class(result$prepped_recipe, "recipe")

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

  it("no NA values in scores", {

    expect_false(any(is.na(result$scores)))

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
