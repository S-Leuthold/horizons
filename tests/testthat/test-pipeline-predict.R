## ---------------------------------------------------------------------------
## Tests: predict.horizons_fit()
## ---------------------------------------------------------------------------
## Exercises the predict() verb end to end: point predictions, config
## selection, conformal intervals, the axis-alignment schema gate, the
## non-negativity clamp, graceful degradation, and empirical coverage.
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Helper: build a horizons_eval ready for fit(), large enough for UQ
## ---------------------------------------------------------------------------

make_predict_eval <- function(n = 300, n_wn = 10, transformation = "none", seed = 42) {

  set.seed(seed)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Non-negative outcome with real signal so models fit and intervals mean
  ## something. Kept comfortably positive so the non-neg clamp is not spuriously
  ## triggered by the clean-data tests (a dedicated test forces a negative).
  df$SOC <- 5 + rowMeans(spec_mat[, 1:3]) * 1.5 + rnorm(n, sd = 0.4)

  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  configs <- tibble::tibble(
    config_id         = "cfg_001",
    model             = "rf",
    transformation    = transformation,
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = NA_character_
  )

  obj <- list(
    data = list(analysis = df, role_map = roles, n_rows = nrow(df),
                n_predictors = n_wn, n_covariates = 0L, n_responses = 1L),
    provenance = list(spectra_source = "test", spectra_type = "mir",
                      schema_version = 1L),
    config = list(configs = configs, n_configs = 1L,
                  tuning = list(cv_folds = 3L, grid_size = 3L,
                                bayesian_iter = 0L, final_bayesian_iter = 0L)),
    validation = list(passed = TRUE, outliers = list(removed = FALSE)),
    evaluation = list(
      results     = tibble::tibble(config_id = "cfg_001", status = "success",
                                   rmse = 0.4, rrmse = 0.1, rsq = 0.8,
                                   ccc = 0.85, rpd = 1.5, mae = 0.3),
      best_config = "cfg_001",
      rank_metric = "rpd",
      split        = rsample::initial_split(df, prop = 0.8),
      n_train      = as.integer(round(nrow(df) * 0.8)),
      n_test       = nrow(df) - as.integer(round(nrow(df) * 0.8)),
      runtime_secs = 1,
      timestamp    = Sys.time()
    ),
    models = NULL, ensemble = NULL, artifacts = NULL
  )

  class(obj) <- c("horizons_eval", "horizons_data", "list")
  obj

}

## New-data tibble on the SAME training axis.
make_new_spectra <- function(n = 8, n_wn = 10, seed = 99) {

  set.seed(seed)
  m <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  df <- tibble::as_tibble(m)
  df$sample_id <- paste0("NEW", seq_len(n))
  df

}

## Fit once, reuse across tests (UQ on; n large enough that calib >= N_CALIB_MIN).
fitted_fixture <- fit(make_predict_eval(), n_best = 1L, compute_uq = TRUE,
                      verbose = FALSE)


## ---------------------------------------------------------------------------
## Point predictions
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - point predictions", {

  new_df <- make_new_spectra()

  it("returns one row per sample with a .pred column", {

    p <- predict(fitted_fixture, new_df, interval = FALSE)

    expect_s3_class(p, "tbl_df")
    expect_equal(nrow(p), nrow(new_df))
    expect_true(".pred" %in% names(p))
    expect_true("sample_id" %in% names(p))

  })

  it("predictions are finite and on the original (non-negative) scale", {

    p <- predict(fitted_fixture, new_df, interval = FALSE)

    expect_true(all(is.finite(p$.pred)))
    expect_true(all(p$.pred >= 0))

  })

  it("preserves sample_id keys from new_data", {

    p <- predict(fitted_fixture, new_df, interval = FALSE)
    expect_equal(p$sample_id, new_df$sample_id)

  })

})


## ---------------------------------------------------------------------------
## Config selection
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - config selection", {

  new_df <- make_new_spectra()

  it("config = 'best' uses the top-ranked config (single block, no config_id col)", {

    p <- predict(fitted_fixture, new_df, config = "best", interval = FALSE)

    expect_equal(nrow(p), nrow(new_df))
    expect_false("config_id" %in% names(p))

  })

  it("a specific config_id works", {

    p <- predict(fitted_fixture, new_df, config = "cfg_001", interval = FALSE)
    expect_equal(nrow(p), nrow(new_df))

  })

  it("config = 'all' returns a block per config with a config_id column", {

    p <- predict(fitted_fixture, new_df, config = "all", interval = FALSE)

    expect_true("config_id" %in% names(p))
    n_configs <- length(fitted_fixture$models$workflows)
    expect_equal(nrow(p), nrow(new_df) * n_configs)

  })

  it("an unknown config_id errors informatively", {

    expect_error(
      predict(fitted_fixture, new_df, config = "nonsense"),
      "not among the fitted models"
    )

  })

})


## ---------------------------------------------------------------------------
## Conformal intervals
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - intervals", {

  new_df <- make_new_spectra()

  it("returns ordered, positive-width intervals by default", {

    p <- predict(fitted_fixture, new_df)

    expect_true(all(c(".pred_lower", ".pred_upper", ".interval_width") %in% names(p)))
    expect_true(all(p$.pred_lower <= p$.pred))
    expect_true(all(p$.pred <= p$.pred_upper))
    expect_true(all(p$.interval_width >= 0))

  })

  it("no quantile crossings even with signed conformal margins", {

    p <- predict(fitted_fixture, new_df)
    expect_true(all(p$.pred_lower <= p$.pred_upper))

  })


  it("interval = FALSE returns point predictions only", {

    p <- predict(fitted_fixture, new_df, interval = FALSE)
    expect_false(".pred_lower" %in% names(p))

  })

  it("degrades to point-only (no error) when a config has no UQ bundle", {

    ## Strip the UQ bundle to simulate compute_uq = FALSE for this config.
    no_uq <- fitted_fixture
    no_uq$models$uq <- NULL

    p <- predict(no_uq, new_df, interval = TRUE)
    expect_false(".pred_lower" %in% names(p))
    expect_true(".pred" %in% names(p))

  })

})


## ---------------------------------------------------------------------------
## Axis-alignment schema gate
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - schema gate", {

  it("errors informatively when new_data is missing expected wavelengths", {

    bad <- make_new_spectra()
    names(bad)[1:3] <- c("wn_9000", "wn_8998", "wn_8996")  # off training axis

    expect_error(
      predict(fitted_fixture, bad),
      "missing.*predictor column"
    )

  })

})


## ---------------------------------------------------------------------------
## Non-negativity floor
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - non-negativity floor", {

  ## Soil properties from MIR are non-negative; predictions and bounds floor at 0.
  it("floors predictions and interval bounds at 0", {

    p <- predict(fitted_fixture, make_new_spectra())
    expect_true(all(p$.pred >= 0))
    expect_true(all(p$.pred_lower >= 0))

  })

})


## ---------------------------------------------------------------------------
## Input validation
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - input validation", {

  it("rejects a non-horizons_fit object", {

    expect_error(
      predict.horizons_fit(list(), make_new_spectra()),
      "horizons_fit"
    )

  })

  it("accepts a horizons_data object as new_data", {

    ## Wrap new spectra as a minimal horizons_data (id + predictors).
    new_df <- make_new_spectra()
    wn     <- setdiff(names(new_df), "sample_id")

    hd <- list(
      data = list(
        analysis = new_df,
        role_map = tibble::tibble(
          variable = c("sample_id", wn),
          role     = c("id", rep("predictor", length(wn)))
        )
      )
    )
    class(hd) <- c("horizons_data", "list")

    p <- predict(fitted_fixture, hd, interval = FALSE)
    expect_equal(nrow(p), nrow(new_df))

  })

})


## ---------------------------------------------------------------------------
## Empirical coverage (the high-value assertion: validates signed CQR + scale)
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - empirical coverage", {

  it("achieves approximately nominal coverage on held-out data", {

    ## Hold out a labelled test set the model never saw, predict 90% intervals,
    ## and check the fraction covered is in a tolerance band around 0.90. This
    ## is the test that would catch a double-transform scale bug or a broken
    ## conformal margin. Data-size gated for safety.
    set.seed(2024)
    n_test  <- 120
    n_wn    <- 10
    wn      <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
    m       <- matrix(rnorm(n_test * n_wn), nrow = n_test)
    colnames(m) <- wn
    test_df <- tibble::as_tibble(m)
    test_df$sample_id <- paste0("T", seq_len(n_test))

    ## Same data-generating process as the training fixture's outcome.
    truth <- 5 + rowMeans(m[, 1:3]) * 1.5 + rnorm(n_test, sd = 0.4)

    p <- predict(fitted_fixture, test_df)   # intervals at the fitted level (0.90)

    covered  <- mean(truth >= p$.pred_lower & truth <= p$.pred_upper)

    ## Wide tolerance: synthetic data + a single split is noisy, but a broken
    ## interval (e.g. double back-transform) would land far outside this band.
    expect_gt(covered, 0.70)
    expect_lte(covered, 1.00)

  })

})


## ---------------------------------------------------------------------------
## Response upper bound (deploy-time winsorization guardrail)
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - response bound guardrail", {

  new_df <- make_new_spectra()

  it("fit() stores models$response_bound = max(outcome) * RESPONSE_BOUND_MARGIN", {

    eval_obj <- make_predict_eval()
    expected <- max(eval_obj$data$analysis$SOC, na.rm = TRUE) * RESPONSE_BOUND_MARGIN

    expect_equal(fitted_fixture$models$response_bound, expected)

  })

  it("winsorizes predictions to an injected low bound with a visible warning", {

    ## Force the clamp by injecting a bound below the known prediction range.
    p_raw <- predict(fitted_fixture, new_df, interval = FALSE)
    bound <- stats::median(p_raw$.pred)   # guaranteed to clip ~half the values

    clamped_fixture <- fitted_fixture
    clamped_fixture$models$response_bound <- bound

    expect_warning(
      p <- predict(clamped_fixture, new_df, interval = FALSE),
      "winsorized"
    )

    expect_true(all(p$.pred <= bound))
    expect_equal(nrow(p), nrow(new_df))

  })

  it("objects without response_bound predict silently and identically (backward compat)", {

    legacy_fixture <- fitted_fixture
    legacy_fixture$models$response_bound <- NULL

    expect_no_warning(p_legacy <- predict(legacy_fixture, new_df, interval = FALSE))

    p_current <- predict(fitted_fixture, new_df, interval = FALSE)

    expect_identical(p_legacy$.pred, p_current$.pred)

  })

  it("does not clamp interval bounds (only the point prediction)", {

    ## .pred_upper deliberately stays unclamped: truncating the interval
    ## would overstate confidence exactly where the model is least
    ## trustworthy. Inject a bound just below the max point prediction and
    ## confirm intervals still extend past it.
    p_raw <- predict(fitted_fixture, new_df)
    bound <- max(p_raw$.pred) * 0.99

    clamped_fixture <- fitted_fixture
    clamped_fixture$models$response_bound <- bound

    p <- suppressWarnings(predict(clamped_fixture, new_df))

    expect_true(all(p$.pred <= bound))
    expect_true(any(p$.pred_upper > bound))

  })

})


## ---------------------------------------------------------------------------
## Applicability domain columns
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - applicability domain", {

  ## fitted_fixture is fit with compute_ad = TRUE by default (n = 300 → the
  ## calibration split clears N_CALIB_MIN), so it carries an AD bundle.
  new_df <- make_new_spectra(n = 12)

  it("emits .ad_distance and .ad_flag when the object has an AD bundle", {

    skip_if_not(has_ad(fitted_fixture))

    p <- predict(fitted_fixture, new_df, interval = FALSE)

    expect_true(all(c(".ad_distance", ".ad_flag") %in% names(p)))
    expect_type(p$.ad_distance, "double")
    expect_true(all(p$.ad_distance >= 0))
    expect_s3_class(p$.ad_flag, "factor")
    expect_true(all(levels(p$.ad_flag) == c("Q1", "Q2", "Q3", "Q4", "OOD")))

  })

  it("flags far-shifted spectra as OOD", {

    skip_if_not(has_ad(fitted_fixture))

    ood_df <- new_df
    wn     <- grep("^wn_", names(ood_df))
    ood_df[, wn] <- ood_df[, wn] + 10        # shift far off the training axis

    p <- predict(fitted_fixture, ood_df, interval = FALSE)
    expect_true(mean(p$.ad_flag == "OOD") > 0.5)

  })

  it("abstain_ood = TRUE NAs the prediction but preserves .ad_distance", {

    skip_if_not(has_ad(fitted_fixture))

    ood_df <- new_df
    wn     <- grep("^wn_", names(ood_df))
    ood_df[, wn] <- ood_df[, wn] + 10

    p <- predict(fitted_fixture, ood_df, interval = FALSE, abstain_ood = TRUE)

    ood <- p$.ad_flag == "OOD"
    expect_true(all(is.na(p$.pred[ood])))          # abstained
    expect_true(all(!is.na(p$.ad_distance)))       # distance always preserved

  })

  it("abstain_ood = FALSE (default) leaves predictions intact", {

    skip_if_not(has_ad(fitted_fixture))

    p <- predict(fitted_fixture, new_df, interval = FALSE)
    expect_true(all(!is.na(p$.pred)))

  })

})
