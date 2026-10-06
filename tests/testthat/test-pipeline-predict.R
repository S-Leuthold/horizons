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

make_predict_eval <- function(n = 300, n_wn = 10, transformation = "none",
                              covariates = NULL, seed = 42, model = "rf") {

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

  ## Covariates carry the `covariate` role, as add_covariates() leaves them;
  ## build_recipe() promotes the ones the config asks for to `predictor`.
  for (cov in covariates) {

    df[[cov]] <- runif(n, 10, 40)
    roles     <- rbind(roles, tibble::tibble(variable = cov, role = "covariate"))

  }

  configs <- tibble::tibble(
    config_id         = "cfg_001",
    model             = model,
    transformation    = transformation,
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = if (is.null(covariates)) {
      NA_character_
    } else {
      paste(covariates, collapse = ",")
    }
  )

  obj <- list(
    data = list(analysis = df, role_map = roles, n_rows = nrow(df),
                n_predictors = n_wn,
                ## SOC carries role "outcome" above, not "response" (n_responses
                ## counts role == "response"); fit()'s entry-stage
                ## validate_horizons_data() call (#24) is the first thing to
                ## actually check this stored count against the role_map.
                n_covariates = length(covariates), n_responses = 0L),
    provenance = list(spectra_source = "test", spectra_type = "mir"),
    config = list(configs = configs, n_configs = 1L,
                  tuning = list(cv_folds = 3L, grid_size = 3L,
                                bayesian_iter = 0L, final_bayesian_iter = 0L)),
    validation = list(passed = TRUE, outliers = list(removed = FALSE)),
    evaluation = list(
      results     = tibble::tibble(config_id = "cfg_001", status = "success",
                                   rmse = 0.4, rrmse = 0.1, rsq = 0.8,
                                   ccc = 0.85, rpd = 1.5, mae = 0.3,
                                   ## fit() ranks on the CV panel (#50)
                                   cv_rmse = 0.45, cv_rrmse = 0.11, cv_rsq = 0.78,
                                   cv_ccc = 0.83, cv_rpd = 1.4, cv_mae = 0.33),
      best_config = "cfg_001",
      rank_metric = "rpd",
      screened    = TRUE,
      parallelize_over = "sequential",
      split        = rsample::initial_split(df, prop = 0.8),
      n_train      = as.integer(round(nrow(df) * 0.8)),
      n_test       = nrow(df) - as.integer(round(nrow(df) * 0.8)),
      response_trim = NULL,
      recipe       = list(sg_window = DEFAULT_SG_WINDOW, sg_window_cm = NA_real_,
                          pca_threshold = DEFAULT_PCA_THRESHOLD),
      runtime_secs = 1,
      timestamp    = Sys.time()
    ),
    models = NULL, ensemble = NULL
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

## A two-member copy of a fitted object, with no new fit: the best member's
## workflow, UQ and AD bundles, config row and evaluation row filed again under
## a second config_id. A claim about "each config" needs more than one config
## to fail.
add_member_copy <- function(fit, config_id = "cfg_002") {

  from <- fit$models$best_config

  fit$models$workflows[[config_id]] <- fit$models$workflows[[from]]
  fit$models$uq[[config_id]]        <- fit$models$uq[[from]]
  fit$models$ad[[config_id]]        <- fit$models$ad[[from]]
  fit$models$n_models               <- length(fit$models$workflows)

  copy_row <- function(tbl) {
    row           <- tbl[tbl$config_id == from, ]
    row$config_id <- config_id
    dplyr::bind_rows(tbl, row)
  }

  fit$config$configs     <- copy_row(fit$config$configs)
  fit$config$n_configs   <- nrow(fit$config$configs)
  fit$evaluation$results <- copy_row(fit$evaluation$results)

  validate_horizons_fit(fit)

}


## ---------------------------------------------------------------------------
## Point predictions
## ---------------------------------------------------------------------------

describe("predict.horizons_fit() - point predictions", {

  new_df <- make_new_spectra()

  it("returns one finite .pred per row of new_data, keyed by its sample_id", {

    p <- predict(fitted_fixture, new_df, interval = FALSE)

    expect_s3_class(p, "tbl_df")
    expect_equal(nrow(p), nrow(new_df))
    expect_true(".pred" %in% names(p))
    expect_true("sample_id" %in% names(p))

    ## The zero floor cannot bind here (these predictions sit near 5);
    ## test-outcome-range.R forces a negative prediction to test it.
    expect_true(all(is.finite(p$.pred)))
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

    two <- add_member_copy(fitted_fixture)
    p   <- predict(two, new_df, config = "all", interval = FALSE)

    expect_true("config_id" %in% names(p))
    expect_equal(nrow(p), nrow(new_df) * 2L)
    expect_equal(p$config_id,
                 rep(names(two$models$workflows), each = nrow(new_df)))

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

  it("returns ordered intervals by default", {

    ## The point need not sit inside its interval: CQR bands are asymmetric
    ## and a winsorized .pred can fall outside, so only the bounds' order and
    ## width are promised here; the outcome range is checked below.
    p <- predict(fitted_fixture, new_df)

    expect_true(all(c(".pred_lower", ".pred_upper", ".interval_width") %in% names(p)))
    expect_true(all(p$.pred_lower <= p$.pred_upper))
    expect_equal(p$.interval_width, p$.pred_upper - p$.pred_lower)

  })

  it("clamps both interval bounds to a recorded outcome range", {

    ## A range set inside the unclamped bounds, so both ends bind. No refit:
    ## predict() reads the range at serving time.
    p0 <- predict(fitted_fixture, new_df)
    r  <- c(stats::median(p0$.pred_lower), stats::median(p0$.pred_upper))

    expect_true(any(p0$.pred_lower < r[1]))
    expect_true(any(p0$.pred_upper > r[2]))

    capped <- fitted_fixture
    capped$config$outcome_range <- r

    p <- predict(capped, new_df)

    expect_true(all(p$.pred_lower >= r[1] & p$.pred_upper <= r[2]))
    expect_true(all(p$.pred_lower <= p$.pred_upper))
    expect_equal(p$.interval_width, p$.pred_upper - p$.pred_lower)

  })

  it("no quantile crossings even with signed conformal margins", {

    ## A margin this negative pulls each raw bound past the other, so the
    ## ordering below holds only through the crossing repair.
    local_mocked_bindings(compute_c_alpha = function(scores, level) -1e6)

    p <- predict(fitted_fixture, new_df)

    expect_true(all(p$.pred_lower <= p$.pred_upper))
    expect_true(all(p$.interval_width >= 0))

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

    ## A margin this large takes every raw lower bound far below 0
    local_mocked_bindings(compute_c_alpha = function(scores, level) 1e6)

    p <- predict(fitted_fixture, make_new_spectra())
    expect_true(all(p$.pred >= 0))
    expect_equal(p$.pred_lower, rep(0, nrow(p)))

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
## Arguments in `...` (#141)
## ---------------------------------------------------------------------------
## predict() has no level argument: intervals are at the level the UQ was
## calibrated at. A `level` warns that it is ignored, and any other argument
## (a misspelled one, say) aborts rather than being dropped.

describe("predict.horizons_fit() - arguments in ...", {

  new_df <- make_new_spectra()

  it("warns that level is ignored and returns the calibration-level intervals", {

    ## Precondition: the fixture's bundle was calibrated at 0.90.
    expect_equal(fitted_fixture$models$uq[["cfg_001"]]$level_default, 0.90)

    reference <- predict(fitted_fixture, new_df)

    w <- expect_warning(
      p <- predict(fitted_fixture, new_df, level = 0.95),
      class = "horizons_input_warning"
    )

    expect_match(conditionMessage(w), "0.9.", fixed = TRUE)
    expect_equal(p, reference)

  })

  it("names the bundle's calibration level, or the default when there is no bundle", {

    ## The warning reads uq$level_default, not the constant.
    relevelled <- fitted_fixture
    relevelled$models$uq[["cfg_001"]]$level_default <- 0.8

    w <- expect_warning(
      predict(relevelled, new_df, level = 0.95),
      class = "horizons_input_warning"
    )

    expect_match(conditionMessage(w), "0.8.", fixed = TRUE)

    no_uq <- fitted_fixture
    no_uq$models$uq <- NULL

    w <- expect_warning(
      predict(no_uq, new_df, level = 0.95),
      class = "horizons_input_warning"
    )

    expect_match(conditionMessage(w), paste0(DEFAULT_UQ_LEVEL, "."), fixed = TRUE)

  })

  it("errors on any other argument, naming it", {

    expect_error(
      predict(fitted_fixture, new_df, intervals = FALSE),
      class  = "horizons_input_error",
      regexp = "intervals"
    )

    ## The stats convention's `newdata` is not `new_data`.
    expect_error(
      predict(fitted_fixture, newdata = new_df),
      class  = "horizons_input_error",
      regexp = "newdata"
    )

  })

  it("a call with neither is unchanged", {

    expect_no_warning(p <- predict(fitted_fixture, new_df))
    expect_true(all(c(".pred", ".pred_lower", ".pred_upper") %in% names(p)))

  })

})

describe("check_predict_dots()", {

  it("does not evaluate the dots", {

    expect_warning(
      check_predict_dots(level = stop("evaluated"), .level_default = 0.9),
      class = "horizons_input_warning"
    )

    expect_error(
      check_predict_dots(foo = stop("evaluated"), .level_default = 0.9),
      class  = "horizons_input_error",
      regexp = "foo"
    )

  })

  it("refuses unnamed arguments, showing them", {

    expect_error(
      check_predict_dots(0.95, .level_default = 0.9),
      class  = "horizons_input_error",
      regexp = "0.95"
    )

  })

  it("aborts rather than warns when level comes with an unknown argument", {

    expect_error(
      check_predict_dots(level = 0.95, foo = 1, .level_default = 0.9),
      class = "horizons_input_error"
    )

  })

  it("is silent on empty dots", {

    expect_no_condition(check_predict_dots(.level_default = 0.9))

  })

  it("checks user arguments that share a name with its own", {

    expect_error(
      check_predict_dots(call = 1, .level_default = 0.9),
      class  = "horizons_input_error",
      regexp = "call"
    )

    expect_error(
      check_predict_dots(level_default = 0.95, .level_default = 0.9),
      class  = "horizons_input_error",
      regexp = "level_default"
    )

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

  it("fit() stores models$response_bound = max(training outcome) * RESPONSE_BOUND_MARGIN", {

    ## The training rows are the ones the final model was fit on, whose ids
    ## the out-of-fold predictions carry (#68); UQ is on here, so the
    ## calibration rows are out.
    eval_obj <- make_predict_eval()
    analysis <- eval_obj$data$analysis
    fit_rows <- analysis$sample_id %in% unique(fitted_fixture$models$cv_predictions$sample_id)
    expected <- max(analysis$SOC[fit_rows]) * RESPONSE_BOUND_MARGIN

    ## Precondition: the fixture discriminates. The largest training-part
    ## outcome sits in the calibration rows, so a bound over Split F's whole
    ## training part would differ from one over the fit rows.
    train_F <- rsample::training(fitted_fixture$models$split)
    expect_gt(max(train_F$SOC), max(analysis$SOC[fit_rows]))

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

    clamped <- p_raw$.pred > bound
    expect_true(any(clamped))

    expect_true(all(p$.pred <= bound))
    expect_true(any(p$.pred_upper > bound))

    ## The intervals are built around the unclamped point, so the clamp
    ## leaves both bounds exactly where they were on the rows it touched.
    expect_equal(p$.pred_upper[clamped], p_raw$.pred_upper[clamped])
    expect_equal(p$.pred_lower[clamped], p_raw$.pred_lower[clamped])

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

  it("flags far-shifted spectra as OOD, and by default still predicts them", {

    skip_if_not(has_ad(fitted_fixture))

    ood_df <- new_df
    wn     <- grep("^wn_", names(ood_df))
    ood_df[, wn] <- ood_df[, wn] + 10        # shift far off the training axis

    p <- predict(fitted_fixture, ood_df, interval = FALSE)
    expect_true(mean(p$.ad_flag == "OOD") > 0.5)

    ## abstain_ood = FALSE is the default, so the OOD rows keep their predictions
    expect_true(all(!is.na(p$.pred)))

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

  it("one malformed spectrum costs that row its AD, not the whole batch", {

    skip_if_not(has_ad(fitted_fixture))

    ## Four out-of-domain spectra plus one that bakes to NA (the documented
    ## step_transform_spectra failure path). Previously the single NA row made
    ## predict_ad() return NULL for the batch, which silently removed the AD
    ## columns AND the abstention the caller asked for.
    bad_df    <- make_new_spectra(n = 5)
    wn        <- grep("^wn_", names(bad_df))
    bad_df[, wn]    <- bad_df[, wn] + 10        # far out of domain
    bad_df[3, wn]   <- NA_real_                 # malformed spectrum

    p <- suppressWarnings(
      predict(fitted_fixture, bad_df, interval = FALSE, abstain_ood = TRUE)
    )

    expect_true(all(c(".ad_distance", ".ad_flag") %in% names(p)))
    expect_equal(sum(!is.na(p$.ad_distance)), 4L)
    expect_true(is.na(p$.ad_distance[3]))

    ## Abstention still applies to the four scored rows.
    expect_true(all(p$.ad_flag[-3] == "OOD"))
    expect_true(all(is.na(p$.pred[-3])))

  })

  it("warns when abstain_ood is asked for and no AD is available", {

    no_ad <- fitted_fixture
    no_ad$models$ad <- NULL

    expect_warning(
      p <- predict(no_ad, new_df, interval = FALSE, abstain_ood = TRUE),
      "`abstain_ood` was requested but no applicability-domain information is available",
      fixed = TRUE, class = "horizons_ad_warning"
    )

    expect_false(".ad_flag" %in% names(p))
    expect_true(all(!is.na(p$.pred)))          # nothing was abstained

  })

  it("is silent about abstention and about AD when the object has no AD bundle and abstain_ood is not requested", {

    no_ad <- fitted_fixture
    no_ad$models$ad <- NULL

    warns <- testthat::capture_warnings(
      predict(no_ad, new_df, interval = FALSE)
    )

    expect_false(any(grepl("abstain_ood", warns)))

    expect_no_warning(predict(no_ad, new_df, interval = FALSE),
                      class = "horizons_ad_warning")

  })

  it("warns, naming the config and the cause, when the AD distance fails", {

    skip_if_not(has_ad(fitted_fixture))

    ## A bundle that no longer matches the recipe's features: the distance
    ## aborts inside predict_ad(), which used to drop the AD columns silently.
    broken <- fitted_fixture
    cid    <- names(broken$models$ad)[1]
    broken$models$ad[[cid]]$centroid <- broken$models$ad[[cid]]$centroid[-1]

    w <- expect_warning(
      p <- keep_only_warning(
        predict(broken, new_df, config = cid, interval = FALSE),
        "horizons_ad_warning"
      ),
      class = "horizons_ad_warning"
    )

    expect_match(conditionMessage(w), cid, fixed = TRUE)
    expect_match(conditionMessage(w), "Number of features must match", fixed = TRUE)

    ## Point predictions are unaffected
    expect_false(any(c(".ad_distance", ".ad_flag") %in% names(p)))
    expect_equal(nrow(p), nrow(new_df))
    expect_true(all(!is.na(p$.pred)))

  })

})


## ---------------------------------------------------------------------------
## check_predictor_schema() — covariate gate runs on pre-schema objects
## ---------------------------------------------------------------------------
## models$predictor_schema is a later addition; objects written before it skip
## the wavenumber-axis gate. The covariate requirement comes from the config
## rows instead, so it must still be checked on those objects.

describe("check_predictor_schema() - required_extra without a stored schema", {

  it("aborts on a missing covariate even when predictor_schema is NULL", {

    obj <- list(models = list(predictor_schema = NULL))

    new_spectra <- tibble::tibble(sample_id = c("A", "B"),
                                  wn_4000   = c(0.1, 0.2))

    expect_error(
      check_predictor_schema(obj, new_spectra, required_extra = "Clay"),
      "covariate column"
    )

  })

  it("passes when the covariate is supplied", {

    obj <- list(models = list(predictor_schema = NULL))

    new_spectra <- tibble::tibble(sample_id = c("A", "B"),
                                  wn_4000   = c(0.1, 0.2),
                                  Clay      = c(20, 30))

    expect_true(check_predictor_schema(obj, new_spectra,
                                       required_extra = "Clay"))

  })

  it("still skips the axis gate when predictor_schema is NULL", {

    obj <- list(models = list(predictor_schema = NULL))

    ## Nothing on the training axis at all, but no schema to check it against.
    expect_true(check_predictor_schema(obj,
                                       tibble::tibble(sample_id = "A",
                                                      nonsense  = 1)))

  })

})


## ---------------------------------------------------------------------------
## Conformal coverage on a selected training set (2026-09-21)
## ---------------------------------------------------------------------------
## select_training() picks calibration rows for proximity to the targets, so
## they are not exchangeable with arbitrary new_data and the conformal
## guarantee does not transfer. predict() says so; nothing else changes.

describe("predict.horizons_fit() - selected training set", {

  new_df <- make_new_spectra()

  ## Same fitted object, with fit()'s selection flag flipped on. fitted_fixture
  ## was built with compute_uq = TRUE, which the warning also depends on.
  selected_fixture <- fitted_fixture
  selected_fixture$models$selection_present <- TRUE

  ## The same object with no UQ bundle: intervals are not returned at all, so
  ## there is no coverage claim to qualify.
  selected_no_uq <- selected_fixture
  selected_no_uq$models$uq <- NULL

  it("the fixture carries UQ, so the gate is exercised", {

    expect_true(has_uq(selected_fixture))
    expect_false(has_uq(selected_no_uq))

  })

  it("warns once, with its class, when intervals are requested on a selected fit", {

    warns <- testthat::capture_warnings(
      p <- predict(selected_fixture, new_df, interval = TRUE)
    )

    hits <- grepl("Conformal coverage is not guaranteed", warns)

    expect_equal(sum(hits), 1L)
    expect_true(any(grepl("target_distances", warns)))
    expect_equal(nrow(p), nrow(new_df))

    expect_warning(
      predict(selected_fixture, new_df, interval = TRUE),
      class = "horizons_select_warning"
    )

  })

  it("warns once per call, not once per config", {

    two <- add_member_copy(selected_fixture)

    warns <- testthat::capture_warnings(
      p <- predict(two, new_df, config = "all", interval = TRUE)
    )

    expect_equal(sum(grepl("Conformal coverage is not guaranteed", warns)), 1L)
    expect_equal(nrow(p), nrow(new_df) * 2L)

  })

  it("is silent when intervals are not requested", {

    warns <- testthat::capture_warnings(
      predict(selected_fixture, new_df, interval = FALSE)
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

  it("is silent on a selected fit with no UQ, where no intervals are returned", {

    warns <- testthat::capture_warnings(
      p <- predict(selected_no_uq, new_df, interval = TRUE)
    )

    expect_false(any(grepl("Conformal coverage", warns)))
    expect_false(".pred_lower" %in% names(p))

  })

  it("is silent when the fit carried no selection", {

    warns <- testthat::capture_warnings(
      predict(fitted_fixture, new_df, interval = TRUE)
    )

    expect_false(any(grepl("Conformal coverage", warns)))

  })

})


## ---------------------------------------------------------------------------
## Configs that use a covariate (2026-09-21)
## ---------------------------------------------------------------------------
## build_recipe() promotes a config's requested covariates to `predictor`, so
## they are in that workflow's blueprint and required at forge time.
## resolve_new_data() used to strip every covariate from new data by role,
## which aborted predict() for any config that used one.

## New data as a horizons_data — the branch that selects columns by role.
make_new_hd <- function(include_cov = TRUE, n = 8, n_wn = 10, seed = 7) {

  set.seed(seed)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  m        <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(m) <- wn_names

  df <- tibble::as_tibble(m)
  df$sample_id <- paste0("NEW", seq_len(n))

  roles <- tibble::tibble(
    variable = c("sample_id", wn_names),
    role     = c("id", rep("predictor", n_wn))
  )

  if (include_cov) {

    df$clay <- runif(n, 10, 40)
    roles   <- rbind(roles, tibble::tibble(variable = "clay",
                                           role     = "covariate"))

  }

  obj <- list(
    data = list(analysis = df, role_map = roles, n_rows = n,
                n_predictors = n_wn,
                n_covariates = as.integer(include_cov), n_responses = 0L)
  )

  class(obj) <- c("horizons_data", "list")
  obj

}

describe("resolve_new_data() - covariate columns", {

  it("drops covariates by default", {

    out <- resolve_new_data(make_new_hd(include_cov = TRUE))
    expect_false("clay" %in% names(out))

  })

  it("keeps the ones the fitted model needs", {

    out <- resolve_new_data(make_new_hd(include_cov = TRUE),
                            keep_extra = "clay")

    expect_true("clay" %in% names(out))
    expect_true("sample_id" %in% names(out))

  })

  it("keeps stripping covariates the fit does not use", {

    out <- resolve_new_data(make_new_hd(include_cov = TRUE),
                            keep_extra = "some_other_covariate")

    expect_false("clay" %in% names(out))

  })

})

describe("predict.horizons_fit() - config with a covariate", {

  cov_fit <- suppressWarnings(
    fit(make_predict_eval(n = 150, covariates = "clay"),
        n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE)
  )

  it("names the covariate as a required column the fit uses", {

    expect_equal(fitted_extra_predictors(cov_fit, "cfg_001",
                                         include_blueprint = FALSE),
                 "clay")

  })

  it("predicts on new data that carries the covariate", {

    p <- predict(cov_fit, make_new_hd(include_cov = TRUE), interval = FALSE)

    expect_equal(nrow(p), 8)
    expect_true(all(is.finite(p$.pred)))

  })

  it("errors naming the column when new data lacks the covariate", {

    expect_error(
      predict(cov_fit, make_new_hd(include_cov = FALSE), interval = FALSE),
      "clay"
    )

    expect_error(
      predict(cov_fit, make_new_hd(include_cov = FALSE), interval = FALSE),
      "covariate"
    )

  })

})


## ---------------------------------------------------------------------------
## abort_on_missing_predict_packages() — missing-package gate (#65)
## ---------------------------------------------------------------------------

describe("abort_on_missing_predict_packages() - missing package gate", {

  it("aborts cleanly, naming the package and the model that needs it", {

    err <- tryCatch(
      abort_on_missing_predict_packages(list(horizonsFakePkgXYZ123 = "cubist")),
      horizons_missing_predict_package = function(e) e
    )

    expect_s3_class(err, "horizons_missing_predict_package")
    expect_match(conditionMessage(err), "horizonsFakePkgXYZ123")
    expect_match(conditionMessage(err), "cubist")

  })

  it("collects every missing package and aborts once, not on the first miss", {

    err <- tryCatch(
      abort_on_missing_predict_packages(list(
        horizonsFakePkgXYZ123 = "cubist",
        horizonsFakePkgABC456 = "mars"
      )),
      horizons_missing_predict_package = function(e) e
    )

    expect_s3_class(err, "horizons_missing_predict_package")
    expect_match(conditionMessage(err), "horizonsFakePkgXYZ123")
    expect_match(conditionMessage(err), "horizonsFakePkgABC456")

  })

  it("does not abort when every needed package is installed", {

    expect_no_error(abort_on_missing_predict_packages(list(ranger = "rf")))

  })

  it("is a no-op for an empty needed list", {

    expect_no_error(abort_on_missing_predict_packages(list()))

  })

})


## ---------------------------------------------------------------------------
## predict_package_install_hint() — CRAN vs. Bioconductor (#65)
## ---------------------------------------------------------------------------
## mixOmics is on Bioconductor; install.packages() cannot find it. Tested
## directly (a pure function) rather than through the abort path, since
## mixOmics may well be installed wherever this runs, which would make the
## abort path never fire and prove nothing either way.

describe("predict_package_install_hint()", {

  it("gives mixOmics a BiocManager install hint", {

    expect_identical(predict_package_install_hint("mixOmics"),
                     'BiocManager::install("mixOmics")')

  })

  it("gives an ordinary CRAN package the install.packages() hint", {

    expect_identical(predict_package_install_hint("ranger"),
                     'install.packages("ranger")')

  })

  it("gives bonsai the ordinary CRAN hint, not a Bioconductor one", {

    ## bonsai (lightgbm's engine-registering package) is on CRAN, unlike
    ## mixOmics — confirm it is not accidentally special-cased.
    expect_identical(predict_package_install_hint("bonsai"),
                     'install.packages("bonsai")')

  })

})


## ---------------------------------------------------------------------------
## compute_needed_predict_packages() — pure model -> package mapping (#65)
## ---------------------------------------------------------------------------

describe("compute_needed_predict_packages()", {

  it("matches MODEL_PREDICT_PACKAGES for a single model", {

    expect_identical(compute_needed_predict_packages("rf"), list(ranger = "rf"))

  })

  it("unions packages across models and dedupes reasons", {

    needed <- compute_needed_predict_packages(c("rf", "cubist", "rf"))

    expect_setequal(names(needed), c("ranger", "rules", "Cubist"))
    expect_identical(needed$ranger, "rf")

  })

  it("adds ranger, reasoned as prediction intervals, only when asked", {

    without_uq <- compute_needed_predict_packages("elastic_net")
    with_uq    <- compute_needed_predict_packages("elastic_net",
                                                   needs_ranger_for_uq = TRUE)

    expect_false("ranger" %in% names(without_uq))
    expect_true("ranger" %in% names(with_uq))
    expect_identical(with_uq$ranger, "prediction intervals")

  })

})


## ---------------------------------------------------------------------------
## ensure_predict_namespaces() — per-model coverage and scoping (#65)
## ---------------------------------------------------------------------------
## Every model horizons supports maps to at least one predict-time namespace
## (MODEL_PREDICT_PACKAGES, R/constants.R). Those packages are Suggests, so
## whether they are installed varies by machine — assert clean success when
## installed, and an informative, package-naming abort when not, rather than
## assuming a fully-installed environment.

make_ns_probe_fit <- function(model, config_id = "cfg1", with_uq = FALSE,
                              extra_configs = NULL) {

  configs <- tibble::tibble(config_id = config_id, model = model)

  if (!is.null(extra_configs)) {

    configs <- dplyr::bind_rows(configs, extra_configs)

  }

  obj <- list(
    models = list(
      workflows = stats::setNames(as.list(rep(TRUE, nrow(configs))),
                                  configs$config_id),
      uq        = if (with_uq) {
        stats::setNames(list(list(quantile_model = TRUE)), config_id)
      } else {
        NULL
      }
    ),
    config = list(configs = configs)
  )

  class(obj) <- "horizons_fit"
  obj

}

describe("ensure_predict_namespaces() - per-model coverage", {

  it("MODEL_PREDICT_PACKAGES covers exactly the supported models", {

    expect_setequal(names(MODEL_PREDICT_PACKAGES), VALID_MODELS)

  })

  for (m in VALID_MODELS) {

    pkgs          <- MODEL_PREDICT_PACKAGES[[m]]
    all_installed <- all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))

    it(paste0("model '", m, "' loads its engine package(s), or aborts naming them"), {

      obj <- make_ns_probe_fit(m)

      if (all_installed) {

        expect_no_error(ensure_predict_namespaces(obj, config_ids = "cfg1"))
        for (pkg in pkgs) expect_true(requireNamespace(pkg, quietly = TRUE))

      } else {

        err <- tryCatch(
          ensure_predict_namespaces(obj, config_ids = "cfg1"),
          horizons_missing_predict_package = function(e) e
        )

        expect_s3_class(err, "horizons_missing_predict_package")
        expect_true(any(vapply(
          pkgs, function(p) grepl(p, conditionMessage(err), fixed = TRUE), logical(1)
        )))

      }

    })

  }

  it("also loads ranger when the object carries a per-config UQ bundle", {

    skip_if_not_installed("ranger")

    obj <- make_ns_probe_fit("elastic_net", with_uq = TRUE)

    expect_no_error(ensure_predict_namespaces(obj, config_ids = "cfg1"))
    expect_true(requireNamespace("ranger", quietly = TRUE))

  })

  it("does not require ranger for a horizons_ensemble's own predict path", {

    ## An ensemble inherits $models$uq from the underlying fit but never
    ## consults it at predict time (ensemble intervals are CV+ fold refits,
    ## not a quantile forest; see the roxygen on ensure_predict_namespaces()).
    ## glmnet is a hard Import, so this must succeed regardless of whether
    ## ranger happens to be installed.
    obj <- make_ns_probe_fit("elastic_net", with_uq = TRUE)
    obj$ensemble <- list(weights = tibble::tibble(member = "cfg1"))
    class(obj) <- c("horizons_ensemble", "horizons_fit")

    expect_no_error(ensure_predict_namespaces(obj, config_ids = "cfg1"))

  })

  it("scopes the engine check to the configs actually being predicted", {

    ## A fit that stores both an rf config (being predicted) and a mars
    ## config (not) must not require mars's engine just because the object
    ## also holds a mars config somewhere — predict(fit, config = "cfg_rf")
    ## must not need it installed to predict the rf one. mars is mapped to a
    ## package that is never installed, so the check below sees a missing
    ## package wherever it runs, whether or not earth is installed.
    unavailable <- MODEL_PREDICT_PACKAGES
    unavailable[["mars"]] <- "horizonsFakePkgXYZ123"
    local_mocked_bindings(MODEL_PREDICT_PACKAGES = unavailable)

    obj <- make_ns_probe_fit(
      "rf", config_id = "cfg_rf",
      extra_configs = tibble::tibble(config_id = "cfg_mars", model = "mars")
    )

    ## ranger is a hard Import, so scoping to cfg_rf alone must succeed
    ## regardless of what else the object stores.
    expect_no_error(ensure_predict_namespaces(obj, config_ids = "cfg_rf"))

    ## An unscoped check (every stored config, the pre-fix behaviour) aborts
    ## naming mars's package even though only cfg_rf was ever requested.
    expect_error(
      ensure_predict_namespaces(obj, config_ids = c("cfg_rf", "cfg_mars")),
      "horizonsFakePkgXYZ123",
      class = "horizons_missing_predict_package"
    )

  })

})


## ---------------------------------------------------------------------------
## predict_intervals() — failure now warns instead of degrading silently (#65)
## ---------------------------------------------------------------------------
## Before this fix, any failure in the bake-then-quantile-predict step (e.g. a
## missing ranger namespace on the quantile forest) degraded to NULL with no
## message, so `interval = TRUE` silently came back with no interval columns.

describe("predict_intervals() - failure warns instead of degrading silently", {

  new_df <- make_new_spectra()
  point_pred <- rep(5, nrow(new_df))

  cfg_id <- names(fitted_fixture$models$workflows)[1]
  uq     <- fitted_fixture$models$uq[[cfg_id]]

  it("the fixture carries a UQ bundle, so the gate is exercised", {

    expect_false(is.null(uq))

  })

  it("warns and returns NULL when the quantile model cannot predict", {

    broken_uq <- uq
    broken_uq$quantile_model <- structure(list(), class = "not_a_real_model")

    result <- NULL

    expect_warning(
      result <- predict_intervals(broken_uq, point_pred, new_df),
      class = "horizons_interval_warning"
    )

    expect_null(result)

  })

  it("warns and returns NULL when baking new data through the UQ recipe fails", {

    broken_uq <- uq
    broken_uq$prepped_recipe <- "not a recipe"

    result <- NULL

    expect_warning(
      result <- predict_intervals(broken_uq, point_pred, new_df),
      class = "horizons_interval_warning"
    )

    expect_null(result)

  })

  it("end to end: predict(interval = TRUE) warns and still returns point predictions", {

    broken_fixture <- fitted_fixture
    broken_fixture$models$uq[[cfg_id]]$quantile_model <-
      structure(list(), class = "not_a_real_model")

    p <- NULL

    expect_warning(
      p <- predict(broken_fixture, new_df, interval = TRUE),
      class = "horizons_interval_warning"
    )

    expect_true(".pred" %in% names(p))
    expect_false(".pred_lower" %in% names(p))

  })

})


## ---------------------------------------------------------------------------
## Row alignment before positional binds (#140)
## ---------------------------------------------------------------------------
## predict_one_config() attaches .pred, the interval columns and the AD columns
## to new_data's sample_id by position, and abstain_ood blanks rows by the same
## alignment. A step upstream that dropped a row has to stop predict(), not
## shift every later value onto another sample. bind_cols() and tibble() already
## refused most size mismatches with an unrelated error, but recycled a single
## row across the batch, and the interval arithmetic recycled any vector whose
## length divides the batch.

describe("predict() - row alignment before positional binds", {

  new_df <- make_new_spectra()
  n_new  <- nrow(new_df)
  cfg_id <- names(fitted_fixture$models$workflows)[1]

  it("aborts when the point predictions are a row short", {

    ## A recipe step that filtered rows at bake would do this.
    local_mocked_s3_method("predict", "workflow", function(object, new_data, ...) {
      tibble::tibble(.pred = rep(5, nrow(new_data) - 1L))
    })

    expect_error(predict(fitted_fixture, new_df, interval = FALSE),
                 "Point predictions for config", class = "horizons_internal_error")

  })

  it("aborts when the interval columns are a row short", {

    local_mocked_bindings(predict_intervals = function(...) {
      tibble::tibble(.pred_lower = rep(1, n_new - 1L), .pred_upper = rep(9, n_new - 1L),
                     .interval_width = rep(8, n_new - 1L))
    })

    expect_error(predict(fitted_fixture, new_df, interval = TRUE),
                 "Interval columns for config", class = "horizons_internal_error")

  })

  it("aborts when a single AD row would be recycled across the batch and abstained on", {

    skip_if_not(has_ad(fitted_fixture))

    ## One OOD row recycled by bind_cols() used to blank every prediction.
    local_mocked_bindings(predict_ad = function(...) {
      tibble::tibble(.ad_distance = 99,
                     .ad_flag     = factor("OOD", levels = c("Q1", "Q2", "Q3", "Q4", "OOD")))
    })

    expect_error(predict(fitted_fixture, new_df, interval = FALSE, abstain_ood = TRUE),
                 "Applicability-domain columns for config",
                 class = "horizons_internal_error")

  })

  it("is silent when every bind lines up", {

    expect_no_error(p <- predict(fitted_fixture, new_df, interval = TRUE))
    expect_identical(p$sample_id, new_df$sample_id)

  })

})

describe("predict_intervals() - row alignment before the bounds are assembled", {

  new_df <- make_new_spectra()
  cfg_id <- names(fitted_fixture$models$workflows)[1]
  uq     <- fitted_fixture$models$uq[[cfg_id]]

  it("aborts when point_pred does not have one value per row of new_data", {

    ## Half the rows: point_pred + q_low recycled this without an error.
    expect_error(predict_intervals(uq, rep(5, nrow(new_df) / 2), new_df, config_id = cfg_id),
                 "Point predictions for config", class = "horizons_internal_error")

  })

  it("aborts when the bake through the UQ recipe drops rows, rather than degrading", {

    real_bake <- recipes::bake

    local_mocked_bindings(
      bake = function(object, new_data, ...) {
        real_bake(object, new_data = new_data[seq_len(nrow(new_data) / 2), ], ...)
      },
      .package = "recipes"
    )

    expect_error(predict_intervals(uq, rep(5, nrow(new_df)), new_df),
                 "Features baked through the UQ recipe",
                 class = "horizons_internal_error")

  })

  it("is silent on the ordinary path", {

    expect_no_error(out <- predict_intervals(uq, rep(5, nrow(new_df)), new_df))
    expect_identical(nrow(out), nrow(new_df))

  })

})


## ---------------------------------------------------------------------------
## warn_interval_failure() — config naming and brace-safety (#65 review)
## ---------------------------------------------------------------------------
## `detail` (the upstream error message) is untrusted, possibly data-derived
## text — a column name like a bare `{wn_600}` is a realistic example. Handed
## to cli as a TEMPLATE rather than a VALUE, a brace or an unmatched quote in
## that text crashes predict() itself, rather than just being reported.

describe("warn_interval_failure() - config naming and brace-safety", {

  it("does not crash when the underlying error message contains braces", {

    brace_error <- simpleError('unexpected column `{wn_600}` in new_data')

    warns <- NULL

    expect_no_error(
      warns <- testthat::capture_warnings(
        warn_interval_failure("baking new data through the UQ recipe", brace_error)
      )
    )

    expect_true(any(grepl("{wn_600}", warns, fixed = TRUE)))

  })

  it("names the config in the warning when one is given", {

    expect_warning(
      warn_interval_failure("predicting quantiles from the UQ model",
                            simpleError("boom"), config_id = "cfg_001"),
      regexp = "cfg_001",
      class = "horizons_interval_warning"
    )

  })

  it("omits any config mention when none is given", {

    warns <- testthat::capture_warnings(
      warn_interval_failure("predicting quantiles from the UQ model",
                            simpleError("boom"))
    )

    expect_false(any(grepl("config", warns, ignore.case = TRUE)))

  })

})


## ---------------------------------------------------------------------------
## Fresh-process round trip (#65)
## ---------------------------------------------------------------------------
## The bug: predict() on a deserialized horizons_fit fails unless workflows
## (and, when a config's model needs one, its engine package) is already
## loaded — a plain `library(horizons)` on an installed build does not load
## them (see the roxygen on ensure_predict_namespaces(), R/pipeline-predict.R;
## verified directly with `Rscript --vanilla` against a from-scratch
## `R CMD INSTALL` with the fix reverted: predict() failed with exactly the
## issue's "no applicable method for 'predict'" error, and succeeded with the
## fix restored). Reproduced here the way a deployed model actually gets
## used: saveRDS() a fit, predict from it in a genuinely fresh process via
## callr, with no namespace preloaded.
##
## The fixture is elastic_net (glmnet — a hard Import, always available),
## not rf: parsnip's own predict.model_fit() dispatch loads an rf's ranger
## engine as a side effect of the point prediction alone (MODEL_PREDICT_
## PACKAGES's rationale, R/constants.R), which would make the interval
## columns come back even if ensure_predict_namespaces()'s ranger-for-UQ
## branch were broken or missing — the point-prediction step would have
## already loaded ranger first, for an unrelated reason, and silently
## covered for it. elastic_net's own predict path never touches ranger, so
## this fixture genuinely exercises that branch: with the fix reverted,
## point predictions alone would still succeed (glmnet needs no extra
## namespace), and only the interval half would fail.
##
## Only meaningful against an installed build — pkgload::load_all() (the
## natural way to run the CURRENT dev tree in a callr child) turns out to
## eagerly load workflows/ranger/tune/xgboost/butcher as part of simulating
## the installed package, even with the #65 fix reverted, so a load_all()
## child cannot independently re-trigger the missing-namespace defect
## (confirmed directly while writing this test, by trying exactly that).
## devtools::test() always load_all()s first, with no way to opt out — "run
## devtools::test() after devtools::install()" does NOT avoid this, since
## devtools::test() ignores what happens to be installed and load_all()s
## regardless. So rather than ship a branch that always passes without
## testing anything, this test skips under load_all() (skip_if_dev_package(),
## from helper-load-all.R, shared with test-evaluate-parallel.R) and runs
## only where the package is genuinely just installed and not load_all()'d:
## R CMD check, or testthat::test_local(load_package = "installed").

describe("predict.horizons_fit() - fresh-process round trip", {

  testthat::skip_if_not_installed("callr")
  testthat::skip_on_cran()
  skip_if_dev_package()

  elastic_net_fixture <- fit(make_predict_eval(model = "elastic_net"),
                             n_best = 1L, compute_uq = TRUE, verbose = FALSE)

  skip_if_not(has_uq(elastic_net_fixture), "fixture carries no UQ bundle")

  fit_path <- tempfile(fileext = ".rds")
  saveRDS(elastic_net_fixture, fit_path)

  new_df <- make_new_spectra()

  run_in_fresh_process <- function(interval) {

    callr::r(
      func = function(fit_path, new_df, interval) {

        library(horizons)

        predict(readRDS(fit_path), new_df, interval = interval)

      },
      args = list(fit_path = fit_path, new_df = new_df, interval = interval)
    )

  }

  it("predicts point estimates with no namespace preloaded in the caller", {

    skip_unless_slow_tier()

    p <- run_in_fresh_process(interval = FALSE)

    expect_true(".pred" %in% names(p))
    expect_equal(nrow(p), nrow(new_df))
    expect_true(all(is.finite(p$.pred)))

  })

  it("predicts intervals with no namespace preloaded in the caller", {

    p <- run_in_fresh_process(interval = TRUE)

    expect_true(all(c(".pred_lower", ".pred_upper", ".interval_width") %in% names(p)))
    expect_true(all(p$.pred_lower <= p$.pred_upper))

  })

})
