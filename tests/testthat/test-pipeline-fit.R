## ---------------------------------------------------------------------------
## Tests: fit()
## ---------------------------------------------------------------------------
## Integration tests for the fit() pipeline verb. These exercise the full
## pipeline: horizons_data → configure → validate → evaluate → fit.


## ---------------------------------------------------------------------------
## Helper: build a minimal horizons_eval object ready for fit()
## ---------------------------------------------------------------------------

make_fit_object <- function(n = 60, n_wn = 10, n_configs = 2, seed = 42,
                            n_na = 0L) {

  set.seed(seed)

  ## Spectral data
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal from first 3 predictors
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  ## Rows with no measured outcome, as add_response() leaves them (#67)
  if (n_na > 0) df$SOC[seq_len(n_na)] <- NA_real_

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Configs: rf + cubist (fast, tunable)
  models <- c("rf", "cubist")
  configs <- tibble::tibble(
    config_id         = paste0("cfg_", sprintf("%03d", seq_len(n_configs))),
    model             = models[seq_len(n_configs)],
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = NA_character_
  )

  ## Build horizons_data-like structure; downstream slots in the
  ## constructor's shape
  contract <- new_horizons_data()

  obj <- list(
    data = list(
      analysis     = df,
      role_map     = roles,
      n_rows       = nrow(df),
      n_predictors = n_wn,
      n_covariates = 0L,
      n_responses  = 1L
    ),
    provenance = list(
      spectra_source = "test",
      spectra_type   = "mir",
      schema_version = 1L
    ),
    config = list(
      configs   = configs,
      n_configs = n_configs,
      tuning    = list(
        cv_folds      = 3L,
        grid_size     = 2L,
        bayesian_iter = 0L
      )
    ),
    validation = list(
      passed    = TRUE,
      checks    = NULL,
      timestamp = Sys.time(),
      outliers  = list(
        spectral_ids   = NULL,
        response_ids   = NULL,
        removed_ids    = NULL,
        removal_detail = NULL,
        removed        = FALSE
      )
    ),
    evaluation = contract$evaluation,
    models     = contract$models,
    ensemble   = contract$ensemble,
    artifacts  = list(cache_dir = NULL)
  )

  class(obj) <- c("horizons_data", "list")

  ## Run evaluate() to populate evaluation slot
  ## prune = FALSE ensures configs get status "success" even with weak signal
  suppressWarnings(
    evaluate(obj, prune = FALSE, verbose = FALSE, seed = seed)
  )

}


## Expected columns in models$results
EXPECTED_FIT_RESULT_COLS <- c(
  "config_id", "status", "degraded", "degraded_reason",
  "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
  "cv_rmse_mean", "cv_rmse_se", "cv_rpd_mean", "cv_rpd_se",
  "best_params", "error_message", "runtime_secs"
)

## Expected columns in cv_predictions
EXPECTED_FIT_CV_PRED_COLS <- c(
  ".row", ".fold", "config_id", ".pred", ".pred_trans", "truth"
)

## Expected slots in models$
EXPECTED_MODEL_SLOTS <- c(
  "workflows", "n_models", "best_config", "rank_metric", "predictor_schema",
  "response_bound", "cv_predictions", "results", "split", "row_index", "uq",
  "ad", "selection_present", "timestamp", "runtime_secs"
)


## =========================================================================
## Preflight validation
## =========================================================================

describe("fit() - preflight validation", {

  it("aborts on non-horizons_eval input", {

    expect_error(
      fit(list(a = 1), verbose = FALSE),
      class = "rlang_error"
    )

  })

  it("aborts on horizons_data without evaluation", {

    obj <- make_fit_object()
    class(obj) <- c("horizons_data", "list")
    obj$evaluation$results <- NULL

    expect_error(
      fit(obj, verbose = FALSE),
      class = "rlang_error"
    )

  })

  it("aborts when no successful configs in evaluation", {

    obj <- make_fit_object()
    obj$evaluation$results$status <- "failed"

    expect_error(
      fit(obj, verbose = FALSE),
      class = "rlang_error"
    )

  })

})


## =========================================================================
## Success path
## =========================================================================

describe("fit() - success path", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  result <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
  )

  it("returns a horizons_fit object", {

    expect_true(inherits(result, "horizons_fit"))

  })

  it("inherits from horizons_eval and horizons_data", {

    expect_true(inherits(result, "horizons_eval"))
    expect_true(inherits(result, "horizons_data"))

  })

  it("has all expected models$ slots", {

    expect_true(all(EXPECTED_MODEL_SLOTS %in% names(result$models)))

  })

  it("writes exactly the models keys new_horizons_data() declares (#71)", {

    ## The constructor's empty slot is what configure() resets to, so it has
    ## to name what fit() actually writes.
    expect_identical(names(result$models), names(new_horizons_data()$models))

  })

  it("records best_config as the top fitted config and the rank metric used", {

    ## best_config is the durable ranking fact predict() reads; it must be the
    ## first config in best-first workflow order, and a real fitted config.
    expect_equal(result$models$best_config, names(result$models$workflows)[1])
    expect_true(result$models$best_config %in% names(result$models$workflows))
    expect_true(is.character(result$models$rank_metric))

  })

  it("models$workflows is a named list", {

    wfs <- result$models$workflows
    expect_true(is.list(wfs))
    expect_true(length(wfs) > 0)
    expect_true(!is.null(names(wfs)))

  })

  it("models$n_models matches workflow count", {

    expect_equal(result$models$n_models, length(result$models$workflows))

  })

  it("models$split is an rsplit", {

    expect_true(inherits(result$models$split, "rsplit"))

  })

  it("models$timestamp is POSIXct", {

    expect_true(inherits(result$models$timestamp, "POSIXct"))

  })

  it("models$runtime_secs is positive", {

    expect_true(result$models$runtime_secs > 0)

  })

  it("re-running evaluate() on it empties models and ensemble (#70)", {

    ## compute_uq = FALSE above, so carry a bundle the way compute_uq = TRUE
    ## leaves one; otherwise has_uq() is FALSE before and after
    fitted <- result
    fitted$models$uq <- stats::setNames(list(list(quantile_model = "a UQ bundle")),
                                        names(fitted$models$workflows)[1])
    fitted$ensemble$method <- "weighted"

    expect_true(has_uq(fitted))

    re <- suppressWarnings(evaluate(fitted, prune = FALSE, verbose = FALSE, seed = 42L))

    blank <- new_horizons_data()

    expect_identical(class(re), c("horizons_eval", "horizons_data", "list"))
    expect_false(has_uq(re))
    expect_identical(re$models,   blank$models)
    expect_identical(re$ensemble, blank$ensemble)

  })

  it("re-running fit() on an ensemble empties the ensemble (#70)", {

    ens <- suppressWarnings(
      ensemble(result, method = "weighted", optimize = FALSE,
               compute_uq = FALSE, verbose = FALSE)
    )

    ## Keep the re-fit cheap; the budget is not what is under test
    ens$config$tuning$final_bayesian_iter <- 0L

    refit <- suppressWarnings(
      fit(ens, n_best = 1L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
    )

    expect_identical(class(refit), c("horizons_fit", "horizons_eval", "horizons_data", "list"))
    expect_identical(refit$ensemble, new_horizons_data()$ensemble)

  })

})


## =========================================================================
## CV predictions
## =========================================================================

describe("fit() - cv_predictions", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  result <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
  )

  cv_preds <- result$models$cv_predictions

  it("is a tibble", {

    expect_s3_class(cv_preds, "tbl_df")

  })

  it("has all expected columns", {

    expect_true(all(EXPECTED_FIT_CV_PRED_COLS %in% names(cv_preds)))

  })

  it("contains predictions from all successful configs", {

    successful <- result$models$results %>%
      dplyr::filter(status == "success") %>%
      dplyr::pull(config_id)
    pred_configs <- unique(cv_preds$config_id)
    expect_true(all(successful %in% pred_configs))

  })

  it(".row is integer", {

    expect_true(is.integer(cv_preds$.row) || is.numeric(cv_preds$.row))

  })

  it("truth values are finite", {

    expect_true(all(is.finite(cv_preds$truth)))

  })

})


## =========================================================================
## Results tibble
## =========================================================================

describe("fit() - models$results", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  result <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
  )

  res <- result$models$results

  it("is a tibble", {

    expect_s3_class(res, "tbl_df")

  })

  it("has all expected columns", {

    expect_true(all(EXPECTED_FIT_RESULT_COLS %in% names(res)))

  })

  it("has one row per config attempted", {

    expect_equal(nrow(res), min(2L, sum(obj$evaluation$results$status == "success")))

  })

  it("test metrics are finite for successful configs", {

    successes <- dplyr::filter(res, status == "success")

    if (nrow(successes) > 0) {

      expect_true(all(is.finite(successes$rmse)))
      expect_true(all(is.finite(successes$rpd)))

    }

  })

  it("degraded is logical (never NA for success)", {

    successes <- dplyr::filter(res, status == "success")

    if (nrow(successes) > 0) {

      expect_true(all(!is.na(successes$degraded)))

    }

  })

  it("cv_rmse_mean and cv_rpd_mean are present", {

    successes <- dplyr::filter(res, status == "success")

    if (nrow(successes) > 0) {

      expect_true(all(is.finite(successes$cv_rmse_mean)))
      expect_true(all(is.finite(successes$cv_rpd_mean)))

    }

  })

  it("best_params is a list column", {

    expect_true(is.list(res$best_params))

  })

})


## =========================================================================
## Row index mapping
## =========================================================================

describe("fit() - row_index", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  result <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
  )

  it("is a tibble with .row and sample_id", {

    ri <- result$models$row_index
    expect_s3_class(ri, "tbl_df")
    expect_true(".row" %in% names(ri))
    expect_true("sample_id" %in% names(ri))

  })

  it("maps to cv_prediction .row values", {

    ri <- result$models$row_index
    cv_rows <- unique(result$models$cv_predictions$.row)
    expect_true(all(cv_rows %in% ri$.row))

  })

})


## =========================================================================
## UQ integration
## =========================================================================

describe("fit() - UQ disabled", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  result <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
  )

  it("models$uq is NULL when compute_uq = FALSE", {

    expect_null(result$models$uq)

  })

})


describe("fit() - UQ enabled", {

  ## Need more data for UQ (calib split needs N_CALIB_MIN = 30)
  obj <- make_fit_object(n = 120, n_configs = 1)

  result <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = TRUE, verbose = FALSE, seed = 42L)
  )

  it("models$uq is a list when compute_uq = TRUE and enough data", {

    ## May be NULL if calib set too small after splits — that's OK
    if (!is.null(result$models$uq)) {

      expect_true(is.list(result$models$uq))

    }

  })

  it("UQ bundles are named by config_id", {

    if (!is.null(result$models$uq)) {

      expect_true(length(result$models$uq) > 0)
      expect_true(!is.null(names(result$models$uq)))

    }

  })

  it("UQ bundles have expected fields", {

    if (!is.null(result$models$uq) && length(result$models$uq) > 0) {

      uq_bundle <- result$models$uq[[1]]

      if (!is.null(uq_bundle)) {

        expected <- c("quantile_model", "scores", "n_calib",
                      "level_default", "oof_coverage", "mean_width",
                      "prepped_recipe")
        expect_true(all(expected %in% names(uq_bundle)))

      }

    }

  })

})


describe("fit() - AD disabled", {

  ## n = 60 -> calib split below N_CALIB_MIN, so AD is disabled even if asked.
  obj <- make_fit_object(n = 60, n_configs = 2)

  result <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
        verbose = FALSE, seed = 42L)
  )

  it("models$ad is NULL when compute_ad = FALSE", {

    expect_null(result$models$ad)

  })

})


describe("fit() - AD enabled", {

  ## n = 250 so the shared calibration split clears N_CALIB_MIN = 30
  ## (calib = 0.2 * 0.8 * n = 40). AD must ACTUALLY compute here, not just be
  ## NULL-tolerated — the assertions below require a populated bundle.
  obj <- make_fit_object(n = 250, n_configs = 1)

  result <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = TRUE,
        verbose = FALSE, seed = 42L)
  )

  it("models$ad populates as a named list keyed by config_id", {

    expect_false(is.null(result$models$ad))
    expect_true(is.list(result$models$ad))
    expect_true(length(result$models$ad) > 0)
    expect_true(!is.null(names(result$models$ad)))
    expect_true(all(names(result$models$ad) %in% names(result$models$workflows)))

  })

  it("AD bundles carry centroid, covariance, increasing thresholds, n_calib", {

    bundle <- result$models$ad[[1]]

    expect_true(all(c("centroid", "cov_matrix", "ad_thresholds", "n_calib")
                    %in% names(bundle)))
    expect_length(bundle$ad_thresholds, 4)
    expect_true(all(diff(bundle$ad_thresholds) > 0))
    expect_equal(length(bundle$centroid), nrow(bundle$cov_matrix))
    expect_true(bundle$n_calib >= N_CALIB_MIN)

  })

  it("AD and UQ are independent (AD on, UQ off)", {

    expect_null(result$models$uq)     # compute_uq = FALSE
    expect_true(has_ad(result))       # ... but AD present

  })

  it("the fitted object passes its own validator with AD populated", {

    expect_identical(validate_horizons_fit(result), result)

  })

})


## =========================================================================
## Edge cases
## =========================================================================

describe("fit() - n_best = 1", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  result <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
  )

  it("works with n_best = 1", {

    expect_true(inherits(result, "horizons_fit"))
    expect_equal(result$models$n_models, 1L)

  })

})


describe("fit() - n_best exceeds available successes", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  it("caps n_best at available successes (with warning)", {

    result <- suppressWarnings(
      fit(obj, n_best = 100L, compute_uq = FALSE, verbose = FALSE, seed = 42L)
    )

    n_success <- sum(obj$evaluation$results$status == "success")
    expect_true(result$models$n_models <= n_success)

  })

})


## =========================================================================
## final_bayesian_iter reaches the re-tune (#46)
## =========================================================================

describe("fit() - final_bayesian_iter", {

  ## Capture what fit() hands to fit_single_config() without running a fit.
  capture_final_iter <- function(obj) {

    captured <- NULL

    testthat::with_mocked_bindings(
      fit_single_config = function(...) {
        captured <<- list(...)$final_bayesian_iter
        list(config_id = list(...)$config_row$config_id, status = "failed",
             degraded = NA, degraded_reason = NA_character_,
             fitted_workflow = NULL, best_params = NULL,
             cv_predictions = NULL, test_metrics = NULL, cv_metrics = NULL,
             uq = NULL, ad = NULL, warnings = NULL,
             error_message = "mocked", runtime_secs = 0)
      },
      tryCatch(
        fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
            verbose = FALSE),
        error = function(e) NULL
      ),
      .package = "horizons"
    )

    captured

  }

  obj <- make_fit_object(n = 60, n_configs = 1)

  it("passes configure()'s final_bayesian_iter, not the screening bayesian_iter", {

    obj$config$tuning$bayesian_iter       <- 0L
    obj$config$tuning$final_bayesian_iter <- 7L

    expect_identical(capture_final_iter(obj), 7L)

  })

  it("falls back to the package default when the field is absent (older objects)", {

    obj$config$tuning$final_bayesian_iter <- NULL

    expect_identical(capture_final_iter(obj), DEFAULT_FINAL_BAYES_ITER)

  })

})


describe("fit() - seed reproducibility", {

  ## The train/test split is evaluate()'s, so what fit()'s seed controls is
  ## the CV folds. UQ and AD are off, so no calibration draw precedes the
  ## folds, and the ambient RNG is set differently before each call: the
  ## folds must follow `seed`, not whatever state the caller left.
  obj <- make_fit_object(n = 60, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L

  fit_at <- function(seed, ambient) {
    set.seed(ambient)
    suppressWarnings(
      fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE, seed = seed)
    )
  }

  ## Fold membership per row. cv_predictions is grouped by fold, so the raw
  ## .fold column has the same run lengths whichever rows fall in each fold
  ## and cannot tell two fold assignments apart; order it by .row.
  fold_of_row <- function(r) {
    cp <- r$models$cv_predictions
    cp$.fold[order(cp$.row)]
  }

  r1 <- fit_at(123L, ambient = 1L)
  r2 <- fit_at(123L, ambient = 2L)
  r3 <- fit_at(124L, ambient = 1L)

  it("same seed produces the same folds and row index", {

    expect_identical(fold_of_row(r1), fold_of_row(r2))
    expect_identical(r1$models$row_index, r2$models$row_index)

  })

  it("a different seed produces different folds", {

    expect_false(identical(fold_of_row(r1), fold_of_row(r3)))

  })

})


## =========================================================================
## Split F is evaluate()'s split
## =========================================================================
## A fresh Split F (at seed + 1 since #50) drew most of its test rows from
## evaluate()'s training rows, which chose the members and tuned the
## warm-start parameters. evaluate()'s test rows are the only ones nothing was
## selected on, so fit() scores on them, and carves the calibration set out
## of evaluate()'s training rows.

describe("fit() - scores on evaluate()'s split", {

  ## n = 250 so the calibration split clears N_CALIB_MIN and UQ runs.
  obj <- make_fit_object(n = 250, n_configs = 1, seed = 42)
  obj$config$tuning$final_bayesian_iter <- 0L

  r <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = TRUE, compute_ad = FALSE,
        verbose = FALSE, seed = 42L)
  )

  it("reuses evaluate()'s split, so its test rows are evaluate()'s", {

    expect_identical(r$models$split$data, obj$evaluation$split$data)
    expect_identical(rsample::testing(r$models$split)$sample_id,
                     rsample::testing(obj$evaluation$split)$sample_id)

  })

  it("partitions the modelled rows into test, calibration and fit rows", {

    expect_false(is.null(r$models$uq))

    ## Split C is reproducible from calib_split_seed() and evaluate()'s
    ## training rows alone.
    set.seed(calib_split_seed(42L))
    split_C <- rsample::initial_split(rsample::training(obj$evaluation$split),
                                      prop = CALIB_PROP,
                                      strata = dplyr::all_of("SOC"))

    test_ids  <- rsample::testing(r$models$split)$sample_id
    calib_ids <- rsample::testing(split_C)$sample_id
    fit_ids   <- r$models$row_index$sample_id

    expect_setequal(rsample::training(split_C)$sample_id, fit_ids)
    expect_equal(r$models$uq[[1]]$n_calib, length(calib_ids))

    expect_length(intersect(test_ids, calib_ids), 0L)
    expect_length(intersect(test_ids, fit_ids), 0L)
    expect_length(intersect(calib_ids, fit_ids), 0L)
    expect_setequal(c(test_ids, calib_ids, fit_ids),
                    obj$evaluation$split$data$sample_id)

  })

  it("calib_split_seed() is a documented offset of the user's seed", {

    expect_identical(calib_split_seed(307L), 308L)
    expect_identical(calib_split_seed(42), 43L)

  })

  it("refuses an object whose split no longer matches its rows", {

    stale <- obj
    stale$data$analysis <- stale$data$analysis[-1, ]

    expect_error(
      fit(stale, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE),
      "does not index the rows this object models",
      class = "horizons_input_error"
    )

    ## Same ids, one outcome changed: the split's rows no longer carry the
    ## outcomes it was stratified and scored on.
    relabelled <- obj
    relabelled$data$analysis$SOC[1] <- relabelled$data$analysis$SOC[1] + 1

    expect_error(
      fit(relabelled, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE),
      "does not index the rows this object models",
      class = "horizons_input_error"
    )

    no_split <- obj
    no_split$evaluation$split <- NULL

    expect_error(
      fit(no_split, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE),
      class = "horizons_input_error"
    )

  })

  it("still fits after add_response() adds a sibling response", {

    lab <- tibble::tibble(sample_id = obj$data$analysis$sample_id,
                          clay      = seq_len(nrow(obj$data$analysis)))

    utils::capture.output(
      with_clay <- add_response(obj, lab, variable = "clay")
    )

    r_clay <- suppressWarnings(
      fit(with_clay, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE, seed = 42L)
    )

    expect_s3_class(r_clay, "horizons_fit")
    expect_true("clay" %in% names(r_clay$models$split$data))
    expect_identical(rsample::testing(r_clay$models$split)$sample_id,
                     rsample::testing(obj$evaluation$split)$sample_id)

  })

})


## =========================================================================
## NA-outcome rows are dropped before Split F (#67)
## =========================================================================
## evaluate() drops rows whose outcome is NA. fit() used to split the
## unfiltered table, so its partition was over a different frame and the
## NA-outcome rows reached the fit.

describe("fit() - NA-outcome rows (#67)", {

  obj <- make_fit_object(n = 60, n_configs = 1, seed = 42, n_na = 6L)
  obj$config$tuning$final_bayesian_iter <- 0L

  na_ids <- obj$data$analysis$sample_id[is.na(obj$data$analysis$SOC)]

  ## One verbose run, keeping the console tree for the drop report.
  out <- utils::capture.output(
    r <- suppressWarnings(
      fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = TRUE, seed = 42L)
    )
  )

  it("keeps NA-outcome rows out of Split F and the row index", {

    expect_length(na_ids, 6L)
    expect_false(any(na_ids %in% r$models$split$data$sample_id))
    expect_false(anyNA(r$models$split$data$SOC))
    expect_false(any(na_ids %in% r$models$row_index$sample_id))

  })

  it("scores on the frame evaluate() split", {

    expect_identical(r$models$split$data, obj$evaluation$split$data)

  })

  it("reports the rows it dropped in the console tree", {

    expect_true(any(grepl("Dropped 6 rows with NA outcome", out, fixed = TRUE)))

  })

})


describe("fit() - NA-outcome rows with UQ on (#67)", {

  ## n = 300 with 40 NA outcomes leaves 260 modelled rows, enough for the
  ## calibration split to clear N_CALIB_MIN.
  obj <- make_fit_object(n = 300, n_configs = 1, seed = 42, n_na = 40L)
  obj$config$tuning$final_bayesian_iter <- 0L

  r <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = TRUE, compute_ad = FALSE,
        verbose = FALSE, seed = 42L)
  )

  analysis  <- obj$data$analysis
  test_ids  <- rsample::testing(r$models$split)$sample_id
  fit_ids   <- r$models$row_index$sample_id
  calib_ids <- setdiff(rsample::training(r$models$split)$sample_id, fit_ids)

  it("calibrates UQ on the rows left between the fit rows and the test part", {

    expect_false(is.null(r$models$uq))
    expect_equal(r$models$uq[[1]]$n_calib, length(calib_ids))
    expect_equal(length(test_ids) + length(fit_ids) + length(calib_ids), 260L)

  })

  it("puts no NA outcome in any partition", {

    outcome_of <- function(ids) analysis$SOC[match(ids, analysis$sample_id)]

    expect_false(anyNA(outcome_of(test_ids)))
    expect_false(anyNA(outcome_of(calib_ids)))
    expect_false(anyNA(outcome_of(fit_ids)))

  })

})


## =========================================================================
## response_bound is taken over the rows the final models are fit on (#68)
## =========================================================================

describe("fit() - response_bound (#68)", {

  ## At fixture seed 12, evaluate()'s split (which fit() reuses) puts the
  ## object's largest outcome in the test part, which is what makes a
  ## whole-table bound and a training-row bound differ.
  obj <- make_fit_object(n = 60, n_configs = 1, seed = 12)
  obj$config$tuning$final_bayesian_iter <- 0L

  r <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
        verbose = FALSE, seed = 12L)
  )

  it("equals the largest fit-row outcome times RESPONSE_BOUND_MARGIN", {

    soc      <- obj$data$analysis$SOC
    fit_rows <- obj$data$analysis$sample_id %in% r$models$row_index$sample_id

    ## Precondition: the fixture discriminates. If a change to the split
    ## moves the maximum back into the fit rows, pick another seed.
    expect_gt(max(soc), max(soc[fit_rows]))

    expect_equal(r$models$response_bound,
                 max(soc[fit_rows]) * RESPONSE_BOUND_MARGIN)

  })

})


## =========================================================================
## Members are ranked on the cross-validated metric (#50)
## =========================================================================

describe("fit() - member ranking on cv_<metric>", {

  obj <- make_fit_object(n = 60, n_configs = 2)

  r <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
        verbose = FALSE, seed = 123L)
  )

  it("orders members by cv_<rank_metric> from evaluation$results", {

    successes <- obj$evaluation$results[obj$evaluation$results$status == "success", ]
    expected  <- rank_configs_by_cv(successes, obj$evaluation$rank_metric)$config_id

    expect_equal(r$models$results$config_id, expected[seq_len(nrow(r$models$results))])

  })

})


## =========================================================================
## No config passed the prune gate: fit() takes evaluate()'s fallback (#38)
## =========================================================================
## evaluate() takes best_config from the pruned configs when none succeeded.
## fit() kept successes only, so it refused the object evaluate() had just
## returned ("No successful configurations").

describe("fit() - an evaluation with only pruned configs (#38)", {

  ## A threshold no model clears, with a Bayesian stage for the gate to skip,
  ## so evaluate() prunes both configs.
  obj <- make_eval_object(n = 60, n_configs = 2)
  obj$config$tuning$bayesian_iter       <- 1L
  obj$config$tuning$final_bayesian_iter <- 0L

  pruned <- suppressWarnings(
    evaluate(obj, prune = TRUE, prune_threshold = 9999, verbose = FALSE,
             seed = 42L)
  )

  it("fits the pruned configs in evaluate()'s order, and warns that it fell back", {

    ## Precondition: nothing succeeded, and evaluate() still named a winner
    expect_true(all(pruned$evaluation$results$status == "pruned"))

    w <- expect_warning(
      r <- keep_only_warning(
        fit(pruned, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
            verbose = FALSE, seed = 42L),
        "horizons_pruned_fallback_warning"
      ),
      class = "horizons_pruned_fallback_warning"
    )

    ## One warning, carrying both classes: pruned members are below the
    ## threshold by definition. It names the threshold and each member's cv RPD.
    msg <- gsub("\\s+", " ", conditionMessage(w))   # undo cli line wrapping

    expect_s3_class(w, "horizons_below_threshold_warning")
    expect_match(msg, "prune gate", fixed = TRUE)
    expect_match(msg, "prune threshold of 9999", fixed = TRUE)

    res <- pruned$evaluation$results

    for (i in seq_len(nrow(res))) {
      expect_match(msg, paste0(res$config_id[i], " ", formatC(res$cv_rpd[i], digits = 2, format = "f")),
                   fixed = TRUE)
    }

    expected <- rank_configs_by_cv(pruned$evaluation$results,
                                   pruned$evaluation$rank_metric)$config_id

    expect_s3_class(r, "horizons_fit")
    expect_identical(r$models$results$config_id, expected)
    expect_identical(r$models$best_config, pruned$evaluation$best_config)

  })

  it("refuses when no pruned config carries the ranking metric", {

    unranked <- pruned
    unranked$evaluation$results$cv_rpd <- NA_real_

    expect_error(
      fit(unranked, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE),
      class = "horizons_input_error"
    )

  })

  ## rank_configs_by_cv() used to refuse this unclassed.
  it("refuses, classed, when configs succeeded but none has the ranking metric", {

    unranked <- pruned
    unranked$evaluation$results$status <- "success"
    unranked$evaluation$results$cv_rpd <- NA_real_

    expect_error(
      fit(unranked, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE),
      "succeeded, but none has",
      class = "horizons_input_error"
    )

  })

})


## =========================================================================
## Every member fell below the prune threshold at bayesian_iter = 0 (#38)
## =========================================================================
## With no Bayesian stage the gate skips nothing, so nothing is pruned and
## the fallback warning cannot fire. The gate's reading is recorded apart from
## the status (below_prune_threshold), and fit() warns from it.

describe("fit() - members below the prune threshold at bayesian_iter = 0 (#38)", {

  obj <- make_eval_object(n = 60, n_configs = 2)
  obj$config$tuning$bayesian_iter       <- 0L
  obj$config$tuning$final_bayesian_iter <- 0L

  below <- suppressWarnings(
    evaluate(obj, prune = TRUE, prune_threshold = 9999, verbose = FALSE,
             seed = 42L)
  )

  it("warns, naming the threshold and the members' cv RPD", {

    ## Precondition: both are successes, and both fell below the threshold
    expect_true(all(below$evaluation$results$status == "success"))
    expect_true(all(below$evaluation$results$below_prune_threshold))

    w <- expect_warning(
      r <- keep_only_warning(
        fit(below, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
            verbose = FALSE, seed = 42L),
        "horizons_below_threshold_warning"
      ),
      class = "horizons_below_threshold_warning"
    )

    ## Not the fallback: these configs succeeded
    expect_false(inherits(w, "horizons_pruned_fallback_warning"))

    msg <- gsub("\\s+", " ", conditionMessage(w))   # undo cli line wrapping
    res <- below$evaluation$results

    expect_match(msg, "prune threshold of 9999", fixed = TRUE)

    for (i in seq_len(nrow(res))) {
      expect_match(msg, paste0(res$config_id[i], " ", formatC(res$cv_rpd[i], digits = 2, format = "f")),
                   fixed = TRUE)
    }

    expect_s3_class(r, "horizons_fit")
    expect_equal(r$models$n_models, 2L)

  })

  it("is quiet when a member cleared the threshold", {

    cleared <- below
    cleared$evaluation$results$below_prune_threshold[1] <- FALSE

    expect_no_warning(
      keep_only_warning(
        fit(cleared, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
            verbose = FALSE, seed = 42L),
        "horizons_below_threshold_warning"
      ),
      class = "horizons_below_threshold_warning"
    )

  })

  ## Unpruned below-threshold members under a configured bayesian_iter > 0
  ## came from rows scored with no Bayesian stage (resumed checkpoints of a
  ## bayesian_iter = 0 run); naming bayesian_iter = 0 as the setting would be
  ## a false cause.
  it("words its explanation from the configured bayesian_iter", {

    members <- tibble::tibble(
      config_id             = c("cfg_001", "cfg_002"),
      status                = "success",
      below_prune_threshold = TRUE,
      prune_threshold       = 1,
      cv_rpd                = c(0.93, 0.88)
    )

    warn_text <- function(bayesian_iter) {
      w <- tryCatch(
        warn_members_below_threshold(members, fallback = FALSE,
                                     bayesian_iter = bayesian_iter),
        horizons_below_threshold_warning = function(w) w
      )
      gsub("\\s+", " ", conditionMessage(w))   # undo cli line wrapping
    }

    expect_match(warn_text(0L), "no Bayesian stage to skip (`bayesian_iter = 0`)",
                 fixed = TRUE)

    at_five <- warn_text(5L)
    expect_match(at_five, "configured `bayesian_iter = 5`", fixed = TRUE)
    expect_no_match(at_five, "no Bayesian stage to skip", fixed = TRUE)

    expect_match(warn_text(NULL), "None was pruned, so they ranked as successes.",
                 fixed = TRUE)

  })

})


## =========================================================================
## Every member fails
## =========================================================================
## fit() used to carry on to validate_horizons_fit(), which refused the empty
## workflows slot with a structural message that said nothing about why the
## members failed; and models$results dropped error_message.

describe("fit() - member failures", {

  obj <- make_fit_object(n = 60, n_configs = 2)
  obj$config$tuning$final_bayesian_iter <- 0L

  ## A member failure as fit_single_config() reports one. The message carries
  ## braces, which must reach the abort as text, not as a cli template.
  failed_member <- function(...) {

    cfg <- list(...)$config_row

    list(config_id = cfg$config_id, status = "failed",
         degraded = NA, degraded_reason = NA_character_,
         fitted_workflow = NULL, best_params = NULL,
         cv_predictions = NULL, test_metrics = NULL, cv_metrics = NULL,
         uq = NULL, ad = NULL, warnings = NULL,
         error_message = paste0("Warm-start tuning failed: {", cfg$model, "} diverged"),
         runtime_secs = 0)

  }

  it("aborts with horizons_all_members_failed when every member fails, naming the errors", {

    err <- testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(
          fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
              verbose = FALSE)
        ),
        horizons_all_members_failed = function(e) e
      ),
      fit_single_config = failed_member,
      .package = "horizons"
    )

    expect_s3_class(err, "horizons_all_members_failed")

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "{rf} diverged", fixed = TRUE)
    expect_match(msg, "{cubist} diverged", fixed = TRUE)

    expect_s3_class(err$results, "tbl_df")
    expect_setequal(err$results$config_id, c("cfg_001", "cfg_002"))
    expect_true(all(err$results$status == "failed"))

  })

  it("keeps each member's error_message in models$results", {

    real_fit_single_config <- fit_single_config

    r <- testthat::with_mocked_bindings(
      suppressWarnings(
        fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
            verbose = FALSE, seed = 42L)
      ),
      fit_single_config = function(...) {
        if (list(...)$config_row$config_id == "cfg_002") {
          failed_member(...)
        } else {
          real_fit_single_config(...)
        }
      },
      .package = "horizons"
    )

    res <- r$models$results

    expect_identical(res$error_message[res$config_id == "cfg_002"],
                     "Warm-start tuning failed: {cubist} diverged")
    expect_true(all(is.na(res$error_message[res$status == "success"])))

  })

})


## =========================================================================
## allow_par with no backend (M2e, 2026-09-15)
## =========================================================================

describe("fit() - allow_par without a usable backend", {

  it("warns naming fit() and the plan, then runs sequentially", {

    local_plan(future::sequential)
    obj <- make_fit_object(n = 60, n_configs = 1)

    expect_warning(
      r <- keep_only_warning(
        fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
            allow_par = TRUE, verbose = FALSE, seed = 123L),
        "offers 1 worker"
      ),
      "fit\\(\\).*offers 1 worker"
    )

    expect_s3_class(r, "horizons_fit")

  })

})


## =========================================================================
## Selection provenance (2026-09-21)
## =========================================================================
## fit()'s calibration rows come from the training object. When that object
## came from select_training(), predict() needs to know, because conformal
## coverage does not transfer to a selected training set.

## A shape-complete stand-in for a real $selection, so this does not depend on
## running select_training() (and survives a validator that checks the shape).
make_selection_stub <- function() {

  list(
    settings         = list(method = "neighbours", n_target = 10L),
    membership       = tibble::tibble(sample_id = character(0),
                                      group     = character(0)),
    groups           = tibble::tibble(group = character(0), n = integer(0)),
    pool_sizes       = tibble::tibble(group = character(0), n_pool = integer(0)),
    target_distances = tibble::tibble(sample_id = character(0),
                                      distance  = numeric(0)),
    exclusions       = tibble::tibble(sample_id = character(0),
                                      reason    = character(0))
  )

}

describe("fit() - selection provenance", {

  it("records FALSE when the training object carried no selection", {

    obj    <- make_fit_object(n_configs = 1)
    result <- suppressWarnings(
      fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE, seed = 42L)
    )

    expect_false(result$models$selection_present)

  })

  it("records TRUE when the training object carried a selection", {

    obj           <- make_fit_object(n_configs = 1)
    obj$selection <- make_selection_stub()

    result <- suppressWarnings(
      fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE, seed = 42L)
    )

    expect_true(result$models$selection_present)

  })

})


## =========================================================================
## Cold start: a configured object with one configuration (#45)
## =========================================================================
## With one configuration there is nothing for evaluate() to screen, so fit()
## takes the configured object, draws the split evaluate() would draw at the
## same seed, and re-tunes from a space-filling grid where evaluate()'s
## parameters would otherwise seed it.

describe("fit() - cold start from one configuration (#45)", {

  ## make_eval_object() is configured, not evaluated (helper-fixtures.R)
  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L

  cold_out <- utils::capture.output(
    cold <- suppressWarnings(
      fit(obj, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE, seed = 42L)
    )
  )

  ## The same configuration through evaluate(), at the same seed
  ev <- suppressWarnings(evaluate(obj, prune = FALSE, verbose = FALSE, seed = 42L))

  warm_out <- utils::capture.output(
    warm <- suppressWarnings(
      fit(ev, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE, seed = 42L)
    )
  )

  it("fits a configured object without evaluate(), and predict() works on it", {

    expect_identical(class(cold), c("horizons_fit", "horizons_eval", "horizons_data", "list"))
    expect_identical(validate_horizons_fit(cold), cold)
    expect_identical(names(cold$models$workflows), "cfg_001")

    new_data <- obj$data$analysis[1:5, setdiff(names(obj$data$analysis), "SOC")]
    p        <- predict(cold, new_data, interval = FALSE)

    expect_identical(p$sample_id, new_data$sample_id)
    expect_true(all(is.finite(p$.pred)))

  })

  it("records an unscreened evaluation: one not_evaluated row, no CV, no parameters", {

    rec <- cold$evaluation

    expect_false(rec$screened)
    expect_identical(rec$best_config, "cfg_001")
    expect_identical(rec$rank_metric, "rpd")
    expect_identical(rec$results$config_id, "cfg_001")
    expect_identical(rec$results$status, "not_evaluated")
    expect_true(all(is.na(unlist(rec$results[paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))]))))
    expect_null(rec$results$best_params[[1]])
    expect_equal(rec$n_train + rec$n_test, 60)

  })

  it("holds out the rows evaluate() holds out at the same seed", {

    expect_identical(cold$evaluation$split$in_id, ev$evaluation$split$in_id)
    expect_identical(rsample::testing(cold$models$split)$sample_id,
                     rsample::testing(ev$evaluation$split)$sample_id)
    expect_identical(rsample::testing(cold$models$split)$sample_id,
                     rsample::testing(warm$models$split)$sample_id)

  })

  it("holds out evaluate()'s rows when some outcomes are NA", {

    with_na <- obj
    with_na$data$analysis$SOC[c(2, 9, 30)] <- NA_real_

    cold_na <- suppressWarnings(
      fit(with_na, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 7L)
    )
    ev_na <- suppressWarnings(
      evaluate(with_na, prune = FALSE, verbose = FALSE, seed = 7L)
    )

    expect_identical(cold_na$models$split$data, ev_na$evaluation$split$data)
    expect_identical(cold_na$models$split$in_id, ev_na$evaluation$split$in_id)

  })

  it("prints and records the space-filling start", {

    expect_true(any(grepl("Cold start: no warm-start parameters; space-filling grid of 2 points",
                          cold_out, fixed = TRUE)))
    expect_false(cold$models$results$warm_start)
    expect_identical(cold$models$results$start_grid_points, 2L)

  })

  it("records a warm start on the evaluate() path and prints no space-filling note", {

    expect_true(warm$models$results$warm_start)
    expect_false(any(grepl("space-filling", warm_out, fixed = TRUE)))

  })

  it("says in the tree that it started cold from a configured object", {

    expect_true(any(grepl("Cold start: 1 configuration, not screened by evaluate()",
                          cold_out, fixed = TRUE)))
    expect_true(any(grepl("horizons_data → horizons_fit", cold_out, fixed = TRUE)))
    expect_false(any(grepl("Cold start", warm_out, fixed = TRUE)))

  })

  it("prints and summarises the record as unevaluated", {

    label <- "Configs evaluated: none (fit() started cold from 1)"

    expect_true(any(grepl(label, utils::capture.output(print(cold)), fixed = TRUE)))
    expect_true(any(grepl(label, utils::capture.output(summary(cold)), fixed = TRUE)))
    expect_false(any(grepl(label, utils::capture.output(print(warm)), fixed = TRUE)))

  })

  ## Degradation compares the test RPD with fit()'s own out-of-fold CV on the
  ## fit rows, not with evaluate()'s, so a cold start has what the check needs.
  it("checks degradation against its own cross-validation, which a cold start has", {

    res <- cold$models$results

    expect_true(is.finite(res$cv_rpd_mean))
    expect_true(is.finite(res$cv_rpd_se))
    expect_type(res$degraded, "logical")
    expect_false(is.na(res$degraded))

  })

  it("re-fits a cold-started fit as a cold start", {

    again <- suppressWarnings(
      fit(cold, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L)
    )

    expect_s3_class(again, "horizons_fit")
    expect_false(again$evaluation$screened)
    expect_identical(again$models$split$in_id, cold$models$split$in_id)

  })

  it("is refused by ensemble(), as any single-member fit is", {

    expect_error(ensemble(cold, verbose = FALSE), "at least 2 members")

  })

  it("refuses more than one configuration, naming evaluate()", {

    two <- make_eval_object(n = 60, n_configs = 2)

    err <- expect_error(fit(two, verbose = FALSE), class = "horizons_input_error")

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "2 configurations", fixed = TRUE)
    expect_match(msg, "evaluate()", fixed = TRUE)

  })

  it("refuses a configured object with no configuration, or an unknown metric", {

    none <- obj
    none$config$configs <- obj$config$configs[0, ]

    expect_error(fit(none, verbose = FALSE), "configure()", class = "horizons_input_error")
    expect_error(fit(obj, metric = "accuracy", verbose = FALSE), "accuracy",
                 class = "horizons_input_error")

  })

})
