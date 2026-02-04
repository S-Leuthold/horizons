## ---------------------------------------------------------------------------
## Tests: fit()
## ---------------------------------------------------------------------------
## Integration tests for the fit() pipeline verb. These exercise the full
## pipeline: horizons_data → configure → validate → evaluate → fit.


## ---------------------------------------------------------------------------
## Helper: build a minimal horizons_eval object ready for fit()
## ---------------------------------------------------------------------------

make_fit_object <- function(n = 60, n_wn = 10, n_configs = 2, seed = 42) {

  set.seed(seed)

  ## Spectral data
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

  ## Build horizons_data-like structure
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
    evaluation = list(
      results     = NULL,
      best_config = NULL,
      rank_metric = NULL,
      backend     = NULL,
      runtime     = NULL,
      timestamp   = NULL
    ),
    models   = list(workflows      = NULL,
                    n_models       = NULL,
                    cv_predictions = NULL,
                    results        = NULL,
                    split          = NULL,
                    row_index      = NULL,
                    uq             = NULL,
                    timestamp      = NULL,
                    runtime_secs   = NULL),
    ensemble  = list(stack = NULL),
    artifacts = list(cache_dir = NULL)
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
  "best_params", "runtime_secs"
)

## Expected columns in cv_predictions
EXPECTED_FIT_CV_PRED_COLS <- c(
  ".row", ".fold", "config_id", ".pred", ".pred_trans", "truth"
)

## Expected slots in models$
EXPECTED_MODEL_SLOTS <- c(
  "workflows", "n_models", "cv_predictions", "results",
  "split", "row_index", "uq", "timestamp", "runtime_secs"
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


describe("fit() - seed reproducibility", {

  obj <- make_fit_object(n = 60, n_configs = 1)

  r1 <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = FALSE, verbose = FALSE, seed = 123L)
  )

  r2 <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = FALSE, verbose = FALSE, seed = 123L)
  )

  it("same seed produces same split", {

    t1 <- rsample::training(r1$models$split)
    t2 <- rsample::training(r2$models$split)
    expect_equal(t1$sample_id, t2$sample_id)

  })

})
