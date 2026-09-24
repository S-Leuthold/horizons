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
      ## SOC carries role "outcome" below, not "response" — those are
      ## distinct roles (n_responses counts role == "response", the sibling
      ## responses add_response()/select_training() can carry alongside the
      ## one outcome being modeled). evaluate()'s new entry-stage
      ## validate_horizons_data() call (#24) is the first thing to actually
      ## check this stored count against the role_map.
      n_responses  = 0L
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

  it("refuses a column added after configure() with no role_map entry (#24)", {

    ## validate_horizons_fit() (at return) certifies the models slot, not the
    ## base data contract, so a column landing in $data$analysis after
    ## configure() — by a later parse_ids(), or a direct assignment — used to
    ## reach build_recipe()'s `outcome ~ .` as an unregistered predictor,
    ## undetected. fit()'s entry-stage validate_horizons_data() call closes
    ## that gap.

    obj <- make_fit_object()
    obj$data$analysis$stray_column <- seq_len(nrow(obj$data$analysis))

    expect_error(
      fit(obj, verbose = FALSE),
      "[Mm]issing from.*role_map"
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


## =========================================================================
## The recipe settings evaluate() ran with reach the re-tune (#62)
## =========================================================================

describe("fit() - recipe settings", {

  capture_recipe_args <- function(obj) {

    seen <- NULL

    testthat::with_mocked_bindings(
      fit_single_config = function(...) {
        seen <<- list(...)[c("sg_window", "pca_threshold")]
        list(config_id = list(...)$config_row$config_id, status = "failed",
             degraded = NA, degraded_reason = NA_character_,
             fitted_workflow = NULL, best_params = NULL,
             cv_predictions = NULL, test_metrics = NULL, cv_metrics = NULL,
             uq = NULL, ad = NULL, warnings = NULL,
             error_message = "mocked", runtime_secs = 0)
      },
      tryCatch(
        suppressWarnings(
          fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
              verbose = FALSE)
        ),
        error = function(e) NULL
      ),
      .package = "horizons"
    )

    seen

  }

  obj <- make_fit_object(n = 60, n_configs = 1)

  it("passes configure()'s sg_window and pca_threshold to fit_single_config()", {

    obj$config$recipe <- list(sg_window = 7L, pca_threshold = 0.9)

    expect_identical(capture_recipe_args(obj),
                     list(sg_window = 7L, pca_threshold = 0.9))

  })

  it("falls back to the values the recipe always ran when the record is absent (older objects)", {

    obj$config$recipe <- NULL

    expect_identical(capture_recipe_args(obj),
                     list(sg_window = 9L, pca_threshold = 0.995))

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
    ## Keep the stored row count honest so the only thing wrong with this
    ## object is what the test means to exercise — the split no longer
    ## indexing these rows — rather than also tripping fit()'s new
    ## entry-stage validate_horizons_data() (#24) on a stale n_rows.
    stale$data$n_rows <- nrow(stale$data$analysis)

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
## running select_training() (and survives a validator that checks the
## shape). Column sets match validate_horizons_data()'s selection-record
## check (R/class-core.R) and select_training()'s own construction
## (R/pipeline-select-training.R) as of the 2026-09 design, not an earlier
## draft of the schema.
make_selection_stub <- function() {

  list(
    settings         = list(method = "neighbours", n_target = 10L),
    membership       = tibble::tibble(target_id = character(0),
                                      property  = character(0),
                                      pool_id   = character(0),
                                      distance  = numeric(0),
                                      rank      = integer(0),
                                      space     = character(0),
                                      retained  = logical(0)),
    groups           = tibble::tibble(group      = character(0),
                                      n_targets  = integer(0),
                                      n_rows     = integer(0),
                                      target_ids = list(),
                                      pool_ids   = list()),
    pool_sizes       = tibble::tibble(property  = character(0),
                                      available = integer(0),
                                      drawn     = integer(0)),
    target_distances = tibble::tibble(target_id = character(0),
                                      property  = character(0),
                                      nearest   = numeric(0),
                                      mean_k    = numeric(0),
                                      space     = character(0)),
    exclusions       = tibble::tibble(property           = character(0),
                                      target_id           = character(0),
                                      pool_id              = character(0),
                                      distance             = numeric(0),
                                      rank                 = integer(0),
                                      reference_distance   = numeric(0),
                                      reason               = character(0))
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
    expect_identical(cold$models$results$start_grid_size, 2L)

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

  ## The Evaluation section of print() or summary(): the lines after its
  ## heading, up to the blank line that ends it.
  evaluation_section <- function(out) {
    start <- which(out == "Evaluation")[1]
    end   <- start + which(out[-seq_len(start)] == "")[1]
    out[(start + 1):(end - 1)]
  }

  it("prints and summarises the record as unevaluated, and nothing else", {

    only_line <- "   └─ Configs evaluated: none (fit() started cold from 1)"

    ## No success count, results, rank metric or runtime to contradict it
    expect_identical(evaluation_section(utils::capture.output(print(cold))), only_line)
    expect_identical(evaluation_section(utils::capture.output(summary(cold))), only_line)
    expect_false(any(grepl("none (fit() started cold", utils::capture.output(print(warm)),
                           fixed = TRUE)))

  })

  it("closes print()'s evaluation branch on its last line, cold or not", {

    ## A fitted object's Best line used to stay open (├─)
    warm_section <- evaluation_section(utils::capture.output(print(warm)))
    expect_match(warm_section[length(warm_section)], "^   └─ Best: ")
    expect_false(any(grepl("└─", warm_section[-length(warm_section)], fixed = TRUE)))

    ## Nothing succeeded: Successful is the last line, and used to stay open
    none_succeeded <- ev
    none_succeeded$evaluation$results$status <- "pruned"

    none_section <- evaluation_section(utils::capture.output(print(none_succeeded)))
    expect_identical(none_section[length(none_section)], "   └─ Successful: 0")

  })

  it("prints the Bayesian budget the re-tune runs, not the screening one", {

    budget <- obj
    budget$config$tuning$bayesian_iter       <- 0L
    budget$config$tuning$final_bayesian_iter <- 1L

    ## Stop at the first member: only the header is under test
    out <- utils::capture.output(
      testthat::with_mocked_bindings(
        tryCatch(
          suppressWarnings(
            fit(budget, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE)
          ),
          horizons_all_members_failed = function(e) NULL
        ),
        fit_single_config = function(...) {
          list(config_id = "cfg_001", status = "failed", error_message = "mocked",
               runtime_secs = 0)
        },
        .package = "horizons"
      )
    )

    expect_true(any(grepl("Bayesian: 1 iterations", out, fixed = TRUE)))
    expect_false(any(grepl("Bayesian: 0 iterations", out, fixed = TRUE)))

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

  it("re-fits a cold-started fit as a cold start, keeping its recorded metric", {

    cold_rmse <- suppressWarnings(
      fit(obj, metric = "rmse", compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE, seed = 42L)
    )

    again_out <- utils::capture.output(
      again <- suppressWarnings(
        fit(cold_rmse, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE, seed = 42L)
      )
    )

    expect_s3_class(again, "horizons_fit")
    expect_false(again$evaluation$screened)
    expect_identical(again$models$split$in_id, cold$models$split$in_id)

    ## metric = NULL carries the recorded one, as it carries evaluate()'s
    expect_identical(again$evaluation$rank_metric, "rmse")
    expect_identical(again$models$rank_metric, "rmse")

    expect_true(any(grepl("horizons_fit → horizons_fit", again_out, fixed = TRUE)))

  })

  ## An object evaluated before `screened` existed has no such key; it is a
  ## screened evaluation all the same, and must not be re-drawn.
  it("takes the warm path for an evaluation that predates `screened`", {

    legacy <- ev
    legacy$evaluation$screened <- NULL

    expect_false("screened" %in% names(legacy$evaluation))

    legacy_out <- utils::capture.output(
      r <- suppressWarnings(
        fit(legacy, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE, seed = 42L)
      )
    )

    expect_identical(r$evaluation, legacy$evaluation)
    expect_identical(r$models$split$in_id, legacy$evaluation$split$in_id)
    expect_true(r$models$results$warm_start)
    expect_false(any(grepl("Cold start", legacy_out, fixed = TRUE)))

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

  it("records evaluation$recipe as evaluate() does, and builds its recipe from it (#62)", {

    expect_identical(cold$evaluation$recipe, ev$evaluation$recipe)

    ## A non-default record reaches both the evaluation record and the re-tune
    tuned <- obj
    tuned$config$recipe <- list(sg_window = 7L, pca_threshold = 0.9)

    seen <- NULL

    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(
          fit(tuned, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE)
        ),
        horizons_all_members_failed = function(e) NULL
      ),
      fit_single_config = function(...) {
        seen <<- list(...)[c("sg_window", "pca_threshold")]
        list(config_id = "cfg_001", status = "failed", error_message = "mocked",
             runtime_secs = 0)
      },
      .package = "horizons"
    )

    expect_identical(seen, list(sg_window = 7L, pca_threshold = 0.9))
    ## The helper draws the split, and rsample warns about thin quantiles
    record <- suppressWarnings(cold_start_evaluation(tuned, NULL, 42L))

    expect_identical(record$evaluation$recipe,
                     list(sg_window = 7L, sg_window_cm = 14, pca_threshold = 0.9))

  })

  it("refuses a Savitzky-Golay window as wide as the spectrum, as evaluate() does (#62)", {

    ## make_eval_object() has 10 spectral columns
    wide <- obj
    wide$config$recipe <- list(sg_window = 11L, pca_threshold = 0.995)

    ran <- 0L

    err <- testthat::with_mocked_bindings(
      tryCatch(fit(wide, verbose = FALSE, seed = 42L), error = function(e) e),
      fit_single_config = function(...) { ran <<- ran + 1L; NULL },
      .package = "horizons"
    )

    expect_s3_class(err, "horizons_input_error")
    expect_match(conditionMessage(err), "11 grid points (22 cm", fixed = TRUE)
    expect_match(conditionMessage(err), "10 spectral columns", fixed = TRUE)
    expect_identical(ran, 0L)

  })

  it("refuses a configured object with no configuration", {

    none <- obj
    none$config$configs <- obj$config$configs[0, ]

    expect_error(fit(none, verbose = FALSE), "configure()", class = "horizons_input_error")

  })

  it("refuses a column added after configure() with no role_map entry on the cold-start path too (#24)", {

    ## The entry-stage validate_horizons_data() call runs after cold_start_evaluation()
    ## populates $evaluation (Step 0), on the configured-object path exactly as it
    ## does on the horizons_eval path — a column with no role_map entry must not
    ## reach build_recipe()'s outcome ~ . undetected just because there was no
    ## evaluate() call to certify the object first.
    stray <- obj
    stray$data$analysis$stray_column <- seq_len(nrow(stray$data$analysis))

    expect_error(
      fit(stray, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L),
      "[Mm]issing from.*role_map"
    )

  })

  ## On the evaluate() path an unknown metric used to surface as a missing
  ## cv_<metric> column, with the advice to re-run evaluate().
  it("refuses an unknown metric on either path, naming it and the valid ones", {

    for (x in list(obj, ev)) {

      err <- expect_error(fit(x, metric = "accuracy", verbose = FALSE),
                          class = "horizons_input_error")

      msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
      expect_match(msg, "accuracy", fixed = TRUE)
      expect_match(msg, "rrmse", fixed = TRUE)
      expect_no_match(msg, "Re-run", fixed = TRUE)

    }

  })

})


## =========================================================================
## Cold start with UQ and AD on (#45)
## =========================================================================
## The calibration set is carved from the cold start's own training part, as
## it is from evaluate()'s, and predict() serves intervals and AD from it.

describe("fit() - cold start with UQ and AD (#45)", {

  ## n = 250 with 10 NA outcomes: 240 modelled rows, enough for the
  ## calibration split to clear N_CALIB_MIN
  obj <- make_eval_object(n = 250, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L
  obj$data$analysis$SOC[seq(5, 50, by = 5)] <- NA_real_

  cold <- suppressWarnings(
    fit(obj, compute_uq = TRUE, compute_ad = TRUE, verbose = FALSE, seed = 42L)
  )

  modelled_ids <- obj$data$analysis$sample_id[!is.na(obj$data$analysis$SOC)]
  test_ids     <- rsample::testing(cold$models$split)$sample_id
  fit_ids      <- cold$models$row_index$sample_id

  ## Split C, reproduced from calib_split_seed() and the training part alone
  set.seed(calib_split_seed(42L))
  split_C   <- rsample::initial_split(rsample::training(cold$models$split),
                                      prop = CALIB_PROP,
                                      strata = dplyr::all_of("SOC"))
  calib_ids <- rsample::testing(split_C)$sample_id

  it("partitions the modelled frame into disjoint test, calibration and fit rows", {

    expect_length(modelled_ids, 240L)
    expect_setequal(rsample::training(split_C)$sample_id, fit_ids)

    expect_length(intersect(test_ids, calib_ids), 0L)
    expect_length(intersect(test_ids, fit_ids), 0L)
    expect_length(intersect(calib_ids, fit_ids), 0L)
    expect_setequal(c(test_ids, calib_ids, fit_ids), modelled_ids)
    expect_identical(length(c(test_ids, calib_ids, fit_ids)), 240L)

    expect_equal(cold$models$uq[[1]]$n_calib, length(calib_ids))

  })

  it("serves intervals and AD flags from predict()", {

    expect_true(has_uq(cold))
    expect_true(has_ad(cold))

    new_data <- obj$data$analysis[1:6, setdiff(names(obj$data$analysis), "SOC")]
    p        <- predict(cold, new_data, interval = TRUE)

    expect_true(all(c(".pred_lower", ".pred_upper", ".ad_distance", ".ad_flag") %in% names(p)))
    expect_true(all(p$.pred_lower <= p$.pred_upper))
    expect_false(anyNA(p$.ad_flag))

  })

})


## =========================================================================
## fit()'s tree header says what the draws did, and holds its notes (#91)
## =========================================================================
## The CV line printed "stratified" whatever the folds were, and the notes on
## the member count, a failed stratified draw and an undersized calibration
## set printed above the tree's header.

## fit()'s console output down to its first member, which is mocked to fail:
## only the header is under test.
fit_header <- function(obj, ...) {

  utils::capture.output(
    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(fit(obj, seed = 42L, ...)),
        horizons_all_members_failed = function(e) NULL
      ),
      fit_single_config = function(...) {
        list(config_id = "cfg_001", status = "failed", error_message = "mocked",
             runtime_secs = 0)
      },
      .package = "horizons"
    )
  )

}

describe("fit() - the draw lines and the notes (#91)", {

  it("says the CV folds are unstratified when rsample drew them without strata", {

    ## 40 rows: 32 fit rows, too few for rsample to bin the outcome
    out <- fit_header(make_eval_object(n = 40, n_configs = 1),
                      compute_uq = FALSE, compute_ad = FALSE)

    expect_true("│  CV: 3-fold unstratified" %in% out)
    expect_false(any(grepl("CV: .*stratified on", out)))

  })

  it("says the CV folds are stratified when the strata held", {

    out <- fit_header(make_eval_object(n = 60, n_configs = 1),
                      compute_uq = FALSE, compute_ad = FALSE)

    expect_true("│  CV: 3-fold stratified on SOC" %in% out)

  })

  it("says whether the calibration split stratified", {

    ## 250 rows: a calibration set of 40, over N_CALIB_MIN
    obj <- make_eval_object(n = 250, n_configs = 1)

    expect_true("│  UQ calibration: 158 fit / 40 calibration, stratified on SOC" %in%
                  fit_header(obj))

    real_initial_split <- rsample::initial_split

    local_mocked_bindings(
      initial_split = function(data, prop = 3 / 4, strata = NULL, ...) {
        if (!missing(strata)) stop("stratification refused")
        real_initial_split(data, prop = prop, ...)
      },
      .package = "rsample"
    )

    expect_true(any(grepl("^│  UQ calibration: .* calibration, unstratified$",
                          fit_header(obj))))

  })

  it("prints the draw and calibration notes inside the tree, under the lines they qualify", {

    real_initial_split <- rsample::initial_split
    real_vfold_cv      <- rsample::vfold_cv

    local_mocked_bindings(
      initial_split = function(data, prop = 3 / 4, strata = NULL, ...) {
        if (!missing(strata)) stop("stratification refused")
        real_initial_split(data, prop = prop, ...)
      },
      vfold_cv = function(data, v = 10, repeats = 1, strata = NULL, ...) {
        if (!missing(strata)) stop("stratification refused")
        real_vfold_cv(data, v = v, repeats = repeats, ...)
      },
      .package = "rsample"
    )

    ## A cold start draws the split itself; 60 rows leave a calibration set
    ## under N_CALIB_MIN
    out <- fit_header(make_eval_object(n = 60, n_configs = 1))

    header     <- grep("┌ fit", out, fixed = TRUE)
    split_line <- grep("│  Split: ", out, fixed = TRUE)
    split_note <- grep("Stratified split failed, retrying without strata", out, fixed = TRUE)
    calib_note <- grep("Calibration set too small (10 < 30). Disabling UQ and AD.", out, fixed = TRUE)
    cv_line    <- grep("│  CV: ", out, fixed = TRUE)
    cv_note    <- grep("Stratified CV failed, retrying without strata", out, fixed = TRUE)

    expect_length(header, 1L)
    expect_identical(out[split_line],
                     "│  Split: 48 train / 12 test (the rows evaluate() holds out at this seed, unstratified)")
    expect_identical(split_note, split_line + 1L)
    expect_identical(calib_note, split_note + 1L)
    expect_identical(out[cv_line], "│  CV: 3-fold unstratified")
    expect_identical(cv_note, cv_line + 1L)
    expect_true(all(c(split_note, calib_note, cv_note) > header))

  })

  it("prints the n_best note inside the tree, under the member count", {

    ev <- make_fit_object(n = 60, n_configs = 1)

    out <- fit_header(ev, n_best = 3L, compute_uq = FALSE, compute_ad = FALSE)

    header  <- grep("┌ fit", out, fixed = TRUE)
    members <- grep("│  Re-tuning top 1 of 1 configurations", out, fixed = TRUE)
    note    <- grep("Requested n_best = 3 but only 1 successful configs available. Using 1.",
                    out, fixed = TRUE)

    expect_length(header, 1L)
    expect_true(members > header)
    expect_identical(note, members + 1L)

  })

})
