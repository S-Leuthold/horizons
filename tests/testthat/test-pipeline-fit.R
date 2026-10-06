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
      spectra_type   = "mir"
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
    ensemble   = contract$ensemble
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
  ".row", "sample_id", ".fold", "config_id", ".pred", ".pred_trans", "truth"
)


## ---------------------------------------------------------------------------
## Shared fixtures, built on first use (helper-memo.R)
## ---------------------------------------------------------------------------
## Read-only: a test that needs a changed object changes its own copy.

## make_fit_object() at its defaults: 60 rows, two configs, evaluated
mfo <- function() memo_fixture("mfo", make_fit_object)

## The two-member fit. Its re-tune runs two Bayesian iterations after the
## warm-start grid, so fit() runs tune_warmstart_bayes()'s Bayesian stage end
## to end here. At n = 60 the calibration set is under N_CALIB_MIN, so UQ and
## AD would be off whatever the flags said.
build_fit60 <- function() {

  obj <- make_fit_object(n = 60, n_configs = 2)
  obj$config$tuning$final_bayesian_iter <- 2L

  result <- suppressWarnings(
    fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
        verbose = FALSE, seed = 42L)
  )

  list(obj = obj, fit = result)

}

fit60 <- function() memo_fixture("fit60", build_fit60)

## One member at n = 250 with UQ on: the calibration split clears N_CALIB_MIN
## (calib = 0.2 * 0.8 * n = 40), so UQ actually runs.
build_fit250_uq <- function() {

  obj <- make_fit_object(n = 250, n_configs = 1, seed = 42)
  obj$config$tuning$final_bayesian_iter <- 0L

  result <- suppressWarnings(
    fit(obj, n_best = 1L, compute_uq = TRUE, compute_ad = FALSE,
        verbose = FALSE, seed = 42L)
  )

  list(obj = obj, fit = result)

}

fit250_uq <- function() memo_fixture("fit250_uq", build_fit250_uq)


## =========================================================================
## Preflight validation
## =========================================================================

describe("fit() - preflight validation", {

  it("aborts on non-horizons_eval input", {

    expect_error(
      fit(list(a = 1), verbose = FALSE),
      "needs a <horizons_eval> from `evaluate()`", fixed = TRUE,
      class = "horizons_input_error"
    )

  })

  it("aborts when no successful configs in evaluation", {

    obj <- mfo()
    obj$evaluation$results$status <- "failed"

    expect_error(
      fit(obj, verbose = FALSE),
      "No configuration in evaluation$results can be fitted", fixed = TRUE,
      class = "horizons_input_error"
    )

  })

  it("aborts when the evaluation record has no results", {

    ## The key is present, so the missing-keys check passes, but the table is
    ## NULL or has no rows.
    obj <- mfo()

    no_results <- obj
    no_results$evaluation["results"] <- list(NULL)

    expect_error(
      fit(no_results, verbose = FALSE),
      "No evaluation results found"
    )

    no_rows <- obj
    no_rows$evaluation$results <- no_rows$evaluation$results[0, ]

    expect_error(
      fit(no_rows, verbose = FALSE),
      "No evaluation results found"
    )

  })

  it("refuses a column added after configure() with no role_map entry (#24)", {

    ## validate_horizons_fit() (at return) certifies the models slot, not the
    ## base data contract, so a column landing in $data$analysis after
    ## configure() — by a later parse_ids(), or a direct assignment — used to
    ## reach build_recipe()'s `outcome ~ .` as an unregistered predictor,
    ## undetected. fit()'s entry-stage validate_horizons_data() call closes
    ## that gap.

    obj <- mfo()
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

  it("writes exactly the models keys new_horizons_data() declares (#71)", {

    result <- fit60()$fit

    ## The constructor's empty slot is what configure() resets to, so it has
    ## to name what fit() actually writes.
    expect_identical(names(result$models), names(new_horizons_data()$models))

  })

  it("orders members by cv_<rank_metric> and records the top one as best_config, with the rank metric used", {

    shared <- fit60()
    obj    <- shared$obj
    result <- shared$fit

    ## Members are ranked on the cross-validated metric from
    ## evaluation$results (#50)
    successes <- obj$evaluation$results[obj$evaluation$results$status == "success", ]
    expected  <- rank_configs_by_cv(successes, obj$evaluation$rank_metric)$config_id

    expect_equal(result$models$results$config_id, expected[seq_len(nrow(result$models$results))])

    ## best_config is the durable ranking fact predict() reads; it must be the
    ## first config in best-first workflow order, and a real fitted config.
    expect_equal(result$models$best_config, names(result$models$workflows)[1])
    expect_true(result$models$best_config %in% names(result$models$workflows))
    expect_true(is.character(result$models$rank_metric))

  })

  it("re-running evaluate() on it empties models and ensemble (#70)", {

    skip_unless_slow_tier()

    result <- fit60()$fit

    ## The shared fit has compute_uq = FALSE, so carry a bundle the way
    ## compute_uq = TRUE leaves one; otherwise has_uq() is FALSE before and
    ## after
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

  it("re-running fit() on an ensemble with n_best = 1 empties the ensemble and fits one member (#70)", {

    result <- fit60()$fit

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

    ## n_best = 1 of the two configurations fits one member
    expect_equal(refit$models$n_models, 1L)

  })

})


## =========================================================================
## CV predictions
## =========================================================================

describe("fit() - cv_predictions", {

  it("is a tibble with every expected column and the predictions of every successful config", {

    result   <- fit60()$fit
    cv_preds <- result$models$cv_predictions

    expect_s3_class(cv_preds, "tbl_df")
    expect_true(all(EXPECTED_FIT_CV_PRED_COLS %in% names(cv_preds)))
    expect_true(is.integer(cv_preds$.row) || is.numeric(cv_preds$.row))

    successful <- result$models$results %>%
      dplyr::filter(status == "success") %>%
      dplyr::pull(config_id)
    pred_configs <- unique(cv_preds$config_id)
    expect_true(all(successful %in% pred_configs))

  })

})


## =========================================================================
## Results tibble
## =========================================================================

describe("fit() - models$results", {

  it("is a tibble with every expected column and one row per member, and records each success's metrics", {

    shared <- fit60()
    obj    <- shared$obj
    res    <- shared$fit$models$results

    expect_s3_class(res, "tbl_df")
    expect_true(all(EXPECTED_FIT_RESULT_COLS %in% names(res)))
    expect_equal(nrow(res), min(2L, sum(obj$evaluation$results$status == "success")))
    expect_true(is.list(res$best_params))

    successes <- dplyr::filter(res, status == "success")

    expect_gt(nrow(successes), 0)

    ## Test metrics, the CV estimates and the degradation flag
    expect_true(all(is.finite(successes$rmse)))
    expect_true(all(is.finite(successes$rpd)))
    expect_true(all(!is.na(successes$degraded)))
    expect_true(all(is.finite(successes$cv_rmse_mean)))
    expect_true(all(is.finite(successes$cv_rpd_mean)))

  })

})


## =========================================================================
## Out-of-fold predictions keep their samples (#131)
## =========================================================================
## .row is a position in the rows the final models are fit on, and the
## object no longer carries a separate map from it; cv_predictions carries
## sample_id itself.

describe("fit() - cv_predictions carries sample_id", {

  it("names the training row .row indexes", {

    result <- fit60()$fit
    cv     <- result$models$cv_predictions

    ## With UQ and AD off nothing is carved out for calibration, so the fit
    ## rows are Split F's training part.
    train <- rsample::training(result$models$split)

    expect_type(cv$sample_id, "character")
    expect_false(anyNA(cv$sample_id))
    expect_identical(cv$sample_id, train$sample_id[cv$.row])

    ## truth comes from tune, independently of the lookup: it is that
    ## sample's outcome
    expect_equal(cv$truth, train$SOC[cv$.row])

  })

  it("has one prediction per training sample per config", {

    result <- fit60()$fit
    cv     <- result$models$cv_predictions

    train <- rsample::training(result$models$split)

    for (cfg in unique(cv$config_id)) {

      expect_setequal(cv$sample_id[cv$config_id == cfg], train$sample_id)
      expect_equal(anyDuplicated(cv$sample_id[cv$config_id == cfg]), 0L)

    }

  })

})


## =========================================================================
## UQ integration
## =========================================================================

describe("fit() - UQ enabled", {

  ## fit250_uq(): n = 250, so the calibration split clears N_CALIB_MIN = 30
  ## and UQ actually runs. Shared with the "scores on evaluate()'s split"
  ## block below.

  it("models$uq is a list when compute_uq = TRUE and enough data, and models$ad is NULL with compute_ad = FALSE", {

    result <- fit250_uq()$fit

    expect_false(is.null(result$models$uq))
    expect_true(is.list(result$models$uq))

    ## At this size AD computes when asked (the "AD enabled" block), so its
    ## absence here is the flag's doing
    expect_null(result$models$ad)

  })

  it("UQ bundles are named by config_id", {

    result <- fit250_uq()$fit

    expect_false(is.null(result$models$uq))
    expect_true(length(result$models$uq) > 0)
    expect_true(!is.null(names(result$models$uq)))

  })

  it("UQ bundles have expected fields", {

    result <- fit250_uq()$fit

    expect_false(is.null(result$models$uq))

    uq_bundle <- result$models$uq[[1]]

    expected <- c("quantile_model", "scores", "n_calib",
                  "level_default", "oof_coverage", "mean_width",
                  "prepped_recipe", "test_coverage", "test_mean_width",
                  "n_test")
    expect_true(all(expected %in% names(uq_bundle)))

    ## Coverage measured on the held-out test rows (#118)
    expect_gt(uq_bundle$n_test, 0)
    expect_gte(uq_bundle$test_coverage, 0)
    expect_lte(uq_bundle$test_coverage, 1)
    expect_gt(uq_bundle$test_mean_width, 0)

  })

  it("measures coverage on the held-out test rows with the intervals predict() serves (#118)", {

    result <- fit250_uq()$fit

    expect_false(is.null(result$models$uq))

    uq_bundle <- result$models$uq[[result$models$best_config]]
    held_out  <- rsample::testing(result$models$split)
    p         <- predict(result, held_out, interval = TRUE)
    covered   <- held_out$SOC >= p$.pred_lower & held_out$SOC <= p$.pred_upper

    expect_identical(uq_bundle$n_test, nrow(held_out))
    expect_equal(uq_bundle$test_coverage, mean(covered))
    expect_equal(uq_bundle$test_mean_width, mean(p$.interval_width))

  })

})


describe("fit() - AD enabled", {

  ## n = 250 so the shared calibration split clears N_CALIB_MIN = 30
  ## (calib = 0.2 * 0.8 * n = 40). AD must ACTUALLY compute here, not just be
  ## NULL-tolerated — the assertions below require a populated bundle. The
  ## re-tune budget is not under test.
  obj <- make_fit_object(n = 250, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L

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

  it("same seed produces the same folds and fit rows", {

    rows_of <- function(r) {
      cp <- r$models$cv_predictions
      unique(cp[order(cp$.row), c(".row", "sample_id")])
    }

    expect_identical(fold_of_row(r1), fold_of_row(r2))
    expect_identical(rows_of(r1), rows_of(r2))

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

  ## fit250_uq(): n = 250, so the calibration split clears N_CALIB_MIN and UQ
  ## runs.

  it("reuses evaluate()'s split, so its test rows are evaluate()'s", {

    shared <- fit250_uq()
    obj    <- shared$obj
    r      <- shared$fit

    expect_identical(r$models$split$data, obj$evaluation$split$data)
    expect_identical(rsample::testing(r$models$split)$sample_id,
                     rsample::testing(obj$evaluation$split)$sample_id)

  })

  it("partitions the modelled rows into test, calibration and fit rows", {

    shared <- fit250_uq()
    obj    <- shared$obj
    r      <- shared$fit

    expect_false(is.null(r$models$uq))

    ## Split C is reproducible from calib_split_seed() and evaluate()'s
    ## training rows alone.
    set.seed(calib_split_seed(42L))
    split_C <- rsample::initial_split(rsample::training(obj$evaluation$split),
                                      prop = CALIB_PROP,
                                      strata = dplyr::all_of("SOC"))

    test_ids  <- rsample::testing(r$models$split)$sample_id
    calib_ids <- rsample::testing(split_C)$sample_id
    fit_ids   <- unique(r$models$cv_predictions$sample_id)

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

    obj <- fit250_uq()$obj

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

    ## A record without the key is refused with the other missing evaluation
    ## keys; one whose split is not an rsplit, by the split check itself.
    no_split <- obj
    no_split$evaluation$split <- NULL

    expect_error(
      fit(no_split, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE),
      "missing split",
      class = "horizons_validation_error"
    )

    bad_split <- obj
    bad_split$evaluation$split <- "an rsplit"

    expect_error(
      fit(bad_split, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE),
      class = "horizons_input_error"
    )

  })

  it("still fits after add_response() adds a sibling response", {

    obj <- fit250_uq()$obj

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
    expect_false(any(na_ids %in% unique(r$models$cv_predictions$sample_id)))

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
  fit_ids   <- unique(r$models$cv_predictions$sample_id)
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
    fit_rows <- obj$data$analysis$sample_id %in% unique(r$models$cv_predictions$sample_id)

    ## Precondition: the fixture discriminates. If a change to the split
    ## moves the maximum back into the fit rows, pick another seed.
    expect_gt(max(soc), max(soc[fit_rows]))

    expect_equal(r$models$response_bound,
                 max(soc[fit_rows]) * RESPONSE_BOUND_MARGIN)

  })

})


## =========================================================================
## Members are ranked on the cross-validated metric, not the test set (#50)
## =========================================================================

describe("fit() - member ranking (#50)", {

  ## The shared fit's two configurations rank the same on either metric, so
  ## it cannot tell the two rules apart. Here the evaluation's CV and test
  ## RPDs disagree, and mocked members record the order fit() re-tunes them in.
  it("re-tunes members in cv_<rank_metric> order when the test-set metric disagrees", {

    obj <- mfo()
    res <- obj$evaluation$results

    expect_identical(obj$evaluation$rank_metric, "rpd")
    expect_identical(res$status, c("success", "success"))

    ## Best first by CV, worst first by the test set
    res$cv_rpd <- c(2.0, 1.5)
    res$rpd    <- c(1.0, 3.0)
    obj$evaluation$results <- res

    seen <- character(0)

    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(
          fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
              verbose = FALSE, seed = 42L)
        ),
        horizons_all_members_failed = function(e) NULL
      ),
      fit_single_config = function(...) {
        cfg  <- list(...)$config_row
        seen <<- c(seen, cfg$config_id)
        list(config_id = cfg$config_id, status = "failed", error_message = "mocked",
             runtime_secs = 0)
      },
      .package = "horizons"
    )

    expect_identical(seen, res$config_id)

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

    skip_unless_slow_tier()

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

    skip_unless_slow_tier()

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

    skip_unless_slow_tier()

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

    skip_unless_slow_tier()

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

    skip_unless_slow_tier()

    local_plan(future::sequential)
    obj <- make_fit_object(n = 60, n_configs = 1)
    obj$config$tuning$final_bayesian_iter <- 0L

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

    result <- fit60()$fit

    expect_false(result$models$selection_present)

  })

  it("records TRUE when the training object carried a selection", {

    skip_unless_slow_tier()

    obj           <- make_fit_object(n_configs = 1)
    obj$selection <- make_selection_stub()
    obj$config$tuning$final_bayesian_iter <- 0L

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

    skip_unless_slow_tier()

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

    skip_unless_slow_tier()

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

  ## evaluate() and the cold start both write `screened`, so an evaluation
  ## without it is not a current object (#130). It is refused before any
  ## member is re-tuned, not by the fit validator after all of them are.
  it("refuses an evaluation without `screened` before fitting anything", {

    legacy <- ev
    legacy$evaluation$screened <- NULL

    expect_false("screened" %in% names(legacy$evaluation))

    local_mocked_bindings(
      fit_single_config = function(...) stop("fit_single_config() was called")
    )

    err <- expect_error(
      fit(legacy, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L),
      class = "horizons_validation_error"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))

    expect_match(msg, "missing", fixed = TRUE)
    expect_match(msg, "screened", fixed = TRUE)
    expect_match(msg, "Re-run `evaluate()`", fixed = TRUE)

  })

  it("names every missing evaluation key in the refusal", {

    legacy <- ev
    legacy$evaluation$recipe        <- NULL
    legacy$evaluation$response_trim <- NULL

    local_mocked_bindings(
      fit_single_config = function(...) stop("fit_single_config() was called")
    )

    err <- expect_error(
      fit(legacy, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L),
      class = "horizons_validation_error"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))

    expect_match(msg, "response_trim", fixed = TRUE)
    expect_match(msg, "recipe", fixed = TRUE)

  })

  it("is refused by ensemble(), as any single-member fit is", {

    expect_error(ensemble(cold, verbose = FALSE), "at least 2 members")

  })

  it("refuses more than one configuration, naming evaluate()", {

    two <- make_eval_object(n = 60, n_configs = 2)

    err <- expect_error(fit(two, verbose = FALSE), class = "horizons_input_error")

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "can start without `evaluate()` only from a single configuration", fixed = TRUE)
    expect_match(msg, "2 configurations", fixed = TRUE)

  })

  it("names an outcome column the analysis table lacks", {

    ## The cold start reads the outcome before fit() validates the object, so
    ## this is the refusal a user meets, not the validator's or "All outcome
    ## values are NA".
    gone <- make_eval_object(n = 60, n_configs = 1)
    outcome <- gone$data$role_map$variable[gone$data$role_map$role == "outcome"]
    gone$data$analysis[[outcome]] <- NULL

    err <- expect_error(fit(gone, verbose = FALSE), class = "horizons_input_error")

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "The analysis table has no outcome column to model.", fixed = TRUE)
    expect_match(msg, paste0("names ", outcome, " as the outcome"), fixed = TRUE)

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
  fit_ids      <- unique(cold$models$cv_predictions$sample_id)

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

  it("says whether the calibration split stratified, and which capabilities it serves", {

    ## 250 rows: a calibration set of 40, over N_CALIB_MIN
    obj <- make_eval_object(n = 250, n_configs = 1)

    expect_true("│  UQ and AD calibration: 158 fit / 40 calibration, stratified on SOC" %in%
                  fit_header(obj))

    ## With AD alone the holdout used to go unreported
    expect_true("│  AD calibration: 158 fit / 40 calibration, stratified on SOC" %in%
                  fit_header(obj, compute_uq = FALSE, compute_ad = TRUE))

    real_initial_split <- rsample::initial_split

    local_mocked_bindings(
      initial_split = function(data, prop = 3 / 4, strata = NULL, ...) {
        if (!missing(strata)) stop("stratification refused")
        real_initial_split(data, prop = prop, ...)
      },
      .package = "rsample"
    )

    out        <- fit_header(obj, compute_uq = FALSE, compute_ad = TRUE)
    calib_line <- grep("│  AD calibration: ", out, fixed = TRUE)

    expect_match(out[calib_line], "calibration, unstratified$")
    expect_identical(out[calib_line + 1L],
                     "│  Stratified calibration split failed, retrying without strata")

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

    skip_unless_slow_tier()

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


## =========================================================================
## evaluate()'s response trim is reused, not recomputed (#77)
## =========================================================================
## validate(remove_outliers = "response") no longer removes rows; evaluate()
## trims the training rows outside fences from its training partition and
## records them. fit() leaves out exactly those rows, on both paths, and
## scores on the untrimmed test rows.

describe("fit() - evaluate()'s response trim (#77)", {

  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$data$analysis$SOC[c(1:4, 31:34)] <- c(20, 25, 30, 35, -15, -20, -25, -30)
  ## The low extremes are negative, so the outcome is signed (#76)
  obj$config$outcome_range <- c(-Inf, Inf)
  obj$config$tuning$final_bayesian_iter <- 0L

  utils::capture.output(
    v <- suppressWarnings(validate(obj, remove_outliers = "response"))
  )

  ev      <- suppressWarnings(evaluate(v, prune = FALSE, verbose = FALSE, seed = 307L))
  trimmed <- ev$evaluation$response_trim$trimmed_ids

  ## What fit() hands its member, without tuning: the split, the rows it fits
  ## on, the console tree and the warnings (kept, and muffled, so none
  ## reaches the reporter).
  seen_by_fit <- function(x, verbose = FALSE) {

    seen   <- NULL
    caught <- list()

    out <- utils::capture.output(
      withCallingHandlers(
        testthat::with_mocked_bindings(
          tryCatch(
            fit(x, compute_uq = FALSE, compute_ad = FALSE, verbose = verbose, seed = 307L),
            horizons_all_members_failed = function(e) NULL
          ),
          fit_single_config = function(...) {
            args <- list(...)
            seen <<- list(split = args$split_F, train = args$train_data)
            list(config_id = args$config_row$config_id, status = "failed",
                 error_message = "mocked", runtime_secs = 0)
          },
          .package = "horizons"
        ),
        warning = function(w) {
          caught[[length(caught) + 1L]] <<- w
          invokeRestart("muffleWarning")
        }
      )
    )

    c(seen, list(out = out, warnings = caught))

  }

  warm <- seen_by_fit(ev, verbose = TRUE)

  it("fits without the rows evaluate() trimmed, and scores on its untrimmed test rows", {

    expect_gt(length(trimmed), 0)
    expect_false(any(trimmed %in% warm$train$sample_id))
    expect_identical(warm$train$sample_id,
                     rsample::training(ev$evaluation$split)$sample_id)
    expect_identical(rsample::testing(warm$split)$sample_id,
                     rsample::testing(ev$evaluation$split)$sample_id)

  })

  it("says so in its tree", {

    expect_true(any(grepl(paste0("Response outliers: ", length(trimmed), " of ",
                                 ev$evaluation$response_trim$n_training,
                                 " training rows trimmed"),
                          warm$out, fixed = TRUE)))

  })

  it("trims the same rows on a cold start, through the same helper", {

    cold <- seen_by_fit(v)

    expect_identical(cold$train$sample_id, warm$train$sample_id)
    expect_identical(rsample::testing(cold$split)$sample_id,
                     rsample::testing(warm$split)$sample_id)

    ## The cold start's record is evaluate()'s
    record <- suppressWarnings(cold_start_evaluation(v, NULL, 307L))

    expect_identical(record$evaluation$response_trim, ev$evaluation$response_trim)
    expect_identical(record$evaluation$split$data, ev$evaluation$split$data)
    expect_identical(record$evaluation$split$in_id, ev$evaluation$split$in_id)

  })

  it("refuses the split once the record of its trimmed rows is gone", {

    ## The request goes too, so the trim-request check (#137) passes and the
    ## split check is the one that refuses
    no_record <- ev
    no_record$evaluation["response_trim"] <- list(NULL)
    no_record$validation$outliers["response_trim"] <- list(NULL)

    expect_error(
      suppressWarnings(fit(no_record, compute_uq = FALSE, compute_ad = FALSE,
                           verbose = FALSE)),
      "does not index the rows this object models",
      class = "horizons_input_error"
    )

  })

  f <- suppressWarnings(
    fit(ev, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 307L)
  )

  it("fits and scores end to end on the trimmed split", {

    expect_s3_class(f, "horizons_fit")
    expect_false(any(trimmed %in% unique(f$models$cv_predictions$sample_id)))
    expect_identical(rsample::testing(f$models$split)$sample_id,
                     rsample::testing(ev$evaluation$split)$sample_id)
    expect_true(is.finite(f$models$results$rmse))

  })

  it("sets the response bound over the fit rows and the trimmed rows", {

    ## The trimmed rows are training-partition rows: a bound below a value
    ## observed there would clamp the extremes the trim set aside.
    analysis <- obj$data$analysis
    fit_soc  <- analysis$SOC[match(unique(f$models$cv_predictions$sample_id), analysis$sample_id)]
    trim_soc <- analysis$SOC[match(trimmed, analysis$sample_id)]

    expect_gt(max(trim_soc), max(fit_soc))
    expect_equal(f$models$response_bound,
                 compute_response_bound(c(fit_soc, trim_soc), c(-Inf, Inf)))

  })

  it("says in summary() that the split's training part was trimmed", {

    out <- utils::capture.output(summary(f))

    expect_true(any(grepl(paste0("Split: .* test \\(", length(trimmed),
                                 " training rows trimmed\\)"), out)))

  })

  it("warns about an object an earlier version validated, on either path", {

    legacy <- legacy_label_removal(obj, sprintf("S%03d", c(1:4, 31:34)))

    cold_legacy <- seen_by_fit(legacy)

    expect_true(any(vapply(cold_legacy$warnings, inherits, logical(1),
                           "horizons_response_trim_warning")))

    ev_legacy   <- suppressWarnings(evaluate(legacy, prune = FALSE, verbose = FALSE, seed = 307L))
    warm_legacy <- seen_by_fit(ev_legacy)

    expect_true(any(vapply(warm_legacy$warnings, inherits, logical(1),
                           "horizons_response_trim_warning")))

  })

})


## =========================================================================
## A trim request changed after evaluate() is refused, not ignored (#137)
## =========================================================================
## validate() writes its request on an evaluated object without resetting
## the evaluation, and fit() reuses the trim evaluate() applied rather than
## reading the request. A request that differs from that trim never reached
## the fit, silently; fit() now refuses it before anything is fitted.

describe("fit() - a trim request changed after evaluate() (#137)", {

  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$data$analysis$SOC[c(1:4, 31:34)] <- c(20, 25, 30, 35, -15, -20, -25, -30)
  ## The low extremes are negative, so the outcome is signed (#76)
  obj$config$outcome_range <- c(-Inf, Inf)
  obj$config$tuning$final_bayesian_iter <- 0L

  quiet_validate <- function(x, ...) {

    utils::capture.output(out <- suppressWarnings(validate(x, ...)))
    out

  }

  quiet_evaluate <- function(x) {

    suppressWarnings(evaluate(x, prune = FALSE, verbose = FALSE, seed = 307L))

  }

  untrimmed <- quiet_evaluate(obj)
  trimmed   <- quiet_evaluate(quiet_validate(obj, remove_outliers = "response"))

  ## fit()'s refusal, with its message stripped of styling; NULL if it fitted
  refusal <- function(x) {

    cnd <- rlang::catch_cnd(
      suppressWarnings(fit(x, compute_uq = FALSE, compute_ad = FALSE,
                           verbose = FALSE, seed = 307L)),
      classes = "horizons_input_error"
    )

    if (is.null(cnd)) return(NULL)

    list(cnd = cnd, message = cli::ansi_strip(conditionMessage(cnd)))

  }

  it("refuses a trim requested after evaluate()", {

    late <- refusal(quiet_validate(untrimmed, remove_outliers = "response"))

    expect_s3_class(late$cnd, "horizons_input_error")
    expect_match(late$message, "requests a trim of SOC at 1.5 x IQR", fixed = TRUE)
    expect_match(late$message, "applied none", fixed = TRUE)
    expect_match(late$message, "Re-run `evaluate()` to apply the current request", fixed = TRUE)

  })

  it("refuses a trim dropped after evaluate()", {

    dropped <- refusal(quiet_validate(trimmed))

    expect_s3_class(dropped$cnd, "horizons_input_error")
    expect_match(dropped$message, "applied a trim of SOC at 1.5 x IQR", fixed = TRUE)
    expect_match(dropped$message, "now requests none", fixed = TRUE)

    ## The cheap way back is offered first, and it works
    expect_match(dropped$message,
                 'validate(x, remove_outliers = "response", response_threshold = 1.5)',
                 fixed = TRUE)
    expect_match(dropped$message, "Or re-run `evaluate()`", fixed = TRUE)

    restored <- quiet_validate(quiet_validate(trimmed), remove_outliers = "response",
                               response_threshold = 1.5)

    expect_null(refusal(restored))

  })

  it("refuses a request at another threshold", {

    ## fit() reuses the recorded rows; fences recomputed at the new threshold
    ## would trim others, so the request used to be ignored
    other <- refusal(quiet_validate(trimmed, remove_outliers = "response",
                                    response_threshold = 3))

    expect_s3_class(other$cnd, "horizons_input_error")
    expect_match(other$message, "threshold is 3 x IQR; `evaluate()` trimmed at 1.5 x IQR",
                 fixed = TRUE)

  })

  it("refuses a request for another outcome or method, naming each", {

    ## configure() clears the request, so only an edited object gets here
    edited <- trimmed
    edited$validation$outliers$response_trim$outcome <- "pH"
    edited$validation$outliers$response_trim$method  <- "mad"

    out <- refusal(edited)

    expect_s3_class(out$cnd, "horizons_input_error")
    expect_match(out$message, "The request is for pH; `evaluate()` trimmed SOC", fixed = TRUE)
    expect_match(out$message, "method is \"mad\"; `evaluate()` used \"iqr\"", fixed = TRUE)
    expect_no_match(out$message, "threshold is", fixed = TRUE)

    ## validate() requests a trim of the configured outcome only, so restoring
    ## the applied request is not offered when the outcomes differ
    expect_no_match(out$message, "restore its request", fixed = TRUE)
    expect_match(out$message, "Re-run `evaluate()` to apply the current request", fixed = TRUE)

  })

  it("compares a trim that drew no fences by the request it ran for", {

    ## Most outcomes equal: the training partition's IQR is zero, so
    ## evaluate() records a skipped trim and trims nothing
    flat <- make_eval_object(n = 60, n_configs = 1)
    flat$data$analysis$SOC[1:48] <- 2
    flat$config$tuning$final_bayesian_iter <- 0L

    skipped <- quiet_evaluate(quiet_validate(flat, remove_outliers = "response"))

    expect_identical(skipped$evaluation$response_trim$skipped, "zero_iqr")
    expect_null(check_trim_request(skipped))

    ## The record says a trim was requested; a request withdrawn since does
    ## not match it
    withdrawn <- refusal(quiet_validate(skipped))

    expect_s3_class(withdrawn$cnd, "horizons_input_error")
    expect_match(withdrawn$message, "no fences could be drawn, so it trimmed no rows",
                 fixed = TRUE)

  })

  it("fits as before when the request matches", {

    skip_unless_slow_tier()

    ## validate() run again with the same request, and with none on an
    ## evaluation that trimmed nothing
    same <- quiet_validate(trimmed, remove_outliers = "response")

    expect_null(check_trim_request(same))
    expect_null(check_trim_request(quiet_validate(untrimmed)))

    f_before <- suppressWarnings(fit(trimmed, compute_uq = FALSE, compute_ad = FALSE,
                                     verbose = FALSE, seed = 307L))
    f_same   <- suppressWarnings(fit(same, compute_uq = FALSE, compute_ad = FALSE,
                                     verbose = FALSE, seed = 307L))

    expect_s3_class(f_same, "horizons_fit")
    expect_identical(f_same$models$results$rmse, f_before$models$results$rmse)
    expect_identical(rsample::training(f_same$models$split)$sample_id,
                     rsample::training(f_before$models$split)$sample_id)

  })

})


## =========================================================================
## The conformal calibration pool is the untrimmed training part (#77)
## =========================================================================
## Split conformal needs calibration rows exchangeable with the rows it will
## be asked about, extremes included. A pool trimmed of them undercovered,
## silently. The pool is the untrimmed training part (the trimmed rows are
## training rows, never test rows); the trimmed rows are dropped from the fit
## rows only.

describe("fit() - calibration after a response trim (#77)", {

  ## n = 250, so the calibration set clears N_CALIB_MIN, with twenty extreme
  ## labels, so some trimmed rows land in it at this seed
  obj <- make_eval_object(n = 250, n_configs = 1)
  extreme <- c(1:10, 101:110)
  obj$data$analysis$SOC[extreme] <- c(seq(20, 38, by = 2), seq(-20, -38, by = -2))
  ## The low extremes are negative, so the outcome is signed (#76)
  obj$config$outcome_range <- c(-Inf, Inf)

  utils::capture.output(
    v <- suppressWarnings(validate(obj, remove_outliers = "response"))
  )

  seen <- NULL

  testthat::with_mocked_bindings(
    tryCatch(
      suppressWarnings(fit(v, compute_uq = TRUE, compute_ad = TRUE,
                           verbose = FALSE, seed = 307L)),
      horizons_all_members_failed = function(e) NULL
    ),
    fit_single_config = function(...) {
      args <- list(...)
      seen <<- list(split = args$split_F, train = args$train_data,
                    calib = args$calib_data, fences = args$response_fences)
      list(config_id = args$config_row$config_id, status = "failed",
           error_message = "mocked", runtime_secs = 0)
    },
    .package = "horizons"
  )

  record  <- suppressWarnings(cold_start_evaluation(v, NULL, 307L))$evaluation
  trimmed <- record$response_trim$trimmed_ids

  train_ids <- seen$train$sample_id
  calib_ids <- seen$calib$sample_id
  test_ids  <- rsample::testing(seen$split)$sample_id

  it("draws calibration rows from the trimmed rows too, and never fits on one", {

    expect_gt(length(trimmed), 0)
    expect_gt(sum(trimmed %in% calib_ids), 0)
    expect_false(any(trimmed %in% train_ids))

  })

  it("keeps calibration, fit and test rows apart, and takes nothing from the test part", {

    expect_length(intersect(calib_ids, train_ids), 0L)
    expect_length(intersect(calib_ids, test_ids), 0L)
    expect_length(intersect(train_ids, test_ids), 0L)

    ## The pool is the untrimmed training partition: the split's training
    ## part plus the trimmed rows, every one of them either calibrating or
    ## fitting, trimmed rows only calibrating
    pool <- c(rsample::training(seen$split)$sample_id, trimmed)

    expect_setequal(c(calib_ids, train_ids, trimmed[!trimmed %in% calib_ids]), pool)

  })

  it("passes the trim's fences to the degradation check", {

    expect_equal(seen$fences,
                 c(lower = record$response_trim$lower, upper = record$response_trim$upper))

  })

})


## =========================================================================
## check_degradation(): a bootstrap interval for the test RPD (#119, #77)
## =========================================================================

describe("check_degradation()", {

  ## 48 test rows from a healthy fit: the point RPD (2.93) sits below the old
  ## cv_mean - 2 * cv_se band (3.389), but its bootstrap interval reaches the
  ## CV mean, so it is not flagged (#119)
  set.seed(11)
  truth   <- stats::rnorm(48, 15, 3)
  healthy <- truth + stats::rnorm(48, 0, 3 / 3.2)
  cv_119  <- tibble::tibble(.metric = "rpd", mean = 3.627, std_err = 0.119)

  ## A fit far worse than its CV estimate
  set.seed(12)
  poor  <- truth + stats::rnorm(48, 0, 3 / 1.1)
  cv_4  <- tibble::tibble(.metric = "rpd", mean = 4, std_err = 0.1)

  it("does not flag a healthy fit whose point estimate falls below the old band", {

    expect_lt(rpd_vec(truth, healthy), 3.627 - 2 * 0.119)
    expect_false(check_degradation(cv_119, truth, healthy)$degraded)

  })

  it("flags a fit whose whole bootstrap interval lies below the CV mean", {

    out <- check_degradation(cv_4, truth, poor)

    expect_true(out$degraded)
    expect_match(out$reason, "95% bootstrap interval", fixed = TRUE)
    expect_match(out$reason, "48 test rows", fixed = TRUE)
    expect_match(out$reason, "below the CV mean RPD 4.000", fixed = TRUE)

  })

  it("is reproducible and leaves the caller's random stream untouched", {

    first  <- check_degradation(cv_4, truth, poor, seed = 7L)
    second <- check_degradation(cv_4, truth, poor, seed = 7L)
    expect_identical(first, second)

    set.seed(9); a <- stats::runif(1)
    set.seed(9); invisible(check_degradation(cv_4, truth, poor)); b <- stats::runif(1)
    expect_identical(a, b)

  })

  ## Thirty test rows inside [0, 30] predicted well, and two extremes far
  ## outside predicted badly
  set.seed(13)
  t_fenced <- c(stats::runif(30, 1, 29), 80, -40)
  p_fenced <- c(t_fenced[1:30] + stats::rnorm(30, 0, 0.5), 15, 15)
  fences   <- c(lower = 0, upper = 30)

  it("compares within the fences when the rows were trimmed", {

    out <- check_degradation(cv_4, t_fenced, p_fenced, response_fences = fences)

    expect_false(out$degraded)
    expect_true(is.na(out$reason))

  })

  it("says the comparison was within the fences when it flags", {

    out <- check_degradation(cv_4, t_fenced, rep(15, 32), response_fences = fences)

    expect_true(out$degraded)
    expect_match(out$reason, "30 test rows of 32 within the training fences [0, 30]",
                 fixed = TRUE)

  })

  it("flags nothing with too few test rows to bootstrap", {

    out <- check_degradation(cv_4, truth[1:5], poor[1:5])

    expect_false(out$degraded)

    out <- check_degradation(cv_4, t_fenced, rep(15, 32),
                             response_fences = c(lower = 100, upper = 200))

    expect_false(out$degraded)

  })

  it("flags nothing when the test outcomes are constant", {

    expect_false(check_degradation(cv_4, rep(5, 20), stats::rnorm(20, 5))$degraded)

  })

  it("flags nothing without a finite CV RPD", {

    no_rpd <- tibble::tibble(.metric = "rmse", mean = 1, std_err = 0.1)

    expect_false(check_degradation(no_rpd, truth, poor)$degraded)

  })

})


## =========================================================================
## format_test_coverage(): the console line for measured coverage (#118)
## =========================================================================

describe("format_test_coverage()", {

  it("reports the measured coverage, the row count and the nominal level", {

    uq <- list(level_default = 0.9, test_coverage = 0.875, n_test = 48L,
               test_mean_width = 3.0412)

    expect_identical(
      format_test_coverage(uq),
      "UQ coverage: 87.5% on 48 held-out test rows (nominal 90%, mean width 3.04)"
    )

  })

  it("says the coverage was not measured when it is missing", {

    expect_match(format_test_coverage(list(level_default = 0.9)),
                 "not measured (nominal 90%)", fixed = TRUE)
    expect_match(format_test_coverage(list(level_default = 0.9, test_coverage = NA_real_)),
                 "not measured", fixed = TRUE)

  })

})
