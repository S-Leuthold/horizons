## ---------------------------------------------------------------------------
## Tests: fit()
## ---------------------------------------------------------------------------
## Integration tests for the fit() pipeline verb. These exercise the full
## pipeline: horizons_data → configure → validate → evaluate → fit.


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
    fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)
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
    fit(obj, n_best = 1L, compute_uq = TRUE, compute_ad = FALSE, seed = 42L)
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
      fit(list(a = 1)),
      "needs a <horizons_eval> from `evaluate()`", fixed = TRUE,
      class = "horizons_input_error"
    )

  })

  it("aborts when no successful configs in evaluation", {

    obj <- mfo()
    obj$evaluation$results$status <- "failed"

    expect_error(
      fit(obj),
      "No configuration in evaluation$results can be fitted", fixed = TRUE,
      class = "horizons_input_error"
    )

  })

  it("aborts when the evaluation record has no results", {

    ## The key is present, but the table is NULL or has no rows: the eval
    ## validator at fit()'s door refuses both.
    obj <- mfo()

    no_results <- obj
    no_results$evaluation["results"] <- list(NULL)

    err <- expect_error(fit(no_results), class = "horizons_error_validation")
    expect_true(any(grepl("results must be a data frame", err$findings, fixed = TRUE)))

    no_rows <- obj
    no_rows$evaluation$results <- no_rows$evaluation$results[0, ]

    err <- expect_error(fit(no_rows), class = "horizons_error_validation")
    expect_true(any(grepl("results has no rows", err$findings, fixed = TRUE)))

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
      fit(obj),
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

    re <- suppressWarnings(evaluate(fitted, prune = FALSE, seed = 42L))

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
               compute_uq = FALSE)
    )

    ## Keep the re-fit cheap; the budget is not what is under test
    ens$config$tuning$final_bayesian_iter <- 0L

    refit <- suppressWarnings(
      fit(ens, n_best = 1L, compute_uq = FALSE, seed = 42L)
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

describe("fit() - its control", {

  it("records the control it ran with, defaults filled in", {

    ctrl <- fit60()$fit$models$control

    expect_s3_class(ctrl, "horizons_control")
    expect_identical(names(ctrl), names(resolve_control(NULL, "fit")))
    expect_identical(ctrl$min_calib_rows, N_CALIB_MIN)

  })

  it("passes its row minimums to every member's fit", {

    seen <- NULL
    local_mocked_bindings(fit_single_config = function(..., min_calib_rows, min_ad_rows, min_degradation_rows) {
      seen <<- c(min_calib_rows, min_ad_rows, min_degradation_rows)
      rlang::abort("stop", class = "member_reached")
    })

    expect_error(
      suppressWarnings(fit(mfo(), n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                           control = control_horizons(min_calib_rows = 12L, min_ad_rows = 5L,
                                                      min_degradation_rows = 6L))),
      class = "member_reached"
    )
    expect_identical(seen, c(12L, 5L, 6L))

  })

  it("gates UQ and AD on the calibration rows it is given as the minimum", {

    ## 60 rows: 48 in Split F's training part, 10 of them calibration rows
    obj <- mfo()
    run <- function(min_calib_rows) {
      calib <- NULL
      local_mocked_bindings(fit_single_config = function(..., calib_data) {
        calib <<- calib_data
        rlang::abort("stop", class = "member_reached")
      })
      try(suppressWarnings(fit(obj, n_best = 1L, control = control_horizons(min_calib_rows = min_calib_rows))),
          silent = TRUE)
      calib
    }

    expect_null(run(N_CALIB_MIN))
    expect_false(is.null(run(9L)))

  })

})


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
    fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = TRUE, seed = 42L)
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
        fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
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
          fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE)
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
      fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, seed = seed)
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
      fit(stale, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
      "does not index the rows this object models",
      class = "horizons_input_error"
    )

    ## Same ids, one outcome changed: the split's rows no longer carry the
    ## outcomes it was stratified and scored on.
    relabelled <- obj
    relabelled$data$analysis$SOC[1] <- relabelled$data$analysis$SOC[1] + 1

    expect_error(
      fit(relabelled, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
      "does not index the rows this object models",
      class = "horizons_input_error"
    )

    ## A record without the key, or with a split that is not an rsplit, is
    ## refused by the eval validator at fit()'s door.
    no_split <- obj
    no_split$evaluation$split <- NULL

    err <- expect_error(
      fit(no_split, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
      class = "horizons_error_validation"
    )
    expect_true(any(grepl("Metadata keys missing from evaluation: split", err$findings, fixed = TRUE)))

    bad_split <- obj
    bad_split$evaluation$split <- "an rsplit"

    err <- expect_error(
      fit(bad_split, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
      class = "horizons_error_validation"
    )
    expect_true(any(grepl("split must be an <rsplit> object", err$findings, fixed = TRUE)))

  })

  it("still fits after add_response() adds a sibling response", {

    obj <- fit250_uq()$obj

    lab <- tibble::tibble(sample_id = obj$data$analysis$sample_id,
                          clay      = seq_len(nrow(obj$data$analysis)))

    utils::capture.output(
      with_clay <- add_response(obj, lab, variable = "clay")
    )

    r_clay <- suppressWarnings(
      fit(with_clay, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)
    )

    expect_s3_class(r_clay, "horizons_fit")
    expect_true("clay" %in% names(r_clay$models$split$data))
    expect_identical(rsample::testing(r_clay$models$split)$sample_id,
                     rsample::testing(obj$evaluation$split)$sample_id)

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
          fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)
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
## allow_par with no backend (M2e, 2026-09-15)
## =========================================================================

describe("fit() - allow_par without a usable backend", {

  it("warns once naming fit() and the plan, on the log too, then runs sequentially", {

    skip_unless_slow_tier()

    local_plan(future::sequential)
    obj <- make_fit_object(n = 60, n_configs = 1)
    obj$config$tuning$final_bayesian_iter <- 0L

    got <- collect_warnings(
      fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, seed = 123L,
          control = control_horizons(allow_par = TRUE)),
      "offers 1 worker"
    )
    r <- got$value
    w <- got$warnings[[1]]

    expect_length(got$warnings, 1L)
    expect_s3_class(w, "horizons_warning_substitution")
    expect_match(conditionMessage(w), "fit\\(\\).*offers 1 worker")
    expect_s3_class(r, "horizons_fit")
    expect_true("substitution" %in% r$log$topic[r$log$verb == "fit"])

  })

})

## On a real two-worker plan tune runs the re-tune and the OOF fits on
## workers, which advances the parent's RNG stream differently from the
## sequential loop. fit_single_config() re-pins the seed before each
## stochastic stage so the result does not depend on that; without the
## re-pin before the OOF fits, the rf member's CV metrics differ between the
## two runs. The sequential run is the shared fit60().

describe("fit() - on a two-worker plan", {

  it("matches the sequential fit exactly (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    shared <- fit60()
    local_plan(future::multisession, workers = 2)

    par_fit <- suppressWarnings(
      fit(shared$obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L, control = control_horizons(allow_par = TRUE))
    )

    cols <- c("config_id", "status", "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
              "cv_rmse_mean", "cv_rmse_se", "cv_rpd_mean", "cv_rpd_se")

    expect_equal(par_fit$models$results[cols], shared$fit$models$results[cols])
    expect_equal(par_fit$models$results$best_params,
                 shared$fit$models$results$best_params)
    expect_equal(par_fit$models$cv_predictions, shared$fit$models$cv_predictions)

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
      fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)
    )

    expect_true(result$models$selection_present)

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
        suppressWarnings(fit(obj, seed = 42L, control = control_horizons(verbose = TRUE), ...)),
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
## The progress tree
## =========================================================================

describe("fit() - the progress tree", {

  it("prints each member's result, the counts, the selected member and the runtime", {

    ## Read the fixture before the runner is mocked (helper-memo.R)
    obj <- mfo()

    ## The runner's results for the two members: one that succeeded, degraded
    ## and warned, and one that failed. The fitted workflow is a stand-in;
    ## nothing past the validator, which does not check its class, reads it.
    members <- list(
      cfg_001 = list(
        config_id = "cfg_001", status = "success", degraded = TRUE,
        degraded_reason = "test RPD 1.100 (95% bootstrap interval [0.900, 1.300], 12 test rows) is below the CV mean RPD 2.000",
        fitted_workflow = "fitted workflow", best_params = tibble::tibble(mtry = 2L, min_n = 5L),
        warm_start = TRUE, start_grid_size = 2L, cv_predictions = NULL,
        test_metrics = tibble::tibble(rmse = 0.512, rrmse = 20.4, rsq = 0.61,
                                      ccc = 0.72, rpd = 1.1, mae = 0.41),
        cv_metrics = tibble::tibble(.metric = c("rmse", "rpd"), mean = c(0.4567, 2.0),
                                    std_err = c(0.0321, 0.1234)),
        uq = NULL, ad = NULL, warnings = "The final fit warned",
        error_message = NA_character_, runtime_secs = 12.34
      ),
      cfg_002 = list(
        config_id = "cfg_002", status = "failed", degraded = NA, degraded_reason = NA_character_,
        fitted_workflow = NULL, best_params = NULL, warm_start = TRUE, start_grid_size = 2L,
        cv_predictions = NULL, test_metrics = NULL, cv_metrics = NULL, uq = NULL, ad = NULL,
        warnings = NULL, error_message = "Warm-start tuning failed: mocked", runtime_secs = 3.4
      )
    )

    local_mocked_bindings(fit_single_config = function(config_row, ...) members[[config_row$config_id]])

    ## The clock reads a minute and a half after the start, so the runtime
    ## line is in minutes
    start <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
    ticks <- 0L
    clock <- function() {
      ticks <<- ticks + 1L
      if (ticks == 1L) start else start + 90
    }

    run <- function() {
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE,
                             seed = 42L, control = control_horizons(verbose = TRUE))),
        Sys.time = clock, .package = "base"
      )
    }

    expect_snapshot(invisible(run()))

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

  it("takes its minimum number of test rows from min_n", {

    ## Twelve of the poor fit's test rows: flagged at a minimum of 10, not at 20
    expect_true(check_degradation(cv_4, truth[1:12], poor[1:12])$degraded)
    expect_false(check_degradation(cv_4, truth[1:12], poor[1:12], min_n = 20L)$degraded)

  })

  it("is reproducible and leaves the caller's random stream untouched", {

    first  <- check_degradation(cv_4, truth, poor, seed = 7L)
    second <- check_degradation(cv_4, truth, poor, seed = 7L)
    expect_identical(first, second)

    set.seed(9); a <- stats::runif(1)
    set.seed(9); invisible(check_degradation(cv_4, truth, poor)); b <- stats::runif(1)
    expect_identical(a, b)

    ## A caller with no stream yet is left with none
    withr::with_preserve_seed({
      rm(".Random.seed", envir = globalenv())
      invisible(check_degradation(cv_4, truth, poor))
      expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    })

  })

  it("scores a bootstrap resample of tied test outcomes as an RPD of 0", {

    ## Eight of ten outcomes tie, so about one resample in ten draws only the
    ## tied rows: its SD is 0 while its RMSE is not, and its RPD is 0, which
    ## then sets the interval's lower bound. Every error is 1 and no
    ## resample's SD reaches 2.2, so the whole interval lies below 4 whatever
    ## the draws.
    tied <- c(rep(5, 8), 6, 9)

    out <- check_degradation(cv_4, tied, tied + c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1))

    expect_true(out$degraded)
    expect_match(out$reason, "95% bootstrap interval [0.000, ", fixed = TRUE)

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
