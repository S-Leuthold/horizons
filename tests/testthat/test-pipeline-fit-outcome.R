## ---------------------------------------------------------------------------
## Tests: fit() and the outcome's rows and range
## ---------------------------------------------------------------------------
## fit() with rows that have no measured outcome (#67), with a response bound
## (#68), and after evaluate()'s response trim (#77, #137). Each block builds
## its own objects; make_fit_object() is in helper-fixtures.R, and the shared
## fit fixtures stay in test-pipeline-fit.R.


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
