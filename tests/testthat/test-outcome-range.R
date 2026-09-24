## ---------------------------------------------------------------------------
## Tests: configure(outcome_range = ) and every clamp that reads it (#76)
## ---------------------------------------------------------------------------
## Two package-wide assumptions made every outcome non-negative: fit()'s
## response bound (max * 1.5, validated positive) and a zero floor on every
## scored and served prediction. A signed property such as d13C aborted after
## all of fit()'s tuning, or had its negative predictions clamped to zero.
## configure()'s outcome_range replaces both, with c(0, Inf) as the default so
## nothing changes for a non-negative outcome.


## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

#' A pre-configure horizons_data built on make_eval_object(): SOC is a
#' response, not yet an outcome, and there is no grid. `value` maps the
#' fixture's SOC (around 2, all positive) to the outcome under test.
#' @noRd
make_range_hd <- function(n = 60, value = identity) {

  obj <- make_eval_object(n = n, n_configs = 1)

  obj$data$analysis$SOC <- value(obj$data$analysis$SOC)
  obj$data$role_map$role[obj$data$role_map$variable == "SOC"] <- "response"
  obj$data$n_responses  <- 1L
  obj$config$configs    <- NULL
  obj$config$n_configs  <- NULL

  obj

}

#' configure() with a small, fast grid: one rf configuration, no Bayesian
#' stage, three folds. Console output is discarded.
#' @noRd
configure_small <- function(x, ...) {

  invisible(utils::capture.output(
    res <- configure(x, models = "rf", cv_folds = 3L, grid_size = 2L,
                     bayesian_iter = 0L, final_bayesian_iter = 0L, ...)
  ))

  res

}

## A d13C-like outcome: the fixture's SOC stretched and shifted to roughly
## -27 to -17 per mil, every value negative.
to_d13c <- function(soc) -22 + 3 * (soc - 2)

## A condition's message on one line: cli wraps long messages, which would
## split the phrases the tests look for.
flat_message <- function(e) gsub("\\s+", " ", conditionMessage(e))


## ===========================================================================
## The rule, the accessor and the clamp
## ===========================================================================

describe("is_valid_outcome_range()", {

  it("accepts two numbers, lower below upper, either end infinite", {

    expect_true(is_valid_outcome_range(c(0, Inf)))
    expect_true(is_valid_outcome_range(c(-Inf, Inf)))
    expect_true(is_valid_outcome_range(c(-Inf, 0)))
    expect_true(is_valid_outcome_range(c(-5, 5)))
    expect_true(is_valid_outcome_range(c(0L, 100L)))

  })

  it("refuses anything else", {

    for (bad in list(c(1, 1), c(2, 1), c(0, NA), c(NaN, 1), 0, c(0, 1, 2),
                     "a", NULL, c(Inf, Inf), c(-Inf, -Inf))) {

      expect_false(is_valid_outcome_range(bad), info = deparse(bad))

    }

  })

})

describe("outcome_range_setting()", {

  it("reads the recorded range", {

    x <- list(config = list(outcome_range = c(-Inf, Inf)))
    expect_identical(outcome_range_setting(x), c(-Inf, Inf))

  })

  it("gives an object configured before the range existed the zero floor", {

    expect_identical(outcome_range_setting(list(config = list(configs = NULL))),
                     DEFAULT_OUTCOME_RANGE)
    expect_identical(DEFAULT_OUTCOME_RANGE, c(0, Inf))

  })

  it("refuses a stored range configure() could not have written", {

    x <- list(config = list(outcome_range = c(5, 1)))
    expect_error(outcome_range_setting(x), class = "horizons_validation_error")

  })

})

describe("clamp_to_outcome_range()", {

  it("floors at zero under the default range, passing NAs through", {

    expect_identical(clamp_to_outcome_range(c(-2, NA, 0.5, 1e6)),
                     c(0, NA, 0.5, 1e6))

  })

  it("clamps nothing under c(-Inf, Inf), and both ends under a finite range", {

    x <- c(-30, -1, NA, 4, 150)

    expect_identical(clamp_to_outcome_range(x, c(-Inf, Inf)), x)
    expect_identical(clamp_to_outcome_range(x, c(-10, 100)),
                     c(-10, -1, NA, 4, 100))

  })

})


## ===========================================================================
## back_transform_predictions() and the tuning metrics
## ===========================================================================

describe("back_transform_predictions() - outcome_range", {

  it("defaults to the zero floor it has always applied", {

    expect_identical(eval(formals(back_transform_predictions)$outcome_range),
                     DEFAULT_OUTCOME_RANGE)
    expect_identical(back_transform_predictions(c(-2, 3), "none", warn = FALSE),
                     c(0, 3))

  })

  it("keeps negative predictions under a signed range", {

    expect_identical(
      back_transform_predictions(c(-25, -18), "none", warn = FALSE,
                                 outcome_range = c(-Inf, Inf)),
      c(-25, -18)
    )

    ## log(x + 1) back-transforms below zero for x in (-1, 0); only the
    ## default range floors it
    expect_equal(back_transform_predictions(log(0.5), "log", warn = FALSE,
                                            outcome_range = c(-Inf, Inf)),
                 -0.5)
    expect_identical(back_transform_predictions(log(0.5), "log", warn = FALSE), 0)

  })

  it("caps at a finite upper bound, silently", {

    expect_silent(
      out <- back_transform_predictions(c(50, 140), "none", warn = FALSE,
                                        outcome_range = c(0, 100))
    )
    expect_identical(out, c(50, 100))

  })

  it("accepts a negative upper_bound only when the range reaches below it", {

    expect_error(back_transform_predictions(c(-20, -10), "none", upper_bound = -12))

    expect_warning(
      out <- back_transform_predictions(c(-20, -10), "none", upper_bound = -12,
                                        outcome_range = c(-Inf, Inf)),
      "winsorized"
    )
    expect_identical(out, c(-20, -12))

  })

  it("refuses a malformed range", {

    expect_error(back_transform_predictions(1, "none", outcome_range = c(1, 0)),
                 "outcome_range")

  })

})

describe("tuning_metric_set() - outcome_range", {

  ## The truth sits below zero; the log-scale estimate back-transforms to
  ## -0.5. Under the default range the estimate is floored to 0 before it is
  ## scored, under a signed range it is scored as it is.
  df <- tibble::tibble(truth = c(-0.5, -0.4, -0.6),
                       estimate = log(c(0.5, 0.6, 0.4)))

  it("scores the clamped estimate, as the test-set scoring does", {

    floored <- tuning_metric_set("log", metrics = "rmse")(df, truth = truth, estimate = estimate)
    signed  <- tuning_metric_set("log", metrics = "rmse",
                                 outcome_range = c(-Inf, Inf))(df, truth = truth, estimate = estimate)

    expect_equal(signed$.estimate, 0)
    expect_equal(floored$.estimate, sqrt(mean(df$truth^2)))

  })

})


## ===========================================================================
## compute_response_bound(): above the maximum for any sign, unchanged by default
## ===========================================================================

describe("compute_response_bound()", {

  it("is max * RESPONSE_BOUND_MARGIN, to the bit, under the default range", {

    set.seed(76)
    tops <- c(stats::runif(500, 0.001, 1e4), 45.2, 1, 0.1, 3.7e-5, 1e12)

    got <- vapply(tops, function(t) compute_response_bound(c(t / 3, NA, t)), numeric(1))

    expect_identical(got, tops * RESPONSE_BOUND_MARGIN)

  })

  it("lies strictly above the maximum for a negative, a zero and a mixed-sign maximum", {

    neg   <- compute_response_bound(c(-30, -22, -15), c(-Inf, Inf))
    zero  <- compute_response_bound(c(-4, -1, 0), c(-Inf, Inf))
    mixed <- compute_response_bound(c(-3, 0.5, 2), c(-Inf, Inf))

    expect_identical(neg, -15 + 0.5 * 15)
    expect_gt(neg, -15)
    expect_gt(zero, 0)
    expect_gt(mixed, 2)

  })

  it("anchors on a finite lower bound, and caps at a finite upper one", {

    expect_identical(compute_response_bound(c(-3, 2), c(-5, Inf)), 2 + 0.5 * 7)
    expect_identical(compute_response_bound(c(10, 80), c(0, 100)), 100)
    expect_identical(compute_response_bound(c(10, 50), c(0, 100)), 75)

  })

})


## ===========================================================================
## configure()
## ===========================================================================

describe("configure() - outcome_range", {

  it("records the non-negative default, the value an older object reads as", {

    res <- configure_small(make_range_hd())

    expect_identical(res$config$outcome_range, DEFAULT_OUTCOME_RANGE)
    expect_identical(eval(formals(configure)$outcome_range), DEFAULT_OUTCOME_RANGE)
    expect_true("outcome_range" %in% names(new_horizons_data()$config))

  })

  it("records a range as an unnamed double, and prints it when it is not the default", {

    out <- utils::capture.output(
      res <- configure(make_range_hd(value = to_d13c), models = "rf",
                       outcome_range = c(lower = -Inf, upper = Inf))
    )

    expect_identical(res$config$outcome_range, c(-Inf, Inf))
    expect_true(any(grepl("range c(-Inf, Inf)", out, fixed = TRUE)))

    res_int <- configure_small(make_range_hd(), outcome_range = c(0L, 100L))
    expect_identical(res_int$config$outcome_range, c(0, 100))

  })

  it("refuses a malformed range", {

    for (bad in list(c(1, 0), c(0, 0), 5, c(0, NA), "c(0, Inf)")) {

      expect_error(
        configure_small(make_range_hd(), outcome_range = bad),
        "`outcome_range` must be a numeric vector of length 2",
        class = "horizons_configure_error",
        info  = deparse(bad)
      )

    }

  })

  it("refuses a negative outcome under the default, classed and naming outcome_range and its remedy", {

    err <- expect_error(
      configure_small(make_range_hd(value = to_d13c)),
      class = "horizons_input_error"
    )

    expect_s3_class(err, "horizons_configure_error")
    expect_match(conditionMessage(err), "outcome_range")
    expect_match(conditionMessage(err), "c(-Inf, Inf)", fixed = TRUE)

  })

  it("refuses an outcome above a finite upper bound", {

    expect_error(
      configure_small(make_range_hd(), outcome_range = c(0, 1)),
      "outcome_range",
      class = "horizons_input_error"
    )

  })

  it("ignores missing outcome values", {

    hd <- make_range_hd(value = function(v) { v[1:3] <- NA_real_; v })

    expect_no_error(configure_small(hd))

  })

  it("applies the range to the outcome it configures, not to other responses", {

    ## Two responses: SOC (positive) and d13C (negative). One object models
    ## one outcome, so the range belongs to that outcome alone.
    hd <- make_range_hd()
    hd$data$analysis$d13C <- to_d13c(hd$data$analysis$SOC)
    hd$data$role_map <- rbind(hd$data$role_map,
                              tibble::tibble(variable = "d13C", role = "response"))
    hd$data$n_responses <- 2L

    expect_no_error(configure_small(hd, outcome = "SOC"))
    expect_error(configure_small(hd, outcome = "d13C"), class = "horizons_input_error")
    expect_no_error(configure_small(hd, outcome = "d13C", outcome_range = c(-Inf, Inf)))

  })

})


## ===========================================================================
## A signed outcome end to end: configure -> evaluate -> fit -> predict
## ===========================================================================

describe("a d13C-like outcome with outcome_range = c(-Inf, Inf)", {

  ## n = 250 leaves enough rows for the calibration split to clear
  ## N_CALIB_MIN, so the intervals are exercised too.
  hd     <- make_range_hd(n = 250, value = to_d13c)
  cfg    <- configure_small(hd, outcome_range = c(-Inf, Inf))
  outdir <- withr::local_tempdir()

  ev <- suppressWarnings(
    evaluate(cfg, prune = FALSE, verbose = FALSE, seed = 42L, output_dir = outdir)
  )

  fitted <- suppressWarnings(
    fit(ev, n_best = 1L, compute_uq = TRUE, compute_ad = FALSE,
        verbose = FALSE, seed = 42L)
  )

  truth    <- hd$data$analysis$SOC
  new_data <- hd$data$analysis[1:8, setdiff(names(hd$data$analysis), "SOC")]
  p        <- predict(fitted, new_data, interval = TRUE)

  it("has an all-negative outcome (the fixture discriminates)", {

    expect_true(all(truth < 0))
    expect_true(all(truth > -30 & truth < -15))

  })

  it("evaluates, scoring test-set predictions that were not floored at zero", {

    res <- ev$evaluation$results

    expect_identical(res$status, "success")

    ## Floored at zero, every prediction would miss by more than 15 per mil,
    ## and so would the RMSE. Scored as predicted it is about the outcome's
    ## own spread.
    expect_lt(res$rmse, 5)
    expect_lt(res$cv_rmse, 5)
    expect_gt(res$rpd, 0.5)

  })

  it("fits, with negative out-of-fold predictions and a negative bound above the maximum", {

    expect_s3_class(fitted, "horizons_fit")
    expect_true(all(fitted$models$cv_predictions$.pred < 0))
    expect_lt(fitted$models$results$rmse, 5)

    fit_rows <- hd$data$analysis$sample_id %in% fitted$models$row_index$sample_id
    y_fit    <- truth[fit_rows]

    expect_identical(fitted$models$response_bound,
                     max(y_fit) + (RESPONSE_BOUND_MARGIN - 1) * (max(y_fit) - min(y_fit)))
    expect_lt(fitted$models$response_bound, 0)
    expect_gt(fitted$models$response_bound, max(y_fit))

  })

  it("predicts negative points and negative interval bounds", {

    expect_true(all(p$.pred < 0))
    expect_true(all(c(".pred_lower", ".pred_upper") %in% names(p)))
    expect_true(all(p$.pred_upper < 0))
    expect_true(all(p$.pred_lower <= p$.pred_upper))

  })

  it("fingerprints the range: a resume under another range refuses", {

    other <- cfg
    other$config$outcome_range <- c(-40, 0)

    expect_error(
      suppressWarnings(evaluate(other, prune = FALSE, verbose = FALSE,
                                seed = 42L, output_dir = outdir)),
      "outcome_range",
      class = "horizons_input_error"
    )

  })

  it("refuses to re-fit the evaluated object under the default range, before fitting", {

    stale <- ev
    stale$config$outcome_range <- DEFAULT_OUTCOME_RANGE

    local_mocked_bindings(
      fit_single_config = function(...) stop("fit_single_config() was reached"),
      .package = "horizons"
    )

    expect_error(fit(stale, n_best = 1L, verbose = FALSE), "outcome_range",
                 class = "horizons_input_error")

  })

  it("refuses to fit an evaluation scored under another range, before fitting", {

    ## c(-40, 0) contains the data, so only the comparison with the range
    ## evaluate() recorded on its rows can refuse it
    moved <- ev
    moved$config$outcome_range <- c(-40, 0)

    local_mocked_bindings(
      fit_single_config = function(...) stop("fit_single_config() was reached"),
      .package = "horizons"
    )

    err <- expect_error(fit(moved, n_best = 1L, verbose = FALSE),
                        class = "horizons_input_error")
    expect_match(flat_message(err), "outcome_range")
    expect_match(flat_message(err), "c(-Inf, Inf)", fixed = TRUE)
    expect_match(flat_message(err), "c(-40, 0)", fixed = TRUE)

  })

  it("refuses to rank by rrmse, in evaluate() and in fit(), before tuning", {

    local_mocked_bindings(
      evaluate_single_config = function(...) stop("evaluate_single_config() was reached"),
      fit_single_config      = function(...) stop("fit_single_config() was reached"),
      .package = "horizons"
    )

    expect_error(evaluate(cfg, metric = "rrmse", verbose = FALSE), "rrmse",
                 class = "horizons_input_error")
    expect_error(fit(ev, metric = "rrmse", verbose = FALSE), "rrmse",
                 class = "horizons_input_error")

    ## ...and when the rrmse comes from the evaluation fit() would re-tune
    by_rrmse <- ev
    by_rrmse$evaluation$rank_metric <- "rrmse"
    expect_error(fit(by_rrmse, verbose = FALSE), "rrmse",
                 class = "horizons_input_error")

  })

  it("refuses to resume checkpoint rows that record no range", {

    ## A copy of one row as it would have been written before #76: the same
    ## data and settings, less the outcome_range. Under c(-Inf, Inf) it was
    ## scored differently (floored at zero), so it is refused, naming it.
    row <- readRDS(list.files(file.path(outdir, "checkpoints"), full.names = TRUE)[1])
    row$settings[[1]]$outcome_range <- NULL

    legacy <- withr::local_tempdir()
    dir.create(file.path(legacy, "checkpoints"))
    saveRDS(row, file.path(legacy, "checkpoints", paste0(row$config_id, ".rds")))

    local_mocked_bindings(
      evaluate_single_config = function(...) stop("evaluate_single_config() was reached"),
      .package = "horizons"
    )

    err <- expect_error(
      suppressWarnings(evaluate(cfg, prune = FALSE, verbose = FALSE, seed = 42L,
                                output_dir = legacy)),
      class = "horizons_input_error"
    )
    expect_match(flat_message(err), "outcome_range = unset", fixed = TRUE)

  })

  it("ensemble() refuses an outcome outside the range at entry", {

    stale <- fitted
    stale$config$outcome_range <- DEFAULT_OUTCOME_RANGE

    ## One member would also be refused, later; the range check comes first
    expect_error(ensemble(stale, verbose = FALSE), "outcome_range",
                 class = "horizons_input_error")

  })

  it("summary() shows the range when it is not the default", {

    out <- utils::capture.output(summary(cfg))
    expect_true(any(grepl("Outcome range: c(-Inf, Inf)", out, fixed = TRUE)))

    plain <- utils::capture.output(summary(configure_small(make_range_hd())))
    expect_false(any(grepl("Outcome range", plain, fixed = TRUE)))

  })

})


## ===========================================================================
## The same outcome under the default range: refused before any tuning
## ===========================================================================

describe("a negative outcome under the default range", {

  ## make_eval_object() builds its object by hand, with no
  ## config$outcome_range, like an object configured before the range
  ## existed. configure() would have refused the outcome; evaluate() and a
  ## cold-start fit() must refuse it before they draw or tune anything.
  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$data$analysis$SOC <- to_d13c(obj$data$analysis$SOC)

  ## Each verb's expensive steps are replaced with recorders: none may run.
  mock_expensive_steps <- function(env = parent.frame()) {

    reached <- new.env()
    reached$steps <- character()

    local_mocked_bindings(
      draw_eval_split        = function(...) { reached$steps <- c(reached$steps, "draw_eval_split");        stop("drew a split") },
      evaluate_single_config = function(...) { reached$steps <- c(reached$steps, "evaluate_single_config"); stop("tuned a config") },
      fit_single_config      = function(...) { reached$steps <- c(reached$steps, "fit_single_config");      stop("fitted a config") },
      .package = "horizons",
      .env     = env
    )

    reached

  }

  it("evaluate() aborts, classed, naming outcome_range, before a split or a config", {

    reached <- mock_expensive_steps()

    elapsed <- system.time(
      err <- expect_error(evaluate(obj, verbose = FALSE), class = "horizons_input_error")
    )[["elapsed"]]

    expect_match(conditionMessage(err), "outcome_range")
    expect_match(conditionMessage(err), "c(-Inf, Inf)", fixed = TRUE)
    expect_length(reached$steps, 0)
    expect_lt(elapsed, 5)

  })

  it("a cold-start fit() aborts the same way, before a split or a config", {

    reached <- mock_expensive_steps()

    err <- expect_error(fit(obj, verbose = FALSE), class = "horizons_input_error")

    expect_match(conditionMessage(err), "outcome_range")
    expect_length(reached$steps, 0)

  })

})


## ===========================================================================
## A mixed-sign outcome under an explicit finite range
## ===========================================================================

describe("a mixed-sign outcome with outcome_range = c(-5, 5)", {

  hd  <- make_range_hd(n = 60, value = function(soc) soc - 2)
  cfg <- configure_small(hd, outcome_range = c(-5, 5))

  cold <- suppressWarnings(
    fit(cfg, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L)
  )

  y <- hd$data$analysis$SOC

  it("has both signs (the fixture discriminates)", {

    expect_true(any(y < 0) && any(y > 0))

  })

  it("anchors the bound on the finite lower bound", {

    fit_rows <- hd$data$analysis$sample_id %in% cold$models$row_index$sample_id
    top      <- max(y[fit_rows])

    expect_identical(cold$models$response_bound,
                     top + (RESPONSE_BOUND_MARGIN - 1) * (top - (-5)))

  })

  it("serves predictions of both signs", {

    p <- predict(cold, hd$data$analysis[, setdiff(names(hd$data$analysis), "SOC")],
                 interval = FALSE)

    expect_true(any(p$.pred < 0) && any(p$.pred > 0))
    expect_true(all(p$.pred >= -5 & p$.pred <= 5))

  })

})


## ===========================================================================
## The default: floor at zero and today's bound, for a non-negative outcome
## ===========================================================================

describe("a non-negative outcome under the default range", {

  ## make_eval_object() carries no config$outcome_range: an object from
  ## before the range existed. The bound and the floor must be what they were.
  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L

  fitted <- suppressWarnings(
    fit(obj, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L)
  )

  new_data <- obj$data$analysis[1:5, setdiff(names(obj$data$analysis), "SOC")]

  it("stores max * RESPONSE_BOUND_MARGIN as the bound, to the bit", {

    fit_rows <- obj$data$analysis$sample_id %in% fitted$models$row_index$sample_id

    expect_null(fitted$config$outcome_range)
    expect_identical(fitted$models$response_bound,
                     max(obj$data$analysis$SOC[fit_rows]) * RESPONSE_BOUND_MARGIN)

  })

  it("validates without the key, and with it removed from a fitted object", {

    expect_no_error(validate_horizons_fit(fitted))

    ## the constructor's shape with the key present but NULL
    keyed <- fitted
    keyed$config["outcome_range"] <- list(NULL)
    expect_no_error(validate_horizons_fit(keyed))

  })

  it("floors a negative prediction at zero, as predict() always has", {

    ## The fitted forest cannot predict below its training range, so the
    ## stored workflow's predict method is replaced with one that does.
    local_mocked_s3_method("predict", "workflow", function(object, new_data, ...) {
      tibble::tibble(.pred = rep(-1, nrow(new_data)))
    })

    old <- predict(fitted, new_data, interval = FALSE)
    expect_identical(old$.pred, rep(0, 5))

    ## The same object configured for a signed outcome serves the -1
    signed <- fitted
    signed$config$outcome_range <- c(-Inf, Inf)
    expect_identical(predict(signed, new_data, interval = FALSE)$.pred, rep(-1, 5))

  })

  it("rejects a bound at or below the floor, as the validator always has", {

    neg <- fitted
    neg$models$response_bound <- -5
    expect_error(suppressMessages(validate_horizons_fit(neg)),
                 class = "horizons_validation_error")

    ## ...which a signed range accepts
    neg$config$outcome_range <- c(-Inf, Inf)
    expect_no_error(validate_horizons_fit(neg))

  })

})


## ===========================================================================
## A finite upper bound caps predictions
## ===========================================================================

describe("a finite upper bound", {

  ## A cap test only, not a way to choose a range: a range comes from the
  ## property's physical bounds, never from the data. Here the upper bound is
  ## the largest observed value so the bound formula (1.5 times the fit rows'
  ## maximum) lands above it and is capped.
  hd  <- make_range_hd(n = 60)
  top <- max(hd$data$analysis$SOC)
  cfg <- configure_small(hd, outcome_range = c(0, top))

  cold <- suppressWarnings(
    fit(cfg, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L)
  )

  it("caps the response bound", {

    expect_identical(cold$models$response_bound, top)
    expect_no_error(validate_horizons_fit(cold))

  })

  it("caps served predictions at the range, silently", {

    local_mocked_s3_method("predict", "workflow", function(object, new_data, ...) {
      tibble::tibble(.pred = rep(1e3, nrow(new_data)))
    })

    new_data <- hd$data$analysis[1:4, setdiff(names(hd$data$analysis), "SOC")]

    expect_no_warning(p <- predict(cold, new_data, interval = FALSE))
    expect_identical(p$.pred, rep(top, 4))

  })

  it("refuses a bound above the upper bound", {

    over <- cold
    over$models$response_bound <- top + 1
    expect_error(suppressMessages(validate_horizons_fit(over)),
                 class = "horizons_validation_error")

  })

})


## ===========================================================================
## The ensemble combine and its fold models read the range too
## ===========================================================================

describe("ensemble combines and fold predictions - outcome_range", {

  member_pred <- tibble::tibble(
    config_id = rep(c("a", "b"), each = 2),
    sample_id = rep(c("s1", "s2"), 2),
    .pred     = c(-20, -18, -22, -16)
  )
  weights <- tibble::tibble(member = c("a", "b"), coef = c(0.5, 0.5))

  it("combine_ensemble_weighted() floors by default and keeps a signed combination", {

    expect_identical(combine_ensemble_weighted(member_pred, weights)$.pred, c(0, 0))
    expect_identical(combine_ensemble_weighted(member_pred, weights, c(-Inf, Inf))$.pred,
                     c(-21, -17))

  })

  it("predict_fold_model() clamps a weighted fold model's output to the range", {

    mat <- data.frame(member_a = c(-20, -18), member_b = c(-22, -16))

    expect_identical(predict_fold_model(weights, "weighted", mat), c(0, 0))
    expect_identical(predict_fold_model(weights, "weighted", mat, c(-Inf, Inf)),
                     c(-21, -17))
    expect_identical(predict_fold_model(weights, "weighted", mat, c(-18, Inf)),
                     c(-18, -17))

  })

})


## ===========================================================================
## Review follow-ups (#76)
## ===========================================================================

describe("configure() - a range with a negative floor and a response transform", {

  it("refuses log, log10 and sqrt, naming both arguments", {

    for (tr in c("log", "log10", "sqrt")) {

      err <- expect_error(
        configure_small(make_range_hd(value = to_d13c),
                        transformations = c("none", tr),
                        outcome_range = c(-Inf, Inf)),
        class = "horizons_configure_error",
        info  = tr
      )

      expect_match(flat_message(err), "transformations", info = tr)
      expect_match(flat_message(err), "outcome_range", info = tr)
      expect_match(flat_message(err), tr, fixed = TRUE, info = tr)

    }

    ## A floor just below zero is enough: sqrt is NaN there
    expect_error(
      configure_small(make_range_hd(), transformations = "sqrt",
                      outcome_range = c(-0.5, Inf)),
      class = "horizons_configure_error"
    )

  })

  it("accepts them under the default range, and 'none' under a signed one", {

    expect_no_error(configure_small(make_range_hd(),
                                    transformations = c("log", "log10", "sqrt")))
    expect_no_error(configure_small(make_range_hd(value = to_d13c),
                                    transformations = "none",
                                    outcome_range = c(-Inf, Inf)))

  })

})

describe("check_rank_metric_range()", {

  it("refuses rrmse only under a negative floor", {

    expect_error(check_rank_metric_range("rrmse", c(-Inf, Inf), "evaluate"),
                 "rrmse", class = "horizons_input_error")
    expect_error(check_rank_metric_range("rrmse", c(-5, 5), "fit"),
                 class = "horizons_input_error")

    expect_null(check_rank_metric_range("rrmse", DEFAULT_OUTCOME_RANGE, "evaluate"))
    expect_null(check_rank_metric_range("rrmse", c(0, 100), "evaluate"))
    expect_null(check_rank_metric_range("rmse", c(-Inf, Inf), "evaluate"))
    expect_null(check_rank_metric_range(NULL, c(-Inf, Inf), "fit"))

  })

})

describe("checkpoint rows that record no outcome_range", {

  ## As written before #76: settings stamped, but without the range
  row <- tibble::tibble(config_id = "a", scoring_schema = SCORING_SCHEMA)
  row <- stamp_eval_settings(row, eval_settings(cv_folds = 3L, grid_size = 2L))

  no_stamp <- tibble::tibble(config_id = "a", scoring_schema = SCORING_SCHEMA)
  data_fp  <- list(data_hash = NA_character_, data_n_rows = 10L)

  it("are refused under a range other than the default, naming the range", {

    v <- checkpoint_row_verdict(
      row, data_fp,
      eval_settings(cv_folds = 3L, grid_size = 2L, outcome_range = c(-Inf, Inf))
    )

    expect_identical(v$verdict, "settings_mismatch")
    expect_identical(v$settings_differ, "outcome_range")
    expect_identical(
      describe_settings_diff(v$stored_settings,
                             eval_settings(outcome_range = c(-Inf, Inf)),
                             v$settings_differ),
      "outcome_range = unset (this run: -Inf, Inf)"
    )

    ## A row with no settings record at all, the same
    v0 <- checkpoint_row_verdict(no_stamp, data_fp,
                                 eval_settings(outcome_range = c(-10, 10)))
    expect_identical(v0$verdict, "settings_mismatch")

  })

  it("resume as unverified under the default, as before", {

    v <- checkpoint_row_verdict(
      row, data_fp,
      eval_settings(cv_folds = 3L, grid_size = 2L, outcome_range = DEFAULT_OUTCOME_RANGE)
    )

    expect_identical(v$verdict, "keep")
    expect_false(v$settings_verified)

  })

  it("a row that records a range is compared as any setting is", {

    stamped <- stamp_eval_settings(
      row, eval_settings(cv_folds = 3L, grid_size = 2L, outcome_range = c(-Inf, Inf))
    )

    keep <- checkpoint_row_verdict(
      stamped, data_fp,
      eval_settings(cv_folds = 3L, grid_size = 2L, outcome_range = c(-Inf, Inf))
    )
    expect_identical(keep$verdict, "keep")
    expect_true(keep$settings_verified)

    moved <- checkpoint_row_verdict(
      stamped, data_fp,
      eval_settings(cv_folds = 3L, grid_size = 2L, outcome_range = c(-40, 0))
    )
    expect_identical(moved$verdict, "settings_mismatch")

  })

})

describe("check_evaluated_outcome_range()", {

  ## An evaluation's results rows, as stamped before and after #76
  results_with <- function(stamps) {
    tibble::tibble(config_id = paste0("c", seq_along(stamps)), settings = stamps)
  }

  x_with <- function(results) list(evaluation = list(results = results))

  it("passes rows that record this range, and unrecorded rows under the default", {

    signed <- results_with(list(eval_settings(outcome_range = c(-Inf, Inf))))
    expect_null(check_evaluated_outcome_range(x_with(signed), c(-Inf, Inf)))

    old <- results_with(list(eval_settings(seed = 1L)))
    expect_null(check_evaluated_outcome_range(x_with(old), DEFAULT_OUTCOME_RANGE))

    ## Evaluations from before the settings stamp carry no settings column
    bare <- tibble::tibble(config_id = "c1")
    expect_null(check_evaluated_outcome_range(x_with(bare), DEFAULT_OUTCOME_RANGE))

  })

  it("refuses a recorded range that differs, and unrecorded rows under another range", {

    signed <- results_with(list(eval_settings(outcome_range = c(-Inf, Inf))))
    expect_error(check_evaluated_outcome_range(x_with(signed), DEFAULT_OUTCOME_RANGE),
                 class = "horizons_input_error")

    old <- results_with(list(eval_settings(seed = 1L)))
    err <- expect_error(check_evaluated_outcome_range(x_with(old), c(-Inf, Inf)),
                        class = "horizons_input_error")
    expect_match(flat_message(err), "unrecorded", fixed = TRUE)

  })

})

describe("validate_horizons_fit() - a malformed outcome range", {

  ## A fitted object from the non-negative fixture, cold-started and cheap
  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L

  fitted <- suppressWarnings(
    fit(obj, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L)
  )

  it("is not read, and not fatal, when there is no bound to check", {

    no_bound <- fitted
    no_bound$models["response_bound"] <- list(NULL)
    no_bound$config$outcome_range <- c(5, 1)

    expect_no_error(validate_horizons_fit(no_bound))

  })

  it("is collected with the other findings when there is a bound", {

    broken <- fitted
    broken$config$outcome_range <- c(5, 1)
    broken$models$n_models      <- 99L

    err <- expect_error(suppressMessages(utils::capture.output(validate_horizons_fit(broken))),
                        class = "horizons_validation_error")

    expect_match(flat_message(err), "config$outcome_range", fixed = TRUE)
    expect_match(flat_message(err), "n_models", fixed = TRUE)

  })

})

describe("infinite outcome values", {

  it("count as breaches whatever the range", {

    b <- outcome_range_breach(c(1, Inf, NA), DEFAULT_OUTCOME_RANGE)
    expect_identical(b$n_infinite, 1L)
    expect_identical(b$n_below + b$n_above, 0L)

    expect_false(is.null(outcome_range_breach(c(-5, -Inf), c(-Inf, Inf))))
    expect_null(outcome_range_breach(c(-5, NA, 3), c(-Inf, Inf)))

  })

  it("are refused by configure() before the bound could become Inf", {

    hd <- make_range_hd(value = function(v) { v[2] <- Inf; v })

    err <- expect_error(configure_small(hd), class = "horizons_input_error")
    expect_match(flat_message(err), "outcome_range")

  })

})

describe("the remedy text", {

  it("points at the property's physical bounds, not the data's", {

    obj <- make_eval_object(n = 60, n_configs = 1)
    obj$data$analysis$SOC <- to_d13c(obj$data$analysis$SOC)

    err <- expect_error(check_outcome_range(obj, verb = "evaluate"),
                        class = "horizons_input_error")

    expect_match(flat_message(err), "physical bounds", fixed = TRUE)
    expect_no_match(flat_message(err), "contain the data", fixed = TRUE)

  })

})
