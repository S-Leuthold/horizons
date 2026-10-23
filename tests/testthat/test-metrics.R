## ---------------------------------------------------------------------------
## Tests: Custom yardstick metrics (RPD, RRMSE, CCC)
## ---------------------------------------------------------------------------

## Known-answer test data:
## truth    = c(1, 2, 3, 4, 5)
## estimate = c(1.1, 2.2, 2.8, 4.1, 4.9)
##
## By hand:
##   residuals = c(-0.1, -0.2, 0.2, -0.1, 0.1)
##   RMSE      = sqrt(mean(c(0.01, 0.04, 0.04, 0.01, 0.01))) = sqrt(0.022) ≈ 0.14832
##   MAE       = mean(c(0.1, 0.2, 0.2, 0.1, 0.1)) = 0.14
##   SD(truth) = sd(c(1,2,3,4,5)) = sqrt(2.5) ≈ 1.58114
##   RPD       = SD / RMSE ≈ 1.58114 / 0.14832 ≈ 10.66
##   mean(truth) = 3
##   RRMSE     = 100 * RMSE / mean = 100 * 0.14832 / 3 ≈ 4.944

describe("rpd_vec()", {

  it("computes RPD = SD(truth) / RMSE for known values", {

    truth    <- c(1, 2, 3, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, 4.9)

    result <- rpd_vec(truth, estimate)

    rmse_expected <- sqrt(mean((truth - estimate)^2))
    sd_expected   <- sd(truth)
    rpd_expected  <- sd_expected / rmse_expected

    expect_equal(result, rpd_expected, tolerance = 1e-10)

  })

  it("returns Inf for perfect predictions (RMSE = 0)", {

    truth    <- c(1, 2, 3, 4, 5)
    estimate <- c(1, 2, 3, 4, 5)

    expect_equal(rpd_vec(truth, estimate), Inf)

  })

  it("handles NA removal when na_rm = TRUE", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, NA)

    result <- rpd_vec(truth, estimate, na_rm = TRUE)
    expect_true(is.finite(result))
    expect_true(result > 0)

  })

  it("returns NA when na_rm = FALSE and NAs present", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, 4.9)

    result <- rpd_vec(truth, estimate, na_rm = FALSE)
    expect_true(is.na(result))

  })

  it("uses SD of truth, not estimate", {

    ## Constant truth → SD = 0, so RPD should be 0 (or NA)
    truth    <- c(5, 5, 5, 5, 5)
    estimate <- c(4, 5, 6, 5, 4)

    result <- rpd_vec(truth, estimate)
    expect_equal(result, 0)

  })

})

describe("rrmse_vec()", {

  it("computes RRMSE = 100 * RMSE / mean(truth)", {

    truth    <- c(1, 2, 3, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, 4.9)

    result <- rrmse_vec(truth, estimate)

    rmse_expected  <- sqrt(mean((truth - estimate)^2))
    rrmse_expected <- 100 * rmse_expected / mean(truth)

    expect_equal(result, rrmse_expected, tolerance = 1e-10)

  })

  it("returns 0 for perfect predictions", {

    truth    <- c(1, 2, 3, 4, 5)
    estimate <- c(1, 2, 3, 4, 5)

    expect_equal(rrmse_vec(truth, estimate), 0)

  })

  it("handles NA removal when na_rm = TRUE", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, NA)

    result <- rrmse_vec(truth, estimate, na_rm = TRUE)
    expect_true(is.finite(result))
    expect_true(result >= 0)

  })

  it("returns NA when na_rm = FALSE and NAs present", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, 4.9)

    expect_identical(rrmse_vec(truth, estimate, na_rm = FALSE), NA_real_)

  })

  it("returns NA when mean(truth) is zero, rather than dividing by it", {

    truth    <- c(-2, -1, 0, 1, 2)
    estimate <- c(-1.8, -1.1, 0.2, 0.9, 2.1)

    expect_identical(rrmse_vec(truth, estimate), NA_real_)

  })

})

describe("ccc_vec()", {

  it("returns 1 for perfect agreement", {

    truth    <- c(1, 2, 3, 4, 5)
    estimate <- c(1, 2, 3, 4, 5)

    expect_equal(ccc_vec(truth, estimate), 1.0)

  })

  it("returns a value between -1 and 1 for imperfect agreement", {

    truth    <- c(1, 2, 3, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, 4.9)

    result <- ccc_vec(truth, estimate)
    expect_true(result > 0 && result <= 1)

    ## Precision times accuracy: Pearson r is below 1 here, so the bias
    ## correction alone would be another number
    expect_lt(cor(truth, estimate), 1)
    expect_equal(result, yardstick::ccc_vec(truth, estimate))

  })

  it("is less than Pearson r when systematic bias exists", {

    ## Shift all predictions by a constant (bias)
    truth    <- c(1, 2, 3, 4, 5)
    estimate <- truth + 2  # constant bias of 2

    ccc_val <- ccc_vec(truth, estimate)
    r_val   <- cor(truth, estimate)

    ## Perfect correlation but biased → CCC < r
    expect_equal(r_val, 1.0)
    expect_true(ccc_val < r_val)

    ## By how much: Lin's coefficient, as yardstick computes it
    expect_equal(ccc_val, yardstick::ccc_vec(truth, estimate))

  })

  it("returns NA with fewer than 2 observations", {

    expect_true(is.na(ccc_vec(1, 1)))

  })

  it("returns NA, without a warning, when SD of either vector is zero", {

    constant <- c(5, 5, 5, 5)
    varying  <- c(4, 5, 6, 7)

    ## Undefined, so not left to cor(), which warns on a zero SD
    expect_no_warning(expect_identical(ccc_vec(constant, varying), NA_real_))
    expect_no_warning(expect_identical(ccc_vec(varying, constant), NA_real_))

  })

  it("handles NA removal when na_rm = TRUE", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, NA)

    result <- ccc_vec(truth, estimate, na_rm = TRUE)
    expect_true(is.finite(result))

  })

  it("returns NA when na_rm = FALSE and NAs present, as rpd_vec() and rrmse_vec() do (#176)", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, 4.9)

    expect_identical(ccc_vec(truth, estimate, na_rm = FALSE), NA_real_)
    expect_identical(ccc_vec(estimate, truth, na_rm = FALSE), NA_real_)

  })

})

## ===========================================================================
## tuning_metric_set()
## ===========================================================================

describe("tuning_metric_set()", {

  all_metric_names <- c("rmse", "rrmse", "rsq", "mae", "rpd", "ccc")

  ## Original-scale truth, with predictions that live on each transformed scale
  set.seed(4901)
  truth <- stats::rgamma(60, shape = 3)
  noise <- stats::rnorm(60, sd = 0.05)

  transformed_estimates <- list(
    log   = log(truth + 1) + noise,
    log10 = log10(truth + 1) + noise,
    sqrt  = sqrt(truth) + noise
  )

  ## One prediction below zero on the sqrt scale, which back-transforms to 0
  transformed_estimates$sqrt[1] <- -0.1

  directions <- function(ms) {
    vapply(attributes(ms)$metrics, function(m) attr(m, "direction"), character(1))
  }

  it("clamps the estimate to outcome_range for transformation = 'none' (#104)", {

    ## Estimates below and above the range are scored as they are served,
    ## clamped, as every transformed set and every reported metric is
    est <- c(-0.5, 12, (truth + noise)[-(1:2)])
    df  <- tibble::tibble(truth = truth, estimate = est)

    got  <- tuning_metric_set("none", outcome_range = c(0, 10))(df, truth = truth, estimate = estimate)
    want <- compute_original_scale_metrics(truth = truth, estimate = pmin(pmax(est, 0), 10))

    expect_equal(got$.metric, all_metric_names)

    for (m in all_metric_names) {
      expect_equal(got$.estimate[got$.metric == m], want$.estimate[want$.metric == m],
                   tolerance = 1e-12, info = m)
    }

    ## The clamp binds here: scored unclamped, the rmse is another number
    expect_gt(abs(got$.estimate[got$.metric == "rmse"] - yardstick::rmse_vec(truth, est)), 0.01)

  })

  it("scores every metric on the original scale for each transformation", {

    ## compute_original_scale_metrics() scores through the plain rpd, rrmse
    ## and ccc metric objects, the wrapped set through their _vec functions,
    ## so this also checks each object's data-frame method against its _vec.
    for (trans in names(transformed_estimates)) {

      est <- transformed_estimates[[trans]]
      df  <- tibble::tibble(truth = truth, estimate = est)

      got  <- tuning_metric_set(trans)(df, truth = truth, estimate = estimate)
      want <- compute_original_scale_metrics(
        truth    = truth,
        estimate = back_transform_predictions(est, trans)
      )

      expect_equal(got$.metric, all_metric_names, info = trans)

      for (m in all_metric_names) {
        expect_equal(
          got$.estimate[got$.metric == m],
          want$.estimate[want$.metric == m],
          tolerance = 1e-12,
          info      = paste(trans, m)
        )
      }

    }

  })

  it("preserves metric names and directions so select_best()/show_best() keep working", {

    ## The exported rrmse, rpd and ccc objects, which
    ## compute_original_scale_metrics() scores through, carry the same
    ## directions as the wrapped set
    expect_identical(vapply(list(rrmse, rpd, ccc), attr, character(1), "direction"),
                     c("minimize", "maximize", "maximize"))

    for (trans in c("none", "log", "log10", "sqrt")) {

      ms <- tuning_metric_set(trans)

      expect_equal(names(attributes(ms)$metrics), all_metric_names, info = trans)
      expect_equal(
        unname(directions(ms)),
        c("minimize", "minimize", "maximize", "minimize", "maximize", "maximize"),
        info = trans
      )

    }

  })

  it("honours the metrics argument and its order", {

    ms  <- tuning_metric_set("log", metrics = c("rpd", "rmse"))
    df  <- tibble::tibble(truth = truth, estimate = transformed_estimates$log)
    got <- ms(df, truth = truth, estimate = estimate)

    expect_equal(got$.metric, c("rpd", "rmse"))

  })

  it("is not a cross-scale comparison (the #49 regression)", {

    ## A near-perfect log-scale prediction. Scored cross-scale it looks poor,
    ## because original-scale truth is being compared to log-scale estimates.
    df <- tibble::tibble(truth = truth, estimate = transformed_estimates$log)

    naive <- tuning_metric_set("none")(df, truth = truth, estimate = estimate)
    fixed <- tuning_metric_set("log")(df,  truth = truth, estimate = estimate)

    naive_rmse <- naive$.estimate[naive$.metric == "rmse"]
    fixed_rmse <- fixed$.estimate[fixed$.metric == "rmse"]
    naive_rpd  <- naive$.estimate[naive$.metric == "rpd"]
    fixed_rpd  <- fixed$.estimate[fixed$.metric == "rpd"]

    expect_lt(fixed_rmse, naive_rmse / 5)
    expect_gt(fixed_rpd, 5)
    expect_lt(naive_rpd, 2)

  })

  it("carries none of its caller's frame, so serializing it sends no training data", {

    ## As the single-config fits call it: the transformation and the range
    ## are the caller's variables, beside its data
    build_beside_data <- function() {
      training_data  <- matrix(0, nrow = 1000, ncol = 1000)
      transformation <- "log"
      outcome_range  <- c(0, Inf)
      list(set       = tuning_metric_set(transformation, outcome_range = outcome_range),
           data_size = length(serialize(training_data, NULL)))
    }

    built <- build_beside_data()

    expect_gt(built$data_size, 8e6)
    expect_lt(length(serialize(built$set, NULL)), built$data_size / 10)

  })

  ## An unknown transformation is refused at every verb's door, before a
  ## metric set is built (config_findings(), test-class-core.R; #234). The
  ## metric names are the two callers' constants, and the range is read
  ## through outcome_range_setting(), which the door validators guarantee, so
  ## the metric set checks neither.

})
