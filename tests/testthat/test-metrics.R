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

  })

  it("returns NA with fewer than 2 observations", {

    expect_true(is.na(ccc_vec(1, 1)))

  })

  it("returns NA when SD of either vector is zero", {

    truth    <- c(5, 5, 5, 5)
    estimate <- c(4, 5, 6, 7)

    expect_true(is.na(ccc_vec(truth, estimate)))

  })

  it("handles NA removal when na_rm = TRUE", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, 2.2, 2.8, 4.1, NA)

    result <- ccc_vec(truth, estimate, na_rm = TRUE)
    expect_true(is.finite(result))

  })

})

describe("yardstick metric_set integration", {

  it("all three metrics work in a metric_set pipeline", {

    test_df <- tibble::tibble(
      truth    = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      estimate = c(1.2, 1.8, 3.1, 4.3, 4.8, 6.2, 7.1, 7.9, 9.3, 9.7)
    )

    metrics <- yardstick::metric_set(rpd, rrmse, ccc)
    result  <- metrics(test_df, truth = truth, estimate = estimate)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 3)
    expect_true(all(c("rpd", "rrmse", "ccc") %in% result$.metric))

    ## RPD should be positive
    rpd_val <- result$.estimate[result$.metric == "rpd"]
    expect_true(rpd_val > 0)

    ## RRMSE should be positive
    rrmse_val <- result$.estimate[result$.metric == "rrmse"]
    expect_true(rrmse_val > 0)

    ## CCC should be between 0 and 1 for this well-correlated data
    ccc_val <- result$.estimate[result$.metric == "ccc"]
    expect_true(ccc_val > 0 && ccc_val <= 1)

  })

  it("metrics work alongside yardstick built-in metrics", {

    test_df <- tibble::tibble(
      truth    = c(1, 2, 3, 4, 5),
      estimate = c(1.1, 2.2, 2.8, 4.1, 4.9)
    )

    metrics <- yardstick::metric_set(
      rpd, rrmse, ccc,
      yardstick::rmse, yardstick::rsq, yardstick::mae
    )

    result <- metrics(test_df, truth = truth, estimate = estimate)

    expect_equal(nrow(result), 6)

  })

  it("rpd direction is maximize", {
    expect_equal(attr(rpd, "direction"), "maximize")
  })

  it("rrmse direction is minimize", {
    expect_equal(attr(rrmse, "direction"), "minimize")
  })

  it("ccc direction is maximize", {
    expect_equal(attr(ccc, "direction"), "maximize")
  })

})
