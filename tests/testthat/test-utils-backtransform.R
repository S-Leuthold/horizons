## ---------------------------------------------------------------------------
## Tests: Back-transform utilities
## ---------------------------------------------------------------------------

describe("back_transform_predictions()", {

  it("returns predictions unchanged for 'none' transformation", {

    preds <- c(1, 2, 3, 4, 5)
    result <- back_transform_predictions(preds, "none")
    expect_equal(result, preds)

  })

  it("applies exp(x) - 1 for log transformation (offset = 1 inverse)", {

    ## step_log(offset = 1) computes log(x + 1)
    ## Correct inverse: exp(x) - 1
    ## This is the bug fix from legacy code which used exp(x)

    original <- c(0, 1, 2, 5, 10)
    transformed <- log(original + 1)  # forward: log(x + 1)

    result <- back_transform_predictions(transformed, "log")

    expect_equal(result, original, tolerance = 1e-10)

  })

  it("applies x^2 for sqrt transformation", {

    original <- c(0, 1, 4, 9, 16)
    transformed <- sqrt(original)

    result <- back_transform_predictions(transformed, "sqrt")

    expect_equal(result, original, tolerance = 1e-10)

  })

  it("applies 10^x - 1 for log10 transformation (offset = 1 inverse)", {

    original    <- c(0, 1, 9, 99, 999)
    transformed <- log10(original + 1)

    result <- back_transform_predictions(transformed, "log10")

    expect_equal(result, original, tolerance = 1e-10)

  })

  it("handles NA values gracefully", {

    preds <- c(1, NA, 3, NA, 5)

    result_log  <- back_transform_predictions(preds, "log")
    result_sqrt <- back_transform_predictions(preds, "sqrt")

    expect_equal(sum(is.na(result_log)), 2)
    expect_equal(sum(is.na(result_sqrt)), 2)
    expect_true(is.finite(result_log[1]))
    expect_true(is.finite(result_sqrt[1]))

  })

  it("handles empty vectors", {

    result <- back_transform_predictions(numeric(0), "log")
    expect_length(result, 0)

  })

  it("handles NULL input", {

    result <- back_transform_predictions(NULL, "log")
    expect_null(result)

  })

  it("is case-insensitive", {

    preds <- c(0.5, 1.0, 1.5)

    expect_equal(
      back_transform_predictions(preds, "LOG"),
      back_transform_predictions(preds, "log")
    )

    expect_equal(
      back_transform_predictions(preds, "Sqrt"),
      back_transform_predictions(preds, "sqrt")
    )

  })

  it("warns for very large log-scale predictions", {

    preds <- c(1, 2, 60)  # 60 on log scale → huge number

    expect_warning(
      back_transform_predictions(preds, "log", warn = TRUE),
      "large"
    )

  })

  it("clamps negative sqrt predictions to 0 with warning", {

    preds <- c(1, -0.5, 3)

    expect_warning(
      result <- back_transform_predictions(preds, "sqrt", warn = TRUE),
      "Negative"
    )

    expect_equal(result[2], 0)

  })

  it("suppresses warnings when warn = FALSE", {

    preds_log  <- c(1, 2, 60)
    preds_sqrt <- c(1, -0.5, 3)

    expect_silent(back_transform_predictions(preds_log, "log", warn = FALSE))
    expect_silent(back_transform_predictions(preds_sqrt, "sqrt", warn = FALSE))

  })

})

describe("needs_back_transformation()", {

  it("returns TRUE for log, sqrt, log10", {

    expect_true(needs_back_transformation("log"))
    expect_true(needs_back_transformation("sqrt"))
    expect_true(needs_back_transformation("log10"))

  })

  it("returns FALSE for none", {

    expect_false(needs_back_transformation("none"))

  })

  it("returns FALSE for NULL and NA", {

    expect_false(needs_back_transformation(NULL))
    expect_false(needs_back_transformation(NA))

  })

  it("is case-insensitive", {

    expect_true(needs_back_transformation("LOG"))
    expect_true(needs_back_transformation("Sqrt"))

  })

})

describe("compute_original_scale_metrics()", {

  it("computes all six metrics for valid input", {

    truth    <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    estimate <- c(1.2, 1.8, 3.1, 4.3, 4.8, 6.2, 7.1, 7.9, 9.3, 9.7)

    result <- compute_original_scale_metrics(truth, estimate)

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae") %in%
                      result$.metric))

  })

  it("drops NA pairs before computing", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, NA, 2.8, 4.1, 4.9)

    result <- compute_original_scale_metrics(truth, estimate)

    ## Should compute on the 3 complete pairs: (1,1.1), (4,4.1), (5,4.9)
    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)

  })

  it("returns empty tibble for insufficient data", {

    truth    <- c(NA)
    estimate <- c(NA)

    result <- compute_original_scale_metrics(truth, estimate)

    expect_equal(nrow(result), 0)

  })

})
