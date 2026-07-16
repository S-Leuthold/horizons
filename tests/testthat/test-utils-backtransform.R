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

  it("clamps negative sqrt predictions even when warn = FALSE", {

    ## Regression guard: the clamp is correctness, not a warning. Production
    ## callers pass warn = FALSE; a negative sqrt-scale value must still be
    ## floored to 0 (not squared into a wrong positive, e.g. -0.5 -> 0.25).
    result <- back_transform_predictions(c(1, -0.5, 3), "sqrt", warn = FALSE)

    expect_equal(result[2], 0)        # clamped, NOT 0.25
    expect_equal(result, c(1, 0, 9))  # 1^2, clamp, 3^2

  })

  it("is an exact round-trip: inverse(forward(y)) == y for every transform", {

    ## Property test that would have caught the original exp(x) vs exp(x)-1
    ## bug. Forward transforms mirror the recipe steps:
    ##   log   -> log(y + 1)     sqrt  -> sqrt(y)     log10 -> log10(y + 1)
    y <- c(0.1, 1, 5, 42, 100)

    expect_equal(back_transform_predictions(log(y + 1),   "log"),   y, tolerance = 1e-10)
    expect_equal(back_transform_predictions(sqrt(y),      "sqrt"),  y, tolerance = 1e-10)
    expect_equal(back_transform_predictions(log10(y + 1), "log10"), y, tolerance = 1e-10)
    expect_equal(back_transform_predictions(y,            "none"),  y, tolerance = 1e-10)

  })

  it("winsorizes log-scale blow-ups to upper_bound with an informative warning", {

    ## The 257 g/kg escape: an unconstrained log-scale prediction inflates
    ## through exp() to a physically impossible value. With a bound supplied,
    ## the output is clamped to exactly the bound and the warning names the
    ## count and the pre-clamp max.
    preds <- c(1, 2, 7)  # exp(7) - 1 = 1095.6

    expect_warning(
      result <- back_transform_predictions(preds, "log", warn = FALSE,
                                           upper_bound = 100),
      "winsorized"
    )

    expect_equal(result[3], 100)
    expect_equal(result[1:2], exp(c(1, 2)) - 1, tolerance = 1e-10)

    ## Warning content: count and pre-clamp max
    w <- tryCatch(
      back_transform_predictions(preds, "log", warn = FALSE, upper_bound = 100),
      warning = function(w) conditionMessage(w)
    )
    expect_match(w, "1 prediction")
    expect_match(w, "1095.6")

  })

  it("applies the bound uniformly across all transforms", {

    expect_warning(r_none  <- back_transform_predictions(c(1, 500),  "none",  upper_bound = 100), "winsorized")
    expect_warning(r_sqrt  <- back_transform_predictions(c(1, 30),   "sqrt",  upper_bound = 100), "winsorized")
    expect_warning(r_log10 <- back_transform_predictions(c(1, 4),    "log10", upper_bound = 100), "winsorized")

    expect_equal(r_none[2],  100)
    expect_equal(r_sqrt[2],  100)   # 30^2 = 900 -> clamped
    expect_equal(r_log10[2], 100)   # 10^4 - 1 = 9999 -> clamped

  })

  it("upper_bound = NULL preserves current behavior exactly (regression)", {

    preds <- c(1, 2, 7)

    for (trans in c("none", "log", "sqrt", "log10")) {

      expect_identical(
        back_transform_predictions(preds, trans, warn = FALSE),
        back_transform_predictions(preds, trans, warn = FALSE, upper_bound = NULL)
      )

    }

    expect_silent(back_transform_predictions(preds, "log", warn = FALSE))

  })

  it("round-trip stays exact when nothing exceeds a generous bound", {

    y <- c(0.1, 1, 5, 42, 100)

    expect_equal(
      back_transform_predictions(log(y + 1), "log", upper_bound = 1e6),
      y, tolerance = 1e-10
    )

  })

  it("clamp warning fires even with warn = FALSE (guardrail is ungated)", {

    ## warn gates the edge-case messages; the winsorization warning is the
    ## guardrail itself and must be visible to production callers.
    expect_warning(
      back_transform_predictions(c(1, 7), "log", warn = FALSE, upper_bound = 100),
      "winsorized"
    )

  })

  it("passes NA values through unclamped when a bound is supplied", {

    result <- suppressWarnings(
      back_transform_predictions(c(1, NA, 7), "log", warn = FALSE,
                                 upper_bound = 100)
    )

    expect_true(is.na(result[2]))
    expect_equal(result[3], 100)

  })

  it("aborts on invalid upper_bound values", {

    preds <- c(1, 2)

    expect_error(back_transform_predictions(preds, "log", upper_bound = -5))
    expect_error(back_transform_predictions(preds, "log", upper_bound = Inf))
    expect_error(back_transform_predictions(preds, "log", upper_bound = c(1, 2)))
    expect_error(back_transform_predictions(preds, "log", upper_bound = "100"))

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
