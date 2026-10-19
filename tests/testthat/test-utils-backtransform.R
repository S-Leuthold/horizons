## ---------------------------------------------------------------------------
## Tests: Back-transform utilities
## ---------------------------------------------------------------------------

describe("back_transform_predictions()", {

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

  it("refuses an unknown transformation as a bug, since the doors refuse one first (#234)", {

    ## Serving it untransformed was the defect: predictions on the wrong scale
    expect_error(back_transform_predictions(c(1, 2, 3), "boxcox"),
                 "boxcox", class = "horizons_error_internal")

  })

  it("raises no warning on large or negative transformed values", {

    ## Large log-scale values are the response bound's to report, where
    ## predict() serves them; a negative sqrt-scale value is set to 0, which is
    ## correctness rather than a departure.
    expect_silent(back_transform_predictions(c(1, 2, 60), "log"))
    expect_silent(back_transform_predictions(c(1, 2, 60), "log10"))
    expect_silent(back_transform_predictions(c(1, -0.5, 3), "sqrt"))

  })

  it("clamps negative sqrt predictions to 0 rather than squaring them", {

    ## Regression guard: the clamp is correctness, not a warning; a negative
    ## sqrt-scale value must be floored to 0 (not squared into a wrong
    ## positive, e.g. -0.5 -> 0.25).
    result <- back_transform_predictions(c(1, -0.5, 3), "sqrt")

    expect_equal(result[2], 0)        # clamped, NOT 0.25
    expect_equal(result, c(1, 0, 9))  # 1^2, clamp, 3^2

  })

  it("floors negative original-scale output at zero for every transformation (#53)", {

    ## log / log10: a transformed-scale value below 0 back-transforms below 0
    ## (exp(-2) - 1 = -0.86). Deploy floors it; evaluation must score the same
    ## vector. "none": a model can predict a negative clay percent directly.
    expect_equal(back_transform_predictions(c(-2, 0, 1),   "log"),
                 c(0, 0, exp(1) - 1))
    expect_equal(back_transform_predictions(c(-2, 0, 1),   "log10"),
                 c(0, 0, 9))
    expect_equal(back_transform_predictions(c(-1, 0, 2.5), "none"),
                 c(0, 0, 2.5))

    ## NAs survive the floor
    expect_equal(back_transform_predictions(c(-1, NA), "none"), c(0, NA))

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

})

describe("apply_response_bound()", {

  ## predict() applies the bound to the original-scale predictions
  ## back_transform_predictions() returns, so these compose the two.

  it("applies after the floor, so both hold at once", {

    expect_warning(
      r <- apply_response_bound(back_transform_predictions(c(-3, 0.5, 10), "log"), 5),
      "winsorized"
    )

    expect_equal(r, c(0, exp(0.5) - 1, 5))

  })

  it("winsorizes log-scale blow-ups to the bound with an informative warning", {

    ## The 257 g/kg escape: an unconstrained log-scale prediction inflates
    ## through exp() to a physically impossible value. With a bound supplied,
    ## the output is clamped to exactly the bound and the warning names the
    ## count and the pre-clamp max.
    preds <- c(1, 2, 7)  # exp(7) - 1 = 1095.6

    expect_warning(
      result <- apply_response_bound(back_transform_predictions(preds, "log"), 100),
      "winsorized"
    )

    expect_equal(result[3], 100)
    expect_equal(result[1:2], exp(c(1, 2)) - 1, tolerance = 1e-10)

    ## Warning content: count and pre-clamp max
    w <- tryCatch(
      apply_response_bound(back_transform_predictions(preds, "log"), 100),
      warning = function(w) conditionMessage(w)
    )
    expect_match(w, "1 prediction")
    expect_match(w, "1095.6")

  })

  it("applies the bound uniformly across all transforms", {

    bound <- function(x, trans) apply_response_bound(back_transform_predictions(x, trans), 100)

    expect_warning(r_none  <- bound(c(1, 500), "none"),  "winsorized")
    expect_warning(r_sqrt  <- bound(c(1, 30),  "sqrt"),  "winsorized")
    expect_warning(r_log10 <- bound(c(1, 4),   "log10"), "winsorized")

    expect_equal(r_none[2],  100)
    expect_equal(r_sqrt[2],  100)   # 30^2 = 900 -> clamped
    expect_equal(r_log10[2], 100)   # 10^4 - 1 = 9999 -> clamped

  })

  it("round-trip stays exact when nothing exceeds a generous bound", {

    y <- c(0.1, 1, 5, 42, 100)

    expect_equal(
      apply_response_bound(back_transform_predictions(log(y + 1), "log"), 1e6),
      y, tolerance = 1e-10
    )

  })

  it("passes NA values through unclamped when a bound is supplied", {

    result <- suppressWarnings(
      apply_response_bound(back_transform_predictions(c(1, NA, 7), "log"), 100)
    )

    expect_true(is.na(result[2]))
    expect_equal(result[3], 100)

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

  it("returns FALSE for NULL", {

    expect_false(needs_back_transformation(NULL))

  })

  it("treats \"notrans\", \"na\", \"\" and NA as unknown, as back_transform_predictions() does", {

    ## "none" is the only spelling of no transformation. These are unknown
    ## values like any other, so both functions handle them as they handle
    ## "boxcox": never mistaken for none, and refused as a bug.
    preds <- c(1, 2, 3)

    for (trans in list("notrans", "NoTrans", "na", "NA", "", NA)) {

      label <- deparse(trans)

      expect_true(needs_back_transformation(trans), label = label)

      expect_error(back_transform_predictions(preds, trans),
                   class = "horizons_error_internal", label = label)

    }

  })

  it("is case-insensitive", {

    expect_true(needs_back_transformation("LOG"))
    expect_true(needs_back_transformation("Sqrt"))
    expect_false(needs_back_transformation("None"))

  })

})

describe("compute_original_scale_metrics()", {

  it("drops NA pairs before computing", {

    truth    <- c(1, 2, NA, 4, 5)
    estimate <- c(1.1, NA, 2.8, 4.1, 4.9)

    result <- compute_original_scale_metrics(truth, estimate)

    ## The same metrics as the 3 complete pairs alone: (1,1.1), (4,4.1), (5,4.9)
    expect_s3_class(result, "tbl_df")
    expect_setequal(result$.metric, c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))
    expect_equal(result, compute_original_scale_metrics(c(1, 4, 5), c(1.1, 4.1, 4.9)))
    expect_identical(unique(result$n), 3L)
    expect_false(anyNA(result$.estimate))

    ## Pairs are dropped before the count, so one complete pair is too few:
    ## six NA metrics, counted, and no warning
    expect_no_warning(short <- compute_original_scale_metrics(c(1, 2, NA), c(1.1, NA, 3)))
    expect_identical(short$.metric, result$.metric)
    expect_true(all(is.na(short$.estimate)))
    expect_identical(unique(short$n), 1L)

  })

  it("returns the six metrics as NA, with no warning, when no pair is complete", {

    expect_no_warning(result <- compute_original_scale_metrics(NA_real_, NA_real_))

    expect_identical(result$.metric, c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))
    expect_identical(result$.estimate, rep(NA_real_, 6))
    expect_identical(unique(result$n), 0L)

    ## The reason a configuration's row gives for them
    expect_identical(metrics_na_reason(result),
                     "The test-set metrics are NA: 0 test rows have both an observed value and a prediction, and they need two.")
    expect_null(metrics_na_reason(compute_original_scale_metrics(c(1, 4, 5), c(1.1, 4.1, 4.9))))

  })

})
