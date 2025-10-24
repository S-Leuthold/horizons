test_that("back_transform_predictions handles basic transformations", {
  x <- c(0, 1, 2, NA)

  # none
  expect_equal(horizons::back_transform_predictions(x, "none"), x)

  # log
  logx <- c(0, log(2), log(3), NA)
  expect_equal(horizons::back_transform_predictions(logx, "log"), exp(logx))

  # sqrt: negative inputs are set to 0 before squaring (when warn=TRUE)
  sqx <- c(-1, 0, 2, NA)
  out <- horizons::back_transform_predictions(sqx, "sqrt")
  expect_equal(out, c(0, 0, 4, NA))

  # unknown transformation returns input unchanged (and may emit info)
  expect_equal(horizons::back_transform_predictions(x, "foobar"), x)
})

test_that("back_transform_predictions handles edge cases", {
  # NULL and empty return unchanged
  expect_null(horizons::back_transform_predictions(NULL, "none"))
  expect_equal(horizons::back_transform_predictions(numeric(0), "log"), numeric(0))
})

test_that("back_transform_predictions emits warnings for extreme inputs", {
  expect_message(
    horizons::back_transform_predictions(c(10, 60), "log"),
    "Very large values detected",
    class = "cliMessage"
  )

  expect_message(
    horizons::back_transform_predictions(c(-5, 4), "sqrt"),
    "Negative values detected",
    class = "cliMessage"
  )
})

test_that("needs_back_transformation returns correct flags", {
  expect_false(horizons:::needs_back_transformation(NULL))
  expect_false(horizons:::needs_back_transformation(NA))
  expect_false(horizons:::needs_back_transformation("none"))
  expect_false(horizons:::needs_back_transformation("notrans"))
  expect_false(horizons:::needs_back_transformation(""))

  expect_true(horizons:::needs_back_transformation("log"))
  expect_true(horizons:::needs_back_transformation("sqrt"))
})

test_that("compute_original_scale_metrics returns metrics tibble", {
  skip_if_not_installed("yardstick")
  skip_if_not_installed("tibble")
  skip_if_not_installed("tidyr")

  truth <- c(1, 2, 3, 4, NA)
  estimate <- c(1.1, 1.9, 3.2, 3.8, 10)

  res <- horizons:::compute_original_scale_metrics(truth, estimate)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c(".metric", ".estimator", ".estimate") %in% names(res)))
})

test_that("compute_original_scale_metrics handles insufficient data", {
  skip_if_not_installed("yardstick")
  skip_if_not_installed("tibble")
  skip_if_not_installed("tidyr")

  truth <- c(1, NA, 2)
  estimate <- c(1.1, 2.0, NA)

  expect_message(
    res <- horizons:::compute_original_scale_metrics(truth, estimate),
    "Insufficient data for metric calculation",
    class = "cliMessage"
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
})

test_that("back_transform_last_fit back-transforms predictions", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("tune")

  dummy_last_fit <- tibble::tibble(.predictions = list(tibble::tibble(.pred = c(0, log(4)))))

  transformed <- with_mocked_bindings(
    horizons:::back_transform_last_fit(dummy_last_fit, "log"),
    collect_predictions = function(x) tibble::tibble(.pred = c(0, log(4))),
    .package = "tune"
  )

  expect_equal(transformed$.predictions[[1]]$.pred, c(1, 4))
})

test_that("back_transform_cv_predictions iterates across folds", {
  skip_if_not_installed("tibble")

  cv_results <- tibble::tibble(
    .predictions = list(
      tibble::tibble(.pred = c(0, log(2))),
      tibble::tibble(.pred = c(log(3)))
    )
  )

  transformed <- horizons:::back_transform_cv_predictions(cv_results, "log")
  expect_equal(transformed$.predictions[[1]]$.pred, c(1, 2))
  expect_equal(transformed$.predictions[[2]]$.pred, c(3))
})

test_that("back_transform_cv_predictions returns input when predictions missing", {
  skip_if_not_installed("tibble")

  cv_results <- tibble::tibble(
    metrics = list(tibble::tibble(.metric = "rmse", .estimate = 0.1))
  )

  unchanged <- horizons:::back_transform_cv_predictions(cv_results, "log")
  expect_identical(unchanged, cv_results)
})

test_that("get_original_scale_predictions applies back transformation", {
  skip_if_not_installed("tibble")

  new_data <- tibble::tibble(x = c(1, 2))
  preds <- with_mocked_bindings(
    horizons:::get_original_scale_predictions(list(), new_data, transformation = "log", warn = FALSE),
    predict = function(object, new_data, ...) tibble::tibble(.pred = c(log(2), log(3))),
    .package = "horizons"
  )

  expect_equal(preds, c(2, 3))
})
