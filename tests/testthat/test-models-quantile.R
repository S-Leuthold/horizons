test_that("define_quantile_specification() creates valid ranger quantile spec", {

  spec <- define_quantile_specification()

  # Check class
  expect_s3_class(spec, "rand_forest")

  # Check mode
  expect_equal(spec$mode, "regression")

  # Check engine
  expect_equal(spec$engine, "ranger")

  # Check quantreg is enabled
  expect_true(spec$eng_args$quantreg)
  expect_true(spec$eng_args$keep.inbag)

  # Check tunable parameters exist
  expect_true("tune" %in% class(spec$args$mtry))
  expect_true("tune" %in% class(spec$args$min_n))
})

test_that("quantile spec can be used in workflow", {

  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  ## Create simple test data -----------------------------------------------------

  set.seed(123)
  test_data <- tibble::tibble(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  ## Build workflow --------------------------------------------------------------

  spec <- define_quantile_specification()

  # Finalize hyperparameters to avoid tuning (use public API, not internal assignment)
  spec <- parsnip::set_args(spec, mtry = 2, min_n = 5)

  recipe <- recipes::recipe(y ~ ., data = test_data)

  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_recipe(recipe)

  # Check workflow is valid
  expect_s3_class(wf, "workflow")

  ## Fit workflow ----------------------------------------------------------------

  fitted_wf <- parsnip::fit(wf, data = test_data)

  expect_s3_class(fitted_wf, "workflow")
  expect_true(workflows::is_trained_workflow(fitted_wf))

  ## Predict should work (point predictions) ------------------------------------

  preds <- predict(fitted_wf, new_data = test_data)

  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), 100)
  expect_true(".pred" %in% names(preds))
  expect_true(all(is.numeric(preds$.pred)))

  ## Quantile prediction requires custom function (not yet implemented) ---------

  # NOTE: ranger quantile prediction requires extracting the fitted model
  # and calling predict() with type = "quantiles"
  # This will be implemented in predict_quantiles() wrapper function

  skip("Quantile prediction wrapper not yet implemented")
})
