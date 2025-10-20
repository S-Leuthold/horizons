library(testthat)
library(horizons)
library(recipes)

test_that("step_select_boruta retains mocked selections and bakes reduced data", {
  data <- tibble::tibble(
    Sample_ID = c("S1", "S2", "S3"),
    Response  = c(10, 12, 11),
    `600`     = c(0.1, 0.2, 0.3),
    `602`     = c(0.4, 0.5, 0.6),
    `604`     = c(0.7, 0.8, 0.9)
  )

  recipe <- recipes::recipe(Response ~ ., data = data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_boruta(recipes::all_predictors(), outcome = "Response")

  cluster_stub <- list(
    reduced_mat  = matrix(
      c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
      ncol = 2,
      dimnames = list(NULL, c("cluster_A", "cluster_B"))
    ),
    cluster_map  = list(cluster_A = c("600", "602"), cluster_B = "604"),
    selected_vars = c("cluster_A", "cluster_B")
  )

  boruta_object <- structure(list(), class = "mock_boruta")

  prepped <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        with_mocked_bindings(
          recipes::prep(recipe, training = data, retain = TRUE),
          Boruta = function(...) boruta_object,
          getSelectedAttributes = function(...) c("cluster_A"),
          .package = "Boruta"
        ),
        ranger = function(...) stop("ranger should not run in mock", call. = FALSE),
        .package = "ranger"
      ),
      cluster_spectral_predictors = function(...) cluster_stub,
      .package = "horizons"
    ),
    cli_alert_info   = function(...) invisible(NULL),
    cli_alert_warning = function(...) invisible(NULL),
    cli_abort        = function(message, ...) stop(message, call. = FALSE),
    cli_text         = function(...) invisible(NULL),
    .package = "cli"
  )

  step <- prepped$steps[[1]]

  expect_true(step$trained)
  expect_equal(step$selected_vars, c("600", "602"))

  baked <- recipes::bake(prepped, new_data = data)

  expect_true(all(c("600", "602") %in% names(baked)))
  expect_false("604" %in% names(baked))
  expect_equal(nrow(baked), nrow(data))
})

test_that("step_select_boruta falls back to original predictors when nothing selected", {
  data <- tibble::tibble(
    Sample_ID = c("S1", "S2"),
    Response  = c(9, 11),
    `600`     = c(0.2, 0.3),
    `602`     = c(0.3, 0.4),
    `604`     = c(0.4, 0.5)
  )

  recipe <- recipes::recipe(Response ~ ., data = data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_boruta(recipes::all_predictors(), outcome = "Response")

  cluster_stub <- list(
    reduced_mat   = matrix(
      c(0.1, 0.2, 0.3, 0.4),
      ncol = 2,
      dimnames = list(NULL, c("cluster_A", "cluster_B"))
    ),
    cluster_map   = list(cluster_A = character(), cluster_B = character()),
    selected_vars = c("cluster_A", "cluster_B")
  )

  boruta_object <- structure(list(), class = "mock_boruta")
  warning_called <- FALSE

  prepped <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        with_mocked_bindings(
          recipes::prep(recipe, training = data, retain = TRUE),
          Boruta = function(...) boruta_object,
          getSelectedAttributes = function(...) character(),
          .package = "Boruta"
        ),
        ranger = function(...) stop("ranger should not run in mock", call. = FALSE),
        .package = "ranger"
      ),
      cluster_spectral_predictors = function(...) cluster_stub,
      .package = "horizons"
    ),
    cli_alert_info    = function(...) invisible(NULL),
    cli_alert_warning = function(...) { warning_called <<- TRUE; invisible(NULL) },
    cli_abort         = function(message, ...) stop(message, call. = FALSE),
    cli_text          = function(...) invisible(NULL),
    .package = "cli"
  )

  step <- prepped$steps[[1]]

  expect_true(step$trained)
  expect_setequal(step$selected_vars, c("600", "602", "604"))
  expect_true(warning_called)

  baked <- recipes::bake(prepped, new_data = data)

  expect_equal(sort(names(baked)), sort(names(data)))
})
