library(testthat)
library(horizons)
library(recipes)

test_that("step_select_cars retains clustered wavenumbers with mocked PLS routine", {
  data <- tibble::tibble(
    Sample_ID = c("S1", "S2", "S3"),
    Response  = c(8, 10, 9),
    `600`     = c(0.1, 0.2, 0.3),
    `602`     = c(0.3, 0.4, 0.5),
    `604`     = c(0.5, 0.6, 0.7)
  )

  recipe <- recipes::recipe(Response ~ ., data = data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_cars(recipes::all_predictors(), outcome = "Response")

  cluster_stub <- list(
    reduced_mat   = matrix(
      c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
      ncol = 2,
      dimnames = list(NULL, c("cluster_A", "cluster_B"))
    ),
    cluster_map   = list(cluster_A = c("600", "602"), cluster_B = "604"),
    selected_vars = c("cluster_A", "cluster_B")
  )

  prepped <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        with_mocked_bindings(
          recipes::prep(recipe, training = data, retain = TRUE),
          plsr = function(...) structure(list(coef = c(cluster_A = 1.5, cluster_B = 0.5)), class = "mock_pls"),
          RMSEP = function(...) list(val = array(0.01, dim = c(1, 1, 1))),
          .package = "pls"
        ),
        cluster_spectral_predictors = function(...) cluster_stub,
        .package = "horizons"
      ),
      coef = function(object, ...) {
        matrix(object$coef, ncol = 1, dimnames = list(names(object$coef), NULL))
      },
      .package = "stats"
    ),
    cli_alert_warning = function(...) invisible(NULL),
    cli_abort         = function(message, ...) stop(message, call. = FALSE),
    cli_alert_info    = function(...) invisible(NULL),
    cli_text          = function(...) invisible(NULL),
    .package = "cli"
  )

  step <- prepped$steps[[1]]

  expect_true(step$trained)
  expect_setequal(step$selected_vars, c("600", "602", "604"))

  baked <- recipes::bake(prepped, new_data = data)

  expect_true(all(c("600", "602", "604") %in% names(baked)))
  expect_true(all(c("Sample_ID", "Response") %in% names(baked)))
})

test_that("step_select_cars retains all predictors when no wavenumbers survive selection", {
  data <- tibble::tibble(
    Sample_ID = c("S1", "S2"),
    Response  = c(7, 9),
    `600`     = c(0.1, 0.2),
    `602`     = c(0.2, 0.3),
    `604`     = c(0.3, 0.4)
  )

  recipe <- recipes::recipe(Response ~ ., data = data) %>%
    recipes::update_role(Sample_ID, new_role = "id") %>%
    step_select_cars(recipes::all_predictors(), outcome = "Response")

  cluster_stub <- list(
    reduced_mat   = matrix(
      c(0.1, 0.2, 0.3, 0.4),
      ncol = 2,
      dimnames = list(NULL, c("cluster_A", "cluster_B"))
    ),
    cluster_map   = list(cluster_A = character(), cluster_B = character()),
    selected_vars = c("cluster_A", "cluster_B")
  )

  warning_called <- FALSE

  prepped <- with_mocked_bindings(
    with_mocked_bindings(
      with_mocked_bindings(
        with_mocked_bindings(
          recipes::prep(recipe, training = data, retain = TRUE),
          plsr = function(...) structure(list(coef = c(cluster_A = 0, cluster_B = 0)), class = "mock_pls"),
          RMSEP = function(...) list(val = array(0.01, dim = c(1, 1, 1))),
          .package = "pls"
        ),
        cluster_spectral_predictors = function(...) cluster_stub,
        .package = "horizons"
      ),
      coef = function(object, ...) {
        matrix(object$coef, ncol = 1, dimnames = list(names(object$coef), NULL))
      },
      .package = "stats"
    ),
    cli_alert_warning = function(...) { warning_called <<- TRUE; invisible(NULL) },
    cli_abort         = function(message, ...) stop(message, call. = FALSE),
    cli_alert_info    = function(...) invisible(NULL),
    cli_text          = function(...) invisible(NULL),
    .package = "cli"
  )

  step <- prepped$steps[[1]]

  expect_true(step$trained)
  expect_true(warning_called)
  expect_setequal(step$selected_vars, c("600", "602", "604"))

  baked <- recipes::bake(prepped, new_data = data)
  expect_equal(sort(names(baked)), sort(names(data)))
})
