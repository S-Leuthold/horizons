#' Back-transform and metrics fallback tests for evaluate_configuration()

library(testthat)
library(horizons)

make_cfg_row_tx <- function() {
  data.frame(
    model = "plsr",
    transformation = "log",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = I(list(NULL)),
    stringsAsFactors = FALSE
  )[1, , drop = FALSE]
}

make_split_tx <- function(n = 30) {
  d <- create_eval_test_data(n_samples = n)
  rsample::initial_split(d, prop = 0.8)
}

test_that("back-transformation is applied when transformation != 'none'", {
  cfg <- make_cfg_row_tx()
  input <- create_eval_test_data(n_samples = 40)
  split <- make_split_tx(40)

  dummy_wflow <- list()
  dummy_param_set <- list()
  dummy_grid_results <- list(tag = "grid")
  dummy_final_wflow <- list()
  dummy_last_fit <- list()

  result <- testthat::with_mocked_bindings(
    build_recipe = function(...) structure(list(), class = "recipe"),
    compute_original_scale_metrics = function(truth, estimate, metrics) {
      tibble::tibble(
        .metric   = c("rrmse", "rmse", "rsq", "mae", "rpd", "ccc"),
        .estimate = c(20, 1.2, 0.6, 0.8, 1.8, 0.7)
      )
    },
    back_transform_predictions = function(predictions, transformation, warn = FALSE) {
      rep(123, length(predictions))
    },
    {
      res <- testthat::with_mocked_bindings(
        workflow = function() dummy_wflow,
        add_recipe = function(w, r) dummy_wflow,
        add_model = function(w, m) dummy_wflow,
        extract_parameter_set_dials = function(w) dummy_param_set,
        .package = "workflows",
        {
          res2 <- testthat::with_mocked_bindings(
            prep = function(...) list(prepped = TRUE),
            bake = function(prepped, new_data = NULL) input[1:10, ],
            .package = "recipes",
            {
              res3 <- testthat::with_mocked_bindings(
                finalize = function(param_set, eval_data) dummy_param_set,
                .package = "dials",
                {
                  res4 <- testthat::with_mocked_bindings(
                    tune_grid = function(...) dummy_grid_results,
                    collect_metrics = function(x) tibble::tibble(.metric = "rrmse", mean = 10),
                    select_best = function(...) tibble::tibble(num_comp = 2),
                    finalize_workflow = function(...) dummy_final_wflow,
                    last_fit = function(...) dummy_last_fit,
                    collect_predictions = function(final_fit) {
                      tibble::tibble(.pred = rnorm(8, 3, 0.1), Response = rnorm(8, 3, 0.1))
                    },
                    .package = "tune",
                    {
                      horizons:::evaluate_configuration(
                        config_row    = cfg,
                        input_data    = input,
                        data_split    = split,
                        config_id     = "cfg_btx",
                        variable      = "Response",
                        grid_size     = 2,
                        bayesian_iter = 0,
                        cv_folds      = 2,
                        allow_par     = FALSE,
                        verbose       = FALSE
                      )
                    }
                  )
                  res3
                }
              )
              res3
            }
          )
          res2
        }
      )
      res
    },
    .package = "horizons"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$status, "success")
  expect_true(all(c("rsq", "rmse", "rrmse", "ccc", "rpd", "mae") %in% names(result)))
})

test_that("metric calculation failure falls back to NA metrics", {
  cfg <- make_cfg_row_tx()
  input <- create_eval_test_data(n_samples = 40)
  split <- make_split_tx(40)

  dummy_wflow <- list()
  dummy_param_set <- list()
  dummy_grid_results <- list(tag = "grid")
  dummy_final_wflow <- list()
  dummy_last_fit <- list()

  result <- testthat::with_mocked_bindings(
    build_recipe = function(...) structure(list(), class = "recipe"),
    compute_original_scale_metrics = function(...) stop("metrics boom"),
    back_transform_predictions = function(predictions, transformation, warn = FALSE) predictions,
    {
      res <- testthat::with_mocked_bindings(
        workflow = function() dummy_wflow,
        add_recipe = function(w, r) dummy_wflow,
        add_model = function(w, m) dummy_wflow,
        extract_parameter_set_dials = function(w) dummy_param_set,
        .package = "workflows",
        {
          res2 <- testthat::with_mocked_bindings(
            prep = function(...) list(prepped = TRUE),
            bake = function(prepped, new_data = NULL) input[1:10, ],
            .package = "recipes",
            {
              res3 <- testthat::with_mocked_bindings(
                finalize = function(param_set, eval_data) dummy_param_set,
                .package = "dials",
                {
                  res4 <- testthat::with_mocked_bindings(
                    tune_grid = function(...) dummy_grid_results,
                    collect_metrics = function(x) tibble::tibble(.metric = "rrmse", mean = 10),
                    select_best = function(...) tibble::tibble(num_comp = 2),
                    finalize_workflow = function(...) dummy_final_wflow,
                    last_fit = function(...) dummy_last_fit,
                    collect_predictions = function(final_fit) {
                      tibble::tibble(.pred = rnorm(8, 3, 0.1), Response = rnorm(8, 3, 0.1))
                    },
                    .package = "tune",
                    {
                      horizons:::evaluate_configuration(
                        config_row    = cfg,
                        input_data    = input,
                        data_split    = split,
                        config_id     = "cfg_mx_fail",
                        variable      = "Response",
                        grid_size     = 2,
                        bayesian_iter = 0,
                        cv_folds      = 2,
                        allow_par     = FALSE,
                        verbose       = FALSE
                      )
                    }
                  )
                  res3
                }
              )
              res3
            }
          )
          res2
        }
      )
      res
    },
    .package = "horizons"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, "success")
  expect_true(all(is.na(result[, c("rrmse", "rmse", "rsq", "mae", "rpd", "ccc")])))
})

