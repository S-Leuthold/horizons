#' Behavior Tests for evaluate_configuration() via mocks (fast path)

library(testthat)
library(horizons)

make_valid_config_row <- function() {
  data.frame(
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = I(list(NULL)),
    stringsAsFactors = FALSE
  )[1, , drop = FALSE]
}

make_valid_split <- function(n = 30) {
  data <- create_eval_test_data(n_samples = n)
  rsample::initial_split(data, prop = 0.8)
}

test_that("evaluate_configuration success path with mocks (no heavy tuning)", {
  config_row <- make_valid_config_row()
  input_data <- create_eval_test_data(n_samples = 40)
  split <- make_valid_split(40)

  # Dummy objects
  dummy_wflow <- list()
  dummy_param_set <- list()
  dummy_grid_results <- list(tag = "grid")
  dummy_final_results <- dummy_grid_results
  dummy_best_params <- list(num_comp = 3)
  dummy_final_wflow <- list()
  dummy_last_fit <- list()

  # Define mocks across packages
  with_mocked_bindings(
    build_recipe = function(...) structure(list(), class = "recipe"),
    compute_original_scale_metrics = function(truth, estimate, metrics) {
      tibble::tibble(
        .metric = c("rrmse", "rmse", "rsq", "mae", "rpd", "ccc"),
        .estimate = c(15, 0.8, 0.75, 0.6, 2.2, 0.85)
      )
    },
    {
      res <- with_mocked_bindings(
        workflow = function() dummy_wflow,
        add_recipe = function(w, r) dummy_wflow,
        add_model = function(w, m) dummy_wflow,
        extract_parameter_set_dials = function(w) dummy_param_set,
        .package = "workflows",
        {
          res2 <- with_mocked_bindings(
            prep = function(...) list(prepped = TRUE),
            bake = function(prepped, new_data = NULL) input_data[1:10, ],
            .package = "recipes",
            {
              res3 <- with_mocked_bindings(
                finalize = function(param_set, eval_data) dummy_param_set,
                .package = "dials",
                {
                  res4 <- with_mocked_bindings(
                    tune_grid = function(object, resamples, grid, metrics, param_info, control) dummy_grid_results,
                    collect_metrics = function(x) {
                      if (identical(x, dummy_grid_results)) {
                        tibble::tibble(.metric = "rrmse", mean = 10)
                      } else {
                        tibble::tibble(.metric = c("rmse", "rsq", "mae", "rrmse", "rpd", "ccc"),
                                        mean = c(0.8, 0.75, 0.6, 15, 2.2, 0.85))
                      }
                    },
                    select_best = function(results, metric) tibble::tibble(num_comp = 3),
                    finalize_workflow = function(wflow, params) dummy_final_wflow,
                    last_fit = function(final_wflow, split, metrics) dummy_last_fit,
                    collect_predictions = function(final_fit) {
                      tibble::tibble(.pred = rnorm(8, 10, 1), Response = rnorm(8, 10, 1))
                    },
                    .package = "tune",
                    {
                      out <- horizons:::evaluate_configuration(
                        config_row = config_row,
                        input_data = input_data,
                        data_split = split,
                        config_id = "cfg_success",
                        variable = "Response",
                        grid_size = 2,
                        bayesian_iter = 0,
                        cv_folds = 2,
                        allow_par = FALSE,
                        prune_models = FALSE,
                        verbose = FALSE
                      )
                      out
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
  ) -> result

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$status, "success")
  expect_true(all(c("rsq", "rmse", "rrmse", "ccc", "rpd", "mae") %in% names(result)))
  expect_true(is.numeric(result$total_seconds))
})

test_that("evaluate_configuration prunes when grid RRMSE worse than threshold", {
  config_row <- make_valid_config_row()
  input_data <- create_eval_test_data(n_samples = 40)
  split <- make_valid_split(40)

  dummy_wflow <- list()
  dummy_param_set <- list()
  dummy_grid_results <- list(tag = "grid")

  with_mocked_bindings(
    build_recipe = function(...) structure(list(), class = "recipe"),
    compute_original_scale_metrics = function(truth, estimate, metrics) {
      tibble::tibble(
        .metric = c("rrmse", "rmse", "rsq", "mae", "rpd", "ccc"),
        .estimate = c(50, 3, 0.1, 2, 0.5, 0.2)
      )
    },
    {
      res <- with_mocked_bindings(
        workflow = function() dummy_wflow,
        add_recipe = function(w, r) dummy_wflow,
        add_model = function(w, m) dummy_wflow,
        extract_parameter_set_dials = function(w) dummy_param_set,
        .package = "workflows",
        {
          res2 <- with_mocked_bindings(
            prep = function(...) list(prepped = TRUE),
            bake = function(prepped, new_data = NULL) input_data[1:10, ],
            .package = "recipes",
            {
              res3 <- with_mocked_bindings(
                finalize = function(param_set, eval_data) dummy_param_set,
                .package = "dials",
                {
                  res4 <- with_mocked_bindings(
                    tune_grid = function(...) dummy_grid_results,
                    collect_metrics = function(x) {
                      if (identical(x, dummy_grid_results)) {
                        # Bad performance so it fails threshold and sets skip_bayesian = TRUE
                        tibble::tibble(.metric = "rrmse", mean = 1e6)
                      } else {
                        tibble::tibble(.metric = c("rmse", "rsq", "mae", "rrmse", "rpd", "ccc"),
                                        mean = c(3, 0.1, 2, 50, 0.5, 0.2))
                      }
                    },
                    select_best = function(...) tibble::tibble(num_comp = 2),
                    finalize_workflow = function(wflow, params) list(),
                    last_fit = function(final_wflow, split, metrics) list(),
                    collect_predictions = function(final_fit) {
                      tibble::tibble(.pred = rnorm(8, 10, 1), Response = rnorm(8, 10, 1))
                    },
                    .package = "tune",
                    {
                      out <- horizons:::evaluate_configuration(
                        config_row = config_row,
                        input_data = input_data,
                        data_split = split,
                        config_id = "cfg_pruned",
                        variable = "Response",
                        grid_size = 2,
                        bayesian_iter = 5,
                        cv_folds = 2,
                        allow_par = FALSE,
                        prune_models = TRUE,
                        prune_threshold = 0.9,
                        verbose = FALSE
                      )
                      out
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
  ) -> result

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, "pruned")
})

