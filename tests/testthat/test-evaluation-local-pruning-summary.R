#' Summary/Counts tests for evaluate_models_local()

library(testthat)
library(horizons)

test_that("evaluate_models_local shows counts and top list", {
  config <- data.frame(
    config_id = c("c1", "c2", "c3", "c4"),
    model = c("plsr", "plsr", "plsr", "plsr"),
    transformation = c("none", "none", "none", "none"),
    preprocessing = c("raw", "raw", "raw", "raw"),
    feature_selection = c("none", "none", "none", "none"),
    covariates = I(list(NULL, NULL, NULL, NULL)),
    stringsAsFactors = FALSE
  )
  data <- create_eval_test_data(n_samples = 40)
  od <- tempfile("eval_counts_"); dir.create(od, recursive = TRUE)

  mk_res <- function(id, status, rrmse = 10, rsq = 0.8) tibble::tibble(
    workflow_id = id, model = "plsr", transformation = "none", preprocessing = "raw",
    feature_selection = "none", covariates = "", status = status, rmse = 1,
    rsq = rsq, rrmse = rrmse, ccc = 0.8, rpd = 2, mae = 0.5, best_params = list(list(num_comp = 2)),
    grid_seconds = 0.1, bayes_seconds = 0, total_seconds = 0.2
  )

  expect_output(
    testthat::with_mocked_bindings(
      evaluate_configuration = function(config_row, ...) {
        switch(
          as.character(config_row$config_id),
          c1 = mk_res("c1", "success", 10, 0.85),
          c2 = mk_res("c2", "pruned",  50, 0.10),
          c3 = mk_res("c3", "failed",  NA,  NA),
          c4 = mk_res("c4", "success", 12, 0.80)
        )
      },
      {
        evaluate_models_local(
          config = config,
          input_data = data,
          variable = "Response",
          output_dir = od,
          grid_size = 2,
          bayesian_iter = 0,
          cv_folds = 2,
          allow_par = FALSE,
          verbose = TRUE
        )
      },
      .package = "horizons"
    ),
    regexp = "Success|Pruned|Failed|Top 5 Models",
    fixed = FALSE
  )
})

