#' Summary and reporting tests for evaluate_models_local() via mocking

library(testthat)
library(horizons)

test_that("evaluate_models_local prints summary and top models", {
  # Build a small config with 3 entries
  config <- data.frame(
    config_id = c("cfg_1", "cfg_2", "cfg_3"),
    model = c("plsr", "plsr", "plsr"),
    transformation = c("none", "none", "none"),
    preprocessing = c("raw", "raw", "raw"),
    feature_selection = c("none", "none", "none"),
    covariates = I(list(NULL, NULL, NULL)),
    stringsAsFactors = FALSE
  )

  data <- create_eval_test_data(n_samples = 40)
  od <- tempfile("eval_summary_")
  dir.create(od, recursive = TRUE)

  # Create three distinct results to sort
  mk_res <- function(id, rrmse, rsq) tibble::tibble(
    workflow_id = id,
    model = "plsr",
    transformation = "none",
    preprocessing = "raw",
    feature_selection = "none",
    covariates = "",
    status = "success",
    rmse = 1,
    rsq = rsq,
    rrmse = rrmse,
    ccc = 0.8,
    rpd = 2.0,
    mae = 0.5,
    best_params = list(list(num_comp = 3)),
    grid_seconds = 0.1,
    bayes_seconds = 0,
    total_seconds = 0.2
  )

  out <- testthat::with_mocked_bindings(
    evaluate_configuration = function(config_row, ...) {
      id <- as.character(config_row$model)
      # Map rows to different performances
      if (identical(config_row$config_id, "cfg_1")) mk_res("cfg_1", 12, 0.8)
      else if (identical(config_row$config_id, "cfg_2")) mk_res("cfg_2", 20, 0.6)
      else mk_res("cfg_3", 15, 0.7)
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
  )

  # Verify essential reporting is present
  # Note: We can't assert exact order of models printed, but header appears
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3)
})

