#' Behavior Tests for evaluate_models_local(): resume, pruning, failures
#'
#' Focus: exercise high-value branches without modifying package code.

library(testthat)
library(horizons)

make_template_bundle <- function(seed = 123) {
  config <- create_eval_test_config()
  data   <- create_eval_test_data(n_samples = 40, seed = seed)
  split  <- rsample::initial_split(data, prop = 0.8)

  result <- suppressMessages(
    horizons:::evaluate_configuration(
      config_row    = config,
      input_data    = data,
      data_split    = split,
      config_id     = config$config_id[1],
      variable      = "Response",
      grid_size     = 1,
      bayesian_iter = 0,
      cv_folds      = 2,
      allow_par     = FALSE,
      prune_models  = FALSE,
      verbose       = FALSE
    )
  )

  list(config = config, data = data, result = result)
}

## ---------------------------------------------------------------------------
## Resume behavior: skips existing results and proceeds without recomputation
## ---------------------------------------------------------------------------

test_that("evaluate_models_local resumes and skips completed configs", {
  bundle <- make_template_bundle()
  existing_result <- bundle$result

  output_dir <- withr::local_tempdir(pattern = "eval_resume_")
  fs::dir_create(fs::path(output_dir, "results"), recurse = TRUE)

  result_path <- fs::path(output_dir, "results", paste0(existing_result$workflow_id, ".qs"))
  qs::qsave(existing_result, result_path)

  result <- suppressMessages(
    with_mocked_bindings(
      evaluate_configuration = function(...) stop("evaluate_configuration should not run when resume=TRUE"),
      {
        evaluate_models_local(
          config        = bundle$config,
          input_data    = bundle$data,
          variable      = "Response",
          grid_size     = 1,
          bayesian_iter = 0,
          cv_folds      = 2,
          allow_par     = FALSE,
          prune_models  = FALSE,
          output_dir    = output_dir,
          resume        = TRUE,
          verbose       = FALSE
        )
      },
      .package = "horizons"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, existing_result$status)
  expect_equal(result$workflow_id, existing_result$workflow_id)
})

## ---------------------------------------------------------------------------
## Pruned path: mock evaluate_configuration to return status = "pruned"
## ---------------------------------------------------------------------------

test_that("evaluate_models_local handles pruned result status", {
  bundle <- make_template_bundle(321)
  pruned_result <- bundle$result
  pruned_result$status <- "pruned"
  pruned_result$rrmse  <- 999
  pruned_result$ccc    <- NA_real_
  pruned_result$rpd    <- NA_real_
  pruned_result$mae    <- NA_real_

  res <- with_mocked_bindings(
    evaluate_configuration = function(...) pruned_result,
    {
      evaluate_models_local(
        config        = bundle$config,
        input_data    = bundle$data,
        variable      = "Response",
        grid_size     = 1,
        bayesian_iter = 0,
        cv_folds      = 2,
        allow_par     = FALSE,
        prune_models  = TRUE,
        output_dir    = withr::local_tempdir(),
        verbose       = FALSE
      )
    },
    .package = "horizons"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$status, "pruned")
})

## ---------------------------------------------------------------------------
## Failure path: mock evaluate_configuration to return status = "failed"
## ---------------------------------------------------------------------------

test_that("evaluate_models_local records failure details from evaluate_configuration", {
  bundle <- make_template_bundle(456)
  failed_result <- bundle$result
  failed_result$status        <- "failed"
  failed_result$rrmse         <- NA_real_
  failed_result$rsq           <- NA_real_
  failed_result$ccc           <- NA_real_
  failed_result$rpd           <- NA_real_
  failed_result$mae           <- NA_real_
  failed_result$error_stage   <- "grid_tuning"
  failed_result$error_class   <- "SimulatedError"
  failed_result$error_message <- "Simulated failure at grid tuning"
  failed_result$total_seconds <- 0

  res <- with_mocked_bindings(
    evaluate_configuration = function(...) failed_result,
    {
      evaluate_models_local(
        config        = bundle$config,
        input_data    = bundle$data,
        variable      = "Response",
        grid_size     = 1,
        bayesian_iter = 0,
        cv_folds      = 2,
        allow_par     = FALSE,
        prune_models  = FALSE,
        output_dir    = withr::local_tempdir(),
        verbose       = FALSE
      )
    },
    .package = "horizons"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$status, "failed")
  expect_true("error_message" %in% names(res))
})
