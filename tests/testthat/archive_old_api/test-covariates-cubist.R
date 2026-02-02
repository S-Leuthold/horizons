#' Tests for fit_cubist_model()
#'
#' Validates both failure and success paths using mocked tidymodels helpers.

library(testthat)
library(horizons)
library(tibble)

make_cluster_sample <- function(n = 6) {
  tibble(
    Dim.1 = seq_len(n),
    Dim.2 = seq_len(n) * 0.5,
    clay  = seq_len(n) * 2
  )
}

## -----------------------------------------------------------------------------
## Failure handling: grid search returns NULL
## -----------------------------------------------------------------------------

test_that("fit_cubist_model returns NULL when grid search fails", {
  train <- make_cluster_sample(8)
  val   <- make_cluster_sample(4)

  with_mocked_bindings(
    vfold_cv = function(data, ...) data,
    .package = "rsample",
    with_mocked_bindings(
      grid_space_filling = function(...) tibble(committees = 5L, neighbors = 1L, max_rules = 35L),
      neighbors          = function(...) list(),
      max_rules          = function(...) list(),
      .package = "dials",
      with_mocked_bindings(
        committees = function(...) list(),
        .package   = "rules",
        with_mocked_bindings(
          cubist_rules = function(...) list(),
          set_engine   = function(x, ...) x,
          set_mode     = function(x, ...) x,
          .package     = "parsnip",
          with_mocked_bindings(
            workflow   = function() list(),
            add_model  = function(wf, ...) wf,
            add_formula = function(wf, ...) wf,
            .package   = "workflows",
            with_mocked_bindings(
              control_grid = function(...) list(),
              tune_grid    = function(...) NULL,
              .package     = "tune",
              with_mocked_bindings(
                cli_text = function(...) invisible(NULL),
                .package = "cli",
                result <- horizons::fit_cubist_model(
                  train_data    = train,
                  val_data      = val,
                  covariate     = "clay",
                  verbose       = TRUE,
                  parallel      = FALSE,
                  bayesian_iter = 0
                )
              )
            )
          )
        )
      )
    )
  )

  expect_null(result)
})

## -----------------------------------------------------------------------------
## Success path with mocked tidymodels calls
## -----------------------------------------------------------------------------

test_that("fit_cubist_model returns workflow and metrics with mocked tidymodels stack", {
  train <- make_cluster_sample(12)
  val   <- make_cluster_sample(6)

  grid_res <- tibble(
    committees = 5,
    neighbors  = 1,
    max_rules  = 35,
    .metric    = "rmse",
    mean       = 0.4,
    std_err    = 0.01
  )

  with_mocked_bindings(
    vfold_cv = function(data, ...) data,
    .package = "rsample",
    with_mocked_bindings(
      grid_space_filling = function(...) grid_res,
      neighbors          = function(...) list(),
      max_rules          = function(...) list(),
      .package = "dials",
      with_mocked_bindings(
        committees = function(...) list(),
        .package   = "rules",
        with_mocked_bindings(
          cubist_rules = function(...) list(),
          set_engine   = function(x, ...) x,
          set_mode     = function(x, ...) x,
          fit          = function(object, data, ...) structure(list(data = data), class = "mock_workflow"),
          .package     = "parsnip",
          with_mocked_bindings(
            workflow    = function() list(),
            add_model   = function(wf, ...) wf,
            add_formula = function(wf, ...) wf,
            .package    = "workflows",
            with_mocked_bindings(
              control_grid    = function(...) list(),
              tune_grid       = function(...) grid_res,
              select_best     = function(res, metric) tibble(committees = 5, neighbors = 1, max_rules = 35),
              finalize_workflow = function(wf, params) list(finalized = TRUE, params = params),
              .package        = "tune",
              with_mocked_bindings(
                butcher = function(x, ...) x,
                .package = "butcher",
                with_mocked_bindings(
                  cli_text = function(...) invisible(NULL),
                  .package = "cli",
                  with_mocked_bindings(
                    predict = function(object, new_data, ...) tibble(.pred = new_data$Dim.1),
                    .package = "stats",
                    with_mocked_bindings(
                      rmse_vec = function(truth, estimate) sqrt(mean((truth - estimate)^2)),
                      mae_vec  = function(truth, estimate) mean(abs(truth - estimate)),
                      rsq_vec  = function(truth, estimate) stats::cor(truth, estimate)^2,
                      .package = "yardstick",
                      with_mocked_bindings(
                        cli_alert_success = function(...) invisible(NULL),
                        cli_progress_step = function(...) invisible(NULL),
                        .package = "cli",
                        with_mocked_bindings(
                          rpd_vec = function(truth, estimate, ...) 1.5,
                          ccc_vec = function(truth, estimate, ...) 0.9,
                          .package = "horizons",
                          result <- horizons::fit_cubist_model(
                            train_data    = train,
                            val_data      = val,
                            covariate     = "clay",
                            verbose       = TRUE,
                            parallel      = FALSE,
                            bayesian_iter = 0
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  expect_type(result, "list")
  expect_true("fitted_workflow" %in% names(result))
  expect_true("validation_metrics" %in% names(result))
  expect_s3_class(result$fitted_workflow, "mock_workflow")
  expect_s3_class(result$validation_metrics, "tbl_df")
})
