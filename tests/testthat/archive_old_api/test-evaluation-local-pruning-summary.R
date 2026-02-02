#' Summary/Counts tests for evaluate_models_local()

library(testthat)
library(horizons)

prepare_bundle <- function(seed = 987) {
  config <- data.frame(
    config_id         = c('c1', 'c2', 'c3', 'c4'),
    model             = 'plsr',
    transformation    = 'none',
    preprocessing     = c('raw', 'snv', 'raw', 'snv'),
    feature_selection = c('none', 'none', 'pca', 'pca'),
    covariates        = I(list(NULL, NULL, NULL, NULL)),
    stringsAsFactors  = FALSE
  )

  data <- create_eval_test_data(n_samples = 40, seed = seed)
  list(config = config, data = data)
}

make_result_row <- function(config_id, workflow_id, status, rrmse = 10, rsq = 0.8) {
  tibble::tibble(
    config_id              = config_id,
    workflow_id            = workflow_id,
    model                  = 'plsr',
    transformation         = 'none',
    preprocessing          = 'raw',
    feature_selection      = 'none',
    covariates             = '',
    covariate_interactions = FALSE,
    best_params            = list(list(num_comp = 2)),
    rsq                    = rsq,
    rmse                   = 1,
    rrmse                  = rrmse,
    rpd                    = 2,
    ccc                    = 0.8,
    mae                    = 0.5,
    grid_seconds           = 0.1,
    bayes_seconds          = 0,
    total_seconds          = 0.2,
    status                 = status,
    error_message          = NA_character_,
    error_stage            = NA_character_,
    error_class            = NA_character_,
    has_trace              = FALSE,
    n_warnings             = 0,
    warning_summary        = NA_character_
  )
}

status_sequence <- c('success', 'pruned', 'failed', 'success')
queue_index <- 0
mock_eval <- function(config_row, ...) {
  queue_index <<- queue_index + 1
  status <- status_sequence[queue_index]
  wf_id <- horizons:::clean_workflow_id(
    model                 = config_row$model,
    transformation        = config_row$transformation,
    preprocessing         = config_row$preprocessing,
    feature_selection     = config_row$feature_selection,
    covariates            = NULL,
    covariate_interactions = FALSE
  )
  make_result_row(config_row$config_id, wf_id, status,
                  rrmse = if (status == 'pruned') 50 else 10,
                  rsq   = if (status == 'failed') NA_real_ else 0.8)
}

test_that("evaluate_models_local aggregates status counts correctly", {
  bundle <- prepare_bundle()

  result <- testthat::with_mocked_bindings(
    evaluate_configuration = mock_eval,
    {
      evaluate_models_local(
        config        = bundle$config,
        input_data    = bundle$data,
        variable      = 'Response',
        output_dir    = withr::local_tempdir(),
        grid_size     = 1,
        bayesian_iter = 0,
        cv_folds      = 2,
        allow_par     = FALSE,
        prune_models  = FALSE,
        verbose       = FALSE
      )
    },
    .package = 'horizons'
  )

  expect_s3_class(result, 'tbl_df')
  expect_equal(sort(result$status), sort(c('success', 'success', 'pruned', 'failed')))
})
