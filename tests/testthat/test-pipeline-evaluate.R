## ---------------------------------------------------------------------------
## Tests: evaluate()
## ---------------------------------------------------------------------------

## Helper: build a minimal horizons_data object ready for evaluate()
make_eval_object <- function(n = 40, n_wn = 10, n_configs = 2,
                             covariates = NULL,
                             add_validation = TRUE) {

  set.seed(42)

  ## Spectral data
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Optionally add covariates
  if (!is.null(covariates)) {

    for (cov in covariates) {
      df[[cov]] <- runif(n, 0, 100)
      roles <- rbind(roles, tibble::tibble(variable = cov, role = "covariate"))
    }

  }

  ## Build configs
  models <- c("rf", "cubist")
  configs <- tibble::tibble(
    config_id         = paste0("cfg_", sprintf("%03d", seq_len(n_configs))),
    model             = models[seq_len(n_configs)],
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = NA_character_
  )

  ## Build horizons_data-like structure
  obj <- list(

    data = list(
      analysis     = df,
      role_map     = roles,
      n_rows       = nrow(df),
      n_predictors = n_wn,
      n_covariates = 0L,
      n_responses  = 1L
    ),

    provenance = list(
      spectra_source = "test",
      spectra_type   = "mir",
      schema_version = 1L
    ),

    config = list(
      configs   = configs,
      n_configs = n_configs,
      tuning    = list(
        cv_folds      = 3L,
        grid_size     = 2L,
        bayesian_iter = 0L
      )
    ),

    validation = list(
      passed    = if (add_validation) TRUE else NULL,
      checks    = NULL,
      timestamp = if (add_validation) Sys.time() else NULL,
      outliers  = list(
        spectral_ids   = NULL,
        response_ids   = NULL,
        removed_ids    = NULL,
        removal_detail = NULL,
        removed        = FALSE
      )
    ),

    evaluation = list(
      results     = NULL,
      best_config = NULL,
      rank_metric = NULL,
      backend     = NULL,
      runtime     = NULL,
      timestamp   = NULL
    ),

    models   = list(workflows = NULL, n_models = NULL,
                    uq = list(enabled = FALSE)),
    ensemble = list(stack = NULL),
    artifacts = list(cache_dir = NULL)

  )

  class(obj) <- c("horizons_data", "list")
  obj

}

## Expected columns in evaluation$results
EXPECTED_EVAL_COLS <- c(
  "config_id", "status", "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
  "best_params", "error_message", "warnings", "runtime_secs"
)

## =========================================================================
## Gate checks
## =========================================================================

describe("evaluate() - gate checks", {

  it("aborts when config$configs is NULL", {

    obj <- make_eval_object()
    obj$config$configs <- NULL

    expect_error(evaluate(obj, verbose = FALSE), "No configurations found")

  })

  it("aborts when config$configs has zero rows", {

    obj <- make_eval_object()
    obj$config$configs <- obj$config$configs[0, ]

    expect_error(evaluate(obj, verbose = FALSE), "No configurations found")

  })

  it("aborts when outcome column has no non-NA values", {

    obj <- make_eval_object()
    outcome <- obj$data$role_map$variable[obj$data$role_map$role == "outcome"]
    obj$data$analysis[[outcome]] <- NA_real_

    expect_error(evaluate(obj, verbose = FALSE), "outcome.*NA")

  })

  it("aborts when sample size is too small for CV", {

    obj <- make_eval_object(n = 4)

    expect_error(evaluate(obj, verbose = FALSE), "sample size")

  })

  it("aborts with invalid metric name", {

    obj <- make_eval_object()

    expect_error(evaluate(obj, metric = "accuracy", verbose = FALSE), "metric")

  })

})

## =========================================================================
## Success path
## =========================================================================

describe("evaluate() - success path", {

  obj <- make_eval_object(n = 60, n_configs = 2)

  result <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L))

  it("returns a horizons_eval object", {

    expect_s3_class(result, "horizons_eval")
    expect_s3_class(result, "horizons_data")

  })

  it("has evaluation$results with one row per config", {

    expect_equal(nrow(result$evaluation$results), 2)

  })

  it("has all expected columns in results", {

    expect_true(all(EXPECTED_EVAL_COLS %in% names(result$evaluation$results)))

  })

  it("sets best_config to a valid config_id", {

    expect_true(result$evaluation$best_config %in%
                  result$config$configs$config_id)

  })

  it("stores rank_metric", {

    expect_equal(result$evaluation$rank_metric, "rpd")

  })

  it("stores the train/test split", {

    expect_s3_class(result$evaluation$split, "rsplit")

  })

  it("records n_train and n_test", {

    expect_true(result$evaluation$n_train > 0)
    expect_true(result$evaluation$n_test > 0)
    expect_equal(result$evaluation$n_train + result$evaluation$n_test,
                 nrow(obj$data$analysis))

  })

  it("records positive runtime", {

    expect_true(result$evaluation$runtime_secs > 0)

  })

  it("records a timestamp", {

    expect_s3_class(result$evaluation$timestamp, "POSIXct")

  })

  it("has non-NA metrics for successful configs", {

    success_rows <- result$evaluation$results$status == "success"
    expect_true(any(success_rows))

    for (m in c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")) {
      expect_false(any(is.na(result$evaluation$results[[m]][success_rows])),
                   info = paste("NA found in", m))
    }

  })

})

## =========================================================================
## Metric ranking
## =========================================================================

describe("evaluate() - metric ranking", {

  it("selects best config by specified metric", {

    obj <- make_eval_object(n_configs = 2)
    result <- suppressWarnings(evaluate(obj, metric = "rsq", verbose = FALSE, seed = 42L))

    expect_equal(result$evaluation$rank_metric, "rsq")

    ## Best config should have highest rsq among successes
    successes <- result$evaluation$results %>%
      dplyr::filter(status == "success")
    best_row <- successes[which.max(successes$rsq), ]

    expect_equal(result$evaluation$best_config, best_row$config_id)

  })

})

## =========================================================================
## All configs fail
## =========================================================================

describe("evaluate() - all configs fail", {

  it("aborts when every config fails", {

    obj <- make_eval_object(n_configs = 2)
    obj$config$configs$model <- c("nope_1", "nope_2")

    expect_error(
      suppressWarnings(evaluate(obj, verbose = FALSE)),
      "All configurations failed"
    )

  })

})

## =========================================================================
## NA outcome handling
## =========================================================================

describe("evaluate() - NA outcome rows", {

  it("drops rows with NA outcome and still succeeds", {

    obj <- make_eval_object(n = 40)
    outcome <- obj$data$role_map$variable[obj$data$role_map$role == "outcome"]

    ## Set 5 rows to NA
    obj$data$analysis[[outcome]][1:5] <- NA_real_

    result <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L))

    ## Should succeed with remaining rows
    expect_s3_class(result, "horizons_eval")
    expect_equal(result$evaluation$n_train + result$evaluation$n_test, 35)

  })

})

## =========================================================================
## Checkpointing
## =========================================================================

describe("evaluate() - checkpointing", {

  it("writes checkpoint file when output_dir is provided", {

    obj <- make_eval_object(n_configs = 2)
    tmpdir <- tempdir()
    checkpoint_path <- file.path(tmpdir, "eval_checkpoint.rds")
    if (file.exists(checkpoint_path)) file.remove(checkpoint_path)

    result <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    ## Checkpoint file should exist after completion
    expect_true(file.exists(checkpoint_path))

    ## Clean up
    file.remove(checkpoint_path)

  })

  it("resumes from checkpoint on re-run", {

    obj <- make_eval_object(n_configs = 2)
    tmpdir <- tempdir()
    checkpoint_path <- file.path(tmpdir, "eval_checkpoint.rds")
    if (file.exists(checkpoint_path)) file.remove(checkpoint_path)

    ## First run
    result1 <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    ## Second run should load from checkpoint (same results)
    result2 <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    expect_equal(result1$evaluation$results$config_id,
                 result2$evaluation$results$config_id)
    expect_equal(result1$evaluation$best_config,
                 result2$evaluation$best_config)

    ## Clean up
    file.remove(checkpoint_path)

  })

})

## =========================================================================
## Pruning passthrough
## =========================================================================

describe("evaluate() - pruning", {

  it("passes prune settings to evaluate_single_config", {

    obj <- make_eval_object(n_configs = 1)

    ## Set a very high prune threshold — RPD must be above 9999
    result <- suppressWarnings(evaluate(obj, prune = TRUE, prune_threshold = 9999,
                                        verbose = FALSE, seed = 42L))

    ## The single config should be pruned
    expect_equal(result$evaluation$results$status, "pruned")

  })

})

## =========================================================================
## Seed reproducibility
## =========================================================================

describe("evaluate() - reproducibility", {

  it("produces identical results with the same seed", {

    obj <- make_eval_object(n_configs = 1)

    r1 <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 123L))
    r2 <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 123L))

    expect_equal(r1$evaluation$results$rmse, r2$evaluation$results$rmse)
    expect_equal(r1$evaluation$results$rsq, r2$evaluation$results$rsq)

  })

})
