## ---------------------------------------------------------------------------
## Tests: evaluate() parallel execution
## ---------------------------------------------------------------------------

## Reuse make_eval_object from test-pipeline-evaluate.R (loaded by testthat)
## These tests are separated because they require skip_on_cran() and are
## inherently slower due to worker startup.


## =========================================================================
## Parameter validation
## =========================================================================

describe("evaluate() - workers parameter validation", {

  it("rejects non-integer workers", {

    obj <- make_eval_object(n_configs = 2)

    expect_error(evaluate(obj, workers = 1.5, verbose = FALSE),
                 "positive integer")
    expect_error(evaluate(obj, workers = -1, verbose = FALSE),
                 "positive integer")
    expect_error(evaluate(obj, workers = "two", verbose = FALSE),
                 "positive integer")

  })

  it("requires output_dir when outer > 1", {

    obj <- make_eval_object(n_configs = 2)

    ## workers = 10 with cv_folds = 3 → inner = 3, outer = 3, needs output_dir
    expect_error(
      evaluate(obj, workers = 10L, output_dir = NULL, verbose = FALSE),
      "output_dir"
    )

  })

  it("allows workers > 1 without output_dir when outer == 1", {

    obj <- make_eval_object(n_configs = 2)

    ## workers = 2 with cv_folds = 3 → inner = 2, outer = 1 → no output_dir needed
    result <- suppressWarnings(
      evaluate(obj, workers = 2L, verbose = FALSE, seed = 42L)
    )

    expect_s3_class(result, "horizons_eval")

  })

})


## =========================================================================
## Auto-split logic
## =========================================================================

describe("evaluate() - auto-split computation", {

  it("correctly splits workers across outer and inner", {

    ## We can test this indirectly through the manifest written during
    ## parallel runs, or directly by checking the tree output

    obj    <- make_eval_object(n_configs = 4)
    tmpdir <- tempfile("eval_par_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    skip_on_cran()

    ## workers = 9, cv_folds = 3 → inner = 3, outer = 3
    result <- suppressWarnings(
      evaluate(obj, workers = 9L, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
    )

    ## Check manifest was written with correct split
    manifest <- readRDS(file.path(tmpdir, "eval_manifest.rds"))
    expect_equal(manifest$inner, 3L)
    expect_equal(manifest$outer, 3L)
    expect_equal(manifest$workers, 9L)

  })

})


## =========================================================================
## Parallel smoke test
## =========================================================================

describe("evaluate() - parallel execution", {

  it("produces results with workers > cv_folds", {

    skip_on_cran()

    obj    <- make_eval_object(n_configs = 4)
    tmpdir <- tempfile("eval_par_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    result <- suppressWarnings(
      evaluate(obj, workers = 10L, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
    )

    expect_s3_class(result, "horizons_eval")
    expect_equal(nrow(result$evaluation$results), 4)

    ## All configs should have been evaluated
    expect_true(all(result$evaluation$results$config_id %in%
                      c("cfg_001", "cfg_002", "cfg_003", "cfg_004")))

    ## Per-config checkpoint files should exist
    checkpoint_dir <- file.path(tmpdir, "checkpoints")
    expect_true(dir.exists(checkpoint_dir))
    checkpoint_files <- list.files(checkpoint_dir, pattern = "\\.rds$")
    expect_equal(length(checkpoint_files), 4)

  })

  it("parallel results match sequential results", {

    skip_on_cran()

    obj <- make_eval_object(n_configs = 2)

    ## Sequential
    seq_result <- suppressWarnings(
      evaluate(obj, workers = 1L, verbose = FALSE, seed = 42L)
    )

    ## Parallel
    tmpdir <- tempfile("eval_par_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    par_result <- suppressWarnings(
      evaluate(obj, workers = 10L, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
    )

    ## Both runs should produce valid results with the same structure
    ## (RNG streams differ between sequential set.seed() and parallel
    ## L'Ecuyer-CMRG, so exact metric values may not match)
    expect_s3_class(par_result, "horizons_eval")

    ## Same config IDs evaluated
    expect_setequal(seq_result$evaluation$results$config_id,
                    par_result$evaluation$results$config_id)

    ## Best config should be one of the evaluated configs
    expect_true(par_result$evaluation$best_config %in%
                  par_result$evaluation$results$config_id)

    ## Same number of results
    expect_equal(nrow(seq_result$evaluation$results),
                 nrow(par_result$evaluation$results))

  })

})


## =========================================================================
## Cross-mode resume
## =========================================================================

describe("evaluate() - cross-mode checkpoint resume", {

  it("resumes parallel run from sequential checkpoints", {

    skip_on_cran()

    obj    <- make_eval_object(n_configs = 4)
    tmpdir <- tempfile("eval_xmode_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    ## Run sequentially first (creates both single-file + per-config)
    seq_result <- suppressWarnings(
      evaluate(obj, workers = 1L, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
    )

    ## Now run in parallel — should load all 4 from checkpoint
    par_result <- suppressWarnings(
      evaluate(obj, workers = 10L, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
    )

    expect_equal(seq_result$evaluation$best_config,
                 par_result$evaluation$best_config)
    expect_equal(nrow(par_result$evaluation$results), 4)

  })

})


## =========================================================================
## monitor_evaluate()
## =========================================================================

describe("monitor_evaluate()", {

  it("errors on missing directory", {

    expect_error(monitor_evaluate("/nonexistent/path"),
                 "not found")

  })

  it("errors on missing manifest", {

    tmpdir <- tempfile("eval_mon_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    expect_error(monitor_evaluate(tmpdir), "eval_manifest")

  })

  it("reads progress from checkpoint directory", {

    skip_on_cran()

    ## Create a mock checkpoint directory
    tmpdir <- tempfile("eval_mon_")
    dir.create(tmpdir)
    checkpoint_dir <- file.path(tmpdir, "checkpoints")
    dir.create(checkpoint_dir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    ## Write a manifest
    manifest <- list(
      n_total    = 10,
      n_pending  = 10,
      config_ids = paste0("cfg_", sprintf("%03d", 1:10)),
      start_time = Sys.time() - 3600,
      workers    = 10L,
      outer      = 2L,
      inner      = 5L,
      metric     = "rpd",
      cv_folds   = 5L
    )
    saveRDS(manifest, file.path(tmpdir, "eval_manifest.rds"))

    ## Write 3 mock checkpoint files
    for (i in 1:3) {

      row <- tibble::tibble(
        config_id     = paste0("cfg_", sprintf("%03d", i)),
        model         = "rf",
        status        = "success",
        rpd           = runif(1, 1, 3),
        rsq           = runif(1, 0.5, 0.9),
        rmse          = runif(1, 0.1, 0.5),
        runtime_secs  = runif(1, 10, 60)
      )
      saveRDS(row, file.path(checkpoint_dir, paste0(row$config_id, ".rds")))

    }

    ## monitor should report 3/10 complete
    stats <- suppressMessages(
      capture.output(result <- monitor_evaluate(tmpdir), type = "message")
    )
    output <- capture.output(monitor_evaluate(tmpdir))

    expect_equal(result$n_complete, 3)
    expect_equal(result$n_total, 10)
    expect_false(is.na(result$best_config))

  })

})
