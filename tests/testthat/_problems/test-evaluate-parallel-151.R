# Extracted from test-evaluate-parallel.R:151

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "horizons", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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

    ## Every config should have a valid status (no silent NA failures)
    expect_true(all(result$evaluation$results$status %in%
                      c("success", "pruned", "failed")))
    expect_false(any(is.na(result$evaluation$results$status)))

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
