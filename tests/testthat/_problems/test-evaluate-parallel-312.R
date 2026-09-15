# Extracted from test-evaluate-parallel.R:312

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "horizons", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
