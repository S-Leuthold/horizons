# Extracted from test-evaluate-parallel.R:185

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "horizons", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
