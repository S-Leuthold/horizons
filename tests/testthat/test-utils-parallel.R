## ---------------------------------------------------------------------------
## Tests: parallelism helpers (R/utils-parallel.R)
## ---------------------------------------------------------------------------
## These run with no backend registered and no installed build: the axis
## resolver is a pure function, and the plan helpers only read the plan.
## Tests that register a plan restore the previous one with withr::defer().

## local_plan() comes from helper-parallel.R.

## =========================================================================
## resolve_parallel_axis()
## =========================================================================

describe("resolve_parallel_axis()", {

  it("is sequential whenever allow_par = FALSE, regardless of the request", {

    for (req in c("auto", "configs", "resamples")) {
      r <- resolve_parallel_axis(req, allow_par = FALSE, n_configs = 100, cv_folds = 5)
      expect_equal(r$axis, "sequential", info = req)
      expect_false(r$dispatch_configs)
      expect_false(r$tune_allow_par)
      expect_null(r$tune_parallel_over)
    }

  })

  it("maps configs and resamples to their dispatch and tune settings", {

    cfg <- resolve_parallel_axis("configs", TRUE, n_configs = 2, cv_folds = 5)
    expect_equal(cfg$axis, "configs")
    expect_true(cfg$dispatch_configs)
    expect_false(cfg$tune_allow_par)          # tune ships nothing inward
    expect_null(cfg$tune_parallel_over)

    res <- resolve_parallel_axis("resamples", TRUE, n_configs = 200, cv_folds = 5)
    expect_equal(res$axis, "resamples")
    expect_false(res$dispatch_configs)
    expect_true(res$tune_allow_par)
    expect_equal(res$tune_parallel_over, "resamples")

  })

  it("auto follows the cost rule: configs when n_configs >= cv_folds", {

    table <- tibble::tribble(
      ~n_configs, ~cv_folds, ~expected,
      1,          5,         "resamples",
      4,          5,         "resamples",
      5,          5,         "configs",     # boundary: equal -> configs
      6,          5,         "configs",
      240,        5,         "configs",
      1,          1,         "configs",
      2,          3,         "resamples",
      3,          3,         "configs"
    )

    for (i in seq_len(nrow(table))) {
      r <- resolve_parallel_axis("auto", TRUE, table$n_configs[i], table$cv_folds[i])
      expect_equal(r$axis, table$expected[i],
                   info = sprintf("n_configs = %d, cv_folds = %d",
                                  table$n_configs[i], table$cv_folds[i]))
    }

  })

  it("refuses 'both' with a pointer to the nested-plan note", {

    expect_error(resolve_parallel_axis("both", TRUE, 10, 5), "not supported")
    expect_error(resolve_parallel_axis("both", TRUE, 10, 5), "nested")

  })

  it("rejects unknown axes and malformed inputs", {

    expect_error(resolve_parallel_axis("folds", TRUE, 10, 5), "parallelize_over")
    expect_error(resolve_parallel_axis("auto", NA, 10, 5), "allow_par")
    expect_error(resolve_parallel_axis("auto", TRUE, 0, 5), "n_configs")
    expect_error(resolve_parallel_axis("auto", TRUE, 10, NA), "cv_folds")

  })

})

## =========================================================================
## Plan inspection
## =========================================================================

describe("registered_plan_label() / registered_workers()", {

  it("describes a sequential plan", {

    local_plan(future::sequential)

    expect_equal(registered_plan_label(), "sequential")
    expect_identical(registered_workers(), 1L)

  })

  it("describes a flat multisession plan with its worker count", {

    skip_on_cran()
    local_plan(future::multisession, workers = 2)

    expect_equal(registered_plan_label(), "multisession")
    expect_identical(registered_workers(), 2L)

  })

  it("describes a nested plan level by level and reports the outer count", {

    skip_on_cran()
    local_plan(list(
      future::tweak(future::multisession, workers = 2),
      future::tweak(future::sequential)
    ))

    expect_equal(registered_plan_label(), "multisession > sequential")
    expect_identical(registered_workers(), 2L)

  })

})

describe("check_parallel_backend()", {

  it("warns and returns FALSE when nothing useful is registered", {

    local_plan(future::sequential)

    expect_warning(ok <- check_parallel_backend("evaluate()"), "offers 1 worker")
    expect_false(ok)

  })

  it("warns for a multisession plan with a single worker (the #37 shape)", {

    skip_on_cran()
    local_plan(future::multisession, workers = 1)

    expect_warning(ok <- check_parallel_backend("fit()"), "fit\\(\\).*1 worker")
    expect_false(ok)

  })

  it("is silent and returns TRUE with two or more workers", {

    skip_on_cran()
    local_plan(future::multisession, workers = 2)

    expect_silent(ok <- check_parallel_backend("evaluate()"))
    expect_true(ok)

  })

  it("never registers or alters the plan", {

    local_plan(future::sequential)
    before <- future::plan("list")

    suppressWarnings(check_parallel_backend())

    expect_identical(future::plan("list"), before)

  })

})

describe("warn_if_mirai_preferred()", {

  it("is silent when no mirai daemons are running", {

    skip_if_not_installed("mirai")
    skip_on_cran()

    if (isTRUE(tryCatch(mirai::status()$connections >= 1, error = function(e) FALSE))) {
      skip("mirai daemons are live in this session")
    }

    local_plan(future::multisession, workers = 2)

    expect_silent(res <- warn_if_mirai_preferred())
    expect_false(res)

  })

})

## =========================================================================
## Thread pinning
## =========================================================================

describe("pin_parent_threads()", {

  it("pins ranger and data.table threads and restores them", {

    old_ranger <- getOption("ranger.num.threads")
    withr::defer(options(ranger.num.threads = old_ranger))

    options(ranger.num.threads = 7L)

    unpin <- pin_parent_threads()

    expect_identical(getOption("ranger.num.threads"), 1L)

    if (requireNamespace("data.table", quietly = TRUE)) {
      expect_identical(data.table::getDTthreads(), 1L)
    }

    unpin()

    expect_identical(getOption("ranger.num.threads"), 7L)

  })

  it("pins BLAS threads at runtime when RhpcBLASctl is available", {

    skip_if_not_installed("RhpcBLASctl")

    unpin <- pin_parent_threads()
    withr::defer(unpin())

    expect_identical(as.integer(RhpcBLASctl::blas_get_num_procs()), 1L)

  })

})

## =========================================================================
## Deprecation of workers
## =========================================================================

describe("deprecate_workers_arg()", {

  it("is silent for NULL", {

    expect_silent(res <- deprecate_workers_arg(NULL))
    expect_false(res)

  })

  it("warns with a stable condition class and names the replacement", {

    expect_warning(deprecate_workers_arg(4L), class = "horizons_deprecated_workers")
    expect_warning(deprecate_workers_arg(4L), "future::plan")
    expect_warning(deprecate_workers_arg(4L), "parallelize_over")

  })

  it("fires every time, not once per session", {

    expect_warning(deprecate_workers_arg(2L), class = "horizons_deprecated_workers")
    expect_warning(deprecate_workers_arg(2L), class = "horizons_deprecated_workers")

  })

})
