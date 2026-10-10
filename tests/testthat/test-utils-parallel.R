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

  it("describes a flat multisession plan with its worker count, which check_parallel_backend() passes silently", {

    skip_on_cran()
    local_plan(future::multisession, workers = 2)

    expect_equal(registered_plan_label(), "multisession")
    expect_identical(registered_workers(), 2L)

    ## Two workers are enough: no warning, and TRUE
    expect_silent(ok <- check_parallel_backend("evaluate()"))
    expect_true(ok)

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

  it("pins ranger's threads and restores them", {

    old_ranger <- getOption("ranger.num.threads")
    withr::defer(options(ranger.num.threads = old_ranger))

    options(ranger.num.threads = 7L)

    unpin <- pin_parent_threads()

    expect_identical(getOption("ranger.num.threads"), 1L)

    unpin()

    expect_identical(getOption("ranger.num.threads"), 7L)

  })

  it("pins BLAS, OpenMP and data.table to one thread and restores each", {

    skip_if_not_installed("RhpcBLASctl")
    skip_if_not_installed("data.table")

    ## Each count is raised to 2 first: a session started with the thread
    ## variables at 1 would pass before the pin ran. data.table caps at
    ## OMP_NUM_THREADS and at OpenMP's count, so both go first.
    withr::local_envvar(OMP_NUM_THREADS = NA)

    counts <- function() {
      c(blas = RhpcBLASctl::blas_get_num_procs(),
        omp  = RhpcBLASctl::omp_get_max_threads(),
        dt   = data.table::getDTthreads())
    }

    old <- counts()
    withr::defer({
      RhpcBLASctl::blas_set_num_threads(old[["blas"]])
      RhpcBLASctl::omp_set_num_threads(old[["omp"]])
      data.table::setDTthreads(old[["dt"]])
    })

    RhpcBLASctl::blas_set_num_threads(2L)
    RhpcBLASctl::omp_set_num_threads(2L)
    data.table::setDTthreads(2L)

    ## A reference BLAS ignores the call, so only the counts that moved are
    ## checked
    raised <- counts() == 2
    skip_if_not(any(raised), "no thread count here can be raised")

    unpin <- pin_parent_threads()
    expect_equal(counts()[raised], c(blas = 1, omp = 1, dt = 1)[raised])

    unpin()
    expect_equal(counts()[raised], c(blas = 2, omp = 2, dt = 2)[raised])

  })

  it("without RhpcBLASctl, says how to pin BLAS unless a thread variable is set, and still pins data.table", {

    skip_if_not_installed("data.table")

    withr::local_envvar(OPENBLAS_NUM_THREADS = NA, OMP_NUM_THREADS = NA)

    old_dt <- data.table::getDTthreads()
    withr::defer(data.table::setDTthreads(old_dt))

    if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
      old_omp <- RhpcBLASctl::omp_get_max_threads()
      withr::defer(RhpcBLASctl::omp_set_num_threads(old_omp))
      RhpcBLASctl::omp_set_num_threads(2L)
    }

    data.table::setDTthreads(2L)
    skip_if_not(data.table::getDTthreads() == 2L, "data.table's threads cannot be raised here")

    real <- base::requireNamespace
    local_mocked_bindings(
      requireNamespace = function(package, ...) {
        !identical(package, "RhpcBLASctl") && real(package, ...)
      },
      .package = "base"
    )

    msgs <- paste(capture_messages(unpin <- pin_parent_threads()), collapse = "")
    expect_identical(data.table::getDTthreads(), 1L)

    unpin()
    expect_identical(data.table::getDTthreads(), 2L)

    expect_match(msgs, "is not installed, so BLAS threads cannot be pinned at runtime", fixed = TRUE)
    expect_match(msgs, "OPENBLAS_NUM_THREADS=1", fixed = TRUE)

    ## Either variable set means the user has pinned BLAS already
    for (set in list(c(OPENBLAS_NUM_THREADS = "1"), c(OMP_NUM_THREADS = "1"))) {
      withr::with_envvar(set, {
        expect_no_message(unpin <- pin_parent_threads())
        unpin()
      })
    }

  })

})
