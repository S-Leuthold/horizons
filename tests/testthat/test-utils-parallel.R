library(testthat)
library(horizons)

test_that("detect_parallel_context returns expected structure", {
  withr::local_envvar(list(
    SLURM_JOB_ID = "",
    PBS_JOBID    = "",
    SGE_TASK_ID  = "",
    RSTUDIO      = "0"
  ))

  context <- horizons:::detect_parallel_context(verbose = FALSE)
  expect_true(context$context %in% c("local", "hpc", "nested"))
  expect_true(is.logical(context$is_unix))
  expect_true(is.logical(context$is_windows))
  expect_true(is.numeric(context$cores_available))

  withr::local_envvar(list(SLURM_JOB_ID = "12345"))
  hpc_context <- horizons:::detect_parallel_context(verbose = FALSE)
  expect_true(hpc_context$on_cluster)
  expect_equal(hpc_context$recommended_backend, if (.Platform$OS.type == "unix") "multicore" else "multisession")
})

test_that("detect_parallel_context flags HPC scheduler correctly", {
  withr::local_envvar(list(
    SLURM_JOB_ID = "12345",
    PBS_JOBID    = "",
    SGE_TASK_ID  = "",
    RSTUDIO      = "0",
    RSTUDIO_SESSION_PORT = ""
  ))

  ctx <- with_mocked_bindings(
    horizons:::detect_parallel_context(min_cores = 1, verbose = FALSE),
    nbrOfWorkers = function() 1L,
    .package = "future"
  )

  expect_equal(ctx$context, "hpc")
  expect_true(ctx$on_cluster)
  expect_equal(
    ctx$recommended_backend,
    if (.Platform$OS.type == "unix") "multicore" else "multisession"
  )
})

test_that("detect_parallel_context identifies nested futures", {
  withr::local_envvar(list(
    SLURM_JOB_ID = "",
    PBS_JOBID    = "",
    SGE_TASK_ID  = "",
    RSTUDIO      = "0",
    RSTUDIO_SESSION_PORT = ""
  ))

  ctx <- with_mocked_bindings(
    horizons:::detect_parallel_context(min_cores = 1, verbose = FALSE),
    nbrOfWorkers = function() 3L,
    .package = "future"
  )

  expect_equal(ctx$context, "nested")
  expect_true(ctx$in_nested)
  expect_equal(ctx$recommended_backend, "multisession")
  expect_false(ctx$use_forking)
})

test_that("setup_parallel_backend configures sequential plan", {
  skip_if_not_installed("future")
  withr::defer(future::plan(future::sequential))
  original_plan <- future::plan()

  withr::local_options(list(future.globals.maxSize = NULL))
  old_plan <- horizons:::setup_parallel_backend(n_workers = 1, force_backend = "sequential", verbose = FALSE)

  expect_s3_class(old_plan, class(original_plan)[1])
  expect_true(inherits(future::plan(), "FutureStrategy"))
  horizons:::restore_parallel_settings(old_plan)
})

test_that("setup_parallel_backend configures multisession workers", {
  skip_if_not_installed("future")
  withr::local_envvar(c(R_PARALLELLY_MAXWORKERS_LOCALHOST = ""))
  withr::defer(future::plan(future::sequential))
  original_plan <- future::plan()

  old_plan <- horizons:::setup_parallel_backend(
    n_workers = 2,
    force_backend = "multisession",
    verbose = FALSE
  )

  expect_equal(Sys.getenv("R_PARALLELLY_MAXWORKERS_LOCALHOST"), "999999")
  expect_true(inherits(future::plan(), "FutureStrategy"))
  expect_equal(attr(old_plan, "old_max_workers"), "")

  horizons:::restore_parallel_settings(old_plan, verbose = FALSE)
  expect_equal(Sys.getenv("R_PARALLELLY_MAXWORKERS_LOCALHOST"), "")
  expect_true(inherits(future::plan(), class(original_plan)[1]))
})

test_that("set_thread_controls enforces thread limits", {
  skip_if_not_installed("data.table")
  withr::local_options(
    ranger.num.threads = NULL,
    ranger.num.cores   = NULL,
    xgboost.nthread    = NULL,
    mc.cores           = NULL
  )

  withr::local_envvar(list(
    SLURM_JOB_ID = "",
    PBS_JOBID    = "",
    SGE_TASK_ID  = ""
  ))

  limit <- horizons:::set_thread_controls(context_aware = FALSE, max_threads = 2)
  expect_equal(limit, 2)
  expect_equal(getOption("ranger.num.threads"), 2)
  expect_equal(getOption("xgboost.nthread"), 2)
})

test_that("set_thread_controls honors forking-aware context", {
  skip_if_not_installed("data.table")
  withr::local_options(
    ranger.num.threads = NULL,
    ranger.num.cores   = NULL,
    xgboost.nthread    = NULL,
    mc.cores           = NULL
  )

  limit <- with_mocked_bindings(
    horizons:::set_thread_controls(context_aware = TRUE, max_threads = 4, verbose = FALSE),
    detect_parallel_context = function(verbose = FALSE) {
      list(
        context = "hpc",
        use_forking = TRUE,
        cores_available = 6,
        in_nested = FALSE
      )
    },
    .package = "horizons"
  )

  expect_equal(limit, 2)
  expect_equal(getOption("mc.cores"), 6)
  expect_equal(getOption("ranger.num.threads"), 2)
  expect_equal(getOption("xgboost.nthread"), 2)
})

test_that("setup_parallel_rng returns independent seeds", {
  seeds <- horizons:::setup_parallel_rng(seed = 123, n_workers = 3, verbose = FALSE)
  expect_length(seeds, 3)
  expect_true(all(vapply(seeds, is.integer, logical(1))))
  expect_false(identical(seeds[[1]], seeds[[2]]))
})

test_that("get_backend_display returns readable backend names", {
  skip_if_not_installed("future")
  withr::defer(future::plan(future::sequential))
  future::plan(future::sequential)
  backend <- horizons:::get_backend_display()
  expect_true(backend %in% c("sequential", "strategy"))
})

test_that("restore_parallel_settings resets globals and env", {
  skip_if_not_installed("future")
  withr::defer(future::plan(future::sequential))
  withr::local_envvar(c(R_PARALLELLY_MAXWORKERS_LOCALHOST = "999"))
  withr::local_options(list(future.globals.maxSize = NULL))

  old_plan <- horizons:::setup_parallel_backend(n_workers = 1, force_backend = "sequential")
  attr(old_plan, "old_max_workers") <- ""
  attr(old_plan, "old_globals_size") <- 12345
  horizons:::restore_parallel_settings(old_plan, verbose = FALSE)
  expect_equal(Sys.getenv("R_PARALLELLY_MAXWORKERS_LOCALHOST"), "")
  expect_equal(getOption("future.globals.maxSize"), 12345)
})

test_that("optimize_parallel_memory returns TRUE invisibly", {
  expect_true(horizons:::optimize_parallel_memory(force_gc = TRUE, verbose = FALSE))
})

test_that("optimize_parallel_memory reports usage when verbose", {
  expect_message(
    horizons:::optimize_parallel_memory(force_gc = FALSE, verbose = TRUE),
    "Memory in use",
    class = "cliMessage"
  )
})
