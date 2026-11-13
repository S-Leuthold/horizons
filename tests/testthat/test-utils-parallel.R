## ---------------------------------------------------------------------------
## Tests: parallelism helpers (R/utils-parallel.R)
## ---------------------------------------------------------------------------
## These run with no installed build: the axis resolver is a pure function,
## and the backend helpers only read the daemons. Tests that set daemons reset
## them on exit (local_test_daemons(), helper-parallel.R).

## =========================================================================
## resolve_parallel_axis()
## =========================================================================

describe("resolve_parallel_axis()", {

  it("is sequential whenever allow_par = FALSE, regardless of the request", {

    for (req in c("auto", "configs", "resamples")) {
      r <- resolve_parallel_axis(req, allow_par = FALSE, n_configs = 100, cv_folds = 5,
                                 has_output_dir = TRUE)
      expect_equal(r$axis, "sequential", info = req)
      expect_false(r$dispatch_configs)
      expect_false(r$tune_allow_par)
      expect_null(r$tune_parallel_over)
    }

  })

  it("maps configs and resamples to their dispatch and tune settings", {

    cfg <- resolve_parallel_axis("configs", TRUE, n_configs = 2, cv_folds = 5, has_output_dir = TRUE)
    expect_equal(cfg$axis, "configs")
    expect_true(cfg$dispatch_configs)
    expect_false(cfg$tune_allow_par)          # tune ships nothing inward
    expect_null(cfg$tune_parallel_over)

    res <- resolve_parallel_axis("resamples", TRUE, n_configs = 200, cv_folds = 5, has_output_dir = TRUE)
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
      r <- resolve_parallel_axis("auto", TRUE, table$n_configs[i], table$cv_folds[i],
                                 has_output_dir = TRUE)
      expect_equal(r$axis, table$expected[i],
                   info = sprintf("n_configs = %d, cv_folds = %d",
                                  table$n_configs[i], table$cv_folds[i]))
    }

  })

  it("auto never chooses the configs axis without an output_dir to checkpoint into (#233)", {

    r <- resolve_parallel_axis("auto", TRUE, n_configs = 240, cv_folds = 5, has_output_dir = FALSE)

    expect_equal(r$axis, "resamples")
    expect_false(r$dispatch_configs)

  })

  ## "both", an unknown axis and a malformed allow_par are refused by
  ## control_horizons() (test-control.R); n_configs is the configured grid's
  ## row count, and cv_folds config$tuning's, which the door validator checks.

})

## =========================================================================
## The daemons
## =========================================================================

describe("connected_daemons()", {

  it("is 0 with no daemons set", {

    mirai::daemons(0)

    expect_identical(connected_daemons(), 0L)

  })

  it("counts the daemons connected, which check_parallel_backend() passes silently at two", {

    skip_on_cran()
    local_test_daemons(2)

    expect_identical(connected_daemons(), 2L)

    ## Two daemons are enough: no warning, and TRUE
    expect_silent(ok <- check_parallel_backend("evaluate", log_open("evaluate")))
    expect_true(ok)

  })

})

describe("check_parallel_backend()", {

  it("records a substitution and returns FALSE with no daemons, naming the call to set them", {

    mirai::daemons(0)
    collector <- log_open("evaluate")

    expect_no_warning(ok <- check_parallel_backend("evaluate", collector))
    expect_false(ok)

    rows <- log_close(collector)
    msg  <- gsub("\\s+", " ", rows$message)
    expect_identical(rows$topic, "substitution")
    expect_match(msg, "but 0 mirai daemons are connected, so it ran sequentially", fixed = TRUE)
    expect_match(msg, "Set daemons before calling, for example `mirai::daemons(4)`", fixed = TRUE)
    expect_match(msg, "A `future::plan()` is no longer used.", fixed = TRUE)

  })

  it("records it for a single daemon (the #37 shape)", {

    skip_on_cran()
    local_test_daemons(1)
    collector <- log_open("fit")

    expect_false(check_parallel_backend("fit", collector))
    rows <- log_close(collector)
    expect_match(rows$message, "fit().*1 mirai daemon is connected")

  })

  it("never sets, resizes or resets the daemons", {

    skip_on_cran()
    local_test_daemons(2)

    ## The dispatcher is a new one after any reset, even to the same size
    before <- daemons_record()

    check_parallel_backend("evaluate", log_open("evaluate"))

    expect_identical(daemons_record(), before)

  })

})

## =========================================================================
## The configs axis's window and the stops on the daemons
## =========================================================================
## mirai() is mocked to hand each configuration's value back at once, as a
## resolved task: a list whose `data` is the value, which mirai::unresolved()
## reads as resolved.

## A daemon's death as mirai returns it
died_value <- function() structure(19L, class = "errorValue")

describe("await_round()", {

  it("stops when no daemon is connected while a task waits, rather than waiting for ever", {

    skip_on_cran()

    task <- local_unresolved_task()

    expect_error(
      with_deadline(await_round(list(task), function() rlang::abort("No daemon is left.", class = "test_no_daemons")), 10),
      class = "test_no_daemons"
    )

  })

})

describe("dispatch_configs()", {

  it("collects every task that finished in a pass before it stops, so the stop carries their rows", {

    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002", "cfg_003"))
    row     <- function(id) tibble::tibble(config_id = id, status = "success")

    ## The first configuration's daemon died, and the second's row came in
    ## in the same pass
    values <- list(died_value(), row("cfg_002"), row("cfg_003"))

    local_mocked_bindings(connected_daemons = function() 2L)
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      list(data = values[[.args$config_i]])
    }, .package = "mirai")

    finished      <- new.env(parent = emptyenv())
    finished$rows <- list()

    err <- expect_error(
      dispatch_configs(list(configs = configs), finished, function() dplyr::bind_rows(finished$rows)),
      class = "horizons_error_systemic"
    )

    expect_identical(err$results$config_id, "cfg_002")
    expect_match(gsub("\\s+", " ", conditionMessage(err)),
                 "The mirai daemon running configuration \"cfg_001\" stopped while it ran (`errorValue` 19, Connection reset)",
                 fixed = TRUE)
    expect_null(err$parent)

  })

})

describe("daemon_reports()", {

  it("stops as systemic when a daemon dies during the check, with the verb's empty table, as the window does", {

    local_mocked_bindings(everywhere = function(...) structure(list(list(data = died_value())), class = "mirai_map"),
                          .package = "mirai")

    empty <- tibble::tibble(config_id = character(0))
    err   <- expect_error(daemon_reports("rules", "fit", empty), class = "horizons_error_systemic")

    expect_false(inherits(err, "horizons_error_dependency"))
    expect_match(gsub("\\s+", " ", conditionMessage(err)),
                 "A mirai daemon stopped while `fit()` checked the daemons before the run (`errorValue` 19, Connection reset)",
                 fixed = TRUE)
    expect_identical(err$results, empty)

  })

})

describe("resume_hint()", {

  it("names the checkpoints when there is an output_dir, and the results field whenever it holds rows", {

    some <- tibble::tibble(config_id = "cfg_001")
    none <- some[0, ]

    expect_named(resume_hint(some, "out"), c("i", "i"))
    expect_match(resume_hint(some, "out")[1], "checkpointed in", fixed = TRUE)
    expect_match(resume_hint(some, "out")[2], "results", fixed = TRUE)
    expect_named(resume_hint(none, "out"), "i")
    expect_match(resume_hint(some, NULL), "results", fixed = TRUE)
    expect_null(resume_hint(none, NULL))
    expect_null(resume_hint(NULL, NULL))

  })

})

## =========================================================================
## Thread pinning
## =========================================================================

## RhpcBLASctl looks uninstalled to the caller's frame; every other package
## resolves as usual
without_rhpcblasctl <- function(env = parent.frame()) {

  real <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      !identical(package, "RhpcBLASctl") && real(package, ...)
    },
    .package = "base", .env = env
  )

}

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

  it("without RhpcBLASctl, pins quietly: the environment variables are the user's to set", {

    withr::local_envvar(OPENBLAS_NUM_THREADS = NA, OMP_NUM_THREADS = NA)
    without_rhpcblasctl()

    expect_no_message(unpin <- pin_parent_threads())
    unpin()

  })

  it("without RhpcBLASctl, still pins data.table and restores it", {

    skip_if_not_installed("data.table")

    withr::local_envvar(OPENBLAS_NUM_THREADS = NA, OMP_NUM_THREADS = NA)

    old_dt <- data.table::getDTthreads()
    withr::defer(data.table::setDTthreads(old_dt))

    ## data.table caps at OpenMP's count, so that goes up first
    if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
      old_omp <- RhpcBLASctl::omp_get_max_threads()
      withr::defer(RhpcBLASctl::omp_set_num_threads(old_omp))
      RhpcBLASctl::omp_set_num_threads(2L)
    }

    data.table::setDTthreads(2L)
    skip_if_not(data.table::getDTthreads() == 2L, "data.table's threads cannot be raised here")

    without_rhpcblasctl()

    suppressMessages(unpin <- pin_parent_threads())
    expect_identical(data.table::getDTthreads(), 1L)

    unpin()
    expect_identical(data.table::getDTthreads(), 2L)

  })

})
