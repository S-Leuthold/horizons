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
## dispatch_order()
## =========================================================================

describe("dispatch_order()", {

  it("has a positive cost for every model and feature selection configure() admits", {

    keys <- as.vector(outer(VALID_MODELS, VALID_FEATURE_SELECTION, paste, sep = "/"))

    ## Each cell once: a second copy of a cell would never be read, since a
    ## lookup takes the first
    expect_identical(sort(names(CONFIG_COST_SECONDS)), sort(keys))
    expect_true(all(is.finite(CONFIG_COST_SECONDS) & CONFIG_COST_SECONDS > 0))

  })

  it("starts the longest configurations first, and keeps grid order among equal costs", {

    configs <- tibble::tibble(
      model             = c("plsr", "cubist", "xgboost", "lightgbm", "mars"),
      feature_selection = c("pca",  "none",   "none",    "none",     "pca")
    )

    ## cubist with no selection is the costliest cell, mars with PCA the
    ## cheapest; xgboost and lightgbm cost the same, so grid order holds
    expect_identical(dispatch_order(configs), c(2L, 3L, 4L, 1L, 5L))

    ## Two rows of one cell keep their grid order too
    expect_identical(dispatch_order(configs[c(1, 1, 2), ]), c(3L, 1L, 2L))

  })

  it("refuses a configuration whose cell the table lacks, as a bug", {

    configs <- tibble::tibble(model = "rf", feature_selection = "lasso")

    expect_error(dispatch_order(configs), "has no cost for", class = "horizons_error_internal")

  })

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

    ## One cell of the cost table, so they start in grid order
    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002", "cfg_003"),
                              model = "plsr", feature_selection = "none")
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
      dispatch_configs(configs, "payload.rds", finished, function() dplyr::bind_rows(finished$rows)),
      class = "horizons_error_systemic"
    )

    expect_identical(err$results$config_id, "cfg_002")
    expect_match(gsub("\\s+", " ", conditionMessage(err)),
                 "The mirai daemon running configuration \"cfg_001\" stopped while it ran (`errorValue` 19, Connection reset)",
                 fixed = TRUE)
    expect_null(err$parent)

  })

  it("stops as systemic, naming the file and why, with the rows so far, when a daemon cannot read the configurations' data", {

    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002", "cfg_003"),
                              model = "plsr", feature_selection = "none")
    row     <- function(id) tibble::tibble(config_id = id, status = "success")

    ## The second configuration's daemon could not read the payload, and the
    ## first's row came in in the same pass
    unread <- structure(list(path = "/runs/out/tmp/payload-1a2b.rds", reason = "the file system is gone"),
                        class = "horizons_payload_unread")
    values <- list(row("cfg_001"), unread, row("cfg_003"))

    local_mocked_bindings(connected_daemons = function() 2L)
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      list(data = values[[.args$config_i]])
    }, .package = "mirai")

    finished      <- new.env(parent = emptyenv())
    finished$rows <- list()

    err <- expect_error(
      dispatch_configs(configs, unread$path, finished, function() dplyr::bind_rows(finished$rows)),
      class = "horizons_error_systemic"
    )

    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
    expect_match(msg, "The mirai daemon running configuration \"cfg_002\" could not read the configurations' data from",
                 fixed = TRUE)
    expect_match(msg, "/runs/out/tmp/payload-1a2b.rds", fixed = TRUE)
    expect_match(msg, "the file system is gone", fixed = TRUE)

    ## Both ways the file goes: daemons that cannot reach output_dir, and
    ## another run started into it
    expect_match(msg, "daemons on another machine need `output_dir` on a file system they share with it", fixed = TRUE)
    expect_match(msg, "One `output_dir` serves one run at a time, and a run started into it removes an earlier run's file.",
                 fixed = TRUE)
    expect_identical(err$results$config_id, "cfg_001")
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

## =========================================================================
## The memory the daemons on this machine need
## =========================================================================

describe("daemon_need() and daemon_processes()", {

  it("counts the daemon, and an isolated fit's process beside it only when the run fits such a model with isolation on", {

    expect_identical(daemon_processes(c("rf", "cubist"), TRUE), c(daemon = 1L, isolated = 1L))
    expect_identical(daemon_processes(c("rf", "cubist"), FALSE), c(daemon = 1L, isolated = 0L))
    expect_identical(daemon_processes("rf", TRUE), c(daemon = 1L, isolated = 0L))

  })

  it("adds each process's line at the run's data, its rows times its predictors times 8 bytes", {

    bytes <- 8 * 1000 * 200
    line  <- function(p) DAEMON_MEMORY[[p]][["base"]] + DAEMON_MEMORY[[p]][["per_byte"]] * bytes

    expect_equal(daemon_need(c(daemon = 1L, isolated = 0L), 1000, 200), line("daemon"))
    expect_equal(daemon_need(c(daemon = 1L, isolated = 1L), 1000, 200), line("daemon") + line("isolated"))

    ## More data needs more
    expect_gt(daemon_need(c(daemon = 1L, isolated = 0L), 10000, 1701),
              daemon_need(c(daemon = 1L, isolated = 0L), 1000, 1701))

  })

})

describe("memory_available()", {

  it("reads what ps says the system can give processes", {

    available <- memory_available()

    expect_true(is.finite(available) && available > 0)
    expect_lte(abs(available - ps::ps_system_memory()[["avail"]]), 0.2 * available)

  })

})

describe("daemon_reports()'s host", {

  it("names the machine each daemon runs on, which the memory check counts by", {

    skip_on_cran()
    local_test_daemons(2)

    reports <- with_deadline(daemon_reports(character(0), "evaluate"), 30)

    expect_identical(unique(vapply(reports, `[[`, character(1), "host")), Sys.info()[["nodename"]])

  })

})

describe("check_daemon_memory()", {

  need_two <- daemon_need(c(daemon = 1L, isolated = 1L), 1000, 200)

  check <- function(reports, available, models = c("rf", "cubist")) {
    collector <- log_open("evaluate")
    local_mocked_bindings(memory_available = function() available)
    warned <- check_daemon_memory(reports, models, TRUE, 1000, 200, collector, "evaluate")
    list(warned = warned, rows = log_close(collector))
  }

  it("records an input warning when this machine's daemons would need more than it has, suggesting a count that fits", {

    ## Four daemons here, room for two and a bit
    got <- check(daemons_on(pids = 1:4, host = this_host()), 2.5 * need_two)
    msg <- gsub("\\s+", " ", got$rows$message)

    expect_true(got$warned)
    expect_identical(got$rows$topic, "input")
    expect_match(msg, "The 4 mirai daemons on this machine would need about", fixed = TRUE)
    expect_match(msg, "and each fit of \"cubist\" runs in a process of its own beside it", fixed = TRUE)
    expect_match(msg, "`mirai::daemons(2)` fits", fixed = TRUE)

    ## Room for fewer than two: a sequential run
    got <- check(daemons_on(pids = 1:4, host = this_host()), 1.5 * need_two)
    expect_match(got$rows$message, "a sequential run does (`control_horizons(allow_par = FALSE)`)", fixed = TRUE)

  })

  it("is quiet when they fit, counts only the daemons on this machine, and notes a reading it cannot make", {

    here <- function(pids) daemons_on(pids = pids, host = this_host())

    expect_false(check(here(1:2), 2.5 * need_two)$warned)
    expect_identical(nrow(check(here(1:2), 2.5 * need_two)$rows), 0L)

    ## Four daemons, two on another machine
    elsewhere <- c(here(1:2), daemons_on(pids = 3:4, host = "another-machine"))
    expect_false(check(elsewhere, 2.5 * need_two)$warned)
    expect_false(check(daemons_on(pids = 1:2, host = "another-machine"), 1)$warned)

    unread <- check(here(1:4), NA_real_)
    expect_false(unread$warned)
    expect_identical(unread$rows$type, "note")
    expect_match(unread$rows$message, "could not read the memory available", fixed = TRUE)

  })

})

## =========================================================================
## Isolated work: one call in a process of its own
## =========================================================================
## run_isolated() relies on mirai starting a new process for each mirai() sent
## to a profile no daemons() call set (mirai 2.7.2: compute_env() is NULL for
## it, and do_mirai() takes the ephemeral_daemon() branch). The first two
## tests are the guard on that: a later mirai that reused a process for the
## name fails the first, on every platform, and one that held the call fails
## it at its deadline rather than hanging the run; a process left running
## fails the second, on CI first. Every call here waits under a deadline.

describe("run_isolated()", {

  it("runs each call in a process of its own", {

    pids <- with_deadline(c(run_isolated(quote(Sys.getpid()), what = "a test"),
                            run_isolated(quote(Sys.getpid()), what = "a test")), 30)

    expect_length(unique(pids), 2L)
    expect_false(Sys.getpid() %in% pids)

  })

  it("leaves no process running once each value is back", {

    ### tools::pskill() sends no signal 0 on Windows, so it cannot ask
    skip_on_os("windows")

    pids <- with_deadline(c(run_isolated(quote(Sys.getpid()), what = "a test"),
                            run_isolated(quote(Sys.getpid()), what = "a test")), 30)

    expect_true(processes_end(pids))

  })

  it("leaves the daemons as it found them, and runs in none of them", {

    skip_on_cran()
    skip_on_os("windows")

    ## With none set
    mirai::daemons(0)
    before <- daemons_record()
    with_deadline(run_isolated(quote(Sys.getpid()), what = "a test"), 30)
    expect_identical(daemons_record(), before)
    expect_false(mirai::daemons_set(.compute = isolated_profile()))

    ## With two set: the call's process is none of theirs
    local_test_daemons(2)
    before      <- daemons_record()
    daemon_pids <- unlist(with_deadline(mirai::everywhere(Sys.getpid(), .compute = NULL)[], 30))
    pid         <- with_deadline(run_isolated(quote(Sys.getpid()), what = "a test"), 30)

    expect_identical(daemons_record(), before)
    expect_false(pid %in% daemon_pids)
    expect_false(mirai::daemons_set(.compute = isolated_profile()))

  })

  it("starts its process on this session's libraries, so a value it is sent that loads horizons loads this one", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## A process started on the environment's paths alone would find another
    ## horizons first
    decoy <- local_decoy_horizons()
    withr::local_envvar(R_LIBS = decoy)

    ## A function of horizons' namespace loads horizons as the process reads
    ## the message, before the body sets its paths
    loaded <- getNamespaceInfo("horizons", "path")
    path   <- with_deadline(run_isolated(quote(getNamespaceInfo("horizons", "path")),
                                         list(f = draw_training_set), what = "a test",
                                         libraries = unique(c(dirname(loaded), .libPaths()))), 30)

    expect_identical(normalizePath(path), normalizePath(loaded))
    expect_identical(Sys.getenv("R_LIBS"), decoy)

  })

  it("prints what the process printed, and raises its warnings, messages and error here, in order and with their classes", {

    raised <- list()

    err <- expect_error(
      withCallingHandlers(
        utils::capture.output(
          with_deadline(run_isolated(quote({
            cat("printed in the process\n")
            message("a message")
            warning("a warning")
            rlang::abort("an error", class = c("horizons_error_probe", "horizons_error_run", "horizons_error"),
                         field = "a field")
          }), what = "a test"), 30),
          file = (printed <- withr::local_tempfile())
        ),
        warning = function(w) {
          raised[[length(raised) + 1L]] <<- w
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          raised[[length(raised) + 1L]] <<- m
          invokeRestart("muffleMessage")
        }
      ),
      class = "horizons_error_probe"
    )

    expect_identical(readLines(printed), "printed in the process")
    expect_length(raised, 2L)
    expect_s3_class(raised[[1]], "message")
    expect_match(conditionMessage(raised[[1]]), "a message")
    expect_s3_class(raised[[2]], "simpleWarning")
    expect_identical(conditionMessage(raised[[2]]), "a warning")

    expect_identical(class(err)[1:3], c("horizons_error_probe", "horizons_error_run", "horizons_error"))
    expect_identical(err$field, "a field")
    expect_identical(conditionMessage(err), "an error")

  })

  it("returns the value of a call that warns, and takes its values by name", {

    expect_warning(
      value <- with_deadline(run_isolated(quote({ warning("careful"); a + b }), list(a = 1, b = 2), what = "a test"), 30),
      "careful"
    )
    expect_identical(value, 3)

    expect_error(run_isolated(quote(1), list(1), what = "a test"), class = "horizons_error_internal")
    expect_error(run_isolated(quote(1), list(.horizons_seed = 1), what = "a test"),
                 class = "horizons_error_internal")

  })

  it("sends a plain body however horizons was installed: no source references and no coverage counts", {

    ## A keep-source install would put srcrefs on the body's calls, and covr
    ## its count() calls in them; either makes the body large, and the
    ## process load horizons to read it
    body <- isolated_body(quote(f(x = x)))

    expect_identical(body, str2lang(deparse1(body, collapse = "\n")))
    expect_false(any(c("count", "covr") %in% all.names(body)))
    expect_lt(length(serialize(body, NULL)), 5000)
    expect_true("f" %in% all.names(body))
    expect_false(".horizons_expr" %in% all.names(body))

  })

  it("is called by name: each argument a symbol of its own name, so the call holds none of the data", {

    expect_identical(call_by_name(quote(f), list(a = 1:3, b = "x")), quote(f(a = a, b = b)))
    expect_identical(call_by_name(quote(pkg::f), list()), quote(pkg::f()))
    expect_error(call_by_name(quote(f), list(1)), class = "horizons_error_internal")
    expect_error(call_by_name(quote(f), list(a = 1, 2)), class = "horizons_error_internal")

  })

  it("seeds its process from one draw of the caller's stream, so the caller's seed governs what it draws", {

    draw <- function() with_deadline(run_isolated(quote(stats::runif(2)), what = "a test"), 30)

    set.seed(11)
    first <- draw()
    after <- stats::runif(1)

    set.seed(11)
    again <- draw()

    ## Another seed, another draw in the process
    set.seed(12)
    other <- draw()

    set.seed(11)
    sample.int(100000L, 1L)

    expect_identical(again, first)
    expect_false(identical(other, first))
    expect_identical(stats::runif(1), after)

  })

  it("stops as horizons_error_process_died when its process ends without its result", {

    skip_on_os("windows")

    err <- expect_error(
      with_deadline(run_isolated(quote(tools::pskill(Sys.getpid(), 9L)), what = "a Cubist fit"), 30),
      class = "horizons_error_process_died"
    )

    expect_s3_class(err, "horizons_error_run")
    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, paste(PROCESS_DIED, "a Cubist fit ended without returning its result"), fixed = TRUE)
    expect_match(msg, "(mirai's `errorValue` 19, Connection reset)", fixed = TRUE)
    expect_match(msg, "control_horizons(isolate = FALSE)", fixed = TRUE)
    expect_identical(connected_daemons(), 0L)

  })

  it("ends its process when it is stopped before the value is back", {

    skip_on_cran()
    skip_on_os("windows")

    pid_file <- withr::local_tempfile()

    stopped <- tryCatch({
      setTimeLimit(elapsed = 3, transient = TRUE)
      run_isolated(quote({ writeLines(as.character(Sys.getpid()), pid_file); Sys.sleep(60) }),
                   list(pid_file = pid_file), what = "a test")
    }, error = function(e) e)
    setTimeLimit(elapsed = Inf)

    expect_s3_class(stopped, "error")
    expect_true(file.exists(pid_file))
    expect_true(processes_end(as.integer(readLines(pid_file)), seconds = 2))

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

  it("pins each engine thread option the model table declares, and restores them", {

    ## A second engine's option, so a pin that names ranger's alone fails
    local_model_specs(list(xgboost = list(threads = list(option = "horizons.test.threads"))))
    withr::local_options(ranger.num.threads = 7L, horizons.test.threads = NULL)

    unpin <- pin_parent_threads()

    expect_identical(getOption("ranger.num.threads"), 1L)
    expect_identical(getOption("horizons.test.threads"), 1L)

    unpin()

    expect_identical(getOption("ranger.num.threads"), 7L)
    expect_null(getOption("horizons.test.threads"))

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
