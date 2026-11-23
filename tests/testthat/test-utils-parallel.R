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
## config_costs() and dispatch_order()
## =========================================================================

describe("config_costs()", {

  it("has a positive cost for every model and feature selection configure() admits", {

    keys <- as.vector(outer(VALID_MODELS, VALID_FEATURE_SELECTION, paste, sep = "/"))

    ## Each cell once: a second copy of a cell would never be read, since a
    ## lookup takes the first
    expect_identical(sort(names(CONFIG_COST_SECONDS)), sort(keys))
    expect_true(all(is.finite(CONFIG_COST_SECONDS) & CONFIG_COST_SECONDS > 0))

  })

  it("has a row exponent for every model, once", {

    expect_identical(sort(names(CONFIG_ROW_EXPONENT)), sort(VALID_MODELS))
    expect_true(all(is.finite(CONFIG_ROW_EXPONENT) & CONFIG_ROW_EXPONENT > 0))

  })

  it("is the cell at the grid's rows", {

    configs <- tibble::tibble(model = c("cubist", "plsr"), feature_selection = c("pca", "none"))

    expect_equal(config_costs(configs, CONFIG_ROWS_REF), unname(CONFIG_COST_SECONDS[c("cubist/pca", "plsr/none")]))

  })

  it("reorders models by their row exponents as the rows grow", {

    ## Cubist's fits grow faster with the rows than xgboost's: the cheaper
    ## configuration on the measured grid is the dearer one on a library
    ## draw's rows
    configs <- tibble::tibble(model = c("xgboost", "cubist"), feature_selection = c("correlation", "pca"))

    expect_gt(CONFIG_ROW_EXPONENT[["cubist"]], CONFIG_ROW_EXPONENT[["xgboost"]])
    expect_identical(dispatch_order(config_costs(configs, CONFIG_ROWS_REF)), c(1L, 2L))
    expect_identical(dispatch_order(config_costs(configs, 8272)), c(2L, 1L))

  })

  it("refuses a configuration whose cell the table lacks, as a bug", {

    configs <- tibble::tibble(model = "rf", feature_selection = "lasso")

    expect_error(config_costs(configs, 100), "has no cost for", class = "horizons_error_internal")

  })

})

describe("dispatch_order()", {

  it("starts the longest first, and keeps grid order among equal costs", {

    expect_identical(dispatch_order(c(5, 30, 10, 10, 1)), c(2L, 3L, 4L, 1L, 5L))

  })

})

describe("split_plan()", {

  it("gives each configuration the daemons the rule does, case by case", {

    ## Each case: the costs, the daemons, the folds, and the plan. The
    ## share is SPLIT_SHARE x total / n.
    cases <- list(
      list(what  = "nothing above the share", costs = rep(1, 10), n = 4L, folds = 10L,
           plan  = rep(1L, 10)),
      list(what  = "the smallest divisor that brings it under: 15 of 100 on 8, where 2 and 5 both do",
           costs = c(15, rep(1, 85)), n = 8L, folds = 10L, plan = c(2L, rep(1L, 85))),
      list(what  = "the largest divisor at most n when none does: 10 is past 8",
           costs = c(100, rep(1, 20)), n = 8L, folds = 10L, plan = c(5L, rep(1L, 20))),
      list(what  = "5 folds on 4 daemons have no divisor to split onto",
           costs = c(100, 1, 1), n = 4L, folds = 5L, plan = c(1L, 1L, 1L)),
      list(what  = "10 folds on 3 daemons split onto 2, never 3 or 5",
           costs = c(100, 1, 1), n = 3L, folds = 10L, plan = c(2L, 1L, 1L)),
      list(what  = "a configuration at 0.8 of its fair share is split",
           costs = c(2, rep(1, 8)), n = 4L, folds = 10L, plan = c(2L, rep(1L, 8))),
      list(what  = "one configuration alone", costs = 5, n = 4L, folds = 10L, plan = 2L),
      list(what  = "equal costs, each above the share", costs = rep(1, 4), n = 4L, folds = 10L,
           plan  = rep(2L, 4)),
      list(what  = "one daemon", costs = c(100, 1), n = 1L, folds = 10L, plan = c(1L, 1L)),
      list(what  = "no configurations", costs = numeric(0), n = 4L, folds = 10L, plan = integer(0))
    )

    for (case in cases) {
      expect_identical(split_plan(case$costs, case$n, case$folds), case$plan, label = case$what)
    }

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

    ## Equal costs, so they start in grid order
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
      dispatch_configs(configs, rep(1, 3), "payload.rds", finished, function() dplyr::bind_rows(finished$rows),
                       finish = function(i, row) NULL),
      class = "horizons_error_systemic"
    )

    expect_identical(err$results$config_id, "cfg_002")
    expect_match(gsub("\\s+", " ", conditionMessage(err)),
                 "The mirai daemon running configuration \"cfg_001\" stopped while it ran (`errorValue` 19, Connection reset)",
                 fixed = TRUE)
    expect_null(err$parent)

  })

  it("hands each row to finish once it is in finished$rows, and stops where finish stops", {

    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002", "cfg_003"),
                              model = "plsr", feature_selection = "none")

    local_mocked_bindings(connected_daemons = function() 1L)
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      list(data = tibble::tibble(config_id = configs$config_id[.args$config_i], status = "success"))
    }, .package = "mirai")

    finished      <- new.env(parent = emptyenv())
    finished$rows <- list()
    handed        <- list()

    ## The second row's finish stops the run, as a failed write does
    finish <- function(i, row) {
      handed[[length(handed) + 1L]] <<- list(i = i, id = row$config_id, in_rows = length(finished$rows))
      if (i == 2L) rlang::abort("The write failed.", class = "test_finish_stop")
    }

    expect_error(dispatch_configs(configs, rep(1, 3), "payload.rds", finished, function() NULL, finish = finish),
                 class = "test_finish_stop")

    expect_identical(handed, list(list(i = 1L, id = "cfg_001", in_rows = 1L), list(i = 2L, id = "cfg_002", in_rows = 2L)))

  })

  it("stops as systemic, naming the file and why, with the rows so far, when a daemon cannot read the configurations' data", {

    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002", "cfg_003"),
                              model = "plsr", feature_selection = "none")
    row     <- function(id) tibble::tibble(config_id = id, status = "success")

    ## The second configuration's daemon could not read the payload, and the
    ## first's row came in in the same pass
    unread <- structure(list(path = "/runs/out/tmp/payload-1a2b.rds", reason = "the file system is gone",
                             file = "the configurations' data"),
                        class = "horizons_run_file_unread")
    values <- list(row("cfg_001"), unread, row("cfg_003"))

    local_mocked_bindings(connected_daemons = function() 2L)
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      list(data = values[[.args$config_i]])
    }, .package = "mirai")

    finished      <- new.env(parent = emptyenv())
    finished$rows <- list()

    err <- expect_error(
      dispatch_configs(configs, rep(1, 3), unread$path, finished, function() dplyr::bind_rows(finished$rows),
                       finish = function(i, row) NULL),
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
    expect_match(msg, "One `output_dir` serves one run at a time, and a run started into it removes an earlier run's files.",
                 fixed = TRUE)
    expect_identical(err$results$config_id, "cfg_001")
    expect_null(err$parent)

  })

})

## The window on a clock of its own: mirai() is mocked to start a unit at
## the clock's time and resolve it `dur` rounds later, and each wait moves
## the clock one round, so which units start when, and how many slots are in
## flight each round, follow from the window's rule alone.
run_window <- function(slots, dur, width, order = seq_along(slots), .env = parent.frame()) {

  clock <- new.env(parent = emptyenv())
  clock$t     <- 0L
  clock$start <- rep(NA_integer_, length(slots))
  clock$busy  <- integer(0)

  testthat::local_mocked_bindings(
    connected_daemons = function() width,
    await_round = function(tasks, no_daemons) {
      running <- which(!is.na(clock$start) & clock$start <= clock$t & clock$t < clock$start + dur)
      clock$busy <- c(clock$busy, sum(slots[running]))
      clock$t <- clock$t + 1L
    },
    .env = .env
  )
  testthat::local_mocked_bindings(
    mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      clock$start[.args$unit] <- clock$t
      list(unit = .args$unit, data = .args$unit)
    },
    unresolved = function(x) clock$t < clock$start[x$unit] + dur[x$unit],
    stop_mirai = function(x) invisible(NULL),
    .package = "mirai", .env = .env
  )

  task_call <- quote(worker(unit))

  with_deadline(dispatch_units(
    n           = length(slots),
    order       = order,
    task        = function(i) list(call = task_call, args = list(worker = identity, unit = i)),
    collect     = function(i, value) NULL,
    label       = function(i) list(what = "unit", id = i),
    rows_so_far = function() NULL,
    slots       = function(i) slots[[i]]
  ), 10)

  list(start = clock$start, busy = clock$busy)

}

describe("dispatch_units() with slots", {

  it("never has more slots in flight than daemons connected", {

    got <- run_window(slots = c(3L, 1L, 1L, 2L, 1L, 1L), dur = c(4L, 1L, 2L, 3L, 1L, 2L), width = 4L)

    expect_false(anyNA(got$start))
    expect_lte(max(got$busy), 4L)

  })

  it("passes over a unit whose slots are not free for the next that fits, and starts it first once they are", {

    ## On 4 daemons: A (1) and B (2) start; C needs 3, so D (1) starts in its
    ## place. Once B and D are done C starts, ahead of F behind it, which
    ## would also have fitted
    got <- run_window(slots = c(A = 1L, B = 2L, C = 3L, D = 1L, E = 1L, F = 1L),
                      dur   = c(10L, 2L, 5L, 1L, 1L, 1L), width = 4L)

    expect_identical(got$start[[4]], 0L)
    expect_lt(got$start[[4]], got$start[[3]])
    expect_lt(got$start[[3]], got$start[[6]])
    expect_lte(max(got$busy), 4L)

  })

  it("starts a unit wider than the daemons connected once nothing is in flight, alone", {

    got <- run_window(slots = c(1L, 4L, 1L), dur = c(2L, 2L, 1L), width = 2L)

    ## The third fits beside the first and starts; the wide one waits until
    ## both are done, then runs alone, its 4 slots the only ones in flight
    expect_identical(got$start, c(0L, 2L, 0L))
    expect_identical(got$busy, c(2L, 1L, 4L, 4L))

  })

  it("starts units of one slot in their order, as many at once as daemons connected", {

    got <- run_window(slots = rep(1L, 5), dur = c(1L, 3L, 1L, 1L, 1L), width = 2L, order = c(2L, 4L, 1L, 3L, 5L))

    expect_identical(got$start, c(1L, 0L, 2L, 0L, 3L))
    expect_lte(max(got$busy), 2L)

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

describe("config_kept()", {

  role_map <- tibble::tibble(variable = c("id", "y", paste0("wn_", 1:40)),
                             role     = c("id", "outcome", rep("predictor", 40)))

  configs <- tibble::tibble(
    model             = "rf",
    preprocessing     = "snv",
    transformation    = c("log", "log", "none", "none"),
    feature_selection = c("none", "correlation", "correlation", "pca")
  )

  it("counts every predictor, the most any keeps, when the fold stage built nothing", {

    expect_equal(config_kept(configs, role_map), rep(40, 4))

  })

  it("takes the mean its folds kept from the stage's record, by the prep each configuration reads", {

    ## correlation reads the outcome, so each transformation is its own
    ## prep; one PCA prep serves every transformation; the training part's
    ## prep is not a fold's
    record <- tibble::tibble(
      prep         = c("snv/correlation/log", "snv/correlation/log", "snv/correlation/none",
                       "snv/correlation/none", "snv/pca", "snv/pca", "snv/pca"),
      fold         = c("Fold1", "Fold2", "Fold1", "Fold2", "Fold1", "Fold2", "training"),
      n_predictors = c(10L, 20L, 6L, 8L, 3L, 5L, 100L)
    )

    expect_equal(config_kept(configs, role_map, record), c(40, 15, 7, 4))

  })

  it("counts every predictor for a configuration the stage built nothing for", {

    record <- tibble::tibble(prep = "snv/pca", fold = "Fold1", n_predictors = 3L)

    expect_equal(config_kept(configs, role_map, record), c(40, 40, 40, 3))

  })

})

describe("split_need()", {

  it("adds each split configuration in flight at once: its inner daemons as daemons, and what its own daemon queues", {

    ## Four configurations, started in the order 3, 1, 4, 2 on 8 daemons:
    ## 3 (on 5) and 1 (on 2) fit together, 4 (on 2) does not beside them,
    ## and 2 is not split
    inner     <- c(2L, 1L, 5L, 2L)
    kept      <- c(100, 10, 50, 20)
    processes <- c(daemon = 1L, isolated = 1L)
    daemon    <- daemon_need(processes, 1000, 200)
    queue     <- function(k) DAEMON_MEMORY$queue[["base"]] + DAEMON_MEMORY$queue[["per_byte"]] * 10 * 8 * 800 * k

    expect_equal(split_need(inner, c(3L, 1L, 4L, 2L), 8L, processes, n_rows = 1000, n_predictors = 200,
                            n_train = 800, kept = kept, folds = 10L),
                 7 * daemon + queue(50) + queue(100))

    expect_identical(split_need(rep(1L, 4), 1:4, 8L, processes, 1000, 200, 800, kept, 10L), 0)

  })

  it("counts what a split configuration's own daemon held while its folds ran elsewhere, as measured", {

    ## The queue line alone: one configuration on 2, no process of the
    ## daemon's own counted
    queue_at <- function(n_train, kept, folds) {
      split_need(2L, 1L, 2L, c(daemon = 0L, isolated = 0L), n_rows = n_train, n_predictors = kept,
                 n_train = n_train, kept = kept, folds = folds)
    }
    mib <- 2^20

    ## At a known frame: 10 folds of 1,000 rows by 100 predictors, 8 MB of
    ## frames in all, a fixed 0.5 GB and 2 a byte
    expect_equal(queue_at(1000, 100, 10L), 0.5e9 + 2 * 8e6)

    ## Above what was measured: local-real at seed 307 (835 training rows,
    ## about 242 predictors kept a fold, 10 folds), where a split daemon held
    ## up to 478 MiB; and the largest with a model that fits nothing, 1,937
    ## MiB on 8,272 rows of 1,701 predictors at 10 folds
    expect_gte(queue_at(835, 242.5, 10L), 478 * mib)
    expect_gte(queue_at(8272, 1701, 10L), 1937 * mib)

  })

})

describe("check_split_memory()", {

  processes <- c(daemon = 1L, isolated = 0L)
  need_two  <- 2 * daemon_need(processes, 1000, 200)

  check <- function(available, extra, reports = daemons_on(pids = 1:2, host = this_host())) {
    collector <- log_open("evaluate")
    local_mocked_bindings(memory_available = function() available)
    fits <- check_split_memory(reports, extra, processes, 1000, 200,
                               n_split = 1L, collector = collector, verb = "evaluate")
    list(fits = fits, rows = log_close(collector))
  }

  it("leaves the plan when the daemons and the split fit", {

    got <- check(need_two + 2e9, 1e9)

    expect_true(got$fits)
    expect_identical(nrow(got$rows), 0L)

  })

  it("splits nothing, with a note naming the figures, when the daemons fit and the split does not, or neither does", {

    got <- check(need_two + 0.5e9, 1e9)
    msg <- gsub("\\s+", " ", got$rows$message)

    expect_false(got$fits)
    expect_identical(got$rows$type, "note")
    expect_match(msg, "`evaluate()` ran each configuration on one daemon: the inner daemons it would start for 1 long configuration would need about 1.0 GB on this machine",
                 fixed = TRUE)
    gb <- function(bytes) format(round(bytes / 1e9, 1), nsmall = 1)
    expect_match(msg, sprintf("beside the %s GB its 2 daemons need, and %s GB is available.",
                              gb(need_two), gb(need_two + 0.5e9)), fixed = TRUE)

    expect_false(check(need_two / 2, 1e9)$fits)

    ## One daemon here
    one <- check(need_two / 2 + 0.5e9, 1e9, daemons_on(pids = 1L, host = this_host()))
    expect_match(gsub("\\s+", " ", one$rows$message), sprintf("beside the %s GB its 1 daemon needs,", gb(need_two / 2)),
                 fixed = TRUE)

  })

  it("counts only the daemons on this machine, as the check at the door does", {

    ## Two here and two elsewhere: room for the two here and the split
    elsewhere <- c(daemons_on(pids = 1:2, host = this_host()), daemons_on(pids = 3:4, host = "another-machine"))
    expect_true(check(need_two + 2e9, 1e9, elsewhere)$fits)

    ## None here: nothing to check
    expect_true(check(1, 1e9, daemons_on(pids = 1:2, host = "another-machine"))$fits)

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
## Inner daemons: a long configuration's folds on daemons of its own
## =========================================================================
## A configuration's task starts them on its daemon's default profile. Here
## the session stands in for that daemon, with no daemons of its own set, so
## each test resets the profile it sets.

describe("start_inner_daemons()", {

  it("starts the daemons on this process's profile, with its libraries and one thread, leaving its variables as they were", {

    skip_on_cran()

    mirai::daemons(0)
    vars <- c("R_LIBS", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS")
    was  <- Sys.getenv(vars, unset = NA)
    withr::defer(mirai::daemons(0))

    expect_true(with_deadline(start_inner_daemons(2L, "rf", "none"), 30))
    expect_identical(connected_daemons(), 2L)

    seen <- with_deadline(mirai::collect_mirai(mirai::mirai(Sys.getenv(vars), vars = vars)), 30)

    expect_identical(seen[["R_LIBS"]], paste(.libPaths(), collapse = .Platform$path.sep))
    expect_identical(unname(seen[c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS")]),
                     rep(as.character(ENGINE_THREADS), 2))
    expect_identical(Sys.getenv(vars, unset = NA), was)

  })

  it("sets each inner daemon up as a configuration's daemon is: the engines' thread options, kept from task to task (installed build)", {

    skip_on_cran()
    skip_if_dev_package()

    mirai::daemons(0)
    withr::defer(mirai::daemons(0))

    expect_true(with_deadline(start_inner_daemons(2L, "rf", "none"), 30))

    ## Read by a task on each, after the set-up's own: mirai's cleanup
    ## restores a daemon's options after every task but what everywhere() set
    wanted <- engine_thread_options()
    seen   <- with_deadline(mirai::everywhere(lapply(stats::setNames(nm = names), getOption),
                                              .args = list(names = names(wanted)))[], 30)

    expect_length(seen, 2L)
    for (s in seen) expect_identical(s, wanted)

  })

  it("resets the profile and says so when the daemons do not connect in time", {

    skip_on_cran()

    mirai::daemons(0)
    withr::defer(mirai::daemons(0))
    local_mocked_bindings(launch_local = function(...) invisible(NULL), .package = "mirai")

    expect_false(with_deadline(start_inner_daemons(2L, "rf", "none", seconds = 0.5), 10))
    expect_false(mirai::daemons_set())

  })

})

describe("watch_inner_daemons()", {

  it("returns the configuration's value while its inner daemons stay, and the death's record once one has gone", {

    skip_on_cran()
    skip_on_os("windows")

    mirai::daemons(0)
    withr::defer(mirai::daemons(0))
    expect_true(with_deadline(start_inner_daemons(2L, "plsr", "none"), 30))

    expect_identical(watch_inner_daemons("row", 2L, "cfg_001"), "row")

    ## One inner daemon kills itself, and the configuration returns
    kill_one <- function() {
      mirai::mirai(tools::pskill(Sys.getpid(), tools::SIGKILL))
      while (connected_daemons() > 1L) Sys.sleep(0.05)
      "row"
    }
    died <- with_deadline(watch_inner_daemons(kill_one(), 2L, "cfg_001"), 30)

    expect_s3_class(died, "horizons_inner_daemon_died")
    expect_identical(died[c("id", "started", "connected")], list(id = "cfg_001", started = 2L, connected = 1L))

    ## The same when the configuration's error is how the death showed
    fail_after <- function() {
      while (connected_daemons() > 1L) Sys.sleep(0.05)
      rlang::abort("Argument 1 must be a data frame or a named atomic vector.")
    }
    expect_s3_class(watch_inner_daemons(fail_after(), 2L, "cfg_001"), "horizons_inner_daemon_died")

  })

  it("passes a configuration's error through while its inner daemons stay", {

    local_mocked_bindings(connected_daemons = function() 2L)

    expect_error(watch_inner_daemons(rlang::abort("its own", class = "test_own"), 2L, "cfg_001"),
                 class = "test_own")

  })

})

describe("dispatch_configs() and an inner daemon's death", {

  it("stops as systemic with the rows so far, naming the configuration and the setting that turns inner daemons off", {

    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002"), model = "plsr", feature_selection = "none")
    died    <- structure(list(id = "cfg_002", started = 2L, connected = 1L), class = "horizons_inner_daemon_died")
    values  <- list(tibble::tibble(config_id = "cfg_001", status = "success"), died)

    local_mocked_bindings(connected_daemons = function() 3L)
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      list(data = values[[.args$config_i]])
    }, .package = "mirai")

    finished      <- new.env(parent = emptyenv())
    finished$rows <- list()

    err <- expect_error(
      dispatch_configs(configs, c(1, 1), "payload.rds", finished, function() dplyr::bind_rows(finished$rows),
                       finish = function(i, row) NULL, inner = c(1L, 2L)),
      class = "horizons_error_systemic"
    )

    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
    expect_match(msg, "An inner mirai daemon running configuration \"cfg_002\"'s folds stopped while it ran", fixed = TRUE)
    expect_match(msg, "1 of the 2 inner daemons `evaluate()` started on its daemon was gone after it.", fixed = TRUE)
    expect_match(msg, "`control_horizons(inner_daemons = FALSE)` runs each configuration on one daemon.", fixed = TRUE)
    expect_identical(err$results$config_id, "cfg_001")

  })

  it("sends each configuration's daemons with its task and keeps its row's record of them apart", {

    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002"), model = "plsr", feature_selection = "none")
    sent    <- list()

    local_mocked_bindings(connected_daemons = function() 3L)
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      sent[[length(sent) + 1L]] <<- .args$inner
      row <- tibble::tibble(config_id = configs$config_id[.args$config_i], status = "success")
      attr(row, "inner_daemons") <- list(daemons = .args$inner, started = .args$inner > 1L, completed = 0L)
      list(data = row)
    }, .package = "mirai")

    finished      <- new.env(parent = emptyenv())
    finished$rows <- list()

    dispatch_configs(configs, c(1, 1), "payload.rds", finished, function() NULL, finish = function(i, row) NULL,
                     inner = c(1L, 2L))

    expect_identical(unlist(sent), c(1L, 2L))
    expect_null(attr(finished$rows[[2]], "inner_daemons"))
    expect_identical(finished$inner$cfg_002$daemons, 2L)

  })

  it("takes as many of the window's slots for each configuration as its daemons", {

    configs <- tibble::tibble(config_id = c("cfg_001", "cfg_002"), model = "plsr", feature_selection = "none")
    seen    <- NULL

    local_mocked_bindings(dispatch_units = function(..., slots, call) seen <<- vapply(1:2, slots, integer(1)))

    dispatch_configs(configs, c(1, 1), "payload.rds", new.env(), function() NULL, finish = function(i, row) NULL,
                     inner = c(1L, 2L))

    expect_identical(seen, c(1L, 2L))

  })

})

describe("inner_daemons_tree_line() and log_inner_daemons()", {

  it("words the tree's line for one configuration and several, for the setting off, and for memory", {

    expect_identical(inner_daemons_tree_line(c(5L, 2L, 2L, 1L), c(5L, 2L, 2L, 1L), TRUE, TRUE),
                     "Inner daemons: 3 long configurations run their folds on 5, 2 and 2 (control_horizons(inner_daemons = FALSE) turns it off)")
    expect_identical(inner_daemons_tree_line(c(1L, 2L), c(1L, 2L), TRUE, TRUE),
                     "Inner daemons: 1 long configuration runs its folds on 2 (control_horizons(inner_daemons = FALSE) turns it off)")
    expect_identical(inner_daemons_tree_line(c(2L, 2L), c(1L, 1L), FALSE, TRUE),
                     "Inner daemons: off, as asked; 2 long configurations run each on one daemon")
    expect_identical(inner_daemons_tree_line(c(2L, 1L), c(1L, 1L), FALSE, TRUE),
                     "Inner daemons: off, as asked; 1 long configuration runs on one daemon")
    expect_identical(inner_daemons_tree_line(c(2L, 1L), c(1L, 1L), TRUE, TRUE),
                     "Inner daemons: none, since they would not fit in this machine's memory (see the log)")
    expect_null(inner_daemons_tree_line(c(1L, 1L), c(1L, 1L), TRUE, TRUE))

  })

  it("adds a line saying BLAS is not pinned, and how to pin it, when some are split and it cannot be", {

    lines <- inner_daemons_tree_line(c(1L, 2L), c(1L, 2L), TRUE, FALSE)

    expect_length(lines, 2L)
    expect_identical(lines[[2]], paste0(
      "BLAS threads: not pinned, so a configuration on inner daemons can differ from an unsplit run in rounding ",
      "(install RhpcBLASctl, or Sys.setenv(OPENBLAS_NUM_THREADS = 1) before mirai::daemons())"
    ))

    ## Nothing split, nothing to say of it
    expect_null(inner_daemons_tree_line(c(1L, 1L), c(1L, 1L), TRUE, FALSE))
    expect_length(inner_daemons_tree_line(c(2L, 1L), c(1L, 1L), FALSE, FALSE), 1L)

  })

  ## Four configurations, the first three split on 2: the first's inner
  ## daemons connected, the second's and third's did not, the fourth ran
  ## whole
  records <- list(
    cfg_001 = list(daemons = 2L, started = TRUE, completed = 6L),
    cfg_002 = list(daemons = 2L, started = FALSE, completed = 0L),
    cfg_003 = list(daemons = 2L, started = FALSE, completed = 0L),
    cfg_004 = list(daemons = 1L, started = FALSE, completed = 0L)
  )
  ids  <- names(records)
  plan <- c(2L, 2L, 2L, 1L)

  notes <- function(setting = TRUE, pinned = TRUE, inner = plan, recs = records) {
    collector <- log_open("evaluate")
    log_inner_daemons(collector, ids, plan, inner, setting, pinned, recs)
    rows <- log_close(collector)
    rows$message <- gsub("\\s+", " ", rows$message)
    rows
  }

  it("notes, from what each task says, the configurations that ran on inner daemons and those whose inner daemons did not connect", {

    rows <- notes()

    expect_identical(rows$type, c("note", "note"))
    expect_identical(rows$ids, list("cfg_001", c("cfg_002", "cfg_003")))
    expect_match(rows$message[[1]],
                 "ran the folds of 1 long configuration on inner daemons it started on its daemon, within the daemons set: cfg_001 on 2.",
                 fixed = TRUE)
    expect_match(rows$message[[2]],
                 "for 2 configurations did not connect within 60 seconds, so they ran their folds on their own daemons",
                 fixed = TRUE)

    ## One that did not connect, alone
    one <- notes(recs = records[c("cfg_001", "cfg_002")], inner = c(2L, 2L, 1L, 1L))
    expect_match(one$message[[2]], "for 1 configuration did not connect within 60 seconds, so it ran its folds on its own daemon:",
                 fixed = TRUE)

    ## None split by the plan's end: nothing to say
    expect_identical(nrow(notes(inner = c(1L, 1L, 1L, 1L))), 0L)

  })

  it("notes, when BLAS cannot be pinned, that the configurations run on inner daemons can differ in rounding, and how to pin it", {

    rows <- notes(pinned = FALSE)
    blas <- rows[grepl("could not pin BLAS", rows$message, fixed = TRUE), ]

    expect_identical(blas$ids, list("cfg_001"))
    expect_match(blas$message,
                 "since RhpcBLASctl is not installed, so the 1 configuration run on inner daemons can differ in floating-point rounding from the same run without them",
                 fixed = TRUE)
    expect_match(blas$message, "or set `Sys.setenv(OPENBLAS_NUM_THREADS = 1)` before `mirai::daemons()`", fixed = TRUE)

    ## Pinned, or none started on inner daemons: nothing of it
    expect_false(any(grepl("could not pin BLAS", notes()$message, fixed = TRUE)))
    started_none <- lapply(records, function(r) { r$started <- FALSE; r })
    expect_false(any(grepl("could not pin BLAS", notes(pinned = FALSE, recs = started_none)$message, fixed = TRUE)))

  })

  it("with the setting off, notes from the plan the configurations that would have had inner daemons", {

    rows <- notes(setting = FALSE, inner = c(1L, 1L, 1L, 1L), recs = list())

    expect_identical(rows$ids, list(c("cfg_001", "cfg_002", "cfg_003")))
    expect_match(rows$message, "as `control_horizons(inner_daemons = FALSE)` asked; it would have run the folds of 3 long configurations on inner daemons.",
                 fixed = TRUE)

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
