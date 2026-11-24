## ---------------------------------------------------------------------------
## Tests: evaluate() parallel execution
## ---------------------------------------------------------------------------

## Daemons resolve horizons by name in the INSTALLED library, so evaluate()
## refuses either parallel axis under devtools::load_all() (see the guard
## tests below). The dispatching tests therefore run only against an installed
## build, with R_LIBS on it: R CMD check, or devtools::test() after
## devtools::install().
## skip_if_dev_package() comes from helper-load-all.R, shared with
## test-pipeline-predict.R's fresh-process round trip.

## local_test_daemons(), with_deadline(), keep_only_warning() and the worker's
## payload (worker_payload(), worker_payload_file()) come from
## helper-parallel.R. Every model's configuration loading and fitting on a
## fresh daemon is held in test-engine-contract.R. Runs that
## several tests read are built once, on first use, through helper-memo.R;
## each accessor is defined just above the tests that read it.

## =========================================================================
## Worker closure footprint — regression guard
## =========================================================================
##
## R serializes a closure together with its enclosing environment. When the
## worker body was an anonymous function defined inside evaluate(), every local
## in that frame went to every worker — including the split and the resample
## object, regardless of what was passed explicitly. Measured on the KSSL clay
## training set at 2 cm-1 (17,788 x 1,701): a 2.26 GiB payload per future
## against 232 MB of genuinely needed inputs, which exceeded
## future.globals.maxSize (1e9, installed by tune at load) and aborted the run.
##
## A synthetic reproduction of the two shapes: a closure referencing only a
## 4-byte integer carried 30.5 MB because its frame held two 15 MB tables, while
## a top-level function serialized 0.0001 MB. A 271,226x difference.
##
## The fix is structural — the worker lives at top level and takes its inputs as
## an argument — so the guard has to be structural too. These assertions fail if
## the body is ever inlined back into evaluate().

describe("evaluate() parallel worker footprint", {

  it("keeps the worker body out of evaluate()'s frame", {

    worker <- horizons:::evaluate_config_worker

    expect_true(isNamespace(environment(worker)))
    expect_match(environmentName(environment(worker)), "horizons")

  })

  it("serializes the worker function without dragging data", {

    ## covr instruments every function with trace calls, which puts the worker
    ## at ~1.02 MB in the coverage run (2026-09-21). The guard is for data in
    ## the closure, and covr's instrumentation is not that.
    testthat::skip_if(identical(Sys.getenv("R_COVR"), "true"),
                      "covr instrumentation inflates the serialized function")

    ## Source references are not data either, and they are what varies by
    ## build: under load_all() they carry the whole source file, which puts
    ## the worker at 0.96 MB, just under this limit, where an installed build
    ## has 14 KB. With them removed the worker is about 3 KB in both, and a
    ## closure over a frame holding data still serializes at that data's size.
    worker <- horizons:::evaluate_config_worker

    expect_lt(length(serialize(utils::removeSource(worker), NULL)), 1e6)

  })

  it("takes its inputs as an argument rather than by capture", {

    expect_named(formals(horizons:::evaluate_config_worker),
                 c("config_i", "payload_path", "inner"))

  })

  it("rejects a malformed payload instead of silently defaulting", {

    good <- setNames(vector("list", length(horizons:::SHARED_ARG_NAMES)),
                     horizons:::SHARED_ARG_NAMES)

    err <- expect_error(
      horizons:::evaluate_config_worker(1L, worker_payload_file(good[setdiff(names(good), "seed")])),
      "Malformed worker payload", class = "horizons_error_internal"
    )
    expect_match(conditionMessage(err), "Missing: seed; unexpected: -", fixed = TRUE)

    typo        <- good
    names(typo)[names(typo) == "grid_size"] <- "gridsize"
    err <- expect_error(horizons:::evaluate_config_worker(1L, worker_payload_file(typo)),
                        "Malformed worker payload", class = "horizons_error_internal")
    expect_match(conditionMessage(err), "Missing: grid_size; unexpected: gridsize", fixed = TRUE)

  })

  it("returns what went wrong, naming the file, rather than raising, when it cannot read its payload", {

    ## Read with base R before anything else, so a daemon on another version
    ## gets as far as saying so; the window stops the run on it
    path <- file.path(withr::local_tempdir(), "payload-gone.rds")

    expect_no_condition(value <- horizons:::evaluate_config_worker(1L, path))

    expect_s3_class(value, "horizons_run_file_unread")
    expect_named(unclass(value), c("path", "reason", "file"))
    expect_identical(value$path, path)
    expect_identical(value$file, "the configurations' data")

    ### base R's words, which name the file: its warning, then its error
    expect_match(value$reason, "payload-gone.rds", fixed = TRUE)

  })

  it("does not carry allow_par in the payload: tune is sequential on the configs axis", {

    expect_false("allow_par" %in% horizons:::SHARED_ARG_NAMES)

  })

  it("does not carry a checkpoint directory in the payload: this session writes every checkpoint", {

    expect_false("checkpoint_dir" %in% horizons:::SHARED_ARG_NAMES)

  })

  it("carries the recipe settings, the outcome range, the ranges searched, the stopping rule, keep_traces and isolate in the payload and hands them to the config (#62, #76)", {

    ## Run in-process: the worker body is an ordinary function, and what is
    ## under test is that it forwards shared$sg_window,
    ## shared$pca_threshold, shared$outcome_range and shared$keep_traces,
    ## not the dispatch. evaluate_single_config() is replaced with a
    ## recorder, so nothing is tuned.
    expect_true(all(c("sg_window", "pca_threshold", "outcome_range", "tune_ranges", "no_improve", "keep_traces",
                      "isolate") %in% horizons:::SHARED_ARG_NAMES))

    ## A debug run never dispatches, so debug is never sent
    expect_false("debug" %in% horizons:::SHARED_ARG_NAMES)

    shared <- worker_payload()
    shared$keep_traces <- TRUE
    shared$isolate     <- FALSE
    shared$tune_ranges <- list(committees = c(1, 8))
    shared$no_improve  <- FALSE

    seen <- NULL

    testthat::local_mocked_bindings(
      evaluate_single_config = function(...) {
        seen <<- list(...)[c("sg_window", "pca_threshold", "outcome_range", "tune_ranges", "no_improve",
                             "keep_traces", "isolate")]
        tibble::tibble(config_id = "cfg_001", status = "failed")
      },
      .package = "horizons"
    )

    horizons:::evaluate_config_worker(1L, worker_payload_file(shared))

    ## The outcome range rides the same payload (#76)
    expect_identical(seen, list(sg_window = 13L, pca_threshold = 0.9,
                                outcome_range = c(-Inf, Inf), tune_ranges = list(committees = c(1, 8)),
                                no_improve = FALSE, keep_traces = TRUE, isolate = FALSE))

  })

  it("runs its configuration with data.table and each engine thread option on one thread", {

    skip_if_not_installed("RhpcBLASctl")
    skip_if_not_installed("data.table")

    ## Both counts are raised first: a session started with the thread
    ## variables at 1 would pass before the worker set anything. data.table
    ## caps at OMP_NUM_THREADS and at OpenMP's count, so both go first. A
    ## second engine's option, so a worker that names ranger's alone fails.
    local_model_specs(list(xgboost = list(threads = list(option = "horizons.test.threads"))))
    withr::local_envvar(OMP_NUM_THREADS = NA)
    withr::local_options(ranger.num.threads = 7L, horizons.test.threads = 7L)

    old_omp <- RhpcBLASctl::omp_get_max_threads()
    old_dt  <- data.table::getDTthreads()
    withr::defer({
      RhpcBLASctl::omp_set_num_threads(old_omp)
      data.table::setDTthreads(old_dt)
    })

    RhpcBLASctl::omp_set_num_threads(2L)
    data.table::setDTthreads(2L)
    skip_if_not(data.table::getDTthreads() == 2L, "data.table's threads cannot be raised here")

    seen <- NULL

    testthat::local_mocked_bindings(
      evaluate_single_config = function(...) {
        seen <<- c(dt = data.table::getDTthreads(), ranger = getOption("ranger.num.threads"),
                   other = getOption("horizons.test.threads"))
        tibble::tibble(config_id = "cfg_001", status = "failed")
      },
      .package = "horizons"
    )

    horizons:::evaluate_config_worker(1L, worker_payload_file())

    expect_identical(seen, c(dt = 1L, ranger = 1L, other = 1L))

  })

  it("stamps the row it returns, writing nothing beside its payload, and refuses a worker on another version", {

    shared <- worker_payload()
    path   <- worker_payload_file(shared)

    testthat::local_mocked_bindings(
      evaluate_single_config = function(...) tibble::tibble(config_id = "cfg_001", status = "failed"),
      .package = "horizons"
    )

    row <- horizons:::evaluate_config_worker(1L, path)

    expect_identical(row$data_hash, shared$data_fp$data_hash)
    expect_identical(row$settings[[1]], shared$settings)
    ## What the row says of its inner daemons is the session's, which also
    ## writes the row's checkpoint as it collects it
    expect_identical(attr(row, "inner_daemons"), list(daemons = 1L, started = FALSE, completed = 0L))
    expect_identical(list.files(dirname(path), all.files = TRUE, no.. = TRUE), "payload.rds")

    ## Another version's worker is a dependency error, which no unit
    ## contains, and it is checked before the payload's keys: another
    ## version's keys may differ, and that is not a bug
    shared$pkg_version <- "0.0.0"
    shared$seed        <- NULL

    err <- expect_error(horizons:::evaluate_config_worker(1L, worker_payload_file(shared)),
                        "but this session runs 0.0.0.", fixed = TRUE,
                        class = "horizons_error_dependency")
    expect_match(conditionMessage(err), "Reinstall the package so the daemons run matching code.",
                 fixed = TRUE)

  })

  it("refuses a parallel run under devtools::load_all(), on either axis, before anything is drawn", {

    ## The seam reports load_all(), so this runs on an installed build too;
    ## the daemons are mocked, since the refusal comes before they would be
    ## used
    local_mocked_bindings(
      running_under_load_all = function() TRUE,
      check_parallel_backend = function(...) TRUE,
      connected_daemons      = function() 2L,
      draw_eval_split        = function(...) stop("the split was drawn"),
      evaluate_single_config = function(...) stop("a configuration ran")
    )

    for (over in c("configs", "resamples")) {

      tmpdir <- withr::local_tempdir()

      out <- utils::capture.output(err <- expect_error(
        evaluate(make_eval_object(n_configs = 4),
                 control = control_horizons(allow_par = TRUE, parallelize_over = over,
                                            output_dir = tmpdir, verbose = TRUE)),
        "cannot run under `devtools::load_all()`", fixed = TRUE,
        class = "horizons_error_dependency"
      ))

      expect_match(conditionMessage(err), "Install the package first", fixed = TRUE)

      ## Refused at the door: no tree, no manifest
      expect_identical(out, character())
      expect_false(file.exists(file.path(tmpdir, "eval_manifest.rds")))

    }

  })

  it("refuses a parallel run whose workers load another version, before anything is drawn", {

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = function(...) daemons_on("0.0.1"),
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      draw_eval_split         = function(...) stop("the split was drawn")
    )

    expect_error(
      evaluate(make_eval_object(n_configs = 1),
               control = control_horizons(allow_par = TRUE, parallelize_over = "resamples")),
      paste0("A mirai daemon loads horizons 0.0.1, but this session runs ",
             utils::packageVersion("horizons"), "."),
      fixed = TRUE, class = "horizons_error_dependency"
    )

  })

  it("says on the console, under verbose only, how many configurations it sends out and where to monitor them", {

    ## The dispatch is stopped at the window, so this runs under load_all()
    ## too
    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE,
      dispatch_configs        = function(...) rlang::abort("stop", class = "dispatched")
    )

    run <- function(verbose) {
      dir <- withr::local_tempdir(.local_envir = parent.frame())
      utils::capture.output(tryCatch(
        suppressWarnings(evaluate(make_eval_object(n_configs = 4), seed = 42L,
                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                             output_dir = dir, verbose = verbose))),
        dispatched = function(e) NULL
      ))
    }

    shown <- run(verbose = TRUE)
    expect_true(any(grepl("Processing 4 pending configs...", shown, fixed = TRUE)))
    expect_true(any(grepl("Monitor: horizons::monitor_evaluate(", shown, fixed = TRUE)))

    quiet <- run(verbose = FALSE)
    expect_false(any(grepl("Processing|Monitor:", quiet)))

  })

  it("asks every daemon which horizons it loads and which of the run's packages it lacks (installed build)", {

    skip_on_cran()
    skip_if_dev_package()
    local_test_daemons(2)

    reports <- with_deadline(daemon_reports(c("rules", "horizonsFakePkgXYZ123")), 30)

    ## One report from each daemon, which loads the build R_LIBS names
    expect_length(reports, 2L)
    expect_length(unique(vapply(reports, function(r) r$pid, integer(1))), 2L)
    expect_false(Sys.getpid() %in% vapply(reports, function(r) r$pid, integer(1)))
    for (r in reports) {
      expect_identical(r$version, as.character(utils::packageVersion("horizons")))
      expect_identical(r$missing, "horizonsFakePkgXYZ123")
    }

  })

  it("refuses a run whose daemons lack a package, naming it and how many daemons lack it, and notes a daemon left unchecked", {

    ## The reports mocked, so this runs under load_all() too
    local_mocked_bindings(running_under_load_all = function() FALSE,
                          connected_daemons      = function() 2L)

    needed  <- needed_packages(c("svm_rbf", "plsr"), "none")
    lacking <- function(...) list(
      list(pid = 11L, version = as.character(utils::packageVersion("horizons")), missing = "kernlab"),
      list(pid = 12L, version = as.character(utils::packageVersion("horizons")), missing = character(0))
    )

    local_mocked_bindings(daemon_reports = lacking)
    err <- expect_error(check_parallel_workers("evaluate", needed, log_open("evaluate")),
                        class = "horizons_error_missing_package")
    expect_s3_class(err, "horizons_error_dependency")
    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "1 of the 2 mirai daemons checked cannot load 1 package `evaluate()` needs.", fixed = TRUE)
    expect_match(msg, "kernlab, needed for svm_rbf", fixed = TRUE)

    ## One daemon of the two answered: the other went unchecked, which is a
    ## note, not a refusal
    local_mocked_bindings(daemon_reports = function(...) daemons_on(pids = 11L))
    collector <- log_open("evaluate")
    expect_no_error(check_parallel_workers("evaluate", needed, collector))
    rows <- log_close(collector)
    expect_identical(rows$type, "note")
    expect_match(rows$message, "checked 1 of the 2 mirai daemons connected", fixed = TRUE)

    ## Every daemon checked: nothing on the log
    local_mocked_bindings(daemon_reports = matching_daemons)
    collector <- log_open("evaluate")
    check_parallel_workers("evaluate", needed, collector)
    expect_identical(nrow(log_close(collector)), 0L)

  })

  it("stops with a results table of every column and no rows when no daemon is left during the check at the door", {

    skip_on_cran()

    ## The check's task is one no daemon takes, and none is connected
    task <- local_unresolved_task()
    local_mocked_bindings(everywhere = function(...) structure(list(task), class = "mirai_map"),
                          .package = "mirai")
    local_mocked_bindings(
      running_under_load_all = function() FALSE,
      check_parallel_backend = function(...) TRUE,
      draw_eval_split        = function(...) stop("the split was drawn")
    )

    err <- expect_error(
      with_deadline(evaluate(make_eval_object(n_configs = 1), seed = 42L,
                             control = control_horizons(allow_par = TRUE, parallelize_over = "resamples")), 30),
      class = "horizons_error_no_daemons"
    )

    one <- create_failed_result("cfg_001", "failed")

    expect_identical(nrow(err$results), 0L)
    expect_identical(names(err$results), names(one))
    expect_identical(vapply(err$results, typeof, character(1)), vapply(one, typeof, character(1)))

  })

})


## =========================================================================
## Argument handling: removed arguments, backend check, output_dir requirement
## =========================================================================

describe("evaluate() - workers is gone", {

  it("is not an argument", {

    obj <- make_eval_object(n_configs = 2)

    expect_error(evaluate(obj, workers = 2L), "unused argument")

  })

})

## The shared runs below keep daemons_record() (helper-parallel.R) from
## before and after, so a run that set, resized or reset the daemons shows.

## One allow_par = TRUE run with no daemons set, read by the two tests below
## (helper-memo.R), recording the daemons as the run found and left them.
## Builders must be quiet, so the warnings evaluate() gave are kept (their
## messages, in order) rather than shown, and replay_warnings() signals them
## again inside the test that expects one.
allow_par_run <- function() memo_fixture("allow_par_run", build_allow_par_run)

build_allow_par_run <- function() {

  ## rf and cubist: the results are compared with a run made later in the
  ## session, and each cubist fit runs in a process of its own, so its
  ## neighbour cutoff does not depend on the fits made before it
  daemons_before <- daemons_record()
  obj            <- make_eval_object(n_configs = 2)
  warnings       <- list()

  result <- withCallingHandlers(
    evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE)),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- simpleWarning(conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(obj = obj, result = result, warnings = warnings,
       daemons_before = daemons_before, daemons_after = daemons_record())

}

replay_warnings <- function(run) {

  for (w in run$warnings) warning(w)
  run$result

}

describe("evaluate() - allow_par without a usable backend", {

  it("warns naming the daemons connected, runs sequentially, and never sets or alters daemons", {

    run <- allow_par_run()

    expect_warning(
      result <- keep_only_warning(
        replay_warnings(run),
        "0 mirai daemons are connected"
      ),
      "0 mirai daemons are connected"
    )

    expect_s3_class(result, "horizons_eval")
    expect_equal(result$evaluation$parallelize_over, "sequential")
    expect_identical(result$evaluation$workers, 1L)

    expect_identical(run$daemons_after, run$daemons_before)
    expect_false(run$daemons_after$set)

  })

  it("gives the same results as allow_par = FALSE", {

    run <- allow_par_run()
    obj <- run$obj

    seq_result <- suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = FALSE))
    )
    par_result <- run$result

    expect_equal(par_result$evaluation$results$rmse,
                 seq_result$evaluation$results$rmse)
    expect_equal(par_result$evaluation$best_config,
                 seq_result$evaluation$best_config)

  })

})

describe("evaluate() - the parallel axis's setup", {

  it("names the axis, the daemons and their count, records them, and pins the threads only while it runs", {

    ## The backend and the runner mocked: the runner records the thread
    ## option it sees and stops the run once the header is out
    run <- function(allow_par, workers) {

      dir    <- withr::local_tempdir()
      inside <- NULL

      out <- utils::capture.output(testthat::with_mocked_bindings(
        tryCatch(
          suppressWarnings(suppressMessages(
            evaluate(make_eval_object(n = 60, n_configs = 1), seed = 42L, control = control_horizons(allow_par = allow_par, parallelize_over = "resamples", output_dir = dir, verbose = TRUE))
          )),
          header_rendered = function(e) NULL
        ),
        check_parallel_backend  = function(...) TRUE,
        check_parallel_workers  = function(...) invisible(NULL),
        connected_daemons       = function() workers,
        evaluate_single_config  = function(...) {
          inside <<- getOption("ranger.num.threads")
          rlang::abort("stop", class = "header_rendered")
        },
        .package = "horizons"
      ))

      manifest <- readRDS(file.path(dir, "eval_manifest.rds"))

      list(line     = grep("Parallel:", out, value = TRUE, fixed = TRUE),
           manifest = manifest[c("plan", "workers", "axis")],
           inside   = inside)

    }

    withr::local_options(ranger.num.threads = 7L)

    two        <- run(TRUE, 2L)
    sequential <- run(FALSE, 2L)

    expect_identical(two$line, "│  Parallel: over resamples on mirai (2 daemons)")
    expect_identical(sequential$line, character())

    ## allow_par = FALSE never reads the daemons
    expect_identical(two$manifest, list(plan = "mirai", workers = 2L, axis = "resamples"))
    expect_identical(sequential$manifest,
                     list(plan = "sequential", workers = 1L, axis = "sequential"))

    ## One thread a daemon on the resamples axis; a run in this session sets
    ## it to control_horizons(threads = ), one by default (#114). Either
    ## way the session's own is back once the run is over
    expect_identical(c(two$inside, sequential$inside), c(1L, 1L))
    expect_identical(getOption("ranger.num.threads"), 7L)

  })

  it("says on the configs axis's parallel line that the daemons have no dispatcher, before any configuration runs", {

    ## The dispatch is stopped at the window, as an interrupt would stop it:
    ## the line is already on the console. Mocked, so this runs under
    ## load_all() too.
    line <- function(dispatcher) {

      testthat::local_mocked_bindings(
        running_under_load_all  = function() FALSE,
        daemon_reports          = matching_daemons,
        check_parallel_backend  = function(...) TRUE,
        connected_daemons       = function() 2L,
        daemons_dispatch        = function() dispatcher,
        dispatch_configs        = function(...) rlang::abort("stop", class = "dispatched")
      )

      dir <- withr::local_tempdir()
      out <- utils::capture.output(tryCatch(
        suppressWarnings(evaluate(make_eval_object(n_configs = 4), seed = 42L,
                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                             output_dir = dir, verbose = TRUE))),
        dispatched = function(e) NULL
      ))

      grep("Parallel:", out, value = TRUE, fixed = TRUE)

    }

    expect_identical(line(FALSE), "│  Parallel: over configs on mirai (2 daemons, no dispatcher)")
    expect_identical(line(TRUE), "│  Parallel: over configs on mirai (2 daemons)")

  })

  it("runs a debug run in this session, whatever allow_par says, without reading the daemons", {

    dir <- withr::local_tempdir()

    out <- utils::capture.output(testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 1), seed = 42L,
                                  control = control_horizons(allow_par = TRUE, debug = TRUE,
                                                             output_dir = dir, verbose = TRUE))),
        header_rendered = function(e) NULL
      ),
      check_parallel_backend = function(...) stop("the daemons were read"),
      check_parallel_workers = function(...) stop("the workers were asked"),
      evaluate_single_config = function(...) rlang::abort("stop", class = "header_rendered"),
      .package = "horizons"
    ))

    expect_identical(grep("Parallel:", out, value = TRUE, fixed = TRUE),
                     "│  Parallel: off, since debug runs in this session")

    manifest <- readRDS(file.path(dir, "eval_manifest.rds"))
    expect_identical(manifest[c("allow_par", "axis", "plan", "workers")],
                     list(allow_par = TRUE, axis = "sequential", plan = "sequential", workers = 1L))

  })

})

describe("evaluate() - the daemons' memory", {

  it("warns once at its end when this machine's daemons would need more memory than it has, and says so on the tree's parallel line", {

    ## A real result row to return, read before anything is mocked
    ## (helper-memo.R)
    template <- allow_par_run()$result$evaluation$results[1, ]

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = function(...) daemons_on(host = this_host()),
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE,
      memory_available        = function() 1e9,
      evaluate_single_config  = function(config_row, ...) {
        row <- template
        row$config_id <- config_row$config_id
        row
      },
      dispatch_configs        = function(configs, payload_path, finished, ...) {
        for (i in seq_len(nrow(configs))) {
          finished$rows <- c(finished$rows, list(evaluate_config_worker(i, payload_path)))
        }
      }
    )

    out <- utils::capture.output(got <- collect_warnings(
      evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L,
               control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                          output_dir = withr::local_tempdir(), verbose = TRUE))
    ))

    input <- Filter(function(w) inherits(w, "horizons_warning_input"), got$warnings)
    row   <- got$value$log[got$value$log$topic %in% "input", ]

    expect_length(input, 1L)
    expect_match(gsub("\\s+", " ", conditionMessage(input[[1]])),
                 "The 2 mirai daemons on this machine would need about", fixed = TRUE)
    expect_identical(nrow(row), 1L)
    expect_identical(input[[1]]$log, row)
    expect_identical(grep("Parallel:", out, value = TRUE, fixed = TRUE),
                     "│  Parallel: over configs on mirai (2 daemons, more memory than available)")

  })

})

## A configs-axis run whose plan splits its first configuration onto two of
## its two daemons, both on this machine, with `available` bytes free. Each
## task runs here as it is sent (mirai() mocked), through the window and the
## worker, which keep the daemons each was sent and what its row says of its
## inner daemons; the inner daemons are mocked to connect or not
## (`connects`), and BLAS to be pinnable or not (`pinned`). The
## configurations' own work is a row of an earlier run.
run_split <- function(available = 1e15, inner_daemons = TRUE, connects = TRUE, pinned = TRUE,
                      .env = parent.frame()) {

  template <- allow_par_run()$result$evaluation$results[1, ]
  sent     <- integer(0)

  local_mocked_bindings(
    running_under_load_all  = function() FALSE,
    daemon_reports          = function(...) daemons_on(host = this_host()),
    check_parallel_backend  = function(...) TRUE,
    connected_daemons       = function() 2L,
    daemons_dispatch        = function() TRUE,
    memory_available        = function() available,
    blas_pinnable           = function() pinned,
    split_plan              = function(costs, n, folds) c(2L, rep(1L, length(costs) - 1L)),
    start_inner_daemons     = function(...) connects,
    evaluate_single_config  = function(config_row, ...) {
      row <- template
      row$config_id <- config_row$config_id
      row
    },
    .env = .env
  )
  local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
    sent <<- c(sent, .args$inner)
    list(data = eval(.expr, .args))
  }, .package = "mirai", .env = .env)

  out <- utils::capture.output(got <- collect_warnings(
    evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L,
             control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                        output_dir = withr::local_tempdir(), verbose = TRUE,
                                        inner_daemons = inner_daemons))
  ))

  list(inner = sent, log = got$value$log, out = out,
       input = Filter(function(w) inherits(w, "horizons_warning_input"), got$warnings))

}

describe("evaluate() - inner daemons and this machine's memory", {

  split_note <- function(log) log[grepl("inner daemons it would start", log$message, fixed = TRUE), ]

  it("splits as planned when the daemons and their inner daemons fit", {

    got <- run_split(1e15)

    expect_identical(got$inner, c(2L, 1L, 1L))
    expect_identical(nrow(split_note(got$log)), 0L)
    expect_length(got$input, 0L)

  })

  it("splits nothing, with a note, when the daemons fit and their inner daemons would not", {

    ## Two daemons need about 4.1 GB for this run, each with a cubist fit's
    ## process, and a split onto two inner daemons as much again
    got <- run_split(5e9)

    expect_identical(got$inner, c(1L, 1L, 1L))
    expect_identical(split_note(got$log)$type, "note")
    expect_length(got$input, 0L)

  })

  it("splits nothing when the daemons alone do not fit, which the check at the door warns of once", {

    got <- run_split(1e9)

    expect_identical(got$inner, c(1L, 1L, 1L))
    expect_length(got$input, 1L)
    expect_match(gsub("\\s+", " ", conditionMessage(got$input[[1]])), "The 2 mirai daemons on this machine would need about",
                 fixed = TRUE)

  })

})

describe("evaluate() - saying which configurations run on inner daemons", {

  it("says once, in the tree and as a note on the log naming them, which configurations run on inner daemons and on how many", {

    got  <- run_split()
    note <- got$log[grepl("on inner daemons it started", got$log$message, fixed = TRUE), ]

    expect_identical(grep("Inner daemons:", got$out, value = TRUE, fixed = TRUE),
                     "\u2502  Inner daemons: 1 long configuration runs its folds on 2 (control_horizons(inner_daemons = FALSE) turns it off)")
    expect_identical(note$type, "note")
    expect_identical(note$ids[[1]], "cfg_001")
    expect_match(gsub("\\s+", " ", note$message),
                 "`evaluate()` ran the folds of 1 long configuration on inner daemons it started on its daemon, within the daemons set: cfg_001 on 2.",
                 fixed = TRUE)
    expect_match(note$message, "`control_horizons(inner_daemons = FALSE)` runs each configuration on one daemon.", fixed = TRUE)

    ## Its inner daemons connected, and BLAS is pinned: nothing else to say
    expect_false(any(grepl("did not connect|could not pin BLAS", got$log$message)))
    expect_false(any(grepl("BLAS threads:", got$out, fixed = TRUE)))

  })

  it("says a configuration the plan split ran on its own daemon when its inner daemons did not connect, and not that it ran on them", {

    got <- run_split(connects = FALSE)
    msg <- gsub("\\s+", " ", got$log$message)

    ## The task was sent its two daemons, and its row said they did not
    ## connect
    expect_identical(got$inner, c(2L, 1L, 1L))

    fallback <- got$log[grepl("did not connect within", msg, fixed = TRUE), ]
    expect_identical(fallback$type, "note")
    expect_identical(fallback$ids[[1]], "cfg_001")
    expect_match(gsub("\\s+", " ", fallback$message),
                 "for 1 configuration did not connect within 60 seconds, so it ran its folds on its own daemon:",
                 fixed = TRUE)
    expect_false(any(grepl("on inner daemons it started", msg, fixed = TRUE)))

  })

  it("says once, in the tree and on the log, that BLAS is not pinned and how to pin it, when it cannot be and a configuration runs on inner daemons", {

    got  <- run_split(pinned = FALSE)
    note <- got$log[grepl("could not pin BLAS", got$log$message, fixed = TRUE), ]

    expect_identical(grep("BLAS threads:", got$out, value = TRUE, fixed = TRUE),
                     paste0("│  BLAS threads: not pinned, so a configuration on inner daemons can differ from an unsplit run in ",
                            "rounding (install RhpcBLASctl, or Sys.setenv(OPENBLAS_NUM_THREADS = 1) before mirai::daemons())"))
    expect_identical(note$type, "note")
    expect_identical(note$ids[[1]], "cfg_001")
    expect_match(gsub("\\s+", " ", note$message), "can differ in floating-point rounding from the same run without them",
                 fixed = TRUE)

  })

  it("says, with inner daemons off, that a configuration that would have had them ran on one daemon", {

    got  <- run_split(inner_daemons = FALSE)
    note <- got$log[grepl("inner_daemons = FALSE)` asked", got$log$message, fixed = TRUE), ]

    expect_identical(got$inner, c(1L, 1L, 1L))
    expect_identical(grep("Inner daemons:", got$out, value = TRUE, fixed = TRUE),
                     "\u2502  Inner daemons: off, as asked; 1 long configuration runs on one daemon")
    expect_identical(note$type, "note")
    expect_identical(note$ids[[1]], "cfg_001")
    expect_match(gsub("\\s+", " ", note$message), "it would have run the folds of 1 long configuration on inner daemons.",
                 fixed = TRUE)

  })

})

describe("evaluate() - output_dir and the configs axis (#233)", {

  it("refuses an explicit configs axis without an output_dir before anything runs", {

    obj <- make_eval_object(n_configs = 4)

    local_mocked_bindings(
      check_parallel_backend = function(...) stop("the daemons were read"),
      evaluate_single_config = function(...) stop("a configuration ran")
    )

    expect_error(
      evaluate(obj, control = control_horizons(allow_par = TRUE, parallelize_over = "configs")),
      "needs an `output_dir`", fixed = TRUE, class = "horizons_error_input"
    )

  })

  it("chooses the resamples axis on auto when there is no output_dir, however many configs", {

    obj <- make_eval_object(n_configs = 4)   # 4 >= 3 folds: auto would choose configs with one

    out <- utils::capture.output(testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(obj, seed = 42L,
                                  control = control_horizons(allow_par = TRUE, verbose = TRUE))),
        header_rendered = function(e) NULL
      ),
      check_parallel_backend = function(...) TRUE,
      check_parallel_workers = function(...) invisible(NULL),
      connected_daemons      = function() 2L,
      evaluate_single_config = function(...) rlang::abort("stop", class = "header_rendered"),
      .package = "horizons"
    ))

    expect_identical(grep("Parallel:", out, value = TRUE, fixed = TRUE),
                     "│  Parallel: over resamples on mirai (2 daemons)")

  })

})


## =========================================================================
## Configs axis (installed build only)
## =========================================================================

## One four-config run on two daemons, parallelize_over = "auto" (4 configs
## >= 3 folds, so the configs axis), read by the test below, which gets a
## private copy of its output_dir (helper-memo.R). The builder sets the
## daemons itself and resets them, recording them (daemons_record(), as in
## allow_par_run() above) as the run found and left them. It muffles the
## run's warnings: a warning in a build would fail every test that reads it.
configs_axis_run <- function(.env = parent.frame()) {
  memo_dir("configs_axis_run", build_configs_axis_run, .env = .env)
}

build_configs_axis_run <- function(dir) {

  local_test_daemons(2)

  daemons_before <- daemons_record()
  obj            <- make_eval_object(n_configs = 4)

  result <- with_deadline(suppressWarnings(
    evaluate(obj,
             seed = 42L, control = control_horizons(allow_par = TRUE, output_dir = dir))
  ), 120)

  list(obj = obj, result = result, daemons_before = daemons_before,
       daemons_after = daemons_record())

}

describe("evaluate() - configs axis", {

  it("writes a schema-5 manifest describing the daemons, the axis, the data and the settings, one result and one checkpoint per config, and leaves the user's daemons exactly as it found them", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    run    <- configs_axis_run()
    tmpdir <- run$dir
    result <- run$value$result

    manifest <- readRDS(file.path(tmpdir, "eval_manifest.rds"))
    expect_identical(manifest$schema_version, 5L)
    expect_identical(manifest$data_hash, result$evaluation$results$data_hash[1])
    expect_identical(manifest$data_n_rows, result$evaluation$n_train)

    ## The worker stamps the record the parent sent, so rows written on the
    ## configs axis carry the same settings as the manifest.
    expect_true(all(vapply(result$evaluation$results$settings, identical,
                           logical(1), manifest$settings)))
    expect_equal(manifest$axis, "configs")
    expect_equal(manifest$parallelize_over_requested, "auto")
    expect_equal(manifest$plan, "mirai")
    expect_identical(manifest$workers, 2L)
    expect_null(manifest$outer)
    expect_null(manifest$inner)

    expect_equal(result$evaluation$parallelize_over, "configs")
    expect_identical(result$evaluation$workers, 2L)

    expect_s3_class(result, "horizons_eval")
    expect_equal(nrow(result$evaluation$results), 4)
    expect_true(all(result$evaluation$results$config_id %in%
                      c("cfg_001", "cfg_002", "cfg_003", "cfg_004")))
    expect_false(any(is.na(result$evaluation$results$status)))

    checkpoint_files <- list.files(file.path(tmpdir, "checkpoints"), pattern = "\\.rds$")
    expect_equal(length(checkpoint_files), 4)

    ## This session wrote each as it collected the row: the row returned,
    ## its unit's records aside
    for (id in result$evaluation$results$config_id) {
      stored <- readRDS(file.path(tmpdir, "checkpoints", paste0(id, ".rds")))
      attr(stored, "unit_records") <- NULL
      expect_identical(stored, result$evaluation$results[result$evaluation$results$config_id == id, ], label = id)
    }

    ## The payload the daemons read was removed when the run returned
    expect_setequal(list.files(tmpdir, recursive = TRUE, all.files = TRUE, include.dirs = TRUE),
                    c("eval_manifest.rds", "checkpoints", file.path("checkpoints", sprintf("cfg_%03d.rds", 1:4))))

    expect_identical(run$value$daemons_after, run$value$daemons_before)
    expect_identical(run$value$daemons_after$connected, 2L)

  })

})


## =========================================================================
## The configs axis's dispatch on mirai
## =========================================================================
## What each task carries, the stop when no daemon is left, a daemon's death,
## the cancellation on an interrupt or an error, daemons without the
## dispatcher and what crosses back from a task. The real-daemon tests run
## plsr, whose cells of the cost table tie, so they start in grid order, all
## but the test of the start order, which puts rf among them.

## A plsr grid of `n` configurations, small enough to run in seconds
plsr_object <- function(n) {
  obj <- make_eval_object(n = 60, n_configs = n)
  obj$config$configs$model <- "plsr"
  obj
}

## On the daemons every configuration but the first notes in `started` that
## it has begun, then waits ten seconds before it runs, in R code a
## cancellation interrupts, so the first comes back first however long a
## fresh daemon takes to load its packages; each leaves a mark in `marks`
## once it has run to its end
local_slow_configs <- function(marks, started) {
  with_deadline(mirai::everywhere({
    ns   <- asNamespace("horizons")
    real <- get("evaluate_single_config", envir = ns)
    utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
      if (config_row$config_id != "cfg_001") {
        file.create(file.path(started, config_row$config_id))
        Sys.sleep(10)
      }
      row <- real(config_row, ...)
      file.create(file.path(marks, config_row$config_id))
      row
    }, ns = "horizons")
  }, marks = marks, started = started)[], 30)
}

## The window's waits, with `raise()` called as the first wait after the
## first configuration's checkpoint is written begins: the first
## configuration collected, the second running and the third sent. With
## `started`, the file a daemon writes as the third begins, it waits for
## that first (up to 30 s), since a cancel sent before a task has reached
## its daemon can miss it, and the test is of the configurations running.
## The check at the door is mocked, so its own wait is not among them.
local_stop_after_first <- function(store, raise, started = NULL, .env = parent.frame()) {
  fired      <- FALSE
  real_round <- await_round
  local_mocked_bindings(
    daemon_reports = matching_daemons,
    await_round    = function(tasks, no_daemons) {
      if (!fired && file.exists(file.path(store, "cfg_001.rds"))) {
        fired <<- TRUE
        until <- Sys.time() + 30
        while (!is.null(started) && !file.exists(started) && Sys.time() < until) Sys.sleep(0.05)
        raise()
      }
      real_round(tasks, no_daemons)
    },
    .env = .env
  )
}

describe("evaluate() - the configs axis on mirai", {

  it("sends each task the worker function, its configuration and the payload's path, and no data", {

    ## The task is recorded and the run stopped there, so this runs under
    ## load_all() too. The payload is read while the task is sent, since the
    ## stop removes it.
    sent <- NULL
    dir  <- withr::local_tempdir()

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE
    )
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      payload <- readRDS(.args$payload_path)
      sent <<- list(expr = .expr, args = .args, compute = .compute, keys = names(payload),
                    fold_cache_dir = payload$fold_cache_dir,
                    ## The worker aside: covr instruments it to about 1 MB
                    task_bytes    = length(serialize(.args[names(.args) != "worker"], NULL)),
                    payload_bytes = length(serialize(payload, NULL)))
      rlang::abort("stop", class = "dispatched")
    }, .package = "mirai")

    tryCatch(
      suppressWarnings(evaluate(make_eval_object(n_configs = 4), seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                           output_dir = dir, verbose = FALSE))),
      dispatched = function(e) NULL
    )

    expect_identical(sent$expr, quote(worker(config_i, payload_path, inner)))
    expect_named(sent$args, c("worker", "config_i", "payload_path", "inner"))
    expect_identical(sent$args$inner, 1L)
    expect_identical(sent$args$worker, evaluate_config_worker)
    expect_true(isNamespace(environment(sent$args$worker)))
    expect_null(sent$compute)

    ## The payload is a file of its own under output_dir, at an absolute
    ## path, holding what a worker needs
    expect_identical(dirname(sent$args$payload_path), file.path(normalizePath(dir, winslash = "/"), "tmp"))
    expect_match(basename(sent$args$payload_path), "^payload-.*\\.rds$")
    ## In one separator: on Windows tempfile() joins with a backslash
    expect_false(grepl("\\", sent$args$payload_path, fixed = TRUE))
    expect_setequal(sent$keys, SHARED_ARG_NAMES)

    ## The fold preparations' directory, absolute: the run's tmp/, where the
    ## payload is. No checkpoint directory: this session writes them
    expect_identical(sent$fold_cache_dir, dirname(sent$args$payload_path))
    expect_false("checkpoint_dir" %in% sent$keys)

    ## The task carries none of it
    expect_lt(sent$task_bytes, 1024)
    expect_lt(sent$task_bytes, sent$payload_bytes / 10)

    ## The run stopped, and the payload went with it
    expect_false(file.exists(sent$args$payload_path))
    expect_false(dir.exists(file.path(dir, "tmp")))

  })

  it("names a payload no other call names, so two calls at one seed write two files", {

    ## Mocked as the test above; the name is read as the task is sent
    paths <- character(0)

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE
    )
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      paths <<- c(paths, .args$payload_path)
      rlang::abort("stop", class = "dispatched")
    }, .package = "mirai")

    obj <- make_eval_object(n_configs = 4)
    dir <- withr::local_tempdir()

    for (call in 1:2) {
      set.seed(307)
      tryCatch(
        suppressWarnings(evaluate(obj, seed = 42L,
                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                             output_dir = dir, verbose = FALSE))),
        dispatched = function(e) NULL
      )
    }

    expect_length(paths, 2L)
    expect_false(identical(paths[1], paths[2]))

  })

  it("removes the payloads an earlier run left in output_dir once its checkpoint gates pass, and nothing of the user's", {

    ## Mocked as the tests above; what tmp/ holds is read as the task is
    ## sent, by when the manifest is written and the payload with it
    template <- allow_par_run()$result$evaluation$results[1, ]
    seen     <- NULL

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE
    )
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      seen <<- list(files = list.files(dirname(.args$payload_path)), own = basename(.args$payload_path))
      rlang::abort("stop", class = "dispatched")
    }, .package = "mirai")

    run <- function(dir) {
      suppressWarnings(evaluate(make_eval_object(n_configs = 4), seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                           output_dir = dir, verbose = FALSE)))
    }

    ## An earlier run's payload, a cut-off write of one and a fold prep this
    ## run does not need, and a file of the user's
    left <- c("payload-old.rds", "file1.rds.tmp", "fold-0000.rds", "notes.txt")
    leave <- function(dir) {
      dir.create(file.path(dir, "tmp"))
      for (f in left) writeLines("left", file.path(dir, "tmp", f))
      file.path(dir, "tmp")
    }

    dir <- withr::local_tempdir()
    tmp <- leave(dir)

    tryCatch(run(dir), dispatched = function(e) NULL)

    expect_setequal(seen$files, c(seen$own, "notes.txt"))
    expect_identical(list.files(tmp), "notes.txt")

    ## A run refused at the gates, a row scored on other data in the store,
    ## owns nothing and removes nothing
    dir <- withr::local_tempdir()
    tmp <- leave(dir)

    row <- template
    row$config_id              <- "cfg_001"
    row$data_fields[[1]]$ids   <- "0000deadbeef"
    dir.create(file.path(dir, "checkpoints"))
    saveRDS(row, file.path(dir, "checkpoints", "cfg_001.rds"))

    err <- expect_error(run(dir), class = "horizons_error_input")
    expect_match(gsub("\\s+", " ", conditionMessage(err)), "different training data", fixed = TRUE)
    expect_setequal(list.files(tmp), left)

    ## A sequential run owns output_dir as well, and removes them too; it is
    ## stopped at its first configuration, after the gates
    dir <- withr::local_tempdir()
    tmp <- leave(dir)

    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(make_eval_object(n_configs = 4), seed = 42L,
                                  control = control_horizons(output_dir = dir, verbose = FALSE))),
        first = function(e) NULL
      ),
      evaluate_single_config = function(config_row, ...) rlang::abort("stop", class = "first")
    )
    expect_identical(list.files(tmp), "notes.txt")

  })

  it("stops as systemic before any task is sent when the payload cannot be written, with the rows resumed", {

    ## Two configurations checkpointed by a sequential run stopped at the
    ## third, so the configs run below resumes them and has two to send.
    ## output_dir is relative, so the stops can be seen to name it as given,
    ## not as the absolute path the daemons are sent
    template <- allow_par_run()$result$evaluation$results[1, ]
    obj      <- make_eval_object(n_configs = 4)
    base     <- withr::local_tempdir()
    dir      <- "out"

    withr::local_dir(base)

    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = dir, verbose = FALSE))),
        third = function(e) NULL
      ),
      evaluate_single_config = function(config_row, ...) {
        if (config_row$config_id == "cfg_003") rlang::abort("stop", class = "third")
        row <- template
        row$config_id <- config_row$config_id
        row
      }
    )
    expect_setequal(list.files(file.path(dir, "checkpoints")), c("cfg_001.rds", "cfg_002.rds"))

    sent <- 0L

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE
    )
    local_mocked_bindings(mirai = function(...) {
      sent <<- sent + 1L
      rlang::abort("stop", class = "dispatched")
    }, .package = "mirai")

    run <- function() {
      suppressWarnings(evaluate(obj, seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                           output_dir = dir, verbose = FALSE)))
    }

    ## The write refused
    real_write <- write_output_file
    err <- expect_error(
      testthat::with_mocked_bindings(run(), write_output_file = function(value, path) {
        if (startsWith(basename(path), "payload-")) return("the disk is full")
        real_write(value, path)
      }),
      class = "horizons_error_systemic"
    )

    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
    expect_match(msg, "`evaluate()` could not write to", fixed = TRUE)
    expect_match(msg, "could not write to \\W?out/tmp\\W")
    expect_match(msg, ".rds: the disk is full", fixed = TRUE)
    expect_identical(err$results$config_id, c("cfg_001", "cfg_002"))
    expect_identical(sent, 0L)
    expect_false(dir.exists(file.path(dir, "tmp")))

    ## The directory refused: a file of the user's where it goes, which is
    ## left as it was
    writeLines("the user's", file.path(dir, "tmp"))

    err <- expect_error(run(), class = "horizons_error_systemic")

    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
    expect_match(msg, "`evaluate()` could not write to", fixed = TRUE)
    expect_match(msg, "could not write to \\W?out\\W")
    expect_match(msg, "tmp: ", fixed = TRUE)
    expect_identical(err$results$config_id, c("cfg_001", "cfg_002"))
    expect_identical(sent, 0L)
    expect_identical(readLines(file.path(dir, "tmp")), "the user's")

  })

  it("keeps at most one configuration in flight per connected daemon (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## Each wait is handed the tasks in flight: four configurations on two
    ## daemons never put more than two there, so this session holds at most
    ## two payloads
    local_test_daemons(2)
    real_await <- await_round
    in_flight  <- integer(0)
    local_mocked_bindings(await_round = function(tasks, no_daemons) {
      in_flight <<- c(in_flight, length(tasks))
      real_await(tasks, no_daemons)
    })

    ev <- with_deadline(suppressWarnings(evaluate(plsr_object(4), seed = 42L,
                                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                             output_dir = withr::local_tempdir(),
                                                                             verbose = FALSE))), 120)

    expect_identical(nrow(ev$evaluation$results), 4L)
    expect_gt(length(in_flight), 0L)
    expect_lte(max(in_flight), 2L)

  })

  it("writes the configurations' data once a call, not once a configuration, for every task to read (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## The writes into the run's tmp/ in this session, and the path the
    ## window is given; the daemons write only checkpoints. Two of the
    ## configurations read one PCA prep per fold, which the stage's tasks
    ## build from the same payload before any configuration runs
    local_test_daemons(2)
    real_write  <- write_output_file
    real_window <- dispatch_configs
    written     <- character(0)
    given       <- character(0)
    local_mocked_bindings(
      write_output_file = function(value, path) {
        if (basename(dirname(path)) == "tmp") written <<- c(written, path)
        real_write(value, path)
      },
      dispatch_configs = function(configs, payload_path, ...) {
        given <<- c(given, payload_path)
        real_window(configs, payload_path, ...)
      }
    )

    obj <- plsr_object(4)
    obj$config$configs$feature_selection[3:4] <- "pca"
    obj$config$configs$model[4] <- "elastic_net"

    ev <- with_deadline(suppressWarnings(evaluate(obj, seed = 42L,
                                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                             output_dir = withr::local_tempdir(),
                                                                             verbose = FALSE))), 120)

    expect_identical(ev$evaluation$results$status, rep("success", 4L))

    payloads <- written[startsWith(basename(written), "payload-")]
    expect_length(payloads, 1L)
    expect_identical(given, payloads)

    ## Written first, then the PCA prep's three folds and its training part
    ## (plsr bounds num_comp), each once
    expect_identical(written[1], payloads)
    expect_length(written, 5L)
    expect_true(all(grepl("^fold-[0-9a-f]+\\.rds$", basename(written[-1]))))

  })

  it("gives the daemons absolute paths, so a relative output_dir reaches them after a setwd() (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## The daemons start in one directory and the run goes on from another,
    ## its output_dir relative to it: each daemon reads the payload and its
    ## fold preps where this session wrote them, and this session writes the
    ## checkpoints
    launch <- withr::local_tempdir()
    work   <- withr::local_tempdir()

    withr::local_dir(launch)
    local_test_daemons(2)
    withr::local_dir(work)

    ## The third configuration reads a fold prep per fold, from the run's
    ## tmp/, as its daemon is given it
    obj <- plsr_object(3)
    obj$config$configs$feature_selection[3] <- "pca"

    ev <- with_deadline(suppressWarnings(evaluate(obj, seed = 42L,
                                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                             output_dir = "out", verbose = FALSE))), 120)

    expect_identical(ev$evaluation$results$status, rep("success", 3L))
    expect_setequal(list.files(file.path(work, "out", "checkpoints")), sprintf("cfg_%03d.rds", 1:3))

  })

  it("starts the longest configurations first, by the cost table (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)

    ## rf costs more than plsr in the table, so on two daemons cfg_002 and
    ## cfg_004 start first, in either order, and cfg_001 and cfg_003 once one
    ## of them has finished
    expect_gt(CONFIG_COST_SECONDS[["rf/none"]], CONFIG_COST_SECONDS[["plsr/none"]])
    obj <- make_eval_object(n = 60, n_configs = 4)
    obj$config$configs$model <- c("plsr", "rf", "plsr", "rf")

    ## Each daemon notes the configuration it starts, each line in one write
    ## so two daemons' lines never interleave
    started <- withr::local_tempfile(pattern = "started")
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("evaluate_single_config", envir = ns)
      utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
        cat(paste0(config_row$config_id, "\n"), file = started, append = TRUE)
        real(config_row, ...)
      }, ns = "horizons")
    }, started = started)[], 30)

    with_deadline(suppressWarnings(evaluate(obj, seed = 42L,
                                            control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                       output_dir = withr::local_tempdir(),
                                                                       verbose = FALSE))), 120)

    order <- readLines(started)
    expect_length(order, 4L)
    expect_setequal(order[1:2], c("cfg_002", "cfg_004"))
    expect_setequal(order[3:4], c("cfg_001", "cfg_003"))

  })

  it("stops with the rows so far when no daemon is left to run the rest (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## Two daemons that exit after two tasks each: the check at the door
    ## takes one of each, the first two configurations the others, and
    ## nothing is left for the third
    local_test_daemons(2, maxtasks = 2)

    dir <- withr::local_tempdir()
    err <- expect_error(
      with_deadline(suppressWarnings(evaluate(plsr_object(3), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 60),
      class = "horizons_error_no_daemons"
    )

    expect_s3_class(err, "horizons_error_systemic")
    expect_identical(err$results$config_id, c("cfg_001", "cfg_002"))

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "No mirai daemon is connected, so `evaluate()` stopped with 1 configuration still to run.",
                 fixed = TRUE)
    expect_match(msg, "`maxtasks` tasks, or past `idletime` or `walltime`", fixed = TRUE)
    expect_match(msg, "resumes from them", fixed = TRUE)

  })

  it("stops as systemic when a configuration's daemon dies, with the rows finished before it (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    store <- file.path(dir, "checkpoints")

    ## On the daemons, the third configuration kills its daemon once the
    ## first two are checkpointed, and a poll later, so their rows are in
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("evaluate_single_config", envir = ns)
      utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
        if (config_row$config_id == "cfg_003") {
          until <- Sys.time() + 30
          while (!all(file.exists(file.path(store, c("cfg_001.rds", "cfg_002.rds")))) && Sys.time() < until) {
            Sys.sleep(0.05)
          }
          Sys.sleep(1)
          tools::pskill(Sys.getpid(), tools::SIGKILL)
        }
        real(config_row, ...)
      }, ns = "horizons")
    }, store = store)[], 30)

    err <- expect_error(
      with_deadline(suppressWarnings(evaluate(plsr_object(3), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 120),
      class = "horizons_error_systemic"
    )

    ## Raised once, by the window, naming the transport's error value
    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, paste("The mirai daemon running configuration \"cfg_003\" stopped while it ran",
                            "(`errorValue` 19, Connection reset), so `evaluate()` cannot go on."),
                 fixed = TRUE)
    expect_match(msg, "resumes from them", fixed = TRUE)
    expect_match(msg, "in this error's results field", fixed = TRUE)
    expect_null(err$parent)
    expect_identical(err$results$config_id, c("cfg_001", "cfg_002"))

  })

  it("stops as systemic, with the rows so far, when a daemon cannot read the configurations' data (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir     <- withr::local_tempdir()
    tmp     <- file.path(dir, "tmp")
    signals <- withr::local_tempdir()

    ## On the daemons, through the file system, every wait with a deadline.
    ## The first and second configurations start together and each reads the
    ## payload before its unit runs. The first says it has; the second then
    ## removes the payload and holds its daemon until the run has stopped
    ## (the cancel, or tmp/ removed as evaluate() exits). The first runs once
    ## the payload is gone, so the third starts on its daemon after it and
    ## finds nothing to read.
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("evaluate_single_config", envir = ns)
      utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
        until   <- Sys.time() + 100
        payload <- function() list.files(tmp, pattern = "^payload-", full.names = TRUE)
        if (config_row$config_id == "cfg_001") {
          file.create(file.path(signals, "cfg_001-read"))
          while (length(payload()) > 0 && Sys.time() < until) Sys.sleep(0.05)
        }
        if (config_row$config_id == "cfg_002") {
          while (!file.exists(file.path(signals, "cfg_001-read")) && Sys.time() < until) Sys.sleep(0.05)
          unlink(payload())
          while (dir.exists(tmp) && Sys.time() < until) Sys.sleep(0.05)
        }
        real(config_row, ...)
      }, ns = "horizons")
    }, tmp = tmp, signals = signals)[], 30)

    err <- expect_error(
      with_deadline(suppressWarnings(evaluate(plsr_object(3), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 120),
      class = "horizons_error_systemic"
    )

    ## Raised once, by the window, naming the file and what the axis needs
    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
    expect_match(msg, "The mirai daemon running configuration \"cfg_003\" could not read the configurations' data from",
                 fixed = TRUE)
    expect_match(msg, file.path(normalizePath(dir, winslash = "/"), "tmp", "payload-"), fixed = TRUE)
    expect_match(msg, "each daemon reads the configurations' data and their fold preparations from `output_dir`, at the paths this session gives it",
                 fixed = TRUE)
    expect_match(msg, "`parallelize_over = \"resamples\"` runs without it.", fixed = TRUE)
    expect_match(msg, "resumes from them", fixed = TRUE)
    expect_null(err$parent)

    ## The first configuration finished; the second was held, and the third
    ## had nothing to run on
    expect_identical(err$results$config_id, "cfg_001")

  })

  it("cancels the configurations in flight when it is interrupted, so none runs on and nothing is written after it returns (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    store <- file.path(dir, "checkpoints")
    marks <- withr::local_tempdir()

    started <- withr::local_tempdir()

    local_slow_configs(marks, started)
    local_stop_after_first(store, rlang::interrupt, started = file.path(started, "cfg_003"))

    got <- tryCatch(
      with_deadline(suppressWarnings(evaluate(plsr_object(4), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 60),
      interrupt = function(i) "interrupted"
    )

    expect_identical(got, "interrupted")

    ## Once the daemons are idle again: a configuration the cancel missed
    ## leaves its mark before its task ends, so no fixed sleep is needed
    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_identical(list.files(marks), "cfg_001")
    expect_identical(list.files(store), "cfg_001.rds")

    ## The payload went when evaluate() stopped, the cancel first
    expect_false(dir.exists(file.path(dir, "tmp")))

  })

  it("cancels the configurations in flight when it stops on an error, so none runs on and nothing is written after it returns (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    store <- file.path(dir, "checkpoints")
    marks <- withr::local_tempdir()

    ## As the interrupt's test above, with an error instead: the cancel is an
    ## on.exit(), so an error reaches it too
    started <- withr::local_tempdir()

    local_slow_configs(marks, started)
    local_stop_after_first(store, function() rlang::abort("The window stopped.", class = "test_window_stop"),
                           started = file.path(started, "cfg_003"))

    expect_error(
      with_deadline(suppressWarnings(evaluate(plsr_object(4), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 60),
      class = "test_window_stop"
    )

    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_identical(list.files(marks), "cfg_001")
    expect_identical(list.files(store), "cfg_001.rds")

    ## The payload went when evaluate() stopped, the cancel first
    expect_false(dir.exists(file.path(dir, "tmp")))

  })

  it("says once, on the log, that daemons without the dispatcher lose the order and the cancellation, and runs as asked (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    row_cols <- c("config_id", "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                  "cv_rmse", "cv_rrmse", "cv_rsq", "cv_ccc", "cv_rpd", "cv_mae")

    obj <- plsr_object(3)
    seq_result <- suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(verbose = FALSE)))

    local_test_daemons(2, dispatcher = FALSE)

    got <- collect_warnings(with_deadline(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                           output_dir = withr::local_tempdir(), verbose = FALSE)),
      120
    ), class = "horizons_warning_input")

    expect_length(got$warnings, 1L)
    msg <- gsub("\\s+", " ", conditionMessage(got$warnings[[1]]))
    expect_match(msg, "set with `dispatcher = FALSE`, so `evaluate()` cannot start the longest configurations first",
                 fixed = TRUE)
    ## A stopped run's configurations run on, and their rows are not kept:
    ## this session writes each checkpoint as it collects the row
    expect_match(msg, "or cancel the ones still running when it stops: those run on to their end, and what they return is not kept.",
                 fixed = TRUE)

    ## The hint holds for a remote pool too: it names the setting, not a
    ## call that would start local daemons
    expect_match(msg, "mirai keeps its dispatcher unless `dispatcher = FALSE` is set; set the daemons again without it.",
                 fixed = TRUE)
    expect_no_match(msg, "mirai::daemons(n)", fixed = TRUE)

    rows <- got$value$log[got$value$log$topic %in% "input", ]
    expect_identical(nrow(rows), 1L)

    expect_identical(got$value$evaluation$parallelize_over, "configs")
    expect_identical(got$value$evaluation$results[row_cols], seq_result$evaluation$results[row_cols])

  })

  it("says nothing of the dispatcher on the resamples axis, where the folds' tasks are tune's (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2, dispatcher = FALSE)

    got <- collect_warnings(with_deadline(
      evaluate(plsr_object(2), seed = 42L, control = control_horizons(allow_par = TRUE, parallelize_over = "resamples",
                                                                      verbose = FALSE)),
      120
    ), class = "horizons_warning_input")

    ## On the daemons without the dispatcher, so the absence below is the
    ## axis's
    expect_false(daemons_dispatch())
    expect_identical(got$value$evaluation$parallelize_over, "resamples")
    expect_length(got$warnings, 0L)
    expect_false(any(got$value$log$topic %in% "input"))

  })

  it("hands a warning raised before a configuration's unit to the configuration, to render with its own", {

    ## The worker body muffles the warning and passes it on, so it is
    ## deduplicated and capped with the unit's warnings rather than appended
    ## after them
    real_rebuild <- rebuild_resamples
    handed       <- NULL
    local_mocked_bindings(rebuild_resamples = function(...) {
      warning("A package warned while the folds were rebuilt.")
      real_rebuild(...)
    })
    local_mocked_bindings(
      evaluate_single_config = function(config_row, ..., outside_warnings) {
        handed <<- outside_warnings
        tibble::tibble(config_id = config_row$config_id, status = "success",
                       warnings = list("The rendered warnings."))
      }
    )

    expect_no_warning(row <- evaluate_config_worker(1L, worker_payload_file()))
    expect_length(handed, 1L)
    expect_match(handed, "A package warned while the folds were rebuilt.", fixed = TRUE)
    expect_identical(row$warnings[[1]], "The rendered warnings.")

  })

  it("keeps a warning raised outside a configuration's unit on its row, where mirai would drop it (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)

    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("rebuild_resamples", envir = ns)
      utils::assignInNamespace("rebuild_resamples", function(...) {
        warning("A package warned while the folds were rebuilt.")
        real(...)
      }, ns = "horizons")
    })[], 30)

    ev <- with_deadline(suppressWarnings(evaluate(plsr_object(2), seed = 42L,
                                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                             output_dir = withr::local_tempdir(),
                                                                             verbose = FALSE))), 120)

    for (w in ev$evaluation$results$warnings) {
      expect_true(any(grepl("A package warned while the folds were rebuilt.", w, fixed = TRUE)))
    }

  })

  it("puts a warning a configuration's unit raised on a daemon on the log, as a sequential run does (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)

    ## On the daemons, the second configuration's recipe step raises a
    ## horizons warning inside its unit
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("build_recipe", envir = ns)
      utils::assignInNamespace("build_recipe", function(config_row, ...) {
        if (config_row$config_id == "cfg_002") {
          get("warn_in_unit", envir = ns)("substitution", "A test substitution on a daemon.")
        }
        real(config_row, ...)
      }, ns = "horizons")
    })[], 30)

    ev <- with_deadline(suppressWarnings(evaluate(plsr_object(3), seed = 42L,
                                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                             output_dir = withr::local_tempdir(),
                                                                             verbose = FALSE))), 120)

    row <- ev$log[grepl("A test substitution on a daemon.", ev$log$message, fixed = TRUE), ]

    expect_identical(nrow(row), 1L)
    expect_identical(row$topic, "substitution")
    expect_identical(row$ids[[1]], "cfg_002")

  })

  it("raises a resumed configuration's warning again on the configs axis, once, with the log row the first run wrote (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)

    ## As the test above: on the daemons, cfg_002's unit raises a horizons
    ## warning, which reaches this session on its row's records
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("build_recipe", envir = ns)
      utils::assignInNamespace("build_recipe", function(config_row, ...) {
        if (config_row$config_id == "cfg_002") {
          get("warn_in_unit", envir = ns)("substitution", "A test substitution on a daemon.")
        }
        real(config_row, ...)
      }, ns = "horizons")
    })[], 30)

    dir <- withr::local_tempdir()
    run <- function() {
      collect_warnings(with_deadline(evaluate(plsr_object(3), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE)), 120),
                       pattern = "A test substitution on a daemon.")
    }
    said <- function(ev) {
      row <- ev$log[grepl("A test substitution on a daemon.", ev$log$message, fixed = TRUE), ]
      row[setdiff(names(row), "time")]
    }

    first <- run()

    ## Every configuration checkpointed, so the second run sends none
    with_deadline(mirai::everywhere(
      utils::assignInNamespace("evaluate_single_config", function(...) stop("a configuration ran"), ns = "horizons")
    )[], 30)

    again <- run()

    expect_length(first$warnings, 1L)
    expect_length(again$warnings, 1L)
    expect_identical(said(again$value)$ids[[1]], "cfg_002")
    expect_identical(said(again$value), said(first$value))

  })

})


## On the configs axis this session writes each configuration's checkpoint as
## the window collects its row, so a write that fails stops the run as it does
## on the sequential path (test-pipeline-evaluate.R), the configurations in
## flight cancelled.

describe("evaluate() - a checkpoint the configs axis cannot write", {

  it("stops as systemic at the first checkpoint it cannot write, with the rows so far, and cancels the configurations in flight (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    store   <- file.path(dir, "checkpoints")
    marks   <- withr::local_tempdir()
    started <- withr::local_tempdir()

    ## cfg_002's checkpoint cannot be written: a directory holds its name, so
    ## the rename is refused
    dir.create(store)
    dir.create(file.path(store, "cfg_002.rds"))

    ## On the daemons, cfg_002 returns once the third configuration has begun,
    ## which the window sends once cfg_001's checkpoint is written, so the
    ## third is running when cfg_002's row comes back; the configurations
    ## after cfg_002 note that they have begun and wait, in R code a
    ## cancellation interrupts, and leave a mark only if they run to their end
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("evaluate_single_config", envir = ns)
      utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
        id <- config_row$config_id
        if (id == "cfg_002") {
          until <- Sys.time() + 30
          while (!file.exists(file.path(started, "cfg_003")) && Sys.time() < until) Sys.sleep(0.05)
        }
        if (id %in% c("cfg_003", "cfg_004")) {
          file.create(file.path(started, id))
          Sys.sleep(10)
        }
        row <- real(config_row, ...)
        file.create(file.path(marks, id))
        row
      }, ns = "horizons")
    }, started = started, marks = marks)[], 30)

    err <- expect_error(
      with_deadline(suppressWarnings(evaluate(plsr_object(4), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 120),
      class = "horizons_error_systemic"
    )

    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
    expect_match(msg, "`evaluate()` could not write to", fixed = TRUE)
    expect_match(msg, "cfg_002.rds: ", fixed = TRUE)

    ## The rows so far, the one whose checkpoint failed among them
    expect_identical(err$results$config_id, c("cfg_001", "cfg_002"))

    ## The third was cancelled before it ended, and the fourth never sent
    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_setequal(list.files(marks), c("cfg_001", "cfg_002"))

    ## cfg_001's checkpoint was written, whole, and nothing else
    expect_setequal(list.files(store, all.files = TRUE, no.. = TRUE), c("cfg_001.rds", "cfg_002.rds"))
    expect_true(dir.exists(file.path(store, "cfg_002.rds")))

    ## The payload went when evaluate() stopped
    expect_false(dir.exists(file.path(dir, "tmp")))

  })

})


## The circuit breaker on the configs axis reads the configurations in the
## order the window collects them. On the daemons a configuration named in
## `fail` fails building its recipe, the same way in each but for its id and
## a number, as the sequential tests' runner does
## (test-pipeline-evaluate.R); one named in `wait` waits first, until cfg_001
## has run to its end and a second more, so cfg_001's success comes in
## before it. cfg_001 leaves a mark once it has run.

local_breaker_daemons <- function(fail, wait = character(0), marks) {
  with_deadline(mirai::everywhere({
    ns           <- asNamespace("horizons")
    real_recipe  <- get("build_recipe", envir = ns)
    real_config  <- get("evaluate_single_config", envir = ns)
    utils::assignInNamespace("build_recipe", function(config_row, ...) {
      id <- config_row$config_id
      if (id %in% wait) {
        until <- Sys.time() + 60
        while (!file.exists(file.path(marks, "cfg_001")) && Sys.time() < until) Sys.sleep(0.05)
        Sys.sleep(1)
      }
      if (id %in% fail) stop(paste0("no recipe for ", id, " at ", nchar(id) * 7, " rows"))
      real_recipe(config_row, ...)
    }, ns = "horizons")
    utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
      row <- real_config(config_row, ...)
      if (config_row$config_id == "cfg_001") file.create(file.path(marks, "cfg_001"))
      row
    }, ns = "horizons")
  }, fail = fail, wait = wait, marks = marks)[], 30)
}

## rf and cubist in turn, so a run of failures spans two models; at 60 rows
## the cost table starts the rf configurations first
breaker_run <- function(n_configs, dir, ...) {
  with_deadline(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = n_configs), seed = 42L,
                                          control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                     output_dir = dir, verbose = FALSE, ...))), 300)
}

describe("evaluate() - the circuit breaker on the configs axis", {

  it("stops when the first configurations to finish fail the same way across two models, checkpointing none, and cancels the rest (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## rf's three start first; cubist's start as they finish, two at a time,
    ## so when the first of cubist's comes in at least one is still to come
    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    marks <- withr::local_tempdir()
    local_breaker_daemons(fail = sprintf("cfg_%03d", 1:6), marks = marks)

    err <- expect_error(breaker_run(6, dir), class = "horizons_error_systemic")

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "The first [3-5] configurations to run all failed the same way, across 2 models, so `evaluate\\(\\)` stopped rather than run the other [1-3]\\.")

    ## Those it read, both models among them (the row that tripped it may
    ## have come in with another, which is among them unread), and none
    ## checkpointed, then or once the daemons are idle
    n_read <- length(err$results$config_id)
    n_held <- as.integer(sub("^.*The first ([0-9]+) configurations.*$", "\\1", msg))
    expect_true(n_held >= 3L && n_held <= n_read && n_read < 6L)
    expect_identical(unique(err$results$status), "failed")
    expect_setequal(make_eval_object(n = 60, n_configs = 6)$config$configs$model[match(err$results$config_id, sprintf("cfg_%03d", 1:6))],
                    c("rf", "cubist"))
    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_length(list.files(file.path(dir, "checkpoints")), 0L)

  })

  it("is disarmed by a success among the first to finish, releasing the failure it held, and the run completes, each configuration checkpointed (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## cfg_003 fails at once and is held; cfg_001 succeeds and disarms the
    ## breaker; the other three fail the same way after it, which would trip
    ## a breaker still armed while cfg_004 was left to run
    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    marks <- withr::local_tempdir()
    local_breaker_daemons(fail = sprintf("cfg_%03d", 2:5), wait = sprintf("cfg_%03d", c(2L, 4L, 5L)), marks = marks)

    ev <- breaker_run(5, dir)

    expect_identical(ev$evaluation$results$status, c("success", rep("failed", 4L)))
    expect_identical(ev$evaluation$best_config, "cfg_001")
    expect_setequal(list.files(file.path(dir, "checkpoints")), sprintf("cfg_%03d.rds", 1:5))

  })

  it("runs every configuration with breaker = FALSE, checkpointing each (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    marks <- withr::local_tempdir()
    local_breaker_daemons(fail = sprintf("cfg_%03d", 1:4), marks = marks)

    err <- expect_error(breaker_run(4, dir, breaker = FALSE), class = "horizons_error_all_configs_failed")

    expect_identical(err$results$config_id, sprintf("cfg_%03d", 1:4))
    expect_setequal(list.files(file.path(dir, "checkpoints")), sprintf("cfg_%03d.rds", 1:4))

  })

  it("does not stop when nothing is left to run: the last configurations to finish failing alike end in the all-failed abort, checkpointed (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir   <- withr::local_tempdir()
    marks <- withr::local_tempdir()
    local_breaker_daemons(fail = sprintf("cfg_%03d", 1:3), marks = marks)

    err <- expect_error(breaker_run(3, dir), class = "horizons_error_all_configs_failed")

    expect_false(inherits(err, "horizons_error_systemic"))
    expect_identical(err$results$config_id, sprintf("cfg_%03d", 1:3))
    expect_setequal(list.files(file.path(dir, "checkpoints")), sprintf("cfg_%03d.rds", 1:3))

  })

  ## The grid of test-pipeline-evaluate.R's twins (make_breaker_grid_object()),
  ## CARS's configurations costed first, so the first to finish are CARS's,
  ## across the three models
  grid_run <- function(dir) {
    local_mocked_bindings(config_costs = function(configs, n_rows) {
      ifelse(configs$feature_selection == "cars", 2, 1)
    }, .package = "horizons")
    with_deadline(suppressWarnings(evaluate(make_breaker_grid_object(), prune = FALSE, seed = 42L,
                                            control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                       output_dir = dir, verbose = FALSE))), 300)
  }

  it("runs the grid when CARS's fold preparation fails, though CARS's configurations finish first: they fail, the rest are scored, and nothing stops as systemic (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("bonsai")
    skip_if_not_installed("lightgbm")

    local_test_daemons(2)
    with_deadline(mirai::everywhere({
      utils::assignInNamespace("cars_folds", function(y, k) stop("CARS found no fold to score its subsets on"),
                               ns = "horizons")
    })[], 30)

    dir  <- withr::local_tempdir()
    ev   <- grid_run(dir)
    rows <- ev$evaluation$results
    cars <- endsWith(rows$config_id, "_cars")

    expect_identical(rows$status[cars], rep("failed", 6L))
    expect_length(unique(rows$error_message[cars]), 1L)
    expect_match(rows$error_message[cars][1], "CARS found no fold to score its subsets on", fixed = TRUE)
    expect_identical(rows$status[!cars], rep("success", 6L))
    expect_length(list.files(file.path(dir, "checkpoints")), 12L)

  })

  it("stops a failure every configuration shares, once the first to finish span every part the grid varies, cancelling the rest (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("bonsai")
    skip_if_not_installed("lightgbm")

    ## Every configuration's own recipe fails, the same way but for its id;
    ## the fold preparations are built as ever
    local_test_daemons(2)
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("build_recipe", envir = ns)
      utils::assignInNamespace("build_recipe", function(config_row, ..., transformed = FALSE) {
        if (isTRUE(transformed)) return(real(config_row, ..., transformed = transformed))
        stop(paste0("no recipe for ", config_row$config_id, " at ", nchar(config_row$config_id) * 7, " rows"))
      }, ns = "horizons")
    })[], 30)

    dir <- withr::local_tempdir()
    err <- expect_error(grid_run(dir), class = "horizons_error_systemic")

    got <- make_breaker_grid_object()$config$configs
    got <- got[match(err$results$config_id, got$config_id), ]
    expect_identical(unique(err$results$status), "failed")
    expect_gte(nrow(got), 3L)
    expect_lt(nrow(got), 12L)
    for (part in c("model", "preprocessing", "feature_selection")) expect_gte(length(unique(got[[part]])), 2L)
    expect_match(gsub("\\s+", " ", conditionMessage(err)), "preprocessing methods and 2 feature selections, so `evaluate()` stopped",
                 fixed = TRUE)
    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_length(list.files(file.path(dir, "checkpoints")), 0L)

  })

})

## =========================================================================
## Inner daemons: a long configuration's folds on daemons of its own
## =========================================================================
## The plan is mocked in this session, where evaluate() makes it, so a small
## grid splits as a long one would. A split configuration takes as many of
## the window's slots as its inner daemons, so on two test daemons it runs
## alone, its daemon waiting in tune while its two inner daemons fit the
## folds.

## The plan with the configurations at `which` (in grid order) split onto
## `inner` daemons, until the calling frame exits
local_split <- function(which, inner = 2L, .env = parent.frame()) {
  testthat::local_mocked_bindings(split_plan = function(costs, n, folds) {
    plan <- rep(1L, length(costs))
    plan[which] <- as.integer(inner)
    plan
  }, .package = "horizons", .env = .env)
}

## Whether any of the user's daemons has a profile of its own set, read with
## one task on each
profiles_left <- function() {
  unlist(with_deadline(mirai::everywhere(mirai::daemons_set())[], 30))
}

## rf, plsr and cubist, so a seeded engine, a submodel engine and an isolated
## fit's chain all run on inner daemons, with a Bayesian search. On this
## data only rf's row moves with its stages' seeds: plsr and cubist choose
## and score the same whatever the seed, so a split that changed a seed
## would show in rf's row alone.
inner_object <- function() {
  obj <- make_eval_object(n = 60, n_configs = 3)
  obj$config$configs$model <- c("rf", "plsr", "cubist")
  obj$config$tuning$bayesian_iter <- 2L
  obj
}

## A worker's payload for one plsr configuration on 200 rows of 200
## predictors, three folds: large enough that BLAS on two threads rounds its
## fits differently from one
blas_payload <- function() {

  obj <- make_eval_object(n = 200, n_wn = 200, n_configs = 1)
  obj$config$configs$model <- "plsr"
  set.seed(1)
  split <- rsample::initial_split(obj$data$analysis, prop = 0.8)
  folds <- rsample::vfold_cv(rsample::training(split), v = 3)

  shared <- worker_payload()
  shared$data         <- split$data
  shared$resample_idx <- resample_indices(split, folds)
  shared$configs      <- obj$config$configs
  shared$role_map     <- obj$data$role_map
  shared$data_fp      <- eval_data_fingerprint(rsample::training(split), obj$data$role_map)
  shared

}

describe("evaluate() - inner daemons", {

  it("sends every task one daemon when inner_daemons = FALSE, though the plan would split", {

    ## Mocked as the task-shape test above; the first task's daemons are
    ## read as it is sent
    sent <- NULL
    local_split(1:4)
    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE
    )
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      sent <<- .args$inner
      rlang::abort("stop", class = "dispatched")
    }, .package = "mirai")

    run <- function(inner_daemons) {
      tryCatch(
        suppressWarnings(evaluate(make_eval_object(n_configs = 4), seed = 42L,
                                  control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                             output_dir = withr::local_tempdir(), verbose = FALSE,
                                                             inner_daemons = inner_daemons))),
        dispatched = function(e) NULL
      )
      sent
    }

    expect_identical(run(TRUE), 2L)
    expect_identical(run(FALSE), 1L)

  })

  it("resumes checkpoints written with inner daemons on in a run with them off, since the results do not depend on them", {

    dir <- withr::local_tempdir()
    obj <- make_eval_object(n_configs = 2)

    first  <- suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = dir, verbose = FALSE)))
    second <- suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = dir, verbose = FALSE,
                                                                                    inner_daemons = FALSE)))

    expect_identical(second$evaluation$results$runtime_secs, first$evaluation$results$runtime_secs)

  })

  it("runs a configuration whose inner daemons did not connect on its own daemon, and says so on its row", {

    local_mocked_bindings(start_inner_daemons = function(...) FALSE)
    path <- worker_payload_file()

    row <- evaluate_config_worker(1L, path, inner = 2L)

    expect_identical(row$status, "success")
    expect_identical(attr(row, "inner_daemons"), list(daemons = 2L, started = FALSE, completed = 0L))
    expect_false(mirai::daemons_set())

  })

  it("fits a split configuration's folds on its inner daemons, and an unsplit one's on its own (installed build)", {

    skip_on_cran()
    skip_if_dev_package()

    ## The worker in this session, which stands in for its daemon
    mirai::daemons(0)
    path <- worker_payload_file()

    split   <- with_deadline(evaluate_config_worker(1L, path, inner = 2L), 120)
    unsplit <- with_deadline(evaluate_config_worker(1L, path, inner = 1L), 120)

    ## Three folds a call, the grid's at least, and only its folds: tune
    ## sends a call's folds together, so the count is a multiple of three,
    ## which the inner daemons' two set-up tasks would not leave it
    expect_true(attr(split, "inner_daemons")$started)
    expect_gte(attr(split, "inner_daemons")$completed, 3L)
    expect_identical(attr(split, "inner_daemons")$completed %% 3L, 0L)
    expect_identical(attr(unsplit, "inner_daemons"), list(daemons = 1L, started = FALSE, completed = 0L))
    expect_false(mirai::daemons_set())

    cols <- setdiff(names(split), "runtime_secs")
    attr(split, "inner_daemons") <- attr(unsplit, "inner_daemons") <- NULL
    expect_identical(split[cols], unsplit[cols])

  })

  it("runs a configuration's daemon on one BLAS thread, as its inner daemons run, so its row is the same split or not where the daemon started on more, and puts its threads back (installed build)", {

    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("RhpcBLASctl")

    ## The worker in this session, which stands in for a daemon the user
    ## started with BLAS on two threads. At 200 x 200 two threads round
    ## plsr's fits differently from one; at a tenth of that they do not.
    mirai::daemons(0)
    old <- RhpcBLASctl::blas_get_num_procs()
    withr::defer(RhpcBLASctl::blas_set_num_threads(old))
    RhpcBLASctl::blas_set_num_threads(2L)
    skip_if_not(RhpcBLASctl::blas_get_num_procs() == 2, "BLAS here runs on one thread whatever is asked")

    path <- worker_payload_file(blas_payload())

    split   <- with_deadline(evaluate_config_worker(1L, path, inner = 2L), 120)
    after   <- RhpcBLASctl::blas_get_num_procs()
    unsplit <- with_deadline(evaluate_config_worker(1L, path, inner = 1L), 120)

    expect_true(attr(split, "inner_daemons")$started)
    expect_identical(split$status, "success")
    expect_equal(c(after, RhpcBLASctl::blas_get_num_procs()), c(2, 2))

    cols <- setdiff(names(split), "runtime_secs")
    attr(split, "inner_daemons") <- attr(unsplit, "inner_daemons") <- NULL
    expect_identical(split[cols], unsplit[cols])

  })

  it("sends tune's fold tasks a split each and little else, so a split configuration's daemon holds its folds (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)

    ## What tune ships with each call's fold tasks, the shared arguments
    ## apart from the folds: read in tune's own call to mirai, then sent on
    sizes <- new.env(parent = emptyenv())
    real  <- get("eval_mirai", envir = asNamespace("tune"))
    local_mocked_bindings(eval_mirai = function(.x, .f, ..., .args) {
      shared <- lapply(.args, get, envir = parent.frame())
      sizes$shared <- c(sizes$shared, length(serialize(list(.f, shared), NULL)))
      sizes$split  <- c(sizes$split, max(vapply(.x, function(s) length(serialize(s, NULL)), numeric(1))))
      res <- mirai::mirai_map(.x, .f, ..., .args = shared)
      mirai::collect_mirai(res)
    }, .package = "tune")

    ## Enough data that a frame carried with the tasks would show
    obj   <- make_eval_object(n = 400, n_wn = 400, n_configs = 1)
    set.seed(1)
    split <- rsample::initial_split(obj$data$analysis, prop = 0.8)
    folds <- rsample::vfold_cv(rsample::training(split), v = 3)

    row <- with_deadline(evaluate_single_config(obj$config$configs[1, ], split, folds, obj$data$role_map,
                                                grid_size = 2L, bayesian_iter = 0L, allow_par = TRUE, seed = 42L), 120)

    expect_identical(row$status, "success")
    expect_gt(length(sizes$shared), 0L)
    expect_lt(max(sizes$shared), min(sizes$split) / 2)

  })

  it("gives the same results with a configuration split as without, and leaves no profile on the user's daemons (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## Every configuration split onto both daemons, so each runs alone on
    ## two inner daemons
    local_test_daemons(2)
    local_split(1:3)

    run <- function(inner_daemons, dir) {
      with_deadline(suppressWarnings(evaluate(inner_object(), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE,
                                                                         inner_daemons = inner_daemons))), 300)
    }

    split   <- run(TRUE, withr::local_tempdir())
    unsplit <- run(FALSE, withr::local_tempdir())

    ## The split engaged: every configuration's inner daemons connected and
    ## ran its folds
    note <- split$log[grepl("on inner daemons it started", split$log$message, fixed = TRUE), ]
    expect_identical(note$ids[[1]], c("cfg_001", "cfg_002", "cfg_003"))
    expect_false(any(grepl("did not connect", split$log$message, fixed = TRUE)))

    cols <- setdiff(names(split$evaluation$results), "runtime_secs")
    expect_identical(split$evaluation$results[cols], unsplit$evaluation$results[cols])
    expect_identical(split$evaluation$best_config, unsplit$evaluation$best_config)

    expect_false(any(profiles_left()))
    expect_identical(connected_daemons(), 2L)

  })

  it("resets a split configuration's inner daemons when the configuration's task fails (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    local_split(1L)

    ## On the daemons, the split configuration fails as a bug would, once
    ## its inner daemons are up, saying how many its daemon had connected
    with_deadline(mirai::everywhere({
      utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
        stop(sprintf("a bug in the configuration's code, on %d inner daemons",
                     as.integer(sum(mirai::info()[["connections"]]))))
      }, ns = "horizons")
    })[], 30)

    expect_error(
      with_deadline(suppressWarnings(evaluate(plsr_object(2), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = withr::local_tempdir(),
                                                                         verbose = FALSE))), 120),
      "a bug in the configuration's code, on 2 inner daemons", fixed = TRUE
    )

    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_false(any(profiles_left()))
    expect_identical(connected_daemons(), 2L)

  })

  it("cancels a split configuration in flight when it is interrupted, ending its inner daemons, and writes nothing after it returns (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(3)
    dir     <- withr::local_tempdir()
    store   <- file.path(dir, "checkpoints")
    signals <- withr::local_tempdir()

    ## The second configuration is split onto two inner daemons and, once
    ## they are up, writes how many its daemon has connected and waits in R
    ## code a cancellation interrupts; the first runs once it is waiting, so
    ## the interrupt, as the window's first wait after the first's checkpoint
    ## begins, finds the split one in flight
    local_split(2L)
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("evaluate_single_config", envir = ns)
      utils::assignInNamespace("evaluate_single_config", function(config_row, ...) {
        until <- Sys.time() + 30
        if (config_row$config_id == "cfg_002") {
          writeLines(as.character(sum(mirai::info()[["connections"]])), file.path(signals, "cfg_002-waiting"))
          Sys.sleep(10)
        }
        if (config_row$config_id == "cfg_001") {
          while (!file.exists(file.path(signals, "cfg_002-waiting")) && Sys.time() < until) Sys.sleep(0.05)
        }
        real(config_row, ...)
      }, ns = "horizons")
    }, signals = signals)[], 30)

    local_stop_after_first(store, rlang::interrupt)

    got <- tryCatch(
      with_deadline(suppressWarnings(evaluate(plsr_object(3), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 60),
      interrupt = function(i) "interrupted"
    )

    expect_identical(got, "interrupted")

    ## The split configuration was in flight on its two inner daemons
    expect_identical(readLines(file.path(signals, "cfg_002-waiting")), "2")

    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_identical(list.files(store), "cfg_001.rds")
    expect_false(any(profiles_left()))
    expect_identical(connected_daemons(), 3L)

  })

})

## =========================================================================
## A package missing on a worker
## =========================================================================
## evaluate() checks the configurations' packages in this session, and on
## every daemon at the door, so a run whose daemons lack one is refused before
## anything is drawn. A daemon whose library loses one after the door fails
## every configuration that needs it: on the configs axis tune runs inside the
## worker and refuses at its own door, by text, the worker's error comes back
## from the daemon with its class, and the run stops as systemic, with the
## rows so far. Read here from real daemons whose library hides kernlab.

## A library holding every package this session sees but `hidden`, and the
## library variables pointing at it, for daemons started in the test.
## Daemons are new Rscript processes, which take their libraries from these
## variables, not from this session's .libPaths(). horizons itself is not
## linked: the daemons load it from its own library, listed after the
## temporary one, because covr records the path each process loaded the
## package from and reads it once the tests are over, when the temporary
## library is gone. That library must not hold `hidden` too.
local_hidden_package_lib <- function(hidden, .env = parent.frame()) {

  horizons_lib <- dirname(find.package("horizons"))

  if (file.exists(file.path(horizons_lib, hidden))) {
    testthat::skip(paste0(hidden, " is installed beside horizons, so the daemons cannot be kept from it"))
  }

  dir <- withr::local_tempdir(.local_envir = .env)

  for (lib in .libPaths()) {
    for (pkg in list.files(lib)) {
      target <- file.path(dir, pkg)
      if (!pkg %in% c(hidden, "horizons") && !file.exists(target)) file.symlink(file.path(lib, pkg), target)
    }
  }

  libs <- paste(dir, horizons_lib, sep = .Platform$path.sep)
  withr::local_envvar(R_LIBS = libs, R_LIBS_USER = dir, R_LIBS_SITE = dir, .local_envir = .env)

  dir

}

## A plsr and an svm_rbf configuration, the second needing kernlab
kernlab_object <- function() {
  obj <- make_eval_object(n = 60, n_configs = 2)
  obj$config$configs$model <- c("plsr", "svm_rbf")
  obj
}

describe("evaluate() - a package missing on a worker", {

  it("refuses a run whose daemons lack a package this session has, on either axis, before anything is drawn (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("kernlab")

    local_hidden_package_lib("kernlab")
    local_test_daemons(2)
    local_mocked_bindings(draw_eval_split = function(...) stop("the split was drawn"))

    for (over in c("resamples", "configs")) {

      err <- expect_error(
        with_deadline(suppressWarnings(evaluate(kernlab_object(), seed = 42L,
                                                control = control_horizons(allow_par = TRUE, parallelize_over = over,
                                                                           output_dir = withr::local_tempdir()))), 60),
        class = "horizons_error_missing_package"
      )

      msg <- gsub("\\s+", " ", conditionMessage(err))
      expect_match(msg, "2 of the 2 mirai daemons checked cannot load 1 package `evaluate()` needs.", fixed = TRUE,
                   info = over)
      expect_match(msg, "kernlab, needed for svm_rbf", fixed = TRUE, info = over)

    }

  })

  it("stops a configs-axis run as systemic, with the worker's class, when a daemon lacks a package after the door (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("kernlab")

    local_hidden_package_lib("kernlab")
    local_test_daemons(2)

    ## The fixture reaches the state under test: a daemon runs this horizons
    ## without kernlab, which this session has
    expect_false(with_deadline(mirai::mirai(requireNamespace("kernlab", quietly = TRUE))[], 30))
    expect_identical(with_deadline(mirai::mirai(as.character(utils::packageVersion("horizons")))[], 30),
                     as.character(utils::packageVersion("horizons")))

    ## The door's check reports kernlab present, as it was before the
    ## daemon's library lost it
    local_mocked_bindings(daemon_reports = matching_daemons)

    ## tune's own refusal in the worker, its class kept
    err <- expect_error(
      with_deadline(suppressWarnings(evaluate(kernlab_object(), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = withr::local_tempdir()))), 120),
      class = "horizons_error_systemic"
    )

    expect_s3_class(err$parent, "horizons_error_missing_package")
    expect_match(gsub("\\s+", " ", conditionMessage(err)), "is required for kernlab", fixed = TRUE)

  })

})


## =========================================================================
## Cross-mode resume
## =========================================================================

## One sequential four-config run with an output_dir, at the defaults
## (prune = TRUE, which at bayesian_iter = 0 skips nothing). The resume test
## below and the two monitor tests further down each get a private copy of
## its directory to resume, read or doctor (helper-memo.R), and the object
## it was run on.
seq_checkpoints <- function(.env = parent.frame()) {
  memo_dir("seq_checkpoints", build_seq_checkpoints, .env = .env)
}

build_seq_checkpoints <- function(dir) {

  obj    <- make_eval_object(n = 60, n_configs = 4)
  result <- suppressWarnings(
    evaluate(obj, seed = 42L, control = control_horizons(output_dir = dir))
  )

  list(obj = obj, result = result)

}

describe("evaluate() - cross-mode checkpoint resume", {

  it("resumes a configs-axis run from sequential checkpoints", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    run        <- seq_checkpoints()
    obj        <- run$value$obj
    tmpdir     <- run$dir
    seq_result <- run$value$result

    ## Both axes share the one store (#42): the per-config files.
    expect_setequal(list.files(tmpdir), c("checkpoints", "eval_manifest.rds"))

    local_test_daemons(2)

    par_result <- with_deadline(suppressWarnings(
      evaluate(obj,
               seed = 42L, control = control_horizons(allow_par = TRUE, output_dir = tmpdir))
    ), 120)

    expect_equal(seq_result$evaluation$best_config,
                 par_result$evaluation$best_config)
    expect_equal(nrow(par_result$evaluation$results), 4)

    ## Every row was resumed, none re-run: the runtimes are the checkpointed
    ## ones.
    by_id <- function(r) r$runtime_secs[order(r$config_id)]
    expect_equal(by_id(par_result$evaluation$results),
                 by_id(seq_result$evaluation$results))

  })

  it("records the daemons it began with when the check at the door spends a maxtasks pool", {

    ## Two daemons set with maxtasks = 1, each of which exits once the check
    ## has spent its task; every configuration resumed, so nothing is sent
    ## after it. The daemons are mocked, so this runs under load_all() too.
    run   <- seq_checkpoints()
    spent <- FALSE

    local_mocked_bindings(
      running_under_load_all = function() FALSE,
      check_parallel_backend = function(...) TRUE,
      connected_daemons      = function() if (spent) 0L else 2L,
      daemons_dispatch       = function() TRUE,
      daemon_reports         = function(...) {
        spent <<- TRUE
        daemons_on()
      }
    )

    ev <- suppressWarnings(
      evaluate(run$value$obj, seed = 42L,
               control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                          output_dir = run$dir, verbose = FALSE))
    )

    expect_s3_class(ev, "horizons_eval")
    expect_identical(ev$evaluation$workers, 2L)
    expect_identical(readRDS(file.path(run$dir, "eval_manifest.rds"))$workers, 2L)

  })

})


## =========================================================================
## monitor_evaluate()
## =========================================================================

describe("monitor_evaluate()", {

  it("renders every line of its display", {

    ## Every optional line is present: ignored and unreadable checkpoints, a
    ## rate, an ETA, the best config and recent completions. The clock and
    ## the screen-clearing form feed are scrubbed.
    manifest <- list(
      axis = "configs", plan = "mirai", workers = 4L,
      data_n_rows = 120, data_fields = list(outcome = "SOC"),
      data_hash = "0123456789abcdef0123",
      settings = list(grid_size = 10L, outcome_range = c(-Inf, Inf)),
      metric = "rpd"
    )
    stats <- list(
      n_complete = 3, n_total = 4, pct = 75,
      ignored = c(other_data = 1, other_settings = 0, earlier_schema = 2, not_in_grid = 0),
      unreadable = "cfg_009.rds", rate = 12.34, eta = "5 min",
      best_config = "cfg_002", best_metric = 2.3456,
      recent = c("cfg_002 RPD = 2.346", "cfg_003 failed")
    )

    expect_snapshot(
      .render_monitor(stats, manifest),
      transform = function(x) sub("monitor \u2014 [0-9:]+", "monitor \u2014 <time>", gsub("\014", "", x, fixed = TRUE))
    )

  })

  it("errors on missing directory", {

    err <- expect_error(monitor_evaluate("/nonexistent/path"), "does not exist",
                        class = "horizons_error_input")
    expect_match(conditionMessage(err), "control_horizons(output_dir = )", fixed = TRUE)
    expect_identical(rlang::call_name(err$call), "monitor_evaluate")

    expect_error(monitor_evaluate(c("a", "b")), "output_dir", class = "horizons_error_input")

  })

  it("errors on missing manifest", {

    tmpdir <- withr::local_tempdir()
    err <- expect_error(monitor_evaluate(tmpdir), "eval_manifest", class = "horizons_error_input")
    expect_identical(rlang::call_name(err$call), "monitor_evaluate")

  })

  it("reads progress, the plan and the training data from the manifest", {

    skip_on_cran()
    run    <- seq_checkpoints()
    tmpdir <- run$dir

    ## The sequential run's manifest says it ran on no daemons
    expect_true(any(grepl("Parallel:  none (sequential)", capture.output(monitor_evaluate(tmpdir)), fixed = TRUE)))

    ## Then made to describe a configs-axis run on ten daemons with one
    ## config still to finish.
    path     <- file.path(tmpdir, "eval_manifest.rds")
    manifest <- readRDS(path)
    manifest$axis    <- "configs"
    manifest$plan    <- "mirai"
    manifest$workers <- 10L
    saveRDS(manifest, path)

    files <- sort(list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE))
    unlink(files[length(files)])

    output <- capture.output(result <- monitor_evaluate(tmpdir))

    expect_equal(result$n_complete, 3)
    expect_equal(result$n_total, 4)
    expect_false(is.na(result$best_config))
    expect_true(any(grepl("over configs on mirai (10 daemons)", output, fixed = TRUE)))

    ## Two runs sharing one output_dir are otherwise indistinguishable here.
    expect_true(any(grepl(paste0(manifest$data_n_rows, " training rows of SOC"),
                          output, fixed = TRUE)))
    expect_true(any(grepl(substr(manifest$data_hash, 1, 12), output, fixed = TRUE)))

  })

  it("reports the share done, the rate, the ETA weighted by each config's cost, the best value and the recent completions, and no rate or ETA it cannot know", {

    skip_on_cran()
    run    <- seq_checkpoints()
    tmpdir <- run$dir
    res    <- run$value$result$evaluation$results
    ckpt   <- file.path(tmpdir, "checkpoints")
    path   <- file.path(tmpdir, "eval_manifest.rds")

    ## The ETA counts each config by its cost in the manifest, set here so
    ## the arithmetic is plain: cfg_004 costs twice each of the others
    manifest <- readRDS(path)
    expect_identical(manifest$config_ids, c("cfg_001", "cfg_002", "cfg_003", "cfg_004"))
    manifest$config_cost <- c(1, 1, 1, 2)
    saveRDS(manifest, path)

    snapshot_at <- function(start_time) {
      manifest <- readRDS(path)
      manifest$start_time <- start_time
      saveRDS(manifest, path)
      invisible(utils::capture.output(stats <- monitor_evaluate(tmpdir)))
      stats
    }

    cv_text <- function(id) paste0(id, " RPD = ", round(res$cv_rpd[res$config_id == id], 3))

    ## A start in the future, as a monitor whose clock runs behind the
    ## machine running evaluate() reads it: no rate and no ETA
    ahead <- snapshot_at(Sys.time() + 3600)

    expect_identical(ahead$n_complete, 4L)
    expect_identical(ahead$rate, NA_real_)
    expect_identical(ahead$eta, NA_character_)

    ## Started an hour ago, with three of the four configs done and one of
    ## them failed: three units of cost an hour, and cfg_004's two left, so
    ## 40 minutes where a count would say 20. Under a second passes between
    ## writing the start and reading it; the rate stays within 1% of 3 for
    ## half a minute, and the ETAs below round as expected for 45 seconds.
    unlink(file.path(ckpt, "cfg_004.rds"))
    failed <- readRDS(file.path(ckpt, "cfg_002.rds"))
    failed$status <- "failed"
    failed[paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))] <- NA_real_
    saveRDS(failed, file.path(ckpt, "cfg_002.rds"))

    three <- snapshot_at(Sys.time() - 3600)
    best  <- rank_configs_by_cv(res[res$config_id %in% c("cfg_001", "cfg_003"), ], "rpd")

    expect_identical(three$pct, 75)
    expect_equal(three$rate, 3, tolerance = 0.01)
    expect_identical(three$eta, "40 min")
    expect_identical(three$best_config, best$config_id[1])
    expect_identical(three$best_metric, best$cv_rpd[1])
    expect_setequal(three$recent, c(cv_text("cfg_001"), "cfg_002 failed", cv_text("cfg_003")))

    ## Started half an hour ago: still a rate per hour
    half <- snapshot_at(Sys.time() - 1800)

    expect_equal(half$rate, 6, tolerance = 0.01)
    expect_identical(half$eta, "20 min")

    ## One done, three to go, so the ETA is in hours: one unit of cost an
    ## hour and four left, where a count would say 3 hr; the failed config
    ## has no value to rank
    unlink(file.path(ckpt, c("cfg_001.rds", "cfg_003.rds")))

    one <- snapshot_at(Sys.time() - 3600)

    expect_identical(one$eta, "4 hr")
    expect_identical(one$best_config, NA_character_)
    expect_identical(one$best_metric, NA_real_)
    expect_identical(one$recent, "cfg_002 failed")

    ## Nothing done: a rate of zero gives no ETA
    unlink(file.path(ckpt, "cfg_002.rds"))

    none <- snapshot_at(Sys.time() - 3600)

    expect_identical(none$rate, 0)
    expect_identical(none$eta, NA_character_)
    expect_identical(none$recent, character(0))

  })

  it("refuses a manifest of another schema, pointing at a new run", {

    tmpdir <- withr::local_tempdir()
    saveRDS(list(schema_version = 3L, n_total = 10, metric = "rpd"),
            file.path(tmpdir, "eval_manifest.rds"))

    err <- expect_error(monitor_evaluate(tmpdir),
                        "was written by another version of horizons", fixed = TRUE,
                        class = "horizons_error_validation")
    expect_match(conditionMessage(err), "rewrites it when a run starts in this directory", fixed = TRUE)

  })

})


## =========================================================================
## Results do not depend on the axis (review finding, 2026-09-15)
## =========================================================================
## tune's parallel path advances the parent RNG stream differently from the
## sequential loop, so when the stages shared one stream the Bayesian stage
## and last_fit() drew from a different position on the resamples axis. Each
## stage is now a unit of its own (with_unit_seed()), seeded on entry. This
## fixture uses rf (deterministic given a seed) with Bayesian iterations ON,
## where the axes diverged on the tune of that time, and asserts the whole
## result row is identical, so it pins the result, not the mechanism.
## The sequential run both tests compare against is built once, by whichever
## of them runs first (helper-memo.R), so a skipped test costs nothing.

axes_seq_run <- function() memo_fixture("axes_seq_run", build_axes_seq_run)

build_axes_seq_run <- function() {

  obj <- make_eval_object(n = 60, n_configs = 1)      # rf only
  obj$config$tuning$bayesian_iter <- 2L

  result <- suppressWarnings(
    evaluate(obj, seed = 42L, control = control_horizons(allow_par = FALSE))
  )

  list(obj = obj, result = result)

}

describe("evaluate() - results are identical across axes", {

  row_cols <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                "cv_rmse", "cv_rrmse", "cv_rsq", "cv_ccc", "cv_rpd", "cv_mae")

  it("resamples axis, chosen by auto on two daemons, matches the sequential run exactly (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    run        <- axes_seq_run()
    obj        <- run$obj
    seq_result <- run$result
    local_test_daemons(2)

    ## One config against three folds: auto resolves to the resamples axis,
    ## which needs no output_dir
    par_result <- with_deadline(suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE))
    ), 120)

    expect_equal(par_result$evaluation$parallelize_over, "resamples")
    expect_identical(par_result$evaluation$workers, 2L)
    expect_equal(par_result$evaluation$results[row_cols],
                 seq_result$evaluation$results[row_cols])
    expect_equal(par_result$evaluation$results$best_params,
                 seq_result$evaluation$results$best_params)

  })

  it("configs axis matches the sequential run exactly (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    run        <- axes_seq_run()
    obj        <- run$obj
    seq_result <- run$result
    local_test_daemons(2)
    tmpdir <- withr::local_tempdir()

    par_result <- with_deadline(suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE, parallelize_over = "configs", output_dir = tmpdir))
    ), 120)

    ## One config against three folds: "auto" would pick resamples
    expect_equal(par_result$evaluation$parallelize_over, "configs")
    expect_equal(par_result$evaluation$results[row_cols],
                 seq_result$evaluation$results[row_cols])
    expect_equal(par_result$evaluation$results$best_params,
                 seq_result$evaluation$results$best_params)

  })

})


## =========================================================================
## The recipe settings reach a worker in another process (#62)
## =========================================================================
## The in-process test above shows the worker forwards the settings; this one
## shows they survive the trip to a daemon and change what it computes. On "raw" a window of 13 trims 12 of the 20 columns rather than 8,
## so the configured run and the default run model different predictors. plsr
## reads every predictor it keeps, where a tuned rf on this little data can
## come out predicting the mean whatever its predictors.

describe("evaluate() - recipe settings on the configs axis", {

  it("runs configure()'s window in the worker, as the sequential run does (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    row_cols <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                  "cv_rmse", "cv_rrmse", "cv_rsq", "cv_ccc", "cv_rpd", "cv_mae")

    obj <- make_eval_object(n = 60, n_wn = 20, n_configs = 1)
    obj$config$configs$model <- "plsr"
    obj$config$recipe <- list(sg_window = 13L, pca_threshold = 0.995)

    seq_result <- suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = FALSE))
    )

    default_obj <- obj
    default_obj$config$recipe <- list(sg_window     = DEFAULT_SG_WINDOW,
                                      pca_threshold = DEFAULT_PCA_THRESHOLD)

    seq_default <- suppressWarnings(
      evaluate(default_obj, seed = 42L, control = control_horizons(allow_par = FALSE))
    )

    local_test_daemons(2)
    tmpdir <- withr::local_tempdir()

    par_result <- with_deadline(suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE, parallelize_over = "configs", output_dir = tmpdir))
    ), 120)

    ## One config against three folds: "auto" would pick resamples
    expect_equal(par_result$evaluation$parallelize_over, "configs")
    expect_equal(par_result$evaluation$results[row_cols],
                 seq_result$evaluation$results[row_cols])
    expect_false(isTRUE(all.equal(seq_result$evaluation$results$cv_rmse,
                                  seq_default$evaluation$results$cv_rmse)))

  })

})


## =========================================================================
## monitor_evaluate() names the same best config evaluate() does
## =========================================================================

describe("monitor_evaluate() - agrees with evaluate()", {

  it("reports evaluate()'s best_config from the same checkpoints", {

    skip_on_cran()
    run    <- seq_checkpoints()
    tmpdir <- run$dir
    result <- run$value$result

    ## The manifest is written on every run with an output_dir now
    expect_true(file.exists(file.path(tmpdir, "eval_manifest.rds")))

    invisible(capture.output(stats <- monitor_evaluate(tmpdir)))

    expect_equal(stats$best_config, result$evaluation$best_config)

  })

})


## =========================================================================
## monitor_evaluate() applies evaluate()'s checkpoint gates (#42)
## =========================================================================

describe("monitor_evaluate() - applies evaluate()'s checkpoint gates", {

  it("neither counts nor ranks a row evaluate() would reject", {

    skip_on_cran()
    run    <- seq_checkpoints()
    tmpdir <- run$dir
    result <- run$value$result

    res <- result$evaluation$results
    expect_equal(res$status[res$config_id == "cfg_003"], "success")

    ckpt <- file.path(tmpdir, "checkpoints")
    doctor <- function(id, edit) {
      f <- file.path(ckpt, paste0(id, ".rds"))
      saveRDS(edit(readRDS(f)), f)
    }

    ## Each rejected row is made to look like the winner.
    doctor("cfg_001", function(r) {        # scored on other training rows
      r$data_hash <- "0000deadbeef"
      r$data_fields[[1]]$ids <- "0000deadbeef"
      r$cv_rpd    <- 999
      r
    })
    doctor("cfg_002", function(r) {        # scored under an earlier schema
      r$scoring_schema <- 1L
      r$cv_rpd         <- 998
      r
    })
    doctor("cfg_004", function(r) {        # tuned under other settings
      s <- r$settings[[1]]
      s$grid_size <- 99
      r$settings  <- list(s)
      r$cv_rpd    <- 997
      r
    })

    ## A config no longer in the grid, and a file that is not a checkpoint.
    stale <- readRDS(file.path(ckpt, "cfg_003.rds"))
    stale$config_id <- "cfg_999"
    stale$cv_rpd    <- 996
    saveRDS(stale, file.path(ckpt, "cfg_999.rds"))
    writeLines("not an rds file", file.path(ckpt, "cfg_005.rds"))

    output <- capture.output(stats <- monitor_evaluate(tmpdir))

    expect_equal(stats$n_complete, 1)
    expect_equal(stats$best_config, "cfg_003")
    expect_true(any(grepl("cfg_005.rds", stats$unreadable, fixed = TRUE)))

    ## Refused rows are shown, not hidden.
    expect_equal(sum(stats$ignored), 4)
    expect_true(any(grepl("Ignored", output)))
    expect_true(any(grepl("grid_size", output)))

    ## Each under its own reason: without the settings mismatch, the row
    ## scored on other training rows still counts as other data
    expect_equal(stats$ignored,
                 c(other_data = 1, other_settings = 1, earlier_schema = 1, not_in_grid = 1))

    unlink(file.path(ckpt, "cfg_004.rds"))
    invisible(capture.output(stats <- monitor_evaluate(tmpdir)))

    expect_equal(stats$ignored,
                 c(other_data = 1, other_settings = 0, earlier_schema = 1, not_in_grid = 1))

  })

  it("re-reads the manifest on every poll in watch mode", {

    skip_on_cran()
    run    <- seq_checkpoints()
    tmpdir <- run$dir
    path   <- file.path(tmpdir, "eval_manifest.rds")

    ## The rows were tuned by the second run; the manifest is first the one
    ## an earlier run with another grid_size wrote.
    second_run <- readRDS(path)
    first_run  <- second_run
    first_run$settings$grid_size <- first_run$settings$grid_size + 1
    saveRDS(first_run, path)

    ## After the first poll, the second run starts and rewrites the manifest.
    ## A monitor still holding the first manifest ignores the rows forever;
    ## the guard turns that hang into a failure.
    polls <- 0L

    local_mocked_bindings(.render_monitor = function(stats, manifest) {

      polls <<- polls + 1L

      if (polls == 1L) saveRDS(second_run, path)

      if (polls > 3L) stop("still gating against the first run's manifest")

    })

    invisible(capture.output(
      stats <- monitor_evaluate(tmpdir, watch = TRUE, interval = 0)
    ))

    expect_equal(stats$n_complete, second_run$n_total)
    expect_equal(polls, 2L)

  })

  it("waits the interval between polls in watch mode", {

    tmpdir <- withr::local_tempdir()
    saveRDS(list(schema_version = EVAL_MANIFEST_SCHEMA, n_total = 2L),
            file.path(tmpdir, "eval_manifest.rds"))

    ## Incomplete at the first poll, complete at the second
    polled <- numeric(0)

    local_mocked_bindings(
      .monitor_snapshot = function(manifest, output_dir) {
        polled <<- c(polled, proc.time()[["elapsed"]])
        list(n_complete = length(polled))
      },
      .render_monitor = function(stats, manifest) invisible(NULL)
    )

    invisible(capture.output(monitor_evaluate(tmpdir, watch = TRUE, interval = 0.25)))

    expect_length(polled, 2L)
    expect_gte(diff(polled), 0.2)

  })

})


## =========================================================================
## The fold preparations on the daemons
## =========================================================================
##
## On a parallel run each fold preparation the run needs is built once, as a
## task of the window's (dispatch_units(), fold_prep_worker()), before any
## configuration runs: on the configs axis from the payload file, on the
## resamples axis from the rows each task carries. This session writes each
## entry as its task returns it; on the configs axis each configuration's
## daemon reads its own from the run's tmp/.

## Four configurations on two daemons: plsr with no selection, and three on
## one PCA prep per fold (plsr bounds num_comp, so the training part's too)
pca_object <- function() {
  obj <- plsr_object(4)
  obj$config$configs$feature_selection[2:4] <- "pca"
  obj$config$configs$model[3:4]             <- c("elastic_net", "svm_rbf")
  obj
}

describe("evaluate() - the fold preparations on the daemons", {

  it("runs a fold preparation's task on one BLAS thread where its daemon started on more, and puts its threads back", {

    skip_if_not_installed("RhpcBLASctl")

    ## In process: the worker body stands in for a daemon the user started
    ## with BLAS on two threads, and the prep reads the count it runs at
    old <- RhpcBLASctl::blas_get_num_procs()
    withr::defer(RhpcBLASctl::blas_set_num_threads(old))
    RhpcBLASctl::blas_set_num_threads(2L)
    skip_if_not(RhpcBLASctl::blas_get_num_procs() == 2, "BLAS here runs on one thread whatever is asked")

    shared <- worker_payload()
    shared$configs$feature_selection <- "pca"
    rebuilt <- rebuild_resamples(shared$data, shared$resample_idx)
    units   <- fold_prep_units(shared$configs, rebuilt$cv_folds, shared$role_map, "evaluate",
                               shared$data_fp$data_fields, shared$seed, shared$sg_window, shared$pca_threshold)

    inside <- NULL
    testthat::local_mocked_bindings(
      prep_fold_entry = function(...) {
        inside <<- RhpcBLASctl::blas_get_num_procs()
        "the entry"
      },
      .package = "horizons"
    )

    expect_identical(fold_prep_worker(units[[1]], worker_payload_file(shared)), "the entry")
    expect_equal(c(inside = inside, after = RhpcBLASctl::blas_get_num_procs()), c(inside = 1, after = 2))

  })

  it("reads its configuration's fold preparations in the worker, and returns one it cannot read as what went wrong, raising nothing", {

    ## In process: the worker body is an ordinary function. The payload's
    ## configuration reads one PCA prep per fold; the stage's own code
    ## writes them where the payload says
    shared <- worker_payload()
    shared$configs$feature_selection <- "pca"
    ## As evaluate() sends it: absolute, with forward slashes on Windows too
    shared$fold_cache_dir <- normalizePath(withr::local_tempdir(), winslash = "/")

    rebuilt <- rebuild_resamples(shared$data, shared$resample_idx)
    units   <- fold_prep_units(shared$configs, rebuilt$cv_folds, shared$role_map, "evaluate",
                               shared$data_fp$data_fields, shared$seed, shared$sg_window, shared$pca_threshold)

    ## Before the preps are there: the first is not, and nothing is raised
    expect_no_condition(value <- evaluate_config_worker(1L, worker_payload_file(shared)))
    expect_s3_class(value, "horizons_run_file_unread")
    expect_identical(value$file, "a fold preparation")
    expect_identical(dirname(value$path), shared$fold_cache_dir)
    expect_true(basename(value$path) %in% fold_unit_files(units))

    store <- list(dir = shared$fold_cache_dir, shown = shared$fold_cache_dir, memory = NULL)
    build_fold_stage(units, store, rebuilt$cv_folds$splits[[1]]$data, rebuilt$cv_folds, shared$role_map,
                     shared$seed, shared$sg_window, shared$pca_threshold, debug = FALSE, keep_traces = FALSE,
                     collector = log_open("evaluate"), output_dir = shared$fold_cache_dir,
                     results = results_so_far(list()))

    seen <- NULL
    testthat::local_mocked_bindings(
      evaluate_single_config = function(..., fold_entries) {
        seen <<- fold_entries
        tibble::tibble(config_id = "cfg_001", status = "failed")
      },
      .package = "horizons"
    )

    evaluate_config_worker(1L, worker_payload_file(shared))

    expect_length(seen$folds, nrow(rebuilt$cv_folds))
    expect_identical(lapply(seen$folds, `[[`, "key"), lapply(units[vapply(units, `[[`, 1L, "fold_i") > 0L], `[[`, "key"))

    ## One that holds another prep's record is not read as its own
    names <- fold_unit_files(units)
    file.copy(file.path(shared$fold_cache_dir, names[1]), file.path(shared$fold_cache_dir, names[2]), overwrite = TRUE)

    value <- evaluate_config_worker(1L, worker_payload_file(shared))
    expect_s3_class(value, "horizons_run_file_unread")
    expect_identical(value$reason, "holds another fold preparation's record")

  })

  it("sends each fold preparation's task the worker function, its unit and the payload's path, the payload written before the first", {

    ## Recorded and stopped at the first task sent, which is the stage's, as
    ## the configs axis's task is recorded above
    sent <- NULL
    dir  <- withr::local_tempdir()

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      daemons_dispatch        = function() TRUE
    )
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      sent <<- list(expr = .expr, args = .args, compute = .compute,
                    payload_there = file.exists(.args$payload),
                    task_bytes    = length(serialize(.args[names(.args) != "worker"], NULL)),
                    payload_bytes = file.size(.args$payload))
      rlang::abort("stop", class = "dispatched")
    }, .package = "mirai")

    tryCatch(
      suppressWarnings(evaluate(pca_object(), seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                           output_dir = dir, verbose = FALSE))),
      dispatched = function(e) NULL
    )

    expect_identical(sent$expr, quote(worker(unit, payload)))
    expect_named(sent$args, c("worker", "unit", "payload"))
    expect_identical(sent$args$worker, fold_prep_worker)
    expect_true(isNamespace(environment(sent$args$worker)))
    expect_null(sent$compute)

    ## The payload's file, absolute, written before the first task
    expect_true(sent$payload_there)
    expect_match(sent$args$payload, "^/|^[A-Za-z]:/")
    expect_match(basename(sent$args$payload), "^payload-.*\\.rds$")

    ## The unit, the longest first (PCA's training part before its folds),
    ## and none of the data
    expect_identical(sent$args$unit$fold, "training")
    expect_lt(sent$task_bytes, 2048)

  })

  it("writes every fold preparation and every checkpoint in this session and nothing on a daemon, and leaves no preparation once the run is complete, its payload removed before its preparations (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## The session's writes counted here; each daemon's write_output_file()
    ## replaced with one that notes the path it is given in a signals
    ## directory, then writes as it did: no daemon writes anything, since
    ## this session writes the fold preparations and, as it collects each
    ## row, the configurations' checkpoints
    local_test_daemons(2)
    signals <- withr::local_tempdir()
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("write_output_file", envir = ns)
      utils::assignInNamespace("write_output_file", function(value, path) {
        writeLines(path, tempfile(tmpdir = signals))
        real(value, path)
      }, ns = "horizons")
    }, signals = signals)[], 30)

    real_write <- write_output_file
    here       <- character(0)
    local_mocked_bindings(write_output_file = function(value, path) {
      here <<- c(here, path)
      real_write(value, path)
    })

    ## What the session's removals find in tmp/, in the order they run
    real_clear  <- clear_run_files
    real_remove <- remove_run_file
    removals    <- character(0)
    local_mocked_bindings(
      clear_run_files = function(output_dir, keep = character(0)) {
        removals <<- c(removals, paste("clear:", paste(sort(list.files(file.path(output_dir, "tmp"))), collapse = " ")))
        real_clear(output_dir, keep)
      },
      remove_run_file = function(path) {
        removals <<- c(removals, paste("remove:", basename(path)))
        real_remove(path)
      }
    )

    dir <- withr::local_tempdir()
    ev  <- with_deadline(suppressWarnings(evaluate(pca_object(), seed = 42L,
                                                   control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                              output_dir = dir, verbose = FALSE))), 180)

    there <- as.character(unlist(lapply(list.files(signals, full.names = TRUE), readLines)))

    expect_identical(ev$evaluation$results$status, rep("success", 4L))
    expect_length(grep("^fold-", basename(here)), 4L)
    expect_setequal(basename(here[basename(dirname(here)) == "checkpoints"]), sprintf("cfg_%03d.rds", 1:4))
    expect_identical(there, character(0))
    expect_false(dir.exists(file.path(dir, "tmp")))

    ## The payload goes first, by its own removal; the clearing of a
    ## completed run then finds the four preparations alone
    n <- length(removals)
    expect_match(removals[n - 1L], "^remove: payload-.*\\.rds$")
    expect_match(removals[n], "^clear: fold-[0-9a-f]+\\.rds( fold-[0-9a-f]+\\.rds){3}$")

  })

  it("stops as systemic when a fold preparation's daemon dies, keeping the preparations written before it (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir <- withr::local_tempdir()
    tmp <- file.path(dir, "tmp")

    ## On the daemons, the last fold's prep kills its daemon once another
    ## prep is written
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("prep_fold_entry", envir = ns)
      utils::assignInNamespace("prep_fold_entry", function(unit, ...) {
        if (identical(unit$fold, "Fold3")) {
          until <- Sys.time() + 30
          while (length(list.files(tmp, pattern = "^fold-")) == 0L && Sys.time() < until) Sys.sleep(0.05)
          tools::pskill(Sys.getpid(), tools::SIGKILL)
        }
        real(unit, ...)
      }, ns = "horizons")
    }, tmp = tmp)[], 30)

    err <- expect_error(
      with_deadline(suppressWarnings(evaluate(pca_object(), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 120),
      class = "horizons_error_systemic"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "The mirai daemon running the fold preparation \"raw/pca/Fold3\" stopped while it ran",
                 fixed = TRUE)
    expect_identical(nrow(err$results), 0L)
    expect_gt(length(list.files(tmp, pattern = "^fold-")), 0L)
    expect_length(list.files(tmp, pattern = "^payload-"), 0L)

  })

  it("cancels the fold preparations in flight when it is interrupted, keeping those written, and none is written after it returns (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2)
    dir  <- withr::local_tempdir()
    tmp  <- file.path(dir, "tmp")
    done <- withr::local_tempdir()

    ## On the daemons the first fold's prep waits three seconds before it
    ## runs and each later fold's fifteen, in R code a cancellation
    ## interrupts, so the cancel lands while the second fold's still waits
    ## however loaded the machine; each says when it has finished
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("prep_fold_entry", envir = ns)
      utils::assignInNamespace("prep_fold_entry", function(unit, ...) {
        if (identical(unit$fold, "Fold1")) Sys.sleep(3) else if (!identical(unit$fold, "training")) Sys.sleep(15)
        entry <- real(unit, ...)
        file.create(file.path(done, unit$fold))
        entry
      }, ns = "horizons")
    }, done = done)[], 30)

    ## The interrupt arrives as the window's second wait ends: the training
    ## part's prep was collected and written after the first, the first
    ## fold's has just finished, uncollected, and the second's is running
    rounds     <- 0L
    real_round <- await_round
    local_mocked_bindings(
      daemon_reports = matching_daemons,
      await_round    = function(tasks, no_daemons) {
        real_round(tasks, no_daemons)
        rounds <<- rounds + 1L
        if (rounds == 2L) rlang::interrupt()
      }
    )

    got <- tryCatch(
      with_deadline(suppressWarnings(evaluate(pca_object(), seed = 42L,
                                              control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                                         output_dir = dir, verbose = FALSE))), 60),
      interrupt = function(i) "interrupted"
    )

    expect_identical(got, "interrupted")

    returned <- list.files(tmp)

    ## Once the daemons are idle again nothing more is there: the session
    ## writes the preps, and a cancelled one returns nothing to write
    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 30)
    expect_identical(list.files(tmp), returned)
    expect_length(returned, 1L)
    expect_match(returned, "^fold-")
    expect_length(list.files(file.path(dir, "checkpoints")), 0L)

    ## The second fold's prep, cancelled in its wait, never finished
    expect_setequal(list.files(done), c("training", "Fold1"))

  })

  it("says once, on the log and the tree, that daemons without the dispatcher lose the order and the cancellation of a run's fold preparations on the resamples axis (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_test_daemons(2, dispatcher = FALSE)

    out <- utils::capture.output(got <- collect_warnings(with_deadline(
      evaluate(pca_object(), seed = 42L, control = control_horizons(allow_par = TRUE, parallelize_over = "resamples",
                                                                     verbose = TRUE)),
      120
    ), class = "horizons_warning_input"))

    expect_length(got$warnings, 1L)
    msg <- gsub("\\s+", " ", conditionMessage(got$warnings[[1]]))
    expect_match(msg, "set with `dispatcher = FALSE`, so `evaluate()` cannot start the longest fold preparations first",
                 fixed = TRUE)
    expect_true(any(grepl("Parallel: over resamples on mirai (2 daemons, no dispatcher)", out, fixed = TRUE)))
    expect_identical(got$value$evaluation$results$status, rep("success", 4L))

  })

})
