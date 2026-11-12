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

## local_test_daemons(), with_deadline() and keep_only_warning() come from
## helper-parallel.R. Runs that
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

## A worker's payload for one rf config on 40 rows, as evaluate() builds it,
## with no checkpoint directory
worker_payload <- function() {

  obj   <- make_eval_object(n = 40, n_wn = 20, n_configs = 1)
  set.seed(1)
  split <- rsample::initial_split(obj$data$analysis, prop = 0.8)
  folds <- rsample::vfold_cv(rsample::training(split), v = 3)

  list(
    data            = split$data,
    resample_idx    = horizons:::resample_indices(split, folds),
    configs         = obj$config$configs,
    role_map        = obj$data$role_map,
    grid_size       = 2L,
    bayesian_iter   = 0L,
    prune           = FALSE,
    prune_threshold = 1,
    seed            = 42L,
    sg_window       = 13L,
    pca_threshold   = 0.9,
    outcome_range   = c(-Inf, Inf),
    data_fp         = horizons:::eval_data_fingerprint(rsample::training(split),
                                                       obj$data$role_map),
    settings        = horizons:::eval_settings(sg_window = 13L, pca_threshold = 0.9,
                                               outcome_range = c(-Inf, Inf)),
    checkpoint_dir  = NULL,
    keep_traces     = FALSE,
    pkg_version     = as.character(utils::packageVersion("horizons"))
  )

}

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
                 c("config_i", "shared"))

  })

  it("rejects a malformed payload instead of silently defaulting", {

    good <- setNames(vector("list", length(horizons:::SHARED_ARG_NAMES)),
                     horizons:::SHARED_ARG_NAMES)

    err <- expect_error(
      horizons:::evaluate_config_worker(1L, good[setdiff(names(good), "seed")]),
      "Malformed worker payload", class = "horizons_error_internal"
    )
    expect_match(conditionMessage(err), "Missing: seed; unexpected: -", fixed = TRUE)

    typo        <- good
    names(typo)[names(typo) == "grid_size"] <- "gridsize"
    err <- expect_error(horizons:::evaluate_config_worker(1L, typo),
                        "Malformed worker payload", class = "horizons_error_internal")
    expect_match(conditionMessage(err), "Missing: grid_size; unexpected: gridsize", fixed = TRUE)

  })

  it("does not carry allow_par in the payload: tune is sequential on the configs axis", {

    expect_false("allow_par" %in% horizons:::SHARED_ARG_NAMES)

  })

  it("carries the recipe settings, the outcome range and keep_traces in the payload and hands them to the config (#62, #76)", {

    ## Run in-process: the worker body is an ordinary function, and what is
    ## under test is that it forwards shared$sg_window,
    ## shared$pca_threshold, shared$outcome_range and shared$keep_traces,
    ## not the dispatch. evaluate_single_config() is replaced with a
    ## recorder, so nothing is tuned.
    expect_true(all(c("sg_window", "pca_threshold", "outcome_range", "keep_traces") %in%
                      horizons:::SHARED_ARG_NAMES))

    ## A debug run never dispatches, so debug is never sent
    expect_false("debug" %in% horizons:::SHARED_ARG_NAMES)

    shared <- worker_payload()
    shared$keep_traces <- TRUE

    seen <- NULL

    testthat::local_mocked_bindings(
      evaluate_single_config = function(...) {
        seen <<- list(...)[c("sg_window", "pca_threshold", "outcome_range", "keep_traces")]
        tibble::tibble(config_id = "cfg_001", status = "failed")
      },
      .package = "horizons"
    )

    horizons:::evaluate_config_worker(1L, shared)

    ## The outcome range rides the same payload (#76)
    expect_identical(seen, list(sg_window = 13L, pca_threshold = 0.9,
                                outcome_range = c(-Inf, Inf), keep_traces = TRUE))

  })

  it("runs its configuration with data.table and ranger on one thread", {

    skip_if_not_installed("RhpcBLASctl")
    skip_if_not_installed("data.table")

    ## Both counts are raised first: a session started with the thread
    ## variables at 1 would pass before the worker set anything. data.table
    ## caps at OMP_NUM_THREADS and at OpenMP's count, so both go first.
    withr::local_envvar(OMP_NUM_THREADS = NA)
    withr::local_options(ranger.num.threads = 7L)

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
        seen <<- c(dt = data.table::getDTthreads(), ranger = getOption("ranger.num.threads"))
        tibble::tibble(config_id = "cfg_001", status = "failed")
      },
      .package = "horizons"
    )

    horizons:::evaluate_config_worker(1L, worker_payload())

    expect_identical(seen, c(dt = 1L, ranger = 1L))

  })

  it("fits a lightgbm configuration in a fresh process, which has not loaded bonsai (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("callr")
    skip_if_not_installed("bonsai")
    skip_if_not_installed("lightgbm")

    shared <- worker_payload()
    shared$configs$model <- "lightgbm"

    ## A callr process rather than a daemon: a task sent from a session that
    ## has loaded bonsai can load it on the daemon as well, which would pass
    ## without the fix. bonsai registers lightgbm's engine only once
    ## loaded, so the process reports whether it had.
    res <- callr::r(function(shared) {
      before <- isNamespaceLoaded("bonsai")
      row    <- suppressWarnings(horizons:::evaluate_config_worker(1L, shared))
      list(before = before, status = row$status, error = row$error_message)
    }, args = list(shared = shared), libpath = .libPaths())

    expect_false(res$before)
    expect_identical(res$status, "success", info = res$error)

  })

  it("writes the checkpoint of the row it returns, stamped, and refuses a worker on another version", {

    shared <- worker_payload()
    shared$checkpoint_dir <- withr::local_tempdir()

    testthat::local_mocked_bindings(
      evaluate_single_config = function(...) tibble::tibble(config_id = "cfg_001", status = "failed"),
      .package = "horizons"
    )

    row <- horizons:::evaluate_config_worker(1L, shared)

    expect_identical(row$data_hash, shared$data_fp$data_hash)
    expect_identical(row$settings[[1]], shared$settings)
    expect_identical(readRDS(file.path(shared$checkpoint_dir, "cfg_001.rds")), row)

    ## Another version's worker is a dependency error, which no unit
    ## contains, and it is checked before the payload's keys: another
    ## version's keys may differ, and that is not a bug
    shared$pkg_version <- "0.0.0"
    shared$seed        <- NULL

    err <- expect_error(horizons:::evaluate_config_worker(1L, shared),
                        "but this session runs 0.0.0.", fixed = TRUE,
                        class = "horizons_error_dependency")
    expect_match(conditionMessage(err), "Reinstall the package so workers run matching code.",
                 fixed = TRUE)

  })

  it("says on the row it returns, raising nothing, when its checkpoint cannot be written", {

    shared <- worker_payload()
    shared$checkpoint_dir <- withr::local_tempdir()
    dir.create(file.path(shared$checkpoint_dir, "cfg_001.rds"))   # the rename is refused

    testthat::local_mocked_bindings(
      evaluate_single_config = function(...) tibble::tibble(config_id = "cfg_001", status = "failed"),
      .package = "horizons"
    )

    expect_no_error(row <- horizons:::evaluate_config_worker(1L, shared))

    expect_type(attr(row, "checkpoint_failed"), "character")
    expect_identical(list.files(shared$checkpoint_dir, all.files = TRUE, no.. = TRUE), "cfg_001.rds")

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

    withr::local_options(future.globals.maxSize = 12345)

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

      ## Refused at the door: no tree, no manifest, the globals ceiling as set
      expect_identical(out, character())
      expect_false(file.exists(file.path(tmpdir, "eval_manifest.rds")))

    }

    expect_identical(getOption("future.globals.maxSize"), 12345)

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
      paste0("A parallel worker loads horizons 0.0.1, but this session runs ",
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

## The default profile's daemons as a run found or left them: how many were
## set, whether any are, their dispatcher (a new one after any reset) and how
## many are connected. The shared runs below keep it, so a run that set,
## resized or reset the daemons shows.
daemons_record <- function() {

  list(n          = mirai::nextget("n"),
       set        = mirai::daemons_set(),
       dispatcher = mirai::nextget("dispatcher"),
       connected  = connected_daemons())

}

## One allow_par = TRUE run with no daemons set, read by the two tests below
## (helper-memo.R), recording the daemons as the run found and left them.
## Builders must be quiet, so the warnings evaluate() gave are kept (their
## messages, in order) rather than shown, and replay_warnings() signals them
## again inside the test that expects one.
allow_par_run <- function() memo_fixture("allow_par_run", build_allow_par_run)

build_allow_par_run <- function() {

  daemons_before <- daemons_record()
  obj            <- make_eval_object(n_configs = 2)
  warnings       <- list()

  ## xgboost, not the fixture's cubist: the results are compared with a run
  ## made later in the session, and Cubist's neighbour cutoff comes from a
  ## process-wide generator no seed reaches, so a cubist with neighbours
  ## depends on the fits before it (configure()'s reproducibility note)
  obj$config$configs$model <- c("rf", "xgboost")

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

    expect_identical(two$line, "│  Parallel: over resamples on mirai (2 workers)")
    expect_identical(sequential$line, character())

    ## allow_par = FALSE never reads the daemons
    expect_identical(two$manifest, list(plan = "mirai", workers = 2L, axis = "resamples"))
    expect_identical(sequential$manifest,
                     list(plan = "sequential", workers = 1L, axis = "sequential"))

    expect_identical(c(two$inside, sequential$inside), c(1L, 7L))
    expect_identical(getOption("ranger.num.threads"), 7L)

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

## On the configs axis a checkpoint a worker could not write is reported once
## every configuration has run: the evaluation is returned, with one warning.
## The sequential path stops at the first failed write
## (test-pipeline-evaluate.R).

describe("evaluate() - a checkpoint a configs-axis worker could not write", {

  it("returns the evaluation on the configs axis when a worker could not write its checkpoint, with one skipped warning naming it", {

    ## A real result row to return, read before anything is mocked
    ## (helper-memo.R)
    template <- allow_par_run()$result$evaluation$results[1, ]

    ## The dispatch runs each worker body in this session, so this runs
    ## under load_all() too
    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L,
      evaluate_single_config  = function(config_row, ...) {
        row <- template
        row$config_id <- config_row$config_id
        row
      },
      dispatch_configs        = function(configs, shared, finished, no_daemons) {
        for (i in seq_len(nrow(configs))) {
          finished$rows <- c(finished$rows, list(evaluate_config_worker(i, shared)))
        }
      }
    )

    tmpdir <- withr::local_tempdir()
    store  <- file.path(tmpdir, "checkpoints")
    dir.create(store)
    dir.create(file.path(store, "cfg_002.rds"))   # the second worker's rename is refused

    got <- collect_warnings(
      evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L,
               control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                          output_dir = tmpdir))
    )

    ## Every configuration's row is returned, and none carries the reason
    res <- got$value$evaluation$results
    expect_setequal(res$config_id, c("cfg_001", "cfg_002", "cfg_003"))
    expect_identical(res$status, rep("success", 3L))
    expect_null(attr(res, "checkpoint_failed"))

    skipped <- Filter(function(w) inherits(w, "horizons_warning_skipped"), got$warnings)
    expect_length(skipped, 1L)

    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(skipped[[1]])))
    expect_match(msg, "1 configuration was evaluated, but its checkpoint could not be written to", fixed = TRUE)
    expect_match(msg, store, fixed = TRUE)
    expect_match(msg, "cfg_002.rds: ", fixed = TRUE)
    expect_match(msg, "a run resumed from", fixed = TRUE)

    ## Recorded on the log first, naming the configuration
    row <- got$value$log[got$value$log$topic %in% "skipped" & grepl("checkpoint", got$value$log$message), ]
    expect_identical(nrow(row), 1L)
    expect_identical(row$ids[[1]], "cfg_002")
    expect_identical(skipped[[1]]$log, row)

    ## The others' checkpoints were written, and no temporary file is left
    expect_true(file.exists(file.path(store, "cfg_001.rds")) && !dir.exists(file.path(store, "cfg_001.rds")))
    expect_true(file.exists(file.path(store, "cfg_003.rds")))
    expect_false(any(grepl("\\.tmp$", list.files(store, all.files = TRUE))))

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
                     "│  Parallel: over resamples on mirai (2 workers)")

  })

})


describe("evaluate() - the worker payload limit", {

  it("dispatches under max_worker_payload, or a future.globals.maxSize the user set, and restores it", {

    ## The configs axis runs only on an installed build
    skip_if_dev_package()

    obj <- make_eval_object(n_configs = 4)

    run <- function(control) {
      seen <- NULL
      testthat::with_mocked_bindings(
        tryCatch(suppressWarnings(evaluate(obj, seed = 42L, control = control)),
                 dispatched = function(e) NULL),
        dispatch_configs        = function(...) {
          seen <<- getOption("future.globals.maxSize")
          rlang::abort("stop", class = "dispatched")
        },
        check_parallel_backend  = function(...) TRUE,
        daemon_reports          = matching_daemons,
        connected_daemons       = function() 2L,
        .package = "horizons"
      )
      seen
    }

    withr::local_options(future.globals.maxSize = 2 * 1024^3)

    expect_identical(run(control_horizons(allow_par = TRUE, output_dir = withr::local_tempdir(),
                                          max_worker_payload = 7e8)), 7e8)
    expect_identical(run(control_horizons(allow_par = TRUE, output_dir = withr::local_tempdir())),
                     2 * 1024^3)
    expect_identical(getOption("future.globals.maxSize"), 2 * 1024^3)

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

  it("writes a schema-4 manifest describing the daemons, the axis, the data and the settings, one result and one checkpoint per config, and leaves the user's daemons exactly as it found them", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    run    <- configs_axis_run()
    tmpdir <- run$dir
    result <- run$value$result

    manifest <- readRDS(file.path(tmpdir, "eval_manifest.rds"))
    expect_identical(manifest$schema_version, 4L)
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

    expect_identical(run$value$daemons_after, run$value$daemons_before)
    expect_identical(run$value$daemons_after$connected, 2L)

  })

})


## =========================================================================
## The configs axis's dispatch on mirai
## =========================================================================
## What each task carries, the stop when no daemon is left, a daemon's death
## and what crosses back from a task. The real-daemon tests run plsr, whose
## cells of the cost table tie, so they start in grid order.

## A plsr grid of `n` configurations, small enough to run in seconds
plsr_object <- function(n) {
  obj <- make_eval_object(n = 60, n_configs = n)
  obj$config$configs$model <- "plsr"
  obj
}

describe("evaluate() - the configs axis on mirai", {

  it("sends each task the worker function and its payload, nothing from evaluate()'s frame", {

    ## The task is recorded and the run stopped there, so this runs under
    ## load_all() too
    sent <- NULL

    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      daemon_reports          = matching_daemons,
      check_parallel_backend  = function(...) TRUE,
      connected_daemons       = function() 2L
    )
    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .timeout = NULL, .compute = NULL) {
      sent <<- list(expr = .expr, args = .args, compute = .compute)
      rlang::abort("stop", class = "dispatched")
    }, .package = "mirai")

    tryCatch(
      suppressWarnings(evaluate(make_eval_object(n_configs = 4), seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                           output_dir = withr::local_tempdir(), verbose = FALSE))),
      dispatched = function(e) NULL
    )

    expect_identical(sent$expr, quote(worker(config_i, shared)))
    expect_named(sent$args, c("worker", "config_i", "shared"))
    expect_identical(sent$args$worker, evaluate_config_worker)
    expect_true(isNamespace(environment(sent$args$worker)))
    expect_setequal(names(sent$args$shared), SHARED_ARG_NAMES)
    expect_null(sent$compute)

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

    ## Wrapped once: the window's reading of the daemon's death is the parent
    expect_match(conditionMessage(err), "A parallel worker stopped while `evaluate()` was running", fixed = TRUE)
    expect_s3_class(err$parent, "horizons_error_systemic")
    expect_match(gsub("\\s+", " ", conditionMessage(err$parent)),
                 "The daemon running configuration \"cfg_003\" stopped while it ran (`errorValue` 19, Connection reset).",
                 fixed = TRUE)
    expect_null(err$parent$parent)
    expect_setequal(err$results$config_id, c("cfg_001", "cfg_002"))

  })

  it("keeps a warning raised outside a configuration's unit on its row, where mirai would drop it (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## In this session first: the worker body muffles the warning and keeps
    ## it on the row
    real_rebuild <- rebuild_resamples
    local_mocked_bindings(rebuild_resamples = function(...) {
      warning("A package warned while the folds were rebuilt.")
      real_rebuild(...)
    })
    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) {
        tibble::tibble(config_id = config_row$config_id, status = "success",
                       warnings = list("The unit's own warning."))
      }
    )

    expect_no_warning(row <- evaluate_config_worker(1L, worker_payload()))
    expect_identical(row$warnings[[1]][1], "The unit's own warning.")
    expect_match(row$warnings[[1]][2], "A package warned while the folds were rebuilt.", fixed = TRUE)

    ## Then across a daemon, on a real configuration
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
## variables, not from this session's .libPaths().
local_hidden_package_lib <- function(hidden, .env = parent.frame()) {

  dir <- withr::local_tempdir(.local_envir = .env)

  for (lib in .libPaths()) {
    for (pkg in list.files(lib)) {
      target <- file.path(dir, pkg)
      if (pkg != hidden && !file.exists(target)) file.symlink(file.path(lib, pkg), target)
    }
  }

  withr::local_envvar(R_LIBS = dir, R_LIBS_USER = dir, R_LIBS_SITE = dir, .local_envir = .env)

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

    ## The sequential run's manifest, made to describe a configs-axis run on
    ## ten workers with one config still to finish.
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
    expect_true(any(grepl("over configs on mirai (10 workers)", output, fixed = TRUE)))

    ## Two runs sharing one output_dir are otherwise indistinguishable here.
    expect_true(any(grepl(paste0(manifest$data_n_rows, " training rows of SOC"),
                          output, fixed = TRUE)))
    expect_true(any(grepl(substr(manifest$data_hash, 1, 12), output, fixed = TRUE)))

  })

  it("reports the share done, the rate, the ETA, the best value and the recent completions, and no rate or ETA it cannot know", {

    skip_on_cran()
    run    <- seq_checkpoints()
    tmpdir <- run$dir
    res    <- run$value$result$evaluation$results
    ckpt   <- file.path(tmpdir, "checkpoints")
    path   <- file.path(tmpdir, "eval_manifest.rds")

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
    ## them failed. Under a second passes between writing the start and
    ## reading it; the rate stays within 1% of 3 for half a minute, and the
    ## ETAs below round as expected for a minute.
    unlink(file.path(ckpt, "cfg_004.rds"))
    failed <- readRDS(file.path(ckpt, "cfg_002.rds"))
    failed$status <- "failed"
    failed[paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))] <- NA_real_
    saveRDS(failed, file.path(ckpt, "cfg_002.rds"))

    three <- snapshot_at(Sys.time() - 3600)
    best  <- rank_configs_by_cv(res[res$config_id %in% c("cfg_001", "cfg_003"), ], "rpd")

    expect_identical(three$pct, 75)
    expect_equal(three$rate, 3, tolerance = 0.01)
    expect_identical(three$eta, "20 min")
    expect_identical(three$best_config, best$config_id[1])
    expect_identical(three$best_metric, best$cv_rpd[1])
    expect_setequal(three$recent, c(cv_text("cfg_001"), "cfg_002 failed", cv_text("cfg_003")))

    ## Started half an hour ago: still a rate per hour
    half <- snapshot_at(Sys.time() - 1800)

    expect_equal(half$rate, 6, tolerance = 0.01)
    expect_identical(half$eta, "10 min")

    ## One done, three to go, so the ETA is in hours; the failed config has
    ## no value to rank
    unlink(file.path(ckpt, c("cfg_001.rds", "cfg_003.rds")))

    one <- snapshot_at(Sys.time() - 3600)

    expect_identical(one$eta, "3 hr")
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
