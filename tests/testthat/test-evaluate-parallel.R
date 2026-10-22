## ---------------------------------------------------------------------------
## Tests: evaluate() parallel execution
## ---------------------------------------------------------------------------

## Parallel workers resolve horizons by name in the INSTALLED library, so
## evaluate() refuses either parallel axis under devtools::load_all() (see the
## guard tests below). The dispatching tests therefore run only against an installed
## build: R CMD check, or devtools::test() after devtools::install().
## skip_if_dev_package() comes from helper-load-all.R, shared with
## test-pipeline-predict.R's fresh-process round trip.

## local_plan() and keep_only_warning() come from helper-parallel.R. Runs that
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

  it("stops on the configs axis once every configuration has returned, when a worker could not write its checkpoint, with every row", {

    ## The dispatch runs each worker body in this session, so this runs
    ## under load_all() too
    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      worker_horizons_version = function() as.character(utils::packageVersion("horizons")),
      check_parallel_backend  = function(...) TRUE,
      registered_workers      = function() 2L,
      registered_plan_label   = function() "multisession",
      evaluate_single_config  = function(config_row, ...) {
        create_failed_result(config_row$config_id, "mocked")
      }
    )
    local_mocked_bindings(future_map = function(.x, .f, ..., .options = NULL) lapply(.x, .f, ...),
                          .package = "furrr")

    tmpdir <- withr::local_tempdir()
    store  <- file.path(tmpdir, "checkpoints")
    dir.create(store)
    dir.create(file.path(store, "cfg_002.rds"))   # the second worker's rename is refused

    err <- expect_error(
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                           output_dir = tmpdir))),
      class = "horizons_error_systemic"
    )

    expect_identical(class(err)[1], "horizons_error_systemic")
    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
    expect_match(msg, "`evaluate()` could not write to", fixed = TRUE)
    expect_match(msg, "cfg_002.rds: ", fixed = TRUE)
    expect_setequal(err$results$config_id, c("cfg_001", "cfg_002", "cfg_003"))
    expect_null(attr(err$results, "checkpoint_failed"))
    expect_true(file.exists(file.path(store, "cfg_001.rds")) && !dir.exists(file.path(store, "cfg_001.rds")))
    expect_false(any(grepl("\\.tmp$", list.files(store, all.files = TRUE))))

  })

  it("refuses a parallel run under devtools::load_all(), on either axis, before anything is drawn", {

    ## The seam reports load_all(), so this runs on an installed build too;
    ## the plan is mocked, since the refusal comes before it would be used
    local_mocked_bindings(
      running_under_load_all = function() TRUE,
      check_parallel_backend = function(...) TRUE,
      registered_workers     = function() 2L,
      registered_plan_label  = function() "multisession",
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
      worker_horizons_version = function() "0.0.1",
      check_parallel_backend  = function(...) TRUE,
      registered_workers      = function() 2L,
      registered_plan_label   = function() "multisession",
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

    ## The dispatch is stopped at furrr, so this runs under load_all() too
    local_mocked_bindings(
      running_under_load_all  = function() FALSE,
      worker_horizons_version = function() as.character(utils::packageVersion("horizons")),
      check_parallel_backend  = function(...) TRUE,
      registered_workers      = function() 2L,
      registered_plan_label   = function() "multisession"
    )
    local_mocked_bindings(future_map = function(...) rlang::abort("stop", class = "dispatched"),
                          .package = "furrr")

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

  it("asks a worker of the registered plan which horizons it loads", {

    ## On a sequential plan the future runs here, so it loads this one
    local_plan(future::sequential)

    expect_identical(horizons:::worker_horizons_version(),
                     as.character(utils::packageVersion("horizons")))

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

## future::plan("list") as a record that holds no live state, for the shared
## runs below to keep. Each level's backend is an environment that counts the
## futures run under it, so a stored plan list would change whenever the plan
## was used again, and a memoised value must not change. The record keeps
## each level's strategy and backend by identity, as identical() compares
## them, and its class and set-up state, so a failure says what moved.
plan_record <- function() {

  lapply(future::plan("list"), function(strategy) {
    list(strategy = rlang::obj_address(strategy),
         class    = class(strategy),
         init     = attr(strategy, "init"),
         backend  = rlang::obj_address(attr(strategy, "backend")))
  })

}

## One allow_par = TRUE run under a sequential plan, read by the two tests
## below (helper-memo.R). The builder registers the sequential plan itself and
## puts the caller's back, recording the plan as the run found it and left
## it. future sets a registered plan up on its first use (nbrOfWorkers() is
## one), which marks it as set up; the builder takes that step before it
## records the plan, so the records compare the plan as a user who has used
## it holds it, and only a change evaluate() makes shows. Builders must be
## quiet, so the warnings evaluate() gave are kept (their messages, in order)
## rather than shown, and replay_warnings() signals them again inside the
## test that expects one.
allow_par_run <- function() memo_fixture("allow_par_run", build_allow_par_run)

build_allow_par_run <- function() {

  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  future::nbrOfWorkers()

  plan_before <- plan_record()
  obj         <- make_eval_object(n_configs = 2)
  warnings    <- list()

  result <- withCallingHandlers(
    evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE)),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- simpleWarning(conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(obj = obj, result = result, warnings = warnings,
       plan_before = plan_before, plan_after = plan_record())

}

replay_warnings <- function(run) {

  for (w in run$warnings) warning(w)
  run$result

}

describe("evaluate() - allow_par without a usable backend", {

  it("warns naming the plan, runs sequentially, and never registers or alters the plan", {

    run <- allow_par_run()

    expect_warning(
      result <- keep_only_warning(
        replay_warnings(run),
        "offers 1 worker"
      ),
      "offers 1 worker"
    )

    expect_s3_class(result, "horizons_eval")
    expect_equal(result$evaluation$parallelize_over, "sequential")
    expect_identical(result$evaluation$workers, 1L)

    expect_identical(run$plan_after, run$plan_before)

  })

  it("gives the same results as allow_par = FALSE", {

    run <- allow_par_run()
    local_plan(future::sequential)
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

  it("names the axis, plan and workers, records them, and pins the threads only while it runs", {

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
        registered_workers      = function() workers,
        registered_plan_label   = function() "multisession",
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
    unbounded  <- run(TRUE, NA_integer_)
    sequential <- run(FALSE, 2L)

    expect_identical(two$line, "│  Parallel: over resamples on multisession (2 workers)")
    expect_identical(unbounded$line, "│  Parallel: over resamples on multisession (unbounded)")
    expect_identical(sequential$line, character())

    ## allow_par = FALSE never inspects the plan
    expect_identical(two$manifest, list(plan = "multisession", workers = 2L, axis = "resamples"))
    expect_identical(unbounded$manifest$workers, NA_integer_)
    expect_identical(sequential$manifest,
                     list(plan = "sequential", workers = 1L, axis = "sequential"))

    expect_identical(c(two$inside, unbounded$inside, sequential$inside), c(1L, 1L, 7L))
    expect_identical(getOption("ranger.num.threads"), 7L)

  })

  it("runs a debug run in this session, whatever allow_par says, without inspecting the plan", {

    dir <- withr::local_tempdir()

    out <- utils::capture.output(testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 1), seed = 42L,
                                  control = control_horizons(allow_par = TRUE, debug = TRUE,
                                                             output_dir = dir, verbose = TRUE))),
        header_rendered = function(e) NULL
      ),
      check_parallel_backend = function(...) stop("the plan was inspected"),
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

describe("evaluate() - output_dir and the configs axis (#233)", {

  it("refuses an explicit configs axis without an output_dir before anything runs", {

    obj <- make_eval_object(n_configs = 4)

    local_mocked_bindings(
      check_parallel_backend = function(...) stop("the plan was inspected"),
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
      registered_workers     = function() 2L,
      registered_plan_label  = function() "multisession",
      evaluate_single_config = function(...) rlang::abort("stop", class = "header_rendered"),
      .package = "horizons"
    ))

    expect_identical(grep("Parallel:", out, value = TRUE, fixed = TRUE),
                     "│  Parallel: over resamples on multisession (2 workers)")

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
        testthat::with_mocked_bindings(
          tryCatch(suppressWarnings(evaluate(obj, seed = 42L, control = control)),
                   dispatched = function(e) NULL),
          future_map = function(...) {
            seen <<- getOption("future.globals.maxSize")
            rlang::abort("stop", class = "dispatched")
          },
          .package = "furrr"
        ),
        check_parallel_backend = function(...) TRUE,
        registered_workers     = function() 2L,
        registered_plan_label  = function() "multisession",
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

## One four-config run on a two-worker plan, parallelize_over = "auto"
## (4 configs >= 3 folds, so the configs axis), read by the test below, which
## gets a private copy of its output_dir (helper-memo.R). The builder
## registers the plan itself and puts the caller's back, recording the
## plan (plan_record(), set up first, as in allow_par_run() above) and the
## worker count as the run found and left them. It muffles the plan's
## warnings as well as the run's: a warning in a build would fail every test
## that reads it.
configs_axis_run <- function(.env = parent.frame()) {
  memo_dir("configs_axis_run", build_configs_axis_run, .env = .env)
}

build_configs_axis_run <- function(dir) {

  old <- suppressWarnings(future::plan(future::multisession, workers = 2))
  on.exit(suppressWarnings(future::plan(old)), add = TRUE)
  future::nbrOfWorkers()

  plan_before <- plan_record()
  obj         <- make_eval_object(n_configs = 4)

  result <- suppressWarnings(
    evaluate(obj,
             seed = 42L, control = control_horizons(allow_par = TRUE, output_dir = dir))
  )

  list(obj = obj, result = result, plan_before = plan_before,
       plan_after = plan_record(), workers_after = future::nbrOfWorkers())

}

describe("evaluate() - configs axis", {

  it("writes a schema-4 manifest describing the plan, the axis, the data and the settings, one result and one checkpoint per config, and leaves the user's plan exactly as it found it", {

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
    expect_equal(manifest$plan, "multisession")
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

    expect_identical(run$value$plan_after, run$value$plan_before)
    expect_identical(run$value$workers_after, 2L)

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

    local_plan(future::multisession, workers = 2)

    par_result <- suppressWarnings(
      evaluate(obj,
               seed = 42L, control = control_horizons(allow_par = TRUE, output_dir = tmpdir))
    )

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
      axis = "configs", plan = "multisession", workers = 4L,
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
    manifest$plan    <- "multisession"
    manifest$workers <- 10L
    saveRDS(manifest, path)

    files <- sort(list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE))
    unlink(files[length(files)])

    output <- capture.output(result <- monitor_evaluate(tmpdir))

    expect_equal(result$n_complete, 3)
    expect_equal(result$n_total, 4)
    expect_false(is.na(result$best_config))
    expect_true(any(grepl("over configs on multisession (10 workers)", output, fixed = TRUE)))

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
## tune's future path advances the parent RNG stream differently from the
## sequential loop, so before the per-stage re-pinning the Bayesian stage and
## last_fit() drew from a different position on the resamples axis. This
## fixture uses rf (deterministic given a seed) with Bayesian iterations ON,
## where the axes diverged on the tune of that time, and asserts the whole
## result row is identical. On later tune releases the axes no longer diverge
## here without the re-pinning, so this pins the result, not the re-pinning.
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

  it("resamples axis, chosen by auto on a real two-worker plan, matches the sequential run exactly (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    run        <- axes_seq_run()
    obj        <- run$obj
    seq_result <- run$result
    local_plan(future::multisession, workers = 2)

    ## One config against three folds: auto resolves to the resamples axis,
    ## which needs no output_dir
    par_result <- suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE))
    )

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
    local_plan(future::multisession, workers = 2)
    tmpdir <- withr::local_tempdir()

    par_result <- suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE, parallelize_over = "configs", output_dir = tmpdir))
    )

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
## shows they survive the trip to a multisession worker and change what it
## computes. On "raw" a window of 13 trims 12 of the 20 columns rather than 8,
## so the configured run and the default run model different predictors.

describe("evaluate() - recipe settings on the configs axis", {

  it("runs configure()'s window in the worker, as the sequential run does (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    row_cols <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                  "cv_rmse", "cv_rrmse", "cv_rsq", "cv_ccc", "cv_rpd", "cv_mae")

    obj <- make_eval_object(n = 60, n_wn = 20, n_configs = 1)
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

    local_plan(future::multisession, workers = 2)
    tmpdir <- withr::local_tempdir()

    par_result <- suppressWarnings(
      evaluate(obj, seed = 42L, control = control_horizons(allow_par = TRUE, parallelize_over = "configs", output_dir = tmpdir))
    )

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

})
