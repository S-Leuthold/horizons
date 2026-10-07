## ---------------------------------------------------------------------------
## Tests: evaluate() parallel execution
## ---------------------------------------------------------------------------

## Parallel dispatch resolves the worker by name in the INSTALLED horizons, so
## evaluate() refuses to dispatch under devtools::load_all() (see the guard
## test below). The dispatching tests therefore run only against an installed
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

    expect_error(
      horizons:::evaluate_config_worker(1L, good[setdiff(names(good), "seed")]),
      "Malformed worker payload"
    )

    typo        <- good
    names(typo)[names(typo) == "grid_size"] <- "gridsize"
    expect_error(horizons:::evaluate_config_worker(1L, typo),
                 "Malformed worker payload")

  })

  it("does not carry allow_par in the payload: tune is sequential on the configs axis", {

    expect_false("allow_par" %in% horizons:::SHARED_ARG_NAMES)

  })

  it("carries the recipe settings and the outcome range in the payload and hands them to the config (#62, #76)", {

    ## Run in-process: the worker body is an ordinary function, and what is
    ## under test is that it forwards shared$sg_window,
    ## shared$pca_threshold and shared$outcome_range, not the dispatch.
    ## evaluate_single_config() is replaced with a recorder, so nothing is
    ## tuned.
    expect_true(all(c("sg_window", "pca_threshold", "outcome_range") %in%
                      horizons:::SHARED_ARG_NAMES))

    obj   <- make_eval_object(n = 40, n_wn = 20, n_configs = 1)
    set.seed(1)
    split <- rsample::initial_split(obj$data$analysis, prop = 0.8)
    folds <- rsample::vfold_cv(rsample::training(split), v = 3)

    shared <- list(
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
      pkg_version     = as.character(utils::packageVersion("horizons"))
    )

    seen <- NULL

    testthat::local_mocked_bindings(
      evaluate_single_config = function(...) {
        seen <<- list(...)[c("sg_window", "pca_threshold", "outcome_range")]
        tibble::tibble(config_id = "cfg_001", status = "failed")
      },
      .package = "horizons"
    )

    horizons:::evaluate_config_worker(1L, shared)

    ## The outcome range rides the same payload (#76)
    expect_identical(seen, list(sg_window = 13L, pca_threshold = 0.9,
                                outcome_range = c(-Inf, Inf)))

  })

  it("refuses to dispatch in parallel under devtools::load_all()", {

    skip_if_not(exists(".__DEVTOOLS__", envir = asNamespace("horizons"),
                       inherits = FALSE),
                "only meaningful under load_all()")
    skip_on_cran()

    local_plan(future::multisession, workers = 2)

    obj    <- make_eval_object(n_configs = 4)   # 4 >= cv_folds (3) -> configs
    tmpdir <- withr::local_tempdir()

    expect_error(
      suppressWarnings(
        evaluate(obj, allow_par = TRUE, output_dir = tmpdir, verbose = FALSE)
      ),
      "load_all"
    )

  })

})


## =========================================================================
## Argument handling: removed arguments, backend check, output_dir requirement
## =========================================================================

describe("evaluate() - workers is gone", {

  it("is not an argument", {

    obj <- make_eval_object(n_configs = 2)

    expect_error(evaluate(obj, workers = 2L, verbose = FALSE), "unused argument")

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

## One allow_par = TRUE run under a sequential plan, read by the three tests
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
    evaluate(obj, allow_par = TRUE, verbose = FALSE, seed = 42L),
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

  it("warns naming the plan and runs sequentially", {

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

  })

  it("gives the same results as allow_par = FALSE", {

    run <- allow_par_run()
    local_plan(future::sequential)
    obj <- run$obj

    seq_result <- suppressWarnings(
      evaluate(obj, allow_par = FALSE, verbose = FALSE, seed = 42L)
    )
    par_result <- run$result

    expect_equal(par_result$evaluation$results$rmse,
                 seq_result$evaluation$results$rmse)
    expect_equal(par_result$evaluation$best_config,
                 seq_result$evaluation$best_config)

  })

  it("never registers or alters the plan", {

    run <- allow_par_run()

    expect_identical(run$plan_after, run$plan_before)

  })

})

describe("evaluate() - output_dir requirement", {

  it("is required when configs are dispatched to workers", {

    skip_on_cran()
    local_plan(future::multisession, workers = 2)
    obj <- make_eval_object(n_configs = 4)   # auto -> configs

    expect_error(
      suppressWarnings(
        evaluate(obj, allow_par = TRUE, parallelize_over = "configs",
                 output_dir = NULL, verbose = FALSE)
      ),
      "output_dir"
    )

  })

  it("is not required on the resamples axis", {

    skip_unless_slow_tier()
    skip_on_cran()
    local_plan(future::multisession, workers = 2)
    obj <- make_eval_object(n_configs = 2)

    result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, parallelize_over = "resamples",
               verbose = FALSE, seed = 42L)
    )

    expect_s3_class(result, "horizons_eval")
    expect_equal(result$evaluation$parallelize_over, "resamples")
    expect_identical(result$evaluation$workers, 2L)

  })

})


## =========================================================================
## Resamples axis runs under load_all(): tune dispatches by value
## =========================================================================

describe("evaluate() - resamples axis", {

  it("runs on the registered plan and records the axis", {

    skip_unless_slow_tier()
    skip_on_cran()
    local_plan(future::multisession, workers = 2)
    obj <- make_eval_object(n_configs = 2)   # 2 < cv_folds (3) -> auto = resamples

    result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, verbose = FALSE, seed = 42L)
    )

    expect_equal(result$evaluation$parallelize_over, "resamples")
    expect_equal(nrow(result$evaluation$results), 2)
    expect_true(all(result$evaluation$results$status %in% c("success", "pruned", "failed")))

  })

})


## =========================================================================
## Configs axis (installed build only)
## =========================================================================

## One four-config run on a two-worker plan, parallelize_over = "auto"
## (4 configs >= 3 folds, so the configs axis), read by three of the tests
## below; each gets a private copy of its output_dir (helper-memo.R). The
## builder registers the plan itself and puts the caller's back, recording the
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
    evaluate(obj, allow_par = TRUE, output_dir = dir, verbose = FALSE,
             seed = 42L)
  )

  list(obj = obj, result = result, plan_before = plan_before,
       plan_after = plan_record(), workers_after = future::nbrOfWorkers())

}

describe("evaluate() - configs axis", {

  it("writes a schema-4 manifest describing the plan, the axis, the data and the settings", {

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

  })

  it("produces one result and one checkpoint per config", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    run    <- configs_axis_run()
    tmpdir <- run$dir
    result <- run$value$result

    expect_s3_class(result, "horizons_eval")
    expect_equal(nrow(result$evaluation$results), 4)
    expect_true(all(result$evaluation$results$config_id %in%
                      c("cfg_001", "cfg_002", "cfg_003", "cfg_004")))
    expect_false(any(is.na(result$evaluation$results$status)))

    checkpoint_files <- list.files(file.path(tmpdir, "checkpoints"), pattern = "\\.rds$")
    expect_equal(length(checkpoint_files), 4)

  })

  it("matches the sequential run in structure", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    obj <- make_eval_object(n_configs = 2)

    seq_result <- suppressWarnings(
      evaluate(obj, verbose = FALSE, seed = 42L)
    )

    local_plan(future::multisession, workers = 2)
    tmpdir <- withr::local_tempdir()

    par_result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, parallelize_over = "configs",
               output_dir = tmpdir, verbose = FALSE, seed = 42L)
    )

    ## Structure rather than exact values: engine-level nondeterminism
    ## (cubist, #51) would make a bitwise comparison flaky for reasons
    ## unrelated to parallelism.
    expect_setequal(seq_result$evaluation$results$config_id,
                    par_result$evaluation$results$config_id)
    expect_true(par_result$evaluation$best_config %in%
                  par_result$evaluation$results$config_id)

  })

  it("leaves the user's plan exactly as it found it", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    run <- configs_axis_run()$value

    expect_identical(run$plan_after, run$plan_before)
    expect_identical(run$workers_after, 2L)

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
    evaluate(obj, output_dir = dir, verbose = FALSE, seed = 42L)
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
      evaluate(obj, allow_par = TRUE, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
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

  it("errors on missing directory", {

    expect_error(monitor_evaluate("/nonexistent/path"), "not found")

  })

  it("errors on missing manifest", {

    tmpdir <- withr::local_tempdir()
    expect_error(monitor_evaluate(tmpdir), "eval_manifest")

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

  it("refuses a manifest of another schema, pointing at a new run", {

    tmpdir <- withr::local_tempdir()
    saveRDS(list(schema_version = 3L, n_total = 10, metric = "rpd"),
            file.path(tmpdir, "eval_manifest.rds"))

    ## The abort carries no package class (DECISIONS 2026-10-05), so its text
    ## is the check.
    err <- expect_error(monitor_evaluate(tmpdir),
                        "was written by another version of horizons", fixed = TRUE)
    expect_match(conditionMessage(err), "evaluate() rewrites it", fixed = TRUE)

  })

})


## =========================================================================
## Results do not depend on the axis (review finding, 2026-09-15)
## =========================================================================
## tune's future path advances the parent RNG stream differently from the
## sequential loop, so before the per-stage re-pinning the Bayesian stage and
## last_fit() drew from a different position on the resamples axis. This
## fixture uses rf (deterministic given a seed) with Bayesian iterations ON,
## which is exactly where the axes used to diverge, and asserts the whole
## result row is identical. The sequential run both tests compare against is
## built once, by whichever of them runs first (helper-memo.R), so a skipped
## test costs nothing.

axes_seq_run <- function() memo_fixture("axes_seq_run", build_axes_seq_run)

build_axes_seq_run <- function() {

  obj <- make_eval_object(n = 60, n_configs = 1)      # rf only
  obj$config$tuning$bayesian_iter <- 2L

  result <- suppressWarnings(
    evaluate(obj, allow_par = FALSE, verbose = FALSE, seed = 42L)
  )

  list(obj = obj, result = result)

}

describe("evaluate() - results are identical across axes", {

  row_cols <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                "cv_rmse", "cv_rrmse", "cv_rsq", "cv_ccc", "cv_rpd", "cv_mae")

  it("resamples axis on a real two-worker plan matches the sequential run exactly", {

    skip_unless_slow_tier()
    skip_on_cran()
    run        <- axes_seq_run()
    obj        <- run$obj
    seq_result <- run$result
    local_plan(future::multisession, workers = 2)

    par_result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, parallelize_over = "resamples",
               verbose = FALSE, seed = 42L)
    )

    expect_equal(par_result$evaluation$parallelize_over, "resamples")
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
      evaluate(obj, allow_par = TRUE, parallelize_over = "configs",
               output_dir = tmpdir, verbose = FALSE, seed = 42L)
    )

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
      evaluate(obj, allow_par = FALSE, verbose = FALSE, seed = 42L)
    )

    default_obj <- obj
    default_obj$config$recipe <- NULL

    seq_default <- suppressWarnings(
      evaluate(default_obj, allow_par = FALSE, verbose = FALSE, seed = 42L)
    )

    local_plan(future::multisession, workers = 2)
    tmpdir <- withr::local_tempdir()

    par_result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, parallelize_over = "configs",
               output_dir = tmpdir, verbose = FALSE, seed = 42L)
    )

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
