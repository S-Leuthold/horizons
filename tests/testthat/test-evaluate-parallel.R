## ---------------------------------------------------------------------------
## Tests: evaluate() parallel execution
## ---------------------------------------------------------------------------

## Parallel dispatch resolves the worker by name in the INSTALLED horizons, so
## evaluate() refuses to dispatch under devtools::load_all() (see the guard
## test below). The dispatching tests therefore run only against an installed
## build: R CMD check, or devtools::test() after devtools::install().
## skip_if_dev_package() comes from helper-load-all.R, shared with
## test-pipeline-predict.R's fresh-process round trip.

## local_plan() and keep_only_warning() come from helper-parallel.R.

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

    worker <- horizons:::evaluate_config_worker

    expect_lt(length(serialize(worker, NULL)), 1e6)

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
## Argument handling: deprecation, backend check, output_dir requirement
## =========================================================================

describe("evaluate() - workers is deprecated", {

  it("warns with a stable class and runs sequentially regardless of the value", {

    obj <- make_eval_object(n_configs = 2)

    expect_warning(
      result <- keep_only_warning(
        evaluate(obj, workers = 10L, verbose = FALSE, seed = 42L),
        "horizons_deprecated_workers"
      ),
      class = "horizons_deprecated_workers"
    )

    ## The old value is ignored: the run is sequential and the object valid.
    expect_s3_class(result, "horizons_eval")
    expect_equal(result$evaluation$parallelize_over, "sequential")
    expect_identical(result$evaluation$workers, 1L)

  })

  it("no longer validates workers as a core count", {

    obj <- make_eval_object(n_configs = 2)

    ## Previously an error ("positive integer"); now only the deprecation.
    expect_warning(
      keep_only_warning(
        evaluate(obj, workers = "two", verbose = FALSE, seed = 42L),
        "horizons_deprecated_workers"
      ),
      class = "horizons_deprecated_workers"
    )

  })

})

describe("evaluate() - allow_par without a usable backend", {

  it("warns naming the plan and runs sequentially", {

    local_plan(future::sequential)
    obj <- make_eval_object(n_configs = 2)

    expect_warning(
      result <- keep_only_warning(
        evaluate(obj, allow_par = TRUE, verbose = FALSE, seed = 42L),
        "offers 1 worker"
      ),
      "offers 1 worker"
    )

    expect_s3_class(result, "horizons_eval")
    expect_equal(result$evaluation$parallelize_over, "sequential")
    expect_identical(result$evaluation$workers, 1L)

  })

  it("gives the same results as allow_par = FALSE", {

    local_plan(future::sequential)
    obj <- make_eval_object(n_configs = 2)

    seq_result <- suppressWarnings(
      evaluate(obj, allow_par = FALSE, verbose = FALSE, seed = 42L)
    )
    par_result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, verbose = FALSE, seed = 42L)
    )

    expect_equal(par_result$evaluation$results$rmse,
                 seq_result$evaluation$results$rmse)
    expect_equal(par_result$evaluation$best_config,
                 seq_result$evaluation$best_config)

  })

  it("never registers or alters the plan", {

    local_plan(future::sequential)
    before <- future::plan("list")
    obj <- make_eval_object(n_configs = 2)

    suppressWarnings(evaluate(obj, allow_par = TRUE, verbose = FALSE, seed = 42L))

    expect_identical(future::plan("list"), before)

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

describe("evaluate() - configs axis", {

  it("writes a schema-4 manifest describing the plan, the axis, the data and the settings", {

    skip_on_cran()
    skip_if_dev_package()
    local_plan(future::multisession, workers = 2)

    obj    <- make_eval_object(n_configs = 4)
    tmpdir <- withr::local_tempdir()

    result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
    )

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

    skip_on_cran()
    skip_if_dev_package()
    local_plan(future::multisession, workers = 2)

    obj    <- make_eval_object(n_configs = 4)
    tmpdir <- withr::local_tempdir()

    result <- suppressWarnings(
      evaluate(obj, allow_par = TRUE, parallelize_over = "configs",
               output_dir = tmpdir, verbose = FALSE, seed = 42L)
    )

    expect_s3_class(result, "horizons_eval")
    expect_equal(nrow(result$evaluation$results), 4)
    expect_true(all(result$evaluation$results$config_id %in%
                      c("cfg_001", "cfg_002", "cfg_003", "cfg_004")))
    expect_false(any(is.na(result$evaluation$results$status)))

    checkpoint_files <- list.files(file.path(tmpdir, "checkpoints"), pattern = "\\.rds$")
    expect_equal(length(checkpoint_files), 4)

  })

  it("matches the sequential run in structure", {

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

    skip_on_cran()
    skip_if_dev_package()
    local_plan(future::multisession, workers = 2)
    before <- future::plan("list")

    obj    <- make_eval_object(n_configs = 4)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(
      evaluate(obj, allow_par = TRUE, output_dir = tmpdir, verbose = FALSE,
               seed = 42L)
    )

    expect_identical(future::plan("list"), before)
    expect_identical(future::nbrOfWorkers(), 2L)

  })

})


## =========================================================================
## Cross-mode resume
## =========================================================================

describe("evaluate() - cross-mode checkpoint resume", {

  it("resumes a configs-axis run from sequential checkpoints", {

    skip_on_cran()
    skip_if_dev_package()

    obj    <- make_eval_object(n_configs = 4)
    tmpdir <- withr::local_tempdir()

    seq_result <- suppressWarnings(
      evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L)
    )

    ## Both axes share the one store (#42): no single file to go stale.
    expect_false(file.exists(file.path(tmpdir, "eval_checkpoint.rds")))

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

write_mock_checkpoints <- function(checkpoint_dir, n = 3) {

  for (i in seq_len(n)) {

    row <- tibble::tibble(
      config_id    = paste0("cfg_", sprintf("%03d", i)),
      model        = "rf",
      status       = "success",
      ## Current schema, as a real row is: the monitor drops the rest (#42).
      scoring_schema = horizons:::SCORING_SCHEMA,
      rpd          = runif(1, 1, 3),
      rsq          = runif(1, 0.5, 0.9),
      rmse         = runif(1, 0.1, 0.5),
      cv_rpd       = runif(1, 1, 3),
      runtime_secs = runif(1, 10, 60)
    )
    saveRDS(row, file.path(checkpoint_dir, paste0(row$config_id, ".rds")))

  }

}

describe("monitor_evaluate()", {

  it("errors on missing directory", {

    expect_error(monitor_evaluate("/nonexistent/path"), "not found")

  })

  it("errors on missing manifest", {

    tmpdir <- withr::local_tempdir()
    expect_error(monitor_evaluate(tmpdir), "eval_manifest")

  })

  it("reads progress from a schema-2 manifest", {

    skip_on_cran()

    tmpdir         <- withr::local_tempdir()
    checkpoint_dir <- file.path(tmpdir, "checkpoints")
    dir.create(checkpoint_dir)

    manifest <- list(
      schema_version             = 2L,
      n_total                    = 10,
      n_pending                  = 10,
      config_ids                 = paste0("cfg_", sprintf("%03d", 1:10)),
      start_time                 = Sys.time() - 3600,
      metric                     = "rpd",
      cv_folds                   = 5L,
      allow_par                  = TRUE,
      parallelize_over_requested = "auto",
      axis                       = "configs",
      tune_parallel_over_requested = NULL,
      plan                       = "multisession",
      workers                    = 10L
    )
    saveRDS(manifest, file.path(tmpdir, "eval_manifest.rds"))
    write_mock_checkpoints(checkpoint_dir, 3)

    output <- capture.output(result <- monitor_evaluate(tmpdir))

    expect_equal(result$n_complete, 3)
    expect_equal(result$n_total, 10)
    expect_false(is.na(result$best_config))
    expect_true(any(grepl("configs", output)))
    expect_true(any(grepl("multisession", output)))

  })

  it("surfaces the training-data fingerprint from a schema-3 manifest", {

    skip_on_cran()

    tmpdir         <- withr::local_tempdir()
    checkpoint_dir <- file.path(tmpdir, "checkpoints")
    dir.create(checkpoint_dir)

    manifest <- list(
      schema_version             = 3L,
      n_total                    = 10,
      n_pending                  = 10,
      config_ids                 = paste0("cfg_", sprintf("%03d", 1:10)),
      start_time                 = Sys.time() - 3600,
      metric                     = "rpd",
      cv_folds                   = 5L,
      allow_par                  = TRUE,
      parallelize_over_requested = "auto",
      axis                       = "configs",
      plan                       = "multisession",
      workers                    = 10L,
      data_hash                  = "abc123def456789",
      data_n_rows                = 1234L
    )
    saveRDS(manifest, file.path(tmpdir, "eval_manifest.rds"))
    write_mock_checkpoints(checkpoint_dir, 3)

    output <- capture.output(monitor_evaluate(tmpdir))

    ## Two runs sharing one output_dir are otherwise indistinguishable here.
    expect_true(any(grepl("1234 training rows", output)))
    expect_true(any(grepl("abc123def456", output)))
    expect_true(any(grepl("multisession", output)))

  })

  it("still reads a legacy (schema-1) manifest from a pre-M2 run", {

    skip_on_cran()

    tmpdir         <- withr::local_tempdir()
    checkpoint_dir <- file.path(tmpdir, "checkpoints")
    dir.create(checkpoint_dir)

    manifest <- list(
      n_total    = 10,
      n_pending  = 10,
      config_ids = paste0("cfg_", sprintf("%03d", 1:10)),
      start_time = Sys.time() - 3600,
      workers    = 10L,
      outer      = 2L,
      inner      = 5L,
      metric     = "rpd",
      cv_folds   = 5L
    )
    saveRDS(manifest, file.path(tmpdir, "eval_manifest.rds"))
    write_mock_checkpoints(checkpoint_dir, 3)

    output <- capture.output(result <- monitor_evaluate(tmpdir))

    expect_equal(result$n_complete, 3)
    expect_true(any(grepl("legacy", output)))

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
## result row is identical.

describe("evaluate() - results are identical across axes", {

  obj <- make_eval_object(n = 60, n_configs = 1)      # rf only
  obj$config$tuning$bayesian_iter <- 2L

  row_cols <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                "cv_rmse", "cv_rrmse", "cv_rsq", "cv_ccc", "cv_rpd", "cv_mae")

  seq_result <- suppressWarnings(
    evaluate(obj, allow_par = FALSE, verbose = FALSE, seed = 42L)
  )

  it("resamples axis on a real two-worker plan matches the sequential run exactly", {

    skip_on_cran()
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

    skip_on_cran()
    skip_if_dev_package()
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
## monitor_evaluate() names the same best config evaluate() does
## =========================================================================

describe("monitor_evaluate() - agrees with evaluate()", {

  it("reports evaluate()'s best_config from the same checkpoints", {

    skip_on_cran()
    obj    <- make_eval_object(n = 60, n_configs = 4)
    tmpdir <- withr::local_tempdir()

    result <- suppressWarnings(
      evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L)
    )

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
    obj    <- make_eval_object(n = 60, n_configs = 4)
    tmpdir <- withr::local_tempdir()

    result <- suppressWarnings(
      evaluate(obj, output_dir = tmpdir, prune = FALSE, verbose = FALSE,
               seed = 42L)
    )

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

    tmpdir <- withr::local_tempdir()
    ckpt   <- file.path(tmpdir, "checkpoints")
    dir.create(ckpt)

    first_run  <- eval_settings(grid_size = 2L)
    second_run <- eval_settings(grid_size = 3L)

    manifest <- list(
      schema_version = 4L,
      n_total        = 1,
      n_pending      = 1,
      config_ids     = "cfg_001",
      start_time     = Sys.time() - 60,
      metric         = "rpd",
      axis           = "configs",
      plan           = "multisession",
      workers        = 2L,
      settings       = first_run
    )
    saveRDS(manifest, file.path(tmpdir, "eval_manifest.rds"))

    ## The one row was tuned by the second run.
    saveRDS(
      tibble::tibble(config_id = "cfg_001", status = "success",
                     scoring_schema = horizons:::SCORING_SCHEMA,
                     rpd = 2, cv_rpd = 2, settings = list(second_run)),
      file.path(ckpt, "cfg_001.rds")
    )

    ## After the first poll, the second run starts and rewrites the manifest.
    ## A monitor still holding the first manifest ignores the row forever;
    ## the guard turns that hang into a failure.
    polls <- 0L

    local_mocked_bindings(.render_monitor = function(stats, manifest) {

      polls <<- polls + 1L

      if (polls == 1L) {
        manifest$settings <- second_run
        saveRDS(manifest, file.path(tmpdir, "eval_manifest.rds"))
      }

      if (polls > 3L) stop("still gating against the first run's manifest")

    })

    invisible(capture.output(
      stats <- monitor_evaluate(tmpdir, watch = TRUE, interval = 0)
    ))

    expect_equal(stats$n_complete, 1)
    expect_equal(polls, 2L)

  })

  it("reads a legacy single file exactly where evaluate() would", {

    skip_on_cran()
    obj    <- make_eval_object(n = 60, n_configs = 2)
    tmpdir <- withr::local_tempdir()

    result <- suppressWarnings(
      evaluate(obj, output_dir = tmpdir, prune = FALSE, verbose = FALSE,
               seed = 42L)
    )

    ## A legacy file holding both configs, where cfg_002 won by a mile.
    files <- list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)
    stale <- dplyr::bind_rows(lapply(files, readRDS))
    stale$cv_rpd[stale$config_id == "cfg_002"] <- 999
    saveRDS(stale, file.path(tmpdir, "eval_checkpoint.rds"))

    ## cfg_001 has no per-config file, so its legacy row counts; cfg_002's
    ## per-config file shadows the legacy row.
    unlink(file.path(tmpdir, "checkpoints", "cfg_001.rds"))

    invisible(capture.output(stats <- monitor_evaluate(tmpdir)))

    expect_equal(stats$n_complete, 2)
    expect_equal(stats$best_config, result$evaluation$best_config)
    expect_false(isTRUE(stats$best_metric == 999))

  })

})
