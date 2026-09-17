## ===========================================================================
## 00 — Shakedown of the M2/M3 parallelism on the real library at 2 cm-1
## ===========================================================================
##
## Purpose (2026-09-15). Before Experiment 2 is built on evaluate()'s
## user-managed backend, run the configs loop once at full resolution on the
## KSSL snapshot and record what it costs: which loop was parallelised, how
## many workers, seconds per config, seconds for fit(), memory from the
## watchdog / RSS logs alongside. Those numbers set Experiment 2's grid size
## and worker count. This is NOT a modelling result: the grid is six configs
## chosen to clear the "auto" threshold (n_configs >= cv_folds) so the configs
## loop is what runs, and to include plsr, which arm P needs.
##
## Experiment 1 (../2026-09-local-strategy/) is reused for the snapshot, the
## splits, and the chain builders, and is NOT written to: every artifact of
## this script lands under results/shakedown/ in THIS directory.
##
## Run (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_shakedown.sh > /dev/null 2>&1 &
##   Rscript dev/experiments/2026-09-factorial/00-shakedown.R [clay oc ph] [--workers=N] [--plan=callr|multisession]
##
## Backend (2026-09-16). Default is future.callr: one fresh R process per
## config, so a worker's memory is returned to the OS when its config ends.
## The 9/15 run on multisession was killed by the watchdog because persistent
## workers never release: a worker that peaked at ~3.5 GB during prep sat at
## 2.2-2.9 GB resident afterwards (R itself using ~0.85 GB; the rest glibc
## holding freed pages, released only by malloc_trim), and six of those were
## still resident when oc dispatched. Pass --plan=multisession to reproduce.

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading

args     <- commandArgs(trailingOnly = TRUE)
flags    <- args[startsWith(args, "--")]
props    <- setdiff(args, flags)
w_arg    <- sub("^--workers=", "", grep("^--workers=", flags, value = TRUE))
plan_arg <- sub("^--plan=", "", grep("^--plan=", flags, value = TRUE))
plan_name <- if (length(plan_arg)) match.arg(plan_arg, c("callr", "multisession")) else "callr"
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
exp1_dir <- file.path(dirname(this_dir), "2026-09-local-strategy")

source(file.path(exp1_dir, "00-config.R"))
suppressPackageStartupMessages(library(horizons))
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)

## ---------------------------------------------------------------------------
## Shakedown settings: override Experiment 1's globals AFTER sourcing them
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- NULL          # full resolution: std() -> resample = 2
SHAKE_DIR   <- file.path(this_dir, "results", "shakedown")
SHAKE_CKPT  <- file.path(SHAKE_DIR, "checkpoints")
for (d in c(SHAKE_DIR, SHAKE_CKPT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

## Six configs: 3 models x 2 preprocessings, PCA everywhere. Six >= 5 folds,
## so parallelize_over = "auto" resolves to "configs" and one worker takes
## one config. plsr is here because Experiment 2's arm P needs its cost.
SHAKE_SET <- expand.grid(model             = c("cubist", "rf", "plsr"),
                         preprocessing     = c("snv", "snv_deriv1"),
                         feature_selection = "pca",
                         stringsAsFactors  = FALSE)

if (!length(props)) props <- EXP_PROPERTIES$property
workers <- if (length(w_arg)) as.integer(w_arg) else nrow(SHAKE_SET)
stopifnot(workers >= 1L, workers <= MAX_WORKERS)

if (plan_name == "callr") {
  future::plan(future.callr::callr, workers = workers)
} else {
  future::plan(future::multisession, workers = workers)
}
on.exit(future::plan(future::sequential), add = TRUE)

snap <- load_snapshot()
msg("[shakedown] snapshot %d samples; properties: %s; plan %s(%d); %d configs at 2 cm-1",
    nrow(snap$lab), paste(props, collapse = ", "), plan_name, workers, nrow(SHAKE_SET))

mem_avail_gb <- function() {
  as.numeric(sub(".*:\\s*(\\d+).*", "\\1", grep("MemAvailable", readLines("/proc/meminfo"), value = TRUE))) / 1024^2
}

for (property in props) {

  sp   <- load_splits(property); assert_splits(sp)
  spec <- property_spec(property)

  msg("[shakedown] %s: building train_core object (%d rows, transformation = %s); MemAvailable %.1f GB",
      property, length(sp$train_core), spec$transformation, mem_avail_gb())
  t_build <- system.time(
    hz <- build_hz(snap, sp$train_core, property, spec$transformation, set = SHAKE_SET)
  )
  n_wn <- length(wn_cols_of(hz$data$analysis %||% data.frame()))
  msg("[shakedown] %s: %d configs, %d wavenumber columns, build %.1f s",
      property, hz$config$n_configs, n_wn, t_build[["elapsed"]])

  eval_dir <- file.path(SHAKE_CKPT, paste0(property, "-eval"))
  mem_before <- mem_avail_gb()
  t_eval <- system.time(
    hz <- eval_exp(hz, eval_dir)   # allow_par = nbrOfWorkers() > 1, parallelize_over = "auto"
  )
  res <- hz$evaluation$results
  msg("[shakedown] %s: evaluate() %.1f min over %s on %d worker(s); statuses: %s; MemAvailable %.1f -> %.1f GB",
      property, t_eval[["elapsed"]] / 60, hz$evaluation$parallelize_over, hz$evaluation$workers,
      paste(res$status, collapse = ","), mem_before, mem_avail_gb())
  print(as.data.frame(res[, intersect(c("config_id", "status", "runtime_secs", "cv_rpd", "cv_rmse", "rpd", "rmse"),
                                      names(res))]), row.names = FALSE)

  t_fit <- system.time(
    fit_obj <- fit(hz, n_best = 1L, compute_uq = TRUE, compute_ad = TRUE,
                   allow_par = workers > 1L, seed = SEED)
  )
  fit_bytes <- artifact_bytes(fit_obj)
  msg("[shakedown] %s: fit() %.1f min; winner %s; fitted object %.0f MB",
      property, t_fit[["elapsed"]] / 60, hz$evaluation$best_config, fit_bytes / 1024^2)

  row <- tibble(
    property        = property,
    resolution_cm   = 2L,
    n_train         = length(sp$train_core),
    n_wavenumbers   = n_wn,
    n_configs       = nrow(SHAKE_SET),
    plan            = plan_name,
    plan_workers    = workers,
    axis            = hz$evaluation$parallelize_over,
    workers_seen    = hz$evaluation$workers,
    secs_build      = t_build[["elapsed"]],
    secs_evaluate   = t_eval[["elapsed"]],
    secs_fit        = t_fit[["elapsed"]],
    config_secs_min = min(res$runtime_secs, na.rm = TRUE),
    config_secs_max = max(res$runtime_secs, na.rm = TRUE),
    statuses        = paste(res$status, collapse = ","),
    best_config     = hz$evaluation$best_config,
    best_cv_rpd     = max(res$cv_rpd, na.rm = TRUE),
    fit_bytes       = fit_bytes,
    horizons_sha    = tryCatch(system2("git", c("-C", shQuote(PKG_DIR), "rev-parse", "--short", "HEAD"),
                                       stdout = TRUE), error = function(e) NA_character_),
    ran_at          = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  print(as.data.frame(row))

  readr::write_csv(row, file.path(SHAKE_DIR, paste0("row-", property, ".csv")))
  qs2::qs_save(list(row = row, eval_results = res, evaluation = hz$evaluation),
               file.path(SHAKE_CKPT, paste0(property, "-eval.qs2")))
  rm(fit_obj, hz); invisible(gc())
  msg("[shakedown] %s: done in %.1f min total.", property,
      (t_build[["elapsed"]] + t_eval[["elapsed"]] + t_fit[["elapsed"]]) / 60)
}

rows <- do.call(rbind, lapply(list.files(SHAKE_DIR, "^row-.*\\.csv$", full.names = TRUE), readr::read_csv,
                              show_col_types = FALSE))
readr::write_csv(rows, file.path(SHAKE_DIR, "shakedown.csv"))
msg("[shakedown] all done; summary at %s", file.path(SHAKE_DIR, "shakedown.csv"))
