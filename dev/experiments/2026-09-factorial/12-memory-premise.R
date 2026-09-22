## ===========================================================================
## 12 — Can the laptop run the full-library workflow at all?
## ===========================================================================
##
## Purpose (2026-09-21, overnight). The library design rests on a premise
## nobody has measured: that a user's laptop cannot hold the full KSSL
## library through configure -> validate -> evaluate -> fit -> predict, and
## that this is why the product ships a thinned library and a fitted model
## rather than the pool. The premise is stated everywhere and tested
## nowhere.
##
## This script measures it. It runs the whole chain once, on the real clay
## train_core (17,788 rows) at OVERNIGHT$resample cm-1, with the experiment's
## four-config set, entirely sequential — one R process, no workers, which is
## the laptop case. At every stage boundary it records wall seconds, R's own
## gc() high-water mark, and the process RSS read from /proc/self/status.
## VmHWM is the peak the kernel saw, and it is the number the verdict uses.
##
## MEASURED, NOT ENFORCED. Nothing here stops when memory gets large, and
## nothing is tuned to fit. The row it writes says what the workflow costs;
## the verdict column (fits_laptop) is just that cost compared against
## OVERNIGHT$laptop_ram_gb. If the premise is wrong, this is where it breaks.
##
## Two artifact sizes come with it, because "can the laptop run it" and "what
## would we ship" are the same design question asked twice: the standardized
## pool serialized as qs2 (what shipping the library itself would cost) and
## the fitted object serialized as qs2 (what shipping a model costs instead).
## Both are read against OVERNIGHT$artifact_mb.
##
## Parallelism: none, deliberately. future::plan(sequential) and
## allow_par = FALSE everywhere, with ranger held to one thread by
## HORIZONS_THREAD_CONTROL. A parallel plan would measure the box, not the
## laptop: the workers' copies are the thing the laptop does not have room
## for, and a callr worker's RSS is not in this process's VmHWM anyway.
##
## Run (from package/, against the INSTALLED horizons). The runner wraps the
## script in /usr/bin/time -v, so the kernel's own maximum resident set is
## recorded alongside the script's self-report:
##   nohup /usr/bin/time -v Rscript dev/experiments/2026-09-factorial/12-memory-premise.R [--dry] > ... 2>&1 &
##
## --dry: 2,000 pool rows, one config, 1,000 test rows. It exercises the
## path; it does not measure the premise.
##
## Results: results/memory-premise/ — memory-premise.csv (one row per run),
## checkpoints/ (evaluate() output dir), and nothing else.
## ===========================================================================

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading
args    <- commandArgs(trailingOnly = TRUE)
flags   <- args[startsWith(args, "--")]
dry     <- "--dry" %in% flags

file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
exp1_dir <- file.path(dirname(this_dir), "2026-09-local-strategy")

source(file.path(exp1_dir, "00-config.R"))
source(file.path(this_dir, "overnight-config.R"))
suppressPackageStartupMessages({
  library(horizons)
  library(dplyr)
  library(tibble)
})
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)
for (p in c("workflows", "parsnip", "recipes", "ranger", "rules", "Cubist")) loadNamespace(p)

## ---------------------------------------------------------------------------
## Settings — every number from OVERNIGHT
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- OVERNIGHT$resample          # std() reads this
PROPERTY            <- OVERNIGHT$property
TR                  <- OVERNIGHT$transformation
TUNING              <- EXP_CONFIG
CONFIG_SET          <- if (dry) EXP_CONFIG_SET[1, , drop = FALSE] else EXP_CONFIG_SET
N_POOL_DRY          <- 2000L
N_TEST_DRY          <- 1000L

OUT   <- file.path(this_dir, "results", "memory-premise")
## Pilot checkpoints live apart from real ones: evaluate() resumes from an
## eval_checkpoint.rds keyed by config_id alone, with no check on the
## training data, so a --dry run under the same tag would silently seed the
## real run's hyperparameters (10-metric-accuracy.R, 2026-09-21).
CKPT  <- file.path(OUT, if (dry) "checkpoints-pilot" else "checkpoints")
for (d in c(OUT, CKPT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
rows_path <- file.path(OUT, "memory-premise.csv")

STAGES <- c("build", "configure", "validate", "evaluate", "fit", "predict")

## ---------------------------------------------------------------------------
## Measurement
## ---------------------------------------------------------------------------

## /proc/self/status, in kB. VmRSS is resident now; VmHWM is the peak
## resident set since the process started, which no amount of gc() lowers.
## That is the honest "did it fit" number.
proc_kb <- function(field) {
  st <- readLines("/proc/self/status", warn = FALSE)
  ln <- grep(paste0("^", field, ":"), st, value = TRUE)
  if (!length(ln)) return(NA_real_)
  as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", ln[1]))
}
rss_gb <- function() proc_kb("VmRSS") / 1024 / 1024
hwm_gb <- function() proc_kb("VmHWM") / 1024 / 1024

## gc()'s last column is "max used" in Mb, for both Ncells and Vcells. It is
## R's own high-water mark, which excludes anything held outside R's heap
## (BLAS scratch, a ranger forest's C++ side), so it is always the smaller
## and more optimistic of the two numbers here. Both get recorded.
gc_max_gb <- function() { g <- gc(verbose = FALSE); sum(g[, ncol(g)]) / 1024 }

secs <- setNames(rep(NA_real_, length(STAGES)), STAGES)
rssg <- setNames(rep(NA_real_, length(STAGES)), STAGES)
gcg  <- setNames(rep(NA_real_, length(STAGES)), STAGES)

## Time one stage, then read memory at its boundary. `expr` is a promise and
## is forced inside system.time(), so the timing covers the stage itself.
stage <- function(name, expr) {
  el  <- system.time(value <- expr)[["elapsed"]]
  g   <- gc_max_gb()                      # collects first, so rss is post-gc
  r   <- rss_gb()
  secs[[name]] <<- el
  rssg[[name]] <<- r
  gcg[[name]]  <<- g
  msg("[stage %-9s] %8.1f s | RSS %5.2f GB | gc max %5.2f GB | peak %5.2f GB",
      name, el, r, g, hwm_gb())
  value
}

## ---------------------------------------------------------------------------
## Already recorded?
## ---------------------------------------------------------------------------

if (file.exists(rows_path)) {
  prev <- read.csv(rows_path, stringsAsFactors = FALSE)
  if (any(prev$pilot == dry)) {
    msg("[memory] a %s row is already in %s; nothing to do", if (dry) "pilot" else "real", rows_path)
    print(as.data.frame(prev[prev$pilot == dry, ]), row.names = FALSE, digits = 3)
    quit(save = "no", status = 0)
  }
}

## ---------------------------------------------------------------------------
## The run
## ---------------------------------------------------------------------------

future::plan(future::sequential)
stopifnot(future::nbrOfWorkers() == 1L)

snap  <- load_snapshot()
sp    <- load_splits(PROPERTY)
assert_splits(sp)

train_ids <- sp$train_core
test_ids  <- sp$test
if (dry) {
  set.seed(SEED)
  train_ids <- sample(train_ids, N_POOL_DRY)
  test_ids  <- sample(test_ids,  N_TEST_DRY)
}

msg("[memory] %s | %d train rows, %d test rows | %d config(s) | resample %s cm-1 | dry %s",
    PROPERTY, length(train_ids), length(test_ids), nrow(CONFIG_SET), EXPERIMENT_RESAMPLE, dry)
msg("[memory] baseline before any data: RSS %.2f GB, peak %.2f GB", rss_gb(), hwm_gb())

## ---- build: standardized spectra + outcome ------------------------------
hz <- stage("build", {
  h   <- hz_spectra(snap, train_ids)
  lab <- snap$lab[match(train_ids, snap$lab$sample_id), c("sample_id", PROPERTY), drop = FALSE]
  stopifnot(!anyNA(lab[[PROPERTY]]))
  add_response(h, source = lab, variable = PROPERTY)
})
n_rows       <- hz$data$n_rows
n_predictors <- hz$data$n_predictors

## The artifact-size reference: what shipping the standardized library itself
## would cost on disk. Measured here, before configure() attaches the grid,
## and not retained — holding a second copy would inflate every later stage.
pool_size_mb <- artifact_bytes(hz) / 1024^2
msg("[memory] standardized pool: %d rows x %d predictors, %.1f MB as qs2 (budget %d MB)",
    n_rows, n_predictors, pool_size_mb, OVERNIGHT$artifact_mb)
invisible(gc())

## ---- configure ----------------------------------------------------------
hz <- stage("configure", {
  h <- configure(hz,
                 outcome             = PROPERTY,
                 models              = unique(CONFIG_SET$model),
                 transformations     = TR,
                 preprocessing       = unique(CONFIG_SET$preprocessing),
                 feature_selection   = unique(CONFIG_SET$feature_selection),
                 cv_folds            = TUNING$cv_folds,
                 grid_size           = TUNING$grid_size,
                 bayesian_iter       = TUNING$bayesian_iter,
                 final_bayesian_iter = TUNING$final_bayesian_iter)
  select_configs(h, CONFIG_SET)
})
n_configs <- hz$config$n_configs

## ---- validate -----------------------------------------------------------
hz <- stage("validate", validate(hz))

## ---- evaluate (sequential) ----------------------------------------------
hz <- stage("evaluate", eval_exp(hz, file.path(CKPT, "full"), verbose = FALSE))

## ---- fit, with UQ and AD ------------------------------------------------
f <- stage("fit", fit(hz, n_best = 1L, compute_uq = TRUE, compute_ad = TRUE,
                      allow_par = FALSE, seed = SEED, verbose = FALSE))

## ---- predict on the held-out test split, with intervals ------------------
## The test spectra object is built inside the stage on purpose: on the
## laptop that matrix is part of what prediction costs, not a free input.
preds <- stage("predict", {
  hz_test <- hz_spectra(snap, test_ids)
  as_tibble(predict(f, hz_test, interval = TRUE))
})
stopifnot(nrow(preds) == length(test_ids))

## ---------------------------------------------------------------------------
## Sizes and the row
## ---------------------------------------------------------------------------

fit_size_mb <- artifact_bytes(f) / 1024^2
peak_rss_gb <- hwm_gb()

## Experiment 1's clay A_global checkpoint, for scale — an already-shipped
## fitted object of the same shape, read off disk without loading it.
if (has_ckpt(PROPERTY, "A_global")) {
  msg("[memory] for reference, experiment 1's %s A_global checkpoint is %.1f MB on disk",
      PROPERTY, file.size(ckpt_path(PROPERTY, "A_global")) / 1024^2)
}

row <- tibble(
  n_rows        = n_rows,
  n_predictors  = n_predictors,
  n_configs     = as.integer(n_configs),
  n_test        = length(test_ids),
  resample      = EXPERIMENT_RESAMPLE
)
for (s in STAGES) row[[paste0("secs_", s)]]       <- unname(secs[[s]])
row$secs_total <- sum(secs, na.rm = TRUE)
row$peak_rss_gb <- peak_rss_gb
for (s in STAGES) row[[paste0("rss_after_", s, "_gb")]] <- unname(rssg[[s]])
for (s in STAGES) row[[paste0("gc_after_",  s, "_gb")]] <- unname(gcg[[s]])
row$gc_max_used_gb <- max(gcg, na.rm = TRUE)
row$fit_size_mb    <- fit_size_mb
row$pool_size_mb   <- pool_size_mb
row$artifact_mb    <- OVERNIGHT$artifact_mb
row$laptop_ram_gb  <- OVERNIGHT$laptop_ram_gb
row$fits_laptop    <- peak_rss_gb <= OVERNIGHT$laptop_ram_gb
row$pilot          <- dry
row$ran_at         <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

write.table(row, rows_path, sep = ",", row.names = FALSE,
            col.names = !file.exists(rows_path), append = file.exists(rows_path))

msg("[memory] peak RSS %.2f GB against a %d GB laptop: fits_laptop = %s",
    peak_rss_gb, OVERNIGHT$laptop_ram_gb, row$fits_laptop)
msg("[memory] fitted object %.1f MB, standardized pool %.1f MB as qs2", fit_size_mb, pool_size_mb)
print(as.data.frame(row[, c("n_rows", "n_configs", "secs_total", "peak_rss_gb",
                            "gc_max_used_gb", "fit_size_mb", "pool_size_mb", "fits_laptop")]),
      row.names = FALSE, digits = 4)
msg("[memory] done")
