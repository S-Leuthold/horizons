## ===========================================================================
## 06 — MBL sweep on the coherent batches: k x preprocessing
## ===========================================================================
##
## Purpose (2026-09-17). In 05-coherent-batch.R arm M ran per-sample mbl at
## one setting (snv, k = 200, the KSSL neighbour-validation choice) and lost
## on both batches: RMSE 0.451 on MOYS against 0.32-0.35 for the pool arms,
## and 0.158 on the Nebraska KSSL batch against 0.120 for global Cubist.
## Everything that went bad on MOYS ran SNV without the derivative, so the
## MOYS loss may be preprocessing rather than locality, and the k was chosen
## on KSSL unknowns, not on the batch. This script runs mbl on both batches
## over k = 50 to 1,600 and both preprocessings in one call per chunk, scored
## on the same targets 05 scored, so the curves are directly comparable.
##
## Reuses 05's target checkpoints (results/coherent-batch/checkpoints/
## <batch>-targets.qs2: ids, y) and its pool (oc train_core at 4 cm-1).
## Writes results/coherent-batch/mbl-sweep.csv and preds/preds-<batch>-Msweep-<prep>.csv.
##
## Run (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_mbl_sweep.sh [--workers=N] [--batches=moys,kssl] > /dev/null 2>&1 &
## ===========================================================================

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")

args    <- commandArgs(trailingOnly = TRUE)
flags   <- args[startsWith(args, "--")]
flag_of <- function(name, default) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="), flags, value = TRUE))
  if (length(v)) v else default
}
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
exp1_dir <- file.path(dirname(this_dir), "2026-09-local-strategy")

source(file.path(exp1_dir, "00-config.R"))
suppressPackageStartupMessages({ library(horizons); library(resemble); library(dplyr); library(tibble) })
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)

EXPERIMENT_RESAMPLE <- 4
PROPERTY <- "oc"; TR <- "log"
workers  <- as.integer(flag_of("workers", "4"))
BATCHES  <- strsplit(flag_of("batches", "moys,kssl"), ",")[[1]]
K_GRID   <- c(50L, 100L, 200L, 400L, 800L, 1600L)
PREPS    <- c("snv", "snv_deriv1")
WAPLS_RANGE <- EXP_MBL$pls_c; PLS_NCOMP <- 20L; dry <- FALSE

OUT   <- file.path(this_dir, "results", "coherent-batch")
PREDS <- file.path(OUT, "preds"); PIVOT_DIR <- OUT
rows_path <- file.path(OUT, "mbl-sweep.csv")

snap <- load_snapshot()
source(file.path(this_dir, "pm-helpers.R"))   # forward_y, inverse_y, pred_at_k, nnv_table

sp <- load_splits(PROPERTY); assert_splits(sp)
pool_ids <- sp$train_core
y_of <- function(ids) snap$lab[[PROPERTY]][match(ids, snap$lab$sample_id)]
pool_hz <- hz_spectra(snap, pool_ids)
Yr <- forward_y(y_of(pool_ids), TR)
msg("[sweep] pool %d rows at %d cm-1; k %s; preps %s; workers %d", length(pool_ids), EXPERIMENT_RESAMPLE,
    paste(K_GRID, collapse = ","), paste(PREPS, collapse = ","), workers)

prep_from_hz <- function(hz, prep) {
  d <- hz$data$analysis; cols <- wn_cols_of(d)
  X <- as.matrix(d[, cols, drop = FALSE]); rownames(X) <- d$sample_id
  X <- prospectr::standardNormalVariate(X)
  if (prep == "snv_deriv1") X <- prospectr::savitzkyGolay(X, m = EXP_LOCAL$sg_m, p = EXP_LOCAL$sg_p, w = EXP_LOCAL$sg_w)
  X[!is.finite(X)] <- 0
  X
}

## The MOYS targets were built by 05 from OPUS files onto the pool grid; the
## kssl targets are snapshot ids. Rebuild the target spectra the same way 05
## did, from the checkpointed ids (05 did not checkpoint the hz object).
moys_hz <- function(ids) {
  MOYS_OPUS <- "/data/workshop/projects/ai-leaf/data/processed/MOYS/opus_files"
  raw <- spectra(MOYS_OPUS, type = "opus"); d <- raw$data$analysis
  sid <- sub("^MOYS_(S[0-9]+-[0-9]+)_.*$", "\\1", d$sample_id)
  raw_cols <- grep("^wn_", names(d), value = TRUE); raw_wn <- as.numeric(sub("^wn_", "", raw_cols))
  pool_cols <- wn_cols_of(pool_hz$data$analysis); pool_wn <- as.numeric(sub("^wn_", "", pool_cols))
  R <- as.matrix(d[, raw_cols, drop = FALSE])
  X <- t(apply(R, 1, function(y) stats::approx(raw_wn, y, xout = pool_wn)$y)); colnames(X) <- pool_cols
  Xm <- rowsum(X, sid) / as.vector(table(sid)[sort(unique(sid))])
  df <- data.frame(sample_id = rownames(Xm), Xm, check.names = FALSE, stringsAsFactors = FALSE)
  df <- df[df$sample_id %in% ids, ]
  std(spectra(df, id_col = "sample_id"))
}

## One mbl() call per chunk returns predictions for EVERY k at once; the
## dissimilarity is computed once in the parent and sliced, as in pm-helpers.
mbl_sweep <- function(Xr, Yr, Xu, ids, ks, chunk_size = NULL) {
  ## chunks of equal size, never a chunk of one: get_predictions() on a
  ## single unknown comes back in a shape pred_at_k() cannot read (20:18 failure).
  if (is.null(chunk_size)) chunk_size <- ceiling(nrow(Xu) / max(1L, ceiling(nrow(Xu) / 50L)))
  D <- resemble::dissimilarity(Xr, Xu, diss_method = resemble::diss_pca(ncomp = resemble::ncomp_by_opc(40L)), Yr = Yr)
  msg("[sweep] dissimilarity %d x %d (opc chose %s components)", nrow(D$dissimilarity), ncol(D$dissimilarity), paste(D$ncomp, collapse = ","))
  D <- D$dissimilarity
  idx    <- split(seq_len(nrow(Xu)), ceiling(seq_len(nrow(Xu)) / chunk_size))
  slices <- lapply(idx, function(i) list(D = D[, i, drop = FALSE], Xu = Xu[i, , drop = FALSE], ids = ids[i]))
  rm(D); invisible(gc())
  old <- options(future.globals.maxSize = 2 * 1024^3); on.exit(options(old), add = TRUE)
  out <- future.apply::future_lapply(slices, function(s) {
    m <- resemble::mbl(Xr = Xr, Yr = Yr, Xu = s$Xu,
                       neighbors   = resemble::neighbors_k(ks),
                       diss_method = s$D,
                       fit_method  = resemble::fit_wapls(min_ncomp = WAPLS_RANGE[1], max_ncomp = WAPLS_RANGE[2]),
                       control     = resemble::mbl_control(validation_type = "none", allow_parallel = FALSE, blas_threads = 1L),
                       verbose = FALSE, seed = SEED)
    bind_rows(lapply(ks, function(k) { p <- pred_at_k(m, k, s$ids); p$k <- k; p }))
  }, future.seed = TRUE, future.globals = list(Xr = Xr, Yr = Yr, ks = ks, WAPLS_RANGE = WAPLS_RANGE, SEED = SEED, pred_at_k = pred_at_k),
     future.packages = c("resemble", "tibble", "dplyr"), future.scheduling = Inf)
  bind_rows(out)
}

future::plan(future::multicore, workers = workers)

for (batch_name in BATCHES) {
  t   <- qs2::qs_read(file.path(OUT, "checkpoints", sprintf("%s-targets.qs2", batch_name)))
  ids <- t$ids; y <- t$y
  hz  <- if (batch_name == "moys") moys_hz(ids) else hz_spectra(snap, ids)
  msg("[%s] %d targets", batch_name, length(ids))
  for (prep in PREPS) {
    t_m <- system.time({
      Xr <- prep_from_hz(pool_hz, prep)[pool_ids, , drop = FALSE]
      Xu <- prep_from_hz(hz, prep)[ids, , drop = FALSE]
      pm <- mbl_sweep(Xr, Yr, Xu, ids, K_GRID)
    })
    pm$.pred <- inverse_y(pm$.pred_t, TR); pm$truth <- y[match(pm$sample_id, ids)]
    pm$batch <- batch_name; pm$prep <- prep
    write.csv(pm, file.path(PREDS, sprintf("preds-%s-Msweep-%s.csv", batch_name, prep)), row.names = FALSE)
    for (k in K_GRID) {
      p <- pm[pm$k == k, ]; m <- metrics_row(p$truth, p$.pred)
      row <- tibble(batch = batch_name, arm = "M", prep = prep, k = k, n_targets = length(ids), n_scored = m$n_scored,
                    rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, rsq = m$rsq,
                    secs = t_m[["elapsed"]], ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
      write.table(row, rows_path, sep = ",", row.names = FALSE, col.names = !file.exists(rows_path), append = file.exists(rows_path))
      msg("[%s M %s k=%d] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", batch_name, prep, k, m$rmse, m$bias, m$ccc, m$rpd)
    }
    rm(Xr, Xu, pm); invisible(gc())
  }
}
future::plan(future::sequential)
msg("[sweep] done.")
print(as.data.frame(read.csv(rows_path)[, c("batch", "prep", "k", "rmse", "bias", "ccc", "rpd")]))
