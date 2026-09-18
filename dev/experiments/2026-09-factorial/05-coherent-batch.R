## ===========================================================================
## 05 — The coherent batch: does one training set drawn around a real batch
##      keep the per-sample gain?
## ===========================================================================
##
## Purpose (2026-09-17). Yesterday's evidence for locality is per-sample: one
## neighbourhood and one fit per unknown (arm M), and the locality curve's
## 1,000 unknowns were scattered across the whole library, the worst case for
## any shared pool. The selection verb (dev/specs/v1-refactor/
## select-training-design.md) proposes something cheaper: draw ONE training
## set as the union of every target's k nearest pool rows and run the
## ordinary horizons grid on it. This script asks whether that one model, on
## the same targets, keeps most of what per-sample mbl gains over the global
## model. It is the number the verb's design rests on.
##
## The verb is not written; the selection here is done by hand in the
## similarity space the spec fixes (SNV, SG 1st derivative 11/2, water bands
## out, PCA of the pool to 99 % capped at 100, Mahalanobis on the scores).
## This script is the reference the verb is tested against later.
##
## Batches (targets), each scored on its own rows only:
##   moys  the product case. 101 MOYS samples (Michigan, 10 farms, 0-10 cm)
##         with bulk C, scanned at CSU, so this carries an instrument /
##         lab transfer on top of locality. Replicate scans are averaged
##         to one spectrum per sample. Bulk C (g/kg) -> % C.
##   kssl  the within-library control: a seeded KSSL test sample and its 100
##         nearest neighbours by lat/long among test rows with coordinates.
##         Same script, no transfer. Says whether a failure on moys is the
##         verb or the transfer.
##
## Pool: experiment 1's oc train_core (31,130 rows), so the numbers line up
## with the pivot. Both sides standardized to 4 cm-1 (full resolution bought
## nothing yesterday; experiment 1's global fit is at 4).
##
## Arms, all scored on the same targets with RMSE / bias / CCC as the
## headline (the MOYS range is narrow, RPD is reported for continuity only):
##   G  global: experiment 1's arm-A oc fit (cubist, snv_deriv1, log, pca),
##      loaded from its checkpoint and asked to predict. No refit.
##   B  batch: one pool = union of every target's k nearest pool rows, k in
##      K_SWEEP; the eight-config grid; evaluate() picks; fit() refits the
##      winner; predict the targets. Pool size per k is recorded.
##   C  cluster: the TARGETS are clustered in the similarity space (k-means,
##      K by silhouette, clusters under MIN_CLUSTER merged into the nearest);
##      one union pool, one grid, one model per cluster; each target predicted
##      by its cluster's model. "Neighbourhoods of the input data."
##   M  per-sample mbl on the full pool (wapls 5-20, k = MBL_K, the pivot's
##      oc choice), the ceiling the batch arms are measured against.
##
## Run (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_coherent.sh [--workers=N] [--arms=G,B,C,M] [--batches=moys,kssl] [--k=100,200,400,800] [--dry] > /dev/null 2>&1 &
##
## Results: results/coherent-batch/ — coherent-batch.csv (one row per
## batch x arm x k), pool-sizes.csv, preds/*.csv (per-target predictions with
## cluster and nearest-pool distance), checkpoints/ (evaluate() output dirs).
## ===========================================================================

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading

args      <- commandArgs(trailingOnly = TRUE)
flags     <- args[startsWith(args, "--")]
dry       <- "--dry" %in% flags
flag_of   <- function(name, default) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="), flags, value = TRUE))
  if (length(v)) v else default
}
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
exp1_dir <- file.path(dirname(this_dir), "2026-09-local-strategy")

source(file.path(exp1_dir, "00-config.R"))
suppressPackageStartupMessages({
  library(horizons)
  library(resemble)
  library(dplyr)
  library(tibble)
})
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)
## predict() on a DESERIALIZED horizons_fit fails with "no applicable method
## for 'predict' applied to butchered_workflow" unless the workflows namespace
## is loaded; horizons does not load it on attach, only as a side effect of
## fitting. Filed 2026-09-17. Loaded here so arm G can use experiment 1's
## checkpointed global fit.
for (p in c("workflows", "parsnip", "recipes", "rules", "Cubist")) loadNamespace(p)

## ---------------------------------------------------------------------------
## Settings
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- 4          # std() -> resample = 4; matches experiment 1 and its A_global fit
PROPERTY    <- "oc"
TR          <- "log"              # experiment 1's oc transformation
workers     <- if (dry) 2L else as.integer(flag_of("workers", "8"))
ARMS        <- strsplit(flag_of("arms", "G,B,C,M"), ",")[[1]]
BATCHES     <- strsplit(flag_of("batches", "moys,kssl"), ",")[[1]]
K_SWEEP     <- as.integer(strsplit(flag_of("k", if (dry) "100,200" else "100,200,400,800"), ",")[[1]])
MBL_K       <- 200L               # pivot's NNv choice for oc (03-pivot-pm.R, 2026-09-16)
MBL_PREP    <- "snv"              # pivot's winning preprocessing for oc's M arm
MIN_CLUSTER <- 30L                # selection-prior-art.md: ~30 rows is the defensible floor
KSSL_BATCH_N <- 100L

MOYS_OPUS <- "/data/workshop/projects/ai-leaf/data/processed/MOYS/opus_files"
MOYS_CSV  <- "/data/workshop/projects/ai-leaf/data/raw/MOYS.csv"
SITE_RAW  <- file.path(RAW_DIR, "ossl_soilsite_L0_v1.2.csv.gz")

OUT   <- file.path(this_dir, "results", "coherent-batch")
CKPT  <- file.path(OUT, "checkpoints")
PREDS <- file.path(OUT, "preds")
for (d in c(OUT, CKPT, PREDS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

## The eight-config grid: three families x two preprocessings on PCA, plus
## plsr on the full width (the one learner that handles it; trees stay on
## PCA per #40).
GRID <- data.frame(
  model             = c("cubist", "cubist", "rf",  "rf",         "plsr", "plsr",       "plsr", "plsr"),
  preprocessing     = c("snv",    "snv_deriv1", "snv", "snv_deriv1", "snv",  "snv_deriv1", "snv",  "snv_deriv1"),
  feature_selection = c("pca",    "pca",    "pca", "pca",        "pca",  "pca",        "none", "none"),
  stringsAsFactors  = FALSE
)

## pm-helpers.R globals (mbl_chunked, prep helpers, forward/inverse_y)
PLS_NCOMP   <- 20L
WAPLS_RANGE <- EXP_MBL$pls_c      # c(5, 20)
PIVOT_DIR   <- OUT

snap <- load_snapshot()
source(file.path(this_dir, "pm-helpers.R"))

msg("[batch] arms %s | batches %s | k %s | workers %d | dry %s",
    paste(ARMS, collapse = ","), paste(BATCHES, collapse = ","),
    paste(K_SWEEP, collapse = ","), workers, dry)

## ---------------------------------------------------------------------------
## Pool
## ---------------------------------------------------------------------------

sp <- load_splits(PROPERTY); assert_splits(sp)
pool_ids <- sp$train_core
if (dry) { set.seed(SEED); pool_ids <- sample(pool_ids, 3000L) }
y_of   <- function(ids) snap$lab[[PROPERTY]][match(ids, snap$lab$sample_id)]
n_pool <- length(pool_ids)
msg("[batch] pool: %d oc train_core rows (dry = %s)", n_pool, dry)

t_pool <- system.time(pool_hz <- hz_spectra(snap, pool_ids))
pool_df <- pool_hz$data$analysis
msg("[batch] pool standardized to %d cm-1 in %.0f s: %d x %d", EXPERIMENT_RESAMPLE, t_pool[["elapsed"]],
    nrow(pool_df), length(wn_cols_of(pool_df)))

## ---------------------------------------------------------------------------
## Targets
## ---------------------------------------------------------------------------

## A batch is: hz (standardized spectra-only horizons_data), ids, y (% C),
## group (farm for moys, NA for kssl), and a note for the record.

## The MOYS scans (600 to 7500 at 1.93 cm-1, starting at 599.74) are put on
## the POOL's grid by linear interpolation here, not by standardize():
## standardize() resamples onto a grid anchored at the data's own maximum
## wavenumber (3999.57 for MOYS against 4000 for KSSL), so two sources never
## share an axis through it. Filed 2026-09-17. The scans are interpolated
## individually, averaged per sample, then rebuilt as horizons_data on the
## pool's exact columns; std() is then a no-op.
moys_batch <- function() {
  raw <- spectra(MOYS_OPUS, type = "opus")
  d   <- raw$data$analysis
  sid <- sub("^MOYS_(S[0-9]+-[0-9]+)_.*$", "\\1", d$sample_id)
  stopifnot(all(grepl("^S[0-9]+-[0-9]+$", sid)))
  raw_cols <- grep("^wn_", names(d), value = TRUE)
  raw_wn   <- as.numeric(sub("^wn_", "", raw_cols))
  pool_cols <- wn_cols_of(pool_df)
  pool_wn   <- as.numeric(sub("^wn_", "", pool_cols))
  stopifnot(min(raw_wn) <= min(pool_wn), max(raw_wn) >= max(pool_wn))
  R  <- as.matrix(d[, raw_cols, drop = FALSE])
  X  <- t(apply(R, 1, function(y) stats::approx(raw_wn, y, xout = pool_wn)$y))
  colnames(X) <- pool_cols
  Xm   <- rowsum(X, sid) / as.vector(table(sid)[sort(unique(sid))])
  n_scans <- as.integer(table(sid)[rownames(Xm)])

  lab <- read.csv(MOYS_CSV, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  lab$oc <- lab$Bulk_C_g_kg / 10                          # g/kg -> % C (total C; see README caveat)
  lab <- lab[!is.na(lab$oc), ]
  keep <- rownames(Xm) %in% lab$Sample_ID
  msg("[moys] %d scans -> %d samples; %d with bulk C; %d dropped without C",
      nrow(X), nrow(Xm), sum(keep), sum(!keep))
  Xm <- Xm[keep, , drop = FALSE]; n_scans <- n_scans[keep]

  df <- data.frame(sample_id = rownames(Xm), Xm, check.names = FALSE, stringsAsFactors = FALSE)
  hz <- std(spectra(df, id_col = "sample_id"))           # rebuild as horizons_data on the same grid
  ids <- hz$data$analysis$sample_id
  if (dry) ids <- ids[seq_len(min(40L, length(ids)))]
  hz <- if (dry) std(spectra(df[df$sample_id %in% ids, ], id_col = "sample_id")) else hz
  list(hz = hz, ids = ids,
       y = lab$oc[match(ids, lab$Sample_ID)],
       group = sub("-.*$", "", ids),
       n_scans = n_scans[match(ids, rownames(Xm))],
       note = "MOYS bulk C (total C, g/kg / 10), scans interpolated onto the pool grid, replicates averaged")
}

kssl_batch <- function() {
  site <- data.table::fread(SITE_RAW, showProgress = FALSE,
                            select = c("id.layer_uuid_txt", "longitude.point_wgs84_dd", "latitude.point_wgs84_dd"))
  site <- site[site$id.layer_uuid_txt %in% sp$test &
               is.finite(site$longitude.point_wgs84_dd) & is.finite(site$latitude.point_wgs84_dd), ]
  site <- site[!duplicated(site$id.layer_uuid_txt), ]
  stopifnot(nrow(site) > KSSL_BATCH_N)
  set.seed(SEED)
  seed_id <- sample(site$id.layer_uuid_txt, 1L)
  s <- site[site$id.layer_uuid_txt == seed_id, ]
  ## great-circle distance (km) from the seed sample
  to_rad <- pi / 180
  dlat <- (site$latitude.point_wgs84_dd - s$latitude.point_wgs84_dd) * to_rad
  dlon <- (site$longitude.point_wgs84_dd - s$longitude.point_wgs84_dd) * to_rad
  a <- sin(dlat / 2)^2 + cos(s$latitude.point_wgs84_dd * to_rad) * cos(site$latitude.point_wgs84_dd * to_rad) * sin(dlon / 2)^2
  km <- 2 * 6371 * asin(pmin(1, sqrt(a)))
  o  <- order(km)[seq_len(if (dry) 40L else KSSL_BATCH_N + 1L)]
  ids <- site$id.layer_uuid_txt[o]
  msg("[kssl] control batch: seed %s at (%.3f, %.3f); %d test rows within %.1f km",
      seed_id, s$latitude.point_wgs84_dd, s$longitude.point_wgs84_dd, length(ids), max(km[o]))
  list(hz = hz_spectra(snap, ids), ids = ids, y = y_of(ids), group = rep(NA_character_, length(ids)),
       n_scans = rep(1L, length(ids)),
       note = sprintf("KSSL oc test rows nearest seed %s by lat/long; radius %.1f km", seed_id, max(km[o])))
}

## ---------------------------------------------------------------------------
## Similarity space (the spec's, fixed): SNV -> SG m1 p2 w11 -> water bands
## out -> PCA of the POOL to 99 % (cap 100) -> scores scaled by sdev, so
## Euclidean on the scaled scores is Mahalanobis on the retained components.
## clustering_matrix() (experiment 1 helpers) does the spectral part.
## ---------------------------------------------------------------------------

sim_space <- new.env()
t_sim <- system.time({
  Mp  <- clustering_matrix(pool_df)
  set.seed(SEED)
  pca <- stats::prcomp(Mp, center = TRUE, scale. = FALSE)
  cum <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  sim_space$n_comp <- min(which(cum >= EXP_LOCAL$variance_threshold)[1], EXP_LOCAL$pca_max_comp)
  sim_space$sdev   <- pca$sdev[seq_len(sim_space$n_comp)]
  sim_space$Sp     <- sweep(pca$x[, seq_len(sim_space$n_comp), drop = FALSE], 2, sim_space$sdev, "/")
  rownames(sim_space$Sp) <- pool_df$sample_id
  pca$x <- NULL
  sim_space$pca <- pca
  sim_space$wn  <- attr(Mp, "wn")
  rm(Mp)
})
msg("[batch] similarity space: %d components to 99 %% on %d pool rows (%.0f s)",
    sim_space$n_comp, nrow(sim_space$Sp), t_sim[["elapsed"]])

project_targets <- function(hz) {
  Mt <- clustering_matrix(hz$data$analysis)
  stopifnot(identical(attr(Mt, "wn"), sim_space$wn))
  St <- stats::predict(sim_space$pca, Mt)[, seq_len(sim_space$n_comp), drop = FALSE]
  St <- sweep(St, 2, sim_space$sdev, "/")
  rownames(St) <- hz$data$analysis$sample_id
  St
}

## Target x pool distance matrix and, per target, the ordered pool ids out
## to max(K_SWEEP). 101 x 31,130 doubles is 25 MB; nothing to chunk.
neighbours_of <- function(St) {
  Sp <- sim_space$Sp
  D  <- sqrt(pmax(outer(rowSums(St^2), rowSums(Sp^2), "+") - 2 * St %*% t(Sp), 0))   # pmax(M, 0): dims come from the first arg
  kmax <- max(K_SWEEP)
  nn <- t(apply(D, 1, function(r) order(r)[seq_len(kmax)]))
  list(nn_ids = matrix(rownames(Sp)[nn], nrow = nrow(nn), dimnames = list(rownames(St), NULL)),
       nn_dist = t(apply(D, 1, function(r) sort(r)[seq_len(kmax)])),
       min_dist = apply(D, 1, min))
}

union_pool <- function(nb, rows, k) unique(as.vector(nb$nn_ids[rows, seq_len(k), drop = FALSE]))

## ---------------------------------------------------------------------------
## Target clustering for arm C: k-means on the scaled scores, K by mean
## silhouette over 2..Kmax, clusters under MIN_CLUSTER folded into the
## nearest remaining centroid. K = 1 means C collapses to B and is recorded.
## ---------------------------------------------------------------------------

cluster_targets <- function(St) {
  n <- nrow(St)
  if (n < 2L * MIN_CLUSTER) return(list(assign = rep(1L, n), K = 1L, sil = NA_real_, reason = "too few targets"))
  Kmax <- min(5L, n %/% MIN_CLUSTER)
  dd   <- stats::dist(St)
  best <- NULL
  for (K in 2:Kmax) {
    set.seed(SEED)
    km  <- stats::kmeans(St, centers = K, nstart = 25L, iter.max = 100L)
    sil <- mean(cluster::silhouette(km$cluster, dd)[, 3])
    if (is.null(best) || sil > best$sil) best <- list(assign = km$cluster, K = K, sil = sil)
  }
  a <- best$assign
  repeat {
    sizes <- table(a)
    small <- as.integer(names(sizes)[sizes < MIN_CLUSTER])
    if (!length(small) || length(sizes) == 1L) break
    s1     <- small[1]
    others <- setdiff(as.integer(names(sizes)), s1)
    cent   <- sapply(others, function(o) colMeans(St[a == o, , drop = FALSE]))
    for (i in which(a == s1)) {
      d2 <- colSums((cent - St[i, ])^2)
      a[i] <- others[which.min(d2)]
    }
  }
  a <- as.integer(factor(a))
  list(assign = a, K = length(unique(a)), sil = best$sil,
       reason = if (length(unique(a)) < best$K) sprintf("silhouette chose %d, %d after merging < %d", best$K, length(unique(a)), MIN_CLUSTER) else "silhouette")
}

## ---------------------------------------------------------------------------
## The ordinary chain on a pool, and prediction on targets
## ---------------------------------------------------------------------------

run_chain <- function(ids, tag) {
  t_b <- system.time(hz <- build_hz(snap, ids, PROPERTY, TR, set = GRID))
  t_e <- system.time(hz <- eval_exp(hz, file.path(CKPT, tag), verbose = FALSE))
  win <- winning_config(hz)
  t_f <- system.time(f <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                              allow_par = workers > 1L, seed = SEED))
  msg("[chain %s] %d rows; build %.0f s, evaluate %.0f s (axis %s), fit %.0f s; winner %s",
      tag, length(ids), t_b[["elapsed"]], t_e[["elapsed"]], hz$evaluation$parallelize_over,
      t_f[["elapsed"]], win$config_id)
  list(fit = f, win = win, eval = hz$evaluation$results,
       secs = t_b[["elapsed"]] + t_e[["elapsed"]] + t_f[["elapsed"]])
}

predict_targets <- function(f, batch) {
  p <- as_tibble(predict(f, batch$hz, interval = FALSE))
  stopifnot(all(batch$ids %in% p$sample_id))
  tibble(sample_id = batch$ids, .pred = p$.pred[match(batch$ids, p$sample_id)], truth = batch$y)
}

## ---------------------------------------------------------------------------
## Records
## ---------------------------------------------------------------------------

rows_path  <- file.path(OUT, "coherent-batch.csv")
pools_path <- file.path(OUT, "pool-sizes.csv")

make_row <- function(batch_name, bt, arm, k, preds, n_pool_used, n_clusters = 1L, winner = NA_character_,
                     secs = NA_real_, note = NA_character_) {
  ## NB: the first column is named `batch`, and tibble() exposes earlier
  ## columns to later expressions, so the batch object is `bt` here.
  m <- metrics_row(preds$truth, preds$.pred)
  n_t <- length(bt$ids)
  tibble(batch = batch_name, arm = arm, k = as.integer(k), n_targets = n_t,
         n_scored = m$n_scored, n_pool = as.integer(n_pool_used), pool_frac = n_pool_used / n_pool,
         n_clusters = as.integer(n_clusters), winner = winner,
         rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, rsq = m$rsq,
         sd_truth = stats::sd(preds$truth), resolution_cm = EXPERIMENT_RESAMPLE,
         sim_ncomp = sim_space$n_comp, secs = secs, note = note, pilot = dry,
         ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}

append_csv <- function(row, path) {
  write.table(row, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path))
}

write_preds <- function(batch_name, arm, k, preds, batch, nb, cluster = NA_integer_) {
  p <- preds
  p$batch <- batch_name; p$arm <- arm; p$k <- as.integer(k)
  p$group <- batch$group[match(p$sample_id, batch$ids)]
  p$n_scans <- batch$n_scans[match(p$sample_id, batch$ids)]
  p$min_dist <- nb$min_dist[match(p$sample_id, names(nb$min_dist))]
  p$cluster <- if (length(cluster) == nrow(p)) cluster else rep(cluster, nrow(p))
  write.csv(p, file.path(PREDS, sprintf("preds-%s-%s-k%s.csv", batch_name, arm, k)), row.names = FALSE)
}

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

for (batch_name in BATCHES) {

  batch <- switch(batch_name, moys = moys_batch(), kssl = kssl_batch(), stop("unknown batch: ", batch_name))
  n_targets_batch <- length(batch$ids)
  msg("[%s] %d targets; %% C range %.2f to %.2f, sd %.2f; %s", batch_name, length(batch$ids),
      min(batch$y), max(batch$y), stats::sd(batch$y), batch$note)

  St <- project_targets(batch$hz)
  nb <- neighbours_of(St)
  msg("[%s] nearest-pool distance: median %.2f, max %.2f (scaled-score units)", batch_name,
      median(nb$min_dist), max(nb$min_dist))
  qs2::qs_save(list(St = St, nb = nb, ids = batch$ids, y = batch$y, group = batch$group),
               file.path(CKPT, sprintf("%s-targets.qs2", batch_name)))

  ## ---- G: global -----------------------------------------------------------
  if ("G" %in% ARMS) {
    if (!has_ckpt(PROPERTY, "A_global")) stop("experiment 1's oc A_global checkpoint is missing")
    t_g <- system.time({
      ck <- load_ckpt(PROPERTY, "A_global")
      pG <- as_tibble(predict(ck$fit, batch$hz, interval = TRUE, abstain_ood = FALSE))
      win_g <- ck$winner$config_id
      rm(ck); invisible(gc())
    })
    preds <- tibble(sample_id = batch$ids, .pred = pG$.pred[match(batch$ids, pG$sample_id)], truth = batch$y)
    row <- make_row(batch_name, batch, "G", NA, preds, n_pool, winner = win_g, secs = t_g[["elapsed"]],
                    note = "experiment 1 A_global oc fit, loaded from checkpoint")
    append_csv(row, rows_path); write_preds(batch_name, "G", NA_integer_, preds, batch, nb)
    msg("[%s G] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", batch_name, row$rmse, row$bias, row$ccc, row$rpd)
  }

  ## ---- B and C run the horizons chain on callr workers ---------------------
  if (any(c("B", "C") %in% ARMS)) {
    future::plan(future.callr::callr, workers = workers)
  }

  ## ---- B: one union pool per k --------------------------------------------
  if ("B" %in% ARMS) {
    for (k in K_SWEEP) {
      ids_k <- union_pool(nb, seq_len(nrow(nb$nn_ids)), k)
      append_csv(tibble(batch = batch_name, arm = "B", k = k, cluster = NA_integer_, n_targets = n_targets_batch,
                        n_pool = length(ids_k), pool_frac = length(ids_k) / n_pool), pools_path)
      msg("[%s B k=%d] union pool %d of %d (%.1f %%)", batch_name, k, length(ids_k), n_pool, 100 * length(ids_k) / n_pool)
      ch    <- run_chain(ids_k, sprintf("%s-B-k%d", batch_name, k))
      preds <- predict_targets(ch$fit, batch)
      row   <- make_row(batch_name, batch, "B", k, preds, length(ids_k), winner = ch$win$config_id, secs = ch$secs)
      append_csv(row, rows_path); write_preds(batch_name, "B", k, preds, batch, nb)
      msg("[%s B k=%d] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f (%s)", batch_name, k, row$rmse, row$bias, row$ccc, row$rpd, ch$win$config_id)
      rm(ch); invisible(gc())
    }
  }

  ## ---- C: cluster the targets, one pool and model per cluster -------------
  if ("C" %in% ARMS) {
    cl <- cluster_targets(St)
    msg("[%s C] %d target clusters (%s; silhouette %.3f); sizes %s", batch_name, cl$K, cl$reason,
        cl$sil %||% NA_real_, paste(table(cl$assign), collapse = "/"))
    if (!is.na(batch$group[1])) {
      msg("[%s C] farm x cluster:\n%s", batch_name, paste(capture.output(print(table(batch$group, cl$assign))), collapse = "\n"))
    }
    for (k in K_SWEEP) {
      preds_all <- vector("list", cl$K); pools_k <- integer(cl$K); secs_k <- 0; winners <- character(cl$K)
      for (g in seq_len(cl$K)) {
        rows_g <- which(cl$assign == g)
        ids_g  <- union_pool(nb, rows_g, k)
        pools_k[g] <- length(ids_g)
        append_csv(tibble(batch = batch_name, arm = "C", k = k, cluster = g, n_targets = length(rows_g),
                          n_pool = length(ids_g), pool_frac = length(ids_g) / n_pool), pools_path)
        msg("[%s C k=%d cluster %d] %d targets; union pool %d (%.1f %%)", batch_name, k, g, length(rows_g),
            length(ids_g), 100 * length(ids_g) / n_pool)
        ch <- run_chain(ids_g, sprintf("%s-C-k%d-c%d", batch_name, k, g))
        sub_batch <- list(hz = batch$hz, ids = batch$ids[rows_g], y = batch$y[rows_g])
        p <- predict_targets(ch$fit, sub_batch); p$cluster <- g
        preds_all[[g]] <- p; secs_k <- secs_k + ch$secs; winners[g] <- ch$win$config_id
        rm(ch); invisible(gc())
      }
      preds <- bind_rows(preds_all)
      preds <- preds[match(batch$ids, preds$sample_id), ]
      row <- make_row(batch_name, batch, "C", k, preds, sum(pools_k), n_clusters = cl$K,
                      winner = paste(winners, collapse = ";"), secs = secs_k,
                      note = sprintf("K=%d (%s); pools %s", cl$K, cl$reason, paste(pools_k, collapse = "/")))
      append_csv(row, rows_path); write_preds(batch_name, "C", k, preds, batch, nb, cluster = preds$cluster)
      msg("[%s C k=%d] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", batch_name, k, row$rmse, row$bias, row$ccc, row$rpd)
    }
  }

  ## ---- M: per-sample mbl on the full pool ---------------------------------
  if ("M" %in% ARMS) {
    ## Forked workers, as in the pivot: the library is shared copy-on-write.
    future::plan(future::multicore, workers = min(workers, 4L))
    prep_from_hz <- function(hz, prep) {
      d <- hz$data$analysis; cols <- wn_cols_of(d)
      X <- as.matrix(d[, cols, drop = FALSE]); rownames(X) <- d$sample_id
      X <- prospectr::standardNormalVariate(X)
      if (prep == "snv_deriv1") X <- prospectr::savitzkyGolay(X, m = EXP_LOCAL$sg_m, p = EXP_LOCAL$sg_p, w = EXP_LOCAL$sg_w)
      X[!is.finite(X)] <- 0
      X
    }
    t_m <- system.time({
      Xr <- prep_from_hz(pool_hz, MBL_PREP)[pool_ids, , drop = FALSE]
      Yr <- forward_y(y_of(pool_ids), TR)
      Xu <- prep_from_hz(batch$hz, MBL_PREP)[batch$ids, , drop = FALSE]
      pm <- mbl_chunked(Xr, Yr, Xu, batch$ids, k = MBL_K, chunk_size = 500L)
    })
    preds <- tibble(sample_id = batch$ids, .pred = inverse_y(pm$.pred_t[match(batch$ids, pm$sample_id)], TR), truth = batch$y)
    row <- make_row(batch_name, batch, "M", MBL_K, preds, n_pool, winner = sprintf("mbl_wapls_%s_k%d", MBL_PREP, MBL_K),
                    secs = t_m[["elapsed"]], note = "resemble::mbl, wapls 5-20, diss_pca opc40, full pool")
    append_csv(row, rows_path); write_preds(batch_name, "M", MBL_K, preds, batch, nb)
    msg("[%s M] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f (%.1f min)", batch_name, row$rmse, row$bias, row$ccc, row$rpd, t_m[["elapsed"]] / 60)
    rm(Xr, Xu, pm); invisible(gc())
    future::plan(future::sequential)
  }
}

future::plan(future::sequential)
msg("[batch] done. Rows in %s:", rows_path)
print(as.data.frame(read.csv(rows_path)[, c("batch", "arm", "k", "n_pool", "rmse", "bias", "ccc", "rpd", "winner")]))
