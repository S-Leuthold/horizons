## ===========================================================================
## 07 — Spike, leave one farm out: do the user's own references fix the pool?
## ===========================================================================
##
## Purpose (2026-09-17, evening). The coherent-batch run (05) showed that on
## MOYS, an off-library batch, the union pool with one model beats global by
## about 10 % RMSE and removes a +0.10 bias, and that per-sample mbl loses at
## every k. The heterogeneity check the same evening showed mbl's gain on the
## full test set is a tail effect (organic soils, heavy clays); on mineral
## soils the global model is already about as good as it gets. So for the
## typical user, a mineral-soil batch on a different instrument, the open
## question is whether their OWN references fix what remains: the transfer.
##
## Design. Ten MOYS farms. For each farm f: targets = farm f's samples; the
## pool = the k = 100 union pool 05 drew for the whole MOYS batch (1,891
## KSSL rows, the same pool for every fold so only the spike varies); arm
## "spiked" adds the other nine farms' samples with their bulk C to the pool
## before the grid; arm "unspiked" is the same pool without them. Same
## eight-config grid, evaluate() picks, fit() refits, predict farm f. Scored
## per farm and pooled over the 99 targets, against 05's global and pool rows
## on the same targets.
##
## Run (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_spike.sh [--workers=N] [--k=100] > /dev/null 2>&1 &
## Results: results/coherent-batch/spike-lofo.csv, preds/preds-moys-spike-*.csv
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
suppressPackageStartupMessages({ library(horizons); library(dplyr); library(tibble) })
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)

EXPERIMENT_RESAMPLE <- 4
PROPERTY <- "oc"; TR <- "log"
workers  <- as.integer(flag_of("workers", "8"))
K_POOL   <- as.integer(flag_of("k", "100"))
ARMS     <- c("unspiked", "spiked")

OUT   <- file.path(this_dir, "results", "coherent-batch")
CKPT  <- file.path(OUT, "checkpoints"); PREDS <- file.path(OUT, "preds")
rows_path <- file.path(OUT, "spike-lofo.csv")

GRID <- data.frame(
  model             = c("cubist", "cubist", "rf",  "rf",         "plsr", "plsr",       "plsr", "plsr"),
  preprocessing     = c("snv",    "snv_deriv1", "snv", "snv_deriv1", "snv",  "snv_deriv1", "snv",  "snv_deriv1"),
  feature_selection = c("pca",    "pca",    "pca", "pca",        "pca",  "pca",        "none", "none"),
  stringsAsFactors  = FALSE
)

snap <- load_snapshot()
sp <- load_splits(PROPERTY); assert_splits(sp)
pool_ids <- sp$train_core
y_of <- function(ids) snap$lab[[PROPERTY]][match(ids, snap$lab$sample_id)]
pool_hz <- hz_spectra(snap, pool_ids)
pool_df <- pool_hz$data$analysis

## MOYS targets and lab, rebuilt as 05 did -----------------------------------
t <- qs2::qs_read(file.path(CKPT, "moys-targets.qs2")); moys_ids <- t$ids; moys_y <- t$y; farm <- t$group
MOYS_OPUS <- "/data/workshop/projects/ai-leaf/data/processed/MOYS/opus_files"
raw <- spectra(MOYS_OPUS, type = "opus"); d <- raw$data$analysis
sid <- sub("^MOYS_(S[0-9]+-[0-9]+)_.*$", "\\1", d$sample_id)
raw_cols <- grep("^wn_", names(d), value = TRUE); raw_wn <- as.numeric(sub("^wn_", "", raw_cols))
pool_cols <- wn_cols_of(pool_df); pool_wn <- as.numeric(sub("^wn_", "", pool_cols))
R <- as.matrix(d[, raw_cols, drop = FALSE])
X <- t(apply(R, 1, function(y) stats::approx(raw_wn, y, xout = pool_wn)$y)); colnames(X) <- pool_cols
Xm <- rowsum(X, sid) / as.vector(table(sid)[sort(unique(sid))])
moys_df <- data.frame(sample_id = rownames(Xm), Xm, check.names = FALSE, stringsAsFactors = FALSE)
moys_df <- moys_df[match(moys_ids, moys_df$sample_id), ]
rm(raw, d, R, X, Xm); invisible(gc())

## The fixed KSSL pool: 05's k = K_POOL union over all MOYS targets ----------
union_ids <- unique(as.vector(t$nb$nn_ids[, seq_len(K_POOL), drop = FALSE]))
msg("[spike] MOYS %d targets over %d farms; fixed pool %d KSSL rows (k = %d)", length(moys_ids), length(unique(farm)), length(union_ids), K_POOL)

## build_hz on a data frame of standardized spectra + a lab table ------------
build_hz_df <- function(df, lab) {
  hz <- std(spectra(df, id_col = "sample_id"))
  stopifnot(all(hz$data$analysis$sample_id %in% lab$sample_id), !anyNA(lab[[PROPERTY]]))
  hz <- add_response(hz, source = lab, variable = PROPERTY)
  hz <- configure(hz, outcome = PROPERTY, models = unique(GRID$model), transformations = TR,
                  preprocessing = unique(GRID$preprocessing), feature_selection = unique(GRID$feature_selection),
                  cv_folds = EXP_CONFIG$cv_folds, grid_size = EXP_CONFIG$grid_size,
                  bayesian_iter = 0L, final_bayesian_iter = 0L)
  hz <- select_configs(hz, GRID)
  validate(hz)
}

pool_rows <- pool_df[match(union_ids, pool_df$sample_id), c("sample_id", pool_cols), drop = FALSE]
pool_lab  <- data.frame(sample_id = union_ids, oc = y_of(union_ids), stringsAsFactors = FALSE)
names(pool_lab)[2] <- PROPERTY
moys_lab  <- data.frame(sample_id = moys_ids, oc = moys_y, stringsAsFactors = FALSE)
names(moys_lab)[2] <- PROPERTY

future::plan(future.callr::callr, workers = workers)
on.exit(future::plan(future::sequential), add = TRUE)

append_csv <- function(row, path) write.table(row, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path))

all_preds <- list()
for (f in sort(unique(farm))) {
  rows_f <- which(farm == f); ids_f <- moys_ids[rows_f]; y_f <- moys_y[rows_f]
  others <- setdiff(seq_along(moys_ids), rows_f)
  target_hz <- std(spectra(moys_df[rows_f, , drop = FALSE], id_col = "sample_id"))
  for (arm in ARMS) {
    df  <- if (arm == "spiked") rbind(pool_rows, moys_df[others, c("sample_id", pool_cols)]) else pool_rows
    lab <- if (arm == "spiked") rbind(pool_lab, moys_lab[others, ]) else pool_lab
    t_c <- system.time({
      hz  <- build_hz_df(df, lab)
      hz  <- eval_exp(hz, file.path(CKPT, sprintf("moys-spike-%s-%s", f, arm)), verbose = FALSE)
      win <- winning_config(hz)
      fit_obj <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, allow_par = workers > 1L, seed = SEED)
      p <- as_tibble(predict(fit_obj, target_hz, interval = FALSE))
    })
    preds <- tibble(sample_id = ids_f, farm = f, arm = arm, .pred = p$.pred[match(ids_f, p$sample_id)], truth = y_f)
    all_preds[[length(all_preds) + 1L]] <- preds
    m <- metrics_row(preds$truth, preds$.pred)
    row <- tibble(farm = f, arm = arm, k = K_POOL, n_targets = length(ids_f), n_train = nrow(df), n_spike = nrow(df) - nrow(pool_rows),
                  winner = win$config_id, rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd,
                  secs = t_c[["elapsed"]], ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
    append_csv(row, rows_path)
    msg("[spike %s %s] %d targets, %d train (%d spike): RMSE %.3f bias %+.3f (%s, %.0f s)", f, arm, length(ids_f), nrow(df),
        nrow(df) - nrow(pool_rows), m$rmse, m$bias, win$config_id, t_c[["elapsed"]])
    rm(hz, fit_obj); invisible(gc())
  }
}
future::plan(future::sequential)

all_preds <- bind_rows(all_preds)
write.csv(all_preds, file.path(PREDS, "preds-moys-spike-lofo.csv"), row.names = FALSE)
for (arm in ARMS) {
  p <- all_preds[all_preds$arm == arm, ]; m <- metrics_row(p$truth, p$.pred)
  row <- tibble(farm = "ALL", arm = arm, k = K_POOL, n_targets = nrow(p), n_train = NA_integer_, n_spike = NA_integer_, winner = NA_character_,
                rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, secs = NA_real_, ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  append_csv(row, rows_path)
  msg("[spike ALL %s] pooled over %d targets: RMSE %.3f bias %+.3f CCC %.3f", arm, nrow(p), m$rmse, m$bias, m$ccc)
}
msg("[spike] done; 05's rows on the same targets: global 0.354 / +0.104, pool k=100 0.343 / -0.004, cluster k=100 0.321 / -0.003")
