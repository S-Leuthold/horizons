## ===========================================================================
## 14 — Off-library, is selection's gain bias correction in disguise?
## ===========================================================================
##
## Purpose (2026-09-21, overnight). 10-metric-accuracy.R found the two
## regimes that make the library question interesting. On-library (the Iowa
## batch, clay) the global forest beat every selection arm, 3.02 against 3.14
## to 3.54. Off-library (MOYS, oc) selection was the only thing that worked:
## global RMSE 0.494 with bias +0.321, every batch arm 0.35 to 0.43 with bias
## around +0.18. That pattern — a large positive bias on the global model,
## most of it gone under selection — is what this script interrogates.
##
## The hypothesis is deflationary. If selection's off-library gain is mostly
## the removal of a local offset, then a far cheaper thing should recover it:
## keep the global model, and correct its prediction using what the global
## model's own out-of-fold residuals say about the target's neighbourhood.
## No second fit, no per-batch training set, no shipped pool — just the
## global model plus k residuals. If that closes most of the gap, selection
## is buying bias correction at the price of a refit, and the product should
## ship the correction instead. If it does not, selection is doing something
## the residuals cannot express, and that is a real result for the design.
##
## Design. One config everywhere: rf on snv with pca, experiment 1's tuning,
## so the only thing that varies is the correction. Per fixture:
##
##   1. Fit the global model on the pool. fit() keeps the winning config's
##      out-of-fold predictions in models$cv_predictions, keyed by .row,
##      which models$row_index maps back to sample_id. Those OOF predictions
##      are honest in the sense that matters here: no pool row's own value
##      informed its own prediction.
##   2. Residual per pool row: truth - .pred, on the original scale (for a
##      log outcome, cv_predictions carries truth untransformed and .pred
##      already back-transformed, so the difference is in the property's
##      units, and the correction is additive there).
##   3. Build the similarity space on the pool and project the fixture, with
##      OVERNIGHT$mask and OVERNIGHT$sdev_floor — the space as it ships. Each
##      target takes its OVERNIGHT$k nearest pool rows, Euclidean.
##   4. Three corrections on the global prediction:
##        offset    + mean residual over the k neighbours
##        weighted  + inverse-distance-weighted mean residual
##        slope     fit truth ~ .pred on the neighbours' OOF pairs and apply
##                  that linear map to the target's global prediction;
##                  degenerate fits fall back to offset.
##
## Scored against uncorrected global on the fixture's own rows, and printed
## beside the best selection arm from 10 (the lowest-RMSE scope = "batch" row
## of results/metric-accuracy/metric-accuracy.csv, per block). That arm is the
## bar: a correction that reaches it makes selection redundant off-library.
##
## One consequence of fit()'s own bookkeeping is load-bearing and worth
## stating. fit() re-splits its training rows before cross-validating, so the
## OOF predictions cover ~80 % of the pool, not all of it. The similarity
## space is therefore built on, and neighbours drawn from, exactly the rows
## that have a residual. That keeps every neighbour usable and keeps the
## space and the correction on the same population; it does mean the space is
## fit on a random 80 % subsample of the pool rather than the pool.
##
## Parallelism: the global fit runs on a future.callr plan of --workers
## (default 8). Everything after it — the space, the neighbours, the
## corrections — is sequential and cheap.
##
## Run (from package/, against the INSTALLED horizons):
##   nohup Rscript dev/experiments/2026-09-factorial/14-bias-correction.R [--workers=N] [--blocks=iowa,moys] [--dry] > ... 2>&1 &
##
## --dry: pool 4,000 rows, Iowa 60 targets, MOYS 40.
##
## Results: results/bias-correction/ — bias-correction.csv (one row per
## fixture x method), preds/preds-<block>.csv, checkpoints/ (evaluate()
## output dirs and the cached global fits), fixture-iowa.csv.
## ===========================================================================

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading
args    <- commandArgs(trailingOnly = TRUE)
flags   <- args[startsWith(args, "--")]
dry     <- "--dry" %in% flags
flag_of <- function(name, default) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="), flags, value = TRUE))
  if (length(v)) v else default
}
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
for (p in c("workflows", "parsnip", "recipes", "ranger")) loadNamespace(p)

## ---------------------------------------------------------------------------
## Settings — the design numbers come from OVERNIGHT
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- OVERNIGHT$resample          # std() reads this
workers    <- if (dry) 2L else as.integer(flag_of("workers", "8"))
BLOCKS     <- strsplit(flag_of("blocks", "iowa,moys"), ",")[[1]]
K          <- OVERNIGHT$k
MASK       <- OVERNIGHT$mask
SDEV_FLOOR <- OVERNIGHT$sdev_floor
AMES       <- OVERNIGHT$ames
TUNING     <- EXP_CONFIG
CONFIG     <- data.frame(model = "rf", preprocessing = "snv", feature_selection = "pca",
                         stringsAsFactors = FALSE)
METHODS    <- c("global", "offset", "weighted", "slope")

IOWA_N     <- if (dry) 60L else OVERNIGHT$iowa_n
MOYS_N_DRY <- 40L
POOL_N_DRY <- 4000L

MOYS_OPUS <- OVERNIGHT$moys_opus
MOYS_CSV  <- OVERNIGHT$moys_csv
SITE_RAW  <- file.path(RAW_DIR, "ossl_soilsite_L0_v1.2.csv.gz")

OUT   <- file.path(this_dir, "results", "bias-correction")
## Pilot checkpoints live apart from real ones: evaluate() resumes from an
## eval_checkpoint.rds keyed by config_id alone, with no check on the
## training data, so a --dry run under the same tag would silently seed the
## real run's hyperparameters (10-metric-accuracy.R, 2026-09-21).
CKPT  <- file.path(OUT, if (dry) "checkpoints-pilot" else "checkpoints")
PREDS <- file.path(OUT, "preds")
for (d in c(OUT, CKPT, PREDS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
rows_path <- file.path(OUT, "bias-correction.csv")
ACC_CSV   <- file.path(this_dir, "results", "metric-accuracy", "metric-accuracy.csv")

snap <- load_snapshot()
msg("[bias] blocks %s | k %d | sdev_floor %.2f | workers %d | dry %s",
    paste(BLOCKS, collapse = ","), K, SDEV_FLOOR, workers, dry)

## ---------------------------------------------------------------------------
## Fixtures (as 10 built them)
## ---------------------------------------------------------------------------

iowa_fixture <- function(property) {
  ok   <- property_rows(snap$lab, property)$ids
  site <- data.table::fread(SITE_RAW, showProgress = FALSE,
                            select = c("id.layer_uuid_txt", "longitude.point_wgs84_dd", "latitude.point_wgs84_dd"))
  site <- site[site$id.layer_uuid_txt %in% ok &
               site$id.layer_uuid_txt %in% snap$spectra$sample_id &
               is.finite(site$longitude.point_wgs84_dd) & is.finite(site$latitude.point_wgs84_dd), ]
  site <- site[!duplicated(site$id.layer_uuid_txt), ]
  stopifnot(nrow(site) > IOWA_N)
  to_rad <- pi / 180
  dlat <- (site$latitude.point_wgs84_dd  - AMES[["lat"]]) * to_rad
  dlon <- (site$longitude.point_wgs84_dd - AMES[["lon"]]) * to_rad
  a  <- sin(dlat / 2)^2 + cos(AMES[["lat"]] * to_rad) * cos(site$latitude.point_wgs84_dd * to_rad) * sin(dlon / 2)^2
  km <- 2 * 6371 * asin(pmin(1, sqrt(a)))
  o  <- order(km)[seq_len(IOWA_N)]
  ids <- site$id.layer_uuid_txt[o]
  y   <- snap$lab[[property]][match(ids, snap$lab$sample_id)]
  msg("[iowa] fixture: %d rows nearest Ames within %.0f km; %s %.1f to %.1f, sd %.2f",
      length(ids), max(km[o]), property, min(y), max(y), stats::sd(y))
  write.csv(data.frame(sample_id = ids, km_from_ames = km[o],
                       lat = site$latitude.point_wgs84_dd[o], lon = site$longitude.point_wgs84_dd[o], y = y),
            file.path(OUT, "fixture-iowa.csv"), row.names = FALSE)
  list(hz = hz_spectra(snap, ids), ids = ids, y = y, group = rep(NA_character_, length(ids)))
}

## MOYS as 05 and 10 built it: scans interpolated onto the pool grid,
## replicates averaged, rebuilt as horizons_data on the pool's exact columns.
moys_fixture <- function(pool_df) {
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
  Xm  <- rowsum(X, sid) / as.vector(table(sid)[sort(unique(sid))])
  lab <- read.csv(MOYS_CSV, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  lab$oc <- lab$Bulk_C_g_kg / 10
  lab <- lab[!is.na(lab$oc), ]
  keep <- rownames(Xm) %in% lab$Sample_ID
  Xm <- Xm[keep, , drop = FALSE]
  df <- data.frame(sample_id = rownames(Xm), Xm, check.names = FALSE, stringsAsFactors = FALSE)
  if (dry) df <- df[seq_len(min(MOYS_N_DRY, nrow(df))), ]
  hz  <- std(spectra(df, id_col = "sample_id"))
  ids <- hz$data$analysis$sample_id
  msg("[moys] fixture: %d samples with bulk C", length(ids))
  list(hz = hz, ids = ids, y = lab$oc[match(ids, lab$Sample_ID)], group = sub("-.*$", "", ids))
}

## ---------------------------------------------------------------------------
## The pool: the property's train_core (minus the fixture, where they overlap)
## ---------------------------------------------------------------------------

pool_ids_for <- function(property, exclude_ids = character()) {
  ids <- setdiff(load_splits(property)$train_core, exclude_ids)
  if (dry) { set.seed(SEED); ids <- sample(ids, min(POOL_N_DRY, length(ids))) }
  ids
}

## ---------------------------------------------------------------------------
## The global fit, cached — it is the only expensive thing here
## ---------------------------------------------------------------------------

global_fit <- function(pool_hz, block) {
  f_path <- file.path(CKPT, sprintf("global-%s.qs2", block))
  if (file.exists(f_path)) {
    msg("[%s] reusing the cached global fit (%s)", block, basename(f_path))
    return(list(fit = qs2::qs_read(f_path), secs = NA_real_))
  }
  t_e <- system.time(hz <- eval_exp(pool_hz, file.path(CKPT, sprintf("%s-global-eval", block)), verbose = FALSE))
  t_f <- system.time(f  <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                               allow_par = workers > 1L, seed = SEED, verbose = FALSE))
  msg("[%s] global fit on %d rows: evaluate %.0f s, fit %.0f s", block, pool_hz$data$n_rows,
      t_e[["elapsed"]], t_f[["elapsed"]])
  qs2::qs_save(f, f_path)
  list(fit = f, secs = t_e[["elapsed"]] + t_f[["elapsed"]])
}

## Out-of-fold predictions of the winning config, keyed back to sample_id.
## Returns one row per pool row that fit() cross-validated: sample_id, truth,
## .pred, resid. Both truth and .pred are on the property's own scale.
oof_residuals <- function(f) {
  cv <- f$models$cv_predictions
  ri <- f$models$row_index
  if (is.null(cv) || is.null(ri)) {
    stop("fit() returned no cv_predictions/row_index; the residual correction has nothing to stand on")
  }
  keep <- if (!is.null(f$models$best_config)) cv$config_id == f$models$best_config else rep(TRUE, nrow(cv))
  if (length(unique(cv$config_id[keep])) != 1L) {
    stop("expected one config's OOF predictions, found: ", paste(unique(cv$config_id), collapse = ", "))
  }
  cv  <- cv[keep, , drop = FALSE]
  out <- tibble(sample_id = ri$sample_id[match(cv$.row, ri$.row)],
                truth     = cv$truth,
                .pred     = cv$.pred)
  out <- out[!is.na(out$sample_id) & is.finite(out$truth) & is.finite(out$.pred), ]
  if (anyDuplicated(out$sample_id)) {
    stop("duplicate sample_id in the OOF table; v-fold should visit each row once")
  }
  out$resid <- out$truth - out$.pred
  out
}

## ---------------------------------------------------------------------------
## Corrections
## ---------------------------------------------------------------------------

## A linear map from the neighbours' OOF (truth ~ .pred), applied to the
## target's global prediction. Degenerate cases — too few pairs, no spread in
## the neighbours' predictions, a non-finite or implausible slope — fall back
## to the offset correction, which is the same fit with the slope held at 1.
slope_correct <- function(nb_truth, nb_pred, pred, fallback) {
  ok <- is.finite(nb_truth) & is.finite(nb_pred)
  if (sum(ok) < 3L) return(list(value = fallback, fell_back = TRUE))
  x <- nb_pred[ok]; y <- nb_truth[ok]
  if (stats::sd(x) < .Machine$double.eps^0.5) return(list(value = fallback, fell_back = TRUE))
  b <- stats::cov(x, y) / stats::var(x)
  a <- mean(y) - b * mean(x)
  if (!is.finite(a) || !is.finite(b) || b <= 0.1 || b >= 10) {
    return(list(value = fallback, fell_back = TRUE))
  }
  list(value = a + b * pred, fell_back = FALSE)
}

## Every corrected prediction is floored at zero. Clay and organic carbon are
## non-negative, and predict() already floors back-transformed predictions
## (#53); a correction that pushes below zero would otherwise be scored as if
## it were a real value.
floor0 <- function(x) pmax(x, 0)

## ---------------------------------------------------------------------------
## Records
## ---------------------------------------------------------------------------

append_csv <- function(row, path) {
  write.table(row, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path))
}

done_methods <- function(block) {
  if (!file.exists(rows_path)) return(character())
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  r$method[r$block == block & r$pilot == dry]
}

## The bar: the best selection arm from 10, on the same fixture.
best_selection_arm <- function(block) {
  if (!file.exists(ACC_CSV)) return(list(arm = NA_character_, rmse = NA_real_))
  r <- read.csv(ACC_CSV, stringsAsFactors = FALSE)
  r <- r[r$block == block & r$scope == "batch" & !r$pilot & is.finite(r$rmse), ]
  if (!nrow(r)) return(list(arm = NA_character_, rmse = NA_real_))
  i <- which.min(r$rmse)
  list(arm = r$arm[i], rmse = r$rmse[i])
}

make_row <- function(block, property, method, truth, pred, n_pool, n_oof, ncomp,
                     n_fallback, ref, secs) {
  m <- metrics_row(truth, pred)
  tibble(block = block, property = property, method = method, k = K, sdev_floor = SDEV_FLOOR,
         n_targets = length(truth), n_scored = m$n_scored, n_pool = as.integer(n_pool),
         n_oof = as.integer(n_oof), sim_ncomp = as.integer(ncomp),
         rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, rsq = m$rsq,
         sd_truth = stats::sd(truth), n_slope_fallback = as.integer(n_fallback),
         ref_arm = ref$arm, ref_rmse = ref$rmse,
         config = paste(CONFIG, collapse = "_"), secs = secs, pilot = dry,
         ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

for (block in BLOCKS) {

  property <- switch(block, iowa = "clay", moys = "oc", stop("unknown block: ", block))
  tr       <- switch(block, iowa = "none", moys = "log")

  done <- done_methods(block)
  if (all(METHODS %in% done)) { msg("[%s] all methods already recorded, skipping", block); next }

  t0 <- proc.time()[["elapsed"]]

  ## ---- fixture and pool ---------------------------------------------------
  if (block == "iowa") {
    fx      <- iowa_fixture(property)
    ids     <- pool_ids_for(property, fx$ids)
    pool_hz <- build_hz(snap, ids, property, tr, set = CONFIG, tuning = TUNING)
  } else {
    ids     <- pool_ids_for(property)
    pool_hz <- build_hz(snap, ids, property, tr, set = CONFIG, tuning = TUNING)
    fx      <- moys_fixture(pool_hz$data$analysis)
  }
  msg("[%s] pool %d rows on %d predictors; %d targets, %s range %.2f to %.2f, sd %.2f",
      block, pool_hz$data$n_rows, pool_hz$data$n_predictors, length(fx$ids), property,
      min(fx$y), max(fx$y), stats::sd(fx$y))

  ## ---- the global model and its out-of-fold residuals ---------------------
  future::plan(future.callr::callr, workers = workers)
  gf  <- global_fit(pool_hz, block)
  future::plan(future::sequential)

  oof <- oof_residuals(gf$fit)
  msg("[%s] OOF residuals on %d of %d pool rows (%.0f %%); mean %+.3f, sd %.3f",
      block, nrow(oof), pool_hz$data$n_rows, 100 * nrow(oof) / pool_hz$data$n_rows,
      mean(oof$resid), stats::sd(oof$resid))

  ## ---- similarity space on the OOF rows, fixture projected ----------------
  pm <- horizons:::predictor_matrix(pool_hz)
  tm <- horizons:::predictor_matrix(fx$hz)
  stopifnot(isTRUE(all.equal(pm$wavenumbers, tm$wavenumbers)))
  keep <- rownames(pm$matrix) %in% oof$sample_id
  stopifnot(sum(keep) == nrow(oof), K <= sum(keep))

  t_sp <- system.time({
    sp <- horizons:::build_similarity_space(pm$matrix[keep, , drop = FALSE], pm$wavenumbers,
                                            mask = MASK, sdev_floor = SDEV_FLOOR)
    St <- horizons:::project_similarity(sp, tm$matrix, tm$wavenumbers)
    nn <- horizons:::nearest_neighbours(St, sp$scores, k = K, metric = "euclidean", sdev = sp$sdev)
  })
  msg("[%s] similarity space: %d components, %d pool rows, %.0f s; neighbour distance %.3f to %.3f",
      block, sp$ncomp, nrow(sp$scores), t_sp[["elapsed"]],
      min(nn$dist, na.rm = TRUE), max(nn$dist, na.rm = TRUE))

  ## ---- the uncorrected global prediction on the fixture -------------------
  p <- as_tibble(predict(gf$fit, fx$hz, interval = FALSE))
  stopifnot(all(fx$ids %in% p$sample_id))
  pred_global <- p$.pred[match(fx$ids, p$sample_id)]
  truth       <- fx$y

  ## ---- the three corrections ----------------------------------------------
  res_by_id <- setNames(oof$resid, oof$sample_id)
  n_fallback <- 0L
  pred_offset <- pred_weighted <- pred_slope <- rep(NA_real_, length(fx$ids))
  mean_dist   <- rep(NA_real_, length(fx$ids))

  for (i in seq_along(fx$ids)) {

    nb_ids <- nn$ids[match(fx$ids[i], rownames(nn$ids)), ]
    nb_d   <- nn$dist[match(fx$ids[i], rownames(nn$dist)), ]
    ok     <- !is.na(nb_ids) & is.finite(nb_d)
    nb_ids <- nb_ids[ok]; nb_d <- nb_d[ok]
    if (!length(nb_ids)) next                       # a target with no defined distance

    r <- unname(res_by_id[nb_ids])
    mean_dist[i] <- mean(nb_d)

    ## offset: the neighbourhood's mean residual
    pred_offset[i] <- pred_global[i] + mean(r)

    ## weighted: the same, with inverse-distance weights. The floor keeps a
    ## target sitting on top of a pool row from taking the whole weight.
    w <- 1 / pmax(nb_d, .Machine$double.eps^0.5)
    pred_weighted[i] <- pred_global[i] + stats::weighted.mean(r, w)

    ## slope: the neighbours' own truth-on-prediction map
    nb <- oof[match(nb_ids, oof$sample_id), ]
    sl <- slope_correct(nb$truth, nb$.pred, pred_global[i], pred_offset[i])
    pred_slope[i] <- sl$value
    n_fallback <- n_fallback + as.integer(sl$fell_back)

  }

  pred_offset   <- floor0(pred_offset)
  pred_weighted <- floor0(pred_weighted)
  pred_slope    <- floor0(pred_slope)

  ## ---- score, record ------------------------------------------------------
  ref   <- best_selection_arm(block)
  secs  <- proc.time()[["elapsed"]] - t0
  preds <- list(global = pred_global, offset = pred_offset,
                weighted = pred_weighted, slope = pred_slope)

  for (method in METHODS) {
    if (method %in% done) { msg("[%s %s] already recorded, skipping", block, method); next }
    row <- make_row(block, property, method, truth, preds[[method]],
                    pool_hz$data$n_rows, nrow(oof), sp$ncomp,
                    if (method == "slope") n_fallback else 0L, ref, secs)
    append_csv(row, rows_path)
    msg("[%s %-8s] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", block, method,
        row$rmse, row$bias, row$ccc, row$rpd)
  }

  if (is.na(ref$rmse)) {
    msg("[%s] no non-pilot batch arm in %s to compare against", block, basename(ACC_CSV))
  } else {
    msg("[%s] the bar from 10: %s at RMSE %.3f", block, ref$arm, ref$rmse)
  }

  write.csv(tibble(sample_id = fx$ids, group = fx$group, truth = truth,
                   pred_global = pred_global, pred_offset = pred_offset,
                   pred_weighted = pred_weighted, pred_slope = pred_slope,
                   mean_neighbour_dist = mean_dist),
            file.path(PREDS, sprintf("preds-%s.csv", block)), row.names = FALSE)

  rm(pool_hz, fx, gf, oof, sp, St, nn, pm, tm); invisible(gc())
}

if (file.exists(rows_path)) {
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  print(as.data.frame(r[r$pilot == dry, c("block", "method", "n_pool", "n_oof", "sim_ncomp",
                                          "rmse", "bias", "ccc", "rpd", "ref_arm", "ref_rmse")]),
        row.names = FALSE, digits = 3)
}
msg("[bias] done")
