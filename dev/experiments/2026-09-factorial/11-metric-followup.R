## ===========================================================================
## 11 — Follow-up to 10: is "global wins on-library" the learner, the floor,
##      or k?
## ===========================================================================
##
## Purpose (2026-09-21, evening). 10-metric-accuracy.R's Iowa block put the
## global random forest ahead of every selection arm on clay (RMSE 3.02
## against 3.14 to 3.54). Two things in that design favour global by
## construction: a forest on 25,868 rows loses nothing to a subset of the
## same rows (the local-calibration literature's gains are mostly over global
## PLS), and no arm ran with the component floor off or at a larger k, while
## the clay learning curve was still climbing at 17,788 rows. This script
## runs the same fixture and pool with those three levers opened.
##
## Design (block iowa only; clay; transformation none):
##   global   rf and plsr on every clay-measured pool row (2 arms)
##   batch    model {rf, plsr} x sdev_floor {0.10, 0} x metric {cosine,
##            mahalanobis} x k {100, 400}, space_rows = "all" (16 arms)
## Tuning as 10: 5-fold CV, 5-point grid, no Bayesian iterations, seed 307.
##
## Run (from package/, against the INSTALLED horizons; chained after 10 by
## run_followup.sh):
##   nohup bash dev/experiments/2026-09-factorial/run_followup.sh [--workers=N] [--dry] > /dev/null 2>&1 &
##
## Results: results/metric-followup/ — metric-followup.csv, preds/, checkpoints/, logs/.
## ===========================================================================

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")
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
suppressPackageStartupMessages({
  if (!isNamespaceLoaded("horizons")) library(horizons)
  library(dplyr)
  library(tibble)
})
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)
for (p in c("workflows", "parsnip", "recipes", "ranger", "plsmod")) loadNamespace(p)

## ---------------------------------------------------------------------------
## Settings
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- 4
workers      <- if (dry) 2L else as.integer(flag_of("workers", "8"))
MODELS       <- c("rf", "plsr")
FLOORS       <- c(0.10, 0)
METRICS      <- c("cosine", "mahalanobis")
KS           <- if (dry) c(50L) else c(100L, 400L)
MASK         <- rbind(c(1580, 1720), c(3100, 3700))
CLUSTER_MIN  <- 30L
KSSL_BATCH_N <- if (dry) 60L else 300L
AMES         <- c(lat = 42.03, lon = -93.62)
PROPERTY     <- "clay"
TR           <- "none"
TUNING       <- EXP_CONFIG

SITE_RAW <- file.path(RAW_DIR, "ossl_soilsite_L0_v1.2.csv.gz")
OUT   <- file.path(this_dir, "results", "metric-followup")
CKPT  <- file.path(OUT, if (dry) "checkpoints-pilot" else "checkpoints")
PREDS <- file.path(OUT, "preds")
for (d in c(OUT, CKPT, PREDS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
rows_path <- file.path(OUT, "metric-followup.csv")

snap <- load_snapshot()
msg("[followup] models %s | floors %s | metrics %s | k %s | workers %d | dry %s",
    paste(MODELS, collapse = ","), paste(FLOORS, collapse = ","), paste(METRICS, collapse = ","),
    paste(KS, collapse = ","), workers, dry)

## ---------------------------------------------------------------------------
## Fixture and pool (as 10)
## ---------------------------------------------------------------------------

iowa_fixture <- function(property) {
  ok   <- property_rows(snap$lab, property)$ids
  site <- data.table::fread(SITE_RAW, showProgress = FALSE,
                            select = c("id.layer_uuid_txt", "longitude.point_wgs84_dd", "latitude.point_wgs84_dd"))
  site <- site[site$id.layer_uuid_txt %in% ok & site$id.layer_uuid_txt %in% snap$spectra$sample_id &
               is.finite(site$longitude.point_wgs84_dd) & is.finite(site$latitude.point_wgs84_dd), ]
  site <- site[!duplicated(site$id.layer_uuid_txt), ]
  to_rad <- pi / 180
  dlat <- (site$latitude.point_wgs84_dd  - AMES[["lat"]]) * to_rad
  dlon <- (site$longitude.point_wgs84_dd - AMES[["lon"]]) * to_rad
  a  <- sin(dlat / 2)^2 + cos(AMES[["lat"]] * to_rad) * cos(site$latitude.point_wgs84_dd * to_rad) * sin(dlon / 2)^2
  km <- 2 * 6371 * asin(pmin(1, sqrt(a)))
  o  <- order(km)[seq_len(KSSL_BATCH_N)]
  ids <- site$id.layer_uuid_txt[o]
  y   <- snap$lab[[property]][match(ids, snap$lab$sample_id)]
  msg("[iowa] fixture: %d rows nearest Ames within %.0f km; %s %.1f to %.1f, sd %.2f",
      length(ids), max(km[o]), property, min(y), max(y), stats::sd(y))
  list(hz = hz_spectra(snap, ids), ids = ids, y = y)
}

build_pool <- function(property, exclude_ids) {
  ids <- setdiff(snap$spectra$sample_id, exclude_ids)
  if (dry) { set.seed(SEED); ids <- sample(ids, 4000L) }
  hz  <- hz_spectra(snap, ids)
  lab <- snap$lab[match(ids, snap$lab$sample_id), c("sample_id", property)]
  bad <- !is.na(lab[[property]]) & !(lab$sample_id %in% property_rows(snap$lab, property)$ids)
  lab[[property]][bad] <- NA
  hz  <- add_response(hz, source = lab, variable = property)
  msg("[pool] %d rows; %s measured on %d", hz$data$n_rows, property, sum(!is.na(lab[[property]])))
  hz
}

## ---------------------------------------------------------------------------
## Chain, prediction, records (as 10, with the model as a lever)
## ---------------------------------------------------------------------------

run_chain <- function(train, model, tag) {
  t_c <- system.time(hz <- configure(train, outcome = PROPERTY, models = model, transformations = TR,
                                     preprocessing = "snv", feature_selection = "pca",
                                     cv_folds = TUNING$cv_folds, grid_size = TUNING$grid_size,
                                     bayesian_iter = TUNING$bayesian_iter,
                                     final_bayesian_iter = TUNING$final_bayesian_iter))
  t_e <- system.time(hz <- eval_exp(hz, file.path(CKPT, tag), verbose = FALSE))
  t_f <- system.time(f <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                              allow_par = workers > 1L, seed = SEED, verbose = FALSE))
  msg("[chain %s] %d rows; evaluate %.0f s, fit %.0f s", tag, train$data$n_rows, t_e[["elapsed"]], t_f[["elapsed"]])
  list(fit = f, secs = t_c[["elapsed"]] + t_e[["elapsed"]] + t_f[["elapsed"]])
}

predict_fixture <- function(f, fx) {
  p <- as_tibble(predict(f, fx$hz, interval = FALSE))
  stopifnot(all(fx$ids %in% p$sample_id))
  tibble(sample_id = fx$ids, .pred = p$.pred[match(fx$ids, p$sample_id)], truth = fx$y)
}

measured_rows <- function(pool) {
  a <- pool$data$analysis
  horizons:::subset_rows(pool, a$sample_id[!is.na(a[[PROPERTY]])])
}

make_row <- function(fx, arm, scope, model, metric, floor, k, preds, n_pool, ncomp = NA_integer_, secs = NA_real_) {
  m <- metrics_row(preds$truth, preds$.pred)
  tibble(block = "iowa", arm = arm, scope = scope, model = model, metric = metric, sdev_floor = floor, k = k,
         n_targets = length(fx$ids), n_scored = m$n_scored, n_pool = as.integer(n_pool), sim_ncomp = ncomp,
         rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, rsq = m$rsq, sd_truth = stats::sd(preds$truth),
         secs = secs, pilot = dry, ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}

append_csv <- function(row, path) {
  write.table(row, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path))
}

done_arms <- function() {
  if (!file.exists(rows_path)) return(character())
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  r$arm[r$pilot == dry]
}

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

fx   <- iowa_fixture(PROPERTY)
pool <- build_pool(PROPERTY, fx$ids)
done <- done_arms()
future::plan(future.callr::callr, workers = workers)

for (model in MODELS) {
  arm <- paste0("global-", model)
  if (arm %in% done) { msg("[%s] already recorded, skipping", arm); next }
  train <- measured_rows(pool)
  ch    <- run_chain(train, model, arm)
  preds <- predict_fixture(ch$fit, fx)
  row   <- make_row(fx, arm, "global", model, NA, NA, NA, preds, train$data$n_rows, secs = ch$secs)
  append_csv(row, rows_path); write.csv(preds, file.path(PREDS, paste0("preds-", arm, ".csv")), row.names = FALSE)
  msg("[%s] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", arm, row$rmse, row$bias, row$ccc, row$rpd)
  rm(ch, train); invisible(gc())
}

for (k in KS) for (floor in FLOORS) for (metric in METRICS) {
  t_s <- system.time(
    sel <- suppressWarnings(select_training(fx$hz, pool, k = k, scope = "batch", properties = PROPERTY,
                                            mask = MASK, metric = metric, sdev_floor = floor,
                                            cluster_min = CLUSTER_MIN, verbose = FALSE))
  )
  ncomp <- sel$selection$settings$ncomp_retained
  msg("[select k=%d floor=%.2f %s] %d rows, %d components, %.0f s", k, floor, metric, sel$data$n_rows, ncomp, t_s[["elapsed"]])
  for (model in MODELS) {
    arm <- sprintf("batch-%s-floor%.2f-%s-k%d", model, floor, metric, k)
    if (arm %in% done) { msg("[%s] already recorded, skipping", arm); next }
    ch    <- run_chain(sel, model, arm)
    preds <- predict_fixture(ch$fit, fx)
    row   <- make_row(fx, arm, "batch", model, metric, floor, k, preds, sel$data$n_rows, ncomp, ch$secs + t_s[["elapsed"]])
    append_csv(row, rows_path); write.csv(preds, file.path(PREDS, paste0("preds-", arm, ".csv")), row.names = FALSE)
    msg("[%s] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", arm, row$rmse, row$bias, row$ccc, row$rpd)
    rm(ch); invisible(gc())
  }
  rm(sel); invisible(gc())
}

future::plan(future::sequential)
r <- read.csv(rows_path)
print(as.data.frame(r[r$pilot == dry, c("arm", "n_pool", "sim_ncomp", "rmse", "bias", "ccc", "rpd")]), row.names = FALSE, digits = 3)
msg("[followup] done")
