## ===========================================================================
## 10 — Does the similarity metric (and the reference population) change the
##      accuracy of the model built on the selected training set?
## ===========================================================================
##
## Purpose (2026-09-21). 09-space-rows.R showed that which rows define the
## similarity space barely changes neighbour sets on Euclidean distance
## (Jaccard@100 0.96-1.00) but changes them a lot on Mahalanobis (0.63 on
## carbonate, 0.71 on clay, 0.94 even on the near-complete oc), because the
## per-component sd rescaling is where all the disagreement lives. That is a
## question about the metric default, and overlap cannot answer it. This
## script asks the accuracy question: with everything else fixed, does the
## metric (and, under it, space_rows) move RMSE on a real batch?
##
## Prior from 9/17: arm B on MOYS gave RMSE 0.343 / 0.343 / 0.345 at k =
## 100 / 200 / 400, so the batch model is insensitive to training-set
## composition at that scale. A null here is a live outcome; it would mean
## choosing the metric on robustness, which points at Euclidean.
##
## Design. One config everywhere: rf on snv with pca, experiment 1's tuning
## (5-point grid, no Bayesian iterations, 5 folds, seed 307), so arms differ
## only by the training set. Every draw goes through select_training() at
## k = 100 with the 9/17 defaults (1st derivative, water mask, PCA to 99 %).
##
##   Block "iowa" (property clay, transformation none):
##     fixture  the KSSL_BATCH_N snapshot rows nearest Ames, Iowa
##              (42.03 N, 93.62 W) by great-circle distance among rows with
##              clay measured and coordinates; pool = snapshot minus fixture.
##              Clay is 57 % measured, so space_rows = "measured" is a real
##              alternative here. oc would not do: 99.6 % measured, the two
##              spaces are the same rows.
##     arms     global (rf on every clay-measured pool row), then
##              scope {batch, cluster} x metric {euclidean, cosine,
##              mahalanobis} x space_rows {all, measured}: 13 runs.
##
##   Block "moys" (property oc, transformation log, as 9/17):
##     fixture  the 99 MOYS samples, interpolated onto the pool grid as 05
##              did; the off-library case where selection showed its only
##              gain on 9/17. oc is near-complete, so space_rows = "all" only.
##     arms     global, then scope {batch, cluster} x metric x {all}: 7 runs.
##
## Cluster scope only says something if the batch splits. --check builds the
## fixtures, runs the verb at scope = "cluster" per metric, prints the
## cluster counts and sizes, and stops without fitting anything.
##
## Parallelism: arms run one after another; inside each, evaluate() and
## fit() dispatch onto a future.callr plan of --workers (default 8), and the
## memory watchdog from experiment 1 kills the run below 15 GB MemAvailable.
##
## Run (from package/, against the INSTALLED horizons built from
## feat/select-training):
##   nohup bash dev/experiments/2026-09-factorial/run_metric.sh [--workers=N] [--blocks=iowa,moys] [--check] [--dry] > /dev/null 2>&1 &
##
## Results: results/metric-accuracy/ — metric-accuracy.csv (one row per
## block x arm), fixture-iowa.csv, preds/*.csv, checkpoints/ (evaluate()
## output dirs), logs/.
## ===========================================================================

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading
args      <- commandArgs(trailingOnly = TRUE)
flags     <- args[startsWith(args, "--")]
dry       <- "--dry" %in% flags
check     <- "--check" %in% flags
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
  library(dplyr)
  library(tibble)
})
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)
if (!exists("select_training", asNamespace("horizons"))) {
  stop("The installed horizons has no select_training(); install feat/select-training first.")
}
for (p in c("workflows", "parsnip", "recipes", "ranger")) loadNamespace(p)

## ---------------------------------------------------------------------------
## Settings
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- 4
workers      <- if (dry) 2L else as.integer(flag_of("workers", "8"))
BLOCKS       <- strsplit(flag_of("blocks", "iowa,moys"), ",")[[1]]
K            <- 100L
MASK         <- rbind(c(1580, 1720), c(3100, 3700))   # EXP_LOCAL$water_bands, as 05 applied them
METRICS      <- c("euclidean", "cosine", "mahalanobis")
SCOPES       <- c("batch", "cluster")
CLUSTER_MIN  <- 30L
KSSL_BATCH_N <- if (dry) 60L else 300L
AMES         <- c(lat = 42.03, lon = -93.62)

## The one config, with experiment 1's tuning (05 used the same).
CONFIG <- data.frame(model = "rf", preprocessing = "snv", feature_selection = "pca", stringsAsFactors = FALSE)
TUNING <- EXP_CONFIG

MOYS_OPUS <- "/data/workshop/projects/ai-leaf/data/processed/MOYS/opus_files"
MOYS_CSV  <- "/data/workshop/projects/ai-leaf/data/raw/MOYS.csv"
SITE_RAW  <- file.path(RAW_DIR, "ossl_soilsite_L0_v1.2.csv.gz")

OUT   <- file.path(this_dir, "results", "metric-accuracy")
## Pilot checkpoints live apart from real ones: evaluate() resumes from an
## eval_checkpoint.rds keyed by config_id alone, with no check on the
## training data, so a --dry run under the same tag silently seeds the real
## run's hyperparameters (found 2026-09-21, 14:12; the first real run was
## discarded for it).
CKPT  <- file.path(OUT, if (dry) "checkpoints-pilot" else "checkpoints")
PREDS <- file.path(OUT, "preds")
for (d in c(OUT, CKPT, PREDS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
rows_path <- file.path(OUT, "metric-accuracy.csv")

snap <- load_snapshot()
msg("[metric] blocks %s | workers %d | k %d | fixture n %d | dry %s | check %s",
    paste(BLOCKS, collapse = ","), workers, K, KSSL_BATCH_N, dry, check)

## ---------------------------------------------------------------------------
## Blocks: property, transformation, fixture
## ---------------------------------------------------------------------------

## A fixture is: hz (standardized spectra-only horizons_data), ids, y (truth
## in the property's units), group (farm for moys, NA otherwise), note.

iowa_fixture <- function(property) {
  ok   <- property_rows(snap$lab, property)$ids
  site <- data.table::fread(SITE_RAW, showProgress = FALSE,
                            select = c("id.layer_uuid_txt", "longitude.point_wgs84_dd", "latitude.point_wgs84_dd"))
  site <- site[site$id.layer_uuid_txt %in% ok &
               site$id.layer_uuid_txt %in% snap$spectra$sample_id &
               is.finite(site$longitude.point_wgs84_dd) & is.finite(site$latitude.point_wgs84_dd), ]
  site <- site[!duplicated(site$id.layer_uuid_txt), ]
  stopifnot(nrow(site) > KSSL_BATCH_N)
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
  write.csv(data.frame(sample_id = ids, km_from_ames = km[o],
                       lat = site$latitude.point_wgs84_dd[o], lon = site$longitude.point_wgs84_dd[o], y = y),
            file.path(OUT, "fixture-iowa.csv"), row.names = FALSE)
  list(hz = hz_spectra(snap, ids), ids = ids, y = y, group = rep(NA_character_, length(ids)),
       note = sprintf("KSSL rows nearest Ames IA with %s measured; radius %.0f km", property, max(km[o])))
}

## MOYS as 05 built it: scans interpolated onto the pool grid, replicates
## averaged, rebuilt as horizons_data on the pool's exact columns.
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
  if (dry) df <- df[seq_len(min(40L, nrow(df))), ]
  hz  <- std(spectra(df, id_col = "sample_id"))
  ids <- hz$data$analysis$sample_id
  msg("[moys] fixture: %d samples with bulk C", length(ids))
  list(hz = hz, ids = ids, y = lab$oc[match(ids, lab$Sample_ID)], group = sub("-.*$", "", ids),
       note = "MOYS bulk C (total C, g/kg / 10), scans interpolated onto the pool grid, replicates averaged")
}

## ---------------------------------------------------------------------------
## Pool for a block: every snapshot row except the fixture, one property
## attached. The verb restricts each draw to rows that have it.
## ---------------------------------------------------------------------------

build_pool <- function(property, exclude_ids) {
  ids <- setdiff(snap$spectra$sample_id, exclude_ids)
  if (dry) { set.seed(SEED); ids <- sample(ids, 4000L) }
  hz  <- hz_spectra(snap, ids)
  lab <- snap$lab[match(ids, snap$lab$sample_id), c("sample_id", property)]
  ## property_rows() drops values outside the physical range; those rows
  ## stay in the pool as NA so the space still sees their spectra.
  bad <- !is.na(lab[[property]]) & !(lab$sample_id %in% property_rows(snap$lab, property)$ids)
  lab[[property]][bad] <- NA
  hz  <- add_response(hz, source = lab, variable = property)
  msg("[pool] %d rows on %d predictors; %s measured on %d (%d out of physical range set NA)",
      hz$data$n_rows, hz$data$n_predictors, property, sum(!is.na(lab[[property]])), sum(bad))
  hz
}

## ---------------------------------------------------------------------------
## The chain on a training set, and prediction on the fixture
## ---------------------------------------------------------------------------

## `train` is a horizons_data with the property attached (a select_training()
## return, a subset_rows() of one, or the pool itself for the global arm).
run_chain <- function(train, property, transformation, tag) {
  t_c <- system.time(hz <- configure(train, outcome = property,
                                     models = CONFIG$model, transformations = transformation,
                                     preprocessing = CONFIG$preprocessing,
                                     feature_selection = CONFIG$feature_selection,
                                     cv_folds = TUNING$cv_folds, grid_size = TUNING$grid_size,
                                     bayesian_iter = TUNING$bayesian_iter,
                                     final_bayesian_iter = TUNING$final_bayesian_iter))
  t_e <- system.time(hz <- eval_exp(hz, file.path(CKPT, tag), verbose = FALSE))
  t_f <- system.time(f <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                              allow_par = workers > 1L, seed = SEED, verbose = FALSE))
  msg("[chain %s] %d rows; configure %.0f s, evaluate %.0f s (axis %s), fit %.0f s",
      tag, train$data$n_rows, t_c[["elapsed"]], t_e[["elapsed"]], hz$evaluation$parallelize_over, t_f[["elapsed"]])
  list(fit = f, secs = t_c[["elapsed"]] + t_e[["elapsed"]] + t_f[["elapsed"]])
}

predict_fixture <- function(f, fx, ids = fx$ids) {
  p <- as_tibble(predict(f, fx$hz, interval = FALSE))
  stopifnot(all(ids %in% p$sample_id))
  tibble(sample_id = ids, .pred = p$.pred[match(ids, p$sample_id)], truth = fx$y[match(ids, fx$ids)])
}

## Drop the rows the property is not measured on: the global arm trains on
## every measured pool row, the same population every draw is restricted to.
measured_rows <- function(pool, property) {
  a <- pool$data$analysis
  horizons:::subset_rows(pool, a$sample_id[!is.na(a[[property]])])
}

## ---------------------------------------------------------------------------
## Records
## ---------------------------------------------------------------------------

make_row <- function(block, fx, arm, scope, metric, space_rows, preds, n_pool, n_pool_total,
                     n_clusters = 1L, ncomp = NA_integer_, secs = NA_real_, note = NA_character_) {
  m <- metrics_row(preds$truth, preds$.pred)
  tibble(block = block, arm = arm, scope = scope, metric = metric, space_rows = space_rows, k = K,
         n_targets = length(fx$ids), n_scored = m$n_scored, n_pool = as.integer(n_pool),
         pool_frac = n_pool / n_pool_total, n_clusters = as.integer(n_clusters), sim_ncomp = ncomp,
         rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, rsq = m$rsq, sd_truth = stats::sd(preds$truth),
         config = paste(CONFIG, collapse = "_"), secs = secs, note = note, pilot = dry,
         ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}

append_csv <- function(row, path) {
  write.table(row, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path))
}

write_preds <- function(block, arm, preds, fx, cluster = NA_integer_) {
  p <- preds; p$block <- block; p$arm <- arm
  p$group   <- fx$group[match(p$sample_id, fx$ids)]
  p$cluster <- if (length(cluster) == nrow(p)) cluster else rep(cluster, nrow(p))
  write.csv(p, file.path(PREDS, sprintf("preds-%s-%s.csv", block, arm)), row.names = FALSE)
}

arm_name <- function(scope, metric, space_rows) paste(scope, metric, space_rows, sep = "-")
done_arms <- function(block) {
  if (!file.exists(rows_path)) return(character())
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  r$arm[r$block == block & r$pilot == dry]
}

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

for (block in BLOCKS) {

  property <- switch(block, iowa = "clay", moys = "oc", stop("unknown block: ", block))
  tr       <- switch(block, iowa = "none", moys = "log")
  rows_opt <- switch(block, iowa = c("all", "measured"), moys = "all")

  if (block == "iowa") {
    fx   <- iowa_fixture(property)
    pool <- build_pool(property, fx$ids)
  } else {
    pool <- build_pool(property, character())
    fx   <- moys_fixture(pool$data$analysis)
  }
  n_pool_total <- pool$data$n_rows
  msg("[%s] %d targets; %s range %.2f to %.2f, sd %.2f; %s", block, length(fx$ids), property,
      min(fx$y), max(fx$y), stats::sd(fx$y), fx$note)

  ## ---- --check: does the fixture split under cluster scope? ---------------
  if (check) {
    for (metric in METRICS) for (rows in rows_opt) {
      sel <- suppressWarnings(select_training(fx$hz, pool, k = K, scope = "cluster", properties = property,
                                              mask = MASK, metric = metric, space_rows = rows,
                                              cluster_min = CLUSTER_MIN, verbose = FALSE))
      g  <- sel$selection$groups
      cl <- sel$selection$clustering
      msg("[%s check] %-12s %-9s clusters %d (%s); targets %s; pool rows %s; union %d; ncomp %d",
          block, metric, rows, nrow(g), cl$reason %||% "silhouette",
          paste(g$n_targets, collapse = "/"), paste(g$n_rows, collapse = "/"),
          sel$data$n_rows, sel$selection$settings$ncomp_retained)
    }
    next
  }

  future::plan(future.callr::callr, workers = workers)
  done <- done_arms(block)

  ## ---- global -------------------------------------------------------------
  if (!"global" %in% done) {
    train <- measured_rows(pool, property)
    ch    <- run_chain(train, property, tr, sprintf("%s-global", block))
    preds <- predict_fixture(ch$fit, fx)
    row   <- make_row(block, fx, "global", "global", NA, NA, preds, train$data$n_rows, n_pool_total, secs = ch$secs,
                      note = "rf on every measured pool row")
    append_csv(row, rows_path); write_preds(block, "global", preds, fx)
    msg("[%s global] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", block, row$rmse, row$bias, row$ccc, row$rpd)
    rm(ch, train); invisible(gc())
  }

  ## ---- scope x metric x space_rows ----------------------------------------
  for (scope in SCOPES) for (metric in METRICS) for (rows in rows_opt) {
    arm <- arm_name(scope, metric, rows)
    if (arm %in% done) { msg("[%s %s] already recorded, skipping", block, arm); next }

    t_s <- system.time(
      sel <- suppressWarnings(select_training(fx$hz, pool, k = K, scope = scope, properties = property,
                                              mask = MASK, metric = metric, space_rows = rows,
                                              cluster_min = CLUSTER_MIN, verbose = FALSE))
    )
    g     <- sel$selection$groups
    ncomp <- sel$selection$settings$ncomp_retained
    msg("[%s %s] drew %d rows in %.0f s; %d group(s) %s; ncomp %d", block, arm, sel$data$n_rows,
        t_s[["elapsed"]], nrow(g), paste(g$n_rows, collapse = "/"), ncomp)

    preds_all <- vector("list", nrow(g)); secs <- t_s[["elapsed"]]
    for (i in seq_len(nrow(g))) {
      train <- if (nrow(g) == 1L) sel else horizons:::subset_rows(sel, g$pool_ids[[i]])
      ch    <- run_chain(train, property, tr, sprintf("%s-%s-g%d", block, arm, i))
      p     <- predict_fixture(ch$fit, fx, ids = g$target_ids[[i]]); p$cluster <- i
      preds_all[[i]] <- p; secs <- secs + ch$secs
      rm(ch, train); invisible(gc())
    }
    preds <- bind_rows(preds_all)
    preds <- preds[match(fx$ids, preds$sample_id), ]
    note  <- if (scope == "cluster") sprintf("clusters %d (%s); pools %s", nrow(g),
                                             sel$selection$clustering$reason %||% "silhouette",
                                             paste(g$n_rows, collapse = "/")) else NA_character_
    row <- make_row(block, fx, arm, scope, metric, rows, preds, sel$data$n_rows, n_pool_total,
                    n_clusters = nrow(g), ncomp = ncomp, secs = secs, note = note)
    append_csv(row, rows_path); write_preds(block, arm, preds, fx, cluster = preds$cluster)
    msg("[%s %s] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", block, arm, row$rmse, row$bias, row$ccc, row$rpd)
    rm(sel, preds_all); invisible(gc())
  }

  future::plan(future::sequential)
  rm(pool, fx); invisible(gc())
}

if (!check && file.exists(rows_path)) {
  r <- read.csv(rows_path)
  print(as.data.frame(r[r$pilot == dry, c("block", "arm", "n_pool", "n_clusters", "sim_ncomp", "rmse", "bias", "ccc", "rpd")]),
        row.names = FALSE, digits = 3)
}
msg("[metric] done")
