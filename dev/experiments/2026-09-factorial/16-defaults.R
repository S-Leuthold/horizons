## ===========================================================================
## 16 — One default each for the correction, the metric and k
## ===========================================================================
##
## Purpose (2026-09-29). See 16-defaults.md. The routing design in
## 16-threshold.md was dropped: a public package cannot condition on the
## instrument, and the distance did not separate the batches we have. This
## asks the plainer question. Do the on-library wins from 15 (the residual
## correction, the cosine draw, k = 100) survive on US batches scanned on
## another instrument, and do they cost anything on KSSL batches?
##
## Design. 15's chain and corrections, unchanged, over nine batches:
##
##   off-instrument  MOYS, AONR, FFAR (A layer); CSU's INVENIO-R; bulk C
##   KSSL            six regional batches of mineral surface layers, the
##                   targets' whole KSSL projects removed from the pool
##
## Per batch: metric {cosine, euclidean} x draw k {100, 400}, each scored
## uncorrected and with the offset, weighted and slope corrections, plus one
## Mahalanobis draw whose distances are recorded and not used.
##
## Property: KSSL total_carbon against bulk C (g/kg / 10), log-transformed.
##
## Run (from package/, against the INSTALLED horizons):
##   nohup bash dev/experiments/2026-09-factorial/run_defaults.sh [--workers=N] [--batches=a,b] [--dry] > /dev/null 2>&1 &
##
## --dry: pool 4,000 rows, 40 targets, batches iowa and moys, k = 100 only.
##
## Results: results/defaults/ — defaults.csv (one row per batch x metric x
## draw k x method), signal.csv (one row per batch), batches.csv (target ids
## and groups), preds/, checkpoints/.
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
## Settings
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- OVERNIGHT$resample          # std() reads this
workers    <- if (dry) 2L else as.integer(flag_of("workers", "8"))
DRAW_KS    <- if (dry) 100L else c(100L, 400L)
METRICS    <- c("cosine", "euclidean")
K          <- OVERNIGHT$k                          # correction neighbourhood, as in 14 and 15
MASK       <- OVERNIGHT$mask
SDEV_FLOOR <- OVERNIGHT$sdev_floor
TUNING     <- EXP_CONFIG
CONFIG     <- data.frame(model = "rf", preprocessing = "snv", feature_selection = "pca",
                         stringsAsFactors = FALSE)
METHODS    <- c("pool", "offset", "weighted", "slope")

PROPERTY   <- "total_carbon"
TR         <- "log"
EXP_PROPERTIES <- rbind(EXP_PROPERTIES,
                        data.frame(property = PROPERTY, transformation = TR, lower = 0, upper = 100,
                                   stringsAsFactors = FALSE))

KSSL_N      <- if (dry) 40L else 150L              # target layers per KSSL batch
MAX_C       <- 8                                   # mineral: total C at most 8 %
MAX_DEPTH   <- 10                                  # surface: upper depth at most 10 cm
AONR_PER_FIELD <- 4L
TARGET_N_DRY   <- 40L
POOL_N_DRY     <- 4000L

CENTRES <- list(
  iowa        = c(lat = 42.03, lon = -93.62),
  w_kansas    = c(lat = 38.9,  lon = -100.9),
  ms_delta    = c(lat = 33.4,  lon = -90.9),
  ga_piedmont = c(lat = 34.0,  lon = -83.4),
  palouse     = c(lat = 46.7,  lon = -117.0),
  c_calif     = c(lat = 36.7,  lon = -119.8)
)
EXTERNAL <- c("moys", "aonr", "ffar")
BATCHES  <- strsplit(flag_of("batches", if (dry) "iowa,moys" else paste(c(names(CENTRES), EXTERNAL), collapse = ",")), ",")[[1]]
stopifnot(all(BATCHES %in% c(names(CENTRES), EXTERNAL)))

AI_LEAF   <- "/data/workshop/projects/ai-leaf/data"
SITE_RAW  <- file.path(RAW_DIR, "ossl_soilsite_L0_v1.2.csv.gz")

OUT   <- file.path(this_dir, "results", "defaults")
CKPT  <- file.path(OUT, if (dry) "checkpoints-pilot" else "checkpoints")
PREDS <- file.path(OUT, "preds")
for (d in c(OUT, CKPT, PREDS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
rows_path   <- file.path(OUT, "defaults.csv")
signal_path <- file.path(OUT, "signal.csv")
batch_path  <- file.path(OUT, "batches.csv")

snap <- load_snapshot()
msg("[defaults] batches %s | draw k %s | metrics %s | property %s (%s) | workers %d | dry %s",
    paste(BATCHES, collapse = ","), paste(DRAW_KS, collapse = ","), paste(METRICS, collapse = ","),
    PROPERTY, TR, workers, dry)

## ---------------------------------------------------------------------------
## Site metadata for the KSSL batches: site, project, location, depth
## ---------------------------------------------------------------------------

meta <- local({
  s <- data.table::fread(SITE_RAW, showProgress = FALSE,
                         select = c("id.layer_uuid_txt", "id.dataset.site_ascii_txt", "id.project_ascii_txt",
                                    "latitude.point_wgs84_dd", "longitude.point_wgs84_dd",
                                    "latitude.county_wgs84_dd", "longitude.county_wgs84_dd"))
  s <- as.data.frame(s[s$id.layer_uuid_txt %in% snap$spectra$sample_id, ])
  s <- s[!duplicated(s$id.layer_uuid_txt), ]
  depth <- as.data.frame(arrow::read_parquet(snapshot_path("site")))
  point <- is.finite(s$latitude.point_wgs84_dd) & is.finite(s$longitude.point_wgs84_dd)
  data.frame(
    sample_id = s$id.layer_uuid_txt,
    site      = as.character(s$id.dataset.site_ascii_txt),
    project   = s$id.project_ascii_txt,
    lat       = ifelse(point, s$latitude.point_wgs84_dd,  s$latitude.county_wgs84_dd),
    lon       = ifelse(point, s$longitude.point_wgs84_dd, s$longitude.county_wgs84_dd),
    located   = ifelse(point, "point", "county"),
    upper_cm  = depth$upper_depth_cm[match(s$id.layer_uuid_txt, depth$sample_id)],
    y         = snap$lab[[PROPERTY]][match(s$id.layer_uuid_txt, snap$lab$sample_id)],
    stringsAsFactors = FALSE
  )
})

haversine_km <- function(lat, lon, centre) {
  to_rad <- pi / 180
  dlat <- (lat - centre[["lat"]]) * to_rad
  dlon <- (lon - centre[["lon"]]) * to_rad
  a <- sin(dlat / 2)^2 + cos(centre[["lat"]] * to_rad) * cos(lat * to_rad) * sin(dlon / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}

## ---------------------------------------------------------------------------
## Batches. Each returns the fixture and the pool rows it removes.
## ---------------------------------------------------------------------------

kssl_batch <- function(name) {
  centre <- CENTRES[[name]]
  ok <- is.finite(meta$lat) & is.finite(meta$y) & meta$y > 0 & meta$y <= MAX_C &
        is.finite(meta$upper_cm) & meta$upper_cm <= MAX_DEPTH &
        meta$sample_id %in% property_rows(snap$lab, PROPERTY)$ids
  el <- meta[ok, ]
  el$km <- haversine_km(el$lat, el$lon, centre)

  ### Whole sites, nearest first, until the batch reaches KSSL_N layers.
  site_km <- tapply(el$km, el$site, min)
  site_n  <- table(el$site)[names(site_km)]
  ord     <- order(site_km, names(site_km))
  take    <- names(site_km)[ord][seq_len(which(cumsum(site_n[ord]) >= KSSL_N)[1])]
  tg      <- el[el$site %in% take, ]
  tg      <- tg[order(tg$km, tg$sample_id), ]

  exclude <- meta$sample_id[meta$project %in% unique(tg$project)]
  msg("[%s] %d layers from %d sites within %.0f km (%d point, %d county); %s %.2f to %.2f %%; pool loses %d rows of %d projects",
      name, nrow(tg), length(take), max(tg$km), sum(tg$located == "point"), sum(tg$located == "county"),
      PROPERTY, min(tg$y), max(tg$y), length(exclude), length(unique(tg$project)))

  list(hz = hz_spectra(snap, tg$sample_id), ids = tg$sample_id, y = tg$y, group = tg$site,
       exclude = exclude, family = "kssl",
       info = tibble(radius_km = max(tg$km), n_sites = length(take),
                     n_projects = length(unique(tg$project)), n_excluded = length(exclude)))
}

## OPUS directory -> replicate-averaged spectra on the pool's grid, as 15's
## moys_fixture() did. `id_of` maps a file's sample_id to the lab's id.
opus_fixture <- function(name, opus_dir, id_of, lab, pool_df) {
  raw <- spectra(opus_dir, type = "opus")
  d   <- raw$data$analysis
  sid <- id_of(d$sample_id)
  keep_scan <- !is.na(sid) & sid %in% lab$id
  d <- d[keep_scan, , drop = FALSE]; sid <- sid[keep_scan]
  raw_cols <- grep("^wn_", names(d), value = TRUE)
  raw_wn   <- as.numeric(sub("^wn_", "", raw_cols))
  pool_cols <- wn_cols_of(pool_df)
  pool_wn   <- as.numeric(sub("^wn_", "", pool_cols))
  stopifnot(min(raw_wn) <= min(pool_wn), max(raw_wn) >= max(pool_wn))
  R  <- as.matrix(d[, raw_cols, drop = FALSE])
  X  <- t(apply(R, 1, function(y) stats::approx(raw_wn, y, xout = pool_wn)$y))
  colnames(X) <- pool_cols
  ids <- sort(unique(sid))
  Xm  <- rowsum(X, sid)[ids, , drop = FALSE] / as.vector(table(sid)[ids])
  df  <- data.frame(sample_id = ids, Xm, check.names = FALSE, stringsAsFactors = FALSE)
  if (dry) df <- df[seq_len(min(TARGET_N_DRY, nrow(df))), ]
  hz  <- std(spectra(df, id_col = "sample_id"))
  ids <- hz$data$analysis$sample_id
  y   <- lab$y[match(ids, lab$id)]
  msg("[%s] %d samples from %d scans; bulk C %.2f to %.2f %%, sd %.2f", name, length(ids), nrow(d),
      min(y), max(y), stats::sd(y))
  list(hz = hz, ids = ids, y = y, group = lab$group[match(ids, lab$id)], exclude = character(),
       family = "external", info = tibble(radius_km = NA_real_, n_sites = length(unique(lab$group[lab$id %in% ids])),
                                          n_projects = NA_integer_, n_excluded = 0L))
}

external_batch <- function(name, pool_df) {
  if (name == "moys") {
    lab <- read.csv(OVERNIGHT$moys_csv, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    lab <- data.frame(id = lab$Sample_ID, y = lab$Bulk_C_g_kg / 10, stringsAsFactors = FALSE)
    lab <- lab[is.finite(lab$y), ]
    lab$group <- sub("-.*$", "", lab$id)                                   # farm
    return(opus_fixture(name, OVERNIGHT$moys_opus,
                        function(x) ifelse(grepl("^MOYS_S[0-9]+-[0-9]+_", x),
                                           sub("^MOYS_(S[0-9]+-[0-9]+)_.*$", "\\1", x), NA_character_),
                        lab, pool_df))
  }
  if (name == "aonr") {
    lab <- read.csv(file.path(AI_LEAF, "raw", "AONR.csv"), fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    co  <- read.csv(file.path(AI_LEAF, "raw", "aonr_coords.csv"), fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    lab <- lab[is.finite(lab$Bulk_C_g_kg), ]
    lab <- merge(lab, co, by = "Sample_ID")
    lab <- lab[is.finite(lab$Latitude) & is.finite(lab$Longitude) & !duplicated(lab$Sample_ID), ]
    lab$group <- paste(round(lab$Latitude, 3), round(lab$Longitude, 3))    # field
    ### At most AONR_PER_FIELD samples per field, seeded, so no field dominates.
    set.seed(SEED)
    lab <- do.call(rbind, lapply(split(lab, lab$group), function(g) g[sample.int(nrow(g), min(nrow(g), AONR_PER_FIELD)), ]))
    lab <- data.frame(id = lab$Sample_ID, y = lab$Bulk_C_g_kg / 10, group = lab$group, stringsAsFactors = FALSE)
    return(opus_fixture(name, file.path(AI_LEAF, "processed", "AONR", "opus_files"),
                        function(x) ifelse(grepl("^AONR-[WMF]_[A-Za-z0-9]+(-[0-9]+)?_GroundBulk_", x),
                                           sub("^AONR-[WMF]_([A-Za-z0-9]+(-[0-9]+)?)_GroundBulk_.*$", "\\1", x),
                                           NA_character_),
                        lab, pool_df))
  }
  if (name == "ffar") {
    lab <- read.csv(file.path(AI_LEAF, "processed", "FFAR", "fraction_data.csv"), fileEncoding = "UTF-8-BOM",
                    stringsAsFactors = FALSE)
    lab <- lab[grepl("-A$", lab$Sample_ID) & is.finite(lab$Bulk_C_g_kg), ]
    ### One duplicated id in the CSV; average it rather than pick a row.
    lab <- stats::aggregate(Bulk_C_g_kg ~ Sample_ID, data = lab, FUN = mean)
    lab <- data.frame(id = lab$Sample_ID, y = lab$Bulk_C_g_kg / 10, group = lab$Sample_ID, stringsAsFactors = FALSE)
    return(opus_fixture(name, file.path(AI_LEAF, "processed", "FFAR", "opus_files"),
                        function(x) ifelse(grepl("^FFAR_S[0-9]+-[A-D]_GroundBulk_", x),
                                           sub("^FFAR_S0*([0-9]+-[A-D])_GroundBulk_.*$", "S\\1", x), NA_character_),
                        lab, pool_df))
  }
  stop("unknown external batch: ", name)
}

## ---------------------------------------------------------------------------
## Pool, chain, OOF residuals, corrections (15's, with the pool's exclusions)
## ---------------------------------------------------------------------------

build_pool <- function(exclude_ids) {
  ids <- setdiff(snap$spectra$sample_id, exclude_ids)
  if (dry) { set.seed(SEED); ids <- sample(ids, POOL_N_DRY) }
  hz  <- hz_spectra(snap, ids)
  lab <- snap$lab[match(ids, snap$lab$sample_id), c("sample_id", PROPERTY)]
  bad <- !is.na(lab[[PROPERTY]]) & !(lab$sample_id %in% property_rows(snap$lab, PROPERTY)$ids)
  lab[[PROPERTY]][bad] <- NA
  hz  <- add_response(hz, source = lab, variable = PROPERTY)
  msg("[pool] %d rows on %d predictors; %s measured on %d (%d out of physical range set NA)",
      hz$data$n_rows, hz$data$n_predictors, PROPERTY, sum(!is.na(lab[[PROPERTY]])), sum(bad))
  hz
}

pool_fit <- function(train, tag) {
  f_path <- file.path(CKPT, sprintf("fit-%s.qs2", tag))
  if (file.exists(f_path)) {
    msg("[%s] reusing the cached pool fit (%s)", tag, basename(f_path))
    return(list(fit = qs2::qs_read(f_path), secs = NA_real_))
  }
  t_c <- system.time(hz <- configure(train, outcome = PROPERTY,
                                     models = CONFIG$model, transformations = TR,
                                     preprocessing = CONFIG$preprocessing,
                                     feature_selection = CONFIG$feature_selection,
                                     cv_folds = TUNING$cv_folds, grid_size = TUNING$grid_size,
                                     bayesian_iter = TUNING$bayesian_iter,
                                     final_bayesian_iter = TUNING$final_bayesian_iter))
  t_e <- system.time(hz <- eval_exp(hz, file.path(CKPT, tag), verbose = FALSE))
  t_f <- system.time(f  <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                               allow_par = workers > 1L, seed = SEED, verbose = FALSE))
  msg("[%s] pool fit on %d rows: configure %.0f s, evaluate %.0f s, fit %.0f s", tag,
      train$data$n_rows, t_c[["elapsed"]], t_e[["elapsed"]], t_f[["elapsed"]])
  qs2::qs_save(f, f_path)
  list(fit = f, secs = t_c[["elapsed"]] + t_e[["elapsed"]] + t_f[["elapsed"]])
}

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

floor0 <- function(x) pmax(x, 0)

## ---------------------------------------------------------------------------
## Records
## ---------------------------------------------------------------------------

append_csv <- function(row, path) {
  write.table(row, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path))
}

done_methods <- function(batch, metric, draw_k) {
  if (!file.exists(rows_path)) return(character())
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  r$method[r$batch == batch & r$metric == metric & r$draw_k == draw_k & r$pilot == dry]
}

signal_done <- function(batch) {
  if (!file.exists(signal_path)) return(FALSE)
  r <- read.csv(signal_path, stringsAsFactors = FALSE)
  any(r$batch == batch & r$pilot == dry)
}

make_row <- function(batch, family, metric, draw_k, method, truth, pred, n_pool_total, n_pool, n_oof,
                     ncomp, n_fallback, secs) {
  m <- metrics_row(truth, pred)
  tibble(batch = batch, family = family, property = PROPERTY, metric = metric, draw_k = as.integer(draw_k),
         method = method, k = K, n_targets = length(truth), n_scored = m$n_scored,
         n_pool_total = as.integer(n_pool_total), n_pool = as.integer(n_pool), n_oof = as.integer(n_oof),
         sim_ncomp = as.integer(ncomp),
         rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, rsq = m$rsq,
         rmse_log = sqrt(mean((log1p(floor0(pred)) - log1p(truth))^2, na.rm = TRUE)),
         sd_truth = stats::sd(truth), n_slope_fallback = as.integer(n_fallback),
         config = paste(CONFIG, collapse = "_"), secs = secs, pilot = dry,
         ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

for (batch in BATCHES) {

  family <- if (batch %in% EXTERNAL) "external" else "kssl"

  ## ---- fixture and pool ---------------------------------------------------
  if (family == "kssl") {
    fx   <- kssl_batch(batch)
    pool <- build_pool(c(fx$exclude, fx$ids))
  } else {
    pool <- build_pool(character())
    fx   <- external_batch(batch, pool$data$analysis)
  }
  n_pool_total <- pool$data$n_rows
  if (!file.exists(batch_path) || !any(read.csv(batch_path)$batch == batch & read.csv(batch_path)$pilot == dry)) {
    append_csv(tibble(batch = batch, family = family, sample_id = fx$ids, group = fx$group, truth = fx$y, pilot = dry),
               batch_path)
  }

  ## ---- the distance, recorded and not used --------------------------------
  if (!signal_done(batch)) {
    sel_m <- suppressWarnings(select_training(fx$hz, pool, k = 100L, scope = "batch", properties = PROPERTY,
                                              mask = MASK, metric = "mahalanobis", space_rows = "all",
                                              verbose = FALSE))
    td  <- sel_m$selection$target_distances
    thr <- sel_m$selection$resemblance$threshold
    append_csv(cbind(tibble(batch = batch, family = family, n_targets = length(fx$ids),
                            sd_truth = stats::sd(fx$y), median_truth = stats::median(fx$y),
                            ncomp = sel_m$selection$settings$ncomp_retained,
                            median_nearest = stats::median(td$nearest), resemblance_threshold = thr,
                            d_ratio = stats::median(td$nearest) / thr,
                            n_beyond = nrow(sel_m$selection$resemblance$beyond), pilot = dry),
                     fx$info), signal_path)
    msg("[%s] distance: median nearest %.3f, resemblance threshold %.3f, ratio %.2f, %d of %d beyond",
        batch, stats::median(td$nearest), thr, stats::median(td$nearest) / thr,
        nrow(sel_m$selection$resemblance$beyond), length(fx$ids))
    rm(sel_m); invisible(gc())
  }

  for (metric in METRICS) for (draw_k in DRAW_KS) {

    tag  <- sprintf("%s-%s-k%d", batch, metric, draw_k)
    done <- done_methods(batch, metric, draw_k)
    if (all(METHODS %in% done)) { msg("[%s] all methods already recorded, skipping", tag); next }

    t0 <- proc.time()[["elapsed"]]

    ## ---- the draw ---------------------------------------------------------
    t_s <- system.time(
      sel <- suppressWarnings(select_training(fx$hz, pool, k = draw_k, scope = "batch", properties = PROPERTY,
                                              mask = MASK, metric = metric, space_rows = "all",
                                              verbose = FALSE))
    )
    msg("[%s] drew %d of %d pool rows in %.0f s; ncomp %d", tag, sel$data$n_rows, n_pool_total,
        t_s[["elapsed"]], sel$selection$settings$ncomp_retained)

    ## ---- the pool fit and its out-of-fold residuals -----------------------
    future::plan(future.callr::callr, workers = workers)
    pf <- pool_fit(sel, tag)
    future::plan(future::sequential)

    oof <- oof_residuals(pf$fit)
    msg("[%s] OOF residuals on %d of %d pool rows (%.0f %%); mean %+.3f, sd %.3f",
        tag, nrow(oof), sel$data$n_rows, 100 * nrow(oof) / sel$data$n_rows,
        mean(oof$resid), stats::sd(oof$resid))

    ## ---- similarity space on the OOF rows, fixture projected --------------
    pm <- horizons:::predictor_matrix(sel)
    tm <- horizons:::predictor_matrix(fx$hz)
    stopifnot(isTRUE(all.equal(pm$wavenumbers, tm$wavenumbers)))
    keep <- rownames(pm$matrix) %in% oof$sample_id
    stopifnot(sum(keep) == nrow(oof), K <= sum(keep))

    sp <- horizons:::build_similarity_space(pm$matrix[keep, , drop = FALSE], pm$wavenumbers,
                                            mask = MASK, sdev_floor = SDEV_FLOOR)
    St <- horizons:::project_similarity(sp, tm$matrix, tm$wavenumbers)
    nn <- horizons:::nearest_neighbours(St, sp$scores, k = K, metric = "euclidean", sdev = sp$sdev)

    ## ---- the uncorrected pool prediction on the fixture -------------------
    p <- as_tibble(predict(pf$fit, fx$hz, interval = FALSE))
    stopifnot(all(fx$ids %in% p$sample_id))
    pred_pool <- p$.pred[match(fx$ids, p$sample_id)]
    truth     <- fx$y

    ## ---- the three corrections --------------------------------------------
    res_by_id <- setNames(oof$resid, oof$sample_id)
    n_fallback <- 0L
    pred_offset <- pred_weighted <- pred_slope <- rep(NA_real_, length(fx$ids))
    mean_dist   <- mean_offset <- rep(NA_real_, length(fx$ids))

    for (i in seq_along(fx$ids)) {
      nb_ids <- nn$ids[match(fx$ids[i], rownames(nn$ids)), ]
      nb_d   <- nn$dist[match(fx$ids[i], rownames(nn$dist)), ]
      ok     <- !is.na(nb_ids) & is.finite(nb_d)
      nb_ids <- nb_ids[ok]; nb_d <- nb_d[ok]
      if (!length(nb_ids)) next
      r <- unname(res_by_id[nb_ids])
      mean_dist[i]   <- mean(nb_d)
      mean_offset[i] <- mean(r)
      pred_offset[i] <- pred_pool[i] + mean(r)
      w <- 1 / pmax(nb_d, .Machine$double.eps^0.5)
      pred_weighted[i] <- pred_pool[i] + stats::weighted.mean(r, w)
      nb <- oof[match(nb_ids, oof$sample_id), ]
      sl <- slope_correct(nb$truth, nb$.pred, pred_pool[i], pred_offset[i])
      pred_slope[i] <- sl$value
      n_fallback <- n_fallback + as.integer(sl$fell_back)
    }

    pred_offset   <- floor0(pred_offset)
    pred_weighted <- floor0(pred_weighted)
    pred_slope    <- floor0(pred_slope)

    ## ---- score, record ----------------------------------------------------
    secs  <- proc.time()[["elapsed"]] - t0
    preds <- list(pool = pred_pool, offset = pred_offset, weighted = pred_weighted, slope = pred_slope)

    for (method in METHODS) {
      if (method %in% done) next
      row <- make_row(batch, family, metric, draw_k, method, truth, preds[[method]],
                      n_pool_total, sel$data$n_rows, nrow(oof), sp$ncomp,
                      if (method == "slope") n_fallback else 0L, secs)
      append_csv(row, rows_path)
      msg("[%s %-8s] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", tag, method,
          row$rmse, row$bias, row$ccc, row$rpd)
    }

    write.csv(tibble(sample_id = fx$ids, group = fx$group, truth = truth,
                     pred_pool = pred_pool, pred_offset = pred_offset,
                     pred_weighted = pred_weighted, pred_slope = pred_slope,
                     mean_offset = mean_offset, mean_neighbour_dist = mean_dist),
              file.path(PREDS, sprintf("preds-%s%s.csv", tag, if (dry) "-pilot" else "")), row.names = FALSE)

    rm(sel, pf, oof, sp, St, nn, pm, tm); invisible(gc())
  }

  rm(pool, fx); invisible(gc())
}

## ---------------------------------------------------------------------------
## The read: three log RMSE ratios per batch (negative = the first setting wins)
## ---------------------------------------------------------------------------

if (file.exists(rows_path)) {
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  r <- r[r$pilot == dry, ]
  cell <- function(b, m, k, meth) {
    v <- r$rmse[r$batch == b & r$metric == m & r$draw_k == k & r$method == meth]
    if (length(v)) v[length(v)] else NA_real_
  }
  read <- do.call(rbind, lapply(unique(r$batch), function(b) {
    data.frame(batch = b, family = r$family[r$batch == b][1],
               euc_k100 = cell(b, "euclidean", 100, "pool"), cos_k100 = cell(b, "cosine", 100, "pool"),
               correction_cos = log(cell(b, "cosine", 100, "offset") / cell(b, "cosine", 100, "pool")),
               correction_euc = log(cell(b, "euclidean", 100, "offset") / cell(b, "euclidean", 100, "pool")),
               metric_cos_vs_euc = log(cell(b, "cosine", 100, "pool") / cell(b, "euclidean", 100, "pool")),
               k400_vs_k100_euc = log(cell(b, "euclidean", 400, "pool") / cell(b, "euclidean", 100, "pool")),
               k400_vs_k100_cos = log(cell(b, "cosine", 400, "pool") / cell(b, "cosine", 100, "pool")))
  }))
  print(read[order(read$family, read$batch), ], row.names = FALSE, digits = 3)
  write.csv(read, file.path(OUT, sprintf("read%s.csv", if (dry) "-pilot" else "")), row.names = FALSE)
}
msg("[defaults] done")
