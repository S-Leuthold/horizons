## ===========================================================================
## 15 — Does the residual correction survive when the fit is the batch pool?
## ===========================================================================
##
## Purpose (2026-09-22). 14 found that on-library (Iowa, clay) a global
## forest corrected by the mean out-of-fold residual over each target's 100
## nearest pool rows beat both the uncorrected global (3.131 -> 2.827) and the
## best selection arm from 10 (3.140). Off-library (MOYS, oc) the same
## correction made the global worse (0.526 -> 0.565) and selection stood
## (0.348). But 14's residuals came from a global fit on the whole train core.
## Under the design as settled on 2026-09-22 — the library ships whole and
## select_training() decides what enters the model per batch — there is no
## global fit at predict time. The question is whether the correction built
## from the DRAWN POOL's own out-of-fold residuals gives the same gain. If it
## does, the product path is draw, fit, correct (and spike when references
## exist). If it does not, the on-library gain needs a global fit the product
## does not have, and that changes what build_library() has to carry.
##
## Design. Same one config as 10 and 14 (rf, snv, pca, experiment 1's
## tuning). Pool as 10 built it: every snapshot row except the fixture, the
## property attached (NA outside physical range). Per fixture and draw size:
##
##   1. select_training(fixture, pool, k = DRAW_K, scope = "batch",
##      metric = METRIC) — the union of each target's k nearest pool rows, the
##      draw the product would make. Euclidean, the metric recommended from
##      the 9/21 results; the default sdev_floor as it ships.
##   2. configure -> evaluate -> fit on that pool, as 10's run_chain(). The
##      uncorrected prediction on the fixture is the "pool" method and should
##      reproduce 10's batch-<metric>-all arm at k = 100 up to tuning seed.
##   3. fit()'s out-of-fold predictions on the pool rows it cross-validated
##      (~80 %, as in 14), residual = truth - .pred on the property's scale.
##   4. Similarity space on those OOF rows, fixture projected, each target's
##      OVERNIGHT$k nearest by Euclidean — 14's step 3 verbatim, with the pool
##      swapped for the drawn pool.
##   5. 14's three corrections on the pool prediction: offset, weighted, slope.
##
## Scored on the fixture's own rows, and printed beside two bars: 14's
## global + offset row (the correction the product cannot have) and 10's best
## batch arm (selection alone). The reading: pool + correction at or under
## 14's global + offset on Iowa means the on-library gain is the product's;
## at or under 10's selection bar on MOYS means correction does not hurt the
## off-library case either.
##
## Parallelism: each chain runs on a future.callr plan of --workers; the
## space, neighbours and corrections are sequential and cheap.
##
## Run (from package/, against the INSTALLED horizons):
##   nohup Rscript dev/experiments/2026-09-factorial/15-pool-correction.R [--workers=N] [--blocks=iowa,moys] [--k=100,400] [--metric=euclidean] [--dry] > ... 2>&1 &
##
## --dry: pool 4,000 rows, Iowa 60 targets, MOYS 40, k = 100 only.
##
## Results: results/pool-correction/ — pool-correction.csv (one row per
## fixture x draw k x method), preds/preds-<block>-k<k>.csv, checkpoints/
## (evaluate() output dirs and the cached pool fits), fixture-iowa.csv.
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
if (!exists("select_training", asNamespace("horizons"))) {
  stop("The installed horizons has no select_training(); install feat/select-training first.")
}

## ---------------------------------------------------------------------------
## Settings — the design numbers come from OVERNIGHT
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- OVERNIGHT$resample          # std() reads this
workers    <- if (dry) 2L else as.integer(flag_of("workers", "8"))
BLOCKS     <- strsplit(flag_of("blocks", "iowa,moys"), ",")[[1]]
DRAW_KS    <- if (dry) 100L else as.integer(strsplit(flag_of("k", "100,400"), ",")[[1]])
METRIC     <- flag_of("metric", "euclidean")
K          <- OVERNIGHT$k                          # correction neighbourhood, as in 14
MASK       <- OVERNIGHT$mask
SDEV_FLOOR <- OVERNIGHT$sdev_floor
AMES       <- OVERNIGHT$ames
TUNING     <- EXP_CONFIG
CONFIG     <- data.frame(model = "rf", preprocessing = "snv", feature_selection = "pca",
                         stringsAsFactors = FALSE)
METHODS    <- c("pool", "offset", "weighted", "slope")

IOWA_N     <- if (dry) 60L else OVERNIGHT$iowa_n
MOYS_N_DRY <- 40L
POOL_N_DRY <- 4000L

MOYS_OPUS <- OVERNIGHT$moys_opus
MOYS_CSV  <- OVERNIGHT$moys_csv
SITE_RAW  <- file.path(RAW_DIR, "ossl_soilsite_L0_v1.2.csv.gz")

OUT   <- file.path(this_dir, "results", "pool-correction")
## Pilot checkpoints live apart from real ones (see 10 and 14 for why).
CKPT  <- file.path(OUT, if (dry) "checkpoints-pilot" else "checkpoints")
PREDS <- file.path(OUT, "preds")
for (d in c(OUT, CKPT, PREDS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
rows_path <- file.path(OUT, "pool-correction.csv")
ACC_CSV   <- file.path(this_dir, "results", "metric-accuracy", "metric-accuracy.csv")
BIAS_CSV  <- file.path(this_dir, "results", "bias-correction", "bias-correction.csv")

snap <- load_snapshot()
msg("[pool-corr] blocks %s | draw k %s | metric %s | correction k %d | sdev_floor %.2f | workers %d | dry %s",
    paste(BLOCKS, collapse = ","), paste(DRAW_KS, collapse = ","), METRIC, K, SDEV_FLOOR, workers, dry)

## ---------------------------------------------------------------------------
## Fixtures (as 10 and 14 built them)
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
## Pool for a block, as 10 built it: every snapshot row except the fixture,
## one property attached; the verb restricts each draw to rows that have it.
## ---------------------------------------------------------------------------

build_pool <- function(property, exclude_ids) {
  ids <- setdiff(snap$spectra$sample_id, exclude_ids)
  if (dry) { set.seed(SEED); ids <- sample(ids, POOL_N_DRY) }
  hz  <- hz_spectra(snap, ids)
  lab <- snap$lab[match(ids, snap$lab$sample_id), c("sample_id", property)]
  bad <- !is.na(lab[[property]]) & !(lab$sample_id %in% property_rows(snap$lab, property)$ids)
  lab[[property]][bad] <- NA
  hz  <- add_response(hz, source = lab, variable = property)
  msg("[pool] %d rows on %d predictors; %s measured on %d (%d out of physical range set NA)",
      hz$data$n_rows, hz$data$n_predictors, property, sum(!is.na(lab[[property]])), sum(bad))
  hz
}

## ---------------------------------------------------------------------------
## The chain on a drawn pool, cached — the only expensive thing here
## ---------------------------------------------------------------------------

pool_fit <- function(train, property, transformation, tag) {
  f_path <- file.path(CKPT, sprintf("fit-%s.qs2", tag))
  if (file.exists(f_path)) {
    msg("[%s] reusing the cached pool fit (%s)", tag, basename(f_path))
    return(list(fit = qs2::qs_read(f_path), secs = NA_real_))
  }
  t_c <- system.time(hz <- configure(train, outcome = property,
                                     models = CONFIG$model, transformations = transformation,
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

## Out-of-fold predictions of the winning config, keyed back to sample_id
## (14's oof_residuals, unchanged).
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
## Corrections (14's, unchanged)
## ---------------------------------------------------------------------------

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

done_methods <- function(block, draw_k) {
  if (!file.exists(rows_path)) return(character())
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  r$method[r$block == block & r$draw_k == draw_k & r$metric == METRIC & r$pilot == dry]
}

## Two bars: 10's best batch arm (selection alone) and 14's global + offset
## (the correction the product cannot have).
best_selection_arm <- function(block) {
  if (!file.exists(ACC_CSV)) return(list(arm = NA_character_, rmse = NA_real_))
  r <- read.csv(ACC_CSV, stringsAsFactors = FALSE)
  r <- r[r$block == block & r$scope == "batch" & !r$pilot & is.finite(r$rmse), ]
  if (!nrow(r)) return(list(arm = NA_character_, rmse = NA_real_))
  i <- which.min(r$rmse)
  list(arm = r$arm[i], rmse = r$rmse[i])
}

global_corrected <- function(block) {
  if (!file.exists(BIAS_CSV)) return(list(global = NA_real_, offset = NA_real_))
  r <- read.csv(BIAS_CSV, stringsAsFactors = FALSE)
  r <- r[r$block == block & !r$pilot, ]
  list(global = r$rmse[r$method == "global"][1], offset = r$rmse[r$method == "offset"][1])
}

make_row <- function(block, property, draw_k, method, truth, pred, n_pool_total, n_pool, n_oof, ncomp,
                     n_fallback, sel_ref, glob_ref, secs) {
  m <- metrics_row(truth, pred)
  tibble(block = block, property = property, draw_k = as.integer(draw_k), metric = METRIC, method = method,
         k = K, sdev_floor = SDEV_FLOOR,
         n_targets = length(truth), n_scored = m$n_scored,
         n_pool_total = as.integer(n_pool_total), n_pool = as.integer(n_pool),
         n_oof = as.integer(n_oof), sim_ncomp = as.integer(ncomp),
         rmse = m$rmse, bias = m$bias, ccc = m$ccc, rpd = m$rpd, rsq = m$rsq,
         sd_truth = stats::sd(truth), n_slope_fallback = as.integer(n_fallback),
         ref_selection_arm = sel_ref$arm, ref_selection_rmse = sel_ref$rmse,
         ref_global_rmse = glob_ref$global, ref_global_offset_rmse = glob_ref$offset,
         config = paste(CONFIG, collapse = "_"), secs = secs, pilot = dry,
         ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

for (block in BLOCKS) {

  property <- switch(block, iowa = "clay", moys = "oc", stop("unknown block: ", block))
  tr       <- switch(block, iowa = "none", moys = "log")

  ## ---- fixture and pool ---------------------------------------------------
  if (block == "iowa") {
    fx   <- iowa_fixture(property)
    pool <- build_pool(property, fx$ids)
  } else {
    pool <- build_pool(property, character())
    fx   <- moys_fixture(pool$data$analysis)
  }
  n_pool_total <- pool$data$n_rows
  msg("[%s] %d targets; %s range %.2f to %.2f, sd %.2f", block, length(fx$ids), property,
      min(fx$y), max(fx$y), stats::sd(fx$y))

  sel_ref  <- best_selection_arm(block)
  glob_ref <- global_corrected(block)

  for (draw_k in DRAW_KS) {

    tag  <- sprintf("%s-%s-k%d", block, METRIC, draw_k)
    done <- done_methods(block, draw_k)
    if (all(METHODS %in% done)) { msg("[%s] all methods already recorded, skipping", tag); next }

    t0 <- proc.time()[["elapsed"]]

    ## ---- the draw: what the product would train on ------------------------
    t_s <- system.time(
      sel <- suppressWarnings(select_training(fx$hz, pool, k = draw_k, scope = "batch", properties = property,
                                              mask = MASK, metric = METRIC, space_rows = "all",
                                              verbose = FALSE))
    )
    msg("[%s] drew %d of %d pool rows in %.0f s; ncomp %d", tag, sel$data$n_rows, n_pool_total,
        t_s[["elapsed"]], sel$selection$settings$ncomp_retained)

    ## ---- the pool fit and its out-of-fold residuals -----------------------
    future::plan(future.callr::callr, workers = workers)
    pf <- pool_fit(sel, property, tr, tag)
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

    t_sp <- system.time({
      sp <- horizons:::build_similarity_space(pm$matrix[keep, , drop = FALSE], pm$wavenumbers,
                                              mask = MASK, sdev_floor = SDEV_FLOOR)
      St <- horizons:::project_similarity(sp, tm$matrix, tm$wavenumbers)
      nn <- horizons:::nearest_neighbours(St, sp$scores, k = K, metric = "euclidean", sdev = sp$sdev)
    })
    msg("[%s] similarity space: %d components, %d pool rows, %.0f s; neighbour distance %.3f to %.3f",
        tag, sp$ncomp, nrow(sp$scores), t_sp[["elapsed"]],
        min(nn$dist, na.rm = TRUE), max(nn$dist, na.rm = TRUE))

    ## ---- the uncorrected pool prediction on the fixture -------------------
    p <- as_tibble(predict(pf$fit, fx$hz, interval = FALSE))
    stopifnot(all(fx$ids %in% p$sample_id))
    pred_pool <- p$.pred[match(fx$ids, p$sample_id)]
    truth     <- fx$y

    ## ---- the three corrections --------------------------------------------
    res_by_id <- setNames(oof$resid, oof$sample_id)
    n_fallback <- 0L
    pred_offset <- pred_weighted <- pred_slope <- rep(NA_real_, length(fx$ids))
    mean_dist   <- rep(NA_real_, length(fx$ids))

    for (i in seq_along(fx$ids)) {

      nb_ids <- nn$ids[match(fx$ids[i], rownames(nn$ids)), ]
      nb_d   <- nn$dist[match(fx$ids[i], rownames(nn$dist)), ]
      ok     <- !is.na(nb_ids) & is.finite(nb_d)
      nb_ids <- nb_ids[ok]; nb_d <- nb_d[ok]
      if (!length(nb_ids)) next

      r <- unname(res_by_id[nb_ids])
      mean_dist[i] <- mean(nb_d)

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
      if (method %in% done) { msg("[%s %s] already recorded, skipping", tag, method); next }
      row <- make_row(block, property, draw_k, method, truth, preds[[method]],
                      n_pool_total, sel$data$n_rows, nrow(oof), sp$ncomp,
                      if (method == "slope") n_fallback else 0L, sel_ref, glob_ref, secs)
      append_csv(row, rows_path)
      msg("[%s %-8s] RMSE %.3f bias %+.3f CCC %.3f RPD %.2f", tag, method,
          row$rmse, row$bias, row$ccc, row$rpd)
    }
    msg("[%s] bars: selection %s %.3f | 14 global %.3f, global + offset %.3f", tag,
        sel_ref$arm, sel_ref$rmse, glob_ref$global, glob_ref$offset)

    write.csv(tibble(sample_id = fx$ids, group = fx$group, truth = truth,
                     pred_pool = pred_pool, pred_offset = pred_offset,
                     pred_weighted = pred_weighted, pred_slope = pred_slope,
                     mean_neighbour_dist = mean_dist),
              file.path(PREDS, sprintf("preds-%s-k%d.csv", block, draw_k)), row.names = FALSE)

    rm(sel, pf, oof, sp, St, nn, pm, tm); invisible(gc())
  }

  rm(pool, fx); invisible(gc())
}

if (file.exists(rows_path)) {
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  print(as.data.frame(r[r$pilot == dry, c("block", "draw_k", "method", "n_pool", "n_oof", "sim_ncomp",
                                          "rmse", "bias", "ccc", "rpd",
                                          "ref_selection_rmse", "ref_global_rmse", "ref_global_offset_rmse")]),
        row.names = FALSE, digits = 3)
}
msg("[pool-corr] done")
