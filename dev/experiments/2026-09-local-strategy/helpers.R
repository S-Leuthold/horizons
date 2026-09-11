## ===========================================================================
## helpers.R — shared functions for the 2026-09 local-strategy experiment
## ===========================================================================
##
## Source order in every strategy script:
##   Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # before horizons loads
##   source("00-config.R"); devtools::load_all(PKG_DIR); source("helpers.R")
##
## Everything horizons can do, horizons does (see README.md, "Dogfooding").
## The functions here are glue: snapshot access, split bookkeeping, the
## shared conformal wrapper, metrics rows, checkpoints, sizing/timing, and
## the clustering-space step that is not yet in the package.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---------------------------------------------------------------------------
## Logging
## ---------------------------------------------------------------------------

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", sprintf(...), "\n")

## ---------------------------------------------------------------------------
## Snapshot access
## ---------------------------------------------------------------------------

load_snapshot <- function() {
  list(
    spectra = as.data.frame(arrow::read_parquet(snapshot_path("spectra"))),
    lab     = as.data.frame(arrow::read_parquet(snapshot_path("lab")))
  )
}

wn_cols_of <- function(df) grep("^wn_[0-9]+$", names(df), value = TRUE)

## Rows usable for one property: non-NA outcome inside the physical range.
property_rows <- function(lab, property) {
  spec      <- EXP_PROPERTIES[EXP_PROPERTIES$property == property, ]
  y         <- lab[[property]]
  keep_na   <- !is.na(y)
  keep_phys <- keep_na & y >= spec$lower & y <= spec$upper
  list(ids          = lab$sample_id[keep_phys],
       n_total      = nrow(lab),
       n_na         = sum(!keep_na),
       n_impossible = sum(keep_na & !keep_phys))
}

property_spec <- function(property) {
  EXP_PROPERTIES[EXP_PROPERTIES$property == property, ]
}

## ---------------------------------------------------------------------------
## Splits
## ---------------------------------------------------------------------------

sha_ids     <- function(ids) digest::digest(sort(as.character(ids)), algo = "sha256")
splits_path <- function(property) file.path(SPLITS_DIR, paste0(property, ".qs2"))
load_splits <- function(property) qs2::qs_read(splits_path(property))

assert_splits <- function(sp) {
  stopifnot(identical(sha_ids(sp$train_core), sp$sha$train_core),
            identical(sha_ids(sp$calib_ext),  sp$sha$calib_ext),
            identical(sha_ids(sp$test),       sp$sha$test),
            length(intersect(sp$train_core, sp$test)) == 0L,
            length(intersect(sp$calib_ext,  sp$test)) == 0L,
            length(intersect(sp$train_core, sp$calib_ext)) == 0L)
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## horizons chain builders (dogfooding)
## ---------------------------------------------------------------------------

## The standardize() call every strategy uses. On the snapshot grid this is a
## no-op unless EXPERIMENT_RESAMPLE is set (pre-registered fallback).
std <- function(hz) {
  standardize(hz, resample = EXPERIMENT_RESAMPLE %||% 2, trim = c(600, 4000))
}

## Spectra-only horizons_data for a set of ids (used for prediction inputs).
hz_spectra <- function(snap, ids) {
  sp <- snap$spectra[match(ids, snap$spectra$sample_id), , drop = FALSE]
  stopifnot(!anyNA(sp$sample_id))
  std(spectra(sp, id_col = "sample_id"))
}

## Keep only the rows of the configure() cross-product that the experiment's
## config set names. config$configs + config$n_configs are the only two fields
## that describe the grid (pipeline-configure.R:507-508), so this is a
## complete, valid subset.
select_configs <- function(hz, set) {
  cfg  <- hz$config$configs
  want <- paste(set$model, set$preprocessing, set$feature_selection)
  have <- paste(cfg$model, cfg$preprocessing, cfg$feature_selection)
  keep <- have %in% want
  if (sum(keep) != nrow(set)) {
    stop("select_configs(): expected ", nrow(set), " configs, matched ", sum(keep),
         ". Available: ", paste(unique(have), collapse = " | "))
  }
  hz$config$configs   <- cfg[keep, , drop = FALSE]
  hz$config$n_configs <- sum(keep)
  hz
}

## Full training object: spectra + outcome + config set, validated.
## `set` defaults to the experiment's four-row config set; pass a one-row
## data frame for a single-config chain (strategies B and E).
build_hz <- function(snap, ids, property, transformation,
                     set    = EXP_CONFIG_SET,
                     tuning = EXP_CONFIG) {
  hz  <- hz_spectra(snap, ids)
  lab <- snap$lab[match(ids, snap$lab$sample_id), c("sample_id", property), drop = FALSE]
  stopifnot(!anyNA(lab[[property]]))
  hz <- add_response(hz, source = lab, variable = property)
  hz <- configure(hz,
                  outcome             = property,
                  models              = unique(set$model),
                  transformations     = transformation,
                  preprocessing       = unique(set$preprocessing),
                  feature_selection   = unique(set$feature_selection),
                  cv_folds            = tuning$cv_folds,
                  grid_size           = tuning$grid_size,
                  bayesian_iter       = tuning$bayesian_iter,
                  final_bayesian_iter = tuning$final_bayesian_iter)
  hz <- select_configs(hz, set)
  validate(hz)
}

## The one-row config set matching a winning config row (strategies B and E).
config_set_of <- function(win) {
  data.frame(model = win$model, preprocessing = win$preprocessing,
             feature_selection = win$feature_selection, stringsAsFactors = FALSE)
}

## Winning config of an evaluated object, as a one-row tibble.
winning_config <- function(hz_eval) {
  cfg <- hz_eval$config$configs
  cfg[cfg$config_id == hz_eval$evaluation$best_config, , drop = FALSE]
}

## Predict a fitted horizons object on a prebuilt spectra object. Returns one
## row per sample with truth, .pred, horizons' native bounds (NA when UQ was
## not available), and base_lower/base_upper for the conformal wrapper.
## Rows come back in the order of `hz`'s samples.
predict_hz <- function(fit_obj, hz, snap, property) {
  out <- as_tibble(predict(fit_obj, hz, interval = TRUE))
  if (!".pred_lower" %in% names(out)) out$.pred_lower <- NA_real_
  if (!".pred_upper" %in% names(out)) out$.pred_upper <- NA_real_
  out$truth      <- snap$lab[[property]][match(out$sample_id, snap$lab$sample_id)]
  out$base_lower <- ifelse(is.finite(out$.pred_lower), out$.pred_lower, out$.pred)
  out$base_upper <- ifelse(is.finite(out$.pred_upper), out$.pred_upper, out$.pred)
  out
}

## Same, building the spectra object from ids first.
predict_ids <- function(fit_obj, snap, ids, property) {
  hz  <- hz_spectra(snap, ids)
  out <- predict(fit_obj, hz, interval = TRUE)
  out <- as_tibble(out)
  if (!".pred_lower" %in% names(out)) out$.pred_lower <- NA_real_
  if (!".pred_upper" %in% names(out)) out$.pred_upper <- NA_real_
  out$truth      <- snap$lab[[property]][match(out$sample_id, snap$lab$sample_id)]
  out$base_lower <- ifelse(is.finite(out$.pred_lower), out$.pred_lower, out$.pred)
  out$base_upper <- ifelse(is.finite(out$.pred_upper), out$.pred_upper, out$.pred)
  out
}

## ---------------------------------------------------------------------------
## Shared conformal wrapper
## ---------------------------------------------------------------------------
## Split conformal on the external calibration set, applied on top of
## whatever base interval a strategy produces (a point for D). Scores are
## signed, so the margin can be negative and tighten an over-wide base.

conformal_margin <- function(truth, lower, upper, level = UQ_LEVEL) {
  ok     <- is.finite(truth) & is.finite(lower) & is.finite(upper)
  scores <- pmax(lower[ok] - truth[ok], truth[ok] - upper[ok])
  compute_c_alpha(scores, level)
}

conformalize <- function(test, calib, level = UQ_LEVEL) {
  c_alpha    <- conformal_margin(calib$truth, calib$base_lower, calib$base_upper, level)
  test$lower <- test$base_lower - c_alpha
  test$upper <- test$base_upper + c_alpha
  attr(test, "c_alpha") <- c_alpha
  attr(test, "n_calib") <- sum(is.finite(calib$truth))
  test
}

## Per-cluster margins, pooled to the global margin below `pool_below_n`.
conformalize_by_cluster <- function(test, calib, level = UQ_LEVEL,
                                    pool_below_n = EXP_LOCAL$pool_below_n) {
  c_global <- conformal_margin(calib$truth, calib$base_lower, calib$base_upper, level)
  margins  <- lapply(sort(unique(test$cluster_id)), function(k) {
    ck     <- calib[calib$cluster_id == k, , drop = FALSE]
    n      <- sum(is.finite(ck$truth))
    pooled <- n < pool_below_n
    c_k    <- if (pooled) c_global else conformal_margin(ck$truth, ck$base_lower, ck$base_upper, level)
    tibble(cluster_id = k, n_calib = n, pooled = pooled, c_alpha = c_k)
  })
  margins    <- bind_rows(margins)
  test       <- left_join(test, margins, by = "cluster_id")
  test$lower <- test$base_lower - test$c_alpha
  test$upper <- test$base_upper + test$c_alpha
  attr(test, "margins")  <- margins
  attr(test, "c_global") <- c_global
  test
}

## ---------------------------------------------------------------------------
## Metrics
## ---------------------------------------------------------------------------

metrics_row <- function(truth, pred, lower = NULL, upper = NULL) {
  ok  <- is.finite(truth) & is.finite(pred)
  out <- tibble(
    n_scored = sum(ok),
    rmse     = yardstick::rmse_vec(truth[ok], pred[ok]),
    rpd      = rpd_vec(truth[ok], pred[ok]),
    ccc      = ccc_vec(truth[ok], pred[ok]),
    rsq      = yardstick::rsq_vec(truth[ok], pred[ok]),
    bias     = mean(pred[ok] - truth[ok])
  )
  if (!is.null(lower)) {
    okb <- ok & is.finite(lower) & is.finite(upper)
    out$coverage   <- if (any(okb)) mean(truth[okb] >= lower[okb] & truth[okb] <= upper[okb]) else NA_real_
    out$mean_width <- if (any(okb)) mean(upper[okb] - lower[okb]) else NA_real_
    out$n_interval <- sum(okb)
  }
  out
}

## ---------------------------------------------------------------------------
## Checkpoints and results
## ---------------------------------------------------------------------------

ckpt_path <- function(property, strategy) file.path(CHECKPOINT_DIR, paste0(property, "-", strategy, ".qs2"))
has_ckpt  <- function(property, strategy) file.exists(ckpt_path(property, strategy))
save_ckpt <- function(x, property, strategy) { qs2::qs_save(x, ckpt_path(property, strategy)); invisible(ckpt_path(property, strategy)) }
load_ckpt <- function(property, strategy) qs2::qs_read(ckpt_path(property, strategy))

write_results_row <- function(row, property, strategy) {
  f <- file.path(RESULTS_DIR, paste0("row-", property, "-", strategy, ".csv"))
  readr::write_csv(row, f)
  invisible(f)
}

## ---------------------------------------------------------------------------
## Sizing and timing
## ---------------------------------------------------------------------------

artifact_bytes <- function(obj) {
  f <- tempfile(fileext = ".qs2")
  on.exit(unlink(f), add = TRUE)
  qs2::qs_save(obj, f)
  file.size(f)
}

## What a user would have to ship to predict: the fitted object without its
## training table, splits, or OOF predictions. (The UQ bundles' prepped
## recipes still retain training data — measured as-is and noted.)
strip_for_predict <- function(fit_obj) {
  fit_obj$data$analysis        <- fit_obj$data$analysis[0, , drop = FALSE]
  fit_obj$evaluation$split     <- NULL
  fit_obj$models$split         <- NULL
  fit_obj$models$cv_predictions <- NULL
  fit_obj
}

time_predict_per_100 <- function(fit_obj, snap, ids, property, n = 100L) {
  ids <- ids[seq_len(min(n, length(ids)))]
  el  <- system.time(invisible(predict_ids(fit_obj, snap, ids, property)))[["elapsed"]]
  el * 100 / length(ids)
}

## ---------------------------------------------------------------------------
## Clustering space (strategies B / C / E) — not yet in the package
## ---------------------------------------------------------------------------
## SNV -> Savitzky-Golay 1st derivative -> drop water-band columns -> PCA to
## 99 % variance -> Gaussian mixture (mclust, K by BIC). The derivative is
## taken on the full spectrum and the water bands removed afterwards so the
## filter never straddles a gap.

clustering_matrix <- function(spectra_df) {
  cols <- wn_cols_of(spectra_df)
  m    <- as.matrix(spectra_df[, cols, drop = FALSE])
  m    <- prospectr::standardNormalVariate(m)
  m    <- prospectr::savitzkyGolay(m, m = EXP_LOCAL$sg_m, p = EXP_LOCAL$sg_p, w = EXP_LOCAL$sg_w)
  wn   <- if (!is.null(colnames(m))) {
    as.numeric(sub("wn_", "", colnames(m)))
  } else {
    h <- (EXP_LOCAL$sg_w - 1) / 2
    as.numeric(sub("wn_", "", cols))[(h + 1):(length(cols) - h)]
  }
  drop <- rep(FALSE, length(wn))
  for (b in EXP_LOCAL$water_bands) drop <- drop | (wn >= b[1] & wn <= b[2])
  m <- m[, !drop, drop = FALSE]
  m[!is.finite(m)] <- 0
  attr(m, "wn") <- wn[!drop]
  m
}

fit_clustering <- function(train_mat, seed = SEED) {
  set.seed(seed)
  pca    <- stats::prcomp(train_mat, center = TRUE, scale. = FALSE)
  cum    <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  n_comp <- min(which(cum >= EXP_LOCAL$variance_threshold)[1], EXP_LOCAL$pca_max_comp)
  scores <- pca$x[, seq_len(n_comp), drop = FALSE]
  gmm    <- mclust::Mclust(scores, G = EXP_LOCAL$k_range, verbose = FALSE)
  if (is.null(gmm)) stop("mclust::Mclust returned NULL — no model could be fitted.")
  ## Keep only what assignment needs (drop the full score matrix to save space).
  pca$x <- NULL
  list(pca = pca, n_comp = n_comp, gmm = gmm, k = gmm$G, model_name = gmm$modelName,
       bic = gmm$bic, train_assign = as.integer(gmm$classification),
       train_sizes = as.integer(table(gmm$classification)))
}

assign_clusters <- function(clust, new_mat) {
  scores <- stats::predict(clust$pca, new_mat)[, seq_len(clust$n_comp), drop = FALSE]
  pr     <- stats::predict(clust$gmm, newdata = scores)
  z      <- pr$z
  K      <- ncol(z)
  zz     <- ifelse(z > 0, z * log(z), 0)
  list(
    assign = tibble(cluster_id = as.integer(pr$classification),
                    posterior  = apply(z, 1, max),
                    entropy    = -rowSums(zz) / log(K)),
    z = z
  )
}
