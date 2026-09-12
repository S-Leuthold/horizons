## ===========================================================================
## 04 — Strategy B (clustered local, one config) and E (soft assignment)
## ===========================================================================
##
## Requires the A checkpoint for the property (winning config + global model).
## Per property:
##   1. clustering space on train_core (SNV -> SG 1st deriv -> water bands
##      out -> PCA 99 % -> mclust GMM, K by BIC over 5:11)
##   2. assign calib_ext and test rows (posterior, entropy)
##   3. per cluster with >= min_cluster_n train rows: the horizons chain with
##      A's winning config only -> evaluate -> fit(n_best = 1, UQ + AD);
##      smaller clusters fall back to A's global model
##   4. predict every test/calib row through EVERY cluster model once;
##      B takes the assigned cluster's prediction (hard assignment),
##      E blends all K by posterior (soft assignment)
##   5. shared conformal wrapper: per-cluster margins for B (pooled below
##      pool_below_n), a single margin for E; metrics; per-cluster table
##      with A's metrics on the same test rows
##
## Run: Rscript dev/experiments/2026-09-local-strategy/04-B-gmm-local.R [clay oc ph] [--workers=N] [--from=A_global|A_pilot] [--force]

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE",
           R_PARALLELLY_MAXWORKERS_LOCALHOST = "Inf")
options(parallelly.maxWorkers.localhost = Inf,
        future.globals.maxSize = 8 * 1024^3)

args     <- commandArgs(trailingOnly = TRUE)
flags    <- args[startsWith(args, "--")]
props    <- setdiff(args, flags)
force    <- "--force" %in% flags
w_arg    <- sub("^--workers=", "", grep("^--workers=", flags, value = TRUE))
from_arg <- sub("^--from=", "", grep("^--from=", flags, value = TRUE))
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
exp_dir  <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()

source(file.path(exp_dir, "00-config.R"))
suppressPackageStartupMessages(devtools::load_all(PKG_DIR, quiet = TRUE))
source(file.path(exp_dir, "helpers.R"))
exp_dirs()

if (!length(props)) props <- EXP_PROPERTIES$property
workers <- min(if (length(w_arg)) as.integer(w_arg) else MAX_WORKERS, MAX_WORKERS)
from    <- if (length(from_arg)) from_arg else "A_global"

future::plan(future::multisession, workers = workers)
on.exit(future::plan(future::sequential), add = TRUE)
EVAL_WORKERS <- EXP_CONFIG$cv_folds

snap <- load_snapshot()
msg("[B/E] snapshot %d samples; properties: %s; workers %d; A source: %s",
    nrow(snap$lab), paste(props, collapse = ", "), workers, from)

for (property in props) {

  if (has_ckpt(property, "B_gmm_oneconfig") && has_ckpt(property, "E_soft") && !force) {
    msg("[B/E] %s: checkpoints exist, skipping (use --force).", property)
    next
  }
  if (!has_ckpt(property, from)) {
    msg("[B/E] %s: no %s checkpoint yet — run 03-A-global.R first. Skipping.", property, from)
    next
  }

  A    <- load_ckpt(property, from)
  sp   <- load_splits(property); assert_splits(sp)
  spec <- property_spec(property)
  win  <- A$winner
  t_total <- system.time({

  ## -------------------------------------------------------------------------
  ## 1. Clustering on train_core
  ## -------------------------------------------------------------------------

  msg("[B] %s: clustering space on %d train_core rows", property, length(sp$train_core))
  train_df <- snap$spectra[match(sp$train_core, snap$spectra$sample_id), , drop = FALSE]
  t_clust  <- system.time({
    Xtr   <- clustering_matrix(train_df)
    clust <- fit_clustering(Xtr)
  })
  rm(Xtr, train_df); invisible(gc())
  K <- clust$k
  msg("[B] %s: K = %d (%s), %d PCs, sizes: %s  (%.1f min)", property, K, clust$model_name,
      clust$n_comp, paste(clust$train_sizes, collapse = "/"), t_clust[["elapsed"]] / 60)

  ## -------------------------------------------------------------------------
  ## 2. Assign calib_ext and test rows
  ## -------------------------------------------------------------------------

  assign_for <- function(ids) {
    df <- snap$spectra[match(ids, snap$spectra$sample_id), , drop = FALSE]
    a  <- assign_clusters(clust, clustering_matrix(df))
    a$assign$sample_id <- ids
    a
  }
  calib_a <- assign_for(sp$calib_ext)
  test_a  <- assign_for(sp$test)
  msg("[B] %s: test assignment sizes: %s; mean entropy %.3f", property,
      paste(tabulate(test_a$assign$cluster_id, K), collapse = "/"), mean(test_a$assign$entropy))

  ## -------------------------------------------------------------------------
  ## 3. Per-cluster fits with A's winning config
  ## -------------------------------------------------------------------------

  cluster_fits <- vector("list", K)
  fallback     <- integer(0)
  t_fits       <- numeric(K)

  cluster_errors <- rep(NA_character_, K)

  for (k in seq_len(K)) {
    ids_k <- sp$train_core[clust$train_assign == k]
    if (length(ids_k) < EXP_LOCAL$min_cluster_n) {
      fallback <- c(fallback, k)
      msg("[B] %s: cluster %d has %d rows < %d — falls back to the global model.",
          property, k, length(ids_k), EXP_LOCAL$min_cluster_n)
      next
    }
    msg("[B] %s: cluster %d/%d — %d rows, %s + %s", property, k, K, length(ids_k),
        win$model, win$preprocessing)
    t_k <- system.time(
      res <- try_cluster_fit(snap, ids_k, property, spec$transformation,
                             set = config_set_of(win),
                             output_dir = file.path(CHECKPOINT_DIR, sprintf("%s-B-eval-k%02d", property, k)),
                             workers = workers, label = sprintf("[B] %s cluster %d", property, k))
    )
    t_fits[k] <- t_k[["elapsed"]]
    if (is.null(res$fit)) {
      fallback <- c(fallback, k); cluster_errors[k] <- res$error
      next
    }
    cluster_fits[[k]] <- res$fit
    msg("[B] %s: cluster %d done in %.1f min (UQ %s, AD %s)", property, k,
        t_fits[k] / 60, !is.null(res$fit$models$uq), !is.null(res$fit$models$ad))
    invisible(gc())
  }

  ## -------------------------------------------------------------------------
  ## 4. Predict every row through every model, once
  ## -------------------------------------------------------------------------

  hz_test  <- hz_spectra(snap, sp$test)
  hz_calib <- hz_spectra(snap, sp$calib_ext)

  predict_all <- function(hz, ids, A_preds) {
    lapply(seq_len(K), function(k) {
      if (k %in% fallback) {
        p <- A_preds[match(ids, A_preds$sample_id), , drop = FALSE]
        p <- p[, c("sample_id", ".pred", ".pred_lower", ".pred_upper", "truth", "base_lower", "base_upper")]
        return(as_tibble(p))
      }
      p <- predict_hz(cluster_fits[[k]], hz, snap, property)
      p[match(ids, p$sample_id), c("sample_id", ".pred", ".pred_lower", ".pred_upper", "truth", "base_lower", "base_upper")]
    })
  }
  t_predall <- system.time({
    P_test  <- predict_all(hz_test,  sp$test,      A$test_preds)
    P_calib <- predict_all(hz_calib, sp$calib_ext, A$calib_preds)
  })
  msg("[B/E] %s: all-through-all predictions in %.1f min", property, t_predall[["elapsed"]] / 60)

  pick_assigned <- function(P, assign) {
    idx <- cbind(seq_along(assign$cluster_id), assign$cluster_id)
    out <- P[[1]][, "sample_id", drop = FALSE]
    for (col in c(".pred", ".pred_lower", ".pred_upper", "truth", "base_lower", "base_upper")) {
      M <- sapply(P, `[[`, col)
      out[[col]] <- M[idx]
    }
    out$cluster_id <- assign$cluster_id
    out$posterior  <- assign$posterior
    out$entropy    <- assign$entropy
    out
  }

  ## ---- B: hard assignment ----
  test_b  <- pick_assigned(P_test,  test_a$assign)
  calib_b <- pick_assigned(P_calib, calib_a$assign)
  test_b  <- conformalize_by_cluster(test_b, calib_b)
  mB_rep  <- metrics_row(test_b$truth, test_b$.pred, test_b$lower, test_b$upper)
  mB_nat  <- metrics_row(test_b$truth, test_b$.pred, test_b$.pred_lower, test_b$.pred_upper)

  ## ---- E: soft assignment ----
  blend <- function(P, z) {
    out <- P[[1]][, c("sample_id", "truth"), drop = FALSE]
    for (col in c(".pred", "base_lower", "base_upper")) {
      M <- sapply(P, `[[`, col)
      out[[col]] <- rowSums(z * M)
    }
    out
  }
  test_e  <- blend(P_test,  test_a$z)
  calib_e <- blend(P_calib, calib_a$z)
  test_e  <- conformalize(test_e, calib_e)
  mE_rep  <- metrics_row(test_e$truth, test_e$.pred, test_e$lower, test_e$upper)

  ## -------------------------------------------------------------------------
  ## 5. Per-cluster table (B vs A on the same test rows), timing, size
  ## -------------------------------------------------------------------------

  A_test <- A$test_preds
  per_cluster <- bind_rows(lapply(seq_len(K), function(k) {
    bk <- test_b[test_b$cluster_id == k, , drop = FALSE]
    if (!nrow(bk)) return(NULL)
    ak <- A_test[match(bk$sample_id, A_test$sample_id), , drop = FALSE]
    mb <- metrics_row(bk$truth, bk$.pred, bk$lower, bk$upper)
    ma <- metrics_row(ak$truth, ak$.pred, ak$lower, ak$upper)
    tibble(property = property, cluster_id = k,
           n_train = sum(clust$train_assign == k),
           n_calib = sum(calib_a$assign$cluster_id == k), n_test = nrow(bk),
           fallback = k %in% fallback, error = cluster_errors[k],
           pooled = attr(test_b, "margins")$pooled[match(k, attr(test_b, "margins")$cluster_id)],
           rpd_B = mb$rpd, rmse_B = mb$rmse, ccc_B = mb$ccc, cov_B = mb$coverage, width_B = mb$mean_width,
           rpd_A = ma$rpd, rmse_A = ma$rmse, ccc_A = ma$ccc, cov_A = ma$coverage, width_A = ma$mean_width,
           mean_entropy = mean(bk$entropy), mean_posterior = mean(bk$posterior))
  }))
  print(as.data.frame(per_cluster))

  ## predict time per 100 unknowns: assignment + one cluster model
  ids100 <- head(sp$test, 100L)
  big_k  <- setdiff(order(-clust$train_sizes), fallback)[1]
  t_pred_B <- system.time({
    a100 <- assign_for(ids100)
    invisible(predict_ids(cluster_fits[[big_k]], snap, ids100, property))
  })[["elapsed"]] * 100 / length(ids100)
  t_pred_E <- system.time({
    a100 <- assign_for(ids100)
    for (k in setdiff(seq_len(K), fallback)) invisible(predict_ids(cluster_fits[[k]], snap, ids100, property))
  })[["elapsed"]] * 100 / length(ids100)

  fitted_k   <- setdiff(seq_len(K), fallback)
  bytes_fits <- sum(vapply(fitted_k, function(k) artifact_bytes(strip_for_predict(cluster_fits[[k]])), numeric(1)))
  bytes_pred <- artifact_bytes(clust) + bytes_fits +
    if (length(fallback)) artifact_bytes(strip_for_predict(A$fit)) else 0

  }) # t_total

  common <- tibble(
    property = property, config = win$config_id, model = win$model,
    preprocessing = win$preprocessing, transformation = spec$transformation,
    n_train = length(sp$train_core), n_calib = length(sp$calib_ext), n_test = length(sp$test),
    k_clusters = K, n_fallback = length(fallback),
    split_sha = substr(sp$sha$test, 1, 12), resample = EXPERIMENT_RESAMPLE %||% 2,
    pilot = from == "A_pilot", ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )

  row_B <- bind_cols(tibble(strategy = "B_gmm_oneconfig"), common,
    tibble(rmse = mB_rep$rmse, rpd = mB_rep$rpd, ccc = mB_rep$ccc, rsq = mB_rep$rsq, bias = mB_rep$bias,
           coverage_90 = mB_rep$coverage, mean_width = mB_rep$mean_width, c_alpha = attr(test_b, "c_global"),
           coverage_90_native = mB_nat$coverage, mean_width_native = mB_nat$mean_width,
           predict_secs_per_100 = t_pred_B, artifact_bytes = bytes_pred, artifact_bytes_full = NA_real_,
           n_pooled_clusters = sum(attr(test_b, "margins")$pooled),
           secs_cluster = t_clust[["elapsed"]], secs_fit_total = sum(t_fits), secs_total = t_total[["elapsed"]]))
  row_E <- bind_cols(tibble(strategy = "E_soft"), common,
    tibble(rmse = mE_rep$rmse, rpd = mE_rep$rpd, ccc = mE_rep$ccc, rsq = mE_rep$rsq, bias = mE_rep$bias,
           coverage_90 = mE_rep$coverage, mean_width = mE_rep$mean_width, c_alpha = attr(test_e, "c_alpha"),
           coverage_90_native = NA_real_, mean_width_native = NA_real_,
           predict_secs_per_100 = t_pred_E, artifact_bytes = bytes_pred, artifact_bytes_full = NA_real_,
           n_pooled_clusters = NA_integer_,
           secs_cluster = t_clust[["elapsed"]], secs_fit_total = sum(t_fits), secs_total = t_total[["elapsed"]]))
  print(as.data.frame(bind_rows(row_B, row_E)))

  save_ckpt(list(results = row_B, per_cluster = per_cluster, clust = clust, fallback = fallback,
                 winner = win, cluster_fits = cluster_fits,
                 test_preds = test_b, calib_preds = calib_b,
                 assign_test = test_a$assign, assign_calib = calib_a$assign,
                 z_test = test_a$z, z_calib = calib_a$z, secs_fit_by_cluster = t_fits),
            property, "B_gmm_oneconfig")
  save_ckpt(list(results = row_E, test_preds = test_e, calib_preds = calib_e),
            property, "E_soft")
  write_results_row(row_B, property, "B_gmm_oneconfig")
  write_results_row(row_E, property, "E_soft")
  readr::write_csv(per_cluster, file.path(RESULTS_DIR, paste0("per-cluster-", property, "-B.csv")))
  msg("[B/E] %s: done in %.1f min total.", property, t_total[["elapsed"]] / 60)
  rm(cluster_fits, P_test, P_calib, hz_test, hz_calib); invisible(gc())
}
