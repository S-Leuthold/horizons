## ===========================================================================
## 05 — Strategy C: clustered local, per-cluster config selection
## ===========================================================================
##
## Uses B's clusters and assignments (so B and C differ ONLY in how the config
## is chosen), and A's global model for the same fallback clusters. Per
## cluster: configure(4 configs) -> evaluate() picks that cluster's winner ->
## fit(n_best = 1, UQ + AD). Predictions, wrapper, metrics as in B.
##
## Run: Rscript dev/experiments/2026-09-local-strategy/05-C-gmm-perconfig.R [clay oc ph] [--workers=N] [--from=A_global|A_pilot] [--force]

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
strategy <- "C_gmm_perconfig"

future::plan(future::multisession, workers = workers)
on.exit(future::plan(future::sequential), add = TRUE)
EVAL_WORKERS <- EXP_CONFIG$cv_folds

snap <- load_snapshot()
msg("[C] snapshot %d samples; properties: %s; workers %d", nrow(snap$lab), paste(props, collapse = ", "), workers)

for (property in props) {

  if (has_ckpt(property, strategy) && !force) {
    msg("[C] %s: checkpoint exists, skipping (use --force).", property); next
  }
  if (!has_ckpt(property, "B_gmm_oneconfig") || !has_ckpt(property, from)) {
    msg("[C] %s: needs B_gmm_oneconfig and %s checkpoints first. Skipping.", property, from); next
  }

  B    <- load_ckpt(property, "B_gmm_oneconfig")
  A    <- load_ckpt(property, from)
  sp   <- load_splits(property); assert_splits(sp)
  spec <- property_spec(property)
  clust <- B$clust; K <- clust$k; fallback <- B$fallback

  t_total <- system.time({

  ## -------------------------------------------------------------------------
  ## Per-cluster screening + fit
  ## -------------------------------------------------------------------------

  cluster_fits <- vector("list", K); winners <- vector("list", K); t_fits <- numeric(K)
  cluster_errors <- rep(NA_character_, K)
  fallback_c <- fallback
  for (k in setdiff(seq_len(K), fallback)) {
    ids_k <- sp$train_core[clust$train_assign == k]
    msg("[C] %s: cluster %d/%d — %d rows, screening %d configs", property, k, K, length(ids_k),
        nrow(EXP_CONFIG_SET))
    t_k <- system.time(
      res <- try_cluster_fit(snap, ids_k, property, spec$transformation,
                             set = EXP_CONFIG_SET,
                             output_dir = file.path(CHECKPOINT_DIR, sprintf("%s-C-eval-k%02d", property, k)),
                             workers = workers, label = sprintf("[C] %s cluster %d", property, k))
    )
    t_fits[k] <- t_k[["elapsed"]]
    if (is.null(res$fit)) {
      fallback_c <- c(fallback_c, k); cluster_errors[k] <- res$error
      next
    }
    cluster_fits[[k]] <- res$fit; winners[[k]] <- res$winner
    msg("[C] %s: cluster %d winner %s + %s; done in %.1f min", property, k,
        winners[[k]]$model, winners[[k]]$preprocessing, t_fits[k] / 60)
    invisible(gc())
  }
  fallback <- sort(unique(fallback_c))

  ## -------------------------------------------------------------------------
  ## Predictions (hard assignment, B's assignments)
  ## -------------------------------------------------------------------------

  hz_test  <- hz_spectra(snap, sp$test)
  hz_calib <- hz_spectra(snap, sp$calib_ext)
  cols <- c("sample_id", ".pred", ".pred_lower", ".pred_upper", "truth", "base_lower", "base_upper")

  predict_assigned <- function(hz, ids, assign, A_preds) {
    parts <- lapply(seq_len(K), function(k) {
      idk <- ids[assign$cluster_id == k]
      if (!length(idk)) return(NULL)
      if (k %in% fallback) {
        p <- as_tibble(A_preds[match(idk, A_preds$sample_id), cols, drop = FALSE])
      } else {
        p <- predict_hz(cluster_fits[[k]], hz, snap, property)
        p <- p[match(idk, p$sample_id), cols, drop = FALSE]
      }
      p$cluster_id <- k
      p
    })
    out <- bind_rows(parts)
    out <- out[match(ids, out$sample_id), , drop = FALSE]
    out$posterior <- assign$posterior; out$entropy <- assign$entropy
    out
  }
  test_c  <- predict_assigned(hz_test,  sp$test,      B$assign_test,  A$test_preds)
  calib_c <- predict_assigned(hz_calib, sp$calib_ext, B$assign_calib, A$calib_preds)
  test_c  <- conformalize_by_cluster(test_c, calib_c)
  m_rep   <- metrics_row(test_c$truth, test_c$.pred, test_c$lower, test_c$upper)
  m_nat   <- metrics_row(test_c$truth, test_c$.pred, test_c$.pred_lower, test_c$.pred_upper)

  B_test <- B$test_preds; A_test <- A$test_preds
  per_cluster <- bind_rows(lapply(seq_len(K), function(k) {
    ck <- test_c[test_c$cluster_id == k, , drop = FALSE]
    if (!nrow(ck)) return(NULL)
    bk <- B_test[match(ck$sample_id, B_test$sample_id), , drop = FALSE]
    ak <- A_test[match(ck$sample_id, A_test$sample_id), , drop = FALSE]
    mc <- metrics_row(ck$truth, ck$.pred, ck$lower, ck$upper)
    mb <- metrics_row(bk$truth, bk$.pred, bk$lower, bk$upper)
    ma <- metrics_row(ak$truth, ak$.pred, ak$lower, ak$upper)
    w  <- winners[[k]]
    tibble(property = property, cluster_id = k, n_train = sum(clust$train_assign == k), n_test = nrow(ck),
           fallback = k %in% fallback,
           winner_model = if (is.null(w)) NA_character_ else w$model,
           winner_preprocessing = if (is.null(w)) NA_character_ else w$preprocessing,
           rpd_C = mc$rpd, rmse_C = mc$rmse, ccc_C = mc$ccc, cov_C = mc$coverage, width_C = mc$mean_width,
           rpd_B = mb$rpd, rpd_A = ma$rpd)
  }))
  print(as.data.frame(per_cluster))

  ids100 <- head(sp$test, 100L)
  big_k  <- setdiff(order(-clust$train_sizes), fallback)[1]
  t_pred <- system.time({
    df <- snap$spectra[match(ids100, snap$spectra$sample_id), , drop = FALSE]
    invisible(assign_clusters(clust, clustering_matrix(df)))
    invisible(predict_ids(cluster_fits[[big_k]], snap, ids100, property))
  })[["elapsed"]] * 100 / length(ids100)

  fitted_k   <- setdiff(seq_len(K), fallback)
  bytes_pred <- artifact_bytes(clust) +
    sum(vapply(fitted_k, function(k) artifact_bytes(strip_for_predict(cluster_fits[[k]])), numeric(1))) +
    if (length(fallback)) artifact_bytes(strip_for_predict(A$fit)) else 0

  }) # t_total

  win_desc <- paste(vapply(fitted_k, function(k) paste0(k, ":", winners[[k]]$model, "+", winners[[k]]$preprocessing), character(1)), collapse = ";")
  row <- tibble(
    property = property, strategy = strategy,
    config = win_desc, model = "per-cluster", preprocessing = "per-cluster", transformation = spec$transformation,
    n_train = length(sp$train_core), n_calib = length(sp$calib_ext), n_test = length(sp$test),
    rmse = m_rep$rmse, rpd = m_rep$rpd, ccc = m_rep$ccc, rsq = m_rep$rsq, bias = m_rep$bias,
    coverage_90 = m_rep$coverage, mean_width = m_rep$mean_width, c_alpha = attr(test_c, "c_global"),
    coverage_90_native = m_nat$coverage, mean_width_native = m_nat$mean_width,
    predict_secs_per_100 = t_pred, artifact_bytes = bytes_pred, artifact_bytes_full = NA_real_,
    k_clusters = K, n_fallback = length(fallback), n_pooled_clusters = sum(attr(test_c, "margins")$pooled),
    secs_fit_total = sum(t_fits), secs_total = t_total[["elapsed"]],
    split_sha = substr(sp$sha$test, 1, 12), resample = EXPERIMENT_RESAMPLE %||% 2,
    pilot = from == "A_pilot", ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  print(as.data.frame(row))

  save_ckpt(list(results = row, per_cluster = per_cluster, winners = winners, fallback = fallback,
                 cluster_fits = cluster_fits, test_preds = test_c, calib_preds = calib_c,
                 secs_fit_by_cluster = t_fits), property, strategy)
  write_results_row(row, property, strategy)
  readr::write_csv(per_cluster, file.path(RESULTS_DIR, paste0("per-cluster-", property, "-C.csv")))
  msg("[C] %s: done in %.1f min total.", property, t_total[["elapsed"]] / 60)
  rm(cluster_fits, hz_test, hz_calib); invisible(gc())
}
