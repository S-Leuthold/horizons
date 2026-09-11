## ===========================================================================
## 06 — Strategy D: memory-based learning (resemble::mbl)
## ===========================================================================
##
## The one strategy that sits outside horizons: neighbours per unknown in PCA
## space, a local weighted-average PLS fitted on the fly. The library is
## train_core. k and the preprocessing (snv vs snv + 1st derivative) are
## chosen by resemble's library-internal nearest-neighbour validation (NNv),
## so calib_ext stays clean for the conformal margin.
##
## Preprocessing mirrors the horizons configs by hand (prospectr SNV and
## Savitzky-Golay) on spectra resampled through horizons' standardize(), so
## the axis matches every other strategy. The response transformation is
## horizons' own (log = log(y + 1), inverted with back_transform_predictions()).
##
## Run: Rscript dev/experiments/2026-09-local-strategy/06-D-mbl.R [clay oc ph] [--dry] [--force]
##   --dry : 800 train / 100 calib / 100 test rows, sequential, prints the
##           structure of resemble's return objects. Writes no checkpoint.

args     <- commandArgs(trailingOnly = TRUE)
flags    <- args[startsWith(args, "--")]
props    <- setdiff(args, flags)
dry      <- "--dry" %in% flags
force    <- "--force" %in% flags
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
exp_dir  <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()

source(file.path(exp_dir, "00-config.R"))
suppressPackageStartupMessages({
  devtools::load_all(PKG_DIR, quiet = TRUE)
  library(resemble)
})
source(file.path(exp_dir, "helpers.R"))
exp_dirs()

if (!length(props)) props <- EXP_PROPERTIES$property
strategy <- "D_mbl"
snap     <- load_snapshot()
msg("[D] snapshot %d samples; properties: %s; dry = %s", nrow(snap$lab), paste(props, collapse = ", "), dry)

## ---------------------------------------------------------------------------
## Preprocessed matrix for a set of ids (resampled through horizons)
## ---------------------------------------------------------------------------

prep_matrix <- function(ids, prep = c("snv", "snv_deriv1")) {
  prep <- match.arg(prep)
  hz   <- hz_spectra(snap, ids)                       # standardize(): resample + trim
  cols <- wn_cols_of(hz$data$analysis)
  X    <- as.matrix(hz$data$analysis[, cols, drop = FALSE])
  rownames(X) <- hz$data$analysis$sample_id
  X <- prospectr::standardNormalVariate(X)
  if (prep == "snv_deriv1") {
    X <- prospectr::savitzkyGolay(X, m = EXP_LOCAL$sg_m, p = EXP_LOCAL$sg_p, w = EXP_LOCAL$sg_w)
  }
  X[!is.finite(X)] <- 0
  X[match(ids, rownames(X)), , drop = FALSE]
}

forward_y <- function(y, transformation) {
  switch(transformation, none = y, log = log(y + 1), sqrt = sqrt(y),
         stop("unknown transformation: ", transformation))
}
inverse_y <- function(y, transformation) {
  if (transformation == "none") return(y)
  back_transform_predictions(y, transformation = transformation, warn = FALSE)
}

## Pull the per-k validation table and the per-k predictions out of an mbl
## object defensively (resemble 3.x); dry mode prints the structures.
nnv_table <- function(m) {
  v <- m$validation_results
  if (is.null(v)) return(NULL)
  nn <- v$nearest_neighbor_validation %||% v[[1]]
  as_tibble(as.data.frame(nn))
}

for (property in props) {

  if (!dry && has_ckpt(property, strategy) && !force) {
    msg("[D] %s: checkpoint exists, skipping (use --force).", property)
    next
  }

  sp   <- load_splits(property); assert_splits(sp)
  spec <- property_spec(property)
  tr   <- spec$transformation

  ids_train <- sp$train_core; ids_calib <- sp$calib_ext; ids_test <- sp$test
  if (dry) {
    set.seed(SEED)
    ids_train <- sample(ids_train, 800L); ids_calib <- sample(ids_calib, 100L); ids_test <- sample(ids_test, 100L)
  }
  y_of <- function(ids) snap$lab[[property]][match(ids, snap$lab$sample_id)]
  Yr   <- forward_y(y_of(ids_train), tr)

  t_total <- system.time({

  ## -------------------------------------------------------------------------
  ## 1. Choose prep and k by NNv on the library (test call; Yu not used)
  ## -------------------------------------------------------------------------

  fits <- list(); nnv <- list()
  for (prep in EXP_CONFIG$preprocessing) {
    msg("[D] %s: prep = %s — building matrices (%d / %d / %d rows)", property, prep,
        length(ids_train), length(ids_calib), length(ids_test))
    Xr <- prep_matrix(ids_train, prep)
    Xt <- prep_matrix(ids_test,  prep)
    t_fit <- system.time(
      m <- mbl(Xr = Xr, Yr = Yr, Xu = Xt,
               neighbors   = neighbors_k(EXP_MBL$k),
               diss_method = diss_pca(ncomp = ncomp_by_opc(40L)),
               fit_method  = fit_wapls(min_ncomp = EXP_MBL$pls_c[1], max_ncomp = EXP_MBL$pls_c[2]),
               control     = mbl_control(validation_type = "NNv", allow_parallel = FALSE, blas_threads = 1L),
               verbose = FALSE, seed = SEED)
    )
    fits[[prep]] <- list(m = m, Xr = Xr, secs = t_fit[["elapsed"]])
    tab <- nnv_table(m); tab$prep <- prep
    nnv[[prep]] <- tab
    msg("[D] %s: prep = %s done in %.1f min", property, prep, t_fit[["elapsed"]] / 60)
    if (dry) {
      cat("\n--- str(m$validation_results) ---\n"); str(m$validation_results, max.level = 2)
      cat("\n--- nnv table ---\n"); print(as.data.frame(tab))
      gp <- get_predictions(m)
      cat("\n--- get_predictions(): class ", class(gp)[1], " dim ", paste(dim(gp), collapse = "x"), " ---\n"); print(head(as.data.frame(gp), 3))
      cat("\n--- names(m) ---\n"); print(names(m))
    }
  }
  nnv_all <- bind_rows(nnv)
  ## expected columns: k (or k_neighbors) and rmse; be defensive about names
  kcol <- intersect(c("k", "k_neighbors", "neighbors"), names(nnv_all))[1]
  rcol <- intersect(c("rmse", "rmse_nnv", "RMSE"), names(nnv_all))[1]
  if (is.na(kcol) || is.na(rcol)) stop("Cannot find k / rmse columns in NNv table: ", paste(names(nnv_all), collapse = ", "))
  best <- nnv_all[which.min(nnv_all[[rcol]]), ]
  best_prep <- best$prep; best_k <- as.integer(best[[kcol]])
  msg("[D] %s: NNv winner prep = %s, k = %d (NNv rmse %.4f on the transformed scale)",
      property, best_prep, best_k, best[[rcol]])

  ## -------------------------------------------------------------------------
  ## 2. Predictions at the winning (prep, k): test from the fit above, calib fresh
  ## -------------------------------------------------------------------------

  pred_at_k <- function(m, k, ids) {
    gp <- as.data.frame(get_predictions(m))
    ## resemble returns one column per k (named by k) or a long table — handle both
    if (all(c("k", "pred") %in% names(gp)) || all(c("k_neighbors", "pred") %in% names(gp))) {
      kc <- intersect(c("k", "k_neighbors"), names(gp))[1]
      p  <- gp$pred[gp[[kc]] == k]
    } else {
      cand <- grep(paste0("(^|_)", k, "$"), names(gp), value = TRUE)
      if (!length(cand)) cand <- grep(paste0(k), names(gp), value = TRUE)
      if (!length(cand)) stop("Cannot locate predictions for k = ", k, " in: ", paste(names(gp), collapse = ", "))
      p <- gp[[cand[1]]]
    }
    tibble(sample_id = ids, .pred_t = as.numeric(p))
  }

  test_p <- pred_at_k(fits[[best_prep]]$m, best_k, ids_test)

  Xr <- fits[[best_prep]]$Xr
  Xc <- prep_matrix(ids_calib, best_prep)
  m_c <- mbl(Xr = Xr, Yr = Yr, Xu = Xc, neighbors = neighbors_k(best_k),
             diss_method = diss_pca(ncomp = ncomp_by_opc(40L)),
             fit_method  = fit_wapls(min_ncomp = EXP_MBL$pls_c[1], max_ncomp = EXP_MBL$pls_c[2]),
             control     = mbl_control(validation_type = "NNv", allow_parallel = FALSE, blas_threads = 1L),
             verbose = FALSE, seed = SEED)
  calib_p <- pred_at_k(m_c, best_k, ids_calib)

  finish <- function(p, ids) {
    p$.pred      <- inverse_y(p$.pred_t, tr)
    p$truth      <- y_of(ids)
    p$base_lower <- p$.pred
    p$base_upper <- p$.pred
    p$.pred_lower <- NA_real_; p$.pred_upper <- NA_real_
    p
  }
  test_p  <- finish(test_p,  ids_test)
  calib_p <- finish(calib_p, ids_calib)
  test_p  <- conformalize(test_p, calib_p)
  m_rep   <- metrics_row(test_p$truth, test_p$.pred, test_p$lower, test_p$upper)

  ## -------------------------------------------------------------------------
  ## 3. Predict time per 100 unknowns (single-threaded) and library bytes
  ## -------------------------------------------------------------------------

  ids100 <- head(ids_test, 100L)
  t_pred <- system.time({
    X100 <- prep_matrix(ids100, best_prep)
    invisible(mbl(Xr = Xr, Yr = Yr, Xu = X100, neighbors = neighbors_k(best_k),
                  diss_method = diss_pca(ncomp = ncomp_by_opc(40L)),
                  fit_method  = fit_wapls(min_ncomp = EXP_MBL$pls_c[1], max_ncomp = EXP_MBL$pls_c[2]),
                  control     = mbl_control(validation_type = "none", allow_parallel = FALSE, blas_threads = 1L),
                  verbose = FALSE, seed = SEED))
  })[["elapsed"]] * 100 / length(ids100)

  lib_obj    <- list(Xr = Xr, Yr = Yr, prep = best_prep, k = best_k, transformation = tr)
  bytes_lib  <- artifact_bytes(lib_obj)                       # double precision, qs2-compressed
  bytes_f32  <- round(bytes_lib / 2)                          # float32 estimate (no native float32 in R)
  pca_est    <- artifact_bytes(stats::prcomp(Xr, rank. = 40L)$rotation) + length(Yr) * 8

  }) # t_total

  row <- tibble(
    property = property, strategy = strategy,
    config = paste0("mbl_wapls_", best_prep, "_k", best_k), model = "wapls",
    preprocessing = best_prep, transformation = tr,
    n_train = length(ids_train), n_calib = length(ids_calib), n_test = length(ids_test),
    rmse = m_rep$rmse, rpd = m_rep$rpd, ccc = m_rep$ccc, rsq = m_rep$rsq, bias = m_rep$bias,
    coverage_90 = m_rep$coverage, mean_width = m_rep$mean_width, c_alpha = attr(test_p, "c_alpha"),
    coverage_90_native = NA_real_, mean_width_native = NA_real_,
    predict_secs_per_100 = t_pred,
    artifact_bytes = bytes_f32, artifact_bytes_full = bytes_lib, artifact_bytes_pca_scores = pca_est,
    k_clusters = NA_integer_, n_fallback = NA_integer_, n_pooled_clusters = NA_integer_,
    k_neighbors = best_k, secs_total = t_total[["elapsed"]],
    split_sha = substr(sp$sha$test, 1, 12), resample = EXPERIMENT_RESAMPLE %||% 2,
    pilot = dry, ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  print(as.data.frame(row))

  if (!dry) {
    save_ckpt(list(results = row, nnv = nnv_all, best = list(prep = best_prep, k = best_k),
                   test_preds = test_p, calib_preds = calib_p, library = lib_obj),
              property, strategy)
    write_results_row(row, property, strategy)
  }
  msg("[D] %s: done in %.1f min.", property, t_total[["elapsed"]] / 60)
}
