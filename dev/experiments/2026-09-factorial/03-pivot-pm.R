## ===========================================================================
## 03 — The pivot: arm P (global PLS) against arm M (memory-based learning)
## ===========================================================================
##
## Purpose (2026-09-16). Experiment 1 found memory-based learning beating the
## global model on every property, but the comparison was confounded: MBL ran
## weighted-average PLS on supervised components while every other arm ran
## Cubist on forced PCA. This script holds the learner constant. Once it is
## PLS on both sides, does predicting each unknown from its most similar
## library samples beat predicting it from the whole library?
##
##   P_pls    global PLS at the CV-best component count
##   P_wapls  global weighted-average PLS over components 5-20 (the same
##            averaging fit_wapls() does locally) — the row that isolates
##            locality from the learner
##   M        resemble::mbl() with experiment 1's arm-D settings, at 2 cm-1
##
## Same splits as everything else: train_core is the library and training
## set, calib_ext the conformal margin only, test the fixed 5,239 rows the
## learning curve scored. Full resolution (2 cm-1). Preprocessing mirrors
## arm D by hand (SNV, SNV + Savitzky-Golay m1 p2 w11) on the standardized
## axis, so M here is arm D at full resolution and nothing else changes.
##
## Run (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_pivot.sh [clay oc ph] [--workers=N] > /dev/null 2>&1 &
##   Rscript dev/experiments/2026-09-factorial/03-pivot-pm.R [clay oc ph] [--workers=N] [--ncomp=20] [--arms=P,M] [--dry]
##   --dry : 800 train / 100 calib / 100 test rows, 2 workers, no checkpoint.
##
## Resumable: an arm whose row CSV exists is skipped.

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading

args     <- commandArgs(trailingOnly = TRUE)
flags    <- args[startsWith(args, "--")]
props    <- setdiff(args, flags)
dry      <- "--dry" %in% flags
w_arg    <- sub("^--workers=", "", grep("^--workers=", flags, value = TRUE))
nc_arg   <- sub("^--ncomp=",   "", grep("^--ncomp=",   flags, value = TRUE))
arms_arg <- sub("^--arms=",    "", grep("^--arms=",    flags, value = TRUE))
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
exp1_dir <- file.path(dirname(this_dir), "2026-09-local-strategy")

source(file.path(exp1_dir, "00-config.R"))
suppressPackageStartupMessages({
  library(horizons)
  library(resemble)
})
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)

## ---------------------------------------------------------------------------
## Settings
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- NULL          # full resolution: std() -> resample = 2
if (!length(props)) props <- "clay"
workers <- if (dry) 2L else if (length(w_arg)) as.integer(w_arg) else 10L
stopifnot(workers >= 1L)

PREPS      <- c("snv", "snv_deriv1")
PLS_NCOMP  <- if (length(nc_arg)) as.integer(nc_arg) else 20L   # global PLS components fitted
ARMS       <- if (length(arms_arg)) strsplit(arms_arg, ",")[[1]] else c("P", "M")
WAPLS_RANGE <- EXP_MBL$pls_c         # c(5, 20): arm D's local range, used by M
MBL_K       <- c(50L, 100L, 200L, 400L, 800L, 1600L)   # arm D's grid, extended: clay's NNv was still improving at 400
P_WA_RANGE  <- c(5L, PLS_NCOMP)      # the global weighted average spans what was fitted
NNV_CHUNK  <- if (dry) 50L else 1000L
PIVOT_DIR  <- file.path(this_dir, "results", "pivot")
PIVOT_CKPT <- file.path(PIVOT_DIR, "checkpoints")
for (d in c(PIVOT_DIR, PIVOT_CKPT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

snap <- load_snapshot()
msg("[pivot] snapshot %d samples; properties: %s; workers %d; dry = %s",
    nrow(snap$lab), paste(props, collapse = ", "), workers, dry)

source(file.path(this_dir, "pm-helpers.R"))   # prep_matrix, pls_fit, mbl_chunked, scoring

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

## Forked workers: the library is shared copy-on-write, nothing is serialized
## to a fresh process. callr failed on oc twice with 'error writing to
## connection' once the per-future payload passed ~0.5 GB (2026-09-16 18:20,
## 18:28); forking is the Linux/macOS answer and what a laptop user gets.
future::plan(future::multicore, workers = workers)
on.exit(future::plan(future::sequential), add = TRUE)

for (property in props) {

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
  n_train <- length(ids_train); n_test <- length(ids_test)

  msg("[pivot] %s: %d train / %d calib / %d test; transformation = %s", property, n_train, length(ids_calib), n_test, tr)

  ## Matrices once per preprocessing -------------------------------------------
  t_mat <- system.time({
    Xr <- lapply(PREPS, function(p) prep_matrix(ids_train, p)); names(Xr) <- PREPS
    Xc <- lapply(PREPS, function(p) prep_matrix(ids_calib, p)); names(Xc) <- PREPS
    Xt <- lapply(PREPS, function(p) prep_matrix(ids_test,  p)); names(Xt) <- PREPS
  })
  msg("[pivot] %s: matrices built in %.1f s (%d columns snv, %d snv_deriv1)", property, t_mat[["elapsed"]],
      ncol(Xr$snv), ncol(Xr$snv_deriv1))

  ## -------------------------------------------------------------------------
  ## Arm P: global PLS, CV inside train_core, two rows
  ## -------------------------------------------------------------------------

  if (!"P" %in% ARMS) {
    msg("[pivot] %s: P not requested, skipping", property)
  } else if (!dry && file.exists(row_path(property, "P_pls")) && file.exists(row_path(property, "P_wapls"))) {
    msg("[pivot] %s: P rows exist, skipping", property)
  } else {

    t_p <- system.time({

      set.seed(SEED)
      fold_id <- sample(rep(seq_len(EXP_CONFIG$cv_folds), length.out = n_train))
      wa_idx  <- seq(P_WA_RANGE[1], P_WA_RANGE[2])

      cv <- lapply(PREPS, function(prep) {
        cv_pred <- matrix(NA_real_, n_train, PLS_NCOMP)
        for (f in seq_len(EXP_CONFIG$cv_folds)) {
          hold <- fold_id == f
          fit  <- pls_fit(Xr[[prep]][!hold, , drop = FALSE], Yr[!hold])
          cv_pred[hold, ] <- pls_pred_all(fit, Xr[[prep]][hold, , drop = FALSE])
        }
        rmse_nc <- sqrt(colMeans((cv_pred - Yr)^2))
        w       <- 1 / rmse_nc[wa_idx]^2; w <- w / sum(w)
        rmse_wa <- sqrt(mean((as.vector(cv_pred[, wa_idx] %*% w) - Yr)^2))
        msg("[pivot] %s: P %-11s CV rmse by ncomp (transformed scale): %s | wapls %.4f",
            property, prep, paste(round(rmse_nc, 4), collapse = " "), rmse_wa)
        list(prep = prep, rmse_nc = rmse_nc, best_nc = which.min(rmse_nc), w = w, rmse_wa = rmse_wa)
      })
      names(cv) <- PREPS

      ## Preprocessing chosen per row by CV RMSE, then one final fit per prep needed.
      prep_pls   <- PREPS[which.min(vapply(cv, function(z) min(z$rmse_nc), numeric(1)))]
      prep_wapls <- PREPS[which.min(vapply(cv, function(z) z$rmse_wa,      numeric(1)))]
      fits <- list()
      for (prep in unique(c(prep_pls, prep_wapls))) {
        fits[[prep]] <- pls_fit(Xr[[prep]], Yr)
      }

      predict_arm <- function(prep, X, ids, how) {
        P <- pls_pred_all(fits[[prep]], X[[prep]])
        p <- if (how == "pls") P[, cv[[prep]]$best_nc] else as.vector(P[, wa_idx] %*% cv[[prep]]$w)
        finish(tibble(sample_id = ids, .pred_t = p), ids, y_of, tr)
      }

      s_pls   <- score(predict_arm(prep_pls,   Xt, ids_test, "pls"),   predict_arm(prep_pls,   Xc, ids_calib, "pls"))
      s_wapls <- score(predict_arm(prep_wapls, Xt, ids_test, "wapls"), predict_arm(prep_wapls, Xc, ids_calib, "wapls"))

    })

    row_pls   <- make_row(property, "P_pls",   prep_pls,   cv[[prep_pls]]$best_nc, n_train, n_test, s_pls$metrics,   t_p[["elapsed"]],
                          note = sprintf("ncomp %d of %d by 5-fold CV", cv[[prep_pls]]$best_nc, PLS_NCOMP))
    row_wapls <- make_row(property, "P_wapls", prep_wapls, PLS_NCOMP, n_train, n_test, s_wapls$metrics, t_p[["elapsed"]],
                          note = sprintf("weights 1/rmse^2 over ncomp %d-%d", P_WA_RANGE[1], P_WA_RANGE[2]))
    msg("[pivot] %s: P_pls   %s ncomp %d: fixed-test RPD %.3f RMSE %.3f CCC %.3f cov %.3f", property, prep_pls,
        cv[[prep_pls]]$best_nc, s_pls$metrics$rpd, s_pls$metrics$rmse, s_pls$metrics$ccc, s_pls$metrics$coverage)
    msg("[pivot] %s: P_wapls %s: fixed-test RPD %.3f RMSE %.3f CCC %.3f cov %.3f (P took %.1f min)", property, prep_wapls,
        s_wapls$metrics$rpd, s_wapls$metrics$rmse, s_wapls$metrics$ccc, s_wapls$metrics$coverage, t_p[["elapsed"]] / 60)

    if (!dry) {
      readr::write_csv(row_pls,   row_path(property, "P_pls"))
      readr::write_csv(row_wapls, row_path(property, "P_wapls"))
      qs2::qs_save(list(rows = bind_rows(row_pls, row_wapls), cv = cv,
                        test_preds = list(P_pls = s_pls$preds, P_wapls = s_wapls$preds)),
                   file.path(PIVOT_CKPT, sprintf("%s-P-nc%d.qs2", property, PLS_NCOMP)))
    } else {
      print(as.data.frame(bind_rows(row_pls, row_wapls)))
    }
    rm(fits); invisible(gc())
  }

  ## -------------------------------------------------------------------------
  ## Arm M: memory-based learning, arm D's settings at 2 cm-1
  ## -------------------------------------------------------------------------

  if (!"M" %in% ARMS) {
    msg("[pivot] %s: M not requested, skipping", property)
  } else if (!dry && file.exists(row_path(property, "M"))) {
    msg("[pivot] %s: M row exists, skipping", property)
  } else {

    t_m <- system.time({

      ## Stage 1: choose (prep, k) by NNv on one seeded chunk of unknowns.
      ## Checkpointed: stage 2 is where memory kills happen, and stage 1 is
      ## 25 minutes on oc that a rerun should not repeat.
      set.seed(SEED)
      nnv_ids <- sample(ids_test, min(NNV_CHUNK, n_test))
      nnv_ckpt <- file.path(PIVOT_CKPT, paste0(property, "-M-nnv.qs2"))
      nnv <- list()
      if (!dry && file.exists(nnv_ckpt)) {
        saved <- qs2::qs_read(nnv_ckpt)
        nnv_all <- saved$nnv; msg("[pivot] %s: M NNv loaded from checkpoint", property)
      } else for (prep in PREPS) {
        t1 <- system.time(
          m <- mbl(Xr = Xr[[prep]], Yr = Yr, Xu = Xt[[prep]][match(nnv_ids, ids_test), , drop = FALSE],
                   neighbors   = neighbors_k(MBL_K[MBL_K < n_train]),
                   diss_method = diss_pca(ncomp = ncomp_by_opc(40L)),
                   fit_method  = fit_wapls(min_ncomp = WAPLS_RANGE[1], max_ncomp = WAPLS_RANGE[2]),
                   control     = mbl_control(validation_type = "NNv", allow_parallel = FALSE, blas_threads = 1L),
                   verbose = FALSE, seed = SEED)
        )
        tab <- nnv_table(m); tab$prep <- prep
        nnv[[prep]] <- tab
        msg("[pivot] %s: M NNv on %d unknowns, prep = %s: %.1f min", property, length(nnv_ids), prep, t1[["elapsed"]] / 60)
        rm(m); invisible(gc())
      }
      if (length(nnv)) nnv_all <- bind_rows(nnv)
      kcol <- intersect(c("k", "k_neighbors", "neighbors"), names(nnv_all))[1]
      rcol <- intersect(c("rmse", "rmse_nnv", "RMSE"), names(nnv_all))[1]
      if (is.na(kcol) || is.na(rcol)) stop("Cannot find k / rmse columns in NNv table: ", paste(names(nnv_all), collapse = ", "))
      print(as.data.frame(nnv_all[, c("prep", kcol, rcol)]), row.names = FALSE)
      best <- nnv_all[which.min(nnv_all[[rcol]]), ]
      best_prep <- best$prep; best_k <- as.integer(best[[kcol]])
      msg("[pivot] %s: M NNv winner prep = %s, k = %d (NNv rmse %.4f, transformed scale)", property, best_prep, best_k, best[[rcol]])
      if (!dry && !file.exists(nnv_ckpt)) qs2::qs_save(list(nnv = nnv_all, nnv_ids = nnv_ids), nnv_ckpt)

      ## Stage 2: predict every test and calib row at the winner, chunked.
      t2 <- system.time({
        test_p  <- mbl_chunked(Xr[[best_prep]], Yr, Xt[[best_prep]], ids_test,  best_k, workers)
        calib_p <- mbl_chunked(Xr[[best_prep]], Yr, Xc[[best_prep]], ids_calib, best_k, workers)
      })
      msg("[pivot] %s: M predicted %d test + %d calib rows on %d workers in %.1f min", property,
          nrow(test_p), nrow(calib_p), workers, t2[["elapsed"]] / 60)
      stopifnot(identical(test_p$sample_id, ids_test), identical(calib_p$sample_id, ids_calib))

      s_m <- score(finish(test_p, ids_test, y_of, tr), finish(calib_p, ids_calib, y_of, tr))

    })

    row_m <- make_row(property, "M", best_prep, best_k, n_train, n_test, s_m$metrics, t_m[["elapsed"]],
                      note = sprintf("mbl wapls %d-%d, diss_pca opc40, k by NNv on %d unknowns over k = %s", WAPLS_RANGE[1], WAPLS_RANGE[2], length(nnv_ids), paste(MBL_K, collapse = "/")))
    msg("[pivot] %s: M %s k %d: fixed-test RPD %.3f RMSE %.3f CCC %.3f cov %.3f (M took %.1f min)", property, best_prep, best_k,
        s_m$metrics$rpd, s_m$metrics$rmse, s_m$metrics$ccc, s_m$metrics$coverage, t_m[["elapsed"]] / 60)

    if (!dry) {
      readr::write_csv(row_m, row_path(property, "M"))
      qs2::qs_save(list(row = row_m, nnv = nnv_all, best = list(prep = best_prep, k = best_k), test_preds = s_m$preds),
                   file.path(PIVOT_CKPT, paste0(property, "-M.qs2")))
    } else {
      print(as.data.frame(row_m))
    }
  }

  rm(Xr, Xc, Xt); invisible(gc())
}

## ---------------------------------------------------------------------------
## Summary with the reference rows, so the table reads on its own
## ---------------------------------------------------------------------------

if (!dry) {

  rows <- bind_rows(lapply(list.files(PIVOT_DIR, "^row-.*\\.csv$", full.names = TRUE), readr::read_csv, show_col_types = FALSE))

  refs <- list()
  lc_path <- file.path(this_dir, "results", "learning-curve", "learning-curve.csv")
  if (file.exists(lc_path)) {
    lc <- readr::read_csv(lc_path, show_col_types = FALSE)
    lc <- lc[lc$config == "cubist_snv_pca" & lc$n_train == max(lc$n_train), ]
    refs$lc <- tibble(property = lc$property, arm = "ref_cubist_snv_pca_2cm_today", preprocessing = "snv",
                      k_or_ncomp = NA_integer_, resolution_cm = 2L, n_train = lc$n_train, n_test = lc$n_test_fixed,
                      rpd = lc$test_rpd, rmse = lc$test_rmse, ccc = lc$test_ccc, rsq = lc$test_rsq, bias = lc$test_bias,
                      coverage_90 = NA_real_, mean_width = NA_real_, secs = NA_real_,
                      note = "learning curve, grid 5, no UQ", pilot = FALSE, ran_at = as.character(lc$ran_at))
  }
  e1_path <- file.path(exp1_dir, "results", "results.csv")
  if (file.exists(e1_path)) {
    e1 <- readr::read_csv(e1_path, show_col_types = FALSE)
    e1 <- e1[e1$strategy %in% c("A_global", "D_mbl") & e1$property %in% props, ]
    refs$e1 <- tibble(property = e1$property, arm = paste0("ref_exp1_", e1$strategy, "_4cm"), preprocessing = e1$preprocessing,
                      k_or_ncomp = NA_integer_, resolution_cm = 4L, n_train = e1$n_train, n_test = e1$n_test,
                      rpd = e1$rpd, rmse = e1$rmse, ccc = e1$ccc, rsq = e1$rsq, bias = e1$bias,
                      coverage_90 = e1$coverage_90, mean_width = e1$mean_width, secs = NA_real_,
                      note = e1$config, pilot = FALSE, ran_at = NA_character_)
  }

  rows$ran_at <- as.character(rows$ran_at)
  summary <- bind_rows(rows, bind_rows(refs)) |>
    select(where(function(x) !is.list(x))) |>
    arrange(property, arm)
  readr::write_csv(summary, file.path(PIVOT_DIR, "pivot.csv"))
  print(as.data.frame(summary[, c("property", "arm", "preprocessing", "k_or_ncomp", "resolution_cm", "rpd", "rmse", "ccc", "coverage_90", "mean_width")]),
        row.names = FALSE, digits = 4)
  msg("[pivot] all done; summary at %s", file.path(PIVOT_DIR, "pivot.csv"))

}
