## ===========================================================================
## pm-helpers.R — shared by 03-pivot-pm.R and 04-locality-curve.R
## ===========================================================================
##
## Sourced AFTER 00-config.R, horizons, resemble, helpers.R, and after these
## globals exist in the calling script: snap, PLS_NCOMP, WAPLS_RANGE, SEED,
## PIVOT_DIR, dry. The first block is verbatim from experiment 1's 06-D-mbl.R
## so arm M is arm D at full resolution and nothing else changes.

## ---------------------------------------------------------------------------
## Verbatim from 06-D-mbl.R: preprocessing, response transform, mbl readers
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

nnv_table <- function(m) {
  v <- m$validation_results
  if (is.null(v)) return(NULL)
  nn <- v$nearest_neighbor_validation %||% v[[1]]
  as_tibble(as.data.frame(nn))
}

pred_at_k <- function(m, k, ids) {
  gp <- as.data.frame(get_predictions(m))
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

## ---------------------------------------------------------------------------
## Pivot helpers
## ---------------------------------------------------------------------------

## Predictions on the transformed scale -> scored tibble with conformal bounds.
finish <- function(p, ids, y_of, tr) {
  p$.pred       <- inverse_y(p$.pred_t, tr)
  p$truth       <- y_of(ids)
  p$base_lower  <- p$.pred
  p$base_upper  <- p$.pred
  p$.pred_lower <- NA_real_; p$.pred_upper <- NA_real_
  p
}

score <- function(test_p, calib_p) {
  test_p <- conformalize(test_p, calib_p)
  list(preds = test_p, metrics = metrics_row(test_p$truth, test_p$.pred, test_p$lower, test_p$upper))
}

row_path <- function(property, arm) {
  tag <- if (startsWith(arm, "P_")) sprintf("%s-nc%d", arm, PLS_NCOMP) else arm
  file.path(PIVOT_DIR, sprintf("row-%s-%s.csv", property, tag))
}

make_row <- function(property, arm, prep, k_or_ncomp, n_train, n_test, m, secs, note = NA_character_) {
  tibble(
    property = property, arm = arm, preprocessing = prep, k_or_ncomp = as.integer(k_or_ncomp),
    resolution_cm = 2L, n_train = n_train, n_test = n_test,
    rpd = m$rpd, rmse = m$rmse, ccc = m$ccc, rsq = m$rsq, bias = m$bias,
    coverage_90 = m$coverage, mean_width = m$mean_width,
    secs = secs, note = note, pilot = dry,
    ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

## Global PLS: fit once at PLS_NCOMP components, predict every component.
## mixOmics is already a horizons dependency (the plsr engine).
pls_fit <- function(X, y) {
  mixOmics::pls(X, matrix(y, ncol = 1, dimnames = list(NULL, "y")),
                ncomp = PLS_NCOMP, mode = "regression", scale = FALSE)
}
pls_pred_all <- function(fit, X) {
  p <- predict(fit, X)$predict               # n x 1 x ncomp
  matrix(p[, 1, ], nrow = nrow(X), ncol = PLS_NCOMP)
}

## Chunk unknowns across callr workers; each chunk is an independent mbl()
## call on the same library, so this is exact.
## The dissimilarity is computed ONCE here, in the parent, and each worker gets
## its slice. Left to mbl(), every call re-projects the whole library and
## re-runs the optimised-component search, which on the 31k-row oc library is
## a multi-gigabyte transient per worker: three workers took the box from
## 46 GB free to the 15 GB watchdog floor in under a minute, twice
## (2026-09-16). A precomputed matrix reproduces mbl()'s own diss_pca path to
## the bit (verified) and costs nrow(Xr) x nrow(Xu) doubles once.
## Chunk SIZE is fixed (rows of unknowns per mbl() call) and independent of the
## worker count; workers then hold the library, one slice, and local fits.
mbl_chunked <- function(Xr, Yr, Xu, ids, k, chunks = NULL, chunk_size = 500L) {
  t_d <- system.time(
    D <- resemble::dissimilarity(Xr, Xu, diss_method = resemble::diss_pca(ncomp = resemble::ncomp_by_opc(40L)), Yr = Yr)
  )
  msg("[mbl] dissimilarity %d x %d in %.1f min (opc chose %s components)", nrow(D$dissimilarity), ncol(D$dissimilarity),
      t_d[["elapsed"]] / 60, paste(D$ncomp, collapse = ","))
  D <- D$dissimilarity
  ## Each future gets ONLY its slice, carried in the element it maps over: the
  ## full matrix (2.3 GB on oc) is above R's 2 GB serialization limit and a
  ## global would be shipped whole to every future (FutureLaunchError,
  ## 2026-09-16 18:20).
  idx    <- split(seq_len(nrow(Xu)), ceiling(seq_len(nrow(Xu)) / chunk_size))
  slices <- lapply(idx, function(i) list(i = i, D = D[, i, drop = FALSE], Xu = Xu[i, , drop = FALSE], ids = ids[i]))
  rm(D); invisible(gc())
  old <- options(future.globals.maxSize = 2 * 1024^3); on.exit(options(old), add = TRUE)
  out <- future.apply::future_lapply(slices, function(s) {
    m <- resemble::mbl(Xr = Xr, Yr = Yr, Xu = s$Xu,
                       neighbors   = resemble::neighbors_k(k),
                       diss_method = s$D,
                       fit_method  = resemble::fit_wapls(min_ncomp = WAPLS_RANGE[1], max_ncomp = WAPLS_RANGE[2]),
                       control     = resemble::mbl_control(validation_type = "none", allow_parallel = FALSE, blas_threads = 1L),
                       verbose = FALSE, seed = SEED)
    pred_at_k(m, k, s$ids)
  }, future.seed = TRUE, future.globals = list(Xr = Xr, Yr = Yr, k = k,
                                              WAPLS_RANGE = WAPLS_RANGE, SEED = SEED,
                                              pred_at_k = pred_at_k),
     future.packages = c("resemble", "tibble"), future.scheduling = Inf)
  bind_rows(out)
}

