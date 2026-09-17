## ===========================================================================
## 04 — The locality curve: how coarse can the neighbourhood be?
## ===========================================================================
##
## Purpose (2026-09-16). The pivot (03) showed the local model beating the
## best global PLS by 1.25 RPD on clay, and its neighbour-validation curve was
## still improving at k = 400, the top of arm D's grid. This script walks k
## from 50 to 12,800 with the learner held fixed (weighted-average PLS,
## 5-20 components, as arm D and arm M) and scores every k on the same
## unknowns. The endpoints are already known: k = library is the global
## weighted-average PLS at 20 components (P_wapls, 2.79 on clay), and the
## global PLS at 120 components (3.38) is the best a global fit does.
##
## The curve answers "how coarse can locality be" directly, without building
## the clustering arms: a cluster of size c is at best a fixed neighbourhood
## of size c chosen once for many unknowns, so the k-curve is an upper bound
## on what any clustering of that granularity can do with this learner.
##
## One mbl() call per chunk returns predictions for every k at once. The
## unknowns are a seeded 1,000-row sample of the fixed test split (the curve's
## shape needs less precision than the pivot's point estimate), scored on the
## original scale. No intervals; this is a shape question.
##
## Run (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_locality.sh [clay oc ph] [--workers=N] > /dev/null 2>&1 &
##   Rscript dev/experiments/2026-09-factorial/04-locality-curve.R [clay oc ph] [--workers=N] [--n=1000] [--dry]
##
## Resumable: a property whose curve CSV exists is skipped.

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading

args     <- commandArgs(trailingOnly = TRUE)
flags    <- args[startsWith(args, "--")]
props    <- setdiff(args, flags)
dry      <- "--dry" %in% flags
w_arg    <- sub("^--workers=", "", grep("^--workers=", flags, value = TRUE))
n_arg    <- sub("^--n=",       "", grep("^--n=",       flags, value = TRUE))
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
workers <- if (dry) 2L else if (length(w_arg)) as.integer(w_arg) else 4L
N_UNKNOWN <- if (dry) 40L else if (length(n_arg)) as.integer(n_arg) else 1000L
CURVE_SEED <- SEED + 1L              # not the NNv chunk 03 used; an independent sample

PREP        <- "snv_deriv1"          # the pivot's NNv winner on clay
K_GRID      <- c(50L, 100L, 200L, 400L, 800L, 1600L, 3200L, 6400L, 12800L)
WAPLS_RANGE <- EXP_MBL$pls_c         # c(5, 20), as arm D and arm M
PLS_NCOMP   <- 20L                   # pm-helpers expects it; unused here
PIVOT_DIR   <- file.path(this_dir, "results", "pivot")
CURVE_DIR   <- file.path(this_dir, "results", "locality-curve")
CURVE_CKPT  <- file.path(CURVE_DIR, "checkpoints")
for (d in c(CURVE_DIR, CURVE_CKPT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

snap <- load_snapshot()
source(file.path(this_dir, "pm-helpers.R"))   # prep_matrix, forward_y, inverse_y, pred_at_k

msg("[curve] snapshot %d samples; properties: %s; workers %d; %d unknowns; k = %s; dry = %s",
    nrow(snap$lab), paste(props, collapse = ", "), workers, N_UNKNOWN, paste(K_GRID, collapse = "/"), dry)

## Predictions for every k in one mbl object, long form.
preds_all_k <- function(m, ks, ids) {
  bind_rows(lapply(ks, function(k) {
    p <- pred_at_k(m, k, ids); p$k <- k; p
  }))
}

## One mbl() call per chunk with the whole k grid; each chunk is independent.
curve_chunked <- function(Xr, Yr, Xu, ids, ks, chunks) {
  ks  <- ks[ks < nrow(Xr)]                 # a neighbourhood cannot exceed the library
  ## Dissimilarity once, in the parent (see mbl_chunked() in pm-helpers.R).
  D   <- resemble::dissimilarity(Xr, Xu, diss_method = resemble::diss_pca(ncomp = resemble::ncomp_by_opc(40L)), Yr = Yr)$dissimilarity
  idx    <- split(seq_len(nrow(Xu)), cut(seq_len(nrow(Xu)), chunks, labels = FALSE))
  slices <- lapply(idx, function(i) list(D = D[, i, drop = FALSE], Xu = Xu[i, , drop = FALSE], ids = ids[i]))
  rm(D); invisible(gc())
  old <- options(future.globals.maxSize = 2 * 1024^3); on.exit(options(old), add = TRUE)
  out <- future.apply::future_lapply(slices, function(s) {
    m <- resemble::mbl(Xr = Xr, Yr = Yr, Xu = s$Xu,
                       neighbors   = resemble::neighbors_k(ks),
                       diss_method = s$D,
                       fit_method  = resemble::fit_wapls(min_ncomp = WAPLS_RANGE[1], max_ncomp = WAPLS_RANGE[2]),
                       control     = resemble::mbl_control(validation_type = "none", allow_parallel = FALSE, blas_threads = 1L),
                       verbose = FALSE, seed = SEED)
    preds_all_k(m, ks, s$ids)
  }, future.seed = TRUE, future.globals = list(Xr = Xr, Yr = Yr, ks = ks,
                                              WAPLS_RANGE = WAPLS_RANGE, SEED = SEED,
                                              pred_at_k = pred_at_k, preds_all_k = preds_all_k),
     future.packages = c("resemble", "tibble", "dplyr"))
  bind_rows(out)
}

## ---------------------------------------------------------------------------
## Main
## ---------------------------------------------------------------------------

future::plan(future::multicore, workers = workers)   # forked: library shared copy-on-write (see 03)
on.exit(future::plan(future::sequential), add = TRUE)

for (property in props) {

  out_csv <- file.path(CURVE_DIR, paste0("curve-", property, ".csv"))
  if (!dry && file.exists(out_csv)) { msg("[curve] %s: curve exists, skipping", property); next }

  sp   <- load_splits(property); assert_splits(sp)
  spec <- property_spec(property)
  tr   <- spec$transformation

  ids_train <- sp$train_core
  if (dry) { set.seed(SEED); ids_train <- sample(ids_train, 800L) }
  set.seed(CURVE_SEED)
  ids_u <- sample(sp$test, min(N_UNKNOWN, length(sp$test)))

  y_of <- function(ids) snap$lab[[property]][match(ids, snap$lab$sample_id)]
  Yr   <- forward_y(y_of(ids_train), tr)

  msg("[curve] %s: library %d rows, %d unknowns from the fixed test split, prep %s", property, length(ids_train), length(ids_u), PREP)

  t_all <- system.time({
    Xr <- prep_matrix(ids_train, PREP)
    Xu <- prep_matrix(ids_u, PREP)
    preds <- curve_chunked(Xr, Yr, Xu, ids_u, K_GRID, workers)
  })

  preds$.pred <- inverse_y(preds$.pred_t, tr)
  preds$truth <- y_of(preds$sample_id)

  curve <- preds |>
    group_by(k) |>
    group_modify(~ metrics_row(.x$truth, .x$.pred)) |>
    ungroup() |>
    mutate(property = property, preprocessing = PREP, n_library = length(ids_train),
           n_unknown = length(ids_u), wapls = paste(WAPLS_RANGE, collapse = "-"),
           resolution_cm = 2L, secs_total = t_all[["elapsed"]], pilot = dry,
           ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")) |>
    arrange(k)

  ## Endpoints from the pivot, if it ran: the global fits are k = library.
  ref <- list.files(PIVOT_DIR, sprintf("^row-%s-P_.*\\.csv$", property), full.names = TRUE)
  if (length(ref)) {
    rows <- bind_rows(lapply(ref, readr::read_csv, show_col_types = FALSE))
    msg("[curve] %s: global reference rows (whole test split): %s", property,
        paste(sprintf("%s nc%s %.3f", rows$arm, rows$k_or_ncomp, rows$rpd), collapse = "; "))
  }

  print(as.data.frame(curve[, c("k", "n_scored", "rpd", "rmse", "ccc", "rsq", "bias")]), row.names = FALSE, digits = 4)
  msg("[curve] %s: %d unknowns x %d k values in %.1f min on %d workers", property, length(ids_u), length(unique(preds$k)),
      t_all[["elapsed"]] / 60, workers)

  if (!dry) {
    readr::write_csv(curve, out_csv)
    qs2::qs_save(list(curve = curve, preds = preds), file.path(CURVE_CKPT, paste0(property, ".qs2")))
  }
  rm(Xr, Xu, preds); invisible(gc())
}

if (!dry) {
  all <- bind_rows(lapply(list.files(CURVE_DIR, "^curve-.*\\.csv$", full.names = TRUE), readr::read_csv, show_col_types = FALSE))
  readr::write_csv(all, file.path(CURVE_DIR, "locality-curve.csv"))
  msg("[curve] all done; summary at %s", file.path(CURVE_DIR, "locality-curve.csv"))
}
