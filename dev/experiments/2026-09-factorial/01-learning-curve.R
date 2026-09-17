## ===========================================================================
## 01 — Learning curve: performance against training-set size on clay
## ===========================================================================
##
## Purpose (2026-09-16). The 2 cm-1 shakedown is memory-bound and the memory
## scales with the fold matrix, i.e. with n. Before treating that as an
## infrastructure problem, measure whether accuracy actually needs the full
## 17,788-row clay training set. If the curve is flat by a quarter of the
## rows, n is the lever and the memory ceiling dissolves at full spectral
## resolution.
##
## Design. Nested subsamples of experiment 1's clay train_core (each smaller
## set is a subset of the next larger one, one seed), so size is the only
## thing that changes between points. For each config and size:
##   build_hz -> evaluate() (5-fold CV, grid 5, resamples axis on callr)
##   -> fit() (no UQ, no AD) -> predict on experiment 1's FIXED test split.
## evaluate()'s own cv_* and test metrics are recorded too, but the fixed
## test set is the comparable y-axis across sizes. Two configs: plsr (the
## global arm, cheap) and cubist_snv (the winning learner family on clay).
##
## Caveat, on the record: uniform thinning keeps the distribution's shape and
## thins its density. A plateau here is necessary but not sufficient for the
## locality arms, which care about density. This curve answers the global
## question only.
##
## Run (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_learning_curve.sh > /dev/null 2>&1 &
##   Rscript dev/experiments/2026-09-factorial/01-learning-curve.R [--workers=N]
##
## Resumable: a (config, size) whose row CSV exists is skipped.

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading

args     <- commandArgs(trailingOnly = TRUE)
w_arg    <- sub("^--workers=", "", grep("^--workers=", args, value = TRUE))
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
exp1_dir <- file.path(dirname(this_dir), "2026-09-local-strategy")

source(file.path(exp1_dir, "00-config.R"))
suppressPackageStartupMessages(library(horizons))
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)

## ---------------------------------------------------------------------------
## Settings
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- NULL          # full resolution: std() -> resample = 2
PROPERTY  <- "clay"
FRACTIONS <- c(1/32, 1/16, 1/8, 1/4, 1/2, 1)
LC_SEED   <- 20260916L

LC_SET <- data.frame(model             = c("plsr", "cubist"),
                     preprocessing     = c("snv_deriv1", "snv"),
                     feature_selection = c("pca", "pca"),
                     stringsAsFactors  = FALSE)

LC_DIR  <- file.path(this_dir, "results", "learning-curve")
LC_CKPT <- file.path(LC_DIR, "checkpoints")
for (d in c(LC_DIR, LC_CKPT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

## One config per evaluate() call, so parallelize_over = "auto" resolves to
## the resamples axis: workers = cv_folds is the natural count.
workers <- if (length(w_arg)) as.integer(w_arg) else EXP_CONFIG$cv_folds
stopifnot(workers >= 1L, workers <= MAX_WORKERS)
future::plan(future.callr::callr, workers = workers)
on.exit(future::plan(future::sequential), add = TRUE)

mem_avail_gb <- function() {
  as.numeric(sub(".*:\\s*(\\d+).*", "\\1", grep("MemAvailable", readLines("/proc/meminfo"), value = TRUE))) / 1024^2
}

## ---------------------------------------------------------------------------
## Data and nested subsamples
## ---------------------------------------------------------------------------

snap <- load_snapshot()
sp   <- load_splits(PROPERTY); assert_splits(sp)
spec <- property_spec(PROPERTY)

set.seed(LC_SEED)
train_perm <- sample(sp$train_core)             # one permutation; prefixes nest
sizes      <- sort(unique(round(FRACTIONS * length(train_perm))))

msg("[lc] snapshot %d samples; %s train_core %d, fixed test %d; sizes: %s; plan callr(%d)",
    nrow(snap$lab), PROPERTY, length(train_perm), length(sp$test),
    paste(sizes, collapse = ", "), workers)

row_path <- function(cfg_label, n) file.path(LC_DIR, sprintf("row-%s-%05d.csv", cfg_label, n))

## ---------------------------------------------------------------------------
## Main loop: config x size
## ---------------------------------------------------------------------------

for (i in seq_len(nrow(LC_SET))) {

  set       <- LC_SET[i, , drop = FALSE]
  cfg_label <- paste(set$model, set$preprocessing, set$feature_selection, sep = "_")

  for (n in sizes) {

    if (file.exists(row_path(cfg_label, n))) {
      msg("[lc] %s n=%d: row exists, skipping", cfg_label, n)
      next
    }

    ids <- train_perm[seq_len(n)]
    msg("[lc] %s n=%d: building; MemAvailable %.1f GB", cfg_label, n, mem_avail_gb())

    t_build <- system.time(
      hz <- build_hz(snap, ids, PROPERTY, spec$transformation, set = set)
    )

    eval_dir <- file.path(LC_CKPT, sprintf("%s-%05d-eval", cfg_label, n))
    mem_before <- mem_avail_gb()
    t_eval <- system.time(
      hz <- eval_exp(hz, eval_dir, verbose = FALSE)
    )
    res <- hz$evaluation$results
    msg("[lc] %s n=%d: evaluate() %.1f s over %s on %d worker(s); status %s; cv_rpd %.3f; MemAvailable %.1f -> %.1f GB",
        cfg_label, n, t_eval[["elapsed"]], hz$evaluation$parallelize_over, hz$evaluation$workers,
        paste(res$status, collapse = ","), max(res$cv_rpd, na.rm = TRUE), mem_before, mem_avail_gb())

    t_fit <- system.time(
      fit_obj <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                     allow_par = workers > 1L, seed = SEED, verbose = FALSE)
    )

    ## Fixed test split: the same 5,239 samples at every size.
    t_pred <- system.time(
      pred <- predict_ids(fit_obj, snap, sp$test, PROPERTY)
    )
    test_m <- metrics_row(pred$truth, pred$.pred)

    row <- tibble(
      property       = PROPERTY,
      config         = cfg_label,
      config_id      = res$config_id[1],
      n_train        = n,
      frac_train     = n / length(train_perm),
      n_test_fixed   = length(sp$test),
      resolution_cm  = 2L,
      plan_workers   = workers,
      axis           = hz$evaluation$parallelize_over,
      status         = res$status[1],
      cv_rpd         = res$cv_rpd[1],
      cv_rmse        = res$cv_rmse[1],
      eval_rpd       = if ("rpd"  %in% names(res)) res$rpd[1]  else NA_real_,
      eval_rmse      = if ("rmse" %in% names(res)) res$rmse[1] else NA_real_,
      test_rpd       = test_m$rpd,
      test_rmse      = test_m$rmse,
      test_ccc       = test_m$ccc,
      test_rsq       = test_m$rsq,
      test_bias      = test_m$bias,
      test_n_scored  = test_m$n_scored,
      secs_build     = t_build[["elapsed"]],
      secs_evaluate  = t_eval[["elapsed"]],
      secs_fit       = t_fit[["elapsed"]],
      secs_predict   = t_pred[["elapsed"]],
      mem_avail_gb_min = mem_avail_gb(),
      horizons_sha   = tryCatch(system2("git", c("-C", shQuote(PKG_DIR), "rev-parse", "--short", "HEAD"),
                                        stdout = TRUE), error = function(e) NA_character_),
      ran_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )

    msg("[lc] %s n=%d: fit %.1f s; fixed-test RPD %.3f, RMSE %.3f, CCC %.3f; total %.1f min",
        cfg_label, n, t_fit[["elapsed"]], test_m$rpd, test_m$rmse, test_m$ccc,
        (t_build[["elapsed"]] + t_eval[["elapsed"]] + t_fit[["elapsed"]] + t_pred[["elapsed"]]) / 60)

    readr::write_csv(row, row_path(cfg_label, n))
    qs2::qs_save(list(row = row, eval_results = res, pred = pred),
                 file.path(LC_CKPT, sprintf("%s-%05d.qs2", cfg_label, n)))
    rm(fit_obj, hz, pred); invisible(gc())

  }

}

rows <- do.call(rbind, lapply(list.files(LC_DIR, "^row-.*\\.csv$", full.names = TRUE),
                              readr::read_csv, show_col_types = FALSE))
rows <- rows[order(rows$config, rows$n_train), ]
readr::write_csv(rows, file.path(LC_DIR, "learning-curve.csv"))
print(as.data.frame(rows[, c("config", "n_train", "cv_rpd", "cv_rmse", "test_rpd", "test_rmse",
                             "test_ccc", "secs_evaluate")]), row.names = FALSE)
msg("[lc] all done; summary at %s", file.path(LC_DIR, "learning-curve.csv"))
