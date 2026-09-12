## ===========================================================================
## 03 — Strategy A: one global model per property (the baseline)
## ===========================================================================
##
## For each property: train_core -> the horizons chain with the full config
## set -> evaluate() picks the winner -> fit(n_best = 1) with UQ + AD ->
## predict test and calib_ext -> shared conformal wrapper -> metrics row.
## The fitted object is checkpointed because B and E reuse the winning config
## and the global model (fallback + per-cluster comparison).
##
## Run: Rscript dev/experiments/2026-09-local-strategy/03-A-global.R [clay oc ph] [--pilot] [--workers=N] [--force]
##   --pilot   : both learners, snv only (2 configs), default 10 workers; writes
##               the "A_pilot" checkpoint. Used to time one fold before launch.

Sys.setenv(HORIZONS_THREAD_CONTROL = "TRUE")   # must precede horizons loading
## evaluate()'s nested multisession plan (outer configs x inner folds) trips
## parallelly's maxWorkers.localhost hard limit, because future sets
## mc.cores = 1 inside each outer worker (DOGFOOD #10). The env var form is
## inherited by the workers; the option covers this process.
Sys.setenv(R_PARALLELLY_MAXWORKERS_LOCALHOST = "Inf")
options(parallelly.maxWorkers.localhost = Inf,
        future.globals.maxSize = 8 * 1024^3)

args     <- commandArgs(trailingOnly = TRUE)
flags    <- args[startsWith(args, "--")]
props    <- setdiff(args, flags)
pilot    <- "--pilot" %in% flags
force    <- "--force" %in% flags
w_arg    <- sub("^--workers=", "", grep("^--workers=", flags, value = TRUE))
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
exp_dir  <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()

source(file.path(exp_dir, "00-config.R"))
suppressPackageStartupMessages(devtools::load_all(PKG_DIR, quiet = TRUE))
source(file.path(exp_dir, "helpers.R"))
exp_dirs()

if (!length(props)) props <- EXP_PROPERTIES$property
workers  <- min(if (length(w_arg)) as.integer(w_arg) else MAX_WORKERS, MAX_WORKERS)
strategy <- if (pilot) "A_pilot" else "A_global"

## Parallelism (DOGFOOD #10, #12, #13): evaluate()'s own nested plan breaks at
## this scale, and its workers <= cv_folds branch never registers a plan, so
## we register one here and call evaluate(workers = cv_folds). That takes the
## sequential-over-configs branch with allow_par = TRUE, and tune then runs
## folds x grid (25 tasks) on our plan. fit(allow_par = TRUE) uses the same
## plan. Per-task globals are one rsplit + workflow, well under the 2 GB
## serialization limit.
future::plan(future::multisession, workers = workers)
on.exit(future::plan(future::sequential), add = TRUE)
EVAL_WORKERS <- EXP_CONFIG$cv_folds

snap <- load_snapshot()
msg("[%s] snapshot %d samples; properties: %s; workers %d",
    strategy, nrow(snap$lab), paste(props, collapse = ", "), workers)

for (property in props) {

  if (has_ckpt(property, strategy) && !force) {
    msg("[%s] %s: checkpoint exists, skipping (use --force).", strategy, property)
    next
  }

  sp   <- load_splits(property); assert_splits(sp)
  spec <- property_spec(property)
  ## Pilot: the snv half of the config set (one Cubist + one rf), to time it.
  set  <- if (pilot) EXP_CONFIG_SET[EXP_CONFIG_SET$preprocessing == "snv", ] else EXP_CONFIG_SET

  ## -------------------------------------------------------------------------
  ## Train: build -> evaluate -> fit
  ## -------------------------------------------------------------------------

  msg("[%s] %s: building train_core object (%d rows, transformation = %s, %d configs)",
      strategy, property, length(sp$train_core), spec$transformation, nrow(set))
  t_build <- system.time(
    hz <- build_hz(snap, sp$train_core, property, spec$transformation, set = set)
  )

  eval_dir <- file.path(CHECKPOINT_DIR, paste0(property, "-", strategy, "-eval"))
  msg("[%s] %s: evaluate() over %d configs; plan = multisession(%d), evaluate(workers = %d)",
      strategy, property, nrow(hz$config$configs), workers, EVAL_WORKERS)
  t_eval <- system.time(
    hz <- eval_exp(hz, eval_dir, workers = EVAL_WORKERS)   # prune = FALSE, see helpers.R
  )
  win <- winning_config(hz)
  msg("[%s] %s: evaluate() done in %.1f min; winner %s (%s + %s)", strategy, property,
      t_eval[["elapsed"]] / 60, win$config_id, win$model, win$preprocessing)
  print(as.data.frame(hz$evaluation$results))

  t_fit <- system.time(
    fit_obj <- fit(hz, n_best = 1L, compute_uq = TRUE, compute_ad = TRUE,
                   allow_par = workers > 1L, seed = SEED)
  )
  msg("[%s] %s: fit() done in %.1f min; UQ %s, AD %s", strategy, property,
      t_fit[["elapsed"]] / 60, !is.null(fit_obj$models$uq), !is.null(fit_obj$models$ad))

  ## -------------------------------------------------------------------------
  ## Predict: calib_ext (margin) and test (scored)
  ## -------------------------------------------------------------------------

  calib <- predict_ids(fit_obj, snap, sp$calib_ext, property)
  test  <- predict_ids(fit_obj, snap, sp$test, property)
  test  <- conformalize(test, calib)

  m_rep <- metrics_row(test$truth, test$.pred, test$lower, test$upper)
  m_nat <- metrics_row(test$truth, test$.pred, test$.pred_lower, test$.pred_upper)

  t_pred     <- time_predict_per_100(fit_obj, snap, sp$test, property)
  bytes_full <- artifact_bytes(fit_obj)
  bytes_pred <- artifact_bytes(strip_for_predict(fit_obj))

  row <- tibble(
    property = property, strategy = strategy,
    config = win$config_id, model = win$model, preprocessing = win$preprocessing,
    transformation = spec$transformation,
    n_train = length(sp$train_core), n_calib = length(sp$calib_ext), n_test = length(sp$test),
    rmse = m_rep$rmse, rpd = m_rep$rpd, ccc = m_rep$ccc, rsq = m_rep$rsq, bias = m_rep$bias,
    coverage_90 = m_rep$coverage, mean_width = m_rep$mean_width, c_alpha = attr(test, "c_alpha"),
    coverage_90_native = m_nat$coverage, mean_width_native = m_nat$mean_width,
    predict_secs_per_100 = t_pred,
    artifact_bytes = bytes_pred, artifact_bytes_full = bytes_full,
    k_clusters = NA_integer_, n_fallback = NA_integer_, n_pooled_clusters = NA_integer_,
    secs_build = t_build[["elapsed"]], secs_evaluate = t_eval[["elapsed"]], secs_fit = t_fit[["elapsed"]],
    split_sha = substr(sp$sha$test, 1, 12), resample = EXPERIMENT_RESAMPLE %||% 2,
    pilot = pilot, ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  print(as.data.frame(row))

  save_ckpt(list(results = row, eval_results = hz$evaluation$results, winner = win,
                 test_preds = test, calib_preds = calib, fit = fit_obj),
            property, strategy)
  write_results_row(row, property, strategy)
  msg("[%s] %s: done in %.1f min total.", strategy, property,
      (t_build[["elapsed"]] + t_eval[["elapsed"]] + t_fit[["elapsed"]]) / 60)
}
