## ===========================================================================
## 13 — Can the library be thinned? A selection competition judged by
##      noninferiority against the full core.
## ===========================================================================
##
## Purpose (2026-09-21). The hypothesis on the board is that the library
## CANNOT be thinned without losing accuracy. The evidence behind it is
## thinner than the claim: 01-learning-curve.R subsampled the clay core at
## random (556 to 17,788 rows, nested prefixes, one seed) and found no
## plateau for Cubist — each doubling still bought 0.3 to 0.4 RPD and the
## last doubling was the largest step — and experiment 1 cut one hard GMM
## partition. Random thinning keeps the distribution's shape and thins its
## density everywhere, which is the worst case for a learner that needs
## coverage; a design-driven selector is allowed to keep the shape and drop
## the redundancy. Nobody has tried one. That is what this script does.
##
## Design. One pool, four alternative selectors against random, three sizes,
## two learners, and a bar declared in overnight-config.R BEFORE the run.
##
##   pool       experiment 1's clay train_core (17,788 rows) MINUS the Iowa
##              fixture ids, so Iowa is a clean second test set.
##   test sets  (a) experiment 1's fixed clay test split (5,239 rows);
##              (b) the Iowa fixture (OVERNIGHT$iowa_n rows nearest Ames
##                  with clay measured and coordinates, built exactly as 10
##                  and 11 build it);
##              (c) tail bins of the test split, OVERNIGHT$tail_bins, taken
##                  as [lo, hi) on measured clay: low = [0, 10), high =
##                  [50, Inf). The tails are where a thinned library is most
##                  likely to fail, and where a global RMSE hides it.
##   references the full pool, one arm per learner ("full-rf", "full-cubist").
##   sizes      OVERNIGHT$sizes.
##   selectors  random (three seeds, reported separately and as a mean),
##              kennard_stone, duplex, kmedoids, stratified. Every selector
##              draws from the pool only; no selector ever sees test data.
##   learners   OVERNIGHT$learners, one config each: snv + pca, transformation
##              none, experiment 1's tuning (5 folds, 5-point grid, no
##              Bayesian iterations, seed 307). Arms differ only by which
##              rows they train on.
##
## The selection space is the shipping similarity space:
## build_similarity_space() on the pool's standardized spectra with
## OVERNIGHT$mask and OVERNIGHT$sdev_floor, and every selector works on
## sp$scores (n x ncomp), never on the raw spectra. Kennard-Stone and duplex
## on 17k x 30 scores hold a ~2.5 GB distance matrix, which the box has; on
## the raw 851-column matrix they would not.
##
## Verdict (pre-registered, overnight-config.R): a (selector, size, learner)
## passes if its test-split RMSE is within (1 + margin_rmse) of that
## learner's reference AND every tail-bin RMSE is within (1 + margin_tail).
## Iowa is reported as an off-split check, not used in the gate. For random
## the ratio is the mean over seeds; the max is printed alongside so a
## selector that passes on average and fails on a draw is visible.
##
## Also recorded once per thinned pool: its on-disk size as a qs2 file, so
## the OVERNIGHT$artifact_mb budget can be read against a real number
## (sizes.csv).
##
## Caveats on the record. (1) duplex partitions the pool into two sets of k,
## so it needs 2k <= n_pool; at size 12,000 on a ~17.5k pool it is
## impossible and the arm is skipped with a note rather than quietly
## redefined. (2) kmedoids is k-means on the scores followed by the pool row
## nearest each centroid; at 12,000 centres that is the most expensive
## selector in the design. (3) Kennard-Stone at 12,000 may be impractically
## slow even on scores; --ks-stage1=N runs it on a random N-row stage-one
## subsample and tops up at random, which is a documented fallback and is
## recorded in the row's note, not a silent substitution.
##
## Run (from package/, against the INSTALLED horizons):
##   Rscript dev/experiments/2026-09-factorial/13-thinning.R --dry --workers=2
##   nohup bash dev/experiments/2026-09-factorial/run_thinning.sh [--workers=N] > /dev/null 2>&1 &
##
## Resumable: an arm already in thinning.csv under this pilot flag is
## skipped. Pilot checkpoints live apart from real ones (a --dry run under
## the same tag would otherwise seed the real run's hyperparameters through
## evaluate()'s eval_checkpoint.rds; found 2026-09-21).
##
## Results: results/thinning/ — thinning.csv (one row per arm), verdict.csv,
## sizes.csv, fixture-iowa.csv, preds/*.csv, checkpoints/, logs/.
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
  if (!isNamespaceLoaded("horizons")) library(horizons)
  library(dplyr)
  library(tibble)
})
source(file.path(exp1_dir, "helpers.R"))
require_fresh_install(PKG_DIR)
for (p in c("workflows", "parsnip", "recipes", "ranger", "rules", "Cubist", "prospectr")) loadNamespace(p)

## ---------------------------------------------------------------------------
## Settings — every value that is not a flag comes from OVERNIGHT
## ---------------------------------------------------------------------------

EXPERIMENT_RESAMPLE <- OVERNIGHT$resample     # std() reads this from the global env

workers   <- if (dry) 2L else as.integer(flag_of("workers", "8"))
PROPERTY  <- OVERNIGHT$property
TR        <- OVERNIGHT$transformation
PREPROC   <- OVERNIGHT$preprocessing
FEATSEL   <- OVERNIGHT$feature_selection
TUNING    <- EXP_CONFIG
MASK      <- OVERNIGHT$mask
FLOOR     <- OVERNIGHT$sdev_floor
TAILS     <- OVERNIGHT$tail_bins

SIZES     <- if (dry) c(500L, 1000L)              else OVERNIGHT$sizes
SEEDS     <- if (dry) OVERNIGHT$random_seeds[1]   else OVERNIGHT$random_seeds
LEARNERS  <- if (dry) "rf"                        else OVERNIGHT$learners
SELECTORS <- OVERNIGHT$selectors
IOWA_N    <- if (dry) 60L                         else OVERNIGHT$iowa_n
POOL_DRY  <- 3000L
AMES      <- OVERNIGHT$ames
KS_STAGE1 <- as.integer(flag_of("ks-stage1", "0"))   # 0 = straight Kennard-Stone
STRAT_Q   <- 10L                                      # clay deciles x 10 spectral clusters

SITE_RAW <- file.path(RAW_DIR, "ossl_soilsite_L0_v1.2.csv.gz")
OUT      <- file.path(this_dir, "results", "thinning")
CKPT     <- file.path(OUT, if (dry) "checkpoints-pilot" else "checkpoints")
PREDS    <- file.path(OUT, "preds")
for (d in c(OUT, CKPT, PREDS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
rows_path    <- file.path(OUT, "thinning.csv")
sizes_path   <- file.path(OUT, "sizes.csv")
verdict_path <- file.path(OUT, "verdict.csv")

msg("[thin] selectors %s | sizes %s | seeds %s | learners %s | workers %d | dry %s",
    paste(SELECTORS, collapse = ","), paste(SIZES, collapse = ","),
    paste(SEEDS, collapse = ","), paste(LEARNERS, collapse = ","), workers, dry)

snap <- load_snapshot()
sp   <- load_splits(PROPERTY); assert_splits(sp)

## ---------------------------------------------------------------------------
## Fixtures: the Iowa batch (as 10 and 11 build it) and the fixed test split
## ---------------------------------------------------------------------------

iowa_fixture <- function(property, n) {
  ok   <- property_rows(snap$lab, property)$ids
  site <- data.table::fread(SITE_RAW, showProgress = FALSE,
                            select = c("id.layer_uuid_txt", "longitude.point_wgs84_dd", "latitude.point_wgs84_dd"))
  site <- site[site$id.layer_uuid_txt %in% ok & site$id.layer_uuid_txt %in% snap$spectra$sample_id &
               is.finite(site$longitude.point_wgs84_dd) & is.finite(site$latitude.point_wgs84_dd), ]
  site <- site[!duplicated(site$id.layer_uuid_txt), ]
  stopifnot(nrow(site) > n)
  to_rad <- pi / 180
  dlat <- (site$latitude.point_wgs84_dd  - AMES[["lat"]]) * to_rad
  dlon <- (site$longitude.point_wgs84_dd - AMES[["lon"]]) * to_rad
  a  <- sin(dlat / 2)^2 + cos(AMES[["lat"]] * to_rad) * cos(site$latitude.point_wgs84_dd * to_rad) * sin(dlon / 2)^2
  km <- 2 * 6371 * asin(pmin(1, sqrt(a)))
  o  <- order(km)[seq_len(n)]
  ids <- site$id.layer_uuid_txt[o]
  y   <- snap$lab[[property]][match(ids, snap$lab$sample_id)]
  msg("[iowa] fixture: %d rows nearest Ames within %.0f km; %s %.1f to %.1f, sd %.2f",
      length(ids), max(km[o]), property, min(y), max(y), stats::sd(y))
  write.csv(data.frame(sample_id = ids, km_from_ames = km[o],
                       lat = site$latitude.point_wgs84_dd[o], lon = site$longitude.point_wgs84_dd[o], y = y),
            file.path(OUT, "fixture-iowa.csv"), row.names = FALSE)
  list(hz = hz_spectra(snap, ids), ids = ids, y = y)
}

fx_iowa <- iowa_fixture(PROPERTY, IOWA_N)

## The Iowa fixture is drawn from the whole snapshot, so some of its rows can
## land in either experiment-1 split. Removing them from the pool is what
## keeps Iowa clean as a second test set; an overlap with the TEST split is
## not leakage but it does mean the two test sets share rows, so it is logged
## rather than silently tolerated.
n_in_train <- length(intersect(fx_iowa$ids, sp$train_core))
n_in_test  <- length(intersect(fx_iowa$ids, sp$test))
msg("[iowa] fixture overlaps train_core on %d row(s) (removed from the pool) and the test split on %d row(s)",
    n_in_train, n_in_test)

test_ids <- sp$test
fx_test  <- list(hz = hz_spectra(snap, test_ids), ids = test_ids,
                 y  = snap$lab[[PROPERTY]][match(test_ids, snap$lab$sample_id)])
msg("[test] fixed clay test split: %d rows; %s %.1f to %.1f, sd %.2f",
    length(test_ids), PROPERTY, min(fx_test$y), max(fx_test$y), stats::sd(fx_test$y))

## Tail masks on the test split's measured clay, [lo, hi).
tail_mask <- lapply(TAILS, function(b) fx_test$y >= b[1] & fx_test$y < b[2])
for (nm in names(tail_mask)) msg("[test] tail %s: %d rows", nm, sum(tail_mask[[nm]]))

## ---------------------------------------------------------------------------
## Pool: clay train_core minus the Iowa fixture
## ---------------------------------------------------------------------------

pool_ids <- setdiff(sp$train_core, fx_iowa$ids)
if (dry) { set.seed(OVERNIGHT$seed); pool_ids <- sample(pool_ids, POOL_DRY) }
pool_lab <- snap$lab[match(pool_ids, snap$lab$sample_id), c("sample_id", PROPERTY)]
stopifnot(!anyNA(pool_lab[[PROPERTY]]))
pool_hz  <- add_response(hz_spectra(snap, pool_ids), source = pool_lab, variable = PROPERTY)
pool_y   <- pool_lab[[PROPERTY]]
n_pool   <- length(pool_ids)
stopifnot(length(intersect(pool_ids, test_ids)) == 0L,
          length(intersect(pool_ids, fx_iowa$ids)) == 0L)
msg("[pool] %d rows on %d predictors; %s %.1f to %.1f, sd %.2f",
    n_pool, pool_hz$data$n_predictors, PROPERTY, min(pool_y), max(pool_y), stats::sd(pool_y))

## ---------------------------------------------------------------------------
## Similarity space: the one the library ships, fitted on the pool
## ---------------------------------------------------------------------------

pm     <- horizons:::predictor_matrix(pool_hz)
t_sp   <- system.time(
  space <- horizons:::build_similarity_space(pm$matrix, pm$wavenumbers,
                                             mask = MASK, sdev_floor = FLOOR)
)
SCORES <- space$scores
SIM_NCOMP <- as.integer(space$ncomp)
stopifnot(nrow(SCORES) == n_pool)
msg("[space] %d components retained (%.0f s); sdev %.3g to %.3g",
    SIM_NCOMP, t_sp[["elapsed"]], space$sdev[1], space$sdev[SIM_NCOMP])

## ---------------------------------------------------------------------------
## Selectors — every one returns row indices into pool_ids
## ---------------------------------------------------------------------------

sel_random <- function(size, seed) {
  set.seed(seed)
  sort(sample.int(n_pool, size))
}

## Kennard-Stone on the scores. --ks-stage1=N restricts the search to a
## random N-row subsample and tops the draw up at random; the note records it.
sel_kennard_stone <- function(size, seed) {
  if (KS_STAGE1 > 0L && KS_STAGE1 < n_pool && size <= KS_STAGE1) {
    set.seed(seed)
    idx <- sort(sample.int(n_pool, KS_STAGE1))
    ks  <- prospectr::kenStone(X = SCORES[idx, , drop = FALSE], k = size, metric = "euclid")
    return(sort(idx[ks$model]))
  }
  ks <- prospectr::kenStone(X = SCORES, k = size, metric = "euclid")
  sort(ks$model)
}

## duplex splits the pool into two sets of k, so it needs 2k <= n.
sel_duplex <- function(size, seed) {
  if (2L * size > n_pool) return(NULL)
  dx <- prospectr::duplex(X = SCORES, k = size, metric = "euclid")
  sort(dx$model)
}

## k-means on the scores, then the pool row nearest each centroid. Ties are
## resolved greedily against each centroid's 50 nearest candidates; a centroid
## whose whole candidate list is taken is dropped and the draw is topped up at
## random, so the returned set is always exactly `size` rows.
##
## At `centers` near `n_pool` Lloyd empties clusters and returns their centre
## as NaN (warned, not errored). A NaN centre would sort the candidate list
## arbitrarily, so those centres are dropped before the search and their share
## of the draw goes to the random top-up.
nearest_candidates <- function(C, X, n_cand, chunk = 500L) {
  x2  <- rowSums(X^2)
  out <- matrix(NA_integer_, nrow(C), n_cand)
  for (s in seq(1L, nrow(C), by = chunk)) {
    e  <- min(s + chunk - 1L, nrow(C))
    Cc <- C[s:e, , drop = FALSE]
    d2 <- outer(rowSums(Cc^2), x2, "+") - 2 * tcrossprod(Cc, X)
    for (i in seq_len(nrow(d2))) out[s + i - 1L, ] <- order(d2[i, ])[seq_len(n_cand)]
  }
  out
}

sel_kmedoids <- function(size, seed) {
  set.seed(seed)
  km   <- suppressWarnings(stats::kmeans(SCORES, centers = size, iter.max = 30L, nstart = 1L, algorithm = "Lloyd"))
  ctr  <- km$centers[stats::complete.cases(km$centers), , drop = FALSE]
  if (nrow(ctr) < nrow(km$centers)) {
    msg("  .. kmedoids: %d of %d centres were empty and are dropped; the draw is topped up at random",
        nrow(km$centers) - nrow(ctr), nrow(km$centers))
  }
  cand <- nearest_candidates(ctr, SCORES, n_cand = min(50L, n_pool))
  taken <- logical(n_pool)
  picks <- integer(0)
  for (i in seq_len(nrow(cand))) {
    free <- cand[i, ][!taken[cand[i, ]]]
    if (length(free)) { taken[free[1]] <- TRUE; picks <- c(picks, free[1]) }
  }
  if (length(picks) < size) {
    pool_free <- which(!taken)
    picks <- c(picks, sample(pool_free, size - length(picks)))
  }
  sort(picks[seq_len(size)])
}

## Clay deciles of the pool crossed with STRAT_Q spectral k-means clusters on
## the scores, drawn proportionally to cell size with a floor per non-empty
## cell so the tails keep their share.
sel_stratified <- function(size, seed) {
  set.seed(seed)
  qs   <- stats::quantile(pool_y, probs = seq(0, 1, length.out = STRAT_Q + 1L), na.rm = TRUE)
  dec  <- cut(pool_y, breaks = unique(qs), include.lowest = TRUE, labels = FALSE)
  km   <- stats::kmeans(SCORES, centers = STRAT_Q, iter.max = 50L, nstart = 3L)
  cell <- paste(dec, km$cluster, sep = "-")
  tab  <- table(cell)
  keys <- names(tab)
  n_i  <- as.integer(tab)
  fl   <- max(2L, as.integer(floor(size / 400)))
  alloc <- pmin(n_i, pmax(fl, as.integer(round(size * n_i / n_pool))))
  ## Reconcile to exactly `size`: trim the largest allocations that sit above
  ## the floor, or top up the cells with capacity left, largest first.
  while (sum(alloc) > size) {
    over <- which(alloc > fl)
    if (!length(over)) { over <- which(alloc > 1L); if (!length(over)) break }
    alloc[over[which.max(alloc[over])]] <- alloc[over[which.max(alloc[over])]] - 1L
  }
  while (sum(alloc) < size) {
    room <- which(alloc < n_i)
    if (!length(room)) break
    alloc[room[which.max(n_i[room] - alloc[room])]] <- alloc[room[which.max(n_i[room] - alloc[room])]] + 1L
  }
  picks <- unlist(lapply(seq_along(keys), function(j) {
    rows <- which(cell == keys[j])
    if (alloc[j] >= length(rows)) rows else sample(rows, alloc[j])
  }), use.names = FALSE)
  sort(picks)
}

draw <- function(selector, size, seed) {
  switch(selector,
         random        = sel_random(size, seed),
         kennard_stone = sel_kennard_stone(size, seed),
         duplex        = sel_duplex(size, seed),
         kmedoids      = sel_kmedoids(size, seed),
         stratified    = sel_stratified(size, seed),
         stop("unknown selector: ", selector))
}

## ---------------------------------------------------------------------------
## The chain, prediction, and records
## ---------------------------------------------------------------------------

run_chain <- function(train, learner, tag) {
  t_c <- system.time(hz <- configure(train, outcome = PROPERTY, models = learner, transformations = TR,
                                     preprocessing = PREPROC, feature_selection = FEATSEL,
                                     cv_folds = TUNING$cv_folds, grid_size = TUNING$grid_size,
                                     bayesian_iter = TUNING$bayesian_iter,
                                     final_bayesian_iter = TUNING$final_bayesian_iter))
  t_e <- system.time(hz <- eval_exp(hz, file.path(CKPT, tag), verbose = FALSE))
  t_f <- system.time(f <- fit(hz, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE,
                              allow_par = workers > 1L, seed = SEED, verbose = FALSE))
  msg("[chain %s] %d rows; configure %.0f s, evaluate %.0f s, fit %.0f s",
      tag, train$data$n_rows, t_c[["elapsed"]], t_e[["elapsed"]], t_f[["elapsed"]])
  list(fit = f, secs = t_c[["elapsed"]] + t_e[["elapsed"]] + t_f[["elapsed"]])
}

predict_fixture <- function(f, fx) {
  p <- as_tibble(predict(f, fx$hz, interval = FALSE))
  stopifnot(all(fx$ids %in% p$sample_id))
  tibble(sample_id = fx$ids, .pred = p$.pred[match(fx$ids, p$sample_id)], truth = fx$y)
}

## yardstick needs a few points; a tail bin can be thin in a pilot.
metrics_safe <- function(truth, pred) {
  ok <- is.finite(truth) & is.finite(pred)
  if (sum(ok) < 3L) {
    return(tibble(n_scored = sum(ok), rmse = NA_real_, rpd = NA_real_,
                  ccc = NA_real_, rsq = NA_real_, bias = NA_real_))
  }
  metrics_row(truth, pred)
}

## One set's metrics, suffixed for the wide row.
set_cols <- function(truth, pred, label) {
  m  <- metrics_safe(truth, pred)
  out <- tibble(m$n_scored, m$rmse, m$bias, m$ccc, m$rpd, m$rsq)
  names(out) <- paste0(c("n_", "rmse_", "bias_", "ccc_", "rpd_", "rsq_"), label)
  out
}

make_row <- function(arm, selector, size, seed, learner, n_train, p_test, p_iowa, secs) {
  base <- tibble(arm = arm, selector = selector, size = as.integer(size),
                 seed = if (is.na(seed)) NA_integer_ else as.integer(seed),
                 learner = learner, n_pool = as.integer(n_pool), n_train = as.integer(n_train),
                 sim_ncomp = SIM_NCOMP)
  tails <- lapply(names(tail_mask), function(nm) {
    k <- tail_mask[[nm]]
    set_cols(p_test$truth[k], p_test$.pred[k], nm)
  })
  bind_cols(base,
            set_cols(p_test$truth, p_test$.pred, "test"),
            set_cols(p_iowa$truth, p_iowa$.pred, "iowa"),
            bind_cols(tails),
            tibble(secs = secs, pilot = dry,
                   ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
}

append_csv <- function(row, path) {
  write.table(row, path, sep = ",", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path))
}

write_preds <- function(arm, p_test, p_iowa) {
  p <- bind_rows(mutate(p_test, set = "test"), mutate(p_iowa, set = "iowa"))
  p$arm <- arm
  write.csv(p, file.path(PREDS, sprintf("preds-%s.csv", arm)), row.names = FALSE)
}

done_arms <- function() {
  if (!file.exists(rows_path)) return(character())
  r <- read.csv(rows_path, stringsAsFactors = FALSE)
  r$arm[r$pilot == dry]
}

## On-disk cost of the thinned pool itself, recorded once per draw.
done_sizes <- function() {
  if (!file.exists(sizes_path)) return(character())
  s <- read.csv(sizes_path, stringsAsFactors = FALSE)
  s$draw[s$pilot == dry]
}

record_size <- function(draw_label, selector, size, seed, idx) {
  if (draw_label %in% done_sizes()) return(invisible(NULL))
  sub <- horizons:::subset_rows(pool_hz, pool_ids[idx])
  mb  <- artifact_bytes(sub) / 1024^2
  append_csv(tibble(draw = draw_label, selector = selector, size = as.integer(size),
                    seed = if (is.na(seed)) NA_integer_ else as.integer(seed),
                    n_rows = as.integer(length(idx)), mb = mb,
                    budget_mb = OVERNIGHT$artifact_mb, pilot = dry,
                    ran_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
             sizes_path)
  msg("[size] %-24s %5d rows -> %.1f MB on disk (budget %d MB)", draw_label, length(idx), mb, OVERNIGHT$artifact_mb)
  rm(sub); invisible(gc())
}

## ---------------------------------------------------------------------------
## The arm list: references first (the verdict needs them), then sizes ascending
## ---------------------------------------------------------------------------

selector_label <- function(selector, seed) {
  if (selector == "random") sprintf("random-s%d", seed) else selector
}

draws <- list()
for (size in sort(SIZES)) {
  for (selector in SELECTORS) {
    seeds <- if (selector == "random") SEEDS else OVERNIGHT$seed
    for (seed in seeds) {
      lab <- selector_label(selector, seed)
      draws[[length(draws) + 1L]] <- list(selector = selector, size = as.integer(size),
                                          seed = as.integer(seed), label = lab,
                                          draw = sprintf("%s-n%d", lab, size))
    }
  }
}

future::plan(future.callr::callr, workers = workers)
on.exit(future::plan(future::sequential), add = TRUE)

## ---- references: the full pool ---------------------------------------------

for (learner in LEARNERS) {
  arm <- paste0("full-", learner)
  if (arm %in% done_arms()) { msg("[%s] already recorded, skipping", arm); next }
  ch     <- run_chain(pool_hz, learner, arm)
  p_test <- predict_fixture(ch$fit, fx_test)
  p_iowa <- predict_fixture(ch$fit, fx_iowa)
  row    <- make_row(arm, "full", n_pool, NA, learner, n_pool, p_test, p_iowa, ch$secs)
  append_csv(row, rows_path); write_preds(arm, p_test, p_iowa)
  msg("[%s] test RMSE %.3f | iowa %.3f | low %.3f | high %.3f", arm,
      row$rmse_test, row$rmse_iowa, row$rmse_low, row$rmse_high)
  rm(ch, p_test, p_iowa); invisible(gc())
}
record_size("full", "full", n_pool, NA, seq_len(n_pool))

## ---- thinned arms -----------------------------------------------------------

for (d in draws) {

  arms <- vapply(LEARNERS, function(l) sprintf("%s-n%d-%s", d$label, d$size, l), character(1))
  todo <- setdiff(arms, done_arms())
  if (!length(todo)) { msg("[%s] all learners recorded, skipping", d$draw); next }

  t_d <- system.time(idx <- draw(d$selector, d$size, d$seed))
  if (is.null(idx)) {
    msg("[%s] SKIPPED: %s needs 2k <= n_pool (2 x %d > %d)", d$draw, d$selector, d$size, n_pool)
    next
  }
  stopifnot(length(idx) == d$size, !anyDuplicated(idx))
  msg("[%s] drew %d rows in %.0f s; %s %.1f to %.1f, sd %.2f", d$draw, length(idx), t_d[["elapsed"]],
      PROPERTY, min(pool_y[idx]), max(pool_y[idx]), stats::sd(pool_y[idx]))
  record_size(d$draw, d$selector, d$size, d$seed, idx)

  train <- horizons:::subset_rows(pool_hz, pool_ids[idx])

  for (learner in LEARNERS) {
    arm <- sprintf("%s-n%d-%s", d$label, d$size, learner)
    if (!arm %in% todo) { msg("[%s] already recorded, skipping", arm); next }
    ch     <- run_chain(train, learner, arm)
    p_test <- predict_fixture(ch$fit, fx_test)
    p_iowa <- predict_fixture(ch$fit, fx_iowa)
    row    <- make_row(arm, d$selector, d$size, d$seed, learner, train$data$n_rows,
                       p_test, p_iowa, ch$secs + t_d[["elapsed"]])
    append_csv(row, rows_path); write_preds(arm, p_test, p_iowa)
    msg("[%s] test RMSE %.3f | iowa %.3f | low %.3f | high %.3f", arm,
        row$rmse_test, row$rmse_iowa, row$rmse_low, row$rmse_high)
    rm(ch, p_test, p_iowa); invisible(gc())
  }
  rm(train); invisible(gc())
}

future::plan(future::sequential)

## ---------------------------------------------------------------------------
## Verdict: noninferiority against the reference, on the bar set before the run
## ---------------------------------------------------------------------------

r <- read.csv(rows_path, stringsAsFactors = FALSE)
r <- r[r$pilot == dry, , drop = FALSE]
ref <- r[r$selector == "full", , drop = FALSE]
body <- r[r$selector != "full", , drop = FALSE]

if (nrow(ref) && nrow(body)) {

  ratio_of <- function(col) {
    b <- body[[col]]
    m <- ref[[col]][match(body$learner, ref$learner)]
    b / m
  }
  body$ratio_test <- ratio_of("rmse_test")
  body$ratio_iowa <- ratio_of("rmse_iowa")
  for (nm in names(tail_mask)) body[[paste0("ratio_", nm)]] <- ratio_of(paste0("rmse_", nm))

  key <- paste(body$selector, body$size, body$learner, sep = "|")
  verdict <- bind_rows(lapply(unique(key), function(k) {
    g  <- body[key == k, , drop = FALSE]
    tl <- vapply(names(tail_mask), function(nm) mean(g[[paste0("ratio_", nm)]], na.rm = TRUE), numeric(1))
    out <- tibble(selector = g$selector[1], size = g$size[1], learner = g$learner[1],
                  n_seeds = nrow(g),
                  rmse_test = mean(g$rmse_test, na.rm = TRUE),
                  ratio_test = mean(g$ratio_test, na.rm = TRUE),
                  ratio_test_max = max(g$ratio_test, na.rm = TRUE),
                  ratio_iowa = mean(g$ratio_iowa, na.rm = TRUE))
    for (nm in names(tl)) out[[paste0("ratio_", nm)]] <- unname(tl[nm])
    out$pass <- isTRUE(out$ratio_test <= 1 + OVERNIGHT$margin_rmse) &&
                all(is.finite(tl)) && all(tl <= 1 + OVERNIGHT$margin_tail)
    out
  }))
  verdict <- verdict[order(verdict$learner, verdict$size, verdict$selector), ]
  verdict$margin_rmse <- OVERNIGHT$margin_rmse
  verdict$margin_tail <- OVERNIGHT$margin_tail
  verdict$pilot       <- dry
  write.csv(verdict, verdict_path, row.names = FALSE)

  cat("\n")
  msg("[verdict] reference RMSE (test): %s",
      paste(sprintf("%s %.3f", ref$learner, ref$rmse_test), collapse = " | "))
  msg("[verdict] pass = ratio_test <= %.2f and every tail ratio <= %.2f",
      1 + OVERNIGHT$margin_rmse, 1 + OVERNIGHT$margin_tail)
  print(as.data.frame(verdict[, c("learner", "size", "selector", "n_seeds", "rmse_test", "ratio_test",
                                  "ratio_test_max", paste0("ratio_", names(tail_mask)), "ratio_iowa", "pass")]),
        row.names = FALSE, digits = 3)

  if (file.exists(sizes_path)) {
    s <- read.csv(sizes_path, stringsAsFactors = FALSE)
    cat("\n")
    msg("[sizes] thinned pools on disk (qs2), budget %d MB", OVERNIGHT$artifact_mb)
    print(as.data.frame(s[s$pilot == dry, c("draw", "n_rows", "mb")]), row.names = FALSE, digits = 3)
  }

} else {
  msg("[verdict] not enough arms recorded to build a verdict table")
}

msg("[thin] done")
