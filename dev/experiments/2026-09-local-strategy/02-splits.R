## ===========================================================================
## 02 — Splits per property: test holdout, external calibration, train core
## ===========================================================================
##
## Per property: keep rows with a non-NA outcome inside the physical range;
## stratified 80/20 holdout (seed 307) -> test; from the 80 %, a stratified
## 15 % (seed 308) -> calib_ext, carved BEFORE any clustering; the rest is
## train_core. Every strategy asserts these id sets (by SHA) before running.
##
## Run: Rscript dev/experiments/2026-09-local-strategy/02-splits.R [--force]

args     <- commandArgs(trailingOnly = TRUE)
force    <- "--force" %in% args
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
exp_dir  <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()

source(file.path(exp_dir, "00-config.R"))
source(file.path(exp_dir, "helpers.R"))
exp_dirs()

snap <- load_snapshot()
msg("Snapshot: %d samples.", nrow(snap$lab))

summary_rows <- list()

for (property in EXP_PROPERTIES$property) {

  if (file.exists(splits_path(property)) && !force) {
    msg("%s: splits exist (use --force to redo).", property)
    next
  }

  pr <- property_rows(snap$lab, property)
  df <- tibble(sample_id = pr$ids,
               y         = snap$lab[[property]][match(pr$ids, snap$lab$sample_id)])

  set.seed(SEED)
  s1        <- rsample::initial_split(df, prop = 1 - EXP_SPLIT$test_prop, strata = y, breaks = 10)
  test_ids  <- rsample::testing(s1)$sample_id
  pool      <- rsample::training(s1)

  set.seed(SEED + 1L)
  s2        <- rsample::initial_split(pool, prop = 1 - EXP_SPLIT$calib_prop, strata = y, breaks = 10)
  calib_ids <- rsample::testing(s2)$sample_id
  train_ids <- rsample::training(s2)$sample_id

  stopifnot(length(intersect(train_ids, test_ids)) == 0L,
            length(intersect(calib_ids, test_ids)) == 0L,
            length(intersect(train_ids, calib_ids)) == 0L,
            setequal(c(train_ids, calib_ids, test_ids), pr$ids))

  out <- list(
    property     = property,
    n_total      = pr$n_total,
    n_na         = pr$n_na,
    n_impossible = pr$n_impossible,
    n_usable     = length(pr$ids),
    train_core   = train_ids,
    calib_ext    = calib_ids,
    test         = test_ids,
    sha          = list(train_core = sha_ids(train_ids),
                        calib_ext  = sha_ids(calib_ids),
                        test       = sha_ids(test_ids)),
    seed         = SEED,
    created      = Sys.time()
  )
  qs2::qs_save(out, splits_path(property))

  summary_rows[[property]] <- tibble(
    property = property, n_total = pr$n_total, n_na = pr$n_na,
    n_impossible = pr$n_impossible, n_usable = length(pr$ids),
    n_train_core = length(train_ids), n_calib_ext = length(calib_ids),
    n_test = length(test_ids), sha_test = substr(out$sha$test, 1, 12)
  )
  msg("%s: usable %d (NA %d, impossible %d) -> train_core %d / calib_ext %d / test %d",
      property, length(pr$ids), pr$n_na, pr$n_impossible,
      length(train_ids), length(calib_ids), length(test_ids))
}

if (length(summary_rows)) {
  tab <- bind_rows(summary_rows)
  readr::write_csv(tab, file.path(RESULTS_DIR, "splits-summary.csv"))
  print(tab)
}
