## ===========================================================================
## 08 — Does select_training() reproduce the hand-built selection?
## ===========================================================================
##
## Purpose (2026-09-21). 05-coherent-batch.R drew the batch pools by hand in
## the similarity space the spec fixed, before the verb existed. The verb
## now exists (feat/select-training) with that chain as its defaults. This
## script runs it on the same pool and the same KSSL control batch and
## checks three things against the 9/17 checkpoint:
##
##   1. the union at k = 100 is exactly 3,426 rows (pool-sizes.csv);
##   2. every target's 100 neighbour ids match kssl-targets.qs2's nb$nn_ids;
##   3. the space retains the same number of components (33).
##
## The KSSL batch is the same-grid case (both sides std() to 4 cm-1 from the
## same snapshot), so reconciliation is a pass-through and the answer should
## be identical. The MOYS batch is NOT re-run here: the experiment
## interpolated MOYS onto the pool's grid, the verb resamples the pool onto
## the targets', and those differ by design.
##
## Run (from package/, against a load_all() or the installed package):
##   Rscript dev/experiments/2026-09-factorial/08-verify-verb.R
## ===========================================================================

## When source()d from package/ after devtools::load_all(), there is no
## --file; fall back to the experiment directory under the working dir.
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else if (dir.exists("dev/experiments/2026-09-factorial")) {
  normalizePath("dev/experiments/2026-09-factorial")
} else {
  getwd()
}
exp1_dir <- file.path(dirname(this_dir), "2026-09-local-strategy")

source(file.path(exp1_dir, "00-config.R"))
suppressPackageStartupMessages({
  if (!isNamespaceLoaded("horizons")) library(horizons)
  library(dplyr)
})
source(file.path(exp1_dir, "helpers.R"))

EXPERIMENT_RESAMPLE <- 4
PROPERTY <- "oc"
K        <- 100L
MASK     <- rbind(c(1580, 1720), c(3100, 3700))   # EXP_LOCAL$water_bands, as 05 applied them

CKPT <- file.path(this_dir, "results", "coherent-batch", "checkpoints")
ref  <- qs2::qs_read(file.path(CKPT, "kssl-targets.qs2"))

## ---------------------------------------------------------------------------
## Pool: oc train_core at 4 cm-1, with oc attached
## ---------------------------------------------------------------------------

snap <- load_snapshot()
sp   <- load_splits(PROPERTY); assert_splits(sp)
pool_ids <- sp$train_core

t_pool <- system.time({
  pool <- hz_spectra(snap, pool_ids)
  lab  <- snap$lab[match(pool_ids, snap$lab$sample_id), c("sample_id", PROPERTY)]
  pool <- add_response(pool, lab, variable = PROPERTY)
})
msg("[verify] pool: %d rows, %d predictors, %.0f s", pool$data$n_rows, pool$data$n_predictors, t_pool[["elapsed"]])

## ---------------------------------------------------------------------------
## Targets: the 9/17 KSSL control batch, same ids
## ---------------------------------------------------------------------------

targets <- hz_spectra(snap, ref$ids)
msg("[verify] targets: %d rows on %d predictors", targets$data$n_rows, targets$data$n_predictors)

## ---------------------------------------------------------------------------
## The verb
## ---------------------------------------------------------------------------

t_sel <- system.time(
  out <- select_training(targets, pool, k = K, properties = PROPERTY, mask = MASK)
)
msg("[verify] select_training() in %.0f s", t_sel[["elapsed"]])

## ---------------------------------------------------------------------------
## Checks
## ---------------------------------------------------------------------------

sel <- out$selection

check <- function(name, ok, detail = "") {
  msg("[verify] %s %s %s", if (ok) "PASS" else "FAIL", name, detail)
  ok
}

r1 <- check("union size = 3426", out$data$n_rows == 3426L, sprintf("(got %d)", out$data$n_rows))
r3 <- check("components = 33",   sel$settings$ncomp_retained == 33L, sprintf("(got %d)", sel$settings$ncomp_retained))

## Per-target neighbour ids, ordered by rank, against the experiment's
m   <- sel$membership |> arrange(target_id, rank)
ids_verb <- split(m$pool_id, m$target_id)
ids_ref  <- setNames(lapply(seq_along(ref$ids), function(i) ref$nb$nn_ids[i, seq_len(K)]), ref$ids)

same_order <- vapply(names(ids_ref), function(t) identical(ids_verb[[t]], unname(ids_ref[[t]])), logical(1))
same_set   <- vapply(names(ids_ref), function(t) setequal(ids_verb[[t]], ids_ref[[t]]), logical(1))

r2a <- check("neighbour sets identical for every target",   all(same_set),   sprintf("(%d/%d)", sum(same_set), length(same_set)))
r2b <- check("neighbour order identical for every target",  all(same_order), sprintf("(%d/%d)", sum(same_order), length(same_order)))

## Distances: the verb's nearest per target against the experiment's min_dist
td  <- sel$target_distances
d_v <- td$nearest[match(ref$ids, td$target_id)]
d_r <- unname(ref$nb$min_dist[ref$ids])
r4  <- check("nearest distances match to 1e-8", isTRUE(all.equal(d_v, d_r, tolerance = 1e-8)),
             sprintf("(max abs diff %.2e)", max(abs(d_v - d_r))))

msg("[verify] twins excluded: %d; targets beyond pool spread: %d; reconciliation: %s",
    nrow(sel$exclusions), nrow(sel$resemblance$beyond), sel$reconciliation$operation)

if (!all(r1, r2a, r2b, r3, r4)) {
  ## Show where the first mismatch is
  bad <- names(same_set)[!same_set][1]
  if (!is.na(bad)) {
    msg("[verify] first mismatch: target %s", bad)
    msg("[verify]   verb-only ids: %s", paste(head(setdiff(ids_verb[[bad]], ids_ref[[bad]]), 5), collapse = ", "))
    msg("[verify]   ref-only ids:  %s", paste(head(setdiff(ids_ref[[bad]], ids_verb[[bad]]), 5), collapse = ", "))
  }
  quit(status = 1)
}

msg("[verify] all checks passed")
