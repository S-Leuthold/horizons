## ===========================================================================
## 09 — Test 2b: the reference population (space_rows = "all" vs "measured")
## ===========================================================================
##
## Purpose (2026-09-21). A property's draw is restricted to the pool rows
## that have it measured. Under the default the similarity space those rows
## are ranked in is fit on the WHOLE pool; under space_rows = "measured" it
## is fit on the measured subset alone. If the measured subset is a
## systematically different population, the two spaces can rank neighbours
## differently. The spec says which is right is not decidable by reasoning;
## this script measures how much the two disagree. The answer decides
## whether a packaged library caches one space or one per property.
##
## Design:
##   pool     the full KSSL snapshot at 4 cm-1 minus the target ids, with
##            oc (99.6% measured), clay (57%) and carbonate (30%) attached.
##            oc is the control: nearly the same rows, so nearly the same space.
##   targets  the 9/17 KSSL control batch (101 ids), same grid, so
##            reconciliation passes through.
##   runs     space_rows in {all, measured} x metric in {mahalanobis, euclidean},
##            k = K_DEEP, experiment defaults otherwise (1st derivative, water
##            mask, PCA to 99%).
##   metrics  per target and property: Jaccard overlap of the two neighbour
##            sets at k = 100 and k = K_DEEP; Spearman correlation of the two
##            distance vectors over the rows both deep sets share. Batch level:
##            union sizes and their overlap. Per space: components retained.
##
## Output: results/space-rows/per-target.csv, summary.csv, spaces.csv, and a
## printed summary.
##
## Run (from package/):
##   Rscript -e 'devtools::load_all(".", quiet = TRUE); source("dev/experiments/2026-09-factorial/09-space-rows.R")'
## ===========================================================================

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
PROPERTIES <- c("oc", "clay", "carbonate")
K_DEEP     <- 500L
K_SHALLOW  <- 100L
MASK       <- rbind(c(1580, 1720), c(3100, 3700))
METRICS    <- c("mahalanobis", "euclidean")

CKPT    <- file.path(this_dir, "results", "coherent-batch", "checkpoints")
OUT_DIR <- file.path(this_dir, "results", "space-rows")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

ref <- qs2::qs_read(file.path(CKPT, "kssl-targets.qs2"))

## ---------------------------------------------------------------------------
## Pool: every snapshot row except the targets, three properties attached
## ---------------------------------------------------------------------------

snap     <- load_snapshot()
pool_ids <- setdiff(snap$spectra$sample_id, ref$ids)

t_pool <- system.time({
  pool <- hz_spectra(snap, pool_ids)
  lab  <- snap$lab[match(pool_ids, snap$lab$sample_id), c("sample_id", PROPERTIES)]
  for (p in PROPERTIES) {
    pool <- add_response(pool, lab[, c("sample_id", p)], variable = p)
  }
})
msg("[2b] pool: %d rows, %d predictors, %.0f s", pool$data$n_rows, pool$data$n_predictors, t_pool[["elapsed"]])
msg("[2b] measured per property: %s",
    paste(sprintf("%s=%d", PROPERTIES, colSums(!is.na(lab[PROPERTIES]))), collapse = ", "))

targets <- hz_spectra(snap, ref$ids)
msg("[2b] targets: %d rows", targets$data$n_rows)

## ---------------------------------------------------------------------------
## The four runs
## ---------------------------------------------------------------------------

runs <- list()
for (metric in METRICS) {
  for (rows in c("all", "measured")) {
    key <- paste(metric, rows, sep = "/")
    t   <- system.time(
      out <- select_training(targets, pool, k = K_DEEP, properties = PROPERTIES,
                             mask = MASK, metric = metric, space_rows = rows,
                             verbose = FALSE)
    )
    sel <- out$selection
    msg("[2b] %-22s %6d union rows, ncomp %s, %.0f s", key, out$data$n_rows,
        if (rows == "all") sel$settings$ncomp_retained
        else paste(sprintf("%s=%d", names(sel$settings$ncomp_by_property), sel$settings$ncomp_by_property), collapse = " "),
        t[["elapsed"]])
    runs[[key]] <- list(membership = sel$membership, n_union = out$data$n_rows,
                        pool_sizes = sel$pool_sizes, settings = sel$settings,
                        union_ids = out$data$analysis$sample_id)
  }
}

## ---------------------------------------------------------------------------
## Per-target comparison, all vs measured, within each metric
## ---------------------------------------------------------------------------

per_target <- list()
for (metric in METRICS) {
  ma <- runs[[paste(metric, "all",      sep = "/")]]$membership |> arrange(property, target_id, rank)
  mm <- runs[[paste(metric, "measured", sep = "/")]]$membership |> arrange(property, target_id, rank)
  for (p in PROPERTIES) {
    a <- ma |> filter(property == p)
    m <- mm |> filter(property == p)
    for (t in ref$ids) {
      at <- a |> filter(target_id == t)
      mt <- m |> filter(target_id == t)
      a100 <- at$pool_id[at$rank <= K_SHALLOW]; m100 <- mt$pool_id[mt$rank <= K_SHALLOW]
      shared <- intersect(at$pool_id, mt$pool_id)
      rho <- if (length(shared) >= 10L) {
        stats::cor(at$distance[match(shared, at$pool_id)],
                   mt$distance[match(shared, mt$pool_id)], method = "spearman")
      } else NA_real_
      per_target[[length(per_target) + 1L]] <- tibble(
        metric = metric, property = p, target_id = t,
        jaccard_100  = length(intersect(a100, m100)) / length(union(a100, m100)),
        jaccard_deep = length(shared) / length(union(at$pool_id, mt$pool_id)),
        n_shared_deep = length(shared),
        spearman_shared = rho,
        nearest_all = at$distance[at$rank == 1L], nearest_measured = mt$distance[mt$rank == 1L]
      )
    }
  }
}
per_target <- bind_rows(per_target)

## ---------------------------------------------------------------------------
## Summaries
## ---------------------------------------------------------------------------

summary_tbl <- per_target |>
  group_by(metric, property) |>
  summarise(
    n_targets            = n(),
    jaccard100_median    = median(jaccard_100),
    jaccard100_q10       = quantile(jaccard_100, 0.10),
    jaccard100_min       = min(jaccard_100),
    jaccard_deep_median  = median(jaccard_deep),
    spearman_median      = median(spearman_shared, na.rm = TRUE),
    spearman_q10         = quantile(spearman_shared, 0.10, na.rm = TRUE),
    .groups = "drop"
  )

batch_tbl <- bind_rows(lapply(METRICS, function(metric) {
  ua <- runs[[paste(metric, "all",      sep = "/")]]$union_ids
  um <- runs[[paste(metric, "measured", sep = "/")]]$union_ids
  tibble(metric = metric, union_all = length(ua), union_measured = length(um),
         union_shared = length(intersect(ua, um)),
         union_jaccard = length(intersect(ua, um)) / length(union(ua, um)))
}))

spaces_tbl <- bind_rows(lapply(names(runs), function(key) {
  s <- runs[[key]]$settings
  if (is.null(s$ncomp_by_property)) {
    tibble(run = key, property = "(all rows)", n_rows = pool$data$n_rows, ncomp = s$ncomp_retained)
  } else {
    tibble(run = key, property = names(s$ncomp_by_property),
           n_rows = unname(s$space_n_rows), ncomp = unname(s$ncomp_by_property))
  }
}))

readr::write_csv(per_target,  file.path(OUT_DIR, "per-target.csv"))
readr::write_csv(summary_tbl, file.path(OUT_DIR, "summary.csv"))
readr::write_csv(batch_tbl,   file.path(OUT_DIR, "batch.csv"))
readr::write_csv(spaces_tbl,  file.path(OUT_DIR, "spaces.csv"))

cat("\n== spaces ==\n");            print(as.data.frame(spaces_tbl), row.names = FALSE)
cat("\n== per-target summary ==\n"); print(as.data.frame(summary_tbl), row.names = FALSE, digits = 3)
cat("\n== batch unions ==\n");      print(as.data.frame(batch_tbl), row.names = FALSE, digits = 3)
msg("[2b] written to %s", OUT_DIR)
