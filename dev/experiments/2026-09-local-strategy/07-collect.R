## ===========================================================================
## 07 — Collect results, paired bootstrap on ΔRPD, apply the decision rule
## ===========================================================================
##
## Reads every results/row-*.csv and results/per-cluster-*.csv, writes
## results/results.csv and results/per-cluster.csv, computes a 500-draw
## paired bootstrap of ΔRPD / ΔRMSE over identical test rows for the pairs
## the decision rule needs, applies the rule exactly as written in README.md,
## and writes results/verdict.md. Safe to run at any time; missing cells are
## reported, not invented.
##
## Run: Rscript dev/experiments/2026-09-local-strategy/07-collect.R [--from=A_global|A_pilot] [--draws=500]

args     <- commandArgs(trailingOnly = TRUE)
from_arg <- sub("^--from=", "", grep("^--from=", args, value = TRUE))
d_arg    <- sub("^--draws=", "", grep("^--draws=", args, value = TRUE))
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
exp_dir  <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()

source(file.path(exp_dir, "00-config.R"))
suppressPackageStartupMessages(devtools::load_all(PKG_DIR, quiet = TRUE))
source(file.path(exp_dir, "helpers.R"))
exp_dirs()

A_name <- if (length(from_arg)) from_arg else "A_global"
DRAWS  <- if (length(d_arg)) as.integer(d_arg) else 500L

## ---------------------------------------------------------------------------
## 1. Results table
## ---------------------------------------------------------------------------

row_files <- list.files(RESULTS_DIR, "^row-.*\\.csv$", full.names = TRUE)
if (!length(row_files)) stop("No results rows yet.")
results <- bind_rows(lapply(row_files, readr::read_csv, show_col_types = FALSE))
results <- results[order(results$property, results$strategy), ]
readr::write_csv(results, file.path(RESULTS_DIR, "results.csv"))

pc_files <- list.files(RESULTS_DIR, "^per-cluster-.*\\.csv$", full.names = TRUE)
if (length(pc_files)) {
  per_cluster <- bind_rows(lapply(pc_files, function(f) {
    x <- readr::read_csv(f, show_col_types = FALSE); x$source <- basename(f); x
  }))
  readr::write_csv(per_cluster, file.path(RESULTS_DIR, "per-cluster.csv"))
}

show <- c("property", "strategy", "config", "n_test", "rmse", "rpd", "ccc", "coverage_90",
          "mean_width", "coverage_90_native", "predict_secs_per_100", "artifact_bytes", "k_clusters", "n_fallback")
cat("\n== results (", nrow(results), " rows) ==\n", sep = "")
print(as.data.frame(results[, intersect(show, names(results))]), digits = 4)

## ---------------------------------------------------------------------------
## 2. Paired bootstrap of ΔRPD and ΔRMSE over identical test rows
## ---------------------------------------------------------------------------

load_preds <- function(property, strategy) {
  s <- if (strategy == "A") A_name else switch(strategy, B = "B_gmm_oneconfig", C = "C_gmm_perconfig",
                                               D = "D_mbl", E = "E_soft", strategy)
  if (!has_ckpt(property, s)) return(NULL)
  p <- load_ckpt(property, s)$test_preds
  p[, c("sample_id", "truth", ".pred")]
}

pairs <- list(c("B", "A"), c("C", "B"), c("D", "B"), c("E", "B"), c("D", "A"), c("C", "A"), c("E", "A"))
boot_rows <- list()
set.seed(SEED)
for (property in EXP_PROPERTIES$property) {
  for (pr in pairs) {
    s <- load_preds(property, pr[1]); r <- load_preds(property, pr[2])
    if (is.null(s) || is.null(r)) next
    j <- inner_join(s, r, by = "sample_id", suffix = c("_s", "_r"))
    j <- j[is.finite(j$.pred_s) & is.finite(j$.pred_r) & is.finite(j$truth_s), ]
    n <- nrow(j); if (n < 50) next
    d_rpd <- d_rmse <- numeric(DRAWS)
    for (b in seq_len(DRAWS)) {
      i <- sample.int(n, n, replace = TRUE)
      d_rpd[b]  <- rpd_vec(j$truth_s[i], j$.pred_s[i]) - rpd_vec(j$truth_s[i], j$.pred_r[i])
      d_rmse[b] <- yardstick::rmse_vec(j$truth_s[i], j$.pred_s[i]) - yardstick::rmse_vec(j$truth_s[i], j$.pred_r[i])
    }
    boot_rows[[length(boot_rows) + 1]] <- tibble(
      property = property, strategy = pr[1], reference = pr[2], n = n,
      d_rpd = rpd_vec(j$truth_s, j$.pred_s) - rpd_vec(j$truth_s, j$.pred_r),
      d_rpd_lo = quantile(d_rpd, 0.025), d_rpd_hi = quantile(d_rpd, 0.975), d_rpd_se = sd(d_rpd),
      d_rmse = yardstick::rmse_vec(j$truth_s, j$.pred_s) - yardstick::rmse_vec(j$truth_s, j$.pred_r),
      d_rmse_lo = quantile(d_rmse, 0.025), d_rmse_hi = quantile(d_rmse, 0.975)
    )
  }
}
boot <- bind_rows(boot_rows)
if (nrow(boot)) {
  readr::write_csv(boot, file.path(RESULTS_DIR, "bootstrap-deltas.csv"))
  cat("\n== paired bootstrap (", DRAWS, " draws) ==\n", sep = "")
  print(as.data.frame(boot), digits = 3)
}

## ---------------------------------------------------------------------------
## 3. Decision rule (README.md, pre-registered)
## ---------------------------------------------------------------------------

props <- EXP_PROPERTIES$property
get <- function(strategy, col) {
  s <- switch(strategy, A = A_name, B = "B_gmm_oneconfig", C = "C_gmm_perconfig", D = "D_mbl", E = "E_soft")
  vapply(props, function(p) { r <- results[results$property == p & results$strategy == s, col]; if (nrow(r)) r[[1]][1] else NA_real_ }, numeric(1))
}
have <- function(strategy) all(is.finite(get(strategy, "rpd")))
eligible <- function(strategy) { cv <- get(strategy, "coverage_90"); all(is.finite(cv)) && all(cv >= 0.87 & cv <= 0.93) }
wins_on <- function(a, b, margin) sum(get(a, "rpd") - get(b, "rpd") >= margin, na.rm = TRUE)
worse_by <- function(a, b, margin) any(get(b, "rpd") - get(a, "rpd") > margin, na.rm = TRUE)
ccc_ok <- function(a, b) all(get(b, "ccc") - get(a, "ccc") <= 0.02, na.rm = TRUE)

lines <- c("# Verdict (generated by 07-collect.R)", "",
           paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), "; A source = `", A_name, "`; draws = ", DRAWS, "."), "")
status <- sapply(c("A", "B", "C", "D", "E"), function(s) if (!have(s)) "missing" else if (eligible(s)) "eligible" else "INELIGIBLE (coverage)")
lines <- c(lines, "## Eligibility", "", paste0("- ", names(status), ": ", status,
           ifelse(status != "missing", paste0(" — coverage ", paste(sprintf("%.3f", get(names(status), "coverage_90")), collapse = "/")), "")), "")

verdict <- "UNDECIDED — cells missing"
if (all(status[c("A", "B", "D")] != "missing")) {
  el <- function(s) status[[s]] == "eligible"
  d_time  <- get("D", "predict_secs_per_100"); d_bytes <- get("D", "artifact_bytes")
  d_ok    <- el("D") && wins_on("D", "B", 0.10) >= 2 && !worse_by("D", "B", 0.05) && ccc_ok("D", "B") &&
             all(d_time <= 60, na.rm = TRUE) && all(d_bytes <= 150e6, na.rm = TRUE)
  b_ok    <- el("B") && wins_on("B", "A", 0.05) >= 2 && !worse_by("B", "A", 0.05) && ccc_ok("B", "A")
  if (d_ok) {
    verdict <- "D wins: fit(local = 'mbl') over a compressed library."
  } else if (b_ok) {
    verdict <- "B ships: fit(local = TRUE) with GMM clusters and one config per property."
    if (status[["E"]] == "eligible" && wins_on("E", "B", 0.02) >= 2 &&
        all(get("E", "mean_width") <= 1.05 * get("B", "mean_width"), na.rm = TRUE))
      verdict <- paste(verdict, "E replaces hard assignment (posterior-weighted blend).")
    if (status[["C"]] == "eligible" && wins_on("C", "B", 0.05) >= 2)
      verdict <- paste(verdict, "C replaces B: per-cluster config selection (evaluate(local = TRUE)).")
  } else {
    verdict <- "A ships: global per-property models + reliability columns; locality deferred to v1.1."
  }
}
lines <- c(lines, "## Verdict", "", verdict, "",
           "## Rule inputs", "",
           "| strategy | rpd clay/oc/ph | ccc | coverage | width | predict s/100 | artifact MB |", "|---|---|---|---|---|---|---|")
for (s in c("A", "B", "C", "D", "E")) if (have(s)) lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |", s,
  paste(sprintf("%.3f", get(s, "rpd")), collapse = "/"), paste(sprintf("%.3f", get(s, "ccc")), collapse = "/"),
  paste(sprintf("%.3f", get(s, "coverage_90")), collapse = "/"), paste(sprintf("%.2f", get(s, "mean_width")), collapse = "/"),
  paste(sprintf("%.1f", get(s, "predict_secs_per_100")), collapse = "/"), paste(sprintf("%.0f", get(s, "artifact_bytes") / 1e6), collapse = "/")))
writeLines(lines, file.path(RESULTS_DIR, "verdict.md"))
cat("\n", paste(lines, collapse = "\n"), "\n")
