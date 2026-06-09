## ===========================================================================
## Build the ANONYMIZED real-data ensemble test fixture (run ONCE).
## Output: tests/testthat/fixtures/ensemble_fit.rds — a small horizons_fit on
## REAL AONR MIR spectra + Bulk_C (genuine signal so the meta-learner is
## non-degenerate), but with all provenance stripped: generic sample IDs
## (sample_001...), no project labels, no coordinates, no source paths.
##
## Members span THREE transforms (log/sqrt/none) so the §9 back-transform
## double-application regression is testable (invisible under identity-only).
## ===========================================================================

devtools::load_all(quiet = TRUE)

OPUS_SUBSET <- "/tmp/aonr_subset"   # 400-file symlink subset staged earlier
REF_CSV     <- "/Users/samleuthold/Desktop/workshop/projects/ai-leaf/data/processed/AONR/soils_data.csv"
OUT         <- "tests/testthat/fixtures/ensemble_fit.rds"

## ---------------------------------------------------------------------------
## Ingest + join (same as dev-realdata-i1.R Stage 1)
## ---------------------------------------------------------------------------

hd <- spectra(OPUS_SUBSET, type = "opus")
hd <- parse_ids(hd, patterns = c(project = "AONR-[A-Z]", "_",
                                 sampleid = "S\\d+-\\d+", "_.*"),
                too_few = "keep_original")
hd <- average(hd, by = "sample_id", quality_check = FALSE, verbose = FALSE)

ref      <- readr::read_csv(REF_CSV, show_col_types = FALSE)
ref_resp <- ref[!is.na(ref$Bulk_C_g_kg), c("Sample_ID", "Bulk_C_g_kg")]
names(ref_resp) <- c("sample_id", "Bulk_C")

hd <- add_response(hd, ref_resp, variable = "Bulk_C", by = "sample_id")

## Keep only matched rows (those with a response), then ANONYMIZE: generic
## sample IDs, no traceable provenance. Do this on the INPUT object so the
## fitted fixture is born clean (no AONR ids threaded through fitted slots).

analysis <- hd$data$analysis
analysis <- analysis[!is.na(analysis$Bulk_C), ]
analysis$sample_id <- sprintf("sample_%03d", seq_len(nrow(analysis)))
hd$data$analysis <- analysis

## Scrub provenance of any AONR/coordinate trace.
hd$provenance$spectra_source <- "fixture"
hd$provenance$parse_ids      <- NULL
hd$provenance$add_response   <- NULL

cat("anonymized samples:", nrow(analysis),
    "| id examples:", paste(head(analysis$sample_id, 3), collapse=" | "), "\n")
cat("Bulk_C range:", paste(round(range(analysis$Bulk_C), 2), collapse=" - "), "\n")

## ---------------------------------------------------------------------------
## Configure (3 models x 3 transforms), evaluate, fit -> 3-member ensemble
## ---------------------------------------------------------------------------

## Trim predictors to keep the fixture small + fast: every 10th wavenumber.
wn_cols <- setdiff(names(hd$data$analysis), c("sample_id", "Bulk_C"))
keep_wn <- wn_cols[seq(1, length(wn_cols), by = 10)]
hd$data$analysis   <- hd$data$analysis[, c("sample_id", keep_wn, "Bulk_C")]
hd$data$role_map   <- hd$data$role_map[hd$data$role_map$variable %in%
                                        c("sample_id", keep_wn, "Bulk_C"), ]
hd$data$n_predictors <- length(keep_wn)
cat("trimmed predictors:", length(keep_wn), "\n")

hd <- configure(hd,
                models          = c("rf", "cubist", "plsr"),
                transformations = c("log", "sqrt", "none"),
                grid_size       = 5L,
                bayesian_iter   = 0L)

ev <- evaluate(hd, prune = FALSE, verbose = TRUE, seed = 42)
ft <- horizons::fit(ev, n_best = 3L, compute_uq = FALSE, verbose = TRUE, seed = 42L)

cat("\nfitted members:", paste(unique(ft$models$cv_predictions$config_id), collapse=", "), "\n")
cat("member transforms in configs:\n")
print(ft$config$configs[, c("config_id", "model", "transformation")])

## ---------------------------------------------------------------------------
## Trim fitted workflows? Keep them — ensemble() needs them to predict test_F.
## Just confirm no residual provenance leak, then save.
## ---------------------------------------------------------------------------

stopifnot(all(grepl("^sample_", ft$models$cv_predictions$sample_id %||% ft$models$row_index$sample_id)))

dir.create("tests/testthat/fixtures", showWarnings = FALSE, recursive = TRUE)
saveRDS(ft, OUT, compress = "xz")
cat("\nsaved fixture:", OUT, "| size:", round(file.size(OUT)/1e6, 2), "MB\n")
