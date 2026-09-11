## ===========================================================================
## 01 — Build the KSSL subset of OSSL v1.2 as a parquet snapshot
## ===========================================================================
##
## Input : the three OSSL v1.2 CSV.gz files in RAW_DIR (00-config.R), already
##         downloaded and MD5-verified.
## Output: SNAPSHOT_DIR/spectra.parquet   sample_id + wn_600 … wn_4000 (1701)
##         SNAPSHOT_DIR/lab.parquet       sample_id + 15 mapped properties
##         SNAPSHOT_DIR/site.parquet      sample_id + upper_depth_cm
##         SNAPSHOT_DIR/manifest.json     provenance, filters, counts, hashes
## Run   : Rscript dev/experiments/2026-09-local-strategy/01-build-kssl-snapshot.R [--force]
##
## This script is the operational definition of the registered source "kssl":
## KSSL samples scanned on the Bruker Vertex 70 with HTS-XT accessory, surface
## layers (upper depth < 30 cm), on the 600–4000 cm-1 @ 2 cm-1 grid, with any
## row whose spectrum has a non-finite value dropped. Lab values are OSSL's
## own units; nothing is converted. It is idempotent unless --force.

suppressPackageStartupMessages(library(data.table))

## ---------------------------------------------------------------------------
## Setup
## ---------------------------------------------------------------------------

args        <- commandArgs(trailingOnly = TRUE)
force       <- "--force" %in% args
file_arg    <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(file_arg)) normalizePath(sub("^--file=", "", file_arg[1])) else NA_character_
exp_dir     <- if (!is.na(script_path)) dirname(script_path) else getwd()

source(file.path(exp_dir, "00-config.R"))

for (p in c("arrow", "jsonlite", "digest")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' is required.")
}

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", sprintf(...), "\n")
t0     <- Sys.time()
counts <- list()
F      <- KSSL_FILTERS

manifest_path <- file.path(SNAPSHOT_DIR, "manifest.json")
if (file.exists(manifest_path) && !force) {
  msg("Snapshot already exists at %s (use --force to rebuild).", SNAPSHOT_DIR)
  quit(save = "no")
}
dir.create(SNAPSHOT_DIR, recursive = TRUE, showWarnings = FALSE)

## ---------------------------------------------------------------------------
## Step 1: verify raw inputs against the published MD5s
## ---------------------------------------------------------------------------

raw <- setNames(file.path(RAW_DIR, OSSL_SOURCES$file), OSSL_SOURCES$table)
if (!all(file.exists(raw))) {
  stop("Missing raw files: ", paste(raw[!file.exists(raw)], collapse = ", "))
}
md5_actual <- unname(tools::md5sum(raw[OSSL_SOURCES$table]))
if (any(md5_actual != OSSL_SOURCES$md5)) {
  stop("MD5 mismatch: ", paste(OSSL_SOURCES$file[md5_actual != OSSL_SOURCES$md5], collapse = ", "))
}
msg("Raw inputs verified (3 files, MD5 match).")

## ---------------------------------------------------------------------------
## Step 2: site table -> ids of surface layers
## ---------------------------------------------------------------------------

site <- fread(raw[["site"]], select = c(F$id_col, F$depth_col), showProgress = FALSE)
counts$site_rows <- nrow(site)
site <- site[!is.na(get(F$depth_col)) & get(F$depth_col) < F$depth_max_exclusive]
site <- unique(site, by = F$id_col)
counts$site_surface_layers <- nrow(site)
msg("Site: %d rows -> %d unique surface layers (upper depth < %s cm).",
    counts$site_rows, counts$site_surface_layers, F$depth_max_exclusive)

## ---------------------------------------------------------------------------
## Step 3: MIR table -> KSSL, Vertex 70, surface, complete spectra
## ---------------------------------------------------------------------------

hdr       <- names(fread(raw[["mir"]], nrows = 0))
spec_cols <- grep(F$spectral_pattern, hdr, value = TRUE)
need      <- c(F$id_col, F$dataset_col, F$instrument_col)
missing   <- setdiff(need, hdr)
if (length(missing)) {
  stop("MIR file lacks expected columns: ", paste(missing, collapse = ", "),
       "\nNon-spectral columns present: ", paste(setdiff(hdr, spec_cols), collapse = ", "))
}
msg("MIR header: %d columns, %d spectral.", length(hdr), length(spec_cols))

## Grid check on the header before reading the body.
wn   <- as.integer(sub(F$spectral_pattern, "\\1", spec_cols))
keep <- wn >= F$wn_min & wn <= F$wn_max
spec_cols <- spec_cols[keep]; wn <- wn[keep]
ord <- order(wn); spec_cols <- spec_cols[ord]; wn <- wn[ord]
expected <- seq(F$wn_min, F$wn_max, by = F$wn_step)
if (!identical(as.integer(wn), as.integer(expected))) {
  stop("Wavenumber grid is not ", F$wn_min, "..", F$wn_max, " by ", F$wn_step,
       ": found ", length(wn), " columns, range ", min(wn), "-", max(wn),
       ", step(s) ", paste(unique(diff(wn)), collapse = "/"))
}
msg("Grid OK: %d wavenumbers, %d..%d step %d.", length(wn), min(wn), max(wn), F$wn_step)

mir <- fread(raw[["mir"]], select = c(need, spec_cols), showProgress = FALSE)
counts$mir_rows <- nrow(mir)

## Record the instrument strings seen among KSSL rows before filtering on one.
kssl_instruments <- mir[get(F$dataset_col) == F$dataset_value,
                        .N, by = c(F$instrument_col)][order(-N)]
setnames(kssl_instruments, c("instrument", "n"))

mir <- mir[get(F$dataset_col) == F$dataset_value]
counts$mir_kssl <- nrow(mir)
mir <- mir[get(F$instrument_col) == F$instrument_value]
counts$mir_kssl_vertex70 <- nrow(mir)
msg("MIR: %d rows -> %d KSSL -> %d Vertex 70.", counts$mir_rows, counts$mir_kssl, counts$mir_kssl_vertex70)

mir <- mir[get(F$id_col) %in% site[[F$id_col]]]
counts$mir_surface <- nrow(mir)
mir <- unique(mir, by = F$id_col)
counts$mir_surface_unique <- nrow(mir)
msg("MIR: -> %d surface -> %d unique ids.", counts$mir_surface, counts$mir_surface_unique)

## Complete spectra only (the old branch's NA filter was dead code).
spec_mat <- as.matrix(mir[, ..spec_cols])
complete <- rowSums(!is.finite(spec_mat)) == 0L
rm(spec_mat)
counts$mir_dropped_incomplete <- sum(!complete)
mir <- mir[complete]
counts$mir_complete <- nrow(mir)
msg("MIR: -> %d with complete spectra (%d dropped).", counts$mir_complete, counts$mir_dropped_incomplete)

## ---------------------------------------------------------------------------
## Step 4: lab table -> mapped properties for the retained ids
## ---------------------------------------------------------------------------

lab_hdr <- names(fread(raw[["lab"]], nrows = 0))
have    <- PROPERTY_MAP$ossl_name %in% lab_hdr
if (!all(have)) {
  warning("Lab file lacks: ", paste(PROPERTY_MAP$ossl_name[!have], collapse = ", "))
}
pm  <- PROPERTY_MAP[have, ]
lab <- fread(raw[["lab"]], select = c(F$id_col, pm$ossl_name), showProgress = FALSE)
counts$lab_rows <- nrow(lab)
lab <- lab[get(F$id_col) %in% mir[[F$id_col]]]
lab <- unique(lab, by = F$id_col)
setnames(lab, old = c(F$id_col, pm$ossl_name), new = c("sample_id", pm$property))
counts$lab_matched <- nrow(lab)
msg("Lab: %d rows -> %d matched to retained spectra.", counts$lab_rows, counts$lab_matched)

## ---------------------------------------------------------------------------
## Step 5: inner join on id, assemble the three output tables
## ---------------------------------------------------------------------------

ids <- sort(intersect(mir[[F$id_col]], lab$sample_id))
counts$final_rows <- length(ids)

mir <- mir[get(F$id_col) %in% ids]
setorderv(mir, F$id_col)
spectra <- data.table(sample_id = mir[[F$id_col]])
spectra <- cbind(spectra, mir[, ..spec_cols])
setnames(spectra, old = spec_cols, new = paste0("wn_", wn))
rm(mir)

lab <- lab[sample_id %in% ids][order(sample_id)]
setcolorder(lab, c("sample_id", pm$property))

site_out <- site[get(F$id_col) %in% ids]
setnames(site_out, c("sample_id", "upper_depth_cm"))
site_out <- site_out[order(sample_id)]

stopifnot(identical(spectra$sample_id, lab$sample_id),
          identical(spectra$sample_id, site_out$sample_id),
          ncol(spectra) == 1L + length(wn),
          !anyNA(spectra))

prop_n <- vapply(pm$property, function(p) sum(!is.na(lab[[p]])), integer(1))
msg("Final: %d samples x %d wavenumbers. Non-NA per property: %s",
    counts$final_rows, length(wn),
    paste(sprintf("%s=%d", names(prop_n), prop_n), collapse = ", "))

## ---------------------------------------------------------------------------
## Step 6: write parquet + manifest
## ---------------------------------------------------------------------------

write_pq <- function(dt, path) {
  ## Write a plain data.frame so readers get a tibble back, not a data.table
  ## (arrow preserves the R class attribute in the parquet metadata).
  df <- as.data.frame(dt)
  ok <- tryCatch({ arrow::write_parquet(df, path, compression = "zstd"); "zstd" },
                 error = function(e) { arrow::write_parquet(df, path); "default" })
  list(path = path, bytes = file.size(path),
       sha256 = digest::digest(file = path, algo = "sha256"), compression = ok)
}

out <- list(
  spectra = write_pq(spectra, snapshot_path("spectra")),
  lab     = write_pq(lab,     snapshot_path("lab")),
  site    = write_pq(site_out, snapshot_path("site"))
)
msg("Wrote parquet: spectra %.1f MB, lab %.2f MB, site %.2f MB.",
    out$spectra$bytes / 1e6, out$lab$bytes / 1e6, out$site$bytes / 1e6)

sources <- OSSL_SOURCES
sources$url          <- paste0(OSSL_BASE_URL, sources$file)
sources$md5_verified <- TRUE

manifest <- list(
  name             = "kssl",
  snapshot_version = "kssl_v1.2",
  built_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  description      = paste0(
    "KSSL subset of OSSL ", OSSL_VERSION, ": dataset ", F$dataset_value,
    ", instrument '", F$instrument_value, "', upper depth < ",
    F$depth_max_exclusive, " cm, complete spectra on the ", F$wn_min, "-",
    F$wn_max, " cm-1 @ ", F$wn_step, " cm-1 grid. Lab values in OSSL-native units."
  ),
  source           = list(provider = "OSSL", version = OSSL_VERSION,
                          base_url = OSSL_BASE_URL, files = sources),
  license          = OSSL_LICENSE,
  citation         = OSSL_CITATION,
  filters          = F,
  kssl_instruments_seen = kssl_instruments,
  counts           = counts,
  n_rows           = counts$final_rows,
  grid             = list(min = F$wn_min, max = F$wn_max, step = F$wn_step,
                          n = length(wn), column_prefix = "wn_", order = "increasing"),
  properties       = cbind(pm, n_non_na = unname(prop_n)),
  units_note       = paste(
    "Values are OSSL-native: weight % for texture, carbon, nitrogen, Fe, Al;",
    "pH unitless; cmolc/kg for CEC and cations. No conversion applied."),
  outputs          = out,
  script           = list(path = script_path,
                          sha256 = if (!is.na(script_path)) digest::digest(file = script_path, algo = "sha256") else NA),
  session          = list(R = R.version.string, platform = R.version$platform,
                          data.table = as.character(packageVersion("data.table")),
                          arrow = as.character(packageVersion("arrow"))),
  runtime_secs     = as.numeric(difftime(Sys.time(), t0, units = "secs"))
)

jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE,
                     digits = NA, null = "null", na = "null")
msg("Manifest written: %s", manifest_path)
msg("Done in %.1f min.", manifest$runtime_secs / 60)
