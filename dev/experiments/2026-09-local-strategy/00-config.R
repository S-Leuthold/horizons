## ===========================================================================
## 2026-09 local-strategy experiment — shared configuration
## ===========================================================================
##
## Sourced by every script in this directory. Nothing here runs anything.
## Paths are absolute on the box; override with the env vars below to run
## elsewhere. See README.md for the protocol and the pre-registered decision
## rule, and ../../../../dev/specs/v1-refactor/library-design.md for the design
## this experiment gates.

## ---------------------------------------------------------------------------
## Paths
## ---------------------------------------------------------------------------

KSSL_ROOT      <- Sys.getenv("HORIZONS_KSSL_ROOT",
                             "/data/workshop/projects/horizons/data/kssl")
RAW_DIR        <- file.path(KSSL_ROOT, "raw")
SNAPSHOT_DIR   <- file.path(KSSL_ROOT, "snapshot", "kssl_v1.2")

PKG_DIR        <- Sys.getenv("HORIZONS_PKG_DIR",
                             "/data/workshop/projects/horizons/package")
EXP_DIR        <- file.path(PKG_DIR, "dev", "experiments", "2026-09-local-strategy")
RESULTS_DIR    <- file.path(EXP_DIR, "results")
CHECKPOINT_DIR <- file.path(RESULTS_DIR, "checkpoints")
SPLITS_DIR     <- file.path(RESULTS_DIR, "splits")
FIGURES_DIR    <- file.path(RESULTS_DIR, "figures")
LOG_DIR        <- file.path(RESULTS_DIR, "logs")

## ---------------------------------------------------------------------------
## OSSL v1.2 sources (verified by HTTP HEAD on 2026-09-11)
## ---------------------------------------------------------------------------

OSSL_VERSION  <- "v1.2"
OSSL_BASE_URL <- "https://storage.googleapis.com/soilspec4gg-public/"
OSSL_LICENSE  <- "CC-BY-4.0"
OSSL_CITATION <- paste0(
  "Safanelli, J.L., Hengl, T., Parente, L.L., et al. (2025). ",
  "Open Soil Spectral Library (OSSL). PLOS ONE 20(1): e0296545. ",
  "https://doi.org/10.1371/journal.pone.0296545"
)

OSSL_SOURCES <- data.frame(
  table         = c("mir", "lab", "site"),
  file          = c("ossl_mir_L0_v1.2.csv.gz",
                    "ossl_soillab_L1_v1.2.csv.gz",
                    "ossl_soilsite_L0_v1.2.csv.gz"),
  bytes         = c(420088016, 9597417, 6371319),
  md5           = c("64f8cdfcc28d861f10b608671e4f0421",
                    "e24b94605b0061add400cf3b617f1f18",
                    "e8c640a9e7b6f15c2dec0a7d2e445716"),
  last_modified = rep("2023-05-04", 3),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------------
## The KSSL subset definition (this IS the registered source "kssl")
## ---------------------------------------------------------------------------

KSSL_FILTERS <- list(
  id_col              = "id.layer_uuid_txt",
  dataset_col         = "dataset.code_ascii_txt",
  dataset_value       = "KSSL.SSL",
  instrument_col      = "scan.mir.model.name_utf8_txt",
  instrument_value    = "Bruker Vertex 70 with HTS-XT accessory",
  depth_col           = "layer.upper.depth_usda_cm",
  depth_max_exclusive = 30,
  spectral_pattern    = "^scan_mir\\.([0-9]+)_abs$",
  wn_min              = 600L,
  wn_max              = 4000L,
  wn_step             = 2L
)

## Short property name -> OSSL v1.2 soillab L1 column. Units are OSSL's own:
## weight % for texture / carbon / nitrogen / Fe / Al, unitless pH, cmolc/kg
## for CEC and cations. The old branch's "target_unit = g/kg" metadata was
## wrong. NOTHING is converted in the snapshot.
PROPERTY_MAP <- data.frame(
  property  = c("clay", "sand", "silt", "total_carbon", "oc", "carbonate",
                "total_nitrogen", "ph", "cec", "calcium", "magnesium",
                "potassium", "sodium", "iron_total", "aluminum_total"),
  ossl_name = c("clay.tot_usda.a334_w.pct", "sand.tot_usda.c60_w.pct",
                "silt.tot_usda.c62_w.pct", "c.tot_usda.a622_w.pct",
                "oc_usda.c729_w.pct", "caco3_usda.a54_w.pct",
                "n.tot_usda.a623_w.pct", "ph.h2o_usda.a268_index",
                "cec_usda.a723_cmolc.kg", "ca.ext_usda.a722_cmolc.kg",
                "mg.ext_usda.a724_cmolc.kg", "k.ext_usda.a725_cmolc.kg",
                "na.ext_usda.a726_cmolc.kg", "fe.dith_usda.a66_w.pct",
                "al.dith_usda.a65_w.pct"),
  unit      = c(rep("% w/w", 7), "pH", rep("cmolc/kg", 5), "% w/w", "% w/w"),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------------
## Experiment design (pre-registered; see README.md before changing)
## ---------------------------------------------------------------------------

SEED <- 307L

EXP_PROPERTIES <- data.frame(
  property       = c("clay", "oc", "ph"),
  transformation = c("none", "log", "none"),
  lower          = c(0, 0, 0),      # rows outside [lower, upper] are
  upper          = c(100, Inf, 14), # physically impossible and dropped
  stringsAsFactors = FALSE
)

## Fixed, small config set shared by every strategy. Grid-only tuning: this
## is a comparison of STRATEGIES, not of tuned models.
EXP_CONFIG <- list(
  models              = c("cubist", "rf"),
  preprocessing       = c("snv", "snv_deriv1"),
  feature_selection   = "none",
  cv_folds            = 5L,
  grid_size           = 5L,
  bayesian_iter       = 0L,
  final_bayesian_iter = 0L
)

## Splits per property: stratified holdout, then an external calibration set
## carved from the training pool BEFORE any clustering (Mondrian condition).
EXP_SPLIT <- list(test_prop = 0.20, calib_prop = 0.15)

## Local (clustered) strategies B / C / E.
EXP_LOCAL <- list(
  k_range            = 5:11,
  min_cluster_n      = 300L,   # smaller clusters fall back to the global model
  pool_below_n       = 200L,   # per-cluster conformal margin pooled below this
  variance_threshold = 0.99,
  pca_max_comp       = 100L,
  sg_m = 1L, sg_p = 2L, sg_w = 11L,
  water_bands        = list(oh_bending    = c(1580, 1720),   # v1 standardize()
                            oh_stretching = c(3100, 3700))   # definition
)

## Memory-based learning, strategy D (resemble::mbl).
EXP_MBL <- list(k = c(50L, 100L, 200L, 400L), pls_c = c(5L, 20L),
                diss_method = "pca")

UQ_LEVEL   <- 0.90
STRATEGIES <- c("A_global", "B_gmm_oneconfig", "C_gmm_perconfig", "D_mbl",
                "E_soft")

## Pre-registered fallback if the pilot shows a Cubist fold > ~10 min at
## 2 cm-1: resample every strategy's input to this step (cm-1). NULL = off.
EXPERIMENT_RESAMPLE <- NULL

## ---------------------------------------------------------------------------
## Helpers
## ---------------------------------------------------------------------------

exp_dirs <- function() {
  for (d in c(RESULTS_DIR, CHECKPOINT_DIR, SPLITS_DIR, FIGURES_DIR, LOG_DIR)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

snapshot_path <- function(table = c("spectra", "lab", "site")) {
  table <- match.arg(table)
  file.path(SNAPSHOT_DIR, paste0(table, ".parquet"))
}
