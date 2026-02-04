# dev/test-pipeline.R
# End-to-end testing script for v1 refactor pipeline
# Run line-by-line in RStudio to test each stage
#
# Uses OPUS test data in dev/test-data/opus/ (264 files, 66 unique samples)
# with real AONR lab data from the ai-leaf project.

## =============================================================================
## Setup
## =============================================================================

devtools::load_all("./")


## =============================================================================
## 1. spectra() — Load spectral data from OPUS files
## =============================================================================

hd <- spectra("dev/test-data/opus", type = "opus")
hd

## 264 files → 264 rows, ~3500 wavenumber columns
hd$data$n_rows
hd$data$n_predictors
hd$data$analysis$sample_id  # filenames at this point


## =============================================================================
## 2. standardize() — Resample to 2 cm-1, trim to 600-4000
## =============================================================================

hd_std <- hd |>
  standardize()

hd_std
hd_std$data$n_predictors  # fewer columns after resampling


## =============================================================================
## 3. parse_ids() — Extract sample IDs from OPUS filenames
## =============================================================================
##
## Filenames: AONR-F_S100-1_GroundBulk_S1_A2
## We want:   S100-1

hd_parsed <- hd_std |>
  parse_ids(patterns = c("AONR-F_", sampleid = "S\\d+-\\d+", "_.*"))

hd_parsed$data$analysis$sample_id  # should be S100-1, S100-2, etc.


## =============================================================================
## 4. average() — Average replicates (4 scans per sample)
## =============================================================================

hd_avg <- hd_parsed |>
  average(by = "sample_id")

hd_avg
hd_avg$data$analysis$sample_id  # 66 unique samples after averaging


## =============================================================================
## 5. add_response() — Join real lab data from AONR.csv
## =============================================================================
##
## AONR.csv has Sample_ID (S100-1 format), Bulk_C_g_kg, POXC, etc.
## Using named join: our sample_id → CSV's Sample_ID

aonr <- readr::read_csv("dev/test-data/AONR.csv", show_col_types = FALSE)

hd_resp <- hd_avg |>
  add_response(
    source   = aonr,
    variable = "Bulk_C_g_kg",
    by       = c("sample_id" = "Sample_ID")
  )

hd_resp
hd_resp$data$analysis$Bulk_C_g_kg  # real SOC values


## =============================================================================
## 6. configure() — Set up model configurations
## =============================================================================
##
## 66 samples — keep tuning minimal for speed.
## cv_folds = 2, grid_size = 2, no Bayesian.

hd_cfg <- hd_resp |>
  configure(
    models            = c("rf", "cubist"),
    preprocessing     = "raw",
    transformations   = "none",
    feature_selection = "none",
    cv_folds          = 2L,
    grid_size         = 2L,
    bayesian_iter     = 0L
  )

hd_cfg
hd_cfg$config$configs
hd_cfg$config$tuning


## =============================================================================
## 7. validate() — Data quality checks
## =============================================================================

hd_val <- hd_cfg |>
  validate(remove_outliers = FALSE)

hd_val
hd_val$validation$passed
hd_val$validation$checks


## =============================================================================
## 8. evaluate() — Train and evaluate models
## =============================================================================
##
## 66 samples is small but enough to get real metrics.
## RPD ~1.0 expected with raw spectra and minimal tuning.

hd_eval <- hd_val |>
  evaluate(
    metric  = "rpd",
    prune   = FALSE,
    seed    = 42L,
    verbose = TRUE
  )

hd_eval
hd_eval$evaluation$results
hd_eval$evaluation$best_config


## =============================================================================
## Inspect results
## =============================================================================

## Full results table
hd_eval$evaluation$results |>
  dplyr::select(config_id, status, rpd, rsq, rmse, runtime_secs)

## Best config details
best_id  <- hd_eval$evaluation$best_config
best_cfg <- hd_eval$config$configs |>
  dplyr::filter(config_id == best_id)
best_cfg

## Class chain
class(hd_eval)

## Split sizes
hd_eval$evaluation$n_train
hd_eval$evaluation$n_test


## =============================================================================
## Full pipeline — single piped call
## =============================================================================
##
## This is how a user would actually write it. Useful for checking
## that the tree output reads coherently as one continuous stream.

aonr <- readr::read_csv("dev/test-data/AONR.csv", show_col_types = FALSE)

result <- spectra("dev/test-data/opus", type = "opus") |>
  standardize() |>
  parse_ids(patterns = c("AONR-F_", sampleid = "S\\d+-\\d+", "_.*")) |>
  average(by = "sample_id") |>
  add_response(
    source   = aonr,
    variable = "Bulk_C_g_kg",
    by       = c("sample_id" = "Sample_ID")
  ) |>
  configure(
    models            = c("rf", "cubist"),
    preprocessing     = "raw",
    transformations   = "none",
    feature_selection = "none",
    cv_folds          = 2L,
    grid_size         = 2L,
    bayesian_iter     = 0L
  ) |>
  validate(remove_outliers = FALSE) |>
  evaluate(
    metric  = "rpd",
    prune   = FALSE,
    seed    = 42L,
    verbose = TRUE
  )

result
