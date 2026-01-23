# dev/test-pipeline.R
# Interactive testing script for v1 refactor pipeline
# Run line-by-line in RStudio to test each stage

## =============================================================================
## Setup
## =============================================================================

devtools::load_all()


## =============================================================================
## 1. spectra() — Load spectral data
## =============================================================================

## From OPUS files -------------------------------------------------------------

hd <- spectra("dev/test-data/opus", type = "opus")
hd

## Inspect the object ----------------------------------------------------------

print(hd)
summary(hd)

## Look at the data structure --------------------------------------------------

hd$data$analysis
hd$data$role_map
hd$data$n_predictors

## Check provenance ------------------------------------------------------------

hd$provenance


## =============================================================================
## 2. standardize() — Resample, trim, baseline
## =============================================================================

## Basic standardization (defaults: resample=2, trim=600-4000) -----------------

hd_std <- hd |>
  standardize()

hd_std

## Check what changed ----------------------------------------------------------

hd$data$n_predictors
hd_std$data$n_predictors

## Full standardization with water removal and baseline ------------------------

hd_full <- hd |>
  standardize(
    remove_water = TRUE,
    baseline     = TRUE
  )

hd_full

## Check provenance ------------------------------------------------------------

hd_full$provenance$standardization


## =============================================================================
## 3. parse_ids() — Extract sample IDs from file paths [NOT YET IMPLEMENTED]
## =============================================================================

# hd_parsed <- hd_std |>
#   parse_ids(pattern = "AONR-F_(S\\d+-\\d+)_")
#
# hd_parsed


## =============================================================================
## 4. average() — Average replicates [NOT YET IMPLEMENTED]
## =============================================================================

# hd_avg <- hd_parsed |>
#   average(by = "sample_id", method = "mean")
#
# hd_avg


## =============================================================================
## 5. add_response() — Add outcome variable [NOT YET IMPLEMENTED]
## =============================================================================

# From CSV file
# hd_with_response <- hd_avg |>
#   add_response("path/to/lab_data.csv", by = "sample_id", variable = "SOC")

# From OSSL
# hd_with_response <- hd_avg |>
#   add_response(ossl("pH", "clay"), by = "sample_id")


## =============================================================================
## 6. add_covariates() — Add environmental predictors [NOT YET IMPLEMENTED]
## =============================================================================

# hd_with_covars <- hd_with_response |>
#   add_covariates(soil = TRUE, climate = TRUE)


## =============================================================================
## 7. configure() — Set up model configurations [NOT YET IMPLEMENTED]
## =============================================================================

# hd_configured <- hd_with_response |>
#   configure(
#     models         = c("cubist", "ranger"),
#     preprocessing  = c("raw", "snv"),
#     transformations = "none"
#   )


## =============================================================================
## 8. validate() — Data quality checks [NOT YET IMPLEMENTED]
## =============================================================================

# hd_validated <- hd_configured |>
#   validate(remove_outliers = FALSE)


## =============================================================================
## 9. evaluate() — Train and evaluate models [NOT YET IMPLEMENTED]
## =============================================================================

# hd_eval <- hd_validated |>
#   evaluate(backend = "local", n_cores = 4)


## =============================================================================
## Accessors — Extract data from object
## =============================================================================

## These should work now -------------------------------------------------------

# get_data(hd_std)
# get_spectra(hd_std)
# get_spectra(hd_std, as_matrix = TRUE)


## =============================================================================
## Predicates — Check object state
## =============================================================================

## These should work now -------------------------------------------------------

# has_outcome(hd_std)
# is_validated(hd_std)
