#' Compare V1 (Unknown-Centric) vs V2 (OSSL-Centric) PCA Approaches
#'
#' Direct head-to-head comparison with optimal parameters from previous benchmark:
#' - derivative_order = 1 (1st derivative - clear winner)
#' - clustering_method = "kmeans" (slightly better than Ward's)
#' - use_mahalanobis = TRUE (slightly better than Euclidean)

library(dplyr)
library(tidyr)
library(ggplot2)
library(horizons)

## =============================================================================
## Setup
## =============================================================================

rm(list = ls())

devtools::load_all()

## Load test data -----------------------------------------------------------

read_spectra(source       = "opus",
             spectra_path = "../../AI-CLIMATE/data/processed/FFAR/opus_files",
             spectra_type = "MIR",
             verbose      = TRUE) %>%
  preprocess_spectra(spectra_data      = .,
                     resample_interval = 2,
                     baseline_method   = "none") %>%
  create_dataset(response_data      = "../../AI-CLIMATE/data/processed/FFAR/fraction_data.csv",
                 parse_ids          = TRUE,
                 include_coords     = TRUE,
                 coord_columns      = c("Longitude", "Latitude"),
                 id_format          = "project_sampleid_fraction_scanid_wellid",
                 aggregate_by       = c("Project", "Sample_ID"),
                 response_variables = c("POM_C_g_kg"),
                 verbose            = TRUE) %>%
  finalize_dataset(dataset                  = .,
                   response_variable        = "POM_C_g_kg",
                   spectral_outlier_method  = "mahalanobis",
                   detect_response_outliers = TRUE,
                   spectral_cutoff          = 0.975,
                   response_cutoff          = 1.5,
                   remove_outliers          = FALSE,
                   enforce_positive         = TRUE,
                   verbose                  = TRUE) -> test_data

covariates_to_test <- c("clay", "ph")

## =============================================================================
## Shared parameters (optimized from previous benchmark)
## =============================================================================

shared_params <- list(
  prop                = 0.85,
  variance_threshold  = 0.985,
  coverage            = 0.8,
  max_clusters        = 10,
  bayesian_iter       = 10,
  allow_par           = TRUE,
  n_workers           = 8,
  refresh             = FALSE,
  verbose             = TRUE,
  ## Optimal experimental params
  derivative_order    = 1,     # WINNER from previous benchmark
  clustering_method   = "kmeans",
  use_mahalanobis     = TRUE,
  distance_percentile = 0.6
)

## =============================================================================
## Test V1: Unknown-Centric PCA
## =============================================================================

cli::cli_h1("Testing V1: Unknown-Centric PCA")
cli::cli_text("PCA trained on unknowns, OSSL projected to unknown space")
cli::cli_text("")

v1_start <- Sys.time()

v1_results <- tryCatch({

  predict_soil_covariates_clustered(
    input_data = test_data,
    covariates = covariates_to_test,
    prop                = shared_params$prop,
    variance_threshold  = shared_params$variance_threshold,
    coverage            = shared_params$coverage,
    max_clusters        = shared_params$max_clusters,
    bayesian_iter       = shared_params$bayesian_iter,
    allow_par           = shared_params$allow_par,
    n_workers           = shared_params$n_workers,
    refresh             = shared_params$refresh,
    verbose             = shared_params$verbose,
    derivative_order    = shared_params$derivative_order,
    clustering_method   = shared_params$clustering_method,
    use_mahalanobis     = shared_params$use_mahalanobis,
    distance_percentile = shared_params$distance_percentile
  )

}, error = function(e) {
  cli::cli_alert_danger("V1 FAILED: {e$message}")
  NULL
})

v1_time <- as.numeric(difftime(Sys.time(), v1_start, units = "secs"))

## =============================================================================
## Test V2: OSSL-Centric PCA
## =============================================================================

cli::cli_text("")
cli::cli_text("")
cli::cli_h1("Testing V2: OSSL-Centric PCA")
cli::cli_text("PCA trained on OSSL, unknowns projected to OSSL space")
cli::cli_text("")

v2_start <- Sys.time()

v2_results <- tryCatch({

  predict_soil_covariates_clustered_v2(
    input_data = test_data,
    covariates = covariates_to_test,
    prop                = shared_params$prop,
    variance_threshold  = shared_params$variance_threshold,
    coverage            = shared_params$coverage,
    max_clusters        = shared_params$max_clusters,
    bayesian_iter       = shared_params$bayesian_iter,
    allow_par           = shared_params$allow_par,
    n_workers           = shared_params$n_workers,
    refresh             = shared_params$refresh,
    verbose             = shared_params$verbose,
    derivative_order    = shared_params$derivative_order,
    clustering_method   = shared_params$clustering_method,
    use_mahalanobis     = shared_params$use_mahalanobis,
    distance_percentile = shared_params$distance_percentile
  )

}, error = function(e) {
  cli::cli_alert_danger("V2 FAILED: {e$message}")
  NULL
})

v2_time <- as.numeric(difftime(Sys.time(), v2_start, units = "secs"))

## =============================================================================
## Compare Results
## =============================================================================

cli::cli_text("")
cli::cli_text("")
cli::cli_h1("HEAD-TO-HEAD COMPARISON")

cli::cli_text("")
cli::cli_h2("Execution Time")
cli::cli_text("├─ V1 (Unknown-centric): {round(v1_time, 1)}s")
cli::cli_text("└─ V2 (OSSL-centric):    {round(v2_time, 1)}s")

if (!is.null(v1_results) && !is.null(v2_results)) {

  cli::cli_text("")
  cli::cli_h2("Validation Metrics")

  for (cov in covariates_to_test) {

    v1_metrics <- v1_results$validation_metrics %>% filter(covariate == cov)
    v2_metrics <- v2_results$validation_metrics %>% filter(covariate == cov)

    if (nrow(v1_metrics) > 0 && nrow(v2_metrics) > 0) {

      cli::cli_text("")
      cli::cli_text("├─ {toupper(cov)}")
      cli::cli_text("│  ├─ V1: R² = {round(v1_metrics$rsq, 3)} | RMSE = {round(v1_metrics$rmse, 2)} | RPD = {round(v1_metrics$rpd, 2)}")
      cli::cli_text("│  └─ V2: R² = {round(v2_metrics$rsq, 3)} | RMSE = {round(v2_metrics$rmse, 2)} | RPD = {round(v2_metrics$rpd, 2)}")

      ## Show winner
      rsq_diff <- v2_metrics$rsq - v1_metrics$rsq

      if (rsq_diff > 0.01) {
        cli::cli_text("│      → {cli::col_green('V2 WINS')} (ΔR² = +{round(rsq_diff, 3)})")
      } else if (rsq_diff < -0.01) {
        cli::cli_text("│      → {cli::col_yellow('V1 WINS')} (ΔR² = +{round(-rsq_diff, 3)})")
      } else {
        cli::cli_text("│      → TIE (ΔR² = {round(rsq_diff, 3)})")
      }

    }
  }

  ## Overall winner
  v1_mean_rsq <- mean(v1_results$validation_metrics$rsq)
  v2_mean_rsq <- mean(v2_results$validation_metrics$rsq)

  cli::cli_text("")
  cli::cli_h2("Overall Performance")
  cli::cli_text("├─ V1 Mean R²: {round(v1_mean_rsq, 4)}")
  cli::cli_text("└─ V2 Mean R²: {round(v2_mean_rsq, 4)}")

  if (v2_mean_rsq > v1_mean_rsq) {
    cli::cli_alert_success("WINNER: V2 (OSSL-centric PCA) by {round((v2_mean_rsq - v1_mean_rsq) * 100, 2)} percentage points")
  } else {
    cli::cli_alert_info("WINNER: V1 (Unknown-centric PCA) by {round((v1_mean_rsq - v2_mean_rsq) * 100, 2)} percentage points")
  }

  ## Save results
  comparison_results <- list(
    v1 = list(
      metrics = v1_results$validation_metrics,
      cluster_info = v1_results$cluster_info,
      time_sec = v1_time
    ),
    v2 = list(
      metrics = v2_results$validation_metrics,
      cluster_info = v2_results$cluster_info,
      time_sec = v2_time
    ),
    parameters = shared_params
  )

  saveRDS(comparison_results, "benchmark_results_v1_v2_comparison.rds")
  cli::cli_alert_success("Results saved to benchmark_results_v1_v2_comparison.rds")

} else {
  cli::cli_alert_danger("One or both methods failed - cannot compare")
}
