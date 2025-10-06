#' Benchmark V2 (OSSL-Centric) - Full Parameter Grid
#'
#' Tests all 12 combinations to find optimal configuration for OSSL-centric approach

library(dplyr)
library(tidyr)
library(purrr)
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
## Define parameter grid
## =============================================================================

param_grid <- tidyr::expand_grid(
  derivative_order   = c(0, 1, 2),
  clustering_method  = c("kmeans", "ward"),
  use_mahalanobis    = c(TRUE, FALSE)
) %>%
  dplyr::mutate(test_id = paste0("V2_Test_", row_number()),
                .before = 1)

cli::cli_h1("V2 (OSSL-Centric) Parameter Grid Test")
cli::cli_text("Architecture: PCA on OSSL, unknowns project to OSSL space")
cli::cli_text("Per-covariate OSSL loading: ENABLED (maximum training data)")
cli::cli_text("Running {nrow(param_grid)} parameter combinations")
cli::cli_text("")
print(param_grid)

## =============================================================================
## Run benchmark
## =============================================================================

results <- vector("list", nrow(param_grid))

for (i in 1:nrow(param_grid)) {

  params <- param_grid[i, ]

  cli::cli_text("")
  cli::cli_text("{params$test_id}: deriv={params$derivative_order}, cluster={params$clustering_method}, dist={if(params$use_mahalanobis) 'Mahal' else 'Euclid'}")

  start_time <- Sys.time()

  result <- tryCatch({

    predict_soil_covariates_clustered_v2(
      input_data          = test_data,
      covariates          = covariates_to_test,
      prop                = 0.85,
      variance_threshold  = 0.985,
      coverage            = 0.8,
      max_clusters        = 10,
      bayesian_iter       = 10,
      allow_par           = TRUE,
      n_workers           = 8,
      refresh             = FALSE,
      verbose             = TRUE,
      derivative_order    = params$derivative_order,
      clustering_method   = params$clustering_method,
      use_mahalanobis     = params$use_mahalanobis,
      distance_percentile = 0.6
    )

  }, error = function(e) {
    cli::cli_alert_danger("FAILED: {e$message}")
    NULL
  })

  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (!is.null(result)) {

    results[[i]] <- result$validation_metrics %>%
      dplyr::mutate(
        test_id            = params$test_id,
        derivative_order   = params$derivative_order,
        clustering_method  = params$clustering_method,
        use_mahalanobis    = params$use_mahalanobis,
        n_clusters         = result$cluster_info$n_clusters,
        elapsed_time       = elapsed_time,
        .before            = 1
      )

  } else {

    results[[i]] <- tibble::tibble(
      test_id            = params$test_id,
      derivative_order   = params$derivative_order,
      clustering_method  = params$clustering_method,
      use_mahalanobis    = params$use_mahalanobis,
      status             = "FAILED",
      elapsed_time       = elapsed_time
    )

  }

  cli::cli_text("Completed in {round(elapsed_time, 1)}s")
}

## =============================================================================
## Analyze Results
## =============================================================================

{all_results_v2 <- dplyr::bind_rows(results)

saveRDS(all_results_v2, "benchmark_results_v2_grid.rds")
write.csv(all_results_v2, "benchmark_results_v2_grid.csv", row.names = FALSE)

cli::cli_text("")
cli::cli_h2("V2 PERFORMANCE SUMMARY")

for (cov in covariates_to_test) {

  cli::cli_text("")
  cli::cli_text("Covariate: {toupper(cov)}")

  all_results_v2 %>%
    dplyr::filter(covariate == cov) %>%
    dplyr::arrange(desc(rsq)) %>%
    dplyr::select(test_id, rsq, rmse, rpd, derivative_order,
                  clustering_method, use_mahalanobis, elapsed_time) %>%
    print()

}

## =============================================================================
## Compare V2 Winner to V1 Winner
## =============================================================================

cli::cli_text("")
cli::cli_h1("V1 vs V2 WINNER COMPARISON")

## Load V1 results from previous benchmark
v1_results <- readRDS("benchmark_results_clustered_experiments.rds")

## V1 Winner (from previous run)
v1_winner <- v1_results %>%
  dplyr::group_by(test_id, derivative_order, clustering_method, use_mahalanobis) %>%
  dplyr::summarise(mean_rsq = mean(rsq, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(mean_rsq)) %>%
  dplyr::slice(1)

cli::cli_text("")
cli::cli_h2("V1 Winner (Unknown-Centric PCA)")
cli::cli_text("├─ Config: {v1_winner$test_id}")
cli::cli_text("├─ Derivative: {v1_winner$derivative_order}")
cli::cli_text("├─ Clustering: {v1_winner$clustering_method}")
cli::cli_text("├─ Distance: {if(v1_winner$use_mahalanobis) 'Mahalanobis' else 'Euclidean'}")
cli::cli_text("└─ Mean R²: {round(v1_winner$mean_rsq, 4)}")

## V2 Winner
v2_winner <- all_results_v2 %>%
  dplyr::group_by(test_id, derivative_order, clustering_method, use_mahalanobis) %>%
  dplyr::summarise(mean_rsq = mean(rsq, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(mean_rsq)) %>%
  dplyr::slice(1)

cli::cli_text("")
cli::cli_h2("V2 Winner (OSSL-Centric PCA)")
cli::cli_text("├─ Config: {v2_winner$test_id}")
cli::cli_text("├─ Derivative: {v2_winner$derivative_order}")
cli::cli_text("├─ Clustering: {v2_winner$clustering_method}")
cli::cli_text("├─ Distance: {if(v2_winner$use_mahalanobis) 'Mahalanobis' else 'Euclidean'}")
cli::cli_text("└─ Mean R²: {round(v2_winner$mean_rsq, 4)}")

## Final verdict
rsq_diff <- v2_winner$mean_rsq - v1_winner$mean_rsq

cli::cli_text("")
cli::cli_h1("FINAL VERDICT")

if (rsq_diff > 0.01) {
  cli::cli_alert_success("V2 (OSSL-CENTRIC) WINS by {round(rsq_diff * 100, 2)} percentage points!")
  cli::cli_text("Architecture change improved performance significantly")
} else if (rsq_diff < -0.01) {
  cli::cli_alert_warning("V1 (UNKNOWN-CENTRIC) WINS by {round(-rsq_diff * 100, 2)} percentage points")
  cli::cli_text("OSSL-centric approach did not improve performance")
} else {
  cli::cli_text("TIE: Both approaches perform similarly (ΔR² = {round(rsq_diff, 4)})")
}

## Show per-covariate breakdown
cli::cli_text("")
cli::cli_h2("Per-Covariate Comparison")

for (cov in covariates_to_test) {

  v1_cov <- v1_results %>%
    filter(test_id == v1_winner$test_id, covariate == cov)

  v2_cov <- all_results_v2 %>%
    filter(test_id == v2_winner$test_id, covariate == cov)

  if (nrow(v1_cov) > 0 && nrow(v2_cov) > 0) {
    cli::cli_text("")
    cli::cli_text("├─ {toupper(cov)}")
    cli::cli_text("│  ├─ V1: R² = {round(v1_cov$rsq, 3)}")
    cli::cli_text("│  └─ V2: R² = {round(v2_cov$rsq, 3)} (Δ = {round(v2_cov$rsq - v1_cov$rsq, 3)})")
  }
}
}
