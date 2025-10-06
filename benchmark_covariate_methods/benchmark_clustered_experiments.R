#' Benchmark Clustered Covariate Prediction - Experimental Parameter Testing
#'
#' Tests all combinations of experimental parameters to identify optimal settings:
#' - derivative_order: 0 (smooth), 1 (1st derivative), 2 (2nd derivative)
#' - clustering_method: "kmeans", "ward"
#' - use_mahalanobis: TRUE, FALSE
#'
#' Total combinations: 3 × 2 × 2 = 12 tests

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(horizons)

## =============================================================================
## Setup: Load test data
## =============================================================================

rm(list = ls())

devtools::load_all()

## Load your test data here --------------------------------------------------

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

## Define covariates to test -------------------------------------------------

covariates_to_test <- c("clay", "ph")

## =============================================================================
## Define parameter grid
## =============================================================================

param_grid <- tidyr::expand_grid(derivative_order   = c(0, 1, 2),
                                 clustering_method  = c("kmeans", "ward"),
                                 use_mahalanobis    = c(TRUE, FALSE)) %>%
  dplyr::mutate(test_id = paste0("Test_", row_number()),
                .before = 1)

## Show what we're testing ---------------------------------------------------

cli::cli_text("Running {nrow(param_grid)} parameter combinations:")
print(param_grid)

## =============================================================================
## Run benchmark for all combinations
## =============================================================================

results <- vector("list", nrow(param_grid))

for (i in 1:nrow(param_grid)) {

  params <- param_grid[i, ]

  cli::cli_text("")
  #cli::cli_text("=" %R>% strrep(70))
  cli::cli_text("{params$test_id}: deriv={params$derivative_order}, cluster={params$clustering_method}, dist={if(params$use_mahalanobis) 'Mahal' else 'Euclid'}")
  #cli::cli_text("=" %R>% strrep(70))

  ## Run prediction with these parameters ------------------------------------

  start_time <- Sys.time()

  result <- tryCatch({

    predict_soil_covariates_clustered(
      input_data         = test_data,
      covariates         = covariates_to_test,
      prop               = 0.85,
      variance_threshold = 0.985,
      coverage           = 0.8,
      max_clusters       = 10,
      bayesian_iter      = 3,
      allow_par          = TRUE,
      n_workers          = 8,
      refresh            = FALSE,
      verbose            = TRUE,
      ## Experimental parameters from grid
      derivative_order   = params$derivative_order,
      clustering_method  = params$clustering_method,
      use_mahalanobis    = params$use_mahalanobis
    )

  }, error = function(e) {

    cli::cli_alert_danger("FAILED: {e$message}")
    NULL

  })

  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  ## Store results -----------------------------------------------------------

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
## Combine and analyze results
## =============================================================================

all_results <- dplyr::bind_rows(results)

## Save raw results ----------------------------------------------------------

saveRDS(all_results, "benchmark_results_clustered_experiments.rds")
write.csv(all_results, "benchmark_results_clustered_experiments.csv", row.names = FALSE)

## =============================================================================
## Performance comparison
## =============================================================================

cli::cli_text("")

cli::cli_text("PERFORMANCE SUMMARY")


## Rank by R² for each covariate ---------------------------------------------

for (cov in covariates_to_test) {

  cli::cli_text("")
  cli::cli_text("Covariate: {toupper(cov)}")

  all_results %>%
    dplyr::filter(covariate == cov) %>%
    dplyr::arrange(desc(rsq)) %>%
    dplyr::select(test_id, rsq, rmse, rpd, derivative_order,
                  clustering_method, use_mahalanobis, elapsed_time) %>%
    print()

}

## =============================================================================
## Visualize results
## =============================================================================

## Plot 1: R² by parameter combination ---------------------------------------

p1 <- all_results %>%
  dplyr::mutate(
    config = paste0(
      "Deriv:", derivative_order, " | ",
      clustering_method, " | ",
      if_else(use_mahalanobis, "Maha", "Eucl")
    )
  ) %>%
  ggplot(aes(x = reorder(config, rsq), y = rsq, fill = covariate)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "R² Performance by Configuration",
    x = "Configuration",
    y = "R²",
    fill = "Covariate"
  ) +
  theme_minimal()

print(p1)
ggsave("benchmark_rsq_comparison.png", p1, width = 10, height = 6)

## Plot 2: Trade-off between accuracy and speed ------------------------------

p2 <- all_results %>%
  ggplot(aes(x = elapsed_time, y = rsq, color = covariate)) +
  geom_point(size = 3) +
  geom_text(aes(label = test_id), vjust = -0.5, size = 2.5) +
  labs(
    title = "Accuracy vs Speed Trade-off",
    x = "Elapsed Time (seconds)",
    y = "R²",
    color = "Covariate"
  ) +
  theme_minimal()

print(p2)
ggsave("benchmark_speed_accuracy_tradeoff.png", p2, width = 10, height = 6)

## =============================================================================
## Identify winner
## =============================================================================

cli::cli_text("")
cli::cli_text("=" %R>% strrep(70))
cli::cli_text("WINNER IDENTIFICATION")
cli::cli_text("=" %R>% strrep(70))

## Best overall R² (averaged across covariates) ------------------------------

winner <- all_results %>%
  dplyr::group_by(test_id, derivative_order, clustering_method, use_mahalanobis) %>%
  dplyr::summarise(
    mean_rsq = mean(rsq, na.rm = TRUE),
    mean_rmse = mean(rmse, na.rm = TRUE),
    mean_rpd = mean(rpd, na.rm = TRUE),
    total_time = mean(elapsed_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(desc(mean_rsq))

cli::cli_text("")
cli::cli_text("Top 5 configurations (by mean R²):")
print(head(winner, 5))

cli::cli_text("")
cli::cli_text("WINNER: {winner$test_id[1]}")
cli::cli_text("  Derivative order: {winner$derivative_order[1]}")
cli::cli_text("  Clustering: {winner$clustering_method[1]}")
cli::cli_text("  Distance: {if (winner$use_mahalanobis[1]) 'Mahalanobis' else 'Euclidean'}")
cli::cli_text("  Mean R²: {round(winner$mean_rsq[1], 4)}")
cli::cli_text("  Mean RMSE: {round(winner$mean_rmse[1], 3)}")
cli::cli_text("  Time: {round(winner$total_time[1], 1)}s")

