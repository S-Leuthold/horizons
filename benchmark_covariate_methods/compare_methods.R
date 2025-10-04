#===============================================================================
# Benchmark: Compare Covariate Prediction Methods
#
# Purpose: Head-to-head comparison of two approaches:
#   - OLD: Clustered local models (from main branch)
#   - NEW: Global model with Kennard-Stone (current branch)
#
# Usage:
#   source("benchmark_covariate_methods/compare_methods.R")
#   results <- compare_covariate_methods(my_spectra, covariates = c("clay", "ph"))
#===============================================================================


## Load required packages
library(dplyr)
library(tibble)
library(cli)
library(purrr)
library(tidyr)
library(stringr)
library(qs)
library(prospectr)
library(rsample)
library(FactoMineR)
library(furrr)
library(future)
library(progressr)
library(workflows)
library(parsnip)
library(recipes)
library(dials)
library(rules)
library(tune)
library(glue)
library(readr)
library(here)
library(cluster)
library(tidyselect)
library(rlang)

## Load both implementations
source("benchmark_covariate_methods/OLD_clustered_approach.R")  # Old method
devtools::load_all()  # New method from current package

## -----------------------------------------------------------------------------
## Comparison Function
## -----------------------------------------------------------------------------

compare_covariate_methods <- function(input_data,
                                     covariates,
                                     n_similar = 20000,
                                     verbose = TRUE) {

  if (verbose) {
    cli::cli_h1("Covariate Prediction Method Comparison")
    cli::cli_text("Testing {length(covariates)} covariate(s): {paste(covariates, collapse = ', ')}")
    cli::cli_text("Input: {nrow(input_data)} samples")
    cli::cli_text("")
  }

  ## ---------------------------------------------------------------------------
  ## Method 1: OLD Clustered Local Models
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_h2("Method 1: Clustered Local Models (OLD)")
    cli::cli_text("Strategy: K-means clusters + local Cubist models per cluster")
  }

  old_start <- Sys.time()
  old_results <- tryCatch({

    predict_covariates(
      covariates = covariates,
      input_data = input_data,
      verbose    = verbose,
      refresh    = TRUE,  # Force fresh prediction
      parallel   = FALSE
    )

  }, error = function(e) {
    cli::cli_alert_danger("OLD method failed: {e$message}")
    list(Predicted_Values = NULL, Evaluation_Statistics = NULL)
  })
  old_time <- as.numeric(difftime(Sys.time(), old_start, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Method 2: NEW Global Model
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_h2("Method 2: Global Model (NEW)")
    cli::cli_text("Strategy: Kennard-Stone sampling + single global Cubist model")
  }

  new_start <- Sys.time()
  new_results <- tryCatch({

    predict_soil_covariates(
      input_data         = input_data,
      covariates         = covariates,
      n_similar          = n_similar,
      prop               = 0.85,
      variance_threshold = 0.985,
      bayesian_iter      = 10,
      allow_par          = FALSE,
      refresh            = TRUE,
      verbose            = verbose
    )

  }, error = function(e) {
    cli::cli_alert_danger("NEW method failed: {e$message}")
    list(predictions = NULL, validation_metrics = NULL)
  })
  new_time <- as.numeric(difftime(Sys.time(), new_start, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Extract Validation Metrics
  ## ---------------------------------------------------------------------------

  old_metrics <- NULL
  new_metrics <- NULL

  if (!is.null(old_results$Evaluation_Statistics)) {
    old_metrics <- old_results$Evaluation_Statistics %>%
      mutate(Method = "OLD_Clustered", .before = 1)
  }

  if (!is.null(new_results$validation_metrics)) {
    new_metrics <- new_results$validation_metrics %>%
      select(covariate, rsq, rmse, ccc, rpd) %>%
      rename(Covariate = covariate,
             R2 = rsq,
             RMSE = rmse,
             CCC = ccc,
             RPD = rpd) %>%
      mutate(Method = "NEW_Global", .before = 1)
  }

  ## ---------------------------------------------------------------------------
  ## Comparison Summary
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_h2("Performance Comparison")
    cli::cli_text("")

    cli::cli_text("{cli::col_blue('Execution Time')}")
    cli::cli_text("├─ OLD (Clustered): {round(old_time, 1)}s")
    cli::cli_text("└─ NEW (Global):    {round(new_time, 1)}s")
    cli::cli_text("")

    if (!is.null(old_metrics) && !is.null(new_metrics)) {

      combined <- bind_rows(old_metrics, new_metrics)

      cli::cli_text("{cli::col_blue('Validation Metrics')}")

      for (cov in covariates) {
        old_row <- old_metrics %>% dplyr::filter(Covariate == cov)
        new_row <- new_metrics %>% dplyr::filter(.data$Covariate == cov)

        if (nrow(old_row) > 0 && nrow(new_row) > 0) {
          cli::cli_text("")
          cli::cli_text("├─ {toupper(cov)}")
          cli::cli_text("│  ├─ OLD: R² = {round(old_row$R2, 3)} | RMSE = {round(old_row$RMSE, 2)} | RPD = {round(old_row$RPD, 2)}")
          cli::cli_text("│  └─ NEW: R² = {round(new_row$R2, 3)} | RMSE = {round(new_row$RMSE, 2)} | RPD = {round(new_row$RPD, 2)}")

          # Show winner
          if (new_row$R2 > old_row$R2) {
            cli::cli_text("│      → {cli::col_green('NEW wins')} (ΔR² = +{round(new_row$R2 - old_row$R2, 3)})")
          } else if (old_row$R2 > new_row$R2) {
            cli::cli_text("│      → {cli::col_yellow('OLD wins')} (ΔR² = +{round(old_row$R2 - new_row$R2, 3)})")
          } else {
            cli::cli_text("│      → Tie")
          }
        }
      }

    } else {
      cli::cli_alert_warning("Metrics unavailable for one or both methods")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Return Results
  ## ---------------------------------------------------------------------------

  return(list(
    old = list(
      predictions = old_results$Predicted_Values,
      metrics = old_metrics,
      time_sec = old_time
    ),
    new = list(
      predictions = new_results$predictions,
      metrics = new_metrics,
      time_sec = new_time
    ),
    summary = bind_rows(old_metrics, new_metrics)
  ))

}

results <- compare_covariate_methods(input_data = test_data,
                                     covariates = c("clay", "ph"),
                                     n_similar = 30000,
                                     verbose = TRUE)
