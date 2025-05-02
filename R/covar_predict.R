#' Predict Covariates from MIR Spectra Using Clustered Cubist Models
#'
#' This function calibrates and applies Cubist models trained on clustered OSSL
#' data to predict selected covariates (e.g., Sand, pH) for new spectral samples.
#' It includes PCA-based dimensionality reduction, k-means clustering, per-cluster
#' Cubist model calibration, and hold-out evaluation of model performance.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_warning cli_progress_step cli_progress_done
#' @importFrom stringr str_detect str_split_i
#' @importFrom stats predict quantile
#' @importFrom here here
#' @importFrom tidyselect starts_with
#' @importFrom prospectr savitzkyGolay standardNormalVariate
#' @importFrom rsample initial_split training testing
#' @importFrom glue glue
#'
#' @param covariates Character vector of covariate names to predict (e.g., "Sand", "pH").
#' @param input_data A data frame of wide-format MIR spectra. Must include `Sample_ID` and numeric wavenumber columns (600–4000 cm⁻¹).
#'
#' @return A list with:
#'   \item{Predicted_Values}{A tibble of covariate predictions for the unknown samples.}
#'   \item{Evaluation_Statistics}{A tibble of model evaluation metrics (e.g., RMSE, R²) based on the hold-out set.}
#'
#' @details
#' The function first smooths and normalizes the MIR input spectra, projects the training data into PCA space,
#' clusters the samples, calibrates per-cluster Cubist models, predicts on new samples, and evaluates model performance.
#' Duplicate `Sample_ID` values are flagged. Progress steps are printed to the console.
#'
#' @export

predict_covariates <- function(covariates,
                               input_data) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Input Validation: Check input_data is a data frame
    ## ---------------------------------------------------------------------------

    if (!is.data.frame(input_data)) {

      cli::cli_alert_danger("Input {.arg input_data} must be a {.cls data.frame}.")
      stop("Aborting: Invalid `input_data` format.")

     }

    ## ---------------------------------------------------------------------------
    ## Input Validation: Check Sample_ID column
    ## ---------------------------------------------------------------------------

    if (!"Sample_ID" %in% names(input_data)) {

       cli::cli_alert_danger("Input {.arg input_data} must contain a {.field Sample_ID} column.")
      stop("Aborting: Missing `Sample_ID` in `input_data`.")

    }

    ## ---------------------------------------------------------------------------
    ## Input Validation: Check spectral columns exist
    ## ---------------------------------------------------------------------------

    if (!any(stringr::str_detect(names(input_data), "^\\d{3,4}$"))) {

      cli::cli_alert_danger("No spectral columns detected in {.arg input_data}.")
      cli::cli_alert_info("Expected numeric wavenumber columns like {.val '600'}, {.val '602'}, ..., {.val '4000'}.")
      stop("Aborting: Spectral input check failed.")

    }

    ## ---------------------------------------------------------------------------
    ## Check for duplicate Sample_IDs in input
    ## ---------------------------------------------------------------------------

   dup_check <- input_data %>%
      dplyr::count(Sample_ID) %>%
      dplyr::filter(n > 1)

    if (nrow(dup_check) > 0) {
      cli::cli_alert_warning("Duplicate Sample_IDs detected in input data. This may cause mapping conflicts.")
      print(dup_check)
    }

      cli::cli_alert_info("Starting prediction of covariates.")

  ## ---------------------------------------------------------------------------
  ## Step 1.5: Smooth and normalize the data
  ## ---------------------------------------------------------------------------

  input_data %>%
    dplyr::select(`600`:`4000`) %>%
    prospectr::savitzkyGolay(X = .,
                             m = 0,
                             p = 1,
                             w = 9) %>%
    prospectr::standardNormalVariate(X = .) %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(input_data %>% select(-c(`600`:`4000`)),
                     .) -> input_data

  ## ---------------------------------------------------------------------------
  ## Step 2: Load the OSSL data
  ## ---------------------------------------------------------------------------

  tryCatch({

    training_data <- download_ossl_data(covariates = covariates)

    }, error = function(e) {

      stop("Error in downloading OSSL data: ", e$message)

    })

  cli::cli_progress_step("OSSL data loaded and processed.")

  ## ---------------------------------------------------------------------------
  ## Step 3: Reduce dimensions
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Running dimensionality reduction...")

  tryCatch({

    reduced_dimensions_data <- reduce_dimensions_pca_covpred(training_data = training_data,
                                                             new_data      = input_data)
  }, error = function(e) {

     stop("Error during dimensionality reduction: ", e$message)

    })

  training_data <- reduced_dimensions_data$training_data
  input_data    <- reduced_dimensions_data$new_data

  cli::cli_progress_step("Dimensionality reduction complete.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Cluster the new data
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Running clustering analysis...")

  tryCatch({

    clustering_results <- cluster_input_data(input_data = input_data)

    }, error = function(e) {

       stop("Error during clustering of input data: ", e$message)

    })

  input_data    <- clustering_results$input_data
  pca_model     <- clustering_results$pca_model
  kmeans_model  <- clustering_results$kmeans_model
  n_componenets <- clustering_results$ncomp

  cli::cli_progress_step("Clustering analysis complete.")

  ## ---------------------------------------------------------------------------
  ## Step 5: Build per-cluster training subsets
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Segmenting data...")

  tryCatch({
    training_data_clustered <- create_training_subsets(training_data = training_data,
                                                       pca_model     = pca_model,
                                                       kmeans_model  = kmeans_model,
                                                       n_components  = n_componenets,
                                                       coverage      = 0.80)
  }, error = function(e) {

    stop("Error during training data segmentation: ", e$message)

  })


  ## ---------------------------------------------------------------------------
  ## Step 6: Split training data into train/holdout
  ## ---------------------------------------------------------------------------

  splits        <- purrr::map(training_data_clustered, ~ rsample::initial_split(.x, prop = 0.95, strata = NULL))
  training_data <- purrr::map(splits, rsample::training)
  holdout_data  <- purrr::map(splits, rsample::testing)

  ## ---------------------------------------------------------------------------
  ## Step 7: Prepare holdout data for evaluation
  ## ---------------------------------------------------------------------------

  holdout_data %>%
    dplyr::bind_rows(.id = "Cluster") %>%
    dplyr::mutate(Cluster      = stringr::str_split_i(Cluster, "_", 2),
                  Cluster      = as.numeric(Cluster),
                  Sample_Index = as.character(Sample_Index)) -> holdout_data

  holdout_data %>%
    dplyr::select(-Cluster,
                  -starts_with("Dim.")) %>%
    dplyr::rename(Sample_ID = Sample_Index) %>%
    dplyr::select(Sample_ID,
                  all_of(covariates)) -> holdout_measurements

   holdout_data %>%
    dplyr::select(Cluster,
                  Sample_Index,
                  starts_with("Dim.")) %>%
    dplyr::mutate(Source = "Holdout",
                  .before = 2) %>%
    dplyr::rename(Sample_ID = Sample_Index) -> holdout_spectra

  cli::cli_progress_step("Training data and holdouts created.")

  ## ---------------------------------------------------------------------------
  ## Step 8: Train Cubist models across clusters and covariates
  ## ---------------------------------------------------------------------------

  model_grid <- purrr::cross_df(list(
    cluster          = names(training_data),
    covariate        = covariates
  )) %>%
    dplyr::mutate(training_subset = purrr::map(cluster, ~ training_data[[.x]]))

  cli::cli_progress_step("Setting up Cubist models...")

  Calibrated_Models <- purrr::pmap(model_grid,
                                   function(covariate,
                                            cluster,
                                            training_subset) {

    cli::cli_progress_step("Calibrating model for: {covariate} ({cluster})")

    calibrated_model <- cubist_model_function_covar(input_data = training_subset,
                                                    covariate  = covariate)

    list(outcome     = covariate,
         cluster     = cluster,
         model       = calibrated_model$Model,
         best_params = calibrated_model$Best_Parameters,
         evaluation  = calibrated_model$Evaluation)
  })

  cli::cli_progress_step("Cubist models calibrated.")

  ## ---------------------------------------------------------------------------
  ## Step 9: Predict covariates for new samples
  ## ---------------------------------------------------------------------------

  input_data %>%
    dplyr::mutate(Source = "Unknown", .before = 2) %>%
    dplyr::bind_rows(holdout_spectra) -> input_data

  predictions <- purrr::map(Calibrated_Models, function(model_info) {

    input_data %>%
      dplyr::filter(Cluster == as.numeric(stringr::str_split_i(model_info$cluster, "_", 2))) -> input_data_local

    if (nrow(input_data_local) == 0) return(NULL)

    preds <- predict(object   = model_info$model,
                     new_data = input_data_local)

    input_data_local %>%
      dplyr::select(Sample_ID,
                    Cluster,
                    Source) %>%
      dplyr::mutate(Predicted_Values = preds$.pred,
                    Covariate        = model_info$outcome)

  }) %>%  dplyr::bind_rows() %>%
          dplyr::group_by(Sample_ID,
                          Covariate,
                          Source,
                          Cluster) %>%
          dplyr::ungroup()


  ## ---------------------------------------------------------------------------
  ## Step 10: Split into holdout and unknown
  ## ---------------------------------------------------------------------------

  predictions %>%
    dplyr::filter(Source == "Holdout") %>%
    dplyr::select(-Source,
                  -Cluster) %>%
    tidyr::pivot_wider(names_from  = Covariate,
                       values_from = Predicted_Values) %>%
    dplyr::select(Sample_ID,
           all_of(covariates)) -> holdout_predictions

  predictions %>%
    dplyr::filter(Source == "Unknown") %>%
    dplyr::select(-Source,
                  -Cluster) %>%
    tidyr::pivot_wider(names_from  = Covariate,
                       values_from = Predicted_Values) -> unknown_predictions

  ## ---------------------------------------------------------------------------
  ## Step 11: Evaluate holdout predictions
  ## ---------------------------------------------------------------------------

  evaluation_stats <- evaluate_predictions(measured_data = holdout_measurements,
                                           modeled_data  = holdout_predictions)

  cli::cli_progress_step("Prediction and evaluation complete.")
  cli::cli_progress_done()

  ## ---------------------------------------------------------------------------
  ## Step 12: Return output
  ## ---------------------------------------------------------------------------

  return(list(Predicted_Values      = unknown_predictions,
              Evaluation_Statistics = evaluation_stats
  ))
}

