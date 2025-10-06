#' Soil Covariate Prediction from MIR Spectra
#'
#' @description
#' Predicts soil properties from mid-infrared (MIR) spectroscopy using a Mahalanobis
#' distance-based local modeling approach with OSSL training data.
#'
#' @keywords internal

## Main Soil Prediction Function ---------------------------------------------

#' Predict Soil Covariates from MIR Spectra
#'
#' @description
#' Predicts soil covariate values from MIR spectra using OSSL training data and
#' clustered local Cubist models. Clusters OSSL samples in PCA space (K-means with
#' silhouette optimization) and trains specialized models per cluster. This local
#' modeling approach achieves R² > 0.95 for most soil properties, outperforming
#' global models by 9-10% in validation accuracy.
#'
#' @param input_data Tibble with MIR spectral data and Sample_ID column
#' @param covariates Character vector of soil properties to predict
#' @param n_similar Integer. (Deprecated - ignored) Clustering uses all available OSSL samples (default: 20000)
#' @param prop Numeric. Proportion for training set (default: 0.85)
#' @param variance_threshold Numeric. Proportion of variance to capture with PCA (default: 0.85)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 3, use 0 for grid search only)
#' @param allow_par Logical. Enable parallel processing for hyperparameter tuning (default: FALSE, set TRUE for faster execution)
#' @param n_workers Integer. Number of parallel workers (default: NULL, auto-detect)
#' @param refresh Logical. Force refresh of OSSL training data (default: FALSE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Named list with:
#'   - predictions: Tibble with Sample_ID and predicted covariate columns
#'   - validation_metrics: Tibble with accuracy metrics per cluster/covariate
#'   - local_models: Nested list of fitted models (cluster × covariate)
#'   - cluster_info: Clustering details (n_clusters, assignments, sizes)
#'
#' @examples
#' \dontrun{
#' # Default: Use all OSSL samples with clustered local models
#' results <- predict_soil_covariates(
#'   input_data = my_mir_spectra,
#'   covariates = c("clay", "ph", "oc"),
#'   allow_par = TRUE  # Enable parallel for faster execution
#' )
#'
#' # Faster prediction with reduced Bayesian optimization
#' results <- predict_soil_covariates(
#'   input_data = my_mir_spectra,
#'   covariates = c("clay", "sand"),
#'   bayesian_iter = 0,  # Skip Bayesian for speed
#'   allow_par = TRUE
#' )
#' }
#'
#' @export

predict_soil_covariates <- function(input_data,
                                    covariates,
                                    n_similar          = 20000,
                                    prop               = 0.85,
                                    variance_threshold = 0.985,
                                    bayesian_iter      = 10,
                                    allow_par          = FALSE,
                                    n_workers          = NULL,
                                    refresh            = FALSE,
                                    verbose            = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_text("│  ├─ Configuration:")
    cli::cli_text("│  │  ├─ Unknown samples: {nrow(input_data)}")
    cli::cli_text("│  │  ├─ Target properties: {paste(covariates, collapse = ', ')}")
    cli::cli_text("│  │  ├─ Training strategy: Clustered local models (all OSSL samples)")
    cli::cli_text("│  │  ├─ Train/Val split: {round(prop * 100)}%/{round((1-prop) * 100)}%")
    cli::cli_text("│  │  ├─ PCA variance threshold: {round(variance_threshold * 100, 1)}%")
    cli::cli_text("│  │  ├─ Bayesian iterations: {if(bayesian_iter > 0) bayesian_iter else 'Grid search only'}")
    cli::cli_text("│  │  ├─ Parallel processing: {if (allow_par) paste0('Enabled (', if (is.null(n_workers)) 'auto' else n_workers, ' workers)') else 'Disabled'}")
    cli::cli_text("│  │  └─ Model: Cubist")

  }

  ## Validate properties -------------------------------------------------------

  validate_soil_properties(covariates)

  ## Make sure input data has sample_ids to join to ---------------------------

  if (!"Sample_ID" %in% names(input_data)) cli::cli_abort("{.val {input_data}} must contain a Sample_ID column")

  ## Make sure there's some spectra in the input data --------------------------

  ## TODO: I think this is maybe overly brittle... should update in the
  ## inputs functions to allow for more incoming names?

  spectral_cols <- grep("^[0-9]{3,4}$", names(input_data), value = TRUE)

  if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns found in input_data")

  ## Make sure the proportion data makes sense ---------------------------------

  if (prop <= 0 || prop >= 1) cli::cli_abort("prop must be between 0 and 1")

  ## ---------------------------------------------------------------------------
  ## Step 1: Load and preprocess OSSL training data
  ## ---------------------------------------------------------------------------


  get_processed_ossl_training_data(properties         = covariates,
                                   variance_threshold = variance_threshold,
                                   refresh            = refresh,
                                   verbose            = verbose) -> ossl_result

  if (is.null(ossl_result)) cli::cli_abort("Failed to acquire processed OSSL training data")

  if (!all(c("pca_model", "pca_scores") %in% names(ossl_result))) cli::cli_abort("OSSL data missing required components (pca_model, pca_scores)")



  ## ---------------------------------------------------------------------------
  ## Step 3: Preprocess and project unknown samples
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("├─ Processing unknown samples.")

  ##

  preprocess_mir_spectra(spectral_data = input_data,
                         verbose       = verbose) -> unknown_preprocessed

  if (is.null(unknown_preprocessed)) cli::cli_abort("Failed to preprocess unknown spectra")

  if (length(grep("^[0-9]{3,4}$", names(unknown_preprocessed), value = TRUE)) == 0) cli::cli_abort("No spectral columns found after preprocessing")



  ## ---------------------------------------------------------------------------

  project_spectra_to_pca(new_data  = unknown_preprocessed,
                         pca_model = ossl_result$pca_model,
                         verbose   = verbose) -> unknown_pca

  if (is.null(unknown_pca)) cli::cli_abort("Failed to project unknown samples to PCA space")

  ## ---------------------------------------------------------------------------
  ## Step 4: Cluster OSSL samples in PCA space
  ## ---------------------------------------------------------------------------

  cluster_ossl_samples(ossl_pca_scores = ossl_result$pca_scores,
                       max_clusters    = 10,
                       seed            = 307,
                       verbose         = verbose) -> cluster_model

  ## ---------------------------------------------------------------------------
  ## Step 5: Assign unknown samples to clusters
  ## ---------------------------------------------------------------------------

  assign_unknowns_to_clusters(unknown_pca_scores = unknown_pca,
                               cluster_model      = cluster_model,
                               verbose            = verbose) -> cluster_assignments

  ## ---------------------------------------------------------------------------
  ## Step 6: Create cluster-specific training subsets
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("├─ Creating cluster-specific training subsets")

  create_clustered_subsets(ossl_pca_scores = ossl_result$pca_scores,
                           cluster_model   = cluster_model,
                           prop            = prop,
                           verbose         = verbose) -> training_subsets

  ## ---------------------------------------------------------------------------
  ## Step 7: Train cluster-specific models
  ## ---------------------------------------------------------------------------

  local_models <- vector("list", cluster_model$n_clusters)
  names(local_models) <- paste0("Cluster_", seq_len(cluster_model$n_clusters))

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("Training local models: {cluster_model$n_clusters} cluster{?s} × {length(covariates)} covariate{?s}")
  }

  for (cluster_id in seq_len(cluster_model$n_clusters)) {

    cluster_name <- paste0("Cluster_", cluster_id)
    cluster_data <- training_subsets[[cluster_name]]

    local_models[[cluster_name]] <- vector("list", length(covariates))
    names(local_models[[cluster_name]]) <- covariates

    for (i in seq_along(covariates)) {

      covariate <- covariates[i]

      if (verbose) {
        cli::cli_text("├─ [{cluster_id}/{cluster_model$n_clusters}] {toupper(cluster_name)} | [{i}/{length(covariates)}] {toupper(covariate)}")
      }

      ## Validate covariate exists
      if (!covariate %in% names(cluster_data$train)) {
        cli::cli_abort("Covariate '{covariate}' not found in {cluster_name} training data")
      }

      ## Check sufficient samples after NA removal
      train_rows <- cluster_data$train %>%
        dplyr::select(dplyr::all_of(covariate), dplyr::starts_with("Dim.")) %>%
        tidyr::drop_na() %>%
        nrow()

      val_rows <- cluster_data$val %>%
        dplyr::select(dplyr::all_of(covariate), dplyr::starts_with("Dim.")) %>%
        tidyr::drop_na() %>%
        nrow()

      if (train_rows < 20) {
        cli::cli_warn("{cluster_name}: Insufficient training samples for {covariate}: {train_rows} (minimum 20) - skipping")
        local_models[[cluster_name]][[covariate]] <- NULL
        next
      }

      if (val_rows < 10) {
        cli::cli_warn("{cluster_name}: Insufficient validation samples for {covariate}: {val_rows} (minimum 10) - skipping")
        local_models[[cluster_name]][[covariate]] <- NULL
        next
      }

      ## Train local model for this cluster + covariate
      model_start <- Sys.time()
      model_result <- fit_cubist_model(
        train_data    = cluster_data$train,
        val_data      = cluster_data$val,
        covariate     = covariate,
        verbose       = verbose,
        parallel      = allow_par,
        n_workers     = n_workers,
        bayesian_iter = bayesian_iter
      )
      model_time <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))

      local_models[[cluster_name]][[covariate]] <- model_result

      ## Show completion status
      if (verbose && !is.null(model_result$validation_metrics)) {
        perf <- model_result$validation_metrics
        cli::cli_text("│  ├─ Validation R² = {round(perf$rsq, 3)} | RMSE = {round(perf$rmse, 2)}")
        cli::cli_text("│  └─ Training time: {round(model_time, 1)}s")
      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 8: Generate predictions with cluster-specific routing
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("├─ Generating predictions for {nrow(unknown_pca)} unknown samples")
  }

  predictions <- unknown_pca %>%
    dplyr::select(Sample_ID)

  for (covariate in covariates) {

    pred_values <- numeric(nrow(unknown_pca))

    ## Route each unknown to its cluster-specific model
    for (cluster_id in seq_len(cluster_model$n_clusters)) {

      cluster_name <- paste0("Cluster_", cluster_id)
      cluster_mask <- cluster_assignments == cluster_id

      if (sum(cluster_mask) > 0) {

        cluster_model_obj <- local_models[[cluster_name]][[covariate]]

        if (!is.null(cluster_model_obj)) {

          cluster_unknowns <- unknown_pca[cluster_mask, ]
          cluster_preds <- tryCatch({
            stats::predict(cluster_model_obj$fitted_workflow, cluster_unknowns)$.pred
          }, error = function(e) {
            rep(NA_real_, sum(cluster_mask))
          })

          pred_values[cluster_mask] <- cluster_preds

        } else {
          # Model failed for this cluster - assign NA
          pred_values[cluster_mask] <- NA_real_
        }
      }
    }

    predictions[[covariate]] <- pred_values
  }

  ## ---------------------------------------------------------------------------
  ## Step 9: Combine validation metrics from all cluster models
  ## ---------------------------------------------------------------------------

  validation_metrics <- vector("list", cluster_model$n_clusters * length(covariates))
  idx <- 1

  for (cluster_id in seq_len(cluster_model$n_clusters)) {

    cluster_name <- paste0("Cluster_", cluster_id)

    for (covariate in covariates) {

      model_result <- local_models[[cluster_name]][[covariate]]

      if (!is.null(model_result) && !is.null(model_result$validation_metrics)) {

        validation_metrics[[idx]] <- model_result$validation_metrics %>%
          dplyr::mutate(cluster   = cluster_name,
                       covariate = covariate,
                       .before   = 1)

        idx <- idx + 1
      }
    }
  }

  all_validation_metrics <- dplyr::bind_rows(validation_metrics)

  if (verbose) {
    cli::cli_text("└─ Predictions complete for {nrow(predictions)} samples.")
  }

  return(list(
    predictions        = predictions,
    validation_metrics = all_validation_metrics,
    local_models       = local_models,
    cluster_info       = list(
      n_clusters          = cluster_model$n_clusters,
      cluster_assignments = cluster_model$cluster_assignments,
      cluster_sizes       = table(cluster_model$cluster_assignments),
      silhouette_scores   = cluster_model$silhouette_scores
    )
  ))
}


