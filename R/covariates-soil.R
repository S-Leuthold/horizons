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
#' Cubist models. By default, uses ALL available OSSL samples (~36,000) for best
#' performance. Simpler selection methods outperform complex algorithms like
#' Kennard-Stone when sufficient training data is used.
#'
#' @param input_data Tibble with MIR spectral data and Sample_ID column
#' @param covariates Character vector of soil properties to predict
#' @param n_similar Integer. Total OSSL samples to select via Kennard-Stone (default: 20000)
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
#'   - validation_metrics: Tibble with accuracy metrics per sample/covariate
#'   - global_models: List of fitted models for each covariate
#'   - selection_info: Information about training set selection
#'
#' @examples
#' \dontrun{
#' # Default: Use 20,000 OSSL samples with clustered Kennard-Stone
#' results <- predict_soil_covariates(
#'   input_data = my_mir_spectra,
#'   covariates = c("clay", "ph", "oc"),
#'   allow_par = TRUE  # Enable parallel for faster execution
#' )
#'
#' # Maximum performance: Use more samples
#' results <- predict_soil_covariates(
#'   input_data = my_mir_spectra,
#'   covariates = "clay",
#'   n_similar = 30000,
#'   allow_par = TRUE  # Enable parallel
#' )
#'
#' # Faster with fewer samples and no Bayesian optimization
#' results <- predict_soil_covariates(
#'   input_data = my_mir_spectra,
#'   covariates = c("clay", "sand"),
#'   n_similar = 10000,
#'   bayesian_iter = 0,  # Skip Bayesian for speed
#'   allow_par = TRUE
#' )
#' }
#'
#' @export
predict_soil_covariates <- function(input_data,
                                   covariates,
                                   n_similar = 20000,
                                   prop = 0.85,
                                   variance_threshold = 0.985,
                                   bayesian_iter = 10,  # Reduced from 5 for faster execution
                                   allow_par = FALSE,
                                   n_workers = NULL,
                                   refresh = FALSE,
                                   verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  if (verbose) {

    list("Unknown samples"         = nrow(input_data),
         "Target properties"      = paste(covariates, collapse = ", "),
         "Training strategy"      = paste0("Clustered Kennard-Stone (", format_metric(n_similar, "count"), " samples)"),
         "Train/Val split"        = paste0(format_metric(prop * 100, "percentage"), "/", format_metric((1-prop) * 100, "percentage")),
         "PCA variance threshold" = paste0(format_metric(variance_threshold * 100, "percentage")),
         "Bayesian iterations"    = if(bayesian_iter > 0) bayesian_iter else "Grid search only",
         "Parallel processing"    = if (allow_par) paste0("Enabled (", if (is.null(n_workers)) "auto" else n_workers, " workers)") else "Disabled",
         "Model"                  = "Cubist") -> config_info

    display_config_summary("Soil Covariate Prediction", config_info, verbose)

  }

  ## ---------------------------------------------------------------------------

  validate_soil_properties(covariates)

  ## ---------------------------------------------------------------------------

  if (!"Sample_ID" %in% names(input_data)) {

    cli::cli_abort("input_data must contain a Sample_ID column")

  }

  ## ---------------------------------------------------------------------------

  spectral_cols <- grep("^[0-9]{3,4}$", names(input_data), value = TRUE)

  if (length(spectral_cols) == 0) {

    cli::cli_abort("No spectral columns found in input_data")

  }

  ## ---------------------------------------------------------------------------

  if (prop <= 0 || prop >= 1) {

    cli::cli_abort("prop must be between 0 and 1")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Load and preprocess OSSL training data
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_text("")
    cli::cli_text(format_header("OSSL Training Data Pipeline", style = "single", center = FALSE))

  }

  ## ---------------------------------------------------------------------------

  get_processed_ossl_training_data(properties         = covariates,
                                   variance_threshold = variance_threshold,
                                   refresh            = refresh,
                                   verbose            = verbose) -> ossl_result

  if (is.null(ossl_result)) {

    cli::cli_abort("Failed to acquire processed OSSL training data")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Preprocess and project unknown samples
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_text("")
    cli::cli_text(format_header("Unknown Sample Processing", style = "single", center = FALSE))

  }

  ## ---------------------------------------------------------------------------

  preprocess_mir_spectra(spectral_data = input_data,
                         verbose = verbose) -> unknown_preprocessed

  if (is.null(unknown_preprocessed)) {

    cli::cli_abort("Failed to preprocess unknown spectra")

    }

  ## ---------------------------------------------------------------------------

  project_spectra_to_pca(new_data  = unknown_preprocessed,
                         pca_model = ossl_result$pca_model,
                         verbose   = verbose) -> unknown_pca

  if (is.null(unknown_pca)) {

    cli::cli_abort("Failed to project unknown samples to PCA space")

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Global training set selection
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_text("")
    cli::cli_text(format_header("Sample Selection Strategy", style = "single", center = FALSE))

  }

  ## ---------------------------------------------------------------------------

  select_global_training_set(unknown_pca_scores  = unknown_pca,
                             ossl_pca_scores     = ossl_result$pca_scores,
                             n_select            = n_similar,
                             prop                = prop,
                             relevance_threshold = 0.6,
                             verbose             = verbose) -> global_selection

  if (is.null(global_selection)) {

    cli::cli_abort("Failed to select global training set")

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Train global models (one per covariate)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text(format_header("Model Training", style = "single", center = FALSE))
  }

  global_models <- vector("list", length(covariates))
  names(global_models) <- covariates

  for (i in seq_along(covariates)) {

    covariate <- covariates[i]

    if (verbose) {
      cli::cli_text("")
      cli::cli_text(format_tree_item(paste0("[", i, "/", length(covariates), "] ", toupper(covariate), " Prediction"), level = 0))

      # Show training configuration
      train_info <- paste0("Training: ", format_metric(nrow(global_selection$train_data), "count"),
                          " samples | Validation: ", format_metric(nrow(global_selection$val_data), "count"), " samples")
      cli::cli_text(format_tree_item(train_info, level = 1, is_last = FALSE))
    }

    # Train single model for this covariate using global training set
    model_start <- Sys.time()
    model_result <- fit_cubist_model(
      train_data = global_selection$train_data,
      val_data = global_selection$val_data,
      covariate = covariate,
      verbose = verbose,
      parallel = allow_par,
      n_workers = n_workers,
      bayesian_iter = bayesian_iter
    )
    model_time <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))

    global_models[[covariate]] <- model_result

    # Show completion status
    if (verbose && !is.null(model_result$performance)) {
      perf <- model_result$performance
      completion_text <- paste0(get_status_symbol("complete"), " Validation R² = ",
                               format_metric(perf$val_r2, "r2"),
                               " | RMSE = ", format_metric(perf$val_rmse))
      cli::cli_text(format_tree_item(completion_text, level = 1, is_last = FALSE))

      timing_text <- paste0("Training time: ", format_time(model_time))
      cli::cli_text(format_tree_item(timing_text, level = 1, is_last = TRUE))
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Apply global models to all unknown samples
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text(format_tree_item(paste0("⟳ Applying global models to ", nrow(unknown_pca), " unknown samples..."), level = 1, is_last = FALSE, symbol = NULL))
  }

  # Prepare predictions tibble
  predictions <- unknown_pca %>%
    dplyr::select(Sample_ID)

  # Apply each global model to all unknowns
  for (covariate in covariates) {

    model_result <- global_models[[covariate]]

    if (!is.null(model_result)) {

      # Predict all unknowns with this global model
      covariate_predictions <- safely_execute(
        expr = {
          stats::predict(model_result$fitted_workflow, unknown_pca)$.pred
        },
        default_value = rep(NA_real_, nrow(unknown_pca)),
        error_message = "Failed to predict {covariate} for unknown samples"
      )$result

      # Add to predictions tibble
      predictions[[covariate]] <- covariate_predictions

    } else {

      # Model failed - add NA column
      predictions[[covariate]] <- rep(NA_real_, nrow(unknown_pca))
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Combine validation metrics
  ## ---------------------------------------------------------------------------

  # Combine validation metrics from all global models
  validation_metrics <- vector("list", length(covariates))

  for (i in seq_along(covariates)) {
    covariate <- covariates[i]
    model_result <- global_models[[covariate]]

    if (!is.null(model_result)) {
      validation_metrics[[i]] <- model_result$validation_metrics %>%
        dplyr::mutate(covariate = covariate, .before = 1)
    }
  }

  all_validation_metrics <- dplyr::bind_rows(validation_metrics)

  if (verbose) {
    cli::cli_text("")
    cli::cli_text(format_header("Final Results", style = "single", center = FALSE))

    # Show completion status
    cli::cli_text("")
    cli::cli_text(format_tree_item(paste0(get_status_symbol("complete"), " Predictions Complete"), level = 0))

    # Show per-covariate results
    for (covariate in covariates) {
      if (!is.null(global_models[[covariate]]$performance)) {
        perf <- global_models[[covariate]]$performance
        metric_text <- paste0(covariate, ": ", format_metric(nrow(predictions), "count"),
                             " predictions (R² = ", format_metric(perf$val_r2, "r2"),
                             ", RMSE = ", format_metric(perf$val_rmse), ")")
        cli::cli_text(format_tree_item(metric_text, level = 1, is_last = (covariate == tail(covariates, 1))))
      }
    }

    cli::cli_text("")
  }

  return(list(
    predictions = predictions,
    validation_metrics = all_validation_metrics,
    global_models = global_models,
    selection_info = list(
      quality = global_selection$selection_quality,
      clusters = global_selection$cluster_info,
      indices = global_selection$global_indices
    )
  ))
}


