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
    cli::cli_text("│  │  ├─ Training strategy: Clustered Kennard-Stone ({format(n_similar, big.mark = ',')} samples)")
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

  if (verbose) cli::cli_text("│  ├─ Processing unknown samples.")

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
  ## Step 4: Global training set selection
  ## ---------------------------------------------------------------------------


  select_global_training_set(unknown_pca_scores  = unknown_pca,
                             ossl_pca_scores     = ossl_result$pca_scores,
                             n_select            = n_similar,
                             prop                = prop,
                             relevance_threshold = 0.85,
                             verbose             = verbose) -> global_selection

  ## ---------------------------------------------------------------------------
  ## Step 5: Train global models (one per covariate)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│  ├─ {cli::style_bold('Training models for {length(covariates)} covariate{if(length(covariates) > 1) \"s\" else \"\"}')}")
  }

  global_models <- vector("list", length(covariates))
  names(global_models) <- covariates

  for (i in seq_along(covariates)) {

    covariate <- covariates[i]

    if (verbose) {
      cli::cli_text("│  │  ├─ [{i}/{length(covariates)}] {toupper(covariate)}")
      cli::cli_text("│  │  │  ├─ Training: {format(nrow(global_selection$train_data), big.mark = ',')} samples | Validation: {format(nrow(global_selection$val_data), big.mark = ',')} samples")
    }

    # Validate covariate exists in data
    if (!covariate %in% names(global_selection$train_data)) cli::cli_abort("Covariate '{covariate}' not found in training data after OSSL processing")
    if (!covariate %in% names(global_selection$val_data)) cli::cli_abort("Covariate '{covariate}' not found in validation data after OSSL processing")

    # Check sufficient samples after NA removal
    train_rows <- global_selection$train_data %>%
      dplyr::select(dplyr::all_of(covariate), dplyr::starts_with("Dim.")) %>%
      tidyr::drop_na() %>%
      nrow()

    val_rows <- global_selection$val_data %>%
      dplyr::select(dplyr::all_of(covariate), dplyr::starts_with("Dim.")) %>%
      tidyr::drop_na() %>%
      nrow()

    if (train_rows < 20) cli::cli_abort("Insufficient training samples for {covariate} after NA removal: {train_rows} (minimum 20)")
    if (val_rows < 10) cli::cli_abort("Insufficient validation samples for {covariate} after NA removal: {val_rows} (minimum 10)")

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
    if (verbose && !is.null(model_result$validation_metrics)) {
      perf <- model_result$validation_metrics
      cli::cli_text("│  │  │  ├─ Validation R² = {round(perf$rsq, 3)} | RMSE = {round(perf$rmse, 2)}")
      cli::cli_text("│  │  │  └─ Training time: {round(model_time, 1)}s")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Apply global models to all unknown samples
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│  ├─ Applying global models to {nrow(unknown_pca)} unknown samples.")
  }

  # Prepare predictions tibble
  predictions <- unknown_pca %>%
    dplyr::select(Sample_ID)

  # Apply each global model to all unknowns
  for (covariate in covariates) {

    model_result <- global_models[[covariate]]

    if (!is.null(model_result)) {

      # Predict all unknowns with this global model
      covariate_predictions <- tryCatch({
        stats::predict(model_result$fitted_workflow, unknown_pca)$.pred
      }, error = function(e) {
        rep(NA_real_, nrow(unknown_pca))
      })

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
    cli::cli_text("│  └─ Predictions complete for {nrow(predictions)} samples.")
  }

  return(list(
    predictions = predictions,
    validation_metrics = all_validation_metrics,
    global_models = global_models,
    selection_info = list(
      clusters = global_selection$cluster_info,
      indices = global_selection$global_indices,
      n_train = nrow(global_selection$train_data),
      n_val = nrow(global_selection$val_data)
    )
  ))
}


