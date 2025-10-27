#' Library-Based Prediction Orchestrator
#'
#' @description
#' Main user-facing API for library-based prediction. Orchestrates the complete
#' workflow: load OSSL library, cluster unknowns, optimize configs per cluster,
#' train models, generate predictions with proper target handling.
#'
#' @importFrom cli cli_text cli_alert_info cli_alert_success cli_abort style_bold
#' @importFrom dplyr filter bind_rows mutate left_join
#' @importFrom tibble tibble
#' @keywords internal

## -----------------------------------------------------------------------------
## Main API: predict_library()
## -----------------------------------------------------------------------------

#' Predict Soil Properties Using Reference Library
#'
#' @description
#' Training-data-free prediction of standard soil properties using OSSL/KSSL
#' reference libraries. Automatically clusters unknowns, optimizes model configs,
#' and generates predictions with proper compositional handling for texture.
#'
#' @param spectra Tibble or data frame with unknown MIR spectra.
#'   Required columns:
#'   - Sample_ID: Character, unique sample identifier
#'   - Spectral columns: Numeric column names (600, 602, 604, ...)
#' @param property Character vector. One or more properties from LIBRARY_PROPERTIES.
#'   Texture properties (sand, silt, clay) are automatically expanded to all 3 components.
#' @param verbose Logical. Print tree-style progress? Default: TRUE.
#'
#' @return Tibble with predictions in long format (one row per sample-property):
#' \describe{
#'   \item{Sample_ID}{Sample identifier from input}
#'   \item{property}{Property name (e.g., "clay", "ph")}
#'   \item{pred}{Point prediction}
#'   \item{cluster_id}{GMM cluster assignment}
#'   \item{config_id}{Winning config used for this cluster}
#' }
#'
#' @section Texture Properties:
#' When any texture property (sand, silt, or clay) is requested, ALL THREE
#' are automatically predicted together using ILR transformation. This ensures
#' predictions sum to 100% (mass balance). A message will inform you of this
#' expansion.
#'
#' @examples
#' \dontrun{
#' # Single property
#' ph_pred <- predict_library(unknown_spectra, property = "ph")
#'
#' # Multiple properties
#' multi_pred <- predict_library(unknown_spectra, property = c("ph", "oc", "clay"))
#' # Note: "clay" auto-expands to c("sand", "silt", "clay")
#'
#' # Texture only
#' texture_pred <- predict_library(unknown_spectra, property = "sand")
#' # Returns predictions for sand, silt, AND clay (guaranteed to sum to 1000 g/kg)
#' }
#'
#' @keywords internal
predict_library <- function(spectra,
                           property,
                           debug_mode = FALSE,
                           verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("{cli::style_bold('Library-Based Prediction')}")
  }

  ## Validate spectra input ---------------------------------------------------

  if (!is.data.frame(spectra)) {
    cli::cli_abort(
      "spectra must be a data frame or tibble",
      "i" = "Received: {class(spectra)[1]}"
    )
  }

  if (!"Sample_ID" %in% names(spectra)) {
    cli::cli_abort(
      "spectra must have a 'Sample_ID' column",
      "i" = "Use: spectra %>% mutate(Sample_ID = row_number())"
    )
  }

  ## Check for spectral columns -----------------------------------------------

  spectral_cols <- grep("^[0-9]{3,4}$", names(spectra), value = TRUE)

  if (length(spectral_cols) == 0) {
    cli::cli_abort(
      "No spectral columns found (expected numeric: 600, 602, ...)",
      "i" = "Column names must be wavenumbers in cm⁻¹"
    )
  }

  ## Validate properties ------------------------------------------------------

  if (length(property) == 0) {
    cli::cli_abort("Must specify at least one property")
  }

  invalid_props <- setdiff(property, LIBRARY_PROPERTIES)

  if (length(invalid_props) > 0) {
    cli::cli_abort(
      "Invalid propert{?y/ies}: {paste(invalid_props, collapse = ', ')}",
      "i" = "Valid: {paste(LIBRARY_PROPERTIES, collapse = ', ')}"
    )
  }

  ## Auto-expand texture properties -------------------------------------------

  if (any(property %in% c("sand", "silt", "clay"))) {

    ## Check if user requested all 3 or just some
    texture_requested <- property[property %in% c("sand", "silt", "clay")]
    texture_complete  <- c("sand", "silt", "clay")

    if (!all(texture_complete %in% property)) {

      ## Expand to all 3 components
      property <- unique(c(setdiff(property, texture_requested), texture_complete))

      if (verbose) {
        cli::cli_text("│")
        cli::cli_text("├─ Texture is compositional - expanding to all 3 components:")
        cli::cli_text("│  └─ Predicting: sand, silt, clay (mass balance guaranteed)")
      }
    }
  }

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ Propert{?y/ies} to predict: {paste(property, collapse = ', ')}")
    cli::cli_text("│  └─ {nrow(spectra)} unknown sample{?s}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Process each property
  ## ---------------------------------------------------------------------------

  all_predictions <- list()

  for (prop in property) {

    if (verbose) {
      cli::cli_text("│")
      cli::cli_text("├─ {cli::style_bold('Processing:')} {prop}")
    }

    ## -------------------------------------------------------------------------
    ## Step 2.1: Load and prepare library for this property
    ## -------------------------------------------------------------------------

    ## Let prepare_library_for_training() output naturally (same tree level) ----

    library_prep <- prepare_library_for_training(
      property           = prop,
      k_range            = if (debug_mode) c(5) else c(5, 7, 9, 11),  # Single K for speed
      variance_threshold = 0.99,
      remove_water_bands = FALSE,
      max_samples        = if (debug_mode) 2000 else NULL,  # Larger for valid training
      seed               = 123,
      verbose            = verbose
    )

    ## -------------------------------------------------------------------------
    ## Step 2.2: Preprocess unknowns (same pipeline as library for clustering)
    ## -------------------------------------------------------------------------

    if (verbose) cli::cli_text("│  │")
    if (verbose) cli::cli_text("│  ├─ {cli::style_bold('Preprocessing unknowns')}...")

    ## Extract spectral columns only (no Sample_ID in preprocessing) -----------

    unknown_spectral <- spectra[, spectral_cols, drop = FALSE]

    ## Apply same preprocessing as library (SNV for clustering) ----------------

    unknown_preprocessed <- preprocess_library_spectra(
      spectral_data      = unknown_spectral,
      remove_water_bands = FALSE,
      verbose            = FALSE
    )

    if (is.null(unknown_preprocessed)) {
      cli::cli_abort("Failed to preprocess unknown spectra for {prop}")
    }

    if (verbose) {
      cli::cli_text("│  │  └─ {nrow(unknown_preprocessed)} spectra preprocessed")
    }

    ## -------------------------------------------------------------------------
    ## Step 2.3: Project unknowns into library PCA space
    ## -------------------------------------------------------------------------

    if (verbose) cli::cli_text("│  │")
    if (verbose) cli::cli_text("│  ├─ {cli::style_bold('Projecting to PCA space')}...")

    unknown_pca_scores <- project_to_library_pca(
      new_data  = unknown_preprocessed,
      pca_model = library_prep$pca_model,
      verbose   = FALSE
    )

    ## Subset to n_components used for clustering (GMM trained on subset) -------
    ## GMM was trained on the first n_components from PCA
    ## We need to match that dimension

    n_components_for_clustering <- library_prep$gmm_result$model$d  # d = dimensions in mclust

    if (!is.null(n_components_for_clustering) &&
        !is.na(n_components_for_clustering) &&
        ncol(unknown_pca_scores) > n_components_for_clustering) {
      unknown_pca_scores <- unknown_pca_scores[, 1:n_components_for_clustering, drop = FALSE]
    }

    if (is.null(unknown_pca_scores)) {
      cli::cli_abort("Failed to project unknowns to PCA space for {prop}")
    }

    if (verbose) {
      cli::cli_text("│  │  └─ {nrow(unknown_pca_scores)} samples × {ncol(unknown_pca_scores)} components")
    }

    ## -------------------------------------------------------------------------
    ## Step 2.4: Assign unknowns to clusters
    ## -------------------------------------------------------------------------

    if (verbose) cli::cli_text("│  │")
    if (verbose) cli::cli_text("│  ├─ {cli::style_bold('Assigning to clusters')}...")

    unknown_assignments <- assign_to_clusters(
      unknown_pca_scores = unknown_pca_scores,
      gmm_model          = library_prep$gmm_result,  # Pass full GMM result structure
      verbose            = FALSE
    )

    if (is.null(unknown_assignments)) {
      cli::cli_abort("Failed to assign unknowns to clusters for {prop}")
    }

    if (verbose) {
      cluster_counts <- table(unknown_assignments$cluster_id)
      cli::cli_text("│  │  └─ Assigned to {length(cluster_counts)} cluster{?s}")
    }

    ## -------------------------------------------------------------------------
    ## Step 2.5: Train models and predict per cluster
    ## -------------------------------------------------------------------------

    if (verbose) cli::cli_text("│  │")
    if (verbose) cli::cli_text("│  ├─ {cli::style_bold('Training and prediction')}")

    cluster_predictions <- list()
    unique_clusters     <- unique(unknown_assignments$cluster_id)

    for (clust_id in unique_clusters) {

      n_unknowns_in_cluster <- sum(unknown_assignments$cluster_id == clust_id)

      if (verbose) {
        cli::cli_text("│  │  │")
        cli::cli_text("│  │  ├─ {cli::style_bold('Cluster {clust_id}')} ({n_unknowns_in_cluster} unknown{?s})")
      }

      ## Get library data for this cluster -------------------------------------

      cluster_data <- library_prep$library_data_raw %>%
        dplyr::filter(cluster_id == clust_id)

      if (verbose) {
        cli::cli_text("│  │  │  ├─ Training data: {nrow(cluster_data)} samples")
      }

      ## ---------------------------------------------------------------------
      ## Step 2.5a: Prepare splits (with ILR if texture)
      ## ---------------------------------------------------------------------

      if (is_texture_property(prop)) {

        ## Texture: prepare splits for BOTH ILR coordinates ------------------

        splits_ilr1 <- prepare_cluster_splits(
          cluster_data   = cluster_data,
          property       = prop,
          ilr_coordinate = 1,
          seed           = 123
        )

        splits_ilr2 <- prepare_cluster_splits(
          cluster_data   = cluster_data,
          property       = prop,
          ilr_coordinate = 2,
          seed           = 123
        )

      } else {

        ## Non-texture: single split -----------------------------------------

        splits <- prepare_cluster_splits(
          cluster_data = cluster_data,
          property     = prop,
          seed         = 123
        )

      }

      ## ---------------------------------------------------------------------
      ## Step 2.5b: OPTIMIZE and TRAIN - Select best config and train final model
      ## ---------------------------------------------------------------------

      if (is_texture_property(prop)) {

        ## TEXTURE: Optimize and train BOTH ILR coordinates ------------------

        if (verbose) {
          cli::cli_text("│  │  │  │")
          cli::cli_text("│  │  │  ├─ {cli::style_bold('Training ilr_1 model')}...")
        }

        optimal_ilr1 <- optimize_config_for_cluster(
          cluster_splits     = splits_ilr1,
          property           = prop,
          n_configs_test     = if (debug_mode) 3 else 10,
          config_subset_prop = if (debug_mode) 0.5 else 0.2,
          quick_cv_folds     = if (debug_mode) 3 else 3,
          final_cv_folds     = if (debug_mode) 5 else 10,
          verbose            = verbose
        )

        if (verbose) {
          cli::cli_text("│  │  │  │")
          cli::cli_text("│  │  │  ├─ {cli::style_bold('Training ilr_2 model')}...")
        }

        ## Use same config for ilr_2 (for simplicity in v1.0) ----------------
        ## TODO Phase 2: Could optimize separately if needed

        optimal_ilr2 <- optimize_config_for_cluster(
          cluster_splits     = splits_ilr2,
          property           = prop,
          n_configs_test     = if (debug_mode) 3 else 10,
          config_subset_prop = if (debug_mode) 0.5 else 0.2,
          quick_cv_folds     = if (debug_mode) 3 else 3,
          final_cv_folds     = if (debug_mode) 5 else 10,
          verbose            = verbose
        )

        ## Bundle both trained models ----------------------------------------

        models <- list(
          ilr_1  = optimal_ilr1$final_workflow,
          ilr_2  = optimal_ilr2$final_workflow,
          config = optimal_ilr1$winning_config,
          type   = "texture"
        )

        winning_config <- optimal_ilr1$winning_config

      } else {

        ## NON-TEXTURE: Optimize and train single model ----------------------

        if (verbose) {
          cli::cli_text("│  │  │  │")
          cli::cli_text("│  │  │  ├─ {cli::style_bold('Optimizing and training')}...")
        }

        optimal_result <- optimize_config_for_cluster(
          cluster_splits     = splits,
          property           = prop,
          n_configs_test     = if (debug_mode) 3 else 10,  # Fewer configs in debug
          config_subset_prop = if (debug_mode) 0.5 else 0.2,  # Larger subset
          quick_cv_folds     = if (debug_mode) 3 else 3,
          final_cv_folds     = if (debug_mode) 5 else 10,  # Fewer folds
          verbose            = verbose
        )

        model          <- optimal_result$final_workflow
        winning_config <- optimal_result$winning_config

      }

      if (verbose) {
        config_desc <- paste0(winning_config$model, " | ",
                             winning_config$preprocessing, " | ",
                             winning_config$feature_selection)
        cli::cli_text("│  │  │  │  └─ Config: {config_desc}")
      }

      ## ---------------------------------------------------------------------
      ## Step 2.5c: PREDICT on unknowns in this cluster
      ## ---------------------------------------------------------------------

      ## Get unknowns for this cluster ----------------------------------------

      unknowns_in_cluster <- spectra %>%
        dplyr::mutate(cluster_id_temp = unknown_assignments$cluster_id) %>%
        dplyr::filter(cluster_id_temp == clust_id) %>%
        dplyr::select(-cluster_id_temp)

      ## Generate predictions -------------------------------------------------

      if (is_texture_property(prop)) {

        predictions <- predict_texture_from_models(
          unknowns   = unknowns_in_cluster,
          models     = models,
          property   = prop,
          cluster_id = clust_id,
          config     = winning_config,
          verbose    = verbose
        )

      } else {

        predictions <- predict_standard_from_model(
          unknowns   = unknowns_in_cluster,
          model      = model,
          property   = prop,
          cluster_id = clust_id,
          config     = winning_config,
          verbose    = verbose
        )

      }

      ## Store cluster predictions --------------------------------------------

      cluster_predictions[[paste0("cluster_", clust_id)]] <- predictions

    }

    ## Combine predictions from all clusters ----------------------------------

    prop_predictions <- dplyr::bind_rows(cluster_predictions)

    ## Store results for this property ----------------------------------------

    all_predictions[[prop]] <- prop_predictions

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Combine and return results
  ## ---------------------------------------------------------------------------

  dplyr::bind_rows(all_predictions)

}

## -----------------------------------------------------------------------------
