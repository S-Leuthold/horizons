# Library-Based Prediction Orchestrator
#
# Main user-facing API for library-based prediction. Orchestrates the complete
# workflow: load OSSL library, cluster unknowns, optimize configs per cluster,
# train models, generate predictions with proper target handling.

#' @importFrom cli cli_text cli_alert_info cli_alert_success cli_abort style_bold
#' @importFrom dplyr filter bind_rows mutate left_join
#' @importFrom tibble tibble
NULL

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
#' @param remove_water_bands Logical. Remove water absorption bands from spectra? Default: TRUE.
#' @param debug_mode Logical. Enable debug output for troubleshooting? Default: FALSE.
#' @param allow_par Logical. Allow parallel processing? Default: TRUE.
#' @param n_workers Integer. Number of parallel workers. Default: 4.
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
                           with_uq            = TRUE,
                           abstain_ood        = FALSE,
                           remove_water_bands = TRUE,
                           debug_mode         = FALSE,
                           allow_par          = TRUE,
                           n_workers          = 4,
                           verbose            = TRUE) {

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

  properties_requested <- property  # Save original request for output filtering

  if (any(property %in% c("sand", "silt", "clay"))) {

    texture_requested <- property[property %in% c("sand", "silt", "clay")]
    texture_complete  <- c("sand", "silt", "clay")

    if (!all(texture_complete %in% property)) {

      ## Partial request - expand to all 3 (needed for ILR)
      property <- unique(c(setdiff(property, texture_requested), texture_complete))

      if (verbose) {
        cli::cli_text("│")
        cli::cli_text("├─ Texture: ILR requires all 3 components (sand, silt, clay)")
        cli::cli_text("│  └─ Will return: {paste(texture_requested, collapse = ', ')}")
      }

    } else {

      ## All 3 requested - inform about compositional handling
      if (verbose) {
        cli::cli_text("│")
        cli::cli_text("├─ Texture: Using ILR transformation for mass balance")
      }
    }
  }

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ Propert{?y/ies} to predict: {paste(property, collapse = ', ')}")
    cli::cli_text("│  └─ {nrow(spectra)} unknown sample{?s}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Deduplicate properties (texture as single unit)
  ## ---------------------------------------------------------------------------

  ## Group texture properties into single processing unit ----------------------

  properties_to_process <- property

  if (any(property %in% c("sand", "silt", "clay"))) {
    ## Remove individual texture props, add "texture" marker
    properties_to_process <- c(
      setdiff(property, c("sand", "silt", "clay")),
      "texture"
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Process each property (or texture group)
  ## ---------------------------------------------------------------------------

  all_predictions <- list()

  for (prop in properties_to_process) {

    ## Determine display name for verbose output --------------------------------

    if (prop == "texture") {
      texture_requested <- properties_requested[properties_requested %in% c("sand", "silt", "clay")]
      texture_list <- paste(texture_requested, collapse = ", ")
      display_msg <- glue::glue("Texture (requested: {texture_list})")
      prop_for_library <- "sand"  # Any texture component works (all share library)
    } else {
      display_msg <- prop
      prop_for_library <- prop
    }

    if (verbose) {
      cli::cli_text("│")
      cli::cli_text("├─ {cli::style_bold('Processing:')} {display_msg}")
    }

    ## -------------------------------------------------------------------------
    ## Step 2.1: Load and prepare library for this property
    ## -------------------------------------------------------------------------

    if (verbose) {
      cli::cli_text("│  │")
      cli::cli_text("│  ├─ Loading library...")
    }

    library_prep <- prepare_library_for_training(
      property           = prop_for_library,
      k_range            = if (debug_mode) c(5) else c(5, 7, 9, 11),
      variance_threshold = 0.99,
      remove_water_bands = remove_water_bands,  # User-controlled
      max_samples        = if (debug_mode) 2000 else NULL,
      seed               = 123,
      verbose            = verbose  # Show library loading progress
    )

    if (verbose) {
      cli::cli_text("│  │  └─ {library_prep$n_samples} samples, {library_prep$n_clusters} clusters")
    }

    ## -------------------------------------------------------------------------
    ## Step 2.2: Preprocess and assign unknowns
    ## -------------------------------------------------------------------------

    if (verbose) {
      cli::cli_text("│  │")
      cli::cli_text("│  ├─ Preprocessing unknowns...")
    }

    ## Extract spectral columns only -------------------------------------------

    unknown_spectral <- spectra[, spectral_cols, drop = FALSE]

    ## Apply same preprocessing as library (SNV for clustering) ----------------

    unknown_preprocessed <- preprocess_library_spectra(
      spectral_data      = unknown_spectral,
      remove_water_bands = remove_water_bands,  # Match library preprocessing!
      verbose            = FALSE
    )

    if (is.null(unknown_preprocessed)) {
      cli::cli_abort("Failed to preprocess unknown spectra")
    }

    ## Project to PCA space ----------------------------------------------------

    unknown_pca_scores <- project_to_library_pca(
      new_data  = unknown_preprocessed,
      pca_model = library_prep$pca_model,
      verbose   = FALSE
    )

    ## Subset to match GMM dimensions ------------------------------------------

    n_components_for_clustering <- library_prep$gmm_result$model$d

    if (!is.null(n_components_for_clustering) &&
        !is.na(n_components_for_clustering) &&
        ncol(unknown_pca_scores) > n_components_for_clustering) {
      unknown_pca_scores <- unknown_pca_scores[, 1:n_components_for_clustering, drop = FALSE]
    }

    if (is.null(unknown_pca_scores)) {
      cli::cli_abort("Failed to project unknowns to PCA space")
    }

    ## Assign to clusters ------------------------------------------------------

    unknown_assignments <- assign_to_clusters(
      unknown_pca_scores = unknown_pca_scores,
      gmm_model          = library_prep$gmm_result,
      verbose            = FALSE
    )

    if (is.null(unknown_assignments)) {
      cli::cli_abort("Failed to assign unknowns to clusters")
    }

    if (verbose) {
      n_clusters_assigned <- length(unique(unknown_assignments$cluster_id))
      cli::cli_text("│  │  ├─ SNV → PCA ({ncol(unknown_pca_scores)} components)")
      cli::cli_text("│  │  └─ Cluster assignments:")

      ## Show distribution and confidence for each cluster
      cluster_summary <- unknown_assignments %>%
        dplyr::group_by(cluster_id) %>%
        dplyr::summarise(
          n = dplyr::n(),
          avg_prob = mean(probability, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n))

      for (i in 1:nrow(cluster_summary)) {
        cs <- cluster_summary[i, ]
        pct <- round(100 * cs$n / nrow(unknown_assignments))
        conf_flag <- if (cs$avg_prob < 0.7) " ⚠" else ""
        cli::cli_text("│  │     • Cluster {cs$cluster_id}: {cs$n} sample{?s} ({pct}%) - confidence: {round(cs$avg_prob, 2)}{conf_flag}")
      }
    }

    ## -------------------------------------------------------------------------
    ## Step 2.4: Calculate AD in PCA/Clustering Space (not model space!)
    ## -------------------------------------------------------------------------

    ## Extract GMM centroids and covariances (already in PCA space)
    gmm_centroids <- library_prep$gmm_result$centroids  # (n_clusters × n_components)
    gmm_covs      <- library_prep$gmm_result$covariances  # (n_components × n_components × n_clusters)

    ## Calculate Mahalanobis distance for each unknown to its assigned cluster
    unknown_ad_distances <- sapply(1:nrow(unknown_pca_scores), function(i) {

      clust_id <- unknown_assignments$cluster_id[i]
      unknown_coords <- unknown_pca_scores[i, ]
      cluster_centroid <- gmm_centroids[clust_id, ]
      cluster_cov <- gmm_covs[, , clust_id]

      ## Mahalanobis distance in PCA space
      stats::mahalanobis(unknown_coords, center = cluster_centroid, cov = cluster_cov)

    })

    ## Add to unknown_assignments
    unknown_assignments$ad_distance_pca <- unknown_ad_distances

    if (verbose) {
      cli::cli_text("│  │")
      cli::cli_text("│  ├─ AD distances calculated in clustering space (PCA)")
      cli::cli_text("│  │  └─ Median distance: {round(median(unknown_ad_distances), 1)}")
    }

    ## -------------------------------------------------------------------------
    ## Step 2.4.5: Reassign unknowns from sparse clusters to nearest neighbors
    ## -------------------------------------------------------------------------

    min_unknown_size <- 20  # Don't train a model for < 20 unknowns (inefficient)

    ## Get unknown counts per cluster
    cluster_unknown_counts <- unknown_assignments %>%
      dplyr::group_by(cluster_id) %>%
      dplyr::summarise(n_unknowns = dplyr::n(), .groups = "drop")

    small_clusters <- cluster_unknown_counts %>%
      dplyr::filter(n_unknowns < min_unknown_size) %>%
      dplyr::pull(cluster_id)

    if (length(small_clusters) > 0) {

      if (verbose) {
        cli::cli_text("│  │")
        cli::cli_text("│  ├─ Reassigning unknowns from small clusters...")
      }

      for (small_clust in small_clusters) {

        ## Find unknowns in this small cluster
        unknowns_in_small <- which(unknown_assignments$cluster_id == small_clust)

        if (length(unknowns_in_small) == 0) next

        ## Find nearest large cluster for each unknown
        for (idx in unknowns_in_small) {

          ## Get PCA coords for this unknown
          unknown_coords <- unknown_pca_scores[idx, , drop = FALSE]

          ## Calculate distances to all LARGE cluster centroids
          large_clusters <- setdiff(unique(unknown_assignments$cluster_id), small_clusters)

          min_dist <- Inf
          nearest_cluster <- NA

          for (large_clust in large_clusters) {

            ## Get centroid of large cluster
            large_clust_samples <- which(unknown_assignments$cluster_id == large_clust)
            centroid <- colMeans(unknown_pca_scores[large_clust_samples, , drop = FALSE])

            ## Euclidean distance
            dist <- sqrt(sum((unknown_coords - centroid)^2))

            if (dist < min_dist) {
              min_dist <- dist
              nearest_cluster <- large_clust
            }
          }

          ## Reassign to nearest large cluster
          unknown_assignments$cluster_id[idx] <- nearest_cluster

        }

        if (verbose) {
          cli::cli_text("│  │  ├─ Cluster {small_clust}: {length(unknowns_in_small)} unknown{?s} → nearest large cluster")
        }

      }

      if (verbose) {
        cli::cli_text("│  │  └─ Reassignment complete")
      }

    }

    ## -------------------------------------------------------------------------
    ## Step 2.5: Train models and predict per cluster
    ## -------------------------------------------------------------------------

    if (verbose) {
      cli::cli_text("│  │")
      cli::cli_text("│  └─ Training & prediction")
    }

    cluster_predictions <- list()
    unique_clusters     <- unique(unknown_assignments$cluster_id)

    for (clust_id in unique_clusters) {

      n_unknowns_in_cluster <- sum(unknown_assignments$cluster_id == clust_id)

      cluster_data <- library_prep$library_data_raw %>%
        dplyr::filter(cluster_id == clust_id)

      if (verbose) {
        cli::cli_text("│     │")
        cli::cli_text("│     ├─ Cluster {clust_id}: {n_unknowns_in_cluster} unknown{?s}, {nrow(cluster_data)} training")
      }

      ## ---------------------------------------------------------------------
      ## Step 2.5a: Prepare splits (with ILR if texture)
      ## ---------------------------------------------------------------------

      if (prop == "texture") {

        ## Texture: prepare splits for BOTH ILR coordinates ------------------

        splits_ilr1 <- prepare_cluster_splits(
          cluster_data   = cluster_data,
          property       = prop_for_library,  # Use "sand" not "texture"
          ilr_coordinate = 1,
          seed           = 123
        )

        splits_ilr2 <- prepare_cluster_splits(
          cluster_data   = cluster_data,
          property       = prop_for_library,  # Use "sand" not "texture"
          ilr_coordinate = 2,
          seed           = 123
        )

      } else {

        ## Non-texture: single split -----------------------------------------

        splits <- prepare_cluster_splits(
          cluster_data = cluster_data,
          property     = prop_for_library,
          seed         = 123
        )

      }

      ## ---------------------------------------------------------------------
      ## Step 2.5b: OPTIMIZE and TRAIN - Select best config and train final model
      ## ---------------------------------------------------------------------

      if (prop == "texture") {

        ## TEXTURE: Optimize and train BOTH ILR coordinates ------------------

        optimal_ilr1 <- optimize_config_for_cluster(
          cluster_splits     = splits_ilr1,
          property           = prop_for_library,
          n_configs_test     = if (debug_mode) 4 else 10,
          config_subset_prop = if (debug_mode) 0.5 else 0.2,
          quick_grid_size    = 5,
          final_grid_size    = 10,
          quick_cv_folds     = if (debug_mode) 3 else 3,
          final_cv_folds     = if (debug_mode) 5 else 10,
          allow_par          = allow_par,
          n_workers          = n_workers,
          verbose            = verbose
        )

        optimal_ilr2 <- optimize_config_for_cluster(
          cluster_splits     = splits_ilr2,
          property           = prop_for_library,
          n_configs_test     = if (debug_mode) 4 else 10,
          config_subset_prop = if (debug_mode) 0.5 else 0.2,
          quick_grid_size    = 5,
          final_grid_size    = 10,
          quick_cv_folds     = if (debug_mode) 3 else 3,
          final_cv_folds     = if (debug_mode) 5 else 10,
          allow_par          = allow_par,
          n_workers          = n_workers,
          verbose            = verbose
        )

        winning_config <- optimal_ilr1$winning_config

        ## Train with UQ if requested -------------------------------------------

        if (with_uq) {

          ## Train UQ models for ILR coordinate 1
          uq_models_ilr1 <- train_cluster_models_with_uq(
            cluster_data = splits_ilr1$training_pool,
            property     = prop_for_library,
            config       = optimal_ilr1$winning_config,
            cv_folds     = if (debug_mode) 5 else 10,
            verbose      = verbose
          )

          ## Train UQ models for ILR coordinate 2
          uq_models_ilr2 <- train_cluster_models_with_uq(
            cluster_data = splits_ilr2$training_pool,
            property     = prop_for_library,
            config       = optimal_ilr2$winning_config,
            cv_folds     = if (debug_mode) 5 else 10,
            verbose      = verbose
          )

        } else {

          ## No UQ: bundle optimized workflows (backward compatibility)
          models <- list(
            ilr_1  = optimal_ilr1$final_workflow,
            ilr_2  = optimal_ilr2$final_workflow,
            config = optimal_ilr1$winning_config,
            type   = "texture"
          )

        }

      } else {

        ## NON-TEXTURE: Optimize and train single model ----------------------

        optimal_result <- optimize_config_for_cluster(
          cluster_splits     = splits,
          property           = prop_for_library,
          n_configs_test     = if (debug_mode) 4 else 10,
          config_subset_prop = if (debug_mode) 0.5 else 0.2,
          quick_grid_size    = 5,
          final_grid_size    = 10,
          quick_cv_folds     = if (debug_mode) 3 else 3,
          final_cv_folds     = if (debug_mode) 5 else 10,
          allow_par          = allow_par,
          n_workers          = n_workers,
          verbose            = verbose
        )

        winning_config <- optimal_result$winning_config

        ## Train with UQ if requested -------------------------------------------

        if (with_uq) {

          uq_models <- train_cluster_models_with_uq(
            cluster_data = splits$training_pool,
            property     = prop_for_library,
            config       = winning_config,
            cv_folds     = if (debug_mode) 5 else 10,
            verbose      = verbose
          )

        } else {

          ## No UQ: just use optimized workflow (backward compatibility)
          model <- optimal_result$final_workflow

        }

      }

      ## Optimization output shows model details, no need for duplicate summary

      ## ---------------------------------------------------------------------
      ## Step 2.5c: PREDICT on unknowns in this cluster
      ## ---------------------------------------------------------------------

      ## Get unknowns for this cluster ----------------------------------------

      unknowns_in_cluster <- spectra %>%
        dplyr::mutate(cluster_id_temp = unknown_assignments$cluster_id) %>%
        dplyr::filter(cluster_id_temp == clust_id) %>%
        dplyr::select(-cluster_id_temp)

      ## Generate predictions -------------------------------------------------

      if (prop == "texture") {

        ## Texture prediction ---------------------------------------------------

        if (with_uq) {

          ## Add Project column if missing
          unknowns_prepared <- unknowns_in_cluster
          if (!"Project" %in% names(unknowns_prepared)) {
            unknowns_prepared <- unknowns_prepared %>%
              dplyr::mutate(Project = "library")
          }

          ## Predict ILR coordinate 1 with UQ
          ilr1_preds <- predict_with_uq(
            point_workflow    = uq_models_ilr1$point_model,
            quantile_workflow = uq_models_ilr1$quantile_model,
            new_data          = unknowns_prepared,
            quantiles         = c(0.05, 0.95),
            c_alpha           = uq_models_ilr1$c_alpha,
            ad_metadata       = uq_models_ilr1$ad_metadata,
            abstain_ood       = abstain_ood,
            repair_crossings  = TRUE
          )

          ## Predict ILR coordinate 2 with UQ
          ilr2_preds <- predict_with_uq(
            point_workflow    = uq_models_ilr2$point_model,
            quantile_workflow = uq_models_ilr2$quantile_model,
            new_data          = unknowns_prepared,
            quantiles         = c(0.05, 0.95),
            c_alpha           = uq_models_ilr2$c_alpha,
            ad_metadata       = uq_models_ilr2$ad_metadata,
            abstain_ood       = abstain_ood,
            repair_crossings  = TRUE
          )

          ## Back-transform ILR to texture with intervals
          texture_preds <- ilr_to_texture(
            ilr_1 = ilr1_preds$.pred,
            ilr_2 = ilr2_preds$.pred,
            as_gkg = TRUE
          )

          ## For intervals: back-transform lower and upper bounds separately
          texture_lower <- ilr_to_texture(
            ilr_1 = ilr1_preds$.pred_lower,
            ilr_2 = ilr2_preds$.pred_lower,
            as_gkg = TRUE
          )

          texture_upper <- ilr_to_texture(
            ilr_1 = ilr1_preds$.pred_upper,
            ilr_2 = ilr2_preds$.pred_upper,
            as_gkg = TRUE
          )

          ## Format as long-format predictions (3 rows per sample)
          predictions <- tibble::tibble(
            Sample_ID   = rep(unknowns_prepared$Sample_ID, 3),
            property    = rep(c("sand", "silt", "clay"), each = nrow(texture_preds)),
            .pred       = c(texture_preds$sand, texture_preds$silt, texture_preds$clay),
            .pred_lower = c(texture_lower$sand, texture_lower$silt, texture_lower$clay),
            .pred_upper = c(texture_upper$sand, texture_upper$silt, texture_upper$clay),
            ad_distance = rep(ilr1_preds$ad_distance, 3),  # Use ILR1 distance
            ad_bin      = rep(ilr1_preds$ad_bin, 3),
            cluster_id  = clust_id,
            config_id   = paste0(winning_config$model, "_",
                                winning_config$preprocessing, "_",
                                winning_config$feature_selection)
          )

        } else {

          ## No UQ: use simple texture prediction
          predictions <- predict_texture_from_models(
            unknowns   = unknowns_in_cluster,
            models     = models,
            property   = prop_for_library,
            cluster_id = clust_id,
            config     = winning_config,
            verbose    = verbose
          )

        }

      } else {

        ## Non-texture prediction -----------------------------------------------

        if (with_uq) {

          ## Add Project column if missing (required by workflows)
          unknowns_prepared <- unknowns_in_cluster
          if (!"Project" %in% names(unknowns_prepared)) {
            unknowns_prepared <- unknowns_prepared %>%
              dplyr::mutate(Project = "library")
          }

          ## UQ prediction: use trained UQ models
          predictions <- predict_with_uq(
            point_workflow    = uq_models$point_model,
            quantile_workflow = uq_models$quantile_model,
            new_data          = unknowns_prepared,
            quantiles         = c(0.05, 0.95),
            c_alpha           = uq_models$c_alpha,
            ad_metadata       = uq_models$ad_metadata,
            abstain_ood       = abstain_ood,
            repair_crossings  = TRUE
          ) %>%
            dplyr::mutate(
              property   = prop_for_library,
              cluster_id = clust_id,
              config_id  = paste0(winning_config$model, "_",
                                 winning_config$preprocessing, "_",
                                 winning_config$feature_selection)
            )

        } else {

          ## No UQ: simple point prediction
          predictions <- predict_standard_from_model(
            unknowns   = unknowns_in_cluster,
            model      = model,
            property   = prop_for_library,
            cluster_id = clust_id,
            config     = winning_config,
            verbose    = verbose
          )

        }

      }

      ## Summary for this cluster ----------------------------------------------

      if (verbose) {
        cli::cli_text("│     │  └─ Predicted {nrow(predictions)} sample{?s}")
      }

      cluster_predictions[[paste0("cluster_", clust_id)]] <- predictions

    }

    ## Combine predictions from all clusters ----------------------------------

    prop_predictions <- dplyr::bind_rows(cluster_predictions)

    ## Replace model-space AD with PCA-space AD (more accurate for clustering-based assignment)
    if ("ad_distance_pca" %in% names(unknown_assignments) && "ad_distance" %in% names(prop_predictions)) {

      ad_lookup <- unknown_assignments %>%
        dplyr::mutate(sample_index = dplyr::row_number()) %>%
        dplyr::select(sample_index, ad_distance_pca)

      ## Match by Sample_ID order (unknowns maintain same order)
      prop_predictions <- prop_predictions %>%
        mutate(sample_index = match(Sample_ID, spectra$Sample_ID)) %>%
        left_join(ad_lookup, by = "sample_index") %>%
        mutate(ad_distance = ad_distance_pca) %>%  # Override model-space distance
        select(-sample_index, -ad_distance_pca)

      if (verbose) {
        cli::cli_text("│     └─ AD distances replaced with PCA-space values")
      }

    }

    if (verbose) {
      cli::cli_text("│     └─ Total: {nrow(prop_predictions)} prediction{?s}")
    }

    ## Store results for this property ----------------------------------------

    all_predictions[[prop]] <- prop_predictions

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Combine and filter to requested properties
  ## ---------------------------------------------------------------------------

  all_results <- dplyr::bind_rows(all_predictions)

  ## Filter to originally requested properties (texture auto-expansion) ---------

  all_results %>%
    dplyr::filter(property %in% properties_requested)

}

## -----------------------------------------------------------------------------
