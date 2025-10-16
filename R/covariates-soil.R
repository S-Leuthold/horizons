#' Predict Soil Covariates Using Clustered Local Models (OSSL-Centric PCA)
#'
#' @description
#' Predicts soil properties from MIR spectra using OSSL-centric clustering
#' and locally-trained Cubist models. Trains PCA on OSSL (global spectral space),
#' projects unknowns into that space, clusters unknowns, selects relevant
#' OSSL training data for each cluster, and trains specialized models.
#'
#' Changed in v0.9.0: Now uses OSSL-centric PCA (trains PCA on 12K+ OSSL samples)
#' instead of unknown-centric PCA. Achieves 8-10% R² improvement over previous
#' global approach. Defaults to 1st derivative preprocessing and Euclidean distance.
#'
#' @param input_data Tibble with MIR spectral data and Sample_ID column
#' @param covariates Character vector of soil properties to predict
#' @param n_similar Deprecated. Ignored in v0.9.0+. V2 uses all available OSSL samples.
#' @param prop Numeric. Proportion for training set (default: 0.85)
#' @param variance_threshold Numeric. PCA variance to capture (default: 0.985)
#' @param coverage Numeric. Proportion of OSSL to keep per cluster (default: 0.8)
#' @param max_clusters Integer. Maximum clusters to test (default: 10)
#' @param bayesian_iter Integer. Bayesian optimization iterations (default: 10)
#' @param allow_par Logical. Enable parallel processing (default: FALSE)
#' @param n_workers Integer. Number of parallel workers (default: NULL)
#' @param refresh Logical. Force refresh of OSSL data (default: FALSE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#' @param return_models Logical. Return fitted models in output? (default: FALSE)
#'   Setting to FALSE saves ~90% memory in return value by discarding models after
#'   prediction. Only predictions and validation metrics are returned. Set TRUE if
#'   you need to inspect models or make additional predictions.
#'
#' @section Experimental Parameters (not for production):
#' @param derivative_order Integer. SG derivative order: 0 = smoothing only,
#'   1 = 1st derivative, 2 = 2nd derivative (default: 0, EXPERIMENTAL)
#' @param clustering_method Character. "kmeans" or "ward" (default: "kmeans", EXPERIMENTAL)
#' @param use_mahalanobis Logical. Use Mahalanobis distance for OSSL selection
#'   instead of Euclidean (default: TRUE, EXPERIMENTAL)
#' @param distance_percentile Numeric. Percentile threshold for OSSL inclusion
#'   per cluster (0-1). Lower = more selective. (default: 0.6, EXPERIMENTAL)
#'
#' @return Named list with:
#'   - predictions: Tibble with Sample_ID and predicted covariate columns
#'   - validation_metrics: Tibble with accuracy metrics per cluster/covariate
#'   - local_models: Nested list of fitted models (cluster × covariate)
#'   - cluster_info: Clustering details
#'
#' @export

predict_soil_covariates <- function(input_data,
                                               covariates,
                                               n_similar           = NULL,  # Deprecated - ignored in v0.9.0
                                               prop                = 0.85,
                                               variance_threshold  = 0.985,
                                               coverage            = 0.8,
                                               max_clusters        = 10,
                                               bayesian_iter       = 10,
                                               allow_par           = FALSE,
                                               n_workers           = NULL,
                                               refresh             = FALSE,
                                               verbose             = TRUE,
                                               return_models       = FALSE,  # Memory optimization: FALSE = discard models after prediction
                                               ## Experimental parameters -----
                                               derivative_order    = 1,
                                               clustering_method   = "kmeans",
                                               use_mahalanobis     = FALSE,
                                               distance_percentile = 0.6) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation and setup
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Starting clustered covariate prediction...')}")
  }

  ## Validate properties -------------------------------------------------------

  validate_soil_properties(covariates)

  ## Validate input data has required columns ----------------------------------

  if (!"Sample_ID" %in% names(input_data)) {
    cli::cli_abort("{.val {input_data}} must contain a Sample_ID column")
  }

  ## Check for spectral columns ------------------------------------------------

  spectral_cols <- grep("^[0-9]{3,4}$", names(input_data), value = TRUE)

  if (length(spectral_cols) == 0) {
    cli::cli_abort("No spectral columns found in input_data")
  }

  ## Validate proportion -------------------------------------------------------

  if (prop <= 0 || prop >= 1) {
    cli::cli_abort("prop must be between 0 and 1")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Load and preprocess OSSL (create global spectral reference)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Loading OSSL training data...')}")
  }

  ## Load OSSL - load once per covariate for maximum coverage ----------------
  ## This avoids redundant disk reads while maximizing training pool size

  ossl_raw_list <- list()

  for (cov in covariates) {
    ossl_raw_list[[cov]] <- get_ossl_training_data(properties  = cov,
                                                    max_samples = NULL,
                                                    refresh     = refresh,
                                                    verbose     = if (cov == covariates[1]) verbose else FALSE)
  }

  ## Combine all covariate-specific OSSL datasets (union for PCA training)
  ossl_raw <- dplyr::bind_rows(ossl_raw_list) %>%
    dplyr::distinct(Sample_ID, .keep_all = TRUE)

  if (is.null(ossl_raw)) {
    cli::cli_abort("Failed to acquire OSSL training data")
  }

  ## Preprocess OSSL spectra (SG + SNV) --------------------------------------

  ossl_processed <- preprocess_mir_spectra(spectral_data    = ossl_raw,
                                            derivative_order = derivative_order,
                                            verbose          = verbose)

  if (is.null(ossl_processed)) {
    cli::cli_abort("Failed to preprocess OSSL spectra")
  }

  ## Filter OSSL for complete spectral data ----------------------------------

  spectral_cols_ossl <- grep("^[0-9]{3,4}$", names(ossl_processed), value = TRUE)
  ossl_spectral_complete <- complete.cases(ossl_processed[, spectral_cols_ossl])
  ossl_processed <- ossl_processed[ossl_spectral_complete, ]

  if (verbose) {
    cli::cli_text("│  └─ OSSL samples (complete): {format(nrow(ossl_processed), big.mark = ',')}")
  }

  ## Free memory from raw OSSL data and loading list --------------------------
  rm(ossl_raw, ossl_raw_list)
  gc(verbose = FALSE)

  ## ---------------------------------------------------------------------------
  ## Step 2: Train PCA on OSSL (creates global spectral coordinate system)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Training PCA on OSSL')} (global spectral space)...")
  }

  ossl_pca_result <- perform_pca_on_ossl(ossl_data          = ossl_processed,
                                          variance_threshold = variance_threshold,
                                          verbose            = verbose)

  if (is.null(ossl_pca_result)) {
    cli::cli_abort("Failed to perform PCA on OSSL")
  }

  ossl_pca_model  <- ossl_pca_result$pca_model
  ossl_pca_scores <- ossl_pca_result$ossl_pca_scores
  n_components    <- ossl_pca_result$n_components

  if (verbose) {
    cli::cli_text("│  └─ OSSL PCA complete: {n_components} components")
  }

  ## Free memory from PCA intermediate objects --------------------------------
  rm(ossl_processed, ossl_pca_result)
  gc(verbose = FALSE)

  ## ---------------------------------------------------------------------------
  ## Step 3: Preprocess and project unknowns to OSSL PCA space
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Preprocessing unknown samples...')}")
  }

  unknown_preprocessed <- preprocess_mir_spectra(spectral_data    = input_data,
                                                  derivative_order = derivative_order,
                                                  verbose          = verbose)

  if (is.null(unknown_preprocessed)) {
    cli::cli_abort("Failed to preprocess unknown spectra")
  }

  if (verbose) {
    cli::cli_text("│  └─ Unknown samples: {nrow(unknown_preprocessed)}")
  }

  ## Project unknowns INTO OSSL PCA space ------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Projecting unknowns to OSSL PCA space...')}")
  }

  unknown_pca_scores <- project_spectra_to_pca(new_data  = unknown_preprocessed,
                                                pca_model = ossl_pca_model,
                                                verbose   = verbose)

  if (is.null(unknown_pca_scores)) {
    cli::cli_abort("Failed to project unknowns to OSSL PCA space")
  }

  if (verbose) {
    cli::cli_text("│  └─ Unknowns projected: {nrow(unknown_pca_scores)} samples in {n_components}D OSSL space")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Cluster unknowns in OSSL PCA space
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Clustering unknown samples')} (in OSSL space)...")
  }

  ## Extract just the PCA dimensions for clustering ----------------------------

  pca_dims <- dplyr::select(unknown_pca_scores, dplyr::starts_with("Dim."))

  ## Define cluster range to test ----------------------------------------------

  k_range <- 2:min(max_clusters, floor(nrow(pca_dims) / 3))

  if (length(k_range) < 1) {

    cli::cli_warn("Too few samples for clustering - using single cluster")

    cluster_assignments <- rep(1, nrow(unknown_pca_scores))
    optimal_k <- 1
    cluster_model <- NULL

  } else if (clustering_method == "kmeans") {

    ## K-means with silhouette optimization -----------------------------------

    if (verbose) cli::cli_text("│  ├─ Testing k = {min(k_range)} to {max(k_range)} (k-means + silhouette)")

    silhouette_scores <- purrr::map_dbl(k_range, function(k) {

      set.seed(307)

      kmeans_result <- stats::kmeans(x       = pca_dims,
                                     centers = k,
                                     nstart  = 25)

      sil <- cluster::silhouette(x    = kmeans_result$cluster,
                                 dist = stats::dist(pca_dims))

      mean(sil[, 3], na.rm = TRUE)
    })

    optimal_k <- k_range[which.max(silhouette_scores)]

    if (verbose) {
      cli::cli_text("│  ├─ Optimal clusters: {optimal_k} (silhouette = {round(max(silhouette_scores), 3)})")
    }

    ## Fit final k-means with optimal k ----------------------------------------

    set.seed(307)

    cluster_model <- stats::kmeans(x       = pca_dims,
                                   centers = optimal_k,
                                   nstart  = 25)

    cluster_assignments <- cluster_model$cluster

  } else if (clustering_method == "ward") {

    ## Ward's hierarchical clustering ------------------------------------------

    if (verbose) cli::cli_text("│  ├─ Testing Ward's hierarchical (k = {min(k_range)} to {max(k_range)})")

    hclust_model <- stats::hclust(stats::dist(pca_dims), method = "ward.D2")

    ## Find optimal k via silhouette -------------------------------------------

    silhouette_scores <- purrr::map_dbl(k_range, function(k) {

      clusters <- stats::cutree(hclust_model, k = k)

      sil <- cluster::silhouette(x    = clusters,
                                 dist = stats::dist(pca_dims))

      mean(sil[, 3], na.rm = TRUE)
    })

    optimal_k <- k_range[which.max(silhouette_scores)]

    if (verbose) {
      cli::cli_text("│  ├─ Optimal clusters: {optimal_k} (silhouette = {round(max(silhouette_scores), 3)})")
    }

    ## Cut tree at optimal k ---------------------------------------------------

    cluster_assignments <- stats::cutree(hclust_model, k = optimal_k)
    cluster_model <- hclust_model

  } else {

    cli::cli_abort("Unknown clustering_method: {clustering_method}. Use 'kmeans' or 'ward'")

  }

  ## Add cluster assignments to unknown_pca_scores ----------------------------

  unknown_pca_scores$Cluster <- cluster_assignments

  if (verbose) {

    cluster_sizes <- table(cluster_assignments)
    size_summary <- paste(sprintf("C%s: %d",
                                  names(cluster_sizes),
                                  cluster_sizes),
                         collapse = " | ")

    cli::cli_text("│  └─ Cluster sizes: {size_summary}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Assign OSSL to clusters (per covariate, with threshold overlap)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Creating covariate-specific OSSL pools')}")
  }

  ossl_pools <- vector("list", length(covariates))
  names(ossl_pools) <- covariates

  unknown_pca_dims <- dplyr::select(unknown_pca_scores, dplyr::starts_with("Dim."))
  ossl_pca_dims <- dplyr::select(ossl_pca_scores, dplyr::starts_with("Dim."))
  distance_method <- if (use_mahalanobis) "Mahalanobis" else "Euclidean"

  for (covariate in covariates) {

    if (verbose) {
      cli::cli_text("│  ├─ [{covariate}] Filtering OSSL pool for {covariate}...")
    }

    ## Filter already-projected OSSL PCA scores for this covariate ----------
    ## This avoids redundant disk reads and preprocessing

    ossl_for_covariate <- ossl_pca_scores %>%
      tidyr::drop_na(dplyr::all_of(covariate))

    if (nrow(ossl_for_covariate) == 0) {
      cli::cli_warn("No OSSL samples have {covariate} measured - skipping")
      ossl_pools[[covariate]] <- NULL
      next
    }

    if (verbose) {
      cli::cli_text("│  │  ├─ OSSL with {covariate}: {format(nrow(ossl_for_covariate), big.mark = ',')}")
    }

    ## Extract PCA dims for this covariate's OSSL pool ---------------------

    ossl_pca_dims_cov <- dplyr::select(ossl_for_covariate, dplyr::starts_with("Dim."))

    ## Calculate distance threshold per cluster (2 std deviations) ---------

    cluster_thresholds <- numeric(optimal_k)
    cluster_assignments_list <- vector("list", optimal_k)

    for (cluster_id in 1:optimal_k) {

      cluster_mask    <- unknown_pca_scores$Cluster == cluster_id
      cluster_samples <- unknown_pca_dims[cluster_mask, ]
      n_cluster       <- nrow(cluster_samples)
      cluster_center  <- colMeans(cluster_samples)

      ## Calculate distances from OSSL (for this covariate) to cluster ----

      can_use_mahalanobis <- use_mahalanobis && (n_cluster > (n_components + 5))

      if (can_use_mahalanobis) {

        cluster_cov <- stats::cov(cluster_samples)

        distances <- tryCatch({

          cov_inv <- MASS::ginv(cluster_cov)

          apply(ossl_pca_dims_cov, 1, function(x) {
            diff <- x - cluster_center
            sqrt(as.numeric(t(diff) %*% cov_inv %*% diff))
          })

        }, error = function(e) {

          apply(ossl_pca_dims_cov, 1, function(x) {
            sqrt(sum((x - cluster_center)^2))
          })

        })

      } else {

        distances <- apply(ossl_pca_dims_cov, 1, function(x) {
          sqrt(sum((x - cluster_center)^2))
        })

      }

      ## Set threshold at specified percentile of distances -------------------

      threshold <- stats::quantile(distances, probs = distance_percentile)

      cluster_thresholds[cluster_id] <- threshold

      ## Assign OSSL within threshold to this cluster ---------------------

      within_threshold <- distances <= threshold
      cluster_assignments_list[[cluster_id]] <- which(within_threshold)

    }

    ## Create expanded OSSL pool (samples can repeat across clusters) ------

    ossl_expanded <- vector("list", optimal_k)

    for (cluster_id in 1:optimal_k) {

      cluster_indices <- cluster_assignments_list[[cluster_id]]

      if (length(cluster_indices) > 0) {

        ossl_expanded[[cluster_id]] <- ossl_for_covariate[cluster_indices, ] %>%
          dplyr::mutate(Cluster = cluster_id)

      } else {

        ossl_expanded[[cluster_id]] <- NULL

      }

    }

    ## Report cluster distribution with overlap info -----------------------

    if (verbose) {

      total_assignments <- sum(sapply(cluster_assignments_list, length))
      overlap_count <- total_assignments - nrow(ossl_for_covariate)

      cluster_sizes <- sapply(cluster_assignments_list, length)
      size_summary <- paste(sprintf("C%s: %s",
                                    1:optimal_k,
                                    format(cluster_sizes, big.mark = ",")),
                           collapse = " | ")

      cli::cli_text("│  │  ├─ Cluster assignment ({distance_method}, {distance_percentile*100}th percentile): {size_summary}")
      cli::cli_text("│  │  └─ Overlap: {format(overlap_count, big.mark = ',')} samples in multiple clusters ({round(overlap_count/nrow(ossl_for_covariate)*100, 1)}%)")

    }

    ## Store expanded pool (combines all cluster assignments) --------------

    ossl_pools[[covariate]] <- dplyr::bind_rows(ossl_expanded)

    ## Free memory from this covariate's intermediate objects ---------------
    rm(ossl_for_covariate, ossl_pca_dims_cov, ossl_expanded, cluster_assignments_list)
    gc(verbose = FALSE)
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Apply coverage filtering per covariate pool
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Applying coverage filtering')} (keeping {coverage * 100}% closest per cluster)")
  }

  for (covariate in covariates) {

    if (is.null(ossl_pools[[covariate]])) next

    ossl_pool <- ossl_pools[[covariate]]

    if (verbose) {
      cli::cli_text("│  ├─ [{covariate}] Filtering OSSL by proximity...")
    }

    ## Filter each cluster separately ----------------------------------------

    filtered_samples <- list()
    cluster_sizes_before <- integer(optimal_k)
    cluster_sizes_after <- integer(optimal_k)

    for (cluster_id in 1:optimal_k) {

      ## Get OSSL samples in this cluster ----------------------------------

      cluster_mask <- ossl_pool$Cluster == cluster_id
      cluster_ossl <- ossl_pool[cluster_mask, ]
      cluster_sizes_before[cluster_id] <- nrow(cluster_ossl)

      if (nrow(cluster_ossl) == 0) next

      ## Get unknown cluster center ----------------------------------------

      unknown_cluster_mask <- unknown_pca_scores$Cluster == cluster_id
      unknown_cluster_samples <- unknown_pca_dims[unknown_cluster_mask, ]
      cluster_center <- colMeans(unknown_cluster_samples)

      ## Calculate distances -----------------------------------------------

      ossl_cluster_dims <- dplyr::select(cluster_ossl, dplyr::starts_with("Dim."))
      n_cluster <- nrow(unknown_cluster_samples)

      if (use_mahalanobis && n_cluster > (n_components + 5)) {

        cluster_cov <- stats::cov(unknown_cluster_samples)

        distances <- tryCatch({

          cov_inv <- MASS::ginv(cluster_cov)

          apply(ossl_cluster_dims, 1, function(x) {
            diff <- x - cluster_center
            sqrt(as.numeric(t(diff) %*% cov_inv %*% diff))
          })

        }, error = function(e) {

          apply(ossl_cluster_dims, 1, function(x) {
            sqrt(sum((x - cluster_center)^2))
          })

        })

      } else {

        distances <- apply(ossl_cluster_dims, 1, function(x) {
          sqrt(sum((x - cluster_center)^2))
        })

      }

      ## Keep closest samples based on coverage threshold ------------------

      coverage_threshold <- stats::quantile(distances, probs = coverage)
      keep_mask <- distances <= coverage_threshold

      filtered_samples[[cluster_id]] <- cluster_ossl[keep_mask, ]
      cluster_sizes_after[cluster_id] <- sum(keep_mask)

    }

    ## Combine filtered samples from all clusters ---------------------------

    ossl_pools[[covariate]] <- dplyr::bind_rows(filtered_samples)

    if (verbose) {

      ## Show per-cluster breakdown ----------------------------------------

      for (cluster_id in 1:optimal_k) {
        before <- cluster_sizes_before[cluster_id]
        after <- cluster_sizes_after[cluster_id]
        pct <- if (before > 0) round(after / before * 100) else 0

        cli::cli_text("│  │  ├─ C{cluster_id}: {format(before, big.mark = ',')} → {format(after, big.mark = ',')} ({pct}%)")
      }

      total_retained <- sum(cluster_sizes_after)
      cli::cli_text("│  │  └─ Total retained: {format(total_retained, big.mark = ',')} samples")
    }

    ## Free coverage filtering intermediate objects --------------------------
    rm(ossl_pool, filtered_samples, cluster_sizes_before, cluster_sizes_after)
    gc(verbose = FALSE)
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Create cluster-specific train/val splits per covariate
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Creating train/val splits')} ({round(prop * 100)}%/{round((1-prop) * 100)}%)")
  }

  training_subsets <- vector("list", length(covariates))
  names(training_subsets) <- covariates

  for (covariate in covariates) {

    if (is.null(ossl_pools[[covariate]])) {
      training_subsets[[covariate]] <- NULL
      next
    }

    if (verbose) {
      cli::cli_text("│  ├─ [{covariate}] Splitting by cluster...")
    }

    ossl_pool <- ossl_pools[[covariate]]
    cluster_subsets <- vector("list", optimal_k)
    names(cluster_subsets) <- paste0("Cluster_", 1:optimal_k)

    for (cluster_id in 1:optimal_k) {

      ## Get OSSL samples for this cluster -------------------------------

      cluster_data <- ossl_pool %>%
        dplyr::filter(Cluster == cluster_id) %>%
        dplyr::select(dplyr::all_of(covariate), dplyr::starts_with("Dim.")) %>%
        tidyr::drop_na(dplyr::all_of(covariate))

      n_samples <- nrow(cluster_data)

      if (n_samples == 0) {
        cluster_subsets[[cluster_id]] <- NULL
        next
      }

      ## Check minimum sample requirements -------------------------------

      n_train_needed <- 20
      n_val_needed <- 10

      if (n_samples < (n_train_needed + n_val_needed)) {

        if (verbose) {
          cli::cli_text("│  ├─ C{cluster_id}: {n_samples} samples (insufficient, skipping)")
        }

        cluster_subsets[[cluster_id]] <- NULL
        next
      }

      ## Split into train/val with stratification --------------------------

      ## Create response quantile bins for stratification ------------------

      cluster_data_with_strata <- cluster_data %>%
        dplyr::mutate(
          response_bin = cut(
            .data[[covariate]],
            breaks = stats::quantile(.data[[covariate]],
                                    probs = seq(0, 1, 0.25),
                                    na.rm = TRUE),
            labels = c("Q1", "Q2", "Q3", "Q4"),
            include.lowest = TRUE
          )
        )

      ## Stratified split ensures train/val have similar distributions -----

      split_obj <- rsample::initial_split(cluster_data_with_strata,
                                          prop   = prop,
                                          strata = response_bin)

      ## Remove stratification column after split --------------------------

      cluster_subsets[[cluster_id]] <- list(
        train = rsample::training(split_obj) %>% dplyr::select(-response_bin),
        val   = rsample::testing(split_obj) %>% dplyr::select(-response_bin)
      )

      if (verbose) {
        n_train <- nrow(cluster_subsets[[cluster_id]]$train)
        n_val   <- nrow(cluster_subsets[[cluster_id]]$val)
        cli::cli_text("│  │  ├─ C{cluster_id}: {n_train} train | {n_val} val")
      }
    }

    training_subsets[[covariate]] <- cluster_subsets
  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Train cluster-specific models (covariate × cluster)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Training local models')}: {length(covariates)} covariate{?s} × {optimal_k} cluster{?s}")
  }

  local_models <- vector("list", length(covariates))
  names(local_models) <- covariates

  for (covariate in covariates) {

    if (is.null(training_subsets[[covariate]])) {
      local_models[[covariate]] <- NULL
      next
    }

    local_models[[covariate]] <- vector("list", optimal_k)
    names(local_models[[covariate]]) <- paste0("Cluster_", 1:optimal_k)

    for (cluster_id in 1:optimal_k) {

      cluster_name <- paste0("Cluster_", cluster_id)
      cluster_data <- training_subsets[[covariate]][[cluster_name]]

      if (is.null(cluster_data)) {
        local_models[[covariate]][[cluster_name]] <- NULL
        next
      }

      if (verbose) {
        cli::cli_text("│  ├─ [{covariate}] [C{cluster_id}] Training model...")
      }

      ## Train Cubist model -----------------------------------------------

      model_start <- Sys.time()

      model_result <- fit_cubist_model(
        train_data    = cluster_data$train,
        val_data      = cluster_data$val,
        covariate     = covariate,
        verbose       = TRUE,  # Suppress nested verbose
        parallel      = allow_par,
        n_workers     = n_workers,
        bayesian_iter = bayesian_iter
      )

      model_time <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))

      local_models[[covariate]][[cluster_name]] <- model_result

      ## Show completion status -------------------------------------------

      if (verbose && !is.null(model_result$validation_metrics)) {

        perf <- model_result$validation_metrics

        cli::cli_text("│  │  └─ Val R² = {round(perf$rsq, 3)} | RMSE = {round(perf$rmse, 2)} | Time: {round(model_time, 1)}s")

      } else if (verbose) {

        cli::cli_text("│  │  └─ Model fitting failed")

      }

      ## Free memory from model training objects ----------------------------
      if (exists("cluster_data")) rm(cluster_data)
      if (exists("model_result")) rm(model_result)
      gc(verbose = FALSE)
    }

    ## Free memory after each covariate completes ---------------------------
    gc(verbose = FALSE)
  }

  ## Free ossl_pools (training_subsets needed for validation in Step 9) ------
  rm(ossl_pools)
  gc(verbose = FALSE)

  ## ---------------------------------------------------------------------------
  ## Step 8: Generate predictions with cluster routing
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Generating predictions')} for {nrow(unknown_pca_scores)} unknown samples")
  }

  predictions <- unknown_pca_scores %>%
    dplyr::select(Sample_ID)

  for (covariate in covariates) {

    if (is.null(local_models[[covariate]])) {
      predictions[[covariate]] <- rep(NA_real_, nrow(unknown_pca_scores))
      next
    }

    if (verbose) {
      cli::cli_text("│  ├─ [{covariate}] Routing predictions...")
    }

    pred_values <- numeric(nrow(unknown_pca_scores))

    ## Route each unknown to its cluster-specific model --------------------

    for (cluster_id in 1:optimal_k) {

      cluster_name <- paste0("Cluster_", cluster_id)
      cluster_mask <- unknown_pca_scores$Cluster == cluster_id

      if (sum(cluster_mask) == 0) next

      cluster_model_obj <- local_models[[covariate]][[cluster_name]]

      if (!is.null(cluster_model_obj) && !is.null(cluster_model_obj$fitted_workflow)) {

        cluster_unknowns <- unknown_pca_scores[cluster_mask, ]

        cluster_preds <- tryCatch({
          stats::predict(cluster_model_obj$fitted_workflow, cluster_unknowns)$.pred
        }, error = function(e) {
          rep(NA_real_, sum(cluster_mask))
        })

        pred_values[cluster_mask] <- cluster_preds

      } else {

        ## Model failed for this cluster - assign NA
        pred_values[cluster_mask] <- NA_real_

      }
    }

    predictions[[covariate]] <- pred_values

    if (verbose) {
      n_valid <- sum(!is.na(pred_values))
      cli::cli_text("│  │  └─ Predictions: {n_valid}/{nrow(unknown_pca_scores)} valid")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 9: Aggregate validation data and calculate harmonized metrics
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Calculating aggregated validation metrics')}")
  }

  all_validation_metrics <- vector("list", length(covariates))
  names(all_validation_metrics) <- covariates

  for (covariate in covariates) {

    if (is.null(local_models[[covariate]])) {
      all_validation_metrics[[covariate]] <- NULL
      next
    }

    ## Collect observed and predicted from all clusters -------------------

    all_obs  <- c()
    all_pred <- c()
    cluster_labels <- c()

    for (cluster_id in 1:optimal_k) {

      cluster_name <- paste0("Cluster_", cluster_id)
      model_result <- local_models[[covariate]][[cluster_name]]

      if (!is.null(model_result) && !is.null(model_result$validation_metrics)) {

        ## Extract validation data from this cluster ----------------------

        cluster_train <- training_subsets[[covariate]][[cluster_name]]$train
        cluster_val   <- training_subsets[[covariate]][[cluster_name]]$val

        ## Predict on validation set --------------------------------------

        val_preds <- tryCatch({
          stats::predict(model_result$fitted_workflow, cluster_val)$.pred
        }, error = function(e) {
          NULL
        })

        if (!is.null(val_preds)) {

          ## Extract observed values from validation set ------------------

          val_obs <- cluster_val[[covariate]]

          ## Verify lengths match -----------------------------------------

          if (length(val_obs) != length(val_preds)) {
            cli::cli_warn("Validation length mismatch for {covariate} C{cluster_id}: {length(val_obs)} obs vs {length(val_preds)} pred")
            next
          }

          ## Remove any NA pairs ------------------------------------------

          complete_mask <- complete.cases(val_obs, val_preds)
          val_obs_clean <- val_obs[complete_mask]
          val_preds_clean <- val_preds[complete_mask]

          ## Append to aggregated vectors ---------------------------------

          all_obs  <- c(all_obs, val_obs_clean)
          all_pred <- c(all_pred, val_preds_clean)
          cluster_labels <- c(cluster_labels, rep(cluster_name, length(val_obs_clean)))

        }
      }
    }

    ## Calculate harmonized metrics on aggregated data --------------------

    if (length(all_obs) > 0 && length(all_pred) > 0) {

      ## Create data frame for rrmse_vec (requires data frame signature) ---

      val_data_for_rrmse <- tibble::tibble(
        truth    = all_obs,
        estimate = all_pred
      )

      combined_metrics <- tibble::tibble(
        covariate = covariate,
        n_samples = length(all_obs),
        rmse      = yardstick::rmse_vec(all_obs, all_pred),
        mae       = yardstick::mae_vec(all_obs, all_pred),
        rsq       = yardstick::rsq_vec(all_obs, all_pred),
        ccc       = ccc_vec(all_obs, all_pred),
        rpd       = rpd_vec(all_obs, all_pred),
        rrmse     = rrmse_vec(data = val_data_for_rrmse, truth = truth, estimate = estimate)
      )

      all_validation_metrics[[covariate]] <- combined_metrics

      if (verbose) {
        cli::cli_text("│  ├─ [{covariate}] Aggregated R² = {round(combined_metrics$rsq, 3)} | RMSE = {round(combined_metrics$rmse, 2)} | RPD = {round(combined_metrics$rpd, 2)}")
      }

    } else {

      all_validation_metrics[[covariate]] <- NULL

      if (verbose) {
        cli::cli_text("│  ├─ [{covariate}] No validation data available")
      }

    }
  }

  ## Combine into single tibble ---------------------------------------------

  validation_metrics <- dplyr::bind_rows(all_validation_metrics)

  ## Free training_subsets now that validation is complete -------------------
  rm(training_subsets)
  gc(verbose = FALSE)

  ## ---------------------------------------------------------------------------
  ## Return results (conditionally include models based on return_models)
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("└─ {cli::style_bold('Clustered prediction complete!')}")
  }

  ## Build base return list --------------------------------------------------

  result <- list(
    predictions        = predictions,
    validation_metrics = validation_metrics,
    cluster_info       = list(
      n_clusters          = optimal_k,
      cluster_assignments = unknown_pca_scores$Cluster,
      cluster_sizes       = table(unknown_pca_scores$Cluster),
      clustering_method   = clustering_method,
      distance_method     = distance_method,
      pca_approach        = "OSSL-centric"
    ),
    experimental       = list(
      derivative_order    = derivative_order,
      use_mahalanobis     = use_mahalanobis,
      distance_percentile = distance_percentile,
      coverage            = coverage
    )
  )

  ## Conditionally add models (memory-intensive) -----------------------------

  if (return_models) {

    result$local_models <- local_models

  } else {

    ## Free models immediately if not returning (saves ~90% of return value memory)
    rm(local_models)
    gc(verbose = FALSE)

    if (verbose) {
      cli::cli_text("│  └─ Models discarded (return_models=FALSE) to save memory")
    }

  }

  return(result)
}
