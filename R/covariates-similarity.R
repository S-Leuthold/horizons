#' Global Training Set Selection Using Stratified Kennard-Stone
#'
#' @description
#' Functions for selecting globally representative OSSL training subsets using
#' a stratified Kennard-Stone approach. Clusters unknown samples to understand
#' spectral diversity, then proportionally selects relevant OSSL samples to
#' create one optimal training set for the entire project.
#'
#' @importFrom dplyr slice arrange mutate bind_rows
#' @importFrom tibble tibble
#' @importFrom stats mahalanobis cov kmeans dist
#' @importFrom cluster silhouette
#' @importFrom rsample initial_split training testing
#' @importFrom cli cli_progress_step cli_alert_info cli_alert_success cli_abort
#' @importFrom purrr map_dbl
#' @keywords internal

## Global Training Set Selection Functions ----------------------------------

#' Select Global OSSL Training Set
#'
#' @description
#' Selects a globally representative OSSL training subset using stratified
#' Kennard-Stone sampling. Clusters unknown samples to identify spectral
#' diversity, then proportionally selects OSSL samples relevant to each cluster.
#' Works best with large sample sizes (15,000-25,000 samples).
#'
#' @param unknown_pca_scores Tibble with PCA scores for all unknown samples
#' @param ossl_pca_scores Tibble with PCA scores for all OSSL samples
#' @param n_select Integer. Total OSSL samples to select (default: 6000)
#' @param prop Numeric. Proportion for training set (default: 0.8)
#' @param relevance_threshold Numeric. Proportion of OSSL to consider (default: 0.6)
#' @param verbose Logical. Print progress messages
#'
#' @return Named list containing:
#'   - train_data: Training subset of OSSL data
#'   - val_data: Validation subset of OSSL data
#'   - selection_quality: Metrics on selection quality
#'   - cluster_info: Information about unknown clustering
#' @keywords internal
select_global_training_set <- function(unknown_pca_scores,
                                     ossl_pca_scores,
                                     n_select = 20000,
                                     prop = 0.85,
                                     relevance_threshold = 0.6,
                                     verbose = TRUE) {
  
  if (verbose) {
    cli::cli_progress_step("Selecting global training set using stratified Kennard-Stone")
  }
  
  # Validate parameters
  if (prop <= 0 || prop >= 1) {
    cli::cli_abort("prop must be between 0 and 1")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Pre-filter OSSL to relevant samples
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_text("")
    cli::cli_text(format_tree_item("Similarity Analysis", level = 0))
    
    prefilter_text <- paste0("Pre-filtering: ", format_metric(relevance_threshold * 100, "percentage"), 
                            " most relevant samples")
    cli::cli_text(format_tree_item(prefilter_text, level = 1, is_last = FALSE))
  }
  
  # Calculate unknown centroid and covariance
  pca_cols <- grep("^Dim\\.", names(unknown_pca_scores), value = TRUE)
  unknown_matrix <- as.matrix(unknown_pca_scores[pca_cols])
  ossl_matrix <- as.matrix(ossl_pca_scores[pca_cols])
  
  unknown_center <- colMeans(unknown_matrix)
  unknown_cov <- stats::cov(unknown_matrix)
  
  # Calculate distances from each OSSL sample to unknown centroid
  distances_to_centroid <- stats::mahalanobis(
    x = ossl_matrix,
    center = unknown_center,
    cov = unknown_cov
  )
  
  # Keep only most relevant OSSL samples
  n_relevant <- floor(nrow(ossl_pca_scores) * relevance_threshold)
  relevant_indices <- order(distances_to_centroid)[1:n_relevant]
  relevant_ossl <- dplyr::slice(ossl_pca_scores, relevant_indices)
  relevant_matrix <- ossl_matrix[relevant_indices, ]
  
  if (verbose) {
    filtered_text <- paste0(get_status_symbol("success"), " Filtered to ", 
                           format_metric(nrow(relevant_ossl), "count"), " relevant samples")
    cli::cli_text(format_tree_item(filtered_text, level = 1, is_last = TRUE))
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Cluster unknown samples
  ## ---------------------------------------------------------------------------
  
  cluster_result <- cluster_unknown_samples(
    unknown_pca_scores = unknown_pca_scores,
    verbose = verbose
  )
  
  if (is.null(cluster_result)) {
    cli::cli_abort("Failed to cluster unknown samples")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Stratified Kennard-Stone selection
  ## ---------------------------------------------------------------------------
  
  selected_indices <- stratified_kennard_stone(
    unknown_clusters = cluster_result,
    unknown_matrix = unknown_matrix,
    relevant_ossl = relevant_ossl,
    relevant_matrix = relevant_matrix,
    n_select = n_select,
    verbose = verbose
  )
  
  if (is.null(selected_indices)) {
    cli::cli_abort("Failed to perform Kennard-Stone selection")
  }
  
  # Map back to original OSSL indices
  global_indices <- relevant_indices[selected_indices]
  selected_ossl <- dplyr::slice(ossl_pca_scores, global_indices)
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Split into training and validation
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_text(format_tree_item("⟳ Splitting selected samples into train/validation...", level = 1, is_last = FALSE, symbol = NULL))
  }
  
  safely_execute(
    expr = {
      rsample::initial_split(selected_ossl, prop = prop)
    },
    default_value = NULL,
    error_message = "Failed to split selected samples"
  ) -> split_safe
  
  if (is.null(split_safe$result)) {
    return(NULL)
  }
  
  split_obj <- split_safe$result
  train_data <- rsample::training(split_obj)
  val_data <- rsample::testing(split_obj)
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Quality assessment
  ## ---------------------------------------------------------------------------
  
  selection_quality <- assess_selection_quality(
    selected_ossl = selected_ossl[pca_cols],
    unknown_matrix = unknown_matrix,
    verbose = verbose
  )
  
  if (verbose) {
    cli::cli_text(format_tree_item("✓ Global training set selection complete", level = 1, is_last = FALSE))
    cli::cli_text(format_tree_item(paste0("Training: ", nrow(train_data), " samples, Validation: ", nrow(val_data), " samples"), level = 1, is_last = TRUE))
  }
  
  return(list(
    train_data = train_data,
    val_data = val_data,
    selection_quality = selection_quality,
    cluster_info = cluster_result,
    global_indices = global_indices
  ))
}

#' Cluster Unknown Samples for Stratification
#'
#' @description
#' Clusters unknown samples in PCA space to identify spectral diversity
#' for proportional OSSL selection. Uses silhouette analysis to determine
#' optimal cluster count (2-4 clusters for soil applications).
#'
#' @param unknown_pca_scores Tibble with unknown PCA scores
#' @param max_clusters Integer. Maximum clusters to consider (default: 4)
#' @param verbose Logical. Print progress messages
#'
#' @return Named list with cluster assignments and proportions
#' @keywords internal
cluster_unknown_samples <- function(unknown_pca_scores,
                                   max_clusters = 4,
                                   verbose = TRUE) {
  
  pca_cols <- grep("^Dim\\.", names(unknown_pca_scores), value = TRUE)
  unknown_matrix <- as.matrix(unknown_pca_scores[pca_cols])
  
  n_samples <- nrow(unknown_matrix)
  
  # For small datasets, use fewer clusters
  max_k <- min(max_clusters, floor(n_samples / 3))
  
  if (max_k < 2) {
    # Too few samples for clustering
    if (verbose) {
      cli::cli_text(format_tree_item("ℹ Too few unknowns for clustering - using single group", level = 1, is_last = TRUE))
    }
    
    return(list(
      cluster_assignments = rep(1, n_samples),
      n_clusters = 1,
      cluster_proportions = 1,
      silhouette_scores = NA
    ))
  }
  
  if (verbose) {
    cluster_text <- paste0("Clustering ", format_metric(n_samples, "count"), " unknowns (testing ", max_k, " cluster options)")
    cli::cli_text(format_tree_item(cluster_text, level = 1, is_last = FALSE))
  }
  
  # Test different cluster counts
  k_range <- 2:max_k
  silhouette_scores <- purrr::map_dbl(k_range, function(k) {
    
    set.seed(0307)
    kmeans_result <- stats::kmeans(unknown_matrix, centers = k, nstart = 25)
    
    silhouette_result <- cluster::silhouette(
      x = kmeans_result$cluster,
      dist = stats::dist(unknown_matrix)
    )
    
    mean(silhouette_result[, 3])
  })
  
  # Select optimal k
  optimal_k <- k_range[which.max(silhouette_scores)]
  
  # Final clustering
  set.seed(0307)
  final_kmeans <- stats::kmeans(unknown_matrix, centers = optimal_k, nstart = 25)
  
  # Calculate cluster proportions
  cluster_counts <- table(final_kmeans$cluster)
  cluster_proportions <- cluster_counts / n_samples
  
  if (verbose) {
    clustering_text <- paste0(get_status_symbol("success"), " Optimal clustering: ", optimal_k, " clusters")
    cli::cli_text(format_tree_item(clustering_text, level = 1, is_last = FALSE))
    
    for (i in seq_along(cluster_counts)) {
      cluster_text <- paste0("Cluster ", i, ": ", format_metric(cluster_counts[i], "count"), 
                           " samples (", format_metric(cluster_proportions[i] * 100, "percentage"), ")")
      is_last <- (i == length(cluster_counts))
      cli::cli_text(format_tree_item(cluster_text, level = 2, is_last = is_last))
    }
  }
  
  return(list(
    cluster_assignments = final_kmeans$cluster,
    n_clusters = optimal_k,
    cluster_proportions = as.numeric(cluster_proportions),
    silhouette_scores = silhouette_scores,
    cluster_centers = final_kmeans$centers
  ))
}

#' Stratified Kennard-Stone Sample Selection
#'
#' @description
#' Performs Kennard-Stone selection within each unknown cluster to ensure
#' representative OSSL sampling across different soil spectral types.
#'
#' @param unknown_clusters Result from cluster_unknown_samples()
#' @param unknown_matrix Matrix of unknown PCA scores
#' @param relevant_ossl Tibble of pre-filtered OSSL samples
#' @param relevant_matrix Matrix of relevant OSSL PCA scores
#' @param n_select Integer. Total samples to select
#' @param verbose Logical. Print progress messages
#'
#' @return Vector of indices into relevant_ossl
#' @keywords internal
stratified_kennard_stone <- function(unknown_clusters,
                                    unknown_matrix,
                                    relevant_ossl,
                                    relevant_matrix,
                                    n_select,
                                    verbose = TRUE) {
  
  # Calculate samples per cluster (proportional allocation)
  samples_per_cluster <- round(n_select * unknown_clusters$cluster_proportions)
  
  # Ensure we get exactly n_select samples
  while (sum(samples_per_cluster) != n_select) {
    if (sum(samples_per_cluster) < n_select) {
      # Add to largest cluster
      largest_cluster <- which.max(samples_per_cluster)
      samples_per_cluster[largest_cluster] <- samples_per_cluster[largest_cluster] + 1
    } else {
      # Remove from largest cluster  
      largest_cluster <- which.max(samples_per_cluster)
      samples_per_cluster[largest_cluster] <- samples_per_cluster[largest_cluster] - 1
    }
  }
  
  if (verbose) {
    allocation_text <- paste0("Kennard-Stone allocation: ", paste(samples_per_cluster, collapse = ", "), " samples per cluster")
    cli::cli_text(format_tree_item(allocation_text, level = 1, is_last = TRUE))
  }
  
  all_selected <- c()
  
  # Perform Kennard-Stone within each cluster
  for (cluster_id in seq_along(samples_per_cluster)) {
    
    n_cluster_samples <- samples_per_cluster[cluster_id]
    if (n_cluster_samples == 0) next
    
    # Get unknown samples in this cluster
    cluster_unknowns <- unknown_matrix[unknown_clusters$cluster_assignments == cluster_id, , drop = FALSE]
    cluster_center <- colMeans(cluster_unknowns)
    
    if (verbose) {
      cli::cli_text(format_tree_item(paste0("⟳ Selecting ", n_cluster_samples, " samples for cluster ", cluster_id, "..."), level = 2, is_last = FALSE, symbol = NULL))
    }
    
    # Kennard-Stone selection for this cluster using prospectr
    # OPTIMIZATION: Use euclidean distance for large datasets to avoid expensive Mahalanobis calculations
    use_euclidean <- nrow(relevant_matrix) > 5000
    
    ks_result <- safely_execute(
      expr = {
        if (use_euclidean) {
          # Use faster Euclidean distance for large datasets
          prospectr::kenStone(
            X = relevant_matrix,
            k = n_cluster_samples, 
            metric = "euclid"  # Much faster than Mahalanobis
          )
        } else {
          # Use Mahalanobis for smaller datasets where quality matters more
          prospectr::kenStone(
            X = relevant_matrix,
            k = n_cluster_samples, 
            metric = "mahal",
            pc = min(ncol(relevant_matrix), 10)  # Limit PCA dimensions for speed
          )
        }
      },
      default_value = NULL,
      error_message = "Kennard-Stone selection failed for cluster {cluster_id}"
    )
    
    if (is.null(ks_result$result)) {
      cli::cli_abort("Failed to select samples for cluster {cluster_id}")
    }
    
    cluster_selected <- ks_result$result$model
    
    all_selected <- c(all_selected, cluster_selected)
  }
  
  return(all_selected)
}

#' Kennard-Stone Selection for Single Cluster
#'
#' @description
#' Core Kennard-Stone algorithm implementation for selecting OSSL samples
#' most relevant to a specific unknown cluster.
#'
#' @param cluster_center Numeric vector of cluster centroid in PCA space
#' @param cluster_unknowns Matrix of unknown samples in this cluster
#' @param relevant_matrix Matrix of relevant OSSL PCA scores
#' @param n_select Integer. Number of samples to select
#'
#' @return Vector of indices into relevant_matrix
#' @keywords internal
kennard_stone_cluster <- function(cluster_center,
                                 cluster_unknowns,
                                 relevant_matrix,
                                 n_select) {
  
  n_ossl <- nrow(relevant_matrix)
  selected <- integer(n_select)
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Initialize with sample closest to cluster center
  ## ---------------------------------------------------------------------------
  
  # Regularize covariance matrix to prevent singular matrix errors
  cluster_cov <- stats::cov(cluster_unknowns)
  if (rcond(cluster_cov) < 1e-10) {
    cluster_cov <- cluster_cov + diag(1e-6, ncol(cluster_cov))
  }
  
  distances_to_center <- safely_execute(
    expr = {
      stats::mahalanobis(
        x = relevant_matrix,
        center = cluster_center,
        cov = cluster_cov
      )
    },
    default_value = NULL,
    error_message = "Failed to calculate Mahalanobis distances to cluster center"
  )$result
  
  if (is.null(distances_to_center)) {
    # Fallback to Euclidean distance if Mahalanobis fails
    distances_to_center <- apply(relevant_matrix, 1, function(x) {
      sqrt(sum((x - cluster_center)^2))
    })
  }
  
  selected[1] <- which.min(distances_to_center)
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Iteratively add samples maximizing coverage + diversity
  ## ---------------------------------------------------------------------------
  
  for (i in 2:n_select) {
    
    # Show progress every 10% or every 100 samples (whichever is less frequent)
    progress_interval <- max(100, ceiling(n_select * 0.1))
    if (i %% progress_interval == 0 && verbose) {
      progress <- round(i / n_select * 100, 1)
      cli::cli_text(format_tree_item(paste0("Selection progress: ", progress, "% (", i, "/", n_select, ")"), level = 2, is_last = TRUE))
    }
    
    remaining_indices <- setdiff(1:n_ossl, selected[1:(i-1)])
    scores <- numeric(length(remaining_indices))
    
    for (j in seq_along(remaining_indices)) {
      
      candidate_idx <- remaining_indices[j]
      
      # Similarity to unknown cluster (smaller = better)
      dist_to_unknowns <- safely_execute(
        expr = {
          stats::mahalanobis(
            x = matrix(relevant_matrix[candidate_idx, ], nrow = 1),
            center = cluster_center,
            cov = cluster_cov
          )
        },
        default_value = sqrt(sum((relevant_matrix[candidate_idx, ] - cluster_center)^2)),
        error_message = NULL  # Silent fallback to Euclidean
      )$result
      
      # Diversity from already selected (larger = better)
      if (i == 2) {
        dist_to_selected <- sqrt(sum((relevant_matrix[selected[1], ] - relevant_matrix[candidate_idx, ])^2))
      } else {
        distances_to_selected <- apply(relevant_matrix[selected[1:(i-1)], , drop = FALSE], 1, function(sel_sample) {
          sqrt(sum((relevant_matrix[candidate_idx, ] - sel_sample)^2))
        })
        dist_to_selected <- min(distances_to_selected)
      }
      
      # Combined score: prioritize diversity, penalize distance to unknowns
      scores[j] <- dist_to_selected / (1 + dist_to_unknowns)
    }
    
    # Select sample with highest score
    best_idx <- which.max(scores)
    selected[i] <- remaining_indices[best_idx]
  }
  
  return(selected)
}

#' Assess Quality of Selected Training Set
#'
#' @description
#' Computes quality metrics for the selected OSSL training subset,
#' including coverage of unknown space and internal diversity.
#'
#' @param selected_ossl Matrix of selected OSSL PCA scores
#' @param unknown_matrix Matrix of unknown PCA scores
#' @param verbose Logical. Print progress messages
#'
#' @return Named list with quality metrics
#' @keywords internal
assess_selection_quality <- function(selected_ossl,
                                    unknown_matrix,
                                    verbose = TRUE) {
  
  # Coverage: How well do selected samples cover unknown space?
  coverage_distances <- apply(unknown_matrix, 1, function(unknown_sample) {
    distances <- apply(selected_ossl, 1, function(ossl_sample) {
      sqrt(sum((unknown_sample - ossl_sample)^2))
    })
    min(distances)
  })
  
  # Diversity: Internal diversity of selected samples
  diversity_score <- mean(stats::dist(selected_ossl))
  
  # Representativeness: How similar is selected centroid to unknown centroid?
  selected_center <- colMeans(selected_ossl)
  unknown_center <- colMeans(unknown_matrix)
  representativeness <- sqrt(sum((selected_center - unknown_center)^2))
  
  quality_metrics <- list(
    mean_coverage_distance = mean(coverage_distances),
    max_coverage_distance = max(coverage_distances),
    diversity_score = diversity_score,
    representativeness = representativeness,
    coverage_uniformity = sd(coverage_distances) / mean(coverage_distances)
  )
  
  if (verbose) {
    cli::cli_text(format_tree_item("✓ Selection quality assessment complete", level = 1, is_last = FALSE))
    cli::cli_text(format_tree_item(paste0("Mean coverage distance: ", round(quality_metrics$mean_coverage_distance, 3)), level = 2, is_last = FALSE))
    cli::cli_text(format_tree_item(paste0("Diversity score: ", round(quality_metrics$diversity_score, 3)), level = 2, is_last = FALSE))
    cli::cli_text(format_tree_item(paste0("Representativeness: ", round(quality_metrics$representativeness, 3)), level = 2, is_last = TRUE))
  }
  
  return(quality_metrics)
}