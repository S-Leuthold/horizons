#' Global Training Set Selection Using Stratified Kennard-Stone
#'
#' @description
#' Functions for selecting globally representative OSSL training subsets using
#' a stratified Kennard-Stone approach. Clusters unknown samples to understand
#' spectral diversity, then proportionally selects relevant OSSL samples to
#' create one optimal training set for the entire project.
#'
#' @importFrom dplyr slice
#' @importFrom stats mahalanobis cov kmeans dist
#' @importFrom cluster silhouette
#' @importFrom rsample initial_split training testing
#' @importFrom cli cli_text cli_abort cli_status cli_status_update cli_status_clear
#' @importFrom prospectr kenStone
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
#' @param n_select Integer. Total OSSL samples to select (default: 20000)
#' @param prop Numeric. Proportion for training set (default: 0.85)
#' @param relevance_threshold Numeric. Proportion of OSSL to consider (default: 0.6)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Named list containing:
#'   - train_data: Training subset of OSSL data
#'   - val_data: Validation subset of OSSL data
#'   - cluster_info: Information about unknown clustering
#'   - global_indices: Indices of selected samples in original OSSL
#'
#' @keywords internal

select_global_training_set <- function(unknown_pca_scores,
                                       ossl_pca_scores,
                                       n_select            = 20000,
                                       prop                = 0.85,
                                       relevance_threshold = 0.6,
                                       verbose             = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Setup
  ## ---------------------------------------------------------------------------

  ## Validate parameters -------------------------------------------------------

  if (prop <= 0 || prop >= 1) cli::cli_abort("prop must be between 0 and 1")

  ## ---------------------------------------------------------------------------
  ## Step 1: Pre-filter OSSL to relevant samples
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_text("")
    cli::cli_text("├─ Method: Stratified Kennard-Stone sampling")
    cli::cli_text("├─ Pre-filtering: {relevance_threshold * 100}% most relevant samples")

  }

  ## Calculate unknown samples centroid and covariance -------------------------

  pca_cols       <- grep("^Dim\\.", names(unknown_pca_scores), value = TRUE)
  unknown_matrix <- as.matrix(unknown_pca_scores[pca_cols])
  ossl_matrix    <- as.matrix(ossl_pca_scores[pca_cols])
  unknown_center <- colMeans(unknown_matrix)
  unknown_cov    <- stats::cov(unknown_matrix)

  ## Calculate Mahalanobis distances from each OSSL sample to unknown centroid -

  stats::mahalanobis(x      = ossl_matrix,
                     center = unknown_center,
                     cov    = unknown_cov) -> distances_to_centroid

  ## Keep only most relevant OSSL samples based on the threshold ---------------

  n_relevant       <- floor(nrow(ossl_pca_scores) * relevance_threshold)
  relevant_indices <- order(distances_to_centroid)[1:n_relevant]
  relevant_ossl    <- dplyr::slice(ossl_pca_scores, relevant_indices)
  relevant_matrix  <- ossl_matrix[relevant_indices, ]

  if (verbose) cli::cli_text("├─ Filtered OSSL library down to {nrow(relevant_ossl)} relevant samples.")


  ## ---------------------------------------------------------------------------
  ## Step 2: Cluster unknown samples
  ## ---------------------------------------------------------------------------

  cluster_unknown_samples(unknown_pca_scores = unknown_pca_scores,
                          verbose            = verbose) -> cluster_result

  if (verbose) cli::cli_text("├─ Clustered unknown samples into {cluster_result$n_clusters} clusters")

  ## ---------------------------------------------------------------------------
  ## Step 3: Stratified Kennard-Stone selection
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("├─ Running stratified Kennard-Stone selection")

  stratified_kennard_stone(unknown_clusters = cluster_result,
                           unknown_matrix   = unknown_matrix,
                           relevant_ossl    = relevant_ossl,
                           relevant_matrix  = relevant_matrix,
                           n_select         = n_select,
                           verbose          = verbose) -> selected_indices


  # Map back to original OSSL indices ------------------------------------------

  global_indices <- relevant_indices[selected_indices]
  selected_ossl  <- dplyr::slice(ossl_pca_scores, global_indices)

  ## ---------------------------------------------------------------------------
  ## Step 4: Split into training and validation
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("├─ Splitting selected samples into train/validation.")

  split_obj <- rsample::initial_split(selected_ossl, prop = prop)

  train_data <- rsample::training(split_obj)
  val_data   <- rsample::testing(split_obj)

  if (verbose) cli::cli_text("│  └─ Split into {nrow(train_data)} training and {nrow(val_data)} validation samples")

  ## ---------------------------------------------------------------------------
  ## Step 5: Return results
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("└─ {cli::col_green('✓')} Global training set selection complete: {nrow(train_data)} training, {nrow(val_data)} validation samples")
  }

  return(list(
    train_data = train_data,
    val_data = val_data,
    cluster_info = cluster_result,
    global_indices = global_indices
  ))
}

#' Cluster Unknown Samples for Stratification
#'
#' @description
#' Clusters unknown samples in PCA space to identify spectral diversity
#' for proportional OSSL selection. Uses silhouette analysis to determine
#' optimal cluster count for soil applications.
#'
#' @param unknown_pca_scores Tibble with unknown PCA scores
#' @param max_clusters Integer. Maximum clusters to consider (default: 10)
#' @param seed Integer. Random seed for reproducibility (default: 0307)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Named list containing:
#'   - cluster_assignments: Integer vector of cluster assignments
#'   - n_clusters: Number of clusters selected
#'   - cluster_proportions: Numeric vector of cluster proportions
#'   - silhouette_scores: Numeric vector of silhouette scores for each k tested
#'
#' @keywords internal

cluster_unknown_samples <- function(unknown_pca_scores,
                                    max_clusters = 10,
                                    seed         = 0307,
                                    verbose      = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Extract data from the PCA and format
  ## ---------------------------------------------------------------------------

  pca_cols       <- grep("^Dim\\.", names(unknown_pca_scores), value = TRUE)
  unknown_matrix <- as.matrix(unknown_pca_scores[pca_cols])
  n_samples      <- nrow(unknown_matrix)


  ## ---------------------------------------------------------------------------
  ## Step 2: Early exit if there's too little data to cluster
  ## ---------------------------------------------------------------------------

  max_k <- min(max_clusters, floor(n_samples / 3))

  if (max_k < 2) {

    if (verbose) cli::cli_text("│  ├─ {cli::col_blue('ℹ')} Too few unknowns for clustering ({n_samples} samples) - using single group")

    return(list(cluster_assignments = rep(1, n_samples),
                n_clusters          = 1,
                cluster_proportions = 1,
                silhouette_scores   = NA))

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Identify optimal k via silhouette analysis
  ## ---------------------------------------------------------------------------

  k_range           <- 2:max_k
  silhouette_scores <- numeric(length(k_range))

  ## Pre-calculate the distance matrix -----------------------------------------

  dist_matrix <- stats::dist(unknown_matrix)

  ## Set up reporting ----------------------------------------------------------

  if (verbose){

    cli::cli_text("│  ├─ Clustering {n_samples} unknowns into 2-{max_k} groups")
    status_id <- cli::cli_status("│      └─ Testing k = 2/{max_k}")

  }

  ## Loop across possible k values ---------------------------------------------

  for (i in seq_along(k_range)) {

    set.seed(seed)
    k <- k_range[i]

    if (verbose) cli::cli_status_update(id = status_id, "│      └─ Testing k = {k}/{max_k}")

    ## Run the k-mean algorithm ------------------------------------------------

    tryCatch({

      stats::kmeans(unknown_matrix,
                    centers  = k,
                    nstart   = 25,
                    iter.max = 100)

      }, error = function(e) NULL) -> kmeans_result

    ## Skip if k-means fails :( -------------------------------------------------

    if (is.null(kmeans_result)) {

      silhouette_scores[i] <- -1
      next

    }

    ## Run the silhouette analysis ----------------------------------------------

    tryCatch({

      cluster::silhouette(x    = kmeans_result$cluster,
                          dist = dist_matrix) -> silhouette_result

      mean(silhouette_result[, 3])

    }, error = function(e) -1) -> silhouette_scores[i]

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Validate the scores
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_status_clear(id = status_id)

  valid_indices <- which(silhouette_scores > -1)

  if (length(valid_indices) == 0) cli::cli_abort("Unable to cluster samples - all clustering attempts failed")

  ## Select optimal number of clusters from valid attempts ---------------------

  optimal_k <- k_range[valid_indices][which.max(silhouette_scores[valid_indices])]

  if (verbose) cli::cli_text("│  ├─ Selected {optimal_k} clusters (silhouette = {round(max(silhouette_scores[valid_indices]), 3)})")


  ## ---------------------------------------------------------------------------
  ## Step 5: Perform the final clustering
  ## ---------------------------------------------------------------------------

  set.seed(seed)

  ## Run kmeans one last time --------------------------------------------------

  stats::kmeans(unknown_matrix,
                centers  = optimal_k,
                nstart   = 25,
                iter.max = 100) -> final_kmeans

  ## Calculate cluster proportions ---------------------------------------------

  cluster_counts      <- table(final_kmeans$cluster)
  cluster_proportions <- as.numeric(cluster_counts) / n_samples

  ## Return clustering results -------------------------------------------------

  return(list(cluster_assignments = final_kmeans$cluster,
              n_clusters          = optimal_k,
              cluster_proportions = cluster_proportions,
              silhouette_scores   = silhouette_scores))

}

#' Stratified Kennard-Stone Sample Selection
#'
#' @description
#' Performs Kennard-Stone selection within each unknown cluster to ensure
#' representative OSSL sampling across different soil spectral types.
#' First assigns OSSL samples to nearest unknown cluster, then performs
#' Kennard-Stone selection within each cluster's OSSL subset.
#'
#' @param unknown_clusters Named list result from cluster_unknown_samples()
#' @param unknown_matrix Matrix of unknown PCA scores
#' @param relevant_ossl Tibble of pre-filtered OSSL samples
#' @param relevant_matrix Matrix of relevant OSSL PCA scores
#' @param n_select Integer. Total samples to select
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Integer vector of indices into relevant_matrix/relevant_ossl
#'
#' @details
#' The function:
#' 1. Calculates proportional allocation based on unknown cluster sizes
#' 2. Assigns each OSSL sample to its nearest unknown cluster
#' 3. Runs Kennard-Stone within each cluster's OSSL subset
#' 4. Returns combined indices from all clusters
#'
#' @keywords internal

stratified_kennard_stone <- function(unknown_clusters,
                                    unknown_matrix,
                                    relevant_ossl,
                                    relevant_matrix,
                                    n_select,
                                    verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Calculate proportional allocation per cluster
  ## ---------------------------------------------------------------------------

  samples_per_cluster <- round(n_select * unknown_clusters$cluster_proportions)

  ## Adjust to get exactly n_select samples -----------------------------------

  while (sum(samples_per_cluster) != n_select) {

    diff <- n_select - sum(samples_per_cluster)

    if (diff > 0) {
      ## Add to largest cluster
      largest <- which.max(samples_per_cluster)
      samples_per_cluster[largest] <- samples_per_cluster[largest] + 1
    } else {
      ## Remove from largest cluster
      largest <- which.max(samples_per_cluster)
      samples_per_cluster[largest] <- samples_per_cluster[largest] - 1
    }

  }

  # Check if requested exceeds available
  n_available <- nrow(relevant_ossl)

  if (verbose) {
    if (n_select > n_available) {
      cli::cli_text("│  ├─ Requested: {n_select} samples (exceeds {n_available} available)")
      cli::cli_text("│  └─ Target allocation: {paste(samples_per_cluster, collapse = '/')} samples per cluster")
    } else {
      cli::cli_text("│  └─ Allocation: {paste(samples_per_cluster, collapse = '/')} samples per cluster")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Assign OSSL samples to nearest unknown cluster
  ## ---------------------------------------------------------------------------

  ## Calculate cluster centers -------------------------------------------------

  cluster_centers <- matrix(nrow = unknown_clusters$n_clusters,
                           ncol = ncol(unknown_matrix))

  for (k in 1:unknown_clusters$n_clusters) {

    cluster_mask     <- unknown_clusters$cluster_assignments == k
    cluster_unknowns <- unknown_matrix[cluster_mask, , drop = FALSE]
    cluster_centers[k, ] <- colMeans(cluster_unknowns)

  }

  ## Assign each OSSL sample to nearest cluster --------------------------------

  ossl_cluster_assignments <- numeric(nrow(relevant_matrix))

  for (i in 1:nrow(relevant_matrix)) {

    ## Calculate distance to each cluster center
    distances <- apply(cluster_centers, 1, function(center) {
      sqrt(sum((relevant_matrix[i, ] - center)^2))
    })

    ossl_cluster_assignments[i] <- which.min(distances)

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Perform Kennard-Stone within each cluster
  ## ---------------------------------------------------------------------------

  all_selected <- c()

  for (cluster_id in 1:unknown_clusters$n_clusters) {

    n_to_select <- samples_per_cluster[cluster_id]

    if (n_to_select == 0) next

    ## Get OSSL samples assigned to this cluster ------------------------------

    cluster_ossl_mask    <- ossl_cluster_assignments == cluster_id
    cluster_ossl_indices <- which(cluster_ossl_mask)
    cluster_ossl_matrix  <- relevant_matrix[cluster_ossl_mask, , drop = FALSE]

    ## Handle edge case: not enough OSSL samples in cluster -------------------

    if (length(cluster_ossl_indices) <= n_to_select) {

      ## Take all available samples
      cluster_selected <- cluster_ossl_indices

      if (verbose) {
        if (length(cluster_ossl_indices) < n_to_select) {
          cli::cli_text("│  ├─ Cluster {cluster_id}: Requested {n_to_select}, only {length(cluster_ossl_indices)} available - taking all.")
        } else {
          cli::cli_text("│  ├─ Cluster {cluster_id}: Taking all {length(cluster_ossl_indices)} available samples.")
        }
      }

    } else {

      ## Run Kennard-Stone selection -------------------------------------------

      ks_result <- tryCatch({

        prospectr::kenStone(X      = cluster_ossl_matrix,
                           k      = n_to_select,
                           metric = "euclid",
                           .center = TRUE,
                           .scale  = TRUE)

      }, error = function(e) NULL)

      if (is.null(ks_result)) {
        cli::cli_abort("Kennard-Stone failed for cluster {cluster_id}")
      }

      ## Map back to indices in relevant_matrix --------------------------------

      cluster_selected <- cluster_ossl_indices[ks_result$model]

      if (verbose) {
        cli::cli_text("│  ├─ Cluster {cluster_id}: Selected {n_to_select} samples from {length(cluster_ossl_indices)} available.")
      }

    }

    all_selected <- c(all_selected, cluster_selected)

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Handle any shortfall from clusters with insufficient samples
  ## ---------------------------------------------------------------------------

  n_selected <- length(all_selected)

  if (verbose) {
    if (n_selected < n_select) {
      cli::cli_text("│  └─ Total selected: {n_selected} samples (requested {n_select} but limited by availability).")
    } else {
      cli::cli_text("│  └─ Total selected: {n_selected} samples.")
    }
  }

  ## Return the clusters -------------------------------------------------------

  return(unique(all_selected))

}



