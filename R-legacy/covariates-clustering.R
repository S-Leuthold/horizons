#' Clustering Utilities for Covariate Prediction
#'
#' @description
#' Functions for clustering OSSL samples and assigning unknown samples to clusters
#' for local model fitting. Uses K-means with automatic cluster number selection
#' via silhouette analysis.
#'
#' @keywords internal



## ===========================================================================



#' Cluster OSSL Samples in PCA Space
#'
#' Performs K-means clustering on OSSL PCA scores with automatic cluster number
#' selection via silhouette analysis. Tests 2-10 clusters and selects the number
#' that maximizes average silhouette width.
#'
#' @param ossl_pca_scores Tibble with OSSL PCA scores (Dim.1, Dim.2, ..., plus covariate columns)
#' @param max_clusters Integer. Maximum clusters to test (default: 10)
#' @param seed Integer. Random seed for reproducibility (default: 307)
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return Named list:
#'   - kmeans_model: fitted kmeans object with cluster centers
#'   - n_clusters: optimal number of clusters selected
#'   - cluster_assignments: integer vector of cluster assignments (length = nrow(ossl_pca_scores))
#'   - silhouette_scores: numeric vector of silhouette scores for each k tested
#'
#' @importFrom stats kmeans
#' @importFrom cluster silhouette
#' @importFrom stats dist
#' @importFrom cli cli_text
#' @keywords internal

cluster_ossl_samples <- function(ossl_pca_scores,
                                  max_clusters = 10,
                                  seed         = 307,
                                  verbose      = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Extract PCA dimensions
  ## ---------------------------------------------------------------------------

  pca_cols       <- grep("^Dim\\.", names(ossl_pca_scores), value = TRUE)
  ossl_matrix    <- as.matrix(ossl_pca_scores[pca_cols])
  n_samples      <- nrow(ossl_matrix)

  ## ---------------------------------------------------------------------------
  ## Step 2: Determine viable cluster range
  ## ---------------------------------------------------------------------------

  max_k <- min(max_clusters, floor(n_samples / 3))

  if (max_k < 2) {

    if (verbose) cli::cli_text("├─ Too few samples ({n_samples}) for clustering - using all data")

    return(list(
      kmeans_model       = NULL,
      n_clusters         = 1,
      cluster_assignments = rep(1, n_samples),
      silhouette_scores  = NULL
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Test cluster numbers via silhouette analysis
  ## ---------------------------------------------------------------------------

  set.seed(seed)

  silhouette_scores <- numeric(max_k - 1)
  kmeans_results    <- vector("list", max_k - 1)

  for (k in 2:max_k) {

    ## Fit K-means
    km_result <- stats::kmeans(ossl_matrix,
                               centers = k,
                               nstart  = 25,
                               iter.max = 100)

    kmeans_results[[k - 1]] <- km_result

    ## Calculate silhouette width
    sil <- cluster::silhouette(km_result$cluster,
                               dist = stats::dist(ossl_matrix))

    silhouette_scores[k - 1] <- mean(sil[, 3])
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Select optimal cluster number
  ## ---------------------------------------------------------------------------

  optimal_k     <- which.max(silhouette_scores) + 1
  optimal_model <- kmeans_results[[optimal_k - 1]]

  if (verbose) {
    cli::cli_text("├─ Optimal number of clusters: {optimal_k}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return clustering result
  ## ---------------------------------------------------------------------------

  return(list(
    kmeans_model        = optimal_model,
    n_clusters          = optimal_k,
    cluster_assignments = optimal_model$cluster,
    silhouette_scores   = silhouette_scores
  ))
}



## ===========================================================================



#' Assign Unknown Samples to OSSL Clusters
#'
#' Matches unknown samples to nearest OSSL cluster centroids using Euclidean
#' distance in PCA space. Each unknown is assigned to the cluster whose centroid
#' it is closest to.
#'
#' @param unknown_pca_scores Tibble with unknown PCA scores (Dim.1, Dim.2, ...)
#' @param cluster_model List from cluster_ossl_samples() containing kmeans_model
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return Integer vector of cluster assignments (length = nrow(unknown_pca_scores))
#'
#' @importFrom cli cli_text
#' @keywords internal

assign_unknowns_to_clusters <- function(unknown_pca_scores,
                                         cluster_model,
                                         verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Handle single-cluster case
  ## ---------------------------------------------------------------------------

  if (cluster_model$n_clusters == 1 || is.null(cluster_model$kmeans_model)) {

    if (verbose) cli::cli_text("├─ Single cluster - all unknowns assigned to Cluster 1")

    return(rep(1, nrow(unknown_pca_scores)))
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract PCA dimensions
  ## ---------------------------------------------------------------------------

  pca_cols       <- grep("^Dim\\.", names(unknown_pca_scores), value = TRUE)
  unknown_matrix <- as.matrix(unknown_pca_scores[pca_cols])
  centers        <- cluster_model$kmeans_model$centers

  ## ---------------------------------------------------------------------------
  ## Step 3: Assign to nearest centroid
  ## ---------------------------------------------------------------------------

  assignments <- apply(unknown_matrix, 1, function(x) {

    distances <- apply(centers, 1, function(center) {
      sqrt(sum((x - center)^2))
    })

    which.min(distances)
  })

  if (verbose) {

    cluster_counts <- table(assignments)
    cluster_summary <- paste(sprintf("%d→Cluster_%d", cluster_counts, names(cluster_counts)),
                            collapse = ", ")

    cli::cli_text("├─ Unknown assignments: {cluster_summary}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Return assignments
  ## ---------------------------------------------------------------------------

  return(as.integer(assignments))
}



## ===========================================================================



#' Create Clustered Training Subsets
#'
#' Partitions OSSL training data into cluster-specific subsets with train/val
#' splits. Each cluster gets its own training and validation sets, preserving
#' all covariate columns.
#'
#' @param ossl_pca_scores Tibble with all OSSL PCA scores + covariate columns
#'   (Dim.1, Dim.2, ..., clay, ph, oc, etc.)
#' @param cluster_model List from cluster_ossl_samples() with cluster_assignments
#' @param prop Numeric. Training proportion (default: 0.85)
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return Named list of lists, one per cluster:
#'   Cluster_1 = list(train = tibble(...), val = tibble(...))
#'   Cluster_2 = list(train = tibble(...), val = tibble(...))
#'   ...
#'
#' @importFrom dplyr slice
#' @importFrom rsample initial_split training testing
#' @importFrom cli cli_text cli_alert_success
#' @keywords internal

create_clustered_subsets <- function(ossl_pca_scores,
                                     cluster_model,
                                     prop    = 0.85,
                                     verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Handle single-cluster case
  ## ---------------------------------------------------------------------------

  if (cluster_model$n_clusters == 1) {

    split_obj <- rsample::initial_split(ossl_pca_scores, prop = prop)

    return(list(
      Cluster_1 = list(
        train = rsample::training(split_obj),
        val   = rsample::testing(split_obj)
      )
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Partition by cluster
  ## ---------------------------------------------------------------------------

  cluster_assignments <- cluster_model$cluster_assignments
  training_subsets    <- vector("list", cluster_model$n_clusters)
  names(training_subsets) <- paste0("Cluster_", seq_len(cluster_model$n_clusters))

  for (cluster_id in seq_len(cluster_model$n_clusters)) {

    ## Get indices for this cluster
    cluster_indices <- which(cluster_assignments == cluster_id)
    cluster_data    <- dplyr::slice(ossl_pca_scores, cluster_indices)

    ## Split into train/val
    split_obj <- rsample::initial_split(cluster_data, prop = prop)

    training_subsets[[cluster_id]] <- list(
      train = rsample::training(split_obj),
      val   = rsample::testing(split_obj)
    )

    if (verbose) {

      n_train <- nrow(training_subsets[[cluster_id]]$train)
      n_val   <- nrow(training_subsets[[cluster_id]]$val)

      cli::cli_text("│  ├─ Cluster_{cluster_id}: {format(n_train, big.mark = ',')} training | {format(n_val, big.mark = ',')} validation")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Report summary
  ## ---------------------------------------------------------------------------

  if (verbose) {
    total_train <- sum(sapply(training_subsets, function(x) nrow(x$train)))
    total_val   <- sum(sapply(training_subsets, function(x) nrow(x$val)))

    cli::cli_alert_success("Created {cluster_model$n_clusters} clustered training subsets ({format(total_train, big.mark = ',')} train / {format(total_val, big.mark = ',')} val)")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Return subsets
  ## ---------------------------------------------------------------------------

  return(training_subsets)
}
