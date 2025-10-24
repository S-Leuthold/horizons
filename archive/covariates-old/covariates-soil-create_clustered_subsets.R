#' Create Clustered Training Subsets from PCA Scores
#'
#' Projects training samples into PCA space, assigns them to clusters based on
#' proximity to k-means centroids, and selects a representative subset from each cluster.
#' The most central samples—defined by Euclidean distance in PCA space—are retained according to
#' a user-specified coverage threshold.
#'
#' @param training_data A `tibble` of MIR spectral data already projected into principal component space
#'   (i.e., columns named `Dim.1`, `Dim.2`, ...). All non-spectral columns are retained in the output.
#' @param pca_model A PCA model object returned by `stats::prcomp()` used for projection into PCA space.
#' @param kmeans_model A fitted `kmeans` object whose cluster centers define proximity for sample selection.
#' @param n_components Integer. Number of PCA components to use during projection (e.g., 50).
#' @param coverage Numeric (between 0 and 1). Proportion of samples per cluster to retain based on distance
#'   to the cluster centroid. Defaults to `0.8`.
#'
#' @return A named `list` of `tibble`s. Each element represents a training subset corresponding to one cluster
#'   (e.g., `"Cluster_1"`, `"Cluster_2"`, ...), retaining only the samples closest to the centroid in PCA space.
#'
#' @details
#' This function is designed to facilitate localized model calibration by filtering training samples
#' based on their similarity to cluster centers. This approach reduces outlier influence and computational
#' burden, particularly when paired with local model ensembles. Coverage values near `1.0` retain most samples,
#' while lower values emphasize only the most representative.
#'
#' Cluster assignments are made using nearest-centroid classification in PCA space.
#' Euclidean distances are used for all calculations.
#'
#' @examples
#' \dontrun{
#' training_subsets <- create_clustered_subsets(
#'   training_data = my_training_data,
#'   pca_model     = my_pca_model,
#'   kmeans_model  = my_kmeans_model,
#'   n_components  = 50,
#'   coverage      = 0.8
#' )
#' }
#'
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom stats predict quantile
#' @importFrom cli cli_alert_success
#' @export


create_clustered_subsets <- function(training_data,
                                     pca_model,
                                     kmeans_model,
                                     n_components,
                                     coverage = 0.8) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Project training data into PCA space
  ## ---------------------------------------------------------------------------

  training_data %>%
    dplyr::select(tidyselect::starts_with("Dim.")) %>%
    scale() %>%
    as.matrix() -> scaled_data

  stats::predict(pca_model,
                 newdata = scaled_data)[, 1:n_components] %>%
    tibble::as_tibble() -> pca_scores

  ## ---------------------------------------------------------------------------
  ## Step 2: Assign training samples to clusters
  ## ---------------------------------------------------------------------------

  apply(pca_scores, 1, function(x) {
    apply(kmeans_model$centers, 1, function(center) sqrt(sum((x - center)^2))) %>%
      which.min()
  }) -> assigned_clusters

  ## ---------------------------------------------------------------------------
  ## Step 3: Build cluster-specific subsets
  ## ---------------------------------------------------------------------------

  purrr::map(unique(assigned_clusters), function(cluster_id) {

    cluster_indices <- which(assigned_clusters == cluster_id)

    cluster_scores <- pca_scores[cluster_indices, ]

    center <- kmeans_model$centers[cluster_id, ]

    distances <- apply(cluster_scores, 1, function(x) sqrt(sum((x - center)^2)))

    threshold <- stats::quantile(distances, probs = coverage)

    selected_indices <- cluster_indices[which(distances <= threshold)]

    training_data[selected_indices, ]

  }) -> training_subsets

  ## ---------------------------------------------------------------------------
  ## Step 4: Return named list
  ## ---------------------------------------------------------------------------

  names(training_subsets) <- paste0("Cluster_", seq_along(training_subsets))

  cli::cli_alert_success("Created {.val {length(training_subsets)}} clustered training subsets.")

  return(training_subsets)

}
