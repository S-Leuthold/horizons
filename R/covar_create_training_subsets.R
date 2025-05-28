#' Create Clustered Training Subsets Based on PCA Projection
#'
#' Projects training samples into PCA space, assigns them to clusters based on distance
#' to k-means centroids, and selects a representative subset of samples within each cluster.
#' The closest samples to the cluster center are retained according to a user-specified coverage proportion.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict quantile
#'
#' @param training_data A tibble containing spectral data projected onto PCA components
#'                      (e.g., columns `Dim.1`, `Dim.2`, ..., `Dim.n`) and associated metadata.
#' @param pca_model A PCA model object created via \code{\link[stats]{prcomp}}.
#' @param kmeans_model A fitted k-means clustering model (from \code{\link[stats]{kmeans}}).
#' @param n_components Integer. Number of principal components to use when projecting data.
#' @param coverage Numeric (between 0 and 1). Proportion of samples within each cluster
#'                 to retain based on proximity to the centroid (default = 0.8).
#'
#' @return A named list of tibbles. Each tibble contains the most representative training samples
#'         for a given cluster (e.g., "Cluster_1", "Cluster_2", etc.).
#'
#' @details
#' This method prioritizes samples closest to the cluster center in PCA space, improving calibration efficiency
#' by removing outliers and redundant points. Coverage can be tuned to balance model robustness and training size.
#'
#' @examples
#' \dontrun{
#' training_subsets <- create_training_subsets(training_data = my_data,
#'                                             pca_model     = my_pca_model,
#'                                             kmeans_model  = my_kmeans_model,
#'                                             n_components  = 50,
#'                                             coverage      = 0.8)
#' }
#'
#' @seealso
#'  \code{\link[stats]{prcomp}}, \code{\link[stats]{kmeans}}, \code{\link[stats]{quantile}}
#'
#' @keywords internal

create_training_subsets <- function(training_data,
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
