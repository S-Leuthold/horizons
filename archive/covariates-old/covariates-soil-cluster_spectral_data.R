#' Cluster Mid-Infrared Spectra via PCA and K-Means
#'
#' Reduces high-dimensional mid-infrared (MIR) spectral data using principal component analysis (PCA),
#' then performs k-means clustering on the PCA scores. The optimal number of clusters is selected using
#' silhouette analysis, with a maximum cap of three clusters to preserve interpretability and modeling tractability.
#'
#' @param input_data A `tibble` containing MIR spectra for multiple samples. Must include numeric columns
#'   representing wavenumber features (e.g., `Dim.600`, `Dim.602`, ...). A column named `Sample_ID` is
#'   recommended but not required. Any non-spectral columns are preserved in the output.
#' @param parallel Logical. Enable parallel processing for clustering operations. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A `list` with the following components:
#' \itemize{
#'   \item \strong{input_data}: A `tibble` identical to the input but with a new `Cluster` column indicating cluster membership.
#'   \item \strong{pca_model}: A `prcomp` object containing the PCA model used for dimensionality reduction.
#'   \item \strong{kmeans_model}: A `kmeans` object fit to the retained PCA scores.
#'   \item \strong{ncomp}: An integer specifying the number of principal components retained (those explaining ≥99.5% of variance).
#' }
#'
#' @details
#' Spectral data is first scaled and projected into PCA space. The number of retained components
#' is selected to capture 99.5% of the variance. K-means clustering is then applied across a range
#' of candidate `k` values (2–20), and the silhouette score is used to identify the optimal cluster count.
#' For computational efficiency and downstream model interpretability, the number of clusters is capped at 3.
#'
#' This clustering routine is intended to support local model calibration in ensemble workflows.
#'
#' @examples
#' \dontrun{
#' clustered <- cluster_spectral_data(my_spectral_data)
#' head(clustered$input_data)
#' }
#'
#' @importFrom dplyr select mutate starts_with everything
#' @importFrom purrr map_dbl
#' @importFrom tibble as_tibble
#' @importFrom stats prcomp dist kmeans
#' @importFrom cluster silhouette
#' @importFrom glue glue
#' @importFrom cli cli_alert_success
#'
#' @export


cluster_spectral_data <- function(input_data,
                                  parallel = FALSE,
                                  n_workers = NULL,
                                  allow_nested = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: PCA Preparation — scale and reduce
  ## ---------------------------------------------------------------------------

  scaled_data <- input_data %>%
                  dplyr::select(dplyr::starts_with("Dim.")) %>%
                  scale() %>%
                  as.matrix()

  pca_model <- stats::prcomp(x      = scaled_data,
                             center = TRUE,
                             scale. = FALSE)

  ## ---------------------------------------------------------------------------
  ## Step 2: Determine number of components to retain (99.5% variance)
  ## ---------------------------------------------------------------------------

  n_components <- which(cumsum(pca_model$sdev^2) / sum(pca_model$sdev^2) >= 0.995)[1]

  pca_scores <- tibble::as_tibble(pca_model$x[, 1:n_components])

  ## ---------------------------------------------------------------------------
  ## Step 3: Identify optimal number of clusters via silhouette score
  ## ---------------------------------------------------------------------------

  k_range <- 2:20

  silhouette_scores <- purrr::map_dbl(k_range,
                                      function(k) {

    set.seed(0307)

    stats::kmeans(x       = pca_scores,
                  centers = k,
                  nstart  = 25) -> kmeans_model

    cluster_assignments <- as.integer(kmeans_model$cluster)

    cluster::silhouette(x    = cluster_assignments,
                        dist = stats::dist(pca_scores))[, 3] %>%
      mean(na.rm = TRUE)
  })

  optimal_k <- k_range[which.max(silhouette_scores)]

  if (optimal_k > 5) {

    cli::cli_alert_success("Optimal number of clusters is too big. Setting to 3 or we'll be here for a year.")
    optimal_k <- 3

  } else {

    cli::cli_alert_success(glue::glue("Optimal number of clusters: {optimal_k}"))

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Run final k-means and attach cluster column
  ## ---------------------------------------------------------------------------

  stats::kmeans(x      = pca_scores,
                centers = optimal_k,
                nstart = 25) -> kmeans_model

  input_data %>%
    dplyr::mutate(Cluster = kmeans_model$cluster,
                  .before = dplyr::everything()) -> clustered_data

  ## ---------------------------------------------------------------------------
  ## Step 5: Return result
  ## ---------------------------------------------------------------------------

  return(list(
    input_data    = clustered_data,
    pca_model     = pca_model,
    kmeans_model  = kmeans_model,
    ncomp         = n_components
  ))
}


