#' Cluster MIR Spectra Using PCA and K-Means Clustering
#'
#' Reduces dimensionality of mid-infrared (MIR) spectral data using PCA, then identifies optimal clusters
#' via silhouette analysis and finalizes clustering with k-means.
#' Clusters are used to guide local model calibration for soil covariate prediction.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats prcomp dist kmeans
#' @importFrom cluster silhouette
#' @importFrom glue glue
#'
#' @param input_data A tibble containing Sample_ID, spectral features (numeric wavenumber columns),
#'                   and optional metadata.
#'
#' @return A list containing:
#' \describe{
#'   \item{input_data}{Input data with an added `Cluster` column assigning each sample to a cluster.}
#'   \item{pca_model}{PCA model object (`prcomp`) used for dimensionality reduction.}
#'   \item{kmeans_model}{K-means clustering model fitted to PCA scores.}
#'   \item{ncomp}{Number of PCA components needed to explain 99.5% of total variance.}
#' }
#'
#' @details
#' If the silhouette-optimal number of clusters exceeds 5, the function caps the number of clusters at 3
#' to prioritize computational efficiency and interpretability.
#'
#' @examples
#' \dontrun{
#' clustered_data <- cluster_input_data(my_spectral_data)
#' head(clustered_data$input_data)
#' }
#'
#' @seealso
#'  \code{\link[stats]{prcomp}}, \code{\link[stats]{kmeans}}, \code{\link[cluster]{silhouette}}
#'
#' @keywords internal

cluster_spectral_data <- function(input_data) {

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


