#' Cluster Spectral Predictors to Reduce Redundancy
#'
#' Groups collinear or redundant spectral bands into clusters and returns a reduced
#' feature matrix containing one representative wavenumber per cluster.
#'
#' @param spectra_mat A numeric matrix or data frame with columns as wavenumber predictors.
#' @param k Integer. Number of clusters to retain (e.g., 300).
#' @param method Character. Clustering method: either `"correlation"` or `"euclidean"`.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{reduced_mat}{A matrix with one representative column per cluster.}
#'   \item{cluster_map}{A named list mapping representative wavenumbers to all members of their cluster.}
#'   \item{selected_vars}{Character vector of retained wavenumber names.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- cluster_spectral_predictors(spectra, k = 300)
#' head(result$reduced_mat)
#' }
#'
#' @importFrom stats cor as.dist hclust cutree
#' @importFrom purrr map_lgl map_chr
#' @importFrom cli cli_abort cli_alert_warning
#' @export

cluster_spectral_predictors <- function(spectra_mat,
                                        k      = 300,
                                        method = c("correlation", "euclidean")) {

  method      <- match.arg(method)
  spectra_mat <- as.data.frame(spectra_mat)

  ## ---------------------------------------------------------------------------
  ## 1. Validate input
  ## ---------------------------------------------------------------------------

  if (!all(purrr::map_lgl(spectra_mat, is.numeric))) {

    cli::cli_abort("All columns in `spectra_mat` must be numeric.")

  }

  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 1 || k %% 1 != 0) {

    cli::cli_abort("{.arg k} must be a positive integer.")

  }

  k <- as.integer(k)

  if (ncol(spectra_mat) < k) {

    cli::cli_alert_warning("Number of clusters (k) exceeds number of wavenumbers. Returning original matrix.")

    cluster_map        <- as.list(colnames(spectra_mat))
    names(cluster_map) <- colnames(spectra_mat)

    return(list(reduced_mat   = spectra_mat,
                cluster_map   = cluster_map,
                selected_vars = colnames(spectra_mat)))

  }

  ## ---------------------------------------------------------------------------
  ## 2. Compute distance matrix
  ## ---------------------------------------------------------------------------

  if (method == "correlation") {

    cormat  <- cor(spectra_mat, use = "pairwise.complete.obs")

    if (anyNA(cormat)) {

      cli::cli_abort(c(
        "Correlation matrix contains NA values",
        "i" = "Remove zero-variance predictors or use {.arg method} = {.val euclidean}"
      ))

    }

    distmat <- as.dist(1 - abs(cormat))

    } else {

      distmat <- dist(t(spectra_mat))

  }

  ## ---------------------------------------------------------------------------
  ## 3. Hierarchical clustering
  ## ---------------------------------------------------------------------------

  hc       <- hclust(distmat, method = "average")
  clusters <- cutree(hc, k = k)

  ## ---------------------------------------------------------------------------
  ## 4. Identify one representative per cluster
  ## ---------------------------------------------------------------------------

  cluster_map <- split(names(clusters), clusters)

  purrr::map_chr(cluster_map,
                 function(group) {

                   wave_nums <- suppressWarnings(as.numeric(group))

                   if (all(is.na(wave_nums))) {

                     return(group[1])

                   } else {

                     median_idx <- which.min(abs(wave_nums - median(wave_nums, na.rm = TRUE)))

                     return(group[median_idx])
                   }
                 }) -> representative_bands

  ## ---------------------------------------------------------------------------
  ## 5. Return reduced matrix + metadata
  ## ---------------------------------------------------------------------------

  reduced_mat <- spectra_mat[, representative_bands, drop = FALSE]

  names(cluster_map) <- representative_bands

  return(list(reduced_mat   = reduced_mat,
              cluster_map   = cluster_map,
              selected_vars = representative_bands))
}
