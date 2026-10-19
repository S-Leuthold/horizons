#' Cluster Spectral Predictors to Reduce Redundancy
#'
#' Groups collinear or redundant spectral bands into clusters and returns a reduced
#' feature matrix containing one representative wavenumber per cluster: the
#' cluster's middle band by its position in `spectra_mat`, which for the
#' steps' columns (`spec001` onward, or wavenumbers in the decreasing order
#' the validator keeps) is the middle of the cluster on the spectrum. Of two
#' middle bands, the first is taken.
#'
#' @param spectra_mat A numeric matrix or data frame with columns as wavenumber predictors.
#' @param k Integer. Number of clusters to retain (e.g., 300).
#' @param method Character. Clustering method: either `"correlation"` or `"euclidean"`.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{reduced_mat}{A data frame with one representative column per cluster.}
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
#' @keywords internal

cluster_spectral_predictors <- function(spectra_mat,
                                        k      = 300,
                                        method = c("correlation", "euclidean")) {

  method      <- match.arg(method)
  spectra_mat <- as.data.frame(spectra_mat)

  ### Both callers pass the numeric output of the steps before them and a k
  ### they compute; a non-numeric column fails in cor() and a bad k in
  ### cutree().
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

    ### The steps drop near-constant bands first (drop_constant_bands()), so
    ### a correlation is missing only for a band with no values at all, which
    ### hclust() refuses.
    cormat  <- cor(spectra_mat, use = "pairwise.complete.obs")
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

  ## The middle band of each cluster by position, whatever the names: read as
  ## numbers, the steps' spec001 names were NA, so every cluster was
  ## represented by its first band (decided 2026-10-10)
  positions <- stats::setNames(seq_len(ncol(spectra_mat)), colnames(spectra_mat))

  representative_bands <- vapply(cluster_map, function(group) {
    at <- positions[group]
    group[which.min(abs(at - stats::median(at)))]
  }, character(1))

  ## ---------------------------------------------------------------------------
  ## 5. Return reduced matrix + metadata
  ## ---------------------------------------------------------------------------

  reduced_mat <- spectra_mat[, representative_bands, drop = FALSE]

  names(cluster_map) <- representative_bands

  return(list(reduced_mat   = reduced_mat,
              cluster_map   = cluster_map,
              selected_vars = representative_bands))
}


#' Drop the Near-Constant Bands Before Clustering
#'
#' @description
#' Shared by `step_select_cars()` and `step_select_boruta()`. A band whose
#' variance is at most `1e-10` has no correlation with any other, so it
#' cannot be clustered, and it carries nothing to select. Says how many it
#' drops.
#'
#' @param spectra_mat Numeric matrix, one column per band.
#' @return `spectra_mat` without its near-constant columns.
#' @keywords internal
#' @noRd
drop_constant_bands <- function(spectra_mat) {

  variances <- apply(spectra_mat, 2, stats::var, na.rm = TRUE)
  keep      <- !is.na(variances) & variances > 1e-10

  if (!all(keep)) {

    cli::cli_alert_info("Filtering {sum(!keep)} near-constant features before clustering")

  }

  spectra_mat[, keep, drop = FALSE]

}
