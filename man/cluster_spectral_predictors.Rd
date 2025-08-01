% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-cluster_predictors.R
\name{cluster_spectral_predictors}
\alias{cluster_spectral_predictors}
\title{Cluster Spectral Predictors to Reduce Redundancy}
\usage{
cluster_spectral_predictors(
  spectra_mat,
  k = 300,
  method = c("correlation", "euclidean")
)
}
\arguments{
\item{spectra_mat}{A numeric matrix or data frame with columns as wavenumber predictors.}

\item{k}{Integer. Number of clusters to retain (e.g., 300).}

\item{method}{Character. Clustering method: either \code{"correlation"} or \code{"euclidean"}.}
}
\value{
A list with three elements:
\describe{
\item{reduced_mat}{A matrix with one representative column per cluster.}
\item{cluster_map}{A named list mapping representative wavenumbers to all members of their cluster.}
\item{selected_vars}{Character vector of retained wavenumber names.}
}
}
\description{
Groups collinear or redundant spectral bands into clusters and returns a reduced
feature matrix containing one representative wavenumber per cluster.
}
\examples{
\dontrun{
result <- cluster_spectral_predictors(spectra, k = 300)
head(result$reduced_mat)
}

}
