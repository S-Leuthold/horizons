% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariates-soil-cluster_spectral_data.R
\name{cluster_spectral_data}
\alias{cluster_spectral_data}
\title{Cluster Mid-Infrared Spectra via PCA and K-Means}
\usage{
cluster_spectral_data(input_data)
}
\arguments{
\item{input_data}{A \code{tibble} containing MIR spectra for multiple samples. Must include numeric columns
representing wavenumber features (e.g., \code{Dim.600}, \code{Dim.602}, ...). A column named \code{Sample_ID} is
recommended but not required. Any non-spectral columns are preserved in the output.}
}
\value{
A \code{list} with the following components:
\itemize{
\item \strong{input_data}: A \code{tibble} identical to the input but with a new \code{Cluster} column indicating cluster membership.
\item \strong{pca_model}: A \code{prcomp} object containing the PCA model used for dimensionality reduction.
\item \strong{kmeans_model}: A \code{kmeans} object fit to the retained PCA scores.
\item \strong{ncomp}: An integer specifying the number of principal components retained (those explaining ≥99.5\% of variance).
}
}
\description{
Reduces high-dimensional mid-infrared (MIR) spectral data using principal component analysis (PCA),
then performs k-means clustering on the PCA scores. The optimal number of clusters is selected using
silhouette analysis, with a maximum cap of three clusters to preserve interpretability and modeling tractability.
}
\details{
Spectral data is first scaled and projected into PCA space. The number of retained components
is selected to capture 99.5\% of the variance. K-means clustering is then applied across a range
of candidate \code{k} values (2–20), and the silhouette score is used to identify the optimal cluster count.
For computational efficiency and downstream model interpretability, the number of clusters is capped at 3.

This clustering routine is intended to support local model calibration in ensemble workflows.
}
\examples{
\dontrun{
clustered <- cluster_spectral_data(my_spectral_data)
head(clustered$input_data)
}

}
