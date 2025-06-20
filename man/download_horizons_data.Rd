% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_download_horizons_data.R
\name{download_horizons_data}
\alias{download_horizons_data}
\title{Download and Cache OSSL Core Datasets}
\usage{
download_horizons_data(force = FALSE, ask = TRUE)
}
\arguments{
\item{force}{Logical. If \code{TRUE}, forces re-download of all files even if they are present.}

\item{ask}{Logical. If \code{TRUE} (default), prompts the user before downloading any files.}
}
\value{
Invisibly returns a named list of local file paths for:
\describe{
\item{\code{location}}{Cached path to the OSSL location metadata.}
\item{\code{lab}}{Cached path to the OSSL laboratory data.}
\item{\code{mir}}{Cached path to the OSSL raw MIR spectra.}
}
}
\description{
Downloads and locally caches the Open Soil Spectral Library (OSSL) datasets required
for Horizons workflows, including location metadata, laboratory measurements, and raw
MIR spectral data. Data are stored in the user-level cache directory defined by
\code{tools::R_user_dir("horizons", "cache")}.
}
\details{
If the data already exist, they are reused unless \code{force = TRUE}. If any files are missing,
or if \code{force = TRUE}, the user will be prompted before download (unless \code{ask = FALSE}).

This function ensures reproducibility and minimizes network overhead by storing
the following files in the Horizons cache:
\itemize{
\item \code{ossl_soilsite_L0_v1.2.qs} — site/location metadata
\item \code{ossl_soillab_L1_v1.2.qs} — harmonized lab measurements
\item \code{ossl_mir_L0_v1.2.qs} — raw MIR spectra in absorbance units
}

These datasets are used as base inputs for downstream functions such as \code{build_ossl_dataset()}.
}
\examples{
\dontrun{
download_horizons_data()
}

}
\seealso{
\code{\link{build_ossl_dataset}}, \code{\link[qs]{qread_url}}, \code{\link[tools]{R_user_dir}}
}
