% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariates-climate-orchestrator.R
\name{fetch_climate_covariates}
\alias{fetch_climate_covariates}
\title{Fetch Climate Covariates Using Daymet and Hargreaves PET}
\usage{
fetch_climate_covariates(
  input_data,
  start_year = 2003,
  end_year = 2024,
  gdd_base = 10,
  cache_dir = tools::R_user_dir("horizons", "cache")
)
}
\arguments{
\item{input_data}{A data frame with columns \code{Longitude} and \code{Latitude}. Additional columns (e.g., \code{Layer_ID}) are preserved for joining.}

\item{start_year}{The first year of climate data to retrieve (default: 2003).}

\item{end_year}{The final year of climate data to retrieve (default: 2024).}

\item{gdd_base}{Base temperature (°C) for Growing Degree Days calculation (default: 10).}

\item{cache_dir}{Directory for storing Daymet downloads (default: user cache directory).}
}
\value{
A data frame joining input data with climate covariates:
\item{MAT}{Mean annual temperature (°C).}
\item{MAP}{Mean annual precipitation (mm).}
\item{PET}{Mean annual potential evapotranspiration (mm, estimated by Hargreaves).}
\item{AI}{Aridity Index (MAP / PET).}
\item{GDD}{Mean annual Growing Degree Days (base temperature defined by \code{gdd_base}).}
\item{Precip_Seasonality}{Coefficient of variation (CV\%) of monthly precipitation totals.}
}
\description{
This function retrieves daily climate data from Daymet for a set of input coordinates and
computes key climate covariates using the Hargreaves method for PET (Potential Evapotranspiration).
It groups samples by Daymet 4-km grid cell to minimize redundant downloads and summarizes
climate variables (MAT, MAP, PET, AI, GDD, Precip_Seasonality) for each unique grid.
Loud, graceful failures are implemented using cli-based error handling.
}
\details{
The function uses the Daymet API to download daily climate data for each unique 4-km grid cell
represented by the input coordinates. Hargreaves PET is calculated for each day, and
annual summaries are averaged across the specified date range.
This approach minimizes API calls while ensuring data consistency.
}
