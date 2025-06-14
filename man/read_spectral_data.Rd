% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inputs-read_opus_file.R
\name{read_spectral_data}
\alias{read_spectral_data}
\title{Read Absorbance Data from an OPUS File}
\usage{
read_spectral_data(file_path)
}
\arguments{
\item{file_path}{Full path to a \code{.0} OPUS file.}
}
\value{
A tibble with columns: \code{File_Name}, \code{Wavenumber}, \code{Absorbance}.
Includes an attribute \code{"channel_used"} indicating the channel read.
Returns \code{NULL} if no valid absorbance data is found.
}
\description{
Extracts absorbance values from a single \code{.0} OPUS file. Attempts to read the
\code{"ab_no_atm_comp"} channel first, then falls back to \code{"ab"} if needed.
}
\keyword{internal}
