% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inputs-parse_metadata.R
\name{parse_filename_metadata}
\alias{parse_filename_metadata}
\title{Parse Sample Metadata from File Name}
\usage{
parse_filename_metadata(
  file_name,
  format_string,
  delimiter = "_",
  default_fraction = "GroundBulk"
)
}
\arguments{
\item{file_name}{Character. The file name (e.g., \code{"FFAR_001_Bulk.0"}).}

\item{format_string}{Character. A format template describing each part of the file name (e.g., \code{"project_sampleid_fraction"}).
Allowed tokens: \code{"sampleid"}, \code{"fraction"}, \code{"project"}, \code{"ignore"}, \code{"wellid"}, \code{"scanid"}.
Tokens must appear in the same order as in the actual file name.}

\item{delimiter}{Character. Delimiter used to split the file name (default = \code{"_"}).}

\item{default_fraction}{Character. Value to assign if \code{fraction} is not provided or cannot be parsed.}
}
\value{
A tibble with columns \code{Sample_ID} and \code{Fraction}, and optionally other parsed tokens.
}
\description{
Interprets a file name using a format string and delimiter to extract sample-level metadata,
such as \code{Sample_ID} and \code{Fraction}. Useful for batch processing OPUS files where metadata
is embedded in the file name.
}
\details{
This function is typically used during OPUS spectral ingestion to extract metadata from filenames.
If a required token like \code{sampleid} is missing from the filename, a warning is issued and the
default value \code{"UNKNOWN"} is used.
}
\seealso{
\code{\link{project_entry}}, \code{\link{create_project_data}}
}
\keyword{internal}
