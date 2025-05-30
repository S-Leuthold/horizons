% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_get_file_locations.R
\name{get_file_location}
\alias{get_file_location}
\title{Get File Locations for OPUS Spectral Files}
\usage{
get_file_location(projects)
}
\arguments{
\item{projects}{A character vector of project folder names (e.g., \code{"MOYS"}, \code{"AONR"}) to search for OPUS files.}
}
\value{
A tibble containing:
\itemize{
\item{\code{Project}} {Project identifier.}
\item{\code{Location}} {Full file path to the OPUS spectral file.}
\item{\code{File_Name}} {Original file name.}
}
}
\description{
Constructs a tibble of file paths for all OPUS-format spectral files associated
with one or more project folders. Checks for folder existence and warns if no
files are found within a specified directory.
}
\details{
The function performs the following steps:
\enumerate{
\item{Constructs a set of base paths based on the provided project identifiers.}
\item{Validates the existence of each directory.}
\item{Enumerates OPUS files within each folder.}
\item{Builds a tidy tibble summarizing project, file name, and path information.}
}

Folders are assumed to live under \code{../../2_Data/1_Input_Data/1_Spectral_Data/}.
If a project folder is missing or empty, a warning is issued (but processing continues).
}
\examples{
\dontrun{
# Locate all OPUS files for two projects
get_file_location(projects = c("MOYS", "AONR"))
}

}
\seealso{
\code{\link{read_spectral_data}}, \code{\link{create_input_data}}
}
\keyword{internal}
