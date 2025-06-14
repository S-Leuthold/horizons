% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inputs-create_project_data.R
\name{create_project_data}
\alias{create_project_data}
\title{Create Unified Input Data for Spectral Modeling}
\usage{
create_project_data(
  projects,
  variables = character(),
  save_spectra = FALSE,
  save_locale = NULL,
  drop_na = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{projects}{A named list of project entries, typically created using
\code{project_list()}, where each entry includes a
\code{spectra_path}, \code{sample_obs}, and
\code{default_fraction}.}

\item{variables}{A character vector of sample observation variables to
join with the spectral matrix (e.g., \code{"Sand"}, \code{"pH"}).}

\item{save_spectra}{Logical; should the final dataset be saved to disk as an
\code{.rds} file? Defaults to \code{FALSE}.}

\item{save_locale}{Folder path for saving output if \code{save_spectra = TRUE}.
Required if saving is enabled.}

\item{drop_na}{Logical; if \code{TRUE}, drops rows where all requested
\code{variables} are missing. Defaults to \code{TRUE}.}

\item{verbose}{Logical; print progress messages using \pkg{cli}. Defaults to \code{TRUE}.}
}
\value{
A tibble in wide format with:
\itemize{
\item One row per sample and fraction
\item Columns for Project, Sample_ID, Fraction, and absorbance values (600–4000 cm⁻¹)
\item Optionally joined sample-level variables
}
}
\description{
Imports and processes OPUS spectral data from one or more projects, joins
the cleaned spectra with sample-level observational data, and returns a
wide-format matrix ready for modeling. Supports flexible project-specific
paths and optional inclusion of metadata or covariates.
}
\examples{
\dontrun{
projects <- project_list(
  FFAR = project_entry("data/FFAR/spectra", "data/FFAR/soil.csv"),
  AONR = project_entry("data/AONR/OPUS", "data/AONR/soil.csv", "Clay")
)

df <- create_input_data(
  projects  = projects,
  variables = c("Sand", "pH")
)

glimpse(df)
}

}
\seealso{
\code{\link{project_entry}}, \code{\link{project_list}}, \code{\link{read_spectral_data}}
}
