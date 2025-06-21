#' Read Absorbance Spectra from an OPUS File
#'
#' Extracts absorbance values from a single `.0` OPUS file, using the \code{"ab_no_atm_comp"}
#' channel when available, or falling back to \code{"ab"}. Returns a long-format tibble
#' with spectral absorbance data and the source channel used.
#'
#' @param file_path Character. Full path to a `.0` OPUS file.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{File_Name}: The basename of the file.
#'     \item \code{Wavenumber}: Wavenumber in cm⁻¹ (as numeric).
#'     \item \code{Absorbance}: Absorbance value at each wavenumber.
#'   }
#' Includes an attribute \code{"channel_used"} indicating which spectral channel was read.
#' Returns \code{NULL} (with a warning) if no valid absorbance data is available.
#'
#' @details
#' This function is designed for use within spectral ingestion workflows such as
#' \code{\link{create_project_data}}. It relies on the \pkg{opusreader2} package to
#' parse binary OPUS files.
#'
#' @seealso \code{\link{create_project_data}}, \code{\link[opusreader2]{read_opus}}
#'
#' @importFrom opusreader2 read_opus
#' @importFrom purrr pluck
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename mutate select
#' @importFrom cli cli_warn
#'
#' @keywords internal



read_spectral_data <- function(file_path) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Read in an OPUS .0 file
  ## ---------------------------------------------------------------------------

  file_name <- basename(file_path)
  raw       <- suppressWarnings(opusreader2::read_opus(file_path))

  ## ---------------------------------------------------------------------------
  ## Step 2: Decide what data channel to ingest (NRCS data was funny about this)
  ## ---------------------------------------------------------------------------

  absorbance   <- purrr::pluck(raw,
                               file_name,
                               "ab_no_atm_comp",
                               "data")

  channel <- "ab_no_atm_comp"

  if (is.null(absorbance)) {
    absorbance <- purrr::pluck(raw, file_name, "ab", "data")
    channel <- "ab"
  }

  if (is.null(absorbance)) {
    cli::cli_warn("No absorbance data found in file {.file {file_name}} (missing both 'ab_no_atm_comp' and 'ab').")
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Pivot and return spectra
  ## ---------------------------------------------------------------------------

  tibble::as_tibble(absorbance) %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::rename(Wavenumber = name, Absorbance = value) %>%
    dplyr::mutate(File_Name  = file_name,
                  Wavenumber = as.numeric(Wavenumber)) %>%
    dplyr::select(File_Name,
                  Wavenumber,
                  Absorbance) -> out

  attr(out, "channel_used") <- channel

  return(out)
}
