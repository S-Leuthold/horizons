#' Read Absorbance Data from an OPUS File
#'
#' Extracts absorbance values from a single `.0` OPUS file. Attempts to read the
#' \code{"ab_no_atm_comp"} channel first, then falls back to \code{"ab"} if needed.
#'
#' @param file_path Full path to a `.0` OPUS file.
#'
#' @return A tibble with columns: \code{File_Name}, \code{Wavenumber}, \code{Absorbance}.
#'         Includes an attribute \code{"channel_used"} indicating the channel read.
#'         Returns \code{NULL} if no valid absorbance data is found.
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
