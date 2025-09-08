#' Get Path to Processed MIR Spectra File
#'
#' Returns the full path to the cached processed mid-infrared (MIR) spectral
#' dataset if it exists on disk. This file typically contains spectra that have
#' been preprocessed (e.g., trimmed, smoothed, transformed) for modeling use.
#'
#' @return A character string giving the path to the `ossl_mir_processed.qs` file,
#'         or `NULL` if the file does not exist.
#'
#' @details
#' This function checks the Horizons user cache directory
#' (`tools::R_user_dir("horizons", "cache")`) for a processed version of the
#' raw OSSL MIR spectra. It is intended to support reproducible preprocessing
#' workflows without redundant computation.
#'
#' @seealso \code{\link{get_ossl_data_path}}, \code{\link{download_horizons_data}}
#'
#' @examples
#' \dontrun{
#' get_processed_mir_path()
#' }
#'
#' @importFrom tools R_user_dir
#' @export

get_processed_mir_path <- function() {

  cache_dir <- tools::R_user_dir("horizons", "cache")

  processed_file <- file.path(cache_dir, "ossl_mir_processed.qs")

  if (file.exists(processed_file)) {

    return(processed_file)

    } else {

      return(NULL)
  }
}
