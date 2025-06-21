#' Get Path to Cached OSSL Dataset File
#'
#' Returns the full path to a cached Open Soil Spectral Library (OSSL) dataset
#' file if it exists on disk. Supports retrieval of location metadata, lab data,
#' or raw MIR spectra stored via \code{download_horizons_data()}.
#'
#' @param type A character string indicating which dataset to retrieve.
#'   Must be one of:
#'   \itemize{
#'     \item{\code{"location"}}{Geospatial metadata}
#'     \item{\code{"lab"}}{Laboratory measurements (e.g., OC, pH, texture)}
#'     \item{\code{"mir"}}{Raw MIR spectral data}
#'   }
#'
#' @return A character string giving the path to the requested \code{.qs} file,
#'         or \code{NULL} if the file does not exist.
#'
#' @details
#' This function checks the Horizons user cache directory
#' (\code{tools::R_user_dir("horizons", "cache")}) for expected OSSL data files.
#' It is typically used internally to conditionally load cached data or
#' trigger downloads if needed.
#'
#' @seealso \code{\link{download_horizons_data}}
#'
#' @examples
#' \dontrun{
#' get_ossl_data_path("lab")
#' }
#'
#' @importFrom tools R_user_dir
#' @keywords internal
#'

get_ossl_data_path <- function(type = c("location",
                                        "lab",
                                        "mir")) {

  type <- match.arg(type)

  cache_dir <- tools::R_user_dir("horizons", "cache")

  file_map  <- list(location = "ossl_location_data.qs",
                    lab      = "ossl_lab_data.qs",
                    mir      = "ossl_mir_raw.qs")

  file_path <- file.path(cache_dir, file_map[[type]])

  if (file.exists(file_path)) {

    return(file_path)

    } else {

      return(NULL)
  }
}
