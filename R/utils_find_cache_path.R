#' Get Path to Cached OSSL Data
#'
#' Returns the path to a specific cached file if it exists.
#'
#' @param type One of "location", "lab", or "mir".
#' @return Path to the cached file or NULL if not present.
#' @keywords internal

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
