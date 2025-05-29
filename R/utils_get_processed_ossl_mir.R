#' Get Path to Processed MIR Spectra
#'
#' @return Path to the processed MIR spectra `.qs` file if it exists, NULL otherwise.
#' @keywords internal

get_processed_mir_path <- function() {

  cache_dir <- tools::R_user_dir("horizons", "cache")

  processed_file <- file.path(cache_dir, "ossl_mir_processed.qs")

  if (file.exists(processed_file)) {

    return(processed_file)

    } else {

      return(NULL)
  }
}
