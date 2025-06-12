#' Download and Cache All Required OSSL Data
#'
#' Downloads location metadata, lab data, and raw MIR data for the OSSL dataset.
#' Stores them in a user-specific cache directory so they don\u2019t need to be
#' redownloaded. If the data already exists, it\u2019s reused silently unless
#' \code{force = TRUE}, in which case the user can choose to re-download it.
#'
#' @param force Logical. If TRUE, forces re-download of all data even if present.
#' @param ask Logical. If TRUE (default), prompts the user for confirmation before download.
#'
#' @return Invisibly returns the paths of the cached files.
#' @export
#'

download_horizons_data <- function(force = FALSE,
                                   ask   = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Set up cache directory and file paths
  ## ---------------------------------------------------------------------------

  cache_dir <- tools::R_user_dir("horizons", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  location_file <- file.path(cache_dir, "ossl_location_data.qs")
  lab_file      <- file.path(cache_dir, "ossl_lab_data.qs")
  mir_file      <- file.path(cache_dir, "ossl_mir_raw.qs")

  existing_files <- all(file.exists(c(location_file,
                                      lab_file,
                                      mir_file)))

  ## ---------------------------------------------------------------------------
  ## Step 2: Check if data already exists
  ## ---------------------------------------------------------------------------

  if (existing_files && !force) {

    cli::cli_alert_success("OSSL data already present in cache at {.path {cache_dir}}")

    return(invisible(list(location = location_file,
                          lab      = lab_file,
                          mir      = mir_file)))
    }

  ## -------------------------------------------------------------------------
  ## Step 3: Prompt the user if data is missing
  ## -------------------------------------------------------------------------

  if (ask) {

      confirm_title <- if (existing_files && force) {
        glue::glue("OSSL data already cached at {cache_dir}. Re-download now?")
      } else {
        glue::glue("The OSSL data (~1–2GB total) is missing in cache at {cache_dir}. Download now?")
      }

      utils::menu(c("Yes, download the data", "No, cancel"),
                  title = confirm_title) -> response

    if (response != 1) {

      cli::cli_alert_info("Download canceled. No changes made.")
      return(invisible(NULL))
    }
  }

  ## -------------------------------------------------------------------------
  ## Step 4: Download and store the data if needed
  ## -------------------------------------------------------------------------

    cli::cli_progress_step("Downloading OSSL location metadata...")
    location_data <- qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_soilsite_L0_v1.2.qs")
    qs::qsave(location_data, location_file)
    cli::cli_alert_success("Saved location metadata to {location_file}")

    cli::cli_progress_step("Downloading OSSL lab measurements...")
    lab_data <- qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_soillab_L1_v1.2.qs")
    qs::qsave(lab_data, lab_file)
    cli::cli_alert_success("Saved lab measurements to {lab_file}")

    cli::cli_progress_step("Downloading OSSL raw MIR spectra...")
    mir_data <- qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_mir_L0_v1.2.qs")
    qs::qsave(mir_data, mir_file)
    cli::cli_alert_success("Saved raw MIR spectra to {mir_file}")

    cli::cli_alert_success("All OSSL data downloaded and cached in {.path {cache_dir}}")

    invisible(list(location = location_file, lab = lab_file, mir = mir_file))

}
