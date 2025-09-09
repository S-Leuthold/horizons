#' Download and Cache OSSL Core Datasets
#'
#' Downloads and locally caches the Open Soil Spectral Library (OSSL) datasets required
#' for Horizons workflows, including location metadata, laboratory measurements, and raw
#' MIR spectral data. Data are stored in the user-level cache directory defined by
#' `tools::R_user_dir("horizons", "cache")`.
#'
#' If the data already exist, they are reused unless `force = TRUE`. If any files are missing,
#' or if `force = TRUE`, the user will be prompted before download (unless `ask = FALSE`).
#'
#' @param force Logical. If `TRUE`, forces re-download of all files even if they are present.
#' @param ask Logical. If `TRUE` (default), prompts the user before downloading any files.
#'
#' @return Invisibly returns a named list of local file paths for:
#' \describe{
#'   \item{`location`}{Cached path to the OSSL location metadata.}
#'   \item{`lab`}{Cached path to the OSSL laboratory data.}
#'   \item{`mir`}{Cached path to the OSSL raw MIR spectra.}
#' }
#'
#' @details
#' This function ensures reproducibility and minimizes network overhead by storing
#' the following files in the Horizons cache:
#' \itemize{
#'   \item `ossl_soilsite_L0_v1.2.qs` — site/location metadata
#'   \item `ossl_soillab_L1_v1.2.qs` — harmonized lab measurements
#'   \item `ossl_mir_L0_v1.2.qs` — raw MIR spectra in absorbance units
#' }
#'
#' These datasets are used as base inputs for downstream functions such as `build_ossl_dataset()`.
#'
#' @seealso \code{\link{build_ossl_dataset}}, \code{\link[qs]{qread_url}}, \code{\link[tools]{R_user_dir}}
#'
#' @examples
#' \dontrun{
#' download_horizons_data()
#' }
#'
#' @importFrom cli cli_alert_success cli_alert_info cli_progress_step
#' @importFrom glue glue
#' @importFrom qs qread_url qsave
#' @importFrom tools R_user_dir
#' @importFrom utils menu
#'
#' @export


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
