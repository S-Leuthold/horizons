#' Create Input Data for Spectral Modeling
#'
#' Imports and formats raw OPUS spectral data into a wide-format absorbance matrix
#' joined to selected soil physicochemical properties. Applies trimming and resampling
#' to a common wavenumber range (600–4000 cm⁻¹) at 2 cm⁻¹ intervals. No spectral transformations are applied.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom cli cli_abort cli_progress_step cli_progress_update cli_progress_bar cli_progress_done
#' @importFrom readr read_rds
#'
#' @param include A character vector of project identifiers to include (e.g., `"MOYS"`, `"AGRO"`).
#' @param variables A character vector of soil physicochemical variable names to join (e.g., `"Sand"`, `"pH"`, `"SOC"`).
#' @param save_spectra Logical; should the output dataset be saved as an `.rds` file? Defaults to \code{FALSE}.
#' @param save_locale Optional character string specifying the path where the dataset should be saved
#'        (only required if \code{save_spectra = TRUE}).
#'
#' @return A wide-format tibble with:
#' \itemize{
#'   \item{One row per Sample_ID and soil fraction}
#'   \item{Columns for 600–4000 cm⁻¹ wavenumber absorbance values (2 cm⁻¹ spacing)}
#'   \item{Joined soil property columns specified by \code{variables}}
#' }
#'
#' @details
#' The function workflow consists of:
#' \enumerate{
#'   \item{Locating OPUS spectral files for specified projects.}
#'   \item{Reading, trimming, and resampling each spectrum.}
#'   \item{Averaging replicate scans for the same sample.}
#'   \item{Pivoting spectra to wide format (one column per wavenumber).}
#'   \item{Joining soil physicochemical data.}
#' }
#' Spectra are resampled to a common grid to ensure compatibility across samples.
#'
#' @seealso
#' \code{\link{read_spectral_data}}, \code{\link{join_physicochemical_data}}, \code{\link{predict_covariates}}
#'
#' @examples
#' \dontrun{
#' # Example usage
#' df <- create_input_data(
#'   include       = c("MOYS"),
#'   variables     = c("Sand", "pH"),
#'   save_spectra  = FALSE
#' )
#'
#' glimpse(df)
#' }
#'
#' @export

create_input_data <- function(include       = character(),
                              variables     = character(),
                              save_spectra  = FALSE,
                              save_locale   = NA) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  if (length(include) == 0) {
    cli::cli_abort("`include` must contain at least one project identifier.")
  }

  if (!is.logical(save_spectra)) {
    cli::cli_abort("`save_spectra` must be TRUE or FALSE.")
  }

  if (save_spectra && is.na(save_locale)) {
    cli::cli_abort("If `save_spectra = TRUE`, a valid `save_locale` must be provided.")
  }

  cli::cli_progress_step("Identifying OPUS files from {.val {length(include)}} project(s).")

  ## ---------------------------------------------------------------------------
  ## Step 1: Identify and locate spectral files
  ## ---------------------------------------------------------------------------

  File_Locations <- get_file_location(include)

  cli::cli_progress_step("Reading and resampling spectral data from {.val {nrow(File_Locations)}} files.")

  ## ---------------------------------------------------------------------------
  ## Step 2: Read, resample, and trim spectra
  ## ---------------------------------------------------------------------------

  pb <- cli::cli_progress_bar("Reading and resampling spectral data.", total = nrow(File_Locations))

  File_Locations %>%
    purrr::pmap(function(Project,
                         Location,
                         File_Name,
                         ...) {

      cli::cli_progress_update(id = pb)

      read_spectral_data(data_source  = Location,
                         file_name    = File_Name,
                         project_name = Project)
    }) %>%
    dplyr::bind_rows() -> Spectral_Data

  cli::cli_progress_step("Spectral import complete. Averaging across scans.")

  ## ---------------------------------------------------------------------------
  ## Step 3: Average across multiple scans
  ## ---------------------------------------------------------------------------

  Spectral_Data %>%
    dplyr::group_by(Project,
                    Sample_ID,
                    Fraction,
                    Wavenumber) %>%
    dplyr::summarise(Absorbance = mean(Absorbance, na.rm = TRUE),
                     .groups = "drop") -> Spectral_Data

  cli::cli_progress_step("Pivoting spectral data to wide format.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Pivot to wide format
  ## ---------------------------------------------------------------------------

  Spectral_Data %>%
    tidyr::pivot_wider(names_from  = Wavenumber,
                       values_from = Absorbance) -> Spectral_Data_Wide

  cli::cli_progress_step("Joining physicochemical data ({.val {length(variables)}} variables)")

  ## ---------------------------------------------------------------------------
  ## Step 5: Join physicochemical data
  ## ---------------------------------------------------------------------------

  join_physicochemical_data(spectral_data = Spectral_Data_Wide,
                            projects      = include,
                            variables     = variables) -> Final_Data

  ## ---------------------------------------------------------------------------
  ## Step 6: Optionally save the data
  ## ---------------------------------------------------------------------------

  if (save_spectra) {

    cli::cli_progress_step("Saving output file to {.path {save_locale}}.")

    saveRDS(Final_Data,
            file = file.path(save_locale,
                             paste0("Model_Input_Data_", Sys.Date(), ".RDS")))
  }

  cli::cli_progress_step("Spectral input data construction complete.")
  cli::cli_progress_done()

  ## ---------------------------------------------------------------------------
  ## Step 7: Return final data
  ## ---------------------------------------------------------------------------

  return(Final_Data)
}
