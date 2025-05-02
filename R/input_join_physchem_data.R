#' Join Spectral and Physicochemical Data
#'
#' Loads project-level physicochemical datasets from `.csv` files and joins them to
#' a wide-format spectral dataset using \code{Project} and \code{Sample_ID} as keys.
#' Ensures that only rows with at least one non-missing covariate are retained.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom readr read_csv
#' @importFrom cli cli_alert_danger cli_alert_info
#'
#' @param spectral_data A tibble containing wide-format absorbance data (e.g., one row per sample,
#'        columns = wavenumbers), with columns \code{Project} and \code{Sample_ID}.
#' @param projects Character vector of project identifiers. Each must correspond to a `.csv` file
#'        in \code{"../../2_Data/1_Input_Data/2_Physicochemical_Data/"}.
#' @param variables Character vector of physicochemical variable names to extract and join
#'        (e.g., \code{"Sand"}, \code{"pH"}, \code{"TC"}, etc.).
#'
#' @return A tibble containing only those rows from \code{spectral_data} with
#'         matching physicochemical measurements, joined by \code{Project} and \code{Sample_ID}.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Loads one `.csv` file per project listed in \code{projects}.
#'   \item Converts all non-ID columns to numeric.
#'   \item Selects the specified covariates from each file and removes rows where all are missing.
#'   \item Performs a left join with \code{spectral_data} using Project and Sample_ID.
#' }
#' A failed join or missing file will raise an error with a helpful message.
#'
#' @seealso \code{\link{create_input_data}}, \code{\link{read_spectral_data}}
#'
#' @examples
#' \dontrun{
#' # Join absorbance matrix with sand and pH for "MOYS" and "AONR" projects
#' wide_data <- join_physicochemical_data(
#'   spectral_data = my_spectra,
#'   projects = c("MOYS", "AONR"),
#'   variables = c("Sand", "pH")
#' )
#' }
#'
#' @keywords internal

join_physicochemical_data <- function(spectral_data,
                                      projects,
                                      variables) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Read and merge physicochemical files
  ## ---------------------------------------------------------------------------

  tryCatch({

    purrr::map(projects,
               function(proj) {

                 suppressMessages(
                   readr::read_csv(file = paste0("../../2_Data/1_Input_Data/2_Physicochemical_Data/", proj, ".csv"),
                                   show_col_types = FALSE) %>%
                    dplyr::mutate(across(-c(Project, Sample_ID), as.numeric)))
                 }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(Project,
                    Sample_ID,
                    all_of(variables)) %>%
      dplyr::filter(!if_all(all_of(variables), is.na)) -> soil_data

  }, error = function(e) {

    cli::cli_alert_danger("Could not read physicochemical data for one or more projects.")
    cli::cli_alert_info("Error: {e$message}")
    stop("Join aborted.")

  })

  ## ---------------------------------------------------------------------------
  ## Step 2: Join to spectral data
  ## ---------------------------------------------------------------------------

  tryCatch({

    dplyr::left_join(x  = soil_data,
                     y  = spectral_data,
                     by = join_by(Project,
                                  Sample_ID)) -> joined_data

  }, error = function(e) {

    cli::cli_alert_danger("Failed to join spectral and soil data.")
    cli::cli_alert_info("Error: {e$message}")
    stop("Join aborted.")

  })

  return(joined_data)
}
