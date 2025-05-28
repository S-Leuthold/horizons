#' Read and Resample OPUS Spectral File
#'
#' Reads a single OPUS binary file using the \pkg{opusreader2} package, extracts
#' the appropriate absorbance channel based on project name, reshapes the spectral
#' data into long format, and resamples it to a common 2 cm⁻¹ resolution. The function
#' returns a cleaned tibble restricted to the 600–4000 cm⁻¹ wavenumber range.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom opusreader2 read_opus
#' @importFrom stringr str_split_i
#' @importFrom prospectr resample
#' @importFrom cli cli_warn
#'
#' @param data_source A character string giving the full file path to the OPUS binary file.
#' @param file_name A character string specifying the file name (used to parse project metadata).
#' @param project_name A character string identifying the project. Used to select the correct absorbance channel:
#'        \itemize{
#'          \item \code{"NRCS"} → uses channel \code{"ab"}
#'          \item all others → use channel \code{"ab_no_atm_comp"}
#'        }
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{Project}{Project identifier parsed from file name.}
#'   \item{Sample_ID}{Sample identifier parsed from file name.}
#'   \item{Fraction}{Fraction label parsed from file name.}
#'   \item{Wavenumber}{Spectral wavenumber (cm⁻¹), 600–4000 range.}
#'   \item{Absorbance}{Absorbance value at each wavenumber.}
#'   \item{File_Name}{Original file name of the OPUS file.}
#' }
#'
#' @details
#' Internally, this function:
#' \enumerate{
#'   \item Calls \code{opusreader2::read_opus()} on the given file path.
#'   \item Extracts the appropriate absorbance channel and converts to long format.
#'   \item Parses \code{Project}, \code{Sample_ID}, and \code{Fraction} from the file name using underscores.
#'   \item Resamples absorbance to 2 cm⁻¹ intervals using cubic spline interpolation.
#'   \item Filters output to include only wavenumbers \code{<= 4000}.
#' }
#'
#' If an error occurs (e.g., unreadable file), the function returns \code{NULL} and issues a warning.
#'
#' @seealso \code{\link{create_input_data}}, \code{\link{get_file_location}}, \code{opusreader2::read_opus()}
#'
#' @examples
#' \dontrun{
#' # Read and resample a single OPUS file for project "MOYS"
#' read_spectral_data(data_source  = "/path/to/file/2022_MOYS_001_Clay.opus",
#'                    file_name    = "2022_MOYS_001_Clay.opus",
#'                    project_name = "MOYS")
#' }
#'
#' @keywords internal

read_spectral_data <- function(data_source,
                               file_name,
                               project_name) {

  tryCatch({

    ## -------------------------------------------------------------------------
    ## Step 1: Read OPUS data using correct channel
    ## -------------------------------------------------------------------------

    suppressWarnings(
      opusreader2::read_opus(trimws(data_source))
      ) -> raw_opus_data

    channel <- ifelse(project_name == "NRCS", "ab", "ab_no_atm_comp")

    ## -------------------------------------------------------------------------
    ## Step 2: Extract and tidy the absorbance data
    ## -------------------------------------------------------------------------

    raw_opus_data %>%
      purrr::pluck(trimws(file_name)) %>%
      purrr::pluck(channel) %>%
      purrr::pluck("data") %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::rename(Wavenumber = name,
                    Absorbance = value) %>%
      dplyr::mutate(File_Name  = file_name,
                    Project    = stringr::str_split_i(file_name, pattern = "_", i = 1),
                    Sample_ID  = stringr::str_split_i(file_name, pattern = "_", i = 2),
                    Fraction   = stringr::str_split_i(file_name, pattern = "_", i = 3),
                    Wavenumber = as.numeric(Wavenumber)) %>%
      dplyr::select(Project,
                    Sample_ID,
                    Fraction,
                    Wavenumber,
                    Absorbance,
                    File_Name) -> Spectral_Data

    ## -------------------------------------------------------------------------
    ## Step 3: Resample and trim spectra to 600–4000 cm⁻¹
    ## -------------------------------------------------------------------------

    prospectr::resample(X        = Spectral_Data$Absorbance,
                        wav      = Spectral_Data$Wavenumber,
                        new.wav  = seq(600, 7500, 2),
                        interpol = "spline") %>%
      unname() %>%
      tibble::tibble(Absorbance = .,
                     Wavenumber = seq(600, 7500, 2)) %>%
      dplyr::filter(Wavenumber <= 4000) %>%
      dplyr::bind_cols(Spectral_Data %>%
                       dplyr::select(Project,
                                     Sample_ID,
                                     Fraction,
                                     File_Name) %>%
                  dplyr::distinct(),
                .) -> Clean_Spectral_Data

    return(Clean_Spectral_Data)

  }, error = function(e) {

    cli::cli_warn("Error reading file {.file {file_name}}: {e$message}")
    return(NULL)

  })
}
