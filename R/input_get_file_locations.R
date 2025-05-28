#' Get File Locations for OPUS Spectral Files
#'
#' Constructs a tibble of file paths for all OPUS-format spectral files associated
#' with one or more project folders. Checks for folder existence and warns if no
#' files are found within a specified directory.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom cli cli_abort cli_warn
#'
#' @param projects A character vector of project folder names (e.g., `"MOYS"`, `"AONR"`) to search for OPUS files.
#'
#' @return A tibble containing:
#' \itemize{
#'   \item{\code{Project}} {Project identifier.}
#'   \item{\code{Location}} {Full file path to the OPUS spectral file.}
#'   \item{\code{File_Name}} {Original file name.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item{Constructs a set of base paths based on the provided project identifiers.}
#'   \item{Validates the existence of each directory.}
#'   \item{Enumerates OPUS files within each folder.}
#'   \item{Builds a tidy tibble summarizing project, file name, and path information.}
#' }
#'
#' Folders are assumed to live under \code{../../2_Data/1_Input_Data/1_Spectral_Data/}.
#' If a project folder is missing or empty, a warning is issued (but processing continues).
#'
#' @seealso
#' \code{\link{read_spectral_data}}, \code{\link{create_input_data}}
#'
#' @examples
#' \dontrun{
#' # Locate all OPUS files for two projects
#' get_file_location(projects = c("MOYS", "AONR"))
#' }
#'
#' @keywords internal

get_file_location <- function(projects) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  if (length(projects) == 0) {

    cli::cli_abort("You must specify at least one project folder name.")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Construct base paths
  ## ---------------------------------------------------------------------------

  tibble::tibble(Project          = projects,
                 Folder_Locations = file.path("../../2_Data/1_Input_Data/1_Spectral_Data/", projects)) -> Folder_Locations

  ## ---------------------------------------------------------------------------
  ## Step 3: Enumerate file locations
  ## ---------------------------------------------------------------------------

  Folder_Locations %>%
    purrr::pmap(function(Project,
                         Folder_Locations,
                         ...) {

      if (!dir.exists(Folder_Locations)) {

        cli::cli_abort("Folder not found for project {.val {Project}}: {.path {Folder_Locations}}")

      }

      files <- list.files(Folder_Locations)

      if (length(files) == 0) {

          cli::cli_warn("No files found in folder: {.path {Folder_Locations}}")

      }

      tibble::tibble(Project   = Project,
                     Location  = file.path(Folder_Locations, files),
                     File_Name = files)

    }) %>%
    purrr::list_rbind()

}
