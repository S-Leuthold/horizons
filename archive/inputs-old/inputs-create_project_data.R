#' Create Unified Input Data for Spectral Modeling
#'
#' Imports, processes, and merges OPUS spectral files across one or more projects. Applies
#' standardized resampling (2 cm⁻¹ resolution, 600–4000 cm⁻¹), parses metadata from filenames,
#' joins cleaned spectra with sample-level observations, and returns a wide-format matrix
#' ready for MIR model development. Can optionally save the output to disk.
#'
#' @param projects A named list of project entries, typically created using
#'   \code{\link{project_list}}, where each element contains:
#'   \itemize{
#'     \item \code{spectra_path}: folder containing OPUS `.0` files
#'     \item \code{sample_obs}: path to CSV file with sample-level data
#'     \item \code{file_name_format}: string defining the filename parsing template
#'     \item \code{file_name_delimiter}: character delimiter in filenames
#'     \item \code{default_fraction}: fallback for samples lacking parsed fraction info
#'   }
#' @param variables Character vector of sample observation variables (e.g., \code{"Sand"}, \code{"pH"}) to join.
#' @param save_spectra Logical. If \code{TRUE}, saves the full wide-format tibble as an `.rds` file.
#' @param save_locale Directory path for saving the `.rds` file if \code{save_spectra = TRUE}.
#' @param drop_na Logical. If \code{TRUE}, rows missing all requested variables are dropped. Default = \code{TRUE}.
#' @param verbose Logical. Whether to print progress messages using \pkg{cli}. Default = \code{TRUE}.
#'
#' @return A tibble in wide format with:
#' \itemize{
#'   \item One row per unique Project–Sample_ID–Fraction
#'   \item Columns for absorbance at 2 cm⁻¹ intervals from 600 to 4000 cm⁻¹
#'   \item Any matched sample-level observation columns requested via \code{variables}
#' }
#'
#' @details
#' This function is a standardized entry point for spectral model development. Each `.0` OPUS file
#' is read and resampled to a uniform grid. Metadata is parsed from filenames, and absorbance spectra
#' are reshaped to wide format. Optional sample variables are joined from a CSV defined in each project entry.
#'
#' Spectra are grouped and averaged when multiple scans exist for the same Sample_ID–Fraction pair.
#'
#' A warning is issued for projects with no `.0` files or if OPUS channels are inconsistent within a project.
#' Invalid or unreadable files are skipped, but execution continues across other projects.
#'
#' @seealso
#'   \code{\link{project_entry}}, \code{\link{project_list}}, \code{\link{read_spectral_data}},
#'   \code{\link{parse_filename_metadata}}
#'
#' @examples
#' \dontrun{
#' projects <- project_list(
#'   FFAR = project_entry("data/FFAR/spectra", "data/FFAR/soil.csv"),
#'   AONR = project_entry("data/AONR/OPUS", "data/AONR/soil.csv", default_fraction = "Clay")
#' )
#'
#' spectra_data <- create_project_data(
#'   projects      = projects,
#'   variables     = c("Sand", "pH"),
#'   save_spectra  = TRUE,
#'   save_locale   = "data/processed"
#' )
#' }
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @importFrom stringr str_detect
#' @importFrom readr read_csv
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_abort cli_warn cli_h2 cli_progress_done
#' @importFrom fs path
#' @importFrom utils combn
#'
#' @family input_preparation
#'
#' @export


create_project_data <- function(projects,
                                variables        = character(),
                                save_spectra     = FALSE,
                                save_locale      = NULL,
                                drop_na          = TRUE,
                                verbose          = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Validate inputs
  ## ---------------------------------------------------------------------------

  if (!is.list(projects) || is.null(names(projects))) {
    cli::cli_abort("`projects` must be a *named list* created via `project_list()`.")
  }

  if (save_spectra && is.null(save_locale)) {
    cli::cli_abort("If `save_spectra = TRUE`, you must specify `save_locale`.")
  }

  if (!is.character(variables) || length(variables) == 0) {
    cli::cli_warn("No variables specified. Will not join sample observations.")
    variables <- character()
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Process each project
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Stage 1: Find files across different defined projects
    ## -------------------------------------------------------------------------

    purrr::imap(.x = projects,
                .f = function(project_def,
                              project_name) {

      if (verbose) cli::cli_h2("Processing {.val {project_name}}")

      list.files(path = project_def$spectra_path,
                 pattern = "\\.0$",
                 full.names = TRUE) -> opus_files

      cli::cli_alert_info("Found {.val {length(opus_files)}} .0 files for the {.val {project_name}} project.")

      if (length(opus_files) == 0) {
        cli::cli_warn("No .0 files found for {.val {project_name}}. Skipping.")
        return(NULL)
      }

      ## -------------------------------------------------------------------------
      ## Stage 2: Read in spectral files.
      ## -------------------------------------------------------------------------

      channel_used <- NULL

      purrr::map(.x = opus_files,
                 .f = function(file_path) {

        file_name <- basename(file_path)

        safely_execute(expr = {

                               ## ----------------------------------------------
                               ## Stage 1: Read spectra
                               ## ----------------------------------------------

                               raw_spectra <- read_spectral_data(file_path = file_path)

                               ## ----------------------------------------------
                               ## Stage 2: Document channel
                               ## ----------------------------------------------

                               this_channel <- attr(raw_spectra, "channel_used")

                               if (is.null(channel_used)) {
                                 channel_used <<- this_channel
                               } else if (!identical(channel_used, this_channel)) {
                                 cli::cli_warn("Inconsistent channel detected in project {.val {project_name}}: {.val {this_channel}} != {.val {channel_used}}.")
                               }

                               ## ----------------------------------------------
                               ## Stage 3: Resample spectra to 600-4000 cm⁻¹
                               ## ----------------------------------------------

                               resampled_wn <-seq(600, 7500, by = 2)

                               prospectr::resample(X   = raw_spectra$Absorbance,
                                                   wav = raw_spectra$Wavenumber,
                                                   new.wav = resampled_wn,
                                                   interpol = "spline") %>%
                                 unname() %>%
                                 tibble::tibble(Wavenumber = resampled_wn,
                                                Absorbance = .) %>%
                                 dplyr::filter(Wavenumber <= 4000) -> raw_spectra

                               ## ----------------------------------------------
                               ## Stage 4: Parse metadata
                               ## ----------------------------------------------

                               parse_filename_metadata(file_name        = basename(file_path),
                                                       format_string    = project_def$file_name_format,
                                                       delimiter        = project_def$file_name_delimiter,
                                                       default_fraction = project_def$default_fraction) -> sample_metadata

                               ## ----------------------------------------------
                               ## Stage 5: Merge metadata with spectra
                               ## ----------------------------------------------

                               raw_spectra %>%
                                 dplyr::mutate(Project    = project_name,
                                               Sample_ID  = sample_metadata$Sample_ID,
                                               Fraction   = sample_metadata$Fraction) %>%
                                 dplyr::select(Project,
                                               Sample_ID,
                                               Fraction,
                                               everything()) -> raw_spectra

                               raw_spectra},
                        error_message = "Failed to read spectral data",
                        default_value = NULL)
          },
        .progress = list(type = "iterator",
                        name = "Reading, resampling, and fetching metadata for the OPUS files.",
                        clear = TRUE)
        ) %>% purrr::compact() %>%
              dplyr::bind_rows() -> spectra_safe

      cli::cli_progress_done()

      spectra <- spectra_safe$result

      if (nrow(spectra) == 0) {
        cli::cli_warn("All OPUS files failed for project {.val {project_name}}. Remove project or replace files to continue.")
        return(NULL)
      }

      if (!is.null(channel_used)) {
        cli::cli_alert_info("{.val {project_name}} spectra were extracted using channel {.val {channel_used}}.")
      }

      ## -------------------------------------------------------------------------
      ## Stage 3: Pivot spectra to wide format
      ## -------------------------------------------------------------------------

      spectra %>%
        dplyr::group_by(Project,
                        Sample_ID,
                        Fraction,
                        Wavenumber) %>%
        dplyr::summarise(Absorbance = mean(Absorbance, na.rm = TRUE),
                         .groups = "drop") %>%
        tidyr::pivot_wider(names_from  = Wavenumber,
                           values_from = Absorbance) -> spectra_wide

      ## ---------------------------------------------------------------------------
      ## Stage 4: Join spectra with sample observations
      ## ---------------------------------------------------------------------------

      cli::cli_alert_info("Joining spectra with sample observations.")

      safely_execute(expr = readr::read_csv(project_def$sample_obs, show_col_types = FALSE),
                     error_message = "Failed to read sample observations csv for {.val {project_name}}",
                     default_value = NULL) -> sample_df_safe

      sample_df <- sample_df_safe$result

      coord_cols <- names(sample_df)[stringr::str_detect(names(sample_df), "Long|Lat|Longitude|Latitude")]

      join_vars <- unique(c(variables, coord_cols))

      safely_execute(expr = {dplyr::left_join(spectra_wide,
                                                sample_df %>%
                                                  dplyr::filter(Project == project_name) %>%
                                                  dplyr::select(Project,
                                                                Sample_ID,
                                                                dplyr::all_of(join_vars)),
                                    by = c("Project", "Sample_ID"))},
                     default_value = NULL,
                     error_message = "Failed to join spectra with sample observations for project '{project_name}'")  -> joined_safe

      joined <- joined_safe$result

      ## -------------------------------------------------------------------------
      ## Stage 5: Optionally drop NA
      ## -------------------------------------------------------------------------

      if (drop_na && length(variables) > 0) {
        joined <- joined %>% dplyr::filter(!if_all(all_of(variables), is.na))
      }

      return(joined)
        }
      ) %>% bind_rows() -> all_projects

  ## ---------------------------------------------------------------------------
  ## Step 2: Optionally save output
  ## ---------------------------------------------------------------------------

  if (save_spectra) {
    cli::cli_alert_success("Saving combined dataset to {.path {save_locale}}")
    saveRDS(all_projects, file.path(save_locale, paste0("Model_Input_Data_", Sys.Date(), ".rds")))
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Return the final dataset
  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("Successfully created input data for all projects. Total samples: {.val {nrow(all_projects)}}.")

  return(all_projects)
}
