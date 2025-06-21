#' Download and Preprocess OSSL Spectral and Covariate Data
#'
#' Retrieves topsoil mid-infrared (MIR) spectral data and associated covariate measurements
#' from the Open Soil Spectroscopy Library (OSSL). Applies Savitzky-Golay smoothing followed by
#' standard normal variate (SNV) preprocessing to the spectra, reshapes and filters lab data,
#' and joins all components into a unified dataset. Final outputs are cached for reuse.
#'
#' @param covariates A character vector of covariate names to retrieve (e.g., `"Sand"`, `"pH"`, `"SOC"`).
#'   Covariate names must match those defined in the internal OSSL variable dictionary.
#' @param window_size Integer. Width of the Savitzky-Golay smoothing window (must be odd). Defaults to `9`.
#' @param max_samples Optional integer. If supplied, limits the number of samples processed—useful for debugging or testing.
#'   If `NULL`, all available samples are used.
#' @param bounding_box Currently unused. Placeholder for future spatial subsetting based on bounding box coordinates.
#'
#' @return A `tibble` containing:
#' \itemize{
#'   \item Preprocessed MIR spectra: SNV-SG0 transformed reflectance values at 2 cm⁻¹ resolution between 600–4000 cm⁻¹.
#'   \item Covariate data: Wide-format table of measured values for requested soil properties.
#'   \item Sample index: A sequential index column for internal referencing.
#' }
#'
#' @details
#' This function follows a multi-step pipeline:
#' \enumerate{
#'   \item Read and filter the internal OSSL variable dictionary.
#'   \item Load cached raw data files (location, lab, and MIR) from disk using `qs::qread()`.
#'   \item Filter and reshape metadata to include only topsoil layers and desired covariates.
#'   \item Load and optionally subsample MIR spectra.
#'   \item Apply parallelized SNV and Savitzky-Golay preprocessing via `furrr::future_map()`.
#'   \item Cache the resulting processed spectra using `qs::qsave()` for future reuse.
#'   \item Join processed spectra with lab data by `Layer_ID`.
#' }
#'
#' If previously processed MIR spectra exist in cache, they are loaded automatically to avoid reprocessing.
#' Progress and errors are communicated through `cli`-based messaging and `safely_execute()` wrappers.
#'
#' @examples
#' \dontrun{
#' # Download and preprocess OSSL data for pH and Sand
#' ossl_data <- download_ossl_data(covariates = c("pH", "Sand"))
#'
#' # Preview structure
#' glimpse(ossl_data)
#' }
#'
#' @seealso
#' \code{\link{predict_covariates}}, \code{\link{create_input_data}}
#'
#' @importFrom dplyr filter select distinct mutate rename group_by ungroup inner_join starts_with row_number
#' @importFrom purrr map map_chr map_dfr pluck
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom stats quantile
#' @importFrom readr read_csv
#' @importFrom qs qread qread_url qsave
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map furrr_options
#' @importFrom prospectr savitzkyGolay standardNormalVariate
#' @importFrom progressr with_progress handlers progressor
#' @importFrom here here
#' @importFrom glue glue
#' @importFrom cli cli_progress_message cli_progress_step cli_alert_success cli_alert_danger cli_alert_info cli_abort
#' @importFrom stringr str_split_i
#' @export


download_ossl_data <- function(covariates,
                               window_size = 9,
                               max_samples = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Read the OSSL data dictionary
  ## ---------------------------------------------------------------------------

  OSSL_Data_Dictionary <- OSSL_Data_Dictionary

  OSSL_Data_Dictionary %>%
    filter(Sam_Include == TRUE) -> OSSL_Data_Dictionary

  if (is.null(OSSL_Data_Dictionary)) {
    cli::cli_abort("Aborting: Failed to read OSSL data dictionary.")
  }


  ## ---------------------------------------------------------------------------
  ## Step 2: Ensure required data is downloaded
  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {download_horizons_data(force = FALSE,
                                                         ask   = TRUE)},
                 default_value = NULL,
                 log_error     = FALSE,
                 error_message = "Failed to download the required OSSL data") -> download_result_safe

  download_result <- download_result_safe$result

  if (is.null(download_result)) {
    cli::cli_abort("Aborting: Unable to download or locate required OSSL data.")
  }

  cli::cli_progress_step("Required OSSL raw data is available.")

  ## ---------------------------------------------------------------------------
  ## Step 3: Load data from cache
  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {qs::qread(get_ossl_data_path("location"))},
                 default_value = NULL,
                 error_message = "Failed to load the required OSSL location data from cache")   -> location_data_safe

  safely_execute(expr          = {qs::qread(get_ossl_data_path("lab"))},
                 default_value = NULL,
                 error_message = "Failed to load the required OSSL laboratory data from cache") -> lab_data_safe

  safely_execute(expr          = {qs::qread(get_ossl_data_path("mir"))},
                 default_value = NULL,
                 error_message = "Failed to load the required OSSL MIR data from cache")        -> mir_data_safe

  location_data <- location_data_safe$result
  lab_data      <- lab_data_safe$result
  mir_data      <- mir_data_safe$result

  if(is.null(location_data) || is.null(lab_data) || is.null(mir_data)) {
    cli::cli_abort("Aborting: One or more required OSSL datasets failed to load from cache.")
  }

  cli::cli_progress_step("Required datasets successfully loaded from cache.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Subset and clean location data
  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {location_data %>%
                                   dplyr::filter(dataset.code_ascii_txt == "KSSL.SSL") %>%
                                   dplyr::select(Layer_ID  = id.layer_uuid_txt,
                                                 Longitude = longitude.point_wgs84_dd,
                                                 Latitude  = latitude.point_wgs84_dd,
                                                Top_Depth = layer.upper.depth_usda_cm) %>%
                                   dplyr::filter(Top_Depth == 0)},
                 default_value = NULL,
                 error_message = "Failed to subset/clean OSSL location metadata") -> location_data_safe

  location_data <- location_data_safe$result

  if(is.null(location_data)){
    cli::cli_abort("Aborting: Issues with subsetting or cleaning OSSL location metadata.")
  }

  cli::cli_progress_step("Location data processed and ready to go.")

  ## ---------------------------------------------------------------------------
  ## Step 5: Subset and reshape lab data
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Downloading and joining lab measurements")

  safely_execute(expr          = {lab_data %>%
                                   dplyr::distinct() %>%
                                   dplyr::rename(Layer_ID = id.layer_uuid_txt) %>%
                                   dplyr::filter(Layer_ID %in% location_data$Layer_ID) %>%
                                   dplyr::select(-dataset.code_ascii_txt,
                                                 -efferv_usda.a479_class) %>%
                                   tidyr::pivot_longer(cols      = -Layer_ID,
                                                       names_to  = "ossl_name_level1",
                                                       values_to = "Measured_Value") %>%
                                   dplyr::left_join(OSSL_Data_Dictionary,
                                                    by = "ossl_name_level1") %>%
                                   dplyr::select(Layer_ID,
                                                 analyte,
                                                 target_unit,
                                                 Measured_Value) %>%
                                   tidyr::drop_na() %>%
                                   dplyr::mutate(Analyte_Base = stringr::str_split_i(analyte, ",", 1)) %>%
                                   dplyr::filter(Analyte_Base %in% covariates) %>%
                                   dplyr::mutate(final_variable = Analyte_Base) %>%
                                   dplyr::select(Layer_ID,
                                                 final_variable,
                                                 Measured_Value) %>%
                                   tidyr::pivot_wider(names_from  = final_variable,
                                                      values_from = Measured_Value)},
                 default_value = NULL,
                 error_message = "Failed to process OSSL lab measurements") -> lab_data_safe

  lab_data <- lab_data_safe$result

  if (is.null(lab_data)) {
    cli::cli_abort("Aborting: OSSL lab measurements were corrupted at some point.")
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Load OSSL MIR spectra from local cache if available.
  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {get_processed_mir_path()},
                 default_value = NULL,
                 error_message = "Failed to get the path for processed  MIR spectra") -> processed_mir_path_safe

  processed_mir_path <- processed_mir_path_safe$result

  processed_mir <- NULL

  if (!is.null(processed_mir_path)) {

    cli::cli_progress_step("Loading processed MIR spectra from the cache at {processed_mir_path}")

    safely_execute(expr          = {qs::qread(processed_mir_path)},
                   default_value = NULL,
                   error_message = glue::glue("Failed to read processed MIR data from {processed_mir_path}")) -> processed_mir_safe

    processed_mir <- processed_mir_safe$result

    if (!is.null(processed_mir)) {
      cli::cli_progress_step("Successfully loaded processed MIR data from cache.")
    }

  }


  ## ---------------------------------------------------------------------------
  ## Step 7: If necessary, process raw OSSL MIR spectra
  ## ---------------------------------------------------------------------------

  if(is.null(processed_mir)) {

    cli::cli_progress_step("No cached processed MIR spectra found. Processing raw OSSL MIR spectra.")
    cli::cli_progress_step("Filtering and reshaping MIR data.")

    safely_execute(expr          = {mir_data %>%
                                      dplyr::rename(Layer_ID = id.layer_uuid_txt) %>%
                                      dplyr::filter(scan.mir.model.name_utf8_txt == "Bruker Vertex 70 with HTS-XT accessory",
                                                    Layer_ID %in% location_data$Layer_ID) %>%
                                      dplyr::select(Layer_ID,
                                      dplyr::starts_with("scan_mir.")) %>%
                                      tidyr::pivot_longer(cols      = -Layer_ID,
                                                          names_to  = "Wavenumber",
                                                          values_to = "Absorbance") %>%
                                      dplyr::mutate(Wavenumber = stringr::str_split_i(Wavenumber, "\\.", 2),
                                      Wavenumber = stringr::str_split_i(Wavenumber, "_", 1))},
                  default_value = NULL,
                   error_message = "Failed to filter/select/pivot the raw OSSL MIR spectra") -> mir_data_safe

    mir_data <- mir_data_safe$result


  if(is.null(mir_data)){
    cli::cli_abort("Aborting: Raw OSSL MIR spectra preprocessing failed.")
  }

  ## ---------------------------------------------------------------------------

  mir_split <- split(mir_data, mir_data$Layer_ID)

  if (!is.null(max_samples)) {
    sampled_layer_ids <- sample(names(mir_split), size = min(max_samples, length(mir_split)))
    mir_split <- mir_split[sampled_layer_ids]
    cli::cli_alert_info("Processing only {length(mir_split)} samples for testing/debugging.")
  }

  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {future::plan(future::multisession,
                                               workers = parallel::detectCores(logical = TRUE) - 1)},
                 default_value = NULL,
                 error_message = "Failed to set parallel plan for processing")

  cli::cli_progress_step("Applying SNV and SG transformation to MIR data.")

  progressr::handlers("txtprogressbar")

  safely_execute(expr = {progressr::with_progress({

                          p <- progressr::progressor(along = mir_split)

                         furrr::future_map(mir_split,
                                           function(sample_data)
                                           {

                                             p(sprintf("Processing %s", unique(sample_data$Layer_ID)))

                                             sample_data <- dplyr::arrange(sample_data,
                                                                           as.numeric(Wavenumber))

                                             Start_Row <- 1 + ((window_size - 1) / 2)
                                             End_Row   <- nrow(sample_data) - ((window_size - 1) / 2)

                                             sample_data$Absorbance %>%
                                               as.matrix() %>%
                                               t() %>%
                                               prospectr::savitzkyGolay(m = 0,
                                                                        p = 1,
                                                                        w = window_size) %>%
                                               prospectr::standardNormalVariate() %>%
                                               t() %>%
                                               as_tibble(.name_repair = ~ "SNV_SG0_Absorbance") -> spectra_matrix

                                             dplyr::bind_cols(sample_data %>% dplyr::select(-Absorbance) %>% dplyr::slice(Start_Row:End_Row),
                                                              spectra_matrix) %>%
                                               tidyr::pivot_wider(names_from = Wavenumber, values_from = SNV_SG0_Absorbance)

                                           },
                                           .options = furrr::furrr_options(seed = TRUE)) %>%
                           list_rbind()
                       })},
                 default_value = NULL,
                 error_message = "Failed during parallel SNV/SG transformation of OSSL MIR spectra") -> processed_mir_safe

  processed_mir <- processed_mir_safe$result

  if (is.null(processed_mir)) {
    cli::cli_abort("Aborting: OSSL MIR spectra processing failed.")
  }

  qs::qsave(processed_mir, file.path(tools::R_user_dir("horizons", "cache"), "ossl_mir_processed.qs"))
  cli::cli_alert_success("Processed MIR data cached for future use.")

  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("OSSL MIR data processed.")

  safely_execute(expr          = {future::plan(future::sequential)},
                 default_value = NULL,
                 error_message = "Failed to reset parallel plan after preprocessing")

 }
  ## ---------------------------------------------------------------------------

  safely_execute(expr       = {dplyr::inner_join(lab_data,
                                                 processed_mir,
                                                 by = "Layer_ID") %>%
                                dplyr::select(-Layer_ID) %>%
                                dplyr::mutate(Sample_Index = dplyr::row_number(),
                                              .before = everything())},
              default_value = NULL,
              error_message = "Failed to join lab data with processed MIR spectra") -> OSSL_Data_safe

  OSSL_Data <- OSSL_Data_safe$result

  if(is.null(OSSL_Data)) {
    cli::cli_abort("Aborting: Joining lab data with processed MIR spectra failed.")
  }

  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("OSSL data download and preprocessing complete!")


  return(OSSL_Data)

}

