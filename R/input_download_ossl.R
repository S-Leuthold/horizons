#' Download and Preprocess OSSL MIR and Covariate Data
#'
#' Retrieves topsoil mid-infrared (MIR) spectral data and associated covariate measurements
#' from the Open Soil Spectroscopy Library (OSSL). Applies smoothing and standard normal variate (SNV)
#' preprocessing to spectra, joins soil covariate information, and caches the final processed dataset.
#'
#' If a cached version matching the requested covariates is found, it will be loaded automatically.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict quantile
#' @importFrom readr read_csv
#' @importFrom qs qread qread_url qsave
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map furrr_options
#' @importFrom prospectr savitzkyGolay standardNormalVariate
#' @importFrom progressr with_progress handlers progressor
#' @importFrom here here
#' @importFrom cli cli_progress_message cli_progress_step cli_alert_success cli_alert_danger cli_alert_info
#' @importFrom stringr str_split_i
#'
#' @param covariates Character vector of covariate names to retrieve (e.g., `"Sand"`, `"pH"`, `"SOC"`).
#' @param window_size Integer. Width of the Savitzky-Golay smoothing window for spectral preprocessing (default = 9).
#' @param bounding_box Optional. A bounding box for spatial subsetting (not currently implemented).
#'
#' @return A tibble containing:
#' \itemize{
#'   \item{PCA-preprocessed MIR spectra (SNV-SG0, 2 cm⁻¹ interval, 600–4000 cm⁻¹)}
#'   \item{Covariate measurements for each sample}
#' }
#'
#' @details
#' The function workflow includes:
#' \enumerate{
#'   \item{Reading the OSSL variable dictionary for analyte selection.}
#'   \item{Downloading topsoil metadata and filtering for U.S. sites (optional spatial subsetting to come).}
#'   \item{Downloading laboratory measurements and reshaping to analyte-wide format.}
#'   \item{Loading raw OSSL MIR spectra, smoothing with Savitzky-Golay, applying SNV.}
#'   \item{Caching processed datasets locally for faster reuse.}
#' }
#'
#' If a suitable cached dataset already exists (i.e., contains the requested covariates), it will be reused automatically.
#'
#' @seealso
#' \code{\link{predict_covariates}}, \code{\link{create_input_data}}
#'
#' @examples
#' \dontrun{
#' # Example: Download and preprocess OSSL data for Sand and pH
#' ossl_data <- download_ossl_data(covariates = c("Sand", "pH"))
#'
#' glimpse(ossl_data)
#' }
#'
#' @keywords internal

download_ossl_data <- function(covariates,
                               window_size = 9,
                               bounding_box = NA) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Read the OSSL data dictionary
  ## ---------------------------------------------------------------------------

  cli::cli_progress_message("Reading OSSL dictionary")

  dict_path <- "../../2_Data/1_Input_Data/OSSL_Data_Dictionary.csv"

  if (!file.exists(dict_path)) {

    stop("Data dictionary file not found: ", dict_path)

  }

  readr::read_csv(file           = dict_path,
                  show_col_types = FALSE) %>%
    dplyr::filter(Sam_Include == TRUE) -> OSSL_Data_Dictionary

  ## ---------------------------------------------------------------------------
  ## Step 2: Get location metadata and subset to US topsoils
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Downloading location metadata")

  qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_soilsite_L0_v1.2.qs") %>%
    dplyr::filter(dataset.code_ascii_txt == "KSSL.SSL") %>%
    dplyr::select(Layer_ID  = id.layer_uuid_txt,
                  Longitude = longitude.point_wgs84_dd,
                  Latitude  = latitude.point_wgs84_dd,
                  Top_Depth = layer.upper.depth_usda_cm) %>%
    dplyr::filter(Top_Depth == 0) -> location_data

  ## TODO: Apply bounding_box spatial filter here if implemented

  ## ---------------------------------------------------------------------------
  ## Step 3: Get and reshape lab measurement data
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Downloading and joining lab measurements")

  lab_data <- qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_soillab_L1_v1.2.qs") %>%
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
                       values_from = Measured_Value)

  ## ---------------------------------------------------------------------------
  ## Step 4: Load OSSL MIR spectra from local cache
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Loading OSSL MIR spectra")

  ## ---------------------------------------------------------------------------
  ## Step 4.1: Cache and reuse preprocessed data
  ## ---------------------------------------------------------------------------

  cache_path <- here::here("inst/extdata", "ossl_mir_processed.qs")

  if (file.exists(cache_path)) {

    temp_file <- qs::qread(cache_path)

    if(all(covariates %in% colnames(temp_file))){

      cli::cli_progress_step("Loaded cached OSSL MIR data.")

      return(qs::qread(cache_path))

    }

    cli::cli_alert_danger("Cached OSSL MIR data does not contain requested covariates. Reprocessing now.")

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Cache and reuse preprocessed data
  ## ---------------------------------------------------------------------------

  # Define a safe getter for the cached path

  get_ossl_mir_path <- function() {

    path <- getOption("ossl_mir_path", "~/soilspec_data/ossl_mir_L0_v1.2.qs")

    if (!file.exists(path)) {

      cli::cli_alert_danger("OSSL MIR cache not found at {.path {path}}.")
      cli::cli_alert_info("Please download it from: {.url https://storage.googleapis.com/soilspec4gg-public/ossl_mir_L0_v1.2.qs}")
      stop("Cannot continue without local OSSL MIR data.")
    }

    return(path)

   }

  ossl_mir_raw <- qs::qread(get_ossl_mir_path())

  ossl_mir_raw %>%
    dplyr::rename(Layer_ID = id.layer_uuid_txt) %>%
    dplyr::filter(scan.mir.model.name_utf8_txt == "Bruker Vertex 70 with HTS-XT accessory",
                  Layer_ID %in% location_data$Layer_ID) %>%
    dplyr::select(Layer_ID,
                  dplyr::starts_with("scan_mir.")) %>%
    tidyr::pivot_longer(cols      = -Layer_ID,
                        names_to  = "Wavenumber",
                        values_to = "Absorbance") %>%
    dplyr::mutate(Wavenumber = stringr::str_split_i(Wavenumber, "\\.", 2),
                  Wavenumber = stringr::str_split_i(Wavenumber, "_", 1)) -> raw_mir


  cli::cli_progress_step("Processing OSSL MIR Spectra")

  ## Split by sample

  mir_split <- split(raw_mir, raw_mir$Layer_ID)

  ## Parallel smoothing + SNV

  future::plan("multisession", workers = parallel::detectCores(logical = TRUE) - 1)

  progressr::handlers("txtprogressbar")

  progressr::with_progress({

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
  }) -> processed

  cli::cli_progress_step("OSSL MIR data processed.")

  future::plan("sequential")

  ## Join lab + spectra
  dplyr::inner_join(lab_data, processed, by = "Layer_ID") %>%
    dplyr::select(-Layer_ID) %>%
    dplyr::mutate(Sample_Index = dplyr::row_number(),
                  .before = everything()) -> OSSL_Data


  ## ---------------------------------------------------------------------------
  ## Step 5: Return final dataset
  ## ---------------------------------------------------------------------------

  cache_path <- system.file("extdata", "ossl_mir_processed.qs", package = "samssoilspecstack")

  if (cache_path == "") {
    cache_path <- file.path("inst", "extdata", "ossl_mir_processed.qs")
  }

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  qs::qsave(OSSL_Data, cache_path)
  cli::cli_alert_success("Saved preprocessed OSSL spectra to cache.")

  return(OSSL_Data)

}

