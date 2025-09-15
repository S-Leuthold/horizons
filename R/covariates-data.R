#' OSSL Data Integration and Property Mapping
#'
#' @description
#' Functions for downloading, processing, and mapping OSSL (Open Soil Spectroscopy Library)
#' data for soil covariate prediction. Handles the complex OSSL data structure and provides
#' clean, standardized property mappings for the horizons covariate prediction system.
#'
#' @importFrom dplyr filter select mutate rename inner_join left_join distinct across all_of slice_sample where bind_cols
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom qs qread_url qread qsave
#' @importFrom prospectr savitzkyGolay standardNormalVariate
#' @importFrom FactoMineR PCA
#' @importFrom stats predict
#' @importFrom tools R_user_dir
#' @importFrom utils menu
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success cli_alert_warning cli_progress_step cli_abort
#' @importFrom glue glue
#' @keywords internal

## -----------------------------------------------------------------------------
## 1. Property Mapping Functions
## -----------------------------------------------------------------------------

#' Get OSSL Property Mapping
#'
#' @description
#' Returns the standardized mapping from OSSL variable names to horizons property names.
#' This replaces the previous sysdata.rda approach with a transparent, documented mapping.
#'
#' @return A tibble with columns:
#'   - property: Standard horizons property name (e.g., "clay", "ph", "oc")
#'   - analyte: Full OSSL analyte description
#'   - ossl_name: OSSL internal variable name
#'   - target_unit: Expected units
#'   - description: Human-readable description
#'
#' @keywords internal
get_ossl_property_mapping <- function() {

  tibble::tibble(
                 ## horizons names ---------------------------------------------
                 property = c("clay",
                              "sand",
                              "silt",
                              "ph",
                              "oc",
                              "cec",
                              "bulk_density",
                              "total_nitrogen",
                              "carbonate",
                              "phosphorus",
                              "potassium",
                              "calcium",
                              "magnesium",
                              "sodium",
                              "aluminum_crystalline",
                              "iron_amorphous"),
                 ## OSSL names -------------------------------------------------
                 analyte = c("Clay, iso 11277",
                             "Sand, Total, iso 11277",
                             "Silt, Total, iso 11277",
                             "pH, 1:1 Soil-Water Suspension, iso 10390",
                             "Organic_Carbon, iso 10694",
                             "CEC, pH 7.0, iso 11260",
                             "Bulk Density, iso 11272",
                             "Nitrogen, Total NCS, iso 11261",
                             "Carbonate, iso 10693",
                             "Phosphorus, Extractable, Mehlich3",
                             "Potassium, Extractable, NH4OAc, 2M KCl displacement",
                             "Calcium, Extractable, NH4OAc",
                             "Magnesium, Extractable, NH4OAc, 2M KCl displacement",
                             "Sodium, Extractable, NH4OAc, 2M KCl displacement",
                             "Aluminum, Crystalline, Total Pedogenic Iron",
                             "Iron, Amorphous, Total Non-Crystalline Iron"),
                 ## Description ------------------------------------------------
                 description = c("Clay content by particle size analysis",
                                 "Sand content by particle size analysis",
                                 "Silt content by particle size analysis",
                                 "Soil pH in water suspension",
                                 "Organic carbon content",
                                 "Cation exchange capacity at pH 7.0",
                                 "Soil bulk density",
                                 "Total nitrogen content",
                                 "Carbonate content",
                                 "Plant-available phosphorus (Mehlich-3)",
                                 "Exchangeable potassium",
                                 "Exchangeable calcium",
                                 "Exchangeable magnesium",
                                 "Exchangeable sodium",
                                 "Crystalline aluminum content",
                                 "Amorphous iron content"),
                 ## Units ------------------------------------------------------
                 target_unit = c("g/kg",
                                 "g/kg",
                                 "g/kg",
                                 "unitless",
                                 "g/kg",
                                 "cmolc/kg",
                                 "g/cm³",
                                 "g/kg",
                                 "g/kg",
                                 "mg/kg",
                                 "cmolc/kg",
                                 "cmolc/kg",
                                 "cmolc/kg",
                                 "cmolc/kg",
                                 "% mass",
                                 "% mass"),
                 ## OSSL internal names ----------------------------------------
                 ossl_name_level1 = c("clay.tot_usda.a334_w.pct",
                                      "sand.tot_usda.c60_w.pct",
                                      "silt.tot_usda.c62_w.pct",
                                      "ph.h2o_usda.a268_index",
                                      "oc_usda.c729_w.pct",
                                      "cec_usda.a723_cmolc.kg",
                                      "bd_usda.a4_g.cm3",
                                      "n.tot_usda.a623_w.pct",
                                      "caco3_usda.a54_w.pct",
                                      "p.ext_usda.a1070_mg.kg",
                                      "k.ext_usda.a725_cmolc.kg",
                                      "ca.ext_usda.a722_cmolc.kg",
                                      "mg.ext_usda.a724_cmolc.kg",
                                      "na.ext_usda.a726_cmolc.kg",
                                      "al.dith_usda.a65_w.pct",
                                      "fe.ox_usda.a60_w.pct"))
}

#' Validate Soil Property Names
#'
#' @description
#' Checks if requested soil property names are available in the OSSL mapping.
#' Provides helpful error messages for invalid property names.
#'
#' @param properties Character vector of property names to validate
#'
#' @return Logical vector indicating which properties are valid
#' @keywords internal

validate_soil_properties <- function(properties) {

  available_properties <- get_ossl_property_mapping()$property

  valid_props   <- properties %in% available_properties
  invalid_props <- properties[!valid_props]

  if (length(invalid_props) > 0) {

      cli::cli_abort("Invalid soil properties requested: {invalid_props}",
                   "i" = "Available properties: {paste(available_properties, collapse = ', ')}")

  }

  return(valid_props)
}

## -----------------------------------------------------------------------------
## 2.) Data aquisition functions
## -----------------------------------------------------------------------------

#' Get OSSL Training Data for Soil Properties
#'
#' @description
#' Downloads and processes OSSL data for the requested soil properties.
#' Provides clean, standardized training data for soil covariate prediction models.
#' Handles all OSSL complexity internally - users just specify property names.
#'
#' @param properties Character vector of soil property names (use get_available_soil_properties() to see options)
#' @param max_samples Integer. Maximum number of OSSL samples to use (for computational efficiency)
#' @param min_samples_per_property Integer. Minimum samples required per property (default: 500)
#' @param refresh Logical. Force re-download of OSSL data (default: FALSE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Tibble with columns:
#'   - Sample_ID: Unique sample identifier
#'   - Spectral columns (wavenumbers 600-4000 cm⁻¹)
#'   - Property columns: One per requested property
#'
#' @keywords internal

get_ossl_training_data <- function(properties,
                                   max_samples              = NULL,
                                   min_samples_per_property = 500,
                                   refresh                  = FALSE,
                                   verbose                  = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate input properties
  ## ---------------------------------------------------------------------------

  validate_soil_properties(properties)

  get_ossl_property_mapping() %>%
    dplyr::filter(property %in% properties) -> property_mapping

  if (verbose) {

    cli::cli_text("")
    cli::cli_text(format_tree_item("Data Loading", level = 0))

    properties_text <- paste0("Properties requested: ", paste(properties, collapse = ", "),
                             " (", length(properties), " total)")
    cli::cli_text(format_tree_item(properties_text, level = 1, is_last = FALSE))
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Setup Cache and Check for Existing Data
  ## ---------------------------------------------------------------------------

  cache_dir <- tools::R_user_dir("horizons", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  location_file <- file.path(cache_dir, "ossl_location_data.qs")
  lab_file      <- file.path(cache_dir, "ossl_lab_data.qs")
  mir_file      <- file.path(cache_dir, "ossl_mir_raw.qs")

  files_exist <- all(file.exists(c(location_file, lab_file, mir_file)))

  if (verbose) {

    cache_status <- if (files_exist) "found" else "missing"
    cache_symbol <- get_status_symbol(if (files_exist) "success" else "warning")
    cache_text <- paste0(cache_symbol, " Cache status: ", cache_status)
    cli::cli_text(format_tree_item(cache_text, level = 1, is_last = TRUE))

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Download data if cache doesn't hit
  ## ---------------------------------------------------------------------------

  ## Inform user about why we're downloading -----------------------------------

  if (!files_exist || refresh) {

    if (verbose && !refresh) {

      cli::cli_alert_warning("OSSL data not found in cache")

      } else if (verbose && refresh) {

        cli::cli_alert_info("Refreshing OSSL data (forced)")
    }

    ## Make it interactive -----------------------------------------------------

    utils::menu(c("Yes, download OSSL data (~1-2GB)", "No, cancel"),
                title = "OSSL data download required. Continue?") -> confirm_download

    if (confirm_download != 1) {

      cli::cli_abort("User cancelled OSSL data download")

    }

    ## Download each file safely -----------------------------------------------

    list(location = list(url  = "https://storage.googleapis.com/soilspec4gg-public/ossl_soilsite_L0_v1.2.qs",
                         file = location_file,
                         desc = "OSSL location metadata"),
         lab      = list(url  = "https://storage.googleapis.com/soilspec4gg-public/ossl_soillab_L1_v1.2.qs",
                         file = lab_file,
                         desc = "OSSL lab measurements"),
         mir      = list(url  = "https://storage.googleapis.com/soilspec4gg-public/ossl_mir_L0_v1.2.qs",
                         file = mir_file,
                         desc = "OSSL raw MIR spectra")) -> downloads

    for (download_info in downloads) {

      if (verbose) cli::cli_progress_step("Downloading {download_info$desc}...")

      safely_execute(expr = {data <- qs::qread_url(download_info$url)
                             qs::qsave(data, download_info$file)
                             data},
                     default_value      = NULL,
                     error_message      = "Failed to download {download_info$desc}",
                     capture_conditions = TRUE) -> download_result

      ## -----------------------------------------------------------------------

      handle_results(safe_result   = download_result,
                     error_title   = "Failed to download {download_info$desc} from {download_info$url}",
                     error_hints   = c("Check internet connection",
                                       "Verify URL is accessible: {download_info$url}",
                                       "OSSL server may be temporarily unavailable"),
                     abort_on_null = TRUE,
                     silent        = FALSE) -> download_result

      if (verbose) cli::cli_alert_success("Cached {download_info$desc}")

    }

    if (verbose) cli::cli_alert_success("All OSSL data downloaded and cached")

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Load OSSL Data from Cache
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cli::cli_text(format_tree_item("Data Processing", level = 0))
    cli::cli_text(format_tree_item("⟳ Loading OSSL data from cache...", level = 1, is_last = FALSE, symbol = NULL))

  }

  # Load the three OSSL components ---------------------------------------------

  location_data <- qs::qread(location_file)
  lab_data      <- qs::qread(lab_file)
  mir_data      <- qs::qread(mir_file)

  if (is.null(location_data) || is.null(lab_data) || is.null(mir_data)) {

    cli::cli_abort("Failed to load OSSL data from cache")

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Process Lab Data for Requested Properties
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text(format_tree_item(paste0("⟳ Processing lab data for ", length(properties), " properties..."),
                                              level   = 1,
                                              is_last = FALSE,
                                              symbol  = NULL))


  # Get OSSL variable names for requested properties ---------------------------

  ossl_variables <- property_mapping$ossl_name_level1

  # Process lab data -----------------------------------------------------------

  safely_execute(expr = {lab_data %>%
                          dplyr::distinct() %>%
                          dplyr::rename(Sample_ID = id.layer_uuid_txt) %>%
                          dplyr::filter(Sample_ID %in% location_data$id.layer_uuid_txt) %>%
                          dplyr::select(Sample_ID, dplyr::all_of(ossl_variables)) %>%
                          tidyr::pivot_longer(cols      = -Sample_ID,
                                              names_to  = "ossl_variable",
                                              values_to = "measured_value") %>%
                          dplyr::left_join(property_mapping %>%
                                             dplyr::select(ossl_variable = ossl_name_level1, property),
                                           by = "ossl_variable") %>%
                          tidyr::drop_na(measured_value) %>%
                          dplyr::select(Sample_ID,
                                        property,
                                        measured_value) %>%
                          tidyr::pivot_wider(names_from  = property,
                                             values_from = measured_value)},
                 default_value      = NULL,
                 error_message      = "Lab data processing failed",
                 capture_conditions = TRUE)  -> processed_lab_safe

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = processed_lab_safe,
                 error_title   = "Failed to process OSSL lab measurements",
                 error_hints   = c("Check that required OSSL variables exist in lab_data",
                                   "Verify property_mapping contains all necessary mappings",
                                   "Ensure Sample_ID column exists in both datasets"),
                 abort_on_null = TRUE,
                 silent        = FALSE) -> processed_lab

  ## ---------------------------------------------------------------------------
  ## Step 6: Clean up and standardize MIR Spectra
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text(format_tree_item("⟳ Processing MIR spectra...",
                                              level   = 1,
                                              is_last = FALSE,
                                              symbol  = NULL))


  ## Apply a standardized preproccesing to match incoming data -----------------

  safely_execute(expr = {mir_data %>%
                          dplyr::rename(Sample_ID = id.layer_uuid_txt) %>%
                          dplyr::filter(Sample_ID %in% processed_lab$Sample_ID) -> mir_clean

                        ## Identify spectral columns (scan_mir.XXX_abs) --------

                        names(mir_clean)[grepl("^scan_mir\\.[0-9]{3,4}_abs$", names(mir_clean))] -> spectral_cols

                        ## Abort if no spectral columns found ------------------

                        if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns found in MIR data")

                        ## Extract spectral columns ----------------------------

                        mir_clean %>%
                          dplyr::select(dplyr::all_of(spectral_cols)) %>%
                          as.matrix() -> spectral_matrix

                        ## Standardize column names: scan_mir.608_abs -> 608 ---

                        wavenumbers <- gsub("^scan_mir\\.([0-9]{3,4})_abs$", "\\1", spectral_cols)
                        colnames(spectral_matrix) <- wavenumbers

                        ## Combine back with Sample_ID -------------------------

                        tibble::tibble(Sample_ID = mir_clean$Sample_ID) %>%
                          dplyr::bind_cols(as.data.frame(spectral_matrix))},

                 default_value      = NULL,
                 error_message      = "MIR spectra processing failed",
                 capture_conditions = TRUE) -> processed_mir_safe

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = processed_mir_safe,
                 error_title   = "Failed to process OSSL MIR spectra",
                 error_hints   = c("Check MIR data contains scan_mir.XXX_abs columns",
                                   "Verify Sample_ID matches between lab and MIR data",
                                   "Ensure spectral columns contain numeric values only"),
                 abort_on_null = TRUE,
                 silent        = FALSE) -> processed_mir

  ## ---------------------------------------------------------------------------
  ## Step 7: Join MIR, lab, and location data, clean up
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text(format_tree_item("⟳ Joining spectral and lab data...",
                                              level   = 1,
                                              is_last = FALSE,
                                              symbol  = NULL))

  safely_execute(expr = {
                         ## Join the MIR and the lab data ----------------------

                         dplyr::inner_join(processed_lab,
                                           processed_mir,
                                           by = "Sample_ID") %>%

                          ## Join the sample location data ---------------------

                          dplyr::inner_join(location_data %>%
                                              dplyr::rename(Sample_ID = id.layer_uuid_txt) %>%

                                              ## Restrict to just topsoil ------

                                              dplyr::filter(layer.upper.depth_usda_cm == 0) %>%
                                              dplyr::select(Sample_ID),
                                            by = "Sample_ID") %>%
                          tidyr::drop_na(dplyr::all_of(properties)) %>%

                          ## Limit to max_samples if specified --------------

                          {if (!is.null(max_samples) && nrow(.) > max_samples) {

                            dplyr::slice_sample(., n = max_samples)

                          } else {

                            .
                          }}},
      default_value      = NULL,
      error_message      = "Data joining failed",
      capture_conditions = TRUE) -> final_data_safe

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = final_data_safe,
                 error_title   = "Failed to create joined OSSL dataset",
                 error_hints   = c("Check that Sample_ID columns match across datasets",
                                   "Verify location_data contains layer.upper.depth_usda_cm column",
                                   "Ensure requested properties exist in processed_lab"),
                 abort_on_null = TRUE,
                 silent        = FALSE) -> final_data

  ## Check minimum sample requirements -----------------------------------------

  for (prop in properties) {

    n_samples <- sum(!is.na(final_data[[prop]]))

    if (n_samples < min_samples_per_property) {

      cli::cli_warn("Property '{prop}' has only {n_samples} samples (minimum {min_samples_per_property} required)")

    }
  }

  ## Inform user about completion stats ----------------------------------------

  if (verbose) {

    cli::cli_text("")

    completion_text <- paste0(get_status_symbol("complete"), " OSSL data prepared: ",
                             format_metric(nrow(final_data), "count"), " samples")

    cli::cli_text(format_tree_item(completion_text,
                                   level = 1,
                                   is_last = FALSE))

    for (i in seq_along(properties)) {

      prop      <- properties[i]
      n_valid   <- sum(!is.na(final_data[[prop]]))
      prop_text <- paste0(prop, ": ", format_metric(n_valid, "count"), " samples")
      is_last   <- (i == length(properties))

      cli::cli_text(format_tree_item(prop_text, level = 2, is_last = is_last))

    }

  }

  return(final_data)
}

## -----------------------------------------------------------------------------
## Spectral Preprocessing Functions
## -----------------------------------------------------------------------------

#' Preprocess MIR Spectra for Analysis
#'
#' @description
#' Applies Savitzky-Golay smoothing and Standard Normal Variate normalization
#' to MIR spectra. This preprocessing pipeline follows established conventions
#' from the ring trial paper from Sanfanelli et al.
#'
#' @param spectral_data Tibble containing numeric spectral columns
#' @param smooth_window Integer. Window size for Savitzky-Golay filter (default: 9)
#' @param smooth_poly Integer. Polynomial order for smoothing (default: 1)
#' @param verbose Logical. Print progress messages
#'
#' @return Tibble with preprocessed spectra (same column structure)
#' @keywords internal

preprocess_mir_spectra <- function(spectral_data,
                                   smooth_window = 9,
                                   smooth_poly   = 1,
                                   verbose       = TRUE) {

  ## TODO: Might need to tree these messages as well.

  if (verbose) cli::cli_progress_step("Preprocessing MIR spectra (SG smoothing + SNV)")

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate and quality check input data
  ## ---------------------------------------------------------------------------

  # Extract spectral columns (numeric wavenumbers) -----------------------------

  spectral_cols     <- grep("^[0-9]{3,4}$", names(spectral_data), value = TRUE)
  non_spectral_cols <- setdiff(names(spectral_data), spectral_cols)

  if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns found (expected numeric wavenumber columns)")

  ## Extract spectral matrix ---------------------------------------------------

  spectra_matrix <- as.matrix(spectral_data[spectral_cols])

  ## QAQC: Check for negative spectra ------------------------------------------

  n_negative <- sum(spectra_matrix < 0, na.rm = TRUE)

  if (n_negative > 0) cli::cli_warn("Found {n_negative} negative spectral values - may indicate measurement issues")

  ## QAQC: Check for all 0 or constant spectra ---------------------------------

  row_sds    <- apply(spectra_matrix, 1, sd, na.rm = TRUE)
  n_constant <- sum(row_sds < 1e-6, na.rm = TRUE)

  if (n_constant > 0) cli::cli_warn("Found {n_constant} constant spectra - may indicate measurement failures")

  ## ---------------------------------------------------------------------------
  ## Step 2: Apply Savitzky-Golay smoothing and SNV normalization
  ## ---------------------------------------------------------------------------

  ## First, the SVG smoothing --------------------------------------------------

  tryCatch({

    prospectr::savitzkyGolay(X = spectra_matrix,
                             m = 0,
                             p = smooth_poly,
                             w = smooth_window)
    }, error = function(e) {

      cli::cli_warn("Failed to apply Savitzky-Golay smoothing: {e$message}")
      return(NULL)

    }
  ) -> smoothed

  if (is.null(smoothed)) return(NULL)

  ## Then the SNV normalization ------------------------------------------------

  tryCatch({

    prospectr::standardNormalVariate(X = smoothed)

    }, error = function(e) {

      cli::cli_warn("Failed to apply SNV normalization: {e$message}")
      return(NULL)

    }
  ) -> processed_spectra

  if (is.null(processed_spectra)) return(NULL)

  ## ---------------------------------------------------------------------------
  ## Step 3: Reconstruct tibble with processed spectra
  ## ---------------------------------------------------------------------------

  ## Make the tibble -----------------------------------------------------------

  processed_spectra           <- tibble::as_tibble(processed_spectra)
  names(processed_spectra) <- spectral_cols

  ## Bind with non-spectral columns --------------------------------------------

  if (length(non_spectral_cols) > 0) {

    result <- dplyr::bind_cols(spectral_data[non_spectral_cols],
                               processed_spectra)
    } else {

    result <- processed_spectra

  }

  ## Return results ------------------------------------------------------------

  return(result)
}

#' Perform PCA on OSSL Training Data
#'
#' @description
#' Performs Principal Component Analysis on preprocessed OSSL MIR spectra,
#' retaining components based on variance threshold for downstream similarity
#' calculations. Uses stats::prcomp for better performance and memory efficiency.
#'
#' @param ossl_data Tibble containing preprocessed OSSL data with spectra and properties
#' @param n_components Integer. Number of PCA components to retain (default: 50)
#' @param verbose Logical. Print progress messages
#'
#' @return Named list containing:
#'   - pca_model: prcomp PCA object for projecting new data
#'   - ossl_pca_scores: Tibble with PCA scores and metadata
#'   - variance_explained: Numeric vector showing variance explained by each component
#' @keywords internal

perform_pca_on_ossl <- function(ossl_data,
                               variance_threshold = 0.95,
                               verbose            = TRUE) {

  if (verbose) cli::cli_progress_step("Performing PCA on OSSL training data")

  ## ---------------------------------------------------------------------------
  ## Step 0: Setup
  ## ---------------------------------------------------------------------------

  ## Extract spectral columns --------------------------------------------------

  spectral_cols <- grep("^[0-9]{3,4}$", names(ossl_data), value = TRUE)

  if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns found in OSSL data (expected numeric wavenumber columns)")

  ## Drop missing values and warn the user -------------------------------------

  ossl_data %>%
    dplyr::select(Sample_ID,
                  dplyr::all_of(spectral_cols)) %>%
    tidyr::drop_na(dplyr::all_of(spectral_cols)) -> ossl_clean

  if(nrow(ossl_clean) < nrow(ossl_data)) {

    n_dropped <- nrow(ossl_data) - nrow(ossl_clean)

    cli::cli_warn("{n_dropped} samples with missing spectral data were dropped before PCA")

  }

  ## Create a fully numeric matrix with no NAs ---------------------------------

  spectral_matrix <- dplyr::select(ossl_clean, dplyr::all_of(spectral_cols))

  ## ---------------------------------------------------------------------------
  ## Step 1: Perform PCA using prcomp
  ## ---------------------------------------------------------------------------

  ## Do the analysis -----------------------------------------------------------

  safely_execute(expr = {stats::prcomp(x      = spectral_matrix,
                                       center = FALSE,
                                       scale. = FALSE)},
                 default_value      = NULL,
                 error_message      = "PCA computation failed",
                 capture_conditions = TRUE) -> pca_safe

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = pca_safe,
                 error_title   = "Failed to perform PCA on OSSL spectral data",
                 error_hints   = c("Check that spectral matrix has sufficient variance",
                                   "Ensure matrix has more rows than columns",
                                   "Verify data contains no all-zero rows or columns",
                                   "Consider reducing spectral resolution if memory issues"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> pca_model

  ## ---------------------------------------------------------------------------

  if (is.null(pca_model)) return(NULL)

  ## Calculate variance explained ----------------------------------------------

  variance_explained <- pca_model$sdev^2 / sum(pca_model$sdev^2)
  cum_variance       <- cumsum(variance_explained)
  n_components       <- which(cum_variance >= variance_threshold)[1]

  ## Report variance stats -----------------------------------------------------

  if (verbose) {

    pca_text <- paste0(get_status_symbol("success"), " PCA: ", n_components,
                      " components (", round(variance_threshold * 100), "% variance)")

    cli::cli_text(format_tree_item(text    = pca_text,
                                   level   = 1,
                                   is_last = TRUE))

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract and return PC scores, along with model.
  ## ---------------------------------------------------------------------------

  pca_scores <- pca_model$x[, 1:n_components]

  ## Convert to tibble and fix the colnames ------------------------------------

  pca_scores        <- tibble::as_tibble(pca_scores)
  names(pca_scores) <- paste0("Dim.", 1:n_components)

  ## Add metadata columns (use cleaned data to maintain row alignment) ---------

  metadata_cols <- setdiff(names(ossl_data), spectral_cols)

  if (length(metadata_cols) > 0) {

    ossl_data %>%
      dplyr::semi_join(ossl_clean, by = "Sample_ID") %>%
      dplyr::select(dplyr::all_of(metadata_cols)) -> ossl_metadata

    ossl_pca_scores <- dplyr::bind_cols(ossl_metadata, pca_scores)

  } else {

    ossl_pca_scores <- pca_scores

  }

  ## Calculate total variance explained by selected components -----------------
  total_variance <- cum_variance[n_components]

  if (verbose) {
    cli::cli_alert_success(
      "PCA complete: {n_components} components explain {round(total_variance * 100, 1)}% of variance"
    )
  }

  ## Store the selected number of components in the model for later use --------
  pca_model$n_components <- n_components

  ## Return results ------------------------------------------------------------

  return(list(pca_model          = pca_model,
              ossl_pca_scores    = ossl_pca_scores,
              variance_explained = variance_explained[1:n_components],
              n_components       = n_components))
}

#' Project New Spectra to PCA Space
#'
#' @description
#' Projects new MIR spectra into the same PCA space as the OSSL training data.
#' Ensures dimensional consistency for similarity calculations.
#'
#' @param new_data Tibble with new spectral data (must have same columns as OSSL)
#' @param pca_model prcomp PCA object from perform_pca_on_ossl()
#' @param verbose Logical. Print progress messages
#'
#' @return Tibble with PCA scores and preserved metadata
#' @keywords internal


project_spectra_to_pca <- function(new_data,
                                   pca_model,
                                   verbose = TRUE) {

  if (verbose) cli::cli_progress_step("Projecting new spectra to PCA space")

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate inputs
  ## ---------------------------------------------------------------------------

  ## Extract spectral columns (numeric wavenumbers) ----------------------------

  spectral_cols <- grep("^[0-9]{3,4}$", names(new_data), value = TRUE)

  if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns found in new data (expected numeric wavenumber columns)")


  pca_cols     <- rownames(pca_model$rotation)
  missing_cols <- setdiff(pca_cols, spectral_cols)
  extra_cols   <- setdiff(spectral_cols, pca_cols)

  if (length(missing_cols) > 0) {
    if (verbose) {
      cli::cli_alert_warning("Missing {length(missing_cols)} columns required by PCA model")
      if (length(missing_cols) <= 10) {
        cli::cli_alert_info("Missing columns: {paste(head(missing_cols, 10), collapse = ', ')}")
      }
    }
  }

  if (length(extra_cols) > 0) {
    if (verbose) {
      cli::cli_alert_warning("Found {length(extra_cols)} extra columns not in PCA model")
    }
  }

  # Create data with matching columns, filling missing with zeros
  newdata_aligned           <- matrix(0, nrow = nrow(new_data), ncol = length(pca_cols))
  colnames(newdata_aligned) <- pca_cols
  common_cols               <- intersect(spectral_cols, pca_cols)

  if (length(common_cols) == 0) {
    cli::cli_abort("No matching columns between new data and PCA model")
  }

  newdata_aligned[, common_cols] <- as.matrix(new_data[, common_cols])

  safely_execute(
    expr = {
      stats::predict(object = pca_model,
                    newdata = newdata_aligned)
    },
    default_value = NULL,
    error_message = "Failed to project new data to PCA space"
  ) -> projection_safe

  if (is.null(projection_safe$result)) {
    return(NULL)
  }

  # Extract the required number of components and format as tibble
  n_components <- pca_model$n_components
  if (is.null(n_components)) {
    # Fallback if n_components wasn't stored
    n_components <- ncol(projection_safe$result)
  }

  projected_scores <- projection_safe$result[, 1:min(n_components, ncol(projection_safe$result))]

  # Convert to tibble with proper column names
  new_pca_scores <- tibble::as_tibble(projected_scores)
  names(new_pca_scores) <- paste0("Dim.", 1:ncol(new_pca_scores))

  metadata_cols <- setdiff(names(new_data), spectral_cols)
  if (length(metadata_cols) > 0) {
    result <- dplyr::bind_cols(
      dplyr::select(new_data, dplyr::all_of(metadata_cols)),
      new_pca_scores
    )
  } else {
    result <- new_pca_scores
  }

  return(result)
}

#' Enhanced OSSL Training Data with Preprocessing
#'
#' @description
#' Extended version of get_ossl_training_data that includes spectral preprocessing
#' and PCA transformation. Returns ready-to-use training data for similarity-based
#' local modeling.
#'
#' @param properties Character vector of soil properties to include
#' @param max_samples Integer. Maximum number of samples to return (for testing)
#' @param n_components Integer. Number of PCA components to retain (default: 50)
#' @param refresh Logical. Force refresh of cached data
#' @param verbose Logical. Print progress messages
#'
#' @return Named list containing:
#'   - raw_data: Original OSSL data with properties
#'   - processed_data: Preprocessed spectral data
#'   - pca_model: PCA transformation model
#'   - pca_scores: PCA-transformed OSSL data ready for similarity matching
#' @keywords internal
get_processed_ossl_training_data <- function(properties,
                                           max_samples = NULL,
                                           variance_threshold = 0.95,
                                           refresh = FALSE,
                                           verbose = TRUE) {

  if (verbose) {
    cli::cli_h2("Enhanced OSSL Training Data Pipeline")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Get basic OSSL data
  ## ---------------------------------------------------------------------------

  ossl_raw <- get_ossl_training_data(
    properties = properties,
    max_samples = max_samples,
    refresh = refresh,
    verbose = verbose
  )

  if (is.null(ossl_raw)) {
    return(NULL)
  }



  ## ---------------------------------------------------------------------------
  ## Step 3: Preprocess spectra
  ## ---------------------------------------------------------------------------

  # Apply preprocessing to OSSL data (columns already standardized to numeric)
  ossl_processed <- preprocess_mir_spectra(
    spectral_data = ossl_raw,
    verbose = verbose
  )

  if (is.null(ossl_processed)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Perform PCA
  ## ---------------------------------------------------------------------------

  pca_result <- perform_pca_on_ossl(
    ossl_data = ossl_processed,
    variance_threshold = variance_threshold,
    verbose = verbose
  )

  if (is.null(pca_result)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Return complete pipeline results
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text(format_tree_item("✓ Enhanced OSSL pipeline complete", level = 1, is_last = FALSE))
    cli::cli_text(format_tree_item("Ready for similarity-based local modeling", level = 1, is_last = TRUE))
  }

  return(list(
    raw_data = ossl_raw,
    processed_data = ossl_processed,
    pca_model = pca_result$pca_model,
    pca_scores = pca_result$ossl_pca_scores,
    variance_explained = pca_result$variance_explained
  ))
}
