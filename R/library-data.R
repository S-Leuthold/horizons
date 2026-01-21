#' Library-Based Prediction Data Infrastructure
#'
#' @description
#' Functions for loading, preprocessing, and transforming reference spectral libraries
#' (OSSL/KSSL) for library-based prediction mode. Provides standardized property mappings,
#' method harmonization, and PCA-based dimensionality reduction for clustering and modeling.
#'
#' @details
#' This module supports the Library Prediction Service, enabling predictions for standard
#' soil properties without requiring user-provided training data. The library is preprocessed
#' once, clustered, and used to train property-specific models at prediction time.
#'
#' @importFrom cli cli_text cli_alert_info cli_alert_success cli_alert_warning cli_abort style_bold
#' @importFrom dplyr filter select mutate rename bind_cols slice_sample
#' @importFrom tibble tibble as_tibble
#' @importFrom qs qread qread_url qsave
#' @importFrom prospectr savitzkyGolay standardNormalVariate
#' @importFrom FactoMineR PCA
#' @importFrom stats predict
#' @importFrom tools R_user_dir
#' @keywords internal

## -----------------------------------------------------------------------------
## Section 1: Property Mapping and Validation
## -----------------------------------------------------------------------------

#' Get Library Property Mapping
#'
#' @description
#' Returns standardized mapping from horizons property names to OSSL variable names
#' for the 15 properties supported in library-based prediction mode. Includes
#' laboratory method metadata for method harmonization.
#'
#' @details
#' This function provides the canonical mapping between horizons standard property
#' names and OSSL internal variable names. The mapping is based on OSSL v1.2 schema
#' and includes method metadata for filtering to single laboratory procedures.
#'
#' Properties were selected based on Ng et al. 2022 benchmarking study showing
#' Category A/B prediction accuracy (R² > 0.70) with MIR spectroscopy.
#'
#' @return A tibble with 15 rows and 6 columns:
#' \describe{
#'   \item{property}{Horizons standard name (e.g., "clay", "ph", "total_carbon")}
#'   \item{analyte}{Full OSSL analyte description with ISO method}
#'   \item{ossl_name}{OSSL internal variable name for data extraction}
#'   \item{lab_method}{Laboratory method/extraction procedure used}
#'   \item{target_unit}{Expected units for the property}
#'   \item{description}{Human-readable description}
#' }
#'
#' @section Properties Supported:
#' - **Texture** (3): clay, sand, silt (pipette method, iso 11277)
#' - **Carbon** (3): total_carbon, oc, carbonate (combustion/acid methods)
#' - **Nitrogen** (1): total_nitrogen (combustion, iso 11261)
#' - **Chemistry** (2): ph (1:1 H2O, iso 10390), cec (NH4OAc pH 7.0, iso 11260)
#' - **Base Cations** (4): calcium, magnesium, potassium, sodium (NH4OAc extraction)
#' - **Major Elements** (2): iron_total, aluminum_total (dithionite extraction)
#'
#' @references
#' Ng et al. 2022. Mid-infrared spectroscopy for accurate measurement of an extensive
#' set of soil properties. Soil Security 6:100043.
#'
#' @seealso [LIBRARY_PROPERTIES], [load_ossl_raw()]
#'
#' @examples
#' \dontrun{
#' # Get mapping table
#' mapping <- horizons:::get_library_property_mapping()
#'
#' # Find OSSL column for a property
#' clay_col <- mapping$ossl_name[mapping$property == "clay"]
#'
#' # Check lab method
#' ph_method <- mapping$lab_method[mapping$property == "ph"]
#' }
#'
#' @keywords internal
get_library_property_mapping <- function() {

  tibble::tibble(
    ## Horizons names ---------------------------------------------------------
    property = c(
      ## Texture
      "clay",
      "sand",
      "silt",

      ## Carbon forms
      "total_carbon",
      "oc",
      "carbonate",

      ## Nitrogen
      "total_nitrogen",

      ## pH and CEC
      "ph",
      "cec",

      ## Base cations (NH4OAc extractable)
      "calcium",
      "magnesium",
      "potassium",
      "sodium",

      ## Major elements (dithionite extractable)
      "iron_total",
      "aluminum_total"
    ),

    ## OSSL analyte descriptions ---------------------------------------------
    analyte = c(
      ## Texture
      "Clay, iso 11277",
      "Sand, Total, iso 11277",
      "Silt, Total, iso 11277",

      ## Carbon
      "Carbon, Total, usda a622",
      "Organic Carbon, iso 10694",
      "Carbonate, iso 10693",

      ## Nitrogen
      "Nitrogen, Total NCS, iso 11261",

      ## pH and CEC
      "pH, 1:1 Soil-Water Suspension, iso 10390",
      "CEC, pH 7.0, iso 11260",

      ## Base cations
      "Calcium, Extractable, NH4OAc",
      "Magnesium, Extractable, NH4OAc, 2M KCl displacement",
      "Potassium, Extractable, NH4OAc, 2M KCl displacement",
      "Sodium, Extractable, NH4OAc, 2M KCl displacement",

      ## Major elements
      "Iron, Dithionite Extractable (Pedogenic), usda a66",
      "Aluminum, Dithionite Extractable (Pedogenic), usda a65"
    ),

    ## OSSL internal variable names ------------------------------------------
    ossl_name = c(
      ## Texture
      "clay.tot_usda.a334_w.pct",
      "sand.tot_usda.c60_w.pct",
      "silt.tot_usda.c62_w.pct",

      ## Carbon
      "c.tot_usda.a622_w.pct",
      "oc_usda.c729_w.pct",
      "caco3_usda.a54_w.pct",

      ## Nitrogen
      "n.tot_usda.a623_w.pct",

      ## pH and CEC
      "ph.h2o_usda.a268_index",
      "cec_usda.a723_cmolc.kg",

      ## Base cations
      "ca.ext_usda.a722_cmolc.kg",
      "mg.ext_usda.a724_cmolc.kg",
      "k.ext_usda.a725_cmolc.kg",
      "na.ext_usda.a726_cmolc.kg",

      ## Major elements
      "fe.dith_usda.a66_w.pct",
      "al.dith_usda.a65_w.pct"
    ),

    ## Laboratory methods ----------------------------------------------------
    lab_method = c(
      ## Texture
      "Pipette method (iso 11277)",
      "Pipette method (iso 11277)",
      "Pipette method (iso 11277)",

      ## Carbon
      "Dry combustion (usda a622)",
      "Walkley-Black / Dry combustion (iso 10694)",
      "Acid neutralization (iso 10693)",

      ## Nitrogen
      "Dry combustion NCS (iso 11261)",

      ## pH and CEC
      "1:1 H2O suspension (iso 10390)",
      "NH4OAc pH 7.0 (iso 11260)",

      ## Base cations (all same method)
      "NH4OAc extraction",
      "NH4OAc extraction with KCl",
      "NH4OAc extraction with KCl",
      "NH4OAc extraction with KCl",

      ## Major elements
      "Dithionite-citrate extraction (usda a66)",
      "Dithionite-citrate extraction (usda a65)"
    ),

    ## Target units ----------------------------------------------------------
    target_unit = c(
      ## Texture
      "g/kg",
      "g/kg",
      "g/kg",

      ## Carbon
      "% wt",
      "g/kg",
      "g/kg",

      ## Nitrogen
      "g/kg",

      ## pH and CEC
      "unitless",
      "cmolc/kg",

      ## Base cations
      "cmolc/kg",
      "cmolc/kg",
      "cmolc/kg",
      "cmolc/kg",

      ## Major elements
      "% wt",
      "% wt"
    ),

    ## Descriptions ----------------------------------------------------------
    description = c(
      ## Texture
      "Clay content by particle size analysis",
      "Sand content by particle size analysis",
      "Silt content by particle size analysis",

      ## Carbon
      "Total carbon content (organic + inorganic)",
      "Organic carbon content",
      "Carbonate content (inorganic C)",

      ## Nitrogen
      "Total nitrogen content",

      ## pH and CEC
      "Soil pH in 1:1 water suspension",
      "Cation exchange capacity at pH 7.0",

      ## Base cations
      "Exchangeable calcium",
      "Exchangeable magnesium",
      "Exchangeable potassium",
      "Exchangeable sodium",

      ## Major elements
      "Total pedogenic iron (dithionite extractable)",
      "Total pedogenic aluminum (dithionite extractable)"
    )
  )
}

## -----------------------------------------------------------------------------
## Section 2: Raw OSSL Data Loading
## -----------------------------------------------------------------------------

#' Load Raw OSSL Data for Property
#'
#' @description
#' Downloads (if needed) and loads OSSL lab measurements and MIR spectra from cache,
#' joins them, and extracts data for the requested property. Raw OSSL files are cached
#' globally in the user directory and reused across properties for efficiency.
#'
#' @details
#' **Workflow:**
#' 1. Check cache for OSSL lab data (ossl_lab_data.qs)
#' 2. If not cached, download from soilspec4gg-public bucket and cache
#' 3. Same process for MIR spectra (ossl_mir_raw.qs)
#' 4. Extract property measurements from lab data (remove NA values)
#' 5. Inner join with MIR spectra on sample_id
#' 6. Rename MIR columns from scan_mir.600_abs to X600 format
#' 7. Optional: subsample for testing speed
#' 8. Clean up intermediate objects (memory discipline)
#'
#' **Caching Strategy:**
#' Raw OSSL files (~200MB each) are downloaded once and cached in:
#' `tools::R_user_dir("horizons", "cache")`
#'
#' Subsequent calls for ANY property reuse these cached files (fast).
#'
#' **Data Sources:**
#' - Lab data: https://storage.googleapis.com/soilspec4gg-public/ossl_soillab_L1_v1.2.qs
#' - MIR spectra: https://storage.googleapis.com/soilspec4gg-public/ossl_mir_L0_v1.2.qs
#'
#' @param property Character. Property name from LIBRARY_PROPERTIES.
#'   Use `LIBRARY_PROPERTIES` to see supported properties.
#' @param cache_dir Character. Cache directory path. Default: NULL (uses `tools::R_user_dir("horizons", "cache")`).
#'   Specify custom path for testing or non-standard installations.
#' @param max_samples Integer. Maximum samples to return. Default: NULL (all available).
#'   Used primarily for testing - limits result size for faster iteration.
#'   Production use should omit this parameter for full library coverage.
#' @param verbose Logical. Print tree-style progress messages? Default: TRUE.
#'   Set FALSE for batch operations or when used programmatically.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{sample_id}{OSSL sample identifier (UUID)}
#'   \item{<property_column>}{Property measurement with OSSL name (e.g., "clay.tot_usda.a334_w.pct")}
#'   \item{X600, X602, ...}{MIR spectral absorbance at each wavenumber (cm⁻¹)}
#' }
#'
#' Returns NULL if loading fails (check logs for details).
#'
#' @section Error Handling:
#' Uses `safely_execute()` + `handle_results()` pattern:
#' - Download failures return NULL with hints (check connection, server status)
#' - Cache read failures return NULL with hints (try deleting corrupt cache)
#' - Missing property columns abort with informative message
#'
#' @section Memory Management:
#' After joining, removes large intermediate objects:
#' - rm(lab_data, mir_data, lab_subset)
#' - gc() to reclaim memory
#' Overhead: ~50-100MB per property
#'
#' @examples
#' \dontrun{
#' # Load clay data
#' clay <- horizons:::load_ossl_raw("clay")
#'
#' # With custom cache (for testing)
#' oc <- horizons:::load_ossl_raw(
#'   property = "oc",
#'   cache_dir = tempdir(),
#'   max_samples = 500,
#'   verbose = TRUE
#' )
#' }
#'
#' @seealso [get_library_property_mapping()], [preprocess_library_spectra()]
#' @keywords internal
load_ossl_raw <- function(property,
                          cache_dir   = NULL,
                          max_samples = NULL,
                          verbose     = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 2.1: Validate property
  ## ---------------------------------------------------------------------------

  if (!property %in% LIBRARY_PROPERTIES) {
    cli::cli_abort("Property '{property}' not in LIBRARY_PROPERTIES",
                   "i" = "Supported: {paste(LIBRARY_PROPERTIES, collapse = ', ')}")
  }

  ## Get property metadata -------------------------------------------------

  get_library_property_mapping() %>%
    dplyr::filter(property == !!property) -> prop_mapping

  if (nrow(prop_mapping) != 1) cli::cli_abort("Property '{property}' not found in mapping")

  ossl_column <- prop_mapping$ossl_name

  ## ---------------------------------------------------------------------------
  ## Step 2.2: Setup cache directory
  ## ---------------------------------------------------------------------------

  if (is.null(cache_dir)) {

    cache_dir <- tools::R_user_dir("horizons", "cache")

  }

  ## Create cache directory if needed ------------------------------------------

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  ## Define cache file paths ---------------------------------------------------

  lab_file      <- file.path(cache_dir, "ossl_lab_data.qs")
  mir_file      <- file.path(cache_dir, "ossl_mir_raw.qs")
  location_file <- file.path(cache_dir, "ossl_location_data.qs")

  ## ---------------------------------------------------------------------------
  ## Step 2.3: Download/load OSSL lab data
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Loading OSSL lab measurements')}...")

  ## Check if cached -----------------------------------------------------------

  if (file.exists(lab_file)) {

    if (verbose) cli::cli_text("│  ├─ Found cached lab data")

    safely_execute(
      qs::qread(lab_file),
      error_message = "Failed to read cached lab data"
    ) %>%
      handle_results(
        error_title = "Cache read failed",
        error_hints = c("Cache may be corrupted", "Try deleting cache and re-downloading"),
        abort_on_null = FALSE
      ) -> lab_data

    if (is.null(lab_data)) return(NULL)

  } else {

    if (verbose) cli::cli_text("│  ├─ Downloading from OSSL server...")

    safely_execute(
      qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_soillab_L1_v1.2.qs"),
      error_message = "Failed to download OSSL lab data"
    ) %>%
      handle_results(
        error_title = "OSSL download failed",
        error_hints = c("Check internet connection", "OSSL server may be down"),
        abort_on_null = FALSE
      ) -> lab_data

    if (is.null(lab_data)) return(NULL)

    ## Cache for future use ---------------------------------------------------

    qs::qsave(lab_data, lab_file)
    if (verbose) cli::cli_text("│  ├─ Cached to {basename(lab_file)}")

  }

  if (verbose) cli::cli_text("│  └─ Lab data: {nrow(lab_data)} samples")

  ## ---------------------------------------------------------------------------
  ## Step 2.4: Download/load OSSL MIR spectra
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Loading OSSL MIR spectra')}...")

  if (file.exists(mir_file)) {

    if (verbose) cli::cli_text("│  ├─ Found cached MIR data")

    safely_execute(
      qs::qread(mir_file),
      error_message = "Failed to read cached MIR data"
    ) %>%
      handle_results(
        error_title = "MIR cache read failed",
        error_hints = c("Cache may be corrupted", "Delete cache and re-download"),
        abort_on_null = FALSE
      ) -> mir_data

    if (is.null(mir_data)) return(NULL)

  } else {

    if (verbose) cli::cli_text("│  ├─ Downloading from OSSL server...")

    safely_execute(
      qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_mir_L0_v1.2.qs"),
      error_message = "Failed to download OSSL MIR data"
    ) %>%
      handle_results(
        error_title = "OSSL MIR download failed",
        error_hints = c("Check internet connection", "OSSL server may be down"),
        abort_on_null = FALSE
      ) -> mir_data

    if (is.null(mir_data)) return(NULL)

    ## Cache for future use ---------------------------------------------------

    qs::qsave(mir_data, mir_file)
    if (verbose) cli::cli_text("│  ├─ Cached to {basename(mir_file)}")

  }

  if (verbose) cli::cli_text("│  └─ MIR spectra: {nrow(mir_data)} samples")

  ## ---------------------------------------------------------------------------
  ## Step 2.4.5: Load location data for depth filtering
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Loading OSSL location metadata')}...")

  if (file.exists(location_file)) {

    if (verbose) cli::cli_text("│  ├─ Found cached location data")

    safely_execute(
      qs::qread(location_file),
      error_message = "Failed to read cached location data"
    ) %>%
      handle_results(
        error_title = "Location cache read failed",
        abort_on_null = FALSE
      ) -> location_data

  } else {

    if (verbose) cli::cli_text("│  ├─ Downloading from OSSL server...")

    safely_execute(
      qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_soilsite_L0_v1.2.qs"),
      error_message = "Failed to download OSSL location data"
    ) %>%
      handle_results(
        error_title = "OSSL location download failed",
        abort_on_null = FALSE
      ) -> location_data

    if (!is.null(location_data)) {
      qs::qsave(location_data, location_file)
      if (verbose) cli::cli_text("│  ├─ Cached to {basename(location_file)}")
    }

  }

  if (!is.null(location_data)) {
    if (verbose) cli::cli_text("│  └─ Location data: {nrow(location_data)} samples")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2.5: Extract property column(s) and join with spectra
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Joining lab data with spectra')}...")

  ## Check if texture property (requires all 3 components) --------------------

  if (is_texture_property(property)) {

    ## For texture: get ALL three texture columns ------------------------------

    texture_mapping <- get_library_property_mapping() %>%
      dplyr::filter(property %in% c("sand", "silt", "clay"))

    texture_cols <- texture_mapping$ossl_name

    ## Verify all columns exist ------------------------------------------------

    missing_cols <- setdiff(texture_cols, names(lab_data))

    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "Missing texture column{?s}: {paste(missing_cols, collapse = ', ')}",
        "i" = "Check OSSL version compatibility"
      )
    }

    ## Select all three texture columns ----------------------------------------

    lab_data %>%
      dplyr::select(id.layer_uuid_txt, dplyr::all_of(texture_cols)) %>%
      dplyr::filter(!is.na(sand.tot_usda.c60_w.pct),
                   !is.na(silt.tot_usda.c62_w.pct),
                   !is.na(clay.tot_usda.a334_w.pct)) -> lab_subset

    if (verbose) {
      cli::cli_text("│  ├─ {nrow(lab_subset)} samples with complete texture measurements")
    }

  } else {

    ## Standard property: single column ----------------------------------------

    if (!ossl_column %in% names(lab_data)) {
      cli::cli_abort("OSSL column '{ossl_column}' not found in lab data",
                     "i" = "Check property mapping or OSSL version compatibility")
    }

    lab_data %>%
      dplyr::select(id.layer_uuid_txt, !!ossl_column) %>%
      dplyr::filter(!is.na(!!rlang::sym(ossl_column))) -> lab_subset

    if (verbose) {
      cli::cli_text("│  ├─ {nrow(lab_subset)} samples with {property} measurements")
    }

  }

  ## Filter MIR to KSSL + Bruker Vertex 70 (Ng et al. 2022 dataset) ------------

  if (verbose) cli::cli_text("│  ├─ Filtering to KSSL + Bruker Vertex 70...")

  n_before_filter <- nrow(mir_data)

  mir_data %>%
    dplyr::filter(
      dataset.code_ascii_txt == "KSSL.SSL",
      scan.mir.model.name_utf8_txt == "Bruker Vertex 70 with HTS-XT accessory"
    ) -> mir_filtered

  n_after_filter <- nrow(mir_filtered)

  if (verbose) {
    cli::cli_text("│  ├─ KSSL + Bruker V70: {n_after_filter}/{n_before_filter} ({round(100*n_after_filter/n_before_filter)}%)")
  }

  ## Join with location data for depth filtering -------------------------------

  if (!is.null(location_data) && "layer.upper.depth_usda_cm" %in% names(location_data)) {

    if (verbose) cli::cli_text("│  ├─ Filtering to surface samples (depth < 30 cm)...")

    location_data %>%
      dplyr::select(id.layer_uuid_txt, layer.upper.depth_usda_cm) %>%
      dplyr::filter(!is.na(layer.upper.depth_usda_cm),
                   layer.upper.depth_usda_cm < 30) -> location_surface

    n_before_depth <- nrow(mir_filtered)

    mir_filtered %>%
      dplyr::inner_join(location_surface, by = "id.layer_uuid_txt") -> mir_filtered

    n_after_depth <- nrow(mir_filtered)

    if (verbose) {
      cli::cli_text("│  ├─ Surface samples: {n_after_depth}/{n_before_depth} ({round(100*n_after_depth/n_before_depth)}%)")
    }

  }

  ## Join with lab property measurements ---------------------------------------

  mir_filtered %>%
    dplyr::inner_join(lab_subset, by = "id.layer_uuid_txt") %>%
    dplyr::rename(sample_id = id.layer_uuid_txt) -> joined_data

  if (verbose) cli::cli_text("│  ├─ Final joined data: {nrow(joined_data)} samples")

  ## Rename MIR columns to numeric wavenumber format ---------------------------
  ## (build_recipe/step_transform_spectra expect numeric names, not X-prefix)

  spectral_cols <- grep("^scan_mir\\.[0-9]+_abs$", names(joined_data), value = TRUE)

  if (length(spectral_cols) > 0) {

    ## Extract just the wavenumber (600, 602, etc.) ------------------------------

    new_names <- gsub("scan_mir\\.", "", spectral_cols)
    new_names <- gsub("_abs$", "", new_names)

    names(joined_data)[names(joined_data) %in% spectral_cols] <- new_names

    if (verbose) cli::cli_text("│  └─ Renamed {length(spectral_cols)} spectral columns to numeric format")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2.5.5: Remove samples with missing spectra
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Filtering incomplete spectra')}...")

  ## Get spectral columns ------------------------------------------------------

  spectral_cols <- grep("^X[0-9]", names(joined_data), value = TRUE)

  if (length(spectral_cols) > 0) {

    ## Count NAs per sample ----------------------------------------------------

    spectral_matrix <- as.matrix(joined_data[, spectral_cols])
    sample_na_count <- rowSums(is.na(spectral_matrix))

    ## Filter to complete spectra ----------------------------------------------

    n_before <- nrow(joined_data)
    joined_data <- joined_data[sample_na_count == 0, ]
    n_after <- nrow(joined_data)
    n_removed <- n_before - n_after

    if (verbose) {
      cli::cli_text("│  ├─ Removed {n_removed} samples with missing spectra")
      cli::cli_text("│  └─ Retained {n_after} complete samples ({round(100*n_after/n_before)}%)")
    }

  }

  ## Free memory from large intermediate objects -------------------------------

  if (exists("location_data")) {
    rm(lab_data, mir_data, mir_filtered, lab_subset, location_data, location_surface)
  } else {
    rm(lab_data, mir_data, mir_filtered, lab_subset)
  }

  gc(verbose = FALSE)

  ## ---------------------------------------------------------------------------
  ## Step 2.6: Apply max_samples limit if specified (for testing)
  ## ---------------------------------------------------------------------------

  if (!is.null(max_samples) && max_samples < nrow(joined_data)) {

    if (verbose) cli::cli_text("│")
    if (verbose) cli::cli_text("├─ {cli::style_bold('Sampling subset for testing')}...")

    joined_data %>%
      dplyr::slice_sample(n = max_samples) -> joined_data

    if (verbose) cli::cli_text("│  └─ Sampled {max_samples} samples")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2.7: Return result
  ## ---------------------------------------------------------------------------

  return(joined_data)

}

## -----------------------------------------------------------------------------
## Section 3: Spectral Preprocessing for Clustering
## -----------------------------------------------------------------------------

#' Preprocess Library MIR Spectra for Clustering
#'
#' @description
#' Applies Standard Normal Variate (SNV) transformation to library spectra for
#' clustering space preparation. Optionally removes water interference bands.
#' This preprocessing creates the stable clustering space - model-specific
#' preprocessing (derivatives, baseline correction) happens separately during training.
#'
#' @details
#' **Preprocessing Pipeline:**
#' 1. Identify spectral columns (X<wavenumber> format)
#' 2. Remove water interference bands (optional, experimental)
#'    - Default regions: 3600-3000 cm⁻¹, 1650-1600 cm⁻¹
#'    - These are H2O absorption bands that mask soil signals
#' 3. Apply SNV transformation (always)
#'    - Normalize each spectrum to mean=0, sd=1
#'    - Removes multiplicative scatter, preserves spectral shape
#' 4. Reconstruct tibble with non-spectral columns preserved
#'
#' **Design Rationale:**
#' Clustering uses minimal preprocessing (SNV only) to create stable, interpretable
#' clusters based on fundamental spectral patterns. More aggressive preprocessing
#' (derivatives, baseline correction) is applied per-model during training to allow
#' flexibility in model configurations.
#'
#' **Edge Case Handling:**
#' - Zero-variance spectra: SNV produces NaN → replaced with 0
#' - Single samples: Handled gracefully
#' - Missing spectral columns: Returns NULL with warning
#'
#' @param spectral_data Data frame or tibble with spectral columns (X<wavenumber> format).
#'   Non-spectral columns (sample_id, property values) are preserved.
#' @param remove_water_bands Logical. Remove H2O interference regions? Default: FALSE.
#'   **Experimental** - validate impact on cluster quality before production use.
#'   From spectroscopy expert review: May improve clustering, test empirically.
#' @param water_regions List of numeric vectors. Wavenumber ranges [high, low] to exclude.
#'   Default: list(c(3600, 3000), c(1650, 1600)) cm⁻¹.
#'   Only used if remove_water_bands = TRUE.
#' @param verbose Logical. Print tree-style progress? Default: TRUE.
#'
#' @return Tibble with:
#' - SNV-transformed spectral columns (X<wavenumber>)
#' - Water bands removed if enabled (fewer columns)
#' - Non-spectral columns preserved
#'
#' Returns NULL if preprocessing fails.
#'
#' @section Performance:
#' - ~1-2 seconds for 500 samples
#' - ~10-20 seconds for 12K samples (full OSSL)
#' - Memory: <100MB overhead
#'
#' @examples
#' \dontrun{
#' # Basic preprocessing (SNV only)
#' preprocessed <- horizons:::preprocess_library_spectra(raw_spectra)
#'
#' # With water band removal (experimental)
#' preprocessed <- horizons:::preprocess_library_spectra(
#'   raw_spectra,
#'   remove_water_bands = TRUE,
#'   verbose = TRUE
#' )
#' }
#'
#' @seealso [load_ossl_raw()], [perform_pca_on_library()]
#' @keywords internal
preprocess_library_spectra <- function(spectral_data,
                                       remove_water_bands = FALSE,
                                       water_regions      = list(c(3600, 3000), c(1650, 1600)),
                                       verbose            = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 3.1: Identify spectral columns
  ## ---------------------------------------------------------------------------

  ## Extract wavenumber columns (numeric: 600, 602, or X600, X602) -------------

  spectral_cols <- grep("^X?[0-9]{3,4}$", names(spectral_data), value = TRUE)

  if (length(spectral_cols) == 0) {

    cli::cli_warn("No spectral columns found (expected X<wavenumber> format)")
    return(NULL)

  }

  ## Extract non-spectral columns (sample_id, property columns, etc.) ----------

  non_spectral_cols <- setdiff(names(spectral_data), spectral_cols)

  ## Extract spectra as matrix -------------------------------------------------

  spectra_matrix <- as.matrix(spectral_data[, spectral_cols])

  ## ---------------------------------------------------------------------------
  ## Step 3.2: Remove water interference bands (optional)
  ## ---------------------------------------------------------------------------

  if (remove_water_bands) {

    if (verbose) cli::cli_text("│")
    if (verbose) cli::cli_text("├─ {cli::style_bold('Removing water bands')} [EXPERIMENTAL]...")

    ## Extract wavenumbers from column names -----------------------------------
    ## Handle both numeric (600) and X-prefixed (X600) formats ----------------

    wavenumbers <- as.numeric(gsub("^X", "", spectral_cols))

    ## Identify columns to keep (outside water regions) ------------------------

    keep_cols <- rep(TRUE, length(wavenumbers))

    for (region in water_regions) {

      in_region <- wavenumbers >= region[2] & wavenumbers <= region[1]
      keep_cols <- keep_cols & !in_region

    }

    n_removed <- sum(!keep_cols)

    if (verbose) cli::cli_text("│  ├─ Removing {n_removed} wavenumbers in H2O regions")

    ## Filter spectra matrix and column names ----------------------------------

    spectra_matrix <- spectra_matrix[, keep_cols, drop = FALSE]
    spectral_cols  <- spectral_cols[keep_cols]

    if (verbose) cli::cli_text("│  └─ Retained {ncol(spectra_matrix)} wavenumbers")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3.3: Apply SNV transformation
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Applying SNV transformation')}...")

  ## Apply SNV to each spectrum (row-wise normalization) ----------------------

  safely_execute(
    prospectr::standardNormalVariate(spectra_matrix),
    error_message = "SNV transformation failed"
  ) %>%
    handle_results(
      error_title = "SNV transformation failed",
      error_hints = c("Check for NaN/Inf in spectra", "Verify spectra have variance"),
      abort_on_null = FALSE
    ) -> spectra_snv

  if (is.null(spectra_snv)) return(NULL)

  ## Handle edge case: zero variance creates NaN --------------------------------

  if (any(is.nan(spectra_snv)) || any(is.infinite(spectra_snv))) {

    cli::cli_warn("NaN/Inf detected after SNV - replacing with zeros")
    spectra_snv[is.nan(spectra_snv) | is.infinite(spectra_snv)] <- 0

  }

  if (verbose) cli::cli_text("│  └─ SNV complete: {nrow(spectra_snv)} spectra normalized")

  ## ---------------------------------------------------------------------------
  ## Step 3.3.5: Apply 1st Derivative (removes instrument baseline artifacts)
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Applying 1st derivative')}...")

  ## Apply Savitzky-Golay 1st derivative (instrument-robust clustering) --------

  safely_execute(
    prospectr::savitzkyGolay(spectra_snv, m = 1, p = 2, w = 11),
    error_message = "1st derivative transformation failed"
  ) %>%
    handle_results(
      error_title = "Derivative transformation failed",
      error_hints = c("Check for edge effects", "Verify sufficient wavenumbers"),
      abort_on_null = FALSE
    ) -> spectra_deriv

  if (is.null(spectra_deriv)) return(NULL)

  ## Handle NaN/Inf if derivative creates any ----------------------------------

  if (any(is.nan(spectra_deriv)) || any(is.infinite(spectra_deriv))) {
    cli::cli_warn("NaN/Inf detected after derivative - replacing with zeros")
    spectra_deriv[is.nan(spectra_deriv) | is.infinite(spectra_deriv)] <- 0
  }

  if (verbose) cli::cli_text("│  └─ 1st derivative complete (removes instrument baseline)")

  ## ---------------------------------------------------------------------------
  ## Step 3.4: Reconstruct tibble with preprocessed spectra
  ## ---------------------------------------------------------------------------

  ## Convert back to data frame ------------------------------------------------

  spectra_df <- as.data.frame(spectra_deriv)

  ## Update column names to match trimmed spectra (derivative removes edge wavenumbers)

  n_cols_after_deriv <- ncol(spectra_deriv)
  n_cols_lost <- length(spectral_cols) - n_cols_after_deriv

  if (n_cols_lost > 0) {
    ## Trim equal number from both ends (SG is symmetric)
    trim_each_side <- floor(n_cols_lost / 2)
    spectral_cols_trimmed <- spectral_cols[(trim_each_side + 1):(length(spectral_cols) - trim_each_side)]

    ## Handle odd number of lost columns (trim extra from end)
    if (length(spectral_cols_trimmed) > n_cols_after_deriv) {
      spectral_cols_trimmed <- spectral_cols_trimmed[1:n_cols_after_deriv]
    }

    colnames(spectra_df) <- spectral_cols_trimmed

    if (verbose) cli::cli_text("│  └─ Trimmed {n_cols_lost} edge wavenumbers (derivative window effect)")
  } else {
    colnames(spectra_df) <- spectral_cols
  }

  ## Add back non-spectral columns ---------------------------------------------

  if (length(non_spectral_cols) > 0) {

    dplyr::bind_cols(
      spectral_data[, non_spectral_cols, drop = FALSE],
      spectra_df
    ) -> result

  } else {

    result <- tibble::as_tibble(spectra_df)

  }

  ## ---------------------------------------------------------------------------
  ## Step 3.5: Return result
  ## ---------------------------------------------------------------------------

  return(result)

}

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Section 4: PCA Transformation for Clustering Space
## -----------------------------------------------------------------------------
## Section 4: PCA Transformation for Clustering Space
## -----------------------------------------------------------------------------

#' Perform PCA on Library Spectra
#'
#' @description
#' Applies Principal Component Analysis to preprocessed library spectra to create
#' the clustering space. Retains components explaining specified variance threshold
#' (default 99%). This PCA model is used to project unknowns into the same space.
#'
#' @details
#' Uses `FactoMineR::PCA()` with automatic component selection based on cumulative variance.
#' The PCA is unscaled (scale = FALSE) because spectra are already SNV-normalized.
#'
#' Typical results: 15-25 components for 99% variance on full OSSL library.
#'
#' @param library_data Tibble with preprocessed spectral columns (output from preprocess_library_spectra)
#' @param variance_threshold Numeric (0-1). Cumulative variance to retain. Default: 0.99 (99%).
#'   Lower values = fewer components = faster clustering, but may lose spectral information.
#' @param verbose Logical. Print tree-style progress? Default: TRUE
#'
#' @return List with:
#' \describe{
#'   \item{pca_model}{FactoMineR::PCA object - use for projecting new samples}
#'   \item{pca_scores}{Matrix of PCA scores (n_samples × n_components)}
#'   \item{n_components}{Integer, number of components retained to reach threshold}
#'   \item{variance_explained}{Numeric, actual cumulative variance explained}
#' }
#'
#' Returns NULL if PCA fails (insufficient variance, constant columns, etc.)
#'
#' @examples
#' \dontrun{
#' preprocessed <- horizons:::preprocess_library_spectra(raw_data)
#' pca_result <- horizons:::perform_pca_on_library(preprocessed, variance_threshold = 0.99)
#' }
#'
#' @seealso [preprocess_library_spectra()], [project_to_library_pca()]
#' @keywords internal
perform_pca_on_library <- function(library_data,
                                   variance_threshold = 0.99,
                                   verbose            = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 4.1: Extract spectral matrix
  ## ---------------------------------------------------------------------------

  ## Identify spectral columns (numeric or X-prefixed) -------------------------

  spectral_cols <- grep("^X?[0-9]{3,4}$", names(library_data), value = TRUE)

  if (length(spectral_cols) == 0) {

    cli::cli_warn("No spectral columns found for PCA")
    return(NULL)

  }

  ## Extract as matrix ---------------------------------------------------------

  spectra_matrix <- as.matrix(library_data[, spectral_cols])

  ## ---------------------------------------------------------------------------
  ## Step 4.2: Perform PCA
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Training PCA on library')}...")

  ## Run PCA with FactoMineR ---------------------------------------------------

  safely_execute(
    FactoMineR::PCA(spectra_matrix,
                    ncp    = min(100, ncol(spectra_matrix) - 1),  # Max components
                    scale  = FALSE,  # Already SNV-normalized
                    graph  = FALSE),
    error_message = "PCA training failed"
  ) %>%
    handle_results(
      error_title = "PCA training failed",
      error_hints = c("Check for constant/zero variance columns", "Verify sufficient samples"),
      abort_on_null = FALSE
    ) -> pca_model

  if (is.null(pca_model)) return(NULL)

  ## ---------------------------------------------------------------------------
  ## Step 4.3: Determine number of components for variance threshold
  ## ---------------------------------------------------------------------------

  ## Calculate cumulative variance explained -----------------------------------

  eigenvalues <- pca_model$eig[, 1]
  cum_var     <- cumsum(eigenvalues) / sum(eigenvalues)

  ## Find number of components to reach threshold ------------------------------

  n_components <- which(cum_var >= variance_threshold)[1]

  if (is.na(n_components)) {

    cli::cli_warn("Could not reach {variance_threshold*100}% variance threshold")
    n_components <- length(cum_var)

  }

  variance_explained <- cum_var[n_components]

  if (verbose) {
    cli::cli_text("│  ├─ Variance threshold: {variance_threshold*100}%")
    cli::cli_text("│  ├─ Components selected: {n_components}")
    cli::cli_text("│  └─ Variance explained: {round(variance_explained*100, 2)}%")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4.4: Extract PCA scores
  ## ---------------------------------------------------------------------------

  ## Get scores for selected components ----------------------------------------

  n_components_available <- ncol(pca_model$ind$coord)
  n_components_final     <- min(n_components, n_components_available)

  pca_scores <- pca_model$ind$coord[, 1:n_components_final, drop = FALSE]

  ## ---------------------------------------------------------------------------
  ## Step 4.5: Return result
  ## ---------------------------------------------------------------------------

  list(pca_model          = pca_model,
       pca_scores         = pca_scores,
       n_components       = n_components_final,
       variance_explained = variance_explained) -> result

  return(result)

}

#' Project Unknowns to Library PCA Space
#'
#' @description
#' Projects new (unknown) spectra into the library PCA space using a trained
#' PCA model. Unknowns must have same preprocessing and wavenumber grid as
#' the library data used to train the PCA.
#'
#' @param new_data Tibble with preprocessed spectral columns (same grid as library)
#' @param pca_model FactoMineR::PCA object from perform_pca_on_library()
#' @param verbose Logical. Print progress? Default: TRUE
#'
#' @return Matrix of PCA scores for unknown samples (n_samples × n_components)
#' Returns NULL if projection fails.
#'
#' @keywords internal
project_to_library_pca <- function(new_data,
                                   pca_model,
                                   verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 4.6: Extract spectral matrix from unknowns
  ## ---------------------------------------------------------------------------

  ## Identify spectral columns (numeric or X-prefixed) -------------------------

  spectral_cols <- grep("^X?[0-9]{3,4}$", names(new_data), value = TRUE)

  if (length(spectral_cols) == 0) {

    cli::cli_warn("No spectral columns found in unknown data")
    return(NULL)

  }

  ## Extract as matrix ---------------------------------------------------------

  spectra_matrix <- as.matrix(new_data[, spectral_cols])

  ## ---------------------------------------------------------------------------
  ## Step 4.7: Project to PCA space
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Projecting unknowns to PCA space')}...")

  ## Use predict method from FactoMineR ----------------------------------------

  safely_execute(
    stats::predict(pca_model, newdata = spectra_matrix),
    error_message = "PCA projection failed"
  ) %>%
    handle_results(
      error_title = "PCA projection failed",
      error_hints = c("Check wavenumber grid matches library", "Verify preprocessing matches library"),
      abort_on_null = FALSE
    ) -> pca_projected

  if (is.null(pca_projected)) return(NULL)

  ## Extract coordinate matrix -------------------------------------------------

  pca_scores <- pca_projected$coord

  if (verbose) cli::cli_text("│  └─ Projected {nrow(pca_scores)} samples to {ncol(pca_scores)} components")

  ## ---------------------------------------------------------------------------
  ## Step 4.8: Return result
  ## ---------------------------------------------------------------------------

  return(pca_scores)

}

## -----------------------------------------------------------------------------
## Section 5: Main Orchestrator - Complete Pipeline
## -----------------------------------------------------------------------------

#' Load and Process Spectral Library Data
#'
#' @description
#' Loads reference spectral library data (OSSL/KSSL) for a given soil property,
#' applies method harmonization, preprocessing, and PCA transformation. This is the
#' primary data preparation function for library-based prediction mode.
#'
#' @details
#' The function executes the following pipeline:
#' 1. Load raw OSSL data for the specified property
#' 2. Preprocess MIR spectra (optional water band removal + SNV)
#' 3. Perform PCA to create clustering space
#' 4. Clean up memory after each stage
#'
#' All intermediate objects are aggressively cleaned to maintain <500MB overhead.
#'
#' Clustering space uses minimal preprocessing (SNV only) to preserve flexibility.
#' Model-specific preprocessing (derivatives, etc.) happens during training.
#'
#' @param property Character. Property name from LIBRARY_PROPERTIES
#' @param variance_threshold Numeric. Cumulative variance to retain in PCA (0-1). Default: 0.99
#' @param remove_water_bands Logical. Remove H2O interference regions? Default: FALSE.
#'   Experimental feature - validate impact before production use.
#' @param water_regions List. Wavenumber ranges to remove if remove_water_bands = TRUE.
#'   Default: list(c(3600, 3000), c(1650, 1600))
#' @param max_samples Integer. Maximum samples to load (for testing). Default: NULL (all).
#' @param cache_dir Character. Cache directory for OSSL downloads. Default: NULL (user dir).
#' @param verbose Logical. Print tree-style progress messages? Default: TRUE
#'
#' @return A list with components:
#' \describe{
#'   \item{library_data}{Tibble with preprocessed spectral data and property measurements}
#'   \item{pca_model}{PCA transformation object for projecting new samples}
#'   \item{pca_scores}{Matrix of PCA scores for library samples}
#'   \item{n_components}{Integer, number of PCA components retained}
#'   \item{n_samples}{Integer, number of library samples}
#'   \item{property}{Character, property name}
#'   \item{variance_explained}{Numeric, cumulative variance in retained components}
#' }
#' Returns NULL if any stage fails.
#'
#' @section Memory Management:
#' The function uses aggressive cleanup:
#' - rm() + gc() after raw loading
#' - rm() + gc() after preprocessing
#' - rm() + gc() after PCA
#' Target: <500MB overhead regardless of library size
#'
#' @examples
#' \dontrun{
#' # Load and process clay data
#' clay_lib <- get_processed_library_data("clay")
#'
#' # With custom settings
#' oc_lib <- get_processed_library_data(
#'   property = "oc",
#'   variance_threshold = 0.95,
#'   remove_water_bands = TRUE,
#'   verbose = TRUE
#' )
#' }
#'
#' @keywords internal
get_processed_library_data <- function(property,
                                       variance_threshold = 0.99,
                                       remove_water_bands = FALSE,
                                       water_regions      = list(c(3600, 3000), c(1650, 1600)),
                                       max_samples        = NULL,
                                       cache_dir          = NULL,
                                       verbose            = TRUE) {

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("{cli::style_bold('Processing library data for:')} {property}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 5.1: Load raw OSSL data
  ## ---------------------------------------------------------------------------

  ## Load with orchestrator-level error handling -------------------------------

  load_ossl_raw(property    = property,
                cache_dir   = cache_dir,
                max_samples = max_samples,
                verbose     = verbose) -> raw_data

  ## Orchestrator aborts on NULL (not recoverable) -----------------------------

  if (is.null(raw_data)) {
    cli::cli_abort("Failed to load OSSL data for {property}",
                   "i" = "Check logs above for details")
  }

  ## Save reference to raw data (needed for training) --------------------------

  raw_library_data <- raw_data
  n_samples_raw    <- nrow(raw_data)

  ## ---------------------------------------------------------------------------
  ## Step 5.2: Preprocess spectra for clustering
  ## ---------------------------------------------------------------------------

  preprocess_library_spectra(spectral_data      = raw_data,
                             remove_water_bands = remove_water_bands,
                             water_regions      = water_regions,
                             verbose            = verbose) -> preprocessed_data

  if (is.null(preprocessed_data)) {
    cli::cli_abort("Failed to preprocess spectra for {property}",
                   "i" = "Check logs above for details")
  }

  ## Note: Keeping raw_library_data for training (no rm here) ------------------
  ## Preprocessed space is for clustering only
  ## Training will use raw data with config-specific preprocessing

  ## ---------------------------------------------------------------------------
  ## Step 5.3: Perform PCA
  ## ---------------------------------------------------------------------------

  perform_pca_on_library(library_data       = preprocessed_data,
                        variance_threshold = variance_threshold,
                        verbose            = verbose) -> pca_result

  if (is.null(pca_result)) {
    cli::cli_abort("Failed to perform PCA on {property} library",
                   "i" = "Check logs above for details")
  }

  ## Free memory from preprocessed data (keep only PCA) ------------------------

  rm(preprocessed_data)
  gc(verbose = FALSE)

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("├─ {cli::style_bold('Memory freed')}: preprocessed spectra")
  }

  ## ---------------------------------------------------------------------------
  ## Step 5.4: Assemble final result
  ## ---------------------------------------------------------------------------

  list(library_data_raw    = raw_library_data,  # RAW for training (preprocessing flexibility)
       pca_model           = pca_result$pca_model,
       pca_scores          = pca_result$pca_scores,
       n_components        = pca_result$n_components,
       n_samples           = n_samples_raw,
       property            = property,
       variance_explained  = pca_result$variance_explained) -> result

  if (verbose) {
    cli::cli_text("│")
    cli::cli_text("└─ {cli::style_bold('Pipeline complete')}")
    cli::cli_text("")
    cli::cli_text("   Property: {property}")
    cli::cli_text("   Samples: {n_samples_raw}")
    cli::cli_text("   Components: {pca_result$n_components}")
    cli::cli_text("   Variance: {round(pca_result$variance_explained*100, 2)}%")
  }

  ## ---------------------------------------------------------------------------
  ## Step 5.5: Return result
  ## ---------------------------------------------------------------------------

  return(result)

}
## Section 2.5: Method Harmonization (Stub for v1.0)
## -----------------------------------------------------------------------------

#' Filter Library Data by Laboratory Method
#'
#' @description
#' Filters OSSL library data to samples measured with a specific laboratory method.
#' Method harmonization reduces variance from different measurement protocols.
#'
#' @param data Tibble with OSSL data
#' @param property Character. Property name
#' @param target_method Character. Method to filter for. Default: "default"
#'
#' @return Filtered tibble (currently returns all data - full implementation in Phase 1.2)
#'
#' @keywords internal
filter_by_method <- function(data,
                             property,
                             target_method = "default") {

  ## TODO: Implement method filtering in Phase 1, Milestone 1.2
  ## For now, return all data (no filtering)

  return(data)

}
