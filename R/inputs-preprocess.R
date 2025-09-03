#' Preprocess Spectral Data for Modeling
#'
#' @description
#' Prepares spectral data for modeling by applying essential cleaning steps.
#' This includes trimming to specific wavelength ranges, resampling to common
#' grids, and optionally masking atmospheric interference regions.
#' 
#' Note: This function performs data preparation only. Spectral transformations
#' (SNV, derivatives, etc.) are handled by step_transform_spectra() in the 
#' modeling workflow.
#'
#' @param spectra_data Tibble. Spectral data from read_spectra() in wide format
#' @param resample_interval Numeric. Interval for resampling spectra.
#'   Default: 2 (every 2 cm⁻¹)
#' @param baseline_method Character. Baseline correction method: 
#'   - "none": No baseline correction (default)
#'   - "rubberband": Convex hull baseline removal
#'   - "polynomial": SNV + 2nd order polynomial detrending
#'   Default: "none"
#' @param verbose Logical. Print progress messages. Default: TRUE
#'
#' @return A tibble with preprocessed spectral data in wide format
#'
#' @examples
#' \dontrun{
#' # Read and preprocess MIR spectra
#' spectra <- read_spectra("opus", "data/spectra/", "MIR") %>%
#'   preprocess_spectra(
#'     resample_interval = 2,
#'     baseline_method = "rubberband"
#'   )
#' }
#'
#' @export
preprocess_spectra <- function(spectra_data,
                              resample_interval = 2,
                              baseline_method   = "none",
                              verbose           = TRUE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------
  
  if (!is.data.frame(spectra_data)) {
    
    cli::cli_abort("▶ preprocess_spectra: spectra_data must be a data frame or tibble")
    
  }
  
  if (!"Sample_ID" %in% names(spectra_data)) {
    
    cli::cli_abort("▶ preprocess_spectra: spectra_data must have a Sample_ID column")
    
  }
  
  if (!is.numeric(resample_interval) || resample_interval <= 0) {
    
    cli::cli_abort("▶ preprocess_spectra: resample_interval must be a positive number")
    
  }
  
  baseline_method <- match.arg(baseline_method, c("none", "rubberband", "polynomial"))
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Extract wavenumber columns
  ## ---------------------------------------------------------------------------
  
  # Get all columns except Sample_ID
  spectral_cols <- setdiff(names(spectra_data), "Sample_ID")
  
  # Extract wavenumbers from column names (remove "wn_" prefix if present)
  wavenumbers <- as.numeric(gsub("^wn_", "", spectral_cols))
  
  if (any(is.na(wavenumbers))) {
    
    cli::cli_abort("▶ preprocess_spectra: Non-numeric column names found in spectral data")
    
  }
  
  if (verbose) {
    
    cli::cli_alert_info("Processing {.val {nrow(spectra_data)}} spectra")
    cli::cli_alert_info("Original range: {.val {round(min(wavenumbers))}}-{.val {round(max(wavenumbers))}} cm⁻¹")
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Apply baseline correction on original data (if requested)
  ## ---------------------------------------------------------------------------
  
  # Check memory requirements before matrix conversion
  n_elements <- nrow(spectra_data) * length(spectral_cols)
  est_memory_gb <- n_elements * 8 / 1e9  # 8 bytes per double
  
  if (est_memory_gb > 2 && verbose) {
    
    cli::cli_alert_warning("Matrix conversion will require ~{.val {round(est_memory_gb, 1)}} GB memory")
    
  }
  
  # Convert to matrix for processing
  spectral_matrix <- as.matrix(spectra_data[, spectral_cols])
  
  if (baseline_method != "none") {
    
    if (verbose) {
      
      cli::cli_alert_info("Applying {.val {baseline_method}} baseline correction to original spectra")
      
    }
    
    if (baseline_method == "rubberband") {
      
      # Use prospectr's convex hull baseline method on original data
      spectral_matrix <- prospectr::baseline(X = spectral_matrix, 
                                             wav = wavenumbers)
      
    } else if (baseline_method == "polynomial") {
      
      # Use prospectr's detrend (SNV + 2nd order polynomial) on original data
      spectral_matrix <- prospectr::detrend(X   = spectral_matrix,
                                            wav = wavenumbers,
                                            p   = 2)
      
    }
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Validate spectral data (warn only, don't stop)
  ## ---------------------------------------------------------------------------
  
  if (any(!is.finite(spectral_matrix))) {
    
    n_inf <- sum(is.infinite(spectral_matrix))
    n_nan <- sum(is.nan(spectral_matrix))
    n_na  <- sum(is.na(spectral_matrix))
    
    cli::cli_warn(c(
      "!" = "Non-finite values detected in spectral data",
      "i" = "Infinite: {.val {n_inf}}, NaN: {.val {n_nan}}, NA: {.val {n_na}}",
      "i" = "These may cause issues in modeling"
    ))
    
  }
  
  # Check for all-zero spectra
  row_sums <- rowSums(spectral_matrix, na.rm = TRUE)
  n_zero   <- sum(row_sums == 0)
  
  if (n_zero > 0) {
    
    zero_samples <- spectra_data$Sample_ID[row_sums == 0]
    cli::cli_warn(c(
      "!" = "Found {.val {n_zero}} spectra with all zero values",
      "i" = "Samples: {.val {head(zero_samples, 3)}}",
      "i" = "These may indicate measurement failures"
    ))
    
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Resample spectra to regular grid
  ## ---------------------------------------------------------------------------
  
  # Check spectra type
  spectra_type <- attr(spectra_data, "spectra_type")
  
  if (is.null(spectra_type)) {
    
    cli::cli_warn("No spectra_type attribute found, assuming MIR")
    spectra_type <- "MIR"
    
  }
  
  # Set standard ranges
  if (spectra_type == "MIR") {
    
    # MIR standard: 600-4000 cm⁻¹
    target_wavenumbers <- seq(from = 600, to = 4000, by = resample_interval)
    
  } else if (spectra_type == "NIR") {
    
    # NIR: To be determined when needed
    cli::cli_abort("NIR preprocessing not yet implemented")
    
  } else {
    
    cli::cli_abort("Unknown spectra_type: {.val {spectra_type}}")
    
  }
  
  if (verbose) {
    
    cli::cli_alert_info("Resampling to {.val {length(target_wavenumbers)}} points at {.val {resample_interval}} cm⁻¹ intervals")
    
  }
  
  # Use prospectr::resample to interpolate to new grid
  # Note: spectral_matrix may already be baseline corrected
  resampled_matrix <- prospectr::resample(X          = spectral_matrix,
                                          wav        = wavenumbers,
                                          new.wav    = target_wavenumbers,
                                          interpol   = "spline")
  
  ## ---------------------------------------------------------------------------
  ## Step 6: Reconstruct tibble with resampled data (memory efficient)
  ## ---------------------------------------------------------------------------
  
  # Convert matrix to tibble efficiently
  resampled_data <- tibble::as_tibble(resampled_matrix) %>%
    setNames(paste0("wn_", target_wavenumbers)) %>%
    tibble::add_column(Sample_ID = spectra_data$Sample_ID, .before = 1)
  
  if (verbose) {
    
    cli::cli_alert_success("Preprocessing complete: {.val {nrow(resampled_data)}} spectra with {.val {length(target_wavenumbers)}} wavenumbers")
    
  }
  
  return(resampled_data)
  
}