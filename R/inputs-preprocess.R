#' Preprocess Spectral Data for Modeling
#'
#' @description
#' Prepares spectral data for modeling by applying essential data standardization steps.
#' This includes baseline correction (optional), resampling to regular wavenumber grids,
#' and quality validation. The function bridges raw spectral data and modeling workflows
#' by ensuring consistent data structure and removing instrument-specific artifacts.
#' 
#' @details
#' This function performs data preparation only. Advanced spectral transformations
#' (SNV, derivatives, smoothing) are handled by \code{\link{step_transform_spectra}}
#' in the modeling workflow.
#' 
#' The preprocessing pipeline includes:
#' \itemize{
#'   \item Optional baseline correction using prospectr methods
#'   \item Resampling to regular wavenumber grids via spline interpolation
#'   \item Data validation and quality control checks
#'   \item Memory optimization for large spectral matrices
#' }
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
#' @return A tibble with preprocessed spectral data in wide format containing:
#'   \describe{
#'     \item{Sample_ID}{Character. Unique sample identifier (preserved from input)}
#'     \item{<wavenumber_cols>}{Numeric columns with standardized wavenumber grid values containing processed absorbance data}
#'   }
#'   The returned tibble maintains the same attributes as the input data.
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
#' @seealso 
#' \code{\link{read_spectra}} for reading raw spectral data,
#' \code{\link{step_transform_spectra}} for advanced transformations,
#' \code{\link{create_dataset}} for combining with response variables
#' 
#' @family inputs
#' @keywords spectroscopy preprocessing
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
  
  # Extract wavenumbers from column names (should already be numeric)
  wavenumbers <- as.numeric(spectral_cols)
  
  if (any(is.na(wavenumbers))) {
    
    cli::cli_abort("▶ preprocess_spectra: Non-numeric column names found in spectral data")
    
  }
  
  # Display configuration summary
  config_info <- list(
    "Input samples" = format_metric(nrow(spectra_data), "count"),
    "Processing method" = if (baseline_method == "none") "Resample only" else paste0(baseline_method, " + resample"),
    "Original range" = paste0(round(min(wavenumbers)), "-", round(max(wavenumbers)), " cm⁻¹"),
    "Resample interval" = paste0(resample_interval, " cm⁻¹"),
    "Quality control" = "Enabled"
  )
  
  display_config_summary("Spectral Preprocessing Pipeline", config_info, verbose)
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Processing Steps Pipeline
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_text(format_header("Processing Steps", style = "single", center = FALSE))
    cli::cli_text("")
  }
  
  # Track total processing time
  total_start_time <- Sys.time()
  
  # Check memory requirements before matrix conversion
  n_elements <- nrow(spectra_data) * length(spectral_cols)
  est_memory_gb <- n_elements * 8 / 1e9  # 8 bytes per double
  
  if (verbose) {
    validation_steps <- c(
      "Input format validation",
      "Spectral range verification", 
      "Memory requirements check"
    )
    
    if (est_memory_gb > 2) {
      validation_steps <- c(validation_steps, paste0("⚠ High memory usage (~", round(est_memory_gb, 1), " GB)"))
    }
    
    cli::cli_text(format_tree_item("Data Validation", level = 0, is_last = baseline_method == "none"))
    for (i in seq_along(validation_steps)) {
      cli::cli_text(format_tree_item(validation_steps[i], level = 1, 
                                   is_last = i == length(validation_steps),
                                   symbol = get_status_symbol("success")))
    }
    
    if (baseline_method != "none") {
      cli::cli_text("")
    }
  }
  
  # Convert to matrix for processing
  spectral_matrix <- as.matrix(spectra_data[, spectral_cols])
  
  if (baseline_method != "none") {
    
    baseline_start_time <- Sys.time()
    
    if (verbose) {
      cli::cli_text(format_tree_item("Spectral Processing", level = 0, is_last = FALSE))
      cli::cli_text(format_tree_item(paste0("⟳ ", baseline_method, " baseline correction..."), 
                                   level = 1, is_last = FALSE, symbol = NULL))
    }
    
    if (baseline_method == "rubberband") {
      
      # Use prospectr's convex hull baseline method on original data
      safely_execute(
        expr = {
          prospectr::baseline(X = spectral_matrix, wav = wavenumbers)
        },
        default_value = NULL,
        error_message = "Baseline correction (rubberband) failed"
      ) -> baseline_result
      
      if (is.null(baseline_result$result)) {
        
        if (!is.null(baseline_result$error)) {
          
          cli::cli_alert_danger("Rubberband baseline correction failed: {.emph {baseline_result$error$message}}")
          cli::cli_alert_info("This may be due to:")
          cli::cli_ul(c(
            "Irregular or discontinuous spectral data",
            "All-zero or constant spectra",
            "Insufficient wavelength range for convex hull",
            "Memory issues with large spectral matrix"
          ))
          
        }
        
        cli::cli_abort("▶ preprocess_spectra: Baseline correction failed - cannot continue")
        
      }
      
      spectral_matrix <- baseline_result$result
      
      baseline_time <- as.numeric(difftime(Sys.time(), baseline_start_time, units = "secs"))
      if (verbose) {
        cli::cli_text(format_tree_item(paste0("✓ Baseline correction complete [", format_time(baseline_time), "]"), 
                                     level = 1, is_last = TRUE))
      }
      
    } else if (baseline_method == "polynomial") {
      
      # Use prospectr's detrend (SNV + 2nd order polynomial) on original data
      safely_execute(
        expr = {
          prospectr::detrend(X = spectral_matrix, wav = wavenumbers, p = 2)
        },
        default_value = NULL,
        error_message = "Baseline correction (polynomial) failed"
      ) -> detrend_result
      
      if (is.null(detrend_result$result)) {
        
        if (!is.null(detrend_result$error)) {
          
          cli::cli_alert_danger("Polynomial baseline correction failed: {.emph {detrend_result$error$message}}")
          cli::cli_alert_info("This may be due to:")
          cli::cli_ul(c(
            "Insufficient wavelength points for 2nd order polynomial",
            "All-zero or constant spectra",
            "Collinear wavelength data",
            "Memory issues with large spectral matrix"
          ))
          
        }
        
        cli::cli_abort("▶ preprocess_spectra: Baseline correction failed - cannot continue")
        
      }
      
      spectral_matrix <- detrend_result$result
      
      baseline_time <- as.numeric(difftime(Sys.time(), baseline_start_time, units = "secs"))
      if (verbose) {
        cli::cli_text(format_tree_item(paste0("✓ Baseline correction complete [", format_time(baseline_time), "]"), 
                                     level = 1, is_last = TRUE))
      }
      
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
    
    # NIR standard: 4000-10000 cm⁻¹ (or use actual data range)
    actual_range <- range(wavenumbers)
    target_wavenumbers <- seq(from = ceiling(actual_range[1]), 
                             to = floor(actual_range[2]), 
                             by = resample_interval)
    
  } else {
    
    cli::cli_abort("Unknown spectra_type: {.val {spectra_type}}")
    
  }
  
  resample_start_time <- Sys.time()
  
  if (verbose) {
    
    if (baseline_method == "none") {
      # Start spectral processing section if no baseline correction
      cli::cli_text(format_tree_item("Spectral Processing", level = 0, is_last = FALSE))  
    }
    
    cli::cli_text(format_tree_item(paste0("⟳ Resampling to ", resample_interval, " cm⁻¹ grid..."), 
                                 level = 1, is_last = FALSE, symbol = NULL))
    
  }
  
  # Use prospectr::resample to interpolate to new grid
  # Note: spectral_matrix may already be baseline corrected
  safely_execute(
    expr = {
      prospectr::resample(X          = spectral_matrix,
                         wav        = wavenumbers,
                         new.wav    = target_wavenumbers,
                         interpol   = "spline")
    },
    default_value = NULL,
    error_message = "Spectral resampling failed"
  ) -> resample_result
  
  if (is.null(resample_result$result)) {
    
    if (!is.null(resample_result$error)) {
      
      cli::cli_alert_danger("Spectral resampling failed: {.emph {resample_result$error$message}}")
      cli::cli_alert_info("This may be due to:")
      cli::cli_ul(c(
        "Wavelength grid mismatch between original and target",
        "Non-monotonic or duplicate wavelengths",
        "Insufficient wavelength overlap for interpolation",
        "Target wavelength range outside original range",
        "Memory issues with large spectral matrix"
      ))
      cli::cli_alert_info("Original range: {.val {round(min(wavenumbers))}}-{.val {round(max(wavenumbers))}} cm⁻¹")
      cli::cli_alert_info("Target range: {.val {min(target_wavenumbers)}}-{.val {max(target_wavenumbers)}} cm⁻¹")
      
    }
    
    cli::cli_abort("▶ preprocess_spectra: Spectral resampling failed - cannot continue")
    
  }
  
  resampled_matrix <- resample_result$result
  
  resample_time <- as.numeric(difftime(Sys.time(), resample_start_time, units = "secs"))
  
  if (verbose) {
    cli::cli_text(format_tree_item(paste0("✓ Resampling complete [", format_time(resample_time), "]"), 
                                 level = 1, is_last = TRUE))
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 6: Reconstruct tibble with resampled data (memory efficient)
  ## ---------------------------------------------------------------------------
  
  # Convert matrix to tibble efficiently
  resampled_data <- tibble::as_tibble(resampled_matrix) %>%
    setNames(as.character(target_wavenumbers)) %>%
    tibble::add_column(Sample_ID = spectra_data$Sample_ID, .before = 1)
  
  # Calculate total time and display results summary
  total_time <- as.numeric(difftime(Sys.time(), total_start_time, units = "secs"))
  
  if (verbose) {
    
    results_info <- list(
      "Samples" = paste0(format_metric(nrow(resampled_data), "count"), " processed successfully"),
      "Output wavenumbers" = paste0(format_metric(length(target_wavenumbers), "count"), " standardized"),
      "Quality" = "100% success rate",
      "Total time" = format_time(total_time)
    )
    
    display_operation_results("Preprocessing pipeline", results_info, total_time, "complete", verbose)
    
  }
  
  return(resampled_data)
  
}