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
#' (SNV, derivatives, smoothing) are handled by [step_transform_spectra()]
#' in the modeling workflow.
#'
#' The preprocessing pipeline includes:
#'
#' * Optional baseline correction using prospectr methods
#' * Resampling to regular wavenumber grids via spline interpolation
#' * Data validation and quality control checks
#' * Memory optimization for large spectral matrices
#'
#' @param spectra_data `[tibble]` Spectral data from `read_spectra()` in wide format
#' @param resample_interval `[numeric]` Interval for resampling spectra (cm⁻¹). Default: `2`
#' @param baseline_method `[character]` Baseline correction method: `"none"` (default),
#'   `"rubberband"` (convex hull baseline removal), or `"polynomial"` (SNV + 2nd order
#'   polynomial detrending)
#' @param verbose `[logical]` Print progress messages. Default: `TRUE`
#'
#' @return A `[tibble]` with preprocessed spectral data in wide format containing:
#'   * `Sample_ID`: Character. Unique sample identifier (preserved from input)
#'   * `<wavenumber_cols>`: Numeric columns with standardized wavenumber grid values
#'     containing processed absorbance data
#'
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
#' [read_spectra()] for reading raw spectral data,
#' [step_transform_spectra()] for advanced transformations,
#' [create_dataset()] for combining with response variables
#'
#' @family inputs
#' @keywords spectroscopy preprocessing
#'
#' @importFrom cli cli_abort cli_warn cli_text cli_alert_info col_yellow
#' @importFrom glue glue
#' @importFrom prospectr baseline detrend resample
#' @importFrom stats setNames
#' @importFrom tibble as_tibble add_column
#'
#' @export
preprocess_spectra <- function(spectra_data,
                               resample_interval = 2,
                               baseline_method   = "none",
                               verbose           = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  ## Validate spectra_data structure -------------------------------------------

  if (!is.data.frame(spectra_data)) cli::cli_abort("spectra_data must be a data frame or tibble")

  if (!"Sample_ID" %in% names(spectra_data)) cli::cli_abort("spectra_data must have a Sample_ID column")


  ## Validate preprocessing parameters -----------------------------------------

  if (!is.numeric(resample_interval) || resample_interval <= 0) cli::cli_abort("resample_interval must be a positive number")

  baseline_method <- match.arg(baseline_method, c("none", "rubberband", "polynomial"))

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract wavenumber columns
  ## ---------------------------------------------------------------------------

  ## Extract spectral data columns ---------------------------------------------

  spectral_cols <- setdiff(names(spectra_data), "Sample_ID")

  if (length(spectral_cols) == 0) {
    cli::cli_abort("spectra_data must have at least one spectral column")
  }

  wavenumbers <- as.numeric(spectral_cols)

  if (any(is.na(wavenumbers))) {
    cli::cli_abort("Non-numeric column names found in spectral data")
  }

  ## Display preprocessing configuration ---------------------------------------

  if (verbose) {

    processing_desc <- if (baseline_method == "none") {
      "Resample only"
    } else {
      paste0(baseline_method, " baseline + resample")
    }

    wn_range <- paste0(round(min(wavenumbers)), "-", round(max(wavenumbers)), " cm⁻¹")

    cli::cli_text("")
    cli::cli_text("{.strong Spectral Preprocessing Pipeline}")
    cli::cli_text("├─ Input samples: {nrow(spectra_data)}")
    cli::cli_text("├─ Processing method: {processing_desc}")
    cli::cli_text("├─ Original range: {wn_range}")
    cli::cli_text("└─ Resample interval: {resample_interval} cm⁻¹")
    cli::cli_text("")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Processing Steps Pipeline
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("{.strong Processing Steps}")
  }

  ## Track total processing time -------------------------------------------

  total_start_time <- Sys.time()

  ## Check memory requirements and warn if high ----------------------------

  n_elements    <- nrow(spectra_data) * length(spectral_cols)
  est_memory_gb <- n_elements * 8 / 1e9

  if (est_memory_gb > 2 && verbose) {
    cli::cli_text("{cli::col_yellow('⚠ High memory usage: ~{round(est_memory_gb, 1)} GB')}")
  }

  ## Convert to matrix for prospectr processing ----------------------------

  spectral_matrix <- as.matrix(spectra_data[, spectral_cols])

  ## Apply baseline correction if requested --------------------------------

  if (baseline_method != "none") {

    baseline_start_time <- Sys.time()

    if (verbose) {
      cli::cli_text("├─ Baseline correction ({baseline_method})")
    }

    if (baseline_method == "rubberband") {

      safely_execute(expr = {
                              prospectr::baseline(X = spectral_matrix, wav = wavenumbers)
                             },
                     default_value = NULL,
                     error_message = glue::glue("Rubberband baseline correction failed")) -> baseline_safe

      handle_results(safe_result   = baseline_safe,
                     error_title   = "Rubberband baseline correction failed",
                     error_hints   = c("Check for irregular or discontinuous spectral data",
                                       "Check for all-zero or constant spectra",
                                       "Check for insufficient wavelength range for convex hull",
                                       "Check for memory issues with large spectral matrix"),
                     abort_on_null = TRUE) -> spectral_matrix

    } else if (baseline_method == "polynomial") {

      safely_execute(expr = {
                              prospectr::detrend(X = spectral_matrix, wav = wavenumbers, p = 2)
                             },
                     default_value = NULL,
                     error_message = glue::glue("Polynomial baseline correction failed")) -> detrend_safe

      handle_results(safe_result   = detrend_safe,
                     error_title   = "Polynomial baseline correction failed",
                     error_hints   = c("Check for insufficient wavelength points for 2nd order polynomial",
                                       "Check for all-zero or constant spectra",
                                       "Check for collinear wavelength data",
                                       "Check for memory issues with large spectral matrix"),
                     abort_on_null = TRUE) -> spectral_matrix

    }

    baseline_time <- as.numeric(difftime(Sys.time(), baseline_start_time, units = "secs"))

    if (verbose) {
      cli::cli_text("│  └─ Complete ({round(baseline_time, 2)}s)")
    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Validate spectral data
  ## ---------------------------------------------------------------------------

  ## Check for non-finite values in spectral matrix ------------------------

  if (any(!is.finite(spectral_matrix))) {

    n_inf <- sum(is.infinite(spectral_matrix))
    n_nan <- sum(is.nan(spectral_matrix))
    n_na  <- sum(is.na(spectral_matrix))

    ## Only warn if there are actual issues (Inf/NaN, not just NA) ----------

    if (n_inf > 0 || n_nan > 0) {
      cli::cli_warn(c(
        "!" = "Non-finite values detected in spectral data",
        "i" = "Infinite: {.val {n_inf}}, NaN: {.val {n_nan}}",
        "i" = "These may cause issues in modeling"
      ))
    } else if (n_na > 0 && verbose) {
      cli::cli_alert_info("Note: {.val {n_na}} NA values in spectral matrix")
    }

  }

  ## Check for all-zero spectra (measurement failures) ---------------------

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

  ## Determine target wavenumber grid based on spectra type ----------------

  spectra_type <- attr(spectra_data, "spectra_type")

  if (is.null(spectra_type)) {
    cli::cli_warn("No spectra_type attribute found, assuming MIR")
    spectra_type <- "MIR"
  }

  if (spectra_type == "MIR") {

    ## MIR standard: 600-4000 cm⁻¹ ------------------------------------------

    target_wavenumbers <- seq(from = 600, to = 4000, by = resample_interval)

  } else if (spectra_type == "NIR") {

    ## NIR standard: use actual data range ----------------------------------

    actual_range       <- range(wavenumbers)
    target_wavenumbers <- seq(from = ceiling(actual_range[1]),
                             to = floor(actual_range[2]),
                             by = resample_interval)

  } else {

    cli::cli_abort("Unknown spectra_type: {.val {spectra_type}}")

  }

  ## Resample to regular grid using spline interpolation -------------------

  resample_start_time <- Sys.time()

  if (verbose) {
    cli::cli_text("└─ Resampling to {resample_interval} cm⁻¹ grid")
  }

  safely_execute(expr = {
                          prospectr::resample(X        = spectral_matrix,
                                             wav      = wavenumbers,
                                             new.wav  = target_wavenumbers,
                                             interpol = "spline")
                         },
                 default_value = NULL,
                 error_message = glue::glue("Spectral resampling failed")) -> resample_safe

  ## Build dynamic error hint with actual wavelength ranges ----------------

  range_hint <- glue::glue(
    "Check for target wavelength range outside original range: ",
    "Original {round(min(wavenumbers))}-{round(max(wavenumbers))} cm⁻¹, ",
    "Target {min(target_wavenumbers)}-{max(target_wavenumbers)} cm⁻¹"
  )

  handle_results(safe_result   = resample_safe,
                 error_title   = "Spectral resampling failed",
                 error_hints   = c("Check for wavelength grid mismatch between original and target",
                                   "Check for non-monotonic or duplicate wavelengths",
                                   "Check for insufficient wavelength overlap for interpolation",
                                   range_hint),
                 abort_on_null = TRUE) -> resampled_matrix

  resample_time <- as.numeric(difftime(Sys.time(), resample_start_time, units = "secs"))

  if (verbose) {
    cli::cli_text("   └─ Complete ({round(resample_time, 2)}s)")
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Reconstruct tibble with resampled data
  ## ---------------------------------------------------------------------------

  ## Convert matrix back to tibble with new wavenumber columns -------------

  input_attrs <- attributes(spectra_data)

  resampled_data <- tibble::as_tibble(resampled_matrix) %>%
    setNames(as.character(target_wavenumbers)) %>%
    tibble::add_column(Sample_ID = spectra_data$Sample_ID, .before = 1)

  ## Restore attributes from input data ------------------------------------

  attrs_to_preserve <- setdiff(names(input_attrs), c("class", "names", "row.names"))

  for (attr_name in attrs_to_preserve) {
    attr(resampled_data, attr_name) <- input_attrs[[attr_name]]
  }

  ## Display processing summary --------------------------------------------

  total_time <- as.numeric(difftime(Sys.time(), total_start_time, units = "secs"))

  if (verbose) {

    cli::cli_text("")
    cli::cli_text("{.strong Summary}")
    cli::cli_text("├─ Samples processed: {nrow(resampled_data)}")
    cli::cli_text("├─ Output wavenumbers: {length(target_wavenumbers)} standardized")
    cli::cli_text("└─ Total time: {round(total_time, 2)}s")
    cli::cli_text("")

  }

  return(resampled_data)

}
