# R/pipeline-standardize.R
# Standardization operations for spectral data (resampling, trimming, etc.)
#
# This file handles data solidification — getting spectra into consistent
# format before modeling. It does NOT apply spectral transforms like SNV
# or derivatives (those are configure() options applied during evaluate()).


## =============================================================================
## Constants
## =============================================================================

#' Water absorption band ranges (cm⁻¹)
#' @noRd
WATER_BANDS <- list(

  oh_bending    = c(1580, 1720),
  oh_stretching = c(3100, 3700)

)


## =============================================================================
## Helper: resample_spectra()
## =============================================================================

#' Resample spectral matrix to target resolution
#'
#' @description
#' Resamples spectra to a new wavenumber grid using spline interpolation.
#' Wraps `prospectr::resample()` with horizons conventions.
#'
#' @param spectra_matrix [matrix.] Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param current_wav [numeric.] Current wavenumber positions (column names).
#' @param target_resolution [numeric.] Target resolution in cm⁻¹ (e.g., 2).
#'
#' @return [list.] With elements:
#'   - `matrix`: Resampled spectral matrix
#'   - `wavelengths`: New wavenumber positions
#'   - `n_before`: Number of wavelengths before resampling
#'   - `n_after`: Number of wavelengths after resampling
#'
#' @noRd
resample_spectra <- function(spectra_matrix, current_wav, target_resolution) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Generate target wavenumber grid
  ## ---------------------------------------------------------------------------

 ## Determine range from current wavelengths ----------------------------------

  wn_min <- min(current_wav)
  wn_max <- max(current_wav)

  ## Create evenly-spaced grid at target resolution ----------------------------
  ## Sequence from max to min (decreasing) to maintain standard order

  new_wav <- seq(from = wn_max, to = wn_min, by = -target_resolution)

  ## ---------------------------------------------------------------------------
  ## Step 2: Resample using prospectr
  ## ---------------------------------------------------------------------------

  ## prospectr::resample expects wavelengths in increasing order ---------------

  current_wav_sorted <- sort(current_wav)
  spectra_sorted     <- spectra_matrix[, order(current_wav), drop = FALSE]

  resampled <- prospectr::resample(
    X        = spectra_sorted,
    wav      = current_wav_sorted,
    new.wav  = sort(new_wav),
    interpol = "spline"
  )

  ## Restore decreasing order --------------------------------------------------

  resampled <- resampled[, rev(seq_len(ncol(resampled))), drop = FALSE]
  new_wav   <- rev(sort(new_wav))

  ## ---------------------------------------------------------------------------
  ## Step 3: Return results
  ## ---------------------------------------------------------------------------

  list(
    matrix      = resampled,
    wavelengths = new_wav,
    n_before    = length(current_wav),
    n_after     = length(new_wav)
  )

}


## =============================================================================
## Helper: trim_spectra()
## =============================================================================

#' Trim spectra to wavenumber range
#'
#' @description
#' Subsets spectral matrix to keep only wavelengths within specified range.
#'
#' @param spectra_matrix [matrix.] Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param wavelengths [numeric.] Current wavenumber positions (column names).
#' @param range [numeric(2).] Min and max wavenumbers to keep, e.g., `c(600, 4000)`.
#'
#' @return [list.] With elements:
#'   - `matrix`: Trimmed spectral matrix
#'   - `wavelengths`: Retained wavenumber positions
#'   - `n_before`: Number of wavelengths before trimming
#'   - `n_after`: Number of wavelengths after trimming
#'
#' @noRd
trim_spectra <- function(spectra_matrix, wavelengths, range) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Find columns within range
  ## ---------------------------------------------------------------------------

  wn_min <- min(range)
  wn_max <- max(range)

  keep_idx <- which(wavelengths >= wn_min & wavelengths <= wn_max)

  ## Validate result --------------------------------------------------------------

  if (length(keep_idx) == 0) {

    cli::cli_abort(c(
      "Trim range does not overlap with wavelength data",
      "i" = "Requested: {wn_min}-{wn_max} cm^-1",
      "i" = "Available: {min(wavelengths)}-{max(wavelengths)} cm^-1"
    ))

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Subset matrix and wavelengths
  ## ---------------------------------------------------------------------------

  trimmed_matrix <- spectra_matrix[, keep_idx, drop = FALSE]
  trimmed_wav    <- wavelengths[keep_idx]

  ## ---------------------------------------------------------------------------
  ## Step 3: Return results
  ## ---------------------------------------------------------------------------

  list(
    matrix      = trimmed_matrix,
    wavelengths = trimmed_wav,
    n_before    = length(wavelengths),
    n_after     = length(trimmed_wav)
  )

}


## =============================================================================
## Helper: remove_water_bands()
## =============================================================================
#' Remove water absorption bands from spectra
#'
#' @description
#' Removes wavelength columns that fall within water absorption regions.
#' No interpolation is performed — columns are simply dropped.
#'
#' @param spectra_matrix [matrix.] Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param wavelengths [numeric.] Current wavenumber positions (column names).
#'
#' @return [list.] With elements:
#'   - `matrix`: Spectral matrix with water bands removed
#'   - `wavelengths`: Retained wavenumber positions
#'   - `n_before`: Number of wavelengths before removal
#'   - `n_after`: Number of wavelengths after removal
#'   - `n_removed`: Number of wavelengths removed
#'
#' @noRd
remove_water_bands <- function(spectra_matrix, wavelengths) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Identify columns in water band regions
  ## ---------------------------------------------------------------------------

  in_water_band <- rep(FALSE, length(wavelengths))

  for (band in WATER_BANDS) {

    in_water_band <- in_water_band |
                     (wavelengths >= band[1] & wavelengths <= band[2])

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Keep only non-water columns
  ## ---------------------------------------------------------------------------

  keep_idx <- which(!in_water_band)

  ## Validate result --------------------------------------------------------------

  if (length(keep_idx) == 0) {

    cli::cli_abort(c(
      "Water band removal would delete all wavelength data",
      "i" = "Data range falls entirely within water absorption regions"
    ))

  }

  filtered_matrix <- spectra_matrix[, keep_idx, drop = FALSE]
  filtered_wav    <- wavelengths[keep_idx]

  ## ---------------------------------------------------------------------------
  ## Step 3: Return results
  ## ---------------------------------------------------------------------------

  list(
    matrix      = filtered_matrix,
    wavelengths = filtered_wav,
    n_before    = length(wavelengths),
    n_after     = length(filtered_wav),
    n_removed   = sum(in_water_band)
  )

}


## =============================================================================
## Helper: apply_baseline_correction()
## =============================================================================

#' Apply convex hull baseline correction
#'
#' @description
#' Fits a baseline to each spectrum using convex hull method and subtracts it.
#' Wraps `prospectr::baseline()`.
#'
#' @param spectra_matrix [matrix.] Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param wavelengths [numeric.] Wavenumber positions (column names).
#'
#' @return [matrix.] Baseline-corrected spectral matrix.
#'
#' @noRd
apply_baseline_correction <- function(spectra_matrix, wavelengths) {

  ## ---------------------------------------------------------------------------
  ## Apply prospectr baseline correction
  ## ---------------------------------------------------------------------------

  ## prospectr::baseline expects wavelengths in increasing order ---------------

  wav_sorted     <- sort(wavelengths)
  spectra_sorted <- spectra_matrix[, order(wavelengths), drop = FALSE]

  corrected <- tryCatch(
    prospectr::baseline(
      X   = spectra_sorted,
      wav = wav_sorted
    ),
    error = function(e) {

      cli::cli_abort(c(
        "Baseline correction failed",
        "i" = "This can happen with constant or near-constant spectra",
        "x" = "Original error: {e$message}"
      ))

    }
  )

  ## Restore decreasing order --------------------------------------------------

  corrected <- corrected[, rev(seq_len(ncol(corrected))), drop = FALSE]

  corrected

}


## =============================================================================
## Helper: report_standardize_summary()
## =============================================================================

#' Report standardization summary to CLI
#'
#' @description
#' Prints a tree-style summary of what standardization operations were applied.
#'
#' @param operations [list.] Named list of operation results, each containing
#'   before/after counts and parameters.
#' @param n_samples [integer.] Number of samples in the data.
#' @param final_n_wavelengths [integer.] Final number of wavelengths.
#'
#' @return NULL (called for side effects).
#'
#' @noRd
report_standardize_summary <- function(operations, n_samples, final_n_wavelengths) {

  cat(paste0("\u251C\u2500 ", cli::style_bold("Standardizing"), "...\n"))

  op_names <- names(operations)

  for (i in seq_along(op_names)) {

    op_name <- op_names[i]
    op      <- operations[[op_name]]

    if (op_name == "trim") {

      msg <- paste0("Trimming: ", op$range[1], "-", op$range[2],
                    " cm\u207B\u00B9 (", op$n_before, " \u2192 ", op$n_after, ")")

    } else if (op_name == "resample") {

      msg <- paste0("Resampling: ", op$resolution_before, " \u2192 ",
                    op$resolution_after, " cm\u207B\u00B9 (",
                    op$n_before, " \u2192 ", op$n_after, ")")

    } else if (op_name == "remove_water") {

      msg <- paste0("Water bands: ", op$n_removed, " removed (",
                    op$n_before, " \u2192 ", op$n_after, ")")

    } else if (op_name == "baseline") {

      msg <- "Baseline correction (convex hull)"

    }

    cat(paste0("\u2502  \u251C\u2500 ", msg, "\n"))

  }

  cat(paste0("\u2502  \u2514\u2500 ", n_samples, " samples \u00D7 ",
             final_n_wavelengths, " predictors\n"))
  cat("\u2502\n")

}


## =============================================================================
## standardize() — User-Facing Function
## =============================================================================

#' Standardize spectral data
#'
#' @description
#' Applies standardization operations to get spectra into a consistent format
#' for modeling. This includes resampling to a common resolution, trimming to
#' a wavenumber range, removing water absorption bands, and baseline correction.
#'
#' @details
#' **This function handles data shape, not spectral transforms.**
#'
#' Standardization operations (handled here):
#' - Resampling to consistent resolution
#' - Trimming to wavenumber range
#' - Water band removal
#' - Baseline correction
#'
#' Spectral transforms (NOT handled here):
#' - SNV (Standard Normal Variate)
#' - Derivatives (Savitzky-Golay)
#' - Smoothing
#'
#' Spectral transforms are specified via `configure()` and applied per-config
#' during `evaluate()`. This separation allows factorial comparison of
#' preprocessing options.
#'
#' **Idempotence:** If the object has already been standardized, calling
#' `standardize()` again will warn and return the object unchanged. Use
#' `force = TRUE` to override (not recommended).
#'
#' @param x [horizons_data.] Object from `spectra()`.
#' @param resample [numeric or NULL.] Target resolution in cm⁻¹. Default `2`
#'   matches OSSL library resolution. Use `NULL` to skip resampling.
#' @param trim [numeric(2) or NULL.] Wavenumber range to keep. Default
#'   `c(600, 4000)` is the standard MIR range. Use `NULL` to skip trimming.
#' @param remove_water [logical.] Remove water absorption bands
#'   (1580-1720, 3100-3700 cm⁻¹)? Default `FALSE`.
#' @param baseline [logical.] Apply convex hull baseline correction?
#'   Default `FALSE`.
#' @param force [logical.] Re-standardize even if already standardized?
#'   Default `FALSE`. Not recommended — may cause data quality issues.
#'
#' @return [horizons_data.] The input object with standardized spectra.
#'   Provenance is updated to record what operations were applied.
#'
#' @examples
#' \dontrun{
#' # Load spectra and standardize to OSSL format
#' hd <- spectra("path/to/data.csv") |>
#'   standardize(resample = 2, trim = c(600, 4000))
#'
#' # Full standardization with water band removal
#' hd <- spectra("path/to/data.csv") |>
#'   standardize(resample = 2, trim = c(600, 4000),
#'               remove_water = TRUE, baseline = TRUE)
#' }
#'
#' @export
standardize <- function(x,
                        resample     = 2,
                        trim         = c(600, 4000),
                        remove_water = FALSE,
                        baseline     = FALSE,
                        force        = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  errors <- character()

  ## Check x is horizons_data --------------------------------------------------

 if (!inherits(x, "horizons_data")) {

    errors <- c(errors,
                cli::format_inline("{.arg x} must be a horizons_data object"))

  }

  ## Check resample ------------------------------------------------------------

  if (!is.null(resample)) {

    if (!is.numeric(resample) || length(resample) != 1 || resample <= 0) {

      errors <- c(errors,
                  cli::format_inline("{.arg resample} must be a positive number or NULL"))

    }

  }

  ## Check trim ----------------------------------------------------------------

  if (!is.null(trim)) {

    if (!is.numeric(trim) || length(trim) != 2) {

      errors <- c(errors,
                  cli::format_inline("{.arg trim} must be numeric(2) or NULL"))

    } else if (trim[1] >= trim[2]) {

      errors <- c(errors,
                  cli::format_inline("{.arg trim} range must be (min, max) with min < max"))

    }

  }

  ## Check logicals ------------------------------------------------------------

  if (!is.logical(remove_water) || length(remove_water) != 1 || is.na(remove_water)) {

    errors <- c(errors,
                cli::format_inline("{.arg remove_water} must be TRUE or FALSE"))

  }

  if (!is.logical(baseline) || length(baseline) != 1 || is.na(baseline)) {

    errors <- c(errors,
                cli::format_inline("{.arg baseline} must be TRUE or FALSE"))

  }

  if (!is.logical(force) || length(force) != 1 || is.na(force)) {

    errors <- c(errors,
                cli::format_inline("{.arg force} must be TRUE or FALSE"))

  }

  ## Report errors -------------------------------------------------------------

  if (length(errors) > 0) {

    cat(cli::col_red(cli::style_bold("! Input validation failed:\n")))

    for (i in seq_along(errors)) {

      branch <- if (i < length(errors)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_red(paste0("   ", branch, " ", errors[i], "\n")))

    }

    cat("\n")
    rlang::abort(
      paste(c("Input validation failed:", errors), collapse = "\n"),
      class = "horizons_input_error"
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Check idempotence
  ## ---------------------------------------------------------------------------

  if (!is.null(x$provenance$standardization) && !force) {

    cli::cli_warn(c(
      "Spectra already standardized",
      "i" = "Returning unchanged. Use {.arg force = TRUE} to re-standardize."
    ))

    return(x)

  }

  if (!is.null(x$provenance$standardization) && force) {

    cli::cli_warn("Re-standardizing previously standardized data (force = TRUE)")

  }

  ## ---------------------------------------------------------------------------
  ## Step 1b: Early exit if no operations requested
  ## ---------------------------------------------------------------------------

  if (is.null(resample) && is.null(trim) && !remove_water && !baseline) {

    cat(paste0("\u251C\u2500 ", cli::style_bold("Standardizing"), "...\n"))
    cat(paste0("\u2502  \u2514\u2500 No operations applied\n"))
    cat("\u2502\n")

    ## Still mark as standardized so downstream steps know it was evaluated -----

    x$provenance$standardization <- list(
      resample     = NULL,
      trim         = NULL,
      remove_water = FALSE,
      baseline     = FALSE,
      applied_at   = Sys.time()
    )

    return(x)

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract spectral matrix
  ## ---------------------------------------------------------------------------

  analysis <- x$data$analysis
  role_map <- x$data$role_map

  ## Get predictor columns (wavelengths) ---------------------------------------

  predictor_vars <- role_map$variable[role_map$role == "predictor"]
  spectra_matrix <- as.matrix(analysis[, predictor_vars, drop = FALSE])

  ## Extract wavenumbers from column names -------------------------------------

  wavelengths <- as.numeric(gsub("^wn_", "", predictor_vars))

  ## Track operations applied --------------------------------------------------

  operations <- list()

  ## ---------------------------------------------------------------------------
  ## Step 3: Apply trim (if requested)
  ## ---------------------------------------------------------------------------
  ## Trim first to reduce data volume for subsequent operations

  if (!is.null(trim)) {

    result <- trim_spectra(spectra_matrix, wavelengths, trim)

    spectra_matrix <- result$matrix
    wavelengths    <- result$wavelengths

    operations$trim <- list(
      range    = trim,
      n_before = result$n_before,
      n_after  = result$n_after
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Apply baseline correction (if requested)
  ## ---------------------------------------------------------------------------
  ## Baseline before water removal — algorithm needs continuous spectral range

  if (baseline) {

    spectra_matrix <- apply_baseline_correction(spectra_matrix, wavelengths)

    operations$baseline <- list(applied = TRUE)

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Apply resample (if requested)
  ## ---------------------------------------------------------------------------
  ## Resample before water removal — creates continuous grid for baseline
  ## Water bands removed last to avoid gaps being filled by interpolation

  if (!is.null(resample)) {

    if (length(wavelengths) < 2) {

      cli::cli_abort("Resampling requires at least two wavelength columns")

    }

    ## Estimate current resolution from wavelength spacing ---------------------

    wn_diff  <- abs(diff(sort(wavelengths)))
    current_resolution <- round(median(wn_diff), 1)

    ## Only resample if resolution differs -------------------------------------

    if (abs(current_resolution - resample) > 0.1) {

      result <- resample_spectra(spectra_matrix, wavelengths, resample)

      spectra_matrix <- result$matrix
      wavelengths    <- result$wavelengths

      operations$resample <- list(
        resolution_before = current_resolution,
        resolution_after  = resample,
        n_before          = result$n_before,
        n_after           = result$n_after
      )

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Remove water bands (if requested)
  ## ---------------------------------------------------------------------------
  ## Water bands last — resampling would fill gaps if done before

  if (remove_water) {

    result <- remove_water_bands(spectra_matrix, wavelengths)

    spectra_matrix <- result$matrix
    wavelengths    <- result$wavelengths

    operations$remove_water <- list(
      n_before  = result$n_before,
      n_after   = result$n_after,
      n_removed = result$n_removed
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 6b: Sanity check for non-finite values
  ## ---------------------------------------------------------------------------

  if (any(!is.finite(spectra_matrix))) {

    n_bad <- sum(!is.finite(spectra_matrix))
    cli::cli_abort("Standardization produced {n_bad} non-finite values")

  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Rebuild analysis tibble
  ## ---------------------------------------------------------------------------

  ## Create new column names ---------------------------------------------------

  new_predictor_names <- paste0("wn_", wavelengths)
  colnames(spectra_matrix) <- new_predictor_names

  ## Get non-predictor columns -------------------------------------------------

  non_predictor_vars <- role_map$variable[role_map$role != "predictor"]
  non_predictor_data <- analysis[, non_predictor_vars, drop = FALSE]

  ## Combine -------------------------------------------------------------------

  new_analysis <- dplyr::bind_cols(
    non_predictor_data,
    tibble::as_tibble(spectra_matrix)
  )

  ## ---------------------------------------------------------------------------
  ## Step 8: Rebuild role_map
  ## ---------------------------------------------------------------------------

  new_role_map <- tibble::tibble(
    variable = c(non_predictor_vars, new_predictor_names),
    role     = c(role_map$role[role_map$role != "predictor"],
                 rep("predictor", length(new_predictor_names)))
  )

  ## ---------------------------------------------------------------------------
  ## Step 9: Update object
  ## ---------------------------------------------------------------------------

  x$data$analysis     <- new_analysis
  x$data$role_map     <- new_role_map
  x$data$n_predictors <- length(new_predictor_names)

  ## Update provenance ---------------------------------------------------------

  x$provenance$standardization <- list(
    resample         = resample,
    trim             = trim,
    remove_water     = remove_water,
    baseline         = baseline,
    applied_at       = Sys.time(),
    n_wavelengths    = length(new_predictor_names),
    wavelength_range = c(min(wavelengths), max(wavelengths))
  )

  ## ---------------------------------------------------------------------------
  ## Step 10: Re-validate
  ## ---------------------------------------------------------------------------

  x <- validate_horizons_data(x)

  ## ---------------------------------------------------------------------------
  ## Step 11: Report
  ## ---------------------------------------------------------------------------

  if (length(operations) > 0) {

    report_standardize_summary(
      operations          = operations,
      n_samples           = nrow(new_analysis),
      final_n_wavelengths = length(new_predictor_names)
    )

  } else {

    cat(paste0("\u251C\u2500 ", cli::style_bold("Standardizing"), "...\n"))
    cat(paste0("\u2502  \u2514\u2500 No operations applied\n"))
    cat("\u2502\n")

  }

  ## ---------------------------------------------------------------------------
  ## Step 12: Return
  ## ---------------------------------------------------------------------------

  x

}
