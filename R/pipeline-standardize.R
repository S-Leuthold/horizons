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

#' Canonical-grid precision
#'
#' @description
#' Grid wavenumbers are rounded to `GRID_DIGITS` decimal places, so the
#' floating-point residue of `k * resample` never reaches a column name or a
#' numeric comparison of two grids, and a step with no finite decimal form
#' (1/3) still names its columns with six decimals rather than fifteen digits.
#' Two wavenumbers within `GRID_TOL` cm⁻¹ of each other are the same point:
#' an axis within it of the grid is already on the grid, and a grid point
#' within it of the data's end is inside the data. Both sit far below any
#' instrument's precision.
#' @noRd
GRID_DIGITS <- 6L
GRID_TOL    <- 1e-6


## =============================================================================
## Helper: canonical_grid()
## =============================================================================

#' The canonical wavenumber grid for a resolution and a range
#'
#' @description
#' Every multiple of `step` inside `[lo, hi]`, decreasing. The grid depends
#' only on `step` and the bounds, never on where a dataset's own axis starts,
#' so two sources resampled with the same arguments land on the same columns
#' (#64).
#'
#' @details
#' Points are `k * step` for integer `k`, rounded to `GRID_DIGITS` decimal
#' places (`6001 * 0.1` is `600.10000000000002` before rounding). The division
#' that finds the first and last `k` carries a 1e-9 allowance, so a bound that
#' is itself a multiple of `step` stays on the grid when `lo / step` lands a
#' hair off an integer.
#'
#' @param lo,hi `numeric(1).` The range, inclusive.
#' @param step `numeric(1).` The resolution in cm⁻¹, positive.
#'
#' @return `numeric.` The grid, decreasing; empty when no multiple of `step`
#'   falls inside the range.
#'
#' @noRd
canonical_grid <- function(lo, hi, step) {

  k_lo <- ceiling(lo / step - 1e-9)
  k_hi <- floor(hi / step + 1e-9)

  if (k_lo > k_hi) return(numeric(0))

  round(seq(k_hi, k_lo) * step, GRID_DIGITS)

}


## =============================================================================
## Helper: resample_spectra()
## =============================================================================

#' Resample spectral matrix to target resolution
#'
#' @description
#' Resamples spectra to a new wavenumber grid using spline interpolation.
#' Wraps `prospectr::resample()` with horizons conventions.
#'
#' @param spectra_matrix `matrix.` Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param current_wav `numeric.` Current wavenumber positions (column names).
#' @param target_resolution `numeric.` Target resolution in cm⁻¹ (e.g., 2).
#'   The grid is the multiples of this resolution inside the data's range
#'   (`canonical_grid()`), not a sequence stepped from the data's own
#'   maximum. Exactly one of `target_resolution` and `new_wav` must be given.
#' @param new_wav `numeric.` An explicit target grid, in any order. Used by
#'   `select_training()` to bring a pool onto the targets' axis, so the
#'   package has one resampling routine. Must lie within the range of
#'   `current_wav`; this function never extrapolates. Default: `NULL`.
#'
#' @return `list.` With elements:
#'   - `matrix`: Resampled spectral matrix
#'   - `wavelengths`: New wavenumber positions, decreasing
#'   - `n_before`: Number of wavelengths before resampling
#'   - `n_after`: Number of wavelengths after resampling
#'
#' @noRd
resample_spectra <- function(spectra_matrix,
                             current_wav,
                             target_resolution = NULL,
                             new_wav           = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Resolve the target wavenumber grid
  ## ---------------------------------------------------------------------------

  if (is.null(target_resolution) == is.null(new_wav)) {

    cli::cli_abort("Give exactly one of {.arg target_resolution} and {.arg new_wav}",
                   class = "horizons_input_error")

  }

  wn_min <- min(current_wav)
  wn_max <- max(current_wav)

  if (is.null(new_wav)) {

    ## Canonical grid inside the data's range (decreasing) ----------------------

    new_wav <- canonical_grid(wn_min, wn_max, target_resolution)

    if (length(new_wav) == 0) {

      cli::cli_abort(c(
        "No multiple of {target_resolution} cm-1 falls inside the data's range",
        "x" = "The data covers {wn_min} to {wn_max} cm-1"
      ), class = "horizons_input_error")

    }

  } else {

    ## Explicit grid: must sit inside the data's range --------------------------

    if (min(new_wav) < wn_min || max(new_wav) > wn_max) {

      cli::cli_abort(c(
        "{.arg new_wav} extends beyond the data's wavenumber range",
        "x" = "Requested {min(new_wav)} to {max(new_wav)}, data covers {wn_min} to {wn_max}",
        "i" = "Resampling never extrapolates; trim the target grid to the data"
      ), class = "horizons_input_error")

    }

  }

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
#' @param spectra_matrix `matrix.` Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param wavelengths `numeric.` Current wavenumber positions (column names).
#' @param range `numeric(2).` Min and max wavenumbers to keep, e.g., `c(600, 4000)`.
#' @param margin `logical.` Also keep the nearest wavenumber beyond each bound,
#'   where the data has one? `standardize()` sets this when it resamples next:
#'   a grid point on the bound usually falls between two of the data's points
#'   (600 between 599.74 and 601.67), and interpolating it needs both.
#'   Default `FALSE`.
#'
#' @return `list.` With elements:
#'   - `matrix`: Trimmed spectral matrix
#'   - `wavelengths`: Retained wavenumber positions
#'   - `n_before`: Number of wavelengths before trimming
#'   - `n_after`: Number of wavelengths after trimming
#'
#' @noRd
trim_spectra <- function(spectra_matrix, wavelengths, range, margin = FALSE) {

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

  ## One point beyond each bound, for the interpolation that follows ----------

  if (margin) {

    below <- which(wavelengths < wn_min)
    above <- which(wavelengths > wn_max)

    if (length(below) > 0) keep_idx <- c(keep_idx, below[which.max(wavelengths[below])])
    if (length(above) > 0) keep_idx <- c(keep_idx, above[which.min(wavelengths[above])])

    ### Back into the input's column order.
    keep_idx <- sort(keep_idx)

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
#' @param spectra_matrix `matrix.` Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param wavelengths `numeric.` Current wavenumber positions (column names).
#'
#' @return `list.` With elements:
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
#' @param spectra_matrix `matrix.` Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param wavelengths `numeric.` Wavenumber positions (column names).
#'
#' @return `matrix.` Baseline-corrected spectral matrix.
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
## Helper: sort_axis_decreasing()
## =============================================================================

#' Put an object's predictor columns in decreasing wavenumber order
#'
#' @description
#' Reorders the predictor columns among themselves, in `data$analysis` and
#' `data$role_map` alike. Names and values are untouched and every other
#' column keeps its place. `spectra()` keeps columns in the order it is given
#' them, while every step of `standardize()` and the validator assume
#' decreasing order.
#'
#' @param x `horizons_data.` The object.
#'
#' @return `list.` With elements:
#'   - `x`: The object, reordered through `set_analysis()` when anything moved
#'   - `sorted`: Whether any column moved
#'
#' @noRd
sort_axis_decreasing <- function(x) {

  analysis <- x$data$analysis
  role_map <- x$data$role_map

  pred_rows <- which(role_map$role == "predictor")
  pred_vars <- role_map$variable[pred_rows]

  ### A predictor name that is not a wavenumber leaves the order to the
  ### validator rather than being pushed to the end.
  wavenumbers <- suppressWarnings(as.numeric(gsub("^wn_", "", pred_vars)))
  axis_order  <- order(wavenumbers, decreasing = TRUE)

  if (anyNA(wavenumbers) || identical(axis_order, seq_along(pred_vars))) {

    return(list(x = x, sorted = FALSE))

  }

  ## The same slots in both tables, filled in the new order ------------------

  role_map[pred_rows, ] <- role_map[pred_rows[axis_order], ]

  cols <- names(analysis)
  cols[cols %in% pred_vars] <- pred_vars[axis_order]

  list(
    x      = set_analysis(x, analysis[, cols, drop = FALSE], role_map),
    sorted = TRUE
  )

}


## =============================================================================
## Helper: report_standardize_summary()
## =============================================================================

#' Report standardization summary to CLI
#'
#' @description
#' Prints a tree-style summary of what standardization operations were applied.
#'
#' @param operations `list.` Named list of operation results, each containing
#'   before/after counts and parameters.
#' @param n_samples `integer.` Number of samples in the data.
#' @param final_n_wavelengths `integer.` Final number of wavelengths.
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

    if (op_name == "sort") {

      msg <- "Sorting: wavenumber columns put in decreasing order"

    } else if (op_name == "trim") {

      msg <- paste0("Trimming: ", op$range[1], "-", op$range[2],
                    " cm\u207B\u00B9 (", op$n_before, " \u2192 ", op$n_after, ")")

    } else if (op_name == "resample" && op$status == "skipped") {

      msg <- paste0("Resampling: already on the ", op$resolution_after,
                    " cm\u207B\u00B9 grid, ", op$grid_range[1], "-",
                    op$grid_range[2], " (", op$n_after, "), not re-interpolated")

    } else if (op_name == "resample") {

      msg <- paste0("Resampling: ", op$resolution_before, " \u2192 ",
                    op$resolution_after, " cm\u207B\u00B9 onto ",
                    op$grid_range[1], "-", op$grid_range[2], " (",
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
#' **The wavenumber axis.** With resampling, the output axis is the canonical
#' grid: every multiple of `resample` inside the `trim` bounds, or inside the
#' data's own range when `trim` is `NULL`. It depends only on the arguments,
#' never on where an instrument's axis happens to start, so two datasets
#' standardized with the same arguments share their columns, which is what
#' `predict()` and `select_training()` compare. `resample = 4` with the
#' default trim gives `wn_4000, wn_3996, ..., wn_600` (851 columns) whether
#' the scans came at 2 cm⁻¹ from 600 or at 1.93 cm⁻¹ from 599.74. Grid
#' wavenumbers are rounded to six decimal places, so a non-integer resolution
#' such as 1.5 gives clean, stable names (`wn_601.5`).
#'
#' The spectra are interpolated onto the grid by cubic spline and never
#' extrapolated: grid points beyond the data's range are dropped with a
#' warning that says how many and where. Data already on the grid, to within
#' 1e-6 cm⁻¹, is not re-interpolated, and the console says so. Columns stored
#' in increasing order are sorted to decreasing first, on every path: a call
#' with every operation off still sorts them, and is still validated before
#' the object is marked standardized, but changes nothing else.
#'
#' The operations run in this order: sort, trim, baseline correction,
#' resampling, water-band removal. Baseline correction sees the trimmed range;
#' water bands are deleted last so the interpolation never spans their gaps.
#'
#' **Idempotence:** If the object has already been standardized, calling
#' `standardize()` again will warn and return the object unchanged. Use
#' `force = TRUE` to override (not recommended).
#'
#' @param x `horizons_data.` Object from `spectra()`.
#' @param resample `numeric or NULL.` Target resolution in cm⁻¹. Default `2`
#'   matches OSSL library resolution. The spectra are resampled onto the
#'   multiples of `resample` inside the `trim` bounds (see Details). Use
#'   `NULL` to skip resampling and keep the data's own axis.
#' @param trim `numeric(2) or NULL.` Wavenumber range to keep, inclusive.
#'   Default `c(600, 4000)` is the standard MIR range. With resampling, the
#'   bounds limit the grid, and the one data point beyond each bound is used
#'   to interpolate the grid points at the bounds and then dropped. Without
#'   resampling, columns outside the range are dropped. Use `NULL` to skip
#'   trimming.
#' @param remove_water `logical.` Remove water absorption bands
#'   (1580-1720, 3100-3700 cm⁻¹)? Default `FALSE`.
#' @param baseline `logical.` Apply convex hull baseline correction?
#'   Default `FALSE`.
#' @param force `logical.` Re-standardize even if already standardized?
#'   Default `FALSE`. Not recommended — may cause data quality issues.
#'
#' @return `horizons_data.` The input object with standardized spectra.
#'   `provenance$standardization` records the arguments, whether the spectra
#'   were actually re-interpolated (`resampled`), and the grid they sit on
#'   (`grid`: `min`, `max`, `step`, `n`; `NULL` without resampling).
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
  ## Step 1b: Put the axis in decreasing order, on every path
  ## ---------------------------------------------------------------------------
  ## spectra() keeps columns in the order it is given them. Every step below
  ## assumes decreasing wavenumbers (the baseline helper reverses its output
  ## on that assumption), and so does the validator. The no-op path sorts too:
  ## a standardized object is never an invalid one.

  sorted <- sort_axis_decreasing(x)
  x      <- sorted$x

  operations <- list()

  if (sorted$sorted) {

    operations$sort <- list(n = sum(x$data$role_map$role == "predictor"))

  }

  ## ---------------------------------------------------------------------------
  ## Step 1c: Early exit if no operations requested
  ## ---------------------------------------------------------------------------
  ## Nothing but the column order changes here: no values, no names. The
  ## object is validated before it is marked, so an invalid one stops here
  ## rather than travelling on as standardized.

  if (is.null(resample) && is.null(trim) && !remove_water && !baseline) {

    x <- validate_horizons_data(x)

    ## Still mark as standardized so downstream steps know it was evaluated -----

    x$provenance$standardization <- list(
      resample     = NULL,
      trim         = NULL,
      remove_water = FALSE,
      baseline     = FALSE,
      resampled    = FALSE,
      grid         = NULL,
      applied_at   = Sys.time()
    )

    if (length(operations) > 0) {

      report_standardize_summary(
        operations          = operations,
        n_samples           = nrow(x$data$analysis),
        final_n_wavelengths = operations$sort$n
      )

    } else {

      cat(paste0("\u251C\u2500 ", cli::style_bold("Standardizing"), "...\n"))
      cat(paste0("\u2502  \u2514\u2500 No operations applied\n"))
      cat("\u2502\n")

    }

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

  ## ---------------------------------------------------------------------------
  ## Step 3: Apply trim (if requested)
  ## ---------------------------------------------------------------------------
  ## Trim first to reduce data volume for subsequent operations. When the
  ## spectra are resampled next, the nearest point beyond each bound is kept
  ## too, so the grid points on the bounds are interpolated, not extrapolated;
  ## the resampling step drops those points again.

  if (!is.null(trim)) {

    result <- trim_spectra(spectra_matrix, wavelengths, trim,
                           margin = !is.null(resample))

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
  ## Step 5: Resample onto the canonical grid (if requested)
  ## ---------------------------------------------------------------------------
  ## After baseline correction, which runs on the data's own trimmed axis, and
  ## before water-band removal, so the interpolation never spans the deleted
  ## bands. The grid is every multiple of `resample` inside the trim bounds
  ## (the data's range when trim is NULL): it depends on the arguments alone,
  ## so two sources standardized alike share their columns (#64).

  grid_record <- NULL

  if (!is.null(resample)) {

    if (length(wavelengths) < 2) {

      cli::cli_abort("Resampling requires at least two wavelength columns")

    }

    ## The grid, from the bounds -----------------------------------------------

    bounds <- if (is.null(trim)) range(wavelengths) else range(trim)
    grid   <- canonical_grid(bounds[1], bounds[2], resample)

    ## Grid points the data does not reach are dropped, never extrapolated ----

    data_lo <- min(wavelengths)
    data_hi <- max(wavelengths)

    below <- grid[grid < data_lo - GRID_TOL]
    above <- grid[grid > data_hi + GRID_TOL]

    if (length(below) + length(above) > 0) {

      where <- c(
        if (length(below) > 0) {
          cli::format_inline("{length(below)} below the data's start at {data_lo} ({min(below)} to {max(below)} cm-1)")
        },
        if (length(above) > 0) {
          cli::format_inline("{length(above)} above the data's end at {data_hi} ({min(above)} to {max(above)} cm-1)")
        }
      )

      cli::cli_warn(c(
        "The data does not cover the {resample} cm-1 grid from {bounds[1]} to {bounds[2]} cm-1",
        "i" = "Dropped {length(below) + length(above)} grid point{?s} rather than extrapolate: {paste(where, collapse = '; ')}"
      ), class = "horizons_standardize_warning")

      grid <- grid[grid >= data_lo - GRID_TOL & grid <= data_hi + GRID_TOL]

    }

    if (length(grid) == 0) {

      cli::cli_abort(c(
        "No multiple of {resample} cm-1 between {bounds[1]} and {bounds[2]} cm-1 falls inside the data",
        "x" = "The data covers {data_lo} to {data_hi} cm-1"
      ), class = "horizons_input_error")

    }

    ## Already on the grid: keep the values, do not re-interpolate ------------

    ### The margin points trim kept lie outside the bounds and are not part of
    ### the comparison; an on-grid axis simply loses them here.

    in_bounds <- wavelengths >= bounds[1] - GRID_TOL & wavelengths <= bounds[2] + GRID_TOL
    on_grid   <- sum(in_bounds) == length(grid) &&
                 all(abs(wavelengths[in_bounds] - grid) <= GRID_TOL)

    if (on_grid) {

      spectra_matrix <- spectra_matrix[, in_bounds, drop = FALSE]
      wavelengths    <- grid

      operations$resample <- list(
        status            = "skipped",
        resolution_after  = resample,
        grid_range        = range(grid),
        n_after           = length(grid)
      )

    } else {

      ### A grid point inside GRID_TOL of the data's end is evaluated at that
      ### end, so resample_spectra()'s strict no-extrapolation check holds.

      result <- resample_spectra(spectra_matrix, wavelengths,
                                 new_wav = pmin(pmax(grid, data_lo), data_hi))

      operations$resample <- list(
        status            = "resampled",
        resolution_before = signif(stats::median(abs(diff(wavelengths))), 3),
        resolution_after  = resample,
        grid_range        = range(grid),
        n_before          = result$n_before,
        n_after           = result$n_after
      )

      spectra_matrix <- result$matrix
      wavelengths    <- grid

    }

    grid_record <- list(
      min  = min(grid),
      max  = max(grid),
      step = resample,
      n    = length(grid)
    )

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

  ## set_analysis() is the one place that recomputes the stored counts, so
  ## the predictor count is never written by hand here.

  x <- set_analysis(x, new_analysis, new_role_map)

  ## Update provenance ---------------------------------------------------------

  x$provenance$standardization <- list(
    resample         = resample,
    trim             = trim,
    remove_water     = remove_water,
    baseline         = baseline,
    resampled        = identical(operations$resample$status, "resampled"),
    grid             = grid_record,
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
