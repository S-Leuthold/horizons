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

#' Decimal places canonical-grid wavenumbers are rounded to (clean names)
#' @noRd
GRID_DIGITS <- 6L

#' Tolerance (cm⁻¹) within which two wavenumbers are the same grid point
#' @noRd
GRID_TOL <- 1e-6


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

#' Resample spectral matrix onto a wavenumber grid
#'
#' @description
#' Resamples spectra to a new wavenumber grid using spline interpolation.
#' Wraps `prospectr::resample()` with horizons conventions.
#'
#' @param spectra_matrix `matrix.` Numeric matrix with samples as rows,
#'   wavelengths as columns.
#' @param current_wav `numeric.` Current wavenumber positions (column names).
#' @param new_wav `numeric.` The target grid, in any order. Used by
#'   `standardize()` for each contiguous run of the data, and by
#'   `select_training()` to put the targets on the library's grid for its
#'   search, so the package has one resampling routine. Every caller builds
#'   it inside the range of `current_wav`, since this function never
#'   extrapolates; one outside it is a bug, refused as an internal error.
#'
#' @return `list.` With elements:
#'   - `matrix`: Resampled spectral matrix
#'   - `wavelengths`: New wavenumber positions, decreasing
#'   - `n_before`: Number of wavelengths before resampling
#'   - `n_after`: Number of wavelengths after resampling
#'
#' @noRd
resample_spectra <- function(spectra_matrix, current_wav, new_wav) {

  ## ---------------------------------------------------------------------------
  ## Step 1: The grid sits inside the data's range
  ## ---------------------------------------------------------------------------

  wn_min <- min(current_wav)
  wn_max <- max(current_wav)

  ### Without this the spline would extrapolate silently: a wrong value.
  if (min(new_wav) < wn_min || max(new_wav) > wn_max) {

    abort_horizons(c(
      "{.arg new_wav} extends beyond the data's wavenumber range.",
      "x" = "Requested {min(new_wav)} to {max(new_wav)}, data covers {wn_min} to {wn_max}."
    ), kind = "internal")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Resample using prospectr
  ## ---------------------------------------------------------------------------

  ## prospectr::resample expects wavelengths in increasing order ---------------

  current_wav_sorted <- sort(current_wav)
  spectra_sorted     <- spectra_matrix[, order(current_wav), drop = FALSE]

  ### prospectr cannot return a single grid point (it builds one row across
  ### the samples and fails to name it), so a lone point is evaluated twice
  ### and one copy kept.
  eval_wav <- sort(new_wav)
  if (length(eval_wav) == 1) eval_wav <- rep(eval_wav, 2)

  resampled <- prospectr::resample(
    X        = spectra_sorted,
    wav      = current_wav_sorted,
    new.wav  = eval_wav,
    interpol = "spline"
  )

  resampled <- resampled[, seq_along(new_wav), drop = FALSE]

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
## Helper: resample_onto_grid()
## =============================================================================

#' Resample each spectrum onto the grid from its own points
#'
#' @description
#' The axis rule `standardize()` applies to the data, applied to each
#' spectrum's own finite points: a grid point beyond the points a spectrum
#' has, or inside a hole in them (a spacing over three times the median and
#' over 30 cm-1, `grid_summary()`'s rule), is `NA` for it, never extrapolated
#' or filled, and each contiguous run of its points is interpolated on its
#' own, so no spline crosses a hole. A single missing point is no hole and is
#' interpolated over. `prospectr::resample()` would otherwise drop a
#' spectrum's missing values silently and fit through or past them.
#'
#' Spectra that share a pattern of finite values are resampled together, so
#' a batch with no missing value is one call per run, the same call the
#' whole batch always had.
#'
#' @param M `matrix.` Spectra as rows, on `wavelengths`.
#' @param wavelengths `numeric.` The data's axis.
#' @param grid `numeric.` The grid, already inside the data's range and
#'   clear of the data's gaps.
#'
#' @return `matrix.` `nrow(M)` by `length(grid)`, `NA` where a spectrum's own
#'   points do not reach.
#'
#' @noRd
resample_onto_grid <- function(M, wavelengths, grid) {

  out    <- matrix(NA_real_, nrow = nrow(M), ncol = length(grid))
  finite <- is.finite(M)

  ## Rows sharing a pattern of finite values ----------------------------------

  complete <- rowSums(!finite) == 0
  key      <- rep("complete", nrow(M))

  if (!all(complete)) {

    key[!complete] <- apply(finite[!complete, , drop = FALSE], 1, function(r) paste(which(r), collapse = ","))

  }

  for (pattern in unique(key)) {

    rows <- which(key == pattern)
    cols <- which(finite[rows[1], ])

    ### A spline needs two points
    if (length(cols) < 2) next

    wn <- wavelengths[cols]

    ## The grid points these spectra's own points reach ------------------------

    gaps  <- grid_summary(wn)$gaps
    reach <- grid >= min(wn) - GRID_TOL & grid <= max(wn) + GRID_TOL

    for (i in seq_len(if (is.null(gaps)) 0L else nrow(gaps))) {

      reach <- reach & !(grid > gaps[i, "low"] + GRID_TOL & grid < gaps[i, "high"] - GRID_TOL)

    }

    ## One contiguous run at a time; a point's run is the number of gaps below it

    gap_highs <- if (is.null(gaps)) numeric(0) else gaps[, "high"]
    run_of    <- function(w) vapply(w, function(v) sum(gap_highs <= v + GRID_TOL), integer(1))

    data_run <- run_of(wn)
    grid_run <- run_of(grid)

    for (run in unique(grid_run[reach])) {

      in_run <- which(data_run == run)
      at     <- which(reach & grid_run == run)

      if (length(in_run) == 1) {

        ### A lone knot between two gaps: the only grid point it can carry is
        ### the knot itself.
        out[rows, at] <- M[rows, cols[in_run]]
        next

      }

      ### A grid point inside GRID_TOL of the run's end is evaluated at that
      ### end, so resample_spectra()'s strict no-extrapolation check holds.
      run_wn <- wn[in_run]
      piece  <- resample_spectra(M[rows, cols[in_run], drop = FALSE], run_wn,
                                 new_wav = pmin(pmax(grid[at], min(run_wn)), max(run_wn)))

      out[rows, at] <- piece$matrix

    }

  }

  out

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
#'   (600 between 599.74 and 601.67), and interpolating it needs both. It is
#'   each spectrum's nearest point with a value, so spectra read on different
#'   grids each keep their own. Default `FALSE`.
#'
#' @return `list.` With elements:
#'   - `matrix`: Trimmed spectral matrix
#'   - `wavelengths`: Retained wavenumber positions
#'   - `n_before`: Number of wavelengths before trimming
#'   - `n_after`: Number of wavelengths returned, margin points included
#'   - `n_inside`: Number of those inside the range, the count to report
#'
#' @noRd
trim_spectra <- function(spectra_matrix, wavelengths, range, margin = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Find columns within range
  ## ---------------------------------------------------------------------------

  wn_min <- min(range)
  wn_max <- max(range)

  ### standardize() has checked that the range holds a wavelength.
  keep_idx <- which(wavelengths >= wn_min & wavelengths <= wn_max)

  ## One point beyond each bound, for the interpolation that follows ----------

  if (margin) {

    ### Each spectrum's own nearest point, nearest first: a spectrum missing
    ### the axis's nearest point (one read on another grid) keeps its own, so
    ### it is resampled from its own points. With no value missing, every
    ### spectrum's is the axis's nearest point.
    nearest_finite <- function(idx) {
      if (length(idx) == 0) return(integer(0))
      has <- is.finite(spectra_matrix[, idx, drop = FALSE])
      any <- rowSums(has) > 0
      unique(idx[max.col(has, ties.method = "first")[any]])
    }

    below <- which(wavelengths < wn_min)
    above <- which(wavelengths > wn_max)

    keep_idx <- c(keep_idx,
                  nearest_finite(below[order(wavelengths[below], decreasing = TRUE)]),
                  nearest_finite(above[order(wavelengths[above])]))

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
    n_after     = length(trimmed_wav),
    n_inside    = sum(trimmed_wav >= wn_min & trimmed_wav <= wn_max)
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
#' @param call The frame a refusal reports: `standardize()`'s.
#'
#' @return `list.` With elements:
#'   - `matrix`: Spectral matrix with water bands removed
#'   - `wavelengths`: Retained wavenumber positions
#'   - `n_before`: Number of wavelengths before removal
#'   - `n_after`: Number of wavelengths after removal
#'   - `n_removed`: Number of wavelengths removed
#'
#' @noRd
remove_water_bands <- function(spectra_matrix, wavelengths, call = rlang::caller_env()) {

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

    abort_horizons(c(
      "Water band removal would delete all wavelength data.",
      "i" = "The axis left by the trim and the resampling lies wholly inside the water bands (1580 to 1720 and 3100 to 3700 cm-1).",
      "i" = "Use {.code remove_water = FALSE}, or a wider {.arg trim}."
    ), kind = "input", call = call)

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
#' @param call The frame a failure reports: `standardize()`'s.
#'
#' @details
#' `prospectr::baseline()` fits every row in one call and fails the whole
#' call on a single non-finite value, so `standardize()` passes only the
#' rows it has not flagged. A failure on those rows hits every one of them
#' alike, so it stops the call, with prospectr's error as its parent.
#'
#' @return `matrix.` Baseline-corrected spectral matrix, the input's shape,
#'   a single spectrum included.
#'
#' @noRd
apply_baseline_correction <- function(spectra_matrix, wavelengths, call = rlang::caller_env()) {

  ## ---------------------------------------------------------------------------
  ## Apply prospectr baseline correction
  ## ---------------------------------------------------------------------------

  ## prospectr::baseline expects wavelengths in increasing order ---------------

  wav_sorted     <- sort(wavelengths)
  spectra_sorted <- spectra_matrix[, order(wavelengths), drop = FALSE]

  corrected <- rlang::try_fetch(
    prospectr::baseline(
      X   = spectra_sorted,
      wav = wav_sorted
    ),
    error = function(e) {

      abort_horizons(c(
        "Baseline correction failed on the {nrow(spectra_sorted)} spectr{?um/a} it was given.",
        "i" = "The convex hull is fitted to every spectrum in one call, so a failure there stops the call."
      ), kind = "run", parent = e, call = call)

    }
  )

  ## One spectrum comes back from prospectr as a vector -------------------------

  ### prospectr::baseline() drops the edge columns it pads with by indexing
  ### without drop = FALSE, so a one-row input returns a named vector and the
  ### reorder below fails on its dimensions (#78). The hull is fitted row by
  ### row, so the row is put back as a 1 x p matrix, named by wavenumber the
  ### way prospectr names a matrix result.
  if (!is.matrix(corrected)) {

    corrected <- matrix(corrected, nrow = 1,
                        dimnames = list(rownames(spectra_sorted), names(corrected)))

  }

  ## Restore the input's own column order ---------------------------------------

  ### Column i of the input is the rank(wavelengths)[i]-th of the sorted
  ### result, whatever order the caller passed.
  corrected <- corrected[, order(order(wavelengths)), drop = FALSE]

  corrected

}


## =============================================================================
## Helper: unusable_spectra() and flag_unusable()
## =============================================================================

#' Which spectra cannot be used, and why
#'
#' @description
#' The one rule `standardize()` and `step_transform_spectra()`'s bake share
#' (decision 5), so a spectrum gets the same answer whichever way it comes.
#' A spectrum is unusable when any value is not finite, or when it has no
#' spread: every value equal, as `prospectr`'s own SNV check reads it.
#' Under prospectr 0.2.10 SNV turns such a spectrum into zeros rather than
#' `NaN`, so the second test cannot be left to a non-finite value appearing
#' later.
#'
#' @param M Numeric matrix, one spectrum per row.
#' @return Character, one per row: `NA` for a usable spectrum,
#'   `"non-finite"` or `"no spread"`.
#' @keywords internal
#' @noRd
unusable_spectra <- function(M) {

  M      <- as.matrix(M)
  reason <- rep(NA_character_, nrow(M))

  non_finite <- rowSums(!is.finite(M)) > 0

  ### Every value equal to the row's first: M[, 1] recycles down each column
  flat <- !non_finite & rowSums(M != M[, 1]) == 0

  reason[non_finite] <- "non-finite"
  reason[flat]       <- "no spread"

  reason

}

#' Flag the spectra standardize() could not use
#'
#' @description
#' Keeps every row: a spectrum that cannot be used is marked in `.flag`
#' (`"standardize"`), the column the modelling verbs read through
#' [flagged_rows()], created only when a row is flagged, and placed ahead
#' of the predictors. A row an earlier step flagged keeps its flag; the
#' caller leaves it out of `reason`. One `flagged` warning counts the rows
#' by reason and names the first of them.
#'
#' @param analysis,role_map The table and its roles.
#' @param reason Character, one per row, from [unusable_spectra()]: `NA` for
#'   a row left as it is.
#' @param collector `standardize()`'s collector, from [log_open()].
#' @return List: `analysis` and `role_map`.
#' @keywords internal
#' @noRd
flag_unusable <- function(analysis, role_map, reason, collector) {

  flag <- !is.na(reason)

  if (!any(flag)) return(list(analysis = analysis, role_map = role_map))

  ## The column, ahead of the predictors, when no step has flagged a row yet --

  if (!".flag" %in% names(analysis)) {

    preds <- role_map$variable[role_map$role == "predictor"]
    at    <- match(TRUE, names(analysis) %in% preds, nomatch = ncol(analysis) + 1L)
    at_rm <- match(TRUE, role_map$variable %in% preds, nomatch = nrow(role_map) + 1L)

    analysis <- dplyr::bind_cols(
      analysis[, seq_len(at - 1L), drop = FALSE],
      tibble::tibble(.flag = rep(NA_character_, nrow(analysis))),
      analysis[, setdiff(seq_len(ncol(analysis)), seq_len(at - 1L)), drop = FALSE]
    )

    role_map <- dplyr::bind_rows(
      role_map[seq_len(at_rm - 1L), , drop = FALSE],
      tibble::tibble(variable = ".flag", role = "meta"),
      role_map[setdiff(seq_len(nrow(role_map)), seq_len(at_rm - 1L)), , drop = FALSE]
    )

  }

  analysis$.flag[flag] <- "standardize"

  ## One warning, counting by reason ------------------------------------------

  ids        <- analysis$sample_id
  n          <- sum(flag)
  non_finite <- ids[flag & reason %in% "non-finite"]
  flat       <- ids[flag & reason %in% "no spread"]
  n_nf       <- sum(flag & reason %in% "non-finite")
  n_flat     <- sum(flag & reason %in% "no spread")

  log_warning(collector, "flagged", c(
    "{n} spectr{?um/a} could not be used and {?is/are} flagged ({.field .flag}); the modelling verbs leave {?it/them} out.",
    "i" = if (n_nf > 0) "{n_nf} {?has/have} a value that is not finite: {shown_ids(non_finite)}.",
    "i" = if (n_flat > 0) "{n_flat} {?has/have} no spread (every value equal): {shown_ids(flat)}."
  ), ids = ids[flag], n = n)

  list(analysis = analysis, role_map = role_map)

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

    if (op_name == "restandardize") {

      msg <- "Re-standardizing (force = TRUE): the earlier record is replaced"

    } else if (op_name == "sort") {

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
#' data's own range when `trim` is `NULL`. The grid points never depend on
#' where an instrument's axis happens to start. With `trim` given, two
#' datasets that cover the trim range and are standardized with the same
#' arguments share all their columns, which is what `predict()` and
#' `select_training()` compare. With `trim = NULL` each dataset keeps its own
#' extent, so two sources share columns only where their ranges overlap; pass
#' `trim` when you need a fixed extent. `resample = 4` with the default trim
#' gives `wn_4000, wn_3996, ..., wn_600` (851 columns) whether the scans came
#' at 2 cm⁻¹ from 600 or at about 1.93 cm⁻¹ from 599.74. Grid wavenumbers are
#' rounded to six decimal places, so a non-integer resolution such as 1.5
#' gives clean, stable names (`wn_601.5`).
#'
#' The spectra are interpolated onto the grid by cubic spline and never
#' extrapolated: grid points beyond the data's range are dropped with a
#' warning that says how many and where. Each spectrum is resampled from its
#' own finite points: one with missing values gets `NA` at a grid point
#' beyond the points it has, or inside a hole in them, rather than a value
#' extrapolated or filled in, and is flagged (see Unusable spectra). A single
#' missing point is no hole, so it is interpolated over; on data already on
#' the grid, which is not re-interpolated, a missing value stays missing and
#' flags its spectrum. Nor is a hole in the input filled:
#' where the data's own axis has a gap (a spacing over three times the median
#' and over 30 cm⁻¹, such as a band deleted upstream or by an earlier
#' `remove_water = TRUE`), the grid points inside it are dropped with a
#' warning naming the gaps, and each side is interpolated on its own. Data
#' already on the grid, to within 1e-6 cm⁻¹ and apart from any such gaps, is
#' not re-interpolated, and the console says so. Columns stored in increasing
#' order are sorted to decreasing first, on every path: a call with every
#' operation off still sorts them, and is still validated before the object
#' is marked standardized, but changes nothing else.
#'
#' The operations run in this order: sort, trim, resampling, baseline
#' correction, water-band removal. With resampling, the baseline is fitted on
#' the canonical grid, so every source's convex hull is pinned at the same
#' ends, the grid's; the points kept beyond the trim bounds for interpolation
#' are gone by then and never reach it. Without resampling, it is fitted on
#' the trimmed native axis. Water bands are deleted last so that
#' `standardize()`'s own interpolation never spans them; a Savitzky-Golay
#' derivative taken later still runs across the hole they leave, which is a
#' separate matter (see `select_training()`'s `mask`).
#'
#' **Unusable spectra:** a spectrum `standardize()` cannot use, one with a
#' value that is not finite once it is trimmed and resampled, or one with no
#' spread (every value equal), is kept and flagged, never dropped: its
#' `.flag` is `"standardize"` (a `meta` column, created when the first row
#' is flagged), and one `horizons_warning_flagged` counts and names them.
#' [evaluate()], [fit()], [predict()] and [select_training()] leave flagged
#' spectra out. The convex hull is fitted to the spectra not flagged, and a
#' flagged spectrum keeps its values as they stood when it was flagged. The
#' rule holds with every operation off too. A row an earlier step flagged
#' keeps its flag.
#'
#' **Idempotence:** If the object has already been standardized, calling
#' `standardize()` again returns the object unchanged, with one
#' `horizons_warning_skipped`. Use `force = TRUE` to override (not
#' recommended); as you asked for it, that is reported in verbose output and
#' in a note on the log rather than warned.
#'
#' **Warnings** are collected as the call runs and raised once per topic at
#' its end, each written to the object's `log` first: grid points dropped
#' because the data does not reach them or they fall in a gap are one
#' `horizons_warning_coverage`.
#'
#' @param x `horizons_data.` Object from `spectra()`.
#' @param resample `numeric or NULL.` Target resolution in cm⁻¹. Default `2`
#'   matches OSSL library resolution. The spectra are resampled onto the
#'   multiples of `resample` inside the `trim` bounds (see Details). Use
#'   `NULL` to skip resampling and keep the data's own axis.
#' @param trim `numeric(2) or NULL.` Wavenumber range to keep, inclusive.
#'   Default `c(600, 4000)` is the standard MIR range. With resampling, the
#'   bounds limit the grid, and the one data point beyond each bound is used
#'   to interpolate the grid points at the bounds and then dropped, before
#'   baseline correction. Without resampling, columns outside the range are
#'   dropped. Use `NULL` to skip trimming.
#' @param remove_water `logical.` Remove water absorption bands
#'   (1580-1720, 3100-3700 cm⁻¹)? Default `FALSE`.
#' @param baseline `logical.` Apply convex hull baseline correction?
#'   Default `FALSE`.
#' @param force `logical.` Re-standardize even if already standardized?
#'   Default `FALSE`. Not recommended — may cause data quality issues.
#' @param verbose `logical.` Print progress messages. Default
#'   `getOption("horizons.verbose", TRUE)`, as every verb's.
#'
#' @return `horizons_data.` The input object with standardized spectra.
#'   `provenance$standardization` records the arguments, whether the spectra
#'   were actually re-interpolated (`resampled`), and the grid they sit on
#'   (`grid`: `min`, `max`, `step`, `n`; `NULL` without resampling).
#'   `resample` records the argument as given, never the axis's spacing;
#'   read that from `grid$step` or from the columns. A training set
#'   `select_training()` returns carries its library's record, with the axis
#'   fields describing the columns it returns (see [select_training()]).
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
                        force        = FALSE,
                        verbose      = getOption("horizons.verbose", TRUE)) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------
  ## One argument at a time, as every verb's door checks them.

  if (!inherits(x, "horizons_data")) {

    abort_horizons(
      "{.arg x} must be a horizons_data object, not {.obj_type_friendly {x}}.",
      kind = "input"
    )

  }

  check_arg(rlang::check_number_decimal, resample, min = 0, allow_infinite = FALSE,
            allow_null = TRUE)

  if (isTRUE(resample == 0)) {

    abort_horizons("{.arg resample} must be a positive number or {.code NULL}, not 0.",
                   kind = "input")

  }

  if (!is.null(trim) && (!is.numeric(trim) || length(trim) != 2 || anyNA(trim))) {

    abort_horizons(
      "{.arg trim} must be numeric(2) or {.code NULL}, not {.obj_type_friendly {trim}}.",
      kind = "input"
    )

  }

  if (!is.null(trim) && trim[1] >= trim[2]) {

    abort_horizons(c(
      "{.arg trim} range must be (min, max) with min < max.",
      "x" = "Got {trim[1]} and {trim[2]}."
    ), kind = "input")

  }

  check_arg(rlang::check_bool, remove_water)
  check_arg(rlang::check_bool, baseline)
  check_arg(rlang::check_bool, force)
  check_arg(rlang::check_bool, verbose)

  call      <- rlang::current_env()
  collector <- log_open("standardize")

  ## ---------------------------------------------------------------------------
  ## Step 1: Check idempotence
  ## ---------------------------------------------------------------------------

  if (!is.null(x$provenance$standardization) && !force) {

    ## Returned as it arrived, so validated as it arrived (#234). A
    ## standardized object is already on a decreasing axis.
    x <- validate_horizons_data(x, stage = "raw")

    ### Nothing ran that was asked for, which the returned object does not
    ### show
    log_warning(collector, "skipped", c(
      "Spectra already standardized, so nothing was done.",
      "i" = "Returning unchanged. Use {.code force = TRUE} to re-standardize."
    ))

    x$log <- log_close(collector, x$log)
    signal_log(collector, x)

    return(x)

  }

  operations <- list()

  if (!is.null(x$provenance$standardization) && force) {

    ### force = TRUE is the user's explicit choice: reported, not warned
    log_note(collector, "Re-standardizing previously standardized data ({.code force = TRUE}).")
    operations$restandardize <- list()

  }

  ## ---------------------------------------------------------------------------
  ## Step 1b: Put the axis in decreasing order, on every path
  ## ---------------------------------------------------------------------------
  ## spectra() already sorts on construction (#24), so this is a no-op for
  ## objects built by it; it stays here for a hand-built object arriving
  ## increasing. The steps below work on a decreasing axis (the margin, the
  ## grid comparison, the column names), and so does the validator. The
  ## no-op path sorts too: a standardized object is never an invalid one.

  sorted <- sort_axis_decreasing(x)
  x      <- sorted$x

  if (sorted$sorted) {

    operations$sort <- list(n = sum(x$data$role_map$role == "predictor"))

  }

  ## ---------------------------------------------------------------------------
  ## Step 1c: Early exit if no operations requested
  ## ---------------------------------------------------------------------------
  ## Nothing but the column order changes here: no values, no names. The
  ## object is validated (stage = "raw") before it is marked, so a
  ## structurally invalid one stops here rather than travelling on as
  ## standardized — though at this stage a duplicate or NA sample_id passes,
  ## and predictor NA is not checked at all (#24).

  if (is.null(resample) && is.null(trim) && !remove_water && !baseline) {

    x <- validate_horizons_data(x, stage = "raw")

    ## A spectrum it cannot use is flagged here too, on the axis as it is ----

    axis_vars <- x$data$role_map$variable[x$data$role_map$role == "predictor"]
    reason    <- unusable_spectra(x$data$analysis[, axis_vars, drop = FALSE])
    reason[flagged_rows(x$data$analysis)] <- NA_character_

    flagged <- flag_unusable(x$data$analysis, x$data$role_map, reason, collector)

    if (any(!is.na(reason))) x <- set_analysis(x, flagged$analysis, flagged$role_map)

    ## Still mark as standardized so downstream steps know it was evaluated -----

    ### Under force = TRUE the object may already sit on a grid an earlier
    ### call built. The axis is untouched here, so that record still holds.
    prior      <- x$provenance$standardization
    keep_prior <- !is.null(prior) && !sorted$sorted

    ### The axis as it stands, recorded as the full path records it.
    axis_wn <- wn_values(axis_vars)

    x$provenance$standardization <- list(
      resample         = NULL,
      trim             = NULL,
      remove_water     = FALSE,
      baseline         = FALSE,
      resampled        = keep_prior && isTRUE(prior$resampled),
      grid             = if (keep_prior) prior$grid else NULL,
      applied_at       = Sys.time(),
      n_wavelengths    = length(axis_vars),
      wavelength_range = if (any(is.finite(axis_wn))) range(axis_wn, na.rm = TRUE) else c(NA_real_, NA_real_)
    )

    x$log <- log_close(collector, x$log)

    if (verbose && length(operations) > 0) {

      report_standardize_summary(
        operations          = operations,
        n_samples           = nrow(x$data$analysis),
        final_n_wavelengths = length(axis_vars)
      )

    } else if (verbose) {

      cat(paste0("\u251C\u2500 ", cli::style_bold("Standardizing"), "...\n"))
      cat(paste0("\u2502  \u2514\u2500 No operations applied\n"))
      cat("\u2502\n")

    }

    signal_log(collector, x)

    return(x)

  }

  ## ---------------------------------------------------------------------------
  ## Step 1d: Structural validation before the rebuild
  ## ---------------------------------------------------------------------------
  ## Steps 7-9 below rebuild data$analysis from role_map's predictor and
  ## non-predictor columns; a column present in analysis but missing from
  ## role_map is referenced by neither set and would be silently dropped
  ## rather than carried through or refused. Catch it here, before that
  ## happens (#24).

  x <- validate_horizons_data(x, stage = "raw")

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract spectral matrix
  ## ---------------------------------------------------------------------------

  analysis <- x$data$analysis
  role_map <- x$data$role_map

  ## Get predictor columns (wavelengths) ---------------------------------------

  predictor_vars <- role_map$variable[role_map$role == "predictor"]
  spectra_matrix <- as.matrix(analysis[, predictor_vars, drop = FALSE])

  ## A row an earlier step flagged stays flagged and is not counted again;
  ## reason[] holds why this call flags a row
  prior  <- flagged_rows(analysis)
  reason <- rep(NA_character_, nrow(analysis))

  ## Extract wavenumbers from column names -------------------------------------

  wavelengths <- wn_values(predictor_vars)

  ## The trim window has to meet the data -----------------------------------

  if (!is.null(trim) && !any(wavelengths >= min(trim) & wavelengths <= max(trim))) {

    abort_horizons(c(
      "{.arg trim} does not overlap the data's axis.",
      "x" = "{.arg trim} is {min(trim)} to {max(trim)} cm-1; the data covers {min(wavelengths)} to {max(wavelengths)} cm-1."
    ), kind = "input")

  }

  data_range <- range(wavelengths)

  ## ---------------------------------------------------------------------------
  ## Step 3: Apply trim (if requested)
  ## ---------------------------------------------------------------------------
  ## Trim first to reduce data volume for subsequent operations. When the
  ## spectra are resampled next, the nearest point beyond each bound is kept
  ## too, so the grid points on the bounds are interpolated, not extrapolated;
  ## the resampling step drops those points again, before baseline correction
  ## could see them.

  if (!is.null(trim)) {

    result <- trim_spectra(spectra_matrix, wavelengths, trim,
                           margin = !is.null(resample))

    spectra_matrix <- result$matrix
    wavelengths    <- result$wavelengths

    ### The margin points are scaffolding for the interpolation and are
    ### dropped there, so the count reported is the one inside the bounds.
    operations$trim <- list(
      range    = trim,
      n_before = result$n_before,
      n_after  = result$n_inside
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Resample onto the canonical grid (if requested)
  ## ---------------------------------------------------------------------------
  ## Before baseline correction, so the hull is fitted on the grid, and before
  ## this call's water-band removal, so this interpolation never spans the
  ## bands it deletes. The grid is every multiple of `resample` inside the
  ## trim bounds (the data's range when trim is NULL): it depends on the
  ## arguments alone, so two sources standardized alike share their columns
  ## (#64). A hole already in the input (a band deleted upstream, or by an
  ## earlier standardize()) is never filled: grid points inside it are
  ## dropped, and each contiguous run is interpolated on its own.

  grid_record <- NULL

  if (!is.null(resample)) {

    if (length(wavelengths) < 2) {

      abort_horizons(c(
        "Resampling requires at least two wavelength columns.",
        "x" = "{length(wavelengths)} column{?s} {?is/are} left to resample.",
        "i" = "Use {.code resample = NULL}, or a wider {.arg trim}."
      ), kind = "input")

    }

    ## Holes in the data's own axis, by select_training()'s rule ---------------

    ### grid_summary() calls a spacing a gap when it is over three times the
    ### median and over 30 cm-1: a deleted band is an absolute width, a
    ### resolution step is a few cm-1.

    gaps <- grid_summary(wavelengths)$gaps

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

      log_warning(collector, "coverage", c(
        "The data does not cover the {resample} cm-1 grid from {bounds[1]} to {bounds[2]} cm-1.",
        "i" = "Dropped {length(below) + length(above)} grid point{?s} rather than extrapolate: {paste(where, collapse = '; ')}."
      ))

      grid <- grid[grid >= data_lo - GRID_TOL & grid <= data_hi + GRID_TOL]

    }

    ## Grid points inside a hole are dropped, never interpolated across -------

    if (!is.null(gaps)) {

      in_gap <- rep(FALSE, length(grid))

      for (i in seq_len(nrow(gaps))) {

        in_gap <- in_gap |
                  (grid > gaps[i, "low"] + GRID_TOL & grid < gaps[i, "high"] - GRID_TOL)

      }

      if (any(in_gap)) {

        gap_text <- sprintf("%g to %g", gaps[, "low"], gaps[, "high"])

        log_warning(collector, "coverage", c(
          "The data's axis has {nrow(gaps)} gap{?s} in it: {paste(gap_text, collapse = ', ')} cm-1.",
          "i" = "Dropped {sum(in_gap)} grid point{?s} that fall in a gap rather than interpolate across it; each side of a gap is resampled on its own."
        ))

        grid <- grid[!in_gap]

      }

    }

    if (length(grid) == 0) {

      abort_horizons(c(
        "No multiple of {resample} cm-1 between {bounds[1]} and {bounds[2]} cm-1 falls inside the data.",
        "x" = "The data covers {data_range[1]} to {data_range[2]} cm-1.",
        "i" = "The {.arg trim} window is narrower than one {resample} cm-1 step: widen {.arg trim} or reduce {.arg resample}."
      ), kind = "input")

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

      ### Each spectrum from its own points: one with missing values is never
      ### extrapolated or filled, the grid points it cannot reach left NA
      resampled <- resample_onto_grid(spectra_matrix, wavelengths, grid)

      operations$resample <- list(
        status            = "resampled",
        resolution_before = signif(stats::median(abs(diff(wavelengths))), 3),
        resolution_after  = resample,
        grid_range        = range(grid),
        n_before          = sum(in_bounds),
        n_after           = length(grid)
      )

      spectra_matrix <- resampled
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
  ## Step 5: Apply baseline correction (if requested)
  ## ---------------------------------------------------------------------------
  ## On the canonical grid when resampling, so every source's convex hull is
  ## pinned at the same ends, the grid's, and the margin points beyond the
  ## trim bounds (already dropped) never reach it. Without resampling, on the
  ## trimmed native axis. Before water removal: the hull needs a continuous
  ## range.

  ## A spectrum it cannot use is flagged before the hull, which would fail
  ## the whole batch on it, and keeps its values as they stand here

  before        <- unusable_spectra(spectra_matrix)
  now           <- !prior & is.na(reason) & !is.na(before)
  reason[now]   <- before[now]

  if (baseline) {

    hull <- !prior & is.na(reason)

    if (any(hull)) {

      spectra_matrix[hull, ] <- apply_baseline_correction(spectra_matrix[hull, , drop = FALSE],
                                                          wavelengths, call = call)

    }

    operations$baseline <- list(applied = TRUE)

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Remove water bands (if requested)
  ## ---------------------------------------------------------------------------
  ## Water bands last — resampling would fill gaps if done before

  if (remove_water) {

    result <- remove_water_bands(spectra_matrix, wavelengths, call = call)

    spectra_matrix <- result$matrix
    wavelengths    <- result$wavelengths

    operations$remove_water <- list(
      n_before  = result$n_before,
      n_after   = result$n_after,
      n_removed = result$n_removed
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 6b: The spectra it cannot use, once more after the hull
  ## ---------------------------------------------------------------------------
  ## Kept and flagged, never dropped: rows are removed only where modelling
  ## happens.

  after       <- unusable_spectra(spectra_matrix)
  now         <- !prior & is.na(reason) & !is.na(after)
  reason[now] <- after[now]

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

  flagged      <- flag_unusable(new_analysis, new_role_map, reason, collector)
  new_analysis <- flagged$analysis
  new_role_map <- flagged$role_map

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
  ## Raw stage: standardize() runs before average() collapses replicates, so
  ## a duplicate sample_id is still legitimate here. A non-finite value left
  ## is in a flagged row (Step 6b), which the full stage admits.

  x <- validate_horizons_data(x, stage = "raw")

  x$log <- log_close(collector, x$log)

  ## ---------------------------------------------------------------------------
  ## Step 11: Report
  ## ---------------------------------------------------------------------------

  ### Every operation requested on this path records itself, and the no-op
  ### call returned at Step 1c, so there is always something to report.

  if (verbose) {

    report_standardize_summary(
      operations          = operations,
      n_samples           = nrow(new_analysis),
      final_n_wavelengths = length(new_predictor_names)
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 12: Raise what was recorded, and return
  ## ---------------------------------------------------------------------------

  signal_log(collector, x)

  x

}
