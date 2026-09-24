# R/select-reconcile.R
# The first act of select_training(): bring the reference pool onto the
# targets' wavenumber axis. The targets' grid is the reference; the pool is
# resampled to it through the same routine standardize() uses. A gapped axis
# and a coverage shortfall beyond half a spacing are the hard stops.


## ---------------------------------------------------------------------------
## predictor_matrix() — Spectra as a matrix with wavenumbers
## ---------------------------------------------------------------------------

#' Extract the spectral matrix of a horizons_data object
#'
#' @description
#' Pulls the predictor columns into a numeric matrix, rows named by
#' `sample_id`, and returns the wavenumbers alongside in the order the
#' columns carry them (decreasing, by the object contract).
#'
#' @param x [horizons_data.] The object.
#'
#' @return [List.] `matrix` (samples by wavenumbers, row names `sample_id`,
#'   no column names) and `wavenumbers` (numeric, decreasing).
#'
#' @seealso [reconcile_axes()]
#' @noRd
predictor_matrix <- function(x) {

  role_map <- x$data$role_map
  vars     <- role_map$variable[role_map$role == "predictor"]

  m <- as.matrix(x$data$analysis[, vars, drop = FALSE])
  dimnames(m) <- list(x$data$analysis$sample_id, NULL)

  list(
    matrix      = m,
    wavenumbers = as.numeric(sub("^wn_", "", vars))
  )

}


## ---------------------------------------------------------------------------
## grid_summary() — Range, resolution, count of a wavenumber grid
## ---------------------------------------------------------------------------

#' Summarise a wavenumber grid for the reconciliation record
#'
#' @details
#' A gap has to clear both a ratio and an absolute width. The ratio alone
#' false-positives on an axis that is gap-free but not uniformly spaced, and
#' those are ordinary: an NIR grid sampled evenly in nm is 20 cm-1 apart at
#' 1000 nm and 3 cm-1 apart at 2500 nm, a sixfold spread with no hole in it,
#' and a mid-IR axis merged from a 2 cm-1 and an 8 cm-1 source is fourfold.
#' A deleted band is an absolute width, not a ratio: the narrowest water
#' range `standardize()` removes is 140 cm-1, while a resolution step is a
#' few cm-1. So 30 cm-1 separates them with room on both sides.
#'
#' @param wn [Numeric.] Wavenumbers, any order.
#' @param gap_ratio [Numeric.] Multiple of the median spacing a gap must
#'   exceed. Default: `3`.
#' @param gap_width [Numeric.] Absolute spacing in cm-1 a gap must also
#'   exceed. Default: `30`.
#'
#' @return [List.] `range` (`c(min, max)`), `resolution` (median absolute
#'   spacing), `n`, `contiguous` (logical), `max_spacing`, and `gaps` (a
#'   two-column matrix of the intervals that exceed both tolerances, or
#'   `NULL`).
#' @noRd
grid_summary <- function(wn, gap_ratio = 3, gap_width = 30) {

  s   <- sort(wn)
  d   <- diff(s)
  med <- stats::median(d)

  wide <- if (length(d)) which(d > gap_ratio * med & d > gap_width) else integer(0)

  list(
    range       = c(min(wn), max(wn)),
    resolution  = med,
    n           = length(wn),
    contiguous  = length(wide) == 0L,
    max_spacing = if (length(d)) max(d) else NA_real_,
    gaps        = if (length(wide)) cbind(low = s[wide], high = s[wide + 1L]) else NULL
  )

}


## ---------------------------------------------------------------------------
## check_contiguous() — refuse a grid with a hole in it
## ---------------------------------------------------------------------------

#' Stop when a wavenumber grid is not contiguous
#'
#' @description
#' A grid with a deleted band passes an endpoint coverage test and then gets
#' spline-filled across the hole by `resample_spectra()`, and differentiated
#' across it by `transform_similarity()`. Both inventions are silent: the
#' output is smooth, finite and correctly named. So the gap is a refusal, and
#' it is checked on the axis rather than on the mask, because masking happens
#' after the derivative and cannot undo a hole that was already in the input.
#'
#' @param grid [List.] From `grid_summary()`.
#' @param side [Character.] `"pool"` or `"targets"`, for the message.
#'
#' @return `NULL`, invisibly. Aborts with class `horizons_input_error`.
#' @noRd
check_contiguous <- function(grid, side) {

  if (isTRUE(grid$contiguous)) return(invisible(NULL))

  gaps  <- grid$gaps
  shown <- utils::head(sprintf("%g to %g cm-1", gaps[, "low"], gaps[, "high"]), 3)

  cli::cli_abort(c(
    "The wavenumber axis of the {side} has {nrow(gaps)} gap{?s} in it",
    "x" = "{shown}{if (nrow(gaps) > 3) ', ...' else ''}, each more than three times and more than 30 cm-1 above the median spacing of {grid$resolution} cm-1",
    "i" = "Resampling would spline-fill the gap and the derivative would run straight across it, inventing absorbance at every edge",
    "i" = "Handle water bands with {.arg mask}, which masks after the derivative; never by deleting columns upstream"
  ), class = "horizons_input_error")

}


## ---------------------------------------------------------------------------
## reconcile_axes() — Put the pool on the targets' grid
## ---------------------------------------------------------------------------

#' Bring a reference pool onto the targets' wavenumber axis
#'
#' @description
#' The targets' wavenumbers are the grid. Every pool spectrum is resampled
#' onto them with `resample_spectra()`, so the training set the verb returns
#' and the targets it will predict share one axis end to end. A pool already
#' on the targets' grid is passed through unchanged.
#'
#' @details
#' `standardize()` puts every dataset on the canonical grid, the multiples of
#' `resample` inside the trim bounds (#64), so two sources standardized with
#' the same `resample` and `trim` normally arrive on the same columns and
#' pass straight through. They still can differ: targets standardized with
#' `resample = NULL` keep their instrument's own axis, a different `resample`
#' or `trim` gives a different grid, and `trim = NULL` leaves each source on
#' its own extent. This function is where the two meet, and it settles the
#' axis in the user's favour.
#'
#' Three conditions are checked, in this order:
#'
#' * Contiguity. Either axis carrying a spacing more than three times its own
#'   median *and* more than 30 cm-1 is a refusal, because a resampled spline
#'   and a Savitzky-Golay window both run straight across a hole and invent
#'   absorbance at its edges without erroring. Both conditions are needed:
#'   the ratio alone fires on an axis that is merely non-uniform, which an
#'   nm-sampled NIR grid converted to cm-1 always is. This is the ordering
#'   `standardize()` was written to avoid, and it is why water bands belong
#'   in `mask` rather than in a column deletion upstream.
#' * Coverage. If the pool does not span the targets' range at either end the
#'   function stops and names the end to trim, and says by how much. Within
#'   half the pool's median spacing the overshoot is clamped instead: the
#'   pool's endpoint is the nearest knot there is, so the overshooting column
#'   is taken there rather than extrapolated, and the clamp is recorded and
#'   warned about. Beyond that the stop stands; a training set narrower than
#'   the targets would break `predict()` later.
#' * Resolution. Targets finer than the pool are allowed with a warning
#'   (class `horizons_select_warning`): the interpolation invents nothing,
#'   and the user would do better standardizing coarser.
#'
#' @param pool [horizons_data.] The reference pool.
#' @param targets [horizons_data.] The samples to be predicted.
#'
#' @return [List.] `matrix` (pool rows by target wavenumbers, row names
#'   `sample_id`), `wavenumbers` (the targets' grid, decreasing), and
#'   `record` with `target_grid`, `pool_grid` (each from `grid_summary()`,
#'   so each carrying `resolution`, `contiguous` and any `gaps`),
#'   `operation` (`"resampled"` or `"none"`), `clamp` (`NULL`, or the
#'   overshoot at each end with the tolerance applied) and `warnings`
#'   (character, possibly empty).
#'
#' @seealso [resample_spectra()], [predictor_matrix()]
#' @noRd
reconcile_axes <- function(pool, targets) {

  for (nm in c("pool", "targets")) {

    obj <- get(nm)

    if (!inherits(obj, "horizons_data")) {

      cli::cli_abort("{.arg {nm}} must be a {.cls horizons_data}, not {.cls {class(obj)[1]}}",
                     class = "horizons_input_error")

    }

  }

  pm <- predictor_matrix(pool)
  tm <- predictor_matrix(targets)

  pool_wn   <- pm$wavenumbers
  target_wn <- tm$wavenumbers

  target_grid <- grid_summary(target_wn)
  pool_grid   <- grid_summary(pool_wn)
  warnings    <- character(0)

  ## Contiguity: before anything interpolates or differentiates ---------------

  check_contiguous(pool_grid,   "pool")
  check_contiguous(target_grid, "targets")

  ## Coverage: the one hard stop, to within half a pool spacing ---------------

  ### Since #64, standardize() puts both sides on the canonical grid, so an
  ### overshoot needs an axis it did not make: targets standardized with
  ### resample = NULL (an instrument's own axis, 3999.57 against the pool's
  ### 4000) or with a trim wider than the pool. Inside half a spacing the
  ### pool's endpoint is the nearest knot there is, so the overshooting
  ### column is resampled at that endpoint and the clamp is recorded. Beyond
  ### it, the verb still stops: a training set narrower than the targets
  ### breaks predict() later.

  tol       <- pool_grid$resolution / 2
  clamp     <- NULL
  resamp_wn <- target_wn

  high_over <- target_grid$range[2] - pool_grid$range[2]
  low_over  <- pool_grid$range[1] - target_grid$range[1]

  if (high_over > tol) {

    cli::cli_abort(c(
      "The pool does not cover the targets' high end",
      "x" = "Targets reach {target_grid$range[2]} cm-1; the pool stops at {pool_grid$range[2]}, an overshoot of {signif(high_over, 4)} cm-1",
      "i" = "Overshoots up to {signif(tol, 4)} cm-1 (half the pool's spacing) are clamped; trim the targets' high end to {pool_grid$range[2]} cm-1 or below before selecting"
    ), class = "horizons_input_error")

  }

  if (low_over > tol) {

    cli::cli_abort(c(
      "The pool does not cover the targets' low end",
      "x" = "Targets reach {target_grid$range[1]} cm-1; the pool stops at {pool_grid$range[1]}, an overshoot of {signif(low_over, 4)} cm-1",
      "i" = "Overshoots up to {signif(tol, 4)} cm-1 (half the pool's spacing) are clamped; trim the targets' low end to {pool_grid$range[1]} cm-1 or above before selecting"
    ), class = "horizons_input_error")

  }

  if (high_over > 0 || low_over > 0) {

    clamp <- list(high = if (high_over > 0) high_over else 0,
                  low  = if (low_over  > 0) low_over  else 0,
                  tolerance = tol)

    resamp_wn[resamp_wn > pool_grid$range[2]] <- pool_grid$range[2]
    resamp_wn[resamp_wn < pool_grid$range[1]] <- pool_grid$range[1]

    msg <- cli::format_inline(
      "The targets overshoot the pool by at most {signif(max(high_over, low_over), 4)} cm-1, within half the pool's {pool_grid$resolution} cm-1 spacing. The overshooting columns were taken at the pool's endpoint rather than extrapolated."
    )
    warnings <- c(warnings, msg)
    cli::cli_warn(msg, class = "horizons_select_warning")

  }

  ## Same grid: nothing to do -------------------------------------------------

  if (identical(pool_wn, target_wn)) {

    return(list(
      matrix      = pm$matrix,
      wavenumbers = pool_wn,
      record      = list(target_grid = target_grid,
                         pool_grid   = pool_grid,
                         operation   = "none",
                         clamp       = clamp,
                         warnings    = warnings)
    ))

  }

  ## Resolution: finer targets are allowed, with a warning --------------------

  if (target_grid$resolution < pool_grid$resolution) {

    msg <- cli::format_inline(
      "The targets are finer ({target_grid$resolution} cm-1) than the pool ({pool_grid$resolution} cm-1); interpolating the pool up adds no information. Standardize the targets coarser if you can."
    )
    warnings <- c(warnings, msg)
    cli::cli_warn(msg, class = "horizons_select_warning")

  }

  ## Resample the pool onto the targets' grid ---------------------------------

  rs <- resample_spectra(pm$matrix, pool_wn, new_wav = resamp_wn)

  m <- rs$matrix
  dimnames(m) <- list(rownames(pm$matrix), NULL)

  ### The returned axis is the targets' own, even when a column was clamped:
  ### the clamp moves where the pool was sampled, not what the targets are,
  ### and one axis end to end is what the rest of the verb rests on.

  list(
    matrix      = m,
    wavenumbers = target_wn,
    record      = list(target_grid = target_grid,
                       pool_grid   = pool_grid,
                       operation   = "resampled",
                       clamp       = clamp,
                       warnings    = warnings)
  )

}


## ---------------------------------------------------------------------------
## reconciled_standardization() — The pool's record, on the targets' axis
## ---------------------------------------------------------------------------

#' Rewrite a resampled pool's standardization record for its new axis
#'
#' @description
#' `provenance$standardization` describes the object's own axis. Once
#' `reconcile_axes()` has resampled the pool onto the targets' wavenumbers,
#' the pool's record describes columns the object no longer has: a pool
#' standardized at 4 cm-1, drawn around targets at 8, would record step 4 on
#' an axis spaced 8 (#90). This returns the record the resampled pool should
#' carry. Call it only when the pool was resampled; a pool already on the
#' targets' grid keeps its record as it is, because it is still true.
#'
#' @details
#' The axis fields are replaced: `grid` becomes the targets' (their own
#' record's `grid`, so `NULL` when the targets were not put on a canonical
#' grid, the convention `standardize()` uses), `n_wavelengths` and
#' `wavelength_range` are read off the new axis, and `resampled` is `TRUE`,
#' since the values were re-interpolated. The targets' `grid` is copied only
#' when its `n` and range agree with `wn`; otherwise `grid` is written
#' `NULL`, so the record describes the axis without resting on the targets'
#' record having been kept true upstream.
#'
#' The arguments stay the pool's: `resample`, `trim`, `remove_water`,
#' `baseline` and `applied_at` describe the `standardize()` call that
#' produced these values, which resampling onto another axis does not undo,
#' and a later `force = TRUE` no-op call already records its own arguments
#' beside a grid an earlier call built. So `resample` can differ from
#' `grid$step`, and `trim` from `wavelength_range`. Under `baseline = TRUE`
#' the convex hull was fitted and subtracted on the pool's grid and the
#' corrected spectra were then interpolated onto the targets', which is not
#' the same as correcting on the targets' grid, where the hull would be
#' pinned at the targets' points. Any other key the pool's record carries
#' is kept.
#'
#' The move itself is recorded under `reconciliation`: `operation`
#' (`"resampled"`), `clamp` (`NULL`, or the overshoot at each end: an end
#' column there holds the pool's endpoint value, up to half a pool spacing
#' off the grid point it is named for) and `pool`, the pool's record as it
#' stood before, whose `grid` is the pool's original grid. This is the
#' object's own axis history and travels with `provenance`; it is not
#' `selection$reconciliation`, which is the selection record's account of
#' the comparison, with the grid summaries both axes were compared on and
#' the warnings raised.
#'
#' A pool that was never standardized has no record, and gets none: the
#' verb does not mark as standardized rows that `standardize()` never saw.
#'
#' @param pool_record [List or NULL.] The pool's `provenance$standardization`.
#' @param target_record [List or NULL.] The targets'
#'   `provenance$standardization`.
#' @param wn [Numeric.] The wavenumbers the pool now sits on, the targets'.
#' @param clamp [List or NULL.] `reconcile_axes()`'s `record$clamp`.
#'
#' @return [List or NULL.] The record for the resampled pool; `NULL` when
#'   `pool_record` is.
#'
#' @seealso [reconcile_axes()]
#' @noRd
reconciled_standardization <- function(pool_record, target_record, wn, clamp) {

  if (is.null(pool_record)) return(NULL)

  ## The targets' grid, if it is the axis the pool now sits on --------------

  ### A grid whose count or ends disagree with wn describes some other axis;
  ### NULL says "no canonical grid" without claiming one.
  grid <- target_record$grid

  grid_ok <- is.list(grid) &&
             is.numeric(grid$n)   && length(grid$n)   == 1L && isTRUE(grid$n == length(wn)) &&
             is.numeric(grid$min) && length(grid$min) == 1L && isTRUE(abs(grid$min - min(wn)) <= GRID_TOL) &&
             is.numeric(grid$max) && length(grid$max) == 1L && isTRUE(abs(grid$max - max(wn)) <= GRID_TOL)

  if (!grid_ok) grid <- NULL

  ## The pool's record, its axis fields replaced -----------------------------

  ### Assigned by name through single brackets, so a NULL grid or clamp is
  ### kept as an entry rather than deleting the key, as standardize() writes
  ### grid.
  axis <- list(
    resampled        = TRUE,
    grid             = grid,
    n_wavelengths    = length(wn),
    wavelength_range = c(min(wn), max(wn)),
    reconciliation   = list(operation = "resampled", clamp = clamp, pool = pool_record)
  )

  record <- pool_record
  record[names(axis)] <- axis

  record

}
