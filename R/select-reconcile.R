# R/select-reconcile.R
# select_training()'s two axes. The search measures distances on the
# library's own grid, with the targets resampled onto it for the search only;
# the training set comes back on the library's own columns at the
# wavenumbers the targets share with it, nothing interpolated (#236). An axis
# with under two columns or a gap, and targets that share no column with the
# library, are refused at the verb's door.


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
#' @seealso [search_axis()]
#' @noRd
predictor_matrix <- function(x) {

  role_map <- x$data$role_map
  vars     <- role_map$variable[role_map$role == "predictor"]

  m <- as.matrix(x$data$analysis[, vars, drop = FALSE])
  dimnames(m) <- list(x$data$analysis$sample_id, NULL)

  list(
    matrix      = m,
    wavenumbers = predictor_wavenumbers(x)
  )

}


## ---------------------------------------------------------------------------
## predictor_wavenumbers() — The axis, without the spectra
## ---------------------------------------------------------------------------

#' The wavenumbers of a horizons_data object's predictor columns
#'
#' @description
#' The axis as `predictor_matrix()` reads it, without building the matrix,
#' which for a registered library is the whole library.
#'
#' @param x [horizons_data.] The object.
#'
#' @return [Numeric.] One wavenumber per predictor column, in column order.
#' @noRd
predictor_wavenumbers <- function(x) {

  role_map <- x$data$role_map

  as.numeric(sub("^wn_", "", role_map$variable[role_map$role == "predictor"]))

}


## ---------------------------------------------------------------------------
## grid_summary() — Range, resolution, count of a wavenumber grid
## ---------------------------------------------------------------------------

#' Summarise a wavenumber grid for the axis checks and the record
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
## check_select_axis() — refuse an axis the search cannot use
## ---------------------------------------------------------------------------

#' Stop when a wavenumber axis has under two columns or a hole in it
#'
#' @description
#' `select_training()`'s door for each axis, the targets' before the library
#' resolves and the library's after. One column has no spacing to summarise
#' and no spectrum to take a derivative along. A grid with a deleted band is
#' spline-filled across the hole when the targets are put on the library's
#' grid for the search, and differentiated across it by
#' `transform_similarity()`. Both inventions are silent: the output is smooth,
#' finite and correctly named. So the gap is a refusal, and it is checked on
#' the axis rather than on the mask, because masking happens after the
#' derivative and cannot undo a hole that was already in the input.
#'
#' @param wn [Numeric.] The axis's wavenumbers.
#' @param arg [Character.] The argument the axis belongs to, `"x"` or
#'   `"library"`.
#' @param what [Character.] What the argument is, for the message.
#' @param call The frame the refusal reports: the verb's.
#'
#' @return The axis's `grid_summary()`, invisibly. Aborts with class
#'   `horizons_error_input`.
#' @noRd
check_select_axis <- function(wn, arg, what, call = rlang::caller_env()) {

  if (length(wn) < 2L) {

    abort_horizons(c(
      "{.arg {arg}}, {what}, must have at least two wavenumbers.",
      "x" = "It has {length(wn)} predictor column{?s}.",
      "i" = "The similarity space takes a derivative along the axis, so it needs a spectrum, not a single band."
    ), kind = "input", call = call)

  }

  grid <- grid_summary(wn)

  if (isTRUE(grid$contiguous)) return(invisible(grid))

  gaps  <- grid$gaps
  shown <- utils::head(sprintf("%g to %g cm-1", gaps[, "low"], gaps[, "high"]), 3)
  more  <- if (nrow(gaps) > 3) ", ..." else ""

  abort_horizons(c(
    "The wavenumber axis of {.arg {arg}}, {what}, has {nrow(gaps)} gap{?s} in it.",
    "x" = "{shown}{more}, each more than three times and more than 30 cm-1 above the median spacing of {grid$resolution} cm-1.",
    "i" = "Resampling would spline-fill the gap and the derivative would run straight across it, inventing absorbance at every edge.",
    "i" = "Handle water bands with {.arg mask}, which masks after the derivative; never by deleting columns upstream."
  ), kind = "input", call = call)

}


## ---------------------------------------------------------------------------
## shared_columns() — the targets' wavenumbers the library has
## ---------------------------------------------------------------------------

#' Split the targets' predictor columns into those the library shares
#'
#' @description
#' The training set is the library's own columns at the wavenumbers the
#' targets share with it (#236). A column is shared when the library has a
#' predictor of the same name, the rule `predict()` applies to the same
#' columns later (`check_predictor_schema()`), so the targets can always
#' predict from a model trained on the result. `standardize()` names every
#' column `wn_<wavenumber>` on a grid rounded to six decimals, so on anything
#' it made, name and value agree.
#'
#' @param target_vars [Character.] The targets' predictor columns.
#' @param library_vars [Character.] The library's predictor columns.
#'
#' @return [List.] `shared` (the library's columns the targets also have, in
#'   the library's order) and `target_only` (the targets' columns the library
#'   lacks, in the targets' order).
#' @noRd
shared_columns <- function(target_vars, library_vars) {

  list(
    shared      = library_vars[library_vars %in% target_vars],
    target_only = target_vars[!target_vars %in% library_vars]
  )

}


## ---------------------------------------------------------------------------
## axes_aligned() — the targets on a grid the library's lines up with
## ---------------------------------------------------------------------------

#' Whether the targets' wavenumbers lie on a grid aligned with the library's
#'
#' @description
#' Two grids are aligned when every target wavenumber inside the library's
#' range lies on one lattice with the library's columns, so their shared
#' columns are the grids' common multiples, not coincidences. Either the
#' targets' columns there are all library columns (one axis, or the targets
#' a subset of it), or both axes are canonical grids, every wavenumber a
#' whole multiple of its own axis's step, which is what `standardize()`
#' makes: two such grids lie on the multiples of the greatest common divisor
#' of their steps, through the library's lowest column. A raw instrument
#' axis is neither. It can still share a few column names with a canonical
#' grid by coincidence (599.74 + 1.93 k shares 4 with a 4 cm-1 grid from 600
#' to 4000), which would otherwise train a model on those few columns.
#'
#' @details
#' The steps are not compared through their greatest common divisor
#' directly: in decimal, 1.93 and 2 have one, 0.01, and that lattice holds
#' the coincidental axis above. Asking that each axis be a canonical grid
#' gives the same answer for every pair `standardize()` can make and refuses
#' that one. A wavenumber is on its grid when it is within `GRID_TOL` of a
#' whole multiple of the axis's median spacing.
#'
#' @param target_wn [Numeric.] The targets' wavenumbers.
#' @param library_wn [Numeric.] The library's wavenumbers.
#'
#' @return [Logical(1).]
#' @noRd
axes_aligned <- function(target_wn, library_wn) {

  inside <- target_wn[target_wn >= min(library_wn) - GRID_TOL & target_wn <= max(library_wn) + GRID_TOL]

  if (all(inside %in% library_wn)) return(TRUE)

  on_own_grid <- function(wn, of = wn) {
    step <- stats::median(abs(diff(sort(of))))
    all(abs(wn - round(wn / step) * step) <= GRID_TOL)
  }

  on_own_grid(inside, of = target_wn) && on_own_grid(library_wn)

}


## ---------------------------------------------------------------------------
## check_shared_columns() — refuse targets not on the library's grid
## ---------------------------------------------------------------------------

#' Stop when the targets are not on a grid aligned with the library's
#'
#' @description
#' `select_training()`'s door for #236. Targets that share no column with
#' the library would leave a training set with no predictors, and targets on
#' an instrument's own axis that share a few columns by coincidence would
#' leave one with only those (`axes_aligned()`). Both are targets that were
#' not put on the library's grid, and the refusal names the `standardize()`
#' call that does it, built from the library's own range and step.
#'
#' @param target_vars [Character.] The targets' predictor columns.
#' @param library_vars [Character.] The library's predictor columns: the
#'   resolved pool's, or a registered library's grid before it is downloaded.
#' @param restandardize [Logical.] Whether the targets were standardized
#'   already, so the call needs `force = TRUE`.
#' @param call The frame the refusal reports: the verb's.
#'
#' @return [List.] `shared_columns()`'s result, when the targets are on an
#'   aligned grid and share at least one column. Aborts with class
#'   `horizons_error_input` otherwise.
#' @noRd
check_shared_columns <- function(target_vars, library_vars, restandardize = FALSE,
                                 call = rlang::caller_env()) {

  cols      <- shared_columns(target_vars, library_vars)
  target_wn <- as.numeric(sub("^wn_", "", target_vars))
  lib_wn    <- as.numeric(sub("^wn_", "", library_vars))

  aligned <- axes_aligned(target_wn, lib_wn)

  if (length(cols$shared) && aligned) return(cols)

  t_grid <- grid_summary(target_wn)
  l_grid <- grid_summary(lib_wn)

  t_step <- signif(t_grid$resolution, 4)
  l_step <- signif(l_grid$resolution, 4)
  remedy <- sprintf("standardize(x, resample = %s, trim = c(%s, %s)%s)",
                    l_step, l_grid$range[1], l_grid$range[2],
                    if (restandardize) ", force = TRUE" else "")

  header <- if (length(cols$shared)) {
    "{.arg x}'s wavenumbers are not on a grid aligned with {.arg library}'s."
  } else {
    "{.arg x} and {.arg library} share no wavenumber column."
  }

  n_in <- sum(target_wn >= l_grid$range[1] - GRID_TOL & target_wn <= l_grid$range[2] + GRID_TOL)

  abort_horizons(c(
    header,
    "x" = "The targets' {t_grid$n} columns run from {t_grid$range[1]} to {t_grid$range[2]} cm-1, about {t_step} cm-1 apart; the library's {l_grid$n} run from {l_grid$range[1]} to {l_grid$range[2]}, {l_step} cm-1 apart.",
    "x" = if (length(cols$shared)) "{length(cols$shared)} of the targets' {n_in} columns inside the library's range coincide with library columns; the rest lie on no grid shared with it, as an instrument's own axis does.",
    "i" = "The training set is the library's own columns at the wavenumbers the targets share with it. Put the targets on the library's grid with {.code {remedy}}, or on a multiple of its {l_step} cm-1 step."
  ), kind = "input", call = call)

}


## ---------------------------------------------------------------------------
## returned_standardization() — the library's record, for the columns kept
## ---------------------------------------------------------------------------

#' The standardization record of a training set on a subset of its columns
#'
#' @description
#' `provenance$standardization` describes the object's own axis. The
#' training set carries its library's record, and when it keeps only some of
#' the library's columns (targets on a coarser or narrower grid), the axis
#' fields are rewritten to describe the columns it has: a library at 4 cm-1
#' drawn around targets at 8 records step 8, the axis's spacing (#90), which
#' `axis_spacing_cm()` falls back to.
#'
#' @details
#' `n_wavelengths` and `wavelength_range` are read off the returned axis.
#' `grid` is `min`, `max`, `step` and `n` of the returned axis, in the order
#' `standardize()` writes them, when the library's record had a grid and the
#' returned wavenumbers are evenly spaced; otherwise `NULL`, `standardize()`'s
#' convention for an axis on no grid. The arguments (`resample`, `trim`,
#' `remove_water`, `baseline`), `resampled` and `applied_at` stay the
#' library's, since they produced the values, which are not interpolated:
#' `resample` can then differ from `grid$step`. When every library column is
#' kept, the record is the library's, unchanged; a library never standardized
#' has none, and gets none.
#'
#' @param pool_record [List or NULL.] The library's
#'   `provenance$standardization`.
#' @param wn [Numeric.] The returned columns' wavenumbers.
#' @param n_library [Integer.] How many predictor columns the library has.
#'
#' @return [List or NULL.] The record for the training set.
#' @noRd
returned_standardization <- function(pool_record, wn, n_library) {

  if (is.null(pool_record) || length(wn) == n_library) return(pool_record)

  spacing <- abs(diff(wn))
  step    <- round(stats::median(spacing), GRID_DIGITS)
  even    <- length(wn) >= 2L && all(abs(spacing - step) <= GRID_TOL)

  grid <- if (!is.null(pool_record$grid) && even) {
    list(min = min(wn), max = max(wn), step = step, n = length(wn))
  } else {
    NULL
  }

  ### Assigned by name through single brackets, so a NULL grid is kept as an
  ### entry rather than deleting the key, as standardize() writes it.
  axis <- list(grid             = grid,
               n_wavelengths    = length(wn),
               wavelength_range = c(min(wn), max(wn)))

  record <- pool_record
  record[names(axis)] <- axis

  record

}


## ---------------------------------------------------------------------------
## search_axis() — the targets on the library's grid, for the search only
## ---------------------------------------------------------------------------

#' Put the targets on the library's grid for the neighbour search
#'
#' @description
#' Distances are measured in the library's own similarity space, on the
#' library's grid, so a cached space serves every batch whatever its grid.
#' The targets are resampled onto that grid for the search only; the
#' training set returned is the library's own columns at the wavenumbers the
#' targets share with it (`shared_columns()`), never interpolated.
#'
#' Coverage decides the grid, in two tiers:
#' * `"full"`: the targets reach both ends of the library's range, to within
#'   half its spacing or less than one of their own steps, which is as close
#'   as a coarse grid can land. The search grid is the library's whole grid,
#'   the one a cached space was built on. Library columns beyond the targets'
#'   ends are sampled at the targets' endpoint.
#' * `"overlap"`: the targets fall short of an end by more, however much.
#'   The search grid is the library's columns inside the targets' range, and
#'   the space is built fresh on them. `select_training()` reports it as a
#'   `coverage` warning from `record$missing`; nothing is raised here.
#'
#' The ranges always overlap here: the verb has refused, at its door, targets
#' that share no column with the library.
#'
#' @param pool_wn [Numeric.] The library's wavenumbers.
#' @param target_m [Matrix.] Target spectra, rows named by sample id.
#' @param target_wn [Numeric.] The targets' wavenumbers.
#'
#' @return [List.] `matrix` (targets on the search grid), `wavenumbers` (the
#'   search grid, decreasing), `pool_cols` (the library columns it keeps, in
#'   order), and `record`: `mode` (`"full"` or `"overlap"`), `library_grid`
#'   and `target_grid` (from `grid_summary()`), `missing` (cm-1 short at the
#'   low and high ends, 0 where covered), and `operation` (`"none"` when the
#'   targets were already on the library's grid, else `"resampled"`).
#' @noRd
search_axis <- function(pool_wn, target_m, target_wn) {

  ### Both axes passed select_training()'s door (check_select_axis()): two
  ### columns or more, and no gap.
  library_grid <- grid_summary(pool_wn)
  target_grid  <- grid_summary(target_wn)

  half  <- library_grid$resolution / 2
  short <- c(low  = max(0, target_grid$range[1] - library_grid$range[1]),
             high = max(0, library_grid$range[2] - target_grid$range[2]))

  ## A batch on a coarse grid of its own cannot land on the library's ends:
  ## standardize()'s canonical grid at 16 cm-1 stops 8 short of 600. A
  ## shortfall under one of the batch's own steps is as close as that grid
  ## gets, so it counts as full coverage; the
  ## missing edge is held at the batch's endpoint, inside the width the
  ## derivative trims anyway.

  full_tol <- max(half, target_grid$resolution * (1 - 1e-8))
  mode     <- if (all(short <= half | short < full_tol)) "full" else "overlap"

  keep <- if (mode == "full") {
    rep(TRUE, length(pool_wn))
  } else {
    pool_wn >= target_grid$range[1] - half & pool_wn <= target_grid$range[2] + half
  }

  search_wn <- pool_wn[keep]

  if (identical(search_wn, target_wn)) {

    m <- target_m
    op <- "none"

  } else {

    ### Positions beyond the targets' own ends, within half a library step,
    ### are sampled at the targets' endpoint rather than extrapolated.

    at <- pmin(pmax(search_wn, target_grid$range[1]), target_grid$range[2])
    m  <- resample_spectra(target_m, target_wn, new_wav = at)$matrix
    dimnames(m) <- list(rownames(target_m), NULL)
    op <- "resampled"

  }

  list(
    matrix      = m,
    wavenumbers = search_wn,
    pool_cols   = which(keep),
    record      = list(mode         = mode,
                       library_grid = library_grid,
                       target_grid  = target_grid,
                       missing      = short,
                       operation    = op)
  )

}
