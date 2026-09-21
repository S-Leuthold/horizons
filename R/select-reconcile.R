# R/select-reconcile.R
# The first act of select_training(): bring the reference pool onto the
# targets' wavenumber axis. The targets' grid is the reference; the pool is
# resampled to it through the same routine standardize() uses. Coverage is
# the one hard stop.


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
#' @param wn [Numeric.] Wavenumbers, any order.
#'
#' @return [List.] `range` (`c(min, max)`), `resolution` (median absolute
#'   spacing), `n`.
#' @noRd
grid_summary <- function(wn) {

  list(
    range      = c(min(wn), max(wn)),
    resolution = stats::median(abs(diff(sort(wn)))),
    n          = length(wn)
  )

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
#' `standardize()` derives its grid from each dataset's own range and
#' maximum wavenumber (horizons #64), so two sources standardized with the
#' same arguments can land on different columns. This function is where the
#' two meet, and it settles the axis in the user's favour.
#'
#' Two conditions are checked, in this order:
#'
#' * Coverage. If the pool does not span the targets' range at either end
#'   the function stops and names the end to trim. A training set narrower
#'   than the targets would break `predict()` later; shrinking the user's
#'   axis silently is worse than stopping.
#' * Resolution. Targets finer than the pool are allowed with a warning
#'   (class `horizons_select_warning`): the interpolation invents nothing,
#'   and the user would do better standardizing coarser.
#'
#' @param pool [horizons_data.] The reference pool.
#' @param targets [horizons_data.] The samples to be predicted.
#'
#' @return [List.] `matrix` (pool rows by target wavenumbers, row names
#'   `sample_id`), `wavenumbers` (the targets' grid, decreasing), and
#'   `record` with `target_grid`, `pool_grid` (each from `grid_summary()`),
#'   `operation` (`"resampled"` or `"none"`) and `warnings` (character,
#'   possibly empty).
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

  ## Coverage: the one hard stop ----------------------------------------------

  if (target_grid$range[2] > pool_grid$range[2]) {

    cli::cli_abort(c(
      "The pool does not cover the targets' high end",
      "x" = "Targets reach {target_grid$range[2]} cm-1; the pool stops at {pool_grid$range[2]}",
      "i" = "Trim the targets' high end to {pool_grid$range[2]} cm-1 or below before selecting"
    ), class = "horizons_input_error")

  }

  if (target_grid$range[1] < pool_grid$range[1]) {

    cli::cli_abort(c(
      "The pool does not cover the targets' low end",
      "x" = "Targets reach {target_grid$range[1]} cm-1; the pool stops at {pool_grid$range[1]}",
      "i" = "Trim the targets' low end to {pool_grid$range[1]} cm-1 or above before selecting"
    ), class = "horizons_input_error")

  }

  ## Same grid: nothing to do -------------------------------------------------

  if (identical(pool_wn, target_wn)) {

    return(list(
      matrix      = pm$matrix,
      wavenumbers = pool_wn,
      record      = list(target_grid = target_grid,
                         pool_grid   = pool_grid,
                         operation   = "none",
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

  rs <- resample_spectra(pm$matrix, pool_wn, new_wav = target_wn)

  m <- rs$matrix
  dimnames(m) <- list(rownames(pm$matrix), NULL)

  list(
    matrix      = m,
    wavenumbers = rs$wavelengths,
    record      = list(target_grid = target_grid,
                       pool_grid   = pool_grid,
                       operation   = "resampled",
                       warnings    = warnings)
  )

}
