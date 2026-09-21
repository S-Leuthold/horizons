# R/select-space.R
# The similarity space select_training() measures distances in. Built for
# "similar soil", not "similar baseline": SNV, a Savitzky-Golay derivative,
# optional masking, then a score space (PCA of the pool, or PLS against one
# property), trimmed at a noise floor. Separate from the model's
# preprocessing and never in the output.


## ---------------------------------------------------------------------------
## transform_similarity() — The spectral part of the chain
## ---------------------------------------------------------------------------

#' Apply the similarity-space spectral transform
#'
#' @description
#' SNV, then a Savitzky-Golay derivative, then masking of wavenumber ranges,
#' in that order. Masking follows the derivative so the filter never
#' straddles a gap. Every step is optional through its argument; the
#' defaults are the chain the experiments ran.
#'
#' @param M [Matrix.] Spectra, rows named by sample id, columns in the order
#'   of `wn`.
#' @param wn [Numeric.] Wavenumbers of the columns, decreasing.
#' @param snv [Logical.] Standard normal variate. Default: `TRUE`.
#' @param derivative [Integer.] Savitzky-Golay derivative order; `0` skips
#'   the filter. Default: `1L`.
#' @param window [Integer.] Filter window, odd. Default: `11L`.
#' @param poly [Integer.] Filter polynomial order. Default: `2L`.
#' @param mask [Matrix or NULL.] Two columns, one range per row, low then
#'   high; columns with a wavenumber inside any range are dropped after the
#'   derivative. Default: `NULL`. Masking is a lever, not a default: the
#'   water regions usually cited (1580-1720 and 3100-3700 cm-1) also carry
#'   the clay-OH structural stretch near 3620-3700 cm-1, so masking them
#'   removes one of the most texture-diagnostic regions from the space that
#'   chooses a texture model's neighbours.
#' @param min_cols [Integer.] Fewest columns the transform may leave, so the
#'   decomposition is not reached with too little to decompose. Default:
#'   `1L`; `build_similarity_space()` passes what its component count
#'   actually needs, never the cap on it. A mask that removes more than 90 %
#'   of the columns aborts separately, whatever `min_cols` is, because that
#'   is a mask argument that did not mean what it said.
#'
#' @return [List.] `matrix` (transformed, row names kept) and `wavenumbers`
#'   (the columns that survive the filter's edge trim and the mask).
#'   Errors of class `horizons_input_error` on a bad mask, on filter
#'   arguments the window cannot honour, when the mask leaves too few
#'   columns, or when a row goes non-finite (a constant spectrum under SNV,
#'   for instance).
#'
#' @seealso [build_similarity_space()]
#' @noRd
transform_similarity <- function(M, wn,
                                 snv        = TRUE,
                                 derivative = 1L,
                                 window     = 11L,
                                 poly       = 2L,
                                 mask       = NULL,
                                 min_cols   = 1L) {

  if (!is.null(mask)) {

    ok <- is.matrix(mask) && is.numeric(mask) && ncol(mask) == 2L &&
          all(is.finite(mask)) && all(mask[, 1] <= mask[, 2])

    if (!ok) {

      cli::cli_abort(c(
        "{.arg mask} must be a numeric matrix with two columns, low then high",
        "i" = "One wavenumber range per row, e.g. {.code rbind(c(2200, 2400))}"
      ), class = "horizons_input_error")

    }

  }

  ## -------------------------------------------------------------------------
  ## Filter arguments — the window bookkeeping below assumes all of this
  ## -------------------------------------------------------------------------

  if (derivative > 0L) {

    window <- as.integer(window)
    poly   <- as.integer(poly)

    if (window %% 2L != 1L) {

      cli::cli_abort(c(
        "{.arg window} must be odd, got {window}",
        "i" = "A Savitzky-Golay window is centred on a point, so it loses {.code (window - 1) / 2} columns at each end"
      ), class = "horizons_input_error")

    }

    if (poly >= window) {

      cli::cli_abort("{.arg poly} ({poly}) must be smaller than {.arg window} ({window})",
                     class = "horizons_input_error")

    }

    if (poly < derivative) {

      cli::cli_abort("{.arg poly} ({poly}) must be at least {.arg derivative} ({derivative}); a lower-order polynomial has no such derivative",
                     class = "horizons_input_error")

    }

    if (window > ncol(M)) {

      cli::cli_abort("{.arg window} ({window}) is wider than the {ncol(M)} column{?s} of the spectra",
                     class = "horizons_input_error")

    }

  }

  ids <- rownames(M)
  m   <- M

  if (isTRUE(snv)) m <- prospectr::standardNormalVariate(m)

  if (derivative > 0L) {

    m <- prospectr::savitzkyGolay(m, m = derivative, p = poly, w = window)
    h  <- (window - 1L) %/% 2L
    wn <- wn[(h + 1L):(length(wn) - h)]

  }

  if (!is.null(mask)) {

    drop <- rep(FALSE, length(wn))
    for (i in seq_len(nrow(mask))) drop <- drop | (wn >= mask[i, 1] & wn <= mask[i, 2])
    m  <- m[, !drop, drop = FALSE]
    wn <- wn[!drop]

    ### Separate from min_cols and checked first, because it is a different
    ### fault with a different fix: a mask that takes almost everything is a
    ### mask argument that did not mean what it said, and saying so is more
    ### use than reporting the column count it happened to leave.

    if (sum(drop) > 0.9 * length(drop)) {

      cli::cli_abort(c(
        "{nrow(mask)} masked range{?s} removed {sum(drop)} of the {length(drop)} columns of the similarity space",
        "i" = "Masking more than 90 % of the spectrum leaves nothing to measure similarity on; check the ranges are in cm-1, low then high"
      ), class = "horizons_input_error")

    }

  }

  if (ncol(m) < min_cols) {

    cli::cli_abort(c(
      "The similarity space is left with {ncol(m)} column{?s}, fewer than the {min_cols} the decomposition needs",
      "i" = "Standardize to a finer resolution, widen the wavenumber range, or ask for fewer components"
    ), class = "horizons_input_error")

  }

  bad <- which(rowSums(!is.finite(m)) > 0)

  if (length(bad)) {

    shown <- utils::head(ids[bad], 5)

    cli::cli_abort(c(
      "{length(bad)} spectr{?um/a} went non-finite in the similarity space",
      "x" = "{.val {shown}}{if (length(bad) > 5) ', ...' else ''}",
      "i" = "A constant or near-constant spectrum divides by zero under SNV; check these rows"
    ), class = "horizons_input_error")

  }

  dimnames(m) <- list(ids, NULL)

  list(matrix = m, wavenumbers = wn)

}


## ---------------------------------------------------------------------------
## build_similarity_space() — Fit the score space on the pool
## ---------------------------------------------------------------------------

#' Build the similarity space from a reference pool
#'
#' @description
#' Runs `transform_similarity()` on the pool, then fits either a PCA of the
#' pool (unsupervised, the default) or a PLS against one property
#' (supervised, property-specific). Stores what projection needs and the
#' pool's own scores.
#'
#' @details
#' For `space = "pca"`, `ncomp` in (0, 1) retains components to that
#' proportion of variance, capped at `max_comp`; an integer retains that
#' many. For `space = "pls"`, `ncomp` must be an integer and `y` must be
#' given; the fit uses the rows where `y` is measured and every row is
#' scored by projection. `mixOmics::pls()` is used because it is already
#' the package's PLS engine.
#'
#' `sdev_floor` is the noise floor on the retained set. Cumulative variance
#' alone keeps a long tail of components whose standard deviation is one to
#' three orders below the first, and the default Mahalanobis metric divides
#' by that standard deviation, so the tail ends up carrying as much of the
#' distance as the dominant chemical axes while being mostly detector and
#' spline-ringing noise. After the variance rule has chosen its components,
#' every retained component whose sd is below `sdev_floor * sdev[1]` is
#' dropped; `sdev_floor = 0` disables the rule and restores selection by
#' variance alone. The decay is recorded either way (`sdev_ratio`), over the
#' variance rule's set rather than the floored one, so it still shows what
#' the floor removed. For `space = "pls"` the floor is recorded but never
#' applied: PLS components are chosen by count against a response, not by
#' spread.
#'
#' Scores are stored unscaled with the per-component standard deviation
#' alongside; `nearest_neighbours()` scales them when the metric asks for
#' Mahalanobis, which on these scores is Euclidean distance after dividing
#' by that standard deviation.
#'
#' @param M [Matrix.] Pool spectra, rows named by sample id.
#' @param wn [Numeric.] Wavenumbers, decreasing.
#' @param snv,derivative,window,poly,mask Passed to `transform_similarity()`.
#' @param space [Character.] `"pca"` or `"pls"`. Default: `"pca"`.
#' @param ncomp [Numeric.] Proportion of variance in (0, 1) or an integer
#'   count. Default: `0.99`.
#' @param y [Numeric or NULL.] The property for `space = "pls"`, one value
#'   per pool row, `NA` where unmeasured. Default: `NULL`.
#' @param max_comp [Integer.] Cap on components when `ncomp` is a
#'   proportion. Default: `SELECT_PCA_MAX_COMP`.
#' @param sdev_floor [Numeric.] Noise floor as a fraction of the first
#'   component's standard deviation; retained components below it are
#'   dropped. `0` disables. PCA only. Default: `0.10`.
#'
#' @return [horizons_similarity_space.] A list with `settings`,
#'   `input_wavenumbers` (what `project_similarity()` expects),
#'   `wavenumbers` (after transform), `ncomp` (retained, after the floor),
#'   `sdev` (the retained set, after the floor), `sdev_ratio`
#'   (`sdev / sdev[1]` over the *pre-floor* set, so the entries past
#'   `ncomp` are what the floor cut and how far below it they were),
#'   `sdev_floor` (the floor actually applied, so `0` for PLS; the argument
#'   as given is in `settings`), `scores` (pool, unscaled), `ncomp_variance` and
#'   `variance_retained` (PCA only; the count the variance rule chose before
#'   the floor, and the variance the final set carries), `n_fit` (PLS only),
#'   and the fitted `center` + `rotation` (PCA) or `fit` (PLS).
#'
#' @seealso [transform_similarity()], [project_similarity()]
#' @noRd
build_similarity_space <- function(M, wn,
                                   snv        = TRUE,
                                   derivative = 1L,
                                   window     = 11L,
                                   poly       = 2L,
                                   mask       = NULL,
                                   space      = c("pca", "pls"),
                                   ncomp      = 0.99,
                                   y          = NULL,
                                   max_comp   = SELECT_PCA_MAX_COMP,
                                   sdev_floor = 0.10) {

  space <- match.arg(space)

  is_prop <- is.numeric(ncomp) && length(ncomp) == 1L && ncomp > 0 && ncomp < 1
  is_int  <- is.numeric(ncomp) && length(ncomp) == 1L && ncomp >= 1 && ncomp == round(ncomp)

  if (!is_prop && !is_int) {

    cli::cli_abort("{.arg ncomp} must be a proportion in (0, 1) or a positive integer",
                   class = "horizons_input_error")

  }

  if (!is.numeric(sdev_floor) || length(sdev_floor) != 1L || is.na(sdev_floor) ||
      sdev_floor < 0 || sdev_floor >= 1) {

    cli::cli_abort("{.arg sdev_floor} must be a single number in [0, 1); {.val 0} disables the floor",
                   class = "horizons_input_error")

  }

  if (space == "pls") {

    if (is.null(y)) {

      cli::cli_abort("{.arg space = \"pls\"} needs {.arg y}, the property to fit against",
                     class = "horizons_input_error")

    }

    if (!is_int) {

      cli::cli_abort("{.arg space = \"pls\"} needs an integer {.arg ncomp}; a variance proportion has no meaning for PLS",
                     class = "horizons_input_error")

    }

    if (length(y) != nrow(M)) {

      cli::cli_abort("{.arg y} must have one value per pool row ({nrow(M)}), got {length(y)}",
                     class = "horizons_input_error")

    }

    rlang::check_installed("mixOmics", reason = "for space = \"pls\"")

  }

  ### The most components either rule could ask for; it bounds what prcomp()
  ### has to compute. It is not a floor on the columns: under a variance
  ### proportion the cap is 100 by default and the rule usually retains a
  ### tenth of that, so requiring columns against the cap would refuse a
  ### perfectly ordinary 24 cm-1 pool (132 columns after the filter's edge
  ### trim) for a cap the user never set. The floor is what the decomposition
  ### actually needs: the components asked for when that is a count, and two
  ### when it is a proportion.
  comp_cap <- as.integer(if (is_prop) max_comp else ncomp)
  min_cols <- if (is_prop) 2L else max(2L, as.integer(ncomp))

  tr <- transform_similarity(M, wn, snv = snv, derivative = derivative,
                             window = window, poly = poly, mask = mask,
                             min_cols = min_cols)

  out <- list(
    settings = list(snv = snv, derivative = as.integer(derivative),
                    window = as.integer(window), poly = as.integer(poly),
                    mask = mask, space = space, ncomp = ncomp,
                    sdev_floor = sdev_floor),
    input_wavenumbers = wn,
    wavenumbers       = tr$wavenumbers
  )

  if (space == "pca") {

    ### rank. bounds the scores and loadings returned, not sdev, so the
    ### variance denominator below is still the full decomposition.
    pca <- stats::prcomp(tr$matrix, center = TRUE, scale. = FALSE, rank. = comp_cap)
    var <- pca$sdev^2 / sum(pca$sdev^2)
    cum <- cumsum(var)

    k <- if (is_prop) min(which(cum >= ncomp)[1], max_comp, length(cum)) else min(as.integer(ncomp), length(cum))
    k <- as.integer(k)

    ## Noise floor on the retained set ----------------------------------------

    k_var <- k
    ratio <- if (pca$sdev[1] > 0) pca$sdev / pca$sdev[1] else rep(1, length(pca$sdev))

    if (sdev_floor > 0) k <- max(1L, sum(ratio[seq_len(k_var)] >= sdev_floor))

    out$ncomp             <- k
    out$ncomp_variance    <- k_var
    out$sdev              <- pca$sdev[seq_len(k)]

    ### The ratio is recorded over the variance rule's set, not the floored
    ### one. Stored post-floor it is a tautology — every entry is above the
    ### floor by construction — and the one question it exists to answer,
    ### what the floor cut and by how far, cannot be asked of it.
    out$sdev_ratio        <- ratio[seq_len(k_var)]
    out$sdev_floor        <- sdev_floor
    out$scores            <- pca$x[, seq_len(k), drop = FALSE]
    out$variance_retained <- cum[k]
    out$center            <- pca$center
    out$rotation          <- pca$rotation[, seq_len(k), drop = FALSE]

  } else {

    keep <- !is.na(y)
    X    <- tr$matrix
    colnames(X) <- paste0("v", seq_len(ncol(X)))
    k    <- as.integer(ncomp)

    fit <- mixOmics::pls(X[keep, , drop = FALSE], y[keep], ncomp = k,
                         mode = "regression", scale = FALSE)

    scores <- stats::predict(fit, newdata = X)$variates[, seq_len(k), drop = FALSE]

    sdev <- apply(scores, 2, stats::sd)

    ### The decay is recorded for audit, never applied: a PLS component is
    ### chosen by its relation to the response, not by its spread.
    out$ncomp      <- k
    out$n_fit      <- sum(keep)
    out$sdev       <- sdev
    out$sdev_ratio <- if (sdev[1] > 0) sdev / sdev[1] else rep(NA_real_, length(sdev))
    out$sdev_floor <- 0
    out$scores     <- scores
    out$fit        <- fit

  }

  dimnames(out$scores) <- list(rownames(M), NULL)

  structure(out, class = c("horizons_similarity_space", "list"))

}


## ---------------------------------------------------------------------------
## project_similarity() — Score new rows in a fitted space
## ---------------------------------------------------------------------------

#' Project spectra into a fitted similarity space
#'
#' @description
#' Applies the space's spectral transform to new rows and projects them with
#' the pool's loadings. The rows must be on the wavenumbers the space was
#' built on; that is what `reconcile_axes()` guarantees for a pool and its
#' targets.
#'
#' @param space [horizons_similarity_space.] From `build_similarity_space()`.
#' @param M [Matrix.] Spectra, rows named by sample id.
#' @param wn [Numeric.] Wavenumbers of `M`'s columns.
#'
#' @return [Matrix.] Unscaled scores, rows named as `M`, `space$ncomp`
#'   columns.
#'
#' @seealso [build_similarity_space()]
#' @noRd
project_similarity <- function(space, M, wn) {

  if (!inherits(space, "horizons_similarity_space")) {

    cli::cli_abort("{.arg space} must come from {.fn build_similarity_space}",
                   class = "horizons_input_error")

  }

  if (!isTRUE(all.equal(wn, space$input_wavenumbers))) {

    cli::cli_abort(c(
      "The rows are not on the wavenumbers the similarity space was built on",
      "x" = "Space: {length(space$input_wavenumbers)} wavenumbers from {max(space$input_wavenumbers)} to {min(space$input_wavenumbers)}; rows: {length(wn)} from {max(wn)} to {min(wn)}",
      "i" = "Reconcile the axes first ({.fn reconcile_axes})"
    ), class = "horizons_input_error")

  }

  s  <- space$settings
  tr <- transform_similarity(M, wn, snv = s$snv, derivative = s$derivative,
                             window = s$window, poly = s$poly, mask = s$mask)

  if (s$space == "pca") {

    scores <- sweep(tr$matrix, 2, space$center, "-") %*% space$rotation

  } else {

    X <- tr$matrix
    colnames(X) <- paste0("v", seq_len(ncol(X)))
    scores <- stats::predict(space$fit, newdata = X)$variates[, seq_len(space$ncomp), drop = FALSE]

  }

  dimnames(scores) <- list(rownames(M), NULL)
  scores

}
