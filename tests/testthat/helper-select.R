# tests/testthat/helper-select.R
# Fixture for select_training() and its internals: a reference pool with two
# spectral families and two responses (one sparse), plus a small batch of
# targets on a coarser grid, one of which is an exact twin of a pool row.


## ---------------------------------------------------------------------------
## gaussian_family() — one family of synthetic spectra
## ---------------------------------------------------------------------------

#' Build one family of synthetic absorbance spectra
#'
#' @description
#' Spectra are a sum of Gaussian peaks at family-specific centres, so rows of
#' one family sit closer to each other than to the other family in any
#' sensible similarity space.
#'
#' @param n [Integer.] Rows to build.
#' @param wn [Numeric.] Wavenumbers, decreasing.
#' @param centres [Numeric.] Peak centres (cm-1).
#' @param noise [Numeric.] SD of the additive noise. Default: `0.01`.
#'
#' @return [Matrix.] `n` by `length(wn)`.
#' @noRd
gaussian_family <- function(n, wn, centres, noise = 0.01) {

  base <- vapply(centres, function(cc) exp(-((wn - cc) / 60)^2), numeric(length(wn)))
  base <- rowSums(base)

  amp  <- stats::runif(n, 0.6, 1.4)
  off  <- stats::runif(n, -0.05, 0.05)

  m <- outer(amp, base) + off
  m + matrix(stats::rnorm(n * length(wn), sd = noise), nrow = n)

}


## ---------------------------------------------------------------------------
## make_select_fixture() — pool + targets
## ---------------------------------------------------------------------------

#' Build the pool and targets fixture for select_training() tests
#'
#' @description
#' The pool has `n_pool` rows split evenly into two families on a 4000 to 600
#' cm-1 grid at 4 cm-1, with `clay` measured on every row and `oc` on half of
#' them. The targets are `n_targets` pool rows (half from each family),
#' resampled onto an 8 cm-1 grid with `prospectr::resample()` and then
#' perturbed with noise, except one, `twin_id`, which is left exact so its
#' reconciled pool row is identical.
#'
#' @param n_pool [Integer.] Pool rows. Default: `300`.
#' @param n_targets [Integer.] Target rows, even. Default: `8`.
#' @param seed [Integer.] Default: `1`.
#' @param target_range [Numeric, length 2.] `c(max, min)` of the target grid.
#'   Default: `c(4000, 600)`, inside the pool. Use a wider range to trigger
#'   the coverage stop.
#'
#' @return [List.] `pool` and `targets` (`horizons_data`), `twin_id`
#'   (the target that is an exact copy of a pool row), `twin_pool_id`,
#'   `family_of_target` (named integer, 1 or 2), `pool_wn`, `target_wn`.
#' @noRd
make_select_fixture <- function(n_pool       = 300,
                                n_targets    = 8,
                                seed         = 1,
                                target_range = c(4000, 600)) {

  set.seed(seed)

  ## Pool spectra ----------------------------------------------------------

  pool_wn <- seq(4000, 600, by = -4)
  n_half  <- n_pool %/% 2

  fam1 <- gaussian_family(n_half,          pool_wn, centres = c(3400, 2920, 1630, 1030))
  fam2 <- gaussian_family(n_pool - n_half, pool_wn, centres = c(3620, 2515, 1420, 870))

  pool_mat <- rbind(fam1, fam2)
  colnames(pool_mat) <- paste0("wn_", pool_wn)

  pool_ids <- sprintf("P%03d", seq_len(n_pool))
  family   <- rep(c(1L, 2L), c(n_half, n_pool - n_half))

  pool_tbl <- dplyr::bind_cols(
    tibble::tibble(sample_id = pool_ids, family = family),
    tibble::as_tibble(pool_mat)
  )

  ## spectra() and add_response() print their progress tree unconditionally;
  ## keep the test log quiet.
  utils::capture.output(pool <- spectra(pool_tbl))

  ## Responses: clay complete, oc on half the rows -------------------------

  lab <- tibble::tibble(
    sample_id = pool_ids,
    clay      = round(stats::runif(n_pool, 5, 60), 1),
    oc        = round(stats::runif(n_pool, 0.2, 6), 2)
  )
  lab$oc[sample(n_pool, n_pool %/% 2)] <- NA_real_

  utils::capture.output(pool <- add_response(pool, lab, variable = c("clay", "oc")))

  ## Targets: pool rows resampled onto a coarser grid ----------------------

  n_each     <- n_targets %/% 2
  pick       <- c(sample(seq_len(n_half), n_each),
                  sample(n_half + seq_len(n_pool - n_half), n_targets - n_each))
  target_wn  <- seq(target_range[1], target_range[2], by = -8)

  ## prospectr wants increasing axes; flip back after
  src <- pool_mat[pick, order(pool_wn), drop = FALSE]
  tgt <- prospectr::resample(X = src, wav = sort(pool_wn),
                             new.wav = sort(target_wn), interpol = "spline")
  tgt <- tgt[, rev(seq_len(ncol(tgt))), drop = FALSE]
  colnames(tgt) <- paste0("wn_", target_wn)

  ## Noise on every target but the twin
  twin_row <- 1L
  noise    <- matrix(stats::rnorm(length(tgt), sd = 0.01), nrow = nrow(tgt))
  noise[twin_row, ] <- 0
  tgt <- tgt + noise

  target_ids <- sprintf("T%02d", seq_len(n_targets))

  targets_tbl <- dplyr::bind_cols(
    tibble::tibble(sample_id = target_ids),
    tibble::as_tibble(tgt)
  )

  utils::capture.output(targets <- spectra(targets_tbl))

  family_of_target <- stats::setNames(family[pick], target_ids)

  list(
    pool             = pool,
    targets          = targets,
    twin_id          = target_ids[twin_row],
    twin_pool_id     = pool_ids[pick[twin_row]],
    family_of_target = family_of_target,
    pool_wn          = pool_wn,
    target_wn        = target_wn
  )

}
