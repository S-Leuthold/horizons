# R/pipeline-select-training.R
# Draw a training set from a reference pool around a batch of targets. The
# only place in the pipeline where library and unknowns meet before anything
# is fitted, so the reconciliation of their axes, the self-leakage check and
# the selection-time distance live here.


## =============================================================================
## select_training() — User-Facing Function
## =============================================================================

#' Pipeline: Select a Training Set From a Library
#'
#' @description
#' Draws a training set from a reference pool (a spectral library with
#' measured properties) around a batch of target spectra, and returns it as
#' an ordinary `horizons_data`. Everything downstream, `configure()`,
#' `validate()`, `evaluate()`, `fit()`, `predict()`, runs unchanged; the
#' model never learns it was trained on a selection.
#'
#' @details
#' **The rule.** For each target and each requested property, the `k`
#' nearest pool rows that have that property measured. Rows enter the
#' training set once, however many targets drew them, and are not
#' weighted. Drawing per property keeps `k` usable rows per target for
#' every property, so a sparse property is reported honestly rather than
#' silently thin.
#'
#' **Scopes.** All four draw the same per-target neighbourhoods; they differ
#' in how those are grouped into training sets, and the grouping lives in
#' the record (`x$selection$groups`), not in the shape of the return:
#'
#' * `"batch"` (default): one training set, the union over every target.
#' * `"cluster"`: the targets are clustered in the similarity space
#'   (k-means, count by silhouette unless `clusters` fixes it, clusters
#'   under `cluster_min` merged into the nearest); one group per cluster.
#' * `"sample"`: one group per target, its own `k` nearest.
#' * `"global"`: no draw; the whole pool, kept so the comparison is one
#'   argument away. `k` only sets how many rows the recorded `mean_k`
#'   averages over, so a `k` above a property's measured rows is capped
#'   there under global, where the drawing scopes refuse it.
#'
#' For `"cluster"` and `"sample"` the return is still the single union, and
#' fitting one model per group is the caller's loop over
#' `x$selection$groups` for now. Because nothing downstream reads those
#' groups yet, the twin subtraction is applied to the union under every scope
#' but `"global"`: an ordinary `configure() |> evaluate() |> fit()` on a
#' `"sample"` return would otherwise train on every target's own replicates.
#' Each group's `pool_ids` are still that target's or cluster's own draw,
#' after the subtraction.
#'
#' **The similarity space.** Distances are measured in a space built for
#' "similar soil", not "similar baseline", and it never reaches the model:
#' a user who configures `preprocessing = "raw"` still gets neighbours
#' chosen on derivative spectra. The default chain is SNV, a Savitzky-Golay
#' first derivative (40 cm-1 wide, order 2), PCA of the pool to 99 % of
#' variance with a standard-deviation floor on the components retained, and
#' Euclidean distance on the scores. Every step is an argument. A PCA
#' space and any scaling on it are defined by the population they were
#' fit on, and that population is always the whole library: one space, fit
#' on every row, with each property's draw restricted to the rows that have
#' it measured. A space per property, fit on its measured rows alone, was an
#' argument until 2026-09-30 and was removed: on KSSL it changed the drawn
#' training set by about 6 % (see the design spec).
#'
#' **Reconciliation.** The targets' wavenumbers are the grid. The pool is
#' resampled onto them through the same routine `standardize()` uses, so
#' the training set and the targets share one axis end to end. Either axis
#' carrying a hole — a spacing more than three times its own median and more
#' than 30 cm-1 — stops the verb before anything interpolates, because a
#' resampling spline and a Savitzky-Golay window both run straight across a
#' gap and invent absorbance at its edges without erroring; a deleted water
#' band belongs in `mask`, which is applied after the derivative. A pool that
#' does not cover the targets' range also stops the verb, except within half
#' the pool's spacing, where the overshooting column is taken at the pool's
#' endpoint rather than extrapolated and the clamp is recorded and warned
#' about. Targets finer than the pool warn. `window` is a width in cm-1, so
#' the filter is the same physical filter on any batch; the point count it
#' becomes on the targets' grid is recorded in `settings$window_points`, and
#' the width that count actually spans in `settings$window_cm`.
#'
#' **Self-leakage.** A pool row that sits far closer to a target than that
#' target's surroundings do — distance zero, or below `twin_ratio` times the
#' reference distance — is the target's twin: the same sample, present in
#' both sets or scanned twice. The reference is the 75th percentile of the
#' target's 50 nearest rows with the property measured, or of a quarter of
#' the measured rows when fewer than 200 have it: a fixed width rather than
#' the `k` being drawn, so a cluster of replicate scans cannot set the very
#' number it is measured against. Every flagged row is dropped from that
#' target's neighbourhood, spares are fetched so the target still reaches
#' `k`, and the exclusion is recorded with its target, property, distance and
#' reason. Except under `scope = "global"`, the flagged rows are also
#' subtracted from the returned object, since a row excluded from one
#' neighbourhood would otherwise walk back in through any other target that
#' drew it. Under `scope = "global"` the same check runs, per property, in
#' the same space and with the same width, and its exclusions and target
#' distances are the ones a batch-scope call with the same arguments
#' records. The flagged rows are recorded but stay in the returned rows,
#' because global returns the whole pool by definition, so a comparison that
#' scores global against batch has to act on `x$selection$exclusions`
#' itself. The membership table keeps the subtracted rows with
#' `retained = FALSE` rather than dropping them, so the record still says
#' what each neighbourhood was.
#'
#' **Units.** The pool and the targets are compared on raw absorbance
#' magnitude before the similarity space is built. The comparison is of
#' spread, the interquartile range, because a unit or gain change multiplies
#' it: a more-than-twofold difference in IQR warns. The medians are compared
#' the same way, but only when each dominates its own spread, since a
#' baseline-offset set has a median near zero whose sign is a coin flip. Two
#' fold rather than three because natural-log against base-10 absorbance is
#' 2.303, which is the mismatch most likely to go unnoticed; an instrument
#' gain difference of about 1.5x stays silent on purpose.
#'
#' A pure additive offset is deliberately not flagged. The similarity space
#' begins with SNV and takes a derivative, and both remove a constant offset,
#' so it cannot change which neighbours are drawn. It can still matter
#' downstream: a model preprocessing that preserves offset (`"raw"`, or a
#' baseline method that does not centre) sees it, and that is the caller's
#' check to make, not this one. The resemblance check below cannot do this
#' job either, for the same reason: its space begins with SNV, so a pure unit
#' difference leaves the scores identical.
#'
#' **Registered libraries.** `library = "kssl"` is the USDA NRCS Kellogg
#' Soil Survey Laboratory's mid-infrared library as published in the Open
#' Soil Spectral Library v1.2 (CC-BY 4.0; Safanelli et al. 2025), every depth,
#' on its native 600 to 4000 cm-1 grid at 2 cm-1. horizons does not host it.
#' The first draw downloads OSSL's public files (about 436 MB), verifies their
#' published MD5s, builds the library and caches it; it asks first in an
#' interactive session, and a non-interactive one needs
#' `options(horizons.library_download = TRUE)`. The option
#' `horizons.cache_dir` moves the cache. Every later draw reads the cache.
#' `x$selection$library` records which library was drawn from.
#'
#' **What the record holds** (`x$selection`): the settings as resolved, the
#' reconciliation, the pool's identity, the membership table (target by
#' property by pool row, with the space it was measured in, distance, rank,
#' and whether the row was `retained` in the object after the twin
#' subtraction), the groups, pool sizes per property against those available, every
#' target's nearest and mean-of-k distance (the applicability signal at
#' selection time), the unit comparison, the twins excluded and how many of
#' them left the union, any target that could not reach `k`, and the
#' clustering when `scope = "cluster"`.
#'
#' @param x `horizons_data.` The targets: the samples to be predicted.
#' @param library `horizons_data` or `character`. The reference library:
#'   a `horizons_data` with one or more response columns (your own pool,
#'   `spectra() |> standardize() |> add_response()`), the name of a
#'   registered library (`"kssl"`), or a path to a library file. A registered
#'   library is built on your machine from its public sources the first time
#'   it is used, after asking, and cached under
#'   `tools::R_user_dir("horizons", "cache")`; see "Registered libraries".
#' @param k `integer.` Neighbours per target per property; a scalar, or a
#'   named vector with one entry per property. 100 is the 2026-09-29 defaults
#'   read: 400 was worse on every batch from another instrument and no better
#'   on KSSL. Default: `100`.
#' @param scope `character.` `"batch"`, `"cluster"`, `"sample"` or
#'   `"global"`. Default: `"batch"`.
#' @param properties `character or NULL.` Response columns of the pool to
#'   draw for. `NULL` means all of them. Each needs at least one measured
#'   pool row, under every scope. Default: `NULL`.
#' @param snv `logical.` SNV in the similarity space. Default: `TRUE`.
#' @param derivative `integer.` Savitzky-Golay derivative order in the
#'   similarity space; `0` disables the filter. Default: `1`.
#' @param window `numeric.` Savitzky-Golay window width in cm-1, measured
#'   between the outermost points, so the filter means the same thing on any
#'   grid: 40 cm-1 is 11 points at 4 cm-1 and 21 at 2 cm-1. It becomes the
#'   nearest odd number of points on the targets' grid (ties round up), and
#'   the record carries both. Default: `40`.
#' @param poly `integer.` Savitzky-Golay polynomial order. Default: `2`.
#' @param mask `matrix or NULL.` Wavenumber ranges to drop from the
#'   similarity space after the derivative, one row per range, low then
#'   high. Default: `NULL`.
#' @param space `character.` `"pca"` (unsupervised, of the pool) or
#'   `"pls"` (against one property; `properties` must name exactly one).
#'   Default: `"pca"`.
#' @param ncomp `numeric.` Components: a proportion of variance in (0, 1)
#'   (PCA only, capped at 100) or an integer count. Default: `0.99`.
#' @param sdev_floor `numeric.` Components whose standard deviation is below
#'   this fraction of PC1's leave the distance. Mahalanobis divides each
#'   component by its standard deviation, so a variance-chosen tail orders of
#'   magnitude below PC1 would otherwise weigh as much as the dominant
#'   chemical axes, and those trailing eigenvectors are the least stable part
#'   of the decomposition. In [0, 1); `0` disables the floor. Default: `0.1`.
#' @param metric `character.` `"euclidean"`, `"mahalanobis"` or
#'   `"cosine"`, on the scores. Euclidean weights components by their
#'   variance; Mahalanobis whitens them and won on no batch in the 2026-09
#'   metric experiments; in the 2026-09-29 read cosine clearly won only on
#'   one KSSL batch, the Mississippi Delta.
#'   Default: `"euclidean"`.
#' @param depth `character.` `"topsoil"` draws only from library rows whose
#'   upper depth is under 30 cm; `"all"` draws from every depth. The space is
#'   fit on every row either way, so depth restricts which rows can be drawn
#'   and not the axes they are measured on. The resemblance check measures
#'   targets against the depth-eligible rows.
#'   Rows with no recorded depth are not topsoil. Depth is read from an
#'   `upper_depth_cm` column; a library without one has every row eligible,
#'   and `x$selection$depth` records that. Global returns the eligible rows.
#'   Default: `"topsoil"`.
#' @param clusters `integer or NULL.` `scope = "cluster"` only: the cluster
#'   count, or `NULL` to choose by silhouette. Default: `NULL`.
#' @param cluster_min `integer.` `scope = "cluster"` only: the floor on
#'   cluster size. Default: `30`.
#' @param twin_ratio `numeric.` A pool row is a target's twin when its
#'   distance to that target is below this fraction of the reference
#'   distance, the 75th percentile of the target's 50 nearest measured rows
#'   (a quarter of the measured rows when fewer than 200 have the property),
#'   under every scope. In (0, 1). An exact match is always a twin.
#'   Currently `0.05`, an internal constant carried over from an earlier rule
#'   and still to be calibrated on replicate scans, so treat the value as
#'   provisional rather than settled.
#' @param chunk_size `integer.` Targets per distance chunk. Default: `500`.
#' @param seed `integer.` Seed for every stochastic step of the verb: the
#'   target clustering under `scope = "cluster"`, and the pool sample the
#'   resemblance check draws its reference distribution from. The caller's
#'   RNG state is saved and restored, so the verb does not advance it.
#'   Default: `1`.
#' @param verbose `logical.` Print the report. Default: `TRUE`.
#'
#' @return `horizons_data.` Rows drawn from `pool`, on the targets'
#'   wavenumber grid, with every response column the pool carried, three
#'   provenance columns with role `meta` (`.drawn_by`, `.min_distance`,
#'   and `.group` except under `scope = "sample"`), and the record in
#'   `x$selection`. `provenance$standardization` describes the returned
#'   axis. When the pool was resampled onto the targets' grid, the pool's
#'   record is rewritten: `grid` is the targets' (`NULL` when they are not on
#'   a canonical grid, or when their recorded grid does not match their
#'   columns), `n_wavelengths` and `wavelength_range` are the new axis's, and
#'   `resampled` is `TRUE`, while the `standardize()` arguments (`resample`,
#'   `trim`, `remove_water`, `baseline`) and `applied_at` stay the pool's,
#'   since they produced its values; so `resample` can differ from
#'   `grid$step`, and `trim` from `wavelength_range`. Its `reconciliation`
#'   entry records the move: `operation`, `clamp` (the overshoot at each end
#'   when an end column was taken at the pool's endpoint, else `NULL`) and
#'   `pool`, the pool's record as it was, carrying its original `grid`. That
#'   entry is the object's axis history and travels with its provenance; it
#'   is distinct from `x$selection$reconciliation`, the selection's record of
#'   how the two axes compared. A pool already on the targets' grid keeps its
#'   record unchanged, and a pool never standardized keeps none.
#'
#' @examples
#' \dontrun{
#' training <- targets |>
#'   select_training("kssl", properties = "clay")
#'
#' model <- training |>
#'   configure(outcome = "clay") |>
#'   validate() |>
#'   evaluate() |>
#'   fit()
#'
#' predict(model, targets)
#' }
#'
#' @export
select_training <- function(x, library,
                            k           = 100L,
                            scope       = c("batch", "cluster", "sample", "global"),
                            properties  = NULL,
                            snv         = TRUE,
                            derivative  = 1L,
                            window      = SELECT_SG_WINDOW_CM,
                            poly        = 2L,
                            mask        = NULL,
                            space       = c("pca", "pls"),
                            ncomp       = 0.99,
                            sdev_floor  = SELECT_SDEV_FLOOR,
                            metric      = c("euclidean", "mahalanobis", "cosine"),
                            depth       = c("topsoil", "all"),
                            clusters    = NULL,
                            cluster_min = 30L,
                            twin_ratio  = SELECT_TWIN_RATIO,
                            chunk_size  = 500L,
                            seed        = 1L,
                            verbose     = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  errors <- character()

  if (!inherits(x, "horizons_data")) {

    errors <- c(errors, cli::format_inline("{.arg x} must be a horizons_data object"))

  }

  scope_ok  <- is.character(scope)  && all(scope  %in% c("batch", "cluster", "sample", "global"))
  space_ok  <- is.character(space)  && all(space  %in% c("pca", "pls"))
  metric_ok <- is.character(metric) && all(metric %in% c("mahalanobis", "euclidean", "cosine"))
  depth_ok  <- is.character(depth) && all(depth %in% c("topsoil", "all"))

  if (!scope_ok)  errors <- c(errors, cli::format_inline("{.arg scope} must be one of batch, cluster, sample, global"))
  if (!space_ok)  errors <- c(errors, cli::format_inline("{.arg space} must be pca or pls"))
  if (!metric_ok) errors <- c(errors, cli::format_inline("{.arg metric} must be mahalanobis, euclidean or cosine"))
  if (!depth_ok)  errors <- c(errors, cli::format_inline("{.arg depth} must be topsoil or all"))

  scope      <- if (scope_ok)  scope[1]      else NA_character_
  space      <- if (space_ok)  space[1]      else NA_character_
  metric     <- if (metric_ok) metric[1]     else NA_character_
  depth      <- if (depth_ok)  depth[1]      else NA_character_

  k_ok <- is.numeric(k) && length(k) >= 1L && all(is.finite(k)) && all(k >= 1) && all(k == round(k))

  if (!k_ok) {

    errors <- c(errors, cli::format_inline("{.arg k} must be a positive integer, or a named vector of them"))

  }

  floor_ok <- is.numeric(sdev_floor) && length(sdev_floor) == 1L &&
              is.finite(sdev_floor)  && sdev_floor >= 0 && sdev_floor < 1

  if (!floor_ok) {

    errors <- c(errors, cli::format_inline("{.arg sdev_floor} must be a single number in [0, 1); 0 disables the floor"))

  }

  window_ok <- is.numeric(window) && length(window) == 1L && is.finite(window) && window > 0

  if (!window_ok) {

    errors <- c(errors, cli::format_inline("{.arg window} must be a single positive width in cm\u207B\u00B9"))

  }

  ## A twin sits below this fraction of its reference distance. At 1 or more
  ## the threshold reaches the reference itself and flags ordinary
  ## neighbours, and global's claim to hold every twin in its fetch (Step 3)
  ## rests on the threshold sitting below the reference.

  ratio_ok <- is.numeric(twin_ratio) && length(twin_ratio) == 1L &&
              is.finite(twin_ratio)  && twin_ratio > 0 && twin_ratio < 1

  if (!ratio_ok) {

    errors <- c(errors, cli::format_inline("{.arg twin_ratio} must be a single number in (0, 1), a fraction of the reference distance"))

  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {

    errors <- c(errors, cli::format_inline("{.arg verbose} must be TRUE or FALSE"))

  }

  ## The similarity space's levers, and the draw's own, are checked here too
  ## rather than where they are used, so none of them is first found wrong
  ## after a registered library has been downloaded and built. The space
  ## functions keep their own checks for callers that reach them directly.

  is_count <- function(v, min = 1) {
    is.numeric(v) && length(v) == 1L && is.finite(v) && v >= min && v == round(v)
  }

  if (!is.logical(snv) || length(snv) != 1L || is.na(snv)) {

    errors <- c(errors, cli::format_inline("{.arg snv} must be TRUE or FALSE"))

  }

  deriv_ok <- is_count(derivative, min = 0)
  poly_ok  <- is_count(poly, min = 0)

  if (!deriv_ok) errors <- c(errors, cli::format_inline("{.arg derivative} must be a single whole number, 0 or more"))
  if (!poly_ok)  errors <- c(errors, cli::format_inline("{.arg poly} must be a single whole number, 0 or more"))

  if (deriv_ok && poly_ok && derivative > 0 && poly < derivative) {

    errors <- c(errors, cli::format_inline("{.arg poly} ({poly}) must be at least {.arg derivative} ({derivative})"))

  }

  ## The window becomes points on the targets' grid, which x already fixes.

  if (inherits(x, "horizons_data") && window_ok && deriv_ok && poly_ok && derivative > 0) {

    target_res <- tryCatch(grid_summary(predictor_matrix(x)$wavenumbers)$resolution,
                           error = function(e) NA_real_)

    if (is.finite(target_res)) {

      pts <- window_to_points(window, target_res)

      if (pts <= poly) {

        errors <- c(errors, cli::format_inline(
          "A {.arg window} of {window} cm\u207B\u00B9 is {pts} point{?s} on the targets' {target_res} cm\u207B\u00B9 grid, too few for a polynomial of order {poly}; widen {.arg window} or lower {.arg poly}"))

      }

    }

  }

  if (!is.null(mask)) {

    mask_ok <- is.matrix(mask) && is.numeric(mask) && ncol(mask) == 2L &&
               all(is.finite(mask)) && all(mask[, 1] <= mask[, 2])

    if (!mask_ok) {

      errors <- c(errors, cli::format_inline("{.arg mask} must be a numeric matrix with two columns, low then high, e.g. {.code rbind(c(2200, 2400))}"))

    }

  }

  ncomp_ok <- is.numeric(ncomp) && length(ncomp) == 1L && is.finite(ncomp) &&
              ((ncomp > 0 && ncomp < 1) || (ncomp >= 1 && ncomp == round(ncomp)))

  if (!ncomp_ok) {

    errors <- c(errors, cli::format_inline("{.arg ncomp} must be a proportion in (0, 1) or a positive integer"))

  }

  if (identical(space, "pls") && ncomp_ok && ncomp < 1) {

    errors <- c(errors, cli::format_inline("{.arg space = \"pls\"} needs a whole-number {.arg ncomp}; a variance proportion has no meaning for PLS"))

  }

  if (identical(space, "pls") && !is.null(properties) && length(properties) != 1L) {

    errors <- c(errors, cli::format_inline("{.arg space = \"pls\"} selects against one property; name exactly one in {.arg properties}"))

  }

  if (!is.null(clusters) && !is_count(clusters, min = 1)) {

    errors <- c(errors, cli::format_inline("{.arg clusters} must be NULL or a single positive whole number"))

  }

  if (!is_count(cluster_min))  errors <- c(errors, cli::format_inline("{.arg cluster_min} must be a single positive whole number"))
  if (!is_count(chunk_size))   errors <- c(errors, cli::format_inline("{.arg chunk_size} must be a single positive whole number"))
  if (!is_count(seed, min = -.Machine$integer.max) || abs(seed) > .Machine$integer.max) {

    errors <- c(errors, cli::format_inline("{.arg seed} must be a single whole number within R's integer range"))

  }

  ## The library resolves only after everything that can be checked without
  ## it has passed: a registered library that is not cached yet is downloaded
  ## and built here, and a mistyped argument must not cost that.

  if (length(errors) > 0) abort_select_inputs(errors)

  resolved <- resolve_source(library, verbose = isTRUE(verbose))
  pool     <- resolved$pool

  if (!inherits(pool, "horizons_data")) {

    errors <- c(errors, cli::format_inline("{.arg library} must be a horizons_data object"))

  } else {

    errors <- c(errors, check_pool_unpromoted(pool))

  }

  ## Properties: the pool's responses ---------------------------------------

  if (inherits(pool, "horizons_data")) {

    responses <- pool$data$role_map$variable[pool$data$role_map$role == "response"]

    if (!length(responses)) {

      errors <- c(errors, cli::format_inline("{.arg library} has no response columns; add the library's lab values first"))

    } else if (is.null(properties)) {

      properties <- responses

    } else {

      unknown <- setdiff(properties, responses)

      if (length(unknown)) {

        errors <- c(errors, cli::format_inline("{.arg properties} not in the pool's responses: {.val {unknown}} (available: {.val {responses}})"))

      }

    }

    if (k_ok && (length(k) > 1L || !is.null(names(k)))) {

      missing_k <- setdiff(properties, names(k))

      if (length(missing_k)) {

        errors <- c(errors, cli::format_inline("A named {.arg k} needs an entry for every property; missing {.val {missing_k}}"))

      }

    }

    if (identical(space, "pls") && length(properties) != 1L) {

      errors <- c(errors, cli::format_inline("{.arg space = \"pls\"} selects against one property; name exactly one in {.arg properties}"))

    }

    ## A prior selection's provenance columns would collide on bind_cols ------

    prior <- intersect(c(".drawn_by", ".min_distance", ".group"), names(pool$data$analysis))

    if (length(prior)) {

      errors <- c(errors, cli::format_inline(
        "{.arg library} already carries the provenance column{?s} {.val {prior}}, so it is itself a selection; pass the library as it comes from standardize()"))

    }

  }

  if (length(errors) > 0) abort_select_inputs(errors)

  k_by <- resolve_k(k, properties)

  if (verbose) cat(paste0("\u251C\u2500 ", cli::style_bold("Selecting training set"), "...\n"))

  ## ---------------------------------------------------------------------------
  ## Step 1: Reconcile the axes
  ## ---------------------------------------------------------------------------

  rc <- reconcile_axes(pool, x)
  tm <- predictor_matrix(x)

  pool_rc <- rebuild_predictors(pool, rc$matrix, rc$wavenumbers)

  ## The return is subset from pool_rc and carries its provenance, so once the
  ## pool is resampled its standardization record has to describe the
  ## targets' axis, not the one it came from (#90).

  if (rc$record$operation == "resampled") {

    pool_rc$provenance$standardization <- reconciled_standardization(
      pool$provenance$standardization, x$provenance$standardization, rc$wavenumbers,
      clamp = rc$record$clamp
    )

  }

  if (verbose) {

    op <- if (rc$record$operation == "none") "already on the targets' grid" else
      paste0("resampled ", rc$record$pool_grid$resolution, " \u2192 ",
             rc$record$target_grid$resolution, " cm\u207B\u00B9")
    cat(paste0("\u2502  \u251C\u2500 Pool: ", nrow(rc$matrix), " rows, ", op, "\n"))

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Build the similarity space on the pool, project the targets
  ## ---------------------------------------------------------------------------

  ## One space, fit on every library row: the draw's space for every
  ## property, and the one target clustering and the resemblance check use,
  ## so none of them depends on which property is being drawn. A space per
  ## property on its measured rows (space_rows = "measured") was removed on
  ## 2026-09-30: on KSSL it moved the drawn training set by about 6 %.

  pool_ids <- pool_rc$data$analysis$sample_id
  resp_tbl <- pool_rc$data$analysis[, c("sample_id", properties), drop = FALSE]

  ## Depth restricts the draw, not the space. A subsoil row stays in the
  ## space (Sam's call, 2026-09-30: the library is the population, and
  ## whether a row is subsoil should not redefine what similar soil means).
  ## Under "topsoil" its responses are hidden from the draw, which is how the
  ## draw already treats a row without the property: the draw, the twin
  ## reference and the resemblance check see only eligible rows. A row with
  ## no recorded depth is not known to be topsoil and is not eligible.
  ## A library that records no depth at all has nothing to restrict on, and
  ## the record says so rather than the default refusing a user's own pool.

  depth_recorded <- "upper_depth_cm" %in% names(pool_rc$data$analysis)
  depth_applied  <- identical(depth, "topsoil") && depth_recorded
  eligible       <- rep(TRUE, length(pool_ids))

  if (depth_applied) {

    upper <- pool_rc$data$analysis$upper_depth_cm

    ## A character column compares as text, where "250" < 30 is TRUE, and
    ## the cut would silently let every depth through while saying it applied.

    if (!is.numeric(upper)) {

      cli::cli_abort(c(
        "{.field upper_depth_cm} must be numeric for {.code depth = \"topsoil\"}; it is {.cls {class(upper)[1]}}",
        "i" = "Convert it to centimetres as numbers, or pass {.code depth = \"all\"}"
      ), class = "horizons_input_error")

    }

    eligible <- !is.na(upper) & upper < SELECT_TOPSOIL_MAX_CM

    for (p in properties) resp_tbl[[p]][!eligible] <- NA

  }

  ## A property no pool row has measured has no rows to draw from and no
  ## rows to check twins against, under any scope. Stop here, before the
  ## draw fails with a message naming none of that.

  n_measured <- vapply(properties, function(p) sum(!is.na(resp_tbl[[p]])), integer(1))

  unmeasured <- properties[n_measured < 1L]

  if (length(unmeasured)) {

    cli::cli_abort(c(
      "{.arg library} has no measured rows for {.field {unmeasured}}",
      "i" = "Drop {cli::qty(unmeasured)}{?it/them} from {.arg properties}, or add the lab values to the library first",
      if (depth_applied) c("i" = "Only topsoil rows (upper depth < {SELECT_TOPSOIL_MAX_CM} cm) count under {.code depth = \"topsoil\"}; {.code depth = \"all\"} draws from every depth")
    ), class = "horizons_input_error")

  }

  ## Units, before SNV erases the evidence ----------------------------------

  units <- check_photometric_units(rc$matrix, tm$matrix)

  if (units$mismatch) {

    cli::cli_warn(c(
      "The pool and the targets look like different photometric units or modes",
      "i" = "Pool absorbance: median {signif(units$pool_median, 3)}, IQR {signif(units$pool_iqr, 3)}",
      "i" = "Targets: median {signif(units$target_median, 3)}, IQR {signif(units$target_iqr, 3)}",
      "i" = "Selection proceeds, but a model trained on one unit cannot predict the other"
    ), class = "horizons_select_warning")

  }

  ## The window is a width in cm-1; the filter counts points on the grid the
  ## space is built on, which is the targets'. Nearest odd count, ties up.

  window_points <- if (derivative > 0) {
    window_to_points(window, rc$record$target_grid$resolution)
  } else {
    NA_integer_
  }

  if (derivative > 0 && window_points <= poly) {

    cli::cli_abort(c(
      "A {.arg window} of {window} cm\u207B\u00B9 is {window_points} point{?s} on the targets' {rc$record$target_grid$resolution} cm\u207B\u00B9 grid, too few for a polynomial of order {poly}",
      "i" = "Widen {.arg window} or lower {.arg poly}."
    ), class = "horizons_input_error")

  }

  build_space_on <- function(rows) {

    y <- if (space == "pls") pool_rc$data$analysis[[properties]][rows] else NULL

    build_similarity_space(rc$matrix[rows, , drop = FALSE], rc$wavenumbers,
                           snv = snv, derivative = derivative,
                           ## With no derivative the filter never runs and its
                           ## window is unread; any valid value will do.
                           window = if (derivative > 0) window_points else 11L,
                           poly = poly, mask = mask, space = space, ncomp = ncomp,
                           sdev_floor = sdev_floor, y = y)

  }

  sp <- build_space_on(seq_along(pool_ids))
  St <- project_similarity(sp, tm$matrix, tm$wavenumbers)


  ## The width the filter actually spans once rounded to whole points.

  window_cm <- if (derivative > 0) {
    (window_points - 1L) * rc$record$target_grid$resolution
  } else {
    NA_real_
  }

  ## A PLS space is fit against the pool's own responses, so which rows land
  ## in the training set was decided by their y. No target leakage, but the
  ## pool-internal CV downstream is optimistic. Stated, not fixed here.

  space_note <- if (space == "pls") {
    paste0("The similarity space is PLS against ", properties,
           ", so rows were selected using their own measured values; ",
           "pool-internal CV is optimistic and the honest score is against the targets")
  } else {
    NULL
  }

  if (verbose) {

    sg <- if (derivative > 0) {
      paste0("SG d", derivative, " w", window_points, " (", signif(window_cm, 3), " cm\u207B\u00B9) p", poly)
    } else {
      NULL
    }

    chain <- c(if (snv) "SNV", sg,
               if (!is.null(mask)) paste0(nrow(mask), " masked range", if (nrow(mask) > 1) "s"),
               toupper(space))
    floored <- if (!is.null(sp$ncomp_variance) && sp$ncomp_variance > sp$ncomp) {
      paste0(" (", sp$ncomp_variance, " by variance, floored at ", sdev_floor, " of PC1 sd)")
    } else {
      ""
    }

    cat(paste0("\u2502  \u251C\u2500 Space: ", paste(chain, collapse = " \u2192 "), ", ",
               sp$ncomp, " components", floored, " on all ", length(pool_ids),
               " rows, ", metric, "\n"))

    depth_line <- if (depth_applied) {
      paste0("topsoil (upper depth < ", SELECT_TOPSOIL_MAX_CM, " cm), ", sum(eligible), " of ",
             length(pool_ids), " rows eligible")
    } else if (identical(depth, "topsoil")) {
      "not recorded in the library, every row eligible"
    } else {
      "all depths"
    }

    cat(paste0("\u2502  \u251C\u2500 Depth: ", depth_line, "\n"))

    if (!is.null(space_note)) {

      cat(paste0("\u2502  \u251C\u2500 ", space_note, "\n"))

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Draw
  ## ---------------------------------------------------------------------------

  ## Global draws nothing, but the twin check still runs: global is the
  ## control arm of the batch-versus-global comparison, and a control arm
  ## whose leakage was never measured biases that comparison in a fixed
  ## direction. So global runs the same draw as batch, in the same space
  ## for its record and not its rows: the
  ## exclusions and target_distances are kept, row for row the ones batch
  ## records, and the membership and short draws are discarded below. The
  ## flagged rows stay in the return, because global returns the whole pool
  ## by definition.
  ##
  ## Global trains on every measured row, so it needs every twin in the
  ## pool, not only those before a k-th kept row. draw_neighbours() records
  ## every flagged column it fetched. A flag is a distance threshold on a
  ## sorted row, so the flagged columns are always the leading run and its
  ## k-th-row cutoff never cuts one. And twin_ratio is validated below 1, so
  ## when the reference distance is positive the threshold sits below it and
  ## no row past the reference width can be flagged: the fetch holds every
  ## twin there is. A reference of zero, which takes exact copies in three
  ## quarters of the width (38 at 50), flags on d == 0 alone, and an exact
  ## copy beyond the fetch then goes unrecorded.
  ##
  ## k is capped at each property's measured rows under global, because
  ## global draws nothing and k only sets the reach of mean_k; the drawing
  ## scopes refuse such a k in draw_neighbours().

  k_draw <- if (scope == "global") pmin(k_by, n_measured) else k_by

  short_hint <- if (depth_applied) {
    cli::format_inline("Only topsoil rows count under {.code depth = \"topsoil\"}; {.code depth = \"all\"} draws from every depth")
  } else {
    NULL
  }

  draw <- draw_neighbours(St, sp$scores, responses = resp_tbl, k = k_draw,
                          properties = properties, metric = metric, sdev = sp$sdev,
                          chunk_size = chunk_size, twin_ratio = twin_ratio,
                          space_label = "all", short_hint = short_hint)

  if (scope == "global") {

    draw$membership  <- tibble::tibble(target_id = character(), property = character(),
                                       space = character(), pool_id = character(),
                                       distance = numeric(), rank = integer(),
                                       retained = logical())
    draw$short_draws <- empty_short_draws()
    draw$k           <- k_by

  }

  membership <- draw$membership

  ## ---------------------------------------------------------------------------
  ## Step 4: Group by scope
  ## ---------------------------------------------------------------------------

  target_ids <- rownames(St)
  clustering <- NULL

  group_of_target <- switch(
    scope,
    global  = stats::setNames(rep(1L, length(target_ids)), target_ids),
    batch   = stats::setNames(rep(1L, length(target_ids)), target_ids),
    sample  = stats::setNames(seq_along(target_ids), target_ids),
    cluster = {

      clustering <- cluster_targets(St, clusters = clusters, cluster_min = cluster_min, seed = seed)

      if (clustering$k == 1L && verbose) {

        ## Through the tree, not cli, so verbose = FALSE is actually silent.
        cat(paste0("\u2502  \u251C\u2500 scope = \"cluster\": ", clustering$reason,
                   "; returning one group\n"))

      }

      clustering$assignment

    }
  )

  ## ---------------------------------------------------------------------------
  ## Step 5: Assemble the return
  ## ---------------------------------------------------------------------------

  ## A twin excluded from one target's neighbourhood is still drawn by every
  ## other target whose neighbourhood it falls in, so it walks straight back
  ## into the union unless it is subtracted here. That holds under sample
  ## too: the return is one object, nothing downstream consumes
  ## selection$groups yet, and the ordinary configure |> evaluate |> fit on a
  ## sample-scope return would otherwise train on every target's own
  ## replicates. Only global keeps them, because global returns the whole
  ## pool by definition and says so. membership keeps the pre-subtraction
  ## draw, which is the record of what each neighbourhood was, and marks the
  ## subtracted rows retained = FALSE rather than dropping them.

  excluded_ids <- unique(draw$exclusions$pool_id)
  union_ids    <- if (scope == "global") pool_ids[eligible] else pool_ids[pool_ids %in% membership$pool_id]

  n_excluded_union <- 0L

  if (scope != "global" && length(excluded_ids)) {

    left             <- setdiff(union_ids, excluded_ids)
    n_excluded_union <- length(union_ids) - length(left)
    union_ids        <- left

  }

  ## A property whose every drawn row was some target's twin has no training
  ## rows left, and the subset below would fail on an empty keep with a
  ## message naming neither the property nor the twin rule. At a twin_ratio
  ## near 1 the threshold reaches ordinary neighbours and this is what
  ## happens, so say that.

  if (scope != "global") {

    emptied <- properties[vapply(properties, function(p) {

      drawn <- unique(membership$pool_id[membership$property == p])
      length(drawn) > 0L && !any(drawn %in% union_ids)

    }, logical(1))]

    if (length(emptied)) {

      ### Whether a lower twin_ratio would bring the rows back. It cannot when
      ### every drawn row is some target's own copy, at distance zero to
      ### rounding, below any ratio. is_exact_copy() is the rule, the same
      ### one that writes "exact" into the record's reason column.

      ex     <- draw$exclusions
      copies <- ex$pool_id[is_exact_copy(ex$distance, ex$reference_distance)]
      drawn  <- unique(membership$pool_id[membership$property %in% emptied])

      cause <- if (all(drawn %in% copies)) {
        "Each was a target's own copy, at distance zero, so the targets are in the pool; a lower {.arg twin_ratio} cannot help. Remove the targets from the pool"
      } else {
        "A twin sits below {.arg twin_ratio} = {twin_ratio} times its target's reference distance, and at this value ordinary neighbours are being flagged; lower it (the default is {SELECT_TWIN_RATIO})"
      }

      cli::cli_abort(c(
        "Every row drawn for {.field {emptied}} was flagged as some target's twin, so the training set has none left for {cli::qty(emptied)}{?it/them}",
        "i" = cause
      ), class = "horizons_input_error")

    }

  }

  ## The record of which drawn rows survived the subtraction. Without it the
  ## first validate(remove_outliers = TRUE) filters membership to the rows
  ## still in the object and the excluded twins vanish from the record
  ## entirely, which is the one thing the record exists to hold.

  membership$retained <- if (scope == "global") {
    rep(TRUE, nrow(membership))
  } else {
    !(membership$pool_id %in% excluded_ids)
  }

  groups <- build_groups(group_of_target, membership,
                         if (scope == "global") pool_ids[eligible] else union_ids, scope)

  out <- subset_rows(pool_rc, union_ids, record = FALSE)

  if (scope == "global") {

    drawn_by <- rep(NA_integer_, length(union_ids))
    min_dist <- rep(NA_real_,    length(union_ids))

  } else {

    by_row   <- split(membership, membership$pool_id)
    drawn_by <- vapply(by_row[union_ids], function(d) length(unique(d$target_id)), integer(1))

    min_dist <- vapply(by_row[union_ids], function(d) min(d$distance), numeric(1))

  }

  meta <- tibble::tibble(.drawn_by = unname(drawn_by), .min_distance = unname(min_dist))

  if (scope != "sample") {

    meta$.group <- if (scope != "cluster") {

      ## batch and global are one group, so no distance comparison is made.
      rep(1L, length(union_ids))

    } else {

      ## The group of the nearest drawing target; a row drawn by two
      ## clusters is in both groups' pool_ids and this column names one.
      nearest_target <- vapply(by_row[union_ids],
                               function(d) d$target_id[which.min(d$distance)], character(1))
      unname(group_of_target[nearest_target])

    }

  }

  analysis <- dplyr::bind_cols(out$data$analysis, meta)
  role_map <- dplyr::bind_rows(out$data$role_map,
                               tibble::tibble(variable = names(meta), role = "meta"))

  out <- set_analysis(out, analysis, role_map)

  ## Pool sizes per property ------------------------------------------------

  ## drawn counts the rows that survived into the return, not the rows the
  ## neighbourhoods named, so it agrees with n_rows after the twin subtraction.

  pool_sizes <- tibble::tibble(
    property  = properties,
    available = vapply(properties, function(p) sum(!is.na(resp_tbl[[p]])), integer(1), USE.NAMES = FALSE),
    drawn     = vapply(properties, function(p) {

      if (scope == "global") {
        length(union_ids)
      } else {
        length(intersect(unique(membership$pool_id[membership$property == p]), union_ids))
      }

    }, integer(1), USE.NAMES = FALSE)
  )

  ## Resemblance: targets beyond the pool's own nearest-neighbour spread ----

  resemblance <- check_resemblance(sp, St, metric = metric, chunk_size = chunk_size, seed = seed,
                                   eligible = eligible)

  ## The record -------------------------------------------------------------

  out$selection <- list(
    settings = list(
      k = k_by, scope = scope, properties = properties,
      snv = snv, derivative = as.integer(derivative), window = window, window_points = window_points,
      poly = as.integer(poly), window_cm = window_cm, mask = mask,
      space = space, ncomp = ncomp, sdev_floor = sdev_floor,
      ncomp_retained = sp$ncomp, ncomp_variance = sp$ncomp_variance,
      sdev_ratio = sp$sdev_ratio, variance_retained = sp$variance_retained,
      depth = depth,
      metric = metric, clusters = clusters,
      cluster_min = as.integer(cluster_min), twin_ratio = twin_ratio, seed = as.integer(seed),
      space_note = space_note
    ),
    reconciliation   = rc$record,
    pool             = list(n_rows = length(pool_ids), id_hash = digest::digest(sort(pool_ids))),
    library          = resolved$record,
    depth            = list(requested = depth, recorded = depth_recorded, applied = depth_applied,
                            max_cm = if (depth_applied) SELECT_TOPSOIL_MAX_CM else NULL,
                            n_eligible = sum(eligible)),
    membership       = membership,
    groups           = groups,
    pool_sizes       = pool_sizes,
    target_distances = draw$target_distances,
    resemblance      = resemblance,
    units            = units,
    exclusions       = draw$exclusions,
    n_excluded_union = n_excluded_union,
    short_draws      = draw$short_draws,
    clustering       = clustering,
    timestamp        = Sys.time()
  )

  ## ---------------------------------------------------------------------------
  ## Step 6: Report
  ## ---------------------------------------------------------------------------

  if (verbose) report_selection(out$selection, n_targets = length(target_ids))

  if (nrow(draw$short_draws)) {

    sd_tbl <- draw$short_draws

    cli::cli_warn(c(
      "{nrow(sd_tbl)} target-property draw{?s} could not reach k after twin exclusions",
      "i" = "Smallest: {min(sd_tbl$k_drawn)} of {max(sd_tbl$k_requested)} rows, on {.field {unique(sd_tbl$property)}}",
      "i" = "The pool has too few measured rows left for those properties; see {.code x$selection$short_draws}"
    ), class = "horizons_select_warning")

  }

  if (!is.null(resemblance$skipped)) {

    cli::cli_warn(c(
      "The resemblance check did not run: {resemblance$skipped}",
      "i" = "Nothing says whether the targets resemble the rows they were drawn from; {.code depth = \"all\"} gives it more rows to measure against"
    ), class = "horizons_select_warning")

  }

  if (nrow(resemblance$beyond)) {

    cli::cli_warn(c(
      "{nrow(resemblance$beyond)} target{?s} sit{?s/} beyond the pool's own nearest-neighbour spread",
      "i" = "{.val {utils::head(resemblance$beyond$target_id, 5)}}{if (nrow(resemblance$beyond) > 5) ', ...' else ''}",
      "i" = "Selection proceeds; treat their predictions with care. The check is made after SNV, so it cannot see a unit mismatch; that is reported separately"
    ), class = "horizons_select_warning")

  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Re-validate
  ## ---------------------------------------------------------------------------

  out <- validate_horizons_data(out)

  out

}


## =============================================================================
## Internal helpers
## =============================================================================


## ---------------------------------------------------------------------------
## window_to_points() — a filter width in cm-1 as an odd point count
## ---------------------------------------------------------------------------

#' Convert a Savitzky-Golay width in cm-1 to points on a grid
#'
#' @description
#' The width is measured between the outermost points, so `n` points at
#' resolution `r` span `(n - 1) * r`. Returns the nearest odd count, ties
#' rounding up: 40 cm-1 is 11 points at 4 cm-1 and 21 at 2 cm-1.
#'
#' @param width [Numeric.] Width in cm-1.
#' @param resolution [Numeric.] Grid spacing in cm-1.
#' @return [Integer.] Odd point count.
#' @noRd
window_to_points <- function(width, resolution) {

  ## Rounded first, so a width that sits exactly on a tie in exact
  ## arithmetic does not flip with floating-point noise in the resolution.

  2L * as.integer(floor(round(width / (2 * resolution), 8) + 0.5)) + 1L

}


## ---------------------------------------------------------------------------
## abort_select_inputs() — the validation tree, then the error
## ---------------------------------------------------------------------------

#' Print collected input errors as a tree and abort
#' @noRd
abort_select_inputs <- function(errors) {

  cat(cli::col_red(cli::style_bold("! Input validation failed:\n")))

  for (i in seq_along(errors)) {

    branch <- if (i < length(errors)) "\u251C\u2500" else "\u2514\u2500"
    cat(cli::col_red(paste0("   ", branch, " ", errors[i], "\n")))

  }

  cat("\n")
  rlang::abort(paste(c("Input validation failed:", errors), collapse = "\n"),
               class = "horizons_input_error")

}


## ---------------------------------------------------------------------------
## check_pool_unpromoted() — The pool must be data, not a fitted object
## ---------------------------------------------------------------------------

#' Refuse a pool that carries state from later in the pipeline
#'
#' @description
#' `select_training()` builds its return by subsetting the pool, and a subset
#' keeps the pool's class and every section the pool was carrying. A promoted
#' pool would come out the other side still claiming to be validated,
#' evaluated or fitted, with `models$row_index` and `evaluation$split` keyed
#' to a row order the subset just destroyed. Promotion is meant to be earned,
#' so this refuses rather than silently clearing.
#'
#' @param pool [horizons_data.] The candidate pool.
#'
#' @return [Character.] Zero or more validation messages.
#' @noRd
check_pool_unpromoted <- function(pool) {

  msgs <- character()

  if (!identical(class(pool), c("horizons_data", "list"))) {

    msgs <- c(msgs, cli::format_inline(
      "{.arg library} is a {.cls {class(pool)[1]}}; it must be a plain horizons_data, as it comes from standardize() or add_response()"))

  }

  carried <- c(
    if (isTRUE(pool$validation$passed))      "validation$passed",
    if (!is.null(pool$evaluation$results))   "evaluation$results",
    if (!is.null(pool$models$workflows))     "models$workflows",
    if (!is.null(pool$models$row_index))     "models$row_index"
  )

  if (length(carried)) {

    msgs <- c(msgs, cli::format_inline(
      "{.arg library} already carries {.val {carried}} from later in the pipeline; selection subsets its rows, which would leave that state describing rows the training set no longer has"))

  }

  msgs

}


## ---------------------------------------------------------------------------
## check_photometric_units() — Pool and targets on the same scale
## ---------------------------------------------------------------------------

#' Compare the raw absorbance magnitude of pool and targets
#'
#' @description
#' Run before the similarity space is built, because the space begins with
#' SNV, which removes per-spectrum offset and scale and so makes a pure unit
#' or gain difference invisible to every check downstream of it.
#'
#' @details
#' The load-bearing comparison is of scale, the interquartile range, because
#' a unit or gain change multiplies it: fractional against percent
#' absorbance is a hundredfold difference in IQR and nothing else can produce
#' one. The medians are compared too, but only when they carry scale
#' information. Spectra carrying a per-sample baseline offset that straddles
#' zero have a pooled median near zero, and its sign is then a coin flip
#' between two sample sets in identical units. The bar for trusting a median
#' is therefore that it dominates its own spread: `|median| >= IQR`, which
#' ordinary absorbance clears easily and a centred set does not.
#'
#' A mismatch is flagged when the IQRs differ by more than `ratio` fold, or
#' when both medians dominate their spread and differ by more than `ratio`
#' fold or in sign.
#'
#' `ratio` is 2 rather than 3 because of the one mismatch that is neither
#' gross nor rare: natural-log against base-10 absorbance is a factor of
#' 2.303, and two libraries that disagree about it look like one library
#' until something notices the scale. A threefold bar misses it. Two still
#' leaves an instrument gain difference of around 1.5x silent, which is the
#' band this check is not for.
#'
#' @param pool_m [Matrix.] Pool spectra on the reconciled grid, raw.
#' @param target_m [Matrix.] Target spectra on the same grid, raw.
#' @param ratio [Numeric.] Fold difference that counts as a mismatch.
#'   Default: `2`.
#'
#' @return [List.] `pool_median`, `target_median`, `pool_iqr`, `target_iqr`,
#'   `basis` (`"iqr"`, or `"median and iqr"` when the locations were
#'   informative) and `mismatch`.
#' @noRd
check_photometric_units <- function(pool_m, target_m, ratio = 2) {

  p_med <- stats::median(pool_m,   na.rm = TRUE)
  t_med <- stats::median(target_m, na.rm = TRUE)
  p_iqr <- stats::IQR(pool_m,      na.rm = TRUE)
  t_iqr <- stats::IQR(target_m,    na.rm = TRUE)

  fold <- function(a, b) {

    lo <- min(abs(a), abs(b))
    hi <- max(abs(a), abs(b))

    if (lo < .Machine$double.eps) Inf else hi / lo

  }

  located <- is.finite(p_iqr) && is.finite(t_iqr) &&
             abs(p_med) >= p_iqr && abs(t_med) >= t_iqr

  scale_off <- is.finite(p_iqr) && is.finite(t_iqr) && fold(p_iqr, t_iqr) > ratio
  loc_off   <- located && (sign(p_med) != sign(t_med) || fold(p_med, t_med) > ratio)

  list(
    pool_median   = p_med,
    target_median = t_med,
    pool_iqr      = p_iqr,
    target_iqr    = t_iqr,
    basis         = if (located) "median and iqr" else "iqr",
    mismatch      = isTRUE(scale_off || loc_off)
  )

}


## ---------------------------------------------------------------------------
## rebuild_predictors() — Replace the predictor block of a horizons_data
## ---------------------------------------------------------------------------

#' Replace a horizons_data object's spectra with a matrix on a new grid
#'
#' @description
#' Swaps the predictor columns for `m` (rows in the object's row order),
#' names them `wn_<wavenumber>`, and rebuilds the role map, the pattern
#' `standardize()` uses after resampling.
#'
#' @param x [horizons_data.] The object.
#' @param m [Matrix.] New spectra, one row per analysis row.
#' @param wn [Numeric.] Wavenumbers of `m`'s columns, decreasing.
#'
#' @return [horizons_data.] `x` with the predictor block replaced.
#' @noRd
rebuild_predictors <- function(x, m, wn) {

  role_map <- x$data$role_map
  analysis <- x$data$analysis

  new_names <- paste0("wn_", wn)
  colnames(m) <- new_names
  rownames(m) <- NULL

  non_pred <- role_map$variable[role_map$role != "predictor"]

  new_analysis <- dplyr::bind_cols(analysis[, non_pred, drop = FALSE], tibble::as_tibble(m))
  new_role_map <- tibble::tibble(
    variable = c(non_pred, new_names),
    role     = c(role_map$role[role_map$role != "predictor"], rep("predictor", length(new_names)))
  )

  set_analysis(x, new_analysis, new_role_map)

}


## ---------------------------------------------------------------------------
## build_groups() — The groups table
## ---------------------------------------------------------------------------

#' Build the groups table for a scope
#'
#' @param group_of_target [Named integer.] Group per target.
#' @param membership [Tibble.] From `draw_neighbours()`.
#' @param pool_ids [Character.] All pool ids, in pool order.
#' @param scope [Character.] The scope.
#'
#' @return [Tibble.] `group`, `n_targets`, `n_rows`, `target_ids`
#'   (list), `pool_ids` (list), in pool order within each group.
#' @noRd
build_groups <- function(group_of_target, membership, pool_ids, scope) {

  gs <- sort(unique(group_of_target))

  rows <- lapply(gs, function(g) {

    t_ids <- names(group_of_target)[group_of_target == g]

    p_ids <- if (scope == "global") pool_ids else {
      drawn <- unique(membership$pool_id[membership$target_id %in% t_ids])
      pool_ids[pool_ids %in% drawn]
    }

    tibble::tibble(group = g, n_targets = length(t_ids), n_rows = length(p_ids),
                   target_ids = list(t_ids), pool_ids = list(p_ids))

  })

  dplyr::bind_rows(rows)

}


## ---------------------------------------------------------------------------
## check_resemblance() — Targets beyond the pool's own spread
## ---------------------------------------------------------------------------

#' Compare each target's nearest-pool distance to the pool's own
#'
#' @description
#' The pool's nearest-neighbour distances (self excluded), on a sample of
#' up to 2,000 pool rows when the pool is larger, give a reference
#' distribution; targets whose nearest-pool distance exceeds its 99th
#' percentile are named. A warning, never a stop.
#'
#' @details
#' The sample is seeded from the verb's `seed` and the caller's RNG state is
#' saved and restored around it, the way `cluster_targets()` does. Without
#' that, the threshold and the list of targets beyond it differ between two
#' identical calls on any pool over 2,000 rows — which is every real library
#' run — and the number a user would cite when defending a prediction cannot
#' be re-derived from the record.
#'
#' @param sp [horizons_similarity_space.] The space, with pool scores.
#' @param St [Matrix.] Target scores.
#' @param metric,chunk_size Passed to `nearest_neighbours()`.
#' @param seed [Integer.] Seed for the reference sample. Default: `1L`.
#' @param eligible [Logical or NULL.] The depth-eligible pool rows. Both
#'   the reference distribution and each target's nearest distance are taken
#'   over these rows only, so a batch that resembles only rows the draw
#'   cannot take (subsoil, under `depth = "topsoil"`) is still named. `NULL`
#'   means every row. Default: `NULL`.
#'
#' @return [List.] `threshold` (the 99th percentile), `n_reference`,
#'   `beyond` (tibble: `target_id`, `nearest`), and `skipped`: `NULL`, or
#'   why the check did not run.
#' @noRd
check_resemblance <- function(sp, St, metric, chunk_size, seed = 1L, eligible = NULL) {

  Sp  <- if (is.null(eligible)) sp$scores else sp$scores[eligible, , drop = FALSE]
  n   <- nrow(Sp)

  ## The reference is each row's distance to its nearest other row, so it
  ## needs at least two, and a handful makes a 99th percentile that is only
  ## the largest of a few gaps. Below the floor the check is recorded as not
  ## run rather than naming every target, or failing after the whole draw.

  if (n < SELECT_RESEMBLANCE_MIN_ROWS) {

    return(list(
      threshold   = NA_real_,
      n_reference = n,
      beyond      = tibble::tibble(target_id = character(), nearest = numeric()),
      skipped     = paste0("only ", n, " row", if (n == 1L) "" else "s",
                           " the draw can reach; the check needs ", SELECT_RESEMBLANCE_MIN_ROWS)
    ))

  }

  ## Preserve the caller's RNG state -----------------------------------------

  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)

  ref <- if (n > 2000L) {

    set.seed(seed)
    Sp[sort(sample.int(n, 2000L)), , drop = FALSE]

  } else {

    Sp

  }

  nn_pool <- nearest_neighbours(ref, Sp, k = 2L, metric = metric, sdev = sp$sdev, chunk_size = chunk_size)
  ## Column 1 is the row itself (distance 0); column 2 its nearest other row
  pool_nn <- nn_pool$dist[, 2]

  threshold <- unname(stats::quantile(pool_nn, 0.99))

  nn_t <- nearest_neighbours(St, Sp, k = 1L, metric = metric, sdev = sp$sdev, chunk_size = chunk_size)
  nearest <- nn_t$dist[, 1]

  beyond <- nearest > threshold

  list(
    threshold   = threshold,
    n_reference = nrow(ref),
    beyond      = tibble::tibble(target_id = rownames(St)[beyond], nearest = unname(nearest[beyond])),
    skipped     = NULL
  )

}


## ---------------------------------------------------------------------------
## report_selection() — The verbose tree
## ---------------------------------------------------------------------------

#' Print the selection report
#'
#' @details
#' Every dereference is guarded the way `describe_selection()` guards its
#' own: a record built by hand, or one a future field is missing from, has to
#' print rather than error inside a report. `nrow(NULL)` is `NULL`, and
#' `if (NULL)` is an error with a message that names neither the field nor
#' the verb.
#'
#' @param sel [List.] `x$selection`.
#' @param n_targets [Integer.] Targets in the batch.
#'
#' @return NULL, called for its output.
#' @noRd
report_selection <- function(sel, n_targets) {

  s  <- sel$settings
  ps <- sel$pool_sizes

  n_rows_of <- function(value) if (is.data.frame(value)) nrow(value) else 0L

  scope <- if (is.null(s$scope) || !length(s$scope)) "unknown" else s$scope

  for (i in seq_len(n_rows_of(ps))) {

    k_p <- if (is.null(s$k)) "unknown" else s$k[[ps$property[i]]]
    msg <- if (identical(scope, "global")) {
      paste0(ps$property[i], ": ", ps$available[i], " measured rows, no draw (scope = global)")
    } else {
      paste0(ps$property[i], ": k = ", k_p, " \u00D7 ", n_targets, " targets \u2192 ",
             ps$drawn[i], " of ", ps$available[i], " measured rows")
    }
    cat(paste0("\u2502  \u251C\u2500 ", msg, "\n"))

  }

  if (identical(scope, "cluster") && !is.null(sel$clustering)) {

    cat(paste0("\u2502  \u251C\u2500 Clusters: ", sel$clustering$k, " (", sel$clustering$reason, ")\n"))

  }

  ## Two counts, because they answer different questions: how many
  ## neighbourhood slots a twin was removed from, and how many rows that
  ## actually kept out of the training set.

  n_tw <- n_rows_of(sel$exclusions)
  tail <- if (n_tw) {
    paste0(" (", paste(utils::head(unique(sel$exclusions$target_id), 3), collapse = ", "),
           if (length(unique(sel$exclusions$target_id)) > 3) ", ..." else "", ")")
  } else {
    ""
  }

  cat(paste0("\u2502  \u251C\u2500 Twins flagged: ", n_tw, tail, "\n"))

  n_un <- sel$n_excluded_union

  union_note <- if (identical(scope, "global")) {
    "0 (scope = global keeps the whole pool; the check is reported, not applied)"
  } else if (is.null(n_un) || !length(n_un)) {
    "unknown"
  } else {
    paste0(n_un, " removed from the union")
  }

  cat(paste0("\u2502  \u251C\u2500 Twin rows: ", union_note, "\n"))

  n_short <- n_rows_of(sel$short_draws)

  if (n_short) {

    cat(paste0("\u2502  \u251C\u2500 Draws short of k: ", n_short,
               " (smallest ", min(sel$short_draws$k_drawn), ")\n"))

  }

  if (isTRUE(sel$units$mismatch)) {

    cat(paste0("\u2502  \u251C\u2500 Units: pool median ", signif(sel$units$pool_median, 3),
               ", targets ", signif(sel$units$target_median, 3), " \u2014 check the photometric unit\n"))

  }

  ## A skipped check has an empty beyond table; printing its count would
  ## read as "every target resembles the library" when nothing was checked.

  n_far <- if (is.data.frame(sel$resemblance$beyond)) nrow(sel$resemblance$beyond) else NA_integer_
  far   <- if (!is.null(sel$resemblance$skipped)) {
    paste0("not checked (", sel$resemblance$skipped, ")")
  } else if (is.na(n_far)) {
    "unknown"
  } else {
    n_far
  }
  cat(paste0("\u2502  \u251C\u2500 Targets beyond the pool's spread: ", far, "\n"))

  n_groups <- n_rows_of(sel$groups)
  n_rows   <- length(unique(unlist(sel$groups$pool_ids)))
  cat(paste0("\u2502  \u2514\u2500 ", n_rows, " rows in ", n_groups, " group", if (n_groups != 1) "s",
             " (scope = ", scope, ")\n"))
  cat("\u2502\n")

  invisible(NULL)

}
