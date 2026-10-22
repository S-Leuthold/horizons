# R/pipeline-select-training.R
# Draw a training set from a reference pool around a batch of targets. The
# only place in the pipeline where library and unknowns meet before anything
# is fitted, so the comparison of their axes, the self-leakage check and the
# selection-time distance live here.


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
#' * `"global"`: no draw; every row the others draw from (the library's
#'   rows at the depth asked for, less the flagged ones), kept so the
#'   comparison is one argument away. `k` only sets how many rows the
#'   recorded `mean_k` averages over, so a `k` above a property's measured
#'   rows is capped there under global, where the drawing scopes refuse or
#'   skip the property (see `properties`).
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
#' **Two axes.** Distances are measured in the library's own similarity
#' space, on the library's grid: the targets are resampled onto it for the
#' search only, so one fitted space serves every batch whatever its grid, and
#' for a registered library or a library file that space is cached (see
#' "The cached space"). The training set comes back on the library's own
#' columns at the wavenumbers the targets share with it: a column is shared
#' when the library has a predictor of the same name, and nothing is
#' interpolated. Targets on a coarser canonical grid (4 or 8 cm-1 against a
#' 2 cm-1 library) share every one of their columns, so the training set
#' comes back on their step. Target columns the library lacks (a wider range,
#' a finer or offset step) are named in one `horizons_warning_coverage`;
#' [predict()] uses only the columns the model was trained on, so the targets
#' predict as they are. Targets that share no column with the library, or
#' whose wavenumbers inside its range are not on a grid aligned with the
#' library's (an instrument's own axis, which can share a few columns by
#' coincidence), are refused, naming the [standardize()] call that puts them
#' on the library's grid; for a registered library that is checked before
#' the download. `x$selection$search` records the search axis
#' and `x$selection$reconciliation` the columns returned.
#'
#' **Coverage.** The targets should span the library's range. Within half
#' the library's spacing, or less than one of their own grid steps (a coarse
#' grid cannot land on 600 or 4000), they do, and the cached space is used.
#' Short by more at an end, however much (an ATR crystal's cutoff, a trimmed
#' noisy edge, a batch trimmed to one region), the space is built fresh on
#' the library's columns inside the targets' range, uncached, with one
#' `horizons_warning_coverage` saying what was missing and what was searched.
#' A `window` that does not fit in that overlap is refused, in cm-1.
#'
#' **The cached space.** A registered library's space at the default
#' settings is built right after the library itself, at first use, and
#' cached beside it; any other setting is built at its first draw and cached
#' then, one small file per setting under
#' `tools::R_user_dir("horizons", "cache")`. A library file's space is cached
#' the same way, keyed by the file's hash. A pool passed in memory, a PLS
#' space and an overlap space are built every call and not cached.
#'
#' **Axes the search cannot use.** Either axis with fewer than two
#' wavenumbers, or carrying a hole (a spacing more than three times its own
#' median and more than 30 cm-1), is refused before anything interpolates,
#' the targets' before the library resolves: a resampling spline and a
#' Savitzky-Golay window both run straight across a gap and invent
#' absorbance at its edges without erroring, and a deleted water band
#' belongs in `mask`, which is applied after the derivative. `window` is a
#' width in cm-1, so
#' the filter is the same physical filter on any batch; the point count it
#' becomes on the library's grid is recorded in `settings$window_points`, and
#' the width that count actually spans in `settings$window_cm`.
#'
#' **Standardized alike.** The training set is the library's spectra as
#' [standardize()] left them, so the targets must have been left the same
#' way: baseline-corrected on both sides or neither, and when both, with the
#' hulls fitted over the same range (each end within the coarser grid's
#' step). Otherwise the model would train on one preprocessing and predict
#' another, and the call is refused before any work, naming the
#' [standardize()] call that matches the targets to the library (#107). A
#' side that never went through [standardize()] counts as not corrected.
#' The registered `"kssl"` library is not baseline-corrected.
#'
#' **Self-leakage.** A pool row that sits far closer to a target than that
#' target's surroundings do — distance zero, or below `twin_ratio` times the
#' reference distance — is the target's twin: the same sample, present in
#' both sets or scanned twice. The reference is the 75th percentile of the
#' target's 50 nearest rows with the property measured, or of a quarter of
#' the measured rows when fewer than 200 have it: a fixed width rather than
#' the `k` being drawn, so a cluster of replicate scans cannot set the very
#' number it is measured against. The exclusion is recorded with its
#' target, property, distance and reason. Except under `scope = "global"`,
#' no target takes any target's twin, under any property, since a row
#' excluded from one neighbourhood would otherwise walk back into the
#' returned object through any other target that drew it: every target
#' takes its first `k` rows outside the twins, spares fetched for any it
#' passed, and one that still cannot reach `k` is recorded in
#' `x$selection$short_draws` and warned about. Under `scope = "global"` the
#' same check runs, per property, in
#' the same space and with the same width, and its exclusions and target
#' distances are the ones a batch-scope call with the same arguments
#' records. The twins are recorded but stay in the returned rows, because
#' global returns every row the others draw from, so a comparison that
#' scores global against batch has to act on `x$selection$exclusions`
#' itself. The membership table keeps the twins a target passed with
#' `retained = FALSE` rather than dropping them, so the record still says
#' what each neighbourhood was, and a target's `target_distances` are over
#' the rows it kept. When the subtraction leaves a property with
#' no rows at all (every row drawn for it was some target's twin), the
#' property is left out with one `horizons_warning_partial_failure` saying
#' why, and the call fails with `horizons_error_selection_failed` only when
#' it leaves every property empty.
#'
#' **Unusable spectra.** A target or library spectrum an earlier step has
#' flagged (`.flag`, see [standardize()]) is left out of the similarity
#' space and the draw. A flagged target gets no training set and is named in
#' one `horizons_warning_flagged`; flagged library rows are left out as
#' [evaluate()] and [fit()] leave them out, with a line in the verbose tree.
#' The call is refused only when every target, or every library row, is
#' flagged.
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
#' `horizons.cache_dir` moves the cache. Every later draw reads the cache,
#' until a version of horizons that builds the library differently: a
#' library cached by an earlier build is built again, after asking as on
#' first use, and the log notes it. `x$selection$library` records which
#' library was drawn from, and its `build`.
#'
#' **What the record holds** (`x$selection`): the settings as resolved, the
#' columns returned, the pool's identity, the targets' identity (their source,
#' row count and a hash of their ids; the source is the paths their
#' [spectra()] note names, `"data frame"` when they were read from one, and
#' `NULL` when they were not made by `spectra()`), the membership table (target by
#' property by pool row, with the space it was measured in, distance, rank,
#' and whether the row was `retained` in the object after the twin
#' subtraction), the groups, pool sizes per property against those available, every
#' target's nearest and mean-of-k distance (the applicability signal at
#' selection time), the unit comparison, the twins excluded and how many of
#' them left the union, any target that could not reach `k`, and the
#' clustering when `scope = "cluster"`.
#'
#' @param x `horizons_data.` The targets: the samples to be predicted. One
#'   row per sample, with finite spectra: the record names each target by its
#'   `sample_id`, so replicate scans that share one are refused and should be
#'   averaged first with [average()]. On a grid that shares columns with the
#'   library's, as [standardize()] puts them (see "Two axes"), and
#'   standardized as the library was (see "Standardized alike").
#' @param library `horizons_data` or `character`. The reference library:
#'   a `horizons_data` with one or more response columns (your own pool,
#'   `spectra() |> standardize() |> add_response()`), the name of a
#'   registered library (`"kssl"`), or a path to a library file. A pool that
#'   has been through [configure()] or [validate()] is refused, since the
#'   training set would inherit that state; pass it as it was before. A
#'   registered library is built on your machine from its public sources the
#'   first time it is used, after asking, and cached under
#'   `tools::R_user_dir("horizons", "cache")`; see "Registered libraries".
#' @param k `integer.` Neighbours per target per property; a scalar, or a
#'   named vector with one entry per property. 100 is the 2026-09-29 defaults
#'   read: 400 was worse on every batch from another instrument and no better
#'   on KSSL. Default: `100`.
#' @param scope `character.` `"batch"`, `"cluster"`, `"sample"` or
#'   `"global"`. Default: `"batch"`.
#' @param properties `character or NULL.` Response columns of the pool to
#'   draw for. `NULL` means all of them. A property named here needs at
#'   least one measured pool row at the depth asked for, under every scope,
#'   and at least `k` under the drawing scopes, or the call is refused;
#'   under `NULL` a response short of either is skipped, with one
#'   `horizons_warning_skipped`, and the call is refused only when none is
#'   left. Default: `NULL`.
#' @param snv `logical.` SNV in the similarity space. Default: `TRUE`.
#' @param derivative `integer.` Savitzky-Golay derivative order in the
#'   similarity space; `0` disables the filter. Default: `1`.
#' @param window `numeric.` Savitzky-Golay window width in cm-1, measured
#'   between the outermost points, so the filter means the same thing on any
#'   grid: 40 cm-1 is 11 points at 4 cm-1 and 21 at 2 cm-1. It becomes the
#'   nearest odd number of points on the grid the space is built on, the
#'   library's (ties round up), and the record carries both. Default: `40`.
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
#' @param seed `integer.` Seed for every stochastic step of the verb: the
#'   target clustering under `scope = "cluster"`, and the pool sample the
#'   resemblance check draws its reference distribution from. The caller's
#'   RNG state is saved and restored, so the verb does not advance it.
#'   Default: `307`, as the other verbs'.
#' @param control A [control_horizons()] object: how the run goes, not what
#'   it computes. `select_training()` reads `verbose`, `debug` (a failure
#'   building a registered library's first-use similarity space stops the
#'   call, rather than being left to the first draw), `chunk_size` (targets
#'   per distance chunk) and `min_resemblance_rows` (the fewest library rows
#'   the resemblance check takes its reference distribution from), and
#'   refuses any other field that is set. The control it ran with, defaults
#'   filled in, is recorded as `selection$settings$control`.
#'
#' @return `horizons_data.` Rows drawn from `pool`, on the library's own
#'   columns at the wavenumbers the targets share with it, with every
#'   response column the pool carried, three provenance columns with role
#'   `meta` (`.drawn_by`, `.min_distance`, and `.group` except under
#'   `scope = "sample"`), and the record in `x$selection`, whose
#'   `settings$properties` are the properties drawn for (a skipped or
#'   emptied property is not among them, so [configure()] warns when one is
#'   chosen as the outcome).
#'   `data$standardization` is the library's, describing the returned
#'   axis: when only some of the library's columns are returned (targets on a
#'   coarser or narrower grid), `wavelength_range` and `grid` describe the
#'   columns returned (`grid` is `NULL` when they are not evenly spaced, or
#'   the library was on no grid), while `baseline` and `baseline_range` stay
#'   the library's, since its hull is in the values. The library's
#'   `standardize()` settings are in its note on the log, so the `resample`
#'   it gives can differ from `grid$step`. A library never standardized has
#'   no record, and the training set gets none. `x$selection$reconciliation`
#'   records the columns returned: `operation` (`"none"` when every library
#'   column is returned, else `"subset"`), `n_shared`, `shared_range`, and
#'   `target_only`, the targets' wavenumbers the library lacks. `x$log` is the
#'   pool's, with this call's rows after it: one per warning topic, each
#'   raised once at the end of the call (`horizons_warning_input` for pool
#'   and targets in different photometric units; `flagged` for targets an
#'   earlier step flagged, which get no training set; `coverage` for target
#'   columns the library lacks, a search on the overlap and draws short of
#'   `k`; `skipped` for a property left out under `properties = NULL` (no
#'   measured rows, or fewer than `k`), the resemblance check not run, and a
#'   similarity space or library that could not be cached; `partial_failure`
#'   for a property every drawn row of which was some target's twin, left
#'   out, so the call did not complete in full; `applicability` for targets
#'   beyond the library's spread), and a note for each cost paid once (a
#'   cached library or similarity space rebuilt, a first-use space not
#'   built).
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
                            seed        = 307L,
                            control     = control_horizons()) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  ## The frame every refusal reports, including those raised in the helpers
  ## the verb hands work to
  call <- rlang::current_env()

  ## How the run goes: the fields select_training() reads, defaults filled
  ## in, and the record of it on the selection
  control    <- resolve_control(control, "select_training")
  chunk_size <- control$chunk_size
  verbose    <- control$verbose

  ## The call's warnings, written to the returned object's log and raised
  ## once per topic at the end (log_close(), signal_log())
  collector <- log_open("select_training")

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

  ## The similarity space's levers, and the draw's own, are checked here
  ## rather than where they are used, so none of them is first found wrong
  ## after a registered library has been downloaded and built.

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

  ## library's form: a horizons_data, a library object holding one, a
  ## registered name, or the path of a file that exists. Checked with the
  ## other arguments, so a mistyped name or a missing file is reported with
  ## them, before the targets are validated or anything is downloaded.

  registry   <- names(library_registry())
  registered <- FALSE

  if (is.character(library)) {

    if (length(library) != 1L || is.na(library) || !nzchar(library)) {

      errors <- c(errors, cli::format_inline("{.arg library} must be a single name or path, not {.obj_type_friendly {library}}"))

    } else if (library %in% registry) {

      registered <- TRUE

    } else if (!grepl("[/\\\\]", library) && !grepl("\\.[A-Za-z0-9]+$", library) &&
               !file.exists(library)) {

      ### No path separator, no file suffix and nothing on disk: a bare word
      ### is a mistyped name, and saying so beats "file not found"
      errors <- c(errors, cli::format_inline(
        "{.val {library}} is not a registered library (registered: {.val {registry}}); for a library file, pass its path"))

    } else if (!file.exists(library)) {

      errors <- c(errors, cli::format_inline(
        "The library file {.path {library}} does not exist; check the path, or pass a registered name ({.val {registry}})"))

    }

  } else if (inherits(library, "horizons_library")) {

    if (!inherits(library$pool, "horizons_data")) {

      errors <- c(errors, cli::format_inline(
        "{.arg library} is a {.cls horizons_library} whose pool is {.obj_type_friendly {library$pool}}, not a horizons_data; rebuild it, or pass the pool itself"))

    }

  } else if (!inherits(library, "horizons_data")) {

    errors <- c(errors, cli::format_inline(
      "{.arg library} must be a horizons_data, the name of a registered library ({.val {registry}}), or a path to a library file, not {.obj_type_friendly {library}}"))

  }

  ## The window becomes points on the library's grid. A registered library's
  ## grid is known from the registry, so a window too narrow for poly is
  ## caught here, before the download; any other library is in hand already
  ## and is checked once it resolves (Step 2), before anything is built.

  if (registered && window_ok && deriv_ok && poly_ok && derivative > 0) {

    lib_res <- library_registry()[[library]]$filters$wn_step
    pts     <- window_to_points(window, lib_res)

    if (pts <= poly) {

      errors <- c(errors, cli::format_inline(
        "A {.arg window} of {window} cm\u207B\u00B9 is {pts} point{?s} on the {library} library's {lib_res} cm\u207B\u00B9 grid, too few for a polynomial of order {poly}; widen {.arg window} or lower {.arg poly}"))

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
  if (!is_count(seed, min = -.Machine$integer.max) || abs(seed) > .Machine$integer.max) {

    errors <- c(errors, cli::format_inline("{.arg seed} must be a single whole number within R's integer range"))

  }

  ## The library resolves only after everything that can be checked without
  ## it has passed: a registered library that is not cached yet is downloaded
  ## and built here, and a mistyped argument must not cost that.

  if (length(errors) > 0) abort_select_inputs(errors)

  ## The targets are certified before the library resolves, so a target set
  ## the verb cannot use does not cost a download. At the full stage, not
  ## raw: the similarity space needs finite spectra, and the record names
  ## each target by its sample_id (membership, target_distances, groups), so
  ## replicate scans sharing an id would be merged there; average() them
  ## first.

  x <- validate_select_input(x, arg = "x", what = "the targets")

  ## The targets' axis needs nothing from the library, so it is checked now:
  ## two columns or more, and no gap (check_select_axis()).
  check_select_axis(predictor_wavenumbers(x), "x", "the targets")

  ## A target an earlier step flagged gets no training set (decision 5), so
  ## targets that are all flagged leave nothing to draw for.

  target_flagged <- flagged_rows(x$data$analysis)

  if (all(target_flagged)) {

    abort_horizons(c(
      "Every target was flagged by an earlier step, so there is nothing to draw for.",
      "i" = "{.field .flag} names the step that could not use each one."
    ), kind = "input")

  }

  ## A fixed cluster count needs as many targets to cluster, and a flagged
  ## target is not one, so this too is refused before the library resolves

  n_drawn_for <- sum(!target_flagged)

  if (scope == "cluster" && !is.null(clusters) && clusters > n_drawn_for) {

    abort_horizons(c(
      "{.arg clusters} = {clusters} is more clusters than the {n_drawn_for} target{?s} to draw for.",
      "i" = if (any(target_flagged)) "{sum(target_flagged)} target{?s} flagged by an earlier step {?is/are} not counted; {.field .flag} names the step.",
      "i" = "Lower {.arg clusters}, or leave it {.code NULL} to choose the count by silhouette."
    ), kind = "input")

  }

  target_vars   <- x$data$role_map$variable[x$data$role_map$role == "predictor"]
  restandardize <- !is.null(x$data$standardization)

  ## A registered library's columns and baseline are known from the
  ## registry, which build_ossl_library() standardizes it with, so targets
  ## off its grid or standardized differently are refused before the
  ## download, as a window too narrow is above. Both checks run again on
  ## the resolved pool below.

  if (registered) {

    reg_grid <- library_registry()[[library]]$filters
    reg_vars <- paste0("wn_", canonical_grid(reg_grid$wn_min, reg_grid$wn_max, reg_grid$wn_step))

    check_shared_columns(target_vars, reg_vars, restandardize = restandardize,
                         library_form = "registered", library_baseline = isTRUE(reg_grid$baseline))

    ### The record build_ossl_library()'s standardize() writes, whose hull,
    ### when it fits one, spans the grid
    reg_range <- c(reg_grid$wn_min, reg_grid$wn_max)

    check_same_standardization(list(baseline         = isTRUE(reg_grid$baseline),
                                    baseline_range   = if (isTRUE(reg_grid$baseline)) reg_range,
                                    wavelength_range = reg_range),
                               x$data$standardization,
                               pool_step     = reg_grid$wn_step,
                               target_step   = grid_summary(predictor_wavenumbers(x))$resolution,
                               library_form  = "registered",
                               restandardize = restandardize)

  }

  resolved <- resolve_source(library, verbose = isTRUE(verbose), collector = collector,
                             debug = control$debug, call = call)
  pool     <- validate_select_input(resolved$pool, arg = "library", what = "the pool")
  errors   <- check_pool_unpromoted(pool)

  ## The library's rows an earlier step flagged are left out of the space
  ## and the draw (decision 5), so a library that is all flagged has none.

  pool_flagged <- flagged_rows(pool$data$analysis)

  if (all(pool_flagged)) {

    errors <- c(errors, cli::format_inline(
      "Every row of {.arg library} was flagged by an earlier step, so there is nothing to draw from; {.field .flag} names the step"))

  }

  ## Properties: the pool's responses ---------------------------------------

  ## A property the user named is refused when it cannot be drawn for; one
  ## taken from the pool's responses (properties = NULL) is skipped
  named     <- !is.null(properties)
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

  ## One rule for a named k, which resolve_k() relies on below
  errors <- c(errors, missing_k(k, properties))

  if (identical(space, "pls") && length(properties) != 1L) {

    errors <- c(errors, cli::format_inline("{.arg space = \"pls\"} selects against one property; name exactly one in {.arg properties}"))

  }

  ## A prior selection's provenance columns would collide on bind_cols ------

  prior <- intersect(c(".drawn_by", ".min_distance", ".group"), names(pool$data$analysis))

  if (length(prior)) {

    errors <- c(errors, cli::format_inline(
      "{.arg library} already carries the provenance column{?s} {.val {prior}}, so it is itself a selection; pass the library as it comes from standardize()"))

  }

  if (length(errors) > 0) abort_select_inputs(errors)

  ## The library's axis, by the targets' rule ---------------------------------

  check_select_axis(predictor_wavenumbers(pool), "library", "the pool")

  ## The columns the training set will have (#236) ---------------------------

  ## The library's own columns at the wavenumbers the targets share with it,
  ## nothing interpolated. Targets that share none are refused; the targets'
  ## columns the library lacks are reported, since predict() uses only the
  ## columns the model was trained on.

  pool_vars <- pool$data$role_map$variable[pool$data$role_map$role == "predictor"]
  cols      <- check_shared_columns(target_vars, pool_vars, restandardize = restandardize,
                                    library_form         = resolved$record$form,
                                    library_standardized = !is.null(pool$data$standardization),
                                    library_baseline     = isTRUE(pool$data$standardization$baseline))

  ## Standardized alike (#107): a hull on one side only, or hulls fitted over
  ## different ranges, would train on one preprocessing and predict another.

  check_same_standardization(pool$data$standardization, x$data$standardization,
                             pool_step     = grid_summary(predictor_wavenumbers(pool))$resolution,
                             target_step   = grid_summary(predictor_wavenumbers(x))$resolution,
                             library_form  = resolved$record$form,
                             restandardize = restandardize)

  if (length(cols$target_only)) {

    lib_wn  <- predictor_wavenumbers(pool)
    only_wn <- as.numeric(sub("^wn_", "", cols$target_only))
    outside <- sum(only_wn < min(lib_wn) - GRID_TOL | only_wn > max(lib_wn) + GRID_TOL)
    between <- length(only_wn) - outside
    lib_lo  <- min(lib_wn)
    lib_hi  <- max(lib_wn)
    lib_res <- signif(grid_summary(lib_wn)$resolution, 4)

    log_warning(collector, "coverage", c(
      "{length(cols$target_only)} of the targets' {length(target_vars)} wavenumber columns are not in the library, so the training set has the {length(cols$shared)} they share.",
      "i" = if (outside > 0) "{outside} lie outside the library's {lib_lo} to {lib_hi} cm-1.",
      "i" = if (between > 0) "{between} fall between the library's columns, which are {lib_res} cm-1 apart; standardize the targets at a multiple of {lib_res} cm-1 to share every column.",
      "i" = "{.fn predict} uses only the columns the model was trained on, so the targets predict as they are."
    ), ids = cols$target_only)

  }

  k_by <- resolve_k(k, properties)

  if (verbose) cat(paste0("\u251C\u2500 ", cli::style_bold("Selecting training set"), "...\n"))

  ## ---------------------------------------------------------------------------
  ## Step 1: The two axes
  ## ---------------------------------------------------------------------------

  ## Distances are measured in the library's own space, on the library's
  ## grid, so one space (cached, for a registered library or a library file)
  ## serves every batch; the targets are resampled onto it for the search
  ## only. The training set comes back on the library's own columns at the
  ## wavenumbers the targets share (Step 5), checked at the door above.

  pm <- predictor_matrix(pool)
  tm <- predictor_matrix(x)

  ## Rows an earlier step flagged leave the space and the draw first, as
  ## every verb that computes over predictors leaves them (decision 5): they
  ## may hold values the search's resampling cannot take. A target left out
  ## gets no training set, and is named; the library's are its modelling
  ## data, left out as evaluate() and fit() leave flagged rows, with a line
  ## in the tree.

  ## Only the columns the call reads, and the matrices copied only when a
  ## row leaves them: a library's analysis table and its matrix are each
  ## about a gigabyte on KSSL.

  if (any(pool_flagged))   pm$matrix <- pm$matrix[!pool_flagged, , drop = FALSE]
  if (any(target_flagged)) tm$matrix <- tm$matrix[!target_flagged, , drop = FALSE]

  read_cols <- intersect(c("sample_id", properties, "upper_depth_cm"), names(pool$data$analysis))
  usable    <- pool$data$analysis[!pool_flagged, read_cols, drop = FALSE]

  if (any(target_flagged)) {

    left_targets <- x$data$analysis$sample_id[target_flagged]

    log_warning(collector, "flagged", c(
      "{length(left_targets)} target{?s} flagged by an earlier step {?was/were} left out, so no training set was drawn for {?it/them}: {shown_ids(left_targets)}.",
      "i" = "{.field .flag} names the step that could not use each one."
    ), ids = left_targets)

  }

  sa <- search_axis(pm$wavenumbers, tm$matrix, tm$wavenumbers)

  ## Targets short of the library's range are searched on the overlap, in a
  ## space built fresh and not cached. The warning's ids are the library
  ## columns the search left out.
  if (sa$record$mode == "overlap") {

    left_out   <- pool_vars[-sa$pool_cols]
    short_low  <- signif(sa$record$missing[["low"]],  4)
    short_high <- signif(sa$record$missing[["high"]], 4)
    searched   <- range(sa$wavenumbers)

    log_warning(collector, "coverage", c(
      "The targets fall short of the library's range, so the similarity space was built on the overlap and not cached.",
      "i" = "Short by {short_low} cm-1 at the low end and {short_high} at the high end; searched on the library's {length(sa$wavenumbers)} columns from {searched[1]} to {searched[2]} cm-1.",
      "i" = "Standardize the targets over the library's range to use the cached space."
    ), ids = left_out)

  }

  if (verbose) {

    search_res <- sa$record$library_grid$resolution
    cat(paste0("\u2502  \u251C\u2500 Pool: ", nrow(pm$matrix), " rows; searched on its own ",
               search_res, " cm\u207B\u00B9 grid",
               if (sa$record$mode == "overlap") " (the overlap with the targets)" else "",
               ", targets ", if (sa$record$operation == "none") "already on it" else "resampled onto it",
               "\n"))

    if (any(pool_flagged)) {

      cat(paste0("\u2502  \u251C\u2500 ", sum(pool_flagged), " flagged row", if (sum(pool_flagged) > 1) "s",
                 " of the library left out (see .flag)\n"))

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Build the similarity space on the pool, project the targets
  ## ---------------------------------------------------------------------------

  ## One space, fit on every library row: the draw's space for every
  ## property, and the one target clustering and the resemblance check use,
  ## so none of them depends on which property is being drawn. A space per
  ## property on its measured rows (space_rows = "measured") was removed on
  ## 2026-09-30: on KSSL it moved the drawn training set by about 6 %.

  pool_ids <- usable$sample_id
  resp_tbl <- usable[, c("sample_id", properties), drop = FALSE]

  ## Depth restricts the draw, not the space. A subsoil row stays in the
  ## space: the library is the population, and whether a row is subsoil
  ## should not redefine what similar soil means.
  ## Under "topsoil" its responses are hidden from the draw, which is how the
  ## draw already treats a row without the property: the draw, the twin
  ## reference and the resemblance check see only eligible rows. A row with
  ## no recorded depth is not known to be topsoil and is not eligible.
  ## A library that records no depth at all has nothing to restrict on, and
  ## the record says so rather than the default refusing a user's own pool.

  depth_recorded <- "upper_depth_cm" %in% names(usable)
  depth_applied  <- identical(depth, "topsoil") && depth_recorded
  eligible       <- rep(TRUE, length(pool_ids))

  if (depth_applied) {

    upper <- usable$upper_depth_cm

    ## A character column compares as text, where "250" < 30 is TRUE, and
    ## the cut would silently let every depth through while saying it applied.

    if (!is.numeric(upper)) {

      abort_horizons(c(
        "{.field upper_depth_cm} must be numeric for {.code depth = \"topsoil\"}; it is {.cls {class(upper)[1]}}.",
        "i" = "Convert it to centimetres as numbers, or pass {.code depth = \"all\"}."
      ), kind = "input")

    }

    eligible <- !is.na(upper) & upper < SELECT_TOPSOIL_MAX_CM

    for (p in properties) resp_tbl[[p]][!eligible] <- NA

  }

  ## The property is the unit (decision 5) -----------------------------------

  ## A property with no measured rows, or, under the drawing scopes, fewer
  ## than k, has nothing to draw from: refused when the user named it,
  ## skipped with a warning when it came with the pool's responses. Global
  ## draws nothing, so k only sets the reach of mean_k there and is capped
  ## at the measured rows (Step 3).

  n_measured <- vapply(properties, function(p) sum(!is.na(resp_tbl[[p]])), integer(1))
  unmeasured <- properties[n_measured < 1L]
  thin       <- if (scope == "global") character() else setdiff(properties[n_measured < k_by[properties]], unmeasured)

  depth_hint <- if (depth_applied) c("i" = "Only topsoil rows (upper depth < {SELECT_TOPSOIL_MAX_CM} cm) count under {.code depth = \"topsoil\"}; {.code depth = \"all\"} draws from every depth.")
  thin_rows  <- cli_escape(sprintf("%s: %d measured row%s, k = %d.", thin, n_measured[thin],
                                   ifelse(n_measured[thin] == 1L, "", "s"), k_by[thin]))

  if (named && length(unmeasured)) {

    abort_horizons(c(
      "{.arg library} has no measured rows for {.field {unmeasured}}.",
      "i" = "Drop {cli::qty(unmeasured)}{?it/them} from {.arg properties}, or add the lab values to the library first.",
      depth_hint
    ), kind = "input")

  }

  if (named && length(thin)) {

    abort_horizons(c(
      "{.arg library} has fewer measured rows than {.arg k} for {.field {thin}}.",
      stats::setNames(thin_rows, rep("x", length(thin))),
      "i" = "Lower {.arg k} for {cli::qty(thin)}{?it/them} (a named {.arg k} sets one per property), or bring more measured rows into the library.",
      depth_hint
    ), kind = "input")

  }

  if (length(unmeasured)) {

    log_warning(collector, "skipped", c(
      "No training rows were drawn for {.field {unmeasured}}: the library has no measured rows for {cli::qty(unmeasured)}{?it/them}.",
      "i" = "Add the lab values to the library, or name the properties to draw for in {.arg properties}.",
      depth_hint
    ), ids = unmeasured)

  }

  if (length(thin)) {

    log_warning(collector, "skipped", c(
      "No training rows were drawn for {.field {thin}}: the library has fewer measured rows than {.arg k} for {cli::qty(thin)}{?it/them}.",
      stats::setNames(thin_rows, rep("*", length(thin))),
      "i" = "A named {.arg k} sets one per property.",
      depth_hint
    ), ids = thin)

  }

  properties <- setdiff(properties, c(unmeasured, thin))

  if (!length(properties)) {

    abort_horizons(c(
      "None of the library's responses can be drawn for.",
      "x" = if (length(unmeasured)) "No measured rows: {.field {unmeasured}}.",
      stats::setNames(thin_rows, rep("x", length(thin))),
      "i" = "Add the lab values to the library, or lower {.arg k}.",
      depth_hint
    ), kind = "input")

  }

  resp_tbl   <- resp_tbl[, c("sample_id", properties), drop = FALSE]
  n_measured <- n_measured[properties]
  k_by       <- k_by[properties]

  ## A PLS space is fit on the property's measured rows at every depth, and
  ## needs more of them than components

  if (space == "pls") {

    n_fit <- sum(!is.na(usable[[properties]]))

    if (n_fit <= ncomp) {

      abort_horizons(c(
        "A PLS similarity space of {ncomp} component{?s} needs more measured rows of {.field {properties}} than that; the library has {n_fit}.",
        "i" = "Lower {.arg ncomp}, or use {.code space = \"pca\"}."
      ), kind = "input")

    }

  }

  ## Units, before SNV erases the evidence ----------------------------------

  lib_m <- if (sa$record$mode == "full") pm$matrix else pm$matrix[, sa$pool_cols, drop = FALSE]
  units <- check_photometric_units(lib_m, sa$matrix)

  if (units$mismatch) {

    log_warning(collector, "input", c(
      "The pool and the targets look like different photometric units or modes.",
      "i" = "Pool absorbance: median {signif(units$pool_median, 3)}, IQR {signif(units$pool_iqr, 3)}.",
      "i" = "Targets: median {signif(units$target_median, 3)}, IQR {signif(units$target_iqr, 3)}.",
      "i" = "Selection proceeds, but a model trained on one unit cannot predict the other."
    ))

  }

  ## The window is a width in cm-1; the filter counts points on the grid the
  ## space is built on, which is the library's. Nearest odd count, ties up.

  search_res <- sa$record$library_grid$resolution

  window_points <- if (derivative > 0) window_to_points(window, search_res) else NA_integer_

  if (derivative > 0 && window_points <= poly) {

    abort_horizons(c(
      "A {.arg window} of {window} cm\u207B\u00B9 is {window_points} point{?s} on the library's {search_res} cm\u207B\u00B9 grid, too few for a polynomial of order {poly}.",
      "i" = "Widen {.arg window} or lower {.arg poly}."
    ), kind = "input")

  }

  ## The filter needs fewer points than the search grid has columns. Targets
  ## that cover only a short stretch of the library are searched on that
  ## overlap alone, which can be narrower than the window.

  n_search <- length(sa$wavenumbers)

  if (derivative > 0 && !sg_window_fits(window_points, n_search)) {

    searched <- range(sa$wavenumbers)
    span     <- searched[2] - searched[1]

    abort_horizons(c(
      "A {.arg window} of {window} cm\u207B\u00B9 does not fit in the {span} cm\u207B\u00B9 of the library the targets cover.",
      "x" = "The similarity space is built on the library's {n_search} column{?s} from {searched[1]} to {searched[2]} cm\u207B\u00B9, and the window is {window_points} points on them; the filter needs fewer.",
      "i" = "Narrow {.arg window} below {span} cm\u207B\u00B9, or standardize the targets over more of the library's range."
    ), kind = "input")

  }

  space_settings <- list(snv = snv, derivative = as.integer(derivative),
                         ## With no derivative the filter never runs and its
                         ## window is unread; any valid value will do.
                         window = if (derivative > 0) window_points else 11L,
                         poly = as.integer(poly), mask = mask, ncomp = ncomp,
                         sdev_floor = sdev_floor)

  build_space <- function() {

    build_similarity_space(lib_m, sa$wavenumbers,
                           snv = space_settings$snv, derivative = space_settings$derivative,
                           window = space_settings$window, poly = space_settings$poly,
                           mask = mask, space = space, ncomp = ncomp, sdev_floor = sdev_floor,
                           y = if (space == "pls") usable[[properties]] else NULL,
                           call = call)

  }

  ## Cached when the space depends on the library alone: a PCA space, on the
  ## library's whole grid, of a library with an identity to key it by. A PLS
  ## space is fit against one property, an overlap space is one batch's, and
  ## a pool passed in memory is not hashed (see R/select-cache.R).

  identity <- if (space == "pca" && sa$record$mode == "full") space_cache_identity(resolved$record) else NULL

  if (!is.null(identity)) {

    cs <- cached_space(identity, space_settings, sa$wavenumbers, build_space, verbose = verbose,
                       collector = collector)

    ### The space's rows are the library's rows in the library's order; a
    ### cached space that disagrees is not this library's and is rebuilt,
    ### which costs only time, so it is a note.

    if (!identical(rownames(cs$space$scores), pool_ids)) {

      log_note(collector, "The cached similarity space at {.path {cs$path}} did not match this library's rows, so it was rebuilt over.")

      if (verbose) cat("\u2502  \u251C\u2500 The cached similarity space did not match this library's rows; rebuilding it\n")

      cs <- cached_space(identity, space_settings, sa$wavenumbers, build_space,
                         verbose = verbose, force = TRUE, collector = collector)

    }

    sp    <- cs$space
    cache <- list(used = TRUE, hit = cs$hit, key = cs$key, path = cs$path)

  } else {

    sp    <- build_space()
    cache <- list(used = FALSE, hit = FALSE, key = NULL, path = NULL,
                  reason = if (space == "pls") "pls space" else if (sa$record$mode != "full") "overlap space" else "pool passed in memory")

  }

  rm(lib_m)

  St <- project_similarity(sp, sa$matrix, sa$wavenumbers)

  ## The width the filter actually spans once rounded to whole points.

  window_cm <- if (derivative > 0) (window_points - 1L) * search_res else NA_real_

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

    ### How much the mask took, from the columns the filter's edge trim
    ### left and the columns the space kept. Most of the space masked is
    ### allowed, and is most often ranges given in another unit.

    if (!is.null(mask)) {

      n_after_trim <- length(sa$wavenumbers) - (if (derivative > 0) window_points - 1L else 0L)
      n_masked     <- n_after_trim - length(sp$wavenumbers)

      cat(paste0("\u2502  \u251C\u2500 Mask: removed ", n_masked, " of the ", n_after_trim,
                 " columns of the similarity space",
                 if (n_masked > 0.9 * n_after_trim) "; over 90 %, so check the ranges are in cm\u207B\u00B9, low then high" else "",
                 "\n"))

    }

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
  ## twins stay in the return, because global returns every row the other
  ## scopes draw from, by definition.
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
  ## scopes refused or skipped such a property in Step 2.

  k_draw <- if (scope == "global") pmin(k_by, n_measured) else k_by

  draw <- draw_neighbours(St, sp$scores, responses = resp_tbl, k = k_draw,
                          properties = properties, metric = metric, sdev = sp$sdev,
                          chunk_size = chunk_size, twin_ratio = twin_ratio,
                          space_label = "all")

  if (scope == "global") {

    draw$membership  <- tibble::tibble(target_id = character(), property = character(),
                                       space = character(), pool_id = character(),
                                       distance = numeric(), rank = integer(),
                                       retained = logical())
    draw$short_draws <- empty_short_draws()

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

  ## A twin excluded from one target's neighbourhood would walk straight back
  ## into the union through any other target whose neighbourhood it falls
  ## in, so the draw gives no target any target's twin, and fetches spares
  ## for the targets that pass one (#149). That holds under sample too: the
  ## return is one object, nothing downstream consumes selection$groups yet,
  ## and the ordinary configure |> evaluate |> fit on a sample-scope return
  ## would otherwise train on every target's own replicates. Only global
  ## keeps them, because global returns every row the others draw from, by
  ## definition, and says so. membership keeps the twins a target passed,
  ## marked retained = FALSE, as the record of what each neighbourhood was,
  ## and the union is its retained rows: the subtraction below counts the
  ## twins it holds.

  excluded_ids <- unique(draw$exclusions$pool_id)
  union_ids    <- if (scope == "global") pool_ids[eligible] else pool_ids[pool_ids %in% membership$pool_id]

  n_excluded_union <- 0L

  if (scope != "global" && length(excluded_ids)) {

    left             <- setdiff(union_ids, excluded_ids)
    n_excluded_union <- length(union_ids) - length(left)
    union_ids        <- left

  }

  ## A property whose every drawn row was some target's twin has no training
  ## rows left. The property is the unit (decision 5): it is left out with a
  ## partial_failure warning saying why, and the call fails only when every
  ## property is emptied. At a twin_ratio near 1 the threshold reaches
  ## ordinary neighbours and this is what happens, so the warning says so.

  emptied <- character()

  if (scope != "global") {

    emptied <- properties[vapply(properties, function(p) {

      drawn <- unique(membership$pool_id[membership$property == p])
      length(drawn) > 0L && !any(drawn %in% union_ids)

    }, logical(1))]

  }

  if (length(emptied)) {

    ### Whether a lower twin_ratio would bring a property's rows back. It
    ### cannot when every row drawn for it is some target's own copy, at
    ### distance zero to rounding, below any ratio. is_exact_copy() is the
    ### rule, the same one that writes "exact" into the record's reason
    ### column.

    ex      <- draw$exclusions
    copies  <- ex$pool_id[is_exact_copy(ex$distance, ex$reference_distance)]
    is_copy <- vapply(emptied, function(p) {
      all(unique(membership$pool_id[membership$property == p]) %in% copies)
    }, logical(1))

    causes <- cli::ansi_strip(ifelse(
      is_copy,
      cli::format_inline("Each row was a target's own copy, at distance zero, so the targets are in the pool; a lower {.arg twin_ratio} cannot help. Remove the targets from the pool."),
      cli::format_inline("A twin sits below {.arg twin_ratio} = {twin_ratio} times its target's reference distance, and at this value ordinary neighbours are being flagged; lower it (the default is {SELECT_TWIN_RATIO}).")
    ))

    ### One bullet per cause, naming its properties
    by_cause <- split(emptied, causes)
    bullets  <- paste0(vapply(by_cause, paste, character(1), collapse = ", "), ": ", names(by_cause))
    bullets  <- stats::setNames(cli_escape(bullets), rep("i", length(bullets)))

    if (length(emptied) == length(properties)) {

      abort_horizons(c(
        "Every row drawn for {.field {emptied}} was flagged as some target's twin, so there is no training set.",
        bullets,
        "i" = "The causes, one row per property, are on this condition as {.field results}: {.code rlang::last_error()$results}."
      ), kind = "run", class = "horizons_error_selection_failed",
         results = tibble::tibble(property = unname(emptied), cause = unname(causes)))

    }

    log_warning(collector, "partial_failure", c(
      "Every row drawn for {.field {emptied}} was flagged as some target's twin, so {cli::qty(emptied)}{?it was/they were} left out of the training set.",
      bullets
    ), ids = emptied)

  }

  groups <- build_groups(group_of_target, membership,
                         if (scope == "global") pool_ids[eligible] else union_ids, scope)

  ## The drawn rows, on the library's own columns at the wavenumbers the
  ## targets share (#236): the other predictor columns are dropped below,
  ## by name, and nothing is interpolated. The door and the coverage warning
  ## ran before the search.

  drawn <- subset_rows(pool, union_ids)

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

  ## The meta columns were computed in union_ids order and are attached by
  ## position, which holds while subset_rows() returns rows in keep order.
  check_rows_aligned(
    what         = paste0("Provenance columns (", paste(names(meta), collapse = ", "), ")"),
    to           = "the drawn rows",
    ids          = drawn$data$analysis$sample_id,
    expected_ids = union_ids
  )

  ## The library's predictor columns the targets do not share leave with the
  ## same rebuild that adds the provenance columns.
  dropped  <- setdiff(pool_vars, cols$shared)
  analysis <- dplyr::bind_cols(drawn$data$analysis[, setdiff(names(drawn$data$analysis), dropped), drop = FALSE],
                               meta)
  role_map <- dplyr::bind_rows(drawn$data$role_map[!drawn$data$role_map$variable %in% dropped, ],
                               tibble::tibble(variable = names(meta), role = "meta"))

  out <- set_analysis(drawn, analysis, role_map)

  ## The library's standardization record, its axis fields describing the
  ## columns returned when they are only some of the library's (#90).
  ## Assigned through single brackets, so a library never standardized
  ## keeps the key as NULL rather than deleting it.
  out$data["standardization"] <- list(returned_standardization(
    pool$data$standardization, as.numeric(sub("^wn_", "", cols$shared)), length(pool_vars)
  ))

  ## Pool sizes per property ------------------------------------------------

  ## drawn counts the rows that survived into the return, not the rows the
  ## neighbourhoods named, so it agrees with n_rows after the twin
  ## subtraction. Under global, which draws every eligible row, it counts
  ## those that carry the property, the set available counts (#153).

  pool_sizes <- tibble::tibble(
    property  = properties,
    available = vapply(properties, function(p) sum(!is.na(resp_tbl[[p]])), integer(1), USE.NAMES = FALSE),
    drawn     = vapply(properties, function(p) {

      if (scope == "global") {
        sum(!is.na(resp_tbl[[p]][pool_ids %in% union_ids]))
      } else {
        length(intersect(unique(membership$pool_id[membership$property == p]), union_ids))
      }

    }, integer(1), USE.NAMES = FALSE)
  )

  ## Resemblance: targets beyond the pool's own nearest-neighbour spread ----

  resemblance <- check_resemblance(sp, St, metric = metric, chunk_size = chunk_size, seed = seed,
                                   eligible = eligible, min_rows = control$min_resemblance_rows)

  ## The record -------------------------------------------------------------

  out$selection <- list(
    settings = list(
      k = k_by, scope = scope, properties = setdiff(properties, emptied),
      snv = snv, derivative = as.integer(derivative), window = window, window_points = window_points,
      poly = as.integer(poly), window_cm = window_cm, mask = mask,
      space = space, ncomp = ncomp, sdev_floor = sdev_floor,
      ncomp_retained = sp$ncomp, ncomp_variance = sp$ncomp_variance,
      sdev_ratio = sp$sdev_ratio, variance_retained = sp$variance_retained,
      depth = depth,
      metric = metric, clusters = clusters,
      cluster_min = as.integer(cluster_min), twin_ratio = twin_ratio, seed = as.integer(seed),
      space_note = space_note,
      control = control
    ),
    reconciliation   = list(operation    = if (length(cols$shared) == length(pool_vars)) "none" else "subset",
                            n_shared     = length(cols$shared),
                            shared_range = range(as.numeric(sub("^wn_", "", cols$shared))),
                            target_only  = as.numeric(sub("^wn_", "", cols$target_only))),
    search           = c(sa$record, list(cache = cache)),
    pool             = list(n_rows = nrow(pool$data$analysis),
                            id_hash = digest::digest(sort(pool$data$analysis$sample_id, method = "radix"))),
    targets          = list(source  = targets_source(x$log),
                            n_rows  = nrow(x$data$analysis),
                            id_hash = digest::digest(sort(x$data$analysis$sample_id, method = "radix"))),
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

  ## Draws short of k: too few measured rows left after the twin
  ## subtraction, or, under cosine, no defined distance at all. One coverage
  ## warning, beside any of the axes', naming the targets.

  sd_tbl <- draw$short_draws

  if (nrow(sd_tbl)) {

    undefined <- startsWith(sd_tbl$reason, "no defined distance")
    thin      <- sd_tbl[!undefined, , drop = FALSE]
    no_angle  <- unique(sd_tbl$target_id[undefined])

    log_warning(collector, "coverage", c(
      "{nrow(sd_tbl)} target-property draw{?s} could not reach k.",
      "i" = if (nrow(thin)) "{nrow(thin)} after twin exclusions, with too few measured rows left. Smallest: {min(thin$k_drawn)} of {max(thin$k_requested)} rows, on {.field {unique(thin$property)}}.",
      "i" = if (length(no_angle)) "{length(no_angle)} target{?s} had no defined distance to any pool row, so nothing was drawn for {?it/them}: {shown_ids(no_angle)}. {.code metric = \"cosine\"} has no angle for a row on the pool's centroid, since the scores are mean-centred; use {.val euclidean} or {.val mahalanobis} for these targets.",
      "i" = "See {.code x$selection$short_draws}."
    ), ids = unique(sd_tbl$target_id))

  }

  if (!is.null(resemblance$skipped)) {

    log_warning(collector, "skipped", c(
      "The resemblance check did not run: {resemblance$skipped}.",
      "i" = "Nothing says whether the targets resemble the rows they were drawn from; {.code depth = \"all\"} gives it more rows to measure against, and {.code control_horizons(min_resemblance_rows = )} sets how many it needs."
    ))

  }

  beyond <- resemblance$beyond

  if (nrow(beyond)) {

    log_warning(collector, "applicability", c(
      "{nrow(beyond)} target{?s} sit{?s/} beyond the pool's own nearest-neighbour spread.",
      "i" = "{shown_ids(beyond$target_id)}.",
      "i" = "Selection proceeds; treat their predictions with care. The check is made after SNV, so it cannot see a unit mismatch; that is reported separately."
    ), ids = beyond$target_id)

  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Record, re-validate, then raise
  ## ---------------------------------------------------------------------------

  ## The pool's log, carried by subset_rows(), gains this call's rows before
  ## the object is validated, and each warning is raised only once its row
  ## is on the object.
  out$log <- log_close(collector, out$log)

  out <- validate_horizons_data(out)

  signal_log(collector, out)

  out

}


## =============================================================================
## Internal helpers
## =============================================================================


## ---------------------------------------------------------------------------
## targets_source() — where the targets were read from, from their log
## ---------------------------------------------------------------------------

#' The source the targets were read from
#'
#' @description
#' The `ids` of the targets' last [spectra()] note, which are the paths it
#' was given. The training set carries the pool's log, not the targets', so the
#' selection record keeps the targets' source itself, beside their size and
#' id hash. This is the one place a verb reads a log row's content, so
#' `spectra()`'s note promises its `ids`.
#'
#' @param log [Tibble.] The targets' log.
#'
#' @return [Character or NULL.] The paths, `"data frame"` when the note names
#'   none, or `NULL` when the log holds no `spectra()` note.
#' @noRd
targets_source <- function(log) {

  read <- log[log$verb %in% "spectra" & log$type %in% "note", , drop = FALSE]

  if (nrow(read) == 0) return(NULL)

  ### The last: the record is the call's last row, and the latest read
  ### describes the targets
  paths <- read$ids[[nrow(read)]]

  if (length(paths) == 0) "data frame" else paths

}


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


#' Does a Savitzky-Golay window fit on a grid of so many columns?
#'
#' @description
#' prospectr's filter needs fewer points than columns. The one rule for the
#' verb's window door and for the first-use space of a registered library
#' ([prime_library_space()]).
#'
#' @param points [Integer.] The window, in points.
#' @param n_columns [Integer.] Columns of the grid it runs on.
#' @return [Logical.]
#' @noRd
sg_window_fits <- function(points, n_columns) {

  points < n_columns

}


## ---------------------------------------------------------------------------
## abort_select_inputs() — every collected argument problem, in one error
## ---------------------------------------------------------------------------

#' Refuse the call with every argument problem found
#'
#' @description
#' `select_training()` checks its arguments, and then the pool's data, all
#' at once before any work, so a call is refused once with everything wrong
#' with it rather than once per mistake, in the shape [abort_findings()]
#' gives a validation error too. Each finding names its own remedy.
#'
#' @param findings [Character.] One message per problem.
#' @param call The frame the error reports: the verb's.
#'
#' @return Does not return; aborts with class `horizons_error_input`.
#' @noRd
abort_select_inputs <- function(findings, call = rlang::caller_env()) {

  abort_findings(findings, "The inputs failed {length(findings)} check{?s}.",
                 kind = "input", call = call)

}


## ---------------------------------------------------------------------------
## validate_select_input() — The base contract, naming the argument
## ---------------------------------------------------------------------------

#' Certify one of select_training()'s inputs against the base contract
#'
#' @description
#' Runs `validate_horizons_data()` at the full stage and, when it fails,
#' re-raises its error naming the argument: the verb takes two objects, and
#' the validator's report does not say which one it was handed.
#'
#' @param obj [horizons_data.] The targets or the pool.
#' @param arg [Character.] The argument name, `"x"` or `"library"`.
#' @param what [Character.] What the argument is, for the message.
#' @param call The frame the error reports: the verb's.
#'
#' @return [horizons_data.] `obj`, unchanged. Aborts with class
#'   `horizons_error_validation`, the validator's error as its parent and its
#'   `findings` carried.
#' @noRd
validate_select_input <- function(obj, arg, what, call = rlang::caller_env()) {

  rlang::try_fetch(
    validate_horizons_data(obj),
    horizons_error_validation = function(e) {

      abort_horizons(c(
        "{.arg {arg}}, {what}, failed validation.",
        "i" = "Average replicate scans with {.fn average}; rebuild an object edited by hand or saved by an older version of horizons with the verbs that wrote it."
      ), kind = "validation", parent = e, findings = e$findings, call = call)

    }
  )

}


## ---------------------------------------------------------------------------
## check_pool_unpromoted() — The pool must be data, not a modelling object
## ---------------------------------------------------------------------------

#' Refuse a pool that carries state from later in the pipeline
#'
#' @description
#' `select_training()` builds its return by subsetting the pool, and a subset
#' keeps the pool's class and every section the pool was carrying. A pool
#' that had been configured, validated, evaluated or fitted would hand that
#' state to the training set: a configuration and an outcome chosen for the
#' library, a verdict and a removal record about rows that are mostly not
#' there, or `models$split` and `evaluation$split` keyed to a row order the
#' subset just destroyed. So the pool has to be data as `standardize()`
#' and `add_response()` leave it, and this refuses rather than silently
#' clearing.
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

  outcome  <- pool$data$role_map$variable[pool$data$role_map$role == "outcome"]
  outliers <- pool$validation$outliers
  removal  <- isTRUE(outliers$removed) || length(outliers$removed_ids) > 0L ||
              NROW(outliers$removal_detail) > 0L

  carried <- c(
    if (!is.null(pool$config$configs))       "config$configs",
    if (length(outcome))                     paste0("an outcome role (", paste(outcome, collapse = ", "), ")"),
    if (!is.null(pool$validation$passed))    "a verdict in validation$passed",
    if (removal)                             "a removal record in validation$outliers",
    if (!is.null(pool$evaluation$results))   "evaluation$results",
    if (!is.null(pool$models$workflows))     "models$workflows"
  )

  if (length(carried)) {

    msgs <- c(msgs, cli::format_inline(
      "{.arg library} already carries {carried} from later in the pipeline, which the training set drawn from it would inherit; use the library as it was before {.fn configure} and {.fn validate}"))

  }

  msgs

}


## ---------------------------------------------------------------------------
## check_same_standardization() — Pool and targets corrected alike (#107)
## ---------------------------------------------------------------------------

#' Refuse a library and targets standardized with different baselines
#'
#' @description
#' The training set is the library's spectra as `standardize()` left them,
#' and the model predicts the targets as theirs were left. A convex-hull
#' baseline on one and not the other, or hulls fitted over different ranges,
#' make the model train on one preprocessing and predict on another, which
#' is a wrong result, not a weak one, so it is refused before any work.
#'
#' @details
#' `standardize()` records the range each hull was fitted over as
#' `baseline_range`, kept when a later `standardize(force = TRUE)` trims the
#' axis further, so a hull's ends are its record's `baseline_range`
#' ([prior_hull_range()]).
#' Those ranges are compared, not the `trim` arguments: data that stops
#' short of a bound fits
#' a different hull under an equal trim, and `trim = NULL` on data spanning
#' the bounds fits the same hull as the bounds. Two ranges agree when each
#' end differs by less than the coarser of the two grids' steps, as close as
#' two grids can land (the search's own reasoning for full coverage), so a
#' 2 cm-1 library over 600 to 4000 and 3 cm-1 targets over 600 to 3999 pass.
#'
#' A side with no record was never standardized, so horizons fitted no hull
#' on it, and it counts as not corrected. Hull correction applied twice gives
#' the same spectra, so the remedy also works on spectra corrected outside
#' horizons. The steps, `resample` and `remove_water` are not compared: a
#' coarser grid shares the library's columns (#236), and a removed band is
#' refused as a gap.
#'
#' @param pool_record,target_record [List or NULL.] Each side's
#'   `data$standardization`.
#' @param pool_step,target_step [Numeric.] Each axis's median spacing.
#' @param library_form [Character.] The library's `form` (`"object"`,
#'   `"path"`, `"registered"`): a pool in memory or a library file is the
#'   user's to standardize again, so it is offered the library-side remedy;
#'   a registered library is not.
#' @param restandardize [Logical.] Whether the targets were standardized
#'   already, so a remedy that adds a baseline needs `force = TRUE`.
#' @param call The frame the refusal reports: the verb's.
#'
#' @return `NULL`, invisibly. Aborts with class `horizons_error_input`.
#' @noRd
check_same_standardization <- function(pool_record, target_record, pool_step, target_step,
                                       library_form, restandardize = FALSE,
                                       call = rlang::caller_env()) {

  pool_hull   <- isTRUE(pool_record$baseline)
  target_hull <- isTRUE(target_record$baseline)

  if (!pool_hull && !target_hull) return(invisible(NULL))

  ### The range each hull was fitted over (prior_hull_range()); a side
  ### without one is read by its axis, which is where a hull would be fitted
  p_range <- if (pool_hull) prior_hull_range(pool_record) else pool_record$wavelength_range
  t_range <- if (target_hull) prior_hull_range(target_record) else target_record$wavelength_range
  t_step  <- signif(target_step, 4)
  l_step  <- signif(pool_step, 4)
  force   <- if (restandardize) ", force = TRUE" else ""
  lib_force <- if (is.null(pool_record)) "" else ", force = TRUE"
  own     <- library_form %in% c("object", "path")

  if (pool_hull && target_hull) {

    tol <- max(pool_step, target_step)

    if (all(abs(p_range - t_range) < tol)) return(invisible(NULL))

    ### The range both reach: a side whose data stops short of the other's
    ### hull end cannot fit a hull there, so both are fitted over the
    ### overlap, each side named only when its hull has to move.
    both    <- c(max(p_range[1], t_range[1]), min(p_range[2], t_range[2]))
    t_moves <- any(abs(t_range - both) >= tol)
    p_moves <- any(abs(p_range - both) >= tol)

    problem <- "Both were baseline-corrected, the library's hull over {p_range[1]} to {p_range[2]} cm-1 and the targets' over {t_range[1]} to {t_range[2]} cm-1, so the hulls are pinned at different ends."
    remedy  <- if (own) {
      c(
        "i" = "Fit both hulls over the range both reach, {both[1]} to {both[2]} cm-1, standardizing from {.fn spectra}:",
        "*" = if (t_moves) "the targets with {.code standardize(x, resample = {t_step}, trim = c({both[1]}, {both[2]}), baseline = TRUE)}",
        "*" = if (p_moves) "the library with {.code standardize(library, resample = {l_step}, trim = c({both[1]}, {both[2]}), baseline = TRUE)}"
      )
    } else {
      c("i" = "Standardize the targets from {.fn spectra} over the library's range: {.code standardize(x, resample = {t_step}, trim = c({p_range[1]}, {p_range[2]}), baseline = TRUE)}.")
    }

  } else if (pool_hull) {

    problem <- "The library was baseline-corrected ({.code standardize(baseline = TRUE)}) and the targets were not."
    remedy  <- c(
      "i" = "Correct the targets over the library's range: {.code standardize(x, resample = {t_step}, trim = c({p_range[1]}, {p_range[2]}), baseline = TRUE{force})}.",
      "i" = if (own) "Or standardize the library from {.fn spectra} without {.arg baseline}, {.code standardize(library, resample = {l_step}, trim = c({p_range[1]}, {p_range[2]}))}; a hull already subtracted cannot be put back."
    )

  } else {

    problem <- "The targets were baseline-corrected ({.code standardize(baseline = TRUE)}) and the library was not."
    remedy  <- c(
      "i" = "Standardize the targets from {.fn spectra} without {.arg baseline}, {.code standardize(x, resample = {t_step}, trim = c({t_range[1]}, {t_range[2]}))}; a hull already subtracted cannot be put back.",
      "i" = if (own) "Or correct the library over the targets' range: {.code standardize(library, resample = {l_step}, trim = c({t_range[1]}, {t_range[2]}), baseline = TRUE{lib_force})}."
    )

  }

  abort_horizons(c(
    "{.arg x} and {.arg library} were standardized differently.",
    "x" = problem,
    "i" = "A model trained on one baseline cannot predict the other.",
    remedy
  ), kind = "input", call = call)

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
#' @param pool_m [Matrix.] Pool spectra on the search grid, raw.
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
#' @param min_rows [Integer.] The fewest rows the reference distribution is
#'   taken from; below it the check is skipped and says why. Default:
#'   `SELECT_RESEMBLANCE_MIN_ROWS`.
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
check_resemblance <- function(sp, St, metric, chunk_size, seed = 1L, eligible = NULL,
                              min_rows = SELECT_RESEMBLANCE_MIN_ROWS) {

  Sp  <- if (is.null(eligible)) sp$scores else sp$scores[eligible, , drop = FALSE]
  n   <- nrow(Sp)

  ## The reference is each row's distance to its nearest other row, so it
  ## needs at least two, and a handful makes a 99th percentile that is only
  ## the largest of a few gaps. Below the floor the check is recorded as not
  ## run rather than naming every target, or failing after the whole draw.

  if (n < min_rows) {

    return(list(
      threshold   = NA_real_,
      n_reference = n,
      beyond      = tibble::tibble(target_id = character(), nearest = numeric()),
      skipped     = paste0("only ", n, " row", if (n == 1L) "" else "s",
                           " the draw can reach; the check needs ", min_rows)
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
  ## Column 1 is the row itself (distance 0); column 2 its nearest other row.
  ## A row with no defined distance (cosine, on the pool's centroid) has no
  ## nearest other row, and is left out of the reference.
  pool_nn <- nn_pool$dist[, 2]

  threshold <- unname(stats::quantile(pool_nn, 0.99, na.rm = TRUE))

  nn_t <- nearest_neighbours(St, Sp, k = 1L, metric = metric, sdev = sp$sdev, chunk_size = chunk_size)
  nearest <- nn_t$dist[, 1]

  ## A target with no defined distance (cosine, on the pool's centroid) is
  ## not beyond anything; the draw reports it as a short draw
  beyond <- which(nearest > threshold)

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
#' Its one caller passes the record it has just built, so every table it
#' reads is there.
#'
#' @param sel [List.] `x$selection`.
#' @param n_targets [Integer.] Targets in the batch.
#'
#' @return NULL, called for its output.
#' @noRd
report_selection <- function(sel, n_targets) {

  s     <- sel$settings
  ps    <- sel$pool_sizes
  scope <- s$scope

  for (i in seq_len(nrow(ps))) {

    msg <- if (scope == "global") {
      paste0(ps$property[i], ": ", ps$available[i], " measured rows, no draw (scope = global)")
    } else {
      paste0(ps$property[i], ": k = ", s$k[[ps$property[i]]], " \u00D7 ", n_targets, " targets \u2192 ",
             ps$drawn[i], " of ", ps$available[i], " measured rows")
    }
    cat(paste0("\u2502  \u251C\u2500 ", msg, "\n"))

  }

  if (scope == "cluster") {

    cat(paste0("\u2502  \u251C\u2500 Clusters: ", sel$clustering$k, " (", sel$clustering$reason, ")\n"))

  }

  ## Two counts, because they answer different questions: how many
  ## neighbourhood slots a twin was removed from, and how many rows that
  ## actually kept out of the training set.

  n_tw <- nrow(sel$exclusions)
  tail <- if (n_tw) {
    paste0(" (", paste(utils::head(unique(sel$exclusions$target_id), 3), collapse = ", "),
           if (length(unique(sel$exclusions$target_id)) > 3) ", ..." else "", ")")
  } else {
    ""
  }

  cat(paste0("\u2502  \u251C\u2500 Twins flagged: ", n_tw, tail, "\n"))

  n_un <- sel$n_excluded_union

  union_note <- if (scope == "global") {
    "0 (scope = global keeps the whole pool; the check is reported, not applied)"
  } else {
    paste0(n_un, " removed from the union")
  }

  cat(paste0("\u2502  \u251C\u2500 Twin rows: ", union_note, "\n"))

  n_short <- nrow(sel$short_draws)

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

  far <- if (!is.null(sel$resemblance$skipped)) {
    paste0("not checked (", sel$resemblance$skipped, ")")
  } else {
    nrow(sel$resemblance$beyond)
  }
  cat(paste0("\u2502  \u251C\u2500 Targets beyond the pool's spread: ", far, "\n"))

  n_groups <- nrow(sel$groups)
  n_rows   <- length(unique(unlist(sel$groups$pool_ids)))
  cat(paste0("\u2502  \u2514\u2500 ", n_rows, " rows in ", n_groups, " group", if (n_groups != 1) "s",
             " (scope = ", scope, ")\n"))
  cat("\u2502\n")

  invisible(NULL)

}
