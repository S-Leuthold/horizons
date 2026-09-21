# R/pipeline-select-training.R
# Draw a training set from a reference pool around a batch of targets. The
# only place in the pipeline where library and unknowns meet before anything
# is fitted, so the reconciliation of their axes, the self-leakage check and
# the selection-time distance live here.


## =============================================================================
## select_training() — User-Facing Function
## =============================================================================

#' Pipeline: Select a Training Set From a Pool
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
#'   argument away.
#'
#' For `"cluster"` and `"sample"` the return is still the single union, and
#' fitting one model per group is the caller's loop over
#' `x$selection$groups` for now.
#'
#' **The similarity space.** Distances are measured in a space built for
#' "similar soil", not "similar baseline", and it never reaches the model:
#' a user who configures `preprocessing = "raw"` still gets neighbours
#' chosen on derivative spectra. The default chain is SNV, a Savitzky-Golay
#' first derivative (window 11, order 2), PCA of the pool to 99 % of
#' variance, and Mahalanobis distance on the scores. Every step is an
#' argument. A PCA space and its Mahalanobis scaling are defined by the
#' population they were fit on; `space_rows` chooses whether that is the
#' whole pool or, per property, only the rows that have it measured.
#'
#' **Reconciliation.** The targets' wavenumbers are the grid. The pool is
#' resampled onto them through the same routine `standardize()` uses, so
#' the training set and the targets share one axis end to end. A pool that
#' does not cover the targets' range stops the verb; targets finer than the
#' pool warn.
#'
#' **Self-leakage.** A pool row whose spectrum is a target's twin (nearest
#' distance zero, or below `twin_ratio` times the second-nearest) is
#' excluded from that target's neighbourhood and reported.
#'
#' **What the record holds** (`x$selection`): the settings as resolved, the
#' reconciliation, the pool's identity, the membership table (target by
#' property by pool row, with distance and rank), the groups, pool sizes
#' per property against those available, every target's nearest and
#' mean-of-k distance (the applicability signal at selection time), the
#' twins excluded, and the clustering when `scope = "cluster"`.
#'
#' @param x `horizons_data.` The targets: the samples to be predicted.
#' @param pool `horizons_data.` The reference pool, with one or more
#'   response columns.
#' @param k `integer.` Neighbours per target per property; a scalar, or a
#'   named vector with one entry per property. Default: `400`.
#' @param scope `character.` `"batch"`, `"cluster"`, `"sample"` or
#'   `"global"`. Default: `"batch"`.
#' @param properties `character or NULL.` Response columns of the pool to
#'   draw for. `NULL` means all of them. Default: `NULL`.
#' @param snv `logical.` SNV in the similarity space. Default: `TRUE`.
#' @param derivative `integer.` Savitzky-Golay derivative order in the
#'   similarity space; `0` disables the filter. Default: `1`.
#' @param window,poly `integer.` Savitzky-Golay window and polynomial
#'   order. Default: `11`, `2`.
#' @param mask `matrix or NULL.` Wavenumber ranges to drop from the
#'   similarity space after the derivative, one row per range, low then
#'   high. Default: `NULL`.
#' @param space `character.` `"pca"` (unsupervised, of the pool) or
#'   `"pls"` (against one property; `properties` must name exactly one).
#'   Default: `"pca"`.
#' @param ncomp `numeric.` Components: a proportion of variance in (0, 1)
#'   (PCA only, capped at 100) or an integer count. Default: `0.99`.
#' @param metric `character.` `"mahalanobis"`, `"euclidean"` or
#'   `"cosine"`, on the scores. Default: `"mahalanobis"`.
#' @param space_rows `character.` Which pool rows define the space the
#'   draw for a property happens in. `"all"`: one space fit on every pool
#'   row, the draw for each property restricted to the rows that have it.
#'   `"measured"`: a space fit per property on only the rows that have it,
#'   so the axes and the Mahalanobis scaling come from that population. The
#'   all-rows space is built in either case and is what the target
#'   clustering and the resemblance check use. `space = "pls"` already fits
#'   on measured rows. Default: `"all"`.
#' @param clusters `integer or NULL.` `scope = "cluster"` only: the cluster
#'   count, or `NULL` to choose by silhouette. Default: `NULL`.
#' @param cluster_min `integer.` `scope = "cluster"` only: the floor on
#'   cluster size. Default: `30`.
#' @param twin_ratio `numeric.` The nearest-to-second-nearest ratio below
#'   which a pool row is a target's twin. Default: an internal constant to
#'   be calibrated on replicate scans.
#' @param chunk_size `integer.` Targets per distance chunk. Default: `500`.
#' @param seed `integer.` Seed for the clustering. Default: `1`.
#' @param verbose `logical.` Print the report. Default: `TRUE`.
#'
#' @return `horizons_data.` Rows drawn from `pool`, on the targets'
#'   wavenumber grid, with every response column the pool carried, three
#'   provenance columns with role `meta` (`.drawn_by`, `.min_distance`,
#'   and `.group` except under `scope = "sample"`), and the record in
#'   `x$selection`.
#'
#' @examples
#' \dontrun{
#' training <- targets |>
#'   select_training(pool, k = 400, properties = "clay")
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
select_training <- function(x, pool,
                            k           = 400L,
                            scope       = c("batch", "cluster", "sample", "global"),
                            properties  = NULL,
                            snv         = TRUE,
                            derivative  = 1L,
                            window      = 11L,
                            poly        = 2L,
                            mask        = NULL,
                            space       = c("pca", "pls"),
                            ncomp       = 0.99,
                            metric      = c("mahalanobis", "euclidean", "cosine"),
                            space_rows  = c("all", "measured"),
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

  if (!inherits(pool, "horizons_data")) {
    errors <- c(errors, cli::format_inline("{.arg pool} must be a horizons_data object"))
  }

  scope_ok  <- is.character(scope)  && all(scope  %in% c("batch", "cluster", "sample", "global"))
  space_ok  <- is.character(space)  && all(space  %in% c("pca", "pls"))
  metric_ok <- is.character(metric) && all(metric %in% c("mahalanobis", "euclidean", "cosine"))
  rows_ok   <- is.character(space_rows) && all(space_rows %in% c("all", "measured"))

  if (!scope_ok)  errors <- c(errors, cli::format_inline("{.arg scope} must be one of batch, cluster, sample, global"))
  if (!space_ok)  errors <- c(errors, cli::format_inline("{.arg space} must be pca or pls"))
  if (!metric_ok) errors <- c(errors, cli::format_inline("{.arg metric} must be mahalanobis, euclidean or cosine"))
  if (!rows_ok)   errors <- c(errors, cli::format_inline("{.arg space_rows} must be all or measured"))

  scope      <- if (scope_ok)  scope[1]      else NA_character_
  space      <- if (space_ok)  space[1]      else NA_character_
  metric     <- if (metric_ok) metric[1]     else NA_character_
  space_rows <- if (rows_ok)   space_rows[1] else NA_character_

  k_ok <- is.numeric(k) && length(k) >= 1L && all(is.finite(k)) && all(k >= 1) && all(k == round(k))

  if (!k_ok) {
    errors <- c(errors, cli::format_inline("{.arg k} must be a positive integer, or a named vector of them"))
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    errors <- c(errors, cli::format_inline("{.arg verbose} must be TRUE or FALSE"))
  }

  ## Properties: the pool's responses ---------------------------------------

  if (inherits(pool, "horizons_data")) {

    responses <- pool$data$role_map$variable[pool$data$role_map$role == "response"]

    if (!length(responses)) {

      errors <- c(errors, cli::format_inline("{.arg pool} has no response columns; add the library's lab values first"))

    } else if (is.null(properties)) {

      properties <- responses

    } else {

      unknown <- setdiff(properties, responses)

      if (length(unknown)) {
        errors <- c(errors, cli::format_inline("{.arg properties} not in the pool's responses: {.val {unknown}} (available: {.val {responses}})"))
      }

    }

    if (k_ok && length(k) > 1L || (k_ok && !is.null(names(k)))) {

      missing_k <- setdiff(properties, names(k))

      if (length(missing_k)) {
        errors <- c(errors, cli::format_inline("A named {.arg k} needs an entry for every property; missing {.val {missing_k}}"))
      }

    }

    if (identical(space, "pls") && length(properties) != 1L) {
      errors <- c(errors, cli::format_inline("{.arg space = \"pls\"} selects against one property; name exactly one in {.arg properties}"))
    }

  }

  if (length(errors) > 0) {

    cat(cli::col_red(cli::style_bold("! Input validation failed:\n")))

    for (i in seq_along(errors)) {
      branch <- if (i < length(errors)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_red(paste0("   ", branch, " ", errors[i], "\n")))
    }

    cat("\n")
    rlang::abort(paste(c("Input validation failed:", errors), collapse = "\n"),
                 class = "horizons_input_error")

  }

  k_by <- if (is.null(names(k))) {
    stats::setNames(rep(as.integer(k), length(properties)), properties)
  } else {
    stats::setNames(as.integer(k[properties]), properties)
  }

  if (verbose) cat(paste0("\u251C\u2500 ", cli::style_bold("Selecting training set"), "...\n"))

  ## ---------------------------------------------------------------------------
  ## Step 1: Reconcile the axes
  ## ---------------------------------------------------------------------------

  rc <- reconcile_axes(pool, x)
  tm <- predictor_matrix(x)

  pool_rc <- rebuild_predictors(pool, rc$matrix, rc$wavenumbers)

  if (verbose) {

    op <- if (rc$record$operation == "none") "already on the targets' grid" else
      paste0("resampled ", rc$record$pool_grid$resolution, " \u2192 ",
             rc$record$target_grid$resolution, " cm\u207B\u00B9")
    cat(paste0("\u2502  \u251C\u2500 Pool: ", nrow(rc$matrix), " rows, ", op, "\n"))

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Build the similarity space on the pool, project the targets
  ## ---------------------------------------------------------------------------

  ## The all-rows space is always built: it is the draw's space under
  ## space_rows = "all", and the space clustering and the resemblance check
  ## use in either mode, so that neither depends on which property is
  ## being drawn. Under space_rows = "measured" a further space is fit per
  ## property on the rows that have it, and the draw for that property
  ## happens there.

  pool_ids <- pool_rc$data$analysis$sample_id
  resp_tbl <- pool_rc$data$analysis[, c("sample_id", properties), drop = FALSE]

  build_space_on <- function(rows) {
    y <- if (space == "pls") pool_rc$data$analysis[[properties]][rows] else NULL
    build_similarity_space(rc$matrix[rows, , drop = FALSE], rc$wavenumbers,
                           snv = snv, derivative = derivative, window = window,
                           poly = poly, mask = mask, space = space, ncomp = ncomp, y = y)
  }

  sp <- build_space_on(seq_along(pool_ids))
  St <- project_similarity(sp, tm$matrix, tm$wavenumbers)

  spaces_by_property <- NULL

  if (space_rows == "measured" && scope != "global") {

    spaces_by_property <- lapply(properties, function(p) {
      rows <- which(!is.na(resp_tbl[[p]]))
      s    <- build_space_on(rows)
      list(space = s, St = project_similarity(s, tm$matrix, tm$wavenumbers), n_rows = length(rows))
    })
    names(spaces_by_property) <- properties

  }

  if (verbose) {

    chain <- c(if (snv) "SNV", if (derivative > 0) paste0("SG d", derivative, " w", window, " p", poly),
               if (!is.null(mask)) paste0(nrow(mask), " masked range", if (nrow(mask) > 1) "s"),
               toupper(space))
    cat(paste0("\u2502  \u251C\u2500 Space: ", paste(chain, collapse = " \u2192 "), ", ",
               sp$ncomp, " components on all ", length(pool_ids), " rows, ", metric, "\n"))

    if (!is.null(spaces_by_property)) {
      for (p in properties) {
        cat(paste0("\u2502  \u251C\u2500 Space for ", p, ": ", spaces_by_property[[p]]$space$ncomp,
                   " components on its ", spaces_by_property[[p]]$n_rows, " measured rows\n"))
      }
    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Draw
  ## ---------------------------------------------------------------------------

  if (scope == "global") {

    nn <- nearest_neighbours(St, sp$scores, k = min(2L, nrow(sp$scores)), metric = metric,
                             sdev = sp$sdev, chunk_size = chunk_size)

    draw <- list(
      membership       = tibble::tibble(target_id = character(), property = character(),
                                        pool_id = character(), distance = numeric(), rank = integer()),
      target_distances = tibble::tibble(target_id = rownames(St), property = NA_character_,
                                        nearest = unname(nn$dist[, 1]), mean_k = unname(nn$dist[, 1])),
      exclusions       = tibble::tibble(property = character(), target_id = character(),
                                        pool_id = character(), distance = numeric(), second_distance = numeric()),
      k                = k_by
    )

  } else if (is.null(spaces_by_property)) {

    draw <- draw_neighbours(St, sp$scores, responses = resp_tbl, k = k_by,
                            properties = properties, metric = metric, sdev = sp$sdev,
                            chunk_size = chunk_size, twin_ratio = twin_ratio)

  } else {

    ## One draw per property, each in its own space, bound together
    per_property <- lapply(properties, function(p) {
      s <- spaces_by_property[[p]]
      draw_neighbours(s$St, s$space$scores, responses = resp_tbl, k = k_by[p],
                      properties = p, metric = metric, sdev = s$space$sdev,
                      chunk_size = chunk_size, twin_ratio = twin_ratio)
    })

    draw <- list(
      membership       = dplyr::bind_rows(lapply(per_property, `[[`, "membership")),
      target_distances = dplyr::bind_rows(lapply(per_property, `[[`, "target_distances")),
      exclusions       = dplyr::bind_rows(lapply(per_property, `[[`, "exclusions")),
      k                = k_by
    )

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
      if (clustering$k == 1L) {
        cli::cli_inform("scope = \"cluster\": {clustering$reason}; returning one group")
      }
      clustering$assignment
    }
  )

  groups <- build_groups(group_of_target, membership, pool_ids, scope)

  ## ---------------------------------------------------------------------------
  ## Step 5: Assemble the return
  ## ---------------------------------------------------------------------------

  union_ids <- if (scope == "global") pool_ids else pool_ids[pool_ids %in% membership$pool_id]

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

    meta$.group <- if (scope == "global") {
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

  pool_sizes <- tibble::tibble(
    property  = properties,
    available = vapply(properties, function(p) sum(!is.na(resp_tbl[[p]])), integer(1), USE.NAMES = FALSE),
    drawn     = vapply(properties, function(p) {
      if (scope == "global") length(union_ids) else length(unique(membership$pool_id[membership$property == p]))
    }, integer(1), USE.NAMES = FALSE)
  )

  ## Resemblance: targets beyond the pool's own nearest-neighbour spread ----

  resemblance <- check_resemblance(sp, St, metric = metric, chunk_size = chunk_size)

  ## The record -------------------------------------------------------------

  out$selection <- list(
    settings = list(
      k = k_by, scope = scope, properties = properties,
      snv = snv, derivative = as.integer(derivative), window = as.integer(window),
      poly = as.integer(poly), mask = mask, space = space, ncomp = ncomp,
      ncomp_retained = sp$ncomp, space_rows = space_rows,
      ncomp_by_property = if (is.null(spaces_by_property)) NULL else
        vapply(spaces_by_property, function(s) s$space$ncomp, integer(1)),
      space_n_rows = if (is.null(spaces_by_property)) NULL else
        vapply(spaces_by_property, function(s) s$n_rows, integer(1)),
      metric = metric, clusters = clusters,
      cluster_min = as.integer(cluster_min), twin_ratio = twin_ratio, seed = as.integer(seed)
    ),
    reconciliation   = rc$record,
    pool             = list(n_rows = length(pool_ids), id_hash = digest::digest(sort(pool_ids))),
    membership       = membership,
    groups           = groups,
    pool_sizes       = pool_sizes,
    target_distances = draw$target_distances,
    resemblance      = resemblance,
    exclusions       = draw$exclusions,
    clustering       = clustering,
    timestamp        = Sys.time()
  )

  ## ---------------------------------------------------------------------------
  ## Step 6: Report
  ## ---------------------------------------------------------------------------

  if (verbose) report_selection(out$selection, n_targets = length(target_ids))

  if (nrow(resemblance$beyond)) {

    cli::cli_warn(c(
      "{nrow(resemblance$beyond)} target{?s} sit{?s/} beyond the pool's own nearest-neighbour spread",
      "i" = "{.val {utils::head(resemblance$beyond$target_id, 5)}}{if (nrow(resemblance$beyond) > 5) ', ...' else ''}",
      "i" = "Selection proceeds; treat their predictions with care and check units and mode"
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
#' @param sp [horizons_similarity_space.] The space, with pool scores.
#' @param St [Matrix.] Target scores.
#' @param metric,chunk_size Passed to `nearest_neighbours()`.
#'
#' @return [List.] `threshold` (the 99th percentile), `n_reference`,
#'   `beyond` (tibble: `target_id`, `nearest`).
#' @noRd
check_resemblance <- function(sp, St, metric, chunk_size) {

  Sp  <- sp$scores
  n   <- nrow(Sp)
  ref <- if (n > 2000L) Sp[sort(sample.int(n, 2000L)), , drop = FALSE] else Sp

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
    beyond      = tibble::tibble(target_id = rownames(St)[beyond], nearest = unname(nearest[beyond]))
  )

}


## ---------------------------------------------------------------------------
## report_selection() — The verbose tree
## ---------------------------------------------------------------------------

#' Print the selection report
#'
#' @param sel [List.] `x$selection`.
#' @param n_targets [Integer.] Targets in the batch.
#'
#' @return NULL, called for its output.
#' @noRd
report_selection <- function(sel, n_targets) {

  s  <- sel$settings
  ps <- sel$pool_sizes

  for (i in seq_len(nrow(ps))) {

    k_p <- s$k[[ps$property[i]]]
    msg <- if (s$scope == "global") {
      paste0(ps$property[i], ": ", ps$available, " measured rows, no draw (scope = global)")
    } else {
      paste0(ps$property[i], ": k = ", k_p, " \u00D7 ", n_targets, " targets \u2192 ",
             ps$drawn[i], " of ", ps$available[i], " measured rows")
    }
    cat(paste0("\u2502  \u251C\u2500 ", msg, "\n"))

  }

  if (s$scope == "cluster" && !is.null(sel$clustering)) {

    cat(paste0("\u2502  \u251C\u2500 Clusters: ", sel$clustering$k, " (", sel$clustering$reason, ")\n"))

  }

  n_tw <- nrow(sel$exclusions)
  cat(paste0("\u2502  \u251C\u2500 Twins excluded: ", n_tw,
             if (n_tw) paste0(" (", paste(utils::head(unique(sel$exclusions$target_id), 3), collapse = ", "),
                             if (n_tw > 3) ", ..." else "", ")") else "", "\n"))

  n_far <- nrow(sel$resemblance$beyond)
  cat(paste0("\u2502  \u251C\u2500 Targets beyond the pool's spread: ", n_far, "\n"))

  n_groups <- nrow(sel$groups)
  n_rows   <- length(unique(unlist(sel$groups$pool_ids)))
  cat(paste0("\u2502  \u2514\u2500 ", n_rows, " rows in ", n_groups, " group", if (n_groups != 1) "s",
             " (scope = ", s$scope, ")\n"))
  cat("\u2502\n")

  invisible(NULL)

}
