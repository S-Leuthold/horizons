# R/select-neighbours.R
# The rule of select_training(): for each target and each property, the k
# nearest pool rows that have the property measured, in the similarity
# space. Plus the self-leakage check, the per-target distance record, and
# the clustering of targets for scope = "cluster".


## ---------------------------------------------------------------------------
## nearest_neighbours() — k nearest pool rows per target, chunked
## ---------------------------------------------------------------------------

#' Find each target's k nearest pool rows in score space
#'
#' @description
#' Computes target-to-pool distances in chunks of targets and keeps the k
#' smallest per target with their pool ids. The full target-by-pool matrix
#' is never held when the targets exceed one chunk.
#'
#' @details
#' Metrics on the scores: `"euclidean"`; `"mahalanobis"`, which is
#' Euclidean after dividing each component by its standard deviation
#' (`sdev`, from the similarity space); `"cosine"`, one minus the cosine of
#' the angle between score vectors. The scores are mean-centred, so the
#' cosine is the angle about the pool's centroid, not the spectral angle
#' about the origin a chemometrician would expect. A row on the centroid has
#' no defined angle and gets an `NA` distance. A *target* on the centroid has
#' no defined distance to anything, and `order()` on an all-`NA` row falls
#' back on the pool's own row order, so the ids returned for it are pool rows
#' 1 to k in matrix order. The `NA` distances are what says so;
#' `draw_neighbours()` treats such a target as a failed draw.
#'
#' Euclidean and Mahalanobis screen the pool with the identity
#' `|a - b|^2 = |a|^2 + |b|^2 - 2 a.b`, clamped at zero, because it is one
#' matrix product rather than a loop. That identity cancels catastrophically
#' for near-identical score vectors, which is exactly the regime the twin
#' check and the nearest few ranks live in, so the k distances that are kept
#' are recomputed by direct differencing before they are returned. The screen
#' decides the ranking; the returned numbers are the accurate ones.
#'
#' @param St [Matrix.] Target scores, rows named.
#' @param Sp [Matrix.] Pool scores, rows named, same columns as `St`.
#' @param k [Integer.] Neighbours to keep per target.
#' @param metric [Character.] `"euclidean"`, `"mahalanobis"` or `"cosine"`.
#' @param sdev [Numeric or NULL.] Per-component sd, required for
#'   `"mahalanobis"`. Default: `NULL`.
#' @param chunk_size [Integer.] Targets per chunk. Default: `500L`.
#'
#' @return [List.] `ids` (character matrix, targets by k, row names the
#'   target ids) and `dist` (numeric matrix, same shape), both ordered by
#'   increasing distance.
#'
#' @seealso [draw_neighbours()], [find_twins()]
#' @noRd
nearest_neighbours <- function(St, Sp, k,
                               metric     = c("euclidean", "mahalanobis", "cosine"),
                               sdev       = NULL,
                               chunk_size = 500L) {

  metric <- match.arg(metric)
  k      <- as.integer(k)

  if (k > nrow(Sp)) {

    cli::cli_abort("{.arg k} = {k} exceeds the pool of {nrow(Sp)} rows",
                   class = "horizons_input_error")

  }

  if (metric == "mahalanobis") {

    if (is.null(sdev)) {

      cli::cli_abort("{.arg metric = \"mahalanobis\"} needs {.arg sdev}, the per-component sd of the space",
                     class = "horizons_input_error")

    }

    St <- sweep(St, 2, sdev, "/")
    Sp <- sweep(Sp, 2, sdev, "/")

  }

  if (metric == "cosine") {

    ## The scores are mean-centred, so this is the angle about the pool's
    ## centroid, not the spectral angle about the origin. A row sitting on
    ## the centroid has zero norm and no defined angle; normalising it gives
    ## NaN, which propagates into order() as an unordered value. Leave those
    ## rows at NA: they sort last and carry a visible NA distance rather than
    ## a plausible-looking one.

    nt <- sqrt(rowSums(St^2))
    np <- sqrt(rowSums(Sp^2))

    nt[nt < .Machine$double.eps] <- NA_real_
    np[np < .Machine$double.eps] <- NA_real_

    St <- St / nt
    Sp <- Sp / np

  }

  pool_ids <- rownames(Sp)
  pool_sq  <- rowSums(Sp^2)
  n_t      <- nrow(St)

  ids  <- matrix(NA_character_, nrow = n_t, ncol = k, dimnames = list(rownames(St), NULL))
  dist <- matrix(NA_real_,      nrow = n_t, ncol = k, dimnames = list(rownames(St), NULL))

  chunks <- split(seq_len(n_t), ceiling(seq_len(n_t) / chunk_size))

  for (rows in chunks) {

    S <- St[rows, , drop = FALSE]

    D <- if (metric == "cosine") {
      1 - S %*% t(Sp)
    } else {
      sqrt(pmax(outer(rowSums(S^2), pool_sq, "+") - 2 * S %*% t(Sp), 0))
    }

    for (j in seq_along(rows)) {

      o <- order(D[j, ], method = "radix")[seq_len(k)]

      ## Recompute the kept distances without the cancellation-prone
      ## identity. Cosine is already a direct dot product of unit rows.

      dj <- if (metric == "cosine") {
        D[j, o]
      } else {
        sqrt(colSums((t(Sp[o, , drop = FALSE]) - S[j, ])^2))
      }

      ## The screen ranked these rows by the identity's answer, which in the
      ## near-duplicate regime is wrong by orders of magnitude. Re-order on
      ## the recomputed numbers before writing them back, or `dist` comes out
      ## unsorted and column one is not the nearest row.

      oo <- order(dj, method = "radix")
      o  <- o[oo]
      dj <- dj[oo]

      ids[rows[j], ]  <- pool_ids[o]
      dist[rows[j], ] <- dj

    }

  }

  list(ids = ids, dist = dist)

}


## ---------------------------------------------------------------------------
## find_twins() — Self-leakage on spectra
## ---------------------------------------------------------------------------

#' Flag pool rows that are a target's twin
#'
#' @description
#' A pool row is a target's twin when it sits far closer to that target than
#' the target's own surroundings do: distance zero, or below `ratio` times the
#' reference distance, the 75th percentile of the target's `k_ref` nearest
#' rows. The same sample, scanned twice, or present in both sets.
#'
#' @details
#' The rule is neighbourhood-relative rather than a gap between the first and
#' second nearest. A gap rule cannot see a pool that holds two or three
#' replicate scans of one sample: every replicate distance is tiny, so
#' `d1 / d2` is about one and nothing is flagged, which is the one case the
#' check exists for. Measuring each distance against the surrounding spread
#' flags the whole replicate cluster.
#'
#' The reference has to be wider than the cluster it is measuring, or the
#' replicates set the number they are then compared against. With four
#' self-rows among five neighbours, the median is a replicate distance and one
#' row is flagged instead of four; the 75th percentile moves the break from
#' half the columns to three quarters of them and no further. So the reference
#' is taken over `k_ref` columns, which `draw_neighbours()` sets by
#' `twin_reference_width()` regardless of the `k` being drawn, and the flag is
#' then independent of how many rows the caller happens to want.
#'
#' @param nn [List.] From `nearest_neighbours()`.
#' @param ratio [Numeric.] The fraction of the reference distance below which
#'   a neighbour is a twin. Default: `SELECT_TWIN_RATIO`.
#' @param k_ref [Integer or NULL.] Columns the reference percentile is taken
#'   over. `NULL` uses every column. Default: `NULL`.
#'
#' @return [Tibble.] `target_id`, `pool_id`, `distance`, `rank`,
#'   `reference_distance`, `reason` (`"exact"` or `"neighbourhood"`); zero
#'   rows when there are no twins.
#'
#' @seealso [nearest_neighbours()], [twin_reference()], [draw_neighbours()]
#' @noRd
find_twins <- function(nn, ratio = SELECT_TWIN_RATIO, k_ref = NULL) {

  d   <- nn$dist
  ref <- twin_reference(d, k_ref = k_ref)

  is_twin <- twin_flags(d, ref, ratio = ratio)
  hit     <- which(is_twin, arr.ind = TRUE)

  if (!nrow(hit)) {

    return(empty_twins())

  }

  ## which() on a matrix gives (row, col) in column order; sort so a target's
  ## exclusions come out nearest first, which is how they are reported.

  hit <- cbind(row = as.integer(hit[, "row"]), col = as.integer(hit[, "col"]))
  hit <- hit[order(hit[, "row"], hit[, "col"]), , drop = FALSE]

  distance <- d[hit]

  twin_tibble(
    target_id = rownames(nn$ids)[hit[, "row"]],
    pool_id   = nn$ids[hit],
    distance  = distance,
    rank      = as.integer(hit[, "col"]),
    reference = unname(ref[hit[, "row"]])
  )

}


#' The per-target reference distance: the 75th percentile of `k_ref` columns
#'
#' @param d [Matrix.] Distances, targets by neighbours, increasing along the
#'   row.
#' @param k_ref [Integer or NULL.] Columns to take the percentile over.
#'   `NULL` uses every column.
#'
#' @return [Numeric.] One reference distance per target row.
#' @noRd
twin_reference <- function(d, k_ref = NULL) {

  n_ref <- if (is.null(k_ref)) ncol(d) else min(as.integer(k_ref), ncol(d))

  apply(d[, seq_len(n_ref), drop = FALSE], 1,
        function(z) unname(stats::quantile(z, 0.75, na.rm = TRUE)))

}


#' The twin rule's reference width, one rule for every scope
#'
#' @description
#' How many of a target's nearest measured rows the reference percentile is
#' taken over. `draw_neighbours()` calls it for every scope, `"global"`
#' included, so the control arm's twin rule is the rule the other arms run.
#'
#' @details
#' The width is fixed rather than the `k` being drawn, which is why `k` is not
#' an argument: a replicate cluster cannot set its own reference, and a k
#' sweep does not move the flags. It has to stay local as well as wide,
#' though. The threshold is a fraction of the reference, so a reference taken
#' over most of the pool is a pool-wide spread and 5 % of that flags ordinary
#' nearest neighbours. A quarter of the measured rows is the ceiling, which
#' binds only on pools under `4 * twin_ref` rows; the floor of four keeps a
#' percentile meaningful on a tiny pool, and the width never exceeds the rows
#' there are.
#'
#' @param n_measured [Integer.] Pool rows with the property measured.
#' @param twin_ref [Integer.] The width before the cap.
#'   Default: `SELECT_TWIN_REF`.
#'
#' @return [Integer.] Columns the reference percentile is taken over.
#'
#' @seealso [twin_reference()], [draw_neighbours()]
#' @noRd
twin_reference_width <- function(n_measured, twin_ref = SELECT_TWIN_REF) {

  n_measured <- as.integer(n_measured)

  min(as.integer(twin_ref), max(4L, n_measured %/% 4L), n_measured)

}


#' The twin flag matrix for a distance matrix and its references
#'
#' @description
#' An `NA` distance is not a twin. Cosine leaves `NA` for a target with no
#' defined angle, and `NA` flags would silently become no flags in `which()`
#' while still being `NA` in any logical the caller wrote.
#'
#' @param d [Matrix.] Distances, targets by neighbours.
#' @param reference [Numeric.] One reference distance per target row.
#' @param ratio [Numeric.] Fraction of the reference below which a distance
#'   is a twin. Default: `SELECT_TWIN_RATIO`.
#'
#' @return [Logical matrix.] Same shape as `d`.
#' @noRd
twin_flags <- function(d, reference, ratio = SELECT_TWIN_RATIO) {

  flag <- (d == 0) | (d < ratio * reference)
  flag[is.na(flag)] <- FALSE
  flag

}


#' The twin table, built the one way, so every producer matches
#' @noRd
twin_tibble <- function(target_id, pool_id, distance, rank, reference) {

  tibble::tibble(
    target_id          = target_id,
    pool_id            = pool_id,
    distance           = distance,
    rank               = rank,
    reference_distance = reference,
    reason             = ifelse(distance == 0, "exact", "neighbourhood")
  )

}


#' The zero-row twin table, so every branch returns one shape
#' @noRd
empty_twins <- function() {

  tibble::tibble(
    target_id          = character(),
    pool_id            = character(),
    distance           = numeric(),
    rank               = integer(),
    reference_distance = numeric(),
    reason             = character()
  )

}


#' The zero-row exclusion table: `find_twins()`'s columns with property first
#' @noRd
empty_exclusions <- function() {

  dplyr::bind_cols(tibble::tibble(property = character()), empty_twins())

}


#' The zero-row short-draw table
#' @noRd
empty_short_draws <- function() {

  tibble::tibble(
    target_id   = character(),
    property    = character(),
    k_requested = integer(),
    k_drawn     = integer(),
    reason      = character()
  )

}


## ---------------------------------------------------------------------------
## resolve_k() — One k rule for the verb and the draw
## ---------------------------------------------------------------------------

#' Expand k to one integer per property
#'
#' @description
#' A scalar `k` applies to every property; a named `k` needs an entry for
#' each. The rule lives here so `select_training()`'s validation block and
#' `draw_neighbours()` cannot drift apart.
#'
#' @param k [Numeric.] A scalar, or a vector named by property.
#' @param properties [Character.] Properties being drawn for.
#'
#' @return [Named integer.] One entry per property, in `properties` order.
#' @noRd
resolve_k <- function(k, properties) {

  if (length(k) == 1L && is.null(names(k))) {

    return(stats::setNames(rep(as.integer(k), length(properties)), properties))

  }

  missing_k <- setdiff(properties, names(k))

  if (length(missing_k)) {

    cli::cli_abort("A named {.arg k} needs an entry for every property; missing {.val {missing_k}}",
                   class = "horizons_input_error")

  }

  stats::setNames(as.integer(k[properties]), properties)

}


## ---------------------------------------------------------------------------
## draw_neighbours() — The rule, per property
## ---------------------------------------------------------------------------

#' Draw each target's k nearest measured pool rows, per property
#'
#' @description
#' For each requested property, restricts the pool to rows where it is
#' measured, finds every target's nearest rows, drops every twin the
#' neighbourhood-relative rule flags, and keeps k. Rows are drawn per
#' property so the sparse-property case is honest rather than silently small.
#'
#' @details
#' The draw costs at most two distance passes per property. The first asks
#' for `twin_ref` columns rather than `k`, because the twin rule's reference
#' has to be wider than any replicate cluster it might be measuring; the
#' reference and the flags that follow from it are therefore the same
#' whatever `k` the caller asked for. Each target then takes the first `k`
#' unflagged columns, and the flagged columns it passed on the way are its
#' exclusions. A flagged row further out than the `k`-th row it kept is not
#' its exclusion: it was never in the neighbourhood.
#'
#' The second pass runs only when some target ran out of unflagged columns
#' inside what the first fetched, and asks for enough to cover the worst-hit
#' target. One that cannot reach `k` even then — because the property has too
#' few measured rows left — is recorded in `short_draws` rather than silently
#' returning a thinner neighbourhood. So is a target whose distances are all
#' `NA`, which `metric = "cosine"` produces for a row on the pool's centroid:
#' the ordering behind such a draw is the pool's own row order, so nothing is
#' drawn for it at all.
#'
#' @param St [Matrix.] Target scores, rows named.
#' @param Sp [Matrix.] Pool scores, rows named.
#' @param responses [Tibble.] `sample_id` plus one column per property, one
#'   row per pool row.
#' @param k [Integer.] A scalar, or a named vector with one entry per
#'   property.
#' @param properties [Character.] Properties to draw for.
#' @param metric,sdev,chunk_size Passed to `nearest_neighbours()`.
#' @param twin_ratio [Numeric.] Fraction of the reference distance below
#'   which a neighbour is a twin. Default: `SELECT_TWIN_RATIO`.
#' @param twin_ref [Integer.] Columns the twin rule's reference percentile is
#'   taken over, capped at a quarter of the measured rows so the reference
#'   stays local. Default: `SELECT_TWIN_REF`.
#' @param space_label [Character.] Value of the `space` column on the
#'   distance tables: `"all"` for the all-rows space, or the property name
#'   when the space was fit on that property's measured rows. Distances from
#'   two spaces are not comparable, and this is what says so.
#'   Default: `"all"`.
#'
#' @return [List.] `membership` (tibble: `target_id`, `property`, `space`,
#'   `pool_id`, `distance`, `rank`), `target_distances` (tibble:
#'   `target_id`, `property`, `space`, `nearest`, `mean_k`, after twin
#'   exclusion), `exclusions` (tibble: `property` plus `find_twins()`'s
#'   columns), `short_draws` (tibble: `target_id`, `property`,
#'   `k_requested`, `k_drawn`, `reason`), and `k` (the resolved per-property
#'   vector).
#'
#' @seealso [nearest_neighbours()], [find_twins()], [resolve_k()]
#' @noRd
draw_neighbours <- function(St, Sp, responses, k, properties,
                            metric      = "mahalanobis",
                            sdev        = NULL,
                            chunk_size  = 500L,
                            twin_ratio  = SELECT_TWIN_RATIO,
                            twin_ref    = SELECT_TWIN_REF,
                            space_label = "all") {

  k_by <- resolve_k(k, properties)

  membership  <- list()
  distances   <- list()
  exclusions  <- list()
  short_draws <- list()

  for (p in properties) {

    kp       <- k_by[[p]]
    measured <- responses$sample_id[!is.na(responses[[p]])]
    measured <- intersect(rownames(Sp), measured)

    if (length(measured) < kp) {

      cli::cli_abort(c(
        "Property {.field {p}} has {length(measured)} measured pool rows, fewer than k = {kp}",
        "i" = "Lower {.arg k} for this property or bring more referenced rows into the pool"
      ), class = "horizons_input_error")

    }

    Sp_p <- Sp[measured, , drop = FALSE]

    ## Pass 1: the reference set, and the twin flags it fixes -----------------

    ### The reference is not the neighbourhood: it is a fixed width, so the
    ### same pool row is or is not a twin of the same target whatever k was
    ### asked for, and a replicate cluster cannot set its own reference.
    ### twin_reference_width() holds the rule and its local cap.

    n_ref   <- twin_reference_width(length(measured), twin_ref)
    n_fetch <- min(max(kp, n_ref), length(measured))

    nn  <- nearest_neighbours(St, Sp_p, k = n_fetch, metric = metric,
                              sdev = sdev, chunk_size = chunk_size)
    ref <- twin_reference(nn$dist, k_ref = n_ref)
    fl  <- twin_flags(nn$dist, ref, ratio = twin_ratio)

    ## Pass 2: spares, only for a target that ran out inside the reference ----

    n_keep <- rowSums(!fl)

    if (any(n_keep < kp)) {

      ask <- min(n_fetch + max(kp - n_keep), length(measured))

      if (ask > n_fetch) {

        nn <- nearest_neighbours(St, Sp_p, k = ask, metric = metric,
                                 sdev = sdev, chunk_size = chunk_size)
        fl <- twin_flags(nn$dist, ref, ratio = twin_ratio)

      }

    }

    undefined <- character(0)

    for (i in seq_len(nrow(St))) {

      tid  <- rownames(St)[i]
      ids  <- nn$ids[i, ]
      dist <- nn$dist[i, ]
      flag <- fl[i, ]

      ## A target with no defined distance to anything is not a thin draw, it
      ## is a failed one: order() fell back on the pool's own row order, so
      ## the "neighbours" would be pool rows 1..k. Draw nothing.

      if (all(is.na(dist))) {

        short_draws[[length(short_draws) + 1L]] <- tibble::tibble(
          target_id   = tid,
          property    = p,
          k_requested = kp,
          k_drawn     = 0L,
          reason      = "no defined distance to any pool row"
        )

        undefined <- c(undefined, tid)

        next

      }

      keep_at <- which(!flag)
      take_at <- utils::head(keep_at, kp)

      ### The exclusions are the flagged columns this target passed on its way
      ### to k. A flagged row further out was never in its neighbourhood, so
      ### recording it would inflate the count and, under batch, subtract a
      ### row from the union that this target never drew.

      limit   <- if (length(take_at)) max(take_at) else length(flag)
      excl_at <- which(flag[seq_len(limit)])

      if (length(take_at) < kp) {

        short_draws[[length(short_draws) + 1L]] <- tibble::tibble(
          target_id   = tid,
          property    = p,
          k_requested = kp,
          k_drawn     = length(take_at),
          reason      = "too few measured rows left after twin exclusion"
        )

      }

      if (length(excl_at)) {

        exclusions[[length(exclusions) + 1L]] <- dplyr::bind_cols(
          tibble::tibble(property = p),
          twin_tibble(target_id = rep(tid, length(excl_at)),
                      pool_id   = unname(ids[excl_at]),
                      distance  = unname(dist[excl_at]),
                      rank      = as.integer(excl_at),
                      reference = rep(unname(ref[i]), length(excl_at)))
        )

      }

      membership[[length(membership) + 1L]] <- tibble::tibble(
        target_id = tid,
        property  = p,
        space     = space_label,
        pool_id   = unname(ids[take_at]),
        distance  = unname(dist[take_at]),
        rank      = seq_along(take_at)
      )

      distances[[length(distances) + 1L]] <- tibble::tibble(
        target_id = tid,
        property  = p,
        space     = space_label,
        nearest   = unname(dist[take_at][1]),
        mean_k    = mean(dist[take_at])
      )

    }

    if (length(undefined)) {

      cli::cli_warn(c(
        "{length(undefined)} target{?s} ha{?s/ve} no defined distance to any pool row for {.field {p}}; nothing was drawn for {?it/them}",
        "x" = "{.val {utils::head(undefined, 5)}}{if (length(undefined) > 5) ', ...' else ''}",
        "i" = "{.arg metric = \"cosine\"} has no angle for a row on the pool's centroid, since the scores are mean-centred; use {.val euclidean} or {.val mahalanobis} for these targets"
      ), class = "horizons_select_warning")

    }

  }

  list(
    membership       = dplyr::bind_rows(membership),
    target_distances = dplyr::bind_rows(distances),
    exclusions       = if (length(exclusions)) dplyr::bind_rows(exclusions) else empty_exclusions(),
    short_draws      = if (length(short_draws)) dplyr::bind_rows(short_draws) else empty_short_draws(),
    k                = k_by
  )

}


## ---------------------------------------------------------------------------
## cluster_targets() — For scope = "cluster"
## ---------------------------------------------------------------------------

#' Cluster the targets in score space
#'
#' @description
#' k-means on the target scores, the count chosen by mean silhouette over
#' 2 to `k_max` unless `clusters` fixes it, then clusters smaller than
#' `cluster_min` folded into the nearest remaining centroid. Lifted from
#' the coherent-batch experiment.
#'
#' @details
#' The clustering is Euclidean on the unscaled scores, while the draw is
#' whitened Mahalanobis by default, so the two see the space differently on
#' purpose. k-means partitions by variance, and the leading components are
#' where the chemical variance is; whitening would hand the trailing
#' components the same say in the partition as PC1, and those are the least
#' stable part of the decomposition. The grouping is a statement about which
#' targets belong together, not a distance the record reports, so it takes
#' the axes at their own weight.
#'
#' @param St [Matrix.] Target scores, rows named.
#' @param clusters [Integer or NULL.] Fixed cluster count, or `NULL` to
#'   choose by silhouette. Default: `NULL`.
#' @param cluster_min [Integer.] Floor on cluster size. A batch below twice
#'   this returns one cluster. Default: `30L`.
#' @param seed [Integer.] Seed for k-means; the caller's RNG state is
#'   restored afterwards. Default: `1L`.
#' @param k_max [Integer.] Upper bound of the silhouette search.
#'   Default: `10L`.
#'
#' @return [List.] `assignment` (named integer, one per target), `k`
#'   (clusters after merging), `k_chosen` (before merging), `silhouette`
#'   (mean, `NA` when not searched), `reason` (how `k` came about).
#'
#' @seealso [draw_neighbours()]
#' @noRd
cluster_targets <- function(St,
                            clusters    = NULL,
                            cluster_min = 30L,
                            seed        = 1L,
                            k_max       = 10L) {

  n   <- nrow(St)
  ids <- rownames(St)

  one <- function(reason) {

    list(assignment = stats::setNames(rep(1L, n), ids),
         k          = 1L,
         k_chosen   = 1L,
         silhouette = NA_real_,
         reason     = reason)

  }

  if (n < 2L * cluster_min) return(one("too few targets for two clusters at this floor"))

  ## Preserve the caller's RNG state -----------------------------------------

  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)

  sil <- NA_real_

  if (!is.null(clusters)) {

    K <- as.integer(clusters)
    set.seed(seed)
    km         <- stats::kmeans(St, centers = K, nstart = 25L, iter.max = 100L)
    cluster_of <- km$cluster
    reason     <- "specified"

  } else {

    Kmax <- min(as.integer(k_max), n %/% cluster_min)

    if (Kmax < 2L) return(one("too few targets for two clusters at this floor"))

    dd   <- stats::dist(St)
    best <- NULL

    for (K in 2:Kmax) {

      set.seed(seed)
      km <- stats::kmeans(St, centers = K, nstart = 25L, iter.max = 100L)
      s  <- mean(cluster::silhouette(km$cluster, dd)[, 3])

      if (is.null(best) || s > best$sil) best <- list(cluster_of = km$cluster, K = K, sil = s)

    }

    cluster_of <- best$cluster_of
    K          <- best$K
    sil        <- best$sil
    reason     <- "silhouette"

  }

  ## Fold clusters under the floor into the nearest remaining centroid -------

  a <- cluster_of

  repeat {

    sizes <- table(a)
    small <- as.integer(names(sizes)[sizes < cluster_min])
    if (!length(small) || length(sizes) == 1L) break

    s1     <- small[1]
    others <- setdiff(as.integer(names(sizes)), s1)
    cent   <- vapply(others, function(o) colMeans(St[a == o, , drop = FALSE]),
                     numeric(ncol(St)))

    ### vapply drops to a vector when the space has one component; the
    ### subtraction below needs components down the rows either way.
    dim(cent) <- c(ncol(St), length(others))

    for (i in which(a == s1)) {

      d2   <- colSums((cent - St[i, ])^2)
      a[i] <- others[which.min(d2)]

    }

  }

  a       <- as.integer(factor(a))
  k_final <- length(unique(a))

  if (k_final < K) {

    reason <- paste0(reason, " chose ", K, ", ", k_final,
                     " after merging clusters under ", cluster_min)

  }

  list(
    assignment = stats::setNames(a, ids),
    k          = k_final,
    k_chosen   = K,
    silhouette = sil,
    reason     = reason
  )

}
