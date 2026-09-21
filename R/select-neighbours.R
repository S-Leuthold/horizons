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
#' the angle between score vectors. Euclidean distances use the identity
#' `|a - b|^2 = |a|^2 + |b|^2 - 2 a.b`, clamped at zero.
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

    St <- St / sqrt(rowSums(St^2))
    Sp <- Sp / sqrt(rowSums(Sp^2))

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

      o <- order(D[j, ])[seq_len(k)]
      ids[rows[j], ]  <- pool_ids[o]
      dist[rows[j], ] <- D[j, o]

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
#' A target whose nearest pool distance is zero, or below `ratio` times its
#' second-nearest, has a twin in the pool: the same sample, scanned twice
#' or present in both sets. The rule is the gap between first and second
#' nearest, from FTIR library searching, rather than an absolute cutoff.
#'
#' @param nn [List.] From `nearest_neighbours()` with at least two columns.
#' @param ratio [Numeric.] The fraction of the second-nearest distance below
#'   which the nearest is a twin. Default: `SELECT_TWIN_RATIO`.
#'
#' @return [Tibble.] `target_id`, `pool_id`, `distance`, `second_distance`;
#'   zero rows when there are no twins.
#'
#' @seealso [nearest_neighbours()]
#' @noRd
find_twins <- function(nn, ratio = SELECT_TWIN_RATIO) {

  d1 <- nn$dist[, 1]
  d2 <- if (ncol(nn$dist) >= 2L) nn$dist[, 2] else rep(Inf, length(d1))

  is_twin <- d1 == 0 | d1 < ratio * d2

  tibble::tibble(
    target_id       = rownames(nn$ids)[is_twin],
    pool_id         = unname(nn$ids[is_twin, 1]),
    distance        = unname(d1[is_twin]),
    second_distance = unname(d2[is_twin])
  )

}


## ---------------------------------------------------------------------------
## draw_neighbours() — The rule, per property
## ---------------------------------------------------------------------------

#' Draw each target's k nearest measured pool rows, per property
#'
#' @description
#' For each requested property, restricts the pool to rows where it is
#' measured, finds every target's nearest rows, drops a twin if one is
#' found, and keeps k. Rows are drawn per property so the sparse-property
#' case is honest rather than silently small.
#'
#' @param St [Matrix.] Target scores, rows named.
#' @param Sp [Matrix.] Pool scores, rows named.
#' @param responses [Tibble.] `sample_id` plus one column per property, one
#'   row per pool row.
#' @param k [Integer.] A scalar, or a named vector with one entry per
#'   property.
#' @param properties [Character.] Properties to draw for.
#' @param metric,sdev,chunk_size Passed to `nearest_neighbours()`.
#' @param twin_ratio [Numeric.] Passed to `find_twins()`.
#'
#' @return [List.] `membership` (tibble: `target_id`, `property`,
#'   `pool_id`, `distance`, `rank`), `target_distances` (tibble:
#'   `target_id`, `property`, `nearest`, `mean_k`, after twin exclusion),
#'   `exclusions` (tibble: `property` plus `find_twins()`'s columns), and
#'   `k` (the resolved per-property vector).
#'
#' @seealso [nearest_neighbours()], [find_twins()]
#' @noRd
draw_neighbours <- function(St, Sp, responses, k, properties,
                            metric     = "mahalanobis",
                            sdev       = NULL,
                            chunk_size = 500L,
                            twin_ratio = SELECT_TWIN_RATIO) {

  ## Resolve k per property ----------------------------------------------------

  if (length(k) == 1L && is.null(names(k))) {

    k_by <- stats::setNames(rep(as.integer(k), length(properties)), properties)

  } else {

    missing_k <- setdiff(properties, names(k))

    if (length(missing_k)) {

      cli::cli_abort("A named {.arg k} needs an entry for every property; missing {.val {missing_k}}",
                     class = "horizons_input_error")

    }

    k_by <- as.integer(k[properties])
    names(k_by) <- properties

  }

  membership <- list()
  distances  <- list()
  exclusions <- list()

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

    ## One extra so a twin can be dropped and k still reached ------------------

    nn <- nearest_neighbours(St, Sp[measured, , drop = FALSE],
                             k = min(kp + 1L, length(measured)),
                             metric = metric, sdev = sdev, chunk_size = chunk_size)

    twins <- find_twins(nn, ratio = twin_ratio)

    for (i in seq_len(nrow(St))) {

      tid  <- rownames(St)[i]
      ids  <- nn$ids[i, ]
      dist <- nn$dist[i, ]

      if (tid %in% twins$target_id) {

        drop <- ids == twins$pool_id[twins$target_id == tid][1]
        ids  <- ids[!drop]
        dist <- dist[!drop]

      }

      take <- seq_len(min(kp, length(ids)))

      membership[[length(membership) + 1L]] <- tibble::tibble(
        target_id = tid,
        property  = p,
        pool_id   = ids[take],
        distance  = dist[take],
        rank      = take
      )

      distances[[length(distances) + 1L]] <- tibble::tibble(
        target_id = tid,
        property  = p,
        nearest   = dist[take][1],
        mean_k    = mean(dist[take])
      )

    }

    if (nrow(twins)) exclusions[[p]] <- dplyr::bind_cols(tibble::tibble(property = p), twins)

  }

  list(
    membership       = dplyr::bind_rows(membership),
    target_distances = dplyr::bind_rows(distances),
    exclusions       = if (length(exclusions)) dplyr::bind_rows(exclusions) else
      tibble::tibble(property = character(), target_id = character(), pool_id = character(),
                     distance = numeric(), second_distance = numeric()),
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
  one <- function(reason) list(assignment = stats::setNames(rep(1L, n), ids),
                                k = 1L, k_chosen = 1L, silhouette = NA_real_, reason = reason)

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
    km     <- stats::kmeans(St, centers = K, nstart = 25L, iter.max = 100L)
    assign <- km$cluster
    reason <- "specified"

  } else {

    Kmax <- min(as.integer(k_max), n %/% cluster_min)

    if (Kmax < 2L) return(one("too few targets for two clusters at this floor"))

    dd   <- stats::dist(St)
    best <- NULL

    for (K in 2:Kmax) {

      set.seed(seed)
      km <- stats::kmeans(St, centers = K, nstart = 25L, iter.max = 100L)
      s  <- mean(cluster::silhouette(km$cluster, dd)[, 3])
      if (is.null(best) || s > best$sil) best <- list(assign = km$cluster, K = K, sil = s)

    }

    assign <- best$assign
    K      <- best$K
    sil    <- best$sil
    reason <- "silhouette"

  }

  ## Fold clusters under the floor into the nearest remaining centroid -------

  a <- assign

  repeat {

    sizes <- table(a)
    small <- as.integer(names(sizes)[sizes < cluster_min])
    if (!length(small) || length(sizes) == 1L) break

    s1     <- small[1]
    others <- setdiff(as.integer(names(sizes)), s1)
    cent   <- sapply(others, function(o) colMeans(St[a == o, , drop = FALSE]))
    cent   <- matrix(cent, ncol = length(others))

    for (i in which(a == s1)) {
      d2   <- colSums((cent - St[i, ])^2)
      a[i] <- others[which.min(d2)]
    }

  }

  a       <- as.integer(factor(a))
  k_final <- length(unique(a))

  if (k_final < K) reason <- sprintf("%s chose %d, %d after merging clusters under %d", reason, K, k_final, cluster_min)

  list(
    assignment = stats::setNames(a, ids),
    k          = k_final,
    k_chosen   = K,
    silhouette = sil,
    reason     = reason
  )

}
