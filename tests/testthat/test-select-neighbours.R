# tests/testthat/test-select-neighbours.R
# Tests for nearest_neighbours(), find_twins(), draw_neighbours() and
# cluster_targets(): the rule of select_training().


## =============================================================================
## Helpers
## =============================================================================

#' Pool and target scores in the default space, reconciled
#' @noRd
fixture_scores <- function(n_pool = 60, ncomp = 4L, seed = 1) {

  fx <- make_select_fixture(n_pool = n_pool, seed = seed)
  rc <- reconcile_axes(fx$pool, fx$targets)
  tm <- predictor_matrix(fx$targets)
  sp <- build_similarity_space(rc$matrix, rc$wavenumbers, ncomp = ncomp)
  st <- project_similarity(sp, tm$matrix, tm$wavenumbers)

  list(fx = fx, space = sp, Sp = sp$scores, St = st)

}

#' Brute-force distance matrix, targets by pool
#' @noRd
brute_distance <- function(St, Sp, metric = "euclidean", sdev = NULL) {

  if (metric == "mahalanobis") {
    St <- sweep(St, 2, sdev, "/")
    Sp <- sweep(Sp, 2, sdev, "/")
    metric <- "euclidean"
  }

  if (metric == "euclidean") {
    D <- as.matrix(stats::dist(rbind(St, Sp)))[seq_len(nrow(St)), nrow(St) + seq_len(nrow(Sp))]
  } else {
    num <- St %*% t(Sp)
    den <- outer(sqrt(rowSums(St^2)), sqrt(rowSums(Sp^2)))
    D   <- 1 - num / den
  }

  dimnames(D) <- list(rownames(St), rownames(Sp))
  D

}


## =============================================================================
## nearest_neighbours()
## =============================================================================

test_that("nearest_neighbours() matches brute force for euclidean", {

  s  <- fixture_scores()
  nn <- nearest_neighbours(s$St, s$Sp, k = 5L, metric = "euclidean")
  D  <- brute_distance(s$St, s$Sp)

  expect_identical(dim(nn$ids),  c(nrow(s$St), 5L))
  expect_identical(dim(nn$dist), c(nrow(s$St), 5L))
  expect_identical(rownames(nn$ids), rownames(s$St))

  for (i in seq_len(nrow(s$St))) {

    o <- order(D[i, ])[1:5]
    expect_identical(nn$ids[i, ], colnames(D)[o])
    expect_equal(nn$dist[i, ], unname(D[i, o]), tolerance = 1e-10)

  }

})


test_that("nearest_neighbours() mahalanobis is euclidean on sdev-scaled scores", {

  s  <- fixture_scores()
  nn <- nearest_neighbours(s$St, s$Sp, k = 5L, metric = "mahalanobis", sdev = s$space$sdev)
  D  <- brute_distance(s$St, s$Sp, metric = "mahalanobis", sdev = s$space$sdev)

  for (i in seq_len(nrow(s$St))) {

    o <- order(D[i, ])[1:5]
    expect_identical(nn$ids[i, ], colnames(D)[o])
    expect_equal(nn$dist[i, ], unname(D[i, o]), tolerance = 1e-10)

  }

})


test_that("nearest_neighbours() mahalanobis needs sdev", {

  s <- fixture_scores()

  expect_error(nearest_neighbours(s$St, s$Sp, k = 5L, metric = "mahalanobis"),
               class = "horizons_input_error")

})


test_that("nearest_neighbours() cosine matches brute force", {

  s  <- fixture_scores()
  nn <- nearest_neighbours(s$St, s$Sp, k = 5L, metric = "cosine")
  D  <- brute_distance(s$St, s$Sp, metric = "cosine")

  for (i in seq_len(nrow(s$St))) {

    o <- order(D[i, ])[1:5]
    expect_identical(nn$ids[i, ], colnames(D)[o])
    expect_equal(nn$dist[i, ], unname(D[i, o]), tolerance = 1e-10)

  }

})


test_that("nearest_neighbours() chunked equals unchunked", {

  s  <- fixture_scores()
  a  <- nearest_neighbours(s$St, s$Sp, k = 7L, metric = "euclidean")
  b  <- nearest_neighbours(s$St, s$Sp, k = 7L, metric = "euclidean", chunk_size = 3L)

  expect_identical(a$ids, b$ids)
  expect_equal(a$dist, b$dist, tolerance = 1e-12)

})


test_that("nearest_neighbours() aborts when k exceeds the pool", {

  s <- fixture_scores(n_pool = 20)

  expect_error(nearest_neighbours(s$St, s$Sp, k = 25L, metric = "euclidean"),
               regexp = "20", class = "horizons_input_error")

})


test_that("nearest_neighbours() puts the exact twin at distance zero", {

  s  <- fixture_scores()
  nn <- nearest_neighbours(s$St, s$Sp, k = 3L, metric = "euclidean")

  expect_identical(unname(nn$ids[s$fx$twin_id, 1]), s$fx$twin_pool_id)
  expect_lt(unname(nn$dist[s$fx$twin_id, 1]), 1e-8)

})


test_that("nearest_neighbours() returns accurate small distances, not the identity's", {

  ## Arrange: score vectors far from the origin but a hair apart. The
  ## |a|^2 + |b|^2 - 2ab identity cancels to about sqrt(eps) * |a| here,
  ## which is the regime the twin rule makes its decisions in.
  set.seed(11)
  base <- matrix(stats::rnorm(6, mean = 1e4, sd = 10), nrow = 1)

  Sp <- rbind(p1 = base[1, ], p2 = base[1, ] + 1e-6, p3 = base[1, ] + 50)
  St <- rbind(t1 = base[1, ] + 5e-7)

  nn    <- nearest_neighbours(St, Sp, k = 2L, metric = "euclidean")
  truth <- sqrt(colSums((t(Sp[nn$ids[1, ], , drop = FALSE]) - St[1, ])^2))

  ## Relative agreement, not absolute: the identity's answer here is wrong
  ## by orders of magnitude, so a loose absolute tolerance would not see it.
  expect_equal(unname(nn$dist[1, ]) / unname(truth), c(1, 1), tolerance = 1e-6)
  expect_gt(nn$dist[1, 1], 0)

})


test_that("nearest_neighbours() returns the k in the recomputed order, not the screen's", {

  ## Arrange: a five-member near-duplicate cluster a few times 1e-7 apart,
  ## far from the origin. The screening identity's ranking of these is noise,
  ## and the recomputed distances used to be written back in its order, so
  ## `dist` came out unsorted and column one was not the nearest row.

  set.seed(13)
  base <- matrix(stats::rnorm(6, mean = 1e4, sd = 10), nrow = 1)
  off  <- c(p1 = 0, p2 = 3e-7, p3 = 1e-7, p4 = 2e-7, p5 = 4e-7)

  Sp <- do.call(rbind, lapply(off, function(o) base[1, ] + o))
  Sp <- rbind(Sp, far = base[1, ] + 10)
  St <- rbind(t1 = base[1, ] + 1.5e-7)

  nn <- nearest_neighbours(St, Sp, k = 5L, metric = "euclidean")

  truth <- sort(sqrt(colSums((t(Sp) - St[1, ])^2)))

  expect_false(is.unsorted(nn$dist[1, ]))
  expect_identical(nn$ids[1, ], names(truth)[1:5])
  expect_equal(nn$dist[1, ] / unname(truth[1:5]), rep(1, 5), tolerance = 1e-6)

  ## The rank-one row is the true minimum, which is what the twin rule and
  ## the target_distances `nearest` column both read.
  expect_identical(unname(nn$ids[1, 1]), "p3")

})


## =============================================================================
## find_twins()
## =============================================================================

test_that("find_twins() flags the exact twin and nothing else", {

  s  <- fixture_scores()
  nn <- nearest_neighbours(s$St, s$Sp, k = 6L, metric = "euclidean")

  tw <- find_twins(nn, ratio = 0.05)

  expect_s3_class(tw, "tbl_df")
  expect_named(tw, c("target_id", "pool_id", "distance", "rank", "reference_distance", "reason"))
  expect_identical(nrow(tw), 1L)
  expect_identical(tw$target_id, s$fx$twin_id)
  expect_identical(tw$pool_id,   s$fx$twin_pool_id)
  expect_lt(tw$distance, 1e-8)
  expect_identical(tw$reason, "exact")

})


test_that("find_twins() measures each neighbour against the reference distance", {

  ## Arrange: a hand-built neighbour set. Target a has one close row against
  ## a 75th percentile of 1.1; target b's neighbourhood is uniformly tight.
  nn <- list(
    ids  = rbind(a = c("x", "y", "z"), b = c("p", "q", "r")),
    dist = rbind(a = c(0.10, 1.00, 1.2), b = c(0.5, 0.6, 0.7))
  )

  expect_identical(find_twins(nn, ratio = 0.05)$target_id, character(0))

  hit <- find_twins(nn, ratio = 0.20)
  expect_identical(hit$target_id, "a")
  expect_identical(hit$pool_id,   "x")
  expect_identical(hit$rank,      1L)
  expect_identical(hit$reason,    "neighbourhood")
  expect_equal(hit$reference_distance, 1.1)

})


test_that("find_twins() survives a replicate majority the median could not", {

  ## Arrange: four replicate scans of the target, then five ordinary rows.
  ## Over the first five columns alone the cluster is the majority and sets
  ## its own reference; over all nine it does not.
  d  <- c(0, 9e-4, 9.5e-4, 1.2e-3, 0.29, 0.30, 0.31, 0.33, 0.35)
  nn <- list(ids = rbind(a = paste0("n", 1:9)), dist = rbind(a = d))

  ## The neighbourhood-width reference the rule used to take: a replicate's
  ## own distance, and 5 % of it is below every other replicate.
  narrow <- find_twins(nn, ratio = 0.05, k_ref = 5L)
  expect_identical(narrow$pool_id, "n1")

  ## Widened past the cluster, the whole cluster is flagged.
  wide <- find_twins(nn, ratio = 0.05)
  expect_identical(wide$pool_id, paste0("n", 1:4))
  expect_true(all(wide$reference_distance == stats::quantile(d, 0.75)))

})


test_that("find_twins() flags every member of a replicate cluster, not just the first", {

  ## The case a first-to-second-nearest gap rule cannot see: three rows all
  ## at essentially zero, so d1 / d2 is about one and no gap opens.
  nn <- list(
    ids  = rbind(a = c("r1", "r2", "r3", "s", "t", "u", "v")),
    dist = rbind(a = c(0.001, 0.0012, 0.0015, 1.0, 1.1, 1.2, 1.3))
  )

  hit <- find_twins(nn, ratio = 0.05)

  expect_identical(hit$pool_id, c("r1", "r2", "r3"))
  expect_true(all(hit$reason == "neighbourhood"))

})


test_that("find_twins() takes its reference over k_ref only, so spares do not move the flags", {

  nn_core  <- list(ids  = rbind(a = c("x", "y", "z")),
                   dist = rbind(a = c(0.01, 1.00, 1.20)))
  nn_spare <- list(ids  = rbind(a = c("x", "y", "z", "w", "v")),
                   dist = rbind(a = c(0.01, 1.00, 1.20, 1.4, 1.6)))

  a <- find_twins(nn_core,  ratio = 0.05)
  b <- find_twins(nn_spare, ratio = 0.05, k_ref = 3L)

  expect_identical(a$pool_id, b$pool_id)
  expect_equal(a$reference_distance, b$reference_distance)

})


test_that("find_twins() does not flag an NA distance", {

  ## A cosine target on the pool's centroid has no defined angle to anything.
  ## NA is not zero and not below the reference; it is unknown.
  nn <- list(ids  = rbind(a = c("x", "y", "z")),
             dist = rbind(a = c(NA_real_, NA_real_, NA_real_)))

  expect_identical(nrow(find_twins(nn, ratio = 0.05)), 0L)

})


## =============================================================================
## resolve_k()
## =============================================================================

test_that("resolve_k() expands a scalar and honours a named vector", {

  expect_identical(resolve_k(5, c("clay", "oc")),               c(clay = 5L, oc = 5L))
  expect_identical(resolve_k(c(clay = 12, oc = 6), c("clay", "oc")), c(clay = 12L, oc = 6L))

  ## Order follows properties, not the names of k
  expect_identical(resolve_k(c(oc = 6, clay = 12), c("clay", "oc")), c(clay = 12L, oc = 6L))

  expect_error(resolve_k(c(clay = 5), c("clay", "oc")),
               regexp = "oc", class = "horizons_input_error")

})


## =============================================================================
## draw_neighbours()
## =============================================================================

test_that("draw_neighbours() gives k rows per target per property, all measured", {

  s   <- fixture_scores()
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay", "oc")]

  out <- draw_neighbours(s$St, s$Sp, responses = rsp, k = 5L,
                         properties = c("clay", "oc"), metric = "euclidean")

  m <- out$membership
  expect_s3_class(m, "tbl_df")
  expect_named(m, c("target_id", "property", "space", "pool_id", "distance", "rank"))
  expect_true(all(m$space == "all"))

  counts <- table(m$target_id, m$property)
  expect_true(all(counts == 5L))
  expect_true(all(m$rank[m$property == "clay"] %in% 1:5))

  oc_rows <- m$pool_id[m$property == "oc"]
  expect_false(any(is.na(rsp$oc[match(oc_rows, rsp$sample_id)])))

})


test_that("draw_neighbours() takes a named k per property", {

  s   <- fixture_scores()
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay", "oc")]

  out <- draw_neighbours(s$St, s$Sp, responses = rsp, k = c(clay = 10L, oc = 4L),
                         properties = c("clay", "oc"), metric = "euclidean")

  counts <- table(out$membership$target_id, out$membership$property)
  expect_true(all(counts[, "clay"] == 10L))
  expect_true(all(counts[, "oc"]   == 4L))

})


test_that("draw_neighbours() excludes the twin from its target's rows and reports it", {

  s   <- fixture_scores()
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay", "oc")]

  out <- draw_neighbours(s$St, s$Sp, responses = rsp, k = 5L,
                         properties = "clay", metric = "euclidean", twin_ratio = 0.05)

  twin_rows <- out$membership[out$membership$target_id == s$fx$twin_id, ]
  expect_identical(nrow(twin_rows), 5L)
  expect_false(s$fx$twin_pool_id %in% twin_rows$pool_id)

  expect_identical(out$exclusions$target_id, s$fx$twin_id)
  expect_identical(out$exclusions$pool_id,   s$fx$twin_pool_id)

})


test_that("draw_neighbours() flags the whole replicate cluster at k below its size", {

  ## The regression: with four self-rows in a neighbourhood of five, the
  ## reference the rule used to take was itself a replicate distance, and one
  ## row was flagged instead of four. The reference is now a fixed width,
  ## wider than the cluster and independent of k, so the same four rows are
  ## flagged whether k is 3, 5 or 10.

  fxr <- make_select_fixture(n_pool = 60, seed = 3, n_replicates = 3)
  rc  <- reconcile_axes(fxr$pool, fxr$targets)
  tm  <- predictor_matrix(fxr$targets)
  sp  <- build_similarity_space(rc$matrix, rc$wavenumbers, ncomp = 4L)
  st  <- project_similarity(sp, tm$matrix, tm$wavenumbers)
  rsp <- fxr$pool$data$analysis[, c("sample_id", "clay")]

  self <- c(fxr$twin_pool_id, fxr$replicate_pool_ids)

  for (k in c(3L, 5L, 10L)) {

    out  <- draw_neighbours(st, sp$scores, responses = rsp, k = k,
                            properties = "clay", metric = "euclidean", twin_ratio = 0.05)
    mine <- out$membership[out$membership$target_id == fxr$twin_id, ]

    expect_setequal(out$exclusions$pool_id[out$exclusions$target_id == fxr$twin_id], self)
    expect_identical(nrow(mine), k)
    expect_false(any(self %in% mine$pool_id))

  }

})


test_that("draw_neighbours() treats an undefined cosine target as a failed draw", {

  ## A target sitting on the pool's centroid has no defined angle to any pool
  ## row, so every distance is NA and order() falls back on the pool's own
  ## row order. Drawing pool rows 1..k for it would be the silent failure.

  s   <- fixture_scores()
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay")]

  St <- s$St
  St[1, ] <- 0

  expect_warning(out <- draw_neighbours(St, s$Sp, responses = rsp, k = 5L,
                                        properties = "clay", metric = "cosine"),
                 regexp = "no defined distance", class = "horizons_select_warning")

  bad <- rownames(St)[1]

  expect_false(bad %in% out$membership$target_id)
  expect_false(bad %in% out$target_distances$target_id)

  sd_tbl <- out$short_draws[out$short_draws$target_id == bad, ]
  expect_identical(nrow(sd_tbl), 1L)
  expect_identical(sd_tbl$k_drawn, 0L)
  expect_match(sd_tbl$reason, "no defined distance")

  ## And the targets that do have an angle are drawn as usual
  expect_identical(length(unique(out$membership$target_id)), nrow(St) - 1L)

})


test_that("draw_neighbours() records the nearest and mean-of-k distance per target", {

  s   <- fixture_scores()
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay", "oc")]

  out <- draw_neighbours(s$St, s$Sp, responses = rsp, k = 5L,
                         properties = "clay", metric = "euclidean")

  td <- out$target_distances
  expect_named(td, c("target_id", "property", "space", "nearest", "mean_k"))
  expect_identical(nrow(td), nrow(s$St))
  expect_true(all(td$nearest <= td$mean_k))

})


test_that("draw_neighbours() drops a whole replicate cluster and still reaches k", {

  ## Arrange: three replicate scans of the twinned pool row, so the gap rule
  ## has nothing to see and the neighbourhood-relative rule has everything.
  s   <- fixture_scores(n_pool = 60, seed = 3)
  fxr <- make_select_fixture(n_pool = 60, seed = 3, n_replicates = 3)
  rc  <- reconcile_axes(fxr$pool, fxr$targets)
  tm  <- predictor_matrix(fxr$targets)
  sp  <- build_similarity_space(rc$matrix, rc$wavenumbers, ncomp = 4L)
  st  <- project_similarity(sp, tm$matrix, tm$wavenumbers)
  rsp <- fxr$pool$data$analysis[, c("sample_id", "clay")]

  ## Act
  out <- draw_neighbours(st, sp$scores, responses = rsp, k = 10L,
                         properties = "clay", metric = "euclidean", twin_ratio = 0.05)

  ## Assert: every replicate and the source row are flagged for the twin
  ## target, none of them is in its neighbourhood, and it still has 10 rows.
  self <- c(fxr$twin_pool_id, fxr$replicate_pool_ids)
  mine <- out$membership[out$membership$target_id == fxr$twin_id, ]

  expect_identical(nrow(mine), 10L)
  expect_false(any(self %in% mine$pool_id))

  flagged <- out$exclusions$pool_id[out$exclusions$target_id == fxr$twin_id]
  expect_setequal(flagged, self)
  expect_identical(nrow(out$short_draws), 0L)

})


test_that("draw_neighbours() records a short draw rather than returning a thin neighbourhood", {

  ## Arrange: k equal to the measured rows, so there is no spare to replace
  ## the twin with.
  s   <- fixture_scores(n_pool = 60)
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay")]
  k   <- nrow(rsp)

  out <- draw_neighbours(s$St, s$Sp, responses = rsp, k = k,
                         properties = "clay", metric = "euclidean", twin_ratio = 0.05)

  sd_tbl <- out$short_draws
  expect_named(sd_tbl, c("target_id", "property", "k_requested", "k_drawn", "reason"))
  expect_gte(nrow(sd_tbl), 1L)
  expect_true(s$fx$twin_id %in% sd_tbl$target_id)
  expect_true(all(sd_tbl$property    == "clay"))
  expect_true(all(sd_tbl$k_requested == as.integer(k)))
  expect_true(all(sd_tbl$k_drawn     <  as.integer(k)))

  ## The record accounts for the shortfall exactly: what is missing from a
  ## target's neighbourhood is what was flagged for it.
  drawn   <- table(out$membership$target_id)
  flagged <- table(out$exclusions$target_id)

  for (tid in sd_tbl$target_id) {

    expect_identical(as.integer(drawn[[tid]]), as.integer(k) - as.integer(flagged[[tid]]))

  }

})


test_that("draw_neighbours() labels the space it measured in", {

  s   <- fixture_scores()
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay")]

  out <- draw_neighbours(s$St, s$Sp, responses = rsp, k = 5L, properties = "clay",
                         metric = "euclidean", space_label = "clay")

  expect_true(all(out$membership$space       == "clay"))
  expect_true(all(out$target_distances$space == "clay"))

})


test_that("draw_neighbours() aborts when a property has fewer measured rows than k", {

  s   <- fixture_scores(n_pool = 20)
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay", "oc")]   # oc has 10 measured

  expect_error(draw_neighbours(s$St, s$Sp, responses = rsp, k = 15L,
                               properties = "oc", metric = "euclidean"),
               regexp = "oc.*10", class = "horizons_input_error")

})


## =============================================================================
## cluster_targets()
## =============================================================================

test_that("cluster_targets() recovers the two families with a low floor", {

  s   <- fixture_scores(n_pool = 100)
  out <- cluster_targets(s$St, cluster_min = 2L, seed = 1L)

  expect_identical(out$k, 2L)
  expect_identical(length(out$assignment), nrow(s$St))
  expect_identical(names(out$assignment), rownames(s$St))

  ## Same family, same cluster
  fam <- s$fx$family_of_target[names(out$assignment)]
  expect_identical(length(unique(paste(fam, out$assignment))), 2L)
  expect_identical(out$reason, "silhouette")

})


test_that("cluster_targets(clusters = ) forces the count before merging", {

  s   <- fixture_scores(n_pool = 100)
  out <- cluster_targets(s$St, clusters = 3L, cluster_min = 1L, seed = 1L)

  expect_identical(out$k, 3L)
  expect_identical(out$k_chosen, 3L)

})


test_that("cluster_targets() merges clusters under the floor into the nearest", {

  s   <- fixture_scores(n_pool = 100)
  out <- cluster_targets(s$St, clusters = 3L, cluster_min = 3L, seed = 1L)

  expect_lt(out$k, 3L)
  expect_match(out$reason, "merg")
  expect_true(all(table(out$assignment) >= 3L))

})


test_that("cluster_targets() returns one cluster when the batch is too small", {

  s   <- fixture_scores(n_pool = 100)
  out <- cluster_targets(s$St, cluster_min = 5L, seed = 1L)   # 8 targets < 2 * 5

  expect_identical(out$k, 1L)
  expect_true(all(out$assignment == 1L))
  expect_match(out$reason, "too few")

})
