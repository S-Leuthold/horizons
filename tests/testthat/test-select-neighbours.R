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


## =============================================================================
## find_twins()
## =============================================================================

test_that("find_twins() flags the exact twin and nothing else", {

  s  <- fixture_scores()
  nn <- nearest_neighbours(s$St, s$Sp, k = 6L, metric = "euclidean")

  tw <- find_twins(nn, ratio = 0.05)

  expect_s3_class(tw, "tbl_df")
  expect_named(tw, c("target_id", "pool_id", "distance", "second_distance"))
  expect_identical(nrow(tw), 1L)
  expect_identical(tw$target_id, s$fx$twin_id)
  expect_identical(tw$pool_id,   s$fx$twin_pool_id)
  expect_lt(tw$distance, 1e-8)

})


test_that("find_twins() uses the ratio to the second-nearest", {

  ## Arrange: a hand-built neighbour set
  nn <- list(
    ids  = rbind(a = c("x", "y", "z"), b = c("p", "q", "r")),
    dist = rbind(a = c(0.10, 1.00, 1.2), b = c(0.5, 0.6, 0.7))
  )

  expect_identical(find_twins(nn, ratio = 0.05)$target_id, character(0))
  expect_identical(find_twins(nn, ratio = 0.20)$target_id, "a")

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
  expect_named(m, c("target_id", "property", "pool_id", "distance", "rank"))

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


test_that("draw_neighbours() records the nearest and mean-of-k distance per target", {

  s   <- fixture_scores()
  rsp <- s$fx$pool$data$analysis[, c("sample_id", "clay", "oc")]

  out <- draw_neighbours(s$St, s$Sp, responses = rsp, k = 5L,
                         properties = "clay", metric = "euclidean")

  td <- out$target_distances
  expect_named(td, c("target_id", "property", "nearest", "mean_k"))
  expect_identical(nrow(td), nrow(s$St))
  expect_true(all(td$nearest <= td$mean_k))

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
