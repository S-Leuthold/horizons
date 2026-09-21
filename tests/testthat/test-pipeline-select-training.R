# tests/testthat/test-pipeline-select-training.R
# Tests for select_training(): the verb end to end.


## =============================================================================
## Helpers
## =============================================================================

#' Run the verb quietly on the fixture
#' @noRd
quiet_select <- function(fx, ...) {

  select_training(fx$targets, fx$pool, ..., verbose = FALSE)

}


## =============================================================================
## Input validation
## =============================================================================

test_that("select_training() rejects inputs that are not horizons_data", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(select_training(data.frame(a = 1), fx$pool, verbose = FALSE),
               class = "horizons_input_error")
  expect_error(select_training(fx$targets, data.frame(a = 1), verbose = FALSE),
               class = "horizons_input_error")

})


test_that("select_training() needs a pool with at least one response", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(select_training(fx$targets, fx$targets, verbose = FALSE),
               regexp = "response", class = "horizons_input_error")

})


test_that("select_training() rejects unknown properties", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, properties = "ph"),
               regexp = "ph", class = "horizons_input_error")

})


test_that("select_training() rejects a bad k", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 0),           class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 2.5),         class = "horizons_input_error")
  expect_error(quiet_select(fx, k = c(clay = 5)), regexp = "oc", class = "horizons_input_error")

})


test_that("select_training() rejects bad scope, metric and space", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 5, scope  = "nope"), class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, metric = "nope"), class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, space  = "nope"), class = "horizons_input_error")

})


test_that("select_training(space = 'pls') needs exactly one property and an integer ncomp", {

  fx <- make_select_fixture(n_pool = 60)

  expect_error(quiet_select(fx, k = 5, space = "pls", ncomp = 3L),
               regexp = "one property", class = "horizons_input_error")
  expect_error(quiet_select(fx, k = 5, space = "pls", ncomp = 0.99, properties = "clay"),
               class = "horizons_input_error")

})


## =============================================================================
## scope = "global"
## =============================================================================

test_that("scope = 'global' returns every pool row on the targets' grid", {

  fx  <- make_select_fixture(n_pool = 60)
  out <- quiet_select(fx, scope = "global")

  expect_s3_class(out, "horizons_data")
  expect_identical(out$data$n_rows, 60L)
  expect_setequal(out$data$analysis$sample_id, fx$pool$data$analysis$sample_id)

  ## On the targets' grid, not the pool's
  pm <- predictor_matrix(out)
  expect_identical(pm$wavenumbers, fx$target_wn)

  ## Record and groups
  expect_identical(out$selection$settings$scope, "global")
  expect_identical(nrow(out$selection$groups), 1L)
  expect_identical(out$selection$groups$n_rows, 60L)
  expect_identical(nrow(out$selection$membership), 0L)
  expect_true(all(is.na(out$data$analysis$.drawn_by)))

})


## =============================================================================
## scope = "batch"
## =============================================================================

test_that("scope = 'batch' draws k per target per property into one training set", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10)

  m <- out$selection$membership
  expect_true(all(table(m$target_id, m$property) == 10L))

  ## Rows enter once
  expect_false(any(duplicated(out$data$analysis$sample_id)))
  expect_setequal(out$data$analysis$sample_id, unique(m$pool_id))

  ## One group, holding every row
  g <- out$selection$groups
  expect_identical(nrow(g), 1L)
  expect_identical(g$n_rows, out$data$n_rows)
  expect_identical(g$n_targets, 8L)
  expect_setequal(g$pool_ids[[1]], out$data$analysis$sample_id)

})


test_that("provenance columns are meta and agree with the membership", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10)

  rm <- out$data$role_map
  expect_identical(rm$role[rm$variable == ".drawn_by"],     "meta")
  expect_identical(rm$role[rm$variable == ".min_distance"], "meta")
  expect_identical(rm$role[rm$variable == ".group"],        "meta")

  m  <- out$selection$membership
  a  <- out$data$analysis
  by <- tapply(m$target_id, m$pool_id, function(t) length(unique(t)))
  md <- tapply(m$distance,  m$pool_id, min)

  expect_identical(unname(a$.drawn_by), as.integer(by[a$sample_id]))
  expect_equal(unname(a$.min_distance), as.numeric(md[a$sample_id]), tolerance = 1e-12)
  expect_true(all(a$.group == 1L))

})


test_that("the return keeps every pool response and carries no similarity columns", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "clay")

  expect_true(all(c("clay", "oc") %in% names(out$data$analysis)))
  expect_identical(out$data$n_responses, 2L)

  ## Only the targets' wavenumbers, nothing from the derivative space
  pm <- predictor_matrix(out)
  expect_identical(pm$wavenumbers, fx$target_wn)
  expect_identical(out$data$n_predictors, length(fx$target_wn))

})


test_that("the sparse property draws k rows that all have it measured", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "oc")

  m  <- out$selection$membership
  oc <- fx$pool$data$analysis$oc[match(m$pool_id, fx$pool$data$analysis$sample_id)]
  expect_false(any(is.na(oc)))

  ps <- out$selection$pool_sizes
  expect_identical(ps$property, "oc")
  expect_identical(ps$available, sum(!is.na(fx$pool$data$analysis$oc)))
  expect_identical(ps$drawn, out$data$n_rows)

})


test_that("a named k is honoured per property", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = c(clay = 12, oc = 6))

  m <- out$selection$membership
  expect_true(all(table(m$target_id[m$property == "clay"]) == 12L))
  expect_true(all(table(m$target_id[m$property == "oc"])   == 6L))
  expect_identical(out$selection$settings$k, c(clay = 12L, oc = 6L))

})


test_that("the twin is excluded from its target's rows and reported", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, properties = "clay")

  m    <- out$selection$membership
  mine <- m$pool_id[m$target_id == fx$twin_id]
  expect_false(fx$twin_pool_id %in% mine)

  ex <- out$selection$exclusions
  expect_identical(ex$target_id, fx$twin_id)
  expect_identical(ex$pool_id,   fx$twin_pool_id)

})


test_that("the record carries settings, reconciliation, pool identity and distances", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10)

  s <- out$selection
  expect_identical(s$settings$scope,  "batch")
  expect_identical(s$settings$metric, "mahalanobis")
  expect_identical(s$settings$space,  "pca")
  expect_true(s$settings$ncomp_retained >= 1L)
  expect_identical(s$reconciliation$operation, "resampled")
  expect_identical(s$pool$n_rows, 100L)
  expect_match(s$pool$id_hash, "^[0-9a-f]+$")
  expect_named(s$target_distances, c("target_id", "property", "nearest", "mean_k"))
  expect_identical(nrow(s$target_distances), 16L)
  expect_s3_class(s$timestamp, "POSIXct")

})


test_that("the return validates and runs through the ordinary chain", {

  skip_on_cran()

  fx  <- make_select_fixture(n_pool = 300)
  out <- quiet_select(fx, k = 30, properties = "clay")

  expect_no_error(validate_horizons_data(out))

  utils::capture.output({
    cfg <- configure(out, outcome = "clay", models = "rf", cv_folds = 3L)
    cfg <- validate(cfg)
  })

  expect_s3_class(cfg, "horizons_data")
  expect_true(any(cfg$data$role_map$role == "outcome"))

  ev <- suppressWarnings(evaluate(cfg, verbose = FALSE))
  expect_s3_class(ev, "horizons_eval")
  expect_true(nrow(ev$evaluation$results) >= 1L)

})


## =============================================================================
## scope = "cluster"
## =============================================================================

test_that("scope = 'cluster' yields one group per target cluster", {

  fx  <- make_select_fixture(n_pool = 100)
  out <- quiet_select(fx, k = 10, scope = "cluster", cluster_min = 2)

  g <- out$selection$groups
  expect_identical(nrow(g), 2L)
  expect_identical(sum(g$n_targets), 8L)

  ## Clusters follow the families
  cl  <- out$selection$clustering$assignment
  fam <- fx$family_of_target[names(cl)]
  expect_identical(length(unique(paste(fam, cl))), 2L)

  ## Every group's rows are the union of its targets' neighbourhoods
  m <- out$selection$membership
  for (i in seq_len(nrow(g))) {
    expect_setequal(g$pool_ids[[i]], unique(m$pool_id[m$target_id %in% g$target_ids[[i]]]))
  }

  ## .group is populated and the return is still the union, rows once
  expect_true(all(out$data$analysis$.group %in% 1:2))
  expect_false(any(duplicated(out$data$analysis$sample_id)))
  expect_setequal(out$data$analysis$sample_id, unique(m$pool_id))

})


test_that("scope = 'cluster' falls back to one group below the floor, with a message", {

  fx <- make_select_fixture(n_pool = 100)

  expect_message(out <- quiet_select(fx, k = 10, scope = "cluster", cluster_min = 5),
                 regexp = "one group")

  expect_identical(nrow(out$selection$groups), 1L)
  expect_identical(out$selection$clustering$k, 1L)

})


## =============================================================================
## scope = "sample"
## =============================================================================

test_that("scope = 'sample' gives one group per target over the same union", {

  fx    <- make_select_fixture(n_pool = 100)
  batch <- quiet_select(fx, k = 10)
  samp  <- quiet_select(fx, k = 10, scope = "sample")

  expect_setequal(samp$data$analysis$sample_id, batch$data$analysis$sample_id)

  g <- samp$selection$groups
  expect_identical(nrow(g), 8L)
  expect_true(all(g$n_targets == 1L))
  expect_setequal(unlist(g$target_ids), fx$targets$data$analysis$sample_id)

  m <- samp$selection$membership
  for (i in seq_len(nrow(g))) {
    expect_setequal(g$pool_ids[[i]], unique(m$pool_id[m$target_id == g$target_ids[[i]]]))
  }

  expect_false(".group" %in% names(samp$data$analysis))

})


## =============================================================================
## Levers reach the draw
## =============================================================================

test_that("metric and space levers change which rows are drawn", {

  fx <- make_select_fixture(n_pool = 100)

  base <- quiet_select(fx, k = 10, properties = "clay")
  eucl <- quiet_select(fx, k = 10, properties = "clay", metric = "euclidean")
  cosn <- quiet_select(fx, k = 10, properties = "clay", metric = "cosine")
  pls  <- quiet_select(fx, k = 10, properties = "clay", space = "pls", ncomp = 3L)

  ids <- function(o) o$selection$membership$pool_id

  expect_false(identical(ids(base), ids(eucl)))
  expect_false(identical(ids(base), ids(cosn)))
  expect_false(identical(ids(base), ids(pls)))
  expect_identical(pls$selection$settings$space, "pls")

})


test_that("space_rows = 'measured' fits a space per property on its measured rows", {

  fx  <- make_select_fixture(n_pool = 100)
  all <- quiet_select(fx, k = 10)
  mea <- quiet_select(fx, k = 10, space_rows = "measured")

  s <- mea$selection$settings
  expect_identical(s$space_rows, "measured")
  expect_named(s$ncomp_by_property, c("clay", "oc"))
  expect_named(s$space_n_rows,      c("clay", "oc"))
  expect_identical(unname(s$space_n_rows["clay"]), 100L)
  expect_identical(unname(s$space_n_rows["oc"]),   sum(!is.na(fx$pool$data$analysis$oc)))
  expect_null(all$selection$settings$ncomp_by_property)

  ## clay is measured on every row, so its space is the all-rows space and
  ## the draw agrees; oc's space is fit on half the rows and can differ
  ids <- function(o, p) { m <- o$selection$membership; m$pool_id[m$property == p] }
  expect_identical(ids(mea, "clay"), ids(all, "clay"))

  ## Still k per target per property, every oc row measured
  m <- mea$selection$membership
  expect_true(all(table(m$target_id, m$property) == 10L))
  oc <- fx$pool$data$analysis$oc[match(m$pool_id[m$property == "oc"], fx$pool$data$analysis$sample_id)]
  expect_false(any(is.na(oc)))

})


test_that("space_rows = 'measured' is ignored under scope = 'global' and rejected when invalid", {

  fx  <- make_select_fixture(n_pool = 60)
  out <- quiet_select(fx, scope = "global", space_rows = "measured")

  expect_null(out$selection$settings$ncomp_by_property)
  expect_error(quiet_select(fx, k = 5, space_rows = "some"), class = "horizons_input_error")

})


## =============================================================================
## Output
## =============================================================================

test_that("verbose = FALSE prints nothing", {

  fx <- make_select_fixture(n_pool = 60)

  expect_silent(select_training(fx$targets, fx$pool, k = 5, verbose = FALSE))

})


test_that("verbose = TRUE reports the pool, the space and the draw", {

  fx  <- make_select_fixture(n_pool = 60)
  txt <- utils::capture.output(out <- select_training(fx$targets, fx$pool, k = 5))

  expect_true(any(grepl("Selecting", txt)))
  expect_true(any(grepl("clay", txt)))
  expect_true(any(grepl("components", txt)))
  expect_true(any(grepl("twin", txt, ignore.case = TRUE)))

})


test_that("print() shows a Selection section", {

  fx  <- make_select_fixture(n_pool = 60)
  out <- quiet_select(fx, k = 5)
  txt <- utils::capture.output(print(out))

  expect_true(any(grepl("^Selection", txt)))
  expect_true(any(grepl("batch", txt)))
  expect_true(any(grepl("k: 5", txt)))

})
