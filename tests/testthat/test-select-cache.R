# tests/testthat/test-select-cache.R
# The flip and the cached similarity space: distances measured in the
# library's own space on its grid, the space cached beside a registered
# library or a library file, and the three coverage tiers.
#
# make_mini_ossl() and local_mini_registry() are defined in
# test-select-library.R; the few lines they need are repeated here so this
# file stands alone.


## ---------------------------------------------------------------------------
## Fixture: a mini registered library and targets on its grid
## ---------------------------------------------------------------------------

mini_entry <- function(dir) {

  set.seed(3)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  wn  <- seq(700L, 600L, by = -2L)
  n   <- 60L
  ids <- sprintf("id%02d", seq_len(n))

  spec <- matrix(stats::runif(n * length(wn), 0.2, 1.2), nrow = n)
  colnames(spec) <- sprintf("scan_mir.%d_abs", wn)

  paths <- c(mir  = file.path(dir, "mir.csv.gz"),
             lab  = file.path(dir, "lab.csv.gz"),
             site = file.path(dir, "site.csv.gz"))

  data.table::fwrite(data.frame(id.layer_uuid_txt = ids,
                                dataset.code_ascii_txt = "KSSL.SSL",
                                scan.mir.model.name_utf8_txt = "Bruker Vertex 70 with HTS-XT accessory",
                                spec, check.names = FALSE), paths[["mir"]])
  data.table::fwrite(data.frame(id.layer_uuid_txt = ids,
                                clay.tot_usda.a334_w.pct = stats::runif(n, 5, 50)), paths[["lab"]])
  data.table::fwrite(data.frame(id.layer_uuid_txt = ids,
                                layer.upper.depth_usda_cm = 0), paths[["site"]])

  entry <- library_registry()$kssl
  entry$name    <- "mini"
  entry$version <- "v0"
  entry$filters$wn_max <- 700L
  entry$sources <- data.frame(table = names(paths), file = basename(paths),
                              url = paste0("file://", normalizePath(paths)),
                              bytes = unname(file.size(paths)), md5 = unname(tools::md5sum(paths)),
                              stringsAsFactors = FALSE)
  entry

}

use_mini <- function(env = parent.frame()) {

  entry <- mini_entry(withr::local_tempdir(.local_envir = env))
  cache <- withr::local_tempdir(.local_envir = env)
  withr::local_options(horizons.cache_dir = cache, horizons.library_download = TRUE,
                       .local_envir = env)
  local_mocked_bindings(library_registry = function() list(mini = entry), .env = env)
  pool <- suppressMessages(resolve_source("mini", verbose = FALSE))$pool
  list(cache = cache, pool = pool)

}

## Targets: library rows plus noise, on the library's grid, or resampled.
mini_targets <- function(pool, n = 5L, wn = NULL) {

  a    <- pool$data$analysis
  cols <- grep("^wn_", names(a))
  sub  <- a[seq_len(n), c("sample_id", names(a)[cols])]
  sub[, -1] <- sub[, -1] + matrix(stats::rnorm(n * length(cols), sd = 0.002), n)
  sub$sample_id <- paste0("T", seq_len(n))

  if (!is.null(wn)) {
    old <- as.numeric(sub("^wn_", "", names(sub)[-1]))
    m   <- t(apply(as.matrix(sub[, -1]), 1, function(y) stats::approx(old, y, xout = wn)$y))
    colnames(m) <- paste0("wn_", wn)
    sub <- data.frame(sample_id = sub$sample_id, m, check.names = FALSE)
  }

  spectra(sub, id_col = "sample_id")

}

space_files <- function(cache) list.files(cache, pattern = "\\.space-", all.files = TRUE)


## ---------------------------------------------------------------------------
## The cache
## ---------------------------------------------------------------------------

test_that("the default space is built at first use, and a default draw reads it", {

  m <- use_mini()
  expect_length(space_files(m$cache), 1L)

  x   <- mini_targets(m$pool)
  out <- suppressWarnings(select_training(x, "mini", k = 5L, verbose = FALSE))

  expect_true(out$selection$search$cache$used)
  expect_true(out$selection$search$cache$hit)
  expect_length(space_files(m$cache), 1L)

})

test_that("a cached space draws exactly what a fresh one does", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  cached <- suppressWarnings(select_training(x, "mini", k = 5L, verbose = FALSE))
  fresh  <- suppressWarnings(select_training(x, m$pool, k = 5L, verbose = FALSE))

  expect_true(cached$selection$search$cache$hit)
  expect_false(fresh$selection$search$cache$used)
  expect_identical(cached$data$analysis$sample_id, fresh$data$analysis$sample_id)
  expect_equal(cached$selection$membership$distance, fresh$selection$membership$distance, tolerance = 1e-10)

})

test_that("changing a lever misses the cache and keeps a second space", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  out <- suppressWarnings(select_training(x, "mini", k = 5L, window = 20, verbose = FALSE))

  expect_false(out$selection$search$cache$hit)
  expect_length(space_files(m$cache), 2L)

  again <- suppressWarnings(select_training(x, "mini", k = 5L, window = 20, verbose = FALSE))
  expect_true(again$selection$search$cache$hit)

})

test_that("a pool in memory is not cached, a library file is", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  obj <- suppressWarnings(select_training(x, m$pool, k = 5L, verbose = FALSE))
  expect_false(obj$selection$search$cache$used)
  expect_identical(obj$selection$search$cache$reason, "pool passed in memory")
  expect_length(space_files(m$cache), 1L)

  path <- file.path(withr::local_tempdir(), "mine.qs2")
  qs2::qs_save(m$pool, path)

  first  <- suppressWarnings(select_training(x, path, k = 5L, verbose = FALSE))
  second <- suppressWarnings(select_training(x, path, k = 5L, verbose = FALSE))
  expect_true(first$selection$search$cache$used)
  expect_false(first$selection$search$cache$hit)
  expect_true(second$selection$search$cache$hit)
  expect_true(any(grepl("^mine\\.space-", space_files(m$cache))))

})

test_that("a PLS space is never cached", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  out <- suppressWarnings(select_training(x, "mini", k = 5L, space = "pls", ncomp = 2L,
                                          properties = "clay", verbose = FALSE))
  expect_false(out$selection$search$cache$used)
  expect_identical(out$selection$search$cache$reason, "pls space")

})


## ---------------------------------------------------------------------------
## The search axis and its three tiers
## ---------------------------------------------------------------------------

test_that("targets on another grid are searched on the library's and returned on their own", {

  m <- use_mini()
  x <- mini_targets(m$pool, wn = seq(700, 600, by = -4))

  out <- suppressWarnings(select_training(x, "mini", k = 5L, verbose = FALSE))

  expect_identical(out$selection$search$mode, "full")
  expect_identical(out$selection$search$operation, "resampled")
  expect_true(out$selection$search$cache$hit)

  ## The training set is on the targets' 4 cm-1 grid
  wn_out <- as.numeric(sub("^wn_", "", grep("^wn_", names(out$data$analysis), value = TRUE)))
  expect_identical(sort(wn_out), sort(seq(600, 700, by = 4)))
  expect_identical(out$selection$reconciliation$operation, "resampled")

})

test_that("targets short of the library by up to 50 cm-1 search the overlap, uncached, and warn", {

  m <- use_mini()
  x <- mini_targets(m$pool, wn = seq(700, 630, by = -2))

  expect_warning(
    out <- select_training(x, "mini", k = 5L, verbose = FALSE),
    "short of the library's range", class = "horizons_select_warning"
  )

  expect_identical(out$selection$search$mode, "overlap")
  expect_equal(out$selection$search$missing[["low"]], 30)
  expect_false(out$selection$search$cache$used)
  expect_length(space_files(m$cache), 1L)

})

test_that("targets short by more than 50 cm-1 are refused, naming the end", {

  m <- use_mini()
  x <- mini_targets(m$pool, wn = seq(700, 660, by = -2))

  expect_error(select_training(x, "mini", k = 5L, verbose = FALSE),
               class = "horizons_input_error", regexp = "low end")

})

test_that("an unreadable or foreign cache file is rebuilt, not trusted", {

  m <- use_mini()
  f <- file.path(m$cache, space_files(m$cache))
  writeLines("not a space", f)

  x   <- mini_targets(m$pool)
  out <- suppressWarnings(select_training(x, "mini", k = 5L, verbose = FALSE))

  expect_false(out$selection$search$cache$hit)
  expect_s3_class(qs2::qs_read(f), "horizons_space_cache")

})
