# tests/testthat/test-select-cache.R
# The flip and the cached similarity space: distances measured in the
# library's own space on its grid, the space cached beside a registered
# library or a library file, and the three coverage tiers.
#
# The miniature library comes from helper-mini-library.R.


## ---------------------------------------------------------------------------
## Fixture: a registered mini library, built, and targets on its grid
## ---------------------------------------------------------------------------

use_mini <- function(env = parent.frame()) {

  entry <- make_mini_ossl(withr::local_tempdir(.local_envir = env), seed = 3,
                          extras = FALSE, n_kssl = 60L)
  cache <- local_mini_registry(entry, env = env)
  withr::local_options(horizons.library_download = TRUE, .local_envir = env)
  pool  <- suppressMessages(resolve_source("mini", verbose = FALSE))$pool
  list(cache = cache, pool = pool)

}

## Targets: library rows plus noise, on the library's grid, or resampled.
mini_targets <- function(pool, n = 5L, wn = NULL, seed = 7) {

  withr::local_seed(seed)

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

test_that("a cached space draws exactly what a fresh one does", {

  ## The default space is built at first use, and a default draw reads it
  m <- use_mini()
  expect_length(space_files(m$cache), 1L)

  x <- mini_targets(m$pool)

  cached <- suppressWarnings(select_training(x, "mini", k = 5L, verbose = FALSE))
  expect_true(cached$selection$search$cache$used)
  expect_length(space_files(m$cache), 1L)

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

test_that("a space that cannot be cached is still returned, with a warning", {

  ## Arrange: a cache directory under a regular file cannot be created by any
  ## user, so the write fails whatever the permissions. cached_space() stores
  ## whatever build() returns, so a stand-in keeps the test off a PCA.
  blocker <- withr::local_tempfile()
  file.create(blocker)
  withr::local_options(horizons.cache_dir = file.path(blocker, "cache"))

  space    <- structure(list(), class = "horizons_similarity_space")
  settings <- list(snv = TRUE, derivative = 1L, window = 21L, poly = 2L, mask = NULL,
                   ncomp = 0.99, sdev_floor = 0.1)

  ## Act
  expect_warning(
    out <- cached_space(structure("lib", label = "lib"), settings, seq(700, 600, by = -2),
                        build = function() space, verbose = FALSE),
    "The similarity space was built but could not be cached", fixed = TRUE,
    class = "horizons_select_warning"
  )

  ## Assert
  expect_identical(out$space, space)
  expect_false(out$hit)
  expect_null(out$path)

})

test_that("a library whose default space cannot be built is still cached, with a warning", {

  ## Arrange: a library from 670 to 700 cm-1 has 16 columns, fewer than the
  ## default space's 21-point window at 2 cm-1, so the space built at first
  ## use fails.
  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 30L)
  entry$filters$wn_min <- 670L
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  ## Act
  w <- expect_warning(
    res <- suppressMessages(resolve_source("mini", verbose = FALSE)),
    "similarity space could not be built now", fixed = TRUE,
    class = "horizons_select_warning"
  )

  ## Assert: the build failed for the reason arranged, not another; the
  ## library is built and cached; its space is left to the first draw
  expect_match(conditionMessage(w), "is wider than the 16 columns of the spectra", fixed = TRUE)
  expect_s3_class(res$pool, "horizons_data")
  expect_true(file.exists(file.path(cache, "mini_v0.qs2")))
  expect_length(space_files(cache), 0L)

})


## ---------------------------------------------------------------------------
## Review fixes (2026-09-30)
## ---------------------------------------------------------------------------

test_that("a batch on a coarse canonical grid counts as full coverage", {

  ## standardize()'s 16 cm-1 grid inside 600-700 runs 608 to 688: 8 short at
  ## the low end, 12 at the high, both under one 16 cm-1 step.

  m <- use_mini()
  capture.output(x <- standardize(mini_targets(m$pool), resample = 16, trim = c(600, 700)))

  warned <- character()
  out <- withCallingHandlers(
    select_training(x, "mini", k = 5L, verbose = FALSE),
    warning = function(w) { warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning") }
  )

  expect_false(any(grepl("short of the library's range", warned)))
  expect_identical(out$selection$search$mode, "full")
  expect_true(out$selection$search$cache$hit)

})

test_that("the cache key carries the space algorithm's version", {

  s  <- list(snv = TRUE, derivative = 1L, window = 21L, poly = 2L, mask = NULL,
             ncomp = 0.99, sdev_floor = 0.1)
  k1 <- space_cache_key("lib", s, 600:700)

  local_mocked_bindings(SELECT_SPACE_CACHE_VERSION = SELECT_SPACE_CACHE_VERSION + 1L)
  expect_false(identical(space_cache_key("lib", s, 600:700), k1))

})

test_that("a cached space whose rows are not the library's is rebuilt over, with a warning", {

  m <- use_mini()
  f <- file.path(m$cache, space_files(m$cache))

  obj <- qs2::qs_read(f)
  rownames(obj$space$scores) <- rev(rownames(obj$space$scores))
  qs2::qs_save(obj, f)

  x <- mini_targets(m$pool)
  expect_warning(select_training(x, "mini", k = 5L, verbose = FALSE),
                 "does not match this library's rows", class = "horizons_select_warning")

  fixed <- qs2::qs_read(f)
  expect_identical(rownames(fixed$space$scores), m$pool$data$analysis$sample_id)

})

test_that("rebuilding a registered library sweeps the spaces cached for the old build", {

  m <- use_mini()

  ## A stray space from an earlier build of the same library
  stray <- file.path(m$cache, "mini_v0.space-0000000000000000.qs2")
  file.create(stray)
  file.remove(file.path(m$cache, "mini_v0.qs2"))

  rebuilt <- suppressMessages(resolve_source("mini", verbose = FALSE))

  expect_false(file.exists(stray))
  files <- space_files(m$cache)
  expect_length(files, 1L)
  expect_identical(qs2::qs_read(file.path(m$cache, files))$identity,
                   as.character(space_cache_identity(rebuilt$record)))

})
