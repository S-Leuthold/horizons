# tests/testthat/test-select-cache.R
# The flip and the cached similarity space: distances measured in the
# library's own space on its grid, the space cached beside a registered
# library or a library file, and the two coverage tiers.
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

  cached <- suppressWarnings(select_training(x, "mini", k = 5L))
  expect_true(cached$selection$search$cache$used)
  expect_length(space_files(m$cache), 1L)

  fresh  <- suppressWarnings(select_training(x, m$pool, k = 5L))

  expect_true(cached$selection$search$cache$hit)
  expect_identical(cached$selection$library$build, 2L)
  expect_false(fresh$selection$search$cache$used)
  expect_identical(cached$data$analysis$sample_id, fresh$data$analysis$sample_id)
  expect_equal(cached$selection$membership$distance, fresh$selection$membership$distance, tolerance = 1e-10)

})

test_that("changing a lever misses the cache and keeps a second space", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  out <- suppressWarnings(select_training(x, "mini", k = 5L, window = 20))

  expect_false(out$selection$search$cache$hit)
  expect_length(space_files(m$cache), 2L)

  again <- suppressWarnings(select_training(x, "mini", k = 5L, window = 20))
  expect_true(again$selection$search$cache$hit)

})

test_that("a pool in memory is not cached, a library file is", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  obj <- suppressWarnings(select_training(x, m$pool, k = 5L))
  expect_false(obj$selection$search$cache$used)
  expect_false(obj$selection$search$cache$hit)
  expect_identical(obj$selection$search$cache$reason, "pool passed in memory")
  expect_length(space_files(m$cache), 1L)

  path <- file.path(withr::local_tempdir(), "mine.qs2")
  qs2::qs_save(m$pool, path)

  first  <- suppressWarnings(select_training(x, path, k = 5L))
  second <- suppressWarnings(select_training(x, path, k = 5L))
  expect_true(first$selection$search$cache$used)
  expect_false(first$selection$search$cache$hit)
  expect_true(second$selection$search$cache$hit)
  expect_true(any(grepl("^mine\\.space-", space_files(m$cache))))

})

test_that("a PLS space is never cached", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  out <- suppressWarnings(select_training(x, "mini", k = 5L, space = "pls", ncomp = 2L,
                                          properties = "clay"))
  expect_false(out$selection$search$cache$used)
  expect_identical(out$selection$search$cache$reason, "pls space")

})


## ---------------------------------------------------------------------------
## The search axis and its two tiers
## ---------------------------------------------------------------------------

test_that("targets on another grid are searched on the library's and returned on the library's columns at theirs", {

  m <- use_mini()
  x <- mini_targets(m$pool, wn = seq(700, 600, by = -4))

  out <- suppressWarnings(select_training(x, "mini", k = 5L))

  expect_identical(out$selection$search$mode, "full")
  expect_identical(out$selection$search$operation, "resampled")
  expect_true(out$selection$search$cache$hit)

  ## The training set is on the targets' 4 cm-1 grid
  wn_out <- as.numeric(sub("^wn_", "", grep("^wn_", names(out$data$analysis), value = TRUE)))
  expect_identical(sort(wn_out), sort(seq(600, 700, by = 4)))
  expect_identical(out$selection$reconciliation$operation, "subset")

})

test_that("targets that share no column with a registered library are refused before it is downloaded (#236)", {

  ## Registered, not built: resolving it would download and build it
  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 60L)
  local_mini_registry(entry)
  local_mocked_bindings(resolve_source = function(...) stop("the library was resolved"))

  ## An instrument's own axis over the mini library's 600 to 700 cm-1
  wn  <- rev(600.1431 + 1.928571 * (0:51))
  tbl <- data.frame(sample_id = paste0("T", 1:3),
                    matrix(stats::runif(3 * length(wn)), 3, dimnames = list(NULL, paste0("wn_", wn))),
                    check.names = FALSE)
  utils::capture.output(x <- spectra(tbl, id_col = "sample_id"))

  err <- expect_error(select_training(x, "mini", k = 5L), class = "horizons_error_input")
  expect_match(gsub("\\s+", " ", conditionMessage(err)),
               "standardize(x, resample = 2, trim = c(600, 700))", fixed = TRUE)

})

test_that("targets that coincide with a registered library at a column, on no aligned grid, are refused before it is downloaded (#236)", {

  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 60L)
  local_mini_registry(entry)
  local_mocked_bindings(resolve_source = function(...) stop("the library was resolved"))

  ## 600 + 1.93 k shares one column, 600, with the mini library's 2 cm-1 grid
  wn  <- rev(600 + 1.93 * (0:51))
  tbl <- data.frame(sample_id = paste0("T", 1:3),
                    matrix(stats::runif(3 * length(wn)), 3, dimnames = list(NULL, paste0("wn_", wn))),
                    check.names = FALSE)
  utils::capture.output(x <- spectra(tbl, id_col = "sample_id"))

  expect_error(select_training(x, "mini", k = 5L),
               "not on a grid aligned with", class = "horizons_error_input")

})

test_that("baseline-corrected targets are refused against the registered library, which is not corrected (#107)", {

  m <- use_mini()
  x <- mini_targets(m$pool)

  utils::capture.output(x <- standardize(x, resample = 2, trim = c(600, 700), baseline = TRUE))

  err <- expect_error(select_training(x, "mini", k = 5L), class = "horizons_error_input")
  msg <- gsub("\\s+", " ", conditionMessage(err))

  expect_match(msg, "The targets were baseline-corrected", fixed = TRUE)
  expect_identical(m$pool$data$standardization$baseline, FALSE)

  ## A registered library is not the user's to standardize again
  expect_false(grepl("Or correct the library", msg, fixed = TRUE))

})

test_that("the registry's grid and baseline are the built library's", {

  ## What the checks before the download compare against is what the build
  ## produces: build_ossl_library() standardizes onto exactly this grid,
  ## with this baseline
  m <- use_mini()
  f <- library_registry()$mini$filters

  expect_identical(paste0("wn_", canonical_grid(f$wn_min, f$wn_max, f$wn_step)),
                   m$pool$data$role_map$variable[m$pool$data$role_map$role == "predictor"])
  expect_identical(m$pool$data$standardization$baseline, f$baseline)
  expect_false(f$baseline)

})

test_that("baseline-corrected targets are refused against a registered library before it is downloaded (#107)", {

  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 60L)
  local_mini_registry(entry)
  local_mocked_bindings(resolve_source = function(...) stop("the library was resolved"))

  ## On the mini library's grid, so the column checks pass and only the
  ## baseline differs
  wn  <- seq(700, 600, by = -2)
  tbl <- withr::with_seed(1, data.frame(sample_id = paste0("T", 1:3),
                                        matrix(1 + stats::runif(3 * length(wn)), 3,
                                               dimnames = list(NULL, paste0("wn_", wn))),
                                        check.names = FALSE))
  utils::capture.output({
    x <- spectra(tbl, id_col = "sample_id")
    x <- standardize(x, resample = 2, trim = c(600, 700), baseline = TRUE)
  })

  expect_error(select_training(x, "mini", k = 5L),
               "The targets were baseline-corrected", class = "horizons_error_input")

})

test_that("targets short of the library's range search the overlap, uncached, and warn coverage", {

  m <- use_mini()
  x <- mini_targets(m$pool, wn = seq(700, 630, by = -2))

  expect_warning(
    out <- select_training(x, "mini", k = 5L),
    "short of the library's range", class = "horizons_warning_coverage"
  )

  expect_identical(out$selection$search$mode, "overlap")
  expect_equal(out$selection$search$missing[["low"]], 30)
  expect_false(out$selection$search$cache$used)
  expect_identical(out$selection$search$cache$reason, "overlap space")
  expect_length(space_files(m$cache), 1L)

})

test_that("targets short by more than 50 cm-1 are searched on the overlap too, not refused", {

  ## 60 cm-1 short at the low end, which was refused until the search took
  ## any overlap: the warning names the shortfall and what was searched. The
  ## overlap is 21 columns, so the window is narrowed to fit inside it.
  m <- use_mini()
  x <- mini_targets(m$pool, wn = seq(700, 660, by = -2))

  w <- expect_warning(out <- select_training(x, "mini", k = 5L, window = 20),
                      class = "horizons_warning_coverage")

  expect_match(conditionMessage(w), "Short by 60 cm-1 at the low end", fixed = TRUE)
  expect_match(conditionMessage(w), "from 660 to 700 cm-1", fixed = TRUE)
  expect_identical(out$selection$search$mode, "overlap")
  expect_equal(out$selection$search$missing[["low"]], 60)

})

test_that("an unreadable or foreign cache file is rebuilt, not trusted, with a note", {

  m <- use_mini()
  f <- file.path(m$cache, space_files(m$cache))
  writeLines("not a space", f)

  x   <- mini_targets(m$pool)
  txt <- utils::capture.output(
    out <- suppressWarnings(select_training(x, "mini", k = 5L, control = control_horizons(verbose = TRUE)))
  )

  expect_false(out$selection$search$cache$hit)
  expect_s3_class(qs2::qs_read(f), "horizons_space_cache")
  expect_true(any(grepl("The cached similarity space could not be used; rebuilding it", txt, fixed = TRUE)))

  notes <- out$log[out$log$verb == "select_training" & out$log$type == "note", ]
  expect_identical(nrow(notes), 1L)
  expect_match(notes$message, "could not be read or was built for other settings, so it was rebuilt", fixed = TRUE)

})

test_that("a space that cannot be cached is still returned, with a skipped warning on the collector", {

  ## Arrange: a cache directory under a regular file cannot be created by any
  ## user, so the write fails whatever the permissions. cached_space() stores
  ## whatever build() returns, so a stand-in keeps the test off a PCA.
  blocker <- withr::local_tempfile()
  file.create(blocker)
  withr::local_options(horizons.cache_dir = file.path(blocker, "cache"))

  space     <- structure(list(), class = "horizons_similarity_space")
  settings  <- list(snv = TRUE, derivative = 1L, window = 21L, poly = 2L, mask = NULL,
                    ncomp = 0.99, sdev_floor = 0.1)
  collector <- log_open("select_training")

  ## Act: nothing is raised here; the verb raises what its collector holds
  expect_no_warning(
    out <- cached_space(structure("lib", label = "lib"), settings, seq(700, 600, by = -2),
                        build = function() space, verbose = FALSE, collector = collector)
  )

  ## Assert
  expect_identical(out$space, space)
  expect_false(out$hit)
  expect_null(out$path)

  rows <- log_rows(collector)
  expect_identical(rows$topic, "skipped")
  expect_match(rows$message, "The similarity space was built but could not be cached", fixed = TRUE)
  expect_match(rows$message, "options(horizons.cache_dir = )", fixed = TRUE)

})

test_that("a space is cached into a directory that does not exist yet, and rewritten over one that does", {

  ## cached_space() stores whatever build() returns, so a stand-in keeps the
  ## test off a PCA; the cache directory is two levels below a fresh one
  withr::local_options(horizons.cache_dir = file.path(withr::local_tempdir(), "a", "b"))

  space    <- structure(list(id = 1), class = "horizons_similarity_space")
  settings <- list(snv = TRUE, derivative = 1L, window = 21L, poly = 2L, mask = NULL,
                   ncomp = 0.99, sdev_floor = 0.1)
  wn       <- seq(700, 600, by = -2)
  lib      <- structure("lib", label = "lib")

  expect_output(
    first <- cached_space(lib, settings, wn, build = function() space, verbose = TRUE),
    "Building the similarity space on the whole library", fixed = TRUE
  )
  expect_true(file.exists(first$path))
  expect_identical(qs2::qs_read(first$path)$space, space)

  ## The directory exists now; a forced rebuild writes over the file, quietly
  space2 <- structure(list(id = 2), class = "horizons_similarity_space")
  expect_silent(
    again <- cached_space(lib, settings, wn, build = function() space2,
                          verbose = FALSE, force = TRUE)
  )
  expect_identical(again$path, first$path)
  expect_identical(qs2::qs_read(again$path)$space, space2)

})

test_that("a cache file that holds another key is rebuilt over, not trusted", {

  withr::local_options(horizons.cache_dir = withr::local_tempdir())

  settings <- list(snv = TRUE, derivative = 1L, window = 21L, poly = 2L, mask = NULL,
                   ncomp = 0.99, sdev_floor = 0.1)
  wn       <- seq(700, 600, by = -2)
  lib      <- structure("lib", label = "lib")
  stale    <- structure(list(id = "stale"), class = "horizons_similarity_space")
  fresh    <- structure(list(id = "fresh"), class = "horizons_similarity_space")

  ## A well-formed cache file at this key's path, holding a space built
  ## under another key
  first <- cached_space(lib, settings, wn, build = function() stale, verbose = FALSE)
  obj   <- qs2::qs_read(first$path)
  obj$key <- "another key"
  qs2::qs_save(obj, first$path)

  out <- cached_space(lib, settings, wn, build = function() fresh, verbose = FALSE)

  expect_false(out$hit)
  expect_identical(out$space, fresh)

})

test_that("a library narrower than the default window is cached, and its space left to a draw that fits", {

  ## Arrange: a library from 670 to 700 cm-1 has 16 columns, fewer than the
  ## default window's 21 points at 2 cm-1, which prospectr cannot filter.
  ## The first-use space is not tried, so prospectr is never reached.
  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 30L)
  entry$filters$wn_min <- 670L
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  local_mocked_bindings(savitzkyGolay = function(...) stop("the filter was run"), .package = "prospectr")

  collector <- log_open("select_training")

  ## Act
  expect_no_warning(
    txt <- utils::capture.output(
      res <- suppressMessages(resolve_source("mini", verbose = TRUE, collector = collector))
    )
  )
  expect_true(any(grepl("narrower than the default window, so its similarity space is built at the first draw", txt,
                        fixed = TRUE)))

  ## Assert: the library is built and cached, no space was written, and the
  ## note says why
  expect_s3_class(res$pool, "horizons_data")
  expect_true(file.exists(file.path(cache, "mini_v0.qs2")))
  expect_length(space_files(cache), 0L)

  rows <- log_rows(collector)
  expect_identical(rows$type, "note")
  expect_match(rows$message, "the default `window` of 40 cm-1 is 21 points on its 16 columns, and the filter needs fewer", fixed = TRUE)

})


test_that("the first-use space leaves out the library's flagged rows, as a draw's does", {

  ## Arrange: a release whose recipe flags one row, as a step that could not
  ## use it would
  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 60L)
  entry$recipe <- function(raw, entry, verbose = TRUE) {
    pool <- build_ossl_library(raw, entry, verbose = verbose)
    pool$data$analysis$.flag <- c("standardize", rep(NA_character_, nrow(pool$data$analysis) - 1L))
    pool$data$role_map <- rbind(pool$data$role_map, tibble::tibble(variable = ".flag", role = "meta"))
    pool
  }
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  ## Act: the first use builds the library and its space
  pool <- suppressMessages(resolve_source("mini", verbose = FALSE))$pool

  ## Assert: the cached space holds every row but the flagged one
  files <- space_files(cache)
  expect_length(files, 1L)
  expect_identical(rownames(qs2::qs_read(file.path(cache, files))$space$scores),
                   pool$data$analysis$sample_id[-1])

  ## And a default draw reads it as it is, with no note
  out <- suppressWarnings(select_training(mini_targets(pool), "mini", k = 5L))

  expect_true(out$selection$search$cache$hit)
  expect_false(any(out$log$type[out$log$verb == "select_training"] == "note"))

})


test_that("a first-use space that fails to build is a note, and stops the call under debug", {

  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 60L)
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  x <- mini_targets(build_ossl_library(
    stats::setNames(sub("^file://", "", entry$sources$url), entry$sources$table), entry, verbose = FALSE))

  ## The first build fails, once: the prime's; the draw's own build works
  calls <- 0L
  real  <- build_similarity_space
  local_mocked_bindings(build_similarity_space = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("out of memory {in the prime}")
    real(...)
  })

  txt <- utils::capture.output(
    out <- suppressWarnings(suppressMessages(select_training(x, "mini", k = 5L,
                                                             control = control_horizons(verbose = TRUE))))
  )
  notes <- out$log[out$log$verb == "select_training" & out$log$type == "note", ]

  expect_true(any(grepl("could not be built now; the first draw will build it", txt, fixed = TRUE)))

  expect_identical(nrow(notes), 1L)
  expect_match(notes$message, "similarity space could not be built at first use; the first draw builds it", fixed = TRUE)
  expect_match(notes$message, "out of memory {in the prime}", fixed = TRUE)
  expect_false(any(out$log$type[out$log$verb == "select_training"] == "warning" &
                     grepl("first use", out$log$message[out$log$verb == "select_training"])))

  ## With debug on, nothing is contained: the prime's failure stops the call
  file.remove(list.files(cache, full.names = TRUE))
  calls <- 0L

  expect_error(suppressMessages(select_training(x, "mini", k = 5L, control = control_horizons(debug = TRUE))),
               "out of memory {in the prime}", fixed = TRUE)

})


test_that("a library built but not moved into the cache is returned, with one skipped warning, and leaves the cache empty", {

  ## The rename after a successful write fails only on a locked or network
  ## filesystem; here every rename fails, the library's after it was written
  ## to its temporary name in the cache. The draw goes on with the library
  ## built in memory, and says every later draw builds it again.
  entry <- make_mini_ossl(withr::local_tempdir(), seed = 3, extras = FALSE, n_kssl = 60L)
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  x <- mini_targets(build_ossl_library(
    stats::setNames(sub("^file://", "", entry$sources$url), entry$sources$table), entry, verbose = FALSE))

  written <- NULL
  local_mocked_bindings(
    file.rename = function(from, to) {
      written <<- c(written, file.exists(from))
      FALSE
    },
    .package = "base"
  )

  txt <- utils::capture.output(
    run <- collect_warnings(
      suppressMessages(select_training(x, "mini", k = 5L, control = control_horizons(verbose = TRUE))),
      class = "horizons_warning_skipped"
    )
  )
  expect_true(any(grepl("mini v0 built, but not cached", cli::ansi_strip(txt), fixed = TRUE)))

  out     <- run$value
  skipped <- run$warnings
  expect_length(skipped, 1L)

  msg <- gsub("\\s+", " ", conditionMessage(skipped[[1]]))
  expect_match(msg, "library was built but could not be moved into the cache, so every later draw downloads and builds it again", fixed = TRUE)
  expect_match(msg, "check that", fixed = TRUE)

  ## The record says it is not cached, and no space was written either
  expect_s3_class(out, "horizons_data")
  expect_true("path" %in% names(out$selection$library))
  expect_null(out$selection$library$path)

  ## The library was on disk under its temporary name when the rename
  ## failed; the build removes it and the raw downloads rather than leaving
  ## them in the cache
  expect_true(written[1])
  expect_true(dir.exists(cache))
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)

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
    select_training(x, "mini", k = 5L),
    warning = function(w) { warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning") }
  )

  expect_false(any(grepl("short of the library's range", warned)))
  expect_identical(out$selection$search$mode, "full")
  expect_true(out$selection$search$cache$hit)

})

test_that("the cache key carries the space algorithm's version and tells masks apart", {

  s  <- list(snv = TRUE, derivative = 1L, window = 21L, poly = 2L, mask = NULL,
             ncomp = 0.99, sdev_floor = 0.1)
  k1 <- space_cache_key("lib", s, 600:700)

  ## Two masks of the same shape are two spaces, and a mask in whole numbers
  ## is the same mask in doubles
  key <- function(mask) space_cache_key("lib", utils::modifyList(s, list(mask = mask)), 600:700)

  expect_false(identical(key(rbind(c(620, 640))), key(rbind(c(650, 680)))))
  expect_false(identical(key(rbind(c(620, 640))), k1))
  expect_identical(key(rbind(c(620L, 640L))), key(rbind(c(620, 640))))

  local_mocked_bindings(SELECT_SPACE_CACHE_VERSION = SELECT_SPACE_CACHE_VERSION + 1L)
  expect_false(identical(space_cache_key("lib", s, 600:700), k1))

})

test_that("a cached space whose rows are not the library's is rebuilt over, with a note", {

  m <- use_mini()
  f <- file.path(m$cache, space_files(m$cache))

  obj <- qs2::qs_read(f)
  rownames(obj$space$scores) <- rev(rownames(obj$space$scores))
  qs2::qs_save(obj, f)

  ## A rebuild costs only time, once: a note on the log and a line in the
  ## tree, not a warning
  x   <- mini_targets(m$pool)
  txt <- utils::capture.output(
    out <- suppressWarnings(select_training(x, "mini", k = 5L, control = control_horizons(verbose = TRUE)))
  )
  expect_true(any(grepl("did not match this library's rows; rebuilding it", txt, fixed = TRUE)))

  notes <- out$log[out$log$verb == "select_training" & out$log$type == "note", ]
  expect_identical(nrow(notes), 1L)
  expect_match(notes$message, "did not match this library's rows, so it was rebuilt over", fixed = TRUE)
  expect_false(any(grepl("rebuilt", out$log$message[out$log$type == "warning"])))

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
