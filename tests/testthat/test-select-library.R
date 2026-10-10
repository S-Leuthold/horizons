# tests/testthat/test-select-library.R
# resolve_source(): the three forms of `library =`, and the registered path
# end to end against a miniature OSSL written to disk and served over
# file:// URLs, so nothing touches the network or the user's cache.


## ---------------------------------------------------------------------------
## The three forms
## ---------------------------------------------------------------------------

test_that("a horizons_data passes through as the object form", {

  fx  <- select_fixture()
  res <- resolve_source(fx$pool)

  expect_identical(res$pool, fx$pool)
  expect_identical(res$record$form, "object")
  expect_null(res$record$name)

})

test_that("a bare word that is not registered says so rather than 'file not found'", {

  expect_error(resolve_source("ksl"), class = "horizons_input_error", regexp = "not a registered library")

})

test_that("a path that does not exist stops with the path", {

  expect_error(resolve_source(file.path(tempdir(), "nope.qs2")),
               class = "horizons_input_error", regexp = "not found")

})

test_that("anything else is refused with the forms listed", {

  expect_error(resolve_source(42), class = "horizons_input_error", regexp = "registered library")
  expect_error(resolve_source(c("a", "b")), class = "horizons_input_error")

})

test_that("a saved horizons_data reads back through the path form", {

  fx   <- select_fixture()
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(fx$pool, path)

  res <- resolve_source(path)

  expect_identical(res$record$form, "path")
  expect_equal(res$pool$data$analysis, fx$pool$data$analysis)

})

test_that("a qs2 file holding something else is not a library", {

  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(list(a = 1), path)

  expect_error(resolve_source(path), class = "horizons_input_error", regexp = "not a horizons library")

})


## ---------------------------------------------------------------------------
## Registered libraries
## ---------------------------------------------------------------------------

test_that("without consent a non-interactive session downloads nothing", {

  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = NULL)

  expect_error(suppressMessages(resolve_source("mini", ask = FALSE)),
               class = "horizons_consent_error", regexp = "horizons.library_download")
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)

})

test_that("the credit is printed before any download", {

  entry <- make_mini_ossl(withr::local_tempdir())
  local_mini_registry(entry)
  withr::local_options(horizons.library_download = NULL)

  expect_message(try(resolve_source("mini", ask = FALSE), silent = TRUE),
                 regexp = "Soil Spectroscopy for Global Good")

})

test_that("first use builds and caches; later uses read the cache", {

  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  res <- suppressMessages(resolve_source("mini", verbose = FALSE))

  expect_true(file.exists(file.path(cache, "mini_v0.qs2")))
  ## The library and its default similarity space, and nothing left over
  files <- list.files(cache, all.files = TRUE, no.. = TRUE)
  expect_setequal(sub("space-[0-9a-f]+", "space-KEY", files), c("mini_v0.qs2", "mini_v0.space-KEY.qs2"))
  expect_identical(res$record$form, "registered")
  expect_identical(res$record$name, "mini")
  expect_identical(res$record$license, "CC-BY-4.0")

  ## The cache, not the network: a download now would fail.

  local_mocked_bindings(download_sources = function(...) stop("downloaded again"))
  again <- resolve_source("mini", verbose = FALSE)

  expect_equal(again$pool$data$analysis, res$pool$data$analysis)

})

test_that("the recipe keeps every depth and filters dataset, instrument and completeness", {

  entry <- make_mini_ossl(withr::local_tempdir())
  local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  pool <- suppressMessages(resolve_source("mini", verbose = FALSE))$pool
  a    <- pool$data$analysis
  rm   <- pool$data$role_map

  expect_setequal(a$sample_id, sprintf("id%02d", 1:8))
  expect_setequal(a$upper_depth_cm, c(0, 10, 20, 30, 45, 60, 0, 25))
  expect_identical(rm$role[rm$variable == "upper_depth_cm"], "meta")
  expect_setequal(rm$variable[rm$role == "response"], c("clay", "oc"))
  expect_length(grep("^wn_", names(a)), 51L)

})

test_that("a source that fails its MD5 stops and caches nothing", {

  entry <- make_mini_ossl(withr::local_tempdir())
  entry$sources$md5[2] <- "00000000000000000000000000000000"
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  expect_error(suppressMessages(resolve_source("mini", verbose = FALSE)),
               class = "horizons_download_error", regexp = "checksum")
  expect_false(file.exists(file.path(cache, "mini_v0.qs2")))

})

test_that("a library built but not moved into the cache stops, classed, and leaves the cache empty", {

  ## The rename after a successful write fails only on a locked or network
  ## filesystem; a qs_save() that writes nothing makes it fail here for want
  ## of a source file (base R warns that the rename failed).
  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)
  local_mocked_bindings(qs_save = function(...) invisible(NULL), .package = "qs2")

  expect_error(suppressWarnings(suppressMessages(resolve_source("mini", verbose = FALSE))),
               "library was built but could not be moved into the cache", fixed = TRUE,
               class = "horizons_build_error")

  ## The build downloads into a raw directory in the cache; a failed build
  ## removes it rather than leaving the downloads behind.
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)

})

## A mini release whose MIR table has been through `edit`, a function of the
## table as a data frame, with the raw paths build_ossl_library() takes.
mini_release_with_mir <- function(edit, env = parent.frame()) {

  dir   <- withr::local_tempdir(.local_envir = env)
  entry <- make_mini_ossl(dir)
  mir   <- file.path(dir, "mir.csv.gz")

  data.table::fwrite(edit(utils::read.csv(gzfile(mir), check.names = FALSE)), mir)

  list(entry = entry,
       raw   = c(mir  = mir,
                 lab  = file.path(dir, "lab.csv.gz"),
                 site = file.path(dir, "site.csv.gz")))

}

test_that("the recipe refuses a MIR table that lacks a filter column", {

  rel <- mini_release_with_mir(function(mir) mir[, names(mir) != "scan.mir.model.name_utf8_txt"])

  expect_error(build_ossl_library(rel$raw, rel$entry, verbose = FALSE),
               "The MIR table lacks", fixed = TRUE, class = "horizons_build_error")

})

test_that("the recipe refuses a MIR table that is not on the entry's grid", {

  rel <- mini_release_with_mir(function(mir) mir[, names(mir) != "scan_mir.650_abs"])

  expect_error(build_ossl_library(rel$raw, rel$entry, verbose = FALSE),
               "The MIR table's grid is not 600 to 700 by 2 cm-1 (50 columns found)",
               fixed = TRUE, class = "horizons_build_error")

})

test_that("the cache file also resolves through the path form", {

  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)
  suppressMessages(resolve_source("mini", verbose = FALSE))

  res <- resolve_source(file.path(cache, "mini_v0.qs2"))

  expect_identical(res$record$form, "path")
  expect_identical(res$record$name, "mini")

})


## ---------------------------------------------------------------------------
## Through the verb
## ---------------------------------------------------------------------------

test_that("select_training() resolves a library name before anything else", {

  expect_error(select_training(select_fixture()$targets, "ksl", verbose = FALSE),
               class = "horizons_input_error", regexp = "not a registered library")

  ## Two names are refused as a library form, before R's own error for a
  ## condition of length two
  expect_error(select_training(select_fixture()$targets, c("kssl", "kssl"), verbose = FALSE),
               class = "horizons_input_error", regexp = "a single name or path")

})


## ---------------------------------------------------------------------------
## The depth lever
## ---------------------------------------------------------------------------

#' The select fixture's pool with an upper depth on every row: two thirds
#' topsoil, the rest deep, and a few with no recorded depth
#' @noRd
with_depth <- function(fx) {

  n     <- nrow(fx$pool$data$analysis)
  upper <- rep(c(0, 15, 60), length.out = n)
  upper[c(4, 5)] <- NA

  fx$pool$data$analysis$upper_depth_cm <- upper
  fx$pool$data$role_map <- rbind(fx$pool$data$role_map,
                                 tibble::tibble(variable = "upper_depth_cm", role = "meta"))
  fx

}

test_that("topsoil, the default, draws only rows under 30 cm and records it", {

  fx  <- with_depth(select_fixture())
  out <- select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE)

  upper <- fx$pool$data$analysis$upper_depth_cm
  top   <- fx$pool$data$analysis$sample_id[!is.na(upper) & upper < 30]

  expect_true(all(out$data$analysis$sample_id %in% top))
  expect_identical(out$selection$settings$depth, "topsoil")
  expect_true(out$selection$depth$applied)
  expect_identical(out$selection$depth$max_cm, SELECT_TOPSOIL_MAX_CM)
  expect_identical(out$selection$depth$n_eligible, length(top))

})

test_that("depth = 'all' can draw deep rows", {

  fx  <- with_depth(select_fixture())
  out <- select_training(fx$targets, fx$pool, k = 60L, depth = "all", verbose = FALSE)

  upper <- fx$pool$data$analysis$upper_depth_cm[match(out$data$analysis$sample_id,
                                                     fx$pool$data$analysis$sample_id)]

  expect_true(any(upper >= 30, na.rm = TRUE))
  expect_false(out$selection$depth$applied)

})

test_that("a library with no depth column draws from every row and says so", {

  fx  <- select_fixture()
  out <- select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE)

  expect_false(out$selection$depth$recorded)
  expect_false(out$selection$depth$applied)
  expect_identical(out$selection$depth$n_eligible, nrow(fx$pool$data$analysis))

  ## The record says which library the draw came from
  expect_identical(out$selection$library$form, "object")

})

test_that("global under topsoil returns the eligible rows, not the whole pool", {

  fx  <- with_depth(select_fixture())
  out <- select_training(fx$targets, fx$pool, k = 20L, scope = "global", verbose = FALSE)

  expect_identical(nrow(out$data$analysis), out$selection$depth$n_eligible)

})

test_that("depth rejects anything but topsoil or all", {

  fx <- select_fixture()

  expect_error(suppressMessages(capture.output(
    select_training(fx$targets, fx$pool, depth = "subsoil", verbose = FALSE))),
    class = "horizons_input_error", regexp = "depth")

})

test_that("the chunked MIR read returns the chosen rows whatever the chunk size", {

  entry <- make_mini_ossl(withr::local_tempdir())
  gz    <- sub("^file://", "", entry$sources$url[entry$sources$table == "mir"])
  csv   <- decompress_gz(gz)

  hdr  <- names(data.table::fread(csv, nrows = 0))
  full <- as.data.frame(data.table::fread(csv))
  spec <- grep("^scan_mir", hdr, value = TRUE)
  rows <- c(1L, 4L, 5L, 9L, 11L)

  for (chunk in c(1L, 2L, 3L, 5000L)) {

    got <- read_mir_rows(csv, hdr, rows, full$id.layer_uuid_txt[rows],
                         "id.layer_uuid_txt", spec, chunk = chunk)

    expect_equal(unname(got), unname(as.matrix(full[rows, spec])))

  }

})


## ---------------------------------------------------------------------------
## The window in cm-1
## ---------------------------------------------------------------------------

test_that("the default window is 11 points at 4 cm-1 and 21 at 2 cm-1", {

  expect_identical(window_to_points(SELECT_SG_WINDOW_CM, 4), 11L)
  expect_identical(window_to_points(SELECT_SG_WINDOW_CM, 2), 21L)

  ## Nearest odd count, ties up: 40 at 8 cm-1 is 2.5 half-widths, so 7
  expect_identical(window_to_points(40, 8), 7L)
  expect_identical(window_to_points(80, 8), 11L)

})

test_that("a window too narrow for the polynomial stops in cm-1 terms", {

  fx <- select_fixture()

  ## 2 cm-1 on the fixture pool's 4 cm-1 grid rounds to 1 point
  expect_error(select_training(fx$targets, fx$pool, k = 20L, window = 2, verbose = FALSE),
               class = "horizons_input_error", regexp = "too few for a polynomial")
  expect_error(suppressMessages(capture.output(
    select_training(fx$targets, fx$pool, k = 20L, window = -1, verbose = FALSE))),
    class = "horizons_input_error", regexp = "positive width")

})


## ---------------------------------------------------------------------------
## Review fixes (2026-09-30)
## ---------------------------------------------------------------------------

test_that("a horizons_library in memory passes through with its record", {

  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)
  suppressMessages(resolve_source("mini", verbose = FALSE))

  lib <- qs2::qs_read(file.path(cache, "mini_v0.qs2"))
  res <- resolve_source(lib)

  expect_identical(res$record$name, "mini")
  expect_identical(res$record$form, "object")
  expect_equal(res$pool$data$analysis, lib$pool$data$analysis)

})

#' The select fixture with one spectral family entirely subsoil
#' @noRd
with_family_depth <- function(fx, deep_family) {

  fam   <- fx$pool$data$analysis$family
  upper <- ifelse(fam == deep_family, 60, 0)

  fx$pool$data$analysis$upper_depth_cm <- upper
  fx$pool$data$role_map <- rbind(fx$pool$data$role_map,
                                 tibble::tibble(variable = "upper_depth_cm", role = "meta"))
  fx

}

test_that("resemblance is measured against the rows the draw can reach", {

  fx   <- select_fixture()
  deep <- fx$family_of_target[[1]]
  fx   <- with_family_depth(fx, deep)
  from_deep <- names(fx$family_of_target)[fx$family_of_target == deep]

  warned <- list()
  top <- withCallingHandlers(
    select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE),
    warning = function(w) {
      warned[[length(warned) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  all <- suppressWarnings(select_training(fx$targets, fx$pool, k = 20L, depth = "all", verbose = FALSE))

  ## The deep-family targets resemble rows topsoil cannot draw. Measured
  ## against the whole library none of them was named; against the drawable
  ## rows they are (all but a target that happens to sit near the other
  ## family, on this fixture).
  flagged <- intersect(top$selection$resemblance$beyond$target_id, from_deep)
  expect_gte(length(flagged), length(from_deep) - 1L)
  expect_true(all(top$selection$resemblance$beyond$target_id %in% from_deep))
  expect_length(intersect(all$selection$resemblance$beyond$target_id, from_deep), 0L)

  ## The warning says how many targets and names the first five
  beyond   <- top$selection$resemblance$beyond
  beyond_w <- Filter(function(w) grepl("beyond the pool", conditionMessage(w)), warned)

  expect_length(beyond_w, 1L)
  expect_s3_class(beyond_w[[1]], "horizons_select_warning")

  msg <- gsub("\\s+", " ", conditionMessage(beyond_w[[1]]))
  expect_match(msg, sprintf("%d targets sit beyond", nrow(beyond)), fixed = TRUE)
  for (id in utils::head(beyond$target_id, 5)) expect_match(msg, id, fixed = TRUE)

})

test_that("a k beyond the topsoil rows says depth = 'all' would reach more", {

  fx <- with_depth(select_fixture())

  expect_error(select_training(fx$targets, fx$pool, k = 250L, properties = "clay", verbose = FALSE),
               class = "horizons_input_error", regexp = "depth = \"all\"")

})


## ---------------------------------------------------------------------------
## Re-review fixes (2026-09-30)
## ---------------------------------------------------------------------------

test_that("the resemblance check is skipped, not failed, when too few rows can be drawn", {

  fx <- select_fixture(n_pool = 60)
  n  <- nrow(fx$pool$data$analysis)

  fx$pool$data$analysis$upper_depth_cm <- c(0, rep(60, n - 1L))
  fx$pool$data$role_map <- rbind(fx$pool$data$role_map,
                                 tibble::tibble(variable = "upper_depth_cm", role = "meta"))

  out <- suppressWarnings(select_training(fx$targets, fx$pool, k = 5L, scope = "global",
                                          properties = "clay", verbose = FALSE))

  r <- out$selection$resemblance
  expect_true(is.na(r$threshold))
  expect_identical(nrow(r$beyond), 0L)
  expect_match(r$skipped, "only 1 row")

  all <- suppressWarnings(select_training(fx$targets, fx$pool, k = 5L, depth = "all",
                                          properties = "clay", verbose = FALSE))
  expect_null(all$selection$resemblance$skipped)

})

test_that("a bad argument is reported before a registered library is fetched", {

  fx    <- select_fixture()
  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  ## Each case is one mistake, and its refusal must name the argument. A case
  ## may name its own targets; the rest draw for the fixture's targets. depth
  ## is reported even when the targets are wrong too.
  bad <- list(
    list(targets = "not data", args = list(depth = "subsoil"), regexp = "depth"),
    list(targets = "not data", args = list(), regexp = "`x`"),
    ## the verb's choices
    list(args = list(scope = "nope"), regexp = "`scope`"),
    list(args = list(space = "nope"), regexp = "`space`"),
    list(args = list(metric = "nope"), regexp = "`metric`"),
    list(args = list(k = 0), regexp = "`k`"),
    list(args = list(k = numeric(0)), regexp = "`k`"),
    list(args = list(k = TRUE), regexp = "`k`"),
    list(args = list(k = Inf), regexp = "`k`"),
    list(args = list(sdev_floor = FALSE), regexp = "`sdev_floor`"),
    list(args = list(verbose = "yes"), regexp = "`verbose`"),
    list(args = list(clusters = 0), regexp = "`clusters`"),
    list(args = list(cluster_min = TRUE), regexp = "`cluster_min`"),
    ## the space's levers
    list(args = list(snv = "yes"), regexp = "`snv`"),
    list(args = list(derivative = NA), regexp = "`derivative`"),
    list(args = list(poly = -1), regexp = "`poly`"),
    list(args = list(derivative = 2, poly = 1), regexp = "at least `derivative`"),
    list(args = list(window = 1), regexp = "`window`"),
    list(args = list(window = c(40, 40)), regexp = "`window`"),
    list(args = list(mask = "bad"), regexp = "`mask`"),
    list(args = list(mask = c(2200, 2400)), regexp = "`mask`"),
    list(args = list(mask = rbind(c(2400, 2200))), regexp = "`mask`"),
    list(args = list(ncomp = -1), regexp = "`ncomp`"),
    list(args = list(ncomp = TRUE), regexp = "`ncomp`"),
    list(args = list(chunk_size = 0), regexp = "`chunk_size`"),
    ## seed and PLS mistakes
    list(args = list(seed = 2^31), regexp = "`seed`"),
    list(args = list(space = "pls", properties = "clay"), regexp = "whole-number `ncomp`"),
    list(args = list(space = "pls", ncomp = 3L, properties = c("clay", "oc")),
         regexp = "one property")
  )

  for (case in bad) {

    targets <- if (is.null(case$targets)) fx$targets else case$targets
    args    <- utils::modifyList(list(verbose = FALSE), case$args)

    expect_error(suppressMessages(capture.output(
      do.call(select_training, c(list(targets, "mini"), args)))),
      class = "horizons_input_error", regexp = case$regexp, fixed = TRUE,
      info = paste(names(case$args), collapse = ", "))

  }

  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)

})

test_that("a build clears what a killed build left, and only when it is old", {

  entry <- make_mini_ossl(withr::local_tempdir())
  dir   <- withr::local_tempdir()

  old_raw <- file.path(dir, ".raw-mini-v0-dead"); dir.create(old_raw)
  old_tmp <- file.path(dir, ".mini-dead.qs2.tmp"); file.create(old_tmp)
  new_raw <- file.path(dir, ".raw-mini-v0-live"); dir.create(new_raw)
  keep    <- file.path(dir, "mini_v0.qs2");        file.create(keep)

  day_ago <- Sys.time() - 3 * 24 * 3600
  Sys.setFileTime(c(old_raw, old_tmp, keep), day_ago)

  clear_stale_builds(entry, dir)

  expect_false(dir.exists(old_raw))
  expect_false(file.exists(old_tmp))
  expect_true(dir.exists(new_raw))
  expect_true(file.exists(keep))

})


## ---------------------------------------------------------------------------
## Pre-merge fixes (2026-09-30)
## ---------------------------------------------------------------------------

test_that("a skipped resemblance check warns and never reports zero targets out", {

  fx <- select_fixture(n_pool = 60)
  n  <- nrow(fx$pool$data$analysis)
  fx$pool$data$analysis$upper_depth_cm <- c(rep(0, 20), rep(60, n - 20L))
  fx$pool$data$role_map <- rbind(fx$pool$data$role_map,
                                 tibble::tibble(variable = "upper_depth_cm", role = "meta"))

  expect_warning(
    printed <- capture.output(select_training(fx$targets, fx$pool, k = 5L, properties = "clay")),
    "resemblance check did not run", class = "horizons_select_warning"
  )
  expect_true(any(grepl("not checked", printed)))
  expect_false(any(grepl("spread: 0", printed)))

})

test_that("a character depth column is refused rather than compared as text", {

  fx <- with_depth(select_fixture())
  fx$pool$data$analysis$upper_depth_cm <- as.character(fx$pool$data$analysis$upper_depth_cm)

  expect_error(select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE),
               class = "horizons_input_error", regexp = "must be numeric")
  expect_no_error(suppressWarnings(
    select_training(fx$targets, fx$pool, k = 20L, depth = "all", verbose = FALSE)))

})

test_that("the window's point count does not flip with floating-point noise at a tie", {

  expect_identical(window_to_points(40, 8), window_to_points(40, 8 + 1e-12))
  expect_identical(window_to_points(40, 8), window_to_points(40, 8 - 1e-12))

})
