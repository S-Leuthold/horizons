# tests/testthat/test-select-library.R
# resolve_source(): the three forms of `library =`, and the registered path
# end to end against a miniature OSSL written to disk and served over
# file:// URLs, so nothing touches the network or the user's cache.


## ---------------------------------------------------------------------------
## Fixture: a miniature OSSL release and a registry entry pointing at it
## ---------------------------------------------------------------------------

#' Write a three-table OSSL-shaped release and return its registry entry
#'
#' @description
#' Eight KSSL Vertex 70 rows at mixed depths, plus one row from another
#' dataset, one from another instrument and one with a missing absorbance,
#' which the recipe must drop. The grid is 600 to 700 cm-1 at 2 cm-1.
#' @noRd
make_mini_ossl <- function(dir, seed = 1) {

  set.seed(seed)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  wn  <- seq(700L, 600L, by = -2L)
  ids <- sprintf("id%02d", 1:11)
  n   <- length(ids)

  spec <- matrix(stats::runif(n * length(wn), 0.2, 1.2), nrow = n)
  spec[11, 5] <- NA
  colnames(spec) <- sprintf("scan_mir.%d_abs", wn)

  mir <- data.frame(
    id.layer_uuid_txt            = ids,
    dataset.code_ascii_txt       = c(rep("KSSL.SSL", 8), "AFSIS1.SSL", "KSSL.SSL", "KSSL.SSL"),
    scan.mir.model.name_utf8_txt = c(rep("Bruker Vertex 70 with HTS-XT accessory", 9),
                                     "Bruker Tensor 27", "Bruker Vertex 70 with HTS-XT accessory"),
    spec, check.names = FALSE, stringsAsFactors = FALSE
  )

  lab <- data.frame(id.layer_uuid_txt        = ids,
                    clay.tot_usda.a334_w.pct = stats::runif(n, 5, 50),
                    oc_usda.c729_w.pct       = c(stats::runif(n - 3, 0.2, 4), NA, NA, NA))

  site <- data.frame(id.layer_uuid_txt         = ids,
                     layer.upper.depth_usda_cm = c(0, 10, 20, 30, 45, 60, 0, 25, 0, 0, 0))

  paths <- c(mir  = file.path(dir, "mir.csv.gz"),
             lab  = file.path(dir, "lab.csv.gz"),
             site = file.path(dir, "site.csv.gz"))

  data.table::fwrite(mir,  paths[["mir"]])
  data.table::fwrite(lab,  paths[["lab"]])
  data.table::fwrite(site, paths[["site"]])

  entry <- library_registry()$kssl
  entry$name    <- "mini"
  entry$version <- "v0"
  entry$filters$wn_max <- 700L
  entry$sources <- data.frame(
    table = names(paths),
    file  = basename(paths),
    url   = paste0("file://", normalizePath(paths)),
    bytes = unname(file.size(paths)),
    md5   = unname(tools::md5sum(paths)),
    stringsAsFactors = FALSE
  )

  entry

}


#' Point the registry at `entry` and the cache at a temporary directory
#' @noRd
local_mini_registry <- function(entry, env = parent.frame()) {

  cache <- withr::local_tempdir(.local_envir = env)
  withr::local_options(horizons.cache_dir = cache, .local_envir = env)
  local_mocked_bindings(library_registry = function() list(mini = entry), .env = env)
  cache

}


## ---------------------------------------------------------------------------
## The three forms
## ---------------------------------------------------------------------------

test_that("a horizons_data passes through as the object form", {

  fx  <- make_select_fixture()
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

  fx   <- make_select_fixture()
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
  expect_identical(list.files(cache, all.files = TRUE, no.. = TRUE), "mini_v0.qs2")
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

test_that("select_training() records which library it drew from", {

  fx  <- make_select_fixture()
  out <- select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE)

  expect_identical(out$selection$library$form, "object")

})

test_that("select_training() resolves a library name before anything else", {

  expect_error(select_training(make_select_fixture()$targets, "ksl", verbose = FALSE),
               class = "horizons_input_error", regexp = "not a registered library")

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

  fx  <- with_depth(make_select_fixture())
  out <- select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE)

  upper <- fx$pool$data$analysis$upper_depth_cm
  top   <- fx$pool$data$analysis$sample_id[!is.na(upper) & upper < 30]

  expect_true(all(out$data$analysis$sample_id %in% top))
  expect_identical(out$selection$settings$depth, "topsoil")
  expect_true(out$selection$depth$applied)
  expect_identical(out$selection$depth$n_eligible, length(top))

})

test_that("depth = 'all' can draw deep rows", {

  fx  <- with_depth(make_select_fixture())
  out <- select_training(fx$targets, fx$pool, k = 60L, depth = "all", verbose = FALSE)

  upper <- fx$pool$data$analysis$upper_depth_cm[match(out$data$analysis$sample_id,
                                                     fx$pool$data$analysis$sample_id)]

  expect_true(any(upper >= 30, na.rm = TRUE))
  expect_false(out$selection$depth$applied)

})

test_that("a library with no depth column draws from every row and says so", {

  fx  <- make_select_fixture()
  out <- select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE)

  expect_false(out$selection$depth$recorded)
  expect_false(out$selection$depth$applied)
  expect_identical(out$selection$depth$n_eligible, nrow(fx$pool$data$analysis))

})

test_that("global under topsoil returns the eligible rows, not the whole pool", {

  fx  <- with_depth(make_select_fixture())
  out <- select_training(fx$targets, fx$pool, k = 20L, scope = "global", verbose = FALSE)

  expect_identical(nrow(out$data$analysis), out$selection$depth$n_eligible)

})

test_that("depth rejects anything but topsoil or all", {

  fx <- make_select_fixture()

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

  fx <- make_select_fixture()

  expect_error(select_training(fx$targets, fx$pool, k = 20L, window = 4, verbose = FALSE),
               class = "horizons_input_error", regexp = "too few for a polynomial")
  expect_error(suppressMessages(capture.output(
    select_training(fx$targets, fx$pool, k = 20L, window = -1, verbose = FALSE))),
    class = "horizons_input_error", regexp = "positive width")

})


## ---------------------------------------------------------------------------
## Review fixes (2026-09-30)
## ---------------------------------------------------------------------------

test_that("a bad argument is reported before a registered library is fetched", {

  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)

  expect_error(suppressMessages(capture.output(
    select_training("not data", "mini", depth = "subsoil", verbose = FALSE))),
    class = "horizons_input_error", regexp = "depth")
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)

})

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

  fx   <- make_select_fixture()
  deep <- fx$family_of_target[[1]]
  fx   <- with_family_depth(fx, deep)
  from_deep <- names(fx$family_of_target)[fx$family_of_target == deep]

  top <- suppressWarnings(select_training(fx$targets, fx$pool, k = 20L, verbose = FALSE))
  all <- suppressWarnings(select_training(fx$targets, fx$pool, k = 20L, depth = "all", verbose = FALSE))

  ## The deep-family targets resemble rows topsoil cannot draw. Measured
  ## against the whole library none of them was named; against the drawable
  ## rows they are (all but a target that happens to sit near the other
  ## family, on this fixture).
  flagged <- intersect(top$selection$resemblance$beyond$target_id, from_deep)
  expect_gte(length(flagged), length(from_deep) - 1L)
  expect_true(all(top$selection$resemblance$beyond$target_id %in% from_deep))
  expect_length(intersect(all$selection$resemblance$beyond$target_id, from_deep), 0L)

})

test_that("a k beyond the topsoil rows says depth = 'all' would reach more", {

  fx <- with_depth(make_select_fixture())

  expect_error(select_training(fx$targets, fx$pool, k = 250L, properties = "clay", verbose = FALSE),
               class = "horizons_input_error", regexp = "depth = \"all\"")

})


## ---------------------------------------------------------------------------
## Re-review fixes (2026-09-30)
## ---------------------------------------------------------------------------

test_that("the resemblance check is skipped, not failed, when too few rows can be drawn", {

  fx <- make_select_fixture(n_pool = 60)
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

test_that("the space's levers are checked before a registered library is fetched", {

  entry <- make_mini_ossl(withr::local_tempdir())
  cache <- local_mini_registry(entry)
  withr::local_options(horizons.library_download = TRUE)
  fx    <- make_select_fixture()

  bad <- list(list(mask = "bad"), list(derivative = NA), list(poly = -1),
              list(ncomp = -1), list(chunk_size = 0), list(window = 4))

  for (args in bad) {

    expect_error(suppressMessages(capture.output(
      do.call(select_training, c(list(fx$targets, "mini", verbose = FALSE), args)))),
      class = "horizons_input_error", info = names(args))

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
