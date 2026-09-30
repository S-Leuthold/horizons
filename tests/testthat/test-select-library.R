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
  expect_false(dir.exists(file.path(cache, ".raw-mini-v0")))
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
