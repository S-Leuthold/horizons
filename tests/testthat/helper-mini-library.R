# tests/testthat/helper-mini-library.R
# A miniature OSSL release written to disk and served over file:// URLs, and
# a registry pointing at it, so the registered-library path runs end to end
# with no network and a cache in a tempdir. Shared by test-select-library.R
# and test-select-cache.R.


## ---------------------------------------------------------------------------
## make_mini_ossl() — write an OSSL-shaped release and return its entry
## ---------------------------------------------------------------------------

#' Write a three-table OSSL-shaped release and return its registry entry
#'
#' @description
#' With `extras = TRUE` (the default), eight KSSL Vertex 70 rows at mixed
#' depths, plus one row from another dataset, one from another instrument and
#' one with a missing absorbance, which the recipe must drop. With
#' `extras = FALSE`, `n_kssl` clean KSSL rows, all topsoil, for tests that
#' need a pool large enough to draw from. The grid is 600 to 700 cm-1 at
#' 2 cm-1.
#'
#' @param dir [Character.] Directory to write the tables into.
#' @param seed [Integer.] Default: `1`.
#' @param extras [Logical.] Default: `TRUE`.
#' @param n_kssl [Integer.] KSSL rows when `extras = FALSE`. Default: `60`.
#' @return [List.] A registry entry named `"mini"`, version `"v0"`.
#' @noRd
make_mini_ossl <- function(dir, seed = 1, extras = TRUE, n_kssl = 60L) {

  set.seed(seed)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  wn <- seq(700L, 600L, by = -2L)

  if (extras) {

    ids <- sprintf("id%02d", 1:11)
    n   <- length(ids)

    spec <- matrix(stats::runif(n * length(wn), 0.2, 1.2), nrow = n)
    spec[11, 5] <- NA

    dataset    <- c(rep("KSSL.SSL", 8), "AFSIS1.SSL", "KSSL.SSL", "KSSL.SSL")
    instrument <- c(rep("Bruker Vertex 70 with HTS-XT accessory", 9),
                    "Bruker Tensor 27", "Bruker Vertex 70 with HTS-XT accessory")
    clay       <- stats::runif(n, 5, 50)
    oc         <- c(stats::runif(n - 3, 0.2, 4), NA, NA, NA)
    depth      <- c(0, 10, 20, 30, 45, 60, 0, 25, 0, 0, 0)

  } else {

    ids <- sprintf("id%03d", seq_len(n_kssl))
    n   <- n_kssl

    spec       <- matrix(stats::runif(n * length(wn), 0.2, 1.2), nrow = n)
    dataset    <- "KSSL.SSL"
    instrument <- "Bruker Vertex 70 with HTS-XT accessory"
    clay       <- stats::runif(n, 5, 50)
    oc         <- stats::runif(n, 0.2, 4)
    depth      <- 0

  }

  colnames(spec) <- sprintf("scan_mir.%d_abs", wn)

  mir  <- data.frame(id.layer_uuid_txt = ids, dataset.code_ascii_txt = dataset,
                     scan.mir.model.name_utf8_txt = instrument, spec,
                     check.names = FALSE, stringsAsFactors = FALSE)
  lab  <- data.frame(id.layer_uuid_txt = ids,
                     clay.tot_usda.a334_w.pct = clay,
                     oc_usda.c729_w.pct = oc)
  site <- data.frame(id.layer_uuid_txt = ids, layer.upper.depth_usda_cm = depth)

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


## ---------------------------------------------------------------------------
## local_mini_registry() — point the registry and the cache at a tempdir
## ---------------------------------------------------------------------------

#' Point the registry at `entry` and the cache at a temporary directory
#' @return [Character.] The cache directory.
#' @noRd
local_mini_registry <- function(entry, env = parent.frame()) {

  cache <- withr::local_tempdir(.local_envir = env)
  withr::local_options(horizons.cache_dir = cache, .local_envir = env)
  local_mocked_bindings(library_registry = function() list(mini = entry), .env = env)
  cache

}
