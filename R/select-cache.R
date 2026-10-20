# R/select-cache.R
# The similarity space, cached beside a library. Building the space is a PCA
# of every library row, about a minute on KSSL, and it depends only on the
# library and the space's levers, not on the batch: distances are measured in
# the library's own space on its own grid (search_axis()). So a registered
# library or a library file keeps its fitted space in a small file in the
# cache directory, one per combination of levers, and a draw reads it rather
# than refitting. A user's own horizons_data is not cached: hashing a 1 GB
# object on every call costs seconds, and arbitrary pools would pile up in the
# cache.


## ---------------------------------------------------------------------------
## space_cache_identity() — what a cached space belongs to
## ---------------------------------------------------------------------------

#' The identity a cached space is keyed by, or NULL when it is not cached
#'
#' @description
#' A registered library is identified by its name, version and build time: a
#' rebuilt library is a new library, and its old spaces are never reused. A
#' library file is identified by a hash of the file itself. An object passed
#' in memory has no identity worth the cost of computing, and is not cached.
#'
#' @param record [List.] The library record from `resolve_source()`.
#' @return [Character or NULL.] The identity string, and its short label as
#'   the attribute `label`, used in the cache file's name.
#' @noRd
space_cache_identity <- function(record) {

  if (identical(record$form, "registered")) {

    id <- paste(record$name, record$version, record$built_at, sep = "|")
    return(structure(id, label = paste0(record$name, "_", record$version)))

  }

  if (identical(record$form, "path") && !is.null(record$path) && file.exists(record$path)) {

    id <- paste0("file|", digest::digest(file = record$path, algo = "xxhash64"))
    label <- gsub("[^A-Za-z0-9_.-]", "_", tools::file_path_sans_ext(basename(record$path)))
    return(structure(id, label = label))

  }

  NULL

}


## ---------------------------------------------------------------------------
## space_cache_key() — the identity plus every lever the space depends on
## ---------------------------------------------------------------------------

#' Hash the identity, the space's levers and the grid it is built on
#'
#' @description
#' The key also carries `SELECT_SPACE_CACHE_VERSION` and the PCA component
#' cap, so a space built by older code is never read as this code's.
#'
#' @param identity [Character.] From `space_cache_identity()`.
#' @param settings [List.] The levers as `build_similarity_space()` takes
#'   them: `snv`, `derivative`, `window` (points), `poly`, `mask`, `ncomp`,
#'   `sdev_floor`.
#' @param wn [Numeric.] The grid the space is built on.
#' @return [Character.] A hex digest.
#' @noRd
space_cache_key <- function(identity, settings, wn) {

  digest::digest(list(
    version    = SELECT_SPACE_CACHE_VERSION,
    max_comp   = SELECT_PCA_MAX_COMP,
    identity   = as.character(identity),
    snv        = isTRUE(settings$snv),
    derivative = as.integer(settings$derivative),
    window     = as.integer(settings$window),
    poly       = as.integer(settings$poly),
    mask       = if (is.null(settings$mask)) NULL else unname(as.matrix(settings$mask) + 0),
    ncomp      = as.numeric(settings$ncomp),
    sdev_floor = as.numeric(settings$sdev_floor),
    wn         = as.numeric(wn)
  ), algo = "xxhash64")

}


## ---------------------------------------------------------------------------
## cached_space() — read the space, or build it and write it
## ---------------------------------------------------------------------------

#' Read a library's similarity space from the cache, or build and cache it
#'
#' @description
#' The cache file sits in `library_cache_dir()`, named
#' `<label>.space-<key>.qs2`. A file that fails to read or holds another key
#' is rebuilt over, which costs only time once, so it is a note on the
#' verb's log. A cache that cannot be written (a read-only directory) costs
#' the speed of every later draw, not this one: the space is returned and a
#' `skipped` warning says why it was not kept.
#'
#' @param identity [Character.] From `space_cache_identity()`.
#' @param settings,wn As for `space_cache_key()`.
#' @param build [Function.] No arguments; returns the fitted space.
#' @param verbose [Logical.] Print the tree line for a build.
#' @param force [Logical.] Rebuild and overwrite even when a matching file
#'   exists. Default: `FALSE`.
#' @param collector `select_training()`'s collector ([log_open()]), which the
#'   note and the warning go to.
#' @return [List.] `space`, `hit` (logical), `key`, `path`.
#' @noRd
cached_space <- function(identity, settings, wn, build, verbose = TRUE, force = FALSE,
                         collector = log_open("select_training")) {

  key  <- space_cache_key(identity, settings, wn)
  path <- file.path(library_cache_dir(),
                    paste0(attr(identity, "label"), ".space-", substr(key, 1, 16), ".qs2"))

  if (!force && file.exists(path)) {

    obj <- tryCatch(qs2::qs_read(path), error = function(e) NULL)

    if (inherits(obj, "horizons_space_cache") && identical(obj$key, key) &&
        inherits(obj$space, "horizons_similarity_space")) {

      return(list(space = obj$space, hit = TRUE, key = key, path = path))

    }

    log_note(collector, "The cached similarity space at {.path {path}} could not be read or was built for other settings, so it was rebuilt.")

    if (verbose) cat("\u2502  \u251C\u2500 The cached similarity space could not be used; rebuilding it\n")

  }

  if (verbose) {

    cat("\u2502  \u251C\u2500 Building the similarity space on the whole library; it is cached, so this happens once per setting\n")

  }

  space <- build()

  obj <- structure(list(key = key, identity = as.character(identity), space = space,
                        built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
                   class = "horizons_space_cache")

  written <- tryCatch({

    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(".space-", tmpdir = dirname(path), fileext = ".qs2.tmp")
    on.exit(unlink(tmp), add = TRUE)
    qs2::qs_save(obj, tmp)
    file.rename(tmp, path)

  }, error = function(e) FALSE)

  if (!isTRUE(written)) {

    log_warning(collector, "skipped", c(
      "The similarity space was built but could not be cached, so every draw at these settings builds it again.",
      "i" = "Cache directory: {.path {dirname(path)}}; set {.code options(horizons.cache_dir = )} to a writable directory."
    ))

  }

  list(space = space, hit = FALSE, key = key, path = if (isTRUE(written)) path else NULL)

}


## ---------------------------------------------------------------------------
## default_space_settings() — the levers select_training() uses by default
## ---------------------------------------------------------------------------

#' The similarity-space levers at select_training()'s defaults, on a grid
#'
#' @description
#' Read from `select_training()`'s own formals, so the space built at a
#' library's first use is keyed exactly as a default draw will look it up.
#'
#' @param resolution [Numeric.] The grid spacing the window is converted on.
#' @return [List.] As `space_cache_key()` takes it.
#' @noRd
default_space_settings <- function(resolution) {

  f <- formals(select_training)

  list(snv        = eval(f$snv),
       derivative = as.integer(eval(f$derivative)),
       window     = window_to_points(eval(f$window), resolution),
       poly       = as.integer(eval(f$poly)),
       mask       = eval(f$mask),
       ncomp      = eval(f$ncomp),
       sdev_floor = eval(f$sdev_floor))

}


## ---------------------------------------------------------------------------
## prime_library_space() — build the default space at a library's first use
## ---------------------------------------------------------------------------

#' Build and cache a new library's default similarity space
#'
#' @description
#' Called right after a registered library is built and cached, so the
#' one-time wait at first use covers the space too and every default draw
#' after it is fast. It is one unit ([run_unit()]): a run failure costs
#' nothing but time, since the first draw builds the space, so it is a note
#' on the verb's log, and with `debug` it stops the call with its
#' backtrace. An input or internal error propagates, as from any unit. A
#' library narrower than the default window is not primed: the filter needs
#' fewer points than the library has columns ([sg_window_fits()]), and a
#' default draw refuses that window at its door. The space is fit without
#' the library's flagged rows, as a draw fits it, so the two agree.
#'
#' @param pool [horizons_data.] The library just built.
#' @param record [List.] Its record.
#' @param verbose [Logical.]
#' @param collector `select_training()`'s collector ([log_open()]).
#' @param debug [Logical.] Passed to [run_unit()].
#' @return `NULL`, invisibly.
#' @noRd
prime_library_space <- function(pool, record, verbose = TRUE,
                                collector = log_open("select_training"), debug = FALSE) {

  identity <- space_cache_identity(record)
  if (is.null(identity)) return(invisible(NULL))

  ## The space a draw builds: without the rows an earlier step flagged
  pm        <- predictor_matrix(pool)
  pm$matrix <- pm$matrix[!flagged_rows(pool$data$analysis), , drop = FALSE]
  settings  <- default_space_settings(grid_summary(pm$wavenumbers)$resolution)
  n_cols    <- length(pm$wavenumbers)

  if (settings$derivative > 0L && !sg_window_fits(settings$window, n_cols)) {

    width <- eval(formals(select_training)$window)

    log_note(collector, c(
      "The {record$name} library's similarity space was not built at first use: the default {.arg window} of {width} cm-1 is {settings$window} points on its {n_cols} columns, and the filter needs fewer.",
      "i" = "A draw at the default window is refused; pass a narrower {.arg window}."
    ))

    if (verbose) {
      cat("\u251C\u2500 The library is narrower than the default window, so its similarity space is built at the first draw that fits\n")
    }

    return(invisible(NULL))

  }

  if (verbose) {

    cat("\u251C\u2500 Building the library's similarity space at the default settings (about a minute, once)...\n")

  }

  ## cached_space() writes its own note or warning to the collector, so the
  ## unit's records hold only a failure
  out <- run_unit(
    cached_space(identity, settings, pm$wavenumbers, verbose = FALSE, collector = collector,
                 build = function() {
                   build_similarity_space(pm$matrix, pm$wavenumbers,
                                          snv = settings$snv, derivative = settings$derivative,
                                          window = settings$window, poly = settings$poly,
                                          mask = settings$mask, space = "pca", ncomp = settings$ncomp,
                                          sdev_floor = settings$sdev_floor)
                 }),
    unit = "library_space", id = record$name, debug = debug
  )

  if (out$failed) {

    cause <- unit_failure_message(out$records, outside = NULL)

    log_note(collector, c(
      "The {record$name} library's similarity space could not be built at first use; the first draw builds it.",
      "x" = "{cause}"
    ))

    if (verbose) cat("\u251C\u2500 The library's similarity space could not be built now; the first draw will build it\n")

  }

  invisible(NULL)

}
