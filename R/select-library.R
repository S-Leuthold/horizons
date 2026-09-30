# R/select-library.R
# What `library =` in select_training() resolves to. A library is a pool of
# reference spectra with lab values attached, and it arrives in one of three
# forms: a horizons_data the user built (`spectra() |> standardize() |>
# add_response()`), a path to a saved library file, or the name of a
# registered library. resolve_source() turns any of the three into the same
# thing, the pool plus a record of where it came from, so the verb never
# branches on the form.
#
# A registered library is never hosted by horizons. The registry pins the
# public source files, their sizes and their MD5s, and the recipe that builds
# the library from them. First use downloads the sources, verifies them,
# builds the library on the user's machine and caches it as qs2 under
# tools::R_user_dir("horizons", "cache"). Every later use reads the cache.
# The data are the source's, credited as such at download and at every draw.


## ---------------------------------------------------------------------------
## The registry
## ---------------------------------------------------------------------------

#' The registered libraries
#'
#' @description
#' One entry per registered library name. An entry carries everything needed
#' to fetch, verify, build and credit the library, and no data. It is a
#' function rather than a constant so tests can substitute a small registry.
#'
#' @return `list.` Named by library name. Each entry has `name`, `version`,
#'   `title`, `sources` (a data.frame of `table`, `file`, `url`, `bytes`,
#'   `md5`), `filters`, `properties` (short name, source column, unit),
#'   `topsoil_max_cm`, `license`, `citation`, `credit`, and `recipe`, the
#'   function that builds the library from the verified source files.
#' @noRd
library_registry <- function() {

  ossl_base <- "https://storage.googleapis.com/soilspec4gg-public/"
  ossl_files <- c(mir  = "ossl_mir_L0_v1.2.csv.gz",
                  lab  = "ossl_soillab_L1_v1.2.csv.gz",
                  site = "ossl_soilsite_L0_v1.2.csv.gz")

  list(

    kssl = list(

      name    = "kssl",
      version = "v1.2",
      title   = "USDA NRCS KSSL mid-infrared library, from the Open Soil Spectral Library v1.2",

      ## Sizes and MD5s as published, verified by HTTP HEAD on 2026-09-11.
      ## OSSL's KSSL MIR data is the laboratory's July 2022 snapshot.

      sources = data.frame(
        table = names(ossl_files),
        file  = unname(ossl_files),
        url   = paste0(ossl_base, unname(ossl_files)),
        bytes = c(420088016, 9597417, 6371319),
        md5   = c("64f8cdfcc28d861f10b608671e4f0421",
                  "e24b94605b0061add400cf3b617f1f18",
                  "e8c640a9e7b6f15c2dec0a7d2e445716"),
        stringsAsFactors = FALSE
      ),

      ## The subset that defines "kssl": KSSL rows scanned on the Vertex 70
      ## with the HTS-XT accessory, every depth, complete spectra on the
      ## native 600 to 4000 cm-1 grid at 2 cm-1.

      filters = list(
        id_col           = "id.layer_uuid_txt",
        dataset_col      = "dataset.code_ascii_txt",
        dataset_value    = "KSSL.SSL",
        instrument_col   = "scan.mir.model.name_utf8_txt",
        instrument_value = "Bruker Vertex 70 with HTS-XT accessory",
        depth_col        = "layer.upper.depth_usda_cm",
        spectral_pattern = "^scan_mir\\.([0-9]+)_abs$",
        wn_min           = 600L,
        wn_max           = 4000L,
        wn_step          = 2L
      ),

      ## Short property name -> OSSL v1.2 soillab L1 column. Units are OSSL's
      ## own and nothing is converted.

      properties = data.frame(
        property = c("clay", "sand", "silt", "total_carbon", "oc", "carbonate",
                     "total_nitrogen", "ph", "cec", "calcium", "magnesium",
                     "potassium", "sodium", "iron_total", "aluminum_total"),
        source   = c("clay.tot_usda.a334_w.pct", "sand.tot_usda.c60_w.pct",
                     "silt.tot_usda.c62_w.pct", "c.tot_usda.a622_w.pct",
                     "oc_usda.c729_w.pct", "caco3_usda.a54_w.pct",
                     "n.tot_usda.a623_w.pct", "ph.h2o_usda.a268_index",
                     "cec_usda.a723_cmolc.kg", "ca.ext_usda.a722_cmolc.kg",
                     "mg.ext_usda.a724_cmolc.kg", "k.ext_usda.a725_cmolc.kg",
                     "na.ext_usda.a726_cmolc.kg", "fe.dith_usda.a66_w.pct",
                     "al.dith_usda.a65_w.pct"),
        unit     = c(rep("% w/w", 7), "pH", rep("cmolc/kg", 5), "% w/w", "% w/w"),
        stringsAsFactors = FALSE
      ),

      topsoil_max_cm = 30,

      license  = "CC-BY-4.0",
      citation = paste0(
        "Safanelli, J.L., Hengl, T., Parente, L.L., Minarik, R., Bloom, D.E., ",
        "Todd-Brown, K., Gholizadeh, A., Mendes, W. de S., Sanderman, J. (2025). ",
        "Open Soil Spectral Library (OSSL): Building reproducible soil calibration ",
        "models through open development and community engagement. ",
        "PLOS ONE 20(1): e0296545. https://doi.org/10.1371/journal.pone.0296545"
      ),

      ## DRAFT pending Sam's approval (2026-09-30).
      credit = paste0(
        "horizons builds the kssl library from the Open Soil Spectral Library (OSSL v1.2). ",
        "This is only possible because of the hard work of the team at Soil Spectroscopy ",
        "for Global Good: Jos\u00e9 L. Safanelli, Tomislav Hengl, Leandro L. Parente, ",
        "Robert Minarik, Dellena E. Bloom, Katherine Todd-Brown, Asa Gholizadeh, ",
        "Wanderson de Sousa Mendes and Jonathan Sanderman. The KSSL spectra and lab data ",
        "were measured and shared by Rich Ferguson and the team at the USDA NRCS Kellogg ",
        "Soil Survey Laboratory."
      ),

      recipe = build_ossl_library

    )

  )

}


## ---------------------------------------------------------------------------
## resolve_source()
## ---------------------------------------------------------------------------

#' Resolve `library =` to a pool
#'
#' @description
#' Dispatches on the form of `library`. A `horizons_data` is the user's own
#' pool and passes through. A character string that names a registered
#' library resolves through the registry to the local cache, building the
#' library on first use. Any other string is a path to a saved library file.
#'
#' @param library `horizons_data` or `character(1)`.
#' @param ask `logical.` Whether a registered library that is not yet cached
#'   may ask before downloading. Default: `interactive()`.
#' @param verbose `logical.` Print progress and the credit line.
#'
#' @return `list` with `pool` (a `horizons_data`) and `record`, the library's
#'   provenance: `form` (`"object"`, `"path"` or `"registered"`), `name`,
#'   `version`, `path`, `citation` and `license` (`NULL` where they do not
#'   apply), and `built_at` for a built library.
#' @noRd
resolve_source <- function(library, ask = interactive(), verbose = TRUE) {

  UseMethod("resolve_source")

}


#' @exportS3Method
#' @noRd
resolve_source.default <- function(library, ask = interactive(), verbose = TRUE) {

  cli::cli_abort(
    c("{.arg library} must be a horizons_data, the name of a registered library, or a path to a library file",
      "x" = "Got {.cls {class(library)[1]}}.",
      "i" = "Registered: {.val {names(library_registry())}}."),
    class = "horizons_input_error"
  )

}


#' @exportS3Method
#' @noRd
resolve_source.horizons_data <- function(library, ask = interactive(), verbose = TRUE) {

  list(pool = library, record = library_record("object"))

}


#' @exportS3Method
#' @noRd
resolve_source.character <- function(library, ask = interactive(), verbose = TRUE) {

  if (length(library) != 1L || is.na(library) || !nzchar(library)) {

    cli::cli_abort("{.arg library} must be a single name or path", class = "horizons_input_error")

  }

  registry <- library_registry()

  if (library %in% names(registry)) {

    return(resolve_registered(registry[[library]], ask = ask, verbose = verbose))

  }

  ## Not a registered name. A string with a path separator or a file suffix,
  ## or one that exists on disk, is a path; a bare word is a mistyped name,
  ## and saying so beats "file not found".

  looks_like_path <- grepl("[/\\\\]", library) || grepl("\\.[A-Za-z0-9]+$", library) ||
                     file.exists(library)

  if (!looks_like_path) {

    cli::cli_abort(
      c("{.val {library}} is not a registered library",
        "i" = "Registered: {.val {names(registry)}}.",
        "i" = "For a library file, pass its path."),
      class = "horizons_input_error"
    )

  }

  if (!file.exists(library)) {

    cli::cli_abort("Library file not found: {.path {library}}", class = "horizons_input_error")

  }

  lib <- read_library_file(library)

  record <- utils::modifyList(lib$record, list(form = "path", path = normalizePath(library)))

  list(pool = lib$pool, record = record)

}


#' Build a library provenance record
#' @noRd
library_record <- function(form, name = NULL, version = NULL, path = NULL,
                           citation = NULL, license = NULL, built_at = NULL) {

  list(form = form, name = name, version = version, path = path,
       citation = citation, license = license, built_at = built_at)

}


## ---------------------------------------------------------------------------
## Library files
## ---------------------------------------------------------------------------

#' Read a saved library file
#'
#' @description
#' A library file is qs2. It holds either a `horizons_library` (the pool plus
#' its record, as the cache writes it) or a bare `horizons_data`, which is a
#' user's own pool saved with `qs2::qs_save()`.
#'
#' @return `list` with `pool` and `record`.
#' @noRd
read_library_file <- function(path) {

  obj <- tryCatch(qs2::qs_read(path), error = function(e) e)

  if (inherits(obj, "error")) {

    cli::cli_abort(
      c("Could not read {.path {path}} as a library file",
        "x" = conditionMessage(obj),
        "i" = "A library file is written by qs2. A cached library that fails to read can be deleted; the next draw rebuilds it."),
      class = "horizons_input_error"
    )

  }

  if (inherits(obj, "horizons_data")) {

    return(list(pool = obj, record = library_record("path")))

  }

  if (!inherits(obj, "horizons_library") || !inherits(obj$pool, "horizons_data")) {

    cli::cli_abort(
      c("{.path {path}} is not a horizons library",
        "x" = "It holds a {.cls {class(obj)[1]}}."),
      class = "horizons_input_error"
    )

  }

  list(pool = obj$pool, record = obj$record)

}


#' Where registered libraries are cached
#'
#' @description
#' `tools::R_user_dir("horizons", "cache")`, unless the option
#' `horizons.cache_dir` names another directory.
#' @noRd
library_cache_dir <- function() {

  getOption("horizons.cache_dir", tools::R_user_dir("horizons", which = "cache"))

}


#' The cache file for a registry entry
#' @noRd
library_cache_path <- function(entry) {

  file.path(library_cache_dir(), paste0(entry$name, "_", entry$version, ".qs2"))

}


## ---------------------------------------------------------------------------
## Registered libraries: cache, consent, download, build
## ---------------------------------------------------------------------------

#' Resolve a registered library
#'
#' @description
#' Reads the cached library when it exists. Otherwise asks (or checks the
#' `horizons.library_download` option), downloads the sources into the cache
#' directory, verifies every MD5, runs the recipe, writes the library and
#' removes the raw downloads.
#'
#' @return `list` with `pool` and `record`.
#' @noRd
resolve_registered <- function(entry, ask = interactive(), verbose = TRUE) {

  path <- library_cache_path(entry)

  if (file.exists(path)) {

    lib <- read_library_file(path)

    if (!identical(lib$record$name, entry$name) || !identical(lib$record$version, entry$version)) {

      cli::cli_abort(
        c("The cached library at {.path {path}} is not {.val {entry$name}} {entry$version}",
          "i" = "Delete it; the next draw rebuilds it."),
        class = "horizons_input_error"
      )

    }

    if (verbose) {

      cat(paste0("\u251C\u2500 Library ", cli::style_bold(entry$name), " ", entry$version,
                 " (cached). Data: ", entry$title, ". Cite: ",
                 sub("\\. .*", "", entry$citation), ", ", entry$license, ".\n"))

    }

    return(lib)

  }

  library_consent(entry, path, ask = ask)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  raw_dir <- file.path(dirname(path), paste0(".raw-", entry$name, "-", entry$version))
  on.exit(unlink(raw_dir, recursive = TRUE), add = TRUE)

  raw  <- download_sources(entry, raw_dir, verbose = verbose)
  pool <- entry$recipe(raw, entry, verbose = verbose)

  record <- library_record(
    "registered",
    name     = entry$name,
    version  = entry$version,
    path     = path,
    citation = entry$citation,
    license  = entry$license,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )

  lib <- structure(list(pool = pool, record = record,
                        sources = entry$sources[, c("file", "url", "bytes", "md5")]),
                   class = "horizons_library")

  ## Write to a temporary name and rename, so an interrupted write never
  ## leaves a file the next draw would mistake for a finished library.

  tmp <- paste0(path, ".tmp")
  qs2::qs_save(lib, tmp)
  file.rename(tmp, path)

  if (verbose) {

    cat(paste0("\u251C\u2500 Library ", cli::style_bold(entry$name), " ", entry$version,
               " built and cached at ", path, " (",
               format(round(file.size(path) / 1e6)), " MB).\n"))

  }

  list(pool = lib$pool, record = lib$record)

}


#' Ask before a registered library's first download
#'
#' @description
#' Always prints what is about to happen, its size and where it goes, and
#' the credit. Proceeds without asking when `horizons.library_download` is
#' `TRUE`; otherwise asks in an interactive session and stops in a
#' non-interactive one, naming the option.
#' @noRd
library_consent <- function(entry, path, ask = interactive()) {

  mb <- round(sum(entry$sources$bytes) / 1e6)

  cli::cli_inform(c(
    "i" = "The {.val {entry$name}} library is not cached yet.",
    " " = "horizons will download {mb} MB of public source files, verify them, and build the library in {.path {dirname(path)}}. The raw downloads are removed once it is built.",
    " " = entry$credit,
    " " = "Please cite: {entry$citation} ({entry$license})."
  ))

  if (isTRUE(getOption("horizons.library_download"))) return(invisible(TRUE))

  if (!isTRUE(ask)) {

    cli::cli_abort(
      c("Not downloading {.val {entry$name}} without consent",
        "i" = "Set {.code options(horizons.library_download = TRUE)} to allow it in a non-interactive session."),
      class = "horizons_consent_error"
    )

  }

  answer <- utils::askYesNo("Download and build it now?", default = FALSE)

  if (!isTRUE(answer)) {

    cli::cli_abort("Download of {.val {entry$name}} declined; nothing was written.",
                   class = "horizons_consent_error")

  }

  invisible(TRUE)

}


#' Download and verify a registry entry's source files
#'
#' @return Named character vector of local paths, named by `table`.
#' @noRd
download_sources <- function(entry, dir, verbose = TRUE) {

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  ## The default 60 s timeout cannot move 400 MB.

  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)

  src   <- entry$sources
  paths <- stats::setNames(file.path(dir, src$file), src$table)

  for (i in seq_len(nrow(src))) {

    if (verbose) cat(paste0("\u251C\u2500 Downloading ", src$file[i], "...\n"))

    status <- tryCatch(
      utils::download.file(src$url[i], paths[[i]], mode = "wb", quiet = TRUE),
      error = function(e) e
    )

    if (inherits(status, "error") || !identical(as.integer(status), 0L)) {

      why <- if (inherits(status, "error")) conditionMessage(status) else paste("status", status)

      cli::cli_abort(
        c("Could not download {.file {src$file[i]}}",
          "x" = why,
          "i" = "Source: {.url {src$url[i]}}"),
        class = "horizons_download_error"
      )

    }

    md5 <- unname(tools::md5sum(paths[[i]]))

    if (!identical(md5, src$md5[i])) {

      cli::cli_abort(
        c("{.file {src$file[i]}} does not match its published checksum",
          "x" = "MD5 {md5}, expected {src$md5[i]}.",
          "i" = "The source may have changed or the download was corrupted; nothing was cached."),
        class = "horizons_download_error"
      )

    }

  }

  paths

}


## ---------------------------------------------------------------------------
## The recipe
## ---------------------------------------------------------------------------

#' Build a library from OSSL's published tables
#'
#' @description
#' The operational definition of an OSSL-derived library, ported from
#' `dev/experiments/2026-09-local-strategy/01-build-kssl-snapshot.R`, with the
#' surface-only filter removed: every depth is kept and the upper depth is
#' carried as a `meta` column, `upper_depth_cm`, so the draw can restrict to
#' topsoil. Rows are the entry's dataset and instrument with a complete
#' spectrum on the entry's grid and a matching lab row.
#'
#' @param raw Named paths: `mir`, `lab`, `site`, already MD5-verified.
#' @param entry A registry entry.
#'
#' @return `horizons_data` with `sample_id`, `upper_depth_cm` (meta), the
#'   spectra on the entry's grid and one response per mapped property present
#'   in the lab table.
#' @noRd
build_ossl_library <- function(raw, entry, verbose = TRUE) {

  f <- entry$filters

  if (verbose) cat("\u251C\u2500 Building the library from the verified sources...\n")

  ## Site: the upper depth of every layer ------------------------------------

  ## fread() for speed on a 400 MB table, then plain data.frames: the
  ## package is not data.table-aware, so `[` must not be data.table's.

  site <- read_ossl_table(raw[["site"]], c(f$id_col, f$depth_col))
  site <- site[!duplicated(site[[f$id_col]]), , drop = FALSE]
  names(site) <- c("sample_id", "upper_depth_cm")

  ## MIR: check the grid on the header before reading the body ---------------

  hdr       <- names(data.table::fread(raw[["mir"]], nrows = 0))
  spec_cols <- grep(f$spectral_pattern, hdr, value = TRUE)
  need      <- c(f$id_col, f$dataset_col, f$instrument_col)

  if (length(setdiff(need, hdr))) {

    cli::cli_abort("The MIR table lacks {.val {setdiff(need, hdr)}}", class = "horizons_build_error")

  }

  wn        <- as.integer(sub(f$spectral_pattern, "\\1", spec_cols))
  keep      <- wn >= f$wn_min & wn <= f$wn_max
  spec_cols <- spec_cols[keep][order(wn[keep])]
  wn        <- sort(wn[keep])

  if (!identical(wn, seq.int(f$wn_min, f$wn_max, by = f$wn_step))) {

    cli::cli_abort(
      "The MIR table's grid is not {f$wn_min} to {f$wn_max} by {f$wn_step} cm-1 ({length(wn)} columns found)",
      class = "horizons_build_error"
    )

  }

  mir <- read_ossl_table(raw[["mir"]], c(need, spec_cols))
  mir <- mir[mir[[f$dataset_col]] %in% f$dataset_value &
             mir[[f$instrument_col]] %in% f$instrument_value, , drop = FALSE]
  mir <- mir[!duplicated(mir[[f$id_col]]), , drop = FALSE]

  spec     <- as.matrix(mir[, spec_cols, drop = FALSE])
  complete <- rowSums(!is.finite(spec)) == 0L
  mir_ids  <- mir[[f$id_col]][complete]
  spec     <- spec[complete, , drop = FALSE]
  rm(mir)

  ## Lab: the mapped properties present in this release ----------------------

  lab_hdr <- names(data.table::fread(raw[["lab"]], nrows = 0))
  pm      <- entry$properties[entry$properties$source %in% lab_hdr, , drop = FALSE]
  lab     <- read_ossl_table(raw[["lab"]], c(f$id_col, pm$source))
  lab     <- lab[!duplicated(lab[[f$id_col]]), , drop = FALSE]
  names(lab) <- c("sample_id", pm$property)

  ## Join on id: spectra with a lab row and a site row -----------------------

  ids <- sort(Reduce(intersect, list(mir_ids, lab$sample_id, site$sample_id)))

  if (!length(ids)) {

    cli::cli_abort("No rows survive the filters and the join", class = "horizons_build_error")

  }

  at   <- match(ids, mir_ids)
  spec <- spec[at, , drop = FALSE]
  colnames(spec) <- paste0("wn_", wn)

  df <- data.frame(sample_id      = ids,
                   upper_depth_cm = site$upper_depth_cm[match(ids, site$sample_id)],
                   spec, check.names = FALSE, stringsAsFactors = FALSE)
  rm(spec)

  lab <- lab[match(ids, lab$sample_id), , drop = FALSE]

  ## standardize() on the native grid moves no values; it records the axis,
  ## which is what lets the draw reconcile the library against the targets.

  build <- function() {

    pool <- spectra(df, id_col = "sample_id")
    pool <- standardize(pool, resample = f$wn_step, trim = c(f$wn_min, f$wn_max))
    add_response(pool, lab, variable = pm$property)

  }

  if (verbose) return(build())

  utils::capture.output(pool <- build())
  pool

}


#' Read selected columns of an OSSL table as a plain data.frame
#' @noRd
read_ossl_table <- function(path, cols) {

  as.data.frame(data.table::fread(path, select = cols, showProgress = FALSE))

}
