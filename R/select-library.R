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
#'   `license`, `short_cite`, `citation`, `credit`, and
#'   `recipe`, the
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
        wn_step          = 2L,
        ## Not hull-corrected. select_training() reads this before the
        ## download to refuse hull-corrected targets (#107), and the build
        ## standardizes with it, so the two cannot disagree.
        baseline         = FALSE
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

      license    = "CC-BY-4.0",
      short_cite = "Safanelli et al. 2025",
      citation   = paste0(
        "Safanelli, J.L., Hengl, T., Parente, L.L., Minarik, R., Bloom, D.E., ",
        "Todd-Brown, K., Gholizadeh, A., Mendes, W. de S., Sanderman, J. (2025). ",
        "Open Soil Spectral Library (OSSL): Building reproducible soil calibration ",
        "models through open development and community engagement. ",
        "PLOS ONE 20(1): e0296545. https://doi.org/10.1371/journal.pone.0296545"
      ),

      ## Wording approved by Sam, 2026-09-30.
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
#' pool and passes through, as does a `horizons_library` already in
#' memory. A character string that names a registered
#' library resolves through the registry to the local cache, building the
#' library on first use. Any other string is a path to a saved library file.
#' `select_training()` has checked the form at its door: one of these, a
#' library object holding a `horizons_data`, a single string, and a path
#' that exists.
#'
#' @param library `horizons_data`, `horizons_library` or `character(1)`.
#' @param ask `logical.` Whether a registered library that is not yet cached
#'   may ask before downloading. Default: `interactive()`.
#' @param verbose `logical.` Print progress and the credit line.
#' @param collector `select_training()`'s collector ([log_open()]): building
#'   a registered library reports to it.
#' @param debug `logical.` Passed to the first-use space's unit
#'   ([prime_library_space()]).
#' @param call The frame a refusal reports: the verb's.
#'
#' @return `list` with `pool` (a `horizons_data`) and `record`, the library's
#'   provenance: `form` (`"object"`, `"path"` or `"registered"`), `name`,
#'   `version`, `path`, `citation` and `license` (`NULL` where they do not
#'   apply), and `built_at` for a built library.
#' @noRd
resolve_source <- function(library, ask = interactive(), verbose = TRUE,
                           collector = log_open("select_training"), debug = FALSE,
                           call = rlang::caller_env()) {

  UseMethod("resolve_source")

}


#' @exportS3Method
#' @noRd
resolve_source.horizons_library <- function(library, ask = interactive(), verbose = TRUE,
                                            collector = log_open("select_training"), debug = FALSE,
                                            call = rlang::caller_env()) {

  list(pool = library$pool, record = utils::modifyList(library$record, list(form = "object")))

}


#' @exportS3Method
#' @noRd
resolve_source.horizons_data <- function(library, ask = interactive(), verbose = TRUE,
                                         collector = log_open("select_training"), debug = FALSE,
                                         call = rlang::caller_env()) {

  list(pool = library, record = library_record("object"))

}


#' @exportS3Method
#' @noRd
resolve_source.character <- function(library, ask = interactive(), verbose = TRUE,
                                     collector = log_open("select_training"), debug = FALSE,
                                     call = rlang::caller_env()) {

  registry <- library_registry()

  if (library %in% names(registry)) {

    return(resolve_registered(registry[[library]], ask = ask, verbose = verbose,
                              collector = collector, debug = debug, call = call))

  }

  lib <- read_library_file(library, call = call)

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
#' @param path [Character.] The file.
#' @param call The frame a refusal reports: the verb's.
#'
#' @return `list` with `pool` and `record`.
#' @noRd
read_library_file <- function(path, call = rlang::caller_env()) {

  obj <- rlang::try_fetch(
    qs2::qs_read(path),
    error = function(e) {
      abort_horizons(c(
        "Could not read {.path {path}} as a library file.",
        "i" = "A library file is written by qs2. If this is a cached registered library, delete it and the next draw rebuilds it."
      ), kind = "input", parent = e, call = call)
    }
  )

  if (inherits(obj, "horizons_data")) {

    return(list(pool = obj, record = library_record("path")))

  }

  if (!inherits(obj, "horizons_library") || !inherits(obj$pool, "horizons_data")) {

    abort_horizons(c(
      "{.path {path}} is not a horizons library.",
      "x" = "It holds {.obj_type_friendly {obj}}.",
      "i" = "A library file is a qs2 file holding a horizons_data, saved with {.code qs2::qs_save()}, or a library horizons wrote."
    ), kind = "input", call = call)

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
#' A library built but not moved into the cache (a rename that fails on a
#' locked or network filesystem) is still returned, with its record's
#' `path` `NULL` and one `skipped` warning, since every later draw downloads
#' and builds it again until the cache directory is fixed; its first-use
#' space is not built.
#'
#' @param entry A registry entry.
#' @param ask,verbose,collector,debug As for [resolve_source()].
#' @param call The frame a refusal reports: the verb's.
#'
#' @return `list` with `pool` and `record`.
#' @noRd
resolve_registered <- function(entry, ask = interactive(), verbose = TRUE,
                               collector = log_open("select_training"), debug = FALSE,
                               call = rlang::caller_env()) {

  path <- library_cache_path(entry)

  if (file.exists(path)) {

    lib <- read_library_file(path, call = call)

    if (!identical(lib$record$name, entry$name) || !identical(lib$record$version, entry$version)) {

      abort_horizons(c(
        "The cached library at {.path {path}} is not {.val {entry$name}} {entry$version}.",
        "i" = "Delete it; the next draw rebuilds it."
      ), kind = "validation", call = call)

    }

    ## A library cached by an earlier version of horizons can lack a field the
    ## object contract now requires. The cache is keyed on the library's name
    ## and version only, so it is never rebuilt on its own.
    tryCatch(
      validate_horizons_data(lib$pool),
      horizons_error_validation = function(e) {
        cli::cli_abort(
          c("The cached library at {.path {path}} was built by an earlier version of horizons and no longer meets the object contract.",
            "i" = "Delete it; the next draw rebuilds it."),
          class = "horizons_input_error", parent = e
        )
      }
    )

    if (verbose) {

      cat(paste0("\u251C\u2500 Library ", cli::style_bold(entry$name), " ", entry$version,
                 " (cached). Data: ", entry$title, ". Cite: ",
                 entry$short_cite, ", ", entry$license, ".\n"))

    }

    return(lib)

  }

  library_consent(entry, path, ask = ask, call = call)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  clear_stale_builds(entry, dirname(path))

  ## A new build is a new library (its build time is part of every cached
  ## space's key), so spaces cached for an earlier build of this name and
  ## version can never be read again.

  unlink(list.files(dirname(path), full.names = TRUE,
                    pattern = paste0("^", gsub(".", "\\.", paste0(entry$name, "_", entry$version), fixed = TRUE),
                                     "\\.space-[0-9a-f]+\\.qs2$")))

  ## Per-process names, so two sessions building at once never share or
  ## delete each other's files.

  raw_dir <- tempfile(paste0(".raw-", entry$name, "-", entry$version, "-"), tmpdir = dirname(path))
  on.exit(unlink(raw_dir, recursive = TRUE), add = TRUE)

  raw  <- download_sources(entry, raw_dir, verbose = verbose, call = call)
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

  lib <- structure(list(pool    = pool,
                        record  = record,
                        sources = entry$sources[, c("file", "url", "bytes", "md5")]),
                   class = "horizons_library")

  ## Write to a temporary name and rename, so an interrupted write never
  ## leaves a file the next draw would mistake for a finished library, and a
  ## failed one never leaves half a gigabyte behind.

  tmp <- tempfile(paste0(".", entry$name, "-"), tmpdir = dirname(path), fileext = ".qs2.tmp")
  on.exit(unlink(tmp), add = TRUE)
  qs2::qs_save(lib, tmp)

  if (!file.rename(tmp, path)) {

    log_warning(collector, "skipped", c(
      "The {.val {entry$name}} library was built but could not be moved into the cache, so every later draw downloads and builds it again.",
      "i" = "Rename to {.path {path}} failed; check that {.path {dirname(path)}} is writable, or set {.code options(horizons.cache_dir = )}."
    ))

    if (verbose) cat(paste0("\u251C\u2500 Library ", cli::style_bold(entry$name), " ", entry$version,
                            " built, but not cached\n"))

    lib$record["path"] <- list(NULL)

    return(list(pool = lib$pool, record = lib$record))

  }

  if (verbose) {

    cat(paste0("\u251C\u2500 Library ", cli::style_bold(entry$name), " ", entry$version,
               " built and cached at ", path, " (",
               format(round(file.size(path) / 1e6)), " MB).\n"))

  }

  prime_library_space(lib$pool, lib$record, verbose = verbose, collector = collector, debug = debug)

  list(pool = lib$pool, record = lib$record)

}



#' Remove what a killed build left behind
#'
#' @description
#' A build writes its raw downloads and its output under per-process names
#' and removes them on exit, but a process killed outright (out of memory,
#' most likely, during the build) never runs its exit handlers. Those leave
#' about 0.9 GB each, and every retry would add another set. Anything of this
#' entry's with a build name that is over a day old is removed before a new
#' build starts; a build running in another session is younger than that.
#'
#' @return The removed paths, invisibly.
#' @noRd
clear_stale_builds <- function(entry, dir, max_age_hours = 24) {

  esc     <- function(v) gsub("([.])", "\\\\\\1", v)
  pattern <- paste0("^\\.(raw-", esc(entry$name), "-", esc(entry$version), "-|",
                    esc(entry$name), "-.*\\.qs2\\.tmp$)")
  found   <- list.files(dir, pattern = pattern, all.files = TRUE, full.names = TRUE)

  age   <- difftime(Sys.time(), file.mtime(found), units = "hours")
  stale <- found[!is.na(age) & age > max_age_hours]

  unlink(stale, recursive = TRUE)
  invisible(stale)

}

#' Ask before a registered library's first download
#'
#' @description
#' Always prints what is about to happen, its size and where it goes, and
#' the credit. Proceeds without asking when `horizons.library_download` is
#' `TRUE`; otherwise asks in an interactive session and stops in a
#' non-interactive one, naming the option.
#'
#' @param call The frame a refusal reports: the verb's.
#' @return `NULL`, invisibly, once consent is given; aborts with
#'   `horizons_error_consent` otherwise.
#' @noRd
library_consent <- function(entry, path, ask = interactive(), call = rlang::caller_env()) {

  mb <- round(sum(entry$sources$bytes) / 1e6)

  cli::cli_inform(c(
    "i" = "The {.val {entry$name}} library is not cached yet.",
    " " = "horizons will download {mb} MB of public source files, verify them, and build the library in {.path {dirname(path)}}. The raw downloads are removed once it is built.",
    " " = "",
    " " = entry$credit,
    " " = "",
    " " = "Please cite: {entry$citation} ({entry$license}).",
    " " = ""
  ))

  if (isTRUE(getOption("horizons.library_download"))) return(invisible(NULL))

  if (!isTRUE(ask)) {

    abort_horizons(c(
      "Not downloading {.val {entry$name}} without consent.",
      "i" = "Set {.code options(horizons.library_download = TRUE)} to allow it in a non-interactive session."
    ), kind = "input", class = "horizons_error_consent", call = call)

  }

  answer <- utils::askYesNo("Download and build it now?", default = FALSE)

  if (!isTRUE(answer)) {

    abort_horizons("Download of {.val {entry$name}} declined; nothing was written.",
                   kind = "input", class = "horizons_error_consent", call = call)

  }

  invisible(NULL)

}


#' Download and verify a registry entry's source files
#'
#' @param dir [Character.] The directory to download into, inside the cache
#'   directory [resolve_registered()] has made.
#' @param call The frame a refusal reports: the verb's.
#'
#' @return Named character vector of local paths, named by `table`.
#' @noRd
download_sources <- function(entry, dir, verbose = TRUE, call = rlang::caller_env()) {

  dir.create(dir, showWarnings = FALSE)

  ## The default 60 s timeout cannot move 400 MB.

  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)

  src   <- entry$sources
  paths <- stats::setNames(file.path(dir, src$file), src$table)

  for (i in seq_len(nrow(src))) {

    if (verbose) cat(paste0("\u251C\u2500 Downloading ", src$file[i], "...\n"))

    failed <- function(why, parent = NULL) {
      abort_horizons(c(
        "Could not download {.file {src$file[i]}}.",
        "x" = "{why}",
        "i" = "Source: {.url {src$url[i]}}.",
        "i" = "Check the connection and run the call again; nothing was cached."
      ), kind = "run", class = "horizons_error_download", parent = parent, call = call)
    }

    status <- rlang::try_fetch(
      utils::download.file(src$url[i], paths[[i]], mode = "wb", quiet = TRUE),
      error = function(e) failed(conditionMessage(e), parent = e)
    )

    if (!identical(as.integer(status), 0L)) failed(paste("status", status))

    md5 <- unname(tools::md5sum(paths[[i]]))

    if (!identical(md5, src$md5[i])) {

      abort_horizons(c(
        "{.file {src$file[i]}} does not match its published checksum.",
        "x" = "MD5 {md5}, expected {src$md5[i]}.",
        "i" = "The source may have changed or the download was corrupted; nothing was cached."
      ), kind = "run", class = "horizons_error_download", call = call)

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
#' The operational definition of an OSSL-derived library, with the
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

  ## OSSL publishes gzipped CSV. fread() reads .gz only when R.utils is
  ## installed, which horizons does not depend on, so decompress with base R
  ## into the raw directory (the cache's volume, not the session tempdir).

  raw <- vapply(raw, decompress_gz, character(1))

  ## Site: the upper depth of every layer ------------------------------------

  ## fread() for speed, then plain data.frames: the package is not
  ## data.table-aware, so `[` must not be data.table's.

  site <- read_ossl_table(raw[["site"]], c(f$id_col, f$depth_col))
  site <- site[!duplicated(site[[f$id_col]]), , drop = FALSE]
  names(site) <- c("sample_id", "upper_depth_cm")

  ## MIR: check the grid on the header before reading the body ---------------

  hdr       <- names(data.table::fread(raw[["mir"]], nrows = 0))
  spec_cols <- grep(f$spectral_pattern, hdr, value = TRUE)
  need      <- c(f$id_col, f$dataset_col, f$instrument_col)

  wn        <- as.integer(sub(f$spectral_pattern, "\\1", spec_cols))
  keep      <- wn >= f$wn_min & wn <= f$wn_max
  spec_cols <- spec_cols[keep][order(wn[keep])]
  wn        <- sort(wn[keep])

  ## The files are MD5-verified against the registry, so this holds for the
  ## release the entry pins; a column missing from the grid would otherwise
  ## shift every wavenumber name past it.

  if (!identical(wn, seq.int(f$wn_min, f$wn_max, by = f$wn_step))) {

    abort_horizons(
      "The MIR table's grid is not {f$wn_min} to {f$wn_max} by {f$wn_step} cm-1 ({length(wn)} columns found).",
      kind = "internal"
    )

  }

  ## The MIR table holds every OSSL dataset. Reading it whole peaked near
  ## 7 GB, so choose the rows from the three filter columns first, then read
  ## the spectra in chunks and keep only those rows.

  meta <- read_ossl_table(raw[["mir"]], need)
  rows <- which(meta[[f$dataset_col]] %in% f$dataset_value &
                meta[[f$instrument_col]] %in% f$instrument_value)
  rows <- rows[!duplicated(meta[[f$id_col]][rows])]

  spec    <- read_mir_rows(raw[["mir"]], hdr, rows, meta[[f$id_col]][rows], f$id_col, spec_cols)
  mir_ids <- meta[[f$id_col]][rows]
  rm(meta)

  complete <- rowSums(!is.finite(spec)) == 0L
  mir_ids  <- mir_ids[complete]
  spec     <- spec[complete, , drop = FALSE]

  ## Lab: the mapped properties present in this release ----------------------

  lab_hdr <- names(data.table::fread(raw[["lab"]], nrows = 0))
  pm      <- entry$properties[entry$properties$source %in% lab_hdr, , drop = FALSE]
  lab     <- read_ossl_table(raw[["lab"]], c(f$id_col, pm$source))
  lab     <- lab[!duplicated(lab[[f$id_col]]), , drop = FALSE]
  names(lab) <- c("sample_id", pm$property)

  ## Join on id: spectra with a lab row and a site row -----------------------

  ids <- sort(Reduce(intersect, list(mir_ids, lab$sample_id, site$sample_id)))

  ## The registry's filters select the rows the release it pins holds; an
  ## empty join would otherwise build a library of no rows.

  if (!length(ids)) {

    abort_horizons("No rows survive the filters and the join.", kind = "internal")

  }

  at   <- match(ids, mir_ids)
  spec <- spec[at, , drop = FALSE]
  colnames(spec) <- paste0("wn_", wn)

  df <- data.frame(sample_id      = ids,
                   upper_depth_cm = site$upper_depth_cm[match(ids, site$sample_id)],
                   spec, check.names = FALSE, stringsAsFactors = FALSE)
  rm(spec)

  lab <- lab[match(ids, lab$sample_id), , drop = FALSE]

  ## standardize() on the native grid moves no values; it records the axis
  ## and how it was standardized, which the draw compares with the targets'.

  build <- function() {

    pool <- spectra(df, id_col = "sample_id")
    pool <- standardize(pool, resample = f$wn_step, trim = c(f$wn_min, f$wn_max),
                        baseline = isTRUE(f$baseline))
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


#' Decompress a .gz file next to itself with base R
#'
#' @description
#' The source is left as it is. A registered library's build decompresses
#' into the per-process raw directory under the cache, which is removed
#' whole when the build ends, so nothing is left behind.
#'
#' @return The path to read: the decompressed file, or `path` unchanged when
#'   it is not gzipped.
#' @noRd
decompress_gz <- function(path) {

  if (!grepl("\\.gz$", path)) return(path)

  out <- sub("\\.gz$", "", path)
  inp <- gzfile(path, "rb")
  on.exit(close(inp), add = TRUE)
  con <- file(out, "wb")
  on.exit(close(con), add = TRUE)

  repeat {

    chunk <- readBin(inp, what = "raw", n = 16L * 1024L^2)
    if (!length(chunk)) break
    writeBin(chunk, con)

  }

  out

}


#' Read the spectra of chosen rows of the MIR table, a chunk at a time
#'
#' @param rows Integer data-row numbers (1 is the first row after the
#'   header), increasing.
#' @param ids The id of each chosen row, as the first pass read it; every
#'   chunk's ids are checked against it, so a misaligned read stops rather
#'   than pairing spectra with the wrong lab rows.
#'
#' @return Numeric matrix, `length(rows)` by `length(spec_cols)`, columns in
#'   `spec_cols` order.
#' @noRd
read_mir_rows <- function(path, hdr, rows, ids, id_col, spec_cols, chunk = 5000L) {

  cols <- sort(match(c(id_col, spec_cols), hdr))
  out  <- matrix(NA_real_, nrow = length(rows), ncol = length(spec_cols))

  n_rows <- max(rows)
  starts <- seq.int(1L, n_rows, by = chunk)

  for (a in starts) {

    b    <- min(a + chunk - 1L, n_rows)
    want <- which(rows >= a & rows <= b)

    if (!length(want)) next

    x  <- read_mir_chunk(path, a, b, hdr, cols)
    at <- rows[want] - a + 1L

    ## A chunk read out of order would pair spectra with the wrong lab rows,
    ## silently

    if (!identical(as.character(x[[id_col]][at]), as.character(ids[want]))) {

      abort_horizons("The MIR table's rows {a} to {b} did not read back in the order the first pass saw.",
                     kind = "internal")

    }

    out[want, ] <- as.matrix(x[at, spec_cols, drop = FALSE])

  }

  out

}


#' Read one chunk of the MIR table's data rows
#'
#' @description
#' The one place the chunked read calls `data.table::fread()`, so a test can
#' rebind it and reach the row-order check in [read_mir_rows()].
#'
#' @param a,b [Integer.] The first and last data rows (1 is the first row
#'   after the header).
#' @param hdr [Character.] The table's header.
#' @param cols [Integer.] The columns to read, by position.
#'
#' @return `data.frame` of rows `a` to `b`, named by `hdr[cols]`.
#' @noRd
read_mir_chunk <- function(path, a, b, hdr, cols) {

  x <- as.data.frame(data.table::fread(path, skip = a, nrows = b - a + 1L, header = FALSE,
                                       select = cols, showProgress = FALSE))
  names(x) <- hdr[cols]
  x

}
