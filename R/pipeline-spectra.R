# R/pipeline-spectra.R
# Entry point for the horizons pipeline. Loads spectral data from OPUS files,
# CSV files, or pre-loaded tibbles into a horizons_data object.

## =============================================================================
## Section 1: Constants and ID Aliases
## =============================================================================

#' ID column aliases in priority order
#' @noRd
ID_ALIASES <- c(
  "sample_id", "Sample_ID", "SampleID", "sampleid",
  "id", "ID", "Id",
  "sample", "Sample",
  "filename", "Filename",
  "name", "Name"
)

#' The file types spectra() reads
#' @noRd
SPECTRA_TYPES <- c("opus", "csv")


## =============================================================================
## Section 2: Main Entry Point
## =============================================================================


## ---------------------------------------------------------------------------
## spectra() — Load spectral data and create horizons_data object
## ---------------------------------------------------------------------------

#' Load spectral data from files or tibble
#'
#' @description
#' Entry point for the horizons pipeline. Loads spectral data from OPUS files,
#' CSV files, or accepts a pre-loaded tibble/data.frame. Returns a `horizons_data`
#' object ready for downstream processing.
#'
#' @details
#' This function handles three input types:
#'
#' * **OPUS files** (`type = "opus"`): Read via `opusreader2::read_opus()`.
#'   Filenames become `sample_id`, wavenumber columns get `wn_` prefix. The
#'   absorbance block is taken by name, `ab` and then `ab_no_atm_comp`
#'   (ignoring case), never the first block on the file's list.
#'
#' * **CSV files** (`type = "csv"`): Read via `readr::read_csv()`.
#'   Auto-detects ID and wavelength columns.
#'
#' * **Tibble/data.frame**: Same detection logic as CSV.
#'
#' The `filename` column (role: meta) preserves original filenames for traceability
#' and later ID parsing via `parse_ids()`.
#'
#' **Several files:** `source` may be a character vector of files and
#' directories, all OPUS or all CSV; a directory is read as the files of
#' that type in it, and every file's rows are returned in one object. Each
#' file is read on its own. One that cannot be read, or that holds no
#' absorbance block, is dropped, and the files dropped are named, with their
#' causes, in one `horizons_warning_partial_failure`, recorded on the
#' object's `log`. When no file can be read the call aborts with
#' `horizons_error_read_failed`, one row per file in its `results`. A
#' warning `opusreader2` or `readr` raises while reading is passed on, once
#' per distinct message, as a `horizons_warning_quality`.
#'
#' **Axes that drift:** scans from different sessions on one instrument can
#' carry wavenumbers that differ in the last digits. A file whose every
#' wavenumber, within the range of those read before it, lies within 1% of
#' its own step of one of them takes their names, so the files bind into one
#' set of columns; only the names move, never the values, and the files
#' moved are named in one `horizons_warning_substitution`. A file at another
#' resolution keeps all its own names.
#'
#' **Wavenumber order:** Predictor columns are sorted into decreasing
#' wavenumber order before the object is returned, whatever order the source
#' gave them (a KSSL-shaped library, for one, is stored increasing). Names
#' and values travel together through the sort; only the column order
#' changes. The console reports it as a "Sorting" step when it happens.
#'
#' **Duplicate or missing sample ids:** Bruker's own OPUS naming can collide
#' at this stage — `S100-1.0` and `S100-1.1` both become `S100-1` once the
#' extension is stripped — and, for CSV or tibble input, a blank or missing
#' cell in the detected id column leaves a `sample_id` of `NA`. Both are
#' legitimate before replicate scans are collapsed, so `spectra()` passes a
#' duplicate or `NA` `sample_id` silently; use [average()] to collapse
#' replicates before anything that requires unique ids (`average()` itself,
#' `add_response()`, `configure()`, and onward, which refuse the object).
#'
#' @param source A data frame, or a character vector of paths, each a file
#'   or a directory (see Details).
#' @param type Character. One of `"opus"`, `"csv"`, or `NULL` for auto-detect.
#'   Default: `NULL`.
#' @param id_col Character. Column name to use as sample ID. If `NULL`,
#'   auto-detects from common aliases. Default: `NULL`.
#' @param wavelength_cols Character vector. Column names containing spectral data.
#'   If `NULL`, auto-detects numeric column names or `wn_*` prefix. Default: `NULL`.
#' @param recursive Logical. For directory paths, search subdirectories.
#'   Default: `FALSE`.
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("horizons.verbose", TRUE)`, as every verb's.
#'
#' @return A `horizons_data` S3 object with:
#'   * `data$analysis`: tibble with `sample_id`, `filename`, `wn_*` columns, and
#'     any additional columns
#'   * `data$role_map`: tibble mapping columns to roles
#'   * `provenance`: source and creation metadata
#'
#' @examples
#' \dontrun{
#' # OPUS files
#' spectra("path/to/opus/")
#'
#' # CSV with auto-detection
#' spectra("data.csv")
#'
#' # Tibble with explicit columns
#' spectra(my_data, id_col = "SampleName")
#' }
#'
#' @export
spectra <- function(source,
                    type            = NULL,
                    id_col          = NULL,
                    wavelength_cols = NULL,
                    recursive       = FALSE,
                    verbose         = getOption("horizons.verbose", TRUE)) {

  ## -------------------------------------------------------------------------
  ## Step 1: Input validation
  ## -------------------------------------------------------------------------

  if (missing(source) || is.null(source)) {

    abort_horizons("{.arg source} must be provided.", kind = "input")

  }

  check_arg(rlang::check_bool, verbose)

  ## Determine input type ----------------------------------------------------

  is_path   <- is.character(source) && length(source) >= 1
  is_tibble <- inherits(source, "data.frame")

  if (!is_path && !is_tibble) {

    abort_horizons(
      "{.arg source} must be a path or a data frame, not {.obj_type_friendly {source}}.",
      kind = "input"
    )

  }

  if (!is.null(type) && (!rlang::is_string(type) || !tolower(type) %in% SPECTRA_TYPES)) {

    abort_horizons(c(
      "{.arg type} must be one of {.val {SPECTRA_TYPES}}, or {.code NULL} to detect it.",
      "x" = "Got {.val {type}}."
    ), kind = "input")

  }

  ## Every path must exist, checked once before anything is read --------------

  if (is_path) {

    absent <- source[!file.exists(source)]

    if (length(absent) > 0) {

      abort_horizons(
        "{length(absent)} path{?s} in {.arg source} {?does/do} not exist: {.path {absent}}.",
        kind = "input"
      )

    }

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Read the source
  ## -------------------------------------------------------------------------
  ## A data frame is one table. A path is a vector of files and directories,
  ## and each file is read as a unit of its own: one that cannot be read is
  ## dropped and named in one partial_failure warning, and the rest are
  ## returned.

  call      <- rlang::current_env()
  collector <- log_open("spectra")

  if (is_tibble) {

    tables      <- list(tibble = spectra_from_tibble(source, id_col, wavelength_cols, call = call))
    source_type <- "tibble"
    source_name <- "tibble"

  } else {

    read        <- spectra_from_path(source, type, id_col, wavelength_cols, recursive,
                                     collector = collector, call = call)
    tables      <- read$tables
    source_type <- read$type
    source_name <- source

  }

  analysis <- bind_spectra_files(tables, collector)
  wn_cols  <- attr(analysis, "wn_cols")

  ### The bind's working record of the wavelength columns is not the table's
  attr(analysis, "wn_cols") <- NULL

  result <- new_horizons_data(
    analysis       = analysis,
    role_map       = build_role_map(
      id_col     = "sample_id",
      wn_cols    = wn_cols,
      meta_cols  = setdiff(names(analysis), c("sample_id", wn_cols)),
      other_cols = character()
    ),
    spectra_source = source_name,
    spectra_type   = source_type
  )

  ## -------------------------------------------------------------------------
  ## Step 2b: Put the axis in decreasing order, then validate
  ## -------------------------------------------------------------------------
  ## Every dispatch path above keeps columns in whatever order the source
  ## gave them (a KSSL-shaped library is stored increasing). standardize()
  ## and the validator both assume decreasing order, so sort here the same
  ## way standardize()'s no-op path does, rather than rejecting increasing
  ## input that `spectra() |> standardize()` is documented to accept.
  ##
  ## Raw stage: replicate scans haven't been collapsed by average() yet, so a
  ## duplicate sample_id here (Bruker's own naming does this: "S100-1.0" and
  ## "S100-1.1" both become "S100-1" once the extension is stripped) passes.

  ## `.flag` is the steps' record of a row they could not use, and the
  ## validators let non-finite predictors through in flagged rows, so a source
  ## column of that name is refused rather than read as one.
  if (".flag" %in% names(result$data$analysis)) {

    abort_horizons(c(
      "The source has a column named {.field .flag}, which horizons reserves for the rows its steps flag.",
      "i" = "Rename the column before reading it with {.fn spectra}."
    ), kind = "input")

  }

  sorted_result <- sort_axis_decreasing(result)
  result        <- validate_horizons_data(sorted_result$x, stage = "raw")
  result$log    <- log_close(collector, result$log)

  ## -------------------------------------------------------------------------
  ## Step 3: CLI output
  ## -------------------------------------------------------------------------
  ## Replicate scans share an id until average() collapses them, so the count
  ## says how many scans sit under how many ids, and how many have none.

  if (verbose) {

    ids    <- result$data$analysis$sample_id
    n_rows <- result$data$n_rows
    n_pred <- result$data$n_predictors
    n_na   <- sum(is.na(ids))
    n_ids  <- length(unique(ids[!is.na(ids)]))

    lines <- c(
      if (n_ids < n_rows - n_na) {
        cli::format_inline("{n_rows} scans under {n_ids} sample id{?s} \u00D7 {n_pred} predictors")
      } else {
        paste0(n_rows, " samples \u00D7 ", n_pred, " predictors")
      },
      if (n_na > 0) cli::format_inline("{n_na} scan{?s} with no sample id ({.code NA})"),
      if (sorted_result$sorted) "Sorting: wavenumber columns put in decreasing order"
    )

    cat(paste0("\u2500\u2500 ", cli::style_bold("horizons pipeline"),
               " ", paste(rep("\u2500", 46), collapse = ""), "\n"))
    cat(paste0("\u251C\u2500 ", cli::style_bold("Loading spectra"), "...\n"))

    branch <- ifelse(seq_along(lines) < length(lines), "\u251C\u2500", "\u2514\u2500")
    cat(paste0("\u2502  ", branch, " ", lines, "\n"), sep = "")
    cat("\u2502\n")

  }

  signal_log(collector, result)

  result

}


## =============================================================================
## Section 3: Reading the source
## =============================================================================


## ---------------------------------------------------------------------------
## spectra_from_tibble() — The table of a data frame
## ---------------------------------------------------------------------------

#' The spectra table of a data frame
#'
#' @description
#' A data frame given to `spectra()`, or one CSV file read: its id column as
#' `sample_id`, its other columns as meta, and its wavelength columns with the
#' `wn_` prefix.
#'
#' @param data A tibble or data.frame with spectral data.
#' @param id_col Optional column name for sample ID.
#' @param wavelength_cols Optional column names for wavelength data.
#' @param call The frame a refusal reports: `spectra()`'s.
#'
#' @return A tibble: `sample_id`, the meta columns, then the wavelength
#'   columns, whose names are its attribute `"wn_cols"`.
#'
#' @noRd
spectra_from_tibble <- function(data,
                                id_col          = NULL,
                                wavelength_cols = NULL,
                                call            = rlang::caller_env()) {

  ## The id and wavelength columns, checked -----------------------------------

  cols      <- check_source_columns(data, id_col, wavelength_cols, call = call)
  id_column <- cols$id_column
  wn_cols   <- cols$wn_cols

  ## Meta columns: everything except the id and the wavelengths --------------

  meta_cols <- setdiff(names(data), c(id_column, wn_cols))

  ## The table ----------------------------------------------------------------

  analysis <- tibble::tibble(sample_id = as.character(data[[id_column]]))

  for (col in meta_cols) {

    analysis[[col]] <- data[[col]]

  }

  wn_names_new <- ensure_wn_prefix(wn_cols)

  for (i in seq_along(wn_cols)) {

    analysis[[wn_names_new[i]]] <- data[[wn_cols[i]]]

  }

  attr(analysis, "wn_cols") <- wn_names_new

  analysis

}


## ---------------------------------------------------------------------------
## spectra_from_path() — Read files, each as a unit
## ---------------------------------------------------------------------------

#' Read the files a path vector names, each as a unit of its own
#'
#' @description
#' Every element of `source` is a file or a directory; a directory is read
#' as the files of the type in it (OPUS by their `.0`, `.1`, ... extension,
#' CSV by `.csv`, subdirectories with `recursive = TRUE`). The files are all
#' of one type: `type` when given, else detected element by element, and a
#' source whose elements detect differently is refused.
#'
#' Each file is read in its own [run_unit()]. A file that cannot be read, or
#' holds no absorbance block, fails its unit and is dropped, and the files
#' dropped are named in one `partial_failure` warning; when none can be read
#' the call aborts with `horizons_error_read_failed`, one row per file in
#' its `results`. A refusal of the user's columns (a CSV's `id_col`) is an
#' input error, which no unit contains. Another package's warnings raised
#' while reading are each collected once as a `quality` warning.
#'
#' @param source Character. Paths, which `spectra()` has checked exist.
#' @param type `spectra()`'s `type`, checked, or `NULL`.
#' @param id_col,wavelength_cols `spectra()`'s arguments, for CSV files.
#' @param recursive Logical. Search subdirectories.
#' @param collector `spectra()`'s collector, from [log_open()].
#' @param call The frame a refusal reports: `spectra()`'s.
#'
#' @return List: `type` (`"opus"` or `"csv"`) and `tables`, one per file
#'   read, as [spectra_from_tibble()] returns them.
#'
#' @noRd
spectra_from_path <- function(source,
                              type            = NULL,
                              id_col          = NULL,
                              wavelength_cols = NULL,
                              recursive       = FALSE,
                              collector,
                              call            = rlang::caller_env()) {

  ## -------------------------------------------------------------------------
  ## Step 1: One type for every element
  ## -------------------------------------------------------------------------

  types <- if (is.null(type)) {
    vapply(source, detect_file_type, character(1), call = call, USE.NAMES = FALSE)
  } else {
    rep(tolower(type), length(source))
  }

  if (length(unique(types)) > 1) {

    abort_horizons(c(
      "{.arg source} mixes OPUS and CSV files; {.fn spectra} reads one type per call.",
      "i" = "OPUS: {.path {source[types == 'opus']}}.",
      "i" = "CSV: {.path {source[types == 'csv']}}."
    ), kind = "input", call = call)

  }

  type  <- types[1]
  label <- if (type == "opus") "OPUS" else "CSV"

  ## -------------------------------------------------------------------------
  ## Step 2: The files, a directory's included
  ## -------------------------------------------------------------------------

  pattern <- if (type == "opus") "\\.[0-9]+$" else "\\.csv$"

  file_paths <- unlist(lapply(source, function(path) {

    if (!dir.exists(path)) return(path)

    found <- list.files(path, pattern = pattern, full.names = TRUE,
                        recursive = recursive, ignore.case = type == "csv")

    if (length(found) == 0) {

      abort_horizons(c(
        "No {label} files found in directory.",
        "i" = "Path: {.path {path}}.",
        "i" = if (type == "opus") "OPUS files end with .0, .1, etc." else "CSV files end with .csv."
      ), kind = "input", call = call)

    }

    found

  }))

  if (type == "opus" && !requireNamespace("opusreader2", quietly = TRUE)) {

    abort_horizons(c(
      "Package {.pkg opusreader2} required for OPUS files.",
      "i" = "Install it with {.code install.packages('opusreader2')}."
    ), kind = "dependency", call = call)

  }

  ## -------------------------------------------------------------------------
  ## Step 3: Each file a unit
  ## -------------------------------------------------------------------------

  n_files <- length(file_paths)
  units   <- vector("list", n_files)

  cli::cli_progress_bar(
    format = "Reading {n_files} {label} files {cli::pb_bar} {cli::pb_current}/{n_files} ({cli::pb_percent})",
    total  = n_files
  )

  for (i in seq_along(file_paths)) {

    units[[i]] <- run_unit(
      if (type == "opus") {
        read_opus_file(file_paths[[i]])
      } else {
        read_csv_file(file_paths[[i]], id_col, wavelength_cols, call = call)
      },
      unit = "file",
      id   = file_paths[[i]]
    )

    cli::cli_progress_update()

  }

  cli::cli_progress_done()

  ## -------------------------------------------------------------------------
  ## Step 4: What the units reported
  ## -------------------------------------------------------------------------

  records <- dplyr::bind_rows(new_unit_records(), lapply(units, `[[`, "records"))
  failed  <- vapply(units, `[[`, logical(1), "failed")

  failures <- records[records$type == "failure", , drop = FALSE]
  results  <- tibble::tibble(
    file  = failures$id,
    step  = failures$step,
    class = failures$class,
    cause = vapply(seq_len(nrow(failures)), function(i) {
      unit_failure_message(failures[i, , drop = FALSE], outside = "Reading")
    }, character(1))
  )
  bullets <- distinct_config_errors(tibble::tibble(config_id = results$file,
                                                   error_message = results$cause))

  if (all(failed)) {

    abort_horizons(c(
      "None of the {n_files} {label} {cli::qty(n_files)}file{?s} in {.arg source} could be read.",
      bullets,
      "i" = "The failures are on this condition as {.field results}: {.code rlang::last_error()$results}."
    ), kind = "run", class = "horizons_error_read_failed", results = results, call = call)

  }

  if (any(failed)) {

    n_failed <- sum(failed)

    log_warning(collector, "partial_failure", c(
      "{n_failed} of {n_files} {label} {cli::qty(n_files)}file{?s} could not be read, so {cli::qty(n_failed)}{?it was/they were} dropped.",
      bullets
    ), ids = results$file)

  }

  log_unit_records(collector, records)
  log_foreign_warnings(collector, records, while_doing = "reading")

  list(type = type,
       tables = stats::setNames(lapply(units[!failed], `[[`, "value"), file_paths[!failed]))

}


## =============================================================================
## Section 4: File Readers
## =============================================================================


## ---------------------------------------------------------------------------
## read_opus_file() — One OPUS file's absorbance spectrum
## ---------------------------------------------------------------------------

#' Read one OPUS file's absorbance spectrum
#'
#' @description
#' Runs inside the file's unit. The absorbance block is taken by name, `ab`
#' and then `ab_no_atm_comp`, ignoring case (#191): instruments write either,
#' beside single-channel, interferogram and parameter blocks
#' (`ab_no_atm_comp_data_param` among them), so the first block on the list,
#' or the first whose name starts with `ab`, can be the wrong one. A file
#' with neither, or whose block is in a form horizons does not read, fails
#' its unit naming what it holds.
#'
#' @param path Character(1). The file.
#'
#' @return A one-row tibble: the wavelength columns (`wn_` names, its
#'   attribute `"wn_cols"`), any other columns the block carries,
#'   `sample_id` and `filename` (the file's name without its extension).
#'
#' @noRd
read_opus_file <- function(path) {

  opus_obj <- unit_step("file_read", opusreader2::read_opus(path))

  ## read_opus() returns a list keyed by file, then the blocks within it -----

  if (length(opus_obj) == 1 && is.list(opus_obj[[1]])) {

    opus_obj <- opus_obj[[1]]

  }

  row_data <- unit_step("file_block", {

    blocks <- names(opus_obj)
    pick   <- match(c("ab", "ab_no_atm_comp"), tolower(blocks))
    pick   <- pick[!is.na(pick)]

    if (length(pick) == 0) {

      abort_horizons(c(
        "The file holds no absorbance block ({.val ab} or {.val ab_no_atm_comp}).",
        "i" = "Its blocks: {.val {blocks}}."
      ), kind = "run")

    }

    ab_name  <- blocks[pick[1]]
    ab_block <- opus_obj[[pick[1]]]

    ## opusreader2 blocks are lists with $data (a 1-row matrix) and
    ## $wavenumbers
    if (is.list(ab_block) && "data" %in% names(ab_block)) {

      row_data <- tibble::as_tibble(ab_block$data)

      ### Wavenumber names when the matrix was unnamed or auto-named (V1, ...)
      if (!is.null(ab_block$wavenumbers)) {

        unnamed <- is.null(names(row_data)) || all(grepl("^V\\d+$", names(row_data)))

        if (unnamed && length(ab_block$wavenumbers) == ncol(row_data)) {
          names(row_data) <- as.character(ab_block$wavenumbers)
        }

      }

    } else if (inherits(ab_block, "data.frame")) {

      row_data <- tibble::as_tibble(ab_block)

    } else if (is.numeric(ab_block)) {

      row_data <- tibble::as_tibble(t(ab_block))

    } else {

      abort_horizons(
        "The absorbance block {.val {ab_name}} is a {.cls {class(ab_block)}}, which horizons does not read.",
        kind = "run"
      )

    }

    row_data

  })

  ## Wavenumber columns are the names that read as one ------------------------

  other_cols <- names(row_data)
  wn_cols    <- other_cols[!is.na(wn_values(other_cols))]
  wn_new     <- ensure_wn_prefix(wn_cols)

  names(row_data)[match(wn_cols, names(row_data))] <- wn_new

  name <- tools::file_path_sans_ext(basename(path))

  row_data$sample_id <- name
  row_data$filename  <- name

  attr(row_data, "wn_cols") <- wn_new

  row_data

}


## ---------------------------------------------------------------------------
## read_csv_file() — One CSV file's table
## ---------------------------------------------------------------------------

#' Read one CSV file's spectra
#'
#' @description
#' Runs inside the file's unit: a file `readr` cannot read fails it. Its
#' columns are checked as a data frame's are ([spectra_from_tibble()]), and
#' a refusal there is an input error, which no unit contains.
#'
#' @param path Character(1). The file.
#' @param id_col,wavelength_cols `spectra()`'s arguments.
#' @param call The frame a refusal reports: `spectra()`'s.
#'
#' @return The file's table, as [spectra_from_tibble()] returns it.
#'
#' @noRd
read_csv_file <- function(path, id_col = NULL, wavelength_cols = NULL, call = rlang::caller_env()) {

  data <- unit_step("file_read", readr::read_csv(path, show_col_types = FALSE))

  spectra_from_tibble(data, id_col, wavelength_cols, call = call)

}


## ---------------------------------------------------------------------------
## bind_spectra_files() — The files' tables as one, on one grid
## ---------------------------------------------------------------------------

#' Bind the files' tables into one, putting drifted axes on one grid
#'
#' @description
#' The one place the tables `spectra()` read are bound, whatever their type.
#' Rows keep the files' order; a wavenumber one file lacks is `NA` in its
#' rows.
#'
#' Before the bind, each file's wavenumbers are matched to the grid read so
#' far, the first file's axis to start (#224). A file whose every point
#' inside the grid's range lies within `WN_SNAP_FRACTION` of its own median
#' step of a grid point has drifted as a whole: those points take the grid's
#' names, and its points beyond the grid join it. Any other file keeps all
#' its names. Only names move, never values, so scans from sessions whose
#' axes differ in the last digits bind into one column set, and a file at
#' another resolution, whose points meet the grid's only here and there,
#' keeps its own. The files
#' whose names moved are collected as one `substitution` warning with the
#' largest distance moved.
#'
#' @param tables Named list of tables, as [spectra_from_tibble()] and
#'   [read_opus_file()] return them, named by file.
#' @param collector `spectra()`'s collector, from [log_open()].
#'
#' @return A tibble: `sample_id`, the meta columns, then the wavelength
#'   columns, whose names are its attribute `"wn_cols"`.
#'
#' @noRd
bind_spectra_files <- function(tables, collector) {

  ## -------------------------------------------------------------------------
  ## Step 1: Each file's wavenumbers on the grid read before it
  ## -------------------------------------------------------------------------

  grid_names  <- character(0)
  grid_values <- numeric(0)
  moved       <- character(0)
  max_drift   <- 0

  for (i in seq_along(tables)) {

    own    <- attr(tables[[i]], "wn_cols")
    values <- wn_values(own)
    named  <- own

    if (length(grid_values) > 0 && length(values) > 1) {

      tol <- WN_SNAP_FRACTION * stats::median(abs(diff(sort(values))))

      ### The nearest grid point to each of the file's, by its two neighbours
      ### on the sorted grid
      ord     <- order(grid_values)
      sorted  <- grid_values[ord]
      at      <- findInterval(values, sorted, all.inside = length(sorted) > 1)
      lower   <- pmax(at, 1L)
      upper   <- pmin(at + 1L, length(sorted))
      nearest <- ifelse(abs(values - sorted[lower]) <= abs(values - sorted[upper]), lower, upper)
      drift   <- abs(values - sorted[nearest])
      target  <- grid_names[ord][nearest]

      ### A file is moved only when its whole axis drifted: every point inside
      ### the grid's range has a grid point that close. A file at another
      ### resolution meets the grid only here and there, and keeps its names.
      inside <- values >= min(sorted) - tol & values <= max(sorted) + tol
      whole  <- all(drift[inside] <= tol)

      ### A point takes a grid name only when it is that close, the name is
      ### not already one of the file's own, and no other point of the file
      ### takes it
      snap        <- whole & drift <= tol & !target %in% own
      snap[snap]  <- !duplicated(target[snap])

      if (any(snap)) {

        named[snap] <- target[snap]
        moved       <- c(moved, names(tables)[i])
        max_drift   <- max(max_drift, drift[snap])

        names(tables[[i]])[match(own[snap], names(tables[[i]]))] <- target[snap]

      }

    }

    new         <- !named %in% grid_names
    grid_names  <- c(grid_names, named[new])
    grid_values <- c(grid_values, values[new])

    attr(tables[[i]], "wn_cols") <- named

  }

  if (length(moved) > 0) {

    n_moved <- length(moved)
    drift   <- signif(max_drift, 3)
    pct     <- 100 * WN_SNAP_FRACTION

    log_warning(collector, "substitution", c(
      "{n_moved} file{?s} had wavenumbers within {pct}% of a step of those read before {?it/them}, and {?its/their} columns were put on the same grid.",
      "i" = "Only names moved, by at most {drift} cm-1; the values are as read.",
      "i" = "{cli::qty(n_moved)}File{?s}: {shown_ids(moved)}."
    ), ids = moved)

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Bind
  ## -------------------------------------------------------------------------

  bound <- dplyr::bind_rows(unname(tables))
  meta  <- setdiff(names(bound), c("sample_id", grid_names))
  bound <- bound[, c("sample_id", meta, grid_names), drop = FALSE]

  attr(bound, "wn_cols") <- grid_names

  bound

}


## =============================================================================
## Section 5: Helper Functions
## =============================================================================


## ---------------------------------------------------------------------------
## check_source_columns() — The id and wavelength columns of a table
## ---------------------------------------------------------------------------

#' The id and wavelength columns of a source table, checked
#'
#' @description
#' The one door both table loaders (a data frame, a CSV file) use for the
#' user's `id_col` and `wavelength_cols`: the columns named must exist, the
#' wavelength columns must be named by wavenumber (#121), and they must
#' hold numbers. Either is detected when not
#' given ([detect_id_column()], [detect_wavelength_columns()]). Refusals
#' report as `spectra()`.
#'
#' @param data The source table.
#' @param id_col,wavelength_cols `spectra()`'s arguments.
#' @param call The frame a refusal reports: `spectra()`'s.
#' @return List: `id_column` (character(1)) and `wn_cols` (character), names
#'   of `data`.
#' @keywords internal
#' @noRd
check_source_columns <- function(data, id_col, wavelength_cols, call = rlang::caller_env()) {

  ## The id column ------------------------------------------------------------

  if (!is.null(id_col)) {

    if (!id_col %in% names(data)) {

      abort_horizons(c(
        "Specified {.arg id_col} not found in data.",
        "x" = "Column {.val {id_col}} does not exist.",
        "i" = "Available columns: {.val {names(data)}}."
      ), kind = "input", call = call)

    }

    id_column <- id_col

  } else {

    id_column <- detect_id_column(names(data), call = call)

  }

  ## The wavelength columns ---------------------------------------------------

  if (!is.null(wavelength_cols)) {

    missing_cols <- setdiff(wavelength_cols, names(data))

    if (length(missing_cols) > 0) {

      abort_horizons(c(
        "Specified wavelength columns not found in data.",
        "x" = "Missing: {.val {missing_cols}}."
      ), kind = "input", call = call)

    }

    wn_cols <- wavelength_cols

  } else {

    wn_cols <- detect_wavelength_columns(names(data), call = call)

  }

  ## Their names are wavenumbers (#121) ----------------------------------------
  ## The axis is read from the names, and the validator refuses a predictor
  ## whose name it cannot read, so the source is refused here, naming them.

  unnamed <- wn_cols[is.na(wn_values(wn_cols))]

  if (length(unnamed) > 0) {

    abort_horizons(c(
      "Wavelength column names must be wavenumbers.",
      "x" = "Not a wavenumber: {.val {unnamed}}.",
      "i" = "Name each by its wavenumber, as {.val 4000} or {.val wn_4000}, or leave it out of {.arg wavelength_cols}."
    ), kind = "input", call = call)

  }

  ## Spectra are numbers ------------------------------------------------------

  non_numeric_wn <- wn_cols[!vapply(data[wn_cols], is.numeric, logical(1))]

  if (length(non_numeric_wn) > 0) {

    abort_horizons(c(
      "Wavelength columns must contain numeric data.",
      "x" = "Non-numeric columns: {.val {non_numeric_wn}}.",
      "i" = "Check that the spectral columns hold absorbance or reflectance values."
    ), kind = "input", call = call)

  }

  list(id_column = id_column, wn_cols = wn_cols)

}


## ---------------------------------------------------------------------------
## detect_file_type() — Auto-detect file type from path
## ---------------------------------------------------------------------------

#' Detect file type from path
#'
#' @param source File or directory path, which `spectra()` has checked
#'   exists.
#' @param call The frame a refusal reports: `spectra()`'s.
#'
#' @return Character: "opus" or "csv".
#'
#' @noRd
detect_file_type <- function(source, call = rlang::caller_env()) {

  if (dir.exists(source)) {

    ## Check for OPUS files --------------------------------------------------

    opus_files <- list.files(source, pattern = "\\.[0-9]+$")
    csv_files  <- list.files(source, pattern = "\\.csv$", ignore.case = TRUE)

    if (length(opus_files) > 0) {

      return("opus")

    } else if (length(csv_files) > 0) {

      return("csv")

    }

    ### Naming the type cannot help: neither loader finds a file here.
    abort_horizons(c(
      "{.arg source} holds no OPUS (.0, .1, ...) or CSV files.",
      "i" = "Directory: {.path {source}}."
    ), kind = "input", call = call)

  }

  if (grepl("\\.[0-9]+$", source)) {

    return("opus")

  } else if (grepl("\\.csv$", source, ignore.case = TRUE)) {

    return("csv")

  }

  abort_horizons(c(
    "Could not detect the type of {.arg source} from its extension.",
    "i" = "File: {.path {source}}.",
    "i" = "Specify {.arg type} explicitly."
  ), kind = "input", call = call)

}


## ---------------------------------------------------------------------------
## detect_id_column() — Find ID column from aliases
## ---------------------------------------------------------------------------

#' Detect ID column from common aliases
#'
#' @param col_names Character vector of column names.
#' @param call The frame a refusal reports: `spectra()`'s.
#'
#' @return Character: detected column name.
#'
#' @noRd
detect_id_column <- function(col_names, call = rlang::caller_env()) {

  for (alias in ID_ALIASES) {

    if (alias %in% col_names) {

      return(alias)

    }

  }

  abort_horizons(c(
    "Could not identify sample ID column.",
    "i" = "Checked aliases: {.val {ID_ALIASES}}.",
    "i" = "Available columns: {.val {col_names}}.",
    "i" = "Specify it with {.arg id_col}."
  ), kind = "input", call = call)

}


## ---------------------------------------------------------------------------
## detect_wavelength_columns() — Find wavelength columns
## ---------------------------------------------------------------------------

#' Detect wavelength columns from column names
#'
#' @param col_names Character vector of column names.
#' @param call The frame a refusal reports: `spectra()`'s.
#'
#' @return Character vector of wavelength column names.
#'
#' @noRd
detect_wavelength_columns <- function(col_names, call = rlang::caller_env()) {

  ## Strategy 1: Columns starting with wn_ -----------------------------------

  wn_prefixed <- col_names[grepl("^wn_", col_names)]

  if (length(wn_prefixed) > 0) {

    return(wn_prefixed)

  }

  ## Strategy 2: Columns with numeric names (wavenumbers) --------------------
  ## Using regex instead of as.numeric() for cleaner detection --------------

  numeric_pattern <- "^[0-9]+(\\.[0-9]+)?$"
  numeric_cols    <- col_names[grepl(numeric_pattern, col_names)]

  if (length(numeric_cols) > 0) {

    return(numeric_cols)

  }

  abort_horizons(c(
    "Could not identify wavelength columns.",
    "i" = "Expected columns starting with {.val wn_} or numeric names (e.g. {.val 4000}, {.val 3998}).",
    "i" = "Found columns: {.val {col_names}}.",
    "i" = "Specify them with {.arg wavelength_cols}."
  ), kind = "input", call = call)

}


## ---------------------------------------------------------------------------
## ensure_wn_prefix() — Add wn_ prefix to column names
## ---------------------------------------------------------------------------

#' Ensure column names have wn_ prefix
#'
#' @param col_names Character vector of column names.
#'
#' @return Character vector with wn_ prefix.
#'
#' @noRd
ensure_wn_prefix <- function(col_names) {

  ifelse(
    grepl("^wn_", col_names),
    col_names,
    paste0("wn_", col_names)
  )

}

#' The wavenumbers a set of column names stand for
#'
#' @description
#' The one parser for wavenumber column names. A name is read as a number
#' once its `wn_` prefix, if it has one, is set aside (`"wn_600"` and
#' `"600"` are both 600, `"wn_600.5"` is 600.5). A name that is not a number
#' (`"Sample_ID"`, `"wn_"`) is `NA`, with no warning: the callers ask which
#' columns are wavenumbers, so a name that is not one is an answer, not a
#' problem.
#'
#' @param col_names Character.
#' @return Numeric, one per name.
#' @keywords internal
#' @noRd
wn_values <- function(col_names) {

  text   <- sub("^wn_", "", as.character(col_names))
  number <- grepl("^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$", text)

  out         <- rep(NA_real_, length(text))
  out[number] <- as.numeric(text[number])

  out

}


## ---------------------------------------------------------------------------
## build_role_map() — Create role mapping tibble
## ---------------------------------------------------------------------------

#' Build role mapping tibble
#'
#' @param id_col ID column name.
#' @param wn_cols Wavelength column names.
#' @param meta_cols Metadata column names.
#' @param other_cols Other column names.
#'
#' @return A tibble with variable and role columns.
#'
#' @noRd
build_role_map <- function(id_col,
                           wn_cols,
                           meta_cols,
                           other_cols) {

  tibble::tibble(
    variable = c(id_col, meta_cols, wn_cols, other_cols),
    role     = c(
      "id",
      rep("meta", length(meta_cols)),
      rep("predictor", length(wn_cols)),
      rep("meta", length(other_cols))
    )
  )

}


## =============================================================================
## Section 6: S3 Methods
## =============================================================================
##
## NOTE: print.horizons_data() and summary.horizons_data() live in class-core.R
## (the canonical location for all S3 methods on horizons_data).
## Do NOT define them here — duplicate definitions cause collation-dependent
## dispatch bugs.
