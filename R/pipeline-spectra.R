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
#'   Filenames become `sample_id`, wavenumber columns get `wn_` prefix.
#'
#' * **CSV files** (`type = "csv"`): Read via `readr::read_csv()`.
#'   Auto-detects ID and wavelength columns.
#'
#' * **Tibble/data.frame**: Same detection logic as CSV.
#'
#' The `filename` column (role: meta) preserves original filenames for traceability
#' and later ID parsing via `parse_ids()`.
#'
#' @param source Path to directory, file(s), or a tibble/data.frame.
#' @param type Character. One of `"opus"`, `"csv"`, or `NULL` for auto-detect.
#'   Default: `NULL`.
#' @param id_col Character. Column name to use as sample ID. If `NULL`,
#'   auto-detects from common aliases. Default: `NULL`.
#' @param wavelength_cols Character vector. Column names containing spectral data.
#'   If `NULL`, auto-detects numeric column names or `wn_*` prefix. Default: `NULL`.
#' @param recursive Logical. For directory paths, search subdirectories.
#'   Default: `FALSE`.
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
                    recursive       = FALSE) {

  ## -------------------------------------------------------------------------
  ## Step 1: Input validation
  ## -------------------------------------------------------------------------

  if (missing(source) || is.null(source)) {

    cli::cli_abort("{.arg source} must be provided")

  }

  ## Determine input type ----------------------------------------------------

  is_path   <- is.character(source) && length(source) >= 1
  is_tibble <- inherits(source, "data.frame")

  if (!is_path && !is_tibble) {

    cli::cli_abort(c(
      "{.arg source} must be a path or data frame",
      "x" = "Got {.cls {class(source)}}"
    ))

  }

  ## Reject multi-element character vectors -----------------------------------
  ## spectra() expects a single path (file or directory), not a vector of paths

  if (is_path && length(source) > 1) {

    cli::cli_abort(c(
      "{.arg source} must be a single path, not a vector of length {length(source)}",
      "i" = "Provide a directory path, or a single file path"
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Dispatch based on input type
  ## -------------------------------------------------------------------------

  if (is_tibble) {

    result <- spectra_from_tibble(
      data            = source,
      id_col          = id_col,
      wavelength_cols = wavelength_cols
    )

  } else if (is_path) {

    result <- spectra_from_path(
      source          = source,
      type            = type,
      id_col          = id_col,
      wavelength_cols = wavelength_cols,
      recursive       = recursive
    )

  }

  ## -------------------------------------------------------------------------
  ## Step 3: CLI output
  ## -------------------------------------------------------------------------

  cat(paste0("\u2500\u2500 ", cli::style_bold("horizons pipeline"),
             " ", paste(rep("\u2500", 46), collapse = ""), "\n"))
  cat(paste0("\u251C\u2500 ", cli::style_bold("Loading spectra"), "...\n"))
  cat(paste0("\u2502  \u2514\u2500 ", result$data$n_rows, " samples \u00D7 ",
             result$data$n_predictors, " predictors\n"))
  cat("\u2502\n")

  result

}


## =============================================================================
## Section 3: Internal Dispatch Functions
## =============================================================================


## ---------------------------------------------------------------------------
## spectra_from_tibble() — Process pre-loaded data
## ---------------------------------------------------------------------------

#' Process tibble/data.frame into horizons_data
#'
#' @param data A tibble or data.frame with spectral data.
#' @param id_col Optional column name for sample ID.
#' @param wavelength_cols Optional column names for wavelength data.
#'
#' @return A horizons_data object.
#'
#' @noRd
spectra_from_tibble <- function(data,
                                id_col          = NULL,
                                wavelength_cols = NULL) {

  ## -------------------------------------------------------------------------
  ## Step 1: Detect ID column
  ## -------------------------------------------------------------------------

  if (!is.null(id_col)) {

    if (!id_col %in% names(data)) {

      cli::cli_abort(c(
        "Specified {.arg id_col} not found in data",
        "x" = "Column {.val {id_col}} does not exist",
        "i" = "Available columns: {.val {names(data)}}"
      ))

    }

    id_column <- id_col

  } else {

    id_column <- detect_id_column(names(data))

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Detect wavelength columns
  ## -------------------------------------------------------------------------

  if (!is.null(wavelength_cols)) {

    missing_cols <- setdiff(wavelength_cols, names(data))

    if (length(missing_cols) > 0) {

      cli::cli_abort(c(
        "Specified wavelength columns not found in data",
        "x" = "Missing: {.val {missing_cols}}"
      ))

    }

    wn_cols <- wavelength_cols

  } else {

    wn_cols <- detect_wavelength_columns(names(data))

  }

  ## -------------------------------------------------------------------------
  ## Step 3: Validate wavelength columns contain numeric data
  ## -------------------------------------------------------------------------

  non_numeric_wn <- character()

  for (col in wn_cols) {

    if (!is.numeric(data[[col]])) {

      non_numeric_wn <- c(non_numeric_wn, col)

    }

  }

  if (length(non_numeric_wn) > 0) {

    cli::cli_abort(c(
      "Wavelength columns must contain numeric data",
      "x" = "Non-numeric columns: {.val {non_numeric_wn}}",
      "i" = "Check that spectral data columns contain absorbance/reflectance values"
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 4: Build analysis tibble
  ## -------------------------------------------------------------------------

  ## Identify meta columns (everything except ID and wavelengths) ------------

  meta_cols <- setdiff(names(data), c(id_column, wn_cols))

  ## Rename wavelength columns to wn_* format if needed ----------------------

  wn_names_new <- ensure_wn_prefix(wn_cols)
  names_map    <- stats::setNames(wn_cols, wn_names_new)

  ## Build analysis tibble ---------------------------------------------------

  ## Start with sample_id column
  analysis <- tibble::tibble(sample_id = as.character(data[[id_column]]))

  ## No filename column for tibble input (no actual files) -------------------

  ## Add meta columns --------------------------------------------------------

  for (col in meta_cols) {

    analysis[[col]] <- data[[col]]

  }

  ## Add wavelength columns with wn_ prefix ----------------------------------

  for (i in seq_along(wn_cols)) {

    analysis[[wn_names_new[i]]] <- data[[wn_cols[i]]]

  }

  ## -------------------------------------------------------------------------
  ## Step 5: Build role_map
  ## -------------------------------------------------------------------------

  role_map <- build_role_map(
    id_col     = "sample_id",
    wn_cols    = wn_names_new,
    meta_cols  = meta_cols,
    other_cols = character()
  )

  ## -------------------------------------------------------------------------
  ## Step 6: Build horizons_data object
  ## -------------------------------------------------------------------------

  create_horizons_data(
    analysis       = analysis,
    role_map       = role_map,
    spectra_source = "tibble",
    spectra_type   = "tibble"
  )

}


## ---------------------------------------------------------------------------
## spectra_from_path() — Load from file path
## ---------------------------------------------------------------------------

#' Load spectral data from file path
#'
#' @param source Path to file or directory.
#' @param type File type ("opus", "csv", or NULL for auto-detect).
#' @param id_col Optional column name for sample ID.
#' @param wavelength_cols Optional column names for wavelength data.
#' @param recursive Search subdirectories.
#'
#' @return A horizons_data object.
#'
#' @noRd
spectra_from_path <- function(source,
                              type            = NULL,
                              id_col          = NULL,
                              wavelength_cols = NULL,
                              recursive       = FALSE) {

  ## -------------------------------------------------------------------------
  ## Step 1: Determine file type
  ## -------------------------------------------------------------------------

  if (is.null(type)) {

    type <- detect_file_type(source)

  }

  type <- tolower(type)

  if (!type %in% c("opus", "csv")) {

    cli::cli_abort(c(
      "{.arg type} must be 'opus' or 'csv'",
      "x" = "Got {.val {type}}"
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Dispatch to type-specific loader
  ## -------------------------------------------------------------------------

  if (type == "opus") {

    load_opus_files(
      source          = source,
      id_col          = id_col,
      wavelength_cols = wavelength_cols,
      recursive       = recursive
    )

  } else if (type == "csv") {

    load_csv_files(
      source          = source,
      id_col          = id_col,
      wavelength_cols = wavelength_cols
    )

  }

}


## =============================================================================
## Section 4: File Loaders
## =============================================================================


## ---------------------------------------------------------------------------
## load_opus_files() — Read OPUS binary files
## ---------------------------------------------------------------------------

#' Load spectral data from OPUS files
#'
#' @param source Path to OPUS file or directory.
#' @param id_col Not used for OPUS (ID comes from filename).
#' @param wavelength_cols Optional wavelength columns.
#' @param recursive Search subdirectories.
#'
#' @return A horizons_data object.
#'
#' @noRd
load_opus_files <- function(source,
                            id_col          = NULL,
                            wavelength_cols = NULL,
                            recursive       = FALSE) {

  ## -------------------------------------------------------------------------
  ## Step 1: Find OPUS files
  ## -------------------------------------------------------------------------

  if (dir.exists(source)) {

    file_paths <- list.files(
      path       = source,
      pattern    = "\\.[0-9]+$",
      full.names = TRUE,
      recursive  = recursive
    )

    if (length(file_paths) == 0) {

      cli::cli_abort(c(
        "No OPUS files found in directory",
        "i" = "Path: {.path {source}}",
        "i" = "OPUS files end with .0, .1, etc."
      ))

    }

  } else if (file.exists(source)) {

    file_paths <- source

  } else {

    cli::cli_abort(c(
      "Path does not exist",
      "x" = "{.path {source}}"
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Read OPUS files
  ## -------------------------------------------------------------------------

  ## Check opusreader2 is available ------------------------------------------

  if (!requireNamespace("opusreader2", quietly = TRUE)) {

    cli::cli_abort(c(
      "Package {.pkg opusreader2} required for OPUS files",
      "i" = "Install with: {.code install.packages('opusreader2')}"
    ))

  }

  ## Read all files ----------------------------------------------------------

  n_files      <- length(file_paths)
  n_failed     <- 0L
  spectra_list <- vector("list", n_files)

  cli::cli_progress_bar(
    format = "Reading {n_files} OPUS files {cli::pb_bar} {cli::pb_current}/{n_files} ({cli::pb_percent})",
    total  = n_files
  )

  for (i in seq_along(file_paths)) {

    spectra_list[[i]] <- tryCatch({

      suppressWarnings(opusreader2::read_opus(file_paths[[i]]))

    }, error = function(e) {

      n_failed <<- n_failed + 1L
      cli::cli_warn("Failed to read {.path {file_paths[[i]]}}: {e$message}")
      return(NULL)

    })

    cli::cli_progress_update()

  }

  cli::cli_progress_done()

  ## Filter out failures -----------------------------------------------------

  valid_idx    <- !vapply(spectra_list, is.null, logical(1))
  spectra_list <- spectra_list[valid_idx]
  file_paths   <- file_paths[valid_idx]

  if (length(spectra_list) == 0) {

    cli::cli_abort("No OPUS files could be read successfully")

  }

  ## -------------------------------------------------------------------------
  ## Step 3: Extract spectra and build tibble
  ## -------------------------------------------------------------------------

  ## Extract filenames (without extension) for sample_id ---------------------

  filenames <- tools::file_path_sans_ext(basename(file_paths))

  ## Extract absorbance data from each file ----------------------------------

  spectra_data <- lapply(seq_along(spectra_list), function(i) {

    opus_obj <- spectra_list[[i]]

    ## Unwrap filename-level nesting from opusreader2 -------------------------
    ## read_opus() returns list keyed by filename, then blocks within each file

    if (length(opus_obj) == 1 && is.list(opus_obj[[1]])) {

      opus_obj <- opus_obj[[1]]

    }

    ## Get AB block (absorbance) ---------------------------------------------

    ab_block <- NULL

    if ("ab" %in% names(opus_obj)) {

      ab_block <- opus_obj$ab

    } else if ("AB" %in% names(opus_obj)) {

      ab_block <- opus_obj$AB

    } else {

      ## Try first block that looks like spectral data
      metadata_names <- c("basic_metadata", "instrument_ref", "optik_ref",
                          "acquisition_ref")
      param_pattern  <- "_data_param$"

      data_blocks <- names(opus_obj)[
        !names(opus_obj) %in% metadata_names &
        !grepl(param_pattern, names(opus_obj))
      ]

      if (length(data_blocks) == 0) {

        cli::cli_warn("No data blocks found in {.path {file_paths[i]}}")
        return(NULL)

      }

      ab_block <- opus_obj[[data_blocks[1]]]

    }

    ## Extract spectral matrix from block ------------------------------------
    ## opusreader2 blocks are lists with $data (1-row matrix) and $wavenumbers

    if (is.list(ab_block) && "data" %in% names(ab_block)) {

      row_data <- tibble::as_tibble(ab_block$data)

      ## Assign wavenumber names if matrix was unnamed or auto-named (V1, V2...)
      if (!is.null(ab_block$wavenumbers)) {

        unnamed <- is.null(names(row_data)) ||
          all(grepl("^V\\d+$", names(row_data)))

        if (unnamed && length(ab_block$wavenumbers) == ncol(row_data)) {
          names(row_data) <- as.character(ab_block$wavenumbers)
        }

      }

    } else if (inherits(ab_block, "data.frame")) {

      row_data <- tibble::as_tibble(ab_block)

    } else if (is.numeric(ab_block)) {

      row_data <- tibble::as_tibble(t(ab_block))

    } else {

      cli::cli_warn("Unexpected data format in {.path {file_paths[i]}}")
      return(NULL)

    }

    ## Add sample_id and filename --------------------------------------------

    row_data$sample_id <- filenames[i]
    row_data$filename  <- filenames[i]

    row_data

  })

  ## Filter NULLs and combine ------------------------------------------------

  spectra_data <- spectra_data[!vapply(spectra_data, is.null, logical(1))]

  if (length(spectra_data) == 0) {

    cli::cli_abort("No valid spectral data could be extracted")

  }

  combined <- dplyr::bind_rows(spectra_data)

  ## -------------------------------------------------------------------------
  ## Step 4: Organize columns
  ## -------------------------------------------------------------------------

  ## Identify wavelength columns (numeric names) -----------------------------

  all_cols   <- names(combined)
  id_cols    <- c("sample_id", "filename")
  other_cols <- setdiff(all_cols, id_cols)

  ## Detect numeric column names (wavenumbers) -------------------------------

  numeric_names <- suppressWarnings(as.numeric(other_cols))
  wn_cols       <- other_cols[!is.na(numeric_names)]
  meta_cols     <- setdiff(other_cols, wn_cols)

  ## Add wn_ prefix to wavelength columns ------------------------------------

  wn_cols_new <- paste0("wn_", wn_cols)

  names(combined)[names(combined) %in% wn_cols] <- wn_cols_new

  ## Reorder columns: sample_id, filename, meta, wn_* ------------------------

  col_order <- c("sample_id", "filename", meta_cols, wn_cols_new)
  combined  <- combined[, col_order, drop = FALSE]

  ## -------------------------------------------------------------------------
  ## Step 5: Build role_map and create object
  ## -------------------------------------------------------------------------

  role_map <- build_role_map(
    id_col     = "sample_id",
    wn_cols    = wn_cols_new,
    meta_cols  = c("filename", meta_cols),
    other_cols = character()
  )

  create_horizons_data(
    analysis       = combined,
    role_map       = role_map,
    spectra_source = source,
    spectra_type   = "opus"
  )

}


## ---------------------------------------------------------------------------
## load_csv_files() — Read CSV files
## ---------------------------------------------------------------------------

#' Load spectral data from CSV file
#'
#' @param source Path to CSV file.
#' @param id_col Optional column name for sample ID.
#' @param wavelength_cols Optional wavelength columns.
#'
#' @return A horizons_data object.
#'
#' @noRd
load_csv_files <- function(source,
                           id_col          = NULL,
                           wavelength_cols = NULL) {

  ## -------------------------------------------------------------------------
  ## Step 1: Validate path
  ## -------------------------------------------------------------------------

  if (!file.exists(source)) {

    cli::cli_abort(c(
      "CSV file not found",
      "x" = "{.path {source}}"
    ))

  }

  if (dir.exists(source)) {

    cli::cli_abort(c(
      "Expected CSV file, got directory",
      "x" = "{.path {source}}",
      "i" = "Provide path to a single CSV file"
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Read CSV
  ## -------------------------------------------------------------------------

  data <- readr::read_csv(source, show_col_types = FALSE)

  ## -------------------------------------------------------------------------
  ## Step 3: Detect ID column
  ## -------------------------------------------------------------------------

  if (!is.null(id_col)) {

    if (!id_col %in% names(data)) {

      cli::cli_abort(c(
        "Specified {.arg id_col} not found in data",
        "x" = "Column {.val {id_col}} does not exist",
        "i" = "Available: {.val {names(data)}}"
      ))

    }

    id_column <- id_col

  } else {

    id_column <- detect_id_column(names(data))

  }

  ## -------------------------------------------------------------------------
  ## Step 4: Detect wavelength columns
  ## -------------------------------------------------------------------------

  if (!is.null(wavelength_cols)) {

    missing_cols <- setdiff(wavelength_cols, names(data))

    if (length(missing_cols) > 0) {

      cli::cli_abort(c(
        "Specified wavelength columns not found",
        "x" = "Missing: {.val {missing_cols}}"
      ))

    }

    wn_cols <- wavelength_cols

  } else {

    wn_cols <- detect_wavelength_columns(names(data))

  }

  ## -------------------------------------------------------------------------
  ## Step 5: Validate wavelength columns contain numeric data
  ## -------------------------------------------------------------------------

  non_numeric_wn <- character()

  for (col in wn_cols) {

    if (!is.numeric(data[[col]])) {

      non_numeric_wn <- c(non_numeric_wn, col)

    }

  }

  if (length(non_numeric_wn) > 0) {

    cli::cli_abort(c(
      "Wavelength columns must contain numeric data",
      "x" = "Non-numeric columns: {.val {non_numeric_wn}}",
      "i" = "Check that spectral data columns contain absorbance/reflectance values"
    ))

  }

  ## -------------------------------------------------------------------------
  ## Step 6: Build analysis tibble
  ## -------------------------------------------------------------------------

  ## Meta columns (everything except ID and wavelengths) ---------------------

  meta_cols <- setdiff(names(data), c(id_column, wn_cols))

  ## Rename wavelength columns to wn_* format --------------------------------

  wn_names_new <- ensure_wn_prefix(wn_cols)

  ## Build analysis tibble ---------------------------------------------------

  analysis <- tibble::tibble(sample_id = as.character(data[[id_column]]))

  ## No filename column for CSV input (no per-sample files) ------------------

  ## Add meta columns --------------------------------------------------------

  for (col in meta_cols) {

    analysis[[col]] <- data[[col]]

  }

  ## Add wavelength columns with wn_ prefix ----------------------------------

  for (i in seq_along(wn_cols)) {

    analysis[[wn_names_new[i]]] <- data[[wn_cols[i]]]

  }

  ## -------------------------------------------------------------------------
  ## Step 7: Build role_map and create object
  ## -------------------------------------------------------------------------

  role_map <- build_role_map(
    id_col     = "sample_id",
    wn_cols    = wn_names_new,
    meta_cols  = meta_cols,
    other_cols = character()
  )

  create_horizons_data(
    analysis       = analysis,
    role_map       = role_map,
    spectra_source = source,
    spectra_type   = "csv"
  )

}


## =============================================================================
## Section 5: Helper Functions
## =============================================================================


## ---------------------------------------------------------------------------
## detect_file_type() — Auto-detect file type from path
## ---------------------------------------------------------------------------

#' Detect file type from path
#'
#' @param source File or directory path.
#'
#' @return Character: "opus" or "csv".
#'
#' @noRd
detect_file_type <- function(source) {

  if (dir.exists(source)) {

    ## Check for OPUS files --------------------------------------------------

    opus_files <- list.files(source, pattern = "\\.[0-9]+$")
    csv_files  <- list.files(source, pattern = "\\.csv$", ignore.case = TRUE)

    if (length(opus_files) > 0) {

      return("opus")

    } else if (length(csv_files) > 0) {

      return("csv")

    } else {

      cli::cli_abort(c(
        "Could not detect file type in directory",
        "i" = "No OPUS (.0, .1, ...) or CSV files found",
        "i" = "Specify {.arg type} explicitly"
      ))

    }

  } else if (file.exists(source)) {

    if (grepl("\\.[0-9]+$", source)) {

      return("opus")

    } else if (grepl("\\.csv$", source, ignore.case = TRUE)) {

      return("csv")

    } else {

      cli::cli_abort(c(
        "Could not detect file type",
        "i" = "File: {.path {source}}",
        "i" = "Specify {.arg type} explicitly"
      ))

    }

  } else {

    cli::cli_abort("Path does not exist: {.path {source}}")

  }

}


## ---------------------------------------------------------------------------
## detect_id_column() — Find ID column from aliases
## ---------------------------------------------------------------------------

#' Detect ID column from common aliases
#'
#' @param col_names Character vector of column names.
#'
#' @return Character: detected column name.
#'
#' @noRd
detect_id_column <- function(col_names) {

  for (alias in ID_ALIASES) {

    if (alias %in% col_names) {

      return(alias)

    }

  }

  cli::cli_abort(c(
    "Could not identify sample ID column",
    "i" = "Checked aliases: {.val {ID_ALIASES}}",
    "i" = "Available columns: {.val {col_names}}",
    "i" = "Specify explicitly with {.arg id_col}"
  ))

}


## ---------------------------------------------------------------------------
## detect_wavelength_columns() — Find wavelength columns
## ---------------------------------------------------------------------------

#' Detect wavelength columns from column names
#'
#' @param col_names Character vector of column names.
#'
#' @return Character vector of wavelength column names.
#'
#' @noRd
detect_wavelength_columns <- function(col_names) {

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

  cli::cli_abort(c(
    "Could not identify wavelength columns",
    "i" = "Expected: columns starting with 'wn_' or numeric names (e.g., '4000', '3998')",
    "i" = "Found columns: {.val {col_names}}",
    "i" = "Specify explicitly with {.arg wavelength_cols}"
  ))

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


## ---------------------------------------------------------------------------
## create_horizons_data() — Create horizons_data S3 object
## ---------------------------------------------------------------------------

#' Create horizons_data S3 object
#'
#' @param analysis Analysis tibble.
#' @param role_map Role mapping tibble.
#' @param spectra_source Source path or "tibble".
#' @param spectra_type Source type ("opus", "csv", "tibble").
#'
#' @return A horizons_data S3 object.
#'
#' @noRd
create_horizons_data <- function(analysis,
                                 role_map,
                                 spectra_source,
                                 spectra_type) {

  ## Count predictors and covariates -----------------------------------------

  n_predictors <- sum(role_map$role == "predictor")
  n_covariates <- sum(role_map$role == "covariate")

  ## Build object ------------------------------------------------------------

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = nrow(analysis),
      n_predictors = n_predictors,
      n_covariates = n_covariates
    ),

    provenance = list(
      spectra_source       = spectra_source,
      spectra_type         = spectra_type,
      response_source      = NULL,
      ossl_properties      = NULL,
      created              = Sys.time(),
      horizons_version     = utils::packageVersion("horizons"),
      schema_version       = 1L,
      preprocessing        = NULL,
      preprocessing_params = list(),
      id_pattern           = NULL,
      aggregation_by       = NULL
    ),

    config = list(
      configs   = NULL,
      n_configs = NULL,
      tuning    = list(
        grid_size     = 10,
        bayesian_iter = 15,
        cv_folds      = 5
      )
    ),

    validation = list(
      passed    = NULL,
      checks    = NULL,
      outliers  = list(
        spectral_ids   = NULL,
        response_ids   = NULL,
        removed_ids    = NULL,
        removal_detail = NULL,
        removed        = FALSE
      ),
      timestamp = NULL
    ),

    evaluation = list(
      results     = NULL,
      best_config = NULL,
      rank_metric = NULL,
      backend     = NULL,
      runtime     = NULL,
      timestamp   = NULL
    ),

    models = list(
      workflows = NULL,
      n_models  = NULL,
      uq        = list(
        enabled         = FALSE,
        quantile_models = NULL,
        conformal       = NULL,
        ad_metadata     = NULL
      )
    ),

    ensemble = list(
      stack   = NULL,
      method  = NULL,
      weights = NULL,
      metrics = NULL
    ),

    artifacts = list(
      cv_preds = list(
        path  = NULL,
        index = NULL
      ),
      fit_objects = list(
        path  = NULL,
        index = NULL
      ),
      cache_dir = NULL
    )
  )

  ## Set class ---------------------------------------------------------------

  class(obj) <- c("horizons_data", "list")

  obj

}


## =============================================================================
## Section 6: S3 Methods
## =============================================================================
##
## NOTE: print.horizons_data() and summary.horizons_data() live in class-core.R
## (the canonical location for all S3 methods on horizons_data).
## Do NOT define them here — duplicate definitions cause collation-dependent
## dispatch bugs.
