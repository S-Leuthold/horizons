# R/pipeline-spectra.R
# Entry point for loading spectral data into the horizons pipeline


## =============================================================================
## Helper: detect_input_type()
## =============================================================================

#' Detect the type of spectral input
#'
#' @description
#' Determines whether the source is a tibble, CSV file, or OPUS file(s).
#' Used internally by `spectra()` to route to the appropriate reader.
#'
#' @param source File path (character) or data.frame/tibble
#' @param type Explicit type override: NULL (auto-detect), "opus", or "csv"
#'
#' @return Character: "tibble", "csv", or "opus"
#'
#' @noRd

detect_input_type <- function(source, type = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: If it's already a data.frame, it's a tibble input
  ## ---------------------------------------------------------------------------

  if (is.data.frame(source)) {

    return("tibble")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Must be a character path from here on
  ## ---------------------------------------------------------------------------

  if (!is.character(source) || length(source) != 1) {

    cat(cli::col_red(cli::style_bold("! Invalid source:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 Expected a file path or data.frame\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 Got: ", class(source)[1], "\n")))
    cat("\n")
    rlang::abort("Invalid source", class = "horizons_input_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: If user explicitly specified type, trust them
  ## ---------------------------------------------------------------------------

  if (!is.null(type)) {

    type <- match.arg(type, c("opus", "csv"))
    return(type)

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Check if path exists
  ## ---------------------------------------------------------------------------

  if (!file.exists(source)) {

    cat(cli::col_red(cli::style_bold("! Path not found:\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 ", source, " does not exist\n")))
    cat("\n")
    rlang::abort("Path not found", class = "horizons_input_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Handle directories
  ## ---------------------------------------------------------------------------

  if (dir.exists(source)) {

    files <- list.files(source, full.names = FALSE)

    if (length(files) == 0) {

      cat(cli::col_red(cli::style_bold("! Empty directory:\n")))
      cat(cli::col_red(paste0("   \u2514\u2500 No files found in ", source, "\n")))
      cat("\n")
      rlang::abort("Empty directory", class = "horizons_input_error")

    }

    has_csv  <- any(grepl("\\.csv$", files, ignore.case = TRUE))
    has_opus <- any(grepl("\\.[0-9]+$", files))

    if (has_csv && has_opus) {

      cat(cli::col_red(cli::style_bold("! Mixed file types in directory:\n")))
      cat(cli::col_red(paste0("   \u251C\u2500 Found both CSV and OPUS files in ", source, "\n")))
      cat(cli::col_red(paste0("   \u2514\u2500 Specify type = 'opus' or type = 'csv' explicitly\n")))
      cat("\n")
      rlang::abort("Mixed file types", class = "horizons_input_error")

    }

    if (has_csv) {

      cat(cli::col_red(cli::style_bold("! Directory of CSV files not supported:\n")))
      cat(cli::col_red(paste0("   \u251C\u2500 Found CSV files in ", source, "\n")))
      cat(cli::col_red(paste0("   \u2514\u2500 For CSV input, provide the file path directly\n")))
      cat("\n")
      rlang::abort("CSV directory not supported", class = "horizons_input_error")

    }

    if (has_opus) {

      return("opus")

    }

    ## No recognized spectral files --------------------------------------------

    cat(cli::col_red(cli::style_bold("! No spectral files found:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 Directory ", source, " contains no OPUS files\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 OPUS files have extensions like .0, .1, .2\n")))
    cat("\n")
    rlang::abort("No spectral files found", class = "horizons_input_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Handle single files — .csv means CSV, otherwise assume OPUS
  ## ---------------------------------------------------------------------------

  if (grepl("\\.csv$", tolower(source))) {

    return("csv")

  } else {

    return("opus")

  }

}


## =============================================================================
## Helper: read_csv_spectra()
## =============================================================================

#' Read CSV file containing spectral data
#'
#' @description
#' Reads a CSV file and returns a tibble with spectral columns coerced to
#' numeric. Non-spectral columns are preserved with their inferred types.
#'
#' @details
#' This is a thin wrapper around `readr::read_csv()` that enforces numeric
#' typing for columns that appear to be spectral data (column names that are
#' purely numeric like `"4000"` or have the `wn_` prefix like `"wn_4000"`).
#'
#' All other columns (sample IDs, response variables, metadata) are preserved
#' with whatever type readr infers. If readr types a response column as
#' character (e.g., because it contains `"<LOD"` values), that's a data quality
#' issue for the user to resolve upstream.
#'
#' The function assumes:
#' - Comma-delimited files (not TSV, semicolon, etc.)
#' - UTF-8 encoding
#' - File fits in memory
#'
#' @param path [character.] Path to a `.csv` file.
#'
#' @return [tibble.] The CSV contents with spectral columns forced to numeric.
#'
#' @seealso [read_opus_files()] for OPUS binary files, [spectra()] for the
#'   user-facing entry point.
#'
#' @noRd

read_csv_spectra <- function(path) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate input
  ## ---------------------------------------------------------------------------

  ## Check file exists ---------------------------------------------------------

  if (!file.exists(path)) {

    cat(cli::col_red(cli::style_bold("! File not found:\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 ", path, " does not exist\n")))
    cat("\n")
    rlang::abort("File not found", class = "horizons_read_error")

  }

  ## Check extension is .csv ---------------------------------------------------

  if (!grepl("\\.csv$", tolower(path))) {

    cat(cli::col_red(cli::style_bold("! Invalid file type:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 Expected a .csv file\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 Got: ", basename(path), "\n")))
    cat("\n")
    rlang::abort("Invalid file type", class = "horizons_read_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Read the file
  ## ---------------------------------------------------------------------------

  data <- tryCatch(
    readr::read_csv(path,
                    show_col_types = FALSE,
                    locale         = readr::locale(encoding = "UTF-8")),
    error = function(e) {

      cat(cli::col_red(cli::style_bold("! Failed to read CSV:\n")))
      cat(cli::col_red(paste0("   \u251C\u2500 File: ", basename(path), "\n")))
      cat(cli::col_red(paste0("   \u2514\u2500 Error: ", e$message, "\n")))
      cat("\n")
      rlang::abort("CSV read failed", class = "horizons_read_error")

    }
  )

  ## Check we got data ---------------------------------------------------------

  if (nrow(data) == 0) {

    cat(cli::col_red(cli::style_bold("! Empty file:\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 ", basename(path), " contains no data rows\n")))
    cat("\n")
    rlang::abort("Empty file", class = "horizons_read_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Force spectral columns to numeric
  ## ---------------------------------------------------------------------------

  ## Identify spectral columns by name pattern ---------------------------------
  ## Matches: "4000", "3998.5", "wn_4000", "wn_3998.5"

  col_names    <- names(data)
  is_numeric_name <- grepl("^[0-9]+(\\.[0-9]+)?$", col_names)
  is_wn_prefix    <- grepl("^wn_[0-9]+(\\.[0-9]+)?$", col_names)
  spectral_cols   <- col_names[is_numeric_name | is_wn_prefix]

  ## Coerce spectral columns and track NAs -------------------------------------

  if (length(spectral_cols) > 0) {

    na_counts <- integer(length(spectral_cols))
    names(na_counts) <- spectral_cols

    for (col in spectral_cols) {

      ## Track which values were NOT NA before coercion ------------------------

      was_not_na <- !is.na(data[[col]])

      data[[col]] <- suppressWarnings(as.numeric(data[[col]]))

      ## Count values that BECAME NA (were valid before, NA after) -------------

      became_na <- was_not_na & is.na(data[[col]])
      na_counts[col] <- sum(became_na)

    }

    ## Warn if coercion introduced NAs -----------------------------------------

    cols_with_new_nas <- names(na_counts)[na_counts > 0]

    if (length(cols_with_new_nas) > 0) {

      total_new_nas <- sum(na_counts)

      cli::cli_alert_warning(
        "{total_new_nas} value{?s} coerced to NA in {length(cols_with_new_nas)} spectral column{?s} (NAs in spectra will cause validation to fail)"
      )

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Return
  ## ---------------------------------------------------------------------------

  data

}


## =============================================================================
## Helper: read_opus_files()
## =============================================================================

#' Read OPUS files into a wide tibble
#'
#' @description
#' Reads OPUS binary files from a directory or single file path and returns
#' a wide tibble with sample_id, directory, and wn_* columns.
#'
#' @details
#' Uses opusreader2 for OPUS file parsing. Automatically selects the best
#' available channel from each file using this priority order:
#'
#' 1. `ab_no_atm_comp` — Absorbance without atmospheric compensation (preferred)
#' 2. `ab` — Absorbance (may have atmospheric compensation)
#' 3. `sc_sample` — Single-channel sample spectrum
#' 4. `sc_ref` — Single-channel reference spectrum
#'
#' Absorbance channels are preferred over single-channel because that's what
#' modeling workflows need. Non-atmospheric-compensated is preferred because
#' the instrument's compensation can introduce artifacts — we handle those
#' bands in preprocessing instead.
#'
#' The function tracks which channel was used and attaches it as an attribute
#' on the returned tibble. If multiple channels were used across files (rare),
#' a warning is issued.
#'
#' @param path [character.] Path to OPUS file or directory containing OPUS files.
#' @param recursive [logical.] If TRUE (default), search subdirectories for
#'   OPUS files. Useful when files are organized by project or batch.
#'
#' @return [tibble.] Wide format with columns:
#'   - `sample_id`: Sample identifier from filename (sans extension)
#'   - `directory`: Parent directory path (useful for batch tracking)
#'   - `wn_*`: Wavenumber columns in decreasing order (e.g., wn_4000, wn_3998)
#'
#'   With attribute `channels_used` containing the channel(s) extracted.
#'
#' @seealso [spectra()] for the user-facing entry point.
#'
#' @noRd

read_opus_files <- function(path, recursive = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Find all OPUS files
  ## ---------------------------------------------------------------------------

  if (dir.exists(path)) {

    opus_files <- list.files(path,
                             pattern    = "\\.[0-9]+$",
                             full.names = TRUE,
                             recursive  = recursive)

    if (length(opus_files) == 0) {

      cat(cli::col_red(cli::style_bold("! No OPUS files found:\n")))
      cat(cli::col_red(paste0("   \u2514\u2500 Directory ", path,
                              " contains no files with .0, .1, etc. extensions\n")))
      cat("\n")
      rlang::abort("No OPUS files found", class = "horizons_read_error")

    }

  } else {

    opus_files <- path

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Read files and extract spectral data with progress
  ## ---------------------------------------------------------------------------

  channel_priority <- c("ab_no_atm_comp", "ab", "sc_sample", "sc_ref")

  n_files         <- length(opus_files)
  spectra_list    <- vector("list", n_files)
  channels_used   <- character(n_files)
  files_with_data <- 0L
  files_failed    <- 0L

  cli::cli_progress_bar(
    format = "Reading OPUS files [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent}",
    total  = n_files
  )

  for (i in seq_along(opus_files)) {

    cli::cli_progress_update()

    file_path <- opus_files[i]

    ## Read single file ----------------------------------------------------------

    file_data <- tryCatch(
      suppressWarnings(opusreader2::read_opus(dsn = file_path, progress_bar = FALSE)),
      error = function(e) NULL
    )

    if (is.null(file_data) || length(file_data) == 0) {

      files_failed <- files_failed + 1L
      next

    }

    ## Extract the file content (read_opus returns a named list) -----------------

    file_content <- file_data[[1]]

    ## Try each channel in priority order ----------------------------------------

    found_channel <- FALSE

    for (channel in channel_priority) {

      if (channel %in% names(file_content) && !is.null(file_content[[channel]]$data)) {

        spectral_data <- file_content[[channel]]$data

        spectra_list[[i]] <- tibble::tibble(
          sample_id  = tools::file_path_sans_ext(basename(file_path)),
          directory  = dirname(file_path),
          wavenumber = as.numeric(colnames(spectral_data)),
          absorbance = as.numeric(spectral_data)
        )

        channels_used[i]  <- channel
        files_with_data   <- files_with_data + 1L
        found_channel     <- TRUE
        break

      }

    }

    if (!found_channel) {

      files_failed <- files_failed + 1L

    }

  }

  cli::cli_progress_done()

  ## Report any failures ---------------------------------------------------------

  if (files_failed > 0L) {

    cli::cli_alert_warning("{files_failed} file{?s} could not be read or had no valid data")

  }

  ## Check we got at least some data ---------------------------------------------

  if (files_with_data == 0L) {

    cat(cli::col_red(cli::style_bold("! No valid spectral data found:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 Checked ", n_files, " files\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 None contained valid absorbance data\n")))
    cat("\n")
    rlang::abort("No valid spectral data", class = "horizons_read_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Combine into wide format with wn_ prefix
  ## ---------------------------------------------------------------------------

  ## Bind all spectra into single long tibble ------------------------------------

  spectra_long <- dplyr::bind_rows(spectra_list)

  ## Pivot to wide format --------------------------------------------------------

  spectra_wide <- spectra_long |>
    tidyr::pivot_wider(
      names_from   = wavenumber,
      values_from  = absorbance,
      names_prefix = "wn_"
    )

  ## Reorder columns: sample_id first, then wn_ columns in decreasing order ------

  wn_cols   <- grep("^wn_", names(spectra_wide), value = TRUE)
  wn_values <- as.numeric(gsub("^wn_", "", wn_cols))
  wn_sorted <- wn_cols[order(wn_values, decreasing = TRUE)]

  spectra_wide <- spectra_wide |>
    dplyr::select(sample_id, directory, dplyr::all_of(wn_sorted))

  ## ---------------------------------------------------------------------------
  ## Step 5: Report channel usage and return
  ## ---------------------------------------------------------------------------

  unique_channels <- unique(channels_used[channels_used != ""])

  if (length(unique_channels) == 1) {

    cli::cli_alert_info("Channel used: {.val {unique_channels}}")

  } else if (length(unique_channels) > 1) {

    cli::cli_alert_warning("Multiple channels used across files: {.val {unique_channels}}")

  }

  ## Attach channel info as attribute for provenance -----------------------------

  attr(spectra_wide, "channels_used") <- unique_channels

  spectra_wide

}


## =============================================================================
## spectra() — Main Entry Point
## =============================================================================
##
## This is the user-facing function that loads spectral data and returns a
## horizons_data object. It unifies the output from different readers (OPUS,
## CSV, tibble) into a consistent structure.
##

## ---------------------------------------------------------------------------
## Constants
## ---------------------------------------------------------------------------

#' @noRd
ID_ALIASES <- c(
 "sample_id", "Sample_ID", "SampleID", "sampleid",
 "id", "ID", "Id",
 "sample", "Sample",
 "filename", "Filename",
 "name", "Name"
)


## ---------------------------------------------------------------------------
## Helper: detect_id_column()
## ---------------------------------------------------------------------------

#' Detect sample ID column from data
#'
#' @description
#' Identifies the sample ID column using a priority-based alias list or an
#' explicit override. Returns information about what was detected for
#' reporting purposes.
#'
#' @param data [tibble.] The input data.
#' @param id_col [character or NULL.] Explicit ID column name, or NULL for
#'   auto-detection.
#'
#' @return [list.] With elements:
#'   - `id_col`: The detected/specified ID column name
#'   - `was_override`: Logical, TRUE if user specified id_col
#'   - `other_candidates`: Character vector of other matching aliases found
#'
#' @noRd
detect_id_column <- function(data, id_col = NULL) {

  col_names <- names(data)

  ## If user provided explicit override -----------------------------------------

  if (!is.null(id_col)) {

    if (!id_col %in% col_names) {

      cat(cli::col_red(cli::style_bold("! ID column not found:\n")))
      cat(cli::col_red(paste0("   \u251C\u2500 Specified: ", id_col, "\n")))
      cat(cli::col_red(paste0("   \u2514\u2500 Available: ",
                              paste(col_names[1:min(5, length(col_names))], collapse = ", "),
                              if (length(col_names) > 5) ", ..." else "", "\n")))
      cat("\n")
      rlang::abort("ID column not found", class = "horizons_input_error")

    }

    return(list(id_col           = id_col,
                was_override     = TRUE,
                other_candidates = character()))

  }

  ## Auto-detect from alias list ------------------------------------------------

  matches <- col_names[col_names %in% ID_ALIASES]

  if (length(matches) == 0) {

    cat(cli::col_red(cli::style_bold("! Could not identify sample ID column:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 Checked aliases: ",
                            paste(ID_ALIASES[1:6], collapse = ", "), ", ...\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 Available columns: ",
                            paste(col_names[1:min(5, length(col_names))], collapse = ", "),
                            if (length(col_names) > 5) ", ..." else "", "\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 Specify explicitly with id_col = \"your_column\"\n")))
    cat("\n")
    rlang::abort("Could not identify sample ID column", class = "horizons_input_error")

  }

  ## Sort matches by alias priority ---------------------------------------------

  match_priority <- match(matches, ID_ALIASES)
  matches_sorted <- matches[order(match_priority)]

  selected <- matches_sorted[1]
  others   <- matches_sorted[-1]

  list(id_col           = selected,
       was_override     = FALSE,
       other_candidates = others)

}


## ---------------------------------------------------------------------------
## Helper: detect_wavelength_columns()
## ---------------------------------------------------------------------------

#' Detect wavelength columns from data
#'
#' @description
#' Identifies wavelength columns using pattern matching (wn_ prefix or numeric
#' names) or an explicit override. Also detects long format data.
#'
#' @param data [tibble.] The input data.
#' @param wavelength_cols [character or NULL.] Explicit wavelength column names,
#'   or NULL for auto-detection.
#' @param id_col [character.] The ID column name (to exclude from detection).
#'
#' @return [list.] With elements:
#'   - `wn_cols`: Character vector of wavelength column names
#'   - `was_override`: Logical, TRUE if user specified wavelength_cols
#'   - `had_wn_prefix`: Logical, TRUE if columns already had wn_ prefix
#'
#' @noRd
detect_wavelength_columns <- function(data, wavelength_cols = NULL, id_col) {

  col_names <- names(data)

  ## If user provided explicit override -----------------------------------------

  if (!is.null(wavelength_cols)) {

    missing <- setdiff(wavelength_cols, col_names)

    if (length(missing) > 0) {

      cat(cli::col_red(cli::style_bold("! Wavelength columns not found:\n")))
      cat(cli::col_red(paste0("   \u251C\u2500 Missing: ",
                              paste(missing[1:min(3, length(missing))], collapse = ", "),
                              if (length(missing) > 3) ", ..." else "", "\n")))
      cat(cli::col_red(paste0("   \u2514\u2500 Available: ",
                              paste(col_names[1:min(5, length(col_names))], collapse = ", "),
                              if (length(col_names) > 5) ", ..." else "", "\n")))
      cat("\n")
      rlang::abort("Wavelength columns not found", class = "horizons_input_error")

    }

    return(list(wn_cols       = wavelength_cols,
                was_override  = TRUE,
                had_wn_prefix = all(grepl("^wn_", wavelength_cols))))

  }

  ## Auto-detect: first try wn_ prefix ------------------------------------------

  wn_prefixed <- col_names[grepl("^wn_[0-9]+(\\.[0-9]+)?$", col_names)]

  if (length(wn_prefixed) > 0) {

    return(list(wn_cols       = wn_prefixed,
                was_override  = FALSE,
                had_wn_prefix = TRUE))

  }

  ## Auto-detect: try numeric column names --------------------------------------

  numeric_names <- col_names[grepl("^[0-9]+(\\.[0-9]+)?$", col_names)]

  if (length(numeric_names) > 0) {

    return(list(wn_cols       = numeric_names,
                was_override  = FALSE,
                had_wn_prefix = FALSE))

  }

  ## Check for long format ------------------------------------------------------

  if ("wavelength" %in% tolower(col_names) || "wavenumber" %in% tolower(col_names)) {

    cat(cli::col_red(cli::style_bold("! Data appears to be in long format:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 Found column named 'wavelength' or 'wavenumber'\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 spectra() expects wide format: one row per sample, wavelengths as columns\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 Use tidyr::pivot_wider() to reshape your data first\n")))
    cat("\n")
    rlang::abort("Data appears to be in long format", class = "horizons_input_error")

  }

  ## Nothing found --------------------------------------------------------------

  cat(cli::col_red(cli::style_bold("! Could not identify wavelength columns:\n")))
  cat(cli::col_red(paste0("   \u251C\u2500 Expected: columns starting with 'wn_' or numeric names (e.g., '4000', '3998')\n")))
  cat(cli::col_red(paste0("   \u251C\u2500 Found columns: ",
                          paste(col_names[1:min(5, length(col_names))], collapse = ", "),
                          if (length(col_names) > 5) ", ..." else "", "\n")))
  cat(cli::col_red(paste0("   \u2514\u2500 Specify explicitly with wavelength_cols = c(\"col1\", \"col2\", ...)\n")))
  cat("\n")
  rlang::abort("Could not identify wavelength columns", class = "horizons_input_error")

}


## ---------------------------------------------------------------------------
## Helper: standardize_columns()
## ---------------------------------------------------------------------------

#' Standardize column names
#'
#' @description
#' Renames the ID column to `sample_id` and adds `wn_` prefix to wavelength
#' columns if not present.
#'
#' @param data [tibble.] The input data.
#' @param detected_id [list.] Output from detect_id_column().
#' @param detected_wn [list.] Output from detect_wavelength_columns().
#'
#' @return [tibble.] Data with standardized column names.
#'
#' @noRd
standardize_columns <- function(data, detected_id, detected_wn) {

  ## Rename ID column to sample_id ----------------------------------------------

  if (detected_id$id_col != "sample_id") {

    names(data)[names(data) == detected_id$id_col] <- "sample_id"

  }

  ## Add wn_ prefix if needed ---------------------------------------------------

  if (!detected_wn$had_wn_prefix) {

    for (col in detected_wn$wn_cols) {

      new_name <- paste0("wn_", col)
      names(data)[names(data) == col] <- new_name

    }

  }

  data

}


## ---------------------------------------------------------------------------
## Helper: reorder_wavelength_columns()
## ---------------------------------------------------------------------------

#' Reorder wavelength columns in decreasing order
#'
#' @description
#' Sorts wavelength columns so that higher wavenumbers come first
#' (e.g., wn_4000, wn_3998, wn_3996, ...).
#'
#' @param data [tibble.] The input data with standardized column names.
#'
#' @return [tibble.] Data with reordered columns.
#'
#' @noRd
reorder_wavelength_columns <- function(data) {

  col_names <- names(data)

  ## Identify column groups -----------------------------------------------------

  wn_cols    <- col_names[grepl("^wn_", col_names)]
  other_cols <- col_names[!grepl("^wn_", col_names)]

  ## Sort wavelength columns by wavenumber (decreasing) -------------------------

  wn_values <- as.numeric(gsub("^wn_", "", wn_cols))
  wn_sorted <- wn_cols[order(wn_values, decreasing = TRUE)]

  ## Reorder: sample_id first, then wn_ columns, then others --------------------

  id_col     <- "sample_id"
  meta_cols  <- setdiff(other_cols, id_col)
  new_order  <- c(id_col, wn_sorted, meta_cols)

  data[, new_order]

}


## ---------------------------------------------------------------------------
## Helper: build_role_map()
## ---------------------------------------------------------------------------

#' Build role_map tibble
#'
#' @description
#' Creates the role_map that tracks what role each column plays:
#' id, predictor, or meta.
#'
#' @param data [tibble.] The standardized data.
#'
#' @return [tibble.] With columns `variable` and `role`.
#'
#' @noRd
build_role_map <- function(data) {

  col_names <- names(data)

  ## Assign roles ---------------------------------------------------------------

  roles <- dplyr::case_when(
    col_names == "sample_id"     ~ "id",
    grepl("^wn_", col_names)     ~ "predictor",
    TRUE                         ~ "meta"
  )

  tibble::tibble(variable = col_names,
                 role     = roles)

}


## ---------------------------------------------------------------------------
## Helper: report_spectra_summary()
## ---------------------------------------------------------------------------

#' Report summary of what spectra() did
#'
#' @description
#' Prints an informed consent summary showing what was detected and how
#' data was processed.
#'
#' @param obj [horizons_data.] The constructed object.
#' @param detected_id [list.] Output from detect_id_column().
#' @param detected_wn [list.] Output from detect_wavelength_columns().
#' @param input_type [character.] The input type ("tibble", "csv", "opus").
#' @param source_path [character.] The source path or "tibble".
#'
#' @return NULL (called for side effects).
#'
#' @noRd
report_spectra_summary <- function(obj, detected_id, detected_wn, input_type, source_path) {

  n_samples    <- obj$data$n_rows
  n_predictors <- obj$data$n_predictors
  n_meta       <- sum(obj$data$role_map$role == "meta")

  ## Get wavelength range -------------------------------------------------------

  wn_cols   <- obj$data$role_map$variable[obj$data$role_map$role == "predictor"]
  wn_values <- as.numeric(gsub("^wn_", "", wn_cols))
  wn_min    <- min(wn_values)
  wn_max    <- max(wn_values)

  ## Print summary --------------------------------------------------------------

  cat("\n")
  cat(cli::style_bold("\u251C\u2500 Input\n"))

  if (input_type == "tibble") {

    cat(paste0("\u2502  \u251C\u2500 Type: tibble (in-memory)\n"))
    cat(paste0("\u2502  \u2514\u2500 Rows: ", n_samples, "\n"))

  } else if (input_type == "csv") {

    cat(paste0("\u2502  \u251C\u2500 Type: CSV file\n"))
    cat(paste0("\u2502  \u2514\u2500 Path: ", source_path, "\n"))

  } else if (input_type == "opus") {

    cat(paste0("\u2502  \u251C\u2500 Type: OPUS files\n"))
    cat(paste0("\u2502  \u2514\u2500 Path: ", source_path, "\n"))

  }

  cat(cli::style_bold("\u251C\u2500 Column Detection\n"))

  ## ID column reporting --------------------------------------------------------

  if (detected_id$was_override) {

    cat(paste0("\u2502  \u251C\u2500 Sample ID: ", cli::col_cyan(detected_id$id_col),
               " (specified) \u2192 sample_id\n"))

  } else if (detected_id$id_col == "sample_id") {

    cat(paste0("\u2502  \u251C\u2500 Sample ID: ", cli::col_cyan("sample_id"), "\n"))

  } else {

    cat(paste0("\u2502  \u251C\u2500 Sample ID: ", cli::col_cyan(detected_id$id_col),
               " \u2192 sample_id\n"))

  }

  if (length(detected_id$other_candidates) > 0) {

    cli::cli_alert_info("Also found: {.field {detected_id$other_candidates}} (using higher priority)")

  }

  ## Wavelength column reporting ------------------------------------------------

  cat(paste0("\u2502  \u251C\u2500 Wavelengths: ", n_predictors,
             " (", cli::col_cyan(paste0("wn_", wn_max)), " to ",
             cli::col_cyan(paste0("wn_", wn_min)), ")\n"))

  if (!detected_wn$had_wn_prefix && !detected_wn$was_override) {

    cat(paste0("\u2502  \u2502     \u2514\u2500 Added wn_ prefix to numeric column names\n"))

  }

  ## Meta columns ---------------------------------------------------------------

  if (n_meta > 0) {

    meta_cols <- obj$data$role_map$variable[obj$data$role_map$role == "meta"]
    meta_str  <- paste(meta_cols, collapse = ", ")
    cat(paste0("\u2502  \u2514\u2500 Other: ", n_meta, " (",
               cli::col_silver(meta_str), ") \u2192 meta\n"))

  } else {

    cat(paste0("\u2502  \u2514\u2500 Other: none\n"))

  }

  ## Result ---------------------------------------------------------------------

  cat(cli::style_bold("\u2514\u2500 Result\n"))
  cat(paste0("   \u2514\u2500 Created horizons_data with ", n_samples, " samples\n"))
  cat("\n")

}


## ---------------------------------------------------------------------------
## spectra() — User-Facing Constructor
## ---------------------------------------------------------------------------

#' Load spectral data into a horizons_data object
#'
#' @description
#' Entry point for the horizons pipeline. Loads spectral data from OPUS files,
#' CSV files, or an in-memory tibble and returns a standardized `horizons_data`
#' object ready for preprocessing and modeling.
#'
#' @details
#' The function automatically detects the input type and routes to the
#' appropriate reader:
#'
#' - **OPUS files**: Binary files from Bruker spectrometers. Provide a directory
#'   path containing `.0`, `.1`, etc. files. Sample IDs are extracted from
#'   filenames.
#'
#' - **CSV files**: Comma-delimited files with samples as rows and wavelengths
#'   as columns. The function auto-detects ID and wavelength columns.
#'
#' - **Tibble/data.frame**: Pre-loaded data with the same structure as CSV.
#'
#' Column detection uses a priority-based alias system for ID columns and
#' pattern matching for wavelength columns. Override with explicit parameters
#' if auto-detection fails.
#'
#' @param source [character or data.frame.] Path to OPUS directory, CSV file,
#'   or a pre-loaded tibble/data.frame.
#' @param type [character or NULL.] Explicit type override: `"opus"`, `"csv"`,
#'   or `NULL` for auto-detection. Default: `NULL`.
#' @param id_col [character or NULL.] Name of the sample ID column. If `NULL`,
#'   auto-detects from common aliases. Default: `NULL`.
#' @param wavelength_cols [character vector or NULL.] Names of wavelength
#'   columns. If `NULL`, auto-detects columns with `wn_` prefix or numeric
#'   names. Default: `NULL`.
#' @param recursive [logical.] For OPUS directories, search subdirectories.
#'   Default: `FALSE`.
#'
#' @return A `horizons_data` object with:
#'   \describe{
#'     \item{data$analysis}{Tibble with `sample_id` and `wn_*` columns}
#'     \item{data$role_map}{Tibble mapping columns to roles (id, predictor, meta)}
#'     \item{provenance}{Source path, type, timestamps, version info}
#'   }
#'
#' @examples
#' \dontrun{
#' # From OPUS files
#' hd <- spectra("path/to/opus/")
#'
#' # From CSV
#' hd <- spectra("data.csv")
#'
#' # From tibble with explicit ID column
#' hd <- spectra(my_data, id_col = "SampleName")
#' }
#'
#' @export
spectra <- function(source,
                    type            = NULL,
                    id_col          = NULL,
                    wavelength_cols = NULL,
                    recursive       = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Input validation
  ## ---------------------------------------------------------------------------

  errors <- character()

  if (!is.null(id_col) && (!is.character(id_col) || length(id_col) != 1)) {

    errors <- c(errors,
                cli::format_inline("{.arg id_col} must be a single character string"))

  }

  if (!is.null(wavelength_cols) && !is.character(wavelength_cols)) {

    errors <- c(errors,
                cli::format_inline("{.arg wavelength_cols} must be a character vector"))

  }

  if (!is.logical(recursive) || length(recursive) != 1 || is.na(recursive)) {

    errors <- c(errors,
                cli::format_inline("{.arg recursive} must be TRUE or FALSE"))

  }

  if (length(errors) > 0) {

    cat(cli::col_red(cli::style_bold("! Input validation failed:\n")))

    for (i in seq_along(errors)) {

      branch <- if (i < length(errors)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_red(paste0("   ", branch, " ", errors[i], "\n")))

    }

    cat("\n")
    rlang::abort("Input validation failed", class = "horizons_input_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Detect input type
  ## ---------------------------------------------------------------------------

  input_type <- detect_input_type(source, type)

  ## ---------------------------------------------------------------------------
  ## Step 2: Load data based on type
  ## ---------------------------------------------------------------------------

  if (input_type == "tibble") {

    data        <- source
    source_path <- "tibble"

  } else if (input_type == "csv") {

    data        <- read_csv_spectra(source)
    source_path <- source

  } else if (input_type == "opus") {

    data        <- read_opus_files(source, recursive = recursive)
    source_path <- source

  }

  ## Check for empty data ------------------------------------------------------

  if (nrow(data) == 0) {

    cat(cli::col_red(cli::style_bold("! Empty data:\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 No rows found in input\n")))
    cat("\n")
    rlang::abort("Empty data", class = "horizons_input_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Detect ID column
  ## ---------------------------------------------------------------------------

  detected_id <- detect_id_column(data, id_col)

  ## ---------------------------------------------------------------------------
  ## Step 4: Detect wavelength columns
  ## ---------------------------------------------------------------------------

  detected_wn <- detect_wavelength_columns(data, wavelength_cols, detected_id$id_col)

  ## ---------------------------------------------------------------------------
  ## Step 5: Standardize column names
  ## ---------------------------------------------------------------------------

  data <- standardize_columns(data, detected_id, detected_wn)

  ## ---------------------------------------------------------------------------
  ## Step 6: Reorder wavelength columns (decreasing wavenumber)
  ## ---------------------------------------------------------------------------

  data <- reorder_wavelength_columns(data)

  ## ---------------------------------------------------------------------------
  ## Step 6.5: Check for duplicate sample IDs (fail fast)
  ## ---------------------------------------------------------------------------

  dup_ids <- data$sample_id[duplicated(data$sample_id)]

  if (length(dup_ids) > 0) {

    unique_dups <- unique(dup_ids)
    n_show      <- min(5, length(unique_dups))
    dup_str     <- paste(unique_dups[1:n_show], collapse = ", ")

    cat(cli::col_red(cli::style_bold("! Duplicate sample IDs found:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 ", length(dup_ids), " duplicate(s): ", dup_str,
                            if (length(unique_dups) > 5) ", ..." else "", "\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 Sample IDs must be unique\n")))
    cat("\n")
    rlang::abort("Duplicate sample IDs", class = "horizons_input_error")

  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Build role_map
  ## ---------------------------------------------------------------------------

  role_map <- build_role_map(data)

  ## ---------------------------------------------------------------------------
  ## Step 8: Construct horizons_data object
  ## ---------------------------------------------------------------------------

  obj <- new_horizons_data(
    analysis       = data,
    role_map       = role_map,
    spectra_source = source_path,
    spectra_type   = input_type
  )

  ## ---------------------------------------------------------------------------
  ## Step 9: Validate
  ## ---------------------------------------------------------------------------

  obj <- validate_horizons_data(obj)

  ## ---------------------------------------------------------------------------
  ## Step 10: CLI output (informed consent)
  ## ---------------------------------------------------------------------------

  report_spectra_summary(obj, detected_id, detected_wn, input_type, source_path)

  obj

}
