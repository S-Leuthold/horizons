# R/pipeline-parse-ids.R
# Extract structured metadata from filenames and update sample IDs in
# horizons_data objects. Supports format strings and explicit regex patterns.

## =============================================================================
## Section 1: Constants
## =============================================================================

#' Variants of sampleid token (case-insensitive matching)
#' @noRd
SAMPLEID_VARIANTS <- c("sampleid", "sample_id", "SampleID", "Sample_ID", "sampleId")


## =============================================================================
## Section 2: Main Function
## =============================================================================


## ---------------------------------------------------------------------------
## parse_ids() — Extract metadata from filenames
## ---------------------------------------------------------------------------

#' Parse sample IDs and metadata from filenames
#'
#' @description
#' Extracts structured metadata from OPUS filenames and updates `sample_id` accordingly.
#' This function is designed for OPUS file workflows where instrument-generated filenames
#' encode project, sample ID, fraction, replicate, or position information.
#'
#' @details
#' **Note:** This function is intended for OPUS file workflows. For CSV or tibble data,
#' sample IDs and metadata are typically already in separate columns and do not require
#' parsing. A warning is issued if `parse_ids()` is called on non-OPUS data.
#'
#' The function supports two modes:
#'
#' **Format string mode** (`format` parameter):
#' Curly brackets denote tokens that become columns. Everything else is a literal
#' separator.
#' ```
#' format = "{project}_{sampleid}_{fraction}"
#' # Parses "PROJ-A_S100-1_GroundBulk" into:
#' #   project = "PROJ-A", sampleid = "S100-1", fraction = "GroundBulk"
#' ```
#'
#' **Regex patterns mode** (`patterns` parameter):
#' Named patterns become columns; unnamed patterns are separators.
#' ```
#' patterns = c(project = "[^_]+", "_", sampleid = "S\\d+-\\d+", "_.*")
#' ```
#'
#' The special token `sampleid` (case-insensitive) overwrites the `sample_id`
#' column. All other tokens create new metadata columns.
#'
#' @param x A `horizons_data` object from `spectra()`.
#' @param format Character. Format string with `{tokens}`, e.g.,
#'   `"{project}_{sampleid}_{fraction}"`. Mutually exclusive with `patterns`.
#' @param patterns Named character vector. Regex patterns where names become
#'   columns and unnamed elements are separators. Mutually exclusive with `format`.
#' @param too_few Character. How to handle non-matching filenames:
#'   * `"keep_original"` (default): Keep original filename as `sample_id`, warn.
#'   * `"error"`: Abort with list of non-matching files.
#'   * `"na"`: Set `sample_id` to NA, warn.
#'
#' @return The input `horizons_data` object with:
#'   * `sample_id` updated from the `sampleid` token (if present)
#'   * New metadata columns added to `data$analysis`
#'   * `data$role_map` updated with new columns (role: meta)
#'   * Provenance recorded in `provenance$parse_ids`
#'
#' @examples
#' \dontrun{
#' # Basic format string
#' hd |> parse_ids(format = "{project}_{sampleid}_{fraction}")
#'
#' # Mixed delimiters
#' hd |> parse_ids(format = "{project}-{sampleid}.{fraction}")
#'
#' # Regex for complex cases
#' hd |> parse_ids(patterns = c(
#'   project = "PROJ-[A-Z]",
#'   "_",
#'   sampleid = "S\\d+-\\d+"
#' ))
#' }
#'
#' @export
parse_ids <- function(x,
                      format   = NULL,
                      patterns = NULL,
                      too_few  = "keep_original") {

  ## -------------------------------------------------------------------------
  ## Step 1: Input validation
  ## -------------------------------------------------------------------------

  ## Validate horizons_data object -------------------------------------------

  if (!inherits(x, "horizons_data")) {

    cli::cli_abort(c(
      "{.arg x} must be a horizons_data object",
      "x" = "Got {.cls {class(x)}}"
    ))

  }

  ## Warn if not OPUS source -------------------------------------------------

  source_type <- x$provenance$spectra_type

  if (!is.null(source_type) && source_type %in% c("csv", "tibble")) {

    cli::cli_warn(c(
      "{.fn parse_ids} is designed for OPUS file workflows",
      "i" = "Data source: {.val {source_type}}",
      "i" = "For CSV/tibble data, sample IDs are typically already in separate columns",
      "i" = "If you need to parse IDs, ensure a {.field filename} column exists"
    ))

  }

  ## Validate exactly one of format/patterns provided ------------------------

  has_format   <- !is.null(format)
  has_patterns <- !is.null(patterns)

  if (!has_format && !has_patterns) {

    cli::cli_abort(c(
      "Must provide either {.arg format} or {.arg patterns}",
      "i" = "Use {.arg format} for simple token-based parsing",
      "i" = "Use {.arg patterns} for complex regex matching"
    ))

  }

  if (has_format && has_patterns) {

    cli::cli_abort(c(
      "Cannot provide both {.arg format} and {.arg patterns}",
      "i" = "Choose one parsing method"
    ))

  }

  ## Validate format string if provided --------------------------------------

  if (has_format) {

    if (!is.character(format) || length(format) != 1) {

      cli::cli_abort("{.arg format} must be a single character string")

    }

    if (nchar(format) == 0) {

      cli::cli_abort("{.arg format} cannot be empty")

    }

  }

  ## Validate patterns if provided -------------------------------------------

  if (has_patterns) {

    if (!is.character(patterns)) {

      cli::cli_abort("{.arg patterns} must be a character vector")

    }

  }

  ## Validate too_few parameter ----------------------------------------------

  too_few <- match.arg(too_few, c("keep_original", "error", "na"))

  ## -------------------------------------------------------------------------
  ## Step 2: Get filenames from analysis tibble
  ## -------------------------------------------------------------------------

  ## Check filename column exists --------------------------------------------

  if (!"filename" %in% names(x$data$analysis)) {

    cli::cli_abort(c(
      "No {.field filename} column found in data",
      "i" = "The {.fn spectra} function should create this column",
      "i" = "Check that you're using a horizons_data object from {.fn spectra}"
    ))

  }

  filenames <- x$data$analysis$filename

  ## Strip file extensions ---------------------------------------------------

  filenames_clean <- tools::file_path_sans_ext(filenames)

  ## -------------------------------------------------------------------------
  ## Step 3: Convert format to patterns (if needed)
  ## -------------------------------------------------------------------------

  if (has_format) {

    patterns <- format_to_patterns(format)

  }

  ## -------------------------------------------------------------------------
  ## Step 4: Apply patterns to extract values
  ## -------------------------------------------------------------------------

  extracted <- apply_patterns(filenames_clean, patterns)

  ## -------------------------------------------------------------------------
  ## Step 5: Handle non-matches
  ## -------------------------------------------------------------------------

  non_matches   <- which(is.na(extracted$.matched))
  n_matched     <- sum(!is.na(extracted$.matched))
  n_unmatched   <- length(non_matches)

  if (n_unmatched > 0) {

    non_match_names <- filenames[non_matches]

    if (too_few == "error") {

      err <- c(
        "Pattern did not match {n_unmatched} filename{?s}",
        "i" = "Non-matching files:",
        "i" = "{.val {head(non_match_names, 10)}}"
      )
      if (n_unmatched > 10) {
        err <- c(err, "i" = "... and {n_unmatched - 10} more")
      }
      cli::cli_abort(err)

    } else if (too_few == "keep_original") {

      cli::cli_warn(c(
        "Pattern did not match {n_unmatched} filename{?s}",
        "i" = "Keeping original filename as sample_id",
        "i" = "Non-matching: {.val {head(non_match_names, 5)}}"
      ))

    } else if (too_few == "na") {

      cli::cli_warn(c(
        "Pattern did not match {n_unmatched} filename{?s}",
        "i" = "Setting sample_id to NA for non-matches",
        "i" = "Non-matching: {.val {head(non_match_names, 5)}}"
      ))

    }

  }

  ## -------------------------------------------------------------------------
  ## Step 6: Handle sampleid token
  ## -------------------------------------------------------------------------

  ## Find sampleid column (case-insensitive) ---------------------------------

  extracted_cols    <- setdiff(names(extracted), ".matched")
  sampleid_matches  <- which(tolower(extracted_cols) %in% tolower(SAMPLEID_VARIANTS))
  sampleid_col      <- if (length(sampleid_matches) > 0) extracted_cols[sampleid_matches[1]] else NULL

  ## Update sample_id from sampleid token ------------------------------------

  if (!is.null(sampleid_col)) {

    new_sample_id <- extracted[[sampleid_col]]

    ## Handle non-matches based on too_few -----------------------------------

    if (n_unmatched > 0) {

      if (too_few == "keep_original") {

        new_sample_id[non_matches] <- filenames[non_matches]

      } else if (too_few == "na") {

        new_sample_id[non_matches] <- NA_character_

      }

    }

    x$data$analysis$sample_id <- new_sample_id

    ## Remove sampleid column from extracted (don't duplicate) ---------------

    extracted[[sampleid_col]] <- NULL
    extracted_cols <- setdiff(extracted_cols, sampleid_col)

  } else {

    ## No sampleid token - handle non-matches anyway -------------------------

    if (n_unmatched > 0 && too_few == "na") {

      x$data$analysis$sample_id[non_matches] <- NA_character_

    }

  }

  ## -------------------------------------------------------------------------
  ## Step 7: Add extracted columns to analysis tibble
  ## -------------------------------------------------------------------------

  ## Remove .matched column --------------------------------------------------

  extracted$.matched <- NULL

  ## Get columns to add (excluding sampleid which was handled above) ---------

  cols_to_add <- setdiff(names(extracted), c(".matched", sampleid_col))

  if (length(cols_to_add) > 0) {

    ## Find position after filename column -----------------------------------

    filename_pos <- which(names(x$data$analysis) == "filename")

    ## Add new columns to existing tibble ------------------------------------

    for (col in cols_to_add) {

      x$data$analysis[[col]] <- extracted[[col]]

    }

    ## Reorder columns: before filename, filename, new cols, after filename --
    ## Using column index selection is more memory-efficient than copying ----

    before_cols <- names(x$data$analysis)[seq_len(filename_pos)]

    if (filename_pos < ncol(x$data$analysis)) {

      after_cols <- setdiff(
        names(x$data$analysis)[(filename_pos + 1):ncol(x$data$analysis)],
        cols_to_add
      )

    } else {

      after_cols <- character(0)

    }

    col_order       <- c(before_cols, cols_to_add, after_cols)
    x$data$analysis <- x$data$analysis[, col_order, drop = FALSE]

    ## Update role_map -------------------------------------------------------

    new_roles <- tibble::tibble(
      variable = cols_to_add,
      role     = rep("meta", length(cols_to_add))
    )

    x$data$role_map <- dplyr::bind_rows(x$data$role_map, new_roles)

  }

  ## -------------------------------------------------------------------------
  ## Step 8: Update provenance
  ## -------------------------------------------------------------------------

  x$provenance$parse_ids <- list(
    format        = format,
    patterns      = if (has_patterns && !has_format) patterns else NULL,
    matched       = n_matched,
    unmatched     = n_unmatched,
    columns_added = cols_to_add,
    applied_at    = Sys.time()
  )

  x$provenance$id_pattern <- format

  ## -------------------------------------------------------------------------
  ## Step 9: CLI output
  ## -------------------------------------------------------------------------

  cat(paste0("\u251C\u2500 ", cli::style_bold("Parsing sample IDs"), "...\n"))

  id_str <- paste0(n_matched, " IDs extracted")
  if (n_unmatched > 0) {

    id_str <- paste0(id_str, " (", n_unmatched, " unmatched)")

  }

  cat(paste0("\u2502  \u2514\u2500 ", id_str, "\n"))
  cat("\u2502\n")

  x

}


## =============================================================================
## Section 3: Helper Functions
## =============================================================================


## ---------------------------------------------------------------------------
## format_to_patterns() — Convert format string to patterns vector
## ---------------------------------------------------------------------------

#' Convert format string to regex patterns
#'
#' @description
#' Parses a format string like `"{project}_{sampleid}_{fraction}"` and converts
#' it to a named patterns vector for regex matching.
#'
#' @details
#' Tokens in curly brackets become named capturing groups. Literal text between
#' tokens becomes separator patterns (escaped for regex). All tokens except the
#' last use non-greedy matching `(.+?)`; the last token uses greedy `(.+)`.
#'
#' @param format Character. Format string with `{token}` placeholders.
#'
#' @return Named character vector of regex patterns.
#'
#' @noRd
format_to_patterns <- function(format) {

  ## -------------------------------------------------------------------------
  ## Step 1: Parse tokens from format string
  ## -------------------------------------------------------------------------

  ## Find all {token} patterns -----------------------------------------------

  token_regex  <- "\\{([^}]+)\\}"
  token_matches <- gregexpr(token_regex, format, perl = TRUE)
  token_starts  <- as.integer(token_matches[[1]])

  if (token_starts[1] == -1) {

    cli::cli_abort(c(
      "No tokens found in format string",
      "i" = "Format: {.val {format}}",
      "i" = "Tokens should be in curly brackets, e.g., {{sampleid}}"
    ))

  }

  token_lengths <- attr(token_matches[[1]], "match.length")
  n_tokens      <- length(token_starts)

  ## Extract token names -----------------------------------------------------

  token_names <- character(n_tokens)

  for (i in seq_len(n_tokens)) {

    token_full <- substr(format, token_starts[i], token_starts[i] + token_lengths[i] - 1)
    token_names[i] <- gsub("^\\{|\\}$", "", token_full)

  }

  ## -------------------------------------------------------------------------
  ## Step 2: Build patterns vector
  ## -------------------------------------------------------------------------

  patterns <- character()
  pos      <- 1

  for (i in seq_len(n_tokens)) {

    ## Add separator before this token (if any) ------------------------------

    if (token_starts[i] > pos) {

      separator <- substr(format, pos, token_starts[i] - 1)

      ## Escape regex special characters -------------------------------------

      separator_escaped <- escape_regex(separator)
      patterns <- c(patterns, separator_escaped)

    }

    ## Add token pattern (named) ---------------------------------------------

    ## Last token gets greedy match, others get non-greedy -------------------

    if (i == n_tokens) {

      token_pattern <- "(.+)"

    } else {

      token_pattern <- "(.+?)"

    }

    names(token_pattern) <- token_names[i]
    patterns <- c(patterns, token_pattern)

    ## Update position -------------------------------------------------------

    pos <- token_starts[i] + token_lengths[i]

  }

  ## Add trailing text (if any) ----------------------------------------------

  if (pos <= nchar(format)) {

    trailing <- substr(format, pos, nchar(format))
    patterns <- c(patterns, escape_regex(trailing))

  }

  patterns

}


## ---------------------------------------------------------------------------
## apply_patterns() — Apply regex patterns to filenames
## ---------------------------------------------------------------------------

#' Apply patterns to extract values from filenames
#'
#' @description
#' Builds a full regex from patterns vector and extracts named groups.
#'
#' @param filenames Character vector of filenames to parse.
#' @param patterns Named character vector of patterns.
#'
#' @return A tibble with one column per named pattern plus `.matched` indicator.
#'
#' @noRd
apply_patterns <- function(filenames, patterns) {

  ## -------------------------------------------------------------------------
  ## Step 1: Build full regex
  ## -------------------------------------------------------------------------

  ## Get named patterns (capturing groups) -----------------------------------

  pattern_names <- names(patterns)

  if (is.null(pattern_names)) {

    pattern_names <- rep("", length(patterns))

  }

  named_idx <- which(pattern_names != "")

  ## Build regex with named groups -------------------------------------------

  regex_parts <- character(length(patterns))

  for (i in seq_along(patterns)) {

    if (pattern_names[i] != "") {

      ## Named pattern: ensure capturing group ------------------------------

      ## If pattern doesn't start with (, wrap it in parentheses

      pat <- patterns[i]

      if (!grepl("^\\(", pat)) {

        pat <- paste0("(", pat, ")")

      }

      regex_parts[i] <- pat

    } else {

      ## Unnamed pattern: use non-capturing group ---------------------------

      regex_parts[i] <- paste0("(?:", patterns[i], ")")

    }

  }

  full_regex <- paste0("^", paste(regex_parts, collapse = ""), "$")

  ## -------------------------------------------------------------------------
  ## Step 2: Apply regex to filenames
  ## -------------------------------------------------------------------------

  matches <- regmatches(filenames, regexec(full_regex, filenames, perl = TRUE))

  ## -------------------------------------------------------------------------
  ## Step 3: Extract values into tibble
  ## -------------------------------------------------------------------------

  ## Initialize result tibble ------------------------------------------------

  result <- tibble::tibble(.matched = rep(NA_character_, length(filenames)))

  ## Add column for each named pattern ---------------------------------------

  for (name in pattern_names[named_idx]) {

    result[[name]] <- NA_character_

  }

  ## Fill in values from matches ---------------------------------------------

  for (i in seq_along(matches)) {

    match_result <- matches[[i]]

    if (length(match_result) > 0 && match_result[1] != "") {

      ## Match succeeded -----------------------------------------------------

      result$.matched[i] <- match_result[1]

      ## Extract captured groups (skip full match at position 1) -------------

      captured <- match_result[-1]

      if (length(captured) == length(named_idx)) {

        for (j in seq_along(named_idx)) {

          col_name <- pattern_names[named_idx[j]]
          result[[col_name]][i] <- captured[j]

        }

      }

    }

  }

  result

}


## ---------------------------------------------------------------------------
## escape_regex() — Escape regex special characters
## ---------------------------------------------------------------------------

#' Escape regex special characters in a string
#'
#' @param string Character. String to escape.
#'
#' @return Character. Escaped string.
#'
#' @noRd
escape_regex <- function(string) {

  ## Characters that need escaping in regex ----------------------------------

  special_chars <- c("\\", ".", "+", "*", "?", "^", "$", "(", ")", "[", "]", "{", "}", "|")

  result <- string

  for (char in special_chars) {

    result <- gsub(char, paste0("\\", char), result, fixed = TRUE)

  }

  result

}
