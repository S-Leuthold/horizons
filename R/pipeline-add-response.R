#' Pipeline: Join Response Data
#'
#' @description
#' Joins lab-measured response data to a `horizons_data` object. This is the
#' bridge between spectral preprocessing and modeling — everything downstream
#' (`configure()`, `evaluate()`, etc.) requires response data.
#'
#' @details
#' `add_response()` performs a left join between the horizons analysis tibble
#' and a source of lab-measured data. Only columns named in `variable` are
#' joined; all other source columns are ignored.
#'
#' **Source types:**
#'
#' * **tibble/data.frame** (canonical): Joined directly. User controls all
#'   preprocessing.
#'
#' * **character path** (convenience): Read via `readr::read_csv()`, then
#'   joined. CSV only — for other formats, read the data yourself and pass
#'   the tibble.
#'
#' **Join behavior:**
#'
#' Left-join semantics matching `dplyr::left_join()` conventions. Strict exact
#' matching — no case-folding or fuzzy matching. When match quality is poor,
#' diagnostic messages detect common patterns (case mismatch, whitespace,
#' prefix/suffix) and suggest fixes.
#'
#' **Duplicate detection:**
#'
#' Aborts if duplicates exist in either the source join key (would multiply
#' spectral rows) or the horizons join key (indicates replicates haven't been
#' averaged — suggests `average()` first).
#'
#' **Repeated calls:**
#'
#' Can be called multiple times to join variables from different sources.
#' Aborts if a variable name already exists in the analysis tibble.
#'
#' Joined variables receive `role = "response"` in the role map. Use
#' `configure()` to promote one response to `role = "outcome"` for modeling.
#'
#' @param x [horizons_data]. Object from pipeline (post-`average()`, unique
#'   `sample_id` required).
#' @param source [tibble, data.frame, or character path]. Response data source.
#'   If a path, read via `readr::read_csv()`.
#' @param variable [character vector]. Column name(s) in the source to join as
#'   response variables. Required — no default. Must be numeric columns.
#' @param by [character]. Join key. Default `"sample_id"`. Use a named vector
#'   for mismatched column names: `c("sample_id" = "Lab_ID")` where the name
#'   is the horizons-side column and the value is the source-side column.
#'
#' @return A modified `horizons_data` object with:
#'   * Response columns added to `data$analysis`
#'   * New entries in `data$role_map` with `role = "response"`
#'   * Updated `data$n_responses` count
#'   * Provenance record appended to `provenance$add_response`
#'
#' @examples
#' \dontrun{
#' # Tibble source (canonical)
#' lab <- readr::read_csv("lab_results.csv")
#' hd |> add_response(lab, variable = c("SOC", "POM_C"), by = "sample_id")
#'
#' # Path source (convenience)
#' hd |> add_response("lab_results.csv", variable = "SOC")
#'
#' # Named join key (different column names)
#' hd |> add_response(lab, variable = "SOC", by = c("sample_id" = "Lab_ID"))
#'
#' # Multiple sources (separate calls)
#' hd |>
#'   add_response(lab_carbon, variable = c("SOC", "POM_C")) |>
#'   add_response(lab_ph, variable = "pH")
#' }
#'
#' @export

add_response <- function(x,
                         source,
                         variable,
                         by = "sample_id") {

  ## ---------------------------------------------------------------------------
  ## Step 0: Print header
  ## ---------------------------------------------------------------------------

  cat("\u2502\n")
  cat(paste0("\u251C\u2500 ", cli::style_bold("Adding response data"), "...\n"))

  ## ---------------------------------------------------------------------------
  ## Helper: abort with tree-nested error
  ## ---------------------------------------------------------------------------

  abort_nested <- function(header, details, error_class = "horizons_input_error") {

    cat(cli::col_red(paste0("\u2502  \u2514\u2500 ", header, "\n")))
    for (i in seq_along(details)) {
      branch <- if (i < length(details)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_red(paste0("\u2502        ", branch, " ", details[i], "\n")))
    }
    cat("\n")
    rlang::abort(
      paste(c(header, details), collapse = "\n"),
      class = error_class
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  ## 1.1 x must be horizons_data -----------------------------------------------

  if (!inherits(x, "horizons_data")) {

    ## Special case: header hasn't printed correctly since x is wrong type
    ## Reprint with standalone error
    cat(cli::col_red(paste0(
      "\u2502  \u2514\u2500 x must be a horizons_data object (got ", class(x)[1], ")\n"
    )))
    cat("\n")
    rlang::abort(
      paste("x must be a horizons_data object. Got:", class(x)[1]),
      class = "horizons_input_error"
    )

  }

  ## 1.2 source type check -----------------------------------------------------

  is_path   <- is.character(source) && length(source) == 1
  is_tibble <- inherits(source, "data.frame")

  if (!is_path && !is_tibble) {

    abort_nested(
      "source must be a data frame or file path",
      c(paste0("Got class: ", class(source)[1]))
    )

  }

  ## 1.3 If path: validate file exists, read CSV, record source label ----------

  if (is_path) {

    if (!file.exists(source)) {

      abort_nested(
        "Source file does not exist",
        c(paste0("Path: ", source))
      )

    }

    source_label <- source
    source       <- readr::read_csv(source, show_col_types = FALSE)

  } else {

    source_label <- "tibble"

  }

  ## 1.4 variable columns exist in source --------------------------------------

  missing_vars <- setdiff(variable, names(source))

  if (length(missing_vars) > 0) {

    abort_nested(
      "Variables not found in source data",
      c(paste0("Missing: ", paste(missing_vars, collapse = ", ")),
        paste0("Available: ", paste(names(source), collapse = ", ")))
    )

  }

  ## 1.5 variable columns are numeric ------------------------------------------

  non_numeric <- variable[!vapply(source[variable], is.numeric, logical(1))]

  if (length(non_numeric) > 0) {

    non_numeric_types <- vapply(
      source[non_numeric], function(col) class(col)[1], character(1)
    )
    type_strs <- paste0(non_numeric, " (", non_numeric_types, ")")

    abort_nested(
      "Response variables must be numeric",
      c(paste0("Non-numeric: ", paste(type_strs, collapse = ", ")))
    )

  }

  ## 1.6 variable names don't already exist in horizons data -------------------

  existing <- intersect(variable, names(x$data$analysis))

  if (length(existing) > 0) {

    current_responses <- x$data$role_map$variable[x$data$role_map$role == "response"]

    abort_nested(
      "Response variables already exist in horizons data",
      c(paste0("Already exists: ", paste(existing, collapse = ", ")),
        paste0("Current responses: ", paste(current_responses, collapse = ", ")))
    )

  }

  ## 1.7 Parse `by` and validate join columns exist in both datasets -----------

  ## Only single-column joins supported
  if (length(by) > 1) {

    abort_nested(
      "Multi-column joins not supported",
      c(paste0("Got ", length(by), " columns: ", paste(by, collapse = ", ")),
        "Create a composite key first, then join on that single column")
    )

  }

  ## Reject partially-named vectors (all or nothing)
  if (!is.null(names(by))) {

    has_name <- names(by) != ""

    if (any(has_name) && !all(has_name)) {

      abort_nested(
        "Join key 'by' is partially named",
        c("Use a named vector: c(\"horizons_col\" = \"source_col\")",
          "Or an unnamed string: \"same_col_name\"")
      )

    }

  }

  if (!is.null(names(by)) && any(names(by) != "")) {

    ## Named vector: c("horizons_col" = "source_col")
    by_horizons <- names(by)
    by_source   <- unname(by)

  } else {

    ## Simple string: same column name in both
    by_horizons <- by
    by_source   <- by

  }

  if (!by_horizons %in% names(x$data$analysis)) {

    abort_nested(
      "Join column not found in horizons data",
      c(paste0("Column: ", by_horizons),
        paste0("Available: ", paste(names(x$data$analysis), collapse = ", ")))
    )

  }

  if (!by_source %in% names(source)) {

    abort_nested(
      "Join column not found in source data",
      c(paste0("Column: ", by_source),
        paste0("Available: ", paste(names(source), collapse = ", ")))
    )

  }

  ## 1.8 No duplicate keys in source ------------------------------------------

  source_keys    <- source[[by_source]]
  source_dup_idx <- duplicated(source_keys)

  if (any(source_dup_idx)) {

    dup_vals   <- unique(source_keys[source_dup_idx])
    dup_counts <- table(source_keys)[as.character(dup_vals)]
    dup_strs   <- paste0('"', names(dup_counts), '" (', dup_counts, "x)")

    abort_nested(
      paste0("Duplicate values in source join key '", by_source, "'"),
      c(paste0("Duplicated: ", paste(dup_strs, collapse = ", ")),
        "Each sample must appear exactly once in the source data")
    )

  }

  ## 1.9 No duplicate keys in horizons (suggests average()) --------------------

  horizons_keys    <- x$data$analysis[[by_horizons]]
  horizons_dup_idx <- duplicated(horizons_keys)

  if (any(horizons_dup_idx)) {

    dup_vals   <- unique(horizons_keys[horizons_dup_idx])
    dup_counts <- table(horizons_keys)[as.character(dup_vals)]
    dup_strs   <- paste0('"', names(dup_counts), '" (', dup_counts, "x)")

    abort_nested(
      "Duplicate sample IDs found in horizons data",
      c(paste0("Duplicated: ", paste(dup_strs, collapse = ", ")),
        "This usually means replicate scans haven't been averaged yet",
        "Try: x |> average() |> add_response(...)")
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Prepare join
  ## ---------------------------------------------------------------------------

  ## Select only join key + variable columns from source -----------------------

  join_cols   <- c(by_source, variable)
  source_slim <- source[, join_cols, drop = FALSE]

  ## ---------------------------------------------------------------------------
  ## Step 3: Diagnose match quality
  ## ---------------------------------------------------------------------------

  horizons_ids <- x$data$analysis[[by_horizons]]
  source_ids   <- source_slim[[by_source]]
  n_horizons   <- length(horizons_ids)
  n_matched    <- sum(horizons_ids %in% source_ids)
  n_unmatched  <- n_horizons - n_matched
  match_rate   <- n_matched / n_horizons

  ## Zero matches — abort with diagnostic ------------------------------------

  if (n_matched == 0) {

    ## Detect common patterns
    details <- c(
      paste0("0/", n_horizons, " samples matched")
    )

    ## Show sample IDs from both sides
    h_preview <- paste0('"', head(horizons_ids, 3), '"', collapse = ", ")
    s_preview <- paste0('"', head(source_ids, 3), '"', collapse = ", ")
    details   <- c(details,
                   paste0("Horizons IDs: ", h_preview, if (n_horizons > 3) ", ..."),
                   paste0("Source IDs: ", s_preview, if (length(source_ids) > 3) ", ..."))

    ## Check for case mismatch
    n_case_match <- sum(tolower(horizons_ids) %in% tolower(source_ids))

    if (n_case_match > 0) {

      details <- c(details,
                   "This looks like a case mismatch",
                   paste0("Try: mutate(source, ", by_source, " = tolower(", by_source, "))"))

    }

    ## Check for whitespace differences
    n_ws_match <- sum(trimws(horizons_ids) %in% trimws(source_ids))

    if (n_ws_match > 0) {

      details <- c(details,
                   "This looks like a whitespace issue",
                   paste0("Try: mutate(source, ", by_source, " = trimws(", by_source, "))"))

    }

    abort_nested(
      "No matching samples between horizons data and source",
      details
    )

  }

  ## Low match rate (<50%) — warn with pattern detection ----------------------

  if (match_rate < 0.5) {

    warn_details <- c(
      paste0(n_matched, "/", n_horizons, " samples matched")
    )

    ## Check for case mismatch
    n_case_match <- sum(tolower(horizons_ids) %in% tolower(source_ids))

    if (n_case_match > n_matched) {

      warn_details <- c(warn_details,
                        "This looks like a case mismatch",
                        paste0("Try: mutate(source, ", by_source,
                               " = tolower(", by_source, "))"))

    }

    ## Check for whitespace
    n_ws_match <- sum(trimws(horizons_ids) %in% trimws(source_ids))

    if (n_ws_match > n_matched) {

      warn_details <- c(warn_details,
                        "This looks like a whitespace issue",
                        paste0("Try: mutate(source, ", by_source,
                               " = trimws(", by_source, "))"))

    }

    cat(cli::col_yellow(paste0("\u2502  \u251C\u2500 ", warn_details[1], "\n")))
    for (i in seq_along(warn_details)[-1]) {
      branch <- if (i < length(warn_details)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_yellow(paste0("\u2502  \u2502     ", branch, " ", warn_details[i], "\n")))
    }

    warning(
      paste0(n_matched, "/", n_horizons, " samples matched — ",
             n_unmatched, " have no matching response data"),
      call. = FALSE
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Execute join
  ## ---------------------------------------------------------------------------

  ## Build the by argument for dplyr::left_join() ----------------------------

  join_by <- stats::setNames(by_source, by_horizons)

  x$data$analysis <- dplyr::left_join(
    x$data$analysis,
    source_slim,
    by = join_by
  )

  ## ---------------------------------------------------------------------------
  ## Step 5: Update object
  ## ---------------------------------------------------------------------------

  ## Add response variables to role_map ----------------------------------------

  new_roles <- tibble::tibble(
    variable = variable,
    role     = rep("response", length(variable))
  )

  x$data$role_map  <- dplyr::bind_rows(x$data$role_map, new_roles)
  x$data$n_responses <- sum(x$data$role_map$role == "response")

  ## Record provenance ---------------------------------------------------------

  prov_entry <- list(
    source      = source_label,
    variables   = variable,
    by          = by,
    n_matched   = n_matched,
    n_unmatched = n_unmatched,
    applied_at  = Sys.time()
  )

  if (is.null(x$provenance$add_response)) {

    x$provenance$add_response <- list(prov_entry)

  } else {

    x$provenance$add_response <- c(x$provenance$add_response, list(prov_entry))

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Progress output
  ## ---------------------------------------------------------------------------

  cat(paste0("\u2502  \u251C\u2500 Source: ", source_label, "\n"))
  cat(paste0("\u2502  \u251C\u2500 Matched: ", n_matched, "/", n_horizons, " samples"))

  if (n_unmatched > 0 && match_rate >= 0.5) {

    cat(paste0(" (", n_unmatched, " unmatched \u2192 NA)"))

    warning(
      paste0(n_unmatched, "/", n_horizons,
             " samples have no matching response data (NA values assigned)"),
      call. = FALSE
    )

  }

  cat("\n")
  cat(paste0("\u2502  \u2514\u2500 Variables added: ",
             paste(variable, collapse = ", "), "\n"))

  ## ---------------------------------------------------------------------------
  ## Step 7: Return
  ## ---------------------------------------------------------------------------

  x

}
