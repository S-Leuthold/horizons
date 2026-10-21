#' Pipeline: Join Response Data
#'
#' @description
#' Joins lab-measured response data to a `horizons_data` object. This is the
#' bridge between spectral preprocessing and modeling - everything downstream
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
#'   joined. CSV only - for other formats, read the data yourself and pass
#'   the tibble.
#'
#' **Join behavior:**
#'
#' Left-join semantics matching `dplyr::left_join()` conventions. Strict exact
#' matching - no case-folding or fuzzy matching. When no sample matches, the
#' join is refused; when fewer than half match, it runs and warns
#' (`horizons_warning_coverage`, naming the unmatched samples on the
#' object's log); when half or more match, the unmatched samples are named
#' in a note on the log and counted in verbose output. The refusal and the
#' warning say when a case or whitespace difference would match more samples,
#' and how to fix it.
#'
#' **Missing values:**
#'
#' "Matched" counts samples whose join key is found in the source. A matched
#' sample whose value is `NA` in the source is matched but unmeasured, so the
#' console also reports, and the call's note on the log records, the
#' non-missing count of each variable. Rows with missing values are kept: an
#' object can carry several responses (`select_training()` puts every pool
#' response on one object), and a row missing one of them may carry another. `evaluate()` and
#' `fit()` drop the rows whose outcome is `NA` when they model it.
#'
#' An `NA` join key on either side is refused before anything is joined, and
#' the error gives the number of `NA` keys on each side. An `NA` key
#' identifies no sample; left in, it would be matched to an `NA` key on the
#' other side. On the horizons side one is usually left by `parse_ids(too_few
#' = "na")`: resolve it with `parse_ids()` or drop the row. `average()`
#' refuses an `NA` grouping value for the same reason, so a chain that
#' already ran `average()` cannot reach `add_response()` with an `NA`
#' `sample_id`. On the source side, drop the rows that have no key.
#'
#' **Duplicate detection:**
#'
#' Aborts if duplicates exist in either the source join key (would multiply
#' spectral rows) or the horizons join key (indicates replicates haven't been
#' averaged - suggests `average()` first).
#'
#' **Repeated calls:**
#'
#' Can be called multiple times to join variables from different sources.
#' Aborts if a variable name already exists in the analysis tibble.
#'
#' Joined variables receive `role = "response"` in the role map. Use
#' `configure()` to promote one response to `role = "outcome"` for modeling.
#'
#' @param x `horizons_data`. Object from pipeline (post-`average()`, unique
#'   `sample_id` required).
#' @param source `tibble, data.frame, or character path`. Response data source.
#'   If a path, read via `readr::read_csv()`.
#' @param variable `character vector`. Column name(s) in the source to join as
#'   response variables. Required - no default. Must be numeric columns.
#' @param by `character`. Join key. Default `"sample_id"`. Use a named vector
#'   for mismatched column names: `c("sample_id" = "Lab_ID")` where the name
#'   is the horizons-side column and the value is the source-side column.
#' @param verbose `logical(1)`. Print the join's report. Default:
#'   `getOption("horizons.verbose", TRUE)`, as every verb's.
#'
#' @return A modified `horizons_data` object with:
#'   * Response columns added to `data$analysis`
#'   * New entries in `data$role_map` with `role = "response"`
#'   * Updated `data$n_responses` count
#'   * A note on the object's `log` recording the call, after any coverage
#'     warning or note: the variables, which are its `ids`, the source (its
#'     path, or a data frame), the key, the samples matched and the measured
#'     values per variable
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
                         by      = "sample_id",
                         verbose = getOption("horizons.verbose", TRUE)) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  ## 1.1 x must be horizons_data -----------------------------------------------

  if (!inherits(x, "horizons_data")) {

    abort_horizons(
      "{.arg x} must be a horizons_data object, not {.obj_type_friendly {x}}.",
      kind = "input"
    )

  }

  ## 1.1b The object as it arrives (#234) ------------------------------------------

  x <- validate_horizons_data(x)

  check_arg(rlang::check_bool, verbose)

  ## 1.2 source type check -----------------------------------------------------

  is_path   <- is.character(source) && length(source) == 1
  is_tibble <- inherits(source, "data.frame")

  if (!is_path && !is_tibble) {

    abort_horizons(
      "{.arg source} must be a data frame or the path of a CSV file, not {.obj_type_friendly {source}}.",
      kind = "input"
    )

  }

  ## 1.3 If path: validate file exists, read CSV, record source label ----------

  if (is_path) {

    if (!file.exists(source)) {

      abort_horizons(c(
        "The source file does not exist.",
        "x" = "Path: {.file {source}}."
      ), kind = "input")

    }

    source_label <- source
    source       <- readr::read_csv(source, show_col_types = FALSE)

  } else {

    source_label <- "tibble"

  }

  ## 1.4 variable is non-empty and unique ----------------------------------------
  ### rlang exports no checker for a character vector.

  if (!is.character(variable) || length(variable) == 0) {

    abort_horizons(
      "{.arg variable} must be a character vector of column names, not {.obj_type_friendly {variable}}.",
      kind = "input"
    )

  }

  if (anyDuplicated(variable)) {

    dup_vars <- unique(variable[duplicated(variable)])

    abort_horizons(c(
      "{.arg variable} names a column more than once.",
      "x" = "Repeated: {.field {dup_vars}}."
    ), kind = "input")

  }

  ## 1.5 variable columns exist in source --------------------------------------

  missing_vars <- setdiff(variable, names(source))

  if (length(missing_vars) > 0) {

    abort_horizons(c(
      "{cli::qty(missing_vars)}{?A variable is/Variables are} not in the source data.",
      "x" = "Missing: {.field {missing_vars}}.",
      "i" = "Available: {.field {names(source)}}."
    ), kind = "input")

  }

  ## 1.5 variable columns are numeric ------------------------------------------

  non_numeric <- variable[!vapply(source[variable], is.numeric, logical(1))]

  if (length(non_numeric) > 0) {

    non_numeric_types <- vapply(
      source[non_numeric], function(col) class(col)[1], character(1)
    )
    type_strs <- paste0(non_numeric, " (", non_numeric_types, ")")

    abort_horizons(c(
      "Response variables must be numeric.",
      "x" = "Not numeric: {type_strs}."
    ), kind = "input")

  }

  ## 1.6 variable names don't already exist in horizons data -------------------

  existing <- intersect(variable, names(x$data$analysis))

  if (length(existing) > 0) {

    current_responses <- x$data$role_map$variable[x$data$role_map$role == "response"]

    abort_horizons(c(
      "{cli::qty(existing)}{?A variable is/Variables are} already in the data.",
      "x" = "Already present: {.field {existing}}.",
      "i" = if (length(current_responses)) "Current responses: {.field {current_responses}}.",
      "i" = "Rename the source column, or join a different variable."
    ), kind = "input")

  }

  ## 1.7 Parse `by` and validate join columns exist in both datasets -----------

  ## Only single-column joins supported
  if (length(by) > 1) {

    abort_horizons(c(
      "{.arg by} must name one join column, not {length(by)}.",
      "x" = "Got: {.field {by}}.",
      "i" = "Make a composite key column first, then join on it."
    ), kind = "input")

  }

  ### A `by` of length one is named or not; it cannot be partly named.
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

    abort_horizons(c(
      "The join column {.field {by_horizons}} is not in the horizons data.",
      "i" = "Available: {.field {names(x$data$analysis)}}."
    ), kind = "input")

  }

  if (!by_source %in% names(source)) {

    abort_horizons(c(
      "The join column {.field {by_source}} is not in the source data.",
      "i" = "Available: {.field {names(source)}}."
    ), kind = "input")

  }

  ## 1.8 No NA keys on either side --------------------------------------------
  ## An NA key identifies no sample. Left in, an NA on each side would pass
  ## the duplicate checks below and be joined to each other.

  n_na_horizons <- sum(is.na(x$data$analysis[[by_horizons]]))
  n_na_source   <- sum(is.na(source[[by_source]]))

  if (n_na_horizons > 0 || n_na_source > 0) {

    na_counts <- c(
      if (n_na_horizons > 0) paste0(n_na_horizons, " in horizons data ('", by_horizons, "')"),
      if (n_na_source > 0)   paste0(n_na_source, " in source ('", by_source, "')")
    )

    abort_horizons(c(
      "NA values in join key: {paste(na_counts, collapse = ', ')}.",
      "i" = "An NA key identifies no sample, so it cannot be joined.",
      "i" = "Drop the rows without a key, or fill their keys in, then join."
    ), kind = "input")

  }

  ## 1.9 No duplicate keys in source ------------------------------------------

  source_keys    <- source[[by_source]]
  source_dup_idx <- duplicated(source_keys)

  if (any(source_dup_idx)) {

    dup_vals   <- unique(source_keys[source_dup_idx])
    dup_counts <- table(source_keys)[as.character(dup_vals)]
    dup_strs   <- paste0('"', names(dup_counts), '" (', dup_counts, "x)")

    abort_horizons(c(
      "Duplicate values in source join key {.field {by_source}}.",
      "x" = "Duplicated: {dup_strs}.",
      "i" = "Each sample must appear exactly once in the source data."
    ), kind = "input")

  }

  ## 1.10 No duplicate keys in horizons ------------------------------------------
  ## The door validator refuses duplicate sample_ids, with average() as the
  ## remedy, so this is reached only by a meta key.

  horizons_keys    <- x$data$analysis[[by_horizons]]
  horizons_dup_idx <- duplicated(horizons_keys)

  if (any(horizons_dup_idx)) {

    dup_vals   <- unique(horizons_keys[horizons_dup_idx])
    dup_counts <- table(horizons_keys)[as.character(dup_vals)]
    dup_strs   <- paste0('"', names(dup_counts), '" (', dup_counts, "x)")

    abort_horizons(c(
      "Duplicate values in horizons join key {.field {by_horizons}}.",
      "x" = "Duplicated: {dup_strs}.",
      "i" = "Each sample must have its own key value to join on."
    ), kind = "input")

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
  matched      <- horizons_ids %in% source_ids
  n_matched    <- sum(matched)
  n_unmatched  <- n_horizons - n_matched
  match_rate   <- n_matched / n_horizons

  ## The case and whitespace hints, when either would match more samples ------

  match_hints <- c(
    if (sum(tolower(horizons_ids) %in% tolower(source_ids)) > n_matched) {
      c("i" = "This looks like a case mismatch: try {.code mutate(source, {by_source} = tolower({by_source}))}.")
    },
    if (sum(trimws(horizons_ids) %in% trimws(source_ids)) > n_matched) {
      c("i" = "This looks like a whitespace issue: try {.code mutate(source, {by_source} = trimws({by_source}))}.")
    }
  )

  ## Zero matches: nothing to join ---------------------------------------------

  if (n_matched == 0) {

    h_preview <- paste0('"', utils::head(horizons_ids, 3), '"', collapse = ", ")
    s_preview <- paste0('"', utils::head(source_ids, 3), '"', collapse = ", ")

    abort_horizons(c(
      "No sample in the horizons data matches the source.",
      "x" = "0 of {n_horizons} samples matched on {.field {by_horizons}}.",
      "i" = "Horizons IDs: {h_preview}{if (n_horizons > 3) ', ...' else ''}.",
      "i" = "Source IDs: {s_preview}{if (length(source_ids) > 3) ', ...' else ''}.",
      match_hints
    ), kind = "input")

  }

  collector     <- log_open("add_response")
  unmatched_ids <- as.character(x$data$analysis$sample_id[!matched])

  ## Under half matched: the result is weaker than its output shows ------------
  ## Half or more: the unmatched samples are reported, not warned.

  low_match <- match_rate < 0.5

  if (low_match) {

    log_warning(collector, "coverage", c(
      "{n_matched} of {n_horizons} samples matched the source, so {n_unmatched} {?has/have} no response values.",
      match_hints
    ), ids = unmatched_ids)

  } else if (n_unmatched > 0) {

    log_note(collector,
             "{n_unmatched} of {n_horizons} samples matched nothing in the source, so their responses are {.code NA}.",
             ids = unmatched_ids)

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Execute join
  ## ---------------------------------------------------------------------------

  ## Build the by argument for dplyr::left_join() ----------------------------

  join_by <- stats::setNames(by_source, by_horizons)

  ## Join onto the key columns only; the table itself is written below -------
  ### NA keys were refused in 1.8; na_matches = "never" states the same rule.

  key_cols <- intersect(c("sample_id", by_horizons), names(x$data$analysis))

  joined <- dplyr::left_join(
    x$data$analysis[, key_cols, drop = FALSE],
    source_slim,
    by         = join_by,
    na_matches = "never"
  )

  ## ---------------------------------------------------------------------------
  ## Step 5: Update object
  ## ---------------------------------------------------------------------------

  ## Count measured values per variable ----------------------------------------
  ## n_matched counts join-key hits; a matched sample whose source value is NA
  ## is matched but unmeasured. Count what each variable actually carries.

  n_non_missing <- vapply(variable,
                          function(v) sum(!is.na(joined[[v]])),
                          integer(1))

  ## Add the response columns, their roles and the counts ----------------------
  ### sample_id rides along as the row key: add_columns() refuses the columns
  ### unless the join kept the table's rows in order, then drops it.

  x <- add_columns(x,
                   columns = joined[, intersect(c("sample_id", variable), names(joined)), drop = FALSE],
                   role    = "response")

  ## ---------------------------------------------------------------------------
  ## Step 5b: Re-validate
  ## ---------------------------------------------------------------------------

  x <- validate_horizons_data(x)

  ## The call's record: what was joined, from where --------------------------
  ## A note, never raised, after the coverage event; its ids are the
  ## variables joined.

  from     <- if (identical(source_label, "tibble")) "a data frame" else "{.file {source_label}}"
  on       <- if (identical(by_horizons, by_source)) "{.field {by_horizons}}" else "{.field {by_horizons}} = {.field {by_source}}"
  measured <- paste0(variable, " ", n_non_missing, collapse = ", ")

  log_note(collector,
           paste0("Joined {.field {variable}} from ", from, " on ", on,
                  ": {n_matched} of {n_horizons} samples matched, with measured values per variable: {measured}."),
           ids = variable)

  x$log <- log_close(collector, x$log)

  ## ---------------------------------------------------------------------------
  ## Step 6: Progress output
  ## ---------------------------------------------------------------------------

  if (verbose) {

    matched_str <- paste0(n_matched, "/", n_horizons, " samples")

    if (n_unmatched > 0 && !low_match) {

      matched_str <- paste0(matched_str, " (", n_unmatched, " unmatched \u2192 NA)")

    }

    ## Under half matched the line is yellow; its hints are in the warning.
    matched_line <- paste0("Matched: ", matched_str)

    cat(paste0("\u251C\u2500 ", cli::style_bold("Adding response data"), "...\n"))
    cat(paste0("\u2502  \u251C\u2500 Source: ", source_label, "\n"))
    cat(paste0("\u2502  \u251C\u2500 ",
               if (low_match) cli::col_yellow(matched_line) else matched_line, "\n"))
    cat("\u2502  \u2514\u2500 Variables:\n")

    for (i in seq_along(variable)) {
      branch <- if (i < length(variable)) "\u251C\u2500" else "\u2514\u2500"
      cat(paste0("\u2502     ", branch, " ", variable[i], ": ",
                 n_non_missing[[i]], "/", n_horizons, " non-missing\n"))
    }
    cat("\u2502\n")

  }

  signal_log(collector, x)

  ## ---------------------------------------------------------------------------
  ## Step 7: Return
  ## ---------------------------------------------------------------------------

  x

}
