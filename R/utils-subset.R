# R/utils-subset.R
# Row-level operations on horizons_data (issue #43). Two internals: replace
# the analysis table and recompute every derived count, and select rows by
# sample_id.


## ---------------------------------------------------------------------------
## set_analysis() — Replace the analysis table and recompute counts
## ---------------------------------------------------------------------------

#' Replace the analysis table of a horizons_data object
#'
#' @description
#' Sets `x$data$analysis` (and optionally `x$data$role_map`) and recomputes
#' `n_rows`, `n_predictors`, `n_covariates` and `n_responses` from the role
#' map, so the counts have one source of truth. Every verb that changes the
#' shape of the analysis table goes through here.
#'
#' @details
#' The role map and the analysis columns must agree exactly; a mismatch is
#' an input error, not something this function repairs. Structural
#' validation (`validate_horizons_data()`) is the caller's job, because a
#' verb may make several changes before the object is whole again.
#'
#' @param x [horizons_data.] The object to update.
#' @param analysis [Tibble.] The new analysis table.
#' @param role_map [Tibble or NULL.] A new role map, or `NULL` to keep the
#'   existing one. Default: `NULL`.
#'
#' @return [horizons_data.] `x` with the table and counts replaced. The input
#'   is not modified.
#'
#' @seealso [subset_rows()], [validate_horizons_data()]
#' @noRd
set_analysis <- function(x, analysis, role_map = NULL) {

  if (!inherits(x, "horizons_data")) {

    cli::cli_abort("{.arg x} must be a {.cls horizons_data}, not {.cls {class(x)[1]}}",
                   class = "horizons_input_error")

  }

  if (is.null(role_map)) role_map <- x$data$role_map

  ## Columns and roles must agree exactly -------------------------------------

  missing_roles <- setdiff(names(analysis), role_map$variable)
  missing_cols  <- setdiff(role_map$variable, names(analysis))

  if (length(missing_roles) || length(missing_cols)) {

    cli::cli_abort(c(
      "{.arg analysis} and {.arg role_map} disagree",
      "x" = if (length(missing_roles)) "Columns without a role: {.field {missing_roles}}",
      "x" = if (length(missing_cols))  "Roles without a column: {.field {missing_cols}}"
    ), class = "horizons_input_error")

  }

  x$data$analysis     <- analysis
  x$data$role_map     <- role_map
  x$data$n_rows       <- nrow(analysis)
  x$data$n_predictors <- sum(role_map$role == "predictor", na.rm = TRUE)
  x$data$n_covariates <- sum(role_map$role == "covariate", na.rm = TRUE)
  x$data$n_responses  <- sum(role_map$role == "response",  na.rm = TRUE)

  x

}


## ---------------------------------------------------------------------------
## subset_rows() — Select rows by sample_id
## ---------------------------------------------------------------------------

#' Select rows of a horizons_data object
#'
#' @description
#' Returns `x` with only the rows in `keep`, in the order `keep` gives them
#' when `keep` is a character vector of `sample_id`s. Columns, roles and
#' everything outside `x$data` are unchanged.
#'
#' @details
#' Before this existed, rows were dropped inline in `validate()` and rebuilt
#' in `average()`, each maintaining the same small invariant by hand. Any
#' operation that needs a subset of samples (a per-group fit, a holdout, a
#' training set drawn from a pool) should use this rather than subsetting the
#' source tibble before `spectra()`, which discards provenance.
#'
#' @param x [horizons_data.] The object to subset.
#' @param keep [Character or logical.] `sample_id`s to keep, or a logical
#'   vector of length `n_rows`. Every id must exist and none may repeat.
#' @param reason [Character or NULL.] Why the subset was taken, stored in the
#'   provenance entry. Default: `NULL`.
#' @param record [Logical.] Append an entry to `x$provenance$subset_rows`.
#'   Set `FALSE` when the calling verb records the operation itself.
#'   Default: `TRUE`.
#'
#' @return [horizons_data.] The subset. Errors of class
#'   `horizons_input_error` on unknown, duplicated or empty `keep`.
#'
#' @seealso [set_analysis()]
#' @noRd
subset_rows <- function(x, keep, reason = NULL, record = TRUE) {

  if (!inherits(x, "horizons_data")) {

    cli::cli_abort("{.arg x} must be a {.cls horizons_data}, not {.cls {class(x)[1]}}",
                   class = "horizons_input_error")

  }

  analysis <- x$data$analysis
  ids      <- analysis$sample_id
  n_before <- nrow(analysis)

  ## Resolve keep to a row index ----------------------------------------------

  if (is.logical(keep)) {

    if (length(keep) != n_before) {

      cli::cli_abort(c(
        "A logical {.arg keep} must have one entry per row",
        "x" = "Got {length(keep)} for {n_before} rows"
      ), class = "horizons_input_error")

    }

    rows <- which(keep)

  } else if (is.character(keep)) {

    dup <- keep[duplicated(keep)]

    if (length(dup)) {

      cli::cli_abort(c(
        "{.arg keep} contains duplicated ids",
        "x" = "{.val {unique(dup)}}"
      ), class = "horizons_input_error")

    }

    unknown <- setdiff(keep, ids)

    if (length(unknown)) {

      shown <- utils::head(unknown, 5)

      cli::cli_abort(c(
        "{length(unknown)} id{?s} in {.arg keep} {?is/are} not in the data",
        "x" = "{.val {shown}}{if (length(unknown) > 5) ', ...' else ''}"
      ), class = "horizons_input_error")

    }

    rows <- match(keep, ids)

  } else {

    cli::cli_abort("{.arg keep} must be character ids or a logical vector, not {.cls {class(keep)[1]}}",
                   class = "horizons_input_error")

  }

  if (!length(rows)) {

    cli::cli_abort("{.arg keep} selects no rows", class = "horizons_input_error")

  }

  ## Apply ---------------------------------------------------------------------

  x <- set_analysis(x, analysis[rows, , drop = FALSE])

  if (isTRUE(record)) {

    entry <- list(
      n_before   = n_before,
      n_after    = length(rows),
      reason     = reason,
      applied_at = Sys.time()
    )

    x$provenance$subset_rows <- c(x$provenance$subset_rows, list(entry))

  }

  x

}
