# R/utils-subset.R
# Row-level operations on horizons_data (issue #43). Two internals: replace
# the analysis table and recompute every derived count, and select rows by
# sample_id. Both refuse to run on a promoted object, because everything a
# promotion earns is keyed to a row order these functions destroy.


## ---------------------------------------------------------------------------
## promoted_state() — Name the downstream state a row operation would strand
## ---------------------------------------------------------------------------

#' Describe the promoted state carried by a horizons object
#'
#' @description
#' Returns human-readable names for every piece of downstream state that a
#' row-level operation would leave keyed to rows that no longer exist. An
#' empty character vector means the object is a plain, unpromoted
#' `horizons_data` and row operations are safe.
#'
#' @details
#' The `evaluation`, `models` and `ensemble` slots are present on every
#' object, carrying constructor defaults, so the presence of a slot is not
#' evidence of promotion. The test is on the specific fields a verb writes
#' when it earns the promotion: `evaluation$results` and `evaluation$split`
#' from `evaluate()`, `models$workflows`, `models$split` and
#' `models$row_index` from `fit()`, `ensemble$method` or `ensemble$model`
#' from `ensemble()`. Anything narrower would miss a promotion; anything
#' broader trips on a default (`models$uq` is `list(enabled = FALSE)` on
#' objects `spectra()` built before it used `new_horizons_data()`, which is
#' not a fitted model).
#'
#' [reset_promotion()] clears everything this function names, and lives
#' beside it so the two stay in step.
#'
#' The class vector is promotion in its own right. `evaluate()` and `fit()`
#' prepend `horizons_eval` and `horizons_fit`, and a class carrying one of
#' those is a claim about state that a row operation would falsify, whether
#' or not the particular slot this function knows about happens to be
#' filled.
#'
#' @param x [horizons_data.] The object to inspect.
#'
#' @return [Character.] Names of the filled downstream slots, possibly empty.
#'
#' @seealso [set_analysis()], [subset_rows()], [reset_promotion()]
#' @noRd
promoted_state <- function(x) {

  states <- character()

  if (!is.null(x$evaluation$results))   states <- c(states, "evaluation results")
  if (!is.null(x$evaluation$split))     states <- c(states, "an evaluation split")
  if (!is.null(x$models$workflows))     states <- c(states, "fitted models")
  if (!is.null(x$models$split))         states <- c(states, "a model split")
  if (!is.null(x$models$row_index))     states <- c(states, "a row index")

  if (!is.null(x$ensemble$method) || !is.null(x$ensemble$model)) {

    states <- c(states, "an ensemble")

  }

  if (!identical(class(x), c("horizons_data", "list"))) {

    states <- c(states, paste0("the class ", class(x)[1]))

  }

  states

}


## ---------------------------------------------------------------------------
## reset_promotion() — Return a promoted object to a plain horizons_data
## ---------------------------------------------------------------------------

#' Clear everything a promotion earned
#'
#' @description
#' Returns `x` with the `evaluation`, `models` and `ensemble` slots back in
#' the shape [new_horizons_data()] gives them, the validation verdict
#' cleared, and the class demoted to `c("horizons_data", "list")`. After it,
#' [promoted_state()] is empty.
#'
#' @details
#' This undoes what `evaluate()`, `fit()` and `ensemble()` write, and it
#' lives beside [promoted_state()] so the two stay in step. The three slots
#' are replaced whole from the constructor rather than cleared key by key,
#' so a key a verb gains later is cleared without anyone having to add it
#' to a list here.
#'
#' Two things survive on purpose:
#'
#' * The record of rows `validate()` already removed (`removed_ids`,
#'   `removal_detail` and `removed` in `validation$outliers`). Those rows are
#'   gone from the analysis table and stay gone, so the record is still
#'   true. The verdict (`passed`, `checks`, `timestamp`) and the flagged ids
#'   (`spectral_ids`, `response_ids`) are cleared, because `validate()`
#'   recomputes them for the rows and outcome in front of it.
#' * `x$selection`. The `select_training()` record describes which rows were
#'   drawn, not the outcome they are modelled for, and `fit()` reads its
#'   presence into `models$selection_present`.
#'
#' `x$config` is the caller's to replace; `configure()` overwrites it.
#'
#' @param x [horizons_data.] The object to reset.
#'
#' @return [horizons_data.] `x` with class `c("horizons_data", "list")` and
#'   an empty [promoted_state()].
#'
#' @seealso [promoted_state()], [configure()]
#' @noRd
reset_promotion <- function(x) {

  ## Downstream slots go back to the constructor's shape ----------------------

  x <- reset_slots(x, c("evaluation", "models", "ensemble"))

  ## Validation: clear the verdict, keep the record of removed rows -----------

  ### Single-bracket assignment of list(value) keeps the key when value is NULL.
  outliers     <- x$validation$outliers
  x$validation <- new_horizons_data()$validation

  x$validation$outliers["removed_ids"]    <- list(outliers$removed_ids)
  x$validation$outliers["removal_detail"] <- list(outliers$removal_detail)
  x$validation$outliers["removed"]        <- list(outliers$removed %||% FALSE)

  ## The class is the claim that the state is there --------------------------

  class(x) <- c("horizons_data", "list")

  x

}


## ---------------------------------------------------------------------------
## reset_slots() — Return named slots to the constructor's shape
## ---------------------------------------------------------------------------

#' Return slots to the shape new_horizons_data() gives them
#'
#' @description
#' Replaces each named slot of `x` with the constructor's empty version, key
#' for key. The building block of [reset_promotion()], and what a verb that
#' re-runs uses to clear the slots downstream of its own: `evaluate()` resets
#' `models` and `ensemble` before writing `evaluation`, and `fit()` resets
#' `ensemble` before writing `models`, so a re-run never leaves a later
#' verb's state (UQ bundles, a meta-learner) describing models that are no
#' longer there. The class is the calling verb's to set.
#'
#' @param x [horizons_data.] The object to reset.
#' @param slots [Character.] Slot names; each must be a section of
#'   [new_horizons_data()].
#'
#' @return [horizons_data.] `x` with those slots replaced.
#'
#' @seealso [reset_promotion()], [promoted_state()]
#' @noRd
reset_slots <- function(x, slots) {

  blank   <- new_horizons_data()
  unknown <- setdiff(slots, names(blank))

  if (length(unknown)) {

    cli::cli_abort("{.fn reset_slots} does not know {.field {unknown}}",
                   class = "horizons_input_error")

  }

  ### list(value) keeps the key even for a slot whose empty value is NULL.
  for (slot in slots) {

    x[slot] <- list(blank[[slot]])

  }

  x

}


## ---------------------------------------------------------------------------
## check_unpromoted() — Abort when a row operation would strand state
## ---------------------------------------------------------------------------

#' Refuse a row operation on a promoted object
#'
#' @description
#' Aborts when `x` carries any state from [evaluate()], [fit()] or
#' [ensemble()], naming what it found. Silent otherwise.
#'
#' @param x [horizons_data.] The object to check.
#' @param fn [Character.] The calling function's name, for the message.
#'
#' @return [NULL.] Called for its side effect. Aborts with class
#'   `horizons_input_error` when `x` is promoted.
#'
#' @seealso [promoted_state()]
#' @noRd
check_unpromoted <- function(x, fn) {

  states <- promoted_state(x)

  if (!length(states)) {

    return(invisible(NULL))

  }

  cli::cli_abort(c(
    "{.fn {fn}} needs an unpromoted {.cls horizons_data}",
    "x" = "This object carries {states}",
    "i" = "Changing rows now would leave that state keyed to rows the object no longer has",
    "i" = "Select rows before {.fn evaluate}, or start again from the object you evaluated"
  ), class = "horizons_input_error")

}


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
#' When `x` carries a `select_training()` record and the row set changes,
#' the record is recomputed here rather than by the caller, because every
#' verb that drops rows goes through this function and a record left
#' describing the pre-change object is stale in a way nothing downstream
#' notices. The new ids must be a subset of the old ones: a caller that
#' renames rows (`average(by = )` promoting a grouping column) has broken
#' the link between the record and the object, and that is refused rather
#' than silently dropping the record.
#'
#' The object must be unpromoted. An object carrying evaluation results,
#' fitted models or an ensemble is refused by name, because `models$split`,
#' `models$row_index` and `evaluation$results` are keyed to a row order this
#' function is free to change, and nothing downstream would notice.
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

  check_unpromoted(x, "set_analysis")

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

  old_ids <- x$data$analysis$sample_id

  x$data$analysis     <- analysis
  x$data$role_map     <- role_map
  x$data$n_rows       <- nrow(analysis)
  x$data$n_predictors <- sum(role_map$role == "predictor", na.rm = TRUE)
  x$data$n_covariates <- sum(role_map$role == "covariate", na.rm = TRUE)
  x$data$n_responses  <- sum(role_map$role == "response",  na.rm = TRUE)

  ## Keep the selection record describing the rows that are actually here ------

  if (!is.null(x$selection) && !is.null(old_ids)) {

    new_ids <- analysis$sample_id

    if (!setequal(old_ids, new_ids)) {

      unknown <- setdiff(new_ids, old_ids)

      if (length(unknown)) {

        cli::cli_abort(c(
          "{.fn set_analysis} cannot maintain the selection record",
          "x" = "{length(unknown)} new {.field sample_id}{?s} {?is/are} not in the object the draw described",
          "i" = "A record can be refiltered when rows leave, not when they are renamed"
        ), class = "horizons_input_error")

      }

      x$selection <- subset_selection(selection = x$selection,
                                      keep_ids  = new_ids,
                                      n_removed = length(setdiff(old_ids, new_ids)))

    }

  }

  x

}


## ---------------------------------------------------------------------------
## subset_rows() — Select rows by sample_id
## ---------------------------------------------------------------------------

#' Select rows of a horizons_data object
#'
#' @description
#' Returns `x` with only the rows in `keep`, in the order `keep` gives them
#' when `keep` is a character vector of `sample_id`s. Columns and roles are
#' unchanged. The selection record, when there is one, is recomputed for the
#' surviving rows; everything else outside `x$data` is unchanged.
#'
#' @details
#' Before this existed, rows were dropped inline in `validate()` and rebuilt
#' in `average()`, each maintaining the same small invariant by hand. Any
#' internal operation that needs a subset of samples (a per-group fit, a
#' holdout, a training set drawn from a pool) should use this rather than
#' subsetting the source tibble before `spectra()`, which discards
#' provenance.
#'
#' The object must be unpromoted. An object carrying evaluation results,
#' fitted models or an ensemble is refused by name: the split, the row index
#' and the fitted workflows are all keyed to a row order this function
#' changes, and a subset taken after `evaluate()` would produce right-looking
#' numbers against the wrong samples.
#'
#' When `x` carries a selection record from `select_training()`, the record
#' is recomputed rather than left describing the pre-subset object.
#' `membership` is filtered to surviving `pool_id`s, each group's `pool_ids`
#' is filtered and `n_rows` recounted, and `pool_sizes$drawn` is recounted
#' per property. `exclusions`, `resemblance`, `target_distances`,
#' `settings` and `clustering` describe the draw itself and are left alone,
#' and the running count of rows that have left since the draw is recorded
#' in `selection$rows_removed`. The per-row `.drawn_by`, `.min_distance` and
#' `.group` columns are per-row facts and survive the subset unchanged.
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
#'   `horizons_input_error` on a promoted object, or on unknown, duplicated
#'   or empty `keep`.
#'
#' @seealso [set_analysis()], [subset_selection()]
#' @noRd
subset_rows <- function(x, keep, reason = NULL, record = TRUE) {

  if (!inherits(x, "horizons_data")) {

    cli::cli_abort("{.arg x} must be a {.cls horizons_data}, not {.cls {class(x)[1]}}",
                   class = "horizons_input_error")

  }

  check_unpromoted(x, "subset_rows")

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

  ## set_analysis() recomputes the selection record for the surviving rows.
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


## ---------------------------------------------------------------------------
## subset_selection() — Recompute the selection record for surviving rows
## ---------------------------------------------------------------------------

#' Recompute a selection record after rows have left
#'
#' @description
#' Filters the row-level parts of a `select_training()` record to the rows
#' that are still in the object, and recounts the two derived counts that
#' depend on them. The parts that describe the draw rather than the result
#' are left as they were.
#'
#' @details
#' Three things are recomputed. `membership` keeps only the rows whose
#' `pool_id` survived, except for rows already marked `retained = FALSE` —
#' those record a neighbour the union subtraction dropped, were never in the
#' analysis table, and are kept so the draw stays readable. Each group's
#' `pool_ids` is filtered to the survivors and `n_rows` is recounted from it.
#' `pool_sizes$drawn` is recounted per property from the retained membership
#' rows — except under `scope = "global"`, where the draw is the whole pool,
#' so every surviving row is drawn for every property.
#'
#' Scope is read from `settings$scope` rather than inferred from an empty
#' `membership`: a batch draw whose rows have all left is also empty, and
#' treating it as global would report the whole object as drawn.
#'
#' `exclusions`, `resemblance`, `target_distances`, `settings`,
#' `clustering` and `pool` are statements about the draw, and stay true
#' after rows leave; they are untouched. `rows_removed` accumulates across
#' repeated subsets so the gap between the record and the object is visible
#' rather than inferred.
#'
#' @param selection [List.] The record from `x$selection`.
#' @param keep_ids [Character.] `sample_id`s still in the object.
#' @param n_removed [Integer.] Rows dropped by this subset.
#'
#' @return [List.] The recomputed record.
#'
#' @seealso [subset_rows()]
#' @noRd
subset_selection <- function(selection, keep_ids, n_removed) {

  membership <- selection$membership
  retained   <- NULL

  if (!is.null(membership)) {

    ## A membership without the column predates it; every row is retained.
    is_retained <- if ("retained" %in% names(membership)) {
      !is.na(membership$retained) & membership$retained
    } else {
      rep(TRUE, nrow(membership))
    }

    keep_row             <- !is_retained | membership$pool_id %in% keep_ids
    membership           <- membership[keep_row, , drop = FALSE]
    retained             <- membership[is_retained[keep_row], , drop = FALSE]
    selection$membership <- membership

  }

  ## Groups: filter the ids, recount the rows ---------------------------------

  groups <- selection$groups

  if (!is.null(groups) && "pool_ids" %in% names(groups)) {

    groups$pool_ids <- lapply(groups$pool_ids, function(ids) ids[ids %in% keep_ids])
    groups$n_rows   <- as.integer(lengths(groups$pool_ids))

    selection$groups <- groups

  }

  ## Pool sizes: recount what is still drawn, per property --------------------

  pool_sizes <- selection$pool_sizes

  if (!is.null(pool_sizes) && all(c("property", "drawn") %in% names(pool_sizes))) {

    if (identical(selection$settings$scope, "global")) {

      ### scope = "global" draws the pool entire, so the survivors are the draw.
      pool_sizes$drawn <- rep(length(keep_ids), nrow(pool_sizes))

      selection$pool_sizes <- pool_sizes

    } else if (!is.null(retained)) {

      pool_sizes$drawn <- vapply(pool_sizes$property,
                                 function(p) length(unique(retained$pool_id[retained$property == p])),
                                 integer(1), USE.NAMES = FALSE)

      selection$pool_sizes <- pool_sizes

    }

  }

  selection$rows_removed <- (selection$rows_removed %||% 0L) + as.integer(n_removed)

  selection

}


## ---------------------------------------------------------------------------
## sort_axis_decreasing() — Put the predictor axis in decreasing order
## ---------------------------------------------------------------------------

#' Put an object's predictor columns in decreasing wavenumber order
#'
#' @description
#' Reorders the predictor columns among themselves, in `data$analysis` and
#' `data$role_map` alike. Names and values are untouched and every other
#' column keeps its place. Shared by [spectra()] and [standardize()]: both
#' sort their predictor axis on this helper, since a source can arrive in
#' either order (a KSSL-shaped library is stored increasing) and every check
#' and downstream step assumes decreasing.
#'
#' @param x `horizons_data.` The object.
#'
#' @return `list.` With elements:
#'   - `x`: The object, reordered through `set_analysis()` when anything moved
#'   - `sorted`: Whether any column moved
#'
#' @noRd
sort_axis_decreasing <- function(x) {

  analysis <- x$data$analysis
  role_map <- x$data$role_map

  pred_rows <- which(role_map$role == "predictor")
  pred_vars <- role_map$variable[pred_rows]

  ### A predictor name that is not a wavenumber leaves the order to the
  ### validator rather than being pushed to the end.
  wavenumbers <- suppressWarnings(as.numeric(gsub("^wn_", "", pred_vars)))
  axis_order  <- order(wavenumbers, decreasing = TRUE)

  if (anyNA(wavenumbers) || identical(axis_order, seq_along(pred_vars))) {

    return(list(x = x, sorted = FALSE))

  }

  ## The same slots in both tables, filled in the new order ------------------

  role_map[pred_rows, ] <- role_map[pred_rows[axis_order], ]

  cols <- names(analysis)
  cols[cols %in% pred_vars] <- pred_vars[axis_order]

  list(
    x      = set_analysis(x, analysis[, cols, drop = FALSE], role_map),
    sorted = TRUE
  )

}
