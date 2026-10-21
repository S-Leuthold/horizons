# R/pipeline-average.R
# Aggregate replicate spectra by sample ID with optional correlation-based
# quality control to detect outlier replicates.


## =============================================================================
## Section 1: Main Function
## =============================================================================


## ---------------------------------------------------------------------------
## average() — Aggregate replicates with optional quality control
## ---------------------------------------------------------------------------

#' Average replicate spectra with quality control
#'
#' @description
#' Aggregates replicate spectra by grouping column (default: `sample_id`) with
#' optional correlation-based quality control to detect and handle outliers.
#'
#' @details
#' **Quality Control:**
#' When `quality_check = TRUE`, outliers are identified via iterative removal:
#' 1. Compute mean pairwise correlation for each replicate
#' 2. Remove the replicate with lowest mean correlation
#' 3. Recompute correlations for remaining replicates
#' 4. Repeat until all remaining replicates pass threshold (or only 1 remains)
#'
#' This iterative approach prevents a single bad replicate from dragging all
#' others below threshold, which can happen with the simpler "flag all at once"
#' method when group sizes are small (n=2-3).
#'
#' **Important:** Run `standardize()` with baseline correction before `average()`
#' to ensure baseline shifts don't confound quality control. Spectra with baseline
#' offsets can correlate ~1.0 but shouldn't be averaged without correction.
#'
#' **Handling All-Outlier Groups:**
#' If QC flags all replicates as outliers:
#' * `"warn"` (default): Keep all replicates and average, with one
#'   `horizons_warning_quality` naming the samples
#' * `"drop"`: Remove the entire sample from results
#' * `"keep_best"`: Keep only the replicate with highest mean correlation
#'
#' **Metadata Handling:**
#' Metadata columns are checked for uniformity within groups:
#' * Uniform values: preserved in output
#' * Non-uniform values: column dropped, and named in a note on the
#'   object's log
#' * `filename` column: always dropped (not meaningful after averaging)
#'
#' **The log:**
#' Every sample whose replicates all failed QC is named on the object's
#' log, whatever `on_all_outliers` is: under `"warn"` as the
#' `horizons_warning_quality` it raises, under `"keep_best"` and `"drop"`
#' as a note, since either is your explicit choice.
#'
#' @param x A `horizons_data` object from the pipeline.
#' @param by Character. Column name to group replicates by. Default: `"sample_id"`.
#'   With any other column, the grouping values are coerced to character and
#'   become the new `sample_id` of the averaged rows; the original ids are not
#'   kept as a column, but the mapping from each new id to the scans it came
#'   from is recorded in `provenance$average$source_ids`. The column name is
#'   recorded in `provenance$aggregation_by`, which is written whatever `by`
#'   is. A custom `by` is refused on an object carrying a `select_training()`
#'   record, because renaming the rows breaks the link between the record and
#'   the object; average first, then select.
#' @param quality_check Logical. Enable correlation-based outlier detection.
#'   Default: `TRUE`.
#' @param correlation_threshold Numeric. Minimum mean pairwise correlation for
#'   a replicate to pass QC. Default: `0.996`. Changing this is rarely needed.
#' @param on_all_outliers Character. Behavior when all replicates fail QC:
#'   * `"warn"` (default): Keep all replicates and average
#'   * `"drop"`: Remove the sample entirely
#'   * `"keep_best"`: Keep only the replicate with highest mean correlation
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("horizons.verbose", TRUE)`, as every verb's.
#'
#' @return The input `horizons_data` object with:
#'   * `data$analysis`: One row per unique value in `by`. With a custom `by`
#'     that column is renamed to `sample_id` (as character), so the grouping
#'     values are the ids of the averaged rows and the `by` column itself is
#'     not in the output.
#'   * Predictor columns: Mean of replicate values
#'   * Response and outcome columns: Mean of replicate values when numeric,
#'     the shared value when not; samples whose non-numeric response values
#'     disagree are refused together, not a silent pick
#'   * Metadata columns: Uniform values preserved, non-uniform dropped
#'   * `data$role_map`: Updated to remove dropped columns and to point the
#'     `id` role at the new `sample_id`
#'   * `provenance$average`: The call's settings and counts, and
#'     `source_ids`, the mapping from each averaged `sample_id` to the source
#'     scans it was built from
#'   * `log`: The samples whose replicates all failed QC, and the metadata
#'     columns dropped
#'   * `provenance$aggregation_by`: The `by` column name, written on every
#'     call
#'
#' @examples
#' \dontrun{
#' # Basic averaging after parsing IDs
#' hd |>
#'   standardize() |>
#'   parse_ids(format = "{sample}_{rep}") |>
#'   average()
#'
#' # Without quality control
#' hd |> average(quality_check = FALSE)
#'
#' # Drop samples where all replicates are outliers
#' hd |> average(on_all_outliers = "drop")
#' }
#'
#' @export
average <- function(x,
                    by                    = "sample_id",
                    quality_check         = TRUE,
                    correlation_threshold = 0.996,
                    on_all_outliers       = c("warn", "drop", "keep_best"),
                    verbose               = getOption("horizons.verbose", TRUE)) {

  ## -------------------------------------------------------------------------
  ## Step 1: Input validation
  ## -------------------------------------------------------------------------

  ## Validate horizons_data object -------------------------------------------

  if (!inherits(x, "horizons_data")) {

    abort_horizons(
      "{.arg x} must be a horizons_data object, not {.obj_type_friendly {x}}.",
      kind = "input"
    )

  }

  ## Structural validation before the rebuild --------------------------------
  ## Step 8 below rebuilds the averaged table from role_map's predictor,
  ## meta and response columns; a column present in analysis but missing
  ## from role_map is referenced by none of them and would be silently
  ## dropped rather than carried through or refused. Catch it here, before
  ## that happens (#24). Raw stage: replicate scans sharing a sample_id are
  ## exactly the input average() exists to collapse.

  x <- validate_horizons_data(x, stage = "raw")

  ## Averaging rebuilds the rows that a promotion's splits are keyed to.
  check_unpromoted(x)

  ## Validate by column exists -----------------------------------------------

  check_arg(rlang::check_string, by)

  if (!by %in% names(x$data$analysis)) {

    abort_horizons(c(
      "{.arg by} must name a column of the data, and {.val {by}} is not one.",
      "i" = "Columns: {.field {names(x$data$analysis)}}."
    ), kind = "input")

  }

  ## A custom `by` renames the rows, and a selection record is keyed to the
  ## ids it drew. Refiltering the record needs the survivors to be a subset
  ## of the ids the draw named; new ids are not a subset of anything, so the
  ## record cannot be maintained and the object must be averaged first.

  if (!is.null(x$selection) && by != "sample_id") {

    abort_horizons(c(
      "{.arg by} cannot be a custom column on an object carrying a {.fn select_training} record.",
      "x" = "Promoting {.val {by}} to {.field sample_id} renames the rows the draw is keyed to.",
      "i" = "Average first, then run {.fn select_training}."
    ), kind = "input")

  }

  ## Validate parameters -----------------------------------------------------

  check_arg(rlang::check_bool, quality_check)
  check_arg(rlang::check_number_decimal, correlation_threshold, min = 0, max = 1)

  outlier_modes <- c("warn", "drop", "keep_best")

  ### The default is the vector of modes, as match.arg() reads it.
  if (identical(on_all_outliers, outlier_modes)) on_all_outliers <- outlier_modes[1]

  if (!rlang::is_string(on_all_outliers) || !on_all_outliers %in% outlier_modes) {

    abort_horizons(c(
      "{.arg on_all_outliers} must be one of {.val {outlier_modes}}.",
      "x" = "Got {.code {paste(deparse(on_all_outliers), collapse = ' ')}}."
    ), kind = "input")

  }

  check_arg(rlang::check_bool, verbose)

  collector <- log_open("average")

  ## -------------------------------------------------------------------------
  ## Step 2: Identify column roles
  ## -------------------------------------------------------------------------

  role_map <- x$data$role_map

  ## Predictor columns (wavelengths) -----------------------------------------

  predictor_cols <- role_map$variable[role_map$role == "predictor"]

  if (length(predictor_cols) == 0) {

    abort_horizons(c(
      "{.arg x} has no spectra to average.",
      "i" = "Average an object with spectra, from {.fn spectra}."
    ), kind = "input")

  }

  ## Meta columns (excluding by column and filename) -------------------------

  meta_cols <- role_map$variable[role_map$role == "meta"]
  meta_cols <- setdiff(meta_cols, c(by, "filename"))

  ## Response and outcome columns --------------------------------------------
  ## Lab measurements of the same physical sample, so they collapse with the
  ## spectra rather than being dropped; dropping them left the role map
  ## naming columns the averaged table did not have.

  response_cols <- role_map$variable[role_map$role %in% c("response", "outcome")]
  response_cols <- setdiff(response_cols, c(by, "filename"))

  ## -------------------------------------------------------------------------
  ## Step 3: Group analysis data
  ## -------------------------------------------------------------------------

  analysis <- x$data$analysis

  ## Get grouping vector -----------------------------------------------------

  group_values <- analysis[[by]]

  if (anyNA(group_values)) {

    n_na <- sum(is.na(group_values))

    abort_horizons(c(
      "The grouping column {.field {by}} has {n_na} {.code NA} value{?s}, which group{?s/} no replicates.",
      "i" = "Resolve the ids with {.fn parse_ids}, or drop the rows, before averaging."
    ), kind = "input")

  }

  unique_groups <- unique(group_values)
  n_groups <- length(unique_groups)

  ## A custom `by` becomes the character sample_id, so its values must stay
  ## distinct as character: two doubles that print alike would merge two
  ## samples' ids. Refused before any group is averaged.

  if (by != "sample_id") {

    as_ids  <- as.character(unique_groups)
    dup_ids <- unique(as_ids[duplicated(as_ids)])

    if (length(dup_ids) > 0) {

      shown <- utils::head(dup_ids, 5)

      abort_horizons(c(
        "{.arg by} column {.field {by}} has values that become the same {.field sample_id} as text.",
        "x" = "Repeated as text: {.val {shown}}.",
        "i" = "Make {.field {by}} character, or round it, before averaging."
      ), kind = "input")

    }

  }

  n_replicates <- nrow(analysis)

  ## Compute replicate counts per group for output ---------------------------

  reps_per_group <- table(group_values)
  min_reps       <- min(reps_per_group)
  max_reps       <- max(reps_per_group)

  ## -------------------------------------------------------------------------
  ## Step 4: Process each group
  ## -------------------------------------------------------------------------

  ## Initialize tracking -----------------------------------------------------

  results_list        <- vector("list", n_groups)
  n_dropped           <- 0
  all_outlier_samples <- character()
  dropped_samples     <- character()
  conflicts           <- list()

  ## For the report: the groups that lost some replicates to QC and averaged
  ## the rest, and the groups whose every replicate failed, handled by
  ## on_all_outliers, with the replicates each kind lost. The report used to
  ## count groups with drops, and a failed group averaged under "warn" drops
  ## nothing, so it was reported clean (#89).
  n_groups_partial <- 0L
  n_groups_failed  <- 0L
  n_reps_partial   <- 0
  n_reps_failed    <- 0

  for (i in seq_len(n_groups)) {

    group_id   <- unique_groups[i]
    group_idx  <- which(group_values == group_id)
    group_data <- analysis[group_idx, , drop = FALSE]
    n_reps     <- nrow(group_data)

    ## Process group ---------------------------------------------------------

    average_group(group_data          = group_data,
                  group_id            = group_id,
                  by                  = by,
                  predictor_cols      = predictor_cols,
                  meta_cols           = meta_cols,
                  response_cols       = response_cols,
                  quality_check       = quality_check,
                  threshold           = correlation_threshold,
                  on_all_outliers     = on_all_outliers) -> group_result

    ## Handle results --------------------------------------------------------

    conflicts <- c(conflicts, group_result$conflicts)

    if (is.null(group_result$averaged)) {

      ## Group was dropped (all outliers + drop mode) ------------------------

      dropped_samples <- c(dropped_samples, as.character(group_id))
      n_dropped       <- n_dropped + n_reps

    } else {

      results_list[[i]] <- group_result$averaged

      n_dropped <- n_dropped + group_result$n_dropped

      if (group_result$all_outliers) {

        all_outlier_samples <- c(all_outlier_samples, as.character(group_id))

      }

    }

    ## Tally for the report --------------------------------------------------

    if (group_result$all_outliers) {

      n_groups_failed <- n_groups_failed + 1L
      n_reps_failed   <- n_reps_failed + group_result$n_dropped

    } else if (group_result$n_dropped > 0) {

      n_groups_partial <- n_groups_partial + 1L
      n_reps_partial   <- n_reps_partial + group_result$n_dropped

    }

  }

  ## Conflicting non-numeric responses: every group, refused once -------------
  ## There is no defensible way to average two different lab classifications
  ## of one sample, so the user resolves them; naming every group at once
  ## spares them a run per group.

  if (length(conflicts) > 0) {

    lines <- vapply(conflicts, function(cf) {
      paste0(cf$column, " in ", cf$group, ": ", paste(cf$values, collapse = ", "))
    }, character(1))
    shown <- utils::head(lines, 5)
    more  <- length(lines) - length(shown)

    abort_horizons(c(
      "{length(lines)} sample{?s} {?has/have} response values that disagree between replicates.",
      stats::setNames(sprintf("{shown[%d]}", seq_along(shown)), rep("x", length(shown))),
      "i" = if (more > 0) "And {more} more.",
      "i" = "Resolve the values before averaging, or drop the column."
    ), kind = "input")

  }

  ## -------------------------------------------------------------------------
  ## Step 5: Combine results
  ## -------------------------------------------------------------------------

  ## Filter out NULLs (dropped groups) ---------------------------------------

  results_list <- results_list[!sapply(results_list, is.null)]

  if (length(results_list) == 0) {

    abort_horizons(c(
      "Every sample failed quality control on every replicate, and {.code on_all_outliers = \"drop\"} dropped them all.",
      "i" = "Keep them with {.code on_all_outliers = \"warn\"} or {.code \"keep_best\"}, or lower {.arg correlation_threshold}.",
      "i" = "The samples, one row each, are on this condition as {.field results}: {.code rlang::last_error()$results}."
    ), kind = "run", class = "horizons_error_qc",
       results = tibble::tibble(sample_id    = as.character(unique_groups),
                                n_replicates = as.integer(reps_per_group[as.character(unique_groups)])))

  }

  averaged <- dplyr::bind_rows(results_list)

  ## -------------------------------------------------------------------------
  ## Step 6: Handle metadata uniformity
  ## -------------------------------------------------------------------------

  ## Check which meta columns were actually retained -------------------------

  retained_meta <- intersect(meta_cols, names(averaged))
  dropped_meta <- setdiff(meta_cols, retained_meta)

  ## filename is always dropped (not in meta_cols already) -------------------

  if ("filename" %in% names(x$data$analysis)) {

    dropped_meta <- c("filename", dropped_meta)

  }

  ## -------------------------------------------------------------------------
  ## Step 7: Reorder columns
  ## -------------------------------------------------------------------------

  ## Order: by column, retained meta, responses, predictors ------------------

  col_order <- c(by, retained_meta, response_cols, predictor_cols)
  col_order <- col_order[col_order %in% names(averaged)]
  averaged <- averaged[, col_order, drop = FALSE]

  ## -------------------------------------------------------------------------
  ## Step 8: Update role_map
  ## -------------------------------------------------------------------------

  new_role_map <- role_map[!role_map$variable %in% dropped_meta, , drop = FALSE]

  ## With a custom `by`, the averaged rows ARE the samples now and the
  ## grouping value is their identity, so the `by` column becomes
  ## `sample_id`: every downstream verb, and validate_horizons_data(), reads
  ## that column by name. The original sample_id column is never carried into
  ## the averaged table (only `by`, retained meta, responses and predictors
  ## are): the door validator requires the id role on sample_id, so it is
  ## neither meta nor a response. `provenance$aggregation_by` keeps the name.

  if (by != "sample_id") {

    ## I2 says sample_id is character. A numeric or factor `by` would pass
    ## the validator's presence and uniqueness checks and then break joins
    ## against models$cv_predictions and ensemble$predictions much later.
    ### NA grouping values and values that coincide as text were refused
    ### before grouping.
    averaged[[by]] <- as.character(averaged[[by]])

    names(averaged)[names(averaged) == by] <- "sample_id"
    new_role_map <- new_role_map[new_role_map$variable != "sample_id", , drop = FALSE]
    new_role_map$variable[new_role_map$variable == by] <- "sample_id"
    new_role_map$role[new_role_map$variable == "sample_id"] <- "id"

  }

  ## -------------------------------------------------------------------------
  ## Step 9: Update horizons_data object
  ## -------------------------------------------------------------------------

  x <- set_analysis(x, averaged, new_role_map)

  ## -------------------------------------------------------------------------
  ## Step 10: Update provenance
  ## -------------------------------------------------------------------------

  ## Which scans each averaged row came from. Without this the source ids are
  ## gone entirely once `by` is promoted, and any earlier
  ## validation$outliers$*_ids become dangling references.

  source_ids <- tibble::tibble(
    sample_id        = as.character(group_values),
    source_sample_id = as.character(analysis$sample_id)
  )

  source_ids <- source_ids[!source_ids$sample_id %in% dropped_samples, , drop = FALSE]

  ### What happened to particular samples and columns is on the log
  ### (Step 11); this records how the call was made.
  x$provenance$average <- list(
    by                    = by,
    source_ids            = source_ids,
    quality_check         = quality_check,
    correlation_threshold = correlation_threshold,
    on_all_outliers       = on_all_outliers,
    n_groups              = n_groups,
    n_replicates_before   = nrow(analysis),
    n_replicates_after    = nrow(averaged),
    n_replicates_dropped  = n_dropped,
    applied_at            = Sys.time()
  )

  x$provenance$aggregation_by <- by

  ## -------------------------------------------------------------------------
  ## Step 10b: Re-validate
  ## -------------------------------------------------------------------------

  x <- validate_horizons_data(x)

  ## -------------------------------------------------------------------------
  ## Step 11: The samples whose every replicate failed, in every mode (#231)
  ## -------------------------------------------------------------------------
  ## Under "warn" everything ran as asked, but these averages are weaker than
  ## the result shows, so it is warned. "keep_best" and "drop" are the user's
  ## explicit choice: reported, not warned. Under "drop" the failed samples
  ## are the dropped ones.

  failed_samples <- c(all_outlier_samples, dropped_samples)

  if (length(failed_samples) > 0) {

    n_failed <- length(failed_samples)
    shown    <- utils::head(failed_samples, 5)
    more     <- if (n_failed > 5) paste0(" and ", n_failed - 5, " more") else ""

    if (on_all_outliers == "warn") {

      log_warning(collector, "quality", c(
        "All replicates failed QC for {n_failed} sample{?s}, averaged anyway.",
        "i" = "Samples: {.val {shown}}{more}.",
        "i" = "Use {.code on_all_outliers = \"keep_best\"} to keep one replicate of each, or {.code \"drop\"} to drop them."
      ), ids = failed_samples)

    } else {

      statement <- if (on_all_outliers == "drop") {
        "All replicates failed QC for {n_failed} sample{?s}, so {.code on_all_outliers = \"drop\"} dropped {?it/them}."
      } else {
        "All replicates failed QC for {n_failed} sample{?s}, so {.code on_all_outliers = \"keep_best\"} kept one replicate of {?it/each}."
      }

      log_note(collector, c(
        statement,
        "i" = "Samples: {.val {shown}}{more}."
      ), ids = failed_samples)

    }

  }

  ## The metadata columns a sample's replicates disagreed on -----------------
  ## filename is dropped on every call, by design, so it is not reported.

  dropped_cols <- setdiff(dropped_meta, "filename")

  if (length(dropped_cols) > 0) {

    log_note(collector,
             "Dropped {length(dropped_cols)} metadata column{?s} whose values differ between a sample's replicates: {.field {dropped_cols}}.",
             ids = dropped_cols)

  }

  x$log <- log_close(collector, x$log)

  ## -------------------------------------------------------------------------
  ## Step 12: Verbose output
  ## -------------------------------------------------------------------------

  if (verbose) {

    cat(paste0("\u251C\u2500 ", cli::style_bold("Averaging replicates"), "...\n"))

    ## Replicate structure line ------------------------------------------------

    reps_str <- if (min_reps == max_reps) {
      paste0(min_reps, " reps/group")
    } else {
      paste0(min_reps, "-", max_reps, " reps/group")
    }

    cat(paste0("\u2502  \u251C\u2500 ", n_groups, " groups from ",
               n_replicates, " scans (", reps_str, ")\n"))

    ## QC sub-tree -------------------------------------------------------------

    if (quality_check) {

      ## One line per kind of group, so the lines add up to n_groups and a
      ## group whose every replicate failed is never counted clean (#89). A
      ## group of one scan has nothing to correlate against, so QC never
      ## looked at it; it is not counted clean either.

      n_single <- sum(reps_per_group < 2)
      n_clean  <- n_groups - n_single - n_groups_partial - n_groups_failed

      if (n_clean < n_groups) {

        cat(paste0("\u2502  \u251C\u2500 QC (r > ", correlation_threshold, ")\n"))
        cat(paste0("\u2502  \u2502  \u251C\u2500 ", n_clean, "/", n_groups,
                   " groups clean\n"))

        if (n_single > 0) {

          cat(paste0("\u2502  \u2502  \u251C\u2500 ", cli::format_inline(
            "{n_single} single-replicate group{?s} (not QC'd)"
          ), "\n"))

        }

        if (n_groups_partial > 0) {

          cat(paste0("\u2502  \u2502  \u251C\u2500 ", cli::format_inline(
            "{n_groups_partial} group{?s} \u2192 {n_reps_partial} replicate{?s} removed"
          ), "\n"))

        }

        if (n_groups_failed > 0) {

          handled <- switch(on_all_outliers,
            warn      = "averaged anyway",
            keep_best = cli::format_inline("{n_groups_failed} replicate{?s} kept, {n_reps_failed} removed"),
            drop      = cli::format_inline("dropped, {n_reps_failed} replicate{?s} removed")
          )

          cat(paste0("\u2502  \u2502  \u251C\u2500 ", cli::format_inline(
            "{n_groups_failed} group{?s} failed QC on every replicate"
          ), " \u2192 ", handled, " (on_all_outliers = '", on_all_outliers, "')\n"))

        }

        cat(paste0("\u2502  \u2502  \u2514\u2500 ", cli::format_inline(
          "{nrow(averaged)} sample{?s} retained"
        ), "\n"))

      } else {

        cat(paste0("\u2502  \u251C\u2500 QC (r > ", correlation_threshold,
                   "): all ", n_groups, " groups clean\n"))

      }

    }

    cat(paste0("\u2502  \u2514\u2500 ", nrow(averaged), " samples \u00D7 ",
               x$data$n_predictors, " predictors\n"))
    cat("\u2502\n")

  }

  signal_log(collector, x)

  x

}


## =============================================================================
## Section 3: Helper Functions
## =============================================================================


## ---------------------------------------------------------------------------
## compute_replicate_quality() — Iterative pairwise correlation QC
## ---------------------------------------------------------------------------

#' Compute replicate quality via iterative pairwise correlations
#'
#' @description
#' Iteratively identifies outlier replicates by removing the worst one at a time
#' and recomputing correlations. This prevents a single bad replicate from
#' dragging all others below threshold.
#'
#' @param predictor_matrix Numeric matrix with rows = replicates, cols = wavelengths.
#' @param threshold Numeric. Minimum mean correlation to pass QC.
#'
#' @return A list with:
#'   * `outliers`: Integer indices of removed replicates (in original indexing)
#'   * `all_fail`: Logical, TRUE if only 1 replicate survived (or none)
#'   * `mean_cors`: Numeric vector of final mean correlations (NA for removed)
#'
#' @noRd
compute_replicate_quality <- function(predictor_matrix, threshold) {

  n <- nrow(predictor_matrix)

  ## Single replicate: no QC possible ----------------------------------------

  if (n < 2) {

    return(list(
      outliers  = integer(0),
      all_fail  = FALSE,
      mean_cors = NA_real_
    ))

  }

  ## Track which rows are still active (not yet flagged as outliers) ---------

  active_idx <- seq_len(n)
  outliers <- integer(0)

  ## Iteratively remove worst replicate until all pass or only 1 remains -----


  repeat {

    n_active <- length(active_idx)

    ## If only 1 remains, stop (can't compute correlation) -------------------

    if (n_active < 2) {

      break

    }

    ## Compute correlation matrix for active replicates ----------------------

    active_matrix <- predictor_matrix[active_idx, , drop = FALSE]

    cor_matrix <- stats::cor(
      t(active_matrix),
      use = "pairwise.complete.obs"
    )

    ## Mean correlation for each active replicate (excluding self) -----------

    diag(cor_matrix) <- NA
    mean_cors_active <- rowMeans(cor_matrix, na.rm = TRUE)

    ## Check if any are below threshold --------------------------------------

    below_threshold <- which(mean_cors_active < threshold)

    if (length(below_threshold) == 0) {

      ## All active replicates pass ------------------------------------------

      break

    }

    ## Remove the single worst replicate (lowest mean correlation) -----------
    ## Even if all fail, removing the worst might let others pass on recompute

    worst_active_idx <- which.min(mean_cors_active)
    worst_original_idx <- active_idx[worst_active_idx]

    outliers <- c(outliers, worst_original_idx)
    active_idx <- active_idx[-worst_active_idx]

  }

  ## Compute final mean correlations for reporting ---------------------------

  final_mean_cors <- rep(NA_real_, n)

  if (length(active_idx) >= 2) {

    active_matrix <- predictor_matrix[active_idx, , drop = FALSE]
    cor_matrix <- stats::cor(t(active_matrix), use = "pairwise.complete.obs")
    diag(cor_matrix) <- NA
    final_mean_cors[active_idx] <- rowMeans(cor_matrix, na.rm = TRUE)

  } else if (length(active_idx) == 1) {

    final_mean_cors[active_idx] <- NA_real_

  }

  ## Determine if "all failed" ------------------------------------------------
  ## With iterative removal, we define this as: only 1 replicate survived
  ## (or all were removed, which shouldn't happen but handle it)

  n_remaining <- length(active_idx)
  all_fail <- n_remaining <= 1 && n >= 2

  list(
    outliers  = as.integer(outliers),
    all_fail  = all_fail,
    mean_cors = final_mean_cors
  )

}


## ---------------------------------------------------------------------------
## average_group() — Process single group
## ---------------------------------------------------------------------------

#' Process and average a single group of replicates
#'
#' @param group_data Tibble of replicates for one group.
#' @param group_id The group identifier value.
#' @param by Name of grouping column.
#' @param predictor_cols Character vector of predictor column names.
#' @param meta_cols Character vector of metadata column names.
#' @param response_cols Character vector of response and outcome column names.
#'   Numeric columns are averaged over the same rows as the predictors;
#'   non-numeric columns must be uniform within the group.
#' @param quality_check Logical. Enable QC.
#' @param threshold Numeric. Correlation threshold.
#' @param on_all_outliers Character. Handling mode for all-outlier groups.
#'
#' @return A list with:
#'   * `averaged`: Single-row tibble (or NULL if dropped)
#'   * `n_dropped`: Number of replicates dropped
#'   * `all_outliers`: Logical, TRUE if all replicates were outliers
#'   * `qc_info`: QC details
#'   * `conflicts`: One entry per non-numeric response whose replicates
#'     disagree (`column`, `group`, `values`), for `average()` to refuse
#'     together
#'
#' @noRd
average_group <- function(group_data,
                          group_id,
                          by,
                          predictor_cols,
                          meta_cols,
                          response_cols = character(),
                          quality_check,
                          threshold,
                          on_all_outliers) {

  n_reps <- nrow(group_data)

  ## Initialize return values ------------------------------------------------

  n_dropped <- 0
  all_outliers <- FALSE
  qc_info <- NULL
  rows_to_average <- seq_len(n_reps)
  conflicts <- list()

  ## -------------------------------------------------------------------------
  ## Quality check (if enabled and n >= 2)
  ## -------------------------------------------------------------------------

  if (quality_check && n_reps >= 2) {

    ## Extract predictor matrix ----------------------------------------------

    predictor_matrix <- as.matrix(group_data[, predictor_cols, drop = FALSE])

    ## Compute quality -------------------------------------------------------

    qc <- compute_replicate_quality(predictor_matrix, threshold)
    qc_info <- qc

    ## Handle outliers -------------------------------------------------------

    if (length(qc$outliers) > 0) {

      if (qc$all_fail) {

        ## All replicates are outliers ---------------------------------------

        all_outliers <- TRUE

        if (on_all_outliers == "drop") {

          ## Drop entire sample ----------------------------------------------

          return(list(
            averaged     = NULL,
            n_dropped    = n_reps,
            all_outliers = TRUE,
            qc_info      = qc_info
          ))

        } else if (on_all_outliers == "keep_best") {

          ## Keep only the best replicate ------------------------------------
          ## When all_fail is TRUE, mean_cors is all NA (single survivor).
          ## which.max(all-NA) returns integer(0), so use the survivor index.

          remaining_idx <- setdiff(seq_len(n_reps), qc$outliers)
          best_idx <- if (length(remaining_idx) == 1L) {
            remaining_idx
          } else {
            which.max(qc$mean_cors)
          }
          rows_to_average <- best_idx
          n_dropped <- n_reps - length(rows_to_average)

        } else {

          ## warn: keep all --------------------------------------------------

          rows_to_average <- seq_len(n_reps)

        }

      } else {

        ## Some pass, some fail: drop outliers -------------------------------

        rows_to_average <- setdiff(seq_len(n_reps), qc$outliers)
        n_dropped <- length(qc$outliers)

      }

    }

  }

  ## -------------------------------------------------------------------------
  ## Average predictors
  ## -------------------------------------------------------------------------

  rows_to_use <- group_data[rows_to_average, , drop = FALSE]

  if (nrow(rows_to_use) == 1) {

    ## Single row: just take values ------------------------------------------

    averaged_predictors <- rows_to_use[, predictor_cols, drop = FALSE]

  } else {

    ## Multiple rows: compute column means -----------------------------------

    averaged_predictors <- tibble::as_tibble(
      lapply(rows_to_use[, predictor_cols, drop = FALSE], mean, na.rm = TRUE)
    )

  }

  ## -------------------------------------------------------------------------
  ## Collapse responses over the same rows as the predictors
  ## -------------------------------------------------------------------------

  averaged_responses <- NULL

  if (length(response_cols) > 0) {

    collapsed <- vector("list", length(response_cols))
    names(collapsed) <- response_cols

    for (col in response_cols) {

      collapsed[[col]] <- collapse_response(values = rows_to_use[[col]])

      ### Kept as the first value so the group still binds; average()
      ### refuses every conflict before the result is used.
      if (inherits(collapsed[[col]], "conflict")) {

        conflicts[[length(conflicts) + 1L]] <- list(
          column = col,
          group  = as.character(group_id),
          values = unclass(collapsed[[col]])
        )
        collapsed[[col]] <- rows_to_use[[col]][1]

      }

    }

    averaged_responses <- tibble::as_tibble(collapsed)

  }

  ## -------------------------------------------------------------------------
  ## Handle metadata
  ## -------------------------------------------------------------------------

  ## Start with group ID -----------------------------------------------------

  result <- tibble::tibble(!!by := group_id)

  ## Check each meta column for uniformity -----------------------------------

  for (col in meta_cols) {

    col_values <- group_data[[col]]

    if (check_uniformity(col_values)) {

      result[[col]] <- col_values[1]

    }

    ## Non-uniform: column not added (will be noted in provenance) -----------

  }

  ## -------------------------------------------------------------------------
  ## Combine
  ## -------------------------------------------------------------------------

  if (!is.null(averaged_responses)) {

    result <- dplyr::bind_cols(result, averaged_responses)

  }

  result <- dplyr::bind_cols(result, averaged_predictors)

  list(
    averaged     = result,
    n_dropped    = n_dropped,
    all_outliers = all_outliers,
    qc_info      = qc_info,
    conflicts    = conflicts
  )

}


## ---------------------------------------------------------------------------
## collapse_response() — One response value for one group
## ---------------------------------------------------------------------------

#' Collapse one response column within a group of replicates
#'
#' @description
#' Numeric response values become their mean over the replicates being
#' averaged. Non-numeric values must be uniform within the group; a group
#' whose values disagree is returned as a conflict rather than a silent
#' first-value pick, because there is no defensible way to average two
#' different lab classifications of one sample. `average()` refuses every
#' conflicting group at once.
#'
#' @param values Vector of the column's values for the group's replicates.
#'
#' @return Length-one vector of the same type as `values` (numeric columns
#'   return `NA_real_` when every replicate is missing), or, for values
#'   that disagree, their distinct non-missing values with class
#'   `"conflict"`.
#'
#' @noRd
collapse_response <- function(values) {

  if (is.numeric(values)) {

    if (all(is.na(values))) {

      return(NA_real_)

    }

    return(mean(values, na.rm = TRUE))

  }

  if (check_uniformity(values)) {

    non_na <- values[!is.na(values)]

    return(if (length(non_na) == 0) values[1] else non_na[1])

  }

  structure(as.character(unique(values[!is.na(values)])), class = "conflict")

}


## ---------------------------------------------------------------------------
## check_uniformity() — Check if values are uniform
## ---------------------------------------------------------------------------

#' Check if all values in a vector are the same
#'
#' @param values Vector of values to check.
#'
#' @return Logical. TRUE if all values are identical (NA-aware).
#'
#' @noRd
check_uniformity <- function(values) {

  ## All NA is considered uniform --------------------------------------------

  if (all(is.na(values))) {

    return(TRUE)

  }

  ## Remove NAs and check uniqueness -----------------------------------------

  non_na <- values[!is.na(values)]

  if (length(non_na) == 0) {

    return(TRUE)

  }

  length(unique(non_na)) == 1

}
