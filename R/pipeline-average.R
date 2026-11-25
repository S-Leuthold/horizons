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
#' **The order with [standardize()]:** the pipeline averages before it
#' standardizes, `spectra() |> parse_ids() |> average() |> standardize()`,
#' so quality control correlates the scans as they were read. Pearson
#' correlation does not change with an offset or a gain between two scans,
#' so a replicate that differs from the others only by a baseline offset or
#' a scale factor passes whether or not a baseline has been corrected; one
#' whose baseline differs in slope or curvature correlates lower. The check
#' reads the range the spectra are modelled over, whichever order the two
#' verbs run in (`qc_range`): on spectra `standardize()` has not run on, the
#' bands inside its default `trim`, 600 to 4000 cm-1; on spectra it has run
#' on, every band it kept. At the defaults the check reads the same range in
#' either order, and the averaged spectra keep every band.
#'
#' **Missing and non-finite values:** an `Inf`, `-Inf` or `NaN` in a
#' replicate is treated as a missing value, as `NA` is. Quality control
#' correlates replicates over the bands they share, and the mean at each band
#' uses the replicates with a value there, so a replicate missing a few
#' bands is still averaged in at the rest. A band no replicate has is `NA` in
#' the result, and [standardize()] decides on it.
#'
#' **Unusable spectra:** `average()` leaves the decision on a spectrum that
#' is not finite or has no spread to [standardize()], which flags it. In the
#' order `spectra() |> parse_ids() |> average() |> standardize()` such a
#' sample is averaged and then flagged, not refused. Quality control fails a
#' replicate it cannot correlate, such as a flat scan, so it is removed
#' rather than averaged in: it is set aside before the iterative removal, as
#' a flagged replicate is. A sample left with one replicate is that
#' replicate; one left with none is averaged, and `standardize()` flags it.
#' Under `quality_check = FALSE` such a replicate is averaged in, as asked.
#'
#' **Handling All-Outlier Groups:**
#' If QC flags all replicates as outliers:
#' * `"warn"` (default): Keep all replicates and average, with one
#'   `horizons_warning_quality` naming the samples
#' * `"drop"`: Remove the entire sample from results
#' * `"keep_best"`: Keep only the replicate with the highest mean
#'   correlation against the sample's other replicates, measured before QC
#'   removes any. A tie goes to the earlier scan, so of two replicates,
#'   whose one correlation cannot tell them apart, the first is kept
#'
#' **Flagged replicates:**
#' A replicate a step flagged (`.flag`, from [standardize()]) is one it
#' could not use, so it is left out of quality control and the mean, as
#' every verb that computes over predictors leaves a flagged row out. A
#' sample is flagged only when every replicate is: it is kept as one row,
#' with the replicates' step in `.flag`, and modelling leaves it out. The
#' verbose output counts both; `standardize()` has already warned about the
#' spectra. The result has a `.flag` column only when a sample is flagged.
#'
#' **Before modelling only:** averaging rebuilds the rows the splits and
#' models of [evaluate()], [fit()] and [ensemble()] are keyed to, so an
#' evaluated, fitted or ensembled object is refused. Average before
#' `evaluate()`.
#'
#' **Metadata Handling:**
#' Metadata columns are checked for uniformity within groups:
#' * Uniform within every sample: preserved in output
#' * Non-uniform within any sample: column dropped from every row, and named
#'   in a note on the object's log
#' * `filename`, the OPUS input's meta column: always dropped (not meaningful
#'   after averaging), unless it is `by`, when it becomes `sample_id` as any
#'   `by` column does. A response of that name is averaged like any other
#'
#' **The log:**
#' Every sample whose replicates all failed QC is named on the object's
#' log, whatever `on_all_outliers` is: under `"warn"` as the
#' `horizons_warning_quality` it raises, under `"keep_best"` and `"drop"`
#' as a note, since either is your explicit choice.
#'
#' **Which scans QC removed:** when the quality check removes any scan, one
#' note on the log names the samples that lost one, and its `detail` is a
#' table with one row per scan removed:
#' * `sample_id`: the averaged sample it belonged to (with a custom `by`, the
#'   `by` value, as the result's `sample_id` is)
#' * `scan`: the scan's `filename` when the input has one (OPUS scans), else
#'   its row number as text
#' * `row`: its row in the data `average()` was given
#' * `reason`: `"below_threshold"`, removed because its mean correlation fell
#'   below `correlation_threshold`, as is every scan of a sample
#'   `on_all_outliers = "drop"` dropped but an uncorrelatable one;
#'   `"uncorrelatable"`, a scan QC could not correlate with the others, such
#'   as a flat one, in every mode; or
#'   `"not_best"`, a scan of a sample whose replicates all failed, left out
#'   when `on_all_outliers = "keep_best"` kept the best
#' * `mean_correlation`: its mean correlation with the sample's other scans
#'   in the last round of the removal it took part in, `NA` when it had none
#'
#' `dplyr::bind_rows(x$log$detail[x$log$verb == "average"])` gathers it,
#' to join to a sample table by `sample_id`. When the check removed no scan,
#' no row carries a table and the gather is a table with no columns, so check
#' it has rows before joining. Nothing resets the log, so the table stays on
#' the object through every later verb. Under `on_all_outliers = "warn"` a
#' sample whose replicates all failed keeps every scan QC could correlate, so
#' only its uncorrelatable scans can be in the table. Flagged replicates are
#' left out of QC rather than removed by it, and are counted in the call's
#' note instead.
#'
#' @param x A `horizons_data` object from the pipeline.
#' @param by Character. Column name to group replicates by. Default: `"sample_id"`.
#'   With any other column, the grouping values are coerced to character and
#'   become the new `sample_id` of the averaged rows; the original ids are not
#'   kept as a column. The column name is recorded in the call's note on the
#'   log. A custom `by` is refused on an object carrying a `select_training()`
#'   record, because renaming the rows breaks the link between the record and
#'   the object; average first, then select.
#' @param quality_check Logical. Enable correlation-based outlier detection.
#'   Default: `TRUE`.
#' @param correlation_threshold Numeric. Minimum mean pairwise correlation for
#'   a replicate to pass QC. Default: `0.996`. Changing this is rarely needed.
#' @param qc_range `numeric(2)` or `NULL`. The wavenumber range, in cm-1 and
#'   inclusive, whose bands quality control correlates over; the averaged
#'   spectra keep every band. `NULL` (the default) is the range the spectra
#'   are modelled over, whichever order `average()` and [standardize()] run
#'   in: on spectra `standardize()` has run on, every band they have; on
#'   spectra it has not, the bands inside its default `trim`, 600 to 4000
#'   cm-1. A correlation needs at least three bands, so a range given that
#'   holds fewer of the spectra's bands is refused. When the default holds
#'   fewer, as for an instrument outside 600 to 4000 cm-1, quality control
#'   correlates over every band, and a note on the log says so. The call's
#'   note names the bands quality control used. An instrument whose axis
#'   only partly overlaps 600 to 4000 cm-1 is checked over the overlap,
#'   which that note names, and can set `qc_range` to the range it is
#'   modelled over.
#' @param on_all_outliers Character. Behavior when all replicates fail QC:
#'   * `"warn"` (default): Keep all replicates and average
#'   * `"drop"`: Remove the sample entirely
#'   * `"keep_best"`: Keep only the replicate with the highest mean
#'     correlation, measured before QC removes any. A tie goes to the earlier
#'     scan, so of two replicates the first is kept
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
#'   * Metadata columns: Those uniform within every sample preserved; one that
#'     varies within any sample dropped
#'   * `data$role_map`: Updated to remove dropped columns and to point the
#'     `id` role at the new `sample_id`
#'   * `log`: The samples whose replicates all failed QC, the metadata
#'     columns dropped, and the scans QC removed, as a table in the note's
#'     `detail`, then a note recording the call: `by`, the scans in and
#'     samples out, the quality check, the bands it correlated over and the
#'     replicates it removed, and any flagged replicates left out
#'
#' @examples
#' \dontrun{
#' # Average the replicate scans, then standardize, as the pipeline runs
#' hd |>
#'   parse_ids(format = "{sampleid}_{replicate}") |>
#'   average() |>
#'   standardize()
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
                    qc_range              = NULL,
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

  if (!is.null(qc_range) && (!is.numeric(qc_range) || length(qc_range) != 2)) {

    abort_horizons(
      "{.arg qc_range} must be numeric(2) or {.code NULL}, not {.obj_type_friendly {qc_range}}.",
      kind = "input"
    )

  }

  if (!is.null(qc_range) && (!all(is.finite(qc_range)) || qc_range[1] >= qc_range[2])) {

    abort_horizons(c(
      "{.arg qc_range} must be two finite wavenumbers, the lower first.",
      "x" = "Got {qc_range[1]} and {qc_range[2]}."
    ), kind = "input")

  }

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

  ## The bands the quality check correlates over -----------------------------
  ## The range the user models, whichever order average() and standardize()
  ## run in; only the correlation reads it, and the mean keeps every band.

  qc_bands <- quality_check_bands(predictor_cols, qc_range,
                                  standardized = !is.null(x$data$standardization))
  qc_wn    <- round(range(wn_values(qc_bands$cols)), 2)

  ### A range given that holds too few bands to correlate over cannot be the
  ### one read, so it is refused, QC on or off; the default's own fallback is
  ### every band
  if (!is.null(qc_range) && qc_bands$fallback) {

    abort_horizons(c(
      "{.arg qc_range} holds {qc_bands$n_inside} band{?s} of the spectra, and the quality check needs at least {QC_MIN_BANDS} to correlate over.",
      "x" = "It is {qc_range[1]} to {qc_range[2]} cm-1, and the spectra run from {qc_wn[1]} to {qc_wn[2]} cm-1.",
      "i" = "Give a range holding at least {QC_MIN_BANDS} bands of the spectra, or {.code NULL} for the range they are modelled over."
    ), kind = "input")

  }

  if (quality_check && qc_bands$fallback) {

    log_note(collector, c(
      "The quality check's range, {qc_bands$asked[1]} to {qc_bands$asked[2]} cm-1, holds {qc_bands$n_inside} band{?s}, fewer than the {QC_MIN_BANDS} it needs to correlate over, so it correlated over every band, {qc_wn[1]} to {qc_wn[2]} cm-1.",
      "i" = "Set {.arg qc_range} to the range the spectra are modelled over."
    ))

  }

  ## Meta columns (excluding by column and filename) -------------------------

  meta_cols <- role_map$variable[role_map$role == "meta"]
  meta_cols <- setdiff(meta_cols, c(by, "filename", ".flag"))

  ## Response and outcome columns --------------------------------------------
  ## Lab measurements of the same physical sample, so they collapse with the
  ## spectra rather than being dropped; dropping them left the role map
  ## naming columns the averaged table did not have.

  response_cols <- role_map$variable[role_map$role %in% c("response", "outcome")]
  response_cols <- setdiff(response_cols, by)

  ## -------------------------------------------------------------------------
  ## Step 3: Group analysis data
  ## -------------------------------------------------------------------------

  analysis <- x$data$analysis

  ## Non-finite values are missing values ------------------------------------
  ## An Inf, -Inf or NaN in a replicate is lost data at that band, as an NA
  ## is: QC correlates over the bands the replicates share, and the mean at a
  ## band uses the replicates with a value there. A band no replicate has
  ## stays NA, for standardize() to decide on. is.na() is TRUE for NaN.

  analysis[predictor_cols] <- lapply(analysis[predictor_cols], function(v) {
    v[is.na(v) | is.infinite(v)] <- NA
    v
  })

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

      abort_horizons(c(
        "{.arg by} column {.field {by}} has values that become the same {.field sample_id} as text.",
        "x" = "Repeated as text: {shown_ids(dup_ids)}.",
        "i" = "Make {.field {by}} character, or round it, before averaging."
      ), kind = "input")

    }

  }

  n_replicates <- nrow(analysis)

  ## How the record names a scan QC removes ------------------------------------
  ## By its file, when the input is OPUS scans with a filename (the column is
  ## dropped from the result, so this is the last place it is known), and by
  ## its row in the table average() was given otherwise.

  scan_names <- if ("filename" %in% role_map$variable[role_map$role == "meta"]) {
    as.character(analysis$filename)
  } else {
    as.character(seq_len(n_replicates))
  }

  ## Compute replicate counts per group for output ---------------------------

  reps_per_group <- table(group_values)
  min_reps       <- min(reps_per_group)
  max_reps       <- max(reps_per_group)

  ## -------------------------------------------------------------------------
  ## Step 4: Process each group
  ## -------------------------------------------------------------------------

  ## Initialize tracking -----------------------------------------------------

  results_list        <- vector("list", n_groups)
  removed_list        <- vector("list", n_groups)
  n_dropped           <- 0
  all_outlier_samples <- character()
  dropped_samples     <- character()
  conflicts           <- list()
  varying_meta        <- character()

  ## For the report: the groups that lost some replicates to QC and averaged
  ## the rest, and the groups whose every replicate failed, handled by
  ## on_all_outliers, with the replicates each kind lost. The report used to
  ## count groups with drops, and a failed group averaged under "warn" drops
  ## nothing, so it was reported clean (#89).
  n_groups_partial <- 0L
  n_groups_failed  <- 0L
  n_reps_partial   <- 0
  n_reps_failed    <- 0

  ## And the replicates a step flagged, left out, and the samples kept
  ## flagged because every replicate was; groups QC could not look at
  n_flag_out     <- 0L
  n_kept_flagged <- 0L
  n_not_qcd      <- 0L

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
                  qc_cols             = qc_bands$cols,
                  meta_cols           = meta_cols,
                  response_cols       = response_cols,
                  quality_check       = quality_check,
                  threshold           = correlation_threshold,
                  on_all_outliers     = on_all_outliers) -> group_result

    ## Handle results --------------------------------------------------------

    conflicts    <- c(conflicts, group_result$conflicts)
    varying_meta <- union(varying_meta, group_result$meta_dropped)

    ## The scans QC removed, by the averaged row's id: the group's value, which
    ## becomes the sample_id under a custom `by`, and by the scan's row in the
    ## table given
    if (nrow(group_result$removed) > 0) {

      rows <- group_idx[group_result$removed$index]

      removed_list[[i]] <- tibble::tibble(
        sample_id        = as.character(group_id),
        scan             = scan_names[rows],
        row              = rows,
        reason           = group_result$removed$reason,
        mean_correlation = group_result$removed$mean_correlation
      )

    }

    if (is.null(group_result$averaged)) {

      ## Group was dropped (all outliers + drop mode) ------------------------

      dropped_samples <- c(dropped_samples, as.character(group_id))
      n_dropped       <- n_dropped + group_result$n_dropped

    } else {

      results_list[[i]] <- group_result$averaged

      n_dropped <- n_dropped + group_result$n_dropped

      if (group_result$all_outliers) {

        all_outlier_samples <- c(all_outlier_samples, as.character(group_id))

      }

    }

    ## Tally for the report --------------------------------------------------

    n_flag_out     <- n_flag_out + group_result$n_flag_out
    n_kept_flagged <- n_kept_flagged + !is.na(group_result$flag)
    n_not_qcd      <- n_not_qcd + (is.na(group_result$flag) && !group_result$qc_run)

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
    ### A sample can disagree on several columns: one bullet each, counted
    ### once.
    n_conflicted <- length(unique(vapply(conflicts, `[[`, character(1), "group")))
    shown        <- utils::head(lines, 5)
    more         <- length(lines) - length(shown)

    abort_horizons(c(
      "{n_conflicted} sample{?s} {?has/have} response values that disagree between replicates.",
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

  ## A column that varies within any sample is dropped from every row ---------
  ## Kept for the samples where it was uniform, it would be NA for the rest,
  ## a value no replicate had (#226). .flag is a step's verdict on each row,
  ## not a description of the sample, and is exempt.

  dropped_meta  <- varying_meta
  retained_meta <- setdiff(intersect(meta_cols, names(averaged)), dropped_meta)
  averaged      <- averaged[, setdiff(names(averaged), dropped_meta), drop = FALSE]

  ## .flag: on the result only when a sample is flagged ----------------------
  ## A step's verdict on each row, not a description of the sample, so it is
  ## not a metadata column: a sample is flagged when every replicate was.

  if (".flag" %in% names(averaged) && all(is.na(averaged$.flag))) {

    averaged$.flag <- NULL
    dropped_meta   <- c(dropped_meta, ".flag")

  }

  retained_meta <- c(retained_meta, intersect(".flag", names(averaged)))

  ## filename is always dropped (not in meta_cols already) -------------------
  ## The OPUS input's meta column, that is: a response of that name is a
  ## response, kept and averaged like any other. As `by` it is the grouping
  ## column, which becomes sample_id below, so it is not dropped.

  if (by != "filename" && "filename" %in% role_map$variable[role_map$role == "meta"]) {

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
  ## neither meta nor a response. The call's note on the log keeps the name.

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
  ## Step 10: Re-validate
  ## -------------------------------------------------------------------------

  ### Every finding but the spectra's: in the documented order average()
  ### runs before standardize(), which decides and flags an unusable
  ### spectrum, so one is left for it rather than refused here.
  x <- validate_horizons_data(x, spectra = FALSE)

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

    if (on_all_outliers == "warn") {

      log_warning(collector, "quality", c(
        "All replicates failed QC for {n_failed} sample{?s}, averaged anyway.",
        "i" = "Samples: {shown_ids(failed_samples)}.",
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
        "i" = "Samples: {shown_ids(failed_samples)}."
      ), ids = failed_samples)

    }

  }

  ## The metadata columns a sample's replicates disagreed on -----------------
  ## filename is dropped on every call, by design, and .flag when no sample
  ## is flagged, so neither is reported.

  dropped_cols <- setdiff(dropped_meta, c("filename", ".flag"))

  if (length(dropped_cols) > 0) {

    log_note(collector,
             "Dropped {length(dropped_cols)} metadata column{?s} whose values differ between a sample's replicates: {.field {dropped_cols}}.",
             ids = dropped_cols)

  }

  ## The scans the quality check removed (#255) -------------------------------
  ## One note whose detail is a row per scan, joined to the sample table by
  ## sample_id and to the scans by scan or row. Written just before the
  ## call's record, which counts them.

  removed_scans <- dplyr::bind_rows(removed_list)

  if (nrow(removed_scans) > 0) {

    lost_samples <- unique(removed_scans$sample_id)
    n_lost       <- length(lost_samples)
    n_removed    <- nrow(removed_scans)

    log_note(collector, c(
      "The quality check removed {n_removed} replicate{?s} from {n_lost} sample{?s}.",
      "i" = "Samples: {shown_ids(lost_samples)}.",
      "i" = "Each scan, why it was removed and its mean correlation are in this row's {.field detail}."
    ), ids = lost_samples, n = n_removed, detail = removed_scans)

  }

  ## The call's record: how the replicates were averaged ----------------------
  ## A note, never raised, after the events above.

  ### Each count in the tree's words: the replicates QC removed, and the
  ### flagged ones left out, counted once each
  n_before <- nrow(analysis)
  n_after  <- nrow(averaged)
  n_qc     <- length(qc_bands$cols)
  qc_text  <- if (quality_check) {
    "quality check on at a correlation of {correlation_threshold} over {n_qc} band{?s}, {qc_wn[1]} to {qc_wn[2]} cm-1, with {.code on_all_outliers = {.val {on_all_outliers}}}; {n_dropped} replicate{?s} removed by the quality check"
  } else {
    "quality check off"
  }
  flag_text <- if (n_flag_out > 0) "; {n_flag_out} flagged (.flag) replicate{?s} left out" else ""

  log_note(collector,
           paste0("Averaged {n_before} scan{?s} into {n_after} sample{?s} by {.field {by}}: ",
                  qc_text, flag_text, "."))

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

    if (n_flag_out > 0 || n_kept_flagged > 0) {

      cat(paste0("\u2502  \u251C\u2500 ", cli::format_inline(
        "Flagged (.flag): {n_flag_out} replicate{?s} left out, {n_kept_flagged} sample{?s} kept flagged"
      ), "\n"))

    }

    ## QC sub-tree -------------------------------------------------------------

    if (quality_check) {

      ## One line per kind of group, so the lines add up to n_groups and a
      ## group whose every replicate failed is never counted clean (#89). A
      ## group with fewer than two scans left once flagged ones are out, or
      ## none QC can correlate, has nothing to correlate against, so QC never
      ## looked at it; it is not counted clean either, nor is a sample kept
      ## flagged, which has a line of its own.

      n_single <- n_not_qcd
      n_clean  <- n_groups - n_single - n_kept_flagged - n_groups_partial - n_groups_failed

      if (n_clean < n_groups) {

        cat(paste0("\u2502  \u251C\u2500 QC (r > ", correlation_threshold, ")\n"))
        cat(paste0("\u2502  \u2502  \u251C\u2500 ", n_clean, "/", n_groups,
                   " groups clean\n"))

        if (n_single > 0) {

          cat(paste0("\u2502  \u2502  \u251C\u2500 ", cli::format_inline(
            "{n_single} group{?s} with fewer than two usable replicates (not QC'd)"
          ), "\n"))

        }

        if (n_kept_flagged > 0) {

          cat(paste0("\u2502  \u2502  \u251C\u2500 ", cli::format_inline(
            "{n_kept_flagged} group{?s} flagged on every replicate (not QC'd)"
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
## quality_check_bands() — The bands QC correlates over
## ---------------------------------------------------------------------------

#' The bands average()'s quality check correlates over
#'
#' @description
#' The range the user models, whichever order `average()` and
#' [standardize()] run in. With `qc_range = NULL`, an object `standardize()`
#' has run on (a `data$standardization` record) is checked over every band
#' it has, the range it was trimmed to; one it has not run on, over the bands
#' inside `standardize()`'s default `trim`, `MIR_RANGE`. A `qc_range` given
#' is the range read either way. The bounds are inclusive, as `trim`'s are.
#' When fewer than `QC_MIN_BANDS` bands lie inside the range, too few to
#' correlate over, the check reads every band: the default's fallback, since
#' `average()` refuses a `qc_range` given that holds fewer.
#'
#' @param predictor_cols Character. The predictor columns, named by
#'   wavenumber.
#' @param qc_range `average()`'s `qc_range`, checked: `NULL` or two finite
#'   wavenumbers, the lower first.
#' @param standardized Logical(1). Whether `standardize()` has run on the
#'   object.
#'
#' @return List: `cols`, the predictor columns QC correlates over; `asked`,
#'   the range they were chosen from, `NULL` for every band of a
#'   standardized object; `n_inside`, the bands inside `asked` (every band
#'   when `asked` is `NULL`); and `fallback`, `TRUE` when `n_inside` is
#'   fewer than `QC_MIN_BANDS`, so `cols` is every band.
#' @keywords internal
#' @noRd
quality_check_bands <- function(predictor_cols, qc_range, standardized) {

  asked <- if (!is.null(qc_range)) qc_range else if (standardized) NULL else MIR_RANGE

  if (is.null(asked)) {

    return(list(cols = predictor_cols, asked = NULL,
                n_inside = length(predictor_cols), fallback = FALSE))

  }

  wn     <- wn_values(predictor_cols)
  inside <- wn >= asked[1] & wn <= asked[2]

  if (sum(inside) < QC_MIN_BANDS) {

    return(list(cols = predictor_cols, asked = asked,
                n_inside = sum(inside), fallback = TRUE))

  }

  list(cols = predictor_cols[inside], asked = asked,
       n_inside = sum(inside), fallback = FALSE)

}


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
#' A replicate QC cannot correlate, one with no spread (a flat or empty
#' scan) or with no correlation against any other replicate, is set aside
#' first, as a flagged replicate is, and the iterative removal runs on the
#' rest. Its mean correlation would be `NaN`, which no threshold catches.
#'
#' @param predictor_matrix Numeric matrix with rows = replicates, cols = wavelengths.
#' @param threshold Numeric. Minimum mean correlation to pass QC.
#'
#' @return A list with:
#'   * `uncorrelatable`: Integer indices of the replicates QC cannot
#'     correlate, set aside before the removal
#'   * `outliers`: Integer indices of removed replicates (in original
#'     indexing), in the order they were removed
#'   * `all_fail`: Logical, TRUE if two or more replicates could be
#'     correlated and only 1 survived
#'   * `best`: The index of the replicate with the highest mean correlation
#'     against the others QC could correlate, from the first round, before
#'     any removal; a tie goes to the earlier replicate. `integer(0)` when
#'     fewer than two could be correlated
#'   * `mean_cors`: Numeric, one per replicate (in original indexing): its
#'     mean correlation with the other active replicates in the last round
#'     it took part in, so for a removed one the round that removed it. `NA`
#'     for one QC could not correlate, for one whose partners had all been
#'     removed, and for every one when no round ran
#'
#' @noRd
compute_replicate_quality <- function(predictor_matrix, threshold) {

  n <- nrow(predictor_matrix)

  ## Single replicate: no QC possible ----------------------------------------

  if (n < 2) {

    return(list(
      uncorrelatable = integer(0),
      outliers       = integer(0),
      all_fail       = FALSE,
      best           = integer(0),
      mean_cors      = rep(NA_real_, n)
    ))

  }

  ## Replicates QC cannot correlate ------------------------------------------
  ## A replicate with no spread has no correlation at all, and stats::cor()
  ## sees only the replicates with spread, so base R's zero-deviation warning
  ## is not raised. Among two or more with spread, one can still share no
  ## complete band with any other.

  spread    <- apply(predictor_matrix, 1, function(r) length(unique(r[is.finite(r)])) > 1)
  correlate <- spread

  if (sum(spread) >= 2) {

    cor_matrix <- stats::cor(t(predictor_matrix[spread, , drop = FALSE]),
                             use = "pairwise.complete.obs")
    diag(cor_matrix) <- NA
    correlate[spread] <- rowSums(!is.na(cor_matrix)) > 0

  }

  uncorrelatable <- which(!correlate)

  ## Track which rows are still active (not yet flagged as outliers) ---------

  active_idx <- which(correlate)
  outliers   <- integer(0)
  best       <- integer(0)
  mean_cors  <- rep(NA_real_, n)

  ## Fewer than two left to correlate: no QC possible ------------------------

  if (length(active_idx) < 2) {

    return(list(
      uncorrelatable = uncorrelatable,
      outliers       = integer(0),
      all_fail       = FALSE,
      best           = integer(0),
      mean_cors      = mean_cors
    ))

  }

  ## Iteratively remove worst replicate until all pass or only 1 remains -----

  repeat {

    n_active <- length(active_idx)

    ## If only 1 remains, stop (can't compute correlation) -------------------

    if (n_active < 2) {

      break

    }

    ## Mean correlation for each active replicate (excluding self) -----------
    ## A replicate whose only partners have been removed has no correlation
    ## left; it fails rather than passes, so it is removed first.

    cor_matrix <- stats::cor(
      t(predictor_matrix[active_idx, , drop = FALSE]),
      use = "pairwise.complete.obs"
    )

    diag(cor_matrix) <- NA
    mean_cors_active <- rowMeans(cor_matrix, na.rm = TRUE)

    ### Each active replicate's value this round, kept as its last; one with
    ### no correlation left has none
    mean_cors[active_idx] <- mean_cors_active
    mean_cors[active_idx][is.nan(mean_cors_active)] <- NA_real_

    mean_cors_active[is.nan(mean_cors_active)] <- -Inf

    ## The first round compares every replicate QC could correlate with all
    ## the others, so its maximum is the best replicate. The last two the
    ## removal leaves always tie, so they cannot say which is best.
    if (length(best) == 0) best <- active_idx[which.max(mean_cors_active)]

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

  ## Determine if "all failed" ------------------------------------------------
  ## With iterative removal, we define this as: only 1 replicate survived

  all_fail <- length(active_idx) <= 1

  list(
    uncorrelatable = uncorrelatable,
    outliers       = as.integer(outliers),
    all_fail       = all_fail,
    best           = as.integer(best),
    mean_cors      = mean_cors
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
#' @param qc_cols Character vector. The predictor columns quality control
#'   correlates over ([quality_check_bands()]); the mean is over every
#'   predictor column.
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
#'   * `n_dropped`: Number of replicates QC removed, the flagged ones not
#'     among them: the rows of `removed`
#'   * `removed`: One row per replicate QC removed, by its row in
#'     `group_data`: `index`, `reason` (`"uncorrelatable"`, set aside before
#'     the removal; `"below_threshold"`, removed by it, as is every other
#'     replicate of a sample `"drop"` dropped; `"not_best"`, left out of a
#'     sample `"keep_best"` kept to its best) and `mean_correlation` (from
#'     the last round of QC it took part in, `NA` when it had none), in row
#'     order
#'   * `all_outliers`: Logical, TRUE if all replicates were outliers
#'   * `qc_info`: QC details
#'   * `conflicts`: One entry per non-numeric response whose replicates
#'     disagree (`column`, `group`, `values`), for `average()` to refuse
#'     together
#'   * `meta_dropped`: The meta columns whose values vary within the group
#'   * `flag`: The step that flagged every replicate, or `NA` when one is
#'     usable
#'   * `n_flag_out`: Flagged replicates left out of QC and the mean
#'   * `qc_run`: Whether QC looked at the group (two usable replicates or
#'     more, with `quality_check`, not every one of them one it cannot
#'     correlate)
#'
#' @noRd
average_group <- function(group_data,
                          group_id,
                          by,
                          predictor_cols,
                          qc_cols       = predictor_cols,
                          meta_cols,
                          response_cols = character(),
                          quality_check,
                          threshold,
                          on_all_outliers) {

  n_reps <- nrow(group_data)

  ## Initialize return values ------------------------------------------------

  all_outliers <- FALSE
  qc_info <- NULL
  conflicts <- list()

  ## The replicates QC removes, with why and each one's mean correlation, as
  ## average()'s record names them (#255); the count it reports is theirs
  removed <- integer(0)
  reason  <- character(0)
  cors    <- rep(NA_real_, n_reps)

  removed_record <- function() {
    ord <- order(removed)
    tibble::tibble(index            = removed[ord],
                   reason           = reason[ord],
                   mean_correlation = cors[removed[ord]])
  }

  ## Flagged replicates ------------------------------------------------------
  ## A replicate a step flagged is one it could not use, so it is left out of
  ## QC and the mean, as every verb that computes over predictors leaves a
  ## flagged row out. A sample whose every replicate is flagged is kept as
  ## one row, flagged with their step: modelling leaves it out, and the step
  ## that flagged it has said so.

  flagged  <- flagged_rows(group_data)
  usable   <- which(!flagged)
  all_flag <- length(usable) == 0
  flag     <- if (all_flag) group_data$.flag[1] else NA_character_

  rows_to_average <- if (all_flag) seq_len(n_reps) else usable
  qc_run          <- quality_check && length(usable) >= 2

  ## -------------------------------------------------------------------------
  ## Quality check (if enabled and n >= 2)
  ## -------------------------------------------------------------------------

  if (qc_run) {

    ## Extract the bands QC correlates over --------------------------------

    predictor_matrix <- as.matrix(group_data[usable, qc_cols, drop = FALSE])

    ## Compute quality, on the usable replicates, in their indexing ------------

    qc <- compute_replicate_quality(predictor_matrix, threshold)
    qc$uncorrelatable <- usable[qc$uncorrelatable]
    qc$outliers       <- usable[qc$outliers]
    qc$best           <- usable[qc$best]
    cors[usable]      <- qc$mean_cors
    qc$mean_cors      <- cors
    qc_info <- qc

    ## Replicates QC cannot correlate ----------------------------------------
    ## Left out of the mean, as a flagged replicate is, and counted among the
    ## replicates QC removed. When every one is, they are all averaged and
    ## standardize() decides on the result, as it does for a lone flat scan.

    n_uncorrelatable <- length(qc$uncorrelatable)

    if (n_uncorrelatable == length(usable)) {

      qc_run <- FALSE

    } else if (n_uncorrelatable > 0) {

      usable          <- setdiff(usable, qc$uncorrelatable)
      rows_to_average <- usable
      removed         <- qc$uncorrelatable
      reason          <- rep("uncorrelatable", n_uncorrelatable)

    }

    ## Handle outliers -------------------------------------------------------

    if (length(qc$outliers) > 0) {

      if (qc$all_fail) {

        ## All replicates are outliers ---------------------------------------

        all_outliers <- TRUE

        if (on_all_outliers == "drop") {

          ## Drop entire sample ----------------------------------------------
          ## Its flagged replicates are counted as left out, in n_flag_out,
          ## so QC removed the rest: each one it could correlate fell below
          ## the threshold in the last round it took part in

          removed <- c(removed, usable)
          reason  <- c(reason, rep("below_threshold", length(usable)))

          return(list(
            averaged     = NULL,
            n_dropped    = length(removed),
            removed      = removed_record(),
            all_outliers = TRUE,
            qc_info      = qc_info,
            flag         = NA_character_,
            n_flag_out   = sum(flagged),
            qc_run       = TRUE
          ))

        } else if (on_all_outliers == "keep_best") {

          ## Keep only the best replicate ------------------------------------
          ## The one with the highest mean correlation before any removal,
          ## not the one the removal left last: its last two always tie.
          ## With two replicates both mean correlations are their one
          ## correlation, so the first is kept (#109).

          rows_to_average <- qc$best
          not_best        <- setdiff(usable, qc$best)
          removed         <- c(removed, not_best)
          reason          <- c(reason, rep("not_best", length(not_best)))

        } else {

          ## warn: keep all --------------------------------------------------

          rows_to_average <- usable

        }

      } else {

        ## Some pass, some fail: drop outliers -------------------------------

        rows_to_average <- setdiff(usable, qc$outliers)
        removed         <- c(removed, qc$outliers)
        reason          <- c(reason, rep("below_threshold", length(qc$outliers)))

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
    ## Over the replicates with a value at each band, NA where none has one,
    ## as a numeric response is averaged

    averaged_predictors <- tibble::as_tibble(
      lapply(rows_to_use[, predictor_cols, drop = FALSE], mean_present)
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

  if (".flag" %in% names(group_data)) result$.flag <- flag

  ## Check each meta column for uniformity -----------------------------------
  ## A column that varies here is reported, so average() can drop it from
  ## every row (#226).

  meta_dropped <- character()

  for (col in meta_cols) {

    col_values <- group_data[[col]]

    if (check_uniformity(col_values)) {

      result[[col]] <- first_present(col_values)

    } else {

      meta_dropped <- c(meta_dropped, col)

    }

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
    n_dropped    = length(removed),
    removed      = removed_record(),
    all_outliers = all_outliers,
    qc_info      = qc_info,
    conflicts    = conflicts,
    meta_dropped = meta_dropped,
    flag         = flag,
    n_flag_out   = if (all_flag) 0L else sum(flagged),
    qc_run       = qc_run
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

    return(mean_present(values))

  }

  if (check_uniformity(values)) {

    return(first_present(values))

  }

  structure(as.character(unique(values[!is.na(values)])), class = "conflict")

}


## ---------------------------------------------------------------------------
## mean_present() — The mean of the values a group has
## ---------------------------------------------------------------------------

#' Mean of the non-missing values, NA when there are none
#'
#' @description
#' `mean(na.rm = TRUE)` over no values is `NaN`; a band or a response no
#' replicate has is missing, so it is `NA`.
#'
#' @param values Numeric vector of the group's values.
#'
#' @return Length-one numeric.
#'
#' @noRd
mean_present <- function(values) {

  if (all(is.na(values))) {

    return(NA_real_)

  }

  mean(values, na.rm = TRUE)

}


## ---------------------------------------------------------------------------
## first_present() — The value a uniform column collapses to
## ---------------------------------------------------------------------------

#' The first non-missing value of a vector
#'
#' @description
#' What a column uniform within a group collapses to, metadata and
#' non-numeric responses alike: the value its replicates share, which a
#' missing first replicate does not hide.
#'
#' @param values Vector of the column's values for the group's replicates.
#'
#' @return Length-one vector of the same type: the first non-`NA` value, or
#'   `values[1]` when every value is `NA`.
#'
#' @noRd
first_present <- function(values) {

  non_na <- values[!is.na(values)]

  if (length(non_na) == 0) values[1] else non_na[1]

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
