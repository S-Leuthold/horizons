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
#' * `"warn"` (default): Keep all replicates and average, emit warning
#' * `"drop"`: Remove the entire sample from results
#' * `"keep_best"`: Keep only the replicate with highest mean correlation
#'
#' **Metadata Handling:**
#' Metadata columns are checked for uniformity within groups:
#' * Uniform values: preserved in output
#' * Non-uniform values: column dropped (tracked in provenance)
#' * `filename` column: always dropped (not meaningful after averaging)
#'
#' @param x A `horizons_data` object from the pipeline.
#' @param by Character. Column name to group replicates by. Default: `"sample_id"`.
#' @param quality_check Logical. Enable correlation-based outlier detection.
#'   Default: `TRUE`.
#' @param correlation_threshold Numeric. Minimum mean pairwise correlation for
#'   a replicate to pass QC. Default: `0.996`. Changing this is rarely needed.
#' @param on_all_outliers Character. Behavior when all replicates fail QC:
#'   * `"warn"` (default): Keep all replicates and average
#'   * `"drop"`: Remove the sample entirely
#'   * `"keep_best"`: Keep only the replicate with highest mean correlation
#' @param verbose Logical. Print progress messages. Default: `TRUE`.
#'
#' @return The input `horizons_data` object with:
#'   * `data$analysis`: One row per unique value in `by` column
#'   * Predictor columns: Mean of replicate values
#'   * Metadata columns: Uniform values preserved, non-uniform dropped
#'   * `data$role_map`: Updated to remove dropped columns
#'   * `provenance$average`: Processing metadata including QC results
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
                    verbose               = TRUE) {

  ## -------------------------------------------------------------------------
  ## Step 1: Input validation
  ## -------------------------------------------------------------------------

  errors <- character()

  ## Validate horizons_data object -------------------------------------------

  if (!inherits(x, "horizons_data")) {

    errors <- c(errors,
                cli::format_inline("{.arg x} must be a horizons_data object, got {.cls {class(x)}}"))

  }

 ## Validate by column exists -----------------------------------------------

  if (inherits(x, "horizons_data") && !by %in% names(x$data$analysis)) {

    errors <- c(errors,
                cli::format_inline("Grouping column {.val {by}} not found in data"))

  }

  ## Validate parameters -----------------------------------------------------

  if (!is.logical(quality_check) || length(quality_check) != 1) {

    errors <- c(errors,
                cli::format_inline("{.arg quality_check} must be TRUE or FALSE"))

  }

  if (!is.numeric(correlation_threshold) || length(correlation_threshold) != 1 ||
      correlation_threshold < 0 || correlation_threshold > 1) {

    errors <- c(errors,
                cli::format_inline("{.arg correlation_threshold} must be a number between 0 and 1"))

  }

  if (!is.character(on_all_outliers) || length(on_all_outliers) == 0) {

    errors <- c(errors,
                cli::format_inline("{.arg on_all_outliers} must be one of 'warn', 'drop', or 'keep_best'"))

  }

  ## Report validation errors with tree style --------------------------------

  if (length(errors) > 0) {

    cat(cli::col_red(cli::style_bold("! Input validation failed:\n")))

    for (i in seq_along(errors)) {

      branch <- if (i < length(errors)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_red(paste0("   ", branch, " ", errors[i], "\n")))

    }

    cat("\n")
    rlang::abort(paste(c("Input validation failed:", errors), collapse = "\n"),
                 class = "horizons_validation_error")

  }

  on_all_outliers <- match.arg(on_all_outliers)

  ## -------------------------------------------------------------------------
  ## Step 2: Identify column roles
  ## -------------------------------------------------------------------------

  role_map <- x$data$role_map

  ## Predictor columns (wavelengths) -----------------------------------------

  predictor_cols <- role_map$variable[role_map$role == "predictor"]

  if (length(predictor_cols) == 0) {

    rlang::abort("No predictor columns found in role_map",
                 class = "horizons_data_error")

  }

  ## Meta columns (excluding by column and filename) -------------------------

  meta_cols <- role_map$variable[role_map$role %in% c("meta", "covariate")]
  meta_cols <- setdiff(meta_cols, c(by, "filename"))

  ## -------------------------------------------------------------------------
  ## Step 3: Group analysis data
  ## -------------------------------------------------------------------------

  analysis <- x$data$analysis

  ## Get grouping vector -----------------------------------------------------

  group_values <- analysis[[by]]

  if (anyNA(group_values)) {

    rlang::abort(
      paste0("Grouping column '", by, "' contains NA values; resolve before averaging"),
      class = "horizons_data_error"
    )

  }

  unique_groups <- unique(group_values)
  n_groups <- length(unique_groups)

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
  n_groups_with_drops <- 0L
  groups_with_drops   <- character()
  all_outlier_samples <- character()
  dropped_samples     <- character()
  qc_details          <- list()

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
                  quality_check       = quality_check,
                  threshold           = correlation_threshold,
                  on_all_outliers     = on_all_outliers) -> group_result

    ## Handle results --------------------------------------------------------

    if (is.null(group_result$averaged)) {

      ## Group was dropped (all outliers + drop mode) ------------------------

      dropped_samples     <- c(dropped_samples, as.character(group_id))
      n_dropped           <- n_dropped + n_reps
      n_groups_with_drops <- n_groups_with_drops + 1L
      groups_with_drops   <- c(groups_with_drops, as.character(group_id))

    } else {

      results_list[[i]] <- group_result$averaged

      if (group_result$n_dropped > 0) {
        n_groups_with_drops <- n_groups_with_drops + 1L
        groups_with_drops   <- c(groups_with_drops, as.character(group_id))
      }

      n_dropped <- n_dropped + group_result$n_dropped

      if (group_result$all_outliers) {

        all_outlier_samples <- c(all_outlier_samples, as.character(group_id))

      }

    }

    ## Store QC details for provenance ---------------------------------------

    if (quality_check && n_reps >= 2) {

      qc_details[[as.character(group_id)]] <- group_result$qc_info

    }

  }

  ## -------------------------------------------------------------------------
  ## Step 5: Combine results
  ## -------------------------------------------------------------------------

  ## Filter out NULLs (dropped groups) ---------------------------------------

  results_list <- results_list[!sapply(results_list, is.null)]

  if (length(results_list) == 0) {

    cat(cli::col_red(cli::style_bold("! All samples dropped due to quality control:\n")))
    cat(cli::col_red(paste0("   \u251C\u2500 ", n_groups, " samples had all replicates flagged as outliers\n")))
    cat(cli::col_red(paste0("   \u2514\u2500 Consider on_all_outliers = 'warn' or lowering threshold\n")))
    cat("\n")

    rlang::abort("All samples dropped due to quality control",
                 class = "horizons_qc_error")

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

  ## Order: by column, retained meta, predictors -----------------------------

  col_order <- c(by, retained_meta, predictor_cols)
  col_order <- col_order[col_order %in% names(averaged)]
  averaged <- averaged[, col_order, drop = FALSE]

  ## -------------------------------------------------------------------------
  ## Step 8: Update role_map
  ## -------------------------------------------------------------------------

  new_role_map <- role_map[!role_map$variable %in% dropped_meta, , drop = FALSE]

  ## -------------------------------------------------------------------------
  ## Step 9: Update horizons_data object
  ## -------------------------------------------------------------------------

  x$data$analysis    <- averaged
  x$data$role_map    <- new_role_map
  x$data$n_rows      <- nrow(averaged)
  x$data$n_covariates <- sum(new_role_map$role == "covariate")

  ## -------------------------------------------------------------------------
  ## Step 10: Update provenance
  ## -------------------------------------------------------------------------

  x$provenance$average <- list(
    by                    = by,
    quality_check         = quality_check,
    correlation_threshold = correlation_threshold,
    on_all_outliers       = on_all_outliers,
    n_groups              = n_groups,
    n_replicates_before   = nrow(analysis),
    n_replicates_after    = nrow(averaged),
    n_replicates_dropped  = n_dropped,
    samples_all_outliers  = all_outlier_samples,
    samples_dropped       = dropped_samples,
    meta_columns_dropped  = dropped_meta,
    applied_at            = Sys.time()
  )

  x$provenance$aggregation_by <- by

  ## -------------------------------------------------------------------------
  ## Step 11: Emit warnings for all-outlier groups
  ## -------------------------------------------------------------------------

  if (length(all_outlier_samples) > 0 && on_all_outliers == "warn") {

    cli::cli_warn(c(
      "All replicates failed QC for {length(all_outlier_samples)} sample{?s}",
      "i" = "Samples: {.val {head(all_outlier_samples, 5)}}",
      if (length(all_outlier_samples) > 5) "i" = "... and {length(all_outlier_samples) - 5} more",
      "i" = "Averaged anyway (consider {.code on_all_outliers = 'keep_best'})"
    ))

  }

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

      n_clean <- n_groups - n_groups_with_drops

      if (n_dropped > 0) {

        cat(paste0("\u2502  \u251C\u2500 QC (r > ", correlation_threshold, ")\n"))
        cat(paste0("\u2502  \u2502  \u251C\u2500 ", n_clean, "/", n_groups,
                   " groups clean\n"))
        cat(paste0("\u2502  \u2502  \u251C\u2500 ", n_groups_with_drops,
                   " groups \u2192 ", n_dropped, " replicates removed\n"))

        if (length(dropped_samples) > 0) {

          cat(paste0("\u2502  \u2502  \u251C\u2500 ", length(dropped_samples),
                     " groups dropped entirely\n"))

        }

        cat(paste0("\u2502  \u2502  \u2514\u2500 ",
                   nrow(averaged), " samples retained\n"))

      } else {

        cat(paste0("\u2502  \u251C\u2500 QC (r > ", correlation_threshold,
                   "): all ", n_groups, " groups clean\n"))

      }

    }

    cat(paste0("\u2502  \u2514\u2500 ", nrow(averaged), " samples \u00D7 ",
               x$data$n_predictors, " predictors\n"))
    cat("\u2502\n")

  }

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
#' @param quality_check Logical. Enable QC.
#' @param threshold Numeric. Correlation threshold.
#' @param on_all_outliers Character. Handling mode for all-outlier groups.
#'
#' @return A list with:
#'   * `averaged`: Single-row tibble (or NULL if dropped)
#'   * `n_dropped`: Number of replicates dropped
#'   * `all_outliers`: Logical, TRUE if all replicates were outliers
#'   * `qc_info`: QC details for provenance
#'
#' @noRd
average_group <- function(group_data,
                          group_id,
                          by,
                          predictor_cols,
                          meta_cols,
                          quality_check,
                          threshold,
                          on_all_outliers) {

  n_reps <- nrow(group_data)

  ## Initialize return values ------------------------------------------------

  n_dropped <- 0
  all_outliers <- FALSE
  qc_info <- NULL
  rows_to_average <- seq_len(n_reps)

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

  result <- dplyr::bind_cols(result, averaged_predictors)

  list(
    averaged     = result,
    n_dropped    = n_dropped,
    all_outliers = all_outliers,
    qc_info      = qc_info
  )

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
