#' Pipeline: Validate Data for Modeling
#'
#' @description
#' Runs pre-flight checks on a configured `horizons_data` object before
#' expensive model evaluation. Checks sample sufficiency, outcome health,
#' predictor quality, and detects spectral and response outliers.
#'
#' @details
#' `validate()` is the gate between `configure()` and `evaluate()`. Detection
#' is mandatory; outlier removal is opt-in. The function populates the
#' `validation` section of the object and sets `validation$passed` based on
#' whether any ERROR-severity checks failed.
#'
#' **Severity levels:**
#'
#' - **ERROR**: Blocks modeling. `validation$passed` set to `FALSE`.
#' - **WARNING**: Non-blocking but noteworthy. Does not affect `passed`.
#' - **INFO**: Descriptive/detection results. Does not affect `passed`.
#'
#' **Outlier detection:**
#'
#' Spectral outliers are detected via Mahalanobis distance on PCA scores
#' of the raw (unpreprocessed) spectral matrix, scaled to unit variance per
#' wavenumber. Response outliers use IQR-based Tukey fences on the outcome
#' variable.
#'
#' **Idempotency:**
#'
#' Running `validate()` multiple times overwrites previous results. If
#' outliers were removed in a prior call, those rows are gone; re-validation
#' operates on the reduced dataset.
#'
#' @param x [horizons_data]. Configured object (must have `config$configs`).
#' @param remove_outliers [logical(1) or character(1)]. `FALSE` to detect
#'   only (default), `TRUE` to remove all flagged outliers, `"spectral"` or
#'   `"response"` for selective removal.
#' @param spectral_method [character(1)]. Spectral outlier detection method.
#'   Currently only `"mahalanobis"`. Default: `"mahalanobis"`.
#' @param spectral_threshold [numeric(1)]. Chi-squared quantile for
#'   Mahalanobis distance cutoff. Default: `0.975`.
#' @param response_method [character(1)]. Response outlier detection method.
#'   Currently only `"iqr"`. Default: `"iqr"`.
#' @param response_threshold [numeric(1)]. IQR multiplier for Tukey fences.
#'   Default: `1.5`.
#'
#' @return [horizons_data]. Same object with `validation` section populated:
#'   - `validation$passed`: `TRUE` if no ERROR checks failed
#'   - `validation$checks`: tibble of check results
#'   - `validation$outliers`: detected and optionally removed outlier IDs
#'   - `validation$timestamp`: when validation ran
#'
#' @examples
#' \dontrun{
#' x |>
#'   configure(models = c("rf", "cubist")) |>
#'   validate() |>
#'   evaluate()
#'
#' # Remove outliers
#' x |> validate(remove_outliers = TRUE)
#'
#' # Remove only spectral outliers
#' x |> validate(remove_outliers = "spectral")
#' }
#'
#' @export
validate <- function(x,
                     remove_outliers    = FALSE,
                     spectral_method    = "mahalanobis",
                     spectral_threshold = 0.975,
                     response_method    = "iqr",
                     response_threshold = 1.5) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Print header
  ## ---------------------------------------------------------------------------

  cat("\u2502\n")
  cat(paste0("\u251C\u2500 ", cli::style_bold("Validating horizons object"), "...\n"))

  ## ---------------------------------------------------------------------------
  ## Helper: abort with tree-nested error
  ## ---------------------------------------------------------------------------

  abort_nested <- function(header, details, error_class = "horizons_validate_error") {

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
  ## Helper: record a single check result
  ## ---------------------------------------------------------------------------

  run_check <- function(check_id, description, condition, severity, value = NULL) {

    tibble::tibble(
      check_id    = check_id,
      description = description,
      status      = if (condition) "pass" else "fail",
      severity    = severity,
      value       = as.character(value %||% "")
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  ## 1.1 Class check ------------------------------------------------------------

  if (!inherits(x, "horizons_data")) {

    cat(cli::col_red(paste0(
      "\u2502  \u2514\u2500 x must be a horizons_data object (got ", class(x)[1], ")\n"
    )))
    cat("\n")
    rlang::abort(
      paste("Expected a `horizons_data` object. Got:", class(x)[1]),
      class = "horizons_validate_error"
    )

  }

  ## 1.2 Config must exist ------------------------------------------------------

  if (is.null(x$config$configs)) {

    abort_nested(
      "Object not configured",
      c("Run `configure()` first.")
    )

  }

  ## 1.3 Validate arguments -----------------------------------------------------

  valid_remove <- c(FALSE, TRUE, "spectral", "response")
  if (!isFALSE(remove_outliers) && !isTRUE(remove_outliers) &&
      !(is.character(remove_outliers) && remove_outliers %in% c("spectral", "response"))) {

    abort_nested(
      "Invalid `remove_outliers`",
      c("Must be FALSE, TRUE, \"spectral\", or \"response\"",
        paste0("Got: ", deparse(remove_outliers)))
    )

  }

  if (!is.character(spectral_method) || length(spectral_method) != 1 ||
      spectral_method != "mahalanobis") {

    abort_nested(
      "Invalid `spectral_method`",
      c("Must be \"mahalanobis\"",
        paste0("Got: \"", spectral_method, "\""))
    )

  }

  if (!is.character(response_method) || length(response_method) != 1 ||
      response_method != "iqr") {

    abort_nested(
      "Invalid `response_method`",
      c("Must be \"iqr\"",
        paste0("Got: \"", response_method, "\""))
    )

  }

  if (!is.numeric(spectral_threshold) || length(spectral_threshold) != 1 ||
      is.na(spectral_threshold) || spectral_threshold <= 0 || spectral_threshold >= 1) {

    abort_nested(
      "`spectral_threshold` must be between 0 and 1 (exclusive)",
      c(paste0("Got: ", spectral_threshold))
    )

  }

  if (!is.numeric(response_threshold) || length(response_threshold) != 1 ||
      is.na(response_threshold) || response_threshold <= 0) {

    abort_nested(
      "`response_threshold` must be positive",
      c(paste0("Got: ", response_threshold))
    )

  }

  ## 1.4 Warn on re-validation --------------------------------------------------

  if (!is.null(x$validation$passed)) {

    warning("Overwriting previous validation", call. = FALSE)

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Compute sample counts + extract outcome
  ## ---------------------------------------------------------------------------

  analysis <- x$data$analysis
  n_total  <- nrow(analysis)

  ## Outcome extraction ---------------------------------------------------------

  outcome_col <- x$data$role_map$variable[x$data$role_map$role == "outcome"]
  has_outcome <- length(outcome_col) == 1 && outcome_col %in% names(analysis)

  if (has_outcome) {

    outcome_vals <- analysis[[outcome_col]]
    n_model      <- sum(!is.na(outcome_vals))

  } else {

    outcome_vals <- NULL
    n_model      <- n_total

  }

  ## CV folds from tuning config ------------------------------------------------

  cv_folds <- x$config$tuning$cv_folds

  ## ---------------------------------------------------------------------------
  ## Step 3: Initialize checks collector
  ## ---------------------------------------------------------------------------

  checks <- tibble::tibble(
    check_id    = character(0),
    description = character(0),
    status      = character(0),
    severity    = character(0),
    value       = character(0)
  )

  ## ---------------------------------------------------------------------------
  ## Step 4: P007 — Near-zero variance predictors (runs before PCA)
  ## ---------------------------------------------------------------------------

  predictor_cols <- x$data$role_map$variable[x$data$role_map$role == "predictor"]
  predictor_cols <- intersect(predictor_cols, names(analysis))

  nzv_cols <- character(0)
  if (length(predictor_cols) > 0) {

    variances <- vapply(
      analysis[, predictor_cols, drop = FALSE],
      function(col) {
        non_na <- col[!is.na(col)]
        if (length(non_na) < 2) return(NA_real_)
        stats::var(non_na)
      },
      numeric(1)
    )

    nzv_cols <- predictor_cols[!is.na(variances) & variances < 1e-10]

  }

  checks <- rbind(checks, run_check(
    "P007", "Near-zero variance predictors",
    condition = length(nzv_cols) == 0,
    severity  = "WARNING",
    value     = as.character(length(nzv_cols))
  ))

  ## ---------------------------------------------------------------------------
  ## Step 5: P001 — Recommended sample count (n_total)
  ## ---------------------------------------------------------------------------

  checks <- rbind(checks, run_check(
    "P001", "Recommended sample count",
    condition = n_total >= 50,
    severity  = "WARNING",
    value     = as.character(n_total)
  ))

  ## ---------------------------------------------------------------------------
  ## Step 6: P001b — CV feasibility (n_model)
  ## ---------------------------------------------------------------------------

  checks <- rbind(checks, run_check(
    "P001b", "Feasible CV split",
    condition = n_model >= cv_folds * 2,
    severity  = "ERROR",
    value     = paste0(n_model, " (need >= ", cv_folds * 2, ")")
  ))

  ## ---------------------------------------------------------------------------
  ## Step 7: P002 — Samples per CV fold (n_model)
  ## ---------------------------------------------------------------------------

  samples_per_fold <- floor(n_model / cv_folds)

  checks <- rbind(checks, run_check(
    "P002", "Samples per CV fold",
    condition = samples_per_fold >= 10,
    severity  = "WARNING",
    value     = paste0("~", samples_per_fold)
  ))

  ## ---------------------------------------------------------------------------
  ## Step 8: P003 — Outcome variance (gated)
  ## ---------------------------------------------------------------------------

  if (has_outcome) {

    outcome_complete <- outcome_vals[!is.na(outcome_vals)]
    outcome_var      <- if (length(outcome_complete) >= 2) stats::var(outcome_complete) else 0

    checks <- rbind(checks, run_check(
      "P003", "Outcome variance",
      condition = outcome_var > 0,
      severity  = "ERROR",
      value     = format(outcome_var, digits = 4)
    ))

  }

  ## ---------------------------------------------------------------------------
  ## Step 9: P004 — Outcome NA proportion (gated)
  ## ---------------------------------------------------------------------------

  if (has_outcome) {

    n_na     <- sum(is.na(outcome_vals))
    pct_na   <- round(100 * n_na / n_total, 1)

    checks <- rbind(checks, run_check(
      "P004", "Outcome NA proportion",
      condition = pct_na <= 20,
      severity  = "WARNING",
      value     = paste0(n_na, "/", n_total, " (", pct_na, "%)")
    ))

  }

  ## ---------------------------------------------------------------------------
  ## Step 10: P005 — Spectral outlier detection
  ## ---------------------------------------------------------------------------

  spectral_outlier_ids <- detect_spectral_outliers(
    analysis       = analysis,
    predictor_cols = predictor_cols,
    nzv_cols       = nzv_cols,
    threshold      = spectral_threshold
  )

  checks <- rbind(checks, run_check(
    "P005", "Spectral outliers",
    condition = TRUE,
    severity  = "INFO",
    value     = paste0(length(spectral_outlier_ids), " flagged")
  ))

  ## ---------------------------------------------------------------------------
  ## Step 11: P006 — Response outlier detection (gated)
  ## ---------------------------------------------------------------------------

  response_outlier_ids <- character(0)

  if (has_outcome) {

    response_outlier_ids <- detect_response_outliers(
      analysis  = analysis,
      outcome_col = outcome_col,
      threshold   = response_threshold
    )

    checks <- rbind(checks, run_check(
      "P006", "Response outliers",
      condition = TRUE,
      severity  = "INFO",
      value     = paste0(length(response_outlier_ids), " flagged")
    ))

  }

  ## ---------------------------------------------------------------------------
  ## Step 12: Outlier removal (if requested)
  ## ---------------------------------------------------------------------------

  removed_ids    <- character(0)
  removal_detail <- NULL
  did_remove     <- FALSE

  if (!isFALSE(remove_outliers)) {

    ids_to_remove <- switch(
      as.character(remove_outliers),
      "TRUE"     = union(spectral_outlier_ids, response_outlier_ids),
      "spectral" = spectral_outlier_ids,
      "response" = response_outlier_ids
    )

    if (length(ids_to_remove) > 0) {

      x$data$analysis <- x$data$analysis[
        !x$data$analysis$sample_id %in% ids_to_remove,
      ]
      x$data$n_rows <- nrow(x$data$analysis)

      ## Build removal detail tibble
      in_spectral <- ids_to_remove %in% spectral_outlier_ids
      in_response <- ids_to_remove %in% response_outlier_ids

      reason <- dplyr::case_when(
        in_spectral & in_response ~ "both",
        in_spectral               ~ "spectral",
        TRUE                      ~ "response"
      )

      removal_detail <- tibble::tibble(
        sample_id = ids_to_remove,
        reason    = reason
      )

      removed_ids <- ids_to_remove
      did_remove  <- TRUE

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 13: Determine passed/failed
  ## ---------------------------------------------------------------------------

  error_checks <- checks[checks$severity == "ERROR" & checks$status == "fail", ]
  passed       <- nrow(error_checks) == 0

  has_warnings <- any(checks$severity == "WARNING" & checks$status == "fail")

  ## ---------------------------------------------------------------------------
  ## Step 14: Store results
  ## ---------------------------------------------------------------------------

  x$validation$passed    <- passed
  x$validation$checks    <- checks
  x$validation$timestamp <- Sys.time()

  x$validation$outliers$spectral_ids   <- spectral_outlier_ids
  x$validation$outliers$response_ids   <- response_outlier_ids
  x$validation$outliers$removed_ids    <- removed_ids
  x$validation$outliers$removal_detail <- removal_detail
  x$validation$outliers$removed        <- did_remove

  ## ---------------------------------------------------------------------------
  ## Step 15: CLI output
  ## ---------------------------------------------------------------------------

  cat("\u2502\n")

  ## Sample checks --------------------------------------------------------------

  cat(paste0("\u251C\u2500 ", cli::style_bold("Sample checks"), "\n"))

  p001 <- checks[checks$check_id == "P001", ]
  sym  <- if (p001$status == "pass") cli::col_green("\u2713") else cli::col_yellow("\u26A0")
  cat(paste0("\u2502  \u251C\u2500 ", sym, " Sample count: ", n_total, " (recommended \u2265 50)\n"))

  p001b <- checks[checks$check_id == "P001b", ]
  sym   <- if (p001b$status == "pass") cli::col_green("\u2713") else cli::col_red("\u2717")
  cat(paste0("\u2502  \u251C\u2500 ", sym, " CV feasibility: ", n_model,
             " complete cases \u2265 ", cv_folds * 2, " (", cv_folds, " folds \u00D7 2)\n"))

  p002 <- checks[checks$check_id == "P002", ]
  sym  <- if (p002$status == "pass") cli::col_green("\u2713") else cli::col_yellow("\u26A0")
  cat(paste0("\u2502  \u2514\u2500 ", sym, " Samples per fold: ~", samples_per_fold,
             " (recommended \u2265 10)\n"))

  ## Outcome checks (gated) ----------------------------------------------------

  if (has_outcome) {

    cat(paste0("\u2502\n"))
    cat(paste0("\u251C\u2500 ", cli::style_bold("Outcome checks"), " (", outcome_col, ")\n"))

    p003 <- checks[checks$check_id == "P003", ]
    sym  <- if (p003$status == "pass") cli::col_green("\u2713") else cli::col_red("\u2717")
    cat(paste0("\u2502  \u251C\u2500 ", sym, " Variance: ",
               format(outcome_var, digits = 4), " (> 0)\n"))

    p004 <- checks[checks$check_id == "P004", ]
    sym  <- if (p004$status == "pass") cli::col_green("\u2713") else cli::col_yellow("\u26A0")
    cat(paste0("\u2502  \u2514\u2500 ", sym, " Missing: ", n_na, "/", n_total,
               " (", pct_na, "%)\n"))

  }

  ## Predictor checks -----------------------------------------------------------

  cat(paste0("\u2502\n"))
  cat(paste0("\u251C\u2500 ", cli::style_bold("Predictor checks"), "\n"))

  p007 <- checks[checks$check_id == "P007", ]
  sym  <- if (p007$status == "pass") cli::col_green("\u2713") else cli::col_yellow("\u26A0")
  cat(paste0("\u2502  \u2514\u2500 ", sym, " Near-zero variance: ",
             length(nzv_cols), " predictors flagged\n"))

  ## Outlier detection ----------------------------------------------------------

  cat(paste0("\u2502\n"))
  cat(paste0("\u251C\u2500 ", cli::style_bold("Outlier detection"), "\n"))

  cat(paste0("\u2502  \u251C\u2500 ",
             cli::col_cyan("\u2139"), " Spectral: ",
             length(spectral_outlier_ids), " flagged",
             " (Mahalanobis, threshold = qchisq(", spectral_threshold, ", df=k))\n"))

  if (has_outcome) {

    cat(paste0("\u2502  \u2514\u2500 ",
               cli::col_cyan("\u2139"), " Response: ",
               length(response_outlier_ids), " flagged",
               " (IQR \u00D7 ", response_threshold, ")\n"))

  } else {

    cat(paste0("\u2502  \u2514\u2500 ", cli::col_grey("Response: skipped (no outcome)"), "\n"))

  }

  ## Outlier removal summary ----------------------------------------------------

  if (did_remove) {

    n_spectral_only <- sum(removal_detail$reason == "spectral")
    n_response_only <- sum(removal_detail$reason == "response")
    n_both          <- sum(removal_detail$reason == "both")

    cat(paste0("\u2502\n"))
    cat(paste0("\u251C\u2500 ", cli::style_bold("Outlier removal"), "\n"))
    cat(paste0("\u2502  \u251C\u2500 Removed ", length(removed_ids), " unique samples",
               " (", n_spectral_only, " spectral, ",
               n_response_only, " response, ",
               n_both, " both)\n"))
    cat(paste0("\u2502  \u2514\u2500 ", x$data$n_rows, " samples remaining\n"))

  }

  ## Status line ----------------------------------------------------------------

  cat(paste0("\u2502\n"))

  if (!passed) {

    cat(paste0("\u2514\u2500 ", cli::style_bold(cli::col_red("Status: FAILED")), "\n"))

    for (i in seq_len(nrow(error_checks))) {
      branch <- if (i < nrow(error_checks)) "\u251C\u2500" else "\u2514\u2500"
      cat(paste0("   ", branch, " ", error_checks$description[i],
                  " (", error_checks$value[i], ")\n"))
    }
    cat(paste0("   \u2514\u2500 Fix data or use skip_validation = TRUE in evaluate()\n"))

  } else if (has_warnings) {

    cat(paste0("\u2514\u2500 ", cli::style_bold(cli::col_yellow("Status: PASSED with warnings")), "\n"))

    if (!isFALSE(remove_outliers) && length(spectral_outlier_ids) + length(response_outlier_ids) == 0) {

      cat(paste0("   \u2514\u2500 No outliers to remove\n"))

    } else if (isFALSE(remove_outliers) &&
               (length(spectral_outlier_ids) > 0 || length(response_outlier_ids) > 0)) {

      cat(paste0("   \u2514\u2500 To remove outliers: validate(x, remove_outliers = TRUE)\n"))

    }

  } else {

    cat(paste0("\u2514\u2500 ", cli::style_bold(cli::col_green("Status: PASSED")), "\n"))

    if (isFALSE(remove_outliers) &&
        (length(spectral_outlier_ids) > 0 || length(response_outlier_ids) > 0)) {

      cat(paste0("   \u2514\u2500 To remove outliers: validate(x, remove_outliers = TRUE)\n"))

    }

  }

  cat("\n")

  ## ---------------------------------------------------------------------------
  ## Return
  ## ---------------------------------------------------------------------------

  invisible(x)

}


## =============================================================================
## Internal helpers
## =============================================================================


#' Detect spectral outliers via Mahalanobis distance on PCA scores
#'
#' @description
#' Computes PCA on the raw spectral matrix (scaled to unit variance per
#' wavenumber), retains components capturing 99% of variance, then flags
#' samples whose squared Mahalanobis distance exceeds the chi-squared
#' quantile threshold.
#'
#' @param analysis [tibble]. The analysis table.
#' @param predictor_cols [character]. Predictor column names.
#' @param nzv_cols [character]. Near-zero variance columns to exclude.
#' @param threshold [numeric(1)]. Chi-squared quantile (0-1).
#'
#' @return [character]. sample_id values of flagged outliers.
#'
#' @noRd
detect_spectral_outliers <- function(analysis, predictor_cols, nzv_cols, threshold) {

  ## Exclude near-zero variance columns
  clean_cols <- setdiff(predictor_cols, nzv_cols)

  if (length(clean_cols) < 2) {

    warning("Fewer than 2 predictor columns after NZV removal. Skipping spectral outlier detection.",
            call. = FALSE)
    return(character(0))

  }

  n <- nrow(analysis)

  ## Guard: too few samples for meaningful PCA
  if (n < 5) {

    warning(paste0("Too few samples for spectral outlier detection (n=", n,
                   "). Skipping."), call. = FALSE)
    return(character(0))

  }

  ## PCA on raw spectra, scaled to unit variance per wavenumber
  ## Scaling ensures equal weighting so high-variance regions (e.g., water bands)
  ## don't dominate detection. Catches instrumentation-driven outliers.
  spectra_matrix <- as.matrix(analysis[, clean_cols, drop = FALSE])

  pca <- stats::prcomp(spectra_matrix, scale. = TRUE)

  ## Retain components capturing 99% variance, capped
  cumvar <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k <- which(cumvar >= 0.99)[1]

  ## Guard: k is NA or < 1
  if (is.na(k) || k < 1) {

    warning("PCA retained zero components. Skipping spectral outlier detection.",
            call. = FALSE)
    return(character(0))

  }

  k <- min(k, n - 2, 50)

  ## Guard: k must still be >= 1 after capping
  if (k < 1) {

    warning("Too few usable PCA components after capping. Skipping spectral outlier detection.",
            call. = FALSE)
    return(character(0))

  }

  scores <- pca$x[, 1:k, drop = FALSE]
  center <- colMeans(scores)

  ## Singularity check via Cholesky decomposition
  S <- stats::cov(scores)
  chol_result <- tryCatch(chol(S), error = function(e) NULL)

  if (is.null(chol_result)) {

    warning("Covariance matrix singular \u2014 skipping spectral outlier detection.",
            call. = FALSE)
    return(character(0))

  }

  ## Compute Mahalanobis distances
  distances  <- stats::mahalanobis(scores, center, S)
  cutoff     <- stats::qchisq(threshold, df = k)
  outlier_ids <- analysis$sample_id[distances > cutoff]

  outlier_ids

}


#' Detect response outliers via IQR-based Tukey fences
#'
#' @description
#' Flags outcome values outside `[Q1 - threshold*IQR, Q3 + threshold*IQR]`.
#' Only non-NA outcome values are considered for fence computation.
#'
#' @param analysis [tibble]. The analysis table.
#' @param outcome_col [character(1)]. Name of the outcome column.
#' @param threshold [numeric(1)]. IQR multiplier.
#'
#' @return [character]. sample_id values of flagged outliers.
#'
#' @noRd
detect_response_outliers <- function(analysis, outcome_col, threshold) {

  outcome_vals  <- analysis[[outcome_col]]
  complete_mask <- !is.na(outcome_vals)
  outcome_complete <- outcome_vals[complete_mask]

  if (length(outcome_complete) < 4) {

    warning(paste0("Too few non-NA outcome values for IQR detection (n=",
                   length(outcome_complete), "). Skipping."), call. = FALSE)
    return(character(0))

  }

  q     <- stats::quantile(outcome_complete, c(0.25, 0.75))
  iqr   <- q[2] - q[1]

  if (iqr < sqrt(.Machine$double.eps)) {

    warning("IQR is zero or near-zero \u2014 skipping response outlier detection.",
            call. = FALSE)
    return(character(0))

  }

  lower <- q[1] - threshold * iqr
  upper <- q[2] + threshold * iqr

  outlier_mask <- complete_mask &
    (outcome_vals < lower | outcome_vals > upper)

  analysis$sample_id[outlier_mask]

}
