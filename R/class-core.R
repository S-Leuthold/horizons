## -----------------------------------------------------------------------------
## class-core.R: S3 class constructors for horizons objects
## -----------------------------------------------------------------------------
##
## Class hierarchy:
##   horizons_data → horizons_eval → horizons_fit
##
## This file contains internal constructors (new_*) that build the object
## structure. User-facing constructors (spectra(), etc.) live in pipeline-*.R.
##
## -----------------------------------------------------------------------------


#' Internal constructor for horizons_data class
#'
#' @description
#' Builds the foundational 8-section object structure for horizons workflows.
#' This is a low-level constructor that assembles the object without validation.
#' User-facing constructors like `spectra()` call this internally after input
#' checking.
#'
#' @details
#' The horizons_data class is the base of the class hierarchy (horizons_data →
#' horizons_eval → horizons_fit). This constructor initializes all 8 sections
#' with either provided values or NULL defaults:
#'
#' 1. **data**: The analysis tibble and role_map
#' 2. **provenance**: Source files, transforms applied, version info
#' 3. **config**: Model configurations and tuning parameters
#' 4. **validation**: Pre-flight check results and outlier tracking
#' 5. **evaluation**: Model comparison results (populated by evaluate())
#' 6. **models**: Fitted workflows and UQ models (populated by finalize())
#' 7. **ensemble**: Optional stacked ensemble (populated by ensemble())
#' 8. **artifacts**: Paths to disk-backed storage for large objects
#'
#' Derived values (n_rows, n_predictors, n_covariates) are computed from
#' the provided data rather than passed as arguments. This ensures consistency
#' and prevents mismatches between metadata and actual data.
#'
#' This function does NOT validate inputs. Call [validate_horizons_data()]
#' after construction to verify structural integrity.
#'
#' @param analysis [tibble or NULL.] The analysis data in wide format with
#'   sample_id column and wavelength columns as predictors. Default: `NULL`.
#' @param role_map [tibble or NULL.] Maps variable names to roles (id, predictor,
#'   covariate, outcome, meta). Must have columns `variable` and `role`.
#'   Default: `NULL`.
#' @param spectra_source [character or NULL.] Path to source spectra files.
#'   Default: `NULL`.
#' @param spectra_type [character or NULL.] Type of spectra source (e.g., "opus",
#'   "csv", "asd"). Default: `NULL`.
#' @param response_source [character or NULL.] Path to response data file.
#'   Default: `NULL`.
#' @param ossl_properties [character vector or NULL.] Properties requested from
#'   OSSL library predictions. Default: `NULL`.
#'
#' @return [horizons_data]. An unvalidated horizons_data object with class
#'   `c("horizons_data", "list")`.
#'
#' @seealso [validate_horizons_data()] for structural validation,
#'   `spectra()` for user-facing construction.
#'
#' @noRd
new_horizons_data <- function(analysis        = NULL,
                              role_map        = NULL,
                              spectra_source  = NULL,
                              spectra_type    = NULL,
                              response_source = NULL,
                              ossl_properties = NULL) {

 ## ----------------------------------------------------------------------------
 ## Compute derived counts from role_map
 ## ----------------------------------------------------------------------------

  if (!is.null(role_map)) {

    n_predictors <- sum(role_map$role == "predictor", na.rm = TRUE)
    n_covariates <- sum(role_map$role == "covariate", na.rm = TRUE)

  } else {

    n_predictors <- NULL
    n_covariates <- NULL

  }

  n_rows <- if (!is.null(analysis)) nrow(analysis) else NULL


  ## ---------------------------------------------------------------------------
  ## Build object structure
  ## ---------------------------------------------------------------------------

  obj <- list(

    ## -------------------------------------------------------------------------
    ## Section 1: DATA — The canonical modeling table
    ## -------------------------------------------------------------------------

    data = list(analysis     = analysis,
                role_map     = role_map,
                n_rows       = n_rows,
                n_predictors = n_predictors,
                n_covariates = n_covariates),

    ## -------------------------------------------------------------------------
    ## Section 2: PROVENANCE — Source and transform history
    ## -------------------------------------------------------------------------

    ## Source provenance (immutable once set)  ---------------------------------

    provenance = list(spectra_source   = spectra_source,
                      spectra_type     = spectra_type,
                      response_source  = response_source,
                      ossl_properties  = ossl_properties,
                      created          = Sys.time(),
                      horizons_version = utils::packageVersion("horizons"),
                      schema_version   = 1L,

    ## Transform provenance (updated by pipeline verbs) ------------------------

                      preprocessing        = NULL,
                      preprocessing_params = list(),
                      id_pattern           = NULL,
                      aggregation_by       = NULL),

    ## -------------------------------------------------------------------------
    ## Section 3: CONFIG — Model configurations
    ## -------------------------------------------------------------------------

    config = list(configs   = NULL,
                  n_configs = NULL,

                  tuning = list(grid_size     = 10L,
                                bayesian_iter = 15L,
                                cv_folds      = 5L)),

    ## -------------------------------------------------------------------------
    ## Section 4: VALIDATION — Pre-flight check results
    ## -------------------------------------------------------------------------

    validation = list(passed    = NULL,
                      checks    = NULL,
                      timestamp = NULL,

                      outliers = list(spectral_idx = NULL,
                                      response_idx = NULL,
                                      removed      = FALSE)),

    ## -------------------------------------------------------------------------
    ## Section 5: EVALUATION — Model comparison results (horizons_eval+)
    ## -------------------------------------------------------------------------

    evaluation = list(results     = NULL,
                      best_config = NULL,
                      rank_metric = NULL,
                      backend     = NULL,
                      runtime     = NULL,
                      timestamp   = NULL),

    ## -------------------------------------------------------------------------
    ## Section 6: MODELS — Finalized models + UQ (horizons_fit+)
    ## -------------------------------------------------------------------------

    models = list(workflows = NULL,
                  n_models  = NULL,
                  uq = list(enabled         = FALSE,
                            quantile_models = NULL,
                            conformal       = NULL,
                            ad_metadata     = NULL)),

    ## -------------------------------------------------------------------------
    ## Section 7: ENSEMBLE — Optional ensemble (horizons_fit only)
    ## -------------------------------------------------------------------------

    ensemble = list(stack   = NULL,
                    method  = NULL,
                    weights = NULL,
                    metrics = NULL),

    ## -------------------------------------------------------------------------
    ## Section 8: ARTIFACTS — Disk-backed storage paths
    ## -------------------------------------------------------------------------

    artifacts = list(cv_preds = list(path  = NULL,
                                     index = NULL),
                     fit_objects = list(path  = NULL,
                                        index = NULL),
                     cache_dir = NULL)
  )


  ## -------------------------------------------------------------------------
  ## Set class and return
  ## -------------------------------------------------------------------------

  class(obj) <- c("horizons_data", "list")

  obj

}


#' Structural validator for horizons_data objects
#'
#' @description
#' Checks structural integrity of a horizons_data object. Silent on success,
#' collects all validation errors and reports them together with tree-style
#' formatting on failure.
#'
#' @details
#' This validator checks structural requirements, not data quality. It ensures
#' the object is well-formed and internally consistent. Data quality checks
#' (outlier detection, response distribution, etc.) belong in separate
#' diagnostic functions.
#'
#' **Validation checks performed:**
#'
#' 1. **Pairing**: If `analysis` exists, `role_map` must also exist (and vice
#'    versa). This is a gate check — failure here aborts immediately since
#'    subsequent checks depend on both being present.
#'
#' 2. **sample_id column**: Must exist in analysis and contain unique values.
#'    Duplicates are reported by ID.
#'
#' 3. **Predictor columns**: Wavelength/predictor columns (role = "predictor")
#'    must not contain NA or Inf values. First 3 problematic columns are named.
#'
#' 4. **Wavelength order**: Numeric column names (interpreted as wavenumbers)
#'    must be in strictly decreasing order (e.g., 4000, 3998, 3996...).
#'
#' 5. **role_map completeness**: Every column in analysis must appear in
#'    role_map. Missing columns are listed.
#'
#' 6. **Single id role**: Exactly one variable must have role = "id".
#'    Zero or multiple id roles both fail.
#'
#' Empty objects (both analysis and role_map NULL) pass validation — this
#' allows for incremental object construction.
#'
#' @param x [horizons_data]. The object to validate.
#'
#' @return [horizons_data]. The input object, unchanged, if validation passes.
#'   Aborts with class `horizons_validation_error` if validation fails.
#'
#' @seealso [new_horizons_data()] for object construction.
#'
#' @noRd
validate_horizons_data <- function(x) {

  ## ---------------------------------------------------------------------------
  ## Early return for empty object
  ## ---------------------------------------------------------------------------

  if (is.null(x$data$analysis) && is.null(x$data$role_map)) {
    return(x)
  }

  ## ---------------------------------------------------------------------------
  ## Gate check: analysis and role_map must be paired
  ## ---------------------------------------------------------------------------

  has_analysis <- !is.null(x$data$analysis)
  has_role_map <- !is.null(x$data$role_map)

  if (has_analysis && !has_role_map) {

    cli::cli_abort("Object has {.field analysis} but no {.field role_map}")

  }

  if (has_role_map && !has_analysis) {

    cli::cli_abort("Object has {.field role_map} but no {.field analysis}")

  }

  ## ---------------------------------------------------------------------------
  ## Collect errors for remaining checks
  ## ---------------------------------------------------------------------------

  errors   <- character()
  analysis <- x$data$analysis
  role_map <- x$data$role_map

  ## sample_id checks ----


  if (!"sample_id" %in% names(analysis)) {

    errors <- c(errors, cli::format_inline("Column {.field sample_id} missing from {.field analysis}"))

    } else {

    dup_ids <- analysis$sample_id[duplicated(analysis$sample_id)]

    if (length(dup_ids) > 0) {

      dup_str <- paste(unique(dup_ids), collapse = ", ")
      errors  <- c(errors, cli::format_inline("Duplicate {.field sample_id} values: {dup_str}"))

    }
  }

  ## Wavelength column checks --------------------------------------------------

  predictor_vars <- role_map$variable[role_map$role == "predictor"]

  # Guard against role_map columns missing from analysis -----------------------

  missing_predictors <- setdiff(predictor_vars, names(analysis))

  if (length(missing_predictors) > 0) {

    col_list <- paste(missing_predictors[1:min(5, length(missing_predictors))], collapse = ", ")
    errors   <- c(errors, cli::format_inline("Predictors in {.field role_map} missing from {.field analysis}: {col_list}"))
    predictor_vars <- intersect(predictor_vars, names(analysis))

  }

  predictor_cols <- analysis[, predictor_vars, drop = FALSE]

  # Check for NA ---------------------------------------------------------------

  na_check <- sapply(predictor_cols, function(col) any(is.na(col)))

  if (any(na_check)) {

    na_cols  <- names(na_check)[na_check]
    col_list <- paste(na_cols[1:min(3, length(na_cols))], collapse = ", ")
    errors   <- c(errors, cli::format_inline("NA values in predictor columns: {col_list}"))

  }

  # Check for Inf --------------------------------------------------------------

  inf_check <- sapply(predictor_cols, function(col) any(is.infinite(col)))
  if (any(inf_check)) {

    inf_cols <- names(inf_check)[inf_check]
    col_list <- paste(inf_cols[1:min(3, length(inf_cols))], collapse = ", ")
    errors   <- c(errors, cli::format_inline("Infinite values in predictor columns: {col_list}"))

  }

  # Check wavelength order (numeric column names should be decreasing) ---------

  numeric_predictors <- predictor_vars[grepl("^[0-9.]+$", predictor_vars)]

  if (length(numeric_predictors) > 1) {

    wn_values <- as.numeric(numeric_predictors)

    if (!all(diff(wn_values) < 0)) {

      errors <- c(errors, cli::format_inline("Wavelength columns must be in decreasing order"))

    }
  }

  ## role_map completeness checks ----------------------------------------------

  # Columns in analysis but not in role_map
  missing_from_role_map <- setdiff(names(analysis), role_map$variable)

  if (length(missing_from_role_map) > 0) {

    col_list <- paste(missing_from_role_map, collapse = ", ")
    errors   <- c(errors, cli::format_inline("Columns missing from {.field role_map}: {col_list}"))

  }

  # Columns in role_map but not in analysis (non-predictors already handled above)
  non_predictor_vars     <- role_map$variable[role_map$role != "predictor"]
  missing_from_analysis  <- setdiff(non_predictor_vars, names(analysis))

  if (length(missing_from_analysis) > 0) {

    col_list <- paste(missing_from_analysis, collapse = ", ")
    errors   <- c(errors, cli::format_inline("Non-predictor columns in {.field role_map} missing from {.field analysis}: {col_list}"))

  }

  ## id role checks ------------------------------------------------------------

  n_id_roles <- sum(role_map$role == "id")

  if (n_id_roles == 0) {

    errors <- c(errors, cli::format_inline("No {.field id} role found in {.field role_map}"))

  } else if (n_id_roles > 1) {

    errors <- c(errors, cli::format_inline("Multiple {.field id} roles in {.field role_map} (exactly one required)"))

  }

  ## ---------------------------------------------------------------------------
  ## Report errors or return
  ## ---------------------------------------------------------------------------

  if (length(errors) > 0) {

    cat(cli::col_red(cli::style_bold("! The horizons_data object failed validation:\n")))

    for (i in seq_along(errors)) {

      branch <- if (i < length(errors)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_red(paste0("   ", branch, " ", errors[i], "\n")))

    }

    cat("\n")
    rlang::abort(
      paste(c("Validation failed:", errors), collapse = "\n"),
      class = "horizons_validation_error"
    )

  }

  x

}


#' Print method for horizons_data objects
#'
#' @description
#' Displays a concise, tree-style summary of a horizons_data object. Shows
#' key dimensions, provenance info, and current pipeline stage at a glance.
#'
#' @details
#' The print output adapts based on what's populated in the object:
#'
#' - **Empty objects**: Shows "(empty)" with a hint to use `spectra()`
#' - **Objects with data**: Shows sample count, predictor count, wavenumber
#'   range, and covariate count if present
#' - **Objects with provenance**: Shows source path and spectra type
#'
#' The output uses tree-style formatting consistent with error messages
#' throughout the package.
#'
#' @param x [horizons_data]. The object to print.
#' @param ... Additional arguments (ignored, for S3 compatibility).
#'
#' @return [horizons_data]. The input object, returned invisibly.
#'
#' @export
print.horizons_data <- function(x, ...) {

  ## ---------------------------------------------------------------------------
  ## Header
  ## ---------------------------------------------------------------------------

  cat(cli::style_bold("\u2500\u2500 horizons_data \u2500\u2500\n\n"))

  ## ---------------------------------------------------------------------------
  ## Check if truly empty (no data AND no provenance)
  ## ---------------------------------------------------------------------------

  has_data       <- !is.null(x$data$analysis)
  has_provenance <- !is.null(x$provenance$spectra_source) ||
                    !is.null(x$provenance$spectra_type)

  if (!has_data && !has_provenance) {

    cat(cli::col_silver("(empty)\n\n"))
    cat(cli::col_silver("Use spectra() to load data.\n"))
    return(invisible(x))

  }

  ## ---------------------------------------------------------------------------
  ## Data section
  ## ---------------------------------------------------------------------------

  if (has_data) {

    cat(cli::style_bold("Data\n"))

    # Samples
    cat(paste0("   \u251C\u2500 Samples: ", x$data$n_rows, "\n"))

    # Predictors with wavenumber range
    if (!is.null(x$data$n_predictors) && x$data$n_predictors > 0) {

      predictor_vars <- x$data$role_map$variable[x$data$role_map$role == "predictor"]
      wn_candidates  <- predictor_vars[grepl("^(wn_)?[0-9.]+$", predictor_vars)]
      wn_values      <- as.numeric(gsub("^wn_", "", wn_candidates))

      has_covars <- !is.null(x$data$n_covariates) && x$data$n_covariates > 0
      branch     <- if (has_covars) "\u251C\u2500" else "\u2514\u2500"

      if (length(wn_values) > 0) {

        wn_range  <- range(wn_values)
        range_str <- paste0(" (", wn_range[2], "\u2013", wn_range[1], " cm\u207B\u00B9)")
        cat(paste0("   ", branch, " Predictors: ", x$data$n_predictors, range_str, "\n"))

      } else {

        cat(paste0("   ", branch, " Predictors: ", x$data$n_predictors, "\n"))

      }
    }

    # Covariates
    if (!is.null(x$data$n_covariates) && x$data$n_covariates > 0) {

      cat(paste0("   \u2514\u2500 Covariates: ", x$data$n_covariates, "\n"))

    }

    cat("\n")

  }

  ## ---------------------------------------------------------------------------
  ## Provenance section
  ## ---------------------------------------------------------------------------

  if (has_provenance) {

    cat(cli::style_bold("Provenance\n"))

    if (!is.null(x$provenance$spectra_source)) {

      has_type <- !is.null(x$provenance$spectra_type)
      branch   <- if (has_type) "\u251C\u2500" else "\u2514\u2500"
      cat(paste0("   ", branch, " Source: ", x$provenance$spectra_source, "\n"))

    }

    if (!is.null(x$provenance$spectra_type)) {

      cat(paste0("   \u2514\u2500 Type: ", x$provenance$spectra_type, "\n"))

    }

    cat("\n")

  }

  ## ---------------------------------------------------------------------------
  ## Status hint
  ## ---------------------------------------------------------------------------

  cat(cli::col_silver("Use summary() for details.\n"))

  invisible(x)

}


#' Summary method for horizons_data objects
#'
#' @description
#' Displays a comprehensive, detailed summary of a horizons_data object. Shows
#' full provenance, data characteristics, configuration status, and pipeline
#' state. Designed for scientists who want to see the complete picture.
#'
#' @details
#' The summary output provides significantly more detail than `print()`:
#'
#' - **Data section**: Sample ID preview, wavenumber range and step size,
#'   covariate names (not just count), outcome variable, memory footprint
#' - **Provenance section**: Full source paths, timestamps, version info,
#'   preprocessing history, aggregation settings
#' - **Configuration section**: Tuning parameter defaults, config count
#' - **Validation section**: Whether validation has run, pass/fail status
#' - **Pipeline status**: Current stage and next step guidance
#'
#' Uses tree-style formatting consistent with the rest of the package.
#'
#' @param object [horizons_data]. The object to summarize.
#' @param ... Additional arguments (ignored, for S3 compatibility).
#'
#' @return [horizons_data]. The input object, returned invisibly.
#'
#' @export
summary.horizons_data <- function(object, ...) {

  x <- object

  ## ---------------------------------------------------------------------------
  ## Header
  ## ---------------------------------------------------------------------------

  header_line <- paste0(rep("\u2500", 79), collapse = "")
  cat(cli::style_bold(paste0("\u2500\u2500 horizons_data summary ",
      paste0(rep("\u2500", 55), collapse = ""), "\n\n")))

  ## ---------------------------------------------------------------------------
  ## Data section
  ## ---------------------------------------------------------------------------

  has_data <- !is.null(x$data$analysis)

  if (has_data) {

    cat(cli::style_bold("Data\n"))

    ## Samples with preview ----

    sample_ids   <- x$data$analysis$sample_id
    n_to_show    <- min(3, length(sample_ids))
    preview      <- paste(sample_ids[1:n_to_show], collapse = ", ")
    if (length(sample_ids) > 3) preview <- paste0(preview, ", ...")

    cat(paste0("   \u251C\u2500 Samples: ", x$data$n_rows, "\n"))
    cat(paste0("   \u2502     \u2514\u2500 First: ", preview, "\n"))

    ## Predictors with range and step ----

    if (!is.null(x$data$n_predictors) && x$data$n_predictors > 0) {

      predictor_vars <- x$data$role_map$variable[x$data$role_map$role == "predictor"]
      wn_candidates  <- predictor_vars[grepl("^(wn_)?[0-9.]+$", predictor_vars)]
      wn_values      <- as.numeric(gsub("^wn_", "", wn_candidates))

      has_covariates <- !is.null(x$data$n_covariates) && x$data$n_covariates > 0
      has_outcome    <- any(x$data$role_map$role == "outcome")
      has_more       <- has_covariates || has_outcome
      branch         <- if (has_more) "\u251C\u2500" else "\u2514\u2500"

      cat(paste0("   ", branch, " Predictors: ", x$data$n_predictors, "\n"))

      if (length(wn_values) > 1) {

        wn_values <- sort(wn_values, decreasing = TRUE)
        wn_range  <- range(wn_values)
        wn_step   <- abs(diff(wn_values[1:2]))

        cat(paste0("   \u2502     \u251C\u2500 Range: ", wn_range[2], "\u2013", wn_range[1], " cm\u207B\u00B9\n"))
        cat(paste0("   \u2502     \u2514\u2500 Step: ", wn_step, " cm\u207B\u00B9\n"))

      }
    }

    ## Covariates with names ----

    if (!is.null(x$data$n_covariates) && x$data$n_covariates > 0) {

      covariate_vars <- x$data$role_map$variable[x$data$role_map$role == "covariate"]
      covar_list     <- paste(covariate_vars, collapse = ", ")
      has_outcome    <- any(x$data$role_map$role == "outcome")
      branch         <- if (has_outcome) "\u251C\u2500" else "\u2514\u2500"

      cat(paste0("   ", branch, " Covariates: ", x$data$n_covariates, "\n"))
      cat(paste0("   \u2502     \u2514\u2500 Names: ", covar_list, "\n"))

    }

    ## Outcome ----

    if (any(x$data$role_map$role == "outcome")) {

      outcome_vars <- x$data$role_map$variable[x$data$role_map$role == "outcome"]
      outcome_list <- paste(outcome_vars, collapse = ", ")

      cat(paste0("   \u251C\u2500 Outcome: ", outcome_list, "\n"))

    }

    ## Memory footprint ----

    mem_bytes <- object.size(x$data$analysis)
    mem_str   <- format(mem_bytes, units = "auto")

    cat(paste0("   \u2514\u2500 Memory: ", mem_str, "\n"))

    cat("\n")

  } else {

    cat(cli::style_bold("Data\n"))
    cat(cli::col_silver("   \u2514\u2500 (no data loaded)\n\n"))

  }

  ## ---------------------------------------------------------------------------
  ## Provenance section
  ## ---------------------------------------------------------------------------

  cat(cli::style_bold("Provenance\n"))

  # Count how many provenance items we have to display
  prov_items <- c(
    !is.null(x$provenance$spectra_source),
    !is.null(x$provenance$spectra_type),
    !is.null(x$provenance$response_source),
    TRUE,  # created (always present)
    TRUE,  # horizons_version (always present)
    TRUE,  # schema_version (always present)
    !is.null(x$provenance$preprocessing),
    !is.null(x$provenance$aggregation_by)
  )
  n_prov <- sum(prov_items)
  prov_idx <- 0

  get_branch <- function() {
    prov_idx <<- prov_idx + 1
    if (prov_idx < n_prov) "\u251C\u2500" else "\u2514\u2500"
  }

  if (!is.null(x$provenance$spectra_source)) {
    cat(paste0("   ", get_branch(), " Spectra source: ", x$provenance$spectra_source, "\n"))
  }

  if (!is.null(x$provenance$spectra_type)) {
    cat(paste0("   ", get_branch(), " Spectra type: ", x$provenance$spectra_type, "\n"))
  }

  if (!is.null(x$provenance$response_source)) {
    cat(paste0("   ", get_branch(), " Response source: ", x$provenance$response_source, "\n"))
  }

  # Created timestamp
  created_str <- format(x$provenance$created, "%Y-%m-%d %H:%M:%S")
  cat(paste0("   ", get_branch(), " Created: ", created_str, "\n"))

  # Horizons version
  cat(paste0("   ", get_branch(), " Horizons version: ", x$provenance$horizons_version, "\n"))

  # Schema version
  cat(paste0("   ", get_branch(), " Schema version: ", x$provenance$schema_version, "\n"))

  if (!is.null(x$provenance$preprocessing)) {
    cat(paste0("   ", get_branch(), " Preprocessing: ", x$provenance$preprocessing, "\n"))
  }

  if (!is.null(x$provenance$aggregation_by)) {
    cat(paste0("   ", get_branch(), " Aggregation: ", x$provenance$aggregation_by, "\n"))
  }

  cat("\n")

  ## ---------------------------------------------------------------------------
  ## Configuration section
  ## ---------------------------------------------------------------------------

  cat(cli::style_bold("Configuration\n"))

  n_configs <- if (!is.null(x$config$n_configs)) x$config$n_configs else 0
  cat(paste0("   \u251C\u2500 Configs defined: ", n_configs, "\n"))

  cat(paste0("   \u2514\u2500 Tuning defaults:\n"))
  cat(paste0("         \u251C\u2500 Grid size: ", x$config$tuning$grid_size, "\n"))
  cat(paste0("         \u251C\u2500 Bayesian iterations: ", x$config$tuning$bayesian_iter, "\n"))
  cat(paste0("         \u2514\u2500 CV folds: ", x$config$tuning$cv_folds, "\n"))

  cat("\n")

  ## ---------------------------------------------------------------------------
  ## Validation section
  ## ---------------------------------------------------------------------------

  cat(cli::style_bold("Validation\n"))

  if (is.null(x$validation$passed)) {

    cat(cli::col_silver(paste0("   \u2514\u2500 Status: not run\n")))

  } else if (x$validation$passed) {

    cat(cli::col_green(paste0("   \u2514\u2500 Status: passed\n")))

  } else {

    cat(cli::col_red(paste0("   \u2514\u2500 Status: failed\n")))

  }

  cat("\n")

  ## ---------------------------------------------------------------------------
  ## Pipeline status
  ## ---------------------------------------------------------------------------

  cat(cli::style_bold("Pipeline Status\n"))

  # Determine next step based on object state
  if (!has_data) {

    next_step <- "spectra()"

  } else if (is.null(x$config$configs) || x$config$n_configs == 0) {

    next_step <- "configure()"

  } else if (is.null(x$evaluation$results)) {

    next_step <- "evaluate()"

  } else if (is.null(x$models$workflows)) {

    next_step <- "finalize()"
  } else {

    next_step <- "predict()"

  }

  cat(paste0("   \u2514\u2500 Next step: ", next_step, "\n"))

  cat("\n")

  ## ---------------------------------------------------------------------------
  ## Footer
  ## ---------------------------------------------------------------------------

  cat(paste0(rep("\u2500", 80), collapse = ""), "\n")

  invisible(x)

}
