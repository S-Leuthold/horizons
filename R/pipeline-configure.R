#' Pipeline: Configure Model Pipelines
#'
#' @description
#' Defines the experimental design space for model evaluation. Takes a
#' `horizons_data` object with response data attached and specifies which
#' combinations of models, transformations, preprocessing, and feature selection
#' to benchmark.
#'
#' @details
#' `configure()` builds a configuration grid — the Cartesian product of all
#' specified modeling axes. Each row represents a distinct pipeline to
#' evaluate. This is the bridge between data preparation (`add_response()`)
#' and model evaluation (`evaluate()`).
#'
#' **Outcome promotion:**
#'
#' `add_response()` assigns `role = "response"` to joined variables.
#' `configure()` promotes exactly one response to `role = "outcome"` for
#' modeling. If only one response exists, it's auto-selected. Multiple
#' responses require explicit `outcome` specification.
#'
#' **Covariate expansion:**
#'
#' When covariates are present, `expand_covariates` controls whether to
#' benchmark across different covariate subsets (power-set expansion) or
#' use all covariates in every configuration.
#'
#' **Reconfiguration:**
#'
#' Can be called multiple times on the same object. Previous configuration
#' is overwritten, and any prior outcome role is reverted to "response".
#' This enables the `purrr::map()` multi-outcome pattern:
#'
#' ```
#' c("SOC", "POM_C", "pH") |>
#'   purrr::map(~base |> configure(outcome = .x) |> evaluate() |> fit())
#' ```
#'
#' @param x [horizons_data]. Object with response data attached via
#'   `add_response()`.
#' @param outcome [character(1) or NULL]. Which response variable to model.
#'   Auto-selects if exactly one response exists.
#' @param models [character]. Model algorithms to benchmark. Default
#'   `c("rf", "cubist", "plsr")`. See `VALID_MODELS` for options.
#' @param transformations [character]. Response transformations to test.
#'   Default `"none"`.
#' @param preprocessing [character]. Per-config spectral preprocessing methods.
#'   Default `"raw"`.
#' @param feature_selection [character]. Feature selection methods to test.
#'   Default `"none"`.
#' @param expand_covariates [logical(1), character, or NULL]. Covariate
#'   expansion strategy. NULL = all covariates in every config (no expansion).
#'   TRUE = power set of all covariate columns. Character vector = power set
#'   of named covariates only. FALSE = exclude all covariates.
#' @param cov_fusion [character(1) or NULL]. Covariate fusion strategy:
#'   NULL (no covariates), `"early"`, or `"late"`.
#' @param cv_folds [integer]. Number of cross-validation folds. Default 5.
#'   Minimum 2.
#' @param grid_size [integer]. Hyperparameter grid size (Latin hypercube).
#'   Default 10. Minimum 1.
#' @param bayesian_iter [integer]. Bayesian optimization iterations.
#'   Default 15. Minimum 0.
#'
#' @return A modified `horizons_data` object with:
#'   * Outcome variable promoted to `role = "outcome"` in `data$role_map`
#'   * `config$configs` — tibble of configuration grid (9 columns)
#'   * `config$n_configs` — integer count
#'   * `config$tuning` — list of tuning parameters
#'   * `config$expansion` — list of original inputs (for reproducibility)
#'   * `config$defaults` — list of method defaults
#'
#' @examples
#' \dontrun{
#' # Minimal (single response auto-selected)
#' hd |> configure()
#'
#' # Explicit outcome with multiple responses
#' hd |> configure(outcome = "SOC")
#'
#' # Full experimental design
#' hd |> configure(
#'   outcome         = "SOC",
#'   models          = c("rf", "cubist", "plsr", "xgboost"),
#'   transformations = c("none", "log"),
#'   preprocessing   = c("raw", "snv", "sg"),
#'   cv_folds        = 10L,
#'   grid_size       = 20L
#' )
#'
#' # Multi-outcome pattern
#' c("SOC", "POM_C", "pH") |>
#'   purrr::map(~base |> configure(outcome = .x) |> evaluate() |> fit())
#' }
#'
#' @export

configure <- function(x,
                      outcome            = NULL,
                      models             = c("rf", "cubist", "plsr"),
                      transformations    = "none",
                      preprocessing      = "raw",
                      feature_selection  = "none",
                      expand_covariates  = NULL,
                      cov_fusion         = NULL,
                      cv_folds           = 5L,
                      grid_size          = 10L,
                      bayesian_iter      = 15L) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Print header
  ## ---------------------------------------------------------------------------

  ## (tree output moved to end of function)

  ## ---------------------------------------------------------------------------
  ## Helper: abort with tree-nested error
  ## ---------------------------------------------------------------------------

  abort_nested <- function(header, details, error_class = "horizons_configure_error") {

    cat(cli::col_red(paste0("\u2502  \u2514\u2500 ", header, "\n")))
    for (i in seq_along(details)) {
      branch <- if (i < length(details)) "\u251C\u2500" else "\u2514\u2500"
      cat(cli::col_red(paste0("\u2502        ", branch, " ", details[i], "\n")))
    }
    cat("\n")
    rlang::abort(
      header,
      class = error_class,
      call  = NULL
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  ## 1.1 x must be horizons_data -----------------------------------------------

  if (!inherits(x, "horizons_data")) {

    cat(cli::col_red(paste0(
      "\u2502  \u2514\u2500 x must be a horizons_data object (got ", class(x)[1], ")\n"
    )))
    cat("\n")
    rlang::abort(
      paste("Expected a `horizons_data` object. Got:", class(x)[1]),
      class = "horizons_configure_error"
    )

  }

  ## 1.2 Response data must exist -----------------------------------------------

  responses <- x$data$role_map$variable[x$data$role_map$role == "response"]
  outcomes  <- x$data$role_map$variable[x$data$role_map$role == "outcome"]
  all_response_vars <- c(responses, outcomes)

  if (length(all_response_vars) == 0) {

    abort_nested(
      "No response data found",
      c("Use `add_response()` to join response variables before configuring")
    )

  }

  ## 1.3 Validate config axis values -------------------------------------------

  bad_models <- setdiff(models, VALID_MODELS)
  if (length(bad_models) > 0) {

    abort_nested(
      "Invalid model(s)",
      c(paste0("Invalid: ", paste(bad_models, collapse = ", ")),
        paste0("Valid options: ", paste(VALID_MODELS, collapse = ", ")))
    )

  }

  bad_transforms <- setdiff(transformations, VALID_TRANSFORMATIONS)
  if (length(bad_transforms) > 0) {

    abort_nested(
      "Invalid transformation(s)",
      c(paste0("Invalid: ", paste(bad_transforms, collapse = ", ")),
        paste0("Valid options: ", paste(VALID_TRANSFORMATIONS, collapse = ", ")))
    )

  }

  bad_preproc <- setdiff(preprocessing, VALID_PREPROCESSING)
  if (length(bad_preproc) > 0) {

    abort_nested(
      "Invalid preprocessing(s)",
      c(paste0("Invalid: ", paste(bad_preproc, collapse = ", ")),
        paste0("Valid options: ", paste(VALID_PREPROCESSING, collapse = ", ")))
    )

  }

  bad_fs <- setdiff(feature_selection, VALID_FEATURE_SELECTION)
  if (length(bad_fs) > 0) {

    abort_nested(
      "Invalid feature selection(s)",
      c(paste0("Invalid: ", paste(bad_fs, collapse = ", ")),
        paste0("Valid options: ", paste(VALID_FEATURE_SELECTION, collapse = ", ")))
    )

  }

  ## 1.4 Validate cov_fusion ---------------------------------------------------

  if (!is.null(cov_fusion)) {

    if (!cov_fusion %in% c("early", "late")) {

      abort_nested(
        paste0("Invalid `cov_fusion` value: '", cov_fusion, "'"),
        c("Use 'early' or 'late'")
      )

    }

  }

  ## 1.5 Validate tuning parameters --------------------------------------------

  if (!is.numeric(cv_folds) || length(cv_folds) != 1 || is.na(cv_folds) ||
      cv_folds != as.integer(cv_folds) || cv_folds < 2) {

    abort_nested(
      "`cv_folds` must be an integer >= 2",
      c(paste0("Got: ", deparse(cv_folds)))
    )

  }

  if (!is.numeric(grid_size) || length(grid_size) != 1 || is.na(grid_size) ||
      grid_size != as.integer(grid_size) || grid_size < 1) {

    abort_nested(
      "`grid_size` must be an integer >= 1",
      c(paste0("Got: ", deparse(grid_size)))
    )

  }

  if (!is.numeric(bayesian_iter) || length(bayesian_iter) != 1 || is.na(bayesian_iter) ||
      bayesian_iter != as.integer(bayesian_iter) || bayesian_iter < 0) {

    abort_nested(
      "`bayesian_iter` must be a non-negative integer",
      c(paste0("Got: ", deparse(bayesian_iter)))
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Resolve and promote outcome
  ## ---------------------------------------------------------------------------

  ## 2.1 Reset any existing outcome to "response" -------------------------------

  if (!is.null(x$config$configs)) {

    warning("Overwriting previous configuration", call. = FALSE)

    ## Clear stale downstream state
    x$validation$passed              <- NULL
    x$validation$checks              <- NULL
    x$validation$timestamp           <- NULL
    x$validation$outliers$spectral_ids   <- NULL
    x$validation$outliers$response_ids   <- NULL
    x$validation$outliers$removed_ids    <- NULL
    x$validation$outliers$removal_detail <- NULL
    x$validation$outliers$removed        <- FALSE
    x$evaluation$results     <- NULL
    x$evaluation$best_config <- NULL
    x$models$workflows       <- NULL
    x$models$n_models        <- NULL
    x$ensemble$stack         <- NULL
    x$ensemble$metrics       <- NULL

  }

  x$data$role_map$role[x$data$role_map$role == "outcome"] <- "response"
  responses <- x$data$role_map$variable[x$data$role_map$role == "response"]

  ## 2.2 Resolve outcome -------------------------------------------------------

  if (is.null(outcome)) {

    if (length(responses) == 1) {

      outcome_var <- responses

    } else {

      abort_nested(
        "Multiple response variables found",
        c(paste0("Available: ", paste(responses, collapse = ", ")),
          "Specify `outcome` to select one")
      )

    }

  } else {

    if (!outcome %in% responses) {

      abort_nested(
        paste0("Response variable '", outcome, "' not found"),
        c(paste0("Available: ", paste(responses, collapse = ", ")))
      )

    }

    outcome_var <- outcome

  }

  ## 2.3 Promote to outcome role -----------------------------------------------

  x$data$role_map$role[x$data$role_map$variable == outcome_var] <- "outcome"

  ## ---------------------------------------------------------------------------
  ## Step 3: Handle covariates
  ## ---------------------------------------------------------------------------

  covariate_cols <- x$data$role_map$variable[x$data$role_map$role == "covariate"]

  if (length(covariate_cols) == 0) {

    ## No covariates in object -------------------------------------------------

    if (!is.null(cov_fusion)) {

      warning("cov_fusion ignored: no covariates in object", call. = FALSE)
      cov_fusion <- NULL

    }

    if (!is.null(expand_covariates)) {

      warning("expand_covariates ignored: no covariates in object", call. = FALSE)
      expand_covariates <- NULL

    }

    covariate_sets <- NA_character_

  } else {

    ## Covariates exist --------------------------------------------------------

    if (is.null(cov_fusion)) {

      abort_nested(
        "Covariates detected but no fusion strategy specified",
        c(paste0("Covariates: ", paste(covariate_cols, collapse = ", ")),
          "Use `cov_fusion = 'early'` or `cov_fusion = 'late'`")
      )

    }

    ## Generate covariate sets based on expand_covariates ----------------------

    if (is.null(expand_covariates)) {

      ## NULL: all covariates in every config
      covariate_sets <- paste(sort(covariate_cols), collapse = ",")

    } else if (is.logical(expand_covariates) && isTRUE(expand_covariates)) {

      ## TRUE: power set of all covariate columns
      covariate_sets <- generate_power_set(covariate_cols)

    } else if (is.logical(expand_covariates) && isFALSE(expand_covariates)) {

      ## FALSE: exclude all covariates
      covariate_sets <- NA_character_

    } else if (is.character(expand_covariates)) {

      ## Character vector: selective expansion
      bad_covs <- setdiff(expand_covariates, covariate_cols)

      if (length(bad_covs) > 0) {

        abort_nested(
          "Covariate(s) not found",
          c(paste0("Not found: ", paste(bad_covs, collapse = ", ")),
            paste0("Available: ", paste(covariate_cols, collapse = ", ")))
        )

      }

      fixed_covs    <- setdiff(covariate_cols, expand_covariates)
      expanded_sets <- generate_power_set(expand_covariates)

      ## Merge fixed covariates into each expanded set
      covariate_sets <- vapply(expanded_sets, function(set) {

        if (is.na(set)) {

          ## "none" from power set — still include fixed covariates
          if (length(fixed_covs) > 0) {
            paste(sort(fixed_covs), collapse = ",")
          } else {
            NA_character_
          }

        } else {

          all_covs <- sort(unique(c(strsplit(set, ",")[[1]], fixed_covs)))
          paste(all_covs, collapse = ",")

        }

      }, character(1))

      ## Deduplicate (fixed-only set may appear twice)
      covariate_sets <- unique(covariate_sets)

    } else {

      abort_nested(
        "Invalid `expand_covariates` value",
        c("Must be NULL, TRUE, FALSE, or a character vector of covariate names")
      )

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Build configuration grid
  ## ---------------------------------------------------------------------------

  config_grid <- tidyr::crossing(
    model             = models,
    transformation    = transformations,
    preprocessing     = preprocessing,
    feature_selection = feature_selection,
    covariates        = covariate_sets
  )

  ## Add list-columns for parameter overrides ----------------------------------

  config_grid$preprocessing_params <- vector("list", nrow(config_grid))
  config_grid$feature_params       <- vector("list", nrow(config_grid))
  config_grid$transform_params     <- vector("list", nrow(config_grid))

  ## ---------------------------------------------------------------------------
  ## Step 5: Generate config IDs
  ## ---------------------------------------------------------------------------

  config_grid$config_id <- mapply(
    generate_config_id,
    model             = config_grid$model,
    preprocessing     = config_grid$preprocessing,
    transformation    = config_grid$transformation,
    feature_selection = config_grid$feature_selection,
    covariates        = config_grid$covariates,
    USE.NAMES         = FALSE
  )

  ## Reorder: config_id first --------------------------------------------------

  config_grid <- config_grid[, c(
    "config_id", "model", "transformation", "preprocessing",
    "feature_selection", "covariates",
    "preprocessing_params", "feature_params", "transform_params"
  )]

  ## Deduplicate (defensive) ---------------------------------------------------

  config_grid <- config_grid[!duplicated(config_grid$config_id), ]

  ## Coerce to tibble ----------------------------------------------------------

  config_grid <- tibble::as_tibble(config_grid)

  ## ---------------------------------------------------------------------------
  ## Step 6: Store configuration
  ## ---------------------------------------------------------------------------

  x$config$configs   <- config_grid
  x$config$n_configs <- nrow(config_grid)

  x$config$tuning <- list(
    cv_folds      = as.integer(cv_folds),
    grid_size     = as.integer(grid_size),
    bayesian_iter = as.integer(bayesian_iter)
  )

  x$config$expansion <- list(
    outcome           = outcome_var,
    models            = models,
    transformations   = transformations,
    preprocessing     = preprocessing,
    feature_selection = feature_selection,
    expand_covariates = expand_covariates,
    cov_fusion        = cov_fusion
  )

  x$config$defaults <- list(
    preprocessing_params = list(sg_window = 11L, sg_order = 2L),
    feature_params       = list(pca_threshold = 0.99, correlation_n = 200L),
    transform_params     = list()
  )

  ## ---------------------------------------------------------------------------
  ## Step 7: CLI output
  ## ---------------------------------------------------------------------------

  cat(paste0("\u251C\u2500 ", cli::style_bold("Configuring pipelines"), "...\n"))
  cat(paste0("\u2502  \u251C\u2500 Outcome: ", outcome_var, "\n"))
  cat(paste0("\u2502  \u251C\u2500 Models: ", paste(models, collapse = ", "), "\n"))
  cat(paste0("\u2502  \u251C\u2500 Tuning: ", cv_folds, "-fold CV, grid = ",
             grid_size, "\n"))

  if (length(covariate_cols) > 0) {

    cat(paste0("\u2502  \u251C\u2500 Covariates: ",
               paste(covariate_cols, collapse = ", "), "\n"))

  }

  cat(paste0("\u2502  \u2514\u2500 Configs: ", nrow(config_grid), " total\n"))
  cat("\u2502\n")

  ## ---------------------------------------------------------------------------
  ## Step 8: Return
  ## ---------------------------------------------------------------------------

  x

}


## =============================================================================
## Internal helpers
## =============================================================================

#' Generate a deterministic config ID
#'
#' @param model Character. Model short name.
#' @param preprocessing Character. Preprocessing method.
#' @param transformation Character. Response transformation.
#' @param feature_selection Character. Feature selection method.
#' @param covariates Character. Canonicalized covariate string or NA.
#'
#' @return Character. Config ID in format `{model}_{preprocessing}_{transformation}_{feature_selection}_{6-char hash}`.
#' @keywords internal

generate_config_id <- function(model, preprocessing, transformation,
                               feature_selection, covariates) {

  base <- paste(model, preprocessing, transformation,
                feature_selection, sep = "_")

  hash_input <- list(
    model             = model,
    preprocessing     = preprocessing,
    transformation    = transformation,
    feature_selection = feature_selection,
    covariates        = covariates
  )

  hash <- substr(digest::digest(hash_input), 1, 6)
  paste(base, hash, sep = "_")

}


#' Generate power set of covariate combinations
#'
#' @param covariates Character vector. Covariate column names.
#'
#' @return Character vector. Each element is a comma-separated canonicalized
#'   string of covariate names, or `NA_character_` for the empty set.
#' @keywords internal

generate_power_set <- function(covariates) {

  covariates <- sort(covariates)
  n          <- length(covariates)
  sets       <- list(NA_character_)

  for (k in seq_len(n)) {

    combos <- utils::combn(covariates, k, simplify = FALSE)
    sets   <- c(sets, lapply(combos, function(x) paste(x, collapse = ",")))

  }

  unlist(sets)

}
