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
#' is overwritten, any prior outcome role is reverted to "response", and
#' everything a previous `evaluate()`, `fit()` or `ensemble()` earned is
#' dropped: the `evaluation`, `models` and `ensemble` slots return to their
#' empty state, and the `horizons_eval`, `horizons_fit` or
#' `horizons_ensemble` class goes with them. All of it is keyed to the
#' outcome that is being replaced. The validation verdict is cleared too,
#' but the record of outliers `validate()` already removed is kept, since
#' those rows stay removed, and so is a `select_training()` record, which
#' describes the rows rather than the outcome. Because neither was chosen for
#' the new outcome, `configure()` warns when rows were removed as response
#' outliers of a different outcome, and when a `select_training()` record
#' (other than `scope = "global"`) was drawn for properties that do not
#' include it.
#' This enables the `purrr::map()` multi-outcome pattern:
#'
#' ```
#' c("SOC", "POM_C", "pH") |>
#'   purrr::map(~base |> configure(outcome = .x) |> evaluate() |> fit())
#' ```
#'
#' **Reproducibility:**
#'
#' `evaluate()` and `fit()` pin the RNG seed and kind, so a run is
#' repeatable from its recorded `seed` for every model except `cubist`.
#' The Cubist C implementation is not bit-reproducible when
#' `committees > 1`: repeated fits on identical data under an identical
#' `set.seed()`, or under an explicit `cubistControl(seed = )`, can differ in
#' the fourth significant figure of a fold metric (measured on Cubist
#' 0.6.0). `committees = 1` is stable. The effect is small, but it can flip
#' `select_best()` between near-tied grid points and move a reported test
#' metric by a few percent, so a `cubist` result should be read as
#' reproducible to that tolerance rather than exactly. `rf` and the other
#' engines are deterministic given `seed`. See GitHub issue #51.
#'
#' **Choosing models for large spectral libraries:**
#'
#' Cost scales very differently with predictor count (`p`) across
#' `MODEL_SPECS`. `rf` (ranger), `xgboost`, `lightgbm` and `elastic_net`
#' (glmnet) stay fast at full spectral resolution; `plsr` is built for
#' `p >> n` and is not a concern either. `cubist` fits a linear model in
#' every rule, so its cost grows sharply with `p`: on the KSSL clay library
#' at 4 cm-1 (14,228 rows x 851 predictors, ~12.1M cells), not one of 25
#' tune tasks finished in 29.5 minutes, while the same config with
#' `feature_selection = "pca"` finished in 368 s (test RPD 3.82) — OSSL's
#' published pipeline is SNV -> PCA(120) -> Cubist (Safanelli et al. 2025,
#' PLOS ONE 20(1):e0296545). `validate()`'s P010 check warns when a
#' `cubist` config with `feature_selection = "none"` sits on a table whose
#' `n_rows * n_predictors` exceeds `CUBIST_MAX_CELLS` (a conservative floor
#' well under the measured failure point, not a benchmark); the fix is
#' `feature_selection = "pca"`. `svm_rbf` (kernlab) and `mars` (earth) also
#' scale with `p` on well-established grounds — an RBF kernel matrix costs
#' `O(n^2 p)` to build, and MARS's forward pass searches every predictor at
#' every candidate knot — but neither has been measured at library scale,
#' and `validate()` does not warn for them. A slow run with either on a
#' full-resolution table is a signal to add `feature_selection` there too.
#' See GitHub issue #40.
#'
#' **Recipe settings:**
#'
#' `sg_window` and `pca_threshold` are set once per object, not per
#' configuration: every configuration's recipe uses the same value, and
#' neither is part of the config id. `sg_window` is the Savitzky-Golay window
#' in grid points. It trims `(sg_window - 1) / 2` columns from each end of the
#' spectrum for every preprocessing method, `"raw"` and `"snv"` included, so
#' it matters even where no filter runs. The polynomial order is not a
#' setting: each method fixes its own (`"sg"` order 1; `"deriv1"` first
#' derivative, order 1; `"deriv2"` second derivative, order 3; the `"snv_"`
#' variants the same), and the cubic is why the window must be at least 5.
#' Because the window counts grid points, its physical width depends on the
#' axis the object was standardized to; `configure()` records it in cm-1 as
#' `config$recipe$sg_window_cm`. `pca_threshold` is the share of variance
#' kept by `feature_selection = "pca"` and is read by nothing else. Both
#' defaults are the values the recipe ran before they were settable. See
#' GitHub issue #62.
#'
#' @param x `horizons_data`. Object with response data attached via
#'   `add_response()`.
#' @param outcome `character(1) or NULL`. Which response variable to model.
#'   Auto-selects if exactly one response exists.
#' @param models `character`. Model algorithms to benchmark. Default
#'   `c("rf", "cubist", "plsr")`. See `VALID_MODELS` for options.
#' @param transformations `character`. Response transformations to test.
#'   Default `"none"`.
#' @param preprocessing `character`. Per-config spectral preprocessing methods.
#'   Default `"raw"`.
#' @param feature_selection `character`. Feature selection methods to test.
#'   Default `"none"`.
#' @param expand_covariates `logical(1), character, or NULL`. Covariate
#'   expansion strategy. NULL = all covariates in every config (no expansion).
#'   TRUE = power set of all covariate columns. Character vector = power set
#'   of named covariates only. FALSE = exclude all covariates.
#' @param cov_fusion `character(1) or NULL`. Covariate fusion strategy:
#'   NULL (no covariates) or `"early"`, which adds the covariates as
#'   predictors beside the spectral features. Required when covariates are
#'   present. `"late"` aborts: late fusion is designed but not built.
#' @param cv_folds `integer`. Number of cross-validation folds. Default 5.
#'   Minimum 2.
#' @param grid_size `integer`. Hyperparameter grid size (Latin hypercube).
#'   Default 10. Minimum 1.
#' @param bayesian_iter `integer`. Bayesian optimization iterations.
#'   Default 15. Minimum 0.
#' @param final_bayesian_iter `integer`. Bayesian optimization iterations for
#'   the final fit on the selected configuration. Default 25. Minimum 0.
#' @param sg_window `integer`. Savitzky-Golay window in grid points, applied
#'   to every configuration. Default 9. Must be odd and at least 5. See
#'   Recipe settings.
#' @param pca_threshold `numeric`. Share of variance `feature_selection =
#'   "pca"` keeps, applied to every configuration that uses it. Default
#'   0.995. Must be in (0, 1].
#'
#' @return A modified `horizons_data` object with:
#'   * Outcome variable promoted to `role = "outcome"` in `data$role_map`
#'   * `config$configs` — tibble of configuration grid (6 columns)
#'   * `config$n_configs` — integer count
#'   * `config$tuning` — list of tuning parameters
#'   * `config$expansion` — list of original inputs (for reproducibility)
#'   * `config$recipe` — the recipe settings every configuration is built
#'     with: `sg_window`, `sg_window_cm` (the window's width in cm-1,
#'     `sg_window` times the median spacing of the predictor axis, `NA` when
#'     the axis is not numeric) and `pca_threshold`
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
#' # A wider Savitzky-Golay window and a tighter PCA for every config
#' hd |> configure(
#'   preprocessing     = c("deriv1", "snv_deriv2"),
#'   feature_selection = "pca",
#'   sg_window         = 15L,
#'   pca_threshold     = 0.99
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
                      cv_folds              = 5L,
                      grid_size             = 10L,
                      bayesian_iter         = 15L,
                      final_bayesian_iter   = 25L,
                      sg_window             = 9L,
                      pca_threshold         = 0.995) {

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

    if (!is.character(cov_fusion) || length(cov_fusion) != 1 || is.na(cov_fusion)) {

      abort_nested(
        "`cov_fusion` must be NULL or a single string",
        c(paste0("Got: ", paste(deparse(cov_fusion), collapse = " ")),
          "Use 'early'")
      )

    }

    ## Late fusion is designed (two models per config, the second fitted to
    ## the first's residuals) but not built; build_recipe() only fuses early.
    ## Accepting "late" would run early fusion under the other name.

    if (cov_fusion == "late") {

      abort_nested(
        "Late covariate fusion (`cov_fusion = 'late'`) is not built",
        c("Only early fusion is implemented: covariates join the spectral features as predictors",
          "Use `cov_fusion = 'early'`"),
        error_class = c("horizons_configure_error", "horizons_input_error")
      )

    }

    if (cov_fusion != "early") {

      abort_nested(
        paste0("Invalid `cov_fusion` value: '", cov_fusion, "'"),
        c("Use 'early'")
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

  if (!is.numeric(final_bayesian_iter) || length(final_bayesian_iter) != 1 ||
      is.na(final_bayesian_iter) ||
      final_bayesian_iter != as.integer(final_bayesian_iter) ||
      final_bayesian_iter < 0) {

    abort_nested(
      "`final_bayesian_iter` must be a non-negative integer",
      c(paste0("Got: ", deparse(final_bayesian_iter)))
    )

  }

  ## 1.6 Validate recipe settings ----------------------------------------------

  ## The window is centred on a point, so it is odd. Its floor is 5 because
  ## deriv2 and snv_deriv2 fit a cubic, and prospectr::savitzkyGolay() needs
  ## the window wider than the polynomial order; the setting is object-level,
  ## so it has to suit every method a grid might hold.

  if (!is.numeric(sg_window) || length(sg_window) != 1 || !is.finite(sg_window) ||
      sg_window != round(sg_window) || sg_window < 5 || sg_window %% 2 != 1) {

    abort_nested(
      "`sg_window` must be an odd integer >= 5",
      c(paste0("Got: ", paste(deparse(sg_window), collapse = " ")),
        "The window is centred on a point, so its width is odd",
        "The second-derivative methods fit a cubic, which needs at least 5 points")
    )

  }

  if (!is.numeric(pca_threshold) || length(pca_threshold) != 1 ||
      is.na(pca_threshold) || pca_threshold <= 0 || pca_threshold > 1) {

    abort_nested(
      "`pca_threshold` must be a single number in (0, 1]",
      c(paste0("Got: ", paste(deparse(pca_threshold), collapse = " ")),
        "It is the share of variance feature_selection = 'pca' keeps")
    )

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Resolve and promote outcome
  ## ---------------------------------------------------------------------------

  ## 2.1 Reset any existing outcome to "response" -------------------------------

  if (!is.null(x$config$configs)) {

    warning("Overwriting previous configuration", call. = FALSE)

    ## Promotion is earned, and reconfiguring un-earns it. The split, the row
    ## index and the cached predictions are all keyed to an outcome that is
    ## about to change, and the class is the claim that they are there, so
    ## the object goes back to being a plain horizons_data. The validation
    ## verdict goes too; the record of rows already removed, and the
    ## select_training() record, describe rows and are kept.

    x <- reset_promotion(x)

  }

  role_map <- x$data$role_map
  role_map$role[role_map$role == "outcome"] <- "response"
  responses <- role_map$variable[role_map$role == "response"]

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

  role_map$role[role_map$variable == outcome_var] <- "outcome"

  ## Demoting the old outcome and promoting the new one changes the response
  ## count, so the roles go back through set_analysis() rather than being
  ## written in place. Otherwise the stored n_responses disagrees with the
  ## role map and validate_horizons_data() aborts on the next verb.

  x <- set_analysis(x, x$data$analysis, role_map)

  ## 2.4 Name what the rows carry from an earlier outcome ----------------------

  ## Both describe rows, so both survive a re-configure; neither was chosen
  ## with this outcome in mind. Warn rather than abort: the object is usable,
  ## and starting again from an earlier object is the user's call.

  warn_stale_removals(x, outcome_var)
  warn_selection_properties(x, outcome_var)

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
          "Use `cov_fusion = 'early'`")
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
    "feature_selection", "covariates"
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
    cv_folds            = as.integer(cv_folds),
    grid_size           = as.integer(grid_size),
    bayesian_iter       = as.integer(bayesian_iter),
    final_bayesian_iter = as.integer(final_bayesian_iter)
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

  ## One value of each for the whole object, read by build_recipe() for every
  ## config through recipe_settings(). The window counts grid points, so its
  ## physical width is recorded beside it, as select_training() records its
  ## similarity-space window.

  x$config$recipe <- list(
    sg_window     = as.integer(sg_window),
    sg_window_cm  = as.integer(sg_window) * axis_spacing_cm(x),
    pca_threshold = as.numeric(pca_threshold)
  )

  ## Objects configured before #62 carry `config$defaults`, a record of
  ## settings the recipe never ran. Re-configuring one drops it.

  x$config$defaults <- NULL

  ## ---------------------------------------------------------------------------
  ## Step 7: CLI output
  ## ---------------------------------------------------------------------------

  cat(paste0("\u251C\u2500 ", cli::style_bold("Configuring pipelines"), "...\n"))
  cat(paste0("\u2502  \u251C\u2500 Outcome: ", outcome_var, "\n"))
  cat(paste0("\u2502  \u251C\u2500 Models: ", paste(models, collapse = ", "), "\n"))
  cat(paste0("\u2502  \u251C\u2500 Tuning: ", cv_folds, "-fold CV, grid = ",
             grid_size, "\n"))

  window_cm <- x$config$recipe$sg_window_cm

  cat(paste0("\u2502  \u251C\u2500 Recipe: SG window ", x$config$recipe$sg_window,
             if (!is.na(window_cm)) paste0(" (", signif(window_cm, 3), " cm\u207B\u00B9)"),
             if ("pca" %in% feature_selection) paste0(", PCA threshold ", pca_threshold),
             "\n"))

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


#' Spacing of an object's wavenumber axis, in cm-1
#'
#' @description
#' The median absolute spacing of the predictor columns' wavenumbers, read
#' from their names (`wn_<wavenumber>`, or a bare number). That is the axis
#' the recipe's Savitzky-Golay window slides along, so it is what converts the
#' window from grid points to cm-1. The grid step `standardize()` recorded is
#' the fallback, used only when the names do not parse.
#'
#' The axis comes first because the recorded step can describe a different
#' one. `select_training()` resamples the pool onto the targets' grid but
#' returns the pool's `provenance$standardization`, so a library standardized
#' at 2 cm-1 and drawn around a batch at 4 cm-1 still records a step of 2.
#'
#' @param x `horizons_data`. The object being configured.
#'
#' @return `numeric(1)`. The spacing in cm-1, or `NA` when neither the names
#'   nor the provenance give one.
#' @noRd

axis_spacing_cm <- function(x) {

  role_map   <- x$data$role_map
  predictors <- role_map$variable[role_map$role == "predictor"]
  wn         <- suppressWarnings(as.numeric(sub("^wn_", "", predictors)))

  if (length(wn) > 1 && !anyNA(wn)) {

    return(stats::median(abs(diff(wn))))

  }

  step <- x$provenance$standardization$grid$step

  if (is.numeric(step) && length(step) == 1 && !is.na(step)) {

    return(step)

  }

  NA_real_

}


#' Warn when rows were removed as response outliers of another outcome
#'
#' @description
#' `validate(remove_outliers = )` records, per removed row, the outcome whose
#' Tukey fences flagged it. Those rows stay removed across a re-configure, so
#' an outcome configured afterwards is modelled without rows that were judged
#' against a different variable. Only `reason == "response"` rows count:
#' spectral removals do not depend on the outcome, and a `"both"` row would
#' have been removed as a spectral outlier anyway. Rows recorded before the
#' `outcome` column existed cannot be attributed and are not counted either.
#'
#' @param x `horizons_data`. The object being configured.
#' @param outcome_var `character(1)`. The outcome being configured.
#'
#' @return `NULL`, invisibly. Called for its warning.
#' @noRd

warn_stale_removals <- function(x, outcome_var) {

  detail <- x$validation$outliers$removal_detail

  if (is.null(detail) || !"outcome" %in% names(detail)) {

    return(invisible(NULL))

  }

  stale <- detail$outcome[detail$reason %in% "response" &
                          !is.na(detail$outcome) &
                          detail$outcome != outcome_var]

  if (length(stale) == 0) {

    return(invisible(NULL))

  }

  counts <- table(stale)

  warning(paste0(
    length(stale), " row(s) were removed by validate() as response outliers of ",
    paste0("'", names(counts), "' (", as.integer(counts), ")", collapse = ", "),
    " and stay removed while modelling '", outcome_var, "'. ",
    "Start from the object before validate() to model them."
  ), call. = FALSE)

  invisible(NULL)

}


#' Warn when the training rows were drawn for other properties
#'
#' @description
#' `select_training()` draws each target's `k` nearest pool rows per
#' property, among the rows that have that property measured. An outcome
#' outside `settings$properties` inherits rows drawn for something else, so
#' the per-target guarantee does not hold for it. `scope = "global"` draws
#' the whole pool, so there is no guarantee to lose.
#'
#' @param x `horizons_data`. The object being configured.
#' @param outcome_var `character(1)`. The outcome being configured.
#'
#' @return `NULL`, invisibly. Called for its warning.
#' @noRd

warn_selection_properties <- function(x, outcome_var) {

  settings <- x$selection$settings

  if (is.null(settings$properties) || identical(settings$scope, "global") ||
      outcome_var %in% settings$properties) {

    return(invisible(NULL))

  }

  warning(paste0(
    "The training rows were drawn by select_training() for ",
    paste0("'", settings$properties, "'", collapse = ", "), "; '",
    outcome_var, "' is not one of them, so each target's k nearest rows ",
    "were not drawn for it."
  ), call. = FALSE)

  invisible(NULL)

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
