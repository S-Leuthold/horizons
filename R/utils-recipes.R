#' Build a Tidymodels Recipe from a Config Row
#'
#' @description
#' Translates a single config row into a tidymodels recipe. Uses `role_map`
#' to identify columns by role — no hard-coded wavenumber ranges. This is
#' the recipe constructor for `evaluate_single_config()`.
#'
#' Recipe step order (early fusion):
#' 1. Response transformation (step_log/step_sqrt with skip = TRUE)
#' 2. Spectral preprocessing (step_transform_spectra on predictor_cols only)
#' 3. Feature selection (pca/boruta/cars/correlation/none)
#' 4. Covariate inclusion (append requested covariates, remove others)
#'
#' A final pass re-points the step selector quosures away from this function's
#' frame, which would otherwise be serialized to every parallel worker. See
#' `strip_selector_envs()`.
#'
#' @param config_row Single-row tibble from `config$configs`.
#' @param train_data Data frame. Training split containing all columns.
#' @param role_map Tibble with `variable` and `role` columns from the
#'   horizons_data object.
#'
#' @return A `recipes::recipe` object ready for `workflows::add_recipe()`.
#' @keywords internal
#' @export
build_recipe <- function(config_row, train_data, role_map) {

  ## Extract columns by role ------------------------------------------------

  outcome_col    <- role_map$variable[role_map$role == "outcome"]
  predictor_cols <- role_map$variable[role_map$role == "predictor"]
  id_col         <- role_map$variable[role_map$role == "id"]
  meta_cols      <- role_map$variable[role_map$role == "meta"]

  ## Covariate columns: all covariates available in the data
  all_covariate_cols <- role_map$variable[role_map$role == "covariate"]

  ## Config-specific covariates: which ones this config wants
  config_covariates <- parse_config_covariates(config_row$covariates)

  ## -----------------------------------------------------------------------
  ## Column ordering invariant
  ## -----------------------------------------------------------------------
  ## Spectral data must be in monotonic order (decreasing wavenumber) for
  ## Savitzky-Golay windows. Check BEFORE passing to step_transform_spectra.

  predictor_nums <- suppressWarnings(
    as.numeric(gsub("[^0-9.\\-]", "", predictor_cols))
  )

  if (!any(is.na(predictor_nums)) && length(predictor_nums) > 1) {

    diffs <- diff(predictor_nums)

    if (!all(diffs < 0) && !all(diffs > 0)) {

      rlang::abort(
        "Predictor columns are not monotonically ordered. Spectral preprocessing requires ordered wavelengths."
      )

    }

  }

  ## -----------------------------------------------------------------------
  ## Step 1: Initialize recipe with roles
  ## -----------------------------------------------------------------------

  ## Build formula: outcome ~ everything
  rec_formula <- stats::as.formula(paste(outcome_col, "~ ."))

  rec <- recipes::recipe(rec_formula, data = train_data)

  ## Assign roles -----------------------------------------------------------

  rec <- recipes::update_role(rec, dplyr::all_of(id_col), new_role = "id")
  rec <- recipes::update_role_requirements(rec, role = "id", bake = TRUE)

  if (length(meta_cols) > 0) {

    rec <- recipes::update_role(rec, dplyr::all_of(meta_cols), new_role = "meta")
    rec <- recipes::update_role_requirements(rec, role = "meta", bake = TRUE)

  }

  ## Mark covariates with a non-predictor role initially
  ## They'll be added back as predictors in Step 4 if this config uses them
  if (length(all_covariate_cols) > 0) {

    rec <- recipes::update_role(rec, dplyr::all_of(all_covariate_cols),
                                new_role = "covariate_hold")

  }

  ## -----------------------------------------------------------------------
  ## Step 2: Response transformation
  ## -----------------------------------------------------------------------

  transformation <- tolower(as.character(config_row$transformation))

  if (transformation == "log") {

    rec <- rec |>
      recipes::step_log(recipes::all_outcomes(), offset = 1, skip = TRUE)

  } else if (transformation == "log10") {

    rec <- rec |>
      recipes::step_log(recipes::all_outcomes(), offset = 1, base = 10, skip = TRUE)

  } else if (transformation == "sqrt") {

    rec <- rec |>
      recipes::step_sqrt(recipes::all_outcomes(), skip = TRUE)

  }

  ## "none" → no step added

  ## -----------------------------------------------------------------------
  ## Step 3: Spectral preprocessing
  ## -----------------------------------------------------------------------
  ## Targets predictor_cols by name (NOT all_predictors()), so covariates
  ## in covariate_hold role are never touched by spectral operations.

  preprocessing <- tolower(as.character(config_row$preprocessing))

  rec <- rec |>
    step_transform_spectra(
      dplyr::all_of(predictor_cols),
      preprocessing = preprocessing
    )

  ## -----------------------------------------------------------------------
  ## Step 4: Feature selection
  ## -----------------------------------------------------------------------
  ## Operates on spectral features only. Covariates bypass this step.

  feature_selection <- tolower(as.character(config_row$feature_selection))

  rec <- switch(feature_selection,

    "none" = rec,

    "pca" = rec |>
      recipes::step_pca(
        recipes::all_predictors(),
        threshold = 0.995,
        options   = list(scale. = TRUE, center = TRUE)
      ),

    "correlation" = rec |>
      step_select_correlation(
        recipes::all_predictors(),
        outcome = outcome_col
      ),

    "boruta" = rec |>
      step_select_boruta(
        recipes::all_predictors(),
        outcome = outcome_col
      ),

    "cars" = rec |>
      step_select_cars(
        recipes::all_predictors(),
        outcome = outcome_col
      ),

    rlang::abort(paste0(
      "Unsupported feature selection method: '", feature_selection, "'. ",
      "Valid methods: ", paste(VALID_FEATURE_SELECTION, collapse = ", ")
    ))

  )

  ## -----------------------------------------------------------------------
  ## Step 5: Covariate inclusion
  ## -----------------------------------------------------------------------
  ## Covariates are already in the data (from add_covariates()). Per-config
  ## handling decides which ones to include as predictors.

  if (length(all_covariate_cols) > 0) {

    if (!is.null(config_covariates) && length(config_covariates) > 0) {

      ## Validate requested covariates exist in the data
      missing_covs <- setdiff(config_covariates, all_covariate_cols)

      if (length(missing_covs) > 0) {

        rlang::abort(paste0(
          "Config requests covariates not available in data: ",
          paste(missing_covs, collapse = ", "),
          ". Available: ", paste(all_covariate_cols, collapse = ", ")
        ))

      }

      ## Promote requested covariates to predictor role
      rec <- recipes::update_role(rec, dplyr::all_of(config_covariates),
                                  new_role = "predictor")

      ## Remove unrequested covariates
      unused_covariates <- setdiff(all_covariate_cols, config_covariates)

      if (length(unused_covariates) > 0) {

        rec <- rec |>
          recipes::step_rm(dplyr::all_of(unused_covariates))

      }

    } else {

      ## No covariates for this config — remove all
      rec <- rec |>
        recipes::step_rm(dplyr::all_of(all_covariate_cols))

    }

  }

  ## -----------------------------------------------------------------------
  ## Step 6: Drop the heavy environment captured by the step selectors
  ## -----------------------------------------------------------------------
  ## This frame holds `train_data` and the recipe under construction, both of
  ## which every step's selector quosure would otherwise carry to a parallel
  ## worker. Passing environment() explicitly keeps that dependency visible.

  strip_selector_envs(rec, environment())

}


#' Re-point Recipe Selector Quosures at a Minimal Environment
#'
#' @description
#' `recipes` step constructors capture their calling frame via
#' `rlang::enquos()`. `build_recipe()`'s frame holds both `train_data` and the
#' recipe under construction, so every step retains references to the training
#' table.
#'
#' Those references cost nothing in memory — R shares the underlying object —
#' but R's serializer does not deduplicate data frames, so each one becomes a
#' full copy whenever the recipe crosses to a parallel worker. Measured on the
#' KSSL clay training split (14,228 x 1,701), an unstripped recipe serialized to
#' **5.26x** its training data; stripped, **1.07x** (the residual is
#' `rec$template`, which is inherent to `recipes`). Neither `object.size()` nor
#' `lobstr::obj_size()` shows the problem, because the duplication exists only
#' at serialization time.
#'
#' Two design points, both learned from review:
#'
#' The bindings are **derived from the selectors**, not enumerated by hand. An
#' earlier version listed `build_recipe()`'s locals, which meant a future step
#' referencing a new local would produce a quosure pointing at a name absent
#' from the replacement environment. Harvesting `all.vars()` off the quosures
#' themselves removes that failure mode rather than documenting it.
#'
#' Every slot is walked recursively rather than the `terms`/`columns` pair.
#' Stock steps keep selectors in `terms` and this package's custom steps keep
#' them in `columns`, but others use `impute_with`, `denom`, `inputs`,
#' `outcome`, or `lon`/`lat`, and a slot holding a *single* quosure is not a
#' list. Both would have been silently missed, restoring the leak.
#'
#' Residual caveat: the replacement environment is parented on the `horizons`
#' namespace, because bare `recipes::all_predictors()` / `all_outcomes()` are
#' imported there (see `R/zzz.R`) and a `dplyr` parent does not reach them.
#' That chain still ends at the global environment, so a selector referencing a
#' name this function failed to harvest could in principle resolve against a
#' same-named global rather than erroring. Deriving the bindings is what makes
#' that unreachable in practice.
#'
#' @param rec A `recipes::recipe` object.
#' @param frame The environment the step selectors were created in — normally
#'   `build_recipe()`'s frame, passed explicitly rather than via
#'   `parent.frame()` so the dependency is visible at the call site.
#'
#' @return The recipe, with every step selector quosure re-pointed. The prepped
#'   and baked output is unchanged.
#' @keywords internal
#' @noRd

strip_selector_envs <- function(rec, frame) {

  ## -------------------------------------------------------------------------
  ## Pass 1: harvest every quosure, wherever it lives in the step
  ## -------------------------------------------------------------------------

  harvest <- function(x, acc = list()) {

    if (rlang::is_quosure(x)) return(c(acc, list(x)))

    if (is.list(x)) {

      for (el in x) acc <- harvest(el, acc)

    }

    acc

  }

  quos <- harvest(lapply(rec$steps, unclass))

  if (length(quos) == 0) return(rec)

  ## -------------------------------------------------------------------------
  ## The names those selectors actually reference
  ## -------------------------------------------------------------------------
  ## all.vars() excludes namespace operands, so `dplyr::all_of(predictor_cols)`
  ## yields "predictor_cols" and `recipes::all_predictors()` yields nothing.
  ## Names that resolve against the data mask rather than the environment (a
  ## bare column name) simply won't be found in `frame`, which is correct.

  needed <- unique(unlist(lapply(quos, function(q) {
    all.vars(rlang::quo_get_expr(q))
  })))

  keep <- intersect(needed, ls(frame, all.names = TRUE))

  env <- rlang::new_environment(data   = mget(keep, envir = frame),
                                parent = rlang::ns_env("horizons"))

  ## -------------------------------------------------------------------------
  ## Pass 2: re-point, preserving every slot's class and attributes
  ## -------------------------------------------------------------------------
  ## Attributes are restored after lapply() because a step slot may hold a
  ## data frame, and rebuilding one as a bare list would corrupt it.

  repoint <- function(x) {

    if (rlang::is_quosure(x)) return(rlang::quo_set_env(x, env))

    if (is.list(x)) {

      out             <- lapply(x, repoint)
      attributes(out) <- attributes(x)
      return(out)

    }

    x

  }

  rec$steps <- lapply(rec$steps, function(step) {

    cls             <- class(step)
    out             <- repoint(unclass(step))
    class(out)      <- cls
    out

  })

  rec

}

## ---------------------------------------------------------------------------
## parse_config_covariates
## ---------------------------------------------------------------------------

#' Parse Covariate String from Config Row
#'
#' @description
#' Converts the `covariates` field in a config row (comma-separated string
#' or NA) into a character vector of covariate names.
#'
#' @param covariates_field Character or NA. The covariates column value from
#'   a config row, e.g. "pH,clay" or NA.
#'
#' @return Character vector of covariate names, or NULL if none.
#' @keywords internal
parse_config_covariates <- function(covariates_field) {

  if (is.null(covariates_field) || is.na(covariates_field) || covariates_field == "") {

    return(NULL)

  }

  covs <- trimws(strsplit(as.character(covariates_field), ",")[[1]])
  covs <- covs[nzchar(covs)]

  if (length(covs) == 0) return(NULL)

  covs

}
