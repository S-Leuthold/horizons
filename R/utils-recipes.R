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
#' 3. Feature selection (pca/boruta/cars/correlation/none), on spectral
#'    columns only
#' 4. Covariate inclusion (promote requested covariates to predictor; the rest
#'    stay in a non-predictor hold role)
#'
#' Columns that are neither the outcome nor spectral predictors are given an
#' explicit non-predictor role — `id`, `meta`, `response_hold` (sibling lab
#' responses) and `covariate_hold`. Nothing is left to the `outcome ~ .`
#' default, which would make a predictor of any column the role map did not
#' name.
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

  ## Sibling responses: every lab-measured property joined by add_response()
  ## that configure() did not promote to outcome. They are in the analysis
  ## table but must never be modelled.
  response_cols  <- setdiff(
    role_map$variable[role_map$role == "response"],
    outcome_col
  )

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

  ## `id` is the one non-predictor role that stays required at bake. It is the
  ## key `predict()` joins its output back on, so a batch arriving without it is
  ## a caller error worth aborting on. The hold roles below are the opposite
  ## case: they exist only in the training table, so requiring them would make
  ## every legitimate prediction batch fail.
  rec <- recipes::update_role(rec, dplyr::all_of(id_col), new_role = "id")
  rec <- recipes::update_role_requirements(rec, role = "id", bake = TRUE)

  if (length(meta_cols) > 0) {

    ## Every meta column is relaxed here, not just select_training()'s
    ## provenance columns (.drawn_by, .min_distance, .group) — those are the
    ## motivating case, but user-supplied meta from spectra() is treated the
    ## same way, since no step consumes a meta column at bake time. On the
    ## workflow path the relaxation also means meta columns are dropped before
    ## bake() even when new data carries them: hardhat builds its extra-role
    ## ptypes from the bake requirements, so a role at FALSE is excluded from
    ## what forge() passes through. A future step that needs a meta column at
    ## bake time would therefore see it silently absent rather than error.
    rec <- recipes::update_role(rec, dplyr::all_of(meta_cols), new_role = "meta")
    rec <- recipes::update_role_requirements(rec, role = "meta", bake = FALSE)

  }

  if (length(response_cols) > 0) {

    ## Without an explicit role these fall through `outcome ~ .` to predictor,
    ## which puts a lab measurement of the same sample into the model matrix —
    ## target leakage that the pool-internal CV cannot see. They are also never
    ## present at predict time, so the role is relaxed at bake like meta.
    rec <- recipes::update_role(rec, dplyr::all_of(response_cols),
                                new_role = "response_hold")
    rec <- recipes::update_role_requirements(rec, role = "response_hold",
                                             bake = FALSE)

  }

  ## Mark covariates with a non-predictor role initially
  ## They'll be added back as predictors in Step 5 if this config uses them.
  ## Relaxed at bake for the same reason as meta: resolve_new_data() strips
  ## covariates from new data unconditionally, so requiring them would break
  ## predict() for any object that carries covariate columns at all.
  if (length(all_covariate_cols) > 0) {

    rec <- recipes::update_role(rec, dplyr::all_of(all_covariate_cols),
                                new_role = "covariate_hold")
    rec <- recipes::update_role_requirements(rec, role = "covariate_hold",
                                             bake = FALSE)

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
  ##
  ## Selection is by name pattern, not `all_predictors()`. `update_role()` is
  ## not sequenced with the steps — it rewrites `var_info` for the whole
  ## recipe — so a covariate promoted to predictor in Step 5 would be inside
  ## `all_predictors()` when these steps prep, folding a non-spectral column
  ## into the PCA rotation or into step_select_correlation()'s 3-wide
  ## contiguity window. Selecting by name is the same approach
  ## step_transform_spectra takes, and it makes the bypass true regardless of
  ## the order roles happen to be assigned in.
  ##
  ## The pattern is "spec" + digits because that is what the transform step's
  ## prep() renames its output to (`recipes::names0(prefix = "spec")`), so by
  ## the time these steps see the data the wavenumber names are gone.
  ##
  ## A zero match therefore means that naming changed, and every branch below
  ## aborts at prep() when it happens rather than quietly selecting nothing.
  ## The three custom steps check it themselves (`check_selection_columns()`).
  ## `recipes::step_pca()` does not — an empty selection there preps and bakes
  ## as a pass-through no-op, which would train the model on the untransformed
  ## spectral columns while the config still claimed PCA — so its branch selects
  ## through `select_generated_spectra()`, which aborts in the selector itself.

  feature_selection <- tolower(as.character(config_row$feature_selection))

  rec <- switch(feature_selection,

    "none" = rec,

    "pca" = rec |>
      recipes::step_pca(
        select_generated_spectra(),
        threshold = 0.995,
        options   = list(scale. = TRUE, center = TRUE)
      ),

    "correlation" = rec |>
      step_select_correlation(
        dplyr::matches("^spec[0-9]+$"),
        outcome = outcome_col
      ),

    "boruta" = rec |>
      step_select_boruta(
        dplyr::matches("^spec[0-9]+$"),
        outcome = outcome_col
      ),

    "cars" = rec |>
      step_select_cars(
        dplyr::matches("^spec[0-9]+$"),
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
  ##
  ## Unrequested covariates are left in `covariate_hold` rather than removed by
  ## a step. The role alone is sufficient — a held column is outside
  ## `all_predictors()`, outside the spectral selectors above, and outside the
  ## workflow blueprint's predictor ptype, so it never reaches a model. A
  ## step_rm() would additionally require those columns at bake time
  ## (`recipes::step_rm`'s bake calls `check_new_data()` on its removals), and
  ## `resolve_new_data()` strips covariates from new data unconditionally, so
  ## the step aborted predict() for covariates the config did not even use.

  if (length(all_covariate_cols) > 0 &&
      !is.null(config_covariates) && length(config_covariates) > 0) {

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
#' Every slot is walked recursively rather than `terms` alone. Stock steps
#' and this package's custom steps keep their selectors in `terms` (#52), but
#' others use `impute_with`, `denom`, `inputs`, `outcome`, or `lon`/`lat`,
#' and a slot holding a *single* quosure is not a list. Both would have been
#' silently missed, restoring the leak.
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
## select_generated_spectra
## ---------------------------------------------------------------------------

#' Select the Transform Step's Generated Spectral Columns, Loudly
#'
#' @description
#' A tidyselect helper for use inside a recipe step's `...`. Matches the
#' columns `step_transform_spectra()` generates (`spec1`, `spec2`, ...) and
#' aborts when the match is empty.
#'
#' The abort is the reason it exists. This package's own selection steps check
#' their resolved columns in `prep()` (`check_selection_columns()`), but
#' `recipes::step_pca()` treats an empty selection as a no-op: it preps, bakes,
#' and passes the untransformed columns straight through, so a config asking
#' for PCA would train on raw spectra with nothing said. Aborting inside the
#' selector puts the same gate in front of a step whose `prep()` this package
#' does not own.
#'
#' @param pattern Regular expression for the generated names. Default is the
#'   `recipes::names0(prefix = "spec")` pattern.
#'
#' @return Integer column positions, as tidyselect helpers return. Aborts on an
#'   empty match. Must be called inside a selection context.
#' @keywords internal
#' @noRd
select_generated_spectra <- function(pattern = "^spec[0-9]+$") {

  matched <- dplyr::matches(pattern)

  if (length(matched) == 0) {

    cli::cli_abort(c(
      "No column matches {.val {pattern}}, so the selection step has nothing to select.",
      "i" = "That pattern is the naming {.fn step_transform_spectra} gives its output; a zero match means the naming changed or the transform step did not run.",
      "i" = "Aborting rather than letting the step prep as a silent pass-through."
    ), class = "horizons_input_error")

  }

  matched

}

## ---------------------------------------------------------------------------
## check_selection_columns
## ---------------------------------------------------------------------------

#' Abort When a Selection Step Resolves to Zero Columns
#'
#' @description
#' Shared `prep()`-time gate for the feature-selection steps. A selection step
#' whose selector matches nothing has nothing to select, which is a
#' configuration error rather than a degenerate-but-valid case: with no columns
#' the step trains on an empty matrix and, depending on the algorithm, either
#' errors deep inside a modelling package or falls through its
#' "retain everything" branch and silently becomes a no-op.
#'
#' In `build_recipe()` the selector is the name pattern the transform step's
#' output uses (`^spec[0-9]+$`), so a zero match means that naming changed and
#' the whole selection stage would otherwise be skipped without a word.
#'
#' @param col_names Character vector from `recipes::recipes_eval_select()`.
#' @param step Character. The step's user-facing name, for the message.
#'
#' @return Invisibly `TRUE`; aborts when `col_names` is empty.
#' @keywords internal
#' @noRd
check_selection_columns <- function(col_names, step) {

  if (length(col_names) == 0) {

    cli::cli_abort(c(
      "{.fn {step}} selected zero columns.",
      "x" = "The step's selector matched no column in the training data.",
      "i" = "Inside {.fn build_recipe} the selector is {.code dplyr::matches(\"^spec[0-9]+$\")}, the names {.fn step_transform_spectra} gives its output. A zero match means that naming changed.",
      "i" = "A selection step with nothing to select cannot train; fix the selector rather than letting the step become a silent no-op."
    ), class = "horizons_input_error")

  }

  invisible(TRUE)

}

## ---------------------------------------------------------------------------
## step_selectors
## ---------------------------------------------------------------------------

#' The Selector Quosures a Custom Step Should Resolve at prep()
#'
#' @description
#' Shared by the `prep()` methods of the four custom steps. Since #52 a step
#' keeps its selectors in `terms` and its resolved names in `columns`. Steps
#' built by earlier versions kept the selectors in `columns` and overwrote them
#' with the resolved names at `prep()`, so this reads whichever layout the step
#' has and names the one case that cannot be recovered.
#'
#' Three layouts, in order:
#'
#' 1. `terms` holds the selectors. Returned as they are. After `butcher()` they
#'    are a plain list of quosures rather than a `quosures` object, which still
#'    counts.
#' 2. `columns` holds quosures: an untrained step from an earlier version.
#'    Returned, so the step preps normally and comes out in the current layout.
#' 3. Neither: a step from an earlier version that has already been prepped,
#'    whose selectors are gone. Aborts. Without this check the step resolves
#'    zero columns and aborts on a symptom (the window check, or a selection
#'    step's "selected zero columns") that points at the wrong cause.
#'
#' A `butcher()`ed step from an earlier version carries `terms = list()`, an
#' empty plain list, which is why an empty `terms` counts as selectors only
#' when it is a `quosures` object (a step called with no selectors at all).
#'
#' @param x A custom step object, as passed to its `prep()` method.
#' @param step Character. The step's user-facing name, for the message.
#'
#' @return The selector quosures to pass to `recipes::recipes_eval_select()`.
#' @keywords internal
#' @noRd
step_selectors <- function(x, step) {

  terms <- x[["terms"]]

  holds_quosures <- rlang::is_quosures(terms) ||
    (is.list(terms) && length(terms) > 0 &&
       all(vapply(terms, rlang::is_quosure, logical(1))))

  if (holds_quosures) return(terms)

  if (rlang::is_quosures(x[["columns"]])) return(x[["columns"]])

  cli::cli_abort(c(
    "{.fn {step}} has no stored selectors, so it cannot be prepped again.",
    "x" = "The step was built by an earlier version of horizons, which replaced its selectors with the resolved column names at {.fn prep} (#52).",
    "i" = "Rebuild the recipe with this version of horizons. A trained recipe from the earlier version still bakes; only re-prepping it needs the rebuild."
  ), class = "horizons_input_error")

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
