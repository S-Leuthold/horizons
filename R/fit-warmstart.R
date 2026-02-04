#' Warm-Start Grid Construction and Bayesian Optimization for fit()
#'
#' @description
#' Two functions that handle the re-tuning phase of `fit()`:
#'
#' 1. `build_warmstart_grid()` — constructs an exploration grid centered on
#'    evaluate()'s best hyperparameters.
#' 2. `tune_warmstart_bayes()` — orchestrates grid search → Bayesian
#'    optimization using the warm-start grid as initial points.
#'
#' @keywords internal

## ---------------------------------------------------------------------------
## build_warmstart_grid()
## ---------------------------------------------------------------------------

#' Build Warm-Start Exploration Grid
#'
#' Constructs a grid of hyperparameter candidates centered on a previous
#' best configuration. For each parameter, generates nearby candidate values
#' respecting the parameter's type, scale, and range constraints.
#'
#' @param best_params Single-row tibble of best hyperparameters from
#'   evaluate() (output of `tune::select_best()`). Can be NULL for fallback.
#' @param param_set A `parameters` object from
#'   `workflows::extract_parameter_set_dials()`. Must be finalized (e.g.,
#'   mtry upper bound set).
#' @param max_points Integer. Maximum number of grid points. Default 25.
#'
#' @return A tibble of hyperparameter combinations.
#'
#' @keywords internal
#' @export
build_warmstart_grid <- function(best_params, param_set, max_points = 25L) {

  ## --- Fallback: if best_params is unusable, return space-filling grid ------

  if (!usable_best_params(best_params, param_set)) {

    return(
      dials::grid_space_filling(param_set, size = as.integer(max_points))
    )

  }

  ## --- Generate candidate values per parameter -----------------------------

  candidate_lists <- purrr::map(seq_len(nrow(param_set)), function(i) {

    p_name  <- param_set$name[i]
    p_obj   <- param_set$object[[i]]
    best_val <- best_params[[p_name]]

    generate_candidates(best_val, p_obj)

  })

  names(candidate_lists) <- param_set$name

  ## --- Expand grid and cap at max_points -----------------------------------

  full_grid <- tibble::as_tibble(
    expand.grid(candidate_lists, stringsAsFactors = FALSE)
  )

  ## Always include the exact best point
  best_row <- tibble::as_tibble(
    best_params[, param_set$name, drop = FALSE]
  )

  if (nrow(full_grid) > max_points) {

    ## Sample (max_points - 1) rows, then bind the best point
    set.seed(1)
    sampled <- dplyr::slice_sample(full_grid, n = max_points - 1L)
    full_grid <- dplyr::bind_rows(best_row, sampled) %>%
      dplyr::distinct()

  }

  ## Ensure the best point is present (may already be via expansion)
  if (!point_in_grid(best_row, full_grid)) {

    full_grid <- dplyr::bind_rows(best_row, full_grid)

  }

  ## Final cap
  if (nrow(full_grid) > max_points) {

    full_grid <- dplyr::slice_head(full_grid, n = max_points)

  }

  full_grid

}


## ---------------------------------------------------------------------------
## Internal helpers for build_warmstart_grid
## ---------------------------------------------------------------------------

#' Check whether best_params is usable
#' @keywords internal
usable_best_params <- function(best_params, param_set) {

  if (is.null(best_params)) return(FALSE)
  if (!inherits(best_params, "data.frame")) return(FALSE)
  if (nrow(best_params) == 0) return(FALSE)

  ## Must have columns matching the param_set
  needed <- param_set$name
  if (!all(needed %in% names(best_params))) return(FALSE)

  ## Must have at least one non-NA value
  vals <- unlist(best_params[1, needed])
  if (all(is.na(vals))) return(FALSE)

  TRUE

}

#' Generate candidate values for a single parameter
#'
#' Respects dials' convention: ranges for transformed params are stored in
#' transformed space. So we clamp in the space where the range lives, then
#' convert to original scale for the final grid.
#'
#' @keywords internal
generate_candidates <- function(best_val, param_obj) {

  lower <- param_obj$range$lower
  upper <- param_obj$range$upper
  trans <- param_obj$trans
  is_int <- isTRUE(param_obj$type == "integer")
  has_trans <- !is.null(trans)

  ## Handle NA best_val: generate evenly-spaced candidates across full range
  if (is.na(best_val)) {

    if (has_trans) {

      t_candidates <- seq(lower, upper, length.out = 5)
      candidates   <- trans$inverse(t_candidates)

    } else if (is_int) {

      candidates <- as.integer(round(seq(lower, upper, length.out = 5)))

    } else {

      candidates <- seq(lower, upper, length.out = 5)

    }

    candidates <- pmax(candidates, if (has_trans) trans$inverse(lower) else lower)
    candidates <- pmin(candidates, if (has_trans) trans$inverse(upper) else upper)

    return(unique(candidates))

  }

  if (has_trans) {

    ## --- Transformed params (e.g., learn_rate on log10 scale) ---
    ## Range is in transformed space already. Work there, clamp there,
    ## then inverse-transform back to original scale.
    t_best <- trans$transform(best_val)
    offsets <- c(-1, -0.5, 0, 0.5, 1) * 0.5
    t_candidates <- t_best + offsets

    ## Clamp in transformed space (where range is defined)
    t_candidates <- pmax(t_candidates, lower)
    t_candidates <- pmin(t_candidates, upper)

    candidates <- trans$inverse(t_candidates)

  } else if (is_int) {

    ## --- Integer params (no transform) ---
    ## Range is in original space.
    offsets <- c(-2L, -1L, 0L, 1L, 2L)
    candidates <- as.integer(round(best_val + offsets))

    candidates <- pmax(candidates, lower)
    candidates <- pmin(candidates, upper)
    candidates <- as.integer(candidates)

  } else {

    ## --- Continuous params (no transform) ---
    ## Range is in original space.
    multipliers <- c(0.8, 0.9, 1.0, 1.1, 1.2)
    candidates <- best_val * multipliers

    candidates <- pmax(candidates, lower)
    candidates <- pmin(candidates, upper)

  }

  unique(candidates)

}

#' Check if a single-row tibble exists in a grid
#' @keywords internal
point_in_grid <- function(point, grid) {

  if (nrow(grid) == 0) return(FALSE)

  cols <- names(point)
  matches <- rep(TRUE, nrow(grid))

  for (col in cols) {

    if (is.numeric(point[[col]])) {

      matches <- matches &
        (abs(grid[[col]] - point[[col]]) < .Machine$double.eps^0.5)

    } else {

      matches <- matches & (grid[[col]] == point[[col]])

    }

  }

  any(matches)

}


## ---------------------------------------------------------------------------
## tune_warmstart_bayes()
## ---------------------------------------------------------------------------

#' Warm-Start Bayesian Optimization
#'
#' Orchestrates the re-tuning sequence for fit():
#' 1. Build warm-start grid via `build_warmstart_grid()`
#' 2. Evaluate grid via `tune::tune_grid()`
#' 3. If `bayesian_iter > 0`, refine via `tune::tune_bayes()` seeded from
#'    the grid results
#' 4. Select best params via `tune::select_best(metric = "rmse")`
#'
#' @param workflow A tidymodels workflow (recipe + model spec, not fitted).
#' @param cv_resamples A `vfold_cv` object.
#' @param best_params Single-row tibble from evaluate's `select_best()`.
#' @param param_set Finalized `parameters` object.
#' @param bayesian_iter Integer. Bayesian iterations after grid. 0 = grid only.
#' @param metric_set A `yardstick::metric_set`.
#' @param allow_par Logical. Passed to tune control functions.
#'
#' @return List with:
#'   - `tune_results`: The final tuning result object
#'   - `best_params`: Single-row tibble of best hyperparameters
#'   - `fallback_used`: Logical, TRUE if grid fallback was used
#'
#' @keywords internal
#' @export
tune_warmstart_bayes <- function(workflow,
                                  cv_resamples,
                                  best_params,
                                  param_set,
                                  bayesian_iter,
                                  grid_size  = WARMSTART_GRID_SIZE,
                                  metric_set,
                                  allow_par = FALSE) {

  ## --- Phase 1: Build warm-start grid and run tune_grid -------------------

  initial_grid <- build_warmstart_grid(best_params, param_set,
                                       max_points = grid_size)
  fallback_used <- !usable_best_params(best_params, param_set)

  grid_result <- safely_execute(
    tune::tune_grid(
      workflow,
      resamples = cv_resamples,
      grid      = initial_grid,
      metrics   = metric_set,
      control   = tune::control_grid(
        save_pred = FALSE,
        allow_par = allow_par
      )
    ),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(grid_result$error)) {

    return(list(
      tune_results  = NULL,
      best_params   = NULL,
      fallback_used = fallback_used,
      error         = grid_result$error$message
    ))

  }

  tune_results <- grid_result$result
  bayes_failed <- FALSE

  ## --- Phase 2: Bayesian optimization (if requested) ----------------------

  if (bayesian_iter > 0) {

    bayes_result <- safely_execute(
      tune::tune_bayes(
        workflow,
        resamples  = cv_resamples,
        initial    = tune_results,
        iter       = as.integer(bayesian_iter),
        metrics    = metric_set,
        param_info = param_set,
        control    = tune::control_bayes(
          no_improve = BAYES_NO_IMPROVE_LIMIT,
          allow_par  = allow_par
        )
      ),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    ## If Bayes fails, fall back to grid results
    if (is.null(bayes_result$error)) {

      tune_results <- bayes_result$result

    } else {

      bayes_failed <- TRUE

    }

  }

  ## --- Phase 3: Select best -----------------------------------------------

  final_best <- tune::select_best(tune_results, metric = "rmse")

  list(
    tune_results  = tune_results,
    best_params   = final_best,
    fallback_used = fallback_used,
    bayes_failed  = bayes_failed
  )

}
