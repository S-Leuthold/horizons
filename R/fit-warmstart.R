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

  ## Always include the best point, clamped into this range as its candidates
  ## are: fit()'s range can be narrower than the one evaluate() searched (a
  ## PLS num_comp capped by fewer training rows), and an out-of-range point
  ## would be fitted at the engine's own cap, tying it with the clamp.
  best_row <- tibble::as_tibble(
    best_params[, param_set$name, drop = FALSE]
  )

  for (i in seq_len(nrow(param_set))) {

    p_name <- param_set$name[i]
    best_row[[p_name]] <- clamp_to_range(best_row[[p_name]], param_set$object[[i]])

  }

  if (nrow(full_grid) > max_points) {

    ## Sample (max_points - 1) rows from the grid without the best point, then
    ## bind it, so the grid holds max_points rows whether or not the sample
    ## would have drawn it (#228). The best point is matched at
    ## point_matches()'s tolerance rather than exactly: a transformed
    ## parameter's candidate at offset 0 is the best value's round trip
    ## through its scale, and can differ from it in the last bits.
    ##
    ## The seed is scoped and the caller's stream restored. A bare set.seed(1)
    ## here clobbered it partway through fit_single_config(), so everything
    ## downstream — tune_grid()'s sampling, tune_bayes()'s GP initialisation —
    ## became independent of the user's `seed` when the grid overflowed
    ## max_points and dependent on it when it didn't. Written out rather than
    ## using withr::with_seed() because withr is only a Suggests. See #51.

    sampled <- local({

      if (exists(".Random.seed", envir = globalenv())) {

        old_seed <- get(".Random.seed", envir = globalenv())
        on.exit(assign(".Random.seed", old_seed, envir = globalenv()),
                add = TRUE)

      }

      set.seed(1, kind = "Mersenne-Twister")
      others <- full_grid[!point_matches(best_row, full_grid), , drop = FALSE]
      dplyr::slice_sample(others, n = max_points - 1L)

    })
    full_grid <- dplyr::bind_rows(best_row, sampled)

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
#'
#' A row is usable when it holds every tuned parameter with a value. A row
#' with any value missing starts from the space-filling grid, as one with
#' none does: a warm start centred on some parameters and spread across the
#' whole range of another would record `warm_start = TRUE` for a search that
#' was not one (#229).
#'
#' @keywords internal
usable_best_params <- function(best_params, param_set) {

  if (is.null(best_params)) return(FALSE)
  if (!inherits(best_params, "data.frame")) return(FALSE)
  if (nrow(best_params) == 0) return(FALSE)

  ## Must have columns matching the param_set
  needed <- param_set$name
  if (!all(needed %in% names(best_params))) return(FALSE)

  ## Must have every value
  vals <- unlist(best_params[1, needed])
  if (anyNA(vals)) return(FALSE)

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

  ## best_val is never missing: usable_best_params() sends a row with a
  ## missing value to the space-filling grid

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

#' Clamp a parameter value into its range
#'
#' In the space where the range lives, as `generate_candidates()` does. A
#' value inside the range, `NA`, and a non-numeric value (a qualitative
#' parameter's) are returned as given.
#'
#' @keywords internal
#' @noRd
clamp_to_range <- function(value, param_obj) {

  if (!is.numeric(value) || is.na(value)) return(value)

  lower <- param_obj$range$lower
  upper <- param_obj$range$upper
  trans <- param_obj$trans

  t_value <- if (is.null(trans)) value else trans$transform(value)

  if (t_value >= lower && t_value <= upper) return(value)

  t_value <- min(max(t_value, lower), upper)
  clamped <- if (is.null(trans)) t_value else trans$inverse(t_value)

  if (isTRUE(param_obj$type == "integer")) as.integer(round(clamped)) else clamped

}

#' Check if a single-row tibble exists in a grid
#' @keywords internal
point_in_grid <- function(point, grid) {

  any(point_matches(point, grid))

}

#' Which rows of a grid match a single-row tibble
#'
#' Numeric columns match within `sqrt(.Machine$double.eps)` relative to the
#' point's value, others exactly. The tolerance is relative so that a value
#' differing only in its last bits matches at any magnitude, while a penalty
#' near 1e-9 does not match its neighbours on the log scale, which an
#' absolute tolerance of 1.5e-8 would take for the same point.
#'
#' @param point Single-row tibble, with no missing value.
#' @param grid Tibble holding `point`'s columns.
#' @return Logical, one per row of `grid`.
#' @keywords internal
#' @noRd
point_matches <- function(point, grid) {

  cols <- names(point)
  matches <- rep(TRUE, nrow(grid))

  for (col in cols) {

    if (is.numeric(point[[col]])) {

      p       <- point[[col]]
      matches <- matches &
        (abs(grid[[col]] - p) <= sqrt(.Machine$double.eps) * pmax(abs(p), .Machine$double.xmin))

    } else {

      matches <- matches & (grid[[col]] == point[[col]])

    }

  }

  matches

}


## ---------------------------------------------------------------------------
## tune_warmstart_bayes()
## ---------------------------------------------------------------------------

#' Warm-Start Bayesian Optimization
#'
#' The re-tuning sequence of a `fit()` member, run inside its unit:
#' 1. Evaluate the starting grid ([build_warmstart_grid()]) with
#'    `tune::tune_grid()`. A result in which every model failed fails the
#'    member, with tune's cause (`check_tune_result()`), and so does a grid
#'    with no candidate scored on every fold (#101).
#' 2. If `bayesian_iter > 0`, re-pin the random stream to `seed` and refine
#'    with `bayes_search()`, which keeps what it finished when it fails, and
#'    says so (#209). The re-pin makes the search independent of where the
#'    grid stage left the stream, which differs between the sequential and
#'    parallel paths (#151).
#' 3. Choose the hyperparameters by RMSE among the candidates scored on every
#'    fold (`select_complete()`).
#'
#' @param workflow A tidymodels workflow (recipe + model spec, not fitted).
#' @param cv_resamples A `vfold_cv` object.
#' @param initial_grid The starting grid, from [build_warmstart_grid()].
#' @param param_set Finalized `parameters` object.
#' @param bayesian_iter Integer. Bayesian iterations after grid. 0 = grid only.
#' @param metric_set A `yardstick::metric_set`.
#' @param allow_par Logical. Passed to tune control functions.
#' @param recipe The member's untrained recipe, to reproduce a step's failure
#'   inside tune with its class; `NULL` when it was prepped before tuning.
#' @param debug Logical. Stop at the first failure rather than fall back.
#' @param seed Integer. The member's seed, set before the Bayesian search.
#'   Default 42.
#'
#' @return List with `tune_results` (the result the hyperparameters were
#'   chosen from), `best_params` (single-row tibble, with `.config`), `grid`
#'   (`initial_grid`, which the member reads its notes through, #109) and
#'   `n_dropped` (the candidates the Bayesian search proposed and tune
#'   dropped, scored on no fold).
#'
#' @keywords internal
tune_warmstart_bayes <- function(workflow,
                                 cv_resamples,
                                 initial_grid,
                                 param_set,
                                 bayesian_iter,
                                 metric_set,
                                 allow_par = FALSE,
                                 recipe    = NULL,
                                 debug     = FALSE,
                                 seed      = 42L) {

  n_folds <- nrow(cv_resamples)

  ## --- Phase 1: the starting grid --------------------------------------------

  grid_results <- unit_step("warm_start_grid", {

    results <- suppressMessages(tune::tune_grid(
      workflow,
      resamples = cv_resamples,
      grid      = initial_grid,
      metrics   = metric_set,
      control   = tune::control_grid(
        save_pred     = FALSE,
        allow_par     = allow_par,
        parallel_over = "resamples"   # what tune resolves NULL to here; explicit for visibility, no behaviour change
      )
    ))

    check_tune_result(results, "all models failed during CV", recipe = recipe, debug = debug)

    ## A candidate scored on fewer folds than were drawn takes no part in the
    ## choice (#101), so a grid with none scored on every fold has nothing to
    ## choose from.
    if (nrow(rank_complete(results, "rmse", n_folds, debug)) == 0) {

      abort_tune_failure(results, "no grid candidate was scored on every fold")

    }

    results

  })

  ## --- Phase 2: Bayesian optimization (if requested) -----------------------

  tune_results <- grid_results
  n_dropped    <- 0L

  if (bayesian_iter > 0) {

    ## Re-pinned before the stochastic stage, as evaluate_single_config()
    ## re-pins before its search: tune's future path advances the parent
    ## stream past where the sequential loop leaves it, so a search seeded
    ## only by what came before could differ between allow_par = TRUE and
    ## FALSE (#151).
    set.seed(seed, kind = "Mersenne-Twister")

    search <- unit_step("warm_start_bayes", bayes_search(
      workflow      = workflow,
      resamples     = cv_resamples,
      initial       = grid_results,
      iter          = bayesian_iter,
      param_info    = param_set,
      metrics       = metric_set,
      allow_par     = allow_par,
      parallel_over = "resamples",
      debug         = debug,
      search        = "the warm-start Bayesian search",
      grid          = "its starting grid"
    ))

    tune_results <- search$results
    n_dropped    <- search$n_dropped

  }

  ## --- Phase 3: Select best -----------------------------------------------

  list(
    tune_results = tune_results,
    best_params  = unit_step("select_best", select_complete(tune_results, "rmse", n_folds, debug)),
    grid         = initial_grid,
    n_dropped    = n_dropped
  )

}


#' Run a Bayesian search from a grid, keeping what it finished
#'
#' @description
#' The Bayesian stage of [evaluate_single_config()] and of `fit()`'s
#' warm-start re-tune ([tune_warmstart_bayes()]). A search that fails keeps
#' what it finished: the iterations that ran, or the grid's choice when none
#' did, and says so with a substitution warning ([warn_in_unit()]) that the
#' unit's row shows and the verb puts on its log (#209). That fallback is
#' containment, so the kinds no unit contains still propagate, as does a
#' package missing on a worker ([missing_package_signal()]), and under
#' `debug` nothing is fallen back from.
#'
#' tune recovers from an error inside its search loop itself: an on.exit()
#' returns the iterations finished so far, and that return cancels the
#' unwind of any handler outside it. So the first error to leave tune's own
#' catches is recorded as it is raised, with its trace, and what to do about
#' it is decided once `tune_bayes()` has returned.
#'
#' A proposed candidate that fails in every fold leaves no note and raises
#' nothing: tune drops it and says only "All models failed for: ..." in its
#' iteration log, or "Couldn't estimate performance" when its resampling
#' errored as a whole. The log is on in every mode, its output muffled, and
#' read: the candidates it drops are counted (`n_dropped`) for the row's note
#' on candidates left out ([incomplete_note()]), and under `debug` the first
#' stops the call.
#'
#' A worker that dies during an iteration on the resamples axis never
#' reaches here as future's error: tune's `try()` around the candidate's
#' resampling catches it. future.apply reports it first, "Caught
#' FutureInterruptError. Canceling all iterations ...", unclassed, as a
#' warning before 1.21.0 and as a message since, so the death is read from
#' that text in either ([worker_death_signal()]) and stops the run as
#' systemic once `tune_bayes()` has returned, as a death in the grid stage
#' does.
#'
#' The same on.exit() return swallows a Ctrl-C during the search, so the
#' interrupt is recorded as it arrives and raised again once tune has
#' returned: the search the user stopped is not kept as a result.
#'
#' The caller re-pins the seed first where it needs one, and grids a space
#' small enough to try whole instead of searching it ([exhaustive_grid()]).
#'
#' @param workflow,resamples,initial,iter,param_info,metrics Passed to
#'   [tune::tune_bayes()].
#' @param allow_par,parallel_over Passed to [tune::control_bayes()].
#' @param debug Logical(1). Stop at the first failure rather than fall back.
#' @param search,grid Character(1). How the messages name the search and the
#'   grid it started from, e.g. `"the Bayesian search"` and `"the grid"`.
#' @return List with `results` (the tune result to choose from: the
#'   search's when it ran an iteration, else `initial`) and `n_dropped` (the
#'   candidates it proposed that tune dropped, scored on no fold).
#' @keywords internal
#' @noRd
bayes_search <- function(workflow,
                         resamples,
                         initial,
                         iter,
                         param_info,
                         metrics,
                         allow_par     = FALSE,
                         parallel_over = "resamples",
                         debug         = FALSE,
                         search        = "the Bayesian search",
                         grid          = "the grid") {

  stopped     <- NULL
  all_failed  <- NULL
  worker_died <- NULL
  interrupted <- FALSE
  n_dropped   <- 0L
  search_cap  <- paste0(toupper(substr(search, 1, 1)), substring(search, 2))

  read_death <- function(cnd) {
    if (is.null(worker_died) && worker_death_signal(cnd)) worker_died <<- cnd
  }

  read_log <- function(m) {
    read_death(m)
    text <- cli::ansi_strip(conditionMessage(m))
    if (grepl("All models failed for:", text, fixed = TRUE)) {
      n_dropped <<- n_dropped + 1L
      if (is.null(all_failed)) {
        all_failed <<- trimws(sub("^.*?(All models failed for:)", "\\1", text, perl = TRUE))
      }
    } else if (grepl("Couldn't estimate performance", text, fixed = TRUE)) {
      n_dropped <<- n_dropped + 1L
    }
  }

  bayes_results <- tryCatch(
    withCallingHandlers(
      suppressMessages(withCallingHandlers(
        tune::tune_bayes(
          object     = workflow,
          resamples  = resamples,
          initial    = initial,
          iter       = as.integer(iter),
          param_info = param_info,
          metrics    = metrics,
          control    = tune::control_bayes(
            save_pred     = FALSE,
            save_workflow = FALSE,
            verbose       = FALSE,
            verbose_iter  = TRUE,
            no_improve    = BAYES_NO_IMPROVE_LIMIT,
            allow_par     = allow_par,
            parallel_over = parallel_over
          )
        ),
        message = read_log
      )),
      warning   = read_death,
      error     = function(e) if (is.null(stopped)) stopped <<- rlang::cnd_entrace(e),
      interrupt = function(i) interrupted <<- TRUE
    ),
    ## An error tune did not recover from (before its loop starts):
    ## recorded above, nothing to keep
    error = function(e) NULL
  )

  if (interrupted) rlang::interrupt()

  if (!is.null(worker_died)) {

    abort_horizons(
      "A parallel worker stopped during {search}.",
      kind = "run", class = "horizons_error_systemic", parent = worker_died
    )

  }

  ## A kind no unit contains, or a package missing on a worker, is raised
  ## again for the unit, not fallen back from
  if (!is.null(stopped) &&
      (debug || rlang::cnd_inherits(stopped, NEVER_CONTAINED) || missing_package_signal(stopped))) {

    stop(stopped)

  }

  if (debug && !is.null(all_failed)) {

    ### tune's own line, capped as another package's text is
    why <- condition_summary(all_failed)

    abort_horizons(
      "a candidate {search} proposed failed in every fold, and tune keeps no record of why: {why}",
      kind = "run"
    )

  }

  if (!is.null(bayes_results)) check_tune_result(bayes_results, failure = NULL, debug = debug)

  ## tune_bayes() returns, rather than erroring, when the search loop errors
  ## (its on.exit() hands back the results so far) or when every candidate
  ## fails in every fold (it drops such a candidate without a note). Either
  ## way the result holds only the initial grid, whose rows tune gives `.iter`
  ## 0, so a search that kept nothing has no iteration past it.
  cause  <- if (!is.null(stopped)) condition_summary(stopped)
  n_iter <- if (is.null(bayes_results)) 0L else max(bayes_results$.iter)

  if (n_iter > 0) {

    if (!is.null(cause)) {

      warn_in_unit("substitution",
                   "{search_cap} stopped early, after {n_iter} iteration{?s}, so the hyperparameters were chosen from those and {grid}: {cause}")

    }

    ## Candidates that failed in some folds leave their reasons in .notes,
    ## which the caller reads
    return(list(results = bayes_results, n_dropped = n_dropped))

  }

  if (!is.null(cause)) {

    warn_in_unit("substitution",
                 "{search_cap} failed, so the hyperparameters were chosen from {grid} alone: {cause}")

  } else {

    ## The search returned without an iteration past the grid: every
    ## candidate failed in every fold, which tune drops without a note.
    warn_in_unit("substitution",
                 "{search_cap} produced no results, so the hyperparameters were chosen from {grid} alone.")

  }

  list(results = initial, n_dropped = n_dropped)

}

## ---------------------------------------------------------------------------
## exhaustive_grid — a search space small enough to try whole
## ---------------------------------------------------------------------------

#' Every point of a search space that fits a budget
#'
#' @description
#' When every tuned parameter takes whole-number values over a known range
#' and the space has no more points than `budget`, the whole space as a grid;
#' otherwise `NULL`. A search whose budget covers the space is then
#' exhaustive: the grid is every point and no Bayesian stage runs, since
#' tune's search, once it has no value left to propose, errors inside its
#' loop rather than stopping.
#'
#' @param param_set A finalized `dials` parameter set.
#' @param budget Numeric(1). The points the search would try: the grid's
#'   size and the Bayesian iterations together.
#' @return A tibble with one column per parameter (named by its id) and one
#'   row per point, or `NULL`.
#' @keywords internal
#' @noRd
exhaustive_grid <- function(param_set, budget) {

  if (nrow(param_set) == 0) return(NULL)

  values <- lapply(param_set$object, function(p) {

    if (!inherits(p, "quant_param") || !identical(p$type, "integer") || !is.null(p$trans)) {
      return(NULL)
    }

    lower <- p$range$lower
    upper <- p$range$upper

    ### unknown() until finalized
    if (!is.numeric(lower) || !is.numeric(upper) || !is.finite(lower) || !is.finite(upper)) {
      return(NULL)
    }

    if (!isTRUE(p$inclusive[[1]])) lower <- lower + 1
    if (!isTRUE(p$inclusive[[2]])) upper <- upper - 1

    if (upper < lower) return(NULL)

    seq.int(as.integer(lower), as.integer(upper))

  })

  if (any(vapply(values, is.null, logical(1)))) return(NULL)
  if (prod(lengths(values)) > budget) return(NULL)

  names(values) <- param_set$id

  tibble::as_tibble(expand.grid(values, KEEP.OUT.ATTRS = FALSE))

}
