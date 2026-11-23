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
    ## The sample draws from the stream as it finds it: fit_single_config()
    ## builds the grid in the member's `warmstart-grid` stage
    ## (with_unit_seed()).

    others    <- full_grid[!point_matches(best_row, full_grid), , drop = FALSE]
    sampled   <- dplyr::slice_sample(others, n = max_points - 1L)
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
#' 2. If `bayesian_iter > 0`, refine with `bayes_search()`, which keeps what
#'    it finished when it fails, and says so (#209).
#'
#' Each phase is a stage of the member's own (`warmstart-tune`, `bayes`;
#' `with_unit_seed()`), with its seed given to the engine and to the search,
#' so the search does not depend on what the grid stage drew, nor on the
#' axis it ran on (#151).
#' 3. Choose the hyperparameters by RMSE among the candidates scored on every
#'    fold (`select_complete()`).
#'
#' @param workflow A tidymodels workflow, not fitted: the member's model
#'   alone, as [fit_single_config()] gives it, over prepped folds.
#' @param cv_resamples An `rset`: the member's folds, each recipe prepped
#'   once.
#' @param initial_grid The starting grid, from [build_warmstart_grid()].
#' @param param_set Finalized `parameters` object.
#' @param bayesian_iter Integer. Bayesian iterations after grid. 0 = grid only.
#' @param no_improve A whole number, or `FALSE`: the Bayesian search's
#'   stopping rule (`bayes_search()`).
#' @param metric_set A `yardstick::metric_set`.
#' @param allow_par Logical. Passed to tune control functions.
#' @param debug Logical. Stop at the first failure rather than fall back.
#' @param seed Integer. The run's seed, from which each phase's is derived.
#' @param config_id Character. The member's configuration id, which names
#'   its phases' units.
#'
#' @return List with `tune_results` (the result the hyperparameters were
#'   chosen from), `best_params` (single-row tibble, with `.config`), `grid`
#'   (`initial_grid`, which the member reads its notes through, #109),
#'   `n_dropped` (the candidates the Bayesian search proposed and tune
#'   dropped, scored on no fold), and `bayes_iter` and `bayes_stop`, the
#'   iterations the search began and why it ended (`bayes_search()`'s
#'   `n_iter` and `stop`; `NA` with no Bayesian search).
#'
#' @keywords internal
tune_warmstart_bayes <- function(workflow,
                                 cv_resamples,
                                 initial_grid,
                                 param_set,
                                 bayesian_iter,
                                 no_improve = BAYES_NO_IMPROVE_DEFAULT,
                                 metric_set,
                                 allow_par = FALSE,
                                 debug     = FALSE,
                                 seed,
                                 config_id) {

  n_folds    <- nrow(cv_resamples)
  tune_stage <- c("fit", config_id, "warmstart-tune")
  bayes_seed <- unit_seed(seed, c("fit", config_id, "bayes"))

  ## --- Phase 1: the starting grid --------------------------------------------

  grid_results <- unit_step("warm_start_grid", with_unit_seed(seed, tune_stage, {

    results <- suppressMessages(tune::tune_grid(
      seed_engine(workflow, unit_seed(seed, tune_stage)),
      resamples = cv_resamples,
      grid      = initial_grid,
      metrics   = metric_set,
      control   = tune::control_grid(
        save_pred     = FALSE,
        allow_par     = allow_par,
        parallel_over = "resamples"   # what tune resolves NULL to here; explicit for visibility, no behaviour change
      )
    ))

    check_tune_result(results, "all models failed during CV", debug = debug)

    ## A candidate scored on fewer folds than were drawn takes no part in the
    ## choice (#101), so a grid with none scored on every fold has nothing to
    ## choose from.
    if (nrow(rank_complete(results, "rmse", n_folds, debug)) == 0) {

      abort_tune_failure(results, "no grid candidate was scored on every fold")

    }

    results

  }))

  ## --- Phase 2: Bayesian optimization (if requested) -----------------------

  tune_results <- grid_results
  n_dropped    <- 0L
  bayes_iter   <- NA_integer_
  bayes_stop   <- NA_character_

  if (bayesian_iter > 0) {

    search <- unit_step("warm_start_bayes", with_unit_seed(seed, c("fit", config_id, "bayes"), bayes_search(
      workflow      = seed_engine(workflow, bayes_seed),
      resamples     = cv_resamples,
      initial       = grid_results,
      iter          = bayesian_iter,
      param_info    = param_set,
      metrics       = metric_set,
      seed          = bayes_seed,
      no_improve    = no_improve,
      allow_par     = allow_par,
      parallel_over = "resamples",
      debug         = debug,
      search        = "the warm-start Bayesian search",
      grid          = "its starting grid"
    )))

    tune_results <- search$results
    n_dropped    <- search$n_dropped
    bayes_iter   <- search$n_iter
    bayes_stop   <- search$stop

  }

  ## --- Phase 3: Select best -----------------------------------------------

  list(
    tune_results = tune_results,
    best_params  = unit_step("select_best", select_complete(tune_results, "rmse", n_folds, debug)),
    grid         = initial_grid,
    n_dropped    = n_dropped,
    bayes_iter   = bayes_iter,
    bayes_stop   = bayes_stop
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
#' The search stops after `no_improve` iterations without improving on the
#' best (tune's own rule), and the log says so ("No improvement for ..."),
#' which is read too, with each iteration's header: the result does not
#' show it, since an iteration whose candidate tune dropped binds no row.
#' tune checks its rule on the last iteration too, so a search the rule
#' stops there ran every iteration asked for, and is recorded as such. The
#' verbs say how many searches stopped early from what this returns.
#'
#' An isolated fit whose process dies ([run_isolated()]) fails the unit
#' with `horizons_error_process_died`, whether tune kept it as a note or
#' dropped the candidate, so the search is not fallen back from. tune keeps
#' no note of a dropped candidate, and on the resamples axis prints its
#' reason on the daemon, so the deaths are read from the processes the fits
#' ran in ([isolated_deaths()]): this one, and each daemon when the folds ran
#' on them. The record is emptied before the search too, since what it holds
#' then is other units' deaths, read already from tune's notes. A search
#' whose model is not fitted through an isolated twin ([fits_isolated()])
#' starts no such process, so it neither empties nor reads the record, and
#' sends the daemons no task for it.
#'
#' A daemon that dies during an iteration on the resamples axis does not
#' reach here at all: tune prints "Optimization stopped prematurely" and
#' returns the iterations so far. The verb reads the death from the
#' connected daemons around the unit (`stop_on_worker_death()`), so the
#' search it cut short is never kept.
#'
#' The same on.exit() return swallows a Ctrl-C during the search, so the
#' interrupt is recorded as it arrives and raised again once tune has
#' returned: the search the user stopped is not kept as a result.
#'
#' The caller runs it inside its stage's unit ([with_unit_seed()]) and passes
#' that unit's seed, and grids a space small enough to try whole instead of
#' searching it ([exhaustive_grid()]).
#'
#' @param workflow,resamples,initial,iter,param_info,metrics Passed to
#'   [tune::tune_bayes()].
#' @param seed Integer. The stage's seed, for [tune::control_bayes()].
#' @param no_improve A whole number of at least 1, or `FALSE` to run every
#'   iteration: the iterations without improvement after which the search
#'   stops (`control_horizons(no_improve = )`).
#' @param allow_par,parallel_over Passed to [tune::control_bayes()].
#' @param debug Logical(1). Stop at the first failure rather than fall back.
#' @param search,grid Character(1). How the messages name the search and the
#'   grid it started from, e.g. `"the Bayesian search"` and `"the grid"`.
#' @return List with `results` (the tune result to choose from: the
#'   search's when it ran an iteration, else `initial`), `n_dropped` (the
#'   candidates it proposed that tune dropped, scored on no fold), `n_iter`
#'   (the iterations it began; 0 when it failed before its first) and `stop`
#'   (why it ended: `"no_improve"`, the stopping rule, before the last
#'   iteration asked for; `"failed"`, an error it kept what it finished from;
#'   or `"budget"`, every iteration asked for).
#' @keywords internal
#' @noRd
bayes_search <- function(workflow,
                         resamples,
                         initial,
                         iter,
                         param_info,
                         metrics,
                         seed,
                         no_improve    = BAYES_NO_IMPROVE_DEFAULT,
                         allow_par     = FALSE,
                         parallel_over = "resamples",
                         debug         = FALSE,
                         search        = "the Bayesian search",
                         grid          = "the grid") {

  stopped     <- NULL
  all_failed  <- NULL
  interrupted <- FALSE
  n_dropped   <- 0L
  n_entered   <- 0L
  no_better   <- FALSE
  search_cap  <- paste0(toupper(substr(search, 1, 1)), substring(search, 2))

  ## The deaths of isolated fits' processes, read where the fits ran: on the
  ## daemons when tune sends them the folds. What the record holds now is
  ## other units', so it is emptied first. Only a model fitted through an
  ## isolated twin starts such processes.
  isolated   <- fits_isolated(workflow)
  on_daemons <- isTRUE(allow_par)
  if (isolated) isolated_deaths(on_daemons)

  read_log <- function(m) {
    text <- cli::ansi_strip(conditionMessage(m))
    if (grepl("Iteration [0-9]+", text)) n_entered <<- n_entered + 1L
    if (grepl("No improvement for", text, fixed = TRUE)) no_better <<- TRUE
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
            ### tune takes Inf for a search with no stopping rule
            no_improve    = if (isFALSE(no_improve)) Inf else no_improve,
            seed          = seed,
            allow_par     = allow_par,
            parallel_over = parallel_over
          )
        ),
        message = read_log
      )),
      error     = function(e) if (is.null(stopped)) stopped <<- rlang::cnd_entrace(e),
      interrupt = function(i) interrupted <<- TRUE
    ),
    ## An error tune did not recover from (before its loop starts):
    ## recorded above, nothing to keep
    error = function(e) NULL
  )

  if (interrupted) rlang::interrupt()

  ## A kind no unit contains, or a package missing on a worker, is raised
  ## again for the unit, not fallen back from
  if (!is.null(stopped) &&
      (debug || rlang::cnd_inherits(stopped, NEVER_CONTAINED) || missing_package_signal(stopped))) {

    stop(stopped)

  }

  ## An isolated fit's process that died fails the unit, as it does in the
  ## grid (check_tune_result()), even where tune dropped every candidate it
  ## killed and kept no note of it; under debug too, before the dropped
  ## candidate is reported as a failure with no record of why
  died <- if (isolated) isolated_deaths(on_daemons) else character(0)

  if (length(died) > 0) abort_process_died_in_tune(died[1])

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
  n_kept <- if (is.null(bayes_results)) 0L else max(bayes_results$.iter)

  ## Why the search ended, which the verbs count: an error first, since an
  ## error's search may also have gone without improvement. tune checks its
  ## rule after every iteration, the last included, so the rule ended the
  ## search only when it stopped short of the iterations asked for
  ended <- if (!is.null(cause)) {
    "failed"
  } else if (no_better && n_entered < iter) {
    "no_improve"
  } else {
    "budget"
  }

  if (n_kept > 0) {

    if (!is.null(cause)) {

      warn_in_unit("substitution",
                   "{search_cap} stopped early, after {n_kept} iteration{?s}, so the hyperparameters were chosen from those and {grid}: {cause}")

    }

    ## Candidates that failed in some folds leave their reasons in .notes,
    ## which the caller reads
    return(list(results = bayes_results, n_dropped = n_dropped, n_iter = n_entered, stop = ended))

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

  list(results = initial, n_dropped = n_dropped, n_iter = n_entered, stop = ended)

}

#' The Bayesian iterations a model's search runs
#'
#' @description
#' The iterations asked for (`configure()`'s `bayesian_iter`, or
#' `final_bayesian_iter` for `fit()`'s re-tune) are a floor: a model tuning
#' `n_params` parameters runs enough that its grid and its search together
#' try `BAYES_POINTS_PER_PARAM` points a parameter, when that is more. No
#' model searches less than asked, and the stopping rule
#' (`control_horizons(no_improve = )`) ends each search where it stops
#' improving. 0 stays 0: a search turned off stays off.
#'
#' @param iter Integer. The iterations asked for.
#' @param n_params Integer. The parameters the model tunes.
#' @param grid_size Integer. The grid's points.
#' @return Integer.
#' @keywords internal
#' @noRd
search_budget <- function(iter, n_params, grid_size) {

  if (iter == 0) return(0L)

  as.integer(max(iter, BAYES_POINTS_PER_PARAM * n_params - grid_size))

}

#' The tree's words for the models whose search the budget raises
#'
#' @param models Character. The configured models, or the members'.
#' @param iter,grid_size As [search_budget()] takes them.
#' @return Character(1), e.g. `"raised to 20 for cubist, 50 for xgboost: at
#'   least 10 points a tuned parameter"`, or `NULL` when no model's is.
#' @keywords internal
#' @noRd
raised_budgets <- function(models, iter, grid_size) {

  ### A model that tunes only its submodel is gridded whole, one fit a fold,
  ### and never searches (exhaustive_grid()), so its budget is not said
  models  <- Filter(function(m) length(setdiff(MODEL_SPECS[[m]]$tuned, MODEL_SPECS[[m]]$submodel)) > 0,
                    unique(models))
  budgets <- vapply(models, function(m) search_budget(iter, length(MODEL_SPECS[[m]]$tuned), grid_size),
                    integer(1))
  raised  <- budgets > iter

  if (!any(raised)) return(NULL)

  by_budget <- split(models[raised], factor(budgets[raised], levels = sort(unique(budgets[raised]))))

  paste0("raised to ",
         paste(vapply(names(by_budget), function(b) paste(b, "for", paste(by_budget[[b]], collapse = " and ")),
                      character(1)), collapse = ", "),
         ": at least ", BAYES_POINTS_PER_PARAM, " points a tuned parameter")

}

#' What a verb says when its Bayesian searches stopped early
#'
#' @description
#' The stopping rule (`control_horizons(no_improve = )`) is the user's to
#' see and to turn off, so a verb whose searches it stopped says how many
#' and how: one note on its log and one line in its tree, counted from each
#' configuration's or member's `bayes_stop`, rows resumed from a checkpoint
#' included (their settings record held the same limit).
#'
#' @param stops Character: each configuration's or member's `bayes_stop`,
#'   `NA` where no search ran.
#' @param no_improve The limit the searches ran under.
#' @param search Character(1). What the searches are, capitalized.
#' @param unit Character(1). What they ran in, singular.
#' @return Character(1), or `NULL` when no search stopped by the rule.
#' @keywords internal
#' @noRd
early_stop_line <- function(stops, no_improve, search, unit) {

  n_ran     <- sum(!is.na(stops))
  n_stopped <- sum(stops %in% "no_improve")

  if (n_stopped == 0) return(NULL)

  units <- if (n_ran == 1) unit else paste0(unit, "s")

  cli::format_inline(
    "{search} stopped early in {n_stopped} of the {n_ran} {units} it ran in, after {no_improve} iteration{?s} without improvement; {.code control_horizons(no_improve = FALSE)} runs every iteration, and a larger number waits longer."
  )

}

## ---------------------------------------------------------------------------
## apply_tune_ranges — the configured ranges on a model's parameter set
## ---------------------------------------------------------------------------

#' Set the configured ranges on a model's parameter set
#'
#' @description
#' Each range of `config$tuning$ranges` (`configure(tune_ranges = )`, merged
#' over `TUNE_RANGES`) the parameter set holds is set on its parameter, by
#' id, in dials' units ([dials::range_set()]). Both single-config runners
#' call it on the set they read from the workflow, before the bounds
#' ([finalize_bound()]): [dials::finalize()] then fills only an upper bound
#' still unknown, and the data cap narrows a range to what the folds hold.
#' The grid, the Bayesian search, [exhaustive_grid()] and the warm start
#' ([build_warmstart_grid()]) all read the set, so each searches the range.
#'
#' @param param_set A `parameters` object.
#' @param ranges Named list of `c(lower, upper)` by parameter id.
#' @return The parameter set.
#' @keywords internal
#' @noRd
apply_tune_ranges <- function(param_set, ranges) {

  for (id in intersect(names(ranges), param_set$id)) {

    param     <- param_set$object[[which(param_set$id == id)]]
    param_set <- update_parameter(param_set, id, dials::range_set(param, ranges[[id]]))

  }

  param_set

}

#' Replace one parameter of a parameter set
#'
#' @param param_set A `parameters` object.
#' @param id Character(1). The parameter's id.
#' @param param The `param` object to put in its place.
#' @return The parameter set.
#' @keywords internal
#' @noRd
update_parameter <- function(param_set, id, param) {

  do.call(stats::update, c(list(param_set), stats::setNames(list(param), id)))

}

## ---------------------------------------------------------------------------
## exhaustive_grid — a search space small enough to try whole
## ---------------------------------------------------------------------------

#' Every point of a search space that fits a budget
#'
#' @description
#' When every tuned parameter takes whole-number values over a known range
#' and the space costs no more fits than `budget`, the whole space as a grid;
#' otherwise `NULL`. A search whose budget covers the space is then
#' exhaustive: the grid is every point and no Bayesian stage runs, since
#' tune's search, once it has no value left to propose, errors inside its
#' loop rather than stopping. The fits are the points of every parameter but
#' the model's submodel (`MODEL_SPECS`' `submodel`), whose values one fit
#' scores, so plsr's whole component range is one fit a fold, and is
#' gridded whole at any budget.
#'
#' @param param_set A finalized `dials` parameter set.
#' @param budget Numeric(1). The fits the search would make: the grid's
#'   size and the Bayesian iterations together.
#' @param submodel Character(1) or `NULL`. The id of the parameter one fit
#'   scores at every value.
#' @return A tibble with one column per parameter (named by its id) and one
#'   row per point, or `NULL`.
#' @keywords internal
#' @noRd
exhaustive_grid <- function(param_set, budget, submodel = NULL) {

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

  names(values) <- param_set$id

  ## A submodel's values share their fit
  if (prod(lengths(values[setdiff(names(values), submodel)])) > budget) return(NULL)

  tibble::as_tibble(expand.grid(values, KEEP.OUT.ATTRS = FALSE))

}
