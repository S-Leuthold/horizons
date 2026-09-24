#' Fit Top Model Configurations
#'
#' @description
#' Re-tunes the top N configurations from `evaluate()` with warm-start
#' Bayesian optimization, generates out-of-fold (OOF) CV predictions for
#' downstream stacking, fits final deployable models, scores them on
#' `evaluate()`'s held-out test rows, and optionally trains uncertainty
#' quantification (UQ) components.
#'
#' This is the render layer — it owns all console output. The capture layer
#' (`fit_single_config()`) runs silently and returns structured results.
#'
#' The train/test partition (Split F) is `evaluate()`'s split, reused. Its
#' test rows are the only rows nothing was selected on: `evaluate()`'s
#' training rows chose the members (on `cv_<metric>`) and tuned the
#' warm-start parameters, so a partition drawn afresh would score the final
#' models partly on those rows. `fit()` checks that the split still indexes
#' the rows this object models: after dropping the rows whose outcome is `NA`
#' (the rule `evaluate()` applies) and the training rows `evaluate()` trimmed
#' as response outliers (`evaluation$response_trim$trimmed_ids`; see
#' `evaluate()`'s "Response outliers" section), the id and outcome columns
#' must match the split's, value for value and in order, or `fit()` aborts
#' with class `horizons_input_error`. The trim is reused, not recomputed, so
#' `fit()` trains without exactly the rows `evaluate()` left out and scores
#' on the same untrimmed test rows. Columns added since `evaluate()`, such
#' as a sibling response from `add_response()`, are carried into the fit.
#' The count of NA-outcome rows, and the response trim, are reported in the
#' console tree when `verbose = TRUE`. With UQ or AD on, the calibration set
#' is carved out of Split F's training part. After a response trim it is
#' carved out of the untrimmed training part (the trimmed rows are training
#' rows too, never test rows), and the trimmed rows are then dropped from the
#' fit rows only: split conformal needs calibration rows exchangeable with
#' the rows it will be asked about, extremes included, and a trimmed pool
#' undercovered them. AD shares that calibration set (D7). Calibration rows
#' never train, and test rows never calibrate. The degradation check then
#' compares the CV RPD with the test RPD over the test rows inside the
#' trim's fences, since the CV ran inside them; the reported test metrics
#' stay untrimmed.
#'
#' The members are the top `n_best` of the configurations `evaluate()` chose
#' `best_config` from, ranked by the same rule: the ones that succeeded or,
#' when none did, the pruned ones that carry a cross-validated value of
#' `metric`. When every member fell below `evaluate()`'s `prune_threshold`
#' (`evaluation$results$below_prune_threshold`), `fit()` warns with class
#' `horizons_below_threshold_warning`, naming the threshold and the members'
#' cross-validated RPD; that includes `bayesian_iter = 0`, where nothing is
#' pruned. When the members are pruned configurations the warning also
#' carries class `horizons_pruned_fallback_warning`. If every member fails,
#' `fit()` aborts with class `horizons_all_members_failed`, listing the
#' distinct error messages; the
#' per-member results travel on the condition as `results`, and
#' `models$results` keeps each member's `error_message` when some succeed.
#'
#' @section Cold start from one configuration:
#' With one configuration there is nothing for `evaluate()` to choose, so a
#' configured `horizons_data` whose `config$configs` has exactly one row can
#' go straight to `fit()` (#45). `fit()` then draws the train/test split
#' itself, by the draw `evaluate()` uses, so at the same `seed` it holds out
#' exactly the rows `evaluate()` would have, and applies a response trim
#' `validate()` requested as `evaluate()` does, on fences from the training
#' partition alone. With no parameters from
#' `evaluate()` to warm-start from, the re-tune starts from a space-filling
#' grid of `grid_size` points; the console tree says so, and
#' `models$results$warm_start` is `FALSE`. The degradation check needs
#' nothing from `evaluate()`, since it compares the test RPD with `fit()`'s
#' own cross-validation.
#'
#' The fit carries an unscreened evaluation record in place of
#' `evaluate()`'s: `evaluation$screened` is `FALSE`, the one configuration is
#' `best_config` with status `"not_evaluated"`, its metric and `cv_*` columns
#' are `NA`, and `runtime_secs` is 0. `evaluation$recipe` records the recipe
#' settings as `evaluate()` does, and `fit()` applies `evaluate()`'s checks
#' first: at least twice `cv_folds` rows with an observed outcome, and a
#' Savitzky-Golay window narrower than the spectrum. Re-fitting a
#' cold-started fit starts cold again. With more than one configuration
#' `fit()` aborts with class `horizons_input_error`: choosing among them is
#' what `evaluate()` is for.
#'
#' @param x A `horizons_eval` object (output of `evaluate()`), or a
#'   configured `horizons_data` with exactly one configuration, which `fit()`
#'   starts cold (see "Cold start from one configuration").
#' @param n_best Integer. Number of top configurations to re-tune. Default 5.
#'   A cold start has one.
#' @param metric Character or NULL. Metric for ranking the candidate
#'   configs, by bare name: one of `"rpd"`, `"rsq"`, `"rmse"`, `"rrmse"`,
#'   `"ccc"`, `"mae"`, or `fit()` aborts with class `horizons_input_error`.
#'   If NULL, uses the rank_metric from `evaluate()`; on a cold start, where
#'   there is nothing to rank and the metric is only recorded, the one a
#'   cold-started fit recorded, else `"rpd"`. Ranking reads the
#'   cross-validated value at each config's selected hyperparameters
#'   (`evaluation$results$cv_<metric>`), never the test-set column, so the
#'   test set stays held out from selection. Default NULL.
#' @param compute_uq Logical. Train UQ components (quantile model +
#'   conformal calibration). Default TRUE.
#' @param compute_ad Logical. Compute applicability-domain metadata (centroid +
#'   shrinkage covariance + held-out OOD thresholds) per config, so `predict()`
#'   can emit `.ad_distance` / `.ad_flag`. Shares the UQ calibration split.
#'   Default TRUE.
#' @param allow_par Logical. If `TRUE`, tune parallelises the CV folds
#'   inside each member's re-tune and OOF pass on whatever `future::plan()`
#'   the caller has registered; `fit()` never registers a plan. If the plan
#'   offers fewer than two workers, warns naming it and runs sequentially.
#'   `fit()` parallelises folds only in this version; a configs axis across
#'   the `n_best` members is gated on the fitted-object memory contract (a
#'   fitted object is hundreds of MB and would cross to workers). Default
#'   FALSE.
#' @param seed Integer. Random seed for the CV folds and, through
#'   `calib_split_seed()` (`seed + 1L`), for the calibration split. The
#'   train/test split is `evaluate()`'s, so it does not depend on this seed,
#'   except on a cold start, where `fit()` draws that split at this seed as
#'   `evaluate()` would. Default 307L.
#' @param verbose Logical. Print progress tree to console. Default TRUE.
#'
#' @return A `horizons_fit` object (inherits from `horizons_eval`,
#'   `horizons_data`) with `models$` slot populated. The slot includes
#'   `response_bound` (max training outcome times `RESPONSE_BOUND_MARGIN`,
#'   where the training rows are the ones the final models are fit on:
#'   `evaluate()`'s training part, less the calibration set when UQ or AD is
#'   on, plus the training rows a response trim left out, so the bound never
#'   sits below a value observed in the training partition),
#'   the deploy-time winsorization guardrail `predict()` applies to
#'   back-transformed point predictions, and `selection_present` (whether the
#'   training object carried a `$selection` from `select_training()`, which
#'   `predict()` reads when asked for conformal intervals). Called on a
#'   `horizons_ensemble`, it returns a `horizons_fit` whose `ensemble` slot
#'   is empty again, since the ensemble was built on the members it replaces.
#'   `models$results` records how each member's re-tune started:
#'   `warm_start` (`TRUE` from `evaluate()`'s parameters, `FALSE` from a
#'   space-filling grid) and `start_grid_size`.
#'
#' @export
fit <- function(x,
                n_best     = 5L,
                metric     = NULL,
                compute_uq = TRUE,
                compute_ad = TRUE,
                allow_par  = FALSE,
                seed       = 307L,
                verbose    = TRUE) {

  start_time <- Sys.time()

  ## -----------------------------------------------------------------------
  ## Step 0: Preflight validation
  ## -----------------------------------------------------------------------
  ## A horizons_eval carries evaluate()'s ranking and split. A configured
  ## object with one configuration has nothing to rank, so fit() starts cold
  ## (#45): it draws the split evaluate() would draw at this seed and records
  ## an unscreened evaluation in its place. A cold-started fit re-fits the
  ## same way, since there is still no screening to reuse.

  class_in   <- class(x)[1]
  cold_start <- !inherits(x, "horizons_eval") || isFALSE(x$evaluation$screened)

  if (cold_start && !inherits(x, "horizons_data")) {

    cli::cli_abort(c(
      "{.fn fit} needs a {.cls horizons_eval} from {.fn evaluate}, or a configured {.cls horizons_data} with one configuration.",
      "x" = "{.arg x} is {.obj_type_friendly {x}}."
    ), class = "horizons_input_error")

  }

  ## `metric` on either path, before anything is drawn or fitted. On the
  ## evaluate() path an unknown name used to surface as a missing cv_<metric>
  ## column with the advice to re-run evaluate(); on a cold start the record
  ## would fail validation only after every model had been fitted.
  if (!is.null(metric) &&
      (!rlang::is_string(metric) || !metric %in% VALID_RANK_METRICS)) {

    cli::cli_abort(c(
      "{.arg metric} must be one of {.val {VALID_RANK_METRICS}}.",
      "x" = "Got {.val {metric}}."
    ), class = "horizons_input_error")

  }

  if (cold_start) {

    cold         <- cold_start_evaluation(x, metric, seed)
    x$evaluation <- cold$evaluation

    if (!cold$stratified && verbose) {

      cat(paste0(
        "\u2502  ", cli::col_yellow("Stratified split failed, ",
                                     "retrying without strata"), "\n"
      ))

    }

  }

  ## validate_horizons_fit() (at return) checks the models slot, not the base
  ## data contract, so a column added after configure() without a role_map
  ## entry reached build_recipe()'s `outcome ~ .` as an unregistered
  ## predictor, undetected, until this call closed that gap (#24). Full
  ## stage: an object this far into the pipeline is expected to have unique,
  ## non-NA sample ids.

  x <- validate_horizons_data(x)

  eval_results <- x$evaluation$results

  if (is.null(eval_results) || nrow(eval_results) == 0) {

    rlang::abort(
      "No evaluation results found. Run `evaluate()` before `fit()`."
    )

  }

  ## -----------------------------------------------------------------------
  ## Step 0b: Parallel backend
  ## -----------------------------------------------------------------------
  ## The user owns the backend. Confirm there is one to run on, pin threads
  ## in this process before tune spawns anything, and never touch the plan.

  if (!rlang::is_bool(allow_par)) {

    rlang::abort("`allow_par` must be TRUE or FALSE.")

  }

  if (allow_par) {

    allow_par <- check_parallel_backend("fit()")

    if (allow_par) {

      warn_if_mirai_preferred()
      unpin_threads <- pin_parent_threads()
      on.exit(unpin_threads(), add = TRUE)

    }

  }

  ## Determine ranking metric. The name stays bare ("rpd"); ranking reads the
  ## cross-validated column cv_rpd, so members are chosen without touching
  ## the test set (#50). The test-set metrics on the leaderboard remain
  ## reported, and are honest precisely because they are not used here.
  rank_metric <- metric %||% x$evaluation$rank_metric %||% "rpd"

  ## The candidates are evaluate()'s best_config candidates: the successes
  ## or, when none succeeded, the pruned configs with a cv_<metric>. fit()
  ## used to keep successes only, so it refused an evaluation whose
  ## best_config was a pruned config (#38). A cold start's one configuration
  ## is the member as it stands: not_evaluated, with nothing to rank on.
  candidates  <- if (cold_start) {
    list(rows = eval_results, fallback = FALSE)
  } else {
    ranking_candidates(eval_results, rank_metric)
  }
  rank_column <- paste0("cv_", rank_metric)

  ## Nothing to fit: no candidates, or candidates none of which has a
  ## cv_<metric> value, which rank_configs_by_cv() would refuse unclassed.
  n_candidates <- nrow(candidates$rows)

  if (!cold_start &&
      (n_candidates == 0 || all(is.na(candidates$rows[[rank_column]])))) {

    cli::cli_abort(c(
      "No configuration in {.field evaluation$results} can be fitted.",
      "x" = if (n_candidates == 0) {
        "None succeeded, and no pruned configuration has a {.field {rank_column}} value."
      } else {
        "{n_candidates} succeeded, but none has a {.field {rank_column}} value."
      },
      "i" = "Re-run {.fn evaluate} to record the cross-validated metrics; when every configuration fails it aborts and lists the errors."
    ), class = "horizons_input_error")

  }

  ranked <- if (cold_start) candidates$rows else rank_configs_by_cv(candidates$rows, rank_metric)

  ## Cap n_best at available candidates. A cold start has one configuration
  ## by construction, so the default n_best is no request worth a note.
  n_available <- nrow(ranked)
  n_best      <- as.integer(n_best)

  if (n_best > n_available) {

    if (verbose && !cold_start) {

      cat(paste0(
        "\u2502  ", cli::col_yellow(
          "Requested n_best = ", n_best,
          " but only ", n_available,
          if (candidates$fallback) " pruned" else " successful",
          " configs available. Using ", n_available, "."
        ), "\n"
      ))

    }

    n_best <- n_available

  }

  top_configs <- ranked[seq_len(n_best), ]

  ## Say so when every member fell below evaluate()'s prune threshold. The
  ## pruned fallback is one case; the other is bayesian_iter = 0, where the
  ## gate skips nothing, so below-threshold configs are successes and nothing
  ## else would say that none cleared the bar (#38).
  warn_members_below_threshold(top_configs, fallback = candidates$fallback,
                               bayesian_iter = x$config$tuning$bayesian_iter)

  ## Extract references
  role_map     <- x$data$role_map
  outcome_col  <- role_map$variable[role_map$role == "outcome"]
  all_configs  <- x$config$configs
  tuning       <- x$config$tuning
  cv_folds     <- tuning$cv_folds

  ## configure() stores the screening and the final re-tune budgets
  ## separately; fit() used to pass the screening one to the re-tune, so the
  ## user-facing `final_bayesian_iter` did nothing (#46). Objects configured
  ## before that field existed fall back to the constant. The tree header
  ## prints this budget, the one that runs.
  final_bayesian_iter <- tuning$final_bayesian_iter %||% DEFAULT_FINAL_BAYES_ITER

  ## The recipe settings evaluate() ran with, or on a cold start the ones
  ## cold_start_evaluation() checked against the spectrum; see
  ## recipe_settings().
  recipe_cfg   <- recipe_settings(x)

  ## The rows evaluate() modelled: the same rule on the same table (#67).
  modelled  <- outcome_complete_rows(x$data$analysis, outcome_col)
  n_dropped <- modelled$n_dropped

  ## -----------------------------------------------------------------------
  ## Step 1: Data partitioning — Split F is evaluate()'s split
  ## -----------------------------------------------------------------------
  ## fit() used to draw its own Split F, at seed + 1 since #50. Measured over
  ## 200 seeds at n = 250, that draw took a median 79 % of its test rows from
  ## evaluate()'s training rows, which chose the members (cv_<metric>) and
  ## tuned the warm-start parameters, so fit()'s test metrics were optimistic
  ## through selection. evaluate()'s test rows are the only rows nothing was
  ## selected on, so fit() is scored on them.
  ##
  ## Reusing the split is sound only if its row positions still name the
  ## same samples with the same outcomes, so that is what is checked: the id
  ## and outcome columns, identical values in the same order. Other columns
  ## may legitimately have changed; add_response() can add a sibling response
  ## to an evaluated object. set_analysis() refuses a promoted object, so a
  ## mismatch here means the rows or outcomes were changed some other way, or
  ## the object was built by hand; refuse it rather than score the wrong rows.
  ## The split is then pointed at the current table, so the fit sees the
  ## object's columns as they are now. On a cold start the split was drawn in
  ## Step 0 from these rows, by the draw evaluate() uses, so it passes.

  split_F <- x$evaluation$split

  if (!inherits(split_F, "rsplit")) {

    cli::cli_abort(c(
      "{.fn fit} scores on {.fn evaluate}'s split, and this object has none.",
      "x" = "{.field evaluation$split} is {.obj_type_friendly {split_F}}, not an {.cls rsplit}.",
      "i" = "Run {.fn evaluate} on the object before {.fn fit}."
    ), class = "horizons_input_error")

  }

  id_col <- id_column(role_map)

  ## evaluate()'s response trim (#77) dropped training rows from the split's
  ## data, so the same rows are dropped here before the comparison. They are
  ## the rows evaluate() recorded, not a fresh trim: fences recomputed here
  ## could fall on another set. An evaluation without the record (no trim
  ## requested, or evaluated before it existed) trimmed nothing.
  trimmed_ids   <- x$evaluation$response_trim$trimmed_ids %||% character(0)
  is_trimmed    <- modelled$data[[id_col]] %in% trimmed_ids
  modelled_rows <- if (length(trimmed_ids) > 0) {
    modelled$data[!is_trimmed, , drop = FALSE]
  } else {
    modelled$data
  }

  ## The trimmed rows themselves. They belong to the training partition,
  ## never the test part, and no model is fitted on them; the calibration
  ## pool and the response bound below take them back in.
  trimmed_rows <- if (length(trimmed_ids) > 0) {
    modelled$data[is_trimmed, , drop = FALSE]
  } else {
    NULL
  }

  ## The fences the rows were trimmed by, for the degradation check: the CV
  ## runs inside them and the test rows are untrimmed, so the check compares
  ## like with like only within them (see fit_single_config()).
  response_fences <- if (length(trimmed_ids) > 0) {
    c(lower = x$evaluation$response_trim$lower,
      upper = x$evaluation$response_trim$upper)
  } else {
    NULL
  }

  if (!identical(split_F$data[[id_col]], modelled_rows[[id_col]]) ||
      !identical(split_F$data[[outcome_col]], modelled_rows[[outcome_col]])) {

    trim_note <- if (length(trimmed_ids) > 0) {
      paste0(" once the ", length(trimmed_ids), " training rows evaluate() trimmed are left out")
    } else {
      ""
    }

    cli::cli_abort(c(
      "{.fn evaluate}'s split does not index the rows this object models.",
      "x" = "{.field evaluation$split} was drawn on {nrow(split_F$data)} row{?s}; the analysis table has {nrow(modelled_rows)} with an observed {.field {outcome_col}}{trim_note}, and their {.field {id_col}} or {.field {outcome_col}} values differ.",
      "i" = "The rows or outcomes changed after {.fn evaluate}, or the object was built by hand.",
      "i" = "Re-run {.fn evaluate} on the object as it is now."
    ), class = "horizons_input_error")

  }

  split_F$data <- modelled_rows

  ## Rows an earlier version's validate() removed on whole-table fences are
  ## gone from both parts, so this fit's test metrics exclude them too.
  legacy_removed <- legacy_response_removals(x)

  if (length(legacy_removed) > 0) {

    warn_legacy_response_removals(legacy_removed, "fit")

  }

  train_F <- rsample::training(split_F)
  test_F  <- rsample::testing(split_F)
  n_train <- nrow(train_F)
  n_test  <- nrow(test_F)

  ## Calibration partitioning: split train_F into train_Fit / calib_Fit.
  ## UQ and AD share this one held-out split (D7) \u2014 both calibrate on calib_Fit
  ## and never train on it. Built whenever either capability is requested.
  ## Seeded on its own, so the calibration rows depend on `seed` and train_F
  ## alone, not on RNG state an earlier draw left behind.
  ##
  ## After a response trim (#77) the pool is the untrimmed training part,
  ## train_F plus the trimmed rows. Split conformal needs calibration rows
  ## exchangeable with the rows it will be asked about, and those include the
  ## extremes the trim left out: a trimmed pool undercovered them, silently,
  ## since the OOF coverage is read on trimmed rows too. The trimmed rows are
  ## then dropped from the fit rows only. Calibration rows still never train,
  ## and test rows never calibrate. Without a trim the pool is train_F.
  calib_data <- NULL
  calib_pool <- if (is.null(trimmed_rows)) train_F else dplyr::bind_rows(train_F, trimmed_rows)

  if (compute_uq || compute_ad) {

    set.seed(calib_split_seed(seed))

    split_C <- tryCatch(
      rsample::initial_split(calib_pool, prop = CALIB_PROP, strata = dplyr::all_of(outcome_col)),
      error = function(e) {

        rsample::initial_split(calib_pool, prop = CALIB_PROP)

      }
    )

    train_Fit  <- rsample::training(split_C)
    calib_data <- rsample::testing(split_C)

    if (!is.null(trimmed_rows)) {

      train_Fit <- train_Fit[!train_Fit[[id_col]] %in% trimmed_ids, , drop = FALSE]

    }

    ## Guard: minimum calibration size. Too small disables BOTH capabilities
    ## that needed it \u2014 neither UQ nor AD can calibrate on an undersized set.
    if (nrow(calib_data) < N_CALIB_MIN) {

      if (verbose) {

        disabled <- paste(c(if (compute_uq) "UQ", if (compute_ad) "AD"),
                          collapse = " and ")

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Calibration set too small (", nrow(calib_data),
            " < ", N_CALIB_MIN, "). Disabling ", disabled, "."
          ), "\n"
        ))

      }

      compute_uq <- FALSE
      compute_ad <- FALSE
      train_Fit  <- train_F
      calib_data <- NULL

    }

  } else {

    train_Fit <- train_F

  }

  ## -----------------------------------------------------------------------
  ## Step 2: Create CV resamples from train_Fit
  ## -----------------------------------------------------------------------
  ## Seeded here: with the split reused, no draw precedes this one when UQ
  ## and AD are off, so the folds would otherwise follow the caller's RNG.

  set.seed(seed)

  cv_resamples <- tryCatch(
    rsample::vfold_cv(train_Fit, v = cv_folds, strata = dplyr::all_of(outcome_col)),
    error = function(e) {

      if (verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Stratified CV failed, retrying without strata"
          ), "\n"
        ))

      }

      rsample::vfold_cv(train_Fit, v = cv_folds)

    }
  )

  ## -----------------------------------------------------------------------
  ## Step 3: Tree header
  ## -----------------------------------------------------------------------

  if (verbose) {

    cat("\n")
    cat(paste0("\u250C fit ",
               paste(rep("\u2500", 57), collapse = ""), "\n"))
    cat("\u2502\n")

    if (n_dropped > 0) {

      cat(paste0("\u2502  ",
                 cli::col_yellow("Dropped ", n_dropped,
                                  " rows with NA outcome"), "\n"))

    }

    if (cold_start) {

      cat("\u2502  Cold start: 1 configuration, not screened by evaluate()\n")

    } else {

      cat(paste0(
        "\u2502  Re-tuning top ", n_best, " of ",
        nrow(eval_results), " configurations\n"
      ))

    }

    cat(paste0(
      "\u2502  Split: ", n_train, " train / ", n_test, " test (",
      if (cold_start) "the rows evaluate() holds out at this seed" else "evaluate()'s held-out rows",
      ")\n"
    ))

    render_response_trim(x$evaluation$response_trim, legacy_removed)

    if (compute_uq) {

      n_trim_calib <- sum(calib_data[[id_col]] %in% trimmed_ids)

      cat(paste0(
        "\u2502  UQ calibration: ", nrow(train_Fit), " fit / ",
        nrow(calib_data), " calibration",
        if (!is.null(trimmed_rows)) {
          paste0(" (drawn from the untrimmed training part; ", n_trim_calib,
                 " trimmed row", if (n_trim_calib != 1) "s", " in it)")
        } else "",
        "\n"
      ))

    }

    cat(paste0(
      "\u2502  CV: ", cv_folds, "-fold stratified on ", outcome_col, "\n"
    ))
    cat(paste0(
      "\u2502  Bayesian: ", final_bayesian_iter,
      if (cold_start) " iterations from a space-filling grid\n" else " iterations with warm-start\n"
    ))
    cat("\u2502\n")

  }

  ## -----------------------------------------------------------------------
  ## Step 4: Config loop
  ## -----------------------------------------------------------------------

  results_list <- list()

  for (i in seq_len(n_best)) {

    top_row   <- top_configs[i, ]
    config_id <- top_row$config_id

    ## Look up full config row from original configs table
    cfg <- all_configs[all_configs$config_id == config_id, ]

    ## Pretty config description
    model_name <- MODEL_DISPLAY_NAMES[cfg$model] %||% cfg$model
    desc_parts <- c(model_name, cfg$transformation, cfg$preprocessing,
                    cfg$feature_selection)

    if (!is.na(cfg$covariates)) {

      desc_parts <- c(desc_parts, paste0("+", cfg$covariates))

    }

    config_desc <- paste(desc_parts, collapse = " + ")
    is_last     <- i == n_best
    branch      <- if (is_last) "\u2514\u2500" else "\u251C\u2500"
    cont        <- if (is_last) "   " else "\u2502  "

    ## Render config start
    if (verbose) {

      cat(paste0(
        "\u2502  ", branch, " [", i, "/", n_best, "] ", config_desc, "\n"
      ))

    }

    ## Extract best_params from evaluate. NULL on a cold start, so the
    ## re-tune starts from build_warmstart_grid()'s space-filling fallback.
    best_params_eval <- top_row$best_params[[1]]

    ## Call capture layer
    config_result <- fit_single_config(
      config_row          = cfg,
      split_F             = split_F,
      cv_resamples        = cv_resamples,
      calib_data          = calib_data,
      train_data          = train_Fit,
      role_map            = role_map,
      best_params_eval    = best_params_eval,
      final_bayesian_iter = final_bayesian_iter,
      grid_size           = tuning$grid_size,
      compute_uq          = compute_uq,
      compute_ad          = compute_ad,
      allow_par           = allow_par,
      seed                = seed,
      sg_window           = recipe_cfg$sg_window,
      pca_threshold       = recipe_cfg$pca_threshold,
      response_fences     = response_fences
    )

    results_list[[i]] <- config_result

    ## Render result
    if (verbose) {

      ## A re-tune with no usable parameters to centre on starts from a
      ## space-filling grid. On a cold start that is by design; on the
      ## evaluate() path it means evaluate()'s parameters were unusable.
      if (isFALSE(config_result$warm_start)) {

        start_text <- paste0(
          if (cold_start) "Cold start: no warm-start parameters" else "No usable warm-start parameters from evaluate()",
          "; space-filling grid of ", config_result$start_grid_size, " points"
        )

        cat(paste0(
          "\u2502  ", cont, "\u251C\u2500 ",
          if (cold_start) start_text else cli::col_yellow(start_text), "\n"
        ))

      }

      if (config_result$status == "success") {

        ## Test metrics
        tm <- config_result$test_metrics
        cat(paste0(
          "\u2502  ", cont, "\u251C\u2500 Test: ",
          "RPD = ", round(tm$rpd, 2),
          ", R\u00B2 = ", round(tm$rsq, 2),
          ", RMSE = ", round(tm$rmse, 3), "\n"
        ))

        ## CV metrics comparison
        cv_met <- config_result$cv_metrics
        cv_rpd <- cv_met[cv_met$.metric == "rpd", ]
        cv_rmse <- cv_met[cv_met$.metric == "rmse", ]

        if (nrow(cv_rpd) == 1 && nrow(cv_rmse) == 1) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 CV:   ",
            "RPD = ", round(cv_rpd$mean, 2),
            " \u00B1 ", round(cv_rpd$std_err, 2),
            ", RMSE = ", round(cv_rmse$mean, 3),
            " \u00B1 ", round(cv_rmse$std_err, 3), "\n"
          ))

        }

        ## Degradation flag
        if (config_result$degraded) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_yellow("\u26A0 DEGRADED: ",
                            config_result$degraded_reason), "\n"
          ))

        }

        ## UQ coverage
        if (!is.null(config_result$uq)) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 UQ coverage: ",
            round(config_result$uq$oof_coverage * 100, 1), "% ",
            "(target ", round(config_result$uq$level_default * 100, 0),
            "%, width = ", round(config_result$uq$mean_width, 3), ")\n"
          ))

        }

      } else {

        cat(paste0(
          "\u2502  ", cont, "\u251C\u2500 ",
          cli::col_red("FAILED: ", config_result$error_message), "\n"
        ))

      }

      ## Warnings
      if (!is.null(config_result$warnings)) {

        for (w in config_result$warnings) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_yellow(w), "\n"
          ))

        }

      }

      ## Runtime
      cat(paste0(
        "\u2502  ", cont, "\u2514\u2500 ",
        if (config_result$status == "failed") {
          cli::col_red("\u2717")
        } else {
          cli::col_green("\u2713")
        },
        " ", round(config_result$runtime_secs, 1), "s\n"
      ))

    }

    ## Memory cleanup after each config
    gc(verbose = FALSE)

  }

  ## -----------------------------------------------------------------------
  ## Step 5: Assemble results
  ## -----------------------------------------------------------------------

  ## Collect workflows (named by config_id)
  workflows_list <- list()

  for (res in results_list) {

    if (res$status == "success" && !is.null(res$fitted_workflow)) {

      workflows_list[[ res$config_id ]] <- res$fitted_workflow

    }

  }

  ## Row-bind CV predictions across configs
  cv_pred_parts <- purrr::compact(purrr::map(results_list, "cv_predictions"))
  all_cv_predictions <- if (length(cv_pred_parts) > 0) {

    dplyr::bind_rows(cv_pred_parts)

  } else {

    NULL

  }

  ## Build results tibble
  results_tibble <- purrr::map_dfr(results_list, function(res) {

    tm <- res$test_metrics
    cv <- res$cv_metrics

    ## Extract CV summary metrics
    cv_rmse <- if (!is.null(cv)) cv[cv$.metric == "rmse", ] else NULL
    cv_rpd  <- if (!is.null(cv)) cv[cv$.metric == "rpd", ]  else NULL

    tibble::tibble(
      config_id       = res$config_id,
      status          = res$status,
      degraded        = res$degraded %||% NA,
      degraded_reason = res$degraded_reason %||% NA_character_,
      rmse            = if (!is.null(tm)) tm$rmse  else NA_real_,
      rrmse           = if (!is.null(tm)) tm$rrmse else NA_real_,
      rsq             = if (!is.null(tm)) tm$rsq   else NA_real_,
      ccc             = if (!is.null(tm)) tm$ccc   else NA_real_,
      rpd             = if (!is.null(tm)) tm$rpd   else NA_real_,
      mae             = if (!is.null(tm)) tm$mae   else NA_real_,
      cv_rmse_mean    = if (!is.null(cv_rmse) && nrow(cv_rmse) == 1) cv_rmse$mean    else NA_real_,
      cv_rmse_se      = if (!is.null(cv_rmse) && nrow(cv_rmse) == 1) cv_rmse$std_err else NA_real_,
      cv_rpd_mean     = if (!is.null(cv_rpd)  && nrow(cv_rpd)  == 1) cv_rpd$mean     else NA_real_,
      cv_rpd_se       = if (!is.null(cv_rpd)  && nrow(cv_rpd)  == 1) cv_rpd$std_err  else NA_real_,
      best_params     = list(res$best_params),
      warm_start      = res$warm_start %||% NA,
      start_grid_size = res$start_grid_size %||% NA_integer_,
      error_message   = res$error_message %||% NA_character_,
      runtime_secs    = res$runtime_secs
    )

  })

  ## Every member failed. Abort here with the members' errors; carrying on
  ## reached validate_horizons_fit(), which refused the empty workflows slot
  ## with a structural message that said nothing about why.
  if (length(workflows_list) == 0) {

    abort_all_members_failed(results_tibble)

  }

  ## Build row_index: .row → id mapping from train_Fit
  row_index <- tibble::tibble(
    .row      = seq_len(nrow(train_Fit)),
    sample_id = train_Fit[[id_col]]
  )

  ## Collect UQ bundles (named by config_id)
  uq_list <- NULL

  if (compute_uq) {

    uq_list <- list()

    for (res in results_list) {

      if (res$status == "success" && !is.null(res$uq)) {

        uq_list[[ res$config_id ]] <- res$uq

      }

    }

    if (length(uq_list) == 0) uq_list <- NULL

  }

  ## Collect AD bundles (named by config_id) — same shape as uq_list
  ad_list <- NULL

  if (compute_ad) {

    ad_list <- list()

    for (res in results_list) {

      if (res$status == "success" && !is.null(res$ad)) {

        ad_list[[ res$config_id ]] <- res$ad

      }

    }

    if (length(ad_list) == 0) ad_list <- NULL

  }

  ## -----------------------------------------------------------------------
  ## Step 6: Populate models$ slot and promote class
  ## -----------------------------------------------------------------------

  total_runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  ## Persist the facts fit() computed so predict() reads them rather than
  ## re-deriving. workflows_list is named in best-first order, so its first name
  ## is the top fitted config. predictor_schema is the training-axis predictor
  ## column set predict() validates new_data against (no need to re-introspect a
  ## butchered recipe at predict time).
  best_config      <- if (length(workflows_list) > 0) names(workflows_list)[1] else NA_character_
  predictor_schema <- role_map$variable[role_map$role == "predictor"]

  ## Deploy-time guardrail bound: predictions are winsorized to this value in
  ## predict_one_config(). max-times-margin (not a quantile) — the bound should
  ## permit modest extrapolation and catch only the physically absurd. Taken
  ## over train_Fit, the rows the final models are fit on, so neither Split
  ## F's test rows nor the calibration rows shape it (#68), plus the rows a
  ## response trim left out of them (#77): those are training-partition rows
  ## too, and a bound under values actually observed there would clamp the
  ## very extremes the trim set aside.
  response_bound <- max(c(train_Fit[[outcome_col]], trimmed_rows[[outcome_col]]),
                        na.rm = TRUE) * RESPONSE_BOUND_MARGIN

  ## Did the training rows come from select_training()? If so the calibration
  ## split below was drawn from rows chosen for proximity to the targets, so
  ## conformal exchangeability with arbitrary prediction data does not hold.
  ## predict() reads this flag to say so; nothing else changes.
  selection_present <- !is.null(x$selection)

  ## Re-fitting an ensembled object replaces the members the ensemble was
  ## built on, so the ensemble goes with them.
  x <- reset_slots(x, "ensemble")

  x$models <- list(
    workflows         = workflows_list,
    n_models          = length(workflows_list),
    best_config       = best_config,
    rank_metric       = rank_metric,
    predictor_schema  = predictor_schema,
    response_bound    = response_bound,
    cv_predictions    = all_cv_predictions,
    results           = results_tibble,
    split             = split_F,
    row_index         = row_index,
    uq                = uq_list,
    ad                = ad_list,
    selection_present = selection_present,
    timestamp         = Sys.time(),
    runtime_secs      = total_runtime
  )

  class(x) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  ## Certify the contract before returning: structural checks only (see
  ## validate_horizons_fit), so every fit() return matches invariants I6/I7
  ## (workflow keys subset config ids; uq keys subset workflow keys) and the
  ## response_bound guardrail contract.
  x <- validate_horizons_fit(x)

  ## -----------------------------------------------------------------------
  ## Step 7: Tree footer / summary
  ## -----------------------------------------------------------------------

  if (verbose) {

    n_success  <- sum(results_tibble$status == "success")
    n_failed   <- sum(results_tibble$status == "failed")
    n_degraded <- sum(results_tibble$degraded == TRUE, na.rm = TRUE)

    cat("\u2502\n")
    cat(paste0("\u2502  Summary\n"))
    cat(paste0(
      "\u2502  \u251C\u2500 Successful: ", n_success, " / ", n_best, "\n"
    ))

    if (n_degraded > 0) {

      degraded_ids <- results_tibble$config_id[results_tibble$degraded == TRUE &
                                                  !is.na(results_tibble$degraded)]
      cat(paste0(
        "\u2502  \u251C\u2500 ",
        cli::col_yellow("Degraded: ", n_degraded, " / ", n_best,
                         " (", paste(degraded_ids, collapse = ", "), ")"),
        "\n"
      ))

    }

    if (n_failed > 0) {

      cat(paste0(
        "\u2502  \u251C\u2500 ",
        cli::col_red("Failed: ", n_failed, " / ", n_best), "\n"
      ))

    }

    ## The CV-selected member's test RPD. The best test RPD across members
    ## would be a best-of-N on the held-out rows, and could name a different
    ## config from models$best_config. A cold start selected nothing.
    if (!is.na(best_config)) {

      best_rpd <- results_tibble$rpd[results_tibble$config_id == best_config]

      cat(paste0(
        "\u2502  \u251C\u2500 ", if (cold_start) "Cold-started: " else "CV-selected: ",
        best_config,
        " (test RPD ", round(best_rpd, 2), ")\n"
      ))

    }

    ## Runtime
    if (total_runtime < 60) {

      time_str <- paste0(round(total_runtime, 1), "s")

    } else {

      time_str <- paste0(round(total_runtime / 60, 1), " min")

    }

    cat(paste0("\u2502  \u2514\u2500 Runtime: ", time_str, "\n"))
    cat("\u2502\n")
    cat(paste0(
      "\u2514\u2500 Class: ", class_in, " \u2192 horizons_fit\n"
    ))
    cat(paste0(
      paste(rep("\u2500", 62), collapse = ""), "\n"
    ))

  }

  x

}

## ---------------------------------------------------------------------------
## cold_start_evaluation \u2014 the evaluation record fit() writes without evaluate()
## ---------------------------------------------------------------------------

#' Build the evaluation record for fit()'s cold start
#'
#' @description
#' With one configuration there is nothing for `evaluate()` to screen, so
#' `fit()` starts from the configured object (#45). This checks that the
#' object can start cold, draws the train/test split `evaluate()` would draw
#' at the same `seed` ([draw_eval_split()], after the same sample-size floor
#' and the same Savitzky-Golay window check, [evaluation_recipe()]), and
#' returns the record `fit()` stores in `x$evaluation` in place of
#' `evaluate()`'s. The record carries every key [validate_horizons_eval()]
#' requires, with `screened = FALSE`, and `recipe` and `response_trim` as
#' `evaluate()` records them (the split is trimmed by the same helper, #77);
#' it leaves out the run provenance (`workers`, `parallelize_over`),
#' since no `evaluate()` ran. Its one
#' results row has status `"not_evaluated"`, `NA` metrics and `cv_*`
#' columns, and `NULL` `best_params`, which sends the re-tune to
#' [build_warmstart_grid()]'s space-filling fallback.
#'
#' @param x A configured `horizons_data`, or a fit that started cold.
#' @param metric The `metric` passed to `fit()`, already checked. `NULL`
#'   keeps the `rank_metric` a cold-started fit recorded, or `"rpd"`.
#'   Recorded as `rank_metric`; with one configuration it ranks nothing.
#' @param seed The `seed` passed to `fit()`.
#' @param call The call the conditions are attributed to. Default: the
#'   caller, `fit()`.
#' @return List with `evaluation` (the record) and `stratified` (`FALSE` when
#'   the split fell back to unstratified). Aborts with class
#'   `horizons_input_error` when the object has no configuration or more than
#'   one, too few rows have an observed outcome, the window is at least as
#'   wide as the spectrum, or `validate()`'s response-trim request is for
#'   another outcome ([response_trim_request()]).
#' @keywords internal
#' @noRd
cold_start_evaluation <- function(x, metric, seed, call = rlang::caller_env()) {

  configs   <- x$config$configs
  n_configs <- if (is.data.frame(configs)) nrow(configs) else 0L

  if (n_configs == 0) {

    cli::cli_abort(c(
      "There is no configuration to fit.",
      "i" = "Run {.fn configure} first."
    ), class = "horizons_input_error", call = call)

  }

  if (n_configs > 1) {

    cli::cli_abort(c(
      "{.fn fit} can start without {.fn evaluate} only from a single configuration, and this object has {n_configs} configurations.",
      "i" = "Choosing among configurations is what {.fn evaluate} is for. Run it first, and {.fn fit} re-tunes the best of them."
    ), class = "horizons_input_error", call = call)

  }

  ## fit() has checked `metric`. A cold re-fit keeps the metric its record
  ## carries, as a fit of an evaluated object keeps evaluate()'s.
  rank_metric <- metric %||% x$evaluation$rank_metric %||% "rpd"

  ## The rows evaluate() would model, and the floor it applies to them
  role_map    <- x$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]
  cv_folds    <- x$config$tuning$cv_folds
  modelled    <- outcome_complete_rows(x$data$analysis, outcome_col)
  n_modelled  <- nrow(modelled$data)

  if (n_modelled < cv_folds * 2) {

    cli::cli_abort(c(
      "Too few rows to fit: {n_modelled} ha{?s/ve} an observed {.field {outcome_col}}.",
      "i" = "{.fn fit} needs at least {cv_folds * 2} (twice {.field cv_folds}), the floor {.fn evaluate} applies."
    ), class = "horizons_input_error", call = call)

  }

  ## evaluate()'s next check: the Savitzky-Golay window has to fit the
  ## spectrum (#62). The record it returns is the one evaluate() stores, and
  ## fit() builds its recipe from the same settings.
  recipe_record <- evaluation_recipe(x, call = call)

  ## The response trim evaluate() applies after the same draw (#77): a trim
  ## validate() requested runs on this split's training partition by the
  ## same helper, so a cold start leaves out the rows evaluate() would. The
  ## request is read first, as evaluate() reads it, so one that cannot apply
  ## is refused before anything is drawn.
  trim_request <- response_trim_request(x, outcome_col, call = call)

  drawn <- draw_eval_split(modelled$data, outcome_col, seed)

  trimmed <- trim_training_responses(drawn$split, outcome_col, trim_request,
                                     id_col = id_column(role_map))
  split   <- trimmed$split

  ## The shape of an evaluate() results row (create_failed_result()'s
  ## columns less scoring_schema, since nothing was scored), all unmeasured
  results <- tibble::tibble(
    config_id             = configs$config_id,
    status                = "not_evaluated",
    below_prune_threshold = NA,
    prune_threshold       = NA_real_,
    rmse    = NA_real_, rrmse    = NA_real_, rsq    = NA_real_,
    ccc     = NA_real_, rpd      = NA_real_, mae    = NA_real_,
    cv_rmse = NA_real_, cv_rrmse = NA_real_, cv_rsq = NA_real_,
    cv_ccc  = NA_real_, cv_rpd   = NA_real_, cv_mae = NA_real_,
    best_params           = list(NULL),
    error_message         = NA_character_,
    warnings              = list(NULL),
    runtime_secs          = NA_real_
  )

  list(
    evaluation = list(
      results       = results,
      best_config   = configs$config_id,
      rank_metric   = rank_metric,
      screened      = FALSE,
      split         = split,
      n_train       = nrow(rsample::training(split)),
      n_test        = nrow(rsample::testing(split)),
      response_trim = trimmed$record,
      recipe        = recipe_record,
      runtime_secs  = 0,
      timestamp     = Sys.time()
    ),
    stratified = drawn$stratified
  )

}

## ---------------------------------------------------------------------------
## abort_all_members_failed \u2014 no member fitted, with the reasons attached
## ---------------------------------------------------------------------------

#' Abort because every member fit() re-tuned failed
#'
#' @description
#' The message lists the distinct error messages (the first three, each with
#' the members that raised it, and a count of the rest), and the per-member
#' results table travels on the condition as `results`, as it does for
#' `evaluate()`'s `horizons_all_configs_failed`.
#'
#' @param results `fit()`'s per-member results tibble, with `error_message`.
#' @param call The call the condition is attributed to. Default: the caller,
#'   `fit()`.
#' @return Never returns; aborts with class `horizons_all_members_failed`.
#' @keywords internal
#' @noRd
abort_all_members_failed <- function(results, call = rlang::caller_env()) {

  n_members <- nrow(results)

  ## distinct_config_errors() returns the bullets brace-escaped, so upstream
  ## error text cannot be read as a cli template.
  cli::cli_abort(c(
    "All {n_members} configuration{?s} {.fn fit} re-tuned failed, so there is no model to return.",
    distinct_config_errors(results),
    "i" = "The per-member results, error messages included, are on this condition as {.field results}. Recover them with {.code rlang::last_error()$results}."
  ), class = "horizons_all_members_failed", results = results, call = call)

}

## ---------------------------------------------------------------------------
## warn_members_below_threshold \u2014 no member cleared the prune threshold
## ---------------------------------------------------------------------------

#' Warn when every member fell below evaluate()'s prune threshold
#'
#' @description
#' `evaluate()` records the prune gate's reading on every row as
#' `below_prune_threshold` whenever `prune = TRUE`, apart from the status,
#' which says only whether Bayesian refinement was skipped (#38). `fit()`
#' warns, with class `horizons_below_threshold_warning`, when every member it
#' is about to fit fell below the threshold, naming the threshold and each
#' member's cross-validated RPD. Without this, `bayesian_iter = 0` would fit
#' below-threshold configurations silently, since nothing is pruned there.
#' When the members are pruned configurations (the fallback, because none
#' succeeded) the one warning also carries class
#' `horizons_pruned_fallback_warning` and says so.
#'
#' A pruned row counts as below the threshold whether or not it carries the
#' column, since the gate put it there. A row with no reading (`prune =
#' FALSE`, or written before the column existed) does not, so one such member
#' keeps the warning from firing.
#'
#' @param members The member rows (`evaluation$results` shape).
#' @param fallback Logical. Whether the members are pruned configurations
#'   because none succeeded.
#' @param bayesian_iter The configured `bayesian_iter`
#'   (`x$config$tuning$bayesian_iter`), which decides how the warning
#'   explains unpruned members; `NULL` when unknown.
#' @return Invisibly `NULL`. Called for the warning.
#' @keywords internal
#' @noRd
warn_members_below_threshold <- function(members, fallback, bayesian_iter = NULL) {

  ## Columns are read by name: results from before they existed, or built by
  ## hand, lack them, and `$` on a tibble without the column warns.
  column_or <- function(col, fill) {
    if (col %in% names(members)) members[[col]] else rep(fill, nrow(members))
  }

  n_members <- nrow(members)
  below     <- column_or("below_prune_threshold", NA) | members$status %in% "pruned"

  if (n_members == 0 || !isTRUE(all(below))) return(invisible(NULL))

  thresholds <- unique(stats::na.omit(column_or("prune_threshold", NA_real_)))

  threshold_text <- if (length(thresholds) == 0) {
    "the prune threshold (not recorded on these rows)"
  } else {
    paste0("the prune threshold of ", paste(format(thresholds), collapse = " and "))
  }

  cv_rpd  <- column_or("cv_rpd", NA_real_)
  cv_text <- paste0(members$config_id, " ",
                    ifelse(is.na(cv_rpd), "NA", formatC(cv_rpd, digits = 2, format = "f")),
                    collapse = ", ")

  header <- if (fallback) {
    "No configuration passed {.fn evaluate}'s prune gate, so {.fn fit} is fitting pruned configurations."
  } else {
    "Every configuration {.fn fit} is fitting fell below {.fn evaluate}'s prune threshold."
  }

  ## The cause of unpruned below-threshold members depends on the configured
  ## bayesian_iter. At 0 there is nothing to skip. Above 0 the gate prunes a
  ## below-threshold config, so a success marked below the threshold was
  ## scored with no Bayesian stage (resumed from a bayesian_iter = 0 run's
  ## checkpoints, say), and bayesian_iter = 0 is not the cause to name.
  why <- if (fallback) {
    "Pruned configurations skipped Bayesian optimization; {.fn evaluate} chose {.field best_config} from them for the same reason."
  } else if (isTRUE(bayesian_iter == 0)) {
    "None was pruned because there was no Bayesian stage to skip ({.code bayesian_iter = 0}), so they ranked as successes."
  } else if (length(bayesian_iter) == 1 && isTRUE(bayesian_iter > 0)) {
    "None was pruned, so they ranked as successes, although the configured {.code bayesian_iter = {bayesian_iter}} prunes a configuration below the threshold: these rows were scored with no Bayesian stage, as rows resumed from a {.code bayesian_iter = 0} run's checkpoints are."
  } else {
    "None was pruned, so they ranked as successes."
  }

  ## The config ids and numbers are the package's, but the text is built
  ## outside cli, so it is escaped rather than trusted as a template.
  cli::cli_warn(c(
    "!" = header,
    "i" = cli_escape(paste0(
      "Their grid-search RPD fell below ", threshold_text,
      ". Cross-validated RPD: ", cv_text, "."
    )),
    "i" = why,
    "i" = "Check their metrics before relying on the fit."
  ), class = c(if (fallback) "horizons_pruned_fallback_warning",
               "horizons_below_threshold_warning"))

  invisible(NULL)

}

## ---------------------------------------------------------------------------
## calib_split_seed \u2014 Split C's seed, derived from the user's seed
## ---------------------------------------------------------------------------

#' Seed for fit()'s calibration split
#'
#' @description
#' `fit()` scores on `evaluate()`'s train/test split and carves the
#' calibration set that UQ and AD share (Split C) out of its training part.
#' Split C is seeded on its own, with `seed + 1L`, so the calibration rows
#' depend only on `seed` and those training rows, not on RNG state left by an
#' earlier draw. The offset is documented rather than hidden so a caller who
#' needs to reproduce the partition can.
#'
#' @param seed Integer seed passed to `fit()`.
#' @return Integer seed for Split C.
#' @keywords internal
calib_split_seed <- function(seed) {

  as.integer(seed) + 1L

}
