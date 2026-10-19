#' Evaluate Model Configurations
#'
#' @description
#' Evaluates all model configurations from `configure()` against held-out test
#' data. Each configuration is tuned via grid search (optionally followed by
#' Bayesian optimization), evaluated on the test set, and ranked by the
#' specified metric. The best configuration is stored for downstream use by
#' `fit()`.
#'
#' @param x A `horizons_data` object with `config$configs` populated by
#'   `configure()`.
#' @param metric Character. Metric for ranking configurations. One of
#'   `"rpd"`, `"rsq"`, `"rmse"`, `"rrmse"`, `"ccc"`, `"mae"`. Ranking uses
#'   the cross-validated value of that metric at each config's selected
#'   hyperparameters (`evaluation$results$cv_<metric>`); the test-set columns
#'   are reported but never used for selection, so they remain honest
#'   held-out estimates. Default `"rpd"`.
#' @param prune Logical. If TRUE, skip Bayesian optimization for configs
#'   whose grid-search RPD falls below `prune_threshold`. When `configure()`
#'   set `bayesian_iter = 0` there is nothing to skip, so no config is
#'   pruned. Either way, whether each config fell below the threshold is
#'   recorded in `evaluation$results$below_prune_threshold` (with the
#'   threshold in `prune_threshold`), and `fit()` warns when every member it
#'   fits did. Default TRUE.
#' @param prune_threshold Numeric. RPD threshold for pruning. Configs with
#'   grid-search RPD below this value skip Bayesian optimization but still
#'   receive test-set metrics from grid-search best. Default 1.0 (the
#'   "no better than the mean" line).
#' @param seed Integer. Random seed for train/test split and CV folds.
#'   Default 307L.
#' @param control A [control_horizons()] object: how the run goes, not what
#'   it computes. `evaluate()` reads `allow_par`, `parallelize_over`,
#'   `output_dir`, `verbose`, `debug`, `breaker`, `keep_traces` and
#'   `max_worker_payload`, and refuses any other field that is set. The
#'   control it ran with, defaults filled in, is recorded as
#'   `evaluation$control`.
#'
#' @section Checkpoints:
#' With `control_horizons(output_dir = )`, each config's result is
#'   written to `<output_dir>/checkpoints/<config_id>.rds` as it finishes, and
#'   a rerun into the same directory resumes from those files. Required when
#'   configs are dispatched to workers. A resumed row must have been scored on
#'   this run's training data (the outcome, the sample ids, which columns hold
#'   the `id`, `outcome` and `predictor` roles, and the outcome and predictor
#'   values) and tuned with this run's settings
#'   (`cv_folds`, `grid_size`, `bayesian_iter`, `prune`, `prune_threshold`
#'   when pruning, `seed`, and `configure()`'s `sg_window`, `pca_threshold`
#'   and `outcome_range`) and this run's response trim
#'   (`response_threshold`, `NA` when none was requested); either mismatch
#'   aborts, naming what differs.
#'   So re-standardized spectra, a rescaled outcome, or another outcome on the
#'   same samples is refused rather than resumed, while adding a sibling
#'   response with `add_response()` or a `meta` column resumes. Keep one
#'   `output_dir` per outcome. The ranking `metric` is not checked, since
#'   every row carries all six cross-validated metrics. A row scored under an
#'   earlier scoring schema, or recording less of the training data and
#'   settings than this run does, was written by an earlier version: it is
#'   dropped and its config re-evaluated. Rows for configs no longer in the
#'   grid are dropped; a file that cannot be used is a note in the object's
#'   `log` and a line in the progress tree, and its config is re-evaluated.
#'
#' @section Parallelism:
#' With `control_horizons(allow_par = TRUE)`, `evaluate()` dispatches onto
#' whatever `future::plan()` the caller has registered. If no usable backend
#' is registered (fewer than two workers), it runs sequentially and warns,
#' with class `horizons_warning_substitution`, naming the plan it found; the
#' warning is also a row of the object's `log`. Without it, `evaluate()` runs
#' in the calling process and never inspects the plan. It never registers or
#' alters a plan.
#'
#' The user owns the backend and the topology; `evaluate()` owns only which
#' axis the plan is applied to, through `control_horizons(parallelize_over = )`.
#'
#' * `"configs"`: one future per config on the registered plan, with tune
#'   running sequentially inside each. The per-worker payload is one config's
#'   worth and tune ships nothing to inner workers. The mode for a large
#'   config grid on a many-core machine.
#' * `"resamples"`: configs run sequentially; inside each, tune parallelises
#'   the CV folds on the registered plan. Useful width is capped at
#'   `cv_folds`. The mode for a few cores and a short grid.
#' * `"auto"` (default): `"configs"` when the number of configs is at least
#'   `cv_folds` and there is an `output_dir` to checkpoint into, otherwise
#'   `"resamples"`. Derived from the measured cost model: configs wins
#'   whenever `n_configs` exceeds the effective inner speedup, which is
#'   bounded by `cv_folds`, and always once it exceeds the worker count, so
#'   the worker count cancels. `"configs"` asked for explicitly without an
#'   `output_dir` is refused before anything runs (#233).
#'
#' A nested plan (configs across an outer level, folds across an inner one)
#' is not built or endorsed by the package in this version. A caller who
#' wants one registers it with `future::plan(list(...))` and uses
#' `"configs"`.
#'
#' ```
#' # Laptop: four cores, parallelise the folds inside each config.
#' future::plan(future::multisession, workers = 4)
#' hd |> evaluate(control = control_horizons(allow_par = TRUE, parallelize_over = "resamples"))
#'
#' # Thirty cores, one config per worker, checkpointed.
#' future::plan(future::multisession, workers = 30)
#' hd |> evaluate(control = control_horizons(allow_par = TRUE, output_dir = "output/run"))
#' ```
#'
#' Before dispatch, BLAS, OpenMP, data.table and ranger threads are pinned to
#' one in the calling process (via `RhpcBLASctl` when installed) and restored
#' on exit. On either axis the workers load the *installed* package, so a
#' parallel run refuses, before anything is drawn, under
#' `devtools::load_all()` or when a worker loads another version of horizons
#' than the session (class `horizons_error_dependency`). With
#' `control_horizons(debug = TRUE)`, `evaluate()` runs in this session
#' whatever `allow_par` says.
#'
#' @section Response outliers:
#' When `validate()` was asked to remove response outliers
#' (`remove_outliers = TRUE` or `"response"`), it records the request and
#' removes nothing on labels. `evaluate()` draws its train/test split on the
#' untrimmed rows, computes Tukey fences (`validate()`'s
#' `response_threshold` times the IQR) from the training partition's
#' outcome values alone, and drops the training rows outside them before
#' the CV folds are drawn. Test rows are never removed on their labels, so
#' the test metrics describe the population the model will meet, extremes
#' included. `n_train` counts the training rows left after the trim, and
#' `evaluation$response_trim` records it: the `outcome`, `method` and
#' `threshold`, `fences_from` (`"training"`), the `lower` and `upper`
#' fences, `n_training` (the training rows the fences were computed over),
#' `trimmed_ids`, and `skipped` (`NA`, or why no fences could be drawn, in
#' which case nothing is trimmed and `evaluate()` warns). `fit()` reuses
#' the split and those ids. The CV assessment folds are drawn from the
#' trimmed training rows, so the cross-validated metrics (and the ranking
#' on them) describe the trimmed population; the test metrics do not.
#'
#' @section Failed configurations:
#' Each configuration runs as one unit. A failure in any of its steps fails
#' that configuration alone: its row has `status` `"failed"`, an
#' `error_message` naming the step and the cause ("Grid search failed:
#' ..."), the root `class` of the failure, and, with
#' `control_horizons(keep_traces = TRUE)`, the backtrace as text in `trace`.
#' A recipe step that fails inside tuning is reproduced by prepping the
#' recipe once on the training part, so the step's own error is recorded with
#' its class; a failure tune keeps only as text is `horizons_error_run`. A
#' hyperparameter candidate scored on fewer cross-validation folds than were
#' drawn takes no part in the prune gate or the choice of hyperparameters,
#' and a configuration with no grid candidate scored on every fold fails; a
#' `cv_*` mean is kept only when it covers every fold, so every ranked value
#' is on the same folds (#101). A configuration whose final fit on the
#' training part fails is failed too (#102). An error that is not the
#' configuration's own (an input or dependency error, or a bug in horizons)
#' stops the call. So does a failure every configuration would share: when
#' the first configurations to run fail the same way across at least two
#' models, `evaluate()` stops with `horizons_error_systemic`, the rows so far
#' as its `results` (`control_horizons(breaker = )`, 3 by default, `FALSE` to
#' run every configuration; the configs axis has no breaker), and their
#' failures are not checkpointed. A parallel worker that dies stops the run
#' the same way, during a Bayesian iteration too: tune catches future's error
#' there, so the death is read from what future.apply reports first
#' ("Caught FutureInterruptError. Canceling all iterations ...", a warning
#' before future.apply 1.21.0 and a message since), which has no class and
#' is matched by its text. When some configurations failed and
#' the rest are returned, one `horizons_warning_partial_failure` names them
#' with each distinct cause. A configuration whose Bayesian search stopped
#' early keeps the iterations it finished; one whose search failed before any
#' iteration, or added nothing to the grid, keeps the grid's choice. Either
#' says so in its `warnings` and is named in one
#' `horizons_warning_substitution`. With
#' `control_horizons(debug = TRUE)` nothing is contained: the first failure,
#' inside tuning included, stops the call with its backtrace, and the
#' Bayesian search is not fallen back from.
#'
#' @section When every configuration fails:
#' `best_config` is chosen from the configs that succeeded or, when none did,
#' from the pruned configs that carry a cross-validated value of `metric`. A
#' config without a value of `metric` is left out of the ranking, with one
#' `horizons_warning_skipped` naming it. When configs were evaluated but none
#' has a value of `metric`, `evaluate()` returns with `best_config` `NA` and
#' that warning, and `fit()` asks for another `metric`.
#'
#' When every config failed, `evaluate()` aborts with class
#' `horizons_error_all_configs_failed` (and `horizons_error_run`). The
#' message lists the distinct error messages (the first three, each with the
#' configs that raised it) and names any configs that were loaded from
#' checkpoints in `output_dir` rather than run, since calling `evaluate()`
#' again resumes them instead of re-running them. The per-config results,
#' `error_message` included, travel on the condition as `results`, because
#' `evaluate()` aborts before it assigns `x$evaluation`; after an uncaught
#' abort, `rlang::last_error()$results` recovers them without re-running.
#'
#' A failure the first configurations share across at least two models stops
#' the call earlier, with `horizons_error_systemic` (also
#' `horizons_error_run`) and the rows finished so far as its `results`; with
#' `control_horizons(breaker = FALSE)` every configuration runs and the call
#' ends in the all-failed abort instead. Both carry `results`, so a loop over
#' subsets can catch `horizons_error_run`, tolerate a subset that fails
#' either way, and keep its errors:
#'
#' ```
#' runs <- lapply(subsets, function(hd) {
#'   tryCatch(
#'     evaluate(hd, control = control_horizons(verbose = FALSE)),
#'     horizons_error_run = function(e) e$results
#'   )
#' })
#'
#' # A horizons_eval for each subset that ranked, and the results table so
#' # far for each subset that failed.
#' failed <- runs[!vapply(runs, inherits, logical(1), "horizons_eval")]
#' ```
#'
#' @return A `horizons_eval` object (inherits from `horizons_data`) with
#'   `evaluation$results`, `evaluation$best_config`, `evaluation$split`, and
#'   associated metadata populated, including `evaluation$parallelize_over`
#'   (the axis actually used), `evaluation$workers` (the worker count the
#'   registered plan offered; 1 when sequential) and `evaluation$recipe` (the
#'   `sg_window`, its width in cm-1 on the evaluated axis as `sg_window_cm`,
#'   and the `pca_threshold` every config's recipe ran with) and
#'   `evaluation$response_trim` (the training-partition response trim, or
#'   `NULL` when none was requested; see "Response outliers"). Test-set
#'   predictions are clamped to `configure()`'s `outcome_range` before they
#'   are scored, as are the tuning predictions of a transformed response,
#'   which are back-transformed first. Aborts, before any config runs, when
#'   `configure()`'s `sg_window` is not narrower than the spectrum, and, with class
#'   `horizons_input_error`, when an observed outcome lies outside
#'   `outcome_range` or is infinite, or when `metric = "rrmse"` under a range
#'   whose lower bound is negative (the mean outcome it divides by can then be
#'   zero or negative) (#76). Also with class `horizons_input_error`, it
#'   aborts when the outcome has zero variance: before the split, when every
#'   observed value is the same, and after it, when every training row left
#'   by the split and the response trim has the same value, since every
#'   config would then be tuned on a constant. The message names the outcome,
#'   the value and how many rows have it (#132). Called on an object that
#'   has already been through `fit()` or `ensemble()`, it returns a
#'   `horizons_eval` whose `models` and `ensemble` slots are empty again,
#'   since both were built on the evaluation it replaces.
#'
#' @export
evaluate <- function(x,
                     metric          = "rpd",
                     prune           = TRUE,
                     prune_threshold = 1.0,
                     seed            = 307L,
                     control         = control_horizons()) {

  start_time <- Sys.time()

  ## How the run goes: the fields evaluate() reads, defaults filled in, and
  ## the record of it on the evaluation
  control          <- resolve_control(control, "evaluate")
  allow_par        <- control$allow_par
  parallelize_over <- control$parallelize_over
  output_dir       <- control$output_dir
  verbose          <- control$verbose

  ## Dispatching configs needs a checkpoint directory; "auto" never chooses
  ## the configs axis without one, so only an explicit request is refused
  ## (#233).
  if (allow_par && parallelize_over == "configs" && is.null(output_dir)) {

    abort_horizons(c(
      "{.code parallelize_over = \"configs\"} needs an {.arg output_dir} to checkpoint each configuration into.",
      "i" = "Set {.code control_horizons(output_dir = )}, or use {.code parallelize_over = \"resamples\"}."
    ), kind = "input")

  }

  collector <- log_open("evaluate")

  ## -----------------------------------------------------------------------
  ## Step 0: Structural validation
  ## -----------------------------------------------------------------------
  ## validate_horizons_eval() (at return) runs the base validator too, but
  ## only after every config has run. Checking here refuses a malformed table
  ## before anything is tuned: a column added after configure() without a
  ## role_map entry — by a later parse_ids(), or a direct
  ## `x$data$analysis$col <- ...` — would otherwise reach build_recipe()'s
  ## `outcome ~ .` as an unregistered predictor (#24). Full stage: an object
  ## this far into the pipeline is expected to have unique, non-NA sample ids.

  x <- validate_horizons_data(x)

  ## -----------------------------------------------------------------------
  ## Step 1: Gate checks
  ## -----------------------------------------------------------------------

  check_rank_metric_arg(metric)

  configs <- x$config$configs

  if (is.null(configs) || nrow(configs) == 0) {

    abort_horizons(c(
      "No configurations found.",
      "i" = "Run {.fn configure} before {.fn evaluate}."
    ), kind = "input")

  }

  role_map    <- x$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]
  analysis    <- x$data$analysis

  ## The axis has to be in order for the spectral steps; the same for every
  ## configuration, so checked once, before anything is drawn (#121)
  check_band_order(role_map$variable[role_map$role == "predictor"])

  ## -----------------------------------------------------------------------
  ## Step 2: Handle NA outcome rows
  ## -----------------------------------------------------------------------
  ## fit() applies the same rule to the same table, so the two verbs model
  ## the same rows (#67).

  modelled  <- modelling_rows(analysis, outcome_col)
  analysis  <- modelled$data
  n_dropped <- modelled$n_dropped
  n_flagged <- modelled$n_flagged

  ## -----------------------------------------------------------------------
  ## Step 2b: The outcome has to lie inside its range
  ## -----------------------------------------------------------------------
  ## Every test-set and tuning prediction is clamped to configure()'s
  ## outcome_range, so an outcome outside it would be scored against values
  ## the predictions cannot reach. configure() refuses that already, but an
  ## object configured before the range existed reads as c(0, Inf), and rows
  ## can change after configure(). Refused here, before any split or tuning,
  ## so it costs a second rather than the whole grid (#76).

  check_outcome_range(x, verb = "evaluate")
  outcome_range <- outcome_range_setting(x)

  ## rrmse divides by the mean outcome, which a range with a negative floor
  ## lets be zero or negative; ranking by its minimum would then pick the
  ## worst configuration.
  check_rank_metric_range(metric, outcome_range, verb = "evaluate")

  ## -----------------------------------------------------------------------
  ## Step 2c: The outcome has to vary
  ## -----------------------------------------------------------------------
  ## validate() reports a constant outcome, but nothing gates on its verdict,
  ## so it is refused here, before any split (#132). The training part is
  ## checked again in Step 4, once the split and the trim have drawn it.

  check_outcome_variance(x, verb = "evaluate")

  ## -----------------------------------------------------------------------
  ## Step 3: The tuning settings
  ## -----------------------------------------------------------------------
  ## Small samples are weak, not wrong, so they run: the folds' row count is
  ## checked once the split and the trim have drawn the training part (Step
  ## 4), and a small assessment fold warns (Step 5).

  tuning   <- x$config$tuning
  cv_folds <- tuning$cv_folds

  ## -----------------------------------------------------------------------
  ## Step 3a: The window has to fit the spectrum
  ## -----------------------------------------------------------------------
  ## Object-level recipe settings, the same for every config; see
  ## recipe_settings() for objects configured before they were recorded.
  ##
  ## configure() cannot check the window against the spectrum, because
  ## standardize() can still change the axis after it. Past this point a
  ## window as wide as the spectrum fails every config inside tune, as a
  ## "Grid search failed" or an "All configurations failed" that never names
  ## the window. The width in cm-1 is read from the axis the recipe will run
  ## on, and recorded in Step 11. The check is one helper, which fit()'s cold
  ## start (#45) applies too.

  recipe_cfg <- evaluation_recipe(x)

  ## -----------------------------------------------------------------------
  ## Step 3b: Resolve the parallel axis against the registered plan
  ## -----------------------------------------------------------------------
  ## The user owns the backend; evaluate() only chooses the axis. The
  ## resolver is pure; the backend check reads the plan and downgrades to
  ## sequential, with a warning, when there is nothing to run on.

  ## Debug runs in this session: a backtrace from a worker shows the
  ## worker's plumbing, and no debugger attaches there. The user asked for
  ## debug, so the tree says so rather than a warning.
  debug    <- control$debug
  dispatch <- allow_par && !debug

  axis <- resolve_parallel_axis(parallelize_over, dispatch,
                                n_configs      = nrow(configs),
                                cv_folds       = cv_folds,
                                has_output_dir = !is.null(output_dir))

  if (dispatch && !check_parallel_backend("evaluate", collector)) {

    axis <- resolve_parallel_axis(parallelize_over, allow_par = FALSE,
                                  n_configs      = nrow(configs),
                                  cv_folds       = cv_folds,
                                  has_output_dir = !is.null(output_dir))

  }

  ## On either axis the workers run horizons code (on the resamples axis,
  ## tune's workers run the recipe steps and the PLS engine), so they must
  ## run this session's horizons: checked here, before anything is drawn.
  if (axis$axis != "sequential") check_parallel_workers()

  plan_label    <- if (axis$axis == "sequential") "sequential" else registered_plan_label()
  plan_workers  <- if (axis$axis == "sequential") 1L else registered_workers()

  if (axis$axis != "sequential") {

    unpin_threads <- pin_parent_threads()
    on.exit(unpin_threads(), add = TRUE)

  }

  ## -----------------------------------------------------------------------
  ## Step 4: Create train/test split
  ## -----------------------------------------------------------------------
  ## The draw is shared with fit()'s cold start (#45), which must hold out
  ## the rows this verb would at the same seed. `analysis` has had its NA
  ## outcomes dropped already; the helper's own pass over it then drops
  ## nothing and returns it uncopied, so the split shares its rows.
  ##
  ## A response trim validate() requested (#77) is read first, so a request
  ## that cannot apply is refused before anything is drawn, and applied to
  ## the split as drawn: the fences come from the training partition's labels
  ## alone, and only training rows outside them leave, before the folds below
  ## are drawn. fit()'s cold start makes the same call after the same draw.
  ## The trim draws nothing from the RNG, so the folds follow the stream they
  ## always did; without a request the split is the draw.

  trim_request <- response_trim_request(x)

  drawn <- draw_eval_split(analysis, outcome_col, seed)

  trimmed <- trim_training_responses(drawn$split, outcome_col, trim_request,
                                     id_col = id_column(role_map), collector = collector)
  split   <- trimmed$split

  train_data <- rsample::training(split)
  test_data  <- rsample::testing(split)
  n_train    <- nrow(train_data)
  n_test     <- nrow(test_data)

  ## The modelled rows vary (Step 2c), but when the few rows with another
  ## value all fall in the test part or are trimmed, every config would be
  ## tuned on a constant. Refused before the folds are drawn (#132).
  drawn_rows <- drawn$split$data
  trim_ids   <- trimmed$record$trimmed_ids %||% character(0)

  ## Every fold needs rows to fit on and a row to assess, and rsample refuses
  ## as many folds as rows. That is the one sample size refused, here,
  ## before anything is tuned.
  check_fold_rows(n_train, cv_folds, n_modelled = nrow(analysis),
                  n_test = n_test, n_trimmed = length(trim_ids))

  check_training_outcome_variance(
    train_data[[outcome_col]],
    held_out    = list(
      "in the test set"              = test_data[[outcome_col]],
      "trimmed as response outliers" = drawn_rows[[outcome_col]][drawn_rows[[id_column(role_map)]] %in% trim_ids]
    ),
    outcome_col = outcome_col,
    verb        = "evaluate"
  )

  ## -----------------------------------------------------------------------
  ## Step 5: Create CV folds
  ## -----------------------------------------------------------------------

  ## Drawn from the stream the split left. The fallback notes print inside
  ## the tree, after its header (#91).

  cv_drawn <- draw_stratified(
    outcome      = train_data[[outcome_col]],
    stratified   = function() rsample::vfold_cv(train_data, v = cv_folds,
                                                strata = dplyr::all_of(outcome_col),
                                                breaks = STRATA_BREAKS, pool = STRATA_POOL),
    unstratified = function() rsample::vfold_cv(train_data, v = cv_folds)
  )
  cv_fold_obj <- cv_drawn$draw

  ## A stratified draw that failed fell back to an unstratified one: what
  ## ran is not what was asked, so each draw that did says so once.
  log_strata_fallback(collector, drawn, "train/test split")
  log_strata_fallback(collector, cv_drawn, "cross-validation folds")

  ## A small assessment fold is weak, not wrong: it runs, with one warning.
  ## Metrics undefined on a fold that small are NA.
  log_small_folds(collector, cv_fold_obj)

  ## -----------------------------------------------------------------------
  ## Step 5b: Fingerprint the training rows
  ## -----------------------------------------------------------------------
  ## Checkpoints are keyed by config_id alone, which says nothing about the
  ## rows a result was computed on. A dry run and a real run pointed at the
  ## same output_dir therefore used to resume each other's CV results and
  ## warm-start fit() from hyperparameters tuned on the wrong data, silently
  ## (2026-09-21). The fingerprint is what makes that visible. It covers the
  ## response as well as the rows: the config id does not hash the outcome, so
  ## two responses on one row set collide by id alone. It also covers the
  ## values on those rows (#42), since re-standardized spectra keep the ids.
  ## Computed once here; the parallel worker is sent it.

  data_fp <- eval_data_fingerprint(train_data, role_map)

  ## -----------------------------------------------------------------------
  ## Step 5c: Record the settings a result row depends on
  ## -----------------------------------------------------------------------
  ## The data fingerprint says which rows a checkpoint was scored on, not how
  ## it was tuned: the same rows re-run with another grid_size into the same
  ## output_dir resumed the old results silently (#42). This list is the one
  ## place a setting is added. The metric is absent on purpose: every row
  ## carries all six cv_ columns and the ranking is recomputed on each run, so
  ## changing it cannot make a row stale. So is the parallel axis, which the
  ## results do not depend on.

  settings <- eval_settings(
    cv_folds        = cv_folds,
    grid_size       = tuning$grid_size,
    bayesian_iter   = tuning$bayesian_iter,
    prune           = prune,
    ## Read only when pruning, so it cannot have changed a row otherwise.
    prune_threshold = if (isTRUE(prune)) prune_threshold else NA_real_,
    seed            = seed,
    ## configure()'s recipe settings (#62). pca_threshold is recorded for
    ## every row, PCA or not: conservative, since a row that never ran PCA
    ## re-runs rather than resumes when only the threshold changed.
    sg_window       = recipe_cfg$sg_window,
    pca_threshold   = recipe_cfg$pca_threshold,
    ## The range every scored prediction is clamped to (#76): the same fit
    ## scores differently under another range, so a resume under one refuses.
    outcome_range      = outcome_range,
    ## The response trim (#77). The fingerprint above already covers the
    ## rows it left, since it hashes the trimmed training part; recording the
    ## threshold as well makes a refusal name the trim as the reason. NA when
    ## no trim was requested, as prune_threshold is when not pruning.
    response_threshold = if (is.null(trim_request)) NA_real_ else trim_request$threshold
  )

  ## -----------------------------------------------------------------------
  ## Step 6: Load checkpoints (if any)
  ## -----------------------------------------------------------------------
  ## One store: a file per config under checkpoints/. Every row passes the
  ## same gates in the same order (see load_eval_checkpoints()).

  checkpoint_dir     <- NULL
  checkpoint_results <- list()
  checkpoint_notes   <- character(0)

  if (!is.null(output_dir)) {

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    checkpoint_dir <- file.path(output_dir, "checkpoints")

    if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir)

    loaded <- load_eval_checkpoints(
      output_dir = output_dir,
      config_ids = configs$config_id,
      data_fp    = data_fp,
      settings   = settings,
      collector  = collector
    )

    ## The count of loaded results is the Configs line's "from checkpoint",
    ## and the notes on dropped rows print under it: both used to print
    ## here, above the tree's header (#91).
    checkpoint_results <- loaded$rows
    checkpoint_notes   <- loaded$notes

  }

  ## Determine which configs still need evaluation
  completed_ids <- names(checkpoint_results)
  pending_ids   <- setdiff(configs$config_id, completed_ids)
  n_total       <- nrow(configs)
  n_pending     <- length(pending_ids)

  ## -----------------------------------------------------------------------
  ## Step 7: Render tree header
  ## -----------------------------------------------------------------------

  if (verbose) {

    cat("\n")
    cat(paste0("\u250C Evaluation ",
               paste(rep("\u2500", 50), collapse = ""), "\n"))
    cat("\u2502\n")
    if (n_dropped > 0) {

      cat(paste0("\u2502  ",
                 cli::col_yellow("Dropped ", n_dropped,
                                  " rows with NA outcome"), "\n"))

    }

    if (n_flagged > 0) {

      cat(paste0("\u2502  ",
                 cli::col_yellow("Left out ", n_flagged,
                                  " flagged rows (see .flag)"), "\n"))

    }

    ## "stratified" only when the strata held: rsample draws unstratified
    ## below 40 rows without an error (#91; see draw_stratified()). The folds
    ## are drawn on the training part, so they can be unstratified under a
    ## stratified split.
    cat(paste0("\u2502  Split: ", n_train, " train / ", n_test, " test (",
               round(100 * SPLIT_PROP), "/", round(100 * (1 - SPLIT_PROP)), ", ",
               if (drawn$stratified) "stratified" else "unstratified", ")\n"))

    if (drawn$strata_failed) {

      cat(paste0("\u2502  ", cli::col_yellow("Stratified split failed, ",
                                             "retrying without strata"), "\n"))

    }

    render_response_trim(trimmed$record)

    cat(paste0("\u2502  Tuning: ", cv_folds, "-fold CV (",
               if (cv_drawn$stratified) "stratified" else "unstratified",
               "), grid = ", tuning$grid_size, ", bayesian = ",
               tuning$bayesian_iter, "\n"))

    if (cv_drawn$strata_failed) {

      cat(paste0("\u2502  ", cli::col_yellow("Stratified CV failed, ",
                                             "retrying without strata"), "\n"))

    }

    cat(paste0("\u2502  Configs: ", n_total, " total",
               if (n_pending < n_total) paste0(" (", n_total - n_pending,
                                                " from checkpoint)") else "",
               "\n"))

    for (note in checkpoint_notes) {

      cat(paste0("\u2502  ", cli::col_yellow(note), "\n"))

    }

    if (axis$axis != "sequential") {

      ## A parallel axis has two workers or more, or an unbounded count
      workers_label <- if (is.na(plan_workers)) "unbounded" else paste0(plan_workers, " workers")

      cat(paste0("\u2502  Parallel: over ", axis$axis, " on ", plan_label,
                 " (", workers_label, ")\n"))

    } else if (allow_par && debug) {

      cat("\u2502  Parallel: off, since debug runs in this session\n")

    }

    cat("\u2502\n")

  }

  ## -----------------------------------------------------------------------
  ## Step 7b: Manifest for monitor_evaluate()
  ## -----------------------------------------------------------------------
  ## Written on EVERY run with an output_dir, whichever axis, and refreshed
  ## each time, so the monitor can watch a resamples-axis or sequential run
  ## and never reports a stale axis from an earlier run in the same
  ## directory. It records the axis and the user's plan, and the training-data
  ## fingerprint, so the monitor can say which rows the run in this directory
  ## is scoring, with the data fields and the tuning settings. The monitor
  ## gates checkpoint rows against them, as evaluate() does, and re-reads the
  ## manifest on every poll, so it is written atomically.

  if (!is.null(output_dir)) {

    manifest <- list(
      schema_version               = EVAL_MANIFEST_SCHEMA,
      n_total                      = n_total,
      n_pending                    = n_pending,
      config_ids                   = configs$config_id,
      start_time                   = start_time,
      metric                       = metric,
      cv_folds                     = cv_folds,
      allow_par                    = allow_par,
      parallelize_over_requested   = parallelize_over,
      axis                         = axis$axis,
      tune_parallel_over_requested = axis$tune_parallel_over,
      plan                         = plan_label,
      workers                      = plan_workers,
      scoring_schema               = SCORING_SCHEMA,
      data_hash                    = data_fp$data_hash,
      data_n_rows                  = data_fp$data_n_rows,
      data_fields                  = data_fp$data_fields,
      settings                     = settings
    )
    tmp_manifest <- tempfile(tmpdir = output_dir, fileext = ".rds.tmp")
    saveRDS(manifest, tmp_manifest)
    file.rename(tmp_manifest, file.path(output_dir, "eval_manifest.rds"))

  }

  ## -----------------------------------------------------------------------
  ## Step 8: Config loop (sequential or parallel)
  ## -----------------------------------------------------------------------

  if (!axis$dispatch_configs) {

    ## -------------------------------------------------------------------
    ## Sequential path — identical to pre-workers behavior
    ## -------------------------------------------------------------------

    results_list <- list()

    ## The circuit breaker watches the first configurations to run, so a
    ## failure every configuration would share stops the call early
    ## (control_horizons(breaker = )); the configs axis has none.
    breaker <- new_breaker(control$breaker)

    for (i in seq_len(nrow(configs))) {

      cfg <- configs[i, ]

      ## Pretty config description — always show full pipeline
      model_name  <- MODEL_DISPLAY_NAMES[cfg$model] %||% cfg$model
      config_desc <- paste(model_name, cfg$transformation, cfg$preprocessing,
                           cfg$feature_selection, sep = " + ")
      is_last     <- i == nrow(configs)
      branch      <- if (is_last) "\u2514\u2500" else "\u251C\u2500"
      cont        <- if (is_last) "   " else "\u2502  "

      ## Skip if already checkpointed
      if (cfg$config_id %in% completed_ids) {

        if (verbose) {

          cat(paste0(
            "\u2502  ", branch, " [", i, "/", n_total, "] ",
            config_desc, "\n"
          ))
          cat(paste0("\u2502  ", cont, "\u2514\u2500 ",
                     cli::col_cyan("loaded from checkpoint"), "\n"))

        }

        results_list[[i]] <- checkpoint_results[[ cfg$config_id ]]
        next

      }

      ## Render config start
      if (verbose) {

        cat(paste0(
          "\u2502  ", branch, " [", i, "/", n_total, "] ",
          config_desc, "\n"
        ))

      }

      ## Evaluate this config. A worker of the resamples axis that dies stops
      ## the run, with the rows so far.
      result_row <- stop_on_worker_death(evaluate_single_config(
        config_row      = cfg,
        split           = split,
        cv_folds        = cv_fold_obj,
        role_map        = role_map,
        grid_size       = tuning$grid_size,
        bayesian_iter   = tuning$bayesian_iter,
        prune           = prune,
        prune_threshold = prune_threshold,
        allow_par       = axis$tune_allow_par,
        parallel_over   = axis$tune_parallel_over %||% "resamples",
        seed            = seed,
        sg_window       = recipe_cfg$sg_window,
        pca_threshold   = recipe_cfg$pca_threshold,
        outcome_range   = outcome_range,
        debug           = debug,
        keep_traces     = control$keep_traces
      ), results_so_far(results_list), output_dir)

      ## Stamp before anything else sees the row, so the in-memory results and
      ## the checkpointed copy carry the same provenance.
      result_row        <- stamp_data_fingerprint(result_row, data_fp)
      result_row        <- stamp_eval_settings(result_row, settings)
      results_list[[i]] <- result_row

      ## Render result
      if (verbose) {

        if (result_row$status == "success") {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 Test metrics: ",
            "RPD = ", round(result_row$rpd, 2),
            ", R\u00B2 = ", round(result_row$rsq, 2),
            ", RMSE = ", round(result_row$rmse, 3), "\n"
          ))

        } else if (result_row$status == "pruned") {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_yellow("Pruned (grid RPD below threshold)"), "\n"
          ))
          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 Test metrics: ",
            "RPD = ", round(result_row$rpd, 2),
            ", R\u00B2 = ", round(result_row$rsq, 2),
            ", RMSE = ", round(result_row$rmse, 3), "\n"
          ))

        } else {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_red("FAILED: ", result_row$error_message), "\n"
          ))

        }

        ## Render warnings (yellow, one per line)
        if (!is.null(result_row$warnings[[1]])) {

          for (w in result_row$warnings[[1]]) {
            cat(paste0(
              "\u2502  ", cont, "\u251C\u2500 ",
              cli::col_yellow(w), "\n"
            ))
          }

        }

        ## Render runtime
        cat(paste0(
          "\u2502  ", cont, "\u2514\u2500 ",
          if (result_row$status == "failed") {
            cli::col_red("\u2717")
          } else {
            cli::col_green("\u2713")
          },
          " ", round(result_row$runtime_secs, 1), "s\n"
        ))

      }

      ## Checkpoint: the per-config file is the only store (#42). The rows of
      ## a run of identical failures the breaker is watching are held back
      ## until it disarms, so a tripped run leaves none to resume.
      released <- breaker_feed(breaker, result_row, cfg$model)

      if (breaker$tripped) {

        ## A stop saves the configurations still to run; with none left it
        ## saves nothing, so the held failures are ordinary ones and the
        ## call ends as any other whose configurations all failed
        not_run <- setdiff(configs$config_id[-seq_len(i)], completed_ids)

        if (length(not_run) > 0) {
          abort_breaker_trip(breaker, results_so_far(results_list),
                             length(not_run))
        }

        released <- breaker_release(breaker)

      }

      if (!is.null(checkpoint_dir)) {

        for (row in released) write_checkpoint_row(row, checkpoint_dir)

      }

      ## Memory cleanup
      gc(verbose = FALSE)

    }

    ## The loop ended with the breaker still watching: its rows are ordinary
    ## failures after all
    if (!is.null(checkpoint_dir)) {

      for (row in breaker_release(breaker)) write_checkpoint_row(row, checkpoint_dir)

    }

  } else {

    ## -------------------------------------------------------------------
    ## Parallel path — furrr::future_map() across configs
    ## -------------------------------------------------------------------

    ## The plan is the user's: evaluate() neither sets nor restores one.
    ## The globals ceiling is declared (max_worker_payload: the user's
    ## future.globals.maxSize when they set one, else EVAL_WORKER_PAYLOAD_LIMIT,
    ## from the measured payload) so a payload regression aborts with
    ## future's clear "size of the globals" error rather than R's long-vector
    ## message.
    old_max_size <- getOption("future.globals.maxSize")
    options(future.globals.maxSize = control$max_worker_payload)
    on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)

    if (verbose) {

      cat(paste0(
        "\u2502  Processing ", n_pending, " pending configs...\n"
      ))
      cat(paste0(
        "\u2502  Monitor: horizons::monitor_evaluate(\"",
        output_dir, "\")\n"
      ))
      cat("\u2502\n")

    }

    ## Filter to pending configs only
    pending_configs <- configs[configs$config_id %in% pending_ids, ]

    ## Assemble everything a worker needs into one value. Two serialization
    ## traps are avoided here; R/utils-resamples.R's header has the mechanism
    ## and the measurements.
    ##
    ## Do not inline the worker back into an anonymous function. A closure
    ## defined in this frame is serialized together with it, so every local —
    ## `split` and `cv_fold_obj` included — would ship to every worker no matter
    ## what is passed explicitly.

    ## The workers resolve the horizons namespace by NAME, which finds the
    ## INSTALLED library; check_parallel_workers() refused a session they
    ## could not match before anything ran.

    shared_args <- list(
      data            = split$data,
      resample_idx    = resample_indices(split, cv_fold_obj),
      configs         = pending_configs,
      role_map        = role_map,
      grid_size       = tuning$grid_size,
      bayesian_iter   = tuning$bayesian_iter,
      prune           = prune,
      prune_threshold = prune_threshold,
      seed            = seed,
      sg_window       = recipe_cfg$sg_window,
      pca_threshold   = recipe_cfg$pca_threshold,
      outcome_range   = outcome_range,
      data_fp         = data_fp,
      settings        = settings,
      checkpoint_dir  = checkpoint_dir,
      keep_traces     = control$keep_traces,
      pkg_version     = as.character(utils::packageVersion("horizons"))
    )

    stopifnot(setequal(names(shared_args), SHARED_ARG_NAMES))

    ## A worker that dies loses every value in memory; what it and the
    ## others finished is in their checkpoints.
    parallel_results <- stop_on_worker_death(furrr::future_map(
      seq_len(nrow(pending_configs)),
      evaluate_config_worker,
      shared   = shared_args,
      .options = furrr::furrr_options(seed = TRUE, chunk_size = 1)
    ), results_so_far(lapply(completed_ids, function(id) checkpoint_results[[id]])),
    output_dir)

    ## Combine: checkpoint results + parallel results
    results_list <- c(
      lapply(completed_ids, function(id) checkpoint_results[[id]]),
      parallel_results
    )

  }

  ## -----------------------------------------------------------------------
  ## Step 9: Aggregate results
  ## -----------------------------------------------------------------------

  results_list <- lift_unit_records(results_list, collector)
  all_results  <- dplyr::bind_rows(results_list)

  ## -----------------------------------------------------------------------
  ## Step 10: Determine best config
  ## -----------------------------------------------------------------------
  ## Ranking uses the cross-validated metric at each config's selected
  ## hyperparameters (cv_<metric>), not the test-set metric. Selecting on the
  ## test set would make the reported test metric of the winner a maximum
  ## over N configs on the same rows rather than a held-out estimate (#50).
  ## `metric` and `rank_metric` keep the bare name; the column is derived.

  rank_column <- paste0("cv_", metric)

  ## Nothing usable: every configuration failed. Rows resumed from
  ## checkpoints are named, since re-running evaluate() does not re-run them.
  evaluated <- all_results$config_id[all_results$status %in% c("success", "pruned")]

  if (length(evaluated) == 0) {

    abort_all_configs_failed(
      all_results,
      checkpoint_ids = intersect(all_results$config_id, completed_ids),
      output_dir     = output_dir
    )

  }

  ## The successes, or the pruned configs when none succeeded: the rule
  ## fit() applies to the same table. A configuration without a value of the
  ## ranking metric is left out of the ranking, and said so once; when none
  ## has one, the evaluation is returned without a best_config, since the
  ## configurations were scored, and fit() asks for another metric.
  candidates <- ranking_candidates(all_results, metric)
  ranked     <- rank_configs_by_cv(candidates$rows, metric)

  best_config_id <- if (nrow(ranked) > 0) ranked$config_id[1] else NA_character_

  if (is.na(best_config_id)) {

    ## The lines are rendered already, so each is substituted as text
    reason <- unranked_reason(all_results, metric, "`evaluate(metric = )` or `fit(metric = )`")

    log_warning(collector, "skipped", c(
      "No configuration could be ranked, so the evaluation has no {.field best_config}: {reason[1]}",
      stats::setNames(sprintf("{reason[%d]}", seq_along(reason)[-1]), names(reason)[-1])
    ), ids = candidates$rows$config_id)

  } else {

    log_unranked(collector, candidates$rows, ranked, metric)

  }

  ## Some configurations failed and the rest are returned: one summary, with
  ## each distinct cause and the configurations it failed
  failed_rows <- all_results[all_results$status == "failed", , drop = FALSE]

  if (nrow(failed_rows) > 0) {

    n_failed <- nrow(failed_rows)

    log_warning(collector, "partial_failure", c(
      "{n_failed} of {nrow(all_results)} configuration{?s} failed, so the evaluation holds the rest.",
      distinct_config_errors(failed_rows)
    ), ids = failed_rows$config_id)

  }

  ## -----------------------------------------------------------------------
  ## Step 11: Store evaluation metadata
  ## -----------------------------------------------------------------------

  total_runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  ## Re-evaluating a fitted or ensembled object replaces the evaluation the
  ## models were selected from, so the models and the ensemble go with it.
  x <- reset_slots(x, c("models", "ensemble"))

  x$evaluation <- list(
    results          = all_results,
    best_config      = best_config_id,
    rank_metric      = metric,
    screened         = TRUE,
    split            = split,
    n_train          = n_train,
    n_test           = n_test,
    ## What the trim did, so fit() drops the same training rows rather than
    ## recomputing fences on another set; NULL when none was requested.
    response_trim    = trimmed$record,
    workers          = plan_workers,
    parallelize_over = axis$axis,
    ## What every config's recipe ran with, the window's width included, so
    ## the results of an sg_window sweep can be told apart after the fact.
    recipe           = recipe_cfg,
    control          = control,
    runtime_secs     = total_runtime,
    timestamp        = Sys.time()
  )

  ## -----------------------------------------------------------------------
  ## Step 12: Promote class
  ## -----------------------------------------------------------------------

  class(x) <- c("horizons_eval", "horizons_data", "list")

  ## What the run reported goes on the object's log before it is certified
  x$log <- log_close(collector, x$log)

  ## Certify the contract before returning: structural checks only (see
  ## validate_horizons_eval), so every evaluate() return matches invariant I5
  ## (results complete; best_config names a real config).
  x <- validate_horizons_eval(x)

  ## -----------------------------------------------------------------------
  ## Step 13: Render tree footer
  ## -----------------------------------------------------------------------

  if (verbose) {

    n_success <- sum(all_results$status == "success")
    n_pruned  <- sum(all_results$status == "pruned")
    n_failed  <- sum(all_results$status == "failed")

    cat("\u2502\n")
    cat(paste0(
      "\u2502  Results: ", n_success, " success, ",
      n_pruned, " pruned, ", n_failed, " failed\n"
    ))

    if (is.na(best_config_id)) {

      ## The successes, or the pruned configurations when none succeeded
      candidates_were <- if (identical(ranking_candidates(all_results, metric)$what, "succeeded")) {
        "configuration that succeeded"
      } else {
        "pruned configuration"
      }

      cat(paste0("\u2514\u2500 Best: none, since no ", candidates_were, " has a CV ",
                 toupper(metric), "\n"))

    } else {

      ## Best config description
      best_cfg   <- configs[configs$config_id == best_config_id, ]
      best_model <- MODEL_DISPLAY_NAMES[best_cfg$model] %||% best_cfg$model
      best_row   <- all_results[all_results$config_id == best_config_id, ]

      cat(paste0(
        "\u2514\u2500 Best: ", best_config_id, " (", best_model, ")",
        " \u2014 CV ", toupper(metric), " = ",
        round(best_row[[rank_column]], 3),
        " (test ", toupper(metric), " = ", round(best_row[[metric]], 3), ")\n"
      ))

    }
    cat(paste0(
      paste(rep("\u2500", 62), collapse = ""), "\n"
    ))

  }

  ## Each warning the run recorded, raised once, after the tree
  signal_log(collector, x)

  x

}

## ---------------------------------------------------------------------------
## Checkpoint store
## ---------------------------------------------------------------------------
## One file per config under <output_dir>/checkpoints/ is the store (#42),
## written by the sequential path and the workers alike. Every row passes one
## gate order: training-data fingerprint, then tuning settings, then scoring
## schema. evaluate() and monitor_evaluate() both read through these helpers,
## so the monitor never counts or ranks a row evaluate() would reject.

#' Read one per-config checkpoint file
#'
#' @param path Path to the file.
#' @return List with `row` (the one-row result, or `NULL`) and `error` (why
#'   the file is not a usable checkpoint, or `NULL`).
#' @keywords internal
#' @noRd
read_checkpoint_file <- function(path) {

  row <- tryCatch(readRDS(path), error = function(e) e)

  if (inherits(row, "error")) {

    return(list(row = NULL, error = condition_summary(row)))

  }

  if (!is.data.frame(row) || nrow(row) != 1L || !"config_id" %in% names(row)) {

    return(list(row = NULL, error = "not a one-row result with a config_id"))

  }

  list(row = row, error = NULL)

}

#' Read every checkpoint row in an output directory
#'
#' Reads the per-config files. A file counts only for the config its name
#' says: `<config_id>.rds` holding that config's row. Anything else is
#' reported, never read as a checkpoint, so a stray or copied file cannot
#' stand in for a config's own; the config is re-evaluated instead. Nothing
#' is gated here; see [gate_checkpoint_rows()].
#'
#' @param output_dir The directory passed to `evaluate(output_dir = )`.
#' @return List with `rows`, a list of candidates, each a list of `row` (the
#'   one-row result), `source` (the file, relative to `output_dir`) and `path`
#'   (the file); and `unreadable`, the reason each unusable file was refused,
#'   named by the file relative to `output_dir`.
#' @keywords internal
#' @noRd
read_checkpoint_store <- function(output_dir) {

  checkpoint_dir <- file.path(output_dir, "checkpoints")

  files <- if (dir.exists(checkpoint_dir)) {
    list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE)
  } else {
    character(0)
  }

  ## Pre-allocated and filled in place: a monitor polls a store of thousands
  ## of files, and growing a list per file is quadratic.
  named   <- sub("\\.rds$", "", basename(files))
  sources <- file.path("checkpoints", basename(files))
  rows    <- vector("list", length(files))
  reasons <- rep(NA_character_, length(files))

  for (i in seq_along(files)) {

    got <- read_checkpoint_file(files[i])

    if (!is.null(got$error)) {

      reasons[i] <- got$error
      next

    }

    id <- as.character(got$row$config_id)

    if (!identical(id, named[i])) {

      reasons[i] <- paste0("holds config ", id, "; not a checkpoint name")
      next

    }

    rows[[i]] <- list(row = got$row, source = sources[i], path = files[i])

  }

  refused <- !is.na(reasons)

  list(rows       = rows[!refused],
       unreadable = stats::setNames(reasons[refused], sources[refused]))

}

#' Judge one checkpoint row against the run that would resume it
#'
#' A row that does not record everything this run records (the fingerprint,
#' every data field and every setting) was written by another version of
#' horizons and cannot be checked against this run, so it is dropped and its
#' config re-evaluated; so is every row when the run has no fingerprint of its
#' own (no sample id column). Every other row passes a fixed gate order: the
#' training-data fingerprint, then the tuning settings, then the scoring
#' schema. The fingerprint is compared field by field: the fields carry
#' everything `data_hash` does, and the hash sorts the ids in the session's
#' collation, so the same rows can hash differently under another locale. A recorded value that differs is refused whatever the row's
#' scoring schema, because it means another run's results are in the
#' directory; a row scored under another `SCORING_SCHEMA` is dropped and
#' re-evaluated.
#'
#' @param row One-row result.
#' @param data_fp This run's fingerprint from [eval_data_fingerprint()].
#' @param settings This run's [eval_settings()].
#' @return List with `verdict` (`"keep"`, `"data_mismatch"`,
#'   `"settings_mismatch"` or `"foreign_schema"`, the last for either kind of
#'   dropped row), `fingerprint` (the row's, from
#'   [checkpoint_row_fingerprint()]), `data_differ` (names of data fields
#'   recorded with another value), `stored_settings` (the row's, or `NULL`)
#'   and `settings_differ` (names of settings recorded with another value).
#' @keywords internal
#' @noRd
checkpoint_row_verdict <- function(row, data_fp, settings) {

  row_fp   <- checkpoint_row_fingerprint(row)
  data_cmp <- compare_record(row_fp$data_fields, data_fp$data_fields)
  stored   <- checkpoint_row_settings(row)
  cmp      <- compare_record(stored, settings)

  checkable <- !is.na(row_fp$data_hash) && !is.na(data_fp$data_hash) &&
    length(data_cmp$missing) == 0 && length(cmp$missing) == 0

  verdict <- if (!checkable) {
    "foreign_schema"
  } else if (length(data_cmp$differ) > 0) {
    "data_mismatch"
  } else if (length(cmp$differ) > 0) {
    "settings_mismatch"
  } else if (!isTRUE(row[["scoring_schema"]] == SCORING_SCHEMA)) {
    "foreign_schema"
  } else {
    "keep"
  }

  list(
    verdict         = verdict,
    fingerprint     = row_fp,
    data_differ     = data_cmp$differ,
    stored_settings = stored,
    settings_differ = cmp$differ
  )

}

#' Gate checkpoint rows and keep the ones a run may resume
#'
#' @param candidates `rows` from [read_checkpoint_store()].
#' @param data_fp,settings This run's fingerprint and settings; see
#'   [checkpoint_row_verdict()].
#' @param config_ids The configs in this run's grid, or `NULL` to keep every
#'   id.
#' @return List with `kept` (surviving candidates named by config id),
#'   `refused` (candidates that failed a fingerprint, each with its verdict
#'   fields added), and the counts `n_foreign` (another schema) and
#'   `n_stale` (config not in the grid).
#' @keywords internal
#' @noRd
gate_checkpoint_rows <- function(candidates, data_fp, settings, config_ids = NULL) {

  verdicts <- lapply(candidates, function(cand) {
    checkpoint_row_verdict(cand$row, data_fp, settings)
  })

  verdict <- vapply(verdicts, `[[`, character(1), "verdict")
  ids     <- vapply(candidates, function(cand) {
    as.character(cand$row$config_id)
  }, character(1))

  in_grid <- if (is.null(config_ids)) rep(TRUE, length(ids)) else ids %in% config_ids
  passed  <- verdict == "keep"

  ## One candidate per id: read_checkpoint_store() takes a file only under
  ## its own config's name.
  keep    <- which(passed & in_grid)
  refused <- which(verdict %in% c("data_mismatch", "settings_mismatch"))

  list(
    kept      = stats::setNames(candidates[keep], ids[keep]),
    refused   = Map(c, candidates[refused], verdicts[refused]),
    n_foreign = sum(verdict == "foreign_schema"),
    n_stale   = sum(passed & !in_grid)
  )

}

#' Load the checkpoint rows evaluate() may resume
#'
#' Reads the store, gates every row, and reports: aborts on the first row
#' written on other training data or under other settings, and returns the
#' drops, and the files it could not use, as notes for `evaluate()` to print
#' inside its tree (they used to print here, above the tree's header; #91).
#' A file it could not use costs only time, since its configuration is
#' evaluated again, so it is a note in the log, not a warning.
#'
#' @param output_dir The run's output directory.
#' @param config_ids The configs in this run's grid.
#' @param data_fp,settings This run's fingerprint and settings.
#' @param call The frame to report a refusal from; the default is the
#'   caller's, so the error reads as `evaluate()`'s.
#' @param collector The verb's collector, from `log_open()`, which records
#'   the files it could not use.
#' @return List with `rows` (one-row results named by config id) and `notes`
#'   (character, one tree line per kind of dropped row and per file it could
#'   not use; empty when there were none).
#' @keywords internal
#' @noRd
load_eval_checkpoints <- function(output_dir, config_ids, data_fp, settings,
                                  collector, call = rlang::caller_env()) {

  store <- read_checkpoint_store(output_dir)
  gated <- gate_checkpoint_rows(store$rows, data_fp, settings, config_ids)

  ## -------------------------------------------------------------------------
  ## Refuse a directory holding another run's results
  ## -------------------------------------------------------------------------

  if (length(gated$refused) > 0) {

    first <- gated$refused[[1]]
    diffs <- describe_settings_diff(first$stored_settings, settings,
                                    first$settings_differ)

    if (first$verdict == "data_mismatch") {

      abort_checkpoint_data_mismatch(
        stored        = first$fingerprint,
        current       = data_fp,
        output_dir    = output_dir,
        source        = first$source,
        differ        = first$data_differ,
        settings_diff = diffs,
        call          = call
      )

    }

    abort_checkpoint_settings_mismatch(diffs, output_dir, first$source,
                                       call = call)

  }

  ## -------------------------------------------------------------------------
  ## Report what was dropped, and what could not be read
  ## -------------------------------------------------------------------------

  notes <- character(0)

  if (gated$n_stale > 0) {

    notes <- c(notes, paste0("Dropped ", gated$n_stale,
                             " stale checkpoint entries"))

  }

  ## Rows scored under a different regime (SCORING_SCHEMA) are not comparable
  ## to what this run produces, and ranking them together would make
  ## best_config an artifact of which regime scored each config. Rows that
  ## record less than this run cannot be checked against it.
  if (gated$n_foreign > 0) {

    notes <- c(notes, paste0(
      "Dropped ", gated$n_foreign, " checkpoint row",
      if (gated$n_foreign > 1) "s" else "",
      " written under an earlier schema (will be re-evaluated)"
    ))

  }

  if (length(store$unreadable) > 0) {

    log_unreadable_checkpoints(collector, store$unreadable, output_dir)

    notes <- c(notes, paste0("Could not use ", names(store$unreadable), ": ",
                             unname(store$unreadable), " (will be re-evaluated)"))

  }

  list(
    rows  = lapply(gated$kept, `[[`, "row"),
    notes = notes
  )

}

#' Write one result row to the per-config store
#'
#' Written under a name the store does not list and renamed into place, so a
#' reader (the monitor polls while workers write) never sees half a file.
#'
#' @param row One-row result.
#' @param checkpoint_dir The `checkpoints/` directory.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
write_checkpoint_row <- function(row, checkpoint_dir) {

  ## A unit's records are lifted onto the call's log, not stored: a resumed
  ## row's warnings are read on the row (lift_unit_records()).
  attr(row, "unit_records") <- NULL

  tmp <- tempfile(tmpdir = checkpoint_dir, fileext = ".rds.tmp")
  saveRDS(row, tmp)
  file.rename(tmp, file.path(checkpoint_dir, paste0(row$config_id, ".rds")))

  invisible(NULL)

}

#' Record the files in the store that could not be used
#'
#' @param collector The verb's collector, from `log_open()`.
#' @param unreadable Reasons, named by file relative to `output_dir`: a read
#'   error, not a result row, or a row under a name other than its config's.
#' @param output_dir The run's output directory.
#' @return `NULL`, invisibly; adds one note to the collector, its ids the
#'   files.
#' @keywords internal
#' @noRd
log_unreadable_checkpoints <- function(collector, unreadable, output_dir) {

  n_files <- length(unreadable)
  details <- paste0(names(unreadable), ": ", unname(unreadable))

  ## Each detail is substituted, not inlined, so braces in upstream error
  ## text are never read as cli markup.
  log_note(collector, c(
    "{n_files} file{?s} in {.path {output_dir}} could not be used as {cli::qty(n_files)}{?a checkpoint/checkpoints}, so {cli::qty(n_files)}{?its configuration was/their configurations were} evaluated again.",
    stats::setNames(sprintf("{details[%d]}", seq_len(n_files)), rep("x", n_files))
  ), ids = names(unreadable))

  invisible(NULL)

}

#' Lift what the configurations reported onto the call's log
#'
#' @description
#' [evaluate_single_config()] returns its unit's records on the row, as the
#' attribute `"unit_records"`, since records are data and cross from a worker
#' with the row. The horizons warnings among them go to the call's collector
#' in one pass, with their notes ([log_unit_records()]), so a message several
#' configurations raised is one event naming them all, and the attribute is dropped, so
#' neither the results nor a checkpoint keeps it. Only rows evaluated in this
#' call carry it: a row resumed from a checkpoint reports its warnings on the
#' row.
#'
#' @param rows List of result rows.
#' @param collector The call's collector, from [log_open()].
#' @return `rows`, without the attribute.
#' @keywords internal
#' @noRd
lift_unit_records <- function(rows, collector) {

  log_unit_records(collector, dplyr::bind_rows(lapply(rows, attr, "unit_records")))

  drop_unit_records(rows)

}

#' The results table so far, for a stop to carry
#'
#' @description
#' The rows finished before a stop, bound into a table without their units'
#' records. With none finished it is a table with every column of a results
#' row ([create_failed_result()]) and no rows, so a stop before the first
#' configuration carries the same shape as one after it.
#'
#' @param rows List of result rows.
#' @return Tibble.
#' @keywords internal
#' @noRd
results_so_far <- function(rows) {

  if (length(rows) == 0) return(create_failed_result(NA_character_)[0, ])

  dplyr::bind_rows(drop_unit_records(rows))

}

#' Result rows without their units' records
#'
#' @param rows List of result rows.
#' @return `rows`, each without the attribute `"unit_records"`.
#' @keywords internal
#' @noRd
drop_unit_records <- function(rows) {

  lapply(rows, function(row) {
    attr(row, "unit_records") <- NULL
    row
  })

}

## ---------------------------------------------------------------------------
## Stopping early: the circuit breaker and a worker's death
## ---------------------------------------------------------------------------
## Both stop the call with horizons_error_systemic and the rows so far, since
## every configuration still to run would fail the same way.

#' A circuit breaker for the configurations a sequential run evaluates
#'
#' @description
#' Armed from the start, it watches the configurations as they finish, not
#' the rows loaded from checkpoints. A success, a pruned configuration, or a
#' failure unlike those before it disarms it for the rest of the call. While
#' armed it holds back the rows of the identical failures so far, and it
#' trips once there are `limit` of them across at least two models, while
#' configurations remain to be run (with none left, `evaluate()` releases the
#' rows instead):
#' `configure()`'s grid runs a model's configurations together, so a failure
#' local to one model fills the run first and never trips it.
#'
#' @param limit A whole number of at least 2, or `FALSE` for no breaker
#'   (`control_horizons(breaker = )`).
#' @return An environment: `armed`, `tripped`, the run's `key`, its `held`
#'   rows and their `models`.
#' @keywords internal
#' @noRd
new_breaker <- function(limit) {

  breaker         <- new.env(parent = emptyenv())
  breaker$limit   <- limit
  breaker$armed   <- !isFALSE(limit)
  breaker$tripped <- FALSE
  breaker$key     <- NULL
  breaker$held    <- list()
  breaker$models  <- character(0)

  breaker

}

#' Feed the breaker one configuration's row
#'
#' @param breaker From [new_breaker()].
#' @param row The row the configuration returned.
#' @param model Character(1). The configuration's model.
#' @return List of the rows that may be checkpointed now: `row`, with the
#'   held rows when it disarms the breaker, or none while the breaker holds
#'   it. Sets `breaker$tripped` when this row trips it.
#' @keywords internal
#' @noRd
breaker_feed <- function(breaker, row, model) {

  if (!breaker$armed) return(list(row))

  key <- if (identical(row$status, "failed")) breaker_key(row)

  if (is.null(key) || (!is.null(breaker$key) && !identical(key, breaker$key))) {

    return(c(breaker_release(breaker), list(row)))

  }

  breaker$key    <- key
  breaker$held   <- c(breaker$held, list(row))
  breaker$models <- c(breaker$models, model)

  breaker$tripped <- length(breaker$held) >= breaker$limit &&
    length(unique(breaker$models)) >= 2L

  list()

}

#' Disarm the breaker, releasing the rows it held
#'
#' @param breaker From [new_breaker()].
#' @return List of the held rows.
#' @keywords internal
#' @noRd
breaker_release <- function(breaker) {

  held          <- breaker$held
  breaker$held  <- list()
  breaker$armed <- FALSE

  held

}

#' What makes two failures the same, for the breaker
#'
#' @param row A failed result row.
#' @return Character(1): the failure's class and its message, with the
#'   configuration's id, the numbers and the spacing set aside. The message
#'   starts with the step, so a failure in another step is another failure.
#' @keywords internal
#' @noRd
breaker_key <- function(row) {

  message <- gsub(row$config_id, "<config>", row$error_message, fixed = TRUE)
  message <- gsub("[0-9]+", "#", message)
  message <- gsub("[[:space:]]+", " ", trimws(message))

  paste(row$class, message, sep = "\r")

}

#' Stop the call because the breaker tripped
#'
#' @param breaker From [new_breaker()], tripped.
#' @param results The results table so far, carried as `results`.
#' @param n_left Integer(1). The units it would still have run (not those
#'   resumed from checkpoints).
#' @param verb Character(1). The verb that stops, `"evaluate"` or `"fit"`.
#' @param units Character(1). What its units are called, plural.
#' @param checkpoints Logical(1). Whether the verb checkpoints its units, so
#'   the error says the held rows were not.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_systemic`
#'   (kind `run`), the results so far as `results`.
#' @keywords internal
#' @noRd
abort_breaker_trip <- function(breaker,
                               results,
                               n_left,
                               verb        = "evaluate",
                               units       = "configurations",
                               checkpoints = TRUE,
                               call        = rlang::caller_env()) {

  n_failed <- length(breaker$held)
  n_models <- length(unique(breaker$models))
  cause    <- breaker$held[[1]]$error_message

  abort_horizons(c(
    "The first {n_failed} {units} to run all failed the same way, across {n_models} models, so {.fn {verb}} stopped rather than run the other {n_left}.",
    "x" = "{cause}",
    "i" = if (checkpoints) {
      "Their rows are in this error's {.field results} field and were not checkpointed, so a run after the fix evaluates them."
    } else {
      "Their rows are in this error's {.field results} field."
    },
    "i" = "To run every one anyway, use {.code control_horizons(breaker = FALSE)}; to see the first failure's backtrace, {.code control_horizons(debug = TRUE)}."
  ), kind = "run", class = "horizons_error_systemic", results = results, call = call)

}

#' Stop the call when a parallel worker dies
#'
#' @description
#' future raises a worker's death as a `FutureError`, which no unit contains
#' (`NEVER_CONTAINED`). A death that tune caught during a Bayesian search
#' reaches here as the unit's `horizons_error_systemic`, which no unit
#' contains either ([worker_death_signal()]). Either is turned into
#' `horizons_error_systemic`, chained to the error, with the rows finished so
#' far; every other error passes through untouched.
#'
#' @param expr The work that dispatches to workers.
#' @param results The results table finished so far, carried as `results`;
#'   evaluated only when a worker dies.
#' @param output_dir The run's `output_dir`, or `NULL`.
#' @param verb Character(1). The verb that stops, `"evaluate"` or `"fit"`.
#' @param call The frame the error reports.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
stop_on_worker_death <- function(expr, results, output_dir = NULL, verb = "evaluate",
                                 call = rlang::caller_env()) {

  ## One handler for both classes. try_fetch() nests its handlers, so with
  ## one per class the systemic error raised for a FutureError would be
  ## caught by the other and wrapped a second time.
  died <- function(e) {

    if (!inherits(e, c("FutureError", "horizons_error_systemic"))) return(rlang::zap())

    abort_horizons(c(
      "A parallel worker stopped while {.fn {verb}} was running, so the run cannot go on.",
      "i" = if (is.null(output_dir)) {
        "The rows finished so far are in this error's {.field results} field."
      } else {
        "The configurations finished are checkpointed in {.path {output_dir}}; calling {.fn {verb}} again with it resumes from them."
      }
    ), kind = "run", class = "horizons_error_systemic", parent = e,
    results = results, call = call)

  }

  rlang::try_fetch(expr, error = died)

}

#' Whether a condition is future.apply's report of a future's error
#'
#' @description
#' future.apply reports `"Caught <class>. Canceling all iterations ..."`
#' before it raises a future's error again: as a warning before 1.21.0, and
#' as a message since. tune's Bayesian search catches that error in a
#' `try()` and goes on, so on the resamples axis a worker's death during an
#' iteration reaches the unit only as this report, which has no class in
#' either version and is matched by its text. Only the classes of future's
#' own errors (`FutureError` and its subclasses, `FutureInterruptError`
#' among them) match: an error raised in a worker's own code is caught and
#' reported the same way.
#'
#' @param cnd A warning or a message.
#' @return Logical(1).
#' @keywords internal
#' @noRd
worker_death_signal <- function(cnd) {

  grepl("^Caught [A-Za-z]*Future[A-Za-z]*Error\\. Canceling all iterations",
        conditionMessage(cnd))

}

## ---------------------------------------------------------------------------
## Checkpoint settings provenance
## ---------------------------------------------------------------------------
## The data fingerprint identifies the rows a checkpoint was scored on; this
## records how it was tuned. Settings are compared by name rather than hashed
## whole, so a refusal can name what changed, and a row that records no value
## for a setting added to the list later is re-evaluated rather than refused.

#' Record the settings a result row depends on
#'
#' @param ... Named settings. Numbers are stored as doubles, so `10L` and `10`
#'   record the same value.
#' @return Named list.
#' @keywords internal
#' @noRd
eval_settings <- function(...) {

  normalize_eval_settings(list(...))

}

#' @rdname eval_settings
#' @noRd
normalize_eval_settings <- function(settings) {

  lapply(settings, function(v) if (is.numeric(v)) as.double(v) else v)

}

#' Attach the settings record to a result row
#'
#' @param row One-row result.
#' @param settings Record from [eval_settings()].
#' @return `row` with a `settings` list-column.
#' @keywords internal
#' @noRd
stamp_eval_settings <- function(row, settings) {

  row$settings <- list(settings)
  row

}

#' Settings record carried by one checkpoint row
#'
#' @param row One-row result read back from a checkpoint.
#' @return Named list, or `NULL` when the row carries none.
#' @keywords internal
#' @noRd
checkpoint_row_settings <- function(row) {

  if (!"settings" %in% names(row)) return(NULL)

  s <- row[["settings"]][[1]]

  if (!is.list(s)) return(NULL)

  normalize_eval_settings(s)

}

#' Compare a row's record with this run's, field by field
#'
#' Used for the tuning settings and for the training-data fields alike.
#'
#' @param stored The row's record, or `NULL`.
#' @param current This run's record, or `NULL` when unknown.
#' @return List with `differ` (fields both record, with different values)
#'   and `missing` (fields this run records and the row does not).
#' @keywords internal
#' @noRd
compare_record <- function(stored, current) {

  stored  <- normalize_eval_settings(stored %||% list())
  current <- normalize_eval_settings(current %||% list())

  ## `%in%` rather than intersect()/setdiff(): this runs once per checkpoint
  ## row on every monitor poll, and the set functions cost a tryCatch each.
  wanted  <- as.character(names(current))
  present <- wanted %in% names(stored)
  shared  <- wanted[present]

  same <- vapply(shared, function(nm) identical(stored[[nm]], current[[nm]]),
                 logical(1), USE.NAMES = FALSE)

  list(
    differ  = shared[!same],
    missing = wanted[!present]
  )

}

#' Describe differing settings for a message
#'
#' @param stored,current Settings records.
#' @param differ Names of the settings that differ.
#' @return Character vector, one `"name = old (this run: new)"` per setting.
#' @keywords internal
#' @noRd
describe_settings_diff <- function(stored, current, differ) {

  vapply(differ, function(nm) {
    paste0(nm, " = ", format_setting_value(stored[[nm]]),
           " (this run: ", format_setting_value(current[[nm]]), ")")
  }, character(1), USE.NAMES = FALSE)

}

#' Format one setting's value for a message
#'
#' @param v A setting's value.
#' @return Character scalar.
#' @keywords internal
#' @noRd
format_setting_value <- function(v) {

  if (length(v) == 1 && is.na(v)) return("NA")

  ## trim: format() pads a vector's elements to one width, which put a
  ## second space into "-Inf,  Inf" for the outcome range (#76)
  paste(format(v, trim = TRUE), collapse = ", ")

}

#' Abort on checkpoints tuned under different settings
#'
#' @param diffs Differences from [describe_settings_diff()].
#' @param output_dir The checkpoint directory being resumed.
#' @param source Which file carried the mismatching settings.
#' @param call The frame to report the error from (`evaluate()`'s).
#' @return Never returns; aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
abort_checkpoint_settings_mismatch <- function(diffs, output_dir, source,
                                               call = rlang::caller_env()) {

  abort_horizons(c(
    "Checkpoints in {.path {output_dir}} were tuned under different settings.",
    "x" = "{.file {source}} was tuned with {diffs}.",
    "i" = "Resuming would rank results tuned under two sets of settings together, and warm-start {.fn fit} from them.",
    "i" = "Use a different {.arg output_dir}, or delete the stale checkpoints in {.path {output_dir}}."
  ), kind = "input", call = call)

}

## ---------------------------------------------------------------------------
## Checkpoint data provenance
## ---------------------------------------------------------------------------
## A checkpoint is keyed by config_id, which identifies the pipeline but not
## the rows it was scored on. These helpers attach the identity of the
## training rows to every checkpoint write and check it on every read.

#' Fingerprint the training data a run is scoring
#'
#' The hash is over the sorted identifier column plus the name of the outcome
#' column, so it is invariant to row order and to the split's RNG but changes
#' the moment either the row set or the response changes.
#'
#' The outcome is in the hash because `generate_config_id()` does not hash it:
#' `configure(outcome = "clay")` and `configure(outcome = "oc")` on the same
#' rows produce the same config ids, so without this the two runs would resume
#' each other's CV results in a shared `output_dir`. That became reachable when
#' `select_training()` started returning every pool response on one object. The
#' per-config transformation needs no such treatment — it is already part of
#' the config id, so it cannot collide.
#'
#' The hash says nothing about the values on those rows, so re-standardized
#' spectra (#64 moved the grid for the same samples) or a rescaled outcome
#' under the same name resumed stale results silently (#42). `data_fields`
#' records them by name: the outcome, the sample ids, the role-map rows whose
#' roles shape a row (`id`, `outcome`, `predictor`), and the outcome and
#' predictor values in id order. They are compared field by field, like the
#' tuning settings, so a refusal can say what changed, and a row with no
#' value for a field this run records is re-evaluated rather than refused.
#' Each value field hashes column by column, so the cost is one pass over
#' the matrix (about 0.3 s at 17,788 x 1,701) with one column in
#' memory at a time; it runs once per run, and the parallel worker is sent
#' the result rather than recomputing it.
#'
#' @param train_data Training rows (the analysis half of the split).
#' @param role_map The object's role map; the `"id"` role names the identifier
#'   column (falling back to `sample_id`), the `"outcome"` role the response,
#'   and the `"predictor"` role the value columns hashed.
#' @return List with `data_hash` (character, `NA` when no identifier column is
#'   available), `data_n_rows` (integer) and `data_fields` (named list of
#'   character: `outcome`, `ids`, `roles`, `outcome_values`, `predictors`;
#'   `NULL` when no identifier column is available).
#' @keywords internal
#' @noRd
eval_data_fingerprint <- function(train_data, role_map = NULL) {

  has_roles <- !is.null(role_map) && "role" %in% names(role_map)

  id_col <- if (has_roles) id_column(role_map) else "sample_id"

  outcome_col <- if (has_roles) {
    role_map$variable[role_map$role == "outcome"]
  } else {
    character(0)
  }

  outcome_col <- if (length(outcome_col) > 0) {
    sort(as.character(outcome_col))
  } else {
    NA_character_
  }

  ## No identifier column: the run is unverifiable rather than wrongly
  ## verified, so the checkpoint gate resumes no row into it.
  if (!id_col %in% names(train_data)) {

    return(list(
      data_hash   = NA_character_,
      data_n_rows = as.integer(nrow(train_data)),
      data_fields = NULL
    ))

  }

  ids <- as.character(train_data[[id_col]])

  ## Unchanged from its first version, which every checkpoint and manifest
  ## records. Its presence marks a fingerprinted row; its value is not
  ## compared, since sort() follows the session's collation and the fields
  ## below carry the same ids in a fixed order.
  data_hash <- digest::digest(list(
    ids     = sort(ids),
    outcome = outcome_col
  ))

  ## -------------------------------------------------------------------------
  ## The values on those rows, by name
  ## -------------------------------------------------------------------------
  ## Radix order is locale-independent, so results agree across machines.

  by_id <- order(ids, method = "radix")

  role_cols <- function(role) {
    if (has_roles) as.character(role_map$variable[role_map$role == role]) else character(0)
  }

  ## Only the roles that shape a row's contents. build_recipe() models the
  ## outcome on the predictors, and the id names the rows; a sibling
  ## `response` becomes `response_hold` and `meta` stays `meta`, neither
  ## reaching the model. So add_response() of another property, or a new meta
  ## column, between two runs does not refuse a resume.
  roles <- if (has_roles) {
    shaping <- role_map$role %in% c("id", "outcome", "predictor")
    data.frame(variable = as.character(role_map$variable[shaping]),
               role     = as.character(role_map$role[shaping]))
  } else {
    NULL
  }

  data_fields <- list(
    outcome        = paste(outcome_col, collapse = ", "),
    ids            = digest::digest(ids[by_id], algo = "xxhash64"),
    roles          = digest::digest(roles, algo = "xxhash64"),
    outcome_values = hash_columns(train_data, role_cols("outcome"), by_id),
    predictors     = hash_columns(train_data, role_cols("predictor"), by_id)
  )

  list(
    data_hash   = data_hash,
    data_n_rows = as.integer(nrow(train_data)),
    data_fields = data_fields
  )

}

#' Hash columns of a table, in a given row order
#'
#' One column at a time, so only one reordered column is in memory, and the
#' column names are part of the hash.
#'
#' @param data Data frame.
#' @param cols Columns to hash; any not in `data` are skipped.
#' @param ord Row order.
#' @return Character scalar.
#' @keywords internal
#' @noRd
hash_columns <- function(data, cols, ord) {

  cols <- intersect(cols, names(data))

  per_column <- vapply(cols, function(cl) {
    digest::digest(data[[cl]][ord], algo = "xxhash64")
  }, character(1))

  digest::digest(per_column, algo = "xxhash64")

}

#' Attach a fingerprint to a result row
#'
#' @param row One-row result tibble.
#' @param fp Fingerprint from [eval_data_fingerprint()].
#' @return `row` with `data_hash` and `data_n_rows` columns and a
#'   `data_fields` list-column.
#' @keywords internal
#' @noRd
stamp_data_fingerprint <- function(row, fp) {

  row$data_hash   <- fp$data_hash
  row$data_n_rows <- fp$data_n_rows
  row$data_fields <- list(fp$data_fields)
  row

}

#' Fingerprint carried by one checkpoint row
#'
#' @param row One-row result tibble read back from a checkpoint.
#' @return List with `data_hash`, `NA` when the row carries none, and
#'   `data_fields`, `NULL` when the row carries none.
#' @keywords internal
#' @noRd
checkpoint_row_fingerprint <- function(row) {

  ## `[[` rather than `$`: tibble's `$` warns on a missing column, and a row
  ## from another version may lack these.
  h <- if ("data_hash" %in% names(row)) row[["data_hash"]] else NULL
  f <- if ("data_fields" %in% names(row)) row[["data_fields"]][[1]] else NULL

  list(
    data_hash   = if (is.null(h) || length(h) != 1 || is.na(h)) {
      NA_character_
    } else {
      as.character(h)
    },
    data_fields = if (is.list(f)) f else NULL
  )

}

#' Abort on a checkpoint written against different training data
#'
#' @param stored,current Fingerprints from [checkpoint_row_fingerprint()] and
#'   [eval_data_fingerprint()].
#' @param output_dir The checkpoint directory being resumed.
#' @param source Which file carried the mismatching fingerprint.
#' @param differ Names of the `data_fields` that differ.
#' @param settings_diff Settings the same row records with another value,
#'   from [describe_settings_diff()]. A changed `seed` moves the split, so it
#'   surfaces here as a data mismatch; naming it says why.
#' @param call The frame to report the error from (`evaluate()`'s).
#' @return Never returns; aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
abort_checkpoint_data_mismatch <- function(stored, current, output_dir, source,
                                           differ = character(0),
                                           settings_diff = character(0),
                                           call = rlang::caller_env()) {

  what <- describe_data_diff(stored$data_fields, current$data_fields, differ)

  ## Descriptions are substituted, not inlined, so a brace in an outcome name
  ## is never read as cli markup. The gate refuses only on a field the row and
  ## the run both record with different values, so a refused row always names
  ## at least one.
  found <- stats::setNames(
    sprintf("{.file {source}} %s", sprintf("{what[%d]}", seq_along(what))),
    rep("x", length(what))
  )

  abort_horizons(c(
    "Checkpoints in {.path {output_dir}} were written on different training data.",
    found,
    if (length(settings_diff) > 0) c("i" = "That checkpoint was also tuned with {settings_diff}, which may be why."),
    "i" = "Resuming would reuse cross-validated results, and warm-start {.fn fit}, from hyperparameters tuned on the wrong rows.",
    if ("outcome" %in% differ) c("i" = "Keep one {.arg output_dir} per outcome."),
    "i" = "Use a different {.arg output_dir}, or delete the stale checkpoints in {.path {output_dir}}."
  ), kind = "input", call = call)

}

#' Describe differing data fields for a message
#'
#' @param stored,current `data_fields` records.
#' @param differ Names of the fields that differ.
#' @return Character vector, one clause per field, each completing a sentence
#'   that starts with the checkpoint's file name.
#' @keywords internal
#' @noRd
describe_data_diff <- function(stored, current, differ) {

  clauses <- c(
    ids            = "was scored on a different set of training samples.",
    roles          = "was scored with different id, outcome or predictor columns (role_map).",
    outcome_values = "was scored on different outcome values for the same samples.",
    predictors     = "was scored on different predictor values for the same samples (re-standardized or re-processed spectra, for example)."
  )

  out <- character(0)

  if ("outcome" %in% differ) {

    out <- paste0("was computed for outcome ", stored$outcome,
                  "; this run models ", current$outcome, ".")

  }

  c(out, unname(clauses[intersect(names(clauses), differ)]))

}

## ---------------------------------------------------------------------------
## abort_all_configs_failed — every configuration failed, with the reasons attached
## ---------------------------------------------------------------------------

#' Abort because every configuration failed
#'
#' @description
#' Raised by `evaluate()` when nothing usable came out: every configuration
#' failed (#41). A configuration that was evaluated but has no value of the
#' ranking metric is usable, so `evaluate()` returns instead. `evaluate()`
#' aborts before it assigns `x$evaluation`, so the results table travels on
#' the condition as `results`, and the message lists the distinct error
#' messages (the first three, each with the configs that raised it, and a
#' count of the rest). A caller looping over subsets can catch the class and
#' keep the per-config errors; see `evaluate()`'s "When every configuration
#' fails" section.
#'
#' Rows resumed from checkpoints are not re-run by calling `evaluate()`
#' again, so the message names them and the files to delete. They are
#' identified by `evaluate()` from the ids its loader returned, without
#' reading the checkpoint files again. Each lives in
#' `checkpoints/<config_id>.rds`.
#'
#' @param results The aggregated result rows (`evaluation$results` shape).
#' @param checkpoint_ids Character. Config ids among `results` that were
#'   loaded from checkpoints rather than run. Default none.
#' @param output_dir The checkpoint directory, or `NULL`.
#' @param call The call the condition is attributed to. Default: the caller,
#'   `evaluate()`.
#' @return Never returns; aborts with class
#'   `horizons_error_all_configs_failed` (kind `run`).
#' @keywords internal
#' @noRd
abort_all_configs_failed <- function(results,
                                     checkpoint_ids = character(0),
                                     output_dir     = NULL,
                                     call           = rlang::caller_env()) {

  n_total <- nrow(results)

  ## Resumed rows are not re-run by calling evaluate() again, so a failure
  ## loaded from a checkpoint persists until its files are deleted. Built as
  ## plain text (the directory is the caller's), escaped for cli.
  checkpoint_note <- NULL

  if (length(checkpoint_ids) > 0 && !is.null(output_dir)) {

    n_ckpt    <- length(checkpoint_ids)
    ids_shown <- paste(utils::head(checkpoint_ids, 3), collapse = ", ")

    if (n_ckpt > 3) ids_shown <- paste0(ids_shown, " and ", n_ckpt - 3, " more")

    files <- if (n_ckpt == 1) {
      paste0("checkpoints/", checkpoint_ids, ".rds")
    } else {
      "their files, checkpoints/<config_id>.rds"
    }

    checkpoint_note <- c("i" = cli_escape(paste0(
      n_ckpt, if (n_ckpt == 1) " configuration was" else " configurations were",
      " loaded from checkpoints in ", output_dir, " rather than run (",
      ids_shown, "). To re-run ", if (n_ckpt == 1) "it" else "them",
      ", delete ", files, "."
    )))

  }

  abort_horizons(c(
    "All configurations failed, so none can be ranked.",
    "i" = "{n_total} configuration{?s} failed.",
    distinct_config_errors(results),
    checkpoint_note,
    "i" = "The per-config results, error messages included, are on this condition as {.field results}. Recover them without re-running with {.code rlang::last_error()$results}, or catch the class: {.code tryCatch(evaluate(x), horizons_error_all_configs_failed = function(e) e$results)}."
  ), kind = "run", class = "horizons_error_all_configs_failed", results = results,
  call = call)

}

## ---------------------------------------------------------------------------
## ranking_candidates \u2014 the one candidate rule evaluate() and fit() share
## ---------------------------------------------------------------------------

#' The result rows eligible for ranking
#'
#' @description
#' Returns the rows `evaluate()` picks `best_config` from and `fit()` picks
#' its members from: the configurations that succeeded, or, when none did,
#' the pruned configurations. [rank_configs_by_cv()] then orders those with a
#' cross-validated value of the ranking metric, and the rest are the ones the
#' verb says it left out. `fit()` used to keep successes only, so it refused
#' an evaluation whose `best_config` was a pruned configuration (#38).
#'
#' @param results Tibble of evaluation result rows (`evaluation$results`
#'   shape).
#' @param metric Bare metric name, e.g. `"rpd"`.
#' @return List with `rows` (the candidate rows, unranked, with or without a
#'   value of the metric; zero rows when nothing succeeded or was pruned),
#'   `fallback` (`TRUE` when the rows are pruned configurations because none
#'   succeeded) and `what` (`"succeeded"` or `"pruned"`, for the messages).
#' @keywords internal
#' @noRd
ranking_candidates <- function(results, metric) {

  successes <- results[results$status %in% "success", , drop = FALSE]

  if (nrow(successes) > 0) return(list(rows = successes, fallback = FALSE, what = "succeeded"))

  pruned <- results[results$status %in% "pruned", , drop = FALSE]

  list(rows = pruned, fallback = nrow(pruned) > 0, what = "pruned")

}

#' The metrics an evaluation can be ranked by
#'
#' @description
#' The ranking metrics for which [ranking_candidates()] holds a row with a
#' cross-validated value: what `evaluate()` and `fit()` suggest when the one
#' asked for ranks nothing, so neither suggests a metric that would be
#' refused the same way.
#'
#' @param results Tibble of evaluation result rows.
#' @return Character, a subset of `VALID_RANK_METRICS`.
#' @keywords internal
#' @noRd
rankable_metrics <- function(results) {

  VALID_RANK_METRICS[vapply(VALID_RANK_METRICS, function(m) {
    rows <- ranking_candidates(results, m)$rows
    any(!is.na(rows[[paste0("cv_", m)]]))
  }, logical(1))]

}

#' Why nothing could be ranked, for the warning and the error that say so
#'
#' @param results Tibble of evaluation result rows.
#' @param metric Bare metric name, which ranks nothing.
#' @param how Character(1). How to rank by another metric, as rendered text,
#'   e.g. ``"`fit(x, metric = )`"``.
#' @return Character, rendered: the reason, then, as `"i"` bullets, where
#'   pruned rows hold the metric and what to rank by instead.
#' @keywords internal
#' @noRd
unranked_reason <- function(results, metric, how) {

  candidates  <- ranking_candidates(results, metric)
  rank_column <- paste0("cv_", metric)
  n           <- nrow(candidates$rows)
  held        <- rankable_metrics(results)

  ## Pruned rows are ranked only when nothing succeeded, so a value they
  ## hold is not one this ranking could use
  pruned_with <- if (identical(candidates$what, "succeeded")) {
    sum(results$status %in% "pruned" & !is.na(results[[rank_column]]))
  } else 0L

  c(
    if (n == 0) {
      cli::format_inline("None succeeded, and no configuration was pruned.")
    } else if (identical(candidates$what, "succeeded")) {
      cli::format_inline("{n} succeeded, but none has a {.field {rank_column}} value.")
    } else {
      cli::format_inline("None succeeded, and none of the {n} pruned configuration{?s} has a {.field {rank_column}} value.")
    },
    if (pruned_with > 0) {
      c("i" = cli::format_inline("{pruned_with} pruned configuration{?s} {?has/have} one, but pruned configurations are ranked only when none succeeded."))
    },
    "i" = if (length(held) > 0) {
      cli::format_inline("Rank by a metric the results hold: {how} with one of {.val {held}}.")
    } else {
      "No cross-validated metric was recorded; re-run `evaluate()`."
    }
  )

}

## ---------------------------------------------------------------------------
## rank_configs_by_cv \u2014 the one ranking rule evaluate() and fit() share
## ---------------------------------------------------------------------------

#' Rank evaluation result rows on the cross-validated metric
#'
#' @description
#' Orders result rows best-first by `cv_<metric>`, the cross-validated mean at
#' each config's selected hyperparameters. This is the single ranking rule for
#' `evaluate()`'s `best_config` and `fit()`'s member selection, so the two can
#' never disagree, and neither touches a test-set column (#50).
#'
#' Rows whose `cv_<metric>` is `NA` cannot be ranked and are left out,
#' silently: the verbs that rank say so once, with the ids, and the monitor
#' only displays the ranking. When no row has a value the result has no rows.
#'
#' @param results Tibble of evaluation result rows (`evaluation$results`
#'   shape).
#' @param metric Bare metric name, e.g. `"rpd"`. Direction comes from
#'   `HIGHER_BETTER_METRICS`.
#' @return `results` with unrankable rows removed, ordered best-first.
#' @keywords internal
rank_configs_by_cv <- function(results, metric) {

  ### The eval validator at fit()'s door requires the cv_ columns.
  rank_column <- paste0("cv_", metric)

  results <- results[!is.na(results[[rank_column]]), , drop = FALSE]
  vals    <- results[[rank_column]]

  ## Explicit tie-break on config_id so a fresh run, a resumed run (whose row
  ## order follows the checkpoint directory) and monitor_evaluate() all name
  ## the same winner when two configs tie.
  key <- if (metric %in% HIGHER_BETTER_METRICS) -vals else vals

  results[order(key, results$config_id), , drop = FALSE]

}

#' Record the configurations a ranking left out
#'
#' @description
#' The one record of [rank_configs_by_cv()]'s left-out rows, which
#' `evaluate()` and `fit()` both write: a `skipped` warning naming them, raised
#' at the verb's end.
#'
#' @param collector The verb's collector, from `log_open()`.
#' @param candidates The rows that were ranked.
#' @param ranked What [rank_configs_by_cv()] returned for them.
#' @param metric Bare metric name.
#' @return The ids left out, invisibly.
#' @keywords internal
#' @noRd
log_unranked <- function(collector, candidates, ranked, metric) {

  rank_column <- paste0("cv_", metric)
  unranked    <- setdiff(candidates$config_id, ranked$config_id)

  if (length(unranked) > 0) {

    log_warning(collector, "skipped", c(
      "{length(unranked)} configuration{?s} left out of the ranking, with no {.field {rank_column}} value.",
      "i" = "Left out: {.val {unranked}}."
    ), ids = unranked)

  }

  invisible(unranked)

}

## ---------------------------------------------------------------------------
## modelling_rows — the one row rule every modelling verb shares
## ---------------------------------------------------------------------------

#' Keep the analysis rows a model can be fitted on
#'
#' @description
#' Leaves out the rows whose outcome is `NA` and the rows a step flagged
#' (`.flag` not `NA`), keeping both in the object. This is the single rule for
#' which rows the modelling verbs model (#67): `evaluate()` draws its split
#' from the rows it returns, and `fit()` reuses that split and applies the
#' same rule to check that the split's ids and outcomes still match the
#' object's. A flagged row is a sample a step kept but could not use, so it is
#' left out wherever a model is fitted, as a row without an outcome is.
#'
#' Rows are dropped per outcome, here, rather than when responses are joined.
#' An object can carry several responses (`select_training()` puts every pool
#' response on one object), and a row missing one of them may carry another.
#'
#' @param analysis Data frame. The object's analysis table.
#' @param outcome_col Character. Name of the outcome column, which the door
#'   validator has checked is in `analysis`.
#' @param call The frame a refusal reports: the verb's.
#' @return List with `data` (the rows kept, in their original order),
#'   `n_dropped` (integer, the rows without an outcome) and `n_flagged`
#'   (integer, the flagged rows with an outcome). Aborts with class
#'   `horizons_error_input` when no row is left.
#' @keywords internal
#' @noRd
modelling_rows <- function(analysis, outcome_col, call = rlang::caller_env()) {

  na_mask <- is.na(analysis[[outcome_col]])
  flagged <- flagged_rows(analysis)
  drop    <- na_mask | flagged

  if (all(na_mask)) {

    abort_horizons(c(
      "All outcome values are NA, so there are no rows to model.",
      "i" = "Outcome column: {.field {outcome_col}}.",
      "i" = "Add measured values of it, or {.fn configure} another outcome."
    ), kind = "input", call = call)

  }

  if (all(drop)) {

    abort_horizons(c(
      "Every row with an outcome is flagged, so there are no rows to model.",
      "i" = "{.field .flag} names the step that could not use each one."
    ), kind = "input", call = call)

  }

  ## Return the table untouched when nothing is dropped, so an object without
  ## NA outcomes or flags splits exactly as it did before this helper existed.
  list(
    data      = if (any(drop)) analysis[!drop, , drop = FALSE] else analysis,
    n_dropped = sum(na_mask),
    n_flagged = sum(flagged & !na_mask)
  )

}

#' The rows a step flagged
#'
#' @description
#' The one rule every verb that computes over predictors reads the flag by: a
#' row whose `.flag` is not `NA` was kept by a step that could not use it.
#' Without a `.flag` column every row is clean. [modelling_rows()] leaves
#' flagged rows out of every model; `predict()` returns them as `NA`.
#'
#' @param data A data frame: an analysis table, or `predict()`'s `new_data`.
#' @return Logical, one per row.
#' @keywords internal
#' @noRd
flagged_rows <- function(data) {

  if (".flag" %in% names(data)) !is.na(data$.flag) else rep(FALSE, nrow(data))

}

## ---------------------------------------------------------------------------
## The small-sample rule evaluate() and fit() share
## ---------------------------------------------------------------------------
## Small n is weak, not wrong, so it runs. The one sample size refused is a
## training part without more rows than folds, which rsample cannot split
## into folds that each have rows to fit on and to assess; a small
## assessment fold warns. The trigger is the realised
## folds, not the total n.

#' Refuse a training part without more rows than cross-validation folds
#'
#' @param n_train Integer. Rows of the training part, after the split and
#'   any response trim.
#' @param cv_folds Integer. `config$tuning$cv_folds`.
#' @param n_modelled,n_test,n_trimmed,n_calib Integer. The modelled rows,
#'   and those the split held out for the test set, the trim left out or
#'   `fit()` set aside for calibration, for the message.
#' @param call The frame the refusal reports: the verb's.
#' @return `NULL`, invisibly; aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_fold_rows <- function(n_train, cv_folds, n_modelled, n_test, n_trimmed = 0L,
                            n_calib = 0L, call = rlang::caller_env()) {

  if (n_train > cv_folds) return(invisible(NULL))

  held <- c(paste0("the test set holds ", n_test),
            if (n_calib > 0) paste0("the calibration set ", n_calib),
            if (n_trimmed > 0) paste0("the response trim left out ", n_trimmed))
  elsewhere <- if (length(held) == 1) held else {
    paste0(paste(utils::head(held, -1), collapse = ", "), " and ", utils::tail(held, 1))
  }

  abort_horizons(c(
    "The training part has {n_train} row{?s}, but {cv_folds}-fold cross-validation needs more training rows than folds.",
    "i" = "{n_modelled} row{?s} {?has/have} an observed outcome; {elsewhere}.",
    "i" = "Add rows, or {.fn configure} fewer {.arg cv_folds}."
  ), kind = "input", call = call)

}

#' Warn once when the smallest assessment fold is small
#'
#' @param collector The verb's collector, from `log_open()`.
#' @param folds The `rset` the verb tunes on.
#' @return The smallest fold's assessment rows, invisibly.
#' @keywords internal
#' @noRd
log_small_folds <- function(collector, folds) {

  n_rows   <- nrow(folds$splits[[1]]$data)
  smallest <- min(vapply(folds$splits, function(s) n_rows - length(s$in_id), integer(1)))

  if (smallest < SMALL_FOLD_ROWS) {

    log_warning(collector, "quality", c(
      "The smallest cross-validation fold has {smallest} row{?s} to assess, so the cross-validated metrics rest on few rows.",
      "i" = "{n_rows} training rows in {nrow(folds)} folds; a metric undefined on a fold is NA."
    ))

  }

  invisible(smallest)

}

#' Check a verb's ranking metric
#'
#' @param metric The verb's `metric` argument.
#' @param call The frame the refusal reports: the verb's.
#' @return `NULL`, invisibly; aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_rank_metric_arg <- function(metric, call = rlang::caller_env()) {

  if (rlang::is_string(metric) && metric %in% VALID_RANK_METRICS) {

    return(invisible(NULL))

  }

  abort_horizons(c(
    "{.arg metric} must be one of {.val {VALID_RANK_METRICS}}.",
    "x" = "Got {.val {metric}}."
  ), kind = "input", call = call)

}

## ---------------------------------------------------------------------------
## draw_eval_split — the one train/test draw evaluate() and fit() share
## ---------------------------------------------------------------------------

#' Draw evaluate()'s train/test split
#'
#' @description
#' The split `evaluate()` scores on and `fit()` reuses: the rows with an
#' observed outcome ([modelling_rows()]), then `set.seed(seed)`, then
#' a `SPLIT_PROP` split stratified on the outcome, falling back to an
#' unstratified one when the stratified draw fails. The split can also come
#' out unstratified with no failure: rsample drops the strata itself when it
#' cannot bin the outcome (under 40 rows, warning "Too little data to
#' stratify"; or silently, when a few-valued outcome pools into one stratum or
#' tied quantiles leave one bin). The object's `strata` attribute names the
#' outcome either way, so `stratified` in the return value is the record of
#' which happened (#91; see [draw_stratified()]). `fit()` calls it when it
#' starts cold from a configured object with one configuration (#45), so that
#' fit holds out exactly the rows `evaluate()` would have at the same seed.
#' The draw was inline in `evaluate()` before, and it is unchanged.
#'
#' It seeds the global RNG, as `evaluate()` always has: `evaluate()`'s CV
#' folds are drawn from the state it leaves.
#'
#' @param analysis Data frame. The object's analysis table. Rows whose
#'   outcome is `NA` are dropped here; a table the callers have already
#'   filtered for their sample-size gate passes through uncopied.
#' @param outcome_col Character. Name of the outcome column.
#' @param seed Integer. The seed passed to `evaluate()` (or to `fit()` on a
#'   cold start).
#' @param call The frame a refusal reports: the verb's.
#' @return List with `split` (the `rsplit`), `n_dropped` (integer, the rows
#'   whose outcome is `NA`), `stratified` (`FALSE` when the split is
#'   unstratified, because rsample dropped the strata or the stratified draw
#'   failed; see [draw_stratified()]), `strata_failed` (`TRUE` when the
#'   stratified draw failed and was retried without strata) and
#'   `strata_error` (why it failed, or `NA`). Aborts as [modelling_rows()]
#'   does.
#' @keywords internal
#' @noRd
draw_eval_split <- function(analysis, outcome_col, seed, call = rlang::caller_env()) {

  modelled <- modelling_rows(analysis, outcome_col, call = call)

  set.seed(seed)

  drawn <- draw_stratified(
    outcome      = modelled$data[[outcome_col]],
    stratified   = function() rsample::initial_split(modelled$data, prop = SPLIT_PROP,
                                                     strata = dplyr::all_of(outcome_col),
                                                     breaks = STRATA_BREAKS, pool = STRATA_POOL),
    unstratified = function() rsample::initial_split(modelled$data, prop = SPLIT_PROP)
  )

  list(split         = drawn$draw,
       n_dropped     = modelled$n_dropped,
       stratified    = drawn$stratified,
       strata_failed = drawn$strata_failed,
       strata_error  = drawn$strata_error)

}

## ---------------------------------------------------------------------------
## Response trim: validate()'s request, applied to the training partition
## ---------------------------------------------------------------------------
## validate() used to remove response outliers on fences computed over every
## row, before any split, so the test set lost exactly its hardest cases,
## chosen by their own labels (#77). It now records the request; these
## helpers read it and apply it to the training partition of the split
## evaluate() (and fit()'s cold start) draws.

#' The identifier column of a role map
#'
#' @param role_map The object's role map.
#' @return Character. The first `"id"` variable, else `"sample_id"`. The one
#'   rule for it: [eval_data_fingerprint()], `fit()` and the response trim
#'   all read it here.
#' @keywords internal
#' @noRd
id_column <- function(role_map) {

  id_col <- role_map$variable[role_map$role == "id"]

  if (length(id_col) == 0) "sample_id" else as.character(id_col[1])

}

#' Read validate()'s response-trim request
#'
#' @description
#' Returns the request `validate(remove_outliers = TRUE or "response")`
#' recorded in `x$validation$outliers$response_trim`, or `NULL` when there
#' is none (not requested, or cleared by a re-configure). The door
#' validator has refused a request for another outcome, or one without a
#' single positive threshold, rather than let it apply to this outcome's
#' labels.
#'
#' @param x A `horizons_data` that passed its door validation.
#' @return `NULL`, or a list with `outcome`, `method` and `threshold`.
#' @keywords internal
#' @noRd
response_trim_request <- function(x) {

  request <- x$validation$outliers$response_trim

  if (is.null(request)) return(NULL)

  ### The door validator has checked that the request is for this outcome and
  ### has a single positive threshold.
  list(outcome   = as.character(request$outcome),
       method    = request$method %||% "iqr",
       threshold = as.numeric(request$threshold))

}

#' Trim response outliers from a split's training partition
#'
#' @description
#' Computes Tukey fences ([tukey_fences()]) from the training partition's
#' outcome values alone and removes the training rows outside them. Test
#' rows are never removed and their labels are never read, so changing a
#' test row's outcome cannot change which training rows are trimmed.
#' `evaluate()` and `fit()`'s cold start call it on the split
#' [draw_eval_split()] returns, so the two apply one rule; given no request
#' it returns the split as drawn. It draws nothing from the RNG.
#'
#' The trimmed rows are dropped from the split's data, and the training
#' indices renumbered, rather than left in the data outside both parts:
#' `rsample::testing()` and the parallel transport ([resample_indices()])
#' take the test part as the complement of the training indices, so a row
#' left in the data would land in the test set. The split keeps its class,
#' attributes and `out_id = NA`, and its test part is the untrimmed test
#' rows in their original order.
#'
#' When no fences can be drawn (fewer than four training values, or a zero
#' IQR), nothing is trimmed, the record carries the reason, and the verb
#' warns once at its end (`horizons_warning_skipped`): the trim asked for
#' did not run.
#'
#' @param split The `rsplit` from [draw_eval_split()].
#' @param outcome_col Character. The outcome column.
#' @param trim The request from [response_trim_request()], or `NULL` for none.
#' @param id_col Character. The identifier column the trimmed rows are
#'   recorded by.
#' @param collector The verb's collector, from `log_open()`, which records
#'   the skipped trim.
#' @return List with `split` (the trimmed split; `split` itself when `trim`
#'   is `NULL`) and `record`: `NULL` when `trim` is, otherwise a list of
#'   `outcome`, `method`, `threshold`, `fences_from` (`"training"`), `lower`
#'   and `upper` (the fences, `NA` when skipped), `n_training` (the training
#'   rows the fences were computed over), `trimmed_ids` (character) and
#'   `skipped` (`NA`, `"too_few"` or `"zero_iqr"`).
#' @keywords internal
#' @noRd
trim_training_responses <- function(split, outcome_col, trim, id_col, collector) {

  if (is.null(trim)) return(list(split = split, record = NULL))

  train_pos <- split$in_id
  values    <- split$data[[outcome_col]][train_pos]
  fences    <- tukey_fences(values, trim$threshold)

  outside <- if (is.na(fences$skipped)) {
    !is.na(values) & (values < fences$lower | values > fences$upper)
  } else {
    rep(FALSE, length(values))
  }

  trimmed_pos <- train_pos[outside]

  record <- list(
    outcome     = trim$outcome,
    method      = trim$method,
    threshold   = trim$threshold,
    fences_from = "training",
    lower       = fences$lower,
    upper       = fences$upper,
    n_training  = length(train_pos),
    trimmed_ids = as.character(split$data[[id_col]][trimmed_pos]),
    skipped     = fences$skipped
  )

  if (!is.na(fences$skipped)) {

    why <- if (fences$skipped == "too_few") {
      paste0("it has fewer than four outcome values (", fences$n, ")")
    } else {
      "its outcome has a zero interquartile range"
    }

    log_warning(collector, "skipped", c(
      "Response trimming was requested, but no fences can be drawn on the training partition: {why}.",
      "i" = "No training rows were trimmed."
    ))

  }

  if (length(trimmed_pos) > 0) {

    ## Renumber the kept rows; the training indices keep their order.
    keep      <- !seq_len(nrow(split$data)) %in% trimmed_pos
    new_index <- cumsum(keep)

    split$in_id <- as.integer(new_index[train_pos[!outside]])
    split$data  <- split$data[keep, , drop = FALSE]

  }

  list(split = split, record = record)

}

#' Print the response-trim lines of a console tree
#'
#' @param trim The record from [trim_training_responses()], or `NULL`.
#' @return `NULL`, invisibly; prints tree lines, or nothing when there is no
#'   trim.
#' @keywords internal
#' @noRd
render_response_trim <- function(trim) {

  if (!is.null(trim) && is.na(trim$skipped)) {

    n_trimmed <- length(trim$trimmed_ids)

    cat(paste0(
      "\u2502  Response outliers: ", n_trimmed, " of ", trim$n_training,
      " training rows trimmed; fences [", signif(trim$lower, 4), ", ",
      signif(trim$upper, 4), "] (", format(trim$threshold),
      " x IQR) from the training partition; test rows untouched\n"
    ))

  } else if (!is.null(trim)) {

    cat(paste0("\u2502  ", cli::col_yellow(
      "Response outliers: trim requested, but no fences on the training partition; nothing trimmed"
    ), "\n"))

  }

  invisible(NULL)

}

## ---------------------------------------------------------------------------
## evaluation_recipe — the window check and the evaluation$recipe record
## ---------------------------------------------------------------------------

#' Check the recipe settings against the spectrum and record them
#'
#' @description
#' Reads the object's recipe settings ([recipe_settings()]) and aborts when
#' the Savitzky-Golay window is at least as wide as the spectrum, which
#' `configure()` cannot check because `standardize()` may still change the
#' axis after it (#62). Past that point such a window fails every config
#' inside tune with a message that never names it. Returns the record
#' `evaluate()` stores as `evaluation$recipe`, the window's width in cm-1 on
#' the axis the recipe runs on included. `fit()`'s cold start (#45) calls it
#' too, so a configured object that skips `evaluate()` is held to the same
#' check and records the same settings.
#'
#' @param x A configured `horizons_data`.
#' @param call The call the condition is attributed to. Default: the caller,
#'   `evaluate()` or `fit()`.
#' @return List with `sg_window`, `sg_window_cm` (`NA` when the axis spacing
#'   cannot be read) and `pca_threshold`. Aborts with class
#'   `horizons_error_input` when the window does not fit the spectrum.
#' @keywords internal
#' @noRd
evaluation_recipe <- function(x, call = rlang::caller_env()) {

  recipe_cfg <- recipe_settings(x)
  n_spectral <- sum(x$data$role_map$role == "predictor")
  window_cm  <- recipe_cfg$sg_window * axis_spacing_cm(x)

  if (recipe_cfg$sg_window >= n_spectral) {

    width <- if (is.na(window_cm)) "" else {
      paste0(" (", signif(window_cm, 3), " cm\u207B\u00B9)")
    }

    abort_horizons(c(
      "The Savitzky-Golay window is at least as wide as the spectrum.",
      "x" = "{.arg sg_window} is {recipe_cfg$sg_window} grid points{width}; the object has {n_spectral} spectral column{?s}.",
      "i" = "The window must be narrower than the spectrum. Re-run {.fn configure} with a smaller {.arg sg_window}, or keep more of the spectrum."
    ), kind = "input", call = call)

  }

  list(sg_window     = recipe_cfg$sg_window,
       sg_window_cm  = window_cm,
       pca_threshold = recipe_cfg$pca_threshold)

}

## -------------------------------------------------------------------------
## The workers a parallel run dispatches to
## -------------------------------------------------------------------------

#' Refuse a parallel run whose workers cannot run this session's horizons
#'
#' @description
#' On either parallel axis the workers run horizons code: the configs-axis
#' worker body, and on the resamples axis the recipe steps and the PLS engine
#' that tune's workers call. They load horizons by name, so they run the
#' installed copy. Under `devtools::load_all()` that copy is not this source
#' tree, and a worker would run stale code silently; otherwise one worker is
#' asked which version it loads (one future, about 0.05 s on a running plan,
#' drawing nothing from the parent's RNG) and a mismatch is refused. Both are
#' `horizons_error_dependency`: an environment that cannot run the call.
#'
#' @param verb Character(1). The verb asking, `"evaluate"` or `"fit"`.
#' @param call The frame a refusal reports: the verb's.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
check_parallel_workers <- function(verb = "evaluate", call = rlang::caller_env()) {

  if (running_under_load_all()) {

    abort_horizons(c(
      "Parallel {.fn {verb}} cannot run under {.fn devtools::load_all}.",
      "i" = "Workers load the installed {.pkg horizons}, not this source tree, so they would run stale code.",
      "i" = "Install the package first ({.code R CMD INSTALL}), or call {.fn {verb}} without parallelism."
    ), kind = "dependency", call = call)

  }

  parent <- as.character(utils::packageVersion("horizons"))
  worker <- worker_horizons_version()

  if (!identical(worker, parent)) abort_worker_version(worker, parent, call = call)

  invisible(NULL)

}

#' Whether horizons was loaded by `devtools::load_all()`
#'
#' @return Logical(1). A seam, so a test can force the refusal on an
#'   installed build.
#' @keywords internal
#' @noRd
running_under_load_all <- function() {

  exists(".__DEVTOOLS__", envir = asNamespace("horizons"), inherits = FALSE)

}

#' The horizons version a worker of the registered plan loads
#'
#' @return Character(1). A seam, so a test can report another version.
#' @keywords internal
#' @noRd
worker_horizons_version <- function() {

  future::value(future::future(as.character(utils::packageVersion("horizons")),
                               seed = FALSE))

}

#' Abort because a worker loads another version of horizons
#'
#' @param worker,parent Character(1). The worker's version and this
#'   session's.
#' @param call The frame the refusal reports.
#' @return Does not return; aborts with class `horizons_error_dependency`.
#' @keywords internal
#' @noRd
abort_worker_version <- function(worker, parent, call = rlang::caller_env()) {

  abort_horizons(c(
    "A parallel worker loads horizons {worker}, but this session runs {parent}.",
    "i" = "Reinstall the package so workers run matching code."
  ), kind = "dependency", call = call)

}

## -------------------------------------------------------------------------
## Parallel worker
## -------------------------------------------------------------------------

#' Evaluate One Configuration Inside a Parallel Worker
#'
#' @description
#' The body `evaluate()`'s parallel branch dispatches, kept at top level on
#' purpose.
#'
#' R serializes a closure together with its enclosing environment. A function
#' defined inside `evaluate()` therefore carries every local in that frame to
#' every worker — including the split and the resample object — regardless of
#' what is passed explicitly. Measured on the KSSL clay training set at 2 cm-1
#' (17,788 x 1,701), that was a 2.26 GiB payload per future against 232 MB of
#' genuinely needed inputs, and it was what tripped
#' `future.globals.maxSize` (1e9, installed by `tune` at load).
#'
#' Because this function lives in the package namespace, `future` resolves it by
#' name rather than serializing it, so the payload is exactly `shared`.
#'
#' Resolving by name has one cost, and it is the reason `evaluate()` refuses to
#' dispatch under `pkgload`: the worker loads the **installed** horizons, not the
#' source tree. A compatible-but-stale install would run old helper code behind a
#' new worker body, silently, and the worker computes every metric.
#'
#' @param config_i Integer row index into `shared$configs`. Named to avoid
#'   colliding with `shared$resample_idx`.
#' @param shared List of worker inputs assembled by `evaluate()`, whose keys are
#'   fixed by `SHARED_ARG_NAMES` in `R/constants.R` and asserted on entry:
#'   `data`, `resample_idx` (from `resample_indices()`), `configs`, `role_map`,
#'   `grid_size`, `bayesian_iter`, `prune`, `prune_threshold`, `seed`,
#'   `sg_window`, `pca_threshold` (the object's recipe settings),
#'   `outcome_range` (the range scored predictions are clamped to), `data_fp`
#'   and `settings` (the parent's [eval_data_fingerprint()] and
#'   [eval_settings()] records, stamped on the row as-is), `checkpoint_dir`,
#'   `keep_traces` and `pkg_version`. There is no `allow_par`: on
#'   the configs axis tune always runs sequentially inside the worker. There
#'   is no `debug` either, since a debug run never dispatches.
#'
#' @return A one-row result tibble from [evaluate_single_config()], its unit's
#'   records still on it for the parent to lift.
#' @keywords internal
#' @noRd

evaluate_config_worker <- function(config_i, shared) {

  ## -------------------------------------------------------------------------
  ## The worker's horizons, then the cross-process contract
  ## -------------------------------------------------------------------------
  ## A partial guard on the installed-vs-source hazard, behind the one at
  ## evaluate()'s door (check_parallel_workers()): it catches a worker
  ## loading a different horizons VERSION than the parent. It cannot catch
  ## same-version source edits, which is why evaluate() also refuses to
  ## dispatch under pkgload. Checked first, since another version's payload
  ## keys may differ, and that is a version skew, not a bug. A payload
  ## without the parent's version is malformed, below.
  worker_version <- as.character(utils::packageVersion("horizons"))

  if (rlang::is_string(shared$pkg_version) &&
      !identical(worker_version, shared$pkg_version)) {

    abort_worker_version(worker_version, shared$pkg_version)

  }

  ## A mis-keyed entry arrives as NULL. Most then error, but `seed` and
  ## `grid_size` do not: set.seed(NULL) reseeds from the clock and
  ## tune_grid(grid = NULL) invents its own grid, so the run would succeed
  ## while being unreproducible or tuned over the wrong space. The parent
  ## asserts the same keys before dispatch, so a mismatch here means the
  ## transport changed the payload.
  if (!setequal(names(shared), SHARED_ARG_NAMES)) {

    missing    <- setdiff(SHARED_ARG_NAMES, names(shared))
    unexpected <- setdiff(names(shared), SHARED_ARG_NAMES)

    abort_horizons(c(
      "Malformed worker payload.",
      "x" = "Missing: {if (length(missing)) paste(missing, collapse = ', ') else '-'}; unexpected: {if (length(unexpected)) paste(unexpected, collapse = ', ') else '-'}."
    ), kind = "internal")

  }

  ## Thread pinning that works happens in the PARENT before dispatch
  ## (pin_parent_threads()): OpenBLAS reads its thread count when it loads,
  ## so setting OPENBLAS_NUM_THREADS here, after the library is loaded in
  ## this worker, changed nothing when measured (2026-09-15). What can be
  ## set at runtime in the worker is set here. future already sets
  ## mc.cores = 1 in every multisession worker.
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(1L)
  }
  options(ranger.num.threads = 1L)

  cfg <- shared$configs[config_i, ]

  ## Reconstruct the split and folds from indices. Fold membership is
  ## reproduced by construction, not by replaying the RNG.
  resamples <- rebuild_resamples(shared$data, shared$resample_idx)

  result_row <- evaluate_single_config(
    config_row      = cfg,
    split           = resamples$split,
    cv_folds        = resamples$cv_folds,
    role_map        = shared$role_map,
    grid_size       = shared$grid_size,
    bayesian_iter   = shared$bayesian_iter,
    prune           = shared$prune,
    prune_threshold = shared$prune_threshold,
    allow_par       = FALSE,     # configs axis: tune runs sequentially inside
    seed            = shared$seed,
    sg_window       = shared$sg_window,
    pca_threshold   = shared$pca_threshold,
    outcome_range   = shared$outcome_range,
    keep_traces     = shared$keep_traces
  )

  ## Both provenance records are sent rather than rebuilt. The data
  ## fingerprint hashes the whole predictor matrix, which is a once-per-run
  ## cost, not a per-config one; and a setting added to the list in
  ## evaluate() reaches the worker's rows with no change here.
  result_row <- stamp_data_fingerprint(result_row, shared$data_fp)
  result_row <- stamp_eval_settings(result_row, shared$settings)

  if (!is.null(shared$checkpoint_dir)) {

    write_checkpoint_row(result_row, shared$checkpoint_dir)

  }

  result_row

}
