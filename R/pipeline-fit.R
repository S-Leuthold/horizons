#' Fit Top Model Configurations
#'
#' @description
#' Takes the top N configurations from `evaluate()` at the hyperparameters
#' `evaluate()` chose for them, which by default it keeps (an optional
#' warm-start Bayesian re-tune can replace them; see "The re-tune"),
#' generates out-of-fold (OOF) CV predictions for downstream stacking, fits
#' final deployable models, scores them on `evaluate()`'s held-out test rows,
#' and optionally trains uncertainty quantification (UQ) components.
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
#' with class `horizons_error_input`. The trim is reused, not recomputed, so
#' `fit()` trains without exactly the rows `evaluate()` left out and scores
#' on the same untrimmed test rows. A trim request `validate()` records
#' after `evaluate()` therefore cannot reach the fit, and `fit()` refuses it
#' rather than ignore it: when `validation$outliers$response_trim` differs
#' from the trim `evaluate()` applied (one without the other, or another
#' outcome, method or threshold), `fit()` aborts with class
#' `horizons_error_input`, saying which differs. Re-running `evaluate()`
#' applies the current request. A trim that drew no fences counts as
#' applied, under the request it ran for. Columns added since `evaluate()`,
#' such as a sibling response from `add_response()`, are carried into the
#' fit.
#' The count of NA-outcome rows, and the response trim, are reported in the
#' console tree when `verbose = TRUE`. With UQ or AD on, the calibration set
#' is carved out of Split F's training part. After a response trim it is
#' carved out of the untrimmed training part (the trimmed rows are training
#' rows too, never test rows), and the trimmed rows are then dropped from the
#' fit rows only. They calibrate the intervals, so the calibration rows are
#' exchangeable with new data, extremes included, and coverage over new data
#' as a whole is at the level, where a pool of kept rows alone fell below it.
#' The coverage is marginal: a sample whose outcome lies far outside the
#' fences is rarely inside its interval. AD shares that calibration set (D7).
#' Calibration rows
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
#' `horizons_warning_quality`, naming the threshold and the members'
#' cross-validated RPD, and saying so when the members are pruned
#' configurations; that includes `bayesian_iter = 0`, where nothing is
#' pruned. If every member fails,
#' `fit()` aborts with class `horizons_error_all_members_failed` (with
#' `horizons_error_run`), listing the
#' distinct error messages; the
#' per-member results travel on the condition as `results`, and
#' `models$results` keeps each member's `error_message` when some succeed.
#'
#' @section The re-tune:
#' By default `fit()` does not re-tune: [configure()]'s `final_bayesian_iter`
#' defaults to 0, so each member keeps `evaluate()`'s hyperparameters,
#' clamped into the range `fit()`'s data allow (a `num_comp` above the cap of
#' fewer rows, say), and is scored by its out-of-fold predictions alone, as
#' `tune::fit_best()` refits a choice. A member whose parameters are unusable
#' is tuned instead on a space-filling grid of `grid_size` points, followed by
#' `final_bayesian_iter` Bayesian iterations when it is above 0. On a
#' library-scale and a local benchmark the re-tune never moved a member past
#' its margin (below): every member kept `evaluate()`'s hyperparameters, the
#' best candidate leading them by at most 0.2 of its standard errors, while
#' the re-tune took most of `fit()`'s run time.
#' `configure(final_bayesian_iter = )` above 0 turns it on; the tree's header
#' says whether it ran. `evaluate()` chose the hyperparameters on its training
#' rows, which hold `fit()`'s and, with UQ or AD on, the calibration rows, so
#' a member that keeps them is fitted at hyperparameters the calibration rows
#' took part in choosing, as they took part in choosing the members (#44).
#'
#' With the re-tune on, each member's re-tune starts from `evaluate()`'s
#' hyperparameters, clamped, and, for a model tuning d parameters, d points
#' drawn at random from a local grid around them, each parameter's value and
#' its nearby values crossed. It then runs a Bayesian search with a budget of
#' at least `final_bayesian_iter` iterations, raised so that the start and
#' the search try ten points a tuned parameter, which stops once
#' `control_horizons(no_improve = )` iterations in a row have not improved on
#' its best. A space small enough is gridded whole instead (see "Cold start
#' from one configuration"). The member then keeps `evaluate()`'s
#' hyperparameters unless the best candidate scored on every fold has a CV
#' RMSE lower than theirs by more than that candidate's standard error:
#' `tune::select_by_one_std_err()`'s bound, with `evaluate()`'s choice in the
#' place of the simplest model. That choice was made on `evaluate()`'s folds
#' and is scored once on `fit()`'s, a fresh draw, while the best candidate is
#' the best of many tried on `fit()`'s folds, so it beats the choice by the
#' selection alone even where nothing is better.
#' Hyperparameters of `evaluate()`'s that were not scored on every fold take
#' no part, and the best candidate is taken, which the member's `warnings`
#' say. The member's out-of-fold predictions are made at the hyperparameters
#' it keeps, and `models$results$retune_moved` records which it kept:
#' `FALSE` for `evaluate()`'s (with the re-tune off, every member whose
#' parameters were usable), `TRUE` for another candidate, `NA` with none to
#' keep (a cold start, or parameters that were unusable). The tree says per
#' member which, and by how many of the best candidate's standard errors it
#' led.
#'
#' @section What fit() reports:
#' `fit()` warns once per kind at its end, after writing each warning to the
#' object's `log`: a stratified draw (the calibration split, the folds, a
#' cold start's split) that came out unstratified, because the stratified
#' draw failed or rsample formed one stratum from the outcome
#' (`horizons_warning_substitution`); a smallest assessment fold under
#' `SMALL_FOLD_ROWS` rows, or every member below `evaluate()`'s prune
#' threshold (`horizons_warning_quality`); UQ and AD switched off because the
#' calibration set is under `min_calib_rows` (`horizons_warning_skipped`);
#' members whose prediction intervals, measured coverage or applicability
#' domain could not be computed, with why (`horizons_warning_skipped`, #145);
#' members whose degradation check could not run, whose `degraded` is then
#' `NA` with the cause in their warnings (`horizons_warning_skipped`);
#' configurations without a value of the ranking metric left out of the
#' choice of members (`horizons_warning_skipped`);
#' a member whose warm-start Bayesian search failed and kept what it finished
#' (`horizons_warning_substitution`, #209); a member whose feature-selection
#' step kept every band (`horizons_warning_substitution`), confirmed nothing
#' (`horizons_warning_quality`) or had too few bands to select from
#' (`horizons_warning_skipped`), read from its final fit's recipe; some
#' members failed and the rest
#' are returned (`horizons_warning_partial_failure`, naming them with each
#' distinct cause). It records as notes on the log, without a warning, a
#' member's fitted workflow stored whole because it could not be slimmed and
#' in-sample interval diagnostics that could not be computed, and how many
#' members' re-tunes the Bayesian search's stopping rule ended early
#' (`control_horizons(no_improve = )`: a search stops once that many
#' iterations in a row have not improved on its best, and `FALSE` runs every
#' iteration), each member recording the iterations its search began
#' (`bayes_iter`) and why it ended (`bayes_stop`); how many members kept
#' `evaluate()`'s hyperparameters, or that the re-tune was off, as it is by
#' default (see "The re-tune"); and, on a cold start, what its Bayesian
#' search ran (see "Cold start from one configuration"), warning with class
#' `horizons_warning_input` when a `final_bayesian_iter` above 0, which a
#' cold start does not read, was set. The folds are
#' drawn from the rows the members are fitted on, which must outnumber the
#' folds, or `fit()` refuses, as `evaluate()` does.
#'
#' @section Members that fail:
#' Each member is one unit: a failure in any step of its re-tune, its
#' out-of-fold predictions, its final fit or its test scoring fails that
#' member alone, and `models$results` records the step and the cause in
#' `error_message`, the root cause's class in `class`, and, with
#' `control_horizons(keep_traces = TRUE)`, its backtrace as text in `trace`.
#' A member's own warnings, from the engines and from tune, are in
#' `models$results$warnings`. A bug, a refusal or a missing package is not
#' the member's and stops the call: before any member is fitted, `fit()`
#' refuses members that run a Savitzky-Golay method (`"sg"`, `"deriv1"`,
#' `"deriv2"`, `"snv_deriv1"`, `"snv_deriv2"`) on a wavenumber axis with a
#' gap in it, as [evaluate()] refuses such a grid (`horizons_error_input`),
#' and members whose model's engine or feature-selection step needs a
#' package that is not installed, naming each (`horizons_error_missing_package`). When the first members fail the same
#' way (3 of them by default, `control_horizons(breaker = )`, `FALSE` to
#' fit every member), and between them span two of the members' models,
#' transformations, preprocessing methods and feature selections, each of
#' these the members have more than one of, `fit()` stops with
#' `horizons_error_systemic` and the members so far as `results`, as
#' [evaluate()] does ("Failed configurations" there); and it stops the same
#' way when a daemon dies (see "Parallelism"). A daemon lacking a package the members need is refused
#' before the run (`horizons_error_missing_package`); one found lacking it
#' later fails the members it runs with tune's error. With
#' `control_horizons(debug = TRUE)` the re-tunes run in this session and the
#' first failure, inside tuning included, stops the call with its backtrace.
#' Before a parallel run `fit()` refuses a session loaded with
#' `devtools::load_all()`, and, asking every daemon connected one task each,
#' daemons that load another version of horizons
#' (`horizons_error_dependency`), since the daemons prepare the members'
#' folds and tune's tasks fit the members' engines, horizons' own PLS engine
#' among them, or cannot load a package the members need, their engines' or
#' their feature selections' (`horizons_error_missing_package`). A feature
#' selection that fails while a fold is prepared fails every member that
#' reads that preparation, with the step's own class and words.
#'
#' @section Cold start from one configuration:
#' With one configuration there is nothing for `evaluate()` to choose, so a
#' configured `horizons_data` whose `config$configs` has exactly one row can
#' go straight to `fit()` (#45). `fit()` then draws the train/test split
#' itself, by the draw `evaluate()` uses, so at the same `seed` it holds out
#' exactly the rows `evaluate()` would have, and applies a response trim
#' `validate()` requested as `evaluate()` does, on fences from the training
#' partition alone. With no parameters from `evaluate()` to keep, `fit()`
#' tunes the configuration as `evaluate()` would, since this is its only
#' tuning: a space-filling grid of `grid_size` points, then a Bayesian search
#' of `bayesian_iter` iterations ([configure()]), raised to ten points a
#' tuned parameter and stopped by `control_horizons(no_improve = )` as in
#' `evaluate()`, keeping the candidate with the lowest CV RMSE. At
#' `bayesian_iter = 0` it grids only, as `evaluate()` does. A cold start does
#' not read `final_bayesian_iter`, the budget of the re-tune of `evaluate()`'s
#' members: one above 0 is ignored, with a warning of class
#' `horizons_warning_input`. The console tree's header and a note on the log
#' say what the search ran, and `models$results$warm_start` is `FALSE`. On
#' either path, a search space
#' whose tuned parameters are all whole numbers, costing no more fits than
#' the grid and the Bayesian iterations together, is gridded whole with no
#' Bayesian search, as `evaluate()` grids it: a submodel's values share one
#' fit, so that is every plsr member, each `num_comp` up to its cap tried
#' from one fit a fold. On the `evaluate()` path that holds only with the
#' re-tune on; off, as it is by default, each member keeps `evaluate()`'s
#' hyperparameters (see "The re-tune"). The degradation
#' check needs
#' nothing from `evaluate()`, since it compares the test RPD with `fit()`'s
#' own cross-validation.
#'
#' The fit carries an unscreened evaluation record in place of
#' `evaluate()`'s: `evaluation$screened` is `FALSE`, the one configuration is
#' `best_config` with status `"not_evaluated"`, its metric and `cv_*` columns
#' are `NA`, and `runtime_secs` is 0. `evaluation$recipe` records the recipe
#' settings as `evaluate()` does, and `fit()` applies `evaluate()`'s checks
#' first: an outcome that is not constant, a Savitzky-Golay window narrower
#' than the spectrum, no window method on an axis with a gap in it, and more
#' rows than `cv_folds` to draw the folds from,
#' which `fit()` checks on either path. Re-fitting a cold-started fit starts
#' cold again. With more than one configuration `fit()` aborts with class
#' `horizons_error_input`:
#' choosing among them is what `evaluate()` is for.
#'
#' @param x A `horizons_eval` object (output of `evaluate()`), or a
#'   configured `horizons_data` with exactly one configuration, which `fit()`
#'   starts cold (see "Cold start from one configuration").
#' @param n_best Integer. Number of top configurations to fit. Default 5.
#'   A cold start has one.
#' @param metric Character or NULL. Metric for ranking the candidate
#'   configs, by bare name: one of `"rpd"`, `"rsq"`, `"rmse"`, `"rrmse"`,
#'   `"ccc"`, `"mae"`, or `fit()` aborts with class `horizons_error_input`.
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
#' @param seed `NULL` or a whole number: the run's seed. The CV folds, the
#'   calibration split and each member's stages are seeded from it and their
#'   own names, so a member's results depend on the seed alone (for `cubist`,
#'   because each of its fits runs in a process of its own; see the
#'   reproducibility note in [configure()]). The
#'   train/test split is `evaluate()`'s, except on a cold start, where `fit()`
#'   draws that split as `evaluate()` would at this seed. `NULL` (the
#'   default) takes the seed the object records (`evaluate()`'s, or a re-fit's
#'   own), and on a cold start from an object that records none, one integer
#'   from the session's random stream, which is left where it was and
#'   printed in the tree. A seed given here overrides the object's and is
#'   recorded as `models$seed`, which [ensemble()] takes.
#' @param control A [control_horizons()] object: how the run goes, and,
#'   through `isolate` and `no_improve`, part of what it computes (see
#'   [control_horizons()]). `fit()` reads `allow_par`, `parallelize_over`,
#'   `verbose`, `debug`, `breaker`, `keep_traces`, `isolate`,
#'   `inner_daemons`, `threads`, `no_improve`, `min_calib_rows`,
#'   `min_ad_rows` and `min_degradation_rows`, and refuses any other field
#'   that is set. The control it ran with, defaults filled in, is recorded
#'   as `models$control`.
#'
#' @section Parallelism:
#' Before any member is fitted, each fold's recipe is prepared once for the
#' call, for every member that shares its preprocessing and feature
#' selection (and, for a selection that reads the outcome, its response
#' transformation), as [evaluate()] prepares its configurations' ("Parallelism"
#' there); the preparations are held in this session and dropped when the
#' call returns. With `control_horizons(allow_par = TRUE)` they are prepared
#' on the daemons, one task each, longest first, each carrying its rows, and
#' those still running are cancelled when the call stops, as the members'
#' tasks are on the members axis (below); both need mirai's dispatcher, so
#' on daemons set with `dispatcher = FALSE` `fit()` warns of it once, with class `horizons_warning_input`, as [evaluate()] does, the
#' warning a row of the object's `log` and "no dispatcher" on the progress
#' tree's parallel line. A pool left with no daemon while they run stops the
#' call with `horizons_error_no_daemons`, every member still to fit. Then
#' the members run on the [mirai](https://mirai.r-lib.org) daemons the
#' caller has set (`mirai::daemons(4)` before the call), on one of two axes,
#' `control_horizons(parallelize_over = )`, by the rule [evaluate()] applies
#' to its configurations ("Parallelism" there):
#'
#' * `"resamples"`: the members are fitted one after another in this
#'   session, and tune runs the CV folds of each member's re-tune, when it
#'   runs, and of its out-of-fold pass on the daemons.
#' * `"configs"`: each member is a task of its own on a daemon, its re-tune,
#'   out-of-fold pass, final fit, intervals and applicability domain all
#'   there, started longest first by an estimate of each one's time, at most
#'   one member per daemon connected; its result comes back to this session
#'   when it is done, and the members keep their rank whatever order they
#'   finish in. Each task carries its member's rows, as the fold
#'   preparations' tasks do, so this session holds a copy of the modelled
#'   rows for each member in flight. A member whose share of the work is
#'   large runs its folds on inner daemons its task starts on its daemon,
#'   within the daemons set, as [evaluate()]'s configurations do ("Inner
#'   daemons" there; `control_horizons(inner_daemons = FALSE)` runs every
#'   member on one daemon). The progress tree prints each member's block, in
#'   rank order, once every member is in.
#' * `"auto"` (default): `"configs"` when there are at least as many members
#'   as `cv_folds`, otherwise `"resamples"`, so the default five members on
#'   [configure()]'s default five folds are fitted on the members axis, and
#'   on ten folds on the folds axis. `fit()` checkpoints nothing, so neither
#'   axis needs an `output_dir`.
#'
#' The results are the same on either axis and on any number of daemons:
#' each member's draws are seeded from the run's seed and their own names,
#' wherever they run. The run's axis and the daemons it began with are
#' recorded as `models$parallelize_over` and `models$workers`. A
#' `future::plan()` is not used, and `fit()` never sets, resizes or resets
#' the daemons. With fewer than two connected, `fit()` runs sequentially and
#' warns, with class `horizons_warning_substitution`; the warning is also a
#' row of the object's `log`. A run in this session fits `rf`, `xgboost` and
#' `lightgbm` members on `control_horizons(threads = )` threads, one by
#' default, and the results are the same on any number; on the daemons each
#' fits on one, and `threads` above one with `allow_par` is refused, as in
#' [evaluate()]. A daemon that dies during a member stops the call with
#' `horizons_error_systemic` and the members so far, on either axis: on the
#' folds axis read as fewer daemons connected after the member than before,
#' which a daemon exiting under `maxtasks`, `idletime` or `walltime` also
#' gives and the count cannot tell apart, and with fewer than two connected
#' as a member starts (tune would fit it sequentially) the call stops with
#' `horizons_error_no_daemons`; on the members axis read from the member's
#' task, which ends in mirai's `errorValue` 19, and the members in flight are
#' cancelled, as a pool left with no daemon stops the call with
#' `horizons_error_no_daemons`. The finished members' rows are the error's
#' `results` and their models are not kept, since `fit()` has no
#' checkpoints: a run after the stop fits every member again. A pool whose
#' daemons all exit during one of tune's calls waits inside tune, and a
#' daemon whose library loses a package after the check before the run
#' fails the member with tune's "Argument 1 must be a data frame or a named
#' atomic vector". Before anything is fitted, a parallel `fit()` checks that
#' the daemons on this machine fit in its memory, as [evaluate()] does
#' ("Parallelism" there), warning with class `horizons_warning_input` when
#' they would not.
#'
#' Each `cubist` fit runs in a process of its own, wherever it runs, as in
#' [evaluate()] ("Isolated fits" there), so a member's results depend on the
#' seed alone; a member whose fit's process dies, in its re-tune, its
#' out-of-fold pass or its final fit, fails with
#' `horizons_error_process_died`. `control_horizons(isolate = FALSE)` fits it
#' in the process that runs it instead, which the tree and the `log` say.
#'
#' @section Outcome range:
#' Every prediction `fit()` scores (out-of-fold, test and calibration) is
#' clamped to `configure()`'s `outcome_range`, as `predict()` clamps what it
#' serves. Before anything is drawn or fitted, on either path, `fit()` aborts
#' with class `horizons_error_input` when an observed outcome lies outside
#' the range or is infinite, naming `outcome_range` and how to set it (#76);
#' when the ranking metric is `"rrmse"` and the range's lower bound is
#' negative, since the mean outcome it divides by can then be zero or
#' negative; and, on the `evaluate()` path, with class `horizons_error_input`,
#' when the range differs from the one `evaluate()` recorded on its results
#' rows, since the members were chosen on predictions clamped to that range.
#'
#' @section Zero outcome variance:
#' On either path, before anything is drawn, `fit()` aborts with class
#' `horizons_error_input` when every observed outcome has the same value, as
#' `evaluate()` does. It also aborts, before any model is fitted, when the
#' modelled rows vary but the rows its models are fitted on do not, because
#' the rows with another value all fell in the test set or the calibration
#' set, or among the training rows a response trim left out. Every member
#' would otherwise be fitted to a constant. The message names the outcome,
#' the value, how many rows have it and where the other rows went (#132).
#'
#' @return A `horizons_fit` object (inherits from `horizons_eval`,
#'   `horizons_data`) with `models$` slot populated. The slot includes
#'   `response_bound`, the deploy-time winsorization guardrail `predict()`
#'   applies to back-transformed point predictions:
#'   `max + (RESPONSE_BOUND_MARGIN - 1) * (max - anchor)`, capped at a finite
#'   upper bound of `outcome_range`, where `max` is the largest training
#'   outcome and the anchor is the lower bound of `outcome_range`, or the
#'   smallest training outcome when that bound is `-Inf`. Under the default
#'   range the anchor is 0 and the bound is the largest training outcome
#'   times `RESPONSE_BOUND_MARGIN`, as before the range existed; for any
#'   range it lies above the largest training outcome unless the cap binds.
#'   The training rows are the ones the final models are fit on:
#'   `evaluate()`'s training part, less the calibration set when UQ or AD is
#'   on, plus the training rows a response trim left out, so the bound never
#'   sits below a value observed in the training partition. The slot also
#'   includes `selection_present` (whether the
#'   training object carried a `$selection` from `select_training()`, which
#'   `predict()` reads when asked for conformal intervals). Called on a
#'   `horizons_ensemble`, it returns a `horizons_fit` whose `ensemble` slot
#'   is empty again, since the ensemble was built on the members it replaces.
#'   `models$results` records how each member's re-tune started and what it
#'   kept: `warm_start` (`TRUE` when `evaluate()`'s parameters were usable,
#'   so the re-tune starts from them and the points drawn around them or
#'   from the whole space, `FALSE` for a space-filling grid), `start_grid_size`, the points
#'   it started from (`NA` with the re-tune off), and `retune_moved` (see
#'   "The re-tune").
#'
#' @export
fit <- function(x,
                n_best     = 5L,
                metric     = NULL,
                compute_uq = TRUE,
                compute_ad = TRUE,
                seed       = NULL,
                control    = control_horizons()) {

  start_time <- Sys.time()

  ## The caller's random state is left as found, as evaluate() leaves it,
  ## and a session that has never drawn is given its stream first
  ensure_rng_stream()
  local_rng_state()

  ## How the run goes: the fields fit() reads, defaults filled in, and the
  ## record of it on the models
  control   <- resolve_control(control, "fit")
  allow_par <- control$allow_par
  verbose   <- control$verbose

  ## threads is for a run in this session (#114)
  check_threads_sequential(control)

  collector <- log_open("fit")

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

    abort_horizons(c(
      "{.fn fit} needs a {.cls horizons_eval} from {.fn evaluate}, or a configured {.cls horizons_data} with one configuration.",
      "x" = "{.arg x} is {.obj_type_friendly {x}}."
    ), kind = "input")

  }

  ## `metric` on either path, before anything is drawn or fitted. On the
  ## evaluate() path an unknown name used to surface as a missing cv_<metric>
  ## column with the advice to re-run evaluate(); on a cold start the record
  ## would fail validation only after every model had been fitted.
  if (!is.null(metric)) check_rank_metric_arg(metric)

  ## n_best is taken as an integer, so it is held to R's integer range, the
  ## seed's bound
  check_arg(rlang::check_number_whole, n_best, min = 1, max = SEED_MAX)
  check_arg(rlang::check_bool, compute_uq)
  check_arg(rlang::check_bool, compute_ad)

  ## The seed, as at every verb, is NULL or a whole number set.seed() takes
  ## unchanged.
  check_arg(rlang::check_number_whole, seed, min = -SEED_MAX, max = SEED_MAX, allow_null = TRUE)

  ## The object as it arrives (#234): on the evaluate() path its evaluation
  ## record, and through it the base contract; on a cold start the base
  ## contract. A record from an earlier version, or one edited by hand, is
  ## refused here, before the checks below read it and before anything is
  ## re-tuned.
  x <- if (cold_start) validate_horizons_data(x) else validate_horizons_eval(x)

  ## The run's root, once, before anything draws: the seed given, else the
  ## object's (evaluate()'s, or a re-fit's own), else one from the session
  root <- resolve_seed(seed, x)
  seed <- root$seed

  ## The outcome has to lie inside the range every prediction is clamped to,
  ## checked on either path before anything is drawn or fitted (#76).
  ## configure() and evaluate() check it too, but the rows can have changed
  ## since; this is the last check before the re-tune pays for itself.
  check_outcome_range(x, verb = "fit")
  outcome_range <- outcome_range_setting(x)

  ## A constant outcome, refused on either path before anything is drawn,
  ## as evaluate() refuses it (#132): validate() reports one, but nothing
  ## gates on its verdict. The rows the models are fitted on are checked
  ## again in Step 1, once the split, the trim and the calibration set have
  ## drawn them.
  check_outcome_variance(x, verb = "fit")

  ## The metric fit() will rank by, and that the ensemble inherits: rrmse
  ## ranks backwards once the mean outcome can be negative.
  check_rank_metric_range(metric %||% x$evaluation$rank_metric, outcome_range,
                          verb = "fit")

  ## evaluate() scored and ranked the configurations under the range it
  ## stamped on each row; members chosen and warm-started under another range
  ## would carry that range's scoring into this fit. A cold start has no
  ## evaluate() rows to compare.
  if (!cold_start) check_evaluated_outcome_range(x, outcome_range)

  ## evaluate() trimmed by the request validate() had recorded then, and
  ## fit() reuses those rows rather than reading the request. validate() can
  ## write a new request on an evaluated object, which would then be ignored,
  ## silently, so a request that differs from the applied trim is refused
  ## (#137). A cold start reads the request itself.
  if (!cold_start) check_trim_request(x)

  ## Notes on the draws and the member count are printed inside the tree,
  ## after its header in Step 3; they used to print above it (#91).

  if (cold_start) {

    cold         <- cold_start_evaluation(x, metric, seed, collector)
    x$evaluation <- cold$evaluation

  }

  ## The door validation above has refused a malformed table (a column added
  ## after configure() without a role_map entry would otherwise reach
  ## build_recipe()'s `outcome ~ .` as an unregistered predictor, #24), and,
  ## on the evaluate() path, results with no rows; a cold start builds its
  ## one row.

  eval_results <- x$evaluation$results

  ## -----------------------------------------------------------------------
  ## Step 0b: Parallel backend
  ## -----------------------------------------------------------------------
  ## The user owns the daemons. Confirm there are some to run on, pin
  ## threads in this process, and never set, resize or reset the daemons.
  ##
  ## Debug runs in this session: a backtrace from a worker shows the worker's
  ## plumbing, and no debugger attaches there. The user asked for debug, so
  ## the tree says so rather than a warning.

  debug       <- control$debug
  par_by_user <- allow_par
  allow_par   <- allow_par && !debug

  if (allow_par) {

    allow_par <- check_parallel_backend("fit", collector)

    if (allow_par) {

      unpin_threads <- pin_threads()
      on.exit(unpin_threads(), add = TRUE)

    }

  }

  ## A run in this session fits the engines with a thread argument on
  ## control_horizons(threads = ) threads (#114), and their options
  ## (ranger's, which it reads to predict) match while it runs
  if (!allow_par) {

    restore_threads <- options(engine_thread_options(control$threads))
    on.exit(options(restore_threads), add = TRUE)

  }

  ## Determine ranking metric. The name stays bare ("rpd"); ranking reads the
  ## cross-validated column cv_rpd, so members are chosen without touching
  ## the test set (#50). The test-set metrics on the leaderboard remain
  ## reported, and are honest precisely because they are not used here.
  rank_metric <- metric %||% x$evaluation$rank_metric %||% "rpd"

  ## The candidates are evaluate()'s best_config candidates: the successes
  ## or, when none succeeded, the pruned configs, and one without a
  ## cv_<metric> is left out of the ranking. fit() used to keep successes
  ## only, so it refused an evaluation whose best_config was a pruned config
  ## (#38). A cold start's one configuration is the member as it stands:
  ## not_evaluated, with nothing to rank on.
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

    ## evaluate() returns such an evaluation, so re-running it would not help;
    ## a metric the same candidates hold would. The lines are rendered
    ## already, so each is substituted as text.
    reason <- unranked_reason(eval_results, rank_metric, "`fit(x, metric = )`")

    abort_horizons(c(
      "No configuration in {.field evaluation$results} can be fitted.",
      "x" = "{reason[1]}",
      stats::setNames(sprintf("{reason[%d]}", seq_along(reason)[-1]), names(reason)[-1])
    ), kind = "input")

  }

  ranked <- if (cold_start) candidates$rows else rank_configs_by_cv(candidates$rows, rank_metric)

  ## A candidate without a value of the ranking metric is left out, said once
  if (!cold_start) log_unranked(collector, candidates$rows, ranked, rank_metric)

  ## Cap n_best at available candidates. A cold start has one configuration
  ## by construction, so the default n_best is no request worth a note.
  n_available <- nrow(ranked)
  n_requested <- as.integer(n_best)
  n_best      <- min(n_requested, n_available)

  top_configs <- ranked[seq_len(n_best), ]

  ## Say so when every member fell below evaluate()'s prune threshold. The
  ## pruned fallback is one case; the other is bayesian_iter = 0, where the
  ## gate skips nothing, so below-threshold configs are successes and nothing
  ## else would say that none cleared the bar (#38).
  log_members_below_threshold(collector, top_configs, fallback = candidates$fallback,
                              bayesian_iter = x$config$tuning$bayesian_iter)

  ## Extract references
  role_map     <- x$data$role_map
  outcome_col  <- role_map$variable[role_map$role == "outcome"]
  all_configs  <- x$config$configs
  tuning       <- x$config$tuning
  cv_folds     <- tuning$cv_folds

  ## Every engine and selection step the members need, before any is
  ## fitted. Only the members: an evaluation that also holds a mars
  ## configuration does not need earth to fit the others.
  member_configs <- all_configs[all_configs$config_id %in% top_configs$config_id, ]

  ## No member runs a Savitzky-Golay window across a gap in the axis. A cold
  ## start checked its one configuration in cold_start_evaluation(); an
  ## evaluation can predate the check evaluate() makes, so its members are
  ## checked here, before any is fitted.
  if (!cold_start) check_window_gaps(x, member_configs$preprocessing)

  check_config_packages(member_configs, "fit")

  ## The recorded ranges on the parameters of the members' models, now that
  ## their engines are known to be installed, as evaluate() types them
  check_range_types(tuning$ranges, member_configs$model)

  ## Fits a model's engine would otherwise carry state across run in this
  ## session when isolation is turned off, which the log notes
  log_isolation_off(collector, member_configs$model, control$isolate)

  ## The axis the daemons are applied to, by evaluate()'s rule with the
  ## members in place of the configurations (#60): the members, each a task
  ## of its own, or tune's folds inside each member. fit() checkpoints
  ## nothing, so its members axis needs no directory.
  axis         <- resolve_parallel_axis(control$parallelize_over, allow_par, n_configs = n_best,
                                        cv_folds = cv_folds, configs_runnable = TRUE)
  members_axis <- axis$dispatch_configs

  ## The daemons prepare the members' folds (their feature selections'
  ## steps), and fit the members or tune's tasks on their folds, horizons'
  ## PLS engine among them, so every daemon must run this session's
  ## horizons; and it must load the members' packages, their engines' and
  ## their feature selections': checked before anything is drawn
  ## The daemons on this machine against the memory it has, for this run's
  ## data, said once and on the tree's parallel line
  short_memory <- FALSE
  n_daemons    <- NA_integer_
  reports      <- NULL

  if (allow_par) {
    ### Read before the check, which spends a task on each daemon and so can
    ### end a pool set with `maxtasks`
    n_daemons <- connected_daemons()
    reports   <- check_parallel_workers("fit", needed_packages(member_configs$model, member_configs$feature_selection),
                                        collector, results = member_results(list()))
    ### The table a daemon holds: the whole axis, masked columns included
    short_memory <- check_daemon_memory(reports, member_configs$model, control$isolate,
                                        n_rows       = nrow(x$data$analysis),
                                        n_predictors = length(axis_columns(role_map)),
                                        collector    = collector, verb = "fit")
  }

  ## configure() stores the screening and the final re-tune budgets
  ## separately; fit() used to pass the screening one to the re-tune, so the
  ## user-facing `final_bayesian_iter` did nothing (#46). It is the budget of
  ## the re-tune of evaluate()'s members. A cold start's tuning is the only
  ## tuning its configuration gets and stands in for evaluate()'s (#45), so it
  ## runs evaluate()'s budget, bayesian_iter, and does not read
  ## final_bayesian_iter, which it says when one above 0 was set. The tree
  ## header prints the budget that runs.
  final_bayesian_iter <- tuning$final_bayesian_iter
  tune_iter           <- if (cold_start) tuning$bayesian_iter else final_bayesian_iter

  if (cold_start && final_bayesian_iter > 0) {

    log_warning(collector, "input",
      "{.arg final_bayesian_iter} is ignored on a cold start, which tunes its one configuration as {.fn evaluate} would, by {.arg bayesian_iter} ({tune_iter}); {.arg final_bayesian_iter} sets the re-tune of {.fn evaluate}'s members only."
    )

  }

  ## What a cold start's Bayesian search runs, for the log's note and the
  ## tree's header
  cold_text <- if (cold_start) {
    words <- search_words(member_configs$model, tune_iter, tuning, warm = FALSE)
    cold_tuning_text(tune_iter, words$raised, words$searched, words$whole)
  }

  ## The recipe settings evaluate() ran with, or on a cold start the ones
  ## cold_start_evaluation() checked against the spectrum.
  recipe_cfg   <- recipe_settings(x)

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
  ## same samples with the same outcomes; rebuild_split() checks that and
  ## points the split at the object's current rows, less the training rows
  ## evaluate()'s response trim (#77) recorded, as ensemble() does for its
  ## test rows. On a cold start the split was drawn in Step 0 from these
  ## rows, by the draw evaluate() uses, so it passes.

  id_col <- id_column(role_map)

  rebuilt         <- rebuild_split(x)
  split_F         <- rebuilt$split
  modelled        <- rebuilt$modelled
  n_dropped       <- modelled$n_dropped
  n_flagged       <- modelled$n_flagged
  trimmed_ids     <- rebuilt$trimmed_ids
  trimmed_rows    <- rebuilt$trimmed_rows
  response_fences <- rebuilt$response_fences

  train_F <- rsample::training(split_F)
  test_F  <- rsample::testing(split_F)
  n_train <- nrow(train_F)
  n_test  <- nrow(test_F)

  ## Calibration partitioning: split train_F into train_Fit / calib_Fit.
  ## UQ and AD share this one held-out split (D7) \u2014 both calibrate on calib_Fit
  ## and never train on it. Built whenever either capability is requested.
  ## A unit of its own, so the calibration rows depend on `seed` and train_F
  ## alone.
  ##
  ## After a response trim (#77) the pool is the untrimmed training part,
  ## train_F plus the trimmed rows. Split conformal needs calibration rows
  ## exchangeable with the rows it will be asked about, and those include the
  ## extremes the trim left out: a trimmed pool undercovered them, silently,
  ## since the OOF coverage is read on trimmed rows too. The trimmed rows are
  ## then dropped from the fit rows only. Calibration rows still never train,
  ## and test rows never calibrate. Without a trim the pool is train_F.
  calib_data  <- NULL
  calib_drawn <- NULL
  calib_note  <- NULL
  split_C     <- NULL
  calib_pool  <- calibration_pool(train_F, trimmed_rows)

  if (compute_uq || compute_ad) {

    calib_drawn <- with_unit_seed(seed, "fit/calibration", draw_stratified(
      outcome      = calib_pool[[outcome_col]],
      stratified   = function() rsample::initial_split(calib_pool, prop = CALIB_PROP,
                                                       strata = dplyr::all_of(outcome_col),
                                                       breaks = STRATA_BREAKS, pool = STRATA_POOL),
      unstratified = function() rsample::initial_split(calib_pool, prop = CALIB_PROP)
    ))
    split_C <- calib_drawn$draw

    ## The fit rows less the trimmed ones, by the code a members-axis task
    ## rebuilds them with (rebuild_resamples())
    parts      <- calibration_parts(split_C, trimmed_rows, id_col)
    train_Fit  <- parts$train
    calib_data <- parts$calib

    ## Guard: minimum calibration size. Too small disables BOTH capabilities
    ## that needed it \u2014 neither UQ nor AD can calibrate on an undersized set.
    if (nrow(calib_data) < control$min_calib_rows) {

      disabled   <- paste(c(if (compute_uq) "UQ", if (compute_ad) "AD"),
                          collapse = " and ")
      calib_note <- paste0("Calibration set too small (", nrow(calib_data),
                           " < ", control$min_calib_rows, "). Disabling ", disabled, ".")

      ## Whatever verbose is: a fit without the intervals or the domain it
      ## was asked for says so (#145)
      log_calibration_skip(collector, compute_uq, compute_ad, nrow(calib_data),
                           control$min_calib_rows, top_configs$config_id)

      compute_uq <- FALSE
      compute_ad <- FALSE
      train_Fit  <- train_F
      calib_data <- NULL
      split_C    <- NULL

    }

  } else {

    train_Fit <- train_F

  }

  ## The modelled rows vary, but the rows every member is fitted on can
  ## still be constant: the few rows with another value can all be test
  ## rows, calibration rows or trimmed rows. When the constant is a finite
  ## lower bound of outcome_range the response bound equals it, and the fit
  ## validator refused the object only after every model was fitted (#132).
  ## The places are disjoint: a trimmed row drawn into the calibration set
  ## counts as trimmed, since turning UQ and AD off would not bring it back
  ## into training; only dropping the trim would.
  calib_kept <- !calib_data[[id_col]] %in% trimmed_rows[[id_col]]

  check_training_outcome_variance(
    train_Fit[[outcome_col]],
    held_out    = list(
      "in the test set"              = test_F[[outcome_col]],
      "in the calibration set"       = calib_data[[outcome_col]][calib_kept],
      "trimmed as response outliers" = trimmed_rows[[outcome_col]]
    ),
    outcome_col = outcome_col,
    verb        = "fit",
    rerun       = if (cold_start) "fit" else "evaluate"
  )

  ## -----------------------------------------------------------------------
  ## Step 2: Create CV resamples from train_Fit
  ## -----------------------------------------------------------------------
  ## The folds are drawn from train_Fit, so it is the part the sample-size
  ## rule reads: more rows than folds, as evaluate() asks of its own.

  check_fold_rows(nrow(train_Fit), cv_folds,
                  n_modelled = nrow(modelled$data),
                  n_test     = n_test,
                  n_trimmed  = length(trimmed_ids),
                  n_calib    = if (is.null(calib_data)) 0L else sum(calib_kept))

  cv_drawn <- with_unit_seed(seed, "fit/folds", draw_stratified(
    outcome      = train_Fit[[outcome_col]],
    stratified   = function() rsample::vfold_cv(train_Fit, v = cv_folds,
                                                strata = dplyr::all_of(outcome_col),
                                                breaks = STRATA_BREAKS, pool = STRATA_POOL),
    unstratified = function() rsample::vfold_cv(train_Fit, v = cv_folds)
  ))
  cv_resamples <- cv_drawn$draw

  ## A stratified draw that fell back to an unstratified one says so once,
  ## each; a small assessment fold is weak, not wrong, and warns
  if (cold_start) log_strata_fallback(collector, cold, "train/test split")
  if (!is.null(calib_drawn)) log_strata_fallback(collector, calib_drawn, "calibration split")
  log_strata_fallback(collector, cv_drawn, "cross-validation folds")
  log_small_folds(collector, cv_resamples)

  ## -----------------------------------------------------------------------
  ## Step 2b: The fold preps the members need
  ## -----------------------------------------------------------------------
  ## Each fold's prep is a unit of its own, named by what is prepped, and
  ## built once for every member that reads it (fold_prep_units()): listed
  ## before the tree's header, as evaluate() lists its configurations'.

  member_rows <- all_configs[match(top_configs$config_id, all_configs$config_id), ]
  fold_units  <- fold_prep_units(member_rows, cv_resamples, role_map, verb = "fit", data = NULL,
                                 seed = seed, sg_window = recipe_cfg$sg_window,
                                 pca_threshold = recipe_cfg$pca_threshold)

  ## With allow_par the fold preps are the window's tasks, and on the members
  ## axis the members are too, so without mirai's dispatcher they lose the
  ## longest-first start and the cancel, and fit() says so once, and on the
  ## tree's parallel line, as evaluate() does
  no_dispatcher <- allow_par && (members_axis || length(fold_units) > 0) && !daemons_dispatch()

  if (no_dispatcher) {
    log_no_dispatcher(collector, if (members_axis) "members" else "fold preparations", verb = "fit")
  }

  ## -----------------------------------------------------------------------
  ## Step 3: Tree header
  ## -----------------------------------------------------------------------

  if (verbose) {

    cat("\n")
    cat(paste0("\u250C fit ",
               paste(rep("\u2500", 57), collapse = ""), "\n"))
    cat("\u2502\n")

    seed_line <- seed_tree_line(root)

    if (!is.null(seed_line)) cat(paste0("\u2502  ", seed_line, "\n"))

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

    if (cold_start) {

      cat("\u2502  Cold start: 1 configuration, not screened by evaluate()\n")

    } else {

      cat(paste0(
        "\u2502  Fitting top ", n_best, " of ",
        nrow(eval_results), " configurations\n"
      ))

      if (n_requested > n_available) {

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Requested n_best = ", n_requested,
            " but only ", n_available,
            if (candidates$fallback) " pruned" else " successful",
            " configs available. Using ", n_available, "."
          ), "\n"
        ))

      }

    }

    ## Each draw fit() makes says "stratified" only when the strata held:
    ## rsample draws unstratified below 40 rows without an error (#91; see
    ## draw_stratified()). evaluate()'s split, reused, is reported by
    ## evaluate().

    cat(paste0(
      "\u2502  Split: ", n_train, " train / ", n_test, " test (",
      if (cold_start) {
        paste0("the rows evaluate() holds out at this seed, ",
               if (cold$stratified) "stratified" else "unstratified")
      } else {
        "evaluate()'s held-out rows"
      },
      ")\n"
    ))

    split_note <- if (cold_start) strata_tree_line(cold, "split")

    if (!is.null(split_note)) {

      cat(paste0("\u2502  ", cli::col_yellow(split_note), "\n"))

    }

    render_response_trim(x$evaluation$response_trim)

    ## The calibration set UQ and AD share, named for whichever it serves;
    ## with AD alone it went unreported. Both flags are FALSE here when the
    ## set was too small, and the note below says so instead.
    if (compute_uq || compute_ad) {

      n_trim_calib <- sum(calib_data[[id_col]] %in% trimmed_ids)

      cat(paste0(
        "\u2502  ", paste(c(if (compute_uq) "UQ", if (compute_ad) "AD"), collapse = " and "),
        " calibration: ", nrow(train_Fit), " fit / ", nrow(calib_data), " calibration, ",
        if (calib_drawn$stratified) paste0("stratified on ", outcome_col) else "unstratified",
        if (!is.null(trimmed_rows)) {
          paste0(" (drawn from the untrimmed training part; ", n_trim_calib,
                 " trimmed row", if (n_trim_calib != 1) "s", " in it)")
        } else "",
        "\n"
      ))

      calib_strata <- strata_tree_line(calib_drawn, "calibration split")

      if (!is.null(calib_strata)) {

        cat(paste0("\u2502  ", cli::col_yellow(calib_strata), "\n"))

      }

    }

    if (!is.null(calib_note)) {

      cat(paste0("\u2502  ", cli::col_yellow(calib_note), "\n"))

    }

    cat(paste0(
      "\u2502  CV: ", cv_folds, "-fold ",
      if (cv_drawn$stratified) paste0("stratified on ", outcome_col) else "unstratified",
      "\n"
    ))

    cv_note <- strata_tree_line(cv_drawn, "CV")

    if (!is.null(cv_note)) {

      cat(paste0("\u2502  ", cli::col_yellow(cv_note), "\n"))

    }

    ## A model gridded whole runs no search, so the header names it rather
    ## than giving it a search's words or budget
    cat(paste0("\u2502  ", if (cold_start) {
      paste0("Bayesian: ", cold_text)
    } else {
      words <- search_words(member_configs$model, final_bayesian_iter, tuning, warm = TRUE)
      retune_header_line(final_bayesian_iter, words$raised, words$searched, words$whole)
    }, "\n"))

    ## A parallel fit runs its members, or the members' folds, on the daemons
    for (line in parallel_tree_lines(axis$axis, n_daemons,
                                     member_configs$model, control$isolate,
                                     no_dispatcher = no_dispatcher, short_memory = short_memory,
                                     debug_off = par_by_user && debug)) {
      cat(paste0("\u2502  ", line, "\n"))
    }

    cat("\u2502\n")

  }

  ## -----------------------------------------------------------------------
  ## Step 3b: Each fold prep the members need, built once for the call
  ## -----------------------------------------------------------------------
  ## As evaluate() builds its configurations': members of one preprocessing
  ## and selection read one prep per fold. fit() has no output_dir and
  ## resumes nothing, so they live in this call; on the daemons with
  ## allow_par, each task carrying its rows. Every member is still to fit
  ## while they are built, which a stop for want of daemons says.

  fold_store <- NULL
  fold_stage <- NULL

  if (length(fold_units) > 0) {

    fold_store <- open_fold_store(NULL, member_results(list()))

    fold_stage <- build_fold_stage(
      fold_units, fold_store, cv_resamples$splits[[1]]$data, cv_resamples, role_map,
      seed          = seed,
      sg_window     = recipe_cfg$sg_window,
      pca_threshold = recipe_cfg$pca_threshold,
      debug         = debug,
      keep_traces   = control$keep_traces,
      collector     = collector,
      output_dir    = NULL,
      results       = member_results(list()),
      daemons       = if (allow_par) list(payload = NULL, n_left = nrow(member_rows), verb = "fit")
    )

    if (verbose) {

      for (line in fold_stage_lines(fold_stage, if (allow_par) n_daemons)) {
        cat(paste0("\u2502  ", line, "\n"))
      }
      cat("\u2502\n")

    }

  }

  ## -----------------------------------------------------------------------
  ## Step 3c: What every member is fitted on, and with
  ## -----------------------------------------------------------------------
  ## The rows, and the settings fit_single_config() takes that are the same
  ## for every member: one list, which the members fitted in this session
  ## and each member's task on the members axis are both fitted with
  ## (fit_member()). An argument fit_single_config() takes for every member
  ## is added to `member_settings`, once, and reaches both.

  member_inputs <- list(
    split_F      = split_F,
    cv_resamples = cv_resamples,
    calib_data   = calib_data,
    train_data   = train_Fit,
    role_map     = role_map
  )

  member_settings <- list(
    final_bayesian_iter  = tune_iter,
    grid_size            = tuning$grid_size,
    tune_ranges          = tuning$ranges,
    max_weights          = tuning$max_weights,
    no_improve           = control$no_improve,
    compute_uq           = compute_uq,
    compute_ad           = compute_ad,
    seed                 = seed,
    sg_window            = recipe_cfg$sg_window,
    pca_threshold        = recipe_cfg$pca_threshold,
    outcome_range        = outcome_range,
    response_fences      = response_fences,
    min_calib_rows       = control$min_calib_rows,
    min_ad_rows          = control$min_ad_rows,
    min_degradation_rows = control$min_degradation_rows,
    keep_traces          = control$keep_traces,
    isolate              = control$isolate
  )

  ## -----------------------------------------------------------------------
  ## Step 4: The members, in this session or each a task on the daemons
  ## -----------------------------------------------------------------------
  ## Each member is one unit (fit_single_config()). The circuit breaker
  ## watches the first members, so a failure every member would share stops
  ## the call early (control_horizons(breaker = )), and a parallel worker
  ## that dies stops it at once; either way the members so far are the
  ## error's results.

  breaker <- new_breaker(control$breaker, member_rows)

  ## A member's fold preps, from the call's stage
  fold_entries_of <- function(cfg, results) {
    if (!is.null(fold_store)) {
      config_fold_entries(fold_store, cfg, cv_resamples, role_map, verb = "fit", data = NULL,
                          seed = seed, sg_window = recipe_cfg$sg_window,
                          pca_threshold = recipe_cfg$pca_threshold, results = results)
    }
  }

  if (!members_axis) {

    ## -------------------------------------------------------------------
    ## In this session, one member after another; on the resamples axis
    ## tune runs each member's folds on the daemons
    ## -------------------------------------------------------------------

    results_list <- list()

    for (i in seq_len(n_best)) {

      cfg <- member_rows[i, ]

      ## Render config start
      if (verbose) render_member_start(i, n_best, cfg)

      ## Extract best_params from evaluate. NULL on a cold start, so the
      ## re-tune starts from build_warmstart_grid()'s space-filling fallback.
      best_params_eval <- top_configs$best_params[[i]]

      ## The member's fold preps, from the call's stage
      fold_entries <- fold_entries_of(cfg, member_results(results_list))

      ## Call capture layer, with the settings every member shares
      config_result <- stop_on_worker_death(fit_member(
        cfg, best_params_eval, fold_entries, member_inputs, member_settings,
        allow_par = axis$tune_allow_par,
        threads   = if (allow_par) ENGINE_THREADS else control$threads,
        debug     = debug
      ), member_results(results_list), verb = "fit", daemons = axis$tune_allow_par)

      results_list[[i]] <- config_result

      ## Render result
      if (verbose) render_member_block(config_result, i == n_best, cold_start, final_bayesian_iter)

      ## A run of identical failures across the members' models,
      ## transformations, preprocessing methods and feature selections stops
      ## the call while members remain; with none left they are ordinary
      ## failures
      finish_member(config_result, cfg, breaker, n_left = n_best - i, results = member_results(results_list))

      ## Memory cleanup after each config
      gc(verbose = FALSE)

    }

  } else {

    ## -------------------------------------------------------------------
    ## The members axis: each member a task of its own on the daemons (#60)
    ## -------------------------------------------------------------------
    ## Through the window evaluate()'s configs axis runs through
    ## (dispatch_units()): longest first by the cost table, a long member's
    ## folds on inner daemons its task starts, within the daemons set
    ## (split_plan()), each member's result collected into its rank's place
    ## as it finishes, and those in flight cancelled when the call stops. A
    ## daemon that dies, a pool left with none, or a member's inner daemon
    ## that dies stops the call as systemic with the members so far, as on
    ## every axis. fit() has no output_dir, so each task carries its member's
    ## data (member_payload()): the session holds one copy of the rows for
    ## each member in flight, since mirai keeps a task's message until it is
    ## collected. A member's result exists only here once it returns, so a
    ## stop loses the models of the members finished, as it does on the
    ## other axes; their rows are on the error.

    member_costs <- config_costs(member_rows, nrow(train_Fit))
    plan         <- inner_daemon_plan(member_costs, member_rows$model, cv_folds, control, reports,
                                      n_rows = nrow(x$data$analysis), n_predictors = length(axis_columns(role_map)),
                                      n_train = nrow(train_Fit), kept = config_kept(member_rows, role_map, fold_stage),
                                      collector = collector, verb = "fit", unit = "member")
    planned      <- plan$planned
    inner_plan   <- plan$inner
    blas_pinned  <- plan$pinned

    ## Which members run their folds on inner daemons, said in the tree from
    ## the plan, and on the log once the window has run, from what started
    inner_lines <- inner_daemons_tree_line(planned, inner_plan, control$inner_daemons, blas_pinned, unit = "member")

    if (verbose && length(inner_lines) > 0) {
      for (line in inner_lines) cat(paste0("\u2502  ", line, "\n"))
      cat("\u2502\n")
    }

    results_list  <- vector("list", n_best)
    inner_records <- list()
    fit_env       <- rlang::current_env()
    collected     <- function() Filter(Negate(is.null), results_list)
    rows_so_far   <- function() member_results(collected())

    ## What every task carries, built once; each task adds its member's own
    ## part, its fold preps read from the call's stage as it is sent
    shared    <- member_payload(split_F, trimmed_rows, cv_resamples, split_C, role_map, member_settings)
    task_call <- quote(worker(payload, inner))

    stop_on_worker_death(dispatch_units(
      n       = n_best,
      order   = dispatch_order(member_costs),
      task    = function(i) {
        cfg <- member_rows[i, ]
        list(call = task_call,
             args = list(worker  = fit_member_worker,
                         payload = member_task_payload(shared, cfg, top_configs$best_params[[i]],
                                                       fold_entries_of(cfg, rows_so_far())),
                         inner   = inner_plan[[i]]))
      },
      ## By its rank, not the order it finished in: best_config is the first
      ## success in rank order. What it says of its inner daemons is kept
      ## apart from it.
      collect = function(i, value) {
        inner_records[[member_rows$config_id[i]]] <<- attr(value, "inner_daemons")
        attr(value, "inner_daemons") <- NULL
        results_list[[i]] <<- value
      },
      ## The breaker reads a pass's members once every one is in, those that
      ## did not fail first; the members still to fit are those not yet
      ## collected, in flight or waiting
      pass    = function(done) {
        n_left <- n_best - length(collected())
        for (k in pass_finish_order(results_list[done])) {
          i <- done[[k]]
          finish_member(results_list[[i]], member_rows[i, ], breaker, n_left, results = rows_so_far(),
                        call = fit_env)
        }
      },
      label       = function(i) list(what = "member", id = member_rows$config_id[i]),
      rows_so_far = rows_so_far,
      verb        = "fit",
      slots       = function(i) inner_plan[[i]],
      call        = fit_env
    ), rows_so_far(), verb = "fit", call = fit_env)

    ## Each member's block, in rank order once every one is in: printed as
    ## they finished they would interleave out of rank
    if (verbose) {
      for (i in seq_len(n_best)) {
        render_member_start(i, n_best, member_rows[i, ])
        render_member_block(results_list[[i]], i == n_best, cold_start, final_bayesian_iter)
      }
    }

    ## Which members ran their folds on inner daemons, from what each task
    ## says; one whose inner daemons did not connect ran its folds on its
    ## own daemon, which costs time only
    log_inner_daemons(collector, member_rows$config_id, planned, inner_plan, control$inner_daemons,
                      blas_pinned, inner_records, verb = "fit", unit = "member")

  }

  ## What the members reported goes on the call's log, from their records
  results_list <- lift_unit_records(results_list, collector)

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
  results_tibble <- member_results(results_list)

  ## Every member failed. Abort here with the members' errors; carrying on
  ## reached validate_horizons_fit(), which refused the empty workflows slot
  ## with a structural message that said nothing about why.
  if (length(workflows_list) == 0) {

    abort_all_members_failed(results_tibble)

  }

  ## Some members failed and the rest are returned: one summary, with each
  ## distinct cause and the members it failed
  failed_rows <- results_tibble[results_tibble$status == "failed", , drop = FALSE]

  if (nrow(failed_rows) > 0) {

    log_warning(collector, "partial_failure", c(
      "{nrow(failed_rows)} of {nrow(results_tibble)} member{?s} failed, so the fit holds the rest.",
      distinct_config_errors(failed_rows)
    ), ids = failed_rows$config_id)

  }

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
  ## predict_one_config(). A margin over the maximum (not a quantile) — the
  ## bound should permit modest extrapolation and catch only the physically
  ## absurd. Scaled on the span from the range's floor (or the training
  ## minimum, with no floor) so it lies above the maximum for a signed outcome
  ## too; under the default range it is max * RESPONSE_BOUND_MARGIN exactly
  ## (#76). Taken over train_Fit, the rows the final models are fit on, so
  ## Split F's test rows never shape it (#68), plus the rows a response trim
  ## left out of them (#77): those are training-partition rows too, and a
  ## bound under values actually observed there would clamp the very
  ## extremes the trim set aside. The calibration draw takes from the trimmed
  ## rows as well, so after a trim the calibration rows among them shape it
  ## too; without a trim no calibration row does.
  response_bound <- compute_response_bound(
    c(train_Fit[[outcome_col]], trimmed_rows[[outcome_col]]), outcome_range
  )

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
    uq                = uq_list,
    ad                = ad_list,
    selection_present = selection_present,
    ## The run's root and its rule: ensemble() takes the root unless given
    ## one, and a re-fit takes it too
    seed              = seed,
    seed_rule         = SEED_RULE,
    ## How the run went, as evaluate() records it: the daemons connected
    ## when it began (1 in this session) and the axis they were applied to.
    ## The results do not depend on either.
    workers           = if (axis$axis == "sequential") 1L else n_daemons,
    parallelize_over  = axis$axis,
    control           = control,
    timestamp         = Sys.time(),
    runtime_secs      = total_runtime
  )

  class(x) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  ## How many of the re-tunes' Bayesian searches the stopping rule ended: a
  ## note on the log, and a line in the summary below
  stop_line <- early_stop_line(results_tibble$bayes_stop, control$no_improve,
                               if (cold_start) "The cold start's Bayesian search" else "The re-tune's Bayesian search",
                               "member")

  if (!is.null(stop_line)) log_note(collector, "{stop_line}")

  ## A cold start's Bayesian search, by bayesian_iter, as the header says it
  if (cold_start) {

    cold_line <- paste0("The cold start's Bayesian search, as evaluate()'s: ", cold_text, ".")
    log_note(collector, "{cold_line}")

  }

  ## How many members kept evaluate()'s parameters: a note, and a line below.
  ## With the re-tune off every member with them keeps them, which one note
  ## says, as the header does.
  kept_line <- if (final_bayesian_iter > 0) retune_kept_line(results_tibble$retune_moved)

  if (!is.null(kept_line)) log_note(collector, "{kept_line}")

  if (final_bayesian_iter == 0 && any(results_tibble$warm_start %in% TRUE)) {

    off_line <- paste0("The re-tune is ", retune_off_text(), ".")
    log_note(collector, "{off_line}")

  }

  ## What the run reported goes on the object's log before it is certified
  x$log <- log_close(collector, x$log)

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

    if (!is.null(stop_line)) cat(paste0("\u2502  \u251C\u2500 ", stop_line, "\n"))
    if (!is.null(kept_line)) cat(paste0("\u2502  \u251C\u2500 ", kept_line, "\n"))

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

  ## Each warning the run recorded, raised once, after the tree
  signal_log(collector, x)

  x

}

## ---------------------------------------------------------------------------
## member_results — the members table
## ---------------------------------------------------------------------------

#' The members table of a fit
#'
#' @description
#' One row per member `fit_single_config()` returned, fitted or failed:
#' `models$results`, and the `results` a stop carries. A member list
#' without a field (a failed one, or one built by hand) records a typed `NA`.
#'
#' @param results_list List of members, as `fit_single_config()` returns them.
#' @return Tibble.
#' @keywords internal
#' @noRd
member_results <- function(results_list) {

  ## No member yet: a table with every column and no rows, so a stop before
  ## the first member finished carries the same shape as one after it
  if (length(results_list) == 0) {
    return(member_row(list(config_id = NA_character_, status = NA_character_))[0, ])
  }

  purrr::map_dfr(results_list, member_row)

}

#' One member's row of the members table
#'
#' @param res A member, as `fit_single_config()` returns it.
#' @return One-row tibble.
#' @keywords internal
#' @noRd
member_row <- function(res) {

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
    start_grid_size = as.integer(res$start_grid_size %||% NA_integer_),
    bayes_iter      = as.integer(res$bayes_iter %||% NA_integer_),
    bayes_stop      = as.character(res$bayes_stop %||% NA_character_),
    retune_moved    = as.logical(res$retune_moved %||% NA),
    error_message   = res$error_message %||% NA_character_,
    class           = res$class %||% NA_character_,
    trace           = list(res$trace),
    warnings        = list(res$warnings),
    runtime_secs    = as.numeric(res$runtime_secs %||% NA_real_)
  )

}

## ---------------------------------------------------------------------------
## A member's fit, in this session or in its own task on a daemon
## ---------------------------------------------------------------------------

#' Fit one member, with the settings every member shares
#'
#' @description
#' The one call of [fit_single_config()] for a member, in this session and in
#' a members-axis task ([fit_member_worker()]): its configuration,
#' `evaluate()`'s parameters for it and its fold preparations; the rows it is
#' fitted, scored and calibrated on (`inputs`); the settings every member
#' shares (`settings`, `fit()`'s `member_settings`); and how this run fits
#' it. An argument [fit_single_config()] takes for every member is added to
#' `member_settings`, once, so a member fits alike wherever it runs. Each
#' argument is passed by its own name ([call_by_name()]), so the call holds
#' none of the data, and nor does a trace that records it.
#'
#' @param config_row The member's configuration, a one-row data frame.
#' @param best_params_eval `evaluate()`'s parameters for it, or `NULL` on a
#'   cold start.
#' @param fold_entries Its fold preparations, or `NULL` with no feature
#'   selection.
#' @param inputs List: `split_F`, `cv_resamples`, `calib_data`, `train_data`
#'   and `role_map`, as [fit_single_config()] takes them.
#' @param settings Named list: the rest of [fit_single_config()]'s arguments
#'   that are the same for every member.
#' @param allow_par Logical(1). Whether tune runs the member's folds on the
#'   daemons: the resamples axis's, or a members-axis task's inner daemons.
#' @param threads A whole number: the threads its engine fits on.
#' @param debug Logical(1). Contain nothing; only in this session.
#' @param outside_warnings Character. Warnings caught outside the member's
#'   unit, by its task.
#' @return What [fit_single_config()] returns.
#' @keywords internal
#' @noRd
fit_member <- function(config_row, best_params_eval, fold_entries, inputs, settings,
                       allow_par, threads, debug = FALSE, outside_warnings = character(0)) {

  args <- c(list(config_row = config_row, best_params_eval = best_params_eval), inputs, settings,
            list(allow_par = allow_par, threads = threads, debug = debug,
                 outside_warnings = outside_warnings, fold_entries = fold_entries))

  eval(call_by_name(quote(fit_single_config), args), args)

}

#' What every member's task carries on the members axis
#'
#' @description
#' The part of a member's task ([fit_member_worker()]) that is the same for
#' every member: the modelled rows once, with the training rows a response
#' trim left out when a calibration set is drawn from them, the positions
#' its split, fit rows, calibration rows and folds are rebuilt from
#' ([resample_indices()]), the role map, `fit()`'s `member_settings` and
#' this session's version. Each member's task adds its own part
#' ([member_task_payload()]).
#'
#' @param split_F `fit()`'s split, pointed at the modelled rows.
#' @param trimmed_rows The training rows a response trim left out, or `NULL`:
#'   carried once, for the calibration pool to take in and the fit rows to
#'   drop ([calibration_parts()]).
#' @param cv_resamples `fit()`'s folds.
#' @param split_C `fit()`'s calibration split, or `NULL` when no calibration
#'   set was drawn or it was too small to use.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param settings `fit()`'s `member_settings`.
#' @return List with the keys `FIT_ARG_NAMES` less the member's own.
#' @keywords internal
#' @noRd
member_payload <- function(split_F, trimmed_rows, cv_resamples, split_C, role_map, settings) {

  calibrated <- !is.null(split_C)

  list(
    data         = split_F$data,
    trimmed      = if (calibrated) trimmed_rows,
    resample_idx = resample_indices(split_F, cv_resamples, calib = split_C),
    role_map     = role_map,
    settings     = settings,
    pkg_version  = as.character(utils::packageVersion("horizons"))
  )

}

#' A member's task: what every member's carries, and its own
#'
#' @param shared From [member_payload()].
#' @param config_row The member's configuration, a one-row data frame.
#' @param best_params `evaluate()`'s parameters for it, or `NULL`.
#' @param fold_entries Its fold preparations, or `NULL`.
#' @return List with the keys `FIT_ARG_NAMES`.
#' @keywords internal
#' @noRd
member_task_payload <- function(shared, config_row, best_params, fold_entries) {

  c(shared, list(config_row = config_row, best_params = best_params, fold_entries = fold_entries))

}

#' Fit One Member Inside a Task on a Daemon
#'
#' @description
#' The body `fit()`'s members axis sends each daemon, one task a member,
#' through the window ([dispatch_units()]), kept at top level, as
#' [evaluate_config_worker()] is, so a task carries the function's code and
#' its arguments and nothing of `fit()`'s frame. `fit()` has no `output_dir`,
#' so there is no payload file: each task carries its member's data
#' (`FIT_ARG_NAMES`), as `fit()`'s fold preparations' tasks carry theirs.
#'
#' The version is checked first, since another version's payload keys may
#' differ, then the payload's keys ([check_worker_payload()]). The daemon is
#' set up for the member as for any unit ([prepare_worker()]: BLAS's,
#' OpenMP's and data.table's threads and the engines' thread options at
#' one, all put back as the task ends, and the member's packages); its
#' split, its fit and calibration
#' rows and its folds are rebuilt from the rows and their positions
#' ([rebuild_resamples()]), the rows sent once. A warning raised in those
#' steps, outside the member's unit, is caught and rendered with the
#' member's own, where mirai would drop it.
#'
#' With `inner` above one the task starts that many inner daemons on its own
#' daemon, tune runs the member's folds on them, and they are reset as the
#' member returns, however it ends; a pool that does not come up runs the
#' folds here ([run_on_inner_daemons()], the bracket both verbs' workers
#' share). Then the member is fitted as this session
#' fits one ([fit_member()]), its prediction intervals and applicability
#' domain among it: they need the workflow before it is slimmed, and their
#' draws are units named by the member, so they give the same numbers on any
#' axis.
#'
#' @param payload List with the keys `FIT_ARG_NAMES`: `data` (the modelled
#'   rows less those a response trim left out), `trimmed` (those, or
#'   `NULL`), `resample_idx` ([resample_indices()], with the calibration
#'   split when there is one), `role_map`, `settings` (`fit()`'s
#'   `member_settings`), `config_row`, `best_params` (`evaluate()`'s, or
#'   `NULL`), `fold_entries` and `pkg_version`. No `debug`, since a debug run
#'   never dispatches.
#' @param inner Integer(1). The daemons the member's folds run on
#'   ([split_plan()]). With 1, tune runs them sequentially in this task.
#' @return What [fit_single_config()] returns, its unit's records on it for
#'   `fit()` to lift, and its inner daemons as the attribute
#'   `"inner_daemons"`: `daemons` (`inner`), `started` (whether they
#'   connected) and `completed` (the tasks they ran for it, past their
#'   set-up). When an inner daemon died, instead, the record
#'   [watch_inner_daemons()] returns.
#' @keywords internal
#' @noRd
fit_member_worker <- function(payload, inner = 1L) {

  ## -------------------------------------------------------------------------
  ## The worker's horizons, then the cross-process contract
  ## -------------------------------------------------------------------------
  ## As evaluate_config_worker() checks them: the version first, since
  ## another version's payload keys may differ, which is a version skew, not
  ## a bug; then the keys, since a mis-keyed entry arrives as NULL.

  worker_version <- as.character(utils::packageVersion("horizons"))

  if (rlang::is_string(payload$pkg_version) && !identical(worker_version, payload$pkg_version)) {
    abort_worker_version(worker_version, payload$pkg_version)
  }

  check_worker_payload(payload, FIT_ARG_NAMES)

  ## The daemon's threads go to one as it is set up below (BLAS's, OpenMP's
  ## and data.table's, and the engines' thread options), as the inner
  ## daemons run them, and all of them are put back as the task ends, since
  ## the daemon is the user's. The put-back is registered first and bound
  ## once the pin is made.
  unpin_threads <- function() invisible(NULL)
  on.exit(unpin_threads(), add = TRUE)

  cfg     <- payload$config_row
  outside <- character(0)

  rows <- withCallingHandlers({

    ## The daemon's threads pinned, and the member's packages loaded
    unpin_threads <- prepare_worker(cfg$model, cfg$feature_selection)

    ## The split, the fit and calibration rows and the folds, from one copy
    ## of the rows and their positions
    rebuild_resamples(payload$data, payload$resample_idx, trimmed = payload$trimmed,
                      id_col = id_column(payload$role_map))

  }, warning = function(w) {
    outside <<- c(outside, condition_summary(w))
    rlang::cnd_muffle(w)
  })

  ## -------------------------------------------------------------------------
  ## The member, its folds on inner daemons when the plan splits it
  ## -------------------------------------------------------------------------
  ## The inner daemons are this daemon's own profile, which tune reads, and
  ## are reset as the bracket returns (run_on_inner_daemons()). What they did
  ## is told the session, which keeps it apart from the member.

  inputs <- list(split_F = rows$split, cv_resamples = rows$cv_folds, calib_data = rows$calib,
                 train_data = rows$train, role_map = payload$role_map)

  run_on_inner_daemons(function(on_inner) {
    fit_member(cfg, payload$best_params, payload$fold_entries, inputs, payload$settings,
               allow_par = on_inner, threads = ENGINE_THREADS, outside_warnings = outside)
  }, inner, cfg)

}

## ---------------------------------------------------------------------------
## A member, finished: the breaker, and its block in the tree
## ---------------------------------------------------------------------------

#' Feed a finished member to the circuit breaker, and stop when it trips
#'
#' @description
#' What `fit()` does with each member as it finishes, on every axis: in this
#' session as each returns, and on the members axis as the window collects
#' each, those of a pass that did not fail first ([pass_finish_order()]).
#' The member feeds the breaker ([breaker_feed()]); when it trips with
#' members still to fit the call stops ([abort_breaker_trip()]), and with
#' none left the members it held are ordinary failures, so it is released.
#' `fit()` checkpoints nothing, so a released member goes nowhere.
#'
#' @param result The member, as [fit_single_config()] returns it.
#' @param config The member's configuration, a one-row data frame: the
#'   breaker reads its model, transformation, preprocessing and feature
#'   selection.
#' @param breaker From [new_breaker()].
#' @param n_left Integer(1). The members still to fit once this one is done:
#'   on the members axis those not yet collected, in flight or waiting.
#' @param results The members table so far ([member_results()]), carried by
#'   the stop; evaluated only when the breaker trips.
#' @param call The frame the stop reports: `fit()`'s.
#' @return `NULL`, invisibly; or does not return, when the call stops.
#' @keywords internal
#' @noRd
finish_member <- function(result, config, breaker, n_left, results, call = rlang::caller_env()) {

  breaker_feed(breaker, result, config)

  if (breaker$tripped) {

    if (n_left > 0) {
      abort_breaker_trip(breaker, results, n_left, verb = "fit", units = "members", checkpoints = FALSE,
                         call = call)
    }

    breaker_release(breaker)

  }

  invisible(NULL)

}

#' Print a member's line in fit()'s tree
#'
#' @param i,n_best Integer(1). The member's rank and the members fitted.
#' @param config_row The member's configuration, a one-row data frame.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
render_member_start <- function(i, n_best, config_row) {

  config_desc <- paste(model_label(config_row$model), config_row$transformation, config_row$preprocessing,
                       config_row$feature_selection, sep = " + ")
  branch      <- if (i == n_best) "\u2514\u2500" else "\u251C\u2500"

  cat(paste0("\u2502  ", branch, " [", i, "/", n_best, "] ", config_desc, "\n"))

  invisible(NULL)

}

#' Print what a member's fit came to, under its line in fit()'s tree
#'
#' @description
#' The member's block: how its re-tune started and what it kept, its test
#' and cross-validated metrics, the degradation flag and the measured
#' coverage, or its failure; its warnings; and its run time. The one
#' printer of a member, so a member prints alike as it returns in this
#' session and, on the members axis, in rank order once the window has
#' collected every member.
#'
#' @param config_result The member, as [fit_single_config()] returns it.
#' @param last Logical(1). Whether it is the last member, whose block closes
#'   the tree's branch.
#' @param cold_start Logical(1). Whether `fit()` started cold.
#' @param final_bayesian_iter Integer(1). The re-tune's budget, 0 when it is
#'   off.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
render_member_block <- function(config_result, last, cold_start, final_bayesian_iter) {

  cont <- if (last) "   " else "\u2502  "

  ## A re-tune with no usable parameters to centre on starts from a
  ## space-filling grid. On a cold start that is by design; on the
  ## evaluate() path it means evaluate()'s parameters were unusable.
  if (isFALSE(config_result$warm_start)) {

    start_text <- paste0(
      if (cold_start) "Cold start: no warm-start parameters" else "No usable warm-start parameters from evaluate()",
      "; grid of ", config_result$start_grid_size, " points"
    )

    cat(paste0(
      "\u2502  ", cont, "\u251C\u2500 ",
      if (cold_start) start_text else cli::col_yellow(start_text), "\n"
    ))

  }

  ## Whether the re-tune kept evaluate()'s parameters, and by how much
  ## its best candidate led them; with the re-tune off the header says
  retune_text <- if (final_bayesian_iter > 0) {
    retune_member_line(config_result$retune_moved, config_result$retune_gain)
  }

  if (!is.null(retune_text)) cat(paste0("\u2502  ", cont, "\u251C\u2500 ", retune_text, "\n"))

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
    if (isTRUE(config_result$degraded)) {

      cat(paste0(
        "\u2502  ", cont, "\u251C\u2500 ",
        cli::col_yellow("\u26A0 DEGRADED: ",
                        config_result$degraded_reason), "\n"
      ))

    }

    ## UQ coverage, measured on the held-out test rows (#118)
    if (!is.null(config_result$uq)) {

      cat(paste0(
        "\u2502  ", cont, "\u251C\u2500 ",
        format_test_coverage(config_result$uq), "\n"
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

  invisible(NULL)

}


## ---------------------------------------------------------------------------
## The re-tune's lines in the tree and the log
## ---------------------------------------------------------------------------

#' The tree header's line for the re-tune on the evaluate() path
#'
#' The search's words for the members that search, and the models gridded
#' whole named apart ([whole_space_models()]). The stopping rule is said
#' where it stops a search, in the summary and the log
#' ([early_stop_line()]), as `evaluate()`'s tree says it.
#'
#' @param iter Integer. `final_bayesian_iter`, 0 for the re-tune off.
#' @param raised Character(1) or `NULL`: [raised_budgets()]'s words, for the
#'   models that search.
#' @param searched,whole Character: the members' models that search, and
#'   those gridded whole.
#' @return Character(1).
#' @keywords internal
#' @noRd
retune_header_line <- function(iter, raised, searched, whole) {

  if (iter == 0) return(paste0("Re-tune: ", retune_off_text()))

  search <- if (length(searched) > 0) {
    paste0("from evaluate()'s hyperparameters and d points drawn around them for d tuned parameters, ", iter,
           " iterations", if (!is.null(raised)) paste0(" (", raised, ")"))
  }

  paste0(
    "Re-tune: ", paste(c(search, gridded_whole_text(whole)), collapse = "; "),
    "; evaluate()'s are kept unless the best candidate is better by more than its standard error"
  )

}

#' The tree's words for the models gridded whole
#'
#' @param whole Character: [whole_space_models()]'s.
#' @return Character(1), e.g. `"plsr and mars gridded whole"`, or `NULL`
#'   when there are none.
#' @keywords internal
#' @noRd
gridded_whole_text <- function(whole) {

  if (length(whole) == 0) return(NULL)

  paste(paste(whole, collapse = " and "), "gridded whole")

}

#' What fit() says when its re-tune is off
#'
#' Off is the default, which the words say, so a run left at the defaults
#' does not read as one set up to skip the re-tune.
#'
#' @return Character(1): the words the tree's header and the log's note share.
#' @keywords internal
#' @noRd
retune_off_text <- function() {

  setting <- paste0("(`final_bayesian_iter = 0`", if (DEFAULT_FINAL_BAYES_ITER == 0L) ", the default", ")")

  paste0("off ", setting, ", so each member keeps evaluate()'s hyperparameters; ",
         "`configure(final_bayesian_iter = )` above 0 turns it on")

}

#' The members' models gridded whole, those that search, and their raised budgets
#'
#' What the tree's header and a cold start's note say of the search: a model
#' gridded whole runs none ([whole_space_models()]), and the budgets of those
#' that search are raised to ten points a tuned parameter
#' ([raised_budgets()]).
#'
#' @param models Character: the members' models.
#' @param iter Integer: the budget the members' tuning runs.
#' @param tuning List: `config$tuning`.
#' @param warm Logical(1): whether the searches start from `evaluate()`'s
#'   point and its neighbours (the re-tune), not from the grid.
#' @return List: `whole` and `searched` (character), `raised` (character(1)
#'   or `NULL`).
#' @keywords internal
#' @noRd
search_words <- function(models, iter, tuning, warm) {

  whole    <- whole_space_models(models, iter, tuning$grid_size, tuning$ranges, warm = warm)
  searched <- setdiff(unique(models), whole)

  list(whole = whole, searched = searched,
       raised = raised_budgets(searched, iter, tuning$grid_size, warm = warm))

}

#' What fit() says of a cold start's Bayesian search
#'
#' A cold start tunes its one configuration as `evaluate()` would, by
#' `bayesian_iter`, so its words name that budget; a model gridded whole
#' runs no search ([whole_space_models()]).
#'
#' @param iter Integer. `bayesian_iter`.
#' @param raised Character(1) or `NULL`: [raised_budgets()]'s words.
#' @param searched,whole Character: the member's model when it searches, or
#'   when it is gridded whole.
#' @return Character(1): the words the tree's header (after "Bayesian: ") and
#'   the log's note share.
#' @keywords internal
#' @noRd
cold_tuning_text <- function(iter, raised, searched, whole) {

  search <- if (length(searched) == 0) {
    "none"
  } else if (iter > 0) {
    paste0(iter, " iterations (`bayesian_iter`) from a space-filling grid",
           if (!is.null(raised)) paste0(" (", raised, ")"))
  } else {
    paste0("none (`bayesian_iter = 0`), so the member keeps the grid's best; ",
           "`configure(bayesian_iter = )` above 0 searches from the grid")
  }

  paste(c(search, gridded_whole_text(whole)), collapse = "; ")

}

#' A member's tree line on what its re-tune kept
#'
#' The lead is rounded to two significant digits, except that a moved
#' member's never reads at or below the margin (`RETUNE_MARGIN_SE`): one
#' that rounds there reads "more than" the margin. A kept member's lead is at
#' most the margin, and so is its rounding.
#'
#' @param moved,gain The member's `retune_moved` and `retune_gain`.
#' @return Character(1), or `NULL` with nothing to keep (`moved` missing or
#'   `NA`).
#' @keywords internal
#' @noRd
retune_member_line <- function(moved, gain) {

  if (is.null(moved) || is.na(moved)) return(NULL)

  gain  <- gain %||% NA_real_
  shown <- signif(gain, 2)

  if (moved && !is.na(gain) && shown <= RETUNE_MARGIN_SE) shown <- paste("more than", RETUNE_MARGIN_SE)

  by <- if (!is.na(gain)) paste0(" by ", shown, " of its standard errors")

  if (moved) {
    return(paste0("Re-tune: moved to ", if (is.null(by)) "its best candidate" else paste0("a candidate better", by)))
  }

  if (isTRUE(gain == 0)) return("Re-tune: kept evaluate()'s hyperparameters, which no candidate beat")

  paste0("Re-tune: kept evaluate()'s hyperparameters", if (!is.null(by)) paste0("; the best candidate was better", by))

}

#' The run's line on how many members kept evaluate()'s hyperparameters
#'
#' @param moved Logical: each member's `retune_moved`, `NA` where its
#'   re-tune had no parameters of `evaluate()`'s to keep.
#' @return Character(1), or `NULL` when no member kept them.
#' @keywords internal
#' @noRd
retune_kept_line <- function(moved) {

  n_ran  <- sum(!is.na(moved))
  n_kept <- sum(moved %in% FALSE)

  if (n_kept == 0) return(NULL)

  cli::format_inline(
    "The re-tune kept evaluate()'s hyperparameters for {n_kept} of {n_ran} member{?s}: the best candidate replaces them only when its CV RMSE is lower by more than its standard error, or when they were not scored on every fold."
  )

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
#' at the same `seed` ([draw_eval_split()], after the same Savitzky-Golay
#' window check, [evaluation_recipe()]), and
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
#' @param seed The run's root, which `fit()` resolved.
#' @param collector `fit()`'s collector, from `log_open()`, which records a
#'   response trim that could not run.
#' @param call The call the conditions are attributed to. Default: the
#'   caller, `fit()`.
#' @return List with `evaluation` (the record), `stratified` (`FALSE` when
#'   the split is unstratified), `strata_failed` (`TRUE` when the
#'   stratified draw failed and was retried without strata),
#'   `strata_error` (why) and `strata_warnings` (what rsample warned of), as
#'   [draw_eval_split()] returns them. It runs no sample-size check: `fit()`
#'   checks the rows its folds are drawn from, which refuses every case a
#'   check here would, and the split refuses one modelled row by the same
#'   rule, since it would leave no training row. Aborts with class
#'   `horizons_error_input` when the object has no configuration or more than
#'   one, the window is at least as
#'   wide as the spectrum or would run across a gap in the axis, or
#'   `validate()`'s response-trim request is for
#'   another outcome ([response_trim_request()]).
#' @keywords internal
#' @noRd
cold_start_evaluation <- function(x, metric, seed, collector, call = rlang::caller_env()) {

  configs   <- x$config$configs
  n_configs <- if (is.data.frame(configs)) nrow(configs) else 0L

  if (n_configs == 0) {

    abort_horizons(c(
      "There is no configuration to fit.",
      "i" = "Run {.fn configure} first."
    ), kind = "input", call = call)

  }

  if (n_configs > 1) {

    abort_horizons(c(
      "{.fn fit} can start without {.fn evaluate} only from a single configuration, and this object has {n_configs} configurations.",
      "i" = "Choosing among configurations is what {.fn evaluate} is for. Run it first, and {.fn fit} re-tunes the best of them."
    ), kind = "input", call = call)

  }

  ## fit() has checked `metric`. A cold re-fit keeps the metric its record
  ## carries, as a fit of an evaluated object keeps evaluate()'s.
  rank_metric <- metric %||% x$evaluation$rank_metric %||% "rpd"

  ## The rows evaluate() would model
  role_map    <- x$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]
  modelled    <- modelling_rows(x$data$analysis, outcome_col, call = call)

  ## evaluate()'s next check: the Savitzky-Golay window has to fit the
  ## spectrum (#62). The record it returns is the one evaluate() stores, and
  ## fit() builds its recipe from the same settings.
  recipe_record <- evaluation_recipe(x, call = call)

  ## The response trim evaluate() applies after the same draw (#77): a trim
  ## validate() requested runs on this split's training partition by the
  ## same helper, so a cold start leaves out the rows evaluate() would. The
  ## request is read first, as evaluate() reads it, so one that cannot apply
  ## is refused before anything is drawn.
  trim_request <- response_trim_request(x)

  drawn <- draw_eval_split(modelled$data, outcome_col, seed, x$config$tuning$cv_folds,
                           call = call)

  trimmed <- trim_training_responses(drawn$split, outcome_col, trim_request,
                                     id_col = id_column(role_map), collector = collector)
  split   <- trimmed$split

  ## The sample-size rule is fit()'s own check on the rows its folds are
  ## drawn from, which are this training part less any calibration rows, so
  ## it refuses every case a check here would, as evaluate() refuses them.
  ## One modelled row, which leaves the split no training row, the draw
  ## above refuses by the same rule.

  ## The shape of an evaluate() results row (create_failed_result()'s
  ## columns less scoring_schema, since nothing was scored), all unmeasured
  results <- tibble::tibble(
    config_id             = configs$config_id,
    status                = "not_evaluated",
    below_prune_threshold = NA,
    prune_threshold       = NA_real_,
    bayes_iter            = NA_integer_,
    bayes_stop            = NA_character_,
    rmse    = NA_real_, rrmse    = NA_real_, rsq    = NA_real_,
    ccc     = NA_real_, rpd      = NA_real_, mae    = NA_real_,
    cv_rmse = NA_real_, cv_rrmse = NA_real_, cv_rsq = NA_real_,
    cv_ccc  = NA_real_, cv_rpd   = NA_real_, cv_mae = NA_real_,
    best_params           = list(NULL),
    error_message         = NA_character_,
    class                 = NA_character_,
    trace                 = list(NULL),
    warnings              = list(NULL),
    runtime_secs          = NA_real_
  )

  list(
    evaluation = list(
      results       = results,
      best_config   = configs$config_id,
      rank_metric   = rank_metric,
      screened      = FALSE,
      ## Positions and identity columns only, as evaluate() stores it
      split         = slim_split(split, role_map),
      n_train       = nrow(rsample::training(split)),
      n_test        = nrow(rsample::testing(split)),
      response_trim = trimmed$record,
      recipe        = recipe_record,
      ## The root the split was drawn from, and its rule, as evaluate()
      ## records them
      seed          = seed,
      seed_rule     = SEED_RULE,
      ## No evaluate() ran, so there is no control it ran with
      control       = NULL,
      runtime_secs  = 0,
      timestamp     = Sys.time()
    ),
    stratified      = drawn$stratified,
    strata_failed   = drawn$strata_failed,
    strata_error    = drawn$strata_error,
    strata_warnings = drawn$strata_warnings
  )

}

## ---------------------------------------------------------------------------
## check_evaluated_outcome_range — evaluate() ranked under this range
## ---------------------------------------------------------------------------

#' Refuse a fit whose evaluation was scored under another outcome range
#'
#' @description
#' `evaluate()` stamps the range it clamped with on every results row, in the
#' `settings` record (#76). `fit()` takes its members and their warm-start
#' parameters from those rows, so a range changed since `evaluate()` would
#' carry the old range's scoring into the new fit. A recorded range must
#' equal this object's, and a row that records none is refused under every
#' range.
#'
#' @param x A `horizons_eval` that was screened by `evaluate()`.
#' @param outcome_range `numeric(2)`. The object's range, from
#'   [outcome_range_setting()].
#' @param call The call the condition is attributed to. Default: the caller,
#'   `fit()`.
#' @return `NULL`, invisibly. Aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_evaluated_outcome_range <- function(x, outcome_range,
                                          call = rlang::caller_env()) {

  ### The eval validator at fit()'s door requires results with rows and,
  ### since fit() calls this only on a screened evaluation, their settings.
  results <- x$evaluation$results

  stamps <- lapply(results$settings, function(s) if (is.list(s)) normalize_eval_settings(s) else NULL)

  differs <- !vapply(stamps, function(s) {
    identical(s$outcome_range, as.double(outcome_range))
  }, logical(1))

  if (!any(differs)) return(invisible(NULL))

  recorded <- unique(vapply(stamps[differs], function(s) {
    if (is.null(s$outcome_range)) "no outcome range" else format_outcome_range(s$outcome_range)
  }, character(1)))

  range_text <- format_outcome_range(outcome_range)

  abort_horizons(c(
    "{.fn evaluate} scored these configurations under another {.arg outcome_range}.",
    "x" = "{.fn evaluate} recorded {recorded} on {sum(differs)} results row{?s}; this object's range is {range_text}.",
    "i" = "The members and their warm-start parameters were chosen on predictions clamped to that range.",
    "i" = "Re-run {.fn evaluate} on the object as it is now."
  ), kind = "input", call = call)

}

## ---------------------------------------------------------------------------
## check_trim_request — the trim evaluate() applied is the one requested
## ---------------------------------------------------------------------------

#' Refuse a fit whose response-trim request changed after evaluate()
#'
#' @description
#' On the `evaluate()` path `fit()` reuses the trim `evaluate()` applied
#' (`evaluation$response_trim`): the split already excludes the rows it
#' trimmed, and fences recomputed here could fall on other rows. `validate()`
#' writes its request (`validation$outliers$response_trim`) on an object of
#' any class without resetting `evaluation`, so a request made after
#' `evaluate()` would be ignored, silently (#137). This compares the two on
#' what a request carries (`outcome`, `method`, `threshold`) and refuses when
#' they differ: a request with no trim applied, a trim applied with no
#' request, or another outcome, method or threshold. No request and no trim
#' match.
#'
#' A trim that drew no fences (`skipped`, so nothing was trimmed) is still
#' the trim `evaluate()` ran under, and compares like any other: it matches
#' the request it ran for, and not a missing one, since the record the fit
#' carries forward says a trim was requested.
#'
#' @param x A `horizons_eval` that was screened by `evaluate()`.
#' @param call The call the condition is attributed to. Default: the caller,
#'   `fit()`.
#' @return `NULL`, invisibly. Aborts with class `horizons_error_input`,
#'   saying what differs.
#' @keywords internal
#' @noRd
check_trim_request <- function(x, call = rlang::caller_env()) {

  requested <- trim_settings(x$validation$outliers$response_trim)
  applied   <- trim_settings(x$evaluation$response_trim)

  if (identical(requested, applied)) return(invisible(NULL))

  ## What differs, one bullet per field when both exist
  req_threshold <- format(requested$threshold)
  app_threshold <- format(applied$threshold)

  differs <- if (is.null(applied)) {

    c("x" = "{.fn validate} requests a trim of {.field {requested$outcome}} at {req_threshold} x IQR, and {.fn evaluate} applied none.")

  } else if (is.null(requested)) {

    skip_note <- if (!is.na(x$evaluation$response_trim$skipped %||% NA_character_)) {
      " (no fences could be drawn, so it trimmed no rows)"
    } else {
      ""
    }

    c("x" = "{.fn evaluate} applied a trim of {.field {applied$outcome}} at {app_threshold} x IQR{skip_note}, and {.fn validate} now requests none.")

  } else {

    ### The door validator refuses a request for another outcome than the
    ### one modelled, which is the one evaluate() trimmed.
    c(
      if (!identical(requested$method, applied$method)) {
        c("x" = "The request's method is {.val {requested$method}}; {.fn evaluate} used {.val {applied$method}}.")
      },
      if (!identical(requested$threshold, applied$threshold)) {
        c("x" = "The request's threshold is {req_threshold} x IQR; {.fn evaluate} trimmed at {app_threshold} x IQR.")
      }
    )

  }

  ## The cheap way back to the trim evaluate() applied: restore its request.
  ## Offered only when the outcome matches, since validate() requests a trim
  ## of the configured outcome.
  restore <- if (!is.null(applied) &&
                 (is.null(requested) || identical(requested$outcome, applied$outcome))) {
    c("i" = "To fit with the trim {.fn evaluate} applied, restore its request: {.code validate(x, remove_outliers = \"response\", response_threshold = {app_threshold})}.")
  }

  abort_horizons(c(
    "{.fn validate}'s response-trim request differs from the trim {.fn evaluate} applied.",
    differs,
    "i" = "{.fn fit} reuses the rows {.fn evaluate} trimmed; it does not apply a request itself.",
    restore,
    "i" = paste(if (is.null(restore)) "Re-run" else "Or re-run",
                "{.fn evaluate} to apply the current request, then {.fn fit}.")
  ), kind = "input", call = call)

}

#' The fields a response-trim request and an applied trim share
#'
#' @param trim `validation$outliers$response_trim` or
#'   `evaluation$response_trim`, or `NULL`.
#' @return `NULL` when `trim` is, otherwise a list of `outcome`, `method`
#'   (`"iqr"` when absent, as [response_trim_request()] reads it) and
#'   `threshold`, normalized so the two records compare with `identical()`.
#' @keywords internal
#' @noRd
trim_settings <- function(trim) {

  if (is.null(trim)) return(NULL)

  list(outcome   = as.character(trim$outcome %||% NA_character_),
       method    = as.character(trim$method %||% "iqr"),
       threshold = as.numeric(trim$threshold %||% NA_real_))

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
#' `evaluate()`'s `horizons_error_all_configs_failed`.
#'
#' @param results `fit()`'s per-member results tibble, with `error_message`.
#' @param call The call the condition is attributed to. Default: the caller,
#'   `fit()`.
#' @return Never returns; aborts with class
#'   `horizons_error_all_members_failed` (kind `run`).
#' @keywords internal
#' @noRd
abort_all_members_failed <- function(results, call = rlang::caller_env()) {

  n_members <- nrow(results)

  ## distinct_config_errors() returns the bullets brace-escaped, so upstream
  ## error text cannot be read as a cli template.
  abort_horizons(c(
    "All {n_members} configuration{?s} {.fn fit} re-tuned failed, so there is no model to return.",
    distinct_config_errors(results),
    "i" = "The per-member results, error messages included, are on this condition as {.field results}. Recover them with {.code rlang::last_error()$results}."
  ), kind = "run", class = "horizons_error_all_members_failed", results = results,
  call = call)

}

## ---------------------------------------------------------------------------
## log_members_below_threshold — no member cleared the prune threshold
## ---------------------------------------------------------------------------

#' Record that every member fell below evaluate()'s prune threshold
#'
#' @description
#' `evaluate()` records the prune gate's reading on every row as
#' `below_prune_threshold` whenever `prune = TRUE`, apart from the status,
#' which says only whether Bayesian refinement was skipped (#38). `fit()`
#' warns, `quality`, when every member it is about to fit fell below the
#' threshold, naming the threshold and each member's cross-validated RPD:
#' everything ran as documented, and the result is weaker than it looks.
#' Without this, `bayesian_iter = 0` would fit below-threshold
#' configurations silently, since nothing is pruned there. When the members
#' are pruned configurations (the fallback, because none succeeded) the
#' warning says so.
#'
#' A pruned row counts as below the threshold whatever its reading, since the
#' gate put it there. A row with no reading (`NA`, as `prune = FALSE` writes
#' it) does not, so one such member keeps the warning from firing.
#'
#' @param collector `fit()`'s collector, from `log_open()`.
#' @param members The member rows (`evaluation$results` shape).
#' @param fallback Logical. Whether the members are pruned configurations
#'   because none succeeded.
#' @param bayesian_iter The configured `bayesian_iter`
#'   (`x$config$tuning$bayesian_iter`), which decides how the warning
#'   explains unpruned members; `NULL` when unknown.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_members_below_threshold <- function(collector, members, fallback, bayesian_iter = NULL) {

  ### The eval validator requires the reading and the cross-validated RPD on
  ### every row, and the cold start writes them.
  n_members <- nrow(members)
  below     <- members$below_prune_threshold | members$status %in% "pruned"

  if (n_members == 0 || !isTRUE(all(below))) return(invisible(collector))

  thresholds <- unique(stats::na.omit(members$prune_threshold))

  threshold_text <- if (length(thresholds) == 0) {
    "the prune threshold (not recorded on these rows)"
  } else {
    paste0("the prune threshold of ", paste(format(thresholds), collapse = " and "))
  }

  cv_rpd  <- members$cv_rpd
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
  log_warning(collector, "quality", c(
    header,
    "i" = cli_escape(paste0(
      "Their grid-search RPD fell below ", threshold_text,
      ". Cross-validated RPD: ", cv_text, "."
    )),
    "i" = why,
    "i" = "Check their metrics before relying on the fit."
  ), ids = members$config_id)

  invisible(collector)

}

#' Record that UQ and AD were turned off for a small calibration set
#'
#' @description
#' The remedy follows `ensemble()`'s CV+ gate for the same shortfall: it
#' offers lowering `min_calib_rows` only when an allowed value would let the
#' calibration run, and names the number that does. `control_horizons()`
#' takes no `min_calib_rows` below the split-conformal floor
#' (`conformal_min_rows()` at `DEFAULT_UQ_LEVEL`, 9 rows), which holds for
#' the applicability domain alone too, so below it the remedy is more rows.
#'
#' @param collector `fit()`'s collector.
#' @param compute_uq,compute_ad Logical. What was asked for.
#' @param n_calib Integer(1). The calibration rows drawn.
#' @param min_calib_rows Integer(1). The minimum they fell short of.
#' @param ids Character. The members it applies to.
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_calibration_skip <- function(collector, compute_uq, compute_ad, n_calib,
                                 min_calib_rows, ids) {

  what <- paste(c(if (compute_uq) "Prediction intervals (UQ)",
                  if (compute_ad) "the applicability domain (AD)"), collapse = " and ")
  what <- paste0(toupper(substr(what, 1, 1)), substring(what, 2))
  ## The verb follows the subject: intervals are plural, the domain is not
  verb <- if (compute_uq) "were" else "was"

  floor_rows <- conformal_min_rows(DEFAULT_UQ_LEVEL)

  remedy <- if (n_calib >= floor_rows) {
    "Fit on more rows, or lower {.code control_horizons(min_calib_rows = )} to {n_calib} or fewer; the calibration set needs at least {floor_rows} rows."
  } else {
    "Fit on more rows: the calibration set needs at least {floor_rows} rows whatever {.code min_calib_rows} is."
  }

  log_warning(collector, "skipped", c(
    "{what} {verb} not computed for any member: the calibration set has {n_calib} row{?s}, fewer than {.code min_calib_rows} ({min_calib_rows}).",
    "i" = remedy
  ), ids = ids)

  invisible(collector)

}

## ---------------------------------------------------------------------------
## format_test_coverage(): the console line for measured interval coverage
## ---------------------------------------------------------------------------

#' Describe a UQ bundle's measured coverage on the held-out test rows
#'
#' The one line every place that reports intervals uses: `fit()`'s tree for
#' each member, `ensemble()`'s tree, and `print()` and `summary()` for a
#' member's intervals and an ensemble's.
#'
#' @param uq A member's UQ bundle or an ensemble's CV+ bundle, carrying
#'   `test_coverage`, `test_mean_width`, `n_test` and `level_default`. A
#'   bundle without them (built by hand) reads as unmeasured.
#' @return Character(1).
#' @keywords internal
#' @noRd
format_test_coverage <- function(uq) {

  nominal <- round(uq$level_default * 100, 0)

  if (is.null(uq$test_coverage) || !is.finite(uq$test_coverage)) {

    return(paste0("UQ coverage on held-out test rows: not measured (nominal ",
                  nominal, "%)"))

  }

  paste0("UQ coverage: ", round(uq$test_coverage * 100, 1), "% on ",
         uq$n_test, " held-out test rows (nominal ", nominal,
         "%, mean width ", signif(uq$test_mean_width, 3), ")")

}
