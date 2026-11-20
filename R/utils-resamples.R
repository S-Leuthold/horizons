## ---------------------------------------------------------------------------
## Resample transfer helpers
## ---------------------------------------------------------------------------
##
## CANONICAL ACCOUNT of the serialization problem this package had. Other sites
## reference this header rather than restating it, so there is one place to
## correct if the figures are ever re-measured.
##
## The mechanism. R's serializer does not deduplicate plain data frames. Objects
## that share one table by reference therefore cost one copy in memory and N
## copies on the wire, where N is the number of references. `rsample` is built
## this way: an `initial_split` and the `vfold_cv` splits derived from it all
## point at the same table. Critically, neither `object.size()` nor
## `lobstr::obj_size()` shows this — both report the honest in-memory figure —
## so the only way to see it is `length(serialize(x, NULL))`. That is why it
## went undetected for months.
##
## The measurements, all on the KSSL clay data at native 2 cm-1. Two row counts
## appear because they are different frames: 17,788 x 1,701 is the full analysis
## table, and 14,228 x 1,701 is its 80% training split.
##
##   object                          resident   serialized
##   analysis table (17,788 rows)     231.7 MB     231.7 MB
##   split + vfold_cv pair            418.2 MB   1,158.7 MB   (5.0x)
##   recipe, selectors unstripped     186.5 MB     928.0 MB   (5.0x)
##   recipe, selectors stripped       186.5 MB     185.7 MB   (1.0x)
##   baked design the model fits        4.5 MB       3.6 MB
##
## The worker payload before any of this was 2,086.6 MB, against R's 2,048 MB
## long-vector ceiling — 38 MB over, which is exactly the "long vectors not
## supported yet" failure. After: 417.6 MB. Halving the spectral resolution
## brought the old payload to roughly 1,043 MB, just under the ceiling, which is
## why the 4 cm-1 resample in the experiment harness appeared to fix it and was
## never a modelling decision.
##
## Three fixes, in the order they matter: strip the recipe's selector
## environments (R/utils-recipes.R), send the table once with indices and rebuild
## worker-side (this file), and keep the worker body out of evaluate()'s frame
## (R/pipeline-evaluate.R).


#' Extract Resample Indices for Worker Transfer
#'
#' @description
#' Reduces an `rsplit` and its `rset` to the integers needed to rebuild them,
#' plus the metadata `rsample` stores as attributes. The result is a few hundred
#' kilobytes regardless of how wide the spectra are.
#'
#' `split_args` is carried because `tune_grid_loop()` reads it via
#' `rsample::.get_split_args()` and passes it to
#' `rsample::internal_calibration_split()`. `manual_rset()` does not reproduce
#' those attributes on its own, so they are transported explicitly and restored
#' by `rebuild_resamples()`.
#'
#' @param split An `rsplit` (typically from `rsample::initial_split()`).
#' @param cv_folds An `rset` (typically from `rsample::vfold_cv()`) built from
#'   `rsample::training(split)`.
#'
#' @return A list with `train_idx`, `n_rows`, `fold_idx`, `fold_ids`,
#'   `fold_class`, and `split_args`. Carries no data.
#' @keywords internal
#' @noRd

resample_indices <- function(split, cv_folds) {

  ## -------------------------------------------------------------------------
  ## Guard the two assumptions the reconstruction rests on
  ## -------------------------------------------------------------------------
  ## 1. Complement semantics. rebuild_resamples() derives the assessment set as
  ##    setdiff(all rows, analysis rows), which is only correct when a split's
  ##    out_id is NA. initial_split() and vfold_cv() both store NA, but a
  ##    gap-carrying rset (sliding_window(), rolling_origin()) stores explicit
  ##    indices, and silently re-partitioning one of those would put rows the
  ##    model trained on into the assessment set.
  ## 2. A single id column. manual_rset() takes one `ids` vector, so a repeated
  ##    rset's id2 would be dropped and v * repeats resamples would collapse
  ##    onto v identifiers.

  all_splits <- c(list(split), cv_folds$splits)

  if (!all(vapply(all_splits, function(s) all(is.na(s$out_id)), logical(1)))) {

    abort_horizons(c(
      "{.fn resample_indices} supports only resamples whose assessment set is the complement of its analysis set ({.field out_id} is NA).",
      "x" = "Got a split with an explicit {.field out_id}, which this transport would silently re-partition."
    ), kind = "internal")

  }

  id_cols <- grep("^id", names(cv_folds), value = TRUE)

  if (length(id_cols) != 1L) {

    abort_horizons(c(
      "{.fn resample_indices} supports only single-id resamples.",
      "x" = "Got {length(id_cols)} id column{?s} ({.field {id_cols}}), which indicates repeats > 1; the extra identifiers would be lost."
    ), kind = "internal")

  }

  list(
    train_idx   = split$in_id,
    n_rows      = nrow(split$data),
    split_class = class(split),
    fold_idx    = lapply(cv_folds$splits, function(s) s$in_id),
    fold_ids    = cv_folds$id,
    fold_class  = class(cv_folds),
    fold_split_class = class(cv_folds$splits[[1]]),
    split_args  = rsample::.get_split_args(cv_folds)
  )

}


#' Rebuild a Split and Resample Set From Indices
#'
#' @description
#' Inverse of `resample_indices()`. Reconstructs the `rsplit` and `rset` from
#' one copy of the data plus the transported indices, then restores the original
#' class and attributes so `rsample::.get_split_args()` reports what it did
#' before the round trip.
#'
#' Fold membership is reproduced by construction, not by replaying the RNG, so
#' the result does not depend on stratified sampling behaving identically in the
#' worker.
#'
#' @param data The analysis data frame — the same table `split` was built from.
#' @param idx The list returned by `resample_indices()`.
#'
#' @return A list with `split` (an `rsplit`) and `cv_folds` (an `rset`),
#'   equivalent to the originals.
#' @keywords internal
#' @noRd

rebuild_resamples <- function(data, idx) {

  ## Fail at the boundary rather than deep inside tune, where an out-of-range
  ## index surfaces as "Grid search failed" for every config, from a worker.
  if (nrow(data) != idx$n_rows) {

    abort_horizons(c(
      "{.fn rebuild_resamples} got {nrow(data)} row{?s} but the indices were computed against {idx$n_rows}.",
      "x" = "The data and indices must come from the same split."
    ), kind = "internal")

  }

  if (max(idx$train_idx) > idx$n_rows) {

    abort_horizons(c(
      "{.fn rebuild_resamples} got a training index past the data ({max(idx$train_idx)} of {idx$n_rows} row{?s}).",
      "x" = "The data and indices must come from the same split."
    ), kind = "internal")

  }

  if (max(unlist(idx$fold_idx)) > length(idx$train_idx)) {

    abort_horizons(c(
      "{.fn rebuild_resamples} got a fold index past the training rows ({max(unlist(idx$fold_idx))} of {length(idx$train_idx)}).",
      "x" = "The data and indices must come from the same split."
    ), kind = "internal")

  }

  ## `class` is passed so the rebuilt splits keep their subclass. Without it
  ## make_splits() returns a bare rsplit, and rsample::internal_calibration_split()
  ## — the very function split_args is transported for — dispatches on the split
  ## subclass with a hard-aborting default method.
  split <- rsample::make_splits(
    x     = list(analysis   = idx$train_idx,
                 assessment = setdiff(seq_len(idx$n_rows), idx$train_idx)),
    data  = data,
    class = setdiff(idx$split_class, "rsplit")
  )

  train_data <- rsample::training(split)
  n_train    <- nrow(train_data)

  fold_splits <- lapply(idx$fold_idx, function(i) {

    rsample::make_splits(
      x     = list(analysis   = i,
                   assessment = setdiff(seq_len(n_train), i)),
      data  = train_data,
      class = setdiff(idx$fold_split_class, "rsplit")
    )

  })

  cv_folds <- rsample::manual_rset(fold_splits, ids = idx$fold_ids)

  ## Restore what manual_rset() does not carry. .get_split_args() resolves the
  ## creating function from class(x)[[1]] and reads its formals off the object's
  ## attributes, so both have to be put back.
  class(cv_folds) <- idx$fold_class

  for (nm in names(idx$split_args)) {

    attr(cv_folds, nm) <- idx$split_args[[nm]]

  }

  list(split = split, cv_folds = cv_folds)

}


## ---------------------------------------------------------------------------
## The evaluation split, stored slim and pointed at the object's rows
## ---------------------------------------------------------------------------
##
## The fitted object stores its data once, in data$analysis. evaluation$split
## keeps the positions of its partition and, as its data, only the columns
## that tell whether those positions still name the same rows: the id and
## the outcome. rebuild_split() points it back at the object's rows.

#' The columns a stored split keeps
#'
#' @param role_map The object's role map.
#' @return Character: the id column, then the outcome.
#' @keywords internal
#' @noRd

split_columns <- function(role_map) {

  c(id_column(role_map), role_map$variable[role_map$role == "outcome"])

}

#' A split as the object stores it: its positions and its identity columns
#'
#' @description
#' Keeps the `rsplit`'s class, `in_id` and `out_id`, so `training()`,
#' `testing()` and [rebuild_split()]'s check work on it unchanged, and cuts
#' its `data` to [split_columns()]. `evaluate()` and `fit()`'s cold start
#' store the split through this.
#'
#' @param split An `rsplit` over the modelled rows.
#' @param role_map The object's role map.
#' @return The `rsplit`, its `data` the id and outcome columns.
#' @keywords internal
#' @noRd

slim_split <- function(split, role_map) {

  split$data <- split$data[, split_columns(role_map), drop = FALSE]
  split

}

#' Point the evaluation split at the rows the object models
#'
#' @description
#' `evaluate()`'s split is what `fit()` is scored on and what `ensemble()`'s
#' members predict, so both rebuild its rows here, by the rule
#' [rebuild_resamples()] follows: positions plus one copy of the data. The
#' rows are the object's modelled rows ([modelling_rows()]) less the training
#' rows `evaluate()`'s response trim (#77) recorded. They are the rows
#' `evaluate()` recorded, not a fresh trim: fences recomputed here could fall
#' on another set. An evaluation without the record (no trim requested)
#' trimmed nothing.
#'
#' The split's positions are sound only if they still name the same samples
#' with the same outcomes, so that is what is checked: the id and outcome
#' columns, identical values in the same order. Other columns may
#' legitimately have changed; `add_response()` can add a sibling response to
#' an evaluated object. `set_analysis()` refuses a promoted object, so a
#' mismatch means the rows or outcomes were changed some other way, or the
#' object was built by hand; it is refused rather than scoring the wrong rows.
#' The split is then pointed at the current table, so the caller sees the
#' object's columns as they are now.
#'
#' @param x A `horizons_eval` (or a cold start's object, whose split was drawn
#'   from these rows by the draw `evaluate()` uses).
#' @param call The frame a mismatch reports.
#'
#' @return A list: `split`, the `rsplit` over the rows; `modelled`, what
#'   [modelling_rows()] returned; `trimmed_ids`; `trimmed_rows`, the trimmed
#'   training rows or `NULL`; `response_fences`, the trim's `lower` and `upper`
#'   or `NULL`.
#' @keywords internal
#' @noRd

rebuild_split <- function(x, call = rlang::caller_env()) {

  role_map    <- x$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]
  id_col      <- id_column(role_map)

  ## The rows evaluate() modelled: the same rule on the same table (#67).
  modelled <- modelling_rows(x$data$analysis, outcome_col, call = call)

  ### An rsplit: the eval validator at the door refuses anything else, and a
  ### cold start draws one.
  split <- x$evaluation$split

  trimmed_ids   <- x$evaluation$response_trim$trimmed_ids %||% character(0)
  is_trimmed    <- modelled$data[[id_col]] %in% trimmed_ids
  modelled_rows <- if (length(trimmed_ids) > 0) {
    modelled$data[!is_trimmed, , drop = FALSE]
  } else {
    modelled$data
  }

  ## The trimmed rows themselves. They belong to the training partition,
  ## never the test part, and no model is fitted on them; fit()'s calibration
  ## pool and response bound take them back in.
  trimmed_rows <- if (length(trimmed_ids) > 0) {
    modelled$data[is_trimmed, , drop = FALSE]
  } else {
    NULL
  }

  ## The fences the rows were trimmed by, for fit()'s degradation check: the
  ## CV runs inside them and the test rows are untrimmed, so the check
  ## compares like with like only within them (see fit_single_config()).
  response_fences <- if (length(trimmed_ids) > 0) {
    c(lower = x$evaluation$response_trim$lower,
      upper = x$evaluation$response_trim$upper)
  } else {
    NULL
  }

  if (!identical(split$data[[id_col]], modelled_rows[[id_col]]) ||
      !identical(split$data[[outcome_col]], modelled_rows[[outcome_col]])) {

    trim_note <- if (length(trimmed_ids) > 0) {
      paste0(" once the ", length(trimmed_ids), " training rows evaluate() trimmed are left out")
    } else {
      ""
    }

    abort_horizons(c(
      "{.fn evaluate}'s split does not index the rows this object models.",
      "x" = "{.field evaluation$split} was drawn on {nrow(split$data)} row{?s}; the analysis table has {nrow(modelled_rows)} with an observed {.field {outcome_col}}{trim_note}, and their {.field {id_col}} or {.field {outcome_col}} values differ.",
      "i" = "The rows or outcomes changed after {.fn evaluate}, or the object was built by hand.",
      "i" = "Re-run {.fn evaluate} on the object as it is now."
    ), kind = "input", call = call)

  }

  split$data <- modelled_rows

  list(
    split           = split,
    modelled        = modelled,
    trimmed_ids     = trimmed_ids,
    trimmed_rows    = trimmed_rows,
    response_fences = response_fences
  )

}


## ---------------------------------------------------------------------------
## Stratified draws: whether the strata held
## ---------------------------------------------------------------------------
##
## evaluate(), fit() and ensemble() ask rsample to stratify every draw on the
## outcome: the train/test split, fit()'s calibration split, the CV folds, and
## ensemble()'s meta-learner and CV+ folds. Two things
## turn such a draw unstratified. The stratified call can error, and each site
## retried without strata. More often, rsample draws unstratified by itself:
## make_strata() bins a numeric outcome into quartiles only while every bin
## would hold 20 rows, so below 80 rows it uses fewer bins and below 40 none,
## warning "Too little data to stratify"; an outcome with five or fewer
## distinct values is stratified by value, with values under 10 % of the rows
## pooled into the others, and pooling down to one stratum draws unstratified
## with no warning at all, as do tied quantiles that leave one bin (a
## zero-inflated outcome). The returned object records none of this: its
## `strata` attribute names the outcome either way. The console said
## "stratified" regardless (#91), so the draws now go through here.


#' Whether rsample stratifies a draw on this outcome
#'
#' @description
#' Asks [rsample::make_strata()], with the `breaks` and `pool` every draw
#' passes (`STRATA_BREAKS`, `STRATA_POOL`), how many strata it would form. One
#' stratum is an unstratified draw. See the section header above for the
#' cases.
#'
#' `make_strata()` draws a random stratum for each pooled value, so the
#' caller's RNG state is restored on exit: a draw then starts from its unit's
#' seed ([with_unit_seed()]) whatever `make_strata()` drew here. Its warning is
#' muffled here only because this is a second look at the outcome: the draw
#' itself raises it.
#'
#' @param outcome The outcome vector the draw stratifies on.
#' @return `TRUE` when `make_strata()` forms at least two strata.
#' @keywords internal
#' @noRd

outcome_stratifies <- function(outcome) {

  local_rng_state()

  strata <- suppressWarnings(
    rsample::make_strata(outcome, breaks = STRATA_BREAKS, pool = STRATA_POOL)
  )

  nlevels(strata) > 1L

}


#' Draw stratified on the outcome, and say whether the strata held
#'
#' @description
#' Runs `stratified()`, the rsample call with `strata`, and on an error runs
#' `unstratified()`, the same call without, as each draw site did inline.
#' The caller runs it inside the draw's unit ([with_unit_seed()]);
#' [outcome_stratifies()] leaves the stream as found, so the draw starts at
#' the unit's seed whatever the check drew. An error from the check
#' itself counts as unstratified rather than aborting: the stratified draw
#' calls `make_strata()` with the same arguments, so it fails too and is
#' retried without strata, as it was before the check existed.
#'
#' The warnings rsample raises during the stratified draw ("Too little data
#' to stratify", or fewer bins than `breaks` asks for) are caught and
#' returned, for the verb to record with the rest of what it reports
#' ([log_strata_fallback()]), rather than printed as they arise.
#'
#' @param outcome The outcome vector `stratified()` stratifies on.
#' @param stratified,unstratified Functions of no arguments returning the
#'   draw, with and without `strata`. `stratified()` passes `breaks =
#'   STRATA_BREAKS, pool = STRATA_POOL`, the values the check uses.
#' @return List with `draw` (the `rsplit` or `rset`), `stratified` (`FALSE`
#'   when the draw is unstratified, whether rsample dropped the strata or the
#'   stratified call failed), `strata_failed` (`TRUE` when the stratified
#'   call failed and `unstratified()` was run), `strata_error` (why it
#'   failed, one line, or `NA`) and `strata_warnings` (what rsample warned
#'   of during a stratified draw that did not fail, one line each).
#' @keywords internal
#' @noRd

draw_stratified <- function(outcome, stratified, unstratified) {

  held            <- tryCatch(outcome_stratifies(outcome), error = function(e) FALSE)
  strata_failed   <- FALSE
  strata_error    <- NA_character_
  strata_warnings <- character(0)

  draw <- tryCatch(
    withCallingHandlers(
      stratified(),
      warning = function(w) {
        strata_warnings <<- c(strata_warnings, condition_summary(w))
        rlang::cnd_muffle(w)
      }
    ),
    error = function(e) {

      strata_failed <<- TRUE
      strata_error  <<- condition_summary(e)
      unstratified()

    }
  )

  ## A draw that failed is recorded by its failure; what rsample said on the
  ## way to it is moot, since the draw that ran is unstratified
  list(draw            = draw,
       stratified      = held && !strata_failed,
       strata_failed   = strata_failed,
       strata_error    = strata_error,
       strata_warnings = if (strata_failed) character(0) else strata_warnings)

}


#' What a stratified draw that did not stratify as asked reports
#'
#' @description
#' A draw that came out unstratified is a substitution: what ran is not what
#' was asked. That is so when the stratified call failed and was retried
#' without strata, and when rsample drew without strata itself, for whatever
#' reason (too little data, tied quantiles leaving one bin, a few-valued
#' outcome pooled into one stratum), with a warning or without. Each such
#' draw is reported once, with the failure's cause or rsample's words when it
#' gave any, under the `substitution` topic. A draw rsample stratified on
#' fewer bins than asked, with a warning, is still stratified, so what ran is
#' what was asked: it is a note.
#'
#' The one message builder for every stratified draw: a verb's own draws
#' report it to the collector ([log_strata_fallback()]), and a draw inside a
#' unit, as `ensemble()`'s are, reports it from there
#' ([report_strata_in_unit()]).
#'
#' @param drawn A result of [draw_stratified()].
#' @param what Character(1). The draw, for the message, e.g.
#'   `"train/test split"`.
#' @return `NULL` when the draw stratified as asked; otherwise a list with
#'   `type` (`"warning"`, under the `substitution` topic, or `"note"`),
#'   `message` (a cli message) and `envir` (where it is interpolated).
#' @keywords internal
#' @noRd

strata_report <- function(drawn, what) {

  if (isTRUE(drawn$strata_failed)) {

    return(list(type = "warning", envir = environment(), message = c(
      "The stratified draw of the {what} failed, so it was drawn without strata.",
      "x" = "{drawn$strata_error}"
    )))

  }

  ### rsample's words, substituted rather than inlined, so braces stay text
  reasons <- drawn$strata_warnings
  bullets <- stats::setNames(sprintf("{reasons[%d]}", seq_along(reasons)),
                             rep("x", length(reasons)))

  ## What rsample did, not why: its warnings, when it gave any, say why
  if (!isTRUE(drawn$stratified)) {

    return(list(type = "warning", envir = environment(), message = c(
      "The stratified draw of the {what} found one stratum in the outcome, so it was drawn without strata.",
      bullets
    )))

  }

  if (length(reasons) == 0) return(NULL)

  list(type = "note", envir = environment(), message = c(
    "The stratified draw of the {what} had too little data for {STRATA_BREAKS} strata, so rsample drew it on fewer.",
    bullets
  ))

}


#' Record a stratified draw that did not stratify as asked
#'
#' @description
#' Adds what [strata_report()] says of the draw to the verb's collector: a
#' `substitution` warning, which the verb raises once at its end, or a note,
#' shown in the tree and not raised.
#'
#' @param collector The verb's collector, from `log_open()`.
#' @inheritParams strata_report
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd

log_strata_fallback <- function(collector, drawn, what) {

  report <- strata_report(drawn, what)

  if (is.null(report)) return(invisible(NULL))

  if (identical(report$type, "note")) {
    log_note(collector, report$message, .envir = report$envir)
  } else {
    log_warning(collector, "substitution", report$message, .envir = report$envir)
  }

  invisible(NULL)

}


#' Report a stratified draw that did not stratify as asked, from inside a unit
#'
#' @description
#' As [log_strata_fallback()], for a draw made inside a unit, which cannot
#' reach the verb's collector: the warning is raised with [warn_in_unit()]
#' and the note with [note_in_unit()], and the verb lifts both onto its log.
#'
#' @inheritParams strata_report
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd

report_strata_in_unit <- function(drawn, what) {

  report <- strata_report(drawn, what)

  if (is.null(report)) return(invisible(NULL))

  if (identical(report$type, "note")) {
    note_in_unit(report$message, .envir = report$envir)
  } else {
    warn_in_unit("substitution", report$message, .envir = report$envir)
  }

  invisible(NULL)

}


#' The tree's line for a stratified draw that did not stratify as asked
#'
#' @param drawn A result of [draw_stratified()].
#' @param what Character(1). The draw, as the tree names it, e.g. `"split"`
#'   or `"CV"`.
#' @return Character(1), or `NULL` when the draw stratified as asked or
#'   rsample left it unstratified (its line says "unstratified").
#' @keywords internal
#' @noRd

strata_tree_line <- function(drawn, what) {

  if (isTRUE(drawn$strata_failed)) {
    return(paste0("Stratified ", what, " failed, retrying without strata"))
  }

  if (isTRUE(drawn$stratified) && length(drawn$strata_warnings) > 0) {
    return(paste0("Stratified ", what, " on fewer strata: too little data for ",
                  STRATA_BREAKS))
  }

  NULL

}
